unit PublishServerNWBLive;

interface

uses
  Logger,
  IMB3NativeClient, ByteBuffers, // imb 3
  imb4,

  ODBFiles2,

  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,

  TimerPool,

  SessionServerLib,

  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector,

  System.Math,
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows;

const
  // NWB
  NWBLiveFeedProjectName = 'NWBLiveFeed';
  NWBLiveFeedRemoteHostSwitch = 'NWBLiveFeedRemoteHost';
    DefaultNWBLiveFeedRemoteHost = 'vps17642.public.cloudvps.com';
  NWBLiveFeedRemotePortSwitch = 'NWBLiveFeedRemotePort';
    DefaultNWBLiveFeedRemotePort = IMB3NativeClient.DefaultRemotePort; // 4000
  NWBLiveFeedPrefixSwitch = 'NWBLiveFeedPrefix';
    DefaultNWBLiveFeedPrefix = 'US_RT';
  NWBLiveFeedShapeFileNameSwitch = 'NWBLiveFeedShapeFileName';
    DefaultNWBLiveFeedShapeFileName = 'Wegvakken.shp'; // 'NL_receptoren.shp'; //'Rdam0102016_uitsnede.shp'; //'Wegvakken.shp';

  NWBUpdateTimeOut_ms = 5000;
  NWBCleanupCycle_ms = 24*60*60*1000; //  every day

  NWBTrafficDomain = 'Verkeer';
  NWBAirDomain = 'Lucht';

  NWBLiveFeedTilesURLSwitch = 'NWBLiveFeedTilesURL';
    DefaultNWBLiveFeedTilesURL = 'http://web-ecodistrict.tno.nl/tiler/TilerWebService.dll/tiles';



type
  TOldTilerLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; {aTilerEvent: TEventEntry; }aRefreshEvent: TIMBEventEntry; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fRefreshEvent: TIMBEventEntry;
    procedure HandleRefreshEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  public
    function SliceType: Integer; override;
  end;

  TNWBLiveFeedRoad = class(TGeometryLayerObject)
  constructor CreateFromFeed(aLayer: TLayer; var aByteBuffer: ByteBuffers.TByteBuffer);
  private
    ftime: Int64; // 1st double, is system epoch time
    //fwvk_id: Integer; // 2nd double , is integer as index into nwb shape attributes
    fVp: Double; // 3th double,
    fi: Double; // 4th double,
    fd: Double; // 5th double, density (number of vehicles/km)
    fCapacity: Double; // todo: from ?
  public
    procedure ConvertFromShape(aShape: TGIS_Shape);
    function Change(aNewRoad: TNWBLiveFeedRoad): Boolean;
  end;

  TNWBObjectList = class
  constructor Create(aOwnsObjects: Boolean);
  destructor Destroy; override;
  private
    fLock: TOmniMREW;
    fObjects: TObjectList<TLayerObject>;
  public
    property lock: TOmniMREW read fLock;
    property objects: TObjectList<TLayerObject> read fObjects;

    procedure add(aObject: TLayerObject);
    procedure remove(aObject: TLayerObject);
    procedure clear;
  end;

  TNWBLiveFeedLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription : string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    {aTilerEvent: TEventEntry; }aLiveFeedConnection: TIMBConnection; const aShapeFileName: string);
  destructor Destroy; override;
  private
    fLiveFeedConnection: TIMBConnection;
    fWVKID2UID: TDictionary<TWDID, Integer>; // own,  lookup wvk_id to shape uid
    fNWBShape: TGIS_LayerSHP;
    fUpdateTimeOut: TTimer; // ref
    fCleanupTimer: TTimer; // ref
    fObjectsAdded: TNWBObjectList; // refs
    fObjectsUpdated: TNWBObjectList; // refs
    fObjectsDeleted: TNWBObjectList; //  owns!
    fLastTimeStamp: string;
  	procedure HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleUpdateTimeOut(aTimer: TTimer);
    procedure HandleCleanup(aTimer: TTimer);
  protected
    function getJSON: string; override;
  public
    function SliceType: Integer; override;
  end;

  TNWBLiveFeedScenario = class(TScenario)
  constructor Create(aProject: TProject; const aScenarioID: string;
    aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string; aAddBasicLayers: Boolean);
  destructor Destroy; override;
  private
    fLiveFeedConnection: TIMBConnection;
    kpi1: TKPI; // own
  public
  end;

  TNWBLiveFeedProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aLiveFeedConnection: TIMBConnection{IMB3}; aPalette: TWDPalette; const aShapeFilename: string; aMaxNearestObjectDistanceInMeters: Integer);
  private
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
  public
    procedure ReadBasicData(); override;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;



implementation

{ utils }

function CreatePaletteFromODB(const aDescription: string; const odbList: TODBList; aIsNoDataTransparent: Boolean): TWDPalette;
var
  entries: TPaletteDiscreteEntryArray;
  noDataColor: TAlphaRGBPixel;
  i: Integer;
  p: TDiscretePaletteEntry;
begin
  noDataColor := 0 ; // transparent // TAlphaColors.black and not TAlphaColors.Alpha;
  setLength(entries, 0);
  for i := 0 to Length(odbList)-1 do
  begin
    if not odbList[i].IsNoData then
    begin
      p.colors := TGeoColors.Create(odbList[i].Color);
      p.minValue := odbList[i].Min;
      p.maxValue := odbList[i].Max;
      p.description := odbList[i].Description;
      setLength(entries, length(entries)+1);
      entries[length(entries)-1] := p;
    end
    else
    begin
      if aIsNoDataTransparent
      then noDataColor := 0
      else noDataColor := odbList[i].Color;
    end;
  end;
  Result := TDiscretePalette.Create(aDescription, entries, TGeoColors.Create(noDataColor));
end;



{ TNWBLiveFeedRoad }

function TNWBLiveFeedRoad.Change(aNewRoad: TNWBLiveFeedRoad): Boolean;
begin
  Result := False;
  if not SameValue(fVp, aNewRoad.fVp) then
  begin
    fVp := aNewRoad.fVp;
    Result := True;
  end;
  if not SameValue(fi, aNewRoad.fi) then
  begin
    fi := aNewRoad.fi;
    Result := True;
  end;
  if not SameValue(fd, aNewRoad.fd) then
  begin
    fd := aNewRoad.fd;
    Result := True;
  end;
  ftime := aNewRoad.ftime; // todo: changes always?
end;

procedure TNWBLiveFeedRoad.ConvertFromShape(aShape: TGIS_Shape);
var
  partI: Integer;
  pointI: Integer;
  p: TGIS_Point3D;
  partG: TWDGeometryPart;
  projection: TGIS_CSProjectedCoordinateSystem;
begin
  geometry.parts.Clear;
  if Assigned(aShape) then
  begin
    projection := (layer.scenario.project as TNWBLiveFeedProject).sourceProjection;
    for partI := 0 to aShape.GetNumParts-1 do
    begin
      partG := geometry.AddPart;
      for pointI := 0 to aShape.GetPartSize(partI)-1 do
      begin
        p := aShape.GetPoint3D(partI, pointI);
        p := projection.ToGeocs3D(p);
        partG.AddPoint(p.X, p.Y, p.Z);
      end;
    end;
  end;
end;

constructor TNWBLiveFeedRoad.CreateFromFeed(aLayer: TLayer; var aByteBuffer: ByteBuffers.TByteBuffer);
var
  id: TWDID;
begin
  ftime := Trunc(aByteBuffer.ReadDouble());
  id := TWDID(Trunc(aByteBuffer.ReadDouble()).ToString);
  fVp := aByteBuffer.ReadDouble();
  fi := aByteBuffer.ReadDouble();
  fd := aByteBuffer.ReadDouble();
  fCapacity := NaN;
  inherited Create(aLayer, ID, TWDGeometry.Create, fVp); // use Vp as value for coloring
end;


{ TObjectList }

procedure TNWBObjectList.add(aObject: TLayerObject);
begin
  fLock.BeginWrite;
  try
    fObjects.Add(aObject);
  finally
    fLock.EndWrite;
  end;
end;

procedure TNWBObjectList.clear;
begin
  fLock.BeginWrite;
  try
    fObjects.Clear;
  finally
    fLock.EndWrite;
  end;
end;

constructor TNWBObjectList.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  fLock.Create;
  fObjects := TObjectList<TLayerObject>.Create(aOwnsObjects); // refs
end;

destructor TNWBObjectList.Destroy;
begin
  FreeAndNil(fObjects);
  inherited;
end;

procedure TNWBObjectList.remove(aObject: TLayerObject);
begin
  fLock.BeginWrite;
  try
    fObjects.Remove(aObject);
  finally
    fLock.EndWrite;
  end;
end;


{ TNWBLiveFeedLayer }

constructor TNWBLiveFeedLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription : string; aDefaultLoad: Boolean;
  aPalette: TWDPalette; {aTilerEvent: TEventEntry; }aLiveFeedConnection: TIMBConnection; const aShapeFileName: string);
var
  shape: TGIS_Shape;
  objJSON: string;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, {aPalette, }'"road"', 'LineString', aPalette.maxValue/2);
  fLiveFeedConnection := aLiveFeedConnection; // ref
  fObjectsAdded := TNWBObjectList.Create(False); // refs
  fObjectsUpdated := TNWBObjectList.Create(False); // refs
  fObjectsDeleted := TNWBObjectList.Create(True); // owns!
  fUpdateTimeOut := aScenario.project.Timers.SetTimer(HandleUpdateTimeOut);
  fWVKID2UID := TDictionary<TWDID, Integer>.Create;
  fNWBShape := TGIS_LayerSHP.Create;
  // load shape file
  fNWBShape.Name := 'NWBWegvakken';
  fNWBShape.Path := aShapeFileName;
  fNWBShape.Open; // todo: call fNWBShape.Open; here so init is slow but first lookup is fast?
  // fill dictionary for lookups of wvk_id to uid in shape layer
  shape := fNWBShape.FindFirst(GisWholeWorld);
  objJSON := '';
  while Assigned(Shape) do
  begin
    fWVKID2UID.AddOrSetValue(TWDID(shape.GetField('WVK_ID')), shape.Uid);
    shape := fNWBShape.FindNext;
  end;
  Log.WriteLn('NWB: read shape file '+fNWBShape.Path);
  // go live
  fLiveFeedConnection.Subscribe('NWB', HandleNormalEvent);
  // start timer to cleanup old layer objects
  fCleanupTimer := scenario.project.Timers.SetTimerDelta(
    HandleCleanup,
    MilliSeconds2HRT(NWBCleanupCycle_ms),
    MilliSeconds2HRT(NWBCleanupCycle_ms));
end;

destructor TNWBLiveFeedLayer.Destroy;
//var
//  iop: TPair<Integer, TLayerObject>;
begin
  //fUpdateTimeOut.Cancel;
  fUpdateTimeOut := nil;
  //fCleanupTimer.Cancel;
  fCleanupTimer := nil;

  // todo: test, remove all objects
//  for iop in objects
//  do fObjectsDeleted.add(iop.Value);
//  HandleUpdateTimeOut(nil);

  inherited;
  //FreeAndNil(fLiveFeedConnection);
  FreeAndNil(fObjectsAdded);
  FreeAndNil(fObjectsUpdated);
  FreeAndNil(fObjectsDeleted);
  FreeAndNil(fNWBShape);
  FreeAndNil(fWVKID2UID);
  fLiveFeedConnection := nil; // ref
end;

function TNWBLiveFeedLayer.getJSON: string;
begin
  Result := inherited+','+
    '"timestamp":"'+fLastTimeStamp+'"';
end;

{
function TNWBLiveFeedLayer.getObjectsJSON: string;
var
  iop: TPair<TWDID, TLayerObject>;
begin
  Result := '';
  for iop in objects do
  begin
    if iop.Value.ValidGeometry
    then jsonAdd(Result, iop.Value.JSON2D);
  end;
  Result := geoJsonFeatureCollection(Result);
end;
}
procedure TNWBLiveFeedLayer.HandleCleanup(aTimer: TTimer);
begin
  // cleanup old layer objects

  // todo: implement

end;

procedure TNWBLiveFeedLayer.HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  newRoad: TLayerObject;
  road: TLayerObject;
  shape: TGIS_Shape;
  uid: Integer;
begin
  // handle feed entry
  newRoad := TNWBLiveFeedRoad.CreateFromFeed(Self, aPayload);
  try
    fLastTimeStamp := FormatDateTime(publisherDateTimeFormat, ((newRoad as TNWBLiveFeedRoad).fTime / 86400) + 25569)+' UTC';
    if FindObject(newRoad.id, road) then
    begin
      if (road as TNWBLiveFeedRoad).Change(newRoad as TNWBLiveFeedRoad) then
      begin
        Log.Progress('NWB: changed '+string(road.id));
        fObjectsUpdated.add(road); // ref
      end;
    end
    else
    begin
      // lookup geometry and capacitiy and add to road information
      if fWVKID2UID.TryGetValue(newRoad.id, uid) then
      begin
        shape := fNWBShape.GetShape(uid);
        if Assigned(shape) then
        begin
          (newRoad as TNWBLiveFeedRoad).ConvertFromShape(shape);
          // todo: newRoad.fCapacity := ..
          Log.Progress('NBW: new '+string(newRoad.id));
          AddObject(newRoad); // own
          fObjectsAdded.add(newRoad); // ref
          newRoad := nil;
        end
        else Log.WriteLn('NWB: did not find shape for '+string(newRoad.id)+' on uid '+uid.ToString, llError);
      end
      else
      begin
        Log.WriteLn('NWB: did not find wvk_id '+string(newRoad.id), llWarning);
        // add empty record to avoid lookup problems later on
        // todo: newRoad.fCapacity := ..
        // no signal
        AddObject(newRoad); // own
        newRoad := nil;
      end;
    end;
  finally
    newRoad.Free;
  end;
  fUpdateTimeOut.DueTimeDelta := MilliSeconds2HRT(NWBUpdateTimeOut_ms);
end;

procedure TNWBLiveFeedLayer.HandleUpdateTimeOut(aTimer: TTimer);
var
  obj: TLayerObject;
  json: string;
  client: TClient;
begin
  // added objects, signal complete structure
  json := '';
  fObjectsAdded.fLock.BeginWrite;
  try
    for obj in fObjectsAdded.objects
    do jsonAdd(json, '"'+string(obj.id)+'"'+':'+obj.JSON2D[geometryType]);
    fObjectsAdded.objects.Clear;
  finally
    fObjectsAdded.fLock.EndWrite;
  end;
  if json<>'' then
  begin
    json :=
      '{"updatelayer":'+
        '{"id":"'+elementID+'",'+
         '"name":"'+name+'",'+
         '"timestamp":"'+fLastTimeStamp+'",'+
         '"newobjects":'+'{'+json+'}'+
        '}'+
      '}';
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(json);
    finally
      TMonitor.Exit(clients);
    end;
  end;
  // updated objects, signal changed colors
  json := '';
  fObjectsUpdated.lock.BeginWrite;
  try
    for obj in fObjectsUpdated.objects
    do jsonAdd(json, '"'+string(obj.id)+'"'+':"'+ColorToJSON(fTilerLayer.palette.ValueToColors((obj as TNWBLiveFeedRoad).fVp).fillColor)+'"');
    fObjectsUpdated.objects.Clear;
  finally
    fObjectsUpdated.lock.EndWrite;
  end;
  if json<>'' then
  begin
    json :=
      '{"updatelayer":'+
        '{"id":"'+elementID+'",'+
         '"name":"'+name+'",'+
         '"timestamp":"'+fLastTimeStamp+'",'+
         '"changedcolors":'+'{'+json+'}'+
        '}'+
      '}';
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(json);
    finally
      TMonitor.Exit(clients);
    end;
  end;
  // removed objects
  json := '';
  fObjectsDeleted.lock.BeginWrite;
  try
    for obj in fObjectsDeleted.objects
    do jsonAdd(json, '"'+string(obj.id)+'":"X"');
    fObjectsDeleted.objects.Clear; // owns objects!
  finally
    fObjectsDeleted.lock.EndWrite;
  end;
  if json<>'' then
  begin
    json :=
      '{"updatelayer":'+
        '{"id":"'+elementID+'",'+
         '"name":"'+name+'",'+
         '"timestamp":"'+fLastTimeStamp+'",'+
         '"removedobjects":'+'{'+json+'}'+
        '}'+
      '}';
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(json);
    finally
      TMonitor.Exit(clients);
    end;
  end;

  // todo: update kpi1 as test
  with fScenario as TNWBLiveFeedScenario do
  begin
    if kpi1.measures[0]<>200
    then kpi1.measures := [200, 290]
    else kpi1.measures := [230, 250];
    kpi1.Update;
  end;

end;

function TNWBLiveFeedLayer.SliceType: Integer;
begin
  Result := stUndefined;
end;

{ TOldTilerLayer }

constructor TOldTilerLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; {aTilerEvent: TEventEntry; }aRefreshEvent: TIMBEventEntry; aBasicLayer: Boolean);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, {aPalette, }aObjectTypes, aGeometryType, NaN, aBasicLayer); // todo:
  fRefreshEvent := aRefreshEvent;
  fRefreshEvent.OnNormalEvent := HandleRefreshEvent;
end;

destructor TOldTilerLayer.Destroy;
begin
  if Assigned(fRefreshEvent) then
  begin
    fRefreshEvent.UnSubscribe;
    fRefreshEvent := nil;
  end;
  inherited;
end;

procedure TOldTilerLayer.HandleRefreshEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  client: TClient;
begin
  // payload contains 1 boolean but is always true, no need to decode
  TMonitor.Enter(clients);
  try
    for client in clients
    do client.SendRefresh(elementID, '', uniqueObjectsTilesLink); // todo: add time stamp?
  finally
    TMonitor.Exit(clients);
  end;
end;

function TOldTilerLayer.SliceType: Integer;
begin
  Result := stUndefined;
end;

{ TNWBLiveFeedScenario }

procedure addLiveAirLayer(aScenario: TScenario; const aDomain: string; aLayerID: Integer; const aName, aDescription: string;
  {aTilerEvent: TEventEntry; }const aAVLFileName: string; aLiveFeedConnection: TIMBConnection);
var
  palette: TWDPalette;
  layer: TLayer;
begin
  if FileExists(aAVLFileName) then
  begin
    palette := CreatePaletteFromODB(aName, ODBFileToODBList(aAVLFileName), True);
    layer := aScenario.AddLayer(
      TOldTilerLayer.Create(aScenario, aDomain, aLayerID.toString, aName, aDescription, False,
      palette, '"road"', 'tile', {aTilerEvent, }aLiveFeedConnection.Subscribe('layers.'+aLayerID.ToString)));
    // todo:
    (*
    layer.objectsTilesID := aLayerID;
    layer.objectsTilesLink := GetSetting(NWBLiveFeedTilesURLSwitch, DefaultNWBLiveFeedTilesURL)+
      '?layer='+aLayerID.ToString+'&zoom={z}&x={x}&y={y}';
    *)
  end
  else Log.WriteLn('Could not create live air layer: avl '+aAVLFileName+' not found', llError);
end;

constructor TNWBLiveFeedScenario.Create(aProject: TProject; const aScenarioID: string;
  aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string; aAddBasicLayers: Boolean);
var
  resourceFolder: string;
begin
  // todo: rotterdam
  inherited Create(aProject, aScenarioID, aScenarioID, '', aAddBasicLayers, TMapView.Create(51.946333, 4.311171, 11));
  fLiveFeedConnection := aLiveFeedConnection;
  AddLayer(
    TNWBLiveFeedLayer.Create(Self, NWBTrafficDomain, 'live', 'Live traffic i/c', 'Live traffic feed', True, aPalette,
    {fProject.TilerEvent, }fLiveFeedConnection, aShapeFileName));
  resourceFolder := ExtractFilePath(ParamStr(0));
  //addLiveAirLayer(Self, AirDomain, 121, 'NOx',  '', fProject.TilerEvent, 'no2.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 122, 'PM10', '', {fProject.TilerEvent, }resourceFolder+'pm10.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 123, 'PM25', '', {fProject.TilerEvent, }resourceFolder+'pm25.avl', fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 124, 'NO2',  '', fProject.TilerEvent, 'no2.avl',  fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 132, 'PM10 tot', '', fProject.TilerEvent, 'pm10.avl', fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 133, 'PM25 tot', '', fProject.TilerEvent, 'pm25.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 134, 'NO2',  '', {fProject.TilerEvent, }resourceFolder+'no2.avl',  fLiveFeedConnection);
  kpi1 := TKPI.Create(Self, NWBTrafficDomain, 'kpi1', 'kpi1', 'a traffic kpi', false);
  with AddKPI(kpi1) do
  begin
    title := 'kpi1';
    subtitle := '-';
    ranges := [150, 225, 300];
    measures := [220, 270];
    markers := [250];
  end;
  with AddKPI(TKPI.Create(Self, NWBTrafficDomain, 'kpi2', 'kpi2', 'a traffic kpi', false)) do
  begin
    title := 'kpi2';
    subtitle := '-';
    ranges := [150, 225, 300];
    measures := [200, 280];
    markers := [260];
  end;
  with AddChart(TChart.Create(Self, NWBTrafficDomain, 'chart1', 'chart1', 'a traffic chart', false, 'verticalBars')) do
  begin
    groupNames := ['set een', 'set twee'];
    groupValues := [
      TChartGroupRow.Create('een', [1,2]),
      TChartGroupRow.Create('twee', [2,3]),
      TChartGroupRow.Create('drie', [3,4]),
      TChartGroupRow.Create('vier', [7,4]),
      TChartGroupRow.Create('vijf', [5,2])
    ];
  end;
  with AddChart(TChart.Create(Self, NWBTrafficDomain, 'chart2', 'chart2', 'a traffic chart 2', false, 'verticalBars')) do
  begin
    groupNames := ['uno'];
    groupValues := [
      TChartGroupRow.Create('1', [1]),
      TChartGroupRow.Create('2', [2]),
      TChartGroupRow.Create('3', [3]),
      TChartGroupRow.Create('4', [3]),
      TChartGroupRow.Create('5', [7]),
      TChartGroupRow.Create('6', [3])
    ];
  end;
  with AddChart(TChart.Create(Self, NWBTrafficDomain, 'chart3', 'chart3', 'a traffic chart 3', false, 'verticalBars')) do
  begin
    groupNames := ['uno', 'due', 'tre', 'quattro'];
    groupValues := [
      TChartGroupRow.Create('een', [1,2,3,4]),
      TChartGroupRow.Create('twee', [2,3,4,5]),
      TChartGroupRow.Create('drie', [3,4,5,2])
    ];
  end;
end;

destructor TNWBLiveFeedScenario.Destroy;
begin
  fLiveFeedConnection.Close; // to unlock readers
  inherited;
  FreeAndNil(fLiveFeedConnection);
end;

{ TNWBLiveFeedProject }

constructor TNWBLiveFeedProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string; aMaxNearestObjectDistanceInMeters: Integer);
begin
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, nil, 0, False, False, False, False, False, aMaxNearestObjectDistanceInMeters);
  {if aSourceEPSG>0
  then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(aSourceEPSG)
  else }
  fProjectCurrentScenario := TNWBLiveFeedScenario.Create(Self, 'Live', aLiveFeedConnection, aPalette, aShapeFileName, addBasicLayers);

  TMonitor.Enter(fScenarios);
  try
    fScenarios.Add(fProjectCurrentScenario.ID, fProjectCurrentScenario);
  finally
    TMonitor.Exit(fScenarios);
  end;
end;

procedure TNWBLiveFeedProject.ReadBasicData;
begin
  //fMeasuresJSON := '[]';
  // all reading is done in live layer
end;



end.
