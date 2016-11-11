unit SessionServerSSM;

interface

uses
  Logger,
  IMB3NativeClient, IMB3Core, ByteBuffers,
  imb4,
  WorldDataCode,
  Data.DB,
  StdIni,

  GisDefs, GisCsSystems,
  TimerPool,
  SessionServerLib,
  WinApi.Windows,
  System.JSON,
  System.Generics.Collections, System.Generics.Defaults,
  SysUtils;

type
  TSSMCar = class(TLayerObject)
  public
    // gtuID = TLayerObject.ID
    // timestamps
    newTimestamp: Double;
    changedTimestamp: Double;
    deletedTimestamp: Double;
    // base, new/change/delete
    x: Double;
    y: Double;
    z: Double;
    rotZ: Double;
    // new
    networkId: AnsiString;
    linkId: AnsiString;
    laneId: AnsiString;
    longitudinalPosition: Double;
    length: Double;
    width: Double;
    baseColor: TAlphaRGBPixel;
    // change/delete
    speed: Double;
    acceleration: Double;
    turnIndicatorStatus: string;
    brakingLights: Boolean;
    odometer: Double;
    // cache
    latlon: TGIS_Point;
  public
    function getGeoJSON2D(const aType: string): string; override;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  public
    // imb 3 decoding
    function new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
    function change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
    function delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;

//    function toJSONNew: string;
  end;

  TSSMLinkLayer = class;

  TSSMCarLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aLinkLayer: TSSMLinkLayer);
  private
    fGTUEvent: TIMBEventEntry;
    fLinkLayer: TSSMLinkLayer;
  public
    procedure HandleGTUEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  public
    property linkLayer: TSSMLinkLayer read fLinkLayer write fLinkLayer;
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  end;

  TSSMLink = class(TGeometryLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry);
  protected
    fNumberOfVehicles: Integer;
    fTotalSpeed: Double;
    //procedure setTotalSpeed(const aValue: Double);
  public
    property numberOfVehicles: Integer read fNumberOfVehicles;// write fNumberOfVehicles;
    property totalSpeed: Double read fTotalSpeed;// write setTotalSpeed;

    procedure AddGTU(aSpeed: Double);
    procedure ChangeGTU(aOldSpeed, aNewSpeed: Double);
    procedure RemoveGTU(aSpeed: Double);
  end;

  TSSMLinkLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aCarLayer: TSSMCarLayer);
  private
    fLinkEvent: TIMBEventEntry;
    fCarLayer: TSSMCarLayer;
  public
    property carLayer: TSSMCarLayer read fCarLayer write fCarLayer;
    procedure HandleLinkEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  end;

  TSSMStatistic = class
  constructor Create(aScenario: TScenario; const aDomain, aID: string);
  destructor Destroy; override;
  public

    // timestamps
    newTimestamp: Double;

    changedTimestamp: Double;
    deletedTimestamp: Double;
    // base, new/change/delete
    statisticId: AnsiString;
    // new
    description: AnsiString;
    networkId: AnsiString;
    numberMetadataEntries: integer;
    //metadataId: AnsiString;
    //metadataType: AnsiString;
    //metadataValue: AnsiString;
    //numberSpaceTimeRegions: integer;
    //startTime: double;
    //endTime: double;
    //linkId: AnsiString;
    //laneId: AnsiString;
    connected: Boolean;
    totalTrajectory: Boolean;
    updateFrequency: double;
    // change
    totalGTUDistance: double;
    totalGTUTravelTime: double;
    averageGTUSpeed: double;
    averageGTUTravelTime: double;
    totalGTUTimeDelay: double;
    averageTripLength: double;
    totalNumberStops: double;
  private
    fScenario: TScenario;
    fDomain: string;
    fID: string;
    fCharts: TList<TChart>;
  public
    property scenario: TScenario read fScenario;
    property domain: string read fDomain;
    property ID: string read fID;
    property charts: TList<TChart> read fCharts;
  public
    function new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
    function change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
    function delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
  end;

  TSSMScenario = class (TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
  destructor Destroy; override;
  protected
    fGTUStatisticEvent: TIMBEventEntry;

    fStatistics: TObjectDictionary<string, TSSMStatistic>;
    fRunning: Boolean;
    fSpeed: Double;

    fSIMStartEvent: TIMBEventEntry;
    fSIMStopEvent: TIMBEventEntry;
    fSIMSpeedEvent: TIMBEventEntry;
  public
    procedure HandleGTUStatisticEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;

    procedure HandleSimStartEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleSimStopEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleSimSpeedEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  end;

  TSSMProject  = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMapView: TMapView; aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    fIMB3Connection: TIMBConnection;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
  public
    procedure SendDomains(aClient: TClient; const aPrefix: string); override;

    procedure ReadBasicData(); override;
    procedure handleClientMessage(aJSONObject: TJSONObject; aScenario: TScenario); override;
    procedure handleNewClient(aClient: TClient); override;
    property imb3Connection: TIMBConnection read fIMB3Connection;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;



implementation

{ TSSMCar }

function TSSMCar.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
//
//  procedure addResult(const aName, aValue: string);
//  begin
//    if Result<>''
//    then Result := Result+',';
//    Result := Result+'"'+aName+'":'+aValue;
//  end;
//
var
  _x: Double;
  _y: Double;
  _turnIndicatorStatus: string;
  _brakingLights: boolean;
begin
  Result := '';
  // start after timestamp and gtuId
  aPayload.Read(_x);
  aPayload.Read(_y);
  // project point always and use offset as an extra option to put a 0 based network in a location
  latlon.X := _x;
  latlon.Y := _y;
  latlon := aSourceProjection.ToGeocs(latlon);
  // check for changes
  if x<>_x then
  begin
    layer.UpdateObjectAttribute(ID, 'lng', DoubleToJSON(latlon.x));
    //addResult('lng', DoubleToJSON(latlon.x));
    x := _x;
  end;
  if y<>_y then
  begin
    layer.UpdateObjectAttribute(ID, 'lat', DoubleToJSON(latlon.y));
    //addResult('lat', DoubleToJSON(latlon.y));
    y := _y;
  end;
  aPayload.Read(z);
  aPayload.Read(rotZ);
  aPayload.Read(networkId);
  aPayload.Read(linkId);
  aPayload.Read(laneId);
  aPayload.Read(longitudinalPosition);
  aPayload.Read(speed);
  aPayload.Read(acceleration);
  aPayload.Read(_turnIndicatorStatus);
  if turnIndicatorStatus<>_turnIndicatorStatus then
  begin
    // do not use turn signal when braking lights are on
    if not _brakingLights then
    begin
      if (_turnIndicatorStatus='NONE') or (_turnIndicatorStatus='NOTPRESENT') then
      begin
        layer.UpdateObjectAttribute(ID, 'style.opacity', 'o');
      end
      else
      begin
        layer.UpdateObjectAttribute(ID, 'style.color', '"yellow"');
        layer.UpdateObjectAttribute(ID, 'style.opacity', '1');
      end;
    end;
    //addResult('tis', '"'+_turnIndicatorStatus+'"');
    turnIndicatorStatus := _turnIndicatorStatus;
  end;
  aPayload.Read(_brakingLights);
  if brakingLights<>_brakingLights then
  begin
    if _brakingLights then
    begin
      layer.UpdateObjectAttribute(ID, 'style.color', '"red"'); //addResult('bl', 'true')
      layer.UpdateObjectAttribute(ID, 'style.opacity', '1');
    end
    else
    begin
      layer.UpdateObjectAttribute(ID, 'style.opacity', '0');//addResult('bl', 'false');
    end;
    brakingLights := _brakingLights;
  end;
  aPayload.Read(odometer);
end;

function TSSMCar.delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem):  string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

begin
  Result := ''; // for now no repsonse so no projection needed
  aPayload.Read(x);
  aPayload.Read(y);
  aPayload.Read(z);
  aPayload.Read(rotZ);
  aPayload.Read(networkId);
  aPayload.Read(linkId);
  aPayload.Read(laneId);
  aPayload.Read(longitudinalPosition);
  aPayload.Read(odometer);
end;

function TSSMCar.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
begin
  Result := Infinite; // todo:
end;

function TSSMCar.getGeoJSON2D(const aType: string): string;
begin
  Result := ''; // todo:
end;

function TSSMCar.intersects(aGeometry: TWDGeometry): Boolean;
begin
  Result := False; // todo:
end;

function TSSMCar.new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
//
//  procedure addResult(const aName, aValue: string);
//  begin
//    if Result<>''
//    then Result := Result+',';
//    Result := Result+'"'+aName+'":'+aValue;
//  end;
//
var
  R, G, B: byte;
begin
  Result := '';
  // start after timestamp and gtuId
  aPayload.Read(x);
  aPayload.Read(y);
  // project point always and use offset as an extra option to put a 0 based network in a location
  latlon.X := x;
  latlon.Y := y;
  latlon := aSourceProjection.ToGeocs(latlon);
  layer.AddObjectAttribute(ID, 'lng', DoubleToJSON(latlon.x));
  layer.AddObjectAttribute(ID, 'lat', DoubleToJSON(latlon.y));
//  addResult('lng', DoubleToJSON(latlon.X));
//  addResult('lat', DoubleToJSON(latlon.Y));
  aPayload.Read(z);
  aPayload.Read(rotZ);
  aPayload.Read(networkId);
  aPayload.Read(linkId);
  aPayload.Read(laneId);
  aPayload.Read(longitudinalPosition);
  aPayload.Read(length);
  aPayload.Read(width);
  aPayload.Read(R);
  aPayload.Read(G);
  aPayload.Read(B);
  baseColor :=  RGBToAlphaColor(R, G, B);
  //Log.WriteLn('color '+baseColor.ToHexString(8), llNormal, 1);
  layer.AddObjectAttribute(ID, 'style.fillcolor', '"'+ColorToJSON(baseColor)+'"');
  //addResult('fill', '"'+ColorToJSON(baseColor)+'"');
end;

{
function TSSMCar.toJSONNew: string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

begin
  addResult('lng', DoubleToJSON(latlon.X));
  addResult('lat', DoubleToJSON(latlon.Y));
  addResult('fill', '"'+ColorToJSON(baseColor)+'"');
end;
}

{ TSSMStatistic }

function TSSMStatistic.new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  i: Integer;

  metadataId: AnsiString;
  metadataType: AnsiString;
  metadataValue: AnsiString;

  numberSpaceTimeRegions: integer;

  startTime: double;
  endTime: double;
  linkId: AnsiString;
  laneId: AnsiString;
begin
  Result := '';
  // start after timestamp and various Ids
  aPayload.Read(description);
  aPayload.Read(networkId);
  aPayload.Read(numberMetadataEntries);
  for i:=1 to numberMetadataEntries do
  begin
    aPayload.Read(metadataId);
    aPayload.Read(metadataType);
    aPayload.Read(metadataValue);
  end;
  aPayload.Read(numberSpaceTimeRegions);
  for i:=1 to numberSpaceTimeRegions do
  begin
    aPayload.Read(startTime);
    aPayload.Read(endTime);
    aPayload.Read(linkId);
    aPayload.Read(laneId);
  end;
  aPayload.Read(connected);
  aPayload.Read(totalTrajectory);
  aPayload.Read(updateFrequency);
  (*
  Result:='{"id":"'+ string(self.ID)+'-KPI01", "name": "Totale voertuigkilometers", "x": {"label": "simulatie seconden"}, "y": [{"label":"meters","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI02", "name": "Totale reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI03", "name": "Gemiddelde snelheid", "x": {"label": "simulatie seconden"}, "y": [{"label":"m/s","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI04", "name": "Gemiddelde reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI05", "name": "Voertuigverliesuren", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI06", "name": "Gemiddelde ritlengte", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI07", "name": "Voertuigstops", "x": {"label": "simulatie seconden"}, "y": [{"label":"stops","color":"LightBlue"}]}';
  *)
end;
(*
function TSSMStatistic.toJSONChange: string;
begin

  Result:='{"id":"'+ string(self.ID)+'-KPI01","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUDistance)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI02","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTravelTime)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI03","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUSpeed)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI04","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUTravelTime)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI05","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTimeDelay)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI06","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageTripLength)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI07","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalNumberStops)+']}';
end;

function TSSMStatistic.toJSONNew: string;
begin
  Result:='{"id":"'+ string(self.ID)+'-KPI01", "name": "Totale voertuigkilometers", "x": {"label": "simulatie seconden"}, "y": [{"label":"meters","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI02", "name": "Totale reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI03", "name": "Gemiddelde snelheid", "x": {"label": "simulatie seconden"}, "y": [{"label":"m/s","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI04", "name": "Gemiddelde reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI05", "name": "Voertuigverliesuren", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI06", "name": "Gemiddelde ritlengte", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI07", "name": "Voertuigstops", "x": {"label": "simulatie seconden"}, "y": [{"label":"stops","color":"LightBlue"}]}';
end;
*)

function TSSMStatistic.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

begin
  Result := '';
  // start after timestamp and gtuId
  aPayload.Read(totalGTUDistance);
  aPayload.Read(totalGTUTravelTime);
  aPayload.Read(averageGTUSpeed);
  aPayload.Read(averageGTUTravelTime);
  aPayload.Read(totalGTUTimeDelay);
  aPayload.Read(averageTripLength);
  aPayload.Read(totalNumberStops);
  (*
  Result:='{"id":"'+ string(self.ID)+'-KPI01","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUDistance)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI02","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTravelTime)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI03","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUSpeed)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI04","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUTravelTime)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI05","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTimeDelay)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI06","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageTripLength)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI07","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalNumberStops)+']}';
  *)
end;

constructor TSSMStatistic.Create(aScenario: TScenario; const aDomain, aID: string);
begin
  inherited Create;
  newTimestamp := Double.NaN;

  fScenario := aScenario;
  fDomain := aDomain;
  fID := aID;
  fCharts := TList<TChart>.Create;
end;

function TSSMStatistic.delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem):  string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

begin
  (*
  Result:='{"id":"'+ string(self.ID)+'-KPI01"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI02"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI03"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI04"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI05"}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI06"}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI07"}';
  *)
end;

destructor TSSMStatistic.Destroy;
begin
  FreeAndNil(fCharts);
  inherited;
end;

{ TSSMLink }

procedure TSSMLink.AddGTU(aSpeed: Double);
begin
  fTotalSpeed := fTotalSpeed+aSpeed;
  fNumberOfVehicles := fNumberOfVehicles+1;
  if fNumberOfVehicles>0
  then fValue := fTotalSpeed/fNumberOfVehicles
  else fValue := double.NaN;
  // todo: update object
end;

procedure TSSMLink.ChangeGTU(aOldSpeed, aNewSpeed: Double);
begin
  fTotalSpeed := fTotalSpeed-aOldSpeed+aNewSpeed;
  if fNumberOfVehicles>0
  then fValue := fTotalSpeed/fNumberOfVehicles
  else fValue := double.NaN;
  // todo: update object
end;

constructor TSSMLink.Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry);
begin
  inherited Create(aLayer, aID, aGeometry, 0);
  fTotalSpeed := 0;
  fNumberOfVehicles := 0;
end;

procedure TSSMLink.RemoveGTU(aSpeed: Double);
begin
  fTotalSpeed := fTotalSpeed-aSpeed;
  fNumberOfVehicles := fNumberOfVehicles-1;
  if fNumberOfVehicles>0
  then fValue := fTotalSpeed/fNumberOfVehicles
  else fValue := double.NaN;
  // todo: update object
end;

//procedure TSSMLink.setTotalSpeed(const aValue: Double);
//begin
//  fTotalSpeed := aValue;
//  if fNumberOfVehicles>0
//  then fValue := fTotalSpeed/fNumberOfVehicles
//  else fValue := Double.NaN;
//   todo: update object on tiler
//end;

{ TSSMLinkLayer }

constructor TSSMLinkLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aCarLayer: TSSMCarLayer);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"link"', 'Line', 0);
  fCarLayer := aCarLayer;
  // link
  fLinkEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('Link_GTU');
  fLinkEvent.OnNormalEvent := HandleLinkEvent;
end;

procedure TSSMLinkLayer.HandleLinkEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: integer;
  timestamp: Double;
  networkId: string;
  linkId: AnsiString;
  startNodeId: string;
  endNodeId: string;
  numberOfPoints: Integer;
  x, y, z: Double;
  p: Integer;
  lo: TLayerObject;
  isVehicleAdded: Boolean;
  gtuId: AnsiString;
  countAfterEvent: Integer;
  speed: Double;
begin
  try
    aPayload.Read(action);
    aPayload.Read(timestamp);
    aPayload.Read(networkId);
    aPayload.Read(AnsiString(linkId));
    case action of
      actionNew:
        begin
          TMonitor.Enter(objects);
          try
            if not objects.TryGetValue(linkid, lo) then
            begin
              lo := TSSMLink.Create(Self, linkId, TWDGeometry.Create());
              objects.Add(linkId, lo);
            end
            else
            begin
              (lo as TSSMLink).geometry.parts.Clear;
              Log.WriteLn('Received new link on known link '+string(UTF8String(linkId)), llWarning);
            end;
          finally
            TMonitor.Exit(objects);
          end;
          aPayload.Read(startNodeId);
          aPayload.Read(endNodeId);
          aPayload.Read(numberOfPoints);
          for p := 0 to numberOfPoints-1 do
          begin
            aPayload.Read(x);
            aPayload.Read(y);
            aPayload.Read(z);
            (lo as TSSMLink).geometry.AddPoint(x, y, z);
          end;
        end;
      actionChange:
        begin
          aPayload.Read(isVehicleAdded);
          aPayload.Read(gtuId);
          aPayload.Read(countAfterEvent);

          fCarLayer.objectsLock.BeginRead;
          try
            if fCarLayer.objects.TryGetValue(gtuId, lo) and (lo is TSSMCar)
            then speed := (lo as TSSMCar).speed
            else
            begin
              speed := double.NaN;
              log.WriteLn('TSSMLinkLayer.HandleLinkEvent unknown gtu in link change: '+string(UTF8String(gtuId)), llError);
            end;
          finally
            fCarLayer.objectsLock.EndRead;
          end;

          TMonitor.Enter(objects);
          try
            if objects.TryGetValue(linkId, lo) then
            begin
              if not speed.IsNan then
              begin
                if isVehicleAdded
                then (lo as TSSMLink).AddGTU(speed)
                else (lo as TSSMLink).RemoveGTU(speed);
              end;
            end
            else Log.WriteLn('Received change link on unknown link '+string(UTF8String(linkId)), llError);
          finally
            TMonitor.Exit(objects);
          end;
        end;
      actionDelete:
        begin
          TMonitor.Enter(objects);
          try
            if objects.TryGetValue(linkId, lo) then
            begin
              objects.Remove(linkId);
            end
            else Log.WriteLn('Received delete link on unknown link '+string(UTF8String(linkId)), llError);
          finally
            TMonitor.Exit(objects);
          end;
        end;
    end;
  except
    on E: Exception
    do log.WriteLn('Exception in TSSMLinkLayer.HandleLinkEvent: '+E.Message, llError);
  end;
end;

{ TSSMCarLayer }

constructor TSSMCarLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aLinkLayer: TSSMLinkLayer);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"car"', 'Point', 0);
  fLinkLayer := aLinkLayer;
  // GTU
  fGTUEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('GTU');
  fGTUEvent.OnNormalEvent := HandleGTUEvent;
end;

function TSSMCarLayer.HandleClientSubscribe(aClient: TClient): Boolean;
var
  iop: TPair<TWDID, TLayerObject>;
begin
  result := inherited;
  // send new cars
  for iop in objects do
  begin
    if iop.Value is TSSMCar then
    begin
      // todo: aClient.signalString('{"addcar": [{'+'"id":"'+string(UTF8String(iop.Key))+'",'+(iop.Value as TSSMCar).toJSONNew+'}]}');
    end;
  end;
end;

function TSSMCarLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  // todo: option to clear objects

  Result := inherited;
end;

procedure TSSMCarLayer.HandleGTUEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: Integer;
  timestamp: Double;
  gtuId: string;
  lo: TLayerObject;
  car: TSSMCar;
  oldSpeed: Double;
  newSpeed: Double;
  link: TLayerObject;
//  jsonStr: string;
begin
  aPayload.Read(action);
  aPayload.Read(timestamp);
  aPayload.Read(gtuId);
  //Log.WriteLn('event length '+l.ToString()+': '+action.ToString()+' ('+gtuId+')');
  case action of
    actionNew:
      begin
        if not FindObject(TWDID(gtuId), lo) then
        begin
          car := TSSMCar.Create(Self, TWDID(gtuId));
          car.newTimestamp := timestamp;
          car.new(aPayload, (scenario.project as TSSMProject).sourceProjection);
          AddObject(car);
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: new, already known car id '+gtuId, llError);
        {
        begin
          car := lo as TSSMCar;
          car.newTimestamp := timestamp;
          car.new(aPayload, (scenario.project as TSSMProject).sourceProjection);
        end;
        }
        //jsonStr := '"id":"'+string(UTF8String(gtuId))+'",'+car.new(aPayload, (scenario.project as TSSMProject).sourceProjection);
        //scenario.project.SendString('{"addcar": [{'+jsonStr+'}]}');
      end;
    actionChange:
      begin
        if FindObject(TWDID(gtuId), lo) then
        begin
          car := lo as TSSMCar;
          car.changedTimestamp := timestamp;
          oldSpeed := car.speed;
          car.change(aPayload, (scenario.project as TSSMProject).sourceProjection);
          newSpeed := car.speed;
          if (oldSpeed<>newSpeed) and Assigned(fLinkLayer) then
          begin
            if fLinkLayer.objects.TryGetValue(car.linkId, link) then
            begin
              if link is TSSMLink
              then (link as TSSMLink).ChangeGTU(oldSpeed, newSpeed)
              else log.WriteLn('TSSMCarLayer.HandleGTUEvent: object is not a link in gtu change of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
            end
            else log.WriteLn('TSSMCarLayer.HandleGTUEvent: link not found in gtu change of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
          end;
          // todo: handle attribute updates
          //UpdateObject(car.ID, '', '');
          //jsonStr := car.change(aPayload, (scenario.project as TSSMProject).sourceProjection);
          //if jsonStr<>'' then
          //begin
          //  jsonStr := '"id":"'+string(UTF8String(gtuId))+'",'+jsonStr;
          //  scenario.project.SendString('{"updatecar": [{'+jsonStr+'}]}');
          //end;
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: change, unknown car id '+gtuId, llError);
      end;
    actionDelete:
      begin
        if FindObject(TWDID(gtuId), lo) then
        begin
          car := lo as TSSMCar;
          car.deletedTimestamp := timestamp;
          car.delete(aPayload, (scenario.project as TSSMProject).sourceProjection);
          RemoveObject(car);
//          jsonStr := '"id":"'+string(UTF8String(gtuId))+'"'; // +car.delete(aPayload, (scenario.project as TSSMProject).sourceProjection)}
//          scenario.project.SendString('{"removecar": [{'+jsonStr+'}]}');
//          objects.Remove(car.ID);
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: delete, unknown car id '+gtuId, llError);
      end;
  end;
end;

(*
procedure TSSMCarLayer.HandleGTUSensorEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: Integer;
  timestamp: Double;
  networkId: AnsiString;
  linkId: AnsiString;
  laneId: AnsiString;
  sensorId: AnsiString;
  combiId: AnsiString;
  lo: TLayerObject;
  //sensor: TSSMSensor;
  jsonStr: string;
  //l: Integer;
begin
  //l := aPayload.Length;
  aPayload.Read(action);
  aPayload.Read(timestamp);
  aPayload.Read(networkId);
  aPayload.Read(linkId);
  aPayload.Read(laneId);
  aPayload.Read(sensorId);
  combiId:=networkId+':'+linkId+':'+laneId+':'+sensorId; // unique
  //Log.WriteLn('event length '+l.ToString()+': '+action.ToString()+' ('+gtuId+')');
  case action of
    actionNew:
      begin
        if not objects.TryGetValue(RawByteString(combiId), lo) then
        begin
          sensor := TSSMSensor.Create(Self, RawByteString(combiId));
          objects.Add(sensor.ID, sensor);
        end
        else sensor := lo as TSSMSensor;
        sensor.newTimestamp := timestamp;
        jsonStr := '"id":"'+string(UTF8String(combiId))+'",'+sensor.new(aPayload, (scenario.project as TSSMProject).sourceProjection);
        scenario.project.SendString('{"newGTUsensor": [{'+jsonStr+'}]}');
      end;
    actionChange:
      begin
        if objects.TryGetValue(RawByteString(combiId), lo) then
        begin
          sensor := lo as TSSMSensor;
          sensor.changedTimestamp := timestamp;
          jsonStr := sensor.change(aPayload, (scenario.project as TSSMProject).sourceProjection);
          if jsonStr<>'' then
          begin
            jsonStr := '"id":"'+string(UTF8String(combiId))+'",'+jsonStr;
            scenario.project.SendString('{"updateGTUsensor": [{'+jsonStr+'}]}');
          end;
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: change, unknown sensor id '+string(UTF8String(combiId)), llError);
      end;
    actionDelete:
      begin
        if objects.TryGetValue(RawByteString(combiId), lo) then
        begin
          sensor := lo as TSSMSensor;
          sensor.deletedTimestamp := timestamp;
          jsonStr := '"id":"'+string(UTF8String(combiId))+'",'+sensor.delete(aPayload, (scenario.project as TSSMProject).sourceProjection);
          scenario.project.SendString('{"removeGTUsensor": [{'+jsonStr+'}]}');
          objects.remove(sensor.id);
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: delete, unknown sensor id '+string(UTF8String(combiId)), llError);
      end;
  end;
end;

*)

{ TSSMScenario }

constructor TSSMScenario.Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
begin
  inherited;
  // statistics
  fStatistics := TObjectDictionary<string, TSSMStatistic>.Create;
  fGTUStatisticEvent := (project as TSSMProject).imb3Connection.Subscribe('StatisticsGTULane');
  fGTUStatisticEvent.OnNormalEvent := HandleGTUStatisticEvent;
    // simulation start
  fSIMStartEvent := (project as TSSMProject).imb3Connection.Subscribe('Sim_Start');
  fSIMStartEvent.OnNormalEvent := HandleSIMStartEvent;
  // simulation stop
  fSIMStopEvent := (project as TSSMProject).imb3Connection.Subscribe('Sim_Stop');
  fSIMStopEvent.OnNormalEvent := HandleSIMStopEvent;
  // simulation speed
  fSIMSpeedEvent := (project as TSSMProject).imb3Connection.Subscribe('Sim_Speed');
  fSIMSpeedEvent.OnNormalEvent := HandleSimSpeedEvent;
end;

destructor TSSMScenario.Destroy;
begin
  fGTUStatisticEvent.UnSubscribe;
  fSIMStartEvent.UnSubscribe;
  fSIMStopEvent.UnSubscribe;
  fSIMSpeedEvent.UnSubscribe;
  FreeAndNil(fStatistics);
  inherited;
end;

procedure TSSMScenario.HandleGTUStatisticEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: Integer;
  timestamp: Double;
  statisticId: string;
  stat: TSSMStatistic;
  chart: TChart;
begin
  aPayload.Read(action);
  aPayload.Read(timestamp);
  aPayload.Read(statisticId);
  case action of
    actionNew:
      begin
        if not fStatistics.TryGetValue(statisticId, stat) then
        begin
          stat := TSSMStatistic.Create(Self, 'domain', statisticId); // todo: domain
          fStatistics.Add(stat.ID, stat);
        end;
        stat.newTimestamp := timestamp;
        stat.new(aPayload, (project as TSSMProject).sourceProjection);
        stat.charts.Add(TChart.Create(Self, 'domain', '0', 'Totale voertuigkilometers', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('km', 'lightBlue', 'Length', 'km')]));
        stat.charts.Add(TChart.Create(Self, 'domain', '1', 'Totale reistijd', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min')]));
        stat.charts.Add(TChart.Create(Self, 'domain', '2', 'Gemiddelde snelheid', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('km/u', 'lightBlue', 'Velocity', 'km/h')]));
        stat.charts.Add(TChart.Create(Self, 'domain', '3', 'Gemiddelde reistijd', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min')]));
        stat.charts.Add(TChart.Create(Self, 'domain', '4', 'Voertuigverliesuren', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('hours', 'lightBlue', 'Time', 'h')]));
        stat.charts.Add(TChart.Create(Self, 'domain', '5', 'Gemiddelde ritlengte', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('km', 'lightBlue', 'Length', 'km')]));
        stat.charts.Add(TChart.Create(Self, 'domain', '6', 'Voertuigstops', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('stops', 'lightBlue', 'Dimensionless', '-')]));

        // add globally
        for chart in stat.charts
        do AddChart(chart);

        project.forEachClient(procedure(aClient: TClient)
          begin
            project.SendDomains(aClient, 'updatedomains');
          end);



        (*
        Result:=           '{"id":"' + string(self.ID)+'-KPI01", "name": "Totale voertuigkilometers", "x": {"label": "simulatie seconden"}, "y": [{"label":"meters","color":"LightBlue"}]}';
        Result:=Result + ', {"id":"' + string(self.ID)+'-KPI02", "name": "Totale reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
        Result:=Result + ', {"id":"' + string(self.ID)+'-KPI03", "name": "Gemiddelde snelheid", "x": {"label": "simulatie seconden"}, "y": [{"label":"m/s","color":"LightBlue"}]}';
        Result:=Result + ', {"id":"' + string(self.ID)+'-KPI04", "name": "Gemiddelde reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
        Result:=Result + ', {"id":"' + string(self.ID)+'-KPI05", "name": "Voertuigverliesuren", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
        Result:=Result + ', {"id":"' + string(self.ID)+'-KPI06", "name": "Gemiddelde ritlengte", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
        Result:=Result + ', {"id":"' + string(self.ID)+'-KPI07", "name": "Voertuigstops", "x": {"label": "simulatie seconden"}, "y": [{"label":"stops","color":"LightBlue"}]}';
        *)

//        jsonStr := stat.new(aPayload, (project as TSSMProject).sourceProjection);
//        project.SendString('{"newGTUstatistics": ['+jsonStr+']}');
      end;
    actionChange:
      begin
        if fStatistics.TryGetValue(statisticId, stat) then
        begin
          stat.changedTimestamp := timestamp;
          stat.change(aPayload, (project as TSSMProject).sourceProjection);
          stat.charts[0].AddValue(stat.changedTimestamp, [stat.totalGTUDistance]);
          stat.charts[1].AddValue(stat.changedTimestamp, [stat.totalGTUTravelTime]);
          stat.charts[2].AddValue(stat.changedTimestamp, [stat.averageGTUSpeed]);
          stat.charts[3].AddValue(stat.changedTimestamp, [stat.averageGTUTravelTime]);
          stat.charts[4].AddValue(stat.changedTimestamp, [stat.totalGTUTimeDelay]);
          stat.charts[5].AddValue(stat.changedTimestamp, [stat.averageTripLength]);
          stat.charts[6].AddValue(stat.changedTimestamp, [stat.totalNumberStops]);

          (*
          DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUDistance)+']}';
          Result:=Result + ',{"id":"' + string(self.ID)+'-KPI02","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTravelTime)+']}';
          Result:=Result + ',{"id":"' + string(self.ID)+'-KPI03","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUSpeed)+']}';
          Result:=Result + ',{"id":"' + string(self.ID)+'-KPI04","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUTravelTime)+']}';
          Result:=Result + ',{"id":"' + string(self.ID)+'-KPI05","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTimeDelay)+']}';
          Result:=Result + ',{"id":"' + string(self.ID)+'-KPI06","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageTripLength)+']}';
          Result:=Result + ',{"id":"' + string(self.ID)+'-KPI07","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalNumberStops)+']}';

          aPayload.Read(totalGTUDistance);
          aPayload.Read(totalGTUTravelTime);
          aPayload.Read(averageGTUSpeed);
          aPayload.Read(averageGTUTravelTime);
          aPayload.Read(totalGTUTimeDelay);
          aPayload.Read(averageTripLength);
          aPayload.Read(totalNumberStops);
          *)
//          jsonStr := stat.change(aPayload, (project as TSSMProject).sourceProjection);
//          if jsonStr<>''  then
//          begin
//            project.SendString('{"updateGTUstatistics": ['+jsonStr+']}');
//          end;
        end
        else Log.WriteLn('TSSMScenario.HandleGTUStatisticEvent: change, unknown sensor id '+statisticId, llError);
      end;
    actionDelete:
      begin
        if fStatistics.TryGetValue(statisticId, stat) then
        begin
          stat.deletedTimestamp := timestamp;
          stat.delete(aPayload, (project as TSSMProject).sourceProjection);
//          jsonStr := stat.delete(aPayload, (project as TSSMProject).sourceProjection);
//          project.SendString('{"removeGTUsensor": ['+jsonStr+']}');

          // todo: implement
          //for chart in stat.charts
          //do RemoveChart(chart);

          fStatistics.Remove(stat.id);
        end
        else Log.WriteLn('TSSMScenario.HandleGTUStatisticEvent: delete, unknown sensor id '+statisticId, llError);
      end;
  end;
end;

procedure TSSMScenario.HandleSimSpeedEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  aPayload.Read(fSpeed);
  project.SendString('{"simulationControl":{"speed":'+DoubleToJSON(fSpeed)+'}}');
end;

procedure TSSMScenario.HandleSimStartEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  fRunning := True;
  project.SendString('{"simulationControl":{"start":true}}');
end;

procedure TSSMScenario.HandleSimStopEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  fRunning := False;
  project.SendString('{"simulationControl":{"stop":true}}');
end;

{ TSSMProject }

constructor TSSMProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMapView: TMapView; aMaxNearestObjectDistanceInMeters: Integer);
begin
  fIMB3Connection := TIMBConnection.Create(
    GetSetting('IMB3RemoteHost', 'vps17642.public.cloudvps.com'),
    GetSetting('IMB3RemotePort', 4000),
    'PublishingServerSSM', 4,
    GetSetting('IMB3Prefix', 'OTS_RT'));
  mapView := aMapView;
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT(
    GetSetting('Projection', 'Amersfoort_RD_New')); // EPSG: 28992
  inherited Create(aSessionModel, aConnection,  aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection,
    aTimeSlider, aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers,
    aMaxNearestObjectDistanceInMeters);
end;

destructor TSSMProject.Destroy;
begin
  FreeAndNil(fIMB3Connection);
  inherited;
end;

procedure TSSMProject.handleClientMessage(aJSONObject: TJSONObject; aScenario: TScenario);
var
  jsonPair: TJSONPair;
  jsonValue: TJSONValue;
  EmptyPayload: ByteBuffers.TByteBuffer;
  speed: Double;
  speedPayload: ByteBuffers.TByteBuffer;
  isp: TPair<string, TScenario>;
begin
  if isObject(aJSONObject, 'simulationControl', jsonPair) then
  begin
    EmptyPayload.Clear();
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'stop', jsonValue) then
    begin
      for isp in scenarios do
      begin
        if isp.Value is TSSMScenario
        then (isp.Value as TSSMScenario).fSIMStopEvent.SignalEvent(ekNormalEvent, EmptyPayload);
      end;
      SendString('{"simulationControl":{"stop":true}}');
    end;
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'start', jsonValue) then
    begin
      for isp in scenarios do
      begin
        if isp.Value is TSSMScenario
        then (isp.Value as TSSMScenario).fSIMStartEvent.SignalEvent(ekNormalEvent, EmptyPayload);
      end;
      // signal status to clients
      SendString('{"simulationControl":{"start":true}}');
    end;
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'speed', jsonValue) then
    begin
      speed := jsonValue.getValue<Double>(); // todo: check if this works
      speedPayload.Clear();
      speedPayload.Write(speed);
      for isp in scenarios do
      begin
        if isp.Value is TSSMScenario
        then (isp.Value as TSSMScenario).fSIMSpeedEvent.SignalEvent(ekNormalEvent, speedPayload);
      end;
      // signal status to clients
      SendString('{"simulationControl":{"speed":'+DoubleToJSON(speed)+'}}');
    end;
  end
  else ;
end;

procedure TSSMProject.handleNewClient(aClient: TClient);
//var
//  isp: TPair<string, TScenario>;
//  ilp: TPair<string, TLayer>;
begin
  {
  for isp in scenarios do
  begin
    try
      for ilp in isp.value.layers do
      begin
        if ilp.Value is TSSMCarLayer
        then (ilp.Value as TSSMCarLayer).handleNewClient(aClient);
      end;
    except
      on E: Exception
      do Log.WriteLn('Exception in TSSMProject.handleNewClient: '+e.Message, llError);
    end;
  end;
  }
  // todo: new project status to client?
end;

procedure TSSMProject.ReadBasicData;
var
  gtuLayer: TSSMCarLayer;
  linkLayer: TSSMLinkLayer;
begin
  fCurrentScenario := TSSMScenario.Create(Self, 'woerden', 'Woerden', 'Woerden test', false, mapView);
  scenarios.Add(fCurrentScenario.ID, fCurrentScenario);
  // links
  linkLayer := TSSMLinkLayer.Create(fCurrentScenario, 'mobiliteit', 'LINK', 'LINK', 'LINK', false, nil);
  fCurrentScenario.Layers.Add(linkLayer.ID, linkLayer);
  // GTUs
  gtuLayer := TSSMCarLayer.Create(fCurrentScenario, 'mobiliteit', 'GTU', 'GTU', 'GTU', false, linkLayer);
  linkLayer.carLayer := gtuLayer;
  fCurrentScenario.Layers.Add(gtuLayer.ID, gtuLayer);
end;

procedure TSSMProject.SendDomains(aClient: TClient; const aPrefix: string);
var
  domains: TDictionary<string, TClientDomain>;
  locLayers: TList<TLayer>;
  layer: TLayer;
  JSON: string;
  refLayer: TLayer;
  _diffElementID: string;
  diffLayer: TDiffLayer;
  d: TClientDomain;
  nkp: TPair<string, TKPI>;
  ngp: TPair<string, TChart>;
  domainsJSON: string;
  ndp: TPair<string, TClientDomain>;
begin
  // todo: implement new layer system
  domains := TDictionary<string, TClientDomain>.Create;
  try
    if Assigned(fCurrentScenario) then
    begin
      // layers
      locLayers := TList<TLayer>.Create(TComparer<TLayer>.Construct(compareLayerNames));
      try
        locLayers.AddRange(fCurrentScenario.Layers.Values);
        locLayers.Sort;
        for layer in locLayers do
        begin
          JSON := layer.JSON;
          if Assigned(fRefScenario) then
          begin
            if fRefScenario.Layers.TryGetValue(layer.ID, refLayer) then
            begin
              // todo: full JSON for ref and diff, to include legend?
              JSON := JSON+',"ref":{'+refLayer.refJSON+'}';
              _diffElementID :=  diffElementID(layer, refLayer);
              TMonitor.Enter(diffLayers);
              try
                if not diffLayers.TryGetValue(_diffElementID, diffLayer) then
                begin
                  diffLayer := TDiffLayer.Create(_diffElementID, layer, refLayer);
                  diffLayers.Add(_diffElementID, diffLayer);
                  // todo:

                end;
              finally
                TMonitor.Exit(diffLayers);
              end;
              // todo: temp removed for testing
              JSON := JSON+',"diff":{'+diffLayer.refJSON+'}';
            end
            else Log.WriteLn('TClient.SendDomains ('+aPrefix+'): no ref layer for '+layer.ID);
          end;
          JSON  := '{'+JSON+'}';
          if domains.TryGetValue(layer.domain, d) then
          begin
            jsonAdd(d.layers, JSON);
            domains[layer.domain] := d;
          end
          else
          begin
            d := TClientDomain.Create(layer.domain);
            d.layers := JSON;
            domains.Add(d.name, d);
          end;
        end;
      finally
        locLayers.Free;
      end;
      // kpis
      for nkp in fCurrentScenario.KPIs do
      begin
        JSON := '{'+nkp.Value.JSON+'}';
        if domains.TryGetValue(nkp.Value.domain, d) then
        begin
          jsonAdd(d.kpis, JSON);
          domains[nkp.Value.domain] := d;
        end
        else
        begin
          d := TClientDomain.Create(nkp.Value.domain);
          d.kpis := JSON;
          domains.Add(d.name, d);
        end;
      end;
      // charts
      for ngp in fCurrentScenario.Charts do
      begin
        JSON := '{'+ngp.Value.JSON+'}';
        if domains.TryGetValue(ngp.Value.domain, d) then
        begin
          jsonAdd(d.charts, JSON);
          domains[ngp.Value.domain] := d;
        end
        else
        begin
          d := TClientDomain.Create(ngp.Value.domain);
          d.charts := JSON;
          domains.Add(d.name, d);
        end;
      end;
    end;
    domainsJSON := '';
    for ndp in domains do
    begin
      d := ndp.Value;
      // extra check to make domains enabled by default through entry in exe ini
      // todo: use const
      d.enabled := standardIni.ReadInteger('DefaultEnabledDomains', ndp.Key, 0);
      jsonAdd(domainsJSON, d.JSON);
    end;
    aClient.signalString('{"'+aPrefix+'":{'+domainsJSON+'}}'); // default prefix is "domains":..
  finally
    domains.Free;
  end;
end;

end.
