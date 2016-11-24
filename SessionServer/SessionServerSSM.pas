unit SessionServerSSM;

interface

uses
  Logger,
  IMB3NativeClient, IMB3Core, ByteBuffers,
  imb4,
  WorldDataCode, WorldLegends, WorldTilerConsts,
  Data.DB,
  StdIni,

  GisDefs, GisCsSystems,
  TimerPool,
  SessionServerLib,
  ModelControllerLib,
  WinApi.Windows,
  System.JSON,
  System.Generics.Collections, System.Generics.Defaults,
  System.SysUtils, System.Classes;

const
  gtCar ='car';
  gtCarEquiped = 'car_equipped';
  gtTruck = 'truck';
  gtTruckEquipped = 'truck_equipped';
  gtBus = 'bus';
  gtBusEquiped = 'bus_equipped';

  gtCarColor = $FFFF8080;
  gtCarEquipedColor = $FFFF0000;
  gtTruckColor = $FFFFFF80;
  gtTruckEquippedColor = $FFFFFF00;
  gtBusColor = $FF8080FF;
  gtBusEquipedColor = $FF0000FF;

  gtCarDescription ='Car';
  gtCarEquipedDescription = 'Car equipped';
  gtTruckDescription = 'Truck';
  gtTruckEquippedDescription = 'Truck equipped';
  gtBusDescription = 'Bus';
  gtBusEquipedDescription = 'Bus equipped';

const
  SSMIdlePrefix = 'USIdle'; // todo: ?

type
  TSSMCar = class(TLayerObject)
  public
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
    // v1.3
    gtuType: string;
    gtuTypeColor : TAlphaRGBPixel; // derived from gtuType

    // change/delete
    speed: Double;
    acceleration: Double;
    turnIndicatorStatus: string;
    brakingLights: Boolean;
    odometer: Double;
    // cache
    latlon: TGIS_Point;
  public
    function getJSON2D(const aType, aExtraJSON2DAttributes: string): string; override;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  public
    // imb 3 decoding
    function new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
    function change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
    function delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
  end;

  TSSMLinkLayer = class;

  TSSMCarLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean; aLinkLayer: TSSMLinkLayer);
  private
    fGTUEvent: TIMBEventEntry;
    fLinkLayer: TSSMLinkLayer;
    fPalette: TWDPalette;
  public
    procedure HandleGTUEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  public
    property linkLayer: TSSMLinkLayer read fLinkLayer write fLinkLayer;
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  public
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

  TSSMLink = class(TGeometryLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry);
  protected
    fNumberOfVehicles: Integer;
    fTotalSpeed: Double;
  public
    property numberOfVehicles: Integer read fNumberOfVehicles;
    property totalSpeed: Double read fTotalSpeed;

    procedure AddGTU(aSpeed: Double);
    procedure ChangeGTU(aOldSpeed, aNewSpeed: Double);
    procedure RemoveGTU(aSpeed: Double);
  end;

  TSSMLinkLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; aShowInDomains: Boolean; aDiffRange: Double; aCarLayer: TSSMCarLayer);
  destructor Destroy; override;
  private
    fLinkEvent: TIMBEventEntry;
    fCarLayer: TSSMCarLayer;
  public
    property carLayer: TSSMCarLayer read fCarLayer write fCarLayer;
    procedure HandleLinkEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  protected
    fLayerType: Integer;
    fPalette: TWDPalette;
  public
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
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
    property running: Boolean read fRunning;
    property speed: Double read fSpeed;
  public
    procedure HandleGTUStatisticEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;

    procedure HandleSimStartEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleSimStopEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleSimSpeedEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  end;

  TSSMSimulationParameter = record
  class function Create(const aName, aValue, aType: string): TSSMSimulationParameter; static;
  public
    name: string;
    value: string;
    _type: string;
    function toModelParameter: TModelParameter;
    function valueAsVariant: Variant;
  end;

  TSSMSimulationParameterList = class(TDictionary<string, TSSMSimulationParameter>)
  public
    procedure setParameter(const aName, aValue: string; const aType: string='string'); overload;
    procedure setParameter(const aName: string; aValue: Integer); overload;
    procedure setParameter(const aName: string; aValue: Double); overload;
    procedure setParameter(const aName: string; aValue: Boolean); overload;
  end;

  TSSMMCControlInterface = class(TMCControlInterface2)
  constructor Create(
    aConnection: TIMBConnection; const aFederation, aDataSource: string;
    aProject: TProject; const aIdleFederation: string=DefaultIdleFederation);
  private
    fProject: TProject;
  public
    procedure HandleModelChange(aModel: TCIModelEntry2; aChange: TMChange); override;
  public
    function jsonModelStatusNew(const aModelID, aModelName, aModelStatus: string; aModelProgress: Integer): string;
    function jsonModelStatusChange(const aModelID, aModelStatus: string; aModelProgress: Integer): string;
    function jsonModelStatusDelete(const aModelID: string): string;
    function jsonModelStatusArray(const aJSONModelStatusArrayContents: string): string;
  end;

  TSSMProject  = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
    aSimulationParameters: TSSMSimulationParameterList; const aSimulationSetup: string;
    aMapView: TMapView; aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
    fControlInterface: TSSMMCControlInterface;
    fSimulationParameters: TSSMSimulationParameterList;
    fSimulationSetupMessage: string;

  public
    procedure SendDomains(aClient: TClient; const aPrefix: string); override;

    function createSSMScenario(const aID, aName, aDescription: string): TSSMScenario;
    procedure closeSimulation();

    procedure ReadBasicData(); override;

    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
    procedure handleNewClient(aClient: TClient); override;

    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
    property controlInterface: TSSMMCControlInterface read fControlInterface;
    property simulationParameters: TSSMSimulationParameterList read fSimulationParameters;
  end;



implementation

{ TSSMCar }

function TSSMCar.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
var
  _x: Double;
  _y: Double;
  _turnIndicatorStatus: string;
  _brakingLights: boolean;
  linkId: AnsiString;
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
    x := _x;
  end;
  if y<>_y then
  begin
    layer.UpdateObjectAttribute(ID, 'lat', DoubleToJSON(latlon.y));
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
        layer.UpdateObjectAttribute(ID, 'opacity', '0');
      end
      else
      begin
        layer.UpdateObjectAttribute(ID, 'color', '"#FFFF00"');
        layer.UpdateObjectAttribute(ID, 'opacity', '1');
      end;
    end;
    turnIndicatorStatus := _turnIndicatorStatus;
  end;
  aPayload.Read(_brakingLights);
  if brakingLights<>_brakingLights then
  begin
    if _brakingLights then
    begin
      layer.UpdateObjectAttribute(ID, 'color', '"#FF0000"');
      layer.UpdateObjectAttribute(ID, 'opacity', '1');
    end
    else
    begin
      layer.UpdateObjectAttribute(ID, 'opacity', '0');
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

function TSSMCar.getJSON2D(const aType, aExtraJSON2DAttributes: string): string;
begin
  Result := ''; // todo:
end;

function TSSMCar.intersects(aGeometry: TWDGeometry): Boolean;
begin
  Result := False; // todo:
end;

function TSSMCar.new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
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
  if aPayload.ReadAvailable>0
  then aPayload.Read(gtuType)
  else gtuType := 'unkown';

  baseColor :=  RGBToAlphaColor(R, G, B);

  if gtuType=gtCar
  then  gtuTypeColor := gtCarColor
  else if gtuType=gtCarEquiped
  then  gtuTypeColor := gtCarEquipedColor
  else if gtuType=gtTruck
  then  gtuTypeColor := gtTruckColor
  else if gtuType=gtTruckEquipped
  then  gtuTypeColor := gtTruckEquippedColor
  else if gtuType=gtBus
  then  gtuTypeColor := gtBusColor
  else if gtuType=gtBusEquiped
  then  gtuTypeColor := gtBusEquipedColor
  else gtuTypeColor := baseColor;

  layer.AddObjectAttribute(ID, [
    TAttrNameValue.Create('lng', DoubleToJSON(latlon.x)),
    TAttrNameValue.Create('lat', DoubleToJSON(latlon.y)),
    TAttrNameValue.Create('fillColor', '"'+ColorToJSON(gtuTypeColor)+'"')]);
end;

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
end;

function TSSMStatistic.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem): string;
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
begin
  Result := '';
end;

destructor TSSMStatistic.Destroy;
begin
  FreeAndNil(fCharts);
  inherited;
end;

{ TSSMLink }

procedure TSSMLink.AddGTU(aSpeed: Double);
begin
  if not double.isNan(aSpeed)
  then fTotalSpeed := fTotalSpeed+aSpeed;
  fNumberOfVehicles := fNumberOfVehicles+1;
  if fNumberOfVehicles>0
  then fValue := fTotalSpeed/fNumberOfVehicles
  else fValue := double.NaN;
  layer.signalObject(Self);
end;

procedure TSSMLink.ChangeGTU(aOldSpeed, aNewSpeed: Double);
begin
  if not double.isNan(aOldSpeed)
  then fTotalSpeed := fTotalSpeed-aOldSpeed;
  if not double.isNan(aNewSpeed)
  then fTotalSpeed := fTotalSpeed+aNewSpeed;
  if fNumberOfVehicles>0
  then fValue := fTotalSpeed/fNumberOfVehicles
  else fValue := double.NaN;
  layer.signalObject(Self);
end;

constructor TSSMLink.Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry);
begin
  inherited Create(aLayer, aID, aGeometry, double.NaN);
  fTotalSpeed := 0;
  fNumberOfVehicles := 0;
end;

procedure TSSMLink.RemoveGTU(aSpeed: Double);
begin
  if not double.isNan(aSpeed)
  then fTotalSpeed := fTotalSpeed-aSpeed;
  fNumberOfVehicles := fNumberOfVehicles-1;
  if fNumberOfVehicles>0
  then fValue := fTotalSpeed/fNumberOfVehicles
  else fValue := double.NaN;
  layer.signalObject(Self);
end;

{ TSSMLinkLayer }

const
  NoSpeedColor: TAlphaRGBPixel = $FF000000;
  LowSpeedColor: TAlphaRGBPixel = $FFFF0000;
  MediumSpeedColor: TAlphaRGBPixel = $FFFFFF00;
  HighSpeedColor: TAlphaRGBPixel = $FF00FF00;

constructor TSSMLinkLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean; aDiffRange: Double; aCarLayer: TSSMCarLayer);
var
  entries: TPaletteRampEntryArray;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"link"', 'LineString',
    ltTile, aShowInDomains, aDiffRange);
  SetLength(entries, 3);
  entries[0] := TRampPaletteEntry.Create(LowSpeedColor, 0, 'low');
  entries[1] := TRampPaletteEntry.Create(MediumSpeedColor, 11.1, 'medium'); // about 40 km/u
  entries[2] := TRampPaletteEntry.Create(HighSpeedColor, 22.2, 'high'); // about 80 km/u
  fPalette := TRampPalette.Create('Link avagere speed', entries, LowSpeedColor, NoSpeedColor, HighSpeedColor);
  legendJSON := BuildRamplLegendJSON(fPalette as TRampPalette);
  fCarLayer := aCarLayer;
  // link
  fLinkEvent := (scenario.project as TSSMProject).controlInterface.Connection.Subscribe('Link_GTU');
  fLinkEvent.OnNormalEvent := HandleLinkEvent;
end;

destructor TSSMLinkLayer.Destroy;
begin
  FreeAndNil(fPalette);
  inherited;
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
  latlon: TGIS_Point;
begin
  try
    aPayload.Read(action);
    aPayload.Read(timestamp);
    aPayload.Read(networkId);
    aPayload.Read(linkId);
    case action of
      actionNew:
        begin
          if not FindObject(TWDID(linkid), lo) then
          begin
            lo := TSSMLink.Create(Self, linkId, TWDGeometry.Create());
            aPayload.Read(startNodeId);
            aPayload.Read(endNodeId);
            aPayload.Read(numberOfPoints);
            for p := 0 to numberOfPoints-1 do
            begin
              aPayload.Read(x);
              aPayload.Read(y);
              aPayload.Read(z);
              latlon.X := x;
              latlon.Y := y;
              latlon := (scenario.project as TSSMProject).sourceProjection.ToGeocs(latlon);
              (lo as TSSMLink).geometry.AddPoint(latlon.X, latlon.Y, z);
            end;
            AddObject(lo);
          end
          else Log.WriteLn('Received new link on known link '+string(UTF8String(linkId)), llWarning);
        end;
      actionChange:
        begin
          aPayload.Read(isVehicleAdded);
          aPayload.Read(gtuId);
          aPayload.Read(countAfterEvent);

          // NOT anymore: for now handled in gtu change event

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
          if FindObject(TWDID(linkid), lo) then
          begin
            RemoveObject(lo);
          end
          else Log.WriteLn('Received delete link on unknown link '+string(UTF8String(linkId)), llError);
        end;
    end;
  except
    on E: Exception
    do log.WriteLn('Exception in TSSMLinkLayer.HandleLinkEvent: '+E.Message, llError);
  end;
end;

procedure TSSMLinkLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500);
end;

procedure TSSMLinkLayer.RegisterSlice;
begin
  if Assigned(fPalette)
  then tilerLayer.signalAddSlice(fPalette.Clone)
  else tilerLayer.signalAddSlice(nil);
end;

function TSSMLinkLayer.SliceType: Integer;
begin
  Result := stGeometryI;
end;

{ TSSMCarLayer }

constructor TSSMCarLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean; aLinkLayer: TSSMLinkLayer);
var
  entries: TPaletteDiscreteEntryArray;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"car"', 'Point', ltObject, aShowInDomains, 0);
  fLinkLayer := aLinkLayer;
  // GTU
  fGTUEvent := (scenario.project as TSSMProject).controlInterface.Connection.Subscribe('GTU');
  fGTUEvent.OnNormalEvent := HandleGTUEvent;
  // legend
  SetLength(entries, 6);

  entries[0] := TDiscretePaletteEntry.Create(TGeoColors.Create(gtCarColor), 0, 2, gtCarDescription);
  entries[1] := TDiscretePaletteEntry.Create(TGeoColors.Create(gtCarEquipedColor), 2, 4, gtCarEquipedDescription);
  entries[2] := TDiscretePaletteEntry.Create(TGeoColors.Create(gtTruckColor), 4, 6, gtTruckDescription);
  entries[3] := TDiscretePaletteEntry.Create(TGeoColors.Create(gtTruckEquippedColor), 6, 8, gtTruckEquippedDescription);
  entries[4] := TDiscretePaletteEntry.Create(TGeoColors.Create(gtBusColor), 8, 10, gtBusDescription);
  entries[5] := TDiscretePaletteEntry.Create(TGeoColors.Create(gtBusEquipedColor), 10, 12, gtBusEquipedDescription);

  fPalette := TDiscretePalette.Create('Vehicle type', entries, TGeoColors.Create());
  legendJSON := BuildDiscreteLegendJSON(fPalette as TDiscretePalette, lfVertical);
end;

function TSSMCarLayer.HandleClientSubscribe(aClient: TClient): Boolean;
var
  iop: TPair<TWDID, TLayerObject>;
  _json: string;
  car: TSSMCar;
begin
  result := inherited;
  // send new cars
  _json := '';
  for iop in objects do
  begin
    if iop.Value is TSSMCar then
    begin
      car := iop.Value as TSSMCar;
      if _json<>''
      then _json  := _json+',';
      _json  := _json+
        '{"newobject":'+
          '{"id":"'+string(UTF8String(car.ID))+'",'+
           '"lng":'+DoubleToJSON(car.latlon.x)+','+
           '"lat":'+DoubleToJSON(car.latlon.y)+','+
           '"fillColor":"'+ColorToJSON(car.baseColor)+'"}}';
    end;
  end;
  if _json<>'' then
  begin
    aClient.signalString('{"type":"updatelayer","payload":{"id":"'+ElementID+'","data":['+_json+']}}');
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
  oldLinkId: AnsiString;
  newLinkId: AnsiString;
begin
  aPayload.Read(action);
  aPayload.Read(timestamp);
  aPayload.Read(gtuId);
  case action of
    actionNew:
      begin
        if not FindObject(TWDID(gtuId), lo) then
        begin
          car := TSSMCar.Create(Self, TWDID(gtuId));
          car.newTimestamp := timestamp;
          car.new(aPayload, (scenario.project as TSSMProject).sourceProjection);
          AddObject(car);
          {
          newLinkId := car.linkId;
          newSpeed := 0;
          TMonitor.Enter(fLinkLayer.objects);
          try
            if fLinkLayer.objects.TryGetValue(newLinkId, link) then
            begin
              if link is TSSMLink
              then (link as TSSMLink).AddGTU(newSpeed)
              else log.WriteLn('TSSMCarLayer.HandleGTUEvent: object is not a link in new gtu to link of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
            end;
          finally
            TMonitor.Exit(fLinkLayer.objects);
          end;
          }
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: new, already known car id '+gtuId, llError);
      end;
    actionChange:
      begin
        if FindObject(TWDID(gtuId), lo) then
        begin
          car := lo as TSSMCar;
          car.changedTimestamp := timestamp;
          oldSpeed := car.speed;
          oldLinkId := car.linkId;
          car.change(aPayload, (scenario.project as TSSMProject).sourceProjection);
          newSpeed := car.speed;
          newLinkId := car.linkId;
          {
          if (oldLinkId<>newLinkId) and Assigned(fLinkLayer) then
          begin
            TMonitor.Enter(fLinkLayer.objects);
            try
              if fLinkLayer.objects.TryGetValue(oldLinkId, link) then
              begin
                if link is TSSMLink
                then (link as TSSMLink).RemoveGTU(oldSpeed)
                else log.WriteLn('TSSMCarLayer.HandleGTUEvent: object is not a link in gtu remove from link of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
              end;
              if fLinkLayer.objects.TryGetValue(newLinkId, link) then
              begin
                if link is TSSMLink
                then (link as TSSMLink).AddGTU(newSpeed)
                else log.WriteLn('TSSMCarLayer.HandleGTUEvent: object is not a link in gtu add to link of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
              end;
            finally
              TMonitor.Exit(fLinkLayer.objects);
            end;
          end
          else
          begin
          }
          if (oldSpeed<>newSpeed) and Assigned(fLinkLayer) then
          begin
            TMonitor.Enter(fLinkLayer.objects);
            try
              if fLinkLayer.objects.TryGetValue(car.linkId, link) then
              begin
                if link is TSSMLink
                then (link as TSSMLink).ChangeGTU(oldSpeed, newSpeed)
                else log.WriteLn('TSSMCarLayer.HandleGTUEvent: object is not a link in gtu speed change of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
              end
              else log.WriteLn('TSSMCarLayer.HandleGTUEvent: link not found in gtu change of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
            finally
              TMonitor.Exit(fLinkLayer.objects);
            end;
          end;
            {
          end;
          }
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
          {
          oldLinkId := car.linkId;
          oldSpeed := car.speed; // last speed
          TMonitor.Enter(fLinkLayer.objects);
          try
            if fLinkLayer.objects.TryGetValue(oldLinkId, link) then
            begin
              if link is TSSMLink
              then (link as TSSMLink).RemoveGTU(oldSpeed)
              else log.WriteLn('TSSMCarLayer.HandleGTUEvent: object is not a link in delete gtu from link of '+string(UTF8String(gtuId))+', '+string(UTF8String(car.linkId)), llError);
            end;
          finally
            TMonitor.Exit(fLinkLayer.objects);
          end;
          }
          RemoveObject(car);
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: delete, unknown car id '+gtuId, llError);
      end;
  end;
end;

procedure TSSMCarLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500);
end;

procedure TSSMCarLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice(nil);
end;

function TSSMCarLayer.SliceType: Integer;
begin
  Result := stLocation;
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
  fGTUStatisticEvent := (project as TSSMProject).controlInterface.Connection.Subscribe('StatisticsGTULane');
  fGTUStatisticEvent.OnNormalEvent := HandleGTUStatisticEvent;
    // simulation start
  fSIMStartEvent := (project as TSSMProject).controlInterface.Connection.Subscribe('Sim_Start');
  fSIMStartEvent.OnNormalEvent := HandleSIMStartEvent;
  // simulation stop
  fSIMStopEvent := (project as TSSMProject).controlInterface.Connection.Subscribe('Sim_Stop');
  fSIMStopEvent.OnNormalEvent := HandleSIMStopEvent;
  // simulation speed
  fSIMSpeedEvent := (project as TSSMProject).controlInterface.Connection.Subscribe('Sim_Speed');
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
          stat := TSSMStatistic.Create(Self, '(1) All Vehicles', statisticId); // todo: domain
          fStatistics.Add(stat.ID, stat);
        end;
        stat.newTimestamp := timestamp;
        stat.new(aPayload, (project as TSSMProject).sourceProjection);
        // (1) All Vehicles
        // (2) Smart Mobility
        // (3) No Service
        stat.charts.Add(TChart.Create(Self, stat.domain, '0', 'Total vehicle kilometers', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('km', 'lightBlue', 'Length', 'km')]));
        stat.charts.Add(TChart.Create(Self, stat.domain, '1', 'Total travel time', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min')]));
        stat.charts.Add(TChart.Create(Self, stat.domain, '2', 'Average speed', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('km/h', 'lightBlue', 'Velocity', 'km/h')]));
        stat.charts.Add(TChart.Create(Self, stat.domain, '3', 'Average travel time', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min')]));
        stat.charts.Add(TChart.Create(Self, stat.domain, '4', 'Vehicle delay hours', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('hours', 'lightBlue', 'Time', 'h')]));
        stat.charts.Add(TChart.Create(Self, stat.domain, '5', 'Average trip length', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('km', 'lightBlue', 'Length', 'km')]));
        stat.charts.Add(TChart.Create(Self, stat.domain, '6', 'Vehicle stops', '', false, 'line',
          TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('stops', 'lightBlue', 'Dimensionless', '-')]));

        // add globally
        for chart in stat.charts
        do AddChart(chart);

        project.forEachClient(procedure(aClient: TClient)
          begin
            project.SendDomains(aClient, 'updatedomains');
          end);
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
        end
        else Log.WriteLn('TSSMScenario.HandleGTUStatisticEvent: change, unknown sensor id '+statisticId, llError);
      end;
    actionDelete:
      begin
        if fStatistics.TryGetValue(statisticId, stat) then
        begin
          stat.deletedTimestamp := timestamp;
          stat.delete(aPayload, (project as TSSMProject).sourceProjection);
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
var
  action: Integer;
begin
  aPayload.Read(action);
  aPayload.Read(fSpeed);
  project.SendString('{"simulationControl":{"speed":'+DoubleToJSON(fSpeed)+'}}');
end;

procedure TSSMScenario.HandleSimStartEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  // action
  fRunning := True;
  project.SendString('{"simulationControl":{"start":true}}');
end;

procedure TSSMScenario.HandleSimStopEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  // action
  fRunning := False;
  project.SendString('{"simulationControl":{"stop":true}}');
end;

{ TSSMSimulationParameter }

class function TSSMSimulationParameter.Create(const aName, aValue, aType: string): TSSMSimulationParameter;
begin
  Result.name := aName;
  Result.value := aValue;
  Result._type := aType;
end;

function TSSMSimulationParameter.toModelParameter: TModelParameter;
begin
  if _type='int'
  then Result := TModelParameter.Create(name, value.ToInteger)
  else if _type='float'
  then Result := TModelParameter.Create(name, Double.Parse(value, dotFormat))
  else if _type='bool'
  then Result := TModelParameter.Create(name, value<>'false')
  else if _type='string'
  then Result := TModelParameter.Create(name, value)
  else Result := nil;
end;

function TSSMSimulationParameter.valueAsVariant: Variant;
begin
  if _type='int'
  then Result := value.ToInteger
  else if _type='float'
  then Result := Double.Parse(value, dotFormat)
  else if _type='bool'
  then Result := Boolean(value<>'false')
  else if _type='string'
  then Result := value
  else Result := NullVar;
end;

{ TSSMSimulationParameterList }

procedure TSSMSimulationParameterList.setParameter(const aName, aValue, aType: string);
var
  param: TSSMSimulationParameter;
begin
  param := TSSMSimulationParameter.Create(aName, aValue, aType);
  AddOrSetValue(param.name, param);
end;

procedure TSSMSimulationParameterList.setParameter(const aName: string; aValue: Integer);
begin
  setParameter(aName, aValue.ToString, 'int')
end;

procedure TSSMSimulationParameterList.setParameter(const aName: string; aValue: Double);
begin
  setParameter(aName, aValue.ToString(dotFormat), 'float')
end;

procedure TSSMSimulationParameterList.setParameter(const aName: string; aValue: Boolean);
begin
  if aValue
  then setParameter(aName, 'true', 'bool')
  else setParameter(aName, 'false', 'bool');
end;

{ TSSMProject }

procedure TSSMProject.closeSimulation;
var
  cim: TCIModelEntry2;
begin
  for cim in controlInterface.Models do
  begin
    if (cim.State<>msIdle) and (AnsiCompareText(cim.Federation, controlInterface.Federation)=0) then
    begin
      controlInterface.UnclaimModel(cim);
    end;
  end;
end;

constructor TSSMProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
    aSimulationParameters: TSSMSimulationParameterList; const aSimulationSetup: string;
    aMapView: TMapView; aMaxNearestObjectDistanceInMeters: Integer);
begin
  if Assigned(aSimulationParameters)
  then fSimulationParameters := aSimulationParameters
  else fSimulationParameters := TSSMSimulationParameterList.Create;
  fControlInterface := TSSMMCControlInterface.Create(
    TIMBConnection.Create(
      GetSetting('IMB3RemoteHost', 'vps17642.public.cloudvps.com'),
      GetSetting('IMB3RemotePort', 4000),
      'PublishingServerSSM-'+aProjectID, 4,
      ''),
    GetSetting('IMB3Prefix-'+aProjectID, 'OTS_RT'),
    'ssm', // todo: datasource
    Self,
    SSMIdlePrefix);
  mapView := aMapView;
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT(
    GetSetting('Projection', 'Amersfoort_RD_New')); // EPSG: 28992
  inherited Create(aSessionModel, aConnection,  aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection,
    aTimeSlider, aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers,
    aSimulationSetup, aMaxNearestObjectDistanceInMeters);
end;

function TSSMProject.createSSMScenario(const aID, aName, aDescription: string): TSSMScenario;
var
  gtuLayer: TSSMCarLayer;
  linkLayer: TSSMLinkLayer;
  switchLayer: TLayerSwitch;
begin
  Result := TSSMScenario.Create(Self, aID, aName, aDescription, false, mapView);
  scenarios.Add(Result.ID, Result);
  // links
  linkLayer := TSSMLinkLayer.Create(Result, 'mobility', 'LINK', 'LINK', 'LINK', false, false, 10, nil);
  linkLayer.extraJSON2DAttributes := ',"weight":1';
  Result.Layers.Add(linkLayer.ID, linkLayer);
  linkLayer.RegisterLayer;
  // GTUs
  gtuLayer := TSSMCarLayer.Create(Result, 'mobility', 'GTU', 'GTU', 'GTU', false, false, linkLayer);
  gtuLayer.extraJSON2DAttributes := '';
  linkLayer.carLayer := gtuLayer;
  Result.Layers.Add(gtuLayer.ID, gtuLayer);
  gtuLayer.RegisterLayer;
  // switch
  switchLayer := TLayerSwitch.Create(Result, 'mobility', 'traffic', 'traffic', 'cars/intensity', true, '"car"', false);
  switchLayer.zoomLayers.Add(TLayerOnZoom.Create(0, linkLayer));
  switchLayer.zoomLayers.Add(TLayerOnZoom.Create(16, gtuLayer));
  Result.Layers.Add(switchLayer.ID, switchLayer);
  // todo: ? switchLayer.RegisterLayer;
end;

destructor TSSMProject.Destroy;
begin
  inherited;
  FreeAndNil(fControlInterface);
  FreeAndNil(fSimulationParameters);
end;

procedure TSSMProject.handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject);
var
  jsonPair: TJSONPair;
  jsonValue: TJSONValue;
  EmptyPayload: ByteBuffers.TByteBuffer;
  speed: Double;
  speedPayload: ByteBuffers.TByteBuffer;
  isp: TPair<string, TScenario>;
  _parameters: TJSONArray;
  parameter: TJSONValue;
  parameterName: string;
  parameterValue: string;
  parameterType: string;
  sp: TSSMSimulationParameter;
  scenario: TSSMScenario;
  modelNames: TStringList;
  cim: TCIModelEntry2;
  parameters: TModelParameters;
  mpn: string;
  nsp: TPair<string, TSSMSimulationParameter>;
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
      //SendString('{"simulationControl":{"stop":true}}');
    end;
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'start', jsonValue) then
    begin
      for isp in scenarios do
      begin
        if isp.Value is TSSMScenario
        then (isp.Value as TSSMScenario).fSIMStartEvent.SignalEvent(ekNormalEvent, EmptyPayload);
      end;
      //SendString('{"simulationControl":{"start":true}}');
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
      //SendString('{"simulationControl":{"speed":'+DoubleToJSON(speed)+'}}');
    end;
  end
  else if aJSONObject.TryGetValue<TJSONValue>('setupSimulation', jsonValue) then
  begin
    closeSimulation();
    if jsonValue.TryGetValue<TJSONArray>('parameters', _parameters) then
    begin
      for parameter in _parameters do
      begin
        if not parameter.TryGetValue<string>('name', parameterName)
        then parameterName := '';
        if not parameter.TryGetValue<string>('value', parameterValue)
        then parameterValue := '';
        if not parameter.TryGetValue<string>('type', parameterType)
        then parameterType := '';
        // process parameters and build and claim models etc..
        sp := TSSMSimulationParameter.Create(parameterName, parameterValue, parameterType);
        simulationParameters.AddOrSetValue(sp.name, sp);
      end;
    end;
    // create scenario
    scenario := createSSMScenario(TGUID.NewGuid.ToString, 'new simulation', simulationParameters['scenarioName'].value);
    aClient.currentScenario := scenario;
    // todo: translate case specific parameters

    // todo: switch to correct prefix
    if simulationParameters.ContainsKey('datasource')
    then controlInterface.DataSource := simulationParameters['datasource'].value;
    controlInterface.Federation := scenario.ID;

    // todo: start models (defined in simulation parameters)
    if simulationParameters.ContainsKey('models') then
    begin
      modelNames := TStringList.Create;
      try
        modelNames.Delimiter := ';';
        modelNames.StrictDelimiter := True;
        modelNames.DelimitedText := simulationParameters['models'].value;
        for cim in controlInterface.Models do
        begin
          if (cim.State=msIdle) and (modelNames.IndexOf(cim.ModelName)>=0) then
          begin
            if controlInterface.RequestModelDefaultParameters(cim) then
            begin
              parameters := TModelParameters.Create(cim.DefaultParameters);
              try
                // todo: set model specific parameters
                for nsp in simulationParameters do
                begin
                  if nsp.key.StartsWith(cim.ModelName+'-') then
                  begin
                    mpn := nsp.key.Substring(length(cim.ModelName+'-'));
                    if parameters.ParameterExists(mpn)
                    then parameters.Value[mpn] := nsp.value.valueAsVariant
                    else parameters.Add(nsp.Value.toModelParameter);
                  end;
                end;

                if not controlInterface.ClaimModel(cim, parameters)
                then log.WriteLn('TSSMProject.handleClientMessage: could not claim model '+cim.ModelName, llError);

              finally
                parameters.Free;
              end;
            end
            else log.WriteLn('TSSMProject.handleClientMessage: could request default parameters for model '+cim.ModelName, llError);
          end;
        end;
      finally
        modelNames.Free;
      end;
    end;

    //
    forEachClient(procedure(aClient: TClient)
      begin;
        SendDomains(aClient, 'updatedomains');
      end);
  end
  else if aJSONObject.TryGetValue<TJSONValue>('closeSimulation', jsonValue) then
  begin
    closeSimulation();
  end;
end;

procedure TSSMProject.handleNewClient(aClient: TClient);
var
  isp: TPair<string, TScenario>;
  model: TCIModelEntry2;
  jsonNewModels: string;
begin
  SendString(fSimulationSetupMessage);
  for isp in scenarios do
  begin
    try
      if (aClient.currentScenario=isp.Value) and (isp.Value is TSSMScenario) then
      begin
        if (isp.Value as TSSMScenario).running
        then aClient.signalString('{"simulationControl":{"start":true,"speed":'+DoubleToJSON((isp.Value as TSSMScenario).speed)+'}}')
        else aClient.signalString('{"simulationControl":{"stop":true}}');
      end;
    except
      on E: Exception
      do Log.WriteLn('Exception in TSSMProject.handleNewClient: '+e.Message, llError);
    end;
  end;
  // models state
  fControlInterface.Lock.Acquire;
  try
    jsonNewModels := '';
    for model in fControlInterface.Models do
    begin
      if (model.state=msIdle) or (fControlInterface.Federation='') or model.IsThisSession(fControlInterface.Federation) then
      begin
        if jsonNewModels<>''
        then jsonNewModels := jsonNewModels+',';
        jsonNewModels := jsonNewModels+controlInterface.jsonModelStatusNew(model.UID.ToString, model.ModelName, model.State.ToString, model.Progress)
      end;
    end;
    aClient.signalString(controlInterface.jsonModelStatusArray(jsonNewModels));
  finally
    fControlInterface.Lock.Release;
  end;
end;

procedure TSSMProject.ReadBasicData;
//var
//  scenario: TScenario;
begin
//  scenario := createSSMScenario('{sim}', 'new simulation', 'user defined name');
  // register scenario as the current
//  projectCurrentScenario := scenario;
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
    if Assigned(aClient.currentScenario) then
    begin
      // layers
      locLayers := TList<TLayer>.Create(TComparer<TLayer>.Construct(compareLayerNames));
      try
        locLayers.AddRange(aClient.currentScenario.Layers.Values);
        locLayers.Sort;
        for layer in locLayers do
        begin
          if layer.showInDomains then
          begin
            JSON := layer.JSON;
            if Assigned(aClient.refScenario) then
            begin
              if aClient.refScenario.Layers.TryGetValue(layer.ID, refLayer) then
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
        end;
      finally
        locLayers.Free;
      end;
      // kpis
      for nkp in aClient.currentScenario.KPIs do
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
      for ngp in aClient.currentScenario.Charts do
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

{ TSSMMCControlInterface }

constructor TSSMMCControlInterface.Create(aConnection: TIMBConnection; const aFederation, aDataSource: string; aProject: TProject;
  const aIdleFederation: string);
begin
  fProject := aProject;
  inherited Create(aConnection, aFederation, aDataSource, aIdleFederation);
end;

procedure TSSMMCControlInterface.HandleModelChange(aModel: TCIModelEntry2; aChange: TMChange);
begin
  if (aModel.State=msIdle) or (Connection.Federation='') or aModel.IsThisSession(Connection.Federation) then
  begin
    case aChange of
      TMChange.mcNew:
        fProject.SendString(jsonModelStatusArray(jsonModelStatusNew(aModel.UID.ToString, aModel.ModelName, aModel.State.ToString, aModel.Progress)));
      TMChange.mcRemove:
        fProject.SendString(jsonModelStatusArray(jsonModelStatusDelete(aModel.UID.ToString)));
    else
        fProject.SendString(jsonModelStatusArray(jsonModelStatusChange(aModel.UID.ToString, aModel.State.ToString, aModel.Progress)));
    end;
  end;

end;

function TSSMMCControlInterface.jsonModelStatusNew(const aModelID, aModelName, aModelStatus: string; aModelProgress: Integer): string;
begin
  Result := '{"new":{"id":"'+aModelID+'","name":"'+aModelName+'","status":"'+aModelStatus+'","progress":'+aModelProgress.toString+'}}';
end;

function TSSMMCControlInterface.jsonModelStatusArray(const aJSONModelStatusArrayContents: string): string;
begin
  Result := '{"type":"modelcontrol","payload":{"status":['+aJSONModelStatusArrayContents+']}}';
end;

function TSSMMCControlInterface.jsonModelStatusChange(const aModelID, aModelStatus: string; aModelProgress: Integer): string;
begin
  Result := '{"change":{"id":"'+aModelID+'","status":"'+aModelStatus+'","progress":'+aModelProgress.toString+'}}';
end;

function TSSMMCControlInterface.jsonModelStatusDelete(const aModelID: string): string;
begin
  Result := '{"delete":{"id":"'+aModelID+'"}}';
end;

end.
