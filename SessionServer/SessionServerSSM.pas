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

  System.JSON,
  SessionServerLib,
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
  public
    function getGeoJSON2D(const aType: string): string; override;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  public
    // imb 3 decoding
    function new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
    function change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
    function delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
  end;

  TSSMSensor = class(TLayerObject)
  public
    // gtuID = TLayerObject.ID
    // timestamps
    newTimestamp: Double;
    changedTimestamp: Double;
    deletedTimestamp: Double;
    // base, new/change/delete
    networkId: AnsiString;
    linkId: AnsiString;
    laneId: AnsiString;
    // new
    x: Double;
    y: Double;
    z: Double;
    longitudinalPosition: Double;
    length: Double;
    // change
    gtuId: AnsiString;
    speed: Double;
    triggerPosition: AnsiString;
    // delete
  public
    function getGeoJSON2D(const aType: string): string; override;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  public
    // imb 3 decoding
    function new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
    function change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
    function delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
  end;

  TSSMStatistic = class(TLayerObject)
  public
    // gtuID = TLayerObject.ID
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
    metadataId: AnsiString;
    metadataType: AnsiString;
    metadataValue: AnsiString;
    numberSpaceTimeRegions: integer;
    startTime: double;
    endTime: double;
    linkId: AnsiString;
    laneId: AnsiString;
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
    // delete
  public
    function getGeoJSON2D(const aType: string): string; override;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  public
    // imb 3 decoding
    function new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
    function change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
    function delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;
  end;

  TSSMLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
  private
    fGTUEvent: TIMBEventEntry;
    fGTUSensorEvent: TIMBEventEntry;
    fGTUStatisticEvent: TIMBEventEntry;
    fOffsetInRD: TGIS_Point;
    // todo:  move to project or scenario
    fSIMStartEvent: TIMBEventEntry;
    fSIMStopEvent: TIMBEventEntry;
    fSIMSpeedEvent: TIMBEventEntry;
  public
    procedure HandleGTUEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleGTUSensorEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
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
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aJSONObject: TJSONObject; aScenario: TScenario); override;
  public
    property imb3Connection: TIMBConnection read fIMB3Connection;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;



implementation

function DoubleToJSON(d: Double): string;
begin
  if d.IsNan
  then Result := 'null'
  else Result := d.toString(dotFormat);
end;

{ TSSMCar }

function TSSMCar.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  _x: Double;
  _y: Double;
  _turnIndicatorStatus: string;
  _brakingLights: boolean;
  p: TGIS_Point;
begin
  Result := '';
  // start after timestamp and gtuId
  aPayload.Read(_x);
  aPayload.Read(_y);
  // project point always and use offset as an extra option to put a 0 based network in a location
  p.X := _x+aOffsetInRD.X;
  p.Y := _y+aOffsetInRD.Y;
  p := aSourceProjection.ToGeocs(p);
  // check for changes
  if x<>_x then
  begin
    addResult('lng', DoubleToJSON(p.x));
    x := _x;
  end;
  if y<>_y then
  begin
    addResult('lat', DoubleToJSON(p.y));
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
    addResult('tis', '"'+_turnIndicatorStatus+'"');
    turnIndicatorStatus := _turnIndicatorStatus;
  end;
  aPayload.Read(_brakingLights);
  if brakingLights<>_brakingLights then
  begin
    if _brakingLights
    then addResult('bl', 'true')
    else addResult('bl', 'false');
    brakingLights := _brakingLights;
  end;
  aPayload.Read(odometer);
end;

function TSSMCar.delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point):  string;

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

function TSSMCar.new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  R, G, B: byte;
  p: TGIS_Point;
begin
  Result := '';
  // start after timestamp and gtuId
  aPayload.Read(x);
  aPayload.Read(y);
  // project point always and use offset as an extra option to put a 0 based network in a location
  p.X := x+aOffsetInRD.X;
  p.Y := y+aOffsetInRD.Y;
  p := aSourceProjection.ToGeocs(p);
  addResult('lng', DoubleToJSON(p.X));
  addResult('lat', DoubleToJSON(p.Y));
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
  addResult('fill', '"'+ColorToJSON(baseColor)+'"');
end;

{ TSSMSensor }

function TSSMSensor.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  gtuId: string;
  speed: double;
  triggerPosition: AnsiString;
begin
  Result := '';
  // start after timestamp and gtuId
  aPayload.Read(gtuId);
  aPayload.Read(speed);
  aPayload.Read(triggerPosition);
  addResult('gtuId', '"'+gtuId+'"');
  addResult('speed', DoubleToJSON(speed));
end;

function TSSMSensor.delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point):  string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

begin
  Result := ''; // for now no repsonse so no projection needed
end;

function TSSMSensor.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
begin
  Result := Infinite; // todo:
end;

function TSSMSensor.getGeoJSON2D(const aType: string): string;
begin
  Result := ''; // todo:
end;

function TSSMSensor.intersects(aGeometry: TWDGeometry): Boolean;
begin
  Result := False; // todo:
end;

function TSSMSensor.new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  p: TGIS_Point;
begin
  Result := '';
  // start after timestamp and various Ids
  aPayload.Read(longitudinalPosition);
  aPayload.Read(length);
  aPayload.Read(x);
  aPayload.Read(y);
  // project point always and use offset as an extra option to put a 0 based network in a location
  p.X := x+aOffsetInRD.X;
  p.Y := y+aOffsetInRD.Y;
  p := aSourceProjection.ToGeocs(p);
  addResult('lng', DoubleToJSON(p.X));
  addResult('lat', DoubleToJSON(p.Y));
  aPayload.Read(z);
  aPayload.Read(triggerPosition);
end;

{ TSSMStatistic }
function TSSMStatistic.new(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  description: AnsiString;
  networkId: AnsiString;
  numberMetadataEntries: integer;
  metadataId: AnsiString;
  metadataType: AnsiString;
  metadataValue: AnsiString;
  numberSpaceTimeRegions: integer;
  startTime: double;
  endTime: double;
  linkId: AnsiString;
  laneId: AnsiString;
  connected: Boolean;
  totalTrajectory: Boolean;
  updateFrequency: double;
  i: Integer;
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
  Result:='{"id":"'+ string(self.ID)+'-KPI01", "name": "Totale voertuigkilometers", "x": {"label": "simulatie seconden"}, "y": [{"label":"meters","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI02", "name": "Totale reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI03", "name": "Gemiddelde snelheid", "x": {"label": "simulatie seconden"}, "y": [{"label":"m/s","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI04", "name": "Gemiddelde reistijd", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI05", "name": "Voertuigverliesuren", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI06", "name": "Gemiddelde ritlengte", "x": {"label": "simulatie seconden"}, "y": [{"label":"seconden","color":"LightBlue"}]}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI07", "name": "Voertuigstops", "x": {"label": "simulatie seconden"}, "y": [{"label":"stops","color":"LightBlue"}]}';
end;

function TSSMStatistic.change(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point): string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

var
  totalGTUDistance: double;
  totalGTUTravelTime: double;
  averageGTUSpeed: double;
  averageGTUTravelTime: double;
  totalGTUTimeDelay: double;
  averageTripLength: double;
  totalNumberStops: double;
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

  Result:='{"id":"'+ string(self.ID)+'-KPI01","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUDistance)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI02","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTravelTime)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI03","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUSpeed)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI04","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageGTUTravelTime)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI05","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalGTUTimeDelay)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI06","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(averageTripLength)+']}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI07","x":'+DoubleToJSON(self.changedTimestamp)+',"y":['+DoubleToJSON(totalNumberStops)+']}';
end;

function TSSMStatistic.delete(var aPayload: ByteBuffers.TByteBuffer; aSourceProjection: TGIS_CSProjectedCoordinateSystem; aOffsetInRD: TGIS_Point):  string;

  procedure addResult(const aName, aValue: string);
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+'"'+aName+'":'+aValue;
  end;

begin
//  Result := ''; // for now no repsonse so no projection needed
  Result:='{"id":"'+ string(self.ID)+'-KPI01"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI02"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI03"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI04"}';
  Result:=Result + ',{"id":"' + string(self.ID)+'-KPI05"}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI06"}';
  Result:=Result + ', {"id":"' + string(self.ID)+'-KPI07"}';
end;

function TSSMStatistic.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
begin
  Result := Infinite; // todo:
end;

function TSSMStatistic.getGeoJSON2D(const aType: string): string;
begin
  Result := ''; // todo:
end;

function TSSMStatistic.intersects(aGeometry: TWDGeometry): Boolean;
begin
  Result := False; // todo:
end;


{ TSSMLayer }

constructor TSSMLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"car"', 'Point', 0);
  // GTU
  fGTUEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('GTU');
  fGTUEvent.OnNormalEvent := HandleGTUEvent;
  // Sensor
  fGTUSensorEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('Sensor_GTU');
  fGTUSensorEvent.OnNormalEvent := HandleGTUSensorEvent;
  // Statistics
  fGTUStatisticEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('StatisticsGTULane');
  fGTUStatisticEvent.OnNormalEvent := HandleGTUStatisticEvent;
  // simulation start
  fSIMStartEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('Sim_Start');
  fSIMStartEvent.OnNormalEvent := HandleSIMStartEvent;
  // simulation stop
  fSIMStopEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('Sim_Stop');
  fSIMStopEvent.OnNormalEvent := HandleSIMStopEvent;
  // simulation speed
  fSIMSpeedEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('Sim_Speed');
  fSIMSpeedEvent.OnNormalEvent := HandleSimSpeedEvent;
  // offset to received coordinates in RD for now..

  //fOffsetInRD.X := scenario.mapView.lon;
  //fOffsetInRD.Y := scenario.mapView.lat;
  //fOffsetInRD := (scenario.project as TSSMProject).sourceProjection.FromGeocs(fOffsetInRD);
  fOffsetInRD.X := 0;
  fOffsetInRD.Y := 0;
end;

procedure TSSMLayer.HandleGTUEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: Integer;
  timestamp: Double;
  gtuId: AnsiString;
  lo: TLayerObject;
  car: TSSMCar;
  jsonStr: string;
  l: Integer;
begin
  l := aPayload.Length;
  aPayload.Read(action);
  aPayload.Read(timestamp);
  aPayload.Read(gtuId);
  //Log.WriteLn('event length '+l.ToString()+': '+action.ToString()+' ('+gtuId+')');
  case action of
    actionNew:
      begin
        if not objects.TryGetValue(RawByteString(gtuId), lo) then
        begin
          car := TSSMCar.Create(Self, RawByteString(gtuId));
          objects.Add(car.ID, car);
        end
        else car := lo as TSSMCar;
        car.newTimestamp := timestamp;
        jsonStr := '"id":"'+string(UTF8String(gtuId))+'",'+car.new(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
        scenario.project.SendString('{"addcar": [{'+jsonStr+'}]}');
      end;
    actionChange:
      begin
        if objects.TryGetValue(RawByteString(gtuId), lo) then
        begin
          car := lo as TSSMCar;
          car.changedTimestamp := timestamp;
          jsonStr := car.change(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
          if jsonStr<>'' then
          begin
            jsonStr := '"id":"'+string(UTF8String(gtuId))+'",'+jsonStr;
            scenario.project.SendString('{"updatecar": [{'+jsonStr+'}]}');
          end;
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: change, unknown car id '+string(UTF8String(gtuId)), llError);
      end;
    actionDelete:
      begin
        if objects.TryGetValue(RawByteString(gtuId), lo) then
        begin
          car := lo as TSSMCar;
          car.deletedTimestamp := timestamp;
          jsonStr := '"id":"'+string(UTF8String(gtuId))+'"'; // +car.delete(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD)}
          scenario.project.SendString('{"removecar": [{'+jsonStr+'}]}');
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: delete, unknown car id '+string(UTF8String(gtuId)), llError);
      end;
  end;
end;

procedure TSSMLayer.HandleGTUSensorEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: Integer;
  timestamp: Double;
  networkId: AnsiString;
  linkId: AnsiString;
  laneId: AnsiString;
  sensorId: AnsiString;
  combiId: AnsiString;
  lo: TLayerObject;
  sensor: TSSMSensor;
  jsonStr: string;
  l: Integer;
begin
  l := aPayload.Length;
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
        jsonStr := '"id":"'+string(UTF8String(combiId))+'",'+sensor.new(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
        scenario.project.SendString('{"newGTUsensor": [{'+jsonStr+'}]}');
      end;
    actionChange:
      begin
        if objects.TryGetValue(RawByteString(combiId), lo) then
        begin
          sensor := lo as TSSMSensor;
          sensor.changedTimestamp := timestamp;
          jsonStr := sensor.change(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
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
          jsonStr := '"id":"'+string(UTF8String(combiId))+'",'+sensor.delete(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
          scenario.project.SendString('{"removeGTUsensor": [{'+jsonStr+'}]}');
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: delete, unknown sensor id '+string(UTF8String(combiId)), llError);
      end;
  end;
end;

procedure TSSMLayer.HandleGTUStatisticEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  action: Integer;
  timestamp: Double;
  statisticId: AnsiString;
  lo: TLayerObject;
  stat: TSSMStatistic;
  jsonStr: string;
  l: Integer;
begin
  l := aPayload.Length;
  aPayload.Read(action);
  aPayload.Read(timestamp);
  aPayload.Read(statisticId);
  case action of
    actionNew:
      begin
        if not objects.TryGetValue(RawByteString(statisticId), lo) then
        begin
          stat := TSSMStatistic.Create(Self, RawByteString(statisticId));
          objects.Add(stat.ID, stat);
        end
        else stat := lo as TSSMStatistic;
        stat.newTimestamp := timestamp;
        jsonStr := stat.new(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
        scenario.project.SendString('{"newGTUstatistics": ['+jsonStr+']}');
      end;
    actionChange:
      begin
        if objects.TryGetValue(RawByteString(statisticId), lo) then
        begin
          stat := lo as TSSMStatistic;
          stat.changedTimestamp := timestamp;
          jsonStr := stat.change(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
          if jsonStr<>''  then
          begin
            scenario.project.SendString('{"updateGTUstatistics": ['+jsonStr+']}');
          end;
        end
        else Log.WriteLn('TSSMLayer.HandleGTUStatisticEvent: change, unknown sensor id '+string(UTF8String(statisticId)), llError);
      end;
    actionDelete:
      begin
        if objects.TryGetValue(RawByteString(statisticId), lo) then
        begin
          stat := lo as TSSMStatistic;
          stat.deletedTimestamp := timestamp;
          jsonStr := stat.delete(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
          scenario.project.SendString('{"removeGTUsensor": ['+jsonStr+']}');
        end
        else Log.WriteLn('TSSMLayer.HandleGTUstatisticEvent: delete, unknown sensor id '+string(UTF8String(statisticId)), llError);
      end;
  end;
end;

procedure TSSMLayer.HandleSimSpeedEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  speed: Double;
begin
  aPayload.Read(speed);
  scenario.project.SendString('{"simulationControl":{"speed":'+DoubleToJSON(speed)+'}}');
end;

procedure TSSMLayer.HandleSimStartEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  scenario.project.SendString('{"simulationControl":{"start":true}}');
end;

procedure TSSMLayer.HandleSimStopEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
begin
  scenario.project.SendString('{"simulationControl":{"stop":true}}');
end;
{ TSSMProject }

constructor TSSMProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMapView: TMapView; aMaxNearestObjectDistanceInMeters: Integer);
begin
  fIMB3Connection := TIMBConnection.Create('vps17642.public.cloudvps.com'{'app-usmodel01.tsn.tno.nl'}{'192.168.1.11'}, 4000, 'PublishingServerSSM', 4, 'OTS_RT');
  //fIMB3Connection := TIMBConnection.Create(GetSetting('RemoteHost', ''){'app-usmodel01.tsn.tno.nl'}{'192.168.1.11'}, 4000, 'PublishingServerSSM', 4, 'OTS_RT');
  //mapView  := TMapView.Create(52.08457, 4.88909, 14);
  mapView := aMapView;
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
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
begin
  if isObject(aJSONObject, 'simulationControl', jsonPair) then
  begin
    EmptyPayload.Clear();
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'stop', jsonValue) then
    begin
      fIMB3Connection.Publish('Sim_Stop').SignalEvent(ekNormalEvent, EmptyPayload);
      SendString('{"simulationControl":{"stop":true}}');
    end;
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'start', jsonValue) then
    begin
      fIMB3Connection.Publish('Sim_Start').SignalEvent(ekNormalEvent, EmptyPayload);
      // signal status to clients
      SendString('{"simulationControl":{"start":true}}');
    end;
    if isObjectValue(jsonPair.JsonValue as TJSONObject, 'speed', jsonValue) then
    begin
      speed := jsonValue.getValue<Double>(); // todo: check if this works
      speedPayload.Clear();
      speedPayload.Write(speed);
      fIMB3Connection.Publish('Sim_Speed').SignalEvent(ekNormalEvent, speedPayload);
      // signal status to clients
      SendString('{"simulationControl":{"speed":'+DoubleToJSON(speed)+'}}');
    end;
  end
  else ;
end;

procedure TSSMProject.ReadBasicData;
var
  gtuLayer: TSSMLayer;
begin
  fCurrentScenario := TScenario.Create(Self, 'woerden', 'Woerden', 'Woerden test', false, mapView);
  scenarios.Add(fCurrentScenario.ID, fCurrentScenario);
  gtuLayer := TSSMLayer.Create(fCurrentScenario, 'mobiliteit', 'GTU', 'GTU', 'GTU', false);
  fCurrentScenario.Layers.Add(gtuLayer.ID, gtuLayer);
end;

end.
