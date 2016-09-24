unit SessionServerSSM;

interface

uses
  Logger,
  IMB3NativeClient, IMB3Core, ByteBuffers,
  imb4,
  WorldDataCode,
  Data.DB,

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

  TSSMLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
  private
    fGTUEvent: TIMBEventEntry;
    fOffsetInRD: TGIS_Point;
  public
    procedure HandleGTUEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  end;

  TSSMProject  = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    fIMB3Connection: TIMBConnection;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aJSONObject: TJSONObject); override;
  public
    property imb3Connection: TIMBConnection read fIMB3Connection;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;



implementation

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
    addResult('lng', p.x.ToString(dotFormat));
    x := _x;
  end;
  if y<>_y then
  begin
    addResult('lat', p.y.toString(dotFormat));
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
  addResult('lng', p.X.ToString(dotFormat));
  addResult('lat', p.Y.ToString(dotFormat));
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
  baseColor := RGBToColor(R, G, B);
  Log.WriteLn('color '+baseColor.ToHexString(8), llNormal, 1);
  addResult('fill', '"'+ColorToJSON(baseColor)+'"');
end;

{ TSSMLayer }

constructor TSSMLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"car"', 'Point', 0);
  fGTUEvent := (scenario.project as TSSMProject).imb3Connection.Subscribe('GTU');
  fGTUEvent.OnNormalEvent := HandleGTUEvent;
  fOffsetInRD.X := scenario.mapView.lon;
  fOffsetInRD.Y := scenario.mapView.lat;
  fOffsetInRD := (scenario.project as TSSMProject).sourceProjection.FromGeocs(fOffsetInRD);
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
        if not objects.ContainsKey(RawByteString(gtuId)) then
        begin
          car := TSSMCar.Create(Self, RawByteString(gtuId));
          car.newTimestamp := timestamp;
          objects.Add(car.ID, car);
          jsonStr := car.new(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
          if jsonStr<>'' then
          begin
            jsonStr := '"id":"'+string(UTF8String(gtuId))+'",'+jsonStr;
            scenario.project.SendString('{"addcar": [{'+jsonStr+'}]}');
          end;
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: new, already known car id '+string(UTF8String(gtuId)), llWarning);
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
          jsonStr := car.delete(aPayload, (scenario.project as TSSMProject).sourceProjection, fOffsetInRD);
          jsonStr := '"id":"'+string(UTF8String(gtuId))+'",'+jsonStr;
          scenario.project.SendString('{"removecar": [{'+jsonStr+'}]}');
        end
        else Log.WriteLn('TSSMLayer.HandleGTUEvent: delete, unknown car id '+string(UTF8String(gtuId)), llError);
      end;
  end;
end;

{ TSSMProject }

constructor TSSMProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer);
begin
  fIMB3Connection := TIMBConnection.Create('vps17642.public.cloudvps.com'{'app-usmodel01.tsn.tno.nl'}{'192.168.1.11'}, 4000, 'PublishingServerSSM', 4, 'OTS_RT');
  mapView  := TMapView.Create(52.08457, 4.88909, 14);
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

procedure TSSMProject.handleClientMessage(aJSONObject: TJSONObject);
var
  jsonPair: TJSONPair;
  jsonValue: TJSONValue;
  EmptyPayload: ByteBuffers.TByteBuffer;
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
