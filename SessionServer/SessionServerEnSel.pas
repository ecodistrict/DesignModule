unit SessionServerEnSel;

interface

uses
  Logger,

  imb4,

  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,

  TilerControl,

  SessionServerLib,
  SessionServerGIS,

  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector,

  System.JSON,
  System.Generics.Collections,
  System.SysUtils;

{
  emmission layer
  sensor layer -> point value, concentration, time,
}

const sources_latitude = 113;
const sources_longitude = 121;
const sources_height = 129;
const sources_timeutc = 137;
const sources_emission_strength_forecast = 145;
const sources_emission_strength_analysis = 153;
const sources_probability_forecast = 161;
const sources_probability_analysis = 169;
const sources_sourceid = 176;

const sensordata_sensorid = 114;
const sensordata_latitude = 121;
const sensordata_longitude = 129;
const sensordata_timeutc = 137;
const sensordata_concentration = 145;

type
  TEnselLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  protected
    fLayerType: Integer;
    fPalette: TWDPalette;

    fDataEvent: TEventEntry;
    fPrivateDataEvent: TEventEntry;
  public
    //procedure ReadObjects(aSender: TObject);
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
    procedure signalInquire(const aQuery: string='');
  end;

  TEnselEmissionLayer = class(TEnselLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  private
    // table: sources
    // id uuid NOT NULL,
    // latitude double precision (
    // longitude double precision,
    // emission_strength_analysis
    procedure handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  end;

  TEnselSensorsLayer = class(TEnselLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    // table: sensordata
    // id uuid NOT NULL,
    // latitude double precision (
    // longitude double precision,
    // emission_strength_analysis
    fGUIDToID: TDictionary<TGUID, Integer>;
    procedure handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  end;

  TEnselScenario = class(TScenario)
  private
    fSensorsLayer: TEnselSensorsLayer;
  public
//    function AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
//      aDefaultLoad: Boolean; aBasicLayer: Boolean;
//      const aSchema, aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;
//    function AddLayerFromQuery(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
//      aDefaultLoad: Boolean; aBasicLayer: Boolean;
//      const aSchema, aQuery: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;

    procedure ReadBasicData(); override;
  public
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; const aQuery: string): string; overload; override;
//
//    function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; override;
  end;

const meteodata_stationid = 114;
const meteodata_timeutc = 121;
const meteodata_winddirection = 129;
const meteodata_windspeed = 137;
const meteodata_gust = 145;
const meteodata_temperature = 153;
const meteodata_minimumtemperature = 161;
const meteodata_dewpoint = 169;
const meteodata_sunshine = 177;
const meteodata_globalradiation = 185;
const meteodata_precipitationduration = 193;
const meteodata_precipitation = 201;
const meteodata_pressureatsealevel = 209;
const meteodata_moninobukhovlength = 217;
const meteodata_mixinglayerheight = 225;
const meteodata_visibility = 232;
const meteodata_cloudiness = 240;
const meteodata_relativehumidity = 248;
const meteodata_weathercode1 = 256;
const meteodata_weathercode2 = 264;
const meteodata_fog = 272;
const meteodata_rainfall = 280;
const meteodata_snow = 288;
const meteodata_thunder = 296;
const meteodata_iceformation = 304;


type
  TEnselWindData = class
  constructor Create(aProject: TProject);
  destructor Destroy; override;
  private
    fDataEvent: TEventEntry;
    fProject: TProject;
    timeutc: double;
    winddirection: double;
    windspeed: double;
    procedure handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  public
    property project: TProject read fProject;
  end;

const complaints_name = 114;
const complaints_timeutc = 121;
const complaints_latitude = 129;
const complaints_longitude = 137;
const complaints_email = 146;
const complaints_message = 154;
const complaints_category = 162;

type
  TEnselComplaints = class
  constructor Create(aProject: TProject);
  destructor Destroy; override;
  private
    fProject: TProject;
    fComplaintsWebSocketEvent: TEventEntry;

    fDataEvent: TEventEntry;
    fPrivateDataEvent: TEventEntry;
    procedure handleComplaintsDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure signalInquire(const aQuery: string='');
  public
    property project: TProject read fProject;
  end;

  TEnselProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
  destructor Destroy; override;
  private
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
    fWindData: TEnselWindData;
    //fComplaints: TEnselComplaints;
    //fSensorsLayer: TEnselSensorsLayer;
  protected
    procedure ReadObjects(aSender: TObject);
    function getMeasuresJSON: string; override;
    function handleTilerStatus(aTiler: TTiler): string;
    procedure handleNewClient(aClient: TClient); override;
  public
    function ReadScenario(const aID: string): TScenario; override;
    procedure ReadBasicData(); override;
  public
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;

  TEnselModule = class
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string;
    aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    fSessionModel: TSessionModel;
    fConnection: TConnection;
    //fDBConnection: TCustomConnection;
    //fConnectString: string;
    fTilerFQDN: string;
    fTilerStatusURL: string;
    //fDashboardEvent: TEventEntry;
    //fDataEvent: TEventEntry;
    //fModuleEvent: TEventEntry;
    //fCaseVariantManagementEvent: TEventEntry;
    fProjects: TDictionary<string, TProject>;
    fMaxNearestObjectDistanceInMeters: Integer;
  end;





implementation


{ TEnselModule }

constructor TEnselModule.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string;
  aMaxNearestObjectDistanceInMeters: Integer);
var
  project: TProject;
begin
  // publish to dashboard
  // subscribe to modules
  // publish and subscribe to data
  inherited Create;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fTilerFQDN := aTilerFQDN;
  fTilerStatusURL := aTilerStatusURL;
  fProjects := TDictionary<string, TProject>.Create;//([doOwnsValues]);
  fMaxNearestObjectDistanceInMeters := aMaxNearestObjectDistanceInMeters;
  //InitPG;
  project := TEnselProject.Create(aSessionModel, aConnection, 'ensel', 'EnSel', aTilerFQDN, aTilerStatusURL,
    1, False, False, False, False, False, aMaxNearestObjectDistanceInMeters, TMapView.Create(52.08895, 5.1707, 14));
  fProjects.Add(project.ProjectID, project);
end;

destructor TEnselModule.Destroy;
begin
  // todo:
  FreeAndNil(fProjects);
  inherited;
end;

{ TEnselProject }

constructor TEnselProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
  aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
begin
  mapView := aMapView;
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN,
    aTilerStatusURL, nil, aTimeSlider, aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers,
    aMaxNearestObjectDistanceInMeters);
  fTiler.onTilerStatus := handleTilerStatus;
  // add ensel scenario
  //scenario := TEnselScenario.Create(Self, '1', '1', '1', false, Self.mapView);
  //scenarios.Add(scenario.id, scenario);
  fWindData := TEnselWindData.Create(Self);
  //fComplaints := TEnselComplaints.Create(Self);
end;

destructor TEnselProject.Destroy;
begin
  fWindData.Free;
  //fComplaints.Free;
  inherited;
end;

function TEnselProject.getMeasuresJSON: string;
begin
  Result := '{}';
end;

function TEnselProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

procedure TEnselProject.handleNewClient(aClient: TClient);
var
  scenario: TScenario;
begin
  // send new sensors
  for scenario in fScenarios.Values do
  begin
    if scenario is TEnselScenario then
    begin
      if Assigned((scenario as TEnselScenario).fSensorsLayer) then
      begin
        //(scenario as TEnselScenario).fSensorsLayer.fGUIDToID

      end;
    end;
  end;
end;

procedure TEnselProject.ReadBasicData;
begin
  fCurrentScenario := ReadScenario('1'); //TEnselScenario.Create(Self, '1', '1', '1', false, Self.mapView);
  scenarios.Add(fCurrentScenario.id, fCurrentScenario);
end;

procedure TEnselProject.ReadObjects(aSender: TObject);
begin
  // todo:
end;

function TEnselProject.ReadScenario(const aID: string): TScenario;
begin
  Result := TEnselScenario.Create(Self, aID, 'Uithof', 'Incident on the Uithof', false, Self.mapView);
end;

{ TEnselScenario }

procedure TEnselScenario.ReadBasicData;
var
  layer: TLayer;
  palette: TDiscretePalette;
  entries: TPaletteDiscreteEntryArray;
begin
  // subscribe to data feeds
  SetLength(entries, 5);
  entries[0] := TDiscretePaletteEntry.Create(TGeoColors.Create($FF00FF00), 0, 5, '0 - 5');
  entries[1] := TDiscretePaletteEntry.Create(TGeoColors.Create($FF66FF00), 5, 10, '5 - 10');
  entries[2] := TDiscretePaletteEntry.Create(TGeoColors.Create($FFFFFF00), 10, 50, '10 - 50');
  entries[3] := TDiscretePaletteEntry.Create(TGeoColors.Create($FFFF6600), 50, 100, '50 - 100');
  entries[4] := TDiscretePaletteEntry.Create(TGeoColors.Create($FFFF0000), 100, 1000000, '> 100');

  palette := TDiscretePalette.Create('Emissions', entries, TGeoColors.Create());
  layer := TEnselEmissionLayer.Create(self, 'air quality', 'emissions', 'Emissions', 'Emissions', True, '"sources"', 'Point', stReceptor,
    palette, BuildDiscreteLegendJSON(palette, TLegendFormat.lfVertical));
  fLayers.Add(layer.ID, layer);

  SetLength(entries, 5);
  entries[0] := TDiscretePaletteEntry.Create(TGeoColors.Create($FF00FF00), 0, 1, '0 - 1');
  entries[1] := TDiscretePaletteEntry.Create(TGeoColors.Create($FF66FF00), 1, 3, '1 - 3');
  entries[2] := TDiscretePaletteEntry.Create(TGeoColors.Create($FFFFFF00), 3, 5, '3 - 5');
  entries[3] := TDiscretePaletteEntry.Create(TGeoColors.Create($FFFF6600), 5, 10, '5 - 10');
  entries[4] := TDiscretePaletteEntry.Create(TGeoColors.Create($FFFF0000), 10, 1000000, '> 10');


  palette := TDiscretePalette.Create('Sensor concentration', entries, TGeoColors.Create());
  fSensorsLayer := TEnselSensorsLayer.Create(self, 'air quality', 'sensor concentration', 'Sensors', 'Sensors', True, '"sensor"', 'Point', stReceptor,
    palette, BuildDiscreteLegendJSON(palette, TLegendFormat.lfVertical));
  fLayers.Add(fSensorsLayer.ID, fSensorsLayer);

end;

{ TEnselLayer }

constructor TEnselLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  fLayerType := aLayerType;
  fPalette := aPalette;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, Double.NaN, aBasicLayer);
  fLegendJSON := aLegendJSON; // property of TLayer
end;

destructor TEnselLayer.Destroy;
begin
  FreeAndNil(fPalette);
  inherited;
end;

procedure TEnselLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name);
end;

procedure TEnselLayer.RegisterSlice;
begin
  if Assigned(fPalette)
  then tilerLayer.signalAddSlice(fPalette.Clone)
  else tilerLayer.signalAddSlice(nil);
  signalInquire(); // layer is defined on tiler so now we are ready to send inquire signal
end;

procedure TEnselLayer.signalInquire(const aQuery: string);
begin
  fDataEvent.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aQuery));
end;

function TEnselLayer.SliceType: Integer;
begin
  Result := fLayerType; // in ensel slice type=layer type
end;

{ TEnselEmissionLayer }

constructor TEnselEmissionLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  inherited;
  fPrivateDataEvent := aScenario.project.Connection.subscribe(aScenario.project.Connection.privateEventName+'.sources', false);
  fPrivateDataEvent.OnEvent.Add(handleDataEvent);
  fDataEvent := aScenario.project.Connection.subscribe('ensel.sources', false);
  fDataEvent.OnEvent.Add(handleDataEvent);
  RegisterLayer;
end;

procedure TEnselEmissionLayer.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TGUID;
  lat: Double;
  lon: double;
  value: Double;
  sourceID: Integer;
  prevID: TGUID;

  procedure addLayerObject(aID: TGUID; aSourceID: Integer; aLat, aLon: Double; aValue: Double);
  var
    wdid: TWDID;
    gp: TWDGeometryPoint;
    gplo: TGeometryPointLayerObject;
    //sourceId: TWDID;
  begin
    if not (aLat.IsNan or aLon.IsNan) then
    begin
      //SetLength(wdid, SizeOf(aID));
      //Move(aID, PAnsiChar(wdid)^, SizeOf(aID));
      //sourceId := Round(aLat*100000+aLon).ToString; // todo: bagger, overlap, range check
      wdid := TWDID(aSourceID.ToString());
      gp := TWDGeometryPoint.Create(aLon, aLat, 0);
      projectGeometryPoint(gp, (scenario.project as TEnSelProject).sourceProjection);
      gplo := TGeometryPointLayerObject.Create(self, wdid, gp, aValue);
      objects.AddOrSetValue(gplo.ID, gplo);
      //Log.WriteLn('emission: '+aSourceID.ToString+' @ '+gp.x.ToString(dotFormat)+','+gp.y.ToString(dotFormat)+': '+aValue.ToString(dotFormat));
      tilerLayer.signalData(gplo.encode, 0); // todo: timestamp
    end
    else Log.WriteLn('TEnselEmissionLayer.handleDataEvent addLayerObject: ignoring NaN coordinates', llWarning);
  end;

  procedure removeLayerObject(aID: TGUID);
  var
    wdid: TWDID;
  begin
    SetLength(wdid, SizeOf(aID));
    Move(aID, PAnsiChar(wdid)^, SizeOf(aID));
    objects.Remove(wdid);
    tilerLayer.signalData(TByteBuffer.bb_tag_rawbytestring(icehNoObjectID, wdid), 0); // todo: timestamp
  end;

begin
  prevID := TGUID.Empty;
  lat := Double.NaN;
  lon := Double.NaN;
  value := Double.NaN;
  sourceID := -1;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_guid(aCursor);
          if prevID<>TGUID.Empty
          then addLayerObject(prevID, sourceID, lat, lon, value);
          prevID := id;
        end;
      sources_latitude:
        lat := aPayload.bb_read_double(aCursor);
      sources_longitude:
        lon := aPayload.bb_read_double(aCursor);
      sources_emission_strength_analysis:
        value := aPayload.bb_read_double(aCursor);
      sources_sourceid:
        sourceID := aPayload.bb_read_int32(aCursor);
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          removeLayerObject(aPayload.bb_read_guid(aCursor));
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
  if previd<>TGUID.Empty then
  begin
    addLayerObject(prevID, sourceID, lat, lon, value);
  end;
end;

{ TEnselWindData }

constructor TEnselWindData.Create(aProject: TProject);
begin
  inherited Create;
  fProject := aProject;
  fDataEvent := project.Connection.subscribe('meteodata');
  fDataEvent.OnEvent.Add(handleMeteoDataEvent);
end;

destructor TEnselWindData.Destroy;
begin
  //
  inherited;
end;

procedure TEnselWindData.handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
begin
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      meteodata_timeutc:
        timeutc := aPayload.bb_read_double(aCursor);
      meteodata_winddirection:
        winddirection := aPayload.bb_read_double(aCursor);
      meteodata_windspeed:
        windspeed := aPayload.bb_read_double(aCursor);
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
  project.SendString('{"winddata":{"speed":'+windspeed.ToString(dotFormat)+',"direction":'+winddirection.ToString(dotFormat)+',"time":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss', timeutc)+'"}}');
end;

{ TEnselSensorsLayer }

constructor TEnselSensorsLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  fGUIDToID := TDictionary<TGUID, Integer>.Create;
  inherited;
  fPrivateDataEvent := aScenario.project.Connection.subscribe(aScenario.project.Connection.privateEventName+'.sensordata', false);
  fPrivateDataEvent.OnEvent.Add(handleDataEvent);
  fDataEvent := aScenario.project.Connection.subscribe('ensel.sensordata', false);
  fDataEvent.OnEvent.Add(handleDataEvent);
  RegisterLayer;
end;

destructor TEnselSensorsLayer.Destroy;
begin
  fGUIDToID.Free;
  inherited;
end;

procedure TEnselSensorsLayer.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TGUID;
  lat: Double;
  lon: double;
  value: Double;
  sourceID: Integer;
  prevID: TGUID;
  sensorID: TGUID;
  timeutc: TDateTime;

  procedure addLayerObject(aID: TGUID; aSensorID: TGUID; aLat, aLon: Double; aValue: Double; aTimeUTC: TDateTime);
  var
    wdid: TWDID;
    gp: TWDGeometryPoint;
    gplo: TGeometryPointLayerObject;
    localID: Integer;
    i: Integer;
    //sourceId: TWDID;
  begin
    if not (aLat.IsNan or aLon.IsNan) then
    begin
      //SetLength(wdid, SizeOf(aID));
      //Move(aID, PAnsiChar(wdid)^, SizeOf(aID));
      //sourceId := Round(aLat*100000+aLon).ToString; // todo: bagger, overlap, range check
      //wdid := TWDID(aSourceID.ToString());
      //move(aSensorID, localID, sizeof(localID)); // todo: work-a-round, Walter maps a int32 to the first bytes of the guid
      //wdid := TWDID(localID.toString);

      if not fGUIDToID.TryGetValue(aSensorID, localID) then
      begin
        localID := 1;
        for i in fGUIDToID.Values do
        begin
          if localID<=i
          then localID := i+1;
        end;
        fGUIDToID.Add(aSensorID, localID);
        //scenario.project.SendString('{"sensor":{"sensorid":'+localID.ToString+',"name":"'+localID.ToString+'","address":"","latitude":'+gp.y.ToString(dotFormat)+',"longitude":'+gp.x.ToString(dotFormat)+',"measuredsubstance":"benzeen","mobile":false}}');

      end;

      gp := TWDGeometryPoint.Create(aLon, aLat, 0);
      projectGeometryPoint(gp, (scenario.project as TEnSelProject).sourceProjection);
      gplo := TGeometryPointLayerObject.Create(self, wdid, gp, aValue);
      objects.AddOrSetValue(gplo.ID, gplo);
      //Log.WriteLn('sensor: '+wdid+' @ '+gp.x.ToString(dotFormat)+','+gp.y.ToString(dotFormat)+': '+aValue.ToString(dotFormat));
      tilerLayer.signalData(gplo.encode, 0); // todo: timestamp

      scenario.project.SendString('{"sensor":{"sensorid":'+localID.ToString+',"name":"'+localID.ToString+'","address":"","latitude":'+gp.y.ToString(dotFormat)+',"longitude":'+gp.x.ToString(dotFormat)+',"measuredsubstance":"benzeen","mobile":false}}');
      scenario.project.SendString('{"sensordata":{"sensorid":'+localID.ToString+',"concentration":'+aValue.ToString(dotFormat)+',"latitude":'+gp.y.ToString(dotFormat)+',"longitude":'+gp.x.ToString(dotFormat)+',"time":"'+formatDateTime('yyyy-mm-dd hh:nn:ss', aTimeUTC)+'"}}');
    end
    else Log.WriteLn('TEnselSensorsLayer.handleDataEvent addLayerObject: ignoring NaN coordinates', llWarning);
  end;

  procedure removeLayerObject(aID: TGUID);
  var
    wdid: TWDID;
  begin
    SetLength(wdid, SizeOf(aID));
    Move(aID, PAnsiChar(wdid)^, SizeOf(aID));
    objects.Remove(wdid);
    tilerLayer.signalData(TByteBuffer.bb_tag_rawbytestring(icehNoObjectID, wdid), 0); // todo: timestamp
  end;

begin
  prevID := TGUID.Empty;
  lat := Double.NaN;
  lon := Double.NaN;
  value := Double.NaN;
  sourceID := -1;
  timeutc := 0;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_guid(aCursor);
          if prevID<>TGUID.Empty
          then addLayerObject(prevID, sensorID, lat, lon, value, timeutc);
          prevID := id;
        end;
      sensordata_sensorid:
        sensorID := aPayload.bb_read_guid(aCursor);
      sensordata_latitude:
        lat := aPayload.bb_read_double(aCursor);
      sensordata_longitude:
        lon := aPayload.bb_read_double(aCursor);
      sensordata_timeutc:
        timeutc := aPayload.bb_read_double(aCursor);
      sensordata_concentration:
        value := aPayload.bb_read_double(aCursor);
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          removeLayerObject(aPayload.bb_read_guid(aCursor));
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
  if previd<>TGUID.Empty then
  begin
    addLayerObject(prevID, sensorID, lat, lon,  value, timeutc);
  end;
end;

{ TEnselComplaints }

constructor TEnselComplaints.Create(aProject: TProject);
begin
  inherited Create;
  fComplaintsWebSocketEvent := aProject.Connection.subscribe(WS2IMBEventName+'.complaints', False);
  fComplaintsWebSocketEvent.OnIntString.Add(
    procedure (aEventEntry: TEventEntry; aInt: Integer; const aString: string)
    var
      complaintsWebClientEvent: TEventEntry;
    begin
      if aInt=actionNew then
      begin
        Log.WriteLn('complaints client: link to '+aString);
        complaintsWebClientEvent := aProject.Connection.subscribe(aString, False);
        complaintsWebClientEvent.OnString.Add(
          procedure(aEventEntry: TEventEntry; const aString: string)
          var
            jsonObject: TJSONObject;
            _id, _name, _email, _category, _message,
            _latitude, _longitude, _time: string;
          begin
            // decode complaint
            Log.WriteLn('Received complaint: '+aString);
            jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
            try
              _id := jsonObject.getValue<string>('id', '');
              _name := jsonObject.getValue<string>('name', '');
              _email := jsonObject.getValue<string>('email', '');
              _category := jsonObject.getValue<string>('category', '');
              _message := jsonObject.getValue<string>('message', '');
              _latitude := jsonObject.getValue<string>('latitude', '');
              _longitude := jsonObject.getValue<string>('longitude', '');
              _time := jsonObject.getValue<string>('time', '');
              aProject.SendString('{"complaint":{"complaintid":"'+_id+'","type":"'+_category+'","latitude":'+_latitude+',"longitude":'+_longitude+',"time":"'+_time+'","text":"'+_message+'"}}');
            finally
              jsonObject.Free;
            end;
          end);
      end;
    end);
  fPrivateDataEvent := project.Connection.subscribe(project.Connection.privateEventName+'.complaints', false);
  fPrivateDataEvent.OnEvent.Add(handleComplaintsDataEvent);
  fDataEvent := project.Connection.subscribe('ensel.complaints', false);
  fDataEvent.OnEvent.Add(handleComplaintsDataEvent);
  signalInquire();
end;

destructor TEnselComplaints.Destroy;
begin

  inherited;
end;

procedure TEnselComplaints.handleComplaintsDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TGUID;
  prevID: TGUID;
  category: string;
  latitude: double;
  longitude: double;
  time: TDateTime;
  _message: string;

  procedure sendComplaint();
  begin
    project.SendString('{"complaint":{"complaintid":"'+id.ToString()+'","type":"'+category+'","latitude":'+latitude.ToString(dotFormat)+',"longitude":'+longitude.ToString(dotFormat)+',"time":"'+FormatDateTime('yyyy-mm-dd hh:nn:ss', time)+'","text":"'+_message+'"}}');
  end;

  procedure sendRemoveComplaint(const aID: TGUID);
  begin
    project.SendString('{"removecomplaint":{"complaintid":"'+aID.ToString+'"}}');
  end;

begin
  prevID := TGUID.Empty;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_guid(aCursor);
          if prevID<>TGUID.Empty
          then sendComplaint();
          prevID := id;
        end;
      // todo: complaints_name:;
      complaints_timeutc:
        time := aPayload.bb_read_double(aCursor);
      complaints_latitude:
        latitude := aPayload.bb_read_double(aCursor);
      complaints_longitude:
        longitude := aPayload.bb_read_double(aCursor);
      // todo: complaints_email:;
      complaints_message:
        _message := aPayload.bb_read_string(aCursor);
      complaints_category:
        category := aPayload.bb_read_string(aCursor);
      (icehNoObjectID shl 3) or wtLengthDelimited:
        sendRemoveComplaint(aPayload.bb_read_guid(aCursor));
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
  if previd<>TGUID.Empty then
  begin
    sendComplaint();
  end;
end;

procedure TEnselComplaints.signalInquire(const aQuery: string);
begin
  fDataEvent.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aQuery));
end;



end.
