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

const
  sensordata_longitude             = 129;               //tag 16
  sensordata_latitude              = 121;               //tag 15
  sensordata_nox                   = 2561;              //tag 320
  sensordata_pm10                  = 1441;              //tag 180
  sensordata_pm25                  = 1601;              //tag 200
  sensordata_no2                   = 961;               //tag 240
  sensordata_pm1                   = 2081;              //tag 260
  sensordata_nh3                   = 2241;              //tag 280
  sensordata_pnc                   = 2401;              //tag 300
  sensordata_pm10_total            = 1449;              //tag 181
  sensordata_pm25_total            = 1609;              //tag 201
  sensordata_no2_total             = 969;               //tag 241
  sensordata_pm1_total             = 2089;              //tag 261
  sensordata_assim_nox             = 2577;              //tag 322
  sensordata_assim_pm10            = 1457;              //tag 182
  sensordata_assim_pm25            = 1617;              //tag 202
  sensordata_assim_no2             = 977;               //tag 242
  sensordata_assim_pm1             = 2097;              //tag 262
  sensordata_assim_nh3             = 2257;              //tag 282
  sensordata_assim_pnc             = 2417;              //tag 302
  sensordata_assim_pm10_total      = 1465;              //tag 183
  sensordata_assim_pm25_total      = 1625;              //tag 203
  sensordata_assim_no2_total       = 985;               //tag 243
  sensordata_assim_pm1_total       = 2105;              //tag 263

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
  TEnselTileLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aKey: UInt32; const aEventName: string; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fEventName: string;
  protected
    fLayerType: Integer;
    fKey: UInt32;
    fPalette: TWDPalette;

    fDataEvent: TEventEntry;
    fPrivateDataEvent: TEventEntry;
    fHandleDataHandlerRef: TOnEvent;

    procedure handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  public
    //procedure ReadObjects(aSender: TObject);
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
    procedure signalInquire(const aQuery: string='');
  end;

  {
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
  }
  TEnselTrackLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean);
  private
    fEvent: TEventEntry;
//    fPalette: TWDPalette;
  public
    procedure HandleEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  public
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

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

  TEnselScenario = class(TScenario)
  private
    //fSensorsLayer: TEnselSensorsLayer;
//    fTrackNOxLayer: TEnselTrackLayer;
//    fTrackPM10Layer: TEnselTrackLayer;
//    fChartGemodelleerdeBlootstelling: TChart;
  public
//    function AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
//      aDefaultLoad: Boolean; aBasicLayer: Boolean;
//      const aSchema, aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;
//    function AddLayerFromQuery(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
//      aDefaultLoad: Boolean; aBasicLayer: Boolean;
//      const aSchema, aQuery: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;

    procedure ReadBasicData(); override;
    procedure addConcentrationLayer(
      const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
      aKey: UInt32; const aEventName: string;
      aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  public
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; const aQuery: string): string; overload; override;
//
//    function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; override;
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
    fTilerFQDN: string;
    fTilerStatusURL: string;
    fProjects: TDictionary<string, TProject>;
    fMaxNearestObjectDistanceInMeters: Integer;
  end;


implementation


{ TEnselTileLayer }

constructor TEnselTileLayer.Create(
  aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer;
  aKey: UInt32; const aEventName: string;
  aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  fLayerType := aLayerType;
  fPalette := aPalette;
  fKey := aKey;
  fEventName := aEventName;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, ltTile, True, Double.NaN, aBasicLayer);
  fLegendJSON := aLegendJSON; // property of TLayer
  fHandleDataHandlerRef :=  handleDataEvent;
  fDataEvent := scenario.project.Connection.subscribe(fEventName);
  if not fDataEvent.OnEvent.Contains(fHandleDataHandlerRef)
  then fDataEvent.OnEvent.Add(fHandleDataHandlerRef);

  fPrivateDataEvent := scenario.project.Connection.subscribe(scenario.project.Connection.privateEventName+'.'+fEventName, False);
  if not fPrivateDataEvent.OnEvent.Contains(fHandleDataHandlerRef)
  then fPrivateDataEvent.OnEvent.Add(fHandleDataHandlerRef);
end;

destructor TEnselTileLayer.Destroy;
begin
  FreeAndNil(fPalette);
  inherited;
end;

procedure TEnselTileLayer.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TWDID;
  timestamp: TDateTime;
  lat: Double;
  lon: double;
  value: Double;
  prevID: TWDID;

  procedure addLayerObject(const aID: TWDID; aLat, aLon: Double; aValue: Double);
  var
    gp: TWDGeometryPoint;
    gplo: TGeometryPointLayerObject;
  begin
    if not (aLat.IsNan or aLon.IsNan) then
    begin
      //SetLength(wdid, SizeOf(aID));
      //Move(aID, PAnsiChar(wdid)^, SizeOf(aID));
      //sourceId := Round(aLat*100000+aLon).ToString; // todo: bagger, overlap, range check
      gp := TWDGeometryPoint.Create(aLon, aLat, 0);
      if Assigned((scenario.project as TEnSelProject).sourceProjection)
      then projectGeometryPoint(gp, (scenario.project as TEnSelProject).sourceProjection);
      gplo := TGeometryPointLayerObject.Create(self, aID, gp, aValue);
      objects.AddOrSetValue(gplo.ID, gplo);
      //Log.WriteLn('emission: '+aSourceID.ToString+' @ '+gp.x.ToString(dotFormat)+','+gp.y.ToString(dotFormat)+': '+aValue.ToString(dotFormat));
      if Assigned(tilerLayer)
      then tilerLayer.signalData(gplo.encode, 0); // todo: timestamp
    end
    else Log.WriteLn('TEnselEmissionLayer.handleDataEvent addLayerObject: ignoring NaN coordinates', llWarning);
  end;

  procedure removeLayerObject(const aID: TWDID);
  begin
    objects.Remove(aID);
    if Assigned(tilerLayer)
    then tilerLayer.signalData(TByteBuffer.bb_tag_rawbytestring(icehNoObjectID, aID), 0); // todo: timestamp
  end;

begin
  prevID := '';
  lat := Double.NaN;
  lon := Double.NaN;
  value := Double.NaN;
  timestamp := double.NaN;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    if fieldInfo=fKey
    then value := aPayload.bb_read_double(aCursor)
    else
    begin
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aPayload.bb_read_rawbytestring(aCursor);
            if prevID<>''
            then addLayerObject(prevID, lat, lon, value);
            prevID := id;
          end;
        ((icehWorldCommandBase+2) shl 3) or wt64Bit:
          timestamp := aPayload.bb_read_double(aCursor);
        sensordata_longitude:
          lon := aPayload.bb_read_double(aCursor);
        sensordata_latitude:
          lat := aPayload.bb_read_double(aCursor);
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            removeLayerObject(aPayload.bb_read_rawbytestring(aCursor));
          end;
      else
        aPayload.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  end;
  if previd<>'' then
  begin
    addLayerObject(prevID, lat, lon, value);
  end;
end;

procedure TEnselTileLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name);
end;

procedure TEnselTileLayer.RegisterSlice;
begin
  if Assigned(fPalette)
  then tilerLayer.signalAddSlice(fPalette.Clone)
  else tilerLayer.signalAddSlice(nil);
  signalInquire(); // layer is defined on tiler so now we are ready to send inquire signal
end;

procedure TEnselTileLayer.signalInquire(const aQuery: string);
begin
  fDataEvent.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aQuery));
end;

function TEnselTileLayer.SliceType: Integer;
begin
  Result := fLayerType; // in ensel slice type=layer type
end;

(*
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
*)

(*
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
*)

{ TEnselTrackLayer }

constructor TEnselTrackLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad,
  aShowInDomains: Boolean);
//var
//  entries: TPaletteDiscreteEntryArray;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"track"', 'Point', ltObject, aShowInDomains, 0);
  // Track
  fEvent := scenario.project.connection.Subscribe('track');
  fEvent.OnEvent.Add(HandleEvent);
  {
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
  }
end;



function TEnselTrackLayer.HandleClientSubscribe(aClient: TClient): Boolean;
var
  iop: TPair<TWDID, TLayerObject>;
  _json: string;
  sclo: TSVGCircleLayerObject;
begin
  result := inherited;
  // send new track points
  _json := '';
  for iop in objects do
  begin
    if iop.Value is TSVGCircleLayerObject then
    begin
      sclo := iop.Value as TSVGCircleLayerObject;
      (*
      if _json<>''
      then _json  := _json+',';
      _json  := _json+
        '{"newobject":'+
          '{"id":"'+string(UTF8String(car.ID))+'",'+
           '"lng":'+DoubleToJSON(car.latlon.x)+','+
           '"lat":'+DoubleToJSON(car.latlon.y)+','+
           '"fillColor":"'+ColorToJSON(car.baseColor)+'"}}';
      *)
    end;
  end;
  if _json<>'' then
  begin
    aClient.signalString('{"type":"updatelayer","payload":{"id":"'+ElementID+'","data":['+_json+']}}');
  end;
end;

function TEnselTrackLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  // todo: option to clear objects

  Result := inherited;
end;

procedure TEnselTrackLayer.HandleEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
begin
  // todo: build track
end;

procedure TEnselTrackLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500);
end;

procedure TEnselTrackLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice(nil);
end;

function TEnselTrackLayer.SliceType: Integer;
begin
  Result := stLocation;
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

{ TEnselScenario }

procedure TEnselScenario.addConcentrationLayer(
  const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  aKey: UInt32; const aEventName: string;
  aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
var
  layer: TEnselTileLayer;
begin
  // create a layer and link to listener
  layer := TEnselTileLayer.Create(
    Self, aDomain, aID, aName, aDescription, aDefaultLoad, '"receptor"', 'Point', stReceptor,
    aKey, aEventName,
    aPalette, aLegendJSON);
  AddLayer(layer);
  layer.RegisterLayer;
end;

function CreateNO2Palette: TWDPalette;
begin
  Result :=
    TRampPalette.Create('NO2',
      [
        TRampPaletteEntry.Create($Ff002BF7, 0, '0'),
        TRampPaletteEntry.Create($FFC4ECFD, 30, '30'),
        TRampPaletteEntry.Create($FFFFFED0, 30, '30'),
        TRampPaletteEntry.Create($FFFFFC4D, 57, '57'),
        TRampPaletteEntry.Create($FFFE7626, 100, '100'),
        TRampPaletteEntry.Create($FFFF0A17, 150, '150'),
        TRampPaletteEntry.Create($FFDC0610, 200, '200'),
        TRampPaletteEntry.Create($FFA21794, 250, '>')
      ],
      $FF0020C5,
      $00000000,
      $FFA21794);
end;

function CreatePM10Palette: TWDPalette;
begin
  Result :=
    TRampPalette.Create('PM10',
      [
        TRampPaletteEntry.Create($Ff002BF7, 0, '0'),
        TRampPaletteEntry.Create($FFC4ECFD, 30, '30'),
        TRampPaletteEntry.Create($FFFFFED0, 30, '30'),
        TRampPaletteEntry.Create($FFFFFC4D, 57, '57'),
        TRampPaletteEntry.Create($FFFE7626, 100, '100'),
        TRampPaletteEntry.Create($FFFF0A17, 150, '150'),
        TRampPaletteEntry.Create($FFDC0610, 200, '200'),
        TRampPaletteEntry.Create($FFA21794, 250, '>')
      ],
      $FF0020C5,
      $00000000,
      $FFA21794);
end;

function BuildLegendJSON(aPalette: TWDPalette): string;
begin
  if aPalette is TRampPalette
  then Result := BuildRamplLegendJSON(aPalette as TRampPalette)
  else if aPalette is TDiscretePalette
  then Result := BuildDiscreteLegendJSON(aPalette as TDiscretePalette, TLegendFormat.lfHorizontal)
  else Result := '';
end;

procedure TEnselScenario.ReadBasicData;
var
  palette: TWDPalette;
//var
//  layer: TLayer;
//  palette: TDiscretePalette;
//  entries: TPaletteDiscreteEntryArray;
begin
  {
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
  }

  // Personal exposure
  // Group exposure

  palette := CreateNO2Palette;
  addConcentrationLayer(
    'Group exposure', 'no2', 'NO2', 'Total exposure to NO2', false,
    sensordata_no2_total, 'receptordata', palette, BuildLegendJSON(palette));

  palette := CreatePM10Palette;
  addConcentrationLayer(
    'Group exposure', 'pm10', 'PM10', 'Total exposure to PM10', false,
    sensordata_pm10_total, 'receptordata', palette, BuildLegendJSON(palette));

  palette := CreateNO2Palette;
  addConcentrationLayer(
    'Group exposure',  'no2assim', 'NO2 assim', 'Total assimilated exposure to NO2', false,
    sensordata_assim_no2_total, 'receptordata', palette, BuildLegendJSON(palette));



  //addConcentrationLayer(
  //  sensordata_pm10_total);
  {
  fTrackNOxLayer := TEnselTrackLayer.Create(Self, 'Persoonlijke blootstelling', 'trackNOx', 'NOx', 'Track, NOx', True, True);
  fTrackPM10Layer := TEnselTrackLayer.Create(Self, 'Persoonlijke blootstelling', 'trackNOx', 'NOx', 'Track, PM10', False, True);

  // tiler lagen concentraties

  fChartGemodelleerdeBlootstelling := TChart.Create(Self, 'Persoonlijke blootstelling', 'pmb', 'Gemoduleerd', 'Gemoduleerde persoonlijke blootstelling', false, 'line',
          TChartAxis.Create('Time', 'lightBlue', 'UTC', ''),
          [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'kg/m3')]);
  fChartGemodelleerdeBlootstelling := TChart.Create(Self, 'Persoonlijke blootstelling', 'pmb', 'Gemoduleerd', 'Gemoduleerde persoonlijke blootstelling', false, 'line',
          TChartAxis.Create('Time', 'lightBlue', 'UTC', ''),
          [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'kg/m3'),
            TChartAxis.Create('concentration', 'yellow', 'Concentration', 'kg/m3'),
            TChartAxis.Create('concentration', 'red', 'Concentration', 'kg/m3'),
            TChartAxis.Create('concentration', 'green', 'Concentration', 'kg/m3')]);
  }
  // I laag

  // walter prikken:
  // layer keys:
end;

{ TEnselProject }

constructor TEnselProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
  aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
begin
  mapView := aMapView;
  //fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  fSourceProjection := nil;
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN,
    aTilerStatusURL, nil, aTimeSlider, aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled,
    aSimulationControlEnabled, aAddBasicLayers, '',
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
//      if Assigned((scenario as TEnselScenario).fSensorsLayer) then
//      begin
        //(scenario as TEnselScenario).fSensorsLayer.fGUIDToID

//      end;
    end;
  end;
end;

procedure TEnselProject.ReadBasicData;
var
  scenario: TScenario;
begin
  scenario := ReadScenario('1'); //TEnselScenario.Create(Self, '1', '1', '1', false, Self.mapView);
  scenarios.Add(scenario.id, scenario);
  fProjectCurrentScenario := scenario;
end;

procedure TEnselProject.ReadObjects(aSender: TObject);
begin
  // todo:
end;

function TEnselProject.ReadScenario(const aID: string): TScenario;
begin
  Result := TEnselScenario.Create(Self, aID, 'Utrecht', 'Incident in Utrecht', false, Self.mapView); // todo:
end;

{ TEnselModule }

constructor TEnselModule.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string;
  aMaxNearestObjectDistanceInMeters: Integer);
var
  project: TProject;
begin
  inherited Create;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fTilerFQDN := aTilerFQDN;
  fTilerStatusURL := aTilerStatusURL;
  fProjects := TDictionary<string, TProject>.Create;//([doOwnsValues]);
  fMaxNearestObjectDistanceInMeters := aMaxNearestObjectDistanceInMeters;
  //InitPG;
  project := TEnselProject.Create(aSessionModel, aConnection, 'ensel2', 'EnSel2', aTilerFQDN, aTilerStatusURL,
    1, False, False, False, False, False, aMaxNearestObjectDistanceInMeters, TMapView.Create(52.0915, 5.12013, 14));
  fProjects.Add(project.ProjectID, project);
end;

destructor TEnselModule.Destroy;
begin
  // todo:
  FreeAndNil(fProjects);
  inherited;
end;


end.
