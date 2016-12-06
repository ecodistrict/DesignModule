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

  System.Classes,
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

  sensordata_pm10                  = 1441;              //tag 180
  sensordata_pm25                  = 1601;              //tag 200
  sensordata_no2                   = 961;               //tag 240
  sensordata_pm1                   = 2081;              //tag 260
  sensordata_nh3                   = 2241;              //tag 280
  sensordata_pnc                   = 2401;              //tag 300
  sensordata_nox                   = 2561;              //tag 320

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

  meteodata_winddirection = 129;
  meteodata_windspeed = 137;
  meteodata_gust = 145;
  meteodata_temperature = 153;
  meteodata_minimumtemperature = 161;
  meteodata_dewpoint = 169;
  meteodata_sunshine = 177;
  meteodata_globalradiation = 185;
  meteodata_precipitationduration = 193;
  meteodata_precipitation = 201;
  meteodata_pressureatsealevel = 209;
  meteodata_moninobukhovlength = 217;
  meteodata_mixinglayerheight = 225;
  meteodata_visibility = 232;
  meteodata_cloudiness = 240;
  meteodata_relativehumidity = 248;
  meteodata_weathercode1 = 256;
  meteodata_weathercode2 = 264;
  meteodata_fog = 272;
  meteodata_rainfall = 280;
  meteodata_snow = 288;
  meteodata_thunder = 296;
  meteodata_iceformation = 304;

  kpi_road_segment_id = 2880;      // tag 360
  kpi_TransportMode = 3042;        //tag 380
  kpi_ComponentName = 3202;        //tag 400
  kpi_Time_Category = 3362;        //tag 420
  kpi_avg_concentration = 3521;    //tag 440
  kpi_duration = 3681;             //tag 460

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
    //procedure signalInquire(const aQuery: string='');
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
    fEventHandler: TOnEvent;
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

  TEnselMobileSensorLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean;  aPalette: TWDPalette; aLegendJSON: string; aChart: TChartLines);
  destructor Destroy; override;
  private
    fDataEvent: TEventEntry;
    fPrivateDataEvent: TEventEntry;
    fDataEventHandler: TOnEvent;
    fPalette: TWDPalette;
    fChart: TChartLines;
    fLastLat: Double;
    fLastLon: Double;
    procedure AddPoint(aObjectID: TGUID; aTimeStamp, aLat, aLon: Double; aSubstance: UInt32; aValue: Double);
  public
    procedure HandleEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
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
  private
    fDataEvent: TEventEntry;
    fProject: TProject; // ref
    fTimestamp: double; // UTC
    fWindDirection: double;
    fWindSpeed: double;
    procedure handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  public
    property project: TProject read fProject;
  end;

  TSpiderData = class;

  TSpiderDataValue = class
  constructor Create(aValue: Double; const aLink: string; aData: TSpiderData);
  destructor Destroy; override;
  private
    fValue: Double;
    fLink: string;
    fData: TSpiderData;
  public
    property value: Double read fValue write fValue;
    property link: string read fLink write fLink;
    property data: TSpiderData read fData write fData;
  end;

  TSpiderData = class
  constructor Create(const aTitle: string);
  destructor Destroy; override;
  private
    fTitle: string;
    fLabels: TDictionary<string, Integer>;
    fAxes: TDictionary<string, Integer>;
    fValues: TObjectList<TObjectList<TSpiderDataValue>>; // label, axis

    function createValue(const aLabel, aAxis: string): TSpiderDataValue;
    function getValue(const aLabel, aAxis: string): TSpiderDataValue;
    function getValueData(const aLabel, aAxis: string): TSpiderData;
    procedure setValueData(const aLabel, aAxis: string; const Value: TSpiderData);
    function getValueLink(const aLabel, aAxis: string): string;
    procedure setValueLink(const aLabel, aAxis, Value: string);
    function getValueValue(const aLabel, aAxis: string): Double;
    procedure setValueValue(const aLabel, aAxis: string; const Value: Double);
  public
    property title: string read fTitle;
    property labels: TDictionary<string, Integer> read fLabels;
    property axes: TDictionary<string, Integer> read fAxes;
    property values: TObjectList<TObjectList<TSpiderDataValue>> read fValues write fValues;

    property value[const aLabel, aAxis: string]: TSpiderDataValue read getValue;
    property valueValue[const aLabel, aAxis: string]: Double read getValueValue write setValueValue;
    property valueLink[const aLabel, aAxis: string]: string read getValueLink write setValueLink;
    property valueData[const aLabel, aAxis: string]: TSpiderData read getValueData write setValueData;

    function GetOrAddValue(const aLabel, aAxis: string): TSpiderDataValue;

    function getJSON: string;

    function maxValue: Double;
    procedure normalize(aMaxValue, aMaxScale: Double); overload; // single
    procedure normalize(aMaxScale: Double); overload; // recursive
    function RecalculateAverages: Double;
  end;

  TSpiderChart  = class(TChart)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
  destructor Destroy; override;
  private
    fWidth: Integer;
    fHeight: Integer;
    fMaxScale: Double;
    fData: TSpiderData;
  protected
    function getJSON: string; override;
    function getJSONData: string; override;
  public
    property data: TSpiderData read fData;
    procedure normalize;
    procedure RecalculateAverages;
  end;

  TEnselSpiderChart = class(TSpiderChart)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aDataEvent: TEventEntry;
    aSegment: Integer; const aComponent: string);
  destructor Destroy; override;
  private
    fDataEvent: TEventEntry;
    fDataEventHandler: TOnEvent;
    fSegment: Integer;
    fComponent: string;
    procedure handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
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

function CreateNiekPalette(const aTitle: string): TWDPalette;
begin
  Result := TRampPalette.Create(aTitle, [
    TRampPaletteEntry.Create($Ff00AF00, 0, '0'),
    TRampPaletteEntry.Create($FF00C800, 5, '5'),
    TRampPaletteEntry.Create($FF00E100, 10, '10'),
    TRampPaletteEntry.Create($FF32FF32, 15, '15'),
    TRampPaletteEntry.Create($FF7DFF4B, 20, '20'),
    TRampPaletteEntry.Create($FFC8FF4B, 25, '25'),
    TRampPaletteEntry.Create($FFF2FF4B, 30, '30'),
    TRampPaletteEntry.Create($FFFFFA01, 35, '35'),
    TRampPaletteEntry.Create($FFFFE101, 40, '40'),
    TRampPaletteEntry.Create($FFFFC801, 45, '45'),
    TRampPaletteEntry.Create($FFFFAF01, 50, '50'),
    TRampPaletteEntry.Create($FFFF9601, 55, '55'),
    TRampPaletteEntry.Create($FFFF7D01, 60, '60'),
    TRampPaletteEntry.Create($FFFF6401, 65, '65'),
    TRampPaletteEntry.Create($FFFF4B01, 70, '70'),
    TRampPaletteEntry.Create($FFFF0000, 75, '75')],
      $FF00AF00,
      $00000000,
      $FFFF0000);
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
  //signalInquire(); // layer is defined on tiler so now we are ready to send inquire signal
end;

//procedure TEnselTileLayer.signalInquire(const aQuery: string);
//begin
//  fDataEvent.signalEvent(
//    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
//    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aQuery));
//end;

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
  fEventHandler := HandleEvent;
  fEvent.OnEvent.Add(fEventHandler);
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

procedure TEnselWindData.handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
begin
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      ((icehWorldCommandBase+2) shl 3) or wt64Bit:
        fTimestamp := aPayload.bb_read_double(aCursor);
      meteodata_winddirection:
        fWindDirection := aPayload.bb_read_double(aCursor);
      meteodata_windspeed:
        fWindSpeed := aPayload.bb_read_double(aCursor);
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
  project.SendString('{"winddata":{"speed":'+fWindSpeed.ToString(dotFormat)+',"direction":'+fWindDirection.ToString(dotFormat)+',"time":"'+FormatDateTime(isoDateTimeFormat, fTimestamp)+'"}}');
end;

{ TSpiderDataValue }

constructor TSpiderDataValue.Create(aValue: Double; const aLink: string; aData: TSpiderData);
begin
  inherited Create;
  fValue := aValue;
  fLink := aLink;
  fData := aData;
end;

destructor TSpiderDataValue.Destroy;
begin

  inherited;
end;

{ TSpiderData }
(*
procedure TSpiderData.AddOrSetData(const aTitles: TArray<string>; const aAxis: String; aValue: Double);
var
  sd: TSpiderData;
  nextTitles: TArray<string>;
  i: Integer;
begin
  if length(aTitles)>0 then
  begin
    if not fCategories.TryGetValue(aTitles[0], sd) then
    begin
      sd := TSpiderData.Create(aTitles[0], '', Double.NaN);
      fCategories.Add(aTitles[0], sd);
    end;
    setLength(nextTitles, length(aTitles)-1);
    for i := 0 to length(nextTitles)-1
    do nextTitles[i] := aTitles[i+1];
    sd.AddOrSetData(nextTitles, aAxis, aValue);
  end
  else
  begin
    fAxis := aAxis;
    fValue := aValue;
  end;
end;
*)
constructor TSpiderData.Create(const aTitle: string);
begin
  inherited Create;
  fTitle  := aTitle;
  fLabels := TDictionary<string, Integer>.Create;
  fAxes := TDictionary<string, Integer>.Create;
  fValues := TObjectList<TObjectList<TSpiderDataValue>>.Create(True);
end;

function TSpiderData.createValue(const aLabel, aAxis: string): TSpiderDataValue;
var
  l: Integer;
  a: Integer;
  va: TObjectList<TSpiderDataValue>;
begin
  // label, axis
  // check if new label
  if not fLabels.TryGetValue(aLabel, l) then
  begin
    // new label
    l := fLabels.Count;
    fLabels.Add(aLabel, l);
    a := fAxes.Count-1;
    // check all new rows in fValues
    while l>=fValues.Count do
    begin
      va := TObjectList<TSpiderDataValue>.Create;
      // check columns for this row
      while a>=va.Count
      do va.Add(TSpiderDataValue.Create(0, '', nil));
      fValues.Add(va);
    end;
  end;
  // check if new axis
  if not fAxes.TryGetValue(aAxis, a) then
  begin
    // new axis
    a := fAxes.Count;
    fAxes.Add(aAxis, a);
    // check all rows in fValues
    for va in fValues do
    begin
      // check columns for this row
      while a>=va.Count
      do va.Add(TSpiderDataValue.Create(0, '', nil));
    end;
  end;
  Result := fValues[l].Items[a];
end;

destructor TSpiderData.Destroy;
begin
  FreeAndNil(fLabels);
  FreeAndNil(fAxes);
  FreeAndNil(fValues);
  inherited;
end;

function TSpiderData.getJSON: string;

  function jsonAxes: string;
  var
    aip: TPair<string, Integer>;
  begin
    Result := '';
    for aip in fAxes do
    begin
      if Result<>''
      then Result := Result+',';
      Result := Result+'"'+aip.Key+'"';
    end;
  end;

  function jsonLabels: string;
  var
    lip: TPair<string, Integer>;
  begin
    Result := '';
    for lip in fLabels do
    begin
      if Result<>''
      then Result := Result+',';
      Result := Result+'"'+lip.Key+'"';
    end;
  end;

  function jsonValues: string;
  var
    row: TObjectList<TSpiderDataValue>;
    col: TSpiderDataValue;
    jsonRow: string;
  begin
    Result := '';
    for row in fValues do
    begin
      jsonRow := '';
      for col in row do
      begin
        if jsonRow<>''
        then jsonRow := jsonRow+',';
        if col.value.IsNan
        then jsonRow := jsonRow+'"value":'+'0'
        else jsonRow := jsonRow+'"value":'+col.value.ToString(dotFormat);
        if col.link<>''
        then jsonRow := jsonRow+',"link":"'+col.link+'"';
        if Assigned(col.data)
        then jsonRow := jsonRow+',"data":{'+col.data.getJSON+'}';
      end;
      if jsonRow<>'' then
      begin
        if Result<>''
        then Result := Result+',';
        Result := Result+'['+jsonRow+']';
      end;
    end;
  end;

begin
  Result :=
    '"title":"'+fTitle+'",'+
    '"labels":['+jsonLabels+'],'+
    '"axes":['+jsonAxes+'],'+
    '"matrix":['+jsonValues+']';
end;

function TSpiderData.GetOrAddValue(const aLabel, aAxis: string): TSpiderDataValue;
begin
  Result := getValue(aLabel, aAxis);
  if not Assigned(Result)
  then Result := createValue(aLabel, aAxis);
end;

function TSpiderData.getValue(const aLabel, aAxis: string): TSpiderDataValue;
var
  l: Integer;
  a: Integer;
begin
  if fLabels.TryGetValue(aLabel, l) and fAxes.TryGetValue(aAxis, a)
  then Result := fValues[l][a]
  else Result := nil;
end;

function TSpiderData.getValueData(const aLabel, aAxis: string): TSpiderData;
var
  sd: TSpiderDataValue;
begin
  sd := getValue(aLabel, aAxis);
  if Assigned(sd)
  then Result := sd.Data
  else Result := nil;
end;

function TSpiderData.getValueLink(const aLabel, aAxis: string): string;
var
  sd: TSpiderDataValue;
begin
  sd := getValue(aLabel, aAxis);
  if Assigned(sd)
  then Result := sd.link
  else Result := '';
end;

function TSpiderData.getValueValue(const aLabel, aAxis: string): Double;
var
  sd: TSpiderDataValue;
begin
  sd := getValue(aLabel, aAxis);
  if Assigned(sd)
  then Result := sd.value
  else Result := Double.NaN;
end;

function TSpiderData.maxValue: Double;
var
  row: TObjectList<TSpiderDataValue>;
  col: TSpiderDataValue;
begin
  Result := Double.NaN;
  for row in fValues do
    for col in row do
    begin
      if (not col.value.IsNan) and (Result.IsNan or (Result<col.value))
      then Result := col.value;
    end;
end;

procedure TSpiderData.normalize(aMaxScale: Double);
// recursive
var
  row: TObjectList<TSpiderDataValue>;
  col: TSpiderDataValue;
begin
  normalize(maxValue, aMaxScale);
  for row in fValues do
    for col in row do
    begin
      if Assigned(col.data)
      then col.data.normalize(aMaxScale);
    end;
end;

function TSpiderData.RecalculateAverages: Double;
// recursive
var
  row: TObjectList<TSpiderDataValue>;
  col: TSpiderDataValue;
  c: Integer;
  v: Double;
begin
  Result := Double.NaN;
  c := 0;
  for row in fValues do
    for col in row do
    begin
      if Assigned(col.data)  then
      begin
        v := col.data.RecalculateAverages();
        if not v.IsNan
        then col.value := v;
      end;
      if not col.value.IsNan then
      begin
        if Result.IsNan
        then Result := col.value
        else Result := Result+col.value;
        c := c+1;
      end;
    end;
  if not Result.IsNan
  then Result := Result/c;
end;

procedure TSpiderData.setValueData(const aLabel, aAxis: string; const Value: TSpiderData);
var
  sd: TSpiderDataValue;
begin
  sd := getValue(aLabel, aAxis);
  if not Assigned(sd)
  then sd := createValue(aLabel, aAxis);
  // clean up previous value (ignrores nil)
  sd.data.Free;
  sd.data := Value; // new value now owned
end;

procedure TSpiderData.setValueLink(const aLabel, aAxis, Value: string);
var
  sd: TSpiderDataValue;
begin
  sd := getValue(aLabel, aAxis);
  if not Assigned(sd)
  then sd := createValue(aLabel, aAxis);
  sd.link := Value;
end;

procedure TSpiderData.setValueValue(const aLabel, aAxis: string; const Value: Double);
var
  sd: TSpiderDataValue;
begin
  sd := getValue(aLabel, aAxis);
  if not Assigned(sd)
  then sd := createValue(aLabel, aAxis);
  sd.value := Value;
end;

procedure TSpiderData.normalize(aMaxValue, aMaxScale: Double);
// single
var
  row: TObjectList<TSpiderDataValue>;
  col: TSpiderDataValue;
begin
  if not (aMaxValue.IsNan or (aMaxValue=0)) then
  begin
    for row in fValues do
      for col in row do
      begin
        if not col.value.IsNan
        then col.value := aMaxScale*col.value/aMaxValue;
      end;
  end;
end;

{ TSpiderChart }
(*
procedure TSpiderChart.AddorSetCategory(const aName, aCategory: string{; aValue: Double});
var
  categories: TObjectDictionary<string, TSpiderData>;
  cat: TSpiderData;
begin
  if not fData.TryGetValue(aName, categories) then
  begin
    categories := TObjectDictionary<string, TSpiderData>.Create;
    fData.Add(aName, categories);
  end;
  if not categories.TryGetValue(aCategory, cat) then
  begin
    cat := TSpiderData.Create(aCategory, Double.NaN);
    categories.Add(aCategory, cat);
  end;
  //else cat.value := aValue;
end;

procedure TSpiderChart.AddOrSetSubCategory(const aName, aCategory, aSubCategory: string; aValue: Double);
var
  categories: TObjectDictionary<string, TSpiderData>;
  cat: TSpiderData;
begin
  if not fData.TryGetValue(aName, categories) then
  begin
    categories := TObjectDictionary<string, TSpiderData>.Create;
    fData.Add(aName, categories);
  end;
  if not categories.TryGetValue(aCategory, cat) then
  begin
    cat := TSpiderData.Create(aCategory, double.NaN);
    categories.Add(aCategory, cat);
  end;
  cat.subcategoriesValues.AddOrSetValue(aSubCategory, aValue);
end;
*)

constructor TSpiderChart.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, 'spider');
  fData := TSpiderData.Create(aName);
  fWidth := 400;
  fHeight := 400;
  fMaxScale := 10; // als niet gedefineerd (NaN) pakt hij de maximale van alle values -> 10 is een goede default?
end;

destructor TSpiderChart.Destroy;
begin
  FreeAndNil(fData);
  inherited;
end;

function TSpiderChart.getJSON: string;
begin
  Result := inherited getJSON;
  if Result<>''
  then Result := Result+',';
  Result := Result+
    //'"level":0'+
    '"width":'+fWidth.toString+',"height":'+fHeight.toString+',"maxValue":'+fMaxScale.toString(dotFormat);
//  if fLabels
//  then Result := Result+',"labels":true'
//  else Result := Result+',"labels":false';
//  if fLegend
//  then Result := Result+',"legend":true'
//  else Result := Result+',"legend":false';
//  if not double.IsNan(fMaxScale)
//  then Result := Result+ ',"maxValue":'+fMaxScale.toString(dotFormat);

end;

function TSpiderChart.getJSONData: string;
begin
  fData.normalize(fMaxScale);
  Result := '{'+fData.getJSON+'}';
end;

procedure TSpiderChart.normalize;
begin
  fData.normalize(fMaxScale);
end;

procedure TSpiderChart.RecalculateAverages;
begin
  fData.RecalculateAverages;
end;

{ TEnselSpiderChart }

constructor TEnselSpiderChart.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aDataEvent: TEventEntry; aSegment: Integer; const aComponent: string);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  fSegment := aSegment;
  fComponent := aComponent;
  fDataEvent := aDataEvent;
  fDataEventHandler := handleDataEvent;
  fDataEvent.OnEvent.Add(fDataEventHandler);
end;

destructor TEnselSpiderChart.Destroy;
begin
  fDataEvent.OnEvent.Remove(fDataEventHandler);
  inherited;
end;

procedure TEnselSpiderChart.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  timestamp: Double;
  roadSegementID: Integer;
  transportMode: string;
  componentName: string;
  TimeCategory: string;
  AvgConcentration: Double;
  duration: Double;
  objectID: TGUID;
  sd: TSpiderDataValue;
begin
  duration := 0;
  AvgConcentration := 0;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        objectID := aPayload.bb_read_guid(aCursor);
      ((icehWorldCommandBase+2) shl 3) or wt64Bit: // time stamp
        timestamp := aPayload.bb_read_double(aCursor);
      kpi_road_segment_id:
        roadSegementID := aPayload.bb_read_int32(aCursor);
      kpi_TransportMode:
        transportMode :=  aPayload.bb_read_string(aCursor);
      kpi_ComponentName:
        componentName :=  aPayload.bb_read_string(aCursor);
      kpi_Time_Category:
        TimeCategory :=  aPayload.bb_read_string(aCursor);
      kpi_avg_concentration:
        begin
          AvgConcentration := aPayload.bb_read_double(aCursor);
          if componentName=fComponent then
          begin
            // todo: not correct
            sd := fData.GetOrAddValue('Average '+componentName, transportMode);
            if not Assigned(sd.data)
            then sd.data := TSpiderData.Create('Average '+componentName+' - '+transportMode);
            sd.data.GetOrAddValue('Average '+componentName+' - '+transportMode, TimeCategory).value := AvgConcentration;

            sd := fData.GetOrAddValue('Average exposure', transportMode);
            if not Assigned(sd.data)
            then sd.data := TSpiderData.Create('Average exposure'+' - '+transportMode);
            sd.data.GetOrAddValue('Average exposure'+' - '+transportMode, TimeCategory).value := AvgConcentration*duration;

//            fData.valueValue['Average '+componentName, transportMode] := AvgConcentration;
//            fData.valueValue['Average exposure', transportMode] := AvgConcentration*duration;
            //TimeCategory

//            fData.AddOrSetData([], TimeCategory, AvgConcentration);
//            fData.AddOrSetData(['Average exposure', transportMode], TimeCategory, AvgConcentration);
            //AddOrSetSubCategory('Average '+componentName, transportMode, TimeCategory, AvgConcentration);
            //AddOrSetSubCategory('Average exposure', transportMode, TimeCategory, AvgConcentration*duration);
          end;
        end;
      kpi_duration:
        begin
          duration := aPayload.bb_read_double(aCursor);
          if componentName=fComponent then
          begin
            // todo: not correct
            sd := fData.GetOrAddValue('Average time', transportMode);
            if not Assigned(sd.data)
            then sd.data := TSpiderData.Create('Average time'+' - '+transportMode);
            sd.data.GetOrAddValue('Average time'+' - '+transportMode, TimeCategory).value := duration;

            fData.valueValue['Average time', transportMode] := duration;
            //fData.AddOrSetData(['Average time', transportMode], TimeCategory, duration);
            //AddOrSetSubCategory('Average time', transportMode, TimeCategory, duration);
          end;
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
  // update averages
  RecalculateAverages;

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

procedure TEnselScenario.ReadBasicData;
var
  palette: TWDPalette;
  spider: TEnselSpiderChart;
  layer: TEnselMobileSensorLayer;
  mobileChart: TChartLines;
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


  palette := CreateNiekPalette('NO2');//  CreateNO2Palette;
  addConcentrationLayer(
    'Group exposure', 'no2', 'NO2', 'Total exposure to NO2', false,
    sensordata_no2_total, 'receptordata', palette, BuildLegendJSON(palette));

  palette := CreateNiekPalette('PM10'); // CreatePM10Palette;
  addConcentrationLayer(
    'Group exposure', 'pm10', 'PM10', 'Total exposure to PM10', false,
    sensordata_pm10_total, 'receptordata', palette, BuildLegendJSON(palette));

  palette := CreateNiekPalette('NO2'); // CreateNO2Palette;
  addConcentrationLayer(
    'Group exposure',  'no2assim', 'NO2 assim', 'Total assimilated exposure to NO2', false,
    sensordata_assim_no2_total, 'receptordata', palette, BuildLegendJSON(palette));

  palette := CreateNiekPalette('PM10'); // CreatePM10Palette;
  addConcentrationLayer(
    'Group exposure', 'pm10assim', 'PM10 assim', 'Total assimilated exposure to PM10', false,
    sensordata_assim_pm10_total, 'receptordata', palette, BuildLegendJSON(palette));


  //spider := TEnselSpiderChart.Create(Self, 'Group exposure per segment', 'spiderseg', 'Group NO2 exposure by mode of transport (MOT) per segment', '', False,
  //  project.Connection.subscribe('ensel_kpi_group_segments'), 1, 'no2'); // per segment

  spider := TEnselSpiderChart.Create(Self, 'Group exposure', 'spiderNO2', 'Group NO2 exposure by mode of transport (MOT)', '', False,
    project.Connection.subscribe('ensel_kpi_group'), -1, 'no2'); // all
  AddChart(spider);
  //spider := TEnselSpiderChart.Create(Self, 'Group exposure per segment', 'spiderseg', 'Group NO2 exposure by mode of transport (MOT) per segment', '', False,
  //  project.Connection.subscribe('ensel_kpi_group_segments'), 1, 'no2'); // per segment

  spider := TEnselSpiderChart.Create(Self, 'Group exposure', 'spiderPM10', 'Group PM10 exposure by mode of transport (MOT)', '', False,
    project.Connection.subscribe('ensel_kpi_group'), -1, 'pm10'); // all
  AddChart(spider);

  mobileChart :=  TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts', 'Mobile sensors', '', False, 'line',

    TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
    [TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3')]);

  AddChart(mobileChart);

  palette := CreateNiekPalette('NO2');//  CreateNO2Palette;
  layer := TEnselMobileSensorLayer.Create(Self, 'Personal exposure', 'mobilesensors', 'Mobile sensors', '', false, true, palette, BuildLegendJSON(palette), mobileChart);
  AddLayer(layer);
  layer.RegisterLayer;


//  spider.AddOrSetCategory('avg NO2', 'bike', Double.NaN);
//  spider.AddOrSetCategory('avg NO2','walk', Double.NaN);
//  spider.AddOrSetCategory('avg NO2', 'motorized', Double.NaN);
//  spider.AddOrSetCategory('avg NO2', 'other', Double.NaN);

//  spider.AddOrSetCategory('avg time', 'bike', Double.NaN);
//  spider.AddOrSetCategory('avg time', 'walk', Double.NaN);
//  spider.AddOrSetCategory('avg time', 'motorized', Double.NaN);
//  spider.AddOrSetCategory('avg time', 'other', Double.NaN);

//  spider.AddOrSetCategory('avg exposure', 'bike', Double.NaN);
//  spider.AddOrSetCategory('avg exposure', 'walk', Double.NaN);
//  spider.AddOrSetCategory('avg exposure', 'motorized', Double.NaN);
//  spider.AddOrSetCategory('avg exposure', 'other', Double.NaN);





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
  Result := TEnselScenario.Create(Self, aID, 'Utrecht', 'Incident in Utrecht', false, Self.mapView, False); // todo:
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
  fProjects := TDictionary<string, TProject>.Create;
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


{ TEnselMobileSensorLayer }

constructor TEnselMobileSensorLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad,
  aShowInDomains: Boolean; aPalette: TWDPalette; aLegendJSON: string; aChart: TChartLines);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"mobilesensor"', 'Point', ltTile, aShowInDomains, 0);
  // mobile sensor points
  fChart := aChart;
  fPalette :=  aPalette;
  fLegendJSON := aLegendJSON;
  fDataEventHandler := HandleEvent;
  fDataEvent := scenario.project.connection.Subscribe('mobilesensordata');
  fDataEvent.OnEvent.Add(fDataEventHandler);
  fPrivateDataEvent := scenario.project.connection.Subscribe(scenario.project.Connection.privateEventName+'.'+'mobilesensordata', false);
  fPrivateDataEvent.OnEvent.Add(fDataEventHandler);
  // inquire
  fDataEvent.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, 'ORDER BY ts')); //'ts>=42700 ORDER BY ts'));
end;

destructor TEnselMobileSensorLayer.Destroy;
begin
  if Assigned(fDataEvent) then
  begin
    if fDataEvent.OnEvent.Contains(fDataEventHandler)
    then fDataEvent.OnEvent.Remove(fDataEventHandler);
  end;
  inherited;
end;

function TEnselMobileSensorLayer.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := True;
end;

function TEnselMobileSensorLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := True;
end;

procedure TEnselMobileSensorLayer.AddPoint(aObjectID: TGUID; aTimeStamp, aLat, aLon: Double; aSubstance: UInt32; aValue: Double);
var
  wdid: TWDID;
  geometryPoint: TWDGeometryPoint;
//  o: TLayerObject;
begin
  //wdid := TWDID(aObjectID.ToString());
  wdid := TWDID(TGUID.NewGuid.ToString);
  //if not objects.TryGetValue(wdid, o) then
  //begin
  geometryPoint := TWDGeometryPoint.Create;
  geometryPoint.x := aLon;
  geometryPoint.y := aLat;
  AddObject(TGeometryPointLayerObject.Create(Self, wdid, geometryPoint, aValue));
  //Log.WriteLn('ms: '+aLat.ToString()+' x '+aLon.ToString()+': '+aSubstance.ToString()+': '+aValue.ToString());
  fChart.AddValue(aTimeStamp, [aValue]);
//  end
//  else
//  begin
//    o
//  end;
end;

procedure TEnselMobileSensorLayer.HandleEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  objectID: TGUID;
  timestamp: Double;
//  pm1: double;
//  pm10: double;
  no2: double;
//  pm25: double;
//  co2: double;
begin
  timestamp := 0;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        objectID := aPayload.bb_read_guid(aCursor);
      ((icehWorldCommandBase+2) shl 3) or wt64Bit: // time stamp
        timestamp := aPayload.bb_read_double(aCursor);
      sensordata_longitude:
        fLastLon := aPayload.bb_read_double(aCursor);
      sensordata_latitude:
        fLastLat := aPayload.bb_read_double(aCursor);
      sensordata_no2:
        begin
          no2 := aPayload.bb_read_double(aCursor);
          if (fLastLat<>0) and (fLastLon<>0)
          then AddPoint(objectID, timestamp, fLastLat, fLastLon, sensordata_no2, no2);
          // todo: ignoring values on 0,0 for now
        end;

      {
      sensordata_pm1:
        pm1 := aPayload.bb_read_double(aCursor);
      sensordata_pm10:
        pm10  := aPayload.bb_read_double(aCursor);
      sensordata_pm25:
        pm25 := aPayload.bb_read_double(aCursor);
      137: // CO2
        co2 := aPayload.bb_read_double(aCursor);
      }


      {
      kpi_road_segment_id:
        roadSegementID := aPayload.bb_read_int32(aCursor);
      kpi_TransportMode:
        transportMode :=  aPayload.bb_read_string(aCursor);
      kpi_ComponentName:
        componentName :=  aPayload.bb_read_string(aCursor);
      kpi_Time_Category:
        TimeCategory :=  aPayload.bb_read_string(aCursor);
      kpi_avg_concentration:
        begin
          AvgConcentration := aPayload.bb_read_double(aCursor);
          AddOrSetSubCategory('Average '+componentName, transportMode, TimeCategory, AvgConcentration);
          AddOrSetSubCategory('Average exposure', transportMode, TimeCategory, AvgConcentration*duration);
        end;
      kpi_duration:
        begin
          duration := aPayload.bb_read_double(aCursor);
          AddOrSetSubCategory('Average time', transportMode, TimeCategory, duration);
        end;
      }
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TEnselMobileSensorLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500, fPalette);
end;

procedure TEnselMobileSensorLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice(nil);
end;

function TEnselMobileSensorLayer.SliceType: Integer;
begin
  Result := stLocation;
end;

end.
