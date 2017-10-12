unit PublishServerSesmi;

interface

uses
  Logger,

  imb4,

  StdIni,

  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,

  TilerControl,

  PublishServerLib,
  PublishServerGIS,

  GisCsSystems,

  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils,
  System.RegularExpressions;

{
  emmission layer
  sensor layer -> point value, concentration, time,
}

const

  sensordata_longitude             = 129;               //tag 16
  sensordata_latitude              = 121;               //tag 15

  sensordata_pm10                  = 1441;              //tag 180
  sensordata_pm25                  = 1601;              //tag 200
  sensordata_no2                   = 1921;              //tag 240
  //sensordata_no2                   = 961;               //tag 120 -> temporary tag!
  sensordata_pm1                   = 2081;              //tag 260
  sensordata_nh3                   = 2241;              //tag 280
  sensordata_pnc                   = 2401;              //tag 300
  sensordata_nox                   = 2561;              //tag 320
  sensordata_linkid                = 2882;              //tag 360

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

  TimeSpanSwitch = 'timespan';
  DefaultTimeSpan = 7;

type
  TSesmiClient = class(TClient)
  constructor Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
  protected
    procedure Login(aJSONObject: TJSONObject); override;
  end;

  TTimedValue = record
    time: Double;
    value: Double;
  end;

  TSesmiLink = class(TGeometryLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry);
  destructor Destroy; override;
  private
    fTotalValue: Double;
    fValueList: TList<TTimedValue>;
  protected
  public
    procedure UpdateValue(aTimeStamp, aValue: Double);
    procedure Reset();
  end;

  TSesmiTileLayer = class(TLayer)
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
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;
{
  TSesmiTrackLayer  = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean);
  private
    fEvent: TEventEntry;
    fEventHandler: TOnEvent;
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
 }

  TSesmiTrackLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean; aPallette: TWDPalette; aLegendJSON: string);
  destructor Destroy; override;
  private
    fLastLats: TDictionary<TGUID, Double>;
    fLastLons: TDictionary<TGUID, Double>;
  protected
  public
    procedure AddPoint(aObjectID: TGUID; aLat, aLon, aValue: Double);
    procedure AddLat(aSensorId: TGUID; aLat: Double);
    procedure AddLon(aSensorId: TGUID; aLon: Double);
    procedure AddValue(aSensorId: TGUID; aValue: Double);//value is always last
    procedure Reset;
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;


  TSesmiMobileSensorLayer  = class(TLayer)
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

  TSesmiLinkLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean;  aPalette: TWDPalette; aLegendJSON: string; aChart, aTotalChart: TChartLines);
  destructor Destroy; override;
  private
    valueList: TDictionary<Double, Double>;
    linkidList: TDictionary<Double, TWDID>;
    fPalette: TWDPalette;
    fChart: TChartLines; //used to lock fChart and fTotalChart
    fTotalChart: TChartLines;
    fTotalValue: Double;
    fPrevTime: Double;
    fPrevValue: Double;
    procedure ProcessMatch(const aTimeStamp, aValue: Double; aID: TWDID);
  protected
  public
    procedure AddValue(const aTimeStamp, aValue: Double);
    procedure AddLinkID(const aTimeStamp: Double; const aID: TWDID);
    procedure AddLink(const aID: TWDID; aGeometry: TWDGeometry);
    procedure Reset;
    function SliceType: Integer; override;
    procedure RegisterSlice; override;
    procedure RegisterLayer; override;
  end;

  TSesmiWindData = class
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

  TSesmiSpiderChart = class(TSpiderChart)
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

  TSesmiScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView; aUseSimulationSetup: Boolean);
  destructor Destroy; override;
  private
    fLinkLayers: TDictionary<Integer, TSesmiLinkLayer>;
    fTrackLayers: TDictionary<Integer, TSesmiTrackLayer>;
  protected
    fGUID: TGUID;
    fLive: Boolean;
    fQueryCounter: Integer;
    fQuerySubscribed: Boolean;
    fPubEvent: TEventEntry;
    fQueryEvent: TEventEntry;
    fLiveEvent: TEventEntry;
    fQueryEventHandler: TOnEvent;
    fLiveEventHandler: TOnEvent;
    fLiveCounter: Integer;
    fDBCounter: Integer;
    procedure handleLiveEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleQueryEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure AddSesmiLinkLayer(const aKey: UInt32; const aDomain, aID, aName, aDescription: string; aPalette, aTrackPalette: TWDPalette; const aLegendJSON: string);
  public
    procedure InquireDB(const aInquire: string; const aLowerTimestamp, aUpperTimestamp: Double);
    procedure ReadBasicData(); override;
    procedure addConcentrationLayer(
      const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
      aKey: UInt32; const aEventName: string;
      aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
    procedure AddLink(const aGuid: TGUID; const aGeometry: TWDGeometry);
    procedure GoLive(const first: Boolean = False);
    procedure GoDB(const aInquire: string; const aLowerTimestamp, aUpperTimestamp: Double);
    procedure Reset;

    property Live: Boolean read fLive;
  public
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
//    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; const aQuery: string): string; overload; override;
//
//    function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; override;
  end;

  TSesmiProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aAddBasicLayers: Boolean; const aDateFormData: string; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView; const aExpertScenarioGUID: TGUID);
  destructor Destroy; override;
  private
    fPubEvent: TEventEntry;
    fNetworkEvent: TEventEntry;
    fLinks: TDictionary<TGUID, TWDGeometry>;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
    fWindData: TSesmiWindData;
    fExpertScenarioGUID: TGUID;
    //fComplaints: TSesmiComplaints;
    //fSensorsLayer: TSesmiSensorsLayer;
    procedure InquireNetwork;
    procedure handleNetworkEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  protected
    procedure ReadObjects(aSender: TObject);
    function getMeasuresJSON: string; override;
    function handleTilerStatus(aTiler: TTiler): string;
    procedure handleNewClient(aClient: TClient); override;
  public
    function ReadScenario(const aID: string): TScenario; override;
    procedure ReadBasicData(); override;
    function addClient(const aClientID: string): TClient; override;
    function CreateSesmiScenario(const aScenarioID: string): TSesmiScenario;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  public
    property pubEvent: TEventEntry read fPubEvent;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
    property ExpertScenarioGUID: TGUID read fExpertScenarioGUID;
  end;

  TSesmiModule = class
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string;
    aMaxNearestObjectDistanceInMeters: Integer; const aExpertScenarioGUID: TGUID;
    const aProjectName: string; aMapView: TMapView);
  destructor Destroy; override;
  private
    fSessionModel: TSessionModel;
    fConnection: TConnection;
    fTilerFQDN: string;
    fTilerStatusURL: string;
    fProjects: TDictionary<string, TProject>;
    fMaxNearestObjectDistanceInMeters: Integer;
    public
    property Projects: TDictionary<string, TProject> read fProjects;
  end;

implementation

//convert a Guid to TWDID
function GuidToTWDID(aGuid: TGUID): TWDID;
begin
  SetLength(Result, SizeOf(aGuid));
  Move(aGuid, PAnsiChar(Result)^, SizeOf(aGuid));
end;

//Discrete pallette for the Sesmi project
function CreateHansPalette(const aTitle: string): TWDPalette;
begin
  Result := TDiscretePalette.Create(aTitle, [
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff000000), -40000, 0, '<0'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff0047ba), 0, 25, '0-25'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff6da5ff), 25, 30, '25-30'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ffffff00), 30, 45, '30-45'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ffff8000), 45, 60, '45-60'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ffff0000), 60, 75, '60-75'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff8100c1), 75, 20000, '>75')
  ],TGeoColors.Create($00000000)); //default: transparant
end;

function CreateNiekPalette(const aTitle: string): TWDPalette;
var
  factor: Double;
begin
  factor := 1 / 1000000000;
  Result := TRampPalette.Create(aTitle, [
    TRampPaletteEntry.Create($Ff00AF00, 0 * factor, '0'),
    //TRampPaletteEntry.Create($FF00C800, 5, '5'),
    TRampPaletteEntry.Create($FF00E100, 10 * factor, '10'),
    //TRampPaletteEntry.Create($FF32FF32, 15, '15'),
    TRampPaletteEntry.Create($FF7DFF4B, 20 * factor, '20'),
    //TRampPaletteEntry.Create($FFC8FF4B, 25, '25'),
    TRampPaletteEntry.Create($FFF2FF4B, 30 * factor, '30'),
    //TRampPaletteEntry.Create($FFFFFA01, 35, '35'),
    TRampPaletteEntry.Create($FFFFE101, 40 * factor, '40'),
    //TRampPaletteEntry.Create($FFFFC801, 45, '45'),
    TRampPaletteEntry.Create($FFFFAF01, 50 * factor, '50'),
    //TRampPaletteEntry.Create($FFFF9601, 55, '55'),
    TRampPaletteEntry.Create($FFFF7D01, 60 * factor, '60'),
    //TRampPaletteEntry.Create($FFFF6401, 65, '65'),
    TRampPaletteEntry.Create($FFFF4B01, 70 * factor, '70'),
    //TRampPaletteEntry.Create($FFFF0000, 75, '75'),
    TRampPaletteEntry.Create($FFE10000, 80 * factor, '80'),
    //TRampPaletteEntry.Create($FFC80000, 85, '85'),
    TRampPaletteEntry.Create($FFAF0000, 90 * factor, '90'),
    //TRampPaletteEntry.Create($FF960019, 95, '95'),
    TRampPaletteEntry.Create($FF7D0032, 100 * factor, '100'),
    //TRampPaletteEntry.Create($FF6E004B, 105, '105'),
    TRampPaletteEntry.Create($FF640064, 110 * factor, '110'),
    //TRampPaletteEntry.Create($FF500073, 115, '115'),
    TRampPaletteEntry.Create($FF37005C, 120 * factor, '120')],
      $FF00AF00,
      $00000000,
      $FF37005C);
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
  then Result := BuildDiscreteLegendJSON(aPalette as TDiscretePalette, TLegendFormat.lfVertical)
  else Result := '';
end;


{ TSesmiTileLayer }

constructor TSesmiTileLayer.Create(
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
  fDataEvent := scenario.project.Connection.eventEntry(fEventName).subscribe;
  if not fDataEvent.OnEvent.Contains(fHandleDataHandlerRef)
  then fDataEvent.OnEvent.Add(fHandleDataHandlerRef);

  fPrivateDataEvent := scenario.project.Connection.eventEntry(scenario.project.Connection.privateEventName+'.'+fEventName, False).subscribe;
  if not fPrivateDataEvent.OnEvent.Contains(fHandleDataHandlerRef)
  then fPrivateDataEvent.OnEvent.Add(fHandleDataHandlerRef);
end;

destructor TSesmiTileLayer.Destroy;
begin
  FreeAndNil(fPalette);
  inherited;
end;

procedure TSesmiTileLayer.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TWDID;
//  timestamp: TDateTime;
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
      if Assigned((scenario.project as TSesmiProject).sourceProjection)
      then projectGeometryPoint(gp, (scenario.project as TSesmiProject).sourceProjection);
      gplo := TGeometryPointLayerObject.Create(self, aID, gp, aValue);
      objects.AddOrSetValue(gplo.ID, gplo);
      //Log.WriteLn('emission: '+aSourceID.ToString+' @ '+gp.x.ToString(dotFormat)+','+gp.y.ToString(dotFormat)+': '+aValue.ToString(dotFormat));
      if Assigned(tilerLayer)
      then tilerLayer.signalData(gplo.encode, 0); // todo: timestamp
    end
    else Log.WriteLn('TSesmiEmissionLayer.handleDataEvent addLayerObject: ignoring NaN coordinates', llWarning);
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
//  timestamp := double.NaN;
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
//        ((icehWorldCommandBase+2) shl 3) or wt64Bit:
//          timestamp := aPayload.bb_read_double(aCursor);
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

procedure TSesmiTileLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name);
end;

procedure TSesmiTileLayer.RegisterSlice;
begin
  if Assigned(fPalette)
  then tilerLayer.signalAddSlice(fPalette.Clone)
  else tilerLayer.signalAddSlice(nil);
  //signalInquire(); // layer is defined on tiler so now we are ready to send inquire signal
end;

//procedure TSesmiTileLayer.signalInquire(const aQuery: string);
//begin
//  fDataEvent.signalEvent(
//    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
//    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aQuery));
//end;

function TSesmiTileLayer.SliceType: Integer;
begin
  Result := fLayerType; // in Sesmi slice type=layer type
end;

(*
{ TSesmiEmissionLayer }

constructor TSesmiEmissionLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  inherited;
  fPrivateDataEvent := aScenario.project.Connection.subscribe(aScenario.project.Connection.privateEventName+'.sources', false);
  fPrivateDataEvent.OnEvent.Add(handleDataEvent);
  fDataEvent := aScenario.project.Connection.subscribe('Sesmi.sources', false);
  fDataEvent.OnEvent.Add(handleDataEvent);
  RegisterLayer;
end;

procedure TSesmiEmissionLayer.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
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
      projectGeometryPoint(gp, (scenario.project as TSesmiProject).sourceProjection);
      gplo := TGeometryPointLayerObject.Create(self, wdid, gp, aValue);
      objects.AddOrSetValue(gplo.ID, gplo);
      //Log.WriteLn('emission: '+aSourceID.ToString+' @ '+gp.x.ToString(dotFormat)+','+gp.y.ToString(dotFormat)+': '+aValue.ToString(dotFormat));
      tilerLayer.signalData(gplo.encode, 0); // todo: timestamp
    end
    else Log.WriteLn('TSesmiEmissionLayer.handleDataEvent addLayerObject: ignoring NaN coordinates', llWarning);
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
{ TSesmiSensorsLayer }

constructor TSesmiSensorsLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  fGUIDToID := TDictionary<TGUID, Integer>.Create;
  inherited;
  fPrivateDataEvent := aScenario.project.Connection.subscribe(aScenario.project.Connection.privateEventName+'.sensordata', false);
  fPrivateDataEvent.OnEvent.Add(handleDataEvent);
  fDataEvent := aScenario.project.Connection.subscribe('Sesmi.sensordata', false);
  fDataEvent.OnEvent.Add(handleDataEvent);
  RegisterLayer;
end;

destructor TSesmiSensorsLayer.Destroy;
begin
  fGUIDToID.Free;
  inherited;
end;

procedure TSesmiSensorsLayer.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
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
      projectGeometryPoint(gp, (scenario.project as TSesmiProject).sourceProjection);
      gplo := TGeometryPointLayerObject.Create(self, wdid, gp, aValue);
      objects.AddOrSetValue(gplo.ID, gplo);
      //Log.WriteLn('sensor: '+wdid+' @ '+gp.x.ToString(dotFormat)+','+gp.y.ToString(dotFormat)+': '+aValue.ToString(dotFormat));
      tilerLayer.signalData(gplo.encode, 0); // todo: timestamp

      scenario.project.SendString('{"sensor":{"sensorid":'+localID.ToString+',"name":"'+localID.ToString+'","address":"","latitude":'+gp.y.ToString(dotFormat)+',"longitude":'+gp.x.ToString(dotFormat)+',"measuredsubstance":"benzeen","mobile":false}}');
      scenario.project.SendString('{"sensordata":{"sensorid":'+localID.ToString+',"concentration":'+aValue.ToString(dotFormat)+',"latitude":'+gp.y.ToString(dotFormat)+',"longitude":'+gp.x.ToString(dotFormat)+',"time":"'+formatDateTime('yyyy-mm-dd hh:nn:ss', aTimeUTC)+'"}}');
    end
    else Log.WriteLn('TSesmiSensorsLayer.handleDataEvent addLayerObject: ignoring NaN coordinates', llWarning);
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

{ TSesmiTrackLayer }
(*
constructor TSesmiTrackLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad, aShowInDomains: Boolean);
//var
//  entries: TPaletteDiscreteEntryArray;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"track"', 'Point', ltObject, aShowInDomains, 0);
  // Track
  fEvent := scenario.project.connection.Subscribe('track');
  fEventHandler := HandleEvent;
  fEvent.OnEvent.Add(fEventHandler);
end;

function TSesmiTrackLayer.HandleClientSubscribe(aClient: TClient): Boolean;
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


function TSesmiTrackLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  // todo: option to clear objects

  Result := inherited;
end;

procedure TSesmiTrackLayer.HandleEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
begin
  // todo: build track

end;

procedure TSesmiTrackLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500);
end;

procedure TSesmiTrackLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice(nil);
end;

function TSesmiTrackLayer.SliceType: Integer;
begin
  Result := stLocation;
end;
*)
{ TSesmiWindData }

constructor TSesmiWindData.Create(aProject: TProject);
begin
  inherited Create;
  fProject := aProject;
  fDataEvent := project.Connection.eventEntry('meteodata').subscribe;
  fDataEvent.OnEvent.Add(handleMeteoDataEvent);
end;

procedure TSesmiWindData.handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
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

{ TSesmiSpiderChart }

constructor TSesmiSpiderChart.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aDataEvent: TEventEntry; aSegment: Integer; const aComponent: string);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  fSegment := aSegment;
  fComponent := aComponent;
  fDataEvent := aDataEvent;
  fDataEventHandler := handleDataEvent;
  fDataEvent.OnEvent.Add(fDataEventHandler);
end;

destructor TSesmiSpiderChart.Destroy;
begin
  fDataEvent.OnEvent.Remove(fDataEventHandler);
  inherited;
end;

procedure TSesmiSpiderChart.handleDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
//  timestamp: Double;
//  roadSegementID: Integer;
  transportMode: string;
  componentName: string;
  TimeCategory: string;
  AvgConcentration: Double;
  duration: Double;
  objectID: TGUID;
  sd: TSpiderDataValue;
begin
  duration := 0;
//  AvgConcentration := 0;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        objectID := aPayload.bb_read_guid(aCursor);
//      ((icehWorldCommandBase+2) shl 3) or wt64Bit: // time stamp
//        timestamp := aPayload.bb_read_double(aCursor);
//      kpi_road_segment_id:
//        roadSegementID := aPayload.bb_read_int32(aCursor);
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

{ TSesmiScenario }

procedure TSesmiScenario.addConcentrationLayer(
  const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  aKey: UInt32; const aEventName: string;
  aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
var
  layer: TSesmiTileLayer;
begin
  // create a layer and link to listener
  layer := TSesmiTileLayer.Create(
    Self, aDomain, aID, aName, aDescription, aDefaultLoad, '"receptor"', 'Point', stReceptor,
    aKey, aEventName,
    aPalette, aLegendJSON);
  AddLayer(layer);
  layer.RegisterLayer;
end;

procedure TSesmiScenario.AddLink(const aGuid: TGUID; const aGeometry: TWDGeometry);
var
  linkLayer: TSesmiLinkLayer;
begin
  TMonitor.Enter(fLinkLayers);
  try
    for linkLayer in fLinkLayers.Values do
    begin
      //linkLayer.AddLink(GuidToTWDID(aGuid), aGeometry);
    end;
  finally
    TMonitor.Exit(fLinkLayers);
  end;
end;

procedure TSesmiScenario.AddSesmiLinkLayer(const aKey: UInt32; const aDomain, aID, aName, aDescription: string; aPalette, aTrackPalette: TWDPalette; const aLegendJSON: string);
var
  layer: TSesmiLinkLayer;
  trackLayer: TSesmiTrackLayer;
  mobileChart, totalChart: TChartLines;
begin
  mobileChart :=  TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts' + aID, aName, aDescription, False, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [TChartAxis.Create('concentratie', 'lightBlue', 'Concentration', 'mg/m3')], 'time');
  AddChart(mobileChart);

  totalChart := TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts' + aID + 'total', aName + '-total', aDescription + ' Total', False, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [TChartAxis.Create('concentratie', 'lightBlue', 'Concentration', 'mg/m3')], 'time');
  AddChart(totalChart);

  layer := TSesmiLinkLayer.Create(Self, 'Personal exposure', fID + 'personal-' + aName, 'Personal ' + aName, aName, False, True, aPalette, BuildLegendJSON(aPalette), mobilechart, totalChart);
  TMonitor.Enter(fLinkLayers);
  try
    begin
      fLinkLayers.AddOrSetValue(aKey, layer);
    end
  finally
    TMonitor.Exit(fLinkLayers);
  end;
  AddLayer(layer);
  layer.RegisterLayer;

  trackLayer := TSesmiTrackLayer.Create(Self, 'Personal exposure', fID + 'personal-track-' + aName, 'Personal Track ' + aName, aName, True, True, aTrackPalette, BuildLegendJSON(aTrackPalette));
  TMonitor.Enter(fTrackLayers);
  try
    begin
      fTrackLayers.AddOrSetValue(aKey, trackLayer);
    end
  finally
    TMonitor.Exit(fTrackLayers);
  end;
  AddLayer(trackLayer);
  trackLayer.RegisterLayer;
end;

constructor TSesmiScenario.Create(aProject: TProject; const aID, aName,
  aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView;
  aUseSimulationSetup: Boolean);
begin
  if TRegEx.IsMatch(aID, '^[{][0-9A-Fa-f]{8}[-]([0-9A-Fa-f]{4}[-]){3}[0-9A-Fa-f]{12}[}]$') then
  begin
    fGUID := TGUID.Create(aID);
  end
  else
    fGUID := TGUID.Empty;
  fLive := True;
  fQueryCounter := 0;
  fQuerySubscribed := False;
  fQueryEventHandler := handleQueryEvent;
  fLiveCounter := 0;
  fDBCounter := 0;
  fLinkLayers := TDictionary<Integer, TSesmiLinkLayer>.Create;
  fTrackLayers := TDictionary<Integer, TSesmiTrackLayer>.Create;
  inherited;
  fPubEvent := project.Connection.eventEntry('mobilesensordata').publish;
  fLiveEvent := project.Connection.eventEntry('mobilesensordata').subscribe;
  fLiveEventHandler := handleLiveEvent;
  fLiveEvent.OnEvent.Add(fLiveEventHandler);
end;

destructor TSesmiScenario.Destroy;
begin
  if fQuerySubscribed then //unsubscribe from the previous returnEvent
  begin
    fqueryEvent.OnEvent.Remove(fQueryEventHandler);
    project.Connection.unSubscribe(fQueryEvent);
  end;
  fLiveEvent.OnEvent.Remove(fLiveEventHandler);
  project.Connection.unSubscribe(fLiveEvent);
  project.Connection.unPublish(fPubEvent);
  FreeAndNil(fLinkLayers);
  FreeAndNil(fTrackLayers);
  inherited;
end;

procedure TSesmiScenario.GoDB(const aInquire: string; const aLowerTimestamp,
  aUpperTimestamp: Double);
begin
  EnableControl(goLiveControl);
  fLive := False;
  Reset;
  InquireDB(aInquire, aLowerTimestamp, aUpperTimestamp);
end;

procedure TSesmiScenario.GoLive(const first: Boolean = False);
var
  currentTime: TDateTime;
begin
  DisableControl(goLiveControl);
  fLive := True;
  if not first then
    Reset;
  currentTime := Now;
  InquireDB('', currentTime - GetSetting(TimeSpanSwitch, DefaultTimeSpan), currentTime);
end;

procedure TSesmiScenario.handleLiveEvent(aEventEntry: TEventEntry;
  const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  layer: TSesmiLinkLayer;
  trackLayer: TSesmiTrackLayer;
  fieldInfo: UInt32;
  value: Double;
  timestamp: Double;
  id: TWDID;
  sensorid: TGUID;
begin
  fLiveCounter := fLiveCounter + 1;
  if not Live then
    exit;
  timestamp := 0;
  sensorid := TGUID.Empty;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      wdatTimeStamp:
        begin
          timestamp := aPayload.bb_read_double(aCursor);
        end;
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          sensorid := aPayload.bb_read_guid(aCursor);
          if (fGUID <> (project as TSesmiProject).ExpertScenarioGUID) and (fGUID <> sensorid)  then //filter: only accept live events that match our id
           exit; //todo: mag dit, of moeten we de buffer leeg lezen? Wordt id altijd verstuurd voor de data?
        end;
      sensordata_no2, sensordata_pm10, sensordata_pm25:
        begin
          value := aPayload.bb_read_double(aCursor);
          if fLinkLayers.TryGetValue(fieldInfo, layer) then
            layer.AddValue(timestamp, value);
          if fTrackLayers.TryGetValue(fieldInfo, trackLayer) then
            trackLayer.AddValue(sensorid, value);
        end;
      sensordata_linkid:
        begin
          id := GuidToTWDID(aPayload.bb_read_guid(aCursor));
          TMonitor.Enter(fLinkLayers);
          try
            for layer in fLinkLayers.Values do
              layer.AddLinkID(timestamp, id);
          finally
            TMonitor.Exit(fLinkLayers);
          end;
        end;
      sensordata_latitude:
        begin
          value := aPayload.bb_read_double(aCursor);
          for trackLayer in fTrackLayers.Values do
            begin
              trackLayer.AddLat(sensorid, value);
            end;
        end;
      sensordata_longitude:
        begin
          value := aPayload.bb_read_double(aCursor);
          for trackLayer in fTrackLayers.Values do
            begin
              trackLayer.AddLon(sensorid, value);
            end;
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSesmiScenario.handleQueryEvent(aEventEntry: TEventEntry;
  const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  layer: TSesmiLinkLayer;
  trackLayer: TSesmiTrackLayer;
  fieldInfo: UInt32;
  value, timestamp: Double;
  id: TWDID;
  sensorid: TGUID;
begin
  fDBCounter := fDBCounter + 1;
  timestamp := 0;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      wdatTimeStamp:
        begin
          timestamp := aPayload.bb_read_double(aCursor);
        end;
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          sensorid := aPayload.bb_read_guid(aCursor);
          if (fGUID <> (project as TSesmiProject).ExpertScenarioGUID) and (fGUID <> sensorid)  then //filter: only accept live events that match our id
           exit; //todo: mag dit, of moeten we de buffer leeg lezen? Wordt id altijd verstuurd voor de data?
        end;
      sensordata_no2, sensordata_pm10, sensordata_pm25:
        begin
          value := aPayload.bb_read_double(aCursor);
          if fLinkLayers.TryGetValue(fieldInfo, layer) then
            layer.AddValue(timestamp, value);
          if fTrackLayers.TryGetValue(fieldInfo, trackLayer) then
            trackLayer.AddValue(sensorid, value);
        end;
      sensordata_linkid:
        begin
          id := GuidToTWDID(aPayload.bb_read_guid(aCursor));
          TMonitor.Enter(fLinkLayers);
          try
            for layer in fLinkLayers.Values do
              layer.AddLinkID(timestamp, id);
          finally
            TMonitor.Exit(fLinkLayers);
          end;
        end;
      sensordata_latitude:
        begin
          value := aPayload.bb_read_double(aCursor);
          for trackLayer in fTrackLayers.Values do
            begin
              trackLayer.AddLat(sensorid, value);
            end;
        end;
      sensordata_longitude:
        begin
          value := aPayload.bb_read_double(aCursor);
          for trackLayer in fTrackLayers.Values do
            begin
              trackLayer.AddLon(sensorid, value);
            end;
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSesmiScenario.InquireDB(const aInquire: string; const aLowerTimestamp, aUpperTimestamp: Double);
var
  returnString: string;
  buffer: TByteBuffer;
begin
  if fQuerySubscribed then //unsubscribe from the previous returnEvent
  begin
    fqueryEvent.OnEvent.Remove(fQueryEventHandler);
    project.Connection.unSubscribe(fQueryEvent);
  end;
  //subscribe to the returnEvent
  returnString := ID + '-' + fQueryCounter.ToString();
  fQueryCounter := fQueryCounter+1;
  fQueryEvent := project.Connection.eventEntry(returnString, False).subscribe;
  fQueryEvent.OnEvent.Add(fQueryEventHandler);
  fQuerySubscribed := True;
  if (fProject is TSesmiProject) and (fGUID = (fProject as TSesmiProject).ExpertScenarioGUID) then
    buffer := TByteBuffer.bb_tag_guid(icehObjectID, fGUID.Empty) //expert scenario, send empty guid to access all data
  else if fGUID <> TGUID.Empty then //check if's not the empty guid
    buffer := TByteBuffer.bb_tag_guid(icehObjectID, fGUID)
  else //constant non-empty Guid, prevents people using the empty guid to access all data
    buffer := TByteBuffer.bb_tag_guid(icehObjectID, TGUID.Create('{00000000-0000-0000-0000-000000000001}'));
  buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampLower shr 3, aLowerTimeStamp);
  buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampUpper shr 3, aUpperTimeStamp);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, returnString);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aInquire);
  fPubEvent.signalEvent(buffer);
end;

procedure TSesmiScenario.ReadBasicData;
var
  palette, trackpalette: TWDPalette;
//  layer: TSesmiLinkLayer;
begin

  // Group exposure
  palette := CreateNiekPalette('NO2');//  CreateNO2Palette;
  addConcentrationLayer(
    'Group exposure', 'no2', 'NO2', 'Total exposure to NO2', false,
    sensordata_no2_total, 'receptordata', palette, BuildLegendJSON(palette));

  //palette := CreateNiekPalette('PM10'); // CreatePM10Palette;
  //addConcentrationLayer(
  //  'Group exposure', 'pm10', 'PM10', 'Total exposure to PM10', false,
  //  sensordata_pm10_total, 'receptordata', palette, BuildLegendJSON(palette));

  palette := CreateNiekPalette('NO2'); // CreateNO2Palette;
  addConcentrationLayer(
    'Group exposure',  'no2assim', 'NO2 assim', 'Total assimilated exposure to NO2', false,
    sensordata_assim_no2_total, 'receptordata', palette, BuildLegendJSON(palette));

  //palette := CreateNiekPalette('PM10'); // CreatePM10Palette;
  //addConcentrationLayer(
  //  'Group exposure', 'pm10assim', 'PM10 assim', 'Total assimilated exposure to PM10', false,
  //  sensordata_assim_pm10_total, 'receptordata', palette, BuildLegendJSON(palette));

  // Personal exposure
//  mobileChart :=  TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts', 'Mobile sensors', '', False, 'line',
//    TChartAxis.Create('minutes', 'lightBlue', 'Time', 'min'),
//    [TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3')]);
//
//  AddChart(mobileChart);

  palette := CreateHansPalette('NO2');
  trackpalette := CreateNiekPalette('Track NO2');
  AddSesmiLinkLayer(sensordata_no2, 'Personal exposure', 'NO2', 'NO2', 'Personal NO2', palette, trackpalette, BuildLegendJSON(palette));
  //palette := CreateHansPalette('PM10');
  //trackpalette := CreateNiekPalette('Track PM10');
  //AddSesmiLinkLayer(sensordata_pm10, 'Personal exposure', 'PM10', 'PM10', 'Personal PM10', palette, trackpalette, BuildLegendJSON(palette));
  //palette := CreateHansPalette('PM25');
  //trackpalette := CreateNiekPalette('Track PM25');
  //AddSesmiLinkLayer(sensordata_pm25, 'Personal exposure', 'PM25', 'PM25', 'Personal PM25', palette, trackpalette, BuildLegendJSON(palette));
  //layer := TSesmiLinkLayer.Create(Self, 'Personal exposure', fID + 'personal-no2', 'Personal NO2', 'NO2', False, True, palette, BuildLegendJSON(palette), mobilechart);
  //fLinkLayers.Add(sensordata_no2, layer);
  //AddLayer(layer);
  //layer.RegisterLayer;
end;

procedure TSesmiScenario.Reset;
var
  linkLayer: TSesmiLinkLayer;
  trackLayer: TSesmiTrackLayer;
begin
  TMonitor.Enter(fLinkLayers);
  try
    for linkLayer in fLinkLayers.Values do
      linkLayer.Reset;
  finally
    TMonitor.Exit(fLinkLayers);
  end;
  TMonitor.Enter(fTrackLayers);
  try
    for trackLayer in fTrackLayers.Values do
      trackLayer.Reset;
  finally
    TMonitor.Exit(fTrackLayers);
  end;
end;

{ TSesmiProject }

function TSesmiProject.addClient(const aClientID: string): TClient;
begin
  Result := TSesmiClient.Create(Self, fProjectCurrentScenario, fProjectRefScenario, aClientID);
  TMonitor.Enter(clients);
  try
    clients.Add(Result);
  finally
    TMonitor.Exit(clients);
  end;
end;

constructor TSesmiProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aAddBasicLayers: Boolean; const aDateFormData: string; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView; const aExpertScenarioGUID: TGUID);
begin
  mapView := aMapView;
  //fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  fSourceProjection := nil;
  fExpertScenarioGUID := aExpertScenarioGUID;
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN,
    aTilerStatusURL, nil, aAddBasicLayers, aMaxNearestObjectDistanceInMeters, mapView, nil, nil); // todo: check projectCurrentScenario
  fTiler.onTilerStatus := handleTilerStatus;

  //Set Sesmi controls
  SetControl(dateFormControl, '{"data":' + aDateFormData + '}');
  fLinks := TDictionary<TGUID, TWDGeometry>.Create;
  fPubEvent := aConnection.eventEntry('geometry_roads').publish;
  InquireNetwork;
  fWindData := TSesmiWindData.Create(Self);
end;

function TSesmiProject.CreateSesmiScenario(
  const aScenarioID: string): TSesmiScenario;
var
  guid: TGUID;
  scenario: TSesmiScenario;
//  palette: TWDPalette;
//  layer: TSesmiMobileSensorLayer;
begin
  scenario := TSesmiScenario.Create(Self, aScenarioID, 'Fietsproject', 'Persoonlijke fietsdata - ' + aScenarioID, False, MapView, False);
  TMonitor.Enter(fLinks);
  try
    for guid in fLinks.Keys
    do scenario.AddLink(guid, fLinks[guid]);
  finally
    TMonitor.Exit(fLinks);
  end;

  //AddSesmiLinkLayer(sensordata_no2, 'Personal exposure', 'NO2', 'NO2', 'Personal NO2', palette, BuildLegendJSON(palette));

  // todo: remove when ready tested; should show links but for testing show track also
  {
  palette := CreateNiekPalette('NO2');
  layer := TSesmiMobileSensorLayer.Create(
      scenario, 'sensor', 'mobilesensor', 'Mobile sensor', 'Mobile sensor',
      False, True, palette, BuildLegendJSON(palette), nil);
  scenario.AddLayer(layer);
  layer.RegisterLayer;

  scenarios.Add(aScenarioID, scenario);
  }
  scenario.GoLive(True);
  Result := scenario;
end;

destructor TSesmiProject.Destroy;
begin
  fWindData.Free;
  FreeAndNil(fLinks);
  //fComplaints.Free;
  inherited;
end;

function TSesmiProject.getMeasuresJSON: string;
begin
  Result := '{}';
end;

function TSesmiProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

procedure TSesmiProject.InquireNetwork;
var
  buffer: TByteBuffer;
  returnEventName: string;
begin
  returnEventName := TGuid.NewGuid.ToString + '.networkInquire';
  //returnEventName := 'Ensel2.' + 'geometry_roads'; //todo: dynamically read prefix!
  fNetworkEvent := fConnection.eventEntry(returnEventName, False).subscribe;
  fNetworkEvent.OnEvent.Add(HandleNetworkEvent);
  buffer := TByteBuffer.bb_tag_double(wDatTimeStampLower shr 3, -1.5);
  buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampUpper shr 3, -0.5);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, returnEventName);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, '');
  fPubEvent.signalEvent(buffer);
end;

procedure TSesmiProject.handleClientMessage(aClient: TClient;
  aScenario: TScenario; aJSONObject: TJSONObject);
  const
    parameterNames: array[0..4] of string = ('daysSelect', 'fromHour', 'toHour', 'fromDate', 'toDate');

  function ParseDays(const aParameterValue: string; out aDays: string) : Boolean;
  var
    selection: Boolean;
  begin
    Result := True;
    try
      selection := False;
      if aParameterValue.Contains('Zo') then
        aDays := aDays + '1'
      else
        selection := True;
      if aParameterValue.Contains('Ma') then
      begin
        if aDays <> '' then
          aDays := aDays + ', ';
        aDays := aDays + '2';
      end
      else
        selection := True;
      if aParameterValue.Contains('Di') then
      begin
        if aDays <> '' then
          aDays := aDays + ', ';
        aDays := aDays + '3';
      end
      else
        selection := True;
      if aParameterValue.Contains('Wo') then
      begin
        if aDays <> '' then
          aDays := aDays + ', ';
        aDays := aDays + '4';
      end
      else
        selection := True;
      if aParameterValue.Contains('Do') then
      begin
        if aDays <> '' then
          aDays := aDays + ', ';
        aDays := aDays + '5';
      end
      else
        selection := True;
      if aParameterValue.Contains('Vr') then
      begin
        if aDays <> '' then
          aDays := aDays + ', ';
        aDays := aDays + '6';
      end
      else
        selection := True;
      if aParameterValue.Contains('Za') then
      begin
        if aDays <> '' then
          aDays := aDays + ', ';
        aDays := aDays + '0';
      end
      else
        selection := True;
      if not selection then
        aDays := '';
    except
      aDays := '';
    end;
  end;

  function ParseHours(const aParameterValue: string; out aHours: Double) : Boolean;
  begin
    Result := True;
    try
      aHours := strtofloat(aParameterValue) / 24;//todo: set delimiter?
    except
      aHours := -1;
      Result := False;
    end;
  end;

  function ParseDate(const aParameterValue: string; out aDate: TDateTime) : Boolean;
  var
    day, month, year: string;
    stringSplit: TArray<string>;
    formatSettings: TFormatSettings;
  begin
    Result := True;
    try
      stringSplit := aParameterValue.Split(['-']);
      day := stringSplit[0];
      month := stringSplit[1];
      year := stringSplit[2];
      formatSettings := TFormatSettings.Create;
      formatSettings.ShortDateFormat := 'dd/mm/yyyy';
      formatSettings.DateSeparator := '/';
      aDate := StrToDate(day + '/' + month + '/' + year, formatSettings);
    except
      aDate := 0;
      Result := False;
    end;
  end;
var
  jsonValue: TJSONValue;
  //isp: TPair<string, TScenario>;

  // Date Form
  formatSettings: TFormatSettings;
  valid: Boolean;
  parameters: TJSONArray;
  parameter: TJSONValue;
  parameterName: string;
  parameterNameValue: string;
  parameterValue: string;
  parameterType: string;
  fromDate, toDate: TDateTime;
  fromHour, toHour: Double;
  daysSelect: string;
  queryString: string;
begin
  if assigned(aClient.currentScenario) and (aClient.currentScenario is TSesmiScenario) then
  begin
    if aJSONObject.TryGetValue<TJSONValue>('formResult', jsonValue) then
    begin
      if jsonValue.TryGetValue<TJSONArray>('parameters', parameters) then
      begin
        valid := True;
        for parameterName in parameterNames do
        begin
          if valid then
          begin
            for parameter in parameters do
            begin
              if parameter.TryGetValue<string>('name', parameterNameValue) then
              begin
                if not parameter.TryGetValue<string>('value', parameterValue)
                then parameterValue := '';
                if not parameter.TryGetValue<string>('type', parameterType)
                then parameterType := '';
                if parameterName = parameterNameValue then
                begin
                  if parameterName = 'daysSelect' then valid := ParseDays(parameterValue, daysSelect)
                  else if parameterName = 'fromHour' then valid := ParseHours(parameterValue, fromHour)
                  else if parameterName = 'toHour' then valid := ParseHours(parameterValue, toHour)
                  else if parameterName = 'fromDate' then valid := ParseDate(parameterValue, fromDate)
                  else if parameterName = 'toDate' then valid := ParseDate(parameterValue, toDate)
                  else; //unknown stuff
                end;
              end
              else
              valid := False;
            end;
          end;
        end;
        if valid then //build query;
        begin
          formatSettings := TFormatSettings.Create;
          formatSettings.DecimalSeparator := '.';
          queryString := '';
          if daysSelect.Length > 0 then
            queryString := queryString + '(mod(floor(ts)::Integer, 7) in (' + daysSelect + '))';
          if (fromHour <> toHour) then
          begin
            if queryString.Length > 0 then
            queryString := queryString + ' AND ';
            queryString := queryString + '(ts - floor(ts) > ' + fromHour.ToString(formatSettings);
            if fromHour < toHour then
              queryString := queryString + ' AND '
            else
              queryString := queryString + ' OR ';
            queryString := queryString + 'ts - floor(ts) < ' + toHour.ToString(formatSettings) + ')';
          end;
          (aClient.currentScenario as TSesmiScenario).GoDB(queryString, fromDate, toDate);
        end;
      end;
    end
    else if aJSONObject.TryGetValue<TJSONValue>('goLive', jsonValue) then
    begin
      (aClient.currentScenario as TSesmiScenario).GoLive;
    end;
  end;
end;

procedure TSesmiProject.handleNetworkEvent(aEventEntry: TEventEntry;
  const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  geometry: TWDGeometry;
  guid: TGUID;
  scenario: Tscenario;
  fieldInfo: UInt32;
  len: UInt64;
begin
  guid := TGUID.Empty;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
    (icehTilerGeometry shl 3) or wtLengthDelimited:
    begin
      geometry := TWDGeometry.Create;
      len := aPayload.bb_read_uint64(aCursor);
      geometry.Decode(aPayload, aCursor, aCursor + Integer(len));
      TMonitor.Enter(scenarios);
      try
      begin
        for scenario in scenarios.Values do
          if scenario is TSesmiScenario then
            (scenario as TSesmiScenario).AddLink(guid, geometry);
      end;
      finally
        TMonitor.Exit(scenarios);
      end;
      TMonitor.Enter(fLinks);
      try
        fLinks.Add(guid, geometry);
      finally
        TMonitor.Exit(fLinks);
      end;

    end;
    (icehObjectID shl 3) or wtLengthDelimited:
    begin
      guid := aPayload.bb_read_guid(aCursor);
    end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSesmiProject.handleNewClient(aClient: TClient);
var
  scenario: TScenario;
begin
  // send new sensors
  for scenario in fScenarios.Values do
  begin
    if scenario is TSesmiScenario then
    begin
//      if Assigned((scenario as TSesmiScenario).fSensorsLayer) then
//      begin
        //(scenario as TSesmiScenario).fSensorsLayer.fGUIDToID

//      end;
    end;
  end;
end;

procedure TSesmiProject.ReadBasicData;
var
  scenario: TScenario;
begin
  scenario := ReadScenario('basic'); //TSesmiScenario.Create(Self, '1', '1', '1', false, Self.mapView);
  scenarios.Add(scenario.id, scenario);
  fProjectCurrentScenario := scenario;
end;

procedure TSesmiProject.ReadObjects(aSender: TObject);
begin
  // todo:
end;

function TSesmiProject.ReadScenario(const aID: string): TScenario;
begin
  // todo: dependent on project? Utrecht <> Eindhven..
  Result := TSesmiScenario.Create(Self, aID, 'Eindhoven', 'Luchtkwaliteit in Eindhoven - Geen persoonlijke data', false, Self.mapView, False); // todo:
end;

{ TSesmiModule }

constructor TSesmiModule.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string;
  aMaxNearestObjectDistanceInMeters: Integer; const aExpertScenarioGUID: TGUID;
  const aProjectName: string; aMapView: TMapView);
var
  project: TProject;
  dateFormData: string;
begin
  inherited Create;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fTilerFQDN := aTilerFQDN;
  fTilerStatusURL := aTilerStatusURL;
  fProjects := TDictionary<string, TProject>.Create;
  fMaxNearestObjectDistanceInMeters := aMaxNearestObjectDistanceInMeters;
  dateFormData := '[{ "formElement": "checkbox", "type": "string", "required": "y", "optionsArray": ["Ma", "Di", "Wo", "Do", "Vr", "Za", "Zo"], "labelText": "Dagen van de week", "idName": "daysSelect", "extraOptions": false },'+
            '{"formElement":"slider", "type":"int", "required":"y", "optionsArray":["0", "24"], "labelText":"Vanaf (tijd):", "idName":"fromHour", "extraOptions":[1, "uur"]},'+
            '{"formElement": "slider", "type": "int", "required": "y", "optionsArray": ["0", "24"], "labelText": "Tot (tijd):", "idName": "toHour", "extraOptions": [1, "uur"]},'+
            '{"formElement": "input", "type": "string", "required": "y", "optionsArray": false, "labelText": "Van datum [dd-mm-jjjj]", "idName": "fromDate", "extraOptions": {"defaultValue": "13-10-2017"} },'+
            '{"formElement": "input", "type": "string", "required": "y", "optionsArray": false, "labelText": "Tot en met datum [dd-mm-jjjj]", "idName": "toDate", "extraOptions": {"defaultValue": "20-10-2017"}}'+
            ']';
  //InitPG;
  project := TSesmiProject.Create(aSessionModel, aConnection, 'Sesmi', aProjectName, aTilerFQDN, aTilerStatusURL,
    False, dateFormData, aMaxNearestObjectDistanceInMeters, aMapView, aExpertScenarioGUID);
  fProjects.Add(project.ProjectID, project);
end;

destructor TSesmiModule.Destroy;
begin
  // todo:
  FreeAndNil(fProjects);
  inherited;
end;

{ TSesmiMobileSensorLayer }

constructor TSesmiMobileSensorLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad,
  aShowInDomains: Boolean; aPalette: TWDPalette; aLegendJSON: string; aChart: TChartLines);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"mobilesensor"', 'Point', ltTile, aShowInDomains, 0);
  // mobile sensor points
  fChart := aChart;
  fPalette :=  aPalette;
  fLegendJSON := aLegendJSON;
  fDataEventHandler := HandleEvent;
  fDataEvent := scenario.project.connection.eventEntry('mobilesensordata').Subscribe;
  fDataEvent.OnEvent.Add(fDataEventHandler);
  fPrivateDataEvent := scenario.project.connection.eventEntry(scenario.project.Connection.privateEventName+'.'+'mobilesensordata', false).Subscribe;
  fPrivateDataEvent.OnEvent.Add(fDataEventHandler);
  // inquire
  fDataEvent.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateDataEvent.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, 'ORDER BY ts')); //'ts>=42700 ORDER BY ts'));
end;

destructor TSesmiMobileSensorLayer.Destroy;
begin
  if Assigned(fDataEvent) then
  begin
    if fDataEvent.OnEvent.Contains(fDataEventHandler)
    then fDataEvent.OnEvent.Remove(fDataEventHandler);
  end;
  inherited;
end;

function TSesmiMobileSensorLayer.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := True;
end;

function TSesmiMobileSensorLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := True;
end;

procedure TSesmiMobileSensorLayer.AddPoint(aObjectID: TGUID; aTimeStamp, aLat, aLon: Double; aSubstance: UInt32; aValue: Double);
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
  if Assigned(fChart)
  then fChart.AddValue(aTimeStamp, [aValue]);
//  end
//  else
//  begin
//    o
//  end;
end;

procedure TSesmiMobileSensorLayer.HandleEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
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
          if ((fScenario.project is TSesmiProject) and (fScenario is TSesmiScenario) and ((fScenario.project as TSesmiProject).ExpertScenarioGUID = (fScenario as TSesmiScenario).fGUID)) or (objectID.ToString = fScenario.ID) then //check if we need to add this point
          begin
            if (fLastLat<>0) and (fLastLon<>0) and (timestamp > Now-1)
            then AddPoint(objectID, timestamp, fLastLat, fLastLon, sensordata_no2, no2);
            // todo: ignoring values on 0,0 for now
          end;
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

procedure TSesmiMobileSensorLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500, fPalette);
  fTilerLayer.signalSliceAction();
end;

procedure TSesmiMobileSensorLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice(nil);
end;

function TSesmiMobileSensorLayer.SliceType: Integer;
begin
  Result := stLocation;
end;

{ TSesmiClient }

constructor TSesmiClient.Create(aProject: TProject; aCurrentScenario,
  aRefScenario: TScenario; const aClientID: string);
begin
  inherited;
end;

procedure TSesmiClient.Login(aJSONObject: TJSONObject);
var
  scenarioID: string;
  userID: string;
  scenario: TScenario;
begin
  scenarioID := aJSONObject.GetValue<string>('scenario'); //todo: check if scenarioID is valid GUID?
  if not TRegEx.IsMatch(scenarioID, '^[{][0-9A-Fa-f]{8}[-]([0-9A-Fa-f]{4}[-]){3}[0-9A-Fa-f]{12}[}]$') then
    exit;
  userID := aJSONObject.GetValue<string>('userid');
  if not fProject.scenarios.TryGetValue(scenarioID, scenario) then
  begin
    scenario := (fProject as TSesmiProject).CreateSesmiScenario(scenarioID);
  end;
  removeClient(fCurrentScenario);
  fCurrentScenario := scenario;
  addClient(fCurrentScenario);
  Log.WriteLn('connected to scenario '+scenarioID+' user '+userid);
  // retry
  SendSession();
  //SendMeasures(); // todo:?
  fProject.SendDomains(self, 'domains');
end;

{ TSesmiLinkLayer }

procedure TSesmiLinkLayer.AddLink(const aID: TWDID; aGeometry: TWDGeometry);
begin
  if not objects.ContainsKey(aID) then
    AddObject(TSesmiLink.Create(Self, aID, aGeometry));
end;

procedure TSesmiLinkLayer.AddLinkID(const aTimeStamp: Double; const aID: TWDID);
var
  value: Double;
begin
  //check if we can make a match
  if valueList.ContainsKey(aTimeStamp) then //match
  begin
    value := valueList[aTimeStamp];
    valueList.Remove(aTimeStamp);
    ProcessMatch(aTimeStamp, value, aID);
  end
  else //add to list so we can match later
  begin
    linkidList.AddOrSetValue(aTimeStamp, aID);
  end;
end;

procedure TSesmiLinkLayer.AddValue(const aTimeStamp, aValue: Double);
var
  linkID: TWDID;
//  lastValue: TChartValue;
  average, delta: Double;
//  total: Double;
begin
  //check if we can make a match
  if linkidList.ContainsKey(aTimeStamp) then //match
  begin
    linkID := linkidList[aTimeStamp];
    linkidList.Remove(aTimeStamp);
    ProcessMatch(aTimeStamp, aValue, linkID);
  end
  else //add to list so we can match later
  begin
    valueList.AddOrSetValue(aTimeStamp, aValue);
  end;

  //always add value to charts
  TMonitor.Enter(fChart);
  try
//    if fChart.values.Count > 0 then
//    begin
//      lastValue := fChart.values[fChart.values.Count - 1];
//      average := (aValue + lastValue.y[0]) / 2;
//      delta := aTimeStamp - lastValue.x;
//      total := average * delta * 24 * 60;
//      if fTotalChart.values.Count > 0 then
//        total := total + fTotalChart.values[fTotalChart.values.Count -1].y[0];
//      fTotalChart.AddValue(aTimeStamp, [total]);
//    end;
    if not Double.IsNaN(fPrevValue) then
    begin
      average := (aValue + fPrevValue) / 2;
      delta := aTimestamp - fPrevTime;
      fTotalValue := fTotalValue + (average * delta * 24 * 60);
      fTotalChart.AddValue(aTimeStamp, [fTotalValue]);
    end;
    fPrevValue := aValue;
    fPrevTime := aTimeStamp;
    fChart.AddValue(aTimeStamp, [aValue]);
  finally
    TMonitor.Exit(fChart);
  end;
end;

constructor TSesmiLinkLayer.Create(aScenario: TScenario; const aDomain, aID,
  aName, aDescription: string; aDefaultLoad, aShowInDomains: Boolean;
  aPalette: TWDPalette; aLegendJSON: string; aChart, aTotalChart: TChartLines);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"Link"', 'LineString', ltTile, aShowInDomains, 0);
  valueList := TDictionary<Double, Double>.Create;
  linkidList := TDictionary<Double, TWDID>.Create;
  fPalette := aPalette;
  fLegendJSON := aLegendJSON;
  fChart := aChart;
  fTotalChart := aTotalChart;
  fTotalValue := 0;
  fPrevTime := Double.NaN;
  fPrevValue := Double.NaN;
end;

destructor TSesmiLinkLayer.Destroy;
begin
  inherited;
end;

procedure TSesmiLinkLayer.ProcessMatch(const aTimeStamp, aValue: Double;
  aID: TWDID);
var
  link: TLayerObject;
begin
  TMonitor.Enter(objects);
  try
    if objects.TryGetValue(aID, link) then
    begin
      (link as TSesmiLink).UpdateValue(aTimeStamp, aValue);
    end;
  finally
    TMonitor.Exit(objects);
  end;
end;

procedure TSesmiLinkLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500);
end;

procedure TSesmiLinkLayer.RegisterSlice;
begin
  if Assigned(fPalette)
  then tilerLayer.signalAddSlice(fPalette.Clone)
  else tilerLayer.signalAddSlice(nil);
end;

procedure TSesmiLinkLayer.Reset;
var
  link: TLayerObject;
begin
  TMonitor.Enter(objects);
  try
  for link in objects.Values do
    (link as TSesmiLink).Reset;
  finally
    TMonitor.Exit(objects);
  end;
  TMonitor.Enter(fChart);
  try
    fChart.reset;
    fTotalChart.reset;
  finally
    TMonitor.Exit(fChart);
  end;
end;

function TSesmiLinkLayer.SliceType: Integer;
begin
  Result := stGeometryI;
end;

{ TSesmiLink }

constructor TSesmiLink.Create(aLayer: TLayer;
  const aID: TWDID; aGeometry: TWDGeometry);
begin
  inherited Create(aLayer, aID, aGeometry, Double.NaN);
  fValueList := TList<TTimedValue>.Create;
end;

destructor TSesmiLink.Destroy;
begin
  inherited;
end;

procedure TSesmiLink.Reset;
begin
  if fValue <> Double.NaN then
  begin
    fValue := Double.NaN;
    fTotalValue := 0;
    fValueList.Clear;
    layer.signalObject(Self);
  end;
end;

procedure TSesmiLink.UpdateValue(aTimeStamp, aValue: Double);
var
  timedValue: TTimedValue;
  i: Integer;
begin
    //check if we need to remove values that are too old
    if (layer.scenario is TSesmiScenario) and (layer.scenario as TSesmiScenario).Live and (fValueList.Count > 0) then
    for i := fValueList.Count - 1 to 0  do
        if fValueList[i].time < (aTimeStamp - GetSetting(TimeSpanSwitch, DefaultTimeSpan)) then
        begin
          fTotalValue := fTotalValue - fValueList[i].value;
          fValueList.Delete(i);
        end;

    //add new value
    timedValue.time := aTimeStamp;
    timedValue.value := aValue;
    fValueList.Add(timedValue);
    fTotalValue := fTotalValue + aValue;

    if fValue <> (fTotalValue / fValueList.Count) then
    begin
      fValue := fTotalValue / fValueList.Count;
      layer.signalObject(Self);
    end;
end;

{ TSesmiTrackLayer }

procedure TSesmiTrackLayer.AddLat(aSensorId: TGUID; aLat: Double);
begin
  fLastLats.AddOrSetValue(aSensorId, aLat);
end;

procedure TSesmiTrackLayer.AddLon(aSensorId: TGUID; aLon: Double);
begin
  fLastLons.AddOrSetValue(aSensorId, aLon);
end;

procedure TSesmiTrackLayer.AddPoint(aObjectID: TGUID; aLat, aLon,
  aValue: Double);
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
end;

procedure TSesmiTrackLayer.AddValue(aSensorId: TGUID; aValue: Double);
var
  lastLon, lastLat: Double;
begin
  if fLastLons.TryGetValue(aSensorId, lastLon) and fLastLats.TryGetValue(aSensorId, lastLat) then
    AddPoint(aSensorId, lastLat, lastLon, aValue);
end;

constructor TSesmiTrackLayer.Create(aScenario: TScenario; const aDomain, aID,
  aName, aDescription: string; aDefaultLoad, aShowInDomains: Boolean;
  aPallette: TWDPalette; aLegendJSON: string);
begin
  fLastLats := TDictionary<TGUID, Double>.Create;
  fLastLons := TDictionary<TGUID, Double>.Create;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, '"mobilesensor"', 'Point', ltTile, aShowInDomains, 0);
  fPalette := aPallette;
  fLegendJSON := aLegendJSON;
end;

destructor TSesmiTrackLayer.Destroy;
begin
  FreeAndNil(fLastLons);
  FreeAndNil(fLastLats);
  inherited;
end;

procedure TSesmiTrackLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500, fPalette);
end;

procedure TSesmiTrackLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice(nil);
end;

procedure TSesmiTrackLayer.Reset;
begin
  tilerLayer.signalSliceAction();
end;

function TSesmiTrackLayer.SliceType: Integer;
begin
  Result := stLocation;
end;
end.
