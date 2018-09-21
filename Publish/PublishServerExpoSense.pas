unit PublishServerExpoSense;

interface

uses
  Logger,

  imb4,

  StdIni,

  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,
  WorldOrderedList,

  TimerPool,
  SensorDataSets,

  TilerControl,

  PublishServerLib,
  PublishServerGIS,

  // US
  PublishServerOra,
  PublishServerUS,
  Ora,
  MyOraLib,

  CommandQueue,

  imb3NativeClient,

  GisCsSystems,

  System.Math,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils,
  System.RegularExpressions;

const
  sensordata_longitude             = 129;               //tag 16
  sensordata_latitude              = 121;               //tag 15

  calculated_latitude	             = 481;               //tag 60
  calculated_longitude             = 489;               //tag 61
  calculated_height	               = 497;               //tag 62

  //sensordata_pm10                  = 1441;              //tag 180
  //sensordata_pm25                  = 1601;              //tag 200
  //sensordata_no2                   = 1921;              //tag 240
  //sensordata_no2                   = 961;               //tag 120 -> temporary tag!
  //sensordata_pm1                   = 2081;              //tag 260
  //sensordata_nh3                   = 2241;              //tag 280
  //sensordata_pnc                   = 2401;              //tag 300
  //sensordata_nox                   = 2561;              //tag 320
  sensordata_ec                    = 4321;              // tag 540
  sensordata_linkid                = 2882;              //tag 360

  sensordata_mode_of_transportation= 2722;              // tag 340
  sensordata_location_type         = 3042;              // tag 380

  //sensordata_pm10_total            = 1449;              //tag 181
  //sensordata_pm25_total            = 1609;              //tag 201
  //sensordata_no2_total             = 969;               //tag 241
  //sensordata_pm1_total             = 2089;              //tag 261
  //sensordata_assim_nox             = 2577;              //tag 322
  //sensordata_assim_pm10            = 1457;              //tag 182
  //sensordata_assim_pm25            = 1617;              //tag 202
  //sensordata_assim_no2             = 977;               //tag 242
  //sensordata_assim_pm1             = 2097;              //tag 262
  //sensordata_assim_nh3             = 2257;              //tag 282
  //sensordata_assim_pnc             = 2417;              //tag 302
  //sensordata_assim_pm10_total      = 1465;              //tag 183
  //sensordata_assim_pm25_total      = 1625;              //tag 203
  //sensordata_assim_no2_total       = 985;               //tag 243
  //sensordata_assim_pm1_total       = 2105;              //tag 263
  sensordata_assim_ec_total        = sensordata_ec+24;    // ((540+3) << 3)+1 // tag 540+3

  meteodata_windspeed              = 401;
  meteodata_winddirection          = 409;

  kpi_road_segment_id              = 2880;              // tag 360
  kpi_TransportMode                = 3042;              //tag 380
  kpi_ComponentName                = 3202;              //tag 400
  kpi_Time_Category                = 3362;              //tag 420
  kpi_avg_concentration            = 3521;              //tag 440
  kpi_duration                     = 3681;              //tag 460

  TimeSpanSwitch = 'timespan';
  DefaultTimeSpan = 7;

  MaxNoSensorValueTime = 5.0/(60.0*24.0); // 5 minutes

  DefaultTimeSliderUpdateTime = 5; // seconds
    MaxTimeSliderUpdateTime = 10; // seconds


  DefaultExpoSensoECValueToHeightFactor = 1/50;

  chartSensorValueFactor = 1.0e-9;
  chartCalculatedValueFactor = 1.0e-6; //1.0e-3; // todo: explain diff in factor compared to sensor factor

const
  motUnknown = 0;
  motsUnknown = 'Unknown';
  motcUnknown = $ffB0E0E6; //c0c0c0;

  motStationary = 1;
  motsStationary = 'Stationary';
  motcStationary = $fff8f8f8;

  motWalking = 2;
  motsWalking = 'Walking';
  motcWalking = $ff90EE90;

  motCycling = 3;
  motsCycling = 'Cycling';
  motcCycling = $ff1E90FF; // 00BFFF; //9370db;

  motMotorized = 4;
  motsMotorized = 'Motorized';
  motcMotorized = $ffFF00FF; // DA70D6; //b22222;

  motFlying = 5;
  motsFlying = 'Flying';
  motcFlying = $ffff0000;


  motStrings: array[0..5] of string = // tag 340, mode of transportation
    (motsUnknown, motsStationary, motsWalking, motsCycling, motsMotorized, motsFlying);

  motColors: array[0..5] of Cardinal =
    (motcUnknown, motcStationary, motcWalking, motcCycling, motcMotorized, motcFlying);

  locUnknown = 0;
  locsUnknown = 'Unknown';
  locUnderway = 1;
  locsUnderway = 'Underway';
  locAtHome = 2;
  locsAtHome = 'At home';
  locNotAtHome = 3;
  locsNotAtHome = 'Not at home';
  locOutdoors = 4;
  locsOutdoors = 'Outdoors';

  LTs: array[0..4] of string = // tag 380, location type
    (locsUnknown, locsUnderway, locsAtHome, locsNotAtHome, locsOutdoors);

  ModalityPaletteJSON : string =
    '{"entries":['+
      '{"color":{"fill":"#90EE90"}, "minValue":1, "description":"'+motsWalking+'"},'+
      '{"color":{"fill":"#1E90FF"}, "minValue":2, "description":"'+motsCycling+'"},'+
      '{"color":{"fill":"#FF00FF"}, "minValue":3, "description":"'+motsMotorized+'"},'+
      '{"color":{"fill":"#f8f8f8"}, "minValue":4, "description":"'+'Indoors'{motsStationary}+'"},'+ // very light gray
      '{"color":{"fill":"#B0E0E6"}, "minValue":0, "description":"'+motsUnknown+'"}'+ // gray
    ']}';




type
  {
  TExpoSenseClient = class(TClient)
  constructor Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
  protected
    procedure Login(aJSONObject: TJSONObject); override;
  end;
  }
  TExpoSenseDataPoint = class(TOrderedListTSEntry)
  constructor Create(aTimeStamp: TDateTime; aLat, aLon: Double; aValue: Double);
  destructor Destroy; override;
  private
    fGeometry: TWDGeometryPoint;
    fValue: Double;
    fCalculatedGeometry: TWDGeometryPoint;
    fModeOfTransportation: Integer;
    fLocation: Integer;
  public
    property geometry: TWDGeometryPoint read fGeometry;
    property value: Double read fValue;
    property calculatedGeometry: TWDGeometryPoint read fCalculatedGeometry write fCalculatedGeometry;
    property modeOfTransportation: Integer read fModeOfTransportation write fModeOfTransportation;
    property location: Integer read fLocation write fLocation;

    function id(const aSensorID: TGUID): string;

    class function ModeOfTransportationStrToInt(aMOT: string): Integer;
    class function LocationStrToInt(aLocation: string): integer;

  end;

  TExpoSenseHeightTrackLayer = class(TLayerBase)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aValueHeightFactor: Double;
    aDefaultLoad: Boolean=False; aShowInDomains: Boolean=True);
  destructor Destroy; override;
  protected
    fValueHeightFactor: Double;
    fTilerLayer: TTilerLayer;
    // timers
    fPreviewRequestTimer: TTimer;
    fSendRefreshTimer: TTImer;
    function getJSON: string; override;
  public
    procedure Update(aStart, aEnd: TDateTime);
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aValue: Double; aMOT: Integer);
    procedure UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
    procedure Reset();
    procedure RegisterLayer; override;
    procedure handleRefreshTrigger(aTimeStamp: TDateTime);
    function uniqueObjectsTilesLink: string;

    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  end;

  TExpoSenseDotTrackLayer = class(TLayerBase)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean=False; aShowInDomains: Boolean=True);
  destructor Destroy; override;
  protected
    fTilerLayer: TTilerLayer;
    // timers
    fPreviewRequestTimer: TTimer;
    fSendRefreshTimer: TTImer;
    function getJSON: string; override;
  public
    procedure Update(aStart, aEnd: TDateTime);
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aMOT: Integer);
    procedure UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
    procedure reset();
    procedure RegisterLayer; override;
    procedure handleRefreshTrigger(aTimeStamp: TDateTime);
    function uniqueObjectsTilesLink: string;

    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  end;

  TExpoSenseLineTrackLayer = class(TLayerBase)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean=False; aShowInDomains: Boolean=True);
  destructor Destroy; override;
  protected
    fTilerLayer: TTilerLayer;
    // timers
    fPreviewRequestTimer: TTimer;
    fSendRefreshTimer: TTImer;
    function getJSON: string; override;
  public
    procedure Update(aStart, aEnd: TDateTime);
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aPrevLat, aPrevLon: Double; aMOT: Integer);
    procedure UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
    procedure reset();
    procedure RegisterLayer; override;
    procedure handleRefreshTrigger(aTimeStamp: TDateTime);
    function uniqueObjectsTilesLink: string;

    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  end;

  TExpoSenseScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
  destructor Destroy; override;
  private
    // tiler track layers
//    fHeightTrackLayer: TExpoSenseHeightTrackLayer;
//    fDotTrackLayer: TExpoSenseDotTrackLayer;
//    fLineTrackLayer: TExpoSenseLineTrackLayer ;
    // simple, combined track layer
    fCombinedTrackLayer: TSimpleLayer;
//    fLastLats: TDictionary<TGUID, Double>;
//    fLastLons: TDictionary<TGUID, Double>;
//    fPrevLats: TDictionary<TGUID, Double>;
//    fPrevLons: TDictionary<TGUID, Double>;
//    fLastCalcLats: TDictionary<TGUID, Double>;
//    fLastCalcLons: TDictionary<TGUID, Double>;
    //fSensorsDataSet: TSensorsDataSet;
    fTimeSliderDataTimer: TTimer;
    fFirstTimeSliderUpdate: Boolean;
    fMeasuredECValuesChart: TChartLines;
    fCalculatedECValuesChart: TChartLines;
    //fTotalChart: TChartLines;
    fFiltered: Boolean;
    fShowDataSelectionTimer: TTimer;
    fDataPoints: TObjectDictionary<TGUID, TOrderedListTS<TExpoSenseDataPoint>>;
    fIMB3Connection: TIMBConnection; //owns
    procedure triggerUpdateTimesliderData;
    function jsonTimesliderData(aPalette: TWDPalette; var aExtent: TWDExtent): string;
  protected
    fPubEvent: TEventEntry;
    fLiveEvent: TEventEntry;
    fLiveEventHandler: TOnEvent;
    fLiveCounter: Integer;
    fDBCounter: Integer;
    fLastClient: TDateTime;
    //fValueStored: TObjectDictionary<Integer, TDictionary<TGUID, Double>>;

    procedure handleLiveEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleUniEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
  protected
//    function GetValueStored(aTag: Integer; const aSensorID: TGUID): Double;
//    procedure setValueStored(aTag: Integer; const aSensorID: TGUID; aValue: Double);
//    property ValueStored[aTag: Integer; const aSensorID: TGUID]: Double read GetValueStored write setValueStored;
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
    procedure HandleTimeSliderEvent(aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure HandleScenarioRefresh(aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aValue: Double);
    procedure UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aMOT: Integer);
    procedure UpdateLocation(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aLocation: Integer);
//    procedure Update(aStart, aEnd: TDateTime; aMode: Integer);
//    procedure CombinedTrackAddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLastLat, aLastLon, aPrevLat, aPrevLon: Double; aMOT: Integer);
//    procedure CombinedTrackUpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
//    procedure CombinedTrackUpdateLocation(const aSensorID: TGUID; aTimeStamp: TDateTime; aLocation: Integer);
  public
    procedure ReadBasicData(); override;
  end;

  TExpoSenseProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
  protected
    function handleTilerStatus(aTiler: TTiler): string;
  public
    function CreateExpoSenseScenario(const aScenarioID: string{; aGUID: TGUID; aLowerTimeStamp, aUpperTimeStamp: Double}): TExpoSenseScenario;
    procedure ReadBasicData(); override;
  end;

implementation

//convert a Guid to TWDID
function GuidToTWDID(aGuid: TGUID): TWDID;
begin
  SetLength(Result, SizeOf(aGuid));
  Move(aGuid, PAnsiChar(Result)^, SizeOf(aGuid));
end;

function CreateBlueTrackPalette(const aTitle: string): TWDPalette;
const
  LowColor = $FF9BFFFF;
  HighColor = $FF2D38FF;
var
  factor: Double;

begin
  factor := 1 / 1000000000;
  Result := TRampPalette.Create(aTitle, [
//    TRampPaletteEntry.Create($FFE0E0FF, 0 * factor, '0'), 9BFFFF
//    TRampPaletteEntry.Create($FFA8A8FF, 30 * factor, '30'),
//    TRampPaletteEntry.Create($FF7070FF, 60 * factor, '60'),
//    TRampPaletteEntry.Create($FF3838FF, 90 * factor, '90'),
//    TRampPaletteEntry.Create($FF0000FF, 120 * factor, '120')], FF2D38FF

    TRampPaletteEntry.Create(LowColor, 0 * factor, '0'),
    TRampPaletteEntry.Create(ColorRamp(30, 0, 120, LowColor, HighColor), 30 * factor, '30'),
    TRampPaletteEntry.Create(ColorRamp(60, 0, 120, LowColor, HighColor), 60 * factor, '60'),
    TRampPaletteEntry.Create(ColorRamp(90, 0, 120, LowColor, HighColor), 90 * factor, '90'),
    TRampPaletteEntry.Create(HighColor, 120 * factor, '120')],
      LowColor,
      $00000000,
      HighColor);
end;

function CreateGraySliderPalette(const aTitle: string): TWDPalette;
var
  factor: Double;
const
  step1 = 1000;
begin
  factor := 1 / 1000000000;
  Result := TDiscretePalette.Create(aTitle, [
    TDiscretePaletteEntry.Create(TGeoColors.Create($FFDDDDDD), Double.NegativeInfinity, step1 * factor, '0 - '+step1.ToString),
    TDiscretePaletteEntry.Create(TGeoColors.Create($FF000000), step1 * factor, Double.PositiveInfinity, step1.toString+'+')],
//    TDiscretePaletteEntry.Create(TGeoColors.Create($FFDDDDDD), 0 * factor, 30 * factor, '0 - 40'),
//    TDiscretePaletteEntry.Create(TGeoColors.Create($FF888888), 40 * factor, 80 * factor, '40 - 80'),
//    TDiscretePaletteEntry.Create(TGeoColors.Create($FF000000), 80 * factor, Double.PositiveInfinity, '80+')],
      TGeoColors.Create(0)); // black transparent
end;

{ TExpoSenseDataPoint }

constructor TExpoSenseDataPoint.Create(aTimeStamp: TDateTime; aLat, aLon, aValue: Double);
begin
  inherited Create(aTimeStamp);
  fGeometry := TWDGeometryPoint.Create(aLon, aLat, double.NaN);
  fValue := aValue;
  fCalculatedGeometry := nil;
  fModeOfTransportation := motUnknown;
  fLocation := locUnknown;
end;

destructor TExpoSenseDataPoint.Destroy;
begin
  FreeAndNil(fGeometry);
  FreeAndNil(fCalculatedGeometry);
  inherited;
end;

function TExpoSenseDataPoint.id(const aSensorID: TGUID): string;
begin
  Result := aSensorID.ToString+FormatDateTime('yyyymmdd-hhnnss', timeStamp);
end;

class function TExpoSenseDataPoint.LocationStrToInt(aLocation: string): integer;
begin
  aLocation := aLocation.toLower;
  Result := length(LTs)-1;
  while (Result>0) and (aLocation<>LTS[Result].toLower)
  do Result := Result-1;
end;

class function TExpoSenseDataPoint.ModeOfTransportationStrToInt(aMOT: string): Integer;
begin
  aMOT := aMOT.ToLower;
  Result := length(motStrings)-1;
  while (Result>0) and (aMOT<>motStrings[Result].toLower)
  do Result := Result-1;
  {
  if aMot.ToLower=motsWalking.ToLower
  then Result := motWalking
  else if aMot.ToLower=motsCycling.ToLower
  then Result := motCycling
  else if aMot.ToLower= motsMotorized.ToLower
  then Result := motMotorized
  else Result := motOther;
  }
end;

{ TExpoSenseTrackLayer }

procedure TExpoSenseHeightTrackLayer.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aValue: Double; aMOT: Integer);
var
  geometryPoint: TWDGeometryPoint;
begin
  geometryPoint := TWDGeometryPoint.Create(aLon, aLat, Double.NaN);
  try
    fTilerLayer.signalData(
      TByteBuffer.bb_tag_guid(icehObjectID, aSensorID)+
      TByteBuffer.bb_tag_rawbytestring(icehTilerGeometryPoint, geometryPoint.Encode)+
      TByteBuffer.bb_tag_double(icehTilerValue, aMOT)+
      TByteBuffer.bb_tag_double(icehTilerValue2, aValue*Self.fValueHeightFactor)+
      TByteBuffer.bb_tag_double(icehObjectTS, aTimeStamp));
  finally
    geometryPoint.Free;
  end;
end;

constructor TExpoSenseHeightTrackLayer.Create(aScenario: TScenario;
  const aDomain, aID, aName, aDescription: string;
  aValueHeightFactor: Double;
  aDefaultLoad, aShowInDomains: Boolean);
begin
  fValueHeightFactor := aValueHeightFactor;
  inherited Create(
    aScenario, aDomain, aID, aName, aDescription,
    aDefaultLoad, 'mobilesensor', aShowInDomains, False, 0.8, '', '');
  fPreviewRequestTimer := scenario.project.Timers.SetTimer(
    procedure (aTimer: TTImer; aTime: THighResTicks)
    begin
      if Assigned(fTilerLayer) then
      begin
        Log.WriteLn('triggered preview timer for '+elementID);
        fTilerLayer.signalRequestPreview;
      end
      else Log.WriteLn('## triggered preview timer for '+elementID+' without having a tiler layer', llError);
    end);
  fSendRefreshTimer := scenario.project.Timers.CreateInactiveTimer;
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*3); // todo: parameterize
  //fLayerUpdateTimer := scenario.project.Timers.CreateInactiveTimer;
  //fLayerUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond/aLayerUpdateFrequency);
  fTilerLayer := nil;
  RegisterLayer;
end;

destructor TExpoSenseHeightTrackLayer.Destroy;
begin
  FreeAndNil(fTilerLayer);
  inherited;
end;

function TExpoSenseHeightTrackLayer.getJSON: string;
begin
  Result := inherited getJSON;
  if Assigned(fTilerLayer) and (fTilerLayer.URL<>'')
  then jsonAddString(Result, 'tiles', fTilerLayer.URLTimeStamped)
  else jsonAddString(Result, 'tiles', '', True);
  jsonAddString(Result, 'type', ltTile);
end;

function TExpoSenseHeightTrackLayer.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
  if Assigned(fTilerLayer) and (fTilerLayer.URL <> '') then
    aClient.SendRefresh(elementID, '', fTilerLayer.URLTimeStamped);

  legendJSON := Self.legendJSON;
  if legendJSON <> '' then
    aClient.SendLegend(elementID, '', Self.legendJSON);
end;

function TExpoSenseHeightTrackLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
end;

procedure TExpoSenseHeightTrackLayer.handleRefreshTrigger(aTimeStamp: TDateTime);
var
  timeStampStr: string;
  tiles: string;
begin
  try
    if aTimeStamp<>0
    then timeStampStr := FormatDateTime(publisherDateTimeFormat, aTimeStamp)
    else timeStampStr := '';
    tiles := uniqueObjectsTilesLink;

    Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+' ('+timeStampStr+'): '+tiles);

    // signal refresh to layer client
    forEachSubscriber<TClient>(
      procedure(aClient: TClient)
      begin
        Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+', direct subscribed client: '+aClient.clientID, llNormal, 1);
        aClient.SendRefresh(elementID, timeStampStr, tiles);
      end);
    scenario.LayerRefreshed(fTilerLayer.ID, elementID, aTimeStamp, Self);
  except
    on E: Exception
    do Log.WriteLn('Exception in TLayer.handleRefreshTrigger: '+e.Message, llError);
  end;
end;

procedure TExpoSenseHeightTrackLayer.RegisterLayer;
var
  paletteJSONObject: TJSONObject;
  modalityPalette: TWDPalette;
  _legendJSON: string;
  //start: TDateTime;
begin
  paletteJSONObject := TJSONObject.ParseJSONValue(ModalityPaletteJSON) as TJSONObject;
  try
    if Assigned(paletteJSONObject)
    then modalityPalette := JSON2PaletteAndLegend(paletteJSONObject, _legendJSON)
    else modalityPalette := CreateBlueTrackPalette('Modality');
  finally
    paletteJSONObject.Free;
  end;
  legendJSON := _legendJSON;
  fTilerLayer.Free;
  fTilerLayer := TTilerLayer.Create(
    scenario.project.Connection, elementID, stGeometryIH,
    // tiler info
    procedure (aTilerLayer: TTilerLayer)
    begin
      aTilerLayer.signalAddSlice();
      aTilerLayer.signalSliceAction(tsaClearSlice);
    end,
    // refresh
    procedure(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime; aImmediate: Boolean)
    begin
      // todo: implement
      try
        if aImmediate  then
        begin
          fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*ImmediateTilerRefreshTime),
            procedure(aTimer: TTimer; aTime: THighResTicks)
            begin
              handleRefreshTrigger(aTimeStamp);
            end);
        end
        else
        begin
          fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*1),
            procedure(aTimer: TTimer; aTime: THighResTicks)
            begin
              handleRefreshTrigger(aTimeStamp);
            end);
        end;
        // refresh preview also
        fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*30);
      except
        on E: Exception
        do Log.WriteLn('Exception in TLayer.handleTilerRefresh ('+elementID+'): '+E.Message, llError);
      end;
    end,
    // preview
    procedure (aTilerLayer: TTilerLayer)
    begin
      fPreviewBase64 := aTilerLayer.previewAsBASE64;
      // layer clients
      forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          aClient.SendPreview(elementID, fPreviewBase64);
        end);
      Log.WriteLn('send normal preview on '+elementID);
    end,
    modalityPalette);//, -1, '' .addLayer(elementID, aSliceType, aPalette);
  fPreviewBase64 := fTilerLayer.previewAsBASE64;
  // trigger registration
  fTilerLayer.signalRegisterLayer(scenario.project.tiler, fDescription, False, 250);
  {
  Log.WriteLn('Waiting on tiler for layer '+elementID);
  start := Now;
  while (fTilerLayer.URL='') and (Now-start<dtOneSecond*15)
  do Sleep(1000);
  if fTilerLayer.URL=''
  then Log.WriteLn('Timeout on layer registration for '+elementID, llWarning);
  }
end;

procedure TExpoSenseHeightTrackLayer.Reset;
begin
  fTilerLayer.signalSliceAction(tsaClearSlice);
end;

function TExpoSenseHeightTrackLayer.uniqueObjectsTilesLink: string;
begin
  if Assigned(fTilerLayer)
  then Result := fTilerLayer.URLTimeStamped
  else Result := '';
end;

procedure TExpoSenseHeightTrackLayer.Update(aStart, aEnd: TDateTime);
begin
  // todo: implement
end;

procedure TExpoSenseHeightTrackLayer.UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
begin
  // todo: implement
end;

{ TExponsenseDotTrackLayer }

procedure TExpoSenseDotTrackLayer.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aMOT: Integer);
var
  geometryPoint: TWDGeometryPoint;
  id: TWDID;
begin
  geometryPoint := TWDGeometryPoint.Create(aLon, aLat, Double.NaN);
  try
    id := TByteBuffer.bb_bytes(aSensorID, SizeOf(aSensorID))+TByteBuffer.bb_bytes(aTimeStamp, SizeOf(aTimeStamp));
    fTilerLayer.signalData(
      TByteBuffer.bb_tag_rawbytestring(icehTilerGeometryPoint, geometryPoint.Encode)+
      TByteBuffer.bb_tag_double(icehTilerValue, aMOT)+
      TByteBuffer.bb_tag_bytes(icehObjectID, PAnsiChar(id)^, length(id)));
      // todo: radius?
  finally
    geometryPoint.Free;
  end;
end;

constructor TExpoSenseDotTrackLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad,
  aShowInDomains: Boolean);
begin
  inherited Create(
    aScenario, aDomain, aID, aName, aDescription,
    aDefaultLoad, 'mobilesensor', aShowInDomains, False, 0.8, '', '');
  fPreviewRequestTimer := scenario.project.Timers.SetTimer(
    procedure (aTimer: TTImer; aTime: THighResTicks)
    begin
      if Assigned(fTilerLayer) then
      begin
        Log.WriteLn('triggered preview timer for '+elementID);
        fTilerLayer.signalRequestPreview;
      end
      else Log.WriteLn('## triggered preview timer for '+elementID+' without having a tiler layer', llError);
    end);
  fSendRefreshTimer := scenario.project.Timers.CreateInactiveTimer;
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*3); // todo: parameterize
  fTilerLayer := nil;
  RegisterLayer;
end;

destructor TExpoSenseDotTrackLayer.Destroy;
begin
  FreeAndNil(fTilerLayer);
  inherited;
end;

function TExpoSenseDotTrackLayer.getJSON: string;
begin
  Result := inherited getJSON;
  if Assigned(fTilerLayer) and (fTilerLayer.URL<>'')
  then jsonAddString(Result, 'tiles', fTilerLayer.URLTimeStamped)
  else jsonAddString(Result, 'tiles', '', True);
  jsonAddString(Result, 'type', ltTile);
end;

function TExpoSenseDotTrackLayer.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
  if Assigned(fTilerLayer) and (fTilerLayer.URL <> '') then
    aClient.SendRefresh(elementID, '', fTilerLayer.URLTimeStamped);

  legendJSON := Self.legendJSON;
  if legendJSON <> '' then
    aClient.SendLegend(elementID, '', Self.legendJSON);
end;

function TExpoSenseDotTrackLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
end;

procedure TExpoSenseDotTrackLayer.handleRefreshTrigger(aTimeStamp: TDateTime);
var
  timeStampStr: string;
  tiles: string;
begin
  try
    if aTimeStamp<>0
    then timeStampStr := FormatDateTime(publisherDateTimeFormat, aTimeStamp)
    else timeStampStr := '';
    tiles := uniqueObjectsTilesLink;

    Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+' ('+timeStampStr+'): '+tiles);

    // signal refresh to layer client
    forEachSubscriber<TClient>(
      procedure(aClient: TClient)
      begin
        Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+', direct subscribed client: '+aClient.clientID, llNormal, 1);
        aClient.SendRefresh(elementID, timeStampStr, tiles);
      end);
    scenario.LayerRefreshed(fTilerLayer.ID, elementID, aTimeStamp, Self);
  except
    on E: Exception
    do Log.WriteLn('Exception in TLayer.handleRefreshTrigger: '+e.Message, llError);
  end;
end;

procedure TExpoSenseDotTrackLayer.RegisterLayer;
var
  paletteJSONObject: TJSONObject;
  modalityPalette: TWDPalette;
  _legendJSON: string;
  //start: TDateTime;
begin
  paletteJSONObject := TJSONObject.ParseJSONValue(ModalityPaletteJSON) as TJSONObject;
  try
    if Assigned(paletteJSONObject)
    then modalityPalette := JSON2PaletteAndLegend(paletteJSONObject, _legendJSON)
    else modalityPalette := CreateBlueTrackPalette('Modality');
  finally
    paletteJSONObject.Free;
  end;
  legendJSON := _legendJSON;
  fTilerLayer.Free;
  fTilerLayer := TTilerLayer.Create(
    scenario.project.Connection, elementID, stLocation,
    // tiler info
    procedure (aTilerLayer: TTilerLayer)
    begin
      aTilerLayer.signalAddSlice();
      aTilerLayer.signalSliceAction(tsaClearSlice);
    end,
    // refresh
    procedure(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime; aImmediate: Boolean)
    begin
      // todo: implement
      try
        if aImmediate  then
        begin
          fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*ImmediateTilerRefreshTime),
            procedure(aTimer: TTimer; aTime: THighResTicks)
            begin
              handleRefreshTrigger(aTimeStamp);
            end);
        end
        else
        begin
          fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*1),
            procedure(aTimer: TTimer; aTime: THighResTicks)
            begin
              handleRefreshTrigger(aTimeStamp);
            end);
        end;
        // refresh preview also
        fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*30);
      except
        on E: Exception
        do Log.WriteLn('Exception in TLayer.handleTilerRefresh ('+elementID+'): '+E.Message, llError);
      end;
    end,
    // preview
    procedure (aTilerLayer: TTilerLayer)
    begin
      fPreviewBase64 := aTilerLayer.previewAsBASE64;
      // layer clients
      forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          aClient.SendPreview(elementID, fPreviewBase64);
        end);
      Log.WriteLn('send normal preview on '+elementID);
    end,
    modalityPalette);
  fPreviewBase64 := fTilerLayer.previewAsBASE64;
  // trigger registration
  fTilerLayer.signalRegisterLayer(scenario.project.tiler, fDescription, False, 250);
end;

procedure TExpoSenseDotTrackLayer.reset;
begin
  fTilerLayer.signalSliceAction(tsaClearSlice);
end;

function TExpoSenseDotTrackLayer.uniqueObjectsTilesLink: string;
begin
  if Assigned(fTilerLayer)
  then Result := fTilerLayer.URLTimeStamped
  else Result := '';
end;

procedure TExpoSenseDotTrackLayer.Update(aStart, aEnd: TDateTime);
begin
  // todo: implement
end;

procedure TExpoSenseDotTrackLayer.UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
begin
  // todo: implement
end;

{ TExpoSenseLineTrackLayer }

procedure TExpoSenseLineTrackLayer.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aPrevLat, aPrevLon: Double; aMOT: Integer);
var
  geometry: TWDGeometry;
//  id: TWDID;
begin
  geometry := TWDGeometry.Create();
  try
    // build geometry from previous to this point
    //aLon, aLat, Double.NaN
    geometry.AddPoint(aPrevLon, aPrevLat, Double.NaN);
    geometry.AddPoint(aLon, aLat, Double.NaN);
    //id := TByteBuffer.bb_bytes(aSensorID, SizeOf(aSensorID))+TByteBuffer.bb_bytes(aTimeStamp, SizeOf(aTimeStamp));
    fTilerLayer.signalData(
      TByteBuffer.bb_tag_rawbytestring(icehTilerGeometry, geometry.Encode)+
      TByteBuffer.bb_tag_double(icehTilerValue, aMOT)+
      //TByteBuffer.bb_tag_bytes(icehObjectID, PAnsiChar(id)^, length(id)));
      TByteBuffer.bb_tag_guid(icehObjectID, TGUID.NewGuid));
      // todo: radius?
  finally
    geometry.Free;
  end;
end;

constructor TExpoSenseLineTrackLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad, aShowInDomains: Boolean);
begin
  inherited Create(
    aScenario, aDomain, aID, aName, aDescription,
    aDefaultLoad, 'mobilesensor', aShowInDomains, False, 0.8, '', '');
  fPreviewRequestTimer := scenario.project.Timers.SetTimer(
    procedure (aTimer: TTImer; aTime: THighResTicks)
    begin
      if Assigned(fTilerLayer) then
      begin
        Log.WriteLn('triggered preview timer for '+elementID);
        fTilerLayer.signalRequestPreview;
      end
      else Log.WriteLn('## triggered preview timer for '+elementID+' without having a tiler layer', llError);
    end);
  fSendRefreshTimer := scenario.project.Timers.CreateInactiveTimer;
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*3); // todo: parameterize
  //fLayerUpdateTimer := scenario.project.Timers.CreateInactiveTimer;
  //fLayerUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond/aLayerUpdateFrequency);
  fTilerLayer := nil;
  RegisterLayer;
end;

destructor TExpoSenseLineTrackLayer.Destroy;
begin
  FreeAndNil(fTilerLayer);
  inherited;
end;

function TExpoSenseLineTrackLayer.getJSON: string;
begin
  Result := inherited getJSON;
  if Assigned(fTilerLayer) and (fTilerLayer.URL<>'')
  then jsonAddString(Result, 'tiles', fTilerLayer.URLTimeStamped)
  else jsonAddString(Result, 'tiles', '', True);
  jsonAddString(Result, 'type', ltTile);
end;

function TExpoSenseLineTrackLayer.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
  if Assigned(fTilerLayer) and (fTilerLayer.URL <> '') then
    aClient.SendRefresh(elementID, '', fTilerLayer.URLTimeStamped);

  legendJSON := Self.legendJSON;
  if legendJSON <> '' then
    aClient.SendLegend(elementID, '', Self.legendJSON);
end;

function TExpoSenseLineTrackLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
end;

procedure TExpoSenseLineTrackLayer.handleRefreshTrigger(aTimeStamp: TDateTime);
var
  timeStampStr: string;
  tiles: string;
begin
  try
    if aTimeStamp<>0
    then timeStampStr := FormatDateTime(publisherDateTimeFormat, aTimeStamp)
    else timeStampStr := '';
    tiles := uniqueObjectsTilesLink;

    Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+' ('+timeStampStr+'): '+tiles);

    // signal refresh to layer client
    forEachSubscriber<TClient>(
      procedure(aClient: TClient)
      begin
        Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+', direct subscribed client: '+aClient.clientID, llNormal, 1);
        aClient.SendRefresh(elementID, timeStampStr, tiles);
      end);
    scenario.LayerRefreshed(fTilerLayer.ID, elementID, aTimeStamp, Self);
  except
    on E: Exception
    do Log.WriteLn('Exception in TLayer.handleRefreshTrigger: '+e.Message, llError);
  end;
end;

procedure TExpoSenseLineTrackLayer.RegisterLayer;
var
  paletteJSONObject: TJSONObject;
  modalityPalette: TWDPalette;
  _legendJSON: string;
  //start: TDateTime;
begin
  paletteJSONObject := TJSONObject.ParseJSONValue(ModalityPaletteJSON) as TJSONObject;
  try
    if Assigned(paletteJSONObject)
    then modalityPalette := JSON2PaletteAndLegend(paletteJSONObject, _legendJSON)
    else modalityPalette := CreateBlueTrackPalette('Modality');
  finally
    paletteJSONObject.Free;
  end;
  legendJSON := _legendJSON;
  fTilerLayer.Free;
  fTilerLayer := TTilerLayer.Create(
    scenario.project.Connection, elementID, stGeometryI,
    // tiler info
    procedure (aTilerLayer: TTilerLayer)
    begin
      aTilerLayer.signalAddSlice();
      aTilerLayer.signalSliceAction(tsaClearSlice);
    end,
    // refresh
    procedure(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime; aImmediate: Boolean)
    begin
      // todo: implement
      try
        if aImmediate  then
        begin
          fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*ImmediateTilerRefreshTime),
            procedure(aTimer: TTimer; aTime: THighResTicks)
            begin
              handleRefreshTrigger(aTimeStamp);
            end);
        end
        else
        begin
          fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*1),
            procedure(aTimer: TTimer; aTime: THighResTicks)
            begin
              handleRefreshTrigger(aTimeStamp);
            end);
        end;
        // refresh preview also
        fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*30);
      except
        on E: Exception
        do Log.WriteLn('Exception in TLayer.handleTilerRefresh ('+elementID+'): '+E.Message, llError);
      end;
    end,
    // preview
    procedure (aTilerLayer: TTilerLayer)
    begin
      fPreviewBase64 := aTilerLayer.previewAsBASE64;
      // layer clients
      forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          aClient.SendPreview(elementID, fPreviewBase64);
        end);
      Log.WriteLn('send normal preview on '+elementID);
    end,
    modalityPalette);//, -1, '' .addLayer(elementID, aSliceType, aPalette);
  fPreviewBase64 := fTilerLayer.previewAsBASE64;
  // trigger registration
  fTilerLayer.signalRegisterLayer(scenario.project.tiler, fDescription, False, 250);
  {
  Log.WriteLn('Waiting on tiler for layer '+elementID);
  start := Now;
  while (fTilerLayer.URL='') and (Now-start<dtOneSecond*15)
  do Sleep(1000);
  if fTilerLayer.URL=''
  then Log.WriteLn('Timeout on layer registration for '+elementID, llWarning);
  }
end;

procedure TExpoSenseLineTrackLayer.reset;
begin
  fTilerLayer.signalSliceAction(tsaClearSlice);
end;

function TExpoSenseLineTrackLayer.uniqueObjectsTilesLink: string;
begin
  if Assigned(fTilerLayer)
  then Result := fTilerLayer.URLTimeStamped
  else Result := '';
end;

procedure TExpoSenseLineTrackLayer.Update(aStart, aEnd: TDateTime);
begin
  // todo: implement
end;

procedure TExpoSenseLineTrackLayer.UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aMOT: Integer);
begin
  // todo: implement
end;

{ TExpoSenseScenario }

procedure TExpoSenseScenario.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aValue: Double);
var
  ol: TOrderedListTS<TExpoSenseDataPoint>;
  edp: TExpoSenseDataPoint;
  sliderTime: string;
  so: TCircleMarker;
  prevESDP: TExpoSenseDataPoint;
begin
  // todo: implement
  if not fDataPoints.TryGetValue(aSensorID, ol) then
  begin
    ol := TOrderedListTS<TExpoSenseDataPoint>.Create();
    fDataPoints.AddOrSetValue(aSensorID, ol);
  end;

  if fDataPoints.Count=1 then
  begin
    so := TCircleMarker.Create(fCombinedTrackLayer, 'home', aLat, aLon, 12);
    so.addOptionGeoColor(TGeoColors.Create(motColors[motStationary]));
    fCombinedTrackLayer.AddObject(so, so.jsonNewObject);
  end;

  prevESDP := ol.ItemsTS[aTimeStamp, True];
  if (prevESDP=nil) or (prevESDP.value<>aValue) then
  begin
    edp := TExpoSenseDataPoint.Create(aTimeStamp, aLat, aLon, aValue);
    ol.AddTS(edp);

    if not fFiltered then
    begin
      // todo: CombinedTrackAddValue(sensorid, timestamp, lastLat, lastLon, prevLat, prevLon, mot);

      so := TCircleMarker.Create(fCombinedTrackLayer, edp.id(aSensorID), aLat, aLon, 2);
      so.addOptionGeoColor(TGeoColors.Create(motColors[edp.modeOfTransportation]));
      fCombinedTrackLayer.AddObject(so, so.jsonNewObject);

  //    fHeightTrackLayer.AddValue(sensorid, timeStamp, lastLat, lastLon, value, mot);
  //    fDotTrackLayer.AddValue(sensorid, timeStamp, lastLat, lastLon, mot);
  //    prevLon := ValueStored[sensordata_longitude-1, sensorid];
  //    prevLat := ValueStored[sensordata_latitude-1, sensorid];
  //    if not (prevLon.IsNan or prevLat.IsNan)
  //    then fLineTrackLayer.AddValue(sensorid, timestamp, lastLat, lastLon, prevLat, prevLon, mot);
  //    ValueStored[sensordata_longitude-1, sensorid] := lastLon;
  //    ValueStored[sensordata_latitude-1, sensorid] := lastLat;

      fMeasuredECValuesChart.AddValue(aTimeStamp, [aValue*chartSensorValueFactor]);

      {
      if fMeasuredECValuesChart.allValues.Count>=2 then
      begin
        average := (value*chartValueFactor+fMeasuredECValuesChart.allValues[fMeasuredECValuesChart.allValues.Count-2].y[0])/2.0;
        delta := timeStamp-fMeasuredECValuesChart.allValues[fMeasuredECValuesChart.allValues.Count-2].x;
        _totalValue := average * delta * 24 * 60;
        if fTotalChart.allValues.Count>0
        then _totalValue := _totalValue + fTotalChart.allValues[fTotalChart.allValues.Count-1].y[0];
        fTotalChart.AddValue(timeStamp, [_totalValue])
      end
      else fTotalChart.AddValue(timeStamp, [value*chartValueFactor]);
      }
      sliderTime := FormatDateTime(publisherDateTimeFormat, aTimestamp);
      forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          aClient.signalString('{"type":"timesliderEvents","payload":{"setCurrentTime":"'+sliderTime+'"}}');
        end);
    end;
    triggerUpdateTimesliderData();
  end;
end;

constructor TExpoSenseScenario.Create(aProject: TProject; const aID, aName,
  aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
begin
  fLiveCounter := 0;
  fDBCounter := 0;
  fDataPoints := TObjectDictionary<TGUID, TOrderedListTS<TExpoSenseDataPoint>>.Create([doOwnsValues]);
//  fLastLats := TDictionary<TGUID, Double>.Create;
//  fLastLons := TDictionary<TGUID, Double>.Create;
//  fPrevLats := TDictionary<TGUID, Double>.Create;
//  fPrevLons := TDictionary<TGUID, Double>.Create;
//  fLastCalcLats := TDictionary<TGUID, Double>.Create;
//  fLastCalcLons := TDictionary<TGUID, Double>.Create;
//  fValueStored := TObjectDictionary<Integer, TDictionary<TGUID, Double>>.Create([doOwnsValues]);
  //fSensorsDataSet := TSensorsDataSet.Create;
  fTimeSliderDataTimer := aProject.Timers.CreateInactiveTimer;
  fTimeSliderDataTimer.MaxPostponeDelta := DateTimeDelta2HRT(MaxTimeSliderUpdateTime*dtOneSecond);
  fShowDataSelectionTimer := aProject.Timers.CreateInactiveTimer;
  fFirstTimeSliderUpdate := True;
  fFiltered := False;
  inherited Create(aProject, aID, aName, aDescription, aAddbasicLayers, aMapView);
  fPubEvent := aProject.Connection.eventEntry('mobilesensordata').publish;
  fLiveEvent := aProject.Connection.eventEntry('mobilesensordata').subscribe;
  fLiveEventHandler := handleLiveEvent;
  fLiveEvent.OnEvent.Add(fLiveEventHandler);
end;

destructor TExpoSenseScenario.Destroy;
begin
  fLiveEvent.OnEvent.Remove(fLiveEventHandler);
  project.Connection.unSubscribe(fLiveEvent);
  project.Connection.unPublish(fPubEvent);
  CancelTimer(fTimeSliderDataTimer);
  CancelTimer(fShowDataSelectionTimer);
//  FreeAndNil(fLastLons);
//  FreeAndNil(fLastLats);
//  FreeAndNil(fPrevLats);
//  FreeAndNil(fPrevLons);
//  FreeAndNil(fLastCalcLats);
//  FreeAndNil(fLastCalcLons);
//  FreeAndNil(fValueStored);
  FreeAndNil(fDataPoints);
  inherited;
end;
{
function TExpoSenseScenario.GetValueStored(aTag: Integer; const aSensorID: TGUID): Double;
var
  d: TDictionary<TGUID, Double>;
begin
  if fValueStored.TryGetValue(aTag, d) then
  begin
    if not d.TryGetValue(aSensorID, Result)
    then Result := Double.NaN;
  end
  else Result := Double.NaN;
end;
}
function TExpoSenseScenario.HandleClientSubscribe(aClient: TClient): Boolean;
var
  jsonTSData: string;
  palette: TWDPalette;
  extent: TWDExtent;
begin
  Result := inherited;
  // send data to time slider
  palette := CreateGraySliderPalette('BC slider');
  try
    extent := TWDExtent.Create;
    jsonTSData := jsonTimesliderData(palette, extent);
  finally
    palette.Free;
  end;
  aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
  // set map view according data set
  {
  if not (extent.CenterY.IsNaN or extent.CenterX.IsNaN) then
  begin
    fMapView := TMapView.Create(extent.CenterY, extent.CenterX, fMapView.zoom);
    aClient.SendView(fMapView.lat, fMapView.lon, Double.NaN);
  end;
  }
  //fMobileChart.sub
  aClient.SubscribeTo(fMeasuredECValuesChart, nil);
  aClient.SubscribeTo(fCalculatedECValuesChart, nil);
  //aClient.SubscribeTo(fTotalChart, nil);
  //aClient.SubscribeTo(fTrackLayer);
end;

function TExpoSenseScenario.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
  aClient.UnsubscribeFrom(fMeasuredECValuesChart);
  aClient.UnsubscribeFrom(fCalculatedECValuesChart);
  //aClient.UnsubscribeFrom(fTotalChart);
  //aClient.UnsubscribeFrom(fTrackLayer);
end;

procedure TExpoSenseScenario.handleLiveEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
begin
  fLiveCounter := fLiveCounter + 1;
  handleUniEvent(aEventEntry, aPayload, aCursor, aLimit);
end;

procedure TExpoSenseScenario.HandleScenarioRefresh(aClient: TClient; const aType: string; aPayload: TJSONObject);
var
  scenario: TJSONValue;
begin
  // todo: right button project description -> refresh clicked
  if aPayload.TryGetValue<TJSONValue>('scenario', scenario) then
  begin
    if scenario is TJSONString then
    begin
      try
        if string.CompareText(id, scenario.Value)=0 then
        begin
          Log.WriteLn('HandleScenarioRefresh: initiated scenario refresh');
          // todo: implement scenario refresh

        end
        else Log.WriteLn('HandleScenarioRefresh: wrong id? "'+scenario.value+'" <> "'+id+'"', llError);
      except
        Log.WriteLn('HandleScenarioRefresh: could not decode scenario from "'+scenario.value+'"', llWarning);
      end;
    end;
  end;
end;

procedure TExpoSenseScenario.HandleTimeSliderEvent(aClient: TClient; const aType: string; aPayload: TJSONObject);
var
  brush: TJSONValue;
  extent: TJSONValue;
//  a: TJSONArray;
  selectedEvent: TJSONValue;
  lat, lon: TJSONValue;
//  trackLayer: TExpoSenseTrackLayer;
//  qdtFrom: TDateTime;
//  qdtTo: TDateTime;
begin
  if aPayload.TryGetValue<TJSONValue>('brush', brush) then
  begin
    if brush.TryGetValue('extent', extent) then
    begin

      // HandleTimeSliderEvent: timeslider: {"brush":{"extent":["2017-10-13 18:09","2017-11-07 16:14"]}}
      // HandleTimeSliderEvent: timeslider: {"brush":{"extent":{}}}

      // add selected points to tracklayer
      (*
      if Assigned(fTrackLayers) then
      begin
        if fTrackLayers.TryGetValue(sensordata_ec{sensordata_no2}, trackLayer) then
        begin
          if not fShowDataSelectionTimer.Enabled then
          begin
            // clear layer and chart
            // todo: **** trackLayer.reset;
            fTotalChart.reset;
            fMobileChart.reset;
          end;
          // check extent of data to show
          if extent is TJSONArray then
          begin
            a := extent as TJSONArray;
            if a.Count>=2 then
            begin
              qdtFrom := StrToDateTime(a.Items[0].ToString, isoDateTimeFormatSettings);
              qdtTo := StrToDateTime(a.Items[a.Count-1].ToString, isoDateTimeFormatSettings);
              fFiltered := True;
            end
            else
            begin
              qdtFrom := 0;
              qdtTo := 0;
              fFiltered := False;
            end;
          end
          else
          begin
            qdtFrom := 0;
            qdtTo := 0;
            fFiltered := False;
          end;
          // schedule job to show data
          fShowDataSelectionTimer.Arm(DateTimeDelta2HRT(dtOneSecond),
            procedure (aTimer: TTimer; aTime: THighResTicks)
            begin
              ShowDataSelection(trackLayer, fMobileChart, fTotalChart, qdtFrom, qdtTo);
            end);

        end;
      end
      else Log.WriteLn('No track layers defined: '+self.ID, llWarning);
      *)
    end;
  end
  else if aPayload.TryGetValue<TJSONValue>('selectedEvent', selectedEvent) then
  begin
    // HandleTimeSliderEvent: timeslider: {"selectedEvent":{"start":"2017-10-21T11:54:00.000Z","end":"2017-10-22T08:31:00.000Z","color":"#B2B2B2","lat":51.465481431,"lon":5.49652172}}

    if selectedEvent.TryGetValue<TJSONValue>('lat', lat) and (selectedEvent.TryGetValue<TJSONValue>('lon', lon)) then
    begin
      if (lat is TJSONNumber) and (lon is TJSONNumber)
      then aCLient.SendView((lat as TJSONNumber).AsDouble,(lon as TJSONNumber).AsDouble, Double.NaN);
    end;
  end
  else
  begin
    // move slider etc..
    //Log.WriteLn('HandleTimeSliderEvent: '+aType+': '+aPayload.ToJSON);
  end;
end;

procedure TExpoSenseScenario.handleUniEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  value, timestamp: Double;
  sensorid: TGUID;
  sensorLat: Double;
  sensorLon: Double;
//  average: Double;
//  delta: Double;
//  _totalValue: Double;
  modeStr: string;
//  prevLon: Double;
//  prevLat: Double;
  mot: Integer;
  locationStr: string;
  location: Integer;
  calcLon: Double;
  calcLat: Double;
begin
  timestamp := 0;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      wdatTimeStamp:
        begin
          timestamp := aPayload.bb_read_double(aCursor);
          // work-a-round for timestamp resolution problem in processor
          timestamp := Round(timestamp*Double(24.0*60.0*60.0))/Double(24.0*60.0*60.0);
        end;
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          sensorid := aPayload.bb_read_guid(aCursor);
        end;
      sensordata_mode_of_transportation:
        begin
          modeStr := aPayload.bb_read_string(aCursor);
//          lastLon := ValueStored[calculated_longitude, sensorid];
//          lastLat := ValueStored[calculated_latitude, sensorid];
          if not (calcLon.IsNan or calcLat.IsNan) then
          begin
            mot := TExpoSenseDataPoint.ModeOfTransportationStrToInt(modeStr);
            UpdateMOT(sensorid, timestamp, calcLat, calcLon, mot);
          end;
        end;
      sensordata_location_type:
        begin
          locationStr := aPayload.bb_read_string(aCursor);
//          lastLon := ValueStored[calculated_longitude, sensorid];
//          lastLat := ValueStored[calculated_latitude, sensorid];
          if not (calcLon.IsNan or calcLat.IsNan) then
          begin
            location := TExpoSenseDataPoint.LocationStrToInt(locationStr);
            UpdateLocation(sensorid, timeStamp, calcLat, calcLon, location);
          end;
        end;
      sensordata_ec:
        begin
          value := aPayload.bb_read_double(aCursor);
          // correct for negative sensor values (just ignore and use 0)
          //if value<0
          //then value := 0;
          if value>0 then
          begin
//            lastLon := ValueStored[sensordata_longitude, sensorid];
//            lastLat := ValueStored[sensordata_latitude, sensorid];
            if not (sensorLon.IsNan or sensorLat.IsNan) then
            begin
              AddValue(sensorid, timeStamp, sensorLat, sensorLon, value);
            end;
          end;
        end;
      sensordata_latitude:
        begin
          sensorLat := aPayload.bb_read_double(aCursor);
          //ValueStored[sensordata_latitude, sensorid] := value;
        end;
      sensordata_longitude:
        begin
          sensorLon := aPayload.bb_read_double(aCursor);
          //ValueStored[sensordata_longitude, sensorid] := value;
        end;
      calculated_latitude:
        begin
          calcLat := aPayload.bb_read_double(aCursor);
          //ValueStored[calculated_latitude, sensorid] := value;
        end;
      calculated_longitude:
        begin
          calcLon := aPayload.bb_read_double(aCursor);
          //ValueStored[calculated_longitude, sensorid] := value;
        end;
      meteodata_windspeed:
        begin
          value := aPayload.bb_read_double(aCursor);
          // todo: store with time stamp
          if not fFiltered then
          begin
            project.windControl.windSpeed := value;
            project.windControl.live := True;
          end;
        end;
      meteodata_winddirection:
        begin
          value := aPayload.bb_read_double(aCursor);
          // todo: store with time stamp
          if not fFiltered then
          begin
            project.windControl.windDirection := value;
            project.windControl.live := True;
          end;
        end
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

function TExpoSenseScenario.jsonTimesliderData(aPalette: TWDPalette; var aExtent: TWDExtent): string;
var
  c: Integer;
  oneList: TOrderedListTS<TExpoSenseDataPoint>;
  sensor: TPair<TGUID, TOrderedListTS<TExpoSenseDataPoint>>;
  edp: TExpoSenseDataPoint;
  stepSize: TDateTime;
  entryStartTimeStamp: TDateTime;
  entryColor: TAlphaRGBPixel;
  prevTimeStamp: TDateTime;
  loopSensorValue: Double;
  loopLat: Double;
  loopLon: Double;
  loopSensorValueColor: TAlphaRGBPixel;
  entryEndTimeStamp: TDateTime;
  entry: string;
begin
  // todo: use cursor, if a sensor has no value on a specific time it is not accounted for and a higher value
  // could be shown then calculated for the time stamp
  Result := '';
  c := 0;
  // merge all sensor into 1 list
  oneList := TOrderedListTS<TExpoSenseDataPoint>.Create();
  try
    for sensor in fDataPoints do
    begin
      for edp in sensor.Value do
      begin
        // todo: add all and sort at the end (not implemented yet)
        oneList.AddTS(TExpoSenseDataPoint.Create(edp.timeStamp, edp.geometry.y, edp.geometry.x, edp.value{, edp.height}));
      end;
    end;
    entryStartTimeStamp := Double.NaN;
    entryColor := 0;
    prevTimeStamp := Double.NaN;
    loopLat := Double.NaN;
    loopLon := Double.NaN;
    for edp in oneList do
    begin
      // init step values
      //loopSensorValue := Double.NaN;
      loopSensorValue := edp.value;
      loopLat := edp.geometry.y;
      loopLon := edp.geometry.x;
      aExtent.Expand(loopLon, loopLat);
      // color with value
      loopSensorValueColor := aPalette.ValueToColors(loopSensorValue).fillColor;
      // calculate length of step
      if Double(prevTimeStamp).IsNaN then
      begin
        // FIRST step, init entry
        stepSize := 0;
        entryStartTimeStamp := edp.timeStamp;
        entryColor := loopSensorValueColor;
      end
      else stepSize := edp.timeStamp-prevTimeStamp; // NOT first step

      // check if entry should be closed (added)
      if (loopSensorValueColor<>entryColor) or (stepSize>MaxNoSensorValueTime) then
      begin
        // check for transparancy
        if ((entryColor and $FF000000)<>0) or (stepSize>MaxNoSensorValueTime) then
        begin
          // calculate end time for entry
          if stepSize>MaxNoSensorValueTime
          then entryEndTimeStamp := prevTimeStamp+MaxNoSensorValueTime
          else entryEndTimeStamp := edp.timeStamp;
          // build entry
          entry :=
            '"start":"'+FormatDateTime(publisherDateTimeFormat, entryStartTimeStamp)+'"'+','+
            '"end":"'+FormatDateTime(publisherDateTimeFormat, entryEndTimeStamp)+'"'+','+
            '"color":"'+ColorToJSON(entryColor)+'"'+','+
            //'"tooltip":'+'"max value: '+prevmax.toString+'"'+','+ // localized double
            '"lat":'+DoubleToJSON(loopLat)+','+
            '"lon":'+DoubleToJSON(loopLon);
          jsonAdd(Result, '{'+entry+'}');
          c := c+1;
        end;
        // start new entry
        entryStartTimeStamp := edp.timeStamp;
        entryColor := loopSensorValueColor;
      end;
      // prepare next step
      prevTimeStamp := edp.timeStamp;
    end;
    // add last step
    // check for transparancy
    if (entryColor and $FF000000)<>0 then
    begin
      entryEndTimeStamp := prevTimeStamp+MaxNoSensorValueTime;
      entry :=
        '"start":"'+FormatDateTime(publisherDateTimeFormat, entryStartTimeStamp)+'"'+','+
        '"end":"'+FormatDateTime(publisherDateTimeFormat, entryEndTimeStamp)+'"'+','+
        '"color":"'+ColorToJSON(entryColor)+'"'+','+
        //'"tooltip":'+'"max value: '+prevmax.toString+'"'+','+ // localized double
        '"lat":'+DoubleToJSON(loopLat)+','+
        '"lon":'+DoubleToJSON(loopLon);
      jsonAdd(Result, '{'+entry+'}');
      c := c+1;
    end;
  finally
    oneList.Free;
  end;
  Log.WriteLn('Create timeslider data: '+c.ToString+' elements');
end;

procedure TExpoSenseScenario.ReadBasicData;
var
  oraSession: TOraSession;
  USDataSource: string;
  USScenarioID: Integer;
  USUserName: string;
  scenarioID: Integer;
  tablePrefix: string;
  federation: string;
  USSourceProjection: TGIS_CSProjectedCoordinateSystem;
  metaLayer: TDictionary<Integer, TMetaLayerEntry>;
  imlep: TPair<Integer, TMetaLayerEntry>;
  layer: TLayerBase;
  IMB3RemoteHost: string;
  USTablePrefix: string;
  paletteJSONObject: TJSONObject;
  legendJSON: string;
begin
  fMeasuredECValuesChart :=  TChartLines.Create(Self, 'Personal exposure', 'Measured' + 'BC'+'Chart', 'Measured BC µg/m³', 'Personal, measured BC exposure', True, 'line',
    TChartAxis.Create('time', 'lightBlue', 'Time', 'min'),
    [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3'){,
      TChartAxis.Create('concentration', 'PaleVioletRed', 'Concentration', 'mg/m3')}],
    'time', 3, True, 10, 0);
  fMeasuredECValuesChart.chartUpdateTime := 2;
  AddChart(fMeasuredECValuesChart);
  fMeasuredECValuesChart.reset;

  fCalculatedECValuesChart :=  TChartLines.Create(Self, 'Personal exposure', 'Calculated' + 'BC'+'Chart', 'Calculated BC µg/m³', 'Personal, Calculated BC exposure', True, 'line',
    TChartAxis.Create('time', 'lightBlue', 'Time', 'min'),
    [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3'){,
      TChartAxis.Create('concentration', 'PaleVioletRed', 'Concentration', 'mg/m3')}],
    'time', 3, True, 10, 0);
  fCalculatedECValuesChart.chartUpdateTime := 2;
  AddChart(fCalculatedECValuesChart);
  fCalculatedECValuesChart.reset;

  {
  fTotalChart := TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts' + 'BC' + 'total', 'BC' + '-total', 'Personal BC' + ' Total', False, 'line',
    TChartAxis.Create('time', 'lightBlue', 'Time', 'min'),
    [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3')], 'time', 3);
  fTotalChart.chartUpdateTime := 2;
  AddChart(fTotalChart);
  }

  // add air (realtime)
  // todo: for now test scenario to see air layer for amsterdam
  USDataSource := 'us_ams_test/us_ams_test@app-usdata01.tsn.tno.nl/uspsde';
  USScenarioID := 1;
  USUserName := 'us_ams_test';
  USTablePrefix := 'V1#';
  IMB3RemoteHost := 'app-usmodel01.tsn.tno.nl';
  USSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(28992);

  oraSession := TOraSession.Create(nil);
  try
    oraSession.ConnectString := USDataSource;
    oraSession.Open;

    scenarioID := USScenarioID;

    tablePrefix := GetScenarioTablePrefix(oraSession, scenarioID);
    federation := GetScenarioFederation(oraSession, scenarioID);


    fIMB3Connection.Free;
    fIMB3Connection := TIMBConnection.Create(IMB3RemoteHost, 4000, 'PublisherExposens', 1, federation);

    metaLayer := TDictionary<Integer, TMetaLayerEntry>.Create;
    try
      if ReadMetaLayer(oraSession, tablePrefix, metaLayer) then
      begin
        imlep.value := metalayer[1000];
//        for imlep in metalayer do
//        begin
//          if imlep.value._published>0 then
//          begin
            layer := imlep.value.CreateUSLayer(
              self, tablePrefix, ConnectStringFromSession(oraSession),
              SubscribeUSDataEvents(USUserName, imlep.value.IMB_EVENTCLASS, USTablePrefix, fIMB3Connection),
              USSourceProjection, imlep.value.Domain, imlep.value.description, 0.2, True);
            if Assigned(layer) then
            begin
              AddLayer(layer);
              // schedule reading objects and send to
              AddCommandToQueue(Self, (layer as TUSLayer).ReadObjects);
              Log.WriteLn('Added layer '+imlep.value.Domain+'\'+imlep.value.description);
            end
            else Log.WriteLn('Could not add US layer '+imlep.value.Domain+'\'+imlep.value.description, llError);
//          end;
//        end;
      end;
    finally
      metaLayer.Free;
    end;
  finally
    oraSession.Free;
  end;

  (*
  fHeightTrackLayer := TExpoSenseHeightTrackLayer.Create(
    Self, 'Personal exposure', 'personal-track-' + 'BC', 'Personal Track ribbon ' + 'BC', 'BC track',
    DefaultExpoSensoECValueToHeightFactor,
    False, True);
  AddLayer(fHeightTrackLayer);

  fDotTrackLayer := TExpoSenseDotTrackLayer.Create(
    Self, 'Personal exposure', 'personal-track-points-' + 'BC', 'Personal Track Points ' + 'BC', 'BC track points',
    True, True);
  AddLayer(fDotTrackLayer);

  fLineTrackLayer := TExpoSenseLineTrackLayer.Create(
    Self, 'Personal exposure', 'personal-track-lines-' + 'BC', 'Personal Track Lines ' + 'BC', 'BC track lines',
    True, True);
  AddLayer(fLineTrackLayer);
  *)

  paletteJSONObject := TJSONObject.ParseJSONValue(ModalityPaletteJSON) as TJSONObject;
  try
    if Assigned(paletteJSONObject)
    then legendJSON := JSON2Legend(paletteJSONObject)
    else legendJSON := '';
  finally
    paletteJSONObject.Free;
  end;

  fCombinedTrackLayer := TSimpleLayer.Create(
    Self, 'Personal exposure', 'personal-track-simple', 'Personal Track combined ' + 'BC', 'BC track',
    [], [], True, 'BC track dots', True, False, 1.0, legendJSON, 1);
  AddLayer(fCombinedTrackLayer);

  forEachSubscriber<TClient>(
    procedure(aClient: TClient)
    begin
      fProject.SendDomains(aClient, 'updatedomains');
    end);
end;
{
procedure TExpoSenseScenario.setValueStored(aTag: Integer; const aSensorID: TGUID; aValue: Double);
var
  d: TDictionary<TGUID, Double>;
begin
  if not fValueStored.TryGetValue(aTag, d) then
  begin
    d := TDictionary<TGUID, Double>.Create;
    fValueStored.AddOrSetValue(aTag, d);
  end;
  d.AddOrSetValue(aSensorID, aValue);
end;
}
procedure TExpoSenseScenario.triggerUpdateTimesliderData;
begin
  if Assigned(fTimeSliderDataTimer) then
  begin
    fTimeSliderDataTimer.Arm(DateTimeDelta2HRT(DefaultTimeSliderUpdateTime*dtOneSecond),
      procedure (aTimer: TTimer; aTime: THighResTicks)
      var
        jsonTSData: string;
        palette: TWDPalette;
        extent: TWDExtent;
      begin
        palette := CreateGraySliderPalette('BC slider');
        try
          extent := TWDExtent.Create;
          jsonTSData := jsonTimesliderData(palette, extent);
        finally
          palette.Free;
        end;
        ForEachSubscriber<TClient>(
          procedure(aClient: TClient)
          begin
            aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
            // set map view according data set if this is the first time time slider data is send to clients ie after inquire
            {
            if fFirstTimeSliderUpdate and not (extent.CenterY.IsNaN or extent.CenterX.IsNaN) then
            begin
              fMapView := TMapView.Create(extent.CenterY, extent.CenterX, fMapView.zoom);
              aClient.SendView(extent.CenterY, extent.CenterX, Double.NaN);
            end;
            }
          end);
        if not (extent.CenterY.IsNaN or extent.CenterX.IsNaN)
        then fFirstTimeSliderUpdate := False;
      end);
  end;
end;

procedure TExpoSenseScenario.UpdateLocation(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aLocation: Integer);
var
  ol: TOrderedListTS<TExpoSenseDataPoint>;
  dp: TExpoSenseDataPoint;
  so: TSimpleObject;
begin
  if fDataPoints.TryGetValue(aSensorID, ol) then
  begin
    dp := ol.ItemsTS[aTimeStamp, True];
    if Assigned(dp) then
    begin
      if not fFiltered then
      begin
        // todo: CombinedTrackUpdateMOT(sensorid, timeStamp, mot);
        if fCombinedTrackLayer.objects.TryGetValue(dp.id(aSensorID), so) then
        begin
          // update location
          if ((so.geometry as TWDGeometryPoint).x <> aLon) or ((so.geometry as TWDGeometryPoint).y <> aLat) then
          begin
            (so.geometry as TWDGeometryPoint).x := aLon;
            (so.geometry as TWDGeometryPoint).y := aLat;
            fCombinedTrackLayer.UpdateObject(so, sojnGeometry, so.jsonGeometryValue);
          end;
          if dp.location<>aLocation then
          begin
            if (aLocation=locNotAtHome) or (aLocation=locOutdoors) then
            begin
              // todo: check location
              so.addOptionGeoColor(TGeoColors.Create(motColors[motStationary]));
              // map value to radius -> 0=3px 5000=5px log
              (so as TCircleMarker).radius := 7;
              fCombinedTrackLayer.UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            end;
            dp.location := aLocation;
          end;
        end;


//        fHeightTrackLayer.UpdateMOT(sensorid, timeStamp, mot);
//        fDotTrackLayer.UpdateMOT(sensorid, timeStamp, mot);
//        fLineTrackLayer.UpdateMOT(sensorid, timeStamp, mot);
      end;
      triggerUpdateTimesliderData();
    end;
  end;
end;

procedure TExpoSenseScenario.UpdateMOT(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aMOT: Integer);
var
  ol: TOrderedListTS<TExpoSenseDataPoint>;
  dp: TExpoSenseDataPoint;
  so: TSimpleObject;
  layerECLive: TLayerBase;
begin
  if fDataPoints.TryGetValue(aSensorID, ol) then
  begin
    dp := ol.ItemsTS[aTimeStamp, True];
    if Assigned(dp) then
    begin
      if not fFiltered then
      begin
        // todo: CombinedTrackUpdateMOT(sensorid, timeStamp, mot);
        if fCombinedTrackLayer.objects.TryGetValue(dp.id(aSensorID), so) then
        begin
          // update location
          if ((so.geometry as TWDGeometryPoint).x <> aLon) or ((so.geometry as TWDGeometryPoint).y <> aLat) then
          begin
            (so.geometry as TWDGeometryPoint).x := aLon;
            (so.geometry as TWDGeometryPoint).y := aLat;
            fCombinedTrackLayer.UpdateObject(so, sojnGeometry, so.jsonGeometryValue);
          end;
          if dp.modeOfTransportation<>aMOT then
          begin
            // map value to radius log -> 0=>3 px 5000=>max px
            //(so as TCircleMarker).radius := 1+min(4*ln(max(dp.value, 1))/ln(6500), 4);
            (so as TCircleMarker).radius := 1.5+min(3.0*dp.value/6500.0, 3.0);
            so.addOptionGeoColor(TGeoColors.Create(motColors[aMOT]));
            fCombinedTrackLayer.UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            dp.modeOfTransportation := aMOT;
          end;
        end;


//        fHeightTrackLayer.UpdateMOT(sensorid, timeStamp, mot);
//        fDotTrackLayer.UpdateMOT(sensorid, timeStamp, mot);
//        fLineTrackLayer.UpdateMOT(sensorid, timeStamp, mot);
      end;
      triggerUpdateTimesliderData();
      // initiate async tiler request to get value of live BC layer
      if layers.TryGetValue('1000', layerECLive) then
      begin
        if layerECLive is TUSLayer then
        begin
          (layerECLive as TUSLayer).tilerLayer.onPointValue :=
            procedure(aTilerLayer: TTilerLayer; const aRequestID: TWDID; aLat, aLon: Double; aTimeStampSlice: TDateTime; aValue: Double)
            var
              timeStamp: TDateTime;
            begin
              timeStamp := StrToDateTime(string(UTF8String(aRequestID)));
              fCalculatedECValuesChart.AddValue(timeStamp, [aValue*chartCalculatedValueFactor]);
            end;
          (layerECLive as TUSLayer).tilerLayer.signalSliceRequestPointValue(RawByteString(UTF8String(DateTimeToStr(aTimeStamp))), aLat, aLon);
        end;
      end;
    end;
  end;
end;

{ TExpoSenseProject }

constructor TExpoSenseProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
begin
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN,
    aTilerStatusURL, nil, aAddBasicLayers, aMaxNearestObjectDistanceInMeters, aMapView);
  fTiler.onTilerStatus := handleTilerStatus;
  //Set ExpoSense controls
  SetControl(timeSliderControl, '1');
  windControl.update(10, 5.4, 1); // init by first use
  clientMessageHandlers.Add(timeSliderControl,
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    begin
      if Assigned(aClient.currentScenario) and (aClient.currentScenario is TExpoSenseScenario)  then
      begin
        (aClient.currentScenario as TExpoSenseScenario).HandleTimeSliderEvent(aClient, aType, aPayload);
      end;
    end);
  clientMessageHandlers.Add('scenarioRefresh',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    begin
      if Assigned(aClient.currentScenario) and (aClient.currentScenario is TExpoSenseScenario)  then
      begin
        (aClient.currentScenario as TExpoSenseScenario).HandleScenarioRefresh(aClient, aType, aPayload);
      end;
    end);
end;

function TExpoSenseProject.CreateExpoSenseScenario(const aScenarioID: string{; aGUID: TGUID; aLowerTimeStamp, aUpperTimeStamp: Double}): TExpoSenseScenario;
begin
  Result := TExpoSenseScenario.Create(Self, aScenarioID, 'GPS tracks', 'Exposure on GPS tracks', False, MapView);
end;

function TExpoSenseProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

procedure TExpoSenseProject.ReadBasicData;
var
  scenario: TExpoSenseScenario;
begin
  scenario := CreateExpoSenseScenario('demo track'{, guid, lts, uts});
  scenarios.Add(scenario.ID, scenario);
  fProjectCurrentScenario := scenario;
end;

end.

