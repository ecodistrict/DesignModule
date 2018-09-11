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

  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils,
  System.RegularExpressions;

const
  sensordata_longitude             = 129;               //tag 16
  sensordata_latitude              = 121;               //tag 15

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

  sensordata_transportationmode    = 2722;              // tag 340                  =

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

  meteodata_winddirection          = 129;
  meteodata_windspeed              = 137;

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

  chartValueFactor = 1.0e-6;

  ModalityPaletteJSON =
    '{"entries":['+
      '{"color":{"fill":"#2e8857"}, "minValue":1, "description":"Walking"},'+ // seaGreen
      '{"color":{"fill":"#9370db"}, "minValue":2, "description":"Bicycle"},'+ // MediumPurple
      '{"color":{"fill":"#b22222"}, "minValue":3, "description":"Motorized"},'+ // Crimson
      '{"color":{"fill":"#2f4f4f"}, "minValue":4, "description":"Other"}'+ // DarkSlateGray
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
  constructor Create(aTimeStamp: TDateTime; aLat, aLon: Double; aValue, aHeight: Double);
  destructor Destroy; override;
  private
    fGeometry: TWDGeometryPoint;
    fValue: Double;
    fHeight: Double;
  public
    property geometry: TWDGeometryPoint read fGeometry;
    property value: Double read fValue;
    property height: Double read fHeight;
  end;

  TExpoSenseHeightTrackLayer = class(TLayerBase)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aValueHeightFactor: Double;
    aDefaultLoad: Boolean=False; aShowInDomains: Boolean=True);
  destructor Destroy; override;
  protected
    fValueHeightFactor: Double;
    //fDataPoints: TObjectDictionary<TGUID, TOrderedListTS<TExpoSenseDataPoint>>; // owns
    fTilerLayer: TTilerLayer;
    // timers
    fPreviewRequestTimer: TTimer;
    fSendRefreshTimer: TTImer;
    function getJSON: string; override;
  public
    procedure Update(aStart, aEnd: TDateTime);
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aValue: Double; aMode: Integer);
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
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aMode: Integer);
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
    procedure AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aPrevLat, aPrevLon: Double; aMode: Integer);
    procedure reset();
    procedure RegisterLayer; override;
    procedure handleRefreshTrigger(aTimeStamp: TDateTime);
    function uniqueObjectsTilesLink: string;

    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  end;

  TExpoSenseScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView{;
    aGUID: TGUID; aLowerTimeStamp, aUpperTimeStamp: Double});
  destructor Destroy; override;
  private
    fHeightTrackLayer: TExpoSenseHeightTrackLayer;
    fDotTrackLayer: TExpoSenseDotTrackLayer;
    fLineTrackLayer: TExpoSenseLineTrackLayer ;
    fLastLats: TDictionary<TGUID, Double>;
    fLastLons: TDictionary<TGUID, Double>;
    fPrevLats: TDictionary<TGUID, Double>;
    fPrevLons: TDictionary<TGUID, Double>;
    //fSensorsDataSet: TSensorsDataSet;
    fTimeSliderDataTimer: TTimer;
    fFirstTimeSliderUpdate: Boolean;
    fMeasuredECValuesChart: TChartLines;
    fCalculatedECValuesChart: TChartLines;
    fTotalChart: TChartLines;
    fFiltered: Boolean;
    fShowDataSelectionTimer: TTimer;
    fDataPoints: TObjectDictionary<TGUID, TOrderedListTS<TExpoSenseDataPoint>>;
    fIMB3Connection: TIMBConnection; //owns
    procedure triggerUpdateTimesliderData;
    function jsonTimesliderData(aPalette: TWDPalette; var aExtent: TWDExtent): string;
    //procedure ShowDataSelection(aTrackLayer: TExpoSenseTrackLayer; aMobileChart, aTotalChart: TChartLines; aFrom, aTo: TDateTime);
  protected
    //fGUID: TGUID;
    //fQueryCounter: Integer;
    //fQuerySubscribed: Boolean;
    fPubEvent: TEventEntry;
    //fQueryEvent: TEventEntry;
    fLiveEvent: TEventEntry;
    //fQueryEventHandler: TOnEvent;
    fLiveEventHandler: TOnEvent;
    fLiveCounter: Integer;
    fDBCounter: Integer;
    fLastClient: TDateTime;
    procedure handleLiveEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    //procedure handleQueryEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleUniEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    //procedure addToSensorDataSet(const aSensorid: TGUID; aTimestamp: TDateTime; aFieldInfo: UInt32; aLat, aLon, aValue: Double);
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
    procedure HandleTimeSliderEvent(aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure HandleScenarioRefresh(aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure AddPoint(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aValue, aHeight: Double);
    procedure Update(aStart, aEnd: TDateTime; aMode: Integer);
    {
    procedure ProcessRecord(
      aTrackLayer: TExpoSenseTrackLayer; aMobileChart, aTotalChart: TChartLines;
      aCursor: TCursor;
      var aLat, aLon: Double;
      var aTimeStamp: TDateTime;
      var aValue, aTotalValue: Double);
    }
  public
    //procedure InquireDB(const aInquire: string; const aLowerTimestamp, aUpperTimestamp: Double);
    procedure ReadBasicData(); override;
  end;

  TExpoSenseProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
  protected
    function handleTilerStatus(aTiler: TTiler): string;
    procedure handleNewClient(aClient: TClient); override;
    procedure handleRemoveClient(aClient: TClient); override;
  public
    //function addClient(const aClientID: string): TClient; override;
    function CreateExpoSenseScenario(const aScenarioID: string{; aGUID: TGUID; aLowerTimeStamp, aUpperTimeStamp: Double}): TExpoSenseScenario;
  end;

implementation

//convert a Guid to TWDID
function GuidToTWDID(aGuid: TGUID): TWDID;
begin
  SetLength(Result, SizeOf(aGuid));
  Move(aGuid, PAnsiChar(Result)^, SizeOf(aGuid));
end;

// Discrete palette for the ExpoSense project

function CreateColorTrackPalette(const aTitle: string): TWDPalette;
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

function CreateGrayTrackPalette(const aTitle: string): TWDPalette;
var
  factor: Double;
begin
  factor := 1 / 1000000000;
  Result := TRampPalette.Create(aTitle, [
    TRampPaletteEntry.Create($FFE0E0E0, 0 * factor, '0'),
    TRampPaletteEntry.Create($FFA8A8A8, 30 * factor, '30'),
    TRampPaletteEntry.Create($FF707070, 60 * factor, '60'),
    TRampPaletteEntry.Create($FF383838, 90 * factor, '90'),
    TRampPaletteEntry.Create($FF000000, 120 * factor, '120')],
      $FFDDDDDD,
      $00000000,
      $FF000000);
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
  {
  Result := TRampPalette.Create(aTitle, [
    TRampPaletteEntry.Create($FFDDDDDD, 0 * factor, '0'),
    TRampPaletteEntry.Create($FF000000, 120 * factor, '120')],
      $FFDDDDDD,
      $00000000,
      $FF000000);
  }
end;

{ TExpoSenseDataPoint }

constructor TExpoSenseDataPoint.Create(aTimeStamp: TDateTime; aLat, aLon, aValue, aHeight: Double);
begin
  inherited Create(aTimeStamp);
  fGeometry := TWDGeometryPoint.Create(aLon, aLat, double.NaN);
  fValue := aValue;
  fHeight := aHeight;
end;

destructor TExpoSenseDataPoint.Destroy;
begin
  FreeAndNil(fGeometry);
  inherited;
end;

{ TExpoSenseTrackLayer }

procedure TExpoSenseHeightTrackLayer.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aValue: Double; aMode: Integer);
var
  geometryPoint: TWDGeometryPoint;
begin
  geometryPoint := TWDGeometryPoint.Create(aLon, aLat, Double.NaN);
  try
    fTilerLayer.signalData(
      TByteBuffer.bb_tag_guid(icehObjectID, aSensorID)+
      TByteBuffer.bb_tag_rawbytestring(icehTilerGeometryPoint, geometryPoint.Encode)+
      TByteBuffer.bb_tag_double(icehTilerValue, aMode)+
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
{
procedure TExpoSenseTrackLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500, fPalette.Clone);
end;

procedure TExpoSenseTrackLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice();
end;

procedure TExpoSenseTrackLayer.reset;
begin
  ClearObjects;
  tilerLayer.signalSliceAction(tsaClearSlice);
end;

function TExpoSenseTrackLayer.SliceType: Integer;
begin
  Result := stGeometryIH;
end;
}

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
  // handle subscribe on dependent diff layers
  {
  TMonitor.Enter(fDependentDiffLayers);
  try
    for diffLayer in fDependentDiffLayers
    do diffLayer.HandleClientSubscribe(aClient);
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
  }
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

{ TExponsenseDotTrackLayer }

procedure TExpoSenseDotTrackLayer.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon: Double; aMode: Integer);
var
  geometryPoint: TWDGeometryPoint;
  id: TWDID;
begin
  geometryPoint := TWDGeometryPoint.Create(aLon, aLat, Double.NaN);
  try
    id := TByteBuffer.bb_bytes(aSensorID, SizeOf(aSensorID))+TByteBuffer.bb_bytes(aTimeStamp, SizeOf(aTimeStamp));
    fTilerLayer.signalData(
      TByteBuffer.bb_tag_rawbytestring(icehTilerGeometryPoint, geometryPoint.Encode)+
      TByteBuffer.bb_tag_double(icehTilerValue, aMode)+
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

{ TExpoSenseLineTrackLayer }

procedure TExpoSenseLineTrackLayer.AddValue(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aPrevLat, aPrevLon: Double; aMode: Integer);
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
      TByteBuffer.bb_tag_double(icehTilerValue, aMode)+
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

{ TExpoSenseScenario }

procedure TExpoSenseScenario.AddPoint(const aSensorID: TGUID; aTimeStamp: TDateTime; aLat, aLon, aValue, aHeight: Double);
var
  ol: TOrderedListTS<TExpoSenseDataPoint>;
  edp: TExpoSenseDataPoint;
begin
  // todo: implement
  if not fDataPoints.TryGetValue(aSensorID, ol) then
  begin
    ol := TOrderedListTS<TExpoSenseDataPoint>.Create();
    fDataPoints.AddOrSetValue(aSensorID, ol);
  end;
  edp := TExpoSenseDataPoint.Create(aTimeStamp, aLat, aLon, aValue, aHeight);
  ol.AddTS(edp);
end;

constructor TExpoSenseScenario.Create(aProject: TProject; const aID, aName,
  aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView{;
  aGUID: TGUID; aLowerTimeStamp, aUpperTimeStamp: Double});
begin
  //if TRegEx.IsMatch(aID, '^[{][0-9A-Fa-f]{8}[-]([0-9A-Fa-f]{4}[-]){3}[0-9A-Fa-f]{12}[}]$')
  //then fGUID := TGUID.Create(aID)
  //else fGUID := TGUID.Empty;
  //fGUID := aGUID;
  //fLive := True;
  //fQueryCounter := 0;
  //fQuerySubscribed := False;
  //fQueryEventHandler := handleQueryEvent;
  fLiveCounter := 0;
  fDBCounter := 0;
  //fTrackLayer := TExpoSenseTrackLayer.Create;
  fDataPoints := TObjectDictionary<TGUID, TOrderedListTS<TExpoSenseDataPoint>>.Create([doOwnsValues]);
  fLastLats := TDictionary<TGUID, Double>.Create;
  fLastLons := TDictionary<TGUID, Double>.Create;
  fPrevLats := TDictionary<TGUID, Double>.Create;
  fPrevLons := TDictionary<TGUID, Double>.Create;
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
  {
  ltst := GetSetting('LowerTimeStamp', '');
  if ltst<>''
  then lts := StrToDateTime(ltst)
  else lts := Double.NaN;
  utst := GetSetting('UpperTimeStamp', '');
  if utst<>''
  then uts := StrToDateTime(utst)
  else uts := Double.NaN;
  InquireDB('', lts, uts);
  }
  //InquireDB('', aLowerTimeStamp, aUpperTimeStamp);
end;

destructor TExpoSenseScenario.Destroy;
begin
  {
  if fQuerySubscribed then //unsubscribe from the previous returnEvent
  begin
    fqueryEvent.OnEvent.Remove(fQueryEventHandler);
    project.Connection.unSubscribe(fQueryEvent);
  end;
  }
  fLiveEvent.OnEvent.Remove(fLiveEventHandler);
  project.Connection.unSubscribe(fLiveEvent);
  project.Connection.unPublish(fPubEvent);
  CancelTimer(fTimeSliderDataTimer);
  CancelTimer(fShowDataSelectionTimer);
  //FreeAndNil(fTrackLayers);
  FreeAndNil(fLastLons);
  FreeAndNil(fLastLats);
  FreeAndNil(fPrevLats);
  FreeAndNil(fPrevLons);
  FreeAndNil(fDataPoints);
  //FreeAndNil(fSensorsDataSet);
  inherited;
end;

function TExpoSenseScenario.HandleClientSubscribe(aClient: TClient): Boolean;
var
  jsonTSData: string;
  palette: TWDPalette;
  extent: TWDExtent;
begin
  Result := inherited;
  // send data to time slider
  palette := CreateGraySliderPalette('EC slider');
  try
    extent := TWDExtent.Create;
    jsonTSData := jsonTimesliderData(palette, extent);
  finally
    palette.Free;
  end;
  aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
  // set map view according data set
  if not (extent.CenterY.IsNaN or extent.CenterX.IsNaN) then
  begin
    fMapView := TMapView.Create(extent.CenterY, extent.CenterX, fMapView.zoom);
    aClient.SendView(fMapView.lat, fMapView.lon, Double.NaN);
  end;
  //fMobileChart.sub
  aClient.SubscribeTo(fMeasuredECValuesChart, nil);
  aClient.SubscribeTo(fCalculatedECValuesChart, nil);
  aClient.SubscribeTo(fTotalChart, nil);
  //aClient.SubscribeTo(fTrackLayer);
end;

function TExpoSenseScenario.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
  aClient.UnsubscribeFrom(fMeasuredECValuesChart);
  aClient.UnsubscribeFrom(fCalculatedECValuesChart);
  aClient.UnsubscribeFrom(fTotalChart);
  //aClient.UnsubscribeFrom(fTrackLayer);
end;

procedure TExpoSenseScenario.handleLiveEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
begin
  fLiveCounter := fLiveCounter + 1;
  handleUniEvent(aEventEntry, aPayload, aCursor, aLimit);
end;

{
procedure TExpoSenseScenario.handleQueryEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
begin
  fDBCounter := fDBCounter + 1;
  handleUniEvent(aEventEntry, aPayload, aCursor, aLimit);
end;
}
(*
procedure TExpoSenseScenario.ProcessRecord(
  aTrackLayer: TExpoSenseTrackLayer; aMobileChart, aTotalChart: TChartLines;
  aCursor: TCursor;
  var aLat, aLon: Double;
  var aTimeStamp: TDateTime;
  var aValue, aTotalValue: Double);
var
  srp: TPair<TSensor, Integer>;
  sr: TSensorsRecord;
  sensorValue: Double;
  _totalValue: Double;
  average: Double;
  delta: Double;
begin
  for srp in aCursor.SensorRecords do
  begin
    sr := fSensorsDataSet.Data[srp.Value];
    sr.values[srp.Key].TryGetValue(sensordata_latitude shr 3, aLat);
    sr.values[srp.Key].TryGetValue(sensordata_longitude shr 3, aLon);
    if not(aLat.IsNan or aLon.IsNan) then
    begin
      if sr.values[srp.Key].TryGetValue(sensordata_ec{sensordata_no2} shr 3, sensorValue) then
      begin
        //aTrackLayer.AddPoint(aCursor.CurrentTimeStamp{srp.Key.IDAsGUID}, aLat, aLon, sensorValue);
        aMobileChart.AddValue(aCursor.CurrentTimeStamp, [sensorValue]);
        if not aValue.IsNan then
        begin
          average := (aValue+sensorValue)/2.0;
          delta := aCursor.CurrentTimeStamp-aTimeStamp;
          _totalValue := average * delta * 24 * 60;
          if aTotalValue.IsNan
          then aTotalValue := _totalValue
          else aTotalValue := aTotalValue + _totalValue;
          aTotalChart.AddValue(aCursor.CurrentTimeStamp, [aTotalValue]);
        end;
        aValue := sensorValue;
        aTimeStamp := aCursor.CurrentTimeStamp;
      end;
    end;
  end;
end;
*)
(*
procedure TExpoSenseScenario.ShowDataSelection(aTrackLayer: TExpoSenseTrackLayer; aMobileChart, aTotalChart: TChartLines; aFrom, aTo: TDateTime);
//var
//  cursor: TCursor;
//  loopLat: Double;
//  loopLon: Double;
//  loopValue: Double;
//  loopTimeStamp: TDateTime;
//  loopTotalValue: Double;
begin
  // clear track and charts again to handle aborted jobs
  // todo: **** aTrackLayer.reset;
  aTotalChart.reset;
  aMobileChart.reset;
  // rebuild data
  aTrackLayer.reset;
  {
  cursor := fSensorsDataSet.NewCursor;
  try
    loopLat := Double.NaN;
    loopLon := Double.NaN;
    loopValue  := Double.NaN;
    loopTimeStamp := Double.NaN;
    loopTotalValue  := Double.NaN;
    TMonitor.Enter(fSensorsDataSet.Cursors);
    try
      if aFrom<>0 then
      begin
        // check if not before first entry
        if cursor.First and (aFrom<cursor.CurrentTimeStamp)
        then aFrom := cursor.CurrentTimeStamp;
        Log.WriteLn('HandleTimeSliderEvent: brush: '+DateTimeToStr(afrom)+' - '+DateTimeToStr(aTo));
        // goto start time and add points till end of data or over to-timestamp
        if cursor.MoveTo(aFrom) then
        begin
          repeat
            ProcessRecord(aTrackLayer, aMobileChart, aTotalChart, cursor, loopLat, loopLon, loopTimeStamp, loopValue, loopTotalValue);
          until (not cursor.Next) or (cursor.CurrentTimeStamp>aTo);
        end;
      end
      else
      begin // reset query
        Log.WriteLn('HandleTimeSliderEvent: brush: reset');
        // add all points
        if cursor.First then
        begin
          repeat
            ProcessRecord(aTrackLayer, aMobileChart, aTotalChart, cursor, loopLat, loopLon, loopTimeStamp, loopValue, loopTotalValue);
          until not cursor.Next;
        end;
      end;
    finally
      TMonitor.Exit(fSensorsDataSet.Cursors);
    end;
  finally
    fSensorsDataSet.RemoveCursor(cursor);
  end;
  }
end;
*)
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

(*
procedure TExpoSenseScenario.addToSensorDataSet(
  const aSensorid: TGUID; aTimestamp: TDateTime; aFieldInfo: UInt32; aLat, aLon, aValue: Double);
var
  id: TWDID;
  sensor: TSensor;
begin
  id := TByteBuffer.bb_bytes(aSensorid, SizeOf(aSensorid));
  TMonitor.Enter(fSensorsDataSet.Sensors);
  try
    if not fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
    begin
      sensor := TSensor.Create(id);
      fSensorsDataSet.Sensors.Add(id, sensor);
    end;
  finally
    TMonitor.Exit(fSensorsDataSet.Sensors);
  end;
  TMonitor.Enter(fSensorsDataSet.Data);
  try
    fSensorsDataSet.NewValues(sensor, aTimestamp,
      [sensordata_latitude shr 3, sensordata_longitude shr 3, aFieldInfo shr 3],
      [aLat, aLon, aValue]);
  finally
    TMonitor.Exit(fSensorsDataSet.Data);
  end;
end;
*)

procedure TExpoSenseScenario.handleUniEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
//  trackLayer: TExpoSenseTrackLayer;
  fieldInfo: UInt32;
  value, timestamp: Double;
  sensorid: TGUID;
  lastLat: Double;
  lastLon: Double;
  average: Double;
  delta: Double;
  _totalValue: Double;
  modeStr: string;
  mode: Integer;
  sliderTime: string;
  prevLon: Double;
  prevLat: Double;
begin
  timestamp := 0;
  mode := 3; // todo: indetermined?
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      wdatTimeStamp:
        begin
          timestamp := aPayload.bb_read_double(aCursor);
          if not fFiltered then
          begin
            sliderTime := FormatDateTime(publisherDateTimeFormat, timestamp);
            forEachSubscriber<TClient>(
              procedure(aClient: TClient)
              begin
                aClient.signalString('{"type":"timesliderEvents","payload":{"setCurrentTime":"'+sliderTime+'"}}');
              end);
          end;
        end;
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          sensorid := aPayload.bb_read_guid(aCursor);
        end;
      sensordata_transportationmode:
        begin
          modeStr := aPayload.bb_read_string(aCursor);
          // todo: derive mode of mode string
          // mode :=
          // todo: change all values in track from this time to the end to this mode
          Update(timeStamp, Double.PositiveInfinity, mode);
          triggerUpdateTimesliderData();
          if not fFiltered then
          begin
            fHeightTrackLayer.Update(timeStamp, Double.PositiveInfinity);
            fDotTrackLayer.Update(timeStamp, Double.PositiveInfinity);
            fLineTrackLayer.Update(timeStamp, Double.PositiveInfinity);
          end;
        end;
      sensordata_ec:
        begin
          value := aPayload.bb_read_double(aCursor);
          // correct for negative sensor values (just ignore)
          if value<0
          then value := 0;
          if fLastLons.TryGetValue(sensorid, lastLon) and fLastLats.TryGetValue(sensorid, lastLat) then
          begin
            //addToSensorDataSet(sensorid, timestamp, fieldInfo, lastLat, lastLon, value);
            AddPoint(sensorid, timeStamp, lastLat, lastLon, mode, value);
            triggerUpdateTimesliderData();
            if not fFiltered then
            begin
              fHeightTrackLayer.AddValue(sensorid, timeStamp, lastLat, lastLon, value, mode);
              fDotTrackLayer.AddValue(sensorid, timeStamp, lastLat, lastLon, mode);
              if fPrevLons.TryGetValue(sensorid, prevLon) and fPrevLats.TryGetValue(sensorid, prevLat)
              then fLineTrackLayer.AddValue(sensorid, timestamp, lastLat, lastLon, prevLat, prevLon, mode);
              if not lastLon.isNaN
              then fPrevLons.AddOrSetValue(sensorid, lastLon);
              if not lastLat.isNaN
              then fPrevLats.AddOrSetValue(sensorid, lastLat);
              fMeasuredECValuesChart.AddValue(timeStamp, [value*chartValueFactor, value*chartValueFactor*1.1]); // todo: measured value
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
            end;

          end;
        end;

      {
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
      }
      sensordata_latitude:
        begin
          value := aPayload.bb_read_double(aCursor);
          fLastLats.AddOrSetValue(sensorid, value);
        end;
      sensordata_longitude:
        begin
          value := aPayload.bb_read_double(aCursor);
          fLastLons.AddOrSetValue(sensorid, value);
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

(*
procedure TExpoSenseScenario.InquireDB(const aInquire: string; const aLowerTimestamp, aUpperTimestamp: Double);
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
  if (fProject is TExpoSenseProject) and not aLowerTimeStamp.IsNan then
  begin
    buffer := TByteBuffer.bb_tag_guid(icehObjectID, fGUID.Empty); //expert scenario, send empty guid to access all data
    Log.WriteLn('Inquire expert scenario');
  end
  else if fGUID <> TGUID.Empty then //check if's not the empty guid
    buffer := TByteBuffer.bb_tag_guid(icehObjectID, fGUID)
  else //constant non-empty Guid, prevents people using the empty guid to access all data
    buffer := TByteBuffer.bb_tag_guid(icehObjectID, TGUID.Create('{00000000-0000-0000-0000-000000000001}'));
  if not aLowerTimeStamp.IsNaN
  then buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampLower shr 3, aLowerTimeStamp);
  if not aUpperTimeStamp.IsNan
  then buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampUpper shr 3, aUpperTimeStamp);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, returnString);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aInquire);
  fPubEvent.signalEvent(buffer);
end;
*)

function TExpoSenseScenario.jsonTimesliderData(aPalette: TWDPalette; var aExtent: TWDExtent): string;
var
  c: Integer;
  oneList: TOrderedListTS<TExpoSenseDataPoint>;
  sensor: TPair<TGUID, TOrderedListTS<TExpoSenseDataPoint>>;
  edp: TExpoSenseDataPoint;
//  cursor: TCursor;
//  srp: TPair<TSensor, Integer>;
//  sensorValue: Double;
//  sr: TSensorsRecord;
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
        oneList.AddTS(TExpoSenseDataPoint.Create(edp.timeStamp, edp.geometry.y, edp.geometry.x, edp.value, edp.height));
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
  (*
  cursor := fSensorsDataSet.NewCursor;
  try
    TMonitor.Enter(fSensorsDataSet);
    try
      entryStartTimeStamp := Double.NaN;
      entryColor := 0;
      prevTimeStamp := Double.NaN;
      if cursor.First then
      begin
        repeat
          // init step values
          loopSensorValue := Double.NaN;
          loopLat := Double.NaN;
          loopLon := Double.NaN;
          for srp in cursor.SensorRecords do
          begin
            sr := fSensorsDataSet.Data[srp.Value];
            // todo: check if sr has always entry for sensor (iesrp.key)?
            if (cursor.CurrentTimeStamp-sr.timeStamp<=MaxNoSensorValueTime) and
               sr.values[srp.Key].TryGetValue(aTag, sensorValue) then
            begin
              // check if value is not too old and if higher then
              if loopSensorValue.IsNaN or (loopSensorValue<sensorValue) then
              begin
                loopSensorValue := sensorValue;
                sr.values[srp.Key].TryGetValue(sensordata_latitude shr 3, loopLat);
                sr.values[srp.Key].TryGetValue(sensordata_longitude shr 3, loopLon);
                if not (loopLat.IsNan or loopLon.IsNan)
                then aExtent.Expand(loopLon, loopLat);
              end;
            end;
          end;

          // color with value
          loopSensorValueColor := aPalette.ValueToColors(loopSensorValue).fillColor;

          // calculate length of step
          if Double(prevTimeStamp).IsNaN then
          begin
            // FIRST step, init entry
            stepSize := 0;
            entryStartTimeStamp := cursor.CurrentTimeStamp;
            entryColor := loopSensorValueColor;
          end
          else stepSize := cursor.CurrentTimeStamp-prevTimeStamp; // NOT first step

          // check if entry should be closed (added)
          if loopSensorValueColor<>entryColor then
          begin
            // check for transparancy
            if (entryColor and $FF000000)<>0 then
            begin
              // calculate end time for entry
              if stepSize>MaxNoSensorValueTime
              then entryEndTimeStamp := prevTimeStamp+MaxNoSensorValueTime
              else entryEndTimeStamp := cursor.CurrentTimeStamp;
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
            entryStartTimeStamp := cursor.CurrentTimeStamp;
            entryColor := loopSensorValueColor;
          end;
          // prepare next step
          prevTimeStamp := cursor.CurrentTimeStamp;
        until not cursor.Next;
        // add last step
        // check for transparancy
        if (entryColor and $FF000000)<>0 then
        begin
          entryEndTimeStamp := cursor.CurrentTimeStamp+MaxNoSensorValueTime;
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
      end;
    finally
      TMonitor.Exit(fSensorsDataSet);
    end;
  finally
    fSensorsDataSet.RemoveCursor(cursor);
  end;
  *)
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
begin
  fMeasuredECValuesChart :=  TChartLines.Create(Self, 'Personal exposure', 'Measured' + 'EC'+'Chart', 'Measured EC', 'Personal, measured EC exposure', True, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3'){,
      TChartAxis.Create('concentration', 'PaleVioletRed', 'Concentration', 'mg/m3')}],
    'time', 3);
  fMeasuredECValuesChart.chartUpdateTime := 2;
  AddChart(fMeasuredECValuesChart);

  fCalculatedECValuesChart :=  TChartLines.Create(Self, 'Personal exposure', 'Calculated' + 'EC'+'Chart', 'Calculated EC', 'Personal, Calculated EC exposure', True, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3'){,
      TChartAxis.Create('concentration', 'PaleVioletRed', 'Concentration', 'mg/m3')}],
    'time', 3);
  fCalculatedECValuesChart.chartUpdateTime := 2;
  AddChart(fCalculatedECValuesChart);

  fTotalChart := TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts' + 'EC' + 'total', 'EC' + '-total', 'Personal EC' + ' Total', False, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [ TChartAxis.Create('concentration', 'lightBlue', 'Concentration', 'mg/m3')], 'time', 3);
  fTotalChart.chartUpdateTime := 2;
  AddChart(fTotalChart);

  fHeightTrackLayer := TExpoSenseHeightTrackLayer.Create(
    Self, 'Personal exposure', 'EC' + 'personal-track-' + 'EC', 'Personal Track ' + 'EC', 'EC track',
    DefaultExpoSensoECValueToHeightFactor,
    False, True);
  AddLayer(fHeightTrackLayer);

  fDotTrackLayer := TExpoSenseDotTrackLayer.Create(
    Self, 'Personal exposure', 'EC' + 'personal-track-points-' + 'EC', 'Personal Track Points ' + 'EC', 'EC track points',
    True, True);
  AddLayer(fDotTrackLayer);

  fLineTrackLayer := TExpoSenseLineTrackLayer.Create(
    Self, 'Personal exposure', 'EC' + 'personal-track-lines-' + 'EC', 'Personal Track Lines ' + 'EC', 'EC track lines',
    True, True);
  AddLayer(fLineTrackLayer);

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
        for imlep in metalayer do
        begin
          if imlep.value._published>0 then
          begin
            layer := imlep.value.CreateUSLayer(
              self, tablePrefix, ConnectStringFromSession(oraSession),
              SubscribeUSDataEvents(USUserName, imlep.value.IMB_EVENTCLASS, USTablePrefix, fIMB3Connection),
              USSourceProjection, imlep.value.Domain, imlep.value.description);
            if Assigned(layer) then
            begin
              AddLayer(layer);
              // schedule reading objects and send to
              AddCommandToQueue(Self, (layer as TUSLayer).ReadObjects);
              Log.WriteLn('Added layer '+imlep.value.Domain+'\'+imlep.value.description);
            end
            else Log.WriteLn('Could not add US layer '+imlep.value.Domain+'\'+imlep.value.description, llError);
          end;
        end;
      end;
    finally
      metaLayer.Free;
    end;
  finally
    oraSession.Free;
  end;

end;

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
        palette := CreateGraySliderPalette('EC slider');
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
            if fFirstTimeSliderUpdate and not (extent.CenterY.IsNaN or extent.CenterX.IsNaN) then
            begin
              fMapView := TMapView.Create(extent.CenterY, extent.CenterX, fMapView.zoom);
              aClient.SendView(extent.CenterY, extent.CenterX, Double.NaN);
            end;
          end);
        if not (extent.CenterY.IsNaN or extent.CenterX.IsNaN)
        then fFirstTimeSliderUpdate := False;
      end);
  end;
end;

procedure TExpoSenseScenario.Update(aStart, aEnd: TDateTime; aMode: Integer);
begin
  // todo: implement
end;

{ TExpoSenseProject }

{
function TExpoSenseProject.addClient(const aClientID: string): TClient;
begin
  Result := TExpoSenseClient.Create(Self, fProjectCurrentScenario, fProjectRefScenario, aClientID);
  TMonitor.Enter(clients);
  try
    clients.Add(Result);
  finally
    TMonitor.Exit(clients);
  end;
end;
}
constructor TExpoSenseProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView);
var
  scenario: TExpoSenseScenario;
begin
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN,
    aTilerStatusURL, nil, aAddBasicLayers, aMaxNearestObjectDistanceInMeters, aMapView);
  fTiler.onTilerStatus := handleTilerStatus;
  //Set ExpoSense controls
  SetControl(timeSliderControl, '1');
  windControl; // init by first use
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
  scenario := CreateExpoSenseScenario('demo track'{, guid, lts, uts});
  scenarios.Add(scenario.ID, scenario);
  fProjectCurrentScenario := scenario;
end;

function TExpoSenseProject.CreateExpoSenseScenario(const aScenarioID: string{; aGUID: TGUID; aLowerTimeStamp, aUpperTimeStamp: Double}): TExpoSenseScenario;
begin
  Result := TExpoSenseScenario.Create(Self, aScenarioID, 'GPS tracks', 'Exposure on GPS tracks', False, MapView{,
    aGUID, aLowerTimeStamp, aUpperTimeStamp});
end;

function TExpoSenseProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

procedure TExpoSenseProject.handleNewClient(aClient: TClient);
begin
  // todo: ?
end;

procedure TExpoSenseProject.handleRemoveClient(aClient: TClient);
var
  scenario: TExpoSenseScenario;
begin
  if aClient.currentScenario is TExpoSenseScenario then
  begin
    scenario := aClient.currentScenario as TExpoSenseScenario;
    if Assigned(scenario) and (scenario.ClientAmount=0) then
    begin
      scenario.fLastClient := Now;
    end;
  end;
end;

{ TExpoSenseClient }
(*
constructor TExpoSenseClient.Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
begin
  inherited;
end;

procedure TExpoSenseClient.Login(aJSONObject: TJSONObject);
var
  scenarioID: string;
//  userID: string;
  scenario: TScenario;
//  userdef: string;
//  lts: Double;
//  uts: Double;
//  userdef_split: TArray<string>;
//  guid: TGUID;
begin
  //userID := aJSONObject.GetValue<string>('userid');
  scenarioID := aJSONObject.GetValue<string>('scenario'); //todo: check if scenarioID is valid GUID?

  if not TRegEx.IsMatch(scenarioID, '^[{][0-9A-Fa-f]{8}[-]([0-9A-Fa-f]{4}[-]){3}[0-9A-Fa-f]{12}[}]$') then
    exit;
  guid := TGUID.Create(scenarioID);
  lts := Double.NaN;
  uts := Double.NaN;
  userdef := StandardIni.ReadString('users', userID, '');
  if userdef<>'' then
  begin
    userdef_split := userdef.Split(['|']);
    if length(userdef_split)>=2 then
    begin
      lts := StrToDateTime(userdef_split[0]);
      uts := StrToDateTime(userdef_split[1]);
      scenarioID := scenarioID+userID;
      Log.WriteLn('EXPERT scenario: '+scenarioID+' '+lts.ToString+' <= ts <= '+uts.ToString, llWarning);
    end;
  end;

  TMonitor.Enter(fProject.scenarios);
  try
    if not fProject.scenarios.TryGetValue(scenarioID, scenario) then
    begin
      scenario := (fProject as TExpoSenseProject).CreateExpoSenseScenario(scenarioID{, guid, lts, uts});
      fProject.scenarios.Add(scenario.ID, scenario);
    end;
  finally
    TMonitor.Exit(fProject.scenarios);
  end;
  removeClient(fCurrentScenario);
  fCurrentScenario := scenario;
  addClient(fCurrentScenario);
  //Log.WriteLn('connected to scenario '+scenarioID+' user '+userid);
  SendSession();
  fProject.SendDomains(self, 'domains');
end;
*)

end.

