unit PublishServerSantos;

// TODO -oPW onclick TSimpleObject?

// Legend?
// show graph via Event on ChargeLoc?

interface

uses
  Logger,
  StdIni,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  MyOraLib,
  MyStr,

  ODBFiles2,

  GISCSSystems,
  GisTypes,

  WorldDataCode,
  WorldLegends,

  IMB3NativeClient,

  imb4,
  WorldTilerConsts,
  CommandQueue,
  TimerPool,

  ModelControllerLib,

  NDWLib,

  System.Generics.Collections,
  
  System.Classes,
  System.DateUtils,
  System.Hash,
  System.Math,
  System.JSON,
  System.SysUtils,

  PublishServerLib,
  PublishServerGIS,
  PublishServerUS,
  PublishServerMCLib;

const
  idPrefixBusStop = 'STP';
//  idPrefixChargeLocation = 'CRG' ;

  cINDIC_DAT_TYPE_BLOCK = '1';
  cINDIC_DAT_TYPE_BUSTYPE = '2';
  cINDIC_DAT_TYPE_GRID = '3';

  cINDIC_DAT_BLOCK_SOC = '01';
  cINDIC_DAT_BLOCK_CONSUMPTION = '02' ;

  cINDIC_DAT_BUSTYPE_DEPTH_DISCHARGE = '01';
  cINDIC_DAT_BUSTYPE_LINE_CONSUMPTION = '02';
  cINDIC_DAT_BUSTYPE_POWER_CONSUMPTION = '03';
  cINDIC_DAT_BUSTYPE_POWER_CONSUMTION_DIST = '04';
  cINDIC_DAT_BUSTYPE_RIDE_POWER_CONSUMPTION = '05';
  cINDIC_DAT_BUSTYPE_RIDE_AVG_MINMAX = '06';

  cINDIC_DAT_GRID_PEAKPOWER = '01';
  cINDIC_DAT_GRID_TOTALPOWER = '02';
  cINDIC_DAT_GRID_PEAK_BUSSES = '03';
  cINDIC_DAT_GRID_PEAK_BUSSES_WARN = '04';

  tagEditChargeLocation = 'EDTCHG';
  tagEditBusParameters = 'EDTBUSPARAM';

  sNoRoute = 'NONE/UNKNOWN';

type
  (*
  TBusStop = record // Todo based on US/Santos data
    id: string;
    name: string;
    lat, lon: double;
    chargePoleType: string;
    numberOfPoles: Integer;
    maxPower: Integer;
    objectID: Integer;
    function location: string;
    function tooltip: string;
    function geoColors(aFill:Cardinal=colorBasicFill): TGeoColors;
    function isCharger: boolean;
  end;

  TTimeStop = record
    timeStart: TDateTime;
    timeStop: TDateTime;
    soc: double;
    stop: string;
    name: string;
    routeDir: string;
    function tooltip(stop: TBusStop): String;
  end;
  *)

  TCharger = class
  constructor Create(aObjectID: Integer; const aPoleType: string; aNumberOfPoles: Integer; aMaxPower: Double);
  public
    objectID: Integer;
    poleType: string;
    numberOfPoles: Integer;
    maxPower: Double;
  end;

  TBusStop2 = class
  constructor Create(aLat, aLon: Double; const aID, aName: string; aNodeObjectID: Integer; aCharger: TCharger);
  destructor Destroy; override;
  public
    lat: Double;
    lon: Double;
    id: string;
    name: string;
    nodeObjectID: Integer;
    charger: TCharger; // owned
  end;

  TBusDataEntry = class
  constructor Create(aArrivalTime: TDateTime; const aTripName: string; const aStopID: string; aSoc: Double);
  public
    arrivalTime: TDateTime;
    tripName: string;
    stopID: string;
    soc: Double;
  end;

  TBusTimeTable = TObjectDictionary<TDateTime, TBusDataEntry>;

  TBusData = class
  constructor Create;
  destructor Destroy; override;
  private
    fBusRecords: TObjectDictionary<Integer, TBusTimeTable>;
  public
    property BusRecords: TObjectDictionary<Integer, TBusTimeTable> read fBusRecords;
  end;

  TBusState = class
  constructor Create(const aTripName: string; aSoc: Double);
  public
    tripName: string;
    soc: Double;
  end;

  TBussesRecords = TDictionary<Integer, TBusDataEntry>; // refs

  TSliderRecord = class
  constructor Create;
  destructor Destroy; override;
  private
    fBusStops: TObjectDictionary<TBusStop2, TBussesRecords>; // owns TBussesRecords but that contains refs to TBusDataEntry
  public
    property BusStops: TObjectDictionary<TBusStop2, TBussesRecords> read fBusStops;
    function MinSoc: Double;
  end;

  TSliderData = class
  constructor Create;
  destructor Destroy; override;
  private
    fBusStops: TObjectDictionary<string, TBusStop2>; // owns
    fTimeTable: TObjectDictionary<TDateTime, TSliderRecord>;
  public
    property BusStops: TObjectDictionary<string, TBusStop2> read fBusStops; // stop_id -> info on bus stop
    property TimeTable: TObjectDictionary<TDateTime, TSliderRecord> read fTimeTable;
  end;

  TSantosLayer = class(TSimpleLayer)
    constructor Create(aScenario: TScenario; aBusBlock: Integer;
        aBaseDay, aBaseMonth, aBaseYear: Word;
        const aDomain, aID, aName, aDescription, aConnectString, aTablePrefix: string;
        aPubEntry: TIMBEventEntry; aIndicEntry, aSocEntry: TIMBEventEntry);
    destructor Destroy; override;
  private
    fEventEntry: TIMBEventEntry;
  protected
    //fBusStops: TDictionary<String, TBusStop>;
    fStopObjects: TList<TSimpleObject>;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem; // ref
    //fTimeTable: TList<TTimeStop>;
    fTablePrefix: String;
    fBlockID: Integer;
    fSoCPalette: TRampPalette;
    fTimeSliderTimer: TTimer;
    fBaseDay: array of word; // [d,m,y]
    fChargerTypes: TStrings;
    fConnectString: string;
    fUpdateTimer: TTimer;
    fLastUpdate: THighResTicks;
    fCurrentTime: TDateTime;

    // new
    fBusData: TBusData;
    fSliderData: TSliderData;

    procedure initStops(aSession: TOraSession);
    procedure initTimeTable(aSession: TOraSession);
    procedure initChargerTypes(aSession: TOraSession);

    procedure readBusData(aSession: TOraSession; const aTablePrefix: string);
    procedure readSliderData(aSession: TOraSession; const aTablePrefix: string);
    procedure addDrivingBussesToPreviousStop;

    procedure updateStop(const aID: string; aStop: TBusStop2; aObject: TSimpleObject);
    procedure showStops;
    procedure updateStopsBase;
    procedure UpdateStopsOnTime(aTime: TDateTime);

    procedure formEditChargeLocation(const aChargeLocation: string; aClient: TClient);

    function  jsonTimesliderData: String;
    procedure handleUpdateLayerObject(aClient: TClient; aPayload: TJSONObject); override;
    procedure HandleOnChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
    procedure HandleDataUpdate(aTimer: TTimer; aTime: THighResTicks);
  public
    procedure HandleSelectedEvent(aClient: TClient; aMessage: TJSONValue);
    procedure handleNewTime(aClient: TClient; aTime: string);
    function  HandleClientSubscribe(aClient: TClient): Boolean; override;
    procedure HandleFormResult(aFormResult: TJSONObject);
    procedure HandleTimeSliderEvent(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject);

    procedure SignalRecalc;
  end;

  TSantosProject = class(TUSProject)
    constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection;
                       const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
                       aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean;
                       aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string);
    destructor Destroy; override;
  private
    fTimeSliderTimer: TTimer;
    fCurrentBusBlock: Integer;
    fSantosLayers: TObjectDictionary<TScenario, TSantosLayer>; // ref
    fBaseDay: Array of Word;
    // base bus parameters
    fBatteryCapacity: Double;
    fAverageEnergyConsumption: Double;
    fDriverEfficiency: Double;
    fPowerConsumptionHVAC: Double;
    procedure LoadBusDefaults;
  protected
    function  ReadScenario(const aID: string): TScenario; override;
    procedure ReadChartBlockSoC(aSession: TOraSession; aScenario: TScenario; const aTablePrefix: string; aBlockID: Integer);

    procedure ReadChartChargerTotalPower(aSession: TOraSession; aScenario: TScenario; const aTablePrefix: string;
                                         aCharger: Integer; const aName: string = '');
    procedure ReadChartChargerPeakBusses(aSession: TOraSession; aScenario: TScenario; const aTablePrefix: string;
                                         aCharger: Integer; const aName: string = '');
    procedure ReadChartChargerWarnings(aSession: TOraSession; aScenario: TScenario; const aTablePrefix: string;
                                         aCharger: Integer; const aName: string = '');

  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
    procedure UpdateBusDefaults;
  end;

implementation

const
  // TODO: DB Legend? Ini?
  NoChargeColor: TAlphaRGBPixel = $FFFF0000;
  LowChargeColor: TAlphaRGBPixel = $FFFFA500;
  MediumChargeColor: TAlphaRGBPixel = $FFFFFF00;
  HighChargeColor: TAlphaRGBPixel = $FF008000;
  Nothing: TAlphaRGBPixel = $AAAAAAAA;

function SantosTimeToDateTime(const aTime: string; aBaseDay: Array of word): TDateTime;
var
  parts: TArray<String>;
  d, // amount of days to increase (h>24)
  h,m: Integer;
begin
  if aTime.IsEmpty then
    Result := EncodeDateTime(aBaseDay[2], aBaseDay[1], aBaseDay[0], 0, 0, 0, 0)
  else
  begin
    parts := aTime.Trim.Split([':']);
    if TryStrToInt(parts[0], h) and TryStrToInt(parts[1], m) then
    begin
      d := 0;
      while h>=24 do
      begin
        Inc(d);
        h := h-24;
      end;
      Result := EncodeDateTime(aBaseDay[2], aBaseDay[1], aBaseDay[0], h, m, 0, 0);
      if d>0 then
        IncDay(Result, d);
    end
    else
      Result := EncodeDateTime(aBaseDay[2], aBaseDay[1], aBaseDay[0], 0, 0, 0, 0);
  end;
end;

function MD5Sum(s: String): string;
begin
   Result := System.hash.THashMD5.GetHashString(s);
end;

{ web form helpers }

function openFormElement_input(const aID, aLabel, aType: string; aDefaultValue: string = ''; aRequired: boolean = true; aHidden: boolean = false): string;
var
  required: string;
  hidden: string;
  options: string;
begin
  if aRequired
  then required := 'y'
  else required := 'n';

  if aDefaultValue.IsEmpty
  then options := 'false'
  else options := '{ "defaultValue" : "'+aDefaultValue+'" }';

  if aHidden
  then hidden := 'true'
  else hidden := 'false';

  Result :=  '{'+
              '"formElement" : "input",'+
              '"type" : "'+aType+'",'+
              '"required" : "'+required+'",'+
              '"optionsArray" : false,'+
              '"labelText" : "'+aLabel+'",'+
              '"idName" : "'+aID+'",'+
              '"hidden" : '+hidden+','+
              '"extraOptions" : '+options+
            '}';
end;

function openFormElement_select(const aID, aLabel: string;
  aOptions: TArray<string>;
  const aDefaultValue: string = '';
  aRequired: boolean = true; aHidden: boolean = false): string;
var
  required: string;
  hidden: string;
  options: string;
  choices: string;
begin
  if aRequired
  then required := 'y'
  else required := 'n';

  if aDefaultValue.IsEmpty
  then options := 'false'
  else options := '{ "defaultValue" : "'+aDefaultValue+'" }';

  if aHidden
  then hidden := 'true'
  else hidden := 'false';

  choices  := '["' + String.Join('","', aOptions) + '"]';

  Result :=  '{'+
              '"formElement" : "select",'+
              '"type" : "string",'+
              '"required" : "'+required+'",'+
              '"optionsArray" : '+choices+','+
              '"labelText" : "'+aLabel+'",'+
              '"idName" : "'+aID+'",'+
              '"hidden" : '+hidden+','+
              '"extraOptions" : '+options+
            '}';
end;

{ TCharger }

constructor TCharger.Create(aObjectID: Integer; const aPoleType: string; aNumberOfPoles: Integer; aMaxPower: Double);
begin
  inherited Create;
  objectID := aObjectID;
  poleType := aPoleType;
  numberOfPoles := aNumberOfPoles;
  maxPower := aMaxPower;
end;

{ TBusStop2 }

constructor TBusStop2.Create(aLat, aLon: Double; const aID, aName: string; aNodeObjectID: Integer; aCharger: TCharger);
begin
  inherited Create;
  lat := aLat;
  lon := aLon;
  id := aID;
  name := aName;
  nodeObjectID := aNodeObjectID;
  charger := aCharger;
end;

destructor TBusStop2.Destroy;
begin
  FreeAndNil(charger);
  inherited;
end;

{ TBusRecord }

constructor TBusDataEntry.Create(aArrivalTime: TDateTime; const aTripName, aStopID: string; aSoc: Double);
begin
  inherited Create;
  arrivalTime := aArrivalTime;
  tripName := aTripName;
  stopID := aStopID;
  soc := aSoc;
end;

{ TBusData }

constructor TBusData.Create;
begin
  inherited Create;
  fBusRecords := TObjectDictionary<Integer, TBusTimeTable>.Create([doOwnsValues]);
end;

destructor TBusData.Destroy;
begin
  FreeAndNil(fBusRecords);
  inherited;
end;

{ TBusState }

constructor TBusState.Create(const aTripName: string; aSoc: Double);
begin
  inherited Create;
  tripName := aTripName;
  soc := aSoc;
end;

{ TSliderRecord }

constructor TSliderRecord.Create;
begin
  inherited Create;
  fBusStops := TObjectDictionary<TBusStop2, TDictionary<Integer, TBusDataEntry>>.Create;
end;

destructor TSliderRecord.Destroy;
begin
  FreeAndNil(fBusStops);
  inherited;
end;

function TSliderRecord.MinSoc: Double;
var
  brs: TBussesRecords;
  bde: TBusDataEntry;
begin
  // find min soc for this time
  Result := Double.NaN;
  for brs in fBusStops.Values do
  begin
    for bde in brs.Values do
    begin
      if Result.IsNan
      then Result := bde.soc
      else if Result>bde.soc
      then Result := bde.soc;
    end;
  end;
end;

{ TSliderData }

constructor TSliderData.Create;
begin
  inherited;
  fBusStops := TObjectDictionary<string, TBusStop2>.Create;
  fTimeTable := TObjectDictionary<TDateTime, TSliderRecord>.Create;
end;

destructor TSliderData.Destroy;
begin
  FreeAndNil(fBusStops);
  FreeAndNil(fTimeTable);
  inherited;
end;

{ TSantosProject }

constructor TSantosProject.Create(aSessionModel: TSessionModel;
  aConnection: {IMB4} TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aStartScenario: string);
var
  measureCat: TMeasureCategory;
begin
  fSantosLayers := TObjectDictionary<TScenario, TSantosLayer>.Create([]);
  fCurrentBusBlock := 2; // Param?

  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL,
                   aDataSource, aDBConnection, aMapView, aPreLoadScenarios , False, aMaxNearestObjectDistanceInMeters);

  EnableControl(modelControl);
  SetControl('timeslider', '1');

  fTimeSliderTimer := Timers.CreateInactiveTimer;
  fTimeSliderTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*0.3);

  clientMessageHandlers.Add('timeslider',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    var
      selectedTime: string;
      selectedEvent: TJSONValue;
      l: TSantosLayer;
    begin
      TMonitor.Enter(fSantosLayers);
      try
        if not fSantosLayers.TryGetValue(aClient.currentScenario, l) then
          l := nil;
      finally
        TMonitor.Exit(fSantosLayers);
      end;
      if assigned(l) then
      begin
        if aPayload.TryGetValue<string>('selectedTime', selectedTime) then
        begin
          fTimeSliderTimer.Arm(DateTimeDelta2HRT(0.1*dtOneSecond),
            procedure (aTimer: TTimer; aTime: THighResTicks)
            begin
              l.handleNewTime(aClient, selectedTime);
            end);
        end;
        if aPayload.TryGetValue<TJSONValue>('selectedEvent', selectedEvent) then
          l.HandleSelectedEvent(aClient, selectedEvent);
//        if aPayload.TryGetValue<boolean>('active', active) then
//        begin
//          // layer.live[aClient] := not active;
//        end;
//        if aPayload.TryGetValue<TJSONValue>('brush', brush) then
//        begin  // Time range selection
//          Log.WriteLn('brush: '+brush.toJSON, llWarning);
//        end;
      end;
    end);

  measureCat := TMeasureCategory.Create('bus', 'Bus', 'Adjust e-bus parameters', 'Adjust e-bus parameters', '', 'Set', 'BatteryCapacity, AverageEnergyConsumption,DriverEfficiency,PowerConsumptionHVAC', 1);
  Measures.Add(tagEditBusParameters, measureCat);
  EnableControl(measuresControl);

  LoadBusDefaults;

  ClientMessageHandlers.Add('measure',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    var
      formID: string;
    formTitle: string;
    _formElements: string;
    _openformdialog: string;
    begin
      // show form to edit bus parameters
      // ID for callback event
      formID := 'DefaultBus';
      // Title of form
      formTitle := 'Edit default bus parameters';
      // Create form elements
      _formElements :=
        openFormElement_input('BatteryCapacity', 'Battery capacity (0 - 600 kWh)', 'float', fBatteryCapacity.ToString(dotFormat))+','+
        openFormElement_input('AverageEnergyConsumption', 'Average energy consumption (1.0 - 2.0 kWh/km)', 'float', fAverageEnergyConsumption.ToString(dotFormat))+','+
        openFormElement_input('DriverEfficiency', 'Driver efficiency (90 - 125)', 'float', fDriverEfficiency.ToString(dotFormat))+','+
        openFormElement_input('PowerConsumptionHVAC', 'Power consumption HVAC (0 - 20 kW)', 'float', fPowerConsumptionHVAC.ToString(dotFormat));

      _openformdialog :=
        '{'+
          '"type" : "openformdialog",'+
          '"payload" : {'+
            '"id" : "'+formID+'",'+
            '"title" : "'+formTitle+'",'+
            '"data" : ['+_formElements+']'+
          '}'+
        '}';

      // Push json to client
      aClient.signalString(_openformdialog);
    end);

end;

destructor TSantosProject.Destroy;
begin
  FreeAndNil(fSantosLayers);
  inherited;
end;

procedure TSantosProject.handleClientMessage(aClient: TClient;
  aScenario: TScenario; aJSONObject: TJSONObject);
var
  formResult: TJSONObject;
  l: TSantosLayer;
begin
  inherited;
  if aJSONObject.TryGetValue<TJSONObject>('formResult', formResult) then
  begin
    TMonitor.Enter(fSantosLayers);
    try
      if fSantosLayers.TryGetValue(fProjectCurrentScenario, l) then
        l.handleFormResult(formResult);
    finally
      TMonitor.Exit(fSantosLayers);
    end;
  end;
end;

procedure TSantosProject.LoadBusDefaults;
var
  oraTable: TOraTable;
begin
  oraTable := TOraTable.Create(nil);
  try
    oraTable.Session := OraSession;
    oraTable.SQL.Text :=
      'SELECT BATTERYCAPACITYKWH, OEM_KWH_KM, FINALDRIVEEFFICIENCY, MAXHVACPOWER_KW '+
      'FROM EBUS_BUSINFO';
    oraTable.Execute;
    if not oraTable.Eof then
    begin
      fBatteryCapacity := oraTable.Fields[0].AsFloat;
      fAverageEnergyConsumption := oraTable.Fields[1].AsFloat;
      fDriverEfficiency := oraTable.Fields[2].AsFloat; // adjust
      fPowerConsumptionHVAC := oraTable.Fields[3].AsFloat;
    end
    else
      Log.WriteLn('NO bus defaults', llError);
  finally
    oraTable.Free;
  end;
end;

(*
procedure TSantosProject.HandleDataUpdate(aSession: TOrasession;
  aScenario: TScenario; aLayer: TSantosLayer; const aTablePrefix: string);
{ todo:
var
  stop: TPair<string, TBusStop>;
}
begin
  //ReadChartBlockSoC(aSession, aScenario, aTablePrefix, fCurrentBusBlock);
  {
  TMonitor.Enter(aLayer.fBusStops);
  try
    for stop in aLayer.fBusStops do
      if stop.Value.isCharger then
      begin
        ReadChartChargerTotalPower(aSession, aScenario, aTablePrefix, stop.Value.objectID, stop.Value.name);
        ReadChartChargerPeakBusses(aSession, aScenario, aTablePrefix, stop.Value.objectID, stop.Value.name);
        ReadChartChargerWarnings(aSession, aScenario, aTablePrefix, stop.Value.objectID, stop.Value.name);
      end;
  finally
    TMonitor.Exit(aLayer.fBusStops);
  end;
  }
end;
*)

procedure TSantosProject.ReadBasicData;
var
  y,m,d: Word;
begin
  setLength(fBaseDay, 3);
  DecodeDate(now, y,m,d);
  fBaseDay[0] := d;
  fBaseDay[1] := m;
  fBaseDay[2] := y;

  inherited;
end;

procedure TSantosProject.ReadChartBlockSoC(aSession: TOraSession; aScenario: TScenario; const aTablePrefix: string; aBlockID: Integer);
var
  zero: TDateTime;
  minOfDay: Integer;
  query: TOraQuery;
  tableName: String;
  isNew: Boolean;
  socChart: TChart;
  t: TDateTime;
  tableExists: boolean;
const
  SocChartID = 'santosSoc';
begin
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_BLOCK +
               aBlockID.ToString.PadLeft(3,'0') + cINDIC_DAT_BLOCK_SOC;

  tableExists := MyOraLib.tableExists(aSession, tableName);

  TMonitor.Enter(aScenario.Charts);
  try
    isNew := not aScenario.Charts.TryGetValue(SocChartID, socChart);
  finally
    TMonitor.Exit(aScenario.Charts);
  end;

  if isNew and tableExists then
  begin
    socChart := TChartLines.Create(aScenario, 'Santos' , SocChartID, 'State of Charge',
                 'State of Charge', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'{, True, 0, 36*60*60}),
              [ TChartAxis.Create('State of Charge (%)', 'lightBlue', 'Dimensionless', '%', True, 0, 100)]);

  end;

  if Assigned(socChart) then
  begin
    TMonitor.enter(socChart);
    try
      socChart.Show();
      socChart.description := 'State of Charge ('+aBlockID.ToString+')';
       // Clear
      if not isNew then
        socChart.reset;

      if tableExists then
      begin
        query := TOraQuery.Create(nil);
        try
          query.Session := aSession;
          query.SQL.Text := 'SELECT X,Y,LPAD(TRIM(TIME), 5, ''0'') as TIME ' +
                            'FROM ' + tableName + ' ORDER BY TIME ASC';
          query.Open;
            while not Query.Eof do
            begin
              t := SantosTimeToDateTime(Query.FieldByName('TIME').AsString, fBaseDay);
              minOfDay := MinutesBetween(zero, t) *60;
              (socChart as TChartLines).AddValue(
                minOfDay,
                [Query.FieldByName('Y').AsFloat/100]
              );
              Query.Next;
            end;

        finally
          query.Free;
        end;

        if isNew then
          aScenario.addChart(socChart);
      end;
    finally
      TMonitor.Exit(socChart);
    end;
  end;
end;

procedure TSantosProject.ReadChartChargerPeakBusses(aSession: TOraSession;
  aScenario: TScenario; const aTablePrefix: string; aCharger: Integer;
  const aName: string);
var
  zero, t: TDateTime;
  minOfDay: Integer;
  query: TOraQuery;
  tableName: String;
  isNew, tableExists: Boolean;
  chgPeakBussesChart: TChart;
  chartID: string;
  name: string;
const
  ChgPwrChartID = 'santosPeakBusses';
begin
  if aName.IsEmpty
  then name := ' Charge Location ' + aCharger.ToString
  else name := aName;
  {$IFDEF DEBUG}
  Log.WriteLn('Loading peak busses chart for: ' + name);
  {$ENDIF}
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(3,'0') + cINDIC_DAT_GRID_PEAK_BUSSES;

  tableExists := MyOraLib.TableExists(aSession, tableName);

  chartID := ChgPwrChartID + aCharger.ToString;

  TMonitor.Enter(aScenario.Charts);
  try
    isNew := not aScenario.Charts.TryGetValue(chartID, chgPeakBussesChart);
  finally
    TMonitor.Exit(aScenario.Charts);
  end;

  if isNew and tableExists then
  begin
    chgPeakBussesChart := TChartLines.Create(aScenario, 'Santos' , chartID, '' + name + ' Peak Busses',
                  'Charge Location ' + aCharger.ToString + ' Peak Busses', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'),
              [ TChartAxis.Create('Peak busses (-)', 'lightBlue', 'Dimensionless', '-')]);
  end;

  if Assigned(chgPeakBussesChart) then
  begin
    TMonitor.enter(chgPeakBussesChart);
    try
       // Clear
      if not isNew then
        chgPeakBussesChart.reset;

      if tableExists then
      begin
        query := TOraQuery.Create(nil);
        try
          query.Session := aSession;
          query.SQL.Text := 'SELECT LPAD(TRIM(X), 5, ''0'') as TIME,Y ' +
                            'FROM ' + tableName + ' ORDER BY TIME ASC';
          query.Open;
            while not Query.Eof do
            begin
              t := SantosTimeToDateTime(Query.FieldByName('TIME').AsString,fBaseDay);
              minOfDay := MinutesBetween(zero, t) *60;
              (chgPeakBussesChart as TChartLines).AddValue(
                minOfDay,
                [Query.FieldByName('Y').AsFloat]
              );
              Query.Next;
            end;

        finally
          query.Free;
        end;
      end;

      if isNew then
        aScenario.addChart(chgPeakBussesChart);
    finally
      TMonitor.Exit(chgPeakBussesChart);
    end;
  end;
end;

procedure TSantosProject.ReadChartChargerTotalPower(aSession: TOraSession; aScenario: TScenario;
  const aTablePrefix: string; aCharger: Integer; const aName: string = '');
var
  zero, t: TDateTime;
  minOfDay: Integer;
  query: TOraQuery;
  tableName: String;
  isNew, tableExists: Boolean;
  chgTotalPowerChart: TChart;
  chartID: string;
  name: string;
const
  ChgPwrChartID = 'santosChgPwr';
begin
  if aName.IsEmpty
  then name := ' Charge Location ' + aCharger.ToString
  else name := aName;
  {$IFDEF DEBUG}
  Log.WriteLn('Loading total power chart for: ' + name);
  {$ENDIF}
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(3,'0') + cINDIC_DAT_GRID_TOTALPOWER;

  tableExists := MyOraLib.TableExists(aSession, tableName);

  chartID := ChgPwrChartID + aCharger.ToString;

  TMonitor.Enter(aScenario.Charts);
  try
    isNew := not aScenario.Charts.TryGetValue(chartID, chgTotalPowerChart);
  finally
    TMonitor.Exit(aScenario.Charts);
  end;

  if isNew and tableExists then
  begin
    chgTotalPowerChart := TChartLines.Create(aScenario, 'Santos' , chartID, '' + name + ' Total Power',
                  'Charge Location ' + aCharger.ToString + ' Total Power', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'),
              [ TChartAxis.Create('Total Power (-)', 'lightBlue', 'Dimensionless', '-')]);
  end;

  if Assigned(chgTotalPowerChart) then
  begin
    TMonitor.enter(chgTotalPowerChart);
    try
       // Clear
      if not isNew then
        chgTotalPowerChart.reset;

      if tableExists then
      begin
        query := TOraQuery.Create(nil);
        try
          query.Session := aSession;
          query.SQL.Text := 'SELECT LPAD(TRIM(X), 5, ''0'') as TIME,Y ' +
                            'FROM ' + tableName + ' ORDER BY TIME ASC';
          query.Open;
            while not Query.Eof do
            begin
              t := SantosTimeToDateTime(Query.FieldByName('TIME').AsString,fBaseDay);
              minOfDay := MinutesBetween(zero, t) *60;
              (chgTotalPowerChart as TChartLines).AddValue(
                minOfDay,
                [Query.FieldByName('Y').AsFloat]
              );
              Query.Next;
            end;

        finally
          query.Free;
        end;
      end;

      if isNew then
        aScenario.addChart(chgTotalPowerChart);
    finally
      TMonitor.Exit(chgTotalPowerChart);
    end;
  end;
end;

procedure TSantosProject.ReadChartChargerWarnings(aSession: TOraSession;
  aScenario: TScenario; const aTablePrefix: string; aCharger: Integer;
  const aName: string);
var
  zero, t: TDateTime;
  minOfDay: Integer;
  query: TOraQuery;
  tableName: String;
  isNew, tableExists: Boolean;
  chgPeakBussesChart: TChart;
  chartID: string;
  name: string;
const
  ChgPwrChartID = 'santosNumberWarn';
begin
  if aName.IsEmpty
  then name := ' Charge Location ' + aCharger.ToString
  else name := aName;
  {$IFDEF DEBUG}
  Log.WriteLn('Loading peak busses warnings chart for: ' + name);
  {$ENDIF}
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(3,'0') + cINDIC_DAT_GRID_PEAK_BUSSES_WARN;

  tableExists := MyOraLib.TableExists(aSession, tableName);

  chartID := ChgPwrChartID + aCharger.ToString;

  TMonitor.Enter(aScenario.Charts);
  try
    isNew := not aScenario.Charts.TryGetValue(chartID, chgPeakBussesChart);
  finally
    TMonitor.Exit(aScenario.Charts);
  end;

  if isNew and tableExists then
  begin
    chgPeakBussesChart := TChartLines.Create(aScenario, 'Santos' , chartID, '' + name + ' Peak Busses Warnings',
                  'Charge Location ' + aCharger.ToString + ' peak busses warnings', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'),
              [ TChartAxis.Create('Peak Busses Warnings (-)', 'lightBlue', 'Dimensionless', '-')]);
  end;

  if Assigned(chgPeakBussesChart) then
  begin
    TMonitor.enter(chgPeakBussesChart);
    try
       // Clear
      if not isNew then
        chgPeakBussesChart.reset;

      if tableExists then
      begin
        query := TOraQuery.Create(nil);
        try
          query.Session := aSession;
          query.SQL.Text := 'SELECT LPAD(TRIM(X), 5, ''0'') as TIME,Y ' +
                            'FROM ' + tableName + ' ORDER BY TIME ASC';
          query.Open;
            while not Query.Eof do
            begin
              t := SantosTimeToDateTime(Query.FieldByName('TIME').AsString,fBaseDay);
              minOfDay := MinutesBetween(zero, t) *60;
              (chgPeakBussesChart as TChartLines).AddValue(
                minOfDay,
                [Query.FieldByName('Y').AsFloat]
              );
              Query.Next;
            end;

        finally
          query.Free;
        end;
      end;

      if isNew then
        aScenario.addChart(chgPeakBussesChart);
    finally
      TMonitor.Exit(chgPeakBussesChart);
    end;
  end;
end;

function TSantosProject.ReadScenario(const aID: string): TScenario;
var
  oraSession: TOraSession;
  tablePrefix, userName: string;
  santosLayer: TSantosLayer;
  indic_event: TIMBEventEntry;
  bs: TBusStop2;
  soc_event: TIMBEventEntry;
  //stop: TPair<string, TBusStop>;
begin
  Result := inherited; // TUSStuff
  if Assigned(Result) then
  begin
    TMonitor.Enter(fSantosLayers);
    try
      if not fSantosLayers.TryGetValue(Result, santosLayer) then
      begin
        // Scenario found, add to it
        oraSession := TOraSession.Create(nil);
        try
          oraSession.ConnectString := (self as TMCProject).controlInterface.DataSource;
          oraSession.Open;
          userName := oraSession.Username;
          tablePrefix := (Result as TUSScenario).Tableprefix;
//          GetScenarioTablePrefix(oraSession,  aID);
          ReadChartBlockSoC(oraSession, Result, tablePrefix, fCurrentBusBlock);

          indic_event := fIMB3Connection.Subscribe(userName +
            tableprefix.Substring(tableprefix.Length-1)+ // #
            tableprefix.Substring(0, tablePrefix.length-1)+
            '.EBUS_INDIC_DAT', False); // add with absolute path

          soc_event := fIMB3Connection.Subscribe(userName +
            tableprefix.Substring(tableprefix.Length-1)+ // #
            tableprefix.Substring(0, tablePrefix.length-1)+
            '.EBUS_SOC', False); // add with absolute path

          santosLayer := TSantosLayer.Create(Result, fCurrentBusBlock, fBaseDay[0], fBaseDay[1], fBaseDay[2], 'Santos', 'Santos'+fCurrentBusBlock.ToString,
              'Bus block '+fCurrentBusBlock.ToString, 'Bus block '+fCurrentBusBlock.ToString+' stops',
              (self as TMCProject).controlInterface.DataSource, tablePrefix, fIMB3Connection.Publish('EBUS_CHARGELOCATION'), indic_event, soc_event);
          if FileExists(ExtractFilePath(ParamStr(0))+'previews\Santos.png') then
            santosLayer.previewBase64 := PNGFileToBase64(ExtractFilePath(ParamStr(0))+'previews\Santos.png');
          TMonitor.Enter(santosLayer.fSliderData);
          try
            for bs in santosLayer.fSliderData.BusStops.Values do
            begin
              //if stop.Value.isCharger then
              if Assigned(bs.charger) and (bs.charger.numberOfPoles>0) then
              begin
                ReadChartChargerTotalPower(oraSession, Result, tablePrefix, bs.charger.objectID, bs.name);// stop.Value.objectID, stop.Value.name);
                ReadChartChargerPeakBusses(oraSession, Result, tablePrefix, bs.charger.objectID, bs.name);// stop.Value.objectID, stop.Value.name);
                ReadChartChargerWarnings(oraSession, Result, tablePrefix, bs.charger.objectID, bs.name);// stop.Value.objectID, stop.Value.name);
              end;
            end;
          finally
            TMonitor.Exit(santosLayer.fSliderData);
          end;
        finally
          oraSession.Free;
        end;

        Result.AddLayer(santosLayer);
        fSantosLayers.Add(Result, santosLayer);
      end;
    finally
      System.TMonitor.Exit(fSantosLayers);
    end;
  end;
end;

procedure TSantosProject.UpdateBusDefaults;
var
  sl: TSantosLayer;
begin
  Log.WriteLn('Bus defaults changed');
  OraSession.ExecSQL(
    'UPDATE EBUS_BUSINFO '+
    'SET BATTERYCAPACITYKWH='+fBatteryCapacity.ToString(dotFormat)+', '+
        'OEM_KWH_KM='+fAverageEnergyConsumption.ToString(dotFormat)+', '+
        'FINALDRIVEEFFICIENCY='+fDriverEfficiency.ToString(dotFormat)+', '+
        'MAXHVACPOWER_KW='+fPowerConsumptionHVAC.ToString(dotFormat));
  OraSession.Commit;
  // signal imb event on chargers so recalc is triggered
  TMonitor.Enter(fSantosLayers);
  try
    for sl in fSantosLayers.Values.ToArray do
    begin
      sl.SignalRecalc;
    end;
  finally
    TMonitor.Exit(fSantosLayers);
  end;
end;

{ TSantosLayer }

procedure TSantosLayer.addDrivingBussesToPreviousStop;
var
  ttt: TArray<TDateTime>;
  t: Integer;
  prevSL: TSliderRecord;
  curSL: TSliderRecord;
  prevBSBRP, curBSBRP: TPair<TBusStop2, TBussesRecords>;
  curBR: TBussesRecords;
  bbdep: TPair<Integer, TBusDataEntry>;
  curBusses: TDictionary<Integer, TBusStop2>;
  b: Integer;
begin
  Log.WriteLn('addDrivingBussesToPreviousStop');
  // add busses to slider data to inbetween time table entries
  // first get all times as list
  TMonitor.Enter(fSliderData);
  try
    ttt := fSliderData.fTimeTable.Keys.ToArray;
    TArray.Sort<TDateTime>(ttt);
    for t := 1 to length(ttt)-1 do
    begin
      // add bus info from previous step when not changed here
      fSliderData.fTimeTable.TryGetValue(ttt[t-1], prevSL);
      fSliderData.fTimeTable.TryGetValue(ttt[t], curSL);
      curBusses := TDictionary<Integer, TBusStop2>.Create;
      try
        // build list of current busses and stops they are at
        for curBSBRP in curSL.BusStops do
        begin
          for b in curBSBRP.Value.Keys.ToArray
          do curBusses.AddOrSetValue(b, curBSBRP.Key);
        end;
        // create entries for all busses at the previous time step that do not exist in this time step
        for prevBSBRP in prevSL.BusStops do
        begin
          curBR := nil;
          // add all busses from the previous time step that do not exist in the current time step
          for bbdep in prevBSBRP.Value do
          begin
            if not curBusses.ContainsKey(bbdep.Key) then
            begin
              if not Assigned(curBR) then
              begin
                // make sure a entry for the bus stop exists
                if not curSL.BusStops.TryGetValue(prevBSBRP.Key, curBR) then
                begin
                  curBR := TBussesRecords.Create();
                  curSL.BusStops.Add(prevBSBRP.Key, curBR);
                end;
              end;
              curBR.AddOrSetValue(bbdep.Key, bbdep.Value);
            end;
          end;
        end;
      finally
        curBusses.Free;
      end;
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

constructor TSantosLayer.Create(aScenario: TScenario; aBusBlock: Integer;
  aBaseDay, aBaseMonth, aBaseYear: Word;
  const aDomain, aID, aName, aDescription, aConnectString, aTablePrefix: string;
  aPubEntry: TIMBEventEntry; aIndicEntry, aSocEntry: TIMBEventEntry);
var
  oraSession: TOraSession;
  entries: TPaletteRampEntryArray;
begin
  // TODO
  inherited Create(aScenario, aDomain, aID, aName, aDescription);
  fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(28992);
  fTablePrefix := aTablePrefix;
  fBlockID := aBusBlock;
  fEventEntry := aPubEntry;
  fConnectString := aConnectString;
  fUpdateTimer := fScenario.project.Timers.CreateInactiveTimer;
  //fUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneMinute*5);
  fUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*10);
  fLastUpdate := hrtNow;
  fCurrentTime := Now;

  setLength(fBaseDay, 3);
  fBaseDay[0] := aBaseDay;
  fBaseDay[1] := aBaseMonth;
  fBaseDay[2] := aBaseYear;

  // todo: fBusStops := TDictionary<String, TBusStop>.Create;
  fStopObjects := TList<TSimpleObject>.Create;
  // todo: fTimeTable := TList<TTimeStop>.Create;
  fChargerTypes := TStringList.Create;

  fBusData := TBusData.Create;
  fSliderData := TSliderData.Create;

  setlength(entries, 7);
  entries[0] := TRampPaletteEntry.Create(NoChargeColor, 0, '0');
  entries[1] := TRampPaletteEntry.Create(NoChargeColor, 10, '10');
  entries[2] := TRampPaletteEntry.Create(NoChargeColor, 30, '30');
  entries[3] := TRampPaletteEntry.Create(LowChargeColor, 50, '50');
  entries[4] := TRampPaletteEntry.Create(MediumChargeColor, 70, '70');
  entries[5] := TRampPaletteEntry.Create(HighChargeColor, 90, '');
  entries[6] := TRampPaletteEntry.Create(HighChargeColor, 100, '100');
  fSoCPalette := TRampPalette.Create('State of Charge', entries, Nothing, Nothing, HighChargeColor);

  legendJSON := BuildRamplLegendJSON(fSoCPalette);

  oraSession := TOraSession.Create(nil);
  try
    oraSession.connectString := aConnectString;
    oraSession.Open;

    if MyOraLib.tableExists(oraSession, fTablePrefix+'EBUS_SOC') then
    begin
      // new
      readBusData(oraSession, fTablePrefix);
      readSliderData(oraSession, fTablePrefix);
      addDrivingBussesToPreviousStop;
      // prev
      initStops(oraSession);
      initTimeTable(oraSession);
      initChargerTypes(oraSession);
      Log.WriteLn('finished loading data for '+fTablePrefix, llNormal);
      showStops;
    end;

  finally
    oraSession.Free;
  end;

  // last: go live
  // todo: link to update of chart aIndicEntry.OnChangeObject := HandleOnChangeObject;
  aIndicEntry.OnChangeObject := HandleOnChangeObject;
  aSocEntry.OnChangeObject := HandleOnChangeObject;

  (*
  options.AddOrSetValue(sojnContextMenu, '"true"');
  options.AddOrSetValue(sojnContextmenuInheritItems, '"false"');
  options.AddOrSetValue(sojnContextmenuItems, '[{"text": "Edit bus parameters","tag":"'+tagEditBusParameters+'"}]');
  *)
end;

destructor TSantosLayer.Destroy;
begin
  // todo: FreeAndNil(fBusStops);
  // todo: FreeandNil(fTimeTable);
  FreeAndNil(fChargerTypes);
  FreeAndNil(fStopObjects);
  // new
  FreeAndNil(fSliderData);
  FreeAndNil(fBusData);
  inherited;
end;

procedure TSantosLayer.formEditChargeLocation(const aChargeLocation: string; aClient: TClient);
var
  chargerTypes: TArray<String>;
  stop: TBusStop2;
  formID: string;
  // todo: stop: TBusStop;
  formTitle: string;
  _openformdialog: string;
  _formElements: string;




begin
  TMonitor.Enter(fChargerTypes);
  try
    chargerTypes := fChargerTypes.ToStringArray;
  finally
    TMonitor.Exit(fChargerTypes);
  end;
  { todo:
  TMonitor.Enter(fBusStops);
  try
    if not fBusStops.TryGetValue(aChargeLocation, stop) then
      raise Exception.Create('No such charger: ' + aChargeLocation);
  finally
    TMonitor.Exit(fBusStops);
  end;
  }
  TMonitor.Enter(fSliderData);
  try
    if fSliderData.BusStops.TryGetValue(aChargeLocation, stop) and Assigned(stop.charger) then
    begin
      // ID for callback event
      formID := stop.id; // .objectID.ToString + '.' + stop.id;
      // Title of form
      formTitle := 'Edit chargelocation: ' + stop.name;
      // Create form elements
      _formElements := openFormElement_select('LOCATION', 'LOCATION:',
                                             [stop.id], stop.id, true, true);
      _formElements := _formElements + ',' + openFormElement_select('CHARGEPOLETYPE', 'Charger type:',
                                             chargerTypes, '' {stop.chargePoleType}, true, false);
      _formElements := _formElements + ',' + openFormElement_input('NUMBEROFCHARGINGPOLES', 'Number of charging poles:', 'int',
                                             stop.charger.numberOfPoles.ToString, true, false);
      _formElements := _formElements + ',' + openFormElement_input('MAXPOWER', 'Maximum power:', 'int',
                                             stop.charger.maxPower.ToString, true, false);
      // Put (name?), objectID, chargertype and chargercount in form
      _openformdialog :=
        '{'+
          '"type" : "openformdialog",'+
          '"payload" : {'+
            '"id" : "'+formID+'",'+
            '"title" : "'+formTitle+'",'+
            '"data" : ['+_formElements+']'+
          '}'+
        '}';

      // Push json to client
      aClient.signalString(_openformdialog);
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

function TSantosLayer.HandleClientSubscribe(aClient: TClient): Boolean;
var
  jsonTSData: string;
begin
  Result := inherited;
  // send data to time slider
  jsonTSData := jsonTimesliderData;
  aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
  aClient.signalString('{"type":"timesliderEvents","payload":{"setCurrentTime":"'+FormatDateTime(publisherDateTimeFormat, fCurrentTime)+'"}}');
end;

procedure TSantosLayer.HandleFormResult(aFormResult: TJSONObject);
var
  parameters: TJSONArray;
  parameter: TJSONValue;
  name: string;
  strLocation: string;
  strPoleType: string;
  poleCount: integer;
  // todo: stop, stopOrig: TBusStop;
  stop: TBusStop2;
  maxPower: integer;
  oraSession: TOraSession;
  oraQuery: TOraQuery;
  formID: string;
//  marker: TSimpleObject;
//  o: TSimpleObject;
begin
  if aFormResult.TryGetValue<string>('id', formID) then
  begin
    if formID='DefaultBus' then
    begin
      if aFormResult.TryGetValue<TJSONArray>('parameters', parameters) then
      begin
        // parse form parameters and update project parameters
        for parameter in parameters do
        begin
          name := parameter.GetValue<string>('name');
          if name='BatteryCapacity'
          then (scenario.project as TSantosProject).fBatteryCapacity := parameter.GetValue<double>('value')
          else if name='AverageEnergyConsumption'
          then (scenario.project as TSantosProject).fAverageEnergyConsumption := parameter.GetValue<double>('value')
          else if name='DriverEfficiency'
          then (scenario.project as TSantosProject).fDriverEfficiency := parameter.GetValue<double>('value')
          else if name='PowerConsumptionHVAC'
          then (scenario.project as TSantosProject).fPowerConsumptionHVAC := parameter.GetValue<double>('value');
        end;
        (scenario.project as TSantosProject).UpdateBusDefaults;
      end;
    end
    else // if formID='xx' other forms
    begin
      if aFormResult.TryGetValue<TJSONArray>('parameters', parameters) then
      begin
        strLocation := '';
        for parameter in parameters do
        begin
          name := parameter.GetValue<string>('name');

          if name.Equals('LOCATION') then
            strLocation := parameter.GetValue<string>('value')
          else if name.Equals('CHARGEPOLETYPE') then
            strPoleType := parameter.GetValue<string>('value')
          else if name.Equals('NUMBEROFCHARGINGPOLES') then
            poleCount :=  parameter.GetValue<integer>('value')
          else if name.Equals('MAXPOWER') then
            maxPower :=  parameter.GetValue<integer>('value');
        end;

        if strLocation<>'' then
        begin
          TMonitor.Enter(fSliderData);
          try
            if fSliderData.BusStops.TryGetValue(strLocation, stop) then
            begin
              stop.charger.poleType := strPoleType;
              stop.charger.numberOfPoles := poleCount;
              stop.charger.maxPower := maxPower;
              oraSession := TOraSession.Create(nil);
              try
                oraSession.connectString := fConnectString;
                oraSession.Open;
                oraQuery := TOraQuery.Create(nil);
                try
                  oraQuery.Session := oraSession;
                  oraQuery.SQL.Text :=
                      'UPDATE ' + fTablePrefix + 'EBUS_CHARGELOCATION SET ' +
                        'CHARGEPOLETYPE='''+strPoleType+''', ' +
                        'NUMBEROFCHARGINGPOLES='+poleCount.ToString+', ' +
                        'MAXPOWER='+maxPower.ToString+' '+
                      'WHERE OBJECT_ID='+stop.charger.objectID.ToString;
                  oraQuery.Execute;
                  oraSession.Commit;
                finally
                  oraQuery.free;
                end;
              finally
                oraSession.Free;
              end;

              fEventEntry.SignalChangeObject(actionChange, stop.charger.objectID);
              log.WriteLn('UPDATED CHARGELOCATION ' + stop.id + ' to ' + polecount.ToString + ' chargers of type: ' + strPoleType);
            end;
          finally
            TMonitor.Exit(fSliderData);
          end;
        end
      end;
    end;
  end;
end;

procedure TSantosLayer.handleNewTime(aClient: TClient; aTime: string);
var
  // todo: item: TTimeStop;
//  stopID: string;
//  o: TSimpleObject;
//  found: Boolean;
//  i, iNearest: Integer;
//  iMax: Integer;
  // todo: items: TDictionary<string, TTimeStop>;
//  done: Boolean;
  // todo: nearest, item2: TTimeStop;
//  J: Integer;
  // todo: stop: TBusStop;
//  ssid: string;
  client: TClient;
  //ttt: TArray<TDateTime>;
  //t: Integer;
  //sliderTime: TDateTime;
  //sl: TSliderRecord;
  //bsbrp: TPair<TBusStop2, TBussesRecords>;
  //minSoc: Double;
  //bbdep: TPair<Integer, TBusDataEntry>;
  //ibsp: TPair<string, TBusStop2>;
  //bde: TBussesRecords;
//  opacity: Double;
//  fillOpacity: Double;
  //context: string;
  //marker: TSimpleObject;
begin
  fCurrentTime := StrToDateTime(aTime, isoDateTimeFormatSettings);
  TMonitor.Enter(fScenario.clients);
  try
    for client in fScenario.clients do
    begin
      // send new time to all other clients
      if Client<>aClient then
        client.signalString('{"type":"timesliderEvents","payload":{"setCurrentTime":"'+FormatDateTime(publisherDateTimeFormat, fCurrentTime)+'"}}');
    end;
  finally
    TMonitor.Exit(fScenario.clients);
  end;

  UpdateStopsOnTime(fCurrentTime);


  { todo:
  stopID := '';
  found := false;
  iNearest := -1;

  items := TDictionary<string, TTimeStop>.Create;
  try
    TMonitor.Enter(fTimeTable);
    try
      i := 0;
      iMax := fTimeTable.Count;
      nearest := fTimeTable[I];

      while (not found) and (i<iMax) do
      begin
        item := fTimeTable[I];
        found := (time<=item.timeStop) and (time>=item.timeStart);
        if time>=item.timeStart then
        begin
          nearest := item;
          iNearest := i;
        end;

        Inc(I);
      end;

      if not found and (nearest.timeStop=0) then
      begin
        item := nearest;
        i := iNearest+1;
      end;

      if found then
      begin
        items.Add(item.stop, item);
        stopID := item.stop;

        if not stopID.Equals(sNoRoute) then
        begin
          J := I-1;
          done := false;
          while (not done) and (J>0) do
          begin
            item2 := fTimeTable[J];
            done := not item2.routeDir.Equals(item.routeDir);
            if not done and not items.ContainsKey(item2.stop) then
              items.Add(item2.stop, item2);
            Dec(J);
          end;

          J := I;
          done := false;
          while (not done) and (J<iMax) do
          begin
            item2 := fTimeTable[J];
            done := not item2.routeDir.Equals(item.routeDir);
            if not done and not items.ContainsKey(item2.stop) then
              items.Add(item2.stop, item2);
            Inc(J);
          end;
        end;
      end;

    finally
      TMonitor.Exit(fTimeTable);
    end;

    TMonitor.Enter(fStopObjects);
    try
      for o in fStopObjects do
      begin
        ssid := o.id;
        if ssid.StartsWith(idPrefixBusStop) then
          ssid := ssid.Substring(idPrefixBusStop.Length);

        TMonitor.Enter(fBusStops);
        try
          if not fBusStops.TryGetValue(ssid, stop) then stop.id:='';
        finally
          TMonitor.Exit(fBusStops);
        end;

        if items.TryGetValue(ssid, item2) then
        begin
          if (item2.timeStart = item.timeStart) and (item2.stop=item.stop) then
            o.addOptionDouble(sojnRadius, 12)
          else
            o.addOptionDouble(sojnRadius, 8);

           o.addOptionGeoColor(stop.geoColors( fSoCPalette.ValueToColors(item2.soc).fillColor) );
           UpdateObject(o, sojnTooltip, '"' + item2.tooltip(stop) + '"');
        end
        else
        begin
          o.addOptionDouble(sojnRadius, 8);
          if not stop.id.IsEmpty then
            o.addOptionGeoColor(stop.geoColors);

          updateObject(o, sojnTooltip, '"' + stop.tooltip + '"');
        end;

        UpdateObject(o, sojnOptions, o.jsonOptionsValue);
      end;
    finally
      TMonitor.Exit(fStopObjects);
    end;
  finally
    items.Free;
  end;
  }
end;

procedure TSantosLayer.HandleOnChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string);
var
  delta: THighResTicks;
begin
  try
    delta := Max(DateTimeDelta2HRT(dtOneSecond*3),DateTimeDelta2HRT(dtOneSecond*20) - (hrtNow - fLastUpdate));
    fUpdateTimer.Arm(delta, HandleDataUpdate);
  except
    on e: Exception
    do Log.WriteLn('Exception in TSantosLayer.HandleOnChangeObject: '+e.Message);
  end;
end;

procedure TSantosLayer.HandleSelectedEvent(aClient: TClient; aMessage: TJSONValue);
begin
end;

procedure TSantosLayer.HandleTimeSliderEvent(aProject: TProject;
  aClient: TClient; const aType: string; aPayload: TJSONObject);
var
  selectedTime: string;
  active: Boolean;
  selectedEvent: TJSONValue;
  brush: TJSONValue;
begin
  if aPayload.TryGetValue<boolean>('active', active) then
  begin
    // layer.live[aClient] := not active;
  end;
  if aPayload.TryGetValue<string>('selectedTime', selectedTime) then
  begin
    fTimeSliderTimer.Arm(DateTimeDelta2HRT(0.1*dtOneSecond),
      procedure (aTimer: TTimer; aTime: THighResTicks)
      begin
        handleNewTime(aClient, selectedTime);
      end);
  end;
  if aPayload.TryGetValue<TJSONValue>('selectedEvent', selectedEvent) then
  begin
     HandleSelectedEvent(aClient, selectedEvent);
  end;
  if aPayload.TryGetValue<TJSONValue>('brush', brush) then
  begin  // Time range selection
    Log.WriteLn('brush: '+brush.toJSON, llWarning);
  end;
end;

procedure TSantosLayer.handleUpdateLayerObject(aClient: TClient;
  aPayload: TJSONObject);
var
  objectID: string;
//  stopID: string;
  contextmenuClick: TJsonObject;
//  tag: string;
  sr: TSliderRecord;
  bs: TBusStop2;
  br: TBussesRecords;
  busid: Integer;
  bde: TBusDataEntry;
  oraSession: TOraSession;
  tag: string;
begin
  objectID := aPayload.GetValue<string>('objectid');

  if aPayload.TryGetValue<TJsonObject>('contextmenuClick', contextmenuclick) then
  begin
    if contextmenuClick.TryGetValue<Integer>('busid', busid) then
    begin
      TMonitor.Enter(fSliderData);
      try
        if fSliderData.BusStops.TryGetValue(objectID, bs) then
        begin
          if fSliderData.fTimeTable.TryGetValue(fCurrentTime, sr) then
          begin
            if sr.BusStops.TryGetValue(bs, br) then
            begin
              if br.TryGetValue(busid, bde) then
              begin
                oraSession := TOraSession.Create(nil);
                try
                  oraSession.connectString := fConnectString;
                  oraSession.Open;

                  (scenario.project as TSantosProject).ReadChartBlockSoC(oraSession, scenario, fTablePrefix, busid);
                finally
                  oraSession.Free;
                end;
              end;
            end;
            //sr.BusStops.TryGetValue()
          end;
        end;
      finally
        TMonitor.Exit(fSliderData);
      end;
    end
    else if contextmenuClick.TryGetValue<string>('tag', tag) then
    begin
      if tag=tagEditChargeLocation then
      begin
        formEditChargeLocation(objectID, aClient);
      end;
    end;
    {
    if objectID.StartsWith(idPrefixBusStop) and
       contextmenuclick.TryGetValue<string>('tag', tag) and
       tag.Equals(tagEditChargeLocation) then
    begin
      stopID := objectID.SubString(idPrefixBusStop.Length);
      formEditChargeLocation(stopID, aClient);
    end;
    }
  end
  else
    inherited;
end;

function TSantosLayer.jsonTimesliderData: String;
var
  // todo: stop: TTimeStop;
  //startTime, endTime,
  //entry,
  color: string;
// tt: string;
  times: TArray<TDateTime>;
  t: TDateTime;
  sr: TSliderRecord;
  soc: Double;
  lastColor: string;
  lastTime: TDateTime;
  curTime: TDateTime;
  entry: TJSONObject;
begin
  (* todo:
  for stop in fTimeTable do
  begin
    startTime := FormatDateTime(publisherDateTimeFormat, stop.timeStart);
    endTime := FormatDateTime(publisherDateTimeFormat, stop.timeStop);

    tt := stop.name + ' - SoC: ' + FloatToStr(stop.soc) + '%';
    color := ColorToJSON( fSoCPalette.ValueToColors(stop.soc).fillColor );

    if startTime<>'' then
    begin
      // add new entry for pervious color: fillColorPrev startTime-endTime
      entry :=
        '"start":"'+startTime+'"'+','+
        '"end":"'+endTime+'"'+','+
        '"color":"'+color+'"'+','+
        '"tooltip":"'+tt+'"';
//        '"id":"'+stop.+'"'+','+
//        stop.location; // localized double
      jsonAdd(Result, '{'+entry+'}');
    end;
  end;
  *)
  Result := '';
  TMonitor.Enter(fSliderData);
  try
    times := fSliderData.TimeTable.Keys.ToArray;
    TArray.Sort<TDateTime>(times);
    lastColor := '';
    lastTime := -1;
    curTime := -1;
    if length(times)>0 then
    begin
      for t in times do
      begin
        sr := fSliderData.TimeTable[t];
        curTime := t; //Date+t; // update date/time to today
        soc := Max(sr.MinSoc, 0);
        color := ColorToJSON(fSoCPalette.ValueToColors(soc).fillColor);
        if lastColor='' then
        begin
          lastTime := curTime;
          lastColor := color;
        end
        else
        begin
          if lastColor<>color then
          begin
            entry := TJSONObject.Create;
            try
              entry.AddPair('start', TJSONString.Create(FormatDateTime(publisherDateTimeFormat, lastTime)));
              entry.AddPair('end', TJSONString.Create(FormatDateTime(publisherDateTimeFormat, curTime)));
              entry.AddPair('color', TJSONString.Create(lastColor));
              jsonAdd(Result, entry.ToJSON);
            finally
              entry.Free;
            end;
            {
            entry :=
              '"start":"'+FormatDateTime(publisherDateTimeFormat, lastTime)+'"'+','+
              '"end":"'+FormatDateTime(publisherDateTimeFormat, curTime)+'"'+','+
              '"color":"'+lastColor+'"';
              // '"tooltip":"'+tt+'"';
              // '"id":"'+stop.+'"'+','+
              // stop.location; // localized double
            }
            lastTime := curTime;
            lastColor := color;
          end;
        end;
      end;
      if lastTime<>curTime then
      begin
  //      entry :=
  //        '"start":"'+FormatDateTime(publisherDateTimeFormat, lastTime)+'"'+','+
  //        '"end":"'+FormatDateTime(publisherDateTimeFormat, curTime)+'"'+','+
  //        '"color":"'+lastColor+'"';
  //      jsonAdd(Result, '{'+entry+'}');
        entry := TJSONObject.Create;
        try
          entry.AddPair('start', TJSONString.Create(FormatDateTime(publisherDateTimeFormat, lastTime)));
          entry.AddPair('end', TJSONString.Create(FormatDateTime(publisherDateTimeFormat, curTime)));
          entry.AddPair('color', TJSONString.Create(lastColor));
          jsonAdd(Result, entry.ToJSON);
        finally
          entry.Free;
        end;
      end;
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

procedure TSantosLayer.readBusData(aSession: TOraSession; const aTablePrefix: string);
var
  query: TOraTable;
  blockID: Integer;
  arrivalTime: string;
  tripName: string;
  stopID: string;
  soc: Double;
  bdes: TBusTimeTable;
  at: TDateTime;
  curDate: TDateTime;
begin
  TMonitor.Enter(fBusData);
  try
    curDate := Date;
    //Log.WriteLn('Loading bus soc data');
    query := TOraTable.Create(nil);
    try
      query.Session := aSession;
      query.ReadOnly := True;
      query.FetchRows := 1000;
      query.UniDirectional := True;
      query.SQL.Text :=
        'SELECT BLOCK_ID, ARRIVAL_TIME, TRIP_NAME, STOP_ID, STATE_OF_CHARGE '+
        'FROM '+aTablePrefix+'EBUS_SOC '+
        'ORDER BY BLOCK_ID, ARRIVAL_TIME';
      query.Execute;
      query.First;
      while not query.Eof do
      begin
        blockID := query.Fields[0].AsInteger;
        arrivalTime := query.Fields[1].AsString;
        tripName := query.Fields[2].AsString;
        stopID := query.Fields[3].AsString;
        soc := query.Fields[4].AsFloat;
        if not fBusData.BusRecords.TryGetValue(blockID, bdes) then
        begin
          bdes := TBusTimeTable.Create([doOwnsValues]);
          fBusData.BusRecords.Add(blockID, bdes);
        end;
        at :=
          curDate+
          Integer.Parse(Copy(arrivalTime,1,2))/24+
          Integer.Parse(Copy(arrivalTime,4,2))/(24*60)+
          Integer.Parse(Copy(arrivalTime,7,2))/(24*60*60);
        // ignore double entries and use last (drop of/pickup problems?)
        bdes.AddOrSetValue(at, TBusDataEntry.Create(at, tripName, stopID, soc));
        query.Next;
      end;
    finally
      query.Free;
    end;
    Log.WriteLn('Loaded bus soc data');
  finally
    TMonitor.Exit(fBusData);
  end;
end;

procedure TSantosLayer.readSliderData(aSession: TOraSession; const aTablePrefix: string);
var
  query: TOraTable;
  chargeLocationObjectID: Integer;
  geneNodeObjectID: Integer;
  chargePoleType: string;
  numberOfChargingPoles: Integer;
  maxPower: Double;
  x: Double;
  y: Double;
  stopName: string;
  stopID: string;
  p: TGIS_Point;
  lat: Double;
  lon: Double;
  charger: TCharger;
  bs: TBusStop2;

  blockID: Integer;
  arrivalTime: string;
  tripName: string;
  //stopID: string;
  at: TDateTime;
  sr: TSliderRecord;
  bussesRecords: TBussesRecords;
  bde: TBusDataEntry;
  tt: TBusTimeTable;
  curDate: TDateTime;
begin
  Log.WriteLn('Loading slider bus stop data');
  curDate := Date;
  TMonitor.Enter(fSliderData);
  try
    query := TOraTable.Create(nil);
    try
      query.Session := aSession;
      query.ReadOnly := True;
      query.FetchRows := 1000;
      query.UniDirectional := True;
      query.SQL.Text :=
        'SELECT '+
           aTablePrefix+'EBUS_CHARGELOCATION.OBJECT_ID EBUS_CHARGELOCATION_OBJECT_ID, '+aTablePrefix+'GENE_NODE.OBJECT_ID GENE_NODE_OBJECT_ID, '+
           'CHARGEPOLETYPE, NUMBEROFCHARGINGPOLES, MAXPOWER,  XCOORD x, YCOORD y, HALTE_NAME, HALTE_ID '+
        'FROM '+aTablePrefix+'EBUS_CHARGELOCATION '+
               'JOIN '+
                aTablePrefix+'GENE_NODE '+
                'ON '+aTablePrefix+'EBUS_CHARGELOCATION.GENE_NODE_ID='+aTablePrefix+'GENE_NODE.OBJECT_ID';
      query.Execute;
      query.First;
      while not query.Eof do
      begin
        chargeLocationObjectID := query.Fields[0].AsInteger;
        geneNodeObjectID := query.Fields[1].AsInteger;
        chargePoleType := query.Fields[2].AsString;
        numberOfChargingPoles := query.Fields[3].AsInteger;
        maxPower := query.Fields[4].AsFloat;
        x := query.Fields[5].AsFloat;
        y := query.Fields[6].AsFloat;
        stopName := query.Fields[7].AsString;
        stopID := query.Fields[8].AsString;


        p.X := x;
        P.Y := y;
        P := fSourceProjection.ToGeocs(P);
        lat := p.Y ;
        lon := p.X ;
        charger := TCharger.Create(chargeLocationObjectID, chargePoleType, numberOfChargingPoles, maxPower);
  //      if numberOfChargingPoles>0
  //      then charger := TCharger.Create(chargeLocationObjectID, chargePoleType, numberOfChargingPoles, maxPower)
  //      else charger := nil;
        // only add when not known.. should always add

        if not fSliderData.BusStops.TryGetValue(stopID, bs) then
        begin
          bs := TBusStop2.Create(lat, lon, stopID, stopName, geneNodeObjectID, charger);
          fSliderData.BusStops.Add(stopID, bs);
        end;
        query.Next;
      end;
    finally
      query.Free;
    end;

    // fSliderData.fTimeTable
    Log.WriteLn('Loading slider time table data');
    query := TOraTable.Create(nil);
    try
      query.Session := aSession;
      query.ReadOnly := True;
      query.FetchRows := 1000;
      query.UniDirectional := True;
      query.SQL.Text :=
        'SELECT BLOCK_ID, ARRIVAL_TIME, TRIP_SHORT_NAME, STOP_ID '+
        'FROM '+aTablePrefix+'EBUS_TIMETABLE '+
        'WHERE PICKUP_TYPE=0 '+
        'ORDER BY BLOCK_ID, ARRIVAL_TIME';
      query.Execute;
      query.First;
      while not query.Eof do
      begin
        blockID := query.Fields[0].AsInteger;
        arrivalTime := query.Fields[1].AsString;
        tripName := query.Fields[2].AsString;
        stopID := query.Fields[3].AsString;

        at :=
          curDate+
          Integer.Parse(Copy(arrivalTime,1,2))/24+
          Integer.Parse(Copy(arrivalTime,4,2))/(24*60)+
          Integer.Parse(Copy(arrivalTime,7,2))/(24*60*60);
        // create base slider record
        if not fSliderData.TimeTable.TryGetValue(at, sr) then
        begin
          sr := TSliderRecord.Create;
          fSliderData.TimeTable.Add(at, sr);
        end;
        // lookup bus stop info
        if fSliderData.BusStops.TryGetValue(stopID, bs) then
        begin
          // lookup bus time table
          TMonitor.Enter(fBusData);
          try
            if fBusData.BusRecords.TryGetValue(blockID, tt) then
            begin
              // lookup bus data entry
              if tt.TryGetValue(at, bde) then
              begin
                // find or create busses record in slider record: bus stops
                if not sr.BusStops.TryGetValue(bs, bussesRecords) then
                begin
                  bussesRecords := TBussesRecords.Create();
                  sr.BusStops.Add(bs, bussesRecords);
                end;
                // add found bus data entry as ref to busses records
                bussesRecords.AddOrSetValue(blockID, bde);
              end
              else Log.WriteLn('Bus data entry not found '+blockID.ToString+' @ '+DateTimeToStr(at), llError);
            end
            else Log.WriteLn('Bus time table not found: '+blockID.ToString, llError);
          finally
            TMonitor.Exit(fBusData);
          end;
        end
        else Log.WriteLn('Bus stop not found: '+stopID, llError);
        query.Next;
      end;
    finally
      query.Free;
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

procedure TSantosLayer.showStops;
var
  ibsp: TPair<string, TBusStop2>;
  marker: TCircleMarker;
begin
  TMonitor.Enter(fSliderData);
  try
    for ibsp in fSliderData.fBusStops do
    begin
      marker :=
          TCircleMarker.Create(Self, ibsp.key,
            ibsp.Value.lat, ibsp.Value.lon, 8, Nan,
            [],
            [ [sojnTooltip, '"'+ibsp.Value.name+'"'] ]);

      UpdateStop(ibsp.key, ibsp.Value, marker);
      AddObject(marker, marker.jsonNewObject);
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

procedure TSantosLayer.SignalRecalc;
begin
  fEventEntry.SignalChangeObject(actionChange, 0);
end;

procedure TSantosLayer.updateStop(const aID: string; aStop: TBusStop2; aObject: TSimpleObject);
begin
  aObject.addOptionString(sojnContextMenu, 'true');
  aObject.addOptionString(sojnContextmenuInheritItems, 'false');
  if Assigned(aStop.charger) then
  begin
    aObject.addOptionString(sojnContextMenu, 'true');
    aObject.addOptionString(sojnContextmenuInheritItems, 'false');
    aObject.addOptionStructure(sojnContextmenuItems, '[{"text": "Edit charge location","id": "'+aID+'", "tag":"'+tagEditChargeLocation+'"}]');

    if aStop.charger.numberOfPoles>0 then
    begin
      aObject.addOptionGeoColor(TGeoColors.Create($FFCCCCCC, $FFFF00FF));
      aObject.addOptionInteger('weight', 2);
    end
    else
    begin
      aObject.addOptionGeoColor(TGeoColors.Create($FFCCCCCC, $FF999999));
      aObject.addOptionInteger('weight', 1);
    end;
  end
  else
  begin
    aObject.addOptionGeoColor(TGeoColors.Create($FFCCCCCC, $FF999999));
    aObject.addOptionInteger('weight', 1);
  end;

end;

procedure TSantosLayer.updateStopsBase;
var
  ibsp: TPair<string, TBusStop2>;
  marker: TSimpleObject;
begin
  TMonitor.Enter(fSliderData);
  try
    for ibsp in fSliderData.fBusStops do
    begin
      if objects.TryGetValue(ibsp.key, marker) then
      begin
        UpdateStop(ibsp.key, ibsp.Value, marker);
        UpdateObject(marker, sojnOptions, marker.jsonOptionsValue)
      end;
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

procedure TSantosLayer.UpdateStopsOnTime(aTime: TDateTime);
var
  ttt: TArray<TDateTime>;
  sliderTime: TDateTime;
  t: Integer;
  sl: TSliderRecord;
  ibsp: TPair<string, TBusStop2>;
  marker: TSimpleObject;
  minSoc: Double;
  bde: TBussesRecords;
  context: string;
  bbdep: TPair<Integer, TBusDataEntry>;
begin
  TMonitor.Enter(fSliderData);
  try
    ttt := fSliderData.fTimeTable.Keys.ToArray;
    TArray.Sort<TDateTime>(ttt);
    if length(ttt)>0 then
    begin
      // find time on or below slider time
      sliderTime := ttt[0]; // sentinel: lowest value
      for t := length(ttt)-1  downto 0 do
      begin
        if ttt[t]<=aTime then
        begin
          sliderTime := ttt[t];
          break;
        end;
      end;
      // process data on slider time
      if fSliderData.TimeTable.TryGetValue(sliderTime, sl) then
      begin
        for ibsp in fSliderData.BusStops do
        begin
          if objects.TryGetValue(ibsp.Key, marker) then
          begin
            minSoc := Double.NaN;
            if sl.BusStops.TryGetValue(ibsp.Value, bde) then
            begin
              context := '';
              for bbdep in bde do
              begin
                if minSoc.IsNaN or (minSoc<bbdep.Value.soc)
                then minSoc := Max(bbdep.Value.soc, 0);
                // add  entry in sub menu for bus
                jsonAdd(context, '{"text": "'+bbdep.Value.tripName+' ('+bbdep.Key.ToString+')","busid":'+bbdep.Key.ToString+'}');
              end;
              if Assigned(ibsp.Value.charger)
              then jsonAdd(context, '{"text": "Edit charge location","tag":"'+tagEditChargeLocation+'"}');

              marker.addOptionString(sojnContextMenu, 'true');
              marker.addOptionString(sojnContextmenuInheritItems, 'false');
              marker.addOptionStructure(sojnContextmenuItems, '['+context+']');
              //marker.addOptionString('interactive', 'true');

              marker.addOptionDouble('opacity', 1.0);
              marker.addOptionDouble('fillOpacity', 1.0);
            end
            else
            begin
              if Assigned(ibsp.Value.charger) then
              begin
                marker.addOptionString(sojnContextMenu, 'true');
                marker.addOptionString(sojnContextmenuInheritItems, 'false');
                marker.addOptionStructure(sojnContextmenuItems, '[{"text": "Edit charge location","id": "'+ibsp.Key+'", "tag":"'+tagEditChargeLocation+'"}]');
                //marker.addOptionString('interactive', 'true');

                if ibsp.Value.charger.numberOfPoles>0 then
                begin
                  marker.addOptionDouble('opacity', 0.5);
                  marker.addOptionDouble('fillOpacity', 0.2);
                end
                else
                begin
                  marker.addOptionDouble('opacity', 0.3);
                  marker.addOptionDouble('fillOpacity', 0.2);
                end;
              end
              else
              begin
                marker.options.Remove(sojnContextMenu);
                marker.options.Remove(sojnContextmenuInheritItems);
                marker.options.Remove(sojnContextmenuItems);
                //marker.addOptionString('interactive', 'false');

                marker.addOptionDouble('opacity', 0.2);
                marker.addOptionDouble('fillOpacity', 0.2);
              end;
            end;
            marker.addOptionString('fillColor', ColorToJSON(fSoCPalette.ValueToColors(minSoc).fillColor));
            UpdateObject(marker, sojnOptions, marker.jsonOptionsValue);
          end;
        end;
      end;
    end;
  finally
    TMonitor.Exit(fSliderData);
  end;
end;

procedure TSantosLayer.HandleDataUpdate(aTimer: TTimer; aTime: THighResTicks);
var
  jsonTSData: string;
  oraSession: TOraSession;
begin
  try
    fLastUpdate := aTime;

    // read data
    oraSession := TOraSession.Create(nil);
    try
      Log.WriteLn('Data changed. Updating');
      oraSession.connectString := fConnectString;
      oraSession.Open;
      readBusData(oraSession , fTablePrefix);
    finally
      oraSession.Free;
    end;

    // update slider
    jsonTSData := jsonTimesliderData;
    scenario.forEachClient(
      procedure(aClient: TClient)
      begin
        // send data to time slider
        aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
      end);
    // update base stop options
    updateStopsBase;
    // update stops based on time slider time
    UpdateStopsOnTime(fCurrentTime);
  except
    on e: Exception
    do Log.WriteLn('Exception in TSantosLayer.HandleDataUpdate: '+e.Message, llError);
  end;
end;

procedure TSantosLayer.initChargerTypes(aSession: TOraSession);
var
  query: TOraQuery;
begin
  query := TOraQuery.Create(nil);
  Log.WriteLn('Loading charger types');
  try
    query.Session := aSession;
    query.SQL.Text :=
      'SELECT cp.TYPE ' +
      'FROM EBUS_CHARGERPOLE cp';
    query.Open;

    TMonitor.Enter(fChargerTypes);
    try
      while not query.Eof do
      begin // TODO, X,Y
        fChargerTypes.Add(query.FieldByName('TYPE').AsString);
        query.next;
       end;
    finally
      TMonitor.Exit(fChargerTypes);
    end;
  finally
    query.free;
  end;
end;

procedure TSantosLayer.initStops(aSession: TOraSession);
//var
//  query: TOraQuery;
  // todo: stop: TBusStop;
  // todo: stopPair: TPair<string, TBusStop>;
//  marker: TCircleMarker;
//  p: TGIS_Point;
//  o: TSimpleObject;
//  removeObjects: TList<TSimpleObject>;
begin
  (* todo:
  TMonitor.Enter(fBusStops);
  Log.WriteLn('Loading stops');
  try
    removeObjects := TList<TSimpleObject>.create;
    try
      TMonitor.Enter(fObjects);
      try
        for stopPair in fBusStops do
        begin
          if fObjects.TryGetValue(idPrefixBusStop+stopPair.value.id, o) then
            removeObjects.Add(o)
        end;
      finally
        TMonitor.Exit(fObjects);
      end;

      TMonitor.Enter(fStopObjects);
      try
        fStopObjects.Clear;
      finally
       TMonitor.Exit(fStopObjects);
      end;

      for o in removeObjects do
        RemoveObject(o);

    finally
      removeObjects.Free;
    end;

    fBusStops.Clear;
    query := TOraQuery.Create(nil);
    try
      query.Session := aSession;
      {
      gene_node (node_type_id=1)
      join
      charlocation.gene_node_id=gene_ndoe.obect_id
      }

      // EBUS_CHARGELOCATION event
      {
      query.SQL.Text :=
        'SELECT DISTINCT ' +
          'tt.STOP_DESC, tt.PLACE, '+    //_stop_name
          'node.XCOORD, node.YCOORD, ' +
          ' c.CHARGEPOLETYPE, c.NUMBEROFCHARGINGPOLES, c.MAXPOWER, c.OBJECT_ID ' +
        'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
          'LEFT OUTER JOIN '+fTablePrefix+'GENE_NODE node '+
            'ON node.HALTE_NAME=COALESCE(tt.STOP_DESC, tt.PLACE) ' +
          'LEFT OUTER JOIN '+fTablePrefix+'EBUS_CHARGELOCATION c ' +
            'ON c.LOCATION=tt.PLACE ' +
        'WHERE tt.BLOCK=' + fBlockID.ToString ; //nodetypeid=1
      }
      query.SQL.Text :=
        'SELECT ' +
          'node.HALTE_NAME STOP_DESC, node.HALTE_ID PLACE, '+    //_stop_name
          'node.XCOORD, node.YCOORD, ' +
          'c.CHARGEPOLETYPE, c.NUMBEROFCHARGINGPOLES, c.MAXPOWER, c.OBJECT_ID ' +
        'FROM '+fTablePrefix+'GENE_NODE node '+
              'JOIN '+
              fTablePrefix+'EBUS_CHARGELOCATION c ' +
              'ON c.GENE_NODE_ID=node.OBJECT_ID ' +
        'WHERE node.NODE_TYPE_ID=1';


      query.Open;

      while not query.Eof do
      begin // TODO, X,Y
        if not query.FieldByName('PLACE').IsNull then
        begin
          stop.id :=  query.FieldByName('PLACE').AsString;
          if not query.FieldByName('STOP_DESC').IsNull then
            stop.name := query.FieldByName('STOP_DESC').AsString
          else
            stop.name := stop.id;
        end
        else if not query.FieldByName('STOP_DESC').IsNull then
        begin
          stop.name := query.FieldByName('STOP_DESC').AsString;
          stop.id := MD5Sum(stop.name);
        end
        else
        begin // Unknown stop.. we can't display this
          stop.name := '';
          stop.id := '';
        end;

        if not query.FieldByName('XCOORD').IsNull then
        begin
          p.X := query.FieldByName('XCOORD').AsFloat;
          P.Y := query.FieldByName('YCOORD').AsFloat;
          P := fSourceProjection.ToGeocs(P);
          stop.lat := p.Y ;
          stop.lon := p.X ;
        end
        else
        begin
//           Dump on the runway
          stop.lat := 52.306625;
          stop.lon :=  4.778465;
          Log.WriteLn('No location known for stop ID: ' + stop.id + ', name: ' + stop.name, llWarning);
        end;
        if not query.FieldByName('CHARGEPOLETYPE').IsNull then
        begin
          stop.chargePoleType := query.FieldByName('CHARGEPOLETYPE').AsString;
          stop.numberOfPoles := query.FieldByName('NUMBEROFCHARGINGPOLES').AsInteger;
          stop.maxPower := query.FieldByName('MAXPOWER').AsInteger;
          stop.objectID := query.FieldByName('OBJECT_ID').AsInteger;
        end
        else
        begin
          stop.chargePoleType := '';
          stop.numberOfPoles := -1;
          stop.maxPower := -1;
          stop.objectID := -1;
        end;

        if not stop.name.IsEmpty then
        begin
          if fBusStops.ContainsKey(stop.id) then
            Log.WriteLn('Skipping duplicate Bus stop ID: ' + stop.id + ', name: ' + stop.name, llError)
          else
          begin
              // Create Marker
            marker := TCircleMarker.Create(Self, idPrefixBusStop+stop.id, stop.lat, stop.lon, 8, Nan,
              [],
              [ [sojnTooltip, '"' + stop.tooltip + '"'] ]); //, Double.NaN, [[sojnDraggable, 'true']]);
            marker.addOptionGeoColor(stop.geoColors);

            if stop.isCharger then
            begin // Chargers can be edited
              marker.addOptionString(sojnContextMenu, 'true');
              marker.addOptionString(sojnContextmenuInheritItems, 'false');
              marker.addOptionStructure(sojnContextmenuItems, '[{"text": "Edit charge location","index": '+stop.objectID.ToString+', "tag":"'+tagEditChargeLocation+'"}]');
            end;

            AddObject(marker, marker.jsonNewObject);

            TMonitor.Enter(fStopObjects);
            try
              fStopObjects.add(marker);
            finally
             TMonitor.Exit(fStopObjects);
            end;
            fBusStops.Add(stop.id, stop);
          end;
        end;
        query.Next;
      end;

    finally
      query.Free;
    end;

  finally
    TMonitor.Exit(fBusStops);
  end;
  *)
end;

procedure TSantosLayer.initTimeTable(aSession: TOraSession);
var
//  query: TOraQuery;
  // todo: ttEntry, ttEntryPrev: TTimeStop;
//  i: Integer;
  tableName: string;
//  indicTableExists: boolean;
begin
  tableName := 'EBUS_INDIC_DAT10' + fBlockID.ToString.PadLeft(2,'0') + '01'; // SoC
// todo:  indicTableExists := MyOraLib.TableExists(aSession, fTablePrefix+tableName);

  Log.WriteLn('Loading timetable');
  (* todo:
  TMonitor.Enter(fTimeTable);
  try
    fTimeTable.Clear;

    query := TOraQuery.Create(nil);
    try
      query.Session := aSession;
      if indicTableExists then
      {
        query.SQL.Text := 'SELECT tt.STOP_DESC, tt.PLACE, LPAD(TRIM(tt.TIME), 5, ''0'') as TIME, tt.ROUTE, tt.DIRECTION, soc.Y ' +
                          'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
                              'LEFT OUTER JOIN '+fTablePrefix+tableName +' soc '+
                              ' ON soc.X LIKE tt.STOP_DESC ' +
                              ' AND LPAD(TRIM(SOC.time), 5,''0'') LIKE LPAD(TRIM(tt.time), 5,''0'')' +
                          'WHERE tt.BLOCK=' + fBlockId.ToString + ' ' +
                          'ORDER BY tt.TIME ASC'
        }
        query.SQL.Text := 'SELECT tt.STOP_NAME STOP_DESC, tt.STOP_ID PLACE, LPAD(TRIM(tt.ARRIVAL_TIME), 5, ''0'') as TIME, tt.ROUTE, tt.DIRECTION, soc.Y ' +
                          'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
                              'LEFT OUTER JOIN '+fTablePrefix+tableName +' soc '+
                              ' ON soc.X LIKE tt.STOP_DESC ' +
                              ' AND LPAD(TRIM(SOC.time), 5,''0'') LIKE LPAD(TRIM(tt.time), 5,''0'')' +
                          'WHERE tt.BLOCK=' + fBlockId.ToString + ' ' +
                          'ORDER BY tt.TIME ASC'
      else
        {
        query.SQL.Text := 'SELECT tt.STOP_DESC, tt.PLACE, LPAD(TRIM(tt.TIME), 5, ''0'') as TIME, tt.ROUTE, tt.DIRECTION, 0 as Y ' +
                          'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
//                              'LEFT OUTER JOIN '+fTablePrefix+tableName +' soc '+
//                              ' ON soc.X LIKE tt.STOP_DESC ' +
//                              ' AND LPAD(TRIM(SOC.time), 5,''0'') LIKE LPAD(TRIM(tt.time), 5,''0'')' +
                          'WHERE tt.BLOCK=' + fBlockId.ToString + ' ' +
                          'ORDER BY tt.TIME ASC';
        }
        query.SQL.Text := 'SELECT tt.STOP_DESC, tt.PLACE, LPAD(TRIM(tt.TIME), 5, ''0'') as TIME, tt.ROUTE, tt.DIRECTION, 0 as Y ' +
                          'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
//                              'LEFT OUTER JOIN '+fTablePrefix+tableName +' soc '+
//                              ' ON soc.X LIKE tt.STOP_DESC ' +
//                              ' AND LPAD(TRIM(SOC.time), 5,''0'') LIKE LPAD(TRIM(tt.time), 5,''0'')' +
                          'WHERE tt.BLOCK=' + fBlockId.ToString + ' ' +
                          'ORDER BY tt.TIME ASC';
      query.Open;

      while not query.Eof do
      begin // TODO, X,Y
        ttEntry.timeStart := SantosTimeToDateTime (query.FieldByName('TIME').AsString, fBaseDay);
        ttEntry.soc := query.FieldByName('Y').AsFloat;
        if not query.FieldByName('PLACE').IsNull then
        begin
          ttEntry.stop :=  query.FieldByName('PLACE').AsString;
          if not query.FieldByName('STOP_DESC').IsNull then
            ttEntry.name := query.FieldByName('STOP_DESC').AsString
          else
            ttEntry.name := ttEntry.stop;
        end
        else if not query.FieldByName('STOP_DESC').IsNull then
        begin
          ttEntry.name := query.FieldByName('STOP_DESC').AsString;
          ttEntry.stop := MD5Sum(ttEntry.name);
        end;

        if not ttEntry.stop.IsEmpty then
        begin
          if not query.FieldByName('ROUTE').IsNull then
            ttEntry.routeDir := query.FieldByName('ROUTE').AsString + '-' +  query.FieldByName('DIRECTION').AsString
          else
            ttEntry.routeDir := sNoRoute;

          i := fTimeTable.Count; // fTimeTable.Add(ttEntry);
          if i>0 then
          begin
            ttEntryPrev := fTimeTable[i-1];
            if ttEntryPrev.timeStart = ttEntry.timeStart then
              ttEntry.timeStart := IncSecond(ttEntry.timeStart, 30);
            ttEntryPrev.timeStop := ttEntry.timeStart;
            fTimeTable[i-1] := ttEntryPrev;
          end;
          fTimeTable.Add(ttEntry);
        end;
        query.Next;
      end;

    finally
      query.Free;
    end;

    fCurrentTime := now;

  finally
    TMonitor.Exit(fTimeTable);
  end;
  *)
end;

(*
{ TBusStop }

function TBusStop.geoColors(aFill:Cardinal=colorBasicFill) : TGeoColors;
const
 colorBasicCharger = $4C177D or $FF000000;
 colorBasicWHEREAMI = $FF0000 or $FF000000;
begin
  if isCharger then
    Result := TGeoColors.Create(
                aFill,
                colorBasicCharger)
  else if SameValue(lat,52.306625) and SameValue(lon,4.778465) then
     Result := TGeoColors.Create(
                aFill,
                colorBasicWHEREAMI)
  else
    Result := TGeoColors.Create(
                aFill,
                colorBasicOutline);

end;

function TBusStop.isCharger: boolean;
begin
  Result := numberOfPoles>=0;
end;

function TBusStop.location: string;
begin
  Result := '"lat": ' + FloatToJson(lat) + ', "lon": ' + FloatToJson(lon)
end;

function TBusStop.tooltip: string;
begin
  if isCharger then
    Result := name + '<br\/>' + numberOfPoles.ToString + ' ' + chargePoleType + ' chargers. ' + self.maxPower.ToString + '[UNIT?] max.'
  else
    Result := name;
end;

{ TTimeStop }

function TTimeStop.tooltip(stop: TBusStop): String;
begin
  Result := stop.tooltip + '<br/>' +
            'Line ' + self.routeDir + '<br/>' +
            FormatDateTime('hh:mm', timeStart) + ' - ' +  FormatDateTime('hh:mm', timeStop) + '<br/>' +
            'SoC: ' + FloatToStr(self.soc) + ' %' ;

end;
*)

end.
