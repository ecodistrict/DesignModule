unit PublishServerSantos;

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
    cINDIC_DAT_TYPE_ID_LENGTH = 5;
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
        aPubEntry: TIMBEventEntry; aIndicEntry, aSocEntry: TIMBEventEntry;
        aSourceProjection: TGIS_CSProjectedCoordinateSystem);
    destructor Destroy; override;
  private
    fEventEntry: TIMBEventEntry;
  protected
    fStopObjects: TList<TSimpleObject>;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem; // ref
    fTablePrefix: String;
    fBlockID: Integer;
    fSoCPalette: TWDPalette;
    fTimeSliderTimer: TTimer;
    fBaseDay: array of word; // [d,m,y]
    fChargerTypes: TStrings;
    fConnectString: string;
    fUpdateTimer: TTimer;
    fLastUpdate: THighResTicks;
    fCurrentTime: TDateTime;
    fTimeTableTime: TDateTime;

    // new
    fBusData: TBusData;
    fSliderData: TSliderData;

    procedure initChargerTypes(aSession: TOraSession);

    procedure readBusData(aSession: TOraSession; const aTablePrefix: string);
    procedure readSliderData(aSession: TOraSession; const aTablePrefix: string);
    procedure addDrivingBussesToPreviousStop;

    procedure updateStop(const aID: string; aStop: TBusStop2; aObject: TSimpleObject);
    procedure showStops;
    procedure updateStopsBase;
    function UpdateStopsOnTime(aTime: TDateTime): TDateTime;

    procedure formEditChargeLocation(const aChargeLocation: string; aClient: TClient);

    function  jsonTimesliderData: String;
    procedure handleUpdateLayerObject(aClient: TClient; aPayload: TJSONObject); override;
    procedure HandleOnChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
    procedure HandleDataUpdate(aTimer: TTimer; aTime: THighResTicks);
  public
    procedure HandleSelectedEvent(aClient: TClient; aMessage: TJSONValue);
    procedure handleNewTime(aClient: TClient; aTime: string);
    function  HandleClientSubscribe(aClient: TClient): Boolean; override;
    procedure HandleTimeSliderEvent(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject);

    procedure SignalRecalc;
  end;

  TSantosProject = class(TUSProject)
    constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection;
                       const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
                       aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean;
                       aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string; aSourceEPSG: Integer);
    destructor Destroy; override;
  private
    fTimeSliderTimer: TTimer;
    fCurrentBusBlock: Integer;
    fCurrentChargerID: Integer;
    fCurrentChargerStopName: string;
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
    procedure UpdateBusDefaults;
  end;

implementation

//const
  // TODO: DB Legend? Ini?
  //NoChargeColor: TAlphaRGBPixel = $FFFF0000;
  //LowChargeColor: TAlphaRGBPixel = $FFFFA500;
  //MediumChargeColor: TAlphaRGBPixel = $FFFFFF00;
  //HighChargeColor: TAlphaRGBPixel = $FF008000;
  //Nothing: TAlphaRGBPixel = $AAAAAAAA;

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
  aStartScenario: string; aSourceEPSG: Integer);
var
  measureCat: TMeasureCategory;
begin
  fSantosLayers := TObjectDictionary<TScenario, TSantosLayer>.Create([]);
  fCurrentBusBlock := 1001; // Param?
  fCurrentChargerID := -1;
  fCurrentChargerStopName := '';

  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL,
                   aDataSource, aDBConnection, aMapView, aPreLoadScenarios , False, aMaxNearestObjectDistanceInMeters,
                   aSourceEPSG);

  EnableControl(modelControl);
  SetControl('timeslider', '1');

  fTimeSliderTimer := Timers.CreateInactiveTimer;
  fTimeSliderTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*0.3);

  clientMessageHandlers.Add('timeslider',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue)
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
        if aPayload.TryGetValue<TJSONValue>('selectedEvent', selectedEvent)
        then l.HandleSelectedEvent(aClient, selectedEvent);
      end;
    end);

  measureCat := TMeasureCategory.Create('bus', 'Bus', 'Adjust e-bus parameters', 'Adjust e-bus parameters', '', 'Set', ''{'"BatteryCapacity AverageEnergyConsumption, DriverEfficiency,PowerConsumptionHVAC'}, 1);
  Measures.Add(tagEditBusParameters, measureCat);
  EnableControl(measuresControl);

  LoadBusDefaults;

  ClientMessageHandlers.AddOrSetValue('measure',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue)
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
      TMonitor.Enter(aClient.formResultHandlers);
      try
        aClient.formResultHandlers.AddOrSetValue(formID, TFormDialogResultHandling.Create(
          procedure(aClient: TClient; aResult: TFormDialogResults)
          begin
            // handle form result
            fBatteryCapacity := double.Parse(aResult.properties['BatteryCapacity'].Value, dotFormat);
            fAverageEnergyConsumption := double.Parse(aResult.properties['AverageEnergyConsumption'].Value, dotFormat);
            fDriverEfficiency := double.Parse(aResult.properties['DriverEfficiency'].Value, dotFormat);
            fPowerConsumptionHVAC := double.Parse(aResult.properties['PowerConsumptionHVAC'].Value, dotFormat);
            UpdateBusDefaults;
          end));
      finally
        TMonitor.Exit(aClient.formResultHandlers);
      end;
      // Push json to client
      aClient.signalString(_openformdialog);
    end);

end;

destructor TSantosProject.Destroy;
begin
  FreeAndNil(fSantosLayers);
  inherited;
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
  //tableExists: boolean;
const
  SocChartID = 'santosSoc';
begin
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_BLOCK +
               aBlockID.ToString.PadLeft(cINDIC_DAT_TYPE_ID_LENGTH,'0') + cINDIC_DAT_BLOCK_SOC;

  //tableExists := MyOraLib.tableExists(aSession, tableName);

  TMonitor.Enter(aScenario.Charts);
  try
    isNew := not aScenario.Charts.TryGetValue(SocChartID, socChart);
  finally
    TMonitor.Exit(aScenario.Charts);
  end;

  if isNew {and tableExists} then
  begin
    socChart := TChartLines.Create(aScenario, 'Santos' , SocChartID, 'State of Charge',
                 'State of Charge', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'{, True, 0, 36*60*60}),
              [ TChartAxis.Create('State of Charge (%)', 'lightBlue', 'Dimensionless', '%', True, -100, 100)]);

  end;

  if Assigned(socChart) then
  begin
    TMonitor.enter(socChart);
    try
      socChart.Show();
      socChart.description := 'State of Charge ('+aBlockID.ToString+')';
       // Clear
      if not isNew
      then socChart.reset;

      //if tableExists then
      //begin
      try
        query := TOraQuery.Create(nil);
        try
          query.Session := aSession;
          query.SQL.Text :=
            'SELECT X,Y,LPAD(TRIM(TIME), 5, ''0'') as TIME ' +
            'FROM ' + tableName + ' ORDER BY TIME ASC';
          query.Open;
          while not Query.Eof do
          begin
            t := SantosTimeToDateTime(Query.FieldByName('TIME').AsString, fBaseDay);
            minOfDay := MinutesBetween(zero, t) *60;
            (socChart as TChartLines).AddValue(minOfDay, [Query.FieldByName('Y').AsFloat/100]);
            Query.Next;
          end;
        finally
          query.Free;
        end;
      except
        on E: Exception
        do Log.WriteLn('Ignoring exception reading graph data for '+tableName+': '+e.Message, llError);
        // ignore errors
      end;

      if isNew then
        aScenario.addChart(socChart);
      //end;
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
  //if aName.IsEmpty
  //then name := ' Charge Location ' + aCharger.ToString
  //else name := aName;
  name := aName;
  {$IFDEF DEBUG}
  Log.WriteLn('Loading peak busses chart for: ' + name);
  {$ENDIF}
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(cINDIC_DAT_TYPE_ID_LENGTH,'0') + cINDIC_DAT_GRID_PEAK_BUSSES;

  tableExists := MyOraLib.TableExists(aSession, tableName);

  chartID := ChgPwrChartID;// + aCharger.ToString;

  TMonitor.Enter(aScenario.Charts);
  try
    isNew := not aScenario.Charts.TryGetValue(chartID, chgPeakBussesChart);
  finally
    TMonitor.Exit(aScenario.Charts);
  end;

  if isNew and tableExists then
  begin
    chgPeakBussesChart :=
      TChartLines.Create(
        aScenario, 'Santos' , chartID, 'Peak Busses',
        'Peak Busses', True, 'line',
        TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'),
        [TChartAxis.Create('Peak busses (-)', 'lightBlue', 'Dimensionless', '-')]);
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
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(cINDIC_DAT_TYPE_ID_LENGTH,'0') + cINDIC_DAT_GRID_TOTALPOWER;

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
  tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(cINDIC_DAT_TYPE_ID_LENGTH,'0') + cINDIC_DAT_GRID_PEAK_BUSSES_WARN;

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
              (self as TMCProject).controlInterface.DataSource, tablePrefix, fIMB3Connection.Publish('EBUS_CHARGELOCATION'), indic_event, soc_event, sourceProjection);
          santosLayer.LoadPreviewFromCache('santos.png');
          TMonitor.Enter(santosLayer.fSliderData);
          try
            // show charts for first charger
            for bs in santosLayer.fSliderData.BusStops. Values do
            begin
              if Assigned(bs.charger) and (bs.charger.numberOfPoles>0) then
              begin
                ReadChartChargerTotalPower(oraSession, Result, tablePrefix, bs.charger.objectID, bs.name);
                ReadChartChargerPeakBusses(oraSession, Result, tablePrefix, bs.charger.objectID, bs.name);
                ReadChartChargerWarnings(oraSession, Result, tablePrefix, bs.charger.objectID, bs.name);
                break;
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
  aPubEntry: TIMBEventEntry; aIndicEntry, aSocEntry: TIMBEventEntry;
  aSourceProjection: TGIS_CSProjectedCoordinateSystem);
var
  oraSession: TOraSession;
  //entries: TPaletteRampEntryArray;
  entries: TPaletteDiscreteEntryArray;
begin
  // TODO
  inherited Create(aScenario, aDomain, aID, aName, aDescription);
  fSourceProjection := aSourceProjection;// CSProjectedCoordinateSystemList.ByEPSG(28992);
  fTablePrefix := aTablePrefix;
  fBlockID := aBusBlock;
  fEventEntry := aPubEntry;
  fConnectString := aConnectString;
  fUpdateTimer := fScenario.project.Timers.CreateInactiveTimer;
  fUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*5);
  fLastUpdate := hrtNow;
  fCurrentTime := Now;
  fTimeTableTime := fCurrentTime;

  setLength(fBaseDay, 3);
  fBaseDay[0] := aBaseDay;
  fBaseDay[1] := aBaseMonth;
  fBaseDay[2] := aBaseYear;

  fStopObjects := TList<TSimpleObject>.Create;
  fChargerTypes := TStringList.Create;

  fBusData := TBusData.Create;
  fSliderData := TSliderData.Create;

  {
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
  }

  setlength(entries, 3);
  entries[0] := TDiscretePaletteEntry.Create(TGeoColors.Create(RGBToAlphaColor(255, 0, 0)), 0, 25, '0-25%');
  entries[1] := TDiscretePaletteEntry.Create(TGeoColors.Create(RGBToAlphaColor(255, 204, 0)), 25, 35, '25-35%');
  entries[2] := TDiscretePaletteEntry.Create(TGeoColors.Create(RGBToAlphaColor(51, 204, 51)), 35, 100, '35-100%');

  fSoCPalette := TDiscretePalette.Create('State of Charge', entries, TGeoColors.Create($AAAAAAAA));
  legendJSON := BuildDiscreteLegendJSON(fSoCPalette as TDiscretePalette, TLegendFormat.lfVertical);

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
      initChargerTypes(oraSession);
      Log.WriteLn('finished loading data for '+fTablePrefix, llNormal);
      showStops;
    end;

  finally
    oraSession.Free;
  end;

  // todo: link to update of chart aIndicEntry.OnChangeObject := HandleOnChangeObject;
  aIndicEntry.OnChangeObject := HandleOnChangeObject;
  aSocEntry.OnChangeObject := HandleOnChangeObject;
end;

destructor TSantosLayer.Destroy;
begin
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
      // add handler for form result
      TMonitor.Enter(aClient.formResultHandlers);
      try
        aClient.formResultHandlers.AddOrSetValue(formID, TFormDialogResultHandling.Create(
          procedure(aClient: TClient; aResult: TFormDialogResults)
          var
            location: TFormDialogResultProperty;
            strPoleType: string;
            poleCount: Integer;
            maxPower: Double;
            oraSession: TOraSession;
            oraQuery: TOraQuery;
          begin
            // todo: handle form result
            if aResult.Properties.TryGetValue('LOCATION', location) then
            begin
              strPoleType := aResult.Properties['CHARGEPOLETYPE'].Value;
              poleCount :=  aResult.Properties['NUMBEROFCHARGINGPOLES'].Value.ToInteger();
              maxPower := aResult.Properties['MAXPOWER'].Value.ToDouble();

              TMonitor.Enter(fSliderData);
              try
                if fSliderData.BusStops.TryGetValue(location.Value, stop) then
                begin
                  stop.charger.poleType := strPoleType; // aResult.Properties['CHARGEPOLETYPE'].Value;  //;
                  stop.charger.numberOfPoles := poleCount;
                  stop.charger.maxPower := maxPower;

                  (scenario.project as TSantosProject).fCurrentChargerID := stop.charger.objectID;
                  (scenario.project as TSantosProject).fCurrentChargerStopName := stop.name;

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
                            'MAXPOWER='+maxPower.ToString(dotFormat)+' '+
                          'WHERE OBJECT_ID='+stop.charger.objectID.ToString;
                      oraQuery.Execute;
                      oraSession.Commit;
                    finally
                      oraQuery.free;
                    end;

                    (scenario.project as TSantosProject).ReadChartChargerPeakBusses(oraSession, scenario, fTablePrefix, (scenario.project as TSantosProject).fCurrentChargerID, (scenario.project as TSantosProject).fCurrentChargerStopName);

                  finally
                    oraSession.Free;
                  end;

                  fEventEntry.SignalChangeObject(actionChange, stop.charger.objectID);
                  log.WriteLn('UPDATED CHARGELOCATION ' + stop.id + ' to ' + polecount.ToString + ' chargers of type: ' + strPoleType, llOK);
                end
                else Log.WriteLn('Stop '+location.value+' not found to update charger', llError);
              finally
                TMonitor.Exit(fSliderData);
              end;
            end;
          end));
      finally
        TMonitor.Exit(aClient.formResultHandlers);
      end;
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

procedure TSantosLayer.handleNewTime(aClient: TClient; aTime: string);
var
  client: TClient;
begin
  fCurrentTime := StrToDateTime(aTime, isoDateTimeFormatSettings);
  client := aClient;
  forEachSubscriber<TClient>(procedure (aClient: TClient)
    begin
      if aClient <> client then
        aClient.signalString('{"type":"timesliderEvents","payload":{"setCurrentTime":"'+FormatDateTime(publisherDateTimeFormat, fCurrentTime)+'"}}');
    end);

  fTimeTableTime := UpdateStopsOnTime(fCurrentTime);
end;

procedure TSantosLayer.HandleOnChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string);
begin
  try
    fUpdateTimer.Arm(DateTimeDelta2HRT(dtOneSecond), HandleDataUpdate);
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
  contextmenuClick: TJsonObject;
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
          if fSliderData.fTimeTable.TryGetValue(fTimeTableTime, sr) then
          begin
            if sr.BusStops.TryGetValue(bs, br) then
            begin
              if br.TryGetValue(busid, bde) then
              begin
                oraSession := TOraSession.Create(nil);
                try
                  oraSession.connectString := fConnectString;
                  oraSession.Open;
                  (scenario.project as TSantosProject).fCurrentBusBlock := busid;
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
  end
  else inherited;
end;

function TSantosLayer.jsonTimesliderData: String;
var
  color: string;
  times: TArray<TDateTime>;
  t: TDateTime;
  sr: TSliderRecord;
  soc: Double;
  lastColor: string;
  lastTime: TDateTime;
  curTime: TDateTime;
  entry: TJSONObject;
begin
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
  bde: TBusDataEntry;
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
        if not bdes.TryGetValue(at, bde) then
        begin
          bde := TBusDataEntry.Create(at, tripName, stopID, soc);
          bdes.AddOrSetValue(at, bde);
        end
        else
        begin
          bde.tripName := tripName;
          bde.stopID := stopID;
          bde.soc := soc;
        end;
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

function TSantosLayer.UpdateStopsOnTime(aTime: TDateTime): TDateTime;
var
  ttt: TArray<TDateTime>;
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
      Result := ttt[0]; // sentinel: lowest value
      for t := length(ttt)-1  downto 0 do
      begin
        if ttt[t]<=aTime then
        begin
          Result := ttt[t];
          break;
        end;
      end;
      // process data on slider time
      if fSliderData.TimeTable.TryGetValue(Result, sl) then
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
    end
    else Result := aTime;
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


      // update slider
      jsonTSData := jsonTimesliderData;
      scenario.forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          // send data to time slider
          aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
        end);
      // update base stop options
      updateStopsBase;
      // update stops based on time slider time
      fTimeTableTime := UpdateStopsOnTime(fCurrentTime);

      if (scenario.project as TSantosProject).fCurrentBusBlock>=0
      then (scenario.project as TSantosProject).ReadChartBlockSoC(oraSession, scenario, fTablePrefix, (scenario.project as TSantosProject).fCurrentBusBlock);

      if (scenario.project as TSantosProject).fCurrentChargerID>=0
      then (scenario.project as TSantosProject).ReadChartChargerPeakBusses(oraSession, scenario, fTablePrefix, (scenario.project as TSantosProject).fCurrentChargerID, (scenario.project as TSantosProject).fCurrentChargerStopName);

    finally
      oraSession.Free;
    end;
    Log.WriteLn('Data changed: handled', llOK);
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

end.
