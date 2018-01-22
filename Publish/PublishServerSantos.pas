unit PublishServerSantos;

// TODO -oPW 24+hr timestamps parsen
// TODO -oPW Chargeloc. data aanpassen
// TODO -oPW onclick TSimpleObject?


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

  Generics.Collections,

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

  cINDIC_DAT_TYPE_BLOCK = '10';
  cINDIC_DAT_TYPE_BUSTYPE = '20';
  cINDIC_DAT_TYPE_GRID = '30';

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

  sNoRoute = 'NONE/UNKNOWN';

type
  TBusStop = record // Todo based on US/Santos data
    id: string;
    name: string;
    lat, lon: double;
    chargePoleType: string;
    numberOfPoles: Integer;
    maxPower: Integer;
    chargeID: Integer;
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

  TSantosLayer = class(TSimpleLayer)
    constructor Create(aScenario: TScenario; aBusBlock: Integer;
        aBaseDay, aBaseMonth, aBaseYear: Word;
        const aDomain, aID, aName, aDescription, aConnectString, aTablePrefix: string);
    destructor Destroy; override;
  protected
    fBusStops: TDictionary<String, TBusStop>;
    fStopObjects: TList<TSimpleObject>;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem; // ref
    fTimeTable: TList<TTimeStop>;
    fTablePrefix: String;
    fBlockID: Integer;
    fSoCPalette: TRampPalette;
    fTimeSliderTimer: TTimer;
    fBaseDay: array of word; // [d,m,y]
    fChargerTypes: TStrings;

    procedure initStops(aSession: TOraSession);
    procedure initTimeTable(aSession: TOraSession);
    procedure initChargerTypes(aSession: TOraSession);

    procedure formEditChargeLocation(const aChargeLocation: string; aClient: TClient);

    function jsonTimesliderData: String;
    procedure handleUpdateLayerObject(aClient: TClient; aPayload: TJSONObject); override;
  public
    procedure HandleSelectedEvent(aClient: TClient; aMessage: TJSONValue);
    procedure handleNewTime(aClient: TClient; aTime: string);
    function  HandleClientSubscribe(aClient: TClient): Boolean; override;
    procedure HandleFormResult(aFormResult: TJSONObject);
    procedure HandleTimeSliderEvent(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject);
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
    fSantosLayer: TSantosLayer;
    fBaseDay: Array of Word;
  protected
    procedure ReadChartBlockSoC(aSession: TOraSession; const aTablePrefix: string);
    procedure ReadChartChargerTotalPower(aSession: TOraSession; const aTablePrefix: string; aCharger: Integer);
  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  end;


implementation

function SantosTimeToDateTime(const aTime: string; aBaseDay: Array of word): TDateTime;
var
  parts: TArray<String>;
  d, h,m: Integer;
begin
  if aTime.IsEmpty then
    Result := EncodeDateTime(aBaseDay[2], aBaseDay[1], aBaseDay[0], 0, 0, 0, 0)
  else
  begin
    parts := aTime.Trim.Split([':']);
    if TryStrToInt(parts[0], h) and TryStrToInt(parts[1], m) then
    begin
      d := aBaseDay[0];
      while h>=24 do
      begin
        Inc(d);
        h := h-24;
      end;
      Result := EncodeDateTime(aBaseDay[2], aBaseDay[1], d, h, m, 0, 0)
    end
    else
      Result := EncodeDateTime(aBaseDay[2], aBaseDay[1], aBaseDay[0], 0, 0, 0, 0);
  end;
end;

function MD5Sum(s: String): string;
begin
   Result := System.hash.THashMD5.GetHashString(s);
end;

{ TSantosProject }

constructor TSantosProject.Create(aSessionModel: TSessionModel;
  aConnection: {IMB4} TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aStartScenario: string);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL,
                   aDataSource, aDBConnection, aMapView, aPreLoadScenarios , False, aMaxNearestObjectDistanceInMeters);

  EnableControl(modelControl);
  SetControl('timeslider', '1');

  fTimeSliderTimer := Timers.CreateInactiveTimer;
  fTimeSliderTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*0.5);
end;

destructor TSantosProject.Destroy;
begin

  inherited;
end;

procedure TSantosProject.handleClientMessage(aClient: TClient;
  aScenario: TScenario; aJSONObject: TJSONObject);
var
  formResult: TJSONObject;
begin
  inherited;
  if aJSONObject.TryGetValue<TJSONObject>('formResult', formResult) then
  begin
    fSantosLayer.handleFormResult(formResult);
  end;
end;

procedure TSantosProject.ReadBasicData;
var
  tablePrefix: string;
  oraSession: TOraSession;
  scenarioID: Integer;
  y,m,d: Word;
begin
  inherited;
  fCurrentBusBlock := 2; // Param?
  setLength(fBaseDay, 3);
  DecodeDate(now, y,m,d);
  fBaseDay[0] := d;
  fBaseDay[1] := m;
  fBaseDay[2] := y;

  oraSession := TOraSession.Create(nil);
  try
    oraSession.ConnectString := (self as TMCProject).controlInterface.DataSource;
    oraSession.Open;

    scenarioID := GetCurrentScenarioID(oraSession);
    tablePrefix := GetScenarioTablePrefix(oraSession, scenarioID);
    ReadChartBlockSoC(oraSession, tablePrefix);
    ReadChartChargerTotalPower(oraSession, tablePrefix, 2); // TODO
  finally
    oraSession.Free;
  end;

  fSantosLayer := TSantosLayer.Create(fProjectCurrentScenario, fCurrentBusBlock, d,m,y, 'Santos', 'Santos'+fCurrentBusBlock.ToString,
      'Bus block '+fCurrentBusBlock.ToString, 'Bus block '+fCurrentBusBlock.ToString+' stops',
      (self as TMCProject).controlInterface.DataSource, tablePrefix);

  // Handle time slider
  clientMessageHandlers.Add('timeslider',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
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
            fSantosLayer.handleNewTime(aClient, selectedTime);
          end);
      end;
      if aPayload.TryGetValue<TJSONValue>('selectedEvent', selectedEvent) then
      begin
          fSantosLayer.HandleSelectedEvent(aClient, selectedEvent);
      end;
      if aPayload.TryGetValue<TJSONValue>('brush', brush) then
      begin  // Time range selection
        Log.WriteLn('brush: '+brush.toJSON, llWarning);
      end;
    end);
  // Add the layer
  fProjectCurrentScenario.AddLayer(fSantosLayer);
end;

procedure TSantosProject.ReadChartBlockSoC(aSession: TOraSession; const aTablePrefix: string);
var
  zero: TDateTime;
  minOfDay: Integer;
  query: TOraQuery;
  tableName: String;
  isNew: Boolean;
  socChart: TChart;
  t: TDateTime;
const
  SocChartID = 'santosSoc';
begin
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  //StrToTime('00:00');

  TMonitor.Enter(fProjectCurrentScenario.Charts);
  try
    isNew := not fProjectCurrentScenario.Charts.TryGetValue(SocChartID, socChart);
  finally
    TMonitor.Exit(fProjectCurrentScenario.Charts);
  end;

  if isNew then
  begin
    socChart := TChartLines.Create(fProjectCurrentScenario, 'Santos' , SocChartID, 'State of Charge',
                 'State of Charge', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'),
              [ TChartAxis.Create('State of Charge (%)', 'lightBlue', 'Dimensionless', '%')]);
  end;

  TMonitor.enter(socChart);
  try
     // Clear
    if not isNew then
      socChart.reset;

    tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_BLOCK + fCurrentBusBlock.ToString.PadLeft(2,'0') + cINDIC_DAT_BLOCK_SOC;

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
      fProjectCurrentScenario.addChart(socChart);
  finally
    TMonitor.Exit(socChart);
  end;
end;

procedure TSantosProject.ReadChartChargerTotalPower(aSession: TOraSession;
  const aTablePrefix: string; aCharger: Integer);
var
  zero, t: TDateTime;
  minOfDay: Integer;
  query: TOraQuery;
  tableName: String;
  isNew: Boolean;
  socChart: TChart;
  chartID: string;
const
  ChgPwrChartID = 'santosChgPwr';
begin
  zero := SantosTimeToDateTime('00:00', fBaseDay);
  chartID := ChgPwrChartID + aCharger.ToString;

  TMonitor.Enter(fProjectCurrentScenario.Charts);
  try
    isNew := not fProjectCurrentScenario.Charts.TryGetValue(chartID, socChart);
  finally
    TMonitor.Exit(fProjectCurrentScenario.Charts);
  end;

  if isNew then
  begin
    socChart := TChartLines.Create(fProjectCurrentScenario, 'Santos' , chartID, 'Charge Location ' + aCharger.ToString + ' Total Power',
                  'Charge Location ' + aCharger.ToString + ' Total Power', True, 'line',
              TChartAxis.Create('Time (hour of day)', 'lightBlue', 'Time', 'h'),
              [ TChartAxis.Create('Total Power (-)', 'lightBlue', 'Dimensionless', '-')]);
  end;

  TMonitor.enter(socChart);
  try
     // Clear
    if not isNew then
      socChart.reset;

    tableName := aTablePrefix + 'EBUS_INDIC_DAT' + cINDIC_DAT_TYPE_GRID + aCharger.ToString.PadLeft(2,'0') + cINDIC_DAT_GRID_TOTALPOWER;

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
          (socChart as TChartLines).AddValue(
            minOfDay,
            [Query.FieldByName('Y').AsFloat]
          );
          Query.Next;
        end;

    finally
      query.Free;
    end;

    if isNew then
      fProjectCurrentScenario.addChart(socChart);
  finally
    TMonitor.Exit(socChart);
  end;
end;

{ TSantosLayer }

constructor TSantosLayer.Create(aScenario: TScenario; aBusBlock: Integer;
  aBaseDay, aBaseMonth, aBaseYear: Word;
  const aDomain, aID, aName, aDescription, aConnectString, aTablePrefix: string);
var
  oraSession: TOraSession;
  entries: TPaletteRampEntryArray;
const
  NoChargeColor: TAlphaRGBPixel = $FFFF0000;
  LowChargeColor: TAlphaRGBPixel = $FFFFA500;
  MediumChargeColor: TAlphaRGBPixel = $FFFFFF00;
  HighChargeColor: TAlphaRGBPixel = $FF008000;
  Nothing: TAlphaRGBPixel = $AAAAAAAA;
begin
  // TODO
  inherited Create(aScenario, aDomain, aID, aName, aDescription);
  fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(28992);
  fTablePrefix := aTablePrefix;
  fBlockID := aBusBlock;

  setLength(fBaseDay, 3);
  fBaseDay[0] := aBaseDay;
  fBaseDay[1] := aBaseMonth;
  fBaseDay[2] := aBaseYear;

  fBusStops := TDictionary<String, TBusStop>.Create;
  fStopObjects := TList<TSimpleObject>.Create;
  fTimeTable := TList<TTimeStop>.Create;
  fChargerTypes := TStringList.Create;

  setlength(entries, 7);
  entries[0] := TRampPaletteEntry.Create(NoChargeColor, 1, '0');
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
    initStops(oraSession);
    initTimeTable(oraSession);
    initChargerTypes(oraSession);
  finally
    oraSession.Free;
  end;
end;

destructor TSantosLayer.Destroy;
begin
  FreeAndNil(fBusStops);
  FreeandNil(fTimeTable);
  FreeAndNil(fChargerTypes);
  FreeAndNil(fStopObjects);
  inherited;
end;

procedure TSantosLayer.formEditChargeLocation(const aChargeLocation: string; aClient: TClient);
var
  chargerTypes: TArray<String>;
  formID: string;
  stop: TBusStop;
  formTitle: string;
  _openformdialog: string;
  _formElements: string;

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


begin
  TMonitor.Enter(fChargerTypes);
  try
    chargerTypes := fChargerTypes.ToStringArray;
  finally
    TMonitor.Exit(fChargerTypes);
  end;

  TMonitor.Enter(fBusStops);
  try
    if not fBusStops.TryGetValue(aChargeLocation, stop) then
      raise Exception.Create('No such charger: ' + aChargeLocation);
  finally
    TMonitor.Exit(fBusStops);
  end;

  // ID for callback event
  formID := stop.chargeID.ToString + '.' + stop.id;

  // Title of form
  formTitle := 'Edit chargelocation: ' + stop.name;

  // Create form elements
  _formElements := openFormElement_select('LOCATION', 'LOCATION:',
                                         [stop.id], stop.id, true, true);
  _formElements := _formElements + ',' + openFormElement_select('CHARGEPOLETYPE', 'Charger type:',
                                         chargerTypes, '' {stop.chargePoleType}, true, false);
  _formElements := _formElements + ',' + openFormElement_input('NUMBEROFCHARGINGPOLES', 'Number of charging poles:', 'int',
                                         stop.numberOfPoles.ToString, true, false);
  _formElements := _formElements + ',' + openFormElement_input('MAXPOWER', 'Maximum power:', 'int',
                                         stop.maxPower.ToString, true, false);

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

function TSantosLayer.HandleClientSubscribe(aClient: TClient): Boolean;
var
  jsonTSData: string;
begin
  Result := inherited;
  // send data to time slider
  jsonTSData := jsonTimesliderData;
  aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
end;

procedure TSantosLayer.HandleFormResult(aFormResult: TJSONObject);
var
  parameters: TJSONArray;
  parameter: TJSONValue;
  name: string;
  strLocation: string;
  strPoleType: string;
  poleCount: integer;
  stop: TBusStop;
  maxPower: integer;
begin
  if aFormResult.TryGetValue<TJSONArray>('parameters', parameters) then
  begin
    for parameter in parameters do
    begin
      name := (parameter as TJSONObject).GetValue<string>('name');

      if name.Equals('LOCATION') then
        strLocation := (parameter as TJSONObject).GetValue<string>('value')
      else if name.Equals('CHARGEPOLETYPE') then
        strPoleType := (parameter as TJSONObject).GetValue<string>('value')
      else if name.Equals('NUMBEROFCHARGINGPOLES') then
        poleCount :=  (parameter as TJSONObject).GetValue<integer>('value')
      else if name.Equals('MAXPOWER') then
        maxPower :=  (parameter as TJSONObject).GetValue<integer>('value');
    end;

    TMonitor.Enter(fBusStops);
    try
      if not fBusStops.TryGetValue(strLocation, stop) then
        raise Exception.Create('Unknown charge location: ' + strLocation);

      TMonitor.Enter(fChargerTypes);
      try
        if fChargerTypes.IndexOf(strPoleType)<0 then
          raise Exception.Create('Unknown charger type: ' + strPoleType);
        stop.chargePoleType := strPoleType;
      finally
        TMonitor.Exit(fChargerTypes);
      end;

      if poleCount<0 then
        raise Exception.Create('Invalid number of chargers: ' + polecount.ToString);
      if maxPower<0 then
        raise Exception.Create('Invalid max power ' + maxPower.ToString);

      stop.numberOfPoles := poleCount;
      stop.maxPower := maxPower;

//        fBusStops.Remove(stop.id);
      fBusStops.AddOrSetValue(stop.id, stop);
      // TODO, update database
      log.WriteLn('UPDATE CHARGELOCATION ' + stop.id + ' to ' + polecount.ToString + ' chargers of type: ' + strPoleType, llSummary);
    finally
      TMonitor.Exit(fBusStops);
    end;


  end;


end;

procedure TSantosLayer.handleNewTime(aClient: TClient; aTime: string);
var
  time: TDateTime;
  item: TTimeStop;
  stopID: string;
  o: TSimpleObject;
  found: Boolean;
  i, iNearest: Integer;
  iMax: Integer;
  items: TDictionary<string, TTimeStop>;
  done: Boolean;
  nearest, item2: TTimeStop;
  J: Integer;
  stop: TBusStop;
  ssid: string;
begin
  time := StrToDateTime(aTime, isoDateTimeFormatSettings);
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
  objectID, stopID: string;
  contextmenuClick: TJsonObject;
  tag: string;
begin
  objectID := aPayload.GetValue<string>('objectid');

  if aPayload.TryGetValue<TJsonObject>('contextmenuClick', contextmenuclick) then
  begin
    if objectID.StartsWith(idPrefixBusStop) and
       contextmenuclick.TryGetValue<string>('tag', tag) and
       tag.Equals(tagEditChargeLocation) then
    begin
      stopID := objectID.SubString(idPrefixBusStop.Length);
      formEditChargeLocation(stopID, aClient);
    end;
  end
  else
    inherited;
end;

function TSantosLayer.jsonTimesliderData: String;
var
  stop: TTimeStop;
  startTime, endTime, color, entry, tt: string;
begin
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
end;

procedure TSantosLayer.initChargerTypes(aSession: TOraSession);
var
  query: TOraQuery;
begin
  query := TOraQuery.Create(nil);
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
var
  query: TOraQuery;
  stop: TBusStop;
  stopPair: TPair<string, TBusStop>;
  marker: TCircleMarker;
  p: TGIS_Point;
  o: TSimpleObject;
  removeObjects: TList<TSimpleObject>;
begin
  TMonitor.Enter(fBusStops);
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
      query.SQL.Text :=
        'SELECT DISTINCT ' +
          'tt.STOP_DESC, tt.PLACE, ' +
          'node.XCOORD, node.YCOORD, ' +
          ' c.CHARGEPOLETYPE, c.NUMBEROFCHARGINGPOLES, c.MAXPOWER, c.OBJECT_ID ' +
        'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
          'LEFT OUTER JOIN '+fTablePrefix+'GENE_NODE node '+
            'ON node.HALTE_NAME=tt.STOP_DESC ' +
          'LEFT OUTER JOIN '+fTablePrefix+'EBUS_CHARGELOCATION c ' +
            'ON c.LOCATION=tt.PLACE ' +
        'WHERE tt.BLOCK=' + fBlockID.ToString ;
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
          // Dump on the runway
          stop.lat := 52.306625;
          stop.lon :=  4.778465;
          Log.WriteLn('No location known for stop ID: ' + stop.id + ', name: ' + stop.name, llWarning);
        end;
        if not query.FieldByName('CHARGEPOLETYPE').IsNull then
        begin
          stop.chargePoleType := query.FieldByName('CHARGEPOLETYPE').AsString;
          stop.numberOfPoles := query.FieldByName('NUMBEROFCHARGINGPOLES').AsInteger;
          stop.maxPower := query.FieldByName('MAXPOWER').AsInteger;
          stop.chargeID := query.FieldByName('OBJECT_ID').AsInteger;
        end
        else
        begin
          stop.chargePoleType := '';
          stop.numberOfPoles := -1;
          stop.maxPower := -1;
          stop.chargeID := -1;
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
              marker.addOptionStructure(sojnContextmenuItems, '[{"text": "Edit charge location","index": '+stop.chargeID.ToString+', "tag":"'+tagEditChargeLocation+'"}]');
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
end;

procedure TSantosLayer.initTimeTable(aSession: TOraSession);
var
  query: TOraQuery;
  ttEntry, ttEntryPrev: TTimeStop;
  i: Integer;
//  today: string;
  tableName: string;
//  y,m,d: word;
begin
  tableName := 'EBUS_INDIC_DAT10' + fBlockID.ToString.PadLeft(2,'0') + '01';

//  today := FormatDateTime('dd/mm/yyyy', now);
//  setLength(fBaseDay, 3);
//  DecodeDate(now, y,m,d);
//  fBaseDay[0] := d;
//  fBaseDay[1] := m;
//  fBaseDay[2] := y;

  TMonitor.Enter(fTimeTable);
  try
    fTimeTable.Clear;

    query := TOraQuery.Create(nil);
    try
      query.Session := aSession;
      query.SQL.Text := 'SELECT tt.STOP_DESC, tt.PLACE, LPAD(TRIM(tt.TIME), 5, ''0'') as TIME, tt.ROUTE, tt.DIRECTION, soc.Y ' +
                        'FROM '+fTablePrefix+'EBUS_TIMETABLE tt ' +
                            'LEFT OUTER JOIN '+fTablePrefix+tableName +' soc '+
                            ' ON soc.X LIKE tt.STOP_DESC ' +
                            ' AND LPAD(TRIM(SOC.time), 5,''0'') LIKE LPAD(TRIM(tt.time), 5,''0'')' +
                        'WHERE tt.BLOCK=' + fBlockId.ToString + ' ' +
                        'ORDER BY tt.TIME ASC';
      query.Open;

      while not query.Eof do
      begin // TODO, X,Y
//        DecodeTime( .AsString), h,m,s,ms);
        ttEntry.timeStart := SantosTimeToDateTime (query.FieldByName('TIME').AsString, fBaseDay);
//        StrToDateTime(today + ' ' + query.FieldByName('TIME').AsString.Trim);
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

  finally
    TMonitor.Exit(fTimeTable);
  end;
end;

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

end.
