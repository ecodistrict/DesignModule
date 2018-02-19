unit PublishServerSesmi;

interface

uses
  Logger,

  imb4,

  StdIni,

  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,

  TimerPool,
  SensorDataSets,

  TilerControl,

  PublishServerLib,
  PublishServerGIS,

  GisCsSystems,

  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils,
  System.RegularExpressions;

const
  sensordata_longitude             = 129;               //tag 16
  sensordata_latitude              = 121;               //tag 15

  sensordata_pm10                  = 1441;              //tag 180
  sensordata_pm25                  = 1601;              //tag 200
  sensordata_no2                   = 1921;              //tag 240
  //sensordata_no2                   = 961;               //tag 120 -> temporary tag!
  //sensordata_pm1                   = 2081;              //tag 260
  //sensordata_nh3                   = 2241;              //tag 280
  //sensordata_pnc                   = 2401;              //tag 300
  //sensordata_nox                   = 2561;              //tag 320
  sensordata_linkid                = 2882;              //tag 360

  //sensordata_pm10_total            = 1449;              //tag 181
  //sensordata_pm25_total            = 1609;              //tag 201
  sensordata_no2_total             = 969;               //tag 241
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
  sensordata_assim_no2_total       = 985;               //tag 243
  //sensordata_assim_pm1_total       = 2105;              //tag 263

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

  MaxNoSensorValueTime = 5/(60*24); // 5 minutes

type
  TSesmiClient = class(TClient)
  constructor Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
  protected
    procedure Login(aJSONObject: TJSONObject); override;
  end;

  TSesmiTrackLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aShowInDomains: Boolean; const aLegendJSON: string; aPalette: TWDPalette);
  protected
  public
    procedure AddPoint(aObjectID: TGUID; aLat, aLon, aValue: Double);
    procedure reset;
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

  TSesmiScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView; aUseSimulationSetup: Boolean);
  destructor Destroy; override;
  private
    //fLinkLayers: TDictionary<Integer, TSesmiLinkLayer>;
    fTrackLayers: TDictionary<Integer, TSesmiTrackLayer>;
    fLastLats: TDictionary<TGUID, Double>;
    fLastLons: TDictionary<TGUID, Double>;
    fSensorsDataSet: TSensorsDataSet;
    fTimeSliderDataTimer: TTimer;
    fFirstTimeSliderUpdate: Boolean;
    fMobileChart: TChartLines;
    fTotalChart: TChartLines;
    fFiltered: Boolean;
    fShowDataSelectionTimer: TTimer;
    procedure triggerUpdateTimesliderData;
    function jsonTimesliderData(aTag: Integer; aPalette: TWDPalette; var aExtent: TWDExtent): string;
    procedure ShowDataSelection(aTrackLayer: TSesmiTrackLayer; aMobileChart, aTotalChart: TChartLines; aFrom, aTo: TDateTime);
  protected
    fGUID: TGUID;
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
    procedure handleUniEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure addToSensorDataSet(const aSensorid: TGUID; aTimestamp: TDateTime; aFieldInfo: UInt32; aLat, aLon, aValue: Double);
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    procedure HandleTimeSliderEvent(aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure HandleScenarioRefresh(aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure ProcessRecord(
      aTrackLayer: TSesmiTrackLayer; aMobileChart, aTotalChart: TChartLines;
      aCursor: TCursor;
      var aLat, aLon: Double;
      var aTimeStamp: TDateTime;
      var aValue, aTotalValue: Double);
  public
    procedure InquireDB(const aInquire: string; const aLowerTimestamp, aUpperTimestamp: Double);
    procedure ReadBasicData(); override;
  end;

  TSesmiProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView; const aExpertScenarioGUID: TGUID);
  private
    fExpertScenarioGUID: TGUID;
  protected
    function handleTilerStatus(aTiler: TTiler): string;
    procedure handleNewClient(aClient: TClient); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  public
    function addClient(const aClientID: string): TClient; override;
    function CreateSesmiScenario(const aScenarioID: string): TSesmiScenario;
  public
    property ExpertScenarioGUID: TGUID read fExpertScenarioGUID;
  end;

implementation

//convert a Guid to TWDID
function GuidToTWDID(aGuid: TGUID): TWDID;
begin
  SetLength(Result, SizeOf(aGuid));
  Move(aGuid, PAnsiChar(Result)^, SizeOf(aGuid));
end;

// Discrete palette for the Sesmi project

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

function CreateGrayPalette(const aTitle: string): TWDPalette;
var
  factor: Double;
begin
  factor := 1 / 1000000000;
  Result := TRampPalette.Create(aTitle, [
    TRampPaletteEntry.Create($FFDDDDDD, 0 * factor, '0'),
    TRampPaletteEntry.Create($FF000000, 120 * factor, '120')],
      $FFDDDDDD,
      $00000000,
      $FF000000);
end;

function BuildLegendJSON(aPalette: TWDPalette): string;
begin
  if aPalette is TRampPalette
  then Result := BuildRamplLegendJSON(aPalette as TRampPalette)
  else if aPalette is TDiscretePalette
  then Result := BuildDiscreteLegendJSON(aPalette as TDiscretePalette, TLegendFormat.lfVertical)
  else Result := '';
end;

{ TSesmiTrackLayer }

procedure TSesmiTrackLayer.AddPoint(aObjectID: TGUID; aLat, aLon,
  aValue: Double);
var
  wdid: TWDID;
  geometryPoint: TWDGeometryPoint;
begin
  wdid := TWDID(TGUID.NewGuid.ToString);
  geometryPoint := TWDGeometryPoint.Create;
  geometryPoint.x := aLon;
  geometryPoint.y := aLat;
  AddObject(TGeometryPointLayerObject.Create(Self, wdid, geometryPoint, aValue));
end;

constructor TSesmiTrackLayer.Create(aScenario: TScenario; const aDomain, aID,
  aName, aDescription: string; aDefaultLoad, aShowInDomains: Boolean;
  const aLegendJSON: string; aPalette: TWDPalette);
begin
  inherited Create(
    aScenario, aDomain, aID, aName, aDescription,
    aDefaultLoad, '"mobilesensor"', 'Point', ltTile, aShowInDomains, 0, False, 0.8,
    'default', aLegendJSON, '', aPalette);
end;

procedure TSesmiTrackLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500, fPalette.Clone);
end;

procedure TSesmiTrackLayer.RegisterSlice;
begin
  tilerLayer.signalAddSlice();
end;

procedure TSesmiTrackLayer.reset;
begin
  ClearObjects;
  tilerLayer.signalSliceAction(tsaClearSlice);
end;

function TSesmiTrackLayer.SliceType: Integer;
begin
  Result := stLocation;
end;

{ TSesmiScenario }

constructor TSesmiScenario.Create(aProject: TProject; const aID, aName,
  aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView;
  aUseSimulationSetup: Boolean);
begin
  if TRegEx.IsMatch(aID, '^[{][0-9A-Fa-f]{8}[-]([0-9A-Fa-f]{4}[-]){3}[0-9A-Fa-f]{12}[}]$')
  then fGUID := TGUID.Create(aID)
  else fGUID := TGUID.Empty;
  //fLive := True;
  fQueryCounter := 0;
  fQuerySubscribed := False;
  fQueryEventHandler := handleQueryEvent;
  fLiveCounter := 0;
  fDBCounter := 0;
  //fLinkLayers := TDictionary<Integer, TSesmiLinkLayer>.Create;
  fTrackLayers := TDictionary<Integer, TSesmiTrackLayer>.Create;
  fLastLats := TDictionary<TGUID, Double>.Create;
  fLastLons := TDictionary<TGUID, Double>.Create;
  fSensorsDataSet := TSensorsDataSet.Create;
  fTimeSliderDataTimer := aProject.Timers.CreateInactiveTimer;
  fShowDataSelectionTimer := aProject.Timers.CreateInactiveTimer;
  fFirstTimeSliderUpdate := True;
  fFiltered := False;
  inherited;
  fPubEvent := aProject.Connection.eventEntry('mobilesensordata').publish;
  fLiveEvent := aProject.Connection.eventEntry('mobilesensordata').subscribe;
  fLiveEventHandler := handleLiveEvent;
  fLiveEvent.OnEvent.Add(fLiveEventHandler);
  InquireDB('', Double.NaN, Double.NaN);
end;

destructor TSesmiScenario.Destroy;
begin
  CancelTimer(fTimeSliderDataTimer);
  CancelTimer(fShowDataSelectionTimer);
  if fQuerySubscribed then //unsubscribe from the previous returnEvent
  begin
    fqueryEvent.OnEvent.Remove(fQueryEventHandler);
    project.Connection.unSubscribe(fQueryEvent);
  end;
  fLiveEvent.OnEvent.Remove(fLiveEventHandler);
  project.Connection.unSubscribe(fLiveEvent);
  project.Connection.unPublish(fPubEvent);
  FreeAndNil(fTrackLayers);
  FreeAndNil(fLastLons);
  FreeAndNil(fLastLats);
  FreeAndNil(fSensorsDataSet);
  inherited;
end;

function TSesmiScenario.HandleClientSubscribe(aClient: TClient): Boolean;
var
  jsonTSData: string;
  palette: TWDPalette;
  extent: TWDExtent;
begin
  Result := inherited;
  // send data to time slider
  palette := CreateGrayPalette('NO2 slider');
  try
    extent := TWDExtent.Create;
    jsonTSData := jsonTimesliderData(sensordata_no2 shr 3, palette, extent);
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
end;

procedure TSesmiScenario.handleLiveEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
begin
  fLiveCounter := fLiveCounter + 1;
  handleUniEvent(aEventEntry, aPayload, aCursor, aLimit);
end;

procedure TSesmiScenario.handleQueryEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
begin
  fDBCounter := fDBCounter + 1;
  handleUniEvent(aEventEntry, aPayload, aCursor, aLimit);
end;

procedure TSesmiScenario.ProcessRecord(
  aTrackLayer: TSesmiTrackLayer; aMobileChart, aTotalChart: TChartLines;
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
      if sr.values[srp.Key].TryGetValue(sensordata_no2 shr 3, sensorValue) then
      begin
        aTrackLayer.AddPoint(srp.Key.IDAsGUID, aLat, aLon, sensorValue);
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

procedure TSesmiScenario.ShowDataSelection(aTrackLayer: TSesmiTrackLayer; aMobileChart, aTotalChart: TChartLines; aFrom, aTo: TDateTime);
var
  cursor: TCursor;
  loopLat: Double;
  loopLon: Double;
  loopValue: Double;
  loopTimeStamp: TDateTime;
  loopTotalValue: Double;
begin
  // clear track and charts again to handle aborted jobs
  aTrackLayer.reset;
  aTotalChart.reset;
  aMobileChart.reset;
  // rebuild data
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
end;

procedure TSesmiScenario.HandleScenarioRefresh(aClient: TClient; const aType: string; aPayload: TJSONObject);
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

procedure TSesmiScenario.HandleTimeSliderEvent(aClient: TClient; const aType: string; aPayload: TJSONObject);
var
  brush: TJSONValue;
  extent: TJSONValue;
  a: TJSONArray;
  selectedEvent: TJSONValue;
  lat, lon: TJSONValue;
  trackLayer: TSesmiTrackLayer;
  qdtFrom: TDateTime;
  qdtTo: TDateTime;
begin
  if aPayload.TryGetValue<TJSONValue>('brush', brush) then
  begin
    if brush.TryGetValue('extent', extent) then
    begin

      // HandleTimeSliderEvent: timeslider: {"brush":{"extent":["2017-10-13 18:09","2017-11-07 16:14"]}}
      // HandleTimeSliderEvent: timeslider: {"brush":{"extent":{}}}

      // add selected points to tracklayer
      if fTrackLayers.TryGetValue(sensordata_no2, trackLayer) then
      begin
        if not fShowDataSelectionTimer.Enabled then
        begin
          // clear layer and chart
          trackLayer.reset;
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

procedure TSesmiScenario.addToSensorDataSet(
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

procedure TSesmiScenario.handleUniEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  trackLayer: TSesmiTrackLayer;
  fieldInfo: UInt32;
  value, timestamp: Double;
  sensorid: TGUID;
  lastLat: Double;
  lastLon: Double;
  average: Double;
  delta: Double;
  _totalValue: Double;
begin
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
          // correct for negative sensor values (just ignore)
          if value<0
          then value := 0;
          if fLastLons.TryGetValue(sensorid, lastLon) and fLastLats.TryGetValue(sensorid, lastLat) then
          begin
            addToSensorDataSet(sensorid, timestamp, fieldInfo, lastLat, lastLon, value);
            triggerUpdateTimesliderData();

            if not fFiltered then
            begin
              if fTrackLayers.TryGetValue(fieldInfo, trackLayer) then
                trackLayer.AddPoint(sensorid, lastLat, lastLon, value);
              fMobileChart.AddValue(timeStamp, [value]);
              if fMobileChart.allValues.Count>=2 then
              begin
                average := (value+fMobileChart.allValues[fMobileChart.allValues.Count-2].y[0])/2.0;
                delta := timeStamp-fMobileChart.allValues[fMobileChart.allValues.Count-2].x;
                _totalValue := average * delta * 24 * 60;
                if fTotalChart.allValues.Count>0
                then _totalValue := _totalValue + fTotalChart.allValues[fTotalChart.allValues.Count-1].y[0];
                fTotalChart.AddValue(timeStamp, [_totalValue])
              end;
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
  if not aLowerTimeStamp.IsNaN
  then buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampLower shr 3, aLowerTimeStamp);
  if not aUpperTimeStamp.IsNan
  then buffer := buffer + TByteBuffer.bb_tag_double(wDatTimeStampUpper shr 3, aUpperTimeStamp);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, returnString);
  buffer := buffer + TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aInquire);
  fPubEvent.signalEvent(buffer);
end;

function TSesmiScenario.jsonTimesliderData(aTag: Integer; aPalette: TWDPalette; var aExtent: TWDExtent): string;
var
  entry: string;
  loopSensorValue: Double;
  cursor: TCursor;
  srp: TPair<TSensor, Integer>;
  sensorValue: Double;
  sr: TSensorsRecord;
  loopLat: Double;
  loopLon: Double;
  prevTimeStamp: TDateTime;
  stepSize: TDateTime;
  entryStartTimeStamp: TDateTime;
  loopSensorValueColor: TAlphaRGBPixel;
  entryColor: TAlphaRGBPixel;
  entryEndTimeStamp: TDateTime;
begin
  // todo: use cursor, if a sensor has no value on a specific time it is not accounted for and a higher value
  // could be shown then calculated for the time stamp
  Result := '';
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
          if (stepSize>MaxNoSensorValueTime) or (loopSensorValueColor<>entryColor) then
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
        end;
      end;
    finally
      TMonitor.Exit(fSensorsDataSet);
    end;
  finally
    fSensorsDataSet.RemoveCursor(cursor);
  end;
end;

procedure TSesmiScenario.ReadBasicData;
var
  trackpalette: TWDPalette;
  trackLayer: TSesmiTrackLayer;
begin
  trackpalette := CreateColorTrackPalette('Track NO2');

  fMobileChart :=  TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts' + 'NO2', 'NO2', 'Personal NO2', False, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [TChartAxis.Create('concentratie', 'lightBlue', 'Concentration', 'mg/m3')], 'time');
  AddChart(fMobileChart);

  fTotalChart := TChartLines.Create(Self, 'Personal exposure', 'mobilesensorcharts' + 'NO2' + 'total', 'NO2' + '-total', 'Personal NO2' + ' Total', False, 'line',
    TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
    [TChartAxis.Create('concentratie', 'lightBlue', 'Concentration', 'mg/m3')], 'time');
  AddChart(fTotalChart);

  trackLayer := TSesmiTrackLayer.Create(
    Self, 'Personal exposure', 'NO2' + 'personal-track-' + 'NO2', 'Personal Track ' + 'NO2', 'NO2',
    True, True, BuildLegendJSON(trackPalette), trackPalette);
  fTrackLayers.AddOrSetValue(sensordata_no2, trackLayer);
  AddLayer(trackLayer);
  trackLayer.RegisterLayer;
end;

procedure TSesmiScenario.triggerUpdateTimesliderData;
begin
  fTimeSliderDataTimer.Arm(DateTimeDelta2HRT(2*dtOneSecond),
    procedure (aTimer: TTimer; aTime: THighResTicks)
    var
      jsonTSData: string;
      palette: TWDPalette;
      extent: TWDExtent;
    begin
      palette := CreateGrayPalette('NO2 slider');
      try
        extent := TWDExtent.Create;
        jsonTSData := jsonTimesliderData(sensordata_no2 shr 3, palette, extent);
      finally
        palette.Free;
      end;
      ForEachClient(
        procedure(aClient: TClient)
        begin
          aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
          // set map view according data set if this is the first time time slider data is send to clients ie after inquire
          if fFirstTimeSliderUpdate and not (extent.CenterY.IsNaN or extent.CenterX.IsNaN) then
          begin
            aClient.SendView(extent.CenterY, extent.CenterX, Double.NaN);
          end;
        end);
      if not (extent.CenterY.IsNaN or extent.CenterX.IsNaN)
      then fFirstTimeSliderUpdate := False;
    end);
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
  aTilerStatusURL: string; aAddBasicLayers: Boolean; {const aDateFormData: string; }aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView; const aExpertScenarioGUID: TGUID);
begin
  fExpertScenarioGUID := aExpertScenarioGUID;
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN,
    aTilerStatusURL, nil, aAddBasicLayers, aMaxNearestObjectDistanceInMeters, aMapView, nil, nil); // todo: check projectCurrentScenario
  fTiler.onTilerStatus := handleTilerStatus;
  //Set Sesmi controls
  SetControl('timeslider', '1');
  clientMessageHandlers.Add('timeslider',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    begin
      if Assigned(aClient.currentScenario) and (aClient.currentScenario is TSesmiScenario)  then
      begin
        (aClient.currentScenario as TSesmiScenario).HandleTimeSliderEvent(aClient, aType, aPayload);
      end;
    end);
  clientMessageHandlers.Add('scenarioRefresh',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    begin
      if Assigned(aClient.currentScenario) and (aClient.currentScenario is TSesmiScenario)  then
      begin
        (aClient.currentScenario as TSesmiScenario).HandleScenarioRefresh(aClient, aType, aPayload);
      end;
    end);
end;

function TSesmiProject.CreateSesmiScenario(const aScenarioID: string): TSesmiScenario;
begin
  Result := TSesmiScenario.Create(Self, aScenarioID, 'Fietsproject', 'Persoonlijke fietsdata - ' + aScenarioID, False, MapView, False);
end;

function TSesmiProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

procedure TSesmiProject.handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject);
begin
  if assigned(aClient.currentScenario) and (aClient.currentScenario is TSesmiScenario) then
  begin
    // todo: ?
  end;
end;

procedure TSesmiProject.handleNewClient(aClient: TClient);
begin
  // todo: ?
end;

{ TSesmiClient }

constructor TSesmiClient.Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
begin
  inherited;
end;

procedure TSesmiClient.Login(aJSONObject: TJSONObject);
var
  scenarioID: string;
  userID: string;
  scenario: TScenario;
begin
  userID := aJSONObject.GetValue<string>('userid');
  scenarioID := aJSONObject.GetValue<string>('scenario'); //todo: check if scenarioID is valid GUID?
  if not TRegEx.IsMatch(scenarioID, '^[{][0-9A-Fa-f]{8}[-]([0-9A-Fa-f]{4}[-]){3}[0-9A-Fa-f]{12}[}]$') then
    exit;
  if not fProject.scenarios.TryGetValue(scenarioID, scenario) then
  begin
    scenario := (fProject as TSesmiProject).CreateSesmiScenario(scenarioID);
    fProject.scenarios.Add(scenario.ID, scenario);
  end;
  removeClient(fCurrentScenario);
  fCurrentScenario := scenario;
  addClient(fCurrentScenario);
  Log.WriteLn('connected to scenario '+scenarioID+' user '+userid);
  SendSession();
  fProject.SendDomains(self, 'domains');
end;


end.
