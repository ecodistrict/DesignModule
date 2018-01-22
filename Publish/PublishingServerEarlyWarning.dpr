program PublishingServerEarlyWarning;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger,
  LogConsole,
  LogFile,
  StdIni,
  imb4,
  imb.SocksLib,
  TimerPool,
  SensorDataSets,
  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,
  PublishServerLib,
  Data.DB,
  System.JSON,
  System.Generics.Collections,
  WinApi.Windows,
  System.SysUtils;

const
  RemoteHostSwitchName = 'RemoteHost';
  RemotePortSwitchName = 'RemotePort';

  MapViewSwitchName = 'MapView';

  DefaultSensorRadius = 100;

  MaxNoSensorValueTime = 4/24;

  wdatSensordata_Benzene = (550 shl 3) or wt64Bit;
  wdatSourceEmissionStrengthAnalysisBenzene = (561 shl 3) or wt64Bit;
  wdatLat = (15 shl 3) or wt64Bit;
  wdatLon = (16 shl 3) or wt64Bit;
  //tag_height = 17;
  wdatSensorCode = (18 shl 3) or wtLengthDelimited;

  tag_meteodata_winddirection = 16; //129;
  tag_meteodata_windspeed = 17; //137;
  tag_meteodata_moninobukhovlength = 27; //217;
  tag_meteodata_mixinglayerheight = 28; //225;
  tag_meteodata_rainfall = 35; //280;

type
  TSensorsLayer2 = class(TSimpleLayer)
  constructor Create(
    aPalette: TWDPalette;
    aEventEntrySensor, aPrivateEventEntrySensor: TEventEntry;
    aEventEntrySource, aPrivateEventEntrySource: TEventEntry;
    aEventEntryMeteo, aPrivateEventEntryMeteo: TEventEntry;
    aScenario: TScenario;
    const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aDisplayGroup: string=''; aShowInDomains: Boolean=True;
    aBasicLayer: Boolean=False; aOpacity: Double=0.8; const aLegendJSON: string='');
  destructor Destroy; override;
  private
    // data
    fPalette: TWDPalette;
    fSensorsDataSet: TSensorsDataSet; // owns records, not sensors in records, ordered by timestamp
    // events
    fEventEntrySensor: TEventEntry;
    fPrivateEventEntrySensor: TEventEntry;
    fEventEntrySource: TEventEntry;
    fPrivateEventEntrySource: TEventEntry;
    fEventEntryMeteo: TEventEntry;
    fPrivateEventEntryMeteo: TEventEntry; // only refs
    fTimeSliderDataTimer: TTimer; // ref
    fCursors: TDictionary<TClient, TCursor>;
//    fClientsToUpdate: TDictionary<TClient, Boolean>;
    fClientsToUpdateTimer: TTimer; // ref
    procedure handleSensorDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleSourceDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure setLive(aClient: TClient; const aValue: Boolean);
    function getLive(aClient: TClient): Boolean;
    procedure signalCursorValues(aClient: TClient; aCursor: TCursor; aPrivateModelSensorEvent, aPrivateModelMeteoEvent: TEventEntry);
    procedure UpdateClientsOn(aTimeStamp: TDateTime);
  protected
    function IsReceivingLayerUpdates(aClient: TClient): Boolean; override;
    function jsonTimesliderData: string;
    procedure triggerUpdateTimesliderData;
  protected
    procedure handleNewTime(aClient: TClient; const aTime: string; aPrivateSensorEvent, aPrivateMeteoEvent: TEventEntry);
    procedure handleUpdateLayerObject(aClient: TClient; aPayload: TJSONObject); override;
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  public
    property live[aClient: TClient]: Boolean read getLive write setLive;
  end;

  TProjectEarlyWarning = class(TProject)
  constructor Create(
    aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView; aProjectCurrentScenario, aProjectRefScenario: TScenario);
  destructor Destroy; override;
  private
    fPrivateModelMeteoEvent: TEventEntry;
    fPrivateModelSensorEvent: TEventEntry;
    fWindSpeed: Double;
    fWindDirection: Double;
  end;



{ utils }

function CreateEWPalette(const aTitle: string): TWDPalette;
begin
  Result := TDiscretePalette.Create(aTitle, [
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff000000), -40000, 0, '<0'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff0047ba), 0, 2, '0-2'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff6da5ff), 2, 5, '2-5'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ffffff00), 5, 10, '5-10'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ffff8000), 10, 20, '10-20'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ffff0000), 20, 50, '20-50'),
    TDiscretePaletteEntry.Create(TGeoColors.Create($Ff8100c1), 50, 20000, '>50')
  ],TGeoColors.Create($00000000)); //default: transparant
end;

{ TSensorsLayer2 }

constructor TSensorsLayer2.Create(
  aPalette: TWDPalette;
  aEventEntrySensor, aPrivateEventEntrySensor: TEventEntry;
  aEventEntrySource, aPrivateEventEntrySource: TEventEntry;
  aEventEntryMeteo, aPrivateEventEntryMeteo: TEventEntry;
  aScenario: TScenario; const aDomain, aID, aName,
  aDescription: string; aDefaultLoad: Boolean; const aDisplayGroup: string; aShowInDomains, aBasicLayer: Boolean;
  aOpacity: Double; const aLegendJSON: string);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription,
    [ (*['contextmenu', 'true'],
      ['contextmenuItems', '[{"text": "Layer item1","index": 0, "tag": 1.5},{"text": "Layer item2","index": 1, "tag": "text"}]'], // , {"separator": true, "index": 1}
      ['contextmenuInheritItems', 'true']*)
    ],
    [],
    aDefaultLoad, aDisplayGroup, aShowInDomains, aBasicLayer, aOpacity, aLegendJSON);

  fPalette := aPalette;
  fSensorsDataSet := TSensorsDataSet.Create;
  fCursors := TDictionary<TClient, TCursor>.Create;
//  fClientsToUpdate := TDictionary<TClient, Boolean>.Create;
  fClientsToUpdateTimer := scenario.project.Timers.CreateInactiveTimer;
  fTimeSliderDataTimer := scenario.project.Timers.CreateInactiveTimer;

  // sensor
  fEventEntrySensor := aEventEntrySensor;
  fEventEntrySensor.OnEvent.Add(handleSensorDataEvent);
  fEventEntrySensor.subscribe; // start listening
  fPrivateEventEntrySensor := aPrivateEventEntrySensor;
  fPrivateEventEntrySensor.OnEvent.Add(handleSensorDataEvent);
  fPrivateEventEntrySensor.subscribe; // start listening
  // signal inquire
  fEventEntrySensor.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateEventEntrySensor.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, ''));

  // source
  fEventEntrySource := aEventEntrySource;
  fEventEntrySource.OnEvent.Add(handleSourceDataEvent);
  fEventEntrySource.subscribe; // start listening
  fPrivateEventEntrySource := aPrivateEventEntrySource;
  fPrivateEventEntrySource.OnEvent.Add(handleSourceDataEvent);
  fPrivateEventEntrySource.subscribe; // start listening
  // signal inquire
  fEventEntrySource.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateEventEntrySource.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, ''));

  // meteo
  fEventEntryMeteo := aEventEntryMeteo;
  fEventEntryMeteo.OnEvent.Add(handleMeteoDataEvent);
  fEventEntryMeteo.subscribe; // start listening
  fPrivateEventEntryMeteo := aPrivateEventEntryMeteo;
  fPrivateEventEntryMeteo.OnEvent.Add(handleMeteoDataEvent);
  fPrivateEventEntryMeteo.subscribe; // start listening
  // signal inquire
  fEventEntryMeteo.signalEvent(
    TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateEventEntryMeteo.eventName)+
    TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, ''));
end;

destructor TSensorsLayer2.Destroy;
begin
  CancelTimer(fTimeSliderDataTimer);
  CancelTimer(fClientsToUpdateTimer);
//  FreeAndNil(fClientsToUpdate);
  FreeAndNil(fSensorsDataSet);
  FreeAndNil(fCursors);
  FreeAndNil(fPalette);
  inherited;
end;

function TSensorsLayer2.getLive(aClient: TClient): Boolean;
var
  cursor: TCursor;
  lastDataRecord: TSensorsRecord;
begin
  TMonitor.Enter(fCursors);
  try
    if fCursors.TryGetValue(aClient, cursor) then
    begin
      lastDataRecord := cursor.Data.Last;
      Result := (not Assigned(lastDataRecord)) or Double(cursor.CurrentTimeStamp).IsNaN or (cursor.CurrentTimeStamp>lastDataRecord.TimeStamp);
    end
    else Result := True;
  finally
    TMonitor.Exit(fCursors);
  end;
end;

function TSensorsLayer2.HandleClientSubscribe(aClient: TClient): Boolean;
var
  jsonTSData: string;
begin
  Result := inherited;
  // send data to time slider
  jsonTSData := jsonTimesliderData;
  aClient.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
end;

function TSensorsLayer2.HandleClientUnsubscribe(aClient: TClient): Boolean;
begin
  Result := inherited HandleClientUnsubscribe(aClient);
  TMonitor.Enter(fCursors);
  try
    fCursors.Remove(aClient);
  finally
    TMonitor.Exit(fCursors);
  end;
end;

procedure TSensorsLayer2.handleMeteoDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TWDID;
  ts: Double;
  value: Double;
  sensor: TSensor;
  affectsLive: Boolean;
begin
  sensor := nil;
  ts := Double.NaN;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fSensorsDataSet);
          try
            if not fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              sensor := TSensor.Create(id);
              fSensorsDataSet.Sensors.Add(id, sensor);
            end;
          finally
            TMonitor.Exit(fSensorsDataSet);
          end;
        end;
      wdatTimeStamp:
        begin
          ts := aPayload.bb_read_double(aCursor);
        end;
      (tag_meteodata_winddirection shl 3) or wt64Bit,
      (tag_meteodata_windspeed shl 3) or wt64Bit,
      (tag_meteodata_moninobukhovlength shl 3) or wt64Bit,
      (tag_meteodata_mixinglayerheight shl 3) or wt64Bit,
      (tag_meteodata_rainfall shl 3) or wt64Bit:
        begin
          value := aPayload.bb_read_double(aCursor);
          if Assigned(sensor) and not ts.IsNan then
          begin
            TMonitor.Enter(fSensorsDataSet.Data);
            try
              affectsLive := fSensorsDataSet.NewValues(sensor, ts, [fieldInfo shr 3], [value]);
            finally
              TMonitor.Exit(fSensorsDataSet.Data);
            end;
            if affectsLive then
            begin
              // show wind speed and direction if live
              if fieldInfo=(tag_meteodata_winddirection shl 3) or wt64Bit then
              begin
                //scenario.project.windControl.updateClient(value, );

              end;
              if fieldInfo=(tag_meteodata_windspeed shl 3) or wt64Bit then
              begin

              end;
              // todo: show temp if live
            end
            // else // todo: affect specific clients looking at timestamp ts
          end;
          // else un-timestamped source value -> ignore
        end;
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fSensorsDataSet);
          try
            if fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              fSensorsDataSet.Sensors.Remove(id);
              // todo: cleanup history and invalidate cursors?
            end;
          finally
            TMonitor.Exit(fSensorsDataSet);
          end;
        end;
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSensorsLayer2.handleNewTime(aClient: TClient; const aTime: string; aPrivateSensorEvent, aPrivateMeteoEvent: TEventEntry);
var
  cursor: TCursor;
  dt: TDateTime;
//  prevIndex: Integer;
begin
  try
    dt := StrToDateTime(aTime, isoDateTimeFormatSettings);
    TMonitor.Enter(fCursors);
    try
      if fCursors.TryGetValue(aClient, cursor) then
      begin
        TMonitor.Enter(fSensorsDataSet.Data);
        try
          cursor.MoveTo(dt);
          // check for change in cursor index
          signalCursorValues(aClient, cursor, aPrivateSensorEvent, aPrivateMeteoEvent);
          Log.WriteLn('cursor @ '+Double(cursor.CurrentTimeStamp).toString+': '+cursor.RangeAsString);
        finally
          TMonitor.Exit(fSensorsDataSet.Data);
        end;
      end;
    finally
      TMonitor.Exit(fCursors);
    end;
    Log.WriteLn('Selected time: '+aTime+' ('+Double(dt).toString+') (live:'+live[aClient].ToString(TUseBoolStrs.True)+')');
  except
    on E: Exception
    do Log.WriteLn('TSensorsLayer2.handleNewTime: '+aTime+': '+e.Message, llError);
  end;
end;

procedure TSensorsLayer2.handleSensorDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TWDID;
  ts: Double;
  value: Double;
  lat: Double;
  lon: Double;
  sensorCode: string;
  sensor: TSensor;
  so: TSimpleObject;
  affectsLive: Boolean;
begin
  sensor := nil;
  so := nil;
  ts := Double.NaN;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fSensorsDataSet);
          try
            if not fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              sensor := TSensor.Create(id);
              fSensorsDataSet.Sensors.Add(id, sensor);
              so := TCircleMarker.Create(Self, sensor.IDAsGUIDStr, 0, 0, 7); //, Double.NaN, [[sojnDraggable, 'true']]);
              (*
              so := TSimpleObject.Create(Self, sensor.IDAsGUIDStr, 'L.circleMarker', // 'L.marker', //
                TWDGeometryPoint.Create(0,0,0), gtPoint,
                [ ['radius', '7']
                  //[sojnContextMenu, 'true'],
                  //[sojnContextmenuItems, '[{"text": "Marker item1","index": 0, "tag":"deze"},{"text": "Marker item2","index": 1, "tag":"ook"}]'], // , {"separator": true, "index": 1}
                  //[sojnContextmenuInheritItems, t],
                  //[sojnInteractive, 'true'],
                  //[sojnDraggable, true]
                ],
                []);
              *)
              AddObject(so, so.jsonNewObject);
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fSensorsDataSet);
          end;
        end;
      wdatLat:
        begin
          lat := aPayload.bb_read_double(aCursor);
          if Assigned(sensor) and (sensor.lat.IsNan) or (sensor.lat<>lat) then
          begin
            sensor.Lat := lat;
            (so.geometry as TWDGeometryPoint).y := lat;
            UpdateObject(so, sojnGeometry, so.jsonGeometryValue);
          end;
        end;
      wdatLon:
        begin
          lon := aPayload.bb_read_double(aCursor);
          if Assigned(sensor)  and (sensor.lon.IsNan) or (sensor.lon<>lon) then
          begin
            sensor.Lon := lon;
            (so.geometry as TWDGeometryPoint).x := lon;
            UpdateObject(so, sojnGeometry, so.jsonGeometryValue);
          end;
        end;
      wdatSensorCode:
        begin
          sensorCode := aPayload.bb_read_string(aCursor);
          if Assigned(sensor) then
          begin
            if sensor.Name<>sensorCode then
            begin
              sensor.Name := sensorCode;
              so.addPropertyString('tooltip', sensor.Name);
              UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            end;
          end;
        end;
      wdatTimeStamp:
        begin
          ts := aPayload.bb_read_double(aCursor);
        end;
      wdatSensordata_Benzene:
        begin
          value := aPayload.bb_read_double(aCursor);
          if Assigned(sensor) and not ts.IsNan then
          begin
            TMonitor.Enter(fSensorsDataSet.Data);
            try
              affectsLive := fSensorsDataSet.NewValues(sensor, ts, [fieldInfo shr 3], [value]);
            finally
              TMonitor.Exit(fSensorsDataSet.Data);
            end;
            if affectsLive then
            begin
              so.addOptionGeoColor(fPalette.ValueToColors(value));
              so.addPropertyString('tooltip', sensor.Name+'<br>'+
                                              'Benzene: '+value.ToString+' µg/m³'+'<br>'+
                                              'live..');
              UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            end
            else UpdateClientsOn(ts);
            triggerUpdateTimesliderData;
          end;
          // else un-timestamped sensor value -> ignore
        end;
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fSensorsDataSet);
          try
            if fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              if objects.TryGetValue(sensor.IDAsGUIDStr, so) then
              begin
                RemoveObject(so);
              end;
              fSensorsDataSet.Sensors.Remove(id);
              // todo: cleanup history and invalidate cursors?
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fSensorsDataSet);
          end;
        end
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSensorsLayer2.handleSourceDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  id: TWDID;
  ts: Double;
  value: Double;
  lat: Double;
  lon: Double;
  sensorCode: string;
  sensor: TSensor;
  so: TSimpleObject;
  affectsLive: Boolean;
begin
  sensor := nil;
  so := nil;
  ts := Double.NaN;
  while aCursor<aLimit do
  begin
    fieldInfo := aPayload.bb_read_uint32(aCursor);
    case fieldInfo of
      (icehObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fSensorsDataSet);
          try
            if not fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              sensor := TSensor.Create(id);
              fSensorsDataSet.Sensors.Add(id, sensor);
              so := TCircleMarker.Create(Self, sensor.IDAsGUIDStr, 0, 0, 3);
              AddObject(so, so.jsonNewObject);
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fSensorsDataSet);
          end;
        end;
      wdatLat:
        begin
          lat := aPayload.bb_read_double(aCursor);
          if Assigned(sensor) and (sensor.lat.IsNan) or (sensor.lat<>lat) then
          begin
            sensor.Lat := lat;
            (so.geometry as TWDGeometryPoint).y := lat;
            UpdateObject(so, sojnGeometry, so.jsonGeometryValue);
          end;
        end;
      wdatLon:
        begin
          lon := aPayload.bb_read_double(aCursor);
          if Assigned(sensor)  and (sensor.lon.IsNan) or (sensor.lon<>lon) then
          begin
            sensor.Lon := lon;
            (so.geometry as TWDGeometryPoint).x := lon;
            UpdateObject(so, sojnGeometry, so.jsonGeometryValue);
          end;
        end;
      wdatSensorCode:
        begin
          sensorCode := aPayload.bb_read_string(aCursor);
          if Assigned(sensor) then
          begin
            if sensor.Name<>sensorCode then
            begin
              sensor.Name := sensorCode;
              so.addPropertyString('tooltip', sensor.Name);
              UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            end;
          end;
        end;
      wdatTimeStamp:
        begin
          ts := aPayload.bb_read_double(aCursor);
        end;
      wdatSourceEmissionStrengthAnalysisBenzene:
        begin
          value := aPayload.bb_read_double(aCursor);
          if Assigned(sensor) and not ts.IsNan then
          begin
            TMonitor.Enter(fSensorsDataSet.Data);
            try
              affectsLive := fSensorsDataSet.NewValues(sensor, ts, [fieldInfo shr 3], [value]);
            finally
              TMonitor.Exit(fSensorsDataSet.Data);
            end;
            if affectsLive then
            begin
              so.addOptionGeoColor(fPalette.ValueToColors(value));
              so.addPropertyString('tooltip', 'Source'+'<br>'+
                                              'Benzene: '+value.ToString+' µg/s'+'<br>'+
                                              'live..');
              UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            end
            else UpdateClientsOn(ts);
          end;
          // else un-timestamped source value -> ignore
        end;
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fSensorsDataSet);
          try
            if fSensorsDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              if objects.TryGetValue(sensor.IDAsGUIDStr, so) then
              begin
                RemoveObject(so);
              end;
              fSensorsDataSet.Sensors.Remove(id);
              // todo: cleanup history and invalidate cursors?
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fSensorsDataSet);
          end;
        end
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSensorsLayer2.handleUpdateLayerObject(aClient: TClient; aPayload: TJSONObject);
begin
  Log.WriteLn('handleUpdateLayerObject: '+aPayload.ToJSON);
end;

function TSensorsLayer2.IsReceivingLayerUpdates(aClient: TClient): Boolean;
begin
  Result := Live[aClient];
end;

function TSensorsLayer2.jsonTimesliderData: string;
var
  entry: string;
  loopSensorValue: Double;
  fillColorPrev: string;
  fillColor: string;
  startTime: string;
  endTime: string;
  cursor: TCursor;
  srp: TPair<TSensor, Integer>;
  sensorValue: Double;
  sr: TSensorsRecord;
  prevMax: Double;
  fc: TAlphaRGBPixel;
  fct: Boolean;
  fctp: Boolean;
  et: TDateTime;
  st: TDateTime;
  fixedEndTime: string;
begin
  // todo: use cursor, if a sensor has no value on a specific time it is not accounted for and a higher value
  // could be shown then calculated for the time stamp
  Result := '';
  //srPRev := nil;
  startTime := '';
  st := 0;
  endTime := '';
  fillColorPrev := '';
  fctp := True;
  fillColor := '';
  prevMax := 0;
  cursor := fSensorsDataSet.NewCursor;
  try
    TMonitor.Enter(fSensorsDataSet);
    try
      if cursor.First then
      begin
        repeat
          loopSensorValue := Double.NaN;
          for srp in cursor.SensorRecords do
          begin
            sr := fSensorsDataSet.Data[srp.Value];
            // todo: check if sr has always entry for sensor (iesrp.key)?
            if (cursor.CurrentTimeStamp-sr.timeStamp<=MaxNoSensorValueTime) and
               sr.values[srp.Key].TryGetValue(wdatSensordata_Benzene shr 3, sensorValue) then
            begin
              // check if value is not too old and if higher then
              if loopSensorValue.IsNaN or (loopSensorValue<sensorValue) then
              begin
                loopSensorValue := sensorValue;
              end;
            end;
          end;
          fc := fPalette.ValueToColors(loopSensorValue).fillColor;
          fct := (fc and $FF000000)=0;
          fillColor := ColorToJSON(fc);
          if fillColor<>fillColorPrev then
          begin
            // store current time on cursor as endTime
            et := cursor.CurrentTimeStamp;
            endTime := FormatDateTime(publisherDateTimeFormat, et);
            // check if we have a valid range (first change in color only initializes the start of the event)
            // and check if not transparant
            if (startTime<>'') and not fctp then
            begin
              // add new entry for pervious color: fillColorPrev startTime-endTime
              if (et-st) <= MaxNoSensorValueTime
              then fixedEndTime := endTime
              else fixedEndTime := FormatDateTime(publisherDateTimeFormat, st+MaxNoSensorValueTime);
              entry :=
                '"start":"'+startTime+'"'+','+
                '"end":"'+fixedEndTime+'"'+','+
                '"color":"'+fillColorPrev+'"'+','+
                '"tooltip":'+'"max value: '+prevmax.toString+'"'; // localized double
              jsonAdd(Result, '{'+entry+'}');
            end;
            // start new range
            fillColorPrev := fillColor;
            fctp := fct;
            startTime := endTime;
            st := et;
            prevMax := loopSensorValue;
          end
          else
          begin
            if CompareLessOrIsNaN(prevMax, loopSensorValue)
            then prevMax := loopSensorValue;
          end;
        until not cursor.Next;
        // add last step
        endTime := FormatDateTime(publisherDateTimeFormat, cursor.CurrentTimeStamp+MaxNoSensorValueTime);
        if (startTime<>'') and not fctp then
        begin
          entry :=
            '"start":"'+startTime+'"'+','+
            '"end":"'+endTime+'"'+','+
            '"color":"'+fillColorPrev+'"'+','+
            '"tooltip":'+'"max value: '+prevmax.toString+'"'; // localized double
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

procedure TSensorsLayer2.setLive(aClient: TClient; const aValue: Boolean);
var
  cursor: TCursor;
begin
  TMonitor.Enter(fCursors);
  try
    if aValue then
    begin
      // switch to live mode -> send latest sensor values
      if fCursors.TryGetValue(aClient, cursor) then
      begin
        TMonitor.Enter(fSensorsDataSet.Data);
        try
          // signal last values via cursor
          cursor.Last;
          signalCursorValues(aClient, cursor, nil, nil);
        finally
          TMonitor.Exit(fSensorsDataSet.Data);
        end;
        // remove cursor so we are live again
        fCursors.Remove(aClient);
      end;
      Log.WriteLn(aClient.clientID+': set live (live:'+Live[aClient].toString(TUseBoolStrs.True)+')');
    end
    else
    begin
      // only create and add cursor if not already in fCursors
      if not fCursors.ContainsKey(aClient)
      then fCursors.Add(aClient, fSensorsDataSet.NewCursor);
      // switch to history mode -> no action
      Log.WriteLn(aClient.clientID+': set NOT live (live:'+Live[aClient].toString(TUseBoolStrs.True)+')');
    end;
  finally
    TMonitor.Exit(fCursors);
  end;
end;

procedure TSensorsLayer2.signalCursorValues(aClient: TClient; aCursor: TCursor;
  aPrivateModelSensorEvent, aPrivateModelMeteoEvent: TEventEntry);
var
  srp: TPair<TSensor, Integer>;
  sensorValue: Double;
  _jsonSensor: string;
  jsonEntry: string;
  jsonColor: string;
  modelPayload: TByteBuffer;
  sr: TSensorsRecord;
  sv: TSensorValues;
  windDirection: Double;
  windSpeed: Double;
  payload: TByteBuffer;
begin
  _jsonSensor := '';
  modelPayload := '';
  windDirection := Double.NaN;
  windSpeed := Double.NaN;
  for srp in aCursor.SensorRecords do
  begin
    sr := fSensorsDataSet.Data[srp.Value];
    if sr.values.TryGetValue(srp.Key, sv) then
    begin
      if not (srp.Key.Lat.IsNan or srp.Key.Lon.IsNan) then
      begin
        if sv.TryGetValue(wdatSensordata_Benzene shr 3, sensorValue) then
        begin
          if (aCursor.CurrentTimeStamp-sr.TimeStamp)<=MaxNoSensorValueTime
          then jsonColor := '"'+ColorToJSON(fPalette.ValueToColors(sensorValue).fillColor)+'"'
          else jsonColor := '"'+ColorToJSON(fPalette.ValueToColors(-1).fillColor)+'"';
          jsonEntry :=
            '{"updateobject":'+
              '{"id":"'+srp.Key.IDAsGUIDStr+'","'+sojnOptions+'":'+
                '{'+
                  '"color":'+jsonColor+','+
                  '"fillColor":'+jsonColor+
                '}'+','+
                '"tooltip":"'+ srp.Key.Name+'<br>'+
                               'Benzene: '+sensorValue.ToString+' µg/m³'+'<br>'+
                               '@ '+FormatDateTime(isoDateTimeFormat, sr.TimeStamp)+'"'+
              '}'+
            '}';
          jsonAdd(_jsonSensor, jsonEntry);
          // todo: for testing sending values to Richard's model
          if Assigned(aPrivateModelSensorEvent) then
          begin
            modelPayload := modelPayload+
              TByteBuffer.bb_tag_guid(icehObjectID, srp.Key.IDAsGUID)+
              TByteBuffer.bb_tag_double(wdatLat shr 3, srp.Key.Lat)+
              TByteBuffer.bb_tag_double(wdatLon shr 3, srp.Key.Lon)+
              TByteBuffer.bb_tag_double(wdatTimeStamp shr 3, aCursor.StartOfRange)+
              TByteBuffer.bb_tag_double(wdatSensordata_Benzene shr 3, sensorValue);
          end;
        end;
        if sv.TryGetValue(wdatSourceEmissionStrengthAnalysisBenzene shr 3, sensorValue) then
        begin
          if (aCursor.CurrentTimeStamp-sr.TimeStamp)<=MaxNoSensorValueTime
          then jsonColor := '"'+ColorToJSON(fPalette.ValueToColors(sensorValue).fillColor)+'"'
          else jsonColor := '"'+ColorToJSON(fPalette.ValueToColors(-1).fillColor)+'"';
          jsonEntry :=
            '{"updateobject":'+
              '{"id":"'+srp.Key.IDAsGUIDStr+'","'+sojnOptions+'":'+
                '{'+
                  '"color":'+jsonColor+','+
                  '"fillColor":'+jsonColor+
                '}'+','+
                '"tooltip":"'+ srp.Key.Name+'<br>'+
                               'Benzene: '+sensorValue.ToString+' µg/s'+'<br>'+
                               '@ '+FormatDateTime(isoDateTimeFormat, sr.TimeStamp)+'"'+
              '}'+
            '}';
          jsonAdd(_jsonSensor, jsonEntry);
        end;
      end
      else
      begin
        // check for meteo 'sensor' -> _jsonMeteo
        sv.TryGetValue(tag_meteodata_winddirection, windDirection);
        sv.TryGetValue(tag_meteodata_windspeed, windSpeed);
      end;
    end;
  end;
  if _jsonSensor<>'' then
  begin
    _jsonSensor := '{"type":"updatelayer","payload":{"id":"'+ElementID+'","data":['+_jsonSensor+']}}';
    aClient.signalString(_jsonSensor);
    // todo: for testing sending values to Richard's model
    if Assigned(aPrivateModelSensorEvent) and (modelPayload<>'')
    then aPrivateModelSensorEvent.signalEvent(modelPayload);
  end;
  if not (windDirection.IsNan or windSpeed.IsNan) then
  begin
    scenario.project.windControl.updateClient(windDirection, windSpeed, 0, aClient);
    if Assigned(aPrivateModelMeteoEvent) then
    begin
      payload :=
        TByteBuffer.bb_tag_guid(icehObjectID, TGUID.Empty)+
        TByteBuffer.bb_tag_double(wdatTimeStamp shr 3, aCursor.StartOfRange)+
        TByteBuffer.bb_tag_double(tag_meteodata_winddirection, windDirection)+
        TByteBuffer.bb_tag_double(tag_meteodata_windspeed, windSpeed);
      aPrivateModelMeteoEvent.signalEvent(payload);
    end;
  end;
end;

procedure TSensorsLayer2.triggerUpdateTimesliderData;
begin
  fTimeSliderDataTimer.Arm(DateTimeDelta2HRT(2*dtOneSecond),
    procedure (aTimer: TTimer; aTime: THighResTicks)
    var
      jsonTSData: string;
      client: TClient;
    begin
      jsonTSData := jsonTimesliderData;
      for client in clients do
      begin
        client.signalString('{"type":"timesliderEvents","payload":{"setEvents":['+jsonTSData+']}}');
      end;
    end);
end;

procedure TSensorsLayer2.UpdateClientsOn(aTimeStamp: TDateTime);
begin
  fClientsToUpdateTimer.Arm(DateTimeDelta2HRT(1*dtOneSecond),
    procedure (aTimer: TTimer; aTime: THighResTicks)
    var
      client: TClient;
      cursor: TCursor;
    begin
      TMonitor.Enter(fCursors);
      try
        for client in clients do
        begin
          if fCursors.TryGetValue(client, cursor) then
          begin
            if not cursor.IsValid then
            begin
              if cursor.MoveTo(cursor.CurrentTimeStamp)
              then signalCursorValues(client, cursor, nil, nil);
            end;
          end;
        end;
      finally
        TMonitor.Exit(fCursors);
      end;
    end);
end;

{ TProjectEarlyWarning }

constructor TProjectEarlyWarning.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName,
  aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection; aAddBasicLayers: Boolean;
  aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView; aProjectCurrentScenario, aProjectRefScenario: TScenario);
var
  scenario: TScenario;
  jsonLegend: string;
  setTimeTimer: TTimer;
  layer: TSensorsLayer2;
  palette: TWDPalette;
begin
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection,
    aAddBasicLayers, aMaxNearestObjectDistanceInMeters, aMapView, aProjectCurrentScenario, aProjectRefScenario);
  fPrivateModelMeteoEvent := nil;
  fPrivateModelSensorEvent := nil;

  scenario := TScenario.Create(Self, 'EarlyWarning', 'EarlyWarning', 'EarlyWarning', False, aMapView, False);
  scenarios.Add(scenario.ID, scenario);
  projectCurrentScenario := scenario;

  fWindSpeed := 0;
  fWindDirection := 0;
  windControl; // enable by using

  SetControl('timeslider', '1');

  palette := CreateEWPalette('Benzene µg/m³');
  jsonLegend := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical);

  layer := TSensorsLayer2.Create(
    palette,
    aConnection.eventEntry('sensordata', True),
    aConnection.eventEntry(connection.privateEventName+'.sensordata', False),
    aConnection.eventEntry('sources', True),
    aConnection.eventEntry(connection.privateEventName+'.sources', False),
    aConnection.eventEntry('meteodata', True),
    aConnection.eventEntry(connection.privateEventName+'.meteodata', False),
    scenario,
    'Benzene', 'Sensors', 'Sensors', 'Sensors for benzene', True, '', True, False, 0.8, jsonLegend);
  scenario.AddLayer(layer);

  layer.previewBase64 := PNGFileToBase64(ExtractFilePath(ParamStr(0))+'previews\EWSensorLayer.png');

  clientMessageHandlers.Add('windData',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    var
      windChanged: Boolean;
      cursor: TCursor;
      dt: TDateTime;
    begin
      windChanged := False;
      if aPayload.TryGetValue<Double>('speed', fWindSpeed)
      then windChanged := True;
      if aPayload.TryGetValue<Double>('direction', fWindDirection)
      then windChanged := True;
      if windChanged then
      begin
        Log.WriteLn('Manual wind (s/d): '+fWindSpeed.ToString+'/'+fWindDirection.toString);

        if Assigned(fPrivateModelMeteoEvent) then
        begin

          TMonitor.Enter(layer.fCursors);
          try
            if layer.fCursors.TryGetValue(aClient, cursor) and cursor.IsValid
            then dt := cursor.StartOfRange
            else dt := Now; // todo: time utc!?
          finally
            TMonitor.Exit(layer.fCursors);
          end;
          fPrivateModelMeteoEvent.signalEvent(
            TByteBuffer.bb_tag_double(wdatTimeStamp shr 3, dt)+
            TByteBuffer.bb_tag_double(tag_meteodata_windspeed , fWindSpeed)+
            TByteBuffer.bb_tag_double(tag_meteodata_winddirection, fWindDirection)+
            TByteBuffer.bb_tag_double(tag_meteodata_moninobukhovlength, 1000)+
            TByteBuffer.bb_tag_double(tag_meteodata_mixinglayerheight, 1000)+
            TByteBuffer.bb_tag_bool(tag_meteodata_rainfall, false));
        end;
      end;
    end);

  setTimeTimer := Timers.CreateInactiveTimer;
  setTimeTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*0.5);
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
        // active ? switch to selected time : switch to now and follow new data
        layer.live[aClient] := not active;
      end;
      if aPayload.TryGetValue<string>('selectedTime', selectedTime) then
      begin
        setTimeTimer.Arm(DateTimeDelta2HRT(0.1*dtOneSecond),
          procedure (aTimer: TTimer; aTime: THighResTicks)
          begin
            layer.handleNewTime(aClient, selectedTime, fPrivateModelSensorEvent, fPrivateModelMeteoEvent);
          end);
      end;
      if aPayload.TryGetValue<TJSONValue>('selectedEvent', selectedEvent) then
      begin
        Log.WriteLn('selected event: '+selectedEvent.toJSON, llWarning);
      end;
      if aPayload.TryGetValue<TJSONValue>('brush', brush) then
      begin
        Log.WriteLn('brush: '+brush.toJSON, llWarning);
      end;
    end);

  // work-a-round for connected models
  aConnection.eventEntry('Clients.earlywarning', false).subscribe.OnString.Add(
    procedure(aEventEntry: TEventEntry; const aString: string)
    begin
      if Assigned(fPrivateModelMeteoEvent)
      then fPrivateModelMeteoEvent.unPublish;
      if Assigned(fPrivateModelSensorEvent)
      then fPrivateModelSensorEvent.unPublish;
      fPrivateModelMeteoEvent := aConnection.eventEntry(aString+'.meteodata', false);
      fPrivateModelSensorEvent := aConnection.eventEntry(aString+'.sensordata', false);
    end);
end;

destructor TProjectEarlyWarning.Destroy;
begin
  inherited;
end;

procedure HandleConnectionException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('Exception in IMB connection: '+aException.Message, llError);
end;

procedure HandleConnectionDisconnect(aConnection: TConnection);
var
  res: Integer;
begin
  res := WSAGetLastError;
  Log.WriteLn('IMB connection is now disconnected ('+res.toString+')', llWarning);
end;

var
  connection: TConnection;
  sessionModel: TSessionModel;
  mapView: TMapView;
  project: TProject;

begin
  try
    FileLogger.SetLogDef(AllLogLevels, [llsTime, llsThreadID]);
    Log.Start();
    connection := TSocketConnection.Create(
      'Publishing Early Warning', 0,
      'earlywarning',
      GetSetting(RemoteHostSwitchName, imbDefaultRemoteHost), GetSetting(RemotePortSwitchName, imbDefaultRemoteSocketPort));
    try
      connection.onException := HandleConnectionException;
      connection.onDisconnect := HandleConnectionDisconnect;
      (connection as TSocketConnection).SetSocketKeepAlive(True);
      if connection.Connected then
      begin
        sessionModel := TSessionModel.Create(connection);
        try
          mapView := TMapView.Create(GetSetting(MapViewSwitchName));
          mapView.DumpToLog;

          project := TProjectEarlyWarning.Create(sessionModel, connection, 'EarlyWarning', 'EarlyWarning', '', '', nil, False, 250, mapView, nil, nil);
          sessionModel.Projects.Add(project);

          (*
          project.projectCurrentScenario.AddLayer(TExternalTilesLayer.Create(
            project.projectCurrentScenario, 'Cycling', 'OCM', 'OCM', 'Open Cycle Map',
            'http://a.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png',
            False, '', True, False));
          *)

          WriteLn('Press return to quit..');
          Readln;

        finally
          WriteLn('Shutting down session..');
          sessionModel.Free;
        end;
      end
      else Log.WriteLn('Could not connect imb to '+GetSetting(RemoteHostSwitchName, imbDefaultRemoteHost)+':'+GetSetting(RemotePortSwitchName, imbDefaultRemoteSocketPort).ToString, llError);
    finally
      WriteLn('Shutting down connection..');
      connection.Free;
    end;
    Log.Finish();
  except
    on E: Exception do
      Log.Writeln(E);
  end;
end.
