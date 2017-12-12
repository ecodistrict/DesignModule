program PublishingServerEarlyWarning;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogConsole, LogFile,
  StdIni,
  imb4, imb.SocksLib,
  TimerPool,
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
  TSensor = class
  constructor Create(const aID: TWDID);
  private
    fID: TWDID;
    fName: string;
    fDescription: string;
    fLat: Double;
    fLon: Double;
  public
    property ID: TWDID read fID;
    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
    property Lat: Double read fLat write fLat;
    property Lon: Double read fLon write fLon;
    function IDAsGUID: TGUID;
    function IDAsGUIDStr: string;
  end;

  TSensorValues = TDictionary<Integer, Double>; // values based on tag

  TSensorsRecord = class
  constructor Create(aTimeStamp: TDateTime);
  destructor Destroy; override;
  private
    fTimeStamp: TDateTime;
    fValues: TObjectDictionary<TSensor, TSensorValues>; // owns, values per sensor on this timestamp
  public
    property timeStamp: TDateTime read fTimeStamp;
    property values: TObjectDictionary<TSensor, TSensorValues> read fValues;
  end;

  TSensorsData = TObjectList<TSensorsRecord>;

  TCursor = class
  constructor Create(aData: TSensorsData);
  destructor Destroy; override;
  private
    fData: TSensorsData; // ref
    fCurrentIndex: Integer;
    fCurrentTimeStamp: TDateTime; // helper
    fSensorRecords: TDictionary<TSensor, Integer>; //  current indexes of sensor values
  public
    property SensorRecords: TDictionary<TSensor, Integer> read fSensorRecords;
    function First: Boolean;
    function Next: Boolean;
    function Prev: Boolean;
    function Last: Boolean;
    function MoveTo(aTimeStamp: TDateTime): Boolean;
    function IsValid: Boolean;
    procedure Invalidate;
  end;

  TSensorsDataSet = class
  constructor Create;
  destructor Destroy; override;
  private
    fSensors: TObjectDictionary<TWDID, TSensor>; // owns
    fData: TSensorsData; // owns
    fCursors:  TObjectList<TCursor>; // owns
  public
    property Sensors: TObjectDictionary<TWDID, TSensor> read fSensors;
    property Data: TSensorsData read fData;
    property Cursors: TObjectList<TCursor> read fCursors;
    function NewCursor: TCursor;
    procedure RemoveCursor(aCursor: TCursor);
    procedure Invalidate;
    procedure NewValue(aSensor: TSensor; aTimeStamp: TDateTime; aTag: Integer; aValue: Double);
  end;
  
  TSensorsLayer2 = class(TSimpleLayer)
  constructor Create(
    aPalette: TWDPalette;
    aEventEntrySensor, aPrivateEventEntrySensor: TEventEntry;
    aEventEntrySource, aPrivateEventEntrySource: TEventEntry;
    aScenario: TScenario;
    const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aDisplayGroup: string=''; aShowInDomains: Boolean=True;
    aBasicLayer: Boolean=False; aOpacity: Double=0.8; const aLegendJSON: string='');
  destructor Destroy; override;
  private
    // data
    fPalette: TWDPalette;
    fDataSet: TSensorsDataSet; // owns records, not sensors in records, ordered by timestamp
    // events
    fEventEntrySensor: TEventEntry;
    fPrivateEventEntrySensor: TEventEntry;
    fEventEntrySource: TEventEntry;
    fPrivateEventEntrySource: TEventEntry;
    fTimeSliderDataTimer: TTimer;
    fCursors: TDictionary<TClient, TCursor>; // only refs
    procedure handleSensorDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleSourceDataEvent(aEventEntry: TEventEntry; const aPayload: TByteBuffer; aCursor, aLimit: Integer);
    procedure setLive(aClient: TClient; const aValue: Boolean);
    function getLive(aClient: TClient): Boolean;
    procedure signalCursorValues(aClient: TClient; aCursor: TCursor; aPrivateModelSensorEvent: TEventEntry);
  protected
    function IsReceivingLayerUpdates(aClient: TClient): Boolean; override;
    function jsonTimesliderData: string;
    procedure triggerUpdateTimesliderData;
  protected
    procedure handleNewTime(aClient: TClient; const aTime: string; aPrivateSensorEvent: TEventEntry);
    procedure handleUpdateLayerObject(aPayload: TJSONObject); override;
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

{ TSensor }

constructor TSensor.Create(const aID: TWDID);
begin
  inherited Create;
  fID := aID;
  fName := '';
  fDescription := '';
  fLat := Double.NaN;
  fLon := Double.NaN;
end;

function TSensor.IDAsGUIDStr: string;
begin
  Result := IDAsGUID.ToString;
end;

function TSensor.IDAsGUID: TGUID;
begin
  Result := TGUID.Create(Pointer(PAnsiCHar(fID))^);
end;

{ TSensorsRecord }

constructor TSensorsRecord.Create(aTimeStamp: TDateTime);
begin
  inherited Create;
  fTimeStamp := aTimeStamp;
  fValues := TObjectDictionary<TSensor, TSensorValues>.Create([doOwnsValues]);
end;

destructor TSensorsRecord.Destroy;
begin
  FreeAndNil(fValues);
  inherited;
end;

{ TCursor }

constructor TCursor.Create(aData: TSensorsData);
begin
  inherited Create;
  fData := aData; // ref
  fSensorRecords := TDictionary<TSensor, Integer>.Create;
  fCurrentIndex := -1;
  fCurrentTimeStamp := Double.NaN;
end;

destructor TCursor.Destroy;
begin
  Invalidate;
  FreeAndNil(fSensorRecords);
  fData := nil; // un-ref
  inherited;
end;

function TCursor.First: Boolean;
var
  sensor: TSensor;
begin
  fSensorRecords.Clear;
  if fData.Count>0 then
  begin
    fCurrentIndex := 0;
    // load first known sensors
    for sensor in fData[fCurrentIndex].fValues.Keys.ToArray
    do fSensorRecords.Add(sensor, fCurrentIndex);
    fCurrentTimeStamp := fData[fCurrentIndex].fTimeStamp;
    Result := True;
  end
  else Result := False;
end;

procedure TCursor.Invalidate;
begin
  // called when updating data -> already locked
  fSensorRecords.Clear;
  fCurrentIndex := -1;
end;

function TCursor.Last: Boolean;
begin
  if not IsValid
  then Result := First
  else Result := True;
  if Result
  then repeat until not Next;
end;

function TCursor.IsValid: Boolean;
begin
  Result := fCurrentIndex >= 0;
end;

function TCursor.MoveTo(aTimeStamp: TDateTime): Boolean;
begin
  if not IsValid
  then First;
  if IsValid then
  begin
    Result := True;
    if aTimeStamp>fData[fCurrentIndex].timeStamp then
    begin
      // move to future
      // check for next step
      while Result and (fCurrentIndex<fData.Count-1) and (aTimeStamp>=fData[fCurrentIndex+1].timeStamp)
      do Result := Next;
    end
    else if aTimeStamp<fData[fCurrentIndex].timeStamp then
    begin
      // move to past
      while Result and (fCurrentIndex>=0) and (aTimeStamp<fData[fCurrentIndex].timeStamp)
      do Result := Prev;
    end;
    // else we are there
  end
  else Result := False;
  // store the given timestamp
  fCurrentTimeStamp := aTimeStamp;
end;

function TCursor.Next: Boolean;
var
  sensor: TSensor;
begin
  if IsValid then
  begin
    if fCurrentIndex<fData.Count-1 then
    begin
      fCurrentIndex := fCurrentIndex+1;
      for sensor in fData[fCurrentIndex].fValues.Keys.ToArray
      do fSensorRecords.AddOrSetValue(sensor, fCurrentIndex);
      fCurrentTimeStamp := fData[fCurrentIndex].fTimeStamp;
      Result := True;
    end
    else Result := False;
  end
  else
  begin
    fSensorRecords.Clear;
    if fData.Count>0 then
    begin
      fCurrentIndex := 0;
      // load first known sensors
      for sensor in fData[fCurrentIndex].fValues.Keys.ToArray
      do fSensorRecords.Add(sensor, fCurrentIndex);
      fCurrentTimeStamp := fData[fCurrentIndex].fTimeStamp;
      Result := True;
    end
    else Result := False;
  end;
end;

function TCursor.Prev: Boolean;
var
  sensor: TSensor;
  changedSensors: TDictionary<TSensor, Boolean>;
  localIndex: Integer;
begin
  if IsValid then
  begin
    if fCurrentIndex>0 then
    begin
      changedSensors := TDictionary<TSensor, Boolean>.Create;
      try
        // store list of changed sensors
        for sensor in fData[fCurrentIndex].fValues.Keys.ToArray
        do changedSensors.Add(sensor, True);
        // step back current index
        fCurrentIndex := fCurrentIndex-1;
        fCurrentTimeStamp := fData[fCurrentIndex].fTimeStamp;
        // walk back on index until all changed sensors have new values
        localIndex := fCurrentIndex;
        while (localIndex>=0) and (changedSensors.Count>0) do
        begin
          for sensor in fData[localIndex].fValues.Keys.ToArray do
          begin
            if changedSensors.ContainsKey(sensor) then
            begin
              fSensorRecords.AddOrSetValue(sensor, localIndex);
              changedSensors.Remove(sensor);
            end;
          end;
          localIndex := localIndex-1;
        end;
      finally
        changedSensors.Free;
      end;
      Result := True;
    end
    else Result := False;
  end
  else
  begin
    fSensorRecords.Clear;
    if fData.Count>0 then
    begin
      fCurrentIndex := 0;
      // load first known sensors
      for sensor in fData[fCurrentIndex].fValues.Keys.ToArray
      do fSensorRecords.Add(sensor, fCurrentIndex);
      fCurrentTimeStamp := fData[fCurrentIndex].fTimeStamp;
      Result := True;
    end
    else Result := False;
  end;
end;

{ TSensorsDataSet }

constructor TSensorsDataSet.Create;
begin
  inherited Create;
  fSensors := TObjectDictionary<TWDID, TSensor>.Create([doOwnsValues]);
  fData := TSensorsData.Create(True);
  fCursors :=  TObjectList<TCursor>.Create;
end;

destructor TSensorsDataSet.Destroy;
begin
  FreeAndNil(fCursors);
  FreeAndNil(fData);
  FreeAndNil(fSensors);
  inherited;
end;

procedure TSensorsDataSet.Invalidate;
var
  cursor: TCursor;
begin
  TMonitor.Enter(fCursors);
  try
    for cursor in fCursors
    do cursor.Invalidate;
  finally
    TMonitor.Exit(fCursors);
  end;
end;

function TSensorsDataSet.NewCursor: TCursor;
begin
  TMonitor.Enter(fCursors);
  try
    Result := TCursor.Create(fData);
    fCursors.Add(Result);
  finally
    TMonitor.Exit(fCursors);
  end;
end;

procedure TSensorsDataSet.NewValue(aSensor: TSensor; aTimeStamp: TDateTime; aTag: Integer; aValue: Double);
var
  sensorsRecord: TSensorsRecord;
  i: Integer;
  sensorValues: TSensorValues;
  cursor: TCursor;
begin
  if fData.Count>0 then
  begin
    // find or add record starting at end (asume values arrive sorted in time)
    i := fData.Count-1;
    if fData[i].fTimeStamp<aTimeStamp then
    begin
      // new entry that is newer then last
      sensorsRecord := TSensorsRecord.Create(aTimeStamp);
      fData.Add(sensorsRecord);
    end
    else
    begin
      while (i>=0) and (fData[i].fTimeStamp>aTimeStamp)
      do i := i-1;
      if i>=0 then
      begin
        if fData[i].fTimeStamp<>aTimeStamp then
        begin
          sensorsRecord := TSensorsRecord.Create(aTimeStamp);
          fData.Insert(i, sensorsRecord);
        end
        else sensorsRecord := fData[i];
      end
      else
      begin
        sensorsRecord := TSensorsRecord.Create(aTimeStamp);
        fData.Insert(0, sensorsRecord);
      end;
    end;
  end
  else
  begin
    // empty so add entry
    sensorsRecord := TSensorsRecord.Create(aTimeStamp);
    fData.Add(sensorsRecord);
  end;
  // correct sensors record is found or added
  if not sensorsRecord.values.TryGetValue(aSensor, sensorValues) then
  begin
    sensorValues := TSensorValues.Create();
    sensorsRecord.values.Add(aSensor, sensorValues);
  end;
  sensorValues.AddOrSetValue(aTag, aValue);
  // check cursors
  for cursor in fCursors do
  begin
    // invalidate cursor if record before current was changed
    if (not Double(cursor.fCurrentTimeStamp).IsNan) and (cursor.fCurrentTimeStamp>aTimeStamp)
    then cursor.Invalidate;
  end;
end;

procedure TSensorsDataSet.RemoveCursor(aCursor: TCursor);
begin
  TMonitor.Enter(fCursors);
  try
    if fCursors.Contains(aCursor)
    then fCursors.Remove(aCursor)
    else aCursor.Free;
  finally
    TMonitor.Exit(fCursors);
  end;
end;

{ TSensorsLayer2 }

constructor TSensorsLayer2.Create(
  aPalette: TWDPalette;
  aEventEntrySensor, aPrivateEventEntrySensor: TEventEntry;
  aEventEntrySource, aPrivateEventEntrySource: TEventEntry;
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
  fDataSet := TSensorsDataSet.Create;
  fCursors := TDictionary<TClient, TCursor>.Create;
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
end;

destructor TSensorsLayer2.Destroy;
begin
  CancelTimer(fTimeSliderDataTimer);
  FreeAndNil(fDataSet);
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
      lastDataRecord := cursor.fData.Last;
      Result := (not Assigned(lastDataRecord)) or Double(cursor.fCurrentTimeStamp).IsNaN or (cursor.fCurrentTimeStamp>lastDataRecord.fTimeStamp);
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

procedure TSensorsLayer2.handleNewTime(aClient: TClient; const aTime: string; aPrivateSensorEvent: TEventEntry);
var
  cursor: TCursor;
  dt: TDateTime;
begin
  try
    dt := StrToDateTime(aTime, isoDateTimeFormatSettings);
    TMonitor.Enter(fCursors);
    try
      if fCursors.TryGetValue(aClient, cursor) then
      begin
        TMonitor.Enter(fDataSet.Data);
        try
          cursor.MoveTo(dt);
          signalCursorValues(aClient, cursor, aPrivateSensorEvent);
        finally
          TMonitor.Exit(fDataSet.Data);
        end;
      end;
    finally
      TMonitor.Exit(fCursors);
    end;
    Log.WriteLn('Selected time: '+aTime+' (live:'+live[aClient].ToString(TUseBoolStrs.True)+')');
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
          TMonitor.Enter(fDataSet);
          try
            if not fDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              sensor := TSensor.Create(id);
              fDataSet.Sensors.Add(id, sensor);
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
            TMonitor.Exit(fDataSet);
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
              //UpdateObject(so, sojnPopup, '{"content":"'+sensor.Name+'","options":{}}');
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
            TMonitor.Enter(fDataSet.Data);
            try
              fDataSet.NewValue(sensor, ts, fieldInfo shr 3, value);
            finally
              TMonitor.Exit(fDataSet.Data);
            end;
            so.addOptionGeoColor(fPalette.ValueToColors(value));
            so.addPropertyString('tooltip', sensor.Name+'<br>'+
                                            'Benzene: '+value.ToString+' µg/m³'+'<br>'+
                                            'live..');
            UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            triggerUpdateTimesliderData;
          end;
          // else un-timestamped sensor value -> ignore
        end;
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fDataSet);
          try
            if fDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              if objects.TryGetValue(sensor.IDAsGUIDStr, so) then
              begin
                RemoveObject(so);
              end;
              fDataSet.Sensors.Remove(id);
              // todo: cleanup history and invalidate cursors?
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fDataSet);
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
          TMonitor.Enter(fDataSet);
          try
            if not fDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              sensor := TSensor.Create(id);
              fDataSet.Sensors.Add(id, sensor);
              so := TCircleMarker.Create(Self, sensor.IDAsGUIDStr, 0, 0, 3);
              AddObject(so, so.jsonNewObject);
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fDataSet);
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
            TMonitor.Enter(fDataSet.Data);
            try
              fDataSet.NewValue(sensor, ts, fieldInfo shr 3, value);
            finally
              TMonitor.Exit(fDataSet.Data);
            end;
            so.addOptionGeoColor(fPalette.ValueToColors(value));
            so.addPropertyString('tooltip', 'Source'+'<br>'+
                                            'Benzene: '+value.ToString+' µg/s'+'<br>'+
                                            'live..');
            UpdateObject(so, sojnOptions, so.jsonOptionsValue);
            triggerUpdateTimesliderData;
          end;
          // else un-timestamped source value -> ignore
        end;
      (icehNoObjectID shl 3) or wtLengthDelimited:
        begin
          id := aPayload.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fDataSet);
          try
            if fDataSet.Sensors.TryGetValue(id, sensor) then
            begin
              if objects.TryGetValue(sensor.IDAsGUIDStr, so) then
              begin
                RemoveObject(so);
              end;
              fDataSet.Sensors.Remove(id);
              // todo: cleanup history and invalidate cursors?
            end
            else so := objects[sensor.IDAsGUIDStr];
          finally
            TMonitor.Exit(fDataSet);
          end;
        end
    else
      aPayload.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TSensorsLayer2.handleUpdateLayerObject(aPayload: TJSONObject);
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
  benzene: Double;
  fillColorPrev: string;
  fillColor: string;
  startTime: string;
  endTime: string;
  cursor: TCursor;
  srp: TPair<TSensor, Integer>;
  sensorValue: Double;
  sr: TSensorsRecord;
  prevMax: Double;
begin
  // todo: use cursor, if a sensor has no value on a specific time it is not accounted for and a higher value
  // could be shown then calculated for the time stamp
  Result := '';
  //srPRev := nil;
  startTime := '';
  endTime := '';
  fillColorPrev := '';
  fillColor := '';
  prevMax := 0;
  cursor := fDataSet.NewCursor;
  try
    TMonitor.Enter(fDataSet);
    try
      if cursor.First then
      begin
        repeat
          benzene := 0;
          for srp in cursor.SensorRecords do
          begin
            sr := fDataSet.fData[srp.Value];
            // todo: check if sr has always entry for sensor (iesrp.key)?
            if (cursor.fCurrentTimeStamp-sr.timeStamp<=MaxNoSensorValueTime) and sr.values[srp.Key].TryGetValue(wdatSensordata_Benzene shr 3, sensorValue) then
            begin
              // check if value is not too old and if higher then
              if benzene<sensorValue
              then benzene := sensorValue;
            end;
          end;
          fillColor := ColorToJSON(fPalette.ValueToColors(benzene).fillColor);
          if fillColor<>fillColorPrev then
          begin
            // store current time on cursor as endTime
            endTime := FormatDateTime(publisherDateTimeFormat, cursor.fCurrentTimeStamp);
            // check if we have a valid range (first change in color only initializes the start of the event)
            if startTime<>'' then
            begin
              // add new entry for pervious color: fillColorPrev startTime-endTime
              entry :=
                '"start":"'+startTime+'"'+','+
                '"end":"'+endTime+'"'+','+
                '"color":"'+fillColorPrev+'"'+','+
                '"tooltip":'+'"max value: '+prevmax.toString+'"'; // localized double
              jsonAdd(Result, '{'+entry+'}');
            end;
            // start new range
            fillColorPrev := fillColor;
            startTime := endTime;
            prevMax := benzene;
          end
          else
          begin
            if prevMax<benzene
            then prevMax := benzene;
          end;
        until not cursor.Next;
        // add last step
        endTime := FormatDateTime(publisherDateTimeFormat, cursor.fCurrentTimeStamp+1/24); // 1 hour
        if (startTime<>'') and (startTime<>endTime) then
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
      TMonitor.Exit(fDataSet);
    end;
  finally
    fDataSet.RemoveCursor(cursor);
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
        TMonitor.Enter(fDataSet.Data);
        try
          // signal last values via cursor
          cursor.Last;
          signalCursorValues(aClient, cursor, nil);
        finally
          TMonitor.Exit(fDataSet.Data);
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
      then fCursors.Add(aClient, fDataSet.NewCursor);
      // switch to history mode -> no action
      Log.WriteLn(aClient.clientID+': set NOT live (live:'+Live[aClient].toString(TUseBoolStrs.True)+')');
    end;
  finally
    TMonitor.Exit(fCursors);
  end;
end;

procedure TSensorsLayer2.signalCursorValues(aClient: TClient; aCursor: TCursor; aPrivateModelSensorEvent: TEventEntry);
var
  srp: TPair<TSensor, Integer>;
  sensorValue: Double;
  _json: string;
  jsonEntry: string;
  jsonColor: string;
  modelPayload: TByteBuffer;
  sr: TSensorsRecord;
begin
  _json := '';
  modelPayload := '';
  for srp in aCursor.SensorRecords do
  begin
    sr := fDataSet.fData[srp.Value];
    if sr.values[srp.Key].TryGetValue(wdatSensordata_Benzene shr 3, sensorValue) then
    begin
      if (aCursor.fCurrentTimeStamp-sr.fTimeStamp)<=MaxNoSensorValueTime
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
                           '@ '+FormatDateTime(isoDateTimeFormat, sr.fTimeStamp)+'"'+
          '}'+
        '}';
      jsonAdd(_json, jsonEntry);
      if Assigned(aPrivateModelSensorEvent) then
      begin
        modelPayload := modelPayload+
          TByteBuffer.bb_tag_guid(icehObjectID, srp.Key.IDAsGUID)+
          TByteBuffer.bb_tag_double(wdatLat shr 3, srp.Key.Lat)+
          TByteBuffer.bb_tag_double(wdatLon shr 3, srp.Key.Lon)+
          TByteBuffer.bb_tag_double(wdatTimeStamp shr 3, aCursor.fCurrentTimeStamp)+
          TByteBuffer.bb_tag_double(wdatSensordata_Benzene shr 3, sensorValue);
      end;
    end;
  end;
  _json := '{"type":"updatelayer","payload":{"id":"'+ElementID+'","data":['+_json+']}}';
  aClient.signalString(_json);
  if Assigned(aPrivateModelSensorEvent) and (modelPayload<>'')
  then aPrivateModelSensorEvent.signalEvent(modelPayload);
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
    scenario,
    'Benzene', 'Sensors', 'Sensors', 'Sensors for benzene', True, '', True, False, 0.8, jsonLegend);
  scenario.AddLayer(layer);

  layer.previewBase64 := PNGFileToBase64(ExtractFilePath(ParamStr(0))+'previews\EWSensorLayer.png');

  clientMessageHandlers.Add('windData',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    var
      windChanged: Boolean;
    begin
      windChanged := False;
      if aPayload.TryGetValue<Double>('speed', fWindSpeed)
      then windChanged := True;
      if aPayload.TryGetValue<Double>('direction', fWindDirection)
      then windChanged := True;
      if windChanged then
      begin
        if Assigned(fPrivateModelMeteoEvent) then
        begin
          fPrivateModelMeteoEvent.signalEvent(
            TByteBuffer.bb_tag_double(wdatTimeStamp shr 3, now)+ // todo: time utc!
            TByteBuffer.bb_tag_double(tag_meteodata_windspeed , fWindSpeed)+
            TByteBuffer.bb_tag_double(tag_meteodata_winddirection, fWindDirection)+
            TByteBuffer.bb_tag_double(tag_meteodata_moninobukhovlength, 0.2)+
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
            layer.handleNewTime(aClient, selectedTime, fPrivateModelSensorEvent);
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
