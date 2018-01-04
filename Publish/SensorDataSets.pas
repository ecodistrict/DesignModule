unit SensorDataSets;

// todo: add islive property to cursor (always latest record)

// todo: cursor now: last data record index per sensor -> last data record index per sensor and tag
//                                                        or cursor per tag?

interface

uses
  WorldDataCode,
  system.generics.collections,
  system.sysutils;


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
  public
    procedure RemoveSensor(aSensor: TSensor);
  end;

  TSensorsData = TObjectList<TSensorsRecord>;

  TCursor = class
  constructor Create(aData: TSensorsData);
  destructor Destroy; override;
  private
    fData: TSensorsData; // ref
    fCurrentIndex: Integer; // current indexes of sensor values
    fCurrentTimeStamp: TDateTime; // helper
    fIsLive: Boolean;
    fSensorRecords: TDictionary<TSensor, Integer>;
    procedure setIsLive(const aValue: Boolean);
  public
    property Data: TSensorsData read fData;
    property SensorRecords: TDictionary<TSensor, Integer> read fSensorRecords;
    property CurrentIndex: Integer read fCurrentIndex;
    property CurrentTimeStamp: TDateTime read fCurrentTimeStamp;
    property IsLive: Boolean read fIsLive write setIsLive;
    function First: Boolean;
    function Next: Boolean;
    function Prev: Boolean;
    function Last: Boolean;
    function MoveTo(aTimeStamp: TDateTime): Boolean;
    function IsValid: Boolean;
    procedure Invalidate;
    function IsInTimeStep(aTimestamp: TDateTime): Boolean;
    function RangeAsString: string;
    function StartOfRange: TDateTime;
  public
    procedure RemoveSensor(aSensor: TSensor);
  end;

  TSensorsDataSet = class
  constructor Create;
  destructor Destroy; override;
  private
    fSensors: TObjectDictionary<TWDID, TSensor>; // owns
    fData: TSensorsData; // owns
    fCursors:  TObjectList<TCursor>; // owns, locked
  public
    property Sensors: TObjectDictionary<TWDID, TSensor> read fSensors;
    property Data: TSensorsData read fData;
    property Cursors: TObjectList<TCursor> read fCursors;
    function NewCursor: TCursor;
    procedure RemoveCursor(aCursor: TCursor);
    procedure Invalidate;
    //function NewValue(aSensor: TSensor; aTimeStamp: TDateTime; aTag: Integer; aValue: Double): Boolean;
    function NewValues(aSensor: TSensor; aTimeStamp: TDateTime; const aTags: TArray<Integer>; const aValues: TArray<Double>): Boolean;
    procedure RemoveSensor(aSensor: TSensor);
  end;


function CompareLessOrIsNaN(aValue1, aValue2: Double): Boolean;


implementation

{ utils }

function CompareLessOrIsNaN(aValue1, aValue2: Double): Boolean;
begin
  if aValue1.IsNaN or aValue2.IsNaN then
  begin
    if aValue1.IsNaN
    then Result := True
    else Result := False;
  end
  else Result := aValue1<aValue2;
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

procedure TSensorsRecord.RemoveSensor(aSensor: TSensor);
begin
  fValues.Remove(aSensor);
end;

{ TCursor }

constructor TCursor.Create(aData: TSensorsData);
begin
  inherited Create;
  fData := aData; // ref
  fSensorRecords := TDictionary<TSensor, Integer>.Create;
  fCurrentIndex := -1;
  fCurrentTimeStamp := Double.NaN;
  fIsLive := False;
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
  fIsLive := False;
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
  // todo: improve performance by only setting flag
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

function TCursor.IsInTimeStep(aTimestamp: TDateTime): Boolean;
begin
  if fCurrentIndex>=0 then
  begin
    if fCurrentIndex+1<fData.Count
    then Result := (fData[fCurrentIndex].timeStamp<=aTimestamp) and (aTimestamp<fData[fCurrentIndex+1].timeStamp)
    else Result := fData[fCurrentIndex].timeStamp<=aTimestamp;
  end
  else Result := False;
end;

function TCursor.IsValid: Boolean;
begin
  Result := fCurrentIndex >= 0;
end;

function TCursor.MoveTo(aTimeStamp: TDateTime): Boolean;
begin
  if not Double(aTimeStamp).IsNaN then
  begin
    fIsLive := False;
    if not IsValid
    then First;
    if IsValid then
    begin
      Result := True;
      if aTimeStamp>fData[fCurrentIndex].timeStamp then
      begin
        // move into future
        // check for next step
        while Result and (fCurrentIndex<fData.Count-1) and (aTimeStamp>=fData[fCurrentIndex+1].timeStamp)
        do Result := Next;
      end
      else if aTimeStamp<fData[fCurrentIndex].timeStamp then
      begin
        // move into past
        while Result and (fCurrentIndex>=0) and (aTimeStamp<fData[fCurrentIndex].timeStamp)
        do Result := Prev;
      end;
      // else we are there
    end
    else Result := False;
  end
  else
  begin
    Invalidate;
    Result := False;
  end;
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
  fIsLive := False;
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

function TCursor.RangeAsString: string;
begin
  if fCurrentIndex>=0 then
  begin
    if fCurrentIndex+1<fData.Count
    then Result := Double(fData[fCurrentIndex].timeStamp).toString+' - '+Double(fData[fCurrentIndex+1].timeStamp).toString
    else Result := Double(fData[fCurrentIndex].timeStamp).toString+' - ...';
  end
  else Result := '##';
end;

procedure TCursor.RemoveSensor(aSensor: TSensor);
begin
  fSensorRecords.Remove(aSensor);
end;

procedure TCursor.setIsLive(const aValue: Boolean);
begin
  if aValue
  then Last;
  fIsLive := aValue;
end;

function TCursor.StartOfRange: TDateTime;
begin
  if fCurrentIndex>=0
  then Result := fData[fCurrentIndex].timeStamp
  else Result := Double.NaN;
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
{
function TSensorsDataSet.NewValue(aSensor: TSensor; aTimeStamp: TDateTime; aTag: Integer; aValue: Double): Boolean;
var
  sensorsRecord: TSensorsRecord;
  i: Integer;
  sensorValues: TSensorValues;
  cursor: TCursor;
begin
  // return if changing newest record (or adding new record)
  Result := False;
  if fData.Count>0 then
  begin
    // start at last record (newest) (assumes mostly data is comming in from old to new
    i := fData.Count-1;
    // check if newer data
    if fData[i].fTimeStamp<aTimeStamp then
    begin
      // new entry that is newer then last
      sensorsRecord := TSensorsRecord.Create(aTimeStamp);
      fData.Add(sensorsRecord);
      Result := True; // newer record
    end
    else
    begin
      // step back in records until record is no longer newer then given timestamp
      while (i>=0) and (fData[i].fTimeStamp>aTimeStamp)
      do i := i-1;
      if i>=0 then
      begin
        if fData[i].fTimeStamp<>aTimeStamp then
        begin
          sensorsRecord := TSensorsRecord.Create(aTimeStamp);
          fData.Insert(i+1, sensorsRecord);
        end
        else
        begin
          sensorsRecord := fData[i];
          Result := i = fData.Count-1; // newest record ?
        end;
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
  TMonitor.Enter(fCursors);
  try
    for cursor in fCursors do
    begin
      // invalidate cursor if record before current was changed
      if (not Double(cursor.fCurrentTimeStamp).IsNan) and (cursor.fCurrentTimeStamp>aTimeStamp)
      then cursor.Invalidate;
    end;
  finally
    TMonitor.Exit(fCursors);
  end;
end;
}
function TSensorsDataSet.NewValues(aSensor: TSensor; aTimeStamp: TDateTime; const aTags: TArray<Integer>; const aValues: TArray<Double>): Boolean;
var
  sensorsRecord: TSensorsRecord;
  i: Integer;
  sensorValues: TSensorValues;
  cursor: TCursor;
begin
  // return if changing newest record (or adding new record)
  Result := False;
  if fData.Count>0 then
  begin
    // start at last record (newest) (assumes mostly data is comming in from old to new
    i := fData.Count-1;
    // check if newer data
    if fData[i].fTimeStamp<aTimeStamp then
    begin
      // new entry that is newer then last
      sensorsRecord := TSensorsRecord.Create(aTimeStamp);
      fData.Add(sensorsRecord);
      Result := True; // newer record
    end
    else
    begin
      // step back in records until record is no longer newer then given timestamp
      while (i>=0) and (fData[i].fTimeStamp>aTimeStamp)
      do i := i-1;
      if i>=0 then
      begin
        if fData[i].fTimeStamp<>aTimeStamp then
        begin
          sensorsRecord := TSensorsRecord.Create(aTimeStamp);
          fData.Insert(i+1, sensorsRecord);
        end
        else
        begin
          sensorsRecord := fData[i];
          Result := i = fData.Count-1; // newest record ?
        end;
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
  for i := 0 to length(aTags)-1
  do sensorValues.AddOrSetValue(aTags[i], aValues[i]);
  // check cursors
  TMonitor.Enter(fCursors);
  try
    for cursor in fCursors do
    begin
      // invalidate cursor if record before current was changed
      if (not Double(cursor.fCurrentTimeStamp).IsNan) and (cursor.fCurrentTimeStamp>aTimeStamp)
      then cursor.Invalidate;
    end;
  finally
    TMonitor.Exit(fCursors);
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

procedure TSensorsDataSet.RemoveSensor(aSensor: TSensor);
var
  cursor: TCursor;
  sensorsRecord: TSensorsRecord;
begin
  for sensorsRecord in fData
  do sensorsRecord.RemoveSensor(aSensor);
  TMonitor.Enter(fCursors);
  try
    for cursor in fCursors
    do cursor.RemoveSensor(aSensor);
  finally
    TMonitor.Exit(fCursors);
  end;
  fSensors.Remove(aSensor.ID);
end;

end.
