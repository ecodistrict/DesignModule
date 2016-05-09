unit TimerPool;

interface

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Math, WinApi.Windows, System.Classes, System.SysUtils;

type
  THighResTicks = Int64;

const
  hrtDisabled = THighResTicks.MaxValue;
  hrtNoRepeat = -1;

  // time units relative in TDateTime
  dtOneWeek = 7;
  dtOneDay = 1;
  dtOneHour = 1/24;
  dtOneMinute = 1/(24*60);
  dtOneSecond = 1/(24*60*60);
  dtOneMilliSecond = dtOneSecond/1000;

  // time units relative in milliseconds
  msOneSecond = 1000;
  msOneMinute = msOneSecond*60;
  msOneHour = msOneMinute*60;
  msOneDay = msOneHour*24;
  msOneWeek = msOneDay*7;

  TimerPoolMaxSleepCyleTime_ms = msOneHour; // 1 hour in milliseocnds

type
  // very lightweight multiple-readers-exclusive-writer lock.
  // ref http://otl.17slon.com/index.htm
  TOmniMREW = record
  procedure Create;
  strict private
    omrewReference: integer; // Reference.Bit0 is 'writing in progress' flag
  public
    procedure BeginRead; inline;
    procedure BeginWrite; inline;
    procedure EndRead; inline;
    procedure EndWrite; inline;

    procedure Acquire; inline;
    procedure Release; inline;
  end;

  TTimer = class; // forward

  TOnTimer = reference to procedure(aTimer: TTimer);

  TTimerPool = class; // forward

  TTimer = class
  constructor Create(aTimerPool: TTimerPool; aOnTimer: TOnTimer; aDueTime: THighResTicks; aRepeatDelta: THighResTicks=hrtNoRepeat);
  private
    fTimerPool: TTimerPool; // ref
    fOnTimer: TOnTimer;
    fDueTime: THighResTicks;
    fRepeatDelta: THighResTicks;
    fLastFired: THighResTicks;
    function getDueTimeDelta: THighResTicks;
    procedure setDueTimeDelta(const aValue: THighResTicks);
    procedure setDueTime(aValue: THighResTicks);
    procedure setRepeatDelta(const aValue: THighResTicks);
    procedure HandleTimerEvent(aNow: THighResTicks);
    function getEnabled: Boolean;
    procedure setEnabled(const aValue: Boolean);
  public
    property TimerPool: TTimerPool read fTimerPool;
    property DueTime: THighResTicks read fDueTime write setDueTime;
    property DueTimeDelta: THighResTicks read getDueTimeDelta write setDueTimeDelta;
    property OnTimer: TOnTimer read fOnTimer;
    property RepeatDelta: THighResTicks read fRepeatDelta write setRepeatDelta;
    property Enabled: Boolean read getEnabled write setEnabled;
    procedure Cancel;
  end;

  TTimers = TObjectList<TTimer>;

  TTimerPool = class(TThread)
  constructor Create;
  destructor Destroy; override;
  private
    fTimers: TTimers;
    fTimersLock: TOmniMREW;
    fRecalculateNextEvent: THandle;
    procedure addTimer(aTimer: TTimer);
    procedure updateDueTime(aTimer: TTimer; aDueTime: THighResTicks);
    procedure updateRepeatDelta(aTimer: TTimer; aRepeatDelta: THighResTicks);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    function SetTimer(aOnTimer: TOnTimer; aDueTime: THighResTicks=hrtDisabled; aRepeatDelta: THighResTicks=hrtNoRepeat): TTimer;
    function SetTimerDelta(aOnTimer: TOnTimer; aDueTimeDelta: THighResTicks; aRepeatDelta: THighResTicks=hrtNoRepeat): TTimer;
    procedure CancelTimer(var aTimer: TTimer);
  end;

function hrtResolution: TDateTime;
function hrtFrequency: THighResTicks;
function hrtNow: THighResTicks;

function HRT2DateTime(aHighResTicks: THighResTicks): TDateTime;
function HRT2MilliSeconds(aHighResTicks: THighResTicks): THighResTicks;
function MilliSeconds2HRT(aMilliseconds: Int64): THighResTicks;
function DateTimeDelta2HRT(aDateTimeDelta: TDateTime): THighResTicks;

implementation

{ high resolution timer }

var
  _hrtResolution: TDateTime=0;
  _hrtFrequency: THighResTicks=0;

function hrtResolution: TDateTime;
begin
  if _hrtResolution=0
  then _hrtResolution := dtOneSecond/hrtFrequency;
  Result := _hrtResolution;
end;

function hrtFrequency: THighResTicks;
begin
  if _hrtFrequency=0
  then QueryPerformanceFrequency(_hrtFrequency);
  Result := _hrtFrequency;
end;

function hrtNow: THighResTicks;
begin
  QueryPerformanceCounter(Result);
end;

function HRT2DateTime(aHighResTicks: THighResTicks): TDateTime;
begin
  Result := aHighResTicks*hrtResolution;
end;

function HRT2MilliSeconds(aHighResTicks: THighResTicks): Int64;
begin
  Result := Round(1000.0*aHighResTicks/hrtFrequency);
end;

function MilliSeconds2HRT(aMilliseconds: Int64): THighResTicks;
begin
  Result := Round((aMilliseconds/1000.0)*hrtFrequency);
end;

function DateTimeDelta2HRT(aDateTimeDelta: TDateTime): THighResTicks;
begin
  Result := Round((aDateTimeDelta/dtOneSecond)*hrtFrequency);
end;

{ TOmniMREW }

// copy of BeginWrite
procedure TOmniMREW.Acquire;
var
  currentReference: integer;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 1, currentReference);
  //Now wait on all readers
  repeat
  until omrewReference = 1;
end;

procedure TOmniMREW.BeginRead;
var
  currentReference: integer;
begin
  //Wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 2, currentReference);
end;

procedure TOmniMREW.BeginWrite;
var
  currentReference: integer;
begin
  //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 1, currentReference);
  //Now wait on all readers
  repeat
  until omrewReference = 1;
end;

procedure TOmniMREW.Create;
begin
  EndWrite;
end;

procedure TOmniMREW.EndRead;
begin
  //Decrease omrewReference
  InterlockedExchangeAdd(omrewReference, -2);
end;

procedure TOmniMREW.EndWrite;
begin
  omrewReference := 0;
end;

// copy of EndWrite
procedure TOmniMREW.Release;
begin
  omrewReference := 0;
end;

{ TTimer }

procedure TTimer.Cancel;
begin
  if Assigned(fTimerPool)
  then fTimerPool.CancelTimer(Self)
  else fDueTime := hrtDisabled;
end;

constructor TTimer.Create(aTimerPool: TTimerPool; aOnTimer: TOnTimer; aDueTime: THighResTicks; aRepeatDelta: THighResTicks);
begin
  inherited Create;
  fTimerPool := aTimerPool;
  fDueTime := aDueTime;
  fOnTimer := aOnTimer;
  fRepeatDelta := aRepeatDelta;
end;

function TTimer.getDueTimeDelta: THighResTicks;
begin
  Result := fDueTime-hrtNow;
end;

function TTimer.getEnabled: Boolean;
begin
  Result := fDueTime<>hrtDisabled;
end;

procedure TTimer.HandleTimerEvent(aNow: THighResTicks);
begin
  if Assigned(OnTimer)
  then OnTimer(Self);
  fLastFired := aNow;
  if fRepeatDelta>0
  then fDueTime := fDueTime+fRepeatDelta // to enable missed events
  else fDueTime := hrtDisabled; // to disable next event
end;

procedure TTimer.setDueTime(aValue: THighResTicks);
begin
  if fDueTime<>aValue then
  begin
    if Assigned(fTimerPool)
    then fTimerPool.updateDueTime(Self, aValue)
    else fDueTime := aValue;
  end;
end;

procedure TTimer.setDueTimeDelta(const aValue: THighResTicks);
begin
  setDueTime(hrtNow+aValue);
end;

procedure TTimer.setEnabled(const aValue: Boolean);
begin
  if aValue<>Enabled then
  begin
    if aValue then
    begin
      if fRepeatDelta>0
      then SetDueTimeDelta(fRepeatDelta) // set at next repeat from now
      else SetDueTimeDelta(0); // is effectively now
    end
    else setDueTime(hrtDisabled);
  end;
end;

procedure TTimer.setRepeatDelta(const aValue: THighResTicks);
begin
  if fRepeatDelta<>aValue then
  begin
    if Assigned(fTimerPool)
    then fTimerPool.updateRepeatDelta(Self, aValue)
    else fRepeatDelta := aValue;
  end;
end;

{ TTimerPool }

procedure TTimerPool.addTimer(aTimer: TTimer);
begin
  fTimersLock.BeginWrite;
  try
    fTimers.Add(aTimer);
    if fTimers.Count>1
    then fTimers.Sort;
  	if fTimers.First=aTimer
  	then SetEvent(fRecalculateNextEvent);
  finally
    fTimersLock.EndWrite;
  end;
end;

procedure TTimerPool.CancelTimer(var aTimer: TTimer);
begin
  fTimersLock.BeginWrite;
  try
    aTimer.fTimerPool := nil;
    // if first event then reset next event time
    if fTimers.Remove(aTimer)=0
    then SetEvent(fRecalculateNextEvent);
    aTimer := nil;
  finally
    fTimersLock.EndWrite;
  end;
end;

constructor TTimerPool.Create;
begin
  fRecalculateNextEvent := CreateEvent(nil, False, False, nil);
  fTimersLock.Create;
  fTimers := TTimers.Create(TComparer<TTimer>.Construct(
    // order timer events on due time
    function(const Left, Right: TTimer): Integer
    begin
      if Left.DueTime < Right.DueTime then
        Result := -1
      else if Left.DueTime > Right.DueTime then
        Result := 1
      else
        Result := 0;
    end));
  inherited Create(False);
end;

destructor TTimerPool.Destroy;
begin
  inherited;
  FreeAndNil(fTimers);
  if fRecalculateNextEvent<>INVALID_HANDLE_VALUE then
  begin
    Closehandle(fRecalculateNextEvent);
    fRecalculateNextEvent := INVALID_HANDLE_VALUE;
  end;
end;

procedure TTimerPool.Execute;
var
  handles: array[0..1] of THandle;
  _now: THighResTicks;
  i: Integer;
  cycleWaitTime_ms: Cardinal;
begin
  handles[0] := Handle; // thread
  handles[1] := fRecalculateNextEvent;
  while not Terminated do
  begin
    cycleWaitTime_ms := TimerPoolMaxSleepCyleTime_ms; // sentinel
    fTimersLock.BeginRead;
    try
      if fTimers.Count>0 then
      begin
        _now := hrtNow;
        i := 0;
        while (i<fTimers.Count) and (fTimers[i].DueTime<=_now) do
        begin
          fTimers[i].HandleTimerEvent(_now);
          i := i+1;
        end;
        if i>0 then
        begin
          // transition reader lock to writer
          fTimersLock.EndRead;
          try
            fTimersLock.BeginWrite;
            try
              fTimers.Sort;
            finally
              fTimersLock.EndWrite;
            end;
          finally
            fTimersLock.BeginRead;
          end;
        end;
        if (fTimers.Count>0) and fTimers.First.Enabled
        then cycleWaitTime_ms := Max(Min(HRT2MilliSeconds(fTimers.First.fDueTime-_now), TimerPoolMaxSleepCyleTime_ms), 0);
      end;
    finally
      fTimersLock.EndRead;
    end;
    WaitForMultipleObjects(2, @Handles[0], False, cycleWaitTime_ms);
  end;
end;

function TTimerPool.SetTimer(aOnTimer: TOnTimer; aDueTime: THighResTicks; aRepeatDelta: THighResTicks): TTimer;
begin
  Result := TTimer.Create(Self, aOnTimer, aDueTime, aRepeatDelta);
  addTimer(Result);
end;

function TTimerPool.SetTimerDelta(aOnTimer: TOnTimer; aDueTimeDelta: THighResTicks; aRepeatDelta: THighResTicks): TTimer;
begin
  Result := SetTimer(aOnTimer, hrtNow+aDueTimeDelta, aRepeatDelta);
end;

procedure TTimerPool.TerminatedSet;
begin
  SetEvent(fRecalculateNextEvent); // shortcut to terminate waiting thread
end;

procedure TTimerPool.updateDueTime(aTimer: TTimer; aDueTime: THighResTicks);
begin
  if Assigned(fTimers) then
  begin
    fTimersLock.BeginWrite;
    try
      aTimer.fDueTime := aDueTime;
      if fTimers.Count>1
      then fTimers.Sort; // todo: expensive
      if fTimers.First=aTimer
      then SetEvent(fRecalculateNextEvent);
    finally
      fTimersLock.EndWrite;
    end;
  end;
end;

procedure TTimerPool.updateRepeatDelta(aTimer: TTimer; aRepeatDelta: THighResTicks);
begin
  fTimersLock.BeginWrite;
  try
    aTimer.fRepeatDelta := aRepeatDelta;
    if not aTimer.Enabled then
    begin
      aTimer.fDueTime := hrtNow+aTimer.fRepeatDelta;
      if fTimers.Count>1
    	then fTimers.Sort;
      if fTimers.First=aTimer
      then SetEvent(fRecalculateNextEvent);
    end;
  finally
    fTimersLock.EndWrite;
  end;
end;

end.
