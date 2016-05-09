unit Timer;

// time always in UTC, system time in file time format

{ from Delphi help (msdn) SetWaitableTimer

  pDueTime [in]
           Time after which the state of the timer is to be set to signaled, in 100 nanosecond intervals.
           Use the format described by the FILETIME structure.
           Positive values indicate absolute time.
           Be sure to use a UTC-based absolute time, as the system uses UTC-based time internally.
           Negative values indicate relative time.
           The actual timer accuracy depends on the capability of your hardware.
           For more information about UTC-based time, see System Time.
  lPeriod  [in]
           Period of the timer, in milliseconds.
           If lPeriod is zero, the timer is signaled once.
           If lPeriod is greater than zero, the timer is periodic.
           A periodic timer automatically reactivates each time the period elapses, until the timer
           is canceled using the CancelWaitableTimer function or reset using SetWaitableTimer.
           If lPeriod is less than zero, the function fails.
}

interface

uses
  Int64Time, DTTime,
  System.SysUtils, System.Classes, WinApi.Windows;

const
  InvalidHighResTick                   = -1;

  itOneMilliSecondPeriod               = 1;
  itOneSecondPeriod                    = 1000*itOneMilliSecondPeriod;

  trcInfinite                          = MaxInt; // send infinite timer events

function  HighResTickResolution        : TDateTime;
function  HighResTickFrequency         : Int64;
function  HighResTick                  : Int64;
function  HighResTickDurationStr       ( aHighResStartTick: Int64; aHighResEndTick: Int64=InvalidHighResTick): string;

type
  TIMBTimer = class; // forward

  TOnTimerTick                         = reference to procedure(aTimer: TIMBTimer; aTick: Integer);

  TIMBTimer = class(TThread)
  constructor Create;
  destructor  Destroy; override;
  private
    FTimerHandle                       : THandle;
    FOnTimerTick                       : TOnTimerTick;
    // positive values are UTC, negative are relative to "now"
    FDueTimeUTCorRel                   : Int64;
    FPeriodms                          : Integer;
    FMaxTickCount                      : Integer;
    FTick                              : Integer;
    FGoToSuspend                       : Boolean;
    // last time of tick or start of timer from HighResTick to calculate next tick on speed change
    FStartHighResTick                  : Int64;
    FLastHighResTick                   : Int64;
    FStopHighResTick                   : Int64;
    function  GetActive                : Boolean;
    procedure SetActive                ( const aValue: Boolean);
    function  GetTick                  : Integer;
    procedure SetPeriodms              ( const aValue: Integer);
    function  RestartTimer             ( aHighResTicksSinceLastTimerTick: Int64): Boolean;
  protected
    procedure Execute; override;
  public
    property  OnTimerTick              : TOnTimerTick read FOnTimerTick write FOnTimerTick;
    property  DueTimeUTCorRel          : Int64 read FDueTimeUTCorRel write FDueTimeUTCorRel;
    property  Periodms                 : Integer read FPeriodms write SetPeriodms;
    property  MaxTickCount             : Integer read FMaxTickCount write FMaxTickCount;
    property  Tick                     : Integer read GetTick;
    function  NextTick                 ( aTickDelta: Integer=1): Integer;
    function  ResetTick                ( aTick: Integer=0): Integer;

    function  RunningHighResTicks      : Int64;

    property  Active                   : Boolean read GetActive write SetActive;

    function  Start                    : Boolean; overload;
    function  Start                    ( aDueTimeUTCorRel: Int64; aPeriodms: Integer; aCount: Integer): Boolean; overload;
    function  Start                    ( aDueTimeUTCorRel: TDateTime; aPeriod: TDateTime; aCount: Integer): Boolean; overload;
    function  Stop                     : Boolean;
    function  Proceed                  : Boolean;
  end;

implementation

var
  hrtResolution: TDateTime=0;
  hrtFrequency: Int64=0;

// IsValidHandle and CloseAndNilHandle have a copy in (IMB) ConnectionStream,
// copied here to remove dependency

function IsValidHandle(aHandle: THandle): Boolean;
begin
  Result := (aHandle<>INVALID_HANDLE_VALUE) AND (aHandle<>0); // check for 0 in case of auto value initialisation
end;

function CloseAndNilHandle(var aHandle: THandle): Boolean;
var
  LocHandle: THandle;
begin
  if IsValidHandle(aHandle) then
  begin
    LocHandle := aHandle; // locally store handle
    aHandle := INVALID_HANDLE_VALUE; // signal via handle that it is invalid
    Result := CloseHandle(LocHandle); // close after signal invalid because of possible thread race condition
  end
  else Result := False;
end;

function HighResTickResolution: TDateTime;
begin
  if hrtResolution=0
  then hrtResolution := dtOneSecond/HighResTickFrequency;
  Result := hrtResolution;
end;

function HighResTickFrequency: Int64;
begin
  if hrtFrequency=0
  then QueryPerformanceFrequency(hrtFrequency);
  Result := hrtFrequency;
end;

function HighResTick: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function HighResTickDurationStr(aHighResStartTick, aHighResEndTick: Int64): string;
begin
  if aHighResEndTick=InvalidHighResTick
  then aHighResEndTick := HighResTick;
  Result := HighResDurationToStr((aHighResEndTick-aHighResStartTick)*HighResTickResolution);
end;


{ TIMBTimer }

constructor TIMBTimer.Create;
begin
  FOnTimerTick := nil;
  // create timer
  FTimerHandle := CreateWaitableTimer(nil, False, nil);
  // create timer thread in suspened state
  inherited Create(True);
end;

destructor TIMBTimer.Destroy;
var
  LocDueTime: Int64;
begin
  // signal termination
  Terminate;
  // unlock waiting thread
  if IsValidHandle(FTimerHandle) then
  begin
    LocDueTime := 0; // immediately
    SetWaitableTimer(FTimerHandle, LocDueTime, 0, nil, nil, True);
  end;
  // make sure thread is running or terminated
  Suspended := False;
  WaitFor;
  // cleanup timer
  CloseAndNilHandle(FTimerHandle);
  inherited;
end;

procedure TIMBTimer.Execute;
begin
  while IsValidHandle(FTimerHandle) and not Terminated do
  begin
    if (WaitForSingleObject(FTimerHandle, INFINITE)=WAIT_OBJECT_0) and (not Terminated) and (not FGoToSuspend) then
    begin
      // handle timer tick
      if Assigned(OnTimerTick)
      then OnTimerTick(Self, Tick)
      else NextTick;
      // timer could have been canceled in OnTimerTick
      if not Terminated then
      begin
        if (FPeriodms=0) or (Tick>=FMaxTickCount)
        then Stop;
      end;
      FLastHighResTick := HighResTick;
    end
    else
    begin
      // thread was terminated or timer was abandonded
      // abondon will never happen (no windows function for this in api but implemented for completeness
      if not Terminated then
      begin
        //Stop;
        Suspended := True;
      end;
    end;
  end;
end;

function TIMBTimer.GetActive: Boolean;
begin
  Result := (Tick<FMaxTickCount) and not Suspended;
end;

function TIMBTimer.GetTick: Integer;
begin
  Result := FTick; // is default an atomic read
end;

function TIMBTimer.NextTick(aTickDelta: Integer): Integer;
begin
  InterlockedExchangeAdd(FTick, aTickDelta);
  Result := FTick;
end;

function TIMBTimer.ResetTick(aTick: Integer): Integer;
begin
  Result := InterlockedExchange(FTick, aTick);
  if aTick=0
  then FStartHighResTick := HighResTick;
end;

function TIMBTimer.RestartTimer(aHighResTicksSinceLastTimerTick: Int64): Boolean;
var
  msSinceLastTick: Integer;
  ns100TillNextTick: Int64;
begin
  if FLastHighResTick>=0 then
  begin // timer is running: adjust start time to now and fixup for ne time left till next tick
    try
      msSinceLastTick := Round(1000*aHighResTicksSinceLastTimerTick/HighResTickFrequency);
      ns100TillNextTick := -(FPeriodms-msSinceLastTick)*10000;
    except
      ns100TillNextTick := 0;
    end;
    // time left? set timer relative to now (ie negative number of 100 ns)
    if ns100TillNextTick>0
    then ns100TillNextTick := 0;
    Result := SetWaitableTimer(FTimerHandle, ns100TillNextTick, FPeriodms, nil, nil, True);
  end
  else Result := SetWaitableTimer(FTimerHandle, FDueTimeUTCorRel, FPeriodms, nil, nil, True);
end;

function TIMBTimer.RunningHighResTicks: Int64;
begin
  Result := HighResTick-FStartHighResTick;
end;

function TIMBTimer.Proceed: Boolean;
begin
  if FGoToSuspend then
  begin
    // reinit timer
    FGoToSuspend := False;
    Result := RestartTimer(FStopHighResTick-FLastHighResTick);
    // resume thread
    Suspended := False;
  end
  else Result := True; // we were not suspended
end;

procedure TIMBTimer.SetActive(const aValue: Boolean);
begin
  if Active xor aValue then
  begin
    if aValue
    then Start
    else Stop;
  end;
end;

procedure TIMBTimer.SetPeriodms(const aValue: Integer);
begin
  if FPeriodms<>aValue then
  begin
    FPeriodms := aValue;
    if Active
    then RestartTimer(HighResTick-FLastHighResTick);
  end;
end;

function TIMBTimer.Start: Boolean;
begin
  ResetTick;
  FLastHighResTick := -1;
  FGoToSuspend := False;
  Result := SetWaitableTimer(FTimerHandle, FDueTimeUTCorRel, FPeriodms, nil, nil, True);
  Suspended := False;
end;

function TIMBTimer.Start(aDueTimeUTCorRel: Int64; aPeriodms: Integer; aCount: Integer): Boolean;
begin
  FDueTimeUTCorRel := aDueTimeUTCorRel;
  FPeriodms := aPeriodms;
  FMaxTickCount := aCount;
  Result := Start;
end;

function TIMBTimer.Start(aDueTimeUTCorRel, aPeriod: TDateTime; aCount: Integer): Boolean;
begin
  if aDueTimeUTCorRel>0
  // absolute
  then Result := Start(DateTimeToInt64(aDueTimeUTCorRel), Round(aPeriod/dtOneMilliSecond), aCount)
  // relative
  else Result := Start(Round(aDueTimeUTCorRel*itOneDay), Round(aPeriod/dtOneMilliSecond), aCount);
end;

function TIMBTimer.Stop: Boolean;
begin
  FStopHighResTick := HighResTick;
  FGoToSuspend := True;
  Result := CancelWaitableTimer(FTimerHandle);
end;

end.
