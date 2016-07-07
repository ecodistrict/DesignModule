unit MyThreads;

{
  Start a thread via a method instead of deriving a customized class from TThread every time.
  Returned Thread is marked as FreeAnTerminate so no freeing is necessary.
  Created thread starts given method (of object).
  TMyThread exposes Terminated property so it can be checked outside of the TThread class

  use: call StartThread with the method to be executed in a seperate thread
}
interface

uses
  DTTime, Classes, Generics.Collections, SyncObjs, SysUtils, Windows;

// single threads

type
  TMyThread = class; // forward declaration

  TThreadedMethod = procedure(aThread: TMyThread) of object;
  TThreadedProc = procedure(aThread: TMyThread);

  TMyThread = class(TThread)
    constructor Create(aMethod: TThreadedMethod; aData: TObject; aCreateSuspended, aFreeOnTerminate: Boolean); overload;
  private
    FMethod: TThreadedMethod;
    FData: TObject;
  protected
    procedure Execute; override;
  public
    property Terminated;
    property Data: TObject read FData;
  end;

function StartThread(aMethod: TThreadedMethod; aData: TObject = nil; aCreateSuspended: Boolean = False; aFreeOnTerminate: Boolean = True): TMyThread; overload;
function StartThread(aProc: TThreadedProc; aData: TObject = nil; aCreateSuspended: Boolean = False; aFreeOnTerminate: Boolean = True): TMyThread; overload;

// thread pool

type
  TMyThreadPool = class; // forward declaration

  TMyThreadPoolQueueMethod = procedure(aThreadPool: TMyThreadPool; aParam: Integer) of object;

  TMyThreadPoolEndEvent = class
    constructor Create(aInitialCount: Integer);
    destructor Destroy; override;
  private
    FCount: Integer;
    FEvent: TEvent;
  public
    procedure Release;
    function WaitFor(aTimeOut: LongWord = INFINITE): TWaitResult;
    procedure ResetCount(aCount: Integer);
  end;

  TMyThreadPoolQueueEntry = class
    constructor Create(aMethod: TMyThreadPoolQueueMethod; aParam: Integer; aEndEvent: TMyThreadPoolEndEvent; aNextEntry: TMyThreadPoolQueueEntry);
    destructor Destroy; override;
  private
    FMethod: TMyThreadPoolQueueMethod;
    FParam: Integer;
    FEndEvent: TMyThreadPoolEndEvent; // not owned
    FNextEntry: TMyThreadPoolQueueEntry; // chain of entries
  public
    property EndEvent: TMyThreadPoolEndEvent read FEndEvent;
    function Run(aThreadPool: TMyThreadPool): Boolean;
  end;

  TMyThreadPoolQueue = TQueue<TMyThreadPoolQueueEntry>;

  TMyThreadPoolThread = class(TThread)
    constructor Create(aThreadPool: TMyThreadPool);
  private
    FThreadPool: TMyThreadPool;
  protected
    procedure Execute; override;
  end;

  TMyThreadPool = class
    constructor Create(aThreadCount: Integer; aMaxQueueLength: Integer = MaxInt; aWaitTimeOut: LongWord = 1000);
    destructor Destroy; override;
  private
    FThreads: array of TMyThreadPoolThread;
    FQueue: TMyThreadPoolQueue;
    FLock: TCriticalSection; // todo:
    FSemaphore: TSemaphore; // todo:
    FActiveThreads: Integer;
    FClaimedThreads: Integer;
    FWaitTimeOut: LongWord;
    function GetThreadCount: Integer;
    procedure SetThreadCount(const aValue: Integer);
  protected
    function Dequeue: TMyThreadPoolQueueEntry;
  public
    property WaitTimeOut: LongWord read FWaitTimeOut write FWaitTimeOut;
    property ThreadCount: Integer read GetThreadCount write SetThreadCount;
    property ActiveThreads: Integer read FActiveThreads;
    procedure Enqueue(aMethod: TMyThreadPoolQueueMethod; aParam: Integer = 0; aEndEvent: TMyThreadPoolEndEvent = nil); overload;
    procedure Enqueue(aMethods: array of TMyThreadPoolQueueMethod; aParams: array of Integer; aEndEvent: TMyThreadPoolEndEvent = nil); overload;
    class function CreateEndEvent(aInitialCount: Integer): TMyThreadPoolEndEvent;
    function ClaimThreads(aThreadCount: Integer): Boolean;
    procedure UnClaimThreads(aThreadCount: Integer);
  end;


  // quick time measuring by reading system tick count

const
  InvalidHighResTick = -1;

function HighResTick: Int64;
function HighResTickDuration(aHighResStartTick: Int64; aHighResEndTick: Int64 = InvalidHighResTick): TDateTime;
function HighResTickDurationStr(aHighResStartTick: Int64; aHighResEndTick: Int64 = InvalidHighResTick): string;

function HighResTickResolution: TDateTime;
function HighResTickFrequency: Int64;


implementation

{ HighResTimer functions }

var
  hrtResolution: TDateTime = 0;
  hrtFrequency: Int64 = 0;

function HighResTickResolution: TDateTime;
begin
  if hrtResolution = 0
  then hrtResolution := dtOneSecond / HighResTickFrequency;
  Result := hrtResolution;
end;

function HighResTickFrequency: Int64;
begin
  if hrtFrequency = 0
  then QueryPerformanceFrequency(hrtFrequency);
  Result := hrtFrequency;
end;

function HighResTick: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function HighResTickDuration(aHighResStartTick: Int64; aHighResEndTick: Int64 = InvalidHighResTick): TDateTime;
begin
  if aHighResEndTick = InvalidHighResTick
  then aHighResEndTick := HighResTick;
  Result := (aHighResEndTick-aHighResStartTick)*HighResTickResolution;
end;

function HighResTickDurationStr(aHighResStartTick, aHighResEndTick: Int64): string;
begin
  if aHighResEndTick = InvalidHighResTick
  then aHighResEndTick := HighResTick;
  Result := HighResDurationToStr((aHighResEndTick-aHighResStartTick)*HighResTickResolution);
end;

{ TMyThread }

constructor TMyThread.Create(aMethod: TThreadedMethod; aData: TObject; aCreateSuspended, aFreeOnTerminate: Boolean);
begin
  FMethod := aMethod;
  FData := aData;
  FreeOnTerminate := aFreeOnTerminate;
  inherited Create(aCreateSuspended);
end;

procedure TMyThread.Execute;
begin
  if Assigned(FMethod) then
    FMethod(Self);
end;

function StartThread(aMethod: TThreadedMethod; aData: TObject; aCreateSuspended, aFreeOnTerminate: Boolean): TMyThread;
begin
  Result := TMyThread.Create(aMethod, aData, aCreateSuspended, aFreeOnTerminate);
end;

function StartThread(aProc: TThreadedProc; aData: TObject; aCreateSuspended, aFreeOnTerminate: Boolean): TMyThread;
var
  LocMethod: TThreadedMethod;
begin
  // recalc eventhandler and self to method
  TMethod(LocMethod).Code := @aProc;
  TMethod(LocMethod).Data := nil;
  Result := TMyThread.Create(LocMethod, aData, aCreateSuspended, aFreeOnTerminate);
end;

{ TMyThreadPoolEndEvent }

constructor TMyThreadPoolEndEvent.Create(aInitialCount: Integer);
begin
  inherited Create;
  FCount := aInitialCount;
  if FCount > 0 then
    FEvent := TEvent.Create(nil, False, False, '')
  else
    FEvent := nil;
end;

destructor TMyThreadPoolEndEvent.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

procedure TMyThreadPoolEndEvent.Release;
begin
  if (InterlockedDecrement(FCount) = 0) and Assigned(FEvent) then
    FEvent.SetEvent;
end;

procedure TMyThreadPoolEndEvent.ResetCount(aCount: Integer);
begin
  FCount := aCount;
  if FCount > 0 then
  begin
    FEvent.Free;
    FEvent := TEvent.Create(nil, False, False, '');
  end
  else
    FreeAndNil(FEvent);
end;

function TMyThreadPoolEndEvent.WaitFor(aTimeOut: LongWord): TWaitResult;
begin
  if Assigned(FEvent) then
    Result := FEvent.WaitFor(aTimeOut)
  else
    Result := wrError;
end;

{ TMyThreadPoolQueueEntry }

constructor TMyThreadPoolQueueEntry.Create(aMethod: TMyThreadPoolQueueMethod; aParam: Integer;
  aEndEvent: TMyThreadPoolEndEvent; aNextEntry: TMyThreadPoolQueueEntry);
begin
  inherited Create;
  FMethod := aMethod;
  FParam := aParam;
  FEndEvent := aEndEvent;
  FNextEntry := aNextEntry;
end;

destructor TMyThreadPoolQueueEntry.Destroy;
begin
  FEndEvent := nil; // not owned
  FreeAndNil(FNextEntry);
  inherited;
end;

function TMyThreadPoolQueueEntry.Run(aThreadPool: TMyThreadPool): Boolean;
var
  Entry: TMyThreadPoolQueueEntry;
begin
  try
    try
      // process list of entries
      Entry := Self;
      while Assigned(Entry) do
      begin
        Entry.FMethod(aThreadPool, FParam);
        Entry := Entry.FNextEntry;
      end;
    finally
      EndEvent.Release;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

{ TMyThreadPoolThread }

constructor TMyThreadPoolThread.Create(aThreadPool: TMyThreadPool);
begin
  FreeOnTerminate := True;
  FThreadPool := aThreadPool;
  inherited Create(False);
end;

procedure TMyThreadPoolThread.Execute;
var
  Entry: TMyThreadPoolQueueEntry;
begin
  while not Terminated do
  begin
    Entry := FThreadPool.Dequeue;
    if Assigned(Entry) then
    begin
      try
        InterlockedIncrement(FThreadPool.FActiveThreads);
        try
          if not Entry.Run(FThreadPool) then
            ; // todo:
        finally
          InterlockedDecrement(FThreadPool.FActiveThreads);
        end;
      finally
        Entry.Free;
      end;
    end;
  end;
end;

{ TMyThreadPool }

function TMyThreadPool.ClaimThreads(aThreadCount: Integer): Boolean;
var
  CurrentCount: Integer;
begin
  CurrentCount := FClaimedThreads;
  if CurrentCount + aThreadCount <= Length(FThreads) then
    Result := InterlockedCompareExchange(FClaimedThreads, CurrentCount + aThreadCount, CurrentCount) = CurrentCount
  else
    Result := False;
end;

constructor TMyThreadPool.Create(aThreadCount, aMaxQueueLength: Integer; aWaitTimeOut: LongWord);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FSemaphore := TSemaphore.Create(nil, 0, aMaxQueueLength, '');
  SetLength(FThreads, 0);
  FQueue := TMyThreadPoolQueue.Create;
  FWaitTimeOut := aWaitTimeOut;
  ThreadCount := aThreadCount;
  FActiveThreads := 0;
  FClaimedThreads := 0;
end;

class function TMyThreadPool.CreateEndEvent(aInitialCount: Integer): TMyThreadPoolEndEvent;
begin
  Result := TMyThreadPoolEndEvent.Create(aInitialCount);
end;

function TMyThreadPool.Dequeue: TMyThreadPoolQueueEntry;
begin
  if FSemaphore.WaitFor(WaitTimeOut) = wrSignaled then
  begin
    FLock.Acquire;
    try
      Result := FQueue.Dequeue;
    finally
      FLock.Release;
    end;
  end
  else Result := nil;
end;

destructor TMyThreadPool.Destroy;
begin
  ThreadCount := 0;
  FreeAndNil(FQueue);
  FreeAndNil(FLock);
  FreeAndNil(FSemaphore);
  inherited;
end;

procedure TMyThreadPool.Enqueue(aMethods: array of TMyThreadPoolQueueMethod; aParams: array of Integer; aEndEvent: TMyThreadPoolEndEvent);
var
  PrevEntry: TMyThreadPoolQueueEntry;
  NextEntry: TMyThreadPoolQueueEntry;
  m: Integer;
begin
  FLock.Acquire;
  try
    // build chain of queue entries from last to first
    NextEntry := nil;
    m := Length(aMethods) - 1;
    while m >= 0 do
    begin
      PrevEntry := NextEntry;
      NextEntry := TMyThreadPoolQueueEntry.Create(aMethods[m], aParams[m], aEndEvent, PrevEntry);
      m := m - 1;
    end;
    FQueue.Enqueue(NextEntry);
    FSemaphore.Release;
  finally
    FLock.Release;
  end;
end;

procedure TMyThreadPool.Enqueue(aMethod: TMyThreadPoolQueueMethod; aParam: Integer; aEndEvent: TMyThreadPoolEndEvent);
begin
  FLock.Acquire;
  try
    FQueue.Enqueue(TMyThreadPoolQueueEntry.Create(aMethod, aParam, aEndEvent, nil));
    FSemaphore.Release;
  finally
    FLock.Release;
  end;
end;

function TMyThreadPool.GetThreadCount: Integer;
begin
  FLock.Acquire;
  try
    Result := Length(FThreads);
  finally
    FLock.Release;
  end;
end;

procedure TMyThreadPool.SetThreadCount(const aValue: Integer);
var
  t: Integer;
begin
  FLock.Acquire;
  try
    if Length(FThreads) <> aValue then
    begin
      if Length(FThreads) > aValue then
      begin // reduce number of threads
        for t := aValue to Length(FThreads) - 1
        do FThreads[t].Terminate;
        SetLength(FThreads, aValue);
      end
      else
      begin // add threads
        t := Length(FThreads);
        SetLength(FThreads, aValue);
        for t := t to aValue - 1
        do FThreads[t] := TMyThreadPoolThread.Create(Self);
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TMyThreadPool.UnClaimThreads(aThreadCount: Integer);
var
  t: Integer;
begin
  for t := 0 to aThreadCount - 1
  do InterlockedDecrement(FClaimedThreads);
end;

end.
