unit CommandQueue;

interface

{$DEFINE CommandQueueLogging}

uses
  {$IFDEF CommandQueueLogging}
  Logger,
  {$ENDIF}
  System.Generics.Collections, System.Types, System.Classes, System.SysUtils;

const
  DefaultMaxQueueDepth = 10000;
  DefaultThreadCount = 8;

type
  TQueueRefObj = procedure(aSender: TObject) of object;
  //TQueueRef = reference to procedure(aSender: TObject);

  TQueueCommand = class
  constructor Create(aSender: TObject; aReference: TQueueRefObj);
  private
    fSender: TObject; // ref
    fReference: TQueueRefObj; // ref
  public
    property sender: TObject read fSender;
    property reference: TQueueRefObj read fReference;
  end;

  TQueueHandler = class(TThread)
  constructor Create(aCommandQueue: TThreadedQueue<TQueueCommand>);
  destructor Destroy; override;
  private
    fCommandQueue: TThreadedQueue<TQueueCommand>; // ref
  protected
    procedure Execute; override;
  end;

  TQueueHandlers = TObjectList<TQueueHandler>;

// call to deviate from defaults
procedure InitializeCommandQueue(aMaxQueueDepth: Integer=DefaultMaxQueueDepth; aThreadCount: Integer=DefaultThreadCount);
procedure FinalizeCommandQueue();

procedure AddCommandToQueue(aSender: TObject; aReference: TQueueRefObj);

implementation

var
  queue: TThreadedQueue<TQueueCommand>=nil;
  handlers: TQueueHandlers=nil;

procedure InitializeCommandQueue(aMaxQueueDepth, aThreadCount: Integer);
var
  t: Integer;
begin
  FinalizeCommandQueue();
  queue := TThreadedQueue<TQueueCommand>.Create(aMaxQueueDepth);
  handlers := TQueueHandlers.Create(True);
  for t := 0 to aThreadCount-1
  do handlers.Add(TQueueHandler.Create(queue));
end;

procedure FinalizeCommandQueue();
var
  handler: TQueueHandler;
begin
  // terminate all handlers
  if Assigned(handlers)
  then for handler in handlers do handler.Terminate;
  // stop queue
  if Assigned(queue)
  then queue.DoShutDown;
  // free all handlers
  FreeAndNil(handlers);
  FreeAndNil(queue);
end;

procedure AddCommandToQueue(aSender: TObject; aReference: TQueueRefObj);
var
  queueSize: Integer;
begin
  if Assigned(queue) then
  begin
    queue.PushItem(TQueueCommand.Create(aSender, aReference), queueSize);
    // logging progress (throttled)
    {$IFDEF CommandQueueLogging}
    if (queueSize<=30) or ((queueSize mod 100)=0)
    then Log.Progress('Queue: '+queueSize.ToString);
    {$ENDIF}
  end;
end;

{ TQueueCommand }

constructor TQueueCommand.Create(aSender: TObject; aReference: TQueueRefObj);
begin
  inherited Create;
  fSender := aSender;
  fReference := aReference;
end;

{ TQueueHandler }

constructor TQueueHandler.Create(aCommandQueue: TThreadedQueue<TQueueCommand>);
begin
  fCommandQueue := aCommandQueue;
  inherited Create(False);
  TThread.NameThreadForDebugging('CommandQueueHandler', ThreadID);
end;

destructor TQueueHandler.Destroy;
begin
  inherited;
  fCommandQueue := nil;
end;

procedure TQueueHandler.Execute;
var
  command: TQueueCommand;
  term: Boolean;
  queueSize: Integer;
  waitResult: TWaitResult;
begin
  repeat
    command := nil;
    waitResult := fCommandQueue.PopItem(queueSize, command);
    try
      term := TThread.CheckTerminated;
      if not term then
      begin
        // logging progress (throttled)
        {$IFDEF CommandQueueLogging}
        if (queueSize<=30) or ((queueSize mod 100)=0)
        then Log.Progress('Queue: '+queueSize.ToString);
        {$ENDIF}
        if waitResult=wrSignaled then
        begin
          try
            if Assigned(command.reference)
            then command.reference(command.sender);
          except
            {$IFDEF CommandQueueLogging}
            on E: Exception
            do Log.WriteLn('Exception in TQueueHandler.Execute: '+E.Message, llError);
            {$ENDIF}
          end;
        end;
      end;
    finally
      command.Free;
    end;
  until term;
end;

initialization
  InitializeCommandQueue(); // with defaults
finalization
  FinalizeCommandQueue();
end.
