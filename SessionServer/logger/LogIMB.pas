unit LogIMB;

interface

uses
  SysUtils,
  Logger,
  IMB3Core,
  IMB3NativeClient;

const
  DefaultLogEventName = 'Log';

type
  TIMBLogger = class(TLogBase)
  constructor Create(aConnection: TIMBConnection; const aLogEventName: string = DefaultLogEventName);
  private
    fLogEvent: TIMBEventEntry;
  public
    procedure WriteLn(const aLine: string; aLevel: Logger.TLogLevel); override;
  end;

function AddIMBLogger(aConnection: TIMBConnection; const aLogEventName: string = DefaultLogEventName): TIMBLogger;

implementation

function AddIMBLogger(aConnection: TIMBConnection; const aLogEventName: string): TIMBLogger;
begin
  Result := TIMBLogger.Create(aConnection, aLogEventName);
end;

{ TLogIMB }

constructor TIMBLogger.Create(aConnection: TIMBConnection; const aLogEventName: string);
begin
  fLogEvent := aConnection.Publish(aLogEventName);
  inherited Create(Log);
  SetLogDef(AllLogLevels, [llsNoPre]);
end;

procedure TIMBLogger.WriteLn(const aLine: string; aLevel: Logger.TLogLevel);
begin
  if Assigned(fLogEvent)
  then fLogEvent.LogWriteLn(fLogEvent.Connection.OwnerName + '(' + fLogEvent.Connection.OwnerID.ToString + '): ' + aLine, Integer(aLevel));
end;

end.
