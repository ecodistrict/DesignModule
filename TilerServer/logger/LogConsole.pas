unit LogConsole;

// written by J.A. Cornelissen

interface

uses
  Logger,
  MyConsole,
  Windows, SysUtils;

type
  TConsoleLogger = class(TLogBase)
  constructor Create(aLog: TLog);
  public
    procedure WriteLn(const aLine: String; aLevel: TLogLevel); override;
    procedure Progress(const aLine: String); override;
    procedure LeaveProgress; override;
    procedure Clear; override;
  end;

var
  ConsoleLogger: TConsoleLogger;

implementation

{ TConsoleLogger }

procedure TConsoleLogger.Clear;
begin
  // Console.ClrScrn;
end;

constructor TConsoleLogger.Create(aLog: TLog);
begin
  inherited Create(aLog);
  SetLogDef([llStart, llFinish, llStamp], [llsNotActive]);
end;

procedure TConsoleLogger.LeaveProgress;
begin
  System.WriteLn;
end;

procedure TConsoleLogger.Progress(const aLine: String);
var
  cs: TCoord;
  cfc: Byte;
begin
  cs := Console.Cursor;
  cfc := Console.ForegroundColor;
  Console.ForegroundColor := ccWhite;
  System.Write(MyCharToOem(Console.MaxFitWrite(aLine)));
  Console.ClrEol;
  Console.ForegroundColor := cfc;
  Console.Cursor := cs;
end;

procedure TConsoleLogger.WriteLn(const aLine: String; aLevel: TLogLevel);
var
  cfc: Byte;
begin
  cfc := Console.ForegroundColor;
  case aLevel of
    llRemark:
      Console.ForegroundColor := ccBlue;
    llWarning:
      Console.ForegroundColor := ccYellow;
    llError:
      Console.ForegroundColor := ccRed;
    llOK:
      Console.ForegroundColor := ccLime;
  else // normal etc.
      Console.ForegroundColor := ccSilver;
  end;
  System.WriteLn(MyCharToOem(aLine));
  Console.ForegroundColor := cfc;
end;

initialization
  ConsoleLogger := TConsoleLogger.Create(Log);
finalization
  ConsoleLogger := nil;
end.
