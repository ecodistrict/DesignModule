unit LogFileRtf;

interface

uses
  Logger, LogFile,
  Windows, SysUtils;

const
  RtfExt = '.rtf';

type
  PTextFile = ^TextFile;

  TFileRtfLogger = class(TFileLogger)
  constructor Create(aLog: TLog);
  protected
    procedure HookFileOpen(aAppended: Boolean); override;
    procedure HookFileClose; override;
  public
    procedure WriteLn(const aLine: string; aLevel: TLogLevel); override;
  end;

var
  FileRtfLogger: TFileRtfLogger;

implementation

{ utils }

function EscapeDoubleBackslash(const s: string): string;
var
  i: Integer;
begin
  Result := s;
  for i := Length(Result) downto 1 do
  begin
    if Result[i] = '\' then
      Insert('\', Result, i);
  end;
end;

{ TFileRtfLogger }

constructor TFileRtfLogger.Create(aLog: TLog);
begin
  FLogExt := RtfExt;
  inherited Create(aLog);
end;

procedure TFileRtfLogger.HookFileClose;
begin
  System.Write(fFile, '}');
  inherited;
end;

const
  SEEK_CUR = 1;

procedure TFileRtfLogger.HookFileOpen(aAppended: Boolean);
begin
  inherited;
  if aAppended then
  begin // remove trailing }, do not have to truncate (close will write "}" or is overwritten with content)
    _llseek(TTextRec(fFile).Handle, -1, SEEK_CUR);
  end
  else
  begin // write header
    System.WriteLn(fFile, '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fmodern\fprq1\fcharset0 Courier New;}}');
    System.WriteLn(fFile, '{\colortbl ;\red0\green0\blue255;\red192\green192\blue0;\red128\green0\blue0;}');
    System.WriteLn(fFile, '{\*\generator logger rtf 1.0.0.0;}\viewkind4\uc1\pard\f0\fs20 ');
  end;
end;

procedure TFileRtfLogger.WriteLn(const aLine: string; aLevel: TLogLevel);
var
  ColorCode: string;
begin
  case aLevel of
    llRemark:
      ColorCode := '1'; // blue
    llWarning:
      ColorCode := '2'; // Yellow;
    llError:
      ColorCode := '3'; // Red;
  else // normal etc.
      ColorCode := '0'; // black
  end;
  System.WriteLn(FilePtr^, '\cf' + ColorCode + ' ' + EscapeDoubleBackslash(aLine) + '\par');
  if FCloseAfterWrite
  then Close
  else Flush(FilePtr^);
end;

initialization
  FileRtfLogger := TFileRtfLogger.Create(Log);
  // remove "derived from" object FileLogger (auto created)
  FreeAndNil(FileLogger);
finalization
  FileRtfLogger := nil;
end.
