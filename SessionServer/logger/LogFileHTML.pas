unit LogFileHTML;

interface

uses
  Logger, LogFile,
  SysUtils;

const
  HTMLExt = '.html';
  DefFontSetting = 'face="Courier New, Courier, monospace" size="2"';

type
  TFileHTMLLogger = class(TFileLogger)
  constructor Create(aLog: TLog);
  private
    fFontSetting: string;
    procedure SetFontSetting(const aValue: string);
  protected
    procedure HookFileOpen(aAppened: Boolean); override;
    procedure HookFileClose; override;
  public
    procedure WriteLn(const aLine: string; aLevel: TLogLevel); override;
    property Fontsetting: string read fFontSetting write SetFontSetting;
  end;

var
  FileHTMLLogger: TFileHTMLLogger;

implementation

{ TFileHTMLLogger }

constructor TFileHTMLLogger.Create(aLog: TLog);
begin
  fLogExt := HTMLExt;
  fFontSetting := DefFontSetting;
  inherited Create(aLog);
end;

procedure TFileHTMLLogger.HookFileClose;
begin
  System.WriteLn(fFile, '</font>');
  inherited;
end;

procedure TFileHTMLLogger.HookFileOpen(aAppened: Boolean);
begin
  inherited;
  System.WriteLn(fFile, '<font ' + Fontsetting + '>');
end;

procedure TFileHTMLLogger.SetFontSetting(const aValue: string);
begin
  Close;
  fFontSetting := aValue;
end;

procedure TFileHTMLLogger.WriteLn(const aLine: string; aLevel: TLogLevel);

  function LeadingSpaces(const Str: string): string;
  var
    s, i: Integer;
  begin
    Result := Str;
    // find last space before text
    s := 0;
    while (s < Length(Result)) AND (Result[s + 1] = ' ') do
      s := s + 1;
    // replace spaces backwards with html space
    for i := s downto 1 do
    begin
      Delete(Result, i, 1);
      Insert('&nbsp;', Result, i);
    end;
  end;

begin
  case aLevel of
    llRemark:
      System.WriteLn(FilePtr^, '<font color="#000099">' + LeadingSpaces(aLine) + '</font><br>');
    llWarning:
      System.WriteLn(FilePtr^, '<font color="#CCCC00">' + LeadingSpaces(aLine) + '</font><br>');
    llError:
      System.WriteLn(FilePtr^, '<font color="#FF3300">' + LeadingSpaces(aLine) + '</font><br>');
  else
      System.WriteLn(FilePtr^, LeadingSpaces(aLine) + '<br>');
  end;
  if FCloseAfterWrite
  then Close
  else Flush(FilePtr^);
end;

initialization
  FileHTMLLogger := TFileHTMLLogger.Create(Log);
  // remove "derived from" object FileLogger (auto created)
  FreeAndNil(FileLogger);
finalization
  FileHTMLLogger := nil;
end.
