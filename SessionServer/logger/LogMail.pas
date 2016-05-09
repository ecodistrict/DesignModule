unit LogMail;

interface

uses
  Logger,
  StdIni,
  MailLib,
  IniFiles, ComObj, Classes, Windows, SysUtils;

const
  MailSection = 'Mail';
    ToItem = 'To';
    CcItem = 'Cc';
    BccItem = 'Bcc';
    MailServerItem = 'MailServer';
      DefMailServer = MailHost;
    FromItem = 'From';
      DefaultFromMailAddress = 'logmailer@tno.nl';
    SubjectItem = 'Subject';


type
  TMailLogger = class(TLogBase)
  constructor Create(aLog: TLog);
  destructor Destroy; override;
  private
    fLines: TStringList;
    procedure SendMail;
    function DefaultSubject: string;
  public
    SendIt: Boolean;
    SendItOn: TLogLevelSet;
    SendAsHtml: Boolean;
    procedure WriteLn(const aLine: string; aLevel: TLogLevel); override;
  end;

var
  MailLogger: TMailLogger;

implementation

{ TConsoleLogger }

constructor TMailLogger.Create(aLog: TLog);
begin
  fLines := TStringList.Create;
  SendIt := False;
  SendItOn := [llWarning, llError];
  SendAsHtml := True;
  inherited Create(aLog);
end;

function TMailLogger.DefaultSubject: string;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '') + ': errors';
end;

destructor TMailLogger.Destroy;
begin
  if SendIt
  then SendMail; // send collected entries to recipients as defined in StandardIni
  inherited;
  fLines.Free;
end;

procedure TMailLogger.SendMail;
begin
  MailLib.SendMail(SendAsHtml, fLines, StandardIni.Readstring(MailSection, MailServerItem, DefMailServer),
    StandardIni.Readstring(MailSection, FromItem, DefaultFromMailAddress), StandardIni.Readstring(MailSection, SubjectItem, DefaultSubject),
    StandardIni.Readstring(MailSection, ToItem, ''), StandardIni.Readstring(MailSection, CcItem, ''),
    StandardIni.Readstring(MailSection, BccItem, ''));
end;

procedure TMailLogger.WriteLn(const aLine: string; aLevel: TLogLevel);

  function LeadingSpaces(const Str: string): string;
  var
    s, i: Integer;
  begin
    Result := Str;
    // find last space before text
    s := 0;
    while (s < Length(Result)) AND (Result[s + 1] = ' ')
    do s := s + 1;
    // replace spaces backwards with html space
    for i := s downto 1 do
    begin
      Delete(Result, i, 1);
      Insert('&nbsp;', Result, i);
    end;
  end;

begin
  if aLevel in SendItOn
  then SendIt := True;
  if SendAsHtml then
  begin
    case aLevel of
      llRemark:
        fLines.Add('<font color="#000099">' + LeadingSpaces(aLine) + '</font><br>');
      llWarning:
        fLines.Add('<font color="#CCCC00">' + LeadingSpaces(aLine) + '</font><br>');
      llError:
        fLines.Add('<font color="#FF3300">' + LeadingSpaces(aLine) + '</font><br>');
    else
        fLines.Add(LeadingSpaces(aLine) + '<br>');
    end;
  end
  else  fLines.Add(aLine);
end;

initialization
  // call InitProc so CDO can be used
  if InitProc <> nil
  then TProcedure(InitProc);
  MailLogger := TMailLogger.Create(Log);
finalization
  MailLogger := nil;
end.
