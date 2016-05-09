unit MailLib;
{ 
????-??-?? (HC) - created
2008-04-11 (RS) - added const MailHost; minor reformatting
}

interface

uses
  ComObj, Classes, Windows, SysUtils;

const
  MailHost = 'mailhost.tno.nl';

procedure SendMail(BodyIsHTML: Boolean; aBody: TStrings; const MailServer, From, Subject, aTo, CC, BCC: String);

implementation

procedure SendMail(BodyIsHTML: Boolean; aBody: TStrings; const MailServer, From, Subject, aTo, CC, BCC: String);
var
  objEMail: Variant;
begin
  objEmail := CreateOleObject('CDO.Message');
  objEmail.From := From;
  objEmail.Subject := Subject;
  if BodyIsHTML
  then objEmail.HTMLBody := '<body><font size="-1" face="Courier New, Courier, mono">'+aBody.Text+'</font></body>'
  else objEmail.TextBody := aBody.Text;
  objEmail.Configuration.Fields.Item['http://schemas.microsoft.com/cdo/configuration/sendusing'] := 2;
  objEmail.Configuration.Fields.Item['http://schemas.microsoft.com/cdo/configuration/smtpserver'] := MailServer;
  objEmail.Configuration.Fields.Item['http://schemas.microsoft.com/cdo/configuration/smtpserverport'] := 25;
  objEmail.To := aTo;
  objEmail.CC := CC;
  objEmail.BCC := BCC;
  objEmail.Configuration.Fields.Update;
  objEmail.Send;
end;

initialization
  if InitProc <> nil then TProcedure(InitProc);
end.
