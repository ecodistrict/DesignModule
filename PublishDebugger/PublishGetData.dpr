program PublishGetData;

uses
  Vcl.Forms,
  PublishGetDataMainFrm in 'PublishGetDataMainFrm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
