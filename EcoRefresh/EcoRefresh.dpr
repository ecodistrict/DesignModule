program EcoRefresh;

uses
  Vcl.Forms,
  EcoRefreshMainFrm in 'EcoRefreshMainFrm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
