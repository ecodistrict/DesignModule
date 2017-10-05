unit PublishGetDataMainFrm;

interface

uses
  Logger, LogFile, LogMemo,
  imb4,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    memoJSON: TMemo;
    editHostName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    editHostPort: TEdit;
    buttonApply: TButton;
    buttonHostConnect: TButton;
    comboEventName: TComboBox;
    labelConnectionStatus: TLabel;
    Label3: TLabel;
    labelEventsSend: TLabel;
    memoLog: TRichEdit;
    procedure buttonHostConnectClick(Sender: TObject);
    procedure buttonApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fConnection: TSocketConnection;
    procedure handleDisconnect(aConnection: TConnection);
    procedure handleException(aConnection: TConnection; aException: Exception);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.buttonApplyClick(Sender: TObject);
var
  event: TEventEntry;
begin
  event := fConnection.eventEntry(comboEventName.Text).publish;
  event.signalString(memoJSON.Text);
  labelEventsSend.Tag := labelEventsSend.Tag+1;
  labelEventsSend.Caption := labelEventsSend.Tag.ToString();
end;

procedure TMainForm.buttonHostConnectClick(Sender: TObject);
begin
  fConnection.Free;
  fConnection := TSocketConnection.Create('Publish debugger getData', 6, 'ecodistrict', editHostName.Text, string(editHostPort.Text).ToInteger);
  fConnection.onDisconnect := handleDisconnect;
  fConnection.onException := handleException;
  if fConnection.connected
  then labelConnectionStatus.Caption := 'conncted'
  else labelConnectionStatus.Caption := 'NOT conncted';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdaptMemo(memoLog);
  AddMemoLogger(memoLog);
end;

procedure TMainForm.handleDisconnect(aConnection: TConnection);
begin
  labelConnectionStatus.Caption := 'Disconnected';
end;

procedure TMainForm.handleException(aConnection: TConnection; aException: Exception);
begin
  labelConnectionStatus.Caption := '## '+aException.Message;
end;

end.
