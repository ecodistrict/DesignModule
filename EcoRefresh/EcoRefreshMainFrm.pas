unit EcoRefreshMainFrm;

interface

uses
  StdIni,
  imb4,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    btnRefreshDMQueries: TButton;
    btnRefreshDIQueries: TButton;
    btnRefreshDIObjectProperties: TButton;
    btnRefreshDIMeasuresHistory: TButton;
    btnRefreshDIData: TButton;
    Label2: TLabel;
    editIMBRemoteHost: TEdit;
    Label3: TLabel;
    editIMBRemotePort: TEdit;
    btnConnect: TButton;
    labelConnectionStatus: TLabel;
    Label4: TLabel;
    cmbCaseId: TComboBox;
    Label5: TLabel;
    cmbUserId: TComboBox;
    Label6: TLabel;
    memoResponse: TMemo;
    Label7: TLabel;
    cmbVariantId: TComboBox;
    procedure btnConnectClick(Sender: TObject);
    procedure btnRefreshDMQueriesClick(Sender: TObject);
    procedure btnRefreshDIQueriesClick(Sender: TObject);
    procedure btnRefreshDIObjectPropertiesClick(Sender: TObject);
    procedure btnRefreshDIMeasuresHistoryClick(Sender: TObject);
    procedure btnRefreshDIDataClick(Sender: TObject);
  private
    fConnection: TConnection;
    fDataEvent: TEventEntry;
    fPrivateEvent: TEventEntry;
    procedure HandleIMBConnectionDisconnect(aConnection: TConnection);
    procedure HandleIMBConnectionException(aConnection: TConnection; aException: Exception);
    procedure HandlePrivateEventOnString(aEventEntry: TEventEntry; const aString: string);
    function RemoveComment(const aID: string): string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  fConnection.Free;
  fConnection := TSocketConnection.Create('EcoRefresh', 94, 'ecodistrict', editIMBRemoteHost.Text, string(editIMBRemotePort.Text).toInteger());
  fConnection.onDisconnect := HandleIMBConnectionDisconnect;
  fConnection.onException := HandleIMBConnectionException;
  fDataEvent := fConnection.publish('data');
  fPrivateEvent := fConnection.subscribe(fConnection.UniqueClientID.ToString);
  fPrivateEvent.OnString.Add(HandlePrivateEventOnString);
  labelConnectionStatus.Font.Color := clBlack;
  if fConnection.connected
  then labelConnectionStatus.Caption := 'connected'
  else labelConnectionStatus.Caption := 'disconnected';
end;

procedure TMainForm.btnRefreshDIDataClick(Sender: TObject);
begin
  fDataEvent.signalString(
    '{'+
      '"type": "request",'+
      '"method": "refresh",'+
      '"caseId": "'+RemoveComment(cmbCaseId.Text)+'",'+
      '"variantId": "'+RemoveComment(cmbVariantId.Text)+'",'+
      '"userId": "'+RemoveComment(cmbUserId.Text)+'",'+
      '"eventId": "'+fConnection.UniqueClientID.ToString+'"'+
    '}'
  );
end;

procedure TMainForm.btnRefreshDIMeasuresHistoryClick(Sender: TObject);
begin
  fDataEvent.signalString(
    '{'+
      '"type": "request",'+
      '"method": "readDIMeasuresHistory",'+
      '"caseId": "'+RemoveComment(cmbCaseId.Text)+'",'+
      '"userId": "'+RemoveComment(cmbUserId.Text)+'",'+
      '"eventId": "'+fConnection.UniqueClientID.ToString+'"'+
    '}'
  );
end;

procedure TMainForm.btnRefreshDIObjectPropertiesClick(Sender: TObject);
begin
  fDataEvent.signalString(
    '{'+
      '"type": "request",'+
      '"method": "readDIObjectProperties",'+
      '"caseId": "'+RemoveComment(cmbCaseId.Text)+'",'+
      '"userId": "'+RemoveComment(cmbUserId.Text)+'",'+
      '"eventId": "'+fConnection.UniqueClientID.ToString+'"'+
    '}'
  );
end;

procedure TMainForm.btnRefreshDIQueriesClick(Sender: TObject);
begin
  fDataEvent.signalString(
    '{'+
      '"type": "request",'+
      '"method": "readDIQueries",'+
      '"caseId": "'+RemoveComment(cmbCaseId.Text)+'",'+
      '"userId": "'+RemoveComment(cmbUserId.Text)+'",'+
      '"eventId": "'+fConnection.UniqueClientID.ToString+'"'+
    '}'
  );
end;

procedure TMainForm.btnRefreshDMQueriesClick(Sender: TObject);
begin
  fDataEvent.signalString(
    '{'+
      '"type": "request",'+
      '"method": "readDMQueries",'+
      '"caseId": "'+RemoveComment(cmbCaseId.Text)+'",'+
      '"userId": "'+RemoveComment(cmbUserId.Text)+'",'+
      '"eventId": "'+fConnection.UniqueClientID.ToString+'"'+
    '}'
  );
end;

procedure TMainForm.HandleIMBConnectionDisconnect(aConnection: TConnection);
begin
  labelConnectionStatus.Font.Color := clBlack;
  labelConnectionStatus.Caption := 'disconnected';
end;

procedure TMainForm.HandleIMBConnectionException(aConnection: TConnection; aException: Exception);
begin
  labelConnectionStatus.Font.Color := clRed;
  labelConnectionStatus.Caption := aException.Message;
end;

procedure TMainForm.HandlePrivateEventOnString(aEventEntry: TEventEntry; const aString: string);
begin
  memoResponse.Text := memoResponse.Text+#13#10+aString;
end;

function TMainForm.RemoveComment(const aID: string): string;
var
  p: Integer;
begin
  p := aID.IndexOf(';');
  if p>=0
  then Result := aID.Substring(0, p).Trim
  else Result := aID.Trim;
end;

end.
