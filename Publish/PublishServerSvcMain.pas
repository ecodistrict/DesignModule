unit PublishServerSvcMain;

interface

uses
  Logger, LogFile,
  StdIni,
  imb4,
  CommandQueue,
  TilerControl,
  PublishServerLib, PublishServerDB,
  PublishServerEcodistrict,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

const
  ServiceNameSwitch = 'ServiceName';
  ServiceDisplayNameSwitch = 'ServiceDisplayName';


type
  TPublishingServer = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    fIMBConnection: TConnection;
    fPublishingModel: TSessionModel;
    fTilerFQDNName: string;
    fEcodistrictModule: TEcodistrictModule;
    procedure HandleDisconnect(aConnection: TConnection);
    procedure HandleException(aConnection: TConnection; aException: Exception);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  PublishingServer: TPublishingServer;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  PublishingServer.Controller(CtrlCode);
end;

function TPublishingServer.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TPublishingServer.HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure TPublishingServer.HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llWarning);
end;

function GetEnvVarValue(const aVarName: string): string;
var
  BufSize: Integer;
begin
  BufSize := GetEnvironmentVariable(PChar(aVarName), nil, 0);
  if BufSize > 0 then
  begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(aVarName), PChar(Result), BufSize);
  end
  else Result := '';
end;



procedure TPublishingServer.ServiceCreate(Sender: TObject);
var
  curdir: string;
begin
  Name := GetSetting(ServiceNameSwitch, Name);
  DisplayName := GetSetting(ServiceDisplayNameSwitch, DisplayName);
  Log.WriteLn('Created service "'+DisplayName+'" as '+Name);
  curdir := ExtractFileDir(ParamStr(0));
  SetCurrentDirectory(PChar(curdir));
  Log.WriteLn('set current directory to '+curdir);
  Log.WriteLn('path='+GetEnvVarValue('PATH'));
end;

procedure TPublishingServer.ServiceDestroy(Sender: TObject);
begin
  Log.WriteLn('Publishing server destroy');
end;

procedure TPublishingServer.ServiceShutdown(Sender: TService);
begin
  Log.WriteLn('Publishing server shutdown');
end;

procedure TPublishingServer.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  Log.WriteLn('Publishing server prepare start');
  // todo: change to tls connection
  fIMBConnection := TSocketConnection.Create(Name);
  fIMBConnection.onException := HandleException;
  fIMBConnection.onDisconnect := HandleDisconnect;
  Log.WriteLn('Publishing server started imb4');
  fPublishingModel := TSessionModel.Create(fIMBConnection);
  Log.WriteLn('Publishing server started publishing model');
  fTilerFQDNName := GetSetting(TilerNameSwitch, DefaultTilerName);
  try
    // nwb live feed
    //CreateSessionProject(fPublishingModel, 'rotterdam', 'Rotterdam dashboard', ptNWBLiveFeed, fTilerEventName, ''); {todo: NWBLiveFeedProjectName}
    //Log.WriteLn('Publishing server started rotterdam dashboard project');

    // ecodistrict module
    fEcodistrictModule := TEcodistrictModule.Create(
      fPublishingModel,
      fIMBConnection,
      'User_Name='+GetSetting('EcoDBUserName', '')+';'+
      'Password='+GetSetting('EcoDBPassword', '')+';'+
      'Server='+GetSetting('EcoDBServer', '')+';'+
      'Port='+GetSetting('EcoDBPort', '')+';'+
      'Database='+GetSetting('EcoDBDatabase', '')+';'+
      'PGAdvanced=sslmode=require',
      fTilerFQDNName,
      GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(fTilerFQDNName)),
      GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters));
    Log.WriteLn('Publishing server started ecodistrict module');
    // every project has own listener for clients -> global list not needed anymore
    {
    // inquire existing session and rebuild internal sessions..
    fIMBConnection.subscribe(fIMBConnection.privateEventName, False).OnIntString.Add(
      procedure(event: TEventEntry; aInt: Integer; const aString: string)
      var
        projectEventPrefix: string;
        p: TProject;
      begin
        // find project with client
        // aString = 'USIdle.Sessions.WS2IMB.1234.uuid:ac1ab9e5-4db9-4f03-ae05-b4558b0f0f8c;id=14'
        // project.fProjectEvent.eventname = 'USIdle.Sessions.WS2IMB.1234'
        for p in fPublishingModel.Projects do
        begin
          projectEventPrefix := p.ProjectEvent.eventName+'.';
          if aString.StartsWith(projectEventPrefix) then
          begin
            p.AddClient(aString);
            Log.WriteLn('linked existing client: '+aString.Substring(projectEventPrefix.Length));
            Log.WriteLn('to project: '+p.ProjectName, llNormal, 1);
            exit;
          end;
        end;
        Log.WriteLn('could not link existing ws2imb client to project: '+aString, llWarning);
      end);

    // inquire existing sessions
    fIMBConnection.publish(WS2IMBEventName, False).signalIntString(actionInquire, fIMBConnection.privateEventName);
    }
    Log.WriteLn('Publishing server start');
    started := True;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception starting service: '+E.Message, llError);
      started := False;
    end;
  end;
end;

procedure TPublishingServer.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FinalizeCommandQueue();
  FreeAndNil(fEcodistrictModule);
  FreeAndNil(fPublishingModel);
  FreeAndNil(fIMBConnection);
  ExitPG;
  Log.WriteLn('Publishing server stop');
  stopped := True;
end;

end.
