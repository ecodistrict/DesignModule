unit SessionServerSvcMain;

interface

uses
  Logger, LogFile,
  StdIni,
  imb4,
  CommandQueue,
  SessionServerLib, SessionServerDB,
  SessionServerSessions,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

type
  TSessionServer = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    fIMBConnection: TConnection;
    fSessionModel: TSessionModel;
    fTilerEventName: string;
    procedure HandleDisconnect(aConnection: TConnection);
    procedure HandleException(aConnection: TConnection; aException: Exception);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  SessionServer: TSessionServer;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SessionServer.Controller(CtrlCode);
end;

function TSessionServer.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSessionServer.HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure TSessionServer.HandleDisconnect(aConnection: TConnection);
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



procedure TSessionServer.ServiceCreate(Sender: TObject);
var
  curdir: string;
begin
  Log.WriteLn('Session server create');
  curdir := ExtractFileDir(ParamStr(0));
  SetCurrentDirectory(PChar(curdir));
  Log.WriteLn('set current directory to '+curdir);
  Log.WriteLn('path='+GetEnvVarValue('PATH'));
end;

procedure TSessionServer.ServiceDestroy(Sender: TObject);
begin
  Log.WriteLn('Session server destroy');
end;

procedure TSessionServer.ServiceShutdown(Sender: TService);
begin
  Log.WriteLn('Session server shutdown');
end;

procedure TSessionServer.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  Log.WriteLn('Session server prepare start');
  // todo: change to tls connection
  fIMBConnection := TSocketConnection.Create('SessionServer');
  fIMBConnection.onException := HandleException;
  fIMBConnection.onDisconnect := HandleDisconnect;
  Log.WriteLn('Session server started imb4');
  fSessionModel := TSessionModel.Create(fIMBConnection);
  Log.WriteLn('Session server started session model');
  fTilerEventName := GetSetting(TilerEventNameSwitch, DefaultTilerEventName);
  try
    // default us
    //CreateSessionProject(fSessionModel, 'us_schiedam_2016', 'Schiedam (2016)', ptUrbanStrategyOracle, fTilerEventName, 'us_schiedam_2016/us_schiedam_2016@app-usdata01.tsn.tno.nl/uspsde');
    //CreateSessionProject(sessionModel, 'us_ws_hc', ptUrbanStrategyOracle, tilerEventName, 'us_ws_hc/us_ws_hc@app-usdata01.tsn.tno.nl/uspsde');

    // default ecodistrict
    CreateSessionProject(fSessionModel, 'ecodistrict', 'Ecodistrict - Valencia', ptEcoDistrict, fTilerEventName,
      StandardIni.ReadString('projects', 'ecodistrict', 'User_Name=ecodistrict;Password=HyDhCpStZQEYrHuagz79;Server=vps17642.public.cloudvps.com;Port=5443;Database=ecodistrict;PGAdvanced=sslmode=require'));
    Log.WriteLn('Session server started ecodistrict project');

    // nwb live feed
    CreateSessionProject(fSessionModel, 'rotterdam', 'Rotterdam dashboard', ptNWBLiveFeed, fTilerEventName, ''); {todo: NWBLiveFeedProjectName}
    Log.WriteLn('Session server started rotterdam dashboard project');

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
        for p in fSessionModel.Projects do
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

    Log.WriteLn('Session server start');
    started := True;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception starting service: '+E.Message, llError);
      started := False;
    end;
  end;
end;

procedure TSessionServer.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FinalizeCommandQueue();
  FreeAndNil(fSessionModel);
  FreeAndNil(fIMBConnection);
  if PGInited then
  begin
    ExitPG;
    PGInited := False;
  end;
  Log.WriteLn('Session server stop');
  stopped := True;
end;

end.
