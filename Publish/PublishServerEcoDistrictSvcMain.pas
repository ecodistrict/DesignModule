unit PublishServerEcoDistrictSvcMain;

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

const
  IMB4RemoteHostSwitch = 'IMB4RemoteHost';
  IMB4RemotePortSwitch = 'IMB4RemotePort';
  IMB4RemotePrefix = 'IMB4RemotePrefix';
    DefaultEcodistrictPrefix = 'ecodistrict';


type
  TPublishingServerEco = class(TService)
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
  PublishingServerEco: TPublishingServerEco;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  PublishingServerEco.Controller(CtrlCode);
end;

function TPublishingServerEco.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TPublishingServerEco.HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure TPublishingServerEco.HandleDisconnect(aConnection: TConnection);
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



procedure TPublishingServerEco.ServiceCreate(Sender: TObject);
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

procedure TPublishingServerEco.ServiceDestroy(Sender: TObject);
begin
  Log.WriteLn('Publishing server destroy');
end;

procedure TPublishingServerEco.ServiceShutdown(Sender: TService);
begin
  Log.WriteLn('Publishing server shutdown');
end;

{
var
  imbConnection: TConnection;
  sessionModel: TSessionModel;
  tilerFQDN: string;
  ecodistrictModule: TEcodistrictModule;
  mapView: TMapView;
begin
  ecodistrictModule := nil; // sentinel
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    try
      // todo: change to tls connection

      imbConnection := TSocketConnection.Create('PublishServerEco', 99,
        GetSetting(IMB4RemotePrefix, DefaultEcodistrictPrefix),
        GetSetting(IMB4RemoteHostSwitch, imbDefaultRemoteHost),
        GetSetting(IMB4RemotePortSwitch, imbDefaultRemoteSocketPort.ToString).ToInteger());
      try
        imbConnection.onException := HandleException;
        imbConnection.onDisconnect := HandleDisconnect;
        sessionModel := TSessionModel.Create(imbConnection);
        try
          tilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
          Log.WriteLn('Tiler name: '+tilerFQDN);
          // ecodistrict module, specific for test project 1234 and no actions on ecodistrict events
          ecodistrictModule := TEcodistrictModule.Create(
            sessionModel,
            imbConnection,
            'User_Name='+GetSetting('EcoDBUserName', '')+';'+
            'Password='+GetSetting('EcoDBPassword', '')+';'+
            'Server='+GetSetting('EcoDBServer', '')+';'+
            'Port='+GetSetting('EcoDBPort', '5432')+';'+
            'Database='+GetSetting('EcoDBDatabase', 'Warsaw')+';'+
            'PGAdvanced=sslmode=require',
            tilerFQDN,
            GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerFQDN)),
            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters),
            True, True, True); // do not load any other projects or execute ecodistrict commands..

          // default load for testing
          mapView := TMapView.Create(51.19002, 4.37689, 15); // Kiel (Antwerp)
          //ecodistrictModule.HandleModuleCase('1234', 'Kiel test case', 'Test case for Ecodistrict publishing server based on Kiel (Antwerp) data..', mapView, nil);
          ecodistrictModule.HandleModuleCase('5b7fc843820bed04cf284953', 'test case', 'Test case for Ecodistrict publishing server..', mapView, nil);


          // main loop
          WriteLn('Press return to quit');
          ReadLn;

        finally
          FinalizeCommandQueue();
          ecodistrictModule.Free;
          sessionModel.Free;
        end;
      finally
        imbConnection.Free;
      end;
    finally
      ExitPG;
    end;
  except
    on E: Exception do
      Log.Writeln(E);
  end;
end.
}

procedure TPublishingServerEco.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  Log.WriteLn('Publishing server prepare start');
  // todo: change to tls connection
  fIMBConnection := TSocketConnection.Create(Name, 0,
    GetSetting(IMB4RemotePrefix, DefaultEcodistrictPrefix),
    GetSetting(IMB4RemoteHostSwitch, imbDefaultRemoteHost),
    GetSetting(IMB4RemotePortSwitch, imbDefaultRemoteSocketPort.ToString).ToInteger());
  fIMBConnection.onException := HandleException;
  fIMBConnection.onDisconnect := HandleDisconnect;
  Log.WriteLn('Publishing server started imb4');
  fPublishingModel := TSessionModel.Create(fIMBConnection);
  Log.WriteLn('Publishing server started publishing model');
  fTilerFQDNName := GetSetting(TilerNameSwitch, DefaultTilerName);
  Log.WriteLn('Tiler name: '+fTilerFQDNName);
  try
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
    started := True;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception starting service: '+E.Message, llError);
      started := False;
    end;
  end;
end;

procedure TPublishingServerEco.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Log.WriteLn('Publishing server stopping queue');
  FinalizeCommandQueue();
  Log.WriteLn('Publishing server stopping module');
  FreeAndNil(fEcodistrictModule);
  Log.WriteLn('Publishing server stopping model');
  FreeAndNil(fPublishingModel);
  Log.WriteLn('Publishing server stopping connection');
  FreeAndNil(fIMBConnection);
  Log.WriteLn('Publishing server stopping pg');
  ExitPG;
  Log.WriteLn('Publishing server stopped');
  stopped := True;
end;

end.
