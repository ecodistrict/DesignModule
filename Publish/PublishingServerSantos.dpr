program PublishingServerSantos;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  MyOraLib,
  DB,
  Ora,
  OraSmart,
  OraObjects,
  CmdLin,
  StdIni,
  MyStr,
  MyConsole,
  IMB3NativeClient,
  IMB3Core,
  ByteBuffers,
  Logger,
  LogConsole,
  LogFile,
  LogIMB,
  imb4,
  CommandQueue,
  TimerPool,
  TilerControl,
  PublishServerLib,
  PublishServerDB,
  PublishServerUS,
  PublishServerSantos,
  ModelControllerLib,
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  WinApi.Windows;

const
  ConnectStringSwitch = 'ConnectString';
    DefaultConnectString = '';

  ProjectIDSwitch = 'ProjectID';
  ProjectNameSwitch = 'ProjectName';
  SourceEPSGIntSwitch = 'SourceEPSGInt';

  PreLoadScenariosSwitch = 'PreLoadScenarios';

  WebClientURISwitch = 'WebClientURI';
    DefaultWebClientURI = 'HTTP://vps17642.public.cloudvps.com';

  IMB4RemoteHostSwitch = 'IMB4RemoteHost';
  IMB4RemotePortSwitch = 'IMB4RemotePort';

  BaseScenarioIDSwitch = 'BaseScenario';
    DefaultBaseScenarioID = '1';

type
  TModel = class(TMCModelStarter2)
  constructor Create();
  destructor Destroy; override;
  public
  // standard overrides
    procedure ParameterRequest(aParameters: TModelParameters); override;
    procedure StartModel(aParameters: TModelParameters); override;
    procedure StopModel; override;
  // manual start
    function CheckManualStart: Boolean;
  private
    fSessionModel: TSessionModel;
    fIMBConnection: TConnection; // imb connection to websocket etc.
    fIMBLogger: TIMBLogger;
    fSantosProject: TProject; // ref
  protected
    procedure HandleException(aConnection: TConnection; aException: Exception);
    procedure HandleDisconnect(aConnection: TConnection);
    procedure ProgressTimerTick(aTimer: TTimer; aTime: THighResTicks);
  public
    property sessionModel: TSessionModel read fSessionModel;
    property imbConnection: TConnection read fIMBConnection;

    procedure TestConnection();
  end;

var
  Model: TModel;

{ TModel }

constructor TModel.Create;
begin
  inherited Create('PublishingServerSantos');
  fIMBLogger := nil;
  fSantosProject := nil;
  // imb4
  fIMBConnection := TSocketConnection.Create('PublishingServerSantos', 2, 'nl.imb', GetSetting(IMB4RemoteHostSwitch, imb4.imbDefaultRemoteHost), GetSetting(IMB4RemotePortSwitch, imb4.imbDefaultRemoteSocketPort));
  fIMBConnection.onException := HandleException;
  fIMBConnection.onDisconnect := HandleDisconnect;
  fIMBConnection.setHeartbeat(60*1000);
  fSessionModel := TSessionModel.Create(fIMBConnection);
end;

destructor TModel.Destroy;
begin
  fSantosProject := nil;
  FreeAndNil(fSessionModel);
  FreeAndNil(fIMBLogger);
  FreeAndNil(fIMBConnection);
  inherited;
end;

procedure TModel.HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llError);
  //Log.WriteLn('FORCING halt', llError);
  //Halt; // todo: force restart in MC mode
  // todo: start reconnect

end;

procedure TModel.HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure TModel.ParameterRequest(aParameters: TModelParameters);
var
  p: Integer;
  projectID: string;
  projectName: string;
  i: Integer;
  dbConnection: TOraSession;
  sourceEPSG: Integer;
begin
  try
    // add parameters with default values
    // DataSourceParameterName and FederationParameterName parameters should be set to
    // enable looking up parameter values in database
    try
      WriteLn('Parameters request');
      if aParameters.Count>0
      then WriteLn('   parameters')
      else WriteLn('## NO parameters defined');
      for p := 0 to aParameters.Count - 1 do
      begin
        WriteLn('      ', aParameters[p].Name, '(', Ord(aParameters[p].ValueType) ,') = ', aParameters[p].Value);
      end;
    except
      on e: Exception do
      begin
        Log.WriteLn('Exception parsing parameters in TModel.ParameterRequest: '+E.Message, llError);
      end;
    end;

    sourceEPSG := GetSetting(SourceEPSGIntSwitch, -1);

    //projectName := GetSetting(ProjectNameSwitch,
    if aParameters.ParameterExists(DataSourceParameterName) then
    begin
      // oracle part
      dbConnection := TOraSession.Create(nil);
      try
        dbConnection.ConnectString := aParameters.ParameterByName[DataSourceParameterName].ValueAsString;
        dbConnection.Open;
        projectID := getUSProjectID(dbConnection, '');
        if projectID=''
        then projectID := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '').Replace('-', '');
        sourceEPSG := getUSSourceESPG(dbConnection, sourceEPSG, projectID);
      finally
        dbConnection.Free;
      end;

      // try to create nice project name..
      projectName := aParameters.ParameterByName[DataSourceParameterName].ValueAsString;
      i := projectName.IndexOf('/');
      if i>0
      then projectName := projectName.Substring(0, i);
      if projectName.ToUpper.StartsWith('US_')
      then projectName := projectName.Substring(length('US_'));
      projectName := projectName.Replace('_', ' ');
      projectName[1] := UpCase(projectName[1]);
    end
    else
    begin
      projectID := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '').Replace('-', '');
      projectName := GetSetting(ProjectNameSwitch, 'E-bus');
    end;
    aParameters.Add(TModelParameter.Create(TilerNameSwitch, GetSetting(TilerNameSwitch, DefaultTilerName)));
    aParameters.Add(TModelParameter.Create(ProjectIDSwitch, projectID));
    aParameters.Add(TModelParameter.Create(ProjectNameSwitch, projectName));
    aParameters.Add(TModelParameter.Create(PreLoadScenariosSwitch, GetSetting(PreLoadScenariosSwitch, True)));
    aParameters.Add(TModelParameter.Create(SourceEPSGIntSwitch, sourceEPSG));
  except
    on E: Exception
    do log.WriteLn('Exception in TModel.ParameterRequest: '+E.Message, llError);
  end;
end;

procedure TModel.ProgressTimerTick(aTimer: TTimer; aTime: THighResTicks);
begin
  // report progress to MC about length of command queue (= shared queue)
  if Assigned(fSantosProject) // if any project is not null report progress
  then SignalModelProgress(CommandQueueLength); //Connection.UpdateStatus();
end;

procedure TModel.StartModel(aParameters: TModelParameters);
var
//  p: Integer;
  dbConnection: TOraSession;
  projectID: string;
  projectName: string;
  mapView: TMapView;
  preLoadScenarios: Boolean;
  tilerName: string;
  sourceEPSG: Integer;
begin
  try
    fIMBLogger := AddIMBLogger(Log, Self.Connection);
    Log.WriteLn('Started');

    dbConnection := TOraSession.Create(nil);
    dbConnection.ConnectString := aParameters.ParameterByName[DataSourceParameterName].ValueAsString;
    dbConnection.Open;

    projectID := aParameters.ParameterByName[ProjectIDSwitch].ValueAsString;
    projectName := aParameters.ParameterByName[ProjectNameSwitch].ValueAsString;
    mapView := getUSMapView(dbConnection as TOraSession, TMapView.Create(52.35264, 4.89544, 13));
    Log.WriteLn('MapView: lat:'+mapView.lat.ToString+' lon:'+mapView.lon.ToString+' zoom:'+mapView.zoom.ToString);
    preLoadScenarios := aParameters.ParameterByName[PreLoadScenariosSwitch].Value;
    tilerName := aParameters.ParameterByName[TilerNameSwitch].ValueAsString;
    setUSProjectID(dbConnection, projectID, mapView.lat, mapView.lon, mapView.zoom); // store project properties in database
    sourceEPSG := aParameters.ParameterByName[SourceEPSGIntSwitch].Value;

    fSantosProject := TSantosProject.Create(fSessionModel, fSessionModel.Connection, connection,
      projectID, projectName,
      tilerName,
      GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerName)),
      aParameters.ParameterByName[DataSourceParameterName].ValueAsString,
      dbConnection,
      mapView,
      preLoadScenarios,
      GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters),
      GetSetting(BaseScenarioIDSwitch, DefaultBaseScenarioID),
      sourceEPSG);
    fSantosProject.Timers.SetTimer(ProgressTimerTick, hrtNow+DateTimeDelta2HRT(dtOneSecond*5), DateTimeDelta2HRT(dtOneSecond*5));
    fSessionModel.Projects.Add(fSantosProject);

    // for now
    Log.WriteLn('URL: '+GetSetting(WebClientURISwitch, DefaultWebClientURI)+'?session='+projectID, llOK);

    // signal we are busy so we do not get killed (this model is different from the standard model)
    SignalModelState(msBusy);
    SignalModelProgress(0); // 0-100% or count down to zero
  except
    on E: Exception
    do log.WriteLn('Exception in TModel.StartModel: '+E.Message, llError);
  end;
end;

procedure TModel.StopModel;
begin
  try
    fSantosProject := nil;
    FreeAndNil(fIMBLogger);

    fSessionModel.Projects.Clear;

    System.TMonitor.Enter(Log);
    try
      WriteLn('Stop model');
    finally
      System.TMonitor.Exit(Log);
    end;
  except
    on E: Exception
    do log.WriteLn('Exception in TModel.StopModel: '+E.Message, llError);
  end;
end;

procedure TModel.TestConnection;
begin
  fIMBConnection.signalHeartBeat('test');
  System.TMonitor.Enter(Log);
  try
    WriteLn('Send IMB4 heartbeat');
  finally
    System.TMonitor.Exit(Log);
  end;
end;

function TModel.CheckManualStart: Boolean;
var
  Parameters: TModelParameters;
  Session: TOraSession;
  connectString: string;
//  sectionValues: TStringList;
//  i: Integer;
//  st: string;
//  p: TArray<string>;
//  vt: TModelParameterValueType;
begin
  // check if we have started in manual or node controller mode
  {
  if StandardIni.SectionExists(RecoverySection) then
  begin
    Log.WriteLn('Started in recovery mode', llWarning);
    Parameters := TModelParameters.Create;
    try
      sectionValues := TStringList.Create;
      try
        StandardIni.ReadSectionValues(RecoverySection, sectionValues);
        for i := 0 to sectionValues.Count-1 do
        begin
          st := sectionValues.Values[sectionValues.Names[i]];
          p := st.Split([',']);
          vt := TModelParameterValueType(p[0].ToInteger);
          Parameters.Add(TModelParameter.Create(sectionValues.Names[i], st, vt));
        end;
      finally
        sectionValues.Free;
      end;
      // set federation if defined
      if Parameters.ParameterExists(FederationParameterName)
      then Connection.Federation := Parameters.Value[FederationParameterName];
      // default signal busy state
      SignalModelState(msBusy);
      try
        StartModel(Parameters);
      except
        on E: Exception
        do Log.WriteLn('Exception in TModel.CheckManualStart (StartModel): '+E.Message, llError);
      end;
    finally
      Parameters.Free;
    end;
  end
  else }
  Result := not CommandLine.TestSwitch(ControllerSwitch);
  if Result then
  begin
    Log.WriteLn('Started in manual mode', llOk);
    Parameters := TModelParameters.Create;
    try
      // handle parameter defaults  retrieval from database, use connectstring from ini file
      if GetSettingExists(ConnectStringSwitch) then
      begin
        Session := TOraSession.Create(nil);
        try
          connectString := GetSetting(ConnectStringSwitch, DefaultConnectString);
          Session.ConnectString := connectString;
          Session.Open;
          Parameters.Add(TModelParameter.Create(DataSourceParameterName, connectString));
          Parameters.Add(TModelParameter.Create(FederationParameterName, GetScenarioFederation(Session ,GetCurrentScenarioID(Session))));
        finally
          Session.Free;
        end;
      end
      else Log.WriteLn('No connect string defined (and cannot set federation)', llWarning);
      // add model specific parameters:
      ParameterRequest(Parameters);
      // set federation if defined
      if Parameters.ParameterExists(FederationParameterName)
      then Connection.Federation := Parameters.Value[FederationParameterName];
      // default signal busy state
      SignalModelState(msBusy);
      try
        StartModel(Parameters);
      except
        on E: Exception
        do Log.WriteLn('Exception in TModel.CheckManualStart (StartModel): '+E.Message, llError);
      end;
    finally
      Parameters.Free;
    end;
  end
  else
  begin
    Log.WriteLn('Started in modelController mode', llOk);
  end;
end;



function ConsoleCtrlHandler(dwCtrlType: DWORD): Boolean; stdcall;
begin
  Result := False; // execute default handler
  if Assigned(Model) then
  begin
    System.TMonitor.Enter(Log);
    try
      case dwCtrlType of
        CTRL_CLOSE_EVENT:
          begin
            Model.QuitApplication;
            WriteLn('Sending model quit on close event');
          end;
        CTRL_LOGOFF_EVENT:
          begin
            Model.QuitApplication;
            WriteLn('Sending model exit on logoff event');
          end;
        CTRL_SHUTDOWN_EVENT:
          begin
            Model.QuitApplication;
            WriteLn('Sending model exit on shutdown event');
          end;
        CTRL_C_EVENT:
          begin
            Model.QuitApplication;
            WriteLn('Sending model exit on ctrl-c event');
          end;
        CTRL_BREAK_EVENT:
          begin
            Model.QuitApplication;
            WriteLn('Sending model exit on ctrl-break event');
          end;
      end;
    finally
      System.TMonitor.Exit(Log);
    end;
  end;
end;

procedure ShowMenu;
begin
  System.TMonitor.Enter(Log);
  try
    WriteLn('Options');
    WriteLn('   ? for help');
    WriteLn('   Q or escape to quit');
    WriteLn('   R send refresh');
    WriteLn('   P send preview');
    WriteLn('   C clients');
    WriteLn('   X test connection');
    WriteLn;
  finally
    System.TMonitor.Exit(Log);
  end;
end;

var
  key: Char;
  project: TProject;
  client: TClient;
begin
  try
    // info on machine we run on
    WriteLn('CPU count: ', System.CPUCount);
    WriteLn('IsMultiThread: ', System.IsMultiThread);
    {$IF defined(CPUX86) or defined(CPUX64)}
    WriteLn('Test8086: ', System.Test8086);
    WriteLn('Test8087: ', System.Test8087);
    WriteLn('TestFDIV: ', System.TestFDIV);
    WriteLn('TestSSE: ', System.TestSSE);
    {$ENDIF defined(CPUX86) or defined(CPUX64)}

    FileLogger.SetLogDef(AllLogLevels, [llsTime]);

    InitializeCommandQueue(20000, 64);

    // start the model
    Model := TModel.Create;
    try
      if Model.Connection.Connected then
      begin
        // make sure that if console window is closed or ctrl-break or ctrl-c is pressed we exit nicely
        SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
        try
          // check if we are started directly or via node controller
          if Model.CheckManualStart then
          begin
            ShowMenu;
            // wait until we received a quit signal
            while Model.QuitApplicationEvent.WaitFor(200)=TWaitResult.wrTimeout do
            begin
              // extra mainloop actions..
              if CheckKeyPressed2 then
              begin
                key := KeyPressed2;
                System.TMonitor.Enter(Log);
                try
                  case key of
                    '?':
                      ShowMenu;
                    'Q', 'q', #27:
                      Model.QuitApplication;
                    'C', 'c':
                      begin
                        for project in Model.sessionModel.Projects do
                        begin
                          WriteLn('Project: '+project.ProjectName);
                          TMonitor.Enter(project.clients);
                          try
                            for client in project.clients do
                            begin
                              WriteLn('   '+client.clientID);
                            end;
                          finally
                            TMonitor.Exit(project.clients);
                          end;
                        end;
                      end;
                    'X', 'x':
                      begin
                        Model.TestConnection();
                      end;
                  end;
                finally
                  System.TMonitor.Exit(Log);
                end;
              end;
            end;
          end
          else
          begin
            Model.QuitApplicationEvent.WaitFor(); // just wait for quit
          end;
        finally
          SetConsoleCtrlHandler(@ConsoleCtrlHandler, False);
        end;
      end
      else Log.WriteLn('Could not connect to IMB framework on '+Model.Connection.RemoteHost+':'+Model.Connection.RemotePort.ToString(), llError);
    finally
      Model.Free;
    end;
  except
    on E: Exception do
      Log.Writeln(E);
  end;
end.
