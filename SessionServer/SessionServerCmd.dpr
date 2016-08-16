program SessionServerCmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogFile, LogConsole,
  StdIni,
  imb4,
  CommandQueue,
  TilerControl,
  SessionServerLib, SessionServerDB,
  SessionServerSessions,
  System.SysUtils;


{
function ConsoleCtrlHandler(dwCtrlType: DWORD): Boolean; stdcall;
begin
  Result := False; // execute default handler
  if Assigned(ModelControl) then
  begin
    Log.WriteLn('Received ctrl-c request', llWarning);
    if not CommandLine.TestSwitch(FederationParameterName)
    then ModelControl.DoStopModel;
    ModelControl.SignalModelExit;
    ModelControl.QuitApplicationEvent.SetEvent;
  end;
end;
}

procedure HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llWarning);
end;

{ main }

var
  imbConnection: TConnection;
  sessionModel: TSessionModel;
  tilerFQDN: string;
  ecodistrictModule: TEcodistrictModule;
begin
  ecodistrictModule := nil; // sentinel
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    try
      // todo: change to tls connection
      imbConnection := TSocketConnection.Create('SessionServer');
      try
        imbConnection.onException := HandleException;
        imbConnection.onDisconnect := HandleDisconnect;
        sessionModel := TSessionModel.Create(imbConnection);
        try
          tilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
          Log.WriteLn('Tiler name: '+tilerFQDN);
          // nwb live feed
          //CreateSessionProject(sessionModel, '1234'{'rotterdam'}, 'Rotterdam dashboard', ptNWBLiveFeed, tilerFQDN, ''); {todo: NWBLiveFeedProjectName}

          // ecodistrict module
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
            GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerFQDN)));

          // inquire existing session and rebuild internal sessions..
          imbConnection.subscribe(imbConnection.privateEventName, False).OnIntString.Add(
            procedure(event: TEventEntry; aInt: Integer; const aString: string)
            var
              projectEventPrefix: string;
              p: TProject;
            begin
              // find project with client
              for p in sessionModel.Projects do
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
          imbConnection.publish(WS2IMBEventName, False).signalIntString(actionInquire, imbConnection.privateEventName);

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
