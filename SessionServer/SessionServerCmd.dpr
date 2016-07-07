program SessionServerCmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogFile, LogConsole,
  StdIni,
  imb4,
  CommandQueue,
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
  tilerEventName: string;
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
          tilerEventName := GetSetting(TilerEventNameSwitch, DefaultTilerEventName);
          Log.WriteLn('Tiler event name: '+tilerEventName);
          // default us
          //CreateSessionProject(sessionModel, '1234' {'us_schiedam_2016'}, 'Schiedam (2016)', ptUrbanStrategyOracle, tilerEventName, 'us_schiedam_2016/us_schiedam_2016@app-usdata01.tsn.tno.nl/uspsde');
          //CreateSessionProject(sessionModel, 'us_ws_hc', ptUrbanStrategyOracle, tilerEventName, 'us_ws_hc/us_ws_hc@app-usdata01.tsn.tno.nl/uspsde');

          // default ecodistrict
          //CreateSessionProject(sessionModel, '1234'{'ecodistrict'}, 'Ecodistrict - Valencia', ptEcoDistrict, tilerEventName, 'User_Name=ecodistrict;Password=HyDhCpStZQEYrHuagz79;Server=vps17642.public.cloudvps.com;Port=5443;Database=ecodistrict;PGAdvanced=sslmode=require');
          //CreateSessionProject(sessionModel, '5721de16eae0e0c543f5234f', 'Ecodistrict - Warsaw', ptEcoDistrict, tilerEventName, 'User_Name=postgres;Password=x0mxaJc69J9KAlFNsaDt;Server=vps17642.public.cloudvps.com;Port=5443;Database=Warsaw;PGAdvanced=sslmode=require');

          // nwb live feed
          //CreateSessionProject(sessionModel, '1234'{'rotterdam'}, 'Rotterdam dashboard', ptNWBLiveFeed, tilerEventName, ''); {todo: NWBLiveFeedProjectName}

          // ecodistrict module
          ecodistrictModule := TEcodistrictModule.Create(sessionModel, imbConnection, 'User_Name=postgres;Password=x0mxaJc69J9KAlFNsaDt;Server=vps17642.public.cloudvps.com;Port=5443;Database=Warsaw;PGAdvanced=sslmode=require', tilerEventName);

          // inquire existing session and rebuild internal sessions..
          imbConnection.subscribe(imbConnection.privateEventName, False).OnIntString.Add(
            procedure(event: TEventEntry; aInt: Integer; const aString: string)
            var
              projectEventPrefix: string;
              p: TProject;
            begin
              // find project with client
              // aString = 'USIdle.Sessions.WS2IMB.1234.uuid:ac1ab9e5-4db9-4f03-ae05-b4558b0f0f8c;id=14'
              // project.fProjectEvent.eventname = 'USIdle.Sessions.WS2IMB.1234'
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
