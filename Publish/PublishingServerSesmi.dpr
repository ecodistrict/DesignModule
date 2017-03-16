program PublishingServerSesmi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogFile, LogConsole,
  StdIni,
  imb4,
  CommandQueue,
  TilerControl,
  SessionServerLib, SessionServerDB,
  SessionServerSesmi,
  System.SysUtils;

const
  RemoteHostSwitch = 'RemoteHost';
  RemotePortSwitch = 'RemotePort';

  ExpertScenarioSwitch = 'ExpertScenario';
  DefaultExpertScenario = '{00000000-0000-0000-0000-000000000000}';

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
  SesmiModule: TSesmiModule;
begin
  SesmiModule := nil; // sentinel
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    try
      // todo: change to tls connection
      imbConnection := TSocketConnection.Create(
        'PublishingServerSesmi', 12,
        'ensel2',
        GetSetting(RemoteHostSwitch, imbDefaultRemoteHost), GetSetting(RemotePortSwitch, imbDefaultRemoteSocketPort));
      try
        imbConnection.onException := HandleException;
        imbConnection.onDisconnect := HandleDisconnect;
        sessionModel := TSessionModel.Create(imbConnection);
        try
          tilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
          Log.WriteLn('Tiler name: '+tilerFQDN);
          // nwb live feed
//          CreateSessionProject(sessionModel, '1234'{'rotterdam'}, 'Rotterdam dashboard', ptNWBLiveFeed, tilerFQDN, '', '',
//            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters)); {todo: NWBLiveFeedProjectName}

          // Sesmi module
          SesmiModule := TSesmiModule.Create(
            sessionModel,
            imbConnection,
            tilerFQDN,
            GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerFQDN)),
            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters), TGUID.Create(GetSetting(ExpertScenarioSwitch, DefaultExpertScenario)));



          // inquire existing session and rebuild internal sessions..
          imbConnection.subscribe(imbConnection.privateEventName, False).OnIntString.Add(
            procedure(event: TEventEntry; aInt: Integer; const aString: string)
            var
              projectEventPrefix: string;
              p: TProject;
            begin
              // find project with client
              for p in SesmiModule.Projects.Values do
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
          SesmiModule.Free;
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
