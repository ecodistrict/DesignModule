program PublishingServerSesmi;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogFile, LogConsole,
  StdIni,
  imb4,
  CommandQueue,
  TilerControl,
  PublishServerLib,
  PublishServerDB,
  PublishServerSesmi,
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
  //sesmiModule: TSesmiModule;
  project: TProject;
begin
  //sesmiModule := nil; // sentinel
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    try
      // todo: change to tls connection
//      imbConnection := TSocketConnection.Create(
//        'PublishingServerSesmi', 12,
//        'ensel2',
//        GetSetting(RemoteHostSwitch, imbDefaultRemoteHost), GetSetting(RemotePortSwitch, imbDefaultRemoteSocketPort));
//      imbConnection := TSocketConnection.Create(
//        'PublishingServerSesmi', 12,
//        'EnSel2',
//        GetSetting(RemoteHostSwitch, imbDefaultRemoteHost), GetSetting(RemotePortSwitch, imbDefaultRemoteSocketPort));
      imbConnection := TSocketConnection.Create(
        'PublishingServerSesmi', 12,
        'ensel_utrecht',
        GetSetting(RemoteHostSwitch, imbDefaultRemoteHost), GetSetting(RemotePortSwitch, imbDefaultRemoteSocketPort));
      try
        imbConnection.onException := HandleException;
        imbConnection.onDisconnect := HandleDisconnect;
        sessionModel := TSessionModel.Create(imbConnection);
        try
          tilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
          Log.WriteLn('Tiler: '+tilerFQDN);
          // nwb live feed
//          CreateSessionProject(sessionModel, '1234'{'rotterdam'}, 'Rotterdam dashboard', ptNWBLiveFeed, tilerFQDN, '', '',
//            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters)); {todo: NWBLiveFeedProjectName}

          // Sesmi module
          project := TSesmiProject.Create(
            sessionModel, imbConnection, 'Sesmi', 'Fietsproject',
            tilerFQDN, GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerFQDN)),
            False, GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters),
            //TMapView.Create(52.0915, 5.12013, 13),
            TMapView.Create(51.441703756941806, 5.499172210693359, 12),
            TGUID.Create(GetSetting(ExpertScenarioSwitch, DefaultExpertScenario)));
          sessionModel.Projects.Add(project);

          // main loop
          WriteLn('Press return to quit');
          ReadLn;

        finally
          FinalizeCommandQueue();
          //sesmiModule.Free;
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
