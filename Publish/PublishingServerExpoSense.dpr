program PublishingServerExpoSense;

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
  PublishServerExpoSense,
  System.SysUtils;

const
  RemoteHostSwitch = 'RemoteHost';
  RemotePortSwitch = 'RemotePort';

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
  project: TProject;
begin
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    try
      imbConnection := TSocketConnection.Create(
        'PublishingServerExponSense V1', 13,
        'exposense',
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

          // module
          project := TExpoSenseProject.Create(
            sessionModel, imbConnection, 'ExpoSense'{$IFDEF DEBUG}+'TestHans'{$ENDIF}, 'ExpoSense project',
            tilerFQDN, GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerFQDN)),
            False, GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters),
            //TMapView.Create(52.184457864, 4.7378540039, 10)
            TMapView.Create(52.336283087, 4.8837661743, 13) // demo
            );
          sessionModel.Projects.Add(project);

          // main loop
          WriteLn('Press return to quit');
          ReadLn;

        finally
          FinalizeCommandQueue();
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
