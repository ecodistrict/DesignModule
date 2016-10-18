program PublishSSMCmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger,
  LogFile,
  LogConsole,
  StdIni,
  imb4,

  CommandQueue,
  TilerControl,

  SessionServerLib,
  SessionServerGIS,
  SessionServerSSM,
  System.SysUtils;

procedure HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llWarning);
end;


var
  imbConnection: TConnection;
  sessionModel: TSessionModel;
  tilerFQDN: string;

  mapView: TMapView;
  //piSSM: Integer;

  { SSM/US
  piUSSSM: Integer;
  usDBConnection: TCustomConnection;
  scenario: TScenario;
  layer: TLayer;
  }
begin
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    // todo: change to tls connection

    imbConnection := TSocketConnection.Create('PublishSSM',98);//,'OTS_RT',GetSetting('RemoteHost', ''),GetSetting('RemotePort', '4004').ToInteger());
    try
      imbConnection.onException := HandleException;
      imbConnection.onDisconnect := HandleDisconnect;
      sessionModel := TSessionModel.Create(imbConnection);
      try
        tilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
        Log.WriteLn('Tiler name: '+tilerFQDN);
        // ssm module
        // mapView := TMapView.Create(52.08457, 4.88909, 14); // woerden
        // mapView := TMapView.Create(52.08606, 5.17689, 11); // loenen a/d vecht?
        mapView := TMapView.Create(52.313939, 4.683429, 14); //  N201 bij schiphol

        {piSSM := }
        sessionModel.Projects.Add(
          TSSMProject.Create(sessionModel, imbConnection, 'SSM', 'SSM', '', '', nil, 0, false, false, false, True, false, mapView, 0));


        { ssm with us
        usDBConnection := ConnectToUSProject('us_schiedam_2016/us_schiedam_2016@app-usdata01.tsn.tno.nl/uspsde', 'us_schiedam_2016', mapView);
        mapView := getUSMapView(usDBConnection as TOraSession, );
        mapView := getUSMapView(usDBConnection as TOraSession, );

        piUSSSM := sessionModel.Projects.Add(
          TUSProject.Create(
            sessionModel,
            imbConnection,
            'USSSM',
            'USSSM',
            tilerFQDN,
            GetSetting(TilerStatusURLSwitch, TilerStatusURLFromTilerName(tilerFQDN)),
            usDBConnection,
            mapView,
            False,
            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters)));

        // add US layer to ssm
        for scenario in (sessionModel.Projects[piUSSSM]as TSSMProject).scenarios.Values do
        begin
          for layer in scenario.Layers.Values do
          begin
            sessionModel.Projects[piSSM].scenarios.Values.ToArray[0].Layers.Add(layer.ID, layer);
          end;
        end;
        }

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
        sessionModel.Free;
      end;
    finally
      imbConnection.Free;
    end;
  except
    on E: Exception do
      Log.Writeln(E);
  end;
end.
