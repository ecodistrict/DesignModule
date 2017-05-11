program PublishServerCmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger,
  LogFile,
  LogConsole,
  StdIni,
  imb4,

  Ora,
  Data.DB,

  CommandQueue,
  TilerControl,

  NDWLib,

  PublishServerLib,
  PublishServerDB,
  PublishServerGIS,
  PublishServerUS,
  //PublishServerEnSel,
  //PublishServerNWBLive,
  PublishServerEcodistrict,
  PublishServerSSM,
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
  mapView: TMapView;

  { SSM

  piSSM: Integer;
  {}

  { SSM/US
  piUSSSM: Integer;
  usDBConnection: TCustomConnection;
  scenario: TScenario;
  layer: TLayer;
  }
begin
  ecodistrictModule := nil; // sentinel
  FileLogger.SetLogDef(AllLogLevels, [llsTime]);
  try
    try
      // todo: change to tls connection

      imbConnection := TSocketConnection.Create('SessionServer',99);//,'OTS_RT',GetSetting('RemoteHost', ''),GetSetting('RemotePort', '4004').ToInteger());
      try
        imbConnection.onException := HandleException;
        imbConnection.onDisconnect := HandleDisconnect;
        sessionModel := TSessionModel.Create(imbConnection);
        try
          tilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
          Log.WriteLn('Tiler name: '+tilerFQDN);
          {
          // nwb live feed
          CreateSessionProject(sessionModel, 'rotterdam', 'Rotterdam dashboard', ptNWBLiveFeed, tilerFQDN, '', '',
            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters)); // todo: NWBLiveFeedProjectName
          }


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

          // default load
          mapView := TMapView.Create(51.19002, 4.37689, 15); // Kiel (Antwerp)
          ecodistrictModule.HandleModuleCase('1234', 'Kiel test case', 'Test case for Ecodistrict publishing server based on Kiel (Antwerp) data..', mapView, nil);



          {
          // ssm module
          // mapView := TMapView.Create(52.08457, 4.88909, 14); // woerden
          // mapView := TMapView.Create(52.08606, 5.17689, 11); // loenen a/d vecht?
          mapView := TMapView.Create(52.313939, 4.683429, 14); //  N201 bij schiphol

          piSSM := sessionModel.Projects.Add(
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

          // every project has own listener for clients -> global list not needed anymore
          {
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
          }
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
