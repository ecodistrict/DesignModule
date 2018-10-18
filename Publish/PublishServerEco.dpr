program PublishServerEco;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger,
  LogFile,
  LogConsole,
  StdIni,
  imb4,
  Data.DB,
  CommandQueue,
  TilerControl,
  PublishServerLib,
  PublishServerDB,
  PublishServerGIS,
  PublishServerEcodistrict,
  System.SysUtils;

const
  IMB4RemoteHostSwitch = 'IMB4RemoteHost';
  IMB4RemotePortSwitch = 'IMB4RemotePort';
  IMB4RemotePrefix = 'IMB4RemotePrefix';
    DefaultEcodistrictPrefix = 'ecodistrict';


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
            GetSetting(MaxNearestObjectDistanceInMetersSwitch, DefaultMaxNearestObjectDistanceInMeters)
            {True, True, True}); // do not load any other projects or execute ecodistrict commands..

          // default load for testing
          mapView := TMapView.Create(51.19002, 4.37689, 15); // Kiel (Antwerp)
          //ecodistrictModule.HandleModuleCase('1234', 'Kiel test case', 'Test case for Ecodistrict publishing server based on Kiel (Antwerp) data..', mapView, nil);
          ecodistrictModule.HandleModuleCase('5b7fc843820bed04cf284953', 'test case', 'Test case for Ecodistrict publishing server..', mapView, nil);

          //mapView := TMapView.Create(39.49756688894252, -0.393362045288086, 15); // Valencia
          //ecodistrictModule.HandleModuleCase('5a6f19f07077d2096a218483', 'blanca case', 'blanca case..', mapView, nil);

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
