program PublisherTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  StdIni,
  Logger, LogConsole, LogFile,
  imb4,
  PublishServerLib,
  TilerControl,
  System.SysUtils;

var
  connection: TConnection;
  sessionModel: TSessionModel;
  mapView: TMapView;
  tilerName: string;
  project: TProject;
begin
  try
    connection := TSocketConnection.Create('PublisherTest', 0, '', 'vps17642.public.cloudvps.com');
    try
      sessionModel := TSessionModel.Create(connection);
      try
        mapView := TMapView.Create(39.4102356979512, -7.410707473754884, 14);
        tilerName := GetSetting(TilerNameSwitch, DefaultTilerName);
        project := TProject.Create(sessionModel, connection, 'test', 'test project',
          tilerName, TilerStatusURLFromTilerName(tilerName),
          nil, true, 150, mapView);
        sessionModel.Projects.Add(project);



        WriteLn('Press return to quit..');
        ReadLn;
      finally
        sessionModel.Free;
      end;
    finally
      connection.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
