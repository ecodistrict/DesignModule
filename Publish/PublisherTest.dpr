program PublisherTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  StdIni,
  Logger,
  LogConsole,
  LogFile,
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
  scenario: TScenario;
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

        // create first scenario
        scenario := TScenario.Create(project, 'scenario1', 'Scenario 1', 'The first scenario', true, project.mapView);
        project.scenarios.Add(scenario.id, scenario);
        // set first scenario as current
        project.projectCurrentScenario := scenario;


        // add 2 kpi's
        with scenario.AddKPI(TKPI.Create(scenario, 'NWB', 'kpi1', 'kpi1', 'a traffic kpi', false)) do
        begin
          title := 'kpi1';
          subtitle := '-';
          ranges := [150, 225, 300];
          measures := [220, 270];
          markers := [250];
        end;

        with scenario.AddKPI(TKPI.Create(scenario, 'NWB', 'kpi2', 'kpi2', 'a traffic kpi', false)) do
        begin
          title := 'kpi2';
          subtitle := '-';
          ranges := [150, 225, 300];
          measures := [200, 280];
          markers := [260];
        end;

        // add example graphs
        // lines chart
        with scenario.AddChart(TChartLines.Create(
          scenario, 'NWB', 'chart1', 'lines chart example', 'this is chart 1',
          False, 'line',
          TChartAxis.Create('tijd', 'lightBlue', 'Time', 'min'),
          [TChartAxis.Create('concentratie', 'lightBlue', 'Concentration', 'mg/m3')], 'time')) do
        begin

        end;

        with scenario.AddChart(TTableChart.Create(
          scenario, 'NWB', 'chart2', 'table chart example', 'this is chart 2',
          False, 'table')) do
        begin

        end;



        // create second scenario
        scenario := TScenario.Create(project, 'scenario2', 'Scenario 2', 'The second scenario', true, project.mapView);
        project.scenarios.Add(scenario.id, scenario);


        project.EnableControl(selectControl);
        project.EnableControl(measuresControl);
        project.EnableControl(measuresHistoryControl);
        project.EnableControl(modelControl);
        project.EnableControl(controlsControl);
        project.EnableControl(overviewControl);
        project.EnableControl(presenterViewerControl);
        project.EnableControl(filesControl);

        project.Control['timeslider'] := '1';
        project.windControl;



        (*
        aClient.signalString(
      '{"type":"rangeslider",'+
       '"payload":'+
         '{"range":{'+
           '"min":"-100",'+
           '"max":"0"'+
           '},'+
          '"orientation": "vertical",'+ // Height is vertical
          '"value": "-1",'+
          '"text": "1 m",' +
          '"updateDelta": "10",' +
          '"colorInversed": "true",' +
          '"ID": "heightSlider"' +
         '}'+
        '}');
        *)

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
