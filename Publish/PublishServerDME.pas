unit PublishServerDME;

interface

uses
  Logger,
  StdIni,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  MyOraLib,
  MyStr,

  ODBFiles2,

  WorldDataCode,
  WorldLegends,

  IMB3NativeClient,

  imb4,
  WorldTilerConsts,
  CommandQueue,
  TimerPool,

  NDWLib,

  System.JSON,
  System.SysUtils,

  PublishServerLib,
  PublishServerGIS,
  PublishServerUS;

const
  //hardcoded bounding box zones for the Park & Shuttle measure
  xMin = '119000';
  xMax = '120600';
  yMin = '482500';
  yMax = '484200';
  targetZone = '719';

type
  TUSDesignProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string);
  destructor Destroy; override;
  private
    fUSIMBConnection: TIMBConnection;
  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  end;

  TUSMonitorProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string{; aSourceEPSG: Integer});
  destructor Destroy; override;
  public
    procedure ReadBasicData(); override;
  end;

  TUSEvaluateProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string{; aSourceEPSG: Integer});
  destructor Destroy; override;
  public
    procedure ReadBasicData(); override;
  end;

implementation

{ TUSDesignProject }

constructor TUSDesignProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection, aMapView, aPreLoadScenarios, aMaxNearestObjectDistanceInMeters);
  fProjectCurrentScenario := ReadScenario(aStartScenario);
  fUSIMBConnection := TIMBConnection.Create(
      GetSetting('IMB3RemoteHost', 'vps17642.public.cloudvps.com'),
      GetSetting('IMB3RemotePort', 4000),
      'PublisherDME-Design', 21, '');
  EnableControl(selectControl);
  EnableControl(measuresControl);
  EnableControl(measuresHistoryControl);
end;

destructor TUSDesignProject.Destroy;
begin
  inherited;
end;

procedure TUSDesignProject.handleClientMessage(aClient: TClient;
  aScenario: TScenario; aJSONObject: TJSONObject);
var
  oraSession: TOraSession;
  measure: TMeasureAction;
  jsonMeasures: TJSONArray;
  jsonMeasure, jsonAction: TJSONValue;
  id: string;
  measureFactor: Double;
  factorString, inverseString: string;
  queryText1, queryText2, table1, table2: string;
  publishEventName: string;
  publishEvent: TIMBEventEntry;
begin
  if Assigned(aScenario) and (aScenario is TUSScenario) then
  begin
    if aJSONObject.TryGetValue<TJSONArray>('applyMeasures', jsonMeasures) then
      for jsonMeasure in jsonMeasures do
        if jsonMeasure.TryGetValue<TJSONValue>('measure', jsonAction) then
          if jsonAction.TryGetValue<string>('id', id) then
            if FindMeasure(id, measure) then
            begin
              if measure.actionID < 0 then
              begin
                if measure.actionID >= -3 then
                begin
                  measureFactor := 1;
                  case measure.actionID of
                    -1: measureFactor := 0.98; //2%
                    -2: measureFactor := 0.95; //5%
                    -3: measureFactor := 0.90; //10%
                  end;

                  table1 := (aScenario as TUSScenario).Tableprefix + 'TRAF_OD';
                  table2 := (aScenario as TUSScenario).Tableprefix + 'TRAF_ZONE';
                  factorString := (FloatToStr(measureFactor)).Replace(',', '.');
                  inverseString := (FloatToStr(1 - measureFactor)).Replace(',', '.');

                  queryText1 := 'update ' + table1 + ' t1' +
                    ' set' +
                    ' t1.CAR_TRIPS = t1.CAR_TRIPS +' +
                    ' (' + inverseString + ' * (select coalesce (sum(CAR_TRIPS), 0) FROM' +
                    ' (select FROM_ZONE, TO_ZONE, CAR_TRIPS, X_CENTROID, Y_CENTROID FROM (' + table1 + ' t_od inner join ' + table2 + ' t_zone on t_od.TO_ZONE = t_zone.OBJECT_ID)' +
                    ' where t_od.FROM_ZONE = t1.FROM_ZONE and t_od.TO_ZONE <> '+ targetZone + ') t2' +
                    ' where (t2.X_CENTROID between ' + xMin + ' and ' + xMax + ') and (t2.Y_CENTROID between ' + yMin + ' and ' + yMax + ')' +
                    '))' +
                    ' where' +
                    ' t1.TO_ZONE = ' + targetZone;

                  queryText2 := 'update ' + table1 + ' t1' +
                    ' set' +
                    ' t1.CAR_TRIPS = t1.CAR_TRIPS * ' + factorString +
                    ' where' +
                    ' t1.FROM_ZONE <> ' + targetZone + ' and t1.TO_ZONE <> ' + targetZone + 'and t1.TO_ZONE in' +
                    ' (select TO_ZONE from ' + table1 + ' t_od inner join ' + table2 + ' t_zone on t_od.TO_ZONE = t_zone.OBJECT_ID' +
                    ' where' +
                    ' t_zone.X_CENTROID between ' + xMin + ' and ' + xMax + ' and t_zone.Y_CENTROID between ' + yMin + ' and ' + yMax + ')';

                  oraSession := fDBConnection as TOraSession;
                  oraSession.StartTransaction;
                  try
                    oraSession.ExecSQL(queryText1);
                    oraSession.ExecSQL(queryText2);
                    oraSession.Commit;

                    publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.TRAF_OD';
                    publishEvent := fUSIMBConnection.publish(publishEventName, false);
                    publishEvent.SignalChangeObject(actionChange, 0, 'CAR_TRIPS'); //todo: send object id = 0 in case of everything?
                    publishEvent.UnPublish;
                  except
                    oraSession.Rollback;
                    //todo: logging transaction failed
                  end;
                end
                else
                begin

                end;
              end
            end;
  end;
end;

procedure TUSDesignProject.ReadBasicData;
var
  scenarioID: Integer;
  s: string;
begin
  ReadScenarios;
  ReadMeasures;
  // load current scenario and ref scenario first
  scenarioID := getUSCurrentPublishedScenarioID(OraSession, GetCurrentScenarioID(OraSession));
  fProjectCurrentScenario := ReadScenario(scenarioID.ToString);
  Log.WriteLn('current US scenario: '+fProjectCurrentScenario.ID+' ('+(fProjectCurrentScenario as TUSScenario).Tableprefix+'): "'+fProjectCurrentScenario.description+'"', llOk);
  // ref
  scenarioID := GetScenarioBaseID(OraSession, scenarioID);
  if scenarioID>=0 then
  begin
    fProjectRefScenario := ReadScenario(scenarioID.ToString);
    Log.WriteLn('reference US scenario: '+fProjectRefScenario.ID+' ('+(fProjectRefScenario as TUSScenario).Tableprefix+'): "'+fProjectRefScenario.description+'"', llOk);
  end
  else Log.WriteLn('NO reference US scenario', llWarning);
  if PreLoadScenarios then
  begin
    for s in USDBScenarios.Keys do
    begin
      if USDBScenarios[s]._published=1
        then ReadScenario(s);
    end;
  end;
end;

{ TUSMonitorProject }

constructor TUSMonitorProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aStartScenario: string);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection, aMapView, aPreLoadScenarios, aMaxNearestObjectDistanceInMeters);
  DisableControl(selectControl);
  DisableControl(measuresControl);
  DisableControl(measuresHistoryControl);
  fProjectCurrentScenario := ReadScenario(aStartScenario);
end;

destructor TUSMonitorProject.Destroy;
begin

  inherited;
end;

procedure TUSMonitorProject.ReadBasicData;
begin

end;

{ TUSEvaluateProject }

constructor TUSEvaluateProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aStartScenario: string);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection, aMapView, aPreLoadScenarios, aMaxNearestObjectDistanceInMeters);
  DisableControl(selectControl);
  DisableControl(measuresControl);
  DisableControl(measuresHistoryControl);
  fProjectCurrentScenario := ReadScenario(aStartScenario);
end;

destructor TUSEvaluateProject.Destroy;
begin

  inherited;
end;

procedure TUSEvaluateProject.ReadBasicData;
begin
end;

end.
