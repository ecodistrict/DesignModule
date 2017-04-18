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

  System.JSON,
  System.SysUtils,

  PublishServerLib,
  PublishServerGIS,
  PublishServerUS;

const
  //hardcoded bounding box zones for the Park & Shuttle measure
  xMin = '119000';
  xMax = '120600';
  yMin = '484200';
  yMax = '482500';
  targetZone = '719';

type
  TUSDesignProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer{; aSourceEPSG: Integer});
  destructor Destroy; override;
  private
    odEvent: TIMBEventEntry;
  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  end;

  TUSMonitorProject = class(TUSProject)

  end;

  TUSEvaluateProject = class(TUSProject)

  end;

implementation

{ TUSDesignProject }

constructor TUSDesignProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer);
begin
  inherited;
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
  query: TOraQuery;
  measure: TMeasureAction;
  jsonMeasures: TJSONArray;
  jsonMeasure, jsonAction: TJSONValue;
  id: string;
  measureFactor: Double;
  factorString, inverseString: string;
  queryText1, queryText2, table1, table2: string;
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
                    //todo: send imb message about update!
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
begin
  inherited;
  ReadMeasures;
end;

end.
