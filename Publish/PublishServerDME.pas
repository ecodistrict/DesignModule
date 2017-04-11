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

  PublishServerLib,
  PublishServerGIS,
  PublishServerUS;

type
  TUSDesignProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer{; aSourceEPSG: Integer});
  public
    procedure ReadBasicData(); override;
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

procedure TUSDesignProject.ReadBasicData;
begin
  inherited;
  ReadMeasures;
end;

end.
