unit PublishServerSantos;

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

  ModelControllerLib,

  NDWLib,

  System.JSON,
  System.SysUtils,

  PublishServerLib,
  PublishServerGIS,
  PublishServerUS,
  PublishServerMCLib;

//const

type
  TSantosProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aStartScenario: string);
  destructor Destroy; override;
  private
    fUSIMBConnection: TIMBConnection;
  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  end;

implementation


{ TSantosProject }

constructor TSantosProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aStartScenario: string);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource, aDBConnection, aMapView, aPreLoadScenarios, True, aMaxNearestObjectDistanceInMeters);
  fProjectCurrentScenario := ReadScenario(aStartScenario);
  fUSIMBConnection := TIMBConnection.Create(
      GetSetting('IMB3RemoteHost', 'vps17642.public.cloudvps.com'),
      GetSetting('IMB3RemotePort', 4000),
      'PublisherSantos', 21, '');
  EnableControl(modelControl);
end;

destructor TSantosProject.Destroy;
begin

  inherited;
end;

procedure TSantosProject.handleClientMessage(aClient: TClient;
  aScenario: TScenario; aJSONObject: TJSONObject);
begin
  inherited;

end;

procedure TSantosProject.ReadBasicData;
begin
  inherited;

end;

end.
