unit PublishServerMCLib;

interface

uses
  StdIni,
  imb4,
  Data.DB,
  PublishServerLib,
  IMB3NativeClient,
  ModelControllerLib,

  System.JSON, System.SysConst, System.Math,
  System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.SysUtils;

type

TClientMCControlInterface = class(TMCControlInterface2)
  constructor Create(
    aConnection: TIMBConnection; const aFederation, aDataSource: string;
    aProject: TProject; const aIdleFederation: string=DefaultIdleFederation);
  private
    fProject: TProject;
  public
    procedure HandleModelChange(aModel: TCIModelEntry2; aChange: TMChange); override;
  public
    function jsonModelStatusNew(const aModelID, aModelName, aModelStatus: string; aModelProgress: Integer): string;
    function jsonModelStatusChange(const aModelID, aModelStatus: string; aModelProgress: Integer): string;
    function jsonModelStatusDelete(const aModelID: string): string;
    function jsonModelStatusArray(const aJSONModelStatusArrayContents: string): string;
  end;

TMCProject = class(TProject)
constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aMCDataSource: string;
    aDBConnection: TCustomConnection;
    aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView;
    aProjectCurrentScenario, aProjectRefScenario: TScenario; aMCIdlePrefix: string=DefaultIdleFederation);
private
protected
    fControlInterface: TClientMCControlInterface;
    fIMB3Connection: TIMBConnection;
public
    property ControlInterface: TClientMCControlInterface read fControlInterface;
end;

implementation

{ TClientMCControlInterface }

constructor TClientMCControlInterface.Create(aConnection: TIMBConnection; const aFederation, aDataSource: string; aProject: TProject;
  const aIdleFederation: string);
begin
  fProject := aProject;
  inherited Create(aConnection, aFederation, aDataSource, aIdleFederation);
end;

procedure TClientMCControlInterface.HandleModelChange(aModel: TCIModelEntry2; aChange: TMChange);
var
  client: TClient;
begin
  if Assigned(fProject.clients) then
    for client in fProject.clients do
    begin
      if Assigned(client.currentScenario) and aModel.IsThisSession(client.currentScenario.ID) then
      begin
        case aChange of
          TMChange.mcNew:
            client.signalString(jsonModelStatusArray(jsonModelStatusNew(aModel.UID.ToString, aModel.ModelName, aModel.State.ToString, aModel.Progress)));
          TMChange.mcRemove:
            client.signalString(jsonModelStatusArray(jsonModelStatusDelete(aModel.UID.ToString)));
        else
            client.signalString(jsonModelStatusArray(
              jsonModelStatusNew(aModel.UID.ToString, aModel.ModelName, aModel.State.ToString, aModel.Progress)
              //jsonModelStatusChange(aModel.UID.ToString, aModel.State.ToString, aModel.Progress))
            ));
        end;
      end;
    end;
end;

function TClientMCControlInterface.jsonModelStatusNew(const aModelID, aModelName, aModelStatus: string; aModelProgress: Integer): string;
begin
  Result := '{"new":{"id":"'+aModelID+'","name":"'+aModelName+'","status":"'+aModelStatus+'","progress":'+aModelProgress.toString+'}}';
end;

function TClientMCControlInterface.jsonModelStatusArray(const aJSONModelStatusArrayContents: string): string;
begin
  Result := '{"type":"modelcontrol","payload":{"status":['+aJSONModelStatusArrayContents+']}}';
end;

function TClientMCControlInterface.jsonModelStatusChange(const aModelID, aModelStatus: string; aModelProgress: Integer): string;
begin
  Result := '{"change":{"id":"'+aModelID+'","status":"'+aModelStatus+'","progress":'+aModelProgress.toString+'}}';
end;

function TClientMCControlInterface.jsonModelStatusDelete(const aModelID: string): string;
begin
  Result := '{"delete":{"id":"'+aModelID+'"}}';
end;


{ TMCProject }

constructor TMCProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL, aMCDataSource: string; aDBConnection: TCustomConnection;
  aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aMapView: TMapView; aProjectCurrentScenario, aProjectRefScenario: TScenario; aMCIdlePrefix: string);
begin
  fIMB3Connection := aIMB3Connection;
  fControlInterface := TClientMCControlInterface.Create(
    fIMB3Connection, '', aMCDataSource, // todo: datasource
    Self,
    aMCIdlePrefix);
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection,
    aAddBasicLayers, aMaxNearestObjectDistanceInMeters, aMapView, aProjectCurrentScenario, aProjectRefScenario);

end;

end.
