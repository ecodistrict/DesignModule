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
    function jsonStatusReset: string;
  end;

  TMCScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription, aFederation: string; aAddbasicLayers: Boolean; aMapView: TMapView; aUseSimulationSetup: Boolean);
  destructor Destroy; override;
  private
    fFederation: string;
  protected
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
    procedure FillModelControl(aClient: TClient);
    procedure HandleRefreshMC;
    property Federation: string read fFederation;
  end;

  TMCProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aMCDataSource: string;
    aDBConnection: TCustomConnection;
    aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aMapView: TMapView;
    aProjectCurrentScenario, aProjectRefScenario: TScenario; aMCIdlePrefix: string=DefaultIdleFederation);
  destructor Destroy; override;
  private
  protected
    fControlInterface: TClientMCControlInterface;
    fIMB3Connection: TIMBConnection; //ref
  public
    property ControlInterface: TClientMCControlInterface read fControlInterface;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
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
  begin
    TMonitor.Enter(fProject.clients);
    try
      for client in fProject.clients do
      begin
        if Assigned(client.currentScenario) then
        begin
          if (client.currentScenario is TMCScenario) and ((aModel.State=msIdle) or (AnsiCompareText(aModel.Federation, (client.currentScenario as TMCScenario).Federation)=0)) then
          begin
            case aChange of
              TMChange.mcNew:
                client.signalString(jsonModelStatusArray(jsonModelStatusNew(aModel.UID.ToString, aModel.ModelName.Replace('\', '\\'), aModel.State.ToString, aModel.Progress)));
              TMChange.mcRemove:
                client.signalString(jsonModelStatusArray(jsonModelStatusDelete(aModel.UID.ToString)));
            else
              client.signalString(jsonModelStatusArray(
                jsonModelStatusNew(aModel.UID.ToString, aModel.ModelName.Replace('\', '\\'), aModel.State.ToString, aModel.Progress)
                //jsonModelStatusChange(aModel.UID.ToString, aModel.State.ToString, aModel.Progress))
              ));
            end;
          end;
  //        else
  //        begin
  //          if (aChange=mcState) //and (aModel.Federation.ToLower=(client.currentScenario as TMCScenario).Federation.ToLower)
  //          then client.signalString(jsonModelStatusArray(jsonModelStatusDelete(aModel.UID.ToString)));
  //        end;
        end;
      end;
    finally
      TMonitor.Exit(fProject.clients);
    end;
  end;
end;

function TClientMCControlInterface.jsonModelStatusNew(const aModelID, aModelName, aModelStatus: string; aModelProgress: Integer): string;
begin
  //todo: remove ' ' after the modelName -> for now fixes error if modelname ends with \
  Result := '{"new":{"id":"'+aModelID+'","name":"'+aModelName+' ","status":"'+aModelStatus+'","progress":'+aModelProgress.toString+'}}';
end;

function TClientMCControlInterface.jsonStatusReset: string;
begin
  Result := '{"reset":"True"}';
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

destructor TMCProject.Destroy;
begin
  inherited;
  FreeAndNil(fControlInterface);
end;

procedure TMCProject.handleClientMessage(aClient: TClient; aScenario: TScenario;
  aJSONObject: TJSONObject);
var
  jsonObject, jsonValue: TJSONValue;
  scenario: TScenario;
begin
  inherited;
  if Assigned(aScenario) and (aScenario is TMCScenario) then
  begin
    if aJSONObject.TryGetValue<TJSONValue>('modelControl', jsonObject) then
      if jsonObject.TryGetValue<TJSONValue>('refresh', jsonValue) then
      begin
        TMonitor.Enter(fScenarios);
        try
          controlInterface.Lock.Acquire;
          try
            controlInterface.Refresh;
          finally
            controlInterface.Lock.Release;
          end;
          for scenario in fScenarios.Values do
            if (scenario is TMCScenario) then
              (scenario as TMCScenario).HandleRefreshMC;
        finally
          TMonitor.Exit(fScenarios);
        end;
      end;
  end;
end;

{ TMCScenario }

constructor TMCScenario.Create(aProject: TProject; const aID, aName,
  aDescription, aFederation: string; aAddbasicLayers: Boolean;
  aMapView: TMapView; aUseSimulationSetup: Boolean);
begin
  fFederation := aFederation;
  inherited Create(aProject, aID, aName, aDescription, aAddbasicLayers, aMapView, aUseSimulationSetup);
end;

destructor TMCScenario.Destroy;
begin

  inherited;
end;

procedure TMCScenario.FillModelControl(aClient: TClient);
var
  mcControlInterface: TClientMCControlInterface;
  jsonNewModels: String;
  model: TCIModelEntry2;
begin
  mcControlInterface := (project as TMCProject).controlInterface;
  mcControlInterface.Lock.Acquire;
  try
    jsonNewModels := mcControlInterface.jsonStatusReset;
    for model in mcControlInterface.Models do
    begin
      if (Assigned(aClient.currentScenario) and ((model.State=msIdle) or (AnsiCompareText(model.Federation, (aClient.currentScenario as TMCScenario).Federation)=0))) then
      begin
        if jsonNewModels<>''
          then jsonNewModels := jsonNewModels+',';
        jsonNewModels := jsonNewModels+mcControlInterface.jsonModelStatusNew(model.UID.ToString, model.ModelName.Replace('\', '\\'), model.State.ToString, model.Progress)
      end;
    end;
    aClient.signalString(mcControlInterface.jsonModelStatusArray(jsonNewModels));
  finally
    mcControlInterface.Lock.Release;
  end;
end;

function TMCScenario.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := inherited HandleClientSubscribe(aClient);
  FillModelControl(aClient);
//  mcControlInterface := (project as TMCProject).controlInterface;
//  mcControlInterface.Lock.Acquire;
//  try
//    jsonNewModels := '';
//    for model in mcControlInterface.Models do
//    begin
//      if (Assigned(aClient.currentScenario) and ((model.State=msIdle) or (AnsiCompareText(model.Federation, (aClient.currentScenario as TMCScenario).Federation)=0))) then
//      begin
//        if jsonNewModels<>''
//          then jsonNewModels := jsonNewModels+',';
//        jsonNewModels := jsonNewModels+mcControlInterface.jsonModelStatusNew(model.UID.ToString, model.ModelName.Replace('\', '\\'), model.State.ToString, model.Progress)
//      end;
//    end;
//    aClient.signalString(mcControlInterface.jsonModelStatusArray(jsonNewModels));
//  finally
//    mcControlInterface.Lock.Release;
//  end;
end;

function TMCScenario.HandleClientUnsubscribe(aClient: TClient): Boolean;
var
  mcControlInterface: TClientMCControlInterface;
  jsonDeleteModels: String;
  model: TCIModelEntry2;
begin
  Result := inherited HandleClientUnsubscribe(aClient);
  //delete the models of this scenario from the ModelControlInterface
  mcControlInterface := (project as TMCProject).controlInterface;
  mcControlInterface.Lock.Acquire;
  try
    jsonDeleteModels := '';
    for model in mcControlInterface.Models do
    begin
      if Assigned(aClient.currentScenario) and model.IsThisSession((aClient.currentScenario as TMCScenario).Federation) then
      begin
        if jsonDeleteModels<>''
          then jsonDeleteModels := jsonDeleteModels+',';
        jsonDeleteModels := jsonDeleteModels+mcControlInterface.jsonModelStatusDelete(model.UID.ToString);
      end;
    end;
    aClient.signalString(mcControlInterface.jsonModelStatusArray(jsonDeleteModels));
  finally
    mcControlInterface.Lock.Release;
  end;
end;

procedure TMCScenario.HandleRefreshMC;
var
  client: TClient;
begin

  TMonitor.Enter(clients);
  try
    for client in clients
    do FillModelControl(client);
  finally
    TMonitor.Exit(clients);
  end;
end;

end.
