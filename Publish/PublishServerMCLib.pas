unit PublishServerMCLib;

interface

uses
  StdIni,
  imb4,
  Data.DB,
  PublishServerLib,
  IMB3NativeClient,
  ModelControllerLib,

  System.JSON, System.SysConst, System.Math, System.Variants,
  System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.SysUtils, System.IOUtils;

const
  ModelFilenameSwitch = 'ModelsFilename';

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

  TMCProjectModelConstParameter = class
  constructor Create(const aName, aValue, aParameterType: string);
  private
    fName, fValue, fParameterType: string;
  protected
  public
    function ValueAsVariant: Variant;
    property Name: string read fName;
    property ParemeterType: string read fParameterType;
  end;

  TMCProjectModelVarParameter = class
  constructor Create(const aName, aParameterType, aDefaultValue: string);
  private
    fName, fParameterType, fDefaultValue: string;
  protected
  public
    function DefaultValueAsVariant: Variant;
    property Name: string read fName;
    property ParemeterType: string read fParameterType;
  end;

  TMCProjectModel = class
  constructor Create(const aName: string; const aOptional: Boolean);
  destructor Destory;
  private
    fName: string;
    fOptional: Boolean;
    fConstParameters: TObjectDictionary<string, TMCProjectModelConstParameter>; //locks with TMonitor
    fVarParameters: TObjectDictionary<string, TMCProjectModelVarParameter>; //locks with TMonitor
  public
    property Name: string read fName;
    property Optional: Boolean read fOptional;
    property ConstParameters: TObjectDictionary<string, TMCProjectModelConstParameter> read fConstParameters;
    property VarParameters: TObjectDictionary<string, TMCProjectModelVarParameter> read fVarParameters;
    procedure AddOrSetConstParameter(aConstParameter: TMCProjectModelConstParameter);
    procedure AddOrSetVarParameter(aVarParameter: TMCProjectModelVarParameter);
  end;

  TMCProjectModelManager = class
  constructor Create;
  destructor Destroy; override;
  private
    fMCProjectModels: TObjectList<TMCProjectModel>; //owns, locks with TMonitor
  public
    procedure AddModel(const aName: string; const aOptional: Boolean = False); overload;
    procedure AddModel(const aModel: TMCProjectModel); overload;
    procedure ClaimModels(models: TObjectList<TMCProjectModel>); overload; //claims with any model list, no locking!
    procedure ClaimModels; overload; //locks and claims with own models
    procedure ReadModelsFromFile(const aFilename: string);
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
    fModelManager: TMCProjectModelManager;
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
  fModelManager := TMCProjectModelManager.Create;
  if (GetSettingExists(ModelFilenameSwitch)) then //check if we need to read a model jsonString from file
    fModelManager.ReadModelsFromFile(GetSetting(ModelFilenameSwitch));
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

{ TMCProjectModelManager }

procedure TMCProjectModelManager.AddModel(const aName: string;
  const aOptional: Boolean = False);
begin
  AddModel(TMCProjectModel.Create(aName, aOptional));
end;

procedure TMCProjectModelManager.AddModel(const aModel: TMCProjectModel);
begin
  TMonitor.Enter(fMCProjectModels);
  try
    fMCProjectModels.Add(aModel);
  finally
    TMonitor.Exit(fMCProjectModels);
  end;
end;

procedure TMCProjectModelManager.ClaimModels(
  models: TObjectList<TMCProjectModel>);
begin
  //TODO: implement
end;

procedure TMCProjectModelManager.ClaimModels;
begin
  TMonitor.Enter(fMCProjectModels);
  try
    ClaimModels(fMCProjectModels);
  finally
    TMonitor.Exit(fMCProjectModels);
  end;
end;

constructor TMCProjectModelManager.Create;
begin
  fMCProjectModels := TObjectList<TMCProjectModel>.Create; //owns by default
end;

destructor TMCProjectModelManager.Destroy;
begin
  FreeAndNil(fMCProjectModels);
  inherited;
end;

procedure TMCProjectModelManager.ReadModelsFromFile(const aFilename: string);
var
  filename : string;
  fileObject: TJSONValue;
  models, constParameters, varParameters: TJSONArray;
  model, parameter: TJSONValue;
  modelName : string;
  optional: Boolean;
  parameterName, parameterType, value: string;
  projectModel: TMCProjectModel;
  constParameter: TMCProjectModelConstParameter;
  varParameter: TMCProjectModelVarParameter;
begin
  fileObject := TJSONObject.ParseJSONValue(TFile.ReadAllText(aFilename));
  if (fileObject.TryGetValue<TJSONArray>('models', models)) then
    for model in models do
      if model.TryGetValue<string>('name', modelName) and model.TryGetValue<TJSONArray>('constParameters', constParameters) and model.TryGetValue<TJSONArray>('varParameters', varParameters) then
      begin
        optional := False;
        model.TryGetValue<Boolean>('optional', optional);
        projectModel := TMCProjectModel.Create(modelName, optional);
        for parameter in constParameters do
          if parameter.TryGetValue<string>('name', parameterName) and parameter.TryGetValue<string>('type', parameterType) and parameter.TryGetValue<string>('value', value) then
          begin
            constParameter := TMCProjectModelConstParameter.Create(parameterName, value, parameterType);
            projectModel.AddOrSetConstParameter(constParameter);
          end;
        for parameter in varParameters do
          if parameter.TryGetValue<string>('name', parameterName) and parameter.TryGetValue<string>('type', parameterType) and parameter.TryGetValue<string>('defaultValue', value) then
          begin
            varParameter := TMCProjectModelVarParameter.Create(parameterName, parameterType, value);
            projectModel.AddOrSetVarParameter(varParameter);
          end;
        AddModel(projectModel);
      end;
end;

{ TMCProjectModel }

procedure TMCProjectModel.AddOrSetConstParameter(
  aConstParameter: TMCProjectModelConstParameter);
begin
  TMonitor.Enter(fConstParameters);
  try
    fConstParameters.AddOrSetValue(aConstParameter.Name, aConstParameter);
  finally
    TMonitor.Exit(fConstParameters);
  end;
end;

procedure TMCProjectModel.AddOrSetVarParameter(
  aVarParameter: TMCProjectModelVarParameter);
begin
  TMonitor.Enter(fVarParameters);
  try
    fVarParameters.AddOrSetValue(aVarParameter.Name, aVarParameter);
  finally
    TMonitor.Exit(fVarParameters);
  end;
end;

constructor TMCProjectModel.Create(const aName: string; const aOptional: Boolean);
begin
  fName := aName;
  fOptional := aOptional;
  fConstParameters := TObjectDictionary<string, TMCProjectModelConstParameter>.Create([doOwnsValues]);
  fVarParameters := TObjectDictionary<string, TMCProjectModelVarParameter>.Create([doOwnsValues]);
end;

destructor TMCProjectModel.Destory;
begin
  FreeAndNil(fConstParameters);
  FreeAndNil(fVarParameters);
end;

{ TMCProjectModelConstParameter }

constructor TMCProjectModelConstParameter.Create(const aName, aValue,
  aParameterType: string);
begin
  fName := aName;
  fValue := aValue;
  fParameterType := aParameterType;
end;

function TMCProjectModelConstParameter.ValueAsVariant: Variant;
begin
  if fParameterType='int'
  then Result := fValue.ToInteger
  else if fParameterType='float'
  then Result := Double.Parse(fValue, dotFormat)
  else if fParameterType='bool'
  then Result := Boolean(fValue.ToLower<>'false')
  else if fParameterType='string'
  then Result := fValue
  else Result := System.Variants.null;
end;

{ TMCProjectModelVarParameter }

constructor TMCProjectModelVarParameter.Create(const aName, aParameterType, aDefaultValue: string);
begin
  fName := aName;
  fParameterType := aParameterType;
  fDefaultValue := aDefaultValue;
end;

function TMCProjectModelVarParameter.DefaultValueAsVariant: Variant;
begin
   if fParameterType='int'
  then Result := fDefaultValue.ToInteger
  else if fParameterType='float'
  then Result := Double.Parse(fDefaultValue, dotFormat)
  else if fParameterType='bool'
  then Result := Boolean(fDefaultValue.ToLower<>'false')
  else if fParameterType='string'
  then Result := fDefaultValue
  else Result := System.Variants.null;
end;

end.
