unit ModelControllerLib;

interface

uses
  CmdLin, StdIni, MyStr,
  IMB3NativeClient, IMB3Core, ByteBuffers,
  Logger,
  Generics.Collections, Windows, SyncObjs, Variants, SysUtils, Classes;

const
  ControllersRootEventName = 'Controllers';
  ClientsRootEventName = 'Clients';

  FederationParameterName = 'Federation';
  DataSourceParameterName = 'DataSource';

  // model commands
  mcModelClaim = 11;
  mcModelUnClaim = 12;

  mcModel = 21;
  mcRequestModels = 22;

  mcDefaultParameters = 31;
  mcRequestDefaultParameters = 32;

  mcFolderContents = 41;
  mcRequestFolderContents = 42;

  mcController = 51;
  mcRequestControllers = 52;
  mcControllerChange = 55;

  mcControllerModelSetup = 61;
  mcRequestControllerModelSetups = 62;
  mcControllerModelSetupNew = 63;
  mcControllerModelSetupDelete = 64;
  mcControllerModelSetupChange = 65;

  mcModelInit = 71;
  mcModelQuitApplication = 72;
  mcModelProgress = 73;

  DefaultParametersEventTimeOut = 20000; // in ms

  RemoteHostSwitch = 'RemoteHost';
  RemotePortSwitch = 'RemotePort';
  IdleFederationSwitch = 'IdleFederation';
    DefaultIdleFederation = 'USidle';
  LinkIDSwitch = 'LinkID';
  ControllersEventNameSwitch = 'ControllersEventName';
  ControllerPrivateEventNameSwitch = 'ControllerPrivateEventName';
  ControllerSwitch = 'ControllerName';

  ControllerPrioritySwitch = 'ControllerPriority';
    DefaultControllerPriority = 1;

  ModelPrioritySwitch = 'ModelPriority';
    DefaultModelPriority = 1;

  RootModelFolderSwitch = 'RootModelFolder';

  ModelsSection = 'Models';

  cecModelAdd = actionNew;
  cecModelRemove = actionDelete;
  cecModelSet = actionChange;

  ControllerLoopWaitTime = 3000;


  ModelNameSwitch = 'ModelName';
    DefaultModelName = 'Undefined model name';
  ModelIDSwitch = 'ModelID';
    DefaultModelID = 99;

  ModelIdleKillTime = 3600; // 3 hours


type
  TModelState = (
    msReady,
    msCalculating,
    msBusy,
    msIdle,           // model is available for use
    msLock,           // claim model for use in this session
    msRemoved=-1      // model is no longer available
  );

  TModelStateHelper = record helper for TModelState
    function ToString(): string;
    function ToShortString(): string;
  end;


type
  // parameters
  TModelParameterValueType = (
    mpvtFloat,
    mpvtBoolean,
    mpvtInteger,
    mpvtString
  );

  TModelParameterValueTypeHelper = record helper for TModelParameterValueType
    function ToString(): string;
  end;

  TModelParameterValueList = array of Variant;

  TModelParameter = class
  constructor Create(const aName: string; aValueType: TModelParameterValueType; aValue: Variant); overload;
  constructor Create(const aName: string; aValueType: TModelParameterValueType; aValue: Variant; aValueList: TModelParameterValueList); overload;
  constructor Create(const aName: string; aValue: Integer; const aValueList: array of Integer); overload;
  constructor Create(const aName: string; aValue: Double; const aValueList: array of Double); overload;
  constructor Create(const aName: string; aValue: Boolean; aValueList: array of string); overload;
  constructor Create(const aName: string; const aValue: string; const aValueList: array of string); overload;
  constructor Create(const aName: string; aValue: Integer); overload;
  constructor Create(const aName: string; aValue: Double); overload;
  constructor Create(const aName: string; const aValue: string); overload;
  constructor Create(const aName: string; aValue: Boolean); overload;
  constructor Create(aModelParameter: TModelParameter); overload;
  destructor Destroy; override;
  private
    FName: string;
    FValueType: TModelParameterValueType;
    FValue: Variant;
    FValueList: TModelParameterValueList;
    function GetValue: Variant;
    function GetValueAsString: string;
    procedure SetValue(const aValue: Variant);
    function GetValueList: TModelParameterValueList;
  public
    property Name: string read FName;
    property ValueType: TModelParameterValueType read FValueType;
    property Value: Variant read GetValue write SetValue;
    property ValueAsString: String read GetValueAsString;
    property ValueList: TModelParameterValueList read GetValueList;
    function GetValueListIndexOf(const aValue: Variant): Integer;
    // serialization
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TModelParameters = class(TObjectList<TModelParameter>)
  constructor Create(aParameters: TModelParameters=nil);
  private
    function GetParameterByName(const aParameterName: string): TModelParameter;
    function GetValue(const aParameterName: string): Variant;
    procedure SetValue(const aParameterName: string; const aValue: Variant);
    function GetParameterType(const aParameterName: string): TModelParameterValueType;
  public
    procedure AddParameters(aParameters: TModelParameters);
    procedure GetParameterNames(aParameterNames: TStrings);
    property ValueType[const aParameterName: string]: TModelParameterValueType read GetParameterType;
    property Value[const aParameterName: string]: Variant read GetValue write SetValue;
    property ParameterByName[const aParameterName: string]: TModelParameter read GetParameterByName;
    function ParameterExists(const aParameterName: string): Boolean;
    // serialization
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  // IMB event definitions

  TModelNewEvent = record
  public
    UID: Integer;
    ModelName: AnsiString;
    Controller: AnsiString;
    Priority: Integer;
    State: Integer;
    Federation: AnsiString;
    ModelPrivateEventName: AnsiString;
    ControllerPrivateEventName: AnsiString;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TModelDeleteEvent = record
  public
    UID: Integer;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TModelChangeEvent = record
  public
    UID: Integer;
    State: Integer;
    Federation: AnsiString;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TModelInitEvent = record
  public
    LinkID: Int64;
    UID: Integer;
    ModelName: AnsiString;
    ModelPrivateEventName: AnsiString;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TControllerNewEvent = record
  public
    UID: Integer;
    Controller: AnsiString;
    ControllerPrivateEventName: AnsiString;
    Priority: Integer;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TControllerDeleteEvent = TModelDeleteEvent;

  TControllerChangeEvent = record
  public
    UID: Integer;
    Priority: Integer;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TControllerModelSetupNewEvent = record
  public
    UID: Integer;
    Path: AnsiString;
    RunCount: Integer;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TControllerModelSetupDeleteEvent = record
  public
    UID: Integer;
    Path: AnsiString;
  public
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
  end;

  TControllerModelSetupChangeEvent = TControllerModelSetupNewEvent;

  TControllerFolderContentsEvent = class
  constructor Create;
  destructor Destroy; override;
  private
    FUID: Integer;
    FSubFolder: AnsiString;
    FNames: TStringList;
  public
    property UID: Integer read FUID;
    property SubFolder: AnsiString read FSubFolder;
    property Names: TStringList read FNames;
    procedure Prepare(var aPayload: TByteBuffer);
    procedure QWrite(var aPayload: TByteBuffer);
    procedure Read(var aPayload: TByteBuffer);
    procedure Browse(aUID: Integer; const aRootModelFolder, aSubFolder: string);
  end;

  // model starter

  TMCModelStarter = class;

  TMSOnParameterRequest = procedure(aModelControl: TMCModelStarter; aConnection: TIMBConnection; aParameters: TModelParameters) of object;
  TMSOnStartModel = procedure(aModelControl: TMCModelStarter; aConnection: TIMBConnection; var aParameters: TModelParameters; var aModelThread: TMyThread) of object;
  TMSOnChangeFederation = procedure(aModelControl: TMCModelStarter; aConnection: TIMBConnection; aNewFederationID: Integer; var aNewFederation: string) of object;
  TMSOnStopModel = procedure(aModelControl: TMCModelStarter; aConnection: TIMBConnection; var aModelThread: TMyThread) of object;
  TMSOnQuitApplication = procedure(aModelControl: TMCModelStarter; aConnection: TIMBConnection) of object;
  TMSOnRequestModels = procedure(aModelControl: TMCModelStarter; const aReturnEventName: string) of object;

  TMSOnParameterRequestNoObj = procedure(aSelf: Pointer; aModelControl: TMCModelStarter; aConnection: TIMBConnection; aParameters: TModelParameters);
  TMSOnStartModelNoObj = procedure(aSelf: Pointer; aModelControl: TMCModelStarter; aConnection: TIMBConnection; var aParameters: TModelParameters; var aModelThread: TMyThread);
  TMSOnChangeFederationNoObj = procedure(aSelf: Pointer; aModelControl: TMCModelStarter; aConnection: TIMBConnection; aNewFederationID: Integer; var aNewFederation: string);
  TMSOnStopModelNoObj = procedure(aSelf: Pointer; aModelControl: TMCModelStarter; aConnection: TIMBConnection; var aModelThread: TMyThread);
  TMSOnQuitApplicationNoObj = procedure(aSelf: Pointer; aModelControl: TMCModelStarter; aConnection: TIMBConnection);
  TMSOnRequestModelsNoObj = procedure(aSelf: Pointer; aModelControl: TMCModelStarter; const aReturnEventName: string);

  TMCModelStarter = class
  constructor Create(const aModelName: string=''; aModelID: Integer=0);
  destructor Destroy; override;
  private
    FConnection: TIMBConnection;
    FController: AnsiString;
    FPrivateModelEvent: TIMBEventEntry;
    FControllersEvent: TIMBEventEntry;
    FPrivateControllerEvent: TIMBEventEntry;
    FOnParameterRequest: TMSOnParameterRequest;
    FOnStartModel: TMSOnStartModel;
    FOnChangeFederation: TMSOnChangeFederation;
    FOnStopModel: TMSOnStopModel;
    FOnQuitApplication: TMSOnQuitApplication;
    FOnRequestModels: TMSOnRequestModels;
    FModelThread: TMyThread;
    FIdleFederation: string;
    FQuitApplicationEvent: TEvent;
    FState: TModelState;
    FPriority: Integer;
    FProgress: Integer;
    procedure HandleControlEvents(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
    procedure HandleChangeFederation(aConnection: TIMBConnection; aNewFederationID: Integer; const aNewFederation: string); stdcall;
    procedure SetOnParameterRequestNoObj(aSelf: Pointer; const aValue: TMSOnParameterRequestNoObj);
    procedure SetOnStartModelNoObj(aSelf: Pointer; const aValue: TMSOnStartModelNoObj);
    procedure SetOnChangeFederationNoObj(aSelf: Pointer; const aValue: TMSOnChangeFederationNoObj);
    procedure SetOnStopModelNoObj(aSelf: Pointer; const aValue: TMSOnStopModelNoObj);
    procedure SetOnQuitApplicationNoObj(aSelf: Pointer; const aValue: TMSOnQuitApplicationNoObj);
    procedure SetOnRequestModelsNoObj(aSelf: Pointer; const aValue: TMSOnRequestModelsNoObj);
    procedure SignalModelInit(aLinkID: Int64; const aModelName: AnsiString);
  public
    property Connection: TIMBConnection read FConnection;
    property IdleFederation: string read FIdleFederation;
    property PrivateModelEvent: TIMBEventEntry read FPrivateModelEvent;
    property QuitApplicationEvent: TEvent read FQuitApplicationEvent;
    // events
    property OnParameterRequest: TMSOnParameterRequest read FOnParameterRequest write FOnParameterRequest;
    property OnStartModel: TMSOnStartModel read FOnStartModel write FOnStartModel;
    property OnChangeFederation: TMSOnChangeFederation read FOnChangeFederation write FOnChangeFederation;
    property OnStopModel: TMSOnStopModel read FOnStopModel write FOnStopModel;
    property OnQuitApplication: TMSOnQuitApplication read FOnQuitApplication write FOnQuitApplication;
    property OnRequestModels: TMSOnRequestModels read FOnRequestModels write FOnRequestModels;
    // events (non methods)
    property OnParameterRequestNoObj[aSelf: Pointer]: TMSOnParameterRequestNoObj write SetOnParameterRequestNoObj;
    property OnStartModelNoObj[aSelf: Pointer]: TMSOnStartModelNoObj write SetOnStartModelNoObj;
    property OnChangeFederationNoObj[aSelf: Pointer]: TMSOnChangeFederationNoObj write SetOnChangeFederationNoObj;
    property OnStopModelNoObj[aSelf: Pointer]: TMSOnStopModelNoObj write SetOnStopModelNoObj;
    property OnQuitApplicationNoObj[aSelf: Pointer]: TMSOnQuitApplicationNoObj write SetOnQuitApplicationNoObj;
    property OnRequestModelsNoObj[aSelf: Pointer]: TMSOnRequestModelsNoObj write SetOnRequestModelsNoObj;

    procedure SignalModelState(aState: TModelState); overload;
    procedure SignalModelState(aState: Integer); overload;
    procedure SignalModelState(aState: TModelState; const aFederation: string); overload;
    procedure SignalModelProgress(aProgress: Integer);
    procedure SignalModelExit;
    procedure SignalModelNew(const aEventName: string);

    procedure DoParameterRequest(aParameters: TModelParameters);
    procedure DoStartModel(aParameters: TModelParameters);
    procedure DoStopModel;
    procedure DoQuitApplication;
    procedure DoRequestModels(const aReturnEventName: string);
  end;

  TMCModelStarter2 = class
  constructor Create(const aModelName: string=''; aModelID: Integer=0);
  destructor Destroy; override;
  private
    FConnection: TIMBConnection;
    FController: AnsiString;
    FPrivateModelEvent: TIMBEventEntry;
    FControllersEvent: TIMBEventEntry;
    FPrivateControllerEvent: TIMBEventEntry;
    FModelThread: TMyThread;
    FIdleFederation: string;
    FQuitApplicationEvent: TEvent;
    FState: TModelState;
    FPriority: Integer;
    FProgress: Integer;
    procedure HandleControlEvents(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
    procedure SignalModelInit(aLinkID: Int64; const aModelName: AnsiString);
  public
    property Connection: TIMBConnection read FConnection;
    property IdleFederation: string read FIdleFederation;
    property PrivateModelEvent: TIMBEventEntry read FPrivateModelEvent;
    property QuitApplicationEvent: TEvent read FQuitApplicationEvent;

    procedure SignalModelState(aState: TModelState); overload;
    procedure SignalModelState(aState: Integer); overload;
    procedure SignalModelState(aState: TModelState; const aFederation: string); overload;
    procedure SignalModelProgress(aProgress: Integer);
    procedure SignalModelExit;
    procedure SignalModelNew(const aEventName: string);

    // override in derived class
    procedure ParameterRequest(aParameters: TModelParameters); virtual; abstract;
    procedure StartModel(aParameters: TModelParameters); virtual; abstract;
    procedure StopModel; virtual; abstract;
    // override in derived class (optional)
    procedure QuitApplication; virtual;
    procedure RequestModels(const aReturnEventName: string); virtual;
    procedure ManualStart(aState: TModelState; const aFederation: string);
  end;

  // controller thread

  TRTState = (rtsRunning, rtsClosing, rtsHanging, rtsTerminated);

  TCTModelRunTime = class
  constructor Create(const aPath, aRemoteHost: string; aRemotePort: Integer;const aIdleFederation: string;
    const aController, aControllersEventName, aControllerPrivateEventName: string; aControllersEvent: TIMBEventEntry;
    aPriority: Integer; aCUDAWorkARound: Boolean);
  destructor Destroy; override;
  private
    FProcessHandle: THandle;
    FUniqueClientID: Integer;
    FState: Integer;
    FIdleTimeLeft: Integer;
    FFederation: string;
    FPrivateEvent: TIMBEventEntry;
    FModelName: AnsiString;
    FRTState: TRTState;
    FControllersEvent: TIMBEventEntry;
    procedure SignalModelQuitApplication;
    function SignalModelExit: Boolean;
  public
    function LinkID: NativeInt;
    property RTState: TRTState read FRTState;
    function Terminate: Boolean;
    procedure Unregister(aReceivedQuit: Boolean);
  end;

  TCTModelSetup = class
  constructor Create(const aPath: string; aRunCount: Integer; aCudaWorkARound: Boolean);
  destructor Destroy; override;
  private
    FPath: string;
    FRunCount: Integer;
    FProcesses: TObjectList<TCTModelRunTime>;
    FCudaWorkARound: Boolean;
    procedure SetRunCount(const aValue: Integer);
  public
    property RunCount: Integer read FRunCount write SetRunCount;
    procedure SaveSetup;
    procedure RemoveSetup;
  end;

  TNodeControllerThread = class(TThread)
  constructor Create(aConnection: TIMBConnection);
  destructor Destroy; override;
  private
    FConnection: TIMBConnection;
    FController: string;
    FControllersEvent: TIMBEventEntry;
    FPrivateEvent: TIMBEventEntry;
    FPriority: Integer;
    FModelsLock: TCriticalSection;
    FModels: TObjectList<TCTModelSetup>;
    FRootModelFolder: string;
  private
    function GetModelOnPath(const aPath: string): TCTModelSetup;
    function GetModelRunTimeOnLinkID(aLinkID: NativeInt): TCTModelRunTime;
    function GetModelRunTimeOnUniqueClientID(aUniqueClientID: Integer): TCTModelRunTime;
    function DeleteModelRunTimeOnUniqueClientID(aUniqueClientID: Integer): Boolean;

    procedure SignalControllerNew(const aEventName: string);
    procedure SignalControllerDelete(const aEventName: string);
    procedure SignalControllerChange(const aEventName: string);

    procedure SignalControllerModelSetupNew(const aEventName: string; aModelSetup: TCTModelSetup);
    procedure SignalControllerModelSetupDelete(const aEventName: string; aModelSetup: TCTModelSetup);
    procedure SignalControllerModelSetupChange(const aEventName: string; aModelSetup: TCTModelSetup);
  protected
    procedure Execute; override;
    procedure HandleControllerEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
  public
    procedure SignalControllerExit;
  end;

  // node controller config

  TMCConfigController = class
  constructor Create(aUID: Integer; const aController, aControllerPrivateEventName: string; aPriority: Integer);
  destructor Destroy; override;
  private
    FUID: Integer;
    FController: string;
    FControllerPrivateEventName: string;
    FPriority: Integer;
    FModelPaths: TStringList;
  public
    property UID: Integer read FUID write FUID;
    property Controller: string read FController write FController;
    property ControllerPrivateEventName: string read FControllerPrivateEventName write FControllerPrivateEventName;
    property Priority: Integer read FPriority write FPriority;
    property ModelPaths: TStringList read FModelPaths;
  end;

  TMCConfigControllerList = class(TObjectList<TMCConfigController>)
  constructor Create;
  destructor Destroy; override;
  private
    FLock: TCriticalSection;
  public
    property Lock: TCriticalSection read FLock;
    function FindControllerOnUID(aUID: Integer): TMCConfigController;
  end;

  // model control interface

  TCIOnModelState = procedure(const aModelName: string; aState: TModelState; aProgress: Integer) of object;

  TCIOnModelStateEx = procedure(aUID: Integer; const aController, aModelName, aFederation: string;
    aState: TModelState; aProgress: Integer; aPriority: Integer; aThisSession: Boolean) of object;

  TCIModelEntry = class
  constructor Create(const aModelName: string; aState: TModelState);
  private
    FModelName: string;
    FState: TModelState;
    FProgress: Integer;
  end;

  TModelCache = record
    FUID: Integer;
    FModelName: string;
    FController: string;
    FPriority: Integer;
    FState: TModelState;
    FProgress: Integer;
    FFederation: string;
    FIsThisSession: Boolean;
  end;

  TCIModelEntryEx = class
  constructor Create(aUID: Integer; const aModelName: string; const aController: string;
    aPriority: Integer; aState: TModelState;
  const aFederation, aModelPrivateEventName, aControllerPrivateEventName: string);
  destructor Destroy; override;
  private
    FUID: Integer;
    FModelName: string;
    FController: string;
    FPriority: Integer;
    FState: TModelState;
    FProgress: Integer;
    FFederation: string;
    FModelPrivateEventName: string;
    FControllerPrivateEventName: string;
    //FDefaultParameters: TModelParameters;
    //FDefaultParametersEvent: TEvent;
    FTag: NativeInt;
    FLinkedModelEntry: TCIModelEntry;
  public
    property UID: Integer read FUID;
    property ModelName: string read FModelName;
    property Controller: string read FController;
    property Priority: Integer read FPriority;
    property State: TModelState read FState;
    property Progress: Integer read FProgress;
    property Federation: string read FFederation;
    //property DefaultParameters: TModelParameters read FDefaultParameters;
    property Tag: NativeInt read FTag write FTag;
//    property DataSource: string read getDataSource;
//    property Scenario: string read getScenario;
    function CreateCache(aIsThisSession: Boolean): TModelCache; overload;
    function CreateCache(aState: TModelState): TModelCache; overload;
  end;

  TMCControlInterface = class; // forward

  TCIOnParameter = reference to procedure(aControlInterface: TMCControlInterface; aModel: TCIModelEntryEx; aParameter: TModelParameter);

{$TYPEINFO ON}

  TMCControlInterface = class
  constructor Create(
    const aRemoteHost: string; aRemotePort: Integer;
    const aOwnerName: string; aOwnerID: Integer;
    const aFederation, aDataSource: string;
    const aIdleFederation: string=DefaultIdleFederation);
  destructor Destroy; override;
  private
    FConnection: TIMBConnection;
    FIdleFederation: string;
    FDataSource: string;
    FOnModelState: TCIOnModelState;
    FOnModelStateEx: TCIOnModelStateEx;
    FOnParameter: TCIOnParameter;
    FModels: TObjectList<TCIModelEntry>;
    FModelsEx: TObjectList<TCIModelEntryEx>;
    FModelsLock: TCriticalSection;
    FPrivateEvent: TIMBEventEntry;
    FControllersEvent: TIMBEventEntry;
    function RequestModels: Boolean;
    function GetModelIndexOnName(const aModelName: string): Integer;
    function GetModelExIndexOnName(const aModelName: string): Integer;
    function GetModelExIndexOnUID(aUID: Integer): Integer;
    function IsThisSession(aModel: TCIModelEntryEx): Boolean;
    function GetCurrentModelEntryState(const aModelName: string): TModelState;
    function SetCurrentModelEntryState(const aModelName: string; aState: TModelState): TCIModelEntry;
    function GetNewModelEntryState(const aModelName: string): TModelState;
    function RequestModelParameters(const aModelPrivateEventName: string): Boolean;
    procedure HandleNewModel(aUID: Integer; const aModelName: string; const aController: string;
      aPriority: Integer; aState: TModelState; const aFederation, aModelPrivateEventName, aControllerPrivateEventName: string);
    procedure HandleDeleteModel(aUID: Integer);
    procedure HandleChangeModel(aUID: Integer; aState: TModelState; const aFederation: string);
    procedure AddSessionParameters(aParameters: TModelParameters);
    procedure HandleControllersEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
  public
    procedure SetEnvironment(const aFederation, aDataSource: string);

    function SignalModelState(aUID: Integer; aState: TModelState; const aFederation: string): Integer;
    function SignalModelQuitApplication(aUID: Integer): Integer;
    function GetFreeModelUIDOnName(const aModelName: string): Integer;
    function GetLockedModelUIDOnName(const aModelName: string): Integer;

    function StartModelEx(aUID: Integer; aParameters: TModelParameters): Boolean;
    function StopModelEx(aUID: Integer): Boolean;
  // standard
  public
    property Connection: TIMBConnection read FConnection;
    procedure GetModels;
    procedure Refresh; // manual refresh, normally not called by application
  //published
    property OnModelState: TCIOnModelState read FOnModelState write FOnModelState;
  // advanced
  public
    procedure GetModelsEx;
    function GetModelParametersEx(aUID: Integer): Boolean;
  //published
    property OnModelStateEx: TCIOnModelStateEx read FOnModelStateEx write FOnModelStateEx;
    property OnParameter: TCIOnParameter read FOnParameter write FOnParameter;
  end;

  TMChange = (
    mcNew,
    mcRemove,
    mcState,
    mcFederation,
    mcProgress
  );

  TCIModelEntry2 = class
  constructor Create(aUID: Integer; const aModelName: string; const aController: string;
    aPriority: Integer; aState: TModelState;
  const aFederation, aModelPrivateEventName, aControllerPrivateEventName: string);
  destructor Destroy; override;
  private
    FUID: Integer;
    FModelName: string;
    FController: string;
    FPriority: Integer;
    FState: TModelState;
    FProgress: Integer;
    FFederation: string;
    FModelPrivateEventName: string;
    FControllerPrivateEventName: string;
    FDefaultParameters: TModelParameters;
    FDefaultParametersEvent: TEvent;
    FTag: NativeInt;
  public
    property UID: Integer read FUID;
    property ModelName: string read FModelName;
    property Controller: string read FController;
    property Priority: Integer read FPriority;
    property State: TModelState read FState;
    property Progress: Integer read FProgress;
    property Federation: string read FFederation;
    property DefaultParameters: TModelParameters read FDefaultParameters;
    property Tag: NativeInt read FTag write FTag;
    function IsThisSession(const aFederation: string): Boolean;
//    todo: property DataSource: string read getDataSource;
//    todo: property Scenario: string read getScenario;
  end;

  TMCControlInterface2 = class
  constructor Create(aConnection: TIMBConnection; const aFederation, aDataSource: string;
    const aIdleFederation: string=DefaultIdleFederation);
  destructor Destroy; override;
  private
    FConnection: TIMBConnection;
    FFederation: string;
    FDataSource: string;
    FIdleFederation: string;
    FModels: TObjectList<TCIModelEntry2>;
    FLock: TCriticalSection;
    FPrivateEvent: TIMBEventEntry;
    FControllersEvent: TIMBEventEntry;
    function GetModelExIndexOnUID(aUID: Integer): Integer;
    procedure SetFederation(const aValue: string);
    procedure AddSessionParameters(aParameters: TModelParameters);
    procedure HandleControllersEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
  private
    function SignalRequestModels: Boolean;
    function SignalModelQuitApplication(aUID: Integer): Integer;
    function SignalRequestModelParameters(const aModelPrivateEventName: string): Boolean;
  public
    property Connection: TIMBConnection read FConnection;
    property DataSource: string read FDataSource;
    property Federation: string read FFederation write SetFederation;
    property Lock: TCriticalSection read FLock;
    property Models: TObjectList<TCIModelEntry2> read FModels;
    procedure HandleModelChange(aModel: TCIModelEntry2; aChange: TMChange); virtual;
    procedure Refresh; // manual refresh, normally not called by application
    function IsThisSession(aModel: TCIModelEntry2): Boolean;
    function ClaimModel(aModel: TCIModelEntry2; aParameters: TModelParameters): Boolean;
    function UnclaimModel(aModel: TCIModelEntry2): Boolean;
    function QuitModel(aModel: TCIModelEntry2): Boolean;
    function RequestModelDefaultParameters(aModel: TCIModelEntry2; aTimeOut: LongWord=DefaultParametersEventTimeOut): Boolean;
  end;

implementation

{ TModelParameter }

constructor TModelParameter.Create(const aName: string; aValueType: TModelParameterValueType; aValue: Variant;
  aValueList: TModelParameterValueList);
begin
  Create(aName, aValueType, aValue);
  FValueList := aValueList;
end;

constructor TModelParameter.Create(aModelParameter: TModelParameter);
begin
  // create this as a copy of the given parameter
  Create(aModelParameter.FName, aModelParameter.FValueType, aModelParameter.FValue, aModelParameter.FValueList);
end;

constructor TModelParameter.Create(const aName: string; aValueType: TModelParameterValueType; aValue: Variant);
begin
  inherited Create;
  FName := aName;
  FValueType := aValueType;
  FValue := aValue;
end;

constructor TModelParameter.Create(const aName: string; aValue: Integer; const aValueList: array of Integer);
var
  ValueList: TModelParameterValueList;
  v: Integer;
begin
  Setlength(ValueList, Length(aValueList));
  for v := 0 to Length(aValueList) - 1
  do ValueList[v] := aValueList[v];
  Create(aName, mpvtInteger, aValue, ValueList);
end;

constructor TModelParameter.Create(const aName: string; aValue: Double; const aValueList: array of Double);
var
  ValueList: TModelParameterValueList;
  v: Integer;
begin
  Setlength(ValueList, Length(aValueList));
  for v := 0 to Length(aValueList) - 1
  do ValueList[v] := aValueList[v];
  Create(aName, mpvtFloat, aValue, ValueList);
end;

constructor TModelParameter.Create(const aName, aValue: string; const aValueList: array of string);
var
  ValueList: TModelParameterValueList;
  v: Integer;
begin
  Setlength(ValueList, Length(aValueList));
  for v := 0 to Length(aValueList) - 1
  do ValueList[v] := aValueList[v];
  Create(aName, mpvtString, aValue, ValueList);
end;

constructor TModelParameter.Create(const aName: string; aValue: Double);
begin
  Create(aName, aValue, []);
end;

constructor TModelParameter.Create(const aName: string; aValue: Integer);
begin
  Create(aName, aValue, []);
end;

constructor TModelParameter.Create(const aName: string; aValue: Boolean);
begin
  Create(aName, aValue, []);
end;

constructor TModelParameter.Create(const aName: string; aValue: Boolean; aValueList: array of string);
var
  ValueList: TModelParameterValueList;
  v: Integer;
begin
  Setlength(ValueList, Length(aValueList));
  for v := 0 to Length(aValueList) - 1
  do ValueList[v] := aValueList[v];
  Create(aName, mpvtBoolean, aValue, ValueList);
end;

constructor TModelParameter.Create(const aName, aValue: string);
begin
  Create(aName, aValue, []);
end;

destructor TModelParameter.Destroy;
begin
  SetLength(FValueList, 0);
  inherited;
end;

function TModelParameter.GetValue: Variant;
begin
  Result := FValue;
end;

function TModelParameter.GetValueAsString: string;
var
  i: Integer;
  d: Double;
begin
  case FValueType of
    mpvtFloat:
      begin
        d := fValue;
        Result := d.toString;
      end;
    mpvtBoolean:
      begin
        i :=  fValue;
        if i=0
        then Result := 'FALSE'
        else Result := 'TRUE';
      end;
    mpvtInteger:
      begin
        i :=  fValue;
        Result := i.toString;
      end;
    mpvtString:
      begin
        Result := fValue;
      end;
  end;
end;

function TModelParameter.GetValueList: TModelParameterValueList;
begin
  Result := FValueList;
end;

function TModelParameter.GetValueListIndexOf(const aValue: Variant): Integer;
begin
  Result := Length(FValueList)-1;
  while (Result>=0) and (FValueList[Result]<> aValue)
  do Result := Result-1;
end;

procedure TModelParameter.Prepare(var aPayload: TByteBuffer);
var
  vli: Integer;
begin
  aPayload.Prepare(AnsiString(FName));
  aPayload.Prepare(Integer(FValueType));
  case FValueType of
    mpvtFloat:
      begin
        aPayload.Prepare(Double(FValue));
        aPayload.Prepare(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.Prepare(Double(FValueList[vli]));
      end;
    mpvtBoolean:
      begin
        aPayload.Prepare(Boolean(FValue));
        aPayload.Prepare(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.Prepare(AnsiString(FValueList[vli]));
      end;
    mpvtInteger:
      begin
        aPayload.Prepare(Integer(FValue));
        aPayload.Prepare(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.Prepare(Integer(FValueList[vli]));
      end;
    mpvtString:
      begin
        aPayload.Prepare(AnsiString(FValue));
        aPayload.Prepare(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.Prepare(AnsiString(FValueList[vli]));
      end;
  end;
end;

procedure TModelParameter.QWrite(var aPayload: TByteBuffer);
var
  vli: Integer;
begin
  aPayload.QWrite(AnsiString(FName));
  aPayload.QWrite(Integer(FValueType));
  case FValueType of
    mpvtFloat:
      begin
        aPayload.QWrite(Double(FValue));
        aPayload.QWrite(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.QWrite(Double(FValueList[vli]));
      end;
    mpvtBoolean:
      begin
        aPayload.QWrite(Boolean(FValue));
        aPayload.QWrite(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.QWrite(AnsiString(FValueList[vli]));
      end;
    mpvtInteger:
      begin
        aPayload.QWrite(Integer(FValue));
        aPayload.QWrite(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.QWrite(Integer(FValueList[vli]));
      end;
    mpvtString:
      begin
        aPayload.QWrite(AnsiString(FValue));
        aPayload.QWrite(Integer(Length(FValueList)));
        for vli := 0 to Length(FValueList) - 1
        do aPayload.QWrite(AnsiString(FValueList[vli]));
      end;
  end;
end;

procedure TModelParameter.Read(var aPayload: TByteBuffer);
var
  c: Integer;
  vli: Integer;
begin
  FName := string(aPayload.ReadAnsiString);
  FValueType := TModelParameterValueType(aPayload.ReadInteger);
  case FValueType of
    mpvtFloat:
      begin
        FValue := aPayload.ReadDouble;
        aPayload.Read(c);
        SetLength(FValueList, c);
        for vli := 0 to c - 1
        do FValueList[vli] := aPayload.ReadDouble;
      end;
    mpvtBoolean:
      begin
        FValue := aPayload.ReadBoolean;
        aPayload.Read(c);
        SetLength(FValueList, c);
        for vli := 0 to c - 1
        do FValueList[vli] := aPayload.ReadAnsiString;
      end;
    mpvtInteger:
      begin
        FValue := aPayload.ReadInteger;
        aPayload.Read(c);
        SetLength(FValueList, c);
        for vli := 0 to c - 1
        do FValueList[vli] := aPayload.ReadInteger;
      end;
    mpvtString:
      begin
        FValue := aPayload.ReadAnsiString;
        aPayload.Read(c);
        SetLength(FValueList, c);
        for vli := 0 to c - 1
        do FValueList[vli] := aPayload.ReadAnsiString;
      end;
  end;
end;

procedure TModelParameter.SetValue(const aValue: Variant);

  procedure SetValueOnType;
  var
    d: Double;
    b: Boolean;
    i: Int64;
    s: string;
  begin
    case FValueType of
      mpvtFloat:
        begin
          d := aValue;
          FValue := d;
        end;
      mpvtBoolean:
        begin
          b := aValue;
          FValue := b;
        end;
      mpvtInteger:
        begin
          i := aValue;
          FValue := i;
        end;
    else // mpvtString
          s := aValue;
          FValue := s;
    end;
  end;

var
  vli: Integer;
begin
  if Length(FValueList)>0 then
  begin
    if FValueType=mpvtBoolean then
    begin
      if VarType(aValue)<>varBoolean then
      begin
        vli := GetValueListIndexOf(aValue);
        if vli>=0 then
        begin
          FValue := (vli MOD 2)=1;
        end
        else raise EInvalidOperation.Create('Invalid value ('+aValue+') for assigning to boolean parameter "'+FName+'"');
      end
      else FValue := aValue;
    end
    else
    begin
      if GetValueListIndexOf(aValue)>=0
      then SetValueOnType
      else raise EInvalidOperation.Create('Invalid value ('+aValue+') for assigning to parameter "'+FName+'"');
    end;
  end
  else SetValueOnType;
end;

{ TModelParameters }

procedure TModelParameters.AddParameters(aParameters: TModelParameters);
var
  p: Integer;
begin
  if Assigned(aParameters) then
  begin
    // copy all parameters
    for p := 0 to aParameters.Count - 1
    do Add(TModelParameter.Create(aParameters[p]));
  end;
end;

constructor TModelParameters.Create(aParameters: TModelParameters);
begin
  inherited Create;
  AddParameters(aParameters);
end;

function TModelParameters.GetParameterByName(const aParameterName: string): TModelParameter;
var
  i: Integer;
begin
  i := Count-1;
  while (i>=0) and (AnsiCompareText(aParameterName, Items[i].Name)<>0)
  do i := i-1;
  if i>=0
  then Result := Items[i]
  else Result := nil;
end;

procedure TModelParameters.GetParameterNames(aParameterNames: TStrings);
var
  i: Integer;
begin
  for i := 0 to Count - 1
  do aParameterNames.Add(Items[i].Name);
end;

function TModelParameters.GetParameterType(const aParameterName: string): TModelParameterValueType;
var
  p: TModelParameter;
begin
  p := GetParameterByName(aParameterName);
  if Assigned(p)
  then Result := p.ValueType
  else raise EListError.Create('Parameter '''+aParameterName+''' not found in getting parameter type');
end;

function TModelParameters.GetValue(const aParameterName: string): Variant;
var
  p: TModelParameter;
begin
  p := GetParameterByName(aParameterName);
  if Assigned(p)
  then Result := p.Value
  else raise EListError.Create('Parameter '''+aParameterName+''' not found in getting parameter value');
end;

function TModelParameters.ParameterExists(const aParameterName: string): Boolean;
begin
  Result := GetParameterByName(aParameterName)<>nil;
end;

procedure TModelParameters.Prepare(var aPayload: TByteBuffer);
var
  p: Integer;
begin
  aPayload.Prepare(Count);
  for p := 0 to Count - 1
  do Items[p].Prepare(aPayload);
end;

procedure TModelParameters.QWrite(var aPayload: TByteBuffer);
var
  p: Integer;
begin
  aPayload.QWrite(Count);
  for p := 0 to Count - 1
  do Items[p].QWrite(aPayload);
end;

procedure TModelParameters.Read(var aPayload: TByteBuffer);
var
  c: Integer;
  p: Integer;
  Parameter: TModelParameter;
begin
  aPayload.Read(c);
  for p := 0 to c - 1 do
  begin
    Parameter := TModelParameter.Create; // todo: using inherited constructor, NOT very nice but works in this case
    Parameter.Read(aPayload);
    Add(Parameter);
  end;
end;

procedure TModelParameters.SetValue(const aParameterName: string; const aValue: Variant);
var
  p: TModelParameter;
begin
  p := GetParameterByName(aParameterName);
  if Assigned(p)
  then p.Value := aValue
  else raise EListError.Create('Parameter '''+aParameterName+''' not found in setting parameter value');
end;

{ TModelEvent }

procedure TModelNewEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
  aPayload.Prepare(ModelName);
  aPayload.Prepare(Controller);
  aPayload.Prepare(Priority);
  aPayload.Prepare(State);
  aPayload.Prepare(Federation);
  aPayload.Prepare(ModelPrivateEventName);
  aPayload.Prepare(ControllerPrivateEventName);
end;

procedure TModelNewEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
  aPayload.QWrite(ModelName);
  aPayload.QWrite(Controller);
  aPayload.QWrite(Priority);
  aPayload.QWrite(State);
  aPayload.QWrite(Federation);
  aPayload.QWrite(ModelPrivateEventName);
  aPayload.QWrite(ControllerPrivateEventName);
end;

procedure TModelNewEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
  aPayload.Read(ModelName);
  aPayload.Read(Controller);
  aPayload.Read(Priority);
  aPayload.Read(State);
  aPayload.Read(Federation);
  aPayload.Read(ModelPrivateEventName);
  aPayload.Read(ControllerPrivateEventName);
end;

{ TModelDeleteEvent }

procedure TModelDeleteEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
end;

procedure TModelDeleteEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
end;

procedure TModelDeleteEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
end;

{ TModelChangeEvent }

procedure TModelChangeEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
  aPayload.Prepare(State);
  aPayload.Prepare(Federation);
end;

procedure TModelChangeEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
  aPayload.QWrite(State);
  aPayload.QWrite(Federation);
end;

procedure TModelChangeEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
  aPayload.Read(State);
  aPayload.Read(Federation);
end;

{ TModelInitEvent }

procedure TModelInitEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(LinkID);
  aPayload.Prepare(UID);
  aPayload.Prepare(ModelName);
  aPayload.Prepare(ModelPrivateEventName);
end;

procedure TModelInitEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(LinkID);
  aPayload.QWrite(UID);
  aPayload.QWrite(ModelName);
  aPayload.QWrite(ModelPrivateEventName);
end;

procedure TModelInitEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(LinkID);
  aPayload.Read(UID);
  aPayload.Read(ModelName);
  aPayload.Read(ModelPrivateEventName);
end;

{ TControllerNewEvent }

procedure TControllerNewEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
  aPayload.Prepare(Controller);
  aPayload.Prepare(ControllerPrivateEventName);
  aPayload.Prepare(Priority);
end;

procedure TControllerNewEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
  aPayload.QWrite(Controller);
  aPayload.QWrite(ControllerPrivateEventName);
  aPayload.QWrite(Priority);
end;

procedure TControllerNewEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
  aPayload.Read(Controller);
  aPayload.Read(ControllerPrivateEventName);
  aPayload.Read(Priority);
end;

{ TControllerChangeEvent }

procedure TControllerChangeEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
  aPayload.Prepare(Priority);
end;

procedure TControllerChangeEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
  aPayload.QWrite(Priority);
end;

procedure TControllerChangeEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
  aPayload.Read(Priority);
end;

{ TControllerModelSetupNewEvent }

procedure TControllerModelSetupNewEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
  aPayload.Prepare(Path);
  aPayload.Prepare(RunCount);
end;

procedure TControllerModelSetupNewEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
  aPayload.QWrite(Path);
  aPayload.QWrite(RunCount);
end;

procedure TControllerModelSetupNewEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
  aPayload.Read(Path);
  aPayload.Read(RunCount);
end;

{ TControllerModelSetupDeleteEvent }

procedure TControllerModelSetupDeleteEvent.Prepare(var aPayload: TByteBuffer);
begin
  aPayload.Prepare(UID);
  aPayload.Prepare(Path);
end;

procedure TControllerModelSetupDeleteEvent.QWrite(var aPayload: TByteBuffer);
begin
  aPayload.QWrite(UID);
  aPayload.QWrite(Path);
end;

procedure TControllerModelSetupDeleteEvent.Read(var aPayload: TByteBuffer);
begin
  aPayload.Read(UID);
  aPayload.Read(Path);
end;

{ TControllerFolderContentsEvent }

procedure TControllerFolderContentsEvent.Browse(aUID: Integer; const aRootModelFolder, aSubFolder: string);
var
  F: TSearchRec;
begin
  FUID := aUID;
  FSubFolder := AnsiString(aSubFolder);
  if FindFirst(aRootModelFolder+IncludeTrailingPathDelimiter(aSubFolder)+'*.*', faAnyFile, F)=0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory)<>0 then
        begin
          if (F.Name<>'.') and (F.Name<>'..')
          then FNames.AddObject(F.Name, Pointer(F.Attr));
        end
        else FNames.AddObject(F.Name, Pointer(F.Attr));
      until FindNext(F)<>0;
    finally
      FindClose(F);
    end;
  end;
end;

constructor TControllerFolderContentsEvent.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
end;

destructor TControllerFolderContentsEvent.Destroy;
begin
  FreeAndNil(FNames);
  inherited Destroy;
end;

procedure TControllerFolderContentsEvent.Prepare(var aPayload: TByteBuffer);
var
  n: Integer;
begin
  aPayload.Prepare(FUID);
  aPayload.Prepare(FSubFolder);
  aPayload.Prepare(FNames.Count);
  for n := 0 to FNames.Count - 1 do
  begin
    aPayload.Prepare(AnsiString(FNames[n]));
    aPayload.Prepare(Integer(FNames.Objects[n]));
  end;
end;

procedure TControllerFolderContentsEvent.QWrite(var aPayload: TByteBuffer);
var
  n: Integer;
begin
  aPayload.QWrite(FUID);
  aPayload.QWrite(FSubFolder);
  aPayload.QWrite(FNames.Count);
  for n := 0 to FNames.Count - 1 do
  begin
    aPayload.QWrite(AnsiString(FNames[n]));
    aPayload.QWrite(Integer(FNames.Objects[n]));
  end;
end;

procedure TControllerFolderContentsEvent.Read(var aPayload: TByteBuffer);
var
  Count: Integer;
  n: Integer;
  Name: AnsiString;
  Attr: Integer;
begin
  aPayload.Read(FUID);
  aPayload.Read(FSubFolder);
  aPayload.Read(Count);
  for n := 0 to Count - 1 do
  begin
    aPayload.Read(Name);
    aPayload.Read(Attr);
    FNames.AddObject(string(Name), Pointer(Attr));
  end;
end;

{ TMCModelStarter }

constructor TMCModelStarter.Create(const aModelName: string; aModelID: Integer);
var
  RemoteHost: string;
  RemotePort: Integer;
  ControllersEventName: string;
  ControllerPrivateEventName: string;
  LinkID: Int64;
  ModelName: string;
  ModelID: Integer;
begin
  inherited Create;
  FModelThread := nil;
  // init parameters
  RemoteHost := GetSetting(RemoteHostSwitch, DefaultRemoteHost);
  RemotePort := GetSetting(RemotePortSwitch, DefaultRemotePort);
  FIdleFederation := GetSetting(IdleFederationSwitch, DefaultIdleFederation);
  FController := AnsiString(CommandLine.GetSwitchDef(ControllerSwitch, GetComputerNameStr));
  ControllersEventName := CommandLine.GetSwitchDef(ControllersEventNameSwitch, FIdleFederation+'.'+ControllersRootEventName);
  ControllerPrivateEventName := CommandLine.GetSwitchDef(ControllerPrivateEventNameSwitch, ControllersEventName+'.'+string(FController));
  LinkID := StrToInt64Def(CommandLine.GetSwitch(LinkIDSwitch), 0);
  if aModelName<>'' then
  begin
    ModelName := aModelName;
    if aModelID<>0
    then ModelID := aModelID
    else ModelID := GetSetting(ModelIDSwitch, DefaultModelID);
  end
  else
  begin
    ModelName := GetSetting(ModelNameSwitch, DefaultModelName);
    ModelID := GetSetting(ModelIDSwitch, DefaultModelID);
  end;
  Log.WriteLn('IMB '+RemoteHost+':'+IntToStr(RemotePort));
  Log.WriteLn('Controller '+string(FController));
  Log.WriteLn('ControllersEventName '+ControllersEventName);
  Log.WriteLn('ControllerPrivateEventName '+ControllerPrivateEventName);
  Log.WriteLn('LinkID '+IntToStr(LinkID));
  Log.WriteLn('ModelName '+ModelName);
  Log.WriteLn('ModelID '+IntToStr(ModelID));
  FQuitApplicationEvent := TEvent.Create(nil, False, False, '');
  // init model control
  FConnection := TIMBConnection.Create(RemoteHost, RemotePort, ModelName, ModelID, '');
  FConnection.KeepAlive := True;
  FPrivateModelEvent := FConnection.Subscribe(
    ControllerPrivateEventName+EventNamePartSeperator+
    ModelName+EventNamePartSeperator+IntToHex(FConnection.UniqueClientID, 8), False);
  FPrivateModelEvent.OnNormalEvent := HandleControlEvents;
  FControllersEvent := FConnection.Subscribe(ControllersEventName, False);
  FControllersEvent.OnNormalEvent := HandleControlEvents;
  FPrivateControllerEvent := FConnection.Subscribe{Publish}(ControllerPrivateEventName, False);
  FPrivateControllerEvent.OnNormalEvent := HandleControlEvents;
  // signal process id and client id etc. to controller
  Log.WriteLn('UID '+IntToHex(FConnection.UniqueClientID, 8));
  SignalModelInit(LinkID, AnsiString(ModelName));
  // signal state as idle
  FState := msIdle;
  FProgress := 0;
  FPriority := GetSetting(ModelPrioritySwitch, DefaultModelPriority);
  SignalModelNew('');
  if not FConnection.Connected
  then Log.WriteLn('IMB is not connected!', llError);
end;

destructor TMCModelStarter.Destroy;
begin
  FreeAndNil(FConnection);
  FreeAndNil(FQuitApplicationEvent);
  inherited;
end;

procedure TMCModelStarter.DoParameterRequest(aParameters: TModelParameters);
begin
  if Assigned(OnParameterRequest) then
  begin
    try
      OnParameterRequest(Self, FConnection, aParameters);
    except
      on E: Exception
      do Log.WriteLn('Exception in TMCModelStarter.OnParameterRequest: '+E.Message, llError);
    end;
  end;
end;

procedure TMCModelStarter.DoQuitApplication;
begin
  if Assigned(OnQuitApplication)
  then OnQuitApplication(Self, FConnection);
  SignalModelExit;
  FQuitApplicationEvent.SetEvent;
end;

procedure TMCModelStarter.DoRequestModels(const aReturnEventName: string);
begin
  SignalModelNew(aReturnEventName);
  if Assigned(FOnRequestModels)
  then OnRequestModels(Self, aReturnEventName);
end;

procedure TMCModelStarter.DoStartModel(aParameters: TModelParameters);
var
  FederationOk: Boolean;
begin
  if aParameters.ParameterExists(FederationParameterName)
  then FConnection.Federation := aParameters.Value[FederationParameterName];
  // avoid setting federation change handling without federation being set (event is not switching to new federation when not prefixed)
  FederationOk := FConnection.Federation<>'';
  if FederationOk
  then FConnection.OnChangeFederation := HandleChangeFederation;
  FreeAndNil(FModelThread);
  SignalModelState(msBusy); // default signal busy state
  try
    if Assigned(OnStartModel)
    then OnStartModel(Self, FConnection, aParameters, FModelThread);
    // retry setting federation change handler (hopefully it is set in OnStartModel)
    if (FConnection.Federation<>'') and not FederationOk
    then FConnection.OnChangeFederation := HandleChangeFederation;
  except
    on E: Exception
    do Log.WriteLn('Exception in TMCModelStarter.DoStartModel: '+E.Message, llError);
  end;
end;

procedure TMCModelStarter.DoStopModel;
begin
  try
    SignalModelState(msBusy);
    if Assigned(OnStopModel)
    then OnStopModel(Self, FConnection, FModelThread);
    FreeAndNil(FModelThread);
    SignalModelProgress(0);
    SignalModelState(msIdle);
  except
    on E: Exception
    do Log.WriteLn('Exception in TMCModelStarter.DoStopModel: '+E.Message, llError);
  end;
end;

procedure TMCModelStarter.HandleChangeFederation(aConnection: TIMBConnection; aNewFederationID: Integer;
  const aNewFederation: string);
var
  federation: string;
begin
  federation := aNewFederation;
  if Assigned(FOnChangeFederation)
  then OnChangeFederation(Self, aConnection, aNewFederationID, federation);
  if federation<>''
  then FConnection.Federation := federation;
end;

procedure TMCModelStarter.HandleControlEvents(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  Command: Integer;
  Parameters: TModelParameters;
  ReturnEventName: AnsiString;
  ParameterPayload: TByteBuffer;
  UID: Integer;
  Action: Integer;
  ModelChange: TModelChangeEvent;
begin
  aPayload.Read(Command);
  case Command of
    mcModel:
      begin
        aPayload.Read(Action);
        case Action of
          actionChange:
            begin // lock or set back to idle
              ModelChange.Read(aPayload);
              if ModelChange.UID=FConnection.UniqueClientID then
              begin
                case ModelChange.State of
                  Ord(msLock):
                    begin
                      if FState=msIdle then
                      begin
                        Connection.Federation := string(ModelChange.Federation);
                        FState := msLock;
                      end
                      else Log.WriteLn('Recevied lock request while not idle ('+IntToStr(Ord(FState))+')', llWarning);
                    end;
                  Ord(msIdle):
                    begin
                      if FState=msLock then
                      begin
                        Connection.Federation := string(ModelChange.Federation);
                        FState := msIdle;
                      end
                      else Log.WriteLn('Recevied unlock request while not locked ('+IntToStr(Ord(FState))+')', llWarning);
                    end;
                else
                  Log.WriteLn('Received unsupported external model state change '+IntToStr(Ord(FState))+' -> '+IntToStr(ModelChange.State), llWarning);
                end;
              end;
            end;
        end;
      end;
    mcRequestDefaultParameters:
      begin
        aPayload.Read(ReturnEventName);
        Parameters := TModelParameters.Create;
        try
          // read default parameters from request if available
          if aPayload.ReadAvailable>0
          then Parameters.Read(aPayload);
          DoParameterRequest(Parameters);
          // build and send ParameterPayload
          ParameterPayload.Clear;
          ParameterPayload.Prepare(Integer(mcDefaultParameters));
          ParameterPayload.Prepare(FConnection.UniqueClientID);
          Parameters.Prepare(ParameterPayload);
          ParameterPayload.PrepareApply;
          ParameterPayload.QWrite(Integer(mcDefaultParameters));
          ParameterPayload.QWrite(FConnection.UniqueClientID);
          Parameters.QWrite(ParameterPayload);
          FConnection.SignalEvent(string(ReturnEventName), ekNormalEvent, ParameterPayload, False);
        finally
          Parameters.Free;
        end;
      end;
    mcModelClaim:
      begin
        aPayload.Read(UID);
        if (UID=FConnection.UniqueClientID) and Assigned(OnStartModel) then
        begin // claim model
          Parameters := TModelParameters.Create;
          try
            Parameters.Read(aPayload);
            DoStartModel(Parameters);
          finally
            Parameters.Free;
          end;
        end;
      end;
    mcModelUnClaim:
      begin // unclaim model
        aPayload.Read(UID);
        if UID=FConnection.UniqueClientID
        then DoStopModel;
      end;
    mcModelQuitApplication:
      begin
        aPayload.Read(UID);
        if UID=FConnection.UniqueClientID
        then DoQuitApplication; // quit model starter (complete application)
      end;
    mcRequestModels:
      begin
        aPayload.Read(ReturnEventName);
        DoRequestModels(string(ReturnEventName));
      end;
  end;
end;

procedure TMCModelStarter.SetOnChangeFederationNoObj(aSelf: Pointer; const aValue: TMSOnChangeFederationNoObj);
var
  F: TMSOnChangeFederation;
begin
  // recalc eventhandler and self to method
  TMethod(F).Code := @aValue;
  TMethod(F).Data := aSelf;
  FOnChangeFederation := F;
end;

procedure TMCModelStarter.SetOnParameterRequestNoObj(aSelf: Pointer; const aValue: TMSOnParameterRequestNoObj);
var
  F: TMSOnParameterRequest;
begin
  // recalc eventhandler and self to method
  TMethod(F).Code := @aValue;
  TMethod(F).Data := aSelf;
  FOnParameterRequest := F;
end;

procedure TMCModelStarter.SetOnQuitApplicationNoObj(aSelf: Pointer; const aValue: TMSOnQuitApplicationNoObj);
var
  F: TMSOnQuitApplication;
begin
  // recalc eventhandler and self to method
  TMethod(F).Code := @aValue;
  TMethod(F).Data := aSelf;
  FOnQuitApplication := F;
end;

procedure TMCModelStarter.SetOnRequestModelsNoObj(aSelf: Pointer; const aValue: TMSOnRequestModelsNoObj);
var
  F: TMSOnRequestModels;
begin
  // recalc eventhandler and self to method
  TMethod(F).Code := @aValue;
  TMethod(F).Data := aSelf;
  FOnRequestModels := F;
end;

procedure TMCModelStarter.SetOnStartModelNoObj(aSelf: Pointer; const aValue: TMSOnStartModelNoObj);
var
  F: TMSOnStartModel;
begin
  // recalc eventhandler and self to method
  TMethod(F).Code := @aValue;
  TMethod(F).Data := aSelf;
  FOnStartModel := F;
end;

procedure TMCModelStarter.SetOnStopModelNoObj(aSelf: Pointer; const aValue: TMSOnStopModelNoObj);
var
  F: TMSOnStopModel;
begin
  // recalc eventhandler and self to method
  TMethod(F).Code := @aValue;
  TMethod(F).Data := aSelf;
  FOnStopModel := F;
end;

procedure TMCModelStarter.SignalModelExit;
var
  Payload: TByteBuffer;
begin
  if Assigned(FConnection) and Assigned(FControllersEvent) then
  begin
    Payload.Clear;
    Payload.Prepare(Integer(mcModel));
    Payload.Prepare(Integer(actionDelete));
    Payload.Prepare(FConnection.UniqueClientID);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModel));
    Payload.QWrite(Integer(actionDelete));
    Payload.QWrite(FConnection.UniqueClientID);
    FControllersEvent.SignalEvent(ekNormalEvent, Payload);
  end;
end;

procedure TMCModelStarter.SignalModelInit(aLinkID: Int64; const aModelName: AnsiString);
var
  Payload: TByteBuffer;
  ModelInitEvent: TModelInitEvent;
begin
  ModelInitEvent.LinkID := aLinkID;
  ModelInitEvent.UID := FConnection.UniqueClientID;
  ModelInitEvent.ModelName := aModelName;
  ModelInitEvent.ModelPrivateEventName := AnsiString(FPrivateModelEvent.EventName);
  Payload.Clear;
  Payload.Prepare(Integer(mcModelInit));
  ModelInitEvent.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModelInit));
  ModelInitEvent.QWrite(Payload);
  FPrivateControllerEvent.SignalEvent(ekNormalEvent, Payload);
end;

procedure TMCModelStarter.SignalModelNew(const aEventName: string);
var
  CommandPayload: TModelNewEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.ModelName := AnsiString(FConnection.OwnerName);
  CommandPayload.Controller := AnsiString(FController);
  CommandPayload.Priority := FPriority;
  CommandPayload.State := Ord(FState);
  CommandPayload.Federation := AnsiString(FConnection.Federation);
  CommandPayload.ModelPrivateEventName := AnsiString(FPrivateModelEvent.EventName);
  CommandPayload.ControllerPrivateEventName := AnsiString(FPrivateControllerEvent.EventName);
  Payload.Clear;
  Payload.Prepare(Integer(mcModel));
  Payload.Prepare(Integer(actionNew));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModel));
  Payload.QWrite(Integer(actionNew));
  CommandPayload.QWrite(Payload);
  if aEventName=''
  then FControllersEvent.SignalEvent(ekNormalEvent, Payload)
  else FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
  // signal progress if set (ie neq 0)
  if FProgress<>0 then
  begin
    Payload.Clear;
    Payload.Prepare(Integer(mcModelProgress));
    Payload.Prepare(FConnection.UniqueClientID);
    Payload.Prepare(FProgress);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModelProgress));
    Payload.QWrite(FConnection.UniqueClientID);
    Payload.QWrite(FProgress);
    if aEventName=''
    then FControllersEvent.SignalEvent(ekNormalEvent, Payload)
    else FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
  end;
end;

procedure TMCModelStarter.SignalModelProgress(aProgress: Integer);
var
  Payload: TByteBuffer;
begin
  if Assigned(Self) then
  begin
    if FProgress<>aProgress then
    begin
      Payload.Clear;
      Payload.Prepare(Integer(mcModelProgress));
      Payload.Prepare(FConnection.UniqueClientID);
      Payload.Prepare(aProgress);
      Payload.PrepareApply;
      Payload.QWrite(Integer(mcModelProgress));
      Payload.QWrite(FConnection.UniqueClientID);
      Payload.QWrite(aProgress);
      FControllersEvent.SignalEvent(ekNormalEvent, Payload);
      FProgress := aProgress;
    end;
  end;
end;

procedure TMCModelStarter.SignalModelState(aState: Integer);
begin
  SignalModelState(TModelState(aState));
end;

procedure TMCModelStarter.SignalModelState(aState: TModelState; const aFederation: string);
var
  ModelChange: TModelChangeEvent;
  Payload: TByteBuffer;
begin
  if Assigned(Self) and (FState<>aState) then
  begin
    ModelChange.UID := FConnection.UniqueClientID;
    ModelChange.State := Ord(aState);
    ModelChange.Federation := AnsiString(aFederation);
    Payload.Clear;
    Payload.Prepare(Integer(mcModel));
    Payload.Prepare(Integer(actionChange));
    ModelChange.Prepare(Payload);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModel));
    Payload.QWrite(Integer(actionChange));
    ModelChange.QWrite(Payload);
    FControllersEvent.SignalEvent(ekNormalEvent, Payload);
    //Log.WriteLn('New model state '+IntToStr(Ord(FState))+' -> '+IntToStr(Ord(aState))+' on '+aFederation);
    FState := aState;
  end;
end;

procedure TMCModelStarter.SignalModelState(aState: TModelState);
begin
  if Assigned(FConnection)
  then SignalModelState(aState, FConnection.Federation);
end;

{ TModelEntry }

constructor TCIModelEntry.Create(const aModelName: string; aState: TModelState);
begin
  inherited Create;
  FModelName := aModelName;
  FState := aState;
  FProgress := 0;
end;

{ TModelEntryEx }

constructor TCIModelEntryEx.Create(aUID: Integer; const aModelName: string; const aController: string;
  aPriority: Integer; aState: TModelState;
  const aFederation, aModelPrivateEventName, aControllerPrivateEventName: string);
begin
  inherited Create;
  FUID := aUID;
  FModelName := aModelName;
  FController := aController;
  FPriority := aPriority;
  FState := aState;
  FProgress := 0;
  FFederation := aFederation;
  FModelPrivateEventName := aModelPrivateEventName;
  FControllerPrivateEventName := aControllerPrivateEventName;
  FTag := 0;
  FLinkedModelEntry := nil;
end;

function TCIModelEntryEx.CreateCache(aIsThisSession: Boolean): TModelCache;
begin
  Result.FUID := FUID;
  Result.FModelName := FModelName;
  Result.FController := FController;
  Result.FPriority := FPriority;
  Result.FState := FState;
  Result.FProgress := FProgress;
  Result.FFederation := FFederation;
  Result.FIsThisSession := aIsThisSession;
end;

function TCIModelEntryEx.CreateCache(aState: TModelState): TModelCache;
begin
  Result.FUID := FUID;
  Result.FModelName := FModelName;
  Result.FController := FController;
  Result.FPriority := FPriority;
  Result.FState := aState;
  Result.FProgress := FProgress;
  Result.FFederation := FFederation;
  Result.FIsThisSession := False;
end;

destructor TCIModelEntryEx.Destroy;
begin
  FLinkedModelEntry := nil;
  FTag := 0;
  inherited;
end;

{ TModelRunTime }

constructor TCTModelRunTime.Create(const aPath, aRemoteHost: string; aRemotePort: Integer;const aIdleFederation: string;
    const aController, aControllersEventName, aControllerPrivateEventName: string;
    aControllersEvent: TIMBEventEntry; aPriority: Integer; aCUDAWorkARound: Boolean);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  LocalPath: string;
  Err: DWORD;
  s: string;
{
  todo: for cuda
  svcToken: THandle;
  provToken: THandle;
  SessionID: DWORD;
}
begin
  inherited Create;
  FControllersEvent := aControllersEvent;
  FUniqueClientID := 0;
  FState := Ord(msIdle);
  FFederation := '';
  FModelName := '';
  FIdleTimeLeft := ModelIdleKillTime;

  if not Assigned(Self)
  then Log.WriteLn('Self=nil in TCTModelRunTime.Create', llError);

  LocalPath := aPath+' '+'/'+RemoteHostSwitch+'="'+aRemoteHost+'" /'+RemotePortSwitch+'='+IntToStr(aRemotePort)+' '+
    '/'+IdleFederationSwitch+'="'+aIdleFederation+'" '+'/'+ControllerSwitch+'="'+aController+'" '+
    '/'+ControllersEventNameSwitch+'="'+aControllersEventName+'" '+
    '/'+ControllerPrivateEventNameSwitch+'="'+aControllerPrivateEventName+'" '+
    '/'+ModelPrioritySwitch+'='+IntToStr(aPriority)+' '+
    '/'+LinkIDSwitch+'='+IntToStr(LinkID);
  // start process
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOW;
  if not CreateProcess(
    nil,
    PChar(LocalPath), { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    false, { handle inheritance flag }
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
    nil, { pointer to new environment block }
    PChar(ExtractFileDir(aPath)), { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo) then
  begin
    Err := GetLastError; { pointer to PROCESS_INF }
    s := SysErrorMessage(Err);
    Log.WriteLn('Could not create process "'+LocalPath+'": '+s, llError);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    FProcessHandle := INVALID_HANDLE_VALUE;
    FRTState := rtsTerminated;
  end
  else
  begin
    Log.WriteLn('Started model: ('+IntToStr(LinkID)+') '+aPath, llNormal, 2);
    FProcessHandle := ProcessInfo.hProcess;
    CloseHandle(ProcessInfo.hThread);
    FRTState := rtsRunning;
  end;
end;

destructor TCTModelRunTime.Destroy;
begin
  if FRTState<>rtsTerminated
  then Terminate;
  if Assigned(FPrivateEvent)
  then FPrivateEvent.Connection.UnPublish(FPrivateEvent.EventName, False);
  inherited;
end;

procedure TCTModelRunTime.Unregister(aReceivedQuit: Boolean);
begin
  SignalModelExit;
  if aReceivedQuit
  then Log.WriteLn('Unregistered '+IntToStr(LinkID), llNormal, 2)
  else Log.WriteLn('Unregistered '+IntToStr(LinkID), llWarning, 2);
  FRTState := rtsTerminated;
end;

function TCTModelRunTime.LinkID: NativeInt;
begin
  Result := NativeInt(Self);
end;

function TCTModelRunTime.SignalModelExit: Boolean;
var
  Payload: TByteBuffer;
begin
  if FUniqueClientID<>0 then
  begin
    Payload.Clear;
    Payload.Prepare(Integer(mcModel));
    Payload.Prepare(Integer(actionDelete));
    Payload.Prepare(FUniqueClientID);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModel));
    Payload.QWrite(Integer(actionDelete));
    Payload.QWrite(FUniqueClientID);
    FControllersEvent.SignalEvent(ekNormalEvent, Payload);
    Result := True;
  end
  else Result := False;
end;

procedure TCTModelRunTime.SignalModelQuitApplication;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(mcModelQuitApplication));
  Payload.Prepare(FUniqueClientID);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModelQuitApplication));
  Payload.QWrite(FUniqueClientID);
  FPrivateEvent.SignalEvent(ekNormalEvent, Payload); // auto publish?
  FRTState := rtsClosing;
  Log.WriteLn('SignalModelQuit '+IntToStr(LinkID), llNormal, 2);
end;

function TCTModelRunTime.Terminate: Boolean;
begin
  if FProcessHandle<>INVALID_HANDLE_VALUE then
  begin
    if TerminateProcess(FProcessHandle, 0) then
    begin
      Result := True;
      Log.WriteLn('Terminated '+IntToStr(LinkID), llWarning, 2);
    end
    else
    begin
      Log.WriteLn('Could not terminate process '+IntToStr(FProcessHandle)+' (or already terminated): '+SysErrorMessage(GetLastError()), llError);
      Result := False;
    end;
    // or ExitProcess keep alive and pickup on next run of this controller?
    // cannot relink processes so kill now
    CloseHandle(FProcessHandle);
    FProcessHandle := INVALID_HANDLE_VALUE;
    Unregister(False);
  end
  else Result := True;
  FRTState := rtsTerminated; // if it didn't work we cannot do anything about it..
end;

{ TModelSetup }

constructor TCTModelSetup.Create(const aPath: string; aRunCount: Integer; aCudaWorkARound: Boolean);
begin
  inherited Create;
  FPath := aPath;
  FRunCount := aRunCount;
  FProcesses := TObjectList<TCTModelRunTime>.Create;
  FCudaWorkARound := aCudaWorkARound;
end;

destructor TCTModelSetup.Destroy;
begin
  FreeAndNil(FProcesses);
  inherited;
end;

procedure TCTModelSetup.RemoveSetup;
begin
  StandardIni.DeleteKey(ModelsSection, FPath);
end;

procedure TCTModelSetup.SaveSetup;
begin
  StandardIni.WriteInteger(ModelsSection, FPath, RunCount);
end;

procedure TCTModelSetup.SetRunCount(const aValue: Integer);
begin
  if FRunCount<>aValue then
  begin
    FRunCount := aValue;
    SaveSetup;
  end;
end;

{ TNodeControllerThread }

constructor TNodeControllerThread.Create(aConnection: TIMBConnection);
var
  ModelPaths: TStringList;
  m: Integer;
  ModelSetup: TCTModelSetup;
  ModelStartupInfo: string;
  p: Integer;
  RunCount: Integer;
  CudaWorkARound: Boolean;
begin
  inherited Create(False);
  FConnection := aConnection;
  FController := GetComputerNameStr;
  FRootModelFolder := ExpandFileName(GetSetting(RootModelFolderSwitch, ExtractFileDir(ParamStr(0))));
  Log.WriteLn('Using RootModelFolder '+FRootModelFolder);
  FControllersEvent := aConnection.Subscribe(ControllersRootEventName);
  FControllersEvent.OnNormalEvent := HandleControllerEvent;
  FPrivateEvent := aConnection.Subscribe(FControllersEvent.EventName+EventNamePartSeperator+
    FController+EventNamePartSeperator+IntToHex(aConnection.UniqueClientID, 8), False);
  FPrivateEvent.OnNormalEvent := HandleControllerEvent;
  FPriority := GetSetting(ControllerPrioritySwitch, DefaultControllerPriority);
  FModelsLock := TCriticalSection.Create;
  FModels := TObjectList<TCTModelSetup>.Create;
  // sign up this controller
  SignalControllerNew(FControllersEvent.EventName);
  // read basic controller setup and start models
  ModelPaths := TStringList.Create;
  try
    StandardIni.ReadSection(ModelsSection, ModelPaths);
    for m := 0 to ModelPaths.Count - 1 do
    begin
      ModelStartupInfo := StandardIni.ReadString(ModelsSection, ModelPaths[m], '');
      p := Pos(',', ModelStartupInfo);
      if p<>0 then
      begin
        RunCount := StrToIntDef(Trim(Copy(ModelStartupInfo, 1, p-1)), 0);
        CudaWorkARound := StrToIntDef(Trim(Copy(ModelStartupInfo, p+1, Length(ModelStartupInfo)-p)), 0)<>0;
      end
      else
      begin
        RunCount := StrToIntDef(Trim(ModelStartupInfo), 0);
        CudaWorkARound := False;
      end;
      ModelSetup := TCTModelSetup.Create(ModelPaths[m], RunCount, CudaWorkARound);
      FModels.Add(ModelSetup);
      SignalControllerModelSetupNew(FControllersEvent.EventName, ModelSetup);
    end;
  finally
    ModelPaths.Free;
  end;
end;

function TNodeControllerThread.DeleteModelRunTimeOnUniqueClientID(aUniqueClientID: Integer): Boolean;
var
  m: Integer;
  p: Integer;
begin
  Result := False;
  m := FModels.Count-1;
  while (m>=0) and not Result do
  begin
    p := FModels[m].FProcesses.Count-1;
    while (p>=0) and not Result do
    begin
      if FModels[m].FProcesses[p].FUniqueClientID=aUniqueClientID then
      begin
        FModels[m].FProcesses[p].Unregister(True);
        FModels[m].FProcesses.Delete(p);
        Result := True;
      end
      else p := p-1;
    end;
    m := m-1;
  end;
end;

destructor TNodeControllerThread.Destroy;
begin
  SignalControllerExit;
  FreeAndNil(FModels);
  FreeAndNil(FModelsLock);
  inherited;
end;

procedure TNodeControllerThread.Execute;
var
  m: Integer;
  Model: TCTModelSetup;
  p: Integer;
  EC: DWORD;
  locCount: Integer;
  Err: DWORD;
begin
  // process monitoring
  while not Terminated do
  begin
    // check all model instances
    FModelsLock.Acquire;
    try
      try
        for m := 0 to FModels.Count - 1 do
        begin
          Model := FModels[m];
          if Model.FProcesses.Count<>Model.FRunCount then
          begin
            if Model.FProcesses.Count>Model.FRunCount then
            begin
              // cleanup processes that are marked msRemoved
              for p := Model.FProcesses.Count - 1 downto 0 do
              begin
                if Model.FProcesses[p].FState = Ord(msRemoved) then
                begin
                  if GetExitCodeProcess(Model.FProcesses[p].FProcessHandle, EC) then
                  begin
                    if EC<>STILL_ACTIVE then
                    begin
                      // seems terminated, remove entry
                      Log.WriteLn('Removing hanging process '+IntToStr(Model.FProcesses[p].LinkID), llWarning);
                      Model.FProcesses[p].Unregister(False);
                      Model.FProcesses.Delete(p);
                    end
                    else
                    begin
                      // something wrong with process: kill?
                      Model.FProcesses.Delete(p);
                    end;
                  end
                  else
                  begin
                    Err := GetLastError;
                    Log.WriteLn('Could not retrieve exit code for '+IntToStr(Model.FProcesses[p].LinkID)+': '+SysErrorMessage(Err), llError);
                    // cannot find process
                    Model.FProcesses[p].Unregister(False);
                    Model.FProcesses.Delete(p);
                  end;
                end;
              end;
              // check again but now in a cleaned up list
              if Model.FProcesses.Count>Model.FRunCount then
              begin
                // remove processes that are idle
                p := Model.FProcesses.Count - 1;
                locCount := Model.FProcesses.Count;
                while (p>=0) and (locCount>Model.FRunCount) do
                begin
                  if Model.FProcesses[p].FState=Ord(msIdle) then
                  begin
                    Model.FProcesses[p].SignalModelQuitApplication;
                    Model.FProcesses[p].FState := Ord(msRemoved);
                    locCount := locCount-1;
                  end;
                  p := p-1;
                end;
              end;
            end
            else
            begin // create new processes
              while Model.FProcesses.Count<Model.FRunCount do
              begin
                Model.FProcesses.Add(TCTModelRunTime.Create(
                  FRootModelFolder+Model.FPath,
                  FConnection.RemoteHost, FConnection.RemotePort, FConnection.Federation,
                  FController, FControllersEvent.EventName, FPrivateEvent.EventName, FControllersEvent, FPriority, Model.FCudaWorkARound));
              end;
            end;
          end
          else
          begin // check running processes
            p := Model.FProcesses.Count - 1;
            while p>=0 do
            begin
              if GetExitCodeProcess(Model.FProcesses[p].FProcessHandle, EC) then
              begin
                if EC<>STILL_ACTIVE then
                begin
                  // seems terminated, remove entry
                  Model.FProcesses.Delete(p);
                end;
              end
              else
              begin
                // something wrong with process: kill?
                Model.FProcesses.Delete(p);
              end;
              //check if process is Idle or Ready too long. Just kill it then.
              if (Model.FProcesses[p].FState=Ord(msIdle)) or
                 (Model.FProcesses[p].FState=Ord(msReady)) then
              begin
                Dec(Model.FProcesses[p].FIdleTimeLeft);
                if Model.FProcesses[p].FIdleTimeLeft<=0 then
                begin
                  Log.WriteLn('Watchdog kill of Model '+ string(Model.FProcesses[p].FModelName) + '.');
                  Model.FProcesses.Delete(p);
                end;
              end
              else Model.FProcesses[p].FIdleTimeLeft := ModelIdleKillTime;
              p := p-1;
            end;
          end;
        end;
      except
        on E: Exception
        do Log.WriteLn('Exception in TNodeControllerThread.Execute: '+E.Message, llError);
      end;
    finally
      FModelsLock.Release;
    end;
    Sleep(ControllerLoopWaitTime);
  end;
end;

function TNodeControllerThread.GetModelOnPath(const aPath: string): TCTModelSetup;
var
  m: Integer;
begin
  m := FModels.Count-1;
  while (m>=0) and (AnsiCompareText(FModels[m].FPath, aPath)<>0)
  do m := m-1;
  if m>=0
  then Result := FModels[m]
  else Result := nil;
end;

function TNodeControllerThread.GetModelRunTimeOnLinkID(aLinkID: NativeInt): TCTModelRunTime;
var
  m: Integer;
  p: Integer;
begin
  Result := nil;
  m := FModels.Count-1;
  while (m>=0) and not Assigned(Result) do
  begin
    p := FModels[m].FProcesses.Count-1;
    while (p>=0) and not Assigned(Result)  do
    begin
      if FModels[m].FProcesses[p].LinkID=aLinkID
      then Result := FModels[m].FProcesses[p]
      else p := p-1;
    end;
    m := m-1;
  end;
end;

function TNodeControllerThread.GetModelRunTimeOnUniqueClientID(aUniqueClientID: Integer): TCTModelRunTime;
var
  m: Integer;
  p: Integer;
begin
  Result := nil;
  m := FModels.Count-1;
  while (m>=0) and not Assigned(Result) do
  begin
    p := FModels[m].FProcesses.Count-1;
    while (p>=0) and not Assigned(Result)  do
    begin
      if FModels[m].FProcesses[p].FUniqueClientID=aUniqueClientID
      then Result := FModels[m].FProcesses[p]
      else p := p-1;
    end;
    m := m-1;
  end;
end;

procedure TNodeControllerThread.HandleControllerEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  ModelCommand: Integer;
  ReturnEventName: AnsiString;
  m: Integer;
  Model: TCTModelSetup;
  ControllerChange: TControllerChangeEvent;
  ControllerModelSetupNew: TControllerModelSetupNewEvent;
  ControllerModelSetupDelete: TControllerModelSetupDeleteEvent;
  SubFolder: AnsiString;
  ReturnPayload: TByteBuffer;
  ControllerFolderContents: TControllerFolderContentsEvent;
  ModelRunTime: TCTModelRunTime;
  ModelInitEvent: TModelInitEvent;
  Action: Integer;
  ModelChange: TModelChangeEvent;
  ModelDelete: TModelDeleteEvent;
begin
  aPayload.Read(ModelCommand);
  case ModelCommand of
    mcRequestControllers:
      begin
        aPayload.Read(ReturnEventName);
        SignalControllerNew(string(ReturnEventName));
      end;
    mcControllerChange:
      begin
        ControllerChange.Read(aPayload);
        if FConnection.UniqueClientID=ControllerChange.UID then
        begin
          if FPriority<>ControllerChange.Priority then
          begin
            FPriority := ControllerChange.Priority;
            SignalControllerChange(FControllersEvent.EventName);
          end;
        end;
      end;
    mcRequestControllerModelSetups:
      begin
        aPayload.Read(ReturnEventName);
        FModelsLock.Acquire;
        try
          for m := 0 to FModels.Count - 1
          do SignalControllerModelSetupNew(string(ReturnEventName), FModels[m]);
        finally
          FModelsLock.Release;
        end;
      end;
    mcControllerModelSetupChange,
    mcControllerModelSetupNew:
      begin
        ControllerModelSetupNew.Read(aPayload);
        if FConnection.UniqueClientID=ControllerModelSetupNew.UID then
        begin
          FModelsLock.Acquire;
          try
            Model := GetModelOnPath(string(ControllerModelSetupNew.Path));
            if Assigned(Model) then
            begin
              Model.RunCount := ControllerModelSetupNew.RunCount;
              Log.WriteLn('change of model run count of '+IntToStr(ControllerModelSetupNew.RunCount)+' for '+string(ControllerModelSetupNew.Path));
              SignalControllerModelSetupChange(FControllersEvent.EventName, Model);
            end
            else
            begin
              Model := TCTModelSetup.Create(string(ControllerModelSetupNew.Path), ControllerModelSetupNew.RunCount, False{ todo: ControllerModelSetupNew.CudaWorkARound });
              Model.SaveSetup;
              FModels.Add(Model);
              Log.WriteLn('new model with run count of '+IntToStr(ControllerModelSetupNew.RunCount)+' for '+string(ControllerModelSetupNew.Path));
              SignalControllerModelSetupNew(FControllersEvent.EventName, Model);
            end;
          finally
            FModelsLock.Release;
          end;
        end;
      end;
    mcControllerModelSetupDelete:
      begin
        ControllerModelSetupDelete.Read(aPayload);
        if FConnection.UniqueClientID=ControllerModelSetupDelete.UID then
        begin
          FModelsLock.Acquire;
          try
            Model := GetModelOnPath(string(ControllerModelSetupDelete.Path));
            if Assigned(Model) then
            begin
              Model.RemoveSetup;
              SignalControllerModelSetupDelete(FControllersEvent.EventName, Model);
              FModels.Remove(Model);
            end;
          finally
            FModelsLock.Release;
          end;
        end;
      end;
    mcRequestFolderContents:
      begin
        aPayload.Read(SubFolder);
        aPayload.Read(ReturnEventName);
        // check for redirections in path
        if Pos('..', string(SubFolder))=0 then
        begin
          ControllerFolderContents := TControllerFolderContentsEvent.Create;
          try
            ControllerFolderContents.Browse(FConnection.UniqueClientID, FRootModelFolder, string(SubFolder));
            ReturnPayload.Clear;
            ReturnPayload.Prepare(Integer(mcFolderContents));
            ControllerFolderContents.Prepare(ReturnPayload);
            ReturnPayload.PrepareApply;
            ReturnPayload.QWrite(Integer(mcFolderContents));
            ControllerFolderContents.QWrite(ReturnPayload);
            FConnection.SignalEvent(string(ReturnEventName), ekNormalEvent, ReturnPayload, False);
          finally
            ControllerFolderContents.Free;
          end;
        end;
      end;
    mcModelInit:
      begin
        ModelInitEvent.Read(aPayload);
        FModelsLock.Acquire;
        try
          ModelRunTime := GetModelRunTimeOnLinkID(ModelInitEvent.LinkID);
          if Assigned(ModelRunTime) then
          begin
            ModelRunTime.FUniqueClientID := ModelInitEvent.UID;
            ModelRunTime.FModelName := ModelInitEvent.ModelName;
            ModelRunTime.FPrivateEvent := FConnection.Publish(string(ModelInitEvent.ModelPrivateEventName), False);
          end
          else Log.WriteLn('Could not find valid model run time for '+string(ModelInitEvent.ModelName)+' ('+IntToHex(ModelInitEvent.LinkID, 8)+')', llError);
        finally
          FModelsLock.Release;
        end;
      end;
    mcModel:
      begin
        aPayload.Read(Action);
        case Action of
          // actionNew: actionDelete: only follow model change events
          actionChange:
            begin
              ModelChange.Read(aPayload);
              FModelsLock.Acquire;
              try
                ModelRunTime := GetModelRunTimeOnUniqueClientID(ModelChange.UID);
                if Assigned(ModelRunTime) then
                begin
                  ModelRunTime.FFederation := string(ModelChange.Federation);
                  ModelRunTime.FState := ModelChange.State;
                end;
              finally
                FModelsLock.Release;
              end;
            end;
          actionDelete:
            begin
              ModelDelete.Read(aPayload);
              FModelsLock.Acquire;
              try
                DeleteModelRunTimeOnUniqueClientID(ModelDelete.UID);
              finally
                FModelsLock.Release;
              end;
            end;
        end;
      end;
  end;
end;

procedure TNodeControllerThread.SignalControllerChange(const aEventName: string);
var
  CommandPayload: TControllerChangeEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.Priority := FPriority;
  Payload.Clear;
  Payload.Prepare(Integer(mcController));
  Payload.Prepare(Integer(actionChange));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcController));
  Payload.QWrite(Integer(actionChange));
  CommandPayload.QWrite(Payload);
  FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
end;

procedure TNodeControllerThread.SignalControllerDelete(const aEventName: string);
var
  CommandPayload: TControllerDeleteEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  Payload.Clear;
  Payload.Prepare(Integer(mcController));
  Payload.Prepare(Integer(actionDelete));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcController));
  Payload.QWrite(Integer(actionDelete));
  CommandPayload.QWrite(Payload);
  FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
end;

procedure TNodeControllerThread.SignalControllerExit;
var
  m: Integer;
begin
  SignalControllerDelete(FControllersEvent.EventName);
  if Assigned(FModels) then
  begin
    FModelsLock.Acquire;
    try
      for m := 0 to FModels.Count - 1
      do SignalControllerModelSetupDelete(FControllersEvent.EventName, FModels[m]);
    finally
      FModelsLock.Release;
    end;
  end;
end;

procedure TNodeControllerThread.SignalControllerModelSetupChange(const aEventName: string; aModelSetup: TCTModelSetup);
var
  CommandPayload: TControllerModelSetupChangeEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.Path := AnsiString(aModelSetup.FPath);
  CommandPayload.RunCount := aModelSetup.FRunCount;
  Payload.Clear;
  Payload.Prepare(Integer(mcControllerModelSetup));
  Payload.Prepare(Integer(actionNew));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcControllerModelSetup));
  Payload.QWrite(Integer(actionNew));
  CommandPayload.QWrite(Payload);
  FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
end;

procedure TNodeControllerThread.SignalControllerModelSetupDelete(const aEventName: string; aModelSetup: TCTModelSetup);
var
  CommandPayload: TControllerModelSetupDeleteEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.Path := AnsiString(aModelSetup.FPath);
  Payload.Clear;
  Payload.Prepare(Integer(mcControllerModelSetup));
  Payload.Prepare(Integer(actionDelete));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcControllerModelSetup));
  Payload.QWrite(Integer(actionDelete));
  CommandPayload.QWrite(Payload);
  FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
end;

procedure TNodeControllerThread.SignalControllerModelSetupNew(const aEventName: string; aModelSetup: TCTModelSetup);
var
  CommandPayload: TControllerModelSetupNewEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.Path := AnsiString(aModelSetup.FPath);
  CommandPayload.RunCount := aModelSetup.FRunCount;
  Payload.Clear;
  Payload.Prepare(Integer(mcControllerModelSetup));
  Payload.Prepare(Integer(actionNew));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcControllerModelSetup));
  Payload.QWrite(Integer(actionNew));
  CommandPayload.QWrite(Payload);
  FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
end;

procedure TNodeControllerThread.SignalControllerNew(const aEventName: string);
var
  CommandPayload: TControllerNewEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.Controller := AnsiString(FController);
  CommandPayload.ControllerPrivateEventName := AnsiString(FPrivateEvent.EventName);
  CommandPayload.Priority := FPriority;
  Payload.Clear;
  Payload.Prepare(Integer(mcController));
  Payload.Prepare(Integer(actionNew));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcController));
  Payload.QWrite(Integer(actionNew));
  CommandPayload.QWrite(Payload);
  FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
end;

{ TMCConfigController }

constructor TMCConfigController.Create(aUID: Integer; const aController, aControllerPrivateEventName: string; aPriority: Integer);
begin
  inherited Create;
  FUID := aUID;
  FController := aController;
  FControllerPrivateEventName := aControllerPrivateEventName;
  FPriority := aPriority;
  FModelPaths := TStringList.Create;
end;

destructor TMCConfigController.Destroy;
begin
  FreeAndNil(FModelPaths);
  inherited;
end;

{ TMCConfigControllerList }

constructor TMCConfigControllerList.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TMCConfigControllerList.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TMCConfigControllerList.FindControllerOnUID(aUID: Integer): TMCConfigController;
var
  c: Integer;
begin
  c := Count-1;
  while (c>=0) and (Items[c].FUID<>aUID)
  do c := c-1;
  if c>=0
  then Result := Items[c]
  else Result := nil;
end;

{ TMCControlInterface }

procedure TMCControlInterface.AddSessionParameters(aParameters: TModelParameters);
begin
  if aParameters.ParameterExists(FederationParameterName)
  then aParameters.Value[FederationParameterName] := FConnection.Federation
  else aParameters.Add(TModelParameter.Create(FederationParameterName, FConnection.Federation));
  if aParameters.ParameterExists(DataSourceParameterName)
  then aParameters.Value[DataSourceParameterName] := FDataSource
  else aParameters.Add(TModelParameter.Create(DataSourceParameterName, FDataSource));
end;

constructor TMCControlInterface.Create(
  const aRemoteHost: string; aRemotePort: Integer;
  const aOwnerName: string; aOwnerID: Integer;
  const aFederation, aDataSource, aIdleFederation: string);
begin
  inherited Create;
  FIdleFederation := aIdleFederation;
  FDataSource := aDataSource;
  FModelsLock := TCriticalSection.Create;
  FModels := TObjectList<TCIModelEntry>.Create;
  FModelsEx := TObjectList<TCIModelEntryEx>.Create;
  FConnection := TIMBConnection.Create(aRemoteHost, aRemotePort, aOwnerName, aOwnerID, aFederation);
  FPrivateEvent := FConnection.Subscribe(aIdleFederation+EventNamePartSeperator+ClientsRootEventName+EventNamePartSeperator+IntToHex(FConnection.UniqueClientID, 8), False);
  FPrivateEvent.OnNormalEvent := HandleControllersEvent;
  FControllersEvent := FConnection.Subscribe(aIdleFederation+EventNamePartSeperator+ControllersRootEventName, False);
  FControllersEvent.OnNormalEvent := HandleControllersEvent;
  RequestModels; // init request for list of models
end;

destructor TMCControlInterface.Destroy;
begin
  FreeAndNil(FModels);
  FreeAndNil(FModelsEx);
  FreeAndNil(FModelsLock);
  FreeAndNil(FConnection);
  inherited;
end;

function TMCControlInterface.GetCurrentModelEntryState(const aModelName: string): TModelState;
var
  m: Integer;
begin
  m := GetModelIndexOnName(aModelName);
  if m>=0
  then Result := FModels[m].FState
  else Result := msRemoved;
end;

function TMCControlInterface.GetFreeModelUIDOnName(const aModelName: string): Integer;
var
  mEx: Integer;
begin
  FModelsLock.Acquire;
  try
    mEx := FModelsEx.Count-1;
    while (mEx>=0) and ((FModelsEx[mEx].FState<>msIdle) or (AnsiCompareText(FModelsEx[mEx].FModelName, aModelName)<>0))
    do mEx := mEx-1;
    if mEx>=0
    then Result := FModelsEx[mEx].FUID
    else Result := 0;
  finally
    FModelsLock.Release;
  end;
end;

function TMCControlInterface.GetLockedModelUIDOnName(const aModelName: string): Integer;
var
  mEx: Integer;
begin
  FModelsLock.Acquire;
  try
    mEx := FModelsEx.Count-1;
    while (mEx>=0) and ((AnsiCompareText(FModelsEx[mEx].FModelName, aModelName)<>0) or not IsThisSession(FModelsEx[mEx]))
    do mEx := mEx-1;
    if mEx>=0
    then Result := FModelsEx[mEx].FUID
    else Result := 0;
  finally
    FModelsLock.Release;
  end;
end;

function TMCControlInterface.GetModelExIndexOnName(const aModelName: string): Integer;
begin
  Result := FModelsEx.Count-1;
  while (Result>=0) and (AnsiCompareText(FModelsEx[Result].FModelName, aModelName)<>0)
  do Result := Result-1;
end;

function TMCControlInterface.GetModelExIndexOnUID(aUID: Integer): Integer;
begin
  Result := FModelsEx.Count-1;
  while (Result>=0) and (FModelsEx[Result].FUID<>aUID)
  do Result := Result-1;
end;

function TMCControlInterface.GetModelIndexOnName(const aModelName: string): Integer;
begin
  Result := FModels.Count-1;
  while (Result>=0) and (AnsiCompareText(FModels[Result].FModelName, aModelName)<>0)
  do Result := Result-1;
end;

function TMCControlInterface.GetModelParametersEx(aUID: Integer): Boolean;
var
  mEx: Integer;
begin
  FModelsLock.Acquire;
  try
    mEx := GetModelExIndexOnUID(aUID);
    if mEx>=0
    then Result := RequestModelParameters(FModelsEx[mEx].FModelPrivateEventName)
    else Result := False;
  finally
    FModelsLock.Release;
  end;
end;

procedure TMCControlInterface.GetModels;
var
  m: Integer;
begin
  if Assigned(FOnModelState) then
  begin
    FModelsLock.Acquire;
    try
      for m := 0 to FModels.Count - 1
      do with FModels[m] do FOnModelState(FModelName, FState, FProgress);
    finally
      FModelsLock.Release;
    end;
  end;
end;

procedure TMCControlInterface.GetModelsEx;
var
  mEx: Integer;
begin
  if Assigned(FOnModelStateEx) then
  begin
    FModelsLock.Acquire;
    try
      for mEx := 0 to FModelsEx.Count - 1
      do with FModelsEx[mEx] do FOnModelStateEx(FUID, FController, FModelname,
           FFederation, FState, FProgress, FPriority, IsThisSession(FModelsEx[mEx]));
    finally
      FModelsLock.Release;
    end;
  end;
end;

function TMCControlInterface.GetNewModelEntryState(const aModelName: string): TModelState;
var
  mEx: Integer;
begin
  // get model state based on entries in FModelsEx
  Result := msRemoved;
  for mEx := 0 to FModelsEx.Count - 1 do
  begin
    // filter selected model
    if AnsiCompareText(FModelsEx[mEx].FModelName, aModelName)=0 then
    begin
      // check if already locked this model
      if IsThisSession(FModelsEx[mEx]) then
      begin // this model is locked by us
        case FModelsEx[mEx].FState of
          msIdle: Result := msLock;
        else
          Result := FModelsEx[mEx].FState;
        end;
        Break;
      end
      else
      begin // this model is NOT locked by us
        if FModelsEx[mEx].FState=msIdle then
        begin
          if Result=msRemoved
          then Result := msIdle;
        end;
      end;
    end;
  end;
end;

procedure TMCControlInterface.HandleChangeModel(aUID: Integer; aState: TModelState; const aFederation: string);
var
  mEx: Integer;
  Model: TCIModelEntryEx;
  Changed: Boolean;
  localState: TModelState;
  modelStateList: TList<TModelCache>;
  modelStateExList: TList<TModelCache>;
  mc: TModelCache;
begin
  modelStateList := TList<TModelCache>.Create;
  modelStateExList := TList<TModelCache>.Create;
  try
    FModelsLock.Acquire;
    try
      mEx := GetModelExIndexOnUID(aUID);
      if mEx>=0 then
      begin
        Model := FModelsEx[mEx];
        Changed :=
          (Model.FState<>aState) or
          (AnsiCompareText(Model.FFederation, aFederation)<>0);
        Model.FState := aState;
        Model.FFederation := aFederation;
        if Changed then
        begin
          if Assigned(OnModelStateEx)
          then modelStateExList.Add(Model.CreateCache(IsThisSession(Model)));
          localState := GetNewModelEntryState(Model.FModelName);
          if GetCurrentModelEntryState(Model.FModelName)<>localState then
          begin
            Model.FLinkedModelEntry := SetCurrentModelEntryState(Model.FModelName, localState);
            if Assigned(OnModelState)
            then modelStateList.Add(Model.CreateCache(localState));
          end;
        end;
      end;
    finally
      FModelsLock.Release;
    end;
    for mc in modelStateExList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
        procedure()
        begin
          OnModelStateEx(mc.FUID, mc.FController, mc.FModelName,
            mc.FFederation, mc.FState, mc.FProgress, mc.FPriority, mc.FIsThisSession);
        end
      );
    end;
    for mc in modelStateList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
          procedure()
          begin
            OnModelState(mc.FModelName, mc.FState, mc.FProgress);
          end
      );
    end;
  finally
    modelStateList.Free;
    modelStateExList.Free;
  end;
end;

procedure TMCControlInterface.HandleControllersEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  Action: Integer;
  ModelCommand: Integer;
  ModelNew: TModelNewEvent;
  ModelDelete: TModelDeleteEvent;
  ModelChange: TModelChangeEvent;
  UID: Integer;
  mEx: Integer;
  Model: TCIModelEntryEx;
  Progress: Integer;
  Parameters: TModelParameters;
  p: TModelParameter;
  modelStateList: TList<TModelCache>;
  modelStateExList: TList<TModelCache>;
  mc: TModelCache;
begin
  modelStateList := TList<TModelCache>.Create;
  modelStateExList := TList<TModelCache>.Create;
  try
    aPayload.Read(ModelCommand);
    case ModelCommand of
      mcModelProgress:
        begin
          aPayload.Read(UID);
          FModelsLock.Acquire;
          try
            mEx := GetModelExIndexOnUID(UID);
            if mEx>=0 then
            begin
              Model := FModelsEx[mEx];
              aPayload.Read(Progress);
              if Model.FProgress<>Progress then
              begin
                Model.FProgress := Progress;
                if Assigned(Model.FLinkedModelEntry) then
                begin
                  Model.FLinkedModelEntry.FProgress := Model.FProgress;
                  if Assigned(FOnModelState)
                  then modelStateList.Add(model.CreateCache(Model.FState));
                end;
                if Assigned(FOnModelStateEx)
                then modelStateExList.Add(model.CreateCache(IsThisSession(Model)));
              end;
            end;
          finally
            FModelsLock.Release;
          end;
        end;
      mcModel:
        begin
          aPayload.Read(Action);
          case Action of
            actionNew:
              begin
                ModelNew.Read(aPayload);
                HandleNewModel(ModelNew.UID, string(ModelNew.ModelName), string(ModelNew.Controller), ModelNew.Priority,
                  TModelState(ModelNew.State), string(ModelNew.Federation),
                  string(ModelNew.ModelPrivateEventName), string(ModelNew.ControllerPrivateEventName));
              end;
            actionDelete:
              begin
                ModelDelete.Read(aPayload);
                HandleDeleteModel(ModelDelete.UID);
              end;
            actionChange:
              begin
                ModelChange.Read(aPayload);
                HandleChangeModel(ModelChange.UID, TModelState(ModelChange.State), string(ModelChange.Federation));
              end;
          end;
        end;
      mcDefaultParameters:
        begin
          aPayload.Read(UID);
          FModelsLock.Acquire;
          try
            mEx := GetModelExIndexOnUID(UID);
            if mEx>=0 then
            begin
              Model := FModelsEx[mEx];
              parameters := TModelParameters.Create();
              try
                Parameters.Read(aPayload);
                if Assigned(OnParameter) then
                begin
                  for p in Parameters do
                  begin
                    FConnection.ReaderThread.Synchronize(
                      FConnection.ReaderThread,
                      procedure()
                      begin
                        OnParameter(Self, Model, p);
                      end
                    );
                  end;
                end;
              finally
                parameters.Free;
              end;
            end;
          finally
            FModelsLock.Release;
          end;
        end;
    end;
    for mc in modelStateExList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
        procedure()
        begin
          OnModelStateEx(mc.FUID, mc.FController, mc.FModelName,
            mc.FFederation, mc.FState, mc.FProgress, mc.FPriority, mc.FIsThisSession);
        end
      );
    end;
    for mc in modelStateList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
          procedure()
          begin
            OnModelState(mc.FModelName, mc.FState, mc.FProgress);
          end
      );
    end;
  finally
    modelStateList.Free;
    modelStateExList.Free;
  end;
end;

procedure TMCControlInterface.HandleDeleteModel(aUID: Integer);
var
  mEx: Integer;
  Model: TCIModelEntryEx;
  localState: TModelState;
  modelStateList: TList<TModelCache>;
  modelStateExList: TList<TModelCache>;
  mc: TModelCache;
begin
  modelStateList := TList<TModelCache>.Create;
  modelStateExList := TList<TModelCache>.Create;
  try
    // todo: does not work on standalone ModelStarter?
    FModelsLock.Acquire;
    try
      try
        mEx := GetModelExIndexOnUID(aUID);
        if mEx>=0 then
        begin
          Model := FModelsEx[mEx];
          Model.FState := msRemoved;
          if Assigned(OnModelStateEx)
          then modelStateExList.Add(model.CreateCache(IsThisSession(Model)));
          localState := GetNewModelEntryState(Model.ModelName);
          if GetCurrentModelEntryState(Model.ModelName)<>localState then
          begin
            if Assigned(OnModelState)
            then modelStateList.Add(model.CreateCache(localState));
            Model.FLinkedModelEntry := SetCurrentModelEntryState(Model.ModelName, localState);
          end;
          FConnection.UnPublish(Model.FModelPrivateEventName, False);
          FModelsEx.Delete(mEx);
        end;
      except
        on E: Exception
        do Log.WriteLn('Exception in TMCControlInterface.HandleDeleteModel on UID '+IntToStr(aUID)+': '+E.Message, llError);
      end;
    finally
      FModelsLock.Release;
    end;
    for mc in modelStateExList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
        procedure()
        begin
          OnModelStateEx(mc.FUID, mc.FController, mc.FModelName,
            mc.FFederation, mc.FState, mc.FProgress, mc.FPriority, mc.FIsThisSession);
        end
      );
    end;
    for mc in modelStateList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
          procedure()
          begin
            OnModelState(mc.FModelName, mc.FState, mc.FProgress);
          end
      );
    end;
  finally
    modelStateList.Free;
    modelStateExList.Free;
  end;
end;

procedure TMCControlInterface.HandleNewModel(aUID: Integer; const aModelName: string; const aController: string;
  aPriority: Integer; aState: TModelState; const aFederation, aModelPrivateEventName, aControllerPrivateEventName: string);
var
  mEx: Integer;
  Model: TCIModelEntryEx;
  Changed: Boolean;
  localState: TModelState;
  modelStateList: TList<TModelCache>;
  modelStateExList: TList<TModelCache>;
  mc: TModelCache;
begin
  modelStateList := TList<TModelCache>.Create;
  modelStateExList := TList<TModelCache>.Create;
  try
    FModelsLock.Acquire;
    try
      mEx := GetModelExIndexOnUID(aUID);
      if mEx<0 then
      begin
        Model := TCIModelEntryEx.Create(aUID, aModelName, aController, aPriority, aState, aFederation,
          aModelPrivateEventName, aControllerPrivateEventName);
        FModelsEx.Add(Model);
        Changed := True;
      end
      else
      begin
        Model := FModelsEx[mEx];
        Changed :=
          (AnsiCompareText(Model.FController, aController)<>0) or
          (AnsiCompareText(Model.FModelName, aModelName)<>0) or
          (Model.FPriority<>aPriority) or
          (Model.FState<>TModelState(aState)) or
          (AnsiCompareText(Model.FFederation, aFederation)<>0);
        Model.FController := aController;
        Model.FModelName := aModelName;
        Model.FPriority := aPriority;
        Model.FState := TModelState(aState);
        Model.FFederation := aFederation;
        Model.FModelPrivateEventName := aModelPrivateEventName;
      end;
      if Changed then
      begin
        if Assigned(OnModelStateEx)
        then modelStateExList.Add(Model.CreateCache(IsThisSession(Model)));
        localState := GetNewModelEntryState(Model.FModelName);
        if GetCurrentModelEntryState(Model.FModelName)<>localState then
        begin
          Model.FLinkedModelEntry := SetCurrentModelEntryState(Model.FModelName, localState);
          if Assigned(OnModelState)
          then modelStateList.Add(Model.CreateCache(localState));
        end;
      end;
    finally
      FModelsLock.Release;
    end;
    for mc in modelStateExList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
        procedure()
        begin
          OnModelStateEx(mc.FUID, mc.FController, mc.FModelName,
            mc.FFederation, mc.FState, mc.FProgress, mc.FPriority, mc.FIsThisSession);
        end
      );
    end;
    for mc in modelStateList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
          procedure()
          begin
            OnModelState(mc.FModelName, mc.FState, mc.FProgress);
          end
      );
    end;
  finally
    modelStateList.Free;
    modelStateExList.Free;
  end;
end;

function TMCControlInterface.IsThisSession(aModel: TCIModelEntryEx): Boolean;
begin
  if Assigned(aModel)
  then Result := (aModel.FState<>msIdle) and (AnsiCompareText(aModel.FFederation, FConnection.Federation)=0)
  else Result := False;
end;

function TMCControlInterface.StartModelEx(aUID: Integer; aParameters: TModelParameters): Boolean;
var
  mEx: Integer;
  m: Integer;
  Payload: TByteBuffer;
  LocalParameters: TModelParameters;
begin
  FModelsLock.Acquire;
  try
    mEx := GetModelExIndexOnUID(aUID);
    if (mEx>=0) and ((FModelsEx[mEx].FState=msIdle) or(FModelsEx[mEx].FState=msLock)) then
    begin
      m := GetModelIndexOnName(FModelsEx[mEx].FModelName);
      if m>=0 then
      begin
        // signal model lock
        LocalParameters := TModelParameters.Create(aParameters);
        try
          // add standard parameters for the the current session
          AddSessionParameters(LocalParameters);
          Payload.Clear;
          Payload.Prepare(Integer(mcModelClaim));
          Payload.Prepare(aUID);
          LocalParameters.Prepare(Payload);
          Payload.PrepareApply;
          Payload.QWrite(Integer(mcModelClaim));
          Payload.QWrite(aUID);
          LocalParameters.QWrite(Payload);
        finally
          LocalParameters.Free;
        end;
        Result := FConnection.SignalEvent(FModelsEx[mEx].FModelPrivateEventName, ekNormalEvent, Payload, False)>=0;
      end
      else Result := False;
    end
    else Result := False;
  finally
    FModelsLock.Release;
  end;
end;

procedure TMCControlInterface.Refresh;
begin
  FModelsLock.Acquire;
  try
    FModels.Clear;
    FModelsEx.Clear;
  finally
    FModelsLock.Release;
  end;
  RequestModels;
end;

function TMCControlInterface.RequestModelParameters(const aModelPrivateEventName: string): Boolean;
var
  Payload: TByteBuffer;
  Parameters: TModelParameters;
begin
  Parameters := TModelParameters.Create;
  try
    // add standard parameters for the the current session
    AddSessionParameters(Parameters);
    // build payload for command to send
    Payload.Clear;
    Payload.Prepare(Integer(mcRequestDefaultParameters)); // command
    Payload.Prepare(AnsiString(FPrivateEvent.EventName)); // return event
    Parameters.Prepare(Payload);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcRequestDefaultParameters));
    Payload.QWrite(AnsiString(FPrivateEvent.EventName));
    Parameters.QWrite(Payload);
  finally
    Parameters.Free;
  end;
  Result := FConnection.SignalEvent(aModelPrivateEventName, ekNormalEvent, Payload, False)>0;
//  if Result
//  then Result := aDefaultParametersEvent.WaitFor(aTimeOut)=wrSignaled;
end;

function TMCControlInterface.RequestModels: Boolean;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(mcRequestModels)); // command
  Payload.Prepare(AnsiString(FPrivateEvent.EventName)); // return event
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcRequestModels));
  Payload.QWrite(AnsiString(FPrivateEvent.EventName));
  Result := FConnection.SignalEvent(FControllersEvent.EventName, ekNormalEvent, Payload, False)>0;
end;

function TMCControlInterface.SetCurrentModelEntryState(const aModelName: string; aState: TModelState): TCIModelEntry;
var
  m: Integer;
begin
  m := GetModelIndexOnName(aModelName);
  if m>=0 then
  begin
    if aState<>msRemoved then
    begin
      Result := FModels[m];
      Result.FState := aState;
    end
    else
    begin
      FModels.Delete(m);
      Result := nil;
    end;
  end
  else
  begin
    if aState<>msRemoved then
    begin
      Result := TCIModelEntry.Create(aModelName, aState);
      FModels.Add(Result);
    end
    else Result := nil;
  end;
end;

procedure TMCControlInterface.SetEnvironment(const aFederation, aDataSource: string);
var
  m: Integer;
  mEx: Integer;
  Model: TCIModelEntryEx;
  localState: TModelState;
  modelStateList: TList<TModelCache>;
  modelStateExList: TList<TModelCache>;
  mc: TModelCache;
begin
  modelStateList := TList<TModelCache>.Create;
  modelStateExList := TList<TModelCache>.Create;
  try
    // TODO: how to react on change of data source
    FDataSource := aDataSource;
    FConnection.Federation := aFederation;
    // reset model states
    FModelsLock.Acquire;
    try
      for m := FModels.Count-1 downto 0 do
      begin
        localState := GetNewModelEntryState(FModels[m].FModelName);
        if FModels[m].FState<>localState then
        begin
          mEx := GetModelExIndexOnName(FModels[m].FModelName);
          if mEx>=0 then
          begin
            Model := FModelsEx[mEx];
            if Assigned(OnModelStateEx)
            then modelStateExList.Add(Model.CreateCache(IsThisSession(Model)));
            if localState<>msRemoved
            then Model.FLinkedModelEntry := SetCurrentModelEntryState(Model.FModelName, localState);
            if Assigned(OnModelState)
            then modelStateList.Add(Model.CreateCache(localState));
            if localState=msRemoved
            then Model.FLinkedModelEntry := SetCurrentModelEntryState(Model.FModelName, localState);
          end;
        end;
      end;
    finally
      FModelsLock.Release;
    end;
    for mc in modelStateExList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
        procedure()
        begin
          OnModelStateEx(mc.FUID, mc.FController, mc.FModelName,
            mc.FFederation, mc.FState, mc.FProgress, mc.FPriority, mc.FIsThisSession);
        end
      );
    end;
    for mc in modelStateList do
    begin
      FConnection.ReaderThread.Synchronize(
        FConnection.ReaderThread,
          procedure()
          begin
            OnModelState(mc.FModelName, mc.FState, mc.FProgress);
          end
      );
    end;
  finally
    modelStateList.Free;
    modelStateExList.Free;
  end;
end;

function TMCControlInterface.SignalModelQuitApplication(aUID: Integer): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(mcModelQuitApplication));
  Payload.Prepare(aUID);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModelQuitApplication));
  Payload.QWrite(aUID);
  Result := FControllersEvent.SignalEvent(ekNormalEvent, Payload); // auto publish?
end;

function TMCControlInterface.SignalModelState(aUID: Integer; aState: TModelState; const aFederation: string): Integer;
var
  ModelChange: TModelChangeEvent;
  Payload: TByteBuffer;
begin
  ModelChange.UID := aUID;
  ModelChange.State := Ord(aState);
  ModelChange.Federation := AnsiString(aFederation);
  Payload.Clear;
  Payload.Prepare(Integer(mcModel));
  Payload.Prepare(Integer(actionChange));
  ModelChange.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModel));
  Payload.QWrite(Integer(actionChange));
  ModelChange.QWrite(Payload);
  Result := FControllersEvent.SignalEvent(ekNormalEvent, Payload);
end;

function TMCControlInterface.StopModelEx(aUID: Integer): Boolean;
var
  mEx: Integer;
  EventName: string;
  Payload: TByteBuffer;
begin
  FModelsLock.Acquire;
  try
    mEx := GetModelExIndexOnUID(aUID);
    if mEx>=0
    then EventName := FModelsEx[mEx].FModelPrivateEventName
    else EventName := '';
  finally
    FModelsLock.Release;
  end;
  if EventName<>'' then
  begin
    // force (always send request)
    Payload.Clear;
    Payload.Prepare(Integer(mcModelUnClaim));
    Payload.Prepare(aUID);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModelUnClaim));
    Payload.QWrite(aUID);
    Result := FConnection.SignalEvent(EventName, ekNormalEvent, Payload, False)>=0;
  end
  else Result := False;
end;

{ TMCModelStarter2 }

constructor TMCModelStarter2.Create(const aModelName: string; aModelID: Integer);
var
  RemoteHost: string;
  RemotePort: Integer;
  ControllersEventName: string;
  ControllerPrivateEventName: string;
  LinkID: Int64;
  ModelName: string;
  ModelID: Integer;
begin
  inherited Create;
  FModelThread := nil;
  // init parameters
  RemoteHost := GetSetting(RemoteHostSwitch, DefaultRemoteHost);
  RemotePort := GetSetting(RemotePortSwitch, DefaultRemotePort);
  FIdleFederation := GetSetting(IdleFederationSwitch, DefaultIdleFederation);
  FController := AnsiString(CommandLine.GetSwitchDef(ControllerSwitch, 'Test'));
  ControllersEventName := CommandLine.GetSwitchDef(ControllersEventNameSwitch, FIdleFederation+'.'+ControllersRootEventName);
  ControllerPrivateEventName := CommandLine.GetSwitchDef(ControllerPrivateEventNameSwitch, ControllersEventName+'.'+string(FController));
  LinkID := StrToInt64Def(CommandLine.GetSwitch(LinkIDSwitch), 0);
  if aModelName<>'' then
  begin
    ModelName := aModelName;
    if aModelID<>0
    then ModelID := aModelID
    else ModelID := GetSetting(ModelIDSwitch, DefaultModelID);
  end
  else
  begin
    ModelName := GetSetting(ModelNameSwitch, DefaultModelName);
    ModelID := GetSetting(ModelIDSwitch, DefaultModelID);
  end;
  Log.WriteLn('IMB '+RemoteHost+':'+IntToStr(RemotePort));
  Log.WriteLn('Controller '+string(FController));
  Log.WriteLn('ControllersEventName '+ControllersEventName);
  Log.WriteLn('ControllerPrivateEventName '+ControllerPrivateEventName);
  Log.WriteLn('LinkID '+IntToStr(LinkID));
  Log.WriteLn('ModelName '+ModelName);
  Log.WriteLn('ModelID '+IntToStr(ModelID));
  FQuitApplicationEvent := TEvent.Create(nil, False, False, '');
  // init model control
  FConnection := TIMBConnection.Create(RemoteHost, RemotePort, ModelName, ModelID, '');
  FConnection.KeepAlive := True;
  FPrivateModelEvent := FConnection.Subscribe(
    ControllerPrivateEventName+EventNamePartSeperator+
    ModelName+EventNamePartSeperator+IntToHex(FConnection.UniqueClientID, 8), False);
  FPrivateModelEvent.OnNormalEvent := HandleControlEvents;
  FControllersEvent := FConnection.Subscribe(ControllersEventName, False);
  FControllersEvent.OnNormalEvent := HandleControlEvents;
  FPrivateControllerEvent := FConnection.Subscribe{Publish}(ControllerPrivateEventName, False);
  FPrivateControllerEvent.OnNormalEvent := HandleControlEvents;
  // signal process id and client id etc. to controller
  Log.WriteLn('UID '+IntToHex(FConnection.UniqueClientID, 8));
  SignalModelInit(LinkID, AnsiString(ModelName));
  // signal state as idle
  FState := msIdle;
  FProgress := 0;
  FPriority := GetSetting(ModelPrioritySwitch, DefaultModelPriority);
  SignalModelNew('');
end;

destructor TMCModelStarter2.Destroy;
begin
  FreeAndNil(FConnection);
  FreeAndNil(FQuitApplicationEvent);
  inherited;
end;

procedure TMCModelStarter2.QuitApplication;
begin
  SignalModelExit;
  FQuitApplicationEvent.SetEvent;
end;

procedure TMCModelStarter2.RequestModels(const aReturnEventName: string);
begin
  SignalModelNew(aReturnEventName);
end;

procedure TMCModelStarter2.HandleControlEvents(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  Command: Integer;
  Parameters: TModelParameters;
  ReturnEventName: AnsiString;
  ParameterPayload: TByteBuffer;
  UID: Integer;
  Action: Integer;
  ModelChange: TModelChangeEvent;
begin
  aPayload.Read(Command);
  case Command of
    mcModel:
      begin
        aPayload.Read(Action);
        case Action of
          actionChange:
            begin // lock or set back to idle
              ModelChange.Read(aPayload);
              if ModelChange.UID=FConnection.UniqueClientID then
              begin
                case ModelChange.State of
                  Ord(msLock):
                    begin
                      if FState=msIdle then
                      begin
                        Connection.Federation := string(ModelChange.Federation);
                        FState := msLock;
                      end
                      else Log.WriteLn('Recevied lock request while not idle ('+IntToStr(Ord(FState))+')', llWarning);
                    end;
                  Ord(msIdle):
                    begin
                      if FState=msLock then
                      begin
                        Connection.Federation := string(ModelChange.Federation);
                        FState := msIdle;
                      end
                      else Log.WriteLn('Recevied unlock request while not locked ('+IntToStr(Ord(FState))+')', llWarning);
                    end;
                else
                  Log.WriteLn('Received unsupported external model state change '+IntToStr(Ord(FState))+' -> '+IntToStr(ModelChange.State), llWarning);
                end;
              end;
            end;
        end;
      end;
    mcRequestDefaultParameters:
      begin
        aPayload.Read(ReturnEventName);
        Parameters := TModelParameters.Create;
        try
          // read default parameters from request if available
          if aPayload.ReadAvailable>0
          then Parameters.Read(aPayload);
          ParameterRequest(Parameters);
          // build and send ParameterPayload
          ParameterPayload.Clear;
          ParameterPayload.Prepare(Integer(mcDefaultParameters));
          ParameterPayload.Prepare(FConnection.UniqueClientID);
          Parameters.Prepare(ParameterPayload);
          ParameterPayload.PrepareApply;
          ParameterPayload.QWrite(Integer(mcDefaultParameters));
          ParameterPayload.QWrite(FConnection.UniqueClientID);
          Parameters.QWrite(ParameterPayload);
          FConnection.SignalEvent(string(ReturnEventName), ekNormalEvent, ParameterPayload, False);
        finally
          Parameters.Free;
        end;
      end;
    mcModelClaim:
      begin
        aPayload.Read(UID);
        if UID=FConnection.UniqueClientID then
        begin // claim model
          Parameters := TModelParameters.Create;
          try
            Parameters.Read(aPayload);
            if Parameters.ParameterExists(FederationParameterName)
            then FConnection.Federation := Parameters.Value[FederationParameterName];
            FreeAndNil(FModelThread);
            SignalModelState(msBusy); // default signal busy state
            try
              StartModel(Parameters);
            except
              on E: Exception
              do Log.WriteLn('Exception in TMCModelStarter2.HandleControlEvents (mcModelClaim): '+E.Message, llError);
            end;
          finally
            Parameters.Free;
          end;
        end;
      end;
    mcModelUnClaim:
      begin // unclaim model
        aPayload.Read(UID);
        if UID=FConnection.UniqueClientID then
        begin
          try
            try
              SignalModelState(msBusy);
              StopModel();
              FreeAndNil(FModelThread);
            finally
              SignalModelProgress(0);
              SignalModelState(msIdle);
            end;
          except
            on E: Exception
            do Log.WriteLn('Exception in TMCModelStarter2.HandleControlEvents (mcModelUnClaim): '+E.Message, llError);
          end;
        end;
      end;
    mcModelQuitApplication:
      begin
        aPayload.Read(UID);
        if UID=FConnection.UniqueClientID
        then QuitApplication; // quit model starter (complete application)
      end;
    mcRequestModels:
      begin
        aPayload.Read(ReturnEventName);
        RequestModels(string(ReturnEventName));
      end;
  end;
end;

procedure TMCModelStarter2.ManualStart(aState: TModelState; const aFederation: string);
begin
  FState := aState;
  Connection.Federation := aFederation;
end;

procedure TMCModelStarter2.SignalModelExit;
var
  Payload: TByteBuffer;
begin
  if Assigned(FConnection) and Assigned(FControllersEvent) then
  begin
    Payload.Clear;
    Payload.Prepare(Integer(mcModel));
    Payload.Prepare(Integer(actionDelete));
    Payload.Prepare(FConnection.UniqueClientID);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModel));
    Payload.QWrite(Integer(actionDelete));
    Payload.QWrite(FConnection.UniqueClientID);
    FControllersEvent.SignalEvent(ekNormalEvent, Payload);
  end;
end;

procedure TMCModelStarter2.SignalModelInit(aLinkID: Int64; const aModelName: AnsiString);
var
  Payload: TByteBuffer;
  ModelInitEvent: TModelInitEvent;
begin
  ModelInitEvent.LinkID := aLinkID;
  ModelInitEvent.UID := FConnection.UniqueClientID;
  ModelInitEvent.ModelName := aModelName;
  ModelInitEvent.ModelPrivateEventName := AnsiString(FPrivateModelEvent.EventName);
  Payload.Clear;
  Payload.Prepare(Integer(mcModelInit));
  ModelInitEvent.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModelInit));
  ModelInitEvent.QWrite(Payload);
  FPrivateControllerEvent.SignalEvent(ekNormalEvent, Payload);
end;

procedure TMCModelStarter2.SignalModelNew(const aEventName: string);
var
  CommandPayload: TModelNewEvent;
  Payload: TByteBuffer;
begin
  CommandPayload.UID := FConnection.UniqueClientID;
  CommandPayload.ModelName := AnsiString(FConnection.OwnerName);
  CommandPayload.Controller := AnsiString(FController);
  CommandPayload.Priority := FPriority;
  CommandPayload.State := Ord(FState);
  CommandPayload.Federation := AnsiString(FConnection.Federation);
  CommandPayload.ModelPrivateEventName := AnsiString(FPrivateModelEvent.EventName);
  CommandPayload.ControllerPrivateEventName := AnsiString(FPrivateControllerEvent.EventName);
  Payload.Clear;
  Payload.Prepare(Integer(mcModel));
  Payload.Prepare(Integer(actionNew));
  CommandPayload.Prepare(Payload);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModel));
  Payload.QWrite(Integer(actionNew));
  CommandPayload.QWrite(Payload);
  if aEventName=''
  then FControllersEvent.SignalEvent(ekNormalEvent, Payload)
  else FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
  // signal progress if set (ie neq 0)
  if FProgress<>0 then
  begin
    Payload.Clear;
    Payload.Prepare(Integer(mcModelProgress));
    Payload.Prepare(FConnection.UniqueClientID);
    Payload.Prepare(FProgress);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModelProgress));
    Payload.QWrite(FConnection.UniqueClientID);
    Payload.QWrite(FProgress);
    if aEventName=''
    then FControllersEvent.SignalEvent(ekNormalEvent, Payload)
    else FConnection.SignalEvent(aEventName, ekNormalEvent, Payload, False);
  end;
end;

procedure TMCModelStarter2.SignalModelProgress(aProgress: Integer);
var
  Payload: TByteBuffer;
begin
  if Assigned(Self) then
  begin
    if FProgress<>aProgress then
    begin
      Payload.Clear;
      Payload.Prepare(Integer(mcModelProgress));
      Payload.Prepare(FConnection.UniqueClientID);
      Payload.Prepare(aProgress);
      Payload.PrepareApply;
      Payload.QWrite(Integer(mcModelProgress));
      Payload.QWrite(FConnection.UniqueClientID);
      Payload.QWrite(aProgress);
      FControllersEvent.SignalEvent(ekNormalEvent, Payload);
      FProgress := aProgress;
    end;
  end;
end;

procedure TMCModelStarter2.SignalModelState(aState: Integer);
begin
  SignalModelState(TModelState(aState));
end;

procedure TMCModelStarter2.SignalModelState(aState: TModelState; const aFederation: string);
var
  ModelChange: TModelChangeEvent;
  Payload: TByteBuffer;
begin
  if Assigned(Self) and (FState<>aState) then
  begin
    ModelChange.UID := FConnection.UniqueClientID;
    ModelChange.State := Ord(aState);
    ModelChange.Federation := AnsiString(aFederation);
    Payload.Clear;
    Payload.Prepare(Integer(mcModel));
    Payload.Prepare(Integer(actionChange));
    ModelChange.Prepare(Payload);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcModel));
    Payload.QWrite(Integer(actionChange));
    ModelChange.QWrite(Payload);
    FControllersEvent.SignalEvent(ekNormalEvent, Payload);
    Log.WriteLn('New model state '+IntToStr(Ord(FState))+' -> '+IntToStr(Ord(aState))+' on '+aFederation);
    FState := aState;
  end;
end;

procedure TMCModelStarter2.SignalModelState(aState: TModelState);
begin
  SignalModelState(aState, FConnection.Federation);
end;


{ TCIModelEntry2 }

constructor TCIModelEntry2.Create(aUID: Integer; const aModelName, aController: string; aPriority: Integer;
  aState: TModelState; const aFederation, aModelPrivateEventName, aControllerPrivateEventName: string);
begin
  inherited Create;
  FUID := aUID;
  FModelName := aModelName;
  FController := aController;
  FPriority := aPriority;
  FState := aState;
  FProgress := 0;
  FFederation := aFederation;
  FModelPrivateEventName := aModelPrivateEventName;
  FControllerPrivateEventName := aControllerPrivateEventName;
  FDefaultParameters := nil;
  FDefaultParametersEvent := TEvent.Create(nil, False, False, '');
  FTag := 0;
end;

destructor TCIModelEntry2.Destroy;
begin
  FreeAndNil(FDefaultParameters);
  FreeAndNil(FDefaultParametersEvent);
  FTag := 0;
  inherited;
end;


function TCIModelEntry2.IsThisSession(const aFederation: string): Boolean;
begin
  if Assigned(Self)
  then Result := (FState<>msIdle) and (AnsiCompareText(FFederation, aFederation)=0)
  else Result := False;
end;

{ TMCControlInterface2 }

procedure TMCControlInterface2.AddSessionParameters(aParameters: TModelParameters);
begin
  aParameters.Add(TModelParameter.Create(FederationParameterName, FFederation));
  aParameters.Add(TModelParameter.Create(DataSourceParameterName, FDataSource));
end;

function TMCControlInterface2.ClaimModel(aModel: TCIModelEntry2; aParameters: TModelParameters): Boolean;
var
  Payload: TByteBuffer;
  LocalParameters: TModelParameters;
begin
  if (aModel.FState=msIdle) or(aModel.FState=msLock) then
  begin
    // signal model lock
    LocalParameters := TModelParameters.Create(aParameters);
    try
      // add standard parameters for the the current session
      AddSessionParameters(LocalParameters);
      Payload.Clear;
      Payload.Prepare(Integer(mcModelClaim));
      Payload.Prepare(aModel.UID);
      LocalParameters.Prepare(Payload);
      Payload.PrepareApply;
      Payload.QWrite(Integer(mcModelClaim));
      Payload.QWrite(aModel.UID);
      LocalParameters.QWrite(Payload);
    finally
      LocalParameters.Free;
    end;
    Result := FConnection.SignalEvent(aModel.FModelPrivateEventName, ekNormalEvent, Payload, False)>=0;
  end
  else Result := False;
end;

constructor TMCControlInterface2.Create(aConnection: TIMBConnection; const aFederation, aDataSource,
  aIdleFederation: string);
begin
  inherited Create;
  FConnection := aConnection;
  FFederation := aFederation;
  FDataSource := aDataSource;
  FIdleFederation := aIdleFederation;
  FLock := TCriticalSection.Create;
  FModels := TObjectList<TCIModelEntry2>.Create;
  FPrivateEvent := FConnection.Subscribe(aIdleFederation+EventNamePartSeperator+ClientsRootEventName+EventNamePartSeperator+IntToHex(FConnection.UniqueClientID, 8), False);
  FPrivateEvent.OnNormalEvent := HandleControllersEvent;
  FControllersEvent := FConnection.Subscribe(aIdleFederation+EventNamePartSeperator+ControllersRootEventName, False);
  FControllersEvent.OnNormalEvent := HandleControllersEvent;
  SignalRequestModels; // init request for list of models
end;


destructor TMCControlInterface2.Destroy;
begin
  FreeAndNil(FModels);
  FreeAndNil(FLock);
  FConnection := nil;
  inherited;
end;

function TMCControlInterface2.GetModelExIndexOnUID(aUID: Integer): Integer;
begin
  Result := FModels.Count-1;
  while (Result>=0) and (FModels[Result].FUID<>aUID)
  do Result := Result-1;
end;

procedure TMCControlInterface2.HandleControllersEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  Action: Integer;
  ModelCommand: Integer;
  ModelNew: TModelNewEvent;
  ModelDelete: TModelDeleteEvent;
  ModelChange: TModelChangeEvent;
  UID: Integer;
  mEx: Integer;
  Model: TCIModelEntry2;
  Progress: Integer;
begin
  aPayload.Read(ModelCommand);
  case ModelCommand of
    mcModelProgress:
      begin
        aPayload.Read(UID);
        FLock.Acquire;
        try
          mEx := GetModelExIndexOnUID(UID);
          if mEx>=0 then
          begin
            try
              Model := FModels[mEx];
              aPayload.Read(Progress);
              if Model.FProgress<>Progress then
              begin
                Model.FProgress := Progress;
                HandleModelChange(Model, mcProgress);
              end;
            except
              on E: Exception
              do Log.WriteLn('Exception in TMCControlInterface2.HandleControllersEvent mcModelProgress on UID '+IntToStr(ModelChange.UID)+': '+E.Message, llError);
            end;
          end;
        finally
          FLock.Release;
        end;
      end;
    mcModel:
      begin
        aPayload.Read(Action);
        case Action of
          actionNew:
            begin
              ModelNew.Read(aPayload);
              FLock.Acquire;
              try
                try
                  mEx := GetModelExIndexOnUID(ModelNew.UID);
                  if mEx<0 then
                  begin
                    Model := TCIModelEntry2.Create(ModelNew.UID, string(ModelNew.ModelName),
                      string(ModelNew.Controller), ModelNew.Priority, TModelState(ModelNew.State), string(ModelNew.Federation),
                      string(ModelNew.ModelPrivateEventName), string(ModelNew.ControllerPrivateEventName));
                    FModels.Add(Model);
                  end
                  else
                  begin
                    Model := FModels[mEx];
                    Model.FController := string(ModelNew.Controller);
                    Model.FModelName := string(ModelNew.ModelName);
                    Model.FPriority := ModelNew.Priority;
                    Model.FState := TModelState(ModelNew.State);
                    Model.FFederation := string(ModelNew.Federation);
                    Model.FModelPrivateEventName := string(ModelNew.ModelPrivateEventName);
                  end;
                  HandleModelChange(Model, mcNew);
                except
                  on E: Exception
                  do Log.WriteLn('Exception in TMCControlInterface2.HandleControllersEvent mcModel actionNew on UID '+IntToStr(ModelNew.UID)+': '+E.Message, llError);
                end;
              finally
                FLock.Release;
              end;
            end;
          actionDelete:
            begin
              ModelDelete.Read(aPayload);
              FLock.Acquire;
              try
                try
                  mEx := GetModelExIndexOnUID(ModelDelete.UID);
                  if mEx>=0 then
                  begin
                    Model := FModels[mEx];
                    Model.FState := msRemoved;
                    HandleModelChange(Model, mcRemove);
                    FConnection.UnPublish(Model.FModelPrivateEventName, False);
                    FModels.Delete(mEx);
                  end;
                except
                  on E: Exception
                  do Log.WriteLn('Exception in TMCControlInterface2.HandleControllersEvent mcModel actionDelete on UID '+IntToStr(ModelDelete.UID)+': '+E.Message, llError);
                end;
              finally
                FLock.Release;
              end;
            end;
          actionChange:
            begin
              ModelChange.Read(aPayload);
              FLock.Acquire;
              try
                try
                  mEx := GetModelExIndexOnUID(ModelChange.UID);
                  if mEx>=0 then
                  begin
                    Model := FModels[mEx];
                    if Model.FState<>TModelState(ModelChange.State) then
                    begin
                      Model.FState := TModelState(ModelChange.State);
                      HandleModelChange(Model, mcState);
                    end;
                    if AnsiCompareText(Model.FFederation, string(ModelChange.Federation))<>0 then
                    begin
                      Model.FFederation := string(ModelChange.Federation);
                      HandleModelChange(Model, mcFederation);
                    end;
                  end;
                except
                  on E: Exception
                  do Log.WriteLn('Exception in TMCControlInterface2.HandleControllersEvent mcModel actionChange on UID '+IntToStr(ModelChange.UID)+': '+E.Message, llError);
                end;
              finally
                FLock.Release;
              end;
            end;
        end;
      end;
    mcDefaultParameters:
      begin
        aPayload.Read(UID);
        FLock.Acquire;
        try
          mEx := GetModelExIndexOnUID(UID);
          if mEx>=0 then
          begin
            Model := FModels[mEx];
            if not Assigned(Model.FDefaultParameters)
            then Model.FDefaultParameters := TModelParameters.Create
            else Model.FDefaultParameters.Clear;
            Model.FDefaultParameters.Read(aPayload);
            Model.FDefaultParametersEvent.SetEvent; // signal receive of parameters
          end;
        finally
          FLock.Release;
        end;
      end;
  end;
end;

procedure TMCControlInterface2.HandleModelChange(aModel: TCIModelEntry2; aChange: TMChange);
begin
  // default: no action
end;

function TMCControlInterface2.IsThisSession(aModel: TCIModelEntry2): Boolean;
begin
  Result := aModel.IsThisSession(FFederation);
end;

function TMCControlInterface2.QuitModel(aModel: TCIModelEntry2): Boolean;
begin
  Result := SignalModelQuitApplication(aModel.UID)>0;
end;

procedure TMCControlInterface2.Refresh;
begin

  FModels.Clear;
  SignalRequestModels;
end;

function TMCControlInterface2.RequestModelDefaultParameters(aModel: TCIModelEntry2; aTimeOut: LongWord): Boolean;
begin
  if SignalRequestModelParameters(aModel.FModelPrivateEventName)
  then Result := aModel.FDefaultParametersEvent.WaitFor(aTimeOut)=wrSignaled
  else Result := False;
end;

procedure TMCControlInterface2.SetFederation(const aValue: string);
var
  mEx: Integer;
  Model: TCIModelEntry2;
begin
  if AnsiCompareText(FFederation, aValue)<>0 then
  begin
    // check for old and new federation matches in models
    FLock.Acquire;
    try
      for mEx := 0 to FModels.Count - 1 do
      begin
        Model := FModels[mEx];
        if (AnsiCompareText(Model.Federation,FFederation)=0) or (AnsiCompareText(Model.Federation, aValue)=0)
        then HandleModelChange(Model, mcFederation);
      end;
      FFederation := aValue;
    finally
      FLock.Release;
    end;
  end;
end;

function TMCControlInterface2.SignalModelQuitApplication(aUID: Integer): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(mcModelQuitApplication));
  Payload.Prepare(aUID);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModelQuitApplication));
  Payload.QWrite(aUID);
  Result := FControllersEvent.SignalEvent(ekNormalEvent, Payload); // auto publish?
end;

function TMCControlInterface2.SignalRequestModelParameters(const aModelPrivateEventName: string): Boolean;
var
  Payload: TByteBuffer;
  Parameters: TModelParameters;
begin
  Parameters := TModelParameters.Create;
  try
    // add standard parameters for the the current session
    AddSessionParameters(Parameters);
    // build payload for command to send
    Payload.Clear;
    Payload.Prepare(Integer(mcRequestDefaultParameters)); // command
    Payload.Prepare(AnsiString(FPrivateEvent.EventName)); // return event
    Parameters.Prepare(Payload);
    Payload.PrepareApply;
    Payload.QWrite(Integer(mcRequestDefaultParameters));
    Payload.QWrite(AnsiString(FPrivateEvent.EventName));
    Parameters.QWrite(Payload);
  finally
    Parameters.Free;
  end;
  Result := FConnection.SignalEvent(aModelPrivateEventName, ekNormalEvent, Payload, False)>0;
end;

function TMCControlInterface2.SignalRequestModels: Boolean;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(mcRequestModels)); // command
  Payload.Prepare(AnsiString(FPrivateEvent.EventName)); // return event
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcRequestModels));
  Payload.QWrite(AnsiString(FPrivateEvent.EventName));
  Result := FConnection.SignalEvent(FControllersEvent.EventName, ekNormalEvent, Payload, False)>0;
end;

function TMCControlInterface2.UnclaimModel(aModel: TCIModelEntry2): Boolean;
var
  EventName: string;
  Payload: TByteBuffer;
begin
  EventName := aModel.FModelPrivateEventName;
  // force (always send request)
  Payload.Clear;
  Payload.Prepare(Integer(mcModelUnClaim));
  Payload.Prepare(aModel.UID);
  Payload.PrepareApply;
  Payload.QWrite(Integer(mcModelUnClaim));
  Payload.QWrite(aModel.UID);
  Result := FConnection.SignalEvent(EventName, ekNormalEvent, Payload, False)>=0;
end;

{ TModelStateHelper }

function TModelStateHelper.ToShortString: string;
begin
  case Self of
    msReady:         Result := 'R';
    msCalculating:   Result := 'C';
    msBusy:          Result := 'B';
    msIdle:          Result := 'I';
    msLock:          Result := 'L';
    msRemoved:       Result := '~';
  else
    Result := '#';
  end;
end;

function TModelStateHelper.ToString: string;
begin
  case Self of
    msReady:         Result := 'ready';
    msCalculating:   Result := 'calc';
    msBusy:          Result := 'busy';
    msIdle:          Result := 'idle';
    msLock:          Result := 'locked';
    msRemoved:       Result := 'removed';
  else
    Result := '## unknown';
  end;
end;

{ TModelParameterValueTypeHelper }

function TModelParameterValueTypeHelper.ToString: string;
begin
  case Self of
    mpvtFloat:     Result :='float';
    mpvtBoolean:   Result :='bool';
    mpvtInteger:   Result :='int';
    mpvtString:    Result :='string';
  else
                   Result :='## unknown';
  end;
end;

end.
