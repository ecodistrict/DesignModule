unit SessionServerLib;

interface

uses
  StdIni,
  imb4,
  Data.DB,
  TimerPool,
  WorldDataCode,
  WorldLegends,
  IdCoderMIME,
  Int64Time,

  WinApi.Windows,

  Vcl.graphics,
  Vcl.Imaging.pngimage,

  // use vcl bitmaps (FMX: conflict on server)

  Logger, // after bitmap units (also use log)
  CommandQueue,
  WorldTilerConsts, // iceh* tiler consts
  TilerControl,

	System.JSON, System.SysConst, System.Math,
  System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.SysUtils;

const
  WS2IMBEventName = 'USIdle.Sessions.WS2IMB';
  sepElementID = '$';
  MaxDirectSendObjectCount = 300; // todo: tune, 200, 500.. ?

  colorBasicOutline = $3388FF or $FF000000;
  colorBasicFill = $B5C6DD or $FF000000;

  TilerNameSwitch = 'TilerName';
    DefaultTilerName = 'vps17642.public.cloudvps.com';

  TilerStatusURLSwitch = 'TilerStatusURL';

  MaxEdgeLengthInMetersSwitchName = 'MaxEdgeLengthInMeters';
    DefaultMaxEdgeLengthInMeters = 250;

  MaxNearestObjectDistanceInMetersSwitch = 'MaxNearestObjectDistanceInMeters';
    DefaultMaxNearestObjectDistanceInMeters = 50;

  SourceEPSGSwitch = 'SourceEPSG';

  UseScenarioHierarchySwitch = 'UseScenarioHierarchy';

type
  TDistanceLatLon = record
    m_per_deg_lat: Double;
    m_per_deg_lon: Double;

    class function Create(aLat1InDegrees, aLat2InDegrees: Double): TDistanceLatLon; overload; static;
    class function Create(aLatMidInRad: Double): TDistanceLatLon; overload; static;
    function distanceInMeters(aDeltaLat, aDeltaLon: Double): Double;
  end;

  TProject = class; // forward

  TScenario = class; // forward

  TLayer = class; // forward

  TClientDomain = record
  class function Create(const aName: string): TClientDomain; static;
  private
    function getJSON: string;
  public
    name: string;
    kpis: string;
    charts: string;
    layers: string;
    enabled: Integer;
    property JSON: string read getJSON;
  end;

  TSessionModel = class; // forward

  TClient = class; // forward

  TClientSubscribable = class
  constructor Create;
  destructor Destroy; override;
  private
    fClients: TObjectList<TClient>; // refs, lock with TMonitor
  protected
    function getElementID: string; virtual; abstract;
  public
    property clients: TObjectList<TClient> read fClients; // refs, lock with TMonitor
    property elementID: string read getElementID;
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; virtual;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; virtual;
    procedure HandleFirstSubscriber; virtual;
    procedure HandleLastSubscriber; virtual;
  end;

  TDiffLayer = class(TClientSubscribable)
  constructor Create(const aElementID: string; aCurrentLayer, aReferenceLayer: TLayer);
  destructor Destroy; override;
  private
    fCurrentLayer: TLayer;
    fReferenceLayer: TLayer;
    fTilerLayer: TTilerLayer; // owns
    fSendRefreshTimer: TTimer;
    fPreviewRequestTimer: TTimer;
    fLegendJSON: string;
    function getRefJSON: string;
  protected
    function getElementID: string; override;
    procedure handleTilerInfo(aTilerLayer: TTilerLayer);
    procedure handleTilerRefresh(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
    procedure handleTilerPreview(aTilerLayer: TTilerLayer);
  public
    property tilerLayer: TTilerLayer read fTilerLayer;
    property refJSON: string read getRefJSON;
    property legendJSON: string read fLegendJSON;
    procedure HandleSubLayerInfo(aLayer: TLayer);
  end;

  TLegendFormat = (lfVertical, lfHorizontal); // todo:..

  TClient = class
  constructor Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
  destructor Destroy; override;
  private
    fSubscribedElements: TObjectList<TClientSubscribable>; // ref, use TMonitor to lock
    fProject: TProject; // ref
    fCurrentScenario: TScenario; // ref
    fRefScenario: TScenario; // ref
    fClientID: string;
    fClientEvent: TEventEntry;

    function getSessionModel: TSessionModel;

    function sessionDescription: string;
    function activeScenario: string;
    function referenceScenario: string;
  protected
    procedure SendErrorMessage(const aMessage: string);
    procedure SendMeasures();
    procedure SendDomains(const aPrefix: string);
    procedure SendSession();
    procedure SendMeasuresHistory();
    procedure UpdateSession();
    procedure SendTimeSlider();
    procedure SendSelectionEnabled();
    procedure SendMeasuresEnabled();
    procedure SendMeasuresHistoryEnabled();
    procedure SendSimulationControlEnabled();

    procedure addClient(aElement: TClientSubscribable);
    procedure removeClient(aElement: TClientSubscribable);
  public
    property sessionModel: TSessionModel read getSessionModel;
    property subscribedElements: TObjectList<TClientSubscribable> read fSubscribedElements; // ref, use TMonitor to lock
    property clientID: string read fClientID;

    procedure signalString(const aString: string);

    procedure SendRefresh(const aElementID, aTimeStamp, aObjectsTilesLink: string);
    procedure SendRefreshRef(const aElementID, aTimeStamp, aRef: string);
    procedure SendRefreshDiff(const aElementID, aTimeStamp, aDiff: string);
    procedure SendPreview(const aElementID, aPreviewBASE64: string);

    procedure HandleElementRemove(aElement: TClientSubscribable);
    procedure HandleScenarioRemove(aScenario: TScenario);

    procedure HandleClientCommand(const aJSONString: string);
  end;

  TScenarioElement = class(TClientSubscribable)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
  destructor Destroy; override;
  protected
    fScenario: TScenario; // ref
    fDomain: string;
    fID: string;
    fName: string;
    fDescription: string;
    fDefaultLoad: Boolean;
  protected
    function getJSON: string; virtual;
    function getElementID: string; override;
  public
    property scenario: TScenario read fScenario;
    property domain: string read fDomain;
    property ID: string read fID;
    property name: string read fName;
    property description: string read fDescription;
    property defaultLoad: Boolean read fDefaultLoad;
    property JSON: string read getJSON;
    property elementID: string read getElementID;
  end;

  TLayerObject = class
  constructor Create(aLayer: TLayer; const aID: TWDID);
  private
    fLayer: TLayer;
    fID: TWDID;
  protected
    function getGeoJSON2D(const aType: string): string; virtual; abstract;
    function getValidGeometry: Boolean; virtual;
    function getExtent: TWDExtent; virtual;
  public
    function Encode: TByteBuffer; virtual;
  public
    property layer: TLayer read fLayer;
    property ID: TWDID read fID;
    property GeoJSON2D[const aType: string]: string read getGeoJSON2D;
    property ValidGeometry: Boolean read getValidGeometry;
    property Extent: TWDExtent read getExtent;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; virtual; abstract;
    function intersects(aGeometry: TWDGeometry): Boolean; virtual; abstract;
  end;

  TGeometryPointLayerObject = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometryPoint: TWDGeometryPoint; aValue: Double);
  destructor Destroy; override;
  protected
    fGeometryPoint: TWDGeometryPoint; // owns
    fValue: Double; // value to lookup color within palette of layer
  protected
    function getGeoJSON2D(const aType: string): string; override;
    function getValidGeometry: Boolean; override;
    function getExtent: TWDExtent; override;
  public
    function Encode: TByteBuffer; override;
  public
    property geometryPoint: TWDGeometryPoint read fGeometryPoint;
    property value: Double read fValue;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TGeometryLayerPOIObject = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aPOI: Integer; aGeometryPoint: TWDGeometryPoint);
  destructor Destroy; override;
  protected
    fPOI: Integer;
    fGeometryPoint: TWDGeometryPoint; // owns
    function getGeoJSON2D(const aType: string): string; override;
    function getValidGeometry: Boolean; override;
    function getExtent: TWDExtent; override;
  public
    function Encode: TByteBuffer; override;
  public
    property POI: Integer read fPOI;
    property geometryPoint: TWDGeometryPoint read fGeometryPoint;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TGeometryLayerObject = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue: Double);
  destructor Destroy; override;
  protected
    fGeometry: TWDGeometry; // owns
    fValue: Double; // value to lookup color within palette of layer
  protected
    function getGeoJSON2D(const aType: string): string; override;
    function getValidGeometry: Boolean; override;
    function getExtent: TWDExtent; override;
  public
    function Encode: TByteBuffer; override;
  public
    property geometry: TWDGeometry read fGeometry;
    property value: Double read fValue;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TLayer = class(TScenarioElement)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
    const aObjectTypes, aGeometryType: string; aDiffRange: Double; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fObjects: TObjectDictionary<TWDID, TLayerObject>; // owns
    fGeometryType: string;
    fObjectsLock: TOmniMREW;
    fBasicLayer: Boolean;
    fDependentDiffLayers: TObjectList<TDiffLayer>; // refs
    fDiffRange: Double;
    fObjectTypes: string;
    fPreviewRequestTimer: TTimer;
    fSendRefreshTimer: TTImer;
  protected
    fLegendJSON: string;
    fQuery: string;
    fTilerLayer: TTilerLayer;
    function getJSON: string; override;
    function getObjectsJSON: string; virtual;
    function getPreviewBASE64: string; virtual;
    function getRefJSON: string; virtual;

    procedure handleTilerInfo(aTilerLayer: TTilerLayer);
  	procedure handleTilerRefresh(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
  	procedure handleTilerPreview(aTilerLayer: TTilerLayer);
  public
    property objects: TObjectDictionary<TWDID, TLayerObject> read fObjects;
    property geometryType: string read fGeometryType;
    property objectsLock: TOmniMREW read fObjectsLock;
    property basicLayer: Boolean read fBasicLayer;

    property objectsJSON: string read getObjectsJSON;
    property previewBASE64: string read getPreviewBASE64;
    //
    property objectTypes: string read fObjectTypes;
    property legendJSON: string read fLegendJSON write fLegendJSON;
    property query: string read fQuery write fQuery; // r/w
    property tilerLayer: TTilerLayer read fTilerLayer;
    property refJSON: string read getRefJSON;
    property diffRange: Double read fDiffRange;
    function uniqueObjectsTilesLink: string;
    function SliceType: Integer; virtual;
  public
    function FindObject(const aID: TWDID; out aObject: TLayerObject): Boolean;
    procedure AddObject(aObject: TLayerObject);
    procedure AddOrSetObject(aObject: TLayerObject);
    procedure RemoveObject(aObject: TLayerObject);
    function ExtractObject(aObject: TLayerObject): TLayerObject;
    procedure ClearObjects;
    function findNearestObject(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double; var aDistance: Double): TLayerObject;
    function findObjectsInCircle(const aDistanceLatLon: TDistanceLatLon; aX, aY, aRadius: Double; var aObjectsJSON: string): Integer;
    function findObjectsInGeometry(const aGeometryExtent: TWDExtent; aGeometry: TWDGeometry; var aObjectsJSON: string): Integer;

  public
    procedure signalObjects(aSender: TObject); virtual;
    procedure ReadObjectsDBSVGPaths(aQuery: TDataSet; aDefaultValue: Double);

    procedure RegisterOnTiler(aPersistent: Boolean; aSliceType: Integer; const aDescription: string; aEdgeLengthInMeters: Double=NaN; aPalette: TWDPalette=nil);

    procedure RegisterLayer; virtual; // override to call -> RegisterOnTiler(XX)
    procedure RegisterSlice; virtual; // override to call -> TTilerLayer.addSliceXX
    // registerLayer (override) -> registerOnTiler -> onTilerInfo -> RegisterSlice (override -> signalAddSlice) + signalObjects via timer

    procedure addDiffLayer(aDiffLayer: TDiffLayer);
    procedure removeDiffLayer(aDiffLayer: TDiffLayer);

    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; override;
  end;

  TKPI = class(TScenarioElement)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
  destructor Destroy; override;
  private
    fTitle: string;
    fSubtitle: string;
    fRanges: TArray<double>;
    fMeasures: TArray<double>;
    fMarkers: TArray<double>;
  protected
    function getJSON: string; override;
  public
    property title: string read fTitle write fTitle;
    property Subtitle: string read fSubtitle write fSubtitle;
    property Ranges: TArray<double> read fRanges write fRanges;
    property Measures: TArray<double> read fMeasures write fMeasures;
    property Markers: TArray<double> read fMarkers write fMarkers;

    procedure Update;
  end;

  TChartGroupNames = TArray<string>;

  TChartGroupRow = record
  class function Create(const aGroup: string; const aValues: TArray<Double>): TChartGroupRow; static;
  public
    group: string;
    values: TArray<Double>;
  end;

  TChartGroupValues = TArray<TChartGroupRow>;

  TChart = class(TScenarioElement)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; const aChartType: string);
  destructor Destroy; override;
  private
    fChartType: string;
    fGroupNames: TChartGroupNames;
    fGroupValues: TChartGroupValues;
  protected
    function getJSON: string; override;
  public
    property chartType: string read fChartType write fChartType;
    property groupNames: TChartGroupNames read fGroupNames write fGroupNames;
    property groupValues: TChartGroupValues read fGroupValues write fGroupValues;
  end;

  TMapView = record
  class function Create(aLat, aLon: Double; aZoom: Integer): TMapView; static;
  private
    fLat: Double;
    fLon: Double;
    fZoom: Integer;
  public
    property lat: Double read fLat;
    property lon: Double read fLon;
    property zoom: Integer read fZoom;

    procedure DumpToLog;
  end;

  TScenario = class(TClientSubscribable)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
  destructor Destroy; override;
  protected
    fProject: TProject; // ref
    fID: string;
    fName: string;
    fDescription: string;
    fMapView: TMapView;
    fLayers: TObjectDictionary<string, TLayer>; // owns
    fKPIs: TObjectDictionary<string, TKPI>; // owns
    fCharts: TObjectDictionary<string, TChart>; // owns
    fAddbasicLayers: Boolean;
    function getElementID: string; override;
    function selectLayersOnCategories(const aSelectedCategories: TArray<string>; aLayers: TList<TLayer>): Boolean;
  public
    property project: TProject read fProject;
    property ID: string read fID;
    property name: string read fName write fName;
    property description: string read fDescription write fDescription;
    property mapView: TMapView read fMapView write fMapView;
    property Layers: TObjectDictionary<string, TLayer> read fLayers;
    property KPIs: TObjectDictionary<string, TKPI> read fKPIs;
    property Charts: TObjectDictionary<string, TChart> read fCharts;
    property addBasicLayers: Boolean read fAddBasicLayers;
    procedure ReadBasicData(); virtual;
    procedure RegisterLayers;
  public
    function AddLayer(aLayer: TLayer): TLayer;
    function AddKPI(aKPI: TKPI): TKPI;
    function AddChart(aChart: TChart): TChart;
  public
    // select objects
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aSelectedIDs: TArray<string>): string; overload; virtual;
    // select object properties
    function selectObjectsProperties(aClient: TClient; const aSelectedCategories, aSelectedObjects: TArray<string>): string; virtual;
  end;

  TScenarioLink = class
  constructor Create(aID, aParentID, aReferenceID: Integer; const aName, aDescription, aStatus: string; aLink: TScenario);
  destructor Destroy; override;
  private
    fChildren: TObjectList<TScenarioLink>;
    fLink: TScenario;
    fParent: TScenarioLink;
    fID: Integer;
    fParentID: Integer;
    fReferenceID: Integer;
    fName: string;
    fDescription: string;
    fStatus: string;
  public
    property children: TObjectList<TScenarioLink> read fChildren;
    property link: TScenario read fLink write fLink;
    property parent: TScenarioLink read fParent write fParent;
    // scenario
    property ID: Integer read fID;
    property parentID: Integer read fParentID;
    property referenceID: Integer read fReferenceID;
    //property name: string read fName;
    function name: string;
    property description: string read fDescription;
    property status: string read fStatus;

    function root: TScenarioLink;
    function findScenario(aID: Integer): TScenarioLink;
    function removeLeave(const aStatus: string): Boolean;
    procedure buildHierarchy();
    procedure sortChildren();
    function JSON: string;
  end;

  TMeasure = class
  constructor Create(const aID, aCategory, aTypology, aApplication, aDescription, aObjectType, aBenefits: string;
    aAction: Integer; aActionParameter: Double);
  destructor Destroy; override;
  private
    fID: string;
    fCategory: string;
    fTypology: string;
    fApplication: string;
    fDescription: string;
    fObjectType: string;
    fBenefits: string;
    fAction: Integer;
    fActionParameter: Double;
  public
    property ID: string read fID;
    property category: string read fCategory;
    property typology: string read fTypology;
    property application: string read fApplication;
    property description: string read fDescription;
    property objectType: string read fObjectType;
    property benefits: string read fBenefits;
    property action: Integer read fAction;
    property actionParameter: Double read fActionParameter;
  end;

  TProject = class
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    fMapView: TMapView;
    procedure setProjectName(const aValue: string);
    procedure setProjectDescription(const aValue: string);
    procedure setMapView(const aValue: TMapView);
  public
    property mapView: TMapView read fMapView write setMapView;
  protected
    fSessionModel: TSessionModel; // ref
    fConnection: TConnection; // ref
    fProjectID: string;
    fProjectName: string;
    fProjectDescription: string;
    fMaxNearestObjectDistanceInMeters: Integer;
    fTiler: TTiler;
    //fTilerEvent: TEventEntry;
    fDBConnection: TCustomConnection; // owns
    fProjectEvent: TEventEntry;
    fClients: TObjectList<TClient>; // owns, lock with TMonitor
    fTimers: TTimerPool;
    fDiffLayers: TObjectDictionary<string, TDiffLayer>; // owns, lock with TMonitor
    // data
    fMeasuresJSON: string;
    fMeasures: TObjectDictionary<string, TMeasure>; // owns
    fScenarios: TObjectDictionary<string, TScenario>; // owns, lock with TMonitor
    fScenarioLinks: TScenarioLink;
    fCurrentScenario: TScenario; // ref
    fRefScenario: TScenario; // ref
    fTimeSlider: Integer;
    fSelectionEnabled: Boolean;
    fMeasuresEnabled: Boolean;
    fMeasuresHistoryEnabled: Boolean;
    fSimualtionControlEnabled: Boolean;
    fAddBasicLayers: Boolean;
    procedure setTimeSlider(aValue: Integer);
    procedure setSelectionEnabled(aValue: Boolean);
    procedure setMeasuresEnabled(aValue: Boolean);
    procedure setMeasuresHistoryEnabled(aValue: Boolean);
    function getMeasuresJSON: string; virtual;
    function ReadScenario(const aID: string): TScenario; virtual;
    procedure handleTilerStartup(aTiler: TTiler; aStartupTime: TDateTime);
    procedure timerTilerStatusAsHeartbeat(aTimer: TTimer);
    procedure newClient(aClient: TClient); virtual;
    function getMeasuresHistoryJSON: string; virtual;
    procedure handleClientMessage(aJSONObject: TJSONObject); virtual;
  public
    function AddClient(const aClientID: string): TClient;
    procedure ReadBasicData(); virtual; abstract;
  public
    property Connection: TConnection read fConnection;
    property dbConnection: TCustomConnection read fDBConnection;
    property ProjectID: string read fProjectID;
    property ProjectName: string read fProjectName write setProjectName;
    property ProjectDescription: string read fProjectDescription write setProjectDescription;
    property ProjectEvent: TEventEntry read fProjectEvent;
    property maxNearestObjectDistanceInMeters: Integer read fMaxNearestObjectDistanceInMeters;
    //property TilerEvent: TEventEntry read fTilerEvent;
    property tiler: TTiler read fTiler;
    property Timers: TTimerPool read fTimers;
    property scenarios: TObjectDictionary<string, TScenario> read fScenarios; // owns, lock with TMonitor
    property clients: TObjectList<TClient> read fClients; // owns, lock with TMonitor

    property measuresJSON: string read getMeasuresJSON;
    property measures: TObjectDictionary<string, TMeasure> read fMeasures;

    property timeSlider: Integer read fTimeSlider write setTimeSlider;
    property selectionEnabled: Boolean read fSelectionEnabled write setSelectionEnabled;
    property measuresEnabled: Boolean read fMeasuresEnabled write setMeasuresEnabled;
    property measuresHistoryEnabled: Boolean read fMeasuresHistoryEnabled write setMeasuresHistoryEnabled;
    property simualtionControlEnabled: Boolean read fSimualtionControlEnabled write fSimualtionControlEnabled;
    property addBasicLayers: Boolean read fAddBasicLayers;
  public
    property diffLayers: TObjectDictionary<string, TDiffLayer> read fDiffLayers;
    function diffElementID(aCurrent, aReference: TScenarioElement): string;
  public
    procedure SendRefresh();
    procedure SendPreview();
    procedure SendString(const aString: string);
  end;

  // empty model control entries
  TModelParameters = class

  end;

  TModel = class
  constructor Create(aConnection: TConnection);
  private
    fConnection: TConnection;
    //procedure HandleParameterRequest(aParameters: TModelParameters); virtual; abstract;
    //procedure HandleStartModel(aParameters: TModelParameters); virtual; abstract;
    //procedure HandleStopModel; virtual; abstract;
    //procedure HandleQuitApplication; virtual; abstract;
    //procedure HandleRequestModels(const aReturnEventName: string); virtual; abstract;
  public
    property Connection: TConnection read fConnection;
  end;

  TSessionModel = class(TModel)
  constructor Create(aConnection: TConnection);
  destructor Destroy; override;
  private
    fProjects: TObjectList<TProject>;
  public
    property Projects: TObjectList<TProject> read fProjects;
    function FindElement(const aElementID: string): TScenarioElement;
  end;


function isObject(aJSONObject: TJSONObject; const aObjectName: string; var aJSONPair: TJSONPair): Boolean;
function isObjectValue(aJSONObject: TJSONObject; const aValueName: string; var aJSONValue: TJSONValue): Boolean;

procedure jsonAdd(var aCurrent: string; const aAdd: string);
function jsonArrayOfDoubleToStr(const aValues: TArray<double>): string;
function jsonString(const aText: string): string;
function geoJsonFeatureCollection(const aFeatures: string): string;
function geoJsonFeature(const aGeometry: string; const aProperties: string=''): string;

function ZoomLevelFromDeltaLon(aDeltaLon: Double): Integer;
function ZoomLevelFromDeltaLat(aDeltaLat: Double): Integer;

function BuildDiscreteLegendJSON(aPalette: TDiscretePalette; aLegendFormat: TLegendFormat): string;
function BuildRamplLegendJSON(aPalette: TRampPalette; aWidth: Integer=300; aLogScale: Boolean=False; aTickFontSize: Integer=11): string;
function CreateBasicPalette: TWDPalette;

implementation

{ utils }

function isObject(aJSONObject: TJSONObject; const aObjectName: string; var aJSONPair: TJSONPair): Boolean;
begin
  aJSONPair := aJSONObject.Get(aObjectName);
  Result := Assigned(aJSONPair);
end;

function isObjectValue(aJSONObject: TJSONObject; const aValueName: string; var aJSONValue: TJSONValue): Boolean;
begin
  aJSONValue := aJSONObject.Values[aValueName];
  Result := Assigned(aJSONValue);
end;

procedure jsonAdd(var aCurrent: string; const aAdd: string);
begin
  if aAdd<>'' then
  begin
    if aCurrent<>''
    then aCurrent := aCurrent+','+aAdd
    else aCurrent := aAdd;
  end;
end;

function jsonArrayOfDoubleToStr(const aValues: TArray<double>): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(aValues)-1
  do jsonAdd(Result, Double.ToString(aValues[i], dotFormat));
end;

function jsonString(const aText: string): string;
var
  jsonS: TJSONString;
begin
  jsonS := TJSONString.Create(aText);
  try
    Result := jsonS.toJSON;
  finally
    jsonS.Free;
  end;
end;

function geoJsonFeatureCollection(const aFeatures: string): string;
begin
  Result := '{"type":"FeatureCollection",'+'"features":['+afeatures+']'+'}';
end;

function geoJsonFeature(const aGeometry, aProperties: string): string;
begin
  Result := '{"type":"Feature","geometry":'+aGeometry;
  jsonAdd(Result, aProperties);
  Result := Result+'}';
end;

function ZoomLevelFromDeltaLon(aDeltaLon: Double): Integer;
// todo: need correction for lat?
begin
  try
    if aDeltaLon>0 then
    begin
      if aDeltaLon<=180 then
      begin
        Result := Trunc(ln(360/aDeltaLon)/ln(2));
        if Result>19
        then Result := 19;
      end
      else Result := 0;
    end
    else Result := 19;
  except
    Result := 0;
  end;
end;

function ZoomLevelFromDeltaLat(aDeltaLat: Double): Integer;
begin
  try
    if aDeltaLat>0 then
    begin
      if aDeltaLat<=180 then
      begin
        Result := Trunc(ln(360/aDeltaLat)/ln(2));
        if Result>19
        then Result := 19;
      end
      else Result := 0;
    end
    else Result := 19;
  except
    Result := 0;
  end;
end;

{ utils }

function BuildDiscreteLegendJSON(aPalette: TDiscretePalette; aLegendFormat: TLegendFormat): string;
var
  i: Integer;
begin
  Result := '';
  case aLegendFormat of
    lfVertical:
      begin
        for i := 0 to length(aPalette.entries)-1 do
        begin
          if Result<>''
          then Result := Result+',';
          Result := Result+'{"'+aPalette.entries[i].description+'":{'+aPalette.entries[i].colors.toJSON+'}}';
        end;
        Result := '"grid":{"title":"'+aPalette.description+'","labels":['+Result+']}';
      end;
    lfHorizontal:
      begin
        for i := 0 to length(aPalette.entries)-1 do
        begin
          if Result<>''
          then Result := Result+',';
          Result := Result+'{"'+aPalette.entries[i].description+'":{'+aPalette.entries[i].colors.toJSON+'}}';
        end;
        Result := '"grid2":{"title":"'+aPalette.description+'","labels":[{'+Result+'}]}';;
      end;
  end;
end;

function BuildDiffLegendJSON(
  const aTitle: string;
  aValueLess: Double; const aColorLess, aLabelLess: string;
  aValueNoChange: Double; const aColorNoChange, aLabelNoChange: string;
  aValueMore: Double; const aColorMore, aLabelMore: string): string;
begin
  Result :=
    '"scale": {'+
      '"width": "300px",'+
      '"title":'+jsonString(aTitle)+','+
      '"logScale":0,'+
      '"tickFontSize": "11px",'+
      '"gradients":['+
        '{"color":"'+aColorLess+'","position":'+aValueLess.ToString(dotFormat)+'},'+
        '{"color":"'+aColorNoChange+'","position":'+aValueNoChange.ToString(dotFormat)+'},'+
        '{"color":"'+aColorMore+'","position":'+aValueMore.toString(dotFormat)+'}'+
      '],'+
      '"labels":['+
        '{"description":"'+aLabelLess+'","position":'+aValueLess.ToString(dotFormat)+'},'+
        '{"description":"'+aLabelNoChange+'","position":'+aValueNoChange.ToString(dotFormat)+'},'+
        '{"description":"'+aLabelMore+'","position":'+aValueMore.ToString(dotFormat)+'}'+
      ']}';
end;

function BuildRamplLegendJSON(aPalette: TRampPalette; aWidth: Integer; aLogScale: Boolean; aTickFontSize: Integer): string;
var
  gradients: string;
  labels: string;
  e: TRampPaletteEntry;
begin
  gradients := '';
  labels := '';
  for e in aPalette.entries do
  begin
    if gradients<>''
    then gradients := gradients+',';
    gradients := gradients+'{"color":"'+ColorToJSON(e.color)+'","position":'+e.value.ToString(dotFormat)+'}';
    if labels<>''
    then labels := labels+',';
    labels := labels+'{"description":'+jsonString(e.description)+',"position":'+e.value.ToString(dotFormat)+'}';
  end;
  Result :=
    '"scale": {'+
      '"width": "'+aWidth.ToString+'px",'+
      '"title":'+jsonString(aPalette.description)+','+
      '"logScale":'+Ord(aLogScale).ToString+','+
      '"tickFontSize": "'+aTickFontSize.ToString+'px",'+
      '"gradients":['+gradients+
        //'{"color":"'+aColorLess+'","position":'+aValueLess.ToString(dotFormat)+'},'+
        //'{"color":"'+aColorNoChange+'","position":'+aValueNoChange.ToString(dotFormat)+'},'+
        //'{"color":"'+aColorMore+'","position":'+aValueMore.toString(dotFormat)+'}'+
      '],'+
      '"labels":['+labels+
        //'{"description":"'+aLabelLess+'","position":'+aValueLess.ToString(dotFormat)+'},'+
        //'{"description":"'+aLabelNoChange+'","position":'+aValueNoChange.ToString(dotFormat)+'},'+
        //'{"description":"'+aLabelMore+'","position":'+aValueMore.ToString(dotFormat)+'}'+
      ']'+
    '}';
end;

function CreateBasicPalette: TWDPalette;
begin
  Result := TDiscretePalette.Create('basic palette', [], TGeoColors.Create(colorBasicFill, colorBasicOutline));
end;

{ TDistanceLatLon }

class function TDistanceLatLon.Create(aLat1InDegrees, aLat2InDegrees: Double): TDistanceLatLon;
begin
  Result := Create(((aLat1InDegrees + aLat2InDegrees) / 2) * PI / 180); // average converted to radians
end;

class function TDistanceLatLon.Create(aLatMidInRad: Double): TDistanceLatLon;
begin
  Result.m_per_deg_lat := 111132.954 - 559.822 * Cos(2 * aLatMidInRad) + 1.175 * Cos(4 * aLatMidInRad);
  Result.m_per_deg_lon := 111132.954 * Cos(aLatMidInRad);
end;

function TDistanceLatLon.distanceInMeters(aDeltaLat, aDeltaLon: Double): Double;
begin
  Result := sqrt(sqr(m_per_deg_lat * aDeltaLat) + sqr(m_per_deg_lon * aDeltaLon));
end;

{ TClientSubscribable }

constructor TClientSubscribable.Create;
begin
  inherited Create;
  fClients := TObjectList<TClient>.Create(False); // refs
end;

destructor TClientSubscribable.Destroy;
var
  client: TClient;
begin
  TMonitor.Enter(fClients);
  try
    for client in fClients
    do client.HandleElementRemove(Self);
  finally
    TMonitor.Exit(fClients);
  end;
  FreeAndNil(fClients);
  inherited;
end;

function TClientSubscribable.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  TMonitor.Enter(fClients);
  try
    if fClients.IndexOf(aClient)<0 then
    begin
      fClients.Add(aClient);
      if fClients.Count=1
      then HandleFirstSubscriber;
      Result := True;
    end
    else Result := False;
  finally
    TMonitor.Exit(fClients);
  end;
end;

function TClientSubscribable.HandleClientUnsubscribe(aClient: TClient): Boolean;
var
  i: Integer;
begin
  TMonitor.Enter(fClients);
  try
    i := fClients.IndexOf(aClient);
    if i>=0 then
    begin
      fClients.Delete(i);
      if fClients.Count=0
      then HandleLastSubscriber;
      Result := True;
    end
    else Result := False;
  finally
    TMonitor.Exit(fClients);
  end;
end;

procedure TClientSubscribable.HandleFirstSubscriber;
begin
  // default no action
end;

procedure TClientSubscribable.HandleLastSubscriber;
begin
  // default no action
end;

{ TDiffLayer }

constructor TDiffLayer.Create(const aElementID: string; aCurrentLayer, aReferenceLayer: TLayer);
begin
  inherited Create;
  fLegendJSON := '';
  fCurrentLayer := aCurrentLayer;
  fReferenceLayer := aReferenceLayer;
  fTilerLayer := TTilerLayer.Create(aCurrentLayer.scenario.project.Connection, aElementID, -aCurrentLayer.SliceType);
  // create timers
  fPreviewRequestTimer := aCurrentLayer.scenario.project.Timers.SetTimer(
    procedure(aTimer: TTImer)
    begin
      Log.WriteLn('triggered preview timer for '+elementID);
      fTilerLayer.signalRequestPreview;
    end);
  fSendRefreshTimer := aCurrentLayer.scenario.project.Timers.CreateInactiveTimer;
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*30);
  // add event handlers
  fTilerLayer.onTilerInfo := handleTilerInfo;
  fTilerLayer.onRefresh := handleTilerRefresh;
  fTilerLayer.onPreview := handleTilerPreview;
  fCurrentLayer.addDiffLayer(Self);
  fReferenceLayer.addDiffLayer(Self);
  HandleSubLayerInfo(nil); // if both layers are known on tiler we can start registering now
end;

destructor TDiffLayer.Destroy;
begin
  fCurrentLayer.removeDiffLayer(Self);
  fReferenceLayer.removeDiffLayer(Self);
  FreeAndNil(fTilerLayer); // owns
  inherited;
  fCurrentLayer := nil; // ref
  fReferenceLayer := nil; // ref
end;

function TDiffLayer.getElementID: string;
begin
  Result := fTilerLayer.elementID;
end;

function TDiffLayer.getRefJSON: string;
begin
  Result := '"id":"'+getElementID+'","tiles":"'+fTilerLayer.URLTimeStamped+'"';
  if fLegendJSON<>''
  then Result := Result+',"legend":{'+legendJSON+'}';
end;

procedure TDiffLayer.HandleSubLayerInfo(aLayer: TLayer);
begin
  if (tilerLayer.URL='') and
     Assigned(fCurrentLayer.tilerLayer) and (fCurrentLayer.tilerLayer.URL<>'') and
     Assigned(fReferenceLayer.tilerLayer) and (fReferenceLayer.tilerLayer.URL<>'')
  then fTilerLayer.signalRegisterLayer(fCurrentLayer.scenario.project.tiler, 'diff-'+fCurrentLayer.Description+'-'+fReferenceLayer.description);
end;

procedure TDiffLayer.handleTilerInfo(aTilerLayer: TTilerLayer);
var
  diffPalette: TWDPalette;
  v: Double;
  title: string;
begin
  v := fCurrentLayer.diffRange;
  title := 'difference';
  if Assigned(fCurrentLayer.tilerLayer) and Assigned(fCurrentLayer.tilerLayer.palette)
  then title := title+', '+FormatLegendDescription(fCurrentLayer.tilerLayer.palette.description);
  diffPalette := TRampPalette.Create(title, [
    TRampPaletteEntry.Create($FF00FF00, -v, 'less'),
    TRampPaletteEntry.Create($FFFFFFFF, 0.0, 'no change'),
    TRampPaletteEntry.Create($FFFF0000, v, 'more')],
    $FF00FF00, 0, $FFFF0000);
  fLegendJSON := BuildDiffLegendJSON(
    diffPalette.description,
    -v, '#00FF00', (-v).ToString(dotFormat),
    0.0, '#FFFFFF', 'no change (0)',
    v, '#FF0000', v.ToString(dotFormat));
  aTilerLayer.signalAddDiffSlice(diffPalette, fCurrentLayer.tilerLayer.ID, fReferenceLayer.tilerLayer.ID);
end;

procedure TDiffLayer.handleTilerPreview(aTilerLayer: TTilerLayer);
var
  pvBASE64: string;
  client: TClient;
begin
  if Assigned(fTilerLayer) then
  begin
    pvBASE64 := fTilerLayer.previewAsBASE64;
    // layer clients
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.SendPreview(elementID, pvBASE64);
    finally
      TMonitor.Exit(clients);
    end;
    Log.WriteLn('send diff preview on '+elementID);
  end;
end;

procedure TDiffLayer.handleTilerRefresh(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
begin
  if Assigned(fSendRefreshTimer) then
  begin
    fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*5),
      procedure(aTimer: TTimer)
      var
        timeStampStr: string;
        tiles: string;
        client: TClient;
      begin
        if aTimeStamp<>0
        then timeStampStr := FormatDateTime('yyyy-mm-dd hh:mm', aTimeStamp)
        else timeStampStr := '';
        // signal refresh to layer client
        tiles := fTilerLayer.URLTimeStamped;

        Log.WriteLn('TDiffLayer.handleTilerRefresh for '+elementID+' ('+timeStampStr+'): '+tiles);

        TMonitor.Enter(clients);
        try
          for client in clients do
          begin
            client.SendRefresh(elementID {fCurrentLayer.elementID}, timeStampStr, tiles); // todo: extra processing needed?
            Log.WriteLn('TDiffLayer.handleTilerRefresh for '+elementID+', direct subscribed client: '+client.fClientID, llNormal, 1);
          end;
        finally
          TMonitor.Exit(clients);
        end;
        // signal current layer of diff layer refresh
        if Assigned(fCurrentLayer) then
        begin
          TMonitor.Enter(fCurrentLayer.clients);
          try
            for client in fCurrentLayer.clients do
            begin
              client.SendRefreshDiff(fCurrentLayer.elementID, timeStampStr, Self.refJSON);
              Log.WriteLn('TDiffLayer.handleTilerRefresh for '+elementID+', current layer subscribed client: '+client.fClientID, llNormal, 1);
            end;
          finally
            TMonitor.Exit(fCurrentLayer.clients);
          end;
        end;
      end);
  end;
  // refresh preview also
  fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*10);
end;

{ TClientDomain }

class function TClientDomain.Create(const aName: string): TClientDomain;
begin
  Result.name := aName;
  Result.kpis := '';
  Result.charts := '';
  Result.layers := '';
  Result.enabled := 0;
end;

function TClientDomain.getJSON: string;
begin
  Result := '"'+name+'":{"enabled":'+enabled.ToString+',"layers":['+layers+'],"kpis":['+kpis+'],"charts": ['+charts+']}';
end;

{ TClient }

function TClient.activeScenario: string;
begin
  if Assigned(fCurrentScenario)
  then Result := '"activeScenario":"'+fCurrentScenario.id+'",'
  else Result := '';
end;

procedure TClient.addClient(aElement: TClientSubscribable);
begin
  if Assigned(aElement) then
  begin
    TMonitor.Enter(fSubscribedElements);
    try
      if fSubscribedElements.IndexOf(aElement)<0
      then fSubscribedElements.Add(aElement);
    finally
      TMonitor.Exit(fSubscribedElements);
    end;
    aElement.HandleClientSubscribe(Self);
  end;
end;

constructor TClient.Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
begin
  inherited Create;
  fSubscribedElements := TObjectList<TClientSubscribable>.Create(False);
  fProject := aProject;
  fClientID := aClientID;
  fCurrentScenario := aCurrentScenario;
  fRefScenario := aRefScenario;
  addClient(fCurrentScenario);
  addClient(fRefScenario);
  if Assigned(fCurrentScenario) and Assigned(fRefScenario) then
  begin
    // todo: create list diff layers, charts and kpis
  end;
  fClientEvent := fProject.fConnection.publish(aClientID, False);
  fClientEvent.subscribe;
  // add handler for disconnecting clients
  fClientEvent.OnIntString.Add(
    procedure(event: TEventEntry; aInt: Integer; const aString: string)
    begin
      if aInt=actionDelete then
      begin
        Log.WriteLn('unlink from '+event.eventName);
        TMonitor.Enter(fProject.fClients);
        try
          fClientEvent := nil;
          event.unPublish;
          event.unSubscribe;
          fProject.fClients.Remove(Self);
        finally
          TMonitor.Exit(fProject.fClients);
        end;
      end;
    end);
  // add handler for client commands
  fClientEvent.OnString.Add(
    procedure(event: TEventEntry; const aString: string)
    begin
      HandleClientCommand(aString);
    end);
  fClientEvent.OnStreamCreate :=
    function(aEventEntry: TEventEntry; const aName: string): TStream
    begin
      Result := TStringStream.Create('', TEncoding.UTF8, False);
    end;
  fClientEvent.OnStreamEnd :=
    procedure(aEventEntry: TEventEntry; const aName: string; var aStream: TStream; aCancel: Boolean)
    begin
      handleClientCommand((aStream as TStringStream).DataString);
    end;
  // trigger login
  signalString('{"login": {}}');
  // do default registration
  SendSession();
  SendMeasures();
  SendDomains('domains');
  fProject.newClient(Self);
end;

destructor TClient.Destroy;
var
  se: TClientSubscribable;
begin
  if Assigned(fClientEvent) then
  begin
    fClientEvent.signalIntString(actionDelete, '');
    fClientEvent.unSubscribe;
    fClientEvent.unPublish;
    fClientEvent := nil;
  end;
  TMonitor.Enter(fSubscribedElements);
  try
    for se in fSubscribedElements
    do se.HandleClientUnsubscribe(Self);
  finally
    TMonitor.Exit(fSubscribedElements);
  end;
  FreeAndNil(fSubscribedElements);
  fCurrentScenario := nil;
  fRefScenario := nil;
  inherited;
end;

function TClient.getSessionModel: TSessionModel;
begin
  Result := fProject.fSessionModel;
end;

procedure TClient.HandleClientCommand(const aJSONString: string);

  procedure login(aJSONObject: TJSONObject);
  var
    scenarioID: string;
    userid: string;
    scenario: TScenario;
  begin
    scenarioID := aJSONObject.GetValue<string>('scenario');
    if scenarioID<>'' then
    begin
      userid := aJSONObject.GetValue<string>('userid');
      if fProject.scenarios.TryGetValue(scenarioID, scenario) then
      begin
        removeClient(fCurrentScenario);
        fCurrentScenario := scenario;
        addClient(fCurrentScenario);
        Log.WriteLn('connected to scenario '+scenarioID+' user '+userid);
        // retry
        SendSession();
        //SendMeasures(); // todo:?
        SendDomains('domains');
      end
      else Log.WriteLn('could not connect to scenario '+scenarioID+' user '+userid, llError);
    end;
  end;

  procedure selectScenario(aJSONObject: TJSONObject);
  var
    scenarioID: string;
    scenario: TScenario;
    jsonValue: TJSONValue;
    //  diffLayerID: string;
    //  diffLayer: TDiffLayer;
    //  ilp: TPair<string, TLayer>;
    //  refLayer: TLayer;
    //  diffPalette: TWDPalette;
  begin
    if isObjectValue(aJSONObject, 'currentScenario', jsonValue) then
    begin
      removeClient(fCurrentScenario);
      scenarioID := jsonValue.Value;
      if not fProject.scenarios.TryGetValue(scenarioID, scenario)
      then scenario := fProject.ReadScenario(scenarioID);
      fCurrentScenario := scenario;
      addClient(fCurrentScenario);
      if Assigned(fCurrentScenario)
      then Log.WriteLn('client switched scenario to '+fCurrentScenario.elementID)
      else Log.WriteLn('client switched scenario to NONE');
    end;
    if isObjectValue(aJSONObject, 'referenceScenario', jsonValue) then
    begin
      removeClient(fRefScenario);
      scenarioID := jsonValue.Value;
      if not fProject.scenarios.TryGetValue(scenarioID, scenario)
      then scenario := fProject.ReadScenario(scenarioID{, fCurrentScenario});
      fRefScenario := scenario;
      addClient(fRefScenario);
      if Assigned(fRefScenario)
      then Log.WriteLn('client switched ref scenario to '+fRefScenario.elementID)
      else Log.WriteLn('client switched ref scenario to NONE');
      {
        // todo: create list diff layers, charts and kpis
        if Assigned(fCurrentScenario) then
        begin
          for ilp in fCurrentScenario.Layers do
          begin
            if fRefScenario.Layers.TryGetValue(ilp.Key, refLayer) then
            begin
              diffLayerID := fProject.diffElementID(ilp.Value, refLayer);
              TMonitor.Enter(fProject.diffLayers);
              try
                if not fProject.diffLayers.TryGetValue(diffLayerID, diffLayer) then
                begin
                  diffLayer := TDiffLayer.Create(diffLayerID, ilp.Value, refLayer);
                  fProject.diffLayers.Add(diffLayerID, diffLayer);
                end;
                addClient(diffLayer);
              finally
                TMonitor.Exit(fProject.diffLayers);
              end;
            end;
          end;
        end;
      end
      else
      }
    end;
    SendDomains('updatedomains');
    UpdateSession();
  end;

  procedure selectObjects(aSelectObjects: TJSONValue);
  var
    t: string;
    m: string;
    g: TJSONObject;
    measure: TJSONObject;
    q: string;
    rStr: string;
    r: Double;
    x, y: Double;
    geometry: TWDGeometry;
    part: TWDGeometryPart;
    c, c2: TJSONArray;
    pointI: Integer;
    sca: TJSONArray;
    sc: TArray<string>;
    oids: TArray<string>;
    i: Integer;
    resp: string;
  begin
    // decode selection and send back objects
    resp := '';
    // type
    t := aSelectObjects.getValue<string>('type', '');
    // mode
    m := aSelectObjects.getValue<string>('mode', '');
    // selectCategories
    sca := aSelectObjects.getValue<TJSONArray>('selectCategories');
    setLength(sc, sca.count);
    for i := 0 to sca.count-1
    do sc[i] := sca.Items[i].Value;
    // radius
    rStr := aSelectObjects.getValue<string>('radius', '');
    // geometry
    g := aSelectObjects.getValue<TJSONObject>('geometry', nil);
    if Assigned(g) then
    begin
      c := g.Values['geometry'].getValue<TJSONArray>('coordinates');
      // select objects based on geometry
      // always polygon or point (radius)? -> simplyfy
      if c.Count=1 then
      begin
        // 1 part with coordinates
        c2 := c.Items[0] as TJSONArray;
        geometry := TWDGeometry.Create;
        try
          part := geometry.AddPart;
          for pointI := 0 to c2.Count-1 do
          begin
            x := Double.Parse(TJSONArray(c2.Items[pointI]).Items[0].tostring, dotFormat);
            y := Double.Parse(TJSONArray(c2.Items[pointI]).Items[1].tostring, dotFormat);
            part.AddPoint(x, y, NaN);
          end;
          // todo:
          if Assigned(fCurrentScenario) then
          begin
            resp := fCurrentScenario.SelectObjects(Self, t, m, sc, geometry);
          end;
        finally
          geometry.Free;
        end;
      end
      else
      begin
        // must be point
        x := Double.Parse(TJSONArray(c).Items[0].tostring, dotFormat);
        y := Double.Parse(TJSONArray(c).Items[1].tostring, dotFormat);
        if rStr<>''
        then r := Double.Parse(rStr, dotFormat)
        else r := 0;
        // todo:
        if Assigned(fCurrentScenario) then
        begin
          resp := fCurrentScenario.SelectObjects(Self, t, m, sc, x, y, r);
        end;
      end;
    end
    else
    begin
      measure := aSelectObjects.getValue<TJSONObject>('measure', nil);
      if Assigned(measure) then
      begin
        // todo: select objects based on given measure

        if Assigned(fCurrentScenario) then
        begin
          // decode objects from measure                         \
          setLength(oids, 0);
          resp := fCurrentScenario.SelectObjects(Self, t, m, sc, oids);
        end;
      end
      else
      begin
        q := aSelectObjects.getValue<string>('query', '');
        // select objects based on query
        if Assigned(fCurrentScenario) then
        begin
          resp := fCurrentScenario.SelectObjects(Self, t, m, sc, q);
        end;
      end;
    end;
    if resp<>'' then
    begin
      signalString(resp);
    end;
  end;

  procedure selectObjectsProperties(aSelectObjectsProperties: TJSONValue);
  var
    sca: TJSONArray;
    sc: TArray<string>;
    i: Integer;
    so: TArray<string>;
    resp: string;
  begin
    // selectCategories
    sca := aSelectObjectsProperties.getValue<TJSONArray>('selectedCategories');
    setLength(sc, sca.count);
    for i := 0 to sca.count-1
    do sc[i] := sca.Items[i].Value;
    // selectedObjects
    sca := aSelectObjectsProperties.getValue<TJSONArray>('selectedObjects');
    setLength(so, sca.count);
    for i := 0 to sca.count-1
    do so[i] := sca.Items[i].Value;
    if Assigned(fCurrentScenario) then
    begin
      resp := fCurrentScenario.selectObjectsProperties(Self, sc, so);
      if resp<>''
      then signalString(resp);
    end;
  end;

var
  jsonObject: TJSONObject;
  jsonPair: TJSONPair;
begin
  try
    if Assigned(fProject) and Assigned(fClientEvent) then
    begin
      //Log.WriteLn('message from client: '+event.eventName+': '+aString);
      // process message
      jsonObject := TJSONObject.ParseJSONValue(aJSONString) as TJSONObject;
      try
        if isObject(jsonObject, 'subscribe', jsonPair)
        then addClient(sessionModel.FindElement(jsonPair.JsonValue.Value))
        else if isObject(jsonObject, 'unsubscribe', jsonPair)
        then removeClient(sessionModel.FindElement(jsonPair.JsonValue.Value))
        else if isObject(jsonObject, 'selectObjects', jsonPair)
        then selectObjects(jsonPair.JsonValue)
        else if isObject(jsonObject, 'login', jsonPair)
        then login(jsonPair.JsonValue as TJSONObject)
        else if isObject(jsonObject, 'selectScenario', jsonPair)
        then selectScenario(jsonPair.JsonValue as TJSONObject)
        else if isObject(jsonObject, 'selectObjectsProperties', jsonPair)
        then selectObjectsProperties(jsonPair.JsonValue)
        // handle all other messages on the connected project
        else fProject.handleClientMessage(jsonObject);

//        if isObject(jsonObject, 'applyMeasures', jsonPair)
//        then applyMeasures(jsonPair.JsonValue as TJSONObject)
//        else ;

        // todo:
        // setObjectProperties
        // applyMeasures
        // selectedTime: "now" | "yyyy-mm-dd hh:nn"

        // rest
      finally
        jsonObject.Free;
      end;
    end;
  except
    on e: Exception
    do Log.WriteLn('exception in TClient.Create: fClientEvent.OnString: '+e.Message, llError);
  end;
end;

procedure TClient.HandleElementRemove(aElement: TClientSubscribable);
var
  i: Integer;
begin
  if Assigned(Self) and Assigned(fSubscribedElements) then
  begin
    TMonitor.Enter(fSubscribedElements);
    try
      if Assigned(Self) and Assigned(fSubscribedElements) then
      begin
        i := fSubscribedElements.IndexOf(aElement);
        if i>=0
        then fSubscribedElements.Delete(i);
      end;
    finally
      TMonitor.Exit(fSubscribedElements);
    end;
  end;
end;

procedure TClient.HandleScenarioRemove(aScenario: TScenario);
begin
  // todo: implement
end;

function TClient.referenceScenario: string;
begin
  if Assigned(fRefScenario)
  then Result := '"referenceScenario":"'+fRefScenario.id+'",'
  else Result := '';
end;

procedure TClient.removeClient(aElement: TClientSubscribable);
var
  i: Integer;
begin
  if Assigned(aElement) then
  begin
    TMonitor.Enter(fSubscribedElements);
    try
      i := fSubscribedElements.IndexOf(aElement);
      if i>=0
      then fSubscribedElements.Delete(i);
    finally
      TMonitor.Exit(fSubscribedElements);
    end;
    aElement.HandleClientUnsubscribe(Self);
  end;
end;

function compareLayerNames(const aLayer1, aLayer2: TLayer): Integer;
begin
  Result := AnsiCompareText(aLayer1.name, aLayer2.name);
end;

procedure TClient.SendDomains(const aPrefix: string);
var
  d: TClientDomain;
  domains: TDictionary<string, TClientDomain>;
  JSON: string;
  layer: TLayer;
  ndp: TPair<string, TClientDomain>;
  domainsJSON: string;
  nkp: TPair<string, TKPI>;
  ngp: TPair<string, TChart>;
  locLayers: TList<TLayer>;
  refLayer: TLayer;
  diffLayer: TDiffLayer;
  diffElementID: string;
begin
  if Assigned(fRefScenario)
  then Log.WriteLn('TClient.SendDomains ('+aPrefix+'), ref scenario '+fRefScenario.fID)
  else Log.WriteLn('TClient.SendDomains ('+aPrefix+'): no ref scenario');
  // todo: add reference and diff layers/charts if fRefScenario<>nil
  domains := TDictionary<string, TClientDomain>.Create;
  try
    if Assigned(fCurrentScenario) then
    begin
      // layers
      locLayers := TList<TLayer>.Create(TComparer<TLayer>.Construct(compareLayerNames));
      try
        locLayers.AddRange(fCurrentScenario.Layers.Values);
        locLayers.Sort;
        for layer in locLayers do
        begin
          JSON := layer.JSON;
          if Assigned(fRefScenario) then
          begin
            if fRefScenario.Layers.TryGetValue(layer.ID, refLayer) then
            begin
              // todo: full JSON for ref and diff, to include legend?
              JSON := JSON+',"ref":{'+refLayer.refJSON+'}';
              diffElementID :=  fProject.diffElementID(layer, refLayer);
              if not fProject.diffLayers.TryGetValue(diffElementID, diffLayer) then
              begin
                diffLayer := TDiffLayer.Create(diffElementID, layer, refLayer);
                fProject.diffLayers.Add(diffElementID, diffLayer);
                // todo:

              end;
              // todo: temp removed for testing
              JSON := JSON+',"diff":{'+diffLayer.refJSON+'}';
            end
            else Log.WriteLn('TClient.SendDomains ('+aPrefix+'): no ref layer for '+layer.ID);
          end;
          JSON  := '{'+JSON+'}';
          if domains.TryGetValue(layer.domain, d) then
          begin
            jsonAdd(d.layers, JSON);
            domains[layer.domain] := d;
          end
          else
          begin
            d := TClientDomain.Create(layer.domain);
            d.layers := JSON;
            domains.Add(d.name, d);
          end;
        end;
      finally
        locLayers.Free;
      end;
      // kpis
      for nkp in fCurrentScenario.fKPIs do
      begin
        JSON := '{'+nkp.Value.JSON+'}';
        if domains.TryGetValue(nkp.Value.domain, d) then
        begin
          jsonAdd(d.kpis, JSON);
          domains[nkp.Value.domain] := d;
        end
        else
        begin
          d := TClientDomain.Create(nkp.Value.domain);
          d.kpis := JSON;
          domains.Add(d.name, d);
        end;
      end;
      // charts
      for ngp in fCurrentScenario.fCharts do
      begin
        JSON := '{'+ngp.Value.JSON+'}';
        if domains.TryGetValue(ngp.Value.domain, d) then
        begin
          jsonAdd(d.charts, JSON);
          domains[ngp.Value.domain] := d;
        end
        else
        begin
          d := TClientDomain.Create(ngp.Value.domain);
          d.charts := JSON;
          domains.Add(d.name, d);
        end;
      end;
    end;
    domainsJSON := '';
    for ndp in domains do
    begin
      d := ndp.Value;
      // extra check to make domains enabled by default through entry in exe ini
      // todo: use const
      d.enabled := standardIni.ReadInteger('DefaultEnabledDomains', ndp.Key, 0);
      jsonAdd(domainsJSON, d.JSON);
    end;
    signalString('{"'+aPrefix+'":{'+domainsJSON+'}}'); // default prefix is "domains":..
  finally
    domains.Free;
  end;
end;

procedure TClient.SendErrorMessage(const aMessage: string);
begin
  signalString('{"connection":{"message":"'+aMessage+'"}}');
end;

procedure TClient.SendMeasures;
var
  mj: string;
begin
  try
    mj := '{"measures":'+fProject.measuresJSON+'}';
    signalString(mj);
  except
    Log.WriteLn('Could not read measures from database', llError);
    mj := '{"measures":{}}';
    signalString(mj);
    SendErrorMessage('Could not read measures');
  end;
end;

procedure TClient.SendMeasuresEnabled;
begin
  signalString(
    '{"session":{'+
      '"measuresEnabled":'+Ord(fProject.measuresEnabled).toString+
    '}}');
end;

procedure TClient.SendMeasuresHistory;
begin
  signalString('{"addhistorymeasures":['+fProject.getMeasuresHistoryJSON+']}');
end;

procedure TClient.SendMeasuresHistoryEnabled;
begin
  signalString(
    '{"session":{'+
      '"measuresHistoryEnabled":'+Ord(fProject.measuresHistoryEnabled).toString+
    '}}');
end;

procedure TClient.SendPreview(const aElementID, aPreviewBASE64: string);
begin
  signalString('{"refresh":"'+aElementID+'","preview":"'+aPreviewBASE64+'"}');
end;

procedure TClient.SendRefresh(const aElementID, aTimeStamp, aObjectsTilesLink: string);
begin
  signalString('{"refresh":"'+aElementID+'","timestamp":"'+aTimeStamp+'","tiles":"'+aObjectsTilesLink+'"}');
end;

procedure TClient.SendRefreshDiff(const aElementID, aTimeStamp, aDiff: string);
begin
  signalString('{"refresh":"'+aElementID+'","timestamp":"'+aTimeStamp+'","diff":{'+aDiff+'}}');
end;

procedure TClient.SendRefreshRef(const aElementID, aTimeStamp, aRef: string);
begin
  // todo: how te see that a layer is a ref layer for this client?
  signalString('{"refresh":"'+aElementID+'","timestamp":"'+aTimeStamp+'","ref":{'+aRef+'}}');
end;

procedure TClient.SendSelectionEnabled;
begin
  signalString(
    '{"session":{'+
      '"selectionEnabled":'+Ord(fProject.selectionEnabled).toString+
    '}}');
end;

procedure TClient.SendSession;

  function view: string;
  begin
    if Assigned(fCurrentScenario) then Result :=
      '"view":{'+
        '"lat":'+Double.ToString(fCurrentScenario.fMapView.lat, dotFormat)+','+
        '"lon":'+Double.ToString(fCurrentScenario.fMapView.lon, dotFormat)+','+
        '"zoom":'+fCurrentScenario.fMapView.zoom.ToString+
      '}'
    else Result :=
      '"view":{'+
        '"lat":'+Double.ToString(fProject.fMapView.lat, dotFormat)+','+
        '"lon":'+Double.ToString(fProject.fMapView.lon, dotFormat)+','+
        '"zoom":'+fProject.fMapView.zoom.ToString+
      '}'
  end;

begin
  signalString(
    '{"session":{'+
      '"description":"'+sessionDescription+'",'+
      activeScenario+
      referenceScenario+
      '"scenarios":'+fProject.fScenarioLinks.JSON+','+
      '"timeslider":'+fProject.timeSlider.toString+','+
      '"selectionEnabled":'+ord(fProject.selectionEnabled).toString+','+
      '"measuresEnabled":'+ord(fProject.measuresEnabled).toString+','+
      '"measuresHistoryEnabled":'+ord(fProject.measuresHistoryEnabled).toString+','+
      '"simulationControlEnabled":'+ord(fProject.simualtionControlEnabled).ToString+','+
      view+
    '}}');
end;

procedure TClient.SendSimulationControlEnabled;
begin
  signalString(
    '{"session":{'+
      '"simulationControlEnabled":'+ord(fProject.simualtionControlEnabled).ToString+
    '}}');
end;

procedure TClient.SendTimeSlider;
begin
  signalString(
    '{"session":{'+
      '"timeslider":'+fProject.timeSlider.toString+
    '}}');
end;

function TClient.sessionDescription: string;
begin
  Result := fProject.ProjectName;
  if Assigned(fCurrentScenario) then
  begin
    if fCurrentScenario.name<>''
    then Result := Result+' - '+fCurrentScenario.name;
    if fCurrentScenario.description<>'' then
    begin
      if fCurrentScenario.name<>''
      then Result := Result+', '+fCurrentScenario.description
      else Result := Result+' - '+fCurrentScenario.description;
    end;
  end;
end;

procedure TClient.signalString(const aString: string);
var
  stream: TStream;
begin
  // signal large string via stream
  if length(aString)>imbMaximumPayloadSize/10 then
  begin
    stream := TStringStream.Create(UTF8String(aString));
    try
      stream.Position := 0;
      fClientEvent.signalStream('string', stream);
    finally
      stream.Free;
    end;
  end
  else fClientEvent.signalString(aString);
end;

procedure TClient.UpdateSession;
begin
  signalString(
    '{"session":{'+
      '"description":"'+sessionDescription+'",'+
      activeScenario+
      referenceScenario+
      '"scenarios":'+fProject.fScenarioLinks.JSON+
    '}}');
end;

{ TScenarioElement }

constructor TScenarioElement.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
begin
  inherited Create;
  fScenario := aScenario;
  fDomain := aDomain;
  fID := aID;
  fName := aName;
  fDescription := aDescription;
  fDefaultLoad := aDefaultLoad;
  //
//  fOutputEvent := fScenario.Project.Connection.publish(fScenario.Project.ProjectEvent.eventName+'.'+fScenario.ID+'.'+ID, false);
end;

destructor TScenarioElement.Destroy;
begin
//  if Assigned(fOutputEvent) then
//  begin
//    fOutputEvent.unPublish;
//    fOutputEvent := nil;
//  end;
  inherited;
end;

function TScenarioElement.getElementID: string;
begin
  Result := ID;
  if Assigned(scenario)
  then Result := fScenario.elementID+sepElementID+Result;
end;

function TScenarioElement.getJSON: string;
begin
  Result :=
    '"domain":"'+domain+'",'+
    '"id":"'+getElementID+'",'+
    '"name":"'+name+'",'+
    '"description":"'+description+'",'+
    '"default":'+Ord(defaultLoad).ToString;
end;

{ TLayerObject }

constructor TLayerObject.Create(aLayer: TLayer; const aID: TWDID);
begin
  inherited Create;
  fLayer := aLayer;
  fID := aID;
end;

{function TLayerObject.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
begin
  Result := Infinity;
end;}

function TLayerObject.Encode: TByteBuffer;
begin
  Result := TByteBuffer.bb_tag_rawbytestring(icehObjectID, ID);
end;

function TLayerObject.getExtent: TWDExtent;
begin
  Result := TWDExtent.Create;
end;

function TLayerObject.getValidGeometry: Boolean;
begin
  Result := False;
end;

{ TGeometryPointLayerObject }

constructor TGeometryPointLayerObject.Create(aLayer: TLayer; const aID: TWDID; aGeometryPoint: TWDGeometryPoint; aValue: Double);
begin
  inherited Create(aLayer, aID);
  fGeometryPoint := aGeometryPoint;
  fValue := aValue;
end;

destructor TGeometryPointLayerObject.Destroy;
begin
  FreeAndNil(fGeometryPoint);
  inherited;
end;

function TGeometryPointLayerObject.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
begin
  Result := aDistanceLatLon.distanceInMeters(Abs(fGeometryPoint.y-aY), Abs(fGeometryPoint.x-aX));
end;

function TGeometryPointLayerObject.Encode: TByteBuffer;
begin
  Result :=
    TByteBuffer.bb_tag_Double(icehTilerValue, value)+
    TByteBuffer.bb_tag_rawbytestring(icehTilerGeometryPoint, fGeometryPoint.Encode)+
    inherited Encode;
end;

function TGeometryPointLayerObject.getExtent: TWDExtent;
begin
  Result.Init(fGeometryPoint.x, fGeometryPoint.y);
end;

function TGeometryPointLayerObject.getGeoJSON2D(const aType: string): string;
var
  colors: TGeoColors;
  jsonOpacity: string;
begin
  if Assigned(fGeometryPoint) then
  begin
    if Assigned(fLayer.tilerLayer) and Assigned(fLayer.tilerLayer.palette)
    then colors := fLayer.tilerLayer.palette.ValueToColors(fValue)
    else colors := TGeoColors.Create($FF000000); // black
    Result := '{ '+
      '"type":"Feature",'+
      '"geometry":{'+
        '"type":"'+fLayer.GeometryType+'",'+
        '"coordinates":'+fGeometryPoint.GeoJSON2D[aType]+
      '},'+
      '"properties":{'+
        '"id":"'+string(ID)+'",'+
        '"color": "'+ColorToJSON(colors.mainColor)+'",'+
        '"fillOpacity":'+Double((colors.mainColor shr 24)/$FF).ToString(dotFormat)+
      '}'+
    '}'
  end
  else Result := '';
end;

function TGeometryPointLayerObject.getValidGeometry: Boolean;
begin
  Result := Assigned(fGeometryPoint);
end;

function TGeometryPointLayerObject.intersects(aGeometry: TWDGeometry): Boolean;
begin
  Result := aGeometry.PointInAnyPart(fGeometryPoint.x, fGeometryPoint.y);
end;

{ TGeometryLayerPOIObject }

constructor TGeometryLayerPOIObject.Create(aLayer: TLayer; const aID: TWDID; aPOI: Integer; aGeometryPoint: TWDGeometryPoint);
begin
  inherited Create(aLayer, aID);
  fPOI := aPOI;
  fGeometryPoint := aGeometryPoint;
end;

destructor TGeometryLayerPOIObject.Destroy;
begin
  FreeAndNil(fGeometryPoint);
  inherited;
end;

function TGeometryLayerPOIObject.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
begin
  Result := aDistanceLatLon.distanceInMeters(Abs(fGeometryPoint.y-aY), Abs(fGeometryPoint.x-aX));
end;

function TGeometryLayerPOIObject.Encode: TByteBuffer;
begin
  Result :=
    TByteBuffer.bb_tag_int32(icehTilerPOI, poi)+
    TByteBuffer.bb_tag_rawbytestring(icehTilerGeometryPoint, fGeometryPoint.Encode)+
    inherited Encode;
end;

function TGeometryLayerPOIObject.getExtent: TWDExtent;
begin
  Result.Init(fGeometryPoint.x, fGeometryPoint.y);
  // todo: inflate for size of poi?
end;

function TGeometryLayerPOIObject.getGeoJSON2D(const aType: string): string;
begin
  if Assigned(fGeometryPoint) then
    Result := '{ '+
      '"type":"Feature",'+
      '"geometry":{'+
        '"type":"'+fLayer.GeometryType+'",'+
        '"coordinates":'+fGeometryPoint.GeoJSON2D[aType]+
      '},'+
      '"properties":{'+
        '"id":"'+string(ID)+'",'+
        '"poi": "'+fPOI.ToString+'"'+
      '}'+
    '}'
  else
    Result := '';
end;

function TGeometryLayerPOIObject.getValidGeometry: Boolean;
begin
  Result := Assigned(fGeometryPoint);
end;

function TGeometryLayerPOIObject.intersects(aGeometry: TWDGeometry): Boolean;
begin
  Result := aGeometry.PointInAnyPart(fGeometryPoint.x, fGeometryPoint.y);
end;

{ TGeometryLayerObject }

constructor TGeometryLayerObject.Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue: Double);
begin
  inherited Create(aLayer, aID);
  fGeometry := aGeometry;
  fValue := aValue;
end;

destructor TGeometryLayerObject.Destroy;
begin
  FreeAndNil(fGeometry);
  inherited;
end;

// http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
function pDistance(const aDistanceLatLon: TDistanceLatLon; x, y, x1, y1, x2, y2: Double): Double;
var
  A, B, C, D: Double;
  dot, len_sq: Double;
  param: Double;
  xx, yy: Double;
  dx, dy: Double;
begin
  A := (x - x1)*aDistanceLatLon.m_per_deg_lon;
  B := (y - y1)*aDistanceLatLon.m_per_deg_lat;
  C := (x2 - x1)*aDistanceLatLon.m_per_deg_lon;
  D := (y2 - y1)*aDistanceLatLon.m_per_deg_lat;

  dot := A * C + B * D;
  len_sq := C * C + D * D;

  if (len_sq <> 0) //in case of 0 length line
  then param := dot / len_sq
  else param := -1;

  if (param < 0) then
  begin
    xx := x1;
    yy := y1;
  end
  else if (param > 1) then
  begin
    xx := x2;
    yy := y2;
  end
  else
  begin
    xx := x1 + param * (x2 - x1);
    yy := y1 + param * (y2 - y1);
  end;

  dx := (x - xx)*aDistanceLatLon.m_per_deg_lon;
  dy := (y - yy)*aDistanceLatLon.m_per_deg_lat;
  Result := sqrt(dx * dx + dy * dy);
end;

function TGeometryLayerObject.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
var
  part: TWDGeometryPart;
  //point: TWDGeometryPoint;
  d: Double;
  p: Integer;
  x1,y1,x2,y2: Double;
begin
  // todo: poor man solution for now, just distance to any point of geometry..
  if (fLayer.fGeometryType='MultiPolygon') and fGeometry.PointInAnyPart(aX, aY)
  then Result := 0
  else
  begin
    Result := Infinity;
    for part in fGeometry.parts do
    begin
      {
      for point in part.points do
      begin
        d := aDistanceLatLon.distanceInMeters(Abs(point.y-aY), Abs(point.x-aX));
        if Result>d
        then Result := d;
      end;
      }
      if part.points.Count>0 then
      begin
        if part.points.Count>1 then
        begin
          // process all segments
          x2 := part.points[0].x;
          y2 := part.points[0].y;
          for p := 1 to part.points.Count-1 do
          begin
            x1 := x2;
            y1 := y2;
            x2 := part.points[p].x;
            y2 := part.points[p].y;
            {
            d := Abs( ((x2-x1)*aDistanceLatLon.m_per_deg_lon*(y1-aY)*aDistanceLatLon.m_per_deg_lat)-
                      ((x1-aX)*aDistanceLatLon.m_per_deg_lon*(y2-y1)*aDistanceLatLon.m_per_deg_lat))
                 /sqrt(sqr((x2-x1)*aDistanceLatLon.m_per_deg_lon)+sqr((y2-y1)*aDistanceLatLon.m_per_deg_lat));
            }
            d := pDistance(aDistanceLatLon, aX, aY, x1, y1, x2, y2);
            if Result>d
            then Result := d;
          end;
        end
        else
        begin
          d := aDistanceLatLon.distanceInMeters(Abs(part.points[0].y-aY), Abs(part.points[0].x-aX));
          if Result>d
          then Result := d;
        end;
      end;
    end;
  end;
end;

function TGeometryLayerObject.Encode: TByteBuffer;
begin
  Result :=
    TByteBuffer.bb_tag_Double(icehTilerValue, value)+
    TByteBuffer.bb_tag_rawbytestring(icehTilerGeometry, fGeometry.Encode)+
    inherited Encode;
end;

function TGeometryLayerObject.getExtent: TWDExtent;
begin
  Result := TWDExtent.FromGeometry(fGeometry);
end;

function TGeometryLayerObject.getGeoJSON2D(const aType: string): string;
var
  colors: TGeoColors;
  mainColorOpacity: Integer;
  jsonOpacity: string;
begin
  if Assigned(fGeometry) then
  begin
    if Assigned(fLayer.tilerLayer) and Assigned(fLayer.tilerLayer.palette) then
    begin
      colors := fLayer.tilerLayer.palette.ValueToColors(fValue);
      mainColorOpacity := colors.mainColor shr 24;
      if mainColorOpacity<$FF
      then jsonOpacity := ',"fillOpacity":'+Double(mainColorOpacity/$FF).ToString(dotFormat)
      else jsonOpacity := '';
    end
    else colors := TGeoColors.Create($FF000000); // black
    Result := '{ '+
      '"type":"Feature",'+
      '"geometry":{'+
        '"type":"'+fLayer.GeometryType+'",'+
        '"coordinates":'+fGeometry.GeoJSON2D[aType]+
      '},'+
      '"properties":{'+
        '"id":"'+string(ID)+'",'+
        '"color": "'+ColorToJSON(colors.mainColor)+'"'+
        jsonOpacity+
      '}'+
    '}'
  end
  else Result := '';
end;

function TGeometryLayerObject.getValidGeometry: Boolean;
begin
  Result := Assigned(fGeometry);
end;

function TGeometryLayerObject.intersects(aGeometry: TWDGeometry): Boolean;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  for part in fGeometry.parts do
  begin
    for point in part.points do
    begin
      if aGeometry.PointInAnyPart(point.x, point.y)
      then Exit(True);
    end;
  end;
  Exit(False);
end;

{ TLayer }

procedure TLayer.addDiffLayer(aDiffLayer: TDiffLayer);
var
  i: Integer;
  client: TClient;
begin
  TMonitor.Enter(fDependentDiffLayers);
  try
    i := fDependentDiffLayers.IndexOf(aDiffLayer);
    if i<0 then
    begin
      fDependentDiffLayers.Add(aDiffLayer);
      // subscribe all clients to this diff layer also
      TMonitor.Enter(fClients);
      try
        for client in fClients
        do aDiffLayer.HandleClientSubscribe(client);
      finally
        TMonitor.Exit(fClients);
      end;
    end;
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

procedure TLayer.AddObject(aObject: TLayerObject);
begin
  fObjectsLock.BeginWrite;
  try
    fObjects.Add(aObject.id, aObject);
  finally
    fObjectsLock.EndWrite;
  end;
end;

procedure TLayer.AddOrSetObject(aObject: TLayerObject);
begin
  fObjectsLock.BeginWrite;
  try
    fObjects.AddOrSetValue(aObject.id, aObject);
  finally
    fObjectsLock.EndWrite;
  end;
end;

procedure TLayer.ClearObjects;
begin
  fObjectsLock.BeginWrite;
  try
    fObjects.Clear;
  finally
    fObjectsLock.EndWrite;
  end;
end;

constructor TLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aDiffRange: Double; aBasicLayer: Boolean=False);
//var
//  previewFileName: string;
//  previewsFolder: string;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  //fPalette := aPalette;
  fObjectTypes := aObjectTypes;
  fGeometryType := aGeometryType;
  fDiffRange := aDiffRange;
  fBasicLayer := aBasicLayer;
  fObjects := TObjectDictionary<TWDID, TLayerObject>.Create([doOwnsValues]);
  fObjectsLock.Create;
  fDependentDiffLayers := TObjectList<TDiffLayer>.Create(False);
  fLegendJSON := '';
  fQuery := '';
  //fObjectsTilesID := -1;
  //fObjectsTilesLink := '';
  //fSlicetype := -1;
  fTilerLayer := nil;
//  fTilerEvent := aTilerEvent;
//  fTilerEvent.OnEvent.Add(HandleTilerEvent);
//  fTilerEvent.subscribe;
  // check fro preview image
  {
  previewsFolder := ExtractFilePath(ParamStr(0))+'previews';
  previewFileName := previewsFolder+'\'+elementID+'.png';
  try
    if FileExists(previewFileName) then
    begin
      fPreview := TPngImage.Create;
      fPreview.LoadFromFile(previewFileName);
    end
    else fPreview := nil;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception creating preview image for '+elementID+': '+e.Message, llError);
    end;
  end;
  }
  fPreviewRequestTimer := scenario.project.Timers.SetTimer(
    procedure (aTimer: TTImer)
    begin
      Log.WriteLn('triggered preview timer for '+elementID);
      fTilerLayer.signalRequestPreview;
    end);
  fSendRefreshTimer := scenario.project.Timers.CreateInactiveTimer;// SetTimer(handleLayerRefresh);
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*30);
//  fOutputEvent.OnEvent.Add(handleOutputEvent);
//  fOutputEvent.Subscribe;
end;

destructor TLayer.Destroy;
begin
//  if Assigned(fTilerEvent) then
//  begin
//    fTilerEvent.OnEvent.Remove(handleTilerEvent);
//    fTilerEvent := nil;
//  end;
//  if Assigned(fOutputEvent) then
//  begin
//    fOutputEvent.OnEvent.Remove(handleOutputEvent);
//    fOutputEvent := nil;
//  end;
  FreeAndNil(fTilerLayer);
  inherited;
  FreeAndNil(fObjects);
  FreeAndNil(fDependentDiffLayers);
  //FreeAndNil(fPalette);
  //FreeAndNil(fPreview);
end;

function TLayer.ExtractObject(aObject: TLayerObject): TLayerObject;
var
  p: TPair<TWDID, TLayerObject>;
begin
  fObjectsLock.BeginWrite;
  try
    p := fObjects.ExtractPair(aObject.id);
    Result := p.Value;
  finally
    fObjectsLock.EndWrite;
  end;
end;

function TLayer.findNearestObject(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double; var aDistance: Double): TLayerObject;
// initialize aDistance to Infinity to get nearest object
var
  o: TPair<TWDID, TLayerObject>;
  d: double;
begin
  Result := nil;
  for o in  fObjects do
  begin
    d := o.value.distance(aDistanceLatLon, aX, aY);
    if d=0 then
    begin
      aDistance := d;
      Result := o.Value;
      break;
    end
    else
    begin
      if aDistance>d then
      begin
        aDistance := d;
        Result := o.Value;
      end;
    end;
  end;
end;

function TLayer.FindObject(const aID: TWDID; out aObject: TLayerObject): Boolean;
begin
  fObjectsLock.BeginRead;
  try
    Result := fObjects.TryGetValue(aID, aObject);
  finally
    fObjectsLock.EndRead;
  end;
end;

function TLayer.findObjectsInCircle(const aDistanceLatLon: TDistanceLatLon; aX, aY, aRadius: Double; var aObjectsJSON: string): Integer;
var
  o: TPair<TWDID, TLayerObject>;
  d: double;
begin
  Result := 0;
  for o in fObjects do
  begin
    d := o.Value.distance(aDistanceLatLon, aX, aY);
    if d<=aRadius then
    begin
      if aObjectsJSON<>''
      then aObjectsJSON := aObjectsJSON+',';
      aObjectsJSON := aObjectsJSON+o.Value.GeoJSON2D[Self.fGeometryType];
      Result := Result+1;
    end;
  end;
end;

function TLayer.findObjectsInGeometry(const aGeometryExtent: TWDExtent; aGeometry: TWDGeometry; var aObjectsJSON: string): Integer;
var
  o: TPair<TWDID, TLayerObject>;
begin
  Result := 0;
  for o in fObjects do
  begin
    if aGeometryExtent.Intersects(o.Value.Extent) then
    begin
      if o.Value.intersects(aGeometry) then
      begin
        if aObjectsJSON<>''
        then aObjectsJSON := aObjectsJSON+',';
        aObjectsJSON := aObjectsJSON+o.Value.GeoJSON2D[Self.fGeometryType];
        Result := Result+1;
      end;
    end;
  end;
end;

procedure TLayer.ReadObjectsDBSVGPaths(aQuery: TDataSet; aDefaultValue: Double);
var
  _id: TWDID;
  _path: string;
  _value: Double;
  obj: TGeometryLayerObject;
begin
  aQuery.Open;
  fObjectsLock.BeginWrite;
  try
    fObjects.Clear;
    while not aQuery.Eof do
    begin
      try
        _id := aQuery.Fields[0].AsAnsiString;
        if not aQuery.Fields[1].IsNull then
        begin
          _path := aQuery.Fields[1].Value;
          if (aQuery.Fields.Count>2) and not aQuery.Fields[2].IsNull
          then _value := aQuery.Fields[2].AsFloat
          else _value := aDefaultValue;
          obj := TGeometryLayerObject.Create(Self, _id, TWDGeometry.CreateFromSVGPath(_path), _value);
          fObjects.Add(obj.id, obj);
        end;
      except
        on e: Exception
        do Log.WriteLn(elementID+': exception in ReadObjectsDBSVGPaths: '+e.Message, llError);
      end;
      aQuery.Next;
    end;
  finally
    fObjectsLock.EndWrite;
  end;
end;

procedure TLayer.RegisterLayer;
begin
  FreeAndNil(fTilerLayer); // default to not-registering (not enough information)
end;

procedure TLayer.RegisterOnTiler(aPersistent: Boolean; aSliceType: Integer; const aDescription: string; aEdgeLengthInMeters: Double; aPalette: TWDPalette);
//var
//  payload: TByteBuffer;
begin
  {
  fSliceType := aSliceType;
  payload :=
    TByteBuffer.bb_tag_string(icehTilerEventName, fOutputEvent.eventName);
  if not IsNaN(aEdgeLengthInMeters)
  then payload := Payload+
    TByteBuffer.bb_tag_double(icehTilerEdgeLength, aEdgeLengthInMeters);
  payload := Payload+
    TByteBuffer.bb_tag_string(icehTilerLayerDescription, aDescription)+
    TByteBuffer.bb_tag_bool(icehTilerPersistent, aPersistent)+
    TByteBuffer.bb_tag_int32(icehTilerRequestNewLayer, aSliceType); // last to trigger new layer request
  fTilerEvent.signalEvent(payload);
  }
  fTilerLayer.Free;
  // recreate tiler layer definition
  fTilerLayer := TTilerLayer.Create(scenario.project.Connection, elementID, aSliceType, aPalette);//, -1, '' .addLayer(elementID, aSliceType, aPalette);
  // add handlers
  fTilerLayer.onTilerInfo := handleTilerInfo;
  fTilerLayer.onRefresh := handleTilerRefresh;
  fTilerLayer.onPreview := handleTilerPreview;
  // trigger registration
  fTilerLayer.signalRegisterLayer(scenario.project.tiler, aDescription, aPersistent, aEdgeLengthInMeters);
end;

procedure TLayer.RegisterSlice;
begin
  // default no action
end;

procedure TLayer.removeDiffLayer(aDiffLayer: TDiffLayer);
var
  i: Integer;
  client: TClient;
begin
  TMonitor.Enter(fDependentDiffLayers);
  try
    i := fDependentDiffLayers.IndexOf(aDiffLayer);
    if i>=0 then
    begin
      fDependentDiffLayers.Delete(i);
      // unsubscribe all clients from this diff layer also
      TMonitor.Enter(fClients);
      try
        for client in fClients
        do aDiffLayer.HandleClientUnsubscribe(client);
      finally
        TMonitor.Exit(fClients);
      end;
    end;
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

procedure TLayer.RemoveObject(aObject: TLayerObject);
begin
  fObjectsLock.BeginWrite;
  try
    fObjects.Remove(aObject.id);
  finally
    fObjectsLock.EndWrite;
  end;
end;

const
  MaxBufferLength = 2048;

procedure TLayer.signalObjects(aSender: TObject);
var
  timeStamp: TDateTime;
  iop: TPair<TWDID, TLayerObject>;
  buffer: TByteBuffer;

  procedure Commit;
  begin
    if buffer<>'' then
    begin
      fTilerLayer.signalData(buffer, timeStamp);
      buffer := '';
    end;
  end;

begin
  if Assigned(fTilerLayer) then
  begin
    // todo: replace with batch buffer
    timeStamp := 0; // todo:
    //fTilerLayer.signalPalette(timeStamp);
    // signal objects (which will trigger creation of layer if not already done above)
    buffer := '';
    for iop in fObjects do
    begin
      buffer := buffer+iop.Value.encode;
      if length(buffer)> MaxBufferLength
      then Commit;
    end;
    Commit;
  end;
end;

function TLayer.SliceType: Integer;
begin
  Result := stGeometry; // default
end;

function TLayer.uniqueObjectsTilesLink: string;
begin
  if Assigned(fTilerLayer)
  then Result := fTilerLayer.URLTimeStamped
  else Result := '';
end;

function TLayer.getJSON: string;
begin
  Result := inherited getJSON+','+
    '"basic":'+Ord(basicLayer).ToString+','+
    '"objectTypes":['+objectTypes+'],'+
    '"legend":{'+legendJSON+'},'+
    '"preview":"'+previewBASE64+'",'+
    '"tiles":"'+uniqueObjectsTilesLink+'"';
  if (objects.Count<=MaxDirectSendObjectCount) and (fGeometryType<>'Point')
  then Result := Result+',"objects": '+objectsJSON;
end;

function TLayer.getObjectsJSON: string;
var
  iop: TPair<TWDID, TLayerObject>;
begin
  Result := '';
  for iop in objects do
  begin
    if iop.Value.ValidGeometry
    then jsonAdd(Result, iop.Value.GeoJSON2D[fGeometryType]);
  end;
  Result := geoJsonFeatureCollection(Result);
end;

function TLayer.getPreviewBASE64: string;
begin
  if Assigned(fTilerLayer)
  then Result := fTilerLayer.previewAsBASE64
  else Result := '';
end;

function TLayer.getRefJSON: string;
begin
  Result := '"id":"'+getElementID+'","tiles":"'+uniqueObjectsTilesLink+'"';
  if (objects.Count<=MaxDirectSendObjectCount) and (fGeometryType<>'Point')
  then Result := Result+',"objects": '+objectsJSON;
end;
function TLayer.HandleClientSubscribe(aClient: TClient): Boolean;
var
  diffLayer: TDiffLayer;
begin
  Result := inherited;
  // handle subscribe on dependent diff layers
  TMonitor.Enter(fDependentDiffLayers);
  try
    for diffLayer in fDependentDiffLayers
    do diffLayer.HandleClientSubscribe(aClient);
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

function TLayer.HandleClientUnsubscribe(aClient: TClient): Boolean;
var
  diffLayer: TDiffLayer;
begin
  Result := inherited;
  // todo: handle unsubscribe on dependent diff layers
  TMonitor.Enter(fDependentDiffLayers);
  try
    for diffLayer in fDependentDiffLayers
    do diffLayer.HandleClientUnsubscribe(aClient);
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

{
procedure TLayer.handleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
//  tilerStartup: TDateTime;
begin
  try
    // todo:
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehTilerStartup shl 3) or wt64Bit:
          begin
            aBuffer.bb_read_double(aCursor);
            // re-register layer
            RegisterLayer;
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  except
    on e: Exception
    do log.WriteLn('exception in TLayer.handleTilerEvent: '+e.Message, llError);
  end;
end;
}
procedure TLayer.handleTilerInfo(aTilerLayer: TTilerLayer);
var
  dl: TDiffLayer;
begin
  RegisterSlice;
  AddCommandToQueue(Self, Self.signalObjects);
  TMonitor.Enter(fDependentDiffLayers);
  try
    for dl in fDependentDiffLayers
    do dl.HandleSubLayerInfo(Self);
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

procedure TLayer.handleTilerPreview(aTilerLayer: TTilerLayer);
var
  pvBASE64: string;
  client: TClient;
begin
  pvBASE64 := previewBASE64;
  // layer clients
  TMonitor.Enter(clients);
  try
    for client in clients
    do client.SendPreview(elementID, pvBASE64);
  finally
    TMonitor.Exit(clients);
  end;
  // scenario clients
  TMonitor.Enter(fScenario.clients);
  try
    for client in fScenario.clients do
    begin
      if not clients.Contains(client)
      then client.SendPreview(elementID, pvBASE64);
    end;
  finally
    TMonitor.Exit(fScenario.clients);
  end;
  Log.WriteLn('send normal preview on '+elementID);
end;

procedure TLayer.handleTilerRefresh(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
begin
  if Assigned(fSendRefreshTimer) then
  begin
    fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*5),
      procedure(aTimer: TTimer)
      var
        timeStampStr: string;
        tiles: string;
        client: TClient;
      begin
        if aTimeStamp<>0
        then timeStampStr := FormatDateTime('yyyy-mm-dd hh:mm', aTimeStamp)
        else timeStampStr := '';
        // signal refresh to layer client
        tiles := uniqueObjectsTilesLink;
        TMonitor.Enter(clients);
        try
          for client in clients
          do client.SendRefresh(elementID, timeStampStr, tiles);
        finally
          TMonitor.Exit(clients);
        end;
        // signal refresh to scenario client
        TMonitor.Enter(fScenario.clients);
        try
          for client in fScenario.clients do
          begin
            if not clients.Contains(client)
            then client.SendRefresh(elementID, timeStampStr, tiles);
          end;
        finally
          TMonitor.Exit(fScenario.clients);
        end;
      end);
  end;
  // refresh preview also
  if Assigned(fPreviewRequestTimer)
  then fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*10);
end;

{ KPI }

constructor TKPI.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
begin
  //
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
end;

destructor TKPI.Destroy;
begin
  inherited;
  //
end;

function TKPI.getJSON: string;
begin
  Result := inherited getJSON+','+
    '"title": "'+title+'", '+ // todo: same as name; remove?
    '"subtitle": "'+subtitle+'", '+
    '"ranges": ['+jsonArrayOfDoubleToStr(ranges)+'], '+
    '"measures": ['+jsonArrayOfDoubleToStr(measures)+'], '+
    '"markers": ['+jsonArrayOfDoubleToStr(markers)+']';
end;

procedure TKPI.Update;
var
  _json: string;
  client: TClient;
begin
  // send update to clients
  _json := '{"updatekpi":{'+JSON+'}}';
  TMonitor.Enter(clients);
  try
    for client in clients
    do client.signalString(_json);
  finally
    TMonitor.Exit(clients);
  end;
  // send also to clients on scenario for udpate of preview
  with fScenario do
  begin
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(_json);
    finally
      TMonitor.Exit(clients);
    end;
  end;
end;

{ TChartGroupRow }

class function TChartGroupRow.Create(const aGroup: string; const aValues: TArray<Double>): TChartGroupRow;
begin
  Result.group := aGroup;
  Result.values := aValues;
end;

{ TChart }

constructor TChart.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; const aChartType: string);
begin
  // todo:
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  fChartType := aChartType;
end;

destructor TChart.Destroy;
begin
  inherited;
  // todo:
end;

function TChart.getJSON: string;
var
  g: Integer;
  r: Integer;
begin
  Result := inherited getJSON;
  if Result<>''
  then Result := Result+',';
  Result := Result+'"chartType":"'+fChartType+'"';
  // todo:
  if Length(GroupNames)>0 then
  begin
    // group names
    if Result<>''
    then Result := Result+',';
    Result := Result+'"groupNames":[';
    for g := 0 to Length(GroupNames)-1 do
    begin
      if g>0
      then Result := Result+',';
      Result := Result+'"'+GroupNames[g]+'"';
    end;
    Result := Result+']';
    // group data
    if Result<>''
    then Result := Result+',';
    Result := Result+'"groupValues":[';
    for r := 0 to Length(GroupValues)-1 do
    begin
      if r>0
      then Result := Result+',';
      Result := Result+'{'+
        '"key":"'+GroupValues[r].group+'",'+
        '"groupValues":[';
      for g := 0 to Length(GroupValues[r].values)-1 do
      begin
        if g>0
      	then Result := Result+',';
        Result := Result+'{"name":"'+GroupNames[g]+'", "value":'+Double.ToString(GroupValues[r].values[g], dotFormat)+'}';
      end;
      Result := Result+']}';
    end;
    Result := Result+']';
  end;
end;

{ TScenario }

function TScenario.AddChart(aChart: TChart): TChart;
begin
  fCharts.Add(aChart.ID, aChart);
  Result := aChart;
end;

function TScenario.AddKPI(aKPI: TKPI): TKPI;
begin
  fKPIs.Add(aKPI.ID, aKPI);
  Result := aKPI;
end;

function TScenario.AddLayer(aLayer: TLayer): TLayer;
begin
  fLayers.Add(aLayer.ID, aLayer);
  Result := aLayer;
end;

constructor TScenario.Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
begin
  inherited Create;
  fProject := aProject;
  fID := aID;
  fName := aName;
  fDescription := aDescription;
  fMapView := aMapView;
  fLayers := TObjectDictionary<string,TLayer>.Create([doOwnsValues]);
  fKPIs := TObjectDictionary<string, TKPI>.Create([doOwnsValues]);
  fCharts := TObjectDictionary<string, TChart>.Create([doOwnsValues]);
  fAddbasicLayers := aAddbasicLayers;
  ReadBasicData;
end;

destructor TScenario.Destroy;
begin
  inherited;
  FreeAndNil(fLayers);
  FreeAndNil(fKPIs);
  FreeAndNil(fCharts);
end;

function TScenario.getElementID: string;
begin
  Result := ID;
  if Assigned(fProject)
  then Result := fProject.fProjectID+sepElementID+Result;
end;

procedure TScenario.ReadBasicData;
begin
  // default no action
end;

procedure TScenario.RegisterLayers;
var
  layer: TLayer;
begin
  for layer in fLayers.Values
  do layer.RegisterLayer;
end;

function TScenario.selectLayersOnCategories(const aSelectedCategories: TArray<string>; aLayers: TList<TLayer>): Boolean;
var
  ilp: TPair<string, TLayer>;
  cat: string;
begin
  Result := False;
  if length(aSelectedCategories)>0  then
  begin
    // select basic layers on categories
    for ilp in fLayers do
    begin
      for cat in aSelectedCategories do
      begin
        if ilp.Key.ToUpper=cat.ToUpper then
        begin
          aLayers.Add(ilp.Value);
          Result := True;
        end;
      end;
    end;
  end
  else
  begin
    // all basic layers
    for ilp in fLayers do
    begin
      if ilp.Value.basicLayer then
      begin
        aLayers.Add(ilp.Value);
        Result := True;
      end;
    end;
  end;
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories, aSelectedIDs: TArray<string>): string;
begin
  Result := '';
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string;
begin
  Result := '';
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string;
begin
  Result := '';
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string;
begin
  Result := '';
end;

function TScenario.selectObjectsProperties(aClient: TClient; const aSelectedCategories, aSelectedObjects: TArray<string>): string;
begin
  Result := '';
end;

{ TUSScenarioLink }

procedure TScenarioLink.buildHierarchy;
// put all direct children in hierarchy
var
  i: Integer;
  child: TScenarioLink;
  _parent: TScenarioLink;
begin
  for i := fChildren.Count-1 downto 0 do
  begin
    child := fChildren[i];
    _parent := root.findScenario(child.parentID);
    if Assigned(_parent) and (_parent<>child) and (child.parent<>_parent) then
    begin
      fChildren.Extract(child);
      _parent.children.Insert(0, child);
      child.parent := _parent;
    end;
  end;
end;

constructor TScenarioLink.Create(aID, aParentID, aReferenceID: Integer; const aName, aDescription, aStatus: string; aLink: TScenario);
begin
  inherited Create;
  fChildren := TObjectList<TScenarioLink>.Create;
  fLink := aLink;
  fParent := nil;
  //
  fID := aID;
  fParentID := aParentID;
  fReferenceID := aReferenceID;
  fName := aName;
  fDescription := aDescription;
  fStatus := aStatus;
end;

destructor TScenarioLink.Destroy;
begin
  FreeAndNil(fChildren);
  fLink := nil;
  inherited;
end;

function TScenarioLink.findScenario(aID: Integer): TScenarioLink;
var
  i: Integer;
begin
  Result := nil;
  i := fChildren.Count-1;
  while (i>=0) and not Assigned(Result) do
  begin
    if fChildren[i].ID<>aID then
    begin
      Result := fChildren[i].findScenario(aID);
      i := i-1;
    end
    else Result := fChildren[i];
  end;
end;

function TScenarioLink.JSON: string;
var
  child: TScenarioLink;
begin
  Result := '';
  for child in fChildren do
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+
      '{"id":'+child.ID.toString+','+
       '"name":"'+child.name+'",'+
       '"description":"'+child.description+'",'+
       '"status":"'+child.status+'",'+
       '"reference":'+child.referenceID.toString+','+
       '"children":'+child.JSON+'}';
  end;
  Result := '['+Result+']';
end;

function TScenarioLink.name: string;
begin
  if Assigned(Self)
  then Result := fName
  else Result := '';
end;

function TScenarioLink.removeLeave(const aStatus: string): Boolean;
var
  i: Integer;
begin
  for i := fChildren.Count-1 downto 0 do
  begin
    if fChildren[i].removeLeave(aStatus)
    then fChildren.Delete(i);
  end;
  Result := (fChildren.Count=0) and (fStatus=aStatus);
end;

function TScenarioLink.root: TScenarioLink;
begin
  Result := Self;
  while Assigned(Result.fParent)
  do Result := Result.fParent;
end;

function compareScenarioLinks(const aScenarioLink1, aScenarioLink2: TScenarioLink): Integer;
begin
  Result := AnsiCompareText(aScenarioLink1.description, aScenarioLink2.description);
end;

procedure TScenarioLink.sortChildren();
begin
  children.Sort(TComparer<TScenarioLink>.Construct(compareScenarioLinks));
end;

{ TMeasure }

constructor TMeasure.Create(const aID, aCategory, aTypology, aApplication, aDescription, aObjectType, aBenefits: string;
  aAction: Integer; aActionParameter: Double);
begin
  inherited Create;
  fID := aID;
  fCategory := aCategory;
  fTypology := aTypology;
  fApplication := aApplication;
  fDescription := aDescription;
  fObjectType := aObjectType;
  fBenefits := aBenefits;
  fAction := aAction;
  fActionParameter := aActionParameter;
end;

destructor TMeasure.Destroy;
begin
  //
  inherited;
end;

{ TProject }

function TProject.AddClient(const aClientID: string): TClient;
begin
  Result := TClient.Create(Self, fCurrentScenario, fRefScenario, aClientID);
  TMonitor.Enter(fClients);
  try
    fClients.Add(Result);
  finally
    TMonitor.Exit(fClients);
  end;
end;

constructor TProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
  aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
  aMaxNearestObjectDistanceInMeters: Integer);
begin
  inherited  Create;
  fTimeSlider := aTimeSlider;
  fSelectionEnabled := aSelectionEnabled;
  fMeasuresEnabled := aMeasuresEnabled;
  fMeasuresHistoryEnabled := aMeasuresHistoryEnabled;
  fSimualtionControlEnabled := aSimualtionControlEnabled;
  fAddBasicLayers := aAddbasicLayers;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fClients := TObjectList<TClient>.Create;
  fScenarios := TObjectDictionary<string, TScenario>.Create([doOwnsValues]);
  fScenarioLinks := TScenarioLink.Create(-1, -1, -1, '', '', '', nil);
  fTimers := TTimerPool.Create;
  fDiffLayers := TObjectDictionary<string, TDiffLayer>.Create([doOwnsValues]);
  fProjectID := aProjectID;
  fProjectName := aProjectName;
  fProjectDescription := ''; // default no description set, set by property..
  fMaxNearestObjectDistanceInMeters := aMaxNearestObjectDistanceInMeters;
  fTiler := TTiler.Create(aConnection, aTilerFQDN, aTilerStatusURL);
  fTiler.onTilerStartup := handleTilerStartup;
//  fTilerEvent := aConnection.publish(aTilerEventName, False);
  fDBConnection := aDBConnection;
  fCurrentScenario := nil;
  fRefScenario := nil;
  fMeasures := TObjectDictionary<string, TMeasure>.Create([doOwnsValues]);
  // projection
  {
  if aSourceEPSG>0
  then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(aSourceEPSG)
  else fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  }
  // imb init
  fProjectEvent := fConnection.subscribe(WS2IMBEventName+'.'+fProjectID, False);
  // read basic data
  ReadBasicData();
  // add handler for new sessions
  fProjectEvent.OnIntString.Add(
    procedure(event: TEventEntry; aInt: Integer; const aString: string)
    begin
      if aInt=actionNew then
      begin
        Log.WriteLn(aProjectId+': link to '+aString);
        AddClient(aString);
      end;
    end);

  // todo: start timer for tiler status as a heartbeat
  if aTilerStatusURL<>''
  then timers.SetTimer(timerTilerStatusAsHeartbeat, hrtNow+DateTimeDelta2HRT(dtOneHour), DateTimeDelta2HRT(dtOneHour))
end;

destructor TProject.Destroy;
begin
  FreeAndNil(fDiffLayers);
  FreeAndNil(fTimers);
  FreeAndNil(fClients);
  FreeAndNil(fScenarios);
  FreeAndNil(fMeasures);
  inherited;
//  if Assigned(fTilerEvent) then
//  begin
//    fTilerEvent.unPublish;
//    fTilerEvent := nil;
//  end;
  FreeAndNil(fDBConnection);
end;


function TProject.diffElementID(aCurrent, aReference: TScenarioElement): string;
begin
  if aCurrent.scenario.project.projectID=aReference.scenario.project.projectID then
  begin
    if aCurrent.scenario.ID=aReference.scenario.ID
    then Result := aCurrent.scenario.project.ProjectID+sepElementID+aCurrent.scenario.ID+sepElementID+aCurrent.ID+'-'+aReference.ID+'-diff'
    else
    begin
      if aCurrent.ID=aReference.ID
      then Result := aCurrent.scenario.project.ProjectID+sepElementID+aCurrent.scenario.ID+'-'+aReference.scenario.ID+sepElementID+aCurrent.ID+'-diff'
      else Result := aCurrent.scenario.project.ProjectID+sepElementID+aCurrent.scenario.ID+sepElementID+aCurrent.ID+'-'+aReference.scenario.ID+sepElementID+aReference.ID+'-diff';
    end;
  end
  else Result := aCurrent.elementID+'-'+aReference.elementID+'-diff';
end;

function TProject.getMeasuresHistoryJSON: string;
begin
  Result := ''; // default no measures history
end;

function TProject.getMeasuresJSON: string;
begin
  if fMeasures.Count>0 then
  begin
    // todo:
    Result := '[]';
  end
  else Result := '[]';
end;

procedure TProject.handleClientMessage(aJSONObject: TJSONObject);
begin
  // default no action
end;

procedure TProject.handleTilerStartup(aTiler: TTiler; aStartupTime: TDateTime);
var
  scenario: TScenario;
begin
  for scenario in fScenarios.Values
  do scenario.RegisterLayers;
end;

procedure TProject.newClient(aClient: TClient);
begin
  // default no actions
end;

function TProject.ReadScenario(const aID: string): TScenario;
begin
  Result := nil;
end;

procedure TProject.SendPreview;
var
  client: TClient;
  se: TClientSubscribable;
  preview: string;
begin
  Log.WriteLn('Sending preview to clients connected to project '+Self.ProjectName);
  TMonitor.Enter(clients);
  try
    for client in clients do
    begin
      TMonitor.Enter(client.subscribedElements);
      try
        for se in client.subscribedElements do
         begin
           if se is TLayer then
           begin
             preview := (se as TLayer).previewBASE64;
             if preview<>'' then
             begin
               client.SendPreview(se.elementID, preview);
               Log.WriteLn('Send preview for '+se.elementID);
             end
             else Log.WriteLn('NO preview to send for '+se.elementID);
           end;
         end;
      finally
        TMonitor.Exit(client.subscribedElements);
      end;
    end;
  finally
    TMonitor.Exit(clients);
  end;
end;

procedure TProject.SendRefresh;
var
  client: TClient;
  se: TClientSubscribable;
  tiles: string;
begin
  Log.WriteLn('Sending refresh to clients connected to project '+Self.ProjectName);
  TMonitor.Enter(clients);
  try
    for client in clients do
    begin
      TMonitor.Enter(client.subscribedElements);
      try
        for se in client.subscribedElements do
        begin
          if se is TLayer then
          begin
            tiles := (se as TLayer).uniqueObjectsTilesLink;
            client.SendRefresh(se.elementID, '', tiles);
            Log.WriteLn('Send refresh for '+se.elementID+': '+tiles);
          end;
        end;
      finally
        TMonitor.Exit(client.subscribedElements);
      end;
    end;
  finally
    TMonitor.Exit(clients);
  end;
end;

procedure TProject.SendString(const aString: string);
var
  client: TClient;
begin
  //Log.WriteLn('Sending string to clients connected to project '+Self.ProjectName);
  TMonitor.Enter(clients);
  try
    for client in clients
    do client.signalString(aString);
  finally
    TMonitor.Exit(clients);
  end;
end;

procedure TProject.setMapView(const aValue: TMapView);
begin
  fMapView := aValue;
  // todo: signal to connected clients? used to set scenarios so maybe not needed hereend;
end;

procedure TProject.setMeasuresEnabled(aValue: Boolean);
var
  client: TClient;
begin
  if fMeasuresEnabled<>aValue then
  begin
    fMeasuresEnabled := aValue;
    // update session info for all connected clients
    for client in clients
    do client.SendMeasuresEnabled;
  end;
end;

procedure TProject.setMeasuresHistoryEnabled(aValue: Boolean);
var
  client: TClient;
begin
  if fMeasuresHistoryEnabled<>aValue then
  begin
    fMeasuresHistoryEnabled := aValue;
    // update session info for all connected clients
    for client in clients
    do client.SendMeasuresHistoryEnabled;
  end;
end;

procedure TProject.setProjectDescription(const aValue: string);
begin
  fProjectDescription := aValue;
  // todo: (re-)signal description to connected clients
end;

procedure TProject.setProjectName(const aValue: string);
begin
  fProjectName := aValue;
  // todo: (re-)signal name to connected clients

end;

procedure TProject.setSelectionEnabled(aValue: Boolean);
var
  client: TClient;
begin
  if fSelectionEnabled<>aValue then
  begin
    fSelectionEnabled := aValue;
    // update session info for all connected clients
    for client in clients
    do client.SendSelectionEnabled;
  end;
end;

procedure TProject.setTimeSlider(aValue: Integer);
var
  client: TClient;
begin
  if fTimeSlider<>aValue then
  begin
    fTimeSlider := aValue;
    // update session info for all connected clients
    for client in clients
    do client.SendTimeSlider;
  end;
end;

procedure TProject.timerTilerStatusAsHeartbeat(aTimer: TTimer);
begin
  // request status from tiler as a heartbeat (keep IIS isapi module a live)
  fTiler.getTilerStatus;
end;

{ TModel }

constructor TModel.Create(aConnection: TConnection);
begin
  inherited Create;
  fConnection := aConnection;
end;

{ TSessionModel }

constructor TSessionModel.Create(aConnection: TConnection);
begin
  inherited Create(aConnection);
  fProjects := TObjectList<TProject>.Create;
end;

destructor TSessionModel.Destroy;
begin
  FreeAndNil(fProjects); // todo: pointer exception some where during free of this list
  inherited;
end;

function TSessionModel.FindElement(const aElementID: string): TScenarioElement;
var
  s: TArray<System.string>;
  project: TProject;
  scenario: TScenario;
  layer: TLayer;
  kpi: TKPI;
  chart: TChart;
begin
  s := aElementID.Split([sepElementID]);
  if Length(s)>=3 then
  begin
    for project in fProjects do
    begin
      if project.ProjectID.CompareTo(s[Length(s)-3])=0 then
      begin
        if project.fScenarios.TryGetValue(s[Length(s)-2], scenario) then
        begin
          if scenario.KPIs.TryGetValue(s[Length(s)-1], kpi)
          then Exit(kpi)
          else if scenario.Layers.TryGetValue(s[Length(s)-1], layer)
          then Exit(layer)
          else if scenario.Charts.TryGetValue(s[Length(s)-1], chart)
          then Exit(chart)
          else Exit(nil);
        end
        else Exit(nil);
      end;
    end;
  end;
  Exit(nil);
end;

{ TMapView }

class function TMapView.Create(aLat, aLon: Double; aZoom: Integer): TMapView;
begin
  Result.fLat := aLat;
  Result.fLon := aLon;
  Result.fZoom := aZoom;
end;

procedure TMapView.DumpToLog;
begin
  Log.WriteLn('MapView: lat:'+lat.ToString+' lon:'+lon.ToString+' zoom:'+zoom.ToString);
end;

end.
