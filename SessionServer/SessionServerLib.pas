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

  // client layer types
  ltTile  = 'tile';
  ltObject = 'object';
  ltGeo = 'geo';
  ltSwitch = 'switch';
  ltEmpty  = '';

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

  TGroup = TObjectList<TClient>;

  TForEachClient = reference to procedure(aClient: TCLient);

  TClientSubscribable = class
  constructor Create;
  destructor Destroy; override;
  private
    fClients: TGroup; // refs, lock with TMonitor
  protected
    function getElementID: string; virtual; abstract;
  public
    property clients: TGroup read fClients; // refs, lock with TMonitor
    property elementID: string read getElementID;
  public
    function HandleClientSubscribe(aClient: TClient): Boolean; virtual;
    function HandleClientUnsubscribe(aClient: TClient): Boolean; virtual;
    procedure HandleFirstSubscriber(aClient: TClient); virtual;
    procedure HandleLastSubscriber(aClient: TClient); virtual;
    procedure forEachClient(aForEachClient: TForEachClient);
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

    procedure handleRefreshTrigger(aTimeStamp: TDateTime);
  public
    property tilerLayer: TTilerLayer read fTilerLayer;
    property refJSON: string read getRefJSON;
    property legendJSON: string read fLegendJSON;

    procedure handleSubLayerInfo(aLayer: TLayer);
    procedure registerOnTiler();
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
    procedure setCurrentScenario(const aValue: TScenario);
    procedure setRefScenario(const aValue: TScenario);
  protected
    procedure SendErrorMessage(const aMessage: string);
    procedure SendMeasures();
    procedure SendSession();
    procedure SendMeasuresHistory();
    procedure sendQueryDialogData();
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
    property currentScenario: TScenario read fCurrentScenario write setCurrentScenario;
    property refScenario: TScenario read fRefScenario write setRefScenario;

    procedure UpdateSession(); //  rename (moved from protected to public..

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
    property defaultLoad: Boolean read fDefaultLoad;
    property JSON: string read getJSON;
    property elementID: string read getElementID;
    // writable
    property description: string read fDescription write fDescription;
  end;

  TLayerObject = class
  constructor Create(aLayer: TLayer; const aID: TWDID);
  private
    fLayer: TLayer;
    fID: TWDID;
  protected
    function getJSON2D(const aType, aExtraJSON2DAttributes: string): string; virtual; abstract;
    function getValidGeometry: Boolean; virtual;
    function getExtent: TWDExtent; virtual;
  public
    function Encode: TByteBuffer; virtual;
    function EncodeRemove: TByteBuffer;
  public
    property layer: TLayer read fLayer;
    property ID: TWDID read fID;
    property JSON2D[const aType, aExtraJSON2DAttributes: string]: string read getJSON2D;
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
    function getJSON2D(const aType, aExtraJSON2DAttributes: string): string; override;
    function getValidGeometry: Boolean; override;
    function getExtent: TWDExtent; override;
  public
    function Encode: TByteBuffer; override;
  public
    property geometryPoint: TWDGeometryPoint read fGeometryPoint;
    property value: Double read fValue write fValue;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TGeometryLayerPOIObject = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aPOI: Integer; aGeometryPoint: TWDGeometryPoint);
  destructor Destroy; override;
  protected
    fPOI: Integer;
    fGeometryPoint: TWDGeometryPoint; // owns
    function getJSON2D(const aType, aExtraJSON2DAttributes: string): string; override;
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

  // circle, rectangle, polygon, path
  TSVGPathLayerObject  = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID;
    aGeometry: TWDGeometry;
    aColor: TAlphaRGBPixel; aWeight, aOpacity: single; // strike
    aFillColor: TAlphaRGBPixel; aFillOpacity: single); //
  protected
    fGeometry: TWDGeometry;
    fColor: TAlphaRGBPixel;
    fWeight: single;
    fOpacity: single;
    fFillColor: TAlphaRGBPixel;
    fFillOpacity: single;
    // stroke
    //   color: TAlphaColor
    //   weight: single
    //   opacity: single 0.0..1.0
    //   dash: string
    // fill
    //   fillColor: TAlphaColor
    //   fillOpacity: single 0.0..1.0
    //   opt: fillColor2: TAlphaColor
    //   opt: fillAngle: single
  protected
    function getJSON2D(const aType, aExtraJSON2DAttributes: string): string; override;
    function getValidGeometry: Boolean; override;
    function getExtent: TWDExtent; override;
  public
    //function Encode: TByteBuffer; virtual;

    property geometry: TWDGeometry read fGeometry;
    property color: TAlphaRGBPixel read fColor;
    property weight: single read fWeight;
    property opacity: single read fOpacity;
    property fillColor: TAlphaRGBPixel read fFillColor;
    property fillOpacity: single read fFillOpacity;
  public
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TSVGCircleLayerObject  = class(TSVGPathLayerObject)
  protected
    fRadius: single;
  end;

  TGeometryLayerObject = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue: Double);
  destructor Destroy; override;
  protected
    fGeometry: TWDGeometry; // owns
    fValue: Double; // value to lookup color within palette of layer
  protected
    function getJSON2D(const aType, aExtraJSON2DAttributes: string): string; override;
    function getValidGeometry: Boolean; override;
    function getExtent: TWDExtent; override;
  public
    function Encode: TByteBuffer; override;
  public
    property geometry: TWDGeometry read fGeometry;
    property value: Double read fValue write fValue;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TLayerUpdateObject = class
  constructor Create(const aID: TWDID);
  destructor Destroy; override;
  private
    fID: TWDID;
    fAttributes: TDictionary<string, string>;
  public
    property ID: TWDID read fID;
    property attributes: TDictionary<string, string> read fAttributes;
  end;

  TAttrNameValue = record
  class function Create(const aName, aValue: string): TAttrNameValue; static;
  public
    name: string;
    value: string;
  end;

  TLayer = class(TScenarioElement)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
    const aObjectTypes, aGeometryType, aLayerType: string; aShowInDomains: Boolean; aDiffRange: Double; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fObjects: TObjectDictionary<TWDID, TLayerObject>; // owns
    fGeometryType: string;
    fObjectsLock: TOmniMREW;
    fBasicLayer: Boolean;
    fDependentDiffLayers: TObjectList<TDiffLayer>; // refs
    fDiffRange: Double;
    fObjectTypes: string;
    fLayerType: string;
    fShowInDomains: Boolean; // for hiding layer in domains request (see switch layer)
    fPreviewRequestTimer: TTimer;
    fSendRefreshTimer: TTImer;
    fLayerUpdateTimer: TTimer;

    fObjectsAdded: TObjectDictionary<TWDID, TLayerUpdateObject>; // owns
    fObjectsUpdated: TObjectDictionary<TWDID, TLayerUpdateObject>; // owns
    fObjectsDeleted: TObjectList<TLayerObject>; // owns

    fExtraJSON2DAttributes: string;
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
    procedure handleRefreshTrigger(aTimeStamp: TDateTime);
    procedure handleLayerUpdateTrigger(aTimer: TTimer);
  public
    property objects: TObjectDictionary<TWDID, TLayerObject> read fObjects;
    property geometryType: string read fGeometryType;
    property objectsLock: TOmniMREW read fObjectsLock;
    property basicLayer: Boolean read fBasicLayer;
    property layerType: string read fLayerType;
    property showInDomains: Boolean read fShowInDomains;

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
    property extraJSON2DAttributes: string read fExtraJSON2DAttributes write fExtraJSON2DAttributes;
  public
    function FindObject(const aID: TWDID; out aObject: TLayerObject): Boolean;
    procedure AddObject(aObject: TLayerObject);
    procedure AddObjectAttribute(const aID: TWDID; const aAttributes: TArray<TAttrNameValue>);
    procedure UpdateObjectAttribute(const aID: TWDID; const aAttribute, aValue: string);
    procedure RemoveObject(aObject: TLayerObject);
    function ExtractObject(aObject: TLayerObject): TLayerObject;
    procedure ClearObjects;
    function findNearestObject(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double; var aDistance: Double): TLayerObject;
    function findObjectsInCircle(const aDistanceLatLon: TDistanceLatLon; aX, aY, aRadius: Double; var aObjectsJSON: string): Integer;
    function findObjectsInGeometry(const aGeometryExtent: TWDExtent; aGeometry: TWDGeometry; var aObjectsJSON: string): Integer;

  public
    procedure signalObject(aObject: TLayerObject); virtual;
    procedure signalNoObject(aObject: TLayerObject); virtual;
    procedure signalObjects(aSender: TObject; aObjects: TObjectDictionary<TWDID, TLayerObject>); overload; virtual;
    procedure signalObjects(aSender: TObject); overload; virtual;
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

  TLayerOnZoom = record
  class function Create(aZoomLevel: Integer; aLayer: TLayer): TLayerOnZoom; static;
  public
    zoomLevel: Integer;
    layer: TLayer; // ref
  end;

  TLayerSwitch = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
    const aObjectTypes: string; aBasicLayer: Boolean);
  destructor Destroy; override;
  private
    // most zoomed out is lowest zoom level is first in list -> use that for preview
    fZoomLayers: TList<TLayerOnZoom>;
  protected
    function getJSON: string; override;
  public
    property zoomLayers: TList<TLayerOnZoom> read fZoomLayers; // refs to layers
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

  //TChartGroupNames = TArray<string>;
  (*
  TChartGroupRow = record
  class function Create(const aGroup: string; const aValues: TArray<Double>): TChartGroupRow; static;
  public
    group: string;
    values: TArray<Double>;
  end;

  TChartGroupValues = TArray<TChartGroupRow>;
  *)

  TChartAxis = class
  constructor Create(const aLabel, aColor, aQuantity, aUnit: string);
  private
    fLabel: string;
    fColor: string;
    fQuantity: string;
    fUnit: string;
  protected
    function getJSON: string; virtual;
  public
    property _label: string read fLabel write fLabel;
    property color: string read fColor write fColor;
    property quantity: string read fQuantity write fQuantity;
    property _unit: string read fUnit write fUnit;

    property toJSON: string read getJSON;
  end;

  TChartValue = record
  class function Create(aX: Double; const aY: TArray<Double>): TChartValue; static;
  private
    function getJSON: string;
  public
    x: Double;
    y: TArray<Double>;
    property toJSON: string read getJSON;
  end;

  TChartValues = TList<TChartValue>;

  TChart = class(TScenarioElement)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; const aChartType: string);
  protected
    fChartType: string;
    function getJSON: string; override;
    function getJSONData: string; virtual; abstract;
  public
    property chartType: string read fChartType;
  end;

  TChartLines = class(TChart)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; const aChartType: string;
    aXAxis: TChartAxis; aYAxes: TArray<TChartAxis>);
  destructor Destroy; override;
  private
    fXAxis: TChartAxis;
    fYAxes: TObjectList<TChartAxis>;
    fValues: TChartValues;
  protected
    function getJSON: string; override;
    function getJSONData: string; override;
  public
    property chartType: string read fChartType write fChartType;
    property xAxis: TChartAxis read fXAxis;
    property yAxes: TObjectList<TChartAxis> read fYAxes;
    property values: TChartValues read fValues;

    procedure AddValue(aX: Double; const aY: TArray<Double>);
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
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView; aUseSimulationSetup: Boolean);
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
    fUseSimulationSetup: Boolean;
    function getElementID: string; override;
    function selectLayersOnCategories(const aSelectCategories: TArray<string>; aLayers: TList<TLayer>): Boolean;
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
    property useSimulationSetup: Boolean read fUseSimulationSetup;
    procedure ReadBasicData(); virtual;
    procedure registerLayers;
  public
    function AddLayer(aLayer: TLayer): TLayer;
    function AddKPI(aKPI: TKPI): TKPI;
    function AddChart(aChart: TChart): TChart;
  public
    // select objects
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; const aSelectedIDs: TArray<string>): string; overload; virtual;
    // select object properties
    function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; virtual;
  end;

  TScenarioLink = class
  constructor Create(const aID, aParentID, aReferenceID: String; const aName, aDescription, aStatus: string; aLink: TScenario);
  destructor Destroy; override;
  private
    fChildren: TObjectList<TScenarioLink>;
    fLink: TScenario;
    fParent: TScenarioLink;
    fID: string; //Integer;
    fParentID: string; //Integer;
    fReferenceID: string; //Integer;
    fName: string;
    fDescription: string;
    fStatus: string;
  public
    property children: TObjectList<TScenarioLink> read fChildren;
    property link: TScenario read fLink write fLink;
    property parent: TScenarioLink read fParent write fParent;
    // scenario
    property ID: string read fID;
    property parentID: string read fParentID;
    property referenceID: string read fReferenceID;
    function name: string;
    property description: string read fDescription;
    property status: string read fStatus;

    function root: TScenarioLink;
    function findScenario(const aID: string): TScenarioLink;
    function removeLeaf(const aStatus: string): Boolean;
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
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
    const aSimulationSetup: string; aMaxNearestObjectDistanceInMeters: Integer);
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
    fClients: TGroup; // owns, lock with TMonitor
    fTimers: TTimerPool;
    fDiffLayers: TObjectDictionary<string, TDiffLayer>; // owns, lock with TMonitor
    // data
    fMeasuresJSON: string;
    fMeasures: TObjectDictionary<string, TMeasure>; // owns
    fScenarios: TObjectDictionary<string, TScenario>; // owns, lock with TMonitor
    fScenarioLinks: TScenarioLink;
    fProjectCurrentScenario: TScenario; // ref
    fProjectRefScenario: TScenario; // ref
    fTimeSlider: Integer;
    fSelectionEnabled: Boolean;
    fMeasuresEnabled: Boolean;
    fMeasuresHistoryEnabled: Boolean;
    fSimulationControlEnabled: Boolean;
    fAddBasicLayers: Boolean;

    fSimulationsetup: string;

    procedure setTimeSlider(aValue: Integer);
    procedure setSelectionEnabled(aValue: Boolean);
    procedure setMeasuresEnabled(aValue: Boolean);
    procedure setMeasuresHistoryEnabled(aValue: Boolean);
    function getMeasuresJSON: string; virtual;
    function getQueryDialogDataJSON: string; virtual;
    function ReadScenario(const aID: string): TScenario; virtual;

    procedure handleTilerStartup(aTiler: TTiler; aStartupTime: TDateTime);
    procedure timerTilerStatusAsHeartbeat(aTimer: TTimer);

    function getMeasuresHistoryJSON: string; virtual;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); virtual;
    procedure handleTypedClientMessage(const aMessageType: string; var aJSONObject: TJSONObject); virtual;
    procedure handleNewClient(aClient: TClient); virtual;
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
    property scenarioLinks: TScenarioLink read fScenarioLinks;
    property clients: TGroup read fClients; // owns, lock with TMonitor

    // todo: signal clients on write?
    property projectCurrentScenario: TScenario read fProjectCurrentScenario write fProjectCurrentScenario;
    property projectRefScenario: TScenario read fProjectRefScenario write fProjectRefScenario;

    property measuresJSON: string read getMeasuresJSON;
    property measures: TObjectDictionary<string, TMeasure> read fMeasures;

    property queryDialogDataJSON: string read getQueryDialogDataJSON;

    property timeSlider: Integer read fTimeSlider write setTimeSlider;
    property selectionEnabled: Boolean read fSelectionEnabled write setSelectionEnabled;
    property measuresEnabled: Boolean read fMeasuresEnabled write setMeasuresEnabled;
    property measuresHistoryEnabled: Boolean read fMeasuresHistoryEnabled write setMeasuresHistoryEnabled;
    property simualtionControlEnabled: Boolean read fSimulationControlEnabled write fSimulationControlEnabled;
    property simulationSetup: string read fSimulationSetup write fSimulationSetup;
    property addBasicLayers: Boolean read fAddBasicLayers;
  public
    property diffLayers: TObjectDictionary<string, TDiffLayer> read fDiffLayers;
    function diffElementID(aCurrent, aReference: TScenarioElement): string;
  public
    procedure SendDomains(aClient: TClient; const aPrefix: string); virtual;
    procedure SendRefresh();
    procedure SendPreview();
    procedure SendString(const aString: string);

    procedure forEachClient(aForEachClient: TForEachClient);
  private
    fGroups: TObjectDictionary<string, TGroup>;
  public
    // group mamanagement
    property groups: TObjectDictionary<string, TGroup> read fGroups;
    function addGroup(const aGroup: string; aPresenter: TClient): Boolean;
    function addGroupMember(const aGroup: string; aMember: TClient): Boolean;
    function getGroupMembersNoLocking(const aGroup: string; aMembers: TGroup): Boolean; // NO locking in function!
    function isPresenterNoLocking(const aGroup: string; aMember: TClient): Boolean; // NO locking in function!
    function removeGroupMember(const aGroup: string; aMember: TClient): Boolean;
    function removeGroupNoLocking(const aGroup: string; aNoSendCloseMember: TClient): Boolean; // NO locking in function!
    function removeMemberFromAllGroups(aMember: TClient): Boolean;
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

function DoubleToJSON(d: Double): string;

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
function BuildRamplLegendJSON(aPalette: TRampPalette; aReverse: Boolean=False; aWidth: Integer=300; aLogScale: Boolean=False; aTickFontSize: Integer=11): string;
function CreateBasicPalette: TWDPalette;

function compareLayerNames(const aLayer1, aLayer2: TLayer): Integer;

implementation

{ utils }

function DoubleToJSON(d: Double): string;
begin
  if d.IsNan
  then Result := 'null'
  else Result := d.toString(dotFormat);
end;

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

function BuildRamplLegendJSON(aPalette: TRampPalette; aReverse: Boolean; aWidth: Integer; aLogScale: Boolean; aTickFontSize: Integer): string;
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
  fClients := TGroup.Create(false); // refs
end;

destructor TClientSubscribable.Destroy;
begin
  forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.HandleElementRemove(Self);
    end);
  FreeAndNil(fClients);
  inherited;
end;

procedure TClientSubscribable.forEachClient(aForEachClient: TForEachClient);
var
  client: TClient;
begin
  TMonitor.Enter(clients);
  try
    for client in clients
    do aForEachClient(client);
  finally
    TMonitor.Exit(clients);
  end;
end;

function TClientSubscribable.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  TMonitor.Enter(clients);
  try
    if clients.IndexOf(aClient)<0 then
    begin
      clients.Add(aClient);
      if clients.Count=1
      then HandleFirstSubscriber(aClient);
      Result := True;
    end
    else Result := False;
  finally
    TMonitor.Exit(clients);
  end;
end;

function TClientSubscribable.HandleClientUnsubscribe(aClient: TClient): Boolean;
var
  i: Integer;
begin
  TMonitor.Enter(clients);
  try
    i := clients.IndexOf(aClient);
    if i>=0 then
    begin
      if clients.Count=1
      then HandleLastSubscriber(aClient);
      clients.Delete(i);
      Result := True;
    end
    else Result := False;
  finally
    TMonitor.Exit(clients);
  end;
end;

procedure TClientSubscribable.HandleFirstSubscriber(aClient: TClient);
begin
  // default no action
end;

procedure TClientSubscribable.HandleLastSubscriber(aClient: TClient);
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
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*20);
  // add event handlers
  fTilerLayer.onTilerInfo := handleTilerInfo;
  fTilerLayer.onRefresh := handleTilerRefresh;
  fTilerLayer.onPreview := handleTilerPreview;
  fCurrentLayer.addDiffLayer(Self);
  fReferenceLayer.addDiffLayer(Self);
  handleSubLayerInfo(nil); // if both layers are known on tiler we can start registering now
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
  if legendJSON<>''
  then Result := Result+',"legend":{'+legendJSON+'}';
end;

procedure TDiffLayer.handleRefreshTrigger(aTimeStamp: TDateTime);
var
  timeStampStr: string;
  tiles: string;
begin
  if aTimeStamp<>0
  then timeStampStr := FormatDateTime('yyyy-mm-dd hh:mm', aTimeStamp)
  else timeStampStr := '';
  // signal refresh to layer client
  tiles := fTilerLayer.URLTimeStamped;

  Log.WriteLn('TDiffLayer.handleRefreshTrigger for '+elementID+' ('+timeStampStr+'): '+tiles);

  forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.SendRefresh(elementID, timeStampStr, tiles);
      Log.WriteLn('TDiffLayer.handleRefreshTrigger for '+elementID+', direct subscribed client: '+aClient.fClientID, llNormal, 1);
    end);
  // signal current layer of diff layer refresh
  if Assigned(fCurrentLayer) then
  begin
    fCurrentLayer.scenario.forEachClient(
      procedure(aClient: TClient)
      begin
        aClient.SendRefreshDiff(fCurrentLayer.elementID, timeStampStr, Self.refJSON);
        Log.WriteLn('TDiffLayer.handleRefreshTrigger for '+elementID+', current layer subscribed client: '+aClient.fClientID, llNormal, 1);
      end);
  end;
end;

procedure TDiffLayer.handleSubLayerInfo(aLayer: TLayer);
begin
  if Assigned(tilerLayer) and (tilerLayer.URL='')
  then registerOnTiler;
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
  if Assigned(fCurrentLayer.tilerLayer) and Assigned(fReferenceLayer.tilerLayer)
  then aTilerLayer.signalAddDiffSlice(diffPalette, fCurrentLayer.tilerLayer.ID, fReferenceLayer.tilerLayer.ID)
  else
  begin
    if not Assigned(fCurrentLayer.tilerLayer)
    then Log.WriteLn('TDiffLayer.handleTilerInfo: fCurrentLayer.tilerLayer=nil on '+ElementID, llWarning);
    if not Assigned(fReferenceLayer.tilerLayer)
    then Log.WriteLn('TDiffLayer.handleTilerInfo: fReferenceLayer.tilerLayer=nil on '+ElementID, llWarning);
  end;
end;

procedure TDiffLayer.handleTilerPreview(aTilerLayer: TTilerLayer);
var
  pvBASE64: string;
begin
  if Assigned(fTilerLayer) then
  begin
    pvBASE64 := fTilerLayer.previewAsBASE64;
    // layer clients
    forEachClient(
      procedure(aClient: TClient)
      begin
        aClient.SendPreview(elementID, pvBASE64);
      end);
    Log.WriteLn('send diff preview on '+elementID);
  end;
end;

procedure TDiffLayer.handleTilerRefresh(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
begin
  try
    if Assigned(fSendRefreshTimer) then
    begin
      fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*5),
        procedure (aTimer: TTimer)
        begin
          handleRefreshTrigger(aTimeStamp);
        end);
    end
    else handleRefreshTrigger(aTimeStamp);
    // refresh preview also
    if Assigned(fPreviewRequestTimer) then
    begin
      fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*10);
      Log.WriteLn('TDiffLayer.handleTilerRefresh: triggered diff preview timer for '+elementID);
    end
    else
    begin
      Log.WriteLn('TDiffLayer.handleTilerRefresh:  direct trigger diff preview for '+elementID);
      fTilerLayer.signalRequestPreview;
    end;
  except
    on E: Exception
    do Log.WriteLn('Exception in TDiffLayer.handleTilerRefresh for ('+self.elementID+'): '+E.Message, llError);
  end;
end;

procedure TDiffLayer.registerOnTiler;
var
  currentLayerTilerUrl: string;
  referenceLayerTilerUrl: string;
begin
  if Assigned(fTilerLayer) and Assigned(fCurrentLayer) and Assigned(fReferenceLayer)
  then fTilerLayer.signalRegisterLayer(fCurrentLayer.scenario.project.tiler, 'diff-'+fCurrentLayer.Description+'-'+fReferenceLayer.description)
  else
  begin
    if Assigned(fCurrentLayer) and Assigned(fCurrentLayer.tilerLayer)
    then currentLayerTilerUrl := fCurrentLayer.tilerLayer.URL
    else currentLayerTilerUrl := '##';

    if Assigned(fReferenceLayer) and Assigned(fReferenceLayer.tilerLayer)
    then referenceLayerTilerUrl := fReferenceLayer.tilerLayer.URL
    else referenceLayerTilerUrl := '##';

    Log.WriteLn('TDiffLayer.registerOnTiler '+elementID+', '+Assigned(fTilerLayer).ToString()+': '+currentLayerTilerUrl+', '+referenceLayerTilerUrl, llWarning);
  end;
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
      try
        if aInt=actionDelete then
        begin
          Log.WriteLn('unlink from '+event.eventName);
          TMonitor.Enter(fProject.clients);
          try
            fClientEvent := nil;
            event.unPublish;
            event.unSubscribe;
            fProject.clients.Remove(Self);
          finally
            TMonitor.Exit(fProject.clients);
          end;
        end;
      except
        on E: Exception
        do Log.WriteLn('Exception in TClient.Create fClientEvent.OnIntString: '+E.Message, llError);
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
  if fProject.measuresEnabled
  then SendMeasures();
  if fProject.measuresHistoryEnabled
  then SendMeasuresHistory();
  if fProject.selectionEnabled
  then SendQueryDialogData();
  fProject.SendDomains(self, 'domains');
  fProject.handleNewClient(Self);
end;

destructor TClient.Destroy;
var
  se: TClientSubscribable;
begin
  fProject.removeMemberFromAllGroups(Self);
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
        fProject.SendDomains(self, 'domains');
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
    fProject.SendDomains(self, 'updatedomains');
    UpdateSession();
  end;

  procedure selectObjects(aSelectObjects: TJSONValue);
  var
    t: string;
    m: string;
    g: TJSONObject;
    measure: TJSONObject;
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
    jsonSelectedObjects: TJSONArray;
    jsonQuery: TJSONArray;
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
    if aSelectObjects.TryGetValue<TJSONObject>('geometry', g) then
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
    else if aSelectObjects.TryGetValue<TJSONObject>('measure', measure) then
    begin
      if Assigned(fCurrentScenario) then
      begin
        // decode objects from measure                         \
        if aSelectObjects.TryGetValue<TJSONArray>('selectedObjects', jsonSelectedObjects) then
        begin
          setLength(oids, jsonSelectedObjects.Count);
          for i := 0 to jsonSelectedObjects.Count-1
          do oids[i] := jsonSelectedObjects.Items[i].value;
        end
        else setLength(oids, 0);
        resp := fCurrentScenario.SelectObjects(Self, t, m, sc, oids);
      end;
    end
    else if aSelectObjects.TryGetValue<TJSONArray>('query', jsonQuery) then
    begin
      // select objects based on query
      if Assigned(fCurrentScenario)
      then resp := fCurrentScenario.SelectObjects(Self, t, m, sc, jsonQuery);
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
    sca := aSelectObjectsProperties.getValue<TJSONArray>('selectCategories');
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

  procedure handleTypedMessage(const aMessageType: string; var aJSONObject: TJSONObject);
  var
    group: string;
    members: TGroup;
    member: TClient;
    command: string;
    payload: TJSONObject;
  begin
    if aMessageType='ccv' then // send to viewers
    begin
      if aJSONObject.TryGetValue<string>('group', group) then
      begin
        members := TGroup.Create(false); // refs
        try
          TMonitor.Enter(fProject.groups);
          try
            if fProject.getGroupMembersNoLocking(group, members) then
            begin
              for member in members do
              begin
                if (member<>Self) and (member<>members[0])
                then member.signalString(aJSONObject.ToJSON);
              end;
            end;
          finally
            TMonitor.Exit(fProject.groups);
          end;
        finally
          members.Free;
        end;
      end;
    end
    else if aMessageType='ccp' then // send to presenter
    begin
      if aJSONObject.TryGetValue<string>('group', group) then
      begin
        members := TGroup.Create(false); // refs
        try
          TMonitor.Enter(fProject.groups);
          try
            fProject.getGroupMembersNoLocking(group, members);
            if (members.Count>0) and (members[0]<>Self)
            then members[0].signalString(aJSONObject.ToJSON);
          finally
            TMonitor.Exit(fProject.groups);
          end;
        finally
          members.Free;
        end;
      end;
    end
    else if aMessageType='ccb' then // broadcast
    begin
      if aJSONObject.TryGetValue<string>('group', group) then
      begin
        members := TGroup.Create(false); // refs
        try
          TMonitor.Enter(fProject.groups);
          try
            if fProject.getGroupMembersNoLocking(group, members) then
            begin
              for member in members do
              begin
                if member<>Self
                then member.signalString(aJSONObject.ToJSON);
              end;
            end;
          finally
            TMonitor.Exit(fProject.groups);
          end;
        finally
          members.Free;
        end;
      end;
    end
    else if aMessagetype='groupcontrol' then
    begin
      (*
      {
        "type":"groupcontrol"
        "payload":
        {
          "command":"presenteron"|"presenteroff"|"vieweron"|"vieweroff"|"leaveallgroups"
          ["group":""] (not on "leaveallgroups")
        }
      }
      answer:
      {
        "type":"groupcontrol"
        "payload":
        {
          "command": "..."
          ["group": "..."]
          "result":"success"|"groupalreadyexists"|"groupdoesnotexist"
        }
      }
      *)
      if aJSONObject.TryGetValue<TJSONObject>('payload', payload) and
         payload.TryGetValue<string>('command', command) and
         payload.TryGetValue<string>('group', group) then
      begin
        if command='presenteron' then
        begin
          if fProject.addGroup(group, Self)
          then payload.AddPair('result', 'success')
          else payload.AddPair('result', 'groupalreadyexists');
          signalString(aJSONObject.ToJSON);
          Log.WriteLn(aJSONObject.ToJSON, llRemark);
        end
        else if command='presenteroff' then
        begin
          TMonitor.Enter(fProject.groups);
          try
            if fProject.isPresenterNoLocking(group, Self) and fProject.removeGroupNoLocking(group, nil)
            then payload.AddPair('result', 'success')
            else payload.AddPair('result', 'groupdoesnotexist');
            signalString(aJSONObject.ToJSON);
            Log.WriteLn(aJSONObject.ToJSON, llRemark);
          finally
            TMonitor.Exit(fProject.groups);
          end;
        end
        else if command='vieweron' then
        begin
          if fProject.addGroupMember(group, Self)
          then payload.AddPair('result', 'success')
          else payload.AddPair('result', 'groupdoesnotexist');
          signalString(aJSONObject.ToJSON);
          Log.WriteLn(aJSONObject.ToJSON, llRemark);
        end
        else if command='vieweroff' then
        begin
          if fProject.removeGroupMember(group, Self)
          then payload.AddPair('result', 'success')
          else payload.AddPair('result', 'groupdoesnotexist');
          signalString(aJSONObject.ToJSON);
          Log.WriteLn(aJSONObject.ToJSON, llRemark);
        end
        else if command='leaveallgroups' then
        begin
          fProject.removeMemberFromAllGroups(Self);
          Log.WriteLn(aJSONObject.ToJSON, llRemark);
        end
        else Log.WriteLn('Unknown on command "'+command+'" in "groupcontrol" message', llWarning);
      end;
    end
    // handle all other typed messages on the connected project
    else fProject.handleTypedClientMessage(aMessageType, aJSONObject);
  end;

var
  jsonObject: TJSONObject;
  jsonPair: TJSONPair;
  messageType: string;
begin
  try
    if Assigned(fProject) and Assigned(fClientEvent) then
    begin
      //Log.WriteLn('message from client: '+event.eventName+': '+aString);
      // process message
      jsonObject := TJSONObject.ParseJSONValue(aJSONString) as TJSONObject;
      try
        if jsonObject.TryGetValue<string>('type', messageType)
        then handleTypedMessage(messageType, jsonObject)
        else if isObject(jsonObject, 'subscribe', jsonPair)
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
        else fProject.handleClientMessage(Self, fCurrentScenario, jsonObject);
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
  signalString('{"type":"refresh","payload":{"id":"'+aElementID+'","preview":"'+aPreviewBASE64+'"}}');
end;

procedure TClient.sendQueryDialogData;
begin
  signalString('{"type":"queryDialogData","payload":'+fProject.queryDialogDataJSON+'}');
end;

procedure TClient.SendRefresh(const aElementID, aTimeStamp, aObjectsTilesLink: string);
begin
  signalString('{"type":"refresh","payload":{"id":"'+aElementID+'","timestamp":"'+aTimeStamp+'","tiles":"'+aObjectsTilesLink+'"}}');
end;

procedure TClient.SendRefreshDiff(const aElementID, aTimeStamp, aDiff: string);
begin
  signalString('{"type":"refresh","payload":{"id":"'+aElementID+'","timestamp":"'+aTimeStamp+'","diff":{'+aDiff+'}}}');
end;

procedure TClient.SendRefreshRef(const aElementID, aTimeStamp, aRef: string);
begin
  // todo: how to see that a layer is a ref layer for this client?
  signalString('{"type":"refresh","payload":{"id":"'+aElementID+'","timestamp":"'+aTimeStamp+'","ref":{'+aRef+'}}}');
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
    if Assigned(fCurrentScenario)
    then Result :=
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

  function jsonSimulationSetup: string;
  begin

    if Assigned(fCurrentScenario) and fCurrentScenario.useSimulationSetup and (fProject.simulationSetup<>'')
    then Result := '"simulationSetup":{"data":'+fProject.simulationSetup+'},'
    else Result := '"simulationSetup":0,';
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
      jsonSimulationSetup+
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

procedure TClient.setCurrentScenario(const aValue: TScenario);
begin
  fCurrentScenario := aValue;
  fProject.SendDomains(self, 'updatedomains');
  UpdateSession;
end;

procedure TClient.setRefScenario(const aValue: TScenario);
begin
  fRefScenario := aValue;
  fProject.SendDomains(self, 'updatedomains');
  UpdateSession;
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
    '"show":'+Ord(defaultLoad).ToString; // todo: change to float -> opacity
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

function TLayerObject.EncodeRemove: TByteBuffer;
begin
  Result := TByteBuffer.bb_tag_rawbytestring(icehNoObjectID, ID);
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

function TGeometryPointLayerObject.getJSON2D(const aType, aExtraJSON2DAttributes: string): string;
var
  colors: TGeoColors;
  opacity: double;
begin
  if Assigned(fGeometryPoint) then
  begin
    if Assigned(fLayer.tilerLayer) and Assigned(fLayer.tilerLayer.palette)
    then colors := fLayer.tilerLayer.palette.ValueToColors(fValue)
    else colors := TGeoColors.Create($FF000000); // black
    opacity := (colors.mainColor shr 24)/$FF;
    Result := '{ '+
        '"type":"Feature",'+
        '"geometry":{'+
          '"type":"'+fLayer.GeometryType+'",'+
          '"coordinates":'+fGeometryPoint.JSON2D[aType]+
        '},'+
        '"properties":{'+
          '"id":"'+string(ID)+'",'+
          '"color": "'+ColorToJSON(colors.mainColor)+'",'+
          '"fillOpacity":'+opacity.ToString(dotFormat)+
          aExtraJSON2DAttributes+
        '}'+
    	'}';
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

function TGeometryLayerPOIObject.getJSON2D(const aType, aExtraJSON2DAttributes: string): string;
begin
  if Assigned(fGeometryPoint) then
    Result := '{ '+
      '"type":"Feature",'+
      '"geometry":{'+
        '"type":"'+fLayer.GeometryType+'",'+
        '"coordinates":'+fGeometryPoint.JSON2D[aType]+
      '},'+
      '"properties":{'+
        '"id":"'+string(ID)+'",'+
        '"poi": "'+fPOI.ToString+'"'+
        aExtraJSON2DAttributes+
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
  d: Double;
  p: Integer;
  x1,y1,x2,y2: Double;
begin
  if (fLayer.fGeometryType='MultiPolygon') and fGeometry.PointInAnyPart(aX, aY)
  then Result := 0
  else
  begin
    Result := Infinity;
    for part in fGeometry.parts do
    begin
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
            // distance to line segment
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

function TGeometryLayerObject.getJSON2D(const aType, aExtraJSON2DAttributes: string): string;
var
  colors: TGeoColors;
  opacity: double;
begin
  if Assigned(fGeometry) then
  begin
    if Assigned(fLayer.tilerLayer) and Assigned(fLayer.tilerLayer.palette)
    then colors := fLayer.tilerLayer.palette.ValueToColors(fValue)
    else colors := TGeoColors.Create($FF000000); // black
    opacity := (colors.mainColor shr 24)/$FF;
    Result := '{ '+
        '"type":"Feature",'+
        '"geometry":{'+
          '"type":"'+fLayer.GeometryType+'",'+
          '"coordinates":'+fGeometry.JSON2D[aType]+
        '},'+
        '"properties":{'+
          '"id":"'+string(ID)+'",'+
          '"color": "'+ColorToJSON(colors.mainColor)+'",'+
          '"fillOpacity":'+opacity.ToString(dotFormat)+
          aExtraJSON2DAttributes+
        '}'+
    	'}';
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

{ TLayerUpdateObject }

constructor TLayerUpdateObject.Create(const aID: TWDID);
begin
  inherited Create;
  fID := aID;
  fAttributes := TDictionary<string, string>.Create;
end;

destructor TLayerUpdateObject.Destroy;
begin
  FreeAndNil(fAttributes);
  inherited;
end;

{ TAttrNameValue }

class function TAttrNameValue.Create(const aName, aValue: string): TAttrNameValue;
begin
  Result.name := aName;
  Result.value := aValue;
end;

{ TLayer }

procedure TLayer.addDiffLayer(aDiffLayer: TDiffLayer);
var
  i: Integer;
begin
  TMonitor.Enter(fDependentDiffLayers);
  try
    i := fDependentDiffLayers.IndexOf(aDiffLayer);
    if i<0 then
    begin
      fDependentDiffLayers.Add(aDiffLayer);
      // subscribe all clients to this diff layer also
      forEachClient(
        procedure(aClient: TClient)
        begin
          aDiffLayer.HandleClientSubscribe(aClient);
        end);
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
    signalObject(aObject);
  finally
    fObjectsLock.EndWrite;
  end;
  fLayerUpdateTimer.Arm(fLayerUpdateTimer.MaxPostponeDelta, handleLayerUpdateTrigger);
end;

procedure TLayer.AddObjectAttribute(const aID: TWDID; const aAttributes: TArray<TAttrNameValue>);
var
  luo: TLayerUpdateObject;
  nv: TAttrNameValue;
begin
  TMonitor.Enter(fObjectsAdded);
  try
    if not fObjectsAdded.TryGetValue(aID, luo) then
    begin
      luo := TLayerUpdateObject.Create(aID);
      fObjectsAdded.Add(aID, luo);
    end;
    for nv in aAttributes do
    begin
      luo.attributes.AddOrSetValue(nv.name, nv.value);
    end;
  finally
    TMonitor.Exit(fObjectsAdded);
  end;
  fLayerUpdateTimer.Arm(fLayerUpdateTimer.MaxPostponeDelta, handleLayerUpdateTrigger);
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
  const aObjectTypes, aGeometryType, aLayerType: string; aShowInDomains: Boolean; aDiffRange: Double; aBasicLayer: Boolean=False);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  //fPalette := aPalette;
  fObjectTypes := aObjectTypes;
  fGeometryType := aGeometryType;
  fDiffRange := aDiffRange;
  fBasicLayer := aBasicLayer;
  fLayerType := aLayerType;
  fShowInDomains := aShowInDomains;
  fObjects := TObjectDictionary<TWDID, TLayerObject>.Create([doOwnsValues]);

  fObjectsAdded := TObjectDictionary<TWDID, TLayerUpdateObject>.Create([doOwnsValues]);// <TLayerObject>.Create(False); // refs
  fObjectsUpdated := TObjectDictionary<TWDID, TLayerUpdateObject>.Create([doOwnsValues]); // owns
  fObjectsDeleted := TObjectList<TLayerObject>.Create(True); // owns

  fExtraJSON2DAttributes := '';

  fObjectsLock.Create;
  fDependentDiffLayers := TObjectList<TDiffLayer>.Create(False);
  fLegendJSON := '';
  fQuery := '';
  fTilerLayer := nil;
  fPreviewRequestTimer := scenario.project.Timers.SetTimer(
    procedure (aTimer: TTImer)
    begin
      if Assigned(fTilerLayer) then
      begin
        Log.WriteLn('triggered preview timer for '+elementID);
        fTilerLayer.signalRequestPreview;
      end
      else Log.WriteLn('## triggered preview timer for '+elementID+' without having a tiler layer', llError);
    end);
  fSendRefreshTimer := scenario.project.Timers.CreateInactiveTimer;// SetTimer(handleLayerRefresh);
  fSendRefreshTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*20);
  fLayerUpdateTimer := scenario.project.Timers.CreateInactiveTimer;
  fLayerUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond/5); // 5Hz
end;

destructor TLayer.Destroy;
begin
  FreeAndNil(fTilerLayer);
  inherited;
  FreeAndNil(fObjectsAdded);
  FreeAndNil(fObjectsUpdated);
  FreeAndNil(fObjectsDeleted);
  FreeAndNil(fObjects);
  FreeAndNil(fDependentDiffLayers);
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
      aObjectsJSON := aObjectsJSON+o.Value.JSON2D[Self.fGeometryType, ''];
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
        aObjectsJSON := aObjectsJSON+o.Value.JSON2D[Self.fGeometryType, ''];
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
begin
  fTilerLayer.Free;
  // recreate tiler layer definition
  fTilerLayer := TTilerLayer.Create(scenario.project.Connection, elementID, aSliceType, aPalette);//, -1, '' .addLayer(elementID, aSliceType, aPalette);
  // add handlers
  fTilerLayer.onTilerInfo := handleTilerInfo;
  fTilerLayer.onRefresh := handleTilerRefresh;
  fTilerLayer.onPreview := handleTilerPreview;
  // trigger registration
  fTilerLayer.signalRegisterLayer(scenario.project.tiler, aDescription, aPersistent, aEdgeLengthInMeters);
  // todo: handle diff layer?

end;

procedure TLayer.RegisterSlice;
begin
  // default no action
end;

procedure TLayer.removeDiffLayer(aDiffLayer: TDiffLayer);
var
  i: Integer;
begin
  TMonitor.Enter(fDependentDiffLayers);
  try
    i := fDependentDiffLayers.IndexOf(aDiffLayer);
    if i>=0 then
    begin
      fDependentDiffLayers.Delete(i);
      // unsubscribe all clients from this diff layer also
      forEachClient(
        procedure(aClient: TClient)
        begin
          aDiffLayer.HandleClientUnsubscribe(aClient);
        end);
    end;
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

procedure TLayer.RemoveObject(aObject: TLayerObject);
begin
  fObjectsLock.BeginWrite;
  try
    //fObjectsAdded.Remove(aObject);
    TMonitor.Enter(fObjectsAdded);
    try
      fObjectsAdded.Remove(aObject.ID);
    finally
      TMonitor.Exit(fObjectsAdded);
    end;
    TMonitor.Enter(fObjectsUpdated);
    try
      fObjectsUpdated.Remove(aObject.ID);
    finally
      TMonitor.Exit(fObjectsUpdated);
    end;
    fObjects.ExtractPair(aObject.id); // extract from active list
    fObjectsDeleted.Add(aObject); // add to deleted list
    signalNoObject(aObject);
  finally
    fObjectsLock.EndWrite;
  end;
  fLayerUpdateTimer.Arm(fLayerUpdateTimer.MaxPostponeDelta, handleLayerUpdateTrigger);
end;

const
  MaxBufferLength = 2048;

procedure TLayer.signalObjects(aSender: TObject; aObjects: TObjectDictionary<TWDID, TLayerObject>);
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
    fTilerLayer.signalPalette(timeStamp);
    // signal objects (which will trigger creation of layer if not already done above)
    buffer := '';
    for iop in aObjects do
    begin
      buffer := buffer+iop.Value.encode;
      if length(buffer)> MaxBufferLength
      then Commit;
    end;
    Commit;
  end;
end;

procedure TLayer.signalNoObject(aObject: TLayerObject);
var
  timeStamp: TDateTime;
begin
  timeStamp := 0; // todo:
  fTilerLayer.signalData(aObject.EncodeRemove, timeStamp);
end;

procedure TLayer.signalObject(aObject: TLayerObject);
var
  timeStamp: TDateTime;
begin
  timeStamp := 0; // todo:
  fTilerLayer.signalData(aObject.encode, timeStamp);
end;

procedure TLayer.signalObjects(aSender: TObject);
begin
  signalObjects(aSender, fObjects);
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

procedure TLayer.UpdateObjectAttribute(const aID: TWDID; const aAttribute, aValue: string);
var
  luo: TLayerUpdateObject;
begin
  TMonitor.Enter(fObjectsUpdated);
  try
    if not fObjectsUpdated.TryGetValue(aID, luo) then
    begin
      luo := TLayerUpdateObject.Create(aID);
      fObjectsUpdated.Add(aID, luo);
    end;
    luo.attributes.AddOrSetValue(aAttribute, aValue);
  finally
    TMonitor.Exit(fObjectsUpdated);
  end;
  fLayerUpdateTimer.Arm(fLayerUpdateTimer.MaxPostponeDelta, handleLayerUpdateTrigger);
end;

function TLayer.getJSON: string;
begin
  Result := inherited getJSON+','+
    '"basic":'+Ord(basicLayer).ToString+','+
    '"objectTypes":['+objectTypes+'],'+
    '"preview":"'+previewBASE64+'",'+
    '"tiles":"'+uniqueObjectsTilesLink+'",'+
    '"type":"'+layerType+'"';
  if legendJSON<>''
  then Result := Result+',"legend":{'+legendJSON+'}';
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
    then jsonAdd(Result, iop.Value.JSON2D[fGeometryType, fExtraJSON2DAttributes]);
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
  // handle unsubscribe on dependent diff layers
  TMonitor.Enter(fDependentDiffLayers);
  try
    for diffLayer in fDependentDiffLayers
    do diffLayer.HandleClientUnsubscribe(aClient);
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

procedure TLayer.handleLayerUpdateTrigger(aTimer: TTimer);
var
  o: TLayerObject;
  iluop: TPair<TWDID, TLayerUpdateObject>;
  anvp: TPair<string, string>;
  _json: string;
begin
  _json := '';
  fObjectsLock.BeginRead;
  try
    // process add objects
    TMonitor.Enter(fObjectsAdded);
    try
      for iluop in fObjectsAdded do
      begin
        if _json<>''
        then _json := _json+',';
        _json := _json+'{"newobject":{"id":"'+string(UTF8String(iluop.Key))+'"';
        for anvp in iluop.Value.attributes
        do _json := _json+',"'+anvp.Key+'":'+anvp.Value;
        _json := _json+'}}';
      end;
      fObjectsAdded.Clear;
    finally
      TMonitor.Exit(fObjectsAdded);
    end;
    // process object updates
    TMonitor.Enter(fObjectsUpdated);
    try
      for iluop in fObjectsUpdated do
      begin
        if _json<>''
        then _json := _json+',';
        _json := _json+'{"updateobject":{"id":"'+string(UTF8String(iluop.Key))+'"';
        for anvp in iluop.Value.attributes
        do _json := _json+',"'+anvp.Key+'":'+anvp.Value;
        _json := _json+'}}';
      end;
      fObjectsUpdated.Clear;
    finally
      TMonitor.Exit(fObjectsUpdated);
    end;
    for o in fObjectsDeleted do
    begin
      if _json<>''
      then _json := _json+',';
      _json := _json+'{"removeobject":{"id":"'+string(UTF8String(o.ID))+'"}}';
    end;
    fObjectsDeleted.Clear;
  finally
    fObjectsLock.EndRead;
  end;
  if _json<>'' then
  begin
    _json := '{"type":"updatelayer","payload":{"id":"'+ElementID+'","data":['+_json+']}}';
    forEachClient(procedure(aClient: TClient)
      begin
        aClient.signalString(_json);
      end);
  end;
end;

procedure TLayer.handleRefreshTrigger(aTimeStamp: TDateTime);
var
  timeStampStr: string;
  tiles: string;
begin
  if aTimeStamp<>0
  then timeStampStr := FormatDateTime('yyyy-mm-dd hh:mm', aTimeStamp)
  else timeStampStr := '';

  Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+' ('+timeStampStr+'): '+tiles);

  // signal refresh to layer client
  tiles := uniqueObjectsTilesLink;
  forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.SendRefresh(elementID, timeStampStr, tiles);
      Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+', direct subscribed client: '+aClient.fClientID, llNormal, 1);
    end);
  // signal refresh to scenario client
  fScenario.forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.SendRefresh(elementID, timeStampStr, tiles);
      Log.WriteLn('TLayer.handleRefreshTrigger for '+elementID+', current layer subscribed client: '+aClient.fClientID, llNormal, 1);
    end);
end;

procedure TLayer.handleTilerInfo(aTilerLayer: TTilerLayer);
var
  dl: TDiffLayer;
begin
  RegisterSlice;
  AddCommandToQueue(Self, Self.signalObjects);
  TMonitor.Enter(fDependentDiffLayers);
  try
    for dl in fDependentDiffLayers
    do dl.handleSubLayerInfo(Self);
  finally
    TMonitor.Exit(fDependentDiffLayers);
  end;
end;

procedure TLayer.handleTilerPreview(aTilerLayer: TTilerLayer);
var
  pvBASE64: string;
begin
  pvBASE64 := previewBASE64;
  // layer clients
  forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.SendPreview(elementID, pvBASE64);
    end);
  // scenario clients
  fScenario.forEachClient(
    procedure(aClient: TClient)
    begin
      if not clients.Contains(aClient)
      then aClient.SendPreview(elementID, pvBASE64);
    end);
  Log.WriteLn('send normal preview on '+elementID);
end;

procedure TLayer.handleTilerRefresh(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
begin
  try
    if Assigned(fSendRefreshTimer) then
    begin
      fSendRefreshTimer.Arm(DateTimeDelta2HRT(dtOneSecond*5),
        procedure(aTimer: TTimer)
        begin
          handleRefreshTrigger(aTimeStamp);
        end);
    end
    else handleRefreshTrigger(aTimeStamp);
    // refresh preview also
    if Assigned(fPreviewRequestTimer)
    then fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneSecond*10)
    else
    begin
      Log.WriteLn('triggered preview timer for '+elementID);
      aTilerLayer.signalRequestPreview;
    end;
  except
    on E: Exception
    do Log.WriteLn('Exception in TLayer.handleTilerRefresh ('+self.elementID+'): '+E.Message, llError);
  end;
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
begin
  // send update to clients
  _json := '{"updatekpi":{'+JSON+'}}';
  forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.signalString(_json);
    end);
  // send also to clients on scenario for udpate of preview
  fScenario.forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.signalString(_json);
    end);
end;

(*
{ TChartGroupRow }

class function TChartGroupRow.Create(const aGroup: string; const aValues: TArray<Double>): TChartGroupRow;
begin
  Result.group := aGroup;
  Result.values := aValues;
end;
*)

{ TChartAxis }

constructor TChartAxis.Create(const aLabel, aColor, aQuantity, aUnit: string);
begin
  inherited Create;
  fLabel  := aLabel;
  fColor := aColor;
  fQuantity := aQuantity;
  fUnit := aUnit;
end;

function TChartAxis.getJSON: string;
begin
  Result := '{"label":"'+fLabel+'","color":"'+fColor+'","qnt":"'+fQuantity+'","unit":"'+fUnit+'"}';
end;

{ TChartValue }

class function TChartValue.Create(aX: Double; const aY: TArray<Double>): TChartValue;
begin
  Result.x := aX;
  Result.y := aY;
end;

function TChartValue.getJSON: string;
var
  res: string;
  v: Double;
begin
  res := '';
  for v in y do
  begin
    if res<>''
    then res := res+',';
    res := res+DoubleToJSON(v);
  end;
  Result := '{"x":'+DoubleToJSON(x)+',"y":['+res+']}';
end;

{ TChartLines }

procedure TChartLines.AddValue(aX: Double; const aY: TArray<Double>);
var
  v: TChartValue;
begin
  v := TChartValue.Create(aX, aY);
  fValues.Add(TChartValue.Create(aX, aY));
  scenario.project.forEachClient(procedure(aClient: TClient)
    begin
      // todo: can be for more then 1 chart!
      aClient.signalString('{"type":"updatechart","payload":[{"id":"'+elementID+'","data":['+v.toJSON+']}]}');
    end);
end;

constructor TChartLines.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; const aChartType: string;
  aXAxis: TChartAxis; aYAxes: TArray<TChartAxis>);
var
  axis: TChartAxis;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aChartType);
  if Assigned(aXAxis)
  then fXAxis := aXAxis
  else fXAxis := TChartAxis.Create('', '', '', '');
  fYAxes := TObjectList<TChartAxis>.Create(True);
  for axis in aYAxes
  do fYAxes.Add(axis);
  fValues := TChartValues.Create;
end;

destructor TChartLines.Destroy;
begin
  inherited;
  FreeAndNil(fXAxis);
  FreeAndNil(fYAxes);
  FreeAndNil(fValues);
end;

function TChartLines.getJSON: string;
var
  axis: TChartAxis;
  res: string;
begin
  Result := inherited getJSON;
  if Result<>''
  then Result := Result+',';
  Result :=
    Result+'"x":'+fXAxis.toJSON;
  res := '';
  for axis in fYAxes do
  begin
    if res<>''
    then res := res+',';
    res  := res+axis.toJSON;
  end;
  Result := Result+',"y":['+res+']';
end;

function TChartLines.getJSONData: string;
var
  v: TChartValue;
begin
  Result := '';
  for v in values do
  begin
    if Result<>''
    then Result := Result+',';
    Result := Result+v.toJSON;
  end;
end;

{ TScenario }

function TScenario.AddChart(aChart: TChart): TChart;
begin
  TMonitor.Enter(fCharts);
  try
    fCharts.AddOrSetValue(aChart.ID, aChart);
  finally
    TMonitor.Exit(fCharts);
  end;
  Result := aChart;
end;

function TScenario.AddKPI(aKPI: TKPI): TKPI;
begin
  TMonitor.Enter(fKPIs);
  try
    fKPIs.AddOrSetValue(aKPI.ID, aKPI);
  finally
    TMonitor.Exit(fKPIs);
  end;
  Result := aKPI;
end;

function TScenario.AddLayer(aLayer: TLayer): TLayer;
begin
  TMonitor.Enter(fLayers);
  try
    fLayers.AddOrSetValue(aLayer.ID, aLayer);
  finally
    TMonitor.Exit(fLayers);
  end;
  Result := aLayer;
end;

constructor TScenario.Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView; aUseSimulationSetup: Boolean);
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
  fUseSimulationSetup := aUseSimulationSetup;
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

procedure TScenario.registerLayers;
var
  layer: TLayer;
begin
  for layer in fLayers.Values
  do layer.RegisterLayer;
end;

function TScenario.selectLayersOnCategories(const aSelectCategories: TArray<string>; aLayers: TList<TLayer>): Boolean;
var
  ilp: TPair<string, TLayer>;
  cat: string;
begin
  Result := False;
  if length(aSelectCategories)>0  then
  begin
    // select basic layers on categories
    for ilp in fLayers do
    begin
      for cat in aSelectCategories do
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

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories, aSelectedIDs: TArray<string>): string;
begin
  Result := '';
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string;
begin
  Result := '';
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string;
begin
  Result := '';
end;

function TScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string;
begin
  Result := '';
end;

function TScenario.selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string;
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

constructor TScenarioLink.Create(const aID, aParentID, aReferenceID: string; const aName, aDescription, aStatus: string; aLink: TScenario);
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

function TScenarioLink.findScenario(const aID: string): TScenarioLink;
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
      '{"id":"'+child.ID+'",'+
       '"name":"'+child.name+'",'+
       '"description":"'+child.description+'",'+
       '"status":"'+child.status+'",'+
       '"reference":"'+child.referenceID+'",'+
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

function TScenarioLink.removeLeaf(const aStatus: string): Boolean;
var
  i: Integer;
begin
  for i := fChildren.Count-1 downto 0 do
  begin
    if fChildren[i].removeLeaf(aStatus)
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
  Result := TClient.Create(Self, fProjectCurrentScenario, fProjectRefScenario, aClientID);
  TMonitor.Enter(clients);
  try
    clients.Add(Result);
  finally
    TMonitor.Exit(clients);
  end;
end;

function TProject.addGroup(const aGroup: string; aPresenter: TClient): Boolean;
var
  group: TGroup;
begin
  Result := False; // sentinel
  TMonitor.Enter(fGroups);
  try
    if not fGroups.ContainsKey(aGroup) then
    begin
      group := TGroup.Create(false);
      try
        group.Add(aPresenter);
        Result := True;
      finally
        fGroups.Add(aGroup, group);
      end;
    end;
  finally
    TMonitor.Exit(fGroups);
  end;
end;

function TProject.addGroupMember(const aGroup: string; aMember: TClient): Boolean;
var
  group: TGroup;
begin
  Result := False; // sentinel
  TMonitor.Enter(fGroups);
  try
    if fGroups.TryGetValue(aGroup, group) then
    begin
      if not group.Contains(aMember)
      then group.Add(aMember);
      Result := true;
    end;
  finally
    TMonitor.Exit(fGroups);
  end;
end;

constructor TProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aDBConnection: TCustomConnection;
  aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
  const aSimulationSetup: string; aMaxNearestObjectDistanceInMeters: Integer);
begin
  inherited  Create;
  fGroups := TObjectDictionary<string, TGroup>.Create([doOwnsValues]);
  fTimeSlider := aTimeSlider;
  fSelectionEnabled := aSelectionEnabled;
  fMeasuresEnabled := aMeasuresEnabled;
  fMeasuresHistoryEnabled := aMeasuresHistoryEnabled;
  fSimulationControlEnabled := aSimulationControlEnabled;
  fSimulationSetup := aSimulationSetup;
  fAddBasicLayers := aAddbasicLayers;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fClients := TGroup.Create(true); // owns
  fScenarios := TObjectDictionary<string, TScenario>.Create([doOwnsValues]);
  fScenarioLinks := TScenarioLink.Create('', '', '', '', '', '', nil);
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
  fProjectCurrentScenario := nil;
  fProjectRefScenario := nil;
  fMeasures := TObjectDictionary<string, TMeasure>.Create([doOwnsValues]);
  // projection
  // imb init
  fProjectEvent := fConnection.subscribe(WS2IMBEventName+'.'+fProjectID, False);
  // read basic data
  ReadBasicData();
  // add handler for new sessions
  fProjectEvent.OnIntString.Add(
    procedure(event: TEventEntry; aInt: Integer; const aString: string)
    begin
      try
        if aInt=actionNew then
        begin
          Log.WriteLn(aProjectId+': link to '+aString);
          AddClient(aString);
        end;
      except
        on E: Exception
        do Log.WriteLn('Exception in TProject.Create fProjectEvent.OnIntString: '+E.Message, llError);
      end;
    end);

  // todo: start timer for tiler status as a heartbeat
  if aTilerStatusURL<>'' then
  begin
    timers.SetTimer(timerTilerStatusAsHeartbeat, hrtNow+DateTimeDelta2HRT(dtOneHour), DateTimeDelta2HRT(dtOneHour));
    log.WriteLn('Set status timer: '+aTilerStatusURL);
  end;
end;

destructor TProject.Destroy;
begin
  FreeAndNil(fDiffLayers);
  FreeAndNil(fTimers);
  FreeAndNil(fClients);
  FreeAndNil(fGroups);
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

procedure TProject.forEachClient(aForEachClient: TForEachClient);
var
  client: TClient;
begin
  TMonitor.Enter(clients);
  try
    for client in clients
    do aForEachClient(client);
  finally
    TMonitor.Exit(clients);
  end;
end;

function TProject.getGroupMembersNoLocking(const aGroup: string; aMembers: TGroup): Boolean;
// NO locking
var
  group: TGroup;
begin
  Result := False; // sentinel
  if fGroups.TryGetValue(aGroup, group) then
  begin
    aMembers.AddRange(group.ToArray);
    Result := true;
  end;
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

function TProject.getQueryDialogDataJSON: string;
begin
  Result := 'null'; // in case of real dat build JSON object {}
end;

procedure TProject.handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject);
begin
  // default no action
end;

procedure TProject.handleNewClient(aClient: TClient);
begin
  // default no action
end;

procedure TProject.handleTilerStartup(aTiler: TTiler; aStartupTime: TDateTime);
var
  scenario: TScenario;
  diffLayer: TDiffLayer;
begin
  for scenario in fScenarios.Values
  do scenario.registerLayers;
  // diff layers
  TMonitor.Enter(diffLayers);
  try
    for diffLayer in diffLayers.Values do
    begin
      if Assigned(diffLayer.tilerLayer) then
      begin
        // reset tiler url to avoid invalid url send to client
        diffLayer.tilerLayer.resetURL;
        // re-register
        diffLayer.registerOnTiler;
      end;
    end;
  finally
    TMonitor.Exit(diffLayers);
  end;
end;

function TProject.isPresenterNoLocking(const aGroup: string; aMember: TClient): Boolean;
var
  group: TGroup;
begin
  Result := fGroups.TryGetValue(aGroup, group) and (group.Count>0) and (group[0]=aMember);
end;

procedure TProject.handleTypedClientMessage(const aMessageType: string; var aJSONObject: TJSONObject);
begin
  // default no processing
end;

function TProject.ReadScenario(const aID: string): TScenario;
begin
  Result := nil;
end;

function TProject.removeGroupNoLocking(const aGroup: string; aNoSendCloseMember: TClient): Boolean;
var
  group: TGroup;
  member: TClient;
begin
  Result := False; //  sentinel
  if fGroups.TryGetValue(aGroup, group) then
  begin
    // signal clients that group is going 'away'
    for member in group do
    begin
      if member<>aNoSendCloseMember
      then member.signalString('{"type":"groupcontrol","payload":{"command":"groupclose","group":"'+aGroup+'"}}');
    end;
    fGroups.Remove(aGroup);
    Result := True;
  end;
end;

function TProject.removeGroupMember(const aGroup: string; aMember: TClient): Boolean;
var
  group: TGroup;
begin
  Result := False; // sentinel
  TMonitor.Enter(fGroups);
  try
    if fGroups.TryGetValue(aGroup, group) then
    begin
      if group.Contains(aMember) then
      begin
        if group[0]<>aMember then
        begin
          group.Remove(aMember);
          Result := True;
        end;
      end
      else Result := True;
    end;
  finally
    TMonitor.Exit(fGroups);
  end;
end;

function TProject.removeMemberFromAllGroups(aMember: TClient): Boolean;
var
  group: string;
  removeGroups: TList<string>;
  ngp: TPair<string, TGroup>;
begin
  Result := False; //  sentinel
  TMonitor.Enter(fGroups);
  try
    removeGroups := TList<string>.Create;
    try
      for ngp in fGroups do
      begin
        if ngp.Value.Contains(aMember) then
        begin
          // check for presenter
          if ngp.Value[0]=aMember
          then removeGroups.Add(ngp.Key)
          else ngp.Value.Remove(aMember);
          Result := true;
        end;
      end;
      // remove groups where aMember was presenter
      for group in removeGroups
      do removeGroupNoLocking(group, aMember);
    finally
      removeGroups.Free;
    end;
  finally
    TMonitor.Exit(fGroups);
  end;
end;

procedure TProject.SendDomains(aClient: TClient; const aPrefix: string);
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
  _diffElementID: string;
begin
  if Assigned(aClient.refScenario)
  then Log.WriteLn('TClient.SendDomains ('+aPrefix+'), ref scenario '+aClient.refScenario.ID)
  else Log.WriteLn('TClient.SendDomains ('+aPrefix+'): no ref scenario');
  // todo: add reference and diff layers/charts if fRefScenario<>nil
  domains := TDictionary<string, TClientDomain>.Create;
  try
    if Assigned(aClient.currentScenario) then
    begin
      // layers
      locLayers := TList<TLayer>.Create(TComparer<TLayer>.Construct(compareLayerNames));
      try
        locLayers.AddRange(aClient.currentScenario.Layers.Values);
        locLayers.Sort;
        for layer in locLayers do
        begin
          if layer.showInDomains then
          begin
            JSON := layer.JSON;
            if Assigned(aClient.refScenario) then
            begin
              if aClient.refScenario.Layers.TryGetValue(layer.ID, refLayer) then
              begin
                // todo: full JSON for ref and diff, to include legend?
                JSON := JSON+',"ref":{'+refLayer.refJSON+'}';
                _diffElementID :=  diffElementID(layer, refLayer);
                TMonitor.Enter(diffLayers);
                try
                  if not diffLayers.TryGetValue(_diffElementID, diffLayer) then
                  begin
                    diffLayer := TDiffLayer.Create(_diffElementID, layer, refLayer);
                    diffLayers.Add(_diffElementID, diffLayer);
                    // todo:

                  end;
                finally
                  TMonitor.Exit(diffLayers);
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
        end;
      finally
        locLayers.Free;
      end;
      // kpis
      for nkp in aClient.currentScenario.KPIs do
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
      for ngp in aClient.currentScenario.Charts do
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
    aClient.signalString('{"'+aPrefix+'":{'+domainsJSON+'}}'); // default prefix is "domains":..
  finally
    domains.Free;
  end;
end;

procedure TProject.SendPreview;
begin
  Log.WriteLn('Sending preview to clients connected to project '+Self.ProjectName);
  forEachClient(
    procedure(aClient: TClient)
    var
      se: TClientSubscribable;
      preview: string;
		begin
      TMonitor.Enter(aClient.subscribedElements);
      try
        for se in aClient.subscribedElements do
         begin
           if se is TLayer then
           begin
             preview := (se as TLayer).previewBASE64;
             if preview<>'' then
             begin
               aClient.SendPreview(se.elementID, preview);
               Log.WriteLn('Send preview for '+se.elementID);
             end
             else Log.WriteLn('NO preview to send for '+se.elementID);
           end;
         end;
      finally
        TMonitor.Exit(aClient.subscribedElements);
      end;
    end);
end;

procedure TProject.SendRefresh;
begin
  Log.WriteLn('Sending refresh to clients connected to project '+Self.ProjectName);
  forEachClient(
    procedure(aClient: TClient)
    var
      se: TClientSubscribable;
      tiles: string;
    begin
      TMonitor.Enter(aClient.subscribedElements);
      try
        for se in aClient.subscribedElements do
        begin
          if se is TLayer then
          begin
            tiles := (se as TLayer).uniqueObjectsTilesLink;
            aClient.SendRefresh(se.elementID, '', tiles);
            Log.WriteLn('Send refresh for '+se.elementID+': '+tiles);
          end;
        end;
      finally
        TMonitor.Exit(aClient.subscribedElements);
      end;
    end);
end;

procedure TProject.SendString(const aString: string);
begin
  //Log.WriteLn('Sending string to clients connected to project '+Self.ProjectName);
  forEachClient(
    procedure(aClient: TClient)
    begin
      aClient.signalString(aString);
    end);
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
    // todo: check locking and use forEachClient
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
    // todo: check locking and use forEachClient
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
    // todo: check locking and use forEachClient
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
    // todo: check locking and use forEachClient
    for client in clients
    do client.SendTimeSlider;
  end;
end;

procedure TProject.timerTilerStatusAsHeartbeat(aTimer: TTimer);
begin
  // request status from tiler as a heartbeat (keep IIS isapi module a live)
  fTiler.getTilerStatus;
  Log.WriteLn('keep-a-live tiler status request');
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

{ TSVGPathLayerObject }

constructor TSVGPathLayerObject.Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aColor: TAlphaRGBPixel; aWeight,
  aOpacity: single; aFillColor: TAlphaRGBPixel; aFillOpacity: single);
begin
  inherited Create(aLayer, aID);
  fGeometry := aGeometry;
  fColor := aColor;
  fWeight := aWeight;
  fOpacity := aOpacity;
  fFillColor := aFillColor;
  fFillOpacity := aFillOpacity;
end;

function TSVGPathLayerObject.distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double;
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

//function TSVGPathLayerObject.Encode: TByteBuffer;
//begin
//
//end;

function TSVGPathLayerObject.getExtent: TWDExtent;
begin
  Result := TWDExtent.FromGeometry(fGeometry);
end;

function TSVGPathLayerObject.getJSON2D(const aType, aExtraJSON2DAttributes: string): string;
begin

end;

function TSVGPathLayerObject.getValidGeometry: Boolean;
begin
  Result := Assigned(fGeometry);
end;

function TSVGPathLayerObject.intersects(aGeometry: TWDGeometry): Boolean;
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

{ TLayerSwitch }

constructor TLayerSwitch.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; const aObjectTypes: string; aBasicLayer: Boolean);
begin
  fZoomLayers := TList<TLayerOnZoom>.Create;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, '', ltSwitch, True, Double.NaN, aBasicLayer);

end;

destructor TLayerSwitch.Destroy;
begin

  inherited;
  FreeAndNil(fZoomLayers);
end;

function TLayerSwitch.getJSON: string;
var
  _json: string;
  loz: TLayerOnZoom;
begin
  // layers:[{zoom:0,layer:{layer}}]
  _json := '';
  for loz in fZoomLayers do
  begin
    if _json<>''
    then _json := _json+',';
    _json := _json+'{"zoom":'+loz.zoomLevel.ToString+', "layer":{'+loz.layer.JSON+'}}';
  end;
  Result := inherited getJSON+',"layers":['+_json+']';
end;

{ TLayerOnZoom }

class function TLayerOnZoom.Create(aZoomLevel: Integer; aLayer: TLayer): TLayerOnZoom;
begin
  Result.zoomLevel := aZoomLevel;
  Result.layer := aLayer;
end;

{ TChart }

constructor TChart.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aChartType: string);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  fChartType := aChartType;
end;

function TChart.getJSON: string;
begin
  Result := inherited;
  if Result<>''
  then Result := Result+',';
  Result := Result+
    '"type":"'+fChartType+'",'+
    '"data":['+getJSONData+']';
end;

end.
