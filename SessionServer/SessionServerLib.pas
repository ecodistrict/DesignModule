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

	System.JSON, System.SysConst, System.Math,
  System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.SysUtils;

const
  WS2IMBEventName = 'USIdle.Sessions.WS2IMB';
  sepElementID = '$';
  PreviewImageWidth = 64; // default width/height of a minuture layer preview in pixels
  MaxDirectSendObjectCount = 300; // todo: tune, 200, 500.. ?

  colorBasicOutline = $3388FF or $FF000000;
  colorBasicFill = $B5C6DD or $FF000000;

type
  TDistanceLatLon = record
    m_per_deg_lat: Double;
    m_per_deg_lon: Double;

    class function Create(aLat1InDegrees, aLat2InDegrees: Double): TDistanceLatLon; overload; static;
    class function Create(aLatMidInRad: Double): TDistanceLatLon; overload; static;
    function distanceInMeters(aDeltaLat, aDeltaLon: Double): Double;
  end;


  TExtent = record
    xMin: Double;
    yMin: Double;
    xMax: Double;
    yMax: Double;
    class function Create: TExtent; static;
    class function FromGeometry(aGeometry: TWDGeometry): TExtent; static;
    procedure Init; overload;
    procedure Init(x, y: Double); overload;
    procedure Init(x, y, aWidth, aHeight: Double); overload;
    function IsEmpty: Boolean;
    function Expand(x, y: Double): Boolean; overload;
    function Expand(aExtent: TExtent): Boolean; overload;
    function Intersects(const aExtent: TExtent): Boolean;
    function Contains(x, y: Double): Boolean;
    function CenterX: Double;
    function CenterY: Double;
    //function Square: TExtent;
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

  TScenarioElement = class; // forward

  TSessionModel = class; // forward

  TClient = class
  constructor Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
  destructor Destroy; override;
  private
    fSubscribedElements: TObjectList<TScenarioElement>; // ref, use TMonitor to lock
    fProject: TProject; // ref
    fCurrentScenario: TScenario; // ref
    fRefScenario: TScenario; // ref
    // todo: diff
    //fDiffLayers: TObjectDictionary<TLayer, TLayer>; // owns
    //fDiffCharts: TObjectDictionary<TChart, TChart>; // owns
    //fDiffKPIs: TObjectDictionary<TKPI, TKPI>; // owns
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
    procedure UpdateSession();
    procedure SendTimeSlider();
    procedure SendSelectionEnabled();
    procedure SendMeasuresEnabled();
    procedure SendMeasuresHistoryEnabled();
  public
    property sessionModel: TSessionModel read getSessionModel;
    property subscribedElements: TObjectList<TScenarioElement> read fSubscribedElements; // ref, use TMonitor to lock
    property clientID: string read fClientID;

    procedure signalString(const aString: string);

    procedure SendRefresh(const aElementID, aTimeStamp, aObjectsTilesLink: string);
    procedure SendPreview(const aElementID, aPreviewBASE64: string);

    procedure HandleElementRemove(aElement: TScenarioElement);
    procedure HandleScenarioRemove(aScenario: TScenario);
  end;

  TScenarioElement = class
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean);
  destructor Destroy; override;
  protected
    fScenario: TScenario; // ref
    fDomain: string;
    fID: string;
    fName: string;
    fDescription: string;
    fDefaultLoad: Boolean;
    //
    fClients: TObjectList<TClient>; // refs, lock with TMonitor
    fOutputEvent: TEventEntry;
  protected
    function getJSON: string; virtual;
    function getElementID: string;
  public
    property scenario: TScenario read fScenario;
    property domain: string read fDomain;
    property ID: string read fID;
    property name: string read fName;
    property description: string read fDescription;
    property defaultLoad: Boolean read fDefaultLoad;
    //
    property clients: TObjectList<TClient> read fClients; // refs, lock with TMonitor
    property outputEvent: TEventEntry read fOutputEvent;
    //
    property JSON: string read getJSON;
    property elementID: string read getElementID;
  public
    procedure HandleClientSubscribe(aClient: TClient); virtual;
    procedure HandleClientUnsubscribe(aClient: TClient); virtual;
  end;

  TLayerObject = class
  constructor Create(aLayer: TLayer; const aID: TWDID);
  private
    fLayer: TLayer;
    fID: TWDID;
  protected
    function getGeoJSON2D(const aType: string): string; virtual; abstract;
    function getValidGeometry: Boolean; virtual;
    function getExtent: TExtent; virtual;
    function Encode: TByteBuffer; virtual;
  public
    property layer: TLayer read fLayer;
    property ID: TWDID read fID;
    property GeoJSON2D[const aType: string]: string read getGeoJSON2D;
    property ValidGeometry: Boolean read getValidGeometry;
    property Extent: TExtent read getExtent;
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
    function getExtent: TExtent; override;
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
    function getExtent: TExtent; override;
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
    function getExtent: TExtent; override;
    function Encode: TByteBuffer; override;
  public
    property geometry: TWDGeometry read fGeometry;
    property value: Double read fValue;
    function distance(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double): Double; override;
    function intersects(aGeometry: TWDGeometry): Boolean; override;
  end;

  TLayer = class(TScenarioElement)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; aTilerEvent: TEventEntry; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fPalette: TWDPalette; // owns
    fObjects: TObjectDictionary<TWDID, TLayerObject>; // owns
    fGeometryType: string;
    fObjectsLock: TOmniMREW;
    fBasicLayer: Boolean;
    fPreview: TPngImage;// TPicture;//TBitmap;
    fObjectTypes: string;
    fPreviewRequestTimer: TTimer; // ref
  protected
    fLegendJSON: string;
    fQuery: string;
    fObjectsTilesID: Integer;
    fObjectsTilesLink: string;
    fTilerEvent: TEventEntry;
    //useTiles: Boolean;
    function getJSON: string; override;
    function getObjectsJSON: string; virtual;
    function getPreviewBASE64: string; virtual;
    procedure handleOutputEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer); virtual;
    procedure signalObjects(aSender: TObject); virtual;
    procedure handleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    procedure signalTilerRequestPreview(aTimer: TTImer);
  public
    property palette: TWDPalette read fPalette;
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
    property objectsTilesID: Integer read fObjectsTilesID write fObjectsTilesID;
    property objectsTilesLink: string read fObjectsTilesLink write fObjectsTilesLink;
  public
    function FindObject(const aID: TWDID; out aObject: TLayerObject): Boolean;
    procedure AddObject(aObject: TLayerObject);
    procedure AddOrSetObject(aObject: TLayerObject);
    procedure RemoveObject(aObject: TLayerObject);
    function ExtractObject(aObject: TLayerObject): TLayerObject;
    procedure ClearObjects;
    function findNearestObject(const aDistanceLatLon: TDistanceLatLon; aX, aY: Double; var aDistance: Double): TLayerObject;
    function findObjectsInCircle(const aDistanceLatLon: TDistanceLatLon; aX, aY, aRadius: Double; var aObjectsJSON: string): Integer;
    function findObjectsInGeometry(const aGeometryExtent: TExtent; aGeometry: TWDGeometry; var aObjectsJSON: string): Integer;
  public
    procedure ReadObjectsDBSVGPaths(aQuery: TDataSet; aDefaultValue: Double);
  public
    procedure RegisterOnTiler(aPersistent: Boolean; aSliceType: Integer; const aDescription: string; aEdgeLengthInMeters: Double=NaN);
    procedure RegisterLayer; virtual;
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
  end;

  TScenario = class
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
    fClients: TObjectList<TClient>; // refs, lock with TMonitor
    fAddbasicLayers: Boolean;
    function getElementID: string;
    function selectLayersOnCategories(const aSelectedCategories: TArray<string>; aLayers: TList<TLayer>): Boolean;
  public
    property Project: TProject read fProject;
    property ID: string read fID;
    property name: string read fName write fName;
    property description: string read fDescription write fDescription;
    property mapView: TMapView read fMapView write fMapView;
    property elementID: string read getElementID;
    property Layers: TObjectDictionary<string, TLayer> read fLayers;
    property KPIs: TObjectDictionary<string, TKPI> read fKPIs;
    property Charts: TObjectDictionary<string, TChart> read fCharts;
    property clients: TObjectList<TClient> read fClients; // refs, lock with TMonitor
    property addBasicLayers: Boolean read fAddBasicLayers;
    procedure ReadBasicData(); virtual;
  public
    function AddLayer(aLayer: TLayer): TLayer;
    function AddKPI(aKPI: TKPI): TKPI;
    function AddChart(aChart: TChart): TChart;
  public
    procedure HandleClientSubscribe(aClient: TClient); virtual;
    procedure HandleClientUnsubscribe(aClient: TClient); virtual;
  public
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; virtual;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string; overload; virtual;
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
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerEventName: string;
    aDBConnection: TCustomConnection; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aAddBasicLayers: Boolean);
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
    fTilerEvent: TEventEntry;
    fDBConnection: TCustomConnection; // owns
    fProjectEvent: TEventEntry;
    fClients: TObjectList<TClient>; // owns, lock with TMonitor
    fTimers: TTimerPool;
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
    fAddBasicLayers: Boolean;
    procedure setTimeSlider(aValue: Integer);
    procedure setSelectionEnabled(aValue: Boolean);
    procedure setMeasuresEnabled(aValue: Boolean);
    procedure setMeasuresHistoryEnabled(aValue: Boolean);
    function getMeasuresJSON: string; virtual;
    function ReadScenario(const aID: string): TScenario; virtual;
  public
    function AddClient(const aClientID: string): TClient;
    procedure ReadBasicData(); virtual; abstract;
  public
    property Connection: TConnection read fConnection;
    property ProjectID: string read fProjectID;
    property ProjectName: string read fProjectName write setProjectName;
    property ProjectDescription: string read fProjectDescription write setProjectDescription;
    property ProjectEvent: TEventEntry read fProjectEvent;
    property TilerEvent: TEventEntry read fTilerEvent;
    property Timers: TTimerPool read fTimers;
    property scenarios: TObjectDictionary<string, TScenario> read fScenarios; // owns, lock with TMonitor
    property clients: TObjectList<TClient> read fClients; // owns, lock with TMonitor

    property measuresJSON: string read getMeasuresJSON;
    property measures: TObjectDictionary<string, TMeasure> read fMeasures;

    property timeSlider: Integer read fTimeSlider write setTimeSlider;
    property selectionEnabled: Boolean read fSelectionEnabled write setSelectionEnabled;
    property measuresEnabled: Boolean read fMeasuresEnabled write setMeasuresEnabled;
    property measuresHistoryEnabled: Boolean read fMeasuresHistoryEnabled write setMeasuresHistoryEnabled;
    property addBasicLayers: Boolean read fAddBasicLayers;
  public
    procedure SendRefresh();
    procedure SendPreview();
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


procedure jsonAdd(var aCurrent: string; const aAdd: string);
function jsonArrayOfDoubleToStr(const aValues: TArray<double>): string;
function geoJsonFeatureCollection(const aFeatures: string): string;
function geoJsonFeature(const aGeometry: string; const aProperties: string=''): string;

function ZoomLevelFromDeltaLon(aDeltaLon: Double): Integer;

implementation

{ utils }

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

function URITimeStamp: string;
begin
  Result := NowUTCInt64.ToString();
end;

function ZoomLevelFromDeltaLon(aDeltaLon: Double): Integer;
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

{ TExtent }

//function TExtent.AsStringInt: string;
//begin
//  Result := IntToStr(Round(xMin))+' x '+IntToStr(Round(yMin))+' - '+IntToStr(Round(xMax))+' x '+IntToStr(Round(yMax))
//end;

function TExtent.CenterX: Double;
begin
  Result := (xMin+xMax)/2;
end;

function TExtent.CenterY: Double;
begin
  Result := (yMin+yMax)/2;
end;

function TExtent.Contains(x, y: Double): Boolean;
begin
  if not IsEmpty
  then Result :=
        (x >= Self.xMin) and
        (x <= Self.xMax) and
        (y >= Self.yMin) and
        (y <= Self.xMax)
  else Result := False;
end;

class function TExtent.Create: TExtent;
begin
  Result.Init;
end;

procedure TExtent.Init;
begin
  xMin := NaN;
  yMin := NaN;
  xMax := NaN;
  yMax := NaN;
end;

function TExtent.Expand(aExtent: TExtent): Boolean;
begin
  if not aExtent.IsEmpty then
  begin
    Result := Expand(aExtent.XMin, aExtent.YMin);
    if Expand(aExtent.XMax, aExtent.YMax)
    then Result := True;
  end
  else Result := False;
end;

function TExtent.Expand(x, y: Double): Boolean;
begin
  Result := False;
  if isEmpty then
  begin
    if not (IsNaN(x) and IsNan(y)) then
    begin
      Init(x, y);
      Result := True;
    end;
  end
  else
  begin
    if xMin>x then
    begin
      xMin := x;
      Result := True;
    end;
    if xMax<x then
    begin
      xMax := x;
      Result := True;
    end;
    if yMin>y then
    begin
      yMin := y;
      Result := True;
    end;
    if yMax<y then
    begin
      yMax := y;
      Result := True;
    end;
  end;
end;

class function TExtent.FromGeometry(aGeometry: TWDGeometry): TExtent;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  Result.Init;
  for part in aGeometry.parts do
  begin
    for point in part.points do
    begin
      if Result.IsEmpty
      then Result.Init(point.X, point.Y)
      else Result.Expand(point.X, point.Y);
    end;
  end;
end;

procedure TExtent.Init(x, y, aWidth, aHeight: Double);
begin
  xMin := x;
  xMax := xMin+aWidth;
  yMin := y;
  yMax := y+aHeight;
end;

procedure TExtent.Init(x, y: Double);
begin
  xMin := x;
  xMax := x;
  yMin := y;
  yMax := y;
end;

function TExtent.Intersects(const aExtent: TExtent): Boolean;
begin
  if not (Self.IsEmpty or aExtent.IsEmpty)
  then Result :=
         (Self.XMin <= aExtent.XMax) and
         (Self.XMax >= aExtent.XMin) and
         (Self.YMin <= aExtent.YMax) and
         (Self.YMax >= aExtent.YMin)
  else Result := False;
end;

function TExtent.IsEmpty: Boolean;
begin
  Result := IsNaN(xMin);
end;

{
function TExtent.Square: TExtent;
var
  d: TDistanceLatLon;
  w, h: Double;
begin
  // make square in meters (relative from center of extent) containing the original extent
  d := TDistanceLatLon.Create(YMin, YMax);
  w := d.m_per_deg_lon*(XMax-XMin);
  h := d.m_per_deg_lat*(YMax-YMin);
  if Abs(w)>=Abs(h) then
  begin
    Result.XMin := XMin;
    Result.XMax := XMax;
    h := w/d.m_per_deg_lat;
    Result.YMin := ((YMin+YMax)/2)-(h/2);
    Result.YMax := ((YMin+YMax)/2)+(h/2);
  end
  else
  begin
    Result.YMin := YMin;
    Result.YMax := YMax;
    w := h/d.m_per_deg_lat;
    Result.XMin := ((XMin+XMax)/2)-(w/2);
    Result.XMax := ((XMin+XMax)/2)+(w/2);
  end;
end;
}

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

constructor TClient.Create(aProject: TProject; aCurrentScenario, aRefScenario: TScenario; const aClientID: string);
begin
  inherited Create;
  fSubscribedElements := TObjectList<TScenarioElement>.Create(False);
  fProject := aProject;
  fClientID := aClientID;
  fCurrentScenario := aCurrentScenario;
  fRefScenario := aRefScenario;
  if Assigned(fCurrentScenario)
  then fCurrentScenario.HandleClientSubscribe(Self);
  if Assigned(fRefScenario) then
  begin
    fRefScenario.HandleClientSubscribe(Self);
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

      procedure addClient(aElement: TScenarioElement);
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

      procedure removeClient(aElement: TScenarioElement);
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

      procedure selectObjects(aSelectObjects: TJSONValue);
      var
        t: string;
        m: string;
        g: TJSONObject;
        q: string;
        rStr: string;
        r: Double;
        x, y: Double;
        geometry: TWDGeometry;
        //partI: Integer;
        part: TWDGeometryPart;
        c, c2: TJSONArray;
        pointI: Integer;
        sca: TJSONArray;
        sc: TArray<string>;
        i: Integer;
        resp: string;
      begin
        // todo: decode selection and send back objects
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
          // todo: select objects based on geometry
          // always polygon or point (radius)? -> simplyfy
          if c.Count=1 then
          begin
            // 1 part with coordinates
            c2 := c.Items[0] as TJSONArray;
            //TJSONArray(TJSONArray(TJSONArray(c.Items[0]).items[0])).items[0].tostring
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
          q := aSelectObjects.getValue<string>('query', '');
          // todo: select objects based on query
          if Assigned(fCurrentScenario) then
          begin
            resp := fCurrentScenario.SelectObjects(Self, t, m, sc, q);
          end;
        end;
        if resp<>'' then
        begin
          signalString(resp);
        end;
      end;

    var
      jsonObject: TJSONObject;
      jsonPair: TJSONPair;
      jsonValue: TJSONValue;
      scenarioID: string;
      userid: string;
      scenario: TScenario;
    begin
      try
        if Assigned(fProject) and Assigned(fClientEvent) then
        begin
          Log.WriteLn('message from client: '+event.eventName+': '+aString);
          // process message
          jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
          try
            jsonPair := jsonObject.Get('subscribe');
            if Assigned(jsonPair)
            then addClient(sessionModel.FindElement(jsonPair.JsonValue.Value))
            else
            begin
              jsonPair := jsonObject.Get('unsubscribe');
              if Assigned(jsonPair)
              then removeClient(sessionModel.FindElement(jsonPair.JsonValue.Value))
              else
              begin
                jsonPair := jsonObject.Get('selectObjects');
                if Assigned(jsonPair)
                then selectObjects(jsonPair.JsonValue)
                else
                begin
                  jsonPair := jsonObject.Get('login');
                  if Assigned(jsonPair) then
                  begin
                    scenarioID := (jsonPair.JsonValue as TJSONObject).GetValue<string>('scenario');
                    if scenarioID<>'' then
                    begin
                      userid := (jsonPair.JsonValue as TJSONObject).GetValue<string>('userid');
                      if fProject.scenarios.TryGetValue(scenarioID, scenario) then
                      begin
                        fCurrentScenario := scenario;
                        fCurrentScenario.HandleClientSubscribe(Self);
                        Log.WriteLn('connected to scenario '+scenarioID+' user '+userid);
                        // retry
                        SendSession();
                        //SendMeasures(); // todo:?
                        SendDomains('domains');

                      end
                      else Log.WriteLn('could not connect to scenario '+scenarioID+' user '+userid, llError);
                    end;
                  end
                  else
                  begin
                    jsonPair := jsonObject.Get('selectScenario');
                    if Assigned(jsonPair) then
                    begin
                      jsonValue := (jsonPair.JsonValue as TJSONObject).Values['currentScenario'];
                      if Assigned(jsonValue) then
                      begin
                        scenarioID := jsonValue.Value;
                        if not fProject.scenarios.TryGetValue(scenarioID, scenario)
                        then scenario := fProject.ReadScenario(scenarioID);
                        if Assigned(fCurrentScenario)
                        then fCurrentScenario.HandleClientUnsubscribe(Self);
                        fCurrentScenario := scenario;
                        if Assigned(fCurrentScenario)
                        then fCurrentScenario.HandleClientSubscribe(Self);
                      end;
                      jsonValue := (jsonPair.JsonValue as TJSONObject).Values['referenceScenario'];
                      if Assigned(jsonValue) then
                      begin
                        scenarioID := jsonValue.Value;
                        if not fProject.scenarios.TryGetValue(scenarioID, scenario)
                        then scenario := fProject.ReadScenario(scenarioID);
                        if Assigned(fRefScenario) then
                        begin
                          fRefScenario.HandleClientUnsubscribe(Self);
                          // todo: clear list of diff layers, charts and kpis
                        end;
                        fRefScenario := scenario;
                        if Assigned(fRefScenario) then
                        begin
                          fRefScenario.HandleClientSubscribe(Self);
                          // todo: create list diff layers, charts and kpis
                        end;
                      end;
                      SendDomains('updatedomains');
                      UpdateSession();
                    end;
                    // rest
                  end;
                end;
              end;
            end;
          finally
            jsonObject.Free;
          end;
        end;
      except
        on e: Exception
        do Log.WriteLn('exception in TClient.Create: fClientEvent.OnString: '+e.Message, llError);
      end;
    end);
  // trigger login
  signalString('{"login": {}}');
  // do default registration
  SendSession();
  SendMeasures();
  SendDomains('domains');
end;

destructor TClient.Destroy;
var
  se: TScenarioElement;
begin
  if Assigned(fClientEvent) then
  begin
    fClientEvent.signalIntString(actionDelete, '');
    fClientEvent.unSubscribe;
    fClientEvent.unPublish;
    fClientEvent := nil;
  end;
  // todo: free diff layers etc.
  if Assigned(fCurrentScenario)  then
  begin
    fCurrentScenario.HandleClientUnsubscribe(Self);
    fCurrentScenario := nil;
  end;
  if Assigned(fRefScenario)  then
  begin
    fRefScenario.HandleClientUnsubscribe(Self);
    fRefScenario := nil;
  end;
  TMonitor.Enter(fSubscribedElements);
  try
    for se in fSubscribedElements
    do se.HandleClientUnsubscribe(Self);
  finally
    TMonitor.Exit(fSubscribedElements);
  end;
  FreeAndNil(fSubscribedElements);
  inherited;
end;

function TClient.getSessionModel: TSessionModel;
begin
  Result := fProject.fSessionModel;
end;

procedure TClient.HandleElementRemove(aElement: TScenarioElement);
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

function compareLayerNames(const aLayer1, aLayer2: TLayer): Integer;
begin
  Result := AnsiCompareText(aLayer1.name, aLayer2.name);
end;

procedure TClient.SendDomains(const aPrefix: string);
var
  d: TClientDomain;
  domains: TDictionary<string, TClientDomain>;
  JSON: string;
  //nlp: TPair<string, TLayer>;
  layer: TLayer;
  ndp: TPair<string, TClientDomain>;
  domainsJSON: string;
  nkp: TPair<string, TKPI>;
  ngp: TPair<string, TChart>;
  locLayers: TList<TLayer>;
  refLayer: TLayer;
begin
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
          if Assigned(fRefScenario) and fRefScenario.Layers.TryGetValue(layer.ID, refLayer) then
          begin
            JSON := JSON+',{"ref":{'+refLayer.JSON+'}}';
            // todo: add diff layer
            //diffLayerID := 'diff|'+layer.elementID+'|'+refLayer.elementID;
            //JSON := JSON+',{"diff":{'+diffLayer.JSON+'}}';
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
      view+
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
  fClients := TObjectList<TClient>.Create(False); // refs
  fOutputEvent := fScenario.Project.Connection.publish(fScenario.Project.ProjectEvent.eventName+'.'+fScenario.ID+'.'+ID, false);
end;

destructor TScenarioElement.Destroy;
var
  client: TClient;
begin
  if Assigned(fOutputEvent) then
  begin
    fOutputEvent.unPublish;
    fOutputEvent := nil;
  end;
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

function TScenarioElement.getElementID: string;
begin
  Result := ID;
  if Assigned(scenario)
  then Result := fScenario.elementID+sepElementID+Result;
end;

function TScenarioElement.getJSON: string;
begin
  Result :=
    '"domain": "'+domain+'",'+
    '"id": "'+getElementID+'",'+
    '"name": "'+name+'",'+
    '"description": "'+description+'",'+
    '"default": '+Ord(defaultLoad).ToString;
end;

procedure TScenarioElement.HandleClientSubscribe(aClient: TClient);
begin
  TMonitor.Enter(fClients);
  try
    if fClients.IndexOf(aClient)<0
    then fClients.Add(aClient);
  finally
    TMonitor.Exit(fClients);
  end;
end;

procedure TScenarioElement.HandleClientUnsubscribe(aClient: TClient);
var
  i: Integer;
begin
  TMonitor.Enter(fClients);
  try
    i := fClients.IndexOf(aClient);
    if i>=0
    then fClients.Delete(i);
  finally
    TMonitor.Exit(fClients);
  end;
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

function TLayerObject.getExtent: TExtent;
begin
  Result := TExtent.Create;
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

function TGeometryPointLayerObject.getExtent: TExtent;
begin
  Result.Init(fGeometryPoint.x, fGeometryPoint.y);
end;

function TGeometryPointLayerObject.getGeoJSON2D(const aType: string): string;
var
  colors: TGeoColors;
begin
  if Assigned(fGeometryPoint) then
  begin
    if Assigned(fLayer.palette)
    then colors := fLayer.palette.ValueToColors(fValue)
    else colors := TGeoColors.Create($FF000000); // black
    Result := '{ '+
      '"type":"Feature",'+
      '"geometry":{'+
        '"type":"'+fLayer.GeometryType+'",'+
        '"coordinates":'+fGeometryPoint.GeoJSON2D[aType]+
      '},'+
      '"properties":{'+
        '"id":"'+string(ID)+'",'+
        '"color": "'+ColorToJSON(colors.mainColor)+'"'+
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

function TGeometryLayerPOIObject.getExtent: TExtent;
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

function TGeometryLayerObject.getExtent: TExtent;
begin
  Result := TExtent.FromGeometry(fGeometry);
end;

function TGeometryLayerObject.getGeoJSON2D(const aType: string): string;
var
  colors: TGeoColors;
begin
  if Assigned(fGeometry) then
  begin
    if Assigned(fLayer.palette)
    then colors := fLayer.palette.ValueToColors(fValue)
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
      '}'+
    '}'
  end
  else Result := '';
end;
{
function TGeometryLayerObject.getJSONColor: string;
begin
  if Assigned(fLayer.palette)
  then Result := fLayer.palette.ValueToColorJSON(fValue)
  else Result := '#000000'; // black, default
end;
}
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

constructor TLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
  const aObjectTypes, aGeometryType: string; aTilerEvent: TEventEntry; aBasicLayer: Boolean=False);
var
  previewFileName: string;
  previewsFolder: string;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad);
  fPalette := aPalette;
  fObjectTypes := aObjectTypes;
  fGeometryType := aGeometryType;
  fBasicLayer := aBasicLayer;
  fObjects := TObjectDictionary<TWDID, TLayerObject>.Create([doOwnsValues]);
  fObjectsLock.Create;
  fLegendJSON := '';
  fQuery := '';
  fObjectsTilesID := -1;
  fObjectsTilesLink := '';
  fTilerEvent := aTilerEvent;
  fTilerEvent.OnEvent.Add(HandleTilerEvent);
  fTilerEvent.subscribe;
  // check fro preview image
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
  fPreviewRequestTimer := scenario.Project.Timers.SetTimer(signalTilerRequestPreview);
  fOutputEvent.OnEvent.Add(handleOutputEvent);
  fOutputEvent.Subscribe;
end;

destructor TLayer.Destroy;
begin
  if Assigned(fTilerEvent) then
  begin
    fTilerEvent.OnEvent.Remove(handleTilerEvent);
    fTilerEvent := nil;
  end;
  if Assigned(fOutputEvent) then
  begin
    fOutputEvent.OnEvent.Remove(handleOutputEvent);
    fOutputEvent := nil;
  end;
  inherited;
  FreeAndNil(fClients);
  FreeAndNil(fObjects);
  FreeAndNil(fPalette);
  FreeAndNil(fPreview);
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

function TLayer.findObjectsInGeometry(const aGeometryExtent: TExtent; aGeometry: TWDGeometry; var aObjectsJSON: string): Integer;
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
begin
  aQuery.Open;
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
        AddObject(TGeometryLayerObject.Create(Self, _id, TWDGeometry.CreateFromSVGPath(_path), _value));
      end;
    except
      on e: Exception
      do Log.WriteLn(elementID+': exception in ReadObjectsDBSVGPaths: '+e.Message, llError);
    end;
    aQuery.Next;
  end;
end;

procedure TLayer.RegisterLayer;
begin
  // todo; default no action?
end;

procedure TLayer.RegisterOnTiler(aPersistent: Boolean; aSliceType: Integer; const aDescription: string; aEdgeLengthInMeters: Double);
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_string(icehEventName, fOutputEvent.eventName);
  if not IsNaN(aEdgeLengthInMeters)
  then payload := Payload+
    TByteBuffer.bb_tag_double(icehTilerEdgeLength, aEdgeLengthInMeters);
  payload := Payload+
    TByteBuffer.bb_tag_string(icehTilerLayerDescription, aDescription)+
    TByteBuffer.bb_tag_bool(icehTilerPersistent, aPersistent)+
    TByteBuffer.bb_tag_int32(icehTilerRequestNewLayer, aSliceType); // last to trigger new layer request
  fTilerEvent.signalEvent(payload);
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
      fOutputEvent.signalEvent(
        TByteBuffer.bb_tag_double(icehTilerSliceID, timeStamp)+
        TByteBuffer.bb_tag_rawbytestring(icehTilerSliceUpdate, buffer));
      buffer := '';
    end;
  end;

begin
  // todo: replace with batch buffer
  timeStamp := 0; // todo:
  // first all layer properties
  if Assigned(fPalette) then
  begin
    // create slice after sending all slice properties needed for creation
    fOutputEvent.signalEvent(
      TByteBuffer.bb_tag_rawbytestring(fPalette.wdTag, fPalette.Encode)+
      TByteBuffer.bb_tag_double(icehTilerSliceID, timeStamp));
  end;
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

procedure TLayer.signalTilerRequestPreview(aTimer: TTImer);
begin
  Log.WriteLn('triggered preview timer for '+elementID);
  fOutputEvent.signalEvent(TByteBuffer.bb_tag_uint32(icehTilerRequestPreviewImage, PreviewImageWidth));
end;

function TLayer.getJSON: string;
begin
  Result := inherited getJSON+','+
    '"basic":'+Ord(basicLayer).ToString+','+
    '"objectTypes":['+objectTypes+'],'+
    '"legend":{'+legendJSON+'},'+
    '"preview":"'+previewBASE64+'",'+
    '"tiles":"'+objectsTilesLink+'"';
  if objects.Count<=MaxDirectSendObjectCount
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
var
  ms: TMemoryStream;
begin
  if Assigned(fPreview) then
  begin
    ms := TMemoryStream.Create;
    try
      fPreview.SaveToStream(ms);
      ms.Position := 0;
      Result := 'data:image/png;base64,'+TIdEncoderMIME.EncodeStream(ms);
    finally
      ms.Free;
    end;
  end
  else Result := '';
end;

procedure TLayer.handleOutputEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  timeStamp: TDateTime;
  client: TClient;
  timeStampStr: string;
  stream: TStream;
  pvBASE64: string;
  previewsFolder: string;
begin
  try
    // todo:
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehTilerID shl 3) or wtVarInt:
          begin
            fObjectsTilesID := aBuffer.bb_read_int32(aCursor);
          end;
        (icehTilerURL shl 3) or wtLengthDelimited:
          begin
            fObjectsTilesLink := aBuffer.bb_read_string(aCursor);
            // todo: make uri unique with time stamp
            fObjectsTilesLink := fObjectsTilesLink+'&ts='+URITimeStamp;
            AddCommandToQueue(Self, Self.signalObjects);
            // todo: update client info
            // NO refresh to clients, slice not created yet!
            {
            // subscribed to layer
            TMonitor.Enter(clients);
            try
              for client in clients
              do client.SendRefresh(elementID, '', objectsTilesLink);
            finally
              TMonitor.Exit(clients);
            end;
            // clients in scenario to update definition
            TMonitor.Enter(fScenario.clients);
            try
              for client in fScenario.clients do
              begin
                if not clients.Contains(client)
                then client.SendRefresh(elementID, '', objectsTilesLink);
              end;
            finally
              TMonitor.Exit(fScenario.clients);
            end;
            }
          end;
        (icehTilerRefresh shl 3) or wt64Bit:
          begin
            timeStamp := aBuffer.bb_read_double(aCursor);
            if timeStamp<>0
            then timeStampStr := FormatDateTime('yyyy-mm-dd hh:mm', timeStamp)
            else timeStampStr := '';
            // todo: start refresh timer
            // signal refresh to layer client
            TMonitor.Enter(clients);
            try
              for client in clients
              do client.SendRefresh(elementID, timeStampStr, fObjectsTilesLink);
            finally
              TMonitor.Exit(clients);
            end;
            // signal refresh to scenario client
            TMonitor.Enter(fScenario.clients);
            try
              for client in fScenario.clients do
              begin
                if not clients.Contains(client)
                then client.SendRefresh(elementID, timeStampStr, fObjectsTilesLink);
              end;
            finally
              TMonitor.Exit(fScenario.clients);
            end;
            // refresh preview also
            //Log.WriteLn('set preview timer for '+elementID);
            fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneMinute/6);
          end;
        (icehTilerPreviewImage shl 3) or wtLengthDelimited:
          begin
            try
              stream := TBytesStream.Create(aBuffer.bb_read_tbytes(aCursor));
              try
                fPreview.Free;
                fPreview := TPngImage.Create;
                fPreview.LoadFromStream(stream);
              finally
                stream.Free;
              end;
              previewsFolder := ExtractFilePath(ParamStr(0))+'previews';
              ForceDirectories(previewsFolder);
              fPreview.SaveToFile(previewsFolder+'\'+elementID+'.png');
              pvBASE64 := previewBASE64;
              // layer clients
              TMonitor.Enter(clients);
              try
                for client in fClients
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
            except
              on E: Exception
              do Log.WriteLn('Exception TLayer.handleOutputEvent handling icehTilerPreviewImage: '+e.Message, llError);
            end;
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  except
    on e: Exception
    do  log.WriteLn('exception in TLayer.handleOutputEvent: '+e.Message, llError);
  end;
end;

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
        //signalEvent(TByteBuffer.bb_tag_double(icehTilerStartup, now));
        (icehTilerStartup shl 3) or wt64Bit:
          begin
            //fObjectsTilesID := aBuffer.bb_read_int32(aCursor);
            {tilerStartup := }aBuffer.bb_read_double(aCursor);
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

{ TKPI }

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
  fClients := TObjectList<TClient>.Create(False); // refs
  fAddbasicLayers := aAddbasicLayers;
  ReadBasicData;
end;

destructor TScenario.Destroy;
var
  client: TClient;
begin
  TMonitor.Enter(fClients);
  try
    for client in fClients
    do client.HandleScenarioRemove(Self);
  finally
    TMonitor.Exit(fClients);
  end;
  FreeAndNil(fClients);
  FreeAndNil(fLayers);
  FreeAndNil(fKPIs);
  FreeAndNil(fCharts);
  inherited;
end;

function TScenario.getElementID: string;
begin
  Result := ID;
  if Assigned(fProject)
  then Result := fProject.fProjectID+sepElementID+Result;
end;

procedure TScenario.HandleClientSubscribe(aClient: TClient);
begin
  TMonitor.Enter(fClients);
  try
    if fClients.IndexOf(aClient)<0
    then fClients.Add(aClient);
  finally
    TMonitor.Exit(fClients);
  end;
end;

procedure TScenario.HandleClientUnsubscribe(aClient: TClient);
var
  i: Integer;
begin
  TMonitor.Enter(fClients);
  try
    i := fClients.IndexOf(aClient);
    if i>=0
    then fClients.Delete(i);
  finally
    TMonitor.Exit(fClients);
  end;
end;

procedure TScenario.ReadBasicData;
begin
  // default no action
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
  const aProjectID, aProjectName, aTilerEventName: string; aDBConnection: TCustomConnection;
  aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aAddBasicLayers: Boolean{; aSourceEPSG: Integer});
begin
  inherited  Create;
  fTimeSlider := aTimeSlider;
  fSelectionEnabled := aSelectionEnabled;
  fMeasuresEnabled := aMeasuresEnabled;
  fMeasuresHistoryEnabled := aMeasuresHistoryEnabled;
  fAddBasicLayers := aAddbasicLayers;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fClients := TObjectList<TClient>.Create;
  fScenarios := TObjectDictionary<string, TScenario>.Create([doOwnsValues]);
  fScenarioLinks := TScenarioLink.Create(-1, -1, -1, '', '', '', nil);
  fTimers := TTimerPool.Create;
  fProjectID := aProjectID;
  fProjectName := aProjectName;
  fProjectDescription := ''; // default no description set, set by property..
  fTilerEvent := aConnection.publish(aTilerEventName, False);
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
end;

destructor TProject.Destroy;
begin
  FreeAndNil(fTimers);
  FreeAndNil(fClients);
  FreeAndNil(fScenarios);
  FreeAndNil(fMeasures);
  inherited;
  if Assigned(fTilerEvent) then
  begin
    fTilerEvent.unPublish;
    fTilerEvent := nil;
  end;
  FreeAndNil(fDBConnection);
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

function TProject.ReadScenario(const aID: string): TScenario;
begin
  Result := nil;
end;

procedure TProject.SendPreview;
var
  client: TClient;
  se: TScenarioElement;
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
  se: TScenarioElement;
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
            client.SendRefresh(se.elementID, '', (se as TLayer).fObjectsTilesLink);
            Log.WriteLn('Send refresh for '+se.elementID+': '+(se as TLayer).fObjectsTilesLink);
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
    for client in fClients
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
    for client in fClients
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
    for client in fClients
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
    for client in fClients
    do client.SendTimeSlider;
  end;
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

end.
