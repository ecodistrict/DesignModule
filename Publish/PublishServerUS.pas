unit PublishServerUS;

// todo: handling changeobject event for chart will trigger multiple charts?
// todo: .. and ref chart is not taken into account

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
  USCopyLib,

  PublishServerLib,
  PublishServerGIS,
  PublishServerOra,
  PublishServerMCLib,

  Vcl.graphics, // TPicture

  GisTypes,
  GisCsSystems,

  WinApi.Windows,
  System.SyncObjs,
  System.JSON,
  System.Math,
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,
  System.RegularExpressions,
  System.Generics.Collections;


const
  PUBLISHINGSERVER_TABLE_PREFIX = 'PBLS';
  PROJECT_TABLE_NAME = PUBLISHINGSERVER_TABLE_PREFIX+'_PROJECT';

  MEASURES_TABLE_NAME = 'META_MEASURES';

  tagEnable = 'Enable';
  tagDisable = 'Disable';
  tagProperties = 'Properties';
  tagSelectObjects = 'SelectObjects';
  tagRemove = 'Remove';

type
  TUSLayer = class; // forward
  TUSScenario = class; //forward

  TIMBEventEntryArray = array of TIMBEventEntry;
  TStringArray = array of string;

  TMetaLayerEntry = record
    valid: Boolean;
    // fields
    OBJECT_ID: Integer;
    LAYER_TYPE: Integer;
    LAYER_TABLE: string;
    LEGEND_FILE: string;
    LEGEND_DESC: string;
    VALUE_EXPR: string;
    VALUE_NODATA: Double;
    JOINCONDITION: string;
    TEXTURE_FILE: string;
    TEXTURE_EXPR: string;
    ROW_START: Integer;
    ROW_SIZE: Integer;
    COL_START: Integer;
    COL_SIZE: Integer;
    ROW_FIELD: string;
    COL_FIELD: string;
    IS_CELL_BASED: Boolean;
    MXR: Double;
    MXC: Double;
    MXT: Double;
    MYR: Double;
    MYC: Double;
    MYT: Double;
    IMB_EVENTCLASS: string;
    // added for web interface
    domain: string;
    description: string;
    diffRange: Double;
    objectType: string;
    geometryType: string;
    _published: Integer;
    // indirect
    legendAVL: string;
    odbList: TODBList;
  public
    procedure ReadFromQueryRow(aQuery: TOraQuery);
    function BaseTable(const aTablePrefix:string): string;
    function BaseTableNoPrefix: string;
    function BuildJoin(const aTablePrefix: string; out aShapePrefix, aObjectIDPrefix, aPreJoin: string): string;
    function SQLQuery(const aTablePrefix:string; xMin: Integer=0; yMin: Integer=0; xMax: Integer=-1; yMax: Integer=-1): string;
    function SQLQueryNew(const aTablePrefix:string): string;
    function SQLQueryChangeMultiple(const aTablePrefix:string): string;
    function autoDiffRange: Double;
    function BuildLegendJSON(aLegendFormat: TLegendFormat): string;
    function CreateUSLayer(aScenario: TScenario; const aTablePrefix: string; const aConnectString: string;
      const aDataEvent: array of TIMBEventEntry; aSourceProjection: TGIS_CSProjectedCoordinateSystem; const aDomain, aName: string;
      aOpacity: Double; aDefaultLoad: Boolean): TLayerBase;
  end;

  TMetaLayer = TDictionary<Integer, TMetaLayerEntry>;

  // extra fields in meta_scenarios
    // published, null,0,1,2
  // extra fields in meta_layer
  	// domain, string
    // published, null,0,1,2,
    // diffRange, double
    // title, string, null, ".."
    // description, string, null, ".."

  TMetaObjectsEntry = record
    OBJECT_ID: Integer;
    TABLE_NAME: string;
    LAYER_DESCRIPTION: string;
    SELECT_PROPERTY: string;
    PROPERTY_DESCRIPTION: string;
    PROPERTY_CATEGORY: string;
    OBJECT_TYPE: string;
    GEOMETRY_TYPE: string;
    EDITABLE: Integer;
    _published: Integer;
  public
    procedure ReadFromQueryRow(aQuery: TOraQuery);
  end;

  TMetaObjectEntry = record
    OBJECT_ID: Integer;
    TABLE_NAME: string;
    LAYER_DESCRIPTION: string;
    OBJECT_TYPE: string;
    GEOMETRY_TYPE: string;
    TABLE_FILTER: string;
    _published: Integer;
  public
    procedure ReadFromQueryRow(aQuery: TOraQuery);
  end;

  //pfCount if used as point where the aggregated functions start. Dus aggregated >= pfCount
  TPropertyFunction = (pfNone, pfAbs, pfCeil, pfFloor, pfCount, pfAvg, pfMax, pfMin, pfSum);

  TMetaObjectPropertyEntry = record
    OBJECT_ID: Integer;
    META_OBJECT_ID: Integer;
    PROPERTY_NAME: string;
    PROPERTY_FUNCTION: TPropertyFunction;
    PROPERTY_CATEGORY: string;
    PROPERTY_DESCRIPTION: string;
    JOIN_TABLE: string;
    BASE_TABLE_ID: string;
    JOIN_TABLE_ID: string;
    SIDE: Integer;
    EDITABLE: Integer;
    ORDERING: Integer;
    _published: Integer;
  public
    procedure ReadFromQueryRow(aQuery: TOraQuery);
  end;

  TUSObjectPropTable = class; // forward
  TUSObjectProp = class; //forward
  TUSBasicLayer = class; //forward

  TUSObjectProperties = class
  constructor Create(const aBaseTableName: string; aBasicLayer: TUSBasicLayer);
  destructor Destroy; override;
  private
    fTables: TObjectDictionary<string, TUSObjectPropTable>;
    fBaseTableName: string;
    fBasicLayer: TUSBasicLayer;
  public
    procedure AddFromObjectPropertyEntry(const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry);
  public
    property BasicLayer: TUSBasicLayer read fBasicLayer;
    property BaseTableName: string read fBaseTableName;
    property Tables: TObjectDictionary<string, TUSObjectPropTable> read fTables;
    function TryGetPropertyTable(const aPropertyName: string; out aObjectPropTable: TUSObjectPropTable): Boolean;
  end;

  TUSObjectPropTable = class
  constructor Create(const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry; aPropertiesBase: TUSObjectProperties);
  destructor Destroy; override;
  private
    fPropertiesBase: TUSObjectProperties;
    fProperties: TObjectDictionary<string, TUSObjectProp>;
  private
    fJoinTableName: string;
    fBaseID: string;
    fJoinID: string;
  public
    procedure AddFromObjectPropertyEntry(const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry);
  public
    property JoinTableName: string read fJoinTableName;
    property BaseID: string read fBaseID;
    property JoinID: string read fJoinID;
    property Properties: TObjectDictionary<string, TUSObjectProp> read fProperties;
    property PropertiesBase: TUSObjectProperties read fPropertiesBase;
    function ContainsProperty(const aPropertyName: string): Boolean;
  end;

  TUSObjectProp = class
  constructor Create(const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry; aPropertyTable: TUSObjectPropTable);
  destructor Destroy; override;
  private
    fPropertyTable: TUSObjectPropTable;
    fSidedProperties: TDictionary<Integer, string>;
  private
    fDescription: string;
    fCategory: string;
    fPropertyFunction: TPropertyFunction;
    fEditable: Boolean;
    fOrdering: Integer;
  public
    procedure AddFromObjectPropertyEntry(const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry);
    property PropertyTable: TUSObjectPropTable read fPropertyTable;
    property SidedProperties: TDictionary<Integer, string> read fSidedProperties;
  public
    property Description: string read fDescription;
    property Category: string read fCategory;
    property PropertyFunction: TPropertyFunction read fPropertyFunction;
    property Editable: Boolean read fEditable;
    property Ordering: Integer read fOrdering;
    function PropertyName(out aPropertyName: string; aSide: Integer=0): Boolean;
    function ContainsSide(aSide: Integer): Boolean;
  end;

  TSelectProperty = record
    ID: Integer;
    SelectProperty: string;
    PropertyDescription: string;
    Editable: Integer;
  public
    function ReadFromMetaObjectsEntry(const aMetaObjectsEntry: TMetaObjectsEntry): Boolean; //returns true if property not null
  end;

  TSelectProperties = array of TSelectProperty;
  TMetaObjectEntries = TList<TMetaObjectEntry>;
  TMetaObjectProperties = TList<TMetaObjectPropertyEntry>;

  TUSSelectedObject = class
  constructor Create(aObjectID: string; aSide: Integer = 0);
  destructor Destroy; override;
  private
    fObjectID: string;
    fSides: TList<Integer>;
  public
    property ObjectID: string read fObjectID;
    property Sides: TList<Integer> read fSides;
  end;

  //helper class for reading properties from the database
  //flow is Initialization (value set to default) -> Reading database (thread safe value changes) -> building property JSON(value will not change)
  TUSBuilderProp = class
  constructor Create(aObjectProp: TUSObjectProp);
  destructor Destroy; override;
  private
    fObjectProp: TUSObjectProp; //doesn't own
    fDataType: string;
    fValue: string;
    fAddValueLock: TOmniMREW; //locks value changes during db access
  private
    function LockedAddValue(const aValue: string; const aDataType: TFieldType; const aCount: Integer): Boolean; virtual; abstract;
  public
    function AddValue(const aValue: string; const aDataType: TFieldType; const aCount: Integer=0): Boolean; //returns true if done
    function getJSON: string; virtual;
    function Open: Boolean; virtual; abstract;
    function SQLProperties(aTableAlias: string): string; virtual; abstract;
    function SidedSQLProperty(const aTableAlias: string; const aSide: Integer): string; virtual; abstract;
  public
    function Description: string;
    function Editable: Boolean;
    function Ordering: Integer;
    function Category: string;
    function PropertyName(out aPropertyName: string; aSide: Integer=0): Boolean;
    property DataType: string read fDataType;
    property Value: string read fValue;
  end;

  TUSBuilderPropNone = class(TUSBuilderProp)
  constructor Create(aObjectProp: TUSObjectProp);
  destructor Destroy; override;
  private
    fDistinctCount: Integer;
  private
    function LockedAddValue(const aValue: string; const aDataType: TFieldType; const aCount: Integer): Boolean; override;
  public
    function Open: Boolean; override;
    function SQLProperties(aTableAlias: string): string; override;
    function SidedSQLProperty(const aTableAlias: string; const aSide: Integer): string; override;
  end;

  TUSBuilderPropSum = class (TUSBuilderProp)
  constructor Create(aObjectProp: TUSObjectProp);
  destructor Destroy; override;
  private
    fCount: Integer;
  private
    function LockedAddValue(const aValue: string; const aDataType: TFieldType; const aCount: Integer): Boolean; override;
  public
    function Open: Boolean; override;
    function SQLProperties(aTableAlias: string): string; override;
  end;

  //TODO: implement Builderprops for all the functions!

  TUSBuilderTable = class
  constructor Create(aObjectTable: TUSObjectPropTable);
  destructor Destroy; override;
  private
    fObjectTable : TUSObjectPropTable;
    fNormProps: TObjectList<TUSBuilderProp>;
    fAggrProps: TObjectList<TUSBuilderProp>; //aggregated properties like sum/min/etc -> need to be done in seperate query!
    fOpenAggrProps: Integer;
    fOpenNormProps: Integer;
  public
    function QueryDB(aOraSession: TOraSession; const aObjectIds: string; aSelectedObjects: TObjectDictionary<string, TUSSelectedObject>): Boolean;
    function QueryNormProps(aOraSession: TOraSession; const aObjectIds: string; aSelectedObjects: TObjectDictionary<string, TUSSelectedObject>): Boolean;
    function QueryAggrProps(aOraSession: TOraSession; const aObjectIds: string; aSelectedObjects: TObjectDictionary<string, TUSSelectedObject>): Boolean;
  public
    property NormProps: TObjectList<TUSBuilderProp> read fNormProps;
    property AggrProps: TObjectList<TUSBuilderProp> read fAggrProps;
  end;

  TUSPropBuilder = class
  constructor Create(aObjectProperties: TUSObjectProperties);
  destructor Destory;
  private
    fBuilderTables: TObjectList<TUSBuilderTable>;
    fObjectProperties: TUSObjectProperties;
  public
    procedure BuildProperties(aOraSession: TOraSession; aSelectedIds: TArray<string>);
    function GetJSON: string;
  end;

  TUSCommitProp = class
  constructor Create(aObjectProp: TUSObjectProp);
  destructor Destroy; override;
  private
    fObjectProp: TUSObjectProp;
    fStringValue, fDataType: string;
    fValue: Variant;
  public
    property StringValue: string read fStringValue;
    property DataType: string read fDataType;
    property Value: Variant read fValue;
    property ObjectProp: TUSObjectProp read fObjectProp;
    function AddValue(aValue, aDataType: string): Boolean;
  end;

  TUSCommitTable = class
  constructor Create(aObjectTable: TUSObjectPropTable);
  destructor Destroy; override;
  private
    fObjectTable: TUSObjectPropTable;
    fCommitProperties: TObjectDictionary<string, TUSCommitProp>;
  public
    function AddCommitProperty(aJSONProperty: TJSONValue): Boolean; //checks if the property is valid and editable, adds and returns true if so
    function CommitChangesToDB(aOraSession: TOraSession; aUSIMBConnection: TIMBConnection; aSelectedIds: TObjectDictionary<Integer, TList<string>>): Boolean;
  end;

  TUSCommitBuilder = class
  constructor Create(aObjectProperties: TUSObjectProperties; aJSONProperties: TJSONArray);
  destructor Destroy; override;
  private
    fCommitTables: TObjectDictionary<string, TUSCommitTable>;
    fObjectProperties: TUSObjectProperties;
  public
    function CommitChangesToDB(aOraSession: TOraSession; aUSIMBConnection: TIMBConnection; aSelectedObjects: TJSONArray; out aChangedCount: Integer): Boolean;
    property ObjectProperties: TUSObjectProperties read fObjectProperties;
  end;

  TUSCommitProperty = class
  constructor Create(aSelectProperty: TSelectProperty);
  destructor Destroy; override;
  private
    fName: string;
    fValue: Variant;
    fStringValue: string;
    fDataType: string;
  public
    function AddValue(const aValue, aType: string): Boolean;
    property ValueAsString: string read fStringValue;
  end;

  TUSCommitPropertyBuilder = class
  constructor Create(aTableName: string; aSelectProperties: TSelectProperties);
  destructor Destroy; override;
  private
    fTableName: string;
    fProperties: TDictionary<string, TSelectProperty>;
    fChangedProperties: TObjectDictionary<string, TUSCommitProperty>;
  public
    procedure ParseJSONProperties(aJSONProperties: TJSONArray);
    function CommitChangesToDB(aLayer: TUSBasicLayer; aOraSession: TOraSession; aUSIMBConnection: TIMBConnection; aSelectedObjects: TJSONArray; aBasicLayer: TUSBasicLayer; out aChangedCount: Integer): Boolean;
  end;

  TUSRoadIC = class(TGeometryLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue, aTexture: Double);
  protected
    fTexture: Double;
  public
    function Encode: TByteBuffer; override;
  public
    property texture: Double read fTexture;
  end;

  TUSRoadICLR = class(TGeometryLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue, aValue2, aTexture, aTexture2: Double);
  protected
    fValue2: Double;
    fTexture: Double;
    fTexture2: Double;
  public
    function Encode: TByteBuffer; override;
  public
    property value2: Double read fValue2 write fValue2;
    property texture: Double read fTexture write fTexture;
    property texture2: Double read fTexture2 write fTexture2;
  end;

  TUSPOI = class
  constructor Create(aID: Integer; aPicture: TPicture);
  destructor Destroy; override;
  protected
    fID: Integer;
    fPicture: TPicture; // owns
  public
    property ID: Integer read fID;
    property picture: TPicture read fPicture;
    // todo: encode ?
  end;

  TUSUpdateQueueEntry = record
  class function Create(aObjectID, aAction: Integer): TUSUpdateQueueEntry; static;
  public
    objectID: Integer;
    action: Integer;
  end;

  TUSChartValue = class
  constructor Create(aValue: string);
  protected
    fStringValue: string;
    fNumValue: Double;
    fNumber: Boolean;
  public
    function GetJSON: string;
  end;

  TUSChartSeries = class
  constructor Create(aLines: TDictionary<string, string>; const aPrefix: string; const aID: Integer; const aSeriesID, aChartTitle: string);
  constructor CreateFromChild(aTitle, aXCol, aYCol, aType: string);
  destructor Destroy; override;
  private
    function GetColumnJSON: string;
  protected
    fTitle, fXCol, fYCol, fType, fMultiBar, fStackGroup, fVertAxis: string;
    fID: Integer;
    fActive: Boolean;
    fYValues: TObjectList<TUSChartValue>;
    procedure AddXValues(aValues: array of string);
  public
    property XCol: string read fXCol;
    property YCol: string read fYCol;
    property MultiBar: string read fMultiBar;
    property StackGroup: string read fStackGroup;
    property VertAxis: string read fVertAxis;
    property Active: Boolean read fActive;
    property Title: string read fTitle;
    procedure FillData(aData: TDictionary<string, TStringList>);
    procedure FillFilteredData(aData: TDictionary<string, TStringList>; aFilterIndex: Integer);
  end;

  TUSChartGroup = class; //forward

  TUSChart = class(TChart)
  constructor Create(aChartGroup: TUSChartGroup; aScenario: TScenario; aLines: TDictionary<string, string>; aPrefix, aGroup, aTitle, aTableName: string);
  constructor CreateChild(aChartGroup: TUSChartGroup; aScenario: TScenario; aID, aTitle, aXCol, aGroup, aXColFilter: string; aSeries: TDictionary<string, string>);
  destructor Destroy; override;
  private
    fChartGroup: TUSChartGroup;
    fTitle, fGroup, fJSON: string;
    fSeries: TDictionary<string, TUSChartSeries>;
    fGroups: TDictionary<string, TList<string>>;
    fDoubleAxes, fChanged: Boolean;
    fClickable, fXColFilter: string;
    fXValues: TDictionary<string, TObjectList<TUSChartValue>>;
    fChildSeries: TDictionary<string, string>;
    function getJSONColumns: string;
    function getJSONXS: string;
    function getJSONX: string;
    function getJSONGroups: string;
    function getJSONTypes: string;
    function getJSONAxes: string;
    function getJSONAxis: string;
  public
    function getJSON: string; override;
    function getJSONData: string; override;
    procedure FillData(aData: TDictionary<string, TStringList>);
    property Series: TDictionary<string, TUSChartSeries> read fSeries;
  public
    procedure AddColumnsToStringList(aStringList: TStringList);
  public
    procedure HandleGraphLabelClick(aClient: TClient; aLabelTitle: string); override;
  end;

  TUSChartGroup = class
  constructor Create(const aScenario: TUSScenario; const aDefTableName, aDatTableName: string);
  destructor Destroy; override;
  private
  fLatestData: TObjectDictionary<string, TStringList>; //owns values, locks with TMonitor.
  fScenario: TUSScenario;
  fDefTableName, fDatTableName: string;
  fCharts: TList<TUSChart>; //only holds ref to group charts
  fDataEvent: TIMBEventEntry;
  fUpdateTimer: TTimer;
  fLastUpdate: THighResTicks;
  protected
    procedure ReadDataFromDB;
  public
    procedure HandleChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
    procedure HandleDataUpdate(aTimer: TTimer; aTime: THighResTicks);
    property LatestData: TObjectDictionary<string, TStringList> read fLatestData;
    property Charts: TList<TUSChart> read fCharts;
    property DefTableName: string read fDefTableName;
    property DatTableName: string read fDatTableName;
    procedure SetEvent(aDataEvent: TIMBEventEntry);
  end;

  TUSControl = class; // forward

  TUSControlObject = class(TSimpleObject)
  constructor Create(aSimpleLayer: TSimpleLayer; const aID: string; aGeometry: TWDGeometryPoint; aName, aDescription: string; aActive: Integer);
  destructor Destroy; override;
  private
    fActive: Integer;
    fName: string;
    fDescription: string;
  public
    property Active: Integer read fActive write fActive;
    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
    function getIconJSON: string;
    function getContextMenuJSON: string;
    function getTooltipJSON: string;
  end;

  TUSControlProperties = class
  constructor Create(const aControlID: Integer; aQuery: TOraQuery);
  destructor Destroy; override;
  private
    fID: Integer;
    fName: string;
    fDescription: string;
    fProperties: TDictionary<string, string>;
    fChildren: TList<TUSControlProperties>;
  public
    function GetJSON: string;
  end;

  TUSControlsLayer = class(TSimpleLayer)
  constructor Create(aScenario: TUSScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aConnectString: string; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
  destructor Destroy; override;
  private
    fUpdateQueue: TList<TUSUpdateQueueEntry>;
    fUpdateQueueEvent: TEvent;
    fUpdateThread: TThread;
    fGlobalEvent: TIMBEventEntry;
    fScenarioEvent: TIMBEventEntry;
    procedure UpdateQueuehandler();
    procedure handleChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
    procedure handleTableChange(aSender: TSubscribeObject; const aAction, aObjectID: Integer);
  private
    function RemoveControl(aControlID: Integer): Boolean;
    procedure SelectControlObjects(aClient: TClient; aControlID: Integer);
    procedure ShowControlProperties(aClient: TClient; aControlID: Integer);
  protected
    fConnectString: string;
    fOraSession: TOraSession;
    fLayerType: Integer;
    fNewPoiCatID: Integer;
    fPoiCategories: TObjectDictionary<string, TUSPOI>;
    fPalette: TWDPalette;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem; // ref
    procedure handleUpdateLayerObject(aClient: TClient; aPayload: TJSONObject); override;
  public
    procedure ReadObjects(aSender: TObject);
  end;

  TUSLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aDiffRange: Double;
    const aConnectString, aNewQuery, aChangeMultipleQuery: string; const aDataEvent: array of TIMBEventEntry;
    aSourceProjection: TGIS_CSProjectedCoordinateSystem; aPalette: TWDPalette; aBasicLayer: Boolean=False;
    aOpacity: Double=0.8);
  destructor Destroy; override;
  private
    fChangeMultipleQuery: string;
  protected
    fConnectString: string;
    fOraSession: TOraSession;
    fLayerType: Integer;
    fNewPoiCatID: Integer;
    fPoiCategories: TObjectDictionary<string, TUSPOI>;
    fPalette: TWDPalette;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem; // ref
    fDataEvents: array of TIMBEventEntry;

    fNewQuery: TOraQuery;
    fUpdateQueue: TList<TUSUpdateQueueEntry>;
    fUpdateQueueEvent: TEvent;
    fUpdateThread: TThread;
    procedure UpdateQueuehandler();

    function UpdateObject(aQuery: TOraQuery; const oid: TWDID; aObject: TLayerObjectWithID): TLayerObjectWithID;
  public
    procedure handleChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
    procedure handleTableChange(aSender: TSubscribeObject; const aAction, aObjectID: Integer);
    property ChangeMultipleQuery: string read fChangeMultipleQuery;
    procedure ReadObjects(aSender: TObject);
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

  TUSBasicLayer = class (TUSLayer)
  constructor Create (aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aDiffRange: Double;
    const aConnectString, aNewQuery, aChangeMultipleQuery: string; const aDataEvent: array of TIMBEventEntry;
    aSourceProjection: TGIS_CSProjectedCoordinateSystem; aPalette: TWDPalette; const aTableName: string);
  destructor Destroy; override;
  private
    fTableName: string;
    fObjectProperties: TUSObjectProperties;
  public
    property BasicTableName: string read fTableName;
    property ObjectProperties: TUSObjectProperties read fObjectProperties;
  end;

  TUSControl = class
  constructor Create(aID: string; aName, aDescription: string; aLat, aLon: Double);
  destructor Destroy; override;
  private
    fID: string;
    fName, fDescription: string;
    fLat, fLon: Double;
  public
    property ID: string read fID;
    property Name: string read fName;
    property Description: string read fDescription;
    property Lat: Double read fLat;
    property Lon: Double read fLon;
  end;

  TUSControlStatus = class
  constructor Create(aID: Integer; aActive: Boolean);
  destructor Destroy; override;
  private
    fID: Integer;
    fActive: Boolean; //TODO: rewrite to Integer to reflect database
  public
    property ID: Integer read fID;
    property Active: Boolean read fActive write fActive;
  end;

  TUSScenario = class(TMCScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription, aFederation: string; aAddBasicLayers: Boolean; aMapView: TMapView; aIMBConnection: TIMBConnection; const aTablePrefix: string);
  destructor Destroy; override;
  private
    fTableprefix: string;
    fIMBConnection: TIMBConnection; // ref
    fLayerRefreshEvent: TIMBEventEntry;
    fUSChartGroups: TObjectList<TUSChartGroup>;
    procedure ReadIndicators (aTableNames: array of string; aOraSession: TOraSession);
    procedure ReadIndicator (aTableName: string; aOraSession: TOraSession);
    procedure ReadBasicLayers(aOraSession: TOraSession);
  public
    property IMBConnection: TIMBConnection read fIMBConnection;
  public
    property Tableprefix: string read fTableprefix;
    function GetUSControlsJSON: string;
    procedure SendUSControlsMessage(aClient: TClient);
    procedure UpsertUSControlStatus(const aControlID, aActive: Integer);
  public
    procedure ReadBasicData(); override;
    function HandleClientSubscribe(aClient: TClient): Boolean; override;
    procedure LayerRefreshed(aTilerLayerID: Integer; const aElementID: string; aTimeStamp: TDateTime; aLayer: TSubscribeObject); override;
  public
    // select objects
    //function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories, aPrefCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    //function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories, aPrefCategories: TArray<string>; aX, aY, aRadius, aMaxZoom: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; const aSelectedIDs: TArray<string>): string; overload; override;
    // select object properties
    function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; override;
  end;

  TUSDBScenario = class
  constructor Create(const aID, aName, aDescription, aParentID, aReferenceID, aTablePrefix, aIMBPrefix, aStatus: string; aPublished: Integer);
  private
    fID: string;
    fName: string;
    fDescription: string;
    fParentID: string;
    fParentname: string;
    fReferenceID: string;
    fReferenceName: string;
    fTablePrefix: string;
    fIMBPrefix: string;
    fStatus: string;
    fPublished: Integer;
  public
    procedure Relink(aUSDBScenarios: TObjectDictionary<string, TUSDBScenario>);
  public
    property ID: string read fID;
    property name: string read fName;
    property description: string read fDescription;
    property parentID: string read fParentID;
    property parentName: string read fParentName;
    property referenceID: string read fReferenceID;
    property referenceName: string read fReferenceName;
    property tablePrefix: string read fTablePrefix;
    property IMBPrefix: string read fIMBPrefix;
    property status: string read fStatus;
    property _published: Integer read fPublished;
  end;

  TUSProject = class(TMCProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios, aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer=-1);
  destructor Destroy; override;
  private
    fUSDBScenarios: TObjectDictionary<string, TUSDBScenario>;
    fUSScenarioFilters: TStringArray;
    fUSControls: TObjectDictionary<string, TUSControl>; //locks with TMonitor
    fUpdateQueue: TList<TUSUpdateQueueEntry>;
    fUpdateQueueEvent: TEvent;
    fControlsUpdateEvent: TIMBEventEntry;
    fUpdateThread: TThread;
    fUSTableSync: TObjectDictionary<string, TSubscribeObject>; //owns, locks with TMonitor
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
    fPreLoadScenarios: Boolean;
    fIMB3Connection: TIMBConnection;
    function getOraSession: TOraSession;
  public
    property IMB3Connection: TIMBConnection read fIMB3Connection;
  protected
    procedure ReadScenarios;
    procedure ReadMeasures;
    procedure ReadUSControls;
    property PreLoadScenarios: Boolean read fPreLoadScenarios;
    property USDBScenarios: TObjectDictionary<string, TUSDBScenario> read fUSDBScenarios;
    function FindMeasure(const aActionID: string; out aMeasure: TMeasureAction): Boolean;
    function ReadScenario(const aID: string): TScenario; override;
  public
    procedure ReadBasicData(); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
    procedure handleTypedClientMessage(aClient: TClient; const aMessageType: string; var aJSONObject: TJSONObject); override;
  public
    property OraSession: TOraSession read getOraSession;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  public
    function GetUSControlsJSON: string;
    function GetUSControlJSONFromDB(aTablePrefix: string): string;
    property USControls: TObjectDictionary<string, TUSControl> read fUSControls;
  private
    procedure HandleControlsUpdate(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
    procedure handleInternalControlsChange(aSender: TSubscribeObject; const aAction, aObjectID: Integer);
    procedure HandleControlsQueueEvent();
  private //typed message handles
    procedure HandleClientMeasureMessage(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject);
    procedure HandleControlPropertyChange(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject);
  public
    function GetTableSync(const aUSTableName: string): TSubscribeObject;
    procedure SendInternalTableUpdate(const aUSTableName: string; aAction, aObjectID: Integer);
  end;


function getUSMapView(aOraSession: TOraSession; const aDefault: TMapView; const aProjectID: string = ''): TMapView;
function getUSProjectID(aOraSession: TOraSession; const aDefault: string): string;
function getUSProjectTypes(aOraSession: TORaSession): TStringArray;
function getUSProjectIDByType(aOraSession: TOraSession; aProjectType: string): string;
function getUSSourceESPG(aOraSession: TOraSession; const aDefault: Integer): Integer;
procedure setUSProjectID(aOraSession: TOraSession; const aProjectID: string; aLat, aLon, aZoomLevel: Double);
function getUSCurrentPublishedScenarioID(aOraSession: TOraSession; aDefault: Integer; const aProjectID: string): Integer;
function getUSScenarioFilter(aOraSession: TOraSession; const aProjectID: string): TStringArray;

function ReadMetaLayer(aSession: TOraSession; const aTablePrefix: string; aMetaLayer: TMetaLayer): Boolean;
function SubscribeUSDataEvents(const aUserName, aIMBEventClass, aTablePrefix: string; aIMBConnection: TIMBConnection): TIMBEventEntryArray;


implementation

function ConnectToUSProject(const aConnectString, aProjectID: string; out aMapView: TMapView): TOraSession;
var
  dbConnection: TOraSession;
begin
  dbConnection := TOraSession.Create(nil);
  dbConnection.ConnectString := aConnectString;
  dbConnection.Open;
  setUSProjectID(dbConnection, aProjectID, aMapView.lat, aMapView.lon, aMapView.zoom); // store project id in database
  aMapView := getUSMapView(dbConnection as TOraSession, TMapView.Create(52.08606, 5.17689, 11));
  Result := dbConnection;
end;

function CreatePaletteFromODB(const aDescription: string; const odbList: TODBList; aIsNoDataTransparent: Boolean): TWDPalette;
var
  entries: TPaletteDiscreteEntryArray;
  noDataColor: TAlphaRGBPixel;
  i: Integer;
  p: TDiscretePaletteEntry;
begin
  noDataColor := 0 ; // transparent // TAlphaColors.black and not TAlphaColors.Alpha;
  setLength(entries, 0);
  for i := 0 to Length(odbList)-1 do
  begin
    if not odbList[i].IsNoData then
    begin
      p.colors := TGeoColors.Create(odbList[i].Color);
      p.minValue := odbList[i].Min;
      p.maxValue := odbList[i].Max;
      p.description := odbList[i].Description;
      setLength(entries, length(entries)+1);
      entries[length(entries)-1] := p;
    end
    else
    begin
      if aIsNoDataTransparent
      then noDataColor := 0
      else noDataColor := odbList[i].Color;
    end;
  end;
  Result := TDiscretePalette.Create(aDescription, entries, TGeoColors.Create(noDataColor));
end;

function ReadMetaObject(aSession: TOraSession; const aTablePrefix: string; aMetaObjectEntries: TMetaObjectEntries): Boolean;
var
  query : TOraQuery;
  metaObjectEntry: TMetaObjectEntry;
begin
  Result := False;
  if TableExists(aSession, aTablePrefix + 'META_OBJECT') then
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := aSession;
      query.SQL.Text := 'SELECT * FROM ' + aTablePrefix + 'META_OBJECT';
      query.Open;
      while not query.EoF do
      begin
        metaObjectEntry.ReadFromQueryRow(query);
        aMetaObjectEntries.Add(metaObjectEntry);
        query.Next;
      end;
    finally
      query.Free;
    end;
    Result := True;
  end;
end;

function ReadMetaObjectProperty(aSession: TOraSession; const aTablePrefix: string; aMetaObjectProperties: TMetaObjectProperties): Boolean;
var
  query : TOraQuery;
  metaObjectPropertyEntry: TMetaObjectPropertyEntry;
begin
  Result := False;
  if TableExists(aSession, aTablePrefix+'META_OBJECT_PROPERTY') then
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := aSession;
      query.SQL.Text := 'SELECT * FROM ' + aTablePrefix + 'META_OBJECT_PROPERTY';
      query.Open;
      while not query.EoF do
      begin
        metaObjectPropertyEntry.ReadFromQueryRow(query);
        aMetaObjectProperties.Add(metaObjectPropertyEntry);
        query.Next;
      end;
    finally
      query.Free;
    end;
    Result := True;
  end;
end;

function ReadMetaLayer(aSession: TOraSession; const aTablePrefix: string; aMetaLayer: TMetaLayer): Boolean;
var
  query: TOraQuery;
  metaLayerEntry: TMetaLayerEntry;
begin
  // check for auto upgrade
  if TableExists(aSession, aTablePrefix+'META_LAYER') and not FieldExists(aSession, aTablePrefix+'META_LAYER', 'DOMAIN') then
  begin
    aSession.ExecSQL(
      'ALTER TABLE '+aTableprefix+'META_LAYER '+
      'ADD (DOMAIN VARCHAR2(50 BYTE),'+
           'DESCRIPTION VARCHAR2(150 BYTE),'+
           'DIFFRANGE NUMBER,'+
           'OBJECTTYPE VARCHAR2(50 BYTE),'+
           'GEOMETRYTYPE VARCHAR2(50 BYTE),'+
           'PUBLISHED INTEGER)');
    aSession.Commit;
  end;

  //TODO: discuss with Hans then implement!
//  if TableExists(aSession, aTablePrefix+'META_LAYER') and not FieldExists(aSession, aTablePrefix+'META_LAYER', 'SELECTPROPERTIES') then
//  begin
//    aSession.ExecSQL(
//      'ALTER TABLE '+aTableprefix+'META_LAYER '+
//      'ADD (SELECTPROPERTIES VARCHAR2(255 BYTE)');
//    aSession.Commit;
//  end;

  query := TOraQuery.Create(nil);
  try
    query.Session := aSession;
    query.SQL.Text := 'SELECT * FROM '+aTablePrefix+'META_LAYER';
    query.Open;
    while not query.Eof do
    begin
      // default
      metaLayerEntry.ReadFromQueryRow(query);
      aMetaLayer.Add(metaLayerEntry.OBJECT_ID, metaLayerEntry);
      query.Next;
    end;
    Result := True;
  finally
    query.Free;
  end;
end;

function SubscribeUSDataEvents(const aUserName, aIMBEventClass, aTablePrefix: string; aIMBConnection: TIMBConnection): TIMBEventEntryArray;
var
  eventNames: TArray<System.string>;
  ev: string;
begin
  setLength(Result, 0);
  eventNames := aIMBEventClass.Split([',']);
  for ev in eventNames do
  begin
    setLength(Result, Length(Result)+1);
    Result[Length(Result)-1] :=
      aIMBConnection.Subscribe(
        aUserName+
        aTableprefix.Substring(aTableprefix.Length-1)+ // #
        aTableprefix.Substring(0, aTablePrefix.length-1)+
        '.'+ev.Trim, False); // add with absolute path
  end;
end;

function GetUSBasicLayerType(const aGeometryType: string): Integer;
begin
  Result := 99;
  if aGeometryType.ToLower = 'multipolygon' then
    Result := 3
  else if aGeometryType.ToLower = 'linestring' then
    Result := 4
  else if aGeometryType.ToLower = 'point' then
    Result := 11;
end;

function RemoveTablePrefix(const aTableName: string): string;
begin
  Result:= StartStripCnt(aTableName, Pos('#', aTableName));
end;

function TMetaLayerEntry.BaseTableNoPrefix: string;
var
  p: Integer;
  t1: string;
  t2: string;
begin
  p := Pos('!', LAYER_TABLE);
  if p>0 then
  begin
    SplitAt(LAYER_TABLE, p, t1, t2);
    Result := t1;
  end
  else
  begin
    p := Pos('*', LAYER_TABLE);
    if p>0 then
    begin
      t1 := Copy(LAYER_TABLE, 1, p-1);
      Result := t1;
    end
    else
    begin // single table?
      if StartsWith(LAYER_TABLE, 'GENE_ROAD_') then
      begin
        t1 := 'GENE_ROAD';
        Result := t1;
      end
      else
      begin
        if StartsWith(LAYER_TABLE, 'GENE_BUILDING_') then
        begin
          t1 := 'GENE_BUILDING';
          Result := t1;
        end
        else
        begin
          t1 := LAYER_TABLE;
          Result := t1;
        end;
      end;
    end;
  end;
end;

function TMetaLayerEntry.BaseTable(const aTablePrefix:string): string;
var
  p: Integer;
  t1: string;
  t2: string;
begin
  p := Pos('!', LAYER_TABLE);
  if p>0 then
  begin
    SplitAt(LAYER_TABLE, p, t1, t2);
    Result := aTablePrefix+t1;
  end
  else
  begin
    p := Pos('*', LAYER_TABLE);
    if p>0 then
    begin
      t1 := Copy(LAYER_TABLE, 1, p-1);
      Result := aTablePrefix+t1;
    end
    else
    begin // single table?
      if StartsWith(LAYER_TABLE, 'GENE_ROAD_') then
      begin
        t1 := 'GENE_ROAD';
        Result := aTablePrefix+t1;
      end
      else
      begin
        if StartsWith(LAYER_TABLE, 'GENE_BUILDING_') then
        begin
          t1 := 'GENE_BUILDING';
          Result := aTablePrefix+t1;
        end
        else
        begin
          t1 := LAYER_TABLE;
          Result := aTablePrefix+t1;
        end;
      end;
    end;
  end;
end;

function TMetaLayerEntry.BuildJoin(const aTablePrefix: string; out aShapePrefix, aObjectIDPrefix, aPreJoin: string): string;
var
  p: Integer;
  t1: string;
  t2: string;
begin
  aShapePrefix := '';
  aObjectIDPrefix := '';
  aPreJoin := '';
  p := Pos('!', LAYER_TABLE);
  if p>0 then
  begin
    SplitAt(LAYER_TABLE, p, t1, t2);
    Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
    aShapePrefix := 't1.';
    aObjectIDPrefix := aShapePrefix;
  end
  else
  begin
    p := Pos('*', LAYER_TABLE);
    if p>0 then
    begin
      t1 := Copy(LAYER_TABLE, 1, p-1);
      t2 := LAYER_TABLE;
      Delete(t2, p, 1);
      Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
      aShapePrefix := 't1.';
      aObjectIDPrefix := aShapePrefix;
    end
    else
    begin // single table?
      if StartsWith(LAYER_TABLE, 'GENE_ROAD_') then
      begin
        t1 := 'GENE_ROAD';
        t2 := LAYER_TABLE;
        Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
        aShapePrefix := 't1.';
        aObjectIDPrefix := aShapePrefix;
      end
      else
      begin
        if StartsWith(LAYER_TABLE, 'GENE_BUILDING_') then
        begin
          t1 := 'GENE_BUILDING';
          t2 := LAYER_TABLE;
          Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
          aShapePrefix := 't1.';
          aObjectIDPrefix := aShapePrefix;
        end
        else if StartsWith(LAYER_TABLE, 'AIR_EMISSIONS') then
        begin
          t1 := LAYER_TABLE;
          Result := aTablePrefix+t1+' t2, '+aTablePrefix+'GENE_ROAD'+' t1';
          aShapePrefix := '';
          aObjectIDPrefix := 't1.';
          aPreJoin := 't1.OBJECT_ID=t2.OBJECT_ID AND ';
        end
        else
        begin
          t1 := LAYER_TABLE;
          //t2 := '';
          Result := aTablePrefix+t1+' t1';
          aShapePrefix := 't1.';
          aObjectIDPrefix := aShapePrefix;
        end;
      end;
    end;
  end;
end;

function TMetaLayerEntry.BuildLegendJSON(aLegendFormat: TLegendFormat): string;
var
  i: Integer;
begin
  Result := '';
  case aLegendFormat of
    lfVertical:
      begin
        for i := 0 to length(odbList)-1 do
        begin
          if not odbList[i].IsNoData then
          begin
            if Result<>''
            then Result := Result+',';
            Result := Result+'{"'+odbList[i].Description+'":{"fillColor":"'+ColorToJSON(odbList[i].Color)+'"}}';
          end;
        end;
        Result := '"grid":{"title":"'+FormatLegendDescription(LEGEND_DESC)+'","labels":['+Result+']}';;
      end;
    lfHorizontal:
      begin
        for i := 0 to length(odbList)-1 do
        begin
          if not odbList[i].IsNoData then
          begin
            if Result<>''
            then Result := Result+',';
            Result := Result+'"'+odbList[i].Description+'":{"fillColor":"'+ColorToJSON(odbList[i].Color)+'"}}';
          end;
        end;
        Result := '"grid2":{"title":"'+FormatLegendDescription(LEGEND_DESC)+'","labels":[{'+Result+'}]}';;
      end;
  end;
end;


function TMetaLayerEntry.CreateUSLayer(aScenario: TScenario; const aTablePrefix: string; const aConnectString: string;
  const aDataEvent: array of TIMBEventEntry; aSourceProjection: TGIS_CSProjectedCoordinateSystem; const aDomain, aName: string;
  aOpacity: Double; aDefaultLoad: Boolean): TLayerBase;

  function defaultValue(aValue, aDefault: Double): Double; overload;
  begin
    if not IsNaN(aValue)
    then Result := aValue
    else Result := aDefault;
  end;

  function defaultValue(const aValue, aDefault: string): string; overload;
  begin

    if aValue<>''
    then Result := aValue
    else Result := aDefault;
  end;

var
  objectTypes: string;
begin
  Result := nil; // sentinel
  case LAYER_TYPE mod 100 of // i+100 image layer version same as i but ignored by US3D
    1:
      begin
        objectTypes := '"receptor"';
        geometryType := 'Point';
        diffRange := defaultValue(diffRange, autoDiffRange*0.3);
      end;
    2:
      begin
        objectTypes := '"grid"';
        geometryType := 'MultiPolygon'; // todo: ?
        diffRange := defaultValue(diffRange, autoDiffRange*0.3);
      end;
    3, 8:
      begin
        objectTypes := '"building"'; // 3 buildings, 8 RS buildings
        geometryType := 'MultiPolygon';
        diffRange := defaultValue(diffRange, autoDiffRange*0.3);
      end;
    4,    // road color (VALUE_EXPR)
    5:    // road color (VALUE_EXPR) and width (TEXTURE_EXPR)
      begin
        objectTypes := '"road"';
        geometryType := 'LineString';
        diffRange := defaultValue(diffRange, autoDiffRange*0.3);
      end;
    9:    // enrg color (VALUE_EXPR) and width (TEXTURE_EXPR)
      begin
        objectTypes := '"energy"';
        geometryType := 'LineString';
        diffRange := defaultValue(diffRange, autoDiffRange*0.3);
      end;
    10:
      begin
        objectTypes := '"location"';
        geometryType := 'Point';
        diffRange := diffRange; // todo:
      end;
    11:
      begin
        objectTypes := '"location"';
        geometryType := 'Point';
        diffRange := defaultValue(diffRange, autoDiffRange*0.3);
      end;
    12:
      begin
        objectTypes := '"control"';
        geometryType := '';
        diffRange := diffRange; // todo:
        if aScenario is TUSScenario then
        begin
          Result := TUSControlsLayer.Create(
            aScenario as TUSScenario,
            aDomain,
            OBJECT_ID.ToString, // id
            aName,
            LEGEND_DESC.Replace('~~', '-').replace('\', '-'),
            False,
            aConnectString,
            aSourceProjection);
        end;
      end;
    21:
      begin
        objectTypes := '"poi"';
        geometryType := 'Point';
        diffRange := diffRange; // todo:
      end;
  else
    // 31 vector layer ?
    objectTypes := '';
    geometryType := '';
    diffRange := diffRange;
  end;
  if (not Assigned(Result))  and (geometryType<>'') then
  begin
    Result := TUSLayer.Create(aScenario,
      aDomain,
      OBJECT_ID.ToString, // id
      aName,
      LEGEND_DESC.Replace('~~', '-').replace('\', '-'), // description
      aDefaultLoad,
      objectTypes, geometryType,
      LAYER_TYPE mod 100,
      diffRange,
      aConnectString,
      SQLQueryNew(aTablePrefix),
      SQLQueryChangeMultiple(aTablePrefix),
      aDataEvent,
      aSourceProjection,
      CreatePaletteFromODB(LEGEND_DESC, odbList, True),
      False,
      aOpacity);
    (Result as TUSLayer).fLegendJSON := BuildLegendJSON(lfVertical);
    (Result as TUSLayer).query := SQLQuery(aTableprefix);
  end;
end;

procedure TMetaLayerEntry.ReadFromQueryRow(aQuery: TOraQuery);

  function StringField(const aFieldName: string; const aDefaultValue: string=''): string;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsString
    else Result := aDefaultValue;
  end;

  function IntField(const aFieldName: string; aDefaultValue: Integer=0): Integer;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsInteger
    else Result := aDefaultValue;
  end;

  function DoubleField(const aFieldName: string; aDefaultValue: Double=NaN): Double;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsFloat
    else Result := aDefaultValue;
  end;

var
  sl: TStringList;
begin
  // default
  OBJECT_ID := IntField('OBJECT_ID');
  LAYER_TYPE := IntField('LAYER_TYPE');
  LAYER_TABLE := StringField('LAYER_TABLE');
  LEGEND_FILE := StringField('LEGEND_FILE');
  LEGEND_DESC := StringField('LEGEND_DESC');
  VALUE_EXPR := StringField('VALUE_EXPR');
  VALUE_NODATA := DoubleField('VALUE_NODATA');
  JOINCONDITION := StringField('JOINCONDITION');
  TEXTURE_FILE := StringField('TEXTURE_FILE');
  TEXTURE_EXPR := StringField('TEXTURE_EXPR');
  ROW_START := IntField('ROW_START');
  ROW_SIZE := IntField('ROW_SIZE');
  COL_START := IntField('COL_START');
  COL_SIZE := IntField('COL_SIZE');
  ROW_FIELD := StringField('ROW_FIELD');
  COL_FIELD := StringField('COL_FIELD');
  IS_CELL_BASED := IntField('IS_CELL_BASED')=1;
  MXR := DoubleField('MXR');
  MXC := DoubleField('MXC');
  MXT := DoubleField('MXT');
  MYR := DoubleField('MYR');
  MYC := DoubleField('MYC');
  MYT := DoubleField('MYT');
  IMB_EVENTCLASS := StringField('IMB_EVENTCLASS');

  // added for web interface
  domain := StringField('DOMAIN');
  description := StringField('DESCRIPTION');
  diffRange := DoubleField('DIFFRANGE');
  objectType := StringField('OBJECTTYPE');
  geometryType := StringField('GEOMETRYTYPE');
  _published := IntField('PUBLISHED', 1);
  {
  ALTER TABLE VXX#META_LAYER
  ADD (DOMAIN VARCHAR2(50), DESCRIPTION VARCHAR2(150), DIFFRANGE NUMBER, OBJECTTYPE VARCHAR2(50), GEOMETRYTYPE VARCHAR2(50), PUBLISHED INTEGER);

  UPDATE V21#META_LAYER SET DIFFRANGE = 2 WHERE object_id in (142,52,53,54,55,153,3,4,28,32,141,56);
  }

  LegendAVL := '';
  setLength(odbList, 0);
  if LEGEND_FILE<>'' then
  begin
    sl := TStringList.Create;
    try
      if SaveBlobToStrings(aQuery.Session, 'VI3D_MODEL', 'PATH', LEGEND_FILE, 'BINFILE', sl) then
      begin
        LegendAVL := sl.Text;
        odbList := ODBFileToODBList(sl);
      end;
    finally
      sl.Free;
    end;
  end;
end;

function TMetaLayerEntry.autoDiffRange: Double;
var
  odb: TODBRecord;
  values: TList<double>;

  procedure addValue(aValue: Double);
  begin
    if not IsNaN(aValue)
    then values.Add(aValue);
  end;

begin
  if Length(odbList) > 2 then
  begin
    values := TList<double>.Create;
    try
      for odb in odbList do
      begin
        addValue(odb.Min);
        addValue(odb.Max);
      end;
      values.Sort;
      // remove first and last value to remove large ranges on the edge off legends
      values.Delete(0);
      values.Delete(values.Count-1);
      // use difference highest-lowest/aFactor
      Result := Abs(values[values.count-1]-values[0]);
    finally
      values.Free;
    end;
  end
  else
    Result := 0.2;
end;

function TMetaLayerEntry.SQLQuery(const aTablePrefix:string; xMin, yMin, xMax, yMax: Integer): string;
var
  join: string;
  ShapePrefix: string;
  ObjectIDPrefix: string;
  PreJoin: string;
  cellIndexFiltering: Boolean;
begin
  join := BuildJoin(aTablePrefix, ShapePrefix, ObjectIDPrefix, PreJoin);
  case LAYER_TYPE mod 100 of
    2:
      begin
        cellIndexFiltering :=  (xMin<=xMax) and (yMin<=yMax);
        join := JOINCONDITION;
        if (join<>'') and cellIndexFiltering
        then join := join+' AND ';
        Result :=
            'SELECT '+
              COL_FIELD+', '+
              ROW_FIELD+', '+
              VALUE_EXPR+' '+
            'FROM '+aTablePrefix+LAYER_TABLE;
        if (join<>'') or cellIndexFiltering then
        begin
          Result := Result+' '+
            'WHERE '+
              join;
          if cellIndexFiltering
          then Result := Result+' '+
              IntToStr(xMin)+'<='+COL_FIELD+' AND '+COL_FIELD+'<='+IntToStr(xMax)+' AND '+
              IntToStr(yMin)+'<='+ROW_FIELD+' AND '+ROW_FIELD+'<='+IntToStr(yMax);
        end;
        Result := Result+' '+
            'ORDER BY '+ROW_FIELD+','+COL_FIELD;
      end;
    4:
      begin
        Result :=
          'SELECT '+
            ObjectIDPrefix+'OBJECT_ID, '+
            VALUE_EXPR+' AS VALUE, '+
            ShapePrefix+'SHAPE '+
          'FROM '+join;
        if JOINCONDITION<>''
        then Result := Result+' '+
          'WHERE '+PreJoin+JOINCONDITION;
      end;
    5, 9:
      begin
        Result :=
          'SELECT '+
            ObjectIDPrefix+'OBJECT_ID, '+
            VALUE_EXPR+','+
            TEXTURE_EXPR+','+
            ShapePrefix+'SHAPE '+
          'FROM '+join;
        if JOINCONDITION<>''
        then Result := Result+' '+
          'WHERE '+PreJoin+JOINCONDITION;
      end;
    21:
      begin
        Result :=
          'SELECT '+
            ObjectIDPrefix+'OBJECT_ID, '+
            ShapePrefix+'shape.sdo_point.x, '+
            ShapePrefix+'shape.sdo_point.y, '+
            ShapePrefix+'poiType, '+
            ShapePrefix+'category '+
          'FROM '+join;
        if JOINCONDITION<>''
        then Result := Result+' '+
          'WHERE '+PreJoin+JOINCONDITION;
      end;
    10, 98: //control, for now hardcoded the base table
    begin
      Result := 'SELECT ' +
                  't1.OBJECT_ID as OBJECT_ID, '+
                  't1.ACTIVE as VALUE, '+
                  't2.Y as Y, '+
                  't2.X as X '+
                  'FROM ' + aTablePrefix +'GENE_CONTROL t1 '+
                  'LEFT JOIN '+
                  'GENE_CONTROL t2 '+
                  'on t1.OBJECT_ID = t2.OBJECT_ID';
    end;
  else
    Result :=
      'SELECT '+
        ObjectIDPrefix+'OBJECT_ID, '+
        VALUE_EXPR+' AS VALUE, '+
        ShapePrefix+'SHAPE '+
      'FROM '+join;
    if JOINCONDITION<>''
    then Result := Result+' '+
      'WHERE '+PreJoin+JOINCONDITION;
  end;
end;

function TMetaLayerEntry.SQLQueryChangeMultiple(
  const aTablePrefix: string): string;
var
  join: string;
  ShapePrefix: string;
  ObjectIDPrefix: string;
  PreJoin: string;
begin
  join := BuildJoin(aTablePrefix, ShapePrefix, ObjectIDPrefix, PreJoin);
  case LAYER_TYPE mod 100 of
    2:
      begin
        Result := ''; // todo: can this work?
      end;
    4:
      begin
        Result :=
          'SELECT '+ObjectIDPrefix+'OBJECT_ID, '+
            VALUE_EXPR+' AS VALUE, '+
            ShapePrefix+'SHAPE '+
          'FROM '+join+' '+
          'WHERE ';
        if JOINCONDITION<>''
          then Result := Result+PreJoin+JOINCONDITION +' AND ';
        Result := Result + ObjectIDPrefix+'OBJECT_ID in ';
      end;
    5, 9:
      begin
        Result :=
          'SELECT '+ObjectIDPrefix+'OBJECT_ID, '+
            VALUE_EXPR+','+
            TEXTURE_EXPR+','+
            ShapePrefix+'SHAPE '+
          'FROM '+join+' '+
          'WHERE ';
        if JOINCONDITION<>''
          then Result := Result+PreJoin+JOINCONDITION +' AND ';
        Result := Result +ObjectIDPrefix+'OBJECT_ID in ';
      end;
    21:
      begin
        Result :=
          'SELECT '+ObjectIDPrefix+'OBJECT_ID, '+
            ShapePrefix+'shape.sdo_point.x, '+
            ShapePrefix+'shape.sdo_point.y, '+
            ShapePrefix+'poiType, '+
            ShapePrefix+'category '+
          'FROM '+join+' '+
          'WHERE ';
        if JOINCONDITION<>''
          then Result := Result+PreJoin+JOINCONDITION +' AND ';
        Result := Result +ObjectIDPrefix+'OBJECT_ID in ';
      end;
    10, 98: //control
      begin
        Result := 'SELECT ' +
                  't1.OBJECT_ID as OBJECT_ID, '+
                  't1.ACTIVE as VALUE, '+
                  't2.Y as Y, '+
                  't2.X as X '+
                  'FROM ' + aTablePrefix +'GENE_CONTROL t1 '+
                  'LEFT JOIN '+
                  'GENE_CONTROL t2 '+
                  'on t1.OBJECT_ID = t2.OBJECT_ID '+
                  'where t1.OBJECT_ID in ';
      end;
  else
    Result :=
      'SELECT '+ObjectIDPrefix+'OBJECT_ID, '+
        VALUE_EXPR+' AS VALUE, '+
        ShapePrefix+'SHAPE '+
      'FROM '+join+' '+
      'WHERE ';
    if JOINCONDITION<>''
      then Result := Result+PreJoin+JOINCONDITION +' AND ';
    Result := Result +ObjectIDPrefix+'OBJECT_ID in ';
  end;
end;

function TMetaLayerEntry.SQLQueryNew(const aTablePrefix: string): string;
var
  join: string;
  ShapePrefix: string;
  ObjectIDPrefix: string;
  PreJoin: string;
begin
  join := BuildJoin(aTablePrefix, ShapePrefix, ObjectIDPrefix, PreJoin);
  case LAYER_TYPE mod 100 of
    2:
      begin
        Result := ''; // todo: can this work?
      end;
    4:
      begin
        Result :=
          'SELECT '+
            VALUE_EXPR+' AS VALUE, '+
            ShapePrefix+'SHAPE '+
          'FROM '+join+' '+
          'WHERE '+ObjectIDPrefix+'OBJECT_ID=:OBJECT_ID';
        if JOINCONDITION<>''
        then Result := Result+' AND '+ PreJoin+JOINCONDITION;
      end;
    5, 9:
      begin
        Result :=
          'SELECT '+
            VALUE_EXPR+','+
            TEXTURE_EXPR+','+
            ShapePrefix+'SHAPE '+
          'FROM '+join+' '+
          'WHERE '+ObjectIDPrefix+'OBJECT_ID=:OBJECT_ID';
        if JOINCONDITION<>''
        then Result := Result+' AND '+PreJoin+JOINCONDITION;
      end;
    21:
      begin
        Result :=
          'SELECT '+
            ShapePrefix+'shape.sdo_point.x, '+
            ShapePrefix+'shape.sdo_point.y, '+
            ShapePrefix+'poiType, '+
            ShapePrefix+'category '+
          'FROM '+join+' '+
          'WHERE '+ObjectIDPrefix+'OBJECT_ID=:OBJECT_ID';
        if JOINCONDITION<>''
        then Result := Result+' AND '+PreJoin+JOINCONDITION;
      end;
    10, 98: //control
      begin
        Result:= 'SELECT ' +
                  't1.OBJECT_ID as OBJECT_ID, '+
                  't1.ACTIVE as VALUE, '+
                  't2.Y as Y, '+
                  't2.X as X '+
                  'FROM ' + aTablePrefix +'GENE_CONTROL t1 '+
                  'LEFT JOIN '+
                  'GENE_CONTROL t2 '+
                  'on t1.OBJECT_ID = t2.OBJECT_ID '+
                  'where t1.OBJECT_ID=:OBJECT_ID';
      end;
  else
    Result :=
      'SELECT '+
        VALUE_EXPR+' AS VALUE, '+
        ShapePrefix+'SHAPE '+
      'FROM '+join+' '+
      'WHERE '+ObjectIDPrefix+'OBJECT_ID=:OBJECT_ID';
    if JOINCONDITION<>''
    then Result := Result+' AND '+PreJoin+JOINCONDITION;
  end;
end;

{ TUSRoadIC }

constructor TUSRoadIC.Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue, aTexture: Double);
begin
  inherited Create(aLayer, aID, aGeometry, aValue);
  fTexture := aTexture;
end;

function TUSRoadIC.Encode: TByteBuffer;
begin
  Result :=
    TByteBuffer.bb_tag_Double(icehTilerTexture, texture)+
    inherited Encode;
end;

{ TUSRoadICLR }

constructor TUSRoadICLR.Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue, aValue2, aTexture, aTexture2: Double);
begin
  inherited Create(aLayer, aID, aGeometry, aValue);
  fValue2 := aValue2;
  fTexture := aTexture;
  fTexture2 := aTexture2;
end;

function TUSRoadICLR.Encode: TByteBuffer;
begin
  Result :=
    TByteBuffer.bb_tag_Double(icehTilerTexture2, texture2)+
    TByteBuffer.bb_tag_Double(icehTilerTexture, texture)+
    TByteBuffer.bb_tag_Double(icehTilerValue2, value2)+
    inherited Encode;
end;

{ TUSPOI }

constructor TUSPOI.Create(aID: Integer; aPicture: TPicture);
begin
  inherited Create;
  fID := aID;
  fPicture := aPicture;
end;

destructor TUSPOI.Destroy;
begin
  FreeAndNil(fPicture);
  inherited Destroy;
end;

{ TUSUpdateQueueEntry }

class function TUSUpdateQueueEntry.Create(aObjectID, aAction: Integer): TUSUpdateQueueEntry;
begin
  Result.objectID := aObjectID;
  Result.action := aAction;
end;

{ TUSLayer }

constructor TUSLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aDiffRange: Double;
  const aConnectString, aNewQuery, aChangeMultipleQuery: string; const aDataEvent: array of TIMBEventEntry; aSourceProjection: TGIS_CSProjectedCoordinateSystem;
  aPalette: TWDPalette; aBasicLayer: Boolean; aOpacity: Double);
var
  i: Integer;
begin
  fLayerType := aLayerType;
  fPoiCategories := TObjectDictionary<string, TUSPOI>.Create([doOwnsValues]);
  fNewPoiCatID := 0;
  fPalette := aPalette;
  fConnectString := aConnectString;

  fOraSession := TOraSession.Create(nil);
  fOraSession.ConnectString := fConnectString;
  fOraSession.Connect;

  fSourceProjection := aSourceProjection;

  if aNewQuery<>'' then
  begin
    fNewQuery := TOraQuery.Create(nil);
    fNewQuery.Session := fOraSession;
    fNewQuery.sql.Text := aNewQuery;
    fNewQuery.Prepare;
  end
  else fNewQuery := nil;
  fChangeMultipleQuery := aChangeMultipleQuery;

  fUpdateQueue := TList<TUSUpdateQueueEntry>.Create;
  fUpdateQueueEvent := TEvent.Create(nil, False, False, '');
  fUpdateThread := TThread.CreateAnonymousThread(UpdateQueuehandler);
  fUpdateThread.NameThreadForDebugging(ElementID + ' queue handler');
  fUpdateThread.FreeOnTerminate := False;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, ltTile, True, aDiffRange, aBasicLayer, aOpacity);
  fUpdateThread.Start;

  setLength(fDataEvents, length(aDataEvent));
  for i := 0 to length(aDataEvent)-1 do
  begin
    fDataEvents[i] := aDataEvent[i];
    fDataEvents[i].OnChangeObject := handleChangeObject;
    if aScenario.Project is TUSProject
    then SubscribeTo((aScenario.Project as TUSProject).GetTableSync(fDataEvents[i].EventName), handleTableChange);
  end;
end;

destructor TUSLayer.Destroy;
begin
  inherited;
  fUpdateThread.Terminate;
  fUpdateQueueEvent.SetEvent;
  FreeAndNil(fUpdateThread);
  FreeAndNil(fUpdateQueueEvent);
  FreeAndNil(fUpdateQueue);
  FreeAndNil(fPoiCategories);
  FreeAndNil(fPalette);
  FreeAndNil(fNewQuery);
  FreeAndNil(fOraSession);
end;

procedure TUSLayer.handleChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string);
begin
  handleTableChange(nil, aAction, aObjectID);
end;

procedure TUSLayer.handleTableChange(aSender: TSubscribeObject; const aAction, aObjectID: Integer);
begin
  TMonitor.Enter(fUpdateQueueEvent);
  try
    begin
      try
        fUpdateQueue.Add(TUSUpdateQueueEntry.Create(aObjectID, aAction));
        fUpdateQueueEvent.SetEvent;
      except
        on e: Exception do
        begin
          Log.WriteLn('TUSLayer.handleTableChange. objectid: ' + aObjectID.ToString + ', action: ' + aAction.ToString+': '+e.Message, llError);
        end
      end;
    end
  finally
    TMonitor.Exit(fUpdateQueueEvent);
  end;
end;

function TUSLayer.UpdateObject(aQuery: TOraQuery; const oid: TWDID; aObject: TLayerObjectWithID): TLayerObjectWithID;

  function FieldFloatValueOrNaN(aField: TField): Double;
  begin
    if aField.IsNull
    then Result := NaN
    else Result := aField.AsFloat;
  end;

var
  value: double;
  geometryPoint: TWDGeometryPoint;
  geometry: TWDGeometry;
  value2: Double;
  texture: Double;
  texture2: Double;
begin
  Result := aObject;
  objectsLock.BeginWrite;
  try
  case fLayerType of
    1, 11:
      begin
        value := FieldFloatValueOrNaN(aQuery.FieldByName('VALUE'));
        if not Assigned(aObject) then
        begin
          geometryPoint := CreateWDGeometryPointFromSDOShape(aQuery, 'SHAPE');
          projectGeometryPoint(geometryPoint, fSourceProjection);
          Result := TGeometryPointLayerObject.Create(Self, oid, geometryPoint, value);
        end
        else (aObject as TGeometryPointLayerObject).value := value;
      end;
    4: // road (VALUE_EXPR)
      begin
        // unidirectional, not left and right
        value := FieldFloatValueOrNaN(aQuery.FieldByName('VALUE'));
        if not Assigned(aObject) then
        begin
          geometry := CreateWDGeometryFromSDOShape(aQuery, 'SHAPE');
          projectGeometry(geometry, fSourceProjection);
          Result := TGeometryLayerObject.Create(Self, oid, geometry, value);
        end
        else (aObject as TGeometryLayerObject).value := value;
      end;
    5,9: // road/energy (VALUE_EXPR) and width (TEXTURE_EXPR) left and right, for energy right will be null -> NaN
      begin
        // Left and right
        value := FieldFloatValueOrNaN(aQuery.Fields[1]);
        value2 := FieldFloatValueOrNaN(aQuery.Fields[2]);
        texture := FieldFloatValueOrNaN(aQuery.Fields[3]);
        texture2 := FieldFloatValueOrNaN(aQuery.Fields[4]);
        if not Assigned(aObject) then
        begin
          geometry := CreateWDGeometryFromSDOShape(aQuery, 'SHAPE');
          projectGeometry(geometry, fSourceProjection);
          Result := TUSRoadICLR.Create(Self, oid, geometry, value, value2, texture, texture2);
        end
        else
        begin
          (aObject as TUSRoadICLR).value := value;
          (aObject as TUSRoadICLR).value2 := value2;
          (aObject as TUSRoadICLR).texture := texture;
          (aObject as TUSRoadICLR).texture2 := texture2;
        end;
      end;
    21: // POI
      begin
        {
        geometryPoint := TWDGeometryPoint.Create;
        try
          geometryPoint.x := aQuery.Fields[1].AsFloat;
          geometryPoint.y := aQuery.Fields[2].AsFloat;
          // no projection, is already in lat/lon
          poiType := aQuery.Fields[3].AsString;
          poiCat := aQuery.Fields[4].AsString;
          if not fPoiCategories.TryGetValue(poicat+'_'+poiType, usPOI) then
          begin
            usPOI := TUSPOI.Create(fNewPoiCatID, TPicture.Create);
            fNewPoiCatID := fNewPoiCatID+1;
            try
              resourceFolder := ExtractFilePath(ParamStr(0));
              usPOI.picture.Graphic.LoadFromFile(resourceFolder+poicat+'_'+poiType+'.png');
            except
              on e: Exception
              do Log.WriteLn('Exception loading POI image '+resourceFolder+poicat+'_'+poiType+'.png', llError);
            end;
            // signal POI image to tiler
            //ImageToBytes(usPOI);
            //fTilerLayer.s
            //stream  := TBytesStream.Create;
            //try
            //  usPOI.picture.Graphic.SaveToStream(stream);
            //  fOutputEvent.signalEvent(TByteBuffer.bb_tag_tbytes(icehTilerPOIImage, stream.Bytes)); // todo: check correct number of bytes
            //finally
            //  stream.Free;
            //end;
          end;
        finally
          objects.Add(oid, TGeometryLayerPOIObject.Create(Self, oid, usPOI.ID, geometryPoint));
        end;
        }
      end;
    10, 98: // control
      begin
        value := FieldFloatValueOrNaN(aQuery.FieldByName('VALUE'));
        if not Assigned(aObject) then
        begin
          geometryPoint := TWDGeometryPoint.Create(FieldFloatValueOrNaN(aQuery.FieldByName('X')),
                          FieldFloatValueOrNaN(aQuery.FieldByName('Y')),
                          NaN);
          projectGeometryPoint(geometryPoint, fSourceProjection);
          Result := TGeometryPointLayerObject.Create(Self, oid, geometryPoint, value);
        end
        else (aObject as TGeometryPointLayerObject).value := value;
      end;
  else
    value := FieldFloatValueOrNaN(aQuery.FieldByName('VALUE'));
    if not Assigned(aObject) then
    begin
      geometry := CreateWDGeometryFromSDOShape(aQuery, 'SHAPE');
      projectGeometry(geometry, fSourceProjection);
      Result := TGeometryLayerObject.Create(Self, oid, geometry, value);
    end
    else (aObject as TGeometryLayerObject).value := value;
  end;
  finally
    objectsLock.EndWrite;
  end
end;

procedure TUSLayer.UpdateQueuehandler;

  procedure ReadMultipleObjects(const aIdString: string; const oraSession: TOraSession);
  var
   query: TOraQuery;
   wdid: TWDID;
   o: TLayerObjectWithID;
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := oraSession;
      query.SQL.Text := ChangeMultipleQuery + '(' + aIdString + ')';
      query.UniDirectional := True;
      query.Open;
      query.First;
      while (not query.Eof) do
        begin
          wdid := AnsiString((query.FieldByName('OBJECT_ID').AsInteger).ToString);
          if FindObject(wdid, o) then
            begin
              UpdateObject(query, wdid, o);
              signalObject(o);
            end
          else
            begin
              AddObject(UpdateObject(query, wdid, nil));
            end;
          query.Next;
        end;
    finally
      query.Free;
    end;
  end;

var
  localQueue: TList<TUSUpdateQueueEntry>;
  tempQueue: TList<TUSUpdateQueueEntry>;
  entry: TUSUpdateQueueEntry;
  newIDs: string;
  ChangeStack: TStack<string>;
  ChangeString: string;
  i: Integer;
  wdid: TWDID;
  o: TLayerObjectWithID;
  oraSession: TOraSession;
begin
  localQueue := TList<TUSUpdateQueueEntry>.Create;
  oraSession := TOraSession.Create(nil);
  ChangeStack := TStack<string>.Create;
  try
    oraSession.connectString := fConnectString;
    oraSession.open;
    while not TThread.CheckTerminated do
    begin
      if fUpdateQueueEvent.WaitFor=wrSignaled then
      begin
        // swap queues
        TMonitor.Enter(fUpdateQueueEvent);
        try
          tempQueue := fUpdateQueue;
          fUpdateQueue := localQueue;
          localQueue := tempQueue;
        finally
          TMonitor.Exit(fUpdateQueueEvent);
        end;
        if localQueue.Count>0 then
        begin
          newIDs := '';
          for entry in localQueue do
          try
            begin
              wdid := AnsiString(entry.objectID.ToString);
              // process entries
              if entry.action=actionDelete then
              begin
                if FindObject(wdid, o)
                then RemoveObject(o);
              end
              else if entry.action=actionNew then
              begin
//                newCount := newCount+1;
                if not FindObject(wdid, o) then
                begin
                  ChangeStack.Push(entry.objectID.ToString);
                end
                else
                  Log.WriteLn('TUSLayer.handleChangeObject: Received actionNew on existing object: ('+entry.objectID.toString+')', llWarning);
              end
              else if entry.action=actionChange then
              begin
                //Especially for Han, no check if object actually exists:-)
                ChangeStack.Push(entry.objectID.ToString);
              end;
            end;
          except
            on e: Exception
            do Log.WriteLn('Exception in handleChangeObject: '+e.Message, llError);
          end;
          if (ChangeStack.Count > 0) and (ChangeMultipleQuery <> '') then
          begin
            while (ChangeStack.Count > 1000) do
            begin
              changestring := ChangeStack.Pop;
              for i:=2 to 1000 do
                changestring := changestring + ', ' + ChangeStack.Pop;
              ReadMultipleObjects(changestring, oraSession);
            end;
            if ChangeStack.Count > 0 then
            begin
              changestring := ChangeStack.Pop;
              while (ChangeStack.Count > 0) do
                changestring := changestring + ', ' + ChangeStack.Pop;
              ReadMultipleObjects(changestring, oraSession);
            end;
          end;
          //Todo: look into cached updates/batching!
        end;
        localQueue.Clear;
      end;
    end;
  finally
    localQueue.Free;
    oraSession.Free; //todo can I use one try/finally for all of these?
    ChangeStack.Free;
  end;
end;

procedure TUSLayer.ReadObjects(aSender: TObject);

  function FieldFloatValueOrNaN(aField: TField): Double;
  begin
    if aField.IsNull
    then Result := NaN
    else Result := aField.AsFloat;
  end;

var
  oraSession: TOraSession;
  query: TOraQuery;
  oid: RawByteString;
begin
  // register layer with tiler?
  // start query
  // decode objects and add to aLayer
  // send objects to tiler?

  // create new ora session because we are running in a different thread
  oraSession := TOraSession.Create(nil);
  try
    oraSession.connectString := fConnectString;
    oraSession.open;
    query := TOraQuery.Create(nil);
    try
      query.Session := oraSession;
      query.SQL.Text := fQuery; //.Replace('SELECT ', 'SELECT t1.OBJECT_ID,');
      query.Open;
      query.First;
      while Assigned(objects) and not query.Eof do
      begin
        try
          oid := AnsiString(query.Fields[0].AsInteger.ToString);
          objects.AddOrSetValue(oid, UpdateObject(query, oid, nil)); // always new object, no registering
          if (objects.Count mod 10000) = 0 then
            Log.Progress('Busy reading objects: ' + objects.Count.ToString + ' for ' + fScenario.ID + '-' + name);
          query.Next;
        except
          Log.WriteLn('Error reading object ' + elementID + ' in layer ' + name, llError, 1);
        end;
      end;
    finally
      query.Free;
    end;
    Log.WriteLn(elementID+' ('+fLayerType.toString+'): '+name+', read objects (us)', llNormal, 1);
    // register with tiler
    RegisterLayer;
  finally
    oraSession.Free;
  end;
end;

procedure TUSLayer.RegisterLayer;
begin
  case fLayerType of
    1: // receptors
      RegisterOnTiler(False, SliceType, name, GetSetting(MaxEdgeLengthInMetersSwitchName, DefaultMaxEdgeLengthInMeters));
    //2:; grid
    3,  // buildings, polygon
    8,  // RS buildings, polygon
    4,  // road color (VALUE_EXPR) unidirectional, path, intensity
    5,  // road color (VALUE_EXPR) and width (TEXTURE_EXPR) left and right, path, intensity/capacity left/right
    9,  // energy color (VALUE_EXPR) and width (TEXTURE_EXPR), path, intensity/capacity unidirectional
    10, // control (VALUE_EXPR)
    11, // points, basic layer
    21: // POI
      RegisterOnTiler(False, SliceType, name);
  end;
end;

procedure TUSLayer.RegisterSlice;
begin
  case fLayerType of
    1:   tilerLayer.signalAddSlice(fPalette.Clone); // receptors
    //2:; grid
    3,8: tilerLayer.signalAddSlice(fPalette.Clone); // buildings, RS buildings
    4:   tilerLayer.signalAddSlice(fPalette.Clone); // road color (VALUE_EXPR) unidirectional
    5:   tilerLayer.signalAddSlice(fPalette.Clone); // road color (VALUE_EXPR) and width (TEXTURE_EXPR) left and right
    9:   tilerLayer.signalAddSlice(fPalette.Clone); // energy color (VALUE_EXPR) and width (TEXTURE_EXPR)
    10,11:  tilerLayer.signalAddSlice(fPalette.Clone); // points, basic layer
    21: // POI
      begin
        // todo: does not work like this!!! TPicture <> TPngImage.. order of id..
        {
        for poi in fPoiCategories.Values do
        begin
          //
          if poi.ID>=length(poiImages)
          then setLength(poiImages, poi.ID+1);
          poiImages[poi.ID] := poi.picture;
        end;
        tilerLayer.signalAddPOISlice(poiImages);
        }
      end;
  end;
  tilerLayer.signalSliceAction(tsaClearSlice); //force clear of our current slice
end;

function TUSLayer.SliceType: Integer;
begin
  case fLayerType of
    1:   Result := stReceptor; // receptors
    //2:; grid
    3,8: Result := stGeometry; // buildings, RS buildings
    4:   Result := stGeometryI; // road color (VALUE_EXPR) unidirectional
    5:   Result := stGeometryICLR; // road color (VALUE_EXPR) and width (TEXTURE_EXPR) left and right
    9:   Result := stGeometryIC; // energy color (VALUE_EXPR) and width (TEXTURE_EXPR)
    10, 11:  Result := stLocation;  // controls, points: basic layer
    21:  Result := stPOI; // POI
  else
         Result := stUndefined;
  end;
end;

{ TUSScenario }

constructor TUSScenario.Create(aProject: TProject; const aID, aName, aDescription, aFederation: string; aAddBasicLayers: Boolean; aMapView: TMapView;
  aIMBConnection: TIMBConnection; const aTablePrefix: string);
begin
  fTablePrefix := aTablePrefix;
  fIMBConnection := aIMBConnection;
  fLayerRefreshEvent := fIMBConnection.Publish(
    (aProject as TUSProject).OraSession.Username+
    aTableprefix.Substring(aTableprefix.Length-1)+ // #
    aTableprefix.Substring(0, aTablePrefix.length-1)+
    '.'+'TilerLayers', false);
  fUSChartGroups := TObjectList<TUSChartGroup>.Create(True);
  inherited Create(aProject, aID, aName, aDescription, aFederation, aAddbasicLayers, aMapView);
end;

destructor TUSScenario.Destroy;
begin
  if Assigned(fLayerRefreshEvent) then
  begin
    fLayerRefreshEvent.UnPublish;
    fLayerRefreshEvent := nil;
  end;
  inherited;
  FreeAndNil(fUSChartGroups);
end;

function TUSScenario.GetUSControlsJSON: string;
var
  layerBase: TLayerBase;
  controlsLayer: TUSControlsLayer;
  simpleObject: TSimpleObject;
  controlObject: TUSControlObject;
  usControl: TUSControl;
begin
  Result := '';
  controlsLayer := nil;
  TMonitor.Enter(fLayers);
  try
    for layerBase in fLayers.Values do
      if layerBase is TUSControlsLayer then
        controlsLayer := (layerBase as TUSControlsLayer);
  finally
    TMonitor.Exit(fLayers);
  end;
  if Assigned(controlsLayer) then
  begin
    TMonitor.Enter((fProject as TUSProject).USControls); //todo: switch to read locking
    try
      TMonitor.Enter(controlsLayer.objects);
      try
        for usControl in (fProject as TUSProject).USControls.Values do
        begin
          if Result <> '' then
            Result := Result + ',';
          if controlsLayer.objects.TryGetValue(usControl.ID, simpleObject) and (simpleObject is TUSControlObject) then
          begin
            controlObject := (simpleObject as TUSControlObject);
            Result := Result + '"' + string(usControl.ID) + '":{"active":' + controlObject.Active.ToString + ', "name": "' + usControl.Description + '"}';
          end
          else
          begin
            Result := Result + '"' + string(usControl.ID) + '":{"active": false, "name": "' + usControl.Description + '"}';
          end;
        end;
      finally
        TMonitor.Exit(controlsLayer.objects);
      end;
    finally
      TMonitor.Exit((fProject as TUSProject).USControls);
    end;
  end
  else
  begin
    //todo: read controls info from database instead of layer?
  end;
  Result := '{' + Result + '}';
end;

function TUSScenario.HandleClientSubscribe(aClient: TClient): Boolean;
begin
  Result := inherited;
  SendUSControlsMessage(aClient);
end;

procedure TUSScenario.LayerRefreshed(aTilerLayerID: Integer; const aElementID: string; aTimeStamp: TDateTime; aLayer: TSubscribeObject);
begin
  if Assigned(fLayerRefreshEvent)
  then fLayerRefreshEvent.SignalChangeObject(actionChange, aTilerLayerID, aElementID);
end;

procedure TUSScenario.ReadBasicData;
var
  mlp: TPair<Integer, TMetaLayerEntry>;
  layer: TLayerBase;
  indicTableNames: TAllRowsSingleFieldResult;
  oraSession: TOraSession;
  metaLayer: TMetaLayer;
  connectString: string;
  sourceProjection: TGIS_CSProjectedCoordinateSystem;
  dom: string;
  nam: string;
begin
  oraSession := (project as TUSProject).OraSession;
  connectString := ConnectStringFromSession(oraSession);
  sourceProjection := (project as TUSProject).sourceProjection;

  //process basic layers
  if addBasicLayers then
  begin
    ReadBasicLayers(oraSession);
  end;

  // process meta layer to build list of available layers
  metaLayer := TMetaLayer.Create;
  try
    ReadMetaLayer(oraSession, fTableprefix, metaLayer);
    Log.WriteLn(elementID+': found '+metaLayer.Count.ToString+' layers in meta_layer');
    for mlp in metaLayer do
    begin
      if mlp.Value._published>0 then
      begin
          nam := mlp.Value.description;
          if nam=''
          then nam := FormatLegendDescription(mlp.value.LEGEND_DESC);

          dom := mlp.Value.domain;
          if dom=''
          then dom := 'general';

          if (dom<>'') and (nam<>'') then
          begin
            //todo: check if fix from name to nam worked!?
            layer := mlp.Value.CreateUSLayer(self, fTablePrefix, connectString, SubscribeUSDataEvents(oraSession.Username, mlp.Value.IMB_EVENTCLASS, fTablePrefix, fIMBConnection), sourceProjection, dom, nam, 0.8, false);
            if Assigned(layer) then
            begin
              Layers.Add(layer.ID, layer);
              Log.WriteLn(elementID+': added layer '+layer.ID+', '+layer.domain+'/'+layer.description, llNormal, 1);
              // schedule reading objects and send to tiler
              if layer is TUSLayer
              then AddCommandToQueue(oraSession, (layer as TUSLayer).ReadObjects)
              else if layer is TUSControlsLayer
              then AddCommandToQueue(oraSession, (layer as TUSControlsLayer).ReadObjects)
            end
            else Log.WriteLn(elementID+': skipped layer ('+mlp.Key.ToString+') type '+mlp.Value.LAYER_TYPE.ToString+' '+mlp.Value.LAYER_TABLE+'-'+mlp.Value.VALUE_EXPR, llRemark, 1);
          end;
      end;
    end;

    // process indicators
    indicTableNames := ReturnAllFirstFields(oraSession,
      'SELECT DISTINCT name FROM '+
      '( '+
      'SELECT table_name AS name '+
      'FROM user_tables '+       // aScenarioPrefix is case-sensitive
      'WHERE table_name LIKE '''+FTablePrefix+'%\_INDIC\_DEF%'' ESCAPE ''\'' '+
      ') '+
      'UNION '+
      '( '+
      'SELECT view_name AS name '+
      'FROM user_views '+
      'WHERE view_name LIKE '''+FTablePrefix+'%\_INDIC\_DEF%'' ESCAPE ''\'' '+
      ') '+
      'ORDER BY name');
    ReadIndicators(indicTableNames, oraSession);
    Log.WriteLn(elementID+': finished building scenario');
  finally
    metaLayer.Free;
  end;
end;

procedure TUSScenario.ReadBasicLayers(aOraSession: TOraSession);
var
  metaObjectEntries: TMetaObjectEntries;
  metaObjectProperties: TMetaObjectProperties;
  metaObjectEntry: TMetaObjectEntry;
  metaObjectPropertyEntry: TMetaObjectPropertyEntry;
  layer: TUSBasicLayer;
  newQuery, changeQuery: string;
begin
  metaObjectEntries := TMetaObjectEntries.Create;
  try
    metaObjectProperties := TMetaObjectProperties.Create;
    try
      if ReadMetaObject(aOraSession, fTablePrefix, metaObjectEntries) then
      begin
        ReadMetaObjectProperty(aOraSession, fTablePrefix, metaObjectProperties);
        for metaObjectEntry in metaObjectEntries do
        begin
          if metaObjectEntry._published > 0 then
          begin
            if metaObjectEntry.TABLE_FILTER <> '' then
            begin
              newQuery := 'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM ' + fTablePrefix.ToUpper + metaObjectEntry.TABLE_NAME + ' t WHERE ' + metaObjectEntry.TABLE_FILTER;
              changeQuery := 'SELECT OBJECT_ID, 0 AS VALUE, SHAPE FROM ' + fTablePrefix.ToUpper + metaObjectEntry.TABLE_NAME + ' WHERE ' + metaObjectEntry.TABLE_FILTER + ' AND OBJECT_ID IN ';
            end
            else
            begin
              newQuery := 'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM ' + fTablePrefix.ToUpper + metaObjectEntry.TABLE_NAME + ' t';
              changeQuery := 'SELECT OBJECT_ID, 0 AS VALUE, SHAPE FROM ' + fTablePrefix.ToUpper + metaObjectEntry.TABLE_NAME + ' WHERE OBJECT_ID IN ';
            end;

            layer := TUSBasicLayer.Create(Self,
              standardIni.ReadString('domains', metaObjectEntry.OBJECT_TYPE, 'basic structures'), //  domain
              metaObjectEntry.OBJECT_TYPE, metaObjectEntry.LAYER_DESCRIPTION, metaObjectEntry.LAYER_DESCRIPTION, false,
              '"'+metaObjectEntry.OBJECT_TYPE+'"', metaObjectEntry.GEOMETRY_TYPE, GetUSBasicLayerType(metaObjectEntry.GEOMETRY_TYPE), NaN,
              ConnectStringFromSession(aOraSession),
              newQuery, //new query
              changeQuery, //change query
              SubscribeUSDataEvents(aOraSession.Username, metaObjectEntry.TABLE_NAME, fTablePrefix, fIMBConnection),
              (project as TUSProject).sourceProjection,
              TDiscretePalette.Create('basic palette', [], TGeoColors.Create(colorBasicOutline)),
              metaObjectEntry.TABLE_NAME);
            for metaObjectPropertyEntry in metaObjectProperties do
              if metaObjectEntry.OBJECT_ID = metaObjectPropertyEntry.META_OBJECT_ID then
                layer.ObjectProperties.AddFromObjectPropertyEntry(metaObjectPropertyEntry);
            layer.query := newQuery;
            Layers.Add(layer.ID, layer);
            Log.WriteLn(elementID+': added layer '+layer.ID+', '+layer.domain+'/'+layer.description, llNormal, 1);
            AddCommandToQueue(aOraSession, layer.ReadObjects);
          end;
        end;
      end;
    finally
      metaObjectProperties.Free;
    end;
  finally
    metaObjectEntries.Free;
  end;
end;

procedure TUSScenario.ReadIndicator(aTableName: string; aOraSession: TOraSession);
var
  defQuery, datTableName: string;
  defResult: TAllRowsResults;
  tab, gridPrefix: string;
  dataCols: TStringList;
  lines: TDictionary<string, string>;
  singleRowResult: TSingleRowResult;
  gridWidth, gridHeight, i, j: Integer;
  uscharts: TUSChartGroup;
  indicatorEvent: TIMBEventEntry;
  imbEventName: string;
begin
  defQuery := 'SELECT * FROM ' + aTableName;
  defResult := ReturnAllResults(aOraSession, defQuery);
  datTableName := aTableName.Replace('_DEF', '_DAT');
  uscharts := TUSChartGroup.Create(Self, aTableName, datTableName);
  dataCols := TStringList.Create;
  dataCols.Duplicates := TDuplicates.dupIgnore;
  lines := TDictionary<string, string>.Create;

  for singleRowResult in defResult do
    lines.Add(singleRowResult[0], singleRowResult[1]);


  if lines.ContainsKey('Tab') then
    tab := lines['Tab']
  else
    tab := 'unknown tab';

  //check size of active indicator grid, if none is specified we assume 1x1
  if lines.ContainsKey('Grid') then
  begin
    gridWidth := length(lines['Grid'].Split(['*'])[0].Split([',']));
    gridHeight := length(lines['Grid'].Split(['*'])[1].Split([',']));
  end
  else
  begin
    gridWidth := 1;
    gridHeight := 1;
  end;

  // loop over our grid
  for i := 1 to gridWidth do
    for j := 1 to gridHeight do
      begin
        gridPrefix := 'Grid' + IntToStr(i) + ',' + IntToStr(j) + '$';

        if lines.ContainsKey(gridPrefix + 'GraphicType') and (lines[gridPrefix + 'GraphicType'] = 'TChart') then
        begin
          if ((not lines.ContainsKey(gridPrefix + 'Enabled')) or (lines[gridPrefix + 'Enabled'] = ' True')) then
            uscharts.Charts.Add(TUSChart.Create(uscharts, Self, lines, gridPrefix, tab, tab + ' (' + inttostr(i) + '-' + inttostr(j) + ')', aTableName));
        end
        else
          continue;
      end;
  uscharts.ReadDataFromDB;
  for i := 0 to uscharts.Charts.Count - 1 do
  begin
    AddChart(uscharts.Charts[i]);
  end;
  imbEventName := aOraSession.UserName + '#' + datTableName.Split(['#'])[0] + '.' + datTableName.Split(['#'])[1]; //assume always contains #
  while (Length(imbEventName) > 0) and TryStrToInt(imbEventName[Length(imbEventName)], i) do //todo better check for numbers?
    SetLength(imbEventName,Length(imbEventName)-1);
  indicatorEvent := fIMBConnection.Subscribe(imbEventName, false);
  uscharts.SetEvent(indicatorEvent);
  fUSChartGroups.Add(uscharts);
end;

procedure TUSScenario.ReadIndicators(aTableNames: array of string; aOraSession: TOraSession);
var
  tableName: string;
begin
  for tableName in aTableNames do
    ReadIndicator(tableName, aOraSession);
end;

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories, aSelectedIDs: TArray<string>): string;
begin
  // todo: implement
  Result := '';
end;

function TUSScenario.selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string;
var
  layer: TLayerBase;
  basicLayer: TUSBasicLayer;
  oraSession: TOraSession;
  ids, id: string;
  propertyBuilder: TUSPropBuilder;
begin
  Result := '';
  if length(aSelectCategories) > 0 then
  begin
    TMonitor.Enter(fLayers);
    try
      fLayers.TryGetValue(aSelectCategories[0], layer);
    finally
      TMonitor.Exit(fLayers);
    end;
    if Assigned(layer) and (layer is TUSBasicLayer) then
    begin
      basicLayer := (layer as TUSBasicLayer);
      oraSession := (project as TUSProject).OraSession;
      try
        propertyBuilder := TUSPropBuilder.Create(basicLayer.ObjectProperties);
        propertyBuilder.BuildProperties(oraSession, aSelectedObjects);
        for id in aSelectedObjects do
        begin
          if ids <> '' then
            ids := ids + ',';
          ids := ids + '"' +id + '"';
        end;
        Result := '{"selectedObjectsProperties":'+
              '{'+
                '"selectedCategories": ["'+layer.ID+'"],'+
                '"properties":['+propertyBuilder.GetJSON+'],'+
                '"selectedObjects":['+ids+']'+
              '}'+
            '}';
      finally
        FreeAndNil(propertyBuilder);
      end;
    end;
  end;
end;

procedure TUSScenario.SendUSControlsMessage(aClient: TClient);
begin
  aClient.signalString('{"type":"scenarioControlsMessage", "payload": { "scenarioControls": ' + GetUSControlsJSON + '}}');
end;

procedure TUSScenario.UpsertUSControlStatus(const aControlID, aActive: Integer);

  procedure ExcecuteUpsert(const aOraSession: TOraSession; const aIDs: string);
  begin
    aOraSession.ExecSQL(
        'MERGE INTO ' + TablePrefix + 'GENE_CONTROL t1' +
        ' using (SELECT OBJECT_ID, :ACTIVE as ACTIVE FROM GENE_CONTROL WHERE OBJECT_ID in (' + aIDs + ')) t2' +
        ' ON' +
        ' (t1.OBJECT_ID = t2.OBJECT_ID)' +
        ' WHEN MATCHED then' +
        ' UPDATE SET ACTIVE = t2.ACTIVE' +
        ' WHEN not matched then' +
        ' INSERT (OBJECT_ID, ACTIVE)' +
        ' VALUES (t2.OBJECT_ID, t2.ACTIVE)'
        , [aActive]);
  end;

var
  query: TOraQuery;
  oraSession: TOraSession;
  idList: TList<Integer>;
  currentIDs: string;
  currentID: Integer;
  i: Integer;
  table: TSubscribeObject;
  publishEvent: TIMBEventEntry;
  publishEventName: string;
begin
  //procedure might go into infinite loop if db is currupt (2 controls with eachother as parent id)
  oraSession := (project as TUSProject).oraSession;
  idList := TList<Integer>.Create;
  idList.Add(aControlID);
  currentIDs := aControlID.ToString;
  query := TOraQuery.Create(nil);
  try
    query.Session := oraSession;
    while currentIDs <> '' do
    begin
      query.SQL.Text := 'SELECT OBJECT_ID FROM GENE_CONTROL WHERE PARENT_ID in (' + currentIDs + ')';
      currentIDs := '';
      query.ExecSQL;
      while not query.Eof do
      begin
        currentID := query.FieldByName('OBJECT_ID').AsInteger;
        if currentIDs <> '' then
          currentIDs := currentIDs + ',';
        currentIDs := currentIDs + currentID.ToString;
        idList.Add(currentID);
        query.Next;
      end;
    end;
  finally
    query.Free
  end;
  currentIDs := '';
  for I := 0 to idList.Count - 1 do
  begin
    if currentIDs <> '' then
      currentIDs := currentIDs + ',';
    currentIDs := currentIDs + idList[i].ToString;
    if (i mod 1000) = 999 then
    begin
      ExcecuteUpsert(oraSession, currentIDs);
      currentIDs := '';
    end;
  end;
  if currentIDs <> '' then
    ExcecuteUpsert(oraSession, currentIDs);
  oraSession.Commit;
  publishEventName := Federation + '.GENE_CONTROL';
  table := (project as TUSProject).GetTableSync(publishEventName);
  publishEvent := (project as TUSProject).IMB3Connection.publish(publishEventName, false);
  try
    for i := 0 to idList.Count - 1 do
    begin
      publishEvent.SignalChangeObject(actionChange, idList[i], 'ACTIVE');
      table.SendAnonymousEvent(actionChange, idList[i]);
    end;

  finally
    publishEvent.UnPublish();
  end;
end;

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string;
var
  layers: TList<TLayerBase>;
begin
  Result := '';
  layers := TList<TLayerBase>.Create;
  try
    if selectLayersOnCategories(aSelectCategories, layers) then
    begin
      if aMode='+' then
      begin
        // only use first layer (only 1 type of object allowed..)
        // todo: warning if more then 1 layer?

      end
      else
      begin
        // select objects in layer that match first

      end;
    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

{ TUSDBScenario }

constructor TUSDBScenario.Create(const aID, aName, aDescription, aParentID, aReferenceID, aTablePrefix, aIMBPrefix, aStatus: string; aPublished: Integer);
begin
  inherited Create;
  fID := aID;
  fName := aName;
  fDescription := aDescription;
  fParentID := aParentID;
  fParentName := '';
  fReferenceID := aReferenceID;
  fReferenceName := '';
  fTablePrefix := aTablePrefix;
  fIMBPrefix := aIMBPrefix;
  fStatus := aStatus;
  fPublished := aPublished;
end;

procedure TUSDBScenario.Relink(aUSDBScenarios: TObjectDictionary<string, TUSDBScenario>);
var
  s: TUSDBScenario;
begin
  if aUSDBScenarios.TryGetValue(fParentID, s)
  then fParentName := s.name;
  if aUSDBScenarios.TryGetValue(fReferenceID, s)
  then fReferenceName := s.name;
end;

{ TUSProject }

constructor TUSProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios, aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aSourceEPSG: Integer);
var
  SourceEPSGstr: string;
  SourceEPSG: Integer;
begin
  fIMB3Connection := aIMB3Connection;
  fUSDBScenarios := TObjectDictionary<string, TUSDBScenario>.Create;
  fUSControls := TObjectDictionary<string, TUSControl>.Create([doOwnsValues]);
  fUSTableSync := TObjectDictionary<string, TSubscribeObject>.Create([doOwnsValues]);
  mapView := aMapView;

  if aSourceEPSG>0
  then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(aSourceEPSG)
  else
  begin
    SourceEPSGstr := GetSetting(SourceEPSGSwitch, 'Amersfoort_RD_New');
    if SourceEPSGstr<>'' then
    begin
      SourceEPSG := StrToIntDef(SourceEPSGstr, -1);
      if SourceEPSG>0
      then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(SourceEPSG)
      else fSourceProjection := CSProjectedCoordinateSystemList.ByWKT(SourceEPSGstr);
    end
    else fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  end;
  if Assigned(fSourceProjection)
  then Log.WriteLn('Using source projection: '+fSourceProjection.FriendlyName)
  else Log.WriteLn('NO source projected defined', llWarning);

  // todo: use center of projection if default location for map is not defined?
  // fSourceProjection.Projection.LatitudeOfCenter fSourceProjection.Projection.LongitudeOfCenter,

  fPreLoadScenarios := aPreLoadScenarios;

  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName,
    aTilerFQDN, aTilerStatusURL, aDataSource,
    aDBConnection, aAddBasicLayers,
    aMaxNearestObjectDistanceInMeters, mapView);
  {
  ClientMessageHandlers.Add('measure',
    procedure(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONObject)
    var
      formID: string;
      form: TFormDialog;
    begin
      formID := TGUID.NewGuid.ToString;
      form := TFormDialog.Create(formID, 'Test dialog');
      try
        //form.Properties.Add(TFormDialogProperty.Create('een', 'label een', 'string', 'input', 'default', True, True));
        //form.Properties.Add(TFormDialogProperty.Create('twee', 'label twee', 'string'));
        form.AddPropertyInput('input', 'input label', 'def');
        form.AddPropertyRadio('radio', 'radio label', 'r1', ['r-1', 'r0', 'r1']);
        form.AddPropertySelect('select', 'select label', 's0', ['s-1', 's0', 's1']);
        form.AddPropertyCheck('check', 'check label', ['c1', 'c2', 'c3']);
        form.AddPropertySlider('slider', 'slider label', 10, 70, 5, '%');
        form.Open(aClient, TFormDialogResultHandling.Create(
          procedure(aClient: TClient; aResult: TFormDialogResults)
          var
            r: TPair<string, TFormDialogResultProperty>;
          begin
            Log.WriteLn('Form results for '+aResult.FormID);
            for r in aResult.Properties
            do Log.WriteLn(r.Key+' ('+r.Value.&Type+') = '+r.Value.Value, llNormal, 2);
            if Assigned(aResult.Context)
            then Log.WriteLn('Context: '+aResult.Context.toJSON, llNormal, 1)
            else Log.WriteLn('No context..', llNormal, 1);
          end));
      finally
        form.Free;
      end;
    end);
  }
  fUpdateQueue := TList<TUSUpdateQueueEntry>.Create;
  fUpdateQueueEvent := TEvent.Create(nil, False, False, '');
  fUpdateThread := TThread.CreateAnonymousThread(HandleControlsQueueEvent);
  fUpdateThread.FreeOnTerminate := False;
  fUpdateThread.NameThreadForDebugging('Project controls queue handler');
  fUpdateThread.Start;
  fControlsUpdateEvent := fIMB3Connection.Subscribe(OraSession.Username+
          '.GENE_CONTROL', False);
  fControlsUpdateEvent.OnChangeObject := HandleControlsUpdate;
  SubscribeTo(GetTableSync('GENE_CONTROL'), handleInternalControlsChange);
  clientMessageHandlers.AddOrSetValue('measure', HandleClientMeasureMessage);
  clientMessageHandlers.AddOrSetValue('graphLabelClick', HandleClientGraphLabelClick);
  clientMessageHandlers.AddOrSetValue('changeControlProperties', HandleControlPropertyChange);
end;

destructor TUSProject.Destroy;
begin
  FreeAndNil(fScenarioLinks);
  inherited;
  FreeAndNil(fUSDBScenarios);
  FreeAndNil(fUSControls);
  FreeAndNil(fUSTableSync);
end;

function TUSProject.FindMeasure(const aActionID: string;
  out aMeasure: TMeasureAction): Boolean;
var
  measureCategory: TMeasureCategory;
begin
  Result := False;
  TMonitor.Enter(fMeasures);
  try
    for measureCategory in fMeasures.Values do
    begin
      if measureCategory.FindMeasure(aActionID, aMeasure) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    TMonitor.Exit(fMeasures);
  end;
end;

function TUSProject.getOraSession: TOraSession;
begin
  Result := fDBConnection as TOraSession;
end;

function TUSProject.GetTableSync(const aUSTableName: string): TSubscribeObject;
begin
  TMonitor.Enter(fUSTableSync);
  try
    if not fUSTableSync.TryGetValue(aUSTableName.ToUpper, Result) then
    begin
      Result := TSubscribeObject.Create;
      fUSTableSync.Add(aUSTableName.ToUpper, Result);
    end;
  finally
    TMonitor.Exit(fUSTableSync);
  end;
end;

function TUSProject.GetUSControlJSONFromDB(aTablePrefix: string): string;
var
  query: TOraQuery;
 Table: string;
 ID: Integer;
 Active: Boolean;
begin
  Result := '';
  Table := aTablePrefix + 'GENE_CONTROL';
  if TableExists(OraSession, Table) then
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := OraSession;
      query.SQL.Text := 'SELECT * FROM ' + Table;
      query.Open;
      while not query.EoF do
      begin
        ID := query.FieldByName('OBJECT_ID').AsInteger;
        Active := query.FieldByName('ACTIVE').AsInteger = 1;
        if Result <> '' then
          Result := Result + ',';
        Result := Result + '"' + ID.ToString + '":{"active":' + Active.ToString + '}';
        query.Next;
      end;
    finally
      query.Free;
    end;
  end;
  Result := '{' + Result + '}';
end;

function TUSProject.GetUSControlsJSON: string;
var
  FoundScenario: Boolean;
  USControl: TUSControl;
  Scenario: TScenario;
  USDBScenario: TUSDBScenario;
  ScenariosJSON, ControlsJSON: string;
  ScenarioControlsJSON, ScenarioName, ScenarioID: string;
begin
  //TODO: duplicate code from TUSProject.handleNewClient -> rewrite to use the same code
  ControlsJSON := '';
  TMonitor.Enter(fUSControls);
  try
    for USControl in fUSControls.Values do
    begin
      if ControlsJSON <> '' then
        ControlsJSON := ControlsJSON + ',';
      ControlsJSON := ControlsJSON + '"' + string(USControl.ID) + '":{' +
        '"name":"' + USControl.Name + '",' +
        '"description":"' + USControl.Description + '",' +
        '"lat":' + DoubleToJSON(USControl.Lat) + ',' +
        '"lon":' + DoubleToJSON(USControl.Lon) + '}';
    end;
  finally
    TMonitor.Exit(fUSControls);
  end;
  ControlsJSON := '{' + ControlsJSON + '}';
  ScenariosJSON := '';
  for USDBScenario in fUSDBScenarios.Values do
  begin
    if USDBScenario._published > 0 then
    begin
      FoundScenario := False;
      if ScenariosJSON <> '' then
        ScenariosJSON := ScenariosJSON + ',';
      TMonitor.Enter(fScenarios);
      try
        if fScenarios.TryGetValue(USDBScenario.fID, Scenario) then
          FoundScenario := True;
      finally
        TMonitor.Exit(fScenarios);
      end;
      if FoundScenario then
      begin

        ScenarioControlsJSON := (scenario as TUSScenario).GetUSControlsJSON;
        ScenarioName := scenario.Name;
        ScenarioID := scenario.Id;
      end
      else
      begin
        ScenarioControlsJSON := GetUSControlJSONFromDB(USDBScenario.tablePrefix);
        ScenarioName := USDBScenario.Name;
        ScenarioID := USDBScenario.Id;
      end;
      ScenariosJSON := ScenariosJSON + '"' + ScenarioID + '":{ "name": "' + ScenarioName + '", "controls": ' + ScenarioControlsJSON + '}';
    end;
  end;
  ScenariosJSON := '{' + ScenariosJSON + '}';
  Result := '{"controls":' + ControlsJSON + ', "scenarios":' + ScenariosJSON + '}';
end;

procedure TUSProject.HandleClientMeasureMessage(aProject: TProject;
  aClient: TClient; const aType: string; aPayload: TJSONObject);
var
  layer: TLayerBase;
  basicLayer: TUSBasicLayer;
  obj: TLayerObjectWithID;
  geometry: TWDGeometry;
  applyObject: TJSONObject;
  selectCategories, selectedObjects: TJSONArray;
  jsonArrayItem, jsonProperty: TJSONValue;
  jsonProperties: TJSONArray;
  id, propID, propValue: string;
  point, point1, point2: TGIS_Point;
  lat, lon: Double;
  measure: TMeasureAction;
  propertyDictionary: TDictionary<string, string>;
  objects: TDictionary<string, Integer>;
  foundLeft, foundRight: Boolean;
  leftID, rightID, mainID, curMaxID, parentIDValue: Integer;
  query: TOraQuery;
  controlName, controlDescription: string;
  adjustedIDs: TDictionary<Integer, Integer>;
  adjustedID, i, j: Integer;
  publishEvent: TIMBEventEntry;
  publishEventName: string;
  table: TSubscribeObject;
  fromID, toID: Integer;
  turncostID: Variant;
  fRoadfNode, fRoadtNode, tRoadfNode, tRoadtNode: Variant;
  nodeA, nodeB, nodeC: Variant;
  connectedRoads: Boolean;
  roadTableName, turncostTableName: string;
begin
  if aPayload.TryGetValue<TJSONObject>('apply', applyObject) then
  begin
    adjustedIDs := TDictionary<Integer, Integer>.Create();
    try
      leftID := -1; //to prevent compiler warnings
      rightID := -1;
      mainID := -1;
      if applyObject.TryGetValue<string>('id', id) and FindMeasure(id, measure) and
        applyObject.TryGetValue<TJsonArray>('parameters', jsonProperties) and
        applyObject.TryGetValue<Double>('lat', lat) and
        applyObject.TryGetValue<Double>('lon', lon) and
        applyObject.TryGetValue<TJSONArray>('selectedObjects', selectedObjects) and
        applyObject.TryGetValue<TJSONArray>('selectCategories', selectCategories) then
      begin
        point.X := lon;
        point.Y := lat;
        point := sourceProjection.FromGeocs(point);
        propertyDictionary := TDictionary<string, string>.Create;
        try
          begin
            for jsonProperty in jsonProperties do
              if jsonProperty.TryGetValue<string>('id', propID) and jsonProperty.TryGetValue<string>('value', propValue) then
              begin
                if propValue <> '' then
                  propertyDictionary.AddOrSetValue(propID, propValue);
              end;
          end;
          if not propertyDictionary.TryGetValue('Name', controlName) then
            controlName := 'Unknown name';
          if not propertyDictionary.TryGetValue('Description', controlDescription) then
            controlDescription := 'Unknown description';
          objects := TDictionary<string, Integer>.Create;
          try
            foundLeft := False;
            foundRight := False;
            for jsonArrayItem in selectedObjects do
            begin
              if jsonArrayItem.Value.StartsWith('L-') then
              begin
                foundLeft := True;
                id := jsonArrayItem.Value.Substring(2);
                objects.AddOrSetValue(id, -1);
              end
              else if jsonArrayItem.Value.StartsWith('R-') then
              begin
                foundRight := True;
                id := jsonArrayItem.Value.Substring(2);
                objects.AddOrSetValue(id, 1);
              end
              else
              begin
                //todo: implement found normal -> when they require unsided properties
                id := jsonArrayItem.Value;
                objects.AddOrSetValue(id, 0);
              end;
            end;
            if (measure.actionID < -100) and (measure.actionID > -110) then
            begin
              adjustedIDs.Clear;
              oraSession.StartTransaction;
              try
                //lock the table so we can extract highest id and use that to determine ids of the new controls
                query := TOraQuery.Create(nil);
                try
                  query.Session := oraSession;

                  if (objects.Count > 0) and (selectCategories.Count > 0) and Assigned(aClient.currentScenario) then
                  begin
                    TMonitor.Enter(aClient.currentScenario.Layers);
                    try
                      aClient.currentScenario.Layers.TryGetValue(selectCategories.Items[0].Value, layer);
                    finally
                      TMonitor.Exit(aClient.currentScenario.Layers);
                    end;
                    if Assigned(layer) and (layer is TUSBasicLayer) then
                    begin
                      basicLayer := (layer as TUSBasicLayer);
                      basicLayer.objectsLock.BeginRead;
                      try
                        if basicLayer.objects.TryGetValue(AnsiString(id), obj) then
                        begin
                          if (obj is TGeometryLayerObject) then
                          begin
                            geometry := (obj as TGeometryLayerObject).geometry;
                            if geometry.parts.Count > 0 then
                            begin
                              i := Ceil(geometry.parts.Count / 2) - 1;
                              if geometry.parts[i].points.Count = 1 then
                              begin
                                point.X := geometry.parts[i].points[0].y;
                                point.Y := geometry.parts[i].points[0].x;
                                point := sourceProjection.FromGeocs(point);
                              end
                              else if geometry.parts[i].points.Count > 1 then
                              begin
                                j := Ceil(geometry.parts[i].points.Count / 2) - 1;
                                point1.X := geometry.parts[i].points[j].x;
                                point1.Y := geometry.parts[i].points[j].y;
                                point2.X := geometry.parts[i].points[j+1].x;
                                point2.Y := geometry.parts[i].points[j+1].y;
                                point1 := sourceProjection.FromGeocs(point1);
                                point2 := sourceProjection.FromGeocs(point2);
                                point.X := (point1.X + point2.X) / 2;
                                point.Y := (point1.Y + point2.Y) / 2;
                              end;
                            end;
                          end;
                        end;
                      finally
                        basicLayer.objectsLock.EndRead;
                      end;
                    end;
                  end;
                  query.SQL.Text := 'LOCK TABLE GENE_CONTROL IN EXCLUSIVE MODE';
                  query.ExecSQL;
                  query.SQL.Text := 'SELECT MAX(OBJECT_ID) as NEWID FROM GENE_CONTROL';
                  query.ExecSQL;
                  if query.FindFirst then
                  begin
                    curMaxID := query.FieldByName('NEWID').AsInteger;
                    if propertyDictionary.ContainsKey('ParentID') then
                    begin
                      if Integer.TryParse(propertyDictionary['ParentID'], parentIDValue) then
                      begin
                        if propertyDictionary.ContainsKey('Speed')
                          or propertyDictionary.ContainsKey('Capacity')
                          or propertyDictionary.ContainsKey('Lanesmask') then
                        begin
                          query.SQL.Text := 'INSERT INTO GENE_CONTROL (OBJECT_ID, NAME, DESCRIPTION, X, Y, PARENT_ID) VALUES (:OBJECT_ID, :NAME, :DESCRIPTION, :X, :Y, :PARENT_ID)';
                          query.ParamByName('X').Value := point.X;
                          query.ParamByName('Y').Value := point.Y;
                          query.ParamByName('PARENT_ID').Value := parentIDValue;
                          if foundLeft then
                          begin
                            curMaxID := curMaxID + 1;
                            leftID := curMaxID;
                            query.ParamByName('OBJECT_ID').Value := leftID;
                            if foundRight then
                            begin
                              query.ParamByName('NAME').Value := controlName + '-L';
                              query.ParamByName('DESCRIPTION').Value := controlDescription + '-L';
                            end
                            else
                            begin
                              query.ParamByName('NAME').Value := controlName;
                              query.ParamByName('DESCRIPTION').Value := controlDescription;
                            end;
                            query.ExecSQL;
                            adjustedIDs.Add(leftID, actionNew);
                          end;
                          if foundRight then
                          begin
                            curMaxID := curMaxID + 1;
                            rightID := curMaxID;
                            query.ParamByName('OBJECT_ID').Value := rightID;
                            if foundLeft then
                            begin
                              query.ParamByName('NAME').Value := controlName + '-R';
                              query.ParamByName('DESCRIPTION').Value := controlDescription + '-R';
                            end
                            else
                            begin
                              query.ParamByName('NAME').Value := controlName;
                              query.ParamByName('DESCRIPTION').Value := controlDescription;
                            end;
                            query.ExecSQL;
                            adjustedIDs.Add(rightID, actionNew);
                          end;
                        end;
                      end;
                    end
                    else
                    begin
                      if propertyDictionary.ContainsKey('Speed')
                        or propertyDictionary.ContainsKey('Capacity')
                        or propertyDictionary.ContainsKey('Lanesmask') then
                      begin
                        query.SQL.Text := 'INSERT INTO GENE_CONTROL (OBJECT_ID, NAME, DESCRIPTION, X, Y, PARENT_ID) VALUES (:OBJECT_ID, :NAME, :DESCRIPTION, :X, :Y, :PARENT_ID)';
                        query.ParamByName('X').Value := point.X;
                        query.ParamByName('Y').Value := point.Y;
                        if foundLeft and foundRight then
                        begin
                          //create parent control
                          curMaxID := curMaxID + 1;
                          mainID := curMaxID;
                          query.ParamByName('OBJECT_ID').Value := mainID;
                          query.ParamByName('NAME').Value := controlName;
                          query.ParamByName('DESCRIPTION').Value := controlDescription;
                          query.ParamByName('PARENT_ID').Clear;
                          query.ExecSQL;
                          adjustedIDs.Add(mainID, actionNew);
                          //create control left
                          curMaxID := curMaxID + 1;
                          leftID := curMaxID;
                          query.ParamByName('OBJECT_ID').Value := leftID;
                          query.ParamByName('NAME').Value := controlName + '-L';
                          query.ParamByName('DESCRIPTION').Value := controlDescription + '-L';
                          query.ParamByName('PARENT_ID').Value := mainID;
                          query.ExecSQL;
                          adjustedIDs.Add(leftID, actionNew);
                          //create control right
                          curMaxID := curMaxID + 1;
                          rightID := curMaxID;
                          query.ParamByName('OBJECT_ID').Value := rightID;
                          query.ParamByName('NAME').Value := controlName + '-R';
                          query.ParamByName('DESCRIPTION').Value := controlDescription + '-R';
                          query.ParamByName('PARENT_ID').Value := mainID;
                          query.ExecSQL;
                          adjustedIDs.Add(rightID, actionNew);
                        end
                        else if foundLeft then
                        begin
                          curMaxID := curMaxID + 1;
                          leftID := curMaxID;
                          query.ParamByName('OBJECT_ID').Value := leftID;
                          query.ParamByName('NAME').Value := controlName;
                          query.ParamByName('DESCRIPTION').Value := controlDescription;
                          query.ParamByName('PARENT_ID').Clear;
                          query.ExecSQL;
                          adjustedIDs.Add(leftID, actionNew);
                        end
                        else if foundRight then
                        begin
                          curMaxID := curMaxID + 1;
                          rightID := curMaxID;
                          query.ParamByName('OBJECT_ID').Value := rightID;
                          query.ParamByName('NAME').Value := controlName;
                          query.ParamByName('DESCRIPTION').Value := controlDescription;
                          query.ParamByName('PARENT_ID').Clear;
                          query.ExecSQL;
                          adjustedIDs.Add(rightID, actionNew);
                        end;
                      end
                    end;
                    //done making controls, now set objects
                    query.SQL.Text := 'INSERT INTO GENE_CONTROL_OBJECTS (CONTROL_ID, OBJECT_ID, OBJECT_TABLE, SIDE, OBJECT_TYPE) VALUES (:CONTROL_ID, :OBJECT_ID, :OBJECT_TABLE, :SIDE, :OBJECT_TYPE)';
                    query.ParamByName('OBJECT_TABLE').Value := 'GENE_ROAD'; //for now can only use GENE_ROAD change when OTConnector is updated
                    if selectCategories.Count > 0 then
                      query.ParamByName('OBJECT_TYPE').Value := selectCategories.Items[0].Value
                    else
                      query.ParamByName('OBJECT_TYPE').Clear;
                    for id in objects.Keys do
                    begin
                      query.ParamByName('OBJECT_ID').Value := id;
                      if objects[id] = -1 then
                      begin
                        query.ParamByName('CONTROL_ID').Value := leftID;
                        query.ParamByName('SIDE').Value := -1;
                      end
                      else if objects[id] = 1 then
                      begin
                        query.ParamByName('CONTROL_ID').Value := rightID;
                        query.ParamByName('SIDE').Value := 1;
                      end
                      else
                      begin
                        query.ParamByName('CONTROL_ID').Value := mainID;
                        query.ParamByName('SIDE').Value := 0;
                      end;
                      query.ExecSQL;
                    end;
                    //done writing objects, now set properties
                    query.SQL.Text := 'INSERT INTO GENE_CONTROL_PROPERTIES (CONTROL_ID, FIELD, VALUE) VALUES (:CONTROL_ID, :FIELD, :VALUE)';
                    for propID in propertyDictionary.Keys do
                    begin
                      query.ParamByName('VALUE').Value := propertyDictionary[propID];
                      if propID = 'Speed' then
                      begin
                        if foundLeft then
                        begin
                          query.ParamByName('CONTROL_ID').Value := leftID;
                          query.ParamByName('FIELD').Value := 'SPEED_L';
                          query.ExecSQL;
                        end;
                        if foundRight then
                        begin
                          query.ParamByName('CONTROL_ID').Value := rightID;
                          query.ParamByName('FIELD').Value := 'SPEED_R';
                          query.ExecSQL;
                        end;
                      end
                      else if propID = 'Capacity' then
                      begin
                        if foundLeft then
                        begin
                          query.ParamByName('CONTROL_ID').Value := leftID;
                          query.ParamByName('FIELD').Value := 'CAPACITY_L';
                          query.ExecSQL;
                        end;
                        if foundRight then
                        begin
                          query.ParamByName('CONTROL_ID').Value := rightID;
                          query.ParamByName('FIELD').Value := 'CAPACITY_R';
                          query.ExecSQL;
                        end;
                      end
                      else if propID = 'Lanesmask' then
                      begin
                        if foundLeft then
                        begin
                          query.ParamByName('CONTROL_ID').Value := leftID;
                          query.ParamByName('FIELD').Value := 'LANESMASK_L';
                          query.ExecSQL;
                        end;
                        if foundRight then
                        begin
                          query.ParamByName('CONTROL_ID').Value := rightID;
                          query.ParamByName('FIELD').Value := 'LANESMASK_R';
                          query.ExecSQL;
                        end;
                      end
                      else if propID = 'Exitlanes' then
                      begin
                        if foundLeft then
                        begin
                          query.ParamByName('CONTROL_ID').Value := leftID;
                          query.ParamByName('FIELD').Value := 'EXITLANES_L';
                          query.ExecSQL;
                        end;
                        if foundRight then
                        begin
                          query.ParamByName('CONTROL_ID').Value := rightID;
                          query.ParamByName('FIELD').Value := 'EXITLANES_R';
                          query.ExecSQL;
                        end;
                      end;
                    end;
                  end;
                finally
                  query.Free;
                end;
                oraSession.Commit;
              except
                oraSession.Rollback;
                adjustedIDs.Clear;
                Log.WriteLn('Error adding control in TUSProject.handleClientMessage. ActionID: ' + measure.actionID.ToString, llError);
              end;
              publishEventName := OraSession.Username + '.GENE_CONTROL';
              table := GetTableSync('GENE_CONTROL');
              publishEvent := IMB3Connection.publish(publishEventName, false);
              try
                for adjustedID in adjustedIDs.Keys do
                begin
                  publishEvent.SignalChangeObject(adjustedIDs[adjustedID], adjustedID);
                  table.SendAnonymousEvent(adjustedIDs[adjustedID], adjustedID);
                end;
              finally
                publishEvent.UnPublish();
              end;
              if adjustedIDs.Count > 0 then
                  aClient.SendMessage('Succesfully added control', mtSucces, 10000);
            end
            else if measure.actionID = -150 then
            begin
              adjustedIDs.Clear;
              if propertyDictionary.ContainsKey('Name')
                and propertyDictionary.ContainsKey('Description')
                and propertyDictionary.ContainsKey('FromRoad')
                and propertyDictionary.ContainsKey('ToRoad')
                and propertyDictionary.ContainsKey('Turncost')
                and Integer.TryParse(propertyDictionary['FromRoad'], fromID)
                and Integer.TryParse(propertyDictionary['ToRoad'], toID) then
              begin
                oraSession.StartTransaction;
                try
                  //lock the table so we can extract highest id and use that to determine ids of the new controls
                  query := TOraQuery.Create(nil);
                  try
                    roadTableName := (aClient.currentScenario as TUSScenario).Tableprefix + 'GENE_ROAD';
                    turncostTableName := (aClient.currentScenario as TUSScenario).Tableprefix + 'TRAF_TURNCOST';
                    query.SQL.Text := 'SELECT FNODE_, TNODE_ FROM ' + roadTableName + ' WHERE OBJECT_ID=:OBJECT_ID AND NOT FNODE_ IS NULL AND NOT TNODE_ IS NULL';
                    query.ParamByName('OBJECT_ID').Value := fromID;
                    query.ExecSQL;
                    if query.FindFirst then
                    begin
                      fRoadfNode := query.FieldByName('FNODE_').Value;
                      fRoadtNode := query.FieldByName('TNODE_').Value;
                      query.ParamByName('OBJECT_ID').Value := toID;
                      query.ExecSQL;
                      if query.FindFirst then
                      begin
                        tRoadfNode := query.FieldByName('FNODE_').Value;
                        tRoadtNode := query.FieldByName('TNODE_').Value;
                        connectedRoads := False;
                        if fRoadfNode = tRoadfNode then
                        begin
                          nodeA := fRoadtNode;
                          nodeB := fRoadfNode;
                          nodeC := tRoadtNode;
                          connectedRoads := True;
                        end
                        else if fRoadfNode = tRoadtNode then
                        begin
                          nodeA := fRoadtNode;
                          nodeB := fRoadfNode;
                          nodeC := tRoadfNode;
                          connectedRoads := True;
                        end
                        else if fRoadtNode = tRoadfNode then
                        begin
                          nodeA := fRoadfNode;
                          nodeB := fRoadtNode;
                          nodeC := tRoadtNode;
                          connectedRoads := True;
                        end
                        else if fRoadtNode = tRoadtNode then
                        begin
                          nodeA := fRoadfNode;
                          nodeB := fRoadtNode;
                          nodeC := tRoadfNode;
                          connectedRoads := True;
                        end;
                        if connectedRoads then
                        begin
                          TMonitor.Enter(aClient.currentScenario.Layers);
                          try
                            aClient.currentScenario.Layers.TryGetValue('road', layer);
                          finally
                            TMonitor.Exit(aClient.currentScenario.Layers);
                          end;
                          if Assigned(layer) and (layer is TUSBasicLayer) then
                          begin
                            basicLayer := (layer as TUSBasicLayer);
                            basicLayer.objectsLock.BeginRead;
                            try
                              if basicLayer.objects.TryGetValue(AnsiString(fromID.ToString), obj) then
                              begin
                                if (obj is TGeometryLayerObject) then
                                begin
                                  geometry := (obj as TGeometryLayerObject).geometry;
                                  if (geometry.parts.Count > 0) then
                                  begin
                                    if (fRoadfNode = nodeB) and (geometry.parts[0].points.Count > 0) then
                                    begin
                                      point.X := geometry.parts[0].points[0].x;
                                      point.Y := geometry.parts[0].points[0].y;
                                      point := sourceProjection.FromGeocs(point);
                                    end
                                    else if ((geometry.parts[geometry.parts.Count - 1].points.Count > 0)) then
                                    begin
                                      point.X := geometry.parts[geometry.parts.Count - 1].points[geometry.parts[geometry.parts.Count - 1].points.Count -1].x;
                                      point.Y := geometry.parts[geometry.parts.Count - 1].points[geometry.parts[geometry.parts.Count - 1].points.Count -1].y;
                                      point := sourceProjection.FromGeocs(point);
                                    end;
                                  end;
                                end;
                              end;
                            finally
                              basicLayer.objectsLock.EndRead;
                            end;
                          end;
                          query.SQL.Text := 'SELECT OBJECT_ID FROM ' + turncostTableName + ' WHERE NODE_A=:NODE_A AND NODE_B=:NODE_B AND NODE_C=:NODE_C';
                          query.ParamByName('NODE_A').Value := nodeA;
                          query.ParamByName('NODE_B').Value := nodeB;
                          query.ParamByName('NODE_C').Value := nodeC;
                          query.ExecSQL;
                          if query.FindFirst then
                          begin
                            turncostID := query.FieldByName('OBJECT_ID').Value;
                            query.SQL.Text := 'LOCK TABLE GENE_CONTROL IN EXCLUSIVE MODE';
                            query.ExecSQL;
                            query.SQL.Text := 'SELECT MAX(OBJECT_ID) as NEWID FROM GENE_CONTROL';
                            query.ExecSQL;
                            if query.FindFirst then
                            begin
                              curMaxID := query.FieldByName('NEWID').AsInteger;
                              curMaxID := curMaxID + 1;
                              mainID := curMaxID;

                              //insert control
                              query.SQL.Text := 'INSERT INTO GENE_CONTROL (OBJECT_ID, NAME, DESCRIPTION, X, Y, PARENT_ID) VALUES (:OBJECT_ID, :NAME, :DESCRIPTION, :X, :Y, :PARENT_ID)';
                              query.ParamByName('X').Value := point.X;
                              query.ParamByName('Y').Value := point.Y;
                              query.ParamByName('OBJECT_ID').Value := mainID;
                              query.ParamByName('Name').Value := propertyDictionary['Name'];
                              query.ParamByName('Description').Value := propertyDictionary['Description'];
                              if propertyDictionary.ContainsKey('ParentID')
                                and Integer.TryParse(propertyDictionary['ParentID'], parentIDValue) then
                                query.ParamByName('PARENT_ID').Value := parentIDValue
                              else
                                query.ParamByName('PARENT_ID').Clear;
                              query.ExecSQL;

                              //insert control object
                              query.SQL.Text := 'INSERT INTO GENE_CONTROL_OBJECTS (CONTROL_ID, OBJECT_ID, OBJECT_TABLE, SIDE, OBJECT_TYPE) VALUES (:CONTROL_ID, :OBJECT_ID, :OBJECT_TABLE, :SIDE, :OBJECT_TYPE)';
                              query.ParamByName('CONTROL_ID').Value := mainID;
                              query.ParamByName('OBJECT_ID').Value := turncostID;
                              query.ParamByName('OBJECT_TABLE').Value := 'GENE_ROAD'; //for now can only use GENE_ROAD change when OTConnector is updated
                              query.ParamByName('SIDE').Value := 0;
                              query.ParamByName('OBJECT_TYPE').Value := 'turncost';
                              query.ExecSQL;

                              //insert control property
                              query.SQL.Text := 'INSERT INTO GENE_CONTROL_PROPERTIES (CONTROL_ID, FIELD, VALUE) VALUES (:CONTROL_ID, :FIELD, :VALUE)';
                              query.ParamByName('CONTROL_ID').Value := mainID;
                              query.ParamByName('FIELD').Value := 'TURNDELAY';
                              query.ParamByName('VALUE').Value := propertyDictionary['Turncost'];
                              query.ExecSQL;
                              adjustedIDs.Add(mainID, actionNew);
                            end;
                          end;
                        end;
                      end;
                    end;
                  finally
                    query.Free;
                  end;
                  oraSession.Commit;
                except
                  adjustedIDs.Clear;
                  oraSession.Rollback;
                  aClient.SendMessage('Error adding control', mtError, 10000);
                end;
                publishEventName := OraSession.Username + '.GENE_CONTROL';
                table := GetTableSync('GENE_CONTROL');
                publishEvent := IMB3Connection.publish(publishEventName, false);
                try
                  for adjustedID in adjustedIDs.Keys do
                  begin
                    publishEvent.SignalChangeObject(adjustedIDs[adjustedID], adjustedID);
                    table.SendAnonymousEvent(adjustedIDs[adjustedID], adjustedID);
                  end;
                finally
                  publishEvent.UnPublish();
                end;
                if adjustedIDs.Count > 0 then
                  aClient.SendMessage('Succesfully added control', mtSucces, 10000);
              end;
            end;
          finally
            objects.Free;
          end;
        finally
          propertyDictionary.Free;
        end;
      end;
    finally
      FreeAndNil(adjustedIDs);
    end;
  end;
end;

procedure TUSProject.handleClientMessage(aClient: TClient; aScenario: TScenario;
  aJSONObject: TJSONObject);
var
  jsonMeasures, selectCategories, selectedObjects: TJSONArray;
  jsonMeasure, jsonArrayItem, jsonValue: TJSONValue;
  jsonStringValue, selectCategoriesString, selectedObjectsString: string;
  responseJSON, requestType, requestID: string;
  baseLayer: TLayerBase;
  selectionLayer: TUSBasicLayer;
  jsonProperties: TJSONArray;
  commitBuilder: TUSCommitBuilder;
  changedCount: Integer;
begin
  inherited;
  if aJSONObject.TryGetValue<TJSONArray>('applyMeasures', jsonMeasures) then
  begin
    for jsonMeasure in jsonMeasures do
      begin
        selectCategoriesString := '';
        if jsonMeasure.TryGetValue('selectCategories', selectCategories) then
        begin
          for jsonArrayItem in selectCategories do
            if jsonArrayItem.TryGetValue<string>(jsonStringValue) then
            begin
              if selectCategoriesString <> '' then
                selectCategoriesString := selectCategoriesString + ', ';
              selectCategoriesString := selectCategoriesString + jsonStringValue;
            end;
        end;
        selectedObjectsString := '';
        if jsonMeasure.TryGetValue('selectedObjects', selectedObjects) then
        begin
          for jsonArrayItem in selectedObjects do
            if jsonArrayItem.TryGetValue<string>(jsonStringValue) then
            begin
              if selectedObjectsString <> '' then
                selectedObjectsString := selectedObjectsString + ', ';
              selectedObjectsString := selectedObjectsString + jsonStringValue;
            end;
        end;
      end;
  end;
  if aJSONObject.TryGetValue<TJSONValue>('dialogDataRequest', jsonValue)
    and jsonValue.TryGetValue('type', requestType)
    and jsonValue.TryGetValue('id', requestID) then //TODO: make a seperate thread to handle data requests
  begin
    if requestType = 'controls' then
    begin
      responseJSON := getUSControlsJSON;
      responseJSON := '{"type": "dialogDataResponse", "payload": { "data": ' + responseJSON +
                          ', "id": "' + requestID + '"' +
                          ', "type": "' + requestType + '" }}';
      aClient.signalString(responseJSON);
    end
    else if (requestType = 'scenarioControls') and (aScenario is TUSScenario) then
    begin
      responseJSON := (aScenario as TUSScenario).GetUSControlsJSON;
      responseJSON := '{"type": "dialogDataResponse", "payload": { "data": ' + responseJSON +
                          ', "id": "' + requestID + '"' +
                          ', "type": "' + requestType + '" }}';
      aClient.signalString(responseJSON);
    end;
    //TODO: generate a response for unknown requestTypes? -> Requires work around for the inheritance fallthrough
  end;
  if aJSONObject.TryGetValue<TJSONValue>('applyObjectsProperties', jsonValue) then
  begin
    Log.WriteLn('Apply object properties');
    if jsonValue.TryGetValue<TJSONArray>('selectedObjects', selectedObjects)
      and jsonValue.TryGetValue<TJSONArray>('selectedCategories', selectCategories)
      and (selectCategories.Count=1)
      and aScenario.Layers.TryGetValue(selectCategories.Items[0].Value, baseLayer)
      and (baseLayer is TUSBasicLayer) then
    begin
      selectionLayer := (baseLayer as TUSBasicLayer);
      jsonProperties := jsonValue.GetValue<TJSONArray>('properties');
      commitBuilder := TUSCommitBuilder.Create(selectionLayer.ObjectProperties, jsonProperties);
      if commitBuilder.CommitChangesToDB(OraSession, IMB3Connection, selectedObjects, changedCount) then
        aClient.SendMessage('Succesfully handled changes. Objects selected: ' + changedCount.toString, mtSucces, 10000)
      else
        aClient.SendMessage('Error applying changes to objects.', mtError);
    end;
  end;
end;

procedure TUSProject.HandleControlPropertyChange(aProject: TProject;
  aClient: TClient; const aType: string; aPayload: TJSONObject);
var
  jsonArray: TJSONArray;
  jsonValue: TJSONValue;
  query: TOraQuery;
  id: Integer;
  field, value: string;
  changeIDs: TDictionary<Integer, Integer>;
  publishEventName: string;
  table: TSubscribeObject;
  publishEvent: TIMBEventEntry;
begin
  query := TOraQuery.Create(nil);
  try
    query.Session := OraSession;
    changeIDs := TDictionary<Integer, Integer>.Create;
    try
      if aPayload.TryGetValue<TJSONArray>('change', jsonArray) then
      begin
        query.SQL.Text := 'UPDATE GENE_CONTROL_PROPERTIES SET VALUE=:VALUE WHERE CONTROL_ID=:CONTROL_ID AND FIELD=:FIELD';
        for jsonValue in jsonArray do
        begin
          if jsonValue.TryGetValue<Integer>('id', id) and
            jsonValue.TryGetValue<string>('field', field) and
            jsonValue.TryGetValue<string>('value', value) then
          begin
            query.ParamByName('CONTROL_ID').Value := id;
            query.ParamByName('FIELD').Value := field;
            query.ParamByName('VALUE').Value := value;
            query.ExecSQL;
            changeIDs.AddOrSetValue(id, id);
          end;
        end;
      end;
      if aPayload.TryGetValue<TJSONArray>('delete', jsonArray) then
      begin
        query.SQL.Text := 'DELETE FROM GENE_CONTROL_PROPERTIES WHERE CONTROL_ID=:CONTROL_ID AND FIELD=:FIELD';
        for jsonValue in jsonArray do
        begin
          if jsonValue.TryGetValue<Integer>('id', id) and
            jsonValue.TryGetValue<string>('field', field) then
          begin
            query.ParamByName('CONTROL_ID').Value := id;
            query.ParamByName('FIELD').Value := field;
            query.ExecSQL;
            changeIDs.AddOrSetValue(id, id);
          end;
        end;
      end;
      publishEventName := OraSession.Username + '.GENE_CONTROL';
      table := GetTableSync('GENE_CONTROL');
      publishEvent := IMB3Connection.publish(publishEventName, false);
      try
        for id in changeIDs.Keys do
        begin
          publishEvent.SignalChangeObject(actionChange, id, 'OBJECT_ID');
          table.SendAnonymousEvent(actionChange, id);
        end;
      finally
        publishEvent.UnPublish();
      end;
    finally
      changeIDs.Free;
    end;
  finally
    query.Free;
  end;
end;

procedure TUSProject.HandleControlsQueueEvent;
  procedure ReadMultipleControls(const aIdString: string; const oraSession: TOraSession);
  var
   query: TOraQuery;
   id: Integer;
   x, y: Double;
   point: TWDGeometryPoint;
   name, description: string;
   control: TUSControl;
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := oraSession;
      query.SQL.Text := 'SELECT OBJECT_ID, NAME, DESCRIPTION, X, Y FROM GENE_CONTROL where PARENT_ID is null and OBJECT_ID in (' + aIdString + ')';
      query.UniDirectional := True;
      query.Open;
      query.First;
      while (not query.Eof) do
        begin
          id := query.FieldByName('OBJECT_ID').AsInteger;
          name := query.FieldByName('NAME').AsString;
          description := query.FieldByName('DESCRIPTION').AsString;
          x := query.FieldByName('X').AsFloat;
          y := query.FieldByName('Y').AsFloat;
          point := TWDGeometryPoint.Create(x, y, 0.0);
          projectGeometryPoint(point, fSourceProjection);
          control := TUSControl.Create(id.ToString, name, description, point.y, point.x);
          point.Free;
          fUSControls.AddOrSetValue(control.ID, control);
          query.Next;
        end;
    finally
      query.Free;
    end;
  end;
var
  localQueue: TList<TUSUpdateQueueEntry>;
  tempQueue: TList<TUSUpdateQueueEntry>;
  entry: TUSUpdateQueueEntry;
  ChangeStack: TStack<string>;
  ChangeString: string;
  i: Integer;
  oraSession: TOraSession;
begin
  localQueue := TList<TUSUpdateQueueEntry>.Create;
  oraSession := TOraSession.Create(nil);
  ChangeStack := TStack<string>.Create;
  try
    oraSession.connectString := ConnectStringFromSession(Self.OraSession);
    oraSession.open;
    while not TThread.CheckTerminated do
    begin
      if fUpdateQueueEvent.WaitFor=wrSignaled then
      begin
        // swap queues
        TMonitor.Enter(fUpdateQueueEvent);
        try
          tempQueue := fUpdateQueue;
          fUpdateQueue := localQueue;
          localQueue := tempQueue;
        finally
          TMonitor.Exit(fUpdateQueueEvent);
        end;
        if localQueue.Count>0 then
        begin
          TMonitor.Enter(fUSControls);
          try
            for entry in localQueue do
            try
              begin
                // process entries
                if entry.action=actionDelete then
                begin
                  if fUSControls.ContainsKey(entry.objectID.ToString) then
                    fUSControls.Remove(entry.objectID.ToString);
                end
                else if entry.action=actionNew then
                begin
                  if not fUSControls.ContainsKey(entry.objectID.ToString) then
                  begin
                    ChangeStack.Push(entry.objectID.ToString);
                  end;
                end
                else if entry.action=actionChange then
                begin
                  if fUSControls.ContainsKey(entry.objectID.ToString) then
                  begin
                    ChangeStack.Push(entry.objectID.ToString);
                  end
                  else Log.WriteLn('TUSScenario.HandleControlsQueueEvent: no result on change control ('+entry.objectID.toString+') query', llWarning);
                end;
              end;
            except
              on e: Exception
              do Log.WriteLn('Exception in handleChangeObject: '+e.Message, llError);
            end;
            if (ChangeStack.Count > 0) then
            begin
              while (ChangeStack.Count > 1000) do
              begin
                changestring := ChangeStack.Pop;
                for i:=2 to 1000 do
                  changestring := changestring + ', ' + ChangeStack.Pop;
                ReadMultipleControls(changestring, oraSession);
              end;
              if ChangeStack.Count > 0 then
              begin
                changestring := ChangeStack.Pop;
                while (ChangeStack.Count > 0) do
                  changestring := changestring + ', ' + ChangeStack.Pop;
                ReadMultipleControls(changestring, oraSession);
              end;
            end;
            finally
              TMonitor.Exit(fUSControls);
            end;
          //Todo: look into cached updates/batching!
        end;
        localQueue.Clear;
      end;
    end;
  finally
    localQueue.Free;
    oraSession.Free; //todo can I use one try/finally for all of these?
    ChangeStack.Free;
  end;
end;

procedure TUSProject.HandleControlsUpdate(aAction, aObjectID: Integer;
  const aObjectName, aAttribute: string);
begin
  TMonitor.Enter(fUpdateQueueEvent);
  try
    begin
      try
        fUpdateQueue.Add(TUSUpdateQueueEntry.Create(aObjectID, aAction));
        fUpdateQueueEvent.SetEvent;
      except
        on e: Exception do
        begin
          Log.WriteLn('TUSProject.handleControlsUpdate. objectid: ' + aObjectID.ToString + ', action: ' + aAction.ToString+': '+e.Message, llError);
        end
      end;
    end
  finally
    TMonitor.Exit(fUpdateQueueEvent);
  end;
end;

procedure TUSProject.handleInternalControlsChange(aSender: TSubscribeObject;
  const aAction, aObjectID: Integer);
begin
  TMonitor.Enter(fUpdateQueueEvent);
  try
    begin
      try
        fUpdateQueue.Add(TUSUpdateQueueEntry.Create(aObjectID, aAction));
        fUpdateQueueEvent.SetEvent;
      except
        on e: Exception do
        begin
          Log.WriteLn('TUSProject.handleInternalControlsChange. objectid: ' + aObjectID.ToString + ', action: ' + aAction.ToString+': '+e.Message, llError);
        end
      end;
    end
  finally
    TMonitor.Exit(fUpdateQueueEvent);
  end;
end;

procedure TUSProject.handleTypedClientMessage(aClient: TClient;
  const aMessageType: string; var aJSONObject: TJSONObject);

  function MakeThreadedScenarioCopy(aSrcID, aConnectString: string; aClient: TClient; aProject: TUSProject): TProc;
  begin
    Result := procedure ()
    var
      dbConnection: TOraSession;
      scenario: TUSScenario;
      dstID, dstDescription: string;
    begin
      dbConnection := TOraSession.Create(nil);
      try
        dbConnection.ConnectString := aConnectString;
        dbConnection.Open;
        aClient.SendMessage('Starting scenario copy', mtSucces, 10000);
        if CopyScenario(aSrcID, dbConnection.Username, dbConnection, dstID, dstDescription) then
        begin
          aClient.SendMessage('Done copying scenario V' + aSrcID + ' to V' + dstID, mtSucces);

          scenario := TUSScenario.Create(Self, dstID, 'V' + dstID, dstDescription, dbConnection.Username.ToUpper + '#V' + dstID, aProject.addBasicLayers, aProject.mapView, fIMB3Connection, 'V' + dstID + '#');
          aProject.fScenarios.Add(dstID, scenario);

          aProject.fScenarioLinks.children.Add(TScenarioLink.Create(
              dstID, aSrcID, aSrcID,
              'V' + dstID, dstDescription, 'OPEN', scenario));
          if GetSetting(UseScenarioHierarchySwitch, False)
          then aProject.fScenarioLinks.buildHierarchy() // todo: use hierarchy via setting?
          else aProject.fScenarioLinks.sortChildren(); // todo: sort scenario links?
          aProject.forEachClient(procedure(aClient: TClient)
          begin;
            SendDomains(aClient, 'updatedomains');
            aClient.UpdateSession; //todo check if this works
          end);
        end
        else //error copying scenario
        begin
          aClient.SendMessage('Error copying scenario', mtError);
        end;
        //handle scenario add
      finally
        dbConnection.Free
      end;
    end;
  end;

var
  jsonArrayItem, payloadValue: TJSONValue;
  controlActive, controlID: Integer;
  payloadArray: TJSONArray;
  scenario: TUSScenario;
//  oraSession: TOraSession;
//  queryText: string;
//  publishEventName: string;
//  publishEvent: TIMBEventEntry;
  scenarioID: Integer;
  baseScenario: TScenario;
  requestID, requestType, responseJSON: string;
//  table: TSubscribeObject;
begin
  inherited;
  if aJSONObject.TryGetValue<TJSONValue>('payload', payloadValue) then
  begin
    if (aMessageType = 'scenarioControlsChanges') and (payloadValue is TJSONArray) then
    begin
      payloadArray := payloadValue as TJSONArray;
      if Assigned(aClient.currentScenario) and (aClient.currentScenario is TUSScenario) then
      begin
        scenario := aClient.currentScenario as TUSScenario;
        for jsonArrayItem in payloadArray do
          if jsonArrayItem.TryGetValue<Integer>('id', controlID) and jsonArrayItem.TryGetValue<Integer>('active', controlActive) then
          begin
            scenario.UpsertUSControlStatus(controlID, controlActive);
          end;
//        oraSession := fDBConnection as TOraSession;
//        queryText := 'update ' + scenario.Tableprefix + 'GENE_CONTROL ' +
//                        'set active = :A where OBJECT_ID = :B';
//        publishEventName := oraSession.Username + '#' + scenario.Name + '.GENE_CONTROL';
//        table := GetTableSync(publishEventName);
//        publishEvent := IMB3Connection.publish(publishEventName, false);
//        try
//          for jsonArrayItem in payloadArray do
//          if jsonArrayItem.TryGetValue<Integer>('id', controlID) and jsonArrayItem.TryGetValue<Integer>('active', controlActive) then
//          begin
//            oraSession.ExecSQL(queryText, [controlActive, controlID]);
//            oraSession.Commit;
//            publishEvent.SignalChangeObject(actionChange, controlID, 'ACTIVE');
//            table.SendAnonymousEvent(actionChange, controlID);
//          end;
//        finally
//          publishEvent.UnPublish;
//        end;
      end;
    end
    else if (aMessageType = 'copyScenario') and payloadValue.TryGetValue<Integer>('scenario', scenarioID) then
    begin
      TMonitor.Enter(fScenarios);
      try
        if fScenarios.TryGetValue(scenarioID.ToString, baseScenario) then
        begin
          TThread.CreateAnonymousThread(MakeThreadedScenarioCopy(baseScenario.ID, ConnectStringFromSession(Self.oraSession), aClient, Self)).Start;
        end;
      finally
        TMonitor.Exit(fScenarios);
      end;
    end
    else if (aMessageType = 'dialogDataRequest') and payloadValue.TryGetValue<string>('id', requestID) and payloadValue.TryGetValue<string>('type', requestType) then
    begin
      if requestType = 'controls' then
      begin
        responseJSON := getUSControlsJSON;
        responseJSON := '{"type": "dialogDataResponse", "payload": { "data": ' + responseJSON +
                            ', "id": "' + requestID + '"' +
                            ', "type": "' + requestType + '" }}';
        aClient.signalString(responseJSON);
      end
      else if (requestType = 'scenarioControls') and (aClient.currentScenario is TUSScenario) then
      begin
        responseJSON := (aClient.currentScenario as TUSScenario).GetUSControlsJSON;
        responseJSON := '{"type": "dialogDataResponse", "payload": { "data": ' + responseJSON +
                            ', "id": "' + requestID + '"' +
                            ', "type": "' + requestType + '" }}';
        aClient.signalString(responseJSON);
      end;
    end
    else if True then

  end;
end;

procedure TUSProject.ReadBasicData;
var
  scenarioID: Integer;
  s: string;
  filter: string;
begin
  fUSScenarioFilters := getUSScenarioFilter(OraSession, fProjectID);
  ReadScenarios;
  ReadMeasures;
  ReadUSControls;
  // load current scenario and ref scenario first
  if USDBScenarios.Count>0 then
  begin
    scenarioID := getUSCurrentPublishedScenarioID(OraSession, GetCurrentScenarioID(OraSession), fProjectID);
    fProjectCurrentScenario := ReadScenario(scenarioID.ToString);
    if not Assigned(fProjectCurrentScenario) then
    begin
      fProjectCurrentScenario := ReadScenario(scenarios.Keys.ToArray[0]);
      Log.WriteLn('current scenario '+scenarioID.ToString+' not found, override by using first', llWarning);
    end;
    Log.WriteLn('current US scenario: '+fProjectCurrentScenario.ID+' ('+(fProjectCurrentScenario as TUSScenario).fTableprefix+'): "'+fProjectCurrentScenario.description+'"', llOk);
    // ref
    scenarioID := GetScenarioBaseID(OraSession, scenarioID);
    if scenarioID>=0 then
    begin
      fProjectRefScenario := ReadScenario(scenarioID.ToString);
      if Assigned(fProjectRefScenario)
      then Log.WriteLn('reference US scenario: '+fProjectRefScenario.ID+' ('+(fProjectRefScenario as TUSScenario).fTableprefix+'): "'+fProjectRefScenario.description+'"', llOk)
      else Log.WriteLn('Reference US scenario '+scenarioID.ToString+' NOT found', llWarning);
    end
    else Log.WriteLn('NO reference US scenario', llWarning);
    if fPreLoadScenarios then
    begin
      for s in fUSDBScenarios.Keys do
      begin
        for filter in fUSScenarioFilters do
          if fUSDBScenarios[s]._published.ToString = filter then
          begin
            ReadScenario(s);
            break;
          end;
      end;
    end;
  end
  else Log.WriteLn('No defined scenarios found!', llError);
end;

procedure TUSProject.ReadMeasures;
var
  measures: TAllRowsResults;
  m: Integer;
begin
  if not TableExists(OraSession, MEASURES_TABLE_NAME) then
  begin
    // add table
    OraSession.ExecSQL(
      'CREATE TABLE '+MEASURES_TABLE_NAME+'('+
        'OBJECT_ID NUMBER,'+
        'CATEGORY VARCHAR2(100 BYTE),'+
        'MEASURE VARCHAR2(100 BYTE),'+
        'DESCRIPTION VARCHAR2(255 BYTE),'+
        'OBJECTTYPES VARCHAR2(50 BYTE),'+
        'ACTION VARCHAR2(100 BYTE),'+
        'ACTION_PARAMETERS VARCHAR2(255 BYTE),'+
        'ACTION_ID INTEGER,'+
        'CONSTRAINT '+MEASURES_TABLE_NAME+'_PK PRIMARY KEY (OBJECT_ID))');
    OraSession.Commit;
  end;
  try
    measures := ReturnAllResults(OraSession,
      'SELECT OBJECT_ID, Category, Measure, Description, ObjectTypes, Action, Action_Parameters, Action_ID '+
      'FROM '+MEASURES_TABLE_NAME);
    if length(measures)>0 then
    begin
      for m := 0 to length(measures)-1 do
      begin
        AddMeasure(measures[m][0], measures[m][1], measures[m][2], measures[m][3], measures[m][4], measures[m][5], measures[m][6], StrToIntDef(measures[m][7], 0));
      end;
    end
    else log.WriteLn('NO measures defined (no entries)', llWarning);
  except
    // no measures, prop. no MEASURES_TABLE_NAME table defined
    log.WriteLn('NO measures defined (no valid table)', llWarning);
  end;
  // todo:
  if length(measures)>0 then
  begin
    Self.EnableControl(measuresControl);
    Self.EnableControl(measuresHistoryControl);
  end;
end;

function TUSProject.ReadScenario(const aID: string): TScenario;
var
  dbScenario: TUSDBScenario;
begin
  System.TMonitor.Enter(fScenarios);
  try
    if not fScenarios.TryGetValue(aID, Result) then
    begin
      if fUSDBScenarios.TryGetValue(aID, dbScenario) then
      begin
        //StatusInc;
        Result := TUSScenario.Create(Self, aID, dbScenario.name, dbScenario.description, dbScenario.IMBPrefix, addBasicLayers, mapView, fIMB3Connection, dbScenario.tablePrefix);
        fScenarios.Add(Result.ID, Result);
      end
      else Result := nil;
    end;
  finally
    System.TMonitor.Exit(fScenarios);
  end;
end;

procedure TUSProject.ReadScenarios;
var
  scenarios: TAllRowsResults;
  s: Integer;
  filterID: string;
  usdbScenario: TUSDBScenario;
  isp: TPair<string, TUSDBScenario>;
begin
  // read scenarios from project database
  if TableExists(OraSession, 'META_SCENARIOS') and not FieldExists(OraSession, 'META_SCENARIOS', 'PUBLISHED') then
  begin
    // add field
    OraSession.ExecSQL(
      'ALTER TABLE META_SCENARIOS '+
      'ADD (PUBLISHED INTEGER)');
    OraSession.Commit;
    Log.WriteLn('Added published field to META_SCENARIOS', llWarning);
  end;
  try
    scenarios := ReturnAllResults(OraSession,
      'SELECT ID, Name, Federate, Parent_ID, Base_ID, Notes, SCENARIO_STATUS, PUBLISHED '+
      'FROM META_SCENARIOS '+
      'ORDER BY ID ASC');
    for s := 0 to Length(scenarios)-1 do
    begin
      usdbScenario := TUSDBScenario.Create(scenarios[s][0], scenarios[s][1], scenarios[s][5],
        scenarios[s][3], scenarios[s][4], scenarios[s][1]+'#', scenarios[s][2], scenarios[s][6], StrToIntDef(scenarios[s][7], 0));
      fUSDBScenarios.Add(usdbScenario.id, usdbScenario);
      for filterID in fUSScenarioFilters do
        if scenarios[s][7]=filterID then
        begin
          fScenarioLinks.children.Add(TScenarioLink.Create(
            usdbScenario.ID, usdbScenario.parentID, usdbScenario.referenceID,
            usdbScenario.name, usdbScenario.description, scenarios[s][6], nil));
          break;
        end;
    end;
  except
    Log.WriteLn('Could not read scenarios, could be published field -> try without', llWarning);
    scenarios := ReturnAllResults(OraSession,
      'SELECT ID, Name, Federate, Parent_ID, Base_ID, Notes, SCENARIO_STATUS '+
      'FROM META_SCENARIOS '+
      //'WHERE SCENARIO_STATUS=''OPEN'''+
      'ORDER BY ID ASC');
    for s := 0 to Length(scenarios)-1 do
    begin
      usdbScenario := TUSDBScenario.Create(scenarios[s][0], scenarios[s][1], scenarios[s][5],
        scenarios[s][3], scenarios[s][4], scenarios[s][1]+'#', scenarios[s][2], scenarios[s][6], 1);
      fUSDBScenarios.Add(usdbScenario.id, usdbScenario);
      fScenarioLinks.children.Add(TScenarioLink.Create(
        usdbScenario.ID, usdbScenario.parentID, usdbScenario.referenceID,
        usdbScenario.name, usdbScenario.description, scenarios[s][6], nil));
    end;
  end;
  // (re)link
  for isp in fUSDBScenarios
  do isp.Value.Relink(fUSDBScenarios);
  // build hierarchy
  if GetSetting(UseScenarioHierarchySwitch, False)
  then fScenarioLinks.buildHierarchy() // todo: use hierarchy via setting?
  else fScenarioLinks.sortChildren(); // todo: sort scenario links?
  // filter closed 'leaves'
  fScenarioLinks.removeLeaf('CLOSED');
end;

procedure TUSProject.ReadUSControls;
var
 query: TOraQuery;
 Table: string;
 ID: Integer;
 Name, Description: string;
 Lat, Lon: Double;
begin
  Table := 'GENE_CONTROL';
  if TableExists(OraSession, Table) then
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := OraSession;
      query.SQL.Text := 'SELECT OBJECT_ID, NAME, DESCRIPTION, Y, X FROM ' + Table + ' WHERE PARENT_ID is NULL';
      query.Open;
      try
        TMonitor.Enter(fUSControls);
        while not query.EoF do
        begin
          ID := query.FieldByName('OBJECT_ID').AsInteger;
          Name := query.FieldByName('NAME').AsString;
          Description := query.FieldByName('DESCRIPTION').AsString;
          Lat := query.FieldByName('Y').AsFloat;
          Lon := query.FieldByName('X').AsFloat;
          fUSControls.Add(ID.ToString, TUSControl.Create(ID.ToString, Name, Description, Lat, Lon));
          query.Next;
        end;
      finally
        TMonitor.Exit(fUSControls);
      end;
    finally
      query.Free;
    end;
  end;
  //TODO: Subscribe to IMB updates
end;

procedure TUSProject.SendInternalTableUpdate(const aUSTableName: string;
  aAction, aObjectID: Integer);
begin
  GetTableSync(aUSTableName).SendAnonymousEvent(aAction, aObjectID);
end;

function getUSMapView(aOraSession: TOraSession; const aDefault: TMapView; const aProjectID: string = ''): TMapView;
var
  table: TOraTable;
  queryText : string;
begin
  if TableExists(aOraSession, PROJECT_TABLE_NAME) then
  begin
    // try to read view from database
    table := TOraTable.Create(nil);
    try
      queryText := 'SELECT Lat, Lon, Zoom '+
        'FROM '+PROJECT_TABLE_NAME;
      if aProjectID <> '' then
        queryText := queryText + ' WHERE PROJECTID='''+aProjectID+'''';
      table.Session := aOraSession;
      table.SQL.Text := queryText;
      try
        table.Execute;
        try
          if table.FindFirst
          then Result := TMapView.Create(table.Fields[0].AsFloat, table.Fields[1].AsFloat, table.Fields[2].AsInteger)
          else Result := aDefault;
        finally
          table.Close;
        end;
      except
        Result := aDefault;
      end;
    finally
      table.Free;
    end;
  end
  else Result := aDefault;
end;

function getUSSourceESPG(aOraSession: TOraSession; const aDefault: Integer): Integer;
var
  table: TOraTable;
begin
  if TableExists(aOraSession, PROJECT_Table_NAME) then
  begin
    if not FieldExists(aOraSession, PROJECT_TABLE_NAME, 'SOURCE_EPSG') then
    begin
      aOraSession.ExecSQL(
        'ALTER TABLE '+PROJECT_TABLE_NAME+ ' ' +
          'ADD SOURCE_EPSG INTEGER');
      aOraSession.Commit;
      Log.WriteLn('Alter table '+PROJECT_TABLE_NAME + ' Added SOURCE_EPSG', llWarning);
    end;
    table := TORaTable.Create(nil);
    try
      table.Session := aOraSession;
      table.SQL.Text := 'SELECT SOURCE_EPSG FROM '+PROJECT_TABLE_NAME;
      try
        table.Execute;
        try
          if table.FindFirst then
          begin
            if not table.Fields[0].IsNull
            then Result := table.Fields[0].AsInteger
            else Result := aDefault;
          end
          else Result := aDefault;
        finally
          table.Close;
        end;
      except
        Result := aDefault;
      end;
    finally
      table.Free;
    end;
  end
  else Result := aDefault;
end;

function getUSProjectID(aOraSession: TOraSession; const aDefault: string): string;
var
  table: TOraTable;
begin
  if TableExists(aOraSession, PROJECT_TABLE_NAME) then
  begin
    // try to read project info from database
    table := TOraTable.Create(nil);
    try
      table.Session := aOraSession;
      table.SQL.Text :=
        'SELECT ProjectID '+
          'FROM '+PROJECT_TABLE_NAME;
      try
        table.Execute;
        try
          if table.FindFirst then
          begin
            if not table.Fields[0].IsNull
            then Result := table.Fields[0].AsString
            else Result := aDefault;
          end
          else Result := aDefault;
        finally
          table.Close;
        end;
      except
        Result := aDefault;
      end;
    finally
      table.Free;
    end;
  end
  else Result := aDefault;
end;

function getUSProjectTypes(aOraSession: TOraSession): TStringArray;
var
  query: TOraQuery;
begin
  SetLength(Result, 0);
  if TableExists(aOraSession, PROJECT_TABLE_NAME) then
  begin
    if FieldExists(aOraSession, PROJECT_TABLE_NAME, 'PROJECT_TYPE') then
    begin
      query := TOraQuery.Create(nil);
      try
        query.Session := aOraSession;
        query.SQL.Text :=
          'SELECT PROJECT_TYPE '+
            'FROM '+PROJECT_TABLE_NAME + ' ' +
            'WHERE NOT ACTIVE = 0 AND NOT PROJECT_TYPE IS NULL';
        query.ExecSQL;
        while not query.Eof do
        begin
          if not query.FieldByName('PROJECT_TYPE').IsNull then //unneeded check...leave to make sure?
          begin
            SetLength(Result, Length(Result) + 1);
            Result[Length(Result) - 1] := query.FieldByName('PROJECT_TYPE').AsString;
          end;
          query.Next;
        end;
      finally
        query.Free;
      end;
    end
    else
    begin
      aOraSession.ExecSQL(
        'ALTER TABLE '+PROJECT_TABLE_NAME+' '+
        'ADD (PROJECT_TYPE VARCHAR(100 BYTE),'+
             'ACTIVE INTEGER,'+
             'SCENARIO_FILTER VARCHAR(100 BYTE))');
      aOraSession.Commit;
    end;
  end;
end;

function getUSProjectIDByType(aOraSession: TOraSession; aProjectType: string): string;
var
  query: TOraQuery;
begin
  Result := '';
  if TableExists(aOraSession, PROJECT_TABLE_NAME) and FieldExists(aOraSession, PROJECT_TABLE_NAME, 'PROJECT_TYPE') then
  begin
    // try to read project info from database
    query := TOraQuery.Create(nil);
    try
      query.Session := aOraSession;
      query.SQL.Text :=
        'SELECT ProjectID '+
          'FROM '+PROJECT_TABLE_NAME+' '+
          'WHERE UPPER(PROJECT_TYPE) = UPPER(:TYPE)';
      query.Params.ParamByName('TYPE').AsString := aProjectType;
      query.ExecSQL;
      if not query.Eof then
      begin
        Result := query.FieldByName('ProjectID').AsString;
      end;
    finally
      query.Free;
    end;
  end;
end;


procedure setUSProjectID(aOraSession: TOraSession; const aProjectID: string; aLat, aLon, aZoomLevel: Double);
var
  query: TOraQuery;
begin
  if not TableExists(aOraSession, PROJECT_TABLE_NAME) then
  begin
    aOraSession.ExecSQL(
      'CREATE TABLE '+PROJECT_TABLE_NAME+'('+
        'PROJECTID VARCHAR2(200 BYTE) NOT NULL ENABLE,'+
        'LAT NUMBER,'+
        'LON NUMBER,'+
        'ZOOM INTEGER,'+
        'STARTPUBLISHEDSCENARIOID INTEGER,'+
        'SOURCE_EPSG INTEGER,'+
        'CONSTRAINT '+PROJECT_TABLE_NAME+'_PK PRIMARY KEY (PROJECTID))');
    aOraSession.Commit;
    Log.WriteLn('Create table '+PROJECT_TABLE_NAME, llWarning);
  end;
  try
    query := TOraQuery.Create(nil);
    try
      query.Session := aOraSession;
      query.SQL.Text :=
        'UPDATE '+PROJECT_TABLE_NAME+' '+
        'SET '+
          'LAT='+aLat.ToString(dotFormat)+', '+
          'LON='+aLon.ToString(dotFormat)+', '+
          'ZOOM='+aZoomLevel.ToString+' '+
        'WHERE PROJECTID='''+aProjectID+'''';
      query.Execute;
      if query.RowsAffected=0 then
      begin
        query.SQL.Text :=
          'INSERT INTO '+PROJECT_TABLE_NAME+' (PROJECTID, LAT, LON, ZOOM) '+
          'VALUES ('''+aProjectID+''', '+aLat.ToString(dotFormat)+', '+aLon.ToString(dotFormat)+', '+aZoomLevel.ToString+')';
        query.Execute;
        query.Close;
      end
      else
      begin
        query.Close;
      end;
      aOraSession.Commit;
    finally
      query.Free;
    end;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception processing project id: '+e.Message, llError);
    end;
  end;
end;

function getUSCurrentPublishedScenarioID(aOraSession: TOraSession; aDefault: Integer; const aProjectID: string): Integer;
var
  table: TOraTable;
begin
  // try to read project info from database
  table := TOraTable.Create(nil);
  try
    table.Session := aOraSession;
    table.SQL.Text :=
      'SELECT StartPublishedScenarioID '+
      'FROM '+PROJECT_TABLE_NAME + ' '+
      'WHERE PROJECTID='''+aProjectID+'''';
    table.Execute;
    try
      if table.FindFirst then
      begin
        if not table.Fields[0].IsNull
        then Result := table.Fields[0].AsInteger
        else Result := aDefault;
      end
      else Result := aDefault;
    finally
      table.Close;
    end;
  finally
    table.Free;
  end;
end;

function getUSScenarioFilter(aOraSession: TOraSession; const aProjectID: string): TStringArray;
var
  table: TOraTable;
  splitArray: TArray<string>;
  i: Integer;
begin
  // try to read project info from database
  table := TOraTable.Create(nil);
  try
    table.Session := aOraSession;
    table.SQL.Text :=
      'SELECT SCENARIO_FILTER '+
      'FROM '+PROJECT_TABLE_NAME + ' '+
      'WHERE PROJECTID='''+aProjectID+'''';
    table.Execute;
    try
      if table.FindFirst then
      begin
        if not table.Fields[0].IsNull
        then
        begin
          splitArray := table.Fields[0].AsString.Split([';', ',', ':']);
          SetLength(Result, Length(splitArray));
          for I := 0 to Length(Result) - 1 do
            Result[i] := splitArray[i];
        end
        else SetLength(Result, 0);
      end
      else SetLength(Result, 0);
    finally
      table.Close;
    end;
  finally
    table.Free;
  end;
end;

{ TUSChart }

procedure TUSChart.AddColumnsToStringList(aStringList: TStringList);

  procedure AddIfNotFound(aValue: string; aStringList: TStringList);
  begin
    if aStringList.IndexOf(aValue) = -1 then
      aStringList.Add(aValue);
  end;

var
  chartSeries: TUSChartSeries;
  columnName: string;
begin
  //add columns from own series
  for chartSeries in Series.Values do
  begin
    AddIfNotFound(chartSeries.XCol, aStringList);
    AddIfNotFound(chartSeries.YCol, aStringList);
  end;

  //add possible child columns
  for columnName in fChildSeries.Keys do
  begin
    AddIfNotFound(columnName, aStringList);
  end;
end;

constructor TUSChart.Create(aChartGroup: TUSChartGroup; aScenario: TScenario; aLines: TDictionary<string, string>; aPrefix, aGroup, aTitle, aTableName: string);
var
  domain, name, description, entry, clickableString: string;
  clickableSplit, entrySplit: TArray<string>;
  defaultLoad: Boolean;
  index: Integer;
  seriesNumber, line, seriesIdentifier: string;
  series: TUSChartSeries;
  seriesIDs: TStringList;
begin
  fChartGroup := aChartGroup;
  fChildSeries := TDictionary<string, string>.Create;
  fXColFilter := '';
  if aLines.ContainsKey(aPrefix + 'Domain') then
    domain := aLines[aPrefix + 'Domain']
  else
    domain := 'US Charts';

  if aLines.TryGetValue(aPrefix + 'Clickable', clickableString) then
  begin
    fClickable := 'labels';
    clickableSplit := clickableString.Split([';']);
    for entry in clickableSplit do
    begin
      entrySplit := entry.Split([':']);
      fChildSeries.Add(entrySplit[0], entrySplit[1]);
    end;
  end
  else
    fClickable := 'none';

  name := '';
  description := 'No Description';
  defaultLoad := True;

  if aLines.ContainsKey(aPrefix + 'TChart$$Title.Text.Strings ') then
    fTitle := copy(aLines[aPrefix + 'TChart$$Title.Text.Strings '], 5, length(aLines[aPrefix + 'TChart$$Title.Text.Strings ']) - 6)
  else
    fTitle := aTitle;

  fGroup := aGroup;
  fSeries := TDictionary<string, TUSChartSeries>.Create;
  fChartType := 'line';
  fGroups := TDictionary<string, TList<string>>.Create;
  fDoubleAxes := False;

  fJSON := '';
  fChanged := True;

  seriesIDs := TStringList.Create;

  for line in aLines.Keys do
  begin
    if line.Contains('TChart$Series') then
    begin
      seriesIdentifier := (line.Split(['TChart$Series'])[1]).Split([':'])[0];
      if seriesIDs.IndexOf(seriesIdentifier) = -1 then
        seriesIDs.Add(seriesIdentifier);
    end;
  end;

  seriesIDs.Sort();

  for index := 0 to 99 do
  begin
    if index > seriesIDs.Count - 1 then
      break;
    if index < 10 then
      seriesNumber := 'Series0'+ IntToStr(index)
    else
      seriesNumber := 'Series' + IntToStr(index);

    if aLines.ContainsKey(aPrefix + seriesNumber + 'Data') then
    begin
      series := TUSChartSeries.Create(aLines, aPrefix, index, seriesIDs[index], fTitle);
      if series.Active then
      begin
        fSeries.Add(seriesNumber, series);
        if series.VertAxis = 'y2' then
          fDoubleAxes := True;
        if fGroups.ContainsKey(series.StackGroup) then
          fGroups[series.StackGroup].Add(series.fTitle)
        else
        begin
          fGroups.Add(series.StackGroup, TList<string>.Create);
          fGroups[series.StackGroup].Add(series.fTitle);
        end;
      end;
    end
    else
      break;
  end;

  inherited Create(aScenario, domain, RemoveTablePrefix(aTableName) + aPrefix, name, description, defaultLoad, 'bar');
end;

constructor TUSChart.CreateChild(aChartGroup: TUSChartGroup; aScenario: TScenario; aID, aTitle, aXCol, aGroup, aXColFilter: string; aSeries: TDictionary<string, string>);
var
  seriesCol: string;
  series: TUSChartSeries;
  counter: Integer;
begin
  inherited Create(aScenario, '', aID, aTitle, aTitle, True, 'bar');
  fTitle := aTitle;
  fChartGroup := aChartGroup;
  fChildSeries := TDictionary<string, string>.Create;
  fGroup := aGroup;
  fSeries := TDictionary<string, TUSChartSeries>.Create;
  fGroups := TDictionary<string, TList<string>>.Create;
  fDoubleAxes := False;
  fXColFilter := aXColFilter;
  counter := 0;
  for seriesCol in aSeries.Keys do
  begin
    series := TUSChartSeries.CreateFromChild(aSeries[seriesCol], aXCol, seriesCol, 'bar');
    fSeries.Add(counter.ToString, series);
    counter := counter + 1;
    if fGroups.ContainsKey(series.StackGroup) then
      fGroups[series.StackGroup].Add(series.fTitle)
    else
    begin
      fGroups.Add(series.StackGroup, TList<string>.Create);
      fGroups[series.StackGroup].Add(series.fTitle);
    end;
  end;
end;

destructor TUSChart.Destroy;
begin
  FreeAndNil(fChildSeries);
  inherited;
end;

procedure TUSChart.FillData(aData: TDictionary<string, TStringList>);
var
  chart: TChart;
  chartSeries: TUSChartSeries;
  index: Integer;
  filterIndex: Integer;
begin
  TMonitor.Enter(Self);
  try
    FreeAndNil(fXValues);
    fXValues := TDictionary<string, TObjectList<TUSChartValue>>.Create;
    if fXColFilter = '' then
    begin
      for chartSeries in fSeries.Values do
        begin
          if chartSeries.Active and not fXValues.ContainsKey(chartSeries.XCol) then
          begin
            fxValues.Add(chartSeries.XCol, TObjectList<TUSChartValue>.Create);
            for index := 0 to aData[chartSeries.XCol].Count - 1 do
              fXValues[chartSeries.XCol].Add(TUSChartValue.Create(aData[chartSeries.XCol][index]));
          end;
          chartSeries.FillData(aData);
        end;
      fChanged := True;
    end
    else
    begin
      for chartSeries in fSeries.Values do
      begin
        if chartSeries.Active then
        begin
          filterIndex := -1;
          for index := 0 to aData[chartSeries.XCol].Count - 1 do
          begin
            if aData[chartSeries.XCol][index] = fXColFilter then
            begin
              filterIndex := index;
              break;
            end;
          end;
          if filterIndex > -1 then
          begin
            if not fXValues.ContainsKey(chartSeries.XCol) then
            begin
              fxValues.Add(chartSeries.XCol, TObjectList<TUSChartValue>.Create);
              fxValues[chartSeries.XCol].Add(TUSChartValue.Create(aData[chartSeries.XCol][filterIndex]));
            end;
            chartSeries.FillFilteredData(aData, filterIndex);
          end;
        end;
      end;
    end;
  finally
    TMonitor.Exit(Self);
  end;
  TMonitor.Enter(fChildCharts);
  try
    for chart in fChildCharts.Values do
    begin
      if chart is TUSChart then
      begin
        (chart as TUSChart).FillData(aData);
      end;
    end;
  finally
    TMonitor.Exit(fChildCharts);
  end;
end;

function TUSChart.getJSON: string;
begin
  if fChanged or not fChanged then    // todo: remove the not fChanged
  begin
    TMonitor.Enter(Self);
    try
      fJSON :=
        '"type":"'+fChartType+'",'+
        '"title":"' + fTitle + '",' +
        '"id":"' + ID + '",' +
        '"clickable":"' + fClickable + '",' +
        '"data":{'+getJSONData+'},' +
        '"axis":{'+ getJSONAxis + '}';
      fChanged := False;
    finally
      TMonitor.Exit(Self);
    end;
  end;
  Result := fJSON;
end;

function TUSChart.getJSONAxes: string;
var
  serie: TUSChartSeries;
begin
  if fDoubleAxes then
  begin
    Result := '';
    for serie in fSeries.Values do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '"' + serie.Title + '": "' + serie.VertAxis + '"';
    end;
  end;
end;

function TUSChart.getJSONAxis: string;
begin
  Result :=  '"x": {"type": "category"}';
  if fDoubleAxes then
    Result := Result + ', "y2": {"show": true }';
end;

function TUSChart.getJSONColumns: string;
var
  dataList: TList<TUSChartValue>;
  key: string;
  columnString: string;
  index: Integer;
  serie: TUSChartSeries;
begin
  Result := '';
  for key in fXValues.Keys do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + '[';
    columnString := '"' + key + '"';
    dataList := fXValues[key];
    for index := 0 to dataList.Count - 1 do
      begin
        columnString := columnString + ',' + dataList[index].GetJSON;
      end;
      Result := Result + columnString + ']';
  end;
  for serie in fSeries.Values do
  begin
    if serie.Active then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '[' + serie.GetColumnJSON + ']';
    end;
  end;
end;

function TUSChart.getJSONData: string;
begin
  Result := '"columns":[' + getJSONColumns +'],';
  if fXValues.Count > 1 then
    Result := Result + '"xs":{' + getJSONXS + '},'
  else if fXValues.Count = 1 then
    Result := Result + '"x":' + getJSONX + ',';

  Result := Result + '"groups":[' + getJSONGroups + '],' +
    '"types":[' + getJSONTypes + '],' +
    '"axes":{' + getJSONAxes + '}';
end;


function TUSChart.getJSONGroups: string;
var
  groupList: TList<string>;
  group, groupstring: string;
begin
  Result := '';
  for groupList in fGroups.Values do
  begin
    if Result <> '' then
      Result := Result + ',';
    groupstring := '';
    for group in groupList do
    begin
      if groupstring <> '' then
        groupstring := groupstring + ',';
      groupstring := groupstring + '"' + group + '"';
    end;
    Result := Result + '[' + groupstring + ']';
  end;
end;

function TUSChart.getJSONTypes: string;
begin
  Result := ''; // todo:..
end;

function TUSChart.getJSONX: string;
var
  key: string;
begin
  Result := '';
  for key in fXValues.Keys do
    if Result = '' then
      Result := '"' + key + '"';
end;

function TUSChart.getJSONXS: string;
var
  serie: TUSChartSeries;
begin
  Result := '';
  for serie in fSeries.Values do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + '"' + serie.YCol + '":"' + serie.XCol + '"';
  end;
end;

procedure TUSChart.HandleGraphLabelClick(aClient: TClient; aLabelTitle: string);
var
  xCol, foundCol: string;
  xValue: TUSChartValue;
  childChart: TChart;
  childUSChart: TUSChart;
  jsonString: string;
begin
  inherited;
  childUSChart := nil;

  TMonitor.Enter(fChildCharts);
  try
    fChildCharts.TryGetValue(aLabelTitle, childChart);
  finally
    TMonitor.Exit(fChildCharts);
  end;

  if not Assigned(childChart) then
  begin
    foundCol := '';
    for xCol in fXValues.Keys do
    begin
      for xValue in fXValues[xCol] do
      begin
        if (xValue.fStringValue = aLabelTitle) or (xValue.fStringValue = '"' + aLabelTitle + '"') then
        begin
          foundCol := xCol;
          break;
        end;
      end;
      if foundCol <> '' then
        break;
    end;
    if foundCol <> '' then
    begin
      childChart := TUSChart.CreateChild(fChartGroup, fScenario, fID + '-' + aLabelTitle, fTitle + ' - ' + aLabelTitle, foundCol, fGroup, aLabelTitle, fChildSeries);
      TMonitor.Enter(fChildCharts);
      try
        if not fChildCharts.ContainsKey(aLabelTitle) then
        begin
          fChildCharts.Add(aLabelTitle, childChart);
          childUSChart := childChart as TUSChart;
        end
        else
        begin
          FreeAndNil(childChart);
          childChart := fChildCharts[aLabelTitle];
          if (childChart is TUSChart) then
            childUSChart := childChart as TUSChart;
        end;
      finally
        TMonitor.Exit(fChildCharts);
      end;
    end;
  end
  else if childChart is TUSChart then
       childUSChart := (childChart as TUSChart);

  if Assigned(childUSChart) then
  begin
    if fScenario is TUSScenario then
    begin
      //todo make chartgroup read all the data instead of only needed data!
      TMonitor.Enter(fChartGroup.LatestData);
      try
        childUSChart.FillData(fChartGroup.LatestData);
      finally
        TMonitor.Exit(fChartGroup.LatestData);
      end;
      jsonString := '{"type":"showelement","payload":[{"type":"chart","element":{' + childUSChart.getJSON + '}}]}';
      aClient.signalString(jsonString);
    end;
  end;
end;

{ TUSChartSeries }

procedure TUSChartSeries.AddXValues(aValues: array of string);
begin

end;

constructor TUSChartSeries.Create(aLines: TDictionary<string, string>; const aPrefix: string; const aID: Integer; const aSeriesID, aChartTitle: string);
var
  sPrefix, sPrefixLong, key: string;
  seriesLines: TDictionary<string, string>;
begin
  fYValues := nil;
  fID := aID;
  sPrefix := 'Series' + IntToStr(aID);
  seriesLines := TDictionary<string, string>.Create;
  if aID < 10 then
    sPrefixLong := 'Series0' + IntToStr(aID)
  else
    sPrefixLong := sPrefix;

  fXCol := aLines[aPrefix + sPrefixLong + 'Data'].Split([','])[1];
  fYCol := aLines[aPrefix + sPrefixLong + 'Data'].Split([','])[0];

  for key in aLines.Keys do
  begin
    if (key.StartsWith(aPrefix + 'TChart$Series' + aSeriesID + ':')) and key.Contains('$$') then
    begin
      seriesLines.AddOrSetValue(key.Split(['$$'])[1], aLines[key]);
    end;
  end;

  if aLines.ContainsKey(aPrefix + sPrefixLong + 'Caption ') then
    fTitle := aLines[aPrefix + sPrefixLong + 'Caption ']
  else if aLines.ContainsKey(aPrefix + sPrefix + 'Caption ')  then
    fTitle := aLines[aPrefix + sPrefix + 'Caption ']
  else if seriesLines.ContainsKey('Title ') then
    fTitle := seriesLines['Title ']
  else
    fTitle := aChartTitle; // 'Unknown Title';

  fTitle := StripChars(fTitle, ['"', '''']);

  if seriesLines.ContainsKey('MultiBar ') then
    fMultiBar := seriesLines['MultiBar ']
  else
    fMultiBar := ' mbNone';

  if fMultiBar <> ' mbNone' then
  begin
    if seriesLines.ContainsKey('StackGroup ') then
      fStackGroup := seriesLines['StackGroup ']
    else
      fStackGroup := '0';
  end
  else
  begin
    fStackGroup := aSeriesID;
  end;

  if seriesLines.ContainsKey('Active ') and ((seriesLines['Active '] = ' False') or (seriesLines['Active '] = 'False')) then
    fActive := False
  else
    fActive := True;

  if seriesLines.ContainsKey('VertAxis ') and (seriesLines['VertAxis '] = ' aRightAxis') then
    fVertAxis := 'y2'
  else
    fVertAxis := 'y';
end;

constructor TUSChartSeries.CreateFromChild(aTitle, aXCol, aYCol, aType: string);
begin
  fYValues := nil;
  fActive := True;
  fTitle := aTitle;
  fXCol := aXCol;
  fYCol := aYCol;
  fType := aType;
  fStackGroup := aTitle;
  fMultiBar := 'mbNone';
  fVertAxis := 'y';
end;

destructor TUSChartSeries.Destroy;
begin
  FreeAndNil(fYValues);
  inherited;
end;

procedure TUSChartSeries.FillData(aData: TDictionary<string, TStringList>);
var
  index: Integer;
begin
  FreeAndNil(fYValues);
  fYValues := TObjectList<TUSChartValue>.Create;
  for index := 0 to aData[YCol].Count - 1 do
    fYValues.Add(TUSChartValue.Create(aData[YCol][index]));
end;

procedure TUSChartSeries.FillFilteredData(
  aData: TDictionary<string, TStringList>; aFilterIndex: Integer);
begin
  FreeAndNil(fYValues);
  fYValues := TObjectList<TUSChartValue>.Create;
  fYValues.Add(TUSChartValue.Create(aData[YCol][aFilterIndex]));
end;

function TUSChartSeries.GetColumnJSON: string;
var
  index: Integer;
begin
  Result := '"' + fTitle + '"';
  for index := 0 to fYValues.Count - 1 do
  begin
    Result := Result + ',' + fYValues[index].GetJSON;
  end;
end;

{ TUSChartValue }

constructor TUSChartValue.Create(aValue: string);
begin
  if aValue = '' then
    fStringValue := 'null'
  else
    fStringValue := '"' + aValue + '"';

  fNumValue := StrToFloatDef(aValue, NaN);
  if isNaN(fNumValue) then
    fNumber := False
  else
    fNumber := True;
end;

function TUSChartValue.GetJSON: string;
begin
  if fNumber then
    Result := FloatToStr(fNumValue).Replace(',', '.')
  else
    Result := fStringValue;
end;

{ TUSChartGroup }

constructor TUSChartGroup.Create(const aScenario: TUSScenario; const aDefTableName, aDatTableName: string);
begin
  fLatestData := TObjectDictionary<string, TStringList>.Create([doOwnsValues]);
  fScenario := aScenario;
  fDefTableName := aDefTableName;
  fDatTableName := aDatTableName;
  fCharts := TList<TUSChart>.Create;
  fUpdateTimer := fScenario.project.Timers.CreateInactiveTimer;
  fUpdateTimer.MaxPostponeDelta := DateTimeDelta2HRT(dtOneSecond*30);
  fLastUpdate := hrtNow;
end;

destructor TUSChartGroup.Destroy;
begin
  FreeAndNil(fLatestData);
  FreeAndNil(fCharts);
end;

procedure TUSChartGroup.HandleChangeObject(aAction, aObjectID: Integer;
  const aObjectName, aAttribute: string);
var
  delta: THighResTicks;
begin
  delta := Max(DateTimeDelta2HRT(dtOneSecond*5),DateTimeDelta2HRT(dtOneSecond*30) - (hrtNow - fLastUpdate));
  fUpdateTimer.Arm(delta,
    HandleDataUpdate);
end;

procedure TUSChartGroup.HandleDataUpdate(aTimer: TTimer; aTime: THighResTicks);
var
  chart: TUSChart;
  clientMessage: string;
begin
  ReadDataFromDB;
  clientMessage := '';
  TMonitor.Enter(fScenario.fCharts);
  try
    for chart in Charts do
    begin
      if clientMessage <> '' then
        clientMessage := clientMessage + ',';
      clientMessage := clientMessage + '{' + chart.getJSON + '}';
    end;
  finally
    TMonitor.Exit(fScenario.fCharts);
  end;
  if clientMessage <> '' then
  begin
    clientMessage := '{"type":"updatechart", "payload":[' + clientMessage + ']}';
    fScenario.forEachSubscriber<TClient>(procedure (aClient: TClient)
      begin
        aClient.signalString(clientMessage);
      end);
  end;
  fLastUpdate := aTime;
end;

procedure TUSChartGroup.ReadDataFromDB;
var
  i,j: Integer;
  oraSession: TOraSession;
  datResult: TAllRowsResults;
  dataCols, dataCol: TStringList;
  datQuery: string;
begin
  TMonitor.Enter(LatestData);
  try
    LatestData.Clear;
    oraSession := (fScenario.project as TUSProject).OraSession;
    dataCols := TStringList.Create;
    dataCols.Duplicates := TDuplicates.dupIgnore;
    TMonitor.Enter(fScenario.fCharts);
    try
      for i := 0 to Charts.Count - 1 do
      begin
        Charts[i].AddColumnsToStringList(dataCols);
      end;
      if dataCols.Count > 0 then
      begin
        datQuery := 'SELECT ' + dataCols[0];
        for i := 1 to dataCols.Count - 1 do
          datQuery := datQuery + ', ' + dataCols[i];
        datQuery := datQuery + ' FROM ' + DatTableName;
        try
          datResult := ReturnAllResults(oraSession, datQuery);
        except
          setLength(datResult, 0);
        end;
        for i := 0 to dataCols.Count -1 do
        begin
          dataCol := TStringList.Create;
          for j := 0 to length(datResult) - 1 do
            dataCol.Add(datResult[j,i]);
          LatestData.Add(dataCols[i], dataCol);
        end;
        for i := 0 to Charts.Count - 1 do
        begin
          charts[i].FillData(LatestData);
        end;
      end;
    finally
      TMonitor.Exit(fScenario.fCharts);
    end;
  finally
    TMonitor.Exit(LatestData);
  end;
end;

procedure TUSChartGroup.SetEvent(aDataEvent: TIMBEventEntry);
begin
  fDataEvent := aDataEvent;
  fDataEvent.OnChangeObject := HandleChangeObject;
end;

{ TUSControl }

constructor TUSControl.Create(aID: string; aName, aDescription: string; aLat,
  aLon: Double);
begin
  fID := aID;
  fName := aName;
  fDescription := aDescription;
  fLat := aLat;
  fLon := aLon;
end;

destructor TUSControl.Destroy;
begin

  inherited;
end;

{ TUSControlSetting }

constructor TUSControlStatus.Create(aID: Integer; aActive: Boolean);
begin
  fID := aID;
  fActive := aActive;
end;

destructor TUSControlStatus.Destroy;
begin

  inherited;
end;

{ TMetaObjectsEntry }

procedure TMetaObjectsEntry.ReadFromQueryRow(aQuery: TOraQuery);
  function StringField(const aFieldName: string; const aDefaultValue: string=''): string;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsString
    else Result := aDefaultValue;
  end;

  function IntField(const aFieldName: string; aDefaultValue: Integer=0): Integer;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsInteger
    else Result := aDefaultValue;
  end;

  function DoubleField(const aFieldName: string; aDefaultValue: Double=NaN): Double;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsFloat
    else Result := aDefaultValue;
  end;
begin
  OBJECT_ID := IntField('OBJECT_ID');
  TABLE_NAME := StringField('TABLE_NAME').ToUpper;
  LAYER_DESCRIPTION := StringField('LAYER_DESCRIPTION');
  SELECT_PROPERTY := StringField('PROPERTY');
  PROPERTY_DESCRIPTION := StringField('PROPERTY_DESCRIPTION');
  PROPERTY_CATEGORY := StringField('PROPERTY_CATEGORY');
  OBJECT_TYPE := StringField('OBJECT_TYPE');
  GEOMETRY_TYPE := StringField('GEOMETRY_TYPE');
  EDITABLE := IntField('EDITABLE', 0);
  _published := IntField('PUBLISHED');
end;

{ TSelectProperty }

function TSelectProperty.ReadFromMetaObjectsEntry(
  const aMetaObjectsEntry: TMetaObjectsEntry): Boolean;
begin
  ID := aMetaObjectsEntry.OBJECT_ID;
  if aMetaObjectsEntry.SELECT_PROPERTY <> '' then
  begin
    Result := True;
    SelectProperty := aMetaObjectsEntry.SELECT_PROPERTY;
    PropertyDescription := aMetaObjectsEntry.PROPERTY_DESCRIPTION;
    Editable := aMetaObjectsEntry.EDITABLE;
  end
  else
    Result := False;

end;

{ TUSBasicLayer }

constructor TUSBasicLayer.Create(aScenario: TScenario; const aDomain, aID,
  aName, aDescription: string; aDefaultLoad: Boolean; const aObjectTypes,
  aGeometryType: string; aLayerType: Integer; aDiffRange: Double;
  const aConnectString, aNewQuery, aChangeMultipleQuery: string;
  const aDataEvent: array of TIMBEventEntry;
  aSourceProjection: TGIS_CSProjectedCoordinateSystem; aPalette: TWDPalette;
  const aTableName: string);
begin
  fTableName := aTableName;
  fObjectProperties := TUSObjectProperties.Create(fTableName, Self);
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes,
    aGeometryType, aLayerType, aDiffRange, aConnectString, aNewQuery, aChangeMultipleQuery, aDataEvent,
    aSourceProjection, aPalette, True);
end;

destructor TUSBasicLayer.Destroy;
begin
  inherited;
  FreeAndNil(fObjectProperties);
end;

{ TUSCommitProperty }

function TUSCommitProperty.AddValue(const aValue, aType: string): Boolean;
var
  parsedValue : Variant;
begin
  Result := True;
  fDataType := aType;
  try
    if aType = 'int' then
      parsedValue := StrToInt(aValue)
    else if aType = 'float' then
      parsedValue := StrToFloat(aValue)
    else if aType = 'bool' then
      parsedValue := StrToBool(aValue)
    else
      parsedValue := aValue;

    fValue := parsedValue;
    fStringValue := aValue;
  except
    on E: Exception do
    begin
     Result := False;
     //todo: logging?
    end;
  end;
end;

constructor TUSCommitProperty.Create(aSelectProperty: TSelectProperty);
begin
  fName := aSelectProperty.SelectProperty;
end;

destructor TUSCommitProperty.Destroy;
begin

  inherited;
end;

{ TUSCommitPropertyBuilder }

function TUSCommitPropertyBuilder.CommitChangesToDB(aLayer: TUSBasicLayer; aOraSession: TOraSession; aUSIMBConnection: TIMBConnection; aSelectedObjects: TJSONArray; aBasicLayer: TUSBasicLayer; out aChangedCount: Integer): Boolean;
var
  baseQuery: string;
  valueArray: array of Variant;
  properties: string;
  commitProperty: TUSCommitProperty;
  index, objectID: Integer;
  selectedID: TJSONValue;
  publishEvent: TIMBEventEntry;
  obj: TLayerObjectWithID;
  subObj: TSubscribeObject;
begin
  Result := True;
  aChangedCount := 0;
  if fChangedProperties.Count > 0 then
  begin
    properties := '';
    SetLength(valueArray, fChangedProperties.Count + 1);
    index := 0;
    for commitProperty in fChangedProperties.Values do
    begin
      if properties <> '' then
        properties := properties + ',';
      properties := properties + commitProperty.fName + '= :' + commitProperty.fName;
      valueArray[index] := commitProperty.fStringValue;
      index := index + 1;
    end;
    baseQuery := 'UPDATE ' + fTableName + ' ' +
                  'SET ' + properties + ' ' +
                  'WHERE OBJECT_ID = :ID';

    publishEvent := aUSIMBConnection.Publish(aOraSession.Username + '#' + aBasicLayer.fTableName.Split(['#'])[0] + '.' + aBasicLayer.fTableName.Split(['#'])[1], false);
    try
      try
        for selectedID in aSelectedObjects do
        if TryStrToInt(selectedId.Value, objectId) and aBasicLayer.Objects.TryGetValue(AnsiString(objectID.ToString), obj) then
        begin
          valueArray[index] := obj.ID;
          aOraSession.ExecSQL(baseQuery, valueArray);
          aOraSession.Commit;
          for commitProperty in fChangedProperties.Values do
          begin
            publishEvent.SignalChangeObject(actionChange, objectID, commitProperty.fName.ToUpper);
          end;
          aChangedCount := aChangedCount + 1;
          for subObj in aLayer.Subscriptions.Values do
            subObj.SendEvent(aLayer, 2 , objectID);
        end;
      except
        on E: Exception do
        begin
          Result := False
          //todo: add error logging
        end;
      end;
    finally
      publishEvent.UnPublish;
    end;
  end;
end;

constructor TUSCommitPropertyBuilder.Create(aTableName: string;
  aSelectProperties: TSelectProperties);
var
  selectProperty: TSelectProperty;
begin
  fTableName := aTableName;
  fProperties := TDictionary<string, TSelectProperty>.Create;
  for selectProperty in aSelectProperties do
    if selectProperty.Editable > 0 then
      fProperties.Add(selectProperty.PropertyDescription, selectProperty);
  fChangedProperties := TObjectDictionary<string, TUSCommitProperty>.Create([doOwnsValues]);
end;

destructor TUSCommitPropertyBuilder.Destroy;
begin
  FreeAndNil(fProperties);
  FreeAndNil(fChangedProperties);
  inherited;
end;

procedure TUSCommitPropertyBuilder.ParseJSONProperties(aJSONProperties: TJSONArray);
var
  jsonProperty, jsonType, jsonValue, jsonName: TJSONValue;
  selectProperty: TSelectProperty;
  commitProperty: TUSCommitProperty;
begin
  for jsonProperty in aJSONProperties do
    if jsonProperty.TryGetValue<TJSONValue>('name', jsonName)
      and fProperties.TryGetValue(jsonName.Value, selectProperty)
      and jsonProperty.TryGetValue<TJSONValue>('value', jsonValue)
      and jsonProperty.TryGetValue<TJSONValue>('type', jsonType) then
    begin
      commitProperty := TUSCommitProperty.Create(selectProperty);
      try
        if (commitProperty.AddValue(jsonValue.Value, jsonType.Value)) then
        begin
          fChangedProperties.Add(selectProperty.SelectProperty, commitProperty); //hand over ownership
          commitProperty := nil;
        end;
      finally
        FreeAndNil(commitProperty); //destroy if ownership hasn't passed
      end;
    end;
end;

{ TMetaObjectEntry }

procedure TMetaObjectEntry.ReadFromQueryRow(aQuery: TOraQuery);
  function StringField(const aFieldName: string; const aDefaultValue: string=''): string;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsString
    else Result := aDefaultValue;
  end;

  function IntField(const aFieldName: string; aDefaultValue: Integer=0): Integer;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsInteger
    else Result := aDefaultValue;
  end;

  function DoubleField(const aFieldName: string; aDefaultValue: Double=NaN): Double;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsFloat
    else Result := aDefaultValue;
  end;
begin
  OBJECT_ID := IntField('OBJECT_ID');
  TABLE_NAME := StringField('TABLE_NAME').ToUpper;
  LAYER_DESCRIPTION := StringField('LAYER_DESCRIPTION');
  OBJECT_TYPE := StringField('OBJECT_TYPE');
  GEOMETRY_TYPE := StringField('GEOMETRY_TYPE');
  TABLE_FILTER := StringField('TABLE_FILTER');
  _published := IntField('PUBLISHED');
end;

{ TMetaObjectPropertyEntry }

procedure TMetaObjectPropertyEntry.ReadFromQueryRow(aQuery: TOraQuery);
  function StringField(const aFieldName: string; const aDefaultValue: string=''): string;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsString
    else Result := aDefaultValue;
  end;

  function IntField(const aFieldName: string; aDefaultValue: Integer=0): Integer;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsInteger
    else Result := aDefaultValue;
  end;

  function DoubleField(const aFieldName: string; aDefaultValue: Double=NaN): Double;
  var
    F: TField;
  begin
    F := aQuery.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsFloat
    else Result := aDefaultValue;
  end;
var
  propertyFunction: string;
begin
  OBJECT_ID := IntField('OBJECT_ID');
  META_OBJECT_ID := IntField('META_OBJECT_ID');
  PROPERTY_NAME := StringField('PROPERTY');

  propertyFunction := StringField('PROPERTY_FUNCTION').ToUpper;
  if propertyFunction = 'ABS' then
    PROPERTY_FUNCTION := pfAbs
  else if propertyFunction = 'CEIL' then
    PROPERTY_FUNCTION := pfCeil
  else if propertyFunction = 'FLOOR' then
    PROPERTY_FUNCTION := pfFloor
  else if propertyFunction = 'AVG' then
    PROPERTY_FUNCTION := pfAvg
  else if propertyFunction = 'COUNT' then
    PROPERTY_FUNCTION := pfCount
  else if propertyFunction = 'MAX' then
    PROPERTY_FUNCTION := pfMax
  else if propertyFunction = 'MIN' then
    PROPERTY_FUNCTION := pfMin
  else if propertyFunction = 'SUM' then
    PROPERTY_FUNCTION := pfSum
  else
    PROPERTY_FUNCTION := pfNone;

  PROPERTY_DESCRIPTION := StringField('PROPERTY_DESCRIPTION');
  PROPERTY_CATEGORY := StringField('PROPERTY_CATEGORY');
  JOIN_TABLE := StringField('JOIN_TABLE');
  BASE_TABLE_ID := StringField('BASE_TABLE_ID');
  JOIN_TABLE_ID := StringField('JOIN_TABLE_ID');
  SIDE := IntField('SIDE');
  EDITABLE := IntField('EDITABLE');
  ORDERING := IntField('ORDERING', Integer.MaxValue);
  _published := IntField('PUBLISHED');
end;

{ TObjectProperties }

procedure TUSObjectProperties.AddFromObjectPropertyEntry(
  const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry);
begin
  //TODO: validation, base table will have empty string as JOIN_TABLE -> do we want this?
  if aMetaObjectPropertyEntry._published > 0 then
  begin
    if fTables.ContainsKey(aMetaObjectPropertyEntry.JOIN_TABLE) then
      fTables[aMetaObjectPropertyEntry.JOIN_TABLE].AddFromObjectPropertyEntry(aMetaObjectPropertyEntry)
    else
      fTables.Add(aMetaObjectPropertyEntry.JOIN_TABLE, TUSObjectPropTable.Create(aMetaObjectPropertyEntry, Self));
  end;
end;

constructor TUSObjectProperties.Create(const aBaseTableName: string; aBasicLayer: TUSBasicLayer);
begin
  fTables := TObjectDictionary<string, TUSObjectPropTable>.Create([doOwnsValues]);
  fBaseTableName := aBaseTableName;
  fBasicLayer := aBasicLayer;
end;

destructor TUSObjectProperties.Destroy;
begin
  FreeAndNil(fTables);
  inherited;
end;

function TUSObjectProperties.TryGetPropertyTable(const aPropertyName: string;
  out aObjectPropTable: TUSObjectPropTable): Boolean;
var
  objectPropTable: TUSObjectPropTable;
begin
  Result := False;
  for objectPropTable in fTables.Values do
    if objectPropTable.ContainsProperty(aPropertyName) then
    begin
      aObjectPropTable := objectPropTable;
      exit(True);
    end;
end;

{ TUSObjectPropTabel }

procedure TUSObjectPropTable.AddFromObjectPropertyEntry(
  const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry);
begin
  //TODO: validation
  if fProperties.ContainsKey(aMetaObjectPropertyEntry.PROPERTY_DESCRIPTION) then
    fProperties[aMetaObjectPropertyEntry.PROPERTY_DESCRIPTION].AddFromObjectPropertyEntry(aMetaObjectPropertyEntry)
  else
    fProperties.Add(aMetaObjectPropertyEntry.PROPERTY_DESCRIPTION, TUSObjectProp.Create(aMetaObjectPropertyEntry, Self));
end;

function TUSObjectPropTable.ContainsProperty(
  const aPropertyName: string): Boolean;
begin
  Result := fProperties.ContainsKey(aPropertyName);
end;

constructor TUSObjectPropTable.Create(
  const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry;
  aPropertiesBase: TUSObjectProperties);
begin
  fPropertiesBase := aPropertiesBase;
  fProperties := TObjectDictionary<string, TUSObjectProp>.Create([doOwnsValues]);
  fJoinTableName := aMetaObjectPropertyEntry.JOIN_TABLE;
  fBaseID := aMetaObjectPropertyEntry.BASE_TABLE_ID;
  fJoinID := AMetaObjectPropertyEntry.JOIN_TABLE_ID;
  AddFromObjectPropertyEntry(aMetaObjectPropertyEntry);
end;

destructor TUSObjectPropTable.Destroy;
begin
  FreeAndNil(fProperties);
  inherited;
end;

{ TUSObjectProp }

procedure TUSObjectProp.AddFromObjectPropertyEntry(
  const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry);
begin
  //TODO: validation
  fSidedProperties.Add(aMetaObjectPropertyEntry.SIDE, aMetaObjectPropertyEntry.PROPERTY_NAME);
  fDescription := aMetaObjectPropertyEntry.PROPERTY_DESCRIPTION;
  fCategory := aMetaObjectPropertyEntry.PROPERTY_CATEGORY;
  fPropertyFunction := aMetaObjectPropertyEntry.PROPERTY_FUNCTION;
  fEditable := (aMetaObjectPropertyEntry.EDITABLE > 0) and (fPropertyFunction = pfNone) and fEditable;
  if aMetaObjectPropertyEntry.ORDERING < fOrdering then
    fOrdering := aMetaObjectPropertyEntry.ORDERING;
end;

function TUSObjectProp.ContainsSide(aSide: Integer): Boolean;
begin
  Result := fSidedProperties.ContainsKey(aSide);
end;

constructor TUSObjectProp.Create(
  const aMetaObjectPropertyEntry: TMetaObjectPropertyEntry;
  aPropertyTable: TUSObjectPropTable);
begin
  fSidedProperties := TDictionary<Integer, string>.Create;
  fPropertyTable := aPropertyTable;
  fEditable := True;
  fOrdering := Integer.MaxValue;
  AddFromObjectPropertyEntry(aMetaObjectPropertyEntry);
end;

destructor TUSObjectProp.Destroy;
begin
  FreeAndNil(fSidedProperties);
  inherited;
end;

function TUSObjectProp.PropertyName(out aPropertyName: string; aSide: Integer=0): Boolean;
begin
  if fSidedProperties.ContainsKey(aSide) then
  begin
    aPropertyName := fSidedProperties[aSide];
    Result := True;
  end
  else if fSidedProperties.ContainsKey(0) then
  begin
    aPropertyName := fSidedProperties[0];
    Result := True;
  end
  else
    Result := False;
end;

{ TUSBuilderProp }

function TUSBuilderProp.AddValue(const aValue: string;
  const aDataType: TFieldType; const aCount: Integer=0): Boolean;
begin
  fAddValueLock.BeginWrite;
  try
    Result := LockedAddValue(aValue, aDataType, aCount);
  finally
    fAddValueLock.EndWrite;
  end;
end;

function TUSBuilderProp.Category: string;
begin
  Result := fObjectProp.Category;
end;

constructor TUSBuilderProp.Create(aObjectProp: TUSObjectProp);
begin
  fObjectProp := aObjectProp;
  fDataType := '';
  fValue := '';
end;

function TUSBuilderProp.Description: string;
begin
  if Assigned(fObjectProp) then
    Result := fObjectProp.Description
  else
    Result := ''; //TODO: should we throw error?
end;

destructor TUSBuilderProp.Destroy;
begin
  fObjectProp := nil;
end;

function TUSBuilderProp.Editable: Boolean;
begin
  Result := fObjectProp.Editable
end;

function TUSBuilderProp.getJSON: string;
var
  doubleValue: Double;
  valueString: string;
  editableString: string;
begin
  {
    "name" : "Height",
    "value" : 5 | "Marie" | false, 300.3
    "type" : "list" | "string" | "int" | "float" | "bool",
    "editable" : "Y" | "N",
    "options" : [1, 3, 343, 5],
    "forced" : "Y" | "N"
  }
  if value <> '' then
  begin
    try
      doubleValue := StrToFloat(value);
      valueString := doubleValue.ToString(dotFormat);
    except
      on E: Exception do
      begin
        valueString := value;
      end;
    end;
  end
  else
    valueString := value;

  if Editable then
    editableString := 'true'
  else
    editableString := 'false';

  Result :=
    '"name":"'+description+'",'+
    '"type":"'+dataType+'",'+
    '"editable":"'+editableString+'",'+
    '"value":"'+valueString + '",'+
    '"ordering":"'+Ordering.ToString+'",'+
    '"category":"'+Category+'"';
end;

function TUSBuilderProp.Ordering: Integer;
begin
  Result := fObjectProp.Ordering;
end;

function TUSBuilderProp.PropertyName(out aPropertyName: string; aSide: Integer=0): Boolean;
begin
  if Assigned(fObjectProp) then
    Result := fObjectProp.PropertyName(aPropertyName, aSide)
  else
    Result := False;
end;

{ TUSBuilderTable }

constructor TUSBuilderTable.Create(aObjectTable: TUSObjectPropTable);
var
  objectProp: TUSObjectProp;
begin
  fObjectTable := aObjectTable;
  fNormProps := TObjectList<TUSBuilderProp>.Create(True);
  fAggrProps := TObjectList<TUSBuilderProp>.Create(True);
  for objectProp in aObjectTable.Properties.Values do
  begin
    case objectProp.PropertyFunction of
      pfNone: fNormProps.Add(TUSBuilderPropNone.Create(objectProp));
      pfAbs: ;
      pfCeil: ;
      pfFloor: ;
      pfAvg: ;
      pfCount: ;
      pfMax: ;
      pfMin: ;
      pfSum: ;
    end;
  end;
  fOpenNormProps := fNormProps.Count;
  fOpenAggrProps := fAggrProps.Count;
end;

destructor TUSBuilderTable.Destroy;
begin
  FreeAndNil(fNormProps);
  FreeAndNil(fAggrProps);
end;

function TUSBuilderTable.QueryAggrProps(aOraSession: TOraSession;
  const aObjectIds: string; aSelectedObjects: TObjectDictionary<string, TUSSelectedObject>): Boolean;
begin
  Result := True;
  //TODO: implement!
end;

function TUSBuilderTable.QueryDB(aOraSession: TOraSession;
  const aObjectIds: string; aSelectedObjects: TObjectDictionary<string, TUSSelectedObject>): Boolean;
var
  normDone, aggrDone: Boolean;
begin
  normDone := (fOpenNormProps = 0);
  if not normDone then
    normDone := QueryNormProps(aOraSession, aObjectIds, aSelectedObjects);

  aggrDone := (fOpenAggrProps = 0);
  if not aggrDone then
    aggrDone := QueryAggrProps(aOraSession, aObjectIds, aSelectedObjects);
  Result := (normDone and aggrDone);
end;

function TUSBuilderTable.QueryNormProps(aOraSession: TOraSession;
  const aObjectIds: string; aSelectedObjects: TObjectDictionary<string, TUSSelectedObject>): Boolean;
var
  queryString, propertiesString, tablePrefix, scenarioPrefix, propertyName: string;
  selectedObject: TUSSelectedObject;
  side: Integer;
  normProperty: TUSBuilderProp;
  query: TOraQuery;
  field: TField;
begin
  Result := False;
  propertiesString := '';
  scenarioPrefix := (fObjectTable.PropertiesBase.BasicLayer.scenario as TUSScenario).Tableprefix;
  if fObjectTable.JoinTableName <> '' then
    tablePrefix := 't1'
  else
    tablePrefix := '';
  for normProperty in fNormProps do
  begin
    if propertiesString<>'' then
      propertiesString := propertiesString + ',';
    propertiesString := propertiesString + normProperty.SQLProperties(tablePrefix);
  end;
  if fObjectTable.JoinTableName <> '' then
    queryString := 'SELECT t2.OBJECT_ID as OBJECT_ID,'+propertiesString+'' +
                    ' FROM ' + scenarioPrefix + fObjectTable.PropertiesBase.fBaseTableName + ' t2 join ' + scenarioPrefix + fObjectTable.JoinTableName + ' t1' +
                    ' ON t2.' + fObjectTable.BaseID + '=t1.' + fObjectTable.JoinID +
                    ' WHERE t2.OBJECT_ID IN (' + aObjectIds + ')'
  else
    queryString := 'SELECT OBJECT_ID, ' + propertiesString + ' FROM ' + scenarioPrefix + fObjectTable.PropertiesBase.BaseTableName +
                    ' WHERE OBJECT_ID IN (' + aObjectIds + ')';

  query := TOraQuery.Create(nil);
  try
    query.Session := aOraSession;
    query.SQL.Text := queryString;
    query.Open;
    while not query.EoF do
    begin
      if aSelectedObjects.TryGetValue(query.FieldByName('OBJECT_ID').AsString, selectedObject) then
      begin
        for normProperty in fNormProps do
          for side in selectedObject.Sides do
            if (normProperty.PropertyName(propertyName, side)) then
            begin
              field := query.FieldByName(propertyName);
              normProperty.AddValue(field.AsString, field.DataType);
              if not normProperty.fObjectProp.ContainsSide(side) then //check if we went to the default side
                break;
            end;
      end;
      query.Next;
    end;
  finally
    query.Free;
  end;
end;

{ TUSBuilderPropNone }

constructor TUSBuilderPropNone.Create(aObjectProp: TUSObjectProp);
begin
  fDistinctCount := 0;
  inherited;
end;

destructor TUSBuilderPropNone.Destroy;
begin
  inherited;
end;

function TUSBuilderPropNone.LockedAddValue(const aValue: string;
  const aDataType: TFieldType; const aCount: Integer): Boolean;
begin
  Result := False;
  if fDistinctCount = 0 then
  begin
    fValue := aValue;
    case aDataType of
      ftSmallint, ftInteger, ftLargeint, ftShortint, ftLongWord, ftWord: fDataType := 'int';
      ftBoolean: fDataType := 'bool';
      ftFloat, ftSingle: fDataType := 'float';
    else
      fDataType := 'string';
    end;
    fDistinctCount := fDistinctCount + 1;
  end
  else if fDistinctCount = 1 then
  begin
    if fValue <> aValue then
    begin
      fDistinctCount := fDistinctCount + 1;
      fValue := '';
      Result := True;
    end;
  end;
end;

function TUSBuilderPropNone.Open: Boolean;
begin
  Result := (fDistinctCount <= 1);
end;

function TUSBuilderPropNone.SidedSQLProperty(const aTableAlias: string; const aSide: Integer): string;
var
  prefix, propertyName: string;
begin
  if aTableAlias <> '' then
    prefix := aTableAlias + '.'
  else
    prefix := '';
  Result := '';
  if fObjectProp.PropertyName(propertyName, aSide) then
    Result := propertyName;
end;

function TUSBuilderPropNone.SQLProperties(aTableAlias: string): string;
var
  prefix: string;
  propertyName: string;
begin
  if aTableAlias <> '' then
    prefix := aTableAlias + '.'
  else
    prefix := '';
  Result := '';
  for propertyName in fObjectProp.SidedProperties.Values do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + prefix + propertyName;
  end;
end;

{ TUSSelectedObject }

constructor TUSSelectedObject.Create(aObjectID: string; aSide: Integer);
begin
  fObjectID := aObjectID;
  fSides := TList<Integer>.Create;
  fSides.Add(aSide);
end;

destructor TUSSelectedObject.Destroy;
begin

  inherited;
end;

{ TUSPropBuilder }

procedure TUSPropBuilder.BuildProperties(aOraSession: TOraSession;
  aSelectedIds: TArray<string>);
var
  selectedObjects: TObjectDictionary<string, TUSSelectedObject>;
  selectedIdsList: TList<string>;
  idsString, objectID, parsedID: string;
  count, side: Integer;
  builderTable: TUSBuilderTable;
begin
  selectedObjects := TObjectDictionary<string, TUSSelectedObject>.Create([doOwnsValues]);
  try
    selectedIdsList := TList<string>.Create;
    try
      count := 0;

      //parse and prepare our object ids
      for objectID in aSelectedIds do
      begin
        if objectID.StartsWith('L-') then
        begin
          parsedID := objectID.Substring(2);
          side := -1;
        end
        else if objectID.StartsWith('R-') then
        begin
          parsedID := objectID.Substring(2);
          side := 1;
        end
        else
        begin
          parsedID := objectID;
          side := 0;
        end;
        if selectedObjects.ContainsKey(parsedID) then
          selectedObjects[parsedID].Sides.Add(side) //TODO: Build in checking if this side is already in the list?
        else
          selectedObjects.Add(parsedID, TUSSelectedObject.Create(parsedID, side));
        if idsString <> '' then
          idsString := idsString + ',';
        idsString := idsString + parsedID;
        count := count + 1;
        if (count mod 999) = 0 then
        begin
          selectedIdsList.Add(idsString);
          idsString := '';
        end;
      end;
      if idsString <> '' then
        selectedIdsList.Add(idsString);
      //query the database TODO: TParallel.&For -> system is designed to work with this but TUSBuilderTable still needs concurrent handling around when a table is 'Done'
      for builderTable in fBuilderTables do
        for idsString in selectedIdsList do
          builderTable.QueryDB(aOraSession, idsString, selectedObjects);
    finally
      FreeAndNil(selectedIdsList);
    end;
  finally
    FreeAndNil(selectedObjects);
  end;
end;

constructor TUSPropBuilder.Create(aObjectProperties: TUSObjectProperties);
var
  propertyTable: TUSObjectPropTable;
begin
  fObjectProperties := aObjectProperties;
  fBuilderTables := TObjectList<TUSBuilderTable>.Create(True);
  for propertyTable in fObjectProperties.Tables.Values do
    fBuilderTables.Add(TUSBuilderTable.Create(propertyTable));
end;

destructor TUSPropBuilder.Destory;
begin

end;

function TUSPropBuilder.GetJSON: string;
var
  builderTable: TUSBuilderTable;
  builderProp: TUSBuilderProp;
begin
  Result := '';
  for builderTable in fBuilderTables do
  begin
    for builderProp in builderTable.NormProps do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '{' + builderProp.getJSON + '}';
    end;
  end;
end;

{ TUSCommitBuilder }

function TUSCommitBuilder.CommitChangesToDB(aOraSession: TOraSession;
  aUSIMBConnection: TIMBConnection; aSelectedObjects: TJSONArray;
  out aChangedCount: Integer): Boolean;
var
  sidedIDs: TObjectDictionary<Integer, TList<string>>;
  uniqueIDs: TDictionary<string, string>;
  jsonID: TJSONValue;
  side, objectId: Integer;
  idString: string;
  commitTable: TUSCommitTable;
begin
  Result := True;
  sidedIds := TObjectDictionary<Integer, TList<string>>.Create([doOwnsValues]);
  try
    uniqueIds := TDictionary<string, string>.Create;
    try
      for jsonID in aSelectedObjects do
      begin
        idString := jsonID.Value;
        if idString[2] = '-' then
        begin
          if idString[1] = 'R' then
            side := 1
          else if idString[1] = 'L' then
            side := -1
          else
            side := 0;
          idString := idString.Substring(2);
        end
        else
          side := 0;
        if  TryStrToInt(idString, objectId) and ObjectProperties.BasicLayer.Objects.ContainsKey(AnsiString(objectID.ToString)) then
        begin
          if not sidedIds.ContainsKey(side) then
            sidedIds.Add(side, TList<string>.Create);
          sidedIds[side].Add(idString);
          uniqueIds.AddOrSetValue(idString, idString);
        end;
      end;
      aChangedCount := uniqueIds.Count;
      for commitTable in fCommitTables.Values do
        Result := Result and commitTable.CommitChangesToDB(aOraSession, aUSIMBConnection, sidedIds);
    finally
      FreeAndNil(uniqueIds);
    end;
  finally
    FreeAndNil(sidedIds);
  end;
end;

constructor TUSCommitBuilder.Create(aObjectProperties: TUSObjectProperties;
  aJSONProperties: TJSONArray);
var
  objectPropTable: TUSObjectPropTable;
  jsonProperty, jsonName: TJSONValue;
  commitTable: TUSCommitTable;
begin
  fObjectProperties := aObjectProperties;
  fCommitTables := TObjectDictionary<string, TUSCommitTable>.Create([doOwnsValues]);
  for jsonProperty in aJSONProperties do
    if jsonProperty.TryGetValue<TJSONValue>('name', jsonName) then
      if aObjectProperties.TryGetPropertyTable(jsonName.Value, objectPropTable) then
      begin
        if fCommitTables.ContainsKey(objectPropTable.JoinTableName) then
          fCommitTables[objectPropTable.JoinTableName].AddCommitProperty(jsonProperty)
        else
        begin
          commitTable := TUSCommitTable.Create(objectPropTable);
          try
            if commitTable.AddCommitProperty(jsonProperty) then
            begin
              fCommitTables.Add(objectPropTable.JoinTableName, commitTable);
              commitTable := nil;
            end;
          finally
            FreeAndNil(commitTable);
          end;
        end;
      end;
end;

destructor TUSCommitBuilder.Destroy;
begin
  FreeAndNil(fCommitTables);
end;

{ TUSCommitTable }

function TUSCommitTable.AddCommitProperty(aJSONProperty: TJSONValue): Boolean;
var
  jsonName, jsonType, jsonValue: TJSONValue;
  objectProp: TUSObjectProp;
  commitProp: TUSCommitProp;
begin
  Result := False;
  if aJsonProperty.TryGetValue<TJSONValue>('name', jsonName)
    and aJsonProperty.TryGetValue<TJSONValue>('value', jsonValue)
    and aJsonProperty.TryGetValue<TJSONValue>('type', jsonType) then
  begin
    if not fCommitProperties.ContainsKey(jsonName.Value)
      and fObjectTable.Properties.TryGetValue(jsonName.Value, objectProp)
      and objectProp.Editable then
    begin
      commitProp := TUSCommitProp.Create(Objectprop);
      if commitProp.AddValue(jsonValue.Value, jsonType.Value) then
      begin
        Result := True;
        fCommitProperties.Add(jsonName.Value, commitProp);
      end
      else
        FreeAndNil(commitProp);
    end;
  end;
end;

function TUSCommitTable.CommitChangesToDB(aOraSession: TOraSession;
  aUSIMBConnection: TIMBConnection;
  aSelectedIds: TObjectDictionary<Integer, TList<string>>): Boolean;
var
  publishEvent: TIMBEventEntry;
  side, counter, objectID: Integer;
  baseQuery, scenarioPrefix, closure, id, idsString, tableName, scenarioName: string;
  valueArray: array of Variant;
  properties, prop: string;
  basicLayer: TUSBasicLayer;
  commitProperty: TUSCommitProp;
  propertyName: string;
  adjustedProperties, queryIdLists: TList<string>;
begin
  Result := True;
  basicLayer := fObjectTable.PropertiesBase.BasicLayer;
  scenarioPrefix := (basicLayer.scenario as TUSScenario).Tableprefix;
  if fObjectTable.JoinTableName <> '' then
    tableName := fObjectTable.JoinTableName
  else
    tableName := fObjectTable.PropertiesBase.BaseTableName;
  scenarioName := basicLayer.scenario.name;

  adjustedProperties := TList<string>.Create;
  try
    publishEvent := aUSIMBConnection.Publish(aOraSession.Username + '#' + scenarioName + '.' + tableName, false);
    try
      for side in aSelectedIds.Keys do
      begin
        properties := '';
        SetLength(valueArray, 0);
        adjustedProperties.Clear;
        for commitProperty in fCommitProperties.Values do
          if commitProperty.ObjectProp.PropertyName(propertyName, side) then
          begin
            if properties <> '' then
              properties := properties + ',';
            properties := properties + propertyName + '=:' + propertyName;
            adjustedProperties.Add(propertyName);
            SetLength(valueArray, Length(valueArray) + 1);
            valueArray[Length(valueArray) - 1] := commitProperty.Value;
          end;
        if properties <> '' then
        begin
          if fObjectTable.JoinTableName <> '' then
          begin
            baseQuery := 'UPDATE ' + scenarioPrefix + fObjectTable.JoinTableName +
                            ' SET ' + properties +
                            ' WHERE ' + fObjectTable.JoinID + ' IN (';
            if fObjectTable.BaseID <> 'OBJECT_ID' then
            begin
              baseQuery := baseQuery + 'SELECT t1.' + fObjectTable.JoinID + ' from' +
                            ' ' + scenarioPrefix + fObjectTable.JoinTableName + ' t1' +
                            ' join' +
                            ' ' + scenarioPrefix + fObjectTable.PropertiesBase.BaseTableName + ' t2' +
                            ' on t1.' + fObjectTable.JoinID + '=t2.' + fObjectTable.BaseID +
                            ' where t2.' + fObjectTable.BaseID + ' in(';
              closure := '))';
            end
            else
              closure := ')';
          end
          else
          begin
            baseQuery := 'UPDATE ' + scenarioPrefix + fObjectTable.PropertiesBase.BaseTableName +
                            ' SET ' + properties +
                            ' WHERE OBJECT_ID IN (';
            closure := ')';
          end;
          counter := 1;
          idsString := '';
          queryIdLists := TList<string>.Create;
          try
            for id in aSelectedIds[side] do
            begin
              if idsString <> '' then
                idsString := idsString + ',';
              idsString := idsString + id;
              if (counter mod 999) = 0 then
              begin
                queryIdLists.Add(idsString);
                idsString := '';
              end;
              counter := counter + 1;
            end;
            if idsString <> '' then
              queryIdLists.Add(idsString);
            for idsString in queryIdLists do
            begin
              aOraSession.ExecSQL(baseQuery + idsString + closure, valueArray);
              aOraSession.Commit;
            end;
            for id in aSelectedIds[side] do
              for prop in adjustedProperties do
                if TryStrToInt(id, objectID) then
                  publishEvent.SignalChangeObject(actionChange, objectID, prop);
          finally
            FreeAndNil(queryIdLists);
          end;
        end;
      end;
    finally
      publishEvent.UnPublish();
    end;
  finally
    FreeAndNil(adjustedProperties);
  end;
end;

constructor TUSCommitTable.Create(aObjectTable: TUSObjectPropTable);
begin
  fObjectTable := aObjectTable;
  fCommitProperties := TObjectDictionary<string, TUSCommitProp>.Create([doOwnsValues]);
end;

destructor TUSCommitTable.Destroy;
begin
  FreeAndNil(fCommitProperties);
end;

{ TUSCommitProp }

function TUSCommitProp.AddValue(aValue, aDataType: string): Boolean;
begin
  Result := True;
  fDataType := aDataType;
  try
    if aDataType = 'int' then
      fValue := StrToInt(aValue)
    else if aDataType = 'float' then
      fValue := StrToFloat(aValue)
    else if aDataType = 'bool' then
      fValue := StrToBool(aValue)
    else
      fValue := aValue;

    fStringValue := aValue;
  except
    on E: Exception do
    begin
     Result := False;
     //todo: logging?
    end;
  end;
end;

constructor TUSCommitProp.Create(aObjectProp: TUSObjectProp);
begin
  fObjectProp := aObjectProp;
end;

destructor TUSCommitProp.Destroy;
begin

end;

{ TUSBuilderPropSum }

constructor TUSBuilderPropSum.Create(aObjectProp: TUSObjectProp);
begin
  fCount := 0;
  inherited;
end;

destructor TUSBuilderPropSum.Destroy;
begin

  inherited;
end;

function TUSBuilderPropSum.LockedAddValue(const aValue: string;
  const aDataType: TFieldType; const aCount: Integer): Boolean;
begin
  Result := False;
  //TODO: Implement
end;

function TUSBuilderPropSum.Open: Boolean;
begin
  Result := False;
end;

function TUSBuilderPropSum.SQLProperties(aTableAlias: string): string;
begin
  Result := ''
  //TODO: Implement
end;

{ TUSControlsLayer }

constructor TUSControlsLayer.Create(aScenario: TUSScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aConnectString: string; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
begin
  fConnectString := aConnectString;
  fSourceProjection := aSourceProjection;
  fOraSession := TOraSession.Create(nil);
  fOraSession.ConnectString := fConnectString;
  fOraSession.Open;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, [], [], aDefaultLoad);
  setPreviewBase64(
    'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFAAAABQCAYAAACOEfKtAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4gUQDi064xlmkQAADKlJREFUeNrtnHl0FFUWxr9X9aqXdDqQzgJhyUISkgCCKChuI+ACbjgq4oDOoB4ZZlRERkbElR1RcRhRUBQGF' +
    'AmQyBJMEDGCCqLOKBoYIYGEkIVs3dnTa3W9+SNdoROy9ZLunLHvOTlJva7ufvXVvb93762qAAELWMACFrCABSxgAfstGultExJ4XgVCggDwAAhjzE4IMdtEsSlwujoTDlAKlN4lUJoqUNogUMoEShnleUmg9KBA6SwFoAko1Z54lOoESj+URevk54hAaUIghJ2Mp5QS4HMCTJDHIsNo7eyHIpX9wqjt51+N3JY0' +
    'vcJmh8LxciknSTdZJCn3Ny+gQ7yDBBjvGLJuWztEMeGakFb7NTbZccfDeefzzlliHEO5hLEpVrs9z98Ccn4UL9zheeMBQKEg1V9uT1JMuCaEMcZa7Rus4dnBbUkxlyWp8x1DSYyQ7QqeH/qbFJCnNJwDUuWwVShI9e4NCUEpCWoAIIRcEhhEIXAsY1Ni/M3Xh8gijmaEZCg5LsmvUeSnsM0gwE2OIfHzrUmakSl' +
    'BtCvcUEow+cYQ3a9nzPkFRRYdgHBGyL0CkGlnzPB/z8B2mCd+nZ5ME2NVzJW5mC0Spjx6Jv9Erine30zkfBm2bZmXnZrksngAoFJybN+/EuN7AxM5X4nXHvOGJardjQKiVPQOJvI+Clt3mNeliL2BicQH4rVi3ldpyXRonOth21uZyPVk2LZhniE7Ncnr4vmbiVxPideGeYY97ydqPGBer2Ui30Nh24p52duSg0' +
    'ckqWlP48gfTCQ9IF4r5h3bk0JjBykZfJhz+pKJnDfD1pl5SgUxfJWW7HPxfM1E3lviOZh3kyxexsZEzbBENfWGeEaTHeu3VpzLLTCHjhqmAYCuTgqhPGH336nT/TfPJIdzFAi5VQAOejOciZfC1rmfJ36TnkwTYlVemWCj0S6NmpRjqmuwKwCwsFBq/Hn/yL5qFddt8WcvPJ//xZF6OZwvcJI00Vv9ROJt5v17X' +
    'wodFNV52G7bqzc99UqhuqPXQ/vwDfs/TNEmxKiw7K2SpjWbyp1b+WzmfeFNq1+KDe4NTOS8yDz90V3JXYrHGENtndjpZ9fU2bWvvFlcDgB558xt50jyiyw1vYWJnIfMmyCLl7l5aPCQaFWXXm2xMvxyyqgGgH7hAsaM1KB/hHDJfge+quu/fZ+ePfVI/0s+7+GpEaG9JU/k3AzbnQBudgzZDu9IDh8+VN0Z9FhD' +
    'o715ZxtDzikjAGDm1AikvzsUfULaX8uefKmQDIlWqdYti6tVKohZq+Esm14fYvr9JF2wO7hSKTm8tzLGWcQkieO+9ERE4iHzbMezhgn9IxWdhu3JXCMm/+mUdeyoYPLO0jhh5KQcAMDhHcMwb+l5HD/Z8SXfiDBqPZV9uaK35om8i8zLcArbqqO7UrRdMQ8AwnUUahXHbUnX8+lZBhhNEgDgRK4RP51s6mIVlaT' +
    'oAQp+RFKQ1wSklLBpd+l02Ufq8ysNog5AOAi5nhJyyNUUh7jCPDls1SrOkLk5UZOSoHYpV/n2xwY8Oj8f+hrRlYM1Fh4dHaRSer1sZ2aLRGYtKHROcXI5SbrblRSHdEM8BQEyiUM8QiB+vzeFDh6gdGvW5VVWjJqcA7u9e/uvfjEGM6dG9FjV0k6eWApJGm+TpLNeWUQ4YIksHsfBnnNgOB08QMncmawkMTz2bE' +
    'G74lFKoA3mcfetFxdYpYJYe1I8AAhS83h/VaxzijMQHLdFIETrsYACx8UyYIEjbGu/3zuMD9cJbte2sxcW4Lvjje2+9sd7wnHuyGj0DbnYtPloTYICPjCVkmOfbk6Mjx6gKHMMXQuen+6xgIzj3nEoJa1aOAiDohQeVS+dLRg/nmjCB6kV2JJeBQC4+vJgXDdGCx+ZfN05ymnsfoFSjUcCEmCMo7Qqum5McF9PZ' +
    'xmsubjoxw1uzdCc00a8uLqkZfux6ZFQKnx63Z/0Cxdw4zityVEyTQRjKrcFVFEaBIA2c4KrjQwTPJ6hsyDJ8WrsXJfY6nVRbEbr6BEaXHulFv6wUSnq5hyLEA6E8G4LaBNF68X6FbxoZx5Pjudhds5nJ17bB6tfjJHa7nf8ZBMWripCWaXV5wKWVdqccyzmtoB2QAQgOYr7fudLPT8Yq5XVOPGhwmKVMHNqBJn9' +
    'YGRVy4Q44MrLNNh/qBb3/SXP9s0P9S1NCGe7UGG9ZMyjpNDxWXs+r5Uz9mo0a+BRGpMGACazFLnnQM05jyFDYGpJH1SciWtejsjyv0dH/O5qbWVzqgMUFJnZC3MG4sw5szDtiTOmjTsqjfINR+VVVhZ/w3HzqMk5jVFjf2qY+Idf686eN3sOQEIwf3mRzWZjMqs+ZYDRIwFtojhX/nvNxoq4Q8fqrR4K2OIyIVr' +
    'exnEXF/SUeLVePuNNRkkcc5lGKjp2BYJUnLBgZVHQ25vLGwtLLNYRt+SQuga7ijEEiyLT5pw29hl390mWmqGvlxnqjq3eUIaPd1c7X/hKE0XR4qkH2gDMljdmzClQfJJV7fYsT+eb+7cIlqBW8XyzgKXlVuzMNOjkReuGq7Sl467QcmoVh/xvRtPrx2rLFq0pCR5z54mO8kIy5+XCkCdfPldwocL1c/zex5XWNz' +
    'ZUSE4p2iaI4mdeqURsorgBwIqWFtPLRWTj9iqXJ5mbb4LZIjFH1VE/YmhQrFydpGboy2vq7P0dDBRfez4m1vm97yyNiwoJ5su6+o70rOohdzx8uvLrH+rFbjKPrfuwAov+cYGXtWDN3ab5tm7wr9v9QJsovgBgvbz94hulcNUTD3xda2IMSgDoG8KXJcapBABoMknSGxvKwuX9HpkWSWMGKlsd6M5Mw9n6RntUd' +
    '76nuMwaed/sPLZ1d5XU2SJDCGFvflBOlr5VxuSuFAO+YMDtVlHsdse725mqTRQfR3MjtcUTDx2rt3X3/dlH62sACABw5QhN39A+zbh58/2yJlFkLexZ9Vx0m+aDTVy+ttSlO/MZg7D7s+oqs4V1Frbk9XfLW8pSBhxkwG12URRd+a5uCyhQSmyi+ICzJ86YUyB0xxOrqm04nW+S6zjr+GtCggBAX23D2s3lLRnz' +
    '+uVxlwDs6cWFda7iYsotoUWpbyf26+jK3eY0vXXxmgs2J+YdYMB0V8Vz1QOZkyeudIWJp8+aqmvqRJlx1tsnhGoB4IEnzrSIEztIWTHlltBWt3/kFZiQfbQ+rLuOBwCzH4wsfW9FXLRC4Npl3qadVXj+tRLCWHM0MOCgBDxoF0W3rhW7db+KTRSfFyjtC+CvMhND+/Ds3tt07TYaii9YzZIEnaOrg4H9Fcg/b8Y' +
    'vp4wtR7nw8QFapYJrddQLXi0yAVB3N0ta8ezgqlnTIwe2c5M6CCHs3a2VZPGaC5KMEpl57nieRwLKnihQGgZgGgA88VIR0YVS2/hxIcKlTQROJxc4z8yKCgaA+BgVIsMoqzSI5suHBxXdeXNoq0uMFquE/+Q01XAERtZFB4gQqF+ZO8j25xn9Omwebk6rksXjnJjnkXgetaYcTGQCpetkT2xOOaLb9cT9h2vrzR' +
    'Yp5J5JOokxxhFCmN3OyOHv6vG7q7QQBK5Vn5ExJhFCvNKO+XiP3vrsihJIUsvTTgc8CdtW9b27b5QkSf6dyXOcEsANAJB1qI6EhvC4YkTrVlpirErZ5jkQwnEEQ6JVcCTUpE3IeXTXBGMMhBC2dZeeLFhZYpfFc3jeDG+I55GAbcTM5jmuH4CxAPDltw0YMljBUhLVfnuUjBDCNu2oIs+tKpUYa0aVE/O89uis1' +
    '26wdHjiMADDZU8cMzLIFjtIyftDwG179GTBq6VeZ16PCehgYpqzJ36SVcP7wxN37DNY5y8vEeVUxZHnzbD3wEPb3vRAl5jobZOZtyPDQP62rNgmSVD1BPN6TEB/M5EQwj7apSfPLCuRnJJkrzPPJwL6g4k79hnIM8tKepx5PhPQl0xMy6y2zltSbPMF83zpgT3KRJl5Oz81kHlLfMc8nwnY00wkhLBte/Rk3hLf' +
    'Ms8vAvYEE9Myq8m8JcU+Z57fBPQmE9Mzq61PLy6yMnaxtvUV8/zpgR4xUWZeelY1mbu4yCpJzW0uXzPPbwJ6ykRCCNueYSBzFxVLsuf5g3m9QsBOmGjtiInpWdVk7iL/M6/XCNgREwUeBeE6GqpWcRBFhsJiC9ZvrSxZ8s8LKlxsAPuNeZdEBnqBCZQuA/CCvK1Rc2V9QngDY2ANjfaIRqPUckGeAZ8x4CF/Ma/' +
    'XmkDprG7887ElPKWKgFodGxUoXStQWiFQanD8lFOe36vg+cEBeVyBMyGcClAGlAhYwAIWsIAFLGDt2/8A9SRhjpW+ExQAAAAASUVORK5CYII=');

  fUpdateQueue := TList<TUSUpdateQueueEntry>.Create;
  fUpdateQueueEvent := TEvent.Create(nil, False, False, '');
  fUpdateThread := TThread.CreateAnonymousThread(UpdateQueuehandler);
  fUpdateThread.NameThreadForDebugging(Scenario.ID + 'TUSControlsLayer queue handler');
  fUpdateThread.FreeOnTerminate := False;
  fUpdateThread.Start;
  fGlobalEvent := aScenario.IMBConnection.Subscribe((scenario.project as TUSProject).OraSession.Username + '.GENE_CONTROL', False);
  fScenarioEvent := aScenario.IMBConnection.Subscribe(aScenario.Federation + '.GENE_CONTROL', False);
  fGlobalEvent.OnChangeObject := handleChangeObject;
  fScenarioEvent.OnChangeObject := handleChangeObject;
  SubscribeTo((aScenario.project as TUSProject).GetTableSync('GENE_CONTROL'), handleTableChange);
  SubscribeTo((aScenario.project as TUSProject).GetTableSync(aScenario.Federation + '.GENE_CONTROL'), handleTableChange);
end;

destructor TUSControlsLayer.Destroy;
begin
  inherited;
  fUpdateThread.Terminate;
  fUpdateQueueEvent.SetEvent;
  FreeAndNil(fUpdateThread);
  FreeAndNil(fUpdateQueueEvent);
  FreeAndNil(fUpdateQueue);
  FreeAndNil(fOraSession);
end;

procedure TUSControlsLayer.handleChangeObject(aAction, aObjectID: Integer; const aObjectName, aAttribute: string);
begin
  handleTableChange(nil, aAction, aObjectID);
end;

procedure TUSControlsLayer.handleTableChange(aSender: TSubscribeObject;
  const aAction, aObjectID: Integer);
begin
  TMonitor.Enter(fUpdateQueueEvent);
  try
    begin
      try
        fUpdateQueue.Add(TUSUpdateQueueEntry.Create(aObjectID, aAction));
        fUpdateQueueEvent.SetEvent;
      except
        on e: Exception do
        begin
          Log.WriteLn('TUSControlsLayer.handleTableObject. objectid: ' + aObjectID.ToString + ', action: ' + aAction.ToString+': '+e.Message, llError);
        end
      end;
    end
  finally
    TMonitor.Exit(fUpdateQueueEvent);
  end;
end;

procedure TUSControlsLayer.handleUpdateLayerObject(aClient: TClient;
  aPayload: TJSONObject);
var
  objectID: string;
  controlID: Integer;
  contextmenuClick: TJSONObject;
  moveTo: TJSONObject;
  tag: string;
  point: TGIS_Point;
  query: TOraQuery;
  publishEvent: TIMBEventEntry;
  table: TSubscribeObject;
  usProject: TUSProject;
  usScenario: TUSScenario;
begin
  usScenario := (scenario as TUSScenario);
  usProject := (scenario.project as TUSProject);
  if aPayload.TryGetValue<string>('objectid', objectID) and  Integer.TryParse(objectID, controlID) then
  begin
    if aPayload.TryGetValue<TJsonObject>('contextmenuClick', contextmenuclick)
      and contextmenuclick.TryGetValue<string>('tag', tag) then
    begin
      if tag = tagEnable then
      begin
        usScenario.UpsertUSControlStatus(controlID, 1);
      end
      else if tag = tagDisable then
      begin
        usScenario.UpsertUSControlStatus(controlID, 0);
      end
      else if tag = tagProperties then
      begin
        ShowControlProperties(aClient, controlID);
      end
      else if tag = tagSelectObjects then
      begin
        SelectControlObjects(aClient, controlID);
      end
      else if tag = tagRemove then
      begin
        RemoveControl(controlID);
      end;
    end
    else if aPayload.TryGetValue<TJsonObject>('moveto', moveTo) then
    begin
      if moveTo.TryGetValue<Double>('lat', point.Y) and moveTo.TryGetValue<Double>('lon', point.X) then
      begin
        point := fSourceProjection.FromGeocs(point);
        query := TOraQuery.Create(nil);
        try
          query.Session := usProject.OraSession;
          query.SQL.Text := 'UPDATE GENE_CONTROL SET X = :X, Y=:Y WHERE OBJECT_ID=:OBJECT_ID';
          query.ParamByName('X').Value := point.X;
          query.ParamByName('Y').Value := point.Y;
          query.ParamByName('OBJECT_ID').Value := controlID;
          query.ExecSQL;
          query.Session.Commit;
          table := usProject.GetTableSync('GENE_CONTROL');
          publishEvent := usProject.IMB3Connection.Publish(query.Session.Username + '.GENE_CONTROL', false);
          try
            publishEvent.SignalChangeObject(actionChange, controlID, 'X');
            publishEvent.SignalChangeObject(actionChange, controlID, 'Y');
            table.SendAnonymousEvent(actionChange, controlID);
          finally
            publishEvent.UnPublish;
          end;
        finally
          query.Free;
        end;
      end;
    end
    else
      inherited;
  end;
end;

procedure TUSControlsLayer.ReadObjects(aSender: TObject);
var
  query: TOraTable;
  so: TSimpleObject;
  id: Integer;
  active: Integer;
  x: Double;
  y: Double;
  p: TWDGeometryPoint;
  name, description: string;
begin
  // todo: implement
  query := TOraTable.Create(nil);
  try
    query.Session := fOraSession;
    query.ReadOnly := True;
    query.UniDirectional := True;
    query.SQL.Text :=
      'SELECT ' +
          't1.OBJECT_ID as OBJECT_ID, '+
          't2.ACTIVE as VALUE, '+
          't1.Y as Y, '+
          't1.X as X, '+
          't1.NAME as NAME, '+
          't1.DESCRIPTION as DESCRIPTION '+
      'FROM GENE_CONTROL t1 '+
          'LEFT JOIN ' +
          (scenario as TUSScenario).Tableprefix +'GENE_CONTROL t2 '+
          'on t1.OBJECT_ID = t2.OBJECT_ID '+
      'WHERE t1.PARENT_ID is NULL';

    query.Execute;
    while not query.Eof do
    begin
      id := query.Fields[0].AsInteger;
      active := query.Fields[1].AsInteger;
      y := query.Fields[2].AsFloat;
      x := query.Fields[3].AsFloat;
      name := query.Fields[4].AsString;
      description := query.Fields[5].AsString;
      p := TWDGeometryPoint.Create(x, y, 0);
      projectGeometryPoint(p, fSourceProjection);
      so := TUSControlObject.Create(
          Self, id.ToString, p, name, description, active);
      AddObject(so, so.jsonNewObject);
      query.Next;
    end;
  finally
    query.Free;
  end;
end;

function TUSControlsLayer.RemoveControl(aControlID: Integer): Boolean;

  procedure DeleteMultipleIDs(aQuery: TOraQuery; aDeleteIDs: string; aDeleteTables: TDictionary<string, string>);
  var
    key: string;
  begin
    for key in aDeleteTables.Keys do
    begin
      aQuery.SQL.Text := 'LOCK TABLE ' + key + ' IN EXCLUSIVE MODE';
      aQuery.ExecSQL;
      aQuery.SQL.Text := 'DELETE FROM ' + key + ' WHERE ' + aDeleteTables[key] + ' IN (' + aDeleteIDs + ')';
      aQuery.ExecSQL;
    end;
  end;

var
  query: TOraQuery;
  publishEvent: TIMBEventEntry;
  table: TSubscribeObject;
  usProject: TUSProject;
  deleteIDs: TList<Integer>;
  deleteTables: TDictionary<string, string>;
  deleteString: string;
  deleteID: Integer;
  error: Boolean;
  I: Integer;
begin
  Result := False;
  deleteIDs := TList<Integer>.Create;
  try
    error := False;
    usProject := ((scenario as TUSScenario).project as TUSProject);
    usProject.OraSession.StartTransaction;
    try
      query := TOraQuery.Create(nil);
      try
        query.Session := usProject.OraSession;

        //lock the table
        query.SQL.Text := 'LOCK TABLE GENE_CONTROL IN EXCLUSIVE MODE';
        query.ExecSQL;

        //read controls we have to delete from db, so we can process them all
        deleteIDs.Add(aControlID);
        deleteString := aControlID.ToString;
        while deleteString <> '' do
        begin
          query.SQL.Text := 'SELECT OBJECT_ID FROM GENE_CONTROL WHERE PARENT_ID in (' + deleteString + ')';
          deleteString := '';
          query.ExecSQL;
          while not query.EoF do
          begin
            deleteID := query.FieldByName('OBJECT_ID').AsInteger;
            deleteIDs.Add(deleteID);
            if deleteString <> '' then
              deleteString := deleteString + ',';
            deleteString := deleteString + deleteID.ToString;
            query.Next;
          end;
        end;

        deleteTables := TDictionary<string, string>.Create;
        try
          deleteTables.Add('GENE_CONTROL', 'OBJECT_ID');
          deleteTables.Add('GENE_CONTROL_OBJECTS', 'CONTROL_ID');
          deleteTables.Add('GENE_CONTROL_PROPERTIES', 'CONTROL_ID');

          query.SQL.Text := 'SELECT TABLE_NAME FROM All_Tables WHERE OWNER=:OWNER and TABLE_NAME like :REG ORDER BY TABLE_NAME';
          query.ParamByName('OWNER').Value := query.Session.username.ToUpper;
          query.ParamByName('REG').Value := 'V%#GENE_CONTROL';
          query.ExecSQL;
          while not query.EoF do
          begin
            deleteTables.Add(query.FieldByName('TABLE_NAME').AsString, 'OBJECT_ID');
            query.Next;
          end;

          deleteString := '';
          for I := 0 to deleteIDs.Count - 1 do
          begin
            if deleteString <> '' then
              deleteString := deleteString + ',';
            deleteString := deleteString + deleteIDs[I].ToString;
            if I mod 1000 = 999 then
            begin
              DeleteMultipleIDs(query, deleteString, deleteTables);
            end;
          end;
          if deleteString <> '' then
            DeleteMultipleIDs(query, deleteString, deleteTables);
        finally
          deleteTables.Free;
        end;
      finally
        query.Free;
      end;
      usProject.OraSession.Commit;
    except
      error := True;
      usProject.OraSession.Rollback;
    end;
    if not error then
    begin
      table := usProject.GetTableSync('GENE_CONTROL');
      publishEvent := usProject.IMB3Connection.Publish(usProject.OraSession.Username + '.GENE_CONTROL', false);
      try
        for deleteID in deleteIDs do
        begin
          publishEvent.SignalChangeObject(actionDelete, deleteID);
          table.SendAnonymousEvent(actionDelete, deleteID);
        end;
      finally
        publishEvent.UnPublish;
      end;
      Result := True;
    end;
  finally
    deleteIDs.Free;
  end;
end;

procedure TUSControlsLayer.SelectControlObjects(aClient: TClient;
  aControlID: Integer);
var
  query: TOraQuery;
  idsString, geoJSONString: string;
  idsList: TList<string>;
  id: string;
  selectCategory: string;
  objectIDs: TList<string>;
  side: Integer;
  valid: Boolean;
  layer: TLayerBase;
  basicLayer: TLayer;
  layerObject: TLayerObjectWithID;
begin
  query := TOraQuery.Create(nil);
  try
    query.Session := ((Scenario as TUSScenario).project as TUSProject).OraSession;
    idsList := TList<string>.Create;
    try
      objectIDs := TList<string>.Create;
      try
        idsList.Add(aControlID.ToString);
        idsString := aControlID.ToString;
        while idsString <> '' do
        begin
          query.SQL.Text := 'SELECT OBJECT_ID FROM GENE_CONTROL WHERE PARENT_ID in (' + idsString + ')';
          idsString := '';
          query.ExecSQL;
          while not query.EoF do
          begin
            id := query.FieldByName('OBJECT_ID').AsString;
            idsList.Add(id);
            if idsString <> '' then
              idsString := idsString + ',';
            idsString := idsString + id;
            query.Next;
          end;
        end;
        for id in idsList do
        begin
          if idsString <> '' then
            idsString := idsString + ',';
          idsString := idsString + id;
        end;
        query.SQL.Text := 'SELECT OBJECT_ID, SIDE, OBJECT_TYPE FROM GENE_CONTROL_OBJECTS WHERE CONTROL_ID in (' + idsString + ')';
        idsString := '';
        geoJSONString := '';
        basicLayer := nil;
        valid := True;
        query.ExecSQL;
        while not query.EoF do
        begin
          if (selectCategory = '') or (query.FieldByName('OBJECT_TYPE').AsString = selectCategory) then
          begin
            if (selectCategory = '') and (query.FieldByName('OBJECT_TYPE').AsString <> '') then
            begin
              selectCategory := query.FieldByName('OBJECT_TYPE').AsString;
              TMonitor.Enter(scenario.Layers);
              try
                if scenario.Layers.TryGetValue(selectCategory, layer) and layer.basicLayer and (layer is TLayer) then
                begin
                  basicLayer := (layer as TLayer);
                end;
              finally
                TMonitor.Exit(scenario.Layers);
              end;
            end;
            if Assigned(basicLayer) then
            begin
              id := query.FieldByName('OBJECT_ID').AsString;
              basicLayer.objectsLock.BeginRead;
              try
                if not basicLayer.objects.TryGetValue(AnsiString(id), layerObject) then
                  layerObject := nil;
              finally
                basicLayer.objectsLock.EndRead;
              end;
              if Assigned(layerObject) then
              begin
              if query.FieldByName('SIDE').IsNull then
                begin
                  if idsString <> '' then
                    idsString := idsString + ',';
                  idsString := idsString + '"' + id + '"';
                  if geoJSONString <> '' then
                    geoJSONString := geoJSONString + ',';
                  geoJSONString := geoJSONString + layerObject.JSON2D[0, basicLayer.geometryType, ''];
                end
                else
                begin
                  side := query.FieldByName('SIDE').AsInteger;
                  if geoJSONString <> '' then
                    geoJSONString := geoJSONString + ',';
                  geoJSONString := geoJSONString + layerObject.JSON2D[side, basicLayer.geometryType, ''];
                  if side = -1 then
                  begin
                    if idsString <> '' then
                      idsString := idsString + ',';
                    idsString := idsString + '"L-' + id + '"';
                  end
                  else if side = 1 then
                  begin
                    if idsString <> '' then
                      idsString := idsString + ',';
                    idsString := idsString + '"R-' + id + '"';
                  end
                  else
                  begin
                    if idsString <> '' then
                      idsString := idsString + ',';
                    idsString := idsString + '"' + id + '"';
                  end;
                end;
              end;
            end;
          end
          else
          begin
            valid := False;
            //todo: logging?
          end;
          query.Next;
        end;
        if valid and (idsString <> '') then
        begin
          aClient.signalString('{"type":"selectedObjects","payload":{"selectCategories":["'+selectCategory+'"],'+
           '"mode":"=",'+
           '"ids":['+idsString+'],'+
           '"objects":['+geoJSONString+']}}');
        end
        else if valid then
          aClient.SendMessage('Control {' + aControlID.ToString +  '} contains no objects', mtWarning, 10000)
        else
          aClient.SendMessage('Unable to select objects of control {' + aControlID.ToString + '}', mtError, 10000);
      finally
        objectIDs.Free;
      end;
    finally
      idsList.Free;
    end;
  finally
    query.Free;
  end;

end;

procedure TUSControlsLayer.ShowControlProperties(aClient: TClient;
  aControlID: Integer);
var
  controlProperties: TUSControlProperties;
  query: TOraQuery;
begin
  query := TOraQuery.Create(nil);
  try
    query.Session := (Scenario.project as TUSProject).OraSession;
    try
      controlProperties := TUSControlProperties.Create(aControlID, query);
      aClient.signalString('{"type":"controlProperties","payload":' + controlProperties.GetJSON + '}');
    finally
      FreeAndNil(controlProperties);
    end;
  finally
    query.Free;
  end;
end;

procedure TUSControlsLayer.UpdateQueuehandler;

  procedure ReadMultipleControls(const aIdString: string; const oraSession: TOraSession);
  var
   query: TOraQuery;
   id: Integer;
   o: TSimpleObject;
   control: TUSControlObject;
   active: Integer;
   x, y: Double;
   name, description: string;
   p: TWDGeometryPoint;
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := oraSession;
      query.SQL.Text :=
        'SELECT ' +
            't1.OBJECT_ID as OBJECT_ID, '+
            't2.ACTIVE as VALUE, '+
            't1.Y as Y, '+
            't1.X as X, '+
            't1.NAME as NAME, '+
            't1.DESCRIPTION as DESCRIPTION '+
        'FROM GENE_CONTROL t1 '+
            'LEFT JOIN ' +
            (scenario as TUSScenario).Tableprefix +'GENE_CONTROL t2 '+
            'on t1.OBJECT_ID = t2.OBJECT_ID '+
        'WHERE t1.PARENT_ID is NULL and t1.OBJECT_ID in (' + aIdString + ')';
      query.UniDirectional := True;
      query.Open;
      query.First;
      TMonitor.Enter(objects);
      try
        while (not query.Eof) do
        begin
          id := query.FieldByName('OBJECT_ID').AsInteger;
          active := query.Fields[1].AsInteger;
          y := query.Fields[2].AsFloat;
          x := query.Fields[3].AsFloat;
          name := query.Fields[4].AsString;
          description := query.Fields[5].AsString;
          p := TWDGeometryPoint.Create(x, y, 0);
          projectGeometryPoint(p, fSourceProjection);
          if objects.TryGetValue(id.ToString, o) and (o is TUSControlObject) then
            begin
              control := (o as TUSControlObject);
              control.Active := active;
              control.Name := name;
              control.Description := Description;
              control.geometry := p;
              control.addOptionStructure(sojnIcon, control.getIconJSON);
              control.addOptionStructure(sojnContextMenuItems, control.getContextMenuJSON);
              control.addPropertyStructure(sojnTooltip, control.getTooltipJSON);
              UpdateObject(control, sojnGeometry, control.jsonGeometryValue);
              UpdateObject(control, sojnTooltip, control.getTooltipJSON);
              UpdateObject(control, 'options', control.jsonOptionsValue);
            end
          else
            begin
              control := TUSControlObject.Create(
                Self, id.ToString, p, name, description, active);
              AddObject(control, control.jsonNewObject);
            end;
          query.Next;
        end;
      finally
        TMonitor.Exit(objects);
      end;
    finally
      query.Free;
    end;
  end;

var
  localQueue: TList<TUSUpdateQueueEntry>;
  tempQueue: TList<TUSUpdateQueueEntry>;
  entry: TUSUpdateQueueEntry;
  changeStack: TStack<string>;
  oraSession: TOraSession;
  simpleObject: TSimpleObject;
  idsString: string;
  counter: Integer;
begin
  localQueue := TList<TUSUpdateQueueEntry>.Create;
  try
    changeStack := TStack<string>.Create;
    try
      oraSession := TOraSession.Create(nil);
      try
        oraSession.ConnectString := fConnectString;
        oraSession.Open;
        while not TThread.CheckTerminated do
        begin
          if fUpdateQueueEvent.WaitFor=wrSignaled then
          begin
            // swap queues
            TMonitor.Enter(fUpdateQueueEvent);
            try
              tempQueue := fUpdateQueue;
              fUpdateQueue := localQueue;
              localQueue := tempQueue;
            finally
              TMonitor.Exit(fUpdateQueueEvent);
            end;
            if localQueue.Count > 0 then
            begin
              for entry in localQueue do
              begin
                if entry.action = actionDelete then
                begin
                  TMonitor.Enter(objects);
                  try
                    if objects.TryGetValue(entry.objectID.ToString, simpleObject) then
                    begin
                      RemoveObject(simpleObject);
                    end;
                  finally
                    TMonitor.Exit(objects);
                  end;
                end
                else if (entry.action = actionChange) or (entry.action = actionNew) then
                begin
                  changeStack.Push(entry.objectID.ToString);
                end;
              end;
              localQueue.Clear;
              counter := 0;
              idsString := '';
              while changeStack.Count > 0 do
              begin
                if idsString <> '' then
                  idsString := idsString + ',';
                idsString := idsString + changeStack.Pop();
                counter := counter + 1;
                if counter >= 999 then
                begin
                  ReadMultipleControls(idsString, oraSession);
                  idsString := '';
                  counter := 0;
                end;
              end;
              if idsString <> '' then
                ReadMultipleControls(idsString, oraSession);
            end;
          end;
        end;
      finally
        oraSession.Free;
      end;
    finally
      changeStack.Free;
    end;
  finally
    localQueue.Free;
  end;
end;

{ TUSControlObject }

constructor TUSControlObject.Create(aSimpleLayer: TSimpleLayer; const aID: string;
  aGeometry: TWDGeometryPoint; aName, aDescription: string; aActive: Integer);
begin
  fActive := aActive;
  fName := aName;
  fDescription := aDescription;
  fID := aID;
  inherited Create(aSimpleLayer, aID, 'L.marker', aGeometry, gtPoint,
          [
             [sojnIcon, getIconJSON],
             [sojnContextMenu, 'true'],
             [sojnContextmenuItems, getContextMenuJSON], // , {"separator": true, "index": 1}
             //   //[sojnContextmenuInheritItems, t],
             [sojnInteractive, 'true'],
             [sojnDraggable, 'true']
          ],
          [[sojnTooltip, getTooltipJSON]]);
end;

destructor TUSControlObject.Destroy;
begin

  inherited;
end;

function TUSControlObject.getContextMenuJSON: string;
var
  contextOption: string;
begin
  if Active <> 0 then
    contextOption := tagDisable
  else
    contextOption := tagEnable;
  Result := '[{"text": "' + contextOption + '","index": 0, "tag":"'+contextOption+'"},' +
             '{"text": "Properties","index": 1, "tag":"'+tagProperties+'"},' +
             '{"text": "Select objects","index": 2, "tag":"'+tagSelectObjects+'"},' +
             '{"text": "Remove","index": 3, "tag":"'+tagRemove+'"}]'
end;

function TUSControlObject.getIconJSON: string;
begin
  if Active <> 0 then
    Result := '{"iconUrl":"Content/images/control-enabled.png"}'
  else
    Result := '{"iconUrl":"Content/images/control-disabled.png"}';
end;

function TUSControlObject.getTooltipJSON: string;
begin
  Result := '"' + ID + ': ' + description + '"';
end;

{ TUSControlProperties }

constructor TUSControlProperties.Create(const aControlID: Integer;
  aQuery: TOraQuery);
var
  childIDs: TList<Integer>;
  field, value: string;
  id: Integer;
begin
  fID := aControlID;
  fProperties := TDictionary<string, string>.Create;
  fChildren := TList<TUSControlProperties>.Create;

  //read name & description
  aQuery.SQL.Text := 'SELECT NAME, DESCRIPTION FROM GENE_CONTROL WHERE OBJECT_ID=:CONTROL_ID';
  aQuery.ParamByName('CONTROL_ID').Value := aControlID;
  aQuery.ExecSQL;
  if aQuery.FindFirst then
  begin
    fName := aQuery.FieldByName('NAME').AsString;
    fDescription := aQuery.FieldByName('DESCRIPTION').AsString;
  end;

  //read properties
  aQuery.SQL.Text := 'SELECT FIELD, VALUE FROM GENE_CONTROL_PROPERTIES WHERE CONTROL_ID=:CONTROL_ID';
  aQuery.ParamByName('CONTROL_ID').Value := aControlID;
  aQuery.ExecSQL;
  while not aQuery.EoF do
  begin
    field := aQuery.FieldByName('FIELD').AsString;
    value := aQuery.FieldByName('VALUE').AsString;
    fProperties.Add(field, value);
    aQuery.Next;
  end;

  //read child controls
  childIDs := TList<Integer>.Create;
  try
    aQuery.SQL.Text := 'SELECT OBJECT_ID FROM GENE_CONTROL WHERE PARENT_ID=:CONTROL_ID';
    aQuery.ParamByName('CONTROL_ID').Value := aControlID;
    aQuery.ExecSQL;
    while not aQuery.EoF do
    begin
      id := aQuery.FieldByName('OBJECT_ID').AsInteger;
      childIDs.Add(id);
      aQuery.Next;
    end;
    for id in childIDs do
    begin
      fChildren.Add(TUSControlProperties.Create(id, aQuery));
    end;
  finally
    childIDs.Free;
  end;
end;

destructor TUSControlProperties.Destroy;
begin
  FreeAndNil(fChildren);
  FreeAndNil(fProperties);
  inherited;
end;

function TUSControlProperties.GetJSON: string;
var
  prop: string;
  propertyString: string;
  childString: string;
  controlProperty: TUSControlProperties;
begin
  propertyString := '';
  for prop in fProperties.Keys do
  begin
    if propertyString <> '' then
      propertyString := propertyString + ',';
    propertyString := propertyString + '{"field":"' + prop + '","value":"' + fProperties[prop] + '"}';
  end;

  childString := '';
  for controlProperty in fChildren do
  begin
    if childString <> '' then
      childString := childString + ',';
    childString := childString + controlProperty.GetJSON;
  end;

  Result := '{"id":"' + fID.ToString + '",' +
            '"name":"' + fName + '",' +
            '"description":"' + fDescription + '",' +
            '"properties":[' + propertyString + '],' +
            '"children":[' + childString + ']}';
end;

end.
