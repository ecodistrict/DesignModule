unit SessionServerSessions;

interface

uses
  Logger,
  StdIni, CmdLin,
  IMB3NativeClient, ByteBuffers, // imb 3
  imb4,
  TimerPool,
  CommandQueue,
  WorldTilerConsts,
  FireDAC.Comp.Client,
  SessionServerLib, SessionServerDB, SessionServerUS,
  MyOraLib, DB, Ora, OraSmart, OraObjects,
  ODBFiles2, Delaunay, ESRIWorldFiles,

  Vcl.graphics, // TPicture

  System.JSON,
  System.SysConst, // parse not expanded..
  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector,
  WorldDataCode, WorldLegends,
  System.UITypes, System.Math, System.Generics.Collections, Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils;


const
  TilerNameSwitch = 'TilerName';
    DefaultTilerName = 'vps17642.public.cloudvps.com';

  TilerStatusURLSwitch = 'TilerStatusURL';

  MaxEdgeLengthInMetersSwitchName = 'MaxEdgeLengthInMeters';
    DefaultMaxEdgeLengthInMeters = 250;

  // NWB
  NWBLiveFeedProjectName = 'NWBLiveFeed';
  NWBLiveFeedRemoteHostSwitch = 'NWBLiveFeedRemoteHost';
    DefaultNWBLiveFeedRemoteHost = 'vps17642.public.cloudvps.com';
  NWBLiveFeedRemotePortSwitch = 'NWBLiveFeedRemotePort';
    DefaultNWBLiveFeedRemotePort = IMB3NativeClient.DefaultRemotePort; // 4000
  NWBLiveFeedPrefixSwitch = 'NWBLiveFeedPrefix';
    DefaultNWBLiveFeedPrefix = 'US_RT';
  NWBLiveFeedShapeFileNameSwitch = 'NWBLiveFeedShapeFileName';
    DefaultNWBLiveFeedShapeFileName = 'Wegvakken.shp'; // 'NL_receptoren.shp'; //'Rdam0102016_uitsnede.shp'; //'Wegvakken.shp';

  NWBUpdateTimeOut_ms = 5000;
  NWBCleanupCycle_ms = 24*60*60*1000; //  every day

  NWBTrafficDomain = 'Verkeer';
  NWBAirDomain = 'Lucht';

  NWBLiveFeedTilesURLSwitch = 'NWBLiveFeedTilesURL';
    DefaultNWBLiveFeedTilesURL = 'http://web-ecodistrict.tno.nl/tiler/TilerWebService.dll/tiles';

  // ecodistrict
  EcodistrictCasePrefix = 'trout_'; // Nicolas's prefix to avoid schema names starting with numbers
  EcodistrictBaseScenario = 'undefined';
  CaseVariantManagementReturnEventName = 'data-to-dashboard';

  SourceEPSGSwitch = 'SourceEPSG';

  UseScenarioHierarchySwitch = 'UseScenarioHierarchy';

  EcodistrictConnectStringSwitchName = 'EcodistrictConnectString';

type
  TProjectType = (
    ptUrbanStrategyOracle,
    ptEcoDistrict,
    ptWorld,
    ptNWBLiveFeed
  );

type
  // urban strategy

  // extra fields in meta_scenarios
    // published, null,0,1,2
  // extra fields in meta_layer
  	// domain, string
    // published, null,0,1,2,
    // diffRange, double
    // title, string, null, ".."
    // description, string, null, ".."

  TUSRoadIC = class(TGeometryLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aGeometry: TWDGeometry; aValue, aTexture: Double);
  protected
    fTexture: Double;
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
    function Encode: TByteBuffer; override;
  public
    property value2: Double read fValue2;
    property texture: Double read fTexture;
    property texture2: Double read fTexture2;
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

  TUSLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aMetaLayerEntry: TMetaLayerEntry; aDiffRange: Double; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  protected
    fLayerType: Integer;
    fMetaLayerEntry: TMetaLayerEntry; // ref
    fNewPoiCatID: Integer;
    fPoiCategories: TObjectDictionary<string, TUSPOI>;
  public
    procedure ReadObjects(aSender: TObject);
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

  TLegendFormat = (lfVertical, lfHorizontal); // todo:..

  TUSScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddBasicLayers: Boolean; aMapView: TMapView; const aTablePrefix: string);
  destructor Destroy; override;
  private
    fTableprefix: string;
    fMetaLayer: TMetaLayer;
    function getOraSession: TOraSession;
  public
    property OraSession: TOraSession read getOraSession;
    procedure ReadBasicData(); override;
    function BuildLegendJSON(aMetaLayerEntry: TMetaLayerEntry; aLayer: TUSLayer; aLegendFormat: TLegendFormat): string;
  public
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string; overload; override;

    //function selectObjectsProperties(aClient: TClient; const aSelectedCategories, aSelectedObjects: TArray<string>): string; overrride;
  end;

  TUSDBScenario = class
  constructor Create(aID: Integer; const aName, aDescription: string; aParentID, aReferenceID: Integer; const aTablePrefix, aIMBPrefix, aStatus: string; aPublished: Integer);
  private
    fID: Integer;
    fName: string;
    fDescription: string;
    fParentID: Integer;
    fParentname: string;
    fReferenceID: Integer;
    fReferenceName: string;
    fTablePrefix: string;
    fIMBPrefix: string;
    fStatus: string;
    fPublished: Integer;
  public
    procedure Relink(aUSDBScenarios: TObjectDictionary<Integer, TUSDBScenario>);
  public
    property ID: Integer read fID;
    property name: string read fName;
    property description: string read fDescription;
    property parentID: Integer read fParentID;
    property parentName: string read fParentName;
    property referenceID: Integer read fReferenceID;
    property referenceName: string read fReferenceName;
    property tablePrefix: string read fTablePrefix;
    property IMBPrefix: string read fIMBPrefix;
    property status: string read fStatus;
    property _published: Integer read fPublished;
  end;

  TUSProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean{; aSourceEPSG: Integer});
  destructor Destroy; override;
  private
    fUSDBScenarios: TObjectDictionary<Integer, TUSDBScenario>;
    fMapView: TMapView;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
    fPreLoadScenarios: Boolean;
    function getOraSession: TOraSession;
  protected
    procedure ReadScenarios;
    procedure ReadMeasures;
    function ReadScenario(const aID: string): TScenario; override;
  public
    procedure ReadBasicData(); override;
  public
    property OraSession: TOraSession read getOraSession;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;

function getUSMapView(aOraSession: TOraSession; const aDefault: TMapView): TMapView;
function getUSProjectID(aOraSession: TOraSession; const aDefault: string): string;
procedure setUSProjectID(aOraSession: TOraSession; const aValue: string);
function getUSCurrentPublishedScenarioID(aOraSession: TOraSession; aDefault: Integer): Integer;

type
  // eco-district

  TEcodistrictScenario = class(TScenario)
  public
    function AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
      aDefaultLoad: Boolean; aBasicLayer: Boolean;
      const aSchema, aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string; aDiffRange: Double): TLayer;
    procedure ReadBasicData(); override;
  public
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string; overload; override;

    function selectObjectsProperties(aClient: TClient; const aSelectedCategories, aSelectedObjects: TArray<string>): string; override;
  end;

  TEcodistrictObjectProperty = class
    category: string;
    propertyName: string;
    propertyType: string;
    selection: string;
    fieldName: string;
    tableName: string;
    keyFieldName: string;
    editable: Boolean;
  end;

  TEcodistrictProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aAddBasicLayers: Boolean);
  destructor Destroy; override;
  protected
    fObjectProperties: TObjectList<TEcodistrictObjectProperty>;
    procedure ReadObjects(aSender: TObject);
    function getMeasuresJSON: string; override;
    function ReadSchemaNames: TArray<string>;
    procedure ReadObjectProperties;
  public
    function ReadScenario(const aID: string): TScenario; override;
    procedure ReadBasicData(); override;
  end;

  TDMQuery = record
    module: string;
    SQL: string;
    ReturnType: string;
 end;

  TEcodistrictModule = class
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aConnectString, aTilerFQDN, aTilerStatusURL: string);
  destructor Destroy; override;
  private
    fSessionModel: TSessionModel;
    fConnection: TConnection;
    fDBConnection: TCustomConnection;
    fConnectString: string;
    fTilerFQDN: string;
    fTilerStatusURL: string;
    fDashboardEvent: TEventEntry;
    fDataEvent: TEventEntry;
    fModuleEvent: TEventEntry;
    fCaseVariantManagementEvent: TEventEntry;
    fProjects: TDictionary<string, TProject>;
    fQueries: TDictionary<string, TDMQuery>;
    procedure ReadDMQueries();

    function SchemaExists(aSchemaName: string): boolean;
    function SchemaCreate(aSchemaName: string; aFromSchemaName: string = 'public'): boolean;
    function SchemaDelete(aSchemaName: string): boolean;

    function GetOrAddCase(const aCaseId: string): TProject;
    procedure HandleModuleCase(const aCaseId, aCaseTitle,  aCaseDescription: string; const aMapView: TMapView);
    procedure HandleModuleCaseDelete(const aCaseId: string);
    procedure HandleModuleVariant(const aCaseId, aVariantID, aVariantName, aVariantDescription: string);
    procedure HandleModuleVariantDelete(const aCaseId, aVariantId: string);

    procedure HandleModuleEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleDataEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleCaseVariantManagentEvent(aEventEntry: TEventEntry; const aString: string);
  end;

  // NWB live feed
  TOldTilerLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; {aTilerEvent: TEventEntry; }aRefreshEvent: TIMBEventEntry; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fRefreshEvent: TIMBEventEntry;
    procedure HandleRefreshEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  public
    function SliceType: Integer; override;
  end;

  TNWBLiveFeedRoad = class(TGeometryLayerObject)
  constructor CreateFromFeed(aLayer: TLayer; var aByteBuffer: ByteBuffers.TByteBuffer);
  private
    ftime: Int64; // 1st double, is system epoch time
    //fwvk_id: Integer; // 2nd double , is integer as index into nwb shape attributes
    fVp: Double; // 3th double,
    fi: Double; // 4th double,
    fd: Double; // 5th double, density (number of vehicles/km)
    fCapacity: Double; // todo: from ?
  public
    procedure ConvertFromShape(aShape: TGIS_Shape);
    function Change(aNewRoad: TNWBLiveFeedRoad): Boolean;
  end;

  TNWBObjectList = class
  constructor Create(aOwnsObjects: Boolean);
  destructor Destroy; override;
  private
    fLock: TOmniMREW;
    fObjects: TObjectList<TLayerObject>;
  public
    property lock: TOmniMREW read fLock;
    property objects: TObjectList<TLayerObject> read fObjects;

    procedure add(aObject: TLayerObject);
    procedure remove(aObject: TLayerObject);
    procedure clear;
  end;

  TNWBLiveFeedLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription : string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    {aTilerEvent: TEventEntry; }aLiveFeedConnection: TIMBConnection; const aShapeFileName: string);
  destructor Destroy; override;
  private
    fLiveFeedConnection: TIMBConnection;
    fWVKID2UID: TDictionary<TWDID, Integer>; // own,  lookup wvk_id to shape uid
    fNWBShape: TGIS_LayerSHP;
    fUpdateTimeOut: TTimer; // ref
    fCleanupTimer: TTimer; // ref
    fObjectsAdded: TNWBObjectList; // refs
    fObjectsUpdated: TNWBObjectList; // refs
    fObjectsDeleted: TNWBObjectList; //  owns!
    fLastTimeStamp: string;
  	procedure HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
    procedure HandleUpdateTimeOut(aTimer: TTimer);
    procedure HandleCleanup(aTimer: TTimer);
  protected
    function getJSON: string; override;
  public
    function SliceType: Integer; override;
  end;

  TNWBLiveFeedScenario = class(TScenario)
  constructor Create(aProject: TProject; const aScenarioID: string;
    aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string; aAddBasicLayers: Boolean);
  destructor Destroy; override;
  private
    fLiveFeedConnection: TIMBConnection;
    kpi1: TKPI; // own
  public
  end;

  TNWBLiveFeedProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aLiveFeedConnection: TIMBConnection{IMB3}; aPalette: TWDPalette; const aShapeFilename: string);
  private
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
  public
    procedure ReadBasicData(); override;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;



function CreateSessionProject(aSessionModel: TSessionModel; const aProjectID, aProjectName: string; aProjectType: TProjectType;
  const aTilerFQDN, aTilerStatusURL, aConnectString: string): TProject;

implementation

{ utils }

function EcoDistrictSchemaId(const aCaseId: string; const aVariantId: string=''): string;
begin
  if (aVariantId='') or (aVariantId='null') or (aVariantId='None')
  then Result := EcoDistrictCasePrefix + aCaseId
  else Result := EcoDistrictCasePrefix + aCaseId + '_' + aVariantId;
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

procedure projectGeometryPoint(aGeometryPoint: TWDGeometryPoint; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
var
  p: TGIS_Point;
begin
  p.X := aGeometryPoint.x;
  p.Y := aGeometryPoint.y;
  p := aSourceProjection.ToGeocs(p);
  aGeometryPoint.x := p.X;
  aGeometryPoint.y := p.Y;
end;

procedure projectGeometry(aGeometry: TWDGeometry; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  for part in aGeometry.parts do
  begin
    for point in part.points
    do projectGeometryPoint(point, aSourceProjection);
  end;
end;


{ TEcodistrictModule }

constructor TEcodistrictModule.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aConnectString, aTilerFQDN, aTilerStatusURL: string);
begin
  // publish to dashboard
  // subscribe to modules
  // publish and subscribe to data
  inherited Create;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fConnectString := aConnectString;
  fTilerFQDN := aTilerFQDN;
  fTilerStatusURL := aTilerStatusURL;
  fProjects := TDictionary<string, TProject>.Create;//([doOwnsValues]);
  fQueries := TDictionary<string, TDMQuery>.Create();
  InitPG;
  fDBConnection := TFDConnection.Create(nil);
  SetPGConnection(fDBConnection as TFDConnection, fConnectString);
  ReadDMQueries;
  fDashboardEvent := fConnection.publish('ecodistrict.dashboard', False);
  fDataEvent := fConnection.subscribe('ecodistrict.data', False); // auto publish
  fDataEvent.OnString.Add(HandleDataEvent);
  fModuleEvent := fConnection.subscribe('ecodistrict.modules', False);
  fModuleEvent.OnString.Add(HandleModuleEvent);
  fCaseVariantManagementEvent := fConnection.subscribe('ecodistrict.'+CaseVariantManagementReturnEventName, False);
  fCaseVariantManagementEvent.OnString.Add(HandleCaseVariantManagentEvent);
end;

destructor TEcodistrictModule.Destroy;
begin
  // todo:
  FreeAndNil(fProjects);
  FreeAndNil(fQueries);
  inherited;
end;

function TEcodistrictModule.GetOrAddCase(const aCaseId: string): TProject;
begin
  // load the case as project
  if not fProjects.TryGetValue(aCaseId, Result) then
  begin
    Result := CreateSessionProject(fSessionModel, aCaseId, '', ptEcoDistrict, fTilerFQDN, fTilerStatusURL, fConnectString);
    fProjects.Add(aCaseId, Result);
  end;
  //else already loaded
end;

procedure TEcodistrictModule.HandleCaseVariantManagentEvent(aEventEntry: TEventEntry; const aString: string);
var
  jsonObject: TJSONObject;
  _type: string;
  _method: string;
  _caseId: string;
  ok: Boolean;
  _status: string;
  response: string;
  _userId: string;
  project: TProject;
begin
  try
    jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
    _type := jsonObject.getValue<string>('type');
    _method := jsonObject.getValue<string>('method');
    if _type='response' then
    begin
      _caseId := jsonObject.getValue<string>('caseId');
      _status := jsonObject.getValue<string>('status');
      _userId := jsonObject.getValue<string>('userId');
      ok := _status.StartsWith('Success');
      Log.WriteLn('HandleCaseVariantManagentEvent: type: '+_type+', method: '+_method+', case: '+_caseId+': '+_status);
      if ok then
      begin
        if _method='createCase' then
        begin
          // signal getCase
          response := '{"method": "getCase", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}';
          fDashboardEvent.signalString(response);
        end
        else if _method='deleteCase' then
        begin
          fProjects.Remove(_caseId);
        end
        else if _method='createVariant' then
        begin
          response := '{"method": "getVariants", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}';
          fDashboardEvent.signalString(response);
        end
        else if _method='deleteVariant' then
        begin
          if fProjects.TryGetValue(_caseId, project) then
          begin
            // todo:
            //project.scenarios.Remove();
          end;
        end;
      end;
      // else NOT ok..
    end;
  except
    on e: Exception
    do log.WriteLn('exception in TEcodistrictModule.HandleCaseVariantManagentEvent: '+e.Message, llError);
  end;
end;

function TEcodistrictModule.SchemaExists(aSchemaName: string): boolean;
var
  query: TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT schema_name '+
			'FROM information_schema.schemata '+
			'WHERE schema_name = '''+aSchemaName+'''';
    query.open();
    query.First;
    Result := not query.Eof;
  finally
    query.Free;
  end;
end;

function TEcodistrictModule.SchemaCreate(aSchemaName: string; aFromSchemaName: string = 'public'): boolean;
var
  query: TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text := 'SELECT clone_schema('''+aFromSchemaName+''', '''+aSchemaName+''',TRUE);';
    query.open();
    query.First;
    Result:= not query.Eof;
  finally
    query.Free;
  end;
end;

function TEcodistrictModule.SchemaDelete(aSchemaName: string): boolean;
var
  query: TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text := 'SELECT drop_schemas('''+aSchemaName+''');';
    query.open();
    query.First;
    Result:= not query.Eof;
  finally
    query.Free;
  end;
end;

procedure TEcodistrictModule.HandleDataEvent(aEventEntry: TEventEntry; const aString: string);
var
  jsonObject: TJSONObject;
//  response: string;
  jsonResponse: TJSONObject;
  datafield: string;
//  dataresponse: string;
  jsonDataResponse: TJSONObject;
  jsonList: TJSONObject;
  jsonIterator: TJSONArrayEnumerator;
  jsonKpi: TJSONValue;
  //attr_value: string;
  dataguid: string;
  thisguid: string;
  dataquery: TDMQuery;
  query: TFDQuery;
  _type: string;
  _method: string;
  _caseId: string;
  _userId: string;
  _status: string;
  _calculationId: string;
  _moduleId: string;
  _eventId: string;
  _kpiId: string;
  _kpiValueList: TJSONArray;
  _variantId: string;
  DataEvent: TEventEntry;
  _variantName: string;
  _variantDescription: string;
  _SQL: string;
//  _caseTitle: string;
//  _caseDescription: string;
begin
  try
    if not (fDBConnection as TFDConnection).ping
    then Log.Writeln('TEcodistrictModule.HandleDataEvent: ping to database returned false', llError);

    // eventId hold the eventname where the results should be published.
    // todo:
    jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
    jsonResponse := TJSONObject.Create;
    if Assigned(jsonObject.GetValue('type')) then _type := jsonObject.getValue<string>('type') else _type:='';
    if Assigned(jsonObject.GetValue('method')) then _method := jsonObject.getValue<string>('method') else _method:='';
    Log.WriteLn('HandleDataEvent: type: '+_type+', method: '+_method);
    if _type='request' then
    begin
      jsonResponse.AddPair('method', _method);
      jsonResponse.AddPair('type', 'response');
      if Assigned(jsonObject.GetValue('caseId')) then
      begin
        _caseId := jsonObject.getValue<string>('caseId');
        jsonResponse.AddPair('caseId', _CaseId);
      end
      else _caseId := '';
      if Assigned(jsonObject.GetValue('userId')) then
      begin
        _userId := jsonObject.getValue<string>('userId');
        jsonResponse.AddPair('userId', _UserId);
      end
      else _userId :='';
      if Assigned(jsonObject.GetValue('variantId')) then
      begin
        _variantId := jsonObject.getValue<string>('variantId');
        jsonResponse.AddPair('variantId', _variantId);
      end
      else _variantId := '';
      if Assigned(jsonObject.GetValue('calculationId')) then
      begin
        _calculationId := jsonObject.getValue<string>('calculationId');
        jsonResponse.AddPair('calculationId', _calculationId);
      end
      else _calculationId := '';
      if Assigned(jsonObject.GetValue('moduleId')) then
      begin
        _moduleId := jsonObject.getValue<string>('moduleId');
        jsonResponse.AddPair('moduleId', _moduleId);
      end
      else _moduleId := '';
      if Assigned(jsonObject.GetValue('kpiId')) then
      begin
        _kpiId := jsonObject.getValue<string>('kpiId');
        jsonResponse.AddPair('kpiId', _kpiId);
      end
      else _kpiId := '';
      if Assigned(jsonObject.GetValue('eventId')) then
      begin
        _eventId := jsonObject.getValue<string>('eventId');
      end
      else _eventId := 'ecodistrict.data-to-dashboard';
      if _eventId='' then _eventId:='data-to-dashboard';
      jsonResponse.AddPair('eventId', _eventId);
      DataEvent:=fConnection.publish('ecodistrict.' + _eventId, false);
      jsonResponse.AddPair('status','<undefined>');
      if _method='createCase' then
      begin
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if (_variantId='') or (_variantId='null') or (_variantId='None') then
          begin
            if SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
            begin
              _status := 'Success - schema already created before';
            end
            else
            begin
              _status := 'In progress - creating schema';
              jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
              DataEvent.signalString(JSONresponse.ToString);
              SchemaCreate(EcoDistrictSchemaId(_caseId));
              _status := 'Success - schema created';
            end;
            // todo: module: new case but we do not know the title, description or mapView;
            //HandleModuleCase(_caseId, '', '', )
            // signal getCase to get the module part up-to-date
            fDashboardEvent.signalString('{"method": "getCase", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}');
            {
            // todo: needing the polygon also..
            try
              _caseTitle  := jsonObject.getValue<string>('title');
              _caseDescription := jsonObject.getValue<string>('description');
              //HandleModuleCase(_caseId, _caseName, _caseDescription, mapView);
            except
              on e: Exception
              do Log.WriteLn('TEcodistrictModule.HandleDataEvent: exception handling module part of creating a case: '+e.Message, llError);
            end;
            }
          end
          else _status := 'failed - not supposed to have a variant id';
        end
        else _status := 'failed - no case id';
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        DataEvent.signalString(JSONresponse.ToString);
        Log.WriteLn(_status);
      end
      else
      if _method='deleteCase' then
      begin
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if (_variantId='') or (_variantId='null') or (_variantId='None') then
          begin
            if SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
            begin
              _status := 'Success - schema already deleted before';
            end
            else
            begin
              _status := 'In progress - deleting cascading schemas';
              jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
              DataEvent.signalString(JSONresponse.ToString);
              SchemaDelete(EcoDistrictSchemaId(_caseId, _variantId));
              _status := 'Success - schema deleted';
            end;
            HandleModuleCaseDelete(_caseId);
          end
          else _status := 'failed - not supposed to have a variant id';
        end
        else _status := 'failed - no case id';
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        DataEvent.signalString(JSONresponse.ToString);
        Log.WriteLn(_status);
      end
      else
      if _method='createVariant' then
      begin
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if not ((_variantId='') or (_variantId='null') or (_variantId='None')) then
          begin
            if SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
            begin
              _status := 'Success - schema already created before';
            end
            else
            begin
              if not SchemaExists(EcoDistrictSchemaId(_caseId)) then
              begin
                _status := 'failed - case schema does not exist';
              end
              else
              begin
                _status := 'In progress - creating variant';
                jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
                DataEvent.signalString(JSONresponse.ToString);
                SchemaCreate(EcoDistrictSchemaId(_caseId, _variantId), EcoDistrictSchemaId(_caseId));
                _status := 'Success - variant created';
              end;
            end;
            // todo: module: new variant but we do not know the title, description
            //HandleModuleVariant(_caseId, _variantId, '', '');
            // signal getVariants to get the module part up-to-date
            //fDashboardEvent.signalString('{"method": "getVariants", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}');
            try
              _variantName  := jsonObject.getValue<string>('name');
              _variantDescription := jsonObject.getValue<string>('description');
              HandleModuleVariant(_caseId, _variantId, _variantName, _variantDescription);
            except
              on e: Exception
              do Log.WriteLn('TEcodistrictModule.HandleDataEvent: exception handling module part of creating a variant: '+e.Message, llError);
            end;
          end
          else _status := 'failed - no variant id';
        end
        else _status := 'failed - no case id';
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        DataEvent.signalString(JSONresponse.ToString);
        Log.WriteLn(_status);
      end
      else
      if _method='deleteVariant' then
      begin
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if not ((_variantId='') or (_variantId='null') or (_variantId='None')) then
          begin
            if not SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
            begin
              _status := 'Success - variant already deleted before';
            end
            else
            begin
              _status := 'In progress - deleting variant';
              jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
              DataEvent.signalString(JSONresponse.ToString);
              SchemaDelete(EcoDistrictSchemaId(_caseId, _variantId));
              _status := 'Success - variant deleted';
            end;
            HandleModuleVariantDelete(_caseId, _variantId);
          end
          else _status := 'failed - no variant id';
        end
        else _status := 'failed - no case id';
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        DataEvent.signalString(JSONresponse.ToString);
        Log.WriteLn(_status);
      end
      else
      if _method='getData' then
      begin
        jsonDataResponse:=TJSONObject.Create;
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if not ((_moduleId = 'null') or (_moduleId = '')) then
          begin
            if not SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
            begin
              _status := 'failed - no schema found for case and variant';
            end
            else
            begin
              Log.WriteLn('getData queries for '+_moduleId);
              _status := 'Success';
              for datafield in fQueries.Keys do
              begin
                if fQueries.TryGetValue(datafield, dataquery) then
                begin
                  if (dataquery.module.Contains(_moduleId)) then
                  begin
                    //(fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''');
                    //try
                      query := TFDQuery.Create(nil);
                      try
                        Log.WriteLn(datafield, llNormal, 1);
                        query.Connection := fDBConnection as TFDConnection;
  //insert variables into query
                        _SQL:= ReplaceStr(dataquery.SQL,'{case_id}', EcoDistrictSchemaId(_caseId)); // todo (HC): should this not be schema_id? or not being used at all in a query?
  //end variables insert
                        _SQL := 'SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''; ' + _SQL;
                        Log.WriteLn(_SQL, llNormal, 1);
                        query.SQL.Text := _SQL;
                        query.Open();
                        try
                          query.First;
                          if dataquery.ReturnType='INT' then
                          begin
                            if not query.Eof then
                            begin
  //                            if dataresponse<>'' then dataresponse:=dataresponse+ ',';
  //                            dataresponse:=dataresponse + '"'+ datafield + '": "'+query.Fields[0].AsInteger.toString()+'"';
                              jsonDataResponse.AddPair(datafield,query.Fields[0].AsInteger.toString());
                            end;
                          end
                          else
                          if dataquery.ReturnType='FLOAT' then
                          begin
                            if not query.Eof then
                            begin
  //                            if dataresponse<>'' then dataresponse:=dataresponse+ ',';
  //                            dataresponse:=dataresponse + '"'+ datafield + '": "'+query.Fields[0].AsFloat.toString()+'"';
                              jsonDataResponse.AddPair(datafield,query.Fields[0].AsFloat.toString());
                            end;
                          end
                          else
                          if dataquery.ReturnType='GEOJSON' then
                          begin

                          end
                          else
                          if dataquery.ReturnType='LIST' then
                          begin
                            jsonList:=TJSONObject.Create;
                            dataguid:='';
  //we expect the query to return: attr_gml_id, attr_name, string_value, double_value, int_value
                            while not query.Eof do
                            begin
                              thisguid:=query.FieldByName('attr_gml_id').AsString;
                              if (dataguid<>thisguid) then
                              begin
                                if dataguid='' then
                                begin
                                  dataguid:=thisguid;
                                end;
                                jsonList.AddPair('gml_id', thisguid);
                              end;
                              if not query.FieldByName('string_value').IsNull
                              then jsonList.AddPair(query.FieldByName('attr_name').AsString, query.FieldByName('string_value').AsString)
                              else if not query.FieldByName('double_value').IsNull
                              then jsonList.AddPair(query.FieldByName('attr_name').AsString, TJSONNumber.Create(StrToFloat(query.FieldByName('double_value').AsString, dotFormat)))
                              else if not query.FieldByName('int_value').IsNull
                              then jsonList.AddPair(query.FieldByName('attr_name').AsString, TJSONNumber.Create(query.FieldByName('int_value').AsLargeInt));
                              //jsonList.AddPair(query.FieldByName('attr_name').AsString, attr_value);
                              query.Next;
                            end;
                            jsonDataResponse.AddPair(datafield,jsonList);
                          end;
                        finally
                          query.Close;
                        end;
                      finally
                        query.Free;
                      end;
                    //finally
                    //  (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''public''');
                    //end;
                  end;
                end;
              end;
            end;
          end
          else _status := 'failed - no module id';
        end
        else _status := 'failed - no case id';
//        response := '{"method": "'+_method+'", "type": "response", "userId": "'+_UserId+'", "caseId": "'+_CaseId+'", "variantId": "'+_variantId+'", "calculationId": "'+_calculationId+'", "moduleId": "'+_moduleId+'", "data": {'+dataresponse+'}, "status": "'+_status+'"}';
//        DataEvent.signalString(response);
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        if Assigned(jsonDataResponse) then jsonResponse.AddPair('data',jsonDataResponse);
        DataEvent.signalString(jsonResponse.ToString);
        Log.WriteLn(_status);
        FreeAndNil(jsonResponse);
      end
      else
      if _method='setKpiResult' then
      begin
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if not ((_moduleId = 'null') or (_moduleId = '')) then
          begin
            if not ((_kpiId = 'null') or (_kpiId = '')) then
            begin
              if not SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
              begin
                _status := 'failed - no schema found for case and variant';
              end
              else
              begin
                if Assigned(jsonObject.GetValue('kpiValueList')) then _kpiValueList := jsonObject.getValue<TJSONArray>('kpiValueList') else _kpiValueList:=nil;
                jsonIterator :=TJSONArrayEnumerator.Create(_kpiValueList);
                while jsonIterator.MoveNext do
                begin
                  jsonKpi:=jsonIterator.GetCurrent
                end;
(*
                query := TFDQuery.Create(nil);
                try
                  query.Connection := fDBConnection as TFDConnection;
//insert variables into query
//                  dataquery.SQL:= ReplaceStr(dataquery.SQL,'{case_id}', EcoDistrictSchemaId(_caseId)); // todo (HC): should this not be schema_id? or not being used at all in a query?
//end variables insert
                  query.SQL.Text :=  'SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''; ' +
                                    'DELETE';
                  query.Open();
                  try
                    query.First;
                  finally
                    query.Close;
                  end;

                finally
                  query.Free;
                end;
*)
              end;
            end
            else _status := 'failed - no kpiId found in request';
          end
          else _status := 'failed - no module id';
        end
        else _status := 'failed - no case id';
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        DataEvent.signalString(JSONresponse.ToString);
        Log.WriteLn(_status);
      end
      else
      if _method='getKpiResult' then
      begin
        if not ((_caseId = 'null') or (_caseId = '')) then
        begin
          if not ((_moduleId = 'null') or (_moduleId = '')) then
          begin
            if not ((_kpiId = 'null') or (_kpiId = '')) then
            begin
              if not SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
              begin
                _status := 'failed - no schema found for case and variant';
              end
              else
              begin

              end;
            end
            else _status := 'failed - no kpiId found in request';
          end
          else _status := 'failed - no module id';
        end
        else _status := 'failed - no case id';
        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
        DataEvent.signalString(JSONresponse.ToString);
        Log.WriteLn(_status);
      end
      else
      if _method='getGeojson' then
      begin

      end
      else
      begin
        Log.WriteLn('HandleDataEvent: unknown type/method: '+_type+', method: '+_method,llWarning);
      end;
    end;
  except
    on e: exception
    do log.WriteLn('exception in TEcodistrictModule.HandleDataEvent: '+e.Message, llError);
  end;
end;

procedure TEcodistrictModule.HandleModuleCase(const aCaseId, aCaseTitle,  aCaseDescription: string; const aMapView: TMapView);
var
  project: TProject;
  scenario: TScenario;
begin
  project := GetOrAddCase(aCaseId);
  project.ProjectName := aCaseTitle;
  project.projectDescription := aCaseDescription;
  project.mapView := aMapView;
  // update map view of all scenarios
  TMonitor.Enter(project.scenarios);
  try
    for scenario in project.scenarios.values
    do scenario.mapView := project.mapView;
    // add base scenario
    if not project.scenarios.ContainsKey(EcodistrictBaseScenario) then
    begin
      scenario := TEcodistrictScenario.Create(project, project.ProjectID, project.ProjectName, project.ProjectDescription, project.addBasicLayers, (project as TEcodistrictProject).mapView);
      project.scenarios.Add(EcodistrictBaseScenario, scenario);
      Log.WriteLn('added base scenario '+EcodistrictBaseScenario+': '+project.ProjectName+', '+project.ProjectDescription, llNormal, 1);
    end
    else
    begin
      Log.WriteLn('already contains base scenario '+EcodistrictBaseScenario, llNormal, 1);
    end;
  finally
    TMonitor.Exit(project.scenarios);
  end;
end;

procedure TEcodistrictModule.HandleModuleCaseDelete(const aCaseId: string);
begin
  // todo: implement
end;

procedure TEcodistrictModule.HandleModuleEvent(aEventEntry: TEventEntry; const aString: string);
var
  // json data procesing
  _jsonObject: TJSONObject;
  _type: string;
  _method: string;
  _caseId: string;
  _userId: string;
  _variants: TJSONArray;
  _variant: TJSONObject;
  _variantId: string;
  _variantName: string;
  _variantDescription: string;
  _title: string;
  _polygons: TJSONArray;
  _polygon: TJSONArray;
  _description: string;
  _coordinate: TJSONArray;
  _lat: Double;
  _lon: Double;
  // normal variables
  i: Integer;
  j: Integer;
  response: string;
  extent: TWDExtent;
  mapView: TMapView;
begin
  try
    _jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
    _type := _jsonObject.getValue<string>('type');
    _method := _jsonObject.getValue<string>('method');
    Log.WriteLn('HandleModuleEvent: type: '+_type+', method: '+_method);
    // process request from dashboard
    if _type='request' then
    begin
      if _method='getModules' then
      begin
        // respond to getModules with this modules info
        response :=
          '{'+
            '"name": "Design/Data Module",'+
            '"description": "Design and view layer based information and apply measures",'+
            '"kpiList": [],'+
            '"moduleId": "design_data",'+
            '"method": "getModules",'+
            '"type": "response"'+
          '}';
        fDashboardEvent.signalString(response);
      end
      else if _method='initModule' then
      begin
        _caseId := _jsonObject.getValue<string>('caseId');
        _userId := _jsonObject.getValue<string>('userId');
        // signal getCase
        response := '{"method": "getCase", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}';
        fDashboardEvent.signalString(response);
        // signal getVariants request
        response := '{"method": "getVariants", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}';
        fDashboardEvent.signalString(response);
        {project := }GetOrAddCase(_caseId);
      end;
    end
    // process respons from dashboard
    else if _type='response' then
    begin
      if _method='getCase' then
      begin
        _caseId := _jsonObject.getValue<string>('caseId');
        _userId := _jsonObject.getValue<string>('userId');
        // parse case data and add case (ie base case scenario on the project)
        _title := _jsonObject.getValue<string>('caseData.title', '');
        _description := _jsonObject.getValue<string>('caseData.description', '');
        _polygons := _jsonObject.getValue<TJSONArray>('caseData.districtPolygon.geometry.coordinates');
        extent := TWDExtent.Create;
        for i := 0 to _polygons.Count-1  do
        begin
          _polygon := _polygons.Items[i] as TJSONArray;
          for j := 0 to _polygon.Count-1 do
          begin
            _coordinate := _polygon.Items[j] as TJSONArray;
            _lat := Double.Parse(_coordinate.Items[1].ToString, dotFormat);
            _lon := Double.Parse(_coordinate.Items[0].ToString, dotFormat);
            extent.Expand(_lon, _lat);
          end;
        end;
        if not extent.IsEmpty
        then mapView := TMapView.Create(extent.centerY, extent.centerX, ZoomLevelFromDeltaLon(1.1*Abs(extent.xMax-extent.xMin)))
        else mapView := TMapView.Create(55.7, 41, 4); //  whole of europe?
        HandleModuleCase(_caseId, _title, _description, mapView);
      end
      else if (_method='getVariants') then
      begin
        _caseId := _jsonObject.getValue<string>('caseId');
        _userId := _jsonObject.getValue<string>('userId');
        // parse variants and load them as scenarios
        _variants := _jsonObject.getValue<TJSONArray>('variants');
        for i := 0 to _variants.Count-1  do
        begin
          _variant := _variants.Items[i] as TJSONObject;
          //_id, name, description
          _variantId := _variant.GetValue<string>('_id');
          _variantName := _variant.GetValue<string>('name');
          _variantDescription := _variant.GetValue<string>('description');
          HandleModuleVariant(_caseId, _variantId, _variantName, _variantDescription);
        end;
      end;
    end;
  except
    on e: exception do log.WriteLn('exception in TEcodistrictModule.HandleModuleEvent: '+e.Message, llError);
  end;
end;

procedure TEcodistrictModule.HandleModuleVariant(const aCaseId, aVariantID, aVariantName, aVariantDescription: string);
var
  project: TProject;
  scenario: TScenario;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    TMonitor.Enter(project.scenarios);
    try
      if project.scenarios.TryGetValue(aVariantId, scenario) then
      begin
        // scenario already defined
        scenario.name := aVariantName;
        scenario.description := aVariantDescription;
        Log.WriteLn('existing scenario '+aVariantId+': '+aVariantName+', '+aVariantDescription, llNormal, 1);
      end
      else
      begin
        // add scenario
        scenario := TEcodistrictScenario.Create(project, aVariantId, aVariantName, aVariantDescription, project.addBasicLayers, (project as TEcodistrictProject).mapView);
        project.scenarios.Add(aVariantId, scenario);
        Log.WriteLn('added scenario '+aVariantId+': '+aVariantName+', '+aVariantDescription, llNormal, 1);
      end;
    finally
      TMonitor.Exit(project.scenarios);
    end;
  end;
end;

procedure TEcodistrictModule.HandleModuleVariantDelete(const aCaseId, aVariantId: string);
begin
  // todo: implement
end;

procedure TEcodistrictModule.ReadDMQueries;
var
  query: TFDQuery;
  DMQuery: TDMQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    // todo: * will not pin order of fields !
    query.SQL.Text :=
      'SELECT object_id, returntype, request, query, module '+
			'FROM public.dm_queries';
    query.open();
    try
      query.First;
      while not query.Eof do
      begin
        DMQuery.module:=query.Fields[4].AsString;
        DMQuery.SQL:=query.Fields[3].AsString;
        DMQuery.ReturnType:=query.Fields[1].AsString;
        fQueries.Add(query.Fields[2].AsString,DMQuery);
        query.Next;
      end;
    except
      on e: exception
      do log.WriteLn('exception in TEcodistrictModule.Create: '+e.Message, llError);
    end;
  finally
    query.Free;
  end;
end;

{ TEcodistrictScenario }

function TEcodistrictScenario.AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
  aDefaultLoad: Boolean; aBasicLayer: Boolean; const aSchema, aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string; aDiffRange: Double): TLayer;
begin
  Result := TLayer.Create(self, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, aDiffRange, aBasicLayer);
  try
    Result.query := PGSVGPathsQuery('"'+aSchema+'".'+aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName);
    //AddCommandToQueue(sl, Self.ReadObjects);
    (fProject as TEcodistrictProject).ReadObjects(Result);
    // todo: other palette types..?
    Layers.Add(Result.ID, Result);
  except
    on E: Exception do
    begin
    	Log.WriteLn('Could not load layer '+aDescription+': '+E.message, llError);
      FreeAndNil(Result);
    end;
  end;
end;

procedure TEcodistrictScenario.ReadBasicData;
var
  schema: string;
  entries: TPaletteDiscreteEntryArray;
  layer: TLayer;
  palette: TWDPalette;
  legendJSON: string;
begin
  // read ecodistrict data
  if fID=fProject.ProjectID
  then schema := EcoDistrictSchemaId(fID)
  else schema := EcoDistrictSchemaId(fProject.ProjectID, fID);
  // buildings              -> v
  //AddLayerFromTable('basic structures', 'buildings', 'buildings', 'basic buildings', '"building"', 'MultiPolygon', false, True, schema, 'bldg_building', 'attr_gml_id', 'bldg_lod1multisurface_value');
  AddLayerFromTable('basic structures', 'building',  'buildings', 'basic buildings',    '"building"',   'MultiPolygon', false, True, schema, '"bldg_building"', 'attr_gml_id', 'bldg_lod0footprint_value', '', NaN);
  //AddLayerFromTable('basic structures', 'building', 'buildings', 'basic buildings', '"building"', 'MultiPolygon', false, True, schema, 'bldg_building_import', 'gid', 'geom');
  AddLayerFromTable('basic structures', 'parking',   'parkings',  'basic parking lots', '"parking"',    'MultiPolygon', false, True, schema, '"green_import" where layer=''PARKING''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'trees  5m', 'trees  5m', 'trees  5m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$5M''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'trees 10m', 'trees 10m', 'trees 10m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$10M''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'trees 15m', 'trees 15m', 'trees 15m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$15M''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'trees 20m', 'trees 20m', 'trees 20m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$20M''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'grass',     'grass',     'grass areas',        '"grass area"', 'MultiPolygon', false, True, schema, '"green_import" where layer=''GRASS_AREA''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'water',     'water',     'water areas',        '"water area"', 'MultiPolygon', false, True, schema, '"green_import" where layer=''WATER''', 'gid', 'geom', '', NaN);
  AddLayerFromTable('basic structures', 'bushes',    'bushes',    'bushes',             '"bush"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''BUSHES''', 'gid', 'geom', '', NaN);

  // building energy label
  setLength(entries, 6);
  with entries[0] do begin colors:=TGeoColors.Create($FF038B42); minValue:=0; maxValue:=10; description:='A'; end;
  with entries[1] do begin colors:=TGeoColors.Create($FF00A650); minValue:=10; maxValue:=15; description:='B'; end;
  with entries[2] do begin colors:=TGeoColors.Create($FF71BF45); minValue:=15; maxValue:=30; description:='C'; end;
  with entries[3] do begin colors:=TGeoColors.Create($FFFFDD00); minValue:=30; maxValue:=45; description:='D'; end;
  with entries[4] do begin colors:=TGeoColors.Create($FFFCB913); minValue:=45; maxValue:=60; description:='E'; end;
  with entries[5] do begin colors:=TGeoColors.Create($FFED1B24); minValue:=60; maxValue:=70; description:='F'; end;
  palette := TDiscretePalette.Create('Energy label', entries, TGeoColors.Create(TAlphaColors.Red and not TAlphaColors.Alpha));
  legendJSON := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical); // todo: parameterize

  layer := AddLayerFromTable('Energy', 'EnergyLabel', 'Energy labels', 'Energy labels of buildings', '"building"', 'MultiPolygon', false,
    false, schema, 'bldg_building join "'+schema+'".bldg_building_energylabel on attr_gml_id=id',
    'attr_gml_id', 'bldg_lod0footprint_value', 'kwh_m2_year', 30);

  if Assigned(layer) then
  begin
    layer.legendJSON := legendJSON;
    layer.RegisterOnTiler(False, stGeometry, layer.name, NaN, palette);
  end;

  // green
  (*
  setLength(entries, 5);
  with entries[0] do begin color:=$FF92D050; minValue:=0; maxValue:=1; description:='Grass area'; end;
  with entries[1] do begin color:=$FF00B050; minValue:=10; maxValue:=15; description:='Trees'; end;
  with entries[2] do begin color:=$FFC5D9F1; minValue:=15; maxValue:=30; description:='Water surfaces'; end;
  with entries[3] do begin color:=$FF92D050; borderColor := $FF; minValue:=30; maxValue:=45; description:='Green roofs'; end;
  with entries[4] do begin color:=$FFFCB913; minValue:=45; maxValue:=60; description:='Permeable surfaces'; end;
  layer := AddLayerFromTable('Energy', 'EnergyLabel', 'Energy labels', 'Energy labels of buildings', '"building"', 'MultiPolygon', false,
    TDiscretePalette.Create('Energy label', entries, TAlphaColors.Red and not TAlphaColors.Alpha),
    false, schema, 'bldg_building join "'+schema+'".bldg_building_energylabel on attr_gml_id=id',
    'attr_gml_id', 'bldg_lod0footprint_value', 'kwh_m2_year');
  if Assigned(layer) then
  begin
    layer.legendJSON := 'grid: [
            {
                'Wilderness Areas': 'LightSeaGreen',
                'Bureau of Land Management, National Monument': 'LightSalmon'
            },
            {
                'Indian Reservation': 'LightYellow',
                'Fish and Wildlife Service': 'SteelBlue'
            },
            {
                'National Park Service': 'Sienna',
                'National Forests & Grasslands': 'LightGreen'
            }
        ]'
    layer.RegisterOnTiler(False, stGeometry, layer.name);
  end;
  *)

  // mobility
  {
    "Flex working"
    "Higher frequency tram and bus services"
    "Modification of tram and bus routes to connect to P and R"
    "Promotion of public transport"
    "Combine tram and bus infrastructure"
    "Optimisation of bus route"
    "P and R"
    "Parking zone policy"
    "Larger tram and bus vehicles"
    "Mixed use planning"
  }

  // green
  {
    "Medium large trees"
    "Large trees"
    "Water surfaces"
    "Small trees"
    "General bushes"
    "Water surface permanent"
    "Total land area"
    "Unsupported ground greenery"
    "Green roof (50 - 300 mm)"
    "Open hard surfaces that allow water to get through"
    "Impermeable surfaces"
  }



     // not selectable energy infrastructure  ->
     // not selectable green spaces           -> v
     // not selectable hard surfaces          ->  v
     // not selectable trees                  ->  v

  // tram lines             ->  ?
  // bus lines              ->  ?
  // tram stops             ->
  // bus stops              ->
  // parking spaces         ->  v

  // roads

  // trees 5/10/.. meter layers

  // energy label color -> kevins ppt



end;

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string;
var
  layers: TList<TLayer>;
  categories: string;
  objectsGeoJSON: string;
  totalObjectCount: Integer;
  extent: TWDExtent;
  l: TLayer;
  objectCount: Integer;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
    begin
      categories := '';
      objectsGeoJSON := '';
      totalObjectCount := 0;
      extent := TWDExtent.FromGeometry(aGeometry);
      for l in layers do
      begin
        objectCount := l.findObjectsInGeometry(extent, aGeometry, objectsGeoJSON);
        if objectCount>0 then
        begin
          if categories=''
          then categories := '"'+l.id+'"'
          else categories := categories+',"'+l.id+'"';
          totalObjectCount := totalObjectCount+objectCount;
        end;
      end;
      Result :=
        '{"selectedObjects":{"selectCategories":['+categories+'],'+
         '"mode":"'+aMode+'",'+
         '"objects":['+objectsGeoJSON+']}}';
      Log.WriteLn('select on geometry:  found '+totalObjectCount.ToString+' objects in '+categories);
    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string;
var
  layers: TList<TLayer>;
  dist: TDistanceLatLon;
  nearestObject: TLayerObject;
  nearestObjectLayer: TLayer;
  nearestObjectDistanceInMeters: Double;
  l: TLayer;
  o: TLayerObject;
  categories: string;
  objectsGeoJSON: string;
  totalObjectCount: Integer;
  objectCount: Integer;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
    begin
      dist := TDistanceLatLon.Create(aY, aY);
      if (aRadius=0) then
      begin
        nearestObject := nil;
        nearestObjectLayer := nil;
        nearestObjectDistanceInMeters := Infinity;
        for l in layers do
        begin
          o := l.findNearestObject(dist, aX, aY, nearestObjectDistanceInMeters);
          if assigned(o) then
          begin
            nearestObjectLayer := l;
            nearestObject := o;
            if nearestObjectDistanceInMeters=0
            then break;
          end;
        end;
        if Assigned(nearestObject) then
        begin
          // todo:
          Result :=
            '{"selectedObjects":{"selectCategories":["'+nearestObjectLayer.ID+'"],'+
             '"mode":"'+aMode+'",'+
             '"objects":['+nearestObject.GeoJSON2D[nearestObjectLayer.geometryType]+']}}';
          Log.WriteLn('found nearest object layer: '+nearestObjectLayer.ID+', object: '+string(nearestObject.ID)+', distance: '+nearestObjectDistanceInMeters.toString);
        end;
      end
      else
      begin
        categories := '';
        objectsGeoJSON := '';
        totalObjectCount := 0;
        for l in layers do
        begin
          objectCount := l.findObjectsInCircle(dist, aX, aY, aRadius, objectsGeoJSON);
          if objectCount>0 then
          begin
            if categories=''
            then categories := '"'+l.id+'"'
            else categories := categories+',"'+l.id+'"';
            totalObjectCount := totalObjectCount+objectCount;
          end;
        end;
        Result :=
          '{"selectedObjects":{"selectCategories":['+categories+'],'+
           '"mode":"'+aMode+'",'+
           '"objects":['+objectsGeoJSON+']}}';
        Log.WriteLn('select on radius:  found '+totalObjectCount.ToString+' objects in '+categories);
      end;
    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string;
var
  layers: TList<TLayer>;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
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

function TEcodistrictScenario.selectObjectsProperties(aClient: TClient; const aSelectedCategories, aSelectedObjects: TArray<string>): string;
// todo: implement
var
  layers: TList<TLayer>;
  l: TLayer;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
    begin
      // find aSelectedObjects in layers and get properties
      for l in layers do
      begin
//        if Result<>''
//        then Result := Result+',';
//        Result := Result+'{"id":"'++'",'++'}';
        // get properties per category

      end;
    end;
    // todo: else warning?

    if Result<>''
    then Result := '"selectedObjectsProperties":{"objects":['+Result+']}';
  finally
    layers.Free;
  end;
end;

{ TEcodistrictProject }

constructor TEcodistrictProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aDBConnection: TCustomConnection; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled,
  aMeasuresHistoryEnabled, aAddBasicLayers: Boolean);
begin
  inherited;
  fObjectProperties := TObjectList<TEcodistrictObjectProperty>.Create;
end;

destructor TEcodistrictProject.Destroy;
begin
  FreeAndNil(fObjectProperties);
  inherited;
end;

function TEcodistrictProject.getMeasuresJSON: string;
begin
  Result := FDReadJSON(
    fDBConnection as TFDConnection,
    'select array_to_json(array_agg(row_to_json(categories)))::text as categories '+
    'from ( '+
    'select category, '+
      '(select array_to_json(array_agg(row_to_json(typologies))) as measures from '+
        '( '+
          'select typology as measure, '+
          '(select array_to_json(array_agg(row_to_json(actions))) as actions from '+
            '(select cat||id as id, application as action, objecttype as objecttypes, benefits as description '+
            'from measures m '+
            'where m.id>=0 and m.category=j.category and m.typology=j.typology '+
            'order by id) as actions) '+
          'from measures j '+
          'group by category, typology '+
          'having category=c.category '+
        ') as typologies '+
      ') as measures '+
      'from measures c '+
      'group by category '+
      'order by category '+
      ') as categories');
    {
    'select array_to_json(array_agg(t))::text '+
    'from ('+
      'select category||'' - ''||typology as cat, '+
        '( select array_to_json(array_agg(row_to_json(jd))) '+
          'from ('+
            'select cat||id as id, application as name, json_build_array(objecttype) as objecttypes, benefits as description '+
            'from measures im '+
            'where j.category||'' - ''||j.typology = im.category||'' - ''||im.typology '+
          ') jd '+
        ') as "measures" '+
      'from measures j group by category, typology '+
		') as t');
    }
end;

procedure TEcodistrictProject.ReadBasicData;
var
//  sl: TLayer;
  schemaNames: TArray<string>;
  schemaName: string;
begin
  {
  // todo: for now just create a single scenario as the current
  fCurrentScenario := TScenario.Create(Self, 'current', 'current', '', TMapView.Create(39.479005, -0.394991, 15));
  fScenarios.Add(fCurrentScenario.ID, fCurrentScenario);
  sl := TLayer.Create(fCurrentScenario, 'basic structures', 'buildings', 'buildings', 'basic buildings', false, nil, '"building"', 'MultiPolygon', fTilerEvent, True);
  sl.query := PGSVGPathsQuery('bldg_building', 'attr_gml_id', 'bldg_lod1multisurface_value');
  //AddCommandToQueue(sl, Self.ReadObjects);
  ReadObjects(sl);
  fCurrentScenario.Layers.Add(sl.ID, sl);
  }
  ReadObjectProperties;
  schemaNames := ReadSchemaNames();
  for schemaName in schemaNames
  do readScenario(schemaName);
end;

procedure TEcodistrictProject.ReadObjectProperties;
var
  query: TFDQuery;
  op: TEcodistrictObjectProperty;
begin
//  if not (fDBConnection as TFDConnection).Ping
//  then Log.WriteLn('TEcodistrictProject.ReadObjects: ping of database returned false', llError);

  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT category, propertyName, propertyType, selection, fieldName, tablename, keyfieldname, editable '+
      'FROM dm_objectproperties';
    query.Open();
    query.First();
    while not query.Eof do
    begin
      //query.Fields[0].
      op := TEcodistrictObjectProperty.Create;
      op.category := query.Fields[0].AsString;
      op.propertyName := query.Fields[1].AsString;
      op.propertyType := query.Fields[2].AsString;
      op.selection := query.Fields[3].AsString;
      op.fieldName := query.Fields[4].AsString;
      op.tableName := query.Fields[5].AsString;
      op.keyFieldName := query.Fields[6].AsString;
      op.editable := query.Fields[7].AsBoolean;
      fObjectProperties.Add(op);
      query.Next();
    end;
  finally
    query.Free;
  end;
end;

procedure TEcodistrictProject.ReadObjects(aSender: TObject);
var
  query: TFDQuery;
begin
  if not (fDBConnection as TFDConnection).Ping
  then Log.WriteLn('TEcodistrictProject.ReadObjects: ping of database returned false', llError);

  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text := (aSender as TLayer).query;
    (aSender as TLayer).ReadObjectsDBSVGPaths(query, NaN);
    (aSender as TLayer).RegisterLayer;
  finally
    query.Free;
  end;
  Log.WriteLn((aSender as TLayer).elementID+': '+(aSender as TLayer).name+', read objects (svg paths)', llNormal, 1);
end;

function TEcodistrictProject.ReadScenario(const aID: string): TScenario;
begin
  TMonitor.Enter(scenarios);
  try
    if not scenarios.TryGetValue(aID, Result) then
    begin
      if aID=EcodistrictBaseScenario
      then Result := TEcodistrictScenario.Create(Self, Self.ProjectID, Self.ProjectName, Self.ProjectDescription, Self.addBasicLayers, Self.mapView)
      else Result := TEcodistrictScenario.Create(Self, aID, '', '', Self.addBasicLayers, Self.mapView);
      scenarios.Add(aID, Result);
    end;
  finally
    TMonitor.Exit(scenarios);
  end;
end;

function TEcodistrictProject.ReadSchemaNames: TArray<string>;
var
  query: TFDQuery;
  schema: string;
  projectSchema: string;
begin
  if not (fDBConnection as TFDConnection).Ping
  then Log.WriteLn('TEcodistrictProject.ReadSchemaNames: ping of database returned false', llError);

  projectSchema := EcoDistrictSchemaId(fProjectID);
  setLength(Result, 0);
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT schema_name '+
			'FROM information_schema.schemata '+
			'WHERE schema_name like '''+projectSchema+'%''';
    query.open();
    query.First;
    while not query.Eof do
    begin
      schema := query.Fields[0].AsString;
      schema := schema.Substring(length(projectSchema));
      if schema.StartsWith('_')
      then schema := schema.Substring(1);
      if schema=''
      then schema := EcodistrictBaseScenario;
      setLength(Result, length(Result)+1);
      Result[length(Result)-1] := schema;
      query.Next;
    end;
  finally
    query.Free;
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

{ TUSLayer }

constructor TUSLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aMetaLayerEntry: TMetaLayerEntry; aDiffRange: Double; aBasicLayer: Boolean);
begin
  fLayerType := aLayerType;
  fMetaLayerEntry := aMetaLayerEntry; // ref
  fPoiCategories := TObjectDictionary<string, TUSPOI>.Create([doOwnsValues]);
  fNewPoiCatID := 0;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, aDiffRange, aBasicLayer);
end;

destructor TUSLayer.Destroy;
begin
  inherited;
  fMetaLayerEntry := nil; // ref
  FreeAndNil(fPoiCategories);
end;

procedure TUSLayer.ReadObjects(aSender: TObject);

  function FieldFloatValueOrNaN(aField: TField): Double;
  begin
    if aField.IsNull
    then Result := NaN
    else Result := aField.AsFloat;
  end;

var
  query: TOraQuery;
  oraSession: TOraSession;
  _connectString: string;
  geometry: TWDGeometry;
  oid: RawByteString;
  value: Double;
  value2: Double;
  texture: Double;
  texture2: Double;
  geometryPoint: TWDGeometryPoint;
  poiType: string;
  poiCat: string;
  usPOI: TUSPOI;
  resourceFolder: string;
//  stream: TBytesStream;
begin
  // register layer with tiler?
  // start query
  // decode objects and add to aLayer
  // send objects to tiler?

  // create new ora session because we are running in a different thread
  oraSession := TOraSession.Create(nil);
  try
    with (scenario.project as TUSProject).OraSession
    do _connectString := userName+'/'+password+'@'+server;
    oraSession.ConnectString := _connectString;
    oraSession.Connect;
    query := TOraQuery.Create(nil);
    try
      query.Session := oraSession;
      query.SQL.Text := fQuery; //.Replace('SELECT ', 'SELECT t1.OBJECT_ID,');
      query.Open;
      query.First;
      while not query.Eof do
      begin
        oid := AnsiString(query.Fields[0].AsInteger.ToString);
        case fLayerType of
          1, 11:
            begin
              value := FieldFloatValueOrNaN(query.FieldByName('VALUE'));
              geometryPoint := CreateWDGeometryPointFromSDOShape(query, 'SHAPE');
              projectGeometryPoint(geometryPoint, (scenario.project as TUSProject).sourceProjection);
              objects.Add(oid, TGeometryPointLayerObject.Create(Self, oid, geometryPoint, value));
            end;
          4: // road (VALUE_EXPR)
            begin
              // unidirectional, not left and right
              value := FieldFloatValueOrNaN(query.Fields[1]);
              geometry := CreateWDGeometryFromSDOShape(query, 'SHAPE');
              projectGeometry(geometry, (scenario.project as TUSProject).sourceProjection);
              objects.Add(oid, TGeometryLayerObject.Create(Self, oid, geometry, value));
            end;
          5,9: // road/energy (VALUE_EXPR) and width (TEXTURE_EXPR) left and right, for energy right will be null -> NaN
            begin
              // Left and right
              value := FieldFloatValueOrNaN(query.Fields[1]);
              value2 := FieldFloatValueOrNaN(query.Fields[2]);
              texture := FieldFloatValueOrNaN(query.Fields[3]);
              texture2 := FieldFloatValueOrNaN(query.Fields[4]);
              geometry := CreateWDGeometryFromSDOShape(query, 'SHAPE');
              projectGeometry(geometry, (scenario.project as TUSProject).sourceProjection);
              objects.Add(oid, TUSRoadICLR.Create(Self, oid, geometry, value, value2, texture, texture2));
            end;
          21: // POI
            begin
              //
              geometryPoint := TWDGeometryPoint.Create;
              try
                geometryPoint.x := query.Fields[1].AsFloat;
                geometryPoint.y := query.Fields[2].AsFloat;
                // no projection, is already in lat/lon
                poiType := query.Fields[3].AsString;
                poiCat := query.Fields[4].AsString;
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
                  {
                  stream  := TBytesStream.Create;
                  try
                    usPOI.picture.Graphic.SaveToStream(stream);
                    fOutputEvent.signalEvent(TByteBuffer.bb_tag_tbytes(icehTilerPOIImage, stream.Bytes)); // todo: check correct number of bytes
                  finally
                    stream.Free;
                  end;
                  }
                end;
              finally
                objects.Add(oid, TGeometryLayerPOIObject.Create(Self, oid, usPOI.ID, geometryPoint));
              end;
            end
        else
          value := FieldFloatValueOrNaN(query.FieldByName('VALUE'));
          geometry := CreateWDGeometryFromSDOShape(query, 'SHAPE');
          projectGeometry(geometry, (scenario.project as TUSProject).sourceProjection);
          objects.Add(oid, TGeometryLayerObject.Create(Self, oid, geometry, value));
        end;
        query.Next;
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
    11, // points, basic layer
    21: // POI
      RegisterOnTiler(False, SliceType, name);
  end;
end;

procedure TUSLayer.RegisterSlice;
var
  //poiImages: TArray<TPngImage>;
//  poi: TUSPOI;
  palette: TWDPalette;
begin
  if basicLayer or not Assigned(fMetaLayerEntry)
  then palette := TDiscretePalette.Create('basic palette', [], TGeoColors.Create(colorBasicOutline))
  else palette := CreatePaletteFromODB(fMetaLayerEntry.LEGEND_DESC, fMetaLayerEntry.odbList, True);;

  // todo:
  case fLayerType of
    1:   tilerLayer.signalAddSlice(palette); // receptors
    //2:; grid
    3,8: tilerLayer.signalAddSlice(palette); // buildings, RS buildings
    4:   tilerLayer.signalAddSlice(palette); // road color (VALUE_EXPR) unidirectional
    5:   tilerLayer.signalAddSlice(palette); // road color (VALUE_EXPR) and width (TEXTURE_EXPR) left and right
    9:   tilerLayer.signalAddSlice(palette); // energy color (VALUE_EXPR) and width (TEXTURE_EXPR)
    11:  tilerLayer.signalAddSlice(palette); // points, basic layer
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
end;

function TUSLayer.SliceType: Integer;
begin
  // todo: implement
  case fLayerType of
    1:   Result := stReceptor; // receptors
    //2:; grid
    3,8: Result := stGeometry; // buildings, RS buildings
    4:   Result := stGeometryI; // road color (VALUE_EXPR) unidirectional
    5:   Result := stGeometryICLR; // road color (VALUE_EXPR) and width (TEXTURE_EXPR) left and right
    9:   Result := stGeometryIC; // energy color (VALUE_EXPR) and width (TEXTURE_EXPR)
    11:  Result := stLocation;  // points, basic layer
    21:  Result := stPOI; // POI
  else
         Result := stUndefined;
  end;
end;

{ TUSScenario }

function TUSScenario.BuildLegendJSON(aMetaLayerEntry: TMetaLayerEntry; aLayer: TUSLayer; aLegendFormat: TLegendFormat): string;

  function FormatLegendDescription(const aDescription: string): string;
  var
    p: integer;
    i: Integer;
    s: string;
    v: Integer;
  begin
    Result := aDescription;
    p := Pos('~~', Result);
    while p<>0 do
    begin
      if (p>1) then
      begin
        Delete(Result, p, 2);
        if Result[p-1]<>'-'
        then Insert(' ', Result, p);
      end
      else
      begin
        Delete(Result, p, 2);
        Insert(' ', Result, p);
      end;
      p := Pos('~~', Result);
    end;
    i := 1;
    while i<Length(Result) do
    begin
      if (Result[i]='\') and (Result[i+1]<>'\') then
      begin
        s := Copy(Result, i+1, 3);
        //v := (Ord(s[1])-Ord('0'))*8*8+(Ord(s[2])-Ord('0'))*8+(Ord(s[3])-Ord('0'));
        v := StrToIntDef(s, 32);
        Delete(Result, i, 4);
        Insert(Chr(v), Result, i);
      end;
      i := i+1;
    end;
  end;

var
  i: Integer;
begin
  Result := '';
  case aLegendFormat of
    lfVertical:
      begin
        for i := 0 to length(aMetaLayerEntry.odbList)-1 do
        begin
          if not aMetaLayerEntry.odbList[i].IsNoData then
          begin
            if Result<>''
            then Result := Result+',';
            Result := Result+'{"'+aMetaLayerEntry.odbList[i].Description+'":{"fillColor":"'+ColorToJSON(aMetaLayerEntry.odbList[i].Color)+'"}}';
          end;
        end;
        Result := '"grid":{"title":"'+FormatLegendDescription(aMetaLayerEntry.LEGEND_DESC)+'","labels":['+Result+']}';;
      end;
    lfHorizontal:
      begin
        for i := 0 to length(aMetaLayerEntry.odbList)-1 do
        begin
          if not aMetaLayerEntry.odbList[i].IsNoData then
          begin
            if Result<>''
            then Result := Result+',';
            Result := Result+'"'+aMetaLayerEntry.odbList[i].Description+'":{"fillColor":"'+ColorToJSON(aMetaLayerEntry.odbList[i].Color)+'"}}';
          end;
        end;
        Result := '"grid2":{"title":"'+FormatLegendDescription(aMetaLayerEntry.LEGEND_DESC)+'","labels":[{'+Result+'}]}';;
      end;
  end;
end;

constructor TUSScenario.Create(aProject: TProject; const aID, aName, aDescription: string; aAddBasicLayers: Boolean; aMapView: TMapView; const aTablePrefix: string);
begin
  fTablePrefix := aTablePrefix;
  fMetaLayer := TMetaLayer.Create([doOwnsValues]);
  inherited Create(aProject, aID, aName, aDescription, aAddbasicLayers, aMapView);
end;

destructor TUSScenario.Destroy;
begin
  inherited;
  FreeAndNil(fMetaLayer);
end;

function TUSScenario.getOraSession: TOraSession;
begin
  Result := (fProject as TUSProject).OraSession;
end;

procedure TUSScenario.ReadBasicData;

  procedure AddBasicLayer(const aID, aName, aDescription, aDefaultDomain, aObjectType, aGeometryType, aQuery: string; aLayerType: Integer; aMetaLayerEntry: TMetaLayerEntry);
  var
    layer: TUSLayer;
  begin
    layer := TUSLayer.Create(Self,
      standardIni.ReadString('domains', aObjectType, aDefaultDomain), //  domain
      aID, aName, aDescription, false,
      //TDiscretePalette.Create('basic palette', [], TGeoColors.Create(colorBasicOutline)),
       '"'+aObjectType+'"', aGeometryType , aLayerType, aMetaLayerEntry, NaN, True);
    layer.query := aQuery;
    Layers.Add(layer.ID, layer);
    Log.WriteLn(elementID+': added layer '+layer.ID+', '+layer.domain+'/'+layer.description, llNormal, 1);
    // schedule reading objects and send to tiler
    AddCommandToQueue(Self, layer.ReadObjects);
  end;

var
  mlp: TPair<Integer, TMetaLayerEntry>;
  layer: TUSLayer;
  layerInfo: string;
  layerInfoParts: TArray<string>;
  //palette: TWDPalette;
  objectTypes: string;
  geometryType: string;
  indicTableNames: TAllRowsSingleFieldResult;
  tableName: string;
  geneTables: TAllRowsSingleFieldResult;
  i: Integer;
  layerInfoKey: string;
  diffRange: Double;
begin
  // process basic layers
  if addBasicLayers then
  begin
    geneTables := ReturnAllFirstFields(OraSession,
      'SELECT DISTINCT OBJECT_NAME '+
      'FROM USER_OBJECTS '+
      'WHERE OBJECT_TYPE = ''TABLE'' AND OBJECT_NAME LIKE '''+fTablePrefix+'GENE%''');
    for i := 0 to Length(geneTables)-1 do
    begin
      tableName := geneTables[i];
      if tableName=fTablePrefix.ToUpper+'GENE_ROAD' then
      begin // always
        AddBasicLayer(
          'road', 'roads', 'roads', 'basic structures', 'road', 'LineString',
          'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_PIPELINES' then
      begin // check count
        if ReturnRecordCount(OraSession, tableName)>0
        then AddBasicLayer(
               'pipeline', 'pipe lines', 'pipe lines', 'basic structures', 'pipe line', 'LineString',
               'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_BUILDING' then
      begin // always
        AddBasicLayer(
          'building', 'buildings', 'buildings', 'basic structures', 'building', 'MultiPolygon',
          'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 3, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_SCREEN' then
      begin // always
        AddBasicLayer(
          'screen', 'screens', 'screens', 'basic structures', 'screen', 'LineString',
          'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_TRAM' then
      begin // check count
        if ReturnRecordCount(OraSession, tableName)>0
        then AddBasicLayer(
               'tramline', 'tram lines', 'tram lines', 'basic structures', 'tram line', 'LineString',
               'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_BIKEPATH' then
      begin // check count
        if ReturnRecordCount(OraSession, tableName)>0
        then AddBasicLayer(
               'bikepath', 'bike paths', 'bike paths', 'basic structures', 'bike path', 'LineString',
               'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_RAIL' then
      begin // check count
        if ReturnRecordCount(OraSession, tableName)>0
        then AddBasicLayer(
               'railline', 'rail lines', 'rail lines', 'basic structures', 'rail line', 'LineString',
               'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4, nil);
      end
      else if tableName=fTablePrefix.ToUpper+'GENE_INDUSTRY_SRC' then
      begin // check count
        if ReturnRecordCount(OraSession, tableName)>0
        then AddBasicLayer(
               'industrysource', 'industry sources', 'industry sources', 'basic structures', 'industry source', 'Point',
               'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 11, nil);
      end;
      //GENE_NEIGHBORHOOD
      //GENE_RESIDENCE
      //GENE_NODE
      //GENE_DISTRICT
      //GENE_STUDY_AREA
      //GENE_BASCOV
    end;
  end;
  // process meta layer to build list of available layers
  ReadMetaLayer(OraSession, fTableprefix, fMetaLayer);
  Log.WriteLn(elementID+': found '+fMetaLayer.Count.ToString+' layers in meta_layer');
  for mlp in fMetaLayer do
  begin
    // try tablename-value, legend description-value..
    layerInfoKey := mlp.Value.LAYER_TABLE.Trim+'-'+mlp.Value.VALUE_EXPR.trim;
    layerInfo := StandardIni.ReadString('layers', layerInfoKey, '');
    if layerInfo='' then
    begin
      layerInfoKey := mlp.Value.LEGEND_DESC.trim+'-'+mlp.Value.VALUE_EXPR.trim;
      layerInfo := StandardIni.ReadString('layers', layerInfoKey, '');
    end;

    if layerInfo<>'' then
    begin
      layerInfoParts := layerInfo.Split([',']);
      if length(layerInfoParts)<2 then
      begin
        setLength(layerInfoParts, 2);
        layerInfoParts[1] := mlp.Value.LEGEND_DESC;
      end;

      //palette := CreatePaletteFromODB(mlp.Value.LEGEND_DESC, mlp.Value.odbList, True);

      case mlp.Value.LAYER_TYPE mod 100 of // i+100 image layer version same as i but ignored by US3D
        1:
          begin
            objectTypes := '"receptor"';
            geometryType := 'Point';
            diffRange := mlp.Value.diffRange(10);
          end;
        2:
          begin
            objectTypes := '"grid"';
            geometryType := 'MultiPolygon'; // todo: ?
            diffRange := mlp.Value.diffRange(10);
          end;
        3, 8:
          begin
            objectTypes := '"building"'; // 3 buildings, 8 RS buildings
            geometryType := 'MultiPolygon';
            diffRange := mlp.Value.diffRange(10);
          end;
        4,    // road color (VALUE_EXPR)
        5:    // road color (VALUE_EXPR) and width (TEXTURE_EXPR)
          begin
            objectTypes := '"road"';
            geometryType := 'LineString';
            diffRange := mlp.Value.diffRange(10);
          end;
        9:    // enrg color (VALUE_EXPR) and width (TEXTURE_EXPR)
          begin
            objectTypes := '"energy"';
            geometryType := 'LineString';
            diffRange := mlp.Value.diffRange(10);
          end;
        11:
          begin
            objectTypes := '"location"';
            geometryType := 'Point';
            diffRange := NaN; // todo:
          end;
        21:
          begin
            objectTypes := '"poi"';
            geometryType := 'Point';
            diffRange := NaN; // todo:
          end;
      else
        // 31 vector layer ?
        objectTypes := '';
        geometryType := '';
        diffRange := NaN;
      end;
      if geometryType<>'' then
      begin
        layer := TUSLayer.Create(Self,
          standardIni.ReadString('domains', layerInfoParts[0], layerInfoParts[0]), //  domain
          mlp.Key.ToString, // id
          layerInfoParts[1], // name
          mlp.Value.LEGEND_DESC.Replace('~~', '-').replace('\', '-'), // description
          false, //false, // todo: default load
          //palette,
          objectTypes, geometryType,
          mlp.Value.LAYER_TYPE mod 100,
          mlp.Value,
          diffRange);
        layer.fLegendJSON := BuildLegendJSON(mlp.Value, layer, lfVertical);
        layer.query := mlp.Value.SQLQuery(fTableprefix);
        Layers.Add(layer.ID, layer);
        Log.WriteLn(elementID+': added layer '+layer.ID+', '+layer.domain+'/'+layer.description, llNormal, 1);
        // schedule reading objects and send to tiler
        AddCommandToQueue(Self, layer.ReadObjects);
      end
      else Log.WriteLn(elementID+': skipped layer ('+mlp.Key.ToString+') type '+mlp.Value.LAYER_TYPE.ToString+' (unsupported) '+mlp.Value.LAYER_TABLE+'-'+mlp.Value.VALUE_EXPR, llRemark, 1);
    end
    else Log.WriteLn(elementID+': skipped layer ('+mlp.Key.ToString+') type '+mlp.Value.LAYER_TYPE.ToString+' (based on ini) ('+mlp.Value.LAYER_TABLE+'-'+mlp.Value.VALUE_EXPR, llRemark, 1);
  end;
  // process indicators
  indicTableNames := ReturnAllFirstFields(OraSession,
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
  for tableName in indicTableNames do
  begin
    //fIndicators.Add(TIndicator.Create(Self, FModelControl.Connection, FDomainsEvent, FSession, FTablePrefix, FSessionName, tableName));
    Log.WriteLn(elementID+': added indicator: '+tableName, llNormal, 1);
  end;
  Log.WriteLn(elementID+': finished building scenario');
  //(fProject as TUSProject).StatusDec;
end;

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string;
var
  layers: TList<TLayer>;
  categories: string;
  objectsGeoJSON: string;
  totalObjectCount: Integer;
  extent: TWDExtent;
  l: TLayer;
  objectCount: Integer;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
    begin
      categories := '';
      objectsGeoJSON := '';
      totalObjectCount := 0;
      extent := TWDExtent.FromGeometry(aGeometry);
      for l in layers do
      begin
        objectCount := l.findObjectsInGeometry(extent, aGeometry, objectsGeoJSON);
        if objectCount>0 then
        begin
          if categories=''
          then categories := '"'+l.id+'"'
          else categories := categories+',"'+l.id+'"';
          totalObjectCount := totalObjectCount+objectCount;
        end;
      end;
      Result :=
        '{"selectedObjects":{"selectCategories":['+categories+'],'+
         '"mode":"'+aMode+'",'+
         '"objects":['+objectsGeoJSON+']}}';
      Log.WriteLn('select on geometry:  found '+totalObjectCount.ToString+' objects in '+categories);
    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string;
var
  layers: TList<TLayer>;
  dist: TDistanceLatLon;
  nearestObject: TLayerObject;
  nearestObjectLayer: TLayer;
  nearestObjectDistanceInMeters: Double;
  l: TLayer;
  o: TLayerObject;
  categories: string;
  objectsGeoJSON: string;
  totalObjectCount: Integer;
  objectCount: Integer;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
    begin
      dist := TDistanceLatLon.Create(aY, aY);
      if (aRadius=0) then
      begin
        nearestObject := nil;
        nearestObjectLayer := nil;
        nearestObjectDistanceInMeters := Infinity;
        for l in layers do
        begin
          o := l.findNearestObject(dist, aX, aY, nearestObjectDistanceInMeters);
          if assigned(o) then
          begin
            nearestObjectLayer := l;
            nearestObject := o;
            if nearestObjectDistanceInMeters=0
            then break;
          end;
        end;
        if Assigned(nearestObject) then
        begin
          // todo:
          Result :=
            '{"selectedObjects":{"selectCategories":["'+nearestObjectLayer.ID+'"],'+
             '"mode":"'+aMode+'",'+
             '"objects":['+nearestObject.GeoJSON2D[nearestObjectLayer.geometryType]+']}}';
          Log.WriteLn('found nearest object layer: '+nearestObjectLayer.ID+', object: '+string(nearestObject.ID)+', distance: '+nearestObjectDistanceInMeters.toString);
        end;
      end
      else
      begin
        categories := '';
        objectsGeoJSON := '';
        totalObjectCount := 0;
        for l in layers do
        begin
          objectCount := l.findObjectsInCircle(dist, aX, aY, aRadius, objectsGeoJSON);
          if objectCount>0 then
          begin
            if categories=''
            then categories := '"'+l.id+'"'
            else categories := categories+',"'+l.id+'"';
            totalObjectCount := totalObjectCount+objectCount;
          end;
        end;
        Result :=
          '{"selectedObjects":{"selectCategories":['+categories+'],'+
           '"mode":"'+aMode+'",'+
           '"objects":['+objectsGeoJSON+']}}';
        Log.WriteLn('select on radius:  found '+totalObjectCount.ToString+' objects in '+categories);
      end;
    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string;
var
  layers: TList<TLayer>;
begin
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
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

constructor TUSDBScenario.Create(aID: Integer; const aName, aDescription: string; aParentID, aReferenceID: Integer; const aTablePrefix, aIMBPrefix, aStatus: string; aPublished: Integer);
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

procedure TUSDBScenario.Relink(aUSDBScenarios: TObjectDictionary<Integer, TUSDBScenario>);
var
  s: TUSDBScenario;
begin
  if aUSDBScenarios.TryGetValue(fParentID, s)
  then fParentName := s.name;
  if aUSDBScenarios.TryGetValue(fReferenceID, s)
  then fReferenceName := s.name;
end;

{ TUSProject }

constructor TUSProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean);
var
  SourceEPSGstr: string;
  SourceEPSG: Integer;
begin
  fUSDBScenarios := TObjectDictionary<Integer, TUSDBScenario>.Create;
  fMapView := aMapView;

  SourceEPSGstr := GetSetting(SourceEPSGSwitch, 'Amersfoort_RD_New');
  if SourceEPSGstr<>'' then
  begin
    SourceEPSG := StrToIntDef(SourceEPSGstr, -1);
    if SourceEPSG>0
    then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(SourceEPSG)
    else fSourceProjection := CSProjectedCoordinateSystemList.ByWKT(SourceEPSGstr);
  end
  else fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  fPreLoadScenarios := aPreLoadScenarios;
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDBConnection, 0, False, False, False, addBasicLayers); //1, True, True, True);
end;

destructor TUSProject.Destroy;
begin
  FreeAndNil(fScenarioLinks);
  inherited;
  FreeAndNil(fUSDBScenarios);
end;

function TUSProject.getOraSession: TOraSession;
begin
  Result := fDBConnection as TOraSession;
end;

procedure TUSProject.ReadBasicData;
var
  scenarioID: Integer;
begin
  ReadScenarios;
  ReadMeasures;
  // load current scenario and ref scenario first
  scenarioID := getUSCurrentPublishedScenarioID(OraSession, GetCurrentScenarioID(OraSession));
  fCurrentScenario := ReadScenario(scenarioID.ToString());
  Log.WriteLn('current US scenario: '+fCurrentScenario.ID+' ('+(fCurrentScenario as TUSScenario).fTableprefix+'): "'+fCurrentScenario.description+'"', llOk);
  // ref
  scenarioID := GetScenarioBaseID(OraSession, scenarioID);
  if scenarioID>=0 then
  begin
    fRefScenario := ReadScenario(scenarioID.ToString());
    Log.WriteLn('reference US scenario: '+fRefScenario.ID+' ('+(fRefScenario as TUSScenario).fTableprefix+'): "'+fRefScenario.description+'"', llOk);
  end
  else Log.WriteLn('NO reference US scenario', llWarning);
  if fPreLoadScenarios then
  begin
    for scenarioID in fUSDBScenarios.Keys do
    begin
      if fUSDBScenarios[scenarioID]._published>0
      then ReadScenario(scenarioID.ToString());
    end;
  end;
end;

procedure TUSProject.ReadMeasures;
var
  measures: TAllRowsResults;
  m: Integer;
begin
  // todo:
  measures := ReturnAllResults(OraSession,
    'SELECT OBJECT_ID, Category, Measure, Description, ObjectTypes, Action, Action_Parameters, Action_ID '+
    'FROM META_MEASURES');
  for m := 0 to length(measures)-1 do
  begin
    //

  end;
  // todo:
  Self.measuresEnabled := length(measures)>0;
  Self.measuresHistoryEnabled := Self.measuresEnabled;
end;

function TUSProject.ReadScenario(const aID: string): TScenario;
var
  dbScenario: TUSDBScenario;
begin
  System.TMonitor.Enter(fScenarios);
  try
    if not fScenarios.TryGetValue(aID, Result) then
    begin
      if fUSDBScenarios.TryGetValue(aID.ToInteger(), dbScenario) then
      begin
        //StatusInc;
        Result := TUSScenario.Create(Self, aID, dbScenario.name, dbScenario.description, addBasicLayers, fMapView, dbScenario.tablePrefix);
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
  usdbScenario: TUSDBScenario;
  isp: TPair<Integer, TUSDBScenario>;
begin
  // read scenarios from project database
  try
    scenarios := ReturnAllResults(OraSession,
      'SELECT ID, Name, Federate, Parent_ID, Base_ID, Notes, SCENARIO_STATUS, PUBLISHED '+
      'FROM META_SCENARIOS '+
      'ORDER BY ID ASC');
    for s := 0 to Length(scenarios)-1 do
    begin
      usdbScenario := TUSDBScenario.Create(scenarios[s][0].ToInteger, scenarios[s][1], scenarios[s][5],
        StrToIntDef(scenarios[s][3], -1), StrToIntDef(scenarios[s][4], -1), scenarios[s][1]+'#', scenarios[s][2], scenarios[s][6], StrToIntDef(scenarios[s][7], 0));
      fUSDBScenarios.Add(usdbScenario.id, usdbScenario);
      if scenarios[s][7]='1' then
      begin
        fScenarioLinks.children.Add(TScenarioLink.Create(
          usdbScenario.ID, usdbScenario.parentID, usdbScenario.referenceID,
          usdbScenario.name, usdbScenario.description, scenarios[s][6], nil));
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
      usdbScenario := TUSDBScenario.Create(scenarios[s][0].ToInteger, scenarios[s][1], scenarios[s][5],
        StrToIntDef(scenarios[s][3], -1), StrToIntDef(scenarios[s][4], -1), scenarios[s][1]+'#', scenarios[s][2], scenarios[s][6], 1);
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
  fScenarioLinks.removeLeave('CLOSED');
end;

function getUSMapView(aOraSession: TOraSession; const aDefault: TMapView): TMapView;
var
  table: TOraTable;
begin
  // try to read view from database
  table := TOraTable.Create(nil);
  try
    table.Session := aOraSession;
    table.SQL.Text := 'SELECT Lat, Lon, Zoom FROM PUBL_PROJECT';
    table.Execute;
    try
      if table.FindFirst
      then Result := TMapView.Create(table.Fields[0].AsFloat, table.Fields[1].AsFloat, table.Fields[2].AsInteger)
      else Result := aDefault;
    finally
      table.Close;
    end;
  finally
    table.Free;
  end;
end;

function getUSProjectID(aOraSession: TOraSession; const aDefault: string): string;
var
  table: TOraTable;
begin
  // try to read project info from database
  table := TOraTable.Create(nil);
  try
    table.Session := aOraSession;
    table.SQL.Text := 'SELECT ProjectID FROM PUBL_PROJECT';
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
  finally
    table.Free;
  end;
end;

procedure setUSProjectID(aOraSession: TOraSession; const aValue: string);
begin
  aOraSession.ExecSQL('UPDATE PUBL_PROJECT SET ProjectID='''+aValue+'''');
  aOraSession.Commit;
end;

function getUSCurrentPublishedScenarioID(aOraSession: TOraSession; aDefault: Integer): Integer;
var
  table: TOraTable;
begin
  // try to read project info from database
  table := TOraTable.Create(nil);
  try
    table.Session := aOraSession;
    table.SQL.Text := 'SELECT StartPublishedScenarioID FROM PUBL_PROJECT';
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

{ TNWBLiveFeedRoad }

function TNWBLiveFeedRoad.Change(aNewRoad: TNWBLiveFeedRoad): Boolean;
begin
  Result := False;
  if not SameValue(fVp, aNewRoad.fVp) then
  begin
    fVp := aNewRoad.fVp;
    Result := True;
  end;
  if not SameValue(fi, aNewRoad.fi) then
  begin
    fi := aNewRoad.fi;
    Result := True;
  end;
  if not SameValue(fd, aNewRoad.fd) then
  begin
    fd := aNewRoad.fd;
    Result := True;
  end;
  ftime := aNewRoad.ftime; // todo: changes always?
end;

procedure TNWBLiveFeedRoad.ConvertFromShape(aShape: TGIS_Shape);
var
  partI: Integer;
  pointI: Integer;
  p: TGIS_Point3D;
  partG: TWDGeometryPart;
  projection: TGIS_CSProjectedCoordinateSystem;
begin
  geometry.parts.Clear;
  if Assigned(aShape) then
  begin
    projection := (layer.scenario.project as TNWBLiveFeedProject).sourceProjection;
    for partI := 0 to aShape.GetNumParts-1 do
    begin
      partG := geometry.AddPart;
      for pointI := 0 to aShape.GetPartSize(partI)-1 do
      begin
        p := aShape.GetPoint3D(partI, pointI);
        p := projection.ToGeocs3D(p);
        partG.AddPoint(p.X, p.Y, p.Z);
      end;
    end;
  end;
end;

constructor TNWBLiveFeedRoad.CreateFromFeed(aLayer: TLayer; var aByteBuffer: ByteBuffers.TByteBuffer);
var
  id: TWDID;
begin
  ftime := Trunc(aByteBuffer.ReadDouble());
  id := TWDID(Trunc(aByteBuffer.ReadDouble()).ToString);
  fVp := aByteBuffer.ReadDouble();
  fi := aByteBuffer.ReadDouble();
  fd := aByteBuffer.ReadDouble();
  fCapacity := NaN;
  inherited Create(aLayer, ID, TWDGeometry.Create, fVp); // use Vp as value for coloring
end;


{ TObjectList }

procedure TNWBObjectList.add(aObject: TLayerObject);
begin
  fLock.BeginWrite;
  try
    fObjects.Add(aObject);
  finally
    fLock.EndWrite;
  end;
end;

procedure TNWBObjectList.clear;
begin
  fLock.BeginWrite;
  try
    fObjects.Clear;
  finally
    fLock.EndWrite;
  end;
end;

constructor TNWBObjectList.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  fLock.Create;
  fObjects := TObjectList<TLayerObject>.Create(aOwnsObjects); // refs
end;

destructor TNWBObjectList.Destroy;
begin
  FreeAndNil(fObjects);
  inherited;
end;

procedure TNWBObjectList.remove(aObject: TLayerObject);
begin
  fLock.BeginWrite;
  try
    fObjects.Remove(aObject);
  finally
    fLock.EndWrite;
  end;
end;


{ TNWBLiveFeedLayer }

constructor TNWBLiveFeedLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription : string; aDefaultLoad: Boolean;
  aPalette: TWDPalette; {aTilerEvent: TEventEntry; }aLiveFeedConnection: TIMBConnection; const aShapeFileName: string);
var
  shape: TGIS_Shape;
  objJSON: string;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, {aPalette, }'"road"', 'LineString', aPalette.maxValue/2);
  fLiveFeedConnection := aLiveFeedConnection; // ref
  fObjectsAdded := TNWBObjectList.Create(False); // refs
  fObjectsUpdated := TNWBObjectList.Create(False); // refs
  fObjectsDeleted := TNWBObjectList.Create(True); // owns!
  fUpdateTimeOut := aScenario.project.Timers.SetTimer(HandleUpdateTimeOut);
  fWVKID2UID := TDictionary<TWDID, Integer>.Create;
  fNWBShape := TGIS_LayerSHP.Create;
  // load shape file
  fNWBShape.Name := 'NWBWegvakken';
  fNWBShape.Path := aShapeFileName;
  fNWBShape.Open; // todo: call fNWBShape.Open; here so init is slow but first lookup is fast?
  // fill dictionary for lookups of wvk_id to uid in shape layer
  shape := fNWBShape.FindFirst(GisWholeWorld);
  objJSON := '';
  while Assigned(Shape) do
  begin
    fWVKID2UID.AddOrSetValue(TWDID(shape.GetField('WVK_ID')), shape.Uid);
    shape := fNWBShape.FindNext;
  end;
  Log.WriteLn('NWB: read shape file '+fNWBShape.Path);
  // go live
  fLiveFeedConnection.Subscribe('NWB', HandleNormalEvent);
  // start timer to cleanup old layer objects
  fCleanupTimer := scenario.project.Timers.SetTimerDelta(
    HandleCleanup,
    MilliSeconds2HRT(NWBCleanupCycle_ms),
    MilliSeconds2HRT(NWBCleanupCycle_ms));
end;

destructor TNWBLiveFeedLayer.Destroy;
//var
//  iop: TPair<Integer, TLayerObject>;
begin
  //fUpdateTimeOut.Cancel;
  fUpdateTimeOut := nil;
  //fCleanupTimer.Cancel;
  fCleanupTimer := nil;

  // todo: test, remove all objects
//  for iop in objects
//  do fObjectsDeleted.add(iop.Value);
//  HandleUpdateTimeOut(nil);

  inherited;
  //FreeAndNil(fLiveFeedConnection);
  FreeAndNil(fObjectsAdded);
  FreeAndNil(fObjectsUpdated);
  FreeAndNil(fObjectsDeleted);
  FreeAndNil(fNWBShape);
  FreeAndNil(fWVKID2UID);
  fLiveFeedConnection := nil; // ref
end;

function TNWBLiveFeedLayer.getJSON: string;
begin
  Result := inherited+','+
    '"timestamp":"'+fLastTimeStamp+'"';
end;

{
function TNWBLiveFeedLayer.getObjectsJSON: string;
var
  iop: TPair<TWDID, TLayerObject>;
begin
  Result := '';
  for iop in objects do
  begin
    if iop.Value.ValidGeometry
    then jsonAdd(Result, iop.Value.GeoJSON2D);
  end;
  Result := geoJsonFeatureCollection(Result);
end;
}
procedure TNWBLiveFeedLayer.HandleCleanup(aTimer: TTimer);
begin
  // cleanup old layer objects

  // todo: implement

end;

procedure TNWBLiveFeedLayer.HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  newRoad: TLayerObject;
  road: TLayerObject;
  shape: TGIS_Shape;
  uid: Integer;
begin
  // handle feed entry
  newRoad := TNWBLiveFeedRoad.CreateFromFeed(Self, aPayload);
  try
    fLastTimeStamp := FormatDateTime('yyyy-mm-dd hh:nn', ((newRoad as TNWBLiveFeedRoad).fTime / 86400) + 25569)+' UTC';
    if FindObject(newRoad.id, road) then
    begin
      if (road as TNWBLiveFeedRoad).Change(newRoad as TNWBLiveFeedRoad) then
      begin
        Log.Progress('NWB: changed '+string(road.id));
        fObjectsUpdated.add(road); // ref
      end;
    end
    else
    begin
      // lookup geometry and capacitiy and add to road information
      if fWVKID2UID.TryGetValue(newRoad.id, uid) then
      begin
        shape := fNWBShape.GetShape(uid);
        if Assigned(shape) then
        begin
          (newRoad as TNWBLiveFeedRoad).ConvertFromShape(shape);
          // todo: newRoad.fCapacity := ..
          Log.Progress('NBW: new '+string(newRoad.id));
          AddObject(newRoad); // own
          fObjectsAdded.add(newRoad); // ref
          newRoad := nil;
        end
        else Log.WriteLn('NWB: did not find shape for '+string(newRoad.id)+' on uid '+uid.ToString, llError);
      end
      else
      begin
        Log.WriteLn('NWB: did not find wvk_id '+string(newRoad.id), llWarning);
        // add empty record to avoid lookup problems later on
        // todo: newRoad.fCapacity := ..
        // no signal
        AddObject(newRoad); // own
        newRoad := nil;
      end;
    end;
  finally
    newRoad.Free;
  end;
  fUpdateTimeOut.DueTimeDelta := MilliSeconds2HRT(NWBUpdateTimeOut_ms);
end;

procedure TNWBLiveFeedLayer.HandleUpdateTimeOut(aTimer: TTimer);
var
  obj: TLayerObject;
  json: string;
  client: TClient;
begin
  // added objects, signal complete structure
  json := '';
  fObjectsAdded.fLock.BeginWrite;
  try
    for obj in fObjectsAdded.objects
    do jsonAdd(json, '"'+string(obj.id)+'"'+':'+obj.GeoJSON2D[geometryType]);
    fObjectsAdded.objects.Clear;
  finally
    fObjectsAdded.fLock.EndWrite;
  end;
  if json<>'' then
  begin
    json :=
      '{"updatelayer":'+
        '{"id":"'+elementID+'",'+
         '"name":"'+name+'",'+
         '"timestamp":"'+fLastTimeStamp+'",'+
         '"newobjects":'+'{'+json+'}'+
        '}'+
      '}';
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(json);
    finally
      TMonitor.Exit(clients);
    end;
  end;
  // updated objects, signal changed colors
  json := '';
  fObjectsUpdated.lock.BeginWrite;
  try
    for obj in fObjectsUpdated.objects
    do jsonAdd(json, '"'+string(obj.id)+'"'+':"'+ColorToJSON(fTilerLayer.palette.ValueToColors((obj as TNWBLiveFeedRoad).fVp).fillColor)+'"');
    fObjectsUpdated.objects.Clear;
  finally
    fObjectsUpdated.lock.EndWrite;
  end;
  if json<>'' then
  begin
    json :=
      '{"updatelayer":'+
        '{"id":"'+elementID+'",'+
         '"name":"'+name+'",'+
         '"timestamp":"'+fLastTimeStamp+'",'+
         '"changedcolors":'+'{'+json+'}'+
        '}'+
      '}';
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(json);
    finally
      TMonitor.Exit(clients);
    end;
  end;
  // removed objects
  json := '';
  fObjectsDeleted.lock.BeginWrite;
  try
    for obj in fObjectsDeleted.objects
    do jsonAdd(json, '"'+string(obj.id)+'":"X"');
    fObjectsDeleted.objects.Clear; // owns objects!
  finally
    fObjectsDeleted.lock.EndWrite;
  end;
  if json<>'' then
  begin
    json :=
      '{"updatelayer":'+
        '{"id":"'+elementID+'",'+
         '"name":"'+name+'",'+
         '"timestamp":"'+fLastTimeStamp+'",'+
         '"removedobjects":'+'{'+json+'}'+
        '}'+
      '}';
    TMonitor.Enter(clients);
    try
      for client in clients
      do client.signalString(json);
    finally
      TMonitor.Exit(clients);
    end;
  end;

  // todo: update kpi1 as test
  with fScenario as TNWBLiveFeedScenario do
  begin
    if kpi1.measures[0]<>200
    then kpi1.measures := [200, 290]
    else kpi1.measures := [230, 250];
    kpi1.Update;
  end;

end;

function TNWBLiveFeedLayer.SliceType: Integer;
begin
  Result := stUndefined;
end;

{ TOldTilerLayer }

constructor TOldTilerLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; {aTilerEvent: TEventEntry; }aRefreshEvent: TIMBEventEntry; aBasicLayer: Boolean);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, {aPalette, }aObjectTypes, aGeometryType, NaN, aBasicLayer); // todo:
  fRefreshEvent := aRefreshEvent;
  fRefreshEvent.OnNormalEvent := HandleRefreshEvent;
end;

destructor TOldTilerLayer.Destroy;
begin
  if Assigned(fRefreshEvent) then
  begin
    fRefreshEvent.UnSubscribe;
    fRefreshEvent := nil;
  end;
  inherited;
end;

procedure TOldTilerLayer.HandleRefreshEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer);
var
  client: TClient;
begin
  // payload contains 1 boolean but is always true, no need to decode
  TMonitor.Enter(clients);
  try
    for client in clients
    do client.SendRefresh(elementID, '', uniqueObjectsTilesLink); // todo: add time stamp?
  finally
    TMonitor.Exit(clients);
  end;
end;

function TOldTilerLayer.SliceType: Integer;
begin
  Result := stUndefined;
end;

{ TNWBLiveFeedScenario }

procedure addLiveAirLayer(aScenario: TScenario; const aDomain: string; aLayerID: Integer; const aName, aDescription: string;
  {aTilerEvent: TEventEntry; }const aAVLFileName: string; aLiveFeedConnection: TIMBConnection);
var
  palette: TWDPalette;
  layer: TLayer;
begin
  if FileExists(aAVLFileName) then
  begin
    palette := CreatePaletteFromODB(aName, ODBFileToODBList(aAVLFileName), True);
    layer := aScenario.AddLayer(
      TOldTilerLayer.Create(aScenario, aDomain, aLayerID.toString, aName, aDescription, False,
      palette, '"road"', 'tile', {aTilerEvent, }aLiveFeedConnection.Subscribe('layers.'+aLayerID.ToString)));
    // todo:
    (*
    layer.objectsTilesID := aLayerID;
    layer.objectsTilesLink := GetSetting(NWBLiveFeedTilesURLSwitch, DefaultNWBLiveFeedTilesURL)+
      '?layer='+aLayerID.ToString+'&zoom={z}&x={x}&y={y}';
    *)
  end
  else Log.WriteLn('Could not create live air layer: avl '+aAVLFileName+' not found', llError);
end;

constructor TNWBLiveFeedScenario.Create(aProject: TProject; const aScenarioID: string;
  aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string; aAddBasicLayers: Boolean);
var
  resourceFolder: string;
begin
  // todo: rotterdam
  inherited Create(aProject, aScenarioID, aScenarioID, '', aAddBasicLayers, TMapView.Create(51.946333, 4.311171, 11));
  fLiveFeedConnection := aLiveFeedConnection;
  AddLayer(
    TNWBLiveFeedLayer.Create(Self, NWBTrafficDomain, 'live', 'Live traffic i/c', 'Live traffic feed', True, aPalette,
    {fProject.TilerEvent, }fLiveFeedConnection, aShapeFileName));
  resourceFolder := ExtractFilePath(ParamStr(0));
  //addLiveAirLayer(Self, AirDomain, 121, 'NOx',  '', fProject.TilerEvent, 'no2.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 122, 'PM10', '', {fProject.TilerEvent, }resourceFolder+'pm10.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 123, 'PM25', '', {fProject.TilerEvent, }resourceFolder+'pm25.avl', fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 124, 'NO2',  '', fProject.TilerEvent, 'no2.avl',  fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 132, 'PM10 tot', '', fProject.TilerEvent, 'pm10.avl', fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 133, 'PM25 tot', '', fProject.TilerEvent, 'pm25.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 134, 'NO2',  '', {fProject.TilerEvent, }resourceFolder+'no2.avl',  fLiveFeedConnection);
  kpi1 := TKPI.Create(Self, NWBTrafficDomain, 'kpi1', 'kpi1', 'a traffic kpi', false);
  with AddKPI(kpi1) do
  begin
    title := 'kpi1';
    subtitle := '-';
    ranges := [150, 225, 300];
    measures := [220, 270];
    markers := [250];
  end;
  with AddKPI(TKPI.Create(Self, NWBTrafficDomain, 'kpi2', 'kpi2', 'a traffic kpi', false)) do
  begin
    title := 'kpi2';
    subtitle := '-';
    ranges := [150, 225, 300];
    measures := [200, 280];
    markers := [260];
  end;
  with AddChart(TChart.Create(Self, NWBTrafficDomain, 'chart1', 'chart1', 'a traffic chart', false, 'verticalBars')) do
  begin
    groupNames := ['set een', 'set twee'];
    groupValues := [
      TChartGroupRow.Create('een', [1,2]),
      TChartGroupRow.Create('twee', [2,3]),
      TChartGroupRow.Create('drie', [3,4]),
      TChartGroupRow.Create('vier', [7,4]),
      TChartGroupRow.Create('vijf', [5,2])
    ];
  end;
  with AddChart(TChart.Create(Self, NWBTrafficDomain, 'chart2', 'chart2', 'a traffic chart 2', false, 'verticalBars')) do
  begin
    groupNames := ['uno'];
    groupValues := [
      TChartGroupRow.Create('1', [1]),
      TChartGroupRow.Create('2', [2]),
      TChartGroupRow.Create('3', [3]),
      TChartGroupRow.Create('4', [3]),
      TChartGroupRow.Create('5', [7]),
      TChartGroupRow.Create('6', [3])
    ];
  end;
  with AddChart(TChart.Create(Self, NWBTrafficDomain, 'chart3', 'chart3', 'a traffic chart 3', false, 'verticalBars')) do
  begin
    groupNames := ['uno', 'due', 'tre', 'quattro'];
    groupValues := [
      TChartGroupRow.Create('een', [1,2,3,4]),
      TChartGroupRow.Create('twee', [2,3,4,5]),
      TChartGroupRow.Create('drie', [3,4,5,2])
    ];
  end;
end;

destructor TNWBLiveFeedScenario.Destroy;
begin
  fLiveFeedConnection.Close; // to unlock readers
  inherited;
  FreeAndNil(fLiveFeedConnection);
end;

{ TNWBLiveFeedProject }

constructor TNWBLiveFeedProject.Create(aSessionModel: TSessionModel; aConnection: TConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string);
begin
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, nil, 0, False, False, False, False);
  {if aSourceEPSG>0
  then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(aSourceEPSG)
  else }
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  fCurrentScenario := TNWBLiveFeedScenario.Create(Self, 'Live', aLiveFeedConnection, aPalette, aShapeFileName, addBasicLayers);

  TMonitor.Enter(fScenarios);
  try
    fScenarios.Add(fCurrentScenario.ID, fCurrentScenario);
  finally
    TMonitor.Exit(fScenarios);
  end;
end;

procedure TNWBLiveFeedProject.ReadBasicData;
begin
  //fMeasuresJSON := '[]';
  // all reading is done in live layer
end;

{ utils }

function CreateSessionProject(aSessionModel: TSessionModel; const aProjectID, aProjectName: string; aProjectType: TProjectType; const aTilerFQDN, aTilerStatusURL, aConnectString: string): TProject;
var
  dbConnection: TCustomConnection;
  palette: TWDPalette;
begin
  case aProjectType of
    ptUrbanStrategyOracle:
      begin
        dbConnection := TOraSession.Create(nil);
        (dbConnection as TOraSession).ConnectString := aConnectString;
        dbConnection.Open;
        Result := TUSProject.Create(aSessionModel, aSessionModel.Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL,
          dbConnection, TMapView.Create(51.919775, 4.403763 {51.914852, 4.394151}, 13), false);
        aSessionModel.Projects.Add(Result);
      end;
    ptEcoDistrict:
      begin
        InitPG;
        dbConnection := TFDConnection.Create(nil);
        SetPGConnection(dbConnection as TFDConnection, aConnectString);
        Result := TEcodistrictProject.Create(aSessionModel, aSessionModel.Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, dbConnection, 0, True, True, True, True);
        aSessionModel.Projects.Add(Result);
      end;
    ptNWBLiveFeed:
      begin
        palette := TRampPalette.Create('Traffic speed', [
            TRampPaletteEntry.Create(TAlphaColors.Red, 0, ''),
            TRampPaletteEntry.Create(TAlphaColors.Yellow, 60, ''),
            TRampPaletteEntry.Create(TAlphaColors.Green, 100, '')
            ],
          TAlphaColors.Black,
          TAlphaColors.Black,
          TAlphaColors.Green);
        Result := TNWBLiveFeedProject.Create(aSessionModel, aSessionModel.Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL,
          TIMBConnection.Create(
            getSetting(NWBLiveFeedRemoteHostSwitch, DefaultNWBLiveFeedRemoteHost),
            getSetting(NWBLiveFeedRemotePortSwitch, DefaultNWBLiveFeedRemotePort),
            aSessionModel.Connection.ModelName, aSessionModel.Connection.ModelID,
            getSetting(NWBLiveFeedPrefixSwitch, DefaultNWBLiveFeedPrefix)),
          palette,
          getSetting(NWBLiveFeedShapeFileNameSwitch, DefaultNWBLiveFeedShapeFileName));
        aSessionModel.Projects.Add(Result);
      end
  else
    // todo: world
    Result := nil;
  end;
end;


end.
