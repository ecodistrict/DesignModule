unit SessionServerSessions;

interface

uses
  Logger,
  StdIni, CmdLin,
  IMB3NativeClient, ByteBuffers, // imb 3
  imb4,
  TimerPool,
  CommandQueue,
  TilerLib,
  FireDAC.Comp.Client,
  SessionServerLib, SessionServerDB, SessionServerUS,
  MyOraLib, DB, Ora, OraSmart, OraObjects,
  ODBFiles2, Delaunay, ESRIWorldFiles,

  Vcl.graphics, // TPicture

  System.JSON,
  System.SysConst, // parse not expanded..
  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector,
  WorldDataCode, WorldLegends,
  System.UITypes, System.Math, System.Generics.Collections, Winapi.Windows, System.Classes, System.SysUtils;


const
  TilerEventNameSwitch = 'TilerEventName';
    DefaultTilerEventName = 'USIdle.Tilers.PC-15176_tsn_tno_nl';

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
  EcodistricBaseScenario = 'undefined';
  CaseVariantManagementReturnEventName = 'data-to-dashboard';

type
  TProjectType = (
    ptUrbanStrategyOracle,
    ptEcoDistrict,
    ptWorld,
    ptNWBLiveFeed
  );

type
  // urban strategy

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
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; aTilerEvent: TEventEntry; aLayerType: Integer; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  protected
    fLayerType: Integer;
    fNewPoiCatID: Integer;
    fPoiCategories: TObjectDictionary<string, TUSPOI>;
  public
    procedure ReadObjects(aSender: TObject);
    procedure RegisterLayer; override;
  end;

  TLegendFormat = (lfVertical, lfHorizontal); // todo:..

  TUSScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aMapView: TMapView; const aTablePrefix: string);
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
  end;

  TUSDBScenario = class
  constructor Create(aID: Integer; const aName, aDescription: string; aParentID, aReferenceID: Integer; const aTablePrefix, aIMBPrefix, aStatus: string);
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
  end;

  TUSProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerEventName: string;
    aDBConnection: TCustomConnection; aMapView: TMapView{; aSourceEPSG: Integer});
  destructor Destroy; override;
  private
    fUSDBScenarios: TObjectDictionary<Integer, TUSDBScenario>;
    fMapView: TMapView;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
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

  // eco-district

  TEcodistrictScenario = class(TScenario)
  public
    procedure AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
      aDefaultLoad: Boolean; aPalette: TWDPalette; aBasicLayer: Boolean;
      const aSchema, aTableName, aIDFieldName, aGeometryFieldName: string);
    procedure ReadBasicData(); override;
  public
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string; overload; override;
  end;

  TEcodistrictProject = class(TProject)
  protected
    procedure ReadObjects(aSender: TObject);
    function getMeasuresJSON: string; override;
  public
    procedure ReadBasicData(); override;
  end;

  TEcodistrictModule = class
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aConnectString, aTilerEventName: string);
  destructor Destroy; override;
  private
    fSessionModel: TSessionModel;
    fConnection: TConnection;
    fConnectString: string;
    fTilerEventName: string;
    fDashboardEvent: TEventEntry;
    fDataEvent: TEventEntry;
    fModuleEvent: TEventEntry;
    fCaseVariantManagementEvent: TEventEntry;
    fProjects: TObjectDictionary<string, TProject>;
    procedure HandleModuleEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleDataEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleCaseVariantManagentEvent(aEventEntry: TEventEntry; const aString: string);
  end;

  // NWB live feed

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
    aTilerEvent: TEventEntry; aLiveFeedConnection: TIMBConnection; const aShapeFileName: string);
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
  end;

  TNWBLiveFeedScenario = class(TScenario)
  constructor Create(aProject: TProject; const aScenarioID: string;
    aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string);
  destructor Destroy; override;
  private
    fLiveFeedConnection: TIMBConnection;
    kpi1: TKPI; // own
  public
  end;

  TNWBLiveFeedProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerEventName: string;
    aLiveFeedConnection: TIMBConnection{IMB3}; aPalette: TWDPalette; const aShapeFilename: string);
  private
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
  public
    procedure ReadBasicData(); override;
    property sourceProjection: TGIS_CSProjectedCoordinateSystem read fSourceProjection;
  end;



var
  PGInited: Boolean = false;


function CreateSessionProject(aSessionModel: TSessionModel; const aProjectID, aProjectName: string; aProjectType: TProjectType;
  const aTilerEventName, aConnectString: string): TProject;

implementation

{ utils }

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
      p.color := odbList[i].Color;
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
  Result := TDiscretePalette.Create(aDescription, entries, noDataColor);
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

constructor TEcodistrictModule.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aConnectString, aTilerEventName: string);
begin
  // publish to dashboard
  // subscribe to modules
  // publish and subscribe to data
  inherited Create;
  fSessionModel := aSessionModel;
  fConnection := aConnection;
  fConnectString := aConnectString;
  fTilerEventName := aTilerEventName;
  fProjects := TObjectDictionary<string, TProject>.Create([doOwnsValues]);
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
  inherited;
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
end;

procedure TEcodistrictModule.HandleDataEvent(aEventEntry: TEventEntry; const aString: string);
begin
  // todo:
end;

procedure TEcodistrictModule.HandleModuleEvent(aEventEntry: TEventEntry; const aString: string);
var
  jsonObject: TJSONObject;
  response: string;
  _type: string;
  _method: string;
  _caseId: string;
  _UserId: string;
  _variants: TJSONArray;
  i: Integer;
  _variant: TJSONObject;
  _variantId: string;
  _variantName: string;
  _variantDescription: string;
  _project: TProject;
  _scenario: TScenario;
  _title: string;
  _polygons: TJSONArray;
  _polygon: TJSONArray;
  _description: string;
  _coordinate: TJSONArray;
  _lat: Double;
  _lon: Double;
  j: Integer;
  extent: TExtent;
begin
  // todo:
  jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
  _type := jsonObject.getValue<string>('type');
  _method := jsonObject.getValue<string>('method');
  Log.WriteLn('HandleModuleEvent: type: '+_type+', method: '+_method);
  // process request from dashboard
  if _type='request' then
  begin
    if _method='getModules' then
    begin
      // respond to getModules with this modules info
      response :=
        '{'+
          '"name": "Design Module",'+
          '"description": "Design and view layer based information and apply measures",'+
          '"kpiList": [],'+
          '"moduleId": "design",'+
          '"method": "getModules",'+
          '"type": "response"'+
        '}';
      fDashboardEvent.signalString(response);
    end
    else if _method='initModule' then
    begin
      _CaseId := jsonObject.getValue<string>('caseId');
      _UserId := jsonObject.getValue<string>('userId');
      // signal getCase
      response := '{"method": "getCase", "type": "request", "userId": "'+_UserId+'", "caseId": "'+_CaseId+'"}';
      fDashboardEvent.signalString(response);
      // signal getVariants
      response := '{"method": "getVariants", "type": "request", "userId": "'+_UserId+'", "caseId": "'+_CaseId+'"}';
      fDashboardEvent.signalString(response);
      // todo: load the case as project
      if fProjects.TryGetValue(_caseId, _project) then
      begin
        // todo; already loaded
      end
      else
      begin
        _project := CreateSessionProject(fSessionModel, _CaseId, '', ptEcoDistrict, fTilerEventName, fConnectString);
        fProjects.Add(_caseId, _project);
      end;
    end;
  end
  // process respons from dashboard
  else if _type='response' then
  begin
    if _method='getCase' then
    begin
      _caseId := jsonObject.getValue<string>('caseId');
      _UserId := jsonObject.getValue<string>('userId');
      if fProjects.TryGetValue(_caseId, _project) then
      begin
        _title := jsonObject.getValue<string>('caseData.title', '');
        _description := jsonObject.getValue<string>('caseData.description', '');
        _project.projectDescription := _description;
        _project.ProjectName := _title;
        // assume multi polygon for coordinates
        _polygons := jsonObject.getValue<TJSONArray>('caseData.districtPolygon.geometry.coordinates');
        extent := TExtent.Create;
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
        then (_project as TEcodistrictProject).mapView := TMapView.Create(extent.centerY, extent.centerX, 11) // todo: calculate zoomlevel from extent?
        else (_project as TEcodistrictProject).mapView := TMapView.Create(55.7, 41, 4); //  whole of europe?
        // add base scenario
        if not _project.scenarios.ContainsKey(EcodistricBaseScenario) then
        begin
          _scenario := TEcodistrictScenario.Create(_project, _project.ProjectID, _project.ProjectName, _project.ProjectDescription, (_project as TEcodistrictProject).mapView);
          _project.scenarios.Add(EcodistricBaseScenario, _scenario);
          Log.WriteLn('added base scenario '+EcodistricBaseScenario, llNormal, 1);
        end
        else Log.WriteLn('already contains base scenario '+EcodistricBaseScenario, llNormal, 1);
      end;
    end
    else if (_method='getVariants') then
    begin
      _caseId := jsonObject.getValue<string>('caseId');
      _UserId := jsonObject.getValue<string>('userId');
      if fProjects.TryGetValue(_caseId, _project) then
      begin
        // parse variants and load them as scenarios
        _variants := jsonObject.getValue<TJSONArray>('variants');
        for i := 0 to _variants.Count-1  do
        begin
          _variant := _variants.Items[i] as TJSONObject;
          //_id
          //name
          //description
          _variantId := _variant.GetValue<string>('_id');
          _variantName := _variant.GetValue<string>('name');
          _variantDescription := _variant.GetValue<string>('description');
          if _project.scenarios.TryGetValue(_variantId, _scenario) then
          begin
            // scenario already defined
            // todo:
            Log.WriteLn('existing scenario '+_variantId, llNormal, 1);
          end
          else
          begin
            // add scenario
            _scenario := TEcodistrictScenario.Create(_project, _variantId, _variantName, _variantDescription, (_project as TEcodistrictProject).mapView);
            _project.scenarios.Add(_variantId, _scenario);
            //_scenario.
            // todo: add layers to the scenario! ->

            Log.WriteLn('added scenario '+_variantId, llNormal, 1);
          end;
        end;
      end;
    end;
  end;
end;

{ TEcodistrictScenario }

procedure TEcodistrictScenario.AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
  aDefaultLoad: Boolean; aPalette: TWDPalette; aBasicLayer: Boolean;
  const aSchema, aTableName, aIDFieldName, aGeometryFieldName: string);
var
  layer: TLayer;
begin
  layer := TLayer.Create(self, aDomain, aID, aName, aDescription, aDefaultLoad, aPalette, aObjectTypes, aGeometryType, fProject.TilerEvent, aBasicLayer);
  layer.query := PGSVGPathsQuery('"'+aSchema+'"."'+aTableName+'"', aIDFieldName, aGeometryFieldName);
  //AddCommandToQueue(sl, Self.ReadObjects);
  (fProject as TEcodistrictProject).ReadObjects(layer);
  Layers.Add(layer.ID, layer);
end;

procedure TEcodistrictScenario.ReadBasicData;
var
  //dbConnection: TCustomConnection;
  schema: string;
begin
  // read ecodistrict data
  //dbConnection := (fProject as TEcodistrictProject).fDBConnection;
  schema := EcodistrictCasePrefix+fID;
  // buildings              -> v
  //AddLayerFromTable('basic structures', 'buildings', 'buildings', 'basic buildings', '"building"', 'MultiPolygon', false, nil, True, schema, 'bldg_building', 'attr_gml_id', 'bldg_lod1multisurface_value');
  AddLayerFromTable('basic structures', 'buildings', 'buildings', 'basic buildings', '"building"', 'MultiPolygon', false, nil, True, schema, 'bldg_building', 'attr_gml_id', 'bldg_lod0footprint_value');


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
begin
  // todo:
  Result := '';
end;

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string;
begin
  // todo:
  Result := '';
end;

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string;
begin
  // todo:
  Result := '';
end;

{ TEcodistrictProject }

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
//var
//  sl: TLayer;
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
end;

procedure TEcodistrictProject.ReadObjects(aSender: TObject);
var
  query: TFDQuery;
begin
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
  aPalette: TWDPalette; const aObjectTypes, aGeometryType: string; aTilerEvent: TEventEntry; aLayerType: Integer; aBasicLayer: Boolean);
begin
  fLayerType := aLayerType;
  fPoiCategories := TObjectDictionary<string, TUSPOI>.Create([doOwnsValues]);
  fNewPoiCatID := 0;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aPalette, aObjectTypes, aGeometryType, aTilerEvent, aBasicLayer);
end;

destructor TUSLayer.Destroy;
begin
  inherited;
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
  stream: TBytesStream;
begin
  // todo: implement
  // register layer with tiler?
  // start query
  // decode objects and add to aLayer
  // send objects to tiler?

  // create new ora session because we are running in a different thread
  oraSession := TOraSession.Create(nil);
  try
    with (scenario.Project as TUSProject).OraSession
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
              projectGeometryPoint(geometryPoint, (scenario.Project as TUSProject).sourceProjection);
              objects.Add(oid, TGeometryPointLayerObject.Create(Self, oid, geometryPoint, value));
            end;
          4: // road (VALUE_EXPR)
            begin
              // unidirectional, not left and right
              value := FieldFloatValueOrNaN(query.Fields[1]);
              geometry := CreateWDGeometryFromSDOShape(query, 'SHAPE');
              projectGeometry(geometry, (scenario.Project as TUSProject).sourceProjection);
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
              projectGeometry(geometry, (scenario.Project as TUSProject).sourceProjection);
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
                  stream  := TBytesStream.Create;
                  try
                    usPOI.picture.Graphic.SaveToStream(stream);
                    fOutputEvent.signalEvent(TByteBuffer.bb_tag_tbytes(icehTilerPOIImage, stream.Bytes)); // todo: check correct number of bytes
                  finally
                    stream.Free;
                  end;
                end;
              finally
                objects.Add(oid, TGeometryLayerPOIObject.Create(Self, oid, usPOI.ID, geometryPoint));
              end;
            end
        else
          value := FieldFloatValueOrNaN(query.FieldByName('VALUE'));
          geometry := CreateWDGeometryFromSDOShape(query, 'SHAPE');
          projectGeometry(geometry, (scenario.Project as TUSProject).sourceProjection);
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
      RegisterOnTiler(False, stReceptor, name, GetSetting(MaxEdgeLengthInMetersSwitchName, DefaultMaxEdgeLengthInMeters));
    //2:; grid
    3, // buildings
    8: // RS buildings
      RegisterOnTiler(False, stGeometry, name); // polygon
    4: // road color (VALUE_EXPR) unidirectional
      RegisterOnTiler(False, stGeometryI, name); // path, intensity
    5: // road color (VALUE_EXPR) and width (TEXTURE_EXPR) left and right
      RegisterOnTiler(False, stGeometryICLR, name); // path, intensity/capacity left/right
    9: // energy color (VALUE_EXPR) and width (TEXTURE_EXPR)
      RegisterOnTiler(False, stGeometryIC, name); // path, intensity/capacity unidirectional
    11: // points, basic layer
      RegisterOnTiler(False, stLocation, name);
    21: // POI
      RegisterOnTiler(False, stPOI, name);
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
            Result := Result+'{"'+aMetaLayerEntry.odbList[i].Description+'":"'+ColorToJSON(aMetaLayerEntry.odbList[i].Color)+'"}';
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
            Result := Result+'"'+aMetaLayerEntry.odbList[i].Description+'":"'+ColorToJSON(aMetaLayerEntry.odbList[i].Color)+'"';
          end;
        end;
        Result := '"grid2":{"title":"'+FormatLegendDescription(aMetaLayerEntry.LEGEND_DESC)+'","labels":[{'+Result+'}]}';;
      end;
  end;
end;

constructor TUSScenario.Create(aProject: TProject; const aID, aName, aDescription: string; aMapView: TMapView; const aTablePrefix: string);
begin
  fTablePrefix := aTablePrefix;
  fMetaLayer := TMetaLayer.Create([doOwnsValues]);
  inherited Create(aProject, aID, aName, aDescription, aMapView);
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

  procedure AddBasicLayer(const aID, aName, aDescription, aDefaultDomain, aObjectType, aGeometryType, aQuery: string; aLayerType: Integer);
  var
    layer: TUSLayer;
  begin
    layer := TUSLayer.Create(Self,
      standardIni.ReadString('domains', aObjectType, aDefaultDomain), //  domain
      aID, aName, aDescription, false,
      TDiscretePalette.Create('basic palette', [], colorBasicOutline),
       '"'+aObjectType+'"', aGeometryType , fProject.tilerEvent, aLayerType, True);
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
  palette: TWDPalette;
  objectTypes: string;
  geometryType: string;
  indicTableNames: TAllRowsSingleFieldResult;
  tableName: string;
  geneTables: TAllRowsSingleFieldResult;
  i: Integer;
begin
  // process basic layers
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
        'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_PIPELINES' then
    begin // check count
      if ReturnRecordCount(OraSession, tableName)>0
      then AddBasicLayer(
             'pipeline', 'pipe lines', 'pipe lines', 'basic structures', 'pipe line', 'LineString',
             'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_BUILDING' then
    begin // always
      AddBasicLayer(
        'building', 'buildings', 'buildings', 'basic structures', 'building', 'MultiPolygon',
        'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 3);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_SCREEN' then
    begin // always
      AddBasicLayer(
        'screen', 'screens', 'screens', 'basic structures', 'screen', 'LineString',
        'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_TRAM' then
    begin // check count
      if ReturnRecordCount(OraSession, tableName)>0
      then AddBasicLayer(
             'tramline', 'tram lines', 'tram lines', 'basic structures', 'tram line', 'LineString',
             'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_BIKEPATH' then
    begin // check count
      if ReturnRecordCount(OraSession, tableName)>0
      then AddBasicLayer(
             'bikepath', 'bike paths', 'bike paths', 'basic structures', 'bike path', 'LineString',
             'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_RAIL' then
    begin // check count
      if ReturnRecordCount(OraSession, tableName)>0
      then AddBasicLayer(
             'railline', 'rail lines', 'rail lines', 'basic structures', 'rail line', 'LineString',
             'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 4);
    end
    else if tableName=fTablePrefix.ToUpper+'GENE_INDUSTRY_SRC' then
    begin // check count
      if ReturnRecordCount(OraSession, tableName)>0
      then AddBasicLayer(
             'industrysource', 'industry sources', 'industry sources', 'basic structures', 'industry source', 'Point',
             'SELECT OBJECT_ID, 0 AS VALUE, t.SHAPE FROM '+tableName+' t', 11);
    end;
    //GENE_NEIGHBORHOOD
    //GENE_RESIDENCE
    //GENE_NODE
    //GENE_DISTRICT
    //GENE_STUDY_AREA
    //GENE_BASCOV
  end;
  // process meta layer to build list of available layers
  ReadMetaLayer(OraSession, fTableprefix, fMetaLayer);
  Log.WriteLn(elementID+': found '+fMetaLayer.Count.ToString+' layers in meta_layer');
  for mlp in fMetaLayer do
  begin
    // try tablename-value
    // try legend description-value
    layerInfo := StandardIni.ReadString('layers', mlp.Value.LAYER_TABLE.Trim+'-'+mlp.Value.VALUE_EXPR.trim, '');
    if layerInfo=''
    then layerInfo := StandardIni.ReadString('layers', mlp.Value.LEGEND_DESC.trim+'-'+mlp.Value.VALUE_EXPR.trim, '');

    if layerInfo<>'' then
    begin
      layerInfoParts := layerInfo.Split([',']);
      if length(layerInfoParts)<2 then
      begin
        setLength(layerInfoParts, 2);
        layerInfoParts[1] := mlp.Value.LEGEND_DESC;
      end;

      //mlp.Value.LEGEND_DESC
      //palette := nil; //  todo: create palette from avl
      palette := CreatePaletteFromODB(mlp.Value.LEGEND_DESC, mlp.Value.odbList, True);

      case mlp.Value.LAYER_TYPE mod 100 of // i+100 image layer version same as i but ignored by US3D
        1:
          begin
            objectTypes := '"receptor"';
            geometryType := 'Point';
          end;
        2:
          begin
            objectTypes := '"grid"';
            geometryType := 'MultiPolygon'; // todo: ?
          end;
        3, 8:
          begin
            objectTypes := '"building"'; // 3 buildings, 8 RS buildings
            geometryType := 'MultiPolygon';
          end;
        4,    // road color (VALUE_EXPR)
        5:    // road color (VALUE_EXPR) and width (TEXTURE_EXPR)
          begin
            objectTypes := '"road"';
            geometryType := 'LineString';
          end;
        9:    // enrg color (VALUE_EXPR) and width (TEXTURE_EXPR)
          begin
            objectTypes := '"energy"';
            geometryType := 'LineString';
          end;
        11:
          begin
            objectTypes := '"location"';
            geometryType := 'Point';
          end;
        21:
          begin
            objectTypes := '"poi"';
            geometryType := 'Point';
          end;
      else
        // 31 vector layer ?
        objectTypes := '';
        geometryType := '';
      end;
      if geometryType<>'' then
      begin
        layer := TUSLayer.Create(Self,
          standardIni.ReadString('domains', layerInfoParts[0], layerInfoParts[0]), //  domain
          mlp.Key.ToString, // id
          layerInfoParts[1], // name
          mlp.Value.LEGEND_DESC.Replace('~~', '-').replace('\', '-'), // description
          false, //false, // todo: default load
          palette,
          objectTypes, geometryType, fProject.tilerEvent,
          mlp.Value.LAYER_TYPE mod 100);
        // todo: create layer
        // todo: create legend
        //sl.legendJSON := '';
        layer.fLegendJSON := BuildLegendJSON(mlp.Value, layer, lfVertical);
        layer.query := mlp.Value.SQLQuery(fTableprefix);
        //sl.objectsTilesID := -1;
        //sl.objectsTilesLink := '';
        Layers.Add(layer.ID, layer);
        Log.WriteLn(elementID+': added layer '+layer.ID+', '+layer.domain+'/'+layer.description, llNormal, 1);
        // schedule reading objects and send to tiler
        AddCommandToQueue(Self, layer.ReadObjects);
      end
      else Log.WriteLn(elementID+': skipped layer type '+mlp.Value.LAYER_TYPE.ToString+' ('+mlp.Key.ToString+') '+mlp.Value.LAYER_TABLE+'-'+mlp.Value.VALUE_EXPR, llRemark, 1);
    end
    else Log.WriteLn(elementID+': skipped layer ('+mlp.Key.ToString+') '+mlp.Value.LAYER_TABLE+'-'+mlp.Value.VALUE_EXPR, llRemark, 1);
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
end;

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string;
var
  layers: TList<TLayer>;
  categories: string;
  objectsGeoJSON: string;
  totalObjectCount: Integer;
  extent: TExtent;
  l: TLayer;
  objectCount: Integer;
begin
  // todo: implement
  Result := '';
  layers := TList<TLayer>.Create;
  try
    if selectLayersOnCategories(aSelectedCategories, layers) then
    begin
      categories := '';
      objectsGeoJSON := '';
      totalObjectCount := 0;
      extent := TExtent.FromGeometry(aGeometry);
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
  // todo: implement
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
  // todo: implement
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

constructor TUSDBScenario.Create(aID: Integer; const aName, aDescription: string; aParentID, aReferenceID: Integer; const aTablePrefix, aIMBPrefix, aStatus: string);
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

constructor TUSProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerEventName: string;
  aDBConnection: TCustomConnection; aMapView: TMapView);
begin
  fUSDBScenarios := TObjectDictionary<Integer, TUSDBScenario>.Create;
  fMapView := aMapView;
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerEventName, aDBConnection, 0, False, False, False); //1, True, True, True);
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
//var
//  _ID: Integer;
//  _name: string;
//  _description: string;
//  _tableprefix: string;
//  refScenarioID: Integer;
//  refTablePrefix: string;
begin
  ReadScenarios;
  ReadMeasures;
  //fMeasuresJSON := '[]';
  // load current scenario and ref scenario first
  fCurrentScenario := readScenario(GetCurrentScenarioID(OraSession).ToString());
  //_name := GetScenarioName(OraSession, _ID);
  //_description := GetScenarioDescription(OraSession, _ID);
  //_tableprefix := GetScenarioTablePrefix(OraSession, _ID);
  //fCurrentScenario := TUSScenario.Create(Self, _ID.ToString(), _name, _description, fMapView, _tableprefix);
  //fScenarios.Add(fCurrentScenario.ID, fCurrentScenario);

  Log.WriteLn('current US scenario: '+fCurrentScenario.ID+' ('+(fCurrentScenario as TUSScenario).fTableprefix+'): "'+fCurrentScenario.description+'"', llOk);

  // .. ref scenario
  // todo: skip for now..
  {
  refScenarioID := GetScenarioBaseID(OraSession, scenarioID);
  if refScenarioID>=0 then
  begin
    refTablePrefix := GetScenarioTablePrefix(OraSession, refScenarioID);
    fRefScenario := TUSScenario.Create(Self, refScenarioID.ToString(), refTableprefix, fMapView);
    fScenarios.Add(fRefScenario.ID, fRefScenario);
    Log.WriteLn('ref scanario: '+fRefScenario.ID+' ('+refTableprefix+')', llOk);
  end
  else Log.WriteLn('NO ref scanario', llWarning);
  }
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
        Result := TUSScenario.Create(Self, aID, dbScenario.name, dbScenario.description, fMapView, dbScenario.tablePrefix);
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
  scenarios := ReturnAllResults(OraSession,
    'SELECT ID, Name, Federate, Parent_ID, Base_ID, Notes, SCENARIO_STATUS '+
    'FROM META_SCENARIOS '+
    'ORDER BY ID ASC');
  for s := 0 to Length(scenarios)-1 do
  begin
    usdbScenario := TUSDBScenario.Create(scenarios[s][0].ToInteger, scenarios[s][1], scenarios[s][5],
      StrToIntDef(scenarios[s][3], -1), StrToIntDef(scenarios[s][4], -1), scenarios[s][1]+'#', scenarios[s][2], scenarios[s][6]);
    fUSDBScenarios.Add(usdbScenario.id, usdbScenario);
    if //(usdbScenario.name='V15') or // added because is parent for some..
       (usdbScenario.name='V20') or
       (usdbScenario.name='V22') or
       (usdbScenario.name='V23') or
       (usdbScenario.name='V25') or
       (usdbScenario.name='V33') or
       (usdbScenario.name='V34') or
       (usdbScenario.name='V36') or
       (usdbScenario.name='V30') or
       (usdbScenario.name='V27') or
       (usdbScenario.name='V32') or
       (usdbScenario.name='V28') then
    begin
      fScenarioLinks.children.Add(TScenarioLink.Create(
        usdbScenario.ID, usdbScenario.parentID, usdbScenario.referenceID,
        usdbScenario.name, usdbScenario.description, scenarios[s][6], nil));
    end;
  end;
  // (re)link
  for isp in fUSDBScenarios
  do isp.Value.Relink(fUSDBScenarios);
  // build hierarchy
  fScenarioLinks.buildHierarchy();
  // filter closed 'leaves'
  fScenarioLinks.removeLeave('CLOSED');
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
    projection := (layer.scenario.Project as TNWBLiveFeedProject).sourceProjection;
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
  aPalette: TWDPalette; aTilerEvent: TEventEntry; aLiveFeedConnection: TIMBConnection; const aShapeFileName: string);
var
  shape: TGIS_Shape;
  objJSON: string;
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aPalette, '"road"', 'LineString', aTilerEvent);
  fLiveFeedConnection := aLiveFeedConnection; // ref
  fObjectsAdded := TNWBObjectList.Create(False); // refs
  fObjectsUpdated := TNWBObjectList.Create(False); // refs
  fObjectsDeleted := TNWBObjectList.Create(True); // owns!
  fUpdateTimeOut := aScenario.Project.Timers.SetTimer(HandleUpdateTimeOut);
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
  fCleanupTimer := scenario.Project.Timers.SetTimerDelta(
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
    do jsonAdd(json, '"'+string(obj.id)+'"'+':"'+ palette.ValueToColorJSON((obj as TNWBLiveFeedRoad).fVp)+'"');
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

type
  TOldTilerLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; aTilerEvent: TEventEntry; aRefreshEvent: TIMBEventEntry; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  private
    fRefreshEvent: TIMBEventEntry;
    procedure HandleRefreshEvent(aEvent: TIMBEventEntry; var aPayload: ByteBuffers.TByteBuffer); stdcall;
  end;

{ TOldTilerLayer }

constructor TOldTilerLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean; aPalette: TWDPalette;
    const aObjectTypes, aGeometryType: string; aTilerEvent: TEventEntry; aRefreshEvent: TIMBEventEntry; aBasicLayer: Boolean);
begin
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aPalette, aObjectTypes, aGeometryType, aTilerEvent, aBasicLayer);
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
    do client.SendRefresh(elementID, '', objectsTilesLink); // todo: add time stamp?
  finally
    TMonitor.Exit(clients);
  end;
end;

{ TNWBLiveFeedScenario }

procedure addLiveAirLayer(aScenario: TScenario; const aDomain: string; aLayerID: Integer; const aName, aDescription: string;
  aTilerEvent: TEventEntry; const aAVLFileName: string; aLiveFeedConnection: TIMBConnection);
var
  palette: TWDPalette;
  layer: TLayer;
begin
  if FileExists(aAVLFileName) then
  begin
    palette := CreatePaletteFromODB(aName, ODBFileToODBList(aAVLFileName), True);
    layer := aScenario.AddLayer(
      TOldTilerLayer.Create(aScenario, aDomain, aLayerID.toString, aName, aDescription, False,
      palette, '"road"', 'tile', aTilerEvent, aLiveFeedConnection.Subscribe('layers.'+aLayerID.ToString)));
    layer.objectsTilesID := aLayerID;
    layer.objectsTilesLink := GetSetting(NWBLiveFeedTilesURLSwitch, DefaultNWBLiveFeedTilesURL)+
      '?layer='+aLayerID.ToString+'&zoom={z}&x={x}&y={y}';
  end
  else Log.WriteLn('Could not create live air layer: avl '+aAVLFileName+' not found', llError);
end;

constructor TNWBLiveFeedScenario.Create(aProject: TProject; const aScenarioID: string;
  aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string);
var
  resourceFolder: string;
begin
  // todo: rotterdam
  inherited Create(aProject, aScenarioID, aScenarioID, '', TMapView.Create(51.946333, 4.311171, 11));
  fLiveFeedConnection := aLiveFeedConnection;
  AddLayer(
    TNWBLiveFeedLayer.Create(Self, NWBTrafficDomain, 'live', 'Live traffic i/c', 'Live traffic feed', True, aPalette,
    fProject.TilerEvent, fLiveFeedConnection, aShapeFileName));
  resourceFolder := ExtractFilePath(ParamStr(0));
  //addLiveAirLayer(Self, AirDomain, 121, 'NOx',  '', fProject.TilerEvent, 'no2.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 122, 'PM10', '', fProject.TilerEvent, resourceFolder+'pm10.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 123, 'PM25', '', fProject.TilerEvent, resourceFolder+'pm25.avl', fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 124, 'NO2',  '', fProject.TilerEvent, 'no2.avl',  fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 132, 'PM10 tot', '', fProject.TilerEvent, 'pm10.avl', fLiveFeedConnection);
  //addLiveAirLayer(Self, AirDomain, 133, 'PM25 tot', '', fProject.TilerEvent, 'pm25.avl', fLiveFeedConnection);
  addLiveAirLayer(Self, NWBAirDomain, 134, 'NO2',  '', fProject.TilerEvent, resourceFolder+'no2.avl',  fLiveFeedConnection);
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

constructor TNWBLiveFeedProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerEventName: string;
  aLiveFeedConnection: TIMBConnection; aPalette: TWDPalette; const aShapeFilename: string);
begin
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerEventName, nil, 0, False, False, False);
  {if aSourceEPSG>0
  then fSourceProjection := CSProjectedCoordinateSystemList.ByEPSG(aSourceEPSG)
  else }
  fSourceProjection := CSProjectedCoordinateSystemList.ByWKT('Amersfoort_RD_New'); // EPSG: 28992
  fCurrentScenario := TNWBLiveFeedScenario.Create(Self, 'Live', aLiveFeedConnection, aPalette, aShapeFileName);
  fScenarios.Add(fCurrentScenario.ID, fCurrentScenario);
end;

procedure TNWBLiveFeedProject.ReadBasicData;
begin
  //fMeasuresJSON := '[]';
  // all reading is done in live layer
end;

{ utils }

function CreateSessionProject(aSessionModel: TSessionModel; const aProjectID, aProjectName: string; aProjectType: TProjectType; const aTilerEventName, aConnectString: string): TProject;
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
        Result := TUSProject.Create(aSessionModel, aSessionModel.Connection, aProjectID, aProjectName, aTilerEventName,
          dbConnection, TMapView.Create(51.919775, 4.403763 {51.914852, 4.394151}, 13));
        aSessionModel.Projects.Add(Result);
      end;
    ptEcoDistrict:
      begin
        if not PGInited then
        begin
          InitPG;
          PGInited := True;
        end;
        dbConnection := TFDConnection.Create(nil);
        SetPGConnection(dbConnection as TFDConnection, aConnectString);
        Result := TEcodistrictProject.Create(aSessionModel, aSessionModel.Connection, aProjectID, aProjectName, aTilerEventName, dbConnection, 0, True, True, True);
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
        Result := TNWBLiveFeedProject.Create(aSessionModel, aSessionModel.Connection, aProjectID, aProjectName, aTilerEventName,
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
