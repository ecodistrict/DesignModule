unit SessionServerEcodistrict;

interface

uses
  Logger,

  imb4,
  WorldDataCode,
  WorldLegends,

  Data.DB,
  FireDAC.Comp.Client,

  TilerControl,

  SessionServerDB,
  SessionServerLib,

  System.JSON,
  System.Generics.Collections,
  System.SysUtils;

const
  // ecodistrict
  EcodistrictCasePrefix = 'trout_'; // Nicolas's prefix to avoid schema names starting with numbers
  EcodistrictBaseScenario = 'undefined';
  CaseVariantManagementReturnEventName = 'data-to-dashboard';

  EcodistrictConnectStringSwitchName = 'EcodistrictConnectString';



type
  // eco-district
  TEcodistrictLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  destructor Destroy; override;
  protected
    fLayerType: Integer;
    fPalette: TWDPalette;
  public
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

  TEcodistrictScenario = class(TScenario)
  protected
    procedure ReadObjectFromQuery(aLayer: TLayer);
  public
    function AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
      aDefaultLoad: Boolean; aBasicLayer: Boolean;
      const aSchema, aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;
    function AddLayerFromQuery(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
      aDefaultLoad: Boolean; aBasicLayer: Boolean;
      const aSchema, aQuery: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;
    procedure ReadBasicData(); override;
  public
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aQuery: string): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories: TArray<string>; const aSelectedIDs: TArray<string>): string; overload; override;

    function selectObjectsProperties(aClient: TClient; const aSelectedCategories, aSelectedObjects: TArray<string>): string; override;
  end;

  {
    CREATE TABLE public.di_objectproperties
    (
      category text NOT NULL,
      propertyname text NOT NULL,
      propertytype text,
      selection text,
      fieldname text,
      editable boolean,
      tablename text,
      keyfieldname text,
      CONSTRAINT dm_objectproperties_pkey PRIMARY KEY (category, propertyname)
    )
    WITH (
      OIDS=FALSE
    );
    ALTER TABLE public.di_objectproperties
      OWNER TO postgres;
    COMMENT ON TABLE public.di_objectproperties
      IS 'This table contains the editable properties of objects for the design interface module';

  }

  TDIObjectProperty = class
    category: string;
    propertyName: string;
    propertyType: string;
    selection: string;
    fieldName: string;
    tableName: string;
    keyFieldName: string;
    editable: Boolean;
  end;

  {
    CREATE TABLE public.di_queries
    (
      id text NOT NULL,
      domain text,
      name text,
      description text,
      objecttypes text,
      geometrytype text,
      defaultload integer,
      basiclayer integer,
      sql text,
      tablename text,
      idfieldname text,
      geometryfieldname text,
      datafieldname text,
      layertype integer,
      palettejson text,
      legendjson text,
      CONSTRAINT di_queries_pkey PRIMARY KEY (id)
    );
  }

  TDIQuery = record //  query for design interface part
    id: string;
    domain: string;
    name: string;
    description: string;
    objecttypes: string;
    geometrytype: string;
    defaultload: Integer;
    basiclayer: integer;
    // sql OR tablename, idfieldname, geometryfieldname, datafieldname!
    sql: string;
    tablename: string;
    idfieldname: string;
    geometryfieldname: string;
    datafieldname: string;
    layertype: integer;
    palettejson: string;
    legendjson: string; // can be generated from palettejson
  end;

  {
    CREATE TABLE public.dm_queries
    (
      object_id integer NOT NULL,
      returntype text,
      request text,
      query text,
      module text,
      CONSTRAINT dm_queries_pkey PRIMARY KEY (object_id)
    )
    WITH (
      OIDS=FALSE
    );
    ALTER TABLE public.dm_queries
      OWNER TO ecodistrict;
    COMMENT ON TABLE public.dm_queries
      IS 'This table contains the queries for the datamodule.';
  }

  TDMQuery = record // query for data module part
    module: string;
    SQL: string;
    ReturnType: string;
  end;

  TDIMeasureHistory = record
    id: TGUID;
    measure_cat: string;
    measure_id: Integer;
    object_ids: string;
    timeutc: TDateTime;
    variants: string;
  end;

  TEcodistrictProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    function getDMQueries: TDictionary<string, TDMQuery>;
    function getDIQueries: TDictionary<string, TDIQuery>;
    function getDIObjectProperties: TObjectList<TDIObjectProperty>;
    function getDIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory>;
  protected
    fDIObjectProperties: TObjectList<TDIObjectProperty>;
    fDMQueries: TDictionary<string, TDMQuery>;
    fDIQueries: TDictionary<string, TDIQuery>;
    fDIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory>;

    function getMeasuresJSON: string; override;
    function getMeasuresHistoryJSON: string; override;

    function ReadSchemaNames: TArray<string>;
    function handleTilerStatus(aTiler: TTiler): string;

    procedure handleClientMessage(aJSONObject: TJSONObject); override;
  public
    // on demand load of items
    property DMQueries: TDictionary<string, TDMQuery> read getDMQueries;
    property DIQueries: TDictionary<string, TDIQuery> read getDIQueries;
    property DIObjectProperties: TObjectList<TDIObjectProperty> read getDIObjectProperties;
    property DIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory> read getDIMeasuresHistory;
    // (re-)read items
    function ReadDMQueries: Boolean;
    function ReadDIQueries: Boolean;
    function ReadDIObjectProperties: Boolean;
    function ReadDIMeasuresHistory: Boolean;

    function ReadScenario(const aID: string): TScenario; override;
    procedure ReadBasicData(); override;
    function PingDatabase(const aCaller: string): Boolean;
  end;

  TEcodistrictModule = class
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aConnectString, aTilerFQDN, aTilerStatusURL: string;
    aMaxNearestObjectDistanceInMeters: Integer);
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
    fMaxNearestObjectDistanceInMeters: Integer;

    function SchemaExists(aSchemaName: string): boolean;
    function SchemaCreate(aSchemaName: string; aFromSchemaName: string = 'public'): boolean;
    function SchemaDelete(aSchemaName: string): boolean;

    function getDMQueries(const aCaseId: string; var aQueries: TDictionary<string, TDMQuery>): boolean;

    function forceReadOfDMQueries(const aCaseId: string): Boolean;
    function forceReadOfDIQueries(const aCaseId: string): Boolean; // todo: does not reload layers!
    function forceReadOfDIObjectProperties(const aCaseId: string): Boolean;
    function forceReadOfDIMeasuresHistory(const aCaseId: string): Boolean;

    function GetOrAddCase(const aCaseId: string): TProject;
    procedure HandleModuleCase(const aCaseId, aCaseTitle,  aCaseDescription: string; const aMapView: TMapView);
    procedure HandleModuleCaseDelete(const aCaseId: string);
    procedure HandleModuleVariant(const aCaseId, aVariantID, aVariantName, aVariantDescription: string);
    procedure HandleModuleVariantDelete(const aCaseId, aVariantId: string);

    procedure HandleModuleEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleDataEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleCaseVariantManagentEvent(aEventEntry: TEventEntry; const aString: string);

    function HandleModuleScenarioRefresh(const aCaseId, aVariantId: string): Boolean;
  end;


function EcoDistrictSchemaId(const aCaseId: string; const aVariantId: string=''): string;

implementation

function EcoDistrictSchemaId(const aCaseId: string; const aVariantId: string=''): string;
begin
  if (aVariantId='') or (aVariantId='null') or (aVariantId='None')
  then Result := EcoDistrictCasePrefix + aCaseId
  else Result := EcoDistrictCasePrefix + aCaseId + '_' + aVariantId;
end;

{ TEcodistrictLayer }

constructor TEcodistrictLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  fLayerType := aLayerType;
  fPalette := aPalette;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, Double.NaN, aBasicLayer);
  fLegendJSON := aLegendJSON; // property of TLayer
end;

destructor TEcodistrictLayer.Destroy;
begin
  FreeAndNil(fPalette);
  inherited;
end;

procedure TEcodistrictLayer.RegisterLayer;
begin
  RegisterOnTiler(False, SliceType, name, 2500);
end;

procedure TEcodistrictLayer.RegisterSlice;
begin
  if Assigned(fPalette)
  then tilerLayer.signalAddSlice(fPalette.Clone)
  else tilerLayer.signalAddSlice(nil);
end;

function TEcodistrictLayer.SliceType: Integer;
begin
  Result := fLayerType; // in ecodistrict slice type=layer type
end;

{ TEcodistrictScenario }

function TEcodistrictScenario.AddLayerFromQuery(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
  aDefaultLoad, aBasicLayer: Boolean; const aSchema: string;
  const aQuery: string;
  aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;
begin
  Result := TEcodistrictLayer.Create(self, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, aLayerType, aPalette, aLegendJSON, aBasicLayer);
  try
    Result.query := aQuery.Replace('{case_id}', aSchema);
    ReadObjectFromQuery(Result);
    Result.RegisterLayer;
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

function TEcodistrictScenario.AddLayerFromTable(const aDomain, aID, aName, aDescription, aObjectTypes, aGeometryType: string;
  aDefaultLoad, aBasicLayer: Boolean; const aSchema:string;
  const aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string;
  aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string): TLayer;
begin
  Result := TEcodistrictLayer.Create(self, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, aLayerType, aPalette, aLegendJSON, aBasicLayer);
  try
    Result.query := PGSVGPathsQuery('"'+aSchema+'".'+aTableName.Replace('{case_id}', aSchema), aIDFieldName, aGeometryFieldName, aDataFieldName);
    ReadObjectFromQuery(Result);
    Result.RegisterLayer;
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
  palette: TWDPalette;
  legendJSON: string;
  iqp: TPair<string, TDIQuery>;
  jsonPalette: TJSONValue;
  layer: TLayer;
begin
  // read ecodistrict data
  if fID=fProject.ProjectID
  then schema := EcoDistrictSchemaId(fID)
  else schema := EcoDistrictSchemaId(fProject.ProjectID, fID);

  // todo: build layers from di_queries

  for iqp in (project as TEcodistrictProject).DIQueries do
  begin
    jsonPalette := TJSONObject.ParseJSONValue(iqp.Value.palettejson);
    if Assigned(jsonPalette) then
    begin
      case jsonPalette.GetValue<Integer>('palettetype', 0) of
        wdkPaletteDiscrete:
          begin
            palette := TDiscretePalette.CreateFromJSON(iqp.Value.palettejson);
            if iqp.Value.legendjson<>''
            then legendJSON := iqp.Value.legendjson
            else legendJSON := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical); // todo: parameterize
          end;
        wdkPaletteRamp:
          begin
            palette := TRampPalette.CreateFromJSON(iqp.Value.palettejson);
            legendJSON := iqp.Value.legendjson;
          end;
      else
        palette := CreateBasicPalette;
        legendJSON := iqp.Value.legendjson;
      end;
    end
    else
    begin
      palette := CreateBasicPalette;
      legendJSON := iqp.Value.legendjson;
      if iqp.Value.palettejson<>''
      then Log.WriteLn('Invalid palette JSON: '+iqp.Value.palettejson, llError);
    end;
    if Layers.TryGetValue(iqp.key, layer) then
    begin
      // todo: check: does this work?
      (layer as TEcodistrictLayer).fPalette.Free;
      (layer as TEcodistrictLayer).fPalette := palette;
      layer.legendJSON := legendJSON;
      layer.RegisterSlice;
      ReadObjectFromQuery(layer);
      // assume that on update the registration on the tiler already succeeded so we can just start signaling objects
      // todo: maybe check if already registered on tiler?
      layer.signalObjects(nil);
    end
    else
    begin
      if iqp.Value.tablename<>'' then
      begin
        AddLayerFromTable(
          iqp.Value.domain, iqp.key,  iqp.Value.name, iqp.Value.description, iqp.Value.objecttypes, iqp.Value.geometrytype,
          iqp.Value.defaultload=1, iqp.Value.basiclayer=1,
          schema, iqp.Value.tablename, iqp.Value.idfieldname, iqp.Value.geometryfieldname, iqp.Value.datafieldname,
          iqp.Value.layertype, palette, legendJSON);
      end
      else
      begin
        AddLayerFromQuery(
          iqp.Value.domain, iqp.key,  iqp.Value.name, iqp.Value.description, iqp.Value.objecttypes, iqp.Value.geometrytype,
          iqp.Value.defaultload=1, iqp.Value.basiclayer=1,
          schema, iqp.Value.sql,
          iqp.Value.layertype, palette, legendJSON);
      end;
    end;
  end;

  (*
  // buildings              -> v
  //AddLayerFromTable('basic structures', 'buildings', 'buildings', 'basic buildings', '"building"', 'MultiPolygon', false, True, schema, 'bldg_building', 'attr_gml_id', 'bldg_lod1multisurface_value');
  AddLayerFromTable('basic structures', 'building',  'buildings', 'basic buildings',    '"building"',   'MultiPolygon', false, True, schema, '"bldg_building"', 'attr_gml_id', 'bldg_lod0footprint_value', '', stGeometry, nil, '');
  //AddLayerFromTable('basic structures', 'building', 'buildings', 'basic buildings', '"building"', 'MultiPolygon', false, True, schema, 'bldg_building_import', 'gid', 'geom');
  AddLayerFromTable('basic structures', 'parking',   'parkings',  'basic parking lots', '"parking"',    'MultiPolygon', false, True, schema, '"green_import" where layer=''PARKING''', 'gid', 'geom', '', stGeometry, nil, '');
  AddLayerFromTable('basic structures', 'trees  5m', 'trees  5m', 'trees  5m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$5M''', 'gid', 'geom', '', stLocation, nil, '');
  AddLayerFromTable('basic structures', 'trees 10m', 'trees 10m', 'trees 10m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$10M''', 'gid', 'geom', '', stLocation, nil, '');
  AddLayerFromTable('basic structures', 'trees 15m', 'trees 15m', 'trees 15m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$15M''', 'gid', 'geom', '', stLocation, nil, '');
  AddLayerFromTable('basic structures', 'trees 20m', 'trees 20m', 'trees 20m',          '"tree"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''TREES$20M''', 'gid', 'geom', '', stLocation, nil, '');
  AddLayerFromTable('basic structures', 'grass',     'grass',     'grass areas',        '"grass area"', 'MultiPolygon', false, True, schema, '"green_import" where layer=''GRASS_AREA''', 'gid', 'geom', '', stGeometry, nil, '');
  AddLayerFromTable('basic structures', 'water',     'water',     'water areas',        '"water area"', 'MultiPolygon', false, True, schema, '"green_import" where layer=''WATER''', 'gid', 'geom', '', stGeometry, nil, '');
  AddLayerFromTable('basic structures', 'bushes',    'bushes',    'bushes',             '"bush"',       'MultiPolygon', false, True, schema, '"green_import" where layer=''BUSHES''', 'gid', 'geom', '', stGeometry, nil, '');

  // general, buildings
  setLength(entries, 1);
  with entries[0] do begin colors:=TGeoColors.Create($FFD9D9D9); minValue:=0; maxValue:=2; description:='Buildings'; end;
  palette := TDiscretePalette.Create('Buildings', entries, TGeoColors.Create(TAlphaColors.Red and not TAlphaColors.Alpha));
  legendJSON := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical); // todo: parameterize

  layer := AddLayerFromTable('General', 'Buildings',  'Buildings', 'Buildings', '"building"', 'MultiPolygon', false,
    false, schema, '"bldg_building"', 'attr_gml_id', 'bldg_lod0footprint_value', '1 as value', stGeometry, palette, legendJSON);
  // general, roads
  // Green, trees
  // Green, rest
  setLength(entries, 4);
  with entries[0] do begin colors:=TGeoColors.Create($FF92D050); minValue:=0; maxValue:=2; description:='Grass area'; end;
  with entries[1] do begin colors:=TGeoColors.Create($FFC5D9F1); minValue:=2; maxValue:=4; description:='Wates surfaces'; end;
  with entries[2] do begin colors:=TGeoColors.Create($FF92D050, $FFC00000); minValue:=4; maxValue:=6; description:='Green roofs'; end;
  with entries[3] do begin colors:=TGeoColors.Create($FF92D050, $FFF79646); minValue:=6; maxValue:=8; description:='Permeable surfaces'; end;
  palette := TDiscretePalette.Create('Green', entries, TGeoColors.Create(TAlphaColors.Red and not TAlphaColors.Alpha));
  legendJSON := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical); // todo: parameterize

  query := // id, geometry as svg, value
    '(select gid::text as id, ST_AsSVG(geom) as geometry, 1 as value from {case_id}."green_import" where layer=''GRASS_AREA'')'+
    'union'+
    '(select gid::text as id, ST_AsSVG(geom) as geometry, 3 as value from {case_id}."green_import" where layer=''WATER'')'+
    'union'+
    '(select attr_gml_id::text as id, ST_AsSVG(bldg_lod0footprint_value) as geometry, 5 as value from {case_id}."bldg_building")'+
    'union'+
    '(select gid::text as id, ST_AsSVG(geom) as geometry, 7 as value from {case_id}."green_import" where layer=''PARKING'')';

  layer := AddLayerFromQuery('Green', 'GreenRest',  'Green', 'Green', '"building,grass,water,parking"', 'MultiPolygon', false,
    false, schema, query, stGeometry, palette, legendJSON);

  // Mobility
  //Area used for parking: FFF79646
  //PArking spaces: outer: FFF79646 inner: FFFFFF99

  // Dimosim
  //Heating: FFFF0000
  //Cooling: FF8DB4E2

  // Energy efficiency improvement factor
  setLength(entries, 6);
  with entries[0] do begin colors:=TGeoColors.Create($FFFF0000); minValue:=0; maxValue:=10; description:='<10%'; end;
  with entries[1] do begin colors:=TGeoColors.Create($FFFF6600); minValue:=10; maxValue:=20; description:='10-20%'; end;
  with entries[2] do begin colors:=TGeoColors.Create($FFFFC000); minValue:=20; maxValue:=30; description:='20-30%'; end;
  with entries[3] do begin colors:=TGeoColors.Create($FFFFDD00); minValue:=30; maxValue:=40; description:='30-40%'; end;
  with entries[4] do begin colors:=TGeoColors.Create($FF92D050); minValue:=40; maxValue:=50; description:='40-50%'; end;
  with entries[5] do begin colors:=TGeoColors.Create($FF00B050); minValue:=50; maxValue:=100; description:='>50%'; end;
  palette := TDiscretePalette.Create('Energy efficiency improvement factor', entries, TGeoColors.Create(TAlphaColors.Red and not TAlphaColors.Alpha));
  legendJSON := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical); // todo: parameterize

  layer := AddLayerFromTable('Energy', 'BuildingEnergyEfficiency', 'Energy efficiency', 'Energy efficiency improvement factor', '"building"', 'MultiPolygon', false,
    false, schema, 'bldg_building join "'+schema+'".bldg_building_energylabel on attr_gml_id=id',
    'attr_gml_id', 'bldg_lod0footprint_value', 'kwh_m2_year', stGeometry, palette, legendJSON);
  *)
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

procedure TEcodistrictScenario.ReadObjectFromQuery(aLayer: TLayer);
var
  query: TFDQuery;
begin
  (project as TEcodistrictProject).PingDatabase('TEcodistrictScenario.ReadObjectFromQuery');
  query := TFDQuery.Create(nil);
  try
    query.Connection := project.dbConnection as TFDConnection;
    query.SQL.Text := aLayer.query;
    aLayer.ReadObjectsDBSVGPaths(query, Double.NaN);
  finally
    query.Free;
  end;
  Log.WriteLn(aLayer.elementID+': '+aLayer.name+', read objects (svg paths)', llNormal, 1);
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
        nearestObjectDistanceInMeters := Double.PositiveInfinity;
        for l in layers do
        begin
          o := l.findNearestObject(dist, aX, aY, nearestObjectDistanceInMeters);
          if assigned(o) and (nearestObjectDistanceInMeters<fProject.maxNearestObjectDistanceInMeters) then
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

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectedCategories, aSelectedIDs: TArray<string>): string;
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

{ TEcodistrictProject }

constructor TEcodistrictProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aDBConnection: TCustomConnection; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled,
  aMeasuresHistoryEnabled, aSimualtionControlEnabled, aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer);
begin
  inherited;
  fDIObjectProperties := nil; // TObjectList<TDIObjectProperty>.Create;
  fDMQueries := nil;
  fDIQueries := nil;
  fDIMeasuresHistory := nil;
  fTiler.onTilerStatus := handleTilerStatus;
end;

destructor TEcodistrictProject.Destroy;
begin
  FreeAndNil(fDIObjectProperties);
  FreeAndNil(fDMQueries);
  FreeAndNil(fDIQueries);
  FreeAndNil(fDIMeasuresHistory);
  inherited;
end;

function TEcodistrictProject.getMeasuresHistoryJSON: string;
var
  mhl: TDictionary<TGUID, TDIMeasureHistory>;
  mi: TPair<TGUID, TDIMeasureHistory>;
begin
  Result := inherited getMeasuresHistoryJSON;
  mhl := DIMeasuresHistory;
  TMonitor.Enter(mhl);
  try
    for mi in mhl do
    begin
      if Result<>''
      then Result := Result+',';
      Result := Result+'{'+''+'}'; // todo:
    end;
  finally
    TMOnitor.Exit(mhl);
  end;
end;

function TEcodistrictProject.getMeasuresJSON: string;
var
  schema: string;
begin
  schema := EcoDistrictSchemaId(projectID);
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
            'from '+schema+'.di_measures m '+
            'where m.id>=0 and m.category=j.category and m.typology=j.typology '+
            'order by id) as actions) '+
          'from '+schema+'.di_measures j '+
          'group by category, typology '+
          'having category=c.category '+
        ') as typologies '+
      ') as measures '+
      'from '+schema+'.di_measures c '+
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

function TEcodistrictProject.getDIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory>;
begin
  if not Assigned(fDIMeasuresHistory)
  then ReadDIMeasuresHistory();
  Result := fDIMeasuresHistory;
end;

function TEcodistrictProject.getDIObjectProperties: TObjectList<TDIObjectProperty>;
begin
  if not Assigned(fDIObjectProperties)
  then ReadDIObjectProperties();
  Result := fDIObjectProperties;
end;

function TEcodistrictProject.getDIQueries: TDictionary<string, TDIQuery>;
begin
  if not Assigned(fDIQueries)
  then ReadDIQueries();
  Result := fDIQueries;
end;

function TEcodistrictProject.getDMQueries: TDictionary<string, TDMQuery>;
begin
  if not Assigned(fDMQueries)
  then ReadDMQueries();
  Result := fDMQueries;
end;

procedure TEcodistrictProject.handleClientMessage(aJSONObject: TJSONObject);
var
  jsonPair: TJSONPair;
  jsonValue: TJSONObject;
  scenarioID: string;
  scenario: TScenario;
begin
  if isObject(aJSONObject, 'applyMeasures', jsonPair) then
  begin
    //jsonValue := jsonPair.JsonValue as TJSONObject;
    // todo:

  end
  else if isObject(aJSONObject, 'scenarioRefresh', jsonPair) then
  begin
    scenarioID := jsonPair.JsonValue.Value;
    if (scenarioID='') or (scenarioID='null') or (scenarioID='None')
    then scenarioID := EcodistrictBaseScenario;
    if scenarios.TryGetValue(scenarioID, scenario) then
    begin
      scenario.ReadBasicData;
      Log.WriteLn('refreshed scenario '+scenarioID);
    end
    else Log.WriteLn('scenario '+scenarioID+' not found to refresh', llWarning);
  end
  else ;
end;

function TEcodistrictProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

function TEcodistrictProject.PingDatabase(const aCaller: string): Boolean;
begin
  Result := (fDBConnection as TFDConnection).Ping;
  if not Result
  then Log.WriteLn(aCaller+': ping of database returned false', llError);
end;

procedure TEcodistrictProject.ReadBasicData;
var
//  sl: TLayer;
  schemaNames: TArray<string>;
  schemaName: string;
begin
  schemaNames := ReadSchemaNames();
  for schemaName in schemaNames
  do readScenario(schemaName);
end;

function TEcodistrictProject.ReadDIMeasuresHistory: Boolean;
var
  query: TFDQuery;
  mh: TDIMeasureHistory;
begin
  fDIObjectProperties.Free;
  fDIObjectProperties := TObjectList<TDIObjectProperty>.Create;
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT id, measure_cat, measure_id, object_ids, timeutc, variants '+
      'FROM '+EcoDistrictSchemaId(projectId)+'.di_measuresHistory';
    try
      query.Open();
      query.First();
      while not query.Eof do
      begin
        mh.id := TGUID.Create(query.Fields[0].AsString);
        mh.measure_cat := query.Fields[1].AsString;
        mh.measure_id := query.Fields[2].AsInteger;
        mh.object_ids := query.Fields[3].AsString;
        mh.timeutc := query.Fields[4].AsDateTime;
        mh.variants := query.Fields[5].AsString;
        fDIMeasuresHistory.Add(mh.id, mh);
        query.Next();
      end;
      Result := True;
    except
      on e: exception do
      begin
        log.WriteLn('exception in TEcodistrictProject.ReadDIMeasuresHistory: '+e.Message, llError);
        Result := False;
      end;
    end;
  finally
    query.Free;
  end;
end;

function TEcodistrictProject.ReadDIObjectProperties: Boolean;
var
  query: TFDQuery;
  op: TDIObjectProperty;
begin
  fDIObjectProperties.Free;
  fDIObjectProperties := TObjectList<TDIObjectProperty>.Create;
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT category, propertyName, propertyType, selection, fieldName, tablename, keyfieldname, editable '+
      'FROM '+EcoDistrictSchemaId(projectId)+'.di_objectproperties '+
      'ORDER BY category, propertyName';
    try
      query.Open();
      query.First();
      while not query.Eof do
      begin
        //query.Fields[0].
        op := TDIObjectProperty.Create;
        try
          op.category := query.Fields[0].AsString;
          op.propertyName := query.Fields[1].AsString;
          op.propertyType := query.Fields[2].AsString;
          op.selection := query.Fields[3].AsString;
          op.fieldName := query.Fields[4].AsString;
          op.tableName := query.Fields[5].AsString;
          op.keyFieldName := query.Fields[6].AsString;
          op.editable := query.Fields[7].AsBoolean;
        finally
          fDIObjectProperties.Add(op);
        end;
        query.Next();
      end;
      Result := True;
    except
      on e: exception do
      begin
        log.WriteLn('exception in TEcodistrictProject.ReadDIObjectProperties: '+e.Message, llError);
        Result := False;
      end;
    end;
  finally
    query.Free;
  end;
end;

function TEcodistrictProject.ReadDIQueries: Boolean;
var
  query: TFDQuery;
  DIQuery: TDIQuery;
begin
  fDIQueries.Free;
  fDIQueries := TDictionary<string, TDIQuery>.Create;
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT '+
        'id, domain, name, description, objecttypes, geometrytype, '+ // text
        'defaultload, basiclayer, '+ // integer
        'sql, tablename, idfieldname, geometryfieldname, datafieldname, '+ // text
        'layertype, '+ // integer
        'palettejson, legendjson '+ // text
			'FROM '+EcoDistrictSchemaId(projectID)+'.di_queries '+
      'WHERE defaultload >= 0'; // only load layers where default load >= 0 to make disabling layers easier
    try
      query.open();
      query.First;
      while not query.Eof do
      begin
        DIQuery.id := query.Fields[0].AsString;
        DIQuery.domain := query.Fields[1].AsString;
        DIQuery.name := query.Fields[2].AsString;
        DIQuery.description := query.Fields[3].AsString;
        DIQuery.objecttypes := query.Fields[4].AsString;
        DIQuery.geometrytype := query.Fields[5].AsString;
        DIQuery.defaultload := query.Fields[6].AsInteger; // int
        DIQuery.basiclayer := query.Fields[7].AsInteger; // int
        DIQuery.sql := query.Fields[8].AsString;
        DIQuery.tablename := query.Fields[9].AsString;
        DIQuery.idfieldname := query.Fields[10].AsString;
        DIQuery.geometryfieldname := query.Fields[11].AsString;
        DIQuery.datafieldname := query.Fields[12].AsString;
        DIQuery.layertype := query.Fields[13].AsInteger; // int
        DIQuery.palettejson := query.Fields[14].AsString;
        DIQuery.legendjson := query.Fields[15].AsString;
        fDIQueries.Add(DIQuery.id, DIQuery);
        query.Next;
      end;
      Result := True;
    except
      on e: exception do
      begin
        log.WriteLn('exception in TEcodistrictProject.ReadDIQueries: '+e.Message, llError);
        Result := False;
      end;
    end;
  finally
    query.Free;
  end;
end;

function TEcodistrictProject.ReadDMQueries: Boolean;
var
  query: TFDQuery;
  DMQuery: TDMQuery;
begin
  fDMQueries.Free;
  fDMQueries := TDictionary<string, TDMQuery>.Create;
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    // * will not pin order of fields !
    query.SQL.Text :=
      'SELECT object_id, returntype, request, query, module '+
			'FROM '+EcoDistrictSchemaId(projectID)+'.dm_queries';
    try
      query.open();
      query.First;
      while not query.Eof do
      begin
        DMQuery.module:=query.Fields[4].AsString;
        DMQuery.SQL:=query.Fields[3].AsString;
        DMQuery.ReturnType:=query.Fields[1].AsString;
        fDMQueries.Add(query.Fields[2].AsString,DMQuery);
        query.Next;
      end;
      Result := True;
    except
      on e: exception do
      begin
        log.WriteLn('exception in TEcodistrictProject.ReadDMQueries: '+e.Message, llError);
        Result := False;
      end;
    end;
  finally
    query.Free;
  end;
end;

{
procedure TEcodistrictProject.ReadObjects(aSender: TObject);
var
  query: TFDQuery;
begin
  PingDatabase('TEcodistrictProject.ReadObjects');

  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text := (aSender as TLayer).query; //+' limit 100'; // todo: remove limit!!
    (aSender as TLayer).ReadObjectsDBSVGPaths(query, Double.NaN);
    (aSender as TLayer).RegisterLayer;

  finally
    query.Free;
  end;
  Log.WriteLn((aSender as TLayer).elementID+': '+(aSender as TLayer).name+', read objects (svg paths)', llNormal, 1);
end;
}

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
  PingDatabase('TEcodistrictProject.ReadSchemaNames');

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

{ TEcodistrictModule }

constructor TEcodistrictModule.Create(aSessionModel: TSessionModel; aConnection: TConnection;
  const aConnectString, aTilerFQDN, aTilerStatusURL: string; aMaxNearestObjectDistanceInMeters: Integer);
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
  fMaxNearestObjectDistanceInMeters := aMaxNearestObjectDistanceInMeters;
  InitPG;
  fDBConnection := TFDConnection.Create(nil);
  SetPGConnection(fDBConnection as TFDConnection, fConnectString);
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

function TEcodistrictModule.forceReadOfDIMeasuresHistory(const aCaseId: string): Boolean;
var
  project: TProject;
begin
  if fProjects.TryGetValue(aCaseId, project)
  then Result := (project as TEcodistrictProject).ReadDIMeasuresHistory
  else Result := False;
end;

function TEcodistrictModule.forceReadOfDIObjectProperties(const aCaseId: string): Boolean;
var
  project: TProject;
begin
  if fProjects.TryGetValue(aCaseId, project)
  then Result := (project as TEcodistrictProject).ReadDIObjectProperties
  else Result := False;
end;

function TEcodistrictModule.forceReadOfDIQueries(const aCaseId: string): Boolean;
var
  project: TProject;
begin
  if fProjects.TryGetValue(aCaseId, project)
  then Result := (project as TEcodistrictProject).ReadDIQueries
  else Result := False;
end;

function TEcodistrictModule.forceReadOfDMQueries(const aCaseId: string): Boolean;
var
  project: TProject;
begin
  if fProjects.TryGetValue(aCaseId, project)
  then Result := (project as TEcodistrictProject).ReadDMQueries
  else Result := False;
end;

function TEcodistrictModule.GetOrAddCase(const aCaseId: string): TProject;
var
  dbConnection: TCustomConnection;
begin
  // load the case as project
  if not fProjects.TryGetValue(aCaseId, Result) then
  begin
    InitPG;
    dbConnection := TFDConnection.Create(nil);
    SetPGConnection(dbConnection as TFDConnection, fConnectString);
    Result := TEcodistrictProject.Create(fSessionModel, fSessionModel.Connection, aCaseID, '', fTilerFQDN, fTilerStatusURL, dbConnection, 0, True, True, True, False, True, fMaxNearestObjectDistanceInMeters);
    fSessionModel.Projects.Add(Result);

    fProjects.Add(aCaseId, Result);
  end;
  //else already loaded
end;

{
function TEcodistrictModule.getDIQueries(const aCaseId: string; var aQueries: TDictionary<string, TDIQuery>): boolean;
var
  project: TProject;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    aQueries := (project as TEcodistrictProject).DIQueries;
    Result := Assigned(aQueries);
  end
  else Result := False;
end;
}

function TEcodistrictModule.getDMQueries(const aCaseId: string; var aQueries: TDictionary<string, TDMQuery>): boolean;
var
  project: TProject;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    aQueries := (project as TEcodistrictProject).DMQueries;
    Result := Assigned(aQueries);
  end
  else Result := False;
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
  jsonResponse: TJSONObject;
  datafield: string;
  jsonDataResponse: TJSONObject;
  jsonList: TJSONObject;
  jsonArray: TJSONArray;
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
  _kpi_type: string;
  _gml_id: string;
  _kpi_value: double;
  _variantId: string;
  DataEvent: TEventEntry;
  _variantName: string;
  _variantDescription: string;
  _SQL: string;
  geojsonObjectAsString: string;
  queries: TDictionary<string, TDMQuery>;
  rowObject: TJSONObject;
  f: TField;
begin
  try
    if not (fDBConnection as TFDConnection).ping
    then Log.Writeln('TEcodistrictModule.HandleDataEvent: ping to database returned false', llError);

    // eventId hold the eventname where the results should be published.
    jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
    try
      jsonResponse := TJSONObject.Create;
      try
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
            try
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
                    queries := nil;
                    if getDMQueries(_caseId, queries) then
                    begin
                      _status := 'Success';
                      for datafield in queries.Keys do
                      begin
                        if queries.TryGetValue(datafield, dataquery) then
                        begin
                          if (dataquery.module.Contains(_moduleId)) then
                          begin
                            (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''');
                            try
                              query := TFDQuery.Create(nil);
                              try
                                Log.WriteLn(datafield, llNormal, 1);
                                query.Connection := fDBConnection as TFDConnection;
          //insert variables into query
                                _SQL:= dataquery.SQL.Replace('{case_id}', EcoDistrictSchemaId(_caseId));
                                //_SQL:= ReplaceStr(dataquery.SQL, '{case_id}', EcoDistrictSchemaId(_caseId)); // todo (HC): should this not be schema_id? or not being used at all in a query?
          //end variables insert
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
                                      jsonDataResponse.AddPair(datafield, TJSONNumber.Create(query.Fields[0].AsInteger));
                                    end;
                                  end
                                  else
                                  if dataquery.ReturnType='FLOAT' then
                                  begin
                                    if not query.Eof then
                                    begin
          //                            if dataresponse<>'' then dataresponse:=dataresponse+ ',';
          //                            dataresponse:=dataresponse + '"'+ datafield + '": "'+query.Fields[0].AsFloat.toString()+'"';
                                      jsonDataResponse.AddPair(datafield, TJSONNumber.Create(query.Fields[0].AsFloat));
                                    end;
                                  end
                                  else
                                  if dataquery.ReturnType='GEOJSON' then
                                  begin // geojson as string
          //we expect the query to return: a geojson object as text
                                    if not query.Eof then
                                    begin
                                      geojsonObjectAsString := query.Fields[0].AsString;
                                      jsonDataResponse.AddPair(datafield, TJSONObject.ParseJSONValue(geojsonObjectAsString));
                                    end;
                                  end
                                  else
                                  if dataquery.ReturnType='LIST' then // specific for citygml structure, see: TABLE
                                  begin




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
                                    finally
                                      jsonDataResponse.AddPair(datafield,jsonList);
                                    end;
                                  end
                                  else
                                  if dataquery.ReturnType='TABLE' then // specific for flat tables, see: LIST
                                  begin




                                        rowObject := TJSONObject.Create;
                                        try
                                          // todo: convert all fields to json object
                                          for f in query.fields do
                                          begin
                                            if not f.IsNull then
                                            begin
                                              case f.DataType of
                                                ftWideMemo,
                                                ftString,
                                                ftWideString: // todo: check widestring conversion..
                                                  rowObject.AddPair(f.FieldName, TJSONString.Create(f.AsString));
                                                ftInteger:
                                                  rowObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsInteger));
                                                ftLargeint:
                                                  rowObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsLargeInt));
                                                ftFloat,
                                                ftExtended,
                                                ftSingle:
                                                  rowObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsFloat));
                                              end;
                                            end
                                            else rowObject.AddPair(f.FieldName, TJSONNull.Create); // todo: check!
                                          end;
                                        finally
                                          jsonArray.Add(rowObject);
                                        end;
                                        query.Next;
                                      end;
                                    finally
                                      jsonDataResponse.AddPair(datafield,jsonArray);
                                    end;
                                  end;
                                finally
                                  query.Close;
                                end;
                              finally
                                query.Free;
                              end;
                            finally
                              (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''public''');
                            end;
                          end;
                        end;
                      end
                    end
                    else _status := 'failed - case not loaded';
                  end;
                end
                else _status := 'failed - no module id';
              end
              else _status := 'failed - no case id';
            finally
              jsonResponse.AddPair('data',jsonDataResponse);
            end;
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(jsonResponse.ToString);
            Log.WriteLn(_status);
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
                    if Assigned(_kpiValueList) then
                    begin
                      jsonIterator :=_kpiValueList.GetEnumerator;
                      while jsonIterator.MoveNext do
                      begin
                        jsonKpi:=jsonIterator.Current;
                        _kpi_type:=jsonKpi.GetValue<string>('type', 'None');
                        _gml_id:=jsonKpi.GetValue<string>('gml_id', 'None');
                        _kpi_value:=jsonKpi.GetValue<double>('kpiValue', 0);
                        (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''');
                        try
    // SQL delete from kpi_results where kpi_type=... and gml_id= and kpi_id =
                          (fDBConnection as TFDConnection).ExecSQL('DELETE FROM kpi_results WHERE kpi_type='''+_kpi_type+''' AND gml_id='''+_gml_id+''' AND kpi_id='''+_kpiId+''';');
    // SQL insert into kpi_results (kpi_type, kpi_id, gml_id, kpi_value) values ()
                          (fDBConnection as TFDConnection).ExecSQL('INSERT INTO kpi_results (kpi_type, kpi_id, gml_id, kpi_value) VALUES ('''+_kpi_type+''', '''+_kpiId+''', '''+_gml_id+''','+_kpi_value.ToString()+');');
                        finally
                          (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''public''');
                          _status := 'Success - data added to the database';
                        end;
                      end;
                    end;
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
                    (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''');
                    try
                      jsonArray:=TJSONArray.Create; // will be owned by jsonResponse
                      try
                        query := TFDQuery.Create(nil);
                        try
                          query.Connection := fDBConnection as TFDConnection;
                          query.SQL.Text :=
                            'SELECT id, kpi_id, gml_id, kpi_value, kpi_type '+
                            'FROM kpi_results '+
                            'WHERE kpi_id='''+_kpiId+'''';
                          query.Open();
                          try
                            query.First;
                            while not query.Eof do
                            begin
                              jsonList:=TJSONObject.Create; // will be owned by the JSON Array, so do not free!
                              try
                                jsonList.AddPair('kpi_id', query.FieldByName('kpi_id').AsString);
                                jsonList.AddPair('gml_id', query.FieldByName('gml_id').AsString);
                                jsonList.AddPair('kpi_type', query.FieldByName('kpi_type').AsString);
                                jsonList.AddPair('kpi_value', TJSONNumber.Create(StrToFloat(query.FieldByName('kpi_value').AsString, dotFormat)));
                              finally
                                jsonArray.Add(jsonList);
                              end;
                            end;
                          finally
                            query.Close;
                          end;
                        finally
                          query.Free;
                        end;
                      finally
                        jsonResponse.AddPair('kpiValue', jsonArray);
                      end;
                    finally
                      (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''public''');
                      _status := 'Success';
                    end;
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
//          if _method='getGeojson' then
//          begin
//
//          end
//          else
          if _method='readDMQueries' then
          begin
            if forceReadOfDMQueries(_caseId)
            then _status := 'Success - read queries'
            else _status := 'failed - case not loaded to read dm-queries for or no queries found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else
          if _method='readDIQueries' then
          begin
            if forceReadOfDIQueries(_caseId)
            then _status := 'Success - read queries'
            else _status := 'failed - case not loaded to read di-queries for or no queries found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else
          if _method='readDIObjectProperties' then
          begin
            if forceReadOfDIObjectProperties(_caseId)
            then _status := 'Success - read object properties'
            else _status := 'failed - case not loaded to read di-object-properties for or no properties found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else
          if _method='readDIMeasuresHistory' then
          begin
            if forceReadOfDIMeasuresHistory(_caseId)
            then _status := 'Success - read measures history'
            else _status := 'failed - case not loaded to read di-measures-history for or no history found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else
          if _method='refresh' then
          begin
            if not ((_caseId = 'null') or (_caseId = '')) then
            begin
              if not SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
              begin
                _status := 'failed - no schema found for case and variant';
              end
              else
              begin
                // we have a valid case id
                if HandleModuleScenarioRefresh(_caseId, _variantId)
                then _status := 'Success - scenario refreshed'
                else _status := 'failed - could not refresh scenario';
              end;
            end
            else
            begin
              _status := 'failed - no case id specified';
            end;
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else
          begin
            Log.WriteLn('HandleDataEvent: unknown type/method: '+_type+', method: '+_method,llWarning);
            // todo: just ignore unknown requests ?
    //        _status := 'failed - unknown request';
    //        jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
    //        DataEvent.signalString(JSONresponse.ToString);
          end;
        end;
      finally
        FreeAndNil(jsonResponse);
      end;
    finally
      FreeAndNil(jsonObject);
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

  function DatabaseConnectionStatus: string;
  begin
    if fDBConnection.Connected and (fDBConnection as TFDConnection).Ping
    then Result := 'Database: connected and alive.'
    else Result := 'Database: NOT connected.';
  end;

  function WebClientsConnected: string;
  var
    cc: Integer;
    ipp: TPair<string, TProject>;
  begin
    cc  := 0;
    for ipp in fProjects
    do cc := cc+ipp.value.clients.Count;
    Result := 'Projects: '+fProjects.Count.ToString+', web clients: '+cc.toString;
  end;

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
            '"description": "Design and view layer based information and apply measures. '+DatabaseConnectionStatus+' '+WebClientsConnected+'",'+
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

function TEcodistrictModule.HandleModuleScenarioRefresh(const aCaseId, aVariantID: string): Boolean;
var
  project: TProject;
  scenarioID: string;
  scenario: TScenario;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    if (aVariantId='') or (avariantId='null') or (aVariantId='None')
    then scenarioID := EcodistrictBaseScenario
    else scenarioID := aVariantId;
    TMonitor.Enter(project.scenarios);
    try
      if project.scenarios.TryGetValue(scenarioID, scenario) then
      begin
        scenario.ReadBasicData;
        Result := True;
      end
      else Result := False;
    finally
      TMonitor.Exit(project.scenarios);
    end;
  end
  else Result := False;
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



end.