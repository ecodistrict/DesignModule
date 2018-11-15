unit PublishServerEcodistrict;

{
  todo:
    base scenario uses internally Scenario.ID=ProjectID!
    in dictionary EcodistrictBaseScenario is used
    -> mismatch scenario.ID and dictionary id within project
      -> solution: remove EcodistrictBaseScenario and always use project id?

    remove case, project , scenario not implemented yet

    measure processing incomplete
}

interface

uses
  StdIni,
  Logger,

  //GisDefs,
  GisLicense, // add this to make the program work outside Delphi IDE
  GisCsSystems, GisLayerSHP, GisLayerVector,
  GisFunctions,
  GisUtils, GisTypes,
  GisCSProjections,

  imb4,
  WorldDataCode,
  WorldLegends,
  WorldTilerConsts,
  WorldJSON,
  Data.DB,
  FireDAC.Comp.Client,

  TimerPool,

  TilerControl,

  PublishServerDB,
  PublishServerLib,

  Zipper,

  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  System.Classes,
  Winapi.Windows,
  System.SysUtils;

const
  // ecodistrict
  EcodistrictCasePrefix = 'trout_'; // Nicolas's prefix to avoid schema names starting with numbers
  EcodistrictBaseScenario = 'undefined';
  CaseVariantManagementReturnEventName = 'data-to-dashboard';

  EcodistrictConnectStringSwitchName = 'EcodistrictConnectString';

  DisabledKPIsSection = 'DisabledKPIs';

  DefaultFieldNameGeometry = 'shape';

  // up/download file types
  udftLayer = 'layer';
  udftShape = 'shape';
  udftText = 'text';

type
  TFileInfoRecord = class
  constructor Create(const aFieldName: string; aFieldType: TFieldType; aIsIndexField, aIsGeometryField: Boolean);
  public
    fieldName: string;
    fieldType: TFieldType;
    isIndexField: Boolean;
    isGeometryField: Boolean;
    sqlFieldValue: string;
    deleteFlag: Boolean;
    degreeConversion: Boolean;
  protected
    function getTypeName: string;
  public
    property TypeName: string read getTypeName;
  public
    class function GISFieldType2DBFieldType(aGISFieldType: TGIS_FieldType): TFieldType;
  end;

  TEcodistrictLayer = class(TLayer)
  constructor Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string;
    aDefaultLoad: Boolean; const aObjectTypes, aGeometryType: string;
    aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean=False);
  protected
    fLayerType: Integer;
  public
    procedure RegisterLayer; override;
    procedure RegisterSlice; override;
    function SliceType: Integer; override;
  end;

  TEcodistrictScenario = class(TScenario)
  constructor Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
  destructor Destroy; override;
  private
    fRefreshTimer: TTimer;
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
    //function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    //function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; const aSelectedIDs: TArray<string>): string; overload; override;

    function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; override;
  public
    function ScenarioSchemaName: string;
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
    function toJSON(const aValue: string): string;
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

  TDIMeasure = record
    id: string; //cat||id as id, application as action, objecttype as objecttypes, benefits as description
    category: string;
    measure: string;
    action: string;
    objecttypes: string;
    description: string;
    query: string;
  end;

  TDIMeasureHistory = record
    id: TGUID;
    measure: string; // combined cat||id
    object_ids: string;
    timeutc: TDateTime;
    variants: string;
    categories: string;
  end;

  TEcodistrictKPI = class
  constructor Create(const aId, aName: string; aSufficient, aExcellent: Double);
  private
    fId: string;
    fName: string;
    fBad: Double;
    fSufficient: Double;
    fExcellent: Double;
  public
    property id: string read fId;
    property name: string read fName;
    property bad: Double read fBad;
    property sufficient: Double read fSufficient;
    property excellent: Double read fExcellent;
  end;

  TEcodistrictProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aAddBasicLayers: Boolean;
    aMaxNearestObjectDistanceInMeters: Integer;
    aKPIList: TObjectList<TEcodistrictKPI>);
  destructor Destroy; override;
  private
    function getDMQueries: TDictionary<string, TDMQuery>;
    function getDIQueries: TDictionary<string, TDIQuery>;
    function getDIObjectProperties: TObjectList<TDIObjectProperty>;
    function getDIMeasures: TDictionary<string, TDIMeasure>;
    function getDIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory>;
  protected
    fDIObjectProperties: TObjectList<TDIObjectProperty>;
    fDMQueries: TDictionary<string, TDMQuery>;
    fDIQueries: TDictionary<string, TDIQuery>;
    fDIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory>;
    fKpiList: TObjectDictionary<string, TEcodistrictKPI>;
    fDIMeasures: TDictionary<string, TDIMeasure>;

    function getMeasuresJSON: string; override;
    function getMeasuresHistoryJSON: string; override;
    function getQueryDialogDataJSON: string; override;
    function ReadSchemaNames: TArray<string>;
    function handleTilerStatus(aTiler: TTiler): string;

    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
    procedure handleTypedClientMessage(aClient: TClient; const aMessageType: string; var aJSONObject: TJSONObject); override;


  public
    //
    property kpiList: TObjectDictionary<string, TEcodistrictKPI> read fKpiList;
    procedure UpdateKPIList(aKPIList: TObjectList<TEcodistrictKPI>);
    // on demand load of items
    property DMQueries: TDictionary<string, TDMQuery> read getDMQueries;
    property DIQueries: TDictionary<string, TDIQuery> read getDIQueries;
    property DIObjectProperties: TObjectList<TDIObjectProperty> read getDIObjectProperties;
    property DIMeasures: TDictionary<string, TDIMeasure> read getDIMeasures;
    property DIMeasuresHistory: TDictionary<TGUID, TDIMeasureHistory> read getDIMeasuresHistory;
    // (re-)read items
    function ReadDMQueries: Boolean;
    function ReadDIQueries: Boolean;
    function ReadDIObjectProperties: Boolean;
    function ReadDIMeasures: Boolean;
    function ReadDIMeasuresHistory: Boolean;

    procedure AddDIMeasureHistory(aMeasureHistory: TDIMeasureHistory);
    procedure AddDownloadableTableNames(const aID: string);
    procedure GenerateDownloadableFile(aProject: TProject; aClient: TClient; const aTableName, aFileType: string);
    procedure handleFileUpload(aClient: TClient; const aFileInfo: TClientFileUploadInfo); override;

    function ProjectSchemaName: string;

    function ReadScenario(const aID: string): TScenario; override;
    procedure ReadBasicData(); override;
    function PingDatabase(const aCaller: string): Boolean;
  end;

  TEcodistrictModule = class
  constructor Create(
    aSessionModel: TSessionModel; aConnection: TConnection;
    const aConnectString, aTilerFQDN, aTilerStatusURL: string;
    aMaxNearestObjectDistanceInMeters: Integer;
    aDoNotListenToDataEvents: Boolean=False;
    aDoNotListenToModuleEvents: Boolean=False;
    aDoNotListenToCaseVariantManagementEvents: Boolean=False);
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
    function forceReadOfDIMeasures(const aCaseId: string): Boolean;

    function GetOrAddCase(const aCaseId: string; aKPIList: TObjectList<TEcodistrictKPI>): TProject;
    procedure HandleModuleCaseDelete(const aCaseId: string);
    procedure HandleModuleVariant(const aCaseId, aVariantID, aVariantName, aVariantDescription: string);
    procedure HandleModuleVariantDelete(const aCaseId, aVariantId: string);

    procedure HandleModuleEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleDataEvent(aEventEntry: TEventEntry; const aString: string);
    procedure HandleCaseVariantManagentEvent(aEventEntry: TEventEntry; const aString: string);

    function HandleModuleScenarioRefresh(const aCaseId, aVariantId: string): Boolean;

    // helpers for status
    function getNumberOfBuildings(const aSchemaName: string): Integer;
  public
    function HandleModuleCase(const aCaseId, aCaseTitle,  aCaseDescription: string; const aMapView: TMapView; aKPIList: TObjectList<TEcodistrictKPI>): TProject;
  end;


function EcoDistrictSchemaId(const aCaseId: string; const aVariantId: string=''): string;

implementation

function GetTempDirectory: String;
var
  tempBase0: array [0..MAX_PATH] of Char;
begin
  if GetTempPath(SizeOf(tempBase0) div SizeOf(Char), tempBase0) <> 0 then
  begin
    Result := ExcludeTrailingPathDelimiter(tempBase0);
    if CompareText(ExtractFileName(Result), 'temp') <> 0
    then Result := Result + '\TEMP';
  end
  else Result := '';
end;

function CleanupDirectory(const aDirectoryPath: string): Boolean;
var
  F: TSearchRec;
begin
  Result := True;
  if FindFirst(aDirectoryPath+'\*.*', faAnyFile, F)=0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory)=0 then
        begin
          if not Winapi.Windows.DeleteFile(PChar(aDirectoryPath+'\'+F.Name))
          then Result := False;
        end
        else
        begin
          if (F.Name<>'.') and (F.Name<>'..') then
          begin
            if not CleanupDirectory(aDirectoryPath+'\'+F.Name)
            then Result := False;
          end;
        end;
      until FindNext(F)<>0;
    finally
      FindClose(F);
    end;
    if Result
    then Result := RemoveDirectory(PChar(aDirectoryPath));
  end;
end;

function EcoDistrictSchemaId(const aCaseId: string; const aVariantId: string=''): string;
begin
  if (aVariantId='') or (aVariantId='null') or (aVariantId='None')
  then Result := EcoDistrictCasePrefix + aCaseId
  else Result := EcoDistrictCasePrefix + aCaseId + '_' + aVariantId;
end;

function CheckSQLValue(const aValue: string): string;
begin
  // remove escape quotes in values used in sql statements
  Result := aValue.Replace('''', '"')
end;

function IsSurroundedByQuotes(const s: string; aQuoteChar: char): Boolean; overload;
begin
  if length(s)>=2
  then Result := ((s[1]=aQuoteChar) and (s[s.Length]=aQuoteChar))
  else Result := False;
end;

function IsSurroundedByQuotes(const s: string; aQuoteChars: array of char): Boolean; overload;
var
  qc: Char;
begin
  for qc in aQuoteChars do
  begin
    if IsSurroundedByQuotes(s, qc)
    then exit(true);
  end;
  exit(false);
end;

function SurroundWithQuotes(const s: string; aQuoteChar: char): string;
begin
  if not IsSurroundedByQuotes(s, aQuoteChar)
  then result := aQuoteChar+s+aQuoteChar
  else result := s;
end;

function UnQuote(const s: string; aQuoteChars: array of char; var aUsedQuoteChar: Char): string;
var
  qc: Char;
begin
  for qc in aQuoteChars do
  begin
    if IsSurroundedByQuotes(s, qc) then
    begin
      aUsedQuoteChar := qc;
      exit(s.Substring(1, s.Length-2));
    end;
  end;
  aUsedQuoteChar := #0;
  exit(s);
end;

function LimitChars(const s: string; const aLimitChars: string; aReplaceChar: Char; out aLimited: Boolean): string;
var
  i: Integer;
begin
  aLimited := False;
  Result := s;
  for i := 1 to result.Length do
  begin
    if aLimitChars.IndexOf(result[i])<0 then
    begin
      result[i] := aReplaceChar;
      aLimited := True;
    end;
  end;
end;

const
  ValidSQLNameFirstChars =
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  ValidSQLNameSubsequentChars =
    ValidSQLNameFirstChars+
    '01234567890';
  ValidSQLQuotedNameChars =
    ValidSQLNameSubsequentChars+
    '!#()-.:;[]{}~�ǀ����������������������������������';

  ValidSQLValueChars =
    ValidSQLNameSubsequentChars+
    '!#$%&()*+,-.:;<=>[]^{}~�ǀ����������������������������������';

function SafeSQLFieldName(const aFieldName: string): string;
var
  qc: Char;
  limited: Boolean;
begin
  if aFieldName.Length>0 then
  begin
    if IsSurroundedByQuotes(aFieldName, ['''', '"']) then
    begin
      Result := LimitChars(UnQuote(aFieldName, ['"',''''], qc), ValidSQLQuotedNameChars, '_', limited);
      Result := qc+Result+qc; // put quotes around it again
    end
    else
    begin
      if ValidSQLNameFirstChars.IndexOf(aFieldName[1])>=0
      then Result := LimitChars(aFieldName, ValidSQLNameSubsequentChars, '_', limited)
      else Result := '_'+LimitChars(aFieldName.Substring(1), ValidSQLNameSubsequentChars, '_', limited);
    end;
  end
  else
  begin
    Result := '_';
    limited := True;
  end;
  if limited
  then Log.WriteLn('escaped SQL field name '+aFieldName+' -> '+Result, llWarning);
end;

function SafeSQLValue(const aValue: string; aMaybeSurroundedByQuotes: Boolean): string;
var
  qc: Char;
  limited: Boolean;
begin
  if aMaybeSurroundedByQuotes and IsSurroundedByQuotes(aValue, ['"', '''']) then
  begin
    Result := SafeSQLValue(UnQuote(aValue, ['"',''''], qc), false);
    Result := qc+Result+qc; // put quotes around it again
  end
  else
  begin
    Result := LimitChars(aValue, ValidSQLValueChars, '_', limited);
    if limited
    then Log.WriteLn('escaped SQL value '+aValue+' -> '+Result, llWarning);
  end;
end;

function FieldTypeName2Type(const aFieldTypeName: string): TFieldType;
// converts from pg and upload/download types
// aFieldTypeName should be in lower case
begin
  if aFieldTypeName='text'
  then Result := ftString
  else if aFieldTypeName='string' // added, not pg
  then Result := ftString
  else if aFieldTypeName='integer'
  then Result := ftInteger
  else if aFieldTypeName='int' // added, not pg
  then Result := ftInteger
  else if aFieldTypeName='double precision'
  then Result := ftFloat
  else if aFieldTypeName='double' // added, not pg
  then Result := ftFloat
  else if aFieldTypeName='float' // added, not pg
  then Result := ftFloat
  else if aFieldTypeName='boolean'
  then Result := ftBoolean
  else if aFieldTypeName='bool' // added, not pg
  then Result := ftBoolean
  //
  else if aFieldTypeName='uuid'
  then Result := ftGuid
  else if aFieldTypeName='guid'
  then Result := ftGuid
  else if aFieldTypeName='money'
  then Result := ftCurrency
  else if aFieldTypeName='timestamp without time zone'
  then Result := ftTimeStamp
  else if aFieldTypeName='timestamp' // added, not pg
  then Result := ftTimeStamp
  else if aFieldTypeName='user-defined'
  then Result := ftObject
  else if aFieldTypeName='public.geometry' // added, pg but only for setting
  then Result := ftObject
  else if aFieldTypeName='geometry' // added, not pg
  then Result := ftObject
  else Result := TFieldType(StrToIntDef(aFieldTypeName, Ord(ftUnknown)));
end;

function PGFieldType2TypeName(aFieldType: TFieldType; aIsGeometryFieldType: Boolean): string;
// converts to pg types
begin
  if aIsGeometryFieldType then
  begin
    Result := 'public.geometry';
  end
  else
  begin
    case aFieldType of
      ftWideMemo,
      ftString,
      ftWideString:  Result := 'text';
      ftInteger,
      ftLargeint:    Result := 'integer';
      ftFloat,
      ftExtended,
      ftSingle:      Result := 'double precision'; // diff
      ftBoolean:     Result := 'boolean';
      //
      ftGuid:        Result := 'uuid';
      ftCurrency:    Result := 'money';
      ftTimeStamp:   Result := 'timestamp without time zone'; // diff
    else
                     Result := 'public.geometry';
    end;
  end;
end;

function DownloadType2TypeName(aFieldType: TFieldType; aIsGeometryFieldType: Boolean): string;
// converts to upload/download types
begin
 if aIsGeometryFieldType then
  begin
    Result := 'geometry';
  end
  else
  begin
    case aFieldType of
      ftWideMemo,
      ftString,
      ftWideString:  Result := 'text';
      ftInteger,
      ftLargeint:    Result := 'integer';
      ftFloat,
      ftExtended,
      ftSingle:      Result := 'float'; // diff
      ftBoolean:     Result := 'boolean';
      //
      ftGuid:        Result := 'uuid';
      ftCurrency:    Result := 'money';
      ftTimeStamp:   Result := 'timestamp'; // diff
    else
                     Result := Ord(aFieldType).toString;
    end;
  end;
end;


{ TFileInfoRecord }

constructor TFileInfoRecord.Create(const aFieldName: string; aFieldType: TFieldType; aIsIndexField, aIsGeometryField: Boolean);
begin
  fieldName := aFieldName;
  fieldType := aFieldType;
  isIndexField := aIsIndexField;
  isGeometryField := aIsGeometryField;
  sqlFieldValue := '';
  deleteFlag := False;
  degreeConversion := False;
end;

function TFileInfoRecord.getTypeName: string;
begin
  Result := PGFieldType2TypeName(fieldType, isGeometryField);
end;

class function TFileInfoRecord.GISFieldType2DBFieldType(aGISFieldType:TGIS_FieldType): TFieldType;
begin
  case aGISFieldType of
    TGIS_FieldType.String:
      Result := ftString;
    TGIS_FieldType.Number: // int or float
      Result := ftFloat; // ftInteger;
    TGIS_FieldType.Float:
      Result := ftFloat;
    TGIS_FieldType.Boolean:
      Result := ftBoolean;
    TGIS_FieldType.Date:
      Result := ftDateTime;
  else
    Result := ftUnknown;
  end;
end;


{ TEcodistrictLayer }

constructor TEcodistrictLayer.Create(aScenario: TScenario; const aDomain, aID, aName, aDescription: string; aDefaultLoad: Boolean;
  const aObjectTypes, aGeometryType: string; aLayerType: Integer; aPalette: TWDPalette; const aLegendJSON: string; aBasicLayer: Boolean);
begin
  fLayerType := aLayerType;
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, ltTile, True, Double.NaN, aBasicLayer, 0.8, 'default', aLegendJSON, '', aPalette, 5, 7);
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
  tilerLayer.signalSliceAction(tsaClearSlice); //force clear of our current slice
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
    Result.query := PGSVGPathsQuery(aSchema+'.'+aTableName.Replace('{case_id}', aSchema), aIDFieldName, aGeometryFieldName, aDataFieldName);
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

constructor TEcodistrictScenario.Create(aProject: TProject; const aID, aName, aDescription: string; aAddbasicLayers: Boolean; aMapView: TMapView);
begin
  inherited Create(aProject, aID, aName, aDescription, aAddbasicLayers, aMapView);
  fRefreshTimer := aProject.Timers.CreateInactiveTimer(
    procedure(aTimer: TTimer; aTime: THighResTicks)
    begin
      Log.WriteLn('Sending refresh to all clients on scenario');
      Self.forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          Self.project.SendDomains(aClient, 'updatedomains');
        end);
    end);
end;

destructor TEcodistrictScenario.Destroy;
begin
  if Assigned(fRefreshTimer) then
  begin
    fRefreshTimer.Cancel;
    fRefreshTimer := nil;
  end;
  inherited;
end;

const
  ecoKPIBadColor = $FFCF051D; // bad: red
  ecoKPISufficiantColor = $FFFCED0E; // suffient: orange/yellow
  ecoKPIExcellentColor = $FF00862F; // excellent: green
  ecoKPINoDataColor = $00000000; // opaque

procedure TEcodistrictScenario.ReadBasicData;
var
  scenarioSchema: string;
  palette: TWDPalette;
  legendJSON: string;
  iqp: TPair<string, TDIQuery>;
  jsonPalette: TJSONValue;
  layer: TLayerBase;
  ikpip: TPair<string, TEcodistrictKpi>;
begin
  // read ecodistrict data
  scenarioSchema := ScenarioSchemaName;
  // data table names
  project.DownloadableFiles.Clear;
  (project as TEcodistrictProject).AddDownloadableTableNames(scenarioSchema);
  forEachSubscriber<TClient>(
    procedure(aClient: TClient)
    begin
      aClient.SendClearDownloadableFile();
      aClient.SendAddDownloadableFiles(project.DownloadableFiles);
    end);

  // build layers from di_queries
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
            else legendJSON := BuildDiscreteLegendJSON(palette as TDiscretePalette, TLegendFormat.lfVertical); // todo: parameterize (from palette json?)
          end;
        wdkPaletteRamp:
          begin
            palette := TRampPalette.CreateFromJSON(iqp.Value.palettejson);
            if iqp.Value.legendjson<>''
            then legendJSON := iqp.Value.legendjson
            else legendJSON := BuildRamplLegendJSON(palette as TRampPalette{, ..}); // todo: parameterize (from palette json?)
          end;
      else
        palette := JSON2PaletteAndLegend(jsonPalette as TJSONObject, legendJSON);
        if iqp.Value.legendjson<>''
        then legendJSON := iqp.Value.legendjson;
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
      (layer as TEcodistrictLayer).fPalette.Free;
      (layer as TEcodistrictLayer).fPalette := palette;
      layer.legendJSON := legendJSON;
      (layer as TEcodistrictLayer).RegisterSlice;
      ReadObjectFromQuery(layer as TEcodistrictLayer);
      // assume that on update the registration on the tiler already succeeded so we can just start signaling objects
      (layer as TEcodistrictLayer).signalObjects(nil);
    end
    else
    begin
      if iqp.Value.tablename<>'' then
      begin
        AddLayerFromTable(
          iqp.Value.domain, iqp.key,  iqp.Value.name, iqp.Value.description, iqp.Value.objecttypes, iqp.Value.geometrytype,
          iqp.Value.defaultload=1, iqp.Value.basiclayer=1,
          scenarioSchema, iqp.Value.tablename, iqp.Value.idfieldname, iqp.Value.geometryfieldname, iqp.Value.datafieldname,
          iqp.Value.layertype, palette, legendJSON);
      end
      else
      begin
        AddLayerFromQuery(
          iqp.Value.domain, iqp.key,  iqp.Value.name, iqp.Value.description, iqp.Value.objecttypes, iqp.Value.geometrytype,
          iqp.Value.defaultload=1, iqp.Value.basiclayer=1,
          scenarioSchema, iqp.Value.sql,
          iqp.Value.layertype, palette, legendJSON);
      end;
    end;
  end;
  // build layers out of kpi entries
  try
    Log.WriteLn('TEcodistrictScenario.ReadBasicData kpiList: '+(project as TEcodistrictProject).kpiList.Count.ToString);
    // add layers for kpi's defined on project
    for ikpip in (project as TEcodistrictProject).kpiList do
    begin
      // bad = same ammount lower/higher then excelent-sufficient
      if not StandardIni.ValueExists(DisabledKPIsSection, ikpip.key) then
      begin
        Log.WriteLn('eco kpi layer '+ikpip.key+': bad '+ikpip.Value.bad.ToString+', sufficient '+ikpip.Value.sufficient.tostring+', '+'excellent '+ikpip.Value.excellent.ToString, llNormal, 1);
        if ikpip.Value.sufficient<ikpip.Value.excellent then
        begin
          palette := TRampPalette.Create(ikpip.Value.name, [
              TRampPaletteEntry.Create(ecoKPIBadColor, ikpip.Value.bad, 'bad'),
              TRampPaletteEntry.Create(ecoKPISufficiantColor, ikpip.Value.sufficient, 'sufficient'),
              TRampPaletteEntry.Create(ecoKPIExcellentColor, ikpip.Value.excellent, 'excellent')],
            ecoKPIBadColor,
            ecoKPINoDataColor,
            ecoKPIExcellentColor);
          legendJSON := BuildRamplLegendJSON(palette as TRampPalette);
        end
        else
        begin
          palette := TRampPalette.Create(ikpip.Value.name, [
              TRampPaletteEntry.Create(ecoKPIExcellentColor, ikpip.Value.excellent, 'excellent'),
              TRampPaletteEntry.Create(ecoKPISufficiantColor, ikpip.Value.sufficient, 'sufficient'),
              TRampPaletteEntry.Create(ecoKPIBadColor, ikpip.Value.bad, 'bad')
              ],
            ecoKPIExcellentColor,
            ecoKPINoDataColor,
            ecoKPIBadColor);
          legendJSON := BuildRamplLegendJSON(palette as TRampPalette); // todo: check: reverse parameter was true but never used in BuildRamplLegendJSON
        end;
        if Layers.TryGetValue(ikpip.key, layer) then
        begin
          // todo: check: does this work?
          (layer as TEcodistrictLayer).fPalette.Free;
          (layer as TEcodistrictLayer).fPalette := palette;
          (layer as TEcodistrictLayer).description := ikpip.Value.name+' (bad '+ikpip.Value.bad.ToString+', sufficient '+ikpip.Value.sufficient.tostring+', '+'excellent '+ikpip.Value.excellent.ToString+')';
          layer.legendJSON := legendJSON;
          (layer as TEcodistrictLayer).RegisterSlice;
          ReadObjectFromQuery(layer as TEcodistrictLayer);
          // assume that on update the registration on the tiler already succeeded so we can just start signaling objects
          // todo: maybe check if already registered on tiler?
          (layer as TEcodistrictLayer).signalObjects(nil);
        end
        else
        begin
          // todo: assuming for now always building
          // todo: table ie type can be derived from kpi_results table field: kpi_type..
          AddLayerFromQuery(
            'KPI', ikpip.Value.id, ikpip.Value.name, ikpip.Value.name+' (bad '+ikpip.Value.bad.ToString+', sufficient '+ikpip.Value.sufficient.tostring+', '+'excellent '+ikpip.Value.excellent.ToString+')', '"building"', 'MultiPolygon',
            False, False,
            scenarioSchema,
            'SELECT building_id as id, ST_AsSVG(lod0footprint) as geometry, kpi_value as value '+
            'FROM {case_id}.building join {case_id}.kpi_results on {case_id}.building.building_id={case_id}.kpi_results.gml_id '+
            'WHERE kpi_id='''+ikpip.Value.id+'''',
            stGeometry, palette, legendJSON);
        end;
      end
      else Log.WriteLn('ignoring disabled kpi '+ikpip.key, llWarning);
    end;
  except
    on E: Exception
    do Log.WriteLn('Exception in TEcodistrictScenario.ReadBasicData kpiList: '+e.Message, llError);
  end;
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


//select from query
function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string;
var
  layers: TList<TLayerBase>;
  q: TJSONValue;
  field: TJSONObject;
  fieldName: string;
  operStr: string;
  valueStr: string;
  oper: Integer;
  v: Double;
  sql: string;
  layer: TLayerBase;
  scenarioSchema: string;
  query: TFDQuery;
  objectID: TWDID;
  layerObject: TLayerObjectWithID;
  objectsGeoJSON: string;
begin
  Result := '';
  layers := TList<TLayerBase>.Create;
  try
    scenarioSchema := ScenarioSchemaName;

    if selectLayersOnCategories(aSelectCategories, layers) then
    begin
      if aMode='+' then
      begin
        // only use first layer (only 1 type of object allowed..)
        // todo: warning if more then 1 layer?
        // never gets called for now..
      end
      else
      begin
        // select objects in layer that match first
        for q in aJSONQuery do
        begin
          field := q as TJSONObject;
          if field.TryGetValue<string>('field', fieldName) and
             field.TryGetValue<string>('operator', operStr) and
             field.TryGetValue<string>('value', valueStr) then
          begin
            fieldName := SafeSQLFieldName(fieldName);
            oper := '|<|<=|=|<>|>|>=|in|IN|'.IndexOf('|'+operStr+'|');
            case oper of
              0:; // <
              2:; // <=
              5:; // =
              7:; // <>
              10:; // >
              12:; // >=
              15, 18:; // in, IN
              // todo: in/IN does not work with strings (' will be filtered out of valueStr)
            else
              oper := -1;
            end;
            if oper>=0 then
            begin
              if not Double.TryParse(valueStr, v)
              then valueStr := SurroundWithQuotes(SafeSQLValue(valueStr, true), '''')
              else valueStr := v.ToString(dotFormat);
              // find table and key field for category
              // for now only process first layer
              layer := layers.First;

              // todo: not fool proof
              if (layer as TLayer).query.ToUpper.IndexOf('WHERE')>=0
              then sql := (layer as TLayer).query+' AND '+fieldName+operStr+valueStr
              else sql := (layer as TLayer).query+' WHERE '+fieldName+operStr+valueStr;

              Log.WriteLn('select by query: '+sql);

              objectsGeoJSON := '';

              query := TFDQuery.Create(nil);
              try
                query.Connection := project.dbConnection as TFDConnection;
                query.SQL.Text := sql;
                try
                  query.Open();
                  query.First();
                  while not query.Eof do
                  begin
                    objectID := query.Fields[0].AsAnsiString;
                    if (layer as TLayer).FindObject(objectID, layerObject) then
                    begin
                      if objectsGeoJSON<>''
                      then objectsGeoJSON := objectsGeoJSON+',';
                      objectsGeoJSON := objectsGeoJSON+layerObject.JSON2D[0, (layer as TLayer).geometryType, ''];
                    end;
                    query.Next();
                  end;
                except
                  on e: exception do
                  begin
                    log.WriteLn('exception in TEcodistrictScenario.selectObjectsProperties: '+e.Message+' (sql: '+query.SQL.Text+')', llError);
                  end;
                end;
              finally
                query.Free;
              end;

              Result :=
                '{"selectedObjects":{"selectCategories":["'+layer.ID+'"],'+
                '"mode":"'+aMode+'",'+
                '"objects":['+objectsGeoJSON+']}}';
            end;
          end;
        end;
      end;


    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

function TEcodistrictScenario.selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string;
var
  layers: TList<TLayer>;
  category: string;
  tableName: string;
  fields: string;
  op: TDIObjectProperty;
  key: string;
  selectedObjects: string;
  so: string;
  query: TFDQuery;
  f: Integer;
  value: string;
  fieldMin: TField;
  fieldMax: TField;
  jsonProperties: string;
  scenarioSchema: string;
  selectedCategories: string;
begin
  (project as TEcodistrictProject).PingDatabase('TEcodistrictScenario.selectObjectsProperties');
  Result := '';
  layers := TList<TLayer>.Create;
  try
    scenarioSchema := ScenarioSchemaName;
    for category in aSelectCategories do
    begin
      tableName := '';
      key := '';
      fields := '';
      jsonProperties := '';
      for op in (project as TEcodistrictProject).DIObjectProperties do
      begin
        if category=op.tableName then
        begin
          if tableName=''
          then tableName := op.tableName;
          if key=''
          then key := op.keyFieldName;
          if fields<>''
          then fields := fields+',';
          fields := fields+'min('+op.fieldName+') as '+op.fieldName+',max('+op.fieldName+') as '+op.fieldName+'_compare';
        end;
      end;
      if tableName<>'' then
      begin
        selectedObjects := '';
        for so in aSelectedObjects do
        begin
          if selectedObjects<>''
          then selectedObjects := selectedObjects+',';
          selectedObjects := selectedObjects+''''+so+'''';
        end;

        query := TFDQuery.Create(nil);
        try
          query.Connection := project.dbConnection as TFDConnection;
          query.SQL.Text :=
            'SELECT '+fields+' '+
            'FROM '+scenarioSchema+'.'+tableName+' '+
            'WHERE '+key+' in ('+selectedObjects+')';
          try
            query.Open();
            query.First();
            while not query.Eof do
            begin
              // should only be 1 entry!
              f := 0;
              while f<query.FieldCount do
              begin
                fieldMin := query.Fields[f];
                fieldMax := query.Fields[f+1];
                if fieldMin.IsNull or fieldMax.IsNull
                then value := 'null'
                else
                begin
                  if fieldMin.AsString=fieldMax.AsString then
                  begin
                    case query.Fields[f].DataType of
                      ftWideMemo,
                      ftString,
                      ftWideString: // todo: check widestring conversion..
                        value := '"'+query.Fields[f].AsString+'"';
                      ftInteger:
                        value := fieldMin.AsInteger.ToString;
                      ftLargeint:
                        value := fieldMin.AsLargeInt.ToString;
                      ftFloat,
                      ftExtended,
                      ftSingle:
                        value := sqlF(fieldMin.AsFloat);
                      ftBoolean:
                        value := sqlB(fieldMin.AsBoolean);
                    else
                      Log.WriteLn('Unsupported DataType in getData TABLE request: '+Ord(fieldMin.DataType).toString, llWarning);
                      value := 'null';
                    end;
                  end
                  else value := 'null';
                end;

                for op in (project as TEcodistrictProject).DIObjectProperties do
                begin
                  if (category=op.tableName) and (query.Fields[f].FieldName=op.fieldName) then
                  begin
                    // add entry to result for field
                    if jsonProperties<>''
                    then jsonProperties := jsonProperties+',';
                    jsonProperties := jsonProperties+'{'+op.toJSON(value)+'}';
                  end;
                end;
                f := f+2;
              end;
              query.Next();
            end;
          except
            on e: exception do
            begin
              log.WriteLn('exception in TEcodistrictScenario.selectObjectsProperties: '+e.Message+' (sql: '+query.SQL.Text+')', llError);
            end;
          end;
        finally
          query.Free;
        end;
        // rebuild to final result if contains data
        if jsonProperties<>'' then
        begin
          Result :=
            '{"selectedObjectsProperties":'+
              '{'+
                '"selectedCategories": ["'+category+'"],'+
                '"properties":['+jsonProperties+'],'+
                '"selectedObjects":['+selectedObjects.Replace('''','"')+']'+
              '}'+
            '}';
          exit;
        end;
      end;
    end;
    selectedCategories := '';
    for category in aSelectCategories do
    begin
      if selectedCategories<>''
      then selectedCategories := selectedCategories+',';
      selectedCategories := selectedCategories+category;
    end;
    log.WriteLn('TEcodistrictScenario.selectObjectsProperties: no table name found for selected categories ('+selectedCategories+')', llWarning);
  finally
    layers.Free;
  end;
end;

function TEcodistrictScenario.ScenarioSchemaName: string;
begin
  if fID=fProject.ProjectID
  then Result := EcoDistrictSchemaId(fID)
  else Result := EcoDistrictSchemaId(fProject.ProjectID, fID);
end;

function TEcodistrictScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories, aSelectedIDs: TArray<string>): string;
var
  layers: TList<TLayerBase>;
  categories: string;
  catList: TDictionary<string, integer>;
  objectsGeoJSON: string;
  totalObjectCount: Integer;
  l: TLayerBase;
  oi: string;
  lo: TLayerObjectWithID;
  c: Integer;
  cat: string;
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

      categories := '';
      objectsGeoJSON := '';
      totalObjectCount := 0;
      catList := TDictionary<string, integer>.Create;
      try
        for l in layers do
        begin
          for oi in aSelectedIDs do
          begin
            if (l as TLayer).objects.TryGetValue(TWDID(oi), lo) then
            begin
              if catList.TryGetValue(l.ID, c)
              then catList[l.ID] := c+1
              else catList.AddOrSetValue(l.ID, 1);
              if objectsGeoJSON<>''
              then objectsGeoJSON := objectsGeoJSON+',';
              objectsGeoJSON := objectsGeoJSON+lo.JSON2D[0, (l as TLayer).geometryType, ''];
              totalObjectCount := totalObjectCount+1;
            end;
          end;
          if (aMode='+') and (catList.Count>0)
          then break;
        end;
        if catList.Count>0 then
        begin
          if catList.Count>1 then
          begin
            for cat in catList.Keys do
            begin
              if categories=''
              then categories := '"'+cat+'"'
              else categories := categories+',"'+cat+'"';
            end;
          end
          else categories := '"'+catList.Keys.ToArray[0]+'"';
        end;


      finally
        catList.Free;
      end;
      Result :=
        '{"type":"selectedObjects","payload":{"selectCategories":['+categories+'],'+
         '"mode":"'+aMode+'",'+
         '"objects":['+objectsGeoJSON+']}}';
      Log.WriteLn('select on radius:  found '+totalObjectCount.ToString+' objects in '+categories);



    end;
    // todo: else warning?
  finally
    layers.Free;
  end;
end;

{ TDIObjectProperty }

function TDIObjectProperty.toJSON(const aValue: string): string;

  function jsonEditable: string;
  begin
    if editable
    then Result := 'Y'
    else Result := 'N';
  end;

  function jsonForced: string;
  begin
    // todo: not in database (yet?)
    if true
    then Result := 'Y'
    else Result := 'N';
  end;

begin
  {
    "name" : "Height",
    "value" : 5 | "Marie" | false, 300.3
    "type" : "list" | "string" | "int" | "float" | "bool",
    "editable" : "Y" | "N",
    "options" : [1, 3, 343, 5],
    "forced" : "Y" | "N"
  }
  Result :=
    '"name":"'+propertyName+'",'+
    '"type":"'+propertyType+'",'+
    '"editable":"'+jsonEditable+'",'+
    '"options":['+selection+'],'+
    '"forced":"'+jsonForced+'",'+
    '"value":'+aValue;
end;

{ TEcodistrictProject }

procedure TEcodistrictProject.AddDIMeasureHistory(aMeasureHistory: TDIMeasureHistory);
begin
  // store locally (read if not loaded)
  DIMeasuresHistory.Add(aMeasureHistory.id, aMeasureHistory);
  // store to database
  (fDBConnection as TFDConnection).ExecSQL(
    'INSERT INTO '+ProjectSchemaName+'.di_measureshistory (id, measure, object_ids, timeutc, variants, categories) '+
    'VALUES ('+
      ''''+aMeasureHistory.id.ToString+''','+
      ''''+aMeasureHistory.measure+''','+
      ''''+aMeasureHistory.object_ids+''','+
      'now() at time zone ''UTC'','+
      ''''+aMeasureHistory.variants+''','+
      ''''+aMeasureHistory.categories+''')');
end;

procedure TEcodistrictProject.AddDownloadableTableNames(const aID: string);
var
  query: TFDQuery;
  tableName: string;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT table_name FROM information_schema.tables '+
      'WHERE table_schema = '''+aID+''' and table_type=''BASE TABLE'' and table_name not in ('+
        '''spatial_ref_sys'','+
        '''dm_queries'','+
        '''di_measures'', ''di_objectproperties'', ''di_queries'', ''di_restapi'', ''di_measureshistory'','+
        '''kpi_results'')';
    query.Open;
    while not query.Eof do
    begin
      tableName :=  query.Fields[0].AsString;
      addDownloadableFile(tableName, [udftShape, udftText, udftLayer], GenerateDownloadableFile);
      query.Next;
    end;
  finally
    query.Free;
  end;
end;

constructor TEcodistrictProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aDBConnection: TCustomConnection; aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aKPIList: TObjectList<TEcodistrictKPI>);
var
  WorldMapView: TMapView;
begin
  fKpiList := TObjectDictionary<string, TEcodistrictKPI>.Create;
  UpdateKPIList(aKPIList);
  fDIObjectProperties := nil; // TObjectList<TDIObjectProperty>.Create;
  fDMQueries := nil;
  fDIQueries := nil;
  fDIMeasuresHistory := nil;
  fDIMeasures := nil;
  WorldMapView := TMapView.Create(0, 0, 1);
  inherited Create(
    aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL,
    aDBConnection, aAddBasicLayers, aMaxNearestObjectDistanceInMeters, WorldMapView);
  fTiler.onTilerStatus := handleTilerStatus;
  //set the EcoDistrict controls
  EnableControl(selectControl);
  EnableControl(measuresControl);
  EnableControl(measuresHistoryControl);
  EnableControl(filesControl);
end;

destructor TEcodistrictProject.Destroy;
begin
  FreeAndNil(fDIObjectProperties);
  FreeAndNil(fDMQueries);
  FreeAndNil(fDIQueries);
  FreeAndNil(fDIMeasuresHistory);
  FreeAndNil(fKpiList);
  inherited;
end;

procedure TEcodistrictProject.GenerateDownloadableFile(aProject: TProject; aClient: TClient; const aTableName, aFileType: string);
var
  stream: TStream;
  query: TFDQuery;
  f: Integer;
  schemaName: string;
  fieldNames: string;
  fieldName: string;
  Line: TStringList;
  writer: TStreamWriter;
  geometryFields: TStrings;
  shape: TGIS_Shape;
  shapeFile: TGIS_LayerSHP;
  shapeJSON: TGIS_Shape;
  _json: string;
  tempDataFolder: string;
  streamFileName: string;
  minValue: Double;
  maxValue: Double;
  layersJSON: TWDJSONObject;
  diQuery: TDIQuery;
  avgValue: Double;
begin
  try
    // signal the preparing of the download to the user
    aClient.SendMessage('Preparing download of '+aTableName, TMessageType.mtNone, 3000);
    // get schema name
    if Assigned(aClient.currentScenario)
    then schemaName :=(aClient.currentScenario as TEcodistrictScenario).ScenarioSchemaName
    else schemaName :=ProjectSchemaName;
    Log.WriteLn('Preparing download of '+aTableName+' from '+schemaName);
    // get column information to build query
    fieldNames := '';
    geometryFields := TStringList.Create;
    try
      Log.WriteLn('reading table structure');
      query := TFDQuery.Create(nil);
      try
        query.Connection := fDBConnection as TFDConnection;
        query.SQL.Text :=
          'SELECT column_name, data_type, is_nullable '+
          'FROM information_schema.columns '+
          'WHERE table_schema = '''+schemaName+''' and table_name='''+aTableName+''' '+
          'ORDER BY ordinal_position';
        query.Open();
        while not query.Eof do
        begin
          if aFileType=udftLayer then
          begin
            // skip indexed fields for now (detected by checking for nullable fields)..
            if query.Fields[2].AsBoolean then
            begin
              fieldName := query.Fields[0].AsString;
              case FieldTypeName2Type(query.Fields[1].AsString) of
                //ftString:
                ftInteger,
                ftFloat,
                ftCurrency:
                  begin
                    fieldName := 'MIN('+fieldName+') as min_'+fieldName+', AVG('+fieldName+') as avg_'+fieldName+', MAX('+fieldName+') max_'+fieldName;
                    if fieldNames<>''
                    then fieldNames := fieldNames+',';
                    fieldNames := fieldNames+fieldName;
                  end;
                //ftTimeStamp
                //ftBoolean
                //ftGuid
                //ftObject
              end;
            end;
          end
          else
          begin
            if query.Fields[1].AsString='USER-DEFINED' then
            begin
              fieldName := 'ST_AsGeoJSON('+query.Fields[0].AsString+') as '+query.Fields[0].AsString;
              geometryFields.Add(query.Fields[0].AsString);
            end
            else fieldName := query.Fields[0].AsString;
            if fieldNames<>''
            then fieldNames := fieldNames+',';
            fieldNames := fieldNames+fieldName;
          end;
          query.Next;
        end;
      finally
        query.Free;
      end;
      // send contents of table to client as csv file
      streamFileName := '';
      tempDataFolder := '';
      writer := nil;
      stream := TMemoryStream.Create();
      try
        query := TFDQuery.Create(nil);
        try
          query.Connection := fDBConnection as TFDConnection;
          query.SQL.Text :=
            'SELECT '+fieldNames+' '+
            'FROM '+schemaName+'.'+aTableName;
          if fieldNames<>''
          then query.Open();
          if aFileType=udftLayer then
          begin
            Log.WriteLn('reading table meta data to save as layer information');
            streamFileName := aTableName+'.layer';

            with WDJSONCreate(layersJSON) do
            begin
              with addArray('layers') do
              begin
                // todo: build layer meta information file in json format
                // only 1 record should be in query giving min and max values of int and float fields
                // fields are in pairs, min and max
                f := 0;
                if fieldNames<>'' then
                begin
                  while f<query.FieldCount do
                  begin
                    fieldName := query.Fields[f].FieldName.Substring(4); // strip min_
                    minValue := query.Fields[f+0].AsFloat;
                    avgValue := query.Fields[f+1].AsFloat;
                    maxValue := query.Fields[f+2].AsFloat;
                    // check for existing definition
                    if DIQueries.TryGetValue(aTableName+'_'+fieldName, diQuery) then
                    begin
                      with addObject() do // object within array
                      begin
                        add('datafieldname', fieldName);
                        with addObject('statistics') do
                        begin
                          add('min', minValue);
                          add('avg', avgValue);
                          add('max', maxValue);
                        end;
                        add('id', diQuery.id);
                        add('name', diQuery.name);
                        add('domain', diQuery.domain);
                        add('description', diQuery.description);
                        add('defaultload', diQuery.defaultload);
                        add('basiclayer', diQuery.basiclayer);
                        add('sql', diQuery.sql);
                        add('palettejson', TWDJSONObjectAsText.Create(diQuery.palettejson));
                        add('legendjson', TWDJSONObjectAsText.Create(diQuery.legendjson));
                      end;
                    end
                    else
                    begin
                      // definition does not exist yet, so create one for the layer file
                      with addObject() do // object within array
                      begin
                        add('datafieldname', fieldName);
                        with addObject('statistics') do
                        begin
                          add('min', minValue);
                          add('avg', avgValue);
                          add('max', maxValue);
                        end;
                        add('id', aTableName+'_'+fieldName);
                        add('name', fieldName);
                        add('domain', 'domain');
                        add('description', aTableName+' '+fieldName);
                        add('defaultload', 0);
                        add('basiclayer', 0);
                        add('sql', '');
                        with addObject('palettejson') do
                        begin
                          add('description', aTableName+' '+fieldName);
                          add('type','ramp');
                          with addArray('entries') do
                          begin
                            with addObject() do // object within array
                            begin
                              add('description', 'low');
                              add('color', '#ffeeee');
                              add('value', minValue);
                            end;
                            with addObject() do // object within array
                            begin
                              add('description', 'high');
                              add('color', '#ff0000');
                              add('value', maxValue);
                            end;
                          end;
                          add('lowerDataColor', '#ffeeee');
                          add('noDataColor', '#000000');
                          add('higherDataColor', '#ff0000');
                          add('type','ramp');
                        end;
                        add('legendjson', TWDJSONObjectAsText.Create(''));
                      end;
                    end;
                    f :=f+3;
                  end;
                end;
              end;
            end;
            try
              writer := TStreamWriter.Create(stream);
              writer.WriteLine(layersJSON.JSON(pretty));
            finally
              layersJSON.Free;
            end;
          end
          else if aFileType=udftText then
          begin
            Log.WriteLn('reading table data to save as text');
            streamFileName := aTableName+'.txt';
            Line := TStringList.Create;
            try
              Line.Delimiter := #9; // ccTab ';';
              Line.StrictDelimiter := True;
              //Line.QuoteChar
              // add header
              for f := 0 to query.FieldCount-1 do
              begin
                Line.Add(query.Fields[f].FieldName+':'+DownloadType2TypeName(
                  query.Fields[f].DataType,
                  geometryFields.IndexOf(query.Fields[f].FieldName)>=0));
              end;
              writer := TStreamWriter.Create(stream);
              writer.WriteLine(Line.DelimitedText);
              // add rows of values
              while not query.Eof do
              begin
                line.Clear;
                for f := 0 to query.FieldCount-1 do
                begin
                  case query.Fields[f].DataType of
                    ftBoolean:   Line.Add(sqlB(query.Fields[f].AsBoolean));
                    ftFloat:     Line.Add(sqlF(query.Fields[f].AsFloat));
                  else
                                 Line.Add(query.Fields[f].AsString);
                  end;
                end;
                writer.WriteLine(Line.DelimitedText);
                query.Next;
              end;
            finally
              Line.Free;
            end;
          end
          else if aFileType=udftShape then
          begin
            Log.WriteLn('reading table data to save as shape in zip');
            streamFileName := aTableName+'.zip';
            if geometryFields.Count=1 then
            begin
              // temp file name
              tempDataFolder := IncludeTrailingPathDelimiter(GetTempDirectory)+TGUID.NewGuid.ToString;
              ForceDirectories(tempDataFolder);
              shapeFile := TGIS_LayerSHP.Create;
              try
                shapeFile.SetCSByEPSG(GIS_EPSG_WGS84);
                shapeFile.Name := aTableName;
                //
                for f := 0 to query.FieldCount-1 do
                begin
                  // parse only non-geometry fields as fields in shape
                  if geometryFields.IndexOf(query.Fields[f].FieldName)<0 then
                  begin
                    case query.Fields[f].DataType of
                      ftString,
                      ftWideMemo:  shapeFile.AddField(query.Fields[f].FieldName, TGIS_FieldType.String, 1, 0);
                      ftInteger:   shapeFile.AddField(query.Fields[f].FieldName, TGIS_FieldType.Number, 10, 0);
                      ftBoolean:   shapeFile.AddField(query.Fields[f].FieldName, TGIS_FieldType.Boolean, 1, 0);
                      ftFloat:     shapeFile.AddField(query.Fields[f].FieldName, TGIS_FieldType.Float, 0, 0 );
                      ftTimeStamp: shapeFile.AddField(query.Fields[f].FieldName, TGIS_FieldType.Date, 0, 0 );
                    else
                                   //Line.Add(query.Fields[f].FieldName+':'+Ord(query.Fields[f].DataType).toString);
                      Log.WriteLn('Unknown field type in download query result: '+query.Fields[f].FieldName+': '+Ord(query.Fields[f].DataType).toString, llWarning);
                    end;
                  end;
                end;

                Log.WriteLn('building shape file');
                shapeFile.Build(tempDataFolder+'\'+aTableName+'.shp', TGIS_Utils.GisExtent( -180, -90, 180, 90 ), TGIS_ShapeType.Polygon, TGIS_DimensionType.XY);
                shapeFile.Open;
                while not query.Eof do
                begin
                  // add shape to file
                  _json := query.FieldByName(geometryFields[0]).AsString;
                  if _json<>''
                  then shapeJSON := TGIS_Utils.GisCreateShapeFromJSON(_json)
                  else shapeJSON := TGIS_ShapePolygon.Create(); // add empty string
                  try
                    if Assigned(shapeJSON) then
                    begin
                      shape := shapeFile.AddShape(shapeJSON);
                      if Assigned(shape) then
                      begin
                        shape.Lock(TGIS_Lock.Projection);
                        try
                          // add properties to shape
                          for f := 0 to query.FieldCount-1 do
                          begin
                            if geometryFields.IndexOf(query.Fields[f].FieldName)<0 then
                            begin
                              case query.Fields[f].DataType of
                                ftString,
                                ftWideMemo:  shape.SetField(query.Fields[f].FieldName.Substring(0, 10), query.Fields[f].AsString);
                                ftInteger:   shape.SetField(query.Fields[f].FieldName.Substring(0, 10), query.Fields[f].AsInteger);
                                ftBoolean:   shape.SetField(query.Fields[f].FieldName.Substring(0, 10), query.Fields[f].AsBoolean);
                                ftFloat:     shape.SetField(query.Fields[f].FieldName.Substring(0, 10), query.Fields[f].AsFloat);
                              end;
                            end;
                          end;
                        finally
                          shape.Unlock;
                        end;
                      end
                      else Log.WriteLn('TEcodistrictProject.GenerateDownloadableFile: could not convert shape in '+aTableName+' with id '+shapeJSON.Uid.ToString, llError);
                    end
                    else Log.WriteLn('TEcodistrictProject.GenerateDownloadableFile: could not read shape in '+aTableNAme+' from JSON '+query.FieldByName(geometryFields[0]).AsString, llWarning);
                  finally
                    shapeJSON.Free;
                  end;
                  query.Next;
                end;
                shapeFile.SaveData;
              finally
                shapeFile.Free;
              end;
              Log.WriteLn('shape file build to start zipping');
              ZipFiles(tempDataFolder+'\'+aTableName+'.zip', tempDataFolder, [
                aTableName+'.shp',
                aTableName+'.shx',
                aTableName+'.prj',
                aTableName+'.dbf',
                aTableName+'.cpg']);
              stream := TFileStream.Create(tempDataFolder+'\'+aTableName+'.zip', fmOpenRead);
            end
            else
            begin
              if geometryFields.Count=0
              then aClient.SendMessage('No geometry in '+aTableName, TMessageType.mtError, 5000)
              else aClient.SendMessage(aFileType+' only supports 1 column with geometry ('+aTableName+')', TMessageType.mtError, 5000);
              Log.WriteLn('File type "'+aFileType+'" does not supported '+geometryFields.Count.ToString+' number of geometry fields that are in '+aTableName, llError);
              // todo: report error to client
            end;
          end
          else
          begin
            Log.WriteLn('File type "'+aFileType+'" not supported for downloading '+aTableName, llError);
            // todo: report error to client
          end;
        finally
          query.Free;
        end;
        if Assigned(writer)
        then writer.Flush;
        stream.Position := 0;
        Log.WriteLn('making file available for user to download through client');
        aClient.SendDownloadableFileStream(streamFileName, stream);
      finally
        writer.Free;
        stream.Free;
      end;
      // cleanup
      if tempDataFolder<>'' then
      begin
        if not CleanupDirectory(tempDataFolder)
        then Log.WriteLn('Could not cleanup temp folder '+tempDataFolder, llWarning);
      end;
    finally
      geometryFields.Free;
    end;
  except
    on E: Exception
    do Log.WriteLn('Unhandled exception in TEcodistrictProject.GenerateDownloadableFile: '+E.Message, llError);
  end;
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
      (*
       	{
          "measure": {
            "description": "...",
            "id": ???,
            "measure": "...",
            "name": "..."
            },
          "selectCategories": ["building", "enzo"],
          "selectedObjects": ["7", "1"],
          "time": "2016-08-09 12:34:52",
          "id": GUID
          }
      *)
      if Result<>''
      then Result := Result+',';
      Result := Result+'{'+
        '"measure":'+mi.Value.measure+', '+
        '"selectCategories": ['+mi.Value.categories+'],'+
        '"selectedObjects": ['+mi.Value.object_ids+'],'+
        '"time": "'+FormatDateTime(publisherDateTimeFormat, mi.Value.timeutc)+'",'+
        '"id": "'+mi.Value.id.ToString+'"'+
        '}'; // todo:
    end;
  finally
    TMOnitor.Exit(mhl);
  end;
end;

function TEcodistrictProject.getMeasuresJSON: string;
var
  projectSchema: string;
begin
  projectSchema := ProjectSchemaName;
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
            'from '+projectSchema+'.di_measures m '+
            'where m.id>=0 and m.category=j.category and m.typology=j.typology '+
            'order by id) as actions) '+
          'from '+projectSchema+'.di_measures j '+
          'where j.id>=0 '+
          'group by category, typology '+
          'having category=c.category '+
        ') as typologies '+
      ') as measures '+
      'from '+projectSchema+'.di_measures c '+
      'where c.id>=0 '+
      'group by category '+
      'order by category '+
      ') as categories');
end;

function TEcodistrictProject.getQueryDialogDataJSON: string;
var
  op: TDIObjectProperty;
  res: TDictionary<string, string>;
  def: string;
  tfp: TPair<string, string>;
begin
  // todo: build JSON object '{..}' or 'null'
  res := TDictionary<string, string>.Create;
  try
    // agregate fields per table name
    for op in DIObjectProperties do
    begin
      if res.TryGetValue(op.tableName, def)
      then def := def+','+'"'+op.fieldName+'"'
      else def := '"'+op.fieldName+'"';
      res.AddOrSetValue(op.tableName, def);
    end;
    // build result
    if res.Count>0 then
    begin
      result := '';
      for tfp in res do
      begin
        if result<>''
        then result := result+',';
        Result := Result+'"'+tfp.Key+'":['+tfp.Value+']';
      end;
      result := '{'+result+'}';
    end
    else Result := 'null';
  finally
    res.Free;
  end;
end;

function TEcodistrictProject.getDIMeasures: TDictionary<string, TDIMeasure>;
begin
  if not Assigned(fDIMeasures)
  then ReadDIMeasures();
  Result := fDIMeasures;
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

procedure TEcodistrictProject.handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject);
var
  jsonMeasures: TJSONArray;
  jsonMeasure: TJSONValue;
  measureId: string;
  categories: string;
  jsonObjectIDs: TJSONArray;
  jsonObjectID: TJSONValue;
  sql: string;
  objectIDs: string;
  projectSchema: string;
  scenarioSchema: string;
  mh: TDIMeasureHistory;
  jsonMeasureCategories: TJSONArray;
  jsonCategory: TJSONValue;
  measure: TJSONObject;
  jsonApplyObjectsProperties: TJSONObject;
  jsonSelectedCategories: TJSONArray;
  tableName: string;
  jsonProperties: TJSONArray;
  jsonProperty: TJSONValue;
  properties: string;
  op: TDIObjectProperty;
  propName: string;
  jsonValue: TJSONValue;
  keyFieldName: string;
  propFieldName: string;
  propValue: string;
begin
  if aJSONObject.TryGetValue<TJSONArray>('applyMeasures', jsonMeasures) then
  begin
    Log.WriteLn('applyMeasures..');
    for jsonMeasure in jsonMeasures do
    begin
      measureId := (jsonMeasure as TJSONObject).GetValue<string>('measure.id');
      measure := (jsonMeasure as TJSONObject).GetValue<TJSONObject>('measure');
      // todo: should always be 1 category (for now)
      jsonObjectIDs := (jsonMeasure as TJSONObject).GetValue<TJSONArray>('selectedObjects');
      objectIDs := '';
      for jsonObjectID in jsonObjectIDs do
      begin
        if objectIDs<>''
        then objectIDs := objectIDs+',';
        objectIDs := objectIDs+jsonObjectID.ToJSON;
      end;
      jsonMeasureCategories :=(jsonMeasure as TJSONObject).GetValue<TJSONArray>('selectCategories');
      categories := '';
      for jsonCategory in jsonMeasureCategories do
      begin
        if categories<>''
        then categories := categories+',';
        categories := categories+jsonCategory.ToJSON;
      end;
      projectSchema := ProjectSchemaName;
      if Assigned(aScenario)
      then scenarioSchema := (aScenario as TEcodistrictScenario).ScenarioSchemaName
      else scenarioSchema := projectSchema;
      // todo: switch to DIMeasures in project
      sql := FDReadJSON(
        fDBConnection as TFDConnection,
        'SELECT query '+
        'FROM '+projectSchema+'.di_measures '+
        'WHERE cat||id='''+measureId+'''').Replace('{case_id}', scenarioSchema).Replace('{ids}', objectIDs.Replace('"', ''''));
      if sql<>'' then
      begin
        // todo: check for sql injection -> fix
        (fDBConnection as TFDConnection).ExecSQL(sql);
        Log.WriteLn('Applied measure '+measureId+' ('+categories+'): '+sql);
        // add measure to history
        mh.id := TGUID.NewGuid;
        mh.measure := measure.ToJSON;
        mh.object_ids := objectIDs;
        mh.timeutc := Now; // todo: convert to utc?
        mh.variants := scenarioSchema;
        mh.categories := categories;
        AddDIMeasureHistory(mh);
      end
      else Log.WriteLn('Could not build SQL to apply measure '+measureId+' ('+categories+'): '+objectIDs);
    end;
  end
  else if aJSONObject.TryGetValue<TJSONObject>('applyObjectsProperties', jsonApplyObjectsProperties) then
  begin
    Log.WriteLn('Apply object properties');
    jsonSelectedCategories := jsonApplyObjectsProperties.GetValue<TJSONArray>('selectedCategories');
    if jsonSelectedCategories.Count=1 then
    begin
      tableName := jsonSelectedCategories.Items[0].Value;

      jsonProperties := jsonApplyObjectsProperties.GetValue<TJSONArray>('properties');
      properties := '';
      for jsonProperty in jsonProperties do
      begin
        propName := jsonProperty.GetValue<string>('name');

        jsonValue := jsonProperty.GetValue<TJSONValue>('value');
        if not (jsonValue is TJSONNull) then
        begin
          if (jsonValue is TJSONNumber)
          then propValue := (jsonValue as TJSONNumber).AsDouble.ToString(dotFormat)
          else propValue := ''''+SafeSQLValue(jsonValue.Value, False)+'''';
        end
        else propValue := 'null';

        propFieldName := '';
        for op in DIObjectProperties do
        begin
          if (op.tableName=tableName) and (op.propertyName=propName) then
          begin
            propFieldName := op.fieldName;
            keyFieldName := op.keyFieldName;
            break;
          end;
        end;
        if propFieldName<>'' then
        begin
          if properties<>''
          then properties := properties+',';
          properties := properties+SafeSQLValue(propFieldName, True)+'='+propValue;
        end;
      end;

      // todo: split in batches to avoid to many object ID's in SQL " WHERE .. IN (..)"

      jsonObjectIDs := jsonApplyObjectsProperties.GetValue<TJSONArray>('selectedObjects');
      objectIDs := '';
      for jsonObjectID in jsonObjectIDs do
      begin
        if objectIDs<>''
        then objectIDs := objectIDs+',';
        objectIDs := objectIDs+SafeSQLValue(jsonObjectID.ToJSON, True);
      end;

      projectSchema := ProjectSchemaName;
      if Assigned(aScenario)
      then scenarioSchema := (aScenario as TEcodistrictScenario).ScenarioSchemaName
      else scenarioSchema := projectSchema;

      sql := 'UPDATE '+scenarioSchema+'.'+tableName+' SET '+properties+' WHERE '+keyFieldName+' IN ('+objectIDs.Replace('"', '''')+')';

      Log.WriteLn(sql);

      // todo: execute sql and inform modules?

    end
    else Log.WriteLn('Apply object properties: selectedCategories.Count='+jsonSelectedCategories.Count.ToString+' (<>1)', llWarning);
  end
  else
  begin
    if Assigned(aScenario)
    then Log.WriteLn('Unhandled client event for scenario '+aScenario.elementID+': '+aJSONObject.ToJSON, llWarning)
    else Log.WriteLn('Unhandled client event for base scenario: '+aJSONObject.ToJSON, llWarning);
  end;
end;

procedure TEcodistrictProject.HandleFileUpload(aClient: TClient; const aFileInfo: TClientFileUploadInfo);

  function TableExists(const aSchemaName, aTableName: string): Boolean;
  var
    query: TFDQuery;
  begin
    query := TFDQuery.Create(nil);
    try
      query.Connection := fDBConnection as TFDConnection;
      query.SQL.Text :=
        'SELECT table_name FROM information_schema.tables '+
        'WHERE table_schema = '''+aSchemaName+''' and table_type=''BASE TABLE'' and table_name='''+aTableName+'''';
      query.Open;
      Result := not query.Eof;
    finally
      query.Free;
    end;
  end;

  procedure AllTableColumns(const aSchemaName, aTableName: string; aExistingFieldDefs: TDictionary<string, TFieldType>);
  // aExistingFieldDefs: field names are cast to lower case for quicker lookup
  var
    query: TFDQuery;
  begin
    query := TFDQuery.Create(nil);
    try
      query.Connection := fDBConnection as TFDConnection;
      query.SQL.Text :=
        'SELECT column_name, data_type '+
        'FROM information_schema.columns '+
        'WHERE table_schema = '''+aSchemaName+''' and table_name='''+aTableName+'''';
      query.Open();
      while not query.Eof do
      begin
        aExistingFieldDefs.AddOrSetValue(query.Fields[0].AsString.ToLower, FieldTypeName2Type(query.Fields[1].AsString.ToLower));
        query.Next;
      end;
    finally
      query.Free;
    end;
  end;

  function FixupTable(const aSchemaName, aTableName: string; aIDFieldIndex: Integer; aFieldDefs: TObjectList<TFileInfoRecord>): Boolean;
  var
    sql: string;
    //fdp: TFileInfoRecord;
    f: Integer;
    existingFieldDefs: TDictionary<string, TFieldType>;
    added: Boolean;
    existingFieldType: TFieldType;
  begin
    if TableExists(aSchemaName, aTableName) then
    begin
      existingFieldDefs := TDictionary<string, TFieldType>.Create;
      try
        AllTableColumns(aSchemaName, aTableName, existingFieldDefs);
        // prepare sql statement
        sql :=
          'ALTER TABLE '+aSchemaName+'.'+aTableName+' ';
        added := False;
        // add columns to be added (we are not going to change columns!)
        //for fdp in aFieldDefs do
        for f := 0 to aFieldDefs.Count-1 do
        begin
          if aFieldDefs[f].fieldType<>ftUnknown then
          begin
            if existingFieldDefs.TryGetValue(aFieldDefs[f].fieldName.ToLower, existingFieldType) then
            begin
              // already exists -> check
              if aFieldDefs[f].fieldType<>existingFieldType
              then Log.WriteLn('Upload of '+aTableName+': column type for '+aFieldDefs[f].fieldName+' deviates ('+aFieldDefs[f].getTypeName+'('+Ord(aFieldDefs[f].fieldType).toString+') <> '+PGFieldType2TypeName(existingFieldType, false)+'('+Ord(existingFieldType).toString+') )', llWarning);
            end
            else
            begin
              // does not already exist -> add
              if added
              then sql := sql+',';
              sql := sql+
                'ADD COLUMN '+aFieldDefs[f].fieldName+' '+aFieldDefs[f].getTypeName;
              added := True;
              Log.WriteLn('Upload of '+aTableName+' to existing data: adding column '+aFieldDefs[f].fieldName+': '+aFieldDefs[f].getTypeName);
            end;
          end
          else Log.WriteLn('Upload of '+aTableName+' to existing data: skipping column '+aFieldDefs[f].fieldName);
        end;
        if added then
        begin
          (fDBConnection as TFDConnection).ExecSQL(sql);
          Log.WriteLn('Upload: updated table structure');
        end;
      finally
        existingFieldDefs.Free;
      end;
      Result := False;
    end
    else
    begin
      // table create statement and id field
      sql :=
        'CREATE TABLE '+aSchemaName+'.'+aTableName+' ('+
          aFieldDefs[aIDFieldIndex].fieldName+' '+aFieldDefs[aIDFieldIndex].getTypeName+' NOT NULL';
      // add all other fields
      for f := 0 to aFieldDefs.Count-1 do
      begin
        if (aFieldDefs[f].fieldType<>ftUnknown) and (f<>aIDFieldIndex)
        then sql := sql+','+aFieldDefs[f].fieldName+' '+aFieldDefs[f].getTypeName;
      end;
      // add constraint
      sql := sql+
        ',CONSTRAINT '+aTableName+'_pkey PRIMARY KEY ('+aFieldDefs[aIDFieldIndex].fieldName+'))';
      // create table
      (fDBConnection as TFDConnection).ExecSQL(sql);
      Result := True;
    end;
  end;

  procedure AddField(const aFieldName, aFieldValue: string; aIsIndexField: Boolean; var aNames, aValues, aSets: string);
  begin
    if aNames<>''
    then aNames := aNames+',';
    if aValues<>''
    then aValues := aValues+',';
    aNames := aNames+aFieldName;
    aValues := aValues+aFieldValue;
    // for aSets parse all fields except index
    if not aIsIndexField then
    begin
      if aSets<>''
      then aSets := aSets+',';
      aSets := aSets+aFieldName+'='+aFieldValue;
    end;
  end;

  procedure UpsertRow(const aSchemaName, aTableName: string; aKeyFieldIndex: Integer; aFieldDefs: TObjectList<TFileInfoRecord>);
  var
    sql: string;
    names: string;
    values: string;
    f: Integer;
    sets: string;
  begin
    // build upsert
    names := '';
    values := '';
    sets := '';
    for f := 0 to aFieldDefs.Count-1 do
    begin
      if aFieldDefs[f].fieldType<>ftUnknown then
      begin
        AddField(aFieldDefs[f].fieldName, aFieldDefs[f].sqlFieldValue, aFieldDefs[f].isIndexField, names, values, sets);
      end;
    end;
    sql :=
      'INSERT INTO '+aSchemaName+'.'+aTableName+' ('+names+') '+
      'VALUES ('+values+') '+
      'ON CONFLICT ('+aFieldDefs[aKeyFieldIndex].fieldName+') '+
      'DO UPDATE SET '+sets;
    (fDBConnection as TFDConnection).ExecSQL(sql);
  end;

  procedure DeleteRow(const aSchemaName, aTableName: string; aKeyFieldINdex: Integer;  aFieldDefs: TObjectList<TFileInfoRecord>);
  var
    sql: string;
  begin
    sql :=
      'DELETE FROM '+aSchemaName+'.'+aTableName+' '+
      'WHERE '+aFieldDefs[aKeyFieldIndex].fieldName+'='+aFieldDefs[aKeyFieldIndex].sqlFieldValue;
    (fDBConnection as TFDConnection).ExecSQL(sql);
  end;

  function handleColumn(const lp: TArray<System.string>; fieldDefs: TObjectList<TFileInfoRecord>; out isGeometry: Boolean; var fieldNameGeometry: string; var f: Integer; var idfni: Integer): TFileInfoRecord;
  begin
    isGeometry := lp[1].ToLower='geometry';
    if isGeometry
    then fieldNameGeometry := lp[0];
    // column is id field if name = 'id' or if no id field is defined and column ends on '_id'
    if (lp[0].ToLower='id') or ((idfni<0) and lp[0].ToLower.EndsWith('_id')) then
    begin
      idfni := f;
      Result := TFileInfoRecord.Create(lp[0], FieldTypeName2Type(lp[1].ToLower), True, isGeometry);
      fieldDefs.Add(Result);
    end
    else
    begin
      if lp[1].ToLower='deg' then
      begin
        // reset to geometry field
        isGeometry := True;
        fieldNameGeometry := DefaultFieldNameGeometry;
        Result := TFileInfoRecord.Create(fieldNameGeometry, ftFloat, False, isGeometry);
        fieldDefs.Add(Result);
        Result.degreeConversion := True;
      end
      else
      begin
        Result := TFileInfoRecord.Create(lp[0], FieldTypeName2Type(lp[1].ToLower), False, isGeometry);
        fieldDefs.Add(Result);
      end;
    end;
  end;

  function DecodeDegreeCoordinate(const aDegreeCoordinate: string; out aIsLat: Boolean): Double;
  var
    dsi: Integer;
    cdeg: Integer;
    cminInt: Integer;
  begin
    dsi := aDegreeCoordinate.IndexOf(',');
    if dsi<0
    then dsi := aDegreeCoordinate.IndexOf('.');
    if dsi>=0 then
    begin
      // degrees part 1 or 2 digits
      if dsi-2>0
      then cdeg := Integer.Parse(aDegreeCoordinate.Substring(0, dsi-2))
      else cdeg := 0;
      // minutes parts (integer part)
      if dsi-2>=0
      then cminInt := Integer.Parse(aDegreeCoordinate.Substring(dsi-2, 2))
      else cminInt := Integer.Parse(aDegreeCoordinate.Substring(0, dsi));
      case UpCase(aDegreeCoordinate[aDegreeCoordinate.Length]) of
        'N':
          begin
            Result := cdeg+(cminInt+Double.Parse('0.'+aDegreeCoordinate.Substring(dsi+1, aDegreeCoordinate.Length-dsi-2), dotFormat))/60.0;
            aIsLat := True;
          end;
        'S':
          begin
            Result := -cdeg-(cminInt+Double.Parse('0.'+aDegreeCoordinate.Substring(dsi+1, aDegreeCoordinate.Length-dsi-2), dotFormat))/60.0;
            aIsLat := True;
          end;
        'E':
          begin
            Result := cdeg+(cminInt+Double.Parse('0.'+aDegreeCoordinate.Substring(dsi+1, aDegreeCoordinate.Length-dsi-2), dotFormat))/60.0;
            aIsLat := False;
          end;
        'W':
          begin
            Result := -cdeg-(cminInt+Double.Parse('0.'+aDegreeCoordinate.Substring(dsi+1, aDegreeCoordinate.Length-dsi-2), dotFormat))/60.0;
            aIsLat := False;
          end;
      else
        Result := cdeg+(cminInt+Double.Parse('0.'+aDegreeCoordinate.Substring(dsi+1, aDegreeCoordinate.Length-dsi-1), dotFormat))/60.0;
        aIsLat := False;
      end;
    end
    else
    begin
      Result := Double.NaN; // conversion error
      aIsLat := False;
    end;
  end;

  procedure UpsertDetailsLayer(
    const aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName, aSQL: string;
    const aID, aDomain, aName, aDescription: string;
    const aObjectTypes, aGeometryType:string; aDefaultLoad, aBasicLayer, aLayerType: Integer;
    const aPaletteJSON, aLegendJSON: string);
  var
    sql: string;
    names: string;
    values: string;
    sets: string;
  begin
    names := '';
    values := '';
    sets := '';

    AddField('id', sqlSN(aID), True, names, values, sets);
    AddField('domain', sqlSN(aDomain), false, names, values, sets);
    AddField('name', sqlSN(aName), false, names, values, sets);
    AddField('description', sqlSN(aDescription), false, names, values, sets);
    AddField('objecttypes', sqlSN(aObjectTypes), false, names, values, sets);
    AddField('geometrytype', sqlSN(aGeometryType), false, names, values, sets);
    AddField('defaultload', sqlI(aDefaultLoad), false, names, values, sets);
    AddField('basiclayer', sqlI(aBasicLayer), false, names, values, sets);
    AddField('sql', sqlSN(aSQL), false, names, values, sets);
    AddField('tablename', sqlSN(aTableName), false, names, values, sets);
    AddField('idfieldname', sqlSN(aIDFieldName), false, names, values, sets);
    AddField('geometryfieldname', sqlSN(aGeometryFieldName), false, names, values, sets);
    AddField('datafieldname', sqlSN(aDataFieldName), false, names, values, sets);
    AddField('layertype', sqlI(aLayerType), false, names, values, sets);
    AddField('palettejson', sqlSN(aPaletteJSON), false, names, values, sets);
    AddField('legendjson', sqlSN(aLegendJSON), false, names, values, sets);

    sql :=
      'INSERT INTO '+ProjectSchemaName+'.di_queries ('+names+') '+
      'VALUES ('+values+') '+
      'ON CONFLICT (id) '+
      'DO UPDATE SET '+sets; //+
    (fDBConnection as TFDConnection).ExecSQL(sql);
  end;

  procedure RemoveLayer(const aID: string);
  begin
    (fDBConnection as TFDConnection).ExecSQL(
      'DELETE FROM '+ProjectSchemaName+'.di_queries '+
      'WHERE id='''+aID+'''');
  end;

  procedure Refresh(aScenario: TScenario);
  begin
    aScenario.ReadBasicData;
    // todo: work-a-round, delay sending refresh to give tiler time to send url to use in update domains signal to clients
    (aScenario as TEcodistrictScenario).fRefreshTimer.Arm(dtOneSecond*5);
  end;

  procedure AddDetailsLayer(const aDataFieldName: string; aDIQuery: TDIQuery; aLayerDefinition: TJSONValue);
  var
    DIQueryNew: TDIQuery;
    jsonValue: TJSONValue;
//    objProp: TJSONValue;
//    op: TDIObjectProperty;
//    foundOP: TDIObjectProperty;
  begin
    DIQueryNew.datafieldname := aDataFieldName;
    if not aLayerDefinition.TryGetValue<string>('id', DIQueryNew.id)
    then DIQueryNew.id := aDIQuery.tablename+'_'+DIQueryNew.datafieldname;
    if not aLayerDefinition.TryGetValue<string>('domain', DIQueryNew.domain)
    then DIQueryNew.domain := aDIQuery.domain;
    if not aLayerDefinition.TryGetValue<string>('name', DIQueryNew.name)
    then DIQueryNew.name := DIQueryNew.id;
    if not aLayerDefinition.TryGetValue<string>('description', DIQueryNew.description)
    then DIQueryNew.description := DIQueryNew.name;
    DIQueryNew.objecttypes := aDIQuery.objecttypes;
    DIQueryNew.geometrytype := aDIQuery.geometryType;
    if not aLayerDefinition.TryGetValue<Integer>('defaultload', DIQueryNew.defaultload)
    then DIQueryNew.defaultload := 0;
    if not aLayerDefinition.TryGetValue<Integer>('basiclayer', DIQueryNew.basiclayer)
    then DIQueryNew.basiclayer := 0;
    if not aLayerDefinition.TryGetValue<string>('sql', DIQueryNew.sql)
    then DIQueryNew.sql := '';
    DIQueryNew.tablename := aDIQuery.tablename;
    DIQueryNew.idfieldname := aDIQuery.idfieldname;
    DIQueryNew.geometryfieldname := aDIQuery.geometryfieldname;
    DIQueryNew.layertype := aDIQuery.layertype;

    if aLayerDefinition.TryGetValue<TJSONValue>('palettejson', jsonValue) then
    begin
      if jsonValue is TJSONObject
      then DIQueryNew.palettejson := jsonValue.ToJSON
      else if jsonValue is TJSONString
      then DIQueryNew.palettejson := jsonValue.toString
      else DIQueryNew.palettejson := '';
    end
    else DIQueryNew.palettejson := '';

    if aLayerDefinition.TryGetValue<TJSONValue>('legendjson', jsonValue) then
    begin
      if jsonValue is TJSONObject
      then DIQueryNew.legendjson := jsonValue.ToJSON
      else if jsonValue is TJSONString
      then DIQueryNew.legendjson := jsonValue.ToString
      else DIQueryNew.legendjson := '';
    end
    else DIQueryNew.legendjson := '';

    DIQueries.AddOrSetValue(DIQueryNew.id, DIQueryNew);

    UpsertDetailsLayer(
      DIQueryNew.tablename, DIQueryNew.idfieldname, DIQueryNew.geometryfieldname, DIQueryNew.dataFieldName, DIQueryNew.SQL,
      DIQueryNew.id, DIQueryNew.domain, DIQueryNew.name, DIQueryNew.description, DIQueryNew.objecttypes, DIQueryNew.geometrytype, DIQueryNew.defaultLoad,
      DIQueryNew.basiclayer, DIQueryNew.layertype, DIQueryNew.paletteJSON, DIQueryNew.legendJSON);
    (*
    // selectable object property
    if aLayerDefinition.TryGetValue<TJSONValue>('objectproperty', objProp) then
    begin
      foundOP := nil;
      for op in DIObjectProperties do
      begin
        if (op.tableName.ToLower=aDIQuery.tablename.ToLower) and (op.fieldName.ToLower=aDataFieldName.ToLower) then
        begin
          foundOP := op;
          break;
        end;
      end;
      if objProp is TJSONObject then
      begin

        if Assigned(foundOP) then
        begin // change
//          foundOP.category := category := aDIQuery.domain;
//          foundOP.propertyname := aDIQuery.description;
//          case aDIQuery. of
//          end;
//          foundOP.propertytype := 'float';
//          foundOP.selection := '';
//          foundOP.fieldname := aDIQuery.datafieldname;
//          foundOP.editable := True;
//          foundOP.tablename := aDIQuery.tablename;
//          foundOP.keyfieldname := aDIQuery.idfieldname;
        end
        else
        begin // add
          foundOP := TDIObjectProperty.Create;
          try
            foundOP.category := aDIQuery.domain;
            foundOP.propertyname := aDIQuery.description;
  //        case aDIQuery. of
  //        end;
            foundOP.propertytype := 'float'; // todo: get from db?
            foundOP.selection := '';
            foundOP.fieldname := aDIQuery.datafieldname;
            foundOP.editable := True;
            foundOP.tablename := aDIQuery.tablename;
            foundOP.keyfieldname := aDIQuery.idfieldname;
          finally
            DIObjectProperties.Add(foundOP);
          end;
          // id is ommited and generated by db (sequence)
          (fDBConnection as TFDConnection).ExecSQL(
            'INSERT INTO '+ProjectSchemaName+'.di_objectproperties '+
              '(category, '+
               'propertyname, '+
               'propertytype, '+
               'selection, '+
               'fieldname, '+
               'editable, '+
               'tablename, '+
               'keyfieldname) '+
            'VALUES '+
              '('+sqlSN(foundOP.category)+','+
                  sqlSN(foundOP.propertyName)+','+
                  sqlSN(foundOP.propertyType)+','+
                  sqlSN(foundOP.selection)+','+
                  sqlSN(foundOP.fieldName)+','+
                  sqlB(foundOP.editable)+','+
                  sqlSN(foundOP.tableName)+','+
                  sqlSN(foundOP.keyFieldName)+')');
        end;
      end
      else if objProp is TJSONNull then
      begin
        if Assigned(foundOP) then
        begin // remmove
          DIObjectProperties.Remove(foundOP);
          (fDBConnection as TFDConnection).ExecSQL(
            'DELETE FROM '+ProjectSchemaName+'.di_objectproperties '+
            'WHERE tablename='''+aDIQuery.tablename.ToLower+''' and fieldname='''+aDataFieldName.ToLower+'''');
        end
        else Log.WriteLn('Object property not found on remove in layer definition: '+objProp.ToJSON, llWarning);
      end
      else Log.WriteLn('Could not process object property in layer definition: '+objProp.ToJSON, llWarning);
    end;
    *)
  end;

  procedure ProcessFileOnExt(const aPath, aFileExt, aSchemaName, aTableName: string);
  var
    shapeFile: TGIS_LayerSHP;
    shape: TGIS_Shape;
    projectedShape: TGIS_Shape;
    tempDataFolder: string;
    fileNames: TArray<string>;
    fn: string;
    idfni: Integer;
    fieldDefs: TObjectList<TFileInfoRecord>;
    lp: TArray<System.string>;
    Lines: TStringList;
    l: Integer;
    line: TStringList;
    f: Integer;
    fieldName: string;
    fieldType: TGIS_FieldType;
    sliceType: Integer;
    geometryType: string;
    fieldNameID: string;
    fieldNameGeometry: string;
    isGeometry: Boolean;
    layer: TLayerBase;
    DIQuery: TDIQuery;
    changed: Boolean;
    lon: Double;
    lat: Double;
    isLat: Boolean;
    WGS84Projection: TGIS_CSCoordinateSystem;
    layerDefinition: TJSONValue;
    removeID: string;
    layersJSONArray: TJSONArray;
    layerEntry: TJSONValue;
    cnt: Integer;
  begin
    if aFileExt='.layer' then
    begin
      try
        if DIQueries.TryGetValue(aTableName, DIQuery) then
        begin
          layerDefinition := TJSONObject.ParseJSONValue(TFile.ReadAllText(aPath));
          try
            if layerDefinition.TryGetValue<string>('remove', removeID) then
            begin
              RemoveLayer(removeID);
              DIQueries.Remove(removeID);
              aClient.SendMessage('Layer "'+removeID+'" removed', TMessageType.mtSucces, 3000);
              Refresh(aClient.currentScenario);
            end
            // array of layers
            else if layerDefinition.TryGetValue<TJSONArray>('layers', layersJSONArray) then
            begin
              aClient.SendMessage('Defining details layer for '+aTableName, TMessageType.mtNone, 3000);
              for layerEntry in layersJSONArray do
              begin
                if (layerEntry as TJSONObject).TryGetValue<string>('datafieldname', fieldname)
                then AddDetailsLayer(fieldName, DIQuery, layerEntry as TJSONObject)
                else Log.WriteLn('skipping incorrect layer definition in');
              end;
              aClient.SendMessage('Details layer added', TMessageType.mtSucces, 3000);
              Refresh(aClient.currentScenario);
            end
            // single layer
            else if layerDefinition.TryGetValue<string>('datafieldname', fieldname) then
            begin
              aClient.SendMessage('Defining details layer for '+aTableName, TMessageType.mtNone, 3000);
              AddDetailsLayer(fieldName, DIQuery, layerDefinition);
              aClient.SendMessage('Details layer added', TMessageType.mtSucces, 3000);
              Refresh(aClient.currentScenario);
            end
            else aClient.SendMessage('No data field name definedfor: '+aTableName, TMessageType.mtError, 5000);
          finally
            layerDefinition.Free;
          end;
        end
        else aClient.SendMessage('No basic layer defined for: '+aTableName, TMessageType.mtError, 5000);
      except
        on E: Exception do
        begin
          Log.WriteLn('Unhandled exception uploading layer file '+aPath+': '+e.Message, llError);
          aClient.SendMessage('Could not upload layer file: '+e.Message, TMessageType.mtError, 5000);
        end;
      end;
    end
    else
    begin
      idfni := -1;
      sliceType := -1;
      fieldNameID := 'id';
      fieldNameGeometry := DefaultFieldNameGeometry;
      fieldDefs := TObjectList<TFileInfoRecord>.Create;
      layer := nil;
      changed := False;
      try
        if aFileExt='.txt' then
        begin
          aClient.SendMessage('Uploading data in text file for '+aTableName+'..', TMessageType.mtNone, 3000);
          lines := TStringList.Create;
          line := TStringList.Create;
          try
            line.Delimiter := #9;
            line.StrictDelimiter := True;
            lines.LoadFromFile(aPath);
            // decode csv header
            line.DelimitedText := lines[0];
            //for f := 0 to line.Count-1 do
            f := 0;
            while f<line.Count do
            begin
              lp := line[f].Split([':']);
              for l := 0 to Length(lp)-1
              do lp[l] := lp[l].trim();

              if length(lp)=2 then
              begin
                handleColumn(lp, fieldDefs, isGeometry, fieldNameGeometry, f, idfni);
              end
              else
              begin
                if length(lp)>2 then
                begin
                  if lp[2]='-' then
                  begin
                    with handleColumn(lp, fieldDefs, isGeometry, fieldNameGeometry, f, idfni) do
                    begin
                      deleteFlag := True;
                    end;
                  end
                  else
                  begin
                    fieldDefs.Add(TFileInfoRecord.Create(line[f], FieldTypeName2Type(lp[1].ToLower), False, False));
                    Log.WriteLn('Upload text: '+aTableName+', invalid field def: '+line[f], llWarning);
                  end;
                end
                else
                begin
                  fieldDefs.Add(TFileInfoRecord.Create(lp[0], ftUnknown, False, False));
                  Log.WriteLn('Upload text: '+aTableName+', invalid field def: '+line[f], llError);
                end;
              end;
              f := f+1;
            end;
            // if no id column is found use first column
            if (idfni<0) and (line.Count>0)
            then idfni := 0;

            if idfni>=0
            then fieldNameID := fieldDefs[idfni].fieldName;

            // do fixup of table with column info from file
            FixupTable(aSchemaName, aTableName, idfni, fieldDefs);
            // decode csv file
            for l := 1 to lines.Count-1 do
            begin
              line.DelimitedText := lines[l];
              // to decode csv data line to field defs values
              f := 0;
              while f<line.Count do
              begin
                if fieldDefs[f].isGeometryField then
                begin
                  if fieldDefs[f].degreeConversion then
                  begin
                    lat := DecodeDegreeCoordinate(line[f], isLat);
                    lon := DecodeDegreeCoordinate(line[f+1], isLat);
                    fieldDefs[f].sqlFieldValue := 'ST_GeomFromGeoJSON(''{"type": "Point","coordinates": ['+lon.toString(dotFormat)+','+lat.toString(dotFormat)+']}'')';
                    sliceType := stLocation;
                    f := f+1; // skip lon field, already processed
                  end
                  else
                  begin
                    fieldDefs[f].sqlFieldValue := 'ST_GeomFromGeoJSON('''+line[f]+''')';
                    // todo: detect kind geometry to add basic layer for..
                    if sliceType<0 then
                    begin
                      if line[f].IndexOf(gtMultiLineString)>=0
                      then sliceType := stGeometryI
                      else if line[f].IndexOf(gtPolygon)>=0
                      then sliceType := stGeometry
                      else if line[f].IndexOf(gtPoint)>=0
                      then sliceType := stLocation;
                    end;
                  end;
                end
                else
                begin
                  case fieldDefs[f].fieldType of
                    ftWideMemo,
                    ftString,
                    ftWideString:
                      fieldDefs[f].sqlFieldValue := sqlSN(line[f]);
                    ftInteger,
                    ftLargeint:
                      fieldDefs[f].sqlFieldValue := line[f];
                    // todo: check all other types!
                    ftFloat,
                    ftExtended,
                    ftSingle:
                      fieldDefs[f].sqlFieldValue := line[f].Replace(',', '.'); // only dot format in SQL
                    ftBoolean:
                      fieldDefs[f].sqlFieldValue := line[f];
                    ftGuid:
                      fieldDefs[f].sqlFieldValue := line[f];
                    ftCurrency:
                      fieldDefs[f].sqlFieldValue := line[f];
                    ftTimeStamp:
                      fieldDefs[f].sqlFieldValue := sqlSN(line[f]);
                  else
                      fieldDefs[f].sqlFieldValue := 'null';
                  end;
                end;
                f := f+1;
              end;
              if not fieldDefs[idfni].deleteFlag
              then UpsertRow(aSchemaName, aTableName, idfni, fieldDefs)
              else DeleteRow(aSchemaName, aTableName, idfni, fieldDefs);
              changed := True;
            end;
          finally
            lines.Free;
            line.Free;
          end;
        end
        else if aFileExt='.zip' then
        begin
          // todo: unzip and process indiviual files
          tempDataFolder := IncludeTrailingPathDelimiter(GetTempDirectory)+TGUID.NewGuid.ToString;
          ForceDirectories(tempDataFolder);
          if UnZipFiles(aPath, tempDataFolder, fileNames) then
          begin
            for fn in fileNames do
            begin
              // skip folders
              if not (fn.EndsWith('\') or fn.EndsWith('/')) then
              begin
                // skip all files starting with a dot, the are prob. secondary stream files from max osx or linux
                l := fn.LastDelimiter('/\:');
                if fn.Substring(l+1,1)<>'.'
                then ProcessFileOnExt(tempDataFolder+'\'+fn, ExtractFileExt(fn).ToLower, aSchemaName, aTableName);
              end;
            end;
          end
          else
          begin
            log.WriteLn('Could not unzip uploaded file: '+aPath, llError);
            aClient.SendMessage('Could not unzip uploaded file', TMessageType.mtError, 5000);
          end;
        end
        // parse shp file, we expect dbf etc. to be there (from zip)
        else if aFileExt='.shp' then
        begin
          aClient.SendMessage('Uploading data in shape file for '+aTableName+'..', TMessageType.mtNone, 3000);
          shapeFile := TGIS_LayerSHP.Create;
          try
            // load shape file
            shapeFile.Name := 'uploaded shape file';
            shapeFile.Path := aPath;
            shapeFile.Open;

            // reprojection
            WGS84Projection := CSGeographicCoordinateSystemList.ByEPSG(4326);
            for f := 0 to shapeFile.Fields.Count-1 do
            begin
              fieldName := shapeFile.FieldInfo(f).Name;
              fieldType := shapeFile.FieldInfo(f).FieldType;
              // column is id field if name = 'id' or if no id field is defined and column ends on '_id'
              if (fieldName.ToLower='id') or ((idfni<0) and fieldName.ToLower.EndsWith('_id')) then
              begin
                idfni := f;
                fieldDefs.Add(TFileInfoRecord.Create(fieldName, TFileInfoRecord.GISFieldType2DBFieldType(fieldType), True, False));
              end
              else fieldDefs.Add(TFileInfoRecord.Create(fieldName, TFileInfoRecord.GISFieldType2DBFieldType(fieldType), False, False));
            end;
            // add the geometry field type as the last entry so shape field indexes match the fieldDefs indexes
            fieldDefs.Add(TFileInfoRecord.Create(fieldNameGeometry, ftString, False, True));
            // if no id column is found use first column
            if (idfni<0) and (shapeFile.Fields.Count>0)
            then idfni := 0;
            if idfni>=0
            then fieldNameID := fieldDefs[idfni].fieldName;
            // check table structure
            FixupTable(aSchemaName, aTableName, idfni, fieldDefs);
            // parse shapes in shape file
            shape := shapeFile.FindFirst(GisWholeWorld);
            cnt := 0;
            while Assigned(shape) do
            begin
              // shape geometry in json format, last fieldDefs entry is the geometry field
              // detect kind geometry to add basic layer for..
              // https://gis.stackexchange.com/questions/60928/how-to-insert-a-geojson-polygon-into-a-postgis-table
              projectedShape := shape.CreateCopyCS(WGS84Projection);
              try
                if projectedShape is TGIS_ShapePoint then
                begin
                  fieldDefs[fieldDefs.Count-1].sqlFieldValue := 'ST_GeomFromGeoJSON('''+TGIS_Utils.GisExportPointToJSON(projectedShape)+''')';
                  sliceType := stLocation;
                end
                else if projectedShape is TGIS_ShapeArc then
                begin
                  fieldDefs[fieldDefs.Count-1].sqlFieldValue := 'ST_GeomFromGeoJSON('''+TGIS_Utils.GisExportArcToJSON(projectedShape)+''')';
                  sliceType := stGeometryI;
                end
                else if projectedShape is TGIS_ShapePolygon then
                begin
                  fieldDefs[fieldDefs.Count-1].sqlFieldValue := 'ST_GeomFromGeoJSON('''+TGIS_Utils.GisExportPolygonToJSON(projectedShape)+''')';
                  sliceType := stGeometry;
                end
                else
                begin // unknown projectedShape type
                  // for now just also use polygon
                  fieldDefs[fieldDefs.Count-1].sqlFieldValue := 'ST_GeomFromGeoJSON('''+TGIS_Utils.GisExportPolygonToJSON(projectedShape)+''')';
                  sliceType := stGeometry;
                end;
              finally
                projectedShape.Free;
              end;
              // fields on shape
              for f := 0 to shapeFile.Fields.Count-1 do
              begin
                fieldName := shapeFile.FieldInfo(f).Name;
                fieldType := shapeFile.FieldInfo(f).FieldType;
                case fieldType of
                  TGIS_FieldType.String:
                    fieldDefs[f].sqlFieldValue := sqlSN(shape.GetField(fieldName));
                  TGIS_FieldType.Number:
                    fieldDefs[f].sqlFieldValue := sqlF(shape.GetField(fieldName));
                  TGIS_FieldType.Float:
                    fieldDefs[f].sqlFieldValue := sqlF(shape.GetField(fieldName));
                  TGIS_FieldType.Boolean:
                    fieldDefs[f].sqlFieldValue := sqlB(shape.GetField(fieldName));
                  TGIS_FieldType.Date:
                    fieldDefs[f].sqlFieldValue := 'null'; // todo: how to parse
                else
                    fieldDefs[f].sqlFieldValue := 'null';
                end;
              end;
              UpsertRow(aSchemaName, aTableName, idfni, fieldDefs);
              changed := True;
              shape := shapeFile.FindNext;
              cnt := cnt+1;
              Log.Progress('uploading shape '+cnt.ToString);
            end;
          finally
            shapeFile.Free;
          end;
        end;
      finally
        fieldDefs.Free;
      end;
      if Changed then
      begin
        if not DIQueries.ContainsKey(aTableName) then
        begin
          case sliceType of
            stLocation:
              geometryType := gtPoint;
            stGeometryI:
              geometryType := gtMultiLineString;
          else
            geometryType := gtPolygon;
          end;
          Log.WriteLn('Adding basic layer definition for '+aSchemaName+'.'+aTableName);
          try
            DIQuery.id := aTableName;
            DIQuery.domain := 'Basic structures';
            DIQuery.name := aTableName;
            DIQuery.description := 'Basic '+aTableName;
            DIQuery.objecttypes := '"'+aTableName+'"';
            DIQuery.geometrytype := geometryType;
            DIQuery.defaultload := 0;
            DIQuery.basiclayer := 1;
            DIQuery.sql := '';
            DIQuery.tablename := '"'+aTableName+'"'; // normally " is not needed
            DIQuery.idfieldname := fieldNameID;
            DIQuery.geometryfieldname := fieldNameGeometry;
            DIQuery.datafieldname := '';
            DIQuery.layertype := sliceType;
            DIQuery.palettejson := '';
            DIQuery.legendjson := '';
            // first create entry in database
            (fDBConnection as TFDConnection).ExecSQL(
              'INSERT INTO '+ProjectSchemaName+'.di_queries ('+
                'id, domain, name, description, objecttypes, geometrytype, defaultload, '+
                'basiclayer, sql, tablename, idfieldname, geometryfieldname, datafieldname,'+
                'layertype, palettejson, legendjson) '+
              'VALUES ('+
                sqlSN(DIQuery.id)+','+sqlSN(DIQuery.domain)+','+sqlSN(DIQuery.name)+','+sqlSN(DIQuery.description)+','+sqlSN(DIQuery.objecttypes)+','+sqlSN(DIQuery.geometrytype)+','+sqlI(DIQuery.defaultload)+','+
                sqlI(DIQuery.basiclayer)+','+sqlSN(DIQuery.sql)+','+sqlSN(DIQuery.tablename)+','+sqlSN(DIQuery.idfieldname)+','+sqlSN(DIQuery.geometryfieldname)+','+sqlSN(DIQuery.datafieldname)+','+
                sqlI(sliceType)+','+sqlSN(DIQuery.palettejson)+','+sqlSN(DIQuery.legendjson)+')');
            // then add to internal list
            DIQueries.Add(aTableName, DIQuery);
          except
            on E: Exception
            do Log.WriteLn('Exception adding basic layer def to diqueries for '+aTableName+': '+E.Message, llError);
          end;
          addDownloadableFile(aTableName, [udftShape, udftText, udftLayer], GenerateDownloadableFile);
          Refresh(aClient.currentScenario);
        end
        else
        begin
          // basic layer defined so just lookup and send refresh
          if (aClient.currentScenario as TEcodistrictScenario).Layers.TryGetValue(aTableName, layer) then
          begin
            (aClient.currentScenario as TEcodistrictScenario).ReadObjectFromQuery(layer as TLayer);
            (layer as TLayer).signalObjects(nil); // send object to tiler which will trigger refresh
          end;
        end;
      end;
    end;
  end;

  function PathToTableName(aPath: string): string;
  var
    i: Integer;
  begin
    Result := '';
    // extract only file name part, in lower case
    aPath := ChangeFileExt(ExtractFileName(aPath), '').toLower;
    // only use chars that are letters or numbers
    for i := 1 to Length(aPath) do
    begin
      if CharInSet(aPath[i],['a'..'z', '0'..'9'])
      then Result := Result+aPath[i];
    end;
  end;

var
  schemaName: string;
  tableName: string;
  fileExt: string;
begin
  try
    if Assigned(aClient.currentScenario)
    then schemaName := (aClient.currentScenario as TEcodistrictScenario).ScenarioSchemaName
    else schemaName := ProjectSchemaName;
    tableName := PathToTableName(aFileInfo.fileName);
    fileExt := ExtractFileExt(aFileInfo.fileName).toLower;
    if (fileExt='.txt') or (fileExt='.zip') or (fileExt='.layer') then
    begin
      ProcessFileOnExt(aFileInfo.path, fileExt, schemaName, tableName);
      aClient.SendMessage('Parsed uploaded file '+aFileInfo.fileName, TMessageType.mtSucces, 3000);
    end
    else aClient.SendMessage('Found file extension '+fileExt+', only .txt and .zip are supported..', TMessageType.mtError, 5000);
  except
    on E: Exception do
    begin
      log.WriteLn('Unhandled exception uploading file '+aFileInfo.fileName+': '+E.Message, llError);
      aClient.SendMessage('Could not process uploaded file '+aFileInfo.fileName+': '+E.Message, TMessageType.mtError, 5000);
    end;
  end;
end;

function TEcodistrictProject.handleTilerStatus(aTiler: TTiler): string;
begin
  // handle status request
  Result := 'project '+projectName+' ('+projectID+')';
end;

procedure TEcodistrictProject.handleTypedClientMessage(aClient: TClient; const aMessageType: string; var aJSONObject: TJSONObject);
var
  payload: TJSONObject;
//  jsonPair: TJSONPair;
  dictScenariosID: string;
  scenario: TScenario;
//  jsonMeasures: TJSONArray;
//  jsonMeasure: TJSONValue;
//  measureId: string;
//  categories: string;
//  jsonObjectIDs: TJSONArray;
//  jsonObjectID: TJSONValue;
//  sql: string;
//  objectIDs: string;
//  projectSchema: string;
//  scenarioSchema: string;
//  mh: TDIMeasureHistory;
//  jsonMeasureCategories: TJSONArray;
//  jsonCategory: TJSONValue;
//  measure: TJSONObject;
//  jsonApplyObjectsProperties: TJSONObject;
//  jsonSelectedCategories: TJSONArray;
//  tableName: string;
//  jsonProperties: TJSONArray;
//  jsonProperty: TJSONValue;
//  properties: string;
//  op: TDIObjectProperty;
//  propName: string;
//  jsonValue: TJSONValue;
//  keyFieldName: string;
//  propFieldName: string;
//  propValue: string;
begin
  if aJSONObject.TryGetValue<TJSONObject>('payload', payload) then
  begin
    (*
    if aJSONObject.TryGetValue<TJSONArray>('applyMeasures', jsonMeasures) then
    begin
      Log.WriteLn('applyMeasures..');
      for jsonMeasure in jsonMeasures do
      begin
        measureId := (jsonMeasure as TJSONObject).GetValue<string>('measure.id');
        measure := (jsonMeasure as TJSONObject).GetValue<TJSONObject>('measure');
        // todo: should always be 1 category (for now)
        jsonObjectIDs := (jsonMeasure as TJSONObject).GetValue<TJSONArray>('selectedObjects');
        objectIDs := '';
        for jsonObjectID in jsonObjectIDs do
        begin
          if objectIDs<>''
          then objectIDs := objectIDs+',';
          objectIDs := objectIDs+jsonObjectID.ToJSON;
        end;
        jsonMeasureCategories :=(jsonMeasure as TJSONObject).GetValue<TJSONArray>('selectCategories');
        categories := '';
        for jsonCategory in jsonMeasureCategories do
        begin
          if categories<>''
          then categories := categories+',';
          categories := categories+jsonCategory.ToJSON;
        end;
        projectSchema := EcoDistrictSchemaId(projectID);
        if Assigned(aScenario) and (aScenario.ID<>projectID)
        then scenarioSchema := EcoDistrictSchemaId(projectID, aScenario.ID)
        else scenarioSchema := projectSchema;
        // todo: switch to DIMeasures in project
        sql := FDReadJSON(
          fDBConnection as TFDConnection,
          'SELECT query '+
          'FROM '+projectSchema+'.di_measures '+
          'WHERE cat||id='''+measureId+'''').Replace('{case_id}', scenarioSchema).Replace('{ids}', objectIDs.Replace('"', ''''));
        if sql<>'' then
        begin
          // todo: check for sql injection -> fix
          (fDBConnection as TFDConnection).ExecSQL(sql);
          Log.WriteLn('Applied measure '+measureId+' ('+categories+'): '+sql);
          // add measure to history
          mh.id := TGUID.NewGuid;
          mh.measure := measure.ToJSON;
          mh.object_ids := objectIDs;
          mh.timeutc := Now; // todo: convert to utc?
          mh.variants := scenarioSchema;
          mh.categories := categories;
          AddDIMeasureHistory(mh);
        end
        else Log.WriteLn('Could not build SQL to apply measure '+measureId+' ('+categories+'): '+objectIDs);
      end;
    end
    // todo: renamed scenarioRefresh to new format: code still to be changed!
    else
    *)
    if aMessageType='scenarioRefresh' then
    begin
      // determine id used for dictionary
      if payload.TryGetValue<string>('scenario', dictScenariosID) then
      begin
       if (dictScenariosID='') or (dictScenariosID='null') or (dictScenariosID='None') or (dictScenariosID=ProjectID)
       then dictScenariosID := EcodistrictBaseScenario;
        if scenarios.TryGetValue(dictScenariosID, scenario) then
        begin
          scenario.ReadBasicData;
          Log.WriteLn('refreshed scenario '+dictScenariosID);
        end
        else Log.WriteLn('scenario '+dictScenariosID+' not found to refresh', llWarning);
      end;
    end;
    (*
    else if aJSONObject.TryGetValue<TJSONObject>('applyObjectsProperties', jsonApplyObjectsProperties) then
    begin
      Log.WriteLn('Apply object properties');
      jsonSelectedCategories := jsonApplyObjectsProperties.GetValue<TJSONArray>('selectedCategories');
      if jsonSelectedCategories.Count=1 then
      begin
        tableName := jsonSelectedCategories.Items[0].Value;

        jsonProperties := jsonApplyObjectsProperties.GetValue<TJSONArray>('properties');
        properties := '';
        for jsonProperty in jsonProperties do
        begin
          propName := jsonProperty.GetValue<string>('name');

          jsonValue := jsonProperty.GetValue<TJSONValue>('value');
          if not (jsonValue is TJSONNull) then
          begin
            if (jsonValue is TJSONNumber)
            then propValue := (jsonValue as TJSONNumber).AsDouble.ToString(dotFormat)
            else propValue := ''''+SafeSQLValue(jsonValue.Value, False)+'''';
          end
          else propValue := 'null';

          propFieldName := '';
          for op in DIObjectProperties do
          begin
            if (op.tableName=tableName) and (op.propertyName=propName) then
            begin
              propFieldName := op.fieldName;
              keyFieldName := op.keyFieldName;
              break;
            end;
          end;
          if propFieldName<>'' then
          begin
            if properties<>''
            then properties := properties+',';
            properties := properties+SafeSQLValue(propFieldName, True)+'='+propValue;
          end;
        end;

        // todo: split in batches to avoid to many object ID's in SQL " WHERE .. IN (..)"

        jsonObjectIDs := jsonApplyObjectsProperties.GetValue<TJSONArray>('selectedObjects');
        objectIDs := '';
        for jsonObjectID in jsonObjectIDs do
        begin
          if objectIDs<>''
          then objectIDs := objectIDs+',';
          objectIDs := objectIDs+SafeSQLValue(jsonObjectID.ToJSON, True);
        end;

        projectSchema := EcoDistrictSchemaId(projectID);
        if Assigned(aScenario) and (aScenario.ID<>projectID)
        then scenarioSchema := EcoDistrictSchemaId(projectID, aScenario.ID)
        else scenarioSchema := projectSchema;

        sql := 'UPDATE '+scenarioSchema+'.'+tableName+' SET '+properties+' WHERE '+keyFieldName+' IN ('+objectIDs.Replace('"', '''')+')';

        Log.WriteLn(sql);


        // todo: execute sql


      end
      else Log.WriteLn('Apply object properties: selectedCategories.Count='+jsonSelectedCategories.Count.ToString+' (<>1)', llWarning);
    end
    else
    begin
      if Assigned(aScenario)
      then Log.WriteLn('Unhandled client event for scenario '+aScenario.elementID+': '+aJSONObject.ToJSON, llWarning)
      else Log.WriteLn('Unhandled client event for base scenario: '+aJSONObject.ToJSON, llWarning);
    end;
    *)
  end;
  inherited;
end;

function TEcodistrictProject.PingDatabase(const aCaller: string): Boolean;
begin
  Result := (fDBConnection as TFDConnection).Ping;
  if not Result
  then Log.WriteLn(aCaller+': ping of database returned false', llError);
end;

procedure TEcodistrictProject.ReadBasicData;
var
  schemaNames: TArray<string>;
  schemaName: string;
begin
  schemaNames := ReadSchemaNames();
  for schemaName in schemaNames
  do readScenario(schemaName);
end;

function TEcodistrictProject.ReadDIMeasures: Boolean;
var
  query: TFDQuery;
  m: TDIMeasure;
begin
  fDIMeasures.Free;
  fDIMeasures := TDictionary<string, TDIMeasure>.Create;
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT cat||id as id, category, typology as measure, application as action, objecttype as objecttypes, benefits as description, query '+
      'FROM '+ProjectSchemaName+'.di_measures';
    try
      query.Open();
      query.First();
      while not query.Eof do
      begin
        m.id := query.Fields[0].AsString; //cat||id as id, application as action, objecttype as objecttypes, benefits as description
        m.category  := query.Fields[1].AsString;
        m.measure  := query.Fields[2].AsString;
        m.action  := query.Fields[3].AsString;
        m.objecttypes  := query.Fields[4].AsString;
        m.description  := query.Fields[5].AsString;
        m.query  := query.Fields[6].AsString;
        fDIMeasures.Add(m.id, m);
        query.Next();
      end;
      Result := True;
    except
      on e: exception do
      begin
        log.WriteLn('exception in TEcodistrictProject.ReadDIMeasures: '+e.Message, llError);
        Result := False;
      end;
    end;
  finally
    query.Free;
  end;
end;

function TEcodistrictProject.ReadDIMeasuresHistory: Boolean;
var
  query: TFDQuery;
  mh: TDIMeasureHistory;
begin
  fDIMeasuresHistory.Free;
  fDIMeasuresHistory := TObjectDictionary<TGUID, TDIMeasureHistory>.Create;
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT id, measure, object_ids, timeutc, variants, categories '+
      'FROM '+ProjectSchemaName+'.di_measuresHistory';
    try
      query.Open();
      query.First();
      while not query.Eof do
      begin
        mh.id := TGUID.Create(query.Fields[0].AsString);
        mh.measure := query.Fields[1].AsString;
        mh.object_ids := query.Fields[2].AsString;
        mh.timeutc := query.Fields[3].AsDateTime;
        mh.variants := query.Fields[4].AsString;
        mh.categories := query.Fields[5].AsString;
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
      'FROM '+ProjectSchemaName+'.di_objectproperties '+
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
			'FROM '+ProjectSchemaName+'.di_queries '+
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
      'SELECT returntype, request, query, module '+
			'FROM '+ProjectSchemaName+'.dm_queries';
    try
      query.open();
      query.First;
      while not query.Eof do
      begin
        DMQuery.module:=query.Fields[3].AsString;
        DMQuery.SQL:=query.Fields[2].AsString;
        DMQuery.ReturnType:=query.Fields[0].AsString;
        fDMQueries.Add(query.Fields[1].AsString,DMQuery);
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
      if not Assigned(projectCurrentScenario)
      then projectCurrentScenario := Result;
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

  projectSchema := ProjectSchemaName;

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

function TEcodistrictProject.ProjectSchemaName: string;
begin
  Result := EcoDistrictSchemaId(projectID);
end;

procedure TEcodistrictProject.UpdateKPIList(aKPIList: TObjectList<TEcodistrictKPI>);
var
  kpi: TEcodistrictKPI;
  ikp: TPair<string, TEcodistrictKPI>;
begin
  if Assigned(aKPIList) then
  begin
    // add to local dictionary
    for kpi in aKPIList
    do fKpiList.AddOrSetValue(kpi.id, kpi);
    // extract from parameter list
    for ikp in fKpiList
    do aKPIList.Extract(ikp.Value);
  end;
end;

{ TEcodistrictKPI }

constructor TEcodistrictKPI.Create(const aId, aName: string; aSufficient, aExcellent: Double);
begin
  inherited Create;
  fId := aId;
  fName := aName;
  fSufficient := aSufficient;
  fExcellent := aExcellent;
  // calculate bad (for now same distance from sufficient as excellent distance (other side)
  fBad := fSufficient-(fExcellent-fSufficient);
end;

{ TEcodistrictModule }

constructor TEcodistrictModule.Create(aSessionModel: TSessionModel; aConnection: TConnection;
  const aConnectString, aTilerFQDN, aTilerStatusURL: string; aMaxNearestObjectDistanceInMeters: Integer;
  aDoNotListenToDataEvents, aDoNotListenToModuleEvents, aDoNotListenToCaseVariantManagementEvents: Boolean);
begin
  Log.WriteLn('Using temp folder: '+GetTempDirectory);
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
  fDashboardEvent := fConnection.eventEntry('ecodistrict.dashboard', False).publish;

  if not aDoNotListenToDataEvents then
  begin
    fDataEvent := fConnection.eventEntry('ecodistrict.data', False).subscribe; // auto publish
    fDataEvent.OnString.Add(HandleDataEvent);
  end;

  if not aDoNotListenToModuleEvents then
  begin
    fModuleEvent := fConnection.eventEntry('ecodistrict.modules', False).subscribe;
    fModuleEvent.OnString.Add(HandleModuleEvent);
  end;

  if not aDoNotListenToCaseVariantManagementEvents then
  begin
    fCaseVariantManagementEvent := fConnection.eventEntry('ecodistrict.'+CaseVariantManagementReturnEventName, False).subscribe;
    fCaseVariantManagementEvent.OnString.Add(HandleCaseVariantManagentEvent);
  end;
end;

destructor TEcodistrictModule.Destroy;
begin
  // todo:
  FreeAndNil(fProjects);
  inherited;
end;

function TEcodistrictModule.forceReadOfDIMeasures(const aCaseId: string): Boolean;
var
  project: TProject;
  isp: TPair<string, TScenario>;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    Result := (project as TEcodistrictProject).ReadDIMeasures;
    for isp in project.scenarios do
    begin
      isp.Value.forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          // todo: update measures? aClient.SendDomains('updatedomains');
        end);
    end;
  end
  else Result := False;
end;

function TEcodistrictModule.forceReadOfDIMeasuresHistory(const aCaseId: string): Boolean;
var
  project: TProject;
  isp: TPair<string, TScenario>;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    Result := (project as TEcodistrictProject).ReadDIMeasuresHistory;
    // signal connected clients of domains update
    for isp in project.scenarios do
    begin
      isp.Value.forEachSubscriber<TClient>(
        procedure(aClient: TClient)
        begin
          project.SendDomains(aClient, 'updatedomains');
        end);
    end;
  end
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
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    Result := (project as TEcodistrictProject).ReadDIQueries;
    // todo: signal connected clients of domains update
  end
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

function TEcodistrictModule.GetOrAddCase(const aCaseId: string; aKPIList: TObjectList<TEcodistrictKPI>): TProject;
var
  dbConnection: TCustomConnection;
begin
  // load the case as project
  if not fProjects.TryGetValue(aCaseId, Result) then
  begin
    InitPG;
    dbConnection := TFDConnection.Create(nil);
    SetPGConnection(dbConnection as TFDConnection, fConnectString);
    Result := TEcodistrictProject.Create(fSessionModel, fSessionModel.Connection, aCaseID, '', fTilerFQDN, fTilerStatusURL, dbConnection, True, fMaxNearestObjectDistanceInMeters, aKPIList);
    fSessionModel.Projects.Add(Result);

    fProjects.Add(aCaseId, Result);
  end
  else
  begin
    if Assigned(aKPIList)
    then (Result as TEcodistrictProject).UpdateKPIList(aKPIList);
  end;
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

function TEcodistrictModule.getNumberOfBuildings(const aSchemaName: string): Integer;
var
  query: TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := fDBConnection as TFDConnection;
    query.SQL.Text :=
      'SELECT Count(*) '+
			'FROM '+aSchemaName+'.building ';
    query.open();
    query.First;
    try
      if not query.Eof
      then Result := query.Fields[0].AsInteger
      else Result := -1;
    except
      on E: Exception do
      begin
        Log.WriteLn('Exception getting number of buildings for status from '+aSchemaName+': '+E.Message, llError);
        Result := -2;
      end;
    end;
  finally
    query.Free;
  end;

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
    query.SQL.Text := 'SELECT         clone_schema('''+aFromSchemaName+''', '''+aSchemaName+''',TRUE);';
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
  sql: string;
  geojsonObjectAsString: string;
  queries: TDictionary<string, TDMQuery>;
  rowObject: TJSONObject;
  f: TField;
  first: Boolean;
  geojsonObject: TJSONObject;
  propertiesObject: TJSONObject;
  geometryObject: TJSONValue;
  _data: TJSONObject;
  tableName: string;
  idField: string;
  entries: TJSONArray;
  i: Integer;
  entry: TJSONObject;
  p: Integer;
  name: string;
  value: string;
  id: string;
  schema: string;
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
          DataEvent:=fConnection.eventEntry('ecodistrict.' + _eventId, false).publish;
          jsonResponse.AddPair('status','<undefined>');
          // process all 'method's
          if _method='createCase' then
          begin
            if not ((_caseId = 'null') or (_caseId = '')) then
            begin
              if (_variantId='') or (_variantId='null') or (_variantId='None') then
              begin
                if SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
                begin
                  _status := 'Success - schema already created before';
                  // add numer of buildings
                  //_status := _status+' (buildings: '+getNumberOfBuildings(EcoDistrictSchemaId(_caseId, _variantId)).toString+')';
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
          else if _method='deleteCase' then
          begin
            if not ((_caseId = 'null') or (_caseId = '')) then
            begin
              if (_variantId='') or (_variantId='null') or (_variantId='None') then
              begin
                if not SchemaExists(EcoDistrictSchemaId(_caseId)) then
                begin
                  _status := 'Success - schema already deleted before';
                end
                else
                begin
                  _status := 'In progress - deleting cascading schemas';
                  jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
                  DataEvent.signalString(JSONresponse.ToString);
                  SchemaDelete(EcoDistrictSchemaId(_caseId));
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
          else if _method='createVariant' then
          begin
            if not ((_caseId = 'null') or (_caseId = '')) then
            begin
              if not ((_variantId='') or (_variantId='null') or (_variantId='None')) then
              begin
                if SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
                begin
                  _status := 'Success - schema already created before';
                  // add numer of buildings
                  _status := _status+' (buildings: '+getNumberOfBuildings(EcoDistrictSchemaId(_caseId, _variantId)).toString+')';
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
                // module: new variant but we do not know the title, description
                try
                  _variantName  := jsonObject.getValue<string>('name');
                  _variantDescription := jsonObject.getValue<string>('description');
                  HandleModuleVariant(_caseId, _variantId, _variantName, _variantDescription);
                except
                  on e: Exception
                  do Log.WriteLn('TEcodistrictModule.HandleDataEvent: exception handling module part of creating a variant: '+e.Message, llError);
                end;
                // signal getVariants to get the module part up-to-date
                //fDashboardEvent.signalString('{"method": "getVariants", "type": "request", "userId": "'+_userId+'", "caseId": "'+_caseId+'"}');
              end
              else _status := 'failed - no variant id';
            end
            else _status := 'failed - no case id';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
            Log.WriteLn(_status);
          end
          else if _method='deleteVariant' then
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
          else if _method='getData' then
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
                        try
                          if queries.TryGetValue(datafield, dataquery) then
                          begin
                            if (dataquery.module.Contains(_moduleId)) then
                            begin
                              // todo: rebuild to {case_id}
                              // todo: first update dm_queries to always use {case_id} (not all are adjusted!)
                              (fDBConnection as TFDConnection).ExecSQL('SET SCHEMA ''' + EcoDistrictSchemaId(_caseId, _variantId) + '''');
                              try
                                query := TFDQuery.Create(nil);
                                try
                                  Log.WriteLn(datafield, llNormal, 1);
                                  query.Connection := fDBConnection as TFDConnection;
                                  sql:= dataquery.SQL.Replace('{case_id}', EcoDistrictSchemaId(_caseId));
                                  Log.WriteLn(sql, llNormal, 1);
                                  query.SQL.Text := sql;
                                  query.Open();
                                  try
                                    query.First;
                                    if dataquery.ReturnType='INT' then
                                    begin
                                      if not query.Eof
                                      then jsonDataResponse.AddPair(datafield, TJSONNumber.Create(query.Fields[0].AsInteger));
                                    end
                                    else if dataquery.ReturnType='FLOAT' then
                                    begin
                                      if not query.Eof
                                      then jsonDataResponse.AddPair(datafield, TJSONNumber.Create(query.Fields[0].AsFloat));
                                    end
                                    else if dataquery.ReturnType='GEOJSON' then
                                    begin
                                      // we expect the query to return: a geojson object as text
                                      if not query.Eof then
                                      begin
                                        geojsonObjectAsString := query.Fields[0].AsString;
                                        jsonDataResponse.AddPair(datafield, TJSONObject.ParseJSONValue(geojsonObjectAsString));
                                      end;
                                    end
                                    else if dataquery.ReturnType='LIST' then // specific for citygml structure, see: TABLE
                                    begin
                                      jsonList:=TJSONObject.Create;
                                      try
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
                                          query.Next;
                                        end;
                                      finally
                                        jsonDataResponse.AddPair(datafield,jsonList);
                                      end;
                                    end
                                    else if dataquery.ReturnType='TABLE' then // specific for flat tables, see: LIST
                                    begin
                                      jsonArray := TJSONArray.Create;
                                      try
                                        while not query.Eof do

                                        begin
                                          rowObject := TJSONObject.Create;
                                          try
                                            // convert all fields to json object
                                            for f in query.fields do
                                            begin
                                              if not f.IsNull then
                                              begin
                                                case f.DataType of
                                                  ftWideMemo,
                                                  ftString,
                                                  ftWideString:
                                                    rowObject.AddPair(f.FieldName, TJSONString.Create(f.AsString));
                                                  ftInteger:
                                                    rowObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsInteger));
                                                  ftLargeint:
                                                    rowObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsLargeInt));
                                                  ftFloat,
                                                  ftExtended,
                                                  ftSingle:
                                                    rowObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsFloat));
                                                  ftBoolean:
                                                    rowObject.AddPair(f.FieldName, TJSONBool.Create(f.AsBoolean));
                                                else
                                                  Log.WriteLn('Unsupported DataType in getData TABLE request: '+Ord(f.DataType).toString, llWarning);
                                                end;
                                              end
                                              else rowObject.AddPair(f.FieldName, TJSONNull.Create);
                                            end;
                                          finally
                                            jsonArray.Add(rowObject);
                                          end;
                                          query.Next;
                                        end;
                                      finally
                                        jsonDataResponse.AddPair(datafield,jsonArray);
                                      end;
                                    end
                                    else if dataquery.ReturnType='GEOJSONFLAT' then
                                    begin // geojson as string
                                      jsonArray := TJSONArray.Create;
                                      try

                                        while not query.Eof do
                                        begin
                                          propertiesObject := TJSONObject.Create;
                                          geometryObject := nil;
                                          try
                                            // convert all fields to GEOJSON object
                                            // assume first object is geometry
                                            first := True;
                                            for f in query.fields do
                                            begin
                                              if first then
                                              begin
                                                geometryObject := TJSONObject.ParseJSONValue(f.AsString);
                                                first := False;
                                              end
                                              else
                                              begin
                                                if not f.IsNull then
                                                begin
                                                  case f.DataType of
                                                    ftWideMemo,
                                                    ftString,
                                                    ftWideString:
                                                      propertiesObject.AddPair(f.FieldName, TJSONString.Create(f.AsString));
                                                    ftInteger:
                                                      propertiesObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsInteger));
                                                    ftLargeint:
                                                      propertiesObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsLargeInt));
                                                    ftFloat,
                                                    ftExtended,
                                                    ftSingle:
                                                      propertiesObject.AddPair(f.FieldName, TJSONNumber.Create(f.AsFloat));
                                                  end;
                                                end
                                                else propertiesObject.AddPair(f.FieldName, TJSONNull.Create);
                                              end;
                                            end;
                                          finally
                                            geojsonObject := TJSONObject.Create;
                                            try
                                              geojsonObject.AddPair('type', 'Feature');
                                              if assigned(geometryObject)
                                              then geojsonObject.AddPair('geometry', geometryObject);
                                              geojsonObject.AddPair('properties', propertiesObject);
                                            finally
                                              jsonArray.Add(geojsonObject);
                                            end;
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
                        except
                          on E: Exception do
                          begin
                            _status := 'failed - exception during query execution: '+e.Message;
                            Log.WriteLn('Exception in TEcodistrictModule.HandleDataEvent during getData execution: '+E.Message, llError);
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
          else if _method='setData' then
          begin
            // json structure
            (*
              {
                "type": "request",
                "method": "setData",
                "caseId": "<caseId>",
                "variantId": "<variantId>",
                "moduleId": "<moduleId>",
                "userId": "<userId>",
                "eventId": "data-to-dashboard"
                "data": {
                   "type": "building",
                   "idField": "building_id",
                   "entries": [
                      {
                        "building_id": "1A",
                        "cost": 245.9
                      },
                      {
                        "building_id": "2F",
                        "cost": 9.0,
                        "levels": 5
                      }
                   ]
              }
            *)
            if jsonObject.TryGetValue<TJSONObject>('data', _data) then
            begin
              if _data.TryGetValue<string>('type', tableName) then
              begin
                if _data.TryGetValue<string>('idField', idField) then
                begin
                  if _data.TryGetValue<TJSONArray>('entries', entries) then
                  begin
                    if not SchemaExists(EcoDistrictSchemaId(_caseId, _variantId)) then
                  	begin
                      _status := 'failed - no schema found for case and variant';
                    end
                    else
                    begin
                      _status := 'Success';
                      try
                        for i := 0 to entries.Count-1 do
                        begin
                          entry := entries.Items[i] as TJSONObject;
                          sql := '';
                          id := '';
                          // process all pairs (except [<idField>]) of data object and build query for update
                          for p := 0 to entry.count-1 do
                          begin
                            name := entry.Pairs[p].JsonString.Value;
                            // use '' around value if text
                            if (entry.Pairs[p].JsonValue is TJSONNumber)
                            then value := (entry.Pairs[p].JsonValue as TJSONNumber).AsDouble.ToString(dotFormat)
                            else value := ''''+entry.Pairs[p].JsonValue.Value+'''';
                            if name=idField
                            then id := value
                            else
                            begin
                              if sql<>''
                              then sql := sql+',';
                              sql := sql+name+'='+value;
                            end;
                          end;
                          if (id<>'') and (sql<>'') then
                          begin
                            // update table <tableName> on key [<idField>]
                            sql :=
                              'UPDATE '+EcoDistrictSchemaId(_caseId, _variantId)+'.'+tableName+' '+
                              'SET '+sql+' '+
                              'WHERE '+idField+'='+id;
                            // execute query
                            (fDBConnection as TFDConnection).ExecSQL(sql);
                          end
                          else
                          begin
                            if id<>''
                            then Log.WriteLn('setData: no field updates found', llWarning)
                            else if sql<>''
                            then Log.WriteLn('setData: no id specified', llWarning)
                            else Log.WriteLn('setData: no id or field updates specified', llWarning);
                          end;
                        end;
                      except
                        on E: Exception do
                        begin
                          _status := 'failed - exception during execution: '+e.Message;
                          Log.WriteLn('Exception in TEcodistrictModule.HandleDataEvent during setData execution: '+E.Message, llError);
                        end;
                      end;

                    end;
                  end
                  else _status := 'failed - no entries specified to update '+tableName+' with';
                end
                else _status := 'failed - no idField specified to update '+tableName+' with';
              end
              else _status := 'failed - no type specified to update';
            end
            else _status := 'failed - no data specified to update';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
            Log.WriteLn(_status);
          end
          else if _method='setKpiResult' then
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
                      try
                        schema := EcoDistrictSchemaId(_caseId, _variantId);
                        jsonIterator :=_kpiValueList.GetEnumerator;
                        while jsonIterator.MoveNext do
                        begin
                          jsonKpi := jsonIterator.Current;
                          _kpi_type := CheckSQLValue(jsonKpi.GetValue<string>('type', 'None'));
                          _gml_id := CheckSQLValue(jsonKpi.GetValue<string>('gml_id', 'None'));
                          _kpi_value := jsonKpi.GetValue<double>('kpiValue', 0);

                          (fDBConnection as TFDConnection).ExecSQL(
                            'DELETE '+
                            'FROM '+schema+'.kpi_results '+
                            'WHERE kpi_type='''+_kpi_type+''' AND gml_id='''+_gml_id+''' AND kpi_id='''+_kpiId+''';');
                          // SQL insert into kpi_results (kpi_type, kpi_id, gml_id, kpi_value) values ()
                          (fDBConnection as TFDConnection).ExecSQL(
                            'INSERT INTO '+schema+'.kpi_results (kpi_type, kpi_id, gml_id, kpi_value) '+
                            'VALUES ('''+_kpi_type+''', '''+_kpiId+''', '''+_gml_id+''','+_kpi_value.ToString(dotFormat)+');');
                        end;
                        _status := 'Success - data added to the database';
                      except
                        on E: Exception do
                        begin
                          Log.WriteLn('exception in TEcodistrictModule.HandleDataEvent, setKpiResult ('+schema+') for '+_kpiId+': '+e.Message, llError);
                          _status := 'failed - exception '+E.Message;
                        end;
                      end;
                      // signal refresh data
                      HandleModuleScenarioRefresh(_caseId, _variantId);
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
          else if _method='getKpiResult' then
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
                    try
                      schema := EcoDistrictSchemaId(_caseId, _variantId);
                      jsonArray:=TJSONArray.Create; // will be owned by jsonResponse
                      try
                        query := TFDQuery.Create(nil);
                        try
                          query.Connection := fDBConnection as TFDConnection;
                          query.SQL.Text :=
                            'SELECT id, kpi_id, gml_id, kpi_value, kpi_type '+
                            'FROM '+schema+'.kpi_results '+
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
                      _status := 'Success';
                    except
                      on E: Exception do
                      begin
                        Log.WriteLn('exception in TEcodistrictModule.HandleDataEvent, getKpiResult ('+schema+') for '+_kpiId+': '+e.Message, llError);
                        _status := 'failed - exception '+E.Message;
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
          else if _method='readDMQueries' then
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
          else if _method='readDIObjectProperties' then
          begin
            if forceReadOfDIObjectProperties(_caseId)
            then _status := 'Success - read object properties'
            else _status := 'failed - case not loaded to read di-object-properties for or no properties found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else if _method='readDIMeasures' then
          begin
            if forceReadOfDIMeasures(_caseId)
            then _status := 'Success - read measures'
            else _status := 'failed - case not loaded to read di-measures for or no measures found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else if _method='readDIMeasuresHistory' then
          begin
            if forceReadOfDIMeasuresHistory(_caseId)
            then _status := 'Success - read measures history'
            else _status := 'failed - case not loaded to read di-measures-history for or no history found';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
          end
          else if _method='refresh' then
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
            _status := 'failed - unknown request';
            jsonResponse.get('status').JsonValue:= TJSONString.Create(_status);
            DataEvent.signalString(JSONresponse.ToString);
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

function TEcodistrictModule.HandleModuleCase(const aCaseId, aCaseTitle,  aCaseDescription: string; const aMapView: TMapView; aKPIList: TObjectList<TEcodistrictKPI>): TProject;
var
  scenario: TScenario;
begin
  result := GetOrAddCase(aCaseId, aKPIList);
  result.ProjectName := aCaseTitle;
  result.projectDescription := aCaseDescription;
  result.mapView := aMapView;
  // update map view of all scenarios
  TMonitor.Enter(result.scenarios);
  try
    for scenario in result.scenarios.values
    do scenario.mapView := result.mapView;
    // add base scenario
    if not result.scenarios.TryGetValue(EcodistrictBaseScenario, scenario) then
    begin
      scenario := TEcodistrictScenario.Create(result, result.ProjectID, result.ProjectName, result.ProjectDescription, result.addBasicLayers, (result as TEcodistrictProject).mapView);
      if not Assigned(result.projectCurrentScenario)
      then result.projectCurrentScenario := scenario;
      result.scenarios.Add(EcodistrictBaseScenario, scenario);
      Log.WriteLn('added base scenario '+EcodistrictBaseScenario+': '+result.ProjectName+', '+result.ProjectDescription, llNormal, 1);
    end
    else
    begin
      Log.WriteLn('already contains base scenario '+EcodistrictBaseScenario, llNormal, 1);
    end;
  finally
    TMonitor.Exit(result.scenarios);
  end;
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
  _kpiList: TJSONArray;
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
  _kpi: TJSONValue;
  kpiSufficient: Double;
  kpiExcellent: Double;
  kpiId: string;
  kpiName: string;
  kpiList: TObjectList<TEcodistrictKPI>;
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
        //{project := }GetOrAddCase(_caseId, nil);
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
        then mapView := TMapView.Create(extent.centerY, extent.centerX, ZoomLevelFromDeltaLat(1.1*Abs(extent.yMax-extent.yMin)))
        else mapView := TMapView.Create(55.7, 41, 4); //  whole of europe?
        kpiList := TObjectList<TEcodistrictKPI>.Create;
        try
          // process kpi list
          try
            _kpiList := _jsonObject.GetValue<TJSONArray>('caseData.kpiList');
            if Assigned(_kpiList) then
            begin
              for i := 0 to _kpiList.Count-1 do
              begin
                try
                  _kpi := _kpiList.Items[i];
                  // get id from kpiAlias or kpiId (backwards compatible)
                  kpiId := _kpi.GetValue<string>('kpiAlias', '');
                  if kpiId=''
                  then kpiId := _kpi.GetValue<string>('kpiId', '');
                  kpiName := _kpi.GetValue<string>('name');
                  if _kpi.TryGetValue<Double>('sufficient', kpiSufficient) and
                  	_kpi.TryGetValue<Double>('excellent', kpiExcellent)
                  then kpiList.Add(TEcodistrictKPI.Create(kpiId, kpiName, kpiSufficient, kpiExcellent))
                  else Log.WriteLn('Missing sufficient or excellent on kpi '+kpiId, llWarning);
                except
                  on E: Exception do
                  begin
                    Log.WriteLn('Exception parsing kpi '+i.ToString+': '+E.Message, llError);
                  end;
                end;
              end;
            end
            else Log.WriteLn('No kpiList in response on getCase', llWarning);
          except
            on E: Exception
            do Log.WriteLn('No kpiList in response on getCase: '+E.Message, llError);
          end;

          // add or set project
          HandleModuleCase(_caseId, _title, _description, mapView, kpiList);
        finally
          kpiList.Free;
        end;
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
  dictScenariosID: string;
  scenario: TScenario;
begin
  if fProjects.TryGetValue(aCaseId, project) then
  begin
    // determine id used in dictionary
    if (aVariantId='') or (avariantId='null') or (aVariantId='None') or (aVariantID=aCaseID)
    then dictScenariosID := EcodistrictBaseScenario
    else dictScenariosID := aVariantId;

    TMonitor.Enter(project.scenarios);
    try
      if project.scenarios.TryGetValue(dictScenariosID, scenario) then
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
        // todo: too late?
        if not Assigned(project.projectCurrentScenario)
        then project.projectCurrentScenario := scenario;
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

procedure TEcodistrictModule.HandleModuleCaseDelete(const aCaseId: string);
begin
  // todo: implement
end;

end.
