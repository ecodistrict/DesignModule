unit SessionServerUS;

interface

uses
  Logger,
  StdIni,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  MyOraLib,

  ODBFiles2,

  WorldDataCode,
  WorldLegends,

  imb4,
  WorldTilerConsts,
  CommandQueue,
  TimerPool,

  SessionServerLib,
  SessionServerGIS,

  Vcl.graphics, // TPicture

  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector,

  System.JSON,
  System.Math,
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections;

type
  TMetaLayerEntry = class
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
    function BaseTable(const aTablePrefix:string): string;
    function BaseTableNoPrefix: string;
    function BuildJoin(const aTablePrefix: string; out aShapePrefix: string): string;
    function SQLQuery(const aTablePrefix:string; xMin: Integer=0; yMin: Integer=0; xMax: Integer=-1; yMax: Integer=-1): string;
    function autoDiffRange: Double;
  end;

  TMetaLayer = TObjectDictionary<Integer, TMetaLayerEntry>;

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
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string; overload; override;
    function SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string; overload; override;

    //function selectObjectsProperties(aClient: TClient; const aSelectCategories, aSelectedObjects: TArray<string>): string; overrride;
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

  TUSProject = class(TProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer{; aSourceEPSG: Integer});
  destructor Destroy; override;
  private
    fUSDBScenarios: TObjectDictionary<string, TUSDBScenario>;
    fSourceProjection: TGIS_CSProjectedCoordinateSystem;
    fPreLoadScenarios: Boolean;
    //fMCProgressTimer: TTimer;
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

  {
  TModelControlUSModel = class(TMCModelStarter2)
  constructor Create();
  destructor Destroy; override;
  public
  // standard overrides
    procedure ParameterRequest(aParameters: TModelParameters); override;
    procedure StartModel(aParameters: TModelParameters); override;
    procedure StopModel; override;
  // manual start
    procedure CheckManualStart;
  private
    fSessionModel: TSessionModel;
    fIMBConnection: TConnection; // imb connection to websocket etc.
    fIMBLogger: TIMBLogger;
  protected
    procedure HandleException(aConnection: TConnection; aException: Exception);
    procedure HandleDisconnect(aConnection: TConnection);
  public
    property sessionModel: TSessionModel read fSessionModel;
    property imbConnection: TConnection read fIMBConnection;
    property imbLogger: TIMBLogger read fIMBLogger;

    procedure TestConnection();
  end;
  }

function getUSMapView(aOraSession: TOraSession; const aDefault: TMapView): TMapView;
function getUSProjectID(aOraSession: TOraSession; const aDefault: string): string;
procedure setUSProjectID(aOraSession: TOraSession; const aValue: string);
function getUSCurrentPublishedScenarioID(aOraSession: TOraSession; aDefault: Integer): Integer;

function Left(const s: string; n: Integer): string;
function Right(const s: string; n: Integer): string;
function StartsWith(const s, LeftStr: string): Boolean;
procedure SplitAt(const s: string; i: Integer; var LeftStr, RightStr: string);

function ReadMetaLayer(aSession: TOraSession; const aTablePrefix: string; aMetaLayer: TMetaLayer): Boolean;

function CreateWDGeometryFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometry;
function CreateWDGeometryPointFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometryPoint;

function ConnectToUSProject(const aConnectString, aProjectID: string; out aMapView: TMapView): TOraSession;

implementation

function ConnectToUSProject(const aConnectString, aProjectID: string; out aMapView: TMapView): TOraSession;
var
  dbConnection: TOraSession;
begin
  dbConnection := TOraSession.Create(nil);
  dbConnection.ConnectString := aConnectString;
  dbConnection.Open;
  setUSProjectID(dbConnection, aProjectID); // store project id in database
  aMapView := getUSMapView(dbConnection as TOraSession, TMapView.Create(52.08606, 5.17689, 11));
  Result := dbConnection;
end;

function Left(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s)
    then Result := s
    else Result := Copy(s, 1, n);
  end
  else Result := '';
end;

function Right(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s)
    then Result := s
    else Result := Copy(s, Length(s) - n + 1, n);
  end
  else Result := '';
end;

function StartsWith(const s, LeftStr: string): Boolean;
begin
  Result := AnsiCompareText(Left(s, Length(LeftStr)), LeftStr) = 0;
end;

procedure SplitAt(const s: string; i: Integer; var LeftStr, RightStr: string);
begin
  LeftStr := Left(s, i - 1);
  RightStr := Right(s, Length(s) - i);
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

function ReadMetaLayer(aSession: TOraSession; const aTablePrefix: string; aMetaLayer: TMetaLayer): Boolean;
var
  query: TOraQuery;
  metaLayerEntry: TMetaLayerEntry;
  //ss: TStringStream;
  sl: TStringList;

  function StringField(const aFieldName: string; const aDefaultValue: string=''): string;
  var
    F: TField;
  begin
    F := query.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsString
    else Result := aDefaultValue;
  end;

  function IntField(const aFieldName: string; aDefaultValue: Integer=0): Integer;
  var
    F: TField;
  begin
    F := query.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsInteger
    else Result := aDefaultValue;
  end;

  function DoubleField(const aFieldName: string; aDefaultValue: Double=NaN): Double;
  var
    F: TField;
  begin
    F := query.FieldByName(aFieldName);
    if Assigned(F) and not F.IsNull
    then Result := F.AsFloat
    else Result := aDefaultValue;
  end;

begin
  query := TOraQuery.Create(nil);
  try
    query.Session := aSession;
    query.SQL.Text := 'SELECT * FROM '+aTablePrefix+'META_LAYER';
    query.Open;
    while not query.Eof do
    begin
      // default
      metaLayerEntry := TMetaLayerEntry.Create;
      metaLayerEntry.OBJECT_ID := IntField('OBJECT_ID');
      metaLayerEntry.LAYER_TYPE := IntField('LAYER_TYPE');
      metaLayerEntry.LAYER_TABLE := StringField('LAYER_TABLE');
      metaLayerEntry.LEGEND_FILE := StringField('LEGEND_FILE');
      metaLayerEntry.LEGEND_DESC := StringField('LEGEND_DESC');
      metaLayerEntry.VALUE_EXPR := StringField('VALUE_EXPR');
      metaLayerEntry.VALUE_NODATA := DoubleField('VALUE_NODATA');
      metaLayerEntry.JOINCONDITION := StringField('JOINCONDITION');
      metaLayerEntry.TEXTURE_FILE := StringField('TEXTURE_FILE');
      metaLayerEntry.TEXTURE_EXPR := StringField('TEXTURE_EXPR');
      metaLayerEntry.ROW_START := IntField('ROW_START');
      metaLayerEntry.ROW_SIZE := IntField('ROW_SIZE');
      metaLayerEntry.COL_START := IntField('COL_START');
      metaLayerEntry.COL_SIZE := IntField('COL_SIZE');
      metaLayerEntry.ROW_FIELD := StringField('ROW_FIELD');
      metaLayerEntry.COL_FIELD := StringField('COL_FIELD');
      metaLayerEntry.IS_CELL_BASED := IntField('IS_CELL_BASED')=1;
      metaLayerEntry.MXR := DoubleField('MXR');
      metaLayerEntry.MXC := DoubleField('MXC');
      metaLayerEntry.MXT := DoubleField('MXT');
      metaLayerEntry.MYR := DoubleField('MYR');
      metaLayerEntry.MYC := DoubleField('MYC');
      metaLayerEntry.MYT := DoubleField('MYT');
      metaLayerEntry.IMB_EVENTCLASS := StringField('IMB_EVENTCLASS');

      // added for web interface
      metaLayerEntry.domain := StringField('DOMAIN');
      metaLayerEntry.description := StringField('DESCRIPTION');
      metaLayerEntry.diffRange := DoubleField('DIFFRANGE');
      metaLayerEntry.objectType := StringField('OBJECTTYPE');
      metaLayerEntry.geometryType := StringField('GEOMETRYTYPE');
      metaLayerEntry._published := IntField('PUBLISHED', 1);

      {
      ALTER TABLE VXX#META_LAYER
      ADD (DOMAIN VARCHAR2(50), DESCRIPTION VARCHAR2(150), DIFFRANGE NUMBER, OBJECTTYPE VARCHAR2(50), GEOMETRYTYPE VARCHAR2(50), PUBLISHED INTEGER);

      UPDATE V21#META_LAYER SET DIFFRANGE = 2 WHERE object_id in (142,52,53,54,55,153,3,4,28,32,141,56);
      }

      aMetaLayer.Add(metaLayerEntry.OBJECT_ID, metaLayerEntry);
      metaLayerEntry.LegendAVL := '';
      setLength(metaLayerEntry.odbList, 0);
      if metaLayerEntry.LEGEND_FILE<>'' then
      begin
        {
        ss := TStringStream.Create();
        try
          if SaveBlobToStream(aSession, 'VI3D_MODEL', 'PATH', metaLayerEntry.LEGEND_FILE, 'BINFILE', ss) then
          begin
            metaLayerEntry.LegendAVL := ss.ToString;
          end;
        finally
          ss.Free;
        end;
        }
        sl := TStringList.Create;
        try
          if SaveBlobToStrings(aSession, 'VI3D_MODEL', 'PATH', metaLayerEntry.LEGEND_FILE, 'BINFILE', sl) then
          begin
            metaLayerEntry.LegendAVL := sl.Text;
            metaLayerEntry.odbList := ODBFileToODBList(sl);
          end;
        finally
          sl.Free;
        end;
      end;
      query.Next;
    end;
    Result := True;
  finally
    query.Free;
  end;
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

function TMetaLayerEntry.BuildJoin(const aTablePrefix: string; out aShapePrefix: string): string;
var
  p: Integer;
  t1: string;
  t2: string;
begin
  aShapePrefix := '';
  p := Pos('!', LAYER_TABLE);
  if p>0 then
  begin
    SplitAt(LAYER_TABLE, p, t1, t2);
    Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
    aShapePrefix := 't1.';
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
    end
    else
    begin // single table?
      if StartsWith(LAYER_TABLE, 'GENE_ROAD_') then
      begin
        t1 := 'GENE_ROAD';
        t2 := LAYER_TABLE;
        Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
        aShapePrefix := 't1.';
      end
      else
      begin
        if StartsWith(LAYER_TABLE, 'GENE_BUILDING_') then
        begin
          t1 := 'GENE_BUILDING';
          t2 := LAYER_TABLE;
          Result := aTablePrefix+t1+' t1 JOIN '+aTablePrefix+t2+' t2 ON t1.OBJECT_ID=t2.OBJECT_ID';
          aShapePrefix := 't1.';
        end
        else
        begin
          t1 := LAYER_TABLE;
          //t2 := '';
          Result := aTablePrefix+t1+' t1';
          aShapePrefix := 't1.';
        end;
      end;
    end;
  end;
end;

{
function compareDiffValues(aValue1, aValue2: Double): Integer;
begin
  if aValue1=aValue2
  then Result := 0
  else if aValue1<aValue2
  then Result := -1
  else Result := 1;
end;
}

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
  values := TList<double>.Create;//(TComparer<Double>.Construct(compareDiffValues));
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
  {
  minValue := NaN;
  maxValue := NaN;
  for odb in odbList do
  begin
    if not IsNaN(odb.Min) then
    begin
      if IsNaN(minValue) or (minValue>odb.Min)
      then minValue := odb.Min;
    end;
    if not IsNaN(odb.Max) then
    begin
      if IsNaN(maxValue) or (maxValue<odb.Max)
      then maxValue := odb.Max;
    end;
  end;
  if IsNaN(minValue) then
  begin
    if IsNaN(maxValue)
    then Result := 100
    else Result := maxValue/aFactor;
  end
  else
  begin
    if IsNaN(maxValue)
    then Result := 100
    else Result := (maxValue-minValue)/aFactor;
  end;
  }
end;

function TMetaLayerEntry.SQLQuery(const aTablePrefix:string; xMin, yMin, xMax, yMax: Integer): string;
var
  join: string;
  ShapePrefix: string;
  cellIndexFiltering: Boolean;
begin
  join := BuildJoin(aTablePrefix, ShapePrefix);
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
            ShapePrefix+'OBJECT_ID, '+
            VALUE_EXPR+' AS VALUE, '+
            ShapePrefix+'SHAPE '+
          'FROM '+join;
        if JOINCONDITION<>''
        then Result := Result+' '+
          'WHERE '+JOINCONDITION;
      end;
    5, 9:
      begin
        Result :=
          'SELECT '+
            ShapePrefix+'OBJECT_ID, '+
            VALUE_EXPR+','+
            TEXTURE_EXPR+','+
            ShapePrefix+'SHAPE '+
          'FROM '+join;
        if JOINCONDITION<>''
        then Result := Result+' '+
          'WHERE '+JOINCONDITION;
      end;
    21:
      begin
        Result :=
          'SELECT '+
            ShapePrefix+'OBJECT_ID, '+
            ShapePrefix+'shape.sdo_point.x, '+
            ShapePrefix+'shape.sdo_point.y, '+
            ShapePrefix+'poiType, '+
            ShapePrefix+'category '+
          'FROM '+join;
        if JOINCONDITION<>''
        then Result := Result+' '+
          'WHERE '+JOINCONDITION;
      end;
  else
    Result :=
      'SELECT '+
        ShapePrefix+'OBJECT_ID, '+
        VALUE_EXPR+' AS VALUE, '+
        ShapePrefix+'SHAPE '+
      'FROM '+join;
    if JOINCONDITION<>''
    then Result := Result+' '+
      'WHERE '+JOINCONDITION;
  end;
end;

function CreateWDGeometryFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometry;
// https://docs.oracle.com/cd/B19306_01/appdev.102/b14255/sdo_objrelschema.htm
var
  Geometry: TOraObject;
  Ordinates: TOraArray;
  ElemInfo: TOraArray;
  GType: Integer;
  NParts: Integer;
  PartNo: Integer;
  PartSize: Integer;
  //PointNo: Integer;
  x1,y1{,z1}: double;
  //idx: Integer;
  //StartIdx: Integer;
  //EndIdx: Integer;
  i: Integer;
  pnt: Integer;
begin
  // create WDGeometry as function result
  Result := TWDGeometry.Create;
  // read SDO geometry
  Geometry := aQuery.GetObject(aFieldName);
  GType := Geometry.AttrAsInteger['SDO_GTYPE'];
  Ordinates := Geometry.AttrAsArray['SDO_ORDINATES'];
  ElemInfo := Geometry.AttrAsArray['SDO_ELEM_INFO'];
  // convert SDO geometry to WDGeometry
  case Gtype of
    2007: // 2D MULTIPOLYGON
      begin
        {
        NParts := ElemInfo.Size div 3;
        idx := 0;
        for PartNo := 1 to NParts do
        begin
          Result.AddPart;
          if PartNo < NParts
          then PartSize := ElemInfo.ItemAsInteger[PartNo*3] - ElemInfo.ItemAsInteger[PartNo*3-3]
          else PartSize := Ordinates.Size - ElemInfo.ItemAsInteger[PartNo*3-3];
          PartSize := PartSize div 2;
          for PointNo := 1 to PartSize do
          begin
            x1 := Ordinates.ItemAsFloat[idx];
            y1 := Ordinates.ItemAsFloat[idx+1];
            Result.AddPoint(x1, y1, NaN);
            Inc(idx, 2);
          end;
        end;
        }
        NParts := ElemInfo.Size div 3;
        for PartNo := 0 to NParts-1 do
        begin
          Result.AddPart;
          i := ElemInfo.ItemAsInteger[PartNo*3]-1; // convert 1-based index to 0-based (TOraArray)
          if PartNo < NParts-1
          then PartSize := ElemInfo.ItemAsInteger[(PartNo+1)*3] - i
          else PartSize := Ordinates.Size - i;
          PartSize := PartSize div 2; // 2 ordinates per co-ordinate
          //pntPartStart := Length(Polygon);
          //SetLength(Polygon, pntPartStart+PartSize);
          for pnt := 0 to PartSize-1 do
          begin
            //Polygon[pntPartStart+pnt].X := Ordinates.ItemAsFloat[i];
            //Polygon[pntPartStart+pnt].Y := Ordinates.ItemAsFloat[i + 1];
            Result.AddPoint(Ordinates.ItemAsFloat[i], Ordinates.ItemAsFloat[i + 1], NaN);
            Inc(i, 2);
          end;
        end;

      end;
    2003: // 2D POLYGON
      begin
        {
        Eleminfo.InsertItem(ElemInfo.Size); // prevent out of bounds
        ElemInfo.ItemAsInteger[ElemInfo.size-1]:=Ordinates.Size + 1;
        NParts := ElemInfo.Size div 3;
        for idx:=0 to (NParts)-1 do
        begin
          Result.AddPart;
          StartIdx :=ElemInfo.ItemAsInteger[idx*3];
          EndIdx   :=ElemInfo.ItemAsInteger[(idx*3)+3] -2 ;
          //PartSize := ElemInfo.ItemAsInteger[idx*3+3] - ElemInfo.ItemAsInteger[idx*3];
          while EndIdx > StartIdx  do
          begin
            x1 := ordinates.itemasfloat[EndIdx-1];
            y1 := ordinates.itemasfloat[EndIdx];
            Result.AddPoint(x1, y1, NaN);
            dec(EndIdx,2) ;
          end;
        end;
        }
        NParts := ElemInfo.Size div 3;
        for PartNo := 0 to NParts-1 do
        begin
          Result.AddPart;
          i := ElemInfo.ItemAsInteger[PartNo*3]-1; // convert 1-based index to 0-based (TOraArray)
          if PartNo < NParts-1
          then PartSize := ElemInfo.ItemAsInteger[(PartNo+1)*3] - i
          else PartSize := Ordinates.Size - i;
          PartSize := PartSize div 2; // 2 ordinates per co-ordinate
          //pntPartStart := Length(Polygon);
          //SetLength(Polygon, pntPartStart+PartSize);
          for pnt := 0 to PartSize-1 do
          begin
            //Polygon[pntPartStart+pnt].X := Ordinates.ItemAsFloat[i];
            //Polygon[pntPartStart+pnt].Y := Ordinates.ItemAsFloat[i + 1];
            Result.AddPoint(Ordinates.ItemAsFloat[i], Ordinates.ItemAsFloat[i + 1], NaN);
            Inc(i, 2);
          end;
        end;
      end;
    2002: // 2D LINE
      begin
        {
        //NParts := 1;
        //PartSize := Ordinates.Size div 2;
        StartIdx:=0;
        while StartIdx<Ordinates.Size do
        begin
          x1 := Ordinates.ItemAsFloat[StartIdx];
          y1 := Ordinates.ItemAsFloat[StartIdx+1];
          Result.AddPoint(x1, y1, NaN);
          inc(StartIdx,2);
        end;
        }
        //SetLength(Polygon, (Ordinates.Size) div 2);
        for pnt := 0 to (Ordinates.Size div 2)-1 do
        begin
          //Polygon[pnt].X := Ordinates.ItemAsFloat[pnt*2];
          //Polygon[pnt].Y := Ordinates.ItemAsFloat[pnt*2 + 1];
          Result.AddPoint(Ordinates.ItemAsFloat[pnt*2], Ordinates.ItemAsFloat[pnt*2 + 1], NaN);
        end;
      end;
    2001: // 2D POINT
      begin
        //NParts := 1;
        //PartSize := 1;
        x1:=Geometry.AttrAsFloat['SDO_POINT.X'];
        y1:=Geometry.AttrAsFloat['SDO_POINT.Y'];
        Result.AddPoint(x1, y1, NaN);
      end;
  end;
end;

function CreateWDGeometryPointFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometryPoint;
// https://docs.oracle.com/cd/B19306_01/appdev.102/b14255/sdo_objrelschema.htm
var
  Geometry: TOraObject;
  Ordinates: TOraArray;
  ElemInfo: TOraArray;
  GType: Integer;
  NParts: Integer;
  idx: Integer;
  StartIdx: Integer;
  EndIdx: Integer;
begin
  // create WDGeometryPoint as function result
  Result := TWDGeometryPoint.Create;
  // read SDO geometry
  Geometry := aQuery.GetObject(aFieldName);
  GType := Geometry.AttrAsInteger['SDO_GTYPE'];
  Ordinates := Geometry.AttrAsArray['SDO_ORDINATES'];
  ElemInfo := Geometry.AttrAsArray['SDO_ELEM_INFO'];
  // convert SDO geometry to WDGeometryPoint, use first point from SDO as value
  case Gtype of
    2007: // 2D MULTIPOLYGON
      begin
        if Ordinates.Size>=2 then
        begin
          Result.x := Ordinates.ItemAsFloat[0];
          Result.y := Ordinates.ItemAsFloat[1];
        end;
      end;
    2003: // 2D POLYGON
      begin
        Eleminfo.InsertItem(ElemInfo.Size); // prevent out of bounds
        ElemInfo.ItemAsInteger[ElemInfo.size-1]:=Ordinates.Size + 1;
        NParts := ElemInfo.Size div 3;
        if NParts>0 then
        begin
          idx := 0;
          StartIdx :=ElemInfo.ItemAsInteger[idx*3];
          EndIdx   :=ElemInfo.ItemAsInteger[(idx*3)+3] -2 ;
          if EndIdx > StartIdx then
          begin
            Result.x := ordinates.itemasfloat[EndIdx-1];
            Result.y := ordinates.itemasfloat[EndIdx];
          end;
        end;
      end;
    2002: // 2D LINE
      begin
        if Ordinates.Size>=2 then
        begin
          Result.x := Ordinates.ItemAsFloat[0];
          Result.y := Ordinates.ItemAsFloat[1];
        end;
      end;
    2001: // 2D POINT
      begin
        Result.x := Geometry.AttrAsFloat['SDO_POINT.X'];
        Result.y := Geometry.AttrAsFloat['SDO_POINT.Y'];
      end;
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
  inherited Create(aScenario, aDomain, aID, aName, aDescription, aDefaultLoad, aObjectTypes, aGeometryType, ltTile, True, aDiffRange, aBasicLayer);
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
  inherited Create(aProject, aID, aName, aDescription, aAddbasicLayers, aMapView, false);
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
    if mlp.Value._published>0 then
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
              diffRange := defaultValue(mlp.Value.diffRange, mlp.Value.autoDiffRange*0.3);
            end;
          2:
            begin
              objectTypes := '"grid"';
              geometryType := 'MultiPolygon'; // todo: ?
              diffRange := defaultValue(mlp.Value.diffRange, mlp.Value.autoDiffRange*0.3);
            end;
          3, 8:
            begin
              objectTypes := '"building"'; // 3 buildings, 8 RS buildings
              geometryType := 'MultiPolygon';
              diffRange := defaultValue(mlp.Value.diffRange, mlp.Value.autoDiffRange*0.3);
            end;
          4,    // road color (VALUE_EXPR)
          5:    // road color (VALUE_EXPR) and width (TEXTURE_EXPR)
            begin
              objectTypes := '"road"';
              geometryType := 'LineString';
              diffRange := defaultValue(mlp.Value.diffRange, mlp.Value.autoDiffRange*0.3);
            end;
          9:    // enrg color (VALUE_EXPR) and width (TEXTURE_EXPR)
            begin
              objectTypes := '"energy"';
              geometryType := 'LineString';
              diffRange := defaultValue(mlp.Value.diffRange, mlp.Value.autoDiffRange*0.3);
            end;
          11:
            begin
              objectTypes := '"location"';
              geometryType := 'Point';
              diffRange := mlp.Value.diffRange; // todo:
            end;
          21:
            begin
              objectTypes := '"poi"';
              geometryType := 'Point';
              diffRange := mlp.Value.diffRange; // todo:
            end;
        else
          // 31 vector layer ?
          objectTypes := '';
          geometryType := '';
          diffRange := mlp.Value.diffRange;
        end;
        if geometryType<>'' then
        begin
          layer := TUSLayer.Create(Self,
            defaultValue(mlp.Value.domain, standardIni.ReadString('domains', layerInfoParts[0], layerInfoParts[0])), //  domain
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
    end
    else Log.WriteLn(elementID+': skipped layer ('+mlp.Key.ToString+') type '+mlp.Value.LAYER_TYPE.ToString+' (based on meta_layer.published) ('+mlp.Value.LAYER_TABLE+'-'+mlp.Value.VALUE_EXPR, llRemark, 1);
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

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aGeometry: TWDGeometry): string;
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
    if selectLayersOnCategories(aSelectCategories, layers) then
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

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aX, aY, aRadius: Double): string;
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
    if selectLayersOnCategories(aSelectCategories, layers) then
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
             '"objects":['+nearestObject.JSON2D[nearestObjectLayer.geometryType, '']+']}}';
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

function TUSScenario.SelectObjects(aClient: TClient; const aType, aMode: string; const aSelectCategories: TArray<string>; aJSONQuery: TJSONArray): string;
var
  layers: TList<TLayer>;
begin
  Result := '';
  layers := TList<TLayer>.Create;
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

constructor TUSProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string;
  aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer);
var
  SourceEPSGstr: string;
  SourceEPSG: Integer;
begin
  fUSDBScenarios := TObjectDictionary<string, TUSDBScenario>.Create;
  mapView := aMapView;

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
  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName,
    aTilerFQDN, aTilerStatusURL,
    aDBConnection,
    0, False, False, False, False, addBasicLayers, '',
    aMaxNearestObjectDistanceInMeters);
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
  s: string;
begin
  ReadScenarios;
  ReadMeasures;
  // load current scenario and ref scenario first
  scenarioID := getUSCurrentPublishedScenarioID(OraSession, GetCurrentScenarioID(OraSession));
  fProjectCurrentScenario := ReadScenario(scenarioID.ToString);
  Log.WriteLn('current US scenario: '+fProjectCurrentScenario.ID+' ('+(fProjectCurrentScenario as TUSScenario).fTableprefix+'): "'+fProjectCurrentScenario.description+'"', llOk);
  // ref
  scenarioID := GetScenarioBaseID(OraSession, scenarioID);
  if scenarioID>=0 then
  begin
    fProjectRefScenario := ReadScenario(scenarioID.ToString);
    Log.WriteLn('reference US scenario: '+fProjectRefScenario.ID+' ('+(fProjectRefScenario as TUSScenario).fTableprefix+'): "'+fProjectRefScenario.description+'"', llOk);
  end
  else Log.WriteLn('NO reference US scenario', llWarning);
  if fPreLoadScenarios then
  begin
    for s in fUSDBScenarios.Keys do
    begin
      if fUSDBScenarios[s]._published>0
      then ReadScenario(s);
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
      if fUSDBScenarios.TryGetValue(aID, dbScenario) then
      begin
        //StatusInc;
        Result := TUSScenario.Create(Self, aID, dbScenario.name, dbScenario.description, addBasicLayers, mapView, dbScenario.tablePrefix);
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
  isp: TPair<string, TUSDBScenario>;
begin
  // read scenarios from project database
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

end.
