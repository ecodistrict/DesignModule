unit SessionServerUS;

interface

uses
  Ora,
  OraObjects,
  Data.DB,
  MyOraLib,
  ODBFiles2,
  WorldDataCode,
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
    publish: Integer;
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


function Left(const s: string; n: Integer): string;
function Right(const s: string; n: Integer): string;
function StartsWith(const s, LeftStr: string): Boolean;
procedure SplitAt(const s: string; i: Integer; var LeftStr, RightStr: string);

function ReadMetaLayer(aSession: TOraSession; const aTablePrefix: string; aMetaLayer: TMetaLayer): Boolean;

function CreateWDGeometryFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometry;
function CreateWDGeometryPointFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometryPoint;

implementation

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
      metaLayerEntry.publish:= IntField('PUBLISHED', 1);

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

end.
