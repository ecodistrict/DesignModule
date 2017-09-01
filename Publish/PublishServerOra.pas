unit PublishServerOra;

interface

uses
  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  WorldDataCode,
  SysUtils;

function ConnectStringFromSession(aOraSession: TOraSession): string;

function TablePrefixFromFederation(const aFederation: string): string;

function CreateWDGeometryFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometry;
function CreateWDGeometryPointFromSDOShape(aQuery: TOraQuery; const aFieldName: string): TWDGeometryPoint;


implementation

function ConnectStringFromSession(aOraSession: TOraSession): string;
begin
  with aOraSession
  do Result := userName+'/'+password+'@'+server;
end;

function TablePrefixFromFederation(const aFederation: string): string;
var
  s: TArray<string>;
begin
  s := aFederation.Split(['#']);
  if length(s)>1
  then Result := s[1]+'#'
  else Result := '';
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
  x1,y1{,z1}: double;
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
        NParts := ElemInfo.Size div 3;
        for PartNo := 0 to NParts-1 do
        begin
          Result.AddPart;
          i := ElemInfo.ItemAsInteger[PartNo*3]-1; // convert 1-based index to 0-based (TOraArray)
          if PartNo < NParts-1
          then PartSize := ElemInfo.ItemAsInteger[(PartNo+1)*3] - i
          else PartSize := Ordinates.Size - i;
          PartSize := PartSize div 2; // 2 ordinates per co-ordinate
          for pnt := 0 to PartSize-1 do
          begin
            Result.AddPoint(Ordinates.ItemAsFloat[i], Ordinates.ItemAsFloat[i + 1], Double.NaN);
            Inc(i, 2);
          end;
        end;

      end;
    2003: // 2D POLYGON
      begin
        NParts := ElemInfo.Size div 3;
        for PartNo := 0 to NParts-1 do
        begin
          Result.AddPart;
          i := ElemInfo.ItemAsInteger[PartNo*3]-1; // convert 1-based index to 0-based (TOraArray)
          if PartNo < NParts-1
          then PartSize := ElemInfo.ItemAsInteger[(PartNo+1)*3] - i
          else PartSize := Ordinates.Size - i;
          PartSize := PartSize div 2; // 2 ordinates per co-ordinate
          for pnt := 0 to PartSize-1 do
          begin
            Result.AddPoint(Ordinates.ItemAsFloat[i], Ordinates.ItemAsFloat[i + 1], Double.NaN);
            Inc(i, 2);
          end;
        end;
      end;
    2002: // 2D LINE
      begin
        for pnt := 0 to (Ordinates.Size div 2)-1 do
        begin
          Result.AddPoint(Ordinates.ItemAsFloat[pnt*2], Ordinates.ItemAsFloat[pnt*2 + 1], Double.NaN);
        end;
      end;
    2001: // 2D POINT
      begin
        x1:=Geometry.AttrAsFloat['SDO_POINT.X'];
        y1:=Geometry.AttrAsFloat['SDO_POINT.Y'];
        Result.AddPoint(x1, y1, Double.NaN);
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