unit MyOraLib;

interface

uses
  DB, Ora, OraSmart, OraObjects,
  SysUtils, Classes;

const
  PrimaryKeyIndexPostFix = '_PK';

type
  TSingleRowResult                     = array of string;
  TSingleRowResultDoubles              = array of Double;
  TAllRowsSingleFieldResult            = TSingleRowResult;
  TAllRowsResults                      = array of TSingleRowResult;
  TIDArray                             = array of Integer;

// simple queries and info for small data sets
function  ReturnOneResult              ( aOraSession: TOraSession; const aSQL, aDefault: string): string; overload;
function  ReturnOneResult              ( aOraSession: TOraSession; const aSQL: string; aDefault: Double): Double; overload;
function  ReturnOneResult              ( aOraSession: TOraSession; const aSQL: string; aDefault: Integer): Integer; overload;
function  ReturnFirstResult            ( aOraSession: TOraSession; const aSQL: string): TSingleRowResult; overload;
function  ReturnFirstResult            ( aOraSession: TOraSession; const aSQL: string; aDefaultValue: Double): TSingleRowResultDoubles; overload;
function  ReturnAllResults             ( aOraSession: TOraSession; const aSQL: string): TAllRowsResults;
function  ReturnAllFirstFields         ( aOraSession: TOraSession; const aSQL: string): TAllRowsSingleFieldResult;
function  ReturnRecordCount            ( aOraSession: TOraSession; const aTableName: string): Integer;
function  QueryReturnedResults         ( aOraSession: TOraSession; const aSQL: string): Boolean;
function  ExecuteQuery                 ( aOraSession: TOraSession; const aSQL: string): Boolean;
function  TableExists                  ( aOraSession: TOraSession; const aTableName: string): Boolean;
procedure ResultToStrings              ( aResults: TAllRowsSingleFieldResult; aStrings: TStrings); overload;
procedure ResultToStrings              ( aResults: TAllRowsResults; aStrings: TStrings); overload;
function  FieldExists                  ( aOraSession: TOraSession; const aTableName, aFieldName: string): Boolean;
function  SaveBlobToFile               ( aOraSession: TOraSession; const aTableName, aKeyName, aKeyValue, aBlobName, aFileName: string): Boolean;
function  SaveBlobToStream             ( aOraSession: TOraSession; const aTableName, aKeyName, aKeyValue, aBlobName: string; aStream: TStream): Boolean;
function  SaveBlobToStrings            ( aOraSession: TOraSession; const aTableName, aKeyName, aKeyValue, aBlobName: string; aStrings: TStrings): Boolean;
function  PutBlob                      ( aOraSession: TOraSession; const aTableName, aPrimaryKey, aWhereKeyName, aWhereKeyValue, aBlobName: string; const aBlob: TBytes): Boolean;

function  CreatePrimaryKey             ( aOraSession: TOraSession; const aTableName, aPrimaryKeyFieldNames: string): Boolean;

function  GetCurrentScenario           ( aOraSession: TOraSession;
                                         out aScenarioID: Integer;
                                         out aScenarioName, aFederation,
                                         aTablePrefix, aBaseTablePrefix, aScenarioNotes: string): Boolean;
function  GetScenarioInfo              ( aOraSession: TOraSession; aID: Integer;
                                         out aScenarioName, aFederation,
                                         aTablePrefix, aBaseTablePrefix, aScenarioNotes: string): Boolean;

function  GetCurrentScenarioID         ( aOraSession: TOraSession): Integer;
function  GetScenarioSeparator         ( aOraSession: TOraSession): string;
function  GetScenarioIDOnName          ( aOraSession: TOraSession; const aScenarioName: string): Integer;
function  GetScenarioIDOnTablePrefix   ( aOraSession: TOraSession; const aTablePrefix: string): Integer;
function  GetScenarioName              ( aOraSession: TOraSession; aScenarioID: Integer): string;
function  GetScenarioNames             ( aOraSession: TOraSession): TAllRowsSingleFieldResult;
function  GetScenarioIDs               ( aOraSession: TOraSession): TIDArray;
function  GetScenarioFederation        ( aOraSession: TOraSession; aScenarioID: Integer): string;
function  GetScenarioBaseID            ( aOraSession: TOraSession; aScenarioID: Integer): Integer;
function  GetScenarioParentID          ( aOraSession: TOraSession; aScenarioID: Integer): Integer;
function  GetScenarioDescription       ( aOraSession: TOraSession; aScenarioID: Integer): string;
function  GetScenarioStatus            ( aOraSession: TOraSession; aScenarioID: Integer): string;
function  GetScenarioTablePrefix       ( aOraSession: TOraSession; aScenarioID: Integer): string;

function  GetNextID                    ( aOraSession: TOraSession; const aTablename: string; const aFieldName: string='OBJECT_ID'): Integer;

function  PointToShapeField            ( const x, y: Double; const aFormatSettings: TFormatSettings): string;
function  ShapeFieldToPoint            ( aDataSet: TOraDataSet; out x, y: Double): Boolean;

function  GetParameterValue            ( aOraSession: TOraSession; const aParameterTableName, aParameterName: string; aDefaultValue: Integer): Integer; overload;
function  GetParameterValue            ( aOraSession: TOraSession; const aParameterTableName, aParameterName: string; aDefaultValue: Double): Double; overload;
function  GetParameterValue            ( aOraSession: TOraSession; const aParameterTableName, aParameterName: string; const aDefaultValue: string): string; overload;

implementation

procedure ResultToStrings(aResults: TAllRowsSingleFieldResult; aStrings: TStrings);
var
  r: Integer;
begin
  for r := 0 to Length(aResults) - 1
  do aStrings.Add(aResults[r]);
end;

procedure ResultToStrings(aResults: TAllRowsResults; aStrings: TStrings);
var
  r: Integer;
  c: Integer;
  s: string;
begin
  for r := 0 to Length(aResults) - 1 do
  begin
    s := '';
    for c := 0 to Length(aResults[r]) - 1 do
    begin
      if s=''
      then s := aResults[r, c]
      else s := s+','+aResults[r, c];
    end;
    aStrings.Add(s);
  end;
end;

function ReturnOneResult(aOraSession: TOraSession; const aSQL, aDefault: string): string;
var
  Table: TOraTable;
begin
  Result := aDefault;
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        if (Table.Fields.Count>=1) and not Table.Fields.FieldByNumber(1).IsNull
        then Result := Table.Fields.FieldByNumber(1).Value;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnOneResult(aOraSession: TOraSession; const aSQL: string; aDefault: Double): Double;
var
  Table: TOraTable;
begin
  Result := aDefault;
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        if (Table.Fields.Count>=1) and not Table.Fields.FieldByNumber(1).IsNull
        then Result := Table.Fields.FieldByNumber(1).AsFloat;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnOneResult(aOraSession: TOraSession; const aSQL: string; aDefault: Integer): Integer;
var
  Table: TOraTable;
begin
  Result := aDefault;
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        if (Table.Fields.Count>=1) and not Table.Fields.FieldByNumber(1).IsNull
        then Result := Table.Fields.FieldByNumber(1).AsInteger;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnFirstResult(aOraSession: TOraSession; const aSQL: string): TSingleRowResult;
var
  Table: TOraTable;
  f: Integer;
begin
  SetLength(Result, 0); // sentinel
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        SetLength(Result, Table.Fields.Count);
        for f := 1 to Table.Fields.Count do
        begin
          if not Table.Fields.FieldByNumber(f).IsNull
          then Result[f-1] := Table.Fields.FieldByNumber(f).Value;
        end;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnFirstResult(aOraSession: TOraSession; const aSQL: string; aDefaultValue: Double): TSingleRowResultDoubles;
var
  Table: TOraTable;
  f: Integer;
begin
  SetLength(Result, 0); // sentinel
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        SetLength(Result, Table.Fields.Count);
        for f := 1 to Table.Fields.Count do
        begin
          if not Table.Fields.FieldByNumber(f).IsNull
          then Result[f-1] := Table.Fields.FieldByNumber(f).AsFloat
          else Result[f-1] := aDefaultValue;
        end;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnAllResults(aOraSession: TOraSession; const aSQL: string): TAllRowsResults;
var
  Table: TOraTable;
  f: Integer;
begin
  SetLength(Result, 0); // sentinel
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        repeat
          // make room for extra row
          SetLength(Result, Length(Result)+1);
          // init new row
          SetLength(Result[Length(Result)-1], Table.Fields.Count);
          // fill fields of extra row
          for f := 1 to Table.Fields.Count do
          begin
            if not Table.Fields.FieldByNumber(f).IsNull
            then Result[Length(Result)-1][f-1] := Table.Fields.FieldByNumber(f).Value;
          end;
        until NOT Table.FindNext;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnAllFirstFields(aOraSession: TOraSession; const aSQL: string): TAllRowsSingleFieldResult;
var
  Table: TOraTable;
begin
  SetLength(Result, 0); // sentinel
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        if Table.Fields.Count>0 then
        begin
          repeat
            // make room for extra row
            SetLength(Result, Length(Result)+1);
            if not Table.Fields[0].IsNull
            then Result[Length(Result)-1] := Table.Fields[0].AsString;
          until NOT Table.FindNext;
        end;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ReturnRecordCount(aOraSession: TOraSession; const aTableName: string): Integer;
var
  Table: TOraTable;
begin
  Result := -1; // sentinel
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := 'SELECT COUNT(*) FROM '+aTableName;
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        if Table.Fields.Count>0
        then Result := Table.Fields[0].AsInteger;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function QueryReturnedResults(aOraSession: TOraSession; const aSQL: string): Boolean;
var
  Table: TOraTable;
begin
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := aSQL;
    Table.Execute;
    try
      Result := Table.FindFirst;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function ExecuteQuery(aOraSession: TOraSession; const aSQL: string): Boolean;
var
  Query: TOraQuery;
begin
  Query := TOraQuery.Create(nil);
  try
    Query.Session := aOraSession;
    Query.SQL.Text :=aSQL;
    Query.Execute;
    Result := Query.RowsProcessed>0;
  finally
    Query.Free;
  end;
end;

function TableExists(aOraSession: TOraSession; const aTableName: string): Boolean;
var
  TableList : TStrings;
begin
  TableList := TStringList.Create;
  if not aOraSession.Connected then aOraSession.Connect;
  aOraSession.GetTableNames(TableList);
  Result := TableList.IndexOf(aTableName) <> -1;
  TableList.Free;
end;

function FieldExists(aOraSession: TOraSession; const aTableName, aFieldName: string): Boolean;
var
  Table: TOraTable;
begin
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.TableName := aTableName;
    Table.Open;
    try
      Result := Table.FindField(aFieldName)<>nil;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function SaveBlobToFile(aOraSession: TOraSession; const aTableName, aKeyName, aKeyValue, aBlobName, aFileName: string): Boolean;
var
  Table: TOraTable;
  F: TField;
begin
  Result := False;
  try
    Table := TOraTable.Create(nil);
    try
      Table.Session := aOraSession;
      Table.SQL.Text := 'SELECT '+aBlobName+' FROM '+aTableName+' WHERE '+aKeyName+'='''+aKeyValue+'''';
      Table.Execute;
      if Table.FindFirst then
      begin
        F := Table.Fields[0];
        if (F is TBlobField) and not F.IsNull then
        begin
          (F as TBlobField).SaveToFile(aFileName);
          Result := True;
        end;
      end;
    finally
      Table.Free;
    end;
  except
  end;
end;

function SaveBlobToStream(aOraSession: TOraSession; const aTableName, aKeyName, aKeyValue, aBlobName: string; aStream: TStream): Boolean;
var
  Table: TOraTable;
  F: TField;
begin
  Result := False;
  try
    Table := TOraTable.Create(nil);
    try
      Table.Session := aOraSession;
      Table.SQL.Text := 'SELECT '+aBlobName+' FROM '+aTableName+' WHERE '+aKeyName+'='''+aKeyValue+'''';
      Table.Execute;
      if Table.FindFirst then
      begin
        F := Table.Fields[0];
        if (F is TBlobField) and not F.IsNull then
        begin
          (F as TBlobField).SaveToStream(aStream);
          Result := True;
        end;
      end;
    finally
      Table.Free;
    end;
  except
  end;
end;

function SaveBlobToStrings(aOraSession: TOraSession; const aTableName, aKeyName, aKeyValue, aBlobName: string; aStrings: TStrings): Boolean;
var
  Table: TOraTable;
  F: TField;
begin
  Result := False;
  try
    Table := TOraTable.Create(nil);
    try
      Table.Session := aOraSession;
      Table.SQL.Text := 'SELECT '+aBlobName+' FROM '+aTableName+' WHERE '+aKeyName+'='''+aKeyValue+'''';
      Table.Execute;
      if Table.FindFirst then
      begin
        F := Table.Fields[0];
        if (F is TBlobField) and not F.IsNull then
        begin
          aStrings.Assign(F);
          Result := True;
        end;
      end;
    finally
      Table.Free;
    end;
  except
  end;
end;

function PutBlob(aOraSession: TOraSession; const aTableName, aPrimaryKey, aWhereKeyName, aWhereKeyValue, aBlobName: string; const aBlob: TBytes): Boolean;
var
  Table: TOraTable;
  F: TBlobField;
begin
  Result := False;
  try
    Table := TOraTable.Create(nil);
    try
      Table.Session := aOraSession;
      Table.SQL.Text := 'SELECT '+aPrimaryKey+', '+aBlobName+' FROM '+aTableName+' WHERE '+aWhereKeyName+'='''+aWhereKeyValue+'''';
      Table.Execute;
      if Table.FindFirst then
      begin
        F := Table.Fields[1] as TBlobField;
        Table.Edit;
        F.Value := aBlob;
        Table.Post;
        Result := True;
      end;
    finally
      Table.Free;
    end;
  except
  end;
end;

function CreatePrimaryKey(aOraSession: TOraSession; const aTableName, aPrimaryKeyFieldNames: string): Boolean;
begin
  Result := True; //sentinel: expect that all will go well
  try
    ExecuteQuery(aOraSession,
      'CREATE UNIQUE INDEX '+aTableName+PrimaryKeyIndexPostFix+' '+
      'ON '+aTableName+' ('+aPrimaryKeyFieldNames+') NOLOGGING NOPARALLEL');
  except
    Result := False;
  end;
  try
    ExecuteQuery(aOraSession,
      'ALTER TABLE '+aTableName+' ADD '+
      '(CONSTRAINT '+aTableName+PrimaryKeyIndexPostFix+' '+
       'PRIMARY KEY ('+aPrimaryKeyFieldNames+') USING INDEX '+aTableName+PrimaryKeyIndexPostFix+')');
  except
    Result := False;
  end;
end;

function GetCurrentScenario(aOraSession: TOraSession; out aScenarioID: Integer;
  out aScenarioName, aFederation, aTablePrefix, aBaseTablePrefix, aScenarioNotes: string): Boolean;
var
  Query: TOraQuery;
  SeparatorChar: string;
begin
  // sentinel
  aScenarioName := '';
  aFederation := '';
  aTablePrefix := '';
  aBaseTablePrefix := '';
  aScenarioNotes := '';
  Query := TOraQuery.Create(nil);
  try
    Query.Session := aOraSession;
    Query.SQL.Text :=
      'SELECT ms.ID, cs.SEPARATOR_CHAR, ms.FEDERATE, ms.NAME, bs.NAME, ms.NOTES '+
      'FROM META_SCENARIOS ms JOIN META_CURRENTSESSION cs ON ms.ID=cs.CURRENT_SESSION_ID LEFT '+
        'JOIN META_SCENARIOS bs ON ms.BASE_ID=bs.ID';
    Query.ExecSQL;
    if Query.FindFirst then
    begin
      aScenarioID := Query.Fields[0].AsInteger;
      SeparatorChar := Query.Fields[1].AsString;
      aFederation := Query.Fields[2].AsString;
      aScenarioName := Query.Fields[3].AsString;
      aTablePrefix := aScenarioName+SeparatorChar;
      if not Query.Fields[4].IsNull
      then aBaseTablePrefix := Query.Fields[4].AsString+SeparatorChar;
      aScenarioNotes := Query.Fields[5].AsString;
      Result := True;
    end
    else Result := False;
  finally
    Query.Free;
  end;
end;

function GetScenarioInfo(aOraSession: TOraSession; aID: Integer; out aScenarioName, aFederation, aTablePrefix, aBaseTablePrefix, aScenarioNotes: string): Boolean;
var
  Query: TOraQuery;
  SeparatorChar: string;
begin
  // sentinel
  aScenarioName := '';
  aFederation := '';
  aTablePrefix := '';
  aBaseTablePrefix := '';
  aScenarioNotes := '';
  Query := TOraQuery.Create(nil);
  try
    Query.Session := aOraSession;
    Query.SQL.Text :=
      'SELECT ms.ID, cs.SEPARATOR_CHAR, ms.FEDERATE, ms.NAME, bs.NAME, ms.NOTES '+
      'FROM META_CURRENTSESSION cs, META_SCENARIOS ms LEFT JOIN META_SCENARIOS bs ON ms.BASE_ID=bs.ID '+
      'WHERE ms.ID='+IntToSTr(aID);
    Query.ExecSQL;
    if Query.FindFirst then
    begin
      //aScenarioID := Query.Fields[0].AsInteger;
      SeparatorChar := Query.Fields[1].AsString;
      aFederation := Query.Fields[2].AsString;
      aScenarioName := Query.Fields[3].AsString;
      aTablePrefix := aScenarioName+SeparatorChar;
      if not Query.Fields[4].IsNull
      then aBaseTablePrefix := Query.Fields[4].AsString+SeparatorChar;
      aScenarioNotes := Query.Fields[5].AsString;
      Result := True;
    end
    else Result := False;
  finally
    Query.Free;
  end;
{
var
  Query: TOraQuery;
  SeparatorChar: string;
begin
  // sentinel
  Result := False;
  aScenarioName := '';
  aFederation := '';
  aTablePrefix := '';
  aBaseTablePrefix := '';
  aScenarioNotes := '';
  Query := TOraQuery.Create(nil);
  try
    Query.Session := aOraSession;
    Query.SQL.Text := 'SELECT SEPARATOR_CHAR FROM META_CURRENTSESSION';
    Query.ExecSQL;
    if Query.FindFirst then
    begin
      SeparatorChar := Query.Fields[0].AsString;
      Query.Close;
      Query.SQL.Text :=
        'SELECT ms.FEDERATE, ms.NAME, bs.NAME, ms.NOTES '+
        'FROM META_SCENARIOS ms LEFT JOIN META_SCENARIOS bs ON ms.BASE_ID=bs.ID';
      Query.ExecSQL;
      if Query.FindFirst then
      begin
        aScenarioName := Query.Fields[1].AsString;
        aFederation := Query.Fields[0].AsString;
        aTablePrefix := aScenarioName+SeparatorChar;
        aBaseTablePrefix := Query.Fields[2].AsString+SeparatorChar;
        aScenarioNotes := Query.Fields[3].AsString;
        Result := True;
      end;
    end;
  finally
    Query.Free;
  end;
  }
end;

function GetCurrentScenarioID(aOraSession: TOraSession): Integer;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT CURRENT_SESSION_ID FROM META_CURRENTSESSION', -1);
end;

function GetScenarioSeparator(aOraSession: TOraSession): string;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT SEPARATOR_CHAR FROM META_CURRENTSESSION', '#');
end;

function GetScenarioIDOnName(aOraSession: TOraSession; const aScenarioName: string): Integer;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT ID FROM META_SCENARIOS WHERE UPPER(NAME)='''+UpperCase(aScenarioName)+'''', -1);
end;

function GetScenarioIDOnTablePrefix(aOraSession: TOraSession; const aTablePrefix: string): Integer;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT ID FROM META_SCENARIOS, META_CURRENTSESSION WHERE UPPER(CONCAT(NAME, SEPARATOR_CHAR))='''+UpperCase(aTablePrefix)+'''', -1);
end;

function GetScenarioName(aOraSession: TOraSession; aScenarioID: Integer): string;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT NAME FROM META_SCENARIOS WHERE ID='+IntToStr(aScenarioID), '');
end;

function GetScenarioNames(aOraSession: TOraSession): TAllRowsSingleFieldResult;
begin
  Result := ReturnAllFirstFields(aOraSession, 'SELECT NAME FROM META_SCENARIOS ORDER BY NAME');
end;

function GetScenarioIDs(aOraSession: TOraSession): TIDArray;
var
  Table: TOraTable;
begin
  SetLength(Result, 0); // sentinel
  Table := TOraTable.Create(nil);
  try
    Table.Session := aOraSession;
    Table.SQL.Text := 'SELECT ID FROM META_SCENARIOS ORDER BY NAME';
    Table.Execute;
    try
      if Table.FindFirst then
      begin
        if Table.Fields.Count>0 then
        begin
          repeat
            // make room for extra row
            SetLength(Result, Length(Result)+1);
            if not Table.Fields[0].IsNull
            then Result[Length(Result)-1] := Table.Fields[0].AsInteger;
          until NOT Table.FindNext;
        end;
      end;
    finally
      Table.Close;
    end;
  finally
    Table.Free;
  end;
end;

function GetScenarioFederation(aOraSession: TOraSession; aScenarioID: Integer): string;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT FEDERATE FROM META_SCENARIOS WHERE ID='+IntToStr(aScenarioID), '');
end;

function GetScenarioBaseID(aOraSession: TOraSession; aScenarioID: Integer): Integer;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT BASE_ID FROM META_SCENARIOS WHERE ID='+IntToStr(aScenarioID), -1);
end;

function GetScenarioParentID(aOraSession: TOraSession; aScenarioID: Integer): Integer;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT PARENT_ID FROM META_SCENARIOS WHERE ID='+IntToStr(aScenarioID), -1);
end;

function GetScenarioDescription(aOraSession: TOraSession; aScenarioID: Integer): string;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT NOTES FROM META_SCENARIOS WHERE ID='+IntToStr(aScenarioID), '');
end;

function GetScenarioStatus(aOraSession: TOraSession; aScenarioID: Integer): string;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT SCENARIO_STATUS FROM META_SCENARIOS WHERE ID='+IntToStr(aScenarioID), '');
end;

function GetScenarioTablePrefix(aOraSession: TOraSession; aScenarioID: Integer): string;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT CONCAT(NAME, SEPARATOR_CHAR) FROM META_SCENARIOS, META_CURRENTSESSION WHERE ID='+IntToStr(aScenarioID), '');
end;

function GetNextID(aOraSession: TOraSession; const aTablename, aFieldName: string): Integer;
begin
  Result := ReturnOneResult(aOraSession, 'SELECT MAX('+aFieldName+')+1 FROM '+aTableName, 0);
end;

//var
//  Geometry: TOraObject;
//  Geometry := TOraObject.Create;
//  Geometry.CreateObject((aDataSet.Connection as TOraSession).OCISvcCtx, '"MDSYS"."SDO_GEOMETRY"');

//  Geometry.AttrAsInteger['SDO_GTYPE'] := 2001;
//  Geometry.AttrAsArray['SDO_ELEM_INFO'].Clear;
//  Geometry.attrasarray['SDO_ORDINATES'].Clear;
//  Geometry.AttrAsFloat['SDO_POINT.X'] := x;
//  Geometry.AttrAsFloat['SDO_POINT.Y'] := y;
//  aDataSet.FieldByName('SHAPE').

function PointToShapeField(const x, y: Double; const aFormatSettings: TFormatSettings): string;
begin
  Result := 'MDSYS.SDO_GEOMETRY(2001,NULL,MDSYS.SDO_POINT_TYPE('+FloatToStr(x, aFormatSettings)+', '+FloatToStr(y, aFormatSettings)+', NULL),NULL,NULL)';
end;

function ShapeFieldToPoint(aDataSet: TOraDataSet; out x, y: Double): Boolean;
var
  Geometry: TOraObject;
begin
  Geometry := aDataSet.GetObject('SHAPE');
  try
    if Geometry.AttrAsInteger['SDO_GTYPE']=2001 then
    begin
      x := Geometry.AttrAsFloat['SDO_POINT.X'];
      y := Geometry.AttrAsFloat['SDO_POINT.Y'];
      Result := True;
    end
    else Result := False;
  finally
    Geometry.FreeObject;
  end;
end;

function GetParameterValue(aOraSession: TOraSession; const aParameterTableName, aParameterName: string; aDefaultValue: Integer): Integer;
begin
  Result := ReturnOneResult(aOraSession,
    'SELECT PARAM_VALUE '+
    'FROM '+aParameterTableName+' '+
    'WHERE LOWER(PARAM_NAME)=LOWER('''+aParameterName+''')',
    aDefaultValue);
end;

function GetParameterValue(aOraSession: TOraSession; const aParameterTableName, aParameterName: string; aDefaultValue: Double): Double;
begin
  Result := ReturnOneResult(aOraSession,
    'SELECT PARAM_VALUE '+
    'FROM '+aParameterTableName+' '+
    'WHERE LOWER(PARAM_NAME)=LOWER('''+aParameterName+''')',
    aDefaultValue);
end;

function GetParameterValue(aOraSession: TOraSession; const aParameterTableName, aParameterName: string; const aDefaultValue: string): string;
begin
  Result := ReturnOneResult(aOraSession,
    'SELECT PARAM_VALUE '+
    'FROM '+aParameterTableName+' '+
    'WHERE LOWER(PARAM_NAME)=LOWER('''+aParameterName+''')',
    aDefaultValue);
end;


end.
