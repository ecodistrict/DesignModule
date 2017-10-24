unit PublishServerDB;

interface

uses
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.Comp.UI,
  FireDAC.Comp.Client,
  FireDAC.Dapt,
  FireDAC.stan.def,
  FireDAC.stan.async,
  Data.DB,
  System.Classes,
  System.SysUtils;


// FireDAC work-a-round for crash
var
  dbFDGUIxWaitCursor1: TFDGUIxWaitCursor=nil;

// FireDAC PG environment
var
  dbFDPhysPgDriverLink: TFDPhysPgDriverLink=nil;

procedure InitPG;
procedure ExitPG;
procedure SetPGConnection(aDBCOnnection: TFDConnection;
  const aDatabase, aUserName, aPassword: string;
  const aServer: string='localhost'; aPort: Integer=5432); overload;

procedure SetPGConnection(aDBConnection: TFDConnection; aParameters: TStrings); overload;
procedure SetPGConnection(aDBConnection: TFDConnection; const aConnectString: string); overload;

function FDReadJSON(aDBConnection: TFDConnection; const aQuery: string): string;
//function FDCreateQuery(aDBConnection: TFDConnection; const aQuery: string ): TFDQuery;
function FDSingleStringResultQuery(aDBConnection: TFDConnection; const aQuery: string): string;

function PGGeometriesJSONQuery(const aTableName, aIDFieldName, aGeometryFieldName: string): string;
function PGSVGPathsQuery(const aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string): string;

implementation

procedure InitPG;
begin
  if not Assigned(dbFDGUIxWaitCursor1) then
  begin
    dbFDGUIxWaitCursor1 := TFDGUIxWaitCursor.Create(nil);
    dbFDPhysPgDriverLink := TFDPhysPgDriverLink.Create(nil);
  end;
end;

procedure ExitPG;
begin
  if Assigned(dbFDGUIxWaitCursor1) then
  begin
    FreeAndNil(dbFDPhysPgDriverLink);
    FreeAndNil(dbFDGUIxWaitCursor1);
  end;
end;

procedure SetPGConnection(aDBConnection: TFDConnection;
  const aDatabase, aUserName, aPassword: string;
  const aServer: string='localhost'; aPort: Integer=5432);
begin
  aDBConnection.ResourceOptions.AutoReconnect := True;
  aDBConnection.LoginPrompt := False;
  aDBConnection.DriverName := 'PG';
  aDBConnection.Params.Add('DriverID=PG');
  aDBConnection.Params.Add('Database='+aDatabase);
  aDBConnection.Params.Add('User_Name='+aUserName);
  aDBConnection.Params.Add('Password='+aPassword);
  aDBConnection.Params.Add('Server='+aServer);
  aDBConnection.Params.Add('Port='+aPort.toString);
end;

procedure SetPGConnection(aDBConnection: TFDConnection; aParameters: TStrings);
begin
  aDBConnection.ResourceOptions.AutoReconnect := True;
  aDBConnection.LoginPrompt := False;
  aDBConnection.DriverName := 'PG';
  aDBConnection.Params.Add('DriverID=PG');
  aDBConnection.Params.AddStrings(aParameters);
end;

procedure SetPGConnection(aDBConnection: TFDConnection; const aConnectString: string);
var
  parameters: TStringList;
begin
  parameters := TStringList.Create();
  try
    parameters.Delimiter := ';';
    parameters.StrictDelimiter := True;
    parameters.DelimitedText := aConnectString;
    SetPGConnection(aDBConnection, parameters);
  finally
    parameters.Free;
  end;
end;

function FDReadJSON(aDBConnection: TFDConnection; const aQuery: string): string;
var
  query: TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := aDBConnection;
    query.SQL.Text := aQuery;
    query.Open;
    if not query.Eof
    then result := query.Fields[0].AsString
    else result := '';
  finally
    query.Free;
  end;
end;

// todo: essentially the same now as FDReadJSON
function FDSingleStringResultQuery(aDBConnection: TFDConnection; const aQuery: string): string;
var
  query: TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := aDBConnection;
    query.SQL.Text := aQuery;
    query.Open;
    if not query.Eof
    then result := query.Fields[0].AsString
    else result := '';
  finally
    query.Free;
  end;
end;

// http://stackoverflow.com/questions/21137237/postgres-nested-json-array-using-row-to-json
function PGGeometriesJSONQuery(const aTableName, aIDFieldName, aGeometryFieldName: string): string;
begin
  Result :=
    'SELECT row_to_json(fc)::text '+
    'FROM ( SELECT ''FeatureCollection'' As type, array_to_json(array_agg(f)) As features '+
		'FROM (SELECT ''Feature'' As type'+
    ', ST_AsGeoJSON(lg.'+aGeometryFieldName+')::json As geometry'+
    ', row_to_json((SELECT l FROM (SELECT '+aIDFieldName+') As l'+
      ')) As properties '+
    'FROM '+aTableName+' As lg   ) As f )  As fc;'
end;

function PGSVGPathsQuery(const aTableName, aIDFieldName, aGeometryFieldName, aDataFieldName: string): string;
begin
  if aDataFieldName<>''
  then Result :=
    'SELECT '+aIDFieldName+', ST_AsSVG('+aGeometryFieldName+'), '+aDataFieldName+' '+
    'FROM '+aTableName
  else Result :=
    'SELECT '+aIDFieldName+', ST_AsSVG('+aGeometryFieldName+') '+
    'FROM '+aTableName;
end;

end.
