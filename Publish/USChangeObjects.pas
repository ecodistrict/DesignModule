unit USChangeObjects;

interface

uses
  IMB3NativeClient,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,

  System.SysUtils;

const
  ORA_BATCHED_QUERY_ARRAY_LENGTH = 1000;

type
  TChangeObjectUpdateQuery = class
  constructor Create(
    aEvent: TIMBEventEntry; const aEventAttribute: string; const aConnectString: string;
    const aTableName, aKeyField, aUpdateField: string;
    aArrayLength: Integer=ORA_BATCHED_QUERY_ARRAY_LENGTH);
  destructor Destroy; override;
  private
    fSession: TOraSession;
    fQuery: TOraSQL;
    fKeyParam: TOraParam; // 1-based
    fUpdateParam: TOraParam; // 1-based
    fUpdateIndex: Integer; // 0-based
    fEvent: TIMBEventEntry;
    fEventAttribute: string;
  public
    procedure Update(aKey: Integer; aValue: Double);
    procedure Commit();
  end;


implementation

{ TChangeObjectUpdateQuery }

procedure TChangeObjectUpdateQuery.Commit;
var
  i: Integer;
begin
  if fUpdateIndex>0 then
  begin
    fQuery.Execute(fUpdateIndex);
    fQuery.session.Commit;
    for i := 0 to fUpdateIndex-1
    do fEvent.SignalChangeObject(actionChange, fKeyParam.ItemAsInteger[i+1], fEventAttribute); // ItemAs.. index is 1-based!
    fUpdateIndex := 0;
  end;
end;

constructor TChangeObjectUpdateQuery.Create(
  aEvent: TIMBEventEntry; const aEventAttribute, aConnectString: string;
  const aTableName, aKeyField, aUpdateField: string;
  aArrayLength: Integer);
begin
  inherited Create;
  fSession := TOraSession.Create(nil);
  fSession.ConnectString := aConnectString;
  fSession.Open;
  fQuery := TOraSQL.Create(nil);
  fQuery.Session := fSession;
  fQuery.SQL.Text :=
    'UPDATE '+aTableName+' '+
    'SET '+aUpdateField+'=:'+aUpdateField+' '+
    'WHERE '+aKeyField+'=:'+aKeyField+'';
  fQuery.Prepare;
  fQuery.ArrayLength := aArrayLength;
  fKeyParam := fQuery.ParamByName(aKeyField);
  fUpdateParam := fQuery.ParamByName(aUpdateField);
  fUpdateIndex := 0;
  fEvent := aEvent;
  fEventAttribute := aEventAttribute;
end;

destructor TChangeObjectUpdateQuery.Destroy;
begin
  Commit;
  FreeAndNil(fQuery);
  inherited;
end;

procedure TChangeObjectUpdateQuery.Update(aKey: Integer; aValue: Double);
begin
  // ItemAs.. index is 1-based, fUpdateIndex is 0-based!
  fKeyParam.ItemAsInteger[fUpdateIndex+1] := aKey;
  fUpdateParam.ItemAsFloat[fUpdateIndex+1] := aValue;
  fUpdateIndex := fUpdateIndex+1;
  if fUpdateIndex = fKeyParam.Length
  then Commit();
end;

end.
