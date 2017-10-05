program PublishDebugger;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  StdIni,
  Logger, LogConsole, LogFile,
  imb4,
  System.JSON,
  Rest.JSON,
  //SuperObject,
  System.Classes, System.SysUtils;

const
  USIdleFederation = 'USIdle';

  ProjectEventNamePrefix = 'Sessions.WS2IMB';

  ProjectIDSwitch = 'ProjectID';
    DefaultProjectID = '4B95BE74F9A44DA0908A30B27C3E8C99'; // schiedam

procedure HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llError);
end;

procedure HandleClientCommand(const aJSONString: string);
var
  lines: TStringList;
  l: Integer;
  jsonValue: TJSONValue;
  //lJSON : ISuperObject;
begin
  try
    jsonValue := TJSONObject.ParseJSONValue(aJSONString);
    try
      //lJSON := SO(aJSONString);
      lines := TStringList.Create;
      try
        lines.Text := Rest.JSON.TJson.Format(jsonValue);
        for l := 0 to lines.Count-1 do
        begin
          Log.WriteLn(lines[l], llDump, 1);
        end;
      finally
        lines.Free;
      end;
    finally
      jsonValue.Free;
    end;
  except
    on e: Exception
    do Log.WriteLn('exception decoding '+aJSONString+': '+e.Message, llError);
  end;
end;

procedure HandleClientIntStringEvent(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
begin
  if aInt=actionDelete then
  begin
    Log.WriteLn('unlink from '+aEventEntry.eventName);
    aEventEntry.unSubscribe;
  end;
end;

procedure HandleClientStringEvent(aEventEntry: TEventEntry; const aString: string);
begin
  Log.WriteLn('onString: '+aEventEntry.eventName);
  HandleClientCommand(aString);
end;

function HandleClientStreamCreate(aEventEntry: TEventEntry; const aName: string): TStream;
begin
  Result := TStringStream.Create('', TEncoding.UTF8, False);
end;

procedure HandleClientStreamEnd(aEventEntry: TEventEntry; const aName: string; var aStream: TStream; aCancel: Boolean);
begin
  Log.WriteLn('onStream: '+aEventEntry.eventName);
  HandleClientCommand((aStream as TStringStream).DataString);
end;

procedure HandleIntString(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
var
  clientEvent: TEventEntry;
begin
  if aInt=actionNew then
  begin
    Log.WriteLn(aEventEntry.eventName+': link to '+aString);
    clientEvent := aEventEntry.connection.eventEntry(aString, False).subscribe;
    clientEvent.OnString.Add(HandleClientStringEvent);
    clientEvent.OnIntString.Add(HandleClientIntStringEvent);
    clientEvent.OnStreamCreate := HandleClientStreamCreate;
    clientEvent.OnStreamEnd := HandleClientStreamEnd;
  end;
end;

var
  connection: TConnection;
  sessionsEvent: TEventEntry;
  projectID: string;
begin
  FileLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  ConsoleLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  try
    connection := TSocketConnection.Create('Publish debugger', 4, USIdleFederation);
    try
      connection.onDisconnect := HandleDisconnect;
      connection.onException := HandleException;
      projectID := GetSetting(ProjectIDSwitch, DefaultProjectID);
      sessionsEvent := connection.eventEntry(ProjectEventNamePrefix+'.'+projectID).subscribe;
      sessionsEvent.OnIntString.Add(HandleIntString);
      Log.WriteLn('Listening on '+sessionsEvent.eventName);

      WriteLn('Press return to quit');
      ReadLn;
    finally
      connection.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
