program EcodistrictDebugger;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogConsole, LogFile,
  imb4,
  System.JSON,
  Rest.JSON,
  //SuperObject,
  System.Classes, System.SysUtils;

procedure HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llError);
end;

function Subscribe(aConnection: TConnection; const aEventName: string): TEventEntry;
begin
  Result := aConnection.eventEntry(aEventName).subscribe;
  if Result.OnString.Count=0 then
  begin
    // no handlers: add new
    Result.OnString.Add(
      procedure(aEventEntry: TEventEntry; const aString: string)
      var
        lines: TStringList;
        l: Integer;
        //lJSON : ISuperObject;
        jsonObject: TJSONObject;
        _type: string;
        _method: string;
        _eventId: string;
      begin
        try
          jsonObject := TJSONObject.Create;
          try
            jsonObject := TJSONObject.ParseJSONValue(aString) as TJSONObject;
            //lJSON := SO(aString);
            if not jsonObject.TryGetValue<string>('type', _type)
            then _type := '##';
            if not jsonObject.TryGetValue<string>('method', _method)
            then _method := '##';
            if jsonObject.TryGetValue<string>('eventId', _eventId) then
            begin
              if _eventId<>''
              then Subscribe(aConnection, _eventId);
            end
            else _eventId:= '';
            Log.WriteLn('on '+aEventName+': '+_type+', '+_method, llNormal, 0);
            lines := TStringList.Create;
            try
              lines.Text := REST.JSON.TJson.Format(jsonObject);
              for l := 0 to lines.Count-1 do
              begin
                Log.WriteLn(lines[l], llDump, 1);
              end;
            finally
              lines.Free;
            end;
          finally
            jsonObject.Free;
          end;
        except
          on e: Exception
          do Log.WriteLn('Exception in OnString: '+e.Message, llError);
        end;
      end);
    Result.OnStreamCreate := function(aEventyEntry: TEventEntry; const aStreamName: string): TStream
      begin
        Log.WriteLn(aEventName+': stream '+aStreamName, llNormal, 0);
        Result := nil;
      end;
    Log.WriteLn('listening on "'+aEventName+'"', llWarning);
  end;
  // else already have an handler on event
end;

var
  connection: TConnection;
begin
  FileLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  ConsoleLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  try
    connection := TSocketConnection.Create('Debugger', 3, 'ecodistrict');
    try
      connection.onDisconnect := HandleDisconnect;
      connection.onException := HandleException;

      Subscribe(connection, 'modules');
      Subscribe(connection, 'dashboard');
      Subscribe(connection, 'data');
      Subscribe(connection, 'data-to-dashboard');

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
