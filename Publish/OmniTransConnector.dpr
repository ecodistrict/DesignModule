program OmniTransConnector;
{
  this is wrapper to the omnitrans model connected with appache nifi via a websocket to imb4
  all data is in JSON from omnitrans side
  connector functions like normal US model on US side
  - listen to specific imb4 channel connected to websocket
  - start model per client
  - wait for modelcontrol to connect the client to a US session
}

// test link: http://vps17642.public.cloudvps.com/?session=OmniTransConnectorA642109

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogFile, LogConsole,
  StdIni,
  MyConsole,
  imb4,
  IMB3NativeClient,
  ModelControllerLib,
  Ora, OraObjects, Data.DB, OraSmart,
  MyOraLib,
  MyStr,
  PublishServerOra,
  JSON, System.Generics.Collections, System.Classes, System.SysUtils;

const
  IMB4RemoteHostSwitch = 'IMB4RemoteHost';
    DefaultIMB4RemoteHost = 'vps17642.public.cloudvps.com';
  IMB4RemotePortSwitch = 'IMB4RemotePort';
    DefaultIMB4RemotePort = 4004;

  WebSocketPrefixSwitch = 'WebSocketPrefix';
    DefaultWebSocketPrefix = 'USIdle.Sessions.WS2IMB';

  OmniTransProjectID = 'OmniTransConnectorA642109';

var
  OmniTransModels: TObjectDictionary<string, TMCModelStarter2>;

type
  TOmniTransModel = class(TMCModelStarter2)
  constructor Create(const aModelName: string; aModelID: Integer; const aClientID: string; aWebSocketConnection: TConnection);
  destructor Destroy; override;
  public
    // model starter
    procedure ParameterRequest(aParameters: TModelParameters); override;
    procedure StartModel(aParameters: TModelParameters); override;
    procedure StopModel; override;
    procedure QuitApplication; override;
  private
    fClientID: string;
    fWebSocketConnection: TConnection; // ref
    fWebSocketEvent: TEventEntry;
    procedure handleWebSocketDataEvent(aEvent: TEventEntry; const aString: string);
    function handleWebSocketDataStreamCreate(aEventEntry: TEventEntry; const aName: string): TStream;
    procedure handleWebSocketDataStreamEnd(aEventEntry: TEventEntry; const aName: string; var aStream: TStream; aCancel: Boolean);
    procedure handleWebSocketControlEvent(aEvent: TEventEntry; aInt: Integer; const aString: string);
  private
    procedure handleMCException(aConnection: TIMBConnection; aException: Exception; var aCallClose: Boolean); stdcall;
    procedure handleMCDisconnect(aConnection: TIMBConnection); stdcall;
  private
    procedure wsSend(const aMessageType: string; const aMessagePayloadJSON: string='{}');
    procedure SignalOTInit();
  private
    fOraSession: TOraSession;
    fTablePrefix: string;
    fScenarioID: Integer;
    fGeneRoadEvent: TIMBEventEntry;
    //fChangeObjectUpdateQueryR: TChangeObjectUpdateQuery;
    //fChangeObjectUpdateQueryL: TChangeObjectUpdateQuery;
    procedure handleGeneRoadEvent(aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;
  end;

{ TOmniTransModel }

constructor TOmniTransModel.Create(const aModelName: string; aModelID: Integer; const aClientID: string; aWebSocketConnection: TConnection);
begin
  fOraSession := nil;
  inherited Create(aModelName, aModelID);
  connection.OnException := handleMCException;
  connection.OnDisconnect := handleMCDisconnect;
  fCLientID := aClientID;
  fWebSocketConnection := aWebSocketConnection;
  fWebSocketEvent := aWebSocketConnection.subscribe(aClientID, False);
  fWebSocketEvent.OnString.Add(handleWebSocketDataEvent);
  fWebSocketEvent.OnStreamCreate := handleWebSocketDataStreamCreate;
  fWebSocketEvent.OnStreamEnd := handleWebSocketDataStreamEnd;
  fWebSocketEvent.OnIntString.Add(handleWebSocketControlEvent);
  SignalOTInit;
end;

destructor TOmniTransModel.Destroy;
begin
  fWebSocketEvent.OnString.Remove(handleWebSocketDataEvent);
  fWebSocketEvent.unSubscribe;
  // signal exit because override of QuitApplication
  SignalModelExit;
  inherited;
  FreeAndNil(fOraSession);
end;

function getFieldDoubleValue(aField: TField): Double;
begin
  if not aField.IsNull
  then Result := aField.AsFloat
  else Result := Double.NaN;
end;

const
  otDirectionRight = 1;
  otDirectionLeft = 2;
  otDirectionBoth = 3;

procedure TOmniTransModel.handleGeneRoadEvent(aAction, aObjectID: Integer; const aObjectName, aAttribute: string);
var
  query: TOraQuery;
  speed: Double;
  capacity: Double;
  res: string;
begin
  Log.WriteLn('road ('+aAction.toString+'): '+aObjectID.toString+' ("'+aAttribute+'"');
  //wsSend('change', '[{"linkid":7234, "direction":1, "speed":80, "cap":2200}]');
  if aAction=actionChange then
  begin
    query := TOraQuery.Create(nil);
    try
      query.Session := fOraSession;
      query.SQL.Text :=
        'SELECT SPEED_R, CAPACITY_R, SPEED_L, CAPACITY_L '+
        'FROM '+fTablePrefix+'GENE_ROAD '+
        'WHERE OBJECT_ID='+aObjectID.ToString;
      query.Execute;
      res := '';
      if not query.Eof then
      begin
        // right
        speed := getFieldDoubleValue(query.FieldByName('SPEED_R'));
        capacity := getFieldDoubleValue(query.FieldByName('CAPACITY_R'));
        if not (speed.isNaN or capacity.isNaN) then
        begin
          if res<>''
          then res := res+',';
          res := res+'{"linkid":'+aObjectID.toString+', "direction":'+otDirectionRight.toString+', "speed":'+speed.toString(dotFormat)+', "cap":'+capacity.toString(dotFormat)+'}';
        end;
        // left
        speed := getFieldDoubleValue(query.FieldByName('SPEED_L'));
        capacity := getFieldDoubleValue(query.FieldByName('CAPACITY_L'));
        if not (speed.isNaN or capacity.isNaN) then
        begin
          if res<>''
          then res := res+',';
          res := res+'{"linkid":'+aObjectID.toString+', "direction":'+otDirectionLeft.toString+', "speed":'+speed.toString(dotFormat)+', "cap":'+capacity.toString(dotFormat)+'}';
        end;
      end;

      wsSend('change', '['+res+']');
      wsSend('start');
    finally
      query.Free;
    end;
  end
  else Log.WriteLn('ignoring action other then change..', llWarning);
end;

procedure TOmniTransModel.handleMCDisconnect(aConnection: TIMBConnection);
begin
  Log.WriteLn('MC disconnect', llWarning);
end;

procedure TOmniTransModel.handleMCException(aConnection: TIMBConnection; aException: Exception; var aCallClose: Boolean);
begin
  Log.WriteLn('MC exception: '+aException.Message, llError);
  aCallClose := False;
end;

procedure TOmniTransModel.handleWebSocketControlEvent(aEvent: TEventEntry; aInt: Integer; const aString: string);
begin
  if aInt=actionDelete then
  begin
    // handle end of this web socket connection
    SignalModelExit;
    QuitApplicationEvent.SetEvent;
    // remove ourselves, which will call destroy..
    TMonitor.Enter(OmniTransModels);
    try
      OmniTransModels.Remove(fClientID);
    finally
      TMonitor.Exit(OmniTransModels);
    end;
    // no actions after this.. (we are destroyed)
  end;
end;

procedure TOmniTransModel.handleWebSocketDataEvent(aEvent: TEventEntry; const aString: string);
var
  msg: TJSONValue;
  msgType: string;
  payload: TJSONObject;
  linkid: Integer;
  direction: Integer;
  load: Double;
  query: TOraSQL;
begin
  try
    msg := TJSONObject.ParseJSONValue(aString);
    if msg.TryGetValue<string>('type', msgType) then
    begin
      if msgType='start_response' then
      begin
        if msg.TryGetValue<TJSONObject>('payload', payload) then
        begin
          if payload.TryGetValue<integer>('linkid', linkid) and payload.TryGetValue<Integer>('direction', direction) and payload.TryGetValue<double>('load', load) then
          begin
            WriteLn('linkid: '+linkid.toString+', direction: '+direction.ToString+', load: '+load.ToString);
            // todo: update link
            if (direction=otDirectionRight) or (direction=otDirectionBoth) then
            begin
              query := TOraSql.Create(nil);
              try
                query.Session := fOraSession;
                query.SQL.Text := 'UPDATE '+fTablePrefix+'GENE_ROAD_INTENSITY SET INTENSITY_R='+load.ToString(dotFormat)+' WHERE OBJECT_ID='+linkid.ToString;
                query.Execute;
              finally
                query.Free;
              end;
              fOraSession.Commit;
              //fGeneRoadEvent.signalChangeObject(actionChange, linkid, 'INTENSITY_R');
            end;

            if (direction=otDirectionLeft) or (direction=otDirectionBoth) then
            begin
              query := TOraSql.Create(nil);
              try
                query.Session := fOraSession;
                query.SQL.Text := 'UPDATE '+fTablePrefix+'GENE_ROAD_INTENSITY SET INTENSITY_L='+load.ToString(dotFormat)+' WHERE OBJECT_ID='+linkid.ToString;
                query.Execute;
              finally
                query.Free;
              end;
              fOraSession.Commit;
              //fGeneRoadEvent.signalChangeObject(actionChange, linkid, 'INTENSITY_L');
            end;

            fOraSession.ExecSQL(
              'UPDATE '+fTablePrefix+'GENE_ROAD_INTENSITY '+
              'SET INTENSITY=INTENSITY_L+INTENSITY_R '+
              'WHERE OBJECT_ID='+linkid.ToString);
            fOraSession.Commit;
            fGeneRoadEvent.signalChangeObject(actionChange, linkid, 'INTENSITY');
          end
          else Log.WriteLn('Invalid start_response in '+aString, llWarning);
        end
        else Log.WriteLn('No payload', llWarning);
      end
      else
      begin
        Log.WriteLn('handleWebSocketEvent: '+msgType);
        Log.WriteLn('json: '+aString, llNormal, 1);
      end;
    end
    else
    begin
      Log.WriteLn('handleWebSocketEvent, unknown format: '+aString, llWarning);
    end;
  except
    on E: Exception
    do Log.WriteLn('Exception in TOmniTransModel.handleWebSocketDataEvent: '+E.Message+' :: '+aString);
  end;
end;

function TOmniTransModel.handleWebSocketDataStreamCreate(aEventEntry: TEventEntry; const aName: string): TStream;
begin
  // build string stream to be able to handle received data as normal string data on end of stream
  Result := TStringStream.Create('', TEncoding.UTF8, False);
end;

procedure TOmniTransModel.handleWebSocketDataStreamEnd(aEventEntry: TEventEntry; const aName: string; var aStream: TStream; aCancel: Boolean);
begin
  // handle streamed data as normal string data
  handleWebSocketDataEvent(aEventEntry, (aStream as TStringStream).DataString);
end;

procedure TOmniTransModel.ParameterRequest(aParameters: TModelParameters);
begin
  Log.WriteLn('parameter request: '+fWebSocketEvent.eventName);
  // todo: implement

  // define parameters that omnitrans can process

end;

procedure TOmniTransModel.QuitApplication;
begin
  Log.WriteLn('quit model: '+fWebSocketEvent.eventName);
  // override default behaviour, no calling inherited function
  // todo: implement sending quit to transform model ?

end;

procedure TOmniTransModel.SignalOTInit;
begin
  //wsSend('session', '{"description":"OmniTransConnector"}');
  //wsSend('login');
  // todo: implement

end;

procedure TOmniTransModel.StartModel(aParameters: TModelParameters);
var
  federation: string;
//  query: TOraQuery;
//  f: Integer;
begin
  Log.WriteLn('start model: '+fWebSocketEvent.eventName);
  // todo: implement

  // parse parameters to json command and send to OmniTrans
  fOraSession.Free;
  fOraSession := TOraSession.Create(nil);
  fOraSession.ConnectString := aParameters.Value[DataSourceParameterName];
  fOraSession.Open;

  federation := aParameters.Value[FederationParameterName];
  fTablePrefix := TablePrefixFromFederation(federation);
  fScenarioID := GetScenarioIDOnTablePrefix(fOraSession, fTablePrefix);

  fGeneRoadEvent := connection.Subscribe('GENE_ROAD');

  //fChangeObjectUpdateQueryIntensityR := TChangeObjectUpdateQuery.Create(fGeneRoadEvent, '', );
  //fChangeObjectUpdateQueryL: TChangeObjectUpdateQuery;

  fGeneRoadEvent.OnChangeObject := handleGeneRoadEvent;
  {
  query := TOraQuery.Create(nil);
  try
    query.Session := fOraSession;
    query.SQL.Text := 'SELECT * FROM META_SCENARIOS WHERE ID='+fScenarioID.toString;
    query.Execute;
    TMonitor.Enter(log);
    try
      while not query.Eof do
      begin
        WriteLn('Scenario ID: '+query.Fields[0].AsString);
        for f := 1 to query.FieldCount-1 do
        begin
          WriteLn('  '+query.FieldDefs[f].Name+': '+query.Fields[f].AsString);
        end;
        query.Next;
      end;
    finally
      TMonitor.Exit(log);
    end;
  finally
    query.Free;
  end;
  }

  wsSend('init');
  //wsSend('start');
  //wsSend('start');

  //wsSend('reset');

  SignalModelState(TModelState.msReady);
end;

procedure TOmniTransModel.StopModel;
begin
  Log.WriteLn('stop model: '+fWebSocketEvent.eventName);
  // todo: implement

  FreeAndNil(fOraSession);
end;

procedure TOmniTransModel.wsSend(const aMessageType, aMessagePayloadJSON: string);
begin
  fWebSocketEvent.signalString('{"type":"'+aMessageType+'","payload":'+aMessagePayloadJSON+'}');
end;

{ main }

procedure handleNewWebSocketClient(aEvent: TEventEntry; aInt: Integer; const aString: string);
var
  clientID: string;
  omniTransModel: TMCModelStarter2;
begin
  try
    if aInt=actionNew then
    begin
      // add client if not already known
      clientID := aString.Split(['&'])[0];
      TMonitor.Enter(OmniTransModels);
      try
        if not OmniTransModels.TryGetValue(clientID, omniTransModel) then
        begin
          Log.WriteLn('inquire response: new client '+aString);
          omniTransModel := TOmniTransModel.Create('OmniTrans', 1, clientID, aEvent.connection);
          OmniTransModels.Add(clientID, omniTransModel);
        end
        else Log.WriteLn('inquire response: existing client '+aString);
      finally
        TMonitor.Exit(OmniTransModels);
      end;
    end;
  except
    on E: Exception
    do Log.WriteLn('Exception in TProject.Create fProjectEvent.OnIntString: '+E.Message, llError);
  end;
end;

procedure handleExceptionWebSocketClient(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('WebSocket IMB4 connection exception: '+aException.Message, llError);
end;

procedure handleDisconnectWebSocketClient(aConnection: TConnection);
begin
  Log.WriteLn('WebSocket IMB4 connection disconnected', llWarning);
end;

procedure ShowHelp();
begin
  System.TMonitor.Enter(Log);
  try
    WriteLn('Options');
    WriteLn('   ? for help');
    WriteLn('   Q or escape to quit');
    WriteLn('   M list of active models');
    // add help for commands here
    WriteLn;
  finally
    System.TMonitor.Exit(Log);
  end;
end;

var
  remoteHost: string;
  remotePort: Integer;
  ws: TConnection;
  webSocketPrefixEventName: string;
  webSocketPrefixEvent: TEventEntry;
  key: Char;
  imp: TPair<string, TMCModelStarter2>;
begin
  // make logger add timestamps etc to log entries in log file
  FileLogger.SetLogDef(AllLogLevels, [llsTime, llsID, llsThreadID]);
  try
    Log.Start();
    try
      remoteHost := GetSetting(IMB4RemoteHostSwitch,  DefaultIMB4RemoteHost);
      remotePort := GetSetting(IMB4RemotePortSwitch,  DefaultIMB4RemotePort);
      ws := TSocketConnection.Create('OmniTransConnector', 0, '', remoteHost, remotePort);
      try
        if ws.connected then
        begin
          Log.WriteLn('Connected to '+remoteHost+':'+remotePort.ToString);
          ws.onDisconnect := handleDisconnectWebSocketClient;
          ws.onException := handleExceptionWebSocketClient;
          OmniTransModels := TObjectDictionary<string, TMCModelStarter2>.Create([doOwnsValues]);
          try
            // link up imb websocket connection
            webSocketPrefixEventName := GetSetting(WebSocketPrefixSwitch, DefaultWebSocketPrefix);
            // listen for new omnitrans model connections
            webSocketPrefixEvent := ws.subscribe(webSocketPrefixEventName+'.'+OmniTransProjectID,False);
            webSocketPrefixEvent.OnIntString.Add(handleNewWebSocketClient);
            // link unlinked omnitrans model connections by inquiring existing sessions
            webSocketPrefixEvent.signalIntString(actionInquire, '');
            // setup is ready

            ShowHelp();
            repeat
              //key := TUSB.KeyPressedWithAppMessges; // to make USB media changes working
              key := KeyPressed2; // normal console key checking
              case key of
                'm', 'M':
                  begin
                    TMonitor.Enter(log);
                    try
                      TMonitor.Enter(OmniTransModels);
                      try
                        if OmniTransModels.Count>0 then
                        begin
                          WriteLn('Connected models: '+OmniTransModels.Count.ToString);
                          for imp in OmniTransModels do
                          begin
                            WriteLn('  '+imp.Key);
                            WriteLn('     connected: '+(imp.Value as TOmniTransModel).Connection.Connected.ToString(False));
                          end;
                        end
                        else WriteLn('No connected models..');
                      finally
                        TMonitor.Exit(OmniTransModels);
                      end;
                    finally
                      TMonitor.Exit(log);
                    end;
                  end;
                '?':ShowHelp();
              end;
            until (key='Q') or (key='q') or (key=#27);

          finally
            OmniTransModels.Free;
          end;
        end
        else Log.WriteLn('Could not connect to '+remoteHost+':'+remotePort.ToString, llError);
      finally
        ws.Free;
      end;
    finally
      Log.Finish();
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
