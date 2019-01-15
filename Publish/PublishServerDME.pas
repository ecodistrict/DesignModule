unit PublishServerDME;

{
  portal todo's:
  - implement create project
  - update fClientStates

  - time out for open project should be longer (wait cursor?) at least several minutes (like create project?)
  - when OT module is not available parameters gives back error but create project dialog is still shown..
  - unblock can be pressed while waiting to open project -> redirect will occure anyway
  - view mode still allows edit of properties (right click on road -> properties)
  fixed - unlock updates web client but TClient stays in edit mode
  - copy scenario does not add scenario to list

  ..
  middelste project
  copy scenario v178


}

interface

uses
  Logger,
  StdIni,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  MyOraLib,
  MyStr,

  ODBFiles2,

  WorldDataCode,
  WorldLegends,
  WorldJSON,

  IMB3NativeClient,

  imb4,
  WorldTilerConsts,
  CommandQueue,
  TimerPool,

  ModelControllerLib,

  NDWLib,

  Vcl.Imaging.pngimage,

  IdHTTP, // authorization check

  // authorization check 2
  IPPeerClient,
  REST.Client,
  REST.HttpClient,
  REST.Types,

  System.JSON,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  PublishServerOra,
  PublishServerLib,
  PublishServerGIS,
  PublishServerUS,
  PublishServerMCLib;

const
  AuthorizationURLSwitch = 'AuthorizationURL';
    DefaultAuthorizationURL = 'https://vps17642.public.cloudvps.com/auth/';

  //hardcoded bounding box zones for the Park & Shuttle measure
  xMin = '119000';
  xMax = '120600';
  yMin = '482500';
  yMax = '484200';
  targetZone = '719';

  NDWRemoteHostSwitch = 'NDWRemoteHost';
    //NDWRemoteHostDefault = 'vps17642.public.cloudvps.com';
    NDWRemoteHostDefault = 'app-usmodel01.tsn.tno.nl';
  NDWRemotePortSwitch = 'NDWRemotePort';
    NDWRemotePortDefault = 4000;
  NDWRemotePrefixSwitch = 'NDWPrefix';
    //NDWRemotePrefixDefault = 'US_RT.NWB';
    NDWRemotePrefixDefault = 'NDW';

  HSC_SUCCESS_OK = 200;
  HSC_SUCCESS_CREATED = 201;

  HSC_ERROR_BADREQUEST = 400;
  HSC_ERROR_UNAUTHORIZED = 401;
  HSC_ERROR_FORBIDDEN = 403;
  HSC_ERROR_NOT_FOUND = 404;
  HSC_ERROR_CONFLICT = 409;
  HSC_ERROR_NOTIMPLEMENTED = 501;

  //TPortalOpenProjectMode
	popmView = 0;
  popmEdit = 1;



type
  TUSDesignProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer);
  public
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
    procedure handleNewClient(aClient: TClient); override;
  end;

  TUSMonitorProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer);
  destructor Destroy; override;
  private
    fNDWConnection: TNDWConnection;
  public
    procedure ReadBasicData(); override;
  end;

  TUSEvaluateProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer);
  end;

  TPortalProjectState = (
    ppsDisabled, // 0
    ppsUnlocked, // 1
    ppsLocked // 2
  );

  TUSPortalProjectStatus = class
  constructor Create(const aProjectID, aName, aDescription: string; aIcon: TPngImage);
  destructor Destroy; override;
  private
    fProjectID: string;
    fName: string;
    fDescription: string;
    fState: TPortalProjectState;
    fIcon: TPngImage;
  public
    property projectID: string read fProjectID;
    property name: string read fName;
    property description: string read fDescription;
    property state: TPortalProjectState read fState write fState;
    property icon: TPngImage read fIcon;
  end;

  TUSPortal = class; // forward

  TUSPortalProject = class(TUSProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios, aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
    aPortal: TUSPortal; aSourceEPSG: Integer=-1);
  private
    fPortal : TUSPortal; // ref only
    procedure SendRedirect(aClient: TClient; const aNewURL: string);
  protected
    procedure SetEditControlsOnClient(aClient: TClient; aEnabled: Boolean);

    procedure HandleCloseProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
  public
    procedure Login(aClient: TClient; aJSONObject: TJSONObject); override;
  end;

  TUSPortal = class(TMCProject)
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
    aDBConnection: TCustomConnection; aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
    const aAuthorizationURL, aRedirectBackToPortalURL: string; aSourceEPSG: Integer);
  destructor Destroy; override;
  private
    fProjectStatus: TObjectDictionary<string, TUSPortalProjectStatus>;
    fTokenCache: TDictionary<string, TDateTime>;
    fMaxTokenAge: TDateTime;
    fPreLoadScenarios: Boolean;
    fSourceEPSG: Integer;
    fAuthorizationURL: string;
    fRedirectBackToPortalURL: string;
    procedure SendStatus(aClient: TClient; const aResponseType: string; aCode: Integer; const aMessage: string);
    procedure SendRedirect(aClient: TClient; const aNewURL: string);
    procedure SendParameters(aClient: TClient; aParameters: TModelParameters);
  protected
    procedure handleNewClient(aClient: TClient); override;

    procedure HandlePortalLogin(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
    procedure HandlePortalProjectList(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
    procedure HandlePortalOpenProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
    procedure HandlePortalUnlockProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
    procedure HandlePortalGetParameters(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
    procedure HandlePortalCreateProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);

    function UpdateProjectListAsJSON: string;
    function isCreateProjectAllowedAsJSON(aIsCreateProjectAllowed: Boolean): string;
    function RedirectURLPostFix(aOpenMode: Integer): string;
    function isValidCachedToken(const aToken: string): Boolean;
    function isCreateProjectAllowed: Boolean;
  public
    procedure ReadBasicData; override;
    function isAuthorized(aClient: TClient; const aToken: string): Boolean; override;
    procedure handleNotAuthorized(aClient: TClient; aMessage: TJSONObject; const aToken: string); override;
  public
    function FindProjectStatus(const aProjectID: string): TUSPortalProjectStatus;
    procedure UnlockProjectStatus(aPortalProjectStatus: TUSPortalProjectStatus; aClient: TClient);
    // works on list of projects in session model (parent of all projects)
    function FindLocalProject(const aProjectID: string): TUSProject;
    function CreateLocalProject(const aProjectID, aProjectName: string {aPortalProjectStatus: TUSPortalProjectStatus}; const aMapView: TMapView): TUSProject;
    // works on requests of client
    function OpenProject(aPortalProjectStatus: TUSPortalProjectStatus; aOpenMode: Integer; aClient: TClient): TUSProject;
    function CreateProject(const aProjectID, aName, aDescription: string; aParameters: TModelParameters; aClient: TClient; const aMapView: TMapView): TUSProject;
  end;

procedure getUSReadProjectsStatusList(aOraSession: TOraSession; fProjectStatus: TObjectDictionary<string, TUSPortalProjectStatus>);

implementation

procedure getUSReadProjectsStatusList(aOraSession: TOraSession; fProjectStatus: TObjectDictionary<string, TUSPortalProjectStatus>);
var
  query: TOraQuery;
  projectID: string;
  description: string;
  iconStream: TStream;
  icon: TPngImage;
  name: string;
begin
  query := TOraQuery.Create(nil);
  try
    try
      query.Session := aOraSession;
      query.SQL.Text :=
        'SELECT * '+
        'FROM '+PROJECT_TABLE_NAME + ' ' +
        'WHERE ACTIVE != 0 AND UPPER(PROJECT_TYPE) =''PORTAL''';
      query.Open;
      while not query.Eof do
      begin
        projectID := query.FieldByName('PROJECTID').Value;
        if not fProjectStatus.ContainsKey(projectID) then
        begin
          try
            name := query.FieldByName('NAME').Value;
          except
            name := '';
          end;
          try
            description := query.FieldByName('DESCRIPTION').Value;
          except
            description := '';
          end;
          icon := TPngImage.Create;
          try
            iconStream := query.CreateBlobStream(query.FieldByName('ICON'), bmRead);
            try
              icon.LoadFromStream(iconStream);
            finally
              iconStream.Free;
            end;
          except
            icon.LoadFromFile('defaultPortProjectIcon.png');
          end;
          fProjectStatus.Add(projectID, TUSPortalProjectStatus.Create(projectID, name, description, icon));
        end;
        query.Next;
      end;
    except
      on E: Exception
      do Log.WriteLn('Exception getting list of portal projects for '+aOraSession.ConnectString+': '+e.Message, llError);
    end;
  finally
    query.Free;
  end;
end;

function IsValidToken(const aToken: string): Boolean;
var
  i: Integer;
begin
  if length(aToken)>0 then
  begin
    for i := 1 to length(aToken) do
    begin
      if not CharInSet(aToken[i], ['0'..'9', 'A'..'Z', 'a'..'z', '.', '=', '-', '_', '+'])
      then Exit(False);
    end;
    Exit(True);
  end
  else Exit(False);
end;

function CheckAuthorization(const aURL, aToken: string): boolean;
var
  lHTTP: TIdHTTP;
begin
  if IsValidToken(aToken) then
  begin
    lHTTP := TIdHTTP.Create;
    try
      lHTTP.HTTPOptions := lHTTP.HTTPOptions+[hoNoProtocolErrorException];
      lHTTP.Request.CustomHeaders.AddValue('Authorization', aToken);
      lHTTP.Get(aURL);
      Result := lHTTP.ResponseCode=HSC_SUCCESS_OK;
    finally
      lHTTP.Free;
    end;
  end
  else Result := False;
end;

function CheckAuthorization2(const aURL, aToken: string): boolean;
var
  r: TRestRequest;
  c: TRESTClient;
  code: Integer;
begin
  if IsValidToken(aToken) then
  begin
    r := TRestRequest.Create(nil);
    try
      c := TRESTClient.Create(aURL);
      try
        r.Client := c;
        r.AddAuthParameter('Authorization', aToken, TRestRequestParameterKind.pkHTTPHEADER, []);
        try
          r.Execute;
          code := r.Response.StatusCode;
        except
          on e: EHTTPProtocolException
          do code := e.ErrorCode;
        end;
      finally
        c.Free;
      end;
    finally
      r.Free;
    end;
    Result := code=HSC_SUCCESS_OK;
  end
  else Result := False;
end;


{ TUSDesignProject }

constructor TUSDesignProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource, aDBConnection, aMapView, aPreLoadScenarios, True, aMaxNearestObjectDistanceInMeters, aSourceEPSG);
  EnableControl(selectControl);
  EnableControl(measuresControl);
  EnableControl(measuresHistoryControl);
  EnableControl(modelControl);
  EnableControl(controlsControl);
  EnableControl(overviewControl);
end;

procedure TUSDesignProject.handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject);
var
  oraSession: TOraSession;
  oraQuery: TOraQuery;
  measure: TMeasureAction;
  jsonMeasures: TJSONArray;
  jsonMeasure, jsonAction: TJSONValue;
  id, parsedID: string;
  objectID, side: Integer;
  jsonObjectIDs: TJSONArray;
  jsonObjectID: TJSONValue;
  measureFactor: Double;
  factorString, inverseString: string;
  queryText1, queryText2, queryText3, table1, table2, table3, value1, value2: string;
  queryResult: TAllRowsResults;
  rowResult: TSingleRowResult;
  table, queryText, queryTextLeft, queryTextRight, value: string;
  publishEventName: string;
  publishEvent: TIMBEventEntry;
  succeeded: Boolean;
  objectIDs: TList<Integer>;
begin
  inherited;
  oraSession := fDBConnection as TOraSession;
  if Assigned(aScenario) and (aScenario is TUSScenario) then
  begin
    if aJSONObject.TryGetValue<TJSONArray>('applyMeasures', jsonMeasures) then
      for jsonMeasure in jsonMeasures do
        if jsonMeasure.TryGetValue<TJSONValue>('measure', jsonAction) and
           jsonAction.TryGetValue<string>('id', id) and
           FindMeasure(id, measure) then
        begin
          if measure.actionID < 0 then
          begin
            if measure.actionID >= -3 then
            begin
              measureFactor := 1;
              case measure.actionID of
                -1: measureFactor := 0.98; //2%
                -2: measureFactor := 0.95; //5%
                -3: measureFactor := 0.90; //10%
              end;

              table1 := (aScenario as TUSScenario).Tableprefix + 'TRAF_OD';
              table2 := (aScenario as TUSScenario).Tableprefix + 'TRAF_ZONE';
              factorString := (FloatToStr(measureFactor)).Replace(',', '.');
              inverseString := (FloatToStr(1 - measureFactor)).Replace(',', '.');

              queryText1 := 'update ' + table1 + ' t1' +
                ' set' +
                ' t1.CAR_TRIPS = t1.CAR_TRIPS +' +
                ' (' + inverseString + ' * (select coalesce (sum(CAR_TRIPS), 0) FROM' +
                ' (select FROM_ZONE, TO_ZONE, CAR_TRIPS, X_CENTROID, Y_CENTROID FROM (' + table1 + ' t_od inner join ' + table2 + ' t_zone on t_od.TO_ZONE = t_zone.OBJECT_ID)' +
                ' where t_od.FROM_ZONE = t1.FROM_ZONE and t_od.TO_ZONE <> '+ targetZone + ') t2' +
                ' where (t2.X_CENTROID between ' + xMin + ' and ' + xMax + ') and (t2.Y_CENTROID between ' + yMin + ' and ' + yMax + ')' +
                '))' +
                ' where' +
                ' t1.TO_ZONE = ' + targetZone;

              queryText2 := 'update ' + table1 + ' t1' +
                ' set' +
                ' t1.CAR_TRIPS = t1.CAR_TRIPS * ' + factorString +
                ' where' +
                ' t1.FROM_ZONE <> ' + targetZone + ' and t1.TO_ZONE <> ' + targetZone + 'and t1.TO_ZONE in' +
                ' (select TO_ZONE from ' + table1 + ' t_od inner join ' + table2 + ' t_zone on t_od.TO_ZONE = t_zone.OBJECT_ID' +
                ' where' +
                ' t_zone.X_CENTROID between ' + xMin + ' and ' + xMax + ' and t_zone.Y_CENTROID between ' + yMin + ' and ' + yMax + ')';

              oraSession.StartTransaction;
              try
                succeeded := True;
                oraSession.ExecSQL(queryText1);
                oraSession.ExecSQL(queryText2);
                oraSession.Commit;
              except
                succeeded := False;
                oraSession.Rollback;
                //todo: logging transaction failed
              end;
              if succeeded then
              begin
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.TRAF_OD';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                publishEvent.SignalChangeObject(actionChange, 0, 'CAR_TRIPS'); //todo: send object id = 0 in case of everything?
                publishEvent.UnPublish;
              end;
            end
            else if (measure.ActionID >= -12) and (measure.ActionID <= -11) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'GENE_ROAD';
              case measure.ActionID of
                -11: value := '1';
                -12: value := '0';
              end;
              queryText := 'update ' + table +
                ' set' +
                ' STATUS_L  = ' + value + ', STATUS_R = ' + value +
                ' where OBJECT_ID = :A';
              queryTextLeft := 'update ' + table +
                ' set' +
                ' STATUS_L = ' + value +
                ' where OBJECT_ID = :A';
              queryTextRight := 'update ' + table +
                ' set' +
                ' STATUS_R  = ' + value +
                ' where OBJECT_ID = :A';
              if jsonMeasure.TryGetValue<TJSONArray>('selectedObjects', jsonObjectIDs) then
              begin
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_ROAD';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  for jsonObjectID in jsonObjectIDs do
                  begin
                    if jsonObjectID.Value.StartsWith('L-') then
                    begin
                      parsedID := jsonObjectID.Value.Substring(2);
                      side := -1;
                    end
                    else if jsonObjectID.Value.StartsWith('R-') then
                    begin
                      parsedID := jsonObjectID.Value.Substring(2);
                      side := 1;
                    end
                    else
                    begin
                      parsedID := jsonObjectID.Value;
                      side := 0;
                    end;
                    if TryStrToInt(parsedID, objectID) then
                    begin
                      //check if this road exists
                      if aScenario.Layers.ContainsKey('road') and (aScenario.Layers['road'] as TLayer).objects.ContainsKey(AnsiString(objectID.ToString)) then
                      begin
                        if side = -1 then
                          oraSession.ExecSQL(queryTextLeft, [objectID])
                        else if side = 1 then
                          oraSession.ExecSQL(queryTextRight, [objectID])
                        else
                          oraSession.ExecSQL(queryText, [objectID]);
                        oraSession.Commit;
                        if (side = -1) or (side = 0) then
                          publishEvent.SignalChangeObject(actionChange, objectID, 'STATUS_L');
                        if (side = 1) or (side = 0) then
                          publishEvent.SignalChangeObject(actionChange, objectID, 'STATUS_R');
                      end;
                    end;
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              end;
            end
            else if (measure.ActionID >= -29) and (measure.ActionID <= -21) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'GENE_ROAD';
              case measure.ActionID of
                -21: value := '0';
                -22: value := '1500';
                -23: value := '2200';
                -24: value := '4400';
                -25: value := '8800';
                -26: value := '13200';
                -27: value := '17600';
                -28: value := '22000';
                -29: value := '26400';
              end;
              queryText := 'update ' + table +
                ' set' +
                ' CAPACITY_L  = ' + value + ', CAPACITY_R = ' + value +
                ' where OBJECT_ID = :A';
              queryTextLeft := 'update ' + table +
                ' set' +
                ' CAPACITY_L  = ' + value +
                ' where OBJECT_ID = :A';
              queryTextRight := 'update ' + table +
                ' set' +
                ' CAPACITY_R = ' + value +
                ' where OBJECT_ID = :A';
              if jsonMeasure.TryGetValue<TJSONArray>('selectedObjects', jsonObjectIDs) then
              begin
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_ROAD';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  for jsonObjectID in jsonObjectIDs do
                  begin
                  	if jsonObjectID.Value.StartsWith('L-') then
                    begin
                      parsedID := jsonObjectID.Value.Substring(2);
                      side := -1;
                    end
                    else if jsonObjectID.Value.StartsWith('R-') then
                    begin
                      parsedID := jsonObjectID.Value.Substring(2);
                      side := 1;
                    end
                    else
                    begin
                      parsedID := jsonObjectID.Value;
                      side := 0;
                    end;
                    if TryStrToInt(parsedID, objectID) then
                    begin
                      //check if this road exists
                      if aScenario.Layers.ContainsKey('road') and (aScenario.Layers['road'] as TLayer).objects.ContainsKey(AnsiString(objectID.ToString)) then
                      begin
                        if side = -1 then
                          oraSession.ExecSQL(queryTextLeft, [objectID])
                        else if side = 1 then
                          oraSession.ExecSQL(queryTextRight, [objectID])
                        else
                          oraSession.ExecSQL(queryText, [objectID]);
                        oraSession.Commit;
                        if (side = -1) or (side = 0) then
                          publishEvent.SignalChangeObject(actionChange, objectID, 'CAPACITY_L');
                        if (side = 1) or (side = 0) then
                          publishEvent.SignalChangeObject(actionChange, objectID, 'CAPACITY_R');
                      end;
                    end;
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              end;
            end
            else if (measure.ActionID >= -35) and (measure.ActionID <= -31) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'GENE_ROAD';
              case measure.ActionID of
                -31: value := '30';
                -32: value := '50';
                -33: value := '80';
                -34: value := '100';
                -35: value := '120';
              end;
              queryText := 'update ' + table +
                ' set' +
                ' SPEED_L = (SPEED_L * 0) + ' + value + ', SPEED_R = (SPEED_R * 0) + ' + value + //preserves null values!
                ' where OBJECT_ID = :A';
              queryTextLeft := 'update ' + table +
                ' set' +
                ' SPEED_L = (SPEED_L * 0) + ' + value + //preserves null values!
                ' where OBJECT_ID = :A';
              queryTextRight := 'update ' + table +
                ' set' +
                ' SPEED_R = (SPEED_R * 0) + ' + value + //preserves null values!
                ' where OBJECT_ID = :A';
              if jsonMeasure.TryGetValue<TJSONArray>('selectedObjects', jsonObjectIDs) then
              begin
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_ROAD';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  for jsonObjectID in jsonObjectIDs do
                  begin
                    if jsonObjectID.Value.StartsWith('L-') then
                    begin
                      parsedID := jsonObjectID.Value.Substring(2);
                      side := -1;
                    end
                    else if jsonObjectID.Value.StartsWith('R-') then
                    begin
                      parsedID := jsonObjectID.Value.Substring(2);
                      side := 1;
                    end
                    else
                    begin
                      parsedID := jsonObjectID.Value;
                      side := 0;
                    end;
                    if TryStrToInt(parsedID, objectID) then
                    begin
                      //check if this road exists
                      if aScenario.Layers.ContainsKey('road') and (aScenario.Layers['road'] as TLayer).objects.ContainsKey(AnsiString(objectID.ToString)) then
                      begin
                        if side = -1 then
                          oraSession.ExecSQL(queryTextLeft, [objectID])
                        else if side = 1 then
                          oraSession.ExecSQL(queryTextRight, [objectID])
                        else
                          oraSession.ExecSQL(queryText, [objectID]);
                        oraSession.Commit;
                        if (side = -1) or (side = 0) then
                          publishEvent.SignalChangeObject(actionChange, objectID, 'SPEED_L');
                        if (side = 1) or (side = 0) then
                          publishEvent.SignalChangeObject(actionChange, objectID, 'SPEED_R');
                      end;
                    end;
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              end;
            end
            else if (measure.ActionID >= -42) and (measure.ActionID <= -41) then
            begin
              case measure.ActionID of
                -41: value := '* 0.7';
                -42: value := '/ 0.7';
              end;
              table := (aScenario as TUSScenario).Tableprefix + 'TRAF_OD';
              queryText := 'update ' + table +
                ' set' +
                ' CAR_TRIPS = CAR_TRIPS ' + value;
              publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.TRAF_OD';
              publishEvent := fIMB3Connection.publish(publishEventName, false);
              try
                oraSession.ExecSQL(queryText);
                oraSession.Commit;
                publishEvent.SignalChangeObject(actionChange, 0, 'CAR_TRIPS');
              finally
                publishEvent.UnPublish;
              end;
            end
            else if (measure.ActionID >= -54) and (measure.ActionID <= -51) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'GENE_BUILDING';
              case measure.ActionID of
                -51: value := '10';
                -52: value := '100';
                -53: value := '1000';
                -54: value := '10000';
              end;
              queryText := 'update ' + table +
                ' set' +
                ' INHABIT = ' + value + //preserves null values!
                ' where OBJECT_ID = :A';
              if jsonMeasure.TryGetValue<TJSONArray>('selectedObjects', jsonObjectIDs) then
              begin
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_BUILDING';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  for jsonObjectID in jsonObjectIDs do
                  begin
                    if TryStrToInt(jsonObjectID.Value, objectID) then
                    begin
                      //check if this building exists
                      if aScenario.Layers.ContainsKey('building') and (aScenario.Layers['building'] as TLayer).objects.ContainsKey(AnsiString(objectID.ToString)) then
                      begin
                        oraSession.ExecSQL(queryText, [objectID]);
                        oraSession.Commit;
                        publishEvent.SignalChangeObject(actionChange, objectID, 'INHABIT');
                      end;
                    end;
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              end;
            end
            else if (measure.ActionID >= -56) and (measure.ActionID <= -55) then
            begin
              table1 := (aScenario as TUSScenario).Tableprefix + 'GENE_INDUSTRY_SRC';
              table2 := (aScenario as TUSScenario).Tableprefix + 'OPS_SOURCES';
              table3 := (aScenario as TUSScenario).Tableprefix + 'GENE_BUILDING';

              case measure.ActionID of
                -55:
                  begin
                     value1 := '1';
                     value2 := '5000';
                  end;
                -56:
                  begin
                     value1 := '0';
                     value2 := '1';
                  end;
              end;

              queryText1 := 'update ' + table1 +
                ' set' +
                ' ACTIVE = ' + value1 +
                ' where groupname =  ''HSM Steel''';

              queryText2 := 'update ' + table2 +
                ' set' +
                ' FACTOR = ' + value1 +
                ' where OBJECT_ID =  30042';

              queryText3 := 'update ' + table3 +
                ' set' +
                ' INHABIT = ' + value2 +
                ' where OBJECT_ID =  15928';

              oraSession.StartTransaction;
              try
                succeeded := True;
                oraSession.ExecSQL(queryText1);
                oraSession.ExecSQL(queryText2);
                oraSession.ExecSQL(queryText3);
                oraSession.Commit;
              except
                succeeded := False;
                oraSession.Rollback;
                //todo: logging transaction failed
              end;
              if succeeded then
              begin
                queryResult := ReturnAllResults(oraSession, 'SELECT OBJECT_ID FROM ' + table1 + ' WHERE groupname =  ''HSM Steel''');
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_INDUSTRY_SRC';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                for rowResult in queryResult do
                  publishEvent.SignalChangeObject(actionChange, StrToIntDef(rowResult[0], -1), 'ACTIVE'); //todo: send object id = 0 in case of everything?
                publishEvent.UnPublish;

                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.OPS_SOURCES';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                publishEvent.SignalChangeObject(actionChange, 30042, 'FACTOR'); //todo: send object id = 0 in case of everything?
                publishEvent.UnPublish;

                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_BUILDING';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                publishEvent.SignalChangeObject(actionChange, 15928, 'INHABIT'); //todo: send object id = 0 in case of everything?
                publishEvent.UnPublish;
              end;
            end
            else if (measure.ActionID >= -62) and (measure.ActionID <= -61) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'PBLS_CONTROLS';
              case measure.ActionID of
                -61: value := '1';
                -62: value := '0';
              end;
              queryText := 'update ' + table +
                ' set' +
                ' ACTIVE  = ' + value +
                ' where ID = :A';
              if jsonMeasure.TryGetValue<TJSONArray>('selectedObjects', jsonObjectIDs) then
              begin
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.PBLS_CONTROLS';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  for jsonObjectID in jsonObjectIDs do
                  begin
                    if TryStrToInt(jsonObjectID.Value, objectID) then
                    begin
                      //check if this road exists
                      if aScenario.Layers.ContainsKey('control') and (aScenario.Layers['control'] as TLayer).objects.ContainsKey(AnsiString(objectID.ToString)) then
                      begin
                        oraSession.ExecSQL(queryText, [objectID]);
                        oraSession.Commit;
                        publishEvent.SignalChangeObject(actionChange, objectID, 'ACTIVE');
//                        if aScenario is TUSScenario then
//                          (aScenario as TUSScenario).ChangeUSControl(actionChange, objectID, 'control', 'Value');
                      end;
                    end;
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              end;
            end
            else if (measure.ActionID >= -98) and (measure.ActionID <= -71) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'GENE_ROAD';
              case measure.ActionID of
                -71: value := 'A2020_1';
                -72: value := 'A2020_2';
                -73: value := 'A2020_3';
                -74: value := 'A2020_4';
                -75: value := 'A2020_5';
                -76: value := 'A2020_6';
                -77: value := 'A2020_7';
                -78: value := 'A2020_8';
                -79: value := 'A2020_9';
                -80: value := 'A2020_10';
                -81: value := 'A2020_11';
                -82: value := 'A2020_12';
                -83: value := 'B2022_1';
                -84: value := 'B2022_2';
                -85: value := 'B2022_3';
                -86: value := 'B2022_4';
                -87: value := 'B2022_5';
                -88: value := 'B2022_6';
                -89: value := 'B2022_7';
                -90: value := 'B2022_8';
                -91: value := 'B2022_9';
                -92: value := 'B2022_10';
                -93: value := 'B2022_11';
                -94: value := 'B2022_12';
              end;
              queryText := 'select OBJECT_ID FROM ' + table +
                ' where ZONE_TYPE=1 and (zone is null or not zone like ''%' + value + '%'')';

              objectIDs := TList<integer>.Create;
              try
                oraQuery := TOraQuery.Create(nil);
                try
                  oraQuery.Session := oraSession;
                  oraQuery.SQL.Text := queryText;
                  oraQuery.ExecSQL;
                  oraQuery.First;
                  while not oraQuery.Eof do
                  begin
                    objectIDs.Add(oraQuery.FieldByName('OBJECT_ID').AsInteger);
                    oraQuery.Next;
                  end;
                finally
                  oraQuery.Free;
                end;
                queryText := 'update ' + table +
                  ' set' +
                  ' ZONE  = CASE WHEN (ZONE is null or zone = '''') then ''' + value + ''' else zone || '';' +  value + ''' END' +
                  ' where ZONE_TYPE=1 and (zone is null or not zone like ''%' + value + '%'')';
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_ROAD';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  oraSession.ExecSQL(queryText, [objectID]);
                  oraSession.Commit;
                  for objectID in ObjectIDs do
                  begin
                    publishEvent.SignalChangeObject(actionChange, objectID, 'ZONE');
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              finally
                FreeAndNil(ObjectIDs);
              end;
            end
            else if (measure.ActionID = -99) then
            begin
              table := (aScenario as TUSScenario).Tableprefix + 'GENE_ROAD';
              queryText := 'select OBJECT_ID FROM ' + table +
                ' where ZONE_TYPE=1 and (not zone is null or not zone = '''')';
              objectIDs := TList<integer>.Create;
              try
                oraQuery := TOraQuery.Create(nil);
                try
                  oraQuery.Session := oraSession;
                  oraQuery.SQL.Text := queryText;
                  oraQuery.ExecSQL;
                  oraQuery.First;
                  while not oraQuery.Eof do
                  begin
                    objectIDs.Add(oraQuery.FieldByName('OBJECT_ID').AsInteger);
                    oraQuery.Next;
                  end;
                finally
                  oraQuery.Free;
                end;
                queryText := 'update ' + table +
                  ' set' +
                  ' ZONE = null' +
                  ' where ZONE_TYPE=1 and (not zone is null or not zone = '''')';
                publishEventName := oraSession.Username + '#' + aClient.currentScenario.Name + '.GENE_ROAD';
                publishEvent := fIMB3Connection.publish(publishEventName, false);
                try
                  oraSession.ExecSQL(queryText, [objectID]);
                  oraSession.Commit;
                  for objectID in ObjectIDs do
                  begin
                    publishEvent.SignalChangeObject(actionChange, objectID, 'ZONE');
                  end;
                finally
                  publishEvent.UnPublish;
                end;
              finally
                FreeAndNil(ObjectIDs);
              end;
            end
          end
        end;
  end;
end;

procedure TUSDesignProject.handleNewClient(aClient: TClient);
begin
  inherited;
  aClient.CanCopyScenario := True;
end;

{ TUSMonitorProject }

constructor TUSMonitorProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource, aDBConnection, aMapView, aPreLoadScenarios, False, aMaxNearestObjectDistanceInMeters, aSourceEPSG);
  DisableControl(selectControl);
  DisableControl(measuresControl);
  DisableControl(measuresHistoryControl);
  EnableControl(modelControl);
end;

destructor TUSMonitorProject.Destroy;
begin
  FreeAndNil(fNDWConnection);
  inherited;
end;


procedure TUSMonitorProject.ReadBasicData;
begin
  inherited;
  // add nwb layer
  if Assigned(fProjectCurrentScenario) then
  begin
    fNDWConnection := TNDWConnection.Create(
      getSetting(NDWRemoteHostSwitch, NDWRemoteHostDefault),
      getSetting(NDWRemotePortSwitch, NDWRemotePortDefault),
      getSetting(NDWRemotePrefixSwitch, NDWRemotePrefixDefault),
      fIMB3Connection.RemoteHost,
      fIMB3Connection.RemotePort,
      (fProjectCurrentScenario as TUSScenario).Federation,
      (fProjectCurrentScenario as TUSScenario).Tableprefix,
      ConnectStringFromSession(OraSession)
      );
  end;
end;

{ TUSEvaluateProject }

constructor TUSEvaluateProject.Create(aSessionModel: TSessionModel;
  aConnection: TConnection; aIMB3Connection: TIMBConnection; const aProjectID,
  aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string;
  aDBConnection: TCustomConnection; aMapView: TMapView;
  aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer; aSourceEPSG: Integer);
begin
  inherited Create(aSessionModel, aConnection, aIMB3Connection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource, aDBConnection, aMapView, aPreLoadScenarios, False, aMaxNearestObjectDistanceInMeters, aSourceEPSG);
  DisableControl(selectControl);
  DisableControl(measuresControl);
  DisableControl(measuresHistoryControl);
  EnableControl(modelControl);
end;

{ TUSPortalProjectStatus }

constructor TUSPortalProjectStatus.Create(const aProjectID, aName, aDescription: string; aIcon: TPngImage);
begin
  inherited Create;
  fProjectID := aProjectID;
  fName := aName;
  fDescription := aDescription;
  fState := ppsDisabled;
  fIcon := aIcon;
end;

destructor TUSPortalProjectStatus.Destroy;
begin
  FreeAndNil(fIcon);
  inherited;
end;

{ TUSPortalProject }

constructor TUSPortal.Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string; aDBConnection: TCustomConnection;
  aMapView: TMapView; aPreLoadScenarios: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  const aAuthorizationURL, aRedirectBackToPortalURL: string; aSourceEPSG: Integer);
begin
  fProjectStatus := TObjectDictionary<string, TUSPortalProjectStatus>.Create([doOwnsValues]);
  fTokenCache := TDictionary<string, TDateTime>.Create;
  fMaxTokenAge := 30/(24*60); // todo: half hour, parameterize
  fPreLoadScenarios := aPreLoadScenarios;
  fSourceEPSG := aSourceEPSG;
  fAuthorizationURL := aAuthorizationURL;
  fRedirectBackToPortalURL := aRedirectBackToPortalURL;

  inherited Create(aSessionModel, aConnection, aIMB3Connection,
    aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource, aDBConnection,
    false, aMaxNearestObjectDistanceInMeters, aMapView);

  ClientMessageHandlers.AddOrSetValue('RequestPortalLogin', HandlePortalLogin);
  ClientMessageHandlers.AddOrSetValue('RequestPortalProjectList', HandlePortalProjectList);
  ClientMessageHandlers.AddOrSetValue('RequestPortalOpenProject', HandlePortalOpenProject);
  ClientMessageHandlers.AddOrSetValue('RequestPortalParameters', HandlePortalGetParameters);
  ClientMessageHandlers.AddOrSetValue('RequestPortalUnlockProject', HandlePortalUnlockProject);
  ClientMessageHandlers.AddOrSetValue('RequestPortalCreateProject', HandlePortalCreateProject);

  {
  authorized check test code

  if IsAuthorized(nil, 'zeyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiI1YmYyY2EyNjRhNTA0YzE5OThjMzQ1ZmQiLCJpYXQiOjE1NDI3MTgwNTUzMTF9.X9DZGcebrutBMw8JmRQNfGj6avNj5jayJhAyCPxkmd4')
  then Log.WriteLn('Authorized', llOk)
  else Log.WriteLn('NOT Authorized', llWarning);
  }
end;

function TUSPortal.CreateLocalProject(const aProjectID, aProjectName: string {aPortalProjectStatus: TUSPortalProjectStatus}; const aMapView: TMapView): TUSProject;
begin
  // create a design mode project but without the client having default edit rights!
  Result := TUSPortalProject.Create(
    fSessionModel, connection, fIMB3Connection,
    aProjectID, aProjectName,
    tiler.tilerFQDN,
    tiler.tilerStatusURL,
    ControlInterface.DataSource,
    dbConnection,
    aMapView,
    fPreLoadScenarios,
    True, // this is essence a design project so we want to show basic layers
    maxNearestObjectDistanceInMeters,
    self,
    fSourceEPSG);
  fSessionModel.Projects.Add(Result);
end;

function TUSPortal.CreateProject(const aProjectID, aName, aDescription: string; aParameters: TModelParameters;
  aClient: TClient; const aMapView: TMapView): TUSProject;
var
  projectMapView: TMapView;
begin
  // todo: implement
  Result := nil;
  Log.WriteLn('Start creating project '+aProjectID);

  pbls_portal table get base scenario and map view to create local project
  get new available scenario filter
  save local project to pbls_project with scenario filter
  copy base scenario and link to local project with scenario filter


  srcProjectID := 0;
  projectMapView := getUSMapView(dbConnection as TOraSession, mapView, srcProjectID);
  //Result := CreateLocalProject(

  //CopyUSScenario(aProject: TUSProject; aClient: TClient; aSrcID: Integer; const aConnectString: string; aIMB3Connection: TIMBConnection);
  {
  MakeThreadedScenarioCopy(
          scenarioID,
          ConnectStringFromSession(Self.oraSession),
          aClient,
          Self,
          fIMB3Connection);
  }
  // check scenario state
  // copy base scenario to new scenario
  // fill scenario from OTConnector (OmniTrans): parameters, claim, wait, unlcaim
  // open scenario

end;

destructor TUSPortal.Destroy;
begin
  inherited;
  FreeAndNil(fProjectStatus);
  FreeAndNil(fTokenCache);
end;

function TUSPortal.FindLocalProject(const aProjectID: string): TUSProject;
var
  p: TProject;
begin
  //for Sessions
  for p in fSessionModel.Projects do
  begin
    if p.ProjectID.ToUpper=aProjectID.ToUpper
    then Exit(p as TUSProject);
  end;
  Exit(nil);
end;

function TUSPortal.FindProjectStatus(const aProjectID: string): TUSPortalProjectStatus;
begin
  if not fProjectStatus.TryGetValue(aProjectID, Result)
  then Result := nil;
end;

procedure TUSPortal.handleNewClient(aClient: TClient);
begin
  inherited;
  // to avoid race condition on startup that websocket is up but on imb client is not connected to publisher:
  // client can start sending without publisher receiving
  aClient.signalString('{"type":"ready"}');
end;

procedure TUSPortal.handleNotAuthorized(aClient: TClient; aMessage: TJSONObject; const aToken: string);
var
  messageType: string;
begin
  if not aMessage.TryGetValue<string>('type', messageType)
  then messageType := '';
  // build UN-AUTH response
  if messageType.toUpper='RequestPortalProjectList'.toUpper
  then SendStatus(aClient, 'ResponsePortalProjectList', HSC_ERROR_UNAUTHORIZED, 'Unautherized token')
  else if messageType.toUpper='RequestPortalLogin'.toUpper
  then SendStatus(aClient, 'ResponsePortalLogin', HSC_ERROR_UNAUTHORIZED, 'Unautherized token')
  else SendStatus(aClient, 'Response', HSC_ERROR_UNAUTHORIZED, 'Unautherized token');
end;

procedure TUSPortal.HandlePortalCreateProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
var
  m: TCIModelEntry2;
  parameters: TModelParameters;
  parameterList: TJSONArray;
  p: TJSONValue;
  n: string;
  v: TJSONValue;
  parameter: TModelParameter;
  found: Boolean;
  _projectID: string;
  _projectName: string;
  _projectDescription: string;
  _project: TUSProject;
  _projectIcon: TPngImage;
  _projectState: TUSPortalProjectStatus;
  newProjectListJSON: string;
  createProjectAllowedJSON: string;
begin
  // find OT connector model and get default parameters
  for m in controlInterface.Models do
  begin
    if m.ModelName='OTConnector' then
    begin
      if aPayload.TryGetValue<string>('name', _projectName) and aPayload.TryGetValue<string>('description', _projectDescription) then
      begin
        // update paramaters with values passed from client
        if aPayload.TryGetValue<TJSONArray>('parameterList', parameterList) then
        begin
          // we should already have the default parameters on the model because of the earlier parameter list request?
          //controlInterface.RequestModelDefaultParameters;

          // create copy of default parameters
          parameters := TModelParameters.Create(m.DefaultParameters);
          try
            for p in parameterList do
            begin
              if p is TJSONObject then
              begin
                if (p as TJSONObject).TryGetValue<string>('name', n) and (p as TJSONObject).TryGetValue<TJSONValue>('value', v) then
                begin
                  found := False;
                  for parameter in parameters do
                  begin
                    if parameter.Name=n then
                    begin
                      found := True;
                      case parameter.ValueType of
                        mpvtFloat:   parameter.Value := (v as TJSONNumber).AsDouble;
                        mpvtBoolean: parameter.Value := (v as TJSONBool).AsBoolean;
                        mpvtInteger: parameter.Value := (v as TJSONNumber).AsInt;
                      else // mpvtString
                                     parameter.Value := v.Value;
                      end;
                    end
                  end;
                  if not found
                  then Log.WriteLn('TUSPortal.HandlePortalCreateProject: parameter "'+n+'" not found to set value ['+v.Value+'] for', llWarning);
                end
                else
                begin
                  Log.WriteLn('TUSPortal.HandlePortalCreateProject: malformed parameter '+p.ToJSON, llError);
                  SendStatus(
                    aClient,
                    'ResponsePortalCreateProject',
                    HSC_ERROR_BADREQUEST,
                    'malformed parameter');
                  Exit;
                end;
              end;
            end;
            // we now have a list of parameters to call create project with
            // create project id
            _projectID := TGUID.NewGuid.ToString;
            _project := CreateProject(_projectID, _projectName, _projectDescription, parameters, aClient, mapView);
            if Assigned(_project) then
            begin
              _projectIcon := nil; // todo: get default image
              _projectState := TUSPortalProjectStatus.Create(_projectID, _projectName, _projectDescription, _projectIcon);
              _projectState.state := ppsUnlocked;
              fProjectStatus.Add(_projectState.projectID, _projectState);
              // send redirect url to client
              SendRedirect(aClient, _project.ClientURL);
              // update "create project is allowed" state
              createProjectAllowedJSON := isCreateProjectAllowedAsJSON(isCreateProjectAllowed);
              forEachClient(
                procedure(aClient: TCLient)
                begin
                  // only send if already validated once before
                  if aClient.lastValidToken<>'' then
                  begin
                    aClient.signalString(createProjectAllowedJSON);
                  end;
                end);
              // send updated project list to all clients
              newProjectListJSON := UpdateProjectListAsJSON;
              forEachClient(
                procedure(aClient: TCLient)
                begin
                  // only send if already validated once before
                  if aClient.lastValidToken<>'' then
                  begin
                    aClient.signalString(newProjectListJSON);
                  end;
                end);
            end
            else
            begin
              SendStatus(
                aClient,
                'ResponsePortalCreateProject',
                HSC_ERROR_CONFLICT,
                'could not create project');
            end;
          finally
            parameters.Free;
          end;
        end
        else
        begin
          Log.WriteLn('TUSPortal.HandlePortalCreateProject: missing parameter list in '+aPayload.ToJSON, llError);
          SendStatus(
            aClient,
            'ResponsePortalCreateProject',
            HSC_ERROR_BADREQUEST,
            'missing parameter list');
        end;
      end
      else
      begin
        Log.WriteLn('TUSPortal.HandlePortalCreateProject: missing name and/or description '+aPayload.ToJSON, llError);
        SendStatus(
          aClient,
          'ResponsePortalCreateProject',
          HSC_ERROR_BADREQUEST,
          'missing project name or description');
      end;
      Exit;
    end;
  end;
  // when we come to this point the OT connector model is not found
  SendStatus(
    aClient,
    'ResponsePortalCreateProject',
    HSC_ERROR_NOT_FOUND,
    'OT model not available');
end;

procedure TUSPortal.HandlePortalGetParameters(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
var
  m: TCIModelEntry2;
  parameters: TModelParameters;
  parameter: TModelParameter;
begin
  // find OT connector model and get default parameters
  for m in controlInterface.Models do
  begin
    if m.ModelName='OTConnector' then
    begin
      // request default parameters
      controlInterface.RequestModelDefaultParameters(m);
      parameters := TModelParameters.Create();
      try
        // filter parameters
        for parameter in m.DefaultParameters do
        begin
          //
          {
          ('OT2US Car Matrix -> Traf_OD', mpvtBoolean, False, ())
          ('OT2US Freight Matrix -> Traf_OD', mpvtBoolean, False, ())
          ('OT2US copy traf_od to ref_od', mpvtBoolean, False, ())
          ('OT2US Skim Car TravelTime -> Traf_OD', mpvtBoolean, False, ())
          ('OT2US Skim PT TravelTime -> Traf_OD', mpvtBoolean, False, ())
          ('OT2US links+Nodes -> gene_road + gene_node', mpvtBoolean, False, ())
          ('OT2US BPRs -> Traf_linetypes', mpvtBoolean, False, ())
          ('OT2US Zones -> Traf_zone', mpvtBoolean, False, ())
          ('OT2US districts -> gene_district (Not yet implemented)', mpvtBoolean, False, ())
          ('OT RUN (flow, turndelay, junction delay will sync. automatically)', mpvtBoolean, False, ())
          ('OT2US Environment Zone', mpvtBoolean, False, ())
          ('US2OT Controls -> OT', mpvtBoolean, False, ())
          ('US2OT Links -> OT', mpvtBoolean, False, ())
          ('OT Project', mpvtString, 'OT812_150215_Zuidas_v2a', ())
          ('OT Project Variant', mpvtString, 'A_KnipOnderA10', ('A_KnipOnderA10', 'Basis', 'B_KipBovenA10', 'C_EenrichtingMahler', 'D_KleinRondjeZuidas', 'E_EenrichtingAlles', 'F_KnipEenrichtingAlles', 'G_CombiKnipEenrichting', 'Play'))
          }
          if (parameter.name='OT Project') or
             (parameter.name='OT Project Variant') then
          begin
            parameters.Add(TModelParameter.Create(parameter));
          end;
        end;
        // build response and send to client
        SendParameters(aClient, parameters);
      finally
        parameters.Free;
      end;
      Exit;
    end;
  end;
  // when we come to this point the OT connector model is not found
  SendStatus(
    aClient,
    'ResponsePortalParameters',
    HSC_ERROR_NOT_FOUND,
    'OT model not available');
end;

procedure TUSPortal.HandlePortalLogin(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
begin
  SendStatus(aClient, 'ResponsePortalLogin', HSC_SUCCESS_OK, 'Successfull login');
  aClient.signalString(isCreateProjectAllowedAsJSON(isCreateProjectAllowed));
end;

procedure TUSPortal.HandlePortalOpenProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
var
  portalProjectStatus: TUSPortalProjectStatus;
  newProjectListJSON: string;
  mode: Integer;
  id: string;
  project: TUSProject;
  createProjectAllowedJSON: string;
begin
  if aPayload.TryGetValue<string>('projectID', id) then
  begin
    portalProjectStatus := FindProjectStatus(id);
    if Assigned(portalProjectStatus) then
    begin
      if aPayload.TryGetValue<integer>('mode', mode) then
      begin
        if (mode=popmView) or (portalProjectStatus.state<ppsLocked) then
        begin
          if mode=popmView then
          begin
            // make sure minimum project state is now: unlocked
            if portalProjectStatus.state<ppsUnlocked
            then portalProjectStatus.state := ppsUnlocked;
          end
          else
          begin
            // lock project
            portalProjectStatus.state := ppsLocked;
          end;
          // send new list to all clients
          newProjectListJSON := UpdateProjectListAsJSON;
          forEachClient(
            procedure(aClient: TCLient)
            begin
              // only send if already validated once before
              if aClient.lastValidToken<>''
              then aClient.signalString(newProjectListJSON);
            end);
          // open project
          try
            project := OpenProject(portalProjectStatus, mode, aClient);
            if Assigned(project) then
            begin
              // respond to client with new status
              SendStatus(
                aClient,
                'ResponsePortalOpenProject',
                HSC_SUCCESS_OK,
                'Project open succeeded');
              // send redirect
              SendRedirect(aClient, project.ClientURL+RedirectURLPostFix(mode));
              // update "create project is allowed" state
              createProjectAllowedJSON := isCreateProjectAllowedAsJSON(isCreateProjectAllowed);
              forEachClient(
                procedure(aClient: TCLient)
                begin
                  // only send if already validated once before
                  if aClient.lastValidToken<>'' then
                  begin
                    aClient.signalString(createProjectAllowedJSON);
                  end;
                end);
            end
            else
            begin
              SendStatus(
                aClient,
                'ResponsePortalOpenProject',
                HSC_ERROR_CONFLICT,
                'Project open failed');
            end;
          except
            on E: Exception do
            begin
              SendStatus(
                aClient,
                'ResponsePortalOpenProject',
                HSC_ERROR_CONFLICT,
                'Project open failed: '+E.Message);
            end;
          end;
        end
        else
        begin
          SendStatus(
            aClient,
            'ResponsePortalOpenProject',
            HSC_ERROR_BADREQUEST,
            'ProjectID defined in RequestPortalUnlockProject is already locked');
        end;
      end
      else
      begin
        SendStatus(
          aClient,
          'ResponsePortalOpenProject',
          HSC_ERROR_BADREQUEST,
          'mode not defined in RequestPortalOpenProject');
      end;
    end
    else
    begin
      SendStatus(
        aClient,
        'ResponsePortalOpenProject',
        HSC_ERROR_BADREQUEST,
        'Unknown projectID defined in RequestPortalOpenProject');
    end;
  end
  else
  begin
    SendStatus(
      aClient,
      'ResponsePortalOpenProject',
      HSC_ERROR_BADREQUEST,
      'projectID not defined in RequestPortalOpenProject');
  end;
end;

procedure TUSPortal.HandlePortalProjectList(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
var
  o: TWDJSONObject;
  projectInfo: TPair<string, TUSPortalProjectStatus>;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', 'ResponsePortalProjectList');
      with addObject('payload') do
      begin
        add('status', HSC_SUCCESS_OK);
        add('message', 'Successfull request of list of projects');
        with addArray('projectList') do
        begin
          for projectInfo in fProjectStatus do
          begin
            with addObject do
            begin
              add('projectID', projectInfo.Value.projectID);
              add('name', projectInfo.Value.name);
              add('description', projectInfo.Value.description);
              add('state', Ord(projectInfo.Value.state));
              add('icon', ImageToBase64(projectInfo.Value.icon));
            end;
          end;
        end;
      end;
    end;
    aClient.signalString(o.JSON());
  finally
    o.Free;
  end;
end;

procedure TUSPortal.HandlePortalUnlockProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
var
  portalProjectStatus: TUSPortalProjectStatus;
  id: string;
  project: TUSPortalProject;
begin
  if aPayload.TryGetValue('projectID', id) then
  begin
    portalProjectStatus := FindProjectStatus(id);
    if Assigned(portalProjectStatus) then
    begin
      if portalProjectStatus.state=ppsLocked then
      begin
        // unlock project
        project := FindLocalProject(id) as TUSPortalProject;
        if Assigned(project) then
        begin
          // change mode to view mode for all connected clients to this project
          project.forEachClient(
            procedure(aClient: TClient)
            begin
              if aClient.editMode=popmEdit.ToString then
              begin
                aClient.editMode := popmView.ToString;
                project.SetEditControlsOnClient(aClient, False);
                aClient.SendMessage(
                  'Edit mode is disabled due to explicit unlock command from portal',
                  TMessageType.mtWarning,
                  5000);
              end;
            end);
        end;
        // unlock project itself and update all connected portal clients
        UnlockProjectStatus(portalProjectStatus, aClient);
        // respond to client with new status
        SendStatus(
          aClient,
          'ResponsePortalUnlockProject',
          HSC_SUCCESS_OK,
          'Project is now unlocked');

      end
      else
      begin
        SendStatus(
          aClient,
          'ResponsePortalUnlockProject',
          HSC_ERROR_BADREQUEST,
          'ProjectID defined in RequestPortalUnlockProject is not locked');
      end;
    end
    else
    begin
      SendStatus(
        aClient,
        'ResponsePortalUnlockProject',
        HSC_ERROR_BADREQUEST,
        'Unknown projectID defined in RequestPortalUnlockProject');
    end;
  end
  else
  begin
    SendStatus(
      aClient,
      'ResponsePortalUnlockProject',
      HSC_ERROR_BADREQUEST,
      'projectID not defined in RequestPortalUnlockProject');
  end;
end;

function TUSPortal.isAuthorized(aClient: TClient; const aToken: string): Boolean;
begin
  if aToken<>'' then
  begin
    // check cache first
    Result := isValidCachedToken(aToken);
    // if not validated token check auth service
    if not Result then
    begin
      try
        Result := CheckAuthorization2(fAuthorizationURL, aToken);
        if Result
        then fTokenCache.AddOrSetValue(aToken, Now);
      except
        on E: Exception do
        begin
          Log.WriteLn('Could not check authorization: '+e.Message, llError);
          Result := False;
        end;
      end;
    end;
  end
  else
  begin
    Result := False;
    Log.WriteLn('No token provided for authorization', llError);
  end;
end;

function TUSPortal.isCreateProjectAllowed: Boolean;
var
  projectStatus: TPair<string, TUSPortalProjectStatus>;
begin
  for projectStatus in fProjectStatus do
  begin
    if projectStatus.Value.state=TPortalProjectState.ppsLocked
    then Exit(False);
  end;
  Exit(True);
end;

function TUSPortal.isCreateProjectAllowedAsJSON(aIsCreateProjectAllowed: Boolean): string;
var
  o: TWDJSONObject;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', 'UpdateIsCreateProjectAllowed');
      with addObject('payload') do
      begin
        add('isAllowed', aIsCreateProjectAllowed);
      end;
    end;
    Result := o.JSON();
  finally
    o.Free;
  end;
end;

function TUSPortal.isValidCachedToken(const aToken: string): Boolean;
var
  timestampToken: TDateTime;
begin
  if aToken<>'' then
  begin
    if fTokenCache.tryGetValue(aToken, timestampToken) then
    begin
      // check if still valid
      if timestampToken+fMaxTokenAge>=now then
      begin
        Result := True;
      end
      else
      begin
        fTokenCache.Remove(aToken);
        Result := False;
      end;
    end
    else Result := False;
  end
  else Result := False;
end;

function TUSPortal.OpenProject(aPortalProjectStatus: TUSPortalProjectStatus; aOpenMode: Integer; aClient: TClient): TUSProject;
var
  projectMapView: TMapView;
begin
  Result := FindLocalProject(aPortalProjectStatus.projectID);
  if not Assigned(Result) then
  begin
    projectMapView := getUSMapView(dbConnection as TOraSession, mapView, aPortalProjectStatus.projectID);
    Result := CreateLocalProject(aPortalProjectStatus.projectID, aPortalProjectStatus.name, projectMapView);
    if aOpenMode=om then

  end;
  Log.WriteLn('Open project '+aPortalProjectStatus.projectID+' in mode '+aOpenMode.ToString);
end;

procedure TUSPortal.ReadBasicData;
var
  p: TPair<string, TUSPortalProjectStatus>;
begin
  inherited;
  getUSReadProjectsStatusList(fDBConnection as TOraSession, fProjectStatus);
  // unlock all projects
  for p in fProjectStatus
  do p.Value.state := ppsUnlocked;
  Log.WriteLn('Read '+fProjectStatus.Count.toString+' portal projects');
end;

function TUSPortal.RedirectURLPostFix(aOpenMode: Integer): string;
begin
  Result := '&editMode='+aOpenMode.toString;
end;

procedure TUSPortal.SendParameters(aClient: TClient; aParameters: TModelParameters);
var
  o: TWDJSONObject;
  parameter: TModelParameter;
  vv: Variant;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', 'ResponsePortalParameters');
      with addObject('payload') do
      begin
        add('status', HSC_SUCCESS_OK);
        add('message', 'Parameter list read');
        with addArray('parameterList') do
        begin
          if Assigned(aParameters) then
          begin
            for parameter in aParameters do
            begin
              with addObject do
              begin
                add('name', parameter.Name);
                add('description', parameter.Name); // todo: no description available yet
                add('type', Ord(parameter.ValueType));
                case parameter.ValueType of
                  mpvtFloat:
                    begin
                      add('defaultValue', Double(parameter.Value));
                      if length(parameter.ValueList)>0 then
                        with addArray('selection')
                        do for vv in parameter.ValueList
                           do add(Double(vv));
                    end;
                  mpvtBoolean:
                    begin
                      add('defaultValue', Boolean(parameter.Value));
                      if length(parameter.ValueList)>0 then
                        with addArray('selection')
                        do for vv in parameter.ValueList
                           do add(Boolean(vv));
                    end;
                  mpvtInteger:
                    begin
                      add('defaultValue', Integer(parameter.Value));
                      if length(parameter.ValueList)>0 then
                        with addArray('selection')
                        do for vv in parameter.ValueList
                           do add(Integer(vv));
                    end
                else
                  //mpvtString..
                      add('defaultValue', parameter.ValueAsString);
                      if length(parameter.ValueList)>0 then
                        with addArray('selection')
                        do for vv in parameter.ValueList
                           do add(string(vv));
                end;
                add('freeEdit', length(parameter.ValueList)=0);
              end;
            end;
          end;
        end;
      end;
    end;
    aClient.signalString(o.JSON());
  finally
    o.Free;
  end;
end;

procedure TUSPortal.SendRedirect(aClient: TClient; const aNewURL: string);
var
  o: TWDJSONObject;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', 'Redirect');
      with addObject('payload') do
      begin
        add('url', aNewURL);
      end;
    end;
    aClient.signalString(o.JSON());
  finally
    o.Free;
  end;
end;

procedure TUSPortal.SendStatus(aClient: TClient; const aResponseType: string; aCode: Integer; const aMessage: string);
var
  o: TWDJSONObject;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', aResponseType);
      with addObject('payload') do
      begin
        add('status', aCode);
        add('message', aMessage);
      end;
    end;
    aClient.signalString(o.JSON());
  finally
    o.Free;
  end;
end;

procedure TUSPortal.UnlockProjectStatus(aPortalProjectStatus: TUSPortalProjectStatus; aClient: TClient);
var
  createProjectAllowedJSON: string;
  newProjectListJSON: string;
begin
  aPortalProjectStatus.state := ppsUnlocked;
  // update "create project is allowed" state
  createProjectAllowedJSON := isCreateProjectAllowedAsJSON(isCreateProjectAllowed);
  forEachClient(
    procedure(aClient: TCLient)
    begin
      // only send if already validated once before
      if aClient.lastValidToken<>'' then
      begin
        aClient.signalString(createProjectAllowedJSON);
      end;
    end);
  // send new list to all clients
  newProjectListJSON := UpdateProjectListAsJSON;
  forEachClient(
    procedure(aClient: TCLient)
    begin
      // only send if already validated once before
      if aClient.lastValidToken<>''
      then aClient.signalString(newProjectListJSON);
    end);
end;

function TUSPortal.UpdateProjectListAsJSON: string;
var
  o: TWDJSONObject;
  projectInfo: TPair<string, TUSPortalProjectStatus>;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', 'UpdatePortalProjectList');
      with addObject('payload') do
      begin
        with addArray('projectList') do
        begin
          for projectInfo in fProjectStatus do
          begin
            with addObject do
            begin
              add('projectID', projectInfo.Value.projectID);
              add('name', projectInfo.Value.name);
              add('description', projectInfo.Value.description);
              add('state', Ord(projectInfo.Value.state));
              add('icon', ImageToBase64(projectInfo.Value.icon));
            end;
          end;
        end;
      end;
    end;
    Result := o.JSON();
  finally
    o.Free;
  end;
end;

{ TUSPortalProject }

constructor TUSPortalProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; aIMB3Connection: TIMBConnection;
  const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource: string; aDBConnection: TCustomConnection;
  aMapView: TMapView; aPreLoadScenarios, aAddBasicLayers: Boolean; aMaxNearestObjectDistanceInMeters: Integer;
  aPortal: TUSPortal; aSourceEPSG: Integer);
begin
  fPortal := aPortal;
  inherited Create(aSessionModel, aConnection, aIMB3Connection,
    aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, aDataSource, aDBConnection,
    aMapView, aPreLoadScenarios, aAddBasicLayers, aMaxNearestObjectDistanceInMeters, aSourceEPSG);
  ClientMessageHandlers.AddOrSetValue('closeProject', HandleCloseProject);
end;

procedure TUSPortalProject.SetEditControlsOnClient(aClient: TClient; aEnabled: Boolean);
var
  controlState: string;
begin
  if aEnabled then
  begin
    aClient.CanCopyScenario := True;
    controlState := controlEnabled
  end
  else
  begin
    aClient.CanCopyScenario := False;
    controlState := controlDisabled;
  end;
  aClient.Control[selectControl] := controlState;
  aClient.Control[measuresControl] := controlState;
  aClient.Control[measuresHistoryControl] := controlState;
  aClient.Control[modelControl] := controlState;
  aClient.Control[controlsControl] := controlState;
  aClient.Control[overviewControl] := controlState;
  aClient.Control[closeProjectControl] := '{"url":"'+fPortal.fRedirectBackToPortalURL+'"}';
  aClient.signalControls;
end;

procedure TUSPortalProject.HandleCloseProject(aProject: TProject; aClient: TClient; const aType: string; aPayload: TJSONValue);
var
  redirectURL: string;
  unlock: Boolean;
  portalProjectStatus: TUSPortalProjectStatus;
begin
  if aPayload.TryGetValue<string>('url', redirectURL)
  then SendRedirect(aClient, redirectURL)
  else Log.WriteLn('Did not receive redirect url in HandleCloseProject', llError);
  // client is closing project so no need to update edit mode to view mode for this client (?)
  // check if we should unlock the project
  if aClient.editMode=popmEdit.ToString then
  begin
    // we were editing so unlock if no one else is editing
    unlock := True;
    forEachClient(
      procedure(aClient2: TClient)
      begin
        if (aClient2<>aClient) and (aClient2.editMode=popmEdit.ToString)
        then unlock := False;
      end);
    if unlock then
    begin
      portalProjectStatus := fPortal.FindProjectStatus(aProject.ProjectID);
      if Assigned(portalProjectStatus) then
      begin
        fPortal.UnlockProjectStatus(portalProjectStatus, aClient);
      end;
    end;
  end;
end;

procedure TUSPortalProject.Login(aClient: TClient; aJSONObject: TJSONObject);
begin
  inherited;
  // set controls like design mode or us mode based on edit mode request of
  SetEditControlsOnClient(aClient, aClient.editMode=popmEdit.ToString);
end;

procedure TUSPortalProject.SendRedirect(aClient: TClient; const aNewURL: string);
var
  o: TWDJSONObject;
begin
  o := nil;
  try
    with WDJSONCreate(o) do
    begin
      add('type', 'Redirect');
      with addObject('payload') do
      begin
        add('url', aNewURL);
      end;
    end;
    aClient.signalString(o.JSON());
  finally
    o.Free;
  end;
end;

end.
