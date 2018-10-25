unit NDWLib;

// density = speed/flow

interface

uses
  Logger,
  imb4,
  WorldDataCode,
  ByteBuffers,
  IMB3NativeClient,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  MyOraLib,

  PublishServerOra, // CreateWDGeometryFromSDOShape

  System.Generics.Collections,
  System.SyncObjs,
  System.Classes,
  System.SysUtils;

const
  // imb 4 tags
  tag_link_id = icehAttributeBase+1;
  tag_flow = icehAttributeBase+2;
  tag_speed = icehAttributeBase+3;
  tag_length = icehAttributeBase+4;
  tag_hwn = icehAttributeBase+5;
  tag_geometry = icehAttributeBase+6;

  // imb 3 tags (live feed from taoufik)

  // links
  tagNDWTime = 1;
  tagNDWLinkID = 2;
  tagNDWSpeed = 3;
  tagNDWFlow = 4;

  // geometry
  tagNDWGEOLinkID = 6;
  tagNDWGEOLength = 7;
  tagNDWGEOHWN = 8;
  tagNDWGEOCoordinates = 9;

  // route
  tagRouteTravelTime = 10;
  tagRouteTrajectSpeed = 11;
  tagRouteDataBeschikbaarheid = 12;
  tagRouteID = 13;
  tagRouteTrajectTimestamp = 14;

  ORA_BATCHED_QUERY_ARRAY_LENGTH = 1000;

  dotFormat: TFormatSettings = (DecimalSeparator:'.');

  isoDateTimeFormat = 'yyyy-mm-dd hh:nn:ss';
  isoDateTimeFormatSettings: TFormatSettings = (
    DateSeparator:'-';
    TimeSeparator:':';
    ShortDateFormat:'yyyy-mm-dd';
    ShortTimeFormat:'hh:nn:ss'
  );


type
  TChangeObjectUpdateQuery = class
  constructor Create(aEvent: TIMBEventEntry; const aEventAttribute: string; const aConnectString, aTableName, aKeyField, aUpdateField: string; aArrayLength: Integer=ORA_BATCHED_QUERY_ARRAY_LENGTH);
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

  TQueuedObjectID = Integer; // normally TGUID;

  TQueuedObject = class
  // class
  public
    class function getUpdateQueries(): TObjectDictionary<Integer, TChangeObjectUpdateQuery>; virtual; abstract;
    class procedure CommitAllL();
  // instance
  constructor Create(const aID: TQueuedObjectID);
  destructor Destroy; override;
  protected
    fID: TQueuedObjectID;
    fDirty: TList<Integer>;
    function fieldOffset(const aField): Integer;
    function getIsDirty: Boolean;
    procedure markDirty(const aField);
    procedure markCreated;
    procedure updateFieldL(const aField; aValue: Double);
    // field type specific
    function getFieldAsDouble(aFieldOffset: Integer): Double;
    procedure setFieldAsDouble(const aField; aValue: Double);
  public
    property ID: TQueuedObjectID read fID;
    property isDirty: Boolean read getIsDirty;
    function isMarkedDirty(const aField): Boolean;
    procedure resetDirty();
    procedure commit;
  end;

  TNDWUnixTime = Int32; // UInt32 in payload

  TNDWLink = class(TQueuedObject)
  // class
  class constructor Create();
  class destructor Destroy;
  private
    class var fUpdateQueries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
  public
    class function getUpdateQueries(): TObjectDictionary<Integer, TChangeObjectUpdateQuery>; override;
  // instance
  constructor Create(const aID: TQueuedObjectID; const aLinkID: Int64);
  destructor Destroy; override;
  private
    fLinkID: Int64;
    fFlow: Double;
    fFlowLastUpdate: TNDWUnixTime;
    fSpeed: Double;
    fSpeedLastUpdate: TNDWUnixTime;
    fLength: Double;
    fHWN: Integer; // 0,1 HoofdWegenNet yes/no
    fGeometry: TWDGEometry; // owned
    procedure setGeometry(const aValue: TWDGeometry);
  public
    property linkID: Int64 read fLinkID;
    property flow: Double read fFlow;
    property speed: Double read fSpeed;
    property length: Double read fLength write fLength;
    property HWN: Integer read fHWN write fHWN;
    property geometry: TWDGeometry read fGeometry write setGeometry;

    procedure UpdateSpeed(aSpeed: Double; aTime: TNDWUnixTime);
    procedure UpdateFlow(aFlow: Double; aTime: TNDWUnixTime);
  public
    function encode: imb4.TByteBuffer;
    procedure dump;
  end;

  TNDWRoute = class(TQueuedObject)
  // class
  class constructor Create();
  class destructor Destroy;
  private
    class var fUpdateQueries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
  public
    class function getUpdateQueries(): TObjectDictionary<Integer, TChangeObjectUpdateQuery>; override;
  // instance
  constructor Create(const aID: TQueuedObjectID);
  private
    fTimestamp: Integer;
    fTravelTime: Double;
    fTrajectSpeed: Double;
    fDataBeschikbaarheid: Integer;
  public
    property timestamp: Integer read fTimestamp write fTimestamp;
    property travelTime: Double read fTravelTime write fTravelTime;
    property trajectSpeed: Double read fTrajectSpeed write fTrajectSpeed;
    property dataBeschikbaarheid: Integer read fDataBeschikbaarheid write fDataBeschikbaarheid;
  end;

  TNDWConnection = class
  constructor Create(
    const aRemoteHostNDW: string; aRemotePortNDW: Integer; const aFederationNDW: string;
    const aRemoteHostUS: string; aRemotePortUS: Integer; const aFederationUS: string;
    const aTablePrefix, aConnectString: string);
  destructor Destroy; override;
  private
    fConnectionNDW: TIMBConnection;
    fLiveEvent: TIMBEventEntry;
    fConnectionUS: TIMBConnection;
    // oracle
    fSession: TOraSession;
    fTablePrefix: string;
    // handling changes
    fChangedQueue: TObjectList<TQueuedObject>; // refs
    fChangedQueueEvent: TEvent;
    fChangedQueueThread: TThread;
    // links
    fLinks: TObjectDictionary<Int64, TNDWLink>; // owns
    fGeneRoadEvent: TIMBEventEntry;
    // routes
    fRoutes: TObjectDictionary<Integer, TNDWRoute>;

    procedure HandleNDWNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
    procedure addToChangedQueue(aObject: TQueuedObject; const aField);
    procedure processQueues();
  protected
    procedure SaveGeometry(aObjectID: Integer; aGeometry: TWDGeometry);
  public
    property links: TObjectDictionary<Int64, TNDWLink> read fLinks;
    procedure SaveLinkInfoToFile(const aFileName: string);
    procedure LoadLinkInfoFromFile(const aFileName: string);
    procedure dump();
    function FixGeometries(): Integer;
    procedure LoadFromUS();
    procedure SaveIndicatorsUS();
  end;


// only works for line right now..
function geometryToSDOCoords(aGeometry: TWDGeometry; aType: Integer=2002): string;


implementation


function geometryToSDOCoords(aGeometry: TWDGeometry; aType: Integer=2002): string;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  x,y: Single;
begin
  if Assigned(aGeometry) then
  begin
    Result := '';
    for part in aGeometry.parts do
    begin
      for point in part.points do
      begin
        if Result<>''
        then Result := Result+',';
        x := point.x;
        y := point.y;
        Result := Result+x.ToString(dotFormat)+','+y.ToString(dotFormat);
      end;
    end;
    Result := 'MDSYS.SDO_GEOMETRY('+aType.ToString+',NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),MDSYS.SDO_ORDINATE_ARRAY('+result+'))';
  end
  else Result := 'null';
end;

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

constructor TChangeObjectUpdateQuery.Create(aEvent: TIMBEventEntry; const aEventAttribute: string; const aConnectString, aTableName, aKeyField, aUpdateField: string; aArrayLength: Integer);
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

{ TQueuedObject }

procedure TQueuedObject.commit;
var
  fo: Integer;
  query: TChangeObjectUpdateQuery;
  value: Double;
  queries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
begin
  queries := getUpdateQueries();
  for fo in fDirty do
  begin
    // only process if query defined for field offset
    if queries.TryGetValue(fo, query) then
    begin
      value :=  getFieldAsDouble(fo);
      query.Update(fID, value);
    end;
  end;
  fDirty.Clear;
end;

class procedure TQueuedObject.CommitAllL;
var
  queries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
  oqp: TPair<Integer, TChangeObjectUpdateQuery>;
begin
  queries := getUpdateQueries();
  if Assigned(queries) then
  begin
    for oqp in queries
    do oqp.value.Commit;
  end;
end;

constructor TQueuedObject.Create(const aID: TQueuedObjectID);
begin
  fID := aID;
  fDirty := TList<Integer>.Create;
end;

destructor TQueuedObject.Destroy;
begin
  FreeAndNil(fDirty);
  inherited;
end;

function TQueuedObject.getFieldAsDouble(aFieldOffset: Integer): Double;
begin
  move(Pointer(NativeInt(Self)+aFieldOffset)^, Result, SizeOf(Result));
end;

function TQueuedObject.getIsDirty: Boolean;
begin
  Result := fDirty.Count>0;
end;

function TQueuedObject.isMarkedDirty(const aField): Boolean;
begin
  Result := fDirty.IndexOf(fieldOffset(aField))>=0;
end;

function TQueuedObject.fieldOffset(const aField): Integer;
begin
  Result := NativeInt(@aField)-NativeInt(Self);
end;

procedure TQueuedObject.markCreated;
var
  fo: Integer;
begin
  fo := -1;
  if fDirty.IndexOf(fo)<0
  then fDirty.Add(fo);
end;

procedure TQueuedObject.markDirty(const aField);
var
  fo: Integer;
begin
  fo := fieldOffset(aField);
  if fDirty.IndexOf(fo)<0
  then fDirty.Add(fo);
end;

procedure TQueuedObject.resetDirty;
begin
  fDirty.Clear;
end;

procedure TQueuedObject.setFieldAsDouble(const aField; aValue: Double);
begin
  move(aValue, (@aField)^, SizeOf(aValue)); // work-a-round
end;

procedure TQueuedObject.updateFieldL(const aField; aValue: Double);
begin
  setFieldAsDouble(aField, aValue);
  markDirty(aField);
end;

{ TNWDLink }

constructor TNDWLink.Create(const aID: TQueuedObjectID; const aLinkID: Int64);
begin
  inherited Create(aID);
  fFlow := Double.NaN;
  fFlowLastUpdate := -1;
  fSpeed := Double.NaN;
  fSpeedLastUpdate := -1;
  fLength := Double.NaN;
  fHWN := -1;
  fGeometry := nil;
  fLinkID := aLinkID;
end;

class destructor TNDWLink.Destroy;
begin
  FreeAndNil(fUpdateQueries);
end;

class constructor TNDWLink.Create();
begin
  fUpdateQueries := TObjectDictionary<Integer, TChangeObjectUpdateQuery>.Create([doOwnsValues]);
end;

destructor TNDWLink.Destroy;
begin
  FreeAndNil(fGeometry);
  inherited;
end;

procedure TNDWLink.dump;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  write(fLinkID.toString+': '+fFlow.ToString+', '+fSpeed.ToString+', '+fLength.ToString+', '+ord(fHWN).ToString+': ');
  if Assigned(fGeometry) then
  begin
    for part in fGeometry.parts do
    begin
      for point in part.points do
      begin
        Write(point.x.toString+' x '+point.y.toString+' ');
      end;
    end;
  end;
  WriteLn;
end;

function TNDWLink.encode: imb4.TByteBuffer;
begin
  Result :=
    imb4.TByteBuffer.bb_tag_uint64(tag_link_id, fLinkID)+
    imb4.TByteBuffer.bb_tag_double(tag_flow, fFlow)+
    imb4.TByteBuffer.bb_tag_double(tag_speed, fSpeed)+
    imb4.TByteBuffer.bb_tag_double(tag_length, fLength)+
    imb4.TByteBuffer.bb_tag_bool(tag_hwn, fHWN=1);
  if Assigned(fGeometry)
  then Result := Result+
    imb4.TByteBuffer.bb_tag_rawbytestring(tag_geometry, fGeometry.encode);
end;

class function TNDWLink.getUpdateQueries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
begin
  Result := fUpdateQueries;
end;

procedure TNDWLink.setGeometry(const aValue: TWDGeometry);
begin
  if fGeometry <> aValue then
  begin
    fGeometry.Free;
    fGeometry := aValue;
  end;
end;

procedure TNDWLink.UpdateFlow(aFlow: Double; aTime: TNDWUnixTime);
begin
  fFlowLastUpdate := aTime;
  fFlow := aFlow;
end;

procedure TNDWLink.UpdateSpeed(aSpeed: Double; aTime: TNDWUnixTime);
begin
  fSpeedLastUpdate := aTime;
  fSpeed := aSpeed;
end;

{ TRoute }

constructor TNDWRoute.Create(const aID: TQueuedObjectID);
begin
  inherited Create(aID);
  fTimestamp := 0;
  fTravelTime := Double.NaN;
  fTrajectSpeed := Double.NaN;
  fDataBeschikbaarheid := -1;
end;

{ TNDWConnection }

procedure TNDWConnection.addToChangedQueue(aObject: TQueuedObject; const aField);
begin
  if aObject.ID>=0 then
  begin
    if not aObject.isDirty then
    begin
      // cannot lock fChangedLinkQueue because could be swapped with local queue in processing thread
      TMonitor.Enter(fLinks);
      try
        fChangedQueue.Add(aObject);
      finally
        TMonitor.Exit(fLinks);
      end;
    end;
    aObject.markDirty(aField);
    fChangedQueueEvent.SetEvent;
  end;
end;

constructor TNDWConnection.Create(
  const aRemoteHostNDW: string; aRemotePortNDW: Integer; const aFederationNDW: string;
  const aRemoteHostUS: string; aRemotePortUS: Integer; const aFederationUS: string;
  const aTablePrefix, aConnectString: string);
var
  queries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
  link: TNDWLink;
begin
  inherited Create;
  fLinks := TObjectDictionary<Int64, TNDWLink>.Create([doOwnsValues]);
  fChangedQueue := TObjectList<TQueuedObject>.Create(False);
  fTablePRefix := aTablePrefix;
  fRoutes := TObjectDictionary<Integer, TNDWRoute>.Create([doOwnsValues]);
  fConnectionNDW := TIMBConnection.Create(aRemoteHostNDW, aRemotePortNDW, 'NDWlistener', 0, aFederationNDW);
  fConnectionUS := TIMBConnection.Create(aRemoteHostUS, aRemotePortUS, 'NDWlistener', 0, aFederationUS);
  if aConnectString<>'' then
  begin
    // oracle session
    fSession := TOraSession.Create(nil);
    fSession.connectString := aConnectString;
    fSession.Open;
    // event for signaling queue
    fChangedQueueEvent := TEvent.Create(nil, False, False, '');
    // thread to process queued items
    fChangedQueueThread := TThread.CreateAnonymousThread(processQueues);
    fChangedQueueThread.FreeOnTerminate := False;
    fChangedQueueThread.NameThreadForDebugging('NDW link processor');
    fChangedQueueThread.Start;
    // imb events to signal on for queued items
    fGeneRoadEvent := fConnectionUS.Publish('GENE_ROAD');
    // add field update queries
    queries := TNDWLink.getUpdateQueries();
    link := TNDWLink.Create(0, 0);
    try
      queries.Add(link.fieldOffset(link.speed), TChangeObjectUpdateQuery.Create(fGeneRoadEvent, 'SPEED_R', aConnectString, fTablePrefix+'GENE_ROAD', 'OBJECT_ID', 'SPEED_R'));
      queries.Add(link.fieldOffset(link.flow), TChangeObjectUpdateQuery.Create(fGeneRoadEvent, 'INTENSITY', aConnectString, fTablePrefix+'GENE_ROAD_INTENSITY', 'OBJECT_ID', 'INTENSITY'));
    finally
      link.Free;
    end;
    LoadFromUS();
    Log.WriteLn('loaded '+fLinks.Count.toString+' links from '+aConnectString);
  end
  else
  begin
    fSession := nil;
    fChangedQueueEvent := nil;
    fChangedQueueThread := nil;
    fGeneRoadEvent := nil;
  end;
  fLiveEvent := fConnectionNDW.Subscribe('Live');
  fLiveEvent.OnNormalEvent := HandleNDWNormalEvent;
end;

destructor TNDWConnection.Destroy;
begin
  fChangedQueueThread.Terminate;
  fChangedQueueEvent.SetEvent;
  FreeAndNil(fChangedQueueThread);
  fLiveEvent := nil;
  fGeneRoadEvent := nil;
  FreeAndNil(fConnectionNDW);
  FreeAndNil(fConnectionUS);
  FreeAndNil(fChangedQueue);
  FreeAndNil(fLinks);
  FreeAndNil(fRoutes);
  inherited;
  FreeAndNil(fChangedQueueEvent);
  FreeAndNil(fSession);
end;

procedure TNDWConnection.HandleNDWNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  key: Integer; // (tag shl 3) or wiretype
  link: TNDWLink;
  route: TNDWRoute;
  geometry: TWDGeometry;
  i: Integer;
  UpdatedRoutes: Boolean;
  // format specific to spec of feed
  linkID: Int64;
  time: TNDWUnixTime;
  linkSpeed: Integer;
  linkFlow: Integer;
  linkNWDLength: Double;
  linkHWN: Integer;
  x, y: Double;
  geometryLengthInBytes: Integer;
  routeID: Integer;
  routeTravelTime: Double;
  routeTrajectSpeed: Double;
  routeDataBeschikbaarheid: Integer;
  routeTrajectTimestamp: TNDWUnixTime;
begin
  link := nil;
  route := nil;
  UpdatedRoutes := false;
  try
    while aPayload.ReadAvailable>0 do
    begin
      key := aPayload.ReadInteger();
      case key of
        tagNDWTime:
          aPayload.Read(time);
        tagNDWLinkID,
        tagNDWGEOLinkID:
          begin
            aPayload.Read(linkID);
            TMonitor.Enter(fLinks);
            try
              if not fLinks.TryGetValue(linkID, link) then
              begin
                link := TNDWLink.Create(-1, linkID); // mark link as NOT active with object id = -1
                fLinks.Add(linkID, link);
                link.markCreated;
                Log.Progress('added link '+linkID.ToString);
              end;
            finally
              TMonitor.Exit(fLinks);
            end;
          end;
        tagNDWSpeed:
          begin
            aPayload.Read(linkSpeed);
            if Assigned(link) then
            begin
              TMonitor.Enter(link);
              try
                if (link.speed<>linkSpeed) and (linkSpeed>=0) then
                begin
                  link.UpdateSpeed(linkSpeed, time);
                  addToChangedQueue(link, link.speed);
                end;
              finally
                TMonitor.Exit(link);
              end;
            end;
          end;
        tagNDWFlow:
          begin
            aPayload.Read(linkFlow);
            if Assigned(link) then
            begin
              TMonitor.Enter(link);
              try
                if (link.flow<>linkFlow) and (linkFlow>=0) then
                begin
                  link.UpdateFlow(linkFlow, time);
                  addToChangedQueue(link, link.flow);
                end;
              finally
                TMonitor.Exit(link);
              end;
            end;
          end;
        tagNDWGEOLength:
          begin
            aPayload.Read(linkNWDLength);
            if Assigned(link) then
            begin
              TMonitor.Enter(link);
              try
                link.length := linkNWDLength;
                addToChangedQueue(link, link.length);
              finally
                TMonitor.Exit(link);
              end;
            end;
          end;
        tagNDWGEOHWN:
          begin
            aPayload.Read(linkHWN);
            if Assigned(link) then
            begin
              TMonitor.Enter(link);
              try
                link.HWN := linkHWN;
                //Log.WriteLn('link hwn for link '+linkID.ToString+': '+Ord(hwn).ToString);
                addToChangedQueue(link, link.HWN);
              finally
                TMonitor.Exit(link);
              end;
            end;
          end;
        tagNDWGEOCoordinates:
          begin
            aPayload.Read(geometryLengthInBytes);
            geometry := TWDGeometry.Create;
            for i := 0 to (geometryLengthInBytes div 16) -1 do
            begin
              aPayload.Read(x);
              aPayload.Read(y);
              geometry.AddPoint(x, y, Double.NaN);
            end;
            if Assigned(link) then
            begin
              TMonitor.Enter(link);
              try
                link.geometry := geometry;
                addToChangedQueue(link, link.geometry);
              finally
                TMonitor.Exit(link);
              end;
            end;
          end;
        tagRouteID:
          begin
            aPayload.Read(routeID);
            TMonitor.Enter(fRoutes);
            try
              if not fRoutes.TryGetValue(routeID, route) then
              begin
                route := TNDWRoute.Create(routeID);
                fRoutes.Add(routeID, route);
                route.markCreated;
                Log.Progress('added rotue '+routeID.ToString);
              end;
            finally
              TMonitor.Exit(fRoutes);
            end;
          end;
        tagRouteTrajectTimestamp:
          begin
            aPayload.Read(routeTrajectTimestamp);
            if Assigned(route) then
            begin
              TMonitor.Enter(route);
              try
                if route.timestamp<>routeTrajectTimestamp then
                begin
                  route.timestamp := routeTrajectTimestamp;
                  addToChangedQueue(route, route.timestamp);
                end;
              finally
                TMonitor.Exit(route);
              end;
            end;
          end;
        tagRouteTravelTime:
          begin
            aPayload.Read(routeTravelTime);
            if Assigned(route) then
            begin
              TMonitor.Enter(route);
              try
                if route.travelTime<>routeTravelTime then
                begin
                  route.travelTime := routeTravelTime;
                  addToChangedQueue(route, route.travelTime);
                end;
              finally
                TMonitor.Exit(route);
              end;
            end;
          end;
        tagRouteTrajectSpeed:
          begin
            aPayload.Read(routeTrajectSpeed);
            if Assigned(route) then
            begin
              TMonitor.Enter(route);
              try
                if route.trajectSpeed <>routeTrajectSpeed then
                begin
                  route.trajectSpeed := routeTrajectSpeed;
                  addToChangedQueue(route, route.trajectSpeed);
                end;
              finally
                TMonitor.Exit(route);
              end;
            end;
          end;
        tagRouteDataBeschikbaarheid:
          begin
            aPayload.Read(routeDataBeschikbaarheid);
            if Assigned(route) then
            begin
              TMonitor.Enter(route);
              try
                if route.fDataBeschikbaarheid<>routeDataBeschikbaarheid then
                begin
                  route.fDataBeschikbaarheid := routeDataBeschikbaarheid;
                  addToChangedQueue(route, route.fDataBeschikbaarheid);
                end;
                UpdatedRoutes := true;
                Log.Progress('route: '+route.ID.ToString+': '+route.fTravelTime.ToString+'  '+route.fTrajectSpeed.ToString);
              finally
                TMonitor.Exit(route);
              end;
            end;
          end;
      end;
    end;
  finally
    if UpdatedRoutes
    then SaveIndicatorsUS();
  end;
end;

function FixGeometry(aGeometry: TWDGeometry): Boolean;
var
  part: TWDGeometryPart;
  p: Integer;

begin
  Result := False; // assume geoemtry is OK
  for part in aGeometry.parts do
  begin
    // traverse in reverse so when can remove points at the end of the part that ar the same as the predecesor point
    for p := part.points.Count-1 downto 1 do
    begin
      if (part.points[p].x = part.points[p-1].x) and (part.points[p].y = part.points[p-1].y) then
      begin
        part.points.Delete(p);
        Result := True; // geometry is NOT OK and now fixed
      end
      else Break; // break on first good point
    end;
  end;
end;

procedure TNDWConnection.LoadFromUS();
var
  query: TOraQuery;
  objectId: Integer;
  linkID: Int64;
  link: TNDWLink;
begin
  query := TOraQuery.Create(nil);
  try
    // link to session
    query.Session := fSession;
    // optimize
    query.UniDirectional := True;
    query.ReadOnly := True;
    // build and execute sql
    query.SQL.Text :=
      'SELECT '+fTablePrefix+'GENE_ROAD.OBJECT_ID, FNODE_, LENGTH, SPEED_R, INTENSITY, '+fTablePrefix+'GENE_ROAD.SHAPE '+
      'FROM '+fTablePRefix+'GENE_ROAD LEFT JOIN '+fTablePRefix+'GENE_ROAD_INTENSITY '+
        'ON '+fTablePRefix+'GENE_ROAD.OBJECT_ID = '+fTablePRefix+'GENE_ROAD_INTENSITY.OBJECT_ID';
    query.Execute;
    query.First;
    TMonitor.Enter(fLinks);
    try
      while not query.Eof do
      begin
        objectId := query.fields[0].AsInteger;
        if not query.Fields[1].IsNull then
        begin
          linkID := query.Fields[1].AsLargeInt;
          if not fLinks.TryGetValue(linkID, link) then
          begin
            link := TNDWLink.Create(objectID, linkID);
            fLinks.Add(linkID, link);
          end;

          if not query.Fields[2].IsNull
          then link.length := query.Fields[2].AsFloat;
          if not query.Fields[3].IsNull
          then link.UpdateSpeed(query.Fields[3].AsFloat, 0);
          if not query.Fields[4].IsNull
          then link.UpdateFlow(query.Fields[4].AsFloat, 0);
          if not query.Fields[5].IsNull then
          begin
            // todo: project to lat/lan
            link.geometry := CreateWDGeometryFromSDOShape(query, 'shape');
          end;
        end;
        query.Next;
      end;
    finally
      TMonitor.Exit(fLinks);
    end;
  finally
    query.Free;
  end;
end;

procedure TNDWConnection.LoadLinkInfoFromFile(const aFileName: string);
var
  F: File;
  fs: Integer;
  buffer: imb4.TByteBuffer;
  cursor: Integer;
  key: Integer;
  linkID: UInt64;
  flow: Double;
  speed: Double;
  ndw_len: Double;
  hwn: Boolean;
  geometry: TWDGeometry;
  len: UInt32;
  link: TNDWLink;
begin
  link := nil;
  AssignFile(F, aFileName);
  Reset(F, 1);
  try
    fs := FileSize(F);
    setLength(buffer, fs);
    BlockRead(F, buffer[1], fs);
    TMonitor.Enter(fLinks);
    try
      cursor := 0;
      while cursor<Length(buffer) do
      begin
        key := buffer.bb_read_uint32(cursor);
        case key of
          (tag_link_id shl 3) or imb4.wtVarInt:
            begin
              linkID := buffer.bb_read_uint64(cursor);
              if not fLinks.TryGetValue(linkID, link) then
              begin
                link := TNDWLink.Create(-1, linkID); // mark link as NOT active with object id = -1
                fLinks.Add(linkID, link);
              end;
            end;
          (tag_flow shl 3) or imb4.wt64Bit:
            begin
              flow := buffer.bb_read_double(cursor);
              if Assigned(link)
              then link.UpdateFlow(flow, 0);
            end;
          (tag_speed shl 3) or imb4.wt64Bit:
            begin
              speed := buffer.bb_read_double(cursor);
              if Assigned(link)
              then link.UpdateSpeed(speed, 0);
            end;
          (tag_length  shl 3) or imb4.wt64Bit:
            begin
              ndw_len := buffer.bb_read_double(cursor);
              if Assigned(link)
              then link.length := ndw_len;
            end;
          (tag_hwn  shl 3) or imb4.wtVarInt:
            begin
              hwn := buffer.bb_read_bool(cursor);
              if Assigned(link)
              then link.HWN := ord(hwn);
            end;
          (tag_geometry  shl 3) or imb4.wtLengthDelimited:
            begin
              geometry := TWDGeometry.Create;
              try
                len := buffer.bb_read_uint32(cursor);
                geometry.Decode(buffer, cursor, cursor+integer(len));
              finally
                if Assigned(link)
                then link.geometry := geometry;
              end;
            end
        else
          buffer.bb_read_skip(cursor, key and 7);
        end;
      end;
    finally
      TMonitor.Exit(fLinks);
    end;
  finally
    CloseFile(F);
  end;
end;

const
  RouteNames: array[1..8] of string = (
    'A10 Zuid O-W', 'A10 Zuid W-O', 'A10 West O-W', 'A10 West W-O',
    'A9 ZuidOost O-W', 'A9 ZuidOost W-O', 'A9 Zuid O-W', 'A9 Zuid W-O' );

procedure TNDWConnection.SaveIndicatorsUS();
var
  stm: TOraSQL;
  route: TNDWRoute;
  traveltime: double;
begin
  stm := TOraSQL.Create(nil);
  try
    // link to session
    stm.Session := fSession;
    // build and execute sql
    stm.SQL.Text :=
      'TRUNCATE TABLE '+fTablePrefix+'TRAF_INDIC_DAT3';
    stm.Execute;
    TMonitor.Enter(fRoutes);
    try
      for route in fRoutes.Values do
      begin
        traveltime:=route.fTravelTime*60;
        stm.SQL.Text :=
          'INSERT INTO '+fTablePrefix+'TRAF_INDIC_DAT3 (OBJECT_ID, ROUTE, TRAVEL_TIME) '+
          'VALUES (';
        if (route.ID>=1) and (route.ID<=8) then
        begin
          stm.SQL.Text := stm.SQL.Text +
            route.ID.ToString + ',''' + RouteNames[route.ID] + ''',' + traveltime.ToString(dotFormat) + ')';
        end
        else
        begin
          stm.SQL.Text := stm.SQL.Text +
            route.ID.ToString + ', ''route_' + route.ID.ToString + ''',' + traveltime.ToString(dotFormat) + ')';
        end;
        stm.Execute;
      end;
      fSession.Commit;
      fConnectionUS.SignalChangeObject('TRAF_INDIC_DAT', actionChange, 0,'');
    finally
      TMonitor.Exit(fRoutes);
    end;
  finally
    stm.Free;
  end;
end;

procedure TNDWConnection.processQueues;
var
//  ChangeSpeedQuery: TOraSQL;
//  ChangeIntensityQuery: TOraSQL;
  o: TQueuedObject;
//  speedObjectIDParam: TOraParam;
//  speedSpeedParam: TOraParam;
//  intensityObjectIDParam: TOraParam;
//  intensityIntensityParam: TOraParam;
//  speedIndex: Integer;
//  intensityIndex: Integer;
//  i: Integer;
  localQueue, tempQueue: TObjectList<TQueuedObject>;
begin
  // we can use global orasession because only 1 thread uses it at the same time right now..
  {
  ChangeSpeedQuery := TOraSQL.Create(nil);
  ChangeIntensityQuery := TOraSQL.Create(nil);
  }
  localQueue := TObjectList<TQueuedObject>.Create(False); // refs
  try
    {
    ChangeSpeedQuery.SQL.Text :=
      'UPDATE '+fTablePrefix+'GENE_ROAD '+
      'SET SPEED_R=:speed '+
      'WHERE OBJECT_ID=:object_id';
    ChangeSpeedQuery.Prepare;
    ChangeSpeedQuery.ArrayLength := ORA_BATCHED_QUERY_ARRAY_LENGTH;
    speedObjectIDParam := ChangeSpeedQuery.ParamByName('object_id');
    speedSpeedParam := ChangeSpeedQuery.ParamByName('speed');

    ChangeIntensityQuery.SQL.Text :=
      'UPDATE '+fTablePrefix+'GENE_ROAD_INTENSITY '+
      'SET INTENSITY=:intensity '+
      'WHERE OBJECT_ID=:object_id';
    ChangeIntensityQuery.Prepare;
    ChangeIntensityQuery.ArrayLength := ORA_BATCHED_QUERY_ARRAY_LENGTH;
    intensityObjectIDParam := ChangeIntensityQuery.ParamByName('object_id');
    intensityIntensityParam := ChangeIntensityQuery.ParamByName('intensity');
    }
    while not TThread.CheckTerminated do
    begin
      if fChangedQueueEvent.WaitFor()=TWaitResult.wrSignaled then
      begin
        try
          // swap queues
          TMonitor.Enter(fLinks);
          try
            tempQueue := fChangedQueue;
            fChangedQueue := localQueue;
            localQueue := tempQueue;
          finally
            TMonitor.Exit(fLinks);
          end;
          // process local (ie swapped queue)
//          speedIndex := 0;
//          intensityIndex := 0;
          for o in localQueue do
          begin
            TMonitor.Enter(o);
            try
              // check if updatable (ie object_id>=0)
              if o.ID>=0 then
              begin
                if o.isDirty then
                begin
                  o.Commit();

                  // todo:
                  {
                  try
                    if o.isMarkedDirty(o.speed) then
                    begin
                      Log.Progress('New speed '+o.speed.tostring+' for '+o.ID.tostring);
                      speedObjectIDParam.ItemAsInteger[speedIndex+1] := o.ID;
                      speedSpeedParam.ItemAsFloat[speedIndex+1] := o.speed;
                      speedIndex := speedIndex+1;
                      if speedIndex = speedObjectIDParam.Length then
                      begin
                        ChangeSpeedQuery.Execute(speedIndex);
                        fSession.Commit;
                        for i := 0 to speedIndex-1
                        do fGeneRoadEvent.SignalChangeObject(actionChange, speedObjectIDParam.ItemAsInteger[i+1], 'SPEED_R');
                        speedIndex := 0;
                      end;
                    end;
                    if o.isMarkedDirty(o.flow) then
                    begin
                      if o.flow>=0 then
                      begin
                        Log.Progress('New intensity '+o.flow.ToString+' for '+o.ID.tostring);
                        intensityObjectIDParam.ItemAsInteger[intensityIndex+1] := o.ID;
                        intensityIntensityParam.ItemAsFloat[intensityIndex+1] := o.flow; // todo: convert to intensity and avoid negative intensity
                        intensityIndex := intensityIndex+1;
                        if intensityIndex = intensityObjectIDParam.Length then
                        begin
                          ChangeIntensityQuery.Execute(intensityIndex);
                          fSession.Commit;
                          for i := 0 to intensityIndex-1
                          do fGeneRoadEvent.SignalChangeObject(actionChange, intensityObjectIDParam.ItemAsInteger[i+1], 'INTENSITY');
                          intensityIndex := 0;
                        end;
                      end;
                    end;

                  finally
                    o.resetDirty();
                  end;
                  }
                end;
              end;
            finally
              TMonitor.Exit(o);
            end;
          end;
          {
          // process rest items in batched queries
          if speedIndex>0 then
          begin
            ChangeSpeedQuery.Execute(speedIndex);
            fSession.Commit;
            for i := 0 to speedIndex-1
            do fGeneRoadEvent.SignalChangeObject(actionChange, speedObjectIDParam.ItemAsInteger[i+1], 'SPEED_R');
            //speedIndex := 0;
          end;
          if intensityIndex>0 then
          begin
            ChangeIntensityQuery.Execute(intensityIndex);
            fSession.Commit;
            for i := 0 to intensityIndex-1
            do fGeneRoadEvent.SignalChangeObject(actionChange, intensityObjectIDParam.ItemAsInteger[i+1], 'INTENSITY');
            //intensityIndex := 0;
          end;
          // clear all processed entries
          }
          // call commit on all queable classes
          TNDWLink.CommitAllL;
          TNDWRoute.CommitAllL;
        except
         on E: Exception
         do Log.WriteLn('Exception in processing loop: '+E.Message, llError);
        end;
        localQueue.Clear;
      end;
    end;
  finally
//    ChangeSpeedQuery.Free;
//    ChangeIntensityQuery.Free;
    localQueue.Free;
  end;
end;

procedure TNDWConnection.dump();
var
  ilp: TPair<Int64, TNDWLink>;
begin
  TMonitor.Enter(fLinks);
  try
    for ilp in fLinks
    do ilp.Value.dump();
  finally
    TMonitor.Exit(fLinks);
  end;
end;

function TNDWConnection.FixGeometries: Integer;
var
  lilp: TPair<Int64, TNDWLink>;
begin
  Result := 0;
  TMonitor.Enter(fLinks);
  try
    // check geometries
    for lilp in fLinks do
    begin
      if Assigned(lilp.Value.geometry) then
      begin
        if FixGeometry(lilp.Value.geometry)
        then Result := Result+1;
//        begin
          //SaveGeometry(lilp.Value.objectID, lilp.Value.geometry);
          //Log.WriteLn('fixed geometry of road '+lilp.Value.objectID.ToString+' (link: '+lilp.Value.linkID.ToString+')');
//        end;
      end;
    end;
  finally
    TMonitor.Exit(fLinks);
  end;
end;

procedure TNDWConnection.SaveGeometry(aObjectID: Integer; aGeometry: TWDGeometry);
begin
  fSession.ExecSQL(
    'UPDATE '+fTablePrefix+'GENE_ROAD '+
    'SET shape='+geometryToSDOCoords(aGeometry)+' '+
    'WHERE OBJECT_ID='+aObjectID.ToString);
  fSession.Commit;
end;

procedure TNDWConnection.SaveLinkInfoToFile(const aFileName: string);
var
  F: File;
  ilp: TPair<Int64, TNDWLink>;
  buffer: imb4.TByteBuffer;
begin
  AssignFile(F, aFileName);
  Rewrite(F, 1);
  try
    TMonitor.Enter(fLinks);
    try
      for ilp in fLinks do
      begin
        buffer := ilp.Value.encode;
        BlockWrite(F, buffer[1], length(buffer));
      end;
    finally
      TMonitor.Exit(fLinks);
    end;
  finally
    CloseFile(F);
  end;
end;

class constructor TNDWRoute.Create;
begin
  fUpdateQueries := TObjectDictionary<Integer, TChangeObjectUpdateQuery>.Create([doOwnsValues]);
end;

class destructor TNDWRoute.Destroy;
begin
  FreeAndNil(fUpdateQueries);
end;

class function TNDWRoute.getUpdateQueries: TObjectDictionary<Integer, TChangeObjectUpdateQuery>;
begin
  Result := fUpdateQueries;
end;

end.
