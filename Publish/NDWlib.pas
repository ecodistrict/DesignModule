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

  dirtySpeed = 1;
  dirtyFlow = 2;
  dirtyLength = 4;
  dirtyHWN = 8;
  dirtyGeometry = 16;

  ORA_BATCHED_QUERY_ARRAY_LENGTH = 1000;


type
  TNDWUnixTime = Int32; // UInt32 in payload
  TNDWLinkID = Int64; // UInt64 in payload
  TNDWFlow = Int32;
  TNDWSpeed = Int32;
  TNDWCoordinate = Double;

  TNDWLink = class
  constructor Create(aLinkID: TNDWLinkID; aObjectID: Integer);
  destructor Destroy; override;
  private
    fLinkID: TNDWLinkID;
    fObjectID: Integer;
    fFlow: Double;
    fFlowLastUpdate: TNDWUnixTime;
    fSpeed: Double;
    fSpeedLastUpdate: TNDWUnixTime;
    fLength: Double;
    fHWN: Integer; // 0,1
    fGeometry: TWDGEometry; // owned
    fDirty: Integer;
    procedure setGeometry(const aValue: TWDGeometry);
  public
    property linkID: TNDWLinkID read fLinkID;
    property objectID: Integer read fObjectID;
    property flow: Double read fFlow;
    property speed: Double read fSpeed;
    property length: Double read fLength write fLength;
    property HWN: Integer read fHWN write fHWN;
    property geometry: TWDGeometry read fGeometry write setGeometry;
    property dirty: Integer read fDirty write fDirty;

    procedure UpdateSpeed(aSpeed: Double; aTime: TNDWUnixTime);
    procedure UpdateFlow(aFlow: Double; aTime: TNDWUnixTime);
  public
    function encode: imb4.TByteBuffer;
    procedure dump;
  end;

  TRoute = class
  constructor Create(aRouteID: Integer);
  private
    fRouteID: Integer;
    fTimestamp: Integer;
    fTravelTime: Double;
    fTrajectSpeed: Double;
    fDataBeschikbaarheid: Integer;
  public
    property routeID: Integer read fRouteID write fRouteID;
    property timestamp: Integer read fTimestamp write fTimestamp;
    property travelTime: Double read fTravelTime write fTravelTime;
    property trajectSpeed: Double read fTrajectSpeed write fTrajectSpeed;
    property dataBeschikbaarheid: Integer read fDataBeschikbaarheid write fDataBeschikbaarheid;
  end;

  TNDWConnection = class
  constructor Create(const aRemoteHost: string; aRemotePort: Integer; const aFederation, aTablePrefix, aConnectString: string);
  destructor Destroy; override;
  private
    fConnection: TIMBConnection;
    fLiveEvent: TIMBEventEntry;
    fGENE_ROAD_event: TIMBEventEntry;
    fLinks: TObjectDictionary<TNDWLinkID, TNDWLink>; // owns
    // oracle
    fSession: TOraSession;
    fTablePrefix: string;
    // handling changes
    fNewLinkQueue: TObjectList<TNDWLink>; // refs
    fChangedLinkQueue: TObjectList<TNDWLink>; // refs
    fChangedLinkQueueEvent: TEvent;
    fChangedLinkQueueThread: TThread;
    // routes
    fRoutes: TObjectDictionary<Integer, TRoute>;

    procedure HandleNDWNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
    procedure addLinkToChanedQueue(aLink: TNDWLink; aDirtyFlag: Integer);
    procedure processQueues();
  public
    property links: TObjectDictionary<TNDWLinkID, TNDWLink> read fLinks;
    procedure SaveLinkInfoToFile(const aFileName: string);
    procedure LoadLinkInfoFromFile(const aFileName: string);
    procedure dump();
    procedure LoadFromUS();
  end;

implementation

{ TNWDLink }

constructor TNDWLink.Create(aLinkID: TNDWLinkID; aObjectID: Integer);
begin
  inherited Create;
  fFlow := Double.NaN;
  fFlowLastUpdate := -1;
  fSpeed := Double.NaN;
  fSpeedLastUpdate := -1;
  fLength := Double.NaN;
  fHWN := -1;
  fGeometry := nil;
  fObjectID := aObjectID;
  fLinkID := aLinkID;
  fDirty := 0;
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

const
  // todo:
  tag_link_id = icehAttributeBase+1;
  tag_flow = icehAttributeBase+2;
  tag_speed = icehAttributeBase+3;
  tag_length = icehAttributeBase+4;
  tag_hwn = icehAttributeBase+5;
  tag_geometry = icehAttributeBase+6;

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

constructor TRoute.Create(aRouteID: Integer);
begin
  inherited Create;
  fRouteID := aRouteID;
  fTimestamp := 0;
  fTravelTime := Double.NaN;
  fTrajectSpeed := Double.NaN;
  fDataBeschikbaarheid := -1;
end;

{ TNDWConnection }

procedure TNDWConnection.addLinkToChanedQueue(aLink: TNDWLink; aDirtyFlag: Integer);
begin
  if aLink.objectID>=0 then
  begin
    if aLink.dirty=0 then
    begin
      // cannot lock fChangedLinkQueue because could be swapped with local queue in processing thread
      TMonitor.Enter(fLinks);
      try
        fChangedLinkQueue.Add(aLink);
      finally
        TMonitor.Exit(fLinks);
      end;
    end;
    aLink.dirty := aLink.dirty or aDirtyFlag;
    fChangedLinkQueueEvent.SetEvent;
  end;
end;

constructor TNDWConnection.Create(const aRemoteHost: string; aRemotePort: Integer; const aFederation, aTablePrefix, aConnectString: string);
begin
  inherited Create;
  fLinks := TObjectDictionary<TNDWLinkID, TNDWLink>.Create([doOwnsValues]);
  fNewLinkQueue := TObjectList<TNDWLink>.Create(False);
  fChangedLinkQueue := TObjectList<TNDWLink>.Create(False);
  fTablePRefix := aTablePrefix;
  fRoutes := TObjectDictionary<Integer, TRoute>.Create([doOwnsValues]);
  fConnection := TIMBConnection.Create(aRemoteHost, aRemotePort, 'NDWlistener', 0, aFederation);
  if aConnectString<>'' then
  begin
    // oracle session
    fSession := TOraSession.Create(nil);
    fSession.connectString := aConnectString;
    fSession.Open;
    // event for signaling queue
    fChangedLinkQueueEvent := TEvent.Create(nil, False, False, '');
    // thread to process queued items
    fChangedLinkQueueThread := TThread.CreateAnonymousThread(processQueues);
    fChangedLinkQueueThread.FreeOnTerminate := False;
    fChangedLinkQueueThread.NameThreadForDebugging('NDW link processor');
    fChangedLinkQueueThread.Start;
    // imb events to signal on for queued items
    fGENE_ROAD_event := fConnection.Publish('GENE_ROAD');
    LoadFromUS();
    Log.WriteLn('loaded '+fLinks.Count.toString+' links from '+aConnectString);
  end
  else
  begin
    fSession := nil;
    fChangedLinkQueueEvent := nil;
    fChangedLinkQueueThread := nil;
    fGENE_ROAD_event := nil;
  end;
  fLiveEvent := fConnection.Subscribe('NDW.Live', False);
  fLiveEvent.OnNormalEvent := HandleNDWNormalEvent;
end;

destructor TNDWConnection.Destroy;
begin
  fChangedLinkQueueThread.Terminate;
  fChangedLinkQueueEvent.SetEvent;
  FreeAndNil(fChangedLinkQueueThread);
  fLiveEvent := nil;
  fGENE_ROAD_event := nil;
  FreeAndNil(fConnection);
  FreeAndNil(fNewLinkQueue);
  FreeAndNil(fChangedLinkQueue);
  FreeAndNil(fLinks);
  FreeAndNil(fRoutes);
  inherited;
  FreeAndNil(fChangedLinkQueueEvent);
  FreeAndNil(fSession);
end;

procedure TNDWConnection.HandleNDWNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
var
  key: Integer;
  time: TNDWUnixTime;
  linkID: TNDWLinkID;
  link: TNDWLink;
  speed: TNDWSpeed;
  flow: TNDWFlow;
  nwd_length: Double;
  hwn: Integer;
  geometry_length: Integer;
  geometry: TWDGeometry;
  i: Integer;
  x, y: TNDWCoordinate;
  routeTravelTime: Double;
  routeTrajectSpeed: Double;
  routeDataBeschikbaarheid: Integer;
  routeID: Integer;
  routeTrajectTimestamp: Integer;
  route: TRoute;
begin
  link := nil;
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
              link := TNDWLink.Create(linkID, -1); // mark link as NOT active with object id = -1
              fLinks.Add(linkID, link);
              fNewLinkQueue.Add(link);
              Log.Progress('added link '+linkID.ToString);
            end;
          finally
            TMonitor.Exit(fLinks);
          end;
        end;
      tagNDWSpeed:
        begin
          aPayload.Read(speed);
          if Assigned(link) then
          begin
            TMonitor.Enter(link);
            try
              if (link.speed<>speed) and (Speed>=0) then
              begin
                //Log.WriteLn('link speed changed for link '+linkID.ToString+': '+speed.ToString);
                link.UpdateSpeed(speed, time);
                addLinkToChanedQueue(link, dirtySpeed);
              end;
            finally
              TMonitor.Exit(link);
            end;
          end;
        end;
      tagNDWFlow:
        begin
          aPayload.Read(flow);
          if Assigned(link) then
          begin
            TMonitor.Enter(link);
            try
              if (link.flow<>flow) and (flow>=0) then
              begin
                //Log.WriteLn('link flow changed for link '+linkID.ToString+': '+flow.ToString);
                link.UpdateFlow(flow, time);
                addLinkToChanedQueue(link, dirtyFlow);
              end;
            finally
              TMonitor.Exit(link);
            end;
          end;
        end;
      tagNDWGEOLength:
        begin
          aPayload.Read(nwd_length);
          if Assigned(link) then
          begin
            TMonitor.Enter(link);
            try
              link.length := nwd_length;
              addLinkToChanedQueue(link, dirtyLength);
            finally
              TMonitor.Exit(link);
            end;
          end;
        end;
      tagNDWGEOHWN:
        begin
          aPayload.Read(hwn);
          if Assigned(link) then
          begin
            TMonitor.Enter(link);
            try
              link.HWN := hwn;
              //Log.WriteLn('link hwn for link '+linkID.ToString+': '+Ord(hwn).ToString);
              addLinkToChanedQueue(link, dirtyHWN);
            finally
              TMonitor.Exit(link);
            end;
          end;
        end;
      tagNDWGEOCoordinates:
        begin
          aPayload.Read(geometry_length);
          geometry := TWDGeometry.Create;
          for i := 0 to (geometry_length div 16) -1 do
          begin
            aPayload.Read(x);
            aPayload.Read(y);
            geometry.AddPoint(x, y, Double.NaN);
          end;
//          if geometry_length>0
//          then Log.WriteLn('link geo for link '+linkID.ToString+': '+geometry.parts[0].points[0].x.toString+' '+geometry.parts[0].points[0].y.toString)
//          else Log.WriteLn('## link geo for link '+linkID.ToString+': EMPTY');
          if Assigned(link) then
          begin
            TMonitor.Enter(link);
            try
              link.geometry := geometry;
              addLinkToChanedQueue(link, dirtyGeometry);
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
              route := TRoute.Create(routeID);
              fRoutes.Add(routeID, route);
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

              end;
              // todo: what to do with route..

              Log.WriteLn('route: '+route.routeID.ToString+': '+route.fTravelTime.ToString+'  '+route.fTrajectSpeed.ToString);
            finally
              TMonitor.Exit(route);
            end;
          end;
        end;
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
      'SELECT V7#GENE_ROAD.OBJECT_ID, FNODE_, LENGTH, SPEED_R, INTENSITY, V7#GENE_ROAD.SHAPE '+
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
            link := TNDWLink.Create(linkID, objectID);
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
                link := TNDWLink.Create(linkID, -1); // mark link as NOT active with object id = -1
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

procedure TNDWConnection.processQueues;
var
  ChangeSpeedQuery: TOraSQL;
  ChangeIntensityQuery: TOraSQL;
  link: TNDWLink;
  speedObjectIDParam: TOraParam;
  speedSpeedParam: TOraParam;
  intensityObjectIDParam: TOraParam;
  intensityIntensityParam: TOraParam;
  speedIndex: Integer;
  intensityIndex: Integer;
  i: Integer;
  localQueue, tempQueue: TObjectList<TNDWLink>;
begin
  // we can use global orasession because only 1 thread uses it at the same time right now..
  ChangeSpeedQuery := TOraSQL.Create(nil);
  ChangeIntensityQuery := TOraSQL.Create(nil);
  localQueue := TObjectList<TNDWLink>.Create(False); // refs
  try
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

    while not TThread.CheckTerminated do
    begin
      if fChangedLinkQueueEvent.WaitFor()=TWaitResult.wrSignaled then
      begin
        try
          // swap queues
          TMonitor.Enter(fLinks);
          try
            tempQueue := fChangedLinkQueue;
            fChangedLinkQueue := localQueue;
            localQueue := tempQueue;
          finally
            TMonitor.Exit(fLinks);
          end;
          // process local (ie swapped queue)
          speedIndex := 0;
          intensityIndex := 0;
          for link in localQueue do
          begin
            TMonitor.Enter(link);
            try
              // check if updatable (ie object_id>=0)
              if link.objectID>=0 then
              begin
                if link.dirty<>0 then
                begin
                  try
                    if (link.dirty and dirtySpeed) <>0 then
                    begin
                      Log.Progress('New speed '+link.speed.tostring+' for '+link.objectID.tostring);
                      speedObjectIDParam.ItemAsInteger[speedIndex+1] := link.objectID;
                      speedSpeedParam.ItemAsFloat[speedIndex+1] := link.speed;
                      speedIndex := speedIndex+1;
                      if speedIndex = speedObjectIDParam.Length then
                      begin
                        ChangeSpeedQuery.Execute(speedIndex);
                        fSession.Commit;
                        for i := 0 to speedIndex-1
                        do fGENE_ROAD_event.SignalChangeObject(actionChange, speedObjectIDParam.ItemAsInteger[i], 'SPEED_R');
                        speedIndex := 0;
                      end;
                    end;
                    if (link.dirty and dirtyFlow) <>0 then
                    begin
                      if link.flow>=0 then
                      begin
                        Log.Progress('New intensity '+link.flow.ToString+' for '+link.objectID.tostring);
                        intensityObjectIDParam.ItemAsInteger[intensityIndex+1] := link.objectID;
                        intensityIntensityParam.ItemAsFloat[intensityIndex+1] := link.flow; // todo: convert to intensity and avoid negative intensity
                        intensityIndex := intensityIndex+1;
                        if intensityIndex = intensityObjectIDParam.Length then
                        begin
                          ChangeIntensityQuery.Execute(intensityIndex);
                          fSession.Commit;
                          for i := 0 to intensityIndex-1
                          do fGENE_ROAD_event.SignalChangeObject(actionChange, intensityObjectIDParam.ItemAsInteger[i], 'INTENSITY');
                          intensityIndex := 0;
                        end;
                      end;
                    end;
                  finally
                    link.dirty := 0;
                  end;
                end;
              end;
            finally
              TMonitor.Exit(link);
            end;
          end;
          // process rest items in batched queries
          if speedIndex>0 then
          begin
            ChangeSpeedQuery.Execute(speedIndex);
            fSession.Commit;
            for i := 0 to speedIndex-1
            do fGENE_ROAD_event.SignalChangeObject(actionChange, speedObjectIDParam.ItemAsInteger[i+1], 'SPEED_R');
            speedIndex := 0;
          end;
          if intensityIndex>0 then
          begin
            ChangeIntensityQuery.Execute(intensityIndex);
            fSession.Commit;
            for i := 0 to intensityIndex-1
            do fGENE_ROAD_event.SignalChangeObject(actionChange, intensityObjectIDParam.ItemAsInteger[i+1], 'INTENSITY');
            intensityIndex := 0;
          end;
          // clear all processed entries
        except
         on E: Exception
         do Log.WriteLn('Exception in processing loop: '+E.Message, llError);
        end;
        localQueue.Clear;
      end;
    end;
  finally
    ChangeSpeedQuery.Free;
    ChangeIntensityQuery.Free;
    localQueue.Free;
  end;
end;

procedure TNDWConnection.dump();
var
  ilp: TPair<TNDWLinkID, TNDWLink>;
begin
  TMonitor.Enter(fLinks);
  try
    for ilp in fLinks
    do ilp.Value.dump();
  finally
    TMonitor.Exit(fLinks);
  end;
end;

procedure TNDWConnection.SaveLinkInfoToFile(const aFileName: string);
var
  F: File;
  ilp: TPair<TNDWLinkID, TNDWLink>;
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

end.
