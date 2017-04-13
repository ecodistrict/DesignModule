unit NDWLib;

interface

uses
  imb4,
  WorldDataCode,
  ByteBuffers,
  IMB3NativeClient,
  System.Generics.Collections,
  System.SysUtils;

const
  tagNDWTime = 1;
  tagNDWLinkID = 2;
  tagNDWSpeed = 3;
  tagNDWFlow = 4;

  tagNDWGEOLinkID = 6;
  tagNDWGEOLength = 7;
  tagNDWGEOHWN = 8;
  tagNDWGEOCoordinates = 9;

type
  TNDWUnixTime = Int32; // UInt32 in payload
  TNDWLinkID = Int64; // UInt64 in payload
  TNDWFlow = Int32;
  TNDWSpeed = Int32;
  TNDWCoordinate = Double;

  TNDWLink = class
  constructor Create(aLinkID: TNDWLinkID); // aLength: Double; aHWN: UInt32; aGeometry: TWDGeometry);
  destructor Destroy; override;
  private
    fLinkID: TNDWLinkID;
    fFlow: Double;
    fFlowLastUpdate: TNDWUnixTime;
    fSpeed: Double;
    fSpeedLastUpdate: TNDWUnixTime;
    fLength: Double;
    fHWN: Integer; // 0,1
    fGeometry: TWDGEometry; // owned
    fDirty: Boolean;
    procedure setGeometry(const aValue: TWDGeometry);
  public
    property linkID: TNDWLinkID read fLinkID;
    property flow: Double read fFlow;
    property speed: Double read fSpeed;
    property length: Double read fLength write fLength;
    property HWN: Integer read fHWN write fHWN;
    property geometry: TWDGeometry read fGeometry write setGeometry;
    property dirty: Boolean read fDirty write fDirty;

    procedure UpdateSpeed(aSpeed: Double; aTime: TNDWUnixTime);
    procedure UpdateFlow(aFlow: Double; aTime: TNDWUnixTime);
  public
    function encode: imb4.TByteBuffer;
    procedure dump;
  end;

  TNDWConnection = class
  constructor Create(const aRemoteHost: string; aRemotePort: Integer);
  destructor Destroy; override;
  private
    fConnection: TIMBConnection;
    fLiveEvent: TIMBEventEntry;
    fLinks: TObjectDictionary<TNDWLinkID, TNDWLink>; // owns
    fNewLinkQueue: TObjectList<TNDWLink>; // refs
    fChangedLinkQueue: TObjectList<TNDWLink>; // refs

    procedure HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
  public
    property links: TObjectDictionary<TNDWLinkID, TNDWLink> read fLinks;
    procedure SaveLinkInfoToFile(const aFileName: string);
    procedure LoadLinkInfoFromFile(const aFileName: string);
    procedure dump();
  end;

implementation

{ TNWDLink }

constructor TNDWLink.Create(aLinkID: TNDWLinkID);
begin
  inherited Create;
  fFlow := Double.NaN;
  fFlowLastUpdate := -1;
  fSpeed := Double.NaN;
  fSpeedLastUpdate := -1;
  fLength := Double.NaN;
  fHWN := -1;
  fGeometry := nil;
  fLinkID := aLinkID;
  fDirty := False;
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

{ TNDWConnection }

constructor TNDWConnection.Create(const aRemoteHost: string; aRemotePort: Integer);
begin
  inherited Create;
  fLinks := TObjectDictionary<TNDWLinkID, TNDWLink>.Create([doOwnsValues]);
  fNewLinkQueue := TObjectList<TNDWLink>.Create(False);
  fChangedLinkQueue := TObjectList<TNDWLink>.Create(False);
  fConnection := TIMBConnection.Create(aRemoteHost, aRemotePort, 'NDWlistener', 0, 'NDW');
  fLiveEvent := fConnection.Subscribe('Live');
  fLiveEvent.OnNormalEvent := HandleNormalEvent;
end;

destructor TNDWConnection.Destroy;
begin
  fLiveEvent := nil;
  FreeAndNil(fConnection);
  FreeAndNil(fNewLinkQueue);
  FreeAndNil(fChangedLinkQueue);
  FreeAndNil(fLinks);
  inherited;
end;

procedure TNDWConnection.HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
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
              link := TNDWLink.Create(linkID);
              fLinks.Add(linkID, link);
              fNewLinkQueue.Add(link);
              WriteLn('added link '+linkID.ToString);
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
              link.UpdateSpeed(speed, time);
              WriteLn('link speed for link '+linkID.ToString+': '+speed.ToString);
              if not link.dirty then
              begin
                fChangedLinkQueue.Add(link);
                link.dirty := True;
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
              link.UpdateFlow(flow, time);
              WriteLn('link flow for link '+linkID.ToString+': '+flow.ToString);
              if not link.dirty then
              begin
                fChangedLinkQueue.Add(link);
                link.dirty := True;
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
              if not link.dirty then
              begin
                fChangedLinkQueue.Add(link);
                link.dirty := True;
              end;
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
              WriteLn('link hwn for link '+linkID.ToString+': '+Ord(hwn).ToString);
              if not link.dirty then
              begin
                fChangedLinkQueue.Add(link);
                link.dirty := True;
              end;
            finally
              TMonitor.Exit(link);
            end;
          end;
        end;
      tagNDWGEOCoordinates:
        begin
          aPayload.Read(geometry_length);
          geometry := TWDGeometry.Create;
          for i := 0 to geometry_length-1 do
          begin
            aPayload.Read(x);
            aPayload.Read(y);
            geometry.AddPoint(x, y, Double.NaN);
          end;
          if geometry_length>0
          then WriteLn('link geo for link '+linkID.ToString+': '+geometry.parts[0].points[0].x.toString+' '+geometry.parts[0].points[0].y.toString)
          else WriteLn('## link geo for link '+linkID.ToString+': EMPTY');
          if Assigned(link) then
          begin
            TMonitor.Enter(link);
            try
              link.geometry := geometry;
              if not link.dirty then
              begin
                fChangedLinkQueue.Add(link);
                link.dirty := True;
              end;
            finally
              TMonitor.Exit(link);
            end;
          end;
        end;
    end;
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
          (tag_link_id shl 3) or imb4.wtVarInt://bb_tag_uint64(, fLinkID)+
            begin
              linkID := buffer.bb_read_uint64(cursor);
              if not fLinks.TryGetValue(linkID, link) then
              begin
                link := TNDWLink.Create(linkID);
                fLinks.Add(linkID, link);
              end;
            end;
          (tag_flow shl 3) or imb4.wt64Bit: //imb4.TByteBuffer.bb_tag_double(, fFlow)+
            begin
              flow := buffer.bb_read_double(cursor);
              if Assigned(link)
              then link.UpdateFlow(flow, 0);
            end;
          (tag_speed shl 3) or imb4.wt64Bit: //imb4.TByteBuffer.bb_tag_double(, fSpeed)+
            begin
              speed := buffer.bb_read_double(cursor);
              if Assigned(link)
              then link.UpdateSpeed(speed, 0);
            end;
          (tag_length  shl 3) or imb4.wt64Bit: //imb4.TByteBuffer.bb_tag_double(, fLength)+
            begin
              ndw_len := buffer.bb_read_double(cursor);
              if Assigned(link)
              then link.length := ndw_len;
            end;
          (tag_hwn  shl 3) or imb4.wtVarInt: //imb4.TByteBuffer.bb_tag_bool(, fHWN=1)+
            begin
              hwn := buffer.bb_read_bool(cursor);
              if Assigned(link)
              then link.HWN := ord(hwn);
            end;
          (tag_geometry  shl 3) or imb4.wtLengthDelimited: //imb4.TByteBuffer.bb_tag_rawbytestring(, fGeometry.encode);
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
        //buffer := BB(length(buffer))+buffer;
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
