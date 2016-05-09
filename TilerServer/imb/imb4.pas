unit imb4;

// todo: if TLS connections is not explicitly freeed nil pointer exception is thrown in a handler.. (reported by Han Zhou)

interface

{$DEFINE imbSocketSupport}
{$DEFINE imbTLSSupport}
{$DEFINE imbSharedMemSupport}

uses
  {$IFDEF imbSocketSupport}
  imb.SocksLib, // winsock 2 and ipv6 support
  {$ENDIF}

  {$IFDEF imbTLSSupport}
  IdSSLOpenSSLHeaders, // for flush ?
  IdSSLOpenSSL, IdTCPClient, IdComponent, IdGlobal, // indy for secure connection
  {$ENDIF}

  {$IFDEF imbSharedMemSupport}
  imb.SharedMemLib, // shared memory connection
  {$ENDIF}
  WinApi.Windows,
  System.Classes, System.SysUtils, System.Generics.Collections;

const
  imbDefaultRemoteHost = 'vps17642.public.cloudvps.com'; //'localhost'; // 'vps17642.public.cloudvps.com'; //
  imbDefaultPrefix = 'nl.imb'; // 'ecodistrict'; //

  imbDefaultRemoteSocketPort = 4004;
  imbDefaultRemoteTLSPort = 4443;

  imbMagic = $FE;

  imbMinimumPacketSize = 16;
  imbMaximumPayloadSize = 10*1024*1024;
  imbMaxStreamBodyBuffer = 8*1024;

  imbSocketDefaultLingerTimeInSec = 2; // in sec

  // client state
  icsClient = 2;

  // command tags
  icehRemark = 1;                      // <string>
  icehSubscribe = 2;                   // <uint32: varint>
  icehPublish = 3;                     // <uint32: varint>
  icehUnsubscribe = 4;                 // <uint32: varint>
  icehUnpublish = 5;                   // <uint32: varint>
  icehSetEventIDTranslation = 6;       // <uint32: varint>
  icehEventName = 7;                   // <string>
  icehEventID = 8;                     // <uint32: varint>

  icehUniqueClientID = 11;             // <guid>
  icehHubID = 12;                      // <guid>
  icehModelName = 13;                  // <string>
  icehModelID = 14;                    // <int32: varint> ?
  icehState = 16;                      // <uint32: varint>
  icehEventNameFilter = 17;            // <string>
  icehClose = 21;                      // <bool: varint>

  // basic event tags
  icehIntString = 1;                   // <varint>
    icehIntStringPayload = 2;          // <string>
  icehString = 3;                      // <string>
  icehChangeObject = 4;                // <int32: varint>
    icehChangeObjectAction = 5;        // <int32: varint>
    icehChangeObjectAttribute = 6;     // <string>

  icehStreamHeader = 7;                // <string> filename
  icehStreamBody = 8;                  // <bytes>
  icehStreamEnd = 9;                   // <bool> true: ok, false: cancel
    icehStreamID = 10;                 // <id: bytes/string>

  icehNoObjectID = 11;                 // <id: bytes/string>
  icehObjectID = 12;                   // <id: bytes/string>
    icehNoAttribute = 13;              // <uint32: varint> key of attribute
    icehAttributeBase = 14;            // attributes of object are icehAttributeBase..
    icehAttributeTop = 899;            // todo: may be lower?

  icehWorldCommandBase = 900; // .. 999

  // monitoring info tags
  icehMonitoringBase = 1000; // .. 1099, see imb.consts
  // timers
  icehTimerBase = 1100; // .. 1199, see imb.consts
  // client log
  icehLogBase = 1200; // .. 1209
    icehLogLine = icehLogBase+0;       // <string>
    icehLevel = icehLogBase+1;         // <uint32: varint>
    icehLogSource = icehLogBase+2;     // <string>
  icehHubManagementBase = 1300; // ..1399

  icehTilerBase = 1400; // ..1499

  // change object actions
  actionNew = 0;
  actionDelete = 1;
  actionChange = 2;
  actionInquire = 3;

  // protobuf wire types
  wtVarInt = 0;                        // int32, int64, uint32, uint64, sint32, sint64, bool, enum
  wt64Bit = 1;                         // double or fixed int64/uint64
  wtLengthDelimited = 2;               // string, bytes, embedded messages, packed repeated fields
  wtStartGroup = 3;                    // deprecated
  wtEndGroup = 4;                      // deprecated
  wt32Bit = 5;                         // float (single) or fixed int32/uint32


type
  TEventID = UInt32;     // event id type for use everywhere except in event packet
  TEventIDFixed = Word;  // event id type for use in event packet (fixed length so hub can rewrite efficiently)

const
  imbInvalidEventID = High(TEventID);

type
  TByteBuffer = RawByteString;

  TByteBufferHelper = record helper for TByteBuffer
    // cursor is 0-based
    function firstByte: Byte;
    function lastByte: Byte;
    function ref(aCursor: Integer): Pointer;
    function refFirstByte: Pointer;
    function refLastByte: Pointer;
    procedure shiftLeft;
    // visual
    function toHex: string;
    // reading
    function bb_read_uint64(var aCursor: Integer): UInt64; // unsigned varint
    function bb_read_int64(var aCursor: Integer): Int64; // signed varint
    function bb_read_uint32(var aCursor: Integer): UInt32; // unsigned varint
    function bb_read_int32(var aCursor: Integer): Int32; // signed varint
    function bb_read_uint16(var aCursor: Integer): Word; // fixed 16 bit (cannot be tagged)
    function bb_read_bool(var aCursor: Integer): Boolean; // 1 byte varint
    function bb_read_double(var aCursor: Integer): Double; // 64 bit float
    function bb_read_single(var aCursor: Integer): Single; //  32 bit float
    function bb_read_guid(var aCursor: Integer): TGUID; // length delimited
    function bb_read_string(var aCursor: Integer): string; // length delimited
    function bb_read_rawbytestring(var aCursor: Integer): TByteBuffer; // length delimited
    function bb_read_tbytes(var aCursor: Integer): TBytes; // length delimited
    // raw field data, not prefixed with length!
    function bb_read_bytes(var aCursor: Integer; aValueSize: Integer): TByteBuffer;
    // skip reading
    procedure bb_read_skip(var aCursor: Integer; aWiretype: Integer);
    // field writing
    class function bb_uint16(aValue: Word): TByteBuffer; static; // fixed 16 bit (cannot be tagged)
    class function bb_uint64(aValue: UInt64): TByteBuffer; static; // unsigned varint
    class function bb_uint32(aValue: UInt32): TByteBuffer; static; // unsigned varint
    class function bb_int64(aValue: Int64): TByteBuffer; static; // signed varint
    class function bb_int32(aValue: Int64): TByteBuffer; static; // signed varint
    class function bb_ansi_string(const aValue: AnsiString): TByteBuffer; static; // length delimited
    class function bb_utf8_string(const aValue: UTF8String): TByteBuffer; static; // length delimited
    class function bb_string(const aValue: string): TByteBuffer; static; // length delimited
    class function bb_rawbytestring(const aValue: RawByteString): TByteBuffer; static; // length delimited
    class function bb_tbytes(const aValue: TBytes): TByteBuffer; static; // length delimited
    // raw field data, not prefixed with length!
    class function bb_bytes(const aValue; aValueSize: Integer): TByteBuffer; static;
    // taged field writing
    class function bb_tag_int32(aTag: UInt32; aValue: Int32): TByteBuffer; static;
    class function bb_tag_uint32(aTag: UInt32; aValue: UInt32): TByteBuffer; static;
    class function bb_tag_int64(aTag: UInt32; aValue: Int64): TByteBuffer; static;
    class function bb_tag_uint64(aTag: UInt32; aValue: UInt64): TByteBuffer; static;
    class function bb_tag_bool(aTag: UInt32; aValue: Boolean): TByteBuffer; static;
    class function bb_tag_single(aTag: UInt32; aValue: Single): TByteBuffer; static;
    class function bb_tag_double(aTag: UInt32; aValue: Double): TByteBuffer; static;
    class function bb_tag_guid(aTag: UInt32; const aValue: TGUID): TByteBuffer; static;
    class function bb_tag_string(aTag: UInt32; const aValue: string): TByteBuffer; static;
    class function bb_tag_rawbytestring(aTag: UInt32; const aValue: TByteBuffer): TByteBuffer; static;
    class function bb_tag_tbytes(aTag: UInt32; const aValue: TBytes): TByteBuffer; static;
    class function bb_tag_bytes(aTag: UInt32; const aValue; aValueSize: Integer): TByteBuffer; static;
  end;

  TConnection = class; // forward
  TEventEntry = class; // forward

  TBatchBuffer = record
    procedure Prepare(aBatchSize: Integer; aEventEntries: TArray<TEventEntry>; const aHeader: TByteBuffer='');
    procedure Add(const aEntry: TByteBuffer);
    procedure Commit();
  private
    fBatch: TByteBuffer;
    fBatchSize: Integer;
    fHeader: TByteBuffer;
    fEventEntries: TArray<TEventEntry>; // ref
  public
    property Header: TByteBuffer read fHeader write fHeader;
  end;

  TOnChangeObject = reference to procedure(aEventEntry: TEventEntry; aAction, aObjectID: Integer; const aAttribute: string);
  TOnString = reference to procedure(aEventEntry: TEventEntry; const aString: string);
  TOnIntString = reference to procedure(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
  TOnTag = reference to procedure(aEventEntry: TEventEntry; aFieldInfo: UInt32; const aBuffer: TByteBuffer; aCursor: Integer);
  TOnEvent = reference to procedure(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);

  TOnStreamCreate = reference to function(aEventEntry: TEventEntry; const aName: string): TStream;
  TOnStreamEnd = reference to procedure(aEventEntry: TEventEntry; const aName: string; var aStream: TStream; aCancel: Boolean);

  TOnDisconnect = reference to procedure(aConnection: TConnection);
  TOnException = reference to procedure(aConnection: TConnection; aException: Exception);

  TOnSubAndPub = reference to procedure(aEventEntry: TEventEntry; aCommand: UInt32);

  TStreamCacheEntry = class
  constructor Create(const aStreamName: string; aStream: TStream);
  destructor Destroy; override;
  private
    fStreamName: string;
    fStream: TStream;
  end;

  TEventEntry = class
  constructor Create(aConnection: TConnection; aEventID: TEventID; const aEventName: string);
  destructor Destroy; override;
  private
    fConnection: TConnection; // ref
    fEventName: string;
    fEventID: TEventID;
    fIsSubscribed: Boolean;
    fIsPublished: Boolean;
    fSubscribers: Boolean;
    fPublishers: Boolean;
    fStreamCache: TObjectDictionary<TGUID, TStreamCacheEntry>;
    // handlers
    fOnChangeObject: TList<TOnChangeObject>;
    fOnString: TList<TOnString>;
    fOnIntString: TList<TOnIntString>;
    fOnTag: TList<TOnTag>;
    fOnEvent: TList<TOnEvent>;
    fOnStreamCreate: TOnStreamCreate;
    fOnStreamEnd: TOnStreamEnd;
    fOnSubAndPub: TList<TOnSubAndPub>;
    procedure handleEvent(const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleSubAndPub(aCommand: UInt32);
    procedure signalSubscribe();
    procedure signalPublish();
    procedure signalUnSubscribe();
    procedure signalUnPublish();
  public
    property connection: TConnection read fConnection;
    property eventName: string read fEventName;
    property eventID: TEventID read fEventID;
    property isSubscribed: Boolean read fIsSubscribed;
    property isPublished: Boolean read fIsPublished;
    property subscribers: Boolean read fSubscribers;
    property publishers: Boolean read fPublishers;
  public
    function publish(): TEventEntry;
    function subscribe(): TEventEntry;
    function unPublish(): TEventEntry;
    function unSubscribe(): TEventEntry;
  public
    // add/remove handler
    property OnChangeObject: TList<TOnChangeObject> read fOnChangeObject;
    property OnString: TList<TOnString> read fOnString;
    property OnIntString: TList<TOnIntString> read fOnIntString;
    property OnTag: TList<TOnTag> read fOnTag;
    property OnEvent: TList<TOnEvent> read fOnEvent;
    // assign handler
    property OnStreamCreate: TOnStreamCreate read fOnStreamCreate write fOnStreamCreate;
    property OnStreamEnd: TOnStreamEnd read fOnStreamEnd write fOnStreamEnd;
    // add/remove handler
    property OnSubAndPub: TList<TOnSubAndPub> read fOnSubAndPub;
  public
    // signal event
    procedure signalChangeObject(aAction, aObjectID: Integer; const aAttribute: string='');
    procedure signalString(const aString: string);
    procedure signalIntString(aInt: Integer; const aString: string);
    procedure signalStream(const aName: string; aStream: TStream);
    procedure signalEvent(const aPayload: TByteBuffer);
  end;

  TConnection = class
  constructor Create(
    const aModelName: string; aModelID: Integer=0;
    const aPrefix: string=imbDefaultPrefix);
  destructor Destroy; override;
  // overide for specific connection type
  private
    function getConnected: Boolean; virtual; abstract;
    procedure setConnected(aValue: Boolean); virtual; abstract;
    function readBytes(var aBuffer; aNumberOfBytes: Integer): Integer; virtual; abstract;
  public
    function writePacket(aPacket: TByteBuffer; aCallCloseOnError: Boolean=True): Boolean; virtual; abstract;
  // generic connection
  private
    fReaderThread: TThread;
    fHeartbeatInterval: Integer;
    fHeartbeatThread: TThread;
    fHeartbeatQuitEvent: THandle;
  public
    property ReaderThread: TThread read fReaderThread;
  private
    fModelName: string;
    fModelID: Integer;
    fPrefix: string;
    fLocalEventEntries: TObjectList<TEventEntry>; // owns
    fRemoteEventEntries: TDictionary<TEventID, TEventEntry>; // refs, only used in reader thread
    fOnDisconnect: TOnDisconnect;
    fOnException: TOnException;
    fUniqueClientID: TGUID;
    fHubID: TGUID;
    function getMonitorEventName: string;
    function getPrivateEventName: string;
    procedure waitForConnected(aSpinCount: Integer=20);
  private
    procedure handleCommand(const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    procedure close(aSendCloseCmd: Boolean);
    procedure signalConnectInfo(const aModelName: string; aModelID: Integer);
  protected
    procedure readPackets; // event reader thread loop
  public
    property ModelName: string read fModelName;
    property ModelID: Integer read fModelID;
    property HubID: TGUID read fHubID;
    property UniqueClientID: TGUID read fUniqueClientID;
    property privateEventName: string read getPrivateEventName;
    property monitorEventName: string read getMonitorEventName;

    property eventEntries: TObjectList<TEventEntry> read fLocalEventEntries; // use TMonitor.Enter/Exit(connection)

    property connected: Boolean read getConnected write setConnected;

    property onDisconnect: TOnDisconnect read fOnDisconnect write fOnDisconnect;
    property onException: TOnException read fOnException write fOnException;

    function subscribe(const aEventName: string; aUsePrefix: Boolean=True): TEventEntry;
    function publish(const aEventName: string; aUsePrefix: Boolean=True): TEventEntry;
    procedure unSubscribe(aEventEntry: TEventEntry);
    procedure unPublish(aEventEntry: TEventEntry);
  protected
    procedure sendHeartbeats();
  public
    procedure signalHeartBeat(const aRemark: string='');
    procedure setHeartbeat(aIntervalms: Integer);
  end;

  {$IFDEF imbSocketSupport}
  TSocketConnection = class(TConnection)
  constructor Create(
    const aModelName: string; aModelID: Integer=0;
    const aPrefix: string=imbDefaultPrefix;
    const aRemoteHost: string=imbDefaultRemoteHost; aRemotePort: Integer=imbDefaultRemoteSocketPort);
  private
    fRemoteHost: string;
    fRemotePort: Integer;
    fSocket: TSocket;
    function getConnected: Boolean; override;
    procedure setConnected(aValue: Boolean); override;
    function readBytes(var aBuffer; aNumberOfBytes: Integer): Integer; override;
  public
    function writePacket(aPacket: TByteBuffer; aCloseOnError: Boolean=True): Boolean; override;
  end;
  {$ENDIF}

  {$IFDEF imbTLSSupport}
  TTLSConnection = class(TConnection)
  constructor Create(
    const aCertFile, aKeyFile, aKeyFilePassword, aRootCertFile: string;
    const aModelName: string; aModelID: Integer=0;
    const aPrefix: string=imbDefaultPrefix;
    const aRemoteHost: string=imbDefaultRemoteHost; aRemotePort: Integer=imbDefaultRemoteTLSPort);
  destructor Destroy; override;
  private
    fIdTCPClient1: TIdTCPClient;
    fIdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    fKeyFilePassword: string;
    function getConnected: Boolean; override;
    procedure setConnected(aValue: Boolean); override;
    function readBytes(var aBuffer; aNumberOfBytes: Integer): Integer; override;
  public
    function writePacket(aPacket: TByteBuffer; aCloseOnError: Boolean=True): Boolean; override;
    function DataAvailable: Boolean;
  private
    // handlers io handler
    procedure HandleGetPassword(var aPassword: String);
    procedure HandleStatusInfo(const aMsg: String);
    function HandleVerifyPeer(aCertificate: TIdX509; aOk: Boolean; aDepth, aError: Integer): Boolean;
    // handlers tcp client
    procedure HandleDisconnected(Sender: TObject);
    procedure HandleStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
  end;
  {$ENDIF}

  {$IFDEF imbSharedMemSupport}
  TSharedMemConnection = class(TConnection)
  constructor Create(
    const aModelName: string; aModelID: Integer=0;
    const aPrefix: string=imbDefaultPrefix;
    const aServerChannelName: string=smServerChannelName);
  private
    fServerChannelName: string;
    fSMFile: TSMFile;
    function getConnected: Boolean; override;
    procedure setConnected(aValue: Boolean); override;
    function readBytes(var aBuffer; aNumberOfBytes: Integer): Integer; override;
  public
    function writePacket(aPacket: TByteBuffer; aCloseOnError: Boolean=True): Boolean; override;
  end;
  {$ENDIF}

function GUIDToStringCompact(const aGUID: TGUID): string;
function TagAndWireTypeToBuffer(aTagAndWireType: UInt32): RawByteString;

implementation

{ utils }

function GUIDToStringCompact(const aGUID: TGUID): string;
begin
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32, '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [aGuid.D1, aGuid.D2, aGuid.D3, aGuid.D4[0], aGuid.D4[1], aGuid.D4[2],
     aGuid.D4[3], aGuid.D4[4], aGuid.D4[5], aGuid.D4[6], aGuid.D4[7]]);
end;

function TagAndWireTypeToBuffer(aTagAndWireType: UInt32): RawByteString;
begin
  Result := TByteBuffer.bb_uint32(aTagAndWireType);
end;

{ TByteBufferHelper }

function VarIntLength(aValue: UInt64): Integer; inline;
begin
  // encode in blocks of 7 bits (high order bit of byte is signal that more bytes are to follow
  // encode lower numbers directly for speed
  if aValue<128
  then Result := 1
  else if aValue<128*128
  then Result := 2
  else if aValue<128*128*128
  then Result := 3
  else
  begin
    // 4 bytes or more: change to dynamic size detection
    Result := 4;
    aValue := aValue shr (7*4);
    while aValue>0 do
    begin
      Inc(Result);
      aValue := aValue shr 7;
    end;
  end;
end;

class function TByteBufferHelper.bb_ansi_string(const aValue: AnsiString): TByteBuffer;
var
  len: uint64;
begin
  len := length(aValue);
  Result := bb_uint64(len)+bb_bytes(PAnsiChar(aValue)^, len);
end;

class function TByteBufferHelper.bb_bytes(const aValue; aValueSize: Integer): TByteBuffer;
begin
  setLength(Result, aValueSize);
  if aValueSize>0
  then Move(aValue, Result[1], aValueSize);
end;

class function TByteBufferHelper.bb_int32(aValue: Int64): TByteBuffer;
begin
  if aValue<0
  then Result := bb_uint32(((-(aValue+1)) shl 1) or 1)
  else Result := bb_uint32(aValue shl 1);
end;

class function TByteBufferHelper.bb_int64(aValue: Int64): TByteBuffer;
begin
  if aValue<0
  then Result := bb_uint64(((-(aValue+1)) shl 1) or 1)
  else Result := bb_uint64(aValue shl 1);
end;

class function TByteBufferHelper.bb_rawbytestring(const aValue: RawByteString): TByteBuffer;
var
  len: uint64;
begin
  len := length(aValue);
  Result := bb_uint64(len)+bb_bytes(PAnsiChar(aValue)^, len);
end;

function TByteBufferHelper.bb_read_bool(var aCursor: Integer): Boolean;
begin
  Result := self[aCursor+1]<>AnsiChar(0);
  Inc(aCursor);
end;

function TByteBufferHelper.bb_read_bytes(var aCursor: Integer; aValueSize: Integer): TByteBuffer;
begin
  setlength(Result, aValueSize);
  if aValueSize>0 then
  begin
    Move(self[aCursor+1], Result[1], aValueSize);
    Inc(aCursor, aValueSize);
  end;
end;

function TByteBufferHelper.bb_read_double(var aCursor: Integer): Double;
begin
  Move(self[aCursor+1], Result, sizeOf(Result));
  Inc(aCursor, sizeOf(Result));
end;

function TByteBufferHelper.bb_read_guid(var aCursor: Integer): TGUID;
var
  len: UInt64;
begin
  len := bb_read_uint64(aCursor);
  if len=sizeOf(Result) then
  begin
    Move(self[aCursor+1], Result, sizeOf(Result));
    Inc(aCursor, sizeOf(Result));
  end
  else raise Exception.Create('Unexpected length of data ('+len.toString+') in TByteBufferHelper.bb_read_guid');
end;

function TByteBufferHelper.bb_read_int32(var aCursor: Integer): Int32;
var
  ui32: UInt32;
begin
  ui32 := bb_read_uint32(aCursor);
  // remove sign bit
  Result := ui32 shr 1;
  // adjust for negative
  if (ui32 and 1)=1
  then Result := -(Result+1);
end;

function TByteBufferHelper.bb_read_int64(var aCursor: Integer): Int64;
var
  ui64: UInt64;
begin
  ui64 := bb_read_uint64(aCursor);
  // remove sign bit
  Result := ui64 shr 1;
  // adjust for negative
  if (ui64 and 1)=1
  then Result := -(Result+1);
end;

function TByteBufferHelper.bb_read_rawbytestring(var aCursor: Integer): TByteBuffer;
var
  len: UInt64;
begin
  len := bb_read_uint64(aCursor);
  setlength(Result, len);
  if len>0
  then Move(self[aCursor+1], Result[1], len);
  Inc(aCursor, len);
end;

function TByteBufferHelper.bb_read_single(var aCursor: Integer): Single;
begin
  Move(self[aCursor+1], Result, sizeOf(Result));
  Inc(aCursor, sizeOf(Result));
end;

procedure TByteBufferHelper.bb_read_skip(var aCursor: Integer; aWiretype: Integer);
var
  len: UInt64;
begin
  case aWiretype of
    wtVarInt:
      bb_read_UInt64(aCursor);
    wt64Bit:
      Inc(aCursor, 8);
    wtLengthDelimited:
      begin
        len := bb_read_UInt64(aCursor);
        Inc(aCursor, len);
      end;
    wt32Bit:
      Inc(aCursor, 4);
  else
    raise Exception.Create('Unsupported wire type ('+aWireType.ToString+') in TByteBufferHelper.bb_read_skip');
  end;
end;

function TByteBufferHelper.bb_read_string(var aCursor: Integer): string;
var
  len: UInt64;
  utf8: UTF8String;
begin
  len := bb_read_uint64(aCursor);
  setlength(utf8, len);
  if len>0
  then Move(self[aCursor+1], utf8[1], len);
  Inc(aCursor, len);
  Result := string(utf8);
end;

function TByteBufferHelper.bb_read_tbytes(var aCursor: Integer): TBytes;
var
  len: UInt64;
begin
  len := bb_read_uint64(aCursor);
  setlength(Result, len);
  if len>0
  then Move(self[aCursor+1], Result[0], len);
  Inc(aCursor, len);
end;

function TByteBufferHelper.bb_read_uint16(var aCursor: Integer): Word;
begin
  Move(self[aCursor+1], Result, sizeOf(Result));
  Inc(aCursor, sizeOf(Result));
end;

function TByteBufferHelper.bb_read_uint32(var aCursor: Integer): UInt32;
var
  shiftLeft: Integer;
  b: UInt32;
begin
  Result := 0;
  shiftLeft := 0;
  repeat
    b := Ord(self[aCursor+1]);
    Inc(aCursor);
    Result := Result or ((b and $7F) shl shiftLeft);
    Inc(shiftLeft, 7);
  until b<128;
end;

function TByteBufferHelper.bb_read_uint64(var aCursor: Integer): UInt64;
var
  shiftLeft: Integer;
  b: UInt64;
begin
  Result := 0;
  shiftLeft := 0;
  repeat
    b := Ord(self[aCursor+1]);
    Inc(aCursor);
    Result := Result or ((b and $7F) shl shiftLeft);
    Inc(shiftLeft, 7);
  until b<128;
end;

class function TByteBufferHelper.bb_string(const aValue: string): TByteBuffer;
begin
  Result := bb_utf8_string(UTF8String(aValue));
end;

class function TByteBufferHelper.bb_tag_bool(aTag: UInt32; aValue: Boolean): TByteBuffer;
begin
  if aValue
  then Result := bb_uint32((aTag shl 3) or wtVarInt)+AnsiChar(1)
  else Result := bb_uint32((aTag shl 3) or wtVarInt)+AnsiChar(0);
end;

class function TByteBufferHelper.bb_tag_bytes(aTag: UInt32; const aValue; aValueSize: Integer): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(aValueSize)+bb_bytes(aValue, aValueSize);
end;

class function TByteBufferHelper.bb_tag_double(aTag: UInt32; aValue: Double): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wt64Bit)+bb_bytes(aValue, SizeOf(aValue));
end;

class function TByteBufferHelper.bb_tag_guid(aTag: UInt32; const aValue: TGUID): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(16)+bb_bytes(aValue, 16);
end;

class function TByteBufferHelper.bb_tag_int32(aTag: UInt32; aValue: Int32): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wtVarInt)+bb_int64(aValue);
end;

class function TByteBufferHelper.bb_tag_int64(aTag: UInt32; aValue: Int64): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wtVarInt)+bb_int64(aValue);
end;

class function TByteBufferHelper.bb_tag_rawbytestring(aTag: UInt32; const aValue: TByteBuffer): TByteBuffer;
var
  len: UInt64;
begin
  len := length(aValue);
  if len>0
  then Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(len)+bb_bytes(aValue[1], len)
  else Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(len);
end;

class function TByteBufferHelper.bb_tag_single(aTag: UInt32; aValue: Single): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wt32Bit)+bb_bytes(aValue, SizeOf(aValue));
end;

class function TByteBufferHelper.bb_tag_string(aTag: UInt32; const aValue: string): TByteBuffer;
var
  utf8: UTF8String;
  len: UInt64;
begin
  utf8 := UTF8String(aValue);
  len := length(utf8);
  if len>0
  then Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(len)+bb_bytes(utf8[1], len)
  else Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(len);
end;

class function TByteBufferHelper.bb_tag_tbytes(aTag: UInt32; const aValue: TBytes): TByteBuffer;
var
  len: UInt64;
begin
  len := length(aValue);
  if len>0
  then Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(len)+bb_bytes(aValue[1], len)
  else Result := bb_uint32((aTag shl 3) or wtLengthDelimited)+bb_uint64(len);
end;

class function TByteBufferHelper.bb_tag_uint32(aTag, aValue: UInt32): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wtVarInt)+bb_uint64(aValue);
end;

class function TByteBufferHelper.bb_tag_uint64(aTag: UInt32; aValue: UInt64): TByteBuffer;
begin
  Result := bb_uint32((aTag shl 3) or wtVarInt)+bb_uint64(aValue);
end;

class function TByteBufferHelper.bb_tbytes(const aValue: TBytes): TByteBuffer;
var
  len: uint64;
begin
  len := length(aValue);
  Result := bb_uint64(len)+bb_bytes(aValue[0], len);
end;

class function TByteBufferHelper.bb_uint16(aValue: Word): TByteBuffer;
begin
  setLength(Result, 2);
  Move(aValue, Result[1], 2);
end;

class function TByteBufferHelper.bb_uint32(aValue: UInt32): TByteBuffer;
var
  i: Integer;
begin
  SetLength(Result, VarIntLength(aValue));
  i := 1;
  while aValue>=128 do
  begin
    Result[i] := AnsiChar((aValue and $7F) or $80); // msb: signal more bytes are to follow
    Inc(i);
    aValue := aValue shr 7;
  end;
  // aValue<128 (msb already 0)
  Result[i] := AnsiChar(aValue);
end;

class function TByteBufferHelper.bb_uint64(aValue: UInt64): TByteBuffer;
var
  i: Integer;
begin
  SetLength(Result, VarIntLength(aValue));
  i := 1;
  while aValue>=128 do
  begin
    Result[i] := AnsiChar((aValue and $7F) or $80); // msb: signal more bytes are to follow
    Inc(i);
    aValue := aValue shr 7;
  end;
  // aValue<128 (msb already 0)
  Result[i] := AnsiChar(aValue);
end;

class function TByteBufferHelper.bb_utf8_string(const aValue: UTF8String): TByteBuffer;
begin
  Result := bb_ansi_string(AnsiString(aValue));
end;

function TByteBufferHelper.firstByte: Byte;
begin
  Result := Byte(self[1]);
end;

function TByteBufferHelper.lastByte: Byte;
begin
  Result := Byte(self[length(self)]);
end;

function TByteBufferHelper.ref(aCursor: Integer): Pointer;
begin
  Result := @self[aCursor+1];
end;

function TByteBufferHelper.refFirstByte: Pointer;
begin
  Result := @self[1];
end;

function TByteBufferHelper.refLastByte: Pointer;
begin
  Result := @self[length(self)];
end;

procedure TByteBufferHelper.shiftLeft;
begin
  if Length(self)>1
  then Move(self[2], self[1], Length(self)-1);
end;

function TByteBufferHelper.toHex: string;
var
  c: AnsiChar;
begin
  Result := '';
  for c in self
  do Result := Result+IntToHex(Ord(c),2);
end;

{ TStreamCacheEntry }

constructor TStreamCacheEntry.Create(const aStreamName: string; aStream: TStream);
begin
  inherited Create;
  fStreamName := aStreamName;
  fStream := aStream;
end;

destructor TStreamCacheEntry.Destroy;
begin
  FreeAndNil(fStream);
  inherited;
end;

{ TBatchedBuffer }

procedure TBatchBuffer.Add(const aEntry: TByteBuffer);
begin
  if length(aEntry)+length(fBatch)>fBatchSize
  then Commit;
  fBatch := fBatch+aEntry;
end;

procedure TBatchBuffer.Commit();
var
  e: TEventEntry;
begin
  if length(fBatch)>0 then
  begin
    for e in fEventEntries do
    begin
      e.publish();
      e.connection.writePacket(
        AnsiChar(imbMagic)+
        TByteBuffer.bb_int64(Length(fBatch)+2)+
        TByteBuffer.bb_uint16(e.EventID)+
        fBatch);
    end;
    fBatch := fHeader;
  end;
end;

procedure TBatchBuffer.Prepare(aBatchSize: Integer; aEventEntries: TArray<TEventEntry>; const aHeader: TByteBuffer);
begin
  fBatch := aHeader;
  fBatchSize := aBatchSize;
  fHeader := aHeader;
  fEventEntries := aEventEntries;
end;

{ TEventEntry }

constructor TEventEntry.Create(aConnection: TConnection; aEventID: TEventID; const aEventName: string);
begin
  fConnection := aConnection;
  fEventName := aEventName;
  fEventID := aEventID;
  fIsSubscribed := False;
  fIsPublished := False;
  fSubscribers := False;
  fPublishers := False;
  fStreamCache := TObjectDictionary<TGUID, TStreamCacheEntry>.Create([doOwnsValues]);
  // handlers
  fOnChangeObject := TList<TOnChangeObject>.Create;
  fOnString := TList<TOnString>.Create;
  fOnIntString := TList<TOnIntString>.Create;
  fOnTag := TList<TOnTag>.Create;
  fOnEvent := TList<TOnEvent>.Create;
  fOnStreamCreate := nil;
  fOnStreamEnd := nil;
  fOnSubAndPub := TList<TOnSubAndPub>.Create;
end;

destructor TEventEntry.Destroy;
begin
  FreeAndNil(fStreamCache);
  FreeAndNil(fOnChangeObject);
  FreeAndNil(fOnString);
  FreeAndNil(fOnIntString);
  FreeAndNil(fOnTag);
  FreeAndNil(fOnEvent);
  fOnStreamCreate := nil;
  fOnStreamEnd := nil;
  FreeAndNil(fOnSubAndPub);
  inherited;
end;

procedure TEventEntry.handleEvent(const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  eventInt: Integer;
  eventString: string;
  streamID: TGUID;
  cancel: Boolean;
  blockSize: UInt64;
  streamName: string;
  objectID: Integer;
  attribute: string;
  action: Integer;
  oit: TOnIntString;
  es: TOnString;
  oco: TOnChangeObject;
  stream: TStream;
  streamCacheEntry: TStreamCacheEntry;
  block: TByteBuffer;
  ot: TOnTag;
  oe: TOnEvent;
begin
  // first dispatch to onEvent handlers
  for oe in fOnEvent
  do oe(Self, aBuffer, aCursor, aLimit);
  // second handle tags
  // process tags
  eventString := '';
  action := -1;
  attribute := '';
  streamID := TGUID.Empty;
  while aCursor<aLimit do
  begin
    fieldInfo := aBuffer.bb_read_UInt32(aCursor);
    case fieldInfo of
      // int string
      (icehIntString shl 3) or wtVarInt:
        begin
          eventInt := aBuffer.bb_read_int32(aCursor);
          TMonitor.Enter(Self);
          try
            for oit in fOnIntString
            do oit(Self, eventInt, eventString);
          finally
            TMonitor.Exit(Self);
          end;
        end;
      (icehIntStringPayload shl 3) or wtLengthDelimited:
        begin
          eventString := aBuffer.bb_read_string(aCursor);
        end;
      // string
      (icehString shl 3) or wtLengthDelimited:
        begin
          eventString := aBuffer.bb_read_string(aCursor);
          TMonitor.Enter(Self);
          try
            for es in fOnString
            do es(Self, eventString);
          finally
            TMonitor.Exit(Self);
          end;
        end;
      // change object
      (icehChangeObjectAction shl 3) or wtVarInt:
        begin
          action := aBuffer.bb_read_int32(aCursor);
        end;
      (icehChangeObjectAttribute shl 3) or wtLengthDelimited:
        begin
          attribute := aBuffer.bb_read_string(aCursor);
        end;
      (icehChangeObject shl 3) or wtVarInt:
        begin
          objectID := aBuffer.bb_read_int32(aCursor);
          TMonitor.Enter(Self);
          try
            for oco in fOnChangeObject
            do oco(Self, action, objectID, attribute);
          finally
            TMonitor.Exit(Self);
          end;
        end;
      // streams
      (icehStreamHeader shl 3) or wtLengthDelimited:
        begin
          streamName := aBuffer.bb_read_string(aCursor);
          TMonitor.Enter(Self);
          try
            if Assigned(fOnStreamCreate) then
            begin
              stream := fOnStreamCreate(Self, streamName);
              if Assigned(stream) then
              begin
                if not fStreamCache.TryGetValue(streamID, streamCacheEntry) then
                begin
                  streamCacheEntry := TStreamCacheEntry.Create(streamName, stream);
                  fStreamCache.Add(streamID, streamCacheEntry);
                end
                else
                begin
                  streamCacheEntry.fStream.Free;
                  streamCacheEntry.fStream := stream;
                end;
              end;
            end;
          finally
            TMonitor.Exit(Self);
          end;
        end;
      (icehStreamBody shl 3) or wtLengthDelimited:
        begin
          blockSize := aBuffer.bb_read_uint64(aCursor);
          if blockSize>0 then
          begin
            block := aBuffer.bb_read_bytes(aCursor, blockSize);
            if fStreamCache.TryGetValue(streamID, streamCacheEntry)
            then streamCacheEntry.fStream.Write(block[1], blockSize);
          end;
        end;
      (icehStreamEnd shl 3) or wtVarInt:
        begin
          cancel := aBuffer.bb_read_bool(aCursor);
          if fStreamCache.TryGetValue(streamID, streamCacheEntry) then
          begin
            TMonitor.Enter(Self);
            try
              if Assigned(fOnStreamEnd)
              then fOnStreamEnd(Self, streamCacheEntry.fStreamName, streamCacheEntry.fStream, cancel);
            finally
              TMonitor.Exit(Self);
            end;
            fStreamCache.Remove(streamID); // should free TStreamCacheEntry
          end;
        end;
      (icehStreamID shl 3) or wtLengthDelimited:
        begin
          streamID := aBuffer.bb_read_guid(aCursor);
        end
    else
      TMonitor.Enter(Self);
      try
        for ot in fOnTag
        do ot(Self, fieldInfo, aBuffer, aCursor);
      finally
        TMonitor.Exit(Self);
      end;
      aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

procedure TEventEntry.handleSubAndPub(aCommand: UInt32);
var
  asap: TOnSubAndPub;
begin
  case aCommand of
    icehSubscribe: fSubscribers := True;
    icehPublish: fPublishers := True;
    icehUnsubscribe: fSubscribers := False;
    icehUnpublish: fPublishers := False;
  end;
  for asap in fOnSubAndPub
  do asap(Self, aCommand);
end;

function TEventEntry.publish: TEventEntry;
begin
  if not isPublished then
  begin
    signalPublish();
    fIsPublished := True;
  end;
  result := self;
end;

procedure TEventEntry.signalChangeObject(aAction, aObjectID: Integer; const aAttribute: string);
var
  payload: TByteBuffer;
begin
  publish();
  payload :=
    TByteBuffer.bb_uint16(EventID)+
    TByteBuffer.bb_tag_int32(icehChangeObjectAction, aAction)+
    TByteBuffer.bb_tag_string(icehChangeObjectAttribute, aAttribute)+
    TByteBuffer.bb_tag_int32(icehChangeObject, aObjectID);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
end;

procedure TEventEntry.signalEvent(const aPayload: TByteBuffer);
var
  payload: TByteBuffer;
begin
  publish();
  payload := TByteBuffer.bb_uint16(EventID)+aPayload;
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
end;

procedure TEventEntry.signalIntString(aInt: Integer; const aString: string);
var
  payload: TByteBuffer;
begin
  publish();
  payload :=
    TByteBuffer.bb_uint16(EventID)+
    TByteBuffer.bb_tag_string(icehIntStringPayload, aString)+
    TByteBuffer.bb_tag_int32(icehIntString, aInt);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
end;

procedure TEventEntry.signalPublish();
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_string(icehEventName, EventName)+
    TByteBuffer.bb_tag_uint32(icehPublish, EventID);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload);
end;

procedure TEventEntry.signalStream(const aName: string; aStream: TStream);
var
  bufferEventIDAndStreamID: TByteBuffer;
  payload: TByteBuffer;
  readSize: Longint;
  buffer: array[0..imbMaxStreamBodyBuffer-1] of byte;
begin
  publish();
  bufferEventIDAndStreamID :=
    TByteBuffer.bb_uint16(eventID)+
    TByteBuffer.bb_tag_guid(icehStreamID, TGUID.NewGuid);
  // header
  payload :=
    bufferEventIDAndStreamID+
    TByteBuffer.bb_tag_string(icehStreamHeader, aName);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
  // body
  readSize := aStream.Read(buffer[0], imbMaxStreamBodyBuffer);
  while readSize>0 do
  begin
    payload :=
      bufferEventIDAndStreamID+
      TByteBuffer.bb_tag_bytes(icehStreamBody, buffer[0], readSize);
    connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
    readSize := aStream.Read(buffer[0], imbMaxStreamBodyBuffer);
  end;
  // end
  payload :=
    bufferEventIDAndStreamID+
    TByteBuffer.bb_tag_bool(icehStreamEnd, readSize<>0);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
end;

procedure TEventEntry.signalString(const aString: string);
var
  payload: TByteBuffer;
begin
  publish();
  payload :=
    TByteBuffer.bb_uint16(EventID)+
    TByteBuffer.bb_tag_string(icehString, aString);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(Length(payload))+payload);
end;

procedure TEventEntry.signalSubscribe();
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_string(icehEventName, EventName)+
    TByteBuffer.bb_tag_uint32(icehSubscribe, EventID);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload);
end;

procedure TEventEntry.signalUnPublish();
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_uint32(icehUnPublish, EventID);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload);
end;

procedure TEventEntry.signalUnSubscribe();
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_uint32(icehUnSubscribe, EventID);
  connection.writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload);
end;

function TEventEntry.subscribe: TEventEntry;
begin
  if not isSubscribed then
  begin
    signalSubscribe();
    fIsSubscribed := True;
  end;
  result := self;
end;

function TEventEntry.unPublish: TEventEntry;
begin
  if isPublished then
  begin
    signalUnPublish();
    fIsPublished := False;
  end;
  result := self;
end;

function TEventEntry.unSubscribe: TEventEntry;
begin
  if isSubscribed then
  begin
    signalUnSubscribe();
    fIsSubscribed := False;
  end;
  result := self;
end;

{ TConnection }

procedure TConnection.close(aSendCloseCmd: Boolean);
var
  payload: TByteBuffer;
begin
  if connected then
  begin
    if Assigned(fOnDisconnect)
    then fOnDisconnect(Self);
    if aSendCloseCmd then
    begin
      payload := TByteBuffer.bb_tag_bool(icehClose, False);
      writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload, False);
    end;
    connected := False;
  end;
end;

constructor TConnection.Create(const aModelName: string; aModelID: Integer; const aPrefix: string);
begin
  inherited Create;
  fReaderThread := nil;
  fHeartbeatThread := nil;
  fHeartbeatQuitEvent := INVALID_HANDLE_VALUE;
  fModelName := aModelName;
  fModelID := aModelID;
  fPrefix := aPrefix;
  fLocalEventEntries := TObjectList<TEventEntry>.Create;
  fRemoteEventEntries := TDictionary<TEventID, TEventEntry>.Create;
  fOnDisconnect := nil;
  fOnException := nil;
  fUniqueClientID := TGUID.Empty;
  fHubID := TGUID.Empty;
end;

destructor TConnection.Destroy;
begin
  connected := False;
  FreeAndNil(fReaderThread);
  if fHeartbeatQuitEvent<>INVALID_HANDLE_VALUE
  then SetEvent(fHeartbeatQuitEvent);
  FreeAndNil(fHeartbeatThread);
  if fHeartbeatQuitEvent<>INVALID_HANDLE_VALUE then
  begin
    CloseHandle(fHeartbeatQuitEvent);
    fHeartbeatQuitEvent := INVALID_HANDLE_VALUE;
  end;
  FreeAndNil(fLocalEventEntries);
  FreeAndNil(fRemoteEventEntries);
  inherited;
end;

function TConnection.getMonitorEventName: string;
begin
  if fHubID<>TGUID.Empty
  then Result := 'Hubs.'+GUIDToStringCompact(fHubID)+'.Monitor'
  else Result := '';
end;

function TConnection.getPrivateEventName: string;
begin
  if fUniqueClientID<>TGUID.Empty
  then Result := 'Clients.'+GUIDToStringCompact(fUniqueClientID)+'.Private'
  else Result := '';
end;

procedure TConnection.handleCommand(const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  remoteEventID: TEventID;
  eventName: string;
  localEventID: TEventID;
  eventEntry: TEventEntry;
begin
  eventName := '';
  localEventID := imbInvalidEventID;
  // process tags
  while aCursor<aLimit do
  begin
    fieldInfo := aBuffer.bb_read_UInt32(aCursor);
    case fieldInfo of
      (icehSubscribe shl 3) or wtVarInt:
        begin
          remoteEventID := aBuffer.bb_read_uint32(aCursor);
          if fRemoteEventEntries.TryGetValue(remoteEventID, eventEntry)
          then eventEntry.handleSubAndPub(icehSubscribe);
        end;
      (icehPublish shl 3) or wtVarInt:
        begin
          remoteEventID := aBuffer.bb_read_uint32(aCursor);
          if fRemoteEventEntries.TryGetValue(remoteEventID, eventEntry)
          then eventEntry.handleSubAndPub(icehPublish);
        end;
      (icehUnsubscribe shl 3) or wtVarInt:
        begin
          eventName := '';
          remoteEventID := aBuffer.bb_read_uint32(aCursor);
          if fRemoteEventEntries.TryGetValue(remoteEventID, eventEntry)
          then eventEntry.handleSubAndPub(icehUnsubscribe);
        end;
      (icehUnpublish shl 3) or wtVarInt:
        begin
          eventName := '';
          remoteEventID := aBuffer.bb_read_uint32(aCursor);
          if fRemoteEventEntries.TryGetValue(remoteEventID, eventEntry)
          then eventEntry.handleSubAndPub(icehUnpublish);
        end;
      (icehEventName shl 3) or wtLengthDelimited:
        begin
          eventName := aBuffer.bb_read_string(aCursor);
        end;
      (icehEventID shl 3) or wtVarInt:
        begin
          localEventID := aBuffer.bb_read_uint32(aCursor);
        end;
      (icehSetEventIDTranslation shl 3) or wtVarInt:
        begin
          remoteEventID := aBuffer.bb_read_uint32(aCursor);
          TMonitor.Enter(self);
          try
            if localEventID<TEventID(fLocalEventEntries.Count) then
            begin
              eventEntry := fLocalEventEntries[localEventID];
              fRemoteEventEntries.AddOrSetValue(remoteEventID, eventEntry);
            end
            else fRemoteEventEntries.Remove(remoteEventID); // local event id is invalid so invalidate remote event id -> remove
          finally
            TMonitor.Exit(self);
          end;
        end;
      (icehClose shl 3) or wtVarInt:
        begin
          aBuffer.bb_read_bool(aCursor);
          close(false);
        end;
      (icehHubID shl 3) or wtLengthDelimited:
        begin
          fHubID := aBuffer.bb_read_guid(aCursor);
        end;
      (icehUniqueClientID shl 3) or wtLengthDelimited:
        begin
          fUniqueClientID := aBuffer.bb_read_guid(aCursor);
        end;
    else
      aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
    end;
  end;
end;

function TConnection.publish(const aEventName: string; aUsePrefix: Boolean): TEventEntry;
var
  longEventName: string;
  eventEntry: TEventEntry;
  upperLongEventName: string;
begin
  if aUsePrefix
  then longEventName := fPrefix+'.'+aEventName
  else longEventName := aEventName;
  upperLongEventName := longEventName.ToUpper;
  TMonitor.Enter(Self);
  try
    for eventEntry in fLocalEventEntries do
    begin
      if eventEntry.EventName.ToUpper=upperLongEventName then
      begin
        eventEntry.publish();
        Exit(eventEntry);
      end;
    end;
    // not found -> create new event entry
    eventEntry := TEventEntry.Create(Self, fLocalEventEntries.Count, longEventName);
    fLocalEventEntries.Add(eventEntry);
    eventEntry.publish();
    Exit(eventEntry);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TConnection.readPackets;
var
  packet: TByteBuffer;
  receivedBytes: Integer;
  cursor: Integer;
  size: Int64;
  limit: Integer;
  eventID: TEventID;
  eventEntry: TEventEntry;
  extraBytes: Integer;
begin
  // read from socket and process
  setLength(packet, imbMinimumPacketSize);
  while not TThread.CheckTerminated do
  begin
    try
      receivedBytes := readBytes(packet.refFirstByte^, imbMinimumPacketSize);
      if receivedBytes=imbMinimumPacketSize then
      begin
        // check magic byte
        while packet.firstByte<>imbMagic do
        begin
          packet.shiftLeft;
          if readBytes(packet.refLastByte^, 1)<1 then
          begin
            close(False);
            Exit;
          end;
        end;
        cursor := 1; // TByteBuffer is 0-based, cursor now on first byte after magic
        size := packet.bb_read_int64(cursor);
        limit := cursor+Abs(size);
        // make sure all packet data is read
        if limit>imbMinimumPacketSize then
        begin
          extraBytes := limit-imbMinimumPacketSize;
          if length(packet)<imbMinimumPacketSize+extraBytes
          then setLength(packet, imbMinimumPacketSize+extraBytes);
          receivedBytes := readBytes(packet.ref(imbMinimumPacketSize)^, extraBytes);
          if receivedBytes<extraBytes then
          begin
            close(False);
            Exit;
          end;
        end;
        // handle packet
        if size>0 then
        begin
          // handle event
          eventID := packet.bb_read_uint16(cursor);
          if fRemoteEventEntries.TryGetValue(eventID, eventEntry)
          then eventEntry.handleEvent(packet, cursor, limit);
          // else discard unsubscribed event
        end
        else
        begin
          // handle command
          handleCommand(packet, cursor, limit);
        end;
      end
      else
      begin
        close(False);
        Exit;
      end;
    except
      on E: Exception do
      begin
        if Assigned(fOnException)
        then fOnException(self, E);
        close(False);
        Exit;
      end;
    end;
  end;
end;

procedure TConnection.sendHeartbeats;
begin
  while (not TThread.CheckTerminated) do
  begin
    signalHeartBeat();
    WaitForSingleObject(fHeartbeatQuitEvent, fHeartbeatInterval);
  end;
end;

procedure TConnection.setHeartbeat(aIntervalms: Integer);
begin
  if fHeartbeatInterval<>aIntervalms then
  begin
    fHeartbeatInterval := aIntervalms;
    if aIntervalms>0 then
    begin
      if fHeartbeatQuitEvent=INVALID_HANDLE_VALUE
      then fHeartbeatQuitEvent := CreateEvent(nil, True, False, nil)
      else ResetEvent(fHeartbeatQuitEvent);
      if not Assigned(fHeartbeatThread) then
      begin
        fHeartbeatThread := TThread.CreateAnonymousThread(sendHeartbeats);
        fHeartbeatThread.FreeOnTerminate := False;
        fHeartbeatThread.NameThreadForDebugging('imb heartbeat sender', fHeartbeatThread.ThreadID);
        fHeartbeatThread.Start;
      end;
    end
    else
    begin
      FreeAndNil(fHeartbeatThread);
    end;
  end;
end;

procedure TConnection.signalConnectInfo(const aModelName: string; aModelID: Integer);
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_string(icehModelName,aModelName)+
    TByteBuffer.bb_tag_int32(icehModelID, aModelID)+
    TByteBuffer.bb_tag_uint32(icehState, icsClient)+
//    bb_tag_bool(icehReconnectable, False)+
//    bb_tag_string(icehEventNameFilter, '')+
    TByteBuffer.bb_tag_guid(icehUniqueClientID, fUniqueClientID); // trigger
  writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload);
end;

procedure TConnection.signalHeartBeat(const aRemark: string);
// a heart beat is an empty command or an icehRemark (if aRemark<>'')
var
  payload: TByteBuffer;
begin
  if aRemark=''
  then writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(0))
  else
  begin
    payload := TByteBuffer.bb_tag_string(icehRemark, aRemark);
    writePacket(AnsiChar(imbMagic)+TByteBuffer.bb_int64(-Length(payload))+payload);
  end;
end;

function TConnection.subscribe(const aEventName: string; aUsePrefix: Boolean): TEventEntry;
var
  longEventName: string;
  eventEntry: TEventEntry;
  upperLongEventName: string;
begin
  if aUsePrefix
  then longEventName := fPrefix+'.'+aEventName
  else longEventName := aEventName;
  upperLongEventName := longEventName.ToUpper;
  TMonitor.Enter(Self);
  try
    for eventEntry in fLocalEventEntries do
    begin
      if eventEntry.EventName.ToUpper=upperLongEventName
      then Exit(eventEntry.subscribe);
    end;
    // not found -> create new event entry
    eventEntry := TEventEntry.Create(Self, fLocalEventEntries.Count, longEventName);
    fLocalEventEntries.Add(eventEntry);
    Exit(eventEntry.subscribe);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TConnection.unPublish(aEventEntry: TEventEntry);
begin
  TMonitor.Enter(Self);
  try
    fLocalEventEntries[aEventEntry.EventID].unPublish;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TConnection.unSubscribe(aEventEntry: TEventEntry);
begin
  TMonitor.Enter(Self);
  try
    fLocalEventEntries[aEventEntry.EventID].unSubscribe;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TConnection.waitForConnected(aSpinCount: Integer);
begin
  while (fUniqueClientID=TGUID.Empty) and (aSpinCount<>0) do
  begin
    sleep(100);
    Dec(aSpinCount);
  end;
end;

{$IFDEF imbSocketSupport}

{ TSocketConnection }

constructor TSocketConnection.Create(const aModelName: string; aModelID: Integer; const aPrefix: string;
  const aRemoteHost: string; aRemotePort: Integer);
begin
  inherited Create(aModelName, aModelID, aPrefix);
  fSocket := INVALID_SOCKET;
  fRemoteHost := aRemoteHost;
  fRemotePort := aRemotePort;
  // try to connect
  connected := True;
  if not connected
  then raise Exception.Create('Could not connect to '+aRemoteHost+':'+aRemotePort.toString);
end;

function TSocketConnection.getConnected: Boolean;
begin
  Result := fSocket<>INVALID_SOCKET;
end;

function TSocketConnection.readBytes(var aBuffer; aNumberOfBytes: Integer): Integer;
begin
  Result := recv(fSocket, aBuffer, aNumberOfBytes, MSG_WAITALL);
end;

procedure TSocketConnection.setConnected(aValue: Boolean);
var
  a: Integer;
  addresses: TSockAddresses;
begin
  if aValue then
  begin
    if not connected then
    begin
      // resolve host
      addresses := getSocketAddresses(fRemoteHost, IntToStr(fRemotePort));
      a := 0;
      while a<Length(addresses) do
      begin
        // try to connect to resolved address
        fSocket := socket(addresses[a].Family, SOCK_STREAM, IPPROTO_TCP);
        if fSocket<>SOCKET_ERROR then
        begin
          if connect(fSocket, addresses[a], sizeof(addresses[a]))<>SOCKET_ERROR then
          begin
            // connected
            // set socket options
            setSocketLinger(fSocket, imbSocketDefaultLingerTimeInSec);
            // start reader thread
            fReaderThread.Free;
            fReaderThread := TThread.CreateAnonymousThread(readPackets);
            fReaderThread.FreeOnTerminate := False;
            fReaderThread.NameThreadForDebugging('imb event reader', fReaderThread.ThreadID);
            fReaderThread.Start;
            // send connect info
            signalConnectInfo(fModelName, fModelID);
            // wait for unique client id as a signal that we are connected
            waitForConnected;
            Exit;
          end
          else
          begin
            MyCloseSocket(fSocket);
            a := a+1;
          end;
        end
        else a := a+1;
      end;
    end;
  end
  else
  begin
    if connected then
    begin
      fReaderThread.Terminate;
      MyCloseSocket(fSocket);
    end;
  end;
end;

function TSocketConnection.writePacket(aPacket: TByteBuffer; aCloseOnError: Boolean): Boolean;
begin
  Result := False;
  if connected then
  begin
    // minimum packet length is imbMinimumPacketSize bytes
    if length(aPacket)<imbMinimumPacketSize
    then Setlength(aPacket, imbMinimumPacketSize);
    TMonitor.Enter(Self);
    try
      if send(fSocket, aPacket[1], Length(aPacket), 0)=SOCKET_ERROR then
      begin
        if aCloseOnError
        then close(false);
      end
      else Result := True;
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

{$ENDIF}

{$IFDEF imbTLSSupport}

{ TTLSConnection }

constructor TTLSConnection.Create(const aCertFile, aKeyFile, aKeyFilePassword, aRootCertFile, aModelName: string; aModelID: Integer; const aPrefix,
  aRemoteHost: string; aRemotePort: Integer);
begin
  inherited Create(aModelName, aModelID, aPrefix);
  fKeyFilePassword := aKeyFilePassword;
  // io handler
  fIdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  // setup
  //fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.CipherList := 'ECDHE-ECDSA-AES256-GCM-SHA384';
  //fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.CipherList := 'AES256-GCM-SHA384';
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.CertFile := aCertFile;
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.KeyFile := aKeyFile;
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.Method := sslvTLSv1_2;
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.Mode := sslmClient;// sslmBoth;
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.RootCertFile := aRootCertFile;
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.SSLVersions := [sslvTLSv1_2];
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.VerifyDepth := 2;
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.VerifyDirs := '.';
  fIdSSLIOHandlerSocketOpenSSL1.SSLOptions.VerifyMode := [sslvrfPeer, sslvrfFailIfNoPeerCert];
  // handlers
  fIdSSLIOHandlerSocketOpenSSL1.OnGetPassword := HandleGetPassword;
  fIdSSLIOHandlerSocketOpenSSL1.OnStatusInfo := HandleStatusInfo;
  fIdSSLIOHandlerSocketOpenSSL1.OnVerifyPeer := HandleVerifyPeer;
  // tcp client
  fIdTCPClient1 := TIdTCPClient.Create(nil);
  // setup
  fIdTCPClient1.Host := aRemoteHost;
  fIdTCPClient1.Port := aRemotePort;
  // handlers
//  fIdTCPClient1.OnConnected := HandleConnected;
  fIdTCPClient1.OnDisconnected := HandleDisconnected;
  fIdTCPClient1.OnStatus := HandleStatus;
  // link handler to tcp client
  fIdTCPClient1.IOhandler := fIdSSLIOHandlerSocketOpenSSL1;
  // try to connect
  connected := True;
  if not connected
  then raise Exception.Create('Could not connect to '+aRemoteHost+':'+aRemotePort.toString);
end;

function TTLSConnection.DataAvailable: Boolean;
begin
  Result := fIdSSLIOHandlerSocketOpenSSL1.Readable(5);
end;

destructor TTLSConnection.Destroy;
begin
  fIdTCPClient1.Socket.Binding.CloseSocket;
  FreeAndNil(fReaderThread);
  inherited;
  FreeAndNil(fIdTCPClient1);
  FreeAndNil(fIdSSLIOHandlerSocketOpenSSL1);
end;

function TTLSConnection.getConnected: Boolean;
begin
  Result := fIdTCPClient1.Connected;
end;

procedure TTLSConnection.HandleDisconnected(Sender: TObject);
begin
  Close(False);
end;

procedure TTLSConnection.HandleGetPassword(var aPassword: String);
begin
  aPassword := fKeyFilePassword;
end;

procedure TTLSConnection.HandleStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  {$IFDEF CONSOLE}
  WriteLn('HandleStatus: '+Ord(aStatus).ToString()+': '+astatustext);
  {$ENDIF}
end;

procedure TTLSConnection.HandleStatusInfo(const aMsg: String);
begin
  {$IFDEF CONSOLE}
  WriteLn('HandleStatusInfo: '+aMsg);
  {$ENDIF}
end;

function TTLSConnection.HandleVerifyPeer(aCertificate: TIdX509; aOk: Boolean; aDepth, aError: Integer): Boolean;
begin
  {$IFDEF CONSOLE}
  if aOK
  then WriteLn('IO handler VerifyPeer ('+aDepth.ToString()+') '+aerror.ToString())
  else WriteLn('## IO handler VerifyPeer ('+aDepth.ToString()+') '+aerror.ToString());
  if Assigned(aCertificate) then
  begin
    WriteLn('      Valid: '+DateTimeToStr(aCertificate.notBefore)+' - '+DateTimeToStr(aCertificate.notAfter));
    if Assigned(aCertificate.Issuer)
    then WriteLn('      Issuer: '+aCertificate.Issuer.oneline);
    if Assigned(aCertificate.Subject)
    then WriteLn('      Subject: '+aCertificate.Subject.oneline);
  end;
  {$ENDIF}
  Result := aOk;
end;

function TTLSConnection.readBytes(var aBuffer; aNumberOfBytes: Integer): Integer;
var
  localBuffer: TIdBytes;
begin
  // SetLength(localBuffer, 0); not needed
  fIdTCPClient1.IOHandler.ReadBytes(localBuffer, aNumberOfBytes);
  //fIdTCPClient1.IOHandler.InputBuffer
  //fIdSSLIOHandlerSocketOpenSSL1.ReadBytes();
  Result := Length(localBuffer);
  if Result>0
  then Move(localBuffer[0], aBuffer, Result);
end;

procedure TTLSConnection.setConnected(aValue: Boolean);
begin
  if aValue then
  begin
    if not connected then
    begin
      fIdTCPClient1.Connect;
      if connected then
      begin
        // start reader thread
        fReaderThread.Free;
        fReaderThread := TThread.CreateAnonymousThread(readPackets);
        fReaderThread.FreeOnTerminate := False;
        fReaderThread.NameThreadForDebugging('imb event reader', fReaderThread.ThreadID);
        fReaderThread.Start;
        // send connect info
        signalConnectInfo(fModelName, fModelID);
        // wait for unique client id as a signal that we are connected
        waitForConnected;
      end;
    end;
  end
  else
  begin
    if connected then
    begin
      fReaderThread.Terminate;
      fIdTCPClient1.Disconnect;
    end;
  end;
end;

function TTLSConnection.writePacket(aPacket: TByteBuffer; aCloseOnError: Boolean): Boolean;
var
  localBuffer: TIdBytes;
  numberOfBytes: Integer;
begin
  numberOfBytes := length(aPacket);
  TMonitor.Enter(Self);
  try
    if Connected then
    begin
      // minimum packet length is imbMinimumPacketSize bytes
      if numberOfBytes>=imbMinimumPacketSize
      then SetLength(localBuffer, numberOfBytes)
      else
      begin
        SetLength(localBuffer, imbMinimumPacketSize);
        FillChar(localBuffer[0], imbMinimumPacketSize, 0);
      end;
      if numberOfBytes>0
      then Move(aPacket.refFirstByte^, localBuffer[0], numberOfBytes);
      fIdTCPClient1.IOHandler.Write(localBuffer);
      Result := True;
    end
    else Result := False;
  finally
    TMonitor.Exit(Self);
  end;
end;

{$ENDIF}

{$IFDEF imbSharedMemSupport}

{ TSharedMemConnection }

constructor TSharedMemConnection.Create(const aModelName: string; aModelID: Integer; const aPrefix, aServerChannelName: string);
begin
  inherited Create(aModelName, aModelID, aPrefix);
  fSMFile := TSMFile.Empty.Empty;
  fServerChannelName := aServerChannelName;
  // try to connect
  connected := True;
  if not connected
  then raise Exception.Create('Could not connect to '+aServerChannelName);
end;

function TSharedMemConnection.getConnected: Boolean;
begin
  Result := fSMFile.Connected;
end;

function TSharedMemConnection.readBytes(var aBuffer; aNumberOfBytes: Integer): Integer;
begin
  Result := fSMFile.Channel[smfDefaultClientReadChannel].Read(aBuffer, aNumberOfBytes);
end;

procedure TSharedMemConnection.setConnected(aValue: Boolean);
begin
  if aValue then
  begin
    if not connected then
    begin
      fSMFile := ConnectToSharedMemConnection(fServerChannelName);
      if connected then
      begin
        fReaderThread.Free;
        fReaderThread := TThread.CreateAnonymousThread(readPackets);
        fReaderThread.FreeOnTerminate := False;
        fReaderThread.NameThreadForDebugging('imb event reader', fReaderThread.ThreadID);
        fReaderThread.Start;
        // send connect info
        signalConnectInfo(fModelName, fModelID);
        // wait for unique client id as a signal that we are connected
        waitForConnected;
      end;
    end;
  end
  else
  begin
    if connected then
    begin
      fReaderThread.Terminate;
      fSMFile.Close(True);
      fSMFile := TSMFile.Empty;
    end;
  end;
end;

function TSharedMemConnection.writePacket(aPacket: TByteBuffer; aCloseOnError: Boolean): Boolean;
begin
  Result := False;
  if connected then
  begin
    // minimum packet length is imbMinimumPacketSize bytes
    if length(aPacket)<imbMinimumPacketSize
    then Setlength(aPacket, imbMinimumPacketSize);
    TMonitor.Enter(Self);
    try
      if fSMFile.Channel[smfDefaultClientWriteChannel].Write(aPacket[1], Length(aPacket))<0 then
      begin
        if aCloseOnError
        then close(false);
      end
      else Result := True;
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

{$ENDIF}

end.
