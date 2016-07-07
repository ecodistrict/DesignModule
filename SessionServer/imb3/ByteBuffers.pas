unit ByteBuffers;

{
  Byte buffers are based on BinStr
  Protocol buffers are from Google:
    http://code.google.com/intl/nl-NL/apis/protocolbuffers/docs/encoding.html

  per property

    messages
      has<FieldName>
      get<FieldName>
      isInitialialized()
      toString()

      byte[] toByteArray()
      static <MessageName> parseFrom(byte[] data)
      void writeTo(OutputStream output)
      static <MessageName> parseFrom(InputStream input)

    builders
      has<FieldName>
      get<FieldName>
      set<FieldName>
      clear<FieldName>
      add<FieldName>
      build()
      mergeFrom(Message other)
      clear()

    http://code.google.com/intl/nl-NL/apis/protocolbuffers/docs/javatutorial.html
}

interface

uses
  SysUtils;

type
  // google protocol buffer types
  // check read type to data type with CheckWireType
  TWireType = (
    wtVarInt=0,            // int32, int64, uint32, uint64, sint32, sint64, bool, enum
    wt64Bit=1,             // doulble or fixed int64/uint64
    wtLengthDelimited=2,   // string, bytes, embedded messages, packed repeated fields
    wtStartGroup=3,        // depricated
    wtEndGroup=4,          // depricated
    wt32Bit=5              // float or fixed int32/uint32
  );

  Fixed64 = UInt64;
  SFixed64 = Int64;
  Fixed32 = UInt32;
  SFixed32 = Int32;

type
  TByteBufferBuffer = RawByteString;
  TByteBufferElement = AnsiChar;
  {$DEFINE OneBasedByteBuffer}

  PByteBuffer = ^TByteBuffer;
  TByteBuffer = record
  private
    FBuffer: TByteBufferBuffer;
    FReadCursor: Integer;
    FPrepareCursor: Integer;
    FWriteCursor: Integer;
    FPBWriteCursor: Integer;
    function GetAddress: Pointer;
    function GetBytes(aIndex: Integer): Byte;
    function GetLength: Integer;
    procedure SetBytes(aIndex: Integer; aValue: Byte);
    procedure SetLength(const aValue: Integer);
    procedure SetBuffer(const aValue: TByteBufferBuffer);
  public
    procedure Clear(aSize: Integer=0);
    property Length: Integer read GetLength write SetLength;
    property Address: Pointer read GetAddress;
    property Bytes[aIndex: Integer]: Byte read GetBytes write SetBytes; default;
    property Buffer: TByteBufferBuffer read FBuffer write SetBuffer;
    procedure Assign(const aBuffer: TByteBuffer);
    function IsEmpty: Boolean;
    // start reading at beginning of buffer
    procedure ReadStart(aIndex: Integer=0);
    // return size (in bytes) that are available for reading from the current reading cursor position
    function ReadAvailable: Integer;
    function ReadAddr: Pointer;
    property ReadCursor: Integer read FReadCursor;
    // read with checking of available data and advance reading cursor
    function Read(out aValue: Boolean): Boolean; overload;
    function Read(out aValue: Byte): Boolean; overload;
    function Read(out aValue: Integer): Boolean; overload;
    function Read(out aValue: Int64): Boolean; overload;
    function Read(out aValue: Single): Boolean; overload;
    function Read(out aValue: Double): Boolean; overload;
    function Read(out aValue: TDateTime): Boolean; overload;
    function Read(out aValue: AnsiString): Boolean; overload;
    //function Read(out aValue: RawByteString): Boolean; overload;
    function Read(out aValue: string): Boolean; overload;
    function Read(var aValue; aValueSize: Integer): Boolean; overload;
    // like read but value as function result
    function ReadBoolean(aDefaultValue: Boolean=False): Boolean;
    function ReadByte(aDefaultValue: Byte=0): Byte;
    function ReadInteger(aDefaultValue: Integer=0): Integer;
    function ReadInt64(const aDefaultValue: Int64=0): Int64;
    function ReadSingle(const aDefaultValue: Single=0): Single;
    function ReadDouble(const aDefaultValue: Double=0): Double;
    function ReadAnsiString(const aDefaultValue: AnsiString=''): AnsiString;
    // read specific types and advance reading cursor but no checking of available data
    procedure QRead(out aValue: Boolean); overload;
    procedure QRead(out aValue: Byte); overload;
    procedure QRead(out aValue: Integer); overload;
    procedure QRead(out aValue: Int64); overload;
    procedure QRead(out aValue: Single); overload;
    procedure QRead(out aValue: Double); overload;
    procedure QRead(out aValue: AnsiString); overload;
    procedure QRead(var aValue; aValueSize: Integer); overload;
    // read rest of buffer from reading cursor position
    procedure ReadRest(var aValue: TByteBuffer); overload;
    function ReadRest: AnsiString; overload;
    // skip reading for a number of bytes ie advance reading cursor
    procedure SkipReading(aValueSize: Integer);
    // remove all that is read from buffer
    //procedure ReadFlush;
    // read without moving cursor but with given offset to current cursor position
    function Peek(out aValue: Boolean; aOffset: Integer): Boolean; overload;
    function Peek(out aValue: Byte; aOffset: Integer): Boolean; overload;
    function Peek(out aValue: Integer; aOffset: Integer): Boolean; overload;
    function Peek(out aValue: Int64; aOffset: Integer): Boolean; overload;
    function Peek(out aValue: Single; aOffset: Integer): Boolean; overload;
    function Peek(out aValue: Double; aOffset: Integer): Boolean; overload;
    function Peek(out aValue: AnsiString; aOffset: Integer): Boolean; overload;
    function Peek(var aValue; aValueSize: Integer; aOffset: Integer): Boolean; overload;
    // like peek but value as function result
    function PeekBoolean(aOffset: Integer; aDefaultValue: Boolean=False): Boolean;
    function PeekByte(aOffset: Integer; aDefaultValue: Byte=0): Byte;
    function PeekInteger(aOffset: Integer; aDefaultValue: Integer=0): Integer;
    function PeekInt64(aOffset: Integer; const aDefaultValue: Int64=0): Int64;
    function PeekSingle(aOffset: Integer; const aDefaultValue: Single=0): Single;
    function PeekDouble(aOffset: Integer; const aDefaultValue: Double=0): Double;
    function PeekAnsiString(aOffset: Integer; const aDefaultValue: AnsiString=''): AnsiString;

    function Compare(const aValue; aValueSize: Integer; aOffset: Integer=0): Boolean;

    // start preparing room creation at writing cursor
    function PrepareStart: Integer;
    //property PrepareCursor: Integer read FPrepareCursor write FPrepareCursor;
    // prepare room for a type
    procedure Prepare(const aValue: Boolean); overload;
    procedure Prepare(const aValue: Byte); overload;
    procedure Prepare(const aValue: Integer); overload;
    procedure Prepare(const aValue: Int64); overload;
    procedure Prepare(const aValue: Single); overload;
    procedure Prepare(const aValue: Double); overload;
    procedure Prepare(const aValue: AnsiString); overload;
    procedure Prepare(const aValue: string); overload;
    procedure Prepare(const aValue: TByteBuffer); overload;
    procedure Prepare(const aValue; aValueSize: Integer); overload;
    // prepare room of specified size, return starting index
    function PrepareSize(aValueSize: Integer): Integer;
    // create prepared room for writing
    procedure PrepareApply;
    procedure PrepareApplyAndTrim;

    // start writing at beginning of buffer
    procedure WriteStart(aIndex: Integer=0);
    property WriteCursor: Integer read FWriteCursor;
    property PBWriteCursor: Integer read FPBWriteCursor;
    // return size (in bytes) that are available for writing from the current writing cursor position
    function WriteAvailable: Integer;
    // write variable to buffer and advance writing cursor making sure there is room for writing
    procedure Write(const aValue: Boolean); overload;
    procedure Write(const aValue: Byte); overload;
    procedure Write(const aValue: Integer); overload;
    procedure Write(const aValue: Int64); overload;
    procedure Write(const aValue: Single); overload;
    procedure Write(const aValue: Double); overload;
    procedure Write(const aValue: AnsiString); overload;
    procedure Write(const aValue: string); overload;
    procedure Write(const aValue: TByteBuffer); overload;
    procedure Write(const aValue; aValueSize: Integer); overload;
    // write variable to buffer and advance writing cursor: room has to be prepared before!
    procedure QWrite(const aValue: Boolean); overload;
    procedure QWrite(const aValue: Byte); overload;
    procedure QWrite(const aValue: Integer); overload;
    procedure QWrite(const aValue: Int64); overload;
    procedure QWrite(const aValue: Single); overload;
    procedure QWrite(const aValue: Double); overload;
    procedure QWrite(const aValue: AnsiString); overload;
    procedure QWrite(const aValue: string); overload;
    procedure QWrite(const aValue: TByteBuffer); overload;
    procedure QWrite(const aValue; aValueSize: Integer); overload;
    // set length buffer to current writing position
    procedure WriteApply;
    function Written(aValueSize: Integer): Boolean;

    // protocol buffers

    function PBPrepare(aValue: Boolean): Integer; overload;
    function PBPrepare(aValue: UInt64): Integer; overload;
    function PBPrepare(aValue: Int64): Integer; overload;
    function PBPrepare(aValue: UInt32): Integer; overload;
    function PBPrepare(aValue: Int32): Integer; overload;
    function PBPrepare(const aValue: AnsiString): Integer; overload;
    function PBPrepare(const aValue: string): Integer; overload;
    function PBPrepare(const aValue; aValueSize: UInt64): Integer; overload;
    function PBPrepare(aValue: Single): Integer; overload;
    function PBPrepare(aValue: Double): Integer; overload;

    function PBPrepareFixed(aValue: Fixed64): Integer; overload;
    function PBPrepareFixed(aValue: SFixed64): Integer; overload;
    function PBPrepareFixed(aValue: Fixed32): Integer; overload;
    function PBPrepareFixed(aValue: SFixed32): Integer; overload;

    function PBPrepareFieldInfo(aTag: UInt64; aWireType: TWireType): Integer;

    function PBPrepareField(aTag: UInt64; aValue: Boolean): Integer; overload;
    function PBPrepareField(aTag: UInt64; aValue: UInt64): Integer; overload;
    function PBPrepareField(aTag: UInt64; aValue: Int64): Integer; overload;
    function PBPrepareField(aTag: UInt64; aValue: UInt32): Integer; overload;
    function PBPrepareField(aTag: UInt64; aValue: Int32): Integer; overload;
    function PBPrepareField(aTag: UInt64; const aValue: AnsiString): Integer; overload;
    function PBPrepareField(aTag: UInt64; const aValue: string): Integer; overload;
    function PBPrepareField(aTag: UInt64; const aValue; aValueSize: UInt64): Integer; overload;
    function PBPrepareField(aTag: UInt64; aValue: Single): Integer; overload;
    function PBPrepareField(aTag: UInt64; aValue: Double): Integer; overload;

    function PBPrepareFixedField(aTag: UInt64; aValue: Fixed64): Integer; overload;
    function PBPrepareFixedField(aTag: UInt64; aValue: SFixed64): Integer; overload;
    function PBPrepareFixedField(aTag: UInt64; aValue: Fixed32): Integer; overload;
    function PBPrepareFixedField(aTag: UInt64; aValue: SFixed32): Integer; overload;

    // PBQ functions work backwards (reverse order) on own cursor

    function PBQWrite(aValue: Boolean): Integer; overload;
    function PBQWrite(aValue: UInt64): Integer; overload;
    function PBQWrite(aValue: Int64): Integer; overload;
    function PBQWrite(aValue: UInt32): Integer; overload;
    function PBQWrite(aValue: Int32): Integer; overload;
    function PBQWrite(const aValue: AnsiString): Integer; overload;
    function PBQWrite(const aValue: string): Integer; overload;
    function PBQWrite(const aValue; aValueSize: UInt64): Integer; overload;
    function PBQWrite(aValue: Single): Integer; overload;
    function PBQWrite(aValue: Double): Integer; overload;

    function PBQWriteFixed(aValue: Fixed64): Integer; overload;
    function PBQWriteFixed(aValue: SFixed64): Integer; overload;
    function PBQWriteFixed(aValue: Fixed32): Integer; overload;
    function PBQWriteFixed(aValue: SFixed32): Integer; overload;

    function PBQWriteFieldInfo(aTag: UInt64; aWiretype: TWiretype): Integer;

    function PBQWriteField(aTag: UInt64; aValue: Boolean): Integer; overload;
    function PBQWriteField(aTag: UInt64; aValue: UInt64): Integer; overload;
    function PBQWriteField(aTag: UInt64; aValue: Int64): Integer; overload;
    function PBQWriteField(aTag: UInt64; aValue: UInt32): Integer; overload;
    function PBQWriteField(aTag: UInt64; aValue: Int32): Integer; overload;
    function PBQWriteField(aTag: UInt64; const aValue: AnsiString): Integer; overload;
    function PBQWriteField(aTag: UInt64; const aValue: string): Integer; overload;
    function PBQWriteField(aTag: UInt64; const aValue; aValueSize: UInt64): Integer; overload;
    function PBQWriteField(aTag: UInt64; aValue: Single): Integer; overload;
    function PBQWriteField(aTag: UInt64; aValue: Double): Integer; overload;

    function PBQWriteFixedField(aTag: UInt64; aValue: Fixed64): Integer; overload;
    function PBQWriteFixedField(aTag: UInt64; aValue: SFixed64): Integer; overload;
    function PBQWriteFixedField(aTag: UInt64; aValue: Fixed32): Integer; overload;
    function PBQWriteFixedField(aTag: UInt64; aValue: SFixed32): Integer; overload;

    // PB read and peek

    function PBPeekUInt64(aOffset: Integer=0): UInt64;
    function PBPeekInt64: Int64;

    function PBPeekVarIntSize: Integer;

    function PBRead(out aValue: Boolean): Integer; overload;
    function PBRead(var aValue: UInt64): Integer; overload;
    function PBRead(out aValue: Int64): Integer; overload;
    function PBRead(out aValue: UInt32): Integer; overload;
    function PBRead(out aValue: Int32): Integer; overload;
    function PBRead(out aValue: AnsiString): Integer; overload;
    function PBRead(out aValue: RawByteString): Integer; overload;
    function PBRead(out aValue: string): Integer; overload;
    //function PBRead(out aValue: TByteBuffer): Integer; overload;

    function PBReadFixed(out aValue: Fixed64): Integer; overload;
    function PBReadFixed(out aValue: SFixed64): Integer; overload;
    function PBReadFixed(out aValue: Fixed32): Integer; overload;
    function PBReadFixed(out aValue: SFixed32): Integer; overload;

    function PBReadAnsiString: AnsiString;
    function PBReadString: string;

    function PBReadFieldInfo(out aTag: UInt64; out aWireType: TWiretype): Integer;
    function PBSkip(aWireType: TWireType): Integer;

    // there are no PBReadField functions because we only now the type after reading the wire type and tag
  end;

const
  EmptyByteBuffer: PByteBuffer = nil;

// same as built in function but more compact: 32 instead of 38 chars (removed human readability 'chrome')
function GUIDToStringCompact(const aGUID: TGUID): string;

// convert between signed and unsigned varint

function VarIntToVarUInt(aValue: Int64): UInt64; inline;
function VarUIntToVarInt(aValue: UInt64): Int64; inline;


// CheckWireType, check if found/read wire type matches expected wire type by finding correct type using overloading

function CheckWireType(aWireType: TWireType; aValue: Boolean): Boolean; overload; // varint but always 1 byte
function CheckWireType(aWireType: TWireType; aValue: UInt32): Boolean; overload; // varint
function CheckWireType(aWireType: TWireType; aValue: Int32): Boolean; overload; // varint
function CheckWireType(aWireType: TWireType; aValue: UInt64): Boolean; overload; // varint
function CheckWireType(aWireType: TWireType; aValue: Int64): Boolean; overload; // varint
function CheckWireType(aWireType: TWireType; aValue: Single): Boolean; overload; // 4 byte
function CheckWireType(aWireType: TWireType; aValue: Double): Boolean; overload; // 8 byte
function CheckWireType(aWireType: TWireType; const aValue: AnsiString): Boolean; overload; // length delimited
function CheckWireType(aWireType: TWireType; const aValue: UTF8String): Boolean; overload; // length delimited
function CheckWireType(aWireType: TWireType; const aValue: string): Boolean; overload; // -> utf8 -> length delimited
function CheckWireType(aWireType: TWireType; const aValue: TGUID): Boolean; overload; // 16 bytes


// BB (write), build buffer per value to concat afterwards (seems as fast as prepare/write but uses less code lines)

function BB(aValue: Boolean): TByteBufferBuffer; overload; // varint but always 1 byte
function BB(aValue: UInt32): TByteBufferBuffer; overload; // varint
function BB(aValue: Int32): TByteBufferBuffer; overload; // varint
function BB(aValue: UInt64): TByteBufferBuffer; overload; // varint
function BB(aValue: Int64): TByteBufferBuffer; overload; // varint
function BB(aValue: Single): TByteBufferBuffer; overload; // 4 byte
function BB(aValue: Double): TByteBufferBuffer; overload; // 8 byte
function BB(const aValue: AnsiString): TByteBufferBuffer; overload; // length delimited
function BB(const aValue: UTF8String): TByteBufferBuffer; overload; // length delimited
function BB(const aValue: string): TByteBufferBuffer; overload; // -> utf8 -> length delimited
function BB(const aValue: TGUID): TByteBufferBuffer; overload; // 16 bytes
function BB(const aValue; aValueSize: Integer): TByteBufferBuffer; overload; inline;// straight (no wire type!)
function BB(aValue: Byte): TByteBufferBuffer; overload; // 1 byte (no wire type!)
function BB(aValue: Word): TByteBufferBuffer; overload; // 2 byte (no wire type!)
function BB(const aValue: TByteBufferBuffer): TByteBufferBuffer; overload; // 2 byte (no wire type!)

// BBtag (write), see BB but with tagged and wire typed field definition in front of value

function BBtag(aTag: UInt64; aValue: Boolean): TByteBufferBuffer; overload; // varint but always 1 byte
function BBtag(aTag: UInt64; aValue: UInt32): TByteBufferBuffer; overload; // varint
function BBtag(aTag: UInt64; aValue: Int32): TByteBufferBuffer; overload; // varint
function BBtag(aTag: UInt64; aValue: UInt64): TByteBufferBuffer; overload; // varint
function BBtag(aTag: UInt64; aValue: Int64): TByteBufferBuffer; overload; // varint
function BBtag(aTag: UInt64; aValue: Single): TByteBufferBuffer; overload; // 4 byte
function BBtag(aTag: UInt64; aValue: Double): TByteBufferBuffer; overload; // 8 byte
function BBtag(aTag: UInt64; const aValue: AnsiString): TByteBufferBuffer; overload; // length delimited
function BBtag(aTag: UInt64; const aValue: UTF8String): TByteBufferBuffer; overload; // length delimited
function BBtag(aTag: UInt64; const aValue: string): TByteBufferBuffer; overload; // -> utf8 -> length delimited
function BBtag(aTag: UInt64; const aValue: TGUID): TByteBufferBuffer; overload; // 16 bytes -> length delimited
function BBtag(aTag: UInt64; const aValue; aValueSize: Integer): TByteBufferBuffer; overload; // -> length delimited
function BBtag(aTag: UInt64; const aValue: TByteBufferBuffer): TByteBufferBuffer; overload; // -> length delimited

// BBread, cursor starts at 0

function BBreadAvailable(const aBuffer: TByteBufferBuffer; aCursor: Integer): Integer;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Boolean): Integer; overload; // varint but always 1 byte
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: UInt32): Integer; overload; // varint
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Int32): Integer; overload; // varint
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: UInt64): Integer; overload; // varint
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Int64): Integer; overload; // varint
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Single): Integer; overload; // 4 byte
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Double): Integer; overload; // 8 byte
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: AnsiString): Integer; overload; // length delimited
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: UTF8String): Integer; overload; // length delimited
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: string): Integer; overload; // -> utf8 -> length delimited
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: TGUID): Integer; overload; // 16 bytes
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; aValueSize: Integer; out aValue: TByteBufferBuffer): Integer; overload; inline;// straight (no wire type!)
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Byte): Integer; overload; // 1 byte (no wire type!)
function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Word): Integer; overload; // 2 byte (no wire type!)

function BBreadBoolean(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Boolean; // varint but always 1 byte
function BBreadUInt32(const aBuffer: TByteBufferBuffer; var aCursor: Integer): UInt32; // varint
function BBreadInt32(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Int32; // varint
function BBreadUInt64(const aBuffer: TByteBufferBuffer; var aCursor: Integer): UInt64; // varint
function BBreadInt64(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Int64; // varint
function BBreadSingle(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Single; // 4 byte
function BBreadDouble(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Double; // 8 byte
function BBreadAnsiString(const aBuffer: TByteBufferBuffer; var aCursor: Integer): AnsiString; // length delimited
function BBreadUTF8String(const aBuffer: TByteBufferBuffer; var aCursor: Integer): UTF8String; // length delimited
function BBreadString(const aBuffer: TByteBufferBuffer; var aCursor: Integer): string; // -> utf8 -> length delimited
function BBreadGUID(const aBuffer: TByteBufferBuffer; var aCursor: Integer): TGUID; // 16 bytes
//function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; aValueSize: Integer; out aValue: TByteBufferBuffer): Integer; inline;// straight (no wire type!)
function BBreadByte(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Byte; // 1 byte (no wire type!)
function BBreadWord(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Word; // 2 byte (no wire type!)


function BBtagRead(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aTag: UInt64; out aWiretype: TWiretype): Integer;
function BBtagSkip(const aBuffer: TByteBufferBuffer; var aCursor: Integer; aWireType: TWireType): Integer;

function BBTagAndWireTypeToKey(aTag: UInt64; aWireType: TWireType): UInt64; inline;
function BBKeyToWireType(aKey: UInt64): TWireType; inline;

implementation

function BBTagAndWireTypeToKey(aTag: UInt64; aWireType: TWireType): UInt64;
begin
  Result := (aTag shl 3) or Ord(aWireType);
end;

function BBKeyToWireType(aKey: UInt64): TWireType;
begin
  Result := TWireType(aKey and $7);
end;

{ TByteBuffer }

procedure TByteBuffer.Assign(const aBuffer: TByteBuffer);
begin
  FBuffer := aBuffer.FBuffer;
  FReadCursor := aBuffer.FReadCursor;
  FPrepareCursor := aBuffer.FPrepareCursor;
  FWriteCursor := aBuffer.FWriteCursor;
  FPBWriteCursor := aBuffer.FPBWriteCursor;
end;

procedure TByteBuffer.Clear(aSize: Integer);
begin
  System.SetLength(FBuffer, aSize);
  FReadCursor := 0;
  FPrepareCursor := 0;
  FWriteCursor := 0;
  FPBWriteCursor := 0;
end;

function TByteBuffer.Compare(const aValue; aValueSize: Integer; aOffset: Integer): Boolean;
begin
  if aOffset+aValueSize<=ReadAvailable
  then Result := CompareMem(@FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], @aValue, aValueSize)
  else Result := False;
end;

function TByteBuffer.GetAddress: Pointer;
begin
  Result := PAnsiChar(FBuffer);
end;

function TByteBuffer.GetBytes(aIndex: Integer): Byte;
begin
  Result := Byte(FBuffer[aIndex{$IFDEF OneBasedByteBuffer}+1{$ENDIF}]);
end;

function TByteBuffer.GetLength: Integer;
begin
  Result := System.Length(FBuffer);
end;

function TByteBuffer.IsEmpty: Boolean;
begin
  if @Self<>nil
  then Result := System.Length(FBuffer)=0
  else Result := True;
end;

function TByteBuffer.PBPrepare(aValue: Int64): Integer;
begin
  if aValue<0
  then Result := PBPrepare(UInt64(((-aValue) shl 1) or $00000001))
  else Result := PBPrepare(UInt64(aValue shl 1));
end;

function TByteBuffer.PBPrepare(aValue: UInt32): Integer;
begin
  Result := PBPrepare(UInt64(aValue));
end;

function TByteBuffer.PBPrepare(aValue: Boolean): Integer;
begin
  Result := 1;
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepare(aValue: UInt64): Integer;
begin
  // encode in blocks of 7 bits (high order bit of byte is signal that more bytes are to follow
  // encode lower numbers directly for speed
  if aValue<128
  then Result := 1
  else
  begin
    if aValue<16384
    then Result := 2
    else
    begin
      if aValue<2097152
      then Result := 3
      else
      begin
        // 4 bytes or more: change to dynamic size detection
        Result := 4;
        aValue := aValue shr (7*4);
        while aValue>0 do
        begin
          Result := Result+1;
          aValue := aValue shr 7;
        end;
      end;
    end;
  end;
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepare(aValue: Single): Integer;
begin
  Result := SizeOf(aValue);
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPeekInt64: Int64;
var
  ui64: UInt64;
begin
  ui64 := PBPeekUInt64;
  Result := ui64 shr 1;
  if (ui64 and 1)=1
  then Result := -Result;
end;

function TByteBuffer.PBPeekUInt64(aOffset: Integer): UInt64;
begin
  aOffset := aOffset+1;
  if Byte(FBuffer[FReadCursor+aOffset{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}])<128 then
  begin
    Result := Byte(FBuffer[FReadCursor+aOffset{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}]);
  end
  else
  begin
    Result := PBPeekUInt64(aOffset);
    Result := (Result shl 7) or (Byte(FBuffer[FReadCursor+aOffset{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}]) and $7F);
  end;
end;

function TByteBuffer.PBPeekVarIntSize: Integer;
var
  offset: Integer;
begin
  Result := 1;
  offset := 0;
  while (Byte(FBuffer[FReadCursor+offset{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}]) and $80)<>0 do
  begin
    Result := Result+1;
    offset := offset+1;
  end;
end;

function TByteBuffer.PBPrepare(aValue: Double): Integer;
begin
  Result := SizeOf(aValue);
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepare(const aValue: string): Integer;
begin
  Result := PBPrepare(AnsiString(UTF8String(aValue)));
end;

function TByteBuffer.PBPrepare(const aValue; aValueSize: UInt64): Integer;
begin
  Result := aValueSize;
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepare(aValue: Int32): Integer;
begin
  Result := PBPrepare(Int64(aValue));
end;

function TByteBuffer.PBPrepare(const aValue: AnsiString): Integer;
var
  Len: UInt64;
begin
  Len := System.Length(aValue);
  Result := PBPrepare(Len)+Len;
  FPrepareCursor := FPrepareCursor+Len; // Len it self is already added in BPPrepare(Len)
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; const aValue; aValueSize: UInt64): Integer;
begin
  Result := PBPrepare(aValue, aValueSize);
  Result := Result+PBPrepare(aValueSize);
  Result := Result+PBPrepareFieldInfo(aTag, wtLengthDelimited);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: Int64): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: UInt32): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: Boolean): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: UInt64): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: Single): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wt32Bit);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: Double): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wt64Bit);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; aValue: Int32): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; const aValue: AnsiString): Integer;
begin
  Result := PBPrepare(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wtLengthDelimited);
end;

function TByteBuffer.PBPrepareField(aTag: UInt64; const aValue: string): Integer;
begin
  Result := PBPrepareField(aTag, AnsiString(UTF8String(aValue)));
end;

function TByteBuffer.PBPrepareFieldInfo(aTag: UInt64; aWireType: TWireType): Integer;
begin
  Result := PBPrepare(UInt64((aTag shl 3) or Ord(aWireType)));
end;

function TByteBuffer.PBPrepareFixed(aValue: SFixed64): Integer;
begin
  Result := SizeOf(aValue);
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepareFixed(aValue: Fixed64): Integer;
begin
  Result := SizeOf(aValue);
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepareFixed(aValue: SFixed32): Integer;
begin
  Result := SizeOf(aValue);
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBPrepareFixedField(aTag: UInt64; aValue: SFixed64): Integer;
begin
  Result := PBPrepareFixed(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wt64Bit);
end;

function TByteBuffer.PBPrepareFixedField(aTag: UInt64; aValue: Fixed64): Integer;
begin
  Result := PBPrepareFixed(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wt64Bit);
end;

function TByteBuffer.PBPrepareFixedField(aTag: UInt64; aValue: SFixed32): Integer;
begin
  Result := PBPrepareFixed(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wt32Bit);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: Int64): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: UInt32): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: Boolean): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: UInt64): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: Int32): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wtVarInt);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: Single): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wt32Bit);
end;

function TByteBuffer.PBQWrite(aValue: UInt32): Integer;
begin
  Result := PBQWrite(UInt64(aValue));
end;

function TByteBuffer.PBQWrite(aValue: Int32): Integer;
begin
  Result := PBQWrite(Int64(aValue));
end;

function TByteBuffer.PBQWrite(const aValue: AnsiString): Integer;
begin
  Result := PBQWrite(aValue[1], System.Length(aValue));
  Result := Result+PBQWrite(UInt64(System.Length(aValue)));
end;

function TByteBuffer.PBQWrite(aValue: Boolean): Integer;
begin
  if aValue
  then Result := PBQWrite(UInt64(1))
  else Result := PBQWrite(UInt64(0));
end;

function TByteBuffer.PBQWrite(aValue: UInt64): Integer;
begin
  if aValue<128 then // no further bytes will follow
  begin
    FBuffer[FPBWriteCursor{+1-1}{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}] := AnsiChar(aValue);
    FPBWriteCursor := FPBWriteCursor-1; // we write backwards because of length determination in 1 pass
    Result := 1;
  end
  else
  begin
    Result := PBQWrite(UInt64(aValue shr 7))+1; // recursive call to get order of byte right (ls group of 7 bits first)
    FBuffer[FPBWriteCursor{+1-1}{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}] := AnsiChar((aValue and $7F) or $80); // msb: signal more bytes are to follow
    FPBWriteCursor := FPBWriteCursor-1; // we write backwards because of length determination in 1 pass
  end;
end;

function TByteBuffer.PBQWrite(aValue: Int64): Integer;
begin
  if aValue<0
  then Result := PBQWrite(UInt64(((-aValue) shl 1) or $00000001))
  else Result := PBQWrite(UInt64(aValue shl 1));
end;

function TByteBuffer.PBQWrite(const aValue; aValueSize: UInt64): Integer;
begin
  Move(aValue, FBuffer[FPBWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}-aValueSize], aValueSize);
  FPBWriteCursor := FPBWriteCursor-aValueSize;
  Result := aValueSize;
end;

function TByteBuffer.PBQWrite(aValue: Double): Integer;
begin
  Result := PBQWrite(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBQWrite(const aValue: string): Integer;
begin
  Result := PBQWrite(AnsiString(UTF8String(aValue)));
end;

function TByteBuffer.PBQWrite(aValue: Single): Integer;
begin
  Result := PBQWrite(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; aValue: Double): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wt64Bit);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; const aValue: string): Integer;
begin
  Result := PBQWriteField(aTag, AnsiString(UTF8String(aValue)));
end;

function TByteBuffer.PBQWriteFieldInfo(aTag: UInt64; aWiretype: TWiretype): Integer;
begin
  Result := PBQWrite(UInt64((aTag shl 3) or Ord(aWireType)));
end;

function TByteBuffer.PBQWriteFixed(aValue: Fixed32): Integer;
begin
  Result := PBQWrite(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBQWriteFixed(aValue: SFixed32): Integer;
begin
  Result := PBQWrite(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBQWriteFixedField(aTag: UInt64; aValue: SFixed64): Integer;
begin
  Result := PBQWriteFixed(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wt64Bit);
end;

function TByteBuffer.PBQWriteFixedField(aTag: UInt64; aValue: Fixed64): Integer;
begin
  Result := PBQWriteFixed(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wt64Bit);
end;

function TByteBuffer.PBQWriteFixedField(aTag: UInt64; aValue: SFixed32): Integer;
begin
  Result := PBQWriteFixed(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wt32Bit);
end;

function TByteBuffer.PBQWriteFixedField(aTag: UInt64; aValue: Fixed32): Integer;
begin
  Result := PBQWriteFixed(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wt32Bit);
end;

function TByteBuffer.PBQWriteFixed(aValue: Fixed64): Integer;
begin
  Result := PBQWrite(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBQWriteFixed(aValue: SFixed64): Integer;
begin
  Result := PBQWrite(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; const aValue: AnsiString): Integer;
begin
  Result := PBQWrite(aValue);
  Result := Result+PBQWriteFieldInfo(aTag, wtLengthDelimited);
end;

function TByteBuffer.PBQWriteField(aTag: UInt64; const aValue; aValueSize: UInt64): Integer;
begin
  Result := PBQWrite(aValue, aValueSize);
  Result := Result+PBQWrite(aValueSize);
  Result := Result+PBQWriteFieldInfo(aTag, wtLengthDelimited);
end;

function TByteBuffer.PBPrepareFixedField(aTag: UInt64; aValue: Fixed32): Integer;
begin
  Result := PBPrepareFixed(aValue);
  Result := Result+PBPrepareFieldInfo(aTag, wt32Bit);
end;

function TByteBuffer.PBPrepareFixed(aValue: Fixed32): Integer;
begin
  Result := SizeOf(aValue);
  FPrepareCursor := FPrepareCursor+Result;
end;

function TByteBuffer.PBRead(out aValue: UInt32): Integer;
var
  ui64: UInt64;
begin
  Result := PBRead(ui64);
  aValue := ui64;
end;

function TByteBuffer.PBRead(out aValue: Int32): Integer;
var
  i64: Int64;
begin
  Result := PBRead(i64);
  aValue := i64;
end;

function TByteBuffer.PBRead(out aValue: Int64): Integer;
var
  ui64: UInt64;
begin
  Result := PBRead(ui64);
  aValue := ui64 shr 1;
  if (ui64 and 1)=1
  then aValue := -aValue;
end;

function TByteBuffer.PBRead(var aValue: UInt64): Integer;
var
  b: Byte;
begin
  FReadCursor := FReadCursor+1;
  b := Byte(FBuffer[FReadCursor{$IFNDEF OneBasedByteBuffer}-1{$ENDIF}]);
  if b<128 then
  begin
    aValue := b;
    Result := 1;
  end
  else
  begin
    Result := PBRead(aValue)+1;
    aValue := (aValue shl 7) or (b and $7F);
  end;
end;

function TByteBuffer.PBRead(out aValue: Boolean): Integer;
var
  ui64: UInt64;
begin
  Result := PBRead(ui64);
  aValue := ui64<>0;
end;

function TByteBuffer.PBRead(out aValue: AnsiString): Integer;
var
  Len: UInt64;
begin
  Result := PBRead(Len);
  Result := Result+Len;
  System.Setlength(aValue, Len);
  if Len>0
  then Read(aValue[1], Len);
end;

function TByteBuffer.PBRead(out aValue: RawByteString): Integer;
var
  Len: UInt64;
begin
  Result := PBRead(Len);
  Result := Result+Len;
  System.Setlength(aValue, Len);
  if Len>0
  then Read(aValue[1], Len);
end;

function TByteBuffer.PBRead(out aValue: string): Integer;
var
  localValue: AnsiString;
begin
  Result := PBRead(localValue);
  aValue := string(UTF8String(localValue));
end;

function TByteBuffer.PBReadAnsiString: AnsiString;
begin
  PBRead(Result);
end;

function TByteBuffer.PBReadFieldInfo(out aTag: UInt64; out aWireType: TWiretype): Integer;
var
  ui64: UInt64;
begin
  Result := PBRead(ui64);
  aWireType := TWireType(ui64 and $7);
  aTag := ui64 shr 3; // could overflow, but tags are defined as 0 <= tag < 2^29-1 and tags 19000 though 19999 are reserved (?)
end;

function TByteBuffer.PBReadFixed(out aValue: SFixed64): Integer;
begin
  Result := SizeOf(aValue);
  Read(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBReadFixed(out aValue: Fixed64): Integer;
begin
  Result := SizeOf(aValue);
  Read(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBReadFixed(out aValue: SFixed32): Integer;
begin
  Result := SizeOf(aValue);
  Read(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBReadString: string;
begin
  Result := string(UTF8String(PBReadAnsiString));
end;

function TByteBuffer.PBReadFixed(out aValue: Fixed32): Integer;
begin
  Result := SizeOf(aValue);
  Read(aValue, SizeOf(aValue));
end;

function TByteBuffer.PBSkip(aWireType: TWireType): Integer;
var
  VarInt: UInt64;
begin
  case aWireType of
    wtVarInt: Result := PBRead(VarInt);
    wt64Bit:
      begin
        SkipReading(8);
        Result := 8;
      end;
    wtLengthDelimited:
      begin
        Result := PBRead(VarInt);
        Result := Result+VarInt;
        SkipReading(VarInt);
      end;
    wtStartGroup:
      raise Exception.Create('"Start group" wire type is not supported');
    wtEndGroup:
      raise Exception.Create('"End group" wire type is not supported');
    wt32Bit:
      begin
        SkipReading(4);
        Result := 4;
      end;
  else
    Result := 0;
  end;
end;

function TByteBuffer.Peek(out aValue: Integer; aOffset: Integer): Boolean;
begin
  if aOffset+SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, SizeOf(aValue));
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Peek(out aValue: Int64; aOffset: Integer): Boolean;
begin
  if aOffset+SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, SizeOf(aValue));
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Peek(out aValue: Boolean; aOffset: Integer): Boolean;
begin
  if aOffset+SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, SizeOf(aValue));
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Peek(out aValue: Byte; aOffset: Integer): Boolean;
begin
  if aOffset+SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, SizeOf(aValue));
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Peek(out aValue: AnsiString; aOffset: Integer): Boolean;
var
  Len: Integer;
begin
  if Peek(Len, aOffset) then
  begin
    System.Setlength(aValue, Len);
    Result := Peek(aValue[1], Len, aOffset+SizeOf(Len));
  end
  else Result := False;
end;

function TByteBuffer.Peek(var aValue; aValueSize, aOffset: Integer): Boolean;
begin
  if aOffset+aValueSize<=ReadAvailable then
  begin
    if aValueSize>0
    then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, aValueSize);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Peek(out aValue: Single; aOffset: Integer): Boolean;
begin
  if aOffset+SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, SizeOf(aValue));
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Peek(out aValue: Double; aOffset: Integer): Boolean;
begin
  if aOffset+SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], aValue, SizeOf(aValue));
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.PeekAnsiString(aOffset: Integer; const aDefaultValue: AnsiString): AnsiString;
var
  Len: Integer;
begin
  if Peek(Len, aOffset) then
  begin
    System.Setlength(Result, Len);
    if not Peek(Result[1], Len, aOffset+SizeOf(Len))
    then Result := aDefaultValue;
  end
  else Result := aDefaultValue;
end;

function TByteBuffer.PeekBoolean(aOffset: Integer; aDefaultValue: Boolean): Boolean;
begin
  if aOffset+SizeOf(Result)<=ReadAvailable
  then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], Result, SizeOf(Result))
  else Result := aDefaultValue;
end;

function TByteBuffer.PeekByte(aOffset: Integer; aDefaultValue: Byte): Byte;
begin
  if aOffset+SizeOf(Result)<=ReadAvailable
  then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], Result, SizeOf(Result))
  else Result := aDefaultValue;
end;

function TByteBuffer.PeekDouble(aOffset: Integer; const aDefaultValue: Double): Double;
begin
  if aOffset+SizeOf(Result)<=ReadAvailable
  then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], Result, SizeOf(Result))
  else Result := aDefaultValue;
end;

function TByteBuffer.PeekInt64(aOffset: Integer; const aDefaultValue: Int64): Int64;
begin
  if aOffset+SizeOf(Result)<=ReadAvailable
  then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], Result, SizeOf(Result))
  else Result := aDefaultValue;
end;

function TByteBuffer.PeekInteger(aOffset: Integer; aDefaultValue: Integer): Integer;
begin
  if aOffset+SizeOf(Result)<=ReadAvailable
  then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], Result, SizeOf(Result))
  else Result := aDefaultValue;
end;

function TByteBuffer.PeekSingle(aOffset: Integer; const aDefaultValue: Single): Single;
begin
  if aOffset+SizeOf(Result)<=ReadAvailable
  then Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}+aOffset], Result, SizeOf(Result))
  else Result := aDefaultValue;
end;

procedure TByteBuffer.Prepare(const aValue: Double);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Prepare(const aValue: Single);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Prepare(const aValue: TByteBuffer);
begin
  FPrepareCursor := FPrepareCursor+aValue.Length;
end;

procedure TByteBuffer.Prepare(const aValue: AnsiString);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(Integer)+System.Length(aValue);
end;

procedure TByteBuffer.Prepare(const aValue: Byte);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Prepare(const aValue: Boolean);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Prepare(const aValue: Int64);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Prepare(const aValue: Integer);
begin
  FPrepareCursor := FPrepareCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Prepare(const aValue; aValueSize: Integer);
begin
  FPrepareCursor := FPrepareCursor+aValueSize;
end;

procedure TByteBuffer.Prepare(const aValue: string);
begin
  Prepare(AnsiString(UTF8String(aValue)));
end;

procedure TByteBuffer.PrepareApply;
begin
  if Length<FPrepareCursor
  then Length := FPrepareCursor;
  FPBWriteCursor := FPrepareCursor;
end;

procedure TByteBuffer.PrepareApplyAndTrim;
begin
  if Length<>FPrepareCursor
  then Length := FPrepareCursor;
  FPBWriteCursor := FPrepareCursor;
end;

function TByteBuffer.PrepareSize(aValueSize: Integer): Integer;
begin
  Result := FPrepareCursor;
  FPrepareCursor := FPrepareCursor+aValueSize;
end;

function TByteBuffer.PrepareStart: Integer;
begin
  FPrepareCursor := FWriteCursor;
  Result := FPrepareCursor;
end;

procedure TByteBuffer.QRead(out aValue: Double);
begin
  Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  FReadCursor := FReadCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QRead(out aValue: Single);
begin
  Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  FReadCursor := FReadCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QRead(out aValue: AnsiString);
var
  Len: Integer;
begin
  QRead(Len);
  System.SetLength(aValue, Len);
  if Len>0 then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue[1], Len);
    FReadCursor := FReadCursor+Len;
  end;
end;

procedure TByteBuffer.QRead(out aValue: Boolean);
begin
  Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  FReadCursor := FReadCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QRead(out aValue: Byte);
begin
  Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  FReadCursor := FReadCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QRead(out aValue: Integer);
begin
  Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  FReadCursor := FReadCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QRead(out aValue: Int64);
begin
  Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  FReadCursor := FReadCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QRead(var aValue; aValueSize: Integer);
begin
  if aValueSize>0 then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, aValueSize);
    FReadCursor := FReadCursor+aValueSize;
  end;
end;

procedure TByteBuffer.QWrite(const aValue: Boolean);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QWrite(const aValue: Byte);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QWrite(const aValue: Double);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QWrite(const aValue: TByteBuffer);
begin
  if aValue.Length>0 then
  begin
    Move(aValue.Address^, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue.Length);
    FWriteCursor := FWriteCursor+aValue.Length;
  end;
end;

procedure TByteBuffer.QWrite(const aValue: AnsiString);
var
  Len: Integer;
begin
  Len := System.Length(aValue);
  Move(Len, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(Len));
  FWriteCursor := FWriteCursor+SizeOf(Len);
  if Len>0 then
  begin
    Move(aValue[1], FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Len);
    FWriteCursor := FWriteCursor+Len;
  end;
end;

procedure TByteBuffer.QWrite(const aValue: Integer);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QWrite(const aValue: Int64);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QWrite(const aValue: Single);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.QWrite(const aValue; aValueSize: Integer);
begin
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValueSize);
  FWriteCursor := FWriteCursor+aValueSize;
end;

procedure TByteBuffer.QWrite(const aValue: string);
begin
  QWrite(AnsiString(UTF8String(aValue)));
end;

function TByteBuffer.Read(out aValue: Integer): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: Int64): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: Boolean): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: Byte): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: AnsiString): Boolean;
var
  Len: Integer;
begin
  if Read(Len) then
  begin
    System.Setlength(aValue, Len);
    if Len>0
    then Result := Read(aValue[1], Len)
    else Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(var aValue; aValueSize: Integer): Boolean;
begin
  if aValueSize<=ReadAvailable then
  begin
    if aValueSize>0 then
    begin
      Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, aValueSize);
      FReadCursor := FReadCursor+aValueSize;
    end;
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: TDateTime): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: string): Boolean;
var
  s: AnsiString;
begin
  Result := Read(s);
  aValue := string(UTF8String(s));
end;

function TByteBuffer.Read(out aValue: Single): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.Read(out aValue: Double): Boolean;
begin
  if SizeOf(aValue)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
    FReadCursor := FReadCursor+SizeOf(aValue);
    Result := True;
  end
  else Result := False;
end;

function TByteBuffer.ReadAddr: Pointer;
begin
  if ReadAvailable>0
  then Result := @FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}]
  else Result := nil;
end;

function TByteBuffer.ReadAnsiString(const aDefaultValue: AnsiString): AnsiString;
var
  Len: Integer;
begin
  if Read(Len) then
  begin
    System.SetLength(Result, Len);
    if Len>0 then
    begin
      if not Read(Result[1], Len)
      then Result := aDefaultValue;
    end;
  end
  else Result := aDefaultValue;
end;

function TByteBuffer.ReadAvailable: Integer;
begin
  Result := Length-FReadCursor;
end;

function TByteBuffer.ReadBoolean(aDefaultValue: Boolean): Boolean;
begin
  if SizeOf(Result)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
    FReadCursor := FReadCursor+SizeOf(Result);
  end
  else Result := aDefaultValue;
end;

function TByteBuffer.ReadByte(aDefaultValue: Byte): Byte;
begin
  if SizeOf(Result)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
    FReadCursor := FReadCursor+SizeOf(Result);
  end
  else Result := aDefaultValue;
end;

function TByteBuffer.ReadDouble(const aDefaultValue: Double): Double;
begin
  if SizeOf(Result)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
    FReadCursor := FReadCursor+SizeOf(Result);
  end
  else Result := aDefaultValue;
end;

function TByteBuffer.ReadInt64(const aDefaultValue: Int64): Int64;
begin
  if SizeOf(Result)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
    FReadCursor := FReadCursor+SizeOf(Result);
  end
  else Result := aDefaultValue;
end;

function TByteBuffer.ReadInteger(aDefaultValue: Integer): Integer;
begin
  if SizeOf(Result)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
    FReadCursor := FReadCursor+SizeOf(Result);
  end
  else Result := aDefaultValue;
end;

procedure TByteBuffer.ReadRest(var aValue: TByteBuffer);
begin
  if ReadAvailable>0 then
  begin
    aValue.PrepareStart;
    aValue.PrepareSize(ReadAvailable);
    aValue.PrepareApply;
    aValue.QWrite(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], ReadAvailable);
  end;
end;

function TByteBuffer.ReadRest: AnsiString;
begin
  System.SetLength(Result, ReadAvailable);
  if System.Length(Result)>0
  then QRead(Result[1], System.Length(Result));
end;

function TByteBuffer.ReadSingle(const aDefaultValue: Single): Single;
begin
  if SizeOf(Result)<=ReadAvailable then
  begin
    Move(FBuffer[FReadCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
    FReadCursor := FReadCursor+SizeOf(Result);
  end
  else Result := aDefaultValue;
end;

procedure TByteBuffer.ReadStart(aIndex: Integer);
begin
  FReadCursor := aIndex;
end;

procedure TByteBuffer.SetBuffer(const aValue: TByteBufferBuffer);
begin
  FBuffer := aValue;
  FReadCursor := 0;
  FPrepareCursor := 0;
  FWriteCursor := 0;
  FPBWriteCursor := 0;
end;

procedure TByteBuffer.SetBytes(aIndex: Integer; aValue: Byte);
begin
  FBuffer[aIndex{$IFDEF OneBasedByteBuffer}+1{$ENDIF}] := AnsiChar(aValue);
end;

procedure TByteBuffer.SetLength(const aValue: Integer);
begin
  System.Setlength(FBuffer, aValue);
end;

procedure TByteBuffer.SkipReading(aValueSize: Integer);
begin
  FReadCursor := FReadCursor+aValueSize;
end;

procedure TByteBuffer.Write(const aValue: Integer);
begin
  if SizeOf(aValue)>WriteAvailable
  then Length := FWriteCursor+SizeOf(aValue);
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Write(const aValue: Int64);
begin
  if SizeOf(aValue)>WriteAvailable
  then Length := FWriteCursor+SizeOf(aValue);
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Write(const aValue: Boolean);
begin
  if SizeOf(aValue)>WriteAvailable
  then Length := FWriteCursor+SizeOf(aValue);
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Write(const aValue: Byte);
begin
  if SizeOf(aValue)>WriteAvailable
  then Length := FWriteCursor+SizeOf(aValue);
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Write(const aValue: Single);
begin
  if SizeOf(aValue)>WriteAvailable
  then Length := FWriteCursor+SizeOf(aValue);
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Write(const aValue: TByteBuffer);
begin
  if aValue.Length>WriteAvailable
  then Length := FWriteCursor+aValue.Length;
  if aValue.Length>0 then
  begin
    Move(aValue.Address^, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue.Length);
    FWriteCursor := FWriteCursor+aValue.Length;
  end;
end;

procedure TByteBuffer.Write(const aValue; aValueSize: Integer);
begin
  if aValueSize>WriteAvailable
  then Length := FWriteCursor+aValueSize;
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValueSize);
  FWriteCursor := FWriteCursor+aValueSize;
end;

procedure TByteBuffer.Write(const aValue: string);
begin
  Write(AnsiString(UTF8String(aValue)));
end;

procedure TByteBuffer.Write(const aValue: Double);
begin
  if SizeOf(aValue)>WriteAvailable
  then Length := FWriteCursor+SizeOf(aValue);
  Move(aValue, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(aValue));
  FWriteCursor := FWriteCursor+SizeOf(aValue);
end;

procedure TByteBuffer.Write(const aValue: AnsiString);
var
  Len: Integer;
begin
  Len := System.Length(aValue);
  if SizeOf(Len)+Len>WriteAvailable
  then Length := FWriteCursor+SizeOf(Len)+Len;
  Move(Len, FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], SizeOf(Len));
  FWriteCursor := FWriteCursor+SizeOf(Len);
  if Len>0 then
  begin
    Move(aValue[1], FBuffer[FWriteCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Len);
    FWriteCursor := FWriteCursor+Len;
  end;
end;

procedure TByteBuffer.WriteApply;
begin
  if FWriteCursor<>Length
  then Length := FWriteCursor;
end;

function TByteBuffer.WriteAvailable: Integer;
begin
  Result := Length-FWriteCursor;
end;

procedure TByteBuffer.WriteStart(aIndex: Integer);
begin
  FWriteCursor := aIndex;
end;

function TByteBuffer.Written(aValueSize: Integer): Boolean;
begin
  FWriteCursor := FWriteCursor+aValueSize;
  Result := WriteAvailable >=0;
end;

{ utils }

function GUIDToStringCompact(const aGUID: TGUID): string;
begin
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
    [aGuid.D1, aGuid.D2, aGuid.D3, aGuid.D4[0], aGuid.D4[1], aGuid.D4[2], aGuid.D4[3],
    aGuid.D4[4], aGuid.D4[5], aGuid.D4[6], aGuid.D4[7]]);
end;

function VarIntToVarUInt(aValue: Int64): UInt64;
begin
  if aValue<0
  then Result := ((-aValue) shl 1) or 1
  else Result := aValue shl 1;
end;

function VarUIntToVarInt(aValue: UInt64): Int64;
begin
  Result := aValue shr 1;
  if (aValue and 1)=1
  then Result := -Result;
end;

// VarInt

function VarIntLength(aValue: UInt64): Integer; overload; inline;
begin
  // encode in blocks of 7 bits (high order bit of byte is signal that more bytes are to follow
  // encode lower numbers directly for speed
  if aValue<128
  then Result := 1
  else
  begin
    if aValue<128*128
    then Result := 2
    else
    begin
      if aValue<128*128*128
      then Result := 3
      else
      begin
        // 4 bytes or more: change to dynamic size detection
        Result := 4;
        aValue := aValue shr (7*4);
        while aValue>0 do
        begin
          Inc(Result, 1);
          aValue := aValue shr 7;
        end;
      end;
    end;
  end;
end;

function VarIntLength(aValue: UInt32): Integer; overload; inline;
begin
  // encode in blocks of 7 bits (high order bit of byte is signal that more bytes are to follow
  // encode lower numbers directly for speed
  if aValue<128
  then Result := 1
  else
  begin
    if aValue<128*128
    then Result := 2
    else
    begin
      if aValue<128*128*128
      then Result := 3
      else
      begin
        if aValue<128*128*128*128
        then Result := 4
        else Result := 5; // uint32 always <= 5 bytes
      end;
    end;
  end;
end;

procedure VarIntWrite(var aBuffer: TByteBufferBuffer; var aWriteCursor: Integer; aValue: UInt64); inline;
begin
  while aValue>=128 do
  begin
    aBuffer[aWriteCursor] := TByteBufferElement((aValue and $7F) or $80); // msb: signal more bytes are to follow
    Inc(aWriteCursor, 1);
    aValue := aValue shr 7;
  end;
  // aValue<128 (msb already 0)
  aBuffer[aWriteCursor] := TByteBufferElement(aValue);
  Inc(aWriteCursor, 1);
end;

// CheckWireType

function CheckWireType(aWireType: TWireType; aValue: Boolean): Boolean;
begin
  Result := aWireType=wtVarInt;
end;

function CheckWireType(aWireType: TWireType; aValue: UInt32): Boolean;
begin
  Result := aWireType=wtVarInt;
end;

function CheckWireType(aWireType: TWireType; aValue: Int32): Boolean;
begin
  Result := aWireType=wtVarInt;
end;

function CheckWireType(aWireType: TWireType; aValue: UInt64): Boolean;
begin
  Result := aWireType=wtVarInt;
end;

function CheckWireType(aWireType: TWireType; aValue: Int64): Boolean;
begin
  Result := aWireType=wtVarInt;
end;

function CheckWireType(aWireType: TWireType; aValue: Single): Boolean;
begin
  Result := aWireType=wt32Bit;
end;

function CheckWireType(aWireType: TWireType; aValue: Double): Boolean;
begin
  Result := aWireType=wt64Bit;
end;

function CheckWireType(aWireType: TWireType; const aValue: AnsiString): Boolean;
begin
  Result := aWireType=wtLengthDelimited;
end;

function CheckWireType(aWireType: TWireType; const aValue: UTF8String): Boolean;
begin
  Result := aWireType=wtLengthDelimited;
end;

function CheckWireType(aWireType: TWireType; const aValue: string): Boolean;
begin
  Result := aWireType=wtLengthDelimited;
end;

function CheckWireType(aWireType: TWireType; const aValue: TGUID): Boolean;
begin
  Result := aWireType=wtLengthDelimited;
end;

// BB

function BB(aValue: Boolean): TByteBufferBuffer;
begin
  SetLength(Result, SizeOf(aValue));
  Move(aValue, Result, SizeOf(aValue));
end;

function BB(aValue: UInt32): TByteBufferBuffer;
var
  fWriteCursor: Integer;
begin
  SetLength(Result, VarIntLength(aValue));
  fWriteCursor := {$IFDEF OneBasedByteBuffer}1{$ELSE}0{$ENDIF};
  while aValue>=128 do
  begin
    Result[fWriteCursor] := TByteBufferElement((aValue and $7F) or $80); // msb: signal more bytes are to follow
    Inc(fWriteCursor);
    aValue := aValue shr 7;
  end;
  // aValue<128 (msb already 0)
  Result[fWriteCursor] := TByteBufferElement(aValue);
end;

function BB(aValue: Int32): TByteBufferBuffer;
begin
  if aValue<0
  then Result := BB(UInt32(((-aValue) shl 1) or 1))
  else Result := BB(UInt32(aValue shl 1));
end;

function BB(aValue: UInt64): TByteBufferBuffer;
var
  fWriteCursor: Integer;
begin
  SetLength(Result, VarIntLength(aValue));
  fWriteCursor := {$IFDEF OneBasedByteBuffer}1{$ELSE}0{$ENDIF};
  while aValue>=128 do
  begin
    Result[fWriteCursor] := TByteBufferElement((aValue and $7F) or $80); // msb: signal more bytes are to follow
    Inc(fWriteCursor);
    aValue := aValue shr 7;
  end;
  // aValue<128 (msb already 0)
  Result[fWriteCursor] := TByteBufferElement(aValue);
end;

function BB(aValue: Int64): TByteBufferBuffer;
begin
  if aValue<0
  then Result := BB(UInt64(((-aValue) shl 1) or 1))
  else Result := BB(UInt64(aValue shl 1));
end;

function BB(aValue: Single): TByteBufferBuffer;
begin
  SetLength(Result, SizeOf(aValue));
  Move(aValue, Result, SizeOf(aValue));
end;

function BB(aValue: Double): TByteBufferBuffer;
begin
  SetLength(Result, SizeOf(aValue));
  Move(aValue, Result, SizeOf(aValue));
end;

function BB(const aValue: AnsiString): TByteBufferBuffer;
var
  len: UInt64;
  fWriteCursor: Integer;
begin
  len := Length(aValue);
  SetLength(Result, VarIntLength(len)+len);
  fWriteCursor := {$IFDEF OneBasedByteBuffer}1{$ELSE}0{$ENDIF};
  VarIntWrite(Result, fWriteCursor, len);
  Move(aValue[1], Result[fWriteCursor], len);
end;

function BB(const aValue: UTF8String): TByteBufferBuffer;
var
  len: UInt64;
  fWriteCursor: Integer;
begin
  len := Length(aValue);
  SetLength(Result, VarIntLength(len)+len);
  fWriteCursor := {$IFDEF OneBasedByteBuffer}1{$ELSE}0{$ENDIF};
  VarIntWrite(Result, fWriteCursor, len);
  Move(aValue[1], Result[fWriteCursor], len);
end;

function BB(const aValue: string): TByteBufferBuffer;
begin
  Result := BB(UTF8String(aValue));
end;

function BB(const aValue: TGUID): TByteBufferBuffer;
var
  len: UInt64;
  fWriteCursor: Integer;
begin
  len := SizeOf(aValue);
  SetLength(Result, VarIntLength(len)+len);
  fWriteCursor := {$IFDEF OneBasedByteBuffer}1{$ELSE}0{$ENDIF};
  VarIntWrite(Result, fWriteCursor, len);
  Move(aValue, Result[fWriteCursor], len);
end;

function BB(const aValue; aValueSize: Integer): TByteBufferBuffer;
begin
  SetLength(Result, aValueSize);
  Move(aValue, Result, aValueSize);
end;

function BB(aValue: Byte): TByteBufferBuffer;
begin
  SetLength(Result, SizeOf(aValue));
  Move(aValue, Result, SizeOf(aValue));
end;

function BB(aValue: Word): TByteBufferBuffer;
begin
  SetLength(Result, SizeOf(aValue));
  Move(aValue, Result, SizeOf(aValue));
end;

function BB(const aValue: TByteBufferBuffer): TByteBufferBuffer;
var
  len: UInt64;
begin
  len := Length(aValue);
  Result := BB(len)+aValue;
end;

// BBtag

function BBtag(aTag: UInt64; aValue: Boolean): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtVarInt)))+BB(aValue);
end;

function BBtag(aTag: UInt64; aValue: UInt32): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtVarInt)))+BB(aValue);
end;

function BBtag(aTag: UInt64; aValue: Int32): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtVarInt)))+BB(aValue);
end;

function BBtag(aTag: UInt64; aValue: UInt64): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtVarInt)))+BB(aValue);
end;

function BBtag(aTag: UInt64; aValue: Int64): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtVarInt)))+BB(aValue);
end;

function BBtag(aTag: UInt64; aValue: Single): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wt32Bit)))+BB(aValue);
end;

function BBtag(aTag: UInt64; aValue: Double): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wt64Bit)))+BB(aValue);
end;

function BBtag(aTag: UInt64; const aValue: AnsiString): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtLengthDelimited)))+BB(aValue);
end;

function BBtag(aTag: UInt64; const aValue: UTF8String): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtLengthDelimited)))+BB(aValue);
end;

function BBtag(aTag: UInt64; const aValue: string): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtLengthDelimited)))+BB(aValue);
end;

function BBtag(aTag: UInt64; const aValue: TGUID): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtLengthDelimited)))+BB(aValue);
end;

function BBtag(aTag: UInt64; const aValue; aValueSize: Integer): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtLengthDelimited)))+BB(aValue, aValueSize);
end;

function BBtag(aTag: UInt64; const aValue: TByteBufferBuffer): TByteBufferBuffer;
begin
  Result := BB(UInt64((aTag shl 3) or Ord(wtLengthDelimited)))+BB(UInt64(Length(aValue)))+aValue;
end;

// BBread

function BBreadAvailable(const aBuffer: TByteBufferBuffer; aCursor: Integer): Integer;
begin
  Result := Length(aBuffer)-aCursor;
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Boolean): Integer;
begin // varint of size 1
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  Result := SizeOf(aValue);
  Inc(aCursor, Result);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: UInt32): Integer;
var
  b: byte;
begin
  b := Byte(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}]);
  Inc(aCursor);
  if b<128 then
  begin
    aValue := b;
    Result := 1;
  end
  else
  begin
    Result := BBread(aBuffer, aCursor, aValue)+1;
    aValue := (aValue shl 7) or (b and $7F);
  end;
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Int32): Integer;
var
  ui32: UInt32;
begin
  Result := BBread(aBuffer, aCursor, ui32);
  aValue := ui32 shr 1;
  if (ui32 and 1)=1
  then aValue := -aValue;
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: UInt64): Integer;
var
  b: byte;
begin
  b := Byte(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}]);
  Inc(aCursor);
  if b<128 then
  begin
    aValue := b;
    Result := 1;
  end
  else
  begin
    Result := BBread(aBuffer, aCursor, aValue)+1;
    aValue := (aValue shl 7) or (b and $7F);
  end;
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Int64): Integer;
var
  ui64: UInt64;
begin
  Result := BBread(aBuffer, aCursor, ui64);
  aValue := ui64 shr 1;
  if (ui64 and 1)=1
  then aValue := -aValue;
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Single): Integer;
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  Result := SizeOf(aValue);
  Inc(aCursor, Result);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Double): Integer;
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  Result := SizeOf(aValue);
  Inc(aCursor, Result);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: AnsiString): Integer;
var
  len: UInt64;
begin
  Result := BBread(aBuffer, aCursor, len);
  Result := Result+len;
  System.Setlength(aValue, len);
  if len>0
  then Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue[1], len);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: UTF8String): Integer;
var
  len: UInt64;
begin
  Result := BBread(aBuffer, aCursor, len);
  Result := Result+len;
  System.Setlength(aValue, len);
  if len>0
  then Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue[1], len);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: string): Integer;
var
  locValue: UTF8String;
begin
  Result := BBread(aBuffer, aCursor, locValue);
  aValue := string(locValue);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: TGUID): Integer;
var
  len: UInt64;
begin
  Result := BBread(aBuffer, aCursor, len);
  Result := Result+len;
  if len>SizeOf(aValue)
  then raise Exception.Create('BBread TGUID; lenght '+IntToStr(len)+'>'+IntToStr(SizeOf(aValue)))
  else if len<SizeOf(aValue)
  then FillChar(aValue, SizeOf(aValue), 0);
  if len>0 then
  begin
    Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, len);
    Inc(aCursor, len);
  end;
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; aValueSize: Integer; out aValue: TByteBufferBuffer): Integer;
begin
  System.SetLength(aValue, aValueSize);
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue[0{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValueSize);
  Result := aValueSize;
  Inc(aCursor, Result);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Byte): Integer;
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  Result := SizeOf(aValue);
  Inc(aCursor, Result);
end;

function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aValue: Word): Integer;
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], aValue, SizeOf(aValue));
  Result := SizeOf(aValue);
  Inc(aCursor, Result);
end;

function BBreadBoolean(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Boolean;
begin // varint of size 1
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
  Inc(aCursor, SizeOf(Result));
end;

function BBreadUInt32(const aBuffer: TByteBufferBuffer; var aCursor: Integer): UInt32; // varint
var
  b: byte;
begin
  b := Byte(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}]);
  Inc(aCursor);
  if b<128 then
  begin
    Result := b;
  end
  else
  begin
    Result := BBreadUInt32(aBuffer, aCursor);
    Result := (Result shl 7) or (b and $7F);
  end;
end;

function BBreadInt32(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Int32; // varint
var
  ui32: UInt32;
begin
  ui32 := BBreadUInt32(aBuffer, aCursor);
  Result := ui32 shr 1;
  if (ui32 and 1)=1
  then Result := -Result;
end;

function BBreadUInt64(const aBuffer: TByteBufferBuffer; var aCursor: Integer): UInt64; // varint
var
  b: byte;
begin
  b := Byte(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}]);
  Inc(aCursor);
  if b<128 then
  begin
    Result := b;
  end
  else
  begin
    Result := BBreadUInt64(aBuffer, aCursor);
    Result := (Result shl 7) or (b and $7F);
  end;
end;

function BBreadInt64(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Int64; // varint
var
  ui64: UInt64;
begin
  ui64 := BBreadUInt64(aBuffer, aCursor);
  Result := ui64 shr 1;
  if (ui64 and 1)=1
  then Result := -Result;
end;

function BBreadSingle(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Single; // 4 byte
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
  Inc(aCursor, SizeOf(Result));
end;

function BBreadDouble(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Double; // 8 byte
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
  Inc(aCursor, SizeOf(Result));
end;

function BBreadAnsiString(const aBuffer: TByteBufferBuffer; var aCursor: Integer): AnsiString; // length delimited
var
  len: UInt64;
begin
  len := BBreadUInt64(aBuffer, aCursor);
  System.Setlength(Result, len);
  if len>0
  then Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result[1], len);
end;

function BBreadUTF8String(const aBuffer: TByteBufferBuffer; var aCursor: Integer): UTF8String; // length delimited
var
  len: UInt64;
begin
  len := BBreadUInt64(aBuffer, aCursor);
  System.Setlength(Result, len);
  if len>0
  then Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result[1], len);
end;

function BBreadString(const aBuffer: TByteBufferBuffer; var aCursor: Integer): string; // -> utf8 -> length delimited
var
  locValue: UTF8String;
begin
  locValue := BBreadUTF8String(aBuffer, aCursor);
  Result := string(locValue);
end;

function BBreadGUID(const aBuffer: TByteBufferBuffer; var aCursor: Integer): TGUID; // 16 bytes
var
  len: UInt64;
begin
  len := BBreadUInt64(aBuffer, aCursor);
  if len>SizeOf(Result)
  then raise Exception.Create('BBreadGUID TGUID; lenght '+IntToStr(len)+'>'+IntToStr(SizeOf(Result)))
  else if len<SizeOf(Result)
  then FillChar(Result, SizeOf(Result), 0);
  if len>0 then
  begin
    Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, len);
    Inc(aCursor, len);
  end;
end;

//function BBread(const aBuffer: TByteBufferBuffer; var aCursor: Integer; aValueSize: Integer; out aValue: TByteBufferBuffer): Integer; inline;// straight (no wire type!)
function BBreadByte(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Byte; // 1 byte (no wire type!)
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
  Inc(aCursor, SizeOf(Result));
end;

function BBreadWord(const aBuffer: TByteBufferBuffer; var aCursor: Integer): Word; // 2 byte (no wire type!)
begin
  Move(aBuffer[aCursor{$IFDEF OneBasedByteBuffer}+1{$ENDIF}], Result, SizeOf(Result));
  Inc(aCursor, SizeOf(Result));
end;

function BBtagRead(const aBuffer: TByteBufferBuffer; var aCursor: Integer; out aTag: UInt64; out aWiretype: TWiretype): Integer;
var
  ui64: UInt64;
begin
  Result := BBread(aBuffer, aCursor, ui64);
  aWireType := TWireType(ui64 and $7);
  aTag := ui64 shr 3; // could overflow, but tags are defined as 0 <= tag < 2^29-1 and tags 19000 though 19999 are reserved (?)
end;

function BBtagSkip(const aBuffer: TByteBufferBuffer; var aCursor: Integer; aWireType: TWireType): Integer;
var
  varInt: UInt64;
begin
  case aWireType of
    wtVarInt:
      Result := BBread(aBuffer, aCursor, varInt);
    wt64Bit:
      begin
        Result := 8;
        Inc(aCursor, Result);
      end;
    wtLengthDelimited:
      begin
        Result := BBread(aBuffer, aCursor, varInt);
        Inc(Result, varInt);
        Inc(aCursor, varInt);
      end;
    wtStartGroup:
      raise Exception.Create('"Start group" wire type is not supported');
    wtEndGroup:
      raise Exception.Create('"End group" wire type is not supported');
    wt32Bit:
      begin
        Result := 4;
        Inc(aCursor, Result);
      end;
  else
    Result := 0;
  end;
end;

end.
