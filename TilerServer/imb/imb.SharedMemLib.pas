unit imb.SharedMemLib;

interface

uses
  Windows, SysUtils;

const
  smServerChannelName = 'imbConnect';

  smfsConnected = 1;
  smfDefaultServerReadChannel = 0;
  smfDefaultServerWriteChannel = 1;
  smfDefaultClientReadChannel = smfDefaultServerWriteChannel;
  smfDefaultClientWriteChannel = smfDefaultServerReadChannel;

  smChannelReadTimeOut = 1000;
  smChannelWriteTimeOut = 10000;

  smChannelError = -1;


type
  // overlay on memory mapped file @ start of every channel
  PSMChannelHeader = ^TSMChannelHeader;
  TSMChannelHeader = record
  function Create(aDataSize: Integer): Boolean;
  procedure Close(aSignalClose: Boolean);
  private
    fDataSize: Integer;
    fWrite: Integer;
    fRead: Integer;
    fAvailableForRead: Integer;
    fLock: Integer;
    function bufferPtr: PByte; inline;
    function readPtr: PByte; inline;
    function writePtr: PByte; inline;
    function getChannelSize: Integer;
    function getNextChannelHeader: PSMChannelHeader;
    function getAvailableForWrite: Integer; inline;
  public
    // properties of channel
    property ChannelSize: Integer read getChannelSize;
    property DataSize: Integer read fDataSize;
    // data
    function Read(var aBuffer: PByte; var aBufferSize: Integer): Boolean;
    function Write(var aBuffer: PByte; var aBufferSize: Integer): Boolean;
    // locking
    procedure Acquire; inline;
    procedure Release; inline;
    function Connected: Boolean; inline;
  end;

  PSMChannel = ^TSMChannel;
  TSMChannel = record
  procedure Create(const aName: string; var aHeader: PSMChannelHeader; aDataSize: Integer);
  procedure Open(const aName: string; var aHeader: PSMChannelHeader);
  procedure Close(aSignalClose: Boolean);
  private
    fName: string;
    fRoom: THandle;
    fFilled: THandle;
    fHeader: PSMChannelHeader;
  public
    property Name: string read fName;
    property Header: PSMChannelHeader read fHeader;
    property Room: THandle read fRoom;
    property Filled: THandle read fFilled;

    function Read(var aBuffer; aBufferSize: Integer): Integer;
    function Write(var aBuffer; aBufferSize: Integer): Integer;
  end;

  // overlay on memory mapped file @ start of file
  PSMFileHeader = ^TSMFileHeader;
  TSMFileHeader = record
  function Create(aSize: Integer; aChannelCount: Integer): Boolean;
  private
    fChannelCount: Integer;
    fClientCount: Integer;
    fStatus: Integer;
    function getFirstChannelHeader: PSMChannelHeader;
  public
    property ChannelCount: Integer read fChannelCount;
  end;

  TSMFile = record
  function Create(const aConnectionName: string; aChannelCount, aChannelDataSize: Integer): Boolean;
  function Open(const aConnectionName: string): Boolean;
  function Close(aSignalClose: Boolean): Boolean;
    // operators and not assigned helpers
    class function Empty: TSMFile; static;
    class operator Equal(const Left, Right: TSMFile): Boolean;
    class operator NotEqual(const Left, Right: TSMFile): Boolean;
  private
    fConnectionName: string;
    fFileMapping: THandle;
    fFileHeader: PSMFileHeader;
    fChannels: array of TSMChannel;
    fIsClient: Boolean;
    function getChannelCount: Integer;
    function getChannel(aIndex: Integer): PSMChannel;
    function getConnected: Boolean;
  public
    property ConnectionName: string read fConnectionName;
    property ChannelCount: Integer read getChannelCount;
    property Channel[aIndex: Integer]: PSMChannel read getChannel;
    property Connected: Boolean read getConnected;
  end;

  TOnNewConnection = procedure(aConnection: TSMFile);

function SharedMemServerAvailable(const aServerChannelName: string): Boolean;
function NextSharedMemConnectionGUID(const aServerChannelName: string): TGUID;
function ConnectToSharedMemConnection(const aServerChannelName: string): TSMFile;

implementation


{ Utils }

function IsValidHandle(aHandle: THandle): Boolean;
begin
  Result := (aHandle<>INVALID_HANDLE_VALUE) AND (aHandle<>0);
end;

function CloseAndNilHandle(var aHandle: THandle): Boolean;
var
  LocHandle: THandle;
begin
  if IsValidHandle(aHandle) then
  begin
    LocHandle := aHandle; // locally store handle
    aHandle := INVALID_HANDLE_VALUE; // signal via handle that it is invalid
    Result := CloseHandle(LocHandle); // close after signal invalid because of possible thread race condition
  end
  else Result := False;
end;

function SharedMemServerAvailable(const aServerChannelName: string): Boolean;
var
  handle: THandle;
begin
  handle := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(aServerChannelName));
  if IsValidHandle(handle) then
  begin
    CloseHandle(handle);
    Result := True;
  end
  else Result := False;
end;

function NextSharedMemConnectionGUID(const aServerChannelName: string): TGUID;
var
  smFile: TSMFile;
  channel: PSMChannel;
begin
  Result := TGUID.Empty; // sentinel
  if smFile.Open(aServerChannelName) then
  begin
    try
      if smFile.ChannelCount>0 then
      begin
        channel := smFile.Channel[0];
        if Assigned(channel)
        then channel.Read(Result, SizeOf(Result));
      end;
    finally
      smFile.Close(False);
    end;
  end;
end;

function ConnectToSharedMemConnection(const aServerChannelName: string): TSMFile;
var
  GUID: TGUID;
begin
  GUID := NextSharedMemConnectionGUID(aServerChannelName);
  if GUID<>TGUID.Empty
  then Result.Open(GUIDToString(GUID))
  else Result := TSMFile.Empty;
end;

{ TSMChannelHeader }

procedure TSMChannelHeader.Acquire;
var
  currentReference: integer;
begin
  if fLock>=0 then
  begin
    //Wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
    repeat
      currentReference := fLock AND NOT 1;
    until currentReference = InterlockedCompareExchange(fLock, currentReference + 1, currentReference);
    //Now wait on all readers
    repeat until fLock = 1;
  end;
end;

function TSMChannelHeader.bufferPtr: PByte;
begin
  Result := PByte(@Self)+SizeOf(Self);
end;

procedure TSMChannelHeader.Close(aSignalClose: Boolean);
begin
  if aSignalClose
  then fLock := -1;
end;

function TSMChannelHeader.Connected: Boolean;
begin
  Result := fLock>=0;
end;

function TSMChannelHeader.Create(aDataSize: Integer): Boolean;
begin
  fDataSize := aDataSize;
  fWrite := 0;
  fRead := 0;
  fAvailableForRead := 0;
  fLock := 0;
  Result := True;
end;

function TSMChannelHeader.getAvailableForWrite: Integer;
begin
  Result := fDataSize-fAvailableForRead;
end;

function TSMChannelHeader.getChannelSize: Integer;
begin
  Result := fDataSize+SizeOf(Self);
end;

function TSMChannelHeader.getNextChannelHeader: PSMChannelHeader;
begin
  Result := PSMChannelHeader(PByte(@Self)+ChannelSize);
end;

function TSMChannelHeader.Read(var aBuffer: PByte; var aBufferSize: Integer): Boolean;
var
  cnt: Integer;
begin
  if fAvailableForRead>0 then
  begin
    // determine number of bytes available in this pass
    if fWrite<=fRead
    then cnt := fDataSize-fRead // read to edge
    else cnt := fWrite-fRead;
    // is there realy data available in this pass
    if cnt>0 then
    begin
      // avoid overshoot in reading
      if cnt>aBufferSize
      then cnt := aBufferSize;
      Move(readPtr^, aBuffer^, cnt);
      // adjust buffer
      aBuffer := aBuffer+cnt;
      aBufferSize := aBufferSize-cnt;
      // adjust read position
      fRead := fRead+cnt;
      fAvailableForRead := fAvailableForRead-cnt;
      // over edge detection
      if fRead=fDataSize
      then fRead := 0;
      Result := True;
    end
    else Result := False;
  end
  else Result := False;
end;

function TSMChannelHeader.readPtr: PByte;
begin
  Result := bufferPtr+fRead;
end;
procedure TSMChannelHeader.Release;
begin
  if fLock>0
  then fLock := 0;
end;

function TSMChannelHeader.Write(var aBuffer: PByte; var aBufferSize: Integer): Boolean;
var
  cnt: Integer;
begin
  if getAvailableForWrite>0 then
  begin
    // determine number of bytes to write in this pass
    if fRead<=fWrite
    then cnt := fDataSize-fWrite // fill to edge
    else cnt := fRead-fWrite; // fill to just under read ptr
    if cnt>0 then
    begin
      // check if cnt will fit (no need to write more then cnt)
      if cnt>aBufferSize
      then cnt := aBufferSize;
      Move(aBuffer^, writePtr^, cnt);
      // adjust buffer
      aBuffer := aBuffer+cnt;
      aBufferSize := aBufferSize-cnt;
      // adjust write position
      fWrite := fWrite+cnt;
      fAvailableForRead := fAvailableForRead+cnt;
      // over edge detection
      if fWrite=fDataSize
      then fWrite := 0;
      Result := True;
    end
    else Result := False;
  end
  else Result := False;
end;

function TSMChannelHeader.writePtr: PByte;
begin
  Result := bufferPtr+fWrite;
end;

{ TSMChannel }

procedure TSMChannel.Close(aSignalClose: Boolean);
begin
  // signal channel closed and unlock waiting threads
  if Assigned(fHeader) then
  begin
    fHeader.Close(aSignalClose);
    fHeader := nil; // todo: too soon?
  end;
  if IsValidHandle(fRoom) then
  begin
    if aSignalClose
    then SetEvent(fRoom);
    CloseAndNilHandle(fRoom);
  end;
  if IsValidHandle(fFilled) then
  begin
    if aSignalClose
    then SetEvent(fFilled);
    CloseAndNilHandle(fFilled);
  end;
end;

procedure TSMChannel.Create(const aName: string; var aHeader: PSMChannelHeader; aDataSize: Integer);
begin
  fName := aName;
  fRoom := CreateEvent(nil, False, True, PChar(fName+'_room'));
  fFilled := CreateEvent(nil, False, False, PChar(fName+'_filled'));
  fHeader := aHeader;
  fHeader.Create(aDataSize);
  aHeader := fHeader.getNextChannelHeader;
end;

procedure TSMChannel.Open(const aName: string; var aHeader: PSMChannelHeader);
begin
  fName := aName;
  fRoom := OpenEvent(EVENT_MODIFY_STATE OR SYNCHRONIZE, False, PChar(fName+'_room'));
  fFilled := OpenEvent(EVENT_MODIFY_STATE OR SYNCHRONIZE, False, PChar(fName+'_filled'));
  fHeader := aHeader;
  aHeader := fHeader.getNextChannelHeader;
end;

function TSMChannel.Read(var aBuffer; aBufferSize: Integer): Integer;
var
  pBuffer: PByte;
  InitialAvailableForWrite: Integer;
  res: DWORD;
begin
  if Assigned(@Self) and Assigned(Header) then
  begin
    Result := aBufferSize; //assume everyting goes well
    if (aBufferSize>0) then
    begin
      Header.Acquire;
      try
        pBuffer := @aBuffer;
        repeat
          // remember if available for write is 0 (a writer might be waiting after our read)
          InitialAvailableForWrite := Header.getAvailableForWrite;
          // try to read as much as possible
          repeat until (aBufferSize=0) or (Header.fAvailableForRead=0) or not Header.Read(pBuffer, aBufferSize);
          // check if we have read data
          if (InitialAvailableForWrite=0) and (Header.getAvailableForWrite>0)
          then SetEvent(fRoom);
          // check if there is more to be read
          if aBufferSize>0 then
          begin
            if fHeader.Connected  then
            begin
              // wait for data; AvailableForRead must be 0
              Header.Release;
              try
                // now we wait for new data to be read
                repeat
                  res := WaitForSingleObject(fFilled, smChannelReadTimeOut{INFINITE});
                  if (res=WAIT_ABANDONED) or
                     ((res=WAIT_TIMEOUT) and (not Assigned(Header) or not Header.Connected)) then
                  begin
                    // only happens when event is killed -> free on channel
                    aBufferSize := 0; // exit loop
                    Result := smChannelError; // signal error
                    // the caller is not informed on how much data there is in the buffer
                  end;
                until (res=WAIT_OBJECT_0) or (Result=smChannelError);
              finally
                // check for valid header in case of terminate
                if Assigned(Header)
                then Header.Acquire;
              end;
            end
            else Result := smChannelError;
          end;
        until (aBufferSize=0) or (Result=smChannelError) or not Assigned(Header);
      finally
        if Assigned(Header)
        then Header.Release;
      end;
    end;
  end
  else Result := smChannelError;
end;

function TSMChannel.Write(var aBuffer; aBufferSize: Integer): Integer;
var
  pBuffer: PByte;
  InitialAvailableForRead: Integer;
  res: DWORD;
begin
  if Assigned(Header) then
  begin
    Result := aBufferSize; // assume everything goes ok
    if aBufferSize>0 then
    begin
      Header.Acquire;
      try
        pBuffer := @aBuffer;
        repeat
          // remember if available for read is 0 (a reader might be waiting after our write)
          InitialAvailableForRead := Header.fAvailableForRead;
          // try to write as much as possible
          repeat until (aBufferSize=0) or (Header.getAvailableForWrite=0) or not Header.Write(pBuffer, aBufferSize);
          // check if we have written data
          if (InitialAvailableForRead=0) and (Header.fAvailableForRead>0)
          then SetEvent(fFilled);
          // check if there is more to be written
          if aBufferSize>0 then
          begin
            if fHeader.Connected then
            begin
              // wait for room; AvailableForWrite must be 0
              Header.Release;
              try
                // now we wait for new data to be read
                res := WaitForSingleObject(fRoom, smChannelWriteTimeOut{INFINITE});
                if res<>WAIT_OBJECT_0 then
                begin
                  // only happens when event is killed -> free on channel
                  aBufferSize := 0; // exit loop
                  Result := smChannelError; // signal error
                  // the caller is not informed on how much data was written
                end;
              finally
                Header.Acquire;
              end;
            end
            else Result := smChannelError;
          end;
        until (aBufferSize=0) or (Result=smChannelError) or not fHeader.Connected;
      finally
        Header.Release;
      end;
    end;
  end
  else Result := smChannelError;
end;

{ TSMFileHeader }

function TSMFileHeader.Create(aSize, aChannelCount: Integer): Boolean;
begin
  fChannelCount := aChannelCount;
  fClientCount := 0;
  fStatus := smfsConnected;
  Result := True;
end;

function TSMFileHeader.getFirstChannelHeader: PSMChannelHeader;
begin
  Result := PSMChannelHeader(PByte(@Self)+SizeOf(Self));
end;

{ TSMFile }

function TSMFile.Create(const aConnectionName: string; aChannelCount, aChannelDataSize: Integer): Boolean;
var
  fileSize: Integer;
  p: PSMChannelHeader;
  c: Integer;
begin
  Result := False; // sentinel, assume fault in create
  fIsClient := False;
  fConnectionName := aConnectionName;
  fileSize := SizeOf(TSMFileHeader)+aChannelCount*(aChannelDataSize+SizeOf(TSMChannelHeader));
  fFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, fileSize, PChar(aConnectionName));
  if IsValidHandle(fFileMapping) then
  begin
    fFileHeader := MapViewOfFile(fFileMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if Assigned(fFileHeader) then
    begin
      // init channels
      SetLength(fChannels, aChannelCount);
      p := fFileHeader.getFirstChannelHeader;
      for c := 0 to Length(fChannels)-1
      do fChannels[c].Create(aConnectionName+IntToStr(c), p, aChannelDataSize);
      // init file header after channels to prevent use before channels are initialized
      fFileHeader.Create(fileSize, aChannelCount);
      Result := True;
    end
    else SetLength(fChannels, 0);
  end
  else
  begin
    fFileHeader := nil;
    SetLength(fChannels, 0);
  end;
end;

class function TSMFile.Empty: TSMFile;
begin
  Result.fConnectionName := '';
  Result.fFileMapping := INVALID_HANDLE_VALUE;
  Result.fFileHeader := nil;
  SetLength(Result.fChannels, 0);
end;

class operator TSMFile.Equal(const Left, Right: TSMFile): Boolean;
begin
  Result := Left.fConnectionName=Right.fConnectionName;
end;

function TSMFile.Close(aSignalClose: Boolean): Boolean;
var
  c: Integer;
  localFileHeader: PSMFileHeader;
begin
  if Assigned(fFileHeader) then
  begin
    localFileHeader := fFileHeader;
    fFileHeader := nil;
    // signal close of connection
    if fIsClient
    then InterlockedDecrement(localFileHeader.fClientCount)
    else localFileHeader.fStatus := localFileHeader.fStatus and not smfsConnected;
    for c := 0 to Length(fChannels)-1
    do fChannels[c].Close(aSignalClose);
    UnmapViewOfFile(localFileHeader);
  end;
  SetLength(fChannels, 0);
  CloseAndNilHandle(fFileMapping);
  Result := True;
end;

function TSMFile.getChannel(aIndex: Integer): PSMChannel;
begin
  if (0<=aIndex) and (aIndex< Length(fChannels))
  then Result := @fChannels[aIndex]
  else Result := nil;
end;

function TSMFile.getChannelCount: Integer;
begin
  Result := Length(fChannels);
end;

function TSMFile.getConnected: Boolean;
begin
  if Assigned(fFileHeader) then
  begin
    if fIsClient
    then Result := (fFileHeader.fStatus and smfsConnected)<>0
    else Result := fFileHeader.fClientCount>0;
  end
  else Result := False;
end;

class operator TSMFile.NotEqual(const Left, Right: TSMFile): Boolean;
begin
  Result := Left.fConnectionName<>Right.fConnectionName;
end;

function TSMFile.Open(const aConnectionName: string): Boolean;
var
  c: Integer;
  p: PSMChannelHeader;
begin
  Result := False;
  fIsClient := True;
  fConnectionName := aConnectionName;
  fFileMapping := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PChar(aConnectionName));
  if IsValidHandle(fFileMapping) then
  begin
    fFileHeader := MapViewOfFile(fFileMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if Assigned(fFileHeader) then
    begin
      SetLength(fChannels, fFileHeader.fChannelCount);
      p := fFileHeader.getFirstChannelHeader;
      for c := 0 to Length(fChannels)-1
      do fChannels[c].Open(aConnectionName+IntToStr(c), p);
      InterlockedIncrement(fFileHeader.fClientCount);
      Result := True;
    end
    else SetLength(fChannels, 0);
  end
  else
  begin
    fFileHeader := nil;
    SetLength(fChannels, 0);
  end;
end;



end.
