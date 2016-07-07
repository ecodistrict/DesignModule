unit IMB3Locator;

interface

uses
  SocksLib,
  ByteBuffers,
  IMB3Core,
  SysUtils;

type
  TIMBLocator = class
  private
    const ProtocolSep = '://';
  public
    class function DecodeServerURI(const aServerURI: string; out aServer: string; out aPort: Integer): Boolean;
    class function LocateServerURI(aAddressFamily: Integer=AF_INET; aPort: Integer=4000; aTimeout: Integer=1000): string;
  end;

implementation

{ TIMBLocator }

class function TIMBLocator.DecodeServerURI(const aServerURI: string; out aServer: string; out aPort: Integer): Boolean;
var
  i: Integer;
begin
  // uri:  protocol://server[:port][/path]
  if not aServerURI.IsEmpty then
  begin
    // remove protocol
    i := aServerURI.IndexOf(ProtocolSep);
    if i >= 0 then
    begin
      aServer := aServerURI.Substring(i + Length(ProtocolSep));
      aPort := DefaultPort;
      // remove optional path
      i := aServer.IndexOf('/');
      if i >= 0
      then aServer := aServer.Substring(0, i);
      // separate optional port from server
      i := aServer.IndexOf(':');
      if i >= 0 then
      begin
        aPort := aServer.Substring(i+1).Trim().ToInteger();
        aServer := aServer.Substring(0, i);
      end;
      Result := True;
    end
    else Result := False;
  end
  else Result := False;
end;

class function TIMBLocator.LocateServerURI(aAddressFamily, aPort, aTimeout: Integer): string;
var
  server: string;
  s: TSocket;
  localAddress: TSockAddr;
  toAddress: TSockAddr;
  OptVal: Integer;
  checkSum: Int32;
  payload: AnsiString;
  buffer: TByteBuffer;
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  remoteAddr: TSockAddr;
  remoteAddrSize: Integer;
  recbytes: Integer;
  command: Int32;
  res: Integer;
begin
  server := '';
  s := socket(aAddressFamily, SOCK_DGRAM, IPPROTO_UDP);
  try
    LocalAddress.Family := aAddressFamily;
    LocalAddress.Port := 0;
    LocalAddress.AddrAny := True;
    if bind(s, LocalAddress, SizeOf(LocalAddress))<>SOCKET_ERROR then
    begin
      // client must be able to send broadcast (option) and multicast (option)
      case aAddressFamily of
        AF_INET:
          begin
            OptVal := 1; // enable sending broadcast on socket
            if setsockopt(s, SOL_SOCKET, SO_BROADCAST, PAnsiChar(@OptVal), SizeOf(OptVal))=SOCKET_ERROR
            then raise Exception.Create('Locator ('+AddressFamilyToStr(aAddressFamily)+'): could not set broadcast send option: '+IntToStr(WSAGetLastError));
            toAddress.Family := aAddressFamily;
            toAddress.SetBroadcast(aPort);
          end;
        AF_INET6:
          begin
            OptVal := 0; // enable sending broadcast on socket
            if setsockopt(s, IPPROTO_IPV6, IPV6_MULTICAST_IF, PAnsiChar(@OptVal), SizeOf(OptVal))=SOCKET_ERROR
            then raise Exception.Create('Locator ('+AddressFamilyToStr(aAddressFamily)+'): could not set multicast send option: '+IntToStr(WSAGetLastError));
            toAddress.Family := aAddressFamily;
            toAddress.SetAllSameLinkNodesMulticast(aPort);
          end;
      else
        raise Exception.Create('Locator only support address family '+IntToStr(AF_INET)+' (IPv4) and '+IntToStr(AF_INET6)+' (IPv6), not '+IntToStr(aAddressFamily));
      end;
      // send request
      // first build message
      buffer.Clear();
      buffer.Prepare(MagicBytes[1], Length(MagicBytes));
      buffer.Prepare(Int32(icHUBLocate));
      buffer.Prepare(Int32(0));
      buffer.PrepareApply();
      buffer.QWrite(MagicBytes[1], Length(MagicBytes));
      buffer.QWrite(Int32(icHUBLocate));
      buffer.QWrite(Int32(0));
      res := sendto(s, buffer.Address^, buffer.Length, 0, toAddress, SizeOf(toAddress));
      if res<>SOCKET_ERROR then
      begin
        // wait for response
        FDSet.fd_count := 1;
        FDSet.fd_array[0] := s;
        TimeVal.tv_sec := aTimeOut div 1000;
        TimeVal.tv_usec := (aTimeOut*1000) mod 1000000;
        if select(0, @FDSet, nil, nil, @TimeVal)>0 then
        begin // there is data
          FillChar(remoteAddr, SizeOf(remoteAddr), 0);
          remoteAddrSize := SizeOf(remoteAddr);
          buffer.Clear(MaxUDPCommandBufferSize);
          // get the received data
          recbytes :=  recvfrom(s, buffer.Address^, buffer.Length, 0, remoteAddr, remoteAddrSize);
          if recbytes>0 then
          begin
            // decode the received data
            buffer.Length := recbytes;
            if buffer.Compare(PAnsiChar(MagicBytes)^, Length(MagicBytes)) then
            begin
              buffer.SkipReading(Length(MagicBytes));
              buffer.Read(command);
              buffer.Read(payload);
              if payload<>'' then
              begin
                buffer.Read(CheckSum);
                if CheckSum=CheckStringMagic then
                begin
                  if command=icHUBFound
                  then server := string(payload);
                end;
              end;
            end;
          end;
        end
      end
      else raise Exception.Create('Could not send locator request ('+WSAGetLastError.ToString()+')');
    end
    else raise Exception.Create('Could not bind socket for sending locator request ('+WSAGetLastError.ToString()+')');
  finally
    MyCloseSocket(s);
  end;
  Result := server;
end;

end.
