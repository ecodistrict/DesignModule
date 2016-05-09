unit imb.SocksLib;

// implements winsock2 for IMB (default winsock and winsock2 are incomplete esp. for ipv6)

// auto init and exit of winsock..

interface

uses
  Windows, SysUtils;

const
  ws2_32DLL = 'ws2_32.dll';

  AI_PASSIVE                = $01;    // The socket address will be used in a call to the bind function.
  AI_CANONNAME              = $02;    // The canonical name is returned in the first ai_canonname member.
  AI_NUMERICHOST            = $04;    // The nodename parameter passed to the getaddrinfo function must be a numeric string.
  AI_ADDRCONFIG             = $400;   // The getaddrinfo will resolve only if a global address is configured. The IPv6 and IPv4 loopback address is not considered a valid global address. This option is only supported on Windows Vista or later.
  AI_NON_AUTHORITATIVE      = $4000;  // The address information can be from a non-authorative namespace provider. This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
  AI_SECURE                 = $8000;  // The address information is from a secure channel. This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
  AI_RETURN_PREFERRED_NAMES = $10000; // The address information is for a preferred name for a user. This option is only supported on Windows Vista or later for the NS_EMAIL namespace.


  SOCK_STREAM = 1;
  SOCK_DGRAM = 2;
  SOCK_RAW = 3;
  SOCK_RDM = 4;
  SOCK_SEQPACKET = 5;

  { Option flags per-socket. }
  SO_DEBUG        = $0001;          { turn on debugging info recording }
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }

  SO_DONTLINGER  =   $ff7f;

  { Additional options. }
  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  SO_SNDTIMEO     = $1005;          { send timeout }
  SO_RCVTIMEO     = $1006;          { receive timeout }
  SO_ERROR        = $1007;          { get error status and clear }
  SO_TYPE         = $1008;          { get socket type }

  { Options for connect and disconnect data and options.  Used only by
    non-TCP/IP transports such as DECNet, OSI TP4, etc. }
  SO_CONNDATA     = $7000;
  SO_CONNOPT      = $7001;
  SO_DISCDATA     = $7002;
  SO_DISCOPT      = $7003;
  SO_CONNDATALEN  = $7004;
  SO_CONNOPTLEN   = $7005;
  SO_DISCDATALEN  = $7006;
  SO_DISCOPTLEN   = $7007;

  { Option for opening sockets for synchronous access. }
  SO_OPENTYPE     = $7008;

  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;

  { Other NT-specific options. }
  SO_MAXDG        = $7009;
  SO_MAXPATHDG    = $700A;
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  SO_CONNECT_TIME = $700C;

  SOL_SOCKET      = $ffff;          {options for socket level }

  { socket options for IPv6 }
  IPV6_HDRINCL          = 2 ;  // int; header is included with data
  IPV6_UNICAST_HOPS     = 4 ;  // Set/get IP unicast hop limit.
  IPV6_MULTICAST_IF     = 9 ;  // Set/get IP multicast interface.
  IPV6_MULTICAST_HOPS   = 10 ; // Set/get IP multicast ttl.
  IPV6_MULTICAST_LOOP   = 11 ; // Set/get IP multicast loopback.
  IPV6_ADD_MEMBERSHIP   = 12 ; // Add an IP group membership.
  IPV6_DROP_MEMBERSHIP  = 13 ; // Drop an IP group membership.
  IPV6_JOIN_GROUP       = IPV6_ADD_MEMBERSHIP;
  IPV6_LEAVE_GROUP      = IPV6_DROP_MEMBERSHIP;
  IPV6_PKTINFO          = 19;  // Receive packet information for ipv6
  IPV6_HOPLIMIT         = 21;  // Receive packet hop limit
  IPV6_PROTECTION_LEVEL = 23;  // Set/get IPv6 protection level



  { Maximum queue length specifiable by listen. }
  SOMAXCONN       = 5;

  MSG_OOB         = $1;             {process out-of-band data }
  MSG_PEEK        = $2;             {peek at incoming message }
  MSG_DONTROUTE   = $4;             {send without using routing tables }
  MSG_WAITALL     = $8;             {do not complete until packet is completely filled}

  MSG_MAXIOVLEN   = 16;

  MSG_PARTIAL     = $8000;          {partial send or recv for message xport }

  { TCP options. }
  TCP_NODELAY     = $0001;
  TCP_BSDURGENT   = $7000;

  { Address families. }
  AF_UNSPEC       = 0;               { unspecified }
  AF_UNIX         = 1;               { local to host (pipes, portals) }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_IMPLINK      = 3;               { arpanet imp addresses }
  AF_PUP          = 4;               { pup protocols: e.g. BSP }
  AF_CHAOS        = 5;               { mit CHAOS protocols }
  AF_IPX          = 6;               { IPX and SPX }
  AF_NS           = 6;               { XEROX NS protocols }
  AF_ISO          = 7;               { ISO protocols }
  AF_OSI          = AF_ISO;          { OSI is ISO }
  AF_ECMA         = 8;               { european computer manufacturers }
  AF_DATAKIT      = 9;               { datakit protocols }
  AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
  AF_SNA          = 11;              { IBM SNA }
  AF_DECnet       = 12;              { DECnet }
  AF_DLI          = 13;              { Direct data link interface }
  AF_LAT          = 14;              { LAT }
  AF_HYLINK       = 15;              { NSC Hyperchannel }
  AF_APPLETALK    = 16;              { AppleTalk }
  AF_NETBIOS      = 17;              { NetBios-style addresses }
  AF_VOICEVIEW    = 18;              { VoiceView }
  AF_FIREFOX      = 19;              { FireFox }
  AF_UNKNOWN1     = 20;              { Somebody is using this! }
  AF_BAN          = 21;              { Banyan }
  AF_INET6        = 23;
  AF_IRDA         = 26;
  AF_BTM          = 32;

type
  u_short = Word;
  u_int = Integer;
  u_long = Longint;

  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: byte;
  end;

  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  TInAddr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;

  S6unB = packed record
    s_b1, s_b2, s_b3, s_b4, s_b5, s_b6, s_b7, s_b8, s_b9, s_b10, s_b11, s_b12, s_b13, s_b14, s_b15, s_b16: byte;
  end;

  S6unW = packed record
    s_w1, s_w2, s_w3, s_w4, s_w5, s_w6, s_w7, s_w8: u_short;
  end;

  TIn6Addr = record
    case Integer of
      0: (S_un_b: S6unB);
      1: (S_un_w: S6unW);
  end;

  PSockAddr = ^TSockAddr;
  TSockAddr = record
  private
    function GetFamily: u_short;
    procedure SetFamily(const aValue: u_short);
    function GetAddrAsStr: string;
    procedure SetAddrAsStr(const aValue: string);
    function GetPort: u_short;
    procedure SetPort(const aValue: u_short);
    function GetAddrAndPortStr: string;
    procedure SetAddrAndPortStr(aValue: string);
    function GetAddrAny: Boolean;
    procedure SetAddrAny(const aValue: Boolean);
    function GetAddrLocalHost: Boolean;
    procedure SetAddrLocalHost(const aValue: Boolean);
  public
    property Family: u_short read GetFamily write SetFamily;
    property Port: u_short read GetPort write SetPort;
    property AddrAsStr: string read GetAddrAsStr write SetAddrAsStr;
    property AddrAndPortStr: string read GetAddrAndPortStr write SetAddrAndPortStr;
    property AddrAny: Boolean read GetAddrAny write SetAddrAny;
    property AddrLocalHost: Boolean read GetAddrLocalHost write SetAddrLocalHost;

    procedure SetBroadcast(aPort: u_Short); // always ipv4, no broadcast in ipv6, use multicast..
    procedure SetAllSameLinkNodesMulticast(aPort: u_Short); // always ipv6

    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr);
      1: (sin6_family: u_short;
          sin6_port: u_short;
          sin6_flowinfo: u_long;
          sin6_addr: TIn6Addr;
          sin6_scope_id: u_long);
      2: (sin6old_family: u_short;
          sin6old_port: u_short;
          sin6old_flowinfo: u_long;
          sin6old_addr: TIn6Addr);
  end;

  TSockAddresses = array of TSockAddr;

type
  PAddrInfoW = ^TAddrInfoW;
  TAddrInfoW = record
    ai_flags        : Integer;      // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family       : Integer;      // PF_xxx
    ai_socktype     : Integer;      // SOCK_xxx
    ai_protocol     : Integer;      // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen      : ULONG;        // Length of ai_addr
    ai_canonname    : PChar;        // todo: // Canonical name for nodename
    ai_addr         : PSockAddr;    // Binary address
    ai_next         : PAddrInfoW;   // Next structure in linked list
    function FamilyToStr: string;
    function SockTypeToStr: string;
    function ProtocolToStr: string;
  end;

  Tipv6_mreq = record
    ipv6mr_multiaddr: TIn6Addr;
    ipv6mr_interface: u_int;
  end;

  { Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = record
    l_onoff: u_short;
    l_linger: u_short;
  end;

type
  TSocket = IntPtr;

const
  // ipv4
  INADDR_ANY = $00000000;
  INADDR_LOOPBACK = $7F000001;
  INADDR_BROADCAST = u_long($FFFFFFFF);
  INADDR_NONE = DWORD($FFFFFFFF);

  INVALID_SOCKET = TSocket(NOT(0));
  SOCKET_ERROR = -1;

//  FD_SETSIZE     =   64;

  // WinSock 2 extension -- manifest constants for shutdown()
  SD_RECEIVE     = 0;
  SD_SEND        = 1;
  SD_BOTH        = 2;

  IPPROTO_IP     =   0;             { dummy for IP }
  IPPROTO_ICMP   =   1;             { control message protocol }
  IPPROTO_IGMP   =   2;             { group management protocol }
  IPPROTO_GGP    =   3;             { gateway^2 (deprecated) }
  IPPROTO_TCP    =   6;             { tcp }
  IPPROTO_PUP    =  12;             { pup }
  IPPROTO_UDP    =  17;             { user datagram protocol }
  IPPROTO_IDP    =  22;             { xns idp }
  IPPROTO_RDP    =  27;

  IPPROTO_IPV6          = 41; // IPv6 header
  IPPROTO_ROUTING       = 43; // IPv6 Routing header
  IPPROTO_FRAGMENT      = 44; // IPv6 fragmentation header
  IPPROTO_ESP           = 50; // encapsulating security payload
  IPPROTO_AH            = 51; // authentication header
  IPPROTO_ICMPV6        = 58; // ICMPv6
  IPPROTO_NONE          = 59; // IPv6 no next header
  IPPROTO_DSTOPTS       = 60; // IPv6 Destination options

  IPPROTO_ND     =  77;             { UNOFFICIAL net disk proto }
  IPPROTO_RM     = 113;             { The PGM protocol for reliable multicast }

  IPPROTO_RAW    =  255;            { raw IP packet }
  IPPROTO_MAX    =  256;

  { Port/socket numbers: network standard functions}
  IPPORT_ECHO    =   7;
  IPPORT_DISCARD =   9;
  IPPORT_SYSTAT  =   11;
  IPPORT_DAYTIME =   13;
  IPPORT_NETSTAT =   15;
  IPPORT_FTP     =   21;
  IPPORT_TELNET  =   23;
  IPPORT_SMTP    =   25;
  IPPORT_TIMESERVER  =  37;
  IPPORT_NAMESERVER  =  42;
  IPPORT_WHOIS       =  43;
  IPPORT_MTP         =  57;

  { Port/socket numbers: host specific functions }
  IPPORT_TFTP        =  69;
  IPPORT_RJE         =  77;
  IPPORT_FINGER      =  79;
  IPPORT_TTYLINK     =  87;
  IPPORT_SUPDUP      =  95;

  { UNIX TCP sockets }
  IPPORT_EXECSERVER  =  512;
  IPPORT_LOGINSERVER =  513;
  IPPORT_CMDSERVER   =  514;
  IPPORT_EFSSERVER   =  520;

  { UNIX UDP sockets }
  IPPORT_BIFFUDP     =  512;
  IPPORT_WHOSERVER   =  513;
  IPPORT_ROUTESERVER =  520;

  { Ports < IPPORT_RESERVED are reserved for
    privileged processes (e.g. root). }
  IPPORT_RESERVED    =  1024;

  { Link numbers }
  IMPLINK_IP         =  155;
  IMPLINK_LOWEXPER   =  156;
  IMPLINK_HIGHEXPER  =  158;

  WSABASEERR              = 10000;

  { Windows Sockets definitions of regular Microsoft C error constants }
  WSAEINTR                = (WSABASEERR+4);
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);

  { Windows Sockets definitions of regular Berkeley error constants }
  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);

  WSAEDISCON              = (WSABASEERR+101);
  { Extended Windows Sockets error constant definitions }
  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);

  { Error return codes from gethostbyname() and gethostbyaddr()
    (when using the resolver). Note that these errors are
    retrieved via WSAGetLastError() and must therefore follow
    the rules for avoiding clashes with error numbers from
    specific implementations or language run-time systems.
    For this reason the codes are based at WSABASEERR+1001.
    Note also that [WSA]NO_ADDRESS is defined only for
    compatibility purposes. }

  { Authoritative Answer: Host not found }
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;

  { Non-Authoritative: Host not found, or SERVERFAIL }
  WSATRY_AGAIN            = (WSABASEERR+1002);
  TRY_AGAIN               = WSATRY_AGAIN;
  { Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  WSANO_RECOVERY          = (WSABASEERR+1003);
  NO_RECOVERY             = WSANO_RECOVERY;
  { Valid name, no data record of requested type }
  WSANO_DATA              = (WSABASEERR+1004);


  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

type
  TWSAData = record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  end;

const
  MAX_PROTOCOL_CHAIN = 7;
  WSAPROTOCOL_LEN = 255;

  { Flag bit definitions for dwProviderFlags }
  PFL_MULTIPLE_PROTO_ENTRIES           = $00000001;
  PFL_RECOMMENDED_PROTO_ENTRY          = $00000002;
  PFL_HIDDEN                           = $00000004;
  PFL_MATCHES_PROTOCOL_ZERO            = $00000008;

  { Flag bit definitions for dwServiceFlags1 }
  XP1_CONNECTIONLESS                   = $00000001;
  XP1_GUARANTEED_DELIVERY              = $00000002;
  XP1_GUARANTEED_ORDER                 = $00000004;
  XP1_MESSAGE_ORIENTED                 = $00000008;
  XP1_PSEUDO_STREAM                    = $00000010;
  XP1_GRACEFUL_CLOSE                   = $00000020;
  XP1_EXPEDITED_DATA                   = $00000040;
  XP1_CONNECT_DATA                     = $00000080;
  XP1_DISCONNECT_DATA                  = $00000100;
  XP1_SUPPORT_BROADCAST                = $00000200;
  XP1_SUPPORT_MULTIPOINT               = $00000400;
  XP1_MULTIPOINT_CONTROL_PLANE         = $00000800;
  XP1_MULTIPOINT_DATA_PLANE            = $00001000;
  XP1_QOS_SUPPORTED                    = $00002000;
  XP1_INTERRUPT                        = $00004000;
  XP1_UNI_SEND                         = $00008000;
  XP1_UNI_RECV                         = $00010000;
  XP1_IFS_HANDLES                      = $00020000;
  XP1_PARTIAL_MESSAGE                  = $00040000;

  BIGENDIAN                            = $0000;
  LITTLEENDIAN                         = $0001;

  SECURITY_PROTOCOL_NONE               = $0000;

type
  TWSAPROTOCOLCHAIN = record
    ChainLen: Integer;
    ChainEntries: array[0..MAX_PROTOCOL_CHAIN-1] of DWORD;
  end;

  TWSAProtocol_Info = record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: TWSAPROTOCOLCHAIN;
    iVersion: Integer;
    iAddressFamily: Integer;
    iMaxSockAddr: Integer;
    iMinSockAddr: Integer;
    iSocketType: Integer;
    iProtocol: Integer;
    iProtocolMaxOffset: Integer;
    iNetworkByteOrder: Integer;
    iSecurityScheme: Integer;
    dwMessageSize: DWORD;
    dwProviderReserved: DWORD;
    szProtocol: array[0..WSAPROTOCOL_LEN] of Char;
  end;

const
  FD_SETSIZE = 64;

type
  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;

  TSelectSet = record
  private
    FCount: Integer;
    FBuffer: TBytes; // todo:
  public
    property Count: Integer read FCount;
    procedure Add(aSocket: TSocket);
    procedure Clear;
    function FDSet: PFDSet;
  end;

  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

  PHostEnt = ^THostEnt;
  hostent = record
    h_name: PChar; // todo:
    h_aliases: ^PChar; // todo:
    h_addrtype: Smallint;
    h_length: Smallint;
    case Byte of
      0: (h_addr_list: ^PChar); // todo:
      1: (h_addr: ^PChar) // todo:
  end;
  THostEnt = hostent;



function getaddrinfo(NodeName: PChar; ServiceName: PChar; Hints: PaddrinfoW; var ppResult: PAddrInfoW): Integer; stdcall; external ws2_32DLL name 'GetAddrInfoW'; // todo:
procedure freeaddrinfo(ai: PAddrinfoW); stdcall; external ws2_32DLL name 'FreeAddrInfoW';
function ntohs(netshort: u_short): u_short; stdcall; external ws2_32DLL name 'ntohs';
function htonl(hostlong: u_long): u_long; stdcall; external ws2_32DLL name 'htonl';
function htons(hostshort: u_short): u_short; stdcall; external ws2_32DLL name 'htons';
function shutdown(s: TSocket; how: Integer): Integer; stdcall; external ws2_32DLL name 'shutdown';
function closesocket(s: TSocket): Integer; stdcall; external ws2_32DLL name 'closesocket';
function setsockopt(s: TSocket; level, optname: Integer; optval: PChar; optlen: Integer): Integer; stdcall; external ws2_32DLL name 'setsockopt'; // todo:
function socket(af, Struct, protocol: Integer): TSocket; stdcall; external ws2_32DLL name 'socket';
function recvfrom(s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall; external ws2_32DLL name 'recvfrom';
function getsockopt(s: TSocket; level, optname: Integer; optval: PChar; var optlen: Integer): Integer; stdcall; external ws2_32DLL name 'getsockopt'; // todo:
function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall; external ws2_32DLL name 'bind';
function sendto(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall; external ws2_32DLL name 'sendto';
function connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall; external ws2_32DLL name 'connect';
function recv(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall; external ws2_32DLL name 'recv';
function send(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall; external ws2_32DLL name 'send';
function listen(s: TSocket; backlog: Integer): Integer; stdcall; external ws2_32DLL name 'listen';
function accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket; stdcall; external ws2_32DLL name 'accept';
function getsockname(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall; external ws2_32DLL name 'getsockname';
function select(nfds: Integer; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; stdcall; external ws2_32DLL name 'select';

// ipv4 only
//function inet_ntoa(inaddr: TInAddr): PAnsiChar; stdcall; external ws2_32DLL name 'inet_ntoa';
// from vista/Windows 2008 and up
//function inet_ntop(aFamily: Integer; pAddr: Pointer; aBuffer: PAnsiChar; aBufferSize: Integer): PAnsiChar; stdcall; external ws2_32DLL name 'inet_ntop';
//function inet_pton(aFamily: Integer; pszAddrString: PAnsiChar; pAddrBuf: Pointer): Integer; stdcall; external ws2_32DLL name 'inet_pton';

function WSAStartup(wVersionRequired: word; var WSData: TWSAData): Integer; stdcall; external ws2_32DLL name 'WSAStartup';
function WSACleanup: Integer; stdcall; external ws2_32DLL name 'WSACleanup';
function WSAGetLastError: Integer; stdcall; external ws2_32DLL name 'WSAGetLastError';
function WSAEnumProtocolsW(lpiProtocols: PInteger; var ProtocolBuffer; var dwBufferLength: DWORD): Integer; stdcall; external ws2_32DLL name 'WSAEnumProtocolsW';
function gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt; stdcall; external ws2_32DLL name 'gethostbyname';
function getnameinfo(sa: PSockAddr; salen: Integer; host: PChar; hostlen: Integer; serv: PChar; servlen: Integer; flags: Integer): Integer; stdcall; external ws2_32DLL name 'GetNameInfoW'; // todo:

const
  NI_MAXHOST = 1025;
  NI_MAXSERV = 32;

  NI_NOFQDN = $1;          // Only return nodename portion for local hosts.
  NI_NUMERICHOST = $2;     // Return numeric form of the host's address.
  NI_NAMEREQD = $4;        // Error if the host's name not in DNS.
  NI_NUMERICSERV = $8;     // Return numeric form of the service (port #).
  NI_DGRAM = $10;          // Service is a datagram service.



function  GetSocketAddresses           ( const aHost, aService: string): TSockAddresses;

procedure MyCloseSocket                ( var aSocket: TSocket);

// to disable nagle algorithm (no delay on sending tcp packets)
function  SetSocketNoDelay             ( var aSocket: TSocket; aValue: Boolean): Boolean;
function  GetSocketNoDelay             ( var aSocket: TSocket; out aValue: Boolean): Boolean;
function  SetSocketKeepAlive           ( var aSocket: TSocket; aValue: Boolean): Boolean;
function  GetSocketKeepAlive           ( var aSocket: TSocket; out aValue: Boolean): Boolean;

function  SetSocketLinger              ( var aSocket: TSocket; aValue: Integer): Boolean;
function  GetSocketLinger              ( var aSocket: TSocket): Integer;

function  AddressFamilyToStr           ( aFamily: Integer): string;
function  AddressProtocolToStr         ( aProtocol: Integer): string;
function  AddressSockTypeToStr         ( aSockType: Integer): string;

type
  TAddressFamilies = array of Integer;

function  AddressFamilies              ( aProtocol: Integer=-1): TAddressFamilies; // aProtocol can be IPPROTO_UDP, IPPROTO_TCP etc.



implementation

function AddressFamilyToStr(aFamily: Integer): string;
begin
  case aFamily of
    AF_UNSPEC:  Result := 'unspecified.';
    AF_INET:    Result := 'IPv4';
    AF_NETBIOS: Result := 'NetBIOS';
    AF_INET6:   Result := 'IPv6';
    AF_IRDA:    Result := 'IrDA';
    AF_BTM:     Result := 'Bluetooth';
  else
                Result := 'Unknown ('+IntToStr(aFamily)+')';
  end;
end;

function AddressProtocolToStr(aProtocol: Integer): string;
begin
  case aProtocol of
    SOCK_STREAM:    Result := 'TCP';
    SOCK_DGRAM:     Result := 'UDP';
    SOCK_RAW:       Result := 'RAW';
    SOCK_RDM:       Result := 'Multicast';
    SOCK_SEQPACKET: Result := 'SEQ';
  else
                    Result := 'Unknown ('+IntToStr(aProtocol)+')';
  end;
end;

function AddressSockTypeToStr(aSockType: Integer): string;
begin
  case aSocktype of
    IPPROTO_TCP:    Result := 'TCP';
    IPPROTO_UDP:    Result := 'UDP';
    IPPROTO_RM:     Result := 'Multicast';
  else
                    Result := 'Unknown ('+IntToStr(aSockType)+')';
  end;
end;


{ TMySockAddr }

function TSockAddr.GetAddrAndPortStr: string;
var
  a: string;
begin
  a := AddrAsStr;
  if a<>'' then
  begin
    if Pos(':', a)<>0
    then Result := '['+a+']'+':'+IntToStr(Port)
    else Result := a+':'+IntToStr(Port);
  end
  else Result := '';
end;

function TSockAddr.GetAddrAny: Boolean;
begin
  case sin_family of
    AF_INET:
      Result :=
        (sin_addr.S_un_b.s_b1=0) and (sin6_addr.S_un_b.s_b2=0) and
        (sin_addr.S_un_b.s_b3=0) and (sin6_addr.S_un_b.s_b4=0);
    AF_INET6:
      Result :=
        (sin6_addr.S_un_w.s_w1=0) and (sin6_addr.S_un_w.s_w2=0) and
        (sin6_addr.S_un_w.s_w3=0) and (sin6_addr.S_un_w.s_w4=0) and
        (sin6_addr.S_un_w.s_w5=0) and (sin6_addr.S_un_w.s_w6=0) and
        (sin6_addr.S_un_w.s_w7=0) and (sin6_addr.S_un_w.s_w8=0);
  else
    Result := False;
  end
end;

function TSockAddr.GetAddrAsStr: string;
begin
  case sin_family of
    AF_INET:
      Result :=
        IntToStr(Ord(sin_addr.S_un_b.s_b1))+'.'+IntToStr(Ord(sin_addr.S_un_b.s_b2))+'.'+
        IntToStr(Ord(sin_addr.S_un_b.s_b3))+'.'+IntToStr(Ord(sin_addr.S_un_b.s_b4));
    AF_INET6:
      Result :=
        IntToHex(ntohs(sin6_addr.S_un_w.s_w1), 0)+':'+IntToHex(ntohs(sin6_addr.S_un_w.s_w2), 0)+':'+
        IntToHex(ntohs(sin6_addr.S_un_w.s_w3), 0)+':'+IntToHex(ntohs(sin6_addr.S_un_w.s_w4), 0)+':'+
        IntToHex(ntohs(sin6_addr.S_un_w.s_w5), 0)+':'+IntToHex(ntohs(sin6_addr.S_un_w.s_w6), 0)+':'+
        IntToHex(ntohs(sin6_addr.S_un_w.s_w7), 0)+':'+IntToHex(ntohs(sin6_addr.S_un_w.s_w8), 0);
  else
    Result := '';
  end
end;

function TSockAddr.GetAddrLocalHost: Boolean;
begin
  case sin_family of
    AF_INET:
      Result :=
        (sin_addr.S_un_b.s_b1 = 127) and (sin_addr.S_un_b.s_b2 = 0) and
        (sin_addr.S_un_b.s_b3 = 0) and (sin_addr.S_un_b.s_b4 = 1);
    AF_INET6:
      Result :=
        (sin6_addr.S_un_w.s_w1 = 0) and (sin6_addr.S_un_w.s_w2 = 0) and
        (sin6_addr.S_un_w.s_w3 = 0) and (sin6_addr.S_un_w.s_w4 = 0) and
        (sin6_addr.S_un_w.s_w5 = 0) and (sin6_addr.S_un_w.s_w6 = 0) and
        (sin6_addr.S_un_w.s_w7 = 0) and (sin6_addr.S_un_w.s_w8 = 1);
  else
    Result := False;
  end;
end;

function TSockAddr.GetFamily: u_short;
begin
  Result := sin_family;
end;

function TSockAddr.GetPort: u_short;
begin
  case sin_family of
    AF_INET:  Result := ntohs(sin_port);
    AF_INET6: Result := ntohs(sin6_port);
  else
    Result := 0;
  end;
end;

procedure TSockAddr.SetAddrAndPortStr(aValue: string);
var
  p: Integer;
begin
  // strip port part
  // get last :
  p := Length(aValue);
  while (p>1) and (aValue[p]<>':') and (aValue[p]<>']')
  do p := p-1;
  if aValue[p]=':' then
  begin
    Port := StrToInt(Copy(aValue, p+1, Length(aValue)-p));
    aValue := Copy(aValue, 1, p-1);
  end;
  // strip surrounding []
  if (aValue<>'') and (aValue[1]='[') and (aValue[Length(aValue)]=']')
  then AddrAsStr := Copy(aValue, 2, Length(aValue)-2)
  else AddrAsStr := aValue;
end;

procedure TSockAddr.SetAddrAny(const aValue: Boolean);
begin
  if aValue then
  begin
    case sin_family of
      AF_INET:
        FillChar(sin_addr, SizeOf(sin_addr), 0);
      AF_INET6:
        begin
          FillChar(sin6_addr, SizeOf(sin6_addr), 0);
          sin6_flowinfo := 0;
          sin6_scope_id := 0;
        end;
    end;
  end;
end;

// inet_pton only vista/windows 2008 and up
//procedure TSockAddr.SetAddrAsStr(const aValue: string);
//begin
//  case sin_family of
//    AF_INET:  inet_pton(sin_family, PAnsiChar(AnsiString(aValue)), @sin_addr);
//    AF_INET6: inet_pton(sin_family, PAnsiChar(AnsiString(aValue)), @sin6_addr);
//  end;
//end;

procedure TSockAddr.SetAddrAsStr(const aValue: string);
var
  aistart: PAddrInfoW;
  ai: PAddrInfoW;
begin
  aistart := nil;
  try
    if getaddrinfo(PChar(aValue), PChar(IntToStr(Port)), nil, aistart)=0 then
    begin
      // find first record that matches the family type
      ai := aistart;
      while Assigned(ai) and (ai.ai_family<>Family)
      do ai := ai.ai_next;
      if Assigned(ai)
      then Self := ai.ai_addr^;
    end;
  finally
    freeaddrinfo(aistart);
  end;
end;

procedure TSockAddr.SetAddrLocalHost(const aValue: Boolean);
begin
  if aValue then
  begin
    case sin_family of
      AF_INET:
        begin
          sin_addr.S_un_b.s_b1 := 127;
          sin_addr.S_un_b.s_b2 := 0;
          sin_addr.S_un_b.s_b3 := 0;
          sin_addr.S_un_b.s_b4 := 1;
        end;
      AF_INET6:
        begin
          FillChar(sin6_addr, SizeOf(sin6_addr), 0);
          sin6_addr.S_un_w.s_w8 := htons(1);
          sin6_flowinfo := 0;
          sin6_scope_id := 0;
        end;
    end;
  end;
end;

procedure TSockAddr.SetAllSameLinkNodesMulticast(aPort: u_Short);
begin
  sin_family := AF_INET6;
  sin_port := htons(aPort);
  FillChar(sin6_addr, SizeOf(sin6_addr), 0);
  sin6_addr.S_un_w.s_w1 := htons($FF02);
  sin6_addr.S_un_w.s_w8 := htons(1);
  sin6_flowinfo := 0;
  sin6_scope_id := 0;
end;

procedure TSockAddr.SetBroadcast(aPort: u_Short);
{
  IPv6: There are other ranges of addresses assigned, but you're unlikely to run into them.
  Note that in IPv6, there are no broadcast addresses.
  Where something like a broadcast is needed, multicasts are used instead.
  For instance, there's a "link-scope all-hosts multicast" address, ff02::1,
  which corresponds to the IPv4 subnet-local broadcast address, 255.255.255.255.
}
begin
  sin_family := AF_INET;
  sin_port := htons(aPort);
  sin_addr.S_addr := INADDR_BROADCAST;
end;

procedure TSockAddr.SetFamily(const aValue: u_short);
begin
  sin_family := aValue;
end;

procedure TSockAddr.SetPort(const aValue: u_short);
begin
  case sin_family of
    AF_INET:  sin_port := htons(aValue);
    AF_INET6: sin6_port := htons(aValue);
  end;
end;

{ TAddrInfo }

function TAddrInfoW.FamilyToStr: string;
begin
  Result := AddressFamilyToStr(ai_family);
end;

function TAddrInfoW.ProtocolToStr: string;
begin
  Result := AddressProtocolToStr(ai_protocol);
end;

function TAddrInfoW.SockTypeToStr: string;
begin
  Result := AddressSockTypeToStr(ai_socktype);
end;


{ rest functions }

function GetSocketAddresses(const aHost, aService: string): TSockAddresses;
var
  aistart: PAddrInfoW;
  ai: PAddrInfoW;
begin
  SetLength(Result, 0);
  aistart := nil;
  try
    if getaddrinfo(PChar(aHost), PChar(aService), nil, aistart)=0 then
    begin
      ai := aistart;
      repeat
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := ai.ai_addr^;
        ai := ai.ai_next;
      until not Assigned(ai);
    end;
  finally
    freeaddrinfo(aistart);
  end;
end;

procedure MyCloseSocket(var aSocket: TSocket);
begin
  if aSocket<>INVALID_SOCKET then
  begin
    try
      shutdown(aSocket, SD_BOTH);
    finally
      closesocket(aSocket);
    end;
    aSocket := INVALID_SOCKET;
  end;
end;

function SetSocketNoDelay(var aSocket: TSocket; aValue: Boolean): Boolean;
var
  OptVal: Integer;
begin
  if aValue
  then OptVal := 1
  else OptVal := 0;
  Result := setsockopt(aSocket, IPPROTO_TCP, TCP_NODELAY, PChar(@OptVal), SizeOf(OptVal))<>SOCKET_ERROR;
end;

function GetSocketNoDelay(var aSocket: TSocket; out aValue: Boolean): Boolean;
var
  OptVal: Integer;
  OptValLen: Integer;
begin
  OptValLen := SizeOf(OptVal);
  if getsockopt(aSocket, IPPROTO_TCP, TCP_NODELAY, PChar(@OptVal), OptValLen)=0 then
  begin
    aValue := OptVal<>0;
    Result := True;
  end
  else Result :=False;
end;

function SetSocketKeepAlive(var aSocket: TSocket; aValue: Boolean): Boolean;
var
  OptVal: Integer;
begin
  if aValue
  then OptVal := 1
  else OptVal := 0;
  Result := setsockopt(aSocket, SOL_SOCKET, SO_KEEPALIVE, PChar(@OptVal), SizeOf(OptVal))<>SOCKET_ERROR;
end;

function GetSocketKeepAlive(var aSocket: TSocket; out aValue: Boolean): Boolean;
var
  OptVal: Integer;
  OptValLen: Integer;
begin
  OptValLen := SizeOf(OptVal);
  if getsockopt(aSocket, SOL_SOCKET, SO_KEEPALIVE, PChar(@OptVal), OptValLen)=0 then
  begin
    aValue := OptVal<>0;
    Result := True;
  end
  else Result :=False;
end;

function GetSocketLinger(var aSocket: TSocket): Integer;
var
  LingerLen: Integer;
  Linger: TLinger;
begin
  LingerLen := SizeOf(Linger);
  if getsockopt(aSocket, SOL_SOCKET, SO_LINGER, PChar(@Linger), LingerLen)=0 then
  begin
    if Linger.l_onoff=0
    then Result := 0
    else Result := Linger.l_linger;
  end
  else Result := -1;
end;

function SetSocketLinger(var aSocket: TSocket; aValue: Integer): Boolean;
var
  Linger: TLinger;
begin
  //  = 0 -> dont linger
  // <> 0 -> linger for specified seconds
  Linger.l_onoff := aValue;
  Linger.l_linger := aValue;
  Result := setsockopt(aSocket, SOL_SOCKET, SO_LINGER, PChar(@Linger), SizeOf(Linger))<>SOCKET_ERROR;
end;

function AddressFamilies(aProtocol: Integer): TAddressFamilies;
// aProtocol can be IPPROTO_UDP, IPPROTO_TCP etc.
var
  Protocols: array[0..1] of Integer;
  ProtocolBuffer: array[0..100] of TWSAProtocol_Info;
  ProtocolBufferSize: DWORD;
  Res: Integer;
  p: Integer;
  q: Integer;
begin
  SetLength(Result, 0);
  ProtocolBufferSize := SizeOf(ProtocolBuffer);
  if aProtocol>=0 then
  begin
    Protocols[0] := aProtocol;
    Protocols[1] := 0;
    Res := WSAEnumProtocolsW(@Protocols[0], ProtocolBuffer, ProtocolBufferSize);
  end
  else Res := WSAEnumProtocolsW(nil, ProtocolBuffer, ProtocolBufferSize);
  if Res>=0 then
  begin
    for p := 0 to Res-1 do
    begin
      q := Length(Result)-1;
      while (q>=0) and (Result[q]<>ProtocolBuffer[p].iAddressFamily)
      do q := q-1;
      if q<0 then
      begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := ProtocolBuffer[p].iAddressFamily;
      end;
    end;
  end;
end;

{ TSelectSet }

procedure TSelectSet.Add(aSocket: TSocket);
begin
  // if empty reserve room for count
  if FCount=0
  then SetLength(FBuffer, SizeOf(FCount));
  // add socket to buffer
  SetLength(FBuffer, Length(FBuffer)+SizeOf(aSocket));
  Move(aSocket, FBuffer[Length(FBuffer)-SizeOf(aSocket)+1], SizeOf(aSocket));
  // increment count
  FCount := FCount+1;
  // update count in buffer
  Move(FCount, FBuffer[1], SizeOf(FCount));
end;

procedure TSelectSet.Clear;
begin
  FCount := 0;
  SetLength(FBuffer, 0);
end;

function TSelectSet.FDSet: PFDSet;
begin
  if FCount>0
  then Result := PFDSet(@FBuffer[1])
  else Result := nil;
end;


var
  WSAData: TWSAData;
initialization
  WSAStartup($0202, WSAData);
finalization
  WSACleanup;
end.
