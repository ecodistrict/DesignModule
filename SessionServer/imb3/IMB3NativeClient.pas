unit IMB3NativeClient;
{
     IMB 2 and earlier do not support hub side var prefixing
     Unique client id has to be known before it can be used to prefix a variable name
     Status updates use this feature and function different for imb 2 and 3
}
interface

{DEFINE IMBDebug}

uses
  SocksLib, IMB3Core, ByteBuffers,
  {$IFDEF IMBDebug}
  Logger,
  {$ENDIF}
  SyncObjs, Classes, Windows, SysUtils;

const
  DefaultRemoteHost                    = 'localhost';
  DefaultRemotePort                    = 4000;
  DefaultFederation                    = DefaultEventNameFilterPrefix; // use default demo filter for hub (if not validated)

  iceConnectionClosed                  = -1;
  iceNotEventPublished                 = -2;
  //iceConnectionWriteError              = -3;

  DefaultStreamBodyBuffer              = 16 * 1024;

  DefaultLingerTime                    = 2; // 2 seconds default sockete linger time (for writing waiting data before closing)

  ModelStatusVarName                   = 'ModelStatus';
  msVarSepChar                         = '|';

  FocusEventName                       = 'Focus';
  FederationChangeEventName            = 'META_CurrentSession';

  // imb 1 action (ChangeObject)
  actionNew                            = 0;
  actionDelete                         = 1;
  actionChange                         = 2;

  statusReady                          = 0;
  statusCalculating                    = 1;
  statusBusy                           = 2;

  StatusCodes                          : array[statusReady..statusBusy] of Char = ('R', 'C', 'B');

  // wait for 5 seconds for unique client id if requested
  UniqueClientIDSpinCount              = 10;
  UniqueClientIDSpinWait               = 500;

type
  PMethod = ^TMethod;

  TEventHandlerList<T, T2> = class // T must be method, T2 must be function pointer version of T
  constructor Create;
  destructor Destroy; override;
  type
    TEventHandlerListEntries = array of T;
  private
    FEntries: TEventHandlerListEntries;
    FLock: TCriticalSection;
    function IsSameHandlerT(aHandler: T; aIndex: Integer): Boolean;
    function IsSameHandlerT2(aHandler: T2; aSelf: Pointer; aIndex: Integer): Boolean;
    function IsSameHandlerS(aSelf: TObject; aIndex: Integer): Boolean;
  public
    function AddHandler(aHandler: T): Integer; overload;
    function AddHandler(aHandler: T2; aSelf: Pointer): Integer; overload;
    function RemoveHandler(aSelf: TObject): Integer; overload;
    function RemoveHandler(aHandler: T): Integer; overload;
    function RemoveHandler(aHandler: T2; aSelf: Pointer): Integer; overload;
    function Clear: Integer;
    function IsEmpty: Boolean;
    property Entries: TEventHandlerListEntries read FEntries;
    property Lock: TCriticalSection read FLock;
  end;

  TMyThread = class; // forward declaration

  ThreadedMethod = procedure(aThread: TMyThread) of object;

  TMyThread = class(TThread)
  constructor Create(aMethod: ThreadedMethod);
  private
    FMethod: ThreadedMethod;
  protected
    procedure Execute; override;
  public
    property Terminated;
    procedure Synchronize(AMethod: TThreadMethod); overload;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload;
  end;

  TIMBEventNameEntry = record
    EventName                          : string;
    Publishers                         : Integer;
    Subscribers                        : Integer;
    Timers                             : Integer;
  end;

  TIMBEventNames                       = array of TIMBEventNameEntry;

  TIMBEventEntry                       = class; // forward declaration
  TIMBConnection                       = class; // forward declaration

  TOnVariable                          = procedure(aConnection: TIMBConnection; const aVarName: string; const aVarValue: AnsiString) of object; stdcall;
  TOnVariableNoObj                     = procedure(aSelf: Pointer; aConnection: TIMBConnection; const aVarName: string; const aVarValue: AnsiString); stdcall;

  TOnStatusUpdate                      = procedure(aConnection: TIMBConnection; const aModelUniqueClientID, aModelName: string; aProgress, aStatus: Integer) of object; stdcall;
  TOnStatusUpdateNoObj                 = procedure(aSelf: Pointer; aConnection: TIMBConnection; const aModelUniqueClientID, aModelName: string; aProgress, aStatus: Integer); stdcall;

  TOnDisconnect                        = procedure(aConnection: TIMBConnection) of object; stdcall;
  TOnDisconnectNoObj                   = procedure(aSelf: Pointer; aConection: TIMBConnection); stdcall;

  TOnSocketError                       = procedure(aConnection: TIMBConnection; var aSocket: TSocket; aOnWrite: Boolean) of object; stdcall;
  TOnSocketErrorNoObj                  = procedure(aSelf: Pointer; aConnection: TIMBConnection; var aSocket: TSocket; aOnWrite: Boolean); stdcall;

  TOnException                         = procedure(aConnection: TIMBConnection; aException: Exception; var aCallClose: Boolean) of object; stdcall;
  TOnExceptionNoObj                    = procedure(aSelf: Pointer; aConnection: TIMBConnection; aException: Exception; var aCallClose: Boolean); stdcall;

  TOnLog                               = procedure(aEventEntry: TIMBEventEntry; aUniqueClientID: Integer; const aLogLine: string; aLogLevel: Integer) of object; stdcall;
  TOnLogNoObj                          = procedure(aSelf: Pointer; aEventEntry: TIMBEventEntry; aUniqueClientID: Integer; const aLogLine: string; aLogLevel: Integer); stdcall;

  TOnEventNames                        = procedure(aConnection: TIMBConnection; aEventNames: TIMBEventNames) of object; stdcall;
  TOnEventNamesNoObj                   = procedure(aSelf: Pointer; aConnection: TIMBConnection; aEventNames: TIMBEventNames); stdcall;

  TOnSubAndPubEvent                    = procedure(aEvent: TIMBEventEntry; aCommand: Integer; const aEventName: string; aIsChildEvent: Boolean) of object; stdcall;
  TOnSubAndPubEventNoObj               = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; aCommand: Integer; const aEventName: string; aIsChildEvent: Boolean); stdcall;

  TOnChangeFederation                  = procedure(aConnection: TIMBConnection; aNewFederationID: Integer; const aNewFederation: string) of object; stdcall;
  TOnChangeFederationNoObj             = procedure(aSelf: Pointer; aConnection: TIMBConnection; aNewFederationID: Integer; const aNewFederation: string); stdcall;

  TOnFocus                             = procedure(aX, aY: Double) of object; stdcall;
  TOnFocusNoObj                        = procedure(aSelf: Pointer; aX, aY: Double); stdcall;

  TOnNormalEvent                       = procedure(aEvent: TIMBEventEntry; var aPayload: TByteBuffer) of object; stdcall;
  TOnNormalEventNoObj                  = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
  TOnChangeObject                      = procedure(aAction, aObjectID: Integer; const aObjectName, aAttribute: string) of object; stdcall;
  TOnChangeObjectNoObj                 = procedure(aSelf: Pointer; aAction, aObjectID: Integer; const aObjectName, aAttribute: string); stdcall;

  TOnBuffer                            = procedure(aEvent: TIMBEventEntry; aTick: Integer; aBufferID: Integer; var aBuffer: TByteBuffer) of object; stdcall;
  TOnBufferNoObj                       = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; aTick: Integer; aBufferID: Integer; var aBuffer: TByteBuffer); stdcall;
  TOnOtherEvent                        = procedure(aEvent: TIMBEventEntry; aTick: Integer; aEventKind: TIMBEventKind; var aPayload: TByteBuffer) of object; stdcall;
  TOnOtherEventNoObj                   = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; aTick: Integer; aEventKind: TIMBEventKind; var aPayload: TByteBuffer); stdcall;

  TOnStreamCreate                      = function(aEvent: TIMBEventEntry; const aStreamName: string): TStream of object; stdcall;
  TOnStreamCreateNoObj                 = function(aSelf: Pointer; aEvent: TIMBEventEntry; const aStreamName: string): TStream; stdcall;
  TOnStreamEnd                         = procedure(aEvent: TIMBEventEntry; var aStream: TStream; const aStreamName: string) of object; stdcall;
  TOnStreamEndNoObj                    = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; var aStream: TStream; const aStreamName: string); stdcall;

  TOnTimerTick                         = procedure(aEvent: TIMBEventEntry; const aTimerName: string; aTick: Integer; aTickTime, aStartTime: Int64) of object; stdcall;
  TOnTimerTickNoObj                    = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; const aTimerName: string; aTick: Integer; aTickTime, aStartTime: Int64); stdcall;

  TOnTimerCmd                          = procedure(aEvent: TIMBEventEntry; aEventKind: TIMBEventKind; const aTimerName: string) of object; stdcall;
  TOnTimerCmdNoObj                     = procedure(aSelf: Pointer; aEvent: TIMBEventEntry; aEventKind: TIMBEventKind; const aTimerName: string); stdcall;

  TIMBEventEntry = class
  constructor Create                   ( aConnection: TIMBConnection; aID: Integer; const aEventName: string);
  destructor  Destroy; override;
  type
    TStreamCacheEntry = record
      StreamID                         : Integer;
      Stream                           : TStream;
      StreamName                       : string;
    end;
    TStreamCache = record
    private
      FCache                           : array of TStreamCacheEntry;
      function  IndexOfStreamID        ( aStreamID: Integer): Integer;
      function  IndexOfStream          ( aStream: TStream): Integer;
    public
      function  Cache                  ( aStreamID: Integer; aStream: TStream; const aStreamName: string): Integer;
      function  Remove                 ( aStreamID: Integer): Integer;
      function  Find                   ( aStreamID: Integer; out aStreamName: string): TStream; overload;
      function  Find                   ( aStream: TStream; out aStreamName: string): Integer; overload;
      procedure Clear                  ( aFreeStream: Boolean = True);
    end;
  private
    FConnection                        : TIMBConnection;
    FID                                : Integer;
    FEventName                         : string;
    FParent                            : TIMBEventEntry;
    FIsPublished                       : Integer;
    FPublishers                        : Boolean;
    FIsSubscribed                      : Integer;
    FSubscribers                       : Boolean;
    FStreamCache                       : TStreamCache;
    // imb 1
    FOnMultipleChangeObject            : TEventHandlerList<TOnChangeObject, TOnChangeObjectNoObj>;
    FOnFocus                           : TOnFocus;
    // imb 2
    FOnMultipleNormalEvent             : TEventHandlerList<TOnNormalEvent, TOnNormalEventNoObj>;
    FOnMultipleBuffer                  : TEventHandlerList<TOnBuffer, TOnBufferNoObj>;
    FOnStreamCreate                    : TOnStreamCreate;
    FOnStreamEnd                       : TOnStreamEnd;
    FOnMultipleChangeFederation        : TEventHandlerList<TOnChangeFederation, TOnChangeFederationNoObj>;
    // imb 3
    FOnMultipleOtherEvent              : TEventHandlerList<TOnOtherEvent, TOnOtherEventNoObj>;
    FOnMultipleTimerTick               : TEventHandlerList<TOnTimerTick, TOnTimerTickNoObj>;
    FOnMultipleTimerCmd                : TEventHandlerList<TOnTimerCmd, TOnTimerCmdNoObj>;
    FOnMultipleSubAndPub               : TEventHandlerList<TOnSubAndPubEvent, TOnSubAndPubEventNoObj>;
  protected
    function  TimerBasicCmd            ( aEventKind: TIMBEventKind; const aTimerName: string): Integer;
    function  TimerAcknowledgeCmd      ( aEventKind: TIMBEventKind; const aTimerName, aClientName: string): Integer;
    procedure SignalSubscribe;
    procedure SignalPublish;
    procedure SignalUnSubscribe             ( aChangeLocalState: Boolean=True);
    procedure SignalUnPublish;
    function  IsEmpty                  : Boolean;
    function  GetShortEventName        : string;
    // dispatcher for all events
    procedure HandleEvent              ( var aPayload: TByteBuffer);
    // dispatchers for specific events
    procedure HandleChangeObject       ( var aPayload: TByteBuffer);
    procedure HandleNormalEvent        ( var aPayload: TByteBuffer);
    procedure HandleBuffer             ( aEventTick: Integer; var aPayload: TByteBuffer);
    procedure HandleOtherEvent         ( aEventTick: Integer; aEventKind: TIMBEventKind; var aPayload: TByteBuffer);
    procedure HandleTimerTick          ( var aPayload: TByteBuffer);
    procedure HandleTimerCmd           ( aEventKind: TIMBEventKind; var aPayload: TByteBuffer);
    procedure HandleSubAndPub          ( aCommand: Integer; const aEventName: string; aIsChild: Boolean);
    procedure HandleStreamEvent        ( aEventKind: TIMBEventKind; var aPayload: TByteBuffer);
    procedure HandleLogWriteLn         ( var aPayload: TByteBuffer);
  private
    procedure SetOnChangeObject        ( const aValue: TOnChangeObject);
    procedure SetOnBuffer              ( const aValue: TOnBuffer);
    procedure SetOnNormalEvent         ( const aValue: TOnNormalEvent);
    procedure SetOnOtherEvent          ( const aValue: TOnOtherEvent);
    procedure SetOnStreamCreate        ( const aValue: TOnStreamCreate);
    procedure SetOnStreamEnd           ( const aValue: TOnStreamEnd);
    procedure SetOnSubAndPub           ( const aValue: TOnSubAndPubEvent);
    procedure SetOnTimerCmd            ( const aValue: TOnTimerCmd);
    procedure SetOnTimerTick           ( const aValue: TOnTimerTick);
    // alternative handlers real functions no methods
    procedure SetOnChangeObjectNoObj   ( aSelf: Pointer; aValue: TOnChangeObjectNoObj);
    procedure SetOnNormalEventNoObj    ( aSelf: Pointer; aValue: TOnNormalEventNoObj);
    procedure SetOnBufferNoObj         ( aSelf: Pointer; aValue: TOnBufferNoObj);
    procedure SetOnOtherEventNoObj     ( aSelf: Pointer; aValue: TOnOtherEventNoObj);
    procedure SetOnStreamCreateNoObj   ( aSelf: Pointer; aValue: TOnStreamCreateNoObj);
    procedure SetOnStreamEndNoObj      ( aSelf: Pointer; aValue: TOnStreamEndNoObj);
    procedure SetOnTimerTickNoObj      ( aSelf: Pointer; aValue: TOnTimerTickNoObj);
    procedure SetOnTimerCmdNoObj       ( aSelf: Pointer; aValue: TOnTimerCmdNoObj);
    procedure SetOnSubAndPubNoObj      ( aSelf: Pointer; const aValue: TOnSubAndPubEventNoObj);
    function  GetIsPublished           : Boolean;
    function  GetIsSubscribed          : Boolean;
  public
    // event info
    property  Connection               : TIMBConnection read FConnection;
    property  ID                       : Integer read FID;
    property  EventName                : string read FEventName;
    property  IsPublished              : Boolean read GetIsPublished;
    property  IsSubscribed             : Boolean read GetIsSubscribed;
    property  Publishers               : Boolean read FPublishers;
    property  Subscribers              : Boolean read FSubscribers;
    property  ShortEventName           : string read GetShortEventName;
    function  IsOnFederation           ( const aFederation: string): Boolean;
    procedure CopyHandlersFrom         ( aEventEntry: TIMBEventEntry);
    // sub and pub
    procedure Subscribe;
    procedure Publish;
    procedure UnSubscribe;
    procedure UnPublish;
    // signals (send events)
    function  SignalChangeObject       ( aAction, aObjectID: Integer; const aAttribute: string=''): Integer;
    function  SignalBuffer             ( aBufferID: Integer; aBuffer: Pointer; aBufferLen: Integer; aEventFlags: Integer=0): Integer;
    function  SignalEvent              ( aEventKind: TIMBEventKind; const aEventPayload: TByteBuffer): Integer;
    function  SignalStream             ( const aStreamName: string; aStream: TStream; aStreamBodyBuffer: Integer): Integer;
    // event handlers
    property  OnChangeObject           : TOnChangeObject write SetOnChangeObject;
    property  OnBuffer                 : TOnBuffer write SetOnBuffer;
    property  OnNormalEvent            : TOnNormalEvent write SetOnNormalEvent;
    property  OnOtherEvent             : TOnOtherEvent write SetOnOtherEvent;
    property  OnStreamCreate           : TOnStreamCreate read FOnStreamCreate write SetOnStreamCreate;
    property  OnStreamEnd              : TOnStreamEnd read FOnStreamEnd write SetOnStreamEnd;
    property  OnTimerTick              : TOnTimerTick write SetOnTimerTick;
    property  OnTimerCmd               : TOnTimerCmd write SetOnTimerCmd;
    property  OnSubAndPub              : TOnSubAndPubEvent write SetOnSubAndPub;
    procedure RemoveHandlers           ( aObject: TObject);
    procedure RemoveAllHandlers;
    function  NoHandlers               : Boolean;
    // alternative event handlers that are not methods
    property  OnChangeObjectNoObj      [ aSelf: Pointer]: TOnChangeObjectNoObj write SetOnChangeObjectNoObj;
    property  OnBufferNoObj            [ aSelf: Pointer]: TOnBufferNoObj write SetOnBufferNoObj;
    property  OnNormalEventNoObj       [ aSelf: Pointer]: TOnNormalEventNoObj write SetOnNormalEventNoObj;
    property  OnOtherEventNoObj        [ aSelf: Pointer]: TOnOtherEventNoObj write SetOnOtherEventNoObj;
    property  OnStreamCreateNoObj      [ aSelf: Pointer]: TOnStreamCreateNoObj write SetOnStreamCreateNoObj;
    property  OnStreamEndNoObj         [ aSelf: Pointer]: TOnStreamEndNoObj write SetOnStreamEndNoObj;
    property  OnTimerTickNoObj         [ aSelf: Pointer]: TOnTimerTickNoObj write SetOnTimerTickNoObj;
    property  OnTimerCmdNoObj          [ aSelf: Pointer]: TOnTimerCmdNoObj write SetOnTimerCmdNoObj;
    property  OnSubAndPubNoObj         [ aSelf: Pointer]: TOnSubAndPubEventNoObj write SetOnSubAndPubNoObj;
    // timers
    function  TimerCreate              ( const aTimerName: string; aStartTimeUTCorRelFT: Int64; aResolutionms: Integer; aSpeedFactor: Double; aRepeatCount:Integer=trcInfinite): Integer;
    function  TimerCancel              ( const aTimerName: string): Integer;
    function  TimerPrepare             ( const aTimerName: string): Integer;
    function  TimerStart               ( const aTimerName: string): Integer;
    function  TimerStop                ( const aTimerName: string): Integer;
    function  TimerSetSpeed            ( const aTimerName: string; aSpeedFactor: Double): Integer;
    function  TimerAcknowledgeAdd      ( const aTimerName, aClientName: string): Integer;
    function  TimerAcknowledgeRemove   ( const aTimerName, aClientName: string): Integer;
    function  TimerAcknowledge         ( const aTimerName, aClientName: string; aProposedTimeStep: Integer): Integer;
    // log
    function  LogWriteLn               ( const aLine: string; aLevel: Integer): Integer;
    // streams
    procedure ClearAllStreams;
  end;

  ///	<summary>
  ///	  Connection to a IMB HUB
  ///	</summary>
  TIMBConnection = class
  ///	<summary>
  ///	  Create a IMB connection to a HUB
  ///	</summary>
  ///	<param name="aRemoteHost">
  ///	  ip address or dns name of the HUB to connect to
  ///	</param>
  ///	<param name="aRemotePort">
  ///	  TCP port of the hub to connect to
  ///	</param>
  ///	<param name="aOwnerName">
  ///	  optional description of the client that connects
  ///	</param>
  ///	<param name="aOwnerID">
  ///	  optional id that describes the client that connects
  ///	</param>
  ///	<param name="aFederation">
  ///	  federation to connect to
  ///	</param>
  constructor Create                   ( const aRemoteHost: string; aRemotePort: Integer;
                                         const aOwnerName: string; aOwnerID: Integer;
                                         const aFederation: string=DefaultFederation;
                                         aUseReaderThread: Boolean=True);
  destructor  Destroy; override;
  type
    TEventList = class(TList)
    private
      function  GetEventEntry          ( aIndex: Integer): TIMBEventEntry;
    protected
      procedure Notify                 ( Ptr: Pointer; Action: TListNotification); override;
    public
      property  EventEntry             [ aIndex: Integer]: TIMBEventEntry read GetEventEntry; default;
      function  AddEvent               ( aConnection: TIMBConnection; const aEventName: string): TIMBEventEntry;
      function  IndexOfEventName       ( const aEventName: string): Integer;
      function  EventEntryOnName       ( const aEventName: string): TIMBEventEntry;
    end;
  private
    // connection
    FRemoteHost                        : string;
    FRemotePort                        : Integer;
    FAddressFamily                     : Integer;
    FSocket                            : TSocket;
    FNoDelay                           : Boolean;
    FKeepAlive                         : Boolean;
    FLingerTime                        : Integer;
    FOnDisconnect                      : TOnDisconnect;
    FOnSocketError                     : TOnSocketError;
    // events
    FEventList                         : TEventList;
    FEventListLock                     : TCriticalSection;
    FFederation                        : string;
    FEventTranslation                  : TIMBEventTranslation;
    // reader
    FUseReaderThread                   : Boolean;
    FReaderThread                      : TThread;
    FReadCommandBuffer                 : AnsiString;
    // writer
    FWriteLock                         : TCriticalSection;
    // time
    FBrokerAbsoluteTime                : Int64;
    FBrokerTick                        : Integer;
    FBrokerTickDelta                   : Integer;
    // owner and client
    FUniqueClientID                    : Integer;
    FClientHandle                      : Integer;
    FOwnerName                         : string;
    FOwnerID                           : Integer;
    // variables, status updates and event names
    FOnVariable                        : TOnVariable;
    FOnStatusUpdate                    : TOnStatusUpdate;
    FOnEventNames                      : TOnEventNames;
    FOnException                       : TOnException;
    FOnLog                             : TOnLog;
    // standard event references
    FFocusEvent                        : TIMBEventEntry;
    FChangeFederationEvent             : TIMBEventEntry;
    FLogEvent                          : TIMBEventEntry;
    // other
    FReInitStatus                      : Boolean; // to be used when implementing reconnect
    FIMB2Compatible                    : Boolean;
    FOldWindows                        : Boolean;
    function  GetConnected             : Boolean;
    procedure SetConnected             ( const aValue: Boolean);
    procedure SetFederation            ( const aValue: string);
    procedure SetNoDelay               ( const aValue: Boolean);
    procedure SetKeepAlive             ( const aValue: Boolean);
    function  GetLinger                : Integer;
    procedure SetLinger                ( aValue: Integer);
    procedure SetOnVariable            ( aValue: TOnVariable);
    procedure SetOnStatusUpdate        ( aValue: TOnStatusUpdate);
    procedure SetOnFocus               ( aValue: TOnFocus);
    procedure SetOnChangeFederation    ( aValue: TOnChangeFederation);

    procedure SetOnDisconnectNoObj     ( aSelf: Pointer; aValue: TOnDisconnectNoObj);
    procedure SetOnSocketErrorNoObj    ( aSelf: Pointer; aValue: TOnSocketErrorNoObj);
    procedure SetOnFocusNoObj          ( aSelf: Pointer; aValue: TOnFocusNoObj);
    procedure SetOnVariableNoObj       ( aSelf: Pointer; aValue: TOnVariableNoObj);
    procedure SetOnStatusUpdateNoObj   ( aSelf: Pointer; const aValue: TOnStatusUpdateNoObj);
    procedure SetOnEventNamesNoObj     ( aSelf: Pointer; aValue: TOnEventNamesNoObj);
    procedure SetOnChangeFederationNoObj(aSelf: Pointer; aValue: TOnChangeFederationNoObj);
    procedure SetOnExceptionNoObj      ( aSelf: Pointer; aValue: TOnExceptionNoObj);
    procedure SetOnLogNoObj            ( aSelf: Pointer; aValue: TOnLogNoObj);

    function  SetOwner                 : Integer;
    procedure SetOwnerID               ( const aValue: Integer);
    procedure SetOwnerName             ( const aValue: string);
  protected
    function  AddEvent                 ( const aEventName: string): TIMBEventEntry;
    function  AddEventL                ( const aEventName: string): TIMBEventEntry;
    function  EventIDToEventL          ( aEventID: Integer): TIMBEventEntry;
    function  FindOrAddEventL          ( const aEventName: string): TIMBEventEntry;
    function  FindEventL               ( const aEventName: string): TIMBEventEntry;
    function  FindEventAutoPublishL    ( const aEventName: string): TIMBEventEntry;
    function  FindEventParentL         ( const aEventName: string): TIMBEventEntry;
    procedure ReadCommandsByThread     ( aThread: TMyThread);
    //function  InternalRecv             ( buf: PAnsiChar; len: Integer): Integer;
    function  ReadCommand              ( out aCommand: Integer; var aPayload: TByteBuffer): Boolean;
    function  ReadCommandOlderWindows  ( out aCommand: Integer; var aPayload: TByteBuffer): Boolean;
    function  WriteCommand             ( aCommand: Integer; const aPayload: TByteBuffer): Integer;
    // command handlers
    procedure HandleCommand            ( aCommand: Integer; var aPayload: TByteBuffer);
    procedure HandleCommandEvent       ( var aPayload: TByteBuffer);
    procedure HandleCommandVariable    ( var aPayload: TByteBuffer);
    procedure HandleEventNames         ( var aPayload: TByteBuffer);
    procedure HandleSubAndPub          ( aCommand: Integer; var aPayload: TByteBuffer);
    procedure HandleCommandOther       ( aCommand: Integer; var aPayload: TByteBuffer); virtual;
    function  RequestUniqueClientID    : Integer;
    function  PrefixFederation         ( const aEventName: string; aUseFederationPrefix: Boolean=True): string;
    function  GetUniqueClientID        : Integer;
  public
    // general
    ///	<summary>
    ///	  The currently selected federation
    ///	</summary>
    ///	<value>
    ///	  a new value for the federation
    ///	</value>
    ///	<remarks>
    ///	  The federation acts as a filter on event on an IMB HUB. The
    ///	  federation is default prefixed to event names. When changing the
    ///	  federation all event names that were prefixed with it are renamed,
    ///	  and if subscribed and/or published they are re-subscribed and/or
    ///	  republished with the new prefix.
    ///	</remarks>
    property  Federation               : string read FFederation write SetFederation;

    ///	<summary>
    ///	  When enabled makes sure that the client tries to act as an IMB 2
    ///	  client
    ///	</summary>
    ///	<value>
    ///	  when true enables IMB2 compatibility mode
    ///	</value>
    ///	<remarks>
    ///	  Features remote prefixed var names are not used. When set tries to
    ///	  act as an IMB 2 client
    ///	</remarks>
    property  IMB2Compatible           : Boolean read FIMB2Compatible write FIMB2Compatible;

    ///	<summary>
    ///	  The ip address or dns name of the connected hub
    ///	</summary>
    ///	<value>
    ///	  ip address or dns name of the hub
    ///	</value>
    property  RemoteHost               : string read FRemoteHost;

    ///	<summary>
    ///	  the tcp port of the connected hub
    ///	</summary>
    ///	<value>
    ///	  port number of the connected hub
    ///	</value>
    property  RemotePort               : integer read FRemotePort;

    ///	<summary>
    ///	  When set disables the nagle algorithm
    ///	</summary>
    property  NoDelay                  : Boolean read FNoDelay write SetNoDelay;

    ///	<summary>
    ///	  When set enables sending keep alive packets
    ///	</summary>
    property  KeepAlive                : Boolean read FKeepAlive write SetKeepAlive;

    ///	<summary>
    ///	  When set with value 0 the linger socket options is turned off. When
    ///	  set to a positive number the linger socket linger options is enabled
    ///	  for that number of seconds.
    ///	</summary>
    ///	<remarks>
    ///	  The socket linger option makes the socket 'linger' around when a the
    ///	  socket is closed to pass all data still buffered.
    ///	</remarks>
    property  Linger                   : Integer read GetLinger write SetLinger;

    ///	<summary>
    ///	  returns if the connection is connected to a hub
    ///	</summary>
    property  Connected                : Boolean read GetConnected write SetConnected;

    ///	<summary>
    ///	  Opens the connection defined by the RemoteHost, RemotePort,
    ///	  Federation, ModelName and ModelID
    ///	</summary>
    ///	<returns>
    ///	  connection state
    ///	</returns>
    function  Open                     ( const aRemoteHost: string; aRemotePort: Integer): Boolean;

    ///	<summary>
    ///	  Closes a connected connection
    ///	</summary>
    ///	<returns>
    ///	  returns true if the close attempt was succesfull
    ///	</returns>
    function  Close                    : Boolean;
    property  OnDisconnect             : TOnDisconnect read FOnDisconnect write FOnDisconnect;
    property  OnDisconnectNoObj        [ aSelf: Pointer]: TOnDisconnectNoObj write SetOnDisconnectNoObj;
    property  OnSocketError            : TOnSocketError read FOnSocketError write FOnSocketError;
    property  OnSocketErrorNoObj       [ aSelf: Pointer] : TOnSocketErrorNoObj write SetOnSocketErrorNoObj;
    property  OnException              : TOnException read FOnException write FOnException;
    property  OnExceptionNoObj         [ aSelf: Pointer]: TOnExceptionNoObj write SetOnExceptionNoObj;
    property  OnLog                    : TOnLog read FOnLog write FOnLog;
    property  OnLogObj                 [ aSelf: Pointer]: TOnLogNoObj write SetOnLogNoObj;
    procedure SetThrottle              ( aThrottle: Integer);
    procedure SetState                 ( aState: TIMBConnectionState);
    // use ReadCommands if no reader thread is used
    property  ReaderThread             : TThread read FReaderThread;
    procedure Synchronize              ( aMethod: TThreadMethod); overload;
    procedure Synchronize              ( aThreadProc: TThreadProcedure); overload;
    property  UseReaderThread          : Boolean read FUseReaderThread write FUseReaderThread; // defaults to True
    function  ReadCommands             ( aMaxCommands: Integer; aTimeOut: Integer): Integer;
    // owner
    property  OwnerID                  : Integer read FOwnerID write SetOwnerID;
    property  OwnerName                : string read FOwnerName write SetOwnerName;
    // id
    property  UniqueClientID           : Integer read GetUniqueClientID;
    property  ClientHandle             : Integer read FClientHandle;
    // publish/subscribe
    function  Publish                  ( const aEventName: string; aUseFederationPrefix: Boolean=True): TIMBEventEntry;
    function  Subscribe                ( const aEventName: string; aUseFederationPrefix: Boolean=True): TIMBEventEntry; overload;
    // do not check number of handlers
    function  UnPublish                ( const aEventName: string; aUseFederationPrefix: Boolean=True): TIMBEventEntry;
    function  UnSubscribe              ( const aEventName: string; aUseFederationPrefix: Boolean=True): TIMBEventEntry; overload;
    // take other handlers into account
    function  Subscribe                ( const aEventName: string; aHandler: TOnNormalEvent): TIMBEventEntry; overload;
    function  Subscribe                ( const aEventName: string; aHandler: TOnNormalEventNoObj; aSelf: Pointer=nil): TIMBEventEntry; overload;
    function  Subscribe                ( const aEventName: string; aHandler: TOnChangeObject): TIMBEventEntry; overload;
    function  Subscribe                ( const aEventName: string; aHandler: TOnChangeObjectNoObj; aSelf: Pointer=nil): TIMBEventEntry; overload;
    // take other handlers into account
    function  UnSubscribe              ( const aEventName: string; aHandler: TOnNormalEvent): TIMBEventEntry; overload;
    function  UnSubscribe              ( const aEventName: string; aHandler: TOnNormalEventNoObj; aSelf: Pointer=nil): TIMBEventEntry; overload;
    function  UnSubscribe              ( const aEventName: string; aHandler: TOnChangeObject): TIMBEventEntry; overload;
    function  UnSubscribe              ( const aEventName: string; aHandler: TOnChangeObjectNoObj; aSelf: Pointer=nil): TIMBEventEntry; overload;
    // remove specific handler object from event entries (all handlers that are methods of the given object ie all references to the specified object)
    procedure RemoveHandlers           ( aObject: TObject);
    // signal with event name lookup or on id: deprecated, use TIMBEvent instead because of performance overhead looking up event entry on every call
    function  SignalEvent              ( const aEventName: string; aEventKind: TIMBEventKind; const aEventPayload: TByteBuffer; aUseFederationPrefix: Boolean=True): Integer;
    function  SignalBuffer             ( const aEventName: string; aBufferID: Integer; aBuffer: Pointer; aBufferLen: Integer; aEventFlags: Integer=0; aUseFederationPrefix: Boolean=True): Integer;
    function  SignalChangeObject       ( const aEventName: string; aAction, aObjectID: Integer; const aAttribute: string=''; aUseFederationPrefix: Boolean=True): Integer;
    function  SignalStream             ( const aEventName: string; const aStreamName: string; aStream: TStream; aUseFederationPrefix: Boolean=True; aStreamBodyBuffer: Integer=DefaultStreamBodyBuffer): Integer;
    // variables
    property  OnVariable               : TOnVariable read FOnVariable write SetOnVariable;
    property  OnVariableNoObj          [ aSelf: Pointer]: TOnVariableNoObj write SetOnVariableNoObj;
    procedure SetVariableValue         ( const aVarName: string; const aVarValue: AnsiString);
    procedure SetVariableValuePrefix   ( const aVarName: string; const aVarValue: AnsiString; aVarPrefix: TIMBVarPrefix);
    // imb 1 status
    class function IsStatusVarName     ( const aVarName: string): Boolean;
    class function StripStatusVarName  ( const aVarName: string): string;
    class procedure DecodeStatus       ( const aVarValue: AnsiString; out aProgress, aStatus: Integer);
    property  OnStatusUpdate           : TOnStatusUpdate read FOnStatusUpdate write SetOnStatusUpdate;
    property  OnStatusUpdateNoObj      [ aSelf: Pointer]: TOnStatusUpdateNoObj write SetOnStatusUpdateNoObj;
    procedure UpdateStatus             ( aProgress, aStatus: Integer);
    procedure RemoveStatus;
    // imb 1 focus
    property  OnFocus                  : TOnFocus write SetOnFocus;
    property  OnFocusNoObj             [ aSelf: Pointer]: TOnFocusNoObj write SetOnFocusNoObj;
    function  SignalFocus              ( const aX, aY: Double): Integer;
    // imb 2 change federation
    property  OnChangeFederation       : TOnChangeFederation write SetOnChangeFederation;
    property  OnChangeFederationNoObj  [ aSelf: Pointer]: TOnChangeFederationNoObj write SetOnChangeFederationNoObj;
    function  SignalChangeFederation   ( aNewFederationID: Integer; const aNewFederation: string): Integer;
    // log
    function  LogWriteLn               ( const aLogEventName, aLine: string; aLevel: Integer): Integer;
    // remote event info
    property  OnEventNames             : TOnEventNames read FOnEventNames write FOnEventNames;
    property  OnEventNamesNoObj        [ aSelf: Pointer]: TOnEventNamesNoObj write SetOnEventNamesNoObj;
    function  RequestEventNames        ( const aEventNameFilter: string=''; aEventFilters: TEventFilters=[]): Integer;
  end;

implementation

{ utils }

function Left(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s)
    then Result := s
    else Result := Copy(s, 1, n);
  end
  else Result := '';
end;

function Right(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s)
    then Result := s
    else Result := Copy(s, Length(s) - n + 1, n);
  end
  else Result := '';
end;

{$R-}{$Q-}
function CheckSum(const s: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Length(s)
  do Result := Result+Byte(s[i]); // no checking because of over flow in result (wrap)
end;
{$R+}{$Q+}

function StartsWith(const s, LeftStr: string): Boolean;
begin
  Result := AnsiCompareText(Left(s, Length(LeftStr)), LeftStr) = 0;
end;

function EndsWith(const s, RightStr: string): Boolean;
begin
  Result := AnsiCompareText(Right(s, Length(RightStr)), RightStr) = 0;
end;

function EndStripCnt(const s: string; aCnt: Integer): string;
begin
  Result := Left(s, Length(s) - aCnt);
end;

function EndStrip(const s, RightStr: string): string;
begin
  if EndsWith(s, RightStr)
  then Result := EndStripCnt(s, Length(RightStr))
  else Result := s;
end;


{ TEventHandlerList<T, T2> }

function TEventHandlerList<T, T2>.AddHandler(aHandler: T): Integer;
var
  e: Integer;
  l: Integer;
begin
  FLock.Acquire;
  try
    l := Length(FEntries);
    e := l-1;
    while (e>=0) and (@FEntries[e]<>@aHandler)
    do e := e-1;
    if e<0 then
    begin
      SetLength(FEntries, l+1);
      FEntries[l] := aHandler;
      Result := 1;
    end
    else Result := 0;
  finally
    FLock.Release;
  end;
end;

function TEventHandlerList<T, T2>.AddHandler(aHandler: T2; aSelf: Pointer): Integer;
var
  m: T;
begin
  // recalc eventhandler and self to method "outsmarting" compiler :-(
  PMethod(@m).Code := Pointer((@aHandler)^); //
  PMethod(@m).Data := aSelf;
  Result := AddHandler(m);
end;

function TEventHandlerList<T, T2>.Clear: Integer;
begin
  Result := Length(FEntries);
  FLock.Acquire;
  try
    SetLength(FEntries, 0);
  finally
    FLock.Release;
  end;
end;

constructor TEventHandlerList<T, T2>.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TEventHandlerList<T, T2>.Destroy;
begin
  Clear;
  FreeAndNil(FLock);
  inherited;
end;

function TEventHandlerList<T, T2>.IsEmpty: Boolean;
begin
  Result := Length(FEntries)=0;
end;

function TEventHandlerList<T, T2>.IsSameHandlerS(aSelf: TObject; aIndex: Integer): Boolean;
begin
  Result := (PMethod(@FEntries[aIndex]))^.Data=aSelf;
end;

function TEventHandlerList<T, T2>.IsSameHandlerT(aHandler: T; aIndex: Integer): Boolean;
begin
  Result := (PMethod(@FEntries[aIndex]).Code=PMethod(@aHandler).Code) and
            (PMethod(@FEntries[aIndex]).Data=PMethod(@aHandler).Data);
end;

function TEventHandlerList<T, T2>.IsSameHandlerT2(aHandler: T2; aSelf: Pointer; aIndex: Integer): Boolean;
begin
  Result := (PMethod(@FEntries[aIndex]).Code=Pointer((@aHandler)^)) and
            (PMethod(@FEntries[aIndex]).Data=aSelf);
end;

function TEventHandlerList<T, T2>.RemoveHandler(aSelf: TObject): Integer;
var
  e: Integer;
  NewLength: Integer;
begin
  Result := 0;
  FLock.Acquire;
  try
    NewLength := Length(FEntries);
    for e := NewLength-1 downto 0 do
    begin
      if IsSameHandlerS(aSelf, e) then
      begin
        if e<NewLength-1
        then FEntries[e] := FEntries[NewLength-1]; // TODO: order has changed!
        NewLength := NewLength-1;
        Inc(Result);
      end;
    end;
    if NewLength<>Length(FEntries)
    then SetLength(FEntries, NewLength);
  finally
    FLock.Release;
  end;
end;

function TEventHandlerList<T, T2>.RemoveHandler(aHandler: T): Integer;
var
  e: Integer;
  NewLength: Integer;
begin
  Result := 0;
  FLock.Acquire;
  try
    NewLength := Length(FEntries);
    for e := NewLength-1 downto 0 do
    begin
      if IsSameHandlerT(aHandler, e) then
      begin
        if e<NewLength-1
        then FEntries[e] := FEntries[NewLength-1]; // TODO: order has changed!
        NewLength := NewLength-1;
        Inc(Result);
      end;
    end;
    if NewLength<>Length(FEntries)
    then SetLength(FEntries, NewLength);
  finally
    FLock.Release;
  end;
end;

function TEventHandlerList<T, T2>.RemoveHandler(aHandler: T2; aSelf: Pointer): Integer;
var
  e: Integer;
  NewLength: Integer;
begin
  Result := 0;
  FLock.Acquire;
  try
    NewLength := Length(FEntries);
    for e := NewLength-1 downto 0 do
    begin
      if IsSameHandlerT2(aHandler, aSelf, e) then
      begin
        if e<NewLength-1
        then FEntries[e] := FEntries[NewLength-1]; // TODO: order has changed!
        NewLength := NewLength-1;
        Inc(Result);
      end;
    end;
    if NewLength<>Length(FEntries)
    then SetLength(FEntries, NewLength);
  finally
    FLock.Release;
  end;
end;


{ TMyThread }

constructor TMyThread.Create(aMethod: ThreadedMethod);
begin
  FMethod := aMethod;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TMyThread.Execute;
begin
  FMethod(Self);
end;

procedure TMyThread.Synchronize(AThreadProc: TThreadProcedure);
begin
  inherited Synchronize(AThreadProc);
end;

procedure TMyThread.Synchronize(AMethod: TThreadMethod);
begin
  inherited Synchronize(AMethod);
end;

{ TIMBEventEntry.TStreamCache }

function TIMBEventEntry.TStreamCache.Cache(aStreamID: Integer; aStream: TStream; const aStreamName: string): Integer;
begin
  Result := Length(FCache);
  SetLength(FCache, Result+1);
  FCache[Result].StreamID := aStreamID;
  FCache[Result].Stream := aStream;
  FCache[Result].StreamName := aStreamName;
end;

procedure TIMBEventEntry.TStreamCache.Clear(aFreeStream: Boolean);
var
  i: Integer;
begin
  if aFreeStream then
  begin
    for i := 0 to Length(FCache) - 1
    do FCache[i].Stream.Free;
  end;
  SetLength(FCache, 0);
end;

function TIMBEventEntry.TStreamCache.Find(aStream: TStream; out aStreamName: string): Integer;
var
  i: Integer;
begin
  i := IndexOfStream(aStream);
  if i >= 0 then
  begin
    aStreamName := FCache[i].StreamName;
    Result := FCache[i].StreamID;
  end
  else
  begin
    aStreamName := '';
    Result := -1;
  end;
end;

function TIMBEventEntry.TStreamCache.Find(aStreamID: Integer; out aStreamName: string): TStream;
var
  i: Integer;
begin
  i := IndexOfStreamID(aStreamID);
  if i >= 0 then
  begin
    aStreamName := FCache[i].StreamName;
    Result := FCache[i].Stream;
  end
  else
  begin
    aStreamName := '';
    Result := nil;
  end;
end;

function TIMBEventEntry.TStreamCache.IndexOfStream(aStream: TStream): Integer;
begin
  Result := Length(FCache) - 1;
  while (Result >= 0) AND (FCache[Result].Stream <> aStream)
  do Result := Result - 1;
end;

function TIMBEventEntry.TStreamCache.IndexOfStreamID(aStreamID: Integer): Integer;
begin
  Result := Length(FCache) - 1;
  while (Result >= 0) AND (FCache[Result].StreamID <> aStreamID)
  do Result := Result - 1;
end;

function TIMBEventEntry.TStreamCache.Remove(aStreamID: Integer): Integer;
begin
  Result := IndexOfStreamID(aStreamID);
  if Result >= 0 then
  begin
    if Result <> Length(FCache) - 1
    // put last on the position of entry to be deleted
    then FCache[Result] := FCache[Length(FCache) - 1];
    SetLength(FCache, Length(FCache) - 1);
  end;
end;

{ TIMBEventEntry }

procedure TIMBEventEntry.ClearAllStreams;
begin
  FStreamCache.Clear;
end;

procedure TIMBEventEntry.CopyHandlersFrom(aEventEntry: TIMBEventEntry);
begin
  FOnMultipleChangeObject.FEntries     := aEventEntry.FOnMultipleChangeObject.FEntries;
  FOnMultipleBuffer.FEntries           := aEventEntry.FOnMultipleBuffer.FEntries;
  FOnMultipleNormalEvent.FEntries      := aEventEntry.FOnMultipleNormalEvent.FEntries;
  FOnMultipleOtherEvent.FEntries       := aEventEntry.FOnMultipleOtherEvent.FEntries;
  FOnFocus                             := aEventEntry.FOnFocus;
  FOnStreamCreate                      := aEventEntry.FOnStreamCreate;
  FOnStreamEnd                         := aEventEntry.FOnStreamEnd;
  FOnMultipleChangeFederation.FEntries := aEventEntry.FOnMultipleChangeFederation.FEntries;
  FOnMultipleTimerTick.FEntries        := aEventEntry.FOnMultipleTimerTick.FEntries;
  FOnMultipleTimerCmd.FEntries         := aEventEntry.FOnMultipleTimerCmd.FEntries;
  FOnMultipleSubAndPub.FEntries        := aEventEntry.FOnMultipleSubAndPub.FEntries;
end;

constructor TIMBEventEntry.Create(aConnection: TIMBConnection; aID: Integer; const aEventName: string);
begin
  inherited Create;
  FID := aID;
  FConnection := aConnection;
  FEventName := aEventName;
  FParent := nil;
  FIsPublished := 0;
  FPublishers := False;
  FIsSubscribed := 0;
  FSubscribers := False;
  FStreamCache.Clear(False);
  FOnMultipleChangeObject := TEventHandlerList<TOnChangeObject, TOnChangeObjectNoObj>.Create;
  FOnMultipleNormalEvent := TEventHandlerList<TOnNormalEvent, TOnNormalEventNoObj>.Create;
  FOnMultipleBuffer := TEventHandlerList<TOnBuffer, TOnBufferNoObj>.Create;
  FOnMultipleOtherEvent := TEventHandlerList<TOnOtherEvent, TOnOtherEventNoObj>.Create;
  FOnMultipleTimerTick := TEventHandlerList<TOnTimerTick, TOnTimerTickNoObj>.Create;
  FOnMultipleTimerCmd := TEventHandlerList<TOnTimerCmd, TOnTimerCmdNoObj>.Create;
  FOnMultipleSubAndPub := TEventHandlerList<TOnSubAndPubEvent, TOnSubAndPubEventNoObj>.Create;
  FOnMultipleChangeFederation := TEventHandlerList<TOnChangeFederation, TOnChangeFederationNoObj>.Create;
end;

destructor TIMBEventEntry.Destroy;
begin
  FreeAndNil(FOnMultipleChangeObject);
  FreeAndNil(FOnMultipleNormalEvent);
  FreeAndNil(FOnMultipleBuffer);
  FreeAndNil(FOnMultipleOtherEvent);
  FreeAndNil(FOnMultipleTimerTick);
  FreeAndNil(FOnMultipleTimerCmd);
  FreeAndNil(FOnMultipleSubAndPub);
  FreeAndNil(FOnMultipleChangeFederation);
  FStreamCache.Clear;
  inherited;
end;

function TIMBEventEntry.GetIsPublished: Boolean;
begin
  Result := FIsPublished>0;
end;

function TIMBEventEntry.GetIsSubscribed: Boolean;
begin
  Result := FIsSubscribed>0;
end;

function TIMBEventEntry.GetShortEventName: string;
begin
  if IsOnFederation(Connection.Federation)
  then Result := Copy(EventName, Length(Connection.Federation)+2, Length(EventName)-Length(Connection.Federation)-1)
  else Result := EventName;
end;

procedure TIMBEventEntry.HandleBuffer(aEventTick: Integer; var aPayload: TByteBuffer);
var
  BufferID: Integer;
  BufferData: AnsiString;
  e: Integer;
  Buffer: TByteBuffer;
begin
  if not FOnMultipleBuffer.IsEmpty then
  begin
    aPayload.Read(BufferID);
    BufferData := aPayload.ReadAnsiString;
    for e := 0 to Length(FOnMultipleBuffer.Entries)-1 do
    begin
      Buffer.Buffer := BufferData;
      FOnMultipleBuffer.Entries[e](Self, aEventTick, BufferID, Buffer);
    end;
  end;
end;

procedure TIMBEventEntry.HandleChangeObject(var aPayload: TByteBuffer);
var
  Action: Integer;
  ObjectID: Integer;
  AttributeA: AnsiString;
  X: Double;
  Y: Double;
  e: Integer;
begin
  if Assigned(FOnFocus) then
  begin
    aPayload.Read(X);
    aPayload.Read(Y);
    FOnFocus(X, Y);
  end
  else
  begin
    if not FOnMultipleChangeFederation.IsEmpty then
    begin
      aPayload.Read(Action);
      if not aPayload.Read(ObjectID)
      then ObjectID := -1;
      if not aPayload.Read(AttributeA)
      then AttributeA := '';
      for e := 0 to Length(FOnMultipleChangeFederation.Entries)-1
      do FOnMultipleChangeFederation.Entries[e](Self.Connection, ObjectID, string(AttributeA));
    end
    else
    begin
      aPayload.Read(Action);
      aPayload.Read(ObjectID);
      aPayload.Read(AttributeA);
      for e := 0 to Length(FOnMultipleChangeObject.Entries)-1
      do FOnMultipleChangeObject.Entries[e](Action, ObjectID, ShortEventName, string(AttributeA));
    end;
  end;
end;

procedure TIMBEventEntry.HandleEvent(var aPayload: TByteBuffer);
var
  EventTick: Integer;
  EventKindInt: Integer;
  EventKind: TIMBEventKind;
begin
  aPayload.Read(EventTick);
  aPayload.Read(EventKindInt);
  EventKind := TIMBEventKind(EventKindInt and EventKindMask);
  case EventKind of
    ekChangeObjectEvent:
      HandleChangeObject(aPayload);
    ekNormalEvent:
      HandleNormalEvent(aPayload);
    ekBuffer:
      HandleBuffer(EventTick, aPayload);
    ekTimerTick:
      HandleTimerTick(aPayload);
    ekTimerPrepare,
    ekTimerStart,
    ekTimerStop:
      HandleTimerCmd(EventKind, aPayload);
    ekStreamHeader,
    ekStreamBody,
    ekStreamTail:
      HandleStreamEvent(EventKind, aPayload);
    ekLogWriteLn:
      HandleLogWriteLn(aPayload);
  else
    HandleOtherEvent(EventTick, EventKind, aPayload);
  end;
end;

procedure TIMBEventEntry.HandleLogWriteLn(var aPayload: TByteBuffer);
var
  uniqueClientID: Integer;
  logLine: string;
  logLevel: Integer;
begin
  aPayload.Read(uniqueClientID);
  aPayload.Read(logLine);
  aPayload.Read(logLevel);
  if Assigned(Connection.OnLog)
  then Connection.OnLog(Self, uniqueClientID, logLine, logLevel);
end;

procedure TIMBEventEntry.HandleNormalEvent(var aPayload: TByteBuffer);
var
  RC: Integer;
  e: Integer;
begin
  if not FOnMultipleNormalEvent.IsEmpty then
  begin
    RC := aPayload.ReadCursor;
    for e := 0 to Length(FOnMultipleNormalEvent.Entries)-1 do
    begin
      aPayload.ReadStart(RC);
      FOnMultipleNormalEvent.Entries[e](Self, aPayload);
    end;
  end;
end;

procedure TIMBEventEntry.HandleOtherEvent(aEventTick: Integer; aEventKind: TIMBEventKind; var aPayload: TByteBuffer);
var
  RC: Integer;
  e: Integer;
begin
  if not FOnMultipleOtherEvent.IsEmpty then
  begin
    RC := aPayload.ReadCursor;
    for e := 0 to Length(FOnMultipleOtherEvent.Entries)-1 do
    begin
      aPayload.ReadStart(RC);
      FOnMultipleOtherEvent.Entries[e](Self, aEventTick, aEventKind, aPayload);
    end;
  end;
end;

procedure TIMBEventEntry.HandleStreamEvent(aEventKind: TIMBEventKind; var aPayload: TByteBuffer);
var
  StreamID: Integer;
  StreamName: string;
  Stream: TStream;
begin
  case aEventKind of
    ekStreamHeader:
      begin
        if Assigned(OnStreamCreate) then
        begin
          aPayload.Read(StreamID);
          StreamName := string(aPayload.ReadAnsiString);
          Stream := OnStreamCreate(Self, StreamName);
          if Assigned(Stream)
          then FStreamCache.Cache(StreamID, Stream, StreamName);
        end;
      end;
    ekStreamBody:
      begin
        aPayload.Read(StreamID);
        Stream := FStreamCache.Find(StreamID, StreamName);
        if Assigned(Stream)
        then Stream.Write(aPayload.ReadAddr^, aPayload.ReadAvailable);
      end;
    ekStreamTail:
      begin
        aPayload.Read(StreamID);
        Stream := FStreamCache.Find(StreamID, StreamName);
        if Assigned(Stream) then
        begin
          Stream.Write(aPayload.ReadAddr^, aPayload.ReadAvailable);
          if Assigned(OnStreamEnd)
          then OnStreamEnd(Self, Stream, StreamName);
          Stream.Free;
          FStreamCache.Remove(StreamID);
        end;
      end;
  end;
end;

procedure TIMBEventEntry.HandleSubAndPub(aCommand: Integer; const aEventName: string; aIsChild: Boolean);
var
  e: Integer;
begin
  if not aIsChild then
  begin
    case aCommand of
      icSubscribe:
        FSubscribers := True;
      icPublish:
        FPublishers := True;
      icUnsubscribe:
        FSubscribers := False;
      icUnpublish:
        FPublishers := False;
    end;
  end;
  for e := 0 to Length(FOnMultipleSubAndPub.Entries)-1
  do FOnMultipleSubAndPub.Entries[e](Self, aCommand, aEventName, aIsChild);
end;

procedure TIMBEventEntry.HandleTimerCmd(aEventKind: TIMBEventKind; var aPayload: TByteBuffer);
var
  TimerName: string;
  e: Integer;
begin
  if not FOnMultipleTimerCmd.IsEmpty then
  begin
    TimerName := string(aPayload.ReadAnsiString);
    for e := 0 to Length(FOnMultipleTimerCmd.Entries)-1
    do FOnMultipleTimerCmd.Entries[e](Self, aEventKind, TimerName);
  end;
end;

procedure TIMBEventEntry.HandleTimerTick(var aPayload: TByteBuffer);
var
  Tick: Integer;
  TickTime: Int64;
  StartTime: Int64;
  TimerName: string;
  e: Integer;
begin
  if FOnMultipleTimerTick.IsEmpty then
  begin
    TimerName := string(aPayload.ReadAnsiString);
    aPayload.Read(Tick);
    aPayload.Read(TickTime);
    aPayload.Read(StartTime);
    for e := 0 to Length(FOnMultipleTimerTick.Entries)-1
    do FOnMultipleTimerTick.Entries[e](Self, TimerName, Tick, TickTime, StartTime);
  end;
end;

function TIMBEventEntry.IsEmpty: Boolean;
begin
  Result := not (IsSubscribed or IsPublished);
end;

function TIMBEventEntry.IsOnFederation(const aFederation: string): Boolean;
begin
  Result := AnsiCompareText(Copy(EventName, 1, Length(aFederation)+1), aFederation+EventNamePartSeperator)=0;
end;

function TIMBEventEntry.LogWriteLn(const aLine: string; aLevel: Integer): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(0)); // client id to be filled in by hub
  Payload.Prepare(AnsiString(aLine));
  Payload.Prepare(Integer(aLevel));
  Payload.PrepareApply;
  Payload.QWrite(Integer(0)); // client id to be filled in by hub
  Payload.QWrite(AnsiString(aLine));
  Payload.QWrite(Integer(aLevel));
  Result := SignalEvent(ekLogWriteLn, Payload);
end;

function TIMBEventEntry.NoHandlers: Boolean;
begin
  Result :=
    (Length(FOnMultipleChangeObject.FEntries)=0) and
    (not Assigned(FOnFocus)) and
    (Length(FOnMultipleNormalEvent.FEntries)=0) and
    (Length(FOnMultipleBuffer.FEntries)=0) and
    (not Assigned(FOnStreamCreate)) and
    (not Assigned(FOnStreamEnd)) and
    (Length(FOnMultipleChangeFederation.FEntries)=0) and
    (Length(FOnMultipleOtherEvent.FEntries)=0) and
    (Length(FOnMultipleTimerTick.FEntries)=0) and
    (Length(FOnMultipleTimerCmd.FEntries)=0);
end;

procedure TIMBEventEntry.Publish;
begin
  if not IsPublished
  then SignalPublish;
  FIsPublished := FIsPublished+1;
end;

procedure TIMBEventEntry.SignalPublish;
var
  Payload: TByteBuffer;
begin
  // send command
  Payload.Clear;
  Payload.Prepare(ID);
  Payload.Prepare(Integer(0)); // EET
  Payload.Prepare(AnsiString(EventName));
  Payload.PrepareApply;
  Payload.QWrite(ID);
  Payload.QWrite(Integer(0)); // EET
  Payload.QWrite(AnsiString(EventName));
  Connection.WriteCommand(icPublish, Payload);
end;

procedure TIMBEventEntry.RemoveAllHandlers;
begin
  FOnMultipleChangeObject.Clear;
  FOnFocus := nil;
  FOnMultipleNormalEvent.Clear;
  FOnMultipleBuffer.Clear;
  FOnMultipleOtherEvent.Clear;
  FOnStreamCreate := nil;
  FOnStreamEnd := nil;
  FOnMultipleChangeFederation.Clear;
  FOnMultipleTimerTick.Clear;
  FOnMultipleTimerCmd.Clear;
  FOnMultipleSubAndPub.Clear;
end;

procedure TIMBEventEntry.RemoveHandlers(aObject: TObject);
begin
  FOnMultipleChangeObject.RemoveHandler(aObject);
  if TMethod(FOnFocus).Data=aObject
  then FOnFocus := nil;
  FOnMultipleNormalEvent.RemoveHandler(aObject);
  FOnMultipleBuffer.RemoveHandler(aObject);
  FOnMultipleOtherEvent.RemoveHandler(aObject);
  if TMethod(FOnStreamCreate).Data=aObject
  then FOnStreamCreate := nil;
  if TMethod(FOnStreamEnd).Data=aObject
  then FOnStreamEnd := nil;
  FOnMultipleChangeFederation.RemoveHandler(aObject);
  FOnMultipleTimerTick.RemoveHandler(aObject);
  FOnMultipleTimerCmd.RemoveHandler(aObject);
  FOnMultipleSubAndPub.RemoveHandler(aObject);
end;

procedure TIMBEventEntry.SetOnBuffer(const aValue: TOnBuffer);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleBuffer.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleBuffer.Clear;
end;

procedure TIMBEventEntry.SetOnBufferNoObj(aSelf: Pointer; aValue: TOnBufferNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleBuffer.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleBuffer.Clear;
end;

procedure TIMBEventEntry.SetOnChangeObject(const aValue: TOnChangeObject);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleChangeObject.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleChangeObject.Clear;
end;

procedure TIMBEventEntry.SetOnChangeObjectNoObj(aSelf: Pointer; aValue: TOnChangeObjectNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleChangeObject.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleChangeObject.Clear;
end;

procedure TIMBEventEntry.SetOnNormalEvent(const aValue: TOnNormalEvent);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleNormalEvent.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleNormalEvent.Clear;
end;

procedure TIMBEventEntry.SetOnNormalEventNoObj(aSelf: Pointer; aValue: TOnNormalEventNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleNormalEvent.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleNormalEvent.Clear;
end;

procedure TIMBEventEntry.SetOnOtherEvent(const aValue: TOnOtherEvent);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleOtherEvent.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleOtherEvent.Clear;
end;

procedure TIMBEventEntry.SetOnOtherEventNoObj(aSelf: Pointer; aValue: TOnOtherEventNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleOtherEvent.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleOtherEvent.Clear;
end;

procedure TIMBEventEntry.SetOnStreamCreate(const aValue: TOnStreamCreate);
begin
  if Assigned(aValue) and not Assigned(FOnStreamCreate) then
  begin
    FOnStreamCreate := aValue;
    if not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnStreamCreate := aValue;
end;

procedure TIMBEventEntry.SetOnStreamCreateNoObj(aSelf: Pointer; aValue: TOnStreamCreateNoObj);
var
  m: TOnStreamCreate;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnStreamCreate := m;
end;

procedure TIMBEventEntry.SetOnStreamEnd(const aValue: TOnStreamEnd);
begin
  if Assigned(aValue) and not Assigned(FOnStreamEnd) then
  begin
    FOnStreamEnd := aValue;
    if not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnStreamEnd := aValue;
end;

procedure TIMBEventEntry.SetOnStreamEndNoObj(aSelf: Pointer; aValue: TOnStreamEndNoObj);
var
  m: TOnStreamEnd;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnStreamEnd := m;
end;

procedure TIMBEventEntry.SetOnSubAndPub(const aValue: TOnSubAndPubEvent);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleSubAndPub.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleSubAndPub.Clear;
end;

procedure TIMBEventEntry.SetOnSubAndPubNoObj(aSelf: Pointer; const aValue: TOnSubAndPubEventNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleSubAndPub.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleSubAndPub.Clear;
end;

procedure TIMBEventEntry.SetOnTimerCmd(const aValue: TOnTimerCmd);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleTimerCmd.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleTimerCmd.Clear;
end;

procedure TIMBEventEntry.SetOnTimerCmdNoObj(aSelf: Pointer; aValue: TOnTimerCmdNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleTimerCmd.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleTimerCmd.Clear;
end;

procedure TIMBEventEntry.SetOnTimerTick(const aValue: TOnTimerTick);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleTimerTick.AddHandler(aValue)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleTimerTick.Clear;
end;

procedure TIMBEventEntry.SetOnTimerTickNoObj(aSelf: Pointer; aValue: TOnTimerTickNoObj);
begin
  if Assigned(aValue) then
  begin
    if (FOnMultipleTimerTick.AddHandler(aValue, aSelf)>0) and not IsSubscribed then
    begin
      SignalSubscribe;
      Inc(FIsSubscribed);
    end;
  end
  else FOnMultipleTimerTick.Clear;
end;

function TIMBEventEntry.SignalBuffer(aBufferID: Integer; aBuffer: Pointer; aBufferLen, aEventFlags: Integer): Integer;
var
  Payload: TByteBuffer;
begin
  if not IsPublished
  then Publish;
  if IsPublished then
  begin
    Payload.Clear;
    Payload.Prepare(ID);
    Payload.Prepare(Integer(0)); // tick
    Payload.Prepare(Integer(ekBuffer) or (aEventFlags and EventFlagsMask));
    Payload.Prepare(aBufferID);
    Payload.Prepare(aBufferLen);
    Payload.Prepare(aBuffer^, aBufferLen);
    Payload.PrepareApply;
    Payload.QWrite(ID);
    Payload.QWrite(Integer(0)); // tick
    Payload.QWrite(Integer(ekBuffer) or (aEventFlags and EventFlagsMask));
    Payload.QWrite(aBufferID);
    Payload.QWrite(aBufferLen);
    Payload.QWrite(aBuffer^, aBufferLen);
    Result := Connection.WriteCommand(icEvent, Payload);
  end
  else Result := iceNotEventPublished;
end;

function TIMBEventEntry.SignalChangeObject(aAction, aObjectID: Integer; const aAttribute: string): Integer;
var
  Payload: TByteBuffer;
begin
  if not IsPublished
  then Publish;
  if IsPublished then
  begin
    Payload.Clear;
    Payload.Prepare(ID);
    Payload.Prepare(Integer(0)); // tick
    Payload.Prepare(Integer(ekChangeObjectEvent));
    Payload.Prepare(aAction);
    Payload.Prepare(aObjectID);
    Payload.Prepare(AnsiString(aAttribute));
    Payload.PrepareApply;
    Payload.QWrite(ID);
    Payload.QWrite(Integer(0)); // tick
    Payload.QWrite(Integer(ekChangeObjectEvent));
    Payload.QWrite(aAction);
    Payload.QWrite(aObjectID);
    Payload.QWrite(AnsiString(aAttribute));
    Result := Connection.WriteCommand(icEvent, Payload);
  end
  else Result := iceNotEventPublished;
end;

function TIMBEventEntry.SignalEvent(aEventKind: TIMBEventKind; const aEventPayload: TByteBuffer): Integer;
var
  Payload: TByteBuffer;
begin
  if not IsPublished
  then Publish;
  if IsPublished then
  begin
    Payload.Clear;
    Payload.Prepare(ID);
    Payload.Prepare(Integer(0)); // tick
    Payload.Prepare(Integer(aEventKind));
    Payload.Prepare(aEventPayload);
    Payload.PrepareApply;
    Payload.QWrite(ID);
    Payload.QWrite(Integer(0)); // tick
    Payload.QWrite(Integer(aEventKind));
    Payload.QWrite(aEventPayload);
    Result := Connection.WriteCommand(icEvent, Payload);
  end
  else Result := iceNotEventPublished;
end;

function TIMBEventEntry.SignalStream(const aStreamName: string; aStream: TStream; aStreamBodyBuffer: Integer): Integer;
// todo: add stream id @ writer to lookup when reconstructing stream @ receiver
// todo: stream id is checksum and crc of cat of client name and stream name?
var
  StreamID: Integer;
  Payload: TByteBuffer;
  ReadSize: Longint;
  BodyIndex: Integer;
  EventKindIndex: Integer;
begin
  if not IsPublished
  then Publish;
  if IsPublished then
  begin
    // ekStreamHeader, includes stream name, no stream data
    StreamID := Integer((CheckSum(AnsiString(aStreamName)) and $FFFF) or ((CheckSum(AnsiString(Connection.RemoteHost)) and $FFFF) shl 16) xor Connection.RemotePort xor Connection.UniqueClientID);
    Payload.Clear;
    Payload.Prepare(ID);
    Payload.Prepare(Integer(0)); // tick
    Payload.Prepare(Integer(ekStreamHeader)); // event kind
    Payload.Prepare(StreamID);
    Payload.Prepare(AnsiString(aStreamName));
    Payload.PrepareApply;
    Payload.QWrite(ID);
    Payload.QWrite(Integer(0)); // tick
    EventKindIndex := Payload.WriteCursor;
    Payload.QWrite(Integer(ekStreamHeader)); // event kind
    Payload.QWrite(StreamID);
    BodyIndex := Payload.WriteCursor;
    Payload.QWrite(AnsiString(aStreamName));
    Result := Connection.WriteCommand(icEvent, Payload);
    if Result>0 then
    begin
      // ekStreamBody, only buffer size chunks of data
      // prepare Payload to same value but aStreamName stripped
      // fixup event kind
      Payload.WriteStart(EventKindIndex);
      Payload.QWrite(Integer(ekStreamBody));
      Payload.WriteStart(BodyIndex);
      // prepare room for body data
      Payload.PrepareStart;
      Payload.PrepareSize(aStreamBodyBuffer);
      Payload.PrepareApply;
      // write pointer in ByteBuffer is still at beginning of stream read buffer!
      // but buffer is already created on correct length
      repeat
        ReadSize := aStream.Read((@Payload.Buffer[BodyIndex+1])^, aStreamBodyBuffer);
        if ReadSize = aStreamBodyBuffer
        then Result := Connection.WriteCommand(icEvent, Payload);
      until (ReadSize <> aStreamBodyBuffer) or (Result<=0);
      if Result>0 then
      begin
        // clip ByteBuffer to bytes read from stream
        // write pointer in ByteBuffer is still at beginning of stream read buffer!
        Payload.PrepareStart;
        Payload.PrepareSize(ReadSize);
        Payload.PrepareApplyAndTrim;
        // fixup event kind
        Payload.WriteStart(EventKindIndex);
        Payload.QWrite(Integer(ekStreamTail));
        Result := Connection.WriteCommand(icEvent, Payload);
      end;
    end;
  end
  else Result := iceNotEventPublished;
end;

procedure TIMBEventEntry.Subscribe;
begin
  if not IsSubscribed
  then SignalSubscribe;
  FIsSubscribed := FIsSubscribed+1;
end;

procedure TIMBEventEntry.SignalSubscribe;
var
  Payload: TByteBuffer;
begin
  // send command
  Payload.Clear;
  Payload.Prepare(ID);
  Payload.Prepare(Integer(0)); // EET
  Payload.Prepare(AnsiString(EventName));
  Payload.PrepareApply;
  Payload.QWrite(ID);
  Payload.QWrite(Integer(0)); // EET
  Payload.QWrite(AnsiString(EventName));
  Connection.WriteCommand(icSubscribe, Payload);
end;

function TIMBEventEntry.TimerAcknowledge(const aTimerName, aClientName: string; aProposedTimeStep: Integer): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(AnsiString(aClientName));
  Payload.Prepare(AnsiString(aTimerName));
  Payload.Prepare(aProposedTimeStep);
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(aClientName));
  Payload.QWrite(AnsiString(aTimerName));
  Payload.QWrite(aProposedTimeStep);
  Result := SignalEvent(ekTimerAcknowledge, Payload);
end;

function TIMBEventEntry.TimerAcknowledgeAdd(const aTimerName, aClientName: string): Integer;
begin
  Result := TimerAcknowledgeCmd(ekTimerAcknowledgedListAdd, aTimerName, aClientName);
end;

function TIMBEventEntry.TimerAcknowledgeCmd(aEventKind: TIMBEventKind; const aTimerName, aClientName: string): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(AnsiString(aTimerName));
  Payload.Prepare(AnsiString(aClientName));
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(aTimerName));
  Payload.QWrite(AnsiString(aClientName));
  Result := SignalEvent(aEventKind, Payload);
end;

function TIMBEventEntry.TimerAcknowledgeRemove(const aTimerName, aClientName: string): Integer;
begin
  Result := TimerAcknowledgeCmd(ekTimerAcknowledgedListRemove, aTimerName, aClientName);
end;

function TIMBEventEntry.TimerBasicCmd(aEventKind: TIMBEventKind; const aTimerName: string): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(AnsiString(aTimerName));
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(aTimerName));
  Result := SignalEvent(aEventKind, Payload);
end;

function TIMBEventEntry.TimerCancel(const aTimerName: string): Integer;
begin
  Result := TimerBasicCmd(ekTimerCancel, aTimerName);
end;

function TIMBEventEntry.TimerCreate(const aTimerName: string; aStartTimeUTCorRelFT: Int64; aResolutionms: Integer;
  aSpeedFactor: Double; aRepeatCount: Integer): Integer;
var
  Payload: TByteBuffer;
begin
  if not IsPublished
  then Publish;
  if IsPublished then
  begin
    Payload.Clear;
    Payload.Prepare(ID);
    Payload.Prepare(AnsiString(aTimerName));
    Payload.Prepare(aStartTimeUTCorRelFT);
    Payload.Prepare(aResolutionms);
    Payload.Prepare(aSpeedFactor);
    Payload.Prepare(aRepeatCount);
    Payload.PrepareApply;
    Payload.QWrite(ID);
    Payload.QWrite(AnsiString(aTimerName));
    Payload.QWrite(aStartTimeUTCorRelFT);
    Payload.QWrite(aResolutionms);
    Payload.QWrite(aSpeedFactor);
    Payload.QWrite(aRepeatCount);
    Result := Connection.WriteCommand(icCreateTimer, Payload);
  end
  else Result := iceNotEventPublished;
end;

function TIMBEventEntry.TimerPrepare(const aTimerName: string): Integer;
begin
  Result := TimerBasicCmd(ekTimerPrepare, aTimerName);
end;

function TIMBEventEntry.TimerSetSpeed(const aTimerName: string; aSpeedFactor: Double): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(AnsiString(aTimerName));
  Payload.Prepare(aSpeedFactor);
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(aTimerName));
  Payload.QWrite(aSpeedFactor);
  Result := SignalEvent(ekTimerSetSpeed, Payload);
end;

function TIMBEventEntry.TimerStart(const aTimerName: string): Integer;
begin
  Result := TimerBasicCmd(ekTimerStart, aTimerName);
end;

function TIMBEventEntry.TimerStop(const aTimerName: string): Integer;
begin
  Result := TimerBasicCmd(ekTimerStop, aTimerName);
end;

procedure TIMBEventEntry.UnPublish;
begin
  if IsPublished then
  begin
    FIsPublished := FIsPublished-1;
    if FIsPublished=0
    then SignalUnPublish;
  end;
end;

procedure TIMBEventEntry.SignalUnPublish;
var
  Payload: TByteBuffer;
begin
  // send command
  Payload.Clear;
  Payload.Prepare(AnsiString(EventName));
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(EventName));
  Connection.WriteCommand(icUnpublish, Payload);
end;

procedure TIMBEventEntry.UnSubscribe;
begin
  if IsSubscribed then
  begin
    FIsSubscribed := FIsSubscribed-1;
    if FIsSubscribed=0
    then SignalUnSubscribe;
  end;
end;

procedure TIMBEventEntry.SignalUnSubscribe;
var
  Payload: TByteBuffer;
begin
  // send command
  Payload.Clear;
  Payload.Prepare(AnsiString(EventName));
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(EventName));
  Connection.WriteCommand(icUnsubscribe, Payload);
end;

{ TIMBConnection.TEventList }

function TIMBConnection.TEventList.AddEvent(aConnection: TIMBConnection; const aEventName: string): TIMBEventEntry;
begin
  Result := TIMBEventEntry.Create(aConnection, Count{=next event id}, aEventName);
  Add(Result);
end;

function TIMBConnection.TEventList.EventEntryOnName(const aEventName: string): TIMBEventEntry;
var
  EventID: Integer;
begin
  EventID := Count-1;
  while (EventID>=0) and (AnsiCompareText(aEventName, EventEntry[EventID].EventName)<>0)
  do EventID := EventID-1;
  if EventID>=0
  then Result := TIMBEventEntry(Items[EventID])
  else Result := nil;
end;

function TIMBConnection.TEventList.GetEventEntry(aIndex: Integer): TIMBEventEntry;
begin
  Result := TIMBEventEntry(Items[aIndex]);
end;

function TIMBConnection.TEventList.IndexOfEventName(const aEventName: string): Integer;
begin
  Result := Count-1;
  while (Result>=0) and (AnsiCompareText(aEventName, EventEntry[Result].EventName)<>0)
  do Result := Result-1;
end;

procedure TIMBConnection.TEventList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action=lnDeleted
  then TObject(Ptr).Free;
end;

{ TIMBConnection }

function TIMBConnection.AddEvent(const aEventName: string): TIMBEventEntry;
var
  EventID: Integer;
begin
  // first try to find empty event entry, oldest first
  EventID := 0;
  while (EventID<FEventList.Count) and not FEventList.EventEntry[EventID].IsEmpty
  do EventID := EventID+1;
  // if no empty entry is found then create new one
  if EventID<FEventList.Count then
  begin
    FEventList[EventID].FEventName := aEventName;
    FEventList[EventID].FParent := nil;
    FEventList[EventID].RemoveAllHandlers;
    Result := FEventList.EventEntry[EventID];
  end
  else Result := FEventList.AddEvent(Self, aEventName);
end;

function TIMBConnection.AddEventL(const aEventName: string): TIMBEventEntry;
begin
  FEventListLock.Acquire;
  try
    Result := AddEvent(aEventName);
  finally
    FEventListLock.Release;
  end;
end;

function TIMBConnection.Close: Boolean;
var
  LocalSocket: TSocket;
begin
  if FSocket<>INVALID_SOCKET then
  begin
    if Assigned(FOnDisconnect)
    then FOnDisconnect(Self);
    // signal socket is closed before realy closing socket to avoid double call of OnDisconnect and double close attempt
    // reader is waiting and will call close imediatly after close
    LocalSocket := FSocket;
    FSocket := INVALID_SOCKET;
    closesocket(LocalSocket);
    Result := True;
  end
  else Result := False;
end;

constructor TIMBConnection.Create(const aRemoteHost: string; aRemotePort: Integer;
  const aOwnerName: string; aOwnerID: Integer; const aFederation: string;
  aUseReaderThread: Boolean);
begin
  // to be compatible with windows xp and older recv must not be called with MSG_WAITALL
  FOldWindows := TOSVersion.Major<=5;
  {$IFDEF IMBDebug}
  if FOldWindows
  then Log.WriteLn('Detected old windows ('+IntToStr(TOSVersion.Major)+'.'+IntToStr(TOSVersion.Minor)+') version (possibly MSG_WAITALL incompatible): switching to alternate socket reading', llWarning);
  {$ENDIF}
  FRemoteHost := '';
  FRemotePort := DefaultRemotePort;
  FAddressFamily := AF_UNSPEC;
  FSocket := INVALID_SOCKET;
  FOnDisconnect := nil;
  FNoDelay := False;
  FKeepAlive := False;
  FLingerTime := DefaultLingerTime;
  FEventList := TEventList.Create;
  FUniqueClientID := 0;
  FClientHandle := 0;
  FEventListLock := TCriticalSection.Create;
  FWriteLock := TCriticalSection.Create;
  SetLength(FReadCommandBuffer, Length(MagicBytes)+SizeOf(Integer)+SizeOf(Integer)); // magic/command/payload-size
  FEventTranslation := TIMBEventTranslation.Create;
  FOnVariable := nil;
  FReInitStatus := False;
  FReaderThread := nil;
  FUseReaderThread := aUseReaderThread;
  FIMB2Compatible := True; // todo: set this for now (to be backwards compatible, with old broker)
  inherited Create;
  FFederation := aFederation;
  FOwnerName := aOwnerName;
  FOwnerID := aOwnerID;
  Open(aRemoteHost, aRemotePort);
end;

class procedure TIMBConnection.DecodeStatus(const aVarValue: AnsiString; out aProgress, aStatus: Integer);
var
  Payload: TByteBuffer;
begin
  Payload.Buffer := aVarValue;
  Payload.Read(aStatus);
  Payload.Read(aProgress);
end;

destructor TIMBConnection.Destroy;
begin
  if Assigned(FReaderThread)
  then FReaderThread.Terminate;
  Close; // should unlock reader thread
  // resume thread if suspended
  if Assigned(FReaderThread) then
  begin
    FReaderThread.Suspended := False;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end;
  FreeAndNil(FEventList);
  FreeAndNil(FEventListLock);
  FreeAndNil(FWriteLock);
  FreeAndNil(FEventTranslation);
  inherited;
end;

function TIMBConnection.EventIDToEventL(aEventID: Integer): TIMBEventEntry;
begin
  FEventListLock.Acquire;
  try
    Result := FEventList.EventEntry[aEventID];
  finally
    FEventListLock.Release;
  end;
end;

function TIMBConnection.FindEventAutoPublishL(const aEventName: string): TIMBEventEntry;
begin
  Result := FindEventL(aEventName);
  if not Assigned(Result)
  then Result := Publish(aEventName, False);
end;

function TIMBConnection.FindEventL(const aEventName: string): TIMBEventEntry;
begin
  FEventListLock.Acquire;
  try
    Result := FEventList.EventEntryOnName(aEventName);
  finally
    FEventListLock.Release;
  end;
end;

function TIMBConnection.FindEventParentL(const aEventName: string): TIMBEventEntry;
var
  ParentEventName: string;
  EventID: Integer;
  EventName: string;
begin
  FEventListLock.Acquire;
  try
    Result := nil;
    ParentEventName := '';
    for EventID := 0 to FEventList.Count-1 do
    begin
      EventName := FEventList.EventEntry[EventID].EventName;
      if EndsWith(EventName, EventFilterPostFix) and StartsWith(aEventName, EndStrip(EventName, EventFilterPostFix)) then
      begin
        if Length(ParentEventName)<Length(EventName) then
        begin
          Result := FEventList.EventEntry[EventID];
          ParentEventName := EventName;
        end;
      end;
    end;
  finally
    FEventListLock.Release;
  end;
end;

function TIMBConnection.FindOrAddEventL(const aEventName: string): TIMBEventEntry;
begin
  FEventListLock.Acquire;
  try
    Result := FEventList.EventEntryOnName(aEventName);
    if not Assigned(Result)
    then Result := AddEvent(aEventName);
  finally
    FEventListLock.Release;
  end;
end;

function TIMBConnection.GetConnected: Boolean;
begin
  Result := FSocket<>INVALID_SOCKET;
end;

function TIMBConnection.GetLinger: Integer;
begin
  Result := GetSocketLinger(FSocket);
end;

function TIMBConnection.GetUniqueClientID: Integer;
var
  SpinCount: Integer;
begin
  if FUniqueClientID=0 then
  begin
    RequestUniqueClientID;
    SpinCount := UniqueClientIDSpinCount;
    while (FUniqueClientID=0) and (SpinCount>0) do
    begin
      Sleep(UniqueClientIDSpinWait);
      SpinCount := SpinCount-1;
    end;
    // avoid requesting again
    if FUniqueClientID=0
    then FUniqueClientID := -1;
  end;
  Result := FUniqueClientID;
end;

procedure TIMBConnection.HandleCommand(aCommand: Integer; var aPayload: TByteBuffer);
begin
  case aCommand of
    icEvent:
      HandleCommandEvent(aPayload);
    icSetVariable:
      HandleCommandVariable(aPayload);
    icSetEventIDTranslation:
      FEventTranslation.SetEventTranslation(
        aPayload.PeekInteger(0, InvalidTranslatedEventID),
        aPayload.PeekInteger(SizeOf(Integer), InvalidTranslatedEventID));
    icUniqueClientID:
      begin
        aPayload.Read(FUniqueClientID);
        aPayload.Read(FClientHandle);
      end;
    icTimeStamp:
      begin
        // ignore for now, only when using and syncing local time (we trust hub time for now)
        aPayload.Read(FBrokerAbsoluteTime);
        aPayload.Read(FBrokerTick);
        aPayload.Read(FBrokerTickDelta);
      end;
    icEventNames:
      HandleEventNames(aPayload);
    icEndSession:
      Close;
    icSubscribe,
    icUnsubscribe,
    icPublish,
    icUnpublish:
      HandleSubAndPub(aCommand, aPayload);
  else
    HandleCommandOther(aCommand, aPayload);
  end;
end;

procedure TIMBConnection.HandleCommandEvent(var aPayload: TByteBuffer);
var
  TxEventID: Integer;
begin
  TxEventID := FEventTranslation.TranslateEventID(aPayload.ReadInteger);
  if TxEventID <> InvalidTranslatedEventID
  then EventIDToEventL(TxEventID).HandleEvent(aPayload)
  else raise Exception.Create('Invalid event id found in event from '+FRemoteHost);
end;

procedure TIMBConnection.HandleCommandOther(aCommand: Integer; var aPayload: TByteBuffer);
begin
  // override to implement protocol extensions
end;

procedure TIMBConnection.HandleCommandVariable(var aPayload: TByteBuffer);
var
  VarName: string;
  VarValue: AnsiString;
  ModelName: string;
  ModelUniqueID: string;
  Status: Integer;
  Progress: Integer;
begin
  VarName := string(aPayload.ReadAnsiString);
  // check if it is a status update
  if EndsWith(VarName, msVarSepChar+ModelStatusVarName) then
  begin // status update
    if Assigned(OnStatusUpdate) then
    begin
      ModelName := EndStrip(VarName, msVarSepChar+ModelStatusVarName);
      ModelUniqueID := Left(ModelName, 8);
      Delete(ModelName, 1, 8);
      aPayload.ReadInteger; // read size of value (should be 2*sizeof int32 = 8)
      aPayload.Read(Status);
      aPayload.Read(Progress);
      OnStatusUpdate(Self, ModelUniqueID, ModelName, Progress, Status);
    end;
  end
  else
  begin // other variable
    if Assigned(OnVariable) then
    begin
      VarValue := aPayload.ReadAnsiString;
      OnVariable(Self, VarName, VarValue);
    end;
  end;
end;

procedure TIMBConnection.HandleEventNames(var aPayload: TByteBuffer);
var
  en: Integer;
  EventNames: TIMBEventNames;
  ec: Integer;
begin
  if Assigned(FOnEventNames) then
  begin
    aPayload.Read(ec);
    SetLength(EventNames, ec);
    for en := 0 to ec-1 do
    begin
      EventNames[en].EventName := string(aPayload.ReadAnsiString);
      aPayload.Read(EventNames[en].Publishers);
      aPayload.Read(EventNames[en].Subscribers);
      aPayload.Read(EventNames[en].Timers);
    end;
    FOnEventNames(Self, EventNames);
  end;
end;

procedure TIMBConnection.HandleSubAndPub(aCommand: Integer; var aPayload: TByteBuffer);
var
  EventID: Integer;
  EventEntryType: Integer;
  EventName: AnsiString;
  EventNameStr: string;
  EventEntry: TIMBEventEntry;
  IsChild: Boolean;
begin
  case aCommand of
    icSubscribe,
    icPublish:
      begin
        aPayload.Read(EventID);
        aPayload.Read(EventEntryType);
        aPayload.Read(EventName);
        EventNameStr := string(EventName);
        EventEntry := FindEventL(EventNameStr);
        if not Assigned(EventEntry) then
        begin
          EventEntry := FindEventParentL(EventNameStr);
          IsChild := True;
        end
        else IsChild := False;
        if Assigned(EventEntry) and not EventEntry.IsEmpty
        then EventEntry.HandleSubAndPub(aCommand, EventNameStr, IsChild);
      end;
    icUnsubscribe,
    icUnpublish:
      begin
        aPayload.Read(EventName);
        EventNameStr := string(EventName);
        EventEntry := FindEventL(EventNameStr);
        if not Assigned(EventEntry) then
        begin
          EventEntry := FindEventParentL(EventNameStr);
          IsChild := True;
        end
        else IsChild := False;
        if Assigned(EventEntry) and not EventEntry.IsEmpty
        then EventEntry.HandleSubAndPub(aCommand, EventNameStr, IsChild);
      end;
  end;
end;
{
function TIMBConnection.InternalRecv(buf: PAnsiChar; len: Integer): Integer;
var
  totalBytesRead: Integer;
	bytesToRead: Integer;
	bytesRead: Integer;
begin
  totalBytesRead := 0;
	bytesToRead := len;
	bytesRead := recv(fSocket, buf, bytesToRead, 0);
	totalBytesRead :=  totalBytesRead+bytesRead;
	while (FSocket <> INVALID_SOCKET) and (bytesRead>0) and (totalBytesRead<len) do
	begin
		buf := buf+bytesRead;
		bytesToRead := bytesToRead-bytesRead;
		bytesRead := recv(FSocket, buf, bytesToRead, 0);
		totalBytesRead :=  totalBytesRead+bytesRead;
	end;
	if bytesRead>0
  then Result := totalBytesRead
	else Result := bytesRead;
end;
}
class function TIMBConnection.IsStatusVarName(const aVarName: string): Boolean;
begin
  Result := EndsWith(aVarName, msVarSepChar+ModelStatusVarName);
end;

function TIMBConnection.LogWriteLn(const aLogEventName, aLine: string; aLevel: Integer): Integer;
begin
  if not Assigned(FLogEvent)
  then FLogEvent := FindEventAutoPublishL(PrefixFederation(aLogEventName));
  if Assigned(FLogEvent)
  then Result := FLogEvent.LogWriteLn(aLine, aLevel)
  else Result := iceNotEventPublished;
end;

function TIMBConnection.Open(const aRemoteHost: string; aRemotePort: Integer): Boolean;
var
  Addresses: TSockAddresses;
  a: Integer;
  e: Integer;
begin
  Result := False; // sentinel
  // close any existing connection
  Close;
  FRemoteHost := aRemoteHost;
  FRemotePort := aRemotePort;
  // resolve host to address and socket family (ie ipv4 or ipv6)
  Addresses := GetSocketAddresses(AnsiString(FRemoteHost), AnsiString(IntToStr(FRemotePort)));
  a := 0;
  while (a<Length(Addresses)) do
  begin
    // try to connect to resolved address
    FAddressFamily := Addresses[a].Family;
    FSocket := socket(Addresses[a].Family, SOCK_STREAM, IPPROTO_TCP);
    if Connected then
    begin
      if connect(FSocket, Addresses[a], sizeof(Addresses[a]))<>SOCKET_ERROR then
      begin
        Result := True;
        a := Length(Addresses); // signal we have found a valid address
        Linger := FLingerTime;
      end
      else
      begin
        // could not connect: cleanup socket because next address could be different family
        closesocket(FSocket);
        FSocket := INVALID_SOCKET;
        a := a+1;
      end;
    end
    else a := a+1;
  end;
  // default actions on connect
  if Connected then
  begin
    // check if we have to start the reader thread
    if Assigned(FReaderThread)
    then FReaderThread.Suspended := False
    else
    begin
      if FUseReaderThread
      then FReaderThread := TMyThread.Create(ReadCommandsByThread);
    end;
    // for backwards compatibility we now request a unique client id
    // imb 3 always immediatly provides for this id on connect
    if IMB2Compatible
    then RequestUniqueClientID;
    // signal who we are to hub
    SetOwner;
    // request all variables
    if Assigned(FOnVariable) or Assigned(FOnStatusUpdate)
    then WriteCommand(icAllVariables, EmptyByteBuffer^);
    // resubscribe and publish events
    FEventListLock.Acquire;
    try
      for e := 0 to FEventList.Count-1 do
      begin
         if FEventList.EventEntry[e].IsPublished
         then FEventList.EventEntry[e].SignalPublish;
         if FEventList.EventEntry[e].IsSubscribed
         then FEventList.EventEntry[e].SignalSubscribe;
      end;
    finally
      FEventListLock.Release;
    end;
  end;
end;

function TIMBConnection.PrefixFederation(const aEventName: string; aUseFederationPrefix: Boolean): string;
begin
  if (Federation<>'') and aUseFederationPrefix
  then Result := Federation+EventNamePartSeperator+aEventName
  else Result := aEventName;
end;

function TIMBConnection.Publish(const aEventName: string; aUseFederationPrefix: Boolean): TIMBEventEntry;
begin
  Result := FindOrAddEventL(PrefixFederation(aEventName, aUseFederationPrefix));
  Result.Publish;
end;

function ReadBytes(aSocket: TSocket; aBuffer: PByte; aBufferSize: Integer): Boolean;
var
  BytesRead: Integer;
begin
  Result := True;
  while Result and (aBufferSize>0) do
  begin
    BytesRead := recv(aSocket, aBuffer^, aBufferSize, 0);
    if BytesRead>0 then
    begin
      aBuffer := aBuffer+BytesRead;
      aBufferSize := aBufferSize-BytesRead;
    end
    else Result := False;
  end;
end;

function TIMBConnection.ReadCommandOlderWindows(out aCommand: Integer; var aPayload: TByteBuffer): Boolean;
var
  BytesRead: Integer;
  Continue: Boolean;
  PayloadSize: Integer;
  CheckSum: Integer;
begin
  if Connected then
  begin
    // at first call of recv set flags=0 and not MSG_WAITALL to avoid write wait
    BytesRead := recv(FSocket, FReadCommandBuffer[1], Length(FReadCommandBuffer), 0);
    if (BytesRead=SOCKET_ERROR) OR (BytesRead=0) then
    begin
      if Assigned(FOnSocketError) and (BytesRead<>0)
      then FOnSocketError(Self, FSocket, False);
      Close;
      {$IFDEF IMBDebug}
      if BytesRead<>0
      then Log.WriteLn('IMB reader thread: SOCKET_ERROR: close', llError);
      {$ENDIF}
      Result := False;
    end
    else
    begin
      // check if we got all
      if BytesRead<Length(FReadCommandBuffer) then
      begin
        if ReadBytes(FSocket, @FReadCommandBuffer[1+BytesRead], Length(FReadCommandBuffer)-BytesRead)
        then BytesRead := Length(FReadCommandBuffer);
      end;
      if BytesRead=Length(FReadCommandBuffer) then
      begin
        Continue := True;
        while Continue and not CompareMem(PAnsiChar(FReadCommandBuffer), PAnsiChar(MagicBytes), Length(MagicBytes)) do
        begin
          // skip 1 byte
          Move(FReadCommandBuffer[2], FReadCommandBuffer[1], Length(FReadCommandBuffer)-1);
          Continue := Connected and (recv(FSocket, FReadCommandBuffer[Length(FReadCommandBuffer)-1], 1, MSG_WAITALL)=1);
        end;
        if Continue then
        begin
          Move(FReadCommandBuffer[1+Length(MagicBytes)], aCommand, SizeOf(aCommand));
          Move(FReadCommandBuffer[1+Length(MagicBytes)+SizeOf(aCommand)], PayloadSize, SizeOf(PayloadSize));
          if PayloadSize<=MaxPayloadSize then
          begin
            if PayloadSize>0 then
            begin
              aPayload.Clear(PayloadSize);
              if Connected and ReadBytes(FSocket, PByte(PAnsiChar(aPayload.Buffer)), PayloadSize) then
              begin
                if Connected and ReadBytes(FSocket, PByte(@CheckSum), SizeOf(CheckSum))
                then Result := CheckSum=CheckStringMagic
                else
                begin
                  Result := False;
                  {$IFDEF IMBDebug}
                  Log.WriteLn('IMB reader thread: invalid string magic: '+IntToHex(CheckSum, 8), llError);
                  {$ENDIF}
                end;
              end
              else
              begin
                Result := False;
                {$IFDEF IMBDebug}
                Log.WriteLn('IMB reader thread: incomplete packet (<'+IntToStr(PayloadSize)+')', llError);
                {$ENDIF}
              end;
            end
            else
            begin
              aPayload.Clear;
              Result := True;
            end;
          end
          else
          begin
            aPayload.Clear;
            Result := False;
            {$IFDEF IMBDebug}
            Log.WriteLn('IMB reader thread: over size payload ('+IntToStr(PayloadSize)+')', llError);
            {$ENDIF}
          end;
        end
        else
        begin
          Result := False;
          {$IFDEF IMBDebug}
          Log.WriteLn('IMB reader thread: invalid magic', llError);
          {$ENDIF}
        end;
      end
      else
      begin
        Result := False; // incomplete command
        {$IFDEF IMBDebug}
        Log.WriteLn('IMB reader thread: incomplete command: '+IntToStr(BytesRead)+' < '+IntToStr(Length(FReadCommandBuffer)), llError);
        {$ENDIF}
      end;
    end;
  end
  else Result := False;
end;

function TIMBConnection.ReadCommand(out aCommand: Integer; var aPayload: TByteBuffer): Boolean;
var
  BytesRead: Integer;
  Continue: Boolean;
  PayloadSize: Integer;
  CheckSum: Integer;
begin
  if Connected then
  begin
    // at first call of recv set flags=0 and not MSG_WAITALL to avoid write wait
    BytesRead := recv(FSocket, FReadCommandBuffer[1], Length(FReadCommandBuffer), 0);
    if (BytesRead=SOCKET_ERROR) OR (BytesRead=0) then
    begin
      if Assigned(FOnSocketError) and (BytesRead<>0)
      then FOnSocketError(Self, FSocket, False);
      Close;
      {$IFDEF IMBDebug}
      if BytesRead<>0
      then Log.WriteLn('IMB reader thread: SOCKET_ERROR: close', llError);
      {$ENDIF}
      Result := False;
    end
    else
    begin
      // check if we got all
      if BytesRead<Length(FReadCommandBuffer)
      then BytesRead := BytesRead+recv(FSocket, FReadCommandBuffer[1+BytesRead], Length(FReadCommandBuffer)-BytesRead, MSG_WAITALL);
      if BytesRead=Length(FReadCommandBuffer) then
      begin
        Continue := True;
        while Continue and not CompareMem(PAnsiChar(FReadCommandBuffer), PAnsiChar(MagicBytes), Length(MagicBytes)) do
        begin
          // skip 1 byte
          Move(FReadCommandBuffer[2], FReadCommandBuffer[1], Length(FReadCommandBuffer)-1);
          Continue := Connected and (recv(FSocket, FReadCommandBuffer[Length(FReadCommandBuffer)-1], 1, MSG_WAITALL)=1);
        end;
        if Continue then
        begin
          Move(FReadCommandBuffer[1+Length(MagicBytes)], aCommand, SizeOf(aCommand));
          Move(FReadCommandBuffer[1+Length(MagicBytes)+SizeOf(aCommand)], PayloadSize, SizeOf(PayloadSize));
          if PayloadSize<=MaxPayloadSize then
          begin
            if PayloadSize>0 then
            begin
              aPayload.Clear(PayloadSize);
              if Connected and (recv(FSocket, PAnsiChar(aPayload.Buffer)^, PayloadSize, MSG_WAITALL)=PayloadSize) then
              begin
                if Connected and (recv(FSocket, CheckSum, SizeOf(CheckSum), MSG_WAITALL)=SizeOf(CheckSum))
                then Result := CheckSum=CheckStringMagic
                else
                begin
                  Result := False;
                  {$IFDEF IMBDebug}
                  Log.WriteLn('IMB reader thread: invalid string magic: '+IntToHex(CheckSum, 8), llError);
                  {$ENDIF}
                end;
              end
              else
              begin
                Result := False;
                {$IFDEF IMBDebug}
                Log.WriteLn('IMB reader thread: incomplete packet (<'+IntToStr(PayloadSize)+')', llError);
                {$ENDIF}
              end;
            end
            else
            begin
              aPayload.Clear;
              Result := True;
            end;
          end
          else
          begin
            aPayload.Clear;
            Result := False;
            {$IFDEF IMBDebug}
            Log.WriteLn('IMB reader thread: over size payload ('+IntToStr(PayloadSize)+')', llError);
            {$ENDIF}
          end;
        end
        else
        begin
          Result := False;
          {$IFDEF IMBDebug}
          Log.WriteLn('IMB reader thread: invalid magic', llError);
          {$ENDIF}
        end;
      end
      else
      begin
        Result := False; // incomplete command
        {$IFDEF IMBDebug}
        Log.WriteLn('IMB reader thread: incomplete command: '+IntToStr(BytesRead)+' < '+IntToStr(Length(FReadCommandBuffer)), llError);
        {$ENDIF}
      end;
    end;
  end
  else Result := False;
end;

function TIMBConnection.ReadCommands(aMaxCommands, aTimeOut: Integer): Integer;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
  Command: Integer;
  Payload: TByteBuffer;
begin
  if Connected then
  begin
    Result := 0;
    FDSet.fd_count := 1;
    FDSet.fd_array[0] := FSocket;
    TimeVal.tv_sec := aTimeOut div 1000;
    TimeVal.tv_usec := (aTimeOut*1000) mod 1000000;
    Payload.Clear;
    if FOldWindows then
    begin
      while Connected and (select(0, @FDSet, nil, nil, @TimeVal)>0) and (Result<aMaxCommands) do
      begin
        if ReadCommandOlderWindows(Command, Payload)
        then HandleCommand(Command, Payload);
        Result := Result+1;
      end;
    end
    else
    begin
      while Connected and (select(0, @FDSet, nil, nil, @TimeVal)>0) and (Result<aMaxCommands) do
      begin
        if ReadCommand(Command, Payload)
        then HandleCommand(Command, Payload);
        Result := Result+1;
      end;
    end;
  end
  else Result := iceConnectionClosed;
end;

procedure TIMBConnection.ReadCommandsByThread(aThread: TMyThread);
var
  Command: Integer;
  Payload: TByteBuffer;
  callClose: Boolean;
begin
  {$IFDEF IMBDebug}
  Log.WriteLn('Enter IMB reader thread');
  {$ENDIF}
  while not aThread.Terminated do
  begin
    if FOldWindows then
    begin
      while Connected and not aThread.Terminated do
      begin
        try
          if ReadCommandOlderWindows(Command, Payload) then
          begin
            {$IFDEF IMBDebug}
            Log.WriteLn('Pre handling IMB command');
            {$ENDIF}
            HandleCommand(Command, Payload);
            {$IFDEF IMBDebug}
            Log.WriteLn('Post handling IMB command');
            {$ENDIF}
          end
          else {$IFDEF IMBDebug}Log.WriteLn('Invalid IMB command', llWarning){$ENDIF};
        except
          on E: Exception do
          begin
            {$IFDEF IMBDebug}
            Log.WriteLn('Exception in IMB reader thread: '+E.Message, llError);
            {$ENDIF}
            callClose := True;
            if Assigned(FOnException)
            then FOnException(Self, E, callClose);
            if callClose
            then Close;
          end;
        end;
      end;
    end
    else
    begin
      while Connected and not aThread.Terminated do
      begin
        try
          if ReadCommand(Command, Payload) then
          begin
            {$IFDEF IMBDebug}
            Log.WriteLn('Pre handling IMB command');
            {$ENDIF}
            HandleCommand(Command, Payload);
            {$IFDEF IMBDebug}
            Log.WriteLn('Post handling IMB command');
            {$ENDIF}
          end
          else {$IFDEF IMBDebug}Log.WriteLn('Invalid IMB command', llWarning){$ENDIF};
        except
          on E: Exception do
          begin
            {$IFDEF IMBDebug}
            Log.WriteLn('Exception in IMB reader thread: '+E.Message, llError);
            {$ENDIF}
            callClose := True;
            if Assigned(FOnException)
            then FOnException(Self, E, callClose);
            if callClose
            then Close;
          end;
        end;
      end;
    end;
    {$IFDEF IMBDebug}
    Log.WriteLn('Exit of inner loop in IMB reader thread');
    {$ENDIF}
    if not (aThread.Terminated or Connected) then
    begin
      aThread.Suspended := True;
      {$IFDEF IMBDebug}
      Log.WriteLn('Suspending  IMB reader thread', llWarning);
      {$ENDIF}
    end;
  end;
  {$IFDEF IMBDebug}
  Log.WriteLn('Exit IMB reader thread');
  {$ENDIF}
end;

procedure TIMBConnection.RemoveHandlers(aObject: TObject);
var
  EventID: Integer;
begin
  FEventListLock.Acquire;
  try
    for EventID := 0 to FEventList.Count-1
    do FEventList.EventEntry[EventID].RemoveHandlers(aObject);
  finally
    FEventListLock.Release;
  end;
end;

procedure TIMBConnection.RemoveStatus;
begin
  FReInitStatus := False;
  if IMB2Compatible
  then SetVariableValue(IntToHex(UniqueClientID,8)+PrefixFederation(OwnerName)+msVarSepChar+ModelStatusVarName, '')
  else SetVariableValuePrefix(PrefixFederation(OwnerName)+msVarSepChar+ModelStatusVarName, '', vpUniqueClientID);
end;

function TIMBConnection.RequestEventNames(const aEventNameFilter: string; aEventFilters: TEventFilters): Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(AnsiString(aEventNameFilter));
  Payload.Prepare(Integer(Byte(aEventFilters)));
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(aEventNameFilter));
  Payload.QWrite(Integer(Byte(aEventFilters)));
  Result := WriteCommand(icRequestEventNames, Payload);
end;

function TIMBConnection.RequestUniqueClientID: Integer;
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(0));
  Payload.Prepare(Integer(0));
  Payload.PrepareApply;
  Payload.QWrite(Integer(0));
  Payload.QWrite(Integer(0));
  Result := WriteCommand(icUniqueClientID, Payload);
end;

procedure TIMBConnection.SetConnected(const aValue: Boolean);
begin
  if aValue xor Connected then
  begin
    if aValue
    then Open(FRemoteHost, FRemotePort)
    else Close;
  end;
end;

procedure TIMBConnection.SetFederation(const aValue: string);
var
  e: Integer;
  OldFederation: string;
  Event: TIMBEventEntry;
begin
  if AnsiCompareText(FFederation, aValue)<>0 then
  begin
    FEventListLock.Acquire;
    try
      // unsubscribe/unpublish
      OldFederation := FFederation;
      if Connected and (OldFederation<>'') then
      begin
        for e := 0 to FEventList.Count - 1 do
        begin
          Event := FEventList.EventEntry[e];
          if Event.IsOnFederation(OldFederation) then
          begin
            if Event.IsSubscribed
            then Event.SignalUnSubscribe(False); // keep state because we need to know it in second stage
            if Event.IsPublished
            then Event.SignalUnPublish; // keep state because we need to know it in second stage
          end;
        end;
      end;
      // now adjust federation
      FFederation := aValue;
      // re-subscribe/re-publish
      if Connected and (OldFederation<>'') then
      begin
        for e := 0 to FEventList.Count - 1 do
        begin
          Event := FEventList.EventEntry[e];
          if Event.IsOnFederation(OldFederation) then
          begin
            // rename event to reflect new federation
            Event.FEventName := Federation+Copy(Event.EventName, Length(OldFederation)+1, Length(Event.EventName)-Length(OldFederation));
            if Event.IsSubscribed
            then Event.SignalSubscribe;
            if Event.IsPublished
            then Event.SignalPublish;
          end;
        end;
      end;
      // if federation is same as before then on change federation is unlinked: relink ?
    finally
      FEventListLock.Release;
    end;
  end;
end;

procedure TIMBConnection.SetKeepAlive(const aValue: Boolean);
begin
  if KeepAlive xor aValue then
  begin
    if Connected then
    begin
      if SetSocketKeepAlive(FSocket, aValue)
      then FKeepAlive := aValue
      else raise Exception.Create(
             'Error '+IntToStr(WSAGetLastError)+' setting Socket Option SO_KEEPALIVE '+
             'to '+BoolToStr(aValue, True)+' on '+AddressFamilyToStr(FAddressFamily));
    end
    else FKeepAlive := aValue;
  end;
end;

procedure TIMBConnection.SetLinger(aValue: Integer);
begin
  if SetSocketLinger(FSocket, aValue)
  then FLingerTime := aValue
  else raise Exception.Create(
         'Error '+IntToStr(WSAGetLastError)+' setting Socket Option SO_LINGER '+
         'to '+IntToStr(aValue)+' on '+ AddressFamilyToStr(FAddressFamily));
end;

procedure TIMBConnection.SetNoDelay(const aValue: Boolean);
var
  Payload: TByteBuffer;
begin
  if NoDelay xor aValue then
  begin
    if SetSocketNoDelay(FSocket, aValue) then
    begin
      FNoDelay := aValue;
      Payload.Clear();
      Payload.Write(aValue);
      WriteCommand(icSetNoDelay, Payload);
    end
    else raise Exception.Create(
           'Error '+IntToStr(WSAGetLastError)+' setting Socket Option TCP_NODELAY '+
           'to '+BoolToStr(aValue, True)+' on '+AddressFamilyToStr(FAddressFamily));
  end;
end;

procedure TIMBConnection.SetOnChangeFederation(aValue: TOnChangeFederation);
begin
  FChangeFederationEvent := Subscribe(FederationChangeEventName);
  FChangeFederationEvent.FOnMultipleChangeFederation.AddHandler(aValue);
end;

procedure TIMBConnection.SetOnChangeFederationNoObj(aSelf: Pointer; aValue: TOnChangeFederationNoObj);
var
  m: TOnChangeFederation;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnChangeFederation := m;
end;

procedure TIMBConnection.SetOnDisconnectNoObj(aSelf: Pointer; aValue: TOnDisconnectNoObj);
var
  m: TOnDisconnect;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnDisconnect := m;
end;

procedure TIMBConnection.SetOnEventNamesNoObj(aSelf: Pointer; aValue: TOnEventNamesNoObj);
var
  m: TOnEventNames;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnEventNames := m;
end;

procedure TIMBConnection.SetOnExceptionNoObj(aSelf: Pointer; aValue: TOnExceptionNoObj);
var
  m: TOnException;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnException := m;
end;

procedure TIMBConnection.SetOnFocus(aValue: TOnFocus);
begin
  if Assigned(aValue) then
  begin
    FFocusEvent := Subscribe(FocusEventName);
    FFocusEvent.FOnFocus := aValue;
  end
  else
  begin
    if Assigned(FFocusEvent)
    then FFocusEvent.FOnFocus := aValue;
  end;
end;

procedure TIMBConnection.SetOnFocusNoObj(aSelf: Pointer; aValue: TOnFocusNoObj);
var
  m: TOnFocus;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnFocus := m;
end;

procedure TIMBConnection.SetOnLogNoObj(aSelf: Pointer; aValue: TOnLogNoObj);
var
  m: TOnLog;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnLog := m;
end;

procedure TIMBConnection.SetOnSocketErrorNoObj(aSelf: Pointer; aValue: TOnSocketErrorNoObj);
var
  m: TOnSocketError;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnSocketError := m;
end;

procedure TIMBConnection.SetOnStatusUpdate(aValue: TOnStatusUpdate);
begin
  FOnStatusUpdate := aValue;
  if Assigned(FOnStatusUpdate) and (not Assigned(FOnVariable)) and Connected
  then WriteCommand(icAllVariables, EmptyByteBuffer^);
end;

procedure TIMBConnection.SetOnStatusUpdateNoObj(aSelf: Pointer; const aValue: TOnStatusUpdateNoObj);
var
  m: TOnStatusUpdate;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnStatusUpdate := m;
end;

procedure TIMBConnection.SetOnVariable(aValue: TOnVariable);
begin
  FOnVariable := aValue;
  if Assigned(FOnVariable) and (not Assigned(FOnStatusUpdate)) and Connected
  then WriteCommand(icAllVariables, EmptyByteBuffer^);
end;

procedure TIMBConnection.SetOnVariableNoObj(aSelf: Pointer; aValue: TOnVariableNoObj);
var
  m: TOnVariable;
begin
  // recalc eventhandler and self to method
  TMethod(m).Code := @aValue;
  TMethod(m).Data := aSelf;
  OnVariable := m;
end;

function TIMBConnection.SetOwner: Integer;
var
  Payload: TByteBuffer;
begin
  if Connected then
  begin
    Payload.Clear;
    Payload.Prepare(OwnerID);
    Payload.Prepare(AnsiString(OwnerName));
    Payload.PrepareApply;
    Payload.QWrite(OwnerID);
    Payload.QWrite(AnsiString(OwnerName));
    Result := WriteCommand(icSetClientInfo, Payload);
  end
  else Result := iceConnectionClosed;
end;

procedure TIMBConnection.SetOwnerID(const aValue: Integer);
begin
  if FOwnerID<>aValue then
  begin
    FOwnerID := aValue;
    SetOwner;
  end;
end;

procedure TIMBConnection.SetOwnerName(const aValue: string);
begin
  if FOwnerName<>aValue then
  begin
    FOwnerName := aValue;
    SetOwner;
  end;
end;

procedure TIMBConnection.SetState(aState: TIMBConnectionState);
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(aState));
  Payload.PrepareApply;
  Payload.QWrite(Integer(aState));
  WriteCommand(icSetState, Payload);
end;

procedure TIMBConnection.SetThrottle(aThrottle: Integer);
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(aThrottle);
  Payload.PrepareApply;
  Payload.QWrite(aThrottle);
  WriteCommand(icSetThrottle, Payload);
end;

procedure TIMBConnection.SetVariableValue(const aVarName: string; const aVarValue: AnsiString);
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(AnsiString(aVarName));
  Payload.Prepare(aVarValue);
  Payload.PrepareApply;
  Payload.QWrite(AnsiString(aVarName));
  Payload.QWrite(aVarValue);
  WriteCommand(icSetVariable, Payload);
end;

procedure TIMBConnection.SetVariableValuePrefix(const aVarName: string; const aVarValue: AnsiString; aVarPrefix: TIMBVarPrefix);
var
  Payload: TByteBuffer;
begin
  Payload.Clear;
  Payload.Prepare(Integer(aVarPrefix));
  Payload.Prepare(AnsiString(aVarName));
  Payload.Prepare(aVarValue);
  Payload.PrepareApply;
  Payload.QWrite(Integer(aVarPrefix));
  Payload.QWrite(AnsiString(aVarName));
  Payload.QWrite(aVarValue);
  WriteCommand(icSetVariablePrefixed, Payload);
end;

function TIMBConnection.SignalBuffer(const aEventName: string; aBufferID: Integer; aBuffer: Pointer; aBufferLen, aEventFlags: Integer; aUseFederationPrefix: Boolean): Integer;
var
  Event: TIMBEventEntry;
begin
  Event := FindEventAutoPublishL(PrefixFederation(aEventName, aUseFederationPrefix));
  if Assigned(Event)
  then Result := Event.SignalBuffer(aBufferID, aBuffer, aBufferLen, aEventFlags)
  else Result := iceNotEventPublished;
end;

function TIMBConnection.SignalChangeFederation(aNewFederationID: Integer; const aNewFederation: string): Integer;
begin
  if not Assigned(FChangeFederationEvent)
  then FChangeFederationEvent := FindEventAutoPublishL(PrefixFederation(FederationChangeEventName));
  if Assigned(FChangeFederationEvent)
  then Result := FChangeFederationEvent.SignalChangeObject(actionChange, aNewFederationID, aNewFederation)
  else Result := iceNotEventPublished;
end;

function TIMBConnection.SignalChangeObject(const aEventName: string; aAction, aObjectID: Integer; const aAttribute: string; aUseFederationPrefix: Boolean): Integer;
var
  Event: TIMBEventEntry;
begin
  Event := FindEventAutoPublishL(PrefixFederation(aEventName, aUseFederationPrefix));
  if Assigned(Event)
  then Result := Event.SignalChangeObject(aAction, aObjectID, aAttribute)
  else Result := iceNotEventPublished;
end;

function TIMBConnection.SignalEvent(const aEventName: string; aEventKind: TIMBEventKind; const aEventPayload: TByteBuffer; aUseFederationPrefix: Boolean=True): Integer;
var
  Event: TIMBEventEntry;
begin
  Event := FindEventAutoPublishL(PrefixFederation(aEventName, aUseFederationPrefix));
  if Assigned(Event)
  then Result := Event.SignalEvent(aEventKind, aEventPayload)
  else Result := iceNotEventPublished;
end;

function TIMBConnection.SignalFocus(const aX, aY: Double): Integer;
var
  Payload: TByteBuffer;
begin
  if not Assigned(FFocusEvent)
  then FFocusEvent := FindEventAutoPublishL(PrefixFederation(FocusEventName));
  if Assigned(FFocusEvent) then
  begin
    Payload.Clear;
    Payload.Prepare(aX);
    Payload.Prepare(aY);
    Payload.PrepareApply;
    Payload.QWrite(aX);
    Payload.QWrite(aY);
    Result := FFocusEvent.SignalEvent(ekChangeObjectEvent, Payload);
  end
  else Result := iceNotEventPublished;
end;

function TIMBConnection.SignalStream(const aEventName, aStreamName: string; aStream: TStream; aUseFederationPrefix: Boolean; aStreamBodyBuffer: Integer): Integer;
var
  Event: TIMBEventEntry;
begin
  Event := FindEventAutoPublishL(PrefixFederation(aEventName, aUseFederationPrefix));
  if Assigned(Event)
  then Result := Event.SignalStream(aStreamName, aStream, aStreamBodyBuffer)
  else Result := iceNotEventPublished;
end;

class function TIMBConnection.StripStatusVarName(const aVarName: string): string;
begin
  Result := EndStrip(aVarName, msVarSepChar+ModelStatusVarName);
end;

function TIMBConnection.Subscribe(const aEventName: string; aHandler: TOnNormalEventNoObj; aSelf: Pointer): TIMBEventEntry;
begin
  Result := Subscribe(aEventName);
  if Assigned(aHandler)
  then Result.OnNormalEventNoObj[aSelf] := aHandler;
end;

function TIMBConnection.Subscribe(const aEventName: string; aHandler: TOnNormalEvent): TIMBEventEntry;
begin
  Result := Subscribe(aEventName);
  if Assigned(aHandler)
  then Result.OnNormalEvent := aHandler;
end;

function TIMBConnection.Subscribe(const aEventName: string; aHandler: TOnChangeObjectNoObj; aSelf: Pointer): TIMBEventEntry;
begin
  Result := Subscribe(aEventName);
  if Assigned(aHandler)
  then Result.OnChangeObjectNoObj[aSelf] := aHandler;
end;

procedure TIMBConnection.Synchronize(aThreadProc: TThreadProcedure);
begin
  (FReaderThread as TMyThread).Synchronize(aThreadProc);
end;

function TIMBConnection.Subscribe(const aEventName: string; aHandler: TOnChangeObject): TIMBEventEntry;
begin
  Result := Subscribe(aEventName);
  if Assigned(aHandler)
  then Result.OnChangeObject := aHandler;
end;

function TIMBConnection.Subscribe(const aEventName: string; aUseFederationPrefix: Boolean): TIMBEventEntry;
begin
  Result := FindOrAddEventL(PrefixFederation(aEventName, aUseFederationPrefix));
  Result.Subscribe;
end;

procedure TIMBConnection.Synchronize(aMethod: TThreadMethod);
begin
  (FReaderThread as TMyThread).Synchronize(aMethod);
end;

function TIMBConnection.UnPublish(const aEventName: string; aUseFederationPrefix: Boolean): TIMBEventEntry;
begin
  Result := FindEventL(PrefixFederation(aEventName, aUseFederationPrefix));
  if Assigned(Result) then
  begin
    // force unpublish
    if Result.FIsPublished>1
    then Result.FIsPublished := 1;
    Result.UnPublish;
  end;
end;

function TIMBConnection.UnSubscribe(const aEventName: string; aHandler: TOnNormalEventNoObj; aSelf: Pointer): TIMBEventEntry;
begin
  Result := FindEventL(PrefixFederation(aEventName));
  if Assigned(Result) then
  begin
    Result.FOnMultipleNormalEvent.RemoveHandler(aHandler, aSelf);
    // if no handlers then force unsubscribe
    if Result.NoHandlers
    then Result.UnSubscribe;
  end;
end;

function TIMBConnection.UnSubscribe(const aEventName: string; aHandler: TOnNormalEvent): TIMBEventEntry;
begin
  Result := FindEventL(PrefixFederation(aEventName));
  if Assigned(Result) then
  begin
    Result.FOnMultipleNormalEvent.RemoveHandler(aHandler);
    // if no handlers then force unsubscribe
    if Result.NoHandlers
    then Result.UnSubscribe;
  end;
end;

function TIMBConnection.UnSubscribe(const aEventName: string; aHandler: TOnChangeObjectNoObj; aSelf: Pointer): TIMBEventEntry;
begin
  Result := FindEventL(PrefixFederation(aEventName));
  if Assigned(Result) then
  begin
    Result.FOnMultipleChangeObject.RemoveHandler(aHandler, aSelf);
    // if no handlers then force unsubscribe
    if Result.NoHandlers
    then Result.UnSubscribe;
  end;
end;

function TIMBConnection.UnSubscribe(const aEventName: string; aHandler: TOnChangeObject): TIMBEventEntry;
begin
  Result := FindEventL(PrefixFederation(aEventName));
  if Assigned(Result) then
  begin
    Result.FOnMultipleChangeObject.RemoveHandler(aHandler);
    // if no handlers then force unsubscribe
    if Result.NoHandlers
    then Result.UnSubscribe;
  end;
end;

function TIMBConnection.UnSubscribe(const aEventName: string; aUseFederationPrefix: Boolean): TIMBEventEntry;
begin
  Result := FindEventL(PrefixFederation(aEventName, aUseFederationPrefix));
  if Assigned(Result) then
  begin
    Result.RemoveAllHandlers;
    // force unsubscribe
    if Result.FIsSubscribed>1
    then Result.FIsSubscribed := 1;
    Result.UnSubscribe;
  end;
end;

procedure TIMBConnection.UpdateStatus(aProgress, aStatus: Integer);
var
  Payload: TByteBuffer;
begin
  FReInitStatus := True;
  Payload.Clear;
  Payload.Prepare(aStatus);
  Payload.Prepare(aProgress);
  Payload.PrepareApply;
  Payload.QWrite(aStatus);
  Payload.QWrite(aProgress);
  // set variable using unique client id
  if IMB2Compatible
  then SetVariableValue(IntToHex(UniqueClientID,8)+UpperCase(PrefixFederation(OwnerName))+msVarSepChar+ModelStatusVarName, Payload.Buffer)
  else SetVariableValuePrefix(UpperCase(PrefixFederation(OwnerName))+msVarSepChar+ModelStatusVarName, Payload.Buffer, vpUniqueClientID);
end;

function TIMBConnection.WriteCommand(aCommand: Integer; const aPayload: TByteBuffer): Integer;
var
  Buffer: TByteBuffer;
begin
  FWriteLock.Acquire;
  try
    // build buffer to send over socket
    Buffer.Clear;
    Buffer.Prepare(MagicBytes[1], Length(MagicBytes));
    Buffer.Prepare(aCommand);
    if not aPayload.IsEmpty then
    begin
      Buffer.Prepare(aPayload.Buffer);
      Buffer.Prepare(CheckStringMagic);
    end
    else Buffer.Prepare(Integer(0));
    Buffer.PrepareApply;
    Buffer.QWrite(MagicBytes[1], Length(MagicBytes));
    Buffer.QWrite(aCommand);
    if not aPayload.IsEmpty then
    begin
      Buffer.QWrite(aPayload.Buffer);
      Buffer.QWrite(CheckStringMagic);
    end
    else Buffer.QWrite(Integer(0));
    // send buffer over socket
    if Connected then
    begin
      Result := send(FSocket, Buffer.Address^, Buffer.Length, 0);
      if Result=SOCKET_ERROR then
      begin
        if Assigned(FOnSocketError)
        then FOnSocketError(Self, FSocket, True);
        Close;
        Result := iceConnectionClosed;
      end;
    end
    else Result := iceConnectionClosed;
  finally
    FWriteLock.Release;
  end;
end;

end.
