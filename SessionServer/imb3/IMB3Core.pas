unit IMB3Core;

{
  payload:
    <integer:event-id><integer:tick><integer:event-kind-and-flags><rest of payload>

    <integer:event-kind-and-flags>
      <bit0..7:  TIMBEventKind>
      <bit8:     priority>
      //<bit9:     overwrite>
      //<bit10:    cleanup>

}

interface

uses
  Contnrs, SyncObjs, Classes, SysUtils, Windows;

const
  MagicBytes                           = AnsiString(#$2F#$47#$61#$71#$95#$AD#$C5#$FB); // 8 bytes
  MaxPayloadSize                       = 10*1024*1024; // in bytes
  CheckStringMagic                     = $10F13467;

  InvalidTranslatedEventID             = -1;

  EventFilterPostFix                   = '*'; // replaces PromiscuousModeEventName, can be post fixed to any event name as filter
  EventNamePartSeperator               = '.';
  DefaultEventNameFilterPrefix         = 'TNOdemo';

  EventNameFilterSwitch                = 'ENF';
    DefaultEventNameFilter             = DefaultEventNameFilterPrefix+EventNamePartSeperator;


  // integer = 4 bytes
  // string  = Count(integer) + ansichar(byte) + ansiChar(byte) ..
  // handle-list = Count(integer) + Handle(integer) + Handle(integer) ..

  // imb2/3 commands
  icHeartBeat                          = -4;
  icEndSession                         = -5;
  icFlushQueue                         = -6;
  icUniqueClientID                     = -7;  // + UniqueClientID(integer) + Handle(integer)
  icTimeStamp                          = -8;  // + absolute time(int64) + Tick(integer) + TickDelta(integer)

  icEvent                              = -15; // + RxEventID(integer) + Tick(Integer) + EventKind(integer) + EventData(string)

  // hub -20..-29

  // hub control
  icEndClientSession                   = -21; // + UniqueClientID(integer)
  icFlushClientQueue                   = -22; // + UniqueClientID(integer)
  icConnectToGateway                   = -23; // + URI[+"|C" | +"|S"]
  icStartUpdate                        = -24; // +event name for stream

  icSetClientInfo                      = -31; // + OwnerID(integer) + OwnerName(string)
  icSetVariable                        = -32; // + VarName(string) + VarValue(string)
  icAllVariables                       = -33;
  icSetState                           = -34; // + State(integer)
  icSetThrottle                        = -35; // + Throttle(integer);
  icSetNoDelay                         = -36; // + State(integer)
  icSetVariablePrefixed                = -37; // + VarPrefix(Integer) + VarName(string) + VarValue(string) -> icSetVariable: VarName(string) + VarValue(string)
  icSetEventNameFilter                 = -38; // + EventNameFilter(string)

  icRequestEventNames                  = -41; // todo: describe
  icEventNames                         = -42; // todo: describe
  //icRequestSubscribers                 = -43; // -> icSubscribe (all events with valid subscribers (not incl. requesting client)
  //icRequestPublishers                  = -44; // -> icPublish (all events with valid subscribers (not incl. requesting client)
  icSubscribe                          = -45; // + TxEventID(integer) + EventEntryType(integer) + EventName(string) -> bcSetEventIDTranslation + RxEventID(integer) + TxEventID(integer)
  icUnsubscribe                        = -46; // + EventName(string) -> bcSetEventIDTranslation + RxEventID(integer) + 0(integer)
  icPublish                            = -47; // + RxEventID(integer) + EventEntryType(integer) + EventName(string)
  icUnpublish                          = -48; // + EventName(string)
  icSetEventIDTranslation              = -49; // + RxEventID(integer) + TxEventID(integer)

  icStatusEvent                        = -52; // + Commands(integer) + ListCommandsPerEventID(count+integer list)
  icStatusClient                       = -53; // + Handle(integer) + Heartbeat(integer) + WrittenCmds(Integer) + WrittenBytes(integer) + ReadCmds(integer) + ReadBytes(integer) + State(Integer) + QueueLength(Integer) + MaxQueueLength(integer)
  icStatusEventPlus                    = -54; // + EventID(integer) + EventName(string) + SubscriberList(handle-list) + PublisherList(handle-list)
  icStatusClientPlus                   = -55; // + Handle(integer) + OwnerID(integer) + OwnerName(string) + URI(string) + IsServer(bool) + NoDelay(bool) + Throttle(integer)
  icStatusHUB                          = -56; // + Commands(Integer) + Bytes(Integer)
  icStatusTimer                        = -57;

  icHumanReadableHeader                = -60;
  icSetMonitor                         = -61;
  icResetMonitor                       = -62;

  //icSetTimer                           = -71; // + DueTime(int64) + Period(int64) + RxEventID(integer) [+ EventKind(integer) + EventData(string)]
  // if no EventData is specified (length=0) icEvent will be sent with kind=ekTimerEvent and EventData=AbsoluteTime(Int64)
  //icCancelTimer                        = -72; // + DueTime(int64) + Period(int64) + RxEventID(integer)
  icCreateTimer                        = -73;

  // locator commands (udp)
  icHUBLocate                          = -81; // -> icHUBFound
  icHUBFound                           = -82; // URI(string)

  // log off HUB
  icLogClear                           = -91;
  icLogRequest                         = -92; // -> icLogContents + Text(string)
  icLogContents                        = -93; // Text(string)

const
  // imb3 data types
  ivtUnknown                           = -1;
  ivtInteger                           = -2;
  ivtInt64                             = -3;
  ivtDouble                            = -4;
  ivtBoolean                           = -5;
  ivtAnsiString                        = -6;
  ivtWideString                        = -7;
  ivtDoublesArray                      = -20;
  ivtDoublesGrid                       = -21;
  ivtIntegerArray                      = -22;
  ivtIntegerGrid                       = -23;
  ivtNetCDF                            = -30;
  ivtXML                               = -40;
  ivtXMLCompressed                     = -41;

  ivtNotAValue                         = 0;

  // log levels
  // translated to integer from TLogLevel in Logger
  lliRemark = 0;
  lliDump = 1;
  lliNormal = 2;
  lliStart = 3;
  lliFinish = 4;
  lliPush = 5;
  lliPop = 6;
  lliStamp = 7;
  lliSummary = 8;
  lliWarning = 9;
  lliError = 10;
  lliOK = 11;

type
  TIMBConnectionState = (
    icsUninitialized=0,
    icsInitialized=1,
    icsClient=2,
    icsHub=3,
    icsEnded=4,
    icsTimer=10,
    // room for extensions ..
    // gateway values are used over network and should be same over all connected clients/brokers
    icsGateway=100, // equal
    icsGatewayClient=101, // this gateway acts as a client; subscribes are not received
    icsGatewayServer=102 // this gateway treats connected broker as client
  );

  TIMBConnectionStates = set of TIMBConnectionState;

type
  // TIMBEventKind must fit in EventKindMask, higher bits (EventFlagsMask) are used for option flags
  TIMBEventKind = (
    // imb version 1
    ekChangeObjectEvent=0,
    // imb version 2
    ekStreamHeader=1,
    ekStreamBody=2,
    ekStreamTail=3,
    ekBuffer=4,
    ekNormalEvent=5,
    // imb version 3
    //ekChangeObjectDataEvent=6,
    // child events
    //ekChildEventAdd=11,
    //ekChildEventRemove=12,
    //log
    ekLogWriteLn=30,
    //ekLogProgress=31,
    //ekLogLeaveProgress=32
    // timer
    ekTimerCancel=40,
    ekTimerPrepare=41,
    ekTimerStart=42,
    ekTimerStop=43,
    //ekTimerReInit=44,
    ekTimerAcknowledgedListAdd=45,
    ekTimerAcknowledgedListRemove=46,
    ekTimerSetSpeed=47,
    ekTimerTick=48,
    ekTimerAcknowledge=49,
    ekTimerStatusRequest=50
  );

  // todo: document payload structure for all TIMBEventKind..

  TConnectionWriterState = (
    cwsIdle,
    cwsWriting,
    cwsStreaming
  );

  TIMBVarPrefix = (
    vpUniqueClientID,
    vpClientHandle
  );

  // do not change order or insert items (only add items) because enum is send over network
  TEventFilter = (
    efPublishers,
    efSubscribers,
    efTimers
  );

  TEventFilters                        = set of TEventFilter;

const
  // event properties
  // mask the event kind part
  EventKindMask                        = $000000FF;
  // mask the event flags part
  EventFlagsMask                       = $0000FF00;
  // specific event flags
  efPriority                           = $00000100;
  //efOverwrite                          = $00000200;
  //efCleanup                            = $00000400;

  trcInfinite                          = MaxInt; // send infinite timer events

const // timer state
  tsCreated                            = 0;
  tsPrepared                           = 1;
  tsRunning                            = 2;
  tsStopped                            = 3;

type
  TIMBEventTranslation = class
  constructor Create;
  destructor  Destroy; override;
  private
    FEventTranslation                  : array of Integer;
  public
    function  TranslateEventID         ( aRxEventID: Integer): Integer;
    procedure SetEventTranslation      ( aRxEventID, aTxEventID: Integer);
    procedure ResetEventTranslation    ( aTxEventID: Integer);
  end;

function IMBStateToStr(aState: TIMBConnectionState): string;
function CommandToStr(aCommand: Integer): string;
function EventKindToStr(aEventKind: TIMBEventKind): string;

implementation

function IMBStateToStr(aState: TIMBConnectionState): string;
begin
  case aState of
    icsUninitialized:  Result := 'U';
    icsInitialized:    Result := 'I';
    icsClient:         Result := '';
    icsHub:            Result := 'H';
    icsEnded:          Result := 'E';
    icsTimer:          Result := 'T';
    icsGateway:        Result := 'G';
    icsGatewayClient:  Result := 'GC';
    icsGatewayServer:  Result := 'GS';
  else
                       Result := '#'+IntToStr(Ord(aState))+'#';
  end;
end;

function CommandToStr(aCommand: Integer): string;
begin
  case aCommand of
    icHeartBeat:                       Result := 'heart beat';
    icEndSession:                      Result := 'end session';
    icFlushQueue:                      Result := 'flush queue';
    icEvent:                           Result := 'event';
    icSetClientInfo:                   Result := 'set client info';
    icSetVariable:                     Result := 'set variable';
    icAllVariables:                    Result := 'all variables';
    icSetState:                        Result := 'set state';
    icSetThrottle:                     Result := 'set throttle';
    icSetNoDelay:                      Result := 'set no-delay (nagle)';

    icSetVariablePrefixed:             Result := 'set variable (prefixed)';

    icEndClientSession:                Result := 'end client session';
    icFlushClientQueue:                Result := 'flush client queue';
    icConnectToGateway:                Result := 'connect to gateway';
    icStartUpdate:                     Result := 'start update';

    icSubscribe:                       Result := 'subscribe';
    icUnsubscribe:                     Result := 'unsubscribe';
    icPublish:                         Result := 'publish';
    icUnpublish:                       Result := 'unpublish';
    icSetEventIDTranslation:           Result := 'set event id translation';
    icStatusEvent:                     Result := 'status event';
    icStatusEventPlus:                 Result := 'status event plus';
    icStatusClient:                    Result := 'status client';
    icStatusClientPlus:                Result := 'status client plus';
    icStatusTimer:                     Result := 'status timer';

    icStatusHUB:                       Result := 'status HUB';
    icSetMonitor:                      Result := 'set monitor';
    icResetMonitor:                    Result := 'reset monitor';
    icHUBLocate:                       Result := 'HUB locate';
    icHUBFound:                        Result := 'HUB found';
    icLogClear:                        Result := 'log clear';
    icLogRequest:                      Result := 'log request';
    icLogContents:                     Result := 'log contents';
    icUniqueClientID:                  Result := 'unique client id';
  else
                                       Result := 'unknown ('+IntToStr(aCommand)+')';
  end;
end;

function EventKindToStr(aEventKind: TIMBEventKind): string;
begin
  case aEventKind of
    ekChangeObjectEvent:               Result := 'change object';
    ekStreamHeader:                    Result := 'stream header';
    ekStreamBody:                      Result := 'stream body';
    ekStreamTail:                      Result := 'stream tail';
    ekBuffer:                          Result := 'buffer event';
    ekNormalEvent:                     Result := 'normal event';
//    ekChildEventAdd:                   Result := 'add child event';
//    ekChildEventRemove:                Result := 'remove child event';
    ekTimerCancel:                     Result := 'timer cancel';
    ekTimerPrepare:                    Result := 'timer prepare';
    ekTimerStart:                      Result := 'timer start';
    ekTimerStop:                       Result := 'timer stop';
    ekTimerAcknowledgedListAdd:        Result := 'timer acknowledge list add';
    ekTimerAcknowledgedListRemove:     Result := 'timer acknowledge list remove';
    ekTimerSetSpeed:                   Result := 'timer set speed';
    ekTimerTick:                       Result := 'timer tick';
    ekTimerAcknowledge:                Result := 'timer acknowledge';
    ekLogWriteLn:                      Result := 'log write line';
  else
                                       Result := 'unknown event kind';
  end;
end;

{ TIMBEventTranslation }

constructor TIMBEventTranslation.Create;
begin
  inherited;
  SetLength(FEventTranslation, 0);
end;

destructor TIMBEventTranslation.Destroy;
begin
  SetLength(FEventTranslation, 0);
  inherited;
end;

procedure TIMBEventTranslation.ResetEventTranslation(aTxEventID: Integer);
var
  e: Integer;
begin
  for e := Low(FEventTranslation) to High(FEventTranslation) do
  begin
    if FEventTranslation[e]=aTxEventID
    then FEventTranslation[e] := InvalidTranslatedEventID;
  end;
end;

procedure TIMBEventTranslation.SetEventTranslation(aRxEventID, aTxEventID: Integer);
begin
  // todo: change to power of 2 growing?
  if aRxEventID>High(FEventTranslation)
  then SetLength(FEventTranslation, aRxEventID+1); // auto null extra entries
  FEventTranslation[aRxEventID] := aTxEventID;
end;

function TIMBEventTranslation.TranslateEventID(aRxEventID: Integer): Integer;
begin
  if (Low(FEventTranslation)<=aRxEventID) AND (aRxEventID<=High(FEventTranslation))
  then Result := FEventTranslation[aRxEventID]
  else Result := InvalidTranslatedEventID;
end;


end.
