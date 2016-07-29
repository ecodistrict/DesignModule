program TilerDebugger;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  StdIni,
  Logger, LogConsole, LogFile,
  imb4,
  WorldTilerConsts,
  System.Math, System.Classes, System.SysUtils;

const
  USIdleFederation = 'USIdle';

  TilerNameSwitch = 'TilerName';
    DefaultTilerName = 'vps17642.public.cloudvps.com';

  actionStatus = 4;

procedure HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llError);
end;

procedure handleLayerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  timeStamp: TDateTime;
  id: Integer;
  url: string;
begin
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehTilerID shl 3) or wtVarInt:
          begin
            id := aBuffer.bb_read_int32(aCursor);
          end;
        (icehTilerURL shl 3) or wtLengthDelimited:
          begin
            url := aBuffer.bb_read_string(aCursor);
            log.WriteLn('tiler info: '+id.ToString+' '+url);
          end;
        (icehTilerRefresh shl 3) or wt64Bit:
          begin
            timeStamp := aBuffer.bb_read_double(aCursor);
            if timeStamp=0
            then log.WriteLn('refresh: main slice (0)')
            else log.WriteLn('refresh: slice '+FormatDateTime('yyyy-mm-dd hh:nn', timeStamp));
          end;
        (icehTilerPreviewImage shl 3) or wtLengthDelimited:
          begin
            aBuffer.bb_read_tbytes(aCursor);
            log.WriteLn('preview image');
          end;
        // skip dedicated handler fields
        (icehIntString shl 3) or wtVarInt,
        (icehIntStringPayload shl 3) or wtLengthDelimited,
        (icehString shl 3) or wtLengthDelimited,
        (icehChangeObject shl 3) or wtVarInt,
        (icehChangeObjectAction shl 3) or wtVarInt,
        (icehChangeObjectAttribute shl 3) or wtLengthDelimited,
        (icehStreamHeader shl 3) or wtLengthDelimited,
        (icehStreamBody shl 3) or wtLengthDelimited,
        (icehStreamEnd shl 3) or wtVarInt,
        (icehStreamID shl 3) or wtLengthDelimited:
          aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
        log.WriteLn('handleLayerEvent: unknown field info '+fieldInfo.ToString, llWarning);
      end;
    end;
  except
    on e: Exception
    do log.WriteLn('exception in handleLayerEvent: '+e.Message, llError);
  end;
end;

procedure handleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  tilerStartup: TDateTime;
  sliceType: Integer;
  eventName: string;
  maxEdgeLengthInMeters: Double;
  persistent: Boolean;
  description: string;
  layerEvent: TEventEntry;
begin
  // defaults
  eventName := '';
  maxEdgeLengthInMeters := NaN;
  persistent := false;
  description := '';
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehTilerStartup shl 3) or wt64Bit:
          begin
            tilerStartup := aBuffer.bb_read_double(aCursor);
            log.WriteLn('Received tiler startup: '+FormatDateTime('yyyy-mm-dd hh:nn', tilerStartup));
          end;
        (icehTilerEventName shl 3) or wtLengthDelimited:
          begin
            eventName := aBuffer.bb_read_string(aCursor);
          end;
        (icehTilerEdgeLength shl 3) or wt64Bit:
          begin
            maxEdgeLengthInMeters := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerPersistent shl 3) or wtVarInt:
          begin
            persistent := aBuffer.bb_read_bool(aCursor);
          end;
        (icehTilerLayerDescription shl 3) or wtLengthDelimited:
          begin
            description := aBuffer.bb_read_string(aCursor);
          end;
        (icehTilerRequestNewLayer shl 3) or wtVarInt:
          begin
            sliceType := aBuffer.bb_read_int32(aCursor);
            if eventName<>'' then
            begin
              layerEvent := aEventEntry.connection.subscribe(eventName, False);
              if layerEvent.OnEvent.IndexOf(handleLayerEvent)<0
              then layerEvent.OnEvent.Add(handleLayerEvent);
              log.WriteLn('register new layer '+eventName);
              log.WriteLn('slice type: '+sliceType.ToString, llNormal, 1);
              log.WriteLn('description: '+description, llNormal, 1);
              if not IsNaN(maxEdgeLengthInMeters)
              then log.WriteLn('maxEdgeLengthInMeters: '+maxEdgeLengthInMeters.ToString, llNormal, 1);
              if persistent
              then log.WriteLn('persistent', llNormal, 1);
            end
            else Log.WriteLn('handleTilerEvent: new layer without event name sepcified', llError);
          end;
        // skip dedicated handler fields
        (icehIntString shl 3) or wtVarInt,
        (icehIntStringPayload shl 3) or wtLengthDelimited,
        (icehString shl 3) or wtLengthDelimited,
        (icehChangeObject shl 3) or wtVarInt,
        (icehChangeObjectAction shl 3) or wtVarInt,
        (icehChangeObjectAttribute shl 3) or wtLengthDelimited,
        (icehStreamHeader shl 3) or wtLengthDelimited,
        (icehStreamBody shl 3) or wtLengthDelimited,
        (icehStreamEnd shl 3) or wtVarInt,
        (icehStreamID shl 3) or wtLengthDelimited:
          aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      else
        log.WriteLn('handleTilerEvent: unknown field info: '+fieldInfo.ToString(), llWarning);
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  except
    on e: Exception
    do log.WriteLn('exception in TTiler.handleTilerEvent: '+e.Message, llError);
  end;
end;

procedure handleTilerStatusReply(aEventEntry: TEventEntry; const aString: string);
begin
  Log.WriteLn('Tiler status reply: '+aString);
end;

procedure handleTilerStatus(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
var
  e: TEventEntry;
begin
  case aInt of
    actionStatus:
      begin
        Log.WriteLn('Tiler status request, answer to '+aString);
        if aString<>''
        then e := aEventEntry.connection.subscribe(aString, False)
        else e := aEventEntry;
        // start listening for status replies on specified event entry
        if e.OnString.IndexOf(handleTilerStatusReply)<0
        then e.OnString.Add(handleTilerStatusReply);
      end;
  end;
end;

var
  TilerFQDN: string;
  connection: TConnection;
  tilerEvent: TEventEntry;
begin
  FileLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  ConsoleLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  try
    TilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
    connection := TSocketConnection.Create('Tiler debugger', 5, USIdleFederation);
    try
      connection.onDisconnect := HandleDisconnect;
      connection.onException := HandleException;
      tilerEvent := connection.subscribe('USIdle.Tilers.'+tilerFQDN.Replace('.', '_'), False);
      tilerEvent.OnEvent.Add(handleTilerEvent);
      tilerEvent.OnIntString.Add(handleTilerStatus);
      tilerEvent.OnString.Add(handleTilerStatusReply);
      // subscribe to main tiler event


      WriteLn('Press return to quit');
      ReadLn;
    finally
      connection.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
