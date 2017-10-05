program TilerDebugger;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  StdIni,
  Logger, LogConsole, LogFile,
  imb4,
  WorldDataCode,
  WorldTilerConsts,
  System.Math, System.Classes, System.SysUtils;

const
  USIdleFederation = 'USIdle';

  TilerNameSwitch = 'TilerName';
    //DefaultTilerName = 'vps17642.public.cloudvps.com';
    DefaultTilerName = 'EnSelSrv';

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
  _layerID: Integer;
  discreteColorsOnStretch: Boolean;
  colorRemovedPOI: UInt32;
  colorSamePOI: UInt32;
  colorNewPOI: UInt32;
  width: UInt32;
begin
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        // publisher sending
        (icehTilerSliceID shl 3) or wt64Bit:
          begin
            timeStamp := aBuffer.bb_read_double(aCursor);
            Log.WriteLn('slice ('+FormatDateTime('yyyy-mm-dd hh:nn:ss', timeStamp)+')');//+fSlicetype.toString);
            {
            slice := findSlice(timeStamp);
            if not Assigned(slice) then
            begin
              case fSliceType of
                stReceptor:
                  slice := TSliceReceptor.Create(Self, timeStamp, palette.Clone);
                stGeometry:
                  slice := TSliceGeometry.Create(Self, timeStamp, palette.Clone);
                stGeometryI:
                  slice := TSliceGeometryI.Create(Self, timeStamp, palette.Clone);
                stGeometryIC:
                  slice := TSliceGeometryIC.Create(Self, timeStamp, palette.Clone);
                stGeometryICLR:
                  slice := TSliceGeometryICLR.Create(Self, timeStamp, palette.Clone);
                stPOI:
                  slice := TSlicePOI.Create(Self, timeStamp, poiImages.ToArray);
                stPNG:
                  slice := TSlicePNG.Create(Self, timeStamp, pngExtent, pngImage, discreteColorsOnStretch);
                stLocation:
                  slice := TSliceLocation.Create(Self, timeStamp, palette.Clone);
                // diff slice types
                stDiffReceptor:
                  slice := TSliceDiffReceptor.Create(Self, timeStamp, currentSlice as TSliceReceptor, refSlice as TSliceReceptor, palette.Clone);
                stDiffGeometry:
                  slice := TSliceDiffGeometry.Create(Self, timeStamp, currentSlice as TSliceGeometry, refSlice as TSliceGeometry, palette.Clone);
                stDiffGeometryI:
                  slice := TSliceDiffGeometryI.Create(Self, timeStamp, currentSlice as TSliceGeometryI, refSlice as TSliceGeometryI, palette.Clone);
                stDiffGeometryIC:
                  slice := TSliceDiffGeometryIC.Create(Self, timeStamp, currentSlice as TSliceGeometryIC, refSlice as TSliceGeometryIC, palette.Clone);
                stDiffGeometryICLR:
                  slice := TSliceDiffGeometryICLR.Create(Self, timeStamp, currentSlice as TSliceGeometryICLR, refSlice as TSliceGeometryICLR, palette.Clone);
                stDiffPOI:
                  slice := TSliceDiffPOI.Create(Self, timeStamp, currentSlice as TSlicePOI, refSlice as TSlicePOI, colorRemovedPOI, colorSamePOI, colorNewPOI);
                stDiffPNG:
                  slice := TSliceDiffPNG.Create(Self, timeStamp, currentSlice as TSlicePNG, refSlice as TSLicePNG);
                stDiffLocation:
                  slice := TSliceDiffLocation.Create(Self, timeStamp, currentSlice as TSliceLocation, refSlice as TSliceLocation, palette.Clone);
              end;
              WORMLock.EndRead;
              try
                AddSlice(slice);
                //Log.WriteLn('Added slice ('+slice.id+') for layer '+LayerID.ToString, llNormal, 1);
              finally
                WORMLock.BeginRead;
              end;
            end;
            }
          end;
        // diff slices
        (icehTilerLayer shl 3) or wtVarInt:
          begin
            _layerID := aBuffer.bb_read_int32(aCursor);
            Log.WriteLn('layer id '+_layerID.toString);
          end;
        (icehTilerCurrentSlice shl 3) or wt64Bit:
          begin
            timeStamp := aBuffer.bb_read_double(aCursor);
            Log.WriteLn('cur slice '+_layerID.toString+' ('+FormatDateTime('yyyy-mm-dd hh:nn:ss', timeStamp)+')');
          end;
        (icehTilerRefSlice shl 3) or wt64Bit:
          begin
            timeStamp := aBuffer.bb_read_double(aCursor);
            Log.WriteLn('ref slice '+_layerID.toString+' ('+FormatDateTime('yyyy-mm-dd hh:nn:ss', timeStamp)+')');
          end;
        // POIs
        (icehTilerPOIImage shl 3) or wtLengthDelimited:
          begin
            // load image to poiImages
            aBuffer.bb_read_rawbytestring(aCursor);
            Log.WriteLn('load image to poiImages');
          end;
        (icehTilerPNGExtent shl 3) or wtLengthDelimited:
          begin
            aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
            //size := aBuffer.bb_read_uint64(aCursor);
            //pngExtent.Decode(aBuffer, aCursor, aCursor+size);
            Log.WriteLn('load png extent');
          end;
        (icehTilerPNGImage shl 3) or wtLengthDelimited:
          begin
            // pngImage
            aBuffer.bb_read_rawbytestring(aCursor);
            Log.WriteLn('load png image');
          end;
        (icehTilerDiscreteColorsOnStretch shl 3) or wtVarInt: // boolean
          begin
            discreteColorsOnStretch := aBuffer.bb_read_bool(aCursor);
            Log.WriteLn('discreteColorsOnStretch: '+discreteColorsOnStretch.ToString);
          end;
        (icehTilerColorRemovedPOI shl 3) or wtVarInt: // cardinal=uint32
          begin
            colorRemovedPOI := aBuffer.bb_read_uint32(aCursor);
            Log.WriteLn('colorRemovedPOI: '+colorRemovedPOI.ToString);
          end;
        (icehTilerColorSamePOI shl 3) or wtVarInt: // cardinal=uint32
          begin
            colorSamePOI := aBuffer.bb_read_uint32(aCursor);
            Log.WriteLn('colorSamePOI: '+colorSamePOI.ToString);
          end;
        (icehTilerColorNewPOI shl 3) or wtVarInt: // cardinal=uint32
          begin
            colorNewPOI := aBuffer.bb_read_uint32(aCursor);
            Log.WriteLn('colorNewPOI: '+colorNewPOI.ToString);
          end;
        (icehTilerSliceUpdate shl 3) or wtLengthDelimited:
          begin
            aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
            Log.WriteLn('Slice update');
          end;
        (icehDiscretePalette shl 3) or wtLengthDelimited:
          begin
            aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
//            size := aBuffer.bb_read_uint64(aCursor);
//            palette := TDiscretePalette.Create;
//            palette.Decode(aBuffer, aCursor, aCursor+size);
            Log.WriteLn('decoded discrete palette');
          end;
        (icehRampPalette shl 3) or wtLengthDelimited:
          begin
            aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
//            size := aBuffer.bb_read_uint64(aCursor);
//            palette := TRampPalette.Create;
//            palette.Decode(aBuffer, aCursor, aCursor+size);
            Log.WriteLn('decoded ramp palette');
          end;
        (icehTilerRequestPreviewImage shl 3) or wtVarInt:
          begin
            width := aBuffer.bb_read_uint32(aCursor);
            Log.WriteLn('request preview image, width: '+width.toString);
          end;

        // tiler response
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
              layerEvent := aEventEntry.connection.eventEntry(eventName, False).subscribe;
              if layerEvent.OnEvent.Count=0
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
        then e := aEventEntry.connection.eventEntry(aString, False).subscribe
        else e := aEventEntry;
        // start listening for status replies on specified event entry
        if e.OnString.Count=0
        then e.OnString.Add(handleTilerStatusReply);
      end;
  end;
end;

var
  TilerFQDN: string;
  connection: TConnection;
  tilerEvent: TEventEntry;
  remoteHost: string;
  remotePort: Integer;
begin
  FileLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  ConsoleLogger.SetLogDef([llNormal, llWarning, llError], [llsTime]);
  try
    TilerFQDN := GetSetting(TilerNameSwitch, DefaultTilerName);
    remoteHost := 'app-usimb01.westeurope.cloudapp.azure.com';//GetSetting('RemoteHost', imbDefaultRemoteHost);
    remotePort := GetSetting('RemotePort', imbDefaultRemoteSocketPort);
    connection := TSocketConnection.Create('Tiler debugger', 5, USIdleFederation, remoteHost, remotePort);
    try
      connection.onDisconnect := HandleDisconnect;
      connection.onException := HandleException;
      tilerEvent := connection.eventEntry('USIdle.Tilers.'+tilerFQDN.Replace('.', '_'), False).subscribe;
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
