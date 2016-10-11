unit TilerControl;

interface

uses
  StdIni,
  imb4,
  WorldDataCode,
  WorldLegends,
  WorldTilerConsts, // iceh* tiler consts

  Int64Time,

  Vcl.graphics,
  Vcl.Imaging.pngimage,

  Logger, // after bitmap units (also use log)

  IdCoderMIME, // base64

  // web request
  {$IFDEF WebRequestWinInet}
  Winapi.WinInet,
  {$ELSE}
  IdHTTP,
  {$ENDIF}

  System.Math,
  System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.SysUtils;

const
  PreviewImageWidth = 64; // default width/height of a minuture layer preview in pixels

  actionStatus = 4;

  sepElementID = '$';
  sepEventName = '.';

  DefaultTilerPort = 4503;

type
  TTilerLayer = class; // forward

  TOnRefresh = reference to procedure(aTilerLayer: TTilerLayer; aTimeStamp: TDateTime);
  TOnTilerInfo = reference to procedure(aTilerLayer: TTilerLayer);
  TOnPreview = reference to procedure(aTilerLayer: TTilerLayer);

  TTiler = class; // forward

  TTilerLayer = class
  constructor Create(aConnection: TConnection; const aElementID: string; aSliceType: Integer; aPalette: TWDPalette=nil; aID: Integer=-1; const aURL: string=''; aPreview: TPngImage=nil);
  destructor Destroy; override;
  private
    //fTiler: TTiler;
    fEvent: TEventEntry;
    // send
    fElementID: string;
    fSliceType: Integer;
    fPalette: TWDPalette;
    // return
    fID: Integer;
    fURL: string;
    fPreview: TPngImage;
    // events
    fOnTilerInfo: TOnTilerInfo;
    fOnRefresh: TOnRefresh;
    fOnPreview: TOnPreview;
    function getURITimeStamp: string;
    function getURLTimeStamped: string;
    procedure handleLayerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    function LoadPreviewFromCache(): Boolean;
  public
    property event: TEventEntry read fEvent;
    property elementID: string read fElementID;
    property sliceType: Integer read fSliceType;
    property palette: TWDPalette read fPalette;
    property ID: Integer read fID;
    property URL: string read fURL;
    property URLTimeStamped: string read getURLTimeStamped;
    property preview: TPngImage read fPreview;
    function previewAsBASE64: string;
    // events
    property onTilerInfo: TOnTilerInfo read fOnTilerInfo write fOnTilerInfo;
    property onRefresh: TOnRefresh read fOnRefresh write fOnRefresh;
    property onPreview: TOnPreview read fOnPreview write fOnPreview;
    // signal layer info
    procedure signalRegisterLayer(aTiler: TTiler; const aDescription: string; aPersistent: Boolean=False; aEdgeLengthInMeters: Double=NaN);
    // add slice with specific data for slice types
    procedure signalAddSlice(aPalette: TWDPalette=nil; aTimeStamp: TDateTime=0);
    procedure signalAddPOISlice(const aPOIImages: TArray<TPngImage>; aTimeStamp: TDateTime=0);
    procedure signalAddPNGSlice(aPNG: TPngImage; const aExtent: TWDExtent; aDiscreteColorsOnStretch: Boolean=True; aTimeStamp: TDateTime=0);
    procedure signalAddDiffSlice(
      aPalette: TWDPalette;
      aCurrentLayerID, aReferenceLayerID: Integer;
      aCurrentTimeStamp: TDateTime=0; aReferenceTimeStamp: TDateTime=0; aTimeStamp: TDateTime=0);
    procedure signalAddDiffPOISlice(
      aColorRemovedPOI, aColorSamePOI, aColorNewPOI: TAlphaRGBPixel;
      aCurrentLayerID, aReferenceLayerID: Integer;
      aCurrentTimeStamp: TDateTime=0; aReferenceTimeStamp: TDateTime=0; aTimeStamp: TDateTime=0);
    // slice properties and data
    procedure signalPalette(aPalette: TWDPalette; aTimeStamp: TDateTime=0); // update palette
    procedure signalData(const aData: TByteBuffer; aTimeStamp: TDateTime=0);
    // when any slice is build
    procedure signalRequestPreview;
  end;

  TOnTilerStartup = reference to procedure(aTiler: TTiler; aStartupTime: TDateTime);
  TOnTilerStatus = reference to function(aTiler: TTiler): string;

  TTiler = class
  constructor Create(aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string);
  destructor Destroy; override;
  private
    fEvent: TEventEntry;
    fOnTilerStartup: TOnTilerStartup;
    fOnTilerStatus: TOnTilerStatus;
    fTilerStatusURL: string;
    procedure handleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    procedure handleTilerStatus(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
  public
    property event: TEventEntry read fEvent;
    property onTilerStartup: TOnTilerStartup read fOnTilerStartup write fOnTilerStartup;
    property onTilerStatus: TOnTilerStatus read fOnTilerStatus write fOnTilerStatus;
    function getTilerStatus: string;
  end;

function ImageToBytes(aImage: TPngImage): TBytes;
function GraphicToBytes(aGraphic: TGraphic): TBytes;
procedure BytesToImage(const aBytes: TBytes; aImage: TPngImage);
function ImageToBase64(aImage: TPngImage): string;

function TilerStatusURLFromTilerName(const aTilerName: string; aTilerPort: Integer=DefaultTilerPort): string;

implementation

{ utils }

function ImageToBytes(aImage: TPngImage): TBytes;
var
  stream: TBytesStream;
begin
  stream := TBytesStream.Create;
  try
    aImage.SaveToStream(stream);
  	Result := stream.Bytes;
  finally
    stream.Free;
  end;
end;

function GraphicToBytes(aGraphic: TGraphic): TBytes;
var
  stream: TBytesStream;
begin
  stream := TBytesStream.Create;
  try
    aGraphic.SaveToStream(stream);
  	Result := stream.Bytes;
  finally
    stream.Free;
  end;
end;

{
function CreateImageFromBytes(const aBytes: TBytes): TPngImage;
var
  stream: TBytesStream;
begin
  stream := TBytesStream.Create(aBytes);
  try
    Result := TPngImage.Create;
    Result.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;
}
procedure BytesToImage(const aBytes: TBytes; aImage: TPngImage);
var
  stream: TBytesStream;
begin
  stream := TBytesStream.Create(aBytes);
  try
    aImage.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

function ImageToBase64(aImage: TPngImage): string;
var
  ms: TMemoryStream;
begin
  if Assigned(aImage) then
  begin
    ms := TMemoryStream.Create;
    try
      aImage.SaveToStream(ms);
      ms.Position := 0;
      Result := 'data:image/png;base64,'+TIdEncoderMIME.EncodeStream(ms);
    finally
      ms.Free;
    end;
  end
  else Result := '';
end;

// http://stackoverflow.com/questions/301546/whats-the-simplest-way-to-call-http-get-url-using-delphi

{$IFDEF WebRequestWinInet}
function WebRequest(const aURL: string): string;
var
  NetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[0..1024] of Char;
  BytesRead: dWord;
begin
  Result := '';
  NetHandle := InternetOpen('Delphi 5.x', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(NetHandle) then
  begin
    UrlHandle := InternetOpenUrl(NetHandle, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);

    if Assigned(UrlHandle) then
      { UrlHandle valid? Proceed with download }
    begin
      FillChar(Buffer, SizeOf(Buffer), 0);
      repeat
        Result := Result + Buffer;
        FillChar(Buffer, SizeOf(Buffer), 0);
        InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
      until BytesRead = 0;
      InternetCloseHandle(UrlHandle);
    end
    else
      { UrlHandle is not valid. Raise an exception. }
      raise Exception.CreateFmt('Cannot open URL %s', [Url]);

    InternetCloseHandle(NetHandle);
  end
  else
    { NetHandle is not valid. Raise an exception }
    raise Exception.Create('Unable to initialize Wininet');
end;
{$ELSE}
function WebRequest(const aURL: string): string;
var
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create;
  try
    Result := lHTTP.Get(aURL);
  finally
    lHTTP.Free;
  end;
end;
{$ENDIF}

function TilerStatusURLFromTilerName(const aTilerName: string; aTilerPort: Integer): string;
begin
  Result := 'http://'+aTilerName+'/tiler/TilerWebService.dll/status';
end;

{ TTilerLayer }

constructor TTilerLayer.Create(aConnection: TConnection; const aElementID: string; aSliceType: Integer; aPalette: TWDPalette; aID: Integer; const aURL: string; aPreview: TPngImage);
begin
  inherited Create;
  fElementID := aElementID;
  fSliceType := aSliceType;
  fPalette := aPalette;
  fID := aID;
  fURL := aURL;
  fPreview := aPreview;
  fOnTilerInfo := nil;
  fOnRefresh := nil;
  fOnPreview := nil;
  // try to load preview from cache
  if not Assigned(fPreview)
  then LoadPreviewFromCache();
  // define layer event name and make active
  fEvent := aConnection.subscribe('USIdle.Sessions.TileLayers.'+aElementID.Replace(sepElementID, sepEventName), False);
  fEvent.OnEvent.Add(handleLayerEvent);
end;

destructor TTilerLayer.Destroy;
begin
  if Assigned(fEvent) then
  begin
    fEvent.OnEvent.Remove(handleLayerEvent);
    fEvent.unSubscribe();
    fEvent := nil;
  end;
  FreeAndNil(fPreview);
  FreeAndNil(fPalette);
  inherited;
end;

function TTilerLayer.getURITimeStamp: string;
begin
  Result := NowUTCInt64.ToString();
end;

function TTilerLayer.getURLTimeStamped: string;
begin
  if fURL<>''
  then Result := fURL+'&ts='+getURITimeStamp
  else Result := '';
end;

procedure TTilerLayer.handleLayerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  timeStamp: TDateTime;
  previewsFolder: string;
  bytes: TBytes;
begin
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehTilerID shl 3) or wtVarInt:
          begin
            fID := aBuffer.bb_read_int32(aCursor);
          end;
        (icehTilerURL shl 3) or wtLengthDelimited:
          begin
            fURL := aBuffer.bb_read_string(aCursor);
            if Assigned(fOnTilerInfo) and Assigned(Self) then
            begin
              try
                fOnTilerInfo(self); // -> AddCommandToQueue(Self, Self.signalObjects);
              except
                on e: Exception
                do Log.WriteLn('Exception TTilerLayer.handleLayerEvent handling OnTilerInfo: '+e.Message, llError);
              end;
            end;
          end;
        (icehTilerRefresh shl 3) or wt64Bit:
          begin
            timeStamp := aBuffer.bb_read_double(aCursor);
            if Assigned(fOnRefresh) and Assigned(Self) then
            begin
              try
                fOnRefresh(self, timeStamp);
              except
                on e: Exception
                do Log.WriteLn('Exception TTilerLayer.handleLayerEvent handling OnRefresh: '+e.Message, llError);
              end;
            end;
            {
            if timeStamp<>0
            then timeStampStr := FormatDateTime('yyyy-mm-dd hh:mm', timeStamp)
            else timeStampStr := '';
            // todo: start refresh timer
            // signal refresh to layer client
            tiles := uniqueObjectsTilesLink;
            TMonitor.Enter(clients);
            try
              for client in clients
              do client.SendRefresh(elementID, timeStampStr, tiles);
            finally
              TMonitor.Exit(clients);
            end;
            // signal refresh to scenario client
            TMonitor.Enter(fScenario.clients);
            try
              for client in fScenario.clients do
              begin
                if not clients.Contains(client)
                then client.SendRefresh(elementID, timeStampStr, tiles);
              end;
            finally
              TMonitor.Exit(fScenario.clients);
            end;
            // refresh preview also
            //Log.WriteLn('set preview timer for '+elementID);
            fPreviewRequestTimer.DueTimeDelta := DateTimeDelta2HRT(dtOneMinute/6);
            }
          end;
        (icehTilerPreviewImage shl 3) or wtLengthDelimited:
          begin
            try
              bytes := aBuffer.bb_read_tbytes(aCursor);
              fPreview.Free;
              fPreview := TPngImage.Create;
              if Assigned(fPreview) then
              begin
                BytesToImage(bytes, fPreview);
                // cache preview
                previewsFolder := ExtractFilePath(ParamStr(0))+'previews';
                ForceDirectories(previewsFolder);
                fPreview.SaveToFile(previewsFolder+'\'+elementID+'.png');
                if Assigned(fOnPreview) and Assigned(Self) then
                begin
                  try
                    fOnPreview(self);
                  except
                    on E: Exception
                    do Log.WriteLn('Exception TTilerLayer.handleLayerEvent handling OnPreview: '+e.Message, llError);
                  end;
                end;
              end
              else Log.WriteLn('TTilerLayer.handleLayerEvent handling icehTilerPreviewImage: could not create preview png', llError);
              { ->
              pvBASE64 := previewBASE64;
              // layer clients
              TMonitor.Enter(clients);
              try
                for client in fClients
                do client.SendPreview(elementID, pvBASE64);
              finally
                TMonitor.Exit(clients);
              end;
              // scenario clients
              TMonitor.Enter(fScenario.clients);
              try
                for client in fScenario.clients do
                begin
                  if not clients.Contains(client)
                  then client.SendPreview(elementID, pvBASE64);
                end;
              finally
                TMonitor.Exit(fScenario.clients);
              end;
              }
            except
              on E: Exception
              do Log.WriteLn('Exception TTilerLayer.handleLayerEvent handling icehTilerPreviewImage: '+e.Message, llError);
            end;
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  except
    on e: Exception
    do log.WriteLn('exception in TTilerLayer.handleLayerEvent: '+e.Message, llError);
  end;
end;

function TTilerLayer.LoadPreviewFromCache: Boolean;
var
  previewFilename: string;
begin
  // try to load preview from cache
  try
    previewFilename := ExtractFilePath(ParamStr(0))+'previews'+'\'+fElementID+'.png';
    if FileExists(previewFilename) then
    begin
      fPreview.Free;
      fPreview := TPngImage.Create;
      fPreview.LoadFromFile(previewFilename);
      Result := True;
    end
    else Result := False;
  except
    Result := False;
  end;
end;

function TTilerLayer.previewAsBASE64: string;
begin
  Result := ImageToBase64(fPreview);
end;

procedure TTilerLayer.signalAddDiffPOISlice(aColorRemovedPOI, aColorSamePOI, aColorNewPOI: TAlphaRGBPixel; aCurrentLayerID,
  aReferenceLayerID: Integer; aCurrentTimeStamp, aReferenceTimeStamp, aTimeStamp: TDateTime);
var
  buffer: TByteBuffer;
begin
  buffer :=
    TByteBuffer.bb_tag_uint32(icehTilerColorRemovedPOI, aColorRemovedPOI)+
    TByteBuffer.bb_tag_uint32(icehTilerColorSamePOI, aColorSamePOI)+
    TByteBuffer.bb_tag_uint32(icehTilerColorNewPOI, aColorNewPOI)+
    TByteBuffer.bb_tag_int32(icehTilerLayer, aCurrentLayerID)+
    TByteBuffer.bb_tag_double(icehTilerCurrentSlice, aCurrentTimeStamp)+
    TByteBuffer.bb_tag_int32(icehTilerLayer, aReferenceLayerID)+
    TByteBuffer.bb_tag_double(icehTilerRefSlice, aReferenceTimeStamp)+
    TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp);
  fEvent.signalEvent(buffer);
end;

procedure TTilerLayer.signalAddDiffSlice(aPalette: TWDPalette; aCurrentLayerID, aReferenceLayerID: Integer; aCurrentTimeStamp, aReferenceTimeStamp, aTimeStamp: TDateTime);
var
  buffer: TByteBuffer;
begin
  if Assigned(aPalette) then
  begin
    fPalette.Free;
    fPalette := aPalette;
  end;
  if Assigned(fPalette)
  then buffer := TByteBuffer.bb_tag_rawbytestring(fPalette.wdTag, fPalette.Encode)
  else buffer :='';
  buffer := buffer+
    TByteBuffer.bb_tag_int32(icehTilerLayer, aCurrentLayerID)+
    TByteBuffer.bb_tag_double(icehTilerCurrentSlice, aCurrentTimeStamp)+
    TByteBuffer.bb_tag_int32(icehTilerLayer, aReferenceLayerID)+
    TByteBuffer.bb_tag_double(icehTilerRefSlice, aReferenceTimeStamp)+
    TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp);
  fEvent.signalEvent(buffer);
end;

procedure TTilerLayer.signalAddPNGSlice(aPNG: TPngImage; const aExtent: TWDExtent; aDiscreteColorsOnStretch: Boolean; aTimeStamp: TDateTime);
var
  buffer: TByteBuffer;
begin
  buffer :=
    TByteBuffer.bb_tag_rawbytestring(icehTilerPNGExtent, aExtent.Encode)+
    TByteBuffer.bb_tag_tbytes(icehTilerPNGImage, ImageToBytes(aPNG))+
    TByteBuffer.bb_tag_bool(icehTilerDiscreteColorsOnStretch, aDiscreteColorsOnStretch)+
    TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp);
  fEvent.signalEvent(buffer);
end;

procedure TTilerLayer.signalAddPOISlice(const aPOIImages: TArray<TPngImage>; aTimeStamp: TDateTime);
var
  buffer: TByteBuffer;
  poi: TPngImage;
begin
  buffer :='';
  for poi in aPOIImages
  do buffer := buffer+TByteBuffer.bb_tag_tbytes(icehTilerPOIImage, ImageToBytes(poi));
  buffer := buffer+TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp);
  fEvent.signalEvent(buffer);
end;

procedure TTilerLayer.signalAddSlice(aPalette: TWDPalette; aTimeStamp: TDateTime);
var
  buffer: TByteBuffer;
begin
  if Assigned(aPalette) then
  begin
    fPalette.Free;
    fPalette := aPalette;
  end;
  // first all layer properties
  if Assigned(fPalette)
  then buffer := TByteBuffer.bb_tag_rawbytestring(fPalette.wdTag, fPalette.Encode)
  else buffer :='';
  buffer := buffer+TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp);
  fEvent.signalEvent(buffer);
end;

procedure TTilerLayer.signalData(const aData: TByteBuffer; aTimeStamp: TDateTime);
begin
  fEvent.signalEvent(
    TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp)+
    TByteBuffer.bb_tag_rawbytestring(icehTilerSliceUpdate, aData));
end;

procedure TTilerLayer.signalPalette(aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  fPalette.Free;
  fPalette := aPalette;
  if Assigned(fPalette) then
  begin
    fEvent.signalEvent(
      TByteBuffer.bb_tag_rawbytestring(fPalette.wdTag, fPalette.Encode)+
      TByteBuffer.bb_tag_double(icehTilerSliceID, aTimeStamp));
  end;
end;

procedure TTilerLayer.signalRegisterLayer(aTiler: TTiler; const aDescription: string; aPersistent: Boolean; aEdgeLengthInMeters: Double);
var
  payload: TByteBuffer;
begin
  payload :=
    TByteBuffer.bb_tag_string(icehTilerEventName, fEvent.eventName);
  if not IsNaN(aEdgeLengthInMeters)
  then payload := Payload+
    TByteBuffer.bb_tag_double(icehTilerEdgeLength, aEdgeLengthInMeters);
  payload := Payload+
    TByteBuffer.bb_tag_string(icehTilerLayerDescription, aDescription)+
    TByteBuffer.bb_tag_bool(icehTilerPersistent, aPersistent)+
    TByteBuffer.bb_tag_int32(icehTilerRequestNewLayer, fSliceType); // last to trigger new layer request
  aTiler.event.signalEvent(payload);
end;

procedure TTilerLayer.signalRequestPreview;
begin
  fEvent.signalEvent(TByteBuffer.bb_tag_uint32(icehTilerRequestPreviewImage, PreviewImageWidth));
end;

{ TTiler }

constructor TTiler.Create(aConnection: TConnection; const aTilerFQDN, aTilerStatusURL: string);
begin
  inherited Create;
  fOnTilerStartup := nil;
  fTilerStatusURL := aTilerStatusURL;
  //fLayers := TObjectDictionary<string, TTilerLayer>.Create([doOwnsValues]);
  fEvent := aConnection.subscribe('USIdle.Tilers.'+aTilerFQDN.Replace('.', '_'), False);
  if not fEvent.OnEvent.Contains(handleTilerEvent)
  then fEvent.OnEvent.Add(handleTilerEvent);
  if not fEvent.OnIntString.Contains(handleTilerStatus)
  then fEvent.OnIntString.Add(handleTilerStatus);
  // start tiler web service
  if fTilerStatusURL<>''
  then getTilerStatus;
end;

destructor TTiler.Destroy;
begin
  if Assigned(fEvent) then
  begin
    fEvent.OnIntString.remove(handleTilerStatus);
    fEvent.OnEvent.Remove(handleTilerEvent);
    fEvent := nil;
  end;
  //FreeAndNil(fLayers);
  inherited;
end;

function TTiler.getTilerStatus: string;
begin
  Result := WebRequest(fTilerStatusURL);
end;

procedure TTiler.handleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  tilerStartup: TDateTime;
begin
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehTilerStartup shl 3) or wt64Bit:
          begin
            tilerStartup := aBuffer.bb_read_double(aCursor);
            if Assigned(fOnTilerStartup)
            then fOnTilerStartup(Self, tilerStartup);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  except
    on e: Exception
    do log.WriteLn('exception in TTiler.handleTilerEvent: '+e.Message, llError);
  end;
end;

procedure TTiler.handleTilerStatus(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
var
  e: TEventEntry;
  status: string;
  info: string;
begin
  case aInt of
    actionStatus:
      begin
        if aString<>''
        then e := aEventEntry.connection.publish(aString, False)
        else e := aEventEntry;
        try
          if e.connection.connected
          then status := 'connected'
          else status := 'NOT connected';
          if Assigned(fOnTilerStatus)
          then info := ',"info":"'+fOnTilerStatus(self)+'"'
          else info := '';
          e.signalString('{"id":"'+aEventEntry.connection.ModelName+'","status":"'+status+'"'+info+'}');
        finally
          if aString<>''
          then e.unPublish();
        end;
      end;
  end;
end;

end.
