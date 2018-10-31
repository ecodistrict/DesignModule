unit TilerWebModule;

{
 link isapi dll in iis7: http://www.delphifeeds.com/go/s/61336
 debug isapi: http://alexandrecmachado.blogspot.nl/2012/04/debug-isapi-extensions-in-windows-7-and.html
 start Delphi IDE with admin rights to be able to start IIS worker process as host app

 set default document to dll

 tiles, example coordinates and lookup on map:
   http://www.maptiler.org/google-maps-coordinates-tile-bounds-projection/

 netherlands: google: zoom=5&x=16&y=10   tms: zoom=5&x=16&y=21   QuadTree: 12020

 http://wiki.openstreetmap.org/wiki/Zoom_levels

 iis install:
   enable websockets, isapi modules..
 iis config:
   add site: DesignTiler binding to specific port (4503)
     add default document TilerWebService.dll
     enable handler mapping: edit feature permissions
     add execute rights..
     set folder security iis_iusr.. (modify for writing to log)
}

{
  FMX TCanvas:
  - gdiplus init only works when NOT a DLL, isapi web module is a dll
  - d2d canvas does not register when theming is not enabled (like default on windows 2012 server)
  - gdiplus is fallback for d2d canvas

  -> fix: changed FMX.Canvas.D2D unit, delphi
}

{
  communication protocol client to tiler and back

  register new layer:                        client -> tiler
      icehTilerEventName (string, event name to register layer control and data on)
      [icehTilerEdgeLength (double)]
      [icehTilerPersistent (bool)]
      [icehTilerLayerDescription (string)]
    icehTilerRequestNewLayer (varint, int32, slice type)

  response on icehTilerEventName             tiler -> client
      icehTilerID (varint, int32)
    icehTilerURL (string)

  icehTilerURL -> client.OnTilerInfo
    RegisterSlice
      signalAddSlice                         client -> tiler
          [palette]
        icehTilerSliceID (double, timestamp)

        TLayer.handleDataEvent               tiler -> client
          create slice
          icehTilerRefresh (double, timestamp)

        client.OnRefresh                     client -> tiler

        request preview
          icehTilerRequestPreviewImage (varint, uint32, PreviewImageWidth)


    signalObjects
}

{
  issues:
    - lat is projected lineair to y in tile bitmap, on large scale this will have a visible
      effect in elements being offset in y direction: can be fixed by using mercator translation but
      that is procesor time consuming. For now leave as is. On urban scale up to dutch latitudes it is ok.
}

interface

uses
  StdIni, CmdLin,
  imb4,
  Delaunay3,
  MyThreads,
  WorldLegends, WorldDataCode,
  WorldTilerConsts,
  WorldOrderedList,
  OmniMREW,

  // bitmaps
  FMX.Platform,
  System.UITypes,
  FMX.Graphics,
  FMX.Types,

// canvas class implementations (different on hardware and VPS machine)
//  FMX.Canvas.GDIP,

// theming problem on server where profile is loaded and D2D canvas is availble only test fails in orginal unit
//  FMX.Canvas.D2D.my,      //  Delphi 10 version
//  FMX.Canvas.D2D.my10_1,  //  Delphi 10.1 version
  FMX.Canvas.D2D.my10_2_u3, //   Delphi 10.2 update 3 version

  // TPolygon
  System.Math.Vectors,
  // drawing
  System.Types, // TPointF

  Logger, LogFile, // after FMX units (have also 'log' defined)

  Web.HTTPApp,

  WinApi.Windows, // before FMX because of bitmaps

  System.SyncObjs, // critical section
  System.Generics.Defaults,
  System.Generics.Collections,
  System.DateUtils,
  System.Math,
  System.SysUtils,
  System.Classes;

const
  RemoteHostSwitch = 'RemoteHost';
  RemotePortSwitch = 'RemotePort';

  ModelNameSwitch = 'ModelName';
    DefaultModelName = 'Tiler';
  ModelIDSwitch = 'ModelID';
    DefaultModelID =  13;
  PrefixSwitch = 'Prefix';
    DefaultPrefix = 'USIdle';

  MaxEdgeLengthSwitch = 'MaxEdgeLength';
    DefaultMaxEdgeLength = 500; // 250;

  ThreadCountSwitch = 'ThreadCount';
    DefaultThreadCount = 8;

  CacheFolderSwitch = 'CacheFolder';
    DefaultCacheFolder = 'TileCache';

  TilerURLSwitch = 'TilerURL';
  TilerEventNameSwitch  = 'TilerEventName';

  LayerIDsSection = 'LayerIDs';

  PersistentLayersSection = 'PersistentLayers';
  PersistentLayersDataFileName = 'PersistentLayers.dat';

  TileSizeX = 256;
  TileSizeY = 256;

  actionStatus = 4;

  // request parameters
  rpLayerID = 'layer';
  rpZoomFactor = 'zoom';
  rpTileX = 'x';
  rpTileY = 'y';
  rpLat = 'lat';
  rpLon = 'lon';
  rpTime = 'time'; // yyyymmddhhmmss
    SliceTimeIDFormat = 'yyyymmdd.hhnn';

  HSC_SUCCESS_OK = 200;
  //HSC_SUCCESS_CREATED = 201;

  HSC_ERROR_BADREQUEST = 400;
  HSC_ERROR_UNAUTHORIZED = 401;
  HSC_ERROR_FORBIDDEN = 403;
  HSC_ERROR_NOT_FOUND = 404;
  HSC_ERROR_CONFLICT = 409;
  HSC_ERROR_NOTIMPLEMENTED = 501;

type
  TTileCacheEntry = class
  constructor Create(const aPath: string);
  private
    fPath: string;
    fCreated: TDateTime;
    fLastAccessed: TDateTime;
    fStream: TStream;
    function getStream: TStream;
    procedure setStream(aValue: TStream);
  public
    property path: string read fPath;
    property created: TDateTime read fCreated;
    property lastAccessed: TDateTime read fLastAccessed;
    property stream: TStream read getStream write setStream;

    procedure FromBitmap(aBitmap: FMX.Graphics.TBitmap);
    function SaveTile(): Boolean;
    procedure ClearStream();
  end;

const
  earthRadius = 6317000; // m

var
  globalModelAndLayersLock: TOmniMREW;
  canvasLock: TObject;

type
  TLatLon = record
    lat: Double;
    lon: Double;
  end;

  TDistanceLatLon = record
    m_per_deg_lat: Double;
    m_per_deg_lon: Double;

    class function Create(aLat1InDegrees, aLat2InDegrees: Double): TDistanceLatLon; overload; static;
    class function Create(aLatMidInRad: Double): TDistanceLatLon; overload; static;
    function distanceInMeters(aDeltaLat, aDeltaLon: Double): Double;
    function distanceInRadians(aDistanceInMetersLat, aDistanceInMetersLon: Double): TLatLon;
  end;

type
  TExtent = TDLExtent;

  TExtentHelper = record helper for TExtent
  class function Create: TExtent; static;
  class function FromGeometry(aGeometry: TWDGeometry): TExtent; static;
  public
    procedure Init; overload;
    procedure Init(x, y: Double); overload;
    procedure Init(x, y, aWidth, aHeight: Double); overload;
  public
    function Width: Double;
    function Height: Double;
    function IsEmpty: Boolean;
    function Expand(x, y: Double): Boolean; overload;
    function Expand(aExtent: TExtent): Boolean; overload;
    function Intersects(const aExtent: TExtent): Boolean;
    function Intersection(const aExtent: TExtent): TExtent;
    function Contains(x, y: Double): Boolean;
    function Center: TPointF;
    function Inflate(aFactor: Double): TExtent; overload;
    function Inflate(aDX, aDY: Double): TExtent; overload;
    function SquareInMeters: TExtent;
  public
    function Encode: TByteBuffer;
    procedure Decode(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer);
  end;

  TDecodedDateTime = record
    year: Word; // year
    doty: Word; // day of the year
    hotd: Word; // hour of the day
    procedure DecodeFrom(aDateTime: TDateTime);
  end;

  TLayer = class; // forward

  TDiffSlice = class; // forward

  TSliceGeometryObject = class
  constructor Create(aGeometry: TWDGeometry; aValue: Double);
  destructor Destroy; override;
  protected
    fGeometry: TWDGeometry;
    fExtent: TExtent;
    fValue: Double;
  public
    property geometry: TWDGeometry read fGeometry;
    property extent: TExtent read fExtent;
    property value: Double read fValue write fValue;
  end;

  TSliceGeometryIHDataPoint = class(TOrderedListTSEntry)
  constructor Create(aTimeStamp: TDateTime; aLat, aLon, aValue, aHeight: Double);
  protected
    fLat: Double;
    fLon: Double;
    fValue: Double;
    fHeight: Double;
  public
    property lat: Double read fLat;
    property lon: Double read fLon;
    property value: Double read fValue write fValue;
    property height: Double read fHeight write fHeight;
  end;

  TSliceGeometryICObject = class(TSliceGeometryObject)
  constructor Create(aGeometry: TWDGeometry; aValue, aTexture: Double);
  protected
    fTexture: Double;
  public
    property texture: Double read fTexture write fTexture;
  end;

  TSliceGeometryICLRObject = class(TSliceGeometryICObject)
  constructor Create(aGeometry: TWDGeometry; aValue, aValue2, aTexture, aTexture2: Double);
  protected
    fValue2: Double;
    fTexture2: Double;
  public
    property value2: Double read fValue2 write fValue2;
    property texture2: Double read fTexture2 write fTexture2;
  end;

  TSliceLocationObject = class
  constructor Create(aLocation: TWDGeometryPoint; aValue, aRadius: Double);
  destructor Destroy; override;
  protected
    fLocation: TWDGeometryPoint;
    fExtent: TExtent;
    fValue: Double;
    fRadius: Double;
  public
    property lcoation: TWDGeometryPoint read fLocation;
    property extent: TExtent read fExtent;
    property value: Double read fValue write fValue;
    property radius: Double read fRadius write fRadius;
  end;

  TGenerateTileStatus = (gtsFailed, gtsOk, gtsRestart);

  TQueueBuffer = record
  class function Create(const aBuffer: TByteBuffer):TQueueBuffer; static;
  procedure Free;
  public
    buffer: Pointer;
    size: Integer;
    function AsByteBuffer: TByteBuffer;
  end;

  TSlice = class(TThread)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime=0);
  destructor Destroy; override;
  private
    fParentSlices: TObjectList<TDiffSlice>; // only ref
    fLayer: TLayer; // only ref
    fPalette: TWDPalette; // own
    fTimeStamp: TDateTime;
    fDataVersion: Integer;
    fMaxExtent: TExtent;
    fTileLock: TOmniMREW;
    fDataLock: TOmniMREW;
  protected
    function TimeFolder: string;
  protected
    // tile generation
    function doGenerateTilePreCalc: Boolean; virtual;
    function GenerateTilePreCalc(aThreadPool: TMyThreadPool): Boolean; virtual;
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; virtual; abstract;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; virtual;
    function GenerateTileCache(const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer; const aFileName, aCacheFolder: string): TGenerateTileStatus;
  public
    function GenerateTile(const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer; const aFileName, aCacheFolder: string; aThreadPool: TMyThreadPool=nil): Integer; virtual;
    function PointValue(const aLat, aLon: Double; aThreadPool: TMyThreadPool=nil): Double; virtual;
  public
    property timeStamp: TDateTime read fTimeStamp;
    function id: string;
    // updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; virtual;
    procedure HandleUpdateOfParents;
    procedure AddParent(aParent: TDiffSlice);
    procedure RemoveParent(aParent: TDiffSlice);
    function UpdatePalette(aPalette: TWDPalette): Boolean; virtual;
    function ClearSlice(): Boolean; virtual;
  protected
    // queue
    fQueue: TList<TQueueBuffer>;
    fQueueFilled: TEvent;
    procedure Execute; override;
  public
    procedure AddToQueue(aBuffer: TByteBuffer);
  public
    procedure Load(aStream: TStream); virtual; // todo: abstract;
    procedure Save(aStream: TStream; aSavedSlices: TList<TSlice>); virtual; // todo: abstract;
  end;

  TSliceReceptor = class(TSlice)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
  destructor Destroy; override;
  private
    fNet: TDLNet; // own
  protected
    // tile generation
    function doGenerateTilePreCalc: Boolean; override;
    function GenerateTilePreCalc(aThreadPool: TMyThreadPool): Boolean; override;
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TSliceOutLineFill = class(TSlice)
  end;

  TSliceGeometry = class(TSliceOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
  destructor Destroy; override;
  protected
    fGeometries: TObjectDictionary<TWDID, TSliceGeometryObject>;
    fLineWidth: Double;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TSliceGeometryI = class(TSliceGeometry)
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  end;

  TSliceGeometryIH = class(TSlice)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aLineThickness: Double);
  destructor Destroy; override;
  protected
    fDataPoints: TObjectDictionary<TWDID, TOrderedListTS<TSliceGeometryIHDataPoint>>;
    fLineThicknesss: Double;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TSliceGeometryIC = class(TSliceOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
  destructor Destroy; override;
  protected
    fGeometries: TObjectDictionary<TWDID, TSliceGeometryICObject>;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TSliceGeometryICLR = class(TSliceOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
  destructor Destroy; override;
  protected
    fGeometries: TObjectDictionary<TWDID, TSliceGeometryICLRObject>;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TPOI = record
    id: Integer;
    latLon: TLatLon;
    imageID: Integer;
  end;

  TSlicePOI = class(TSlice)
  constructor Create(aLayer: TLayer; aTimeStamp: TDateTime; const aImages: array of FMX.Graphics.TBitmap); // todo: add images as parameter in creation
  destructor Destroy; override;
  private
    fImages: TObjectList<FMX.Graphics.TBitmap>; // own
    fPOIs: TDictionary<Integer, TPOI>; // own
    // todo: zoom handler
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  public
    property Images: TObjectList<FMX.Graphics.TBitmap> read fImages;
    // no PointValue
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TSlicePNG = class(TSlice)
  constructor Create(aLayer: TLayer; aTimeStamp: TDateTime; aExtent: TExtent; aImage: FMX.Graphics.TBitmap; aDiscreteColorsOnStretch: Boolean);
  destructor Destroy; override;
  private
    fExtent: TExtent;
    fImage: FMX.Graphics.TBitmap; // own
    fDiscreteColorsOnStretch: Boolean;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  public
    // no PointValue
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TSliceLocation  = class(TSliceOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
  destructor Destroy; override;
  protected
    fLocations: TObjectDictionary<TWDID, TSliceLocationObject>;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    // for updating data
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
    function ClearSlice(): Boolean; override;
  end;

  TDiffSlice = class(TSlice)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlice);
  destructor Destroy; override;
  protected
    fCurrentSlice: TSlice; // ref
    fRefSlice: TSlice; // ref
  public
    function GenerateTile(const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer; const aFileName, aCacheFolder: string; aThreadPool: TMyThreadPool=nil): Integer; override;
    function getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus; override;
  public
    procedure RemoveChild(aChild: TSlice);
    // for updating data
    procedure HandleDiffUpdate; virtual;
  end;

  TSliceDiffReceptor = class(TDiffSlice)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceReceptor);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TSliceDiffOutLineFill = class(TDiffSlice)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlice);
  end;

  TSliceDiffGeometry = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometry);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TSliceDiffGeometryI = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryI);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TSliceDiffGeometryIC = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryIC);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TSliceDiffGeometryICLR = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryICLR);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;
  
  TSliceDiffGeometryICLR2 = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryICLR);
  destructor Destroy; override;
  protected
    fGeometries: TObjectDictionary<TWDID, TSliceGeometryICLRObject>;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function ComputeCoordinate(aWidth, aActiveValue, aRefValue, aCapacityFactor, aXY_Diff, aPerpDist: Double; aIsCommonPoly: Boolean): Double;
    function CreatePolygon(aPolyPoints: TArray<Double>): TPolygon;
    function HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean; override;
  end;

  TSliceDiffGeometryICLR3 = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryICLR);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
    function ComputeICRatioClass(aICValue: Double): Integer;
  end;

  TSliceDiffPOI = class(TDiffSlice)
  constructor Create(aLayer: TLayer; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlicePOI;
    aColorRemovedPOI, aColorSamePOI, aColorNewPOI: TAlphaRGBPixel);
  private
    fColorRemovedPOI: TAlphaRGBPixel;
    fColorSamePOI: TAlphaRGBPixel;
    fColorNewPOI: TAlphaRGBPixel;
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TSliceDiffPNG = class(TDiffSlice)
  constructor Create(aLayer: TLayer; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlicePNG);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TSliceDiffLocation = class(TSliceDiffOutLineFill)
  constructor Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceLocation);
  protected
    // tile generation
    function GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus; override;
  end;

  TModel = class; // forward

  TLayer = class
  constructor Create(aModel: TModel; aLayerID: Integer; aMaxEdgeLengthInMeters: TDLCoordinate; aSliceType: Integer; const aDescription: string; aPersistent: Boolean; const aEventName: string);
  destructor Destroy; override;
  private
    fModel: TModel; // ref
    fLayerID: Integer;
    fMaxEdgeLengthInMeters: TDLCoordinate;
    fSliceType: Integer;
    fDescription: string;
    fPersistent: Boolean;
    fWORMLock: TOmniMREW;
    fSlices: TObjectList<TSlice>; // own, sorted (on date/time) list of slices
    fDataEvent: TEventEntry; // ref
    procedure addSlice(aSlice: TSlice);
    function findSlice(aTimeStamp: TDateTime): TSLice; overload;
    function findSlice(aLayerID: Integer; aTimeStamp: TDateTime): TSLice; overload;
    procedure handleDataEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    procedure signalRefresh(aTimeStamp: TDateTime; aImmediate: Boolean);
  public
    property Model: TModel read fModel;
    property LayerID: Integer read fLayerID;
    property MaxEdgeLengthInMeters: TDLCoordinate read fMaxEdgeLengthInMeters;
    property SliceType: Integer read fSliceType;
    property Slices: TObjectList<TSlice> read fSlices;
    property DataEvent: TEventEntry read fDataEvent;
    property WORMLock: TOmniMREW read fWORMLock;
    function GenerateTile(aTimeStamp: TDateTime; const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer;{aBitmap: FMX.Graphics.TBitmap;} const aFileName, aCacheFolder: string; aThreadPool: TMyThreadPool=nil): Integer;
    function PointValue(aTimeStamp: TDateTime; const aLat, aLon: Double; aThreadPool: TMyThreadPool=nil): Double;
    function URL: string;
    function dateTimeRange: string;
    procedure signalTilerInfo;
    procedure storePersistencyInfo;
    procedure removePersistencyInfo;
    function extent: TExtent;
    procedure Clear(aMaxEdgeLengthInMeters: Double; const aDescription: string; aPersistent: Boolean); virtual;
  public
    procedure Load(aStream: TStream);
    procedure Save(aStream: TStream; aSavedSlices: TList<TSlice>);
  end;

  TModel = class
  constructor Create(aMaxEdgeLengthInMeters: TDLCoordinate; aThreadCount: Integer);
  destructor Destroy; override;
  private
    fMaxEdgeLengthInMeters: TDLCoordinate;
    fConnection: TConnection;
    fLayers: TObjectDictionary<Integer, TLayer>;
    fThreadPool: TMyThreadPool;
    fCacheFolder: string;
    fDefaultCanvasTriggerLock: TCriticalSection;
    fTilerEvent: TEventEntry;
    fWS2IMBEvent: TEventEntry;
    fURL: string;
    fConnectedServices: TStringList;
  private
    procedure HandleDisconnect(aConnection: TConnection);
    procedure HandleException(aConnection: TConnection; aException: Exception);
    procedure HandleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
    procedure HandleTilerStatus(aEventEntry: TEventEntry; const aString: string);
    procedure HandleTilerStatusRequest(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
    procedure setURL(const aValue: string);
    procedure ClearCache;
  private
    procedure LoadPersistentLayers;
    procedure SavePersistentLayers;
  public
    function GenerateTile(aLayerID: Integer; aTimeStamp: TDateTime; const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer;{aBitmap: FMX.Graphics.TBitmap;} const aFileName: string): Integer;
    function PointValue(aLayerID: Integer; aTimeStamp: TDateTime; aLat, aLon: Double): Double;

    property defaultCanvasTriggerLock: TCriticalSection read fDefaultCanvasTriggerLock;
    property Connection: TConnection read fConnection;
    property URL: string read fURL write setURL;
    property TilerEvent: TEventEntry read fTilerEvent;

    procedure RequestCSStatus;
  end;

  TTilerWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);

    procedure WebModuleException(Sender: TObject; E: Exception; var Handled: Boolean);
    // url actions
    procedure WebModuleDefaultHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleTilerRequestTileAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleTilerRequestPointValueAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure TilerWebModuleRequestStatusAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  end;

var
  WebModuleClass: TComponentClass = TTilerWebModule;
  model: TModel = nil;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

function LatToMercator(aLat: Double): Double;
begin
  Result := (ln(tan((90 + aLat) * Pi / 360)) / (Pi / 180)) * (20037508.34 / 180);
end;

// http://stackoverflow.com/questions/7661/java-code-for-wgs84-to-google-map-position-and-back
function LatToY(aLat, aLatMin, aLatMax: Double; aHeight: Integer): Double;
var
  merc, mercMin, mercMax: Double;
begin
  merc := LatToMercator(aLat);
  mercMin := LatToMercator(aLatMin);
  mercMax := LatToMercator(aLatMax);
  // we are not calculating in pixels but in position (float) within the bitmap
  Result := ((merc-mercMin)/(mercMax-mercMin))*aHeight;
end;

function GeometryToPoint(const aExtent: TExtent; const aPixelWidth, aPixelHeight: Double; aGeometry: TWDGeometryPoint): TPointF;
begin
  // recalc coordinates relative to extent
  Result.X := (aGeometry.x-aExtent.XMin)/aPixelWidth;
  Result.Y := (aExtent.YMax-aGeometry.y)/aPixelHeight;
end;

function GeometryToPolygon(const aExtent: TExtent; const aPixelWidth, aPixelHeight: Double; aGeometry: TWDGeometry): TPolygon;
var
  i: Integer;
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  i := 0;
  for part in aGeometry.parts
  do i := i+part.points.count;
  setLength(Result, i);
  i := 0;
  for part in aGeometry.parts do
  begin
    for point in part.points do
    begin
      // recalc coordinates relative to extent
      Result[i].X := (point.X-aExtent.XMin)/aPixelWidth;
      Result[i].Y := (aExtent.YMax-point.Y)/aPixelHeight;
      i := i+1;
    end;
  end;
end;

// asume geometry is multi line
function GeometryToPath(const aExtent: TExtent; const aPixelWidth, aPixelHeight: Double; aGeometry: TWDGeometry): TPathData;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  first: Boolean;
  x: Double;
  y: Double;
begin
  Result := TPathData.Create;
  for part in aGeometry.parts do
  begin
    first := True;
    for point in part.points do
    begin
      x := (point.X-aExtent.XMin)/aPixelWidth;
      y := (aExtent.YMax-point.Y)/aPixelHeight;
      if first then
      begin
        Result.MoveTo(TPointF.Create(x, y));
        first := False;
      end
      else Result.LineTo(TPointF.Create(x, y));
    end;
  end;
end;

// calculate tile extent in lat/lon from zoom level and tile index x,y
function ZoomXYToExtent(aZoomFactor, aTileX, aTileY: Integer): TExtent;
var
  numtiles: Integer;
  longitudeSpan: Double;
  lat_rad: Double;
begin
  numtiles := 1 shl aZoomFactor;
  longitudeSpan := 360.0 / numtiles;
  Result.XMin := -180.0 + aTileX * longitudeSpan;
  lat_rad := arctan(sinh(PI * (1 - 2 * aTileY/numtiles)));
  Result.YMax := lat_rad * 180/PI;
  Result.XMax := (aTileX + 1)/numtiles * 360 - 180;
  lat_rad := arctan(sinh(PI * (1 - 2 * (aTileY + 1)/numtiles)));
  Result.YMin := lat_rad * 180/PI;
end;

procedure LatLonZoomToXY(aLat, aLon: Double; aZoomFactor: Integer; out aX, aY: Integer);
var
  n: Double;
  lat_rad: Double;
begin
  n := Power(2, aZoomFactor);
  lat_rad := aLat*PI/180.0;
	aX := Trunc(n * ((aLon + 180.0) / 360.0));
	aY := Trunc(n * (1 - (ln(tan(lat_rad) + sec(lat_rad)) / PI)) / 2.0);
end;

procedure ExtentToXYZoom(const aExtent: TExtent; out aX, aY, aZoomFactor: Integer);
var
  x2: Integer;
  y2: Integer;
begin
  aZoomFactor := 16; // very small on urban scale
  repeat
    aZoomFactor := aZoomFactor-1;
    LatLonZoomToXY(aExtent.YMin, aExtent.XMin, aZoomFactor, aX, aY);
    LatLonZoomToXY(aExtent.YMax, aExtent.XMax, aZoomFactor, x2, y2);
  until (aZoomFactor=0) or ((aX=x2) and (aY=y2));
end;

function getFQDN: string;
var
  nSize: DWORD;
begin
  nSize := 0;
  GetComputerNameEx(ComputerNameDnsFullyQualified, nil, nSize);
  setLength(Result, nSize);
  if GetComputerNameEx(ComputerNameDnsFullyQualified, PChar(Result), nSize) then
  begin
    // strip terminating 0
    while Result.EndsWith(#0)
    do Result := Result.Substring(0, Length(Result)-1);
  end
  else Result := '';
end;

procedure deleteDirectory(const aDirectoryName: string);
var
  path: string;
  F: TSearchRec;
begin
  path := IncludeTrailingPathDelimiter(aDirectoryName);
  if FindFirst(path+'*.*', faAnyFile, F) = 0 then
  begin
    try
      repeat
        if (F.Attr and faDirectory) <> 0 then
        begin
          if (F.Name <> '.') and (F.Name <> '..')
          then deleteDirectory(path+F.Name);
        end
        else DeleteFile(path+F.Name);
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;
    RemoveDir(aDirectoryName);
  end;
end;

{ TilesCache }

var
  tilesCache: TObjectDictionary<string, TTileCacheEntry>;
  monitorTilesCacheThread: TThread;
  monitorTilesCacheWaitEvent: TEvent;

const
  dtOneHour = 1.0/24.0;
  dtOneMinute = dtOneHour/60;

  dtStreamClear = dtOneMinute*5;
  dtTileCacheEntryOutOfDate = dtOneHour;

  monitorTilesCacheCycle = 5*60*1000; // ms

procedure MonitorTilesCache;
var
  _now: TDateTime;
  removeEntries: TList<string>;
  tcep: TPair<string, TTileCacheEntry>;
  re: string;
  unLock: Boolean;
begin
  while not TThread.CheckTerminated do
  begin
    if monitorTilesCacheWaitEvent.WaitFor(monitorTilesCacheCycle)=wrSignaled then
    begin
      if not TThread.CheckTerminated then
      begin
        _now := Now();
        removeEntries := nil; // only create list when needed
        try
          // lock list: we maybe want to remove items
          TMonitor.Enter(tilesCache);
          try
            // check all entries
            for tcep in tilesCache do
            begin
              unLock := True; // default to: unlock entry so it can be used later
              TMonitor.Enter(tcep.Value);
              try
                // check out-of-date
                if tcep.Value.lastAccessed+dtTileCacheEntryOutOfDate<_now then
                begin
                  if not Assigned(removeEntries)
                  then removeEntries := TList<string>.Create;
                  removeEntries.Add(tcep.Key);
                  unLock := False; // keep entry locked so it can never be used again
                end
                else
                begin
                  // check stream to be cleared (if path is set otherwise no way to re-create)
                  if (length(tcep.Value.path)>0) and (tcep.Value.lastAccessed+dtStreamClear<_now)
                  then tcep.Value.ClearStream();
                end;
              finally
                if unLock
                then TMonitor.Exit(tcep.Value);
              end;
            end;
            // remove tile cache entries that are out-of-date
            if Assigned(removeEntries) then
            begin
              for re in removeEntries
              do tilesCache.Remove(re);
            end;
          finally
            TMonitor.Exit(tilesCache);
          end;
        finally
          removeEntries.Free;
        end;
      end;
    end;
  end;
end;

{ TTileCacheEntry }

procedure TTileCacheEntry.ClearStream;
begin
  FreeAndNil(fStream);
end;

constructor TTileCacheEntry.Create(const aPath: string);
begin
  inherited Create;
  fPath := aPath;
  fCreated := Now;
  fLastAccessed := fCreated;
  fStream := nil;
end;

procedure TTileCacheEntry.FromBitmap(aBitmap: FMX.Graphics.TBitmap);
var
  s: TStream;
begin
  s := TMemoryStream.Create;
  try
    aBitmap.SaveToStream(s);
  finally
    stream := s;
  end;
end;

function TTileCacheEntry.getStream: TStream;
var
  fileStream: TFileStream;
begin
  // if not stream defined load into file stream and copy to memory stream to be stored in cache
  if not Assigned(fStream) then
  begin
    if path<>'' then
    begin
      fStream := TMemoryStream.Create;
      fileStream := TFileStream.Create(path, fmOpenRead+fmShareDenyWrite);
      try
        fStream.CopyFrom(fileStream, 0);
      finally
        fileStream.Free;
      end;
    end;
  end;
  result := fStream;
  fLastAccessed := Now;
end;

function TTileCacheEntry.SaveTile: Boolean;
var
  fileStream: TFileStream;
begin
  if Assigned(stream) then
  begin
    // make sure folder exists first
    ForceDirectories(ExtractFileDir(path));
    // save memory stream with btimap to file
    fileStream := TFileStream.Create(path, fmCreate+fmShareExclusive);
    try
      fileStream.CopyFrom(stream, 0);
    finally
      fileStream.Free;
    end;
    Result := True;
  end
  else Result := False;
end;

procedure TTileCacheEntry.setStream(aValue: TStream);
begin
  if fStream<>aValue then
  begin
    fStream.Free;
    fStream := aValue;
    fLastAccessed := Now;
  end;
end;

{ TDistanceLatLon }

class function TDistanceLatLon.Create(aLat1InDegrees, aLat2InDegrees: Double): TDistanceLatLon;
begin
  Result := Create(((aLat1InDegrees + aLat2InDegrees) / 2) * PI / 180); // average converted to radians
end;

class function TDistanceLatLon.Create(aLatMidInRad: Double): TDistanceLatLon;
begin
  Result.m_per_deg_lat := 111132.954 - 559.822 * Cos(2 * aLatMidInRad) + 1.175 * Cos(4 * aLatMidInRad);
  Result.m_per_deg_lon := 111132.954 * Cos(aLatMidInRad);
end;

function TDistanceLatLon.distanceInMeters(aDeltaLat, aDeltaLon: Double): Double;
begin
  Result := sqrt(sqr(m_per_deg_lat * aDeltaLat) + sqr(m_per_deg_lon * aDeltaLon));
end;

function TDistanceLatLon.distanceInRadians(aDistanceInMetersLat, aDistanceInMetersLon: Double): TLatLon;
begin
  Result.lat := aDistanceInMetersLat/m_per_deg_lat;
  Result.lon := aDistanceInMetersLon/m_per_deg_lon;
end;

function distanceInMeters(aLat1, aLon1, aLat2, aLon2: Double): Double;
begin
  Result := TDistanceLatLon.Create(aLat1, aLat2).distanceInMeters(aLat1-aLat2, aLon1-aLon2);
end;

function distanceInRadians(aReferencePointLat, aDistanceInMetersLat, aDistanceInMetersLon: Double): TLatLon;
begin
  Result := TDistanceLatLon.Create(aReferencePointLat).distanceInRadians(aDistanceInMetersLat, aDistanceInMetersLon);
end;


function AllCiphers(const s: string): Boolean;
var
  c: Char;
begin
  for c in s do
  begin
    if not (('0'<=c) and (c<='9'))
    then Exit(False);
  end;
  Result := True;
end;

{ TSessionExtent }

function TExtentHelper.Center: TPointF;
begin
  Result.X := (xMin+XMax)/2;
  Result.Y := (yMin+yMax)/2;
end;

function TExtentHelper.Contains(x, y: Double): Boolean;
begin
  if not IsEmpty
  then Result :=
        (x >= Self.xMin) and
        (x <= Self.xMax) and
        (y >= Self.yMin) and
        (y <= Self.yMax)
  else Result := False;
end;

class function TExtentHelper.Create: TExtent;
begin
  Result.Init;
end;

procedure TExtentHelper.Decode(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer);
begin
  XMin := aBuffer.bb_read_double(aCursor);
  YMin := aBuffer.bb_read_double(aCursor);
  XMax := aBuffer.bb_read_double(aCursor);
  YMax := aBuffer.bb_read_double(aCursor);
  if aCursor>aLimit
  then Exception.Create('TExtentHelper.Decode read over limit ('+aCursor.toString+', '+aLimit.toString);
  aCursor := aLimit;
end;

procedure TExtentHelper.Init;
begin
  xMin := NaN;
  yMin := NaN;
  xMax := NaN;
  yMax := NaN;
end;

function TExtentHelper.Encode: TByteBuffer;
begin
  Result :=
    TByteBuffer.bb_bytes(xMin, sizeOf(xMin))+
    TByteBuffer.bb_bytes(xMin, sizeOf(yMin))+
    TByteBuffer.bb_bytes(xMin, sizeOf(xMax))+
    TByteBuffer.bb_bytes(xMin, sizeOf(yMax));
end;

function TExtentHelper.Expand(aExtent: TExtent): Boolean;
begin
  if not aExtent.IsEmpty then
  begin
    Result := Expand(aExtent.XMin, aExtent.YMin);
    if Expand(aExtent.XMax, aExtent.YMax)
    then Result := True;
  end
  else Result := False;
end;

procedure TExtentHelper.Init(x, y: Double);
begin
  xMin := x;
  xMax := x;
  yMin := y;
  yMax := y;
end;

function TExtentHelper.Expand(x, y: Double): Boolean;
begin
  if isEmpty then
  begin
    Init(x, y);
    Result := True;
  end
  else
  begin
    Result := False;
    if not x.IsNan then
    begin
      if xMin>x then
      begin
        xMin := x;
        Result := True;
      end;
      if xMax<x then
      begin
        xMax := x;
        Result := True;
      end;
    end;
    if not y.IsNan then
    begin
      if yMin>y then
      begin
        yMin := y;
        Result := True;
      end;
      if yMax<y then
      begin
        yMax := y;
        Result := True;
      end;
    end;
  end;
end;

class function TExtentHelper.FromGeometry(aGeometry: TWDGeometry): TExtent;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  Result.Init;
  for part in aGeometry.parts do
  begin
    for point in part.points do
    begin
      if Result.IsEmpty
      then Result.Init(point.X, point.Y)
      else Result.Expand(point.X, point.Y);
    end;
  end;
end;

function TExtentHelper.Height: Double;
begin
  Result := YMax-YMin;
end;

function TExtentHelper.Inflate(aFactor: Double): TExtent;
var
  c: TPointF;
begin
  c := Center;
  Result.XMin := c.X+(XMin-c.X)*aFactor;
  Result.XMax := c.X+(XMax-c.X)*aFactor;
  Result.YMin := c.Y+(YMin-c.Y)*aFactor;
  Result.YMax := c.Y+(YMax-c.Y)*aFactor;
end;

function TExtentHelper.Inflate(aDX, aDY: Double): TExtent;
begin
  Result.XMin := XMin-aDX;
  Result.XMax := XMax+aDX;
  Result.YMin := YMin-aDY;
  Result.YMax := YMax+aDY;
end;

procedure TExtentHelper.Init(x, y, aWidth, aHeight: Double);
begin
  xMin := x;
  xMax := xMin+aWidth;
  yMin := y;
  yMax := y+aHeight;
end;

function TExtentHelper.Intersection(const aExtent: TExtent): TExtent;
begin
  if Intersects(aExtent) then
  begin
    Result.XMin := Max(Self.XMin, aExtent.XMin);
    Result.YMin := Max(Self.YMin, aExtent.YMin);
    Result.XMax := Min(Self.XMax, aExtent.XMax);
    Result.YMax := Min(Self.YMax, aExtent.YMax);
  end
  else Result.Init;
end;

function TExtentHelper.Intersects(const aExtent: TExtent): Boolean;
begin
  if not (IsEmpty or aExtent.IsEmpty) then
    Result :=
         (Self.XMin <= aExtent.XMax) and
         (Self.XMax >= aExtent.XMin) and
         (Self.YMin <= aExtent.YMax) and
         (Self.YMax >= aExtent.YMin)
  else
    Result := False;
end;

function TExtentHelper.IsEmpty: Boolean;
begin
  Result := IsNaN(xMin);// SameValue(xMax, xMin) or SameValue(yMax, yMin);
end;

function TExtentHelper.SquareInMeters: TExtent;
var
  d: TDistanceLatLon;
  w, h: Double;
begin
  // make square in meters (relative from center of extent) containing the original extent
  d := TDistanceLatLon.Create(YMin, YMax);
  w := d.m_per_deg_lon*Width;
  h := d.m_per_deg_lat*Height;
  if Abs(w)>=Abs(h) then
  begin
    Result.XMin := XMin;
    Result.XMax := XMax;
    h := w/d.m_per_deg_lat;
    Result.YMin := ((YMin+YMax)/2)-(h/2);
    Result.YMax := ((YMin+YMax)/2)+(h/2);
  end
  else
  begin
    Result.YMin := YMin;
    Result.YMax := YMax;
    w := h/d.m_per_deg_lat;
    Result.XMin := ((XMin+XMax)/2)-(w/2);
    Result.XMax := ((XMin+XMax)/2)+(w/2);
  end;
end;

function TExtentHelper.Width: Double;
begin
  Result := XMax-XMin;
end;

{ TDecodedDateTime }

procedure TDecodedDateTime.DecodeFrom(aDateTime: TDateTime);
begin
  year := YearOf(aDateTime);
  doty := DayOfTheYear(aDateTime);
  hotd := HourOfTheDay(aDateTime);
end;

{ TQueueBuffer }

function TQueueBuffer.AsByteBuffer: TByteBuffer;
begin
  SetLength(Result, size);
  if size>0
  then Move(buffer^, Result[1], size);
end;

class function TQueueBuffer.Create(const aBuffer: TByteBuffer): TQueueBuffer;
begin
  Result.size := Length(aBuffer);
  GetMem(Result.buffer, Result.size);
  if Result.size>0
  then Move(aBuffer[1], Result.buffer^, Result.size);
end;

procedure TQueueBuffer.Free;
begin
  if Assigned(buffer) then
  begin
    FreeMem(buffer, size);
    buffer := nil;
  end;
end;

{ TSlice }

procedure TSlice.AddParent(aParent: TDiffSlice);
begin
  TMonitor.Enter(fParentSlices);
  try
    fParentSlices.Add(aParent);
  finally
    TMonitor.Exit(fParentSlices);
  end;
end;

procedure TSlice.AddToQueue(aBuffer: TByteBuffer);
begin
  TMonitor.Enter(fQueueFilled);
  try
    fQueue.Add(TQueueBuffer.Create(aBuffer));
  finally
    TMonitor.Exit(fQueueFilled);
  end;
  fQueueFilled.SetEvent;
end;

function TSlice.ClearSlice: Boolean;
begin
  TMonitor.Enter(fQueueFilled);
  try
    if fQueue.Count>0 then
    begin
      fQueue.Clear;
      Result := True;
    end
    else Result := False;
  finally
    TMonitor.Exit(fQueueFilled);
  end;
end;

constructor TSlice.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  fQueue := TList<TQueueBuffer>.Create;
  fQueueFilled := TEvent.Create(nil, False, False, '');
  inherited Create(True);
  self.NameThreadForDebugging('slice queue handler');
  fParentSlices := TObjectList<TDiffSlice>.Create(False);
  fLayer := aLayer;
  fPalette := aPalette;
  fTimeStamp := aTimeStamp;
  fDataVersion := 0;
  fMaxExtent := TExtent.Create;
  fTileLock.Create;
  fDataLock.Create;
end;

destructor TSlice.Destroy;
var
  ds: TDiffSlice;
begin
  fLayer := nil;
  TMonitor.Enter(fParentSlices);
  try
    for ds in fParentSlices
    do ds.RemoveChild(Self);
  finally
    TMonitor.Exit(fParentSlices);
  end;
  FreeAndNil(fParentSlices);
  FreeAndNil(fPalette);
  // queues
  FreeAndNil(fQueueFilled);
  FreeAndNil(fQueue);
  inherited;
end;

function TSlice.doGenerateTilePreCalc: Boolean;
begin
  Result := False; // default no pre calc needed
end;

procedure TSlice.Execute;
var
  locQueue: TList<TQueueBuffer>;
  tmpQueue: TList<TQueueBuffer>;
  bb: TByteBuffer;
  cursor: Integer;
  update: Boolean;
  qb: TQueueBuffer;
begin
  locQueue := TList<TQueueBuffer>.Create;
  try
    while fQueueFilled.WaitFor(INFINITE)=wrSignaled do
    begin
      // swap queues
      TMonitor.Enter(fQueueFilled);
      try
        tmpQueue := fQueue;
        fQueue := locQueue;
        locQueue := tmpQueue;
      finally
        TMonitor.Exit(fQueueFilled);
      end;
      if locQueue.Count>0 then
      begin
        // process swapped queue
        update := False;
        fDataLock.BeginWrite;
        try
          try
            for qb in locQueue do
            begin
              bb := qb.AsByteBuffer;
              qb.Free;
              cursor := 0;
              try
                if HandleSliceUpdate(bb, cursor, length(bb))
                then update := true;
              except
                on E: Exception
                do Log.WriteLn('Exception processing slice '+fLayer.LayerID.ToString+' update @ '+cursor.toString+': '+E.Message, llError);
              end;
            end;
          except
            on E: Exception
            do Log.WriteLn('Exception queue problems processing slice '+fLayer.LayerID.ToString+' update: '+E.Message, llError);
          end;
          if update then
          begin
            // todo: if specified do recalc of data now (triangulate for receptors)?
            fDataVersion := fDataVersion+1; // trigger new set of tiles in cache
            fLayer.SignalRefresh(timeStamp, False);
            HandleUpdateOfParents;
          end;
        finally
          fDataLock.EndWrite;
        end;
        // clear processed queue
        locQueue.Clear;
      end;
    end;
  finally
    locQueue.Free;
  end;
end;

function TSlice.GenerateTile(const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer; const aFileName, aCacheFolder: string; aThreadPool: TMyThreadPool): Integer;
var
  status: TGenerateTileStatus;
begin
  repeat
    // pre calc phase, potentially write lock of tile -> only when not avoidable
    if doGenerateTilePreCalc then
    begin
      fTileLock.BeginWrite;
      try
        GenerateTilePreCalc(aThreadPool);
      finally
        fTileLock.EndWrite;
      end;
    end;
    // tile generation phase, max read lock of tile
    fTileLock.BeginRead;
    try
      status := GenerateTileCache(aExtent, aStream, aWidthPixels, aHeightPixels, aFileName, aCacheFolder);
    finally
      fTileLock.EndRead;
    end;
  until status<>gtsRestart;
  if status=gtsOk
  then Result := HSC_SUCCESS_OK
  else Result := HSC_ERROR_NOT_FOUND;
end;

function TSlice.GenerateTileCache(const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer; const aFileName, aCacheFolder: string): TGenerateTileStatus;
var
  tileFileName: string;
  bitmap: FMX.Graphics.TBitmap;
  newTCE: Boolean;
  tce: TTileCacheEntry;
begin
  Result  := gtsRestart; // sentinel
  tileFileName := aCacheFolder+TimeFolder+aFileName; // included layer id, data version and time of slice
  // get or create tile cache entry
  // lock list and then lock found or created item; unlock list first so items can be accessed in parallel
  // refactor to mrow lock to avoid readers waiting on tilescache lock because of wait on lock of tce (not releasing tilces lock)
  TMonitor.Enter(tilesCache);
  try
    newTCE := not tilesCache.TryGetValue(tileFileName, tce);
    if newTCE then
    begin
      // create new entry
      if aCacheFolder<>''
      then tce := TTileCacheEntry.Create(tileFileName)
      else tce := TTileCacheEntry.Create('');
      // add to list
      tilesCache.Add(tileFileName, tce);
    end;
    // lock now to make other users wait
    // we have to lock tce within lock on tilescache because of potential delete of tce in cleanup
    TMonitor.Enter(tce);
  finally
    TMonitor.Exit(tilesCache);
  end;
  // no TMonitor.Enter because is pre-locked above
  try
    if newTCE then
    begin
      // build new tile
      try
        // create tile bitmap
        bitmap := FMX.Graphics.TBitmap.Create(aWidthPixels, aHeightPixels);
        try
          // build tile bitmap contents
          try
            Result := GenerateTileCalc(aExtent, bitmap, (aExtent.XMax-aExtent.XMin)/bitmap.Width, (aExtent.YMax-aExtent.YMin)/bitmap.Height);
          except
            on E: Exception
            do Log.WriteLn('Exception generating tile bitmap '+TimeFolder+aFileName+': '+E.Message, llError);
          end;
          try
            if Result=gtsOk then
            begin
              try
                tce.FromBitmap(bitmap);
              except
                on E: Exception do
                begin
                  Log.WriteLn('Exception saving tile bitmap to cache: '+E.Message, llError);
                  Result := gtsRestart;
                end;
              end;
              // check if we have to save the bitmap
              if aCacheFolder<>'' then
              begin
                try
                  if not tce.SaveTile
                  then Log.WriteLn('No stream to save tile from '+TimeFolder+aFileName, llError);
                except
                  on E: Exception
                  do Log.WriteLn('Could not save tile bitmap '+TimeFolder+aFileName+': '+E.Message, llWarning);
                end;
              end;
              // return copy of tile stream if still Ok
              if Result=gtsOk
              then aStream.CopyFrom(tce.stream, 0);
            end
            else
            begin
              if Result=gtsFailed
              then Log.WriteLn('Could not calculate tile '+TimeFolder+aFileName, llError)
              else Log.WriteLn('Retry on calculating tile '+TimeFolder+aFileName, llWarning);
            end;
          except
            on E: Exception
            do Log.WriteLn('Exception in TSlice.GenerateTileCache for '+TimeFolder+aFileName+': '+E.Message, llError);
          end;
        finally
          bitmap.Free;
        end;
      except
        on E: Exception
        do Log.WriteLn('Exception creating tile bitmap '+TimeFolder+aFileName+': '+E.Message, llError);
      end;
    end
    else
    begin
      // use existing tile from stream or file
      aStream.CopyFrom(tce.stream, 0);
      Result := gtsOk;
    end;
  finally
    TMonitor.Exit(tce);
  end;
end;

function TSlice.GenerateTilePreCalc(aThreadPool: TMyThreadPool): Boolean;
begin
  Result := True; // asume OK
end;

function TSlice.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
begin
  // defaults
  aValue := NaN;
  Result := gtsFailed;
end;

function TSlice.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
begin
  // default no action but we must skip data
  aCursor := aLimit;
  Result := True; // trigger refresh
end;

procedure TSlice.HandleUpdateOfParents;
var
  parent: TDiffSlice;
begin
  TMonitor.Enter(fParentSlices);
  try
    for parent in fParentSlices
    do parent.HandleDiffUpdate;
  finally
    TMonitor.Exit(fParentSlices);
  end;
end;

function TSlice.id: string;
begin
  if fTimeStamp<>0
  then Result := fDataVersion.ToString+'-'+FormatDateTime(SliceTimeIDFormat, fTimeStamp)
  else Result := fDataVersion.ToString;
end;

procedure TSlice.Load(aStream: TStream);
begin
  // todo: make abstract, remove body
end;

function TSlice.PointValue(const aLat, aLon: Double; aThreadPool: TMyThreadPool): Double;
var
  status: TGenerateTileStatus;
begin
  Result := NaN; // sentinel
  repeat
    // pre calc phase, potentially write lock of tile -> only when not avoidable
    if doGenerateTilePreCalc then
    begin
      fTileLock.BeginWrite;
      try
        GenerateTilePreCalc(aThreadPool);
      finally
        fTileLock.EndWrite;
      end;
    end;
    // value phase
    status := getDataValueAtPoint(aLat, aLon, Result);
    if status=gtsRestart
    then Log.WriteLn('retry on point value '+TimeFolder+': lat='+aLat.ToString+', lon='+aLon.ToString, llWarning)
    else if status=gtsFailed
    then Log.WriteLn('could not calculate point value '+TimeFolder+': lat='+aLat.ToString+', lon='+aLon.ToString, llError);
  until status<>gtsRestart;
end;

procedure TSlice.RemoveParent(aParent: TDiffSlice);
begin
  TMonitor.Enter(fParentSlices);
  try
    fParentSlices.Remove(aParent);
  finally
    TMonitor.Exit(fParentSlices);
  end;
end;

procedure TSlice.Save(aStream: TStream; aSavedSlices: TList<TSlice>);
begin
  // todo: make abstract, remove body
end;

function TSlice.TimeFolder: string;
begin
  Result := fLayer.fLayerID.ToString+PathDelim+id+PathDelim;
end;

function TSlice.UpdatePalette(aPalette: TWDPalette): Boolean;
begin
  // todo: check if data lock is needed, i think not..
  fTileLock.BeginWrite;
  try
    fDataLock.BeginWrite;
    try
      fPalette.Free;
      fPalette := aPalette;
      fDataVersion := fDataVersion+1; // trigger new set of tiles in cache
      Result := True;
    finally
      fDataLock.EndWrite;
    end;
  finally
    fTileLock.EndWrite;
  end;
end;

{ TSliceReceptor }

constructor TSliceReceptor.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fNet := TDLNet.Create;
end;

// returns true if the net is triangulated
function TSliceReceptor.GenerateTilePreCalc(aThreadPool: TMyThreadPool): Boolean;
var
  distLatLon: TDistanceLatLon;
begin
  // check if triangles need to be calculated
  if fNet.Triangles.Count=0 then
  begin
    fDataLock.BeginWrite;
    try
      // use first point as reference for max edge length
      if fNet.Points.Count>0 then
      begin
        Log.WriteLn('receptor layer '+fLayer.LayerID.ToString+': triangulating '+fNet.Points.Count.ToString+' points');
        // get distance in lat/lon (degrees) for x and y in middle of extent for disabling triangles
        distLatLon := TDistanceLatLon.Create(fMaxExtent.YMin, fMaxExtent.YMax);
        //Log.WriteLn('receptor layer '+fLayer.LayerID.ToString+': before fNet.Triangulate');
        fNet.Triangulate(fLayer.MaxEdgeLengthInMeters/distLatLon.m_per_deg_lon, fLayer.MaxEdgeLengthInMeters/distLatLon.m_per_deg_lat, NaN, aThreadPool);
        //Log.WriteLn('receptor layer '+fLayer.LayerID.ToString+': after fNet.Triangulate');
        if fNet.Triangles.Count>0
        then Log.WriteLn('receptor layer '+fLayer.LayerID.ToString+': triangulated, '+fNet.Triangles.Count.ToString()+' triangles')
        else Log.WriteLn('receptor layer '+fLayer.LayerID.ToString+': triangulated, NO triangles', llError);
        Result := True;
      end
      else
      begin
        Log.WriteLn('receptor layer '+fLayer.LayerID.ToString+': triangulating '+fNet.Points.Count.ToString+' points', llWarning);
        Result := False;
      end;
    finally
      fDataLock.EndWrite;
    end;
  end
  else Result := True;
end;

function TSliceReceptor.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
begin
  fDataLock.BeginRead;
  try
    if fNet.Triangles.Count>0 then
    begin
      aValue := fNet.Triangles.PointToValue(aLon, aLat, NaN);
      Result := gtsOk;
    end
    else Result := gtsRestart;
  finally
    fDataLock.EndRead;
  end;
end;

function TSliceReceptor.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fNet.Points.Count>0 then
  begin
    fNet.Clear;
    Result := True;
  end;
end;

destructor TSliceReceptor.Destroy;
begin
  FreeAndNil(fNet);
  inherited;
end;

function TSliceReceptor.doGenerateTilePreCalc: Boolean;
begin
  Result := fNet.Triangles.Count=0;
end;

function TSliceReceptor.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  data: FMX.Graphics.TBitmapData;
  triangleCursor: TDLTriangle;
  row: Integer;
  col: Integer;
  x, y: TDLCoordinate;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    fDataLock.BeginRead;
    try
      if fNet.Triangles.Count>0 then
      begin
        if aBitmap.Map(TMapAccess.Write, data) then
        begin
          try
            if Assigned(data.data) then
            begin
              triangleCursor := nil;
              for row := 0 to aBitmap.Height-1 do
              begin
                y := aExtent.YMax-(row+0.5)*aPixelHeight;
                if (row mod 2)=0 then
                begin
                  for col := 0 to aBitmap.Width-1 do
                  begin
                    x := aExtent.XMin+(col+0.5)*aPixelWidth;
                    data.SetPixel(col, row, fPalette.ValueToColors(fNet.ValueAtPoint(triangleCursor, x, y, NaN)).fillColor);
                  end;
                end
                else
                begin
                  for col := aBitmap.Width-1 downto 0 do
                  begin
                    x := aExtent.XMin+(col+0.5)*aPixelWidth;
                    data.SetPixel(col, row, fPalette.ValueToColors(fNet.ValueAtPoint(triangleCursor, x, y, NaN)).fillColor);
                  end;
                end;
              end;
              Result := gtsOk;
            end
            else Log.WriteLn('TSliceReceptor layer '+fLayer.LayerID.ToString+': data=nil out of map bitmap', llError);
          finally
            aBitmap.Unmap(data);
          end;
        end
        else Log.WriteLn('TSliceReceptor layer '+fLayer.LayerID.ToString+': could not map bitmap', llWarning);
      end
      else Result := gtsOk; // no data, nothing to do
    finally
      fDataLock.EndRead;
    end;
  end
  else Log.WriteLn('TSliceReceptor layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceReceptor.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  fieldInfo: UInt32;
  id: TWDID;
  netPoint: TDLPoint;
  point: TWDGeometryPoint;
  len: UInt64;
  value: Double;
begin
  Result := True; // trigger refresh
  value := NaN;
  id := '';
  point := TWDGeometryPoint.Create;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        //(icehSliceID shl 3) or wt64Bit:
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            // find point
            if fNet.Points.TryGetValue(id, netPoint) then
            begin
              if not IsNaN(point.x)
              then point.x := NaN;
              if not IsNaN(value) then
              begin
                netPoint.Value := value;
                value := NaN;
              end;
            end
            else
            begin
              fNet.Points.Add(id, TDLPoint.Create(point.x, point.y, value));
              fNet.Triangles.Clear;
            end;
          end;
        (icehTilerValue shl 3) or wt64Bit:
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometryPoint shl 3) or wtLengthDelimited:
          begin
            len := aBuffer.bb_read_uint64(aCursor);
            point.Decode(aBuffer, aCursor, aCursor+Integer(len));
            if fMaxExtent.IsEmpty
            then fMaxExtent.Init(point.x, point.y)
            else fMaxExtent.Expand(point.x, point.y);
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            fNet.Points.Remove(id);
            fNet.Triangles.Clear;
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  finally
    point.Free;
  end;
end;

{ TSliceGeometryObject }

constructor TSliceGeometryObject.Create(aGeometry: TWDGeometry; aValue: Double);
begin
  inherited Create;
  fGeometry := aGeometry;
  fValue := aValue;
  if Assigned(fGeometry)
  then fExtent := TExtent.FromGeometry(fGeometry)
  else fExtent.Init;
end;

destructor TSliceGeometryObject.Destroy;
begin
  FreeAndNil(fGeometry);
  inherited;
end;

{ TSliceGeometryIHObject }

constructor TSliceGeometryIHDataPoint.Create(aTimeStamp: TDateTime; aLat, aLon, aValue, aHeight: Double);
begin
  inherited Create(aTimeStamp);
  fLat := aLat;
  fLon := aLon;
  fValue := aValue;
  fHeight := aHeight;
end;

{ TSliceGeometryICObject }

constructor TSliceGeometryICObject.Create(aGeometry: TWDGeometry; aValue, aTexture: Double);
begin
  inherited Create(aGeometry, aValue);
  fTexture := aTexture;
end;

{ TSliceGeometryICLRObject }

constructor TSliceGeometryICLRObject.Create(aGeometry: TWDGeometry; aValue, aValue2, aTexture, aTexture2: Double);
begin
  inherited Create(aGeometry, aValue, aTexture);
  fValue2 := aValue2;
  fTexture2 := aTexture2;
end;

{ TSliceLocationObject }

constructor TSliceLocationObject.Create(aLocation: TWDGeometryPoint; aValue, aRadius: Double);
var
  dist: TDistanceLatLon;
  radiusInMeters: Double;
begin
  inherited Create;
  fLocation := aLocation;
  fValue := aValue;
  fRadius := aRadius;
  fExtent := TExtent.Create;
  fExtent.Init(fLocation.x, fLocation.y);
  dist := TDistanceLatLon.Create(aLocation.y);
  radiusInMeters := 10; // todo: 10 meter radius for clicking etc..
  fExtent.Inflate(radiusInMeters/dist.m_per_deg_lon, radiusInMeters/dist.m_per_deg_lon);
end;

destructor TSliceLocationObject.Destroy;
begin
  FreeAndNil(fLocation);
  inherited;
end;

{ TSliceGeometry }

constructor TSliceGeometry.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fGeometries := TObjectDictionary<TWDID, TSliceGeometryObject>.Create([doOwnsValues]);
  fLineWidth := 2;
end;

function TSliceGeometry.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fGeometries.Count>0 then
  begin
    fGeometries.Clear;
    Result := True;
  end;
end;

destructor TSliceGeometry.Destroy;
begin
  FreeAndNil(fGeometries);
  inherited;
end;

function TSliceGeometry.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryObject>;
  polygon: TPolygon;
  colors: TGeoColors;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      // draw geometries
      for isgop in fGeometries do
      begin
        if aExtent.Intersects(isgop.Value.fExtent) then
        begin
          polygon := GeometryToPolygon(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
          colors :=  fPalette.ValueToColors(isgop.Value.value);
          if colors.fillColor<>0 then
          begin
            aBitmap.Canvas.Fill.Color := colors.fillColor;
            aBitmap.Canvas.FillPolygon(polygon, 1);
          end;
          if colors.outlineColor<>0 then
          begin
            aBitmap.Canvas.Stroke.Color := colors.outlineColor;
            aBitmap.Canvas.DrawPolygon(polygon, 1);
          end;
        end;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceGeometry layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceGeometry.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryObject>;
begin
  fDataLock.BeginRead;
  try
    for isgop in fGeometries do
    begin
      if isgop.Value.extent.Contains(aLon, aLat) then
      begin
        // test geometry, exit(isgop.Value) if within
        if isgop.Value.fGeometry.PointInFirstPart(aLon, aLat) then
        begin
          aValue := isgop.Value.value;
          exit(gtsOk);
        end;
      end;
    end;
    aValue := NaN;
    exit(gtsFailed);
  finally
    fDataLock.EndRead;
  end;
end;

function TSliceGeometry.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  id: TWDID;
  fieldInfo: Uint32;
  geometry: TWDGeometry;
  len: Uint64;
  sgo: TSliceGeometryObject;
  value: Double;
begin
  Result := True; // trigger refresh
  id := '';
  value := NaN;
  geometry := nil;
  sgo := nil;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if fGeometries.TryGetValue(id, sgo) then
            begin
              if Assigned(geometry) then
              begin
                // Assume that geometry is modified if we received geometry in the payload.
                // We remove the geometry from fLocations. The list will free the geometry.
                fGeometries.Remove(id);
                sgo := TSliceGeometryObject.Create(geometry, value);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                value := NaN;
                geometry := nil; // transfer ownership
              end
              else
              begin
                if not IsNaN(value) then
                begin
                  sgo.value := value;
                end;
              end;
            end
            else
            begin
              if Assigned(geometry) then
              begin
                sgo := TSliceGeometryObject.Create(geometry, value);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                geometry := nil; // transfer ownership
                value := NaN;
              end;
            end;
          end;
        (icehTilerValue shl 3) or wt64Bit:
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometry shl 3) or wtLengthDelimited:
          begin
            geometry.Free;
            geometry := TWDGeometry.Create;
            len := aBuffer.bb_read_uint64(aCursor);
            geometry.Decode(aBuffer, aCursor, aCursor+Integer(len));
          end;
        (icehTilerLineWidth shl 3) or wt64Bit:
          begin
            fLineWidth := aBuffer.bb_read_double(aCursor);
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            fGeometries.Remove(id);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;


  finally
    geometry.Free;
  end;
end;

{ TSliceGeometryI }

function TSliceGeometryI.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryObject>;
  path: TPathData;
  bufferExtent: TExtent;
begin
  aBitmap.Canvas.BeginScene;
  try
    aBitmap.Canvas.Clear(0);
    aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
    // adjust extent to check
    bufferExtent := aExtent.Inflate(aPixelWidth*2, aPixelHeight*2);
    // draw geometries
    fDataLock.BeginRead;
    try
      for isgop in fGeometries do
      begin
        if bufferExtent.Intersects(isgop.Value.fExtent) then
        begin
          path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
          try
            if Assigned(fPalette)
            then aBitmap.Canvas.Stroke.Color := fPalette.ValueToColors(isgop.Value.value).mainColor
            else aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue or TAlphaColorRec.Alpha;
            aBitmap.Canvas.Stroke.Thickness := 2; // todo: adjustable width ie parameter?
            aBitmap.Canvas.DrawPath(path, 1);
          finally
            path.Free;
          end;
        end;
      end;
    finally
      fDataLock.EndRead;
    end;
  finally
    aBitmap.Canvas.EndScene;
  end;
  Result := gtsOk;
end;

function TSliceGeometryI.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
begin
  aValue := NaN;
  Result := gtsFailed;
end;

{ TSliceGeometryIH }

function TSliceGeometryIH.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fDataPoints.Count>0 then
  begin
    fDataPoints.Clear;
    Result := True;
  end;
end;

constructor TSliceGeometryIH.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aLineThickness: Double);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fDataPoints := TObjectDictionary<TWDID, TOrderedListTS<TSliceGeometryIHDataPoint>>.Create();
  fLineThicknesss := 1;
end;

destructor TSliceGeometryIH.Destroy;
begin
  FreeAndNil(fDataPoints);
  inherited;
end;

function TSliceGeometryIH.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  maxHeight: Double;
  bufferExtent: TExtent;
  colors: TGeoColors;
  dp: TSliceGeometryIHDataPoint;
  x, y, height: Double;
  prevX, prevY, prevHeight: Double;
  ilp: TPair<TWDID, TOrderedListTS<TSliceGeometryIHDataPoint>>;
  polygon: TPolygon;
  dataExtent: TExtent;
  prevDPLon: Double;
  prevDPLat: Double;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    setLength(polygon, 4);
    //TMonitor.Enter(canvasLock);
    //try
      aBitmap.Canvas.BeginScene;
      try
        aBitmap.Canvas.Clear(0);
        aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        aBitmap.Canvas.Stroke.Thickness := fLineThicknesss;
        fDataLock.BeginRead;
        try

          // determine max value of height for stretching extent
          maxHeight := 0;
          for ilp in fDataPoints do
          begin
            for dp in ilp.Value do
            begin
              if maxHeight<dp.height
              then maxHeight := dp.height;
            end;
          end;
          bufferExtent := aExtent.Inflate(0, maxHeight*aPixelHeight); // todo: only up?
          // draw data points
          for ilp in fDataPoints do
          begin
            prevX := Double.NaN;
            prevY := Double.NaN;
            prevDPLon := Double.NaN;
            prevDPLat := Double.NaN;
            prevHeight := Double.NaN;
            for dp in ilp.Value do
            begin
              x := (dp.lon-aExtent.xMin)/aPixelWidth;
              y := (aExtent.yMax-dp.lat)/aPixelHeight;
              height := dp.height;
              dataExtent.Init(dp.lon, dp.lat);
              if not prevDPLon.IsNan
              then dataExtent.Expand(prevDPLon, prevDPLat);
              if bufferExtent.Intersects(dataExtent) then
              begin
                colors := fPalette.ValueToColors(dp.Value);
                aBitmap.Canvas.Stroke.Color := colors.mainColor;
                aBitmap.Canvas.Fill.Color := colors.mainColor;
                polygon[0].X := x;
                polygon[0].Y := y;
                polygon[1].X := x;
                polygon[1].Y := y-height;
                // draw connection to this point
                if not prevX.IsNan then
                begin
                  polygon[2].X := prevX;
                  polygon[2].Y := prevY-prevHeight;
                  polygon[3].X := prevX;
                  polygon[3].Y := prevY;
                  aBitmap.Canvas.DrawPolygon(polygon, 1);
                  aBitmap.Canvas.FillPolygon(polygon, 0.5);
                end
                else aBitmap.Canvas.DrawLine(polygon[0], polygon[1], 1);
              end;
              prevX := x;
              prevY := y;
              prevDPLon := dp.lon;
              prevDPLat := dp.lat;
              prevHeight := height;
            end;
          end;
        finally
          fDataLock.EndRead;
        end;
        Result := gtsOk;
      finally
        aBitmap.Canvas.EndScene;
      end;
    //finally
    //  TMonitor.Exit(canvasLock);
    //end;
  end
  else Log.WriteLn('TSliceGeometryIH layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceGeometryIH.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
begin
  aValue := NaN;
  Result := gtsFailed;
end;

function TSliceGeometryIH.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  ts: TDateTime;
  fieldInfo: Uint32;
  len: Uint64;
  list: TOrderedListTS<TSliceGeometryIHDataPoint>;
  sgo: TSliceGeometryIHDataPoint;
  value: Double;
  height: double;
  geometryPoint: TWDGeometryPoint;
  id: TWDID;
begin
  Result := False;
  value := NaN;
  height := NaN;
  geometryPoint := TWDGeometryPoint.Create;
  list := nil;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if not fDataPoints.TryGetValue(id, list) then
            begin
              list := TOrderedListTS<TSliceGeometryIHDataPoint>.Create;
              fDataPoints.AddOrSetValue(id, list);
            end;
          end;
        (icehObjectTS shl 3) or wt64Bit:
          begin
            ts := aBuffer.bb_read_double(aCursor);
            if assigned(list) then
            begin
              fMaxExtent.Expand(geometryPoint.x, geometryPoint.y);
              sgo := list.ItemsTS[ts, True];
              if Assigned(sgo) then
              begin
                // update existing data point
                if not geometryPoint.x.IsNan
                then sgo.fLon := geometryPoint.x;
                if not geometryPoint.y.IsNan
                then sgo.fLat := geometryPoint.y;
                if not value.IsNan
                then sgo.value := value;
                if not height.IsNan
                then sgo.height := height;
              end
              else
              begin
                sgo := TSliceGeometryIHDataPoint.Create(ts, geometryPoint.y, geometryPoint.x, value, height);
                list.Add(sgo);
              end;
              Result := True;
            end
            else Log.WriteLn('list not set before icehObjectTS in TSliceGeometryIH.HandleSliceUpdate', llError);
          end;
        (icehTilerValue shl 3) or wt64Bit: // color of track
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerValue2 shl 3) or wt64Bit: // "height" in pixels of track
          begin
            height := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometryPoint shl 3) or wtLengthDelimited:
          begin
            len := aBuffer.bb_read_uint64(aCursor);
            geometryPoint.Decode(aBuffer, aCursor, aCursor+Integer(len));
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if fDataPoints.ContainsKey(id) then
            begin
              fDataPoints.Remove(id);
              Result := True;
            end;
          end;
        (icehNoObjectTS shl 3) or wt64Bit:
          begin
            ts := aBuffer.bb_read_double(aCursor);
            if Assigned(list) then
            begin
              list.RemoveTS(ts);
              Result := True;
            end;
            // todo: log error that list is not set?
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  finally
    geometryPoint.Free;
  end;
end;

{ TSliceGeometryIC }

constructor TSliceGeometryIC.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fGeometries := TObjectDictionary<TWDID, TSliceGeometryICObject>.Create([doOwnsValues]);
end;

function TSliceGeometryIC.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fGeometries.Count>0 then
  begin
    fGeometries.Clear;
    Result := True;
  end;
end;

destructor TSliceGeometryIC.Destroy;
begin
  FreeAndNil(fGeometries);
  inherited;
end;

function TSliceGeometryIC.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryICObject>;
  capacityFactor: Double;
  path: TPathData;
  maxValue: Double;
  maxPixels: Double;
  bufferExtent: TExtent;
  colors: TGeoColors;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      capacityFactor := 1/(100000*Abs(aExtent.YMax-aExtent.YMin)); // todo: determine based on center of slice extent?
      fDataLock.BeginRead;
      try
        // determine max value
        maxValue := double.NaN;
        for isgop in fGeometries do
        begin
          if not isgop.Value.value.IsNan then
          begin
            if maxValue.IsNan or (maxValue<isgop.Value.value)
            then maxValue := isgop.Value.value;
          end;
        end;
        // adjust extent to max value
        if maxValue.IsNan
        then maxPixels := 0
        else maxPixels := ceil(abs(maxValue*capacityFactor));
        bufferExtent := aExtent.Inflate(maxPixels*aPixelWidth, maxPixels*aPixelHeight);
        // draw geometries
        for isgop in fGeometries do
        begin
          if bufferExtent.Intersects(isgop.Value.fExtent) and not isgop.Value.value.IsNan then
          begin
            colors := fPalette.ValueToColors(isgop.Value.texture);
            path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
            try
              aBitmap.Canvas.Stroke.Color := colors.mainColor;
              aBitmap.Canvas.Stroke.Thickness := isgop.Value.value*capacityFactor;
              aBitmap.Canvas.DrawPath(path, 1);
            finally
              path.Free;
            end;
          end;
        end;
      finally
        fDataLock.EndRead;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceGeometryIC layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceGeometryIC.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
begin
  aValue := NaN;
  Result := gtsFailed;
end;

function TSliceGeometryIC.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  id: TWDID;
  fieldInfo: Uint32;
  geometry: TWDGeometry;
  len: Uint64;
  sgo: TSliceGeometryICObject;
  value: Double;
  texture: Double;
begin
  Result := True; // trigger refresh
  id := '';
  value := NaN;
  texture := NaN;
  geometry := nil;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if fGeometries.TryGetValue(id, sgo) then
            begin
              if Assigned(geometry) then
              begin
                // Assume that geometry is modified if we received geometry in the payload.
                // We remove the geometry from fLocations. The list will free the geometry.
                fGeometries.Remove(id);
                sgo := TSliceGeometryICObject.Create(geometry, value, texture);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                geometry := nil;
              end
              else
              begin
                if not IsNaN(value) then
                begin
                  sgo.value := value;
                end;
                if not IsNaN(texture) then
                begin
                  sgo.texture := texture;
                end;
              end;
            end
            else
            begin
              sgo := TSliceGeometryICObject.Create(geometry, value, texture);
              if fMaxExtent.IsEmpty
              then fMaxExtent := sgo.extent
              else fMaxExtent.Expand(sgo.extent);
              fGeometries.Add(id, sgo);
              geometry := nil;
              value := NaN;
              texture := NaN;
            end;
          end;
        (icehTilerValue shl 3) or wt64Bit:
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerTexture shl 3) or wt64Bit:
          begin
            texture := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometry shl 3) or wtLengthDelimited:
          begin
            geometry.Free;
            geometry := TWDGeometry.Create;
            len := aBuffer.bb_read_uint64(aCursor);
            geometry.Decode(aBuffer, aCursor, aCursor+Integer(len));
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            fGeometries.Remove(id);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  finally
    geometry.Free;
  end;
end;

{ TSliceGeometryICLR }

constructor TSliceGeometryICLR.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fGeometries := TObjectDictionary<TWDID, TSliceGeometryICLRObject>.Create([doOwnsValues]);
end;

function TSliceGeometryICLR.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fGeometries.Count>0 then
  begin
    fGeometries.Clear;
    Result := True;
  end;
end;

destructor TSliceGeometryICLR.Destroy;
begin
  FreeAndNil(fGeometries);
  inherited;
end;

function TSliceGeometryICLR.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryICLRObject>;
  polygon: TPolygon;
  capacityFactor: Double;
  maxValue: Double;
  maxPixels: Double;
  bufferExtent: TExtent;
  // polygon drawing
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  path: TPathData;
  x, y, xPrev, yPrev, xn, yn, xd, yd: Double;
  l: Double;
  colors: TGeoColors;
  colors2: TGeoColors;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      capacityFactor := 0.001/Abs(aExtent.YMax-aExtent.YMin);
      fDataLock.BeginRead;
      try
        // calculate max value
        maxValue := double.NaN;
        for isgop in fGeometries do
        begin
          if not isgop.Value.value.IsNan then
          begin
            if maxValue.IsNan or (maxValue<isgop.Value.value)
            then maxValue := isgop.Value.value;
          end;
          if not isgop.Value.value2.IsNan then
          begin
            if maxValue.IsNan or (maxValue<isgop.Value.value2)
            then maxValue := isgop.Value.value2;
          end;
        end;
        // adjust extent to max dynamic width of geometry
        if maxValue.IsNan
        then maxPixels := 0
        else maxPixels := ceil(abs(maxValue*capacityFactor));
        bufferExtent := aExtent.Inflate(maxPixels*aPixelWidth, maxPixels*aPixelHeight);
        for isgop in fGeometries do
        begin
          if bufferExtent.Intersects(isgop.Value.fExtent) then
          begin
            if IsNaN(isgop.Value.texture) and IsNaN(isgop.Value.texture2) then
            begin // both values are NaN -> draw small black line
              path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
              try
                aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black or TAlphaColorRec.Alpha;
                aBitmap.Canvas.Stroke.Thickness := 1;
                aBitmap.Canvas.DrawPath(path, 1);
              finally
                path.Free;
              end;
            end
            else
            begin
              if IsNaN(isgop.Value.texture2) then
              begin
                colors := fPalette.ValueToColors(isgop.Value.texture);
                colors2 := colors;
              end
              else
              begin
                colors2 := fPalette.ValueToColors(isgop.Value.texture2);
                if IsNaN(isgop.Value.texture)
                then colors := colors2
                else colors := fPalette.ValueToColors(isgop.Value.texture);
              end;
              setLength(polygon, 5);
              for part in isgop.Value.fGeometry.parts do
              begin
                x := NaN;
                y := NaN;
                for point in part.points do
                begin
                  // recalc coordinates relative to extent
                  xPrev := x;
                  yPrev := y;
                  // adjust for x/y ratio ie pixels not being square
                  x := (point.X-aExtent.XMin)/aPixelWidth;
                  y := (aExtent.YMax-point.Y)/aPixelHeight;
                  if not IsNaN(xPrev) then
                  begin
                    xn := y-yPrev;
                    yn := xPrev-x;
                    // normalize..
                    l := sqrt((xn*xn)+(yn*yn));
                    polygon[0].X := xPrev;
                    polygon[0].Y := yPrev;
                    polygon[1].X := x;
                    polygon[1].Y := y;
                    polygon[4].X := xPrev;
                    polygon[4].Y := yPrev;

                    // right = value2 and colors2
                    if not IsNaN(isgop.Value.value2) then
                    begin
                      xd := isgop.Value.value2*capacityFactor*xn/l;
                      yd := isgop.Value.value2*capacityFactor*yn/l;

                      polygon[2].X := x-xd;
                      polygon[2].Y := y-yd;
                      polygon[3].X := xPrev-xd;
                      polygon[3].Y := yPrev-yd;

                      // draw
                      if colors2.fillColor<>0  then
                      begin
                        aBitmap.Canvas.Fill.Color := colors2.fillColor;
                        aBitmap.Canvas.FillPolygon(polygon, 1);
                      end;
                      if colors2.outlineColor<>0 then
                      begin
                        aBitmap.Canvas.Stroke.Color := colors2.outlineColor;
                        aBitmap.Canvas.DrawPolygon(polygon, 1);
                      end;
                    end;

                    //left (value and colors)
                    if not IsNaN(isgop.Value.value) then
                    begin
                      xd := isgop.Value.value*capacityFactor*xn/l;
                      yd := isgop.Value.value*capacityFactor*yn/l;

                      // todo: wrong rotation direction..
                      polygon[2].X := x+xd;
                      polygon[2].Y := y+yd;
                      polygon[3].X := xPrev+xd;
                      polygon[3].Y := yPrev+yd;
                      // draw left
                      if colors.fillColor<>0 then
                      begin
                        aBitmap.Canvas.Fill.Color := colors.fillColor;
                        aBitmap.Canvas.FillPolygon(polygon, 1);
                      end;
                      if colors.outlineColor<>0 then
                      begin
                        aBitmap.Canvas.Stroke.Color := colors.outlineColor;
                        aBitmap.Canvas.DrawPolygon(polygon, 1);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        fDataLock.EndRead;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceGeometryICLR layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceGeometryICLR.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
begin
  aValue := NaN;
  Result := gtsFailed;
end;

function TSliceGeometryICLR.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  id: TWDID;
  fieldInfo: Uint32;
  geometry: TWDGeometry;
  len: Uint64;
  sgo: TSliceGeometryICLRObject;
  value: Double;
  texture: Double;
  value2: Double;
  texture2: Double;
begin
  Result := True; // trigger refresh
  id := '';
  value := NaN;
  value2 := NaN;
  texture := NaN;
  texture2 := NaN;
  geometry := nil;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if fGeometries.TryGetValue(id, sgo) then
            begin
              if Assigned(geometry) then
              begin
                // Assume that geometry is modified if we received geometry in the payload.
                // We remove the geometry from fLocations. The list will free the geometry.
                fGeometries.Remove(id);
                sgo := TSliceGeometryICLRObject.Create(geometry, value, value2, texture, texture2);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                geometry := nil; // do not free. Object is owned by fLocations
              end
              else
              begin
                if not IsNaN(value) then
                begin
                  sgo.value := value;
                end;
                if not IsNaN(value2) then
                begin
                  sgo.value2 := value2;
                end;
                if not IsNaN(texture) then
                begin
                  sgo.texture := texture;
                end;
                if not IsNaN(texture2) then
                begin
                  sgo.texture2 := texture2;
                end;
              end;
            end
            else
            begin
              if Assigned(geometry) then
              begin
                sgo := TSliceGeometryICLRObject.Create(geometry, value, value2, texture, texture2);
                if fMaxExtent.IsEmpty
                  then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                geometry := nil;
                value := NaN;
              end;
            end;
          end;
        (icehTilerValue shl 3) or wt64Bit:
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerTexture shl 3) or wt64Bit:
          begin
            texture := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerValue2 shl 3) or wt64Bit:
          begin
            value2 := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerTexture2 shl 3) or wt64Bit:
          begin
            texture2 := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometry shl 3) or wtLengthDelimited:
          begin
            geometry.Free;
            geometry := TWDGeometry.Create;
            len := aBuffer.bb_read_uint64(aCursor);
            geometry.Decode(aBuffer, aCursor, aCursor+Integer(len));
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            fGeometries.Remove(id);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;


  finally
    geometry.Free;
  end;
end;

{ TSlicePOI }

constructor TSlicePOI.Create(aLayer: TLayer; aTimeStamp: TDateTime; const aImages: array of FMX.Graphics.TBitmap);
begin
  inherited Create(aLayer, nil, aTimeStamp);
  fImages := TObjectList<FMX.Graphics.TBitmap>.Create;
  fPOIs := TDictionary<Integer, TPOI>.Create;
  // todo: copy images to fImages

end;

function TSlicePOI.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fPOIs.Count>0 then
  begin
    fPOIs.Clear;
    Result := True;
  end;
end;

destructor TSlicePOI.Destroy;
begin
  FreeAndNil(fImages);
  FreeAndNil(fPOIs);
  inherited;
end;

function TSlicePOI.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
begin
  // todo:
  // have to draw pois on neighbouring tiles because of pissible overlap of image,
  // calculating bigger extent from image size is hard..?
  Result := gtsOk;
end;

function TSlicePOI.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
begin
  // todo: implement
  aCursor := aLimit;
  Result := True; // trigger refresh
end;

{ TSlicePNG }

constructor TSlicePNG.Create(aLayer: TLayer; aTimeStamp: TDateTime; aExtent: TExtent; aImage: FMX.Graphics.TBitmap; aDiscreteColorsOnStretch: Boolean);
begin
  inherited Create(aLayer, nil, aTimeStamp);
  fExtent := aExtent;
  fImage := FMX.Graphics.TBitmap.Create(0, 0); // aImage.Width, aImage.Height);
  fImage.Assign(aImage);
  fDiscreteColorsOnStretch := aDiscreteColorsOnStretch;
end;

function TSlicePNG.ClearSlice: Boolean;
begin
  Inherited ClearSlice;
  fImage.Clear(TAlphaColorRec.Null);
  Result := True;
end;

destructor TSlicePNG.Destroy;
begin
  FreeAndNil(fImage);
  inherited;
end;

function TSlicePNG.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  data: FMX.Graphics.TBitmapData;
  //row: Integer;
  //col: Integer;
  //x, y: TDLCoordinate;
begin
  Result := gtsFailed; // sentinel
  if aBitmap.Map(TMapAccess.Write, data) then
  begin
    try
      if Assigned(data.data) then
      begin
        // todo: implement
        (*
        Log.WriteLn('generate tile from net');
        triangleCursor := nil;
        for row := 0 to aBitmap.Height-1 do
        begin
          y := aExtent.YMax-(row+0.5)*aPixelHeight;
          if (row mod 2)=0 then
          begin
            for col := 0 to aBitmap.Width-1 do
            begin
              x := aExtent.XMin+(col+0.5)*aPixelWidth;
              data.SetPixel(col, row, fPalette.ValueToColor(fNet.ValueAtPoint(triangleCursor, x, y, NaN)));
            end;
          end
          else
          begin
            for col := aBitmap.Width-1 downto 0 do
            begin
              x := aExtent.XMin+(col+0.5)*aPixelWidth;
              data.SetPixel(col, row, fPalette.ValueToColor(fNet.ValueAtPoint(triangleCursor, x, y, NaN)));
            end;
          end;
        end;
        *)
        Result := gtsOk;
      end
      else Log.WriteLn('png layer '+fLayer.LayerID.ToString+': data=nil out of map bitmap', llError);
    finally
      aBitmap.Unmap(data);
    end;
  end
  else Log.WriteLn('png layer '+fLayer.LayerID.ToString+': could not map bitmap', llWarning);
end;

function TSlicePNG.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
begin
  // todo: implement
  aCursor := aLimit;
  Result := True; // trigger refresh
end;

{ TSliceLocation }

constructor TSliceLocation.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fLocations := TObjectDictionary<TWDID, TSliceLocationObject>.Create([doOwnsValues]);
end;

function TSliceLocation.ClearSlice: Boolean;
begin
  Result := Inherited ClearSlice;
  if fLocations.Count>0 then
  begin
    fLocations.Clear;
    Result := True;
  end;
end;

destructor TSliceLocation.Destroy;
begin
  FreeAndNil(fLocations);
  inherited;
end;

function TSliceLocation.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceLocationObject>;
  point: TPointF;
  rect: TRectF;
  colors: TGeoColors;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      for isgop in fLocations do
      begin
        point := GeometryToPoint(aExtent, aPixelWidth, aPixelHeight, isgop.Value.lcoation);
        rect.Create(point);
        rect.Inflate(isgop.Value.radius, isgop.Value.radius);
        // todo: test intersection with bitmap
        if (rect.Right>=0) and (rect.Top>=0) and (rect.Left<=aBitmap.Width) and (rect.Bottom<=aBitmap.Height) then
        begin
          colors := fPalette.ValueToColors(isgop.Value.value);
          if colors.fillColor<>0 then
          begin
            aBitmap.Canvas.Fill.Color := colors.fillColor;
            aBitmap.Canvas.FillEllipse(rect, 1);
          end;
          if colors.outlineColor<>0 then
          begin
            aBitmap.Canvas.Stroke.Color := colors.outlineColor;
            aBitmap.Canvas.Stroke.Thickness := 1; // todo: default width?
            aBitmap.Canvas.DrawEllipse(rect, 1);
          end;
        end;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceLocation layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceLocation.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceLocationObject>;
begin
  fDataLock.BeginRead;
  try
    for isgop in fLocations do
    begin
      if isgop.Value.extent.Contains(aLon, aLat) then
      begin
        // test geometry, exit(isgop.Value) if within
        if isgop.Value.extent.Contains(aLon, aLat) then
        begin
          aValue := isgop.Value.value;
          exit(gtsOk);
        end;
      end;
    end;
    aValue := NaN;
    exit(gtsFailed);
  finally
    fDataLock.EndRead;
  end;
end;

function TSliceLocation.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  id: TWDID;
  fieldInfo: Uint32;
  geometry: TWDGeometryPoint;
  len: Uint64;
  sgo: TSliceLocationObject;
  value: Double;
  radius: Double;
begin
  Result := True; // trigger refresh
  id := '';
  value := NaN;
  radius := 3; // todo: parameterize default radius
  geometry := nil;
  sgo := nil;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if fLocations.TryGetValue(id, sgo) then
            begin
              if Assigned(geometry) then
              begin
                // Assume that geometry is modified if we received geometry in the payload.
                // We remove the geometry from fLocations. The list will free the geometry.
                fLocations.Remove(id);
                sgo := TSliceLocationObject.Create(geometry, value, radius);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fLocations.Add(id, sgo);
                geometry := nil; // do not free. Object is owned by fLocations
              end
              else
              begin
                if not IsNaN(value)
                then sgo.value := value;
                if not IsNaN(radius)
                then sgo.radius := radius;
              end;
            end
            else
            begin
              if Assigned(geometry) then
              begin
                sgo := TSliceLocationObject.Create(geometry, value, radius);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fLocations.Add(id, sgo);
                geometry := nil;
                value := NaN;
              end;
            end;
          end;
        (icehTilerValue shl 3) or wt64Bit:
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerLocationRadius shl 3) or wt64Bit:
          begin
            radius := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometryPoint shl 3) or wtLengthDelimited:
          begin
            geometry.Free;
            geometry := TWDGeometryPoint.Create;
            len := aBuffer.bb_read_uint64(aCursor);
            geometry.Decode(aBuffer, aCursor, aCursor+Integer(len));
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            fLocations.Remove(id);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  finally
    geometry.Free;
  end;
end;

{ TDiffSlice }

constructor TDiffSlice.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlice);
begin
  inherited Create(aLayer, aPalette, aTimeStamp);
  fCurrentSlice := aCurrentSlice;
  fRefSlice := aRefSlice;
  fMaxExtent := fCurrentSlice.fMaxExtent.Intersection(fRefSlice.fMaxExtent);
  fCurrentSlice.AddParent(self);
  fRefSlice.AddParent(self);
  // send refresh if both cur and ref are already ok after this follow updates of cur and ref
  aLayer.signalRefresh(aTimeStamp, true);
end;

destructor TDiffSlice.Destroy;
begin
  if Assigned(fCurrentSlice) then
  begin
    fCurrentSlice.RemoveParent(Self);
    fCurrentSlice := nil;
  end;
  if Assigned(fRefSlice) then
  begin
    fRefSlice.RemoveParent(Self);
    fRefSlice := nil;
  end;
  inherited;
end;

function TDiffSlice.GenerateTile(const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer;
  const aFileName, aCacheFolder: string; aThreadPool: TMyThreadPool): Integer;
var
  status: TGenerateTileStatus;
begin
  repeat
    // pre calc phase, potentially write lock of tile -> only when not avoidable
    if fCurrentSlice.doGenerateTilePreCalc then
    begin
      fCurrentSlice.fTileLock.BeginWrite;
      try
        fCurrentSlice.GenerateTilePreCalc(aThreadPool);
      finally
        fCurrentSlice.fTileLock.EndWrite;
      end;
    end;
    if fRefSlice.doGenerateTilePreCalc then
    begin
      fRefSlice.fTileLock.BeginWrite;
      try
        fRefSlice.GenerateTilePreCalc(aThreadPool);
      finally
        fRefSlice.fTileLock.EndWrite;
      end;
    end;
    // tile generation phase, max read lock of tile
    fTileLock.BeginRead;
    fCurrentSlice.fTileLock.BeginRead;
    fRefSlice.fTileLock.BeginRead;
    try
      status := GenerateTileCache(aExtent, aStream, aWidthPixels, aHeightPixels, aFileName, aCacheFolder);
    finally
      fTileLock.EndRead;
      fCurrentSlice.fTileLock.EndRead;
      fRefSlice.fTileLock.EndRead;
    end;
  until status<>gtsRestart;
  if status=gtsOk
  then Result := HSC_SUCCESS_OK
  else Result := HSC_ERROR_NOT_FOUND;
end;

function TDiffSlice.getDataValueAtPoint(const aLat, aLon: Double; var aValue: Double): TGenerateTileStatus;
var
  cur: Double;
  ref: Double;
begin
  if Assigned(fCurrentSlice) and Assigned(fRefSlice) then
  begin
    fCurrentSlice.fDataLock.BeginRead;
    fRefSlice.fDataLock.BeginRead;
    try
      Result := fCurrentSlice.getDataValueAtPoint(aLat, aLon, cur);
      if Result=gtsOk then 
      begin
        Result := fRefSlice.getDataValueAtPoint(aLat, aLon, ref);
        if Result=gtsOk then
        begin
          if IsNaN(cur) or IsNaN(ref)
          then aValue := NaN
          else aValue := cur-ref;
        end;
      end;
    finally
      fCurrentSlice.fDataLock.EndRead;
      fRefSlice.fDataLock.EndRead;
    end;
  end
  else
  begin
    aValue := NaN;
    Result := gtsFailed;
  end;
end;

procedure TDiffSlice.HandleDiffUpdate;
begin
  // recalc extent
  if Assigned(fCurrentSlice) and Assigned(fRefSlice)
  then fMaxExtent := fCurrentSlice.fMaxExtent.Intersection(fRefSlice.fMaxExtent)
  else if Assigned(fCurrentSlice)
  then fMaxExtent := fCurrentSlice.fMaxExtent
  else fMaxExtent := TExtent.Create;
  fDataVersion := fDataVersion+1; // trigger new set of tiles in cache
  fLayer.signalRefresh(timeStamp, false);
end;

procedure TDiffSlice.RemoveChild(aChild: TSlice);
begin
  if fCurrentSlice=aChild
  then fCurrentSlice := nil;
  if fRefSlice=aChild
  then fRefSlice := nil;
  HandleDiffUpdate;
end;

{ TSliceDiffReceptor }

constructor TSliceDiffReceptor.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceReceptor);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffReceptor.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  data: FMX.Graphics.TBitmapData;
  triangleCursorCur, triangleCursorRef: TDLTriangle;
  row: Integer;
  col: Integer;
  x, y: TDLCoordinate;
  cur, ref: Double;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    if aBitmap.Map(TMapAccess.Write, data) then
    begin
      try
        if Assigned(data.data) then
        begin
          triangleCursorCur := nil;
          triangleCursorRef := nil;
          fCurrentSlice.fDataLock.BeginRead;
          fRefSlice.fDataLock.BeginRead;
          try
            for row := 0 to aBitmap.Height-1 do
            begin
              y := aExtent.YMax-(row+0.5)*aPixelHeight;
              if (row mod 2)=0 then
              begin
                for col := 0 to aBitmap.Width-1 do
                begin
                  x := aExtent.XMin+(col+0.5)*aPixelWidth;
                  cur := (fCurrentSlice as TSliceReceptor).fNet.ValueAtPoint(triangleCursorCur, x, y, NaN);
                  if not IsNaN(cur) then
                  begin
                    ref := (fRefSlice as TSliceReceptor).fNet.ValueAtPoint(triangleCursorRef, x, y, NaN);
                    if not IsNaN(ref)
                    then data.SetPixel(col, row, fPalette.ValueToColors(cur-ref).fillColor)
                    else data.SetPixel(col, row, fPalette.ValueToColors(NaN).fillColor);
                  end
                  else data.SetPixel(col, row, fPalette.ValueToColors(NaN).fillColor);
                end;
              end
              else
              begin
                for col := aBitmap.Width-1 downto 0 do
                begin
                  x := aExtent.XMin+(col+0.5)*aPixelWidth;
                  cur := (fCurrentSlice as TSliceReceptor).fNet.ValueAtPoint(triangleCursorCur, x, y, NaN);
                  if not IsNaN(cur) then
                  begin
                    ref := (fRefSlice as TSliceReceptor).fNet.ValueAtPoint(triangleCursorRef, x, y, NaN);
                    if not IsNaN(ref)
                    then data.SetPixel(col, row, fPalette.ValueToColors(cur-ref).fillColor)
                    else data.SetPixel(col, row, fPalette.ValueToColors(NaN).fillColor);
                  end
                  else data.SetPixel(col, row, fPalette.ValueToColors(NaN).fillColor);
                end;
              end;
            end;
          finally
            fCurrentSlice.fDataLock.EndRead;
            fRefSlice.fDataLock.EndRead;
          end;
          Result := gtsOk;
        end
        else Log.WriteLn('TSliceDiffReceptor layer '+fLayer.LayerID.ToString+': data=nil out of map bitmap', llError);
      finally
        aBitmap.Unmap(data);
      end;
    end
    else Log.WriteLn('TSliceDiffReceptor layer '+fLayer.LayerID.ToString+': could not map bitmap', llWarning);
  end
  else Log.WriteLn('TSliceDiffReceptor layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

{ TSliceDiffOutLineFill }

constructor TSliceDiffOutLineFill.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlice);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

{ TSliceDiffGeometry }

constructor TSliceDiffGeometry.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometry);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffGeometry.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
begin
  // todo: implement
  Result := gtsFailed;
end;

{ TSliceDiffGeometryI }

constructor TSliceDiffGeometryI.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryI);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffGeometryI.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryObject>;
  refObj: TSliceGeometryObject;
  path: TPathData;
  bufferExtent: TExtent;
begin
  aBitmap.Canvas.BeginScene;
  try
    aBitmap.Canvas.Clear(0);
    aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
    // adjust extent to check
    bufferExtent := aExtent.Inflate(aPixelWidth*2, aPixelHeight*2);
    // draw geometries
    fCurrentSlice.fDataLock.BeginRead;
    fRefSlice.fDataLock.BeginRead;
    try
      for isgop in (fCurrentSlice as TSliceGeometryI).fGeometries do
      begin
        if bufferExtent.Intersects(isgop.Value.fExtent) then
        begin
          path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
          try
            if (fRefSlice as TSliceGeometryI).fGeometries.TryGetValue(isgop.Key, refObj) then
            begin
              if Assigned(fPalette)
              then aBitmap.Canvas.Stroke.Color := fPalette.ValueToColors(isgop.Value.value - refObj.value).mainColor
              else aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue or TAlphaColorRec.Alpha;
            end
            else //no refObject found color black?
            begin
              aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black or TAlphaColorRec.Alpha;
            end;
            aBitmap.Canvas.Stroke.Thickness := 2; // todo: default width?
            aBitmap.Canvas.DrawPath(path, 1);
          finally
            path.Free;
          end;
        end;
      end;
    finally
      fCurrentSlice.fDataLock.EndRead;
      fRefSlice.fDataLock.EndRead;
    end;
  finally
    aBitmap.Canvas.EndScene;
  end;
  Result := gtsOk;
end;

{ TSliceDiffGeometryIC }

constructor TSliceDiffGeometryIC.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryIC);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffGeometryIC.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryICObject>;
  capacityFactor: Double;
  path: TPathData;
  bufferExtent: TExtent;
  width: Double;
  colors: TGeoColors;
  refObj: TSliceGeometryICObject;
begin
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      capacityFactor := 0.001/Abs(aExtent.YMax-aExtent.YMin);
      bufferExtent := aExtent.Inflate(1.3); // todo: make dependent on max values like TSlideGeometryIC
      fCurrentSlice.fDataLock.BeginRead;
      fRefSlice.fDataLock.BeginRead;
      try
        for isgop in (fCurrentSlice as TSliceGeometryIC).fGeometries do
        begin
          if bufferExtent.Intersects(isgop.Value.fExtent) then
          begin
            width := Double.NaN;
            // get reference geometry object
            if (fRefSlice as TSliceGeometryIC).fGeometries.TryGetValue(isgop.Key, refObj) then
            begin
              //see if we can find width
              if not IsNaN(isgop.Value.value) then
              begin
                if not IsNaN(refObj.value)
                then width := (isgop.Value.value + refObj.value) / 2
                else width := isgop.Value.value;
              end
              else if not IsNaN(refObj.value)
              then width := refObj.value;
              //see if we can find color
              if not (IsNaN(isgop.Value.texture) or IsNaN(refObj.texture))
              then colors := fPalette.ValueToColors(isgop.Value.texture-refObj.texture)
              else width := Double.NaN;
            end;

            if isNaN(width) then //either color or value is not valid -> draw a thin black line
            begin
              path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
              try
                aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black or TAlphaColorRec.Alpha;
                aBitmap.Canvas.Stroke.Thickness := 1;
                aBitmap.Canvas.DrawPath(path, 1);
              finally
                path.Free;
              end;
            end
            else //valid color and width -> draw the path
            begin
              path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
              try
                aBitmap.Canvas.Stroke.Color := colors.mainColor;
                aBitmap.Canvas.Stroke.Thickness := width*capacityFactor;
                aBitmap.Canvas.DrawPath(path, 1);
              finally
                path.Free;
              end;
            end;
          end;
        end;
        // todo: process ref geometries not in current? or just skip..
      finally
        fCurrentSlice.fDataLock.EndRead;
        fRefSlice.fDataLock.EndRead;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceDiffGeometryIC layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

{ TSliceDiffGeometryICLR }

constructor TSliceDiffGeometryICLR.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryICLR);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffGeometryICLR.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryICLRObject>;
  polygon: TPolygon;
  capacityFactor: Double;
  // polygon drawing
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  path: TPathData;
  x, y, xPrev, yPrev, xn, yn, xd, yd: Double;
  l: Double;
  bufferExtent: TExtent;
  widthL, widthR: Double;
  colorsL: TGeoColors;
  colorsR: TGeoColors;
  validL, validR: Boolean;
  refObj: TSliceGeometryICLRObject;
begin
  widthL := Double.NaN;
  widthR := Double.NaN;
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      capacityFactor := 0.001/Abs(aExtent.YMax-aExtent.YMin);
      bufferExtent := aExtent.Inflate(1.3); // todo: make dependent on max values like TSlideGeometryICLR
      fCurrentSlice.fDataLock.BeginRead;
      fRefSlice.fDataLock.BeginRead;
      try
        for isgop in (fCurrentSlice as TSliceGeometryICLR).fGeometries do
        begin
          if bufferExtent.Intersects(isgop.Value.fExtent) then
          begin
            // get reference geometry object
            validL := True;
            validR := True;
            if not (fRefSlice as TSliceGeometryICLR).fGeometries.TryGetValue(isgop.Key, refObj) then
            begin
              validL := False;
              validR := False;
            end
            else
            begin
              //see if we can find L width
              if not IsNaN(isgop.Value.value) then
              begin
                if not IsNaN(refObj.value)
                then widthL := (isgop.Value.value + refObj.value) / 2
                else widthL := isgop.Value.value;
              end
              else
              begin
                if not IsNaN(refObj.value)
                then widthL := refObj.value
                else validL := False;
              end;
              //see if we can find R width
              if not IsNaN(isgop.Value.value2) then
              begin
                if not IsNaN(refObj.value2)
                then widthR := (isgop.Value.value2 + refObj.value2) / 2
                else widthR := isgop.Value.value2;
              end
              else
              begin
                if not IsNaN(refObj.value2)
                then widthR := refObj.value2
                else validR := False;
              end;
              //see if we can find L color
              if not (IsNaN(isgop.Value.texture) or IsNaN(refObj.texture))
              then colorsL := fPalette.ValueToColors(isgop.Value.texture-refObj.texture)
              else validL := False;
              //see if we can find R color
              if not (IsNaN(isgop.Value.texture2) or IsNaN(refObj.texture2))
              then colorsR := fPalette.ValueToColors(isgop.Value.texture2-refObj.texture2)
              else validR := False;
            end;

            if not (validR or validL) then //if neither side is valid -> draw a thin black line
            begin
              path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
              try
                aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black or TAlphaColorRec.Alpha;
                //aBitmap.Canvas.StrokeThickness := 1;
                aBitmap.Canvas.Stroke.Thickness := 1;
                aBitmap.Canvas.DrawPath(path, 1);
              finally
                path.Free;
              end;
            end
            else //draw our polygons
            begin
              setLength(polygon, 5);
              for part in isgop.Value.fGeometry.parts do
              begin
                x := NaN;
                y := NaN;
                for point in part.points do
                begin
                  // recalc coordinates relative to extent
                  xPrev := x;
                  yPrev := y;
                  x := (point.X-aExtent.XMin)/aPixelWidth;
                  y := (aExtent.YMax-point.Y)/aPixelHeight;
                  if not IsNaN(xPrev) then
                  begin
                    xn := y-yPrev;
                    yn := xPrev-x;
                    // normalize..
                    l := sqrt((xn*xn)+(yn*yn));
                    polygon[0].X := xPrev;
                    polygon[0].Y := yPrev;
                    polygon[1].X := x;
                    polygon[1].Y := y;
                    polygon[4].X := xPrev;
                    polygon[4].Y := yPrev;
                    //draw right side
                    if validR then
                    begin
                      xd := {(1+}widthR*capacityFactor{)}*xn/l;
                      yd := {(1+}widthR*capacityFactor{)}*yn/l;
                      polygon[2].X := x-xd;
                      polygon[2].Y := y-yd;
                      polygon[3].X := xPrev-xd;
                      polygon[3].Y := yPrev-yd;
                      // draw
                      if colorsR.fillColor<>0  then
                      begin
                        aBitmap.Canvas.Fill.Color := colorsR.fillColor;
                        aBitmap.Canvas.FillPolygon(polygon, 1);
                      end;
                      if colorsR.outlineColor<>0 then
                      begin
                        aBitmap.Canvas.Stroke.Color := colorsR.outlineColor;
                        aBitmap.Canvas.DrawPolygon(polygon, 1);
                      end;
                    end;
                    //draw left side
                    if validL then
                    begin
                      xd := {(1+}widthL*capacityFactor{)}*xn/l;
                      yd := {(1+}widthL*capacityFactor{)}*yn/l;
                      // todo: wrong rotation direction..
                      polygon[2].X := x+xd;
                      polygon[2].Y := y+yd;
                      polygon[3].X := xPrev+xd;
                      polygon[3].Y := yPrev+yd;
                      // draw left
                      if colorsL.fillColor<>0 then
                      begin
                        aBitmap.Canvas.Fill.Color := colorsL.fillColor;
                        aBitmap.Canvas.FillPolygon(polygon, 1);
                      end;
                      if colorsL.outlineColor<>0 then
                      begin
                        aBitmap.Canvas.Stroke.Color := colorsL.outlineColor;
                        aBitmap.Canvas.DrawPolygon(polygon, 1);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        // todo: process ref geometries not in current? or just skip..
      finally
        fCurrentSlice.fDataLock.EndRead;
        fRefSlice.fDataLock.EndRead;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceDiffGeometryICLR layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;


{ TSliceDiffGeometryICLR2 }

constructor TSliceDiffGeometryICLR2.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryICLR);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
  fGeometries := TObjectDictionary<TWDID, TSliceGeometryICLRObject>.Create([doOwnsValues]);
end;

destructor TSliceDiffGeometryICLR2.Destroy;
begin
  FreeAndNil(fGeometries);
  inherited;
end;

function TSliceDiffGeometryICLR2.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryICLRObject>;
  polygon: TPolygon;
  capacityFactor: Double;
  // polygon drawing
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  path: TPathData;
  x, y, xPrev, yPrev, xn, yn: Double;
  l: Double;
  bufferExtent: TExtent;
  widthL, widthR: Double;
  colorsL: TGeoColors;
  colorsR: TGeoColors;
  validL, validR: Boolean;
  refObj: TSliceGeometryICLRObject;

  xCommon, yCommon, xExtra, yExtra: Double;
  polygonExtra: TPolygon;
  isCommonPoly: Boolean;
  polyPoints: TArray<Double>;
begin
  widthL := Double.NaN;
  widthR := Double.NaN;
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      capacityFactor := 0.001/Abs(aExtent.YMax-aExtent.YMin);
      bufferExtent := aExtent.Inflate(1.3); // todo: make dependent on max values like TSlideGeometryICLR
      fCurrentSlice.fDataLock.BeginRead;
      fRefSlice.fDataLock.BeginRead;
      try
        for isgop in (fCurrentSlice as TSliceGeometryICLR).fGeometries do
        begin
          if bufferExtent.Intersects(isgop.Value.fExtent) then
          begin
            // get reference geometry object
            validL := True;
            validR := True;
            if not (fRefSlice as TSliceGeometryICLR).fGeometries.TryGetValue(isgop.Key, refObj) then
            begin
              validL := False;
              validR := False;
            end
            else
            begin
              //see if we can find L width
              if not IsNaN(isgop.Value.value) then
              begin
                if not IsNaN(refObj.value) then
                  widthL := (refObj.value - isgop.Value.value)
                else widthL := isgop.Value.value;
              end
              else
              begin
                if not IsNaN(refObj.value)
                then widthL := refObj.value
                else validL := False;
              end;

              //see if we can find R width
              if not IsNaN(isgop.Value.value2) then
              begin
                if not IsNaN(refObj.value2) then
                  widthR := (refObj.value2 - isgop.Value.value2)
                else widthR := isgop.Value.value2;
              end
              else
              begin
                if not IsNaN(refObj.value2)
                then widthR := refObj.value2
                else validR := False;
              end;

              //see if we can find L color
              if not (IsNaN(isgop.Value.texture) or IsNaN(refObj.texture)) then
              begin
                //colorsL := fPalette.ValueToColors(isgop.Value.texture-refObj.texture)
                if widthL > 0 then
                  colorsL := fPalette.ValueToColors(fPalette.minValue())
                else if widthL < 0 then
                  colorsL := fPalette.ValueToColors(fPalette.maxValue())
                else
                  colorsL := fPalette.ValueToColors((fPalette.minValue() + fPalette.maxValue())/2);
              end
              else validL := False;

              //see if we can find R color
              if not (IsNaN(isgop.Value.texture2) or IsNaN(refObj.texture2)) then
              begin
                //colorsR := fPalette.ValueToColors(isgop.Value.texture2 - refObj.texture2)
                if widthR > 0 then
                  colorsR := fPalette.ValueToColors(fPalette.minValue())
                else if widthR < 0 then
                  colorsR := fPalette.ValueToColors(fPalette.maxValue())
                else
                  colorsR := fPalette.ValueToColors((fPalette.minValue() + fPalette.maxValue())/2);
              end
              else validR := False;
            end;

            if not (validR or validL) then //if neither side is valid -> draw a thin black line
            begin
              path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
              try
                aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black or TAlphaColorRec.Alpha;
                aBitmap.Canvas.Stroke.Thickness := 1;
                aBitmap.Canvas.DrawPath(path, 1);
              finally
                path.Free;
              end;
            end
            else //draw our polygons
            begin
              setLength(polygon, 5);
              for part in isgop.Value.fGeometry.parts do
              begin
                x := NaN;
                y := NaN;
                for point in part.points do
                begin
                  // recalc coordinates relative to extent
                  xPrev := x;
                  yPrev := y;
                  x := (point.X-aExtent.XMin)/aPixelWidth;
                  y := (aExtent.YMax-point.Y)/aPixelHeight;
                  if not IsNaN(xPrev) then
                  begin
                    xn := y-yPrev;
                    yn := xPrev-x;
                    // normalize..
                    l := sqrt((xn*xn)+(yn*yn));

                    //draw right side
                    if validR then
                    begin
                      isCommonPoly := True;
                      xCommon := ComputeCoordinate(widthR, isgop.Value.value2, refObj.value2, capacityFactor, xn, l, isCommonPoly);
                      yCommon := ComputeCoordinate(widthR, isgop.Value.value2, refObj.value2, capacityFactor, yn, l, isCommonPoly);

                      isCommonPoly := False;
                      xExtra := ComputeCoordinate(widthR, isgop.Value.value2, refObj.value2, capacityFactor, xn, l, isCommonPoly);
                      yExtra := ComputeCoordinate(widthR, isgop.Value.value2, refObj.value2, capacityFactor, yn, l, isCommonPoly);

                      polyPoints := [xPrev, yPrev, x, y, x-xCommon, y-yCommon, xPrev-xCommon, yPrev-yCommon];
                      polygon := CreatePolygon(polyPoints);

                      if xCommon <> xExtra then
                      begin
                        setLength(polygonExtra, 5);
                        polyPoints := [polygon[3].X, polygon[3].Y, polygon[2].X, polygon[2].Y, x-xExtra, y-yExtra, xPrev-xExtra, yPrev-yExtra];
                        polygonExtra := CreatePolygon(polyPoints);
                      end;

                      // draw
                      if colorsR.fillColor<>0  then
                      begin
                        if xCommon <> xExtra then
                        begin
                          aBitmap.Canvas.Fill.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.FillPolygon(polygon, 1);

                          aBitmap.Canvas.Fill.Color := colorsR.fillColor;
                          aBitmap.Canvas.FillPolygon(polygonExtra, 1);
                        end
                        else
                        begin
                          aBitmap.Canvas.Fill.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.FillPolygon(polygon, 1);
                        end;

                      end;
                      if colorsR.outlineColor<>0 then
                      begin
                        if xCommon <> xExtra then
                        begin
                          aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.DrawPolygon(polygon, 1);

                          aBitmap.Canvas.Stroke.Color := colorsR.outlineColor;
                          aBitmap.Canvas.DrawPolygon(polygonExtra, 1);
                        end
                        else
                        begin
                          aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.DrawPolygon(polygon, 1);
                        end;
                      end;
                    end;


                    //draw left side
                    if validL then
                    begin
                      isCommonPoly := True;
                      xCommon := ComputeCoordinate(widthL, isgop.Value.value, refObj.value, capacityFactor, xn, l, isCommonPoly);
                      yCommon := ComputeCoordinate(widthL, isgop.Value.value, refObj.value, capacityFactor, yn, l, isCommonPoly);

                      isCommonPoly := False;
                      xExtra := ComputeCoordinate(widthL, isgop.Value.value, refObj.value, capacityFactor, xn, l, isCommonPoly);
                      yExtra := ComputeCoordinate(widthL, isgop.Value.value, refObj.value, capacityFactor, yn, l, isCommonPoly);

                      polyPoints := [xPrev, yPrev, x, y, x+xCommon, y+yCommon, xPrev+xCommon, yPrev+yCommon];
                      polygon := CreatePolygon(polyPoints);

                      if xCommon <> xExtra then
                      begin
                        setLength(polygonExtra, 5);
                        polyPoints := [polygon[3].X, polygon[3].Y, polygon[2].X, polygon[2].Y, x+xExtra, y+yExtra, xPrev+xExtra, yPrev+yExtra];
                        polygonExtra := CreatePolygon(polyPoints);
                      end;

                      // draw left
                      if colorsL.fillColor<>0 then
                      begin
                        if xCommon <> xExtra then
                        begin
                          aBitmap.Canvas.Fill.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.FillPolygon(polygon, 1);

                          aBitmap.Canvas.Fill.Color := colorsL.fillColor;
                          aBitmap.Canvas.FillPolygon(polygonExtra, 1);
                        end
                        else
                        begin
                          aBitmap.Canvas.Fill.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.FillPolygon(polygon, 1);
                        end;
                      end;
                      if colorsL.outlineColor<>0 then
                      begin
                        if xCommon <> xExtra then
                        begin
                          aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.DrawPolygon(polygon, 1);

                          aBitmap.Canvas.Stroke.Color := colorsL.outlineColor;
                          aBitmap.Canvas.DrawPolygon(polygonExtra, 1);
                        end
                        else
                        begin
                          aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Lightsteelblue;//LtGray;
                          aBitmap.Canvas.DrawPolygon(polygon, 1);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        // todo: process ref geometries not in current? or just skip..
      finally
        fCurrentSlice.fDataLock.EndRead;
        fRefSlice.fDataLock.EndRead;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceDiffGeometryICLR2 layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceDiffGeometryICLR2.ComputeCoordinate(aWidth, aActiveValue, aRefValue, aCapacityFactor, aXY_Diff, aPerpDist: Double; aIsCommonPoly: Boolean): Double;
begin
  if aWidth > 0 then
  begin
    if aIsCommonPoly then
      Result := aActiveValue*aCapacityFactor*aXY_Diff/aPerpDist
    else
      Result := aRefValue*aCapacityFactor*aXY_Diff/aPerpDist;
  end
  else
  begin
    if aWidth < 0 then
    begin
      if aIsCommonPoly then
        Result := aRefValue*aCapacityFactor*aXY_Diff/aPerpDist
      else
        Result := aActiveValue*aCapacityFactor*aXY_Diff/aPerpDist;
    end
    else
      Result := aRefValue*aCapacityFactor*aXY_Diff/aPerpDist;
  end;
end;

function TSliceDiffGeometryICLR2.CreatePolygon(aPolyPoints: TArray<Double>): TPolygon;
var
  polygon: TPolygon;
  ele: Integer;
begin
  setLength(polygon, 5);
  ele := 0;
  While ele < length(aPolyPoints) do
  begin
    polygon[trunc(ele/2)].X := aPolyPoints[ele];
    inc(ele,1);
    polygon[trunc(ele/2)].Y := aPolyPoints[ele];
    inc(ele,1);
  end;

  polygon[trunc(ele/2)].X := aPolyPoints[0];
  ele := ele+1;
  polygon[trunc(ele/2)].Y := aPolyPoints[1];

  Result := polygon;
end;

function TSliceDiffGeometryICLR2.HandleSliceUpdate(const aBuffer: TByteBuffer; var aCursor: Integer; aLimit: Integer): Boolean;
var
  id: TWDID;
  fieldInfo: Uint32;
  geometry: TWDGeometry;
  len: Uint64;
  sgo: TSliceGeometryICLRObject;
  value: Double;
  texture: Double;
  value2: Double;
  texture2: Double;
begin
  Result := True; // trigger refresh
  id := '';
  value := NaN;
  value2 := NaN;
  texture := NaN;
  texture2 := NaN;
  geometry := nil;
  try
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
        (icehObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            if fGeometries.TryGetValue(id, sgo) then
            begin
              if Assigned(geometry) then
              begin
                // Assume that geometry is modified if we received geometry in the payload.
                // We remove the geometry from fLocations. The list will free the geometry.
                fGeometries.Remove(id);
                sgo := TSliceGeometryICLRObject.Create(geometry, value, value2, texture, texture2);
                if fMaxExtent.IsEmpty
                then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                geometry := nil; // do not free. Object is owned by fLocations
              end
              else
              begin
                if not IsNaN(value) then
                begin
                  sgo.value := value;
                end;
                if not IsNaN(value2) then
                begin
                  sgo.value2 := value2;
                end;
                if not IsNaN(texture) then
                begin
                  sgo.texture := texture;
                end;
                if not IsNaN(texture2) then
                begin
                  sgo.texture2 := texture2;
                end;
              end;
            end
            else
            begin
              if Assigned(geometry) then
              begin
                sgo := TSliceGeometryICLRObject.Create(geometry, value, value2, texture, texture2);
                if fMaxExtent.IsEmpty
                  then fMaxExtent := sgo.extent
                else fMaxExtent.Expand(sgo.extent);
                fGeometries.Add(id, sgo);
                geometry := nil;
                value := NaN;
              end;
            end;
          end;
        (icehTilerValue shl 3) or wt64Bit:
          begin
            value := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerTexture shl 3) or wt64Bit:
          begin
            texture := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerValue2 shl 3) or wt64Bit:
          begin
            value2 := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerTexture2 shl 3) or wt64Bit:
          begin
            texture2 := aBuffer.bb_read_double(aCursor);
          end;
        (icehTilerGeometry shl 3) or wtLengthDelimited:
          begin
            geometry.Free;
            geometry := TWDGeometry.Create;
            len := aBuffer.bb_read_uint64(aCursor);
            geometry.Decode(aBuffer, aCursor, aCursor+Integer(len));
          end;
        (icehNoObjectID shl 3) or wtLengthDelimited:
          begin
            id := aBuffer.bb_read_rawbytestring(aCursor);
            fGeometries.Remove(id);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;


  finally
    geometry.Free;
  end;
end;


{ TSliceDiffGeometryICLR3 }

constructor TSliceDiffGeometryICLR3.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceGeometryICLR);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffGeometryICLR3.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceGeometryICLRObject>;
  polygon: TPolygon;
  capacityFactor: Double;
  // polygon drawing
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  path: TPathData;
  x, y, xPrev, yPrev, xn, yn, xd, yd: Double;
  l: Double;
  bufferExtent: TExtent;
  widthL, widthR: Double;
  colorsL: TGeoColors;
  colorsR: TGeoColors;
  validL, validR: Boolean;
  refObj: TSliceGeometryICLRObject;

  activeClass, refClass: Integer;
begin
  widthL := Double.NaN;
  widthR := Double.NaN;
  Result := gtsFailed; // sentinel
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      capacityFactor := 0.001/Abs(aExtent.YMax-aExtent.YMin);
      bufferExtent := aExtent.Inflate(1.3); // todo: make dependent on max values like TSlideGeometryICLR
      fCurrentSlice.fDataLock.BeginRead;
      fRefSlice.fDataLock.BeginRead;
      try
        for isgop in (fCurrentSlice as TSliceGeometryICLR).fGeometries do
        begin
          if bufferExtent.Intersects(isgop.Value.fExtent) then
          begin
            // get reference geometry object
            validL := True;
            validR := True;
            if not (fRefSlice as TSliceGeometryICLR).fGeometries.TryGetValue(isgop.Key, refObj) then
            begin
              validL := False;
              validR := False;
            end
            else
            begin
              //L width
              if IsNaN(isgop.Value.value) and IsNaN(refObj.value) then
                validL := False
              else
                widthL := 60;

              //R width
              if IsNaN(isgop.Value.value2) and IsNaN(refObj.value2) then
                validR := False
              else
                widthR := 60;

              //L color
              if not (IsNaN(isgop.Value.texture) or IsNaN(refObj.texture)) then
              begin
                activeClass := ComputeICRatioClass(isgop.Value.texture);
                refClass := ComputeICRatioClass(refObj.texture);
                if activeClass<refClass then
                  colorsL := fPalette.ValueToColors(fPalette.minValue())
                else if activeClass>refClass then
                  colorsL := fPalette.ValueToColors(fPalette.maxValue())
                else
                  colorsL := fPalette.ValueToColors((fPalette.minValue() + fPalette.maxValue())/2);
              end
              else validL := False;

              //R color
              if not (IsNaN(isgop.Value.texture2) or IsNaN(refObj.texture2)) then
              begin
                activeClass := ComputeICRatioClass(isgop.Value.texture2);
                refClass := ComputeICRatioClass(refObj.texture2);
                if activeClass<refClass then
                  colorsR := fPalette.ValueToColors(fPalette.minValue())
                else if activeClass>refClass then
                  colorsR := fPalette.ValueToColors(fPalette.maxValue())
                else
                  colorsR := fPalette.ValueToColors((fPalette.minValue() + fPalette.maxValue())/2);
              end
              else validR := False;
            end;

            if not (validR or validL) then //if neither side is valid -> draw a thin black line
            begin
              path := GeometryToPath(aExtent, aPixelWidth, aPixelHeight, isgop.Value.fGeometry);
              try
                aBitmap.Canvas.Stroke.Color := TAlphaColorRec.Black or TAlphaColorRec.Alpha;
                //aBitmap.Canvas.StrokeThickness := 1;
                aBitmap.Canvas.Stroke.Thickness := 1;
                aBitmap.Canvas.DrawPath(path, 1);
              finally
                path.Free;
              end;
            end
            else //draw our polygons
            begin
              setLength(polygon, 5);
              for part in isgop.Value.fGeometry.parts do
              begin
                x := NaN;
                y := NaN;
                for point in part.points do
                begin
                  // recalc coordinates relative to extent
                  xPrev := x;
                  yPrev := y;
                  x := (point.X-aExtent.XMin)/aPixelWidth;
                  y := (aExtent.YMax-point.Y)/aPixelHeight;
                  if not IsNaN(xPrev) then
                  begin
                    xn := y-yPrev;
                    yn := xPrev-x;
                    // normalize..
                    l := sqrt((xn*xn)+(yn*yn));
                    polygon[0].X := xPrev;
                    polygon[0].Y := yPrev;
                    polygon[1].X := x;
                    polygon[1].Y := y;
                    polygon[4].X := xPrev;
                    polygon[4].Y := yPrev;
                    //draw right side
                    if validR then
                    begin
                      xd := {(1+}widthR*capacityFactor{)}*xn/l;
                      yd := {(1+}widthR*capacityFactor{)}*yn/l;
                      polygon[2].X := x-xd;
                      polygon[2].Y := y-yd;
                      polygon[3].X := xPrev-xd;
                      polygon[3].Y := yPrev-yd;
                      // draw
                      if colorsR.fillColor<>0  then
                      begin
                        aBitmap.Canvas.Fill.Color := colorsR.fillColor;
                        aBitmap.Canvas.FillPolygon(polygon, 1);
                      end;
                      if colorsR.outlineColor<>0 then
                      begin
                        aBitmap.Canvas.Stroke.Color := colorsR.outlineColor;
                        aBitmap.Canvas.DrawPolygon(polygon, 1);
                      end;
                    end;
                    //draw left side
                    if validL then
                    begin
                      xd := {(1+}widthL*capacityFactor{)}*xn/l;
                      yd := {(1+}widthL*capacityFactor{)}*yn/l;
                      // todo: wrong rotation direction..
                      polygon[2].X := x+xd;
                      polygon[2].Y := y+yd;
                      polygon[3].X := xPrev+xd;
                      polygon[3].Y := yPrev+yd;
                      // draw left
                      if colorsL.fillColor<>0 then
                      begin
                        aBitmap.Canvas.Fill.Color := colorsL.fillColor;
                        aBitmap.Canvas.FillPolygon(polygon, 1);
                      end;
                      if colorsL.outlineColor<>0 then
                      begin
                        aBitmap.Canvas.Stroke.Color := colorsL.outlineColor;
                        aBitmap.Canvas.DrawPolygon(polygon, 1);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        // todo: process ref geometries not in current? or just skip..
      finally
        fCurrentSlice.fDataLock.EndRead;
        fRefSlice.fDataLock.EndRead;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceDiffGeometryICLR2 layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

function TSliceDiffGeometryICLR3.ComputeICRatioClass(aICValue: Double): Integer;
begin
  if aICValue <= 0.7 then
    Result := 1
  else if (aICValue > 0.7) and (aICValue <= 0.9) then
    Result := 2
  else if aICValue > 0.9 then
    Result := 3;
end;


{ TSliceDiffPOI }

constructor TSliceDiffPOI.Create(aLayer: TLayer; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlicePOI; aColorRemovedPOI, aColorSamePOI, aColorNewPOI: TAlphaRGBPixel);
begin
  inherited Create(aLayer, nil, aTimeStamp, aCurrentSlice, aRefSlice);
  fColorRemovedPOI := aColorRemovedPOI;
  fColorSamePOI := aColorSamePOI;
  fColorNewPOI := aColorNewPOI;
end;

function TSliceDiffPOI.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
begin
  // todo: implement
  Result := gtsFailed;
end;

{ TSliceDiffPNG }

constructor TSliceDiffPNG.Create(aLayer: TLayer; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSlicePNG);
begin
  inherited Create(aLayer, nil, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffPNG.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
begin
  // todo: implement
  Result := gtsFailed;
end;

{ TSliceDiffLocation }

constructor TSliceDiffLocation.Create(aLayer: TLayer; aPalette: TWDPalette; aTimeStamp: TDateTime; aCurrentSlice, aRefSlice: TSliceLocation);
begin
  inherited Create(aLayer, aPalette, aTimeStamp, aCurrentSlice, aRefSlice);
end;

function TSliceDiffLocation.GenerateTileCalc(const aExtent: TExtent; aBitmap: FMX.Graphics.TBitmap; aPixelWidth, aPixelHeight: Double): TGenerateTileStatus;
var
  isgop: TPair<TWDID, TSliceLocationObject>;
  refLoc: TSliceLocationObject;
  point: TPointF;
  rect: TRectF;
  bufferExtent: TExtent;
  colors: TGeoColors;
begin
  Result := gtsFailed;
  if Assigned(fPalette) then
  begin
    aBitmap.Canvas.BeginScene;
    try
      aBitmap.Canvas.Clear(0);
      aBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      bufferExtent := aExtent.Inflate(1.2);
      for isgop in (fCurrentSlice as TSliceLocation).fLocations do
      begin
        if bufferExtent.Intersects(isgop.Value.fExtent) and (fRefSlice as TSliceLocation).fLocations.TryGetValue(isgop.Key, refLoc) then
        begin
          point := GeometryToPoint(aExtent, aPixelWidth, aPixelHeight, isgop.Value.lcoation);
          rect.Create(point);
          rect.Inflate(isgop.Value.radius, isgop.Value.radius);
          colors := fPalette.ValueToColors(isgop.Value.value-refLoc.value);
          if colors.fillColor<>0 then
          begin
            aBitmap.Canvas.Fill.Color := colors.fillColor;
            aBitmap.Canvas.FillEllipse(rect, 1);
          end;
          if colors.outlineColor<>0 then
          begin
            aBitmap.Canvas.Stroke.Color := colors.outlineColor;
            aBitmap.Canvas.Stroke.Thickness := 1; // todo: default width?
            aBitmap.Canvas.DrawEllipse(rect, 1);
          end;
        end;
      end;
      Result := gtsOk;
    finally
      aBitmap.Canvas.EndScene;
    end;
  end
  else Log.WriteLn('TSliceDiffLocation layer '+fLayer.LayerID.ToString+': no palette defined', llError);
end;

{ TLayer }

procedure TLayer.addSlice(aSlice: TSlice);
begin
  WORMLock.BeginWrite;
  try
    fSlices.Add(aSlice);
    fSlices.Sort;
    Log.WriteLn('added layer '+self.fLayerID.ToString+' ('+self.fSliceType.ToString+')'+' slice '+aSlice.id+' '+fDataEvent.eventName);
  finally
    WORMLock.EndWrite;
  end;
end;

procedure TLayer.Clear(aMaxEdgeLengthInMeters: Double; const aDescription: string; aPersistent: Boolean);
begin
  if aPersistent  then
  begin
    if not fPersistent then
    begin
      storePersistencyInfo;
      fPersistent := true;
    end;
  end
  else
  begin
    removePersistencyInfo;
    fPersistent := false;
  end;
  fDescription := aDescription;
  fMaxEdgeLengthInMeters := aMaxEdgeLengthInMeters;
  // todo: clear slices? prob. not..
end;

constructor TLayer.Create(aModel: TModel; aLayerID: Integer; aMaxEdgeLengthInMeters: TDLCoordinate; aSliceType: Integer; const aDescription: string; aPersistent: Boolean; const aEventName: string);
begin
  inherited Create;
  fModel := aModel;
  fLayerID := aLayerID;
  fMaxEdgeLengthInMeters := aMaxEdgeLengthInMeters;
  fSlicetype := aSliceType;
  fDescription := aDescription;
  fPersistent := aPersistent;
  fWORMLock.Create;
  fSlices := TObjectList<TSlice>.Create(TComparer<TSlice>.Construct(
    // order slices on date/time
    function(const Left, Right: TSlice): Integer
    begin
      if Left.timeStamp < Right.timeStamp then
        Result := -1
      else if Left.timeStamp > Right.timeStamp then
        Result := 1
      else
        Result := 0;
    end));
  fDataEvent := fModel.Connection.eventEntry(aEventName, False).subscribe;
  fDataEvent.OnEvent.Add(handleDataEvent);
end;

function TLayer.dateTimeRange: string;
var
  l, h: TDateTime;
  z: Boolean;
  slice: TSlice;
begin
  if fSlices.Count>0 then
  begin
    l := 0;
    h := 0;
    z := False;
    for slice in fSlices do
    begin
      if slice.timeStamp<>0 then
      begin
        if (l=0) or (slice.timeStamp<l)
        then l := slice.timeStamp;
        if (h=0) or (slice.timeStamp>h)
        then h := slice.timeStamp;
      end
      else z := True;
    end;
    if l=0 then
    begin
      if z
      then Result := 'no range'
      else Result := '## no range';
    end
    else
    begin
      Result := FormatDateTime(SliceTimeIDFormat, l)+' - '+FormatDateTime(SliceTimeIDFormat, h);
      if z
      then Result := Result+' incl. 0';
    end;
  end
  else Result := 'no slices';
end;

destructor TLayer.Destroy;
begin
  if Assigned(fDataEvent) then
  begin
    fDataEvent.unSubscribe;
    fDataEvent := Nil;
  end;
  FreeAndNil(fSlices);
  inherited;
end;

function TLayer.extent: TExtent;
var
  slice: TSlice;
begin
  Result := TExtent.Create;
  for slice in fSlices do
  begin
    if Result.IsEmpty
    then Result := slice.fMaxExtent
    else Result.Expand(slice.fMaxExtent);
  end;
end;

function TLayer.findSlice(aLayerID: Integer; aTimeStamp: TDateTime): TSLice;
var
  layer: TLayer;
begin
  // find slice in specific layer
  if aLayerID<>layerID then
  begin
    globalModelAndLayersLock.BeginRead;
    try
      if not (Assigned(fModel.fLayers) and fModel.fLayers.TryGetValue(aLayerID, layer))
      then layer := nil;
      if Assigned(layer)
      then layer.WORMLock.BeginRead; //lock layer itself inside layers lock
    finally
      globalModelAndLayersLock.EndRead;
    end;
    if Assigned(layer) then
    begin
      try
        Result := layer.findSlice(aTimeStamp);
      finally
        layer.WORMLock.endRead;
      end;
    end
    else Result := nil;
  end
  else
  begin
    WORMLock.BeginRead;
    try
      Result := findSlice(aTimeStamp);
    finally
      WORMLock.EndRead;
    end;
  end;
end;

function TLayer.findSlice(aTimeStamp: TDateTime): TSLice;
var
  s: Integer;
begin
  if fSlices.Count>0 then
  begin
    if aTimeStamp<>0 then
    begin
      for s := fSlices.Count-1 downto 0 do
      begin
        if fSlices[s].timeStamp<aTimeStamp
        then exit(fSlices[s]);
      end;
    end;
    exit(fSlices.First);
  end
  else exit(nil);
end;

function TLayer.GenerateTile(aTimeStamp: TDateTime; const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer;{aBitmap: FMX.Graphics.TBitmap;} const aFileName, aCacheFolder: string; aThreadPool: TMyThreadPool): Integer;
var
  slice: TSlice;
begin
  slice := findSlice(aTimeStamp);
  if Assigned(slice)
  then Result := slice.GenerateTile(aExtent, aStream, aWidthPixels, aheightPixels, aFileName, aCacheFolder, aThreadPool)
  else
  begin
    if aTimeStamp=0
    then Log.WriteLn('Layer id '+LayerID.ToString+', slice (0) not found to generate tile from', llWarning)
    else Log.WriteLn('Layer id '+LayerID.ToString+', slice ('+FormatDateTime(SliceTimeIDFormat, aTimeStamp)+') not found to generate tile from', llWarning);
    Result := HSC_ERROR_NOT_FOUND;
  end;
end;

procedure TLayer.handleDataEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  timeStamp: TDateTime;
  slice: TSlice;
  fieldInfo: UInt32;
  size: UInt64;
  palette: TWDPalette;
  poiImages: TObjectList<FMX.Graphics.TBitmap>;
  pngExtent: TExtent;
  pngImage: FMX.Graphics.TBitmap;
  discreteColorsOnStretch: Boolean;
  colorRemovedPOI, colorSamePOI, colorNewPOI: TAlphaRGBPixel;
  currentSlice: TSlice;
  refSlice: TSlice;
  stream: TStream;
  _layerID: Integer;
  width: UInt32;
  res: Integer;
  action: UInt32;
  otherTilerURL: string;
  lineThickness: Double;
  lat, lon: Double;
  value: Double;
  pointValueRequestID: TWDID;
begin
  try
    palette :=  nil;
    poiImages := TObjectList<FMX.Graphics.TBitmap>.Create;
    pngExtent := TExtent.Create;
    pngImage := nil;
    discreteColorsOnStretch := true;
    // color use!
    colorRemovedPOI := TAlphaColorRec.Red;
    colorSamePOI := TAlphaColorRec.Gray;
    colorNewPOI := TAlphaColorRec.Green;
    currentSlice := nil;
    refSlice := nil;
    lineThickness := 1;
    lat := Double.NaN;
    lon := Double.NaN;
    _layerID := Self.LayerID; // default to this layer
    try
      // decode data event and send to correct slice (or create slice if not yet exists)
      WORMLock.BeginRead;
      try
        // check if layer is still valid
        if Assigned(DataEvent) then
        begin
          slice := nil;
          timeStamp := 0;
          while aCursor<aLimit do
          begin
            fieldInfo := aBuffer.bb_read_UInt32(aCursor);
            case fieldInfo of
              (icehTilerSliceID shl 3) or wt64Bit:
                begin
                  timeStamp := aBuffer.bb_read_double(aCursor);
                  slice := findSlice(timeStamp);
                  if not Assigned(slice) then
                  begin
                    try
                      case fSliceType of
                        stReceptor:
                          slice := TSliceReceptor.Create(Self, palette.Clone, timeStamp);
                        stGeometry:
                          slice := TSliceGeometry.Create(Self, palette.Clone, timeStamp);
                        stGeometryI:
                          slice := TSliceGeometryI.Create(Self, palette.Clone, timeStamp);
                        stGeometryIC:
                          slice := TSliceGeometryIC.Create(Self, palette.Clone, timeStamp);
                        stGeometryICLR, stGeometryICLR2, stGeometryICLR3:
                          slice := TSliceGeometryICLR.Create(Self, palette.Clone, timeStamp);
                        stGeometryIH:
                          slice := TSliceGeometryIH.Create(Self, palette.Clone, timeStamp, lineThickness);
                        stPOI:
                          slice := TSlicePOI.Create(Self, timeStamp, poiImages.ToArray);
                        stPNG:
                          slice := TSlicePNG.Create(Self, timeStamp, pngExtent, pngImage, discreteColorsOnStretch);
                        stLocation:
                          slice := TSliceLocation.Create(Self, palette.Clone, timeStamp);
                        // diff slice types
                        stDiffReceptor:
                          slice := TSliceDiffReceptor.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceReceptor, refSlice as TSliceReceptor);
                        stDiffGeometry:
                          slice := TSliceDiffGeometry.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceGeometry, refSlice as TSliceGeometry);
                        stDiffGeometryI:
                          slice := TSliceDiffGeometryI.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceGeometryI, refSlice as TSliceGeometryI);
                        stDiffGeometryIC:
                          slice := TSliceDiffGeometryIC.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceGeometryIC, refSlice as TSliceGeometryIC);
                        stDiffGeometryICLR:
                          slice := TSliceDiffGeometryICLR.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceGeometryICLR, refSlice as TSliceGeometryICLR);
                        stDiffGeometryICLR2:
                          slice := TSliceDiffGeometryICLR2.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceGeometryICLR, refSlice as TSliceGeometryICLR);
                        stDiffGeometryICLR3:
                          slice := TSliceDiffGeometryICLR3.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceGeometryICLR, refSlice as TSliceGeometryICLR);
						stDiffPOI:
                          slice := TSliceDiffPOI.Create(Self, timeStamp, currentSlice as TSlicePOI, refSlice as TSlicePOI, colorRemovedPOI, colorSamePOI, colorNewPOI);
                        stDiffPNG:
                          slice := TSliceDiffPNG.Create(Self, timeStamp, currentSlice as TSlicePNG, refSlice as TSLicePNG);
                        stDiffLocation:
                          slice := TSliceDiffLocation.Create(Self, palette.Clone, timeStamp, currentSlice as TSliceLocation, refSlice as TSliceLocation);
                      end;
                      slice.start;
                      WORMLock.EndRead;
                      try
                        AddSlice(slice);
                        Log.WriteLn('added slice ('+slice.id+') for layer '+LayerID.ToString, llNormal, 1);
                      finally
                        WORMLock.BeginRead;
                      end;
                    except
                      on E: Exception
                      do Log.WriteLn('exception in TLayer.handleDataEvent ('+layerID.ToString+') creating slice ('+FormatDateTime(SliceTimeIDFormat, timeStamp)+'): '+e.Message, llError);
                    end;
                  end
                  else
                  begin
                    if Assigned(palette) then
                    begin
                      if slice.UpdatePalette(palette.Clone)
                      then signalRefresh(timeStamp, true);
                    end;
                  end;
                end;
              // diff slices
              (icehTilerLayer shl 3) or wtVarInt:
                begin
                  _layerID := aBuffer.bb_read_int32(aCursor);
                end;
              (icehTilerCurrentSlice shl 3) or wt64Bit:
                begin
                  timeStamp := aBuffer.bb_read_double(aCursor);
                  currentSlice := FindSlice(_layerID, timeStamp);
                  if not Assigned(currentSlice)
                  then Log.WriteLn('TLayer.handleDataEvent: icehTilerCurrentSlice Could not find current slice '+_layerID.ToString+'/'+FormatDateTime(SliceTimeIDFormat, timeStamp), llWarning);
                end;
              (icehTilerRefSlice shl 3) or wt64Bit:
                begin
                  timeStamp := aBuffer.bb_read_double(aCursor);
                  refSlice := FindSlice(_layerID, timeStamp);
                  if not Assigned(refSlice)
                  then Log.WriteLn('TLayer.handleDataEvent: icehTilerCurrentSlice Could not find ref slice '+_layerID.ToString+'/'+FormatDateTime(SliceTimeIDFormat, timeStamp), llWarning);
                end;
              // POIs
              (icehTilerPOIImage shl 3) or wtLengthDelimited:
                begin
                  // load image to poiImages
                  stream := TStringStream.Create(aBuffer.bb_read_rawbytestring(aCursor));
                  try
                    poiImages.Add(FMX.Graphics.TBitmap.CreateFromStream(stream));
                  finally
                    stream.Free;
                  end;
                end;
              (icehTilerPNGExtent shl 3) or wtLengthDelimited:
                begin
                  size := aBuffer.bb_read_uint64(aCursor);
                  pngExtent.Decode(aBuffer, aCursor, aCursor+Integer(size));
                end;
              (icehTilerPNGImage shl 3) or wtLengthDelimited:
                begin
                  stream := TStringStream.Create(aBuffer.bb_read_rawbytestring(aCursor));
                  try
                    pngImage.Free;
                    pngImage := FMX.Graphics.TBitmap.CreateFromStream(stream);
                  finally
                    stream.Free;
                  end;
                end;
              (icehTilerDiscreteColorsOnStretch shl 3) or wtVarInt: // boolean
                begin
                  discreteColorsOnStretch := aBuffer.bb_read_bool(aCursor);
                end;
              (icehTilerColorRemovedPOI shl 3) or wtVarInt: // cardinal=uint32
                begin
                  colorRemovedPOI := aBuffer.bb_read_uint32(aCursor);
                end;
              (icehTilerColorSamePOI shl 3) or wtVarInt: // cardinal=uint32
                begin
                  colorSamePOI := aBuffer.bb_read_uint32(aCursor);
                end;
              (icehTilerColorNewPOI shl 3) or wtVarInt: // cardinal=uint32
                begin
                  colorNewPOI := aBuffer.bb_read_uint32(aCursor);
                end;
              (icehTilerSliceUpdate shl 3) or wtLengthDelimited:
                begin
                  if Assigned(slice)
                  then slice.AddToQueue(aBuffer.bb_read_rawbytestring(aCursor))
                  else aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
                end;
              (icehDiscretePalette shl 3) or wtLengthDelimited:
                begin
                  size := aBuffer.bb_read_uint64(aCursor);
                  palette.Free;
                  palette := TDiscretePalette.Create();
                  palette.Decode(aBuffer, aCursor, aCursor+Integer(size));
                end;
              (icehRampPalette shl 3) or wtLengthDelimited:
                begin
                  size := aBuffer.bb_read_uint64(aCursor);
                  palette.Free;
                  palette := TRampPalette.Create;
                  palette.Decode(aBuffer, aCursor, aCursor+Integer(size));
                end;
              (icehTilerRequestPreviewImage shl 3) or wtVarInt:
                begin
                  width := aBuffer.bb_read_uint32(aCursor);
                  stream := TMemoryStream.Create;
                  try
                    res := GenerateTile(0, Self.extent.SquareInMeters, stream, width, width, 'preview.png', model.fCacheFolder);
                    if res=HSC_SUCCESS_OK
                    then aEventEntry.signalEvent(TByteBuffer.bb_tag_bytes(icehTilerPreviewImage, (stream as TMemoryStream).Memory^, stream.Size))
                    else Log.WriteLn('TLayer.handleDataEvent ('+LayerID.ToString+'): could not create bitmap on icehTilerRequestPreviewImage');
                  finally
                    stream.Free;
                  end;
                end;
              (icehTilerSliceAction shl 3) or wtVarInt:
                begin
                  action := aBuffer.bb_read_uint32(aCursor);
                  case action of
                    tsaClearSlice:
                      begin
                        if Assigned(slice) then
                        begin
                          slice.fDataLock.BeginWrite;
                          try
                            slice.ClearSlice();
                          finally
                            slice.fDataLock.EndWrite;
                          end;
                          signalRefresh(timeStamp, true);
                        end;
                      end;
                    {
                    tsaSlicePointValue:
                      begin
                        if Assigned(slice)
                        then value := slice.PointValue(lat, lon)
                        else value := Double.NaN;
                        // signal back value
                        aEventEntry.signalEvent(
                          TByteBuffer.bb_tag_double(icehTilerSliceID, timeStamp)+
                          TByteBuffer.bb_tag_double(icehTilerPointValueLat, lat)+
                          TByteBuffer.bb_tag_double(icehTilerPointValueLon, lon)+
                          TByteBuffer.bb_tag_double(icehTilerValue, value)+
                          TByteBuffer.bb_tag_uint32(icehTilerSliceAction, tsaSlicePointValue)
                          );
                      end;
                    }
                  end;
                end;
              (icehTilerSlicePointValue shl 3) or wtLengthDelimited:
                begin
                  pointValueRequestID := aBuffer.bb_read_rawbytestring(aCursor);
                  if Assigned(slice)
                  then value := slice.PointValue(lat, lon)
                  else value := Double.NaN;
                  // signal back value
                  aEventEntry.signalEvent(
                    TByteBuffer.bb_tag_double(icehTilerSliceID, timeStamp)+
                    TByteBuffer.bb_tag_double(icehTilerPointValueLat, lat)+
                    TByteBuffer.bb_tag_double(icehTilerPointValueLon, lon)+
                    TByteBuffer.bb_tag_double(icehTilerValue, value)+
                    TByteBuffer.bb_tag_rawbytestring(icehTilerSlicePointValue, pointValueRequestID)
                    );
                end;
              (icehTilerID shl 3) or wtVarInt,
              (icehTilerRefresh shl 3) or wt64Bit,
              (icehTilerRefreshImmediate shl 3) or wt64Bit,
              (icehTilerPreviewImage shl 3) or wtLengthDelimited:
                begin
                  aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
                  // received refresh etc. from other layer linked to same data event -> ignore
                end;
              (icehTilerURL shl 3) or wtLengthDelimited:
                begin
                  otherTilerURL := aBuffer.bb_read_string(aCursor);
                  Log.WriteLn('Other tiler is linked to same event: '+otherTilerURL+' on '+aEventEntry.eventName, llWarning);
                end;
              (icehTilerLineThickness shl 3) or wt64Bit:
                begin
                  lineThickness := aBuffer.bb_read_double(aCursor);
                end;
              (icehTilerPointValueLat shl 3) or wt64bit:
                lat := aBuffer.bb_read_double(aCursor);
              (icehTilerPointValueLon shl 3) or wt64bit:
                lon := aBuffer.bb_read_double(aCursor);
            else
              Log.WriteLn('unknown fields in layer ('+LayerID.ToString+') data: '+fieldInfo.toString, llWarning);
              aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
            end;
          end;
        end;
      finally
        if Assigned(DataEvent)
        then WORMLock.EndRead;
      end;
    finally
      palette.Free;
      poiImages.Free;
      pngImage.Free;
    end;
  except
    on e: Exception
    do Log.WriteLn('exception in TLayer.handleDataEvent ('+layerID.ToString+'): '+e.Message, llError);
  end;
end;

procedure TLayer.Load(aStream: TStream);
begin
  // todo: implement
end;

function TLayer.PointValue(aTimeStamp: TDateTime; const aLat, aLon: Double; aThreadPool: TMyThreadPool): Double;
var
  slice: TSlice;
begin
  slice := findSlice(aTimeStamp);
  if Assigned(slice)
  then Result := slice.PointValue(aLat, aLon, aThreadPool)
  else Result := NaN;
end;

procedure TLayer.removePersistencyInfo;
begin
  StandardIni.DeleteKey(PersistentLayersSection, fLayerID.ToString);
end;

procedure TLayer.Save(aStream: TStream; aSavedSlices: TList<TSlice>);
var
  slice: TSlice;
begin
  if fPersistent then
  begin
    for slice in fSlices do
    begin
      if aSavedSlices.IndexOf(slice)<0 then
      begin
        slice.Save(aStream, aSavedSlices);
        aSavedSlices.Add(slice);
      end;
    end;
  end;
end;

procedure TLayer.signalRefresh(aTimeStamp: TDateTime; aImmediate: Boolean);
begin
  if aImmediate //set to False to revert changes
  then fDataEvent.signalEvent(TByteBuffer.bb_tag_double(icehTilerRefreshImmediate, aTimeStamp))
  else fDataEvent.signalEvent(TByteBuffer.bb_tag_double(icehTilerRefresh, aTimeStamp));
end;

procedure TLayer.signalTilerInfo;
begin
  fDataEvent.signalEvent(
    TByteBuffer.bb_tag_int32(icehTilerID, fLayerID)+
    TByteBuffer.bb_tag_string(icehTilerURL, URL));
end;

procedure TLayer.storePersistencyInfo;
begin
  StandardIni.WriteString(
    PersistentLayersSection,
    fLayerID.ToString,
    fDataEvent.eventName+'|'+Double.ToString(fMaxEdgeLengthInMeters, dotFormat)+'|'+fSliceType.toString+'|'+fDescription);
end;

function TLayer.URL: string;
begin
  Result := 'http://'+Model.URL+'/tiles?layer='+fLayerID.ToString+'&zoom={z}&x={x}&y={y}';
end;

{ TModel }

procedure TModel.ClearCache;
var
  F: TSearchRec;
  id: Integer;
begin
  if FindFirst(fCacheFolder+'*.*', faDirectory, F)=0 then
  begin
    try
      repeat
        id  := StrToIntDef(F.Name, -1);
        if (id>=0) and not fLayers.ContainsKey(id)
        then deleteDirectory(fCacheFolder+F.Name);
      until FindNext(F)<>0;
    finally
      FindClose(F);
    end;
  end;
end;

constructor TModel.Create(aMaxEdgeLengthInMeters: TDLCoordinate; aThreadCount: Integer);
begin
  globalModelAndLayersLock.BeginWrite;
  try
    inherited Create;
    fMaxEdgeLengthInMeters := aMaxEdgeLengthInMeters;
    fConnectedServices := TStringList.Create;
    fConnection := TSocketConnection.Create(
      GetStdIniSetting(ModelNameSwitch, DefaultModelName), GetStdIniSetting(ModelIDSwitch, DefaultModelID),
      GetStdIniSetting(PrefixSwitch, DefaultPrefix),
      GetStdIniSetting(RemoteHostSwitch, imbDefaultRemoteHost), GetStdIniSetting(RemotePortSwitch, imbDefaultRemoteSocketPort));

    fConnection.onDisconnect := HandleDisconnect;
    fConnection.onException := HandleException;

    fDefaultCanvasTriggerLock := TCriticalSection.Create;

    fCacheFolder := IncludeTrailingPathDelimiter(GetStdIniSetting(CacheFolderSwitch, ExtractFilePath(StandardIni.FileName)+DefaultCacheFolder));
    Log.WriteLn('cache folder '+ExpandFileName(fCacheFolder));

    fThreadPool := TMyThreadPool.Create(aThreadCount);
    fLayers := TObjectDictionary<Integer, TLayer>.Create([doOwnsValues]);

    URL := GetStdIniSetting(TilerURLSwitch, '');

    // execute actions needed to stop the model
    Log.WriteLn('Start model');
    LoadPersistentLayers;
    Log.WriteLn('Loaded persistent layers');
    // clear cache except for persistent layers
    ClearCache;
    Log.WriteLn('Cleared non-persistent cache');
    fWS2IMBEvent := fConnection.eventEntry('Sessions.WS2IMB').publish; // for gettings status from ws2imb services
    fTilerEvent := fConnection.eventEntry('Tilers.'+GetStdIniSetting(TilerEventNameSwitch, GetFQDN.Replace('.', '_'))).subscribe;
    fTilerEvent.OnEvent.Add(HandleTilerEvent);
    fTilerEvent.OnString.Add(HandleTilerStatus);
    fTilerEvent.OnIntString.Add(HandleTilerStatusRequest);
    Log.WriteLn(TilerEventNameSwitch+': '+fTilerEvent.eventName);
    fTilerEvent.signalEvent(TByteBuffer.bb_tag_double(icehTilerStartup, now));
    Log.WriteLn('Finished startup');
  finally
    globalModelAndLayersLock.EndWrite; // enable other methods
  end;
end;

destructor TModel.Destroy;
begin
  // execute actions needed to stop the model
  Log.WriteLn('Stop model');
  FreeAndNil(fThreadPool);
  fConnection.connected := False;
  globalModelAndLayersLock.BeginWrite;
  try
    SavePersistentLayers;
    FreeAndNil(fLayers);
  finally
    globalModelAndLayersLock.EndWrite;
  end;
  inherited;
  FreeAndNil(fDefaultCanvasTriggerLock);
  FreeAndNil(fConnection);
  FreeAndNil(fConnectedServices);
end;

function TModel.GenerateTile(aLayerID: Integer; aTimeStamp: TDateTime; const aExtent: TExtent; var aStream: TStream; aWidthPixels, aHeightPixels: Integer; const aFileName: string): Integer;
var
  layer: TLayer;
begin
  try
    layer := nil;
    try
      globalModelAndLayersLock.BeginRead;
      try
        if not (Assigned(fLayers) and fLayers.TryGetValue(aLayerID, layer))
        then layer := nil;
        if Assigned(layer) and Assigned(layer.DataEvent)
        then layer.WORMLock.BeginRead; // lock layer itself inside layers lock
      finally
        globalModelAndLayersLock.EndRead;
      end;
      if Assigned(layer) and Assigned(layer.DataEvent)
      then Result := layer.GenerateTile(aTimeStamp, aExtent, aStream, aWidthPixels, aHeightPixels, aFileName, fCacheFolder, fThreadPool)
      else
      begin
        Log.WriteLn('Layer id '+aLayerID.ToString+' not found to generate tile from', llWarning);
        Result := HSC_ERROR_NOT_FOUND;
      end;
    finally
      if Assigned(layer) and Assigned(layer.DataEvent)
      then layer.WORMLock.EndRead; // unlock layer itself
    end;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception generating bitmap '+aLayerID.ToString()+' '+aFileName+': '+E.Message, llError);
      Result := HSC_ERROR_CONFLICT;
    end;
  end;
end;

procedure TModel.HandleDisconnect(aConnection: TConnection);
begin
  if assigned(fThreadPool) then
  begin
    Log.WriteLn('Disconnect', llError);
    // todo: try to reconnect?
    fConnection.connected := True;
  end;
end;

procedure TModel.HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('IMB reader thread exception: '+aException.Message, llError);
end;

procedure TModel.HandleTilerEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  fieldInfo: UInt32;
  sliceType: Integer;
  eventName: string;
  maxEdgeLengthInMeters: Double;
  persistent: Boolean;
  description: string;
  newLayer: TLayer;
  newLayerID: Integer;
  ilp: TPair<Integer, TLayer>;
begin
  try
    // defaults
    eventName := '';
    maxEdgeLengthInMeters := fMaxEdgeLengthInMeters;
    persistent := false;
    description := '';
    while aCursor<aLimit do
    begin
      fieldInfo := aBuffer.bb_read_UInt32(aCursor);
      case fieldInfo of
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
              globalModelAndLayersLock.BeginWrite;
              try
                if Assigned(fLayers) then
                begin
                  newLayer := nil;
                  for ilp in fLayers do
                  begin
                    if ilp.Value.fDataEvent.eventName.ToLower=eventName.tolower then
                    begin
                      // found existing layer with same event name -> use that after clearing..?
                      newLayer := ilp.Value;
                      Break;
                    end;
                  end;
                  if Assigned(newLayer) then
                  begin
                    // reuse layer, but check slice type
                    if newLayer.SliceType<>sliceType then
                    begin
                      Log.WriteLn(
                        'Layer '+eventName+' ('+newLayer.fLayerID.ToString+') has different slice type '+
                        sliceType.ToString+' <> '+newLayer.SliceType.ToString, llError);
                    end;
                    newLayer.Clear(maxEdgeLengthInMeters, description, persistent);
                    newLayer.signalTilerInfo;
                    Log.WriteLn('re-used existing layer '+description+' ('+newLayer.LayerID.ToString+') for '+eventName);
                  end
                  else
                  begin
                    // try to get id from cache
                    newLayerID := StandardIni.ReadInteger(LayerIDsSection, eventName, 1);
                    // make sure it is unique
                    while fLayers.ContainsKey(newLayerID)
                    do newLayerID := newLayerID+1;
                    // put in cache
                    StandardIni.WriteInteger(LayerIDsSection, eventName, newLayerID);
                    // create layer
                    newLayer := TLayer.Create(Self, newLayerID, maxEdgeLengthInMeters, sliceType, description, persistent, eventName);
                    fLayers.Add(newLayer.fLayerID, newLayer);
                    if persistent
                    then newLayer.storePersistencyInfo;
                      // signal layer id etc. to trigger sending data
                    newLayer.signalTilerInfo;
                    Log.WriteLn('added new layer '+description+' ('+newLayer.LayerID.ToString+') for '+eventName);
                  end;
                  Log.WriteLn(newLayer.URL, llNormal, 1);
                end;
                // else shuting down?
              finally
                globalModelAndLayersLock.EndWrite;
              end;
            end
            else Log.WriteLn('TModel.HandleTilerEvent: new layer without event name sepcified', llError);
          end;
      else
        aBuffer.bb_read_skip(aCursor, fieldInfo and 7);
      end;
    end;
  except
    on e: Exception
    do log.WriteLn('Exception in TModel.HandleTilerEvent: '+e.Message, llError);
  end;
end;

procedure TModel.HandleTilerStatus(aEventEntry: TEventEntry; const aString: string);
var
  statusElements: TArray<string>;
  se: Integer;
  element: TArray<string>;
  id: string;
  status: string;
  info: string;
begin
  try
    // process status of service {"id":"<id>","status":"<status>"[,"info":"<extra info>"}
    id := 'unknown';
    status := 'unknown';
    info := '';
    statusElements := aString.Trim(['{','}']).Split([',']);
    for se := 0 to length(statusElements)-1 do
    begin
      element := statusElements[se].Split(['":"']); // removes " also (one side)
      if element[0]='"id'
      then id := element[1].TrimRight(['"'])
      else if element[0]='"status'
      then status := element[1].TrimRight(['"'])
      else if element[0]='"info'
      then info := ', '+element[1].TrimRight(['"']);
    end;
    fConnectedServices.Add(id+': '+status+info);
  except
    on e: Exception
    do log.WriteLn('Exception in TModel.HandleTilerStatus: '+e.Message, llError);
  end;
end;

procedure TModel.HandleTilerStatusRequest(aEventEntry: TEventEntry; aInt: Integer; const aString: string);
var
  e: TEventEntry;
  status: string;
begin
  case aInt of
    actionStatus:
      begin
        if aString<>''
        then e := aEventEntry.connection.eventEntry(aString, False).publish
        else e := aEventEntry;
        try
          if e.connection.connected
          then status := 'connected'
          else status := 'NOT connected';
          e.signalString('{"id":"'+aEventEntry.connection.ModelName+'","status":"'+status+'"}');
        finally
          if aString<>''
          then e.unPublish();
        end;
      end;
  end;
end;

procedure TModel.LoadPersistentLayers;
var
  layerIDs: TStringList;
  id: string;
  persistencyInfo: TArray<string>;
  layer: TLayer;
  stream: TStream;
begin
  layerIDs := TStringList.Create;
  try
    StandardIni.ReadSection(PersistentLayersSection, layerIDs);
    if layerIDs.Count>0 then
    begin
      try
        stream := TFileStream.Create(PersistentLayersDataFileName, fmOpenRead);
        try
          globalModelAndLayersLock.BeginWrite;
          try
            for id in layerIDs do
            begin
              persistencyInfo := StandardIni.ReadString(PersistentLayersSection, id, '').Split(['|']);
              if length(persistencyInfo)>3 {0..3} then
              begin
                layer := TLayer.Create(
                  Self,
                  id.ToInteger,
                  Double.Parse(persistencyInfo[1], dotFormat),
                  persistencyInfo[2].ToInteger,
                  persistencyInfo[3],
                  True,
                  persistencyInfo[0]);
                fLayers.Add(layer.fLayerID, layer);
                layer.Load(stream);
              end;
            end;
          finally
            globalModelAndLayersLock.EndWrite;
          end;
        finally
          stream.Free;
        end;
      except
        on e: Exception
        do Log.WriteLn('Exception opening persistent layer data file: '+PersistentLayersDataFileName, llError);
      end;
    end;
  finally
    layerIDs.Free;
  end;
end;

function TModel.PointValue(aLayerID: Integer; aTimeStamp: TDateTime; aLat, aLon: Double): Double;
var
  layer: TLayer;
begin
  layer := nil;
  try
    globalModelAndLayersLock.BeginRead;
    try
      if not (Assigned(fLayers) and fLayers.TryGetValue(aLayerID, layer))
      then layer := nil;
      if Assigned(layer)
      then layer.WORMLock.BeginRead; // lock layer itself inside layers lock
    finally
      globalModelAndLayersLock.EndRead;
    end;
    if Assigned(layer) and Assigned(layer.DataEvent)
    then Result := layer.PointValue(aTimeStamp, aLat, aLon, fThreadPool)
    else Result := NaN;
  finally
    if Assigned(layer) and Assigned(layer.DataEvent)
    then layer.WORMLock.EndRead; // unlock layer itself
  end;
end;

procedure TModel.RequestCSStatus;
begin
  fTilerEvent.signalIntString(actionStatus, ''); // respond on same event
  fWS2IMBEvent.signalIntString(actionStatus, fTilerEvent.eventName); // respond on tiler event
end;

procedure TModel.SavePersistentLayers;
var
  savedSlices: TList<TSlice>;
  stream: TStream;
  ilp: TPair<Integer, TLayer>;
begin
  savedSlices := TList<TSlice>.Create;
  try
    stream := nil;
    try
      for ilp in fLayers do
      begin
        if ilp.Value.fPersistent then
        begin
          if not Assigned(stream)
          then stream := TFileStream.Create(PersistentLayersDataFileName, fmCreate);
          ilp.Value.Save(stream, savedSlices);
        end;
      end;
    finally
      stream.Free;
    end;
  finally
    savedSlices.Free;
  end;
end;

procedure TModel.setURL(const aValue: string);
begin
  if not SameText(fURL, aValue) then
  begin
    Log.WriteLn('Set tiler URL '+aValue);
    StandardIni.WriteString(SettingsSection, TilerURLSwitch, aValue);
    fURL := aValue;
  end;
end;

{ TTilerWebModule }

procedure TTilerWebModule.WebModuleDefaultHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  html: string;
  ilp: TPair<integer, TLayer>;
  extent: TExtent;
  tx: Integer;
  ty: Integer;
  tzoom: Integer;
  exampleURL: string;
begin
  html :=
    '<html>' +
    '<head><title>Tiler</title></head>' +
    '<body>TilerWebService<br/><br/>';
  html := html+
   '<div>FQDN: '+GetFQDN+'</div>'+
   '<div>tiler event: '+model.TilerEvent.eventName+'</div>'+
   '<br/><br/>';
  // handle request on model
  globalModelAndLayersLock.BeginRead;
  try
    if Assigned(model.fLayers) then
    begin
      html := html+'<div>number of layers: '+model.fLayers.Count.ToString()+'</div>'+'<br/><br/>';
      if model.fLayers.Count>0 then
      begin
        for ilp in model.fLayers do
        begin
          extent := ilp.Value.extent;
          if not extent.IsEmpty then
          begin
            ExtentToXYZoom(extent, tx, ty, tzoom);
            // use tile that fits over max extent
            exampleURL := ilp.Value.URL.Replace('{z}', tzoom.ToString).Replace('{x}', tx.ToString).Replace('{y}', ty.ToString);
            // use helper html file that has to reside in root on IIS server
            {
            exampleURL :=
              '/tilerhelper.html?'+
                'lat='+extent.Center.Y.ToString(dotFormat)+'&'+
                'lon='+extent.Center.X.ToString(dotFormat)+'&'+
                'zoom='+(tzoom+1).ToString+'&'+
                'layerURL='+ilp.Value.URL; // http://vps17642.public.cloudvps.com/tilerus/TilerWebService.dll/tiles?layer=46
            }
            html := html+
              '<div>'+
                'layer: '+ilp.Key.ToString+' ('+ilp.Value.MaxEdgeLengthInMeters.ToString+'), '+
                'slices ('+ilp.Value.SliceType.ToString+'): '+ilp.Value.Slices.Count.ToString+', '+
                'range: '+ilp.Value.dateTimeRange+', '+
                'event: '+ilp.Value.fDataEvent.eventName+', '+
                '<a href="'+exampleURL+'">'+ilp.Value.fDescription+'</a>'+
              '</div>';
          end
          else
          begin
            html := html+
              '<div>'+
                'layer: '+ilp.Key.ToString+' ('+ilp.Value.MaxEdgeLengthInMeters.ToString+', '+
                'slice type: '+ilp.Value.SliceType.ToString+', '+
                'slices: '+ilp.Value.Slices.Count.ToString+', '+
                'range: '+ilp.Value.dateTimeRange+', '+
                'event: '+ilp.Value.fDataEvent.eventName+', '+
                ilp.Value.fDescription+
              '</div>';
          end;
        end;
      end
      else html := html+'<div>no layers defined</div';
    end
    else html := html+'<div>layers not created (yet)</div';
  finally
    globalModelAndLayersLock.EndRead;
  end;
  html := html+
    '</body>' +
    '</html>';
  Response.Content := html;
end;

procedure TTilerWebModule.TilerWebModuleRequestStatusAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  html: string;
  imbStatus: string;
  s: string;
begin
  model.fConnectedServices.Clear;
  model.RequestCSStatus;

  // show status of connected services and tiler it self..
  if model.Connection.connected
  then imbStatus := 'connected'
  else imbStatus := 'NOT connected';

  sleep(2000); // wait for status reponses to come in

  html :=
    '<html>' +
    '<head><title>Tiler status</title></head>' +
    '<body>'+
      '<h2>Tiler web service @ '+getFQDN+'</h2>'+
		  '<ul>'+
		    '<li>Status: running</li>'+
		    '<li>IMB connection: '+imbStatus+'</li>'+
        '<li>Layers: '+model.fLayers.Count.ToString()+'</li>'+
		  '</ul>'+
		  '<h3>Connected services</h3>'+
		  '<ul>';

  for s in model.fConnectedServices
  do html := html+'<li>'+s+'</li>';

  html := html+
		  '</ul>'+
      '<small>@ '+FormatDateTime('yyyy-mm-dd hh:nn', Now)+'</small>'+
    '</body>'+
    '</html>';
  Response.Content := html;
  log.WriteLn('handled TTilerWebModule.TilerWebModuleRequestStatusAction');
end;

procedure TTilerWebModule.WebModuleCreate(Sender: TObject);
var
  c: TCanvasClass;
begin
  try
    // trigger creation of canvas classes to avoid multi-threading issues later on
    model.defaultCanvasTriggerLock.Acquire;
    try
      c := TCanvasManager.DefaultCanvas;
      if not Assigned(c)
      then Log.WriteLn('Could not create default canvas', llError);
    finally
      model.defaultCanvasTriggerLock.Release;
    end;
    Log.WriteLn('Created web module');
  except
    on E: Exception
    do Log.WriteLn('Exception in TTilerWebModule.WebModuleCreate, could not create default canvas: '+E.Message, llError);
  end;
end;

procedure TTilerWebModule.WebModuleDestroy(Sender: TObject);
begin
  Log.WriteLn('Destroyed web module');
end;

procedure TTilerWebModule.WebModuleException(Sender: TObject; E: Exception; var Handled: Boolean);
begin
  Log.WriteLn('Tiler web module exception: '+E.Message, llError);
end;

procedure TTilerWebModule.WebModuleTilerRequestPointValueAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  layerID: Integer;
  lat: Double;
  lon: Double;
  value: Double;
  time: string;
  timeStamp: TDateTime;
begin
  try
    // decode parameters
    layerID := Request.QueryFields.Values[rpLayerID].ToInteger();
    lat := StrToFloat(Request.QueryFields.Values[rpLat], dotFormat);
    lon := StrToFloat(Request.QueryFields.Values[rpLon], dotFormat);
    time := Request.QueryFields.Values[rpTime]; // yyyymmddhhmmss or empty
    if time<>''
    then timeStamp := EncodeDateTime(
                     time.Substring(0,4).toInteger, time.Substring(4,2).toInteger, time.Substring(6,2).toInteger,
                     time.Substring(8,2).toInteger, time.Substring(10,2).toInteger, time.Substring(12,2).toInteger, 0)
    else timeStamp := 0;

    // handle request on model
    value := model.PointValue(layerID, timeStamp, lat, lon);
    Response.ContentType := 'application/json';
    Response.Content := '{ "value": '+DoubleToJSON(value)+' }';
    Handled := True;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception in WebModuleTilerRequestPointValueAction '+layerID.ToString+' '+lat.ToString()+' '+lon.ToString+': '+E.Message, llError);
      Response.StatusCode := HSC_ERROR_BADREQUEST;
      Response.ContentType := 'text/plain';
      Response.Content := 'Exception during getting value on point '+lat.ToString()+' '+lon.ToString+' of layer '+layerID.ToString+' : '+E.Message;
    end;
  end;
end;

procedure TTilerWebModule.WebModuleTilerRequestTileAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  extent: TExtent;
  layerID: Integer;
  zoomFactor: Integer;
  tileX, tiley: Integer;
  fileName: string;
  stream: TStream;
  time: string;
  timeStamp: TDateTime;
begin
  try
    // decode parameters
    timeStamp := 0;
    try
      layerID := Request.QueryFields.Values[rpLayerID].ToInteger();
      zoomFactor := Request.QueryFields.Values[rpZoomFactor].ToInteger();
      tileX := Request.QueryFields.Values[rpTileX].ToInteger();
      tiley := Request.QueryFields.Values[rpTileY].ToInteger();
      time := Request.QueryFields.Values[rpTime]; // yyyymmddhhmmss or empty

      extent := ZoomXYToExtent(zoomfactor, tileX, tileY);
      fileName := layerID.ToString+'_'+zoomFactor.ToString+'_'+tileX.ToString+'_'+tileY.ToString+'.png';

      if time<>''
      then timeStamp := EncodeDateTime(
                       time.Substring(0,4).toInteger, time.Substring(4,2).toInteger, time.Substring(6,2).toInteger,
                       time.Substring(8,2).toInteger, time.Substring(10,2).toInteger, time.Substring(12,2).toInteger, 0)
      else timeStamp := 0;
    except
      on E: Exception
      do Log.WriteLn('Exception in WebModuleTilerRequestTileAction, decoding parameters: '+E.Message, llError);
    end;

    // handle request on model
    stream := TMemoryStream.Create;
    try
      Response.StatusCode := model.GenerateTile(layerID, timeStamp, extent, stream, TileSizeX, TileSizeY, fileName);
      if Response.StatusCode=HSC_SUCCESS_OK then
      begin
        try
          stream.Position := 0;
          Response.ContentType := 'image/png';
          Response.ContentStream :=stream;
          Response.SetCustomHeader('Access-Control-Allow-Origin', '*');
          stream := nil; // now owned by repsonse
        except
          on E: Exception
          do Log.WriteLn('Exception in WebModuleTilerRequestTileAction, returning image: '+E.Message, llError);
        end;
      end;
    finally
      stream.Free;
    end;
    Handled := True;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception in WebModuleTilerRequestTileAction '+zoomFactor.ToString()+': '+tileX.ToString+' x '+tileY.ToString+': '+E.Message, llError);
      Response.StatusCode := HSC_ERROR_BADREQUEST;
      Response.ContentType := 'text/plain';
      Response.Content := 'Exception during generation of tile '+zoomFactor.ToString()+': '+tileX.ToString+' x '+tileY.ToString+': '+E.Message;
    end;
  end;
end;

var
  maxEdgeLengthInMeters: TDLCoordinate;
  threadCount: Integer;
initialization
  globalModelAndLayersLock.Create;
  canvasLock := TObject.Create;

  FileLogger.MakeModuleFileName;
  FileLogger.SetLogDef(AllLogLevels, [llsTime, llsThreadID]);
  Log.Start();

  Log.WriteLn('Ini file: '+StandardIni.FileName);

  tilesCache := TObjectDictionary<string, TTileCacheEntry>.Create([doOwnsValues]);
  monitorTilesCacheWaitEvent := TEvent.Create();
  monitorTilesCacheThread := TThread.CreateAnonymousThread(MonitorTilesCache);
  monitorTilesCacheThread.NameThreadForDebugging('monitorTilesCacheThread');
  monitorTilesCacheThread.Start();

  maxEdgeLengthInMeters := GetStdIniSetting(MaxEdgeLengthSwitch, DefaultMaxEdgeLength);
  threadCount := GetStdIniSetting(ThreadCountSwitch, DefaultThreadCount);
  Log.WriteLn('creating tiler process with '+threadCount.ToString+' threads and max edge length (m) of '+maxEdgeLengthInMeters.ToString(dotFormat));
  model := TModel.Create(maxEdgeLengthInMeters, threadCount);
finalization
  FreeAndNil(model);
  monitorTilesCacheThread.Terminate; // mark as terminated
  monitorTilesCacheWaitEvent.SetEvent(); // no longer wait
  FreeAndNil(monitorTilesCacheThread);
  FreeAndNil(monitorTilesCacheWaitEvent);
  FreeAndNil(tilesCache);
  FreeAndNil(canvasLock);
  Log.Finish();
end.

