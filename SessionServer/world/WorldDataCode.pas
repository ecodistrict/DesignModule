unit WorldDataCode;

// point in polygon functions: http://geomalgorithms.com/a03-_inclusion.html

interface

uses
  imb4,
  System.SysConst,
  System.Math, System.RTTI, System.TypInfo, System.SysUtils,
  System.Generics.Collections;

type
  TWDKey = UInt32;

const
  wdatNoObjectID =                     TWDKey((icehNoObjectID shl 3) or Ord(wtLengthDelimited)); // TWDID
  wdatObjectID =                       TWDKey((icehObjectID shl 3) or Ord(wtLengthDelimited)); // TWDID, external object id
    wdatNoAttribute =                  TWDKey((icehNoAttribute shl 3) or Ord(wtVarInt)); // uint32, key

// icehWorldCommandBase
  wdatObjectsInquire =                 TWDKey(((icehWorldCommandBase+0) shl 3) or Ord(wtLengthDelimited)); // string, filter
    wdatReturnEventName =              TWDKey(((icehWorldCommandBase+1) shl 3) or Ord(wtLengthDelimited)); // string

  wdatSchemaInquire =                  TWDKey(((icehWorldCommandBase+2) shl 3) or wtLengthDelimited); // string, return event name
    wdatSchemaSource =                 TWDKey(((icehWorldCommandBase+3)  shl 3) or wtLengthDelimited); // string
    wdatSchemaCollection =             TWDKey(((icehWorldCommandBase+4) shl 3) or wtLengthDelimited); // string, name of schema ie collection
    wdatSchemaAttributes =             TWDKey(((icehWorldCommandBase+5)  shl 3) or wtLengthDelimited); // blob of pb tagged values

  wdatSchemaAttributeName =            TWDKey(((icehWorldCommandBase+6) shl 3) or wtLengthDelimited); // string
    wdatSchemaAttributeWorldType =     TWDKey(((icehWorldCommandBase+7) shl 3) or wtVarInt); // int32
    wdatSchemaAttributeFunction =      TWDKey(((icehWorldCommandBase+8) shl 3) or wtVarInt); // uint32
    wdatSchemaAttributeKey =           TWDKey(((icehWorldCommandBase+9) shl 3) or wtVarInt); // uint32, key, trigger to check schema entry

  //wdatSchemaAttributeAdd =             TWDKey(((icehWorldCommandBase+10) shl 3) or wtVarInt); // uint32, key, trigger to add entry to schema
    // prefixed by:
    //   wdatSchemaCollection
    //   wdatSchemaAttributeName
    //   wdatSchemaAttributeWorldType

  wdatDataSourcesInquire =             TWDKey(((icehWorldCommandBase+11) shl 3) or wtLengthDelimited); // string, return event name

  wdatDataSource =                     TWDKey(((icehWorldCommandBase+12) shl 3) or wtLengthDelimited); // string, private event name of data source
  wdatNoDataSource =                   TWDKey(((icehWorldCommandBase+13) shl 3) or wtLengthDelimited); // string, private event name of data source

  wdatBinsInquire =                    TWDKey(((icehWorldCommandBase+14) shl 3) or wtLengthDelimited); // string, return event name

    wdatBinDescription =               TWDKey(((icehWorldCommandBase+15) shl 3) or wtLengthDelimited); // string
    wdatBinParent =                    TWDKey(((icehWorldCommandBase+16) shl 3) or wtLengthDelimited); // TWDID
    wdatBinRef =                       TWDKey(((icehWorldCommandBase+17) shl 3) or wtLengthDelimited); // TWDID
    wdatBinState =                     TWDKey(((icehWorldCommandBase+18) shl 3) or wtVarInt); // uint32

  wdatBinLoad =                        TWDKey(((icehWorldCommandBase+19) shl 3) or wtLengthDelimited); // TWDID
  wdatBinLoaded =                      TWDKey(((icehWorldCommandBase+20) shl 3) or wtLengthDelimited); // TWDID
  wdatBinUnLoad =                      TWDKey(((icehWorldCommandBase+21) shl 3) or wtLengthDelimited); // TWDID
  wdatBinUnLoaded =                    TWDKey(((icehWorldCommandBase+22) shl 3) or wtLengthDelimited); // TWDID

  wdatBinCreate =                      TWDKey(((icehWorldCommandBase+23) shl 3) or wtLengthDelimited); // TWDID
  wdatBinCreated =                     TWDKey(((icehWorldCommandBase+24) shl 3) or wtLengthDelimited); // TWDID

  wdatBinCopy =                        TWDKey(((icehWorldCommandBase+25) shl 3) or wtLengthDelimited); // TWDID
    wdatBinSourceID =                  TWDKey(((icehWorldCommandBase+26) shl 3) or wtLengthDelimited); // TWDID

  wdatBinDelete =                      TWDKey(((icehWorldCommandBase+27) shl 3) or wtLengthDelimited); // TWDID
  wdatBinDeleted =                     TWDKey(((icehWorldCommandBase+28) shl 3) or wtLengthDelimited); // TWDID

  wdatCollectionAdd =                  TWDKey(((icehWorldCommandBase+29) shl 3) or wtLengthDelimited); // string, name
  wdatCollectionTruncate =             TWDKey(((icehWorldCommandBase+30) shl 3) or wtLengthDelimited); // string, name


  // palette
  icehDiscretePalette =                icehWorldCommandBase+51; // wtLengthDelimited, discrete palette
  icehRampPalette =                    icehWorldCommandBase+52; // wtLengthDelimited, ramp palette



type
  TWDWorldType = Integer;

const
  // TWDWorldType
  wdkGeometry = 1;
  wdkGeometryPart = 2;
  wdkGeometryPoint = 3;
  wdkGridRow = 4;
  wdkPaletteDiscrete = 5;
  wdkPaletteRamp = 6;




type
  TWDValue = TByteBuffer;

  TWDEncodedAttributes = TDictionary<TWDKey, TWDValue>;

  TWDID = TWDValue; // id used externally, mostly contains a guid for uniqueness;

  TWDBinState = (
    wbsClosed,
    wbsOpen
  );

  TWDBinStateHelper = record helper for TWDBinState
    function ToString: string;
  end;

  TWDBinLoadState = (
    wblsUnloaded,
    wblsLoading,
    wblsLoaded
  );

  TWDBinLoadStateHelper = record helper for TWDBinLoadState
    function ToString: string;
  end;

  TWDClass = class
  //class function CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass; virtual; abstract;
  class function WorldType: TWDWorldType; virtual; abstract;
  public
    function Encode: TWDValue; virtual; abstract;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); virtual; abstract;
  end;

  TWDGeometryBase = class(TWDClass)
  private
    function getGeoJSON2D(const aType: string): string; virtual; abstract;
  public
    property GeoJSON2D[const aType: string]: string read getGeoJSON2D;
  end;

  TWDGeometryPoint = class(TWDGeometryBase)
  constructor Create; overload;
  constructor Create(ax,ay,az: Double); overload;
  //class function CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass; override;
  class function WorldType: TWDWorldType; override;
  private
    function getGeoJSON2D(const aType: string): string; override;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  public
    x: Double;
    y: Double;
    z: Double;
  end;
  {
  TWDGeometryExtent  = class(TWDClass)
  constructor Create; overload;
  constructor Create(axMin,ayMin,azMin, axMax,ayMax,azMax: Double); overload;
  class function WorldType: TWDWorldType; override;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  public
    // todo: 2 points or specific coordinates
    xMin: Double;
    yMin: Double;
    zMin: Double;
    xMax: Double;
    yMax: Double;
    zMax: Double;
  end;
  }
  TWDGeometryPart = class(TWDClass)
  constructor Create;
  destructor Destroy; override;
  //class function CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass; override;
  class function WorldType: TWDWorldType; override;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  private
    fPoints: TObjectList<TWDGeometryPoint>;
    function getGeoJSON2D: string;
  public
    property points: TObjectList<TWDGeometryPoint> read fPoints;
    property GeoJSON2D: string read getGeoJSON2D;
    procedure AddPoint(x, y, z: Double);
    // point in polygon functions
    function cn_PnPoly(x, y: Double): Boolean;
    function wn_PnPoly(x, y: Double): Integer;
  end;

  TWDGeometry = class(TWDGeometryBase)
  constructor Create;
  constructor CreateFromSVGPath(const aSVGPath: string);
  destructor Destroy; override;
  //class function CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass; override;
  class function WorldType: TWDWorldType; override;
  private
    function getGeoJSON2D(const aType: string): string; override;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  private
    fParts: TObjectList<TWDGeometryPart>;
  public
    property parts: TObjectList<TWDGeometryPart> read fParts;

    function AddPart: TWDGeometryPart;
    procedure AddPoint(x, y, z: Double); overload;
    procedure AddPoint(aPoint: TWDGeometryPoint); overload; // ref only!

    function PointInFirstPart(x, y: Double): Boolean;
    function PointInAnyPart(x, y: Double): Boolean;
  end;

  TAlphaRGBPixel = Cardinal; // 32 bit ARGB color

  TWDPalette = class(TWDClass)
  constructor Create(const aDescription: string); overload;
  function Clone: TWDPalette;
  class function wdTag: UInt32; virtual; abstract;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  protected
    fDescription: string;
    function _clone: TWDPalette; virtual; abstract;
  public
    function ValueToColor(const aValue: Double): TAlphaRGBPixel; virtual; abstract;
    function ValueToColorJSON(const aValue: Double): string;
  end;

const
  // TWDFunction, world function value
  wdfConsumerOptional = $01;
  wdfConsumer = $02;
  wdfProducerOptional = $04;
  wdfProducer = $08;
  wdfStored = $40;

type
  TWDFunction = UInt32; // single function, see wdf.. above
  TWDFunctions = UInt32; // or-ed together TWDFunction values

  // RTTI (de)coding custom
  TWorldKey = class(TCustomAttribute)
  constructor Create(aKey: TWDKey);
  constructor CreateWithWiretype(aTag: TWDKey; aWireType: TWDKey);
  private
    fKey: TWDKey;
  public
    property wKey: TWDKey read fKey;
  end;

  TWorldFunctions = class(TCustomAttribute)
  constructor Create(aFunctions: TWDFunctions);
  private
    sFunctions: TWDFunctions;
  public
    property wFunctions: TWDFunctions read sFunctions;
  end;

  TWorldType = class(TCustomAttribute)
  constructor Create(aWorldType: TWDWorldType);
  private
    fType: TWDWorldType;
  public
    property wType: TWDWorldType read fType;
  end;

  TWorldName = class(TCustomAttribute)
  constructor Create(const aWorldName: string);
  private
    fName: string;
  public
    property wName: string read fName;
  end;

  TWDClassCacheField = class
  constructor Create(aRttiField: TRttiField; const aName: string; aKey: TWDKey; aFunctions: TWDFunctions; aType: TWDWorldType; const aSource: TWDID);
  destructor Destroy; override;
  private
    fRttiField: TRttiField;
    fName: string;
    fKey: TWDKey;
    fFunctions: TWDFunctions;
    fType: TWDWorldType;
    fDominantSource: TWDID;
  public
    property RttiField: TRttiField read fRttiField;
    property Name: string read fName;
    property Key: TWDKey read fKey write fKey;
    property Functions: TWDFunctions read fFunctions;
    property WType: TWDWorldType read fType;
    property DominantSource: TWDID read fDominantSource;

    function Update(aKey: TWDKey; aFunctions: TWDFunctions; const aSource: TWDID): Boolean;
  end;

  TWDClassCache = class
  //constructor Create(aObject: TObject; const aSource: TWDID); overload;
  constructor Create(aClass: TClass; const aSource: TWDID); overload;
  destructor Destroy; override;
  private
    fRttiContext: TRttiContext;
    fClass: TClass;
    fAttributesOnName: TObjectDictionary<string, TWDClassCacheField>;
    fAttributesOnKey: TObjectDictionary<TWDKey, TWDClassCacheField>;
  public
    function NextFreeKey(aKey: TWDKey): TWDKey;
  public
    class function getAttributeWireType(aRttiField: TRttiField): TWDKey;

    function getAttribute(aObject: TObject; aKey: TWDKey; aRttiField: TRttiField): TWDValue;
    function EncodeAttributes(aObject: TObject): TWDValue; overload;
    procedure EncodeAttributes(aObject: TObject; aEncodedAttributes: TWDEncodedAttributes); overload;
    function EncodeAttributesWithoutObjectID(aObject: TObject): TWDValue;
    function EncodeAttributesFiltered(aObject: TObject; aAttributes: TArray<TWDKey>): TWDValue;
    function setAttribute(aObject: TObject; aKey: TWDKey; const aValue: TWDValue; var aCursor: Integer): Boolean;
    procedure DecodeAttributes(aObject: TObject; const aEncodedAttributes: TWDValue); overload;
    procedure DecodeAttributes(aObject: TObject; aEncodedAttributes: TWDEncodedAttributes); overload;

    property attributesOnName: TObjectDictionary<string, TWDClassCacheField> read fAttributesOnName;
    property attributesOnKey: TObjectDictionary<TWDKey, TWDClassCacheField> read fAttributesOnKey;
  end;


function CompareHighestFunction(aFunctions1, aFunctions2: TWDFunctions): Integer;

function NewGUIDAsWDID: TWDID;

var
  dotFormat: TFormatSettings;

function ColorToJSON(aColor: TAlphaRGBPixel): string;


implementation


function CompareHighestFunction(aFunctions1, aFunctions2: TWDFunctions): Integer;
// >0: aFunctions1 > aFunctions2
// =0: aFunctions1 = aFunctions2
// <0: aFunctions1 < aFunctions2
begin
  // strip to highest bit
  while (aFunctions1<>0) and (aFunctions2<>0) do
  begin
    aFunctions1 := aFunctions1 shr 1;
    aFunctions2 := aFunctions2 shr 1;
  end;
  Result := aFunctions1-aFunctions2;
end;

function NewGUIDAsWDID: TWDID;
var
  g: TGUID;
begin
  g := TGUID.NewGuid;
  SetLength(Result, SizeOf(g));
  Move(g, PAnsiChar(Result)^, SizeOf(g));
end;

function ColorToJSON(aColor: TAlphaRGBPixel): string;
begin
  Result := '#'+(aColor and $FFFFFF).ToHexString(6);
end;

{ TWDBinStateHelper }

function TWDBinStateHelper.ToString: string;
begin
  case Self of
    wbsClosed:
      Result := 'open';
    wbsOpen:
      Result := 'open';
  else
    Result := '## unknown';
  end;
end;

{ TWDBinLoadStateHelper }

function TWDBinLoadStateHelper.ToString: string;
begin
  case Self of
    wblsUnloaded:
      Result := 'unloaded';
    wblsLoading:
      Result := 'loading';
    wblsLoaded:
      Result := 'loaded';
  else
    Result := '## unknown';
  end;
end;


{ TWorldKey }

// http://docwiki.embarcadero.com/RADStudio/XE5/en/Extracting_Attributes_at_Run_Time

constructor TWorldKey.Create(aKey: TWDKey);
begin
  inherited Create;
  fKey := aKey;
end;

constructor TWorldKey.CreateWithWiretype(aTag, aWireType: TWDKey);
begin
  Create((aTag shl 3) or aWireType);
end;

{ TWorldFunction }

constructor TWorldFunctions.Create(aFunctions: TWDFunctions);
begin
  inherited Create;
  sFunctions := aFunctions;
end;

{ TWorldType }

constructor TWorldType.Create(aWorldType: TWDWorldType);
begin
  inherited Create;
  fType := aWorldType;
end;

{ TWorldName }

constructor TWorldName.Create(const aWorldName: string);
begin
  inherited Create;
  fName := aWorldName;
end;

{ TWDClassCacheField }

constructor TWDClassCacheField.Create(aRttiField: TRttiField; const aName: string; aKey: TWDKey; aFunctions: TWDFunctions; aType: TWDWorldType; const aSource: TWDID);
begin
  inherited Create;
  fRttiField := aRttiField; // ref only
  fName := aName;
  fKey := aKey;
  fFunctions := aFunctions;
  fType := aType;
  fDominantSource := aSource;
end;

destructor TWDClassCacheField.Destroy;
begin
  fRttiField := nil; // remove ref
  inherited;
end;

function TWDClassCacheField.Update(aKey: TWDKey; aFunctions: TWDFunctions; const aSource: TWDID): Boolean;
begin
  Result := False; // sentinel
  // check key for change
  if fKey<>aKey then
  begin
    fKey := aKey;
    Result := True;
  end;
  // check change in functions and for more dominant function
  if (fFunctions or aFunctions) <> fFunctions then
  begin
    if (aFunctions>fFunctions) and (aSource<>'')
    then fDominantSource := aSource;
    fFunctions := fFunctions or aFunctions;
    Result := True;
  end;
end;

{ TWDClassCacheEntry }

constructor TWDClassCache.Create(aClass: TClass; const aSource: TWDID);
var
  tt: TRttiType;
  ti: PTypeInfo;
  fields: TArray<TRttiField>;
  f: TRttiField;
  attributes: TArray<TCustomAttribute>;
  a: TCustomAttribute;
  wKey, nextKey: TWDKey;
  wFunctions: UInt32;
  wType: TWDWorldType;
  wName: string;
  ccf: TWDClassCacheField;
begin
  inherited Create;
  fClass := aClass;
  fAttributesOnName := TObjectDictionary<string, TWDClassCacheField>.Create([doOwnsValues]);
  fAttributesOnKey := TObjectDictionary<TWDKey, TWDClassCacheField>.Create([]);
  fRttiContext := TRttiContext.Create;
  // find attributes via RTTI with TProtoBufKey defined
  nextKey := icehAttributeBase shl 3;
  ti := aClass.ClassInfo;
  tt := fRttiContext.GetType(ti); // todo: if we re-create the RTTI context every time do we lose the link to the stored fields?
  fields := tt.GetFields;
  for f in fields do
  begin
    // defaults
    wKey := 0;
    wFunctions := 0;
    wType := -1;
    wName := '';
    // decode custom attributes
    attributes := f.GetAttributes;
    for a in attributes do
    begin
      if a.ClassType=TWorldKey
      then wKey := (a as TWorldKey).wKey
      else if a.ClassType=TWorldFunctions
      then wFunctions := wFunctions or (a as TWorldFunctions).wFunctions
      else if a.ClassType=TWorldType
      then wType := (a as TWorldType).wType
      else if a.ClassType=TWorldName
      then wName := (a as TWorldName).wName;
    end;
    // detect type
    if wType=-1 then
    begin
      if (f.FieldType.TypeKind=tkClass) and f.FieldType.ClassType.InheritsFrom(TWDClass)
      then wType := TWDClass(f.FieldType.ClassType).WorldType
      else wType := 0;
      // .. extend non-classes here
    end;
    // build key if not defined
    if (wKey=0)
    then wKey := nextKey or TWDClassCache.getAttributeWireType(f);
    // make key unique
    while fAttributesOnKey.ContainsKey(wKey)
    do wKey := wKey+(1 shl 3);
    // default or specific name
    if wName=''
    then wName := f.Name;
    // add attribute
    ccf := TWDClassCacheField.Create(f,  wName, wKey, wFunctions,  wType, aSource);
    fAttributesOnName.AddOrSetValue(ccf.Name, ccf);
    // only add non-consumer attributes on key (they will be linked later by the schema)
    //if wFunctions>=wdfProducerOptional
    //then
    fAttributesOnKey.AddOrSetValue(ccf.Key, ccf);
    nextKey := (wKey and not 7)+(1 shl 3);
  end;
end;

//constructor TWDClassCache.Create(aObject: TObject; const aSource: TWDID);
//begin
//  Create(aObject.ClassType, aSource);
//end;

procedure TWDClassCache.DecodeAttributes(aObject: TObject; const aEncodedAttributes: TWDValue);
var
  cursor: Integer;
  key: TWDKey;
begin
  cursor := 0;
  while cursor<Length(aEncodedAttributes) do
  begin
    key := aEncodedAttributes.bb_read_uint32(cursor);

    if not setAttribute(aObject, key, aEncodedAttributes, cursor)
    then aEncodedAttributes.bb_read_skip(cursor, key and 7);
  end;
end;

procedure TWDClassCache.DecodeAttributes(aObject: TObject; aEncodedAttributes: TWDEncodedAttributes);
var
  eap: TPair<TWDKey, TWDValue>;
  cursor: Integer;
begin
  for eap in aEncodedAttributes do
  begin
    cursor  := 0;
    setAttribute(aObject, eap.Key, eap.Value, cursor);
  end;
end;

destructor TWDClassCache.Destroy;
begin
  FreeAndNil(fAttributesOnKey);
  FreeAndNil(fAttributesOnName);
  inherited;
  FreeAndNil(fRttiContext);
end;

procedure TWDClassCache.EncodeAttributes(aObject: TObject; aEncodedAttributes: TWDEncodedAttributes);
var
  kf: TPair<TWDKey, TWDClassCacheField>;
begin
  for kf in fAttributesOnKey
  do aEncodedAttributes.AddOrSetValue(kf.Key, getAttribute(aObject, kf.Key, kf.Value.fRttiField));
end;

function TWDClassCache.EncodeAttributesFiltered(aObject: TObject; aAttributes: TArray<TWDKey>): TWDValue;
var
  attrKey: TWDKey;
  attrField: TWDClassCacheField;
begin
  Result := '';
  for attrKey in aAttributes do
  begin
    if fAttributesOnKey.TryGetValue(attrKey, attrField)
    then Result := Result+TByteBuffer.bb_uint32(attrKey)+getAttribute(aObject, attrKey, attrField.RttiField);
  end;
end;

function TWDClassCache.EncodeAttributesWithoutObjectID(aObject: TObject): TWDValue;
var
  kf: TPair<TWDKey, TWDClassCacheField>;
begin
  Result := '';
  for kf in fAttributesOnKey do
  begin
    if kf.Key<>wdatObjectID
    then Result := Result+TByteBuffer.bb_uint32(kf.Key)+getAttribute(aObject, kf.Key, kf.Value.RttiField);
  end;
end;

function TWDClassCache.getAttribute(aObject: TObject; aKey: TWDKey; aRttiField: TRttiField): TWDValue;
var
  value: TValue;
begin
  value := aRttiField.GetValue(aObject);
  case value.Kind of
    //      tkUnknown: ;
    tkInteger:
      begin
        if value.TypeInfo=TypeInfo({Integer}Int32)
        then Result := TByteBuffer.bb_int32(TValueData(value).FAsSLong)
        else if value.TypeInfo=TypeInfo(UInt32)
        then Result := TByteBuffer.bb_uint32(TValueData(value).FAsULong)
        else if value.TypeInfo=TypeInfo(Byte)
        then Result := TByteBuffer.bb_int32(TValueData(value).FAsUByte)
        else if value.TypeInfo=TypeInfo(Word)
        then Result := TByteBuffer.bb_int32(TValueData(value).FAsUWord)
        else raise Exception.Create('Unsupported int type '+string(value.TypeInfo.Name)+' in TWDClassCacheEntry.getAttribute');
      end;
    tkEnumeration:
      Result := TByteBuffer.bb_uint32(TValueData(value).FAsULong);
    tkFloat:
      begin
        if value.TypeInfo=TypeInfo(Double)
        then Result := TByteBuffer.bb_bytes(TValueData(value).FAsDouble, SizeOf(Double))
        else if value.TypeInfo=TypeInfo(Single)
        then Result := TByteBuffer.bb_bytes(TValueData(value).FAsSingle, SizeOf(Single))
        else raise Exception.Create('Unsupported float type ('+string(value.TypeInfo.Name)+') in TWDClassCacheEntry.getAttribute');
      end;
    tkChar, // AnsiChar, 1
    tkWChar:
      Result := TByteBuffer.bb_uint64(value.DataSize)+TByteBuffer.bb_bytes(value.GetReferenceToRawData^, value.DataSize);
    tkString:
      Result := TByteBuffer.bb_ansi_string(PShortString(value.GetReferenceToRawData)^);
//      tkSet: ;
    tkClass:
      begin
        // todo: process supported classes as attribute
        if value.AsObject is TWDClass then
        begin
          Result := (value.AsObject as TWDClass).Encode;
          Result := TByteBuffer.bb_uint64(Length(Result))+Result;
        end
        else raise Exception.Create('Unsupported attribute class ('+value.AsObject.ClassName+') in TWDClassCacheEntry.getAttribute');
      end;
//      tkMethod: ;
    tkLString:
      Result := TByteBuffer.bb_ansi_string(PAnsiString(value.GetReferenceToRawData)^);
    tkWString:
      Result := TByteBuffer.bb_utf8_string(UTF8String(PWideString(value.GetReferenceToRawData)^));
//      tkVariant: ;
//      tkArray:
//        Result := PrepareArrayElements(value, aTypeInfo);
    tkRecord:
      if value.TypeInfo=TypeInfo(TGUID)
      then Result := TByteBuffer.bb_uint64(sizeof(TGUID))+TByteBuffer.bb_bytes(TGUID(value.GetReferenceToRawData^), sizeof(TGUID));
//        else Result := PrepareFields(value.GetReferenceToRawData, aTypeInfo);
//      tkInterface: ;
    tkInt64:
      if value.TypeInfo=TypeInfo(Int64)
      then Result := TByteBuffer.bb_int64(TValueData(value).FAsSInt64)
      else if value.TypeInfo=TypeInfo(UInt64)
      then Result := TByteBuffer.bb_uint64(TValueData(value).FAsUInt64)
      else raise Exception.Create('Unsupported int64 type '+string(value.TypeInfo.Name)+' in TWDClassCacheEntry.getAttribute');
//      tkDynArray:
//        Result := PrepareDynamicArrayElements(value, aTypeInfo);
    tkUString:
      Result := TByteBuffer.bb_string(value.AsString);
//      tkClassRef: ;
//      tkPointer: ;
//      tkProcedure: ;
  else
    raise Exception.Create('Unsupported field type ('+IntToStr(Ord(value.Kind))+') in TWDClassCacheEntry.getAttribute');
  end;
end;

class function TWDClassCache.getAttributeWireType(aRttiField: TRttiField): TWDKey;
begin
  case aRttiField.FieldType.TypeKind of
    //tkUnknown: ;
    tkInteger:        Result := wtVarInt;
    tkChar:           Result := wtLengthDelimited;
    tkEnumeration:    Result := wtVarInt;
    tkFloat:
      begin
        if aRttiField.FieldType.TypeSize=8
        then Result := wt64Bit
        else if aRttiField.FieldType.TypeSize=4
        then Result := wt32Bit
        else raise Exception.Create('Unsupported float type ('+IntToStr(aRttiField.FieldType.TypeSize)+') for '+aRttiField.Name+' in TWDClassCache.getAttributeWireType');
      end;
    tkString:         Result := wtLengthDelimited;
    //tkSet: ;
    tkClass:
      begin
        // todo: why is the type name expanded with generics
        //if aRttiField.FieldType.Name=TWDGeometry.ClassName
        if aRttiField.FieldType.BaseType.Name=TWDClass.ClassName
        then Result := wtLengthDelimited
        else raise Exception.Create('Unsupported class type '+aRttiField.Name+' in TWDClassCache.getAttributeWireType');
      end;
    //tkMethod: ;
    tkWChar:          Result := wtLengthDelimited;
    tkLString:        Result := wtLengthDelimited;
    tkWString:        Result := wtLengthDelimited;
    //tkVariant: ;
    //tkArray: ;
    tkRecord:         Result := wtLengthDelimited;
    //tkInterface: ;
    tkInt64:          Result := wtVarInt;
    //tkDynArray: ;
    tkUString:        Result := wtLengthDelimited;
    //tkClassRef: ;
    //tkPointer: ;
    //tkProcedure: ;
  else
    raise Exception.Create('Unsupported field type ('+IntToStr(Ord(aRttiField.FieldType.TypeKind))+') for '+aRttiField.Name+' in TWDClassCache.getAttributeWireType');
  end;
end;

function TWDClassCache.NextFreeKey(aKey: TWDKey): TWDKey;
var
  keywoWT: TWDKey;
  akp: TPair<string, TWDClassCacheField>;
  akpwoWT: TWDKey;
begin
  // calc given key without wire type
  keywoWT := aKey shr 3;
  // check to min value of key, (-1 because we are going to get the NEXT key)
  if keywoWT<icehAttributeBase-1
  then keywoWT := icehAttributeBase-1;
  // check for existing higher keys
  for akp in fAttributesOnName do
  begin
    akpwoWT := akp.Value.Key shr 3;
    if keywoWT<akpwoWT
    then keywoWT := akpwoWT;
  end;
  // rebuild key with wire type as the NEXT free key
  Result := ((keywoWT+1) shl 3) or (aKey and 7);
end;

{
function TWDClassCache.getIsComplete: Boolean;
var
  nccfp: TPair<string, TWDClassCacheField>;
  classCacheField: TWDClassCacheField;
begin
  for nccfp in fAttributesByName do
  begin
    if (nccfp.Value.Key shr 3)>0 then
    begin
      if AttributesByKey.TryGetValue(nccfp.Value.Key, classCacheField) then
      begin
        if nccfp.Value<>classCacheField then
        begin
          // handle key collision or re-key!
          // for now remove from key list to recheck on next change?
          AttributesByKey.Remove(nccfp.Value.Key);
          Exit(False);
        end;
      end
      else AttributesByKey.Add(nccfp.Value.Key, nccfp.Value);
    end
    else Exit(False);
  end;
  // if all pass then return compete ie true
  Exit(True);
end;
}

function TWDClassCache.setAttribute(aObject: TObject; aKey: TWDKey; const aValue: TWDValue; var aCursor: Integer): Boolean;
var
  len: UInt64;
  field: TWDClassCacheField;
  value: TValue;

  procedure CheckKey(aWireType: Integer);
  begin
    if Integer(aKey and 7)<>aWireType
    then raise Exception.Create('wire type mismatch in type '+string(field.Name)+' in TWDClassCacheEntry.setAttribute: '+aKey.ToString+' <> '+aWireType.ToString);
  end;

begin
  if fAttributesOnKey.TryGetValue(aKey, field) then
  begin
    Result := True;
    value := field.RttiField.GetValue(aObject);
    case value.Kind of
      //      tkUnknown: ;
      tkInteger:
        begin
          CheckKey(wtVarInt);
          if value.TypeInfo=TypeInfo({Integer}Int32)
          then value := aValue.bb_read_int32(aCursor)
          else if value.TypeInfo=TypeInfo(UInt32)
          then value := aValue.bb_read_uint32(aCursor)
          else if value.TypeInfo=TypeInfo(Byte)
          then value := aValue.bb_read_uint32(aCursor)
          else if value.TypeInfo=TypeInfo(Word)
          then value := aValue.bb_read_uint32(aCursor)
          else raise Exception.Create('Unsupported int type '+string(value.TypeInfo.Name)+' in TWDClassCacheEntry.setAttribute');
          field.RttiField.SetValue(aObject, value);
        end;
      tkEnumeration:
        begin
          CheckKey(wtVarint);
          //aEncodedAttributes.bb_read_uint32(cursor);
          value := TValue.FromOrdinal(value.TypeInfo, aValue.bb_read_uint32(aCursor));
          field.RttiField.SetValue(aObject, value);
        end;
      tkFloat:
        begin
          if value.TypeInfo=TypeInfo(Double)
          then value := aValue.bb_read_double(aCursor)
          else if value.TypeInfo=TypeInfo(Single)
          then value := aValue.bb_read_single(aCursor)
          else raise Exception.Create('Unsupported float type ('+string(value.TypeInfo.Name)+') in TWDClassCacheEntry.setAttribute');
          field.RttiField.SetValue(aObject, value);
        end;
//        tkChar, // AnsiChar, 1
//        tkWChar:
//          Result := Result+BBtag(kf.Key, value.GetReferenceToRawData^, value.DataSize);
      tkString:
        begin
          CheckKey(wtLengthDelimited);
          value := {PShortString(}aValue.bb_read_string(aCursor); // todo:
          field.RttiField.SetValue(aObject, value);
        end;
  //      tkSet: ;
      tkClass:
        begin
          CheckKey(wtLengthDelimited);
          len := aValue.bb_read_uint64(aCursor);
          // todo: more classes to decode
          {value := }
          (value.AsObject as TWDClass).Decode(aValue, aCursor, aCursor+len);
          //value := TWDGeometry.CreateFromBuffer(aValue, aCursor, aCursor+len);
          field.RttiField.SetValue(aObject, value);
        end;
  //      tkMethod: ;
      tkLString:
        begin
          CheckKey(wtLengthDelimited);
          value := aValue.bb_read_string(aCursor); // todo:
          field.RttiField.SetValue(aObject, value);
        end;
      tkWString:
        begin
          CheckKey(wtLengthDelimited);
          value := WideString(aValue.bb_read_string(aCursor));
          field.RttiField.SetValue(aObject, value);
        end;
  //      tkVariant: ;
//      tkArray:
//        Result := PrepareArrayElements(value, aTypeInfo);
      tkRecord:
        if value.TypeInfo=TypeInfo(TGUID) then
        begin
          CheckKey(wtLengthDelimited);
          value := TValue.From<TGUID>(aValue.bb_read_guid(aCursor));
          field.RttiField.SetValue(aObject, value);
        end
        else raise Exception.Create('Unsupported record type '+string(value.TypeInfo.Name)+' in TWDClassCacheEntry.setAttribute');
//        else Result := PrepareFields(value.GetReferenceToRawData, aTypeInfo);
  //      tkInterface: ;
      tkInt64:
        begin
          CheckKey(wtVarint);
          if value.TypeInfo=TypeInfo(Int64)
          then value := aValue.bb_read_int64(aCursor)
          else if value.TypeInfo=TypeInfo(UInt64)
          then value := aValue.bb_read_uint64(aCursor)
          else raise Exception.Create('Unsupported int64 type '+string(value.TypeInfo.Name)+' in TWDClassCacheEntry.setAttribute');
          field.RttiField.SetValue(aObject, value);
        end;
//      tkDynArray:
//        Result := PrepareDynamicArrayElements(value, aTypeInfo);
      tkUString:
        begin
          CheckKey(wtLengthDelimited);
          value := aValue.bb_read_string(aCursor);
          field.RttiField.SetValue(aObject, value);
        end;
  //      tkClassRef: ;
  //      tkPointer: ;
  //      tkProcedure: ;
    else
      raise Exception.Create('Unsupported field type ('+IntToStr(Ord(value.Kind))+') in TWDClassCacheEntry.setAttribute');
      Result := False;
    end;
  end
  else Result := False; // aValue.bb_read_skip(aCursor, aKey and 7);
end;

function TWDClassCache.EncodeAttributes(aObject: TObject): TWDValue;
var
  kf: TPair<TWDKey, TWDClassCacheField>;
begin
  Result := '';
  for kf in fAttributesOnKey
  do Result := Result+TByteBuffer.bb_uint32(kf.Key)+getAttribute(aObject, kf.Key, kf.Value.RttiField);
end;

{ encode/decode }

{
function EncodeAttributes(aObject: TObject): TWDValue;
var
  wc: TWDClassCacheEntry;
begin
  if not classCache.TryGetValue(aObject.ClassType, wc) then
  begin
    wc := TWDClassCacheEntry.Create(aObject);
    classCache.Add(aObject.ClassType, wc);
  end;
  Result := wc.EncodeAttributes(aObject);
end;

procedure EncodeAttributes(aObject: TObject; aEncodedAttributes: TWDEncodedAttributes);
var
  wc: TWDClassCacheEntry;
begin
  if not classCache.TryGetValue(aObject.ClassType, wc) then
  begin
    wc := TWDClassCacheEntry.Create(aObject);
    classCache.Add(aObject.ClassType, wc);
  end;
  wc.EncodeAttributes(aObject, aEncodedAttributes);
end;

procedure DecodeAttributes(aObject: TObject; const aEncodedAttributes: TWDValue);
var
  wc: TWDClassCacheEntry;
begin
  if not classCache.TryGetValue(aObject.ClassType, wc) then
  begin
    wc := TWDClassCacheEntry.Create(aObject);
    classCache.Add(aObject.ClassType, wc);
  end;
  wc.DecodeAttributes(aObject, aEncodedAttributes);
end;

procedure DecodeAttributes(aObject: TObject; aEncodedAttributes: TWDEncodedAttributes);
var
  wc: TWDClassCacheEntry;
begin
  if not classCache.TryGetValue(aObject.ClassType, wc) then
  begin
    wc := TWDClassCacheEntry.Create(aObject);
    classCache.Add(aObject.ClassType, wc);
  end;
  wc.DecodeAttributes(aObject, aEncodedAttributes);
end;
}

{ TWDGeometryPoint }
{
class function TWDGeometryPoint.CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass;
begin
  Result := TWDGeometryPoint.Create;
  Result.Decode(aBuffer, aCursor, aLimit);
end;
}
constructor TWDGeometryPoint.Create(ax, ay, az: Double);
begin
  inherited Create;
  x := ax;
  y := ay;
  z := az;
end;

constructor TWDGeometryPoint.Create;
begin
  inherited Create;
  x := NaN;
  y := NaN;
  z := NaN;
end;

procedure TWDGeometryPoint.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
begin
  if aCursor<aLimit
  then x := aBuffer.bb_read_double(aCursor)
  else x := NaN;
  if aCursor<aLimit
  then y := aBuffer.bb_read_double(aCursor)
  else y := NaN;
  if aCursor<aLimit
  then z := aBuffer.bb_read_double(aCursor)
  else z := NaN;
end;

function TWDGeometryPoint.Encode: TWDValue;
begin
  if IsNaN(z)
  then Result :=
         TByteBuffer.bb_bytes(x, SizeOf(x))+
         TByteBuffer.bb_bytes(y, SizeOf(y))
  else Result :=
         TByteBuffer.bb_bytes(x, SizeOf(x))+
         TByteBuffer.bb_bytes(y, SizeOf(y))+
         TByteBuffer.bb_bytes(z, SizeOf(z));
end;

function TWDGeometryPoint.getGeoJSON2D(const aType: string): string;
begin
  // todo: ignore aType, always Point, check?
  Result := '['+x.toString(dotFormat)+','+y.toString(dotFormat)+']';
end;

class function TWDGeometryPoint.WorldType: TWDWorldType;
begin
  Result := wdkGeometryPoint;
end;

{ TWDGeometryPart }

procedure TWDGeometryPart.AddPoint(x, y, z: Double);
var
  point: TWDGeometryPoint;
begin
  point := TWDGeometryPoint.Create;
  point.x := x;
  point.y := y;
  point.z := z;
  fPoints.Add(point);
end;

function TWDGeometryPart.cn_PnPoly(x, y: Double): Boolean;
var
  cn: Integer;
  i: Integer;
  vt: Double;
begin
  cn := 0; // the  crossing number counter
  // loop through all edges of the polygon
  for i := 0 to points.Count-2 do
  begin    // edge from V[i]  to V[i+1]
    if (((points[i].y <= y) and (points[i+1].y >  y)) or     // an upward crossing
        ((points[i].y >  y) and (points[i+1].y <= y))) then  // a downward crossing
    begin
      // compute  the actual edge-ray intersect x-coordinate
      vt := (y  - points[i].y) / (points[i+1].y - points[i].y);
      if (x < points[i].x + vt * (points[i+1].x - points[i].x)) // P.x < intersect
      then cn := cn+1; // a valid crossing of y=P.y right of P.x
    end;
  end;
  Result := (cn AND 1)=1; // 0 if even (out), and 1 if  odd (in)
end;

constructor TWDGeometryPart.Create;
begin
  inherited Create;
  fPoints := TObjectList<TWDGeometryPoint>.Create;
end;
{
class function TWDGeometryPart.CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass;
begin
  Result := TWDGeometryPart.Create;
  Result.Decode(aBuffer, aCursor, aLimit);
end;
}
procedure TWDGeometryPart.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
var
  len: UInt64;
  point: TWDGeometryPoint;
begin
  while aCursor<aLimit do
  begin
    len := aBuffer.bb_read_uint64(aCursor);
    if len>0 then
    begin
      point := TWDGeometryPoint.Create;
      try
        point.Decode(aBuffer, aCursor, aCursor+len);
      finally
        points.Add(point);
      end;
    end;
  end;
end;

destructor TWDGeometryPart.Destroy;
begin
  FreeAndNil(fPoints);
  inherited;
end;

function TWDGeometryPart.Encode: TWDValue;
var
  point: TWDGeometryPoint;
  pointBuffer: TWDValue;
begin
  Result := '';
  for point in points do
  begin
    pointBuffer := point.Encode;
    Result := Result+TByteBuffer.bb_uint64(Length(pointBuffer))+pointBuffer;
  end;
end;

function TWDGeometryPart.getGeoJSON2D: string;
var
  i: Integer;
begin
  Result := '[';
  for i := 0 to fPoints.Count-1 do
  begin
    if i>0
    then Result := Result+',';
    Result := Result+fPoints[i].GeoJSON2D['Point'];
  end;
  Result := Result+']';
end;

// isLeft(): tests if a point is Left|On|Right of an infinite line.
//    Input:  line segment from L0 to L1, point x,y
//    Return: >0 for x,y left of the line through L0 and L1
//            =0 for x,y on the line
//            <0 for x,y  right of the line
//    See: Algorithm 1 "Area of Triangles and Polygons"
function isLeft(const L0, L1: TWDGeometryPoint; x, y: Double): Double; inline;
begin
  Result := (L1.x - L0.x) * (y - L0.y) - (x -  L0.x) * (L1.y - L0.y);
end;

(*
// cn_PnPoly(): crossing number test for a point in a polygon
//      Input:   P = a point,
//               V[] = vertex points of a polygon V[n+1] with V[n]=V[0]
//      Return:  0 = outside, 1 = inside
// This code is patterned after [Franklin, 2000]
function cn_PnPoly(const P: TWDGeometryPoint; const V: TArray<TWDGeometryPoint>): Integer;
var
  cn: Integer;
  i: Integer;
  vt: Double;
begin
  cn := 0;    // the  crossing number counter
  // loop through all edges of the polygon
  for i := 0 to length(V)-1 do
  begin    // edge from V[i]  to V[i+1]
    if (((V[i].y <= P.y) and (V[i+1].y >  P.y)) or     // an upward crossing
        ((V[i].y >  P.y) and (V[i+1].y <= P.y))) then  // a downward crossing
    begin
      // compute  the actual edge-ray intersect x-coordinate
      vt := (P.y  - V[i].y) / (V[i+1].y - V[i].y);
      if (P.x < V[i].x + vt * (V[i+1].x - V[i].x)) // P.x < intersect
      then cn := cn+1; // a valid crossing of y=P.y right of P.x
    end;
  end;
  Result := cn AND 1;    // 0 if even (out), and 1 if  odd (in)
end;

// wn_PnPoly(): winding number test for a point in a polygon
//      Input:   P = a point,
//               V[] = vertex points of a polygon V[n+1] with V[n]=V[0]
//      Return:  wn = the winding number (=0 only when P is outside)
function wn_PnPoly(const p: TWDGeometryPoint; const V: TArray<TWDGeometryPoint>): Integer;
var
  wn: Integer;
  i: Integer;
begin
  wn := 0; // the  winding number counter
  // loop through all edges of the polygon
  for i := 0 to length(V)-1 do    // edge from V[i] to  V[i+1]
  begin
    if (V[i].y <= P.y) then           // start y <= P.y
    begin
      if (V[i+1].y  > P.y)       // an upward crossing
      then if (isLeft(V[i], V[i+1], P) > 0)  // P left of  edge
           then wn := wn+1;            // have  a valid up intersect
    end
    else                         // start y > P.y (no test needed)
    begin
      if (V[i+1].y  <= P.y)      // a downward crossing
      then if (isLeft(V[i], V[i+1], P) < 0)  // P right of  edge
           then wn := wn-1;            // have  a valid down intersect
    end;
  end;
  Result := wn;
end;
*)

function TWDGeometryPart.wn_PnPoly(x, y: Double): Integer;
var
  wn: Integer;
  i: Integer;
begin
  wn := 0; // the  winding number counter
  // loop through all edges of the polygon
  for i := 0 to points.count-2 do    // edge from points[i] to  points[i+1]
  begin
    if (points[i].y <= y) then       // start y <= P.y
    begin
      if (points[i+1].y  > y)        // an upward crossing
      then if (isLeft(points[i], points[i+1], x, y) > 0)  // x,y left of  edge
           then wn := wn+1;          // have  a valid up intersect
    end
    else                             // start y > P.y (no test needed)
    begin
      if (points[i+1].y  <= y)       // a downward crossing
      then if (isLeft(points[i], points[i+1], x, y) < 0)  // x,y right of  edge
           then wn := wn-1;          // have  a valid down intersect
    end;
  end;
  Result := wn; // wn = the winding number (=0 only when x,y is outside)
end;

class function TWDGeometryPart.WorldType: TWDWorldType;
begin
  Result := wdkGeometryPart;
end;

{ TWDGeometry }

function TWDGeometry.AddPart: TWDGeometryPart;
begin
  Result := TWDGeometryPart.Create;
  parts.Add(Result);
end;

procedure TWDGeometry.AddPoint(x, y, z: Double);
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  if parts.Count=0
  then part := AddPart // new
  else part := parts.Last;
  point := TWDGeometryPoint.Create;
  point.x := x;
  point.y := y;
  point.z := z;
  part.points.Add(point);
end;

procedure TWDGeometry.AddPoint(aPoint: TWDGeometryPoint);
begin
  AddPoint(aPoint.x, aPoint.y, aPoint.z);
end;

constructor TWDGeometry.Create;
begin
  inherited Create;
  fParts := TObjectList<TWDGeometryPart>.Create;
end;
constructor TWDGeometry.CreateFromSVGPath(const aSVGPath: string);
var
  elements: TArray<string>;
  i: Integer;
  e: string;
  x, y: Double;
  command: Integer;
begin
  Create;
  // "M -0.395195537337987 -39.477414746610833 L -0.395540696741319 -39.477884184697793 -0.395547058653566 -39.477891264070308 -0.395556723160279 -39.477897066138951 -0.395565187274533 -39.477900769276062 -0.395576390026876 -39.477902732028269 -0.39559012262954  (...)"
  elements := aSVGPath.Split([' ']);
  x := Double.NaN;
  for i := 0 to Length(elements)-1 do
  begin
    e := elements[i];
    command := Pos(copy(e,1,1), 'MmLlZz');
    if command<=0 then
    begin
      // must be coordinates
      if IsNaN(x)
      then x := Double.Parse(e, dotFormat)
      else
      begin
        y := -Double.Parse(e, dotFormat);  // todo: orientation in y is oposite to geojson?
        // handle complete point
        AddPoint(x, y, Double.NaN);
        x := Double.NaN;
      end;
    end
    else
    begin
      case command of
        1,2:
          begin
            AddPart;
            x := Double.NaN; // signal first coordinate
          end;
        3,4:
          begin
            x := Double.NaN; // signal first coordinate
          end;
        5,6:
          begin
            if parts.Count>0
            then AddPoint(parts.Last.points.first);
          end;
      end;
    end;
  end;
end;

{
class function TWDGeometry.CreateFromBuffer(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer): TWDClass;
begin
  Result := TWDGeometry.Create;
  Result.Decode(aBuffer, aCursor, aLimit);
end;
}
procedure TWDGeometry.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
var
  len: UInt64;
  part: TWDGeometryPart;
begin
  while aCursor<aLimit do
  begin
    len := aBuffer.bb_read_uint64(aCursor);
    if len>0 then
    begin
      part := TWDGeometryPart.Create;
      try
        part.Decode(aBuffer, aCursor, aCursor+len);
      finally
        parts.Add(part);
      end;
    end;
  end;
end;

destructor TWDGeometry.Destroy;
begin
  FreeAndNil(fParts);
  inherited;
end;

function TWDGeometry.Encode: TWDValue;
var
  part: TWDGeometryPart;
  partBuffer: TWDValue;
begin
  Result := '';
  for part in parts do
  begin
    partBuffer := part.Encode;
    Result := Result+TByteBuffer.bb_uint64(Length(partBuffer))+partBuffer;
  end;
end;

function TWDGeometry.getGeoJSON2D(const aType: string): string;
var
  i: Integer;
begin
  if fParts.Count=0
  then Result := '"[]'
  else
  begin
    if aType='Point' then
    begin
      if fParts[0].fPoints.Count>0
      then Result := fParts[0].fPoints[0].GeoJSON2D[aType]
      else Result := '[]'
    end
    else
    begin
      if (aType='LineString') or (aType='MultiPoint')
      then Result := ''+fParts[0].GeoJSON2D // must be single part
      else
      begin
        Result := '';
        for i := 0 to fParts.Count-1 do
        begin
          if i>0
          then Result := Result+',';
          Result := Result+fParts[i].GeoJSON2D;
        end;
        if (aType='Polygon') or (aType='MultiLineString')
        then Result := '['+Result+']' // can be multiple part
        else Result := '[['+Result+']]'; // todo: asume MultiPolygon; how to handle extra layer?
      end;
    end;
  end;
end;

function TWDGeometry.PointInAnyPart(x, y: Double): Boolean;
var
  part: TWDGeometryPart;
begin
  Result := False;
  for part in parts do
  begin
    if part.cn_PnPoly(x,y) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TWDGeometry.PointInFirstPart(x, y: Double): Boolean;
begin
  if parts.Count>0
  then Result := parts.First.cn_PnPoly(x,y)
  else Result := False;
end;

class function TWDGeometry.WorldType: TWDWorldType;
begin
  Result := wdkGeometry;
end;

{ TWDPalette }

function TWDPalette.Clone: TWDPalette;
begin
  if Assigned(Self)
  then Result := Self._clone
  else Result := nil;
end;

constructor TWDPalette.Create(const aDescription: string);
begin
  inherited Create;
  fDescription := aDescription;
end;

procedure TWDPalette.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
begin
  fDescription := aBuffer.bb_read_string(aCursor);
end;

function TWDPalette.Encode: TWDValue;
begin
  Result := TByteBuffer.bb_string(fDescription);
end;

function TWDPalette.ValueToColorJSON(const aValue: Double): string;
begin
  Result := ColorToJSON(ValueToColor(aValue));
end;



initialization
  dotFormat.DecimalSeparator := '.';
end.
