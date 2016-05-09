unit WorldClientLib;

interface

uses
  Logger,
  imb4,
  WorldDataCode,
  WinApi.Windows, System.SysUtils, System.Generics.Collections;

const
  RemoteHostSwitch = 'RemoteHost';
  RemotePortSwitch = 'RemotePort';
  PrefixSwitch = 'Prefix';
    DefaultPrefix = 'World';


type
  TWDIDList = TList<TWDID>;

  TWDDataSources = class; // forward

  TWDBin = class; // forward

  TWDSchemaAttribute = class
  public
    key: TWDKey;
    name: string;
    worldType: TWDWorldType;
    worldFunctions: TWDFunctions;
    source: string;
  end;

  TWDSchemaCollection = class
  constructor Create;
  destructor Destroy; override;
  public
    function NextFreeKey(aKey: TWDKey): TWDKey;
  public
    attributesOnName: TObjectDictionary<string, TWDSchemaAttribute>;
    attributesOnKey: TObjectDictionary<TWDKey, TWDSchemaAttribute>;
  end;

  TWDSchema = TObjectDictionary<string, TWDSchemaCollection>;

  TWDObjectBase = class
  constructor Create(aObjectID: TWDID);
  protected
    [TWorldKey((icehObjectID shl 3) or wtLengthDelimited), TWorldName('object_id')]
    fObjectID: TWDID;
  public
    property objectID: TWDID read fObjectID;

    procedure AddorSetAttribute(aSchema: TWDClassCache; aKey: TWDKey; const aValue: TWDValue); virtual; abstract;
    procedure RemoveAttribute(aSchema: TWDClassCache; aKey: TWDKey); virtual; abstract;

    function Encode(aSchema: TWDClassCache): TByteBuffer;
    function EncodeObjectID: TByteBuffer;
    function EncodeAttributes(aSchema: TWDClassCache): TByteBuffer; virtual; abstract;
    function EncodeAttributesFiltered(aSchema: TWDClassCache; const aAttributes: TArray<TWDKey>): TByteBuffer; virtual; abstract;
  end;

  TWDObject = class(TWDObjectBase)
  constructor Create(aObjectID: TWDID);
  destructor Destroy; override;
  protected
    fAttributes: TDictionary<TWDKey, TWDValue>;
  public
    property attributes: TDictionary<TWDKey, TWDValue> read fAttributes;

    procedure AddorSetAttribute(aSchema: TWDClassCache; aKey: TWDKey; const aValue: TWDValue); override;
    procedure RemoveAttribute(aSchema: TWDClassCache; aKey: TWDKey); override;

    function EncodeAttributes(aSchema: TWDClassCache): TByteBuffer; override;
    function EncodeAttributesFiltered(aSchema: TWDClassCache; const aAttributes: TArray<TWDKey>): TByteBuffer; override;
  end;

  TWDObjectBaseRTTI = class(TWDObjectBase)
    procedure AddorSetAttribute(aSchema: TWDClassCache; aKey: TWDKey; const aValue: TWDValue); override;
    procedure RemoveAttribute(aSchema: TWDClassCache; aKey: TWDKey); override;

    function EncodeAttributes(aSchema: TWDClassCache): TByteBuffer; override;
    function EncodeAttributesFiltered(aSchema: TWDClassCache; const aAttributes: TArray<TWDKey>): TByteBuffer; override;
  end;

  TWDOnCreateObject = reference to function(const aObjectID: TWDID): TWDObjectBase;

  TWDCollection = class
  constructor Create(aBin: TWDBin; const aName: string; aLocalSchema: TWDClassCache; aOnCreateObject: TWDOnCreateObject=nil);
  destructor Destroy; override;
  private
    fBin: TWDBin; // ref
    fEvent: TEventEntry; // ref
    fPrivateEvent: TEventEntry; // ref
    fUpdateBuffer: TByteBuffer;
    fUpdateSourceFramework: Boolean;
    fName: string;
    fObjects: TObjectDictionary<TWDID, TWDObjectBase>; // owned

    fOnCreateObject: TWDOnCreateObject;
    fLocalSchema: TWDClassCache; // ref
  private
    procedure handleCollectionEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
  public
    property Objects: TObjectDictionary<TWDID, TWDObjectBase> read fObjects;
    property Name: string read fName;

    property OnCreateObject: TWDOnCreateObject read fOnCreateObject write fOnCreateObject;

    procedure signalAddObject(aObject: TWDObjectBase);
    function signalUpdateObject(aObject: TWDObjectBase; const aAttributes: TArray<TWDKey>): Boolean;
    procedure signalNoAttribute(const aObjectID: TWDID; aKey: TWDKey);
    procedure signalNoObject(const aObjectID: TWDID);

    // todo:
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure signalTruncate;
    procedure signalInquire(const aFilter: string; const aReturnEventName: string='');

    procedure Subscribe;
    procedure UnSubscribe;
  end;

  TWDCollections = TObjectDictionary<string, TWDCollection>;

  TWDBin = class
  constructor Create(aDataSources: TWDDataSources; const aBinID: TWDID; aLoaded: Boolean=False);
  destructor Destroy; override;
  private
    fDataSources: TWDDataSources;
    fEvent: TEventEntry;
    fBinID: TWDID;
    fLoaded: Boolean;
    fCollections: TWDCollections;
  private
    procedure handleBinEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
  protected
    procedure signalBinLoad(aEventEntry: TEventEntry; const aBinID: TWDID);
    procedure signalBinUnLoad(aEventEntry: TEventEntry; const aBinID: TWDID);
    procedure signalCollectionAdd(aEventEntry: TEventEntry; const aCollectionName: string);
  public
    property DataSources: TWDDataSources read fDataSources;
    property BinID: TWDID read fBinID;
    property Loaded: Boolean read fLoaded;
    property Collections: TWDCollections read fCollections;

    procedure Subscribe;
    procedure UnSubscribe;
    // load/unload does implicit subscribe/unsubscribe
    procedure Load;
    procedure UnLoad;
  end;

  TWDBins = TObjectDictionary<TWDID, TWDBin>;

  TWDDataSources = class
  constructor Create(aConnection: TConnection);
  destructor Destroy; override;
  private
    fConnection: TConnection;
    fEvent: TEventEntry;
    fPrivateEvent: TEventEntry;
    fBins: TWDBins;
    fSources: TList<string>;
    fSchema: TWDSchema;
  private
    procedure handleDataSourcesEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
  protected
    // data source
    procedure signalInquireDataSources(aEventEntry: TEventEntry; const aReturnEventName: string='');
    procedure signalInquireBins(aEventEntry: TEventEntry; const aReturnEventName: string='');
    procedure signalInquireSchema(aEventEntry: TEventEntry; const aReturnEventName: string='');

    //procedure signalSchemaAddAttribute(aEventEntry: TEventEntry; const aSchemaCollectionName: string; aAttributeKey: TWDKey; const aAttributeName: string; aWorldType: TWDWorldType);
    procedure signalSchemaAttribute(aEventEntry: TEventEntry; const aSchemaSource, aSchemaCollectionName: string; const aSchemaAttribute: TWDSchemaAttribute); overload;
    procedure signalSchemaAttribute(aEventEntry: TEventEntry; const aSchemaSource, aSchemaCollectionName: string; const aSchemaAttribute: TWDClassCacheField); overload;

    procedure signalBinCreate(aEventEntry: TEventEntry; const aBinID: TWDID; const aDescription: string; aParentID, aRefID: TWDID);
    procedure signalBinCopy(aEventEntry: TEventEntry; const aSrcID, aDstID: TWDID); overload;
    procedure signalBinDelete(aEventEntry: TEventEntry; const aBinID: TWDID);
  public
    property Connection: TConnection read fConnection;
    property Bins: TWDBins read fBins;
    property Sources: TList<string> read fSources;
    property Schema: TWDSchema read fSchema;

    function Bin(const aBinID: TWDID): TWDBin;
  end;

implementation

function ReadValue(const aPayload: TByteBuffer; var aCursor: Integer; aWireType: Integer): TWDValue;
var
  sv: Integer;
  len: UInt64;
begin
  case aWireType of
    wtVarInt:
      begin
        sv := aCursor;
        aPayload.bb_read_uint64(aCursor); // we only want the new cursor to store the buffer occupied by the varint
        result := copy(aPayload, sv+1, aCursor-sv);
      end;
    wt64Bit:
      begin
        result := copy(aPayload, aCursor+1, 8);
        Inc(aCursor, 8);
      end;
    wtLengthDelimited:
      begin
        sv := aCursor;
        len := aPayload.bb_read_uint64(aCursor);
        result := copy(aPayload, sv+1, len+aCursor-sv);
        Inc(aCursor, len);
      end;
    wt32Bit:
      begin
        result := copy(aPayload, aCursor+1, 4);
        Inc(aCursor, 4);
      end
  else
    raise Exception.Create('Unsupported wire type ('+aWireType.ToString+') in ReadValue');
  end;
end;

{ TWDSchemaCollection }

constructor TWDSchemaCollection.Create;
begin
  inherited Create;
  attributesOnName := TObjectDictionary<string, TWDSchemaAttribute>.Create([doOwnsValues]);
  attributesOnKey := TObjectDictionary<TWDKey, TWDSchemaAttribute>.Create([]);
end;

destructor TWDSchemaCollection.Destroy;
begin
  FreeAndNil(attributesOnName);
  FreeAndNil(attributesOnKey);
  inherited;
end;

function TWDSchemaCollection.NextFreeKey(aKey: TWDKey): TWDKey;
var
  keywoWT: TWDKey;
  akp: TPair<string, TWDSchemaAttribute>;
  akpwoWT: TWDKey;
begin
  // calc given key without wire type
  keywoWT := aKey shr 3;
  // check to min value of key, (-1 because we are going to get the NEXT key)
  if keywoWT<icehAttributeBase-1
  then keywoWT := icehAttributeBase-1;
  // check for existing higher keys
  for akp in attributesOnName do
  begin
    akpwoWT := akp.Value.Key shr 3;
    if keywoWT<akpwoWT
    then keywoWT := akpwoWT;
  end;
  // rebuild key with wire type as the NEXT free key
  Result := ((keywoWT+1) shl 3) or (aKey and 7);
end;

{ TWDObjectBase }

constructor TWDObjectBase.Create(aObjectID: TWDID);
begin
  inherited Create;
  fObjectID := aObjectID;
end;

function TWDObjectBase.Encode(aSchema: TWDClassCache): TByteBuffer;
begin
  Result := EncodeObjectID+EncodeAttributes(aSchema);
end;

function TWDObjectBase.EncodeObjectID: TByteBuffer;
begin
  Result := TByteBuffer.bb_tag_rawbytestring(wdatObjectID shr 3, fObjectID);
end;

{ TWDObject }

procedure TWDObject.AddorSetAttribute(aSchema: TWDClassCache; aKey: TWDKey; const aValue: TWDValue);
begin
  fAttributes.AddOrSetValue(aKey, aValue);
end;

constructor TWDObject.Create(aObjectID: TWDID);
begin
  inherited Create(aObjectID);
  fAttributes := TDictionary<TWDKey, TWDValue>.Create;
end;

destructor TWDObject.Destroy;
begin
  FreeAndNil(fAttributes);
  inherited;
end;

function TWDObject.EncodeAttributes(aSchema: TWDClassCache): TByteBuffer;
var
  kav: TPair<TWDKey, TWDValue>;
begin
  Result := '';
  for kav in fAttributes
  do Result := Result+TByteBuffer.bb_uint32(kav.Key)+kav.Value;
end;

function TWDObject.EncodeAttributesFiltered(aSchema: TWDClassCache; const aAttributes: TArray<TWDKey>): TByteBuffer;
var
  key: TWDKey;
  value: TWDValue;
begin
  Result := '';
  for key in aAttributes do
  begin
    if fAttributes.TryGetValue(key, value)
    then Result := Result+TByteBuffer.bb_uint32(key)+value;
  end;
end;

procedure TWDObject.RemoveAttribute(aSchema: TWDClassCache; aKey: TWDKey);
begin
  fAttributes.Remove(aKey); // todo: exception?
end;

{ TWDObjectBaseRTTI }

procedure TWDObjectBaseRTTI.AddorSetAttribute(aSchema: TWDClassCache; aKey: TWDKey; const aValue: TWDValue);
var
  cursor: Integer;
begin
  cursor := 0;
  aSchema.setAttribute(Self, aKey, aValue, cursor);
end;

function TWDObjectBaseRTTI.EncodeAttributes(aSchema: TWDClassCache): TByteBuffer;
begin
  Result := aSchema.EncodeAttributesWithoutObjectID(Self);
end;

function TWDObjectBaseRTTI.EncodeAttributesFiltered(aSchema: TWDClassCache; const aAttributes: TArray<TWDKey>): TByteBuffer;
begin
  Result := aSchema.EncodeAttributesFiltered(Self, aAttributes);
end;

procedure TWDObjectBaseRTTI.RemoveAttribute(aSchema: TWDClassCache; aKey: TWDKey);
begin
  // todo:
end;

{ TWDCollection }

procedure TWDCollection.BeginUpdate;
begin
  // todo: implement
end;

constructor TWDCollection.Create(aBin: TWDBin; const aName: string; aLocalSchema: TWDClassCache; aOnCreateObject: TWDOnCreateObject);
var
  laccfp: TPair<string, TWDClassCacheField>;
  schemaCollection: TWDSchemaCollection;
  schemaAttributeOnName: TWDSchemaAttribute;
  schemaAttributeOnKey: TWDSchemaAttribute;
  cmp: Integer;
begin
  inherited Create;
  fBin := aBin;
  fUpdateBuffer := '';
  fName := aName;
  fObjects := TObjectDictionary<TWDID, TWDObjectBase>.Create([doOwnsValues]);
  fOnCreateObject := aOnCreateObject;
  fLocalSchema := aLocalSchema;
  fBin.Collections.Add(fName, self);
  // update local schema to data source schema
  if fBin.DataSources.Schema.TryGetValue(fName, schemaCollection) then
  begin
    // check for collisions
    for laccfp in fLocalSchema.attributesOnName do
    begin
      if schemaCollection.attributesOnName.TryGetValue(laccfp.Value.Name, schemaAttributeOnName) then
      begin
        schemaAttributeOnName.worldFunctions := schemaAttributeOnName.worldFunctions or laccfp.Value.Functions;
        if schemaAttributeOnName.worldType<>laccfp.Value.WType then
        begin
          // conflict on world type
          Log.WriteLn(
            'Local schema update conflict on world type '+schemaAttributeOnName.WorldType.ToHexString+' <> '+laccfp.Value.WType.ToHexString+
            ' for '+fName+'.'+schemaAttributeOnName.name, llWarning);
        end;
        if laccfp.Value.Key<>schemaAttributeOnName.key then
        begin
          // conflict on key: choose which key to use
          cmp := CompareHighestFunction(laccfp.Value.Functions, schemaAttributeOnName.worldFunctions);
          if (cmp<0) or ((cmp=0) and (fBin.DataSources.fPrivateEvent.eventName<schemaAttributeOnName.source)) then
          begin // 'they' win, we adjust our schema definition and (re-)signal for this attribute
            // use their key
            fLocalSchema.attributesOnKey.Remove(laccfp.Value.Key);
            laccfp.Value.Key := schemaAttributeOnName.key;
            fLocalSchema.attributesOnKey.Add(laccfp.Value.Key, laccfp.Value);
          end
          else
          begin // we win -> signal our schema definition for this attribute
            schemaCollection.attributesOnKey.Remove(schemaAttributeOnName.key);
            schemaAttributeOnName.key := laccfp.Value.Key;
            schemaCollection.attributesOnKey.Add(schemaAttributeOnName.key, schemaAttributeOnName);
          end;
          // signal the new key, we have reset the key for us or are now using their key
          fBin.DataSources.signalSchemaAttribute(fBin.DataSources.fEvent, fBin.DataSources.fPrivateEvent.eventName, Name, laccfp.Value);
        end;
      end
      else
      begin
        // our field name is unknown -> add and signal but check for key collision first
        schemaAttributeOnName := TWDSchemaAttribute.Create;
        // fill
        schemaAttributeOnName.name := laccfp.Value.Name;
        schemaAttributeOnName.key := laccfp.Value.Key;
        schemaAttributeOnName.worldType := laccfp.Value.WType;
        schemaAttributeOnName.worldFunctions := laccfp.Value.Functions;
        schemaAttributeOnName.source := fBin.DataSources.fPrivateEvent.eventName;
        // add on name
        schemaCollection.attributesOnName.Add(schemaAttributeOnName.Name, schemaAttributeOnName);
        // check on key
        if schemaCollection.attributesOnKey.TryGetValue(schemaAttributeOnName.key, schemaAttributeOnKey) then
        begin
          // name is not yet known but we have a conflict based on our key, we or they must find a new key
          cmp := CompareHighestFunction(laccfp.Value.Functions, schemaAttributeOnKey.worldFunctions);
          if (cmp<0) or ((cmp=0) and (fBin.DataSources.fPrivateEvent.eventName<schemaAttributeOnKey.source)) then
          begin // 'they' win
            fLocalSchema.attributesOnKey.Remove(schemaAttributeOnName.key);
            schemaAttributeOnName.key := schemaCollection.NextFreeKey(fLocalSchema.NextFreeKey(schemaAttributeOnName.key));
            laccfp.Value.Key := schemaAttributeOnName.key;
            fLocalSchema.attributesOnKey.Add(laccfp.Value.Key, laccfp.Value);
            schemaCollection.attributesOnKey.Add(schemaAttributeOnName.key, schemaAttributeOnName);
          end
          else
          begin // we win
            // kick other key out, leave name for now, but signal our key; this will trigger re-key of 'their' entry
            schemaCollection.attributesOnKey.AddOrSetValue(schemaAttributeOnName.key, schemaAttributeOnName);
            // todo: mark 'their' as invalid?
            schemaAttributeOnName.key := 0;
          end;
          // signal the key we are using
          fBin.DataSources.signalSchemaAttribute(fBin.DataSources.fEvent, fBin.DataSources.fPrivateEvent.eventName, Name, laccfp.Value);
        end
        else
        begin
          schemaCollection.attributesOnKey.Add(schemaAttributeOnName.Key, schemaAttributeOnName); // no conflict
          // signal the key we are using
          fBin.DataSources.signalSchemaAttribute(fBin.DataSources.fEvent, fBin.DataSources.fPrivateEvent.eventName, Name, schemaAttributeOnName);
        end;
      end;
    end;
  end
  else
  begin
    // add this new collection to the schema, cannot be any conflicts..
    schemaCollection := TWDSchemaCollection.Create;
    fBin.DataSources.Schema.Add(fName, schemaCollection);
    for laccfp in fLocalSchema.attributesOnName do
    begin
      schemaAttributeOnName := TWDSchemaAttribute.Create;
      // fill
      schemaAttributeOnName.name := laccfp.Value.Name;
      schemaAttributeOnName.key := laccfp.Value.Key;
      schemaAttributeOnName.worldType := laccfp.Value.WType;
      schemaAttributeOnName.worldFunctions := laccfp.Value.Functions;
      schemaAttributeOnName.source := fBin.DataSources.fPrivateEvent.eventName;
      // add
      schemaCollection.attributesOnName.Add(schemaAttributeOnName.Name, schemaAttributeOnName);
      schemaCollection.attributesOnKey.Add(schemaAttributeOnName.Key, schemaAttributeOnName);
      // signal the key we are using
      fBin.DataSources.signalSchemaAttribute(fBin.DataSources.fEvent, fBin.DataSources.fPrivateEvent.eventName, Name, schemaAttributeOnName);
    end;
  end;
  // data channel
  fEvent := nil;
  fPrivateEvent := nil;
  Subscribe;
  signalInquire(''); // request initial set of objects
end;

destructor TWDCollection.Destroy;
begin
  if Assigned(fEvent) and fEvent.OnEvent.Contains(handleCollectionEvent) then
  begin
    fEvent.OnEvent.Remove(handleCollectionEvent);
    fEvent := nil;
  end;
  if Assigned(fPrivateEvent) and fPrivateEvent.OnEvent.Contains(handleCollectionEvent) then
  begin
    fPrivateEvent.OnEvent.Remove(handleCollectionEvent);
    fPrivateEvent := nil;
  end;
  FreeAndNil(fObjects);
  inherited;
end;

procedure TWDCollection.EndUpdate;
begin
  // todo: implement
end;

procedure TWDCollection.handleCollectionEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  key: TWDKey;
  objectID: TWDID;
  obj: TWDObjectBase;
  attributeKey: TWDKey;
  collectionName: string;
  attributeValue: TWDValue;
begin
  while aCursor<aLimit do
  begin
    key := aBuffer.bb_read_uint32(aCursor);
    case key of
      wdatNoObjectID:
        begin
          objectID := aBuffer.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fObjects);
          try
            fUpdateSourceFramework := True;
            try
              fObjects.Remove(objectID);
            finally
              fUpdateSourceFramework := False;
            end;
          finally
            TMonitor.Exit(fObjects);
          end;
        end;
      wdatObjectID:
        begin
          objectID := aBuffer.bb_read_rawbytestring(aCursor);
          if not fObjects.TryGetValue(objectID, obj) then
          begin
            if Assigned(fOnCreateObject)
            then obj := OnCreateObject(objectID)
            else obj := TWDObject.Create(objectID);
            TMonitor.Enter(fObjects);
            try
              fUpdateSourceFramework := True;
              try
                fObjects.AddOrSetValue(objectID, obj);
              finally
                fUpdateSourceFramework := False;
              end;
            finally
              TMonitor.Exit(fObjects);
            end;
//            Inc(fBin.fDataSources.fObjectCount);
//            if (fBin.fDataSources.fObjectCount mod 100)=0
//            then Log.Progress('Objects: '+fBin.fDataSources.fObjectCount.ToString());
          end;
        end;
      (icehAttributeBase shl 3)..((icehAttributeTop shl 3) or 7):
        begin
          attributeValue := ReadValue(aBuffer, aCursor, key and 7);
          if Assigned(obj) then
          begin
            TMonitor.Enter(fObjects);
            try
              fUpdateSourceFramework := True;
              try
                obj.AddorSetAttribute(fLocalSchema, key, attributeValue);
              finally
                fUpdateSourceFramework := False;
              end;
            finally
              TMonitor.Exit(fObjects);
            end;
          end;
        end;
      wdatNoAttribute:
        begin
          attributeKey := aBuffer.bb_read_uint32(aCursor);
          if Assigned(obj) then
          begin
            TMonitor.Enter(fObjects);
            try
              fUpdateSourceFramework := True;
              try
                obj.RemoveAttribute(fLocalSchema, attributeKey);
              finally
                fUpdateSourceFramework := False;
              end;
            finally
              TMonitor.Exit(fObjects);
            end;
          end;
        end;
      wdatCollectionTruncate:
        begin
          collectionName := aBuffer.bb_read_string(aCursor);
          if (collectionName='') or (SameText(collectionName, fName)) then
          begin
            TMonitor.Enter(fObjects);
            try
              fUpdateSourceFramework := True;
              try
                fObjects.Clear;
              finally
                fUpdateSourceFramework := False;
              end;
            finally
              TMonitor.Exit(fObjects);
            end;
          end;
        end;
    else
      aBuffer.bb_read_skip(aCursor, key and 7);
    end;
  end;
end;

procedure TWDCollection.signalAddObject(aObject: TWDObjectBase);
begin
  fEvent.signalEvent(aObject.Encode(fLocalSchema));
end;

procedure TWDCollection.signalInquire(const aFilter: string; const aReturnEventName: string);
begin
  if aReturnEventName='' then
  begin
    fEvent.signalEvent(
      TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, fPrivateEvent.eventName)+
      TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aFilter));
  end
  else
  begin
    fEvent.signalEvent(
      TByteBuffer.bb_tag_string(wdatReturnEventName shr 3, aReturnEventName)+
      TByteBuffer.bb_tag_string(wdatObjectsInquire shr 3, aFilter));
  end;
end;

procedure TWDCollection.signalNoAttribute(const aObjectID: TWDID; aKey: TWDKey);
begin
  fEvent.signalEvent(
    TByteBuffer.bb_tag_rawbytestring(wdatObjectID shr 3, aObjectID)+
    TByteBuffer.bb_tag_uint32(wdatNoAttribute shr 3, aKey));
end;

procedure TWDCollection.signalNoObject(const aObjectID: TWDID);
begin
  fEvent.signalEvent(TByteBuffer.bb_tag_rawbytestring(wdatNoAttribute shr 3, aObjectID));
end;

procedure TWDCollection.signalTruncate;
begin
  fEvent.signalEvent(TByteBuffer.bb_tag_string(wdatCollectionTruncate shr 3, fName));
end;

function TWDCollection.signalUpdateObject(aObject: TWDObjectBase; const aAttributes: TArray<TWDKey>): Boolean;
var
  payload: TByteBuffer;
begin
  if Assigned(aObject) then
  begin
    payload := aObject.EncodeAttributesFiltered(fLocalSchema, aAttributes);
    if payload<>'' then
    begin
      payload := aObject.EncodeObjectID+payload;
      fEvent.signalEvent(payload);
      Result := True;
    end
    else Result := False;
  end
  else Result := False;
end;

procedure TWDCollection.Subscribe;
begin
  fEvent := fBin.fDataSources.Connection.subscribe(fBin.fEvent.eventName+'.'+fName, False);
  fEvent.OnEvent.Add(handleCollectionEvent);
  fPrivateEvent := fBin.fDataSources.Connection.subscribe(fBin.fDataSources.fPrivateEvent.eventName+'.'+string(fBin.fBinID)+'.'+fName, False);
  fPrivateEvent.OnEvent.Add(handleCollectionEvent);
end;

procedure TWDCollection.UnSubscribe;
begin
  if Assigned(fEvent) and fEvent.OnEvent.Contains(handleCollectionEvent)
  then fEvent.OnEvent.Remove(handleCollectionEvent);
  if Assigned(fPrivateEvent) and fPrivateEvent.OnEvent.Contains(handleCollectionEvent)
  then fPrivateEvent.OnEvent.Remove(handleCollectionEvent);
end;

{ TWDBin }

constructor TWDBin.Create(aDataSources: TWDDataSources; const aBinID: TWDID; aLoaded: Boolean);
begin
  inherited Create;
  fDataSources := aDataSources;
  fBinID := aBinID;
  fLoaded := aLoaded;
  fEvent := nil;
  fCollections := TWDCollections.Create([doOwnsValues]);
end;

destructor TWDBin.Destroy;
begin
  UnSubscribe;
  FreeAndNil(fCollections);
  inherited;
end;

procedure TWDBin.handleBinEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  key: TWDKey;
  binID: TWDID;
begin
  while aCursor<aLimit do
  begin
    key := aBuffer.bb_read_uint32(aCursor);
    case key of
      wdatBinLoaded,
      wdatBinUnLoaded:
        begin
          binID := aBuffer.bb_read_rawbytestring(aCursor);
          if (binID='') or (binID=fBinID)
          then fLoaded := key=wdatBinLoaded;
        end;
    else
      aBuffer.bb_read_skip(aCursor, key and 7); // 7290=911=wdatDataSource
    end;
  end;
end;

procedure TWDBin.Load;
begin
  Subscribe;
  signalBinLoad(fEvent, fBinID);
end;

procedure TWDBin.signalBinLoad(aEventEntry: TEventEntry; const aBinID: TWDID);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_rawbytestring(wdatBinLoad shr 3, aBinID));
end;

procedure TWDBin.signalBinUnLoad(aEventEntry: TEventEntry; const aBinID: TWDID);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_rawbytestring(wdatBinUnLoad shr 3, aBinID));
end;

procedure TWDBin.signalCollectionAdd(aEventEntry: TEventEntry; const aCollectionName: string);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_string(wdatCollectionAdd shr 3, aCollectionName));
end;

procedure TWDBin.Subscribe;
begin
  fEvent := fDataSources.Connection.Subscribe('Bins.'+string(UTF8String(fBinID)));
  if not fEvent.OnEvent.Contains(handleBinEvent)
  then fEvent.OnEvent.Add(handleBinEvent);
end;

procedure TWDBin.UnLoad;
begin
  signalBinUnLoad(fDataSources.fEvent, fBinID);
  UnSubscribe;
end;

procedure TWDBin.UnSubscribe;
begin
  if Assigned(fEvent) and fEvent.OnEvent.Contains(handleBinEvent)
  then fEvent.OnEvent.Remove(handleBinEvent);
  fEvent := nil;
end;

{ TWDDataSources }

function TWDDataSources.Bin(const aBinID: TWDID): TWDBin;
begin
  TMonitor.Enter(fBins);
  try
    if not fBins.TryGetValue(aBinID, Result) then
    begin
      Result := TWDBin.Create(Self, aBinID);
      fBins.Add(aBinID, Result);
    end;
    Result.Subscribe;
  finally
    TMonitor.Exit(fBins);
  end;
end;

constructor TWDDataSources.Create(aConnection: TConnection);
begin
  inherited Create;
  fConnection := aConnection;
  fBins := TWDBins.Create([doOwnsValues]);
  fSources := TList<string>.Create;
  fSchema := TWDSchema.Create([doOwnsValues]);
  fPrivateEvent := fConnection.subscribe(fConnection.privateEventName, False);
  fPrivateEvent.OnEvent.Add(handleDataSourcesEvent);
  fEvent := fConnection.subscribe('DataSources');
  fEvent.OnEvent.Add(handleDataSourcesEvent);

  signalInquireDataSources(fEvent, fPrivateEvent.eventName);
  signalInquireBins(fEvent, fPrivateEvent.eventName);
  signalInquireSchema(fEvent, fPrivateEvent.eventName);
end;

destructor TWDDataSources.Destroy;
begin
  if Assigned(fPrivateEvent) and fPrivateEvent.OnEvent.Contains(handleDataSourcesEvent)
  then fPrivateEvent.OnEvent.Remove(handleDataSourcesEvent);
  if Assigned(fEvent) and fEvent.OnEvent.Contains(handleDataSourcesEvent)
  then fEvent.OnEvent.Remove(handleDataSourcesEvent);
  FreeAndNil(fBins);
  FreeAndNil(fSources);
  FreeAndNil(fSchema);
  inherited;
end;

procedure TWDDataSources.handleDataSourcesEvent(aEventEntry: TEventEntry; const aBuffer: TByteBuffer; aCursor, aLimit: Integer);
var
  key: TWDKey;
  dataSourceEventName: string;
  binID: TWDID;
  bin: TWDBin;
  schemaSource: string;
  schemaCollectionName: string;
  schemaCollection: TWDSchemaCollection;
  schemaAttributeKey: TWDKey;
  schemaAttributeOnName: TWDSchemaAttribute;
  schemaAttributeName: string;
  schemaAttributeWorldFunction: TWDFunction;
  schemaAttributeWorldType: TWDWorldType;
  loaded: Boolean;
  schemaAttributeOnKey: TWDSchemaAttribute;
  cmp: Integer;
  collection: TWDCollection;
  ls: TWDClassCacheField;
begin
  schemaSource := '';
  schemaCollectionName := '';
  schemaCollection := nil;
  schemaAttributeOnName := nil;
  schemaAttributeName := '';
  schemaAttributeWorldFunction := 0;
  schemaAttributeWorldType := 0;
  while aCursor<aLimit do
  begin
    key := aBuffer.bb_read_uint32(aCursor);
    case key of
      wdatDataSource:
        begin
          dataSourceEventName := aBuffer.bb_read_string(aCursor);
          if fSources.IndexOf(dataSourceEventName)<0
          then fSources.Add(dataSourceEventName);
        end;
      wdatBinLoaded,
      wdatBinUnLoaded:
        begin
          binID := aBuffer.bb_read_rawbytestring(aCursor);
          loaded := key=wdatBinLoaded;
          TMonitor.Enter(fBins);
          try
            if not fBins.TryGetValue(binID, bin) then
            begin
              bin := TWDBin.Create(Self, binID, loaded);
              fBins.Add(binID, bin);
            end
            else bin.fLoaded := loaded;
          finally
            TMonitor.Exit(fBins);
          end;
        end;
      wdatBinCreated:
        begin
          binID := aBuffer.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fBins);
          try
            if not fBins.ContainsKey(binID) then
            begin
              bin := TWDBin.Create(Self, binID, True);
              fBins.Add(binID, bin);
            end;
          finally
            TMonitor.Exit(fBins);
          end;
        end;
      wdatBinDeleted:
        begin
          binID := aBuffer.bb_read_rawbytestring(aCursor);
          TMonitor.Enter(fBins);
          try
            fBins.Remove(binID); // todo:
          finally
            TMonitor.Exit(fBins);
          end;
        end;
      // schema
      wdatSchemaSource:
        begin
          schemaSource := aBuffer.bb_read_string(aCursor);
        end;
      wdatSchemaCollection:
        begin
          schemaCollectionName := aBuffer.bb_read_string(aCursor);
          // todo: for now we process the schema for all known collections
          // todo: this could change to only registering the schema for the collections known to us..
          if not fSchema.TryGetValue(schemaCollectionName, schemaCollection) then
          begin
            schemaCollection := TWDSchemaCollection.Create;
            fSchema.Add(schemaCollectionName, schemaCollection);
          end;
        end;
      wdatSchemaAttributeName:
        begin
          schemaAttributeName := aBuffer.bb_read_string(aCursor);
        end;
      wdatSchemaAttributeFunction:
        begin
          schemaAttributeworldFunction := aBuffer.bb_read_uint32(aCursor);
        end;
      wdatSchemaAttributeWorldType:
        begin
          schemaAttributeWorldType := aBuffer.bb_read_int32(aCursor);
        end;
      wdatSchemaAttributeKey:
        begin
          // pre: schemaAttributeName
          // pre: schemaAttributeWorldFunction
          // pre: schemaAttributeWorldType
          schemaAttributeKey := aBuffer.bb_read_uint32(aCursor);
          if Assigned(schemaCollection) then
          begin
            if schemaCollection.attributesOnName.TryGetValue(schemaAttributeName, schemaAttributeOnName) then
            begin
              // check world type
              if schemaAttributeOnName.WorldType<>schemaAttributeWorldType then
              begin
                // conflict on world type, log only for now..
                Log.WriteLn(
                  'Remote schema update conflict on world type '+schemaAttributeOnName.WorldType.ToHexString+' <> '+schemaAttributeWorldType.ToHexString+' '+
                  'for '+schemaCollectionName+'.'+schemaAttributeName+' recevied from '+schemaSource, llWarning);
              end;
              // check the key
              if schemaAttributeKey<>schemaAttributeOnName.key then
              begin
                // conflict on key: choose which key to use
                cmp := CompareHighestFunction(schemaAttributeOnName.worldFunctions, schemaAttributeWorldFunction);
                if (cmp<0) or ((cmp=0) and (schemaAttributeOnName.source<schemaSource)) then
                begin // 'they' win, use their key
                  // adjust the specific collection in all bins; schemaAttributeOnName.key -> schemaAttributeKey
                  TMonitor.Enter(fBins);
                  try
                    for bin in fBins.Values do
                    begin
                      if bin.Collections.TryGetValue(schemaCollectionName, collection) then
                      begin
                        if collection.fLocalSchema.attributesOnKey.TryGetValue(schemaAttributeOnName.key, ls) then
                        begin
                          collection.fLocalSchema.attributesOnKey.Remove(schemaAttributeOnName.key);
                          ls.Key := schemaAttributeKey;
                          collection.fLocalSchema.attributesOnKey.Add(ls.Key, ls);
                        end;
                      end;
                    end;
                  finally
                    TMonitor.Exit(fBins);
                  end;
                  // adjust the general schema
                  schemaCollection.attributesOnKey.Remove(schemaAttributeOnName.key);
                  schemaAttributeOnName.key := schemaAttributeKey;
                  schemaCollection.attributesOnKey.Add(schemaAttributeOnName.key, schemaAttributeOnName);
                end
                else
                begin // we win -> signal our schema registration for this attribute to override on the sender
                  // could be multiple clients sending this: thats ok
                  signalSchemaAttribute(fEvent, schemaAttributeOnName.source, schemaCollectionName, schemaAttributeOnName);
                end;
              end;
              // adjust the world functions registration based on the name
              schemaAttributeOnName.worldFunctions := schemaAttributeOnName.worldFunctions or schemaAttributeWorldFunction;
            end
            else
            begin
              if schemaCollection.attributesOnKey.TryGetValue(schemaAttributeKey, schemaAttributeOnKey) then
              begin
                // conflict on key in schemaAttributeOnName and in schemaAttributeOnKey: must be different
                // name is not yet known but we have a conflict based on our key, we or they must find a new key
                cmp := CompareHighestFunction(schemaAttributeOnKey.worldFunctions, schemaAttributeWorldFunction);
                if (cmp<0) or ((cmp=0) and (schemaAttributeOnKey.source<schemaSource)) then
                begin // 'they' win
                  // get new key for existing registration
                  schemaAttributeOnKey.key := schemaCollection.NextFreeKey(schemaAttributeOnKey.key);
                  // adjust the specific collection in all bins; schemaAttributeKey -> schemaAttributeOnKey.key
                  TMonitor.Enter(fBins);
                  try
                    for bin in fBins.Values do
                    begin
                      if bin.Collections.TryGetValue(schemaCollectionName, collection) then
                      begin
                        if collection.fLocalSchema.attributesOnKey.TryGetValue(schemaAttributeKey, ls) then
                        begin
                          collection.fLocalSchema.attributesOnKey.Remove(schemaAttributeKey);
                          ls.Key := schemaAttributeOnKey.key;
                          collection.fLocalSchema.attributesOnKey.Add(ls.Key, ls);
                        end;
                      end;
                    end;
                  finally
                    TMonitor.Exit(fBins);
                  end;
                  // adjust general schema
                  schemaCollection.attributesOnKey.Remove(schemaAttributeKey);
                  schemaCollection.attributesOnKey.Add(schemaAttributeOnKey.key, schemaAttributeOnKey);
                  // signal our new general schema
                  signalSchemaAttribute(fEvent, schemaAttributeOnKey.source, schemaCollectionName, schemaAttributeOnKey);
                  // now handle the new received schema registration that won
                  // fill schemaAttributeOnName to add on the schema for this collection
                  schemaAttributeOnName := TWDSchemaAttribute.Create;
                  schemaAttributeOnName.name := schemaAttributeName;
                  schemaAttributeOnName.worldFunctions := schemaAttributeWorldFunction;
                  schemaAttributeOnName.WorldType := schemaAttributeWorldType;
                  schemaAttributeOnName.key := schemaAttributeKey;
                  schemaAttributeOnName.source := schemaSource;
                  // add and overwrite registration
                  schemaCollection.attributesOnName.Add(schemaAttributeOnName.name, schemaAttributeOnName);
                  schemaCollection.attributesOnKey.AddOrSetValue(schemaAttributeOnName.key, schemaAttributeOnName);
                end
                else
                begin // we win
                  // we reject the senders registration
                  // we send our registration to trigger the overruling of the senders registration
                  signalSchemaAttribute(fEvent, schemaAttributeOnName.source, schemaCollectionName, schemaAttributeOnName);
                end;
              end
              else schemaCollection.attributesOnKey.Add(schemaAttributeOnName.key, schemaAttributeOnName);
            end;
          end
          else Log.WriteLn('Received schema attribute without schema name: '+schemaAttributeKey.ToString+'('+schemaAttributeName+', '+schemaCollectionName+')');
        end;
    else
      aBuffer.bb_read_skip(aCursor, key and 7); // 7306=913=bin descr 7322=915=bin ref 7328=916=bin state 7314=914=bin parent
    end;
  end;
end;

procedure TWDDataSources.signalBinCopy(aEventEntry: TEventEntry; const aSrcID, aDstID: TWDID);
begin
  aEventEntry.signalEvent(
    TByteBuffer.bb_tag_rawbytestring(wdatBinSourceID shr 3, aSrcID)+
    TByteBuffer.bb_tag_rawbytestring(wdatBinCopy shr 3, aDstID));
end;

procedure TWDDataSources.signalBinCreate(aEventEntry: TEventEntry; const aBinID: TWDID; const aDescription: string; aParentID, aRefID: TWDID);
begin
  aEventEntry.signalEvent(
    TByteBuffer.bb_tag_string(wdatBinDescription shr 3, aDescription)+
    TByteBuffer.bb_tag_rawbytestring(wdatBinParent shr 3, aParentID)+
    TByteBuffer.bb_tag_rawbytestring(wdatBinRef shr 3, aRefID)+
    TByteBuffer.bb_tag_rawbytestring(wdatBinCreate shr 3, aBinID));
end;

procedure TWDDataSources.signalBinDelete(aEventEntry: TEventEntry; const aBinID: TWDID);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_rawbytestring(wdatBinDelete shr 3, aBinID));
end;

procedure TWDDataSources.signalInquireBins(aEventEntry: TEventEntry; const aReturnEventName: string);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_string(wdatBinsInquire shr 3, aReturnEventName));
end;

procedure TWDDataSources.signalInquireDataSources(aEventEntry: TEventEntry; const aReturnEventName: string);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_string(wdatDataSourcesInquire shr 3, aReturnEventName));
end;

procedure TWDDataSources.signalInquireSchema(aEventEntry: TEventEntry; const aReturnEventName: string);
begin
  aEventEntry.signalEvent(TByteBuffer.bb_tag_string(wdatSchemaInquire shr 3, aReturnEventName));
end;
{
procedure TWDDataSources.signalSchemaAddAttribute(aEventEntry: TEventEntry; const aSchemaCollectionName: string; aAttributeKey: TWDKey; const aAttributeName: string; aWorldType: TWDWorldType);
begin
  aEventEntry.signalEvent(
    TByteBuffer.bb_tag_string(wdatSchemaCollection shr 3, aSchemaCollectionName)+
    TByteBuffer.bb_tag_string(wdatSchemaAttributeName shr 3, aAttributeName)+
    TByteBuffer.bb_tag_int32(wdatSchemaAttributeWorldType shr 3, aWorldType)+
    TByteBuffer.bb_tag_uint32(wdatSchemaAttributeAdd shr 3, aAttributeKey));
end;
}
procedure TWDDataSources.signalSchemaAttribute(aEventEntry: TEventEntry; const aSchemaSource, aSchemaCollectionName: string;
  const aSchemaAttribute: TWDSchemaAttribute);
begin
  aEventEntry.signalEvent(
    TByteBuffer.bb_tag_string(wdatSchemaSource shr 3, aSchemaSource{fPrivateEvent.eventName})+
    TByteBuffer.bb_tag_string(wdatSchemaCollection shr 3, aSchemaCollectionName)+
    TByteBuffer.bb_tag_string(wdatSchemaAttributeName shr 3, aSchemaAttribute.name)+
    TByteBuffer.bb_tag_uint32(wdatSchemaAttributeFunction shr 3, aSchemaAttribute.worldFunctions)+
    TByteBuffer.bb_tag_int32(wdatSchemaAttributeWorldType shr 3, aSchemaAttribute.worldType)+
    TByteBuffer.bb_tag_uint32(wdatSchemaAttributeKey shr 3, aSchemaAttribute.key));
end;

procedure TWDDataSources.signalSchemaAttribute(aEventEntry: TEventEntry; const aSchemaSource, aSchemaCollectionName: string;
  const aSchemaAttribute: TWDClassCacheField);
begin
  aEventEntry.signalEvent(
    TByteBuffer.bb_tag_string(wdatSchemaSource shr 3, aSchemaSource{fPrivateEvent.eventName})+
    TByteBuffer.bb_tag_string(wdatSchemaCollection shr 3, aSchemaCollectionName)+
    TByteBuffer.bb_tag_string(wdatSchemaAttributeName shr 3, aSchemaAttribute.name)+
    TByteBuffer.bb_tag_uint32(wdatSchemaAttributeFunction shr 3, aSchemaAttribute.Functions)+
    TByteBuffer.bb_tag_int32(wdatSchemaAttributeWorldType shr 3, aSchemaAttribute.WType)+
    TByteBuffer.bb_tag_uint32(wdatSchemaAttributeKey shr 3, aSchemaAttribute.key));
end;

end.
