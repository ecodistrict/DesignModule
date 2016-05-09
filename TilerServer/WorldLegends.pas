unit WorldLegends;

interface

uses
  WorldDataCode,
  imb4,
  System.SysUtils, System.Math;

type
  // [minValue, maxValue>
  TDiscretePaletteEntry = record
    color: TAlphaRGBPixel;
    minValue: Double;
    maxValue: Double;
    description: string;
    function ValueInRange(const aValue: Double): Boolean;
    function Encode: TWDValue;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
  end;

  TPaletteDiscreteEntryArray = array of TDiscretePaletteEntry;

  TDiscretePalette = class(TWDPalette)
  constructor Create(const aDescription: string; const aEntries: TPaletteDiscreteEntryArray; aNoDataColor: TAlphaRGBPixel); overload;
  destructor Destroy; override;
  class function WorldType: TWDWorldType; override;
  class function wdTag: UInt32; override;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  private
    fEntries: TPaletteDiscreteEntryArray;
    fNoDataColor: TAlphaRGBPixel;
  protected
    function _clone: TWDPalette; override;
  public
    property entries: TPaletteDiscreteEntryArray read fEntries;
    property noDataColor: TAlphaRGBPixel read fNoDataColor;
    function ValueToColor(const aValue: Double): TAlphaRGBPixel; override;
  end;

  TRampPaletteEntry = record
  class function Create(aColor: TAlphaRGBPixel; aValue: Double; const aDescription: string): TRampPaletteEntry; static;
  public
    color: TAlphaRGBPixel;
    value: Double;
    description: string;
    function Encode: TWDValue;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
  end;

  TPaletteRampEntryArray = array of TRampPaletteEntry;

  // values in entries: low to high
  TRampPalette = class(TWDPalette)
  constructor Create(const aDescription: string; const aEntries: TPaletteRampEntryArray; aLowerDataColor, aNoDataColor, aHigherDataColor: TAlphaRGBPixel); overload;
  destructor Destroy; override;
  class function WorldType: TWDWorldType; override;
  class function wdTag: UInt32; override;
  public
    function Encode: TWDValue; override;
    procedure Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer); override;
  private
    fEntries: TPaletteRampEntryArray;
    fLowerDataColor: TAlphaRGBPixel;
    fNoDataColor: TAlphaRGBPixel;
    fHigherDataColor: TAlphaRGBPixel;
  protected
    function _clone: TWDPalette; override;
  public
    function ValueToColor(const aValue: Double): TAlphaRGBPixel; override;
  end;

implementation

function RampByte(aFraction: Double; aLow, aHigh: Byte): Cardinal;
begin
  Result := Round(aLow+aFraction*(aHigh-aLow));
end;

function ColorRamp(aValue, aLowValue, aHighValue: Double; aLowColor, aHighColor: TAlphaRGBPixel): TAlphaRGBPixel;
var
  f: Double;
begin
  f := (aValue-aLowValue)/(aHighValue-aLowValue);
  Result :=
    (RampByte(f, (aLowColor shr  0) and $FF, (aHighColor shr  0) and $FF) shl  0) or
    (RampByte(f, (aLowColor shr  8) and $FF, (aHighColor shr  8) and $FF) shl  8) or
    (RampByte(f, (aLowColor shr 16) and $FF, (aHighColor shr 16) and $FF) shl 16) or
    (RampByte(f, (aLowColor shr 24) and $FF, (aHighColor shr 24) and $FF) shl 24);
end;

{ TDiscretePaletteEntry }

procedure TDiscretePaletteEntry.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
begin
  color := aBuffer.bb_read_uint32(aCursor);
  minValue := aBuffer.bb_read_double(aCursor);
  maxValue := aBuffer.bb_read_double(aCursor);
  description := aBuffer.bb_read_string(aCursor);
  if aCursor>aLimit
  then Exception.Create('TPaletteDiscreteEntry.Decode read over limit ('+aCursor.toString+', '+aLimit.toString);
end;

function TDiscretePaletteEntry.Encode: TWDValue;
begin
  Result :=
    TByteBuffer.bb_uint32(color)+
    TByteBuffer.bb_bytes(minValue, SizeOf(minValue))+
    TByteBuffer.bb_bytes(maxValue, SizeOf(maxValue))+
    TByteBuffer.bb_string(description);
end;

function TDiscretePaletteEntry.ValueInRange(const aValue: Double): Boolean;
begin
  Result := (IsNaN(MinValue) or (MinValue <= aValue)) and (IsNaN(MaxValue) or (aValue < MaxValue));
end;

{ TDiscretePalette }

constructor TDiscretePalette.Create(const aDescription: string; const aEntries: TPaletteDiscreteEntryArray; aNoDataColor: TAlphaRGBPixel);
begin
  inherited Create(aDescription);
  fEntries := aEntries;
  fNoDataColor := aNoDataColor;
end;

procedure TDiscretePalette.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
begin
  inherited Decode(aBuffer, aCursor, aLimit);
  fNoDataColor := aBuffer.bb_read_uint32(aCursor);
  while aCursor<aLimit do
  begin
    setLength(fEntries, length(fEntries)+1);
    fEntries[length(fEntries)-1].Decode(aBuffer, aCursor, aLimit);
  end;
end;

destructor TDiscretePalette.Destroy;
begin
  setLength(fEntries, 0);
  inherited;
end;

function TDiscretePalette.Encode: TWDValue;
var
  i: Integer;
begin
  Result := inherited Encode;
  Result := Result+TByteBuffer.bb_uint32(fNoDataColor);
  for i := 0 to length(fEntries)-1
  do Result := Result+fEntries[i].Encode;
end;

function TDiscretePalette.ValueToColor(const aValue: Double): TAlphaRGBPixel;
var
  e: Integer;
begin
  if not IsNaN(aValue) then
  begin
    e := Length(fEntries)-1;
    while (e>=0) and not fEntries[e].ValueInRange(aValue)
    do e := e-1;
    if e>=0
    then Result := fEntries[e].Color
    else Result := fNoDataColor;
  end
  else Result := fNoDataColor;
end;

class function TDiscretePalette.wdTag: UInt32;
begin
  Result := icehDiscretePalette;
end;

class function TDiscretePalette.WorldType: TWDWorldType;
begin
  Result := wdkPaletteDiscrete;
end;

function TDiscretePalette._clone: TWDPalette;
begin
  Result := TDiscretePalette.Create(fDescription);
  (Result as  TDiscretePalette).fEntries := fEntries;
  (Result as  TDiscretePalette).fNoDataColor := fNoDataColor;
end;

{ TRampPaletteEntry }

class function TRampPaletteEntry.Create(aColor: TAlphaRGBPixel; aValue: Double; const aDescription: string): TRampPaletteEntry;
begin
  Result.color := aColor;
  Result.value := aValue;
  Result.description := aDescription;
end;

procedure TRampPaletteEntry.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
begin
  color := aBuffer.bb_read_uint32(aCursor);
  value := aBuffer.bb_read_double(aCursor);
  description := aBuffer.bb_read_string(aCursor);
  if aCursor>aLimit
  then Exception.Create('TPaletteRampEntry.Decode read over limit ('+aCursor.toString+', '+aLimit.toString);
end;

function TRampPaletteEntry.Encode: TWDValue;
begin
  Result := Result+
    TByteBuffer.bb_uint32(color)+
    TByteBuffer.bb_bytes(value, SizeOf(value))+
    TByteBuffer.bb_string(description);
end;

{ TRampPalette }

constructor TRampPalette.Create(const aDescription: string; const aEntries: TPaletteRampEntryArray; aLowerDataColor, aNoDataColor, aHigherDataColor: TAlphaRGBPixel);
begin
  inherited Create(aDescription);
  fEntries := aEntries;
  fLowerDataColor := aHigherDataColor;
  fNoDataColor := aNoDataColor;
  fHigherDataColor := aLowerDataColor;
end;

procedure TRampPalette.Decode(const aBuffer: TWDValue; var aCursor: Integer; aLimit: Integer);
begin
  inherited Decode(aBuffer, aCursor, aLimit);
  fLowerDataColor := aBuffer.bb_read_uint32(aCursor);
  fNoDataColor := aBuffer.bb_read_uint32(aCursor);
  fHigherDataColor := aBuffer.bb_read_uint32(aCursor);
  while aCursor<aLimit do
  begin
    setLength(fEntries, length(fEntries)+1);
    fEntries[length(fEntries)-1].Decode(aBuffer, aCursor, aLimit);
  end;
end;

destructor TRampPalette.Destroy;
begin
  setLength(fEntries, 0);
  inherited;
end;

function TRampPalette.Encode: TWDValue;
var
  i: Integer;
begin
  Result := inherited Encode;
  Result := Result+
    TByteBuffer.bb_uint32(fLowerDataColor)+
    TByteBuffer.bb_uint32(fNoDataColor)+
    TByteBuffer.bb_uint32(fHigherDataColor);
  for i := 0 to length(fEntries)-1
  do Result := Result+fEntries[i].Encode;
end;

function TRampPalette.ValueToColor(const aValue: Double): TAlphaRGBPixel;
var
  e: Integer;
begin
  if not IsNaN(aValue) then
  begin
    e := Length(fEntries)-1;
    while (e>=0) and (aValue<fEntries[e].value)
    do e := e-1;
    if e>=0 then
    begin
      if e<Length(fEntries)-1 then
      begin
        if not SameValue(aValue, fEntries[e].Value)
        then Result := ColorRamp(aValue, fEntries[e].Value, fEntries[e+1].Value, fEntries[e].Color, fEntries[e+1].Color)
        else Result := fEntries[e].color;
      end
      else Result := fHigherDataColor;
    end
    else Result := fLowerDataColor;
  end
  else Result := fNoDataColor;
end;

class function TRampPalette.wdTag: UInt32;
begin
  Result := icehRampPalette;
end;

class function TRampPalette.WorldType: TWDWorldType;
begin
  Result := wdkPaletteRamp;
end;

function TRampPalette._clone: TWDPalette;
begin
  Result := TRampPalette.Create(fDescription);
  (Result as TRampPalette).fEntries := fEntries;
  (Result as TRampPalette).fLowerDataColor := fLowerDataColor;
  (Result as TRampPalette).fNoDataColor := fNoDataColor;
  (Result as TRampPalette).fHigherDataColor := fHigherDataColor;
end;

end.
