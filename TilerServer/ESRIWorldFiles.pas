unit ESRIWorldFiles;

interface

uses
  MyStr,
  Math, Classes, SysUtils;

type
  TESRIWorldFile = class
  constructor Create                   ( const aFileName: string);
  private
    FFileName                          : string;
    FPixelSizeX                        : Double;
    FRowRotation                       : Double;
    FColumnRotation                    : Double;
    FPixelSizeY                        : Double;
    FUpperLeftPixelCentreX             : Double;
    FUpperLeftPixelCentreY             : Double;
  public
    function  LoadFromFile             ( const aFileName: string=''): Boolean;
    function  SaveToFile               ( const aFileName: string=''): Boolean;
    procedure MoveBy                   ( dx, dy: Double);
    procedure ScaleBy                  ( dx, dy: Double);
    procedure GetExtent                ( const aWidthInPixels, aHeightInPixels: Integer;
                                         var aXMin, aYMin, aXMax, aYMax: Double);
    procedure SetOnExtent              ( const aXMin, aYMin, aXMax, aYMax: Double;
                                         const aWidthInPixels, aHeightInPixels: Integer);
    property FileName: string read FFileName;
    property PixelSizeX: Double read FPixelSizeX;
    property RowRotation: Double read FRowRotation;
    property ColumnRotation: Double read FColumnRotation;
    property PixelSizeY: Double read FPixelSizeY;
    property UpperLeftPixelCentreX: Double read FUpperLeftPixelCentreX;
    property UpperLeftPixelCentreY: Double read FUpperLeftPixelCentreY;
  end;

function  GraphicToWorldFileNameExt    ( const aExtension: string): string;
function  WorldToGraphicFileNameExt    ( const aExtension: string): string;

function  GraphicToWorldFileName       ( const aGraphicFileName: string): string;
function  WorldToGraphicFileName       ( const aWorldFileName: string): string;

function  CreateWorldFile              ( const aGraphicPath: string;
                                         const aXMin, aYMin, aXMax, aYMax: Double;
                                         const aWidthInPixels, aHeightInPixels: Integer): Boolean;

function  IsWorldFileName              ( const aWorldFileName: string): Boolean;
function  IsGraphicFileName            ( const aGraphicFileName: string): Boolean;

implementation

const
  GraphicFileExtentsions: array[0..17] of string =
    ( '.jpg',  // 0
      '.jpeg', // 1
      '.bmp',  // 2
      '.tif',  // 3
      '.png',  // 4
      '.bil',  // 5
      '.dem',  // 6
      '.ecw',  // 7
      '.jpf',  // 8
      '.jpx',  // 9
      '.jpc',  // 10
      '.j2c',  // 11
      '.j2x',  // 12
      '.j2k',  // 13
      '.jp2',  // 14
      '.gif',  // 15
      '.img',  // 16
      '.sid'   // 17
    );

  WorldFileExtentsions: array[0..17] of string =
    ( '.jpw',  // 0
      '.jpw',  // 1
      '.bmw',  // 2
      '.tfw',  // 3
      '.pgw',  // 4
      '.blw',  // 5
      '.dmw',  // 6
      '.eww',  // 7
      '.jfw',  // 8
      '.jxw',  // 9
      '.jcw',  // 10
      '.jcw',  // 11
      '.jxw',  // 12
      '.jkw',  // 13
      '.j2w',  // 14
      '.gfw',  // 15
      '.igw',  // 16
      '.sdw'   // 17
    );

function IsWorldFileName(const aWorldFileName: string): Boolean;
begin
  Result := InList(ExtractFileExt(aWorldFilename), WorldFileExtentsions)>=0;
end;

function IsGraphicFileName(const aGraphicFileName: string): Boolean;
begin
  Result := InList(ExtractFileExt(aGraphicFilename), GraphicFileExtentsions)>=0;
end;

function GraphicToWorldFileNameExt(const aExtension: string): string;
var
  i: Integer;
begin
  i := InList(aExtension, GraphicFileExtentsions);
  if i>=0
  then Result := WorldFileExtentsions[i]
  else Result := '';
end;

function WorldToGraphicFileNameExt(const aExtension: string): string;
var
  i: Integer;
begin
  i := InList(aExtension, WorldFileExtentsions);
  if i>=0
  then Result := GraphicFileExtentsions[i]
  else Result := '';
end;

function GraphicToWorldFileName(const aGraphicFileName: string): string;
var
  Ext: string;
begin
  Ext := GraphicToWorldFileNameExt(ExtractFileExt(aGraphicFileName));
  if Ext<>''
  then Result := ChangeFileExt(aGraphicFileName, Ext)
  else Result := '';
end;

function WorldToGraphicFileName(const aWorldFileName: string): string;
var
  Ext: string;
begin
  Ext := WorldToGraphicFileNameExt(ExtractFileExt(aWorldFileName));
  if Ext<>''
  then Result := ChangeFileExt(aWorldFileName, Ext)
  else Result := '';
end;

function CreateWorldFile(const aGraphicPath: string; const aXMin, aYMin, aXMax, aYMax: Double;
  const aWidthInPixels, aHeightInPixels: Integer): Boolean;
var
  WorldFileName: string;
  WorldFile: TESRIWorldFile;
begin
  WorldFileName := GraphicToWorldFileName(aGraphicPath);
  if WorldFileName<>'' then
  begin
    WorldFile := TESRIWorldFile.Create(WorldFileName);
    try
      WorldFile.SetOnExtent(aXMin, aYMin, aXMax, aYMax, aWidthInPixels, aHeightInPixels);
      try
        WorldFile.SaveToFile;
        Result := True;
      except
        Result := False;
      end;
    finally
      WorldFile.Free;
    end;
  end
  else Result := False;
end;

{ TESRIWorldFile }

constructor TESRIWorldFile.Create(const aFileName: string);
begin
  inherited Create;
  // set defaults
  FPixelSizeX := 1; // Cell size in the "X" direction
  FRowRotation := 0; // always 0
  FColumnRotation := 0; // always 0
  FPixelSizeY := -1; // Cell size in the "Y" direction (always negative)
  FUpperLeftPixelCentreX := 0; // Easting value of insertion point "X"
  FUpperLeftPixelCentreY := 0; // Northing value of insertion point "Y"
  if aFileName<>'' then
  begin
    if FileExists(aFileName)
    then LoadFromFile(aFileName)
    else FFileName := aFileName;
  end
  else FFileName := '';
end;

procedure TESRIWorldFile.GetExtent(const aWidthInPixels, aHeightInPixels: Integer; var aXMin, aYMin, aXMax, aYMax: Double);
begin
  aXMin := FUpperLeftPixelCentreX-0.5*FPixelSizeX;
  aXMax := aXMin+aWidthInPixels*FPixelSizeX;
  // y inverted (ie FPixelSizeY is negative)
  aYMax := FUpperLeftPixelCentreY-0.5*FPixelSizeY;
  aYMin := aYMax+aHeightInPixels*FPixelSizeY;
end;

function TESRIWorldFile.LoadFromFile(const aFileName: string): Boolean;
var
  FormatSettings: TFormatSettings;
  Lines: TStringList;
begin
  if aFileName<>''
  then FFileName := aFileName;
  Result := False;
  if FileExists(FFileName) then
  begin
    Lines := TStringList.Create;
    try
      FormatSettings.DecimalSeparator := '.';
      Lines.LoadFromFile(FFileName);
      if Lines.Count>=6 then
      begin
        FPixelSizeX := StrToFloatDef(Lines[0], NaN, FormatSettings);
        FRowRotation := StrToFloatDef(Lines[1], NaN, FormatSettings);
        FColumnRotation := StrToFloatDef(Lines[2], NaN, FormatSettings);
        FPixelSizeY := StrToFloatDef(Lines[3], NaN, FormatSettings);
        FUpperLeftPixelCentreX := StrToFloatDef(Lines[4], NaN, FormatSettings);
        FUpperLeftPixelCentreY := StrToFloatDef(Lines[5], NaN, FormatSettings);
        Result := True;
      end;
    finally
      Lines.Free;
    end;
  end;
end;

procedure TESRIWorldFile.MoveBy(dx, dy: Double);
begin
  if dx<>0
  then FUpperLeftPixelCentreX := FUpperLeftPixelCentreX+dx;
  if dy<>0
  then FUpperLeftPixelCentreY := FUpperLeftPixelCentreY+dy;
end;

function TESRIWorldFile.SaveToFile(const aFileName: string): Boolean;
var
  FormatSettings: TFormatSettings;
  Lines: TStringList;
begin
  if aFileName<>''
  then FFileName := aFileName;
  Lines := TStringList.Create;
  try
    FormatSettings.DecimalSeparator := '.';
    Lines.Add(FloatToStr(FPixelSizeX, FormatSettings));
    Lines.Add(FloatToStr(FRowRotation, FormatSettings));
    Lines.Add(FloatToStr(FColumnRotation, FormatSettings));
    Lines.Add(FloatToStr(FPixelSizeY, FormatSettings));
    Lines.Add(FloatToStr(FUpperLeftPixelCentreX, FormatSettings));
    Lines.Add(FloatToStr(FUpperLeftPixelCentreY, FormatSettings));
    Lines.SaveToFile(FFileName);
    Result := True;
  finally
    Lines.Free;
  end;
end;

procedure TESRIWorldFile.ScaleBy(dx, dy: Double);
begin
  if dx<>0
  then FPixelSizeX := FPixelSizeX*dx;
  if dy<>0
  then FPixelSizeY := FPixelSizeY*dy;
end;

procedure TESRIWorldFile.SetOnExtent(const aXMin, aYMin, aXMax, aYMax: Double; const aWidthInPixels, aHeightInPixels: Integer);
begin
  FPixelSizeX :=(aXMax-aXMin)/aWidthInPixels; // scale x
  FPixelSizeY := -(aYMax-aYMin)/aHeightInPixels; // scale y (negative)
  FUpperLeftPixelCentreX := aXMin+0.5*FPixelSizeX; // half pixel offset
  FUpperLeftPixelCentreY := aYMax+0.5*FPixelSizeY; // half pixel negative offset because of YMax in negative value of FPixelSizeY
end;


end.
