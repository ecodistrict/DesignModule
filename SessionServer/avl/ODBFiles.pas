unit ODBFiles;

// Delphi 2010 and earlier (TColor has no alpha channel)

// http://blogs.ugidotnet.org/GisSharpBlog/archive/2005/01/13/9215.aspx

interface

uses
  MyStr,
  Graphics, // TColor
  Windows,
  Classes, // TStringList
  Math,
  SysUtils;

type
  TODBRecord = record
    Color                              : TColor;
    Description                        : String;
    Min                                : Double;
    Max                                : Double;
    IsNoData                           : Boolean;
  end;

  TODBList                             = array of TODBRecord;


function  ODBFileToODBList             ( const aFileName: String): TODBList; overload;
function  ODBFileToODBList             ( aODBFile: TStrings): TODBList; overload;
procedure ODBListToAltitudeMapZones    ( aODBList: TODBList; aAltitudeMapZones: TStrings; aDefaultMin: Double=MinDouble; aDefaultMax: Double=MaxDouble);
procedure AddNoValueToAltitudeMapZones ( aAltitudeMapZones: TStrings; aNoValue: Double; aColor: TColor);
function ODBValueToColor               ( aODBList: TODBList; aValue: Double; aDefaultColor: TColor): TColor;

implementation


function StartOfSection(aODBFile: TStrings; const aSection: String; aPartial: Boolean=False): Integer;
begin
  Result := aODBFile.Count-1;
  if aPartial then
  begin
    while (Result>=0) and not StartsWith(aODBFile[Result], aSection)
    do Result := Result-1;
  end
  else
  begin
    while (Result>=0) and not (AnsiCompareText(aODBFile[Result], aSection)=0)
    do Result := Result-1;
  end;
end;

function EndOfSection(aODBFile: TStrings; aStartOfSection: Integer): Integer;
begin
  Result := aStartOfSection;
  while (Result<aODBFile.Count) and (Trim(aODBFile[Result])<>')')
  do Result := Result+1;
end;

function GetClassID(aODBFile: TStrings; ci: Integer): Integer;
var
  s, e, i: Integer;
begin
  Result := 0;
  ci := ci+1;
  s := StartOfSection(aODBFile, '(Legend.', True);
  if s>=0 then
  begin
    e := EndOfSection(aODBFile, s);
    i := s;
    while (i<e) and (ci>0) do
    begin
      i := i+1;
      if Contains(aODBFile[i], 'Class:')
      then ci := ci-1;
    end;
    if ci=0
    then Result := StrToIntDef(Trim(StartStrip(Trim(aODBFile[i]), 'Class:')), 0);
  end;
end;

function GetClassCount(aODBFile: TStrings): Integer;
var
  s, e, i: Integer;
begin
  Result := 0;
  s := StartOfSection(aODBFile, '(Legend.', True);
  if s>=0 then
  begin
    e := EndOfSection(aODBFile, s);
    for i := s to e do
    begin
      if Contains(aODBFile[i], 'Class:')
      then Result := Result+1;
    end;
  end;
end;

function GetChildID(aODBFile: TStrings; ci: Integer): Integer;
var
  s, e, i: Integer;
begin
  Result := 0;
  ci := ci+1;
  s := StartOfSection(aODBFile, '(SymList.', True);
  if s>=0 then
  begin
    e := EndOfSection(aODBFile, s);
    i := s;
    while (i<e) and (ci>0) do
    begin
      i := i+1;
      if Contains(aODBFile[i], 'Child:')
      then ci := ci-1;
    end;
    if ci=0
    then Result := StrToIntDef(Trim(StartStrip(Trim(aODBFile[i]), 'Child:')), 0);
  end;
end;

function GetColorID(aODBFile: TStrings; ci: Integer): Integer;
var
  s, e, i: Integer;
begin
  Result := 0;
  s := StartOfSection(aODBFile, '(BShSym.'+IntToStr(ci));
  if s>=0 then
  begin
    e := EndOfSection(aODBFile, s);
    i := s;
    while (i<e) and not Contains(aODBFile[i], 'Color:')
    do i := i+1;
    Result := StrToIntDef(Trim(StartStrip(Trim(aODBFile[i]), 'Color:')), 0);
  end;
end;

function GetColor(aODBFile: TStrings; ci: Integer): TColor;
var
  s, e, i: Integer;
  Red: Integer;
  Green: Integer;
  Blue: Integer;
begin
  Red := 0;
  Green := 0;
  Blue := 0;
  s := StartOfSection(aODBFile, '(TClr.'+IntToStr(ci));
  if s>=0 then
  begin
    e := EndOfSection(aODBFile, s);
    for i := s to e do
    begin
      if Contains(aODBFile[i], 'Red:')
      then Red  := StrToIntDef(Trim(StartStrip(Trim(aODBFile[i]), 'Red:')), 0);
      if Contains(aODBFile[i], 'Green:')
      then Green  := StrToIntDef(Trim(StartStrip(Trim(aODBFile[i]), 'Green:')), 0);
      if Contains(aODBFile[i], 'Blue:')
      then Blue  := StrToIntDef(Trim(StartStrip(Trim(aODBFile[i]), 'Blue:')), 0);
    end;
  end;
  Result := RGB(Red shr 8, Green shr 8, Blue shr 8);
end;

procedure GetLabelMinMax(aODBFile: TStrings; ci: Integer; var aDescription: String; var aMin, aMax: Double; var aIsNoData: Boolean);
var
  s, e, i: Integer;
  FloatFormat: TFormatsettings;
begin
  FloatFormat.DecimalSeparator := '.';
  s := StartOfSection(aODBFile, '(LClass.'+IntToStr(ci));
  if s>=0 then
  begin
    e := EndOfSection(aODBFile, s);
    for i := s to e do
    begin
      if Contains(aODBFile[i], 'Label:')
      then aDescription  := AnsiDequotedStr(Trim(StartStrip(Trim(aODBFile[i]), 'Label:')),'"');
      if Contains(aODBFile[i], 'MinNum:')
      then aMin  := StrToFloatDef(Trim(StartStrip(Trim(aODBFile[i]), 'MinNum:')), 0, FloatFormat);
      if Contains(aODBFile[i], 'MaxNum:')
      then aMax  := StrToFloatDef(Trim(StartStrip(Trim(aODBFile[i]), 'MaxNum:')), 0, FloatFormat);
      if Contains(aODBFile[i], 'IsNoData:')
      then aIsNoData := Trim(StartStrip(Trim(aODBFile[i]), 'IsNoData:'))='1';
    end;
  end;
end;

function ODBFileToODBList(const aFileName: String): TODBList;
var
  ODBFile: TStringList;
begin
  ODBFile := TStringList.Create;
  try
    // not needed to use unicode because TStringList has default ansi encoding!
    //ODBFile.LoadFromFile(aFileName, TEncoding.Unicode);
    ODBFile.LoadFromFile(aFileName);
    Result := ODBFileToODBList(ODBFile);
  finally
    ODBFile.Free;
  end;
end;

function ODBFileToODBList(aODBFile: TStrings): TODBList;
var
  ClassCount: Integer;
  ci: Integer;
  ClassID: Integer;
  ChildID: Integer;
  ColorID: Integer;
begin
  ClassCount := GetClassCount(aODBFile);
  SetLength(Result, ClassCount);
  for ci := 0 to ClassCount - 1 do
  begin
    ClassID := GetClassID(aODBFile, ci);
    ChildID := GetChildID(aODBFile, ci);
    ColorID := GetColorID(aODBFile, ChildID);
    Result[ci].Color := GetColor(aODBFile, ColorID);
    Result[ci].Min := NaN;
    Result[ci].Max := NaN;
    Result[ci].IsNoData := False;
    Result[ci].Description := '';
    GetLabelMinMax(aODBFile, ClassID, Result[ci].Description, Result[ci].Min, Result[ci].Max, Result[ci].IsNoData);
  end;
end;

function DefaultOnNaN(aValue, aDefault: Double): Double;
begin
  if not IsNaN(aValue)
  then Result := aValue
  else Result := aDefault
end;

procedure ODBListToAltitudeMapZones(aODBList: TODBList; aAltitudeMapZones: TStrings; aDefaultMin, aDefaultMax: Double);
var
  FloatFormat: TFormatsettings;
  i: Integer;
begin
  FloatFormat.DecimalSeparator := '.';
  for i := 0 to Length(aODBList) - 1 do
  begin
    aAltitudeMapZones.Add(
      FloatToStr(DefaultOnNaN(aODBList[i].Min, aDefaultMin), FloatFormat)+','+
      FloatToStr(DefaultOnNaN(aODBList[i].Max, aDefaultMax), FloatFormat)+','+
      //ColorToString(aODBList[i].Color)+','+
      '$'+IntToHex(ColorToRGB(aODBList[i].Color),8)+','+
      aODBList[i].Description
    );
  end;
end;

procedure AddNoValueToAltitudeMapZones(aAltitudeMapZones: TStrings; aNoValue: Double; aColor: TColor);
var
  FloatFormat: TFormatsettings;
begin
  FloatFormat.DecimalSeparator := '.';
  aAltitudeMapZones.Insert(0,
    FloatToStr(aNoValue, FloatFormat)+','+
    FloatToStr(aNoValue, FloatFormat)+','+
    '$'+IntToHex(ColorToRGB(aColor), 8)
  );
end;

function ODBValueToColor(aODBList: TODBList; aValue: Double; aDefaultColor: TColor): TColor;
var
  i: Integer;
begin
  i := 0;
  Result := aDefaultColor;
  while i<Length(aODBList) do
  begin
    if (IsNaN(aODBList[i].Min) or (aODBList[i].Min<=aValue)) and
       (IsNaN(aODBList[i].Max) or (aValue<aODBList[i].Max)) then
    begin
      Result := aODBList[i].Color;
      i := Length(aODBList);
    end
    else i := i+1;
  end;

end;

end.
