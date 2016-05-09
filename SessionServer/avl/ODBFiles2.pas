unit ODBFiles2;

// Delphi XE2, TAlphaColor (different byte ordering and alpha channel)

// http://blogs.ugidotnet.org/GisSharpBlog/archive/2005/01/13/9215.aspx


interface

uses
  MyStr,
  UITypes,
  Classes,
  Math,
  SysUtils;

type
  TODBRecord = record
    Color                              : TAlphaColor;
    Description                        : String;
    Min                                : Double;
    Max                                : Double;
    IsNoData                           : Boolean;
  end;

  TODBList                             = array of TODBRecord;


function  ODBFileToODBList             ( const aFileName: String; aAlpha: Byte=$FF): TODBList; overload;
function  ODBFileToODBList             ( aODBFile: TStrings; aAlpha: Byte=$FF): TODBList; overload;
procedure ODBListToAltitudeMapZones    ( aODBList: TODBList; aAltitudeMapZones: TStrings);
procedure AddNoValueToAltitudeMapZones ( aAltitudeMapZones: TStrings; aNoValue: Double; aColor: TAlphaColor);
function  ODBValueToColor              ( aODBList: TODBList; aValue: Double; aDefaultColor: TAlphaColor): TAlphaColor;
function  ODBListToFile                ( aODBList: TODBList; const aFileName: string): Boolean;
function  ODBListToStream              ( aODBList: TODBList; aStream: TStream): Boolean;

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

function GetColor(aODBFile: TStrings; ci: Integer; aAlpha: Byte): TAlphaColor;
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
  Result := (Blue shr 8)+((Green shr 8) shl 8)+((Red shr 8) shl 16)+(aAlpha shl 24);
end;

function GetLabelMinMax(aODBFile: TStrings; ci: Integer; var aDescription: String; var aMin, aMax: Double; var aIsNoData: Boolean): Boolean;
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
    Result := True;
  end
  else Result := False;
end;

function ODBFileToODBList(const aFileName: String; aAlpha: Byte): TODBList;
var
  ODBFile: TStringList;
begin
  ODBFile := TStringList.Create;
  try
    ODBFile.LoadFromFile(aFileName);
    Result := ODBFileToODBList(ODBFile, aAlpha);
  finally
    ODBFile.Free;
  end;
end;

function ODBFileToODBList(aODBFile: TStrings; aAlpha: Byte): TODBList;
var
  ClassCount: Integer;
  ci: Integer;
  ClassID: Integer;
  ChildID: Integer;
  ColorID: Integer;
  c: Integer;
begin
  ClassCount := GetClassCount(aODBFile);
  SetLength(Result, ClassCount);
  ci := 0;
  for c := 0 to ClassCount - 1 do
  begin
    ClassID := GetClassID(aODBFile, ci);
    ChildID := GetChildID(aODBFile, ci);
    ColorID := GetColorID(aODBFile, ChildID);
    Result[ci].Color := GetColor(aODBFile, ColorID, aAlpha);
    Result[ci].Min := NaN;
    Result[ci].Max := NaN;
    Result[ci].IsNoData := False;
    Result[ci].Description := '';
    if GetLabelMinMax(aODBFile, ClassID, Result[ci].Description, Result[ci].Min, Result[ci].Max, Result[ci].IsNoData)
    then ci := ci+1;
  end;
  // remove invalid classes
  if ci<ClassCount
  then SetLength(Result, ci);
end;

procedure ODBListToAltitudeMapZones(aODBList: TODBList; aAltitudeMapZones: TStrings);
var
  FloatFormat: TFormatsettings;
  i: Integer;
begin
  FloatFormat.DecimalSeparator := '.';
  for i := 0 to Length(aODBList) - 1 do
  begin
    aAltitudeMapZones.Add(
      FloatToStr(aODBList[i].Min, FloatFormat)+','+
      FloatToStr(aODBList[i].Max, FloatFormat)+','+
      //ColorToString(aODBList[i].Color)+','+
      '$'+IntToHex(TColorRec.ColorToRGB(aODBList[i].Color),8)+','+
      aODBList[i].Description
    );
  end;
end;

procedure AddNoValueToAltitudeMapZones(aAltitudeMapZones: TStrings; aNoValue: Double; aColor: TAlphaColor);
var
  FloatFormat: TFormatsettings;
begin
  FloatFormat.DecimalSeparator := '.';
  aAltitudeMapZones.Insert(0,
    FloatToStr(aNoValue, FloatFormat)+','+
    FloatToStr(aNoValue, FloatFormat)+','+
    '$'+IntToHex(TColorRec.ColorToRGB(aColor), 8)
  );
end;

function ODBValueToColor(aODBList: TODBList; aValue: Double; aDefaultColor: TAlphaColor): TAlphaColor;
var
  i: Integer;
begin
  i := 0;
  Result := aDefaultColor;
  if not IsNaN(aValue) then
  begin
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
end;


const
  ValuePrefix = '      ';

procedure WriteSectionHeader(aSL: TStrings; aSection: string; aSectionID: Integer);
begin
  aSL.Add('('+aSection+'.'+IntToStr(aSectionID));
end;

procedure WriteSectionValueInQuotes(aSL: TStrings; const aValueName, aValue: string);
begin
  aSL.Add(ValuePrefix + aValueName + ':' + ccTab + '"' + aValue + '"');
end;

procedure WriteSectionValue(aSL: TStrings; const aValueName, aValue: string); overload;
begin
  aSL.Add(ValuePrefix + aValueName + ':' + ccTab + aValue);
end;

procedure WriteSectionValue(aSL: TStrings; const aValueName: string; aValue: Integer); overload;
begin
  aSL.Add(ValuePrefix + aValueName + ':' + ccTab + IntToStr(aValue));
end;

procedure WriteSectionValue(aSL: TStrings; const aValueName: string; aValue: Double); overload;
var
  FloatFormat: TFormatSettings;
begin
  FloatFormat.DecimalSeparator := '.';
  aSL.Add(ValuePrefix + aValueName + ':' + ccTab + FloatToStr(aValue, FloatFormat));
end;

procedure WriteSectionFooter(aSL: TStrings);
begin
  aSL.Add(')');
  aSL.Add('');
end;

function ODBListToFile(aODBList: TODBList; const aFileName: string): Boolean;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('/3.2');
    WriteSectionHeader(SL, 'ODB', 1);
    WriteSectionValueInQuotes(SL, 'FirstRootClassName', 'Legend');
    WriteSectionValue(SL, 'Roots', 2);
    WriteSectionValue(SL, 'Version', 32);
    WriteSectionFooter(SL);

    WriteSectionHeader(SL, 'Legend', 2);
    WriteSectionValue(SL, 'Symbols', 3);
    for i := 0 to Length(aODBList)-1
    do WriteSectionValue(SL, 'Class', (6 + i * 3));
    WriteSectionFooter(SL);

    WriteSectionHeader(SL, 'SymList', 3);
    for i := 0 to Length(aODBList)-1
    do WriteSectionValue(SL, 'Child', (4 + i * 3));
    WriteSectionFooter(SL);

    for i := 0 to Length(aODBList)-1 do
    begin
      WriteSectionHeader(SL, 'BShSym', 4 + i * 3);
      WriteSectionValue(SL, 'Color', (5 + i * 3));
      WriteSectionFooter(SL);

      WriteSectionHeader(SL, 'TClr', 5 + i * 3);

      WriteSectionValue(SL, 'Red', '0x' + IntToHex(TAlphaColorRec(aODBList[i].color).R, 2) + '00');
      WriteSectionValue(SL, 'Green', '0x' + IntToHex(TAlphaColorRec(aODBList[i].color).G, 2) + '00');
      WriteSectionValue(SL, 'Blue', '0x' + IntToHex(TAlphaColorRec(aODBList[i].color).B, 2) + '00');
      WriteSectionFooter(SL);

      WriteSectionHeader(SL, 'LClass', 6 + i * 3);
      WriteSectionValueInQuotes(SL, 'Label', aODBList[i].description);
      if not IsNaN(aODBList[i].min)
      then WriteSectionValue(SL, 'MinNum', aODBList[i].min);
      if not IsNaN(aODBList[i].max)
      then WriteSectionValue(SL, 'MaxNum', aODBList[i].max);
      if aODBList[i].isNoData
      then WriteSectionValue(SL, 'IsNoData', 1);
      WriteSectionFooter(SL);
    end;
    SL.SaveToFile(aFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ODBListToStream(aODBList: TODBList; aStream: TStream): Boolean;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('/3.2');
    WriteSectionHeader(SL, 'ODB', 1);
    WriteSectionValueInQuotes(SL, 'FirstRootClassName', 'Legend');
    WriteSectionValue(SL, 'Roots', 2);
    WriteSectionValue(SL, 'Version', 32);
    WriteSectionFooter(SL);

    WriteSectionHeader(SL, 'Legend', 2);
    WriteSectionValue(SL, 'Symbols', 3);
    for i := 0 to Length(aODBList)-1
    do WriteSectionValue(SL, 'Class', (6 + i * 3));
    WriteSectionFooter(SL);

    WriteSectionHeader(SL, 'SymList', 3);
    for i := 0 to Length(aODBList)-1
    do WriteSectionValue(SL, 'Child', (4 + i * 3));
    WriteSectionFooter(SL);

    for i := 0 to Length(aODBList)-1 do
    begin
      WriteSectionHeader(SL, 'BShSym', 4 + i * 3);
      WriteSectionValue(SL, 'Color', (5 + i * 3));
      WriteSectionFooter(SL);

      WriteSectionHeader(SL, 'TClr', 5 + i * 3);

      WriteSectionValue(SL, 'Red', '0x' + IntToHex(TAlphaColorRec(aODBList[i].color).R, 2) + '00');
      WriteSectionValue(SL, 'Green', '0x' + IntToHex(TAlphaColorRec(aODBList[i].color).G, 2) + '00');
      WriteSectionValue(SL, 'Blue', '0x' + IntToHex(TAlphaColorRec(aODBList[i].color).B, 2) + '00');
      WriteSectionFooter(SL);

      WriteSectionHeader(SL, 'LClass', 6 + i * 3);
      WriteSectionValueInQuotes(SL, 'Label', aODBList[i].description);
      if not IsNaN(aODBList[i].min)
      then WriteSectionValue(SL, 'MinNum', aODBList[i].min);
      if not IsNaN(aODBList[i].max)
      then WriteSectionValue(SL, 'MaxNum', aODBList[i].max);
      if aODBList[i].isNoData
      then WriteSectionValue(SL, 'IsNoData', 1);
      WriteSectionFooter(SL);
    end;
    SL.SaveToStream(aStream);
    Result := True;
  finally
    SL.Free;
  end;
end;

end.
