unit MyStr;

// written by J.A. Cornelissen

interface

uses
  Variants, Classes, SysUtils, Windows;

const
  ccNil = #$00;
  ccTab = #$09;
  ccCR = #$0D;
  ccLF = #$0A;
  ccEOF = #$1A;
  ccEsc = #$1B;
  ccSpace = #$20;
  ccCRLF = ccCR + ccLF; //=sLineBreak;
  vbLF = ccLF;

  htmlCR = '%0D';
  htmlLF = '%0A';
  htmlCRLF = htmlCR+htmlLF;
  htmlTAB = '%09';

  VarNameSepChar = '%';

type
  TValidChars = set of AnsiChar;

function Left(const s: string; n: Integer): string;
function Right(const s: string; n: Integer): string;

function StartsWith(const s, LeftStr: string): Boolean;
function StartsWithList(const s: string; const LeftStrList: array of string): Integer;
function StartsWithCase(const s, LeftStr: string): Boolean;
function EndsWith(const s, RightStr: string): Boolean; overload;
function EndsWith(const s: string; RightStr: array of string): Boolean; overload;
function Contains(const s, InStr: string): Boolean;

function StartStrip(const s, LeftStr: string): string;
function StartStripList(const s: string; const LeftStrList: array of string; out aIndexInList: Integer): string;
function EndStrip(const s, RightStr: string): string;
function StartStripCnt(const s: string; aCnt: Integer): string;
function EndStripCnt(const s: string; aCnt: Integer): string;
function Replace(const s, FindStr, ReplaceStr: string): string;
function ReplaceNoCase(const s, FindStr, ReplaceStr: string): string;
function ReplaceOne(const s, FindStr, ReplaceStr: string): string;
function ReplaceVars(const s: string; aVarList: TStrings): string; overload;
function ReplaceVars(const s: string; const aVarList: array of string): string; overload;
function StripChars(const s: string; aCharList: TValidChars): string;
function StripAllButChars(const s: string; aCharList: TValidChars): string;
// Replace is case sensitive
function UnSpace(const s: string): string;
function UnWhiteSpace(const s: string): string;
function UnSpaceCap(const s: string): string;
function CheckCharsInStr(const s: string; const ValidChars: TValidChars; const ReplaceChar: Char = ccNil): string;

// char at i is removed
procedure SplitAt(const s: string; i: Integer; var LeftStr, RightStr: string); overload;
procedure SplitAt(const s: string; c: Char; var LeftStr, RightStr: string); overload;
procedure SplitAt2(const s: string; const c: string; var LeftStr, RightStr: string);

function BytesToStr(n: Extended): string; overload;
function BytesToStr(n: Int64): string; overload;
function CompareBytesStr(const s1, s2: string): Integer;

// format the string to be at least <Len> charakters long
function FormatLen(const s: string; Len: Integer): string;
function FormatLenPre(const s: string; Len: Integer): string;
function FormatLenMax(const s: string; Len: Integer): string;

function FormatLen0(const s: string; Len: Integer): string; overload
function FormatLen0(i: Integer; Len: Integer): string; overload
function StartStrip0(const s: string): string;
function IsNumeric(const s: string): Boolean;

function Spaces(Len: Integer): string;

function ItemCountToStr(ItemCount: Integer): string;

function InsertF(const SubStr, Dest: string; Index: Integer): string;

function HexToInt(Hex: string): Int64;
function IntToBin(aValue: Integer; aDigits: Integer): string;
function PtrToHex(p: Pointer): string;
function RectToStr(const R: TRect): string;

function HexToBytes(const aHex: string): RawByteString;
function BytesToHex(const aBytes: RawByteString): string;

function VarArrayTostrings(const V: Variant; List: TStrings): Boolean;
function stringsToVarArray(List: TStrings): Variant;
function VarArrayToCommaText(const V: Variant): string;

function DumpWideChar(const W: WideString; s: Integer = 0; l: Integer = 0): string;

// to convert a path from wide to multi byte (UTF-8) to rpeserve special chars
// short version is only to store and use in path manipulation functions,
// path could not exist in "A" file system functions  so use "W" file system functions with expanded path
function ConvertAW(const s: AnsiString): WideString;
function ConvertWA(const s: WideString): AnsiString;

function TInt64(Hi, Lo: DWORD): Int64;

function GetUserNameStr: string;
function GetComputerNameStr: string;

function ExpandUNCFileDrive(const aPath: string): string;
// empty aComputerName means use local computer name (default)
function ExpandUNCFileNameAllways(const aPath: string; const aComputerName: string = ''): string;

function ExtIs(const aPath, aExt: string): Boolean;

function ConvertEscapes(const s: string): string;
function ShowEscapes(const s: string): string;

function InList(const s: string; const aList: array of string): Integer;
function IndexOf(const s: string; const aList: array of string): Integer;
function InListOfObjects(o: TObject; const aList: array of TObject): Integer;
function TranslateList(const s: string; const aSrcList, aDstList: array of string; aUseOrgIfNotFound: Boolean=True): string;

function Push(const s, aValue, aSeparator: string): string;
function PushRev(const s, aValue, aSeparator: string): string;

function Pop(var s: string; const aSeparator: string): string;
function Pop2(var s: string; const aSeparator: string): string;
function PopRev(var s: string; const aSeparator: string): string;
function PopRev2(var s: string; const aSeparator: string): string;
function PopLen(var s: string; n: Integer): string;
procedure PopAll(s, aSeparator: string; aValues: TStrings);

function Parts(const s, aSeparator: string): Integer;

function PosRev(const SubStr, str: string): Integer;

function DefStr(const s, default: string): string;
function TrimSpec(const s, StartStr, EndStr: string): string;
function TrimHooks(const s: string): string;
function TrimQuotes(const s: string): string;

function MySysErrorMessage: string;
function MySysErrorMessage2(aMsgNum: DWORD): string;

function CiscoMACToDefMAC(const aCiscoMAC: string): string;
function DefMACToCiscoMAC(const aDefMAC: string): string;

function NoDelimMACToDefMAC(const aNoDelimMAC: string): string;
function DefMACToNoDelimMAC(const aDefMAC: string): string;

function AllToNoDelimMAC(const aMAC: string): string;

function MakeShareName(const s: string): string;

function FirstCharOr0(const s: string): Char; overload;
function FirstCharOr0(const s: WideString): WideChar; overload;

function StrToIdent(const s: string): string;

function IsUpCase(c: Char): Boolean;
function IsLowCase(c: Char): Boolean;
function LowCase(c: Char): Char;

function UnCamelCaps(const s: string): string;
function CamelCaps(const s: string): string;

function NewGUID: string;

// calc 32 bit checksum for all bytes in AnsiString or magic int depending on compiler define DebugCheckString
function  CheckSum(const s: AnsiString): Integer; inline;

// URIs
function SplitURI(const aURI: string; var aProtocol, aHost, aPort: string): string;
function ProtocolFromURI( const aURI: string): string;
function HostFromURI(const aURI: string): string;
function PortFromURI(const aURI: string): string;
function PathFromURI(const aURI: string): string;
function BuildURI(const aProtocol, aHost: string; const aPort: string=''; const aPath: string=''): string;


implementation

const
  MaxUnitIndex = 4;
  Units: array [0 .. MaxUnitIndex] of string = ('Bytes', 'kB', 'MB', 'GB', 'TB');

  // build a readable form to output a count of bytes with an extension
function BytesToStr(n: Extended): string;
var
  u: Integer;
begin
  Result := '';
  u := 0;
  while (n >= 1024) and (u < MaxUnitIndex) do
  begin
    n := n / 1024;
    u := u + 1;
  end;
  if (n > 10) or (u = 0) then
  begin
    if (n > 100) or (u = 0)
    then Result := FloatToStrF(n, ffNumber, 4, 0) + ' ' + Units[u]
    else Result := FloatToStrF(n, ffNumber, 4, 1) + ' ' + Units[u];
  end
  else Result := FloatToStrF(n, ffNumber, 4, 2) + ' ' + Units[u];
end;

function BytesToStr(n: Int64): string;
var
  RealSize: Extended;
begin
  RealSize := n;
  Result := BytesToStr(RealSize);
end;

function CompareBytesStr(const s1, s2: string): Integer;

  function Decode(const s: string; out n: Extended; out u: Integer): Boolean;
  var
    p: Integer;
    us: string;
  begin
    p := Pos(' ', s);
    if p <> 0 then
    begin
      n := StrToFloat(Copy(s, 1, p - 1));
      us := Copy(s, p + 1, Length(s) - p);
      u := MaxUnitIndex;
      while (u > 0) and (AnsiCompareText(Units[u], us) <> 0) do
        u := u - 1;
      Result := u >= 0;
    end
    else
    begin
      n := 0;
      u := -1;
      Result := False;
    end;
  end;

var
  n1, n2: Extended;
  u1, u2: Integer;
begin
  Decode(s1, n1, u1);
  Decode(s2, n2, u2);
  if u1 < u2
  then Result := -2
  else
  begin
    if u1 > u2
    then Result := 2
    else
    begin // units same
      if n1 < n2
      then Result := -1
      else
      begin
        if n1 > n2
        then Result := 1
        else Result := 0;
      end;
    end;
  end;
end;

// format the string to be at least <Len> charakters long
function FormatLen(const s: string; Len: Integer): string;
begin
  Result := s;
  while Length(Result) < Len
  do Result := Result + ' ';
  while (Length(Result) > Len) and EndsWith(Result, ' ')
  do Result := Copy(Result, 1, Length(Result) - 1);
end;

function FormatLenPre(const s: string; Len: Integer): string;
begin
  Result := s;
  while Length(Result) < Len
  do Result := ' ' + Result;
  while (Length(Result) > Len) and StartsWith(Result, ' ')
  do Delete(Result, 1, 1);
end;

function FormatLenMax(const s: string; Len: Integer): string;
begin
  if Length(s) > Len
  then Result := Copy(Copy(s, 1, Len - 2) + '..', 1, Len)
  else Result := FormatLen(s, Len);
end;

function FormatLen0(const s: string; Len: Integer): string;
begin
  Result := s;
  while Length(Result) < Len
  do Result := '0' + Result;
  while (Length(Result) > Len) and StartsWith(Result, '0')
  do Delete(Result, 1, 1);
end;

function FormatLen0(i: Integer; Len: Integer): string;
begin
  Result := FormatLen0(IntToStr(i), Len);
end;

function StartStrip0(const s: string): string;
begin
  Result := s;
  while (Copy(Result, 1, 1) = '0') and (Length(Result) > 1)
  do Delete(Result, 1, 1);
end;

function IsNumeric(const s: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  i := 1;
  while Result and (i <= Length(s)) do
  begin
    if (s[i] >= '0') and (s[i] <= '9')
    then i := i + 1
    else Result := False;
  end;
end;

function Spaces(Len: Integer): string;
begin
  Result := '';
  while Length(Result) < Len
  do Result := Result + ' ';
end;

function ItemCountToStr(ItemCount: Integer): string;
begin
  if ItemCount = 0
  then Result := 'no items'
  else
  begin
    if ItemCount = 1
    then Result := '1 item'
    else Result := IntToStr(ItemCount) + ' items';
  end;
end;

function InsertF(const SubStr, Dest: string; Index: Integer): string;
begin
  Result := Dest;
  Insert(SubStr, Result, Index);
end;

function HexToInt(Hex: string): Int64;
var
  c: Char;
begin
  Result := 0;
  while Hex <> '' do
  begin
    Result := Result * 16;
    c := UpCase(Hex[1]);
    if c < 'A'
    then Result := Result + (Ord(c) - Ord('0'))
    else Result := Result + (10 + Ord(c) - Ord('A'));
    Delete(Hex, 1, 1);
  end;
end;

function IntToBin(aValue: Integer; aDigits: Integer): string;
begin
  Result := '';
  while (aValue <> 0) or (aDigits > 0) do
  begin
    if (aValue and 1) <> 0
    then Result := '1' + Result
    else Result := '0' + Result;
    aDigits := aDigits - 1;
    aValue := aValue shr 1;
  end;
end;

function PtrToHex(p: Pointer): string;
begin
  Result := IntToHex(Int64(p), 8{SizeOf(p)*2}); // use a minimum of 8 characters
  if Length(Result)>8
  then Insert(':', Result, Length(Result)-8);
end;

function RectToStr(const R: TRect): string;
begin
  Result := IntToStr(R.Left)+','+IntToStr(R.Top)+' x '+
            IntToStr(R.Right)+','+IntToStr(R.Bottom)+' = '+
            IntToStr(R.Right-R.Left+1)+','+IntToStr(R.Bottom-R.Top+1);
end;

function HexToBytes(const aHex: string): RawByteString;
var
  i: Integer;
begin
  SetLength(Result, Length(aHex) div SizeOf(Char));
  for i := 0 to Length(Result)-1
  do Result[i+1] := AnsiChar(StrToInt('$'+Copy(aHex, 1+i*2, 2)));
end;

function BytesToHex(const aBytes: RawByteString): string;

  function IntToHexChar(i: Integer): Char; inline;
  begin
    if (0<=i) and (i<10)
    then Result := Char(Ord('0')+i)
    else if (10<=i) and (i<16)
    then Result := Char(Ord('A')+i-10)
    else Result := '*';
  end;

  function IntToHexCharLow(c: AnsiChar): Char; inline;
  begin
    Result := IntToHexChar(Ord(c) and $F);
  end;

  function IntToHexCharHigh(c: AnsiChar): Char; inline;
  begin
    Result := IntToHexChar((Ord(c) shr 4) and $F);
  end;

var
  i: Integer;
begin
  SetLength(Result, Length(aBytes)*2);
  for i := 0 to Length(aBytes)-1 do
  begin
    Result[1+i*2] := IntToHexCharHigh(aBytes[i+1]);
    Result[1+i*2+1] := IntToHexCharLow(aBytes[i+1]);
  end;
end;

function Left(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s)
    then Result := s
    else Result := Copy(s, 1, n);
  end
  else Result := '';
end;

function Right(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s)
    then Result := s
    else Result := Copy(s, Length(s) - n + 1, n);
  end
  else Result := '';
end;

function StartsWith(const s, LeftStr: string): Boolean;
begin
  Result := AnsiCompareText(Left(s, Length(LeftStr)), LeftStr) = 0;
end;

function StartsWithList(const s: string; const LeftStrList: array of string): Integer;
begin
  Result := high(LeftStrList);
  while (Result >= low(LeftStrList)) and not StartsWith(s, LeftStrList[Result])
  do Result := Result - 1;
end;

function StartsWithCase(const s, LeftStr: string): Boolean;
begin
  Result := Left(s, Length(LeftStr)) = LeftStr;
end;

function EndsWith(const s, RightStr: string): Boolean;
begin
  Result := AnsiCompareText(Right(s, Length(RightStr)), RightStr) = 0;
end;

function EndsWith(const s: string; RightStr: array of string): Boolean;
var
  i: Integer;
begin
  i := low(RightStr);
  while (i <= high(RightStr)) and not EndsWith(s, RightStr[i])
  do i := i + 1;
  Result := i <= high(RightStr);
end;

function Contains(const s, InStr: string): Boolean;
begin
  Result := (s <> '') and (InStr <> '') and (Pos(UpperCase(InStr), UpperCase(s)) > 0);
end;

function StartStrip(const s, LeftStr: string): string;
begin
  if StartsWith(s, LeftStr)
  then Result := StartStripCnt(s, Length(LeftStr))
  else Result := s;
end;

function StartStripList(const s: string; const LeftStrList: array of string; out aIndexInList: Integer): string;
begin
  aIndexInList := StartsWithList(s, LeftStrList);
  if aIndexInList>=0
  then Result := StartStrip(s, LeftStrList[aIndexInList])
  else Result := '';
end;

function EndStrip(const s, RightStr: string): string;
begin
  if EndsWith(s, RightStr)
  then Result := EndStripCnt(s, Length(RightStr))
  else Result := s;
end;

function StartStripCnt(const s: string; aCnt: Integer): string;
begin
  Result := Right(s, Length(s) - aCnt);
end;

function EndStripCnt(const s: string; aCnt: Integer): string;
begin
  Result := Left(s, Length(s) - aCnt);
end;

function Replace(const s, FindStr, ReplaceStr: string): string;
var
  p: Integer;
begin
  Result := s;
  p := Pos(FindStr, Result);
  while p <> 0 do
  begin
    Delete(Result, p, Length(FindStr));
    Insert(ReplaceStr, Result, p);
    p := Pos(FindStr, Result);
  end;
end;

function ReplaceNoCase(const s, FindStr, ReplaceStr: string): string;
var
  p: Integer;
begin
  Result := s;
  p := Pos(UpperCase(FindStr), UpperCase(Result));
  while p <> 0 do
  begin
    Delete(Result, p, Length(FindStr));
    Insert(ReplaceStr, Result, p);
    p := Pos(FindStr, Result);
  end;
end;

function ReplaceOne(const s, FindStr, ReplaceStr: string): string;
var
  p: Integer;
begin
  Result := s;
  p := Pos(FindStr, Result);
  if p <> 0 then
  begin
    Delete(Result, p, Length(FindStr));
    Insert(ReplaceStr, Result, p);
  end;
end;

function FindMarker(const s: string; var i: Integer): Boolean;
begin
  while (i <= Length(s)) and (s[i] <> VarNameSepChar)
  do i := i + 1;
  Result := (i <= Length(s));
end;

function ReplaceVars(const s: string; aVarList: TStrings): string;
var
  b, e: Integer;
  VarName: string;
begin
  Result := s;
  b := 1;
  while b <= Length(Result) do
  begin
    if FindMarker(Result, b) then
    begin
      e := b + 1;
      if FindMarker(Result, e) then
      begin
        if e = b + 1 then
        begin // Not a var but <VarNameSepChar><VarNameSepChar> translate to one <VarNameSepChar>
          Delete(Result, e, 1);
          b := b + 1;
        end
        else
        begin // It is a variable so fill it into the string and signal a succesfull expansion
          VarName := Copy(Result, b + 1, e - b - 1);
          Delete(Result, b, e - b + 1);
          Insert(aVarList.Values[VarName], Result, b);
          b := b + Length(aVarList.Values[VarName]);
        end;
      end
      else b := Length(Result) + 1;
    end
    else b := Length(Result) + 1;
  end;
end;

function ReplaceVars(const s: string; const aVarList: array of string): string;
var
  VarListSL: TStringList;
  i: Integer;
begin
  VarListSL := TStringList.Create;
  try
    for i := low(aVarList) to high(aVarList)
    do VarListSL.Add(aVarList[i]);
    Result := ReplaceVars(s, VarListSL);
  finally
    VarListSL.Free;
  end;
end;

function StripChars(const s: string; aCharList: TValidChars): string;
var
  i: Integer;
begin
  Result := s;
  for i := Length(Result) downto 1 do
  begin
    if AnsiChar(Result[i]) in aCharList
    then Delete(Result, i, 1);
  end;
end;

function StripAllButChars(const s: string; aCharList: TValidChars): string;
var
  i: Integer;
begin
  Result := s;
  for i := Length(Result) downto 1 do
  begin
    if AnsiChar(Result[i]) in aCharList
    then Delete(Result, i, 1);
  end;
end;

function UnSpace(const s: string): string;
var
  p: Integer;
begin
  Result := s;
  p := Pos(' ', Result);
  while p <> 0 do
  begin
    Delete(Result, p, 1);
    p := Pos(' ', Result);
  end;
end;

function UnWhiteSpace(const s: string): string;
begin
  Result := Replace(Replace(Replace(Replace(s, ' ', ''), ccTab, ''), ccCR, ''), ccLF, '');
end;

function UnSpaceCap(const s: string): string;
var
  p: Integer;
begin
  Result := s;
  p := Pos(' ', Result);
  while p <> 0 do
  begin
    Delete(Result, p, 1);
    if p <= Length(Result)
    then Result[p] := UpCase(Result[p]);
    p := Pos(' ', Result);
  end;
end;

function CheckCharsInStr(const s: string; const ValidChars: TValidChars; const ReplaceChar: Char): string;
var
  i: Integer;
begin
  Result := s;
  for i := Length(Result) downto 1 do
  begin
    if not(AnsiChar(Result[i]) in ValidChars) then
    begin
      if ReplaceChar = ccNil
      then Delete(Result, i, 1)
      else Result[i] := ReplaceChar;
    end;
  end;
end;

procedure SplitAt(const s: string; i: Integer; var LeftStr, RightStr: string);
begin
  LeftStr := Left(s, i - 1);
  RightStr := Right(s, Length(s) - i);
end;

procedure SplitAt(const s: string; c: Char; var LeftStr, RightStr: string);
var
  p: Integer;
begin
  p := Pos(c, s);
  if p <> 0
  then SplitAt(s, p, LeftStr, RightStr)
  else
  begin
    LeftStr := s;
    RightStr := '';
  end;
end;

procedure SplitAt2(const s: string; const c: string; var LeftStr, RightStr: string);
var
  p: Integer;
begin
  p := Pos(c, s);
  if p <> 0 then
  begin
    SplitAt(s, p, LeftStr, RightStr);
    if Length(c) > 1
    then Delete(RightStr, 1, Length(c) - 1);
  end
  else
  begin
    LeftStr := s;
    RightStr := '';
  end;
end;

function VarArrayTostrings(const V: Variant; List: TStrings): Boolean;
var
  i: Integer;
begin
  if VarIsArray(V) then
  begin
    for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1)
    do List.Add(V[i]);
    Result := True;
  end
  else
  begin
    if VarIsStr(V) then
    begin
      List.Add(V);
      Result := True;
    end
    else Result := False;
  end;
end;

function stringsToVarArray(List: TStrings): Variant;
var
  i: Integer;
begin
  Result := VarArrayCreate([0, List.Count - 1], varVariant);
  for i := 0 to List.Count - 1
  do Result[i] := List[i];
end;

function VarArrayToCommaText(const V: Variant): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if VarArrayTostrings(V, SL)
    then Result := SL.CommaText
    else Result := '';
  finally
    SL.Free;
  end;
end;

function DumpWideChar(const W: WideString; s: Integer = 0; l: Integer = 0): string;
var
  i: Integer;
begin
  Result := '(' + IntToStr(Length(W)) + ')';
  if s < 1
  then s := 1
  else
  begin
    if s > Length(W)
    then s := Length(W);
  end;
  if l < 1
  then l := Length(W);
  if l > Length(W) + 1 - s
  then l := Length(W) + 1 - s;
  for i := 0 to l - 1
  do Result := Result + ' ' + IntToHex(Ord(W[s + i]), 4);
end;

const
  MaxValueSize = 1024;

function ConvertAW(const s: AnsiString): WideString;
var
  Buffer: array [0 .. MaxValueSize] of WideChar;
  Res: Integer;
begin
  if s <> '' then
  begin
    Res := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(s), Length(s), Buffer, MaxValueSize);
    if Res <> 0
    then Result := Copy(Buffer, 1, Res)
    else Result := '';
  end
  else Result := '';
end;

function ConvertWA(const s: WideString): AnsiString;
var
  Buffer: array [0 .. MaxValueSize] of AnsiChar;
  Res: Integer;
begin
  if s <> '' then
  begin
    Res := WideCharToMultiByte(CP_UTF8, 0, PWideChar(s), Length(s), Buffer, MaxValueSize, nil, nil);
    if Res <> 0
    then Result := Copy(Buffer, 1, Res)
    else Result := '';
  end
  else Result := '';
end;

function TInt64(Hi, Lo: DWORD): Int64;
begin
  Int64Rec(Result).Hi := Hi;
  Int64Rec(Result).Lo := Lo;
end;

function GetUserNameStr: string;
var
  Name: array [0 .. MAX_PATH] of Char;
  s: DWORD;
begin
  s := SizeOf(Name);
  if GetUserName(Name, s)
  then Result := Name
  else Result := '';
end;

function GetComputerNameStr: string;
var
  Name: array [0 .. MAX_PATH] of Char;
  s: DWORD;
begin
  s := SizeOf(Name);
  if GetComputerName(Name, s)
  then Result := Name
  else Result := '';
end;

function ExpandUNCFileDrive(const aPath: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(ExpandUNCFileName(ExtractFileDrive(aPath) + '\'));
end;

function ExpandUNCFileNameAllways(const aPath, aComputerName: string): string;
begin
  // fault in ExpandUNCFileName on x: format
  if Length(aPath) = 2
  then Result := ExpandUNCFileName(aPath + '\')
  else Result := ExpandUNCFileName(aPath);
  if Length(Result) >= 2 then
  begin
    if Result[2] = ':' then
    begin
      Result[2] := '$';
      if aComputerName = ''
      then Result := GetComputerNameStr + '\' + Result
      else Result := aComputerName + '\' + Result;
      while Copy(Result, 1, 2) <> '\\'
      do Result := '\' + Result;
    end;
    // extra check if origianl had trailing backslash (spec. for unc drive path)
    if aPath[Length(aPath)] <> '\'
    then Result := ExcludeTrailingPathDelimiter(Result);
  end;
end;

function ExtIs(const aPath, aExt: string): Boolean;
begin
  Result := AnsiCompareText(ExtractFileExt(aPath), aExt) = 0;
end;

function ConvertEscapes(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    case UpCase(s[i]) of
      '`', '0' .. '9', '-', '=', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 'A' .. 'Z',
        ' ', '[', ']', '\', '{', '}', '|', ';', '''', ':', '"', ',', '.', '/', '<', '>', '?':
        Result := Result + s[i];
      ccCR:
        Result := Result + '<CR>';
      ccLF:
        Result := Result + '<LF>';
      ccTab:
        Result := Result + '<TAB>';
      ccNil:
        Result := Result + '<0>';
    else
      Result := Result + '<' + IntToHex(Ord(s[i]), 2) + '>';
    end;
  end;
end;

function ShowEscapes(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(s) do
  begin
    case UpCase(s[i]) of
      '`', '0' .. '9', '-', '=', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 'A' .. 'Z',
        ' ', '[', ']', '\', '{', '}', '|', ';', '''', ':', '"', ',', '.', '/', '<', '>', '?':
        ;
      ccCR:
        begin
          if (i < Length(s)) and (s[i + 1] = ccLF) then
          begin
            Result := Result + '<CR><LF>' + ccCR;
            i := i + 1;
          end
          else Result := Result + '<CR>';
        end;
      ccLF:
        Result := Result + '<LF>';
      ccTab:
        Result := Result + '<TAB>';
      ccNil:
        Result := Result + '<0>';
    else
      Result := Result + '<' + IntToHex(Ord(s[i]), 2) + '>';
    end;
    Result := Result + s[i];
    i := i + 1;
  end;
end;

function InList(const s: string; const aList: array of string): Integer;
begin
  Result := high(aList);
  while (Result >= low(aList)) and (AnsiCompareText(s, aList[Result]) <> 0)
  do Result := Result - 1;
end;

function IndexOf(const s: string; const aList: array of string): Integer;
begin
  Result := InList(s, aList);
end;

function InListOfObjects(o: TObject; const aList: array of TObject): Integer;
begin
  Result:=High(aList);
  while (Result>=Low(aList)) AND (o<>aList[Result])
  do Result:=Result-1;
end;

function TranslateList(const s: string; const aSrcList, aDstList: array of string; aUseOrgIfNotFound: Boolean): string;
var
  i: Integer;
begin
  i := InList(s, aSrcList);
  if i>=0 then
  begin
    if i<Length(aDstList)
    then Result := aDstList[i]
    else Result := ''; // empty if no entry in aDstList! does not take aUseOrgIfNotFound
  end
  else
  begin
    if aUseOrgIfNotFound
    then Result := s
    else Result := '';
  end;
end;

function Push(const s, aValue, aSeparator: string): string;
begin
  if s <> ''
  then Result := s + aSeparator + aValue
  else Result := aValue;
end;

function PushRev(const s, aValue, aSeparator: string): string;
begin
  if s <> ''
  then Result := aValue + aSeparator + s
  else Result := aValue;
end;

function Pop(var s: string; const aSeparator: string): string;
var
  p: Integer;
begin
  p := Pos(aSeparator, s);
  if p <> 0 then
  begin
    Result := Copy(s, 1, p - 1);
    Delete(s, 1, p + Length(aSeparator) - 1);
  end
  else Result := '';
end;

function Pop2(var s: string; const aSeparator: string): string;
var
  p: Integer;
begin
  p := Pos(aSeparator, s);
  if p <> 0 then
  begin
    Result := Copy(s, 1, p - 1);
    Delete(s, 1, p + Length(aSeparator) - 1);
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

function PosRev(const SubStr, str: string): Integer;
begin
  if (SubStr <> '') and (str <> '') then
  begin
    Result := Length(str) - Length(SubStr) + 1;
    while (Result > 0) and (Copy(str, Result, Length(SubStr)) <> SubStr)
    do Result := Result - 1;
  end
  else Result := 0;
end;

function PopRev(var s: string; const aSeparator: string): string;
// pop from end of list
var
  p: Integer;
begin
  p := PosRev(aSeparator, s);
  if p <> 0 then
  begin
    Result := StartStripCnt(s, p + Length(aSeparator) - 1);
    s := Left(s, p - 1);
  end
  else Result := '';
end;

function PopRev2(var s: string; const aSeparator: string): string;
// pop from end of list
var
  p: Integer;
begin
  p := PosRev(aSeparator, s);
  if p <> 0 then
  begin
    Result := StartStripCnt(s, p + Length(aSeparator) - 1);
    s := Left(s, p - 1);
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

function PopLen(var s: string; n: Integer): string;
begin
  if n < Length(s) then
  begin
    Result := Left(s, n);
    s := Right(s, Length(s) - n)
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

function Parts(const s, aSeparator: string): Integer;
var
  i: Integer;
begin
  if s <> '' then
  begin
    Result := 1;
    i := 1;
    while i <= Length(s) do
    begin
      if Copy(s, i, Length(aSeparator)) = aSeparator then
      begin
        Result := Result + 1;
        i := i + Length(aSeparator);
      end
      else i := i + 1;
    end;
  end
  else Result := 0;
end;

procedure PopAll(s, aSeparator: string; aValues: TStrings);
var
  Value: string;
begin
  if s <> '' then
  begin
    repeat
      Value := Pop2(s, aSeparator);
      aValues.Add(Value);
    until s = '';
  end;
end;

function DefStr(const s, default: string): string;
begin
  if s = ''
  then Result := default
  else Result := s;
end;

function TrimSpec(const s, StartStr, EndStr: string): string;
begin
  Result := Trim(s);
  if StartsWith(Result, StartStr) and EndsWith(Result, EndStr) then
  begin
    Result := StartStrip(Result, StartStr);
    Result := EndStrip(Result, EndStr);
  end;
end;

function TrimHooks(const s: string): string;
begin
  Result := TrimSpec(s, '(', ')');
  Result := TrimSpec(Result, '{', '}');
  Result := TrimSpec(Result, '[', ']');
  Result := TrimSpec(Result, '<', '>');
end;

function TrimQuotes(const s: string): string;
begin
  Result := TrimSpec(s, '''', '''');
  Result := TrimSpec(Result, '"', '"');
end;

function MySysErrorMessage2(aMsgNum: DWORD): string;
begin
  Result := SysErrorMessage(aMsgNum) + ' (' + IntToHex(aMsgNum, 8) + ', ' + IntToStr(aMsgNum) + ')';
end;

function MySysErrorMessage: string;
begin
  Result := MySysErrorMessage2(GetLastError);
end;

function CiscoMACToDefMAC(const aCiscoMAC: string): string;
begin // 0030.0569.908f -> 00-30-05-69-90-8f
  Result := Copy(aCiscoMAC, 1, 2) + '-' + Copy(aCiscoMAC, 3, 2) + '-' + Copy(aCiscoMAC, 6, 2) + '-' + Copy
    (aCiscoMAC, 8, 2) + '-' + Copy(aCiscoMAC, 11, 2) + '-' + Copy(aCiscoMAC, 13, 2);
end;

function DefMACToCiscoMAC(const aDefMAC: string): string;
begin // 00-30-05-69-90-8f -> 0030.0569.908f
  Result := Copy(aDefMAC, 1, 2) + Copy(aDefMAC, 4, 2) + '.' + Copy(aDefMAC, 7, 2) + Copy(aDefMAC, 10, 2)
    + '.' + Copy(aDefMAC, 13, 2) + Copy(aDefMAC, 16, 2);
end;

function NoDelimMACToDefMAC(const aNoDelimMAC: string): string;
begin // 00300569908f -> 00-30-05-69-90-8F
  Result := UpperCase(Copy(aNoDelimMAC, 1, 2) + '-' + Copy(aNoDelimMAC, 3, 2) + '-' + Copy(aNoDelimMAC, 5,
      2) + '-' + Copy(aNoDelimMAC, 7, 2) + '-' + Copy(aNoDelimMAC, 9, 2) + '-' + Copy(aNoDelimMAC, 11, 2));
end;

function DefMACToNoDelimMAC(const aDefMAC: string): string;
begin // 00-30-05-69-90-8f -> 00300569908f
  Result := Copy(aDefMAC, 1, 2) + Copy(aDefMAC, 4, 2) + Copy(aDefMAC, 7, 2) + Copy(aDefMAC, 10, 2) + Copy
    (aDefMAC, 13, 2) + Copy(aDefMAC, 16, 2);
end;

function AllToNoDelimMAC(const aMAC: string): string;
begin
  Result := CheckCharsInStr(UpperCase(aMAC), ['0' .. '9', 'A' .. 'F']);
end;

function MakeShareName(const s: string): string;
begin
  Result := Copy(CheckCharsInStr(UnSpaceCap(s), ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '-', '_'], '_'), 1, 32);
end;

function FirstCharOr0(const s: string): Char;
begin
  if s <> ''
  then Result := s[1]
  else Result := ccNil;
end;

function FirstCharOr0(const s: WideString): WideChar;
begin
  if s <> ''
  then Result := s[1]
  else Result := ccNil;
end;

function StrToIdent(const s: string): string;
var
  i: Integer;
begin
  Result := Trim(s);
  // remove/replace invalid chars
  for i := Length(Result) downto 1 do
  begin
    case Result[i] of
      '%':
        begin
          Delete(Result, i, 1);
          Insert('Prct', Result, i);
        end;
      '(',')', '"', '''', '.':
        begin
          Delete(Result, i, 1);
        end;
      'a'..'z':;
      'A'..'Z':;
      '0'..'9':;
    else //' ', '_', '-':
      //camel caps
      Delete(Result, i, 1);
      Result[i] := UpCase(Result[i]);
    end;
  end;
  // remove double underscores
  Result := Replace(Result, '__', '_');
  // check for starting with number
  if Result<>'' then
  begin
    case Result[1] of
      '0': Result := 'Zero'+StartStripCnt(Result, 1);
      '1': Result := 'One'+StartStripCnt(Result, 1);
      '2': Result := 'Two'+StartStripCnt(Result, 1);
      '3': Result := 'Three'+StartStripCnt(Result, 1);
      '4': Result := 'Four'+StartStripCnt(Result, 1);
      '5': Result := 'Five'+StartStripCnt(Result, 1);
      '6': Result := 'Six'+StartStripCnt(Result, 1);
      '7': Result := 'Seven'+StartStripCnt(Result, 1);
      '8': Result := 'Eight'+StartStripCnt(Result, 1);
      '9': Result := 'Nine'+StartStripCnt(Result, 1);
    end;
  end;
end;

function IsUpCase(c: Char): Boolean;
begin
  Result := ('A' <= c) and (c <= 'Z');
end;

function IsLowCase(c: Char): Boolean;
begin
  Result := ('a' <= c) and (c <= 'z');
end;

function LowCase(c: Char): Char;
begin
  if IsUpCase(c)
  then Result := Chr(Ord(c)-Ord('A')+Ord('a'))
  else Result :=c;
end;

function UnCamelCaps(const s: string): string;
var
  i: Integer;
begin
  Result := s;
  for i := Length(Result)-1 downto 3 do
  begin
    {
      if this is upper case
        if next is lower case or previous is lower case
        then insert space
        if next is lower case
        then make lower case
    }
    if IsUpCase(Result[i]) then
    begin
      if IsLowCase(Result[i-1]) or (i=Length(Result)) or IsLowCase(Result[i+1]) then
      begin
        if (i=Length(Result)) or IsLowCase(Result[i+1])
        then Result[i] := LowCase(Result[i]);
        Insert(' ', Result, i);
      end;
    end;
  end;
end;

function CamelCaps(const s: string): string;
var
  i: Integer;
begin
  Result := Trim(s);
  if Result<>'' then
  begin
    Result[1] := UpCase(Result[1]);
    for i := Length(Result)-1 downto 2 do
    begin
      if Result[i]=' ' then
      begin
        Delete(Result, i, 1);
        Result[i] := UpCase(Result[i]);
      end;
    end;
  end;
end;

function NewGUID: string;
var
  GUID: TGUID;
begin
  if CreateGUID(GUID)=S_OK
  then Result := GuidToString(GUID)
  else Result := '';
end;

function SplitURI(const aURI: string; var aProtocol, aHost, aPort: string): string;
begin
  Result:=aURI;
  aProtocol:=Pop(Result, '://');
  aHost:=Pop2(Result, '/');
  aPort:=PopRev(aHost, ':');
  aHost:=TrimSpec(aHost,'[',']');
end;

function ProtocolFromURI(const aURI: string): string;
var
  Host, Port: string;
begin
  SplitURI(aURI, Result, Host, Port);
end;

function HostFromURI(const aURI: string): string;
var
  Protocol, Port: string;
begin
  SplitURI(aURI, Protocol, Result, Port);
end;

function PortFromURI(const aURI: string): string;
var
  Protocol, Host: string;
begin
  SplitURI(aURI, Protocol, Host, Result);
end;

function PathFromURI(const aURI: string): string;
var
  Protocol, Host, Port: string;
begin
  Result:=SplitURI(aURI, Protocol, Host, Port);
end;

function BuildURI(const aProtocol, aHost, aPort, aPath: string): string;
begin
  if Pos(':', aHost)<>0
  then Result:=aProtocol+'://'+'['+aHost+']'
  else Result:=aProtocol+'://'+aHost;
  if aPort<>''
  then Result:=Result+':'+aPort;
  if aPath<>''
  then Result:=Result+'/'+aPath;
end;

{$R-}{$Q-}
function CheckSum(const s: AnsiString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Length(s)
  do Result := Result+Byte(s[i]); // no checking because of over flow in result (wrap)
end;
{$R+}{$Q+}


end.
