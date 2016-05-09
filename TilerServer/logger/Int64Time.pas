unit Int64Time;

// written by J.A. Cornelissen

{ Int64 time = TFileTime
  Int64 time resolution is 100 nanoseconds
}

interface

uses
  SysUtils, Windows;

const
  InvalidInt64Time                     = 0;
  InvalidDTTime                        = 0;

  UniversalDateFormat                  = 'yyyy-mm-dd';
  UniversalTimeFormat                  = 'hh:nn:ss';
  UniversalTimemsFormat                = 'hh:nn:ss.zzz';
  UniversalDateTimeFormat              = UniversalDateFormat+' '+UniversalTimeFormat;
  USADateTimeFormat                    = 'm"/"d"/"yyyy h:nn:ss am/pm';

  itOneMicroSecond                     = Int64(10);
  itOneMilliSecond                     = Int64(itOneMicroSecond*1000);
  itOneSecond                          = Int64(itOneMilliSecond*1000);
  itOneMinute                          = Int64(itOneSecond*60);
  itOneHour                            = Int64(itOneMinute*60);
  itOneDay                             = Int64(itOneHour*24);

var
  UniversalFormatSettings              : TFormatSettings;
  UniversalFormatSettingsms            : TFormatSettings;


function  NowInt64                     : Int64;
function  NowUTCInt64                  : Int64;
function  Int64ToDateTime              ( aInt64FT: Int64): TDateTime;
function  DateTimeToInt64              ( aDT: TDateTime): Int64;
function  CompareInt64Time             ( aInt64a, aInt64b: Int64): Integer;
function  Int64SysToLocal              ( aInt64SysTime: Int64): Int64;
function  Int64LocalToSys              ( aInt64LocalTime: Int64): Int64;

function  Int64ToUniversal             ( aInt64FT: Int64): String;
function  Int64ToUSA                   ( aInt64FT: Int64): String;

function  Int64DeltaToTDateTimeDelta   ( aInt64FTDelta: Int64): TDateTime;
function  TDateTimeDeltaToInt64Delta   ( aDTDelta: TDateTime): Int64;


implementation

function NowInt64: Int64;
var
  ST: TSystemTime;
begin
  GetLocalTime(ST);
  if NOT SystemTimeToFileTime(ST,TFileTime(Result))
  then Result:=InvalidInt64Time;
end;

function NowUTCInt64: Int64;
var
  ST: TSystemTime;
begin
  GetSystemTime(ST);
  if NOT SystemTimeToFileTime(ST,TFileTime(Result))
  then Result:=InvalidInt64Time;
end;

function Int64ToDateTime(aInt64FT: Int64): TDateTime;
var
  ST:TSystemTime;
begin
  if (aInt64FT<>InvalidInt64Time) AND FileTimeToSystemTime(TFileTime(aInt64FT),ST)
  then Result:=SystemTimeToDateTime(ST)
  else Result:=InvalidDTTime;
end;

function DateTimeToInt64(aDT: TDateTime): Int64;
var
  ST:TSystemTime;
begin
  if aDT<>InvalidDTTime then
  begin
    DateTimeToSystemTime(aDT,ST);
    if NOT SystemTimeToFileTime(ST,TFileTime(Result))
    then Result:=InvalidInt64Time;
  end
  else Result:=InvalidInt64Time;
end;

function CompareInt64Time(aInt64a, aInt64b: Int64): Integer;
begin
  Result:=CompareFileTime(TFileTime(aInt64a),TFileTime(aInt64b));
end;

function Int64ToUniversal(aInt64FT: Int64): String;
begin
  if aInt64FT>=0 then
  begin
    if aInt64FT<10*itOneDay
    then Result := FormatDateTime(UniversalTimeFormat,Int64ToDateTime(aInt64FT))
    else Result := FormatDateTime(UniversalDateTimeFormat,Int64ToDateTime(aInt64FT));
  end
  else Result := '';
end;

function Int64ToUSA(aInt64FT: Int64): String;
begin
  Result:=FormatDateTime(USADateTimeFormat,Int64ToDateTime(aInt64FT))
end;

function Int64SysToLocal(aInt64SysTime: Int64): Int64;
begin
  if NOT FileTimeToLocalFileTime(TFileTime(aInt64SysTime),TFileTime(Result))
  then Result:=InvalidInt64Time;
end;

function  Int64LocalToSys(aInt64LocalTime: Int64): Int64;
begin
  if NOT LocalFileTimeToFileTime(TFileTime(aInt64LocalTime),TFileTime(Result))
  then Result:=InvalidInt64Time;
end;

function Int64DeltaToTDateTimeDelta(aInt64FTDelta: Int64): TDateTime;
begin
  Result := aInt64FTDelta/itOneDay;
end;

function TDateTimeDeltaToInt64Delta(aDTDelta: TDateTime): Int64;
begin
  Result := Round(aDTDelta*itOneDay);
end;

initialization
  with UniversalFormatSettings do
  begin
    CurrencyString := '';
    CurrencyFormat := 0;
    CurrencyDecimals := 2;
    DateSeparator := '-';
    TimeSeparator := ':';
    ListSeparator := ';';
    ShortDateFormat := 'yyyy-mm-dd';
    LongDateFormat := 'yyyy-mm-dd';
    TimeAMString := '';
    TimePMString := '';
    ShortTimeFormat := 'hh:nn:ss';
    LongTimeFormat := 'hh:nn:ss';
    ThousandSeparator := '.';
    DecimalSeparator := '.';
    NegCurrFormat := 0;
  end;
  UniversalFormatSettingsms := UniversalFormatSettings;
  with UniversalFormatSettingsms do
  begin
    ShortTimeFormat := 'hh:nn:ss.zzz';
    LongTimeFormat := 'hh:nn:ss.zzz';
  end;
end.
