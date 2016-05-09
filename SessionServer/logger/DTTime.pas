unit DTTime;

interface

uses
  Math, Windows, SysUtils;

const
  dtOneHour                            = 1/24;
  dtOneMinute                          = 1/(60*24);
  dtOneSecond                          = 1/(60*60*24);
  dtOneMilliSecond                     = dtOneSecond/1000;
  dtOneMicroSecond                     = dtOneSecond/(1000*1000);
  dtOneNanoSecond                      = dtOneSecond/(1000*1000*1000);

// build a readable form to output for a diference in 2 times (delta) to show a duration of this time
function DurationToStr(Delta: TDateTime): string;
function StrToDuration(const aStr: string): TDateTime;

function DurationToShortStr(Delta: TDateTime): string;
function DurationToShortStr2(Delta: TDateTime): string;

function HighResDurationToStr(Delta: TDateTime): string;

function NowUTC: TDateTime;

function TDateTimeTimePart(aDateTime: TDateTime): TTime;
function TDateTimeDatePart(aDateTime: TDateTime): TDate;
function CombineDateTime(const aDatePart: TDate; const aTimePart: TTime): TDateTime;

implementation

function Left(const s: string; n: Integer): string;
begin
  if n > 0 then
  begin
    if n >= Length(s) then
      Result := s
    else
      Result := Copy(s, 1, n);
  end
  else
    Result := '';
end;

function StartsWith(const s, LeftStr: string): Boolean;
begin
  Result := AnsiCompareText(Left(s, Length(LeftStr)), LeftStr) = 0;
end;

function FormatLen0(const s: string; Len: Integer): string;
begin
  Result := s;
  while Length(Result) < Len do
    Result := '0' + Result;
  while (Length(Result) > Len) and StartsWith(Result, '0') do
    Delete(Result, 1, 1);
end;


// build a readable form to output for a diference in 2 times (delta) to show a duration of this time
function DurationToStr(Delta: TDateTime): string;

  procedure LokAdd(a: Integer; const TimeUnit: string);
  begin
    if a >= 1 then
    begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + IntToStr(a) + ' ' + TimeUnit;
      // add s after time unit if more then one
      if a >= 2 then
        Result := Result + 's';
    end;
  end;

begin
  if Delta < 1 / (24 * 60 * 60 * 2) then
    Result := '< 1 second'
  else
  begin
    if Delta < 0 then
      Result := '## ERROR, negative duration! (- ' + DurationToStr(-Delta) + ')'
    else
    begin
      Result := '';
      LokAdd(Trunc(Abs(Delta)), 'day');
      LokAdd(Trunc(Frac(Abs(Delta)) * 24), 'hour');
      LokAdd(Trunc(Frac(Abs(Delta) * 24) * 60), 'minute');
      LokAdd(Round(Frac(Abs((Delta * 24) * 60)) * 60), 'second');
    end;
  end;
end;

function StrToDuration(const aStr: string): TDateTime;

  function DecodeNum(const aLokStr: string; var i: Integer): Integer;
  begin
    // find first num
    while (i <= Length(aLokStr)) and ((aLokStr[i] < '0') or (aLokStr[i] > '9')) do
      i := i + 1;
    // find number from here
    if i <= Length(aLokStr) then
    begin // we have a number
      Result := 0;
      // decode all chars that are a number till end or nu number found
      while (i <= Length(aLokStr)) and ((aLokStr[i] >= '0') and (aLokStr[i] <= '9')) do
      begin
        Result := (Result * 10) + (Ord(aLokStr[i]) - Ord('0'));
        i := i + 1;
      end;
    end
    else
      Result := 0; // no number found
  end;

var
  i: Integer;
  H, M, s, MSec: Integer;
begin
  if aStr <> '' then
  begin
    i := 1;
    H := DecodeNum(aStr, i);
    if i <= Length(aStr) then
    begin
      M := DecodeNum(aStr, i);
      if i <= Length(aStr) then
      begin
        s := DecodeNum(aStr, i);
        if i <= Length(aStr) then
          MSec := DecodeNum(aStr, i)
        else
          MSec := 0;
      end
      else
      begin
        s := 0;
        MSec := 0;
      end;
    end
    else
    begin
      M := 0;
      s := 0;
      MSec := 0;
    end;
    // WriteLn('H:',H,' M:',M,' S:',S,' MSec:',MSec);
    Result := H / 24 + M / (24 * 60) + s / (24 * 60 * 60) + (MSec / (24 * 60 * 60)) / 1000;
  end
  else
    Result := 0;
end;

function DurationToShortStr(Delta: TDateTime): string;

  procedure LokAdd(a, l: Integer; const TimeSep: string);
  begin
    if (l > 0) or (a > 0) then
      Result := Result + FormatLen0(IntToStr(a), l) + TimeSep;
  end;

begin
  if Delta < 1 / (24 * 60 * 60 * 2) then
    Result := '< 1 sec'
  else
  begin
    if Delta < 0 then
      Result := '## ERROR, negative duration! (- ' + DurationToStr(-Delta) + ')'
    else
    begin
      Result := '';
      LokAdd(Trunc(Abs(Delta)), 0, ' day ');
      LokAdd(Trunc(Frac(Abs(Delta)) * 24), 0, ':');
      LokAdd(Trunc(Frac(Abs(Delta) * 24) * 60), 2, ':');
      LokAdd(Round(Frac(Abs((Delta * 24) * 60)) * 60), 2, '');
    end;
  end;
end;

function DurationToShortStr2(Delta: TDateTime): string;
var
  res: string;

  function LokAdd(a: Integer; const TimeSep: string): Boolean;
  begin
    if a > 0 then
    begin
      res := res + IntToStr(a) + TimeSep;
      Result := True;
    end
    else Result := False;
  end;

var
  c: Integer;
begin
  if Delta < 1 / (24 * 60 * 60 * 2) then
    res := '< 1 sec'
  else
  begin
    if Delta < 0 then
      res := '## ERROR, negative duration! (- ' + DurationToStr(-Delta) + ')'
    else
    begin
      res := '';
      c := 0;
      if LokAdd(Trunc(Abs(Delta)), 'd ') then Inc(c);
      if LokAdd(Trunc(Frac(Abs(Delta)) * 24), 'h ') then Inc(c);
      if LokAdd(Trunc(Frac(Abs(Delta) * 24) * 60), 'm ') then Inc(c);
      if c<3 then LokAdd(Round(Frac(Abs((Delta * 24) * 60)) * 60), 's ');
    end;
  end;
  Result := res.Trim;
end;

function HighResDurationToStr(Delta: TDateTime): string;
var
  Factor: Integer;
begin
  if Delta>(30/(24*60*60)) // 30 seconds
  then Result := DurationToStr(Delta)
  else
  begin
    // calc in seconds
    Delta := Delta*24*60*60;
    if Delta<>0 then
    begin
      Factor := Floor(Log10(Delta));
      if Factor>0
      then Result := FloatToStrF(Delta, ffFixed, 5, 1)+' seconds'
      else
      begin
        case Factor of
           0: Result := FloatToStrF(Delta, ffFixed, 5, 2)+' seconds';
          -1: Result := FloatToStrF(Delta*1000, ffFixed, 5, 0)+' milliseconds';
          -2: Result := FloatToStrF(Delta*1000, ffFixed, 5, 1)+' milliseconds';
          -3: Result := FloatToStrF(Delta*1000, ffFixed, 5, 2)+' milliseconds';
          -4: Result := FloatToStrF(Delta*1000000, ffFixed, 5, 0)+' microseconds';
          -5: Result := FloatToStrF(Delta*1000000, ffFixed, 5, 1)+' microseconds';
          -6: Result := FloatToStrF(Delta*1000000, ffFixed, 5, 2)+' microseconds';
        else
          Result := FloatToStrF(Delta*1000000000, ffFixed, 5, 1)+' nanoseconds';
        end;
      end;
      // debug: Result := Result+' ('+IntToStr(Factor)+')';
    end
    else Result := '< 0 nanoseconds';
  end;
end;

function NowUTC: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

function TDateTimeTimePart(aDateTime: TDateTime): TTime;
begin
  Result := Frac(aDateTime);
end;

function TDateTimeDatePart(aDateTime: TDateTime): TDate;
begin
  Result := Trunc(aDateTime);
end;

function CombineDateTime(const aDatePart: TDate; const aTimePart: TTime): TDateTime;
begin
  Result := TDateTimeDatePart(aDatePart)+TDateTimeTimePart(aTimePart);
end;

end.
