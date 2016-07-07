unit ODBToPNG;

interface

uses
  ODBFIles2,
  Vcl.Imaging.PNGImage,
  UITypes,
  Math;

function CreatePNGFromODB(const aODBList: TODBList; out aMin, aMax, aStep: Double; out aTotalStepCount: Integer): TPNGImage;

implementation

type
  TDoubles = array of Double;
  TIntegers = array of Integer;

function ReadValues(const aODBList: TODBList): TDoubles;
var
  v: Integer;
begin
  // build list of all values
  Setlength(Result, Length(aODBList)*2);
  for v := 0 to Length(aODBList)-1 do
  begin
    if aODBList[v].IsNoData then
    begin
      Result[v*2] := NaN;
      Result[v*2+1] := NaN;
    end
    else
    begin
      Result[v*2] := aODBList[v].Min;
      Result[v*2+1] := aODBList[v].Max;
    end;
  end;
end;

function CompareValues(d1, d2: Double): Boolean;
// return true: put higher in list ie d1>d2
// makes sure that NaNs go at end of list
begin
  if IsNaN(d1) then
  begin // d1 = NaN
    if IsNaN(d2)
    then Result := False
    else Result := True;
  end
  else
  begin // d1 not NaN
    if IsNaN(d2)
    then Result := False
    else Result := d1>d2;
  end;
end;

procedure SortValues(var aValues: TDoubles);
// sorts list low to high and puts NaNs at end of list
var
  v: Integer;
  Sorted: Boolean;
  t: Double;
begin
  repeat
    Sorted := True;
    for v := 0 to Length(aValues)-2 do
    begin
      if aValues[v]>aValues[v+1] then
      begin
        t := aValues[v];
        aValues[v] := aValues[v+1];
        aValues[v+1] := t;
        Sorted := False;
      end;
    end;
  until Sorted;
end;

procedure RemoveNaNsAndDoubleValues(var aValues: TDoubles);
// pre: list is sorted and NaN values are at end of list
var
  v: Integer;
  i: Integer;
begin
  // remove NaNs from end of list
  v := Length(aValues);
  while (v>0) and IsNaN(aValues[v-1])
  do v := v-1;
  SetLength(aValues, v);
  v := 1;
  while v<Length(aValues) do
  begin
    if SameValue(aValues[v], aValues[v-1]) then
    begin // remove value
      for i := v to Length(aValues)-2
      do aValues[i] := aValues[i+1];
      SetLength(aValues, Length(aValues)-1);
    end
    else v := v+1;
  end;
end;

function CalculateSteps(const aValues: TDoubles; out aFactor: Double; out aSmallestStep: Integer): TIntegers;
var
  v: Integer;
  d: Double;
begin
  aFactor := 1; // minimal aFactor on step to get whole numbers
  // calculate minimal aFactor over steps between aValues
  for v := 0 to Length(aValues)-2 do
  begin
    d := (aValues[v+1]-aValues[v])*aFactor;
    while not IsZero(Round(d)-d, d/1000000000) do
    begin
      aFactor := aFactor*10;
      d := (aValues[v+1]-aValues[v])*aFactor;
    end;
  end;
  aSmallestStep := MaxInt;
  Setlength(Result, Length(aValues)-1);
  for v := 0 to Length(Result)-1 do
  begin
    Result[v] := Round((aValues[v+1]-aValues[v])*aFactor);
    if aSmallestStep>Result[v]
    then aSmallestStep := Result[v];
  end;
end;

function CanDevideSteps(const aSteps: TIntegers; f: Integer): Boolean;
var
  i: Integer;
begin
  i := Length(aSteps)-1;
  while (i>=0) and ((aSteps[i] mod f)=0)
  do i := i-1;
  Result := i<0;
end;

procedure DevideSteps(var aSteps: TIntegers; f: Integer);
var
  s: Integer;
begin
  for s := 0 to Length(aSteps)-1
  do aSteps[s] := aSteps[s] div f;
end;

procedure OptimizeStep(var aSteps: TIntegers; aSmallestStep: Integer; aFactor: Double; out aStep: Double);
var
  f: Integer;
begin
  f := 2;
  while aSmallestStep>1 do
  begin
    if (aSmallestStep mod f)=0 then
    begin
      aSmallestStep := aSmallestStep div f;
      if CanDevideSteps(aSteps, f) then
      begin
        DevideSteps(aSteps, f);
        aFactor := aFactor/f;
      end;
    end
    else f := f+1;
  end;
  aStep := 1/aFactor;
end;

function CreatePNGFromODB(const aODBList: TODBList; out aMin, aMax, aStep: Double; out aTotalStepCount: Integer): TPNGImage;
var
  v: Integer;
  Values: TDoubles;
  Steps: TIntegers;
  SmallestStep: Integer;
  Factor: Double;
  ac: TAlphaColor;
  c: TColor;
begin
  Values := ReadValues(aODBList);
  SortValues(Values);
  RemoveNaNsAndDoubleValues(Values);
  if Length(Values)>0 then
  begin
    aMin := Values[0];
    aMax := Values[Length(Values)-1];
  end
  else
  begin
    aMin := 0;
    aMax := 0;
  end;
  Steps := CalculateSteps(Values, Factor, SmallestStep);
  OptimizeStep(Steps, SmallestStep, Factor, aStep);
  aTotalStepCount := 0;
  for v := 0 to Length(Steps)-1
  do aTotalStepCount := aTotalStepCount+Steps[v];
  if aTotalStepCount>0 then
  begin
    Result := TPNGImage.CreateBlank(COLOR_RGB, 8, aTotalStepCount, 1);
    // TODO: optimize to process step counts instead of single steps
    for v := 0 to aTotalStepCount-1 do
    begin
      // pick point half way step to avoid rounding problems
      ac := ODBValueToColor(aODBList, aMin+(v+0.5)*aStep, TAlphaColorRec.Null);
      TColorRec(c).R := TAlphaColorRec(ac).R;
      TColorRec(c).G := TAlphaColorRec(ac).G;
      TColorRec(c).B := TAlphaColorRec(ac).B;
      Result.Pixels[v, 0] := c;
    end;
  end
  else Result := nil;
end;


end.