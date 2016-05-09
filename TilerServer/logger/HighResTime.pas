unit HighResTime;

interface

uses
  Int64Time, DTTime, Windows;

const
  InvalidHighResTick                   = -1;

function  HighResTickResolution        : TDateTime;
function  HighResTickFrequency         : Int64;
function  HighResTick                  : Int64;
function  HighResTickDurationSeconds   ( aHighResStartTick: Int64; aHighResEndTick: Int64=InvalidHighResTick): Double;
function  HighResTickDurationStr       ( aHighResStartTick: Int64; aHighResEndTick: Int64=InvalidHighResTick): string;

type
  THighResTimer = record
  private
    fStartTick: Int64;
    fStopTick: Int64;
  public
    procedure Start;
    procedure Stop;
    function DurationInSeconds: Double;
    function DurationStr: string;
    function DurationInTicks: Int64;
  end;

  THighResAverage = record
  private
    fTotalTicks: Extended;
    fCount: Integer;
  public
    procedure Clear;
    // add a measurement
    procedure Add(aTimer: THighResTimer);
    // retrieve
    function DurationInSeconds: Double;
    function DurationStr: string;
    function DurationInTicks: Int64;

    property TotalTicks: Extended read fTotalTicks;
    property Count: Integer  read fCount;
  end;

implementation

var
  hrtResolution: TDateTime=0;
  hrtFrequency: Int64=0;

function HighResTickResolution: TDateTime;
begin
  if hrtResolution=0
  then hrtResolution := dtOneSecond/HighResTickFrequency;
  Result := hrtResolution;
end;

function HighResTickFrequency: Int64;
begin
  if hrtFrequency=0
  then QueryPerformanceFrequency(hrtFrequency);
  Result := hrtFrequency;
end;

function HighResTick: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function HighResTickDurationSeconds(aHighResStartTick: Int64; aHighResEndTick: Int64=InvalidHighResTick): Double;
begin
  if aHighResEndTick=InvalidHighResTick
  then aHighResEndTick := HighResTick;
  Result := (aHighResEndTick-aHighResStartTick)/HighResTickFrequency;
end;

function HighResTickDurationStr(aHighResStartTick, aHighResEndTick: Int64): string;
begin
  if aHighResEndTick=InvalidHighResTick
  then aHighResEndTick := HighResTick;
  Result := HighResDurationToStr((aHighResEndTick-aHighResStartTick)*HighResTickResolution);
end;


{ THighResTimer }

function THighResTimer.DurationInSeconds: Double;
begin
  Result := HighResTickDurationSeconds(fStartTick, fStopTick);
end;

function THighResTimer.DurationStr: string;
begin
  Result := HighResTickDurationStr(fStartTick, fStopTick);
end;

procedure THighResTimer.Start;
begin
  fStartTick := HighResTick;
end;

procedure THighResTimer.Stop;
begin
  fStopTick := HighResTick;
end;

function THighResTimer.DurationInTicks: Int64;
begin
  Result := fStopTick-fStartTick;
end;

{ THighResAverage }

procedure THighResAverage.Add(aTimer: THighResTimer);
begin
  fTotalTicks := fTotalTicks+aTimer.DurationInTicks;
  Inc(fCount);
end;

procedure THighResAverage.Clear;
begin
  fTotalTicks := 0;
  fCount := 0;
end;

function THighResAverage.DurationInSeconds: Double;
begin
  Result := DurationInTicks/HighResTickFrequency;
end;

function THighResAverage.DurationInTicks: Int64;
begin
  Result := Round(fTotalTicks/fCount);
end;

function THighResAverage.DurationStr: string;
begin
  Result := HighResDurationToStr(DurationInTicks*HighResTickResolution);
end;

end.
