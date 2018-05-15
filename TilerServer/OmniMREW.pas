unit OmniMREW;

interface

uses
  WinApi.Windows;

type
  // very lightweight multiple-readers-exclusive-writer lock.
  // ref http://otl.17slon.com/index.htm

  TOmniMREW = record
  procedure Create;
  strict private
    omrewReference: integer; // Reference.Bit0 is 'writing in progress' flag
  public
    procedure BeginRead; inline;
    procedure BeginWrite; inline;
    procedure EndRead; inline;
    procedure EndWrite; inline;
  end;

  TOmniMREWSleep = record
  procedure Create(aReadCycleMS: Integer; aWriteCycleMS: Integer);
  strict private
    omrewReference: integer; // Reference.Bit0 is 'writing in progress' flag
  private
    fReadCycleMS: Integer;
    fWriteCycleMS: Integer;
  public
    procedure BeginRead; inline;
    procedure BeginWrite; inline;
    procedure EndRead; inline;
    procedure EndWrite; inline;
  end;


implementation

{ TOmniMREW }

procedure TOmniMREW.BeginRead;
var
  currentReference: integer;
begin
  // wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 2, currentReference);
end;

procedure TOmniMREW.BeginWrite;
var
  currentReference: integer;
begin
  // wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  //Log.WriteLn('>BWRL '+IntToHex(GetCurrentThreadID, 8));
  repeat
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 1, currentReference);
  // now wait on all readers
  repeat until omrewReference = 1;
  //Log.WriteLn('<BWRL '+IntToHex(GetCurrentThreadID, 8));
end;

procedure TOmniMREW.Create;
begin
  EndWrite;
end;

procedure TOmniMREW.EndRead;
begin
  //Decrease omrewReference
  InterlockedExchangeAdd(omrewReference, -2);
end;

procedure TOmniMREW.EndWrite;
begin
  omrewReference := 0;
  //Log.WriteLn('EWRL '+IntToHex(GetCurrentThreadID, 8));
end;

{ TOmniMREWSleep }

procedure TOmniMREWSleep.BeginRead;
var
  currentReference: integer;
begin
  currentReference := 0;
  // wait on writer to reset write flag so Reference.Bit0 must be 0 than increase Reference
  repeat
    if (currentReference<>0) and (fReadCycleMS<>0)
    then Sleep(fReadCycleMS);
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 2, currentReference);
end;

procedure TOmniMREWSleep.BeginWrite;
var
  currentReference: integer;
begin
  // wait on writer to reset write flag so omrewReference.Bit0 must be 0 then set omrewReference.Bit0
  //Log.WriteLn('>BWRL '+IntToHex(GetCurrentThreadID, 8));
  currentReference := 0;
  repeat
    if (currentReference<>0) and (fWriteCycleMS<>0)
    then Sleep(fWriteCycleMS);
    currentReference := omrewReference AND NOT 1;
  until currentReference = InterlockedCompareExchange(omrewReference, currentReference + 1, currentReference);
  // now wait on all readers
  repeat until omrewReference = 1;
  //Log.WriteLn('<BWRL '+IntToHex(GetCurrentThreadID, 8));
end;

procedure TOmniMREWSleep.Create(aReadCycleMS, aWriteCycleMS: Integer);
begin
  fReadCycleMS := aReadCycleMS;
  fWriteCycleMS := aWriteCycleMS;
  EndWrite;
end;

procedure TOmniMREWSleep.EndRead;
begin
  //Decrease omrewReference
  InterlockedExchangeAdd(omrewReference, -2);
end;

procedure TOmniMREWSleep.EndWrite;
begin
  omrewReference := 0;
  //Log.WriteLn('EWRL '+IntToHex(GetCurrentThreadID, 8));
end;

end.
