unit MyConsole;

// written by J.A. Cornelissen

interface

uses
  SyncObjs, SysUtils, Windows;

// Read and ReadLn wait for a return, this function overcomes this problem
// in a console application by asking a questoin and wait for 1 key to be pressed
// default answer is no, yes is given with y and no is given with return or n
function  GetConfirm                   ( const line: string): Boolean;

// Dependent on the compiler switch ANSICONSOLE the function just returns the given string as result
// or translates the string to the oem charset to be displayed in a console window
function  MyCharToOem                  ( const s: string): string;

function  CheckKeyPressed              : Boolean;
function  GetNextKey                   : Integer;
function  GetNextKeyASCII              : Char;

// no flushing
function  CheckKeyPressed2             : Boolean;
function  KeyPressed2                  : Char;
procedure KeyPressedFlush;

// utility to convert procedure/function to method.
// add "self: pointer" as first parameter to procedure/function definition to conform to method calling convention
function  ProcedureToMethod            ( aProcedure: pointer; aSelf: Pointer=nil): TMethod;


const
  ccBlack                              = $0;
  ccNavy                               = $1;
  ccGreen                              = $2;
  ccCyaan                              = $3;
  ccMaroon                             = $4;
  ccPurple                             = $5;
  ccOlive                              = $6;
  ccSilver                             = $7;
  ccGray                               = $8;
  ccBlue                               = $9;
  ccLime                               = $A;
  ccAqua                               = $B;
  ccRed                                = $C;
  ccFuchsia                            = $D;
  ccYellow                             = $E;
  ccWhite                              = $F;

type
  TConsole = class
  constructor Create;
  destructor  Destroy; override;
  private
    Handle                             : THandle;
    FLock                              : TCriticalSection;
    function  GetBackgroundColor       : Byte;
    function  GetCursor                : TCoord;
    function  GetForegroundColor       : Byte;
    function  GetScreenHeight          : Byte;
    function  GetScreenWidth           : Byte;
    procedure SetBackgroundColor       ( const Value: Byte);
    procedure SetCursor                ( const Value: TCoord);

    procedure SetForegroundColor       ( const Value: Byte);

  public
    procedure Lock;
    procedure UnLock;
    property  ForegroundColor          : Byte read GetForegroundColor write SetForegroundColor;
    property  BackgroundColor          : Byte read GetBackgroundColor write SetBackgroundColor;
    property  Cursor                   : TCoord read GetCursor write SetCursor;
    procedure SetCursorXY              ( x, y: Integer);
    procedure ClrEol;
    procedure ClrScrn;
    property  ScreenWidth              : Byte read GetScreenWidth;
    property  ScreenHeight             : Byte read GetScreenHeight;

    procedure ColorChart;
    procedure ScreenInfo;
    function  MaxFitWrite              ( const aLine: string): string;
  end;

var
  Console: TConsole;

implementation

function GetNextKey: Integer;
var
  InputRecord: TInputRecord;
  Console: THandle;
  NumRead: DWORD;
begin
  Console := GetStdHandle(STD_INPUT_HANDLE);
  // flush key buffer of console so we don't start working on accidental keys entered
  // this function is for confirmation so we want to make sure the correct answer is returned
  FlushConsoleInputBuffer(Console);
  // read charakter, wait till one is provided
  if ReadConsoleInput(Console,InputRecord,1,NumRead) AND (InputRecord.EventType=KEY_EVENT) then
  begin
    // search for data that makes any sense
    if InputRecord.Event.KeyEvent.bKeyDown
    then Result := InputRecord.Event.KeyEvent.wVirtualKeyCode
    else Result := 0;
  end
  else Result := 0;
end;

function GetNextKeyASCII: Char;
var
  InputRecord:TInputRecord;
  Console:THandle;
  NumRead:DWORD;
begin
  Console := GetStdHandle(STD_INPUT_HANDLE);
  // flush key buffer of console so we don't start working on accidental keys entered
  // this function is for confirmation so we want to make sure the correct answer is returned
  FlushConsoleInputBuffer(Console);
  // read charakter, wait till one is provided
  if ReadConsoleInput(Console,InputRecord,1,NumRead) AND (InputRecord.EventType=KEY_EVENT) then
  begin
    // search for data that makes any sense
    if InputRecord.Event.KeyEvent.bKeyDown
    then Result  :=  InputRecord.Event.KeyEvent.UnicodeChar // AsciiChar
    else Result := #$00;
  end
  else Result := #$00;
end;

function GetConfirm(const line:string):Boolean;
var
  InputRecord:TInputRecord;
  NumRead:DWORD;
  Console:THandle;
  OK:Boolean;
begin
  GetConfirm := False;
  // line is question to be asked choice os keys is added here
  Write(Line+'(y/N) ');
  // get handle for console we need it later to retrieve keyboard buffer information
  Console := GetStdHandle(STD_INPUT_HANDLE);
  // flush key buffer of console so we don't start working on eccidental keys entered
  // this function is for confirmation so we want to make sure the correct answer is returned
  FlushConsoleInputBuffer(Console);
  OK := False;
  // we start reading charakters till we find something usefull
  repeat
    // read charakter, wait till one is provided
    if ReadConsoleInput(Console,InputRecord,1,NumRead) then
    begin
      // check if it is a key
      if InputRecord.EventType=KEY_EVENT then
      begin
        // search for data that makes any sense
        if InputRecord.{$IFNDEF Delphi30}Event.{$ENDIF}KeyEvent.bKeyDown AND
           (InputRecord.{$IFNDEF Delphi30}Event.{$ENDIF}KeyEvent.AsciiChar in ['y','Y','n','N',#13]) then
        begin
          // Only confirm on y
          if UpCase(InputRecord.{$IFNDEF Delphi30}Event.{$ENDIF}KeyEvent.AsciiChar)='Y' then
          begin
            Write('Y'); // reflect chosen option
            GetConfirm := True;
          end
          else
          begin
            Write('N'); // reflect chosen option
            GetConfirm := False;
          end;
          OK := True;
        end;
      end;
    end;
  until OK;
  // go to next line so the answer is not writen over
  WriteLn;
end;

function MyCharToOem(const s:string):string;
{$IFNDEF ANSICONSOLE}
const
  MaxConvertBuffer = 1024;
var
  Buffer:array[0..MaxConvertBuffer] of AnsiChar;
{$ENDIF}
begin
  {$IFDEF ANSICONSOLE}
  Result := s;
  {$else}
  if CharToOem(PChar(Copy(s,1,MaxConvertBuffer-1)),Buffer)
  then Result := string(Buffer)
  else Result := s;
  {$ENDIF}
end;

function CheckKeyPressed:Boolean;
var
  InputRecord:TInputRecord;
  NumRead:DWORD;
  Console:THandle;
begin
  // get handle for console we need it later to retrieve keyboard buffer information
  Console := GetStdHandle(STD_INPUT_HANDLE);
  if PeekConsoleInput(Console,InputRecord,1,NumRead) then
  begin
    FlushConsoleInputBuffer(Console);
    Result := (NumRead>0) AND (InputRecord.EventType=KEY_EVENT);
  end
  else Result := False;
end;

function CheckKeyPressed2: Boolean;
var
  InputRecord:TInputRecord;
  NumRead:DWORD;
  Console:THandle;
begin
  Console  :=  GetStdHandle(STD_INPUT_HANDLE);
  Result  :=  False;
  while PeekConsoleInput(Console, InputRecord, 1, NumRead) and (NumRead>0) and not Result do
  begin
    if (InputRecord.EventType<>KEY_EVENT) or not InputRecord.Event.KeyEvent.bKeyDown
    then ReadConsoleInput(Console, InputRecord, 1, NumRead) // remove event from buffer by reading it; we only want key events
    else Result  :=  True;
  end;
end;

function KeyPressed2: Char;
var
  InputRecord:TInputRecord;
  Console:THandle;
  NumRead:DWORD;
begin
  Console := GetStdHandle(STD_INPUT_HANDLE);
  // init input record with all zeros
  FillChar(InputRecord, SizeOf(InputRecord), 0);
  // read from buffer, wait till one key down is provided
  repeat until ReadConsoleInput(Console, InputRecord, 1, NumRead) AND (InputRecord.EventType=KEY_EVENT) and InputRecord.Event.KeyEvent.bKeyDown;
  if (InputRecord.EventType=KEY_EVENT) and InputRecord.Event.KeyEvent.bKeyDown
  then Result  :=  InputRecord.Event.KeyEvent.UnicodeChar
  else Result  :=  #$00;
end;

procedure KeyPressedFlush;
var
  Console:THandle;
begin
  Console := GetStdHandle(STD_INPUT_HANDLE);
  FlushConsoleInputBuffer(Console);
end;

{ TConsole }

procedure TConsole.ClrEol;
var
  NumberOfCharsWritten: Cardinal;
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo) then
  begin
    with ConsoleScreenBufferInfo do
    begin
      FillConsoleOutputCharacter( Handle,
                                  ' ',
                                  srWindow.Right-srWindow.Left+1-dwCursorPosition.X,
                                  Cursor,
                                  NumberOfCharsWritten);
      FillConsoleOutputAttribute( Handle,
                                  (BackgroundColor SHL 4) OR ForegroundColor,
                                  srWindow.Right-srWindow.Left+1-dwCursorPosition.X,
                                  Cursor,
                                  NumberOfCharsWritten);
    end;
  end
  else Write('## ClrEol (',GetLastError,')');
end;

procedure TConsole.ClrScrn;
var
  NumberOfCharsWritten: Cardinal;
begin
  SetCursorXY(0,0);
  FillConsoleOutputCharacter(Handle,' ',ScreenWidth*ScreenHeight,Cursor,NumberOfCharsWritten);
  FillConsoleOutputAttribute(Handle,(BackgroundColor SHL 4) OR ForegroundColor,ScreenWidth*ScreenHeight,Cursor,NumberOfCharsWritten);
end;

procedure TConsole.ColorChart;
var
  fs, bs: Byte;
  f, b: Byte;
  cs: TCoord;
begin
  fs := ForegroundColor;
  bs := BackgroundColor;
  cs := Cursor;
  for b := $0 to $F do
  begin
    for f := $0 to $F do
    begin
      ForegroundColor := f;
      BackgroundColor := b;
      SetCursorXY(cs.X+(f*3),cs.Y+b);
      Write(IntToHex(b,1),IntToHex(f,1));
    end;
  end;
  ForegroundColor := fs;
  BackgroundColor := bs;
  Cursor := cs;
end;

constructor TConsole.Create;
begin
  inherited Create;
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  FLock := TCriticalSection.Create;
end;

destructor TConsole.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TConsole.GetBackgroundColor: Byte;
var
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo)
  then Result := (ConsoleScreenBufferInfo.wAttributes AND $F0) SHR 4
  else Result := ccBlack;
end;

function TConsole.GetCursor: TCoord;
var
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo)
  then Result := ConsoleScreenBufferInfo.dwCursorPosition
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

function TConsole.GetForegroundColor: Byte;
var
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo)
  then Result := (ConsoleScreenBufferInfo.wAttributes AND $0F)
  else Result := ccSilver;
end;

function TConsole.GetScreenHeight: Byte;
var
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo)
  then Result := ConsoleScreenBufferInfo.srWindow.Bottom-ConsoleScreenBufferInfo.srWindow.Top+1
  else Result := 0;
end;

function TConsole.GetScreenWidth: Byte;
var
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo)
  then Result := ConsoleScreenBufferInfo.srWindow.Right-ConsoleScreenBufferInfo.srWindow.Left+1
  else Result := 0;
end;

procedure TConsole.Lock;
begin
  FLock.Acquire;
end;

function TConsole.MaxFitWrite(const aLine: string): string;
var
  MaxCharsLeft: Integer;
begin
  MaxCharsLeft := ScreenWidth-Cursor.X-1;
  if Length(aLine)>MaxCharsLeft
  then Result := Copy(aLine,1,MaxCharsLeft-2)+'..'
  else Result := aLine;
end;

procedure TConsole.ScreenInfo;
var
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  if GetConsoleScreenBufferInfo(Handle,ConsoleScreenBufferInfo) then
  begin
    with ConsoleScreenBufferInfo
    do Write( 'size: ',dwSize.X,',',dwSize.Y,' ',
              'Cursor: ',dwCursorPosition.X,',',dwCursorPosition.Y,' ',
              'attr: ',wAttributes,' ',
              'window: ',srWindow.Top,',',srWindow.Left,' ',srWindow.Right,',',srWindow.Bottom,' ',
              'max ws: ',dwMaximumWindowSize.X,',',dwMaximumWindowSize.Y);

  end
  else Write('## No screen buffer info (',GetLastError,')');
end;

procedure TConsole.SetBackgroundColor(const Value: Byte);
begin
  SetConsoleTextAttribute(Handle,ForeGroundColor OR ((Value SHL 4) AND $F0));
end;

procedure TConsole.SetCursor(const Value: TCoord);
begin
  SetConsoleCursorPosition(Handle,Value);
end;

procedure TConsole.SetCursorXY(x, y: Integer);
var
  C: TCoord;
begin
  C.X := X;
  C.Y := y;
  SetCursor(C);
end;

procedure TConsole.SetForegroundColor(const Value: Byte);
begin
  SetConsoleTextAttribute(Handle,(BackgroundColor SHL 4) OR Value);
end;

procedure TConsole.UnLock;
begin
  FLock.Release;
end;

function ProcedureToMethod(aProcedure: pointer; aSelf: Pointer=nil): TMethod;
begin
  TMethod(Result).Data := aSelf;
  TMethod(Result).Code := aProcedure;
end;

initialization
  Console := TConsole.Create;
finalization
  FreeAndNil(Console);
end.
