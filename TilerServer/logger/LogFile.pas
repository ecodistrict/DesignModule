unit LogFile;

// written by J.A. Cornelissen

interface

uses
  Logger,
  Windows, SysUtils;

const
  LogExt = '.log';
  DefMaxLogFileNameIndex = 99;

type
  PTextFile = ^TextFile;

  TFileLogger = class(TLogBase)
  constructor Create(aLog: TLog);
  destructor Destroy; override;
  protected
    fFile: TextFile;
    fFileIsOpen: Boolean;
    fFileName: String;
    fCloseAfterWrite: Boolean;
    fLogExt: String;
    fMaxLogFileNameIndex: Integer;

    procedure SetFileName(const aValue: String);
    function GetFilePtr: PTextFile;
    procedure SetCloseAfterWrite(const aValue: Boolean); virtual;
    property FilePtr: PTextFile Read GetFilePtr; // auto opening if used private
    function Open: Boolean; virtual;
    procedure HookFileOpen(aAppended: Boolean); virtual;
    procedure HookFileClose; virtual;
  public
    procedure WriteLn(const aLine: String; aLevel: TLogLevel); override;

    property FileName: String read fFileName write SetFileName;
    property CloseAfterWrite: Boolean read fCloseAfterWrite write SetCloseAfterWrite;
    property MaxLogFileNameIndex: Integer read fMaxLogFileNameIndex write fMaxLogFileNameIndex;
    procedure Close; virtual;

    procedure MakeLocalFileName;
    procedure MakeExeFileName;
    procedure MakeExeFileNameOnDir(const aDir: String);
    procedure MakeUserNameOnDir(const aDir: String);
    procedure MakeModuleFileName;

    procedure Clear; override;
  end;

var
  FileLogger: TFileLogger;

implementation

{ Utils }

function DirectoryExists(const aName: string): Boolean;
var
  code: DWORD;
begin
  code := GetFileAttributes(PChar(aName));
  Result := (code <> $FFFFFFFF) AND (FILE_ATTRIBUTE_DIRECTORY AND code <> 0);
end;

function ForceDirectories(const aDir: string): Boolean;
begin
  if aDir <> '' then
  begin
    if (Length(aDir) < 3) OR DirectoryExists(aDir) OR (ExtractFilePath(aDir) = aDir)
    then Result := True
    else Result := ForceDirectories(ExtractFileDir(aDir)) AND CreateDir(aDir);
  end
  else Result := False;
end;

function GetThisModuleFileName: String;
var
  buf: array [0 .. MAX_PATH] of Char;
begin
  if GetModuleFileName(HInstance, buf, SizeOf(buf) div SizeOf(Char)) <> 0
  then Result := buf
  else Result := '';
end;

function GetTempDirectory: String;
var
  tempBase0: array [0..MAX_PATH] of Char;
begin
  if GetTempPath(SizeOf(tempBase0) div SizeOf(Char), tempBase0) <> 0 then
  begin
    Result := ExcludeTrailingPathDelimiter(tempBase0);
    if CompareText(ExtractFileName(Result), 'temp') <> 0
    then Result := Result + '\TEMP';
  end
  else Result := '';
end;

{ TFileLogger }

procedure TFileLogger.Clear;

  function FreeLogFileName(const aFN: String): String;
  var
    i: Integer;
  begin
    i := 1;
    repeat
      Result := ChangeFileExt(ChangeFileExt(aFN, '') + ';' + IntToStr(i), LogExt);
      i := i + 1;
    until NOT FileExists(Result);
  end;

var
  fn: String;
begin
  // close current log
  Close;
  // determine current log file name
  if fFileName = ''
  then fn := ChangeFileExt(ParamStr(0), fLogExt) // not yet open?!
  else fn := fFileName;
  // rename existing log
  if FileExists(fn)
  then RenameFile(fn, FreeLogFileName(fn));
end;

procedure TFileLogger.Close;
begin
  if fFileIsOpen then
  begin
    HookFileClose;
    CloseFile(fFile);
    fFileIsOpen := False;
  end;
end;

constructor TFileLogger.Create(aLog: TLog);
begin
  fLogExt := LogExt;
  fFileName := '';
  fFileIsOpen := False;
  fCloseAfterWrite := False;
  fMaxLogFileNameIndex := DefMaxLogFileNameIndex;
  inherited Create(aLog);
end;

destructor TFileLogger.Destroy;
begin
  inherited;
  Close;
end;

function TFileLogger.GetFilePtr: PTextFile;
begin
  if Open
  then Result := @fFile
  else Result := nil;
end;

procedure TFileLogger.HookFileClose;
begin
end;

procedure TFileLogger.HookFileOpen(aAppended: Boolean);
begin
end;

procedure TFileLogger.MakeExeFileName;
begin
  FileName := ChangeFileExt(ParamStr(0), fLogExt);
end;

procedure TFileLogger.MakeExeFileNameOnDir(const aDir: String);
begin
  FileName := IncludeTrailingPathDelimiter(aDir) + ChangeFileExt(ExtractFileName(ParamStr(0)), fLogExt);
end;

procedure TFileLogger.MakeLocalFileName;
var
  winDir0: array [0 .. MAX_PATH] of Char;
begin
  if GetWindowsDirectory(winDir0, SizeOf(winDir0) div SizeOf(Char)) <> 0
  then FileName := String(winDir0)
  else FileName := 'c:';
  if (Length(FileName) > 0) AND (FileName[Length(FileName)] <> '\')
  then FileName := FileName + '\';
  FileName := FileName + ChangeFileExt(ExtractFileName(ParamStr(0)), fLogExt);
end;

procedure TFileLogger.MakeModuleFileName;
begin
  FileName := ChangeFileExt(GetThisModuleFileName, fLogExt);
end;

procedure TFileLogger.MakeUserNameOnDir(const aDir: String);
begin
  FileName := IncludeTrailingPathDelimiter(aDir) + FLog.Username + fLogExt;
end;

function TFileLogger.Open: Boolean;

  function OpenTheIndexedFile(const aBaseFileName: String): Boolean;
  var
    i: Integer;
  begin
    if ForceDirectories(ExtractFileDir(aBaseFileName)) then
    begin
      FileName := aBaseFileName;
      i := 0;
      repeat
        try
          AssignFile(fFile, FileName);
          if FileExists(FileName) then
          begin
            Append(fFile);
            HookFileOpen(True);
          end
          else
          begin
            Rewrite(fFile);
            HookFileOpen(False);
          end;
          fFileIsOpen := True;
        except
          on e: Exception do
          begin
            // test sharing violation:
            // 32: in use
            if (e IS EInOutError) AND ((E AS EInOutError).ErrorCode = 32) then
            begin
              i := i + 1;
              FileName := ChangeFileExt(aBaseFileName, '') + ';' + IntToStr(i) + ExtractFileExt(aBaseFileName);
            end
            else
            begin
              // skip index file name and return unsuccessfull
              i := MaxLogFileNameIndex + 1;
            end;
          end;
        end;
      until fFileIsOpen OR (i > MaxLogFileNameIndex);
    end;
    // else could not create directory, ignore error and return unsuccessfull
    Result := fFileIsOpen;
  end;

var
  baseFileName: String;
begin
  if NOT fFileIsOpen then
  begin
    // if no file name specified use exe name with log ext
    if FileName = ''
    then baseFileName := ChangeFileExt(ParamStr(0), fLogExt)
    else baseFileName := FileName;
    // try to open log file
    // if it cannot try to use index appended to file name for MaxLogFileNameIndex number of times
    if NOT OpenTheIndexedFile(baseFileName) then
    begin
      // try the temp directory
      if NOT OpenTheIndexedFile(IncludeTrailingPathDelimiter(GetTempDirectory) + ExtractFileName(baseFileName)) then
      begin
        // try the root of c:
        if NOT OpenTheIndexedFile('c:\' + ExtractFileName(baseFileName))
        then FileName := baseFileName; // of all fail, reset to original file name
      end;
    end;
  end;
  Result := fFileIsOpen;
end;

procedure TFileLogger.SetCloseAfterWrite(const aValue: Boolean);
begin
  fCloseAfterWrite := aValue;
  if fCloseAfterWrite
  then Close;
end;

procedure TFileLogger.SetFileName(const aValue: String);
begin
  Close;
  fFileName := aValue;
end;

procedure TFileLogger.WriteLn(const aLine: String; aLevel: TLogLevel);
begin
  System.WriteLn(FilePtr^, aLine);
  if fCloseAfterWrite
  then Close
  else Flush(FilePtr^);
end;

initialization
  FileLogger := TFileLogger.Create(Log);
finalization
  FileLogger := nil;
end.
