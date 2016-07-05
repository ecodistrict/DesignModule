unit Logger;

interface

uses
  Int64Time, DTTime,
  MyStr,
  Generics.Collections,
  SyncObjs, Classes, Windows, SysUtils;

type
  TLogLevel = (llRemark, llDump, llNormal, llStart, llFinish, llPush, llPop, llStamp, llSummary, llWarning, llError, llOK);

  TLogLevelSet = set of TLogLevel;

  TLogLevelSwitches = (llsNotActive, llsTime, llsID, llsThreadID, llsNoIndent, llsNoPre);

  TLogLevelSwitchSet = set of TLogLevelSwitches;

  TLogDef = array [TLogLevel] of TLogLevelSwitchSet;

  TDateTimeFormatPrecision = (dtfpLow, dtfpStandard, dtfpHigh);

  TLog = class; // forward

  // all specific loggers derive from TLogBase
  TLogBase = class
  constructor Create(aLog: TLog);
  destructor Destroy; override;
  protected
    fLog: TLog;
  public
    LogDef: TLogDef;
    procedure WriteLn(const aLine: string; aLevel: TLogLevel); virtual;
    procedure Progress(const aLine: string); virtual;
    procedure LeaveProgress; virtual;

    procedure SetLogDef(aLevels: TLogLevelSet; aSwitches: TLogLevelSwitchSet);
    procedure ResetLogDef(aLevels: TLogLevelSet; aSwitches: TLogLevelSwitchSet);

    procedure Clear; virtual;
  end;

  TLog = class
  constructor Create;
  destructor Destroy; override;
  protected
    fLogList: TObjectList<TLogBase>;
    fInProgress: Boolean;
    fStartTime: TDateTime;
    fLogLevelStatus: TLogLevelSet;
    fNoProgress: Boolean;
    fPPIndent: Integer;
    fDateTimeFormat: string;

    function ThisComputerName: string;
    function ThisUserName: string;
    function ThisThreadID: string;
    function LoggerByIndex(aIndex: Integer): TLogBase;
    function GetID: string;
    function GetTime(aTime: TDateTime): string;
    procedure Flush; virtual;
  public
    procedure WriteLn(const aLine: string; aLevel: TLogLevel = llNormal; aIndent: Integer = 0; aTimeStampOverride: Boolean = False; aTime: TDateTime = 0); overload; virtual;
    procedure WriteLn(E: Exception); overload;
    procedure WriteLines(aLines: Tstrings; aLevel: TLogLevel = llNormal; aIndent: Integer = 0);
    procedure Progress(const aLine: string; aIndent: Integer = -1); // no leading spaces like writeln for level
    procedure Start(const aMessage: string = 'Started');
    procedure Finish(const aMessage: string = 'Finished');

    property LogLevelStatus: TLogLevelSet read fLogLevelStatus;
    procedure ResetLogLevelStatus;
    property NoProgress: Boolean read fNoProgress write fNoProgress;

    procedure AddLogger(aLogger: TLogBase);
    procedure RemLogger(aLogger: TLogBase);

    procedure LeaveProgress;

    property ComputerName: string read ThisComputerName;
    property UserName: string read ThisUserName;
    property ThreadID: string read ThisThreadID;

    procedure Push(const aLine: string = ''; aTimeStampOverride: Boolean = False); virtual;
    procedure Pop(const aLine: string = ''; aTimeStampOverride: Boolean = False); virtual;

    property DateTimeFormat: string read fDateTimeFormat write fDateTimeFormat;
    procedure SetDateTimeFormat(aPrecision: TDateTimeFormatPrecision);

    procedure Clear;
  end;

function ErrorToLogLevel(aError: Boolean): TLogLevel;

const
  AllLogLevels: TLogLevelSet = [llRemark, llDump, llNormal, llStart, llFinish, llPush, llPop, llStamp, llSummary, llWarning, llError, llOK];

  Prestring: array [TLogLevel] of string = (
    '// ', // remark
    '', // dump
    '   ', // normal
    '', // start
    '', // finish
    '-> ', // push
    '<- ', // pop
    '', // stamp
    '-- ', // summary
    '>> ', // warning
    '## ', // error
    'OK ' // ok
  );

var
  Log: TLog; // abstract place holder for logging facility

implementation

function ErrorToLogLevel(aError: Boolean): TLogLevel;
begin
  if aError
  then Result := llError
  else Result := llNormal;
end;

{ TLogBase }

procedure TLogBase.Clear;
begin
  // default no action
end;

constructor TLogBase.Create(aLog: TLog);
begin
  inherited Create;
  fLog := aLog;
  if Assigned(fLog)
  then fLog.AddLogger(Self);
  SetLogDef([llStart, llFinish, llStamp], [llsTime, llsID]);
end;

destructor TLogBase.Destroy;
begin
  if Assigned(fLog) then
  begin
	TMonitor.Enter(Self);
	try
    fLog.fLogList.Extract(Self);
	finally
	  TMonitor.Exit(Self);
	end;
    fLog := nil;
  end;
  inherited;
end;

procedure TLogBase.LeaveProgress;
begin
  // default no action
end;

procedure TLogBase.Progress(const aLine: string);
begin
  // default no action
end;

procedure TLogBase.ResetLogDef(aLevels: TLogLevelSet; aSwitches: TLogLevelSwitchSet);
var
  ll: TLogLevel;
begin
  for ll := Low(TLogLevel) to High(TLogLevel) do
  begin
    if ll in aLevels
    then LogDef[ll] := LogDef[ll] - aSwitches;
  end;
end;

procedure TLogBase.SetLogDef(aLevels: TLogLevelSet; aSwitches: TLogLevelSwitchSet);
var
  ll: TLogLevel;
begin
  for ll := Low(TLogLevel) to High(TLogLevel) do
  begin
    if ll in aLevels
    then LogDef[ll] := LogDef[ll] + aSwitches;
  end;
end;

procedure TLogBase.WriteLn(const aLine: string; aLevel: TLogLevel);
begin
  // default no actions
end;

{ TLog }

procedure TLog.AddLogger(aLogger: TLogBase);
begin
  TMonitor.Enter(Self);
  try
    if Assigned(aLogger) then
    begin
      fLogList.Add(aLogger);
      aLogger.fLog := Self;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TLog.Clear;
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    for i := 0 to fLogList.Count - 1
    do LoggerByIndex(i).Clear;
  finally
    TMonitor.Exit(Self);
  end;
end;

constructor TLog.Create;
begin
  inherited Create;
  fLogList := TObjectList<TLogBase>.Create(True); // owns specific loggers
  ResetLogLevelStatus;
  fNoProgress := False;
  fPPIndent := 0;
  fDateTimeFormat := UniversalDateTimeFormat;
end;

destructor TLog.Destroy;
var
  l: TLogBase;
begin
  // detach from this logger
  for l in fLogList
  do l.fLog := nil;
  // destroy all registered loggers
  FreeAndNil(fLogList);
  inherited;
end;

procedure TLog.Finish(const aMessage: string);
begin
  if fStartTime <> 0
  then WriteLn(aMessage + ' (' + DurationToStr(Now - fStartTime) + ')', llFinish)
  else WriteLn(aMessage, llFinish);
end;

procedure TLog.Flush;
begin
  // default no action?
end;

function TLog.GetID: string;
begin
  Result := ComputerName + '\' + UserName;
end;

function TLog.GetTime(aTime: TDateTime): string;
begin
  if aTime=0
  then Result := FormatDateTime(DateTimeFormat, Now)
  else Result := FormatDateTime(DateTimeFormat, aTime);
end;

procedure TLog.LeaveProgress;
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    if fInProgress then
    begin
      fInProgress := False;
      for i := 0 to fLogList.Count - 1
      do LoggerByIndex(i).LeaveProgress;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TLog.LoggerByIndex(aIndex: Integer): TLogBase;
begin
  Result := fLogList[aIndex];
end;

procedure TLog.Pop(const aLine: string; aTimeStampOverride: Boolean);
begin
  TMonitor.Enter(Self);
  try
    if fPPIndent > 0
    then fPPIndent := fPPIndent - 1;
    if aLine <> ''
    then WriteLn(aLine, llPop);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TLog.Progress(const aLine: string; aIndent: Integer);
// no leading spaces like writeln for level
var
  i: Integer;
  indent: string;
begin
  if not NoProgress then
  begin
    TMonitor.Enter(Self);
    try
      fInProgress := True;
      // prepare indent
      indent := '';
      while aIndent > -1 do
      begin
        indent := indent + Prestring[llNormal]; // **** Indentstring;
        aIndent := aIndent - 1;
      end;
      // call specific loggers
      for i := 0 to fLogList.Count - 1
      do LoggerByIndex(i).Progress(indent + aLine);
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

procedure TLog.Push(const aLine: string; aTimeStampOverride: Boolean);
begin
  TMonitor.Enter(Self);
  try
    if aLine <> ''
    then WriteLn(aLine, llPush);
    fPPIndent := fPPIndent + 1;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TLog.RemLogger(aLogger: TLogBase);
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    Flush;
    if Assigned(aLogger) then
    begin
      i := fLogList.IndexOf(aLogger);
      if i >= 0
      then fLogList.Delete(i);
      aLogger.fLog := nil;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TLog.ResetLogLevelStatus;
begin
  fLogLevelStatus := [];
end;

procedure TLog.SetDateTimeFormat(aPrecision: TDateTimeFormatPrecision);
begin
  TMonitor.Enter(Self);
  try
    case aPrecision of
      dtfpLow:
        DateTimeFormat := 'mm-dd' + ' ' + 'hh:nn';
      dtfpHigh:
        DateTimeFormat := UniversalDateTimeFormat + '.zzz';
    else // dtfpStandard
        DateTimeFormat := UniversalDateTimeFormat;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TLog.Start(const aMessage: string);
begin
  WriteLn(aMessage, llStart);
  fStartTime := Now;
end;

function TLog.ThisComputerName: string;
var
  computerName: array [0..MAX_PATH] of Char;
  s: DWORD;
begin
  s := SizeOf(computerName) div SizeOf(Char);
  GetComputerName(computerName, s);
  Result := string(computerName);
end;

function TLog.ThisThreadID: string;
begin
  Result := IntToHex(GetCurrentThreadID, 8);
end;

function TLog.ThisUserName: string;
var
  userName: array [0..MAX_PATH] of Char;
  s: DWORD;
begin
  s := SizeOf(userName) div SizeOf(Char);
  GetUserName(userName, s);
  Result := string(userName);
end;

procedure TLog.WriteLines(aLines: Tstrings; aLevel: TLogLevel; aIndent: Integer);
var
  l: Integer;
begin
  for l := 0 to aLines.Count - 1
  do WriteLn(aLines[l], aLevel, aIndent);
end;

procedure TLog.WriteLn(E: Exception);
begin
  WriteLn(E.ClassName + ': ' + E.Message, llError);
end;

procedure TLog.WriteLn(const aLine: string; aLevel: TLogLevel; aIndent: Integer; aTimeStampOverride: Boolean; aTime: TDateTime);
var
  i: Integer;
  indent: string;
  locLine: string;
begin
  TMonitor.Enter(Self);
  try
    // check if progress is active; if so clear progress text and mark inactive
    if fInProgress then
    begin
      Progress('');
      fInProgress := False;
    end;
    // prepare indent
    indent := '';
    if aIndent = 0 then
      aIndent := fPPIndent;
    while aIndent > 0 do
    begin
      indent := indent + Prestring[llNormal]; // Indentstring;
      aIndent := aIndent - 1;
    end;
    // dispatch line to all active registered loggers
    for i := 0 to fLogList.Count - 1 do
    begin
      with LoggerByIndex(i) do
      begin
        if not(llsNotActive in LogDef[aLevel]) then
        begin
          // add items to line if requested
          locLine := '';
          if (llsTime in LogDef[aLevel]) OR aTimeStampOverride
          then locLine := locLine + GetTime(aTime) + ' ';
          if llsID in LogDef[aLevel]
          then locLine := locLine + GetID + ' ';
          if llsThreadID in LogDef[aLevel]
          then locLine := locLine + ThreadID + ' ';
          if not(llsNoIndent in LogDef[aLevel])
          then locLine := locLine + indent;
          if not(llsNoPre in LogDef[aLevel])
          then locLine := locLine + Prestring[aLevel];
          WriteLn(locLine + aLine, aLevel);
        end;
      end;
    end;
    // mark status
    Include(fLogLevelStatus, aLevel);
  finally
    TMonitor.Exit(Self);
  end;
end;

initialization
  Log := TLog.Create;
finalization
  FreeAndNil(Log);
end.
