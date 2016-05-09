unit LogThreads;

interface

uses
  Logger,
  Classes, Windows, SysUtils;

type
  TLogMemStoreInfo = class
    Line                               : String;
    Level                              : TLogLevel;
    Indent                             : Integer;
    TimeStampOverride                  : Boolean;
    Time                               : TDateTime;
    procedure WriteLn;
  end;
  
  TLogMemStore = class(TList)
  constructor Create;
  destructor  Destroy; override;
  private
    function  LogMemStoreInfo          ( aIndex: Integer): TLogMemStoreInfo;
  protected
    FPPIndent                          : Integer;
    procedure Flush;
  public
    procedure Clear; override;
    
    procedure WriteLn                  ( const aLine: String; aLevel: TLogLevel; aIndent:Integer; aTimeStampOverride: Boolean; aTime: TDateTime);

    procedure Push                     ( const aLine: String; aTimeStampOverride: Boolean);
    procedure Pop                      ( const aLine: String; aTimeStampOverride: Boolean);
  end;

  TLogThreads = class(TLog)
  constructor Create;
  destructor  Destroy; override;
  private
    FLogMemList                        : TStringList;
    function  ProcessID                : String;
    function  IsMainProcess            : Boolean;
    function  LogMemStore              : TLogMemStore;
  protected
    procedure Flush; override;
  public
    procedure WriteLn                  ( const aLine: String; aLevel: TLogLevel=llNormal; aIndent:Integer=0; aTimeStampOverride: Boolean=False; aTime: TDateTime=0); override;

    procedure Push                     ( const aLine: String=''; aTimeStampOverride: Boolean=False); override;
    procedure Pop                      ( const aLine: String=''; aTimeStampOverride: Boolean=False); override;
  end;

implementation

{ TLogMemStoreInfo }

procedure TLogMemStoreInfo.WriteLn;
begin
  Log.WriteLn(Line,Level,Indent,TimeStampOverride,Time);
end;

{ TLogMemStore }

procedure TLogMemStore.Clear;
var
  i: Integer;
begin
  for i:=0 to Count - 1 do
  begin
    if Assigned(Items[i]) then
    begin
      TLogMemStoreInfo(Items[i]).Free;
      Items[i]:=nil;
    end;
  end;
  inherited;
end;

constructor TLogMemStore.Create;
begin
  inherited;
  FPPIndent:=0;
end;

destructor TLogMemStore.Destroy;
begin
  Clear;
  inherited;
end;

procedure TLogMemStore.Flush;
var
  l: Integer;
begin // only call this from main thread (or synchronize)
  for l := 0 to Count - 1 
  do LogMemStoreInfo(l).WriteLn;
  Clear;
end;

function TLogMemStore.LogMemStoreInfo(aIndex: Integer): TLogMemStoreInfo;
begin
  Result:=Items[aIndex];
end;

procedure TLogMemStore.Pop(const aLine: String; aTimeStampOverride: Boolean);
begin
  if FPPIndent>0
  then FPPIndent:=FPPIndent-1;
  if aLine<>''
  then WriteLn(aLine,llPop,0,aTimeStampOverride,0);
end;

procedure TLogMemStore.Push(const aLine: String; aTimeStampOverride: Boolean);
begin
  if aLine<>''
  then WriteLn(aLine,llPush,0,aTimeStampOverride,0);
  FPPIndent:=FPPIndent+1;
end;

procedure TLogMemStore.WriteLn(const aLine: String; aLevel: TLogLevel; aIndent: Integer; aTimeStampOverride: Boolean; aTime: TDateTime);
var
  LogMemStoreInfo: TLogMemStoreInfo;
begin
  LogMemStoreInfo:=TLogMemStoreInfo.Create;
  LogMemStoreInfo.Line:=aLine;
  LogMemStoreInfo.Level:=aLevel;
  LogMemStoreInfo.Indent:=aIndent+FPPIndent;
  LogMemStoreInfo.TimeStampOverride:=aTimeStampOverride;
  if aTime=0 
  then LogMemStoreInfo.Time:=Now
  else LogMemStoreInfo.Time:=aTime;
  Add(LogMemStoreInfo);
end;


{ TLogThreads }

constructor TLogThreads.Create;
begin
  inherited;
  FLogMemList:=TStringList.Create;
end;

destructor TLogThreads.Destroy;
var
  lmsi: Integer;
begin
  Flush;
  if Assigned(FLogMemList) then
  begin
    for lmsi := 0 to FLogMemList.Count - 1 do
    begin
      if Assigned(FLogMemList.Objects[lmsi]) then
      begin
        FLogMemList.Objects[lmsi].Free;
        FLogMemList.Objects[lmsi]:=nil;
      end;
    end;
    FLogMemList.Free;
    FLogMemList:=nil;
  end;
  inherited;
end;

procedure TLogThreads.Flush;
var
  lmsi: Integer;
begin
  if IsMainProcess then
  begin
    for lmsi := 0 to FLogMemList.Count - 1
    do (FLogMemList.Objects[lmsi] AS TLogMemStore).Flush;
  end;
  inherited;
end;

function TLogThreads.IsMainProcess: Boolean;
begin
  Result:=GetCurrentThreadID=MainThreadID;
end;

function TLogThreads.LogMemStore: TLogMemStore;
var
  lmsi: Integer;
begin
  if NOT IsMainProcess then
  begin
    lmsi:=FLogMemList.IndexOf(ProcessID);
    if lmsi<0 then
    begin
      Result:=TLogMemStore.Create;
      FLogMemList.AddObject(ProcessID,Result);
    end
    else Result:=FLogMemList.Objects[lmsi] AS TLogMemStore;
  end
  else Result:=nil;
end;

procedure TLogThreads.Pop(const aLine: String; aTimeStampOverride: Boolean);
begin
  if NOT IsMainProcess 
  then LogMemStore.Pop(aLine,aTimeStampOverride)
  else inherited;
end;

function TLogThreads.ProcessID: String;
begin
  Result:=IntToHex(GetCurrentThreadID,8);
end;

procedure TLogThreads.Push(const aLine: String; aTimeStampOverride: Boolean);
begin
  if NOT IsMainProcess 
  then LogMemStore.Push(aLine,aTimeStampOverride)
  else inherited;
end;

procedure TLogThreads.WriteLn(const aLine: String; aLevel: TLogLevel; aIndent: Integer; aTimeStampOverride: Boolean; aTime: TDateTime);
begin
  if NOT IsMainProcess 
  then LogMemStore.WriteLn(aLine,aLevel,aIndent,aTimeStampOverride,aTime)
  else inherited;
end;

initialization
  // replace abstract logger entry point
  Log.Free;
  Log := TLogThreads.Create;
end.

