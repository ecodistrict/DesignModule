unit LogMemory;

// not ready yet

interface

uses
  Logger,
  Classes, Windows, SysUtils;

type
  TMemoryLogger = class(TLogBase)
  constructor Create(aLog: TLog);
  destructor Destroy; override;
  private
    fLogLines: TStringList;
  public
    procedure WriteLn(const aLine: string; aLevel: TLogLevel); override;
    procedure Clear; override;
  end;

implementation

{ TMemoryLogger }

procedure TMemoryLogger.Clear;
begin
  fLogLines.Clear;
end;

constructor TMemoryLogger.Create(aLog: TLog);
begin
  fLogLines := TStringList.Create;
  inherited Create(aLog);
end;

destructor TMemoryLogger.Destroy;
begin
  inherited;
  FreeAndNil(fLogLines);
end;

procedure TMemoryLogger.WriteLn(const aLine: string; aLevel: TLogLevel);
begin
  fLogLines.Add(aLine);
end;

end.
