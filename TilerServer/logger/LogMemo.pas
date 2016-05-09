unit LogMemo;
{
  to make log scroll nicely within rich-edit:
  TRichEdit:
  HideScrollBars := False;
  HideSelection := False;
  ReadOnly := True;
  ScrollBars := ssVertical;
  WantReturns := False;
  WordWrap := True;
}

interface

uses
  Logger,
  Forms, RichEdit, StdCtrls, Graphics, ComCtrls, Messages, Windows, SysUtils;

const
  DefaultMaxLogLines = 1000;

type
  TMemoLogger = class(TLogBase)
  constructor Create(aLog: TLog; aMemo: TRichEdit; aProgressLabel: TLabel = nil; aMaxLogLines: Integer=DefaultMaxLogLines);
  destructor Destroy; override;
  private
    fMemo: TRichEdit;
    fProgressLabel: TLabel;
    fMaxLogLines: Integer;
    procedure SetMemo(const Value: TRichEdit);
    procedure SetProgressLabel(const Value: TLabel);
  public
    property Memo: TRichEdit read fMemo write SetMemo;
    procedure WriteLn(const aLine: string; aLevel: TLogLevel); override;

    property ProgressLabel: TLabel read fProgressLabel write SetProgressLabel;
    procedure Progress(const aLine: string); override;
    procedure LeaveProgress; override;
    procedure Clear; override;

    property MaxLogLines: Integer read fMaxLogLines write fMaxLogLines;
    // add statusbar progress element ??
  end;

function AddMemoLogger(aMemo: TRichEdit; aProgressLabel: TLabel = nil; aMaxLogLines: Integer=DefaultMaxLogLines): TMemoLogger;

implementation

function AddMemoLogger(aMemo: TRichEdit; aProgressLabel: TLabel; aMaxLogLines: Integer): TMemoLogger;
begin
  Result := TMemoLogger.Create(Log, aMemo, aProgressLabel, aMaxLogLines);
end;

{ TMemoLogger }

procedure TMemoLogger.Clear;
begin
  fMemo.Clear;
end;

constructor TMemoLogger.Create(aLog: TLog; aMemo: TRichEdit; aProgressLabel: TLabel; aMaxLogLines: Integer);
begin
  fMemo := aMemo;
  fProgressLabel := aProgressLabel;
  fMaxLogLines := aMaxLogLines;
  inherited Create(aLog);
  ResetLogDef([llStart, llFinish, llStamp], [llsTime, llsID]);
end;

destructor TMemoLogger.Destroy;
begin
  inherited;
  fMemo := nil;
  fProgressLabel := nil;
end;

procedure TMemoLogger.LeaveProgress;
begin
  if Assigned(fProgressLabel)
  then fProgressLabel.Caption := '';
end;

procedure TMemoLogger.Progress(const aLine: string);
begin
  if Assigned(fProgressLabel)
  then fProgressLabel.Caption := aLine;
end;

procedure TMemoLogger.SetMemo(const Value: TRichEdit);
begin
  fMemo := Value;
end;

procedure TMemoLogger.SetProgressLabel(const Value: TLabel);
begin
  fProgressLabel := Value;
end;

procedure TMemoLogger.WriteLn(const aLine: string; aLevel: TLogLevel);
begin
  if Assigned(fMemo) then
  begin
    while fMemo.Lines.Count >= MaxLogLines
    do fMemo.Lines.Delete(0);
    fMemo.SelLength := 0;
    fMemo.SelStart := Length(fMemo.Text);
    case aLevel of
      llRemark:
        fMemo.SelAttributes.Color := clBlue;
      llWarning:
        fMemo.SelAttributes.Color := $00A0A0; // clOlive; //clYellow;
      llError:
        fMemo.SelAttributes.Color := clRed;
      llOK:
        fMemo.SelAttributes.Color := clGreen;
    else
        fMemo.SelAttributes.Color := fMemo.Font.Color;
    end;
    fMemo.SelText := aLine + #$0D + #$0A;
  end;
end;

end.
