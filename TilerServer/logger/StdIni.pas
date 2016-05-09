unit StdIni;

// written by J.A. Cornelissen

interface

uses
  CmdLin,
  Classes, Windows, IniFiles, SysUtils;

const
  SettingsSection                      = 'Settings';

var
  StandardIni                          : TIniFile;

// just ini
function  GetStdIniSetting             ( const aSwitch: string; const aDefaultValue: string=''; const aSection: string=SettingsSection): string; overload;
function  GetStdIniSetting             ( const aSwitch: string; aDefaultValue: Integer; const aSection: string=SettingsSection): Integer; overload;
function  GetStdIniSetting             ( const aSwitch: string; aDefaultValue: Boolean; const aSection: string=SettingsSection): Boolean; overload;

// first commandline, then ini
function  GetSetting                   ( const aSwitch: string; const aDefaultValue: string=''; const aSection: string=SettingsSection): string; overload;
function  GetSetting                   ( const aSwitch: string; aDefaultValue: Integer; const aSection: string=SettingsSection): Integer; overload;
function  GetSetting                   ( const aSwitch: string; aDefaultValue: Boolean; const aSection: string=SettingsSection): Boolean; overload;

function  GetSettingExists             ( const aSwitch: string; const aSection: string=SettingsSection): Boolean;


// store commandline override
function  CheckAndStoreStdIniSetting   ( const aSwitch: string; const aSection: string=SettingsSection): Boolean;

function  IniSections                  : TArray<string>;

procedure SetStdIniFileFolder          ( const aIniFolder: string);

implementation

function GetStdIniSetting(const aSwitch, aDefaultValue, aSection: string): string;
begin
  Result := StandardIni.Readstring(aSection, aSwitch, aDefaultValue);
end;

function GetStdIniSetting(const aSwitch: string; aDefaultValue: Integer; const aSection: string): Integer;
begin
  Result := StandardIni.ReadInteger(aSection, aSwitch, aDefaultValue);
end;

function GetStdIniSetting(const aSwitch: string; aDefaultValue: Boolean; const aSection: string): Boolean;
begin
  Result := StandardIni.ReadBool(aSection, aSwitch, aDefaultValue);
end;

function GetSetting(const aSwitch, aDefaultValue, aSection: string): string;
begin
  if CommandLine.TestSwitch(aSwitch)
  then Result := CommandLine.GetSwitch(aSwitch)
  else Result := GetStdIniSetting(aSwitch, aDefaultValue, aSection);
end;

function GetSetting(const aSwitch: string; aDefaultValue: Integer; const aSection: string): Integer;
begin
  if CommandLine.TestSwitch(aSwitch)
  then Result := StrToIntDef(CommandLine.GetSwitch(aSwitch), aDefaultValue)
  else Result := GetStdIniSetting(aSwitch, aDefaultValue, aSection);
end;

function GetSetting(const aSwitch: string; aDefaultValue: Boolean; const aSection: string): Boolean;
begin
  if CommandLine.TestSwitch(aSwitch)
  then Result := (AnsiCompareText(CommandLine.GetSwitch(aSwitch), 'FALSE')<>0) OR (CommandLine.GetSwitch(aSwitch)<>'0')
  else Result := GetStdIniSetting(aSwitch, aDefaultValue, aSection);
end;

function GetSettingExists(const aSwitch, aSection: string): Boolean;
begin
  Result := CommandLine.TestSwitch(aSwitch) or StandardIni.ValueExists(aSection, aSwitch);
end;

function CheckAndStoreStdIniSetting(const aSwitch, aSection: string): Boolean;
// Look out for boolean switch store,  command line can be FALSE but ini must be 0
begin
  if CommandLine.TestSwitch(aSwitch) then
  begin
    StandardIni.WriteString(aSection, aSwitch, CommandLine.GetSwitch(aSwitch));
    Result := True;
  end
  else Result := False;
end;

function IniSections: TArray<string>;
var
  sections: TStringList;
  i: Integer;
begin
  sections := TStringList.Create;
  try
    StandardIni.ReadSections(sections);
    setLength(Result, sections.count);
    for i := 0 to sections.Count-1
    do Result[i] := sections[i];
  finally
    sections.Free;
  end;
end;

function GetThisModuleFileName: string;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  if GetModuleFileName(HInstance, Buf, SizeOf(Buf) div SizeOf(Char))<>0
  then Result := Buf
  else Result := '';
end;

procedure SetStdIniFileFolder(const aIniFolder: string);
var
  iniFileName: string;
begin
  iniFileName :=
    IncludeTrailingPathDelimiter(aIniFolder) +
    ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
  StandardIni.Free;
  StandardIni := TIniFile.Create(iniFileName);
end;

initialization
  if IsLibrary
  then StandardIni := TIniFile.Create(ChangeFileExt(GetThisModuleFileName, '.ini'))
  else StandardIni := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
finalization
  StandardIni.Free;
  StandardIni := nil;
end.

