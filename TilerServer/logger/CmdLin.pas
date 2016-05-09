unit CmdLin;

// written by J.A. Cornelissen

// unit for handling command lines, default the OS command line past to executable is used

{  After reading the given command line, default:CmdLine, the switches
   and arguments are collected and stored in ArgList and SwitchList.
   All tests are on the 2 lists
   Functions without the word Switch in their name opperate on ArgList,
   with the word Switch in their name operate on SwitchList

   ReadingArg and ReadingSwitch keep track of the index of the item to process
   in the FindFirst(Switch) en FindNext(Switch) functions

   The CommandLine object is initialized and freed in the unit (Initialization/Finalization).
   The default command line is the applications exe command line but can be
   replaced with any other by calling SetCommandLine
}


interface

{$WARN SYMBOL_PLATFORM OFF} 

uses
  Windows, Classes,SysUtils;

const
  HelpSwitch                           = '?';
  HelpSwitchChar                       = '/';

type
  TCommandLine = class
  constructor Create;
  destructor Destroy;override;
  private
    ArgList                            : TStringList; // all arguments on the given command line
    SwitchList                         : TStringList; // all switches on the given command line (switch=value)
    ReadingArg                         : Integer; // index of the argument item last read
    ReadingSwitch                      : Integer; // index of the switch last read
    function  FindSwitch               ( const aSwitch: String): Integer;
    function  GetCommandLine           : String;
    // Resets the command line to a user defined one
    procedure SetCommandLine           ( Const Value: String);
  public
    property  CommandLine              : String read GetCommandLine write SetCommandLine;

    // IsEmpty returns true if no arguments or switches are given on command line except the exe path (the first argument)
    function  IsEmpty                  : Boolean;

    // Returns the first argument on the given command line, normally the path to 'this' executable
    function  FindFirst                : String;
    // Returns the next argument on the given command line from the position of the last read argument
    function  FindNext                 : String;
    // Returns the argument at <Index> where 0 would return the path to 'this' executable
    function  Arg                      ( aIndex: Integer): String;

    // FindFirstSwitch returns the first switch found on the command line
    // it sets the reading index for switches to 1
    function  FindFirstSwitch          : String;
    // FindNextSwitch returns the next switch found on the command line from the
    // last switch position read
    function  FindNextSwitch           : String;

    // TestSwitch returns true if a switch <aSwitch> is found on the command line
    function  TestSwitch               ( const aSwitch: String): Boolean;
    // GetSwitch returns the value of switch <aSwitch> if it doesn't exist an empty value is returned ('')
    function  GetSwitch                ( const aSwitch: String): String;
    // GetSwitchDef returns the value of switch <aSwitch> if it exists on the command line
    // else it returns <aDefault>
    function  GetSwitchDef             ( const aSwitch, aDefault: String): String;

    // TestSwitches tests all switches on the given command line to <ValidSwitchList>
    // all not conforming sitches are returned with an * added if it could not identified uniquely
    // or complete if it could not be identified at all
    function  TestSwitches             ( const ValidSwitchList: String): String;

    // change commandline
    function  DeleteSwitch             ( const aSwitch: String): Boolean;
    function  DeleteArg                ( aIndex: Integer): Boolean;
    //function  SetArg                   ( aIndex: Integer; const aArgValue: String): Boolean;
    function  InsertArg                ( aIndex: Integer; const aArgValue: String): Boolean;
    function  SetSwitch                ( const aSwitchName: String; const aSwitchValue: String=''): Boolean;
  end;

var
  CommandLine: TCommandLine;

// Returns a comma delimited list of command line switches that represent the comma delimited
// list in SwitchListStr but with only the charakters needed to make each item unique within the list
function  MinimizeSwitches             ( const SwitchListStr: String): String;

// GetItemNum returns the match of aItem in ItemList (comma delimited list)
// aItem only has to match the start of an item in ItemList but has to only match 1 item from the list
// if aItem matches more then one item or no item -1 is returned
function  GetItemNum                   ( const aItem, ItemList: String): Integer;

// Extract the executable name without path and extension from the 0-paramter on the command line
function  GetExeName                   : String;
function  GetExePath                   : String;

// Help system
function  HelpRegisterSwitch           ( const aSwitch, aValue, aHelpLine: String; const aSection: String=''; aValueOptional: Boolean=False): Boolean;
function  HelpRegisterArgument         ( const aValue, aHelpLine: String; aOptional: Boolean=False): Boolean;
function  HelpRegisterComment          ( const aComment: String): Boolean;
function  HelpRegisteredSwitches       : String;
// shows registered help list
function  HelpShow                     : Boolean;
// test if help should be shown, return state and shows help (true if help is shown)
function  HelpShowTest                 : Boolean;

implementation

const
  // defines what charakters can be used as switch charakter (if changed change TestQuotes too)
  SwitchStartSet                       : set of AnsiChar=['-','/'];
  SpaceSet                             : set of AnsiChar=[' '];
  // defines what charakters can be used to start the value of a switch
  EndSwitchSet                         : set of AnsiChar=[':','='];

// help functions support const and var
var
  HelpSwitchList:TStringList;
  HelpTextList:TStringList;
  HelpComments:TStringList;
  HelpNotOptionalArguments:Boolean;

const
  SwitchPre                            = '      ';
  SectionPre                           = '   ';
  HelpValueChar                        = ':';
  HelpSwitchHelpLine                   = 'this screen';



{ parsing functions }

function FindNonSpace(var i:Integer;const s:String):Boolean;
begin
  while (i<=Length(s)) AND (AnsiChar(s[i]) in SpaceSet) do i:=i+1;
  Result:=i<=Length(s);
end;

procedure FindSpace(var i:Integer;const s:String);
begin
  while (i<=Length(s)) AND NOT (AnsiChar(s[i]) in SpaceSet)
  do i:=i+1;
end;

function FindEndString(var i:Integer;const s:String):Boolean;
begin
  i:=i+1;
  while (i<=Length(s)) AND (s[i]<>'"') do i:=i+1;
  Result:=i<=Length(s);
end;

function FindEndSwitchName(var i:Integer;const s:String):Boolean;
begin
  while (i<=Length(s)) AND NOT (AnsiChar(s[i]) IN (SpaceSet+EndSwitchSet)) do i:=i+1;
  Result:=(i<=Length(s)) AND (AnsiChar(s[i]) in EndSwitchSet);
end;

function ArgNeedesQuotes(const aArg: String): Boolean;
var
  i:Integer;
begin
  i:=1;
  while (i<=Length(aArg)) AND NOT (AnsiChar(aArg[i]) IN SwitchStartSet+SpaceSet+EndSwitchSet)
  do i:=i+1;
  Result:=i<=Length(aArg);
end;

function ReadArg(var i:Integer;const s:String):String;
var
  Start:Integer;
begin
  if i<=Length(s) then
  begin
    // save start
    Start:=i;
    // if we find a " search for counter part
    if s[i]='"' then
    begin
      // read and strip "
      FindEndString(i,s);
      Result:=Copy(s,Start+1,i-Start-1);
      i:=i+1; // set past " on end of arg
    end
    else
    begin
      // No " found so stop reading till next space
      FindSpace(i,s);
      Result:=Copy(s,Start,i-Start);
    end;
  end
  else Result:='';
end;

function MinimizeSwitches(const SwitchListStr:String):String;
var
  Switches:TStringList;

  function TestConflict(const sw:String;si:Integer):Boolean;
  var
    i:Integer;
  begin
    Result:=False;
    for i:=0 to Switches.Count-1 do
    begin
      if i<>si then
      begin
        if Copy(Switches[i],1,Length(sw))=sw
        then Result:=True;
      end;
    end;
  end;

var
  Switch:String;
  SwitchIndex:Integer;
  ProposedSwitchLength:Integer;
begin
  Switches:=TStringList.Create;
  try
    Switches.CommaText:=Uppercase(SwitchListStr);
    for SwitchIndex:=0 to Switches.Count-1 do
    begin
      Switch:=Switches[SwitchIndex];
      ProposedSwitchLength:=1;
      while (ProposedSwitchLength<Length(Switch)) AND TestConflict(Copy(Switch,1,ProposedSwitchLength),SwitchIndex)
      do ProposedSwitchLength:=ProposedSwitchLength+1;
      if ProposedSwitchLength<Length(Switch)
      then Switches[SwitchIndex]:=Copy(Switch,1,ProposedSwitchLength)
      else
      begin // Test if switch isn't in conflict, if so add # for conflicting part
        while TestConflict(Switch,SwitchIndex) do Switch:=Switch+'#';
        if ProposedSwitchLength<Length(Switch)
        then Switches[SwitchIndex]:=Switch;
      end;
    end;
    Result:=Switches.CommaText;
  finally
    Switches.Free;
  end;
end;

function GetExeName:String;
// Returns the name of the executable or dll without the directory part and without the exe or dll extension
begin
  Result:=ChangeFileExt(ExtractFileName(ParamStr(0)),'')
end;

function  GetExePath:String;
begin
  Result:=ExtractFilePath(ParamStr(0));
end;

function GetItemNum(const aItem,ItemList:String):Integer;
var
  List:TStringList;
  i:Integer;
  fc:Integer;
begin
  List:=TStringList.Create;
  try
    List.CommaText:=ItemList;
    Result:=-1;
    // keep track of the number of matching items
    fc:=0;
    // go through the list
    for i:=0 To List.Count-1 do
    begin
      // check if it fits
      if AnsiCompareText(Copy(List[i],1,Length(aItem)),aItem)=0 then
      begin
        // return the last matching item
        Result:=i;
        // increment the matching count
        fc:=fc+1;
      end;
    end;
    // if the matching count isn't 1 as it should be return -1 as an error code
    if fc<>1
    then Result:=-1;
  finally
    List.Free;
  end;
end;

{ TCommandLine }

function TCommandLine.Arg(aIndex: Integer): String;
begin
  if (aIndex>=0) AND (aIndex<ArgList.Count)
  then Result:=ArgList[aIndex]
  else Result:='';
end;

constructor TCommandLine.Create;
begin
  inherited Create;
  ArgList:=TStringList.Create;
  SwitchList:=TStringList.Create;
  // Default indexes to "nothing read"
  ReadingArg:=-1;
  ReadingSwitch:=-1;
end;

function TCommandLine.DeleteArg(aIndex: Integer): Boolean;
begin
  if (aIndex>=0) AND (aIndex<ArgList.Count) then
  begin
    ArgList.Delete(aIndex);
    Result:=True;
  end
  else Result:=False;
end;

function TCommandLine.DeleteSwitch(const aSwitch: String): Boolean;
var
  si:Integer;
begin
  Result:=False;
  si:=FindSwitch(aSwitch);
  while si>=0 do
  begin
    SwitchList.Delete(si);
    Result:=True;
    si:=FindSwitch(aSwitch);
  end;
end;

destructor TCommandLine.Destroy;
begin
  ArgList.Free;
  SwitchList.Free;
  inherited Destroy;
end;

function TCommandLine.FindFirst:String;
begin
  // reset index of item to read
  ReadingArg:=-1;
  // get the next item ie the first item
  Result:=FindNext;
end;

function TCommandLine.FindFirstSwitch:String;
begin
  // reset index of item to read
  ReadingSwitch:=-1;
  // get the next item ie the first item
  Result:=FindNextSwitch;
end;

function TCommandLine.FindNext:String;
begin
  // go for the next item
  ReadingArg:=ReadingArg+1;
  // check if there is one left to read
  if ReadingArg<ArgList.Count
  // return the argument
  then Result:=ArgList[ReadingArg]
  // no arguments left to read, return an empty string
  else Result:='';
end;

function TCommandLine.FindNextSwitch:String;
begin
  // go for the next item
  ReadingSwitch:=ReadingSwitch+1;
  // check if there is one left to read
  if ReadingSwitch<SwitchList.Count
  // return the switch
  then Result:=SwitchList.Names[ReadingSwitch]
  // no switches left to read, return an empty string
  else Result:='';
end;

function TCommandLine.FindSwitch(const aSwitch:String):Integer;
var
  si:Integer;
begin
  // Sentinel
  Result:=-1;
  // We are going to walk through the list to find the switch or else we use the sentinel
  si:=0;
  while si<SwitchList.Count do
  begin
    // Only the check the given part of the switch
    if AnsiCompareText(Copy(aSwitch,1,Length(SwitchList.Names[si])),SwitchList.Names[si])=0 then
    begin // found it
      Result:=si;
      si:=SwitchList.Count;
    end
    // not found, check the next
    else si:=si+1;
  end;
end;

function TCommandLine.GetCommandLine: String;
var
  i:Integer;
  SwitchValue:String;
  SwitchName:String;
  p:Integer;
begin
  Result:='';
  for i:=0 to ArgList.Count-1 do
  begin
    // add extra seperator space
    if Result<>''
    then Result:=Result+' ';
    // add arg
    if ArgNeedesQuotes(ArgList[i])
    then Result:=Result+'"'+ArgList[i]+'"'
    else Result:=Result+ArgList[i];
  end;
  for i:=0 to SwitchList.Count-1 do
  begin
    SwitchValue:=SwitchList[i];
    p:=Pos('=',SwitchValue);
    if p=0 then
    begin // invalid switch entry, use it as if it were just switch name
      SwitchName:=SwitchValue;
      SwitchValue:='';
    end
    else
    begin
      SwitchName:=Copy(SwitchValue,1,p-1);
      Delete(SwitchValue,1,p);
    end;
    if SwitchName<>'' then
    begin
      // add extra seperator space
      if Result<>''
      then Result:=Result+' ';
      // add switch
      if SwitchValue<>'' then
      begin // switch name AND value
        if ArgNeedesQuotes(SwitchValue)
        then Result:=Result+HelpSwitchChar+SwitchName+HelpValueChar+'"'+SwitchValue+'"'
        else Result:=Result+HelpSwitchChar+SwitchName+HelpValueChar+SwitchValue;
      end
      else Result:=Result+HelpSwitchChar+SwitchName; // only switch name
    end;
    // else illegal switch entry ??
  end;
end;

function TCommandLine.GetSwitch(const aSwitch:String):String;
// Gets the value of the given switch if it exists else it returns an empty
var
  si:Integer;
begin
  // Find the switch, can be part of the name
  si:=FindSwitch(aSwitch);
  if si>=0
  // found the switch, get the value
  then Result:=SwitchList.Values[SwitchList.Names[si]]
  // switch not found, return the empty string
  else Result:='';
end;

function TCommandLine.GetSwitchDef(const aSwitch,aDefault:String):String;
// returns a given default value if the switch is not found
begin
  if TestSwitch(aSwitch) then
  begin
    Result:=GetSwitch(aSwitch);
    if Result=''
    then Result:=aDefault;
  end
  else Result:=aDefault;
end;

function TCommandLine.InsertArg(aIndex: Integer; const aArgValue: String): Boolean;
begin
  if (aIndex>=0) AND (aIndex<=ArgList.Count) then
  begin
    ArgList.Insert(aIndex,aArgValue);
    Result:=True;
  end
  else Result:=False;
end;

function TCommandLine.IsEmpty:Boolean;
begin
  // ArgList[0] is always path to executable, skip for check
  Result:=(ArgList.Count<=1) AND (SwitchList.Count=0);
end;

procedure TCommandLine.SetCommandLine(const Value:String);
var
  Position:Integer;
  PositionStart:Integer;
  SwitchName:String;
  SwitchValue:String;
  ArgValue:String;
begin
  ArgList.Clear;
  SwitchList.Clear;
  // Decode whole string to argumenrs and switches
  Position:=1;
  while FindNonSpace(Position,Value) do
  begin
    if AnsiChar(Value[Position]) in SwitchStartSet then
    begin // start of switch
      // Skip switch char
      Position:=Position+1;
      // Find start of switch name
      FindNonSpace(Position,Value);
      PositionStart:=Position;
      // find end of switch name
      if FindEndSwitchName(Position,Value) then
      begin // try to retrieve value with switch
        SwitchName:=Copy(Value,PositionStart,Position-PositionStart);
        // Read Value
        Position:=Position+1;
        SwitchValue:=ReadArg(Position,Value);
      end
      else
      begin // no value
        SwitchName:=Copy(Value,PositionStart,Position-PositionStart);
        SwitchValue:='';
      end;
      SwitchList.Add(SwitchName+'='+SwitchValue);
    end
    else
    begin // start of arg
      ArgValue:=ReadArg(Position,Value);
      ArgList.Add(ArgValue);
    end;
  end;
end;

function TCommandLine.SetSwitch(const aSwitchName, aSwitchValue: String): Boolean;
begin
  Result:=DeleteSwitch(aSwitchName);
  SwitchList.Add(aSwitchName+'='+aSwitchValue);
end;

function TCommandLine.TestSwitch(const aSwitch:String):Boolean;
// Test if the switch is present in the list
begin
  Result:=FindSwitch(aSwitch)>=0;
end;

function TCommandLine.TestSwitches(const ValidSwitchList:String):String;
var
  Switch:String;
  vs:Integer;
  ValidSwitches:TStringList;
  FoundCount:Integer;
begin
  ValidSwitches:=TStringList.Create;
  try
    // Sentinel
    Result:='';
    // Check if the switch name isn't to long, if it exists in the list at all
    // or if it can't be interpreted in more then one way
    ValidSwitches.CommaText:=ValidSwitchList;
    Switch:=FindFirstSwitch;
    while Switch<>'' do
    begin
      FoundCount:=0;
      for vs:=0 To ValidSwitches.Count-1 do
      begin
        if AnsiCompareText(Copy(ValidSwitches[vs],1,Length(Switch)),Switch)=0
        then FoundCount:=FoundCount+1;
      end;
      if FoundCount<>1 then
      begin // we haven't found a valid switch so return it
        if Result<>'' // seperate returned switches with space
        then Result:=Result+' ';
        if FoundCount=0
        then Result:=Result+'/'+Switch
        // if more then one interpretation is possible mark the switch with an asterix
        else Result:=Result+'/'+Switch+'*';
      end;
      Switch:=FindNextSwitch;
    end;
  finally
    ValidSwitches.Free;
  end;
end;

// help system

function HelpInit:Boolean;
begin
  if NOT Assigned(HelpTextList) then
  begin
    HelpTextList:=TStringList.Create;
    HelpComments:=TStringList.Create;
    HelpTextList.Add('Use: '+GetExeName);
  end;
  if NOT Assigned(HelpSwitchList) then
  begin
    HelpSwitchList:=TStringList.Create;
    HelpSwitchList.Sorted:=True;
    HelpSwitchList.Duplicates:=dupIgnore;
    HelpNotOptionalArguments:=False;
  end;
  Result:=HelpTextList.Count>0;
end;

function HelpRegisterComment(const aComment:String):Boolean;
begin
  if HelpInit then
  begin
    HelpComments.Add('  '+aComment);
    Result:=True;
  end
  else Result:=False;
end;

function HelpRegisterSwitch(const aSwitch,aValue,aHelpLine:String;const aSection:String;aValueOptional:Boolean):Boolean;
var
  i:Integer;
begin
  if HelpInit then
  begin
    HelpSwitchList.Add(aSwitch);
    if aSection<>'' then
    begin
      // find section, if not found create section
      i:=HelpTextList.IndexOf(SectionPre+aSection);
      if i<0
      then i:=HelpTextList.Add(SectionPre+aSection)+1
      else i:=i+1;
    end
    else i:=1;
    // find position before other section, if not found just add to end of list
    while (i<HelpTextList.Count) AND (Copy(HelpTextList[i],1,Length(SwitchPre))=SwitchPre)
    do i:=i+1;
    if aValue<>'' then
    begin
      if aValueOptional
      then HelpTextList.Insert(i,SwitchPre+'['+HelpSwitchChar+aSwitch+HelpValueChar+'[<'+aValue+'>]] '+aHelpLine)
      else HelpTextList.Insert(i,SwitchPre+'['+HelpSwitchChar+aSwitch+HelpValueChar+'<'+aValue+'>] '+aHelpLine);
    end
    else HelpTextList.Insert(i,SwitchPre+'['+HelpSwitchChar+aSwitch+'] '+aHelpLine);
    Result:=True;
  end
  else Result:=False;
end;

function HelpRegisterArgument(const aValue,aHelpLine:String;aOptional:Boolean):Boolean;
var
  LokValue:String;
  i:Integer;
begin
  if HelpInit then
  begin
    LokValue:='<'+aValue+'>';
    if aOptional
    then LokValue:='['+LokValue+']'
    else HelpNotOptionalArguments:=True;
    HelpTextList[0]:=HelpTextList[0]+' '+LokValue;
    i:=1;
    while (i<HelpTextList.Count) AND (Copy(HelpTextList[i],1,Length(SwitchPre+'<'))=SwitchPre+'<')
    do i:=i+1;
    HelpTextList.Insert(i,SwitchPre+'<'+aValue+'> '+aHelpLine);
    Result:=True;
  end
  else Result:=False;
end;

function HelpRegisteredSwitches:String;
begin
  if HelpInit
  then Result:=HelpSwitchList.CommaText
  else Result:='';
end;

function HelpShow:Boolean;
begin
  if IsConsole then
  begin
    WriteLn(HelpTextList.Text);
    if HelpComments.Count>0 then
    begin
      WriteLn;
      WriteLn(HelpComments.Text);
    end;
    if HelpNotOptionalArguments AND CommandLine.IsEmpty then
    begin
      WriteLn;
      WriteLn('## No arguments specified');
    end;
    Result:=True;
  end
  else
  begin
    MessageBox(0,PChar(HelpTextList.Text+#13+HelpComments.Text),'Help with parameters',MB_OK);
    Result:=True;
  end;
end;

function HelpShowTest: Boolean;
begin
  if CommandLine.TestSwitch(HelpSwitch) OR (HelpNotOptionalArguments AND CommandLine.IsEmpty)
  then Result:=HelpShow
  else Result:=False;
end;

initialization
  CommandLine:=TCommandLine.Create;
  CommandLine.SetCommandLine(CmdLine);
  HelpRegisterSwitch(HelpSwitch,'',HelpSwitchHelpLine);
finalization
  FreeAndNil(CommandLine);
  FreeAndNil(HelpSwitchList);
  FreeAndNil(HelpTextList);
  FreeAndNil(HelpComments);
end.

