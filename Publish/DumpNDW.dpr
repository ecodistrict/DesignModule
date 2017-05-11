program DumpNDW;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  NDWLib,
  System.SysUtils;


procedure Test;
var
  connection: TNDWConnection;
  scenario: string;
begin
  scenario := 'v11';

  connection := TNDWConnection.Create(
    'app-usmodel01.tsn.tno.nl', 4000, 'NDW',
    'vps17642.public.cloudvps.com', 4000, 'us_ams_2017#'+scenario,
    scenario+'#', 'us_ams_2017/us_ams_2017@app-usdata01.tsn.tno.nl/uspsde');
  try
    connection.LoadLinkInfoFromFile('c:\temp\ndw'+FormatDateTime('yyyy-mm-dd_hh-mm-ss', now)+'.world');
    WriteLn(connection.links.Count);
    ReadLn;
    connection.Dump;
    ReadLn;
  finally
    connection.Free;
  end;
end;



begin
  try
    Test;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
