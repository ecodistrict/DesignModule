program TestNDW;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogFile, LogConsole,
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
    WriteLn('Waiting for events');
    ReadLn;
    //connection.SaveLinkInfoToFile('c:\temp\ndw.world');
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
