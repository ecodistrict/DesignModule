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
begin
  connection := TNDWConnection.Create(
    'app-usmodel01.tsn.tno.nl', 4000,
    'us_ams_2017#v7', 'v7#',
    'us_ams_2017/us_ams_2017@app-usdata01.tsn.tno.nl/uspsde');
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
