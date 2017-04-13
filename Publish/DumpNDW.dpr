program DumpNDW;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  NDWLib,
  System.SysUtils;


procedure Test;
var
  connection: TNDWConnection;
begin
  connection := TNDWConnection.Create('app-usmodel01.tsn.tno.nl', 4000);
  try
    connection.LoadLinkInfoFromFile('c:\temp\ndw.world');
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
