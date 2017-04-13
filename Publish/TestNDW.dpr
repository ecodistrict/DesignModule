program TestNDW;

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
    WriteLn('Waiting for events');
    ReadLn;
    connection.SaveLinkInfoToFile('c:\temp\ndw.world');
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
