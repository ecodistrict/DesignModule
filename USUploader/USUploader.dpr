program USUploader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

function ConvertToWGS84(const aPoint: TPoint): TPoint;
begin
  // we'll let oracle or postgis handle this
  // epsg wgs84: 4326
  // epsg rd: 28992
  // geojson: only wgs84 -> oralce handling transformation
end;

procedure UploadTable(const aSrcConnection: T


// odac to firedac pg

// https://docs.oracle.com/cd/E17952_01/refman-5.7-en/spatial-geojson-functions.html
// https://docs.oracle.com/html/A88805_01/sdo_cs_r.htm#81118

// http://postgis.net/docs/ST_Transform.html


procedure Upload(const aSrvConnectString, aDstConnectString: string);
var
  srvConnection: string;
  dstConnection: string;
begin
  // upload buildings

  // upload roads

end;

begin
  try

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
