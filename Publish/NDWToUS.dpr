program NDWToUS;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Logger, LogConsole,
  WorldDataCode,
  NDWLib,

  Ora,
  OraObjects,
  Data.DB,
  OraSmart,
  MyOraLib,

  System.Generics.Collections,
  System.SysUtils;

function NullOnNaN(aValue: Double): string;
begin
  if not aValue.IsNaN
  then Result := aValue.ToString(dotFormat)
  else Result := 'null';
end;

function geometryToSDOCoords(aGeometry: TWDGeometry): string;
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
  x,y: Single;
begin
  if Assigned(aGeometry) then
  begin
    Result := '';
    for part in aGeometry.parts do
    begin
      for point in part.points do
      begin
        if Result<>''
        then Result := Result+',';
        x := point.x;
        y := point.y;
        Result := Result+x.ToString(dotFormat)+','+y.ToString(dotFormat);
      end;
    end;
    Result := 'MDSYS.SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),MDSYS.SDO_ORDINATE_ARRAY('+result+'))';
  end
  else Result := 'null';
end;

var
  connection: TNDWConnection;
  session: TOraSession;
  prefix: string;
  ilp: TPair<TNDWLinkID, TNDWLink>;
  i: Integer;
begin
  try
    connection := TNDWConnection.Create(
      'app-usmodel01.tsn.tno.nl', 4000,
      'us_ams_2017#v7', 'v7#',
      'us_ams_2017/us_ams_2017@app-usdata01.tsn.tno.nl/uspsde');
    try
      connection.LoadLinkInfoFromFile('c:\temp\ndw.world');
      WriteLn('read '+connection.links.Count.toString+' from c:\temp\ndw.world');
      session := TOraSession.Create(nil);
      try
        session.ConnectString := 'us_ams_2017/us_ams_2017@app-usdata01.tsn.tno.nl/uspsde';
        session.Open;
        prefix := 'V7#';

        TMonitor.Enter(connection.links);
        try
          session.ExecSQL('DELETE FROM '+prefix+'GENE_ROAD WHERE OBJECT_ID>1');
          i := connection.links.Count;
          for ilp in connection.links do
          begin
            Log.Progress(i.toString+': '+ilp.Key.ToString);
            i := i-1;
            //ilp.Value
            session.ExecSQL(
              'INSERT INTO '+prefix+'GENE_ROAD '+
                '(SLOPE,MATERIAL,STATUS,OBJECT_ID,FNODE_,TNODE_,LPOLY_,RPOLY_,'+
                'LENGTH,STRAATNAAM,ZONE,SNELHEID,WEGTYPE,CONGESTION_FACTOR,TREECODE,HOOGTEWEG,GROUND_LEVEL,'+
                'SRM2TYPE,RIJBANEN,WEGDEK_COD,BEBKOM_,WIDTH,'+
                'PCUURD,PCLID,PCMZD,PCZWD,PCUURA,PCLIA,PCMZA,PCZWA,PCUURN,PCLIN,PCMZN,PCZWN,PCMOD,PCMOA,PCMON,PCBUD,PCBUA,PCBUN,'+
                'SPEEDPADAG,SPEEDPANCT,SPEEDMZDAG,SPEEDMZNCT,SPEEDVVDAG,SPEEDVVNCT,VOLUME24H,DISTRICT_ID,INT_DHV,'+
                'CAPACITY_L,CAPACITY_R,SPEED_R,SPEED_L,STATUS_L,STATUS_R,CALCULATION_AREA,CONSTRUCTION_TYPE,MBS_ROAD_ID,'+
                'COST_TYPE,COST_TYPE_ADD,COST_CALC,ROUTE_ID,'+
                'SHAPE) '+
              'values '+
                '(null,null,null,'+(i+1000).ToString+','+ilp.Key.ToString+',0,null,null,'+
                ''+NullOnNaN(ilp.Value.length)+','''',null,null,null,null,null,null,null,null,'+
                'null,null,null,null,6.55,'+
                '94.5,4.5,1,3.675,97.2999,2,0.7,0.8375,94.5993,3.8002,1.6005,null,null,null,null,null,null,'+
                'null,null,null,null,null,null,null,null,null,'+
                '1400,1400,50,50,null,null,1,null,null,'+
                'null,null,null,null,'+
                geometryToSDOCoords(ilp.Value.geometry)+')');
            // todo: fill gene_road_intensity!
            session.Commit;
          end;
        finally
          TMonitor.Exit(connection.links);
        end;
        WriteLn('ready uploading roads');
        ReadLn;

      finally
        session.Free;
      end;
    finally
      connection.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
