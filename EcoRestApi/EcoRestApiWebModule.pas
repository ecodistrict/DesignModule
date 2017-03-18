unit EcoRestApiWebModule;

interface

uses
  Logger, LogFile,
  StdIni,
  PublishServerDB,

  System.JSON,
  System.Generics.Collections,

  System.SysUtils, System.Classes, Web.HTTPApp, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Phys.PGDef, FireDAC.Phys.PG, FireDAC.Comp.UI, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

const
  EcodistrictCasePrefix = 'trout_'; // Nicolas's prefix to avoid schema names starting with numbers
  EcodistrictBaseScenario = 'undefined';

  dotFormat: TFormatSettings = (DecimalSeparator: '.');

  HSC_SUCCESS_OK = 200;
  HSC_ERROR_BADREQUEST = 400;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
    procedure WebModuleException(Sender: TObject; E: Exception; var Handled: Boolean);
    procedure WebModule1GPSToBuildingHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1AllBuildingsHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    fDBConnection: TFDConnection;
  public
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function EcoDistrictSchemaId(const aCaseId: string; const aVariantId: string=''): string;
begin
  if (aVariantId='') or (aVariantId='null') or (aVariantId='None')
  then Result := EcoDistrictCasePrefix + aCaseId
  else Result := EcoDistrictCasePrefix + aCaseId + '_' + aVariantId;
end;

procedure TWebModule1.WebModule1AllBuildingsHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  caseId: string;
  variantId: string;
  schema: string;
  query: TFDQuery;
  jsonArray: TJSONArray;
  rowObject: TJSONObject;
  jsonResponse: TJSONObject;
  propertiesObject: TJSONObject;
begin
  try
    jsonResponse := TJSONObject.Create;
    try
      // decode parameters
      caseId := Request.QueryFields.Values['caseId'];
      if caseId<>'' then
      begin
        variantId := Request.QueryFields.Values['variantId'];
        // set correct schema
        schema := EcoDistrictSchemaId(caseId, variantId);
        // execute query
        jsonArray := TJSONArray.Create; // will become part of jsonResponse
        try
          query := TFDQuery.Create(nil);
          try
            query.Connection := fDBConnection;
            // todo: get query from ini or database ?
            query.SQL.Text :=
              FDSingleStringResultQuery(fDBConnection,
                'SELECT query '+
                'FROM '+EcoDistrictSchemaId(caseId)+'.di_restapi '+
                'WHERE name=''AllBuildings''').
                  Replace('{case_id}',schema);
              //'SELECT object_id, ST_AsGeoJSON(lod0footprint)::text, street_name, zip_code '+
              //'FROM '+schema+'.building';
            query.Open;
            while not query.Eof do
            begin
              rowObject := TJSONObject.Create;
              try
                rowObject.AddPair('geometry', TJSONObject.ParseJSONValue(query.Fields[1].AsString));
                propertiesObject := TJSONObject.Create;
                try
                  propertiesObject.AddPair('id', TJSONString.Create(query.Fields[0].AsString));
                  propertiesObject.AddPair('street', TJSONString.Create(query.Fields[2].AsString));
                  propertiesObject.AddPair('zip', TJSONString.Create(query.Fields[3].AsString))
                finally
                  rowObject.AddPair('properties', propertiesObject);
                end;
              finally
                jsonArray.Add(rowObject);
              end;
              query.Next;
            end;
          finally
            query.Free;
          end;
        finally
          jsonResponse.AddPair('buildings', jsonArray);
        end;
        Response.StatusCode := HSC_SUCCESS_OK;
        Response.ContentType := 'application/json';
        Response.Content := jsonResponse.ToString;
        Handled := True;
      end
      else
      begin
        Response.StatusCode := HSC_ERROR_BADREQUEST;
        Response.ContentType := 'text/plain';
        Response.Content := 'No caseId specified for querying all buildings';
      end;
    finally
      jsonResponse.Free;
    end;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception in TWebModule1.WebModule1AllBuildingsHandlerAction caseId: '+caseId+', variantId: '+variantId+', message: '+E.Message, llError);
      Response.StatusCode := HSC_ERROR_BADREQUEST;
      Response.ContentType := 'text/plain';
      Response.Content := 'Exception during querying all buildings for caseId: '+caseId+', variantId: '+variantId+', message: '+E.Message;
    end;
  end;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Rest api for Ecodistrict</title></head>' +
    '<body>'+
      'This is the REST api for the Ecodistrict project<br>'+
      '<br>'+
      '/allbuildings?caseId=(caseId)[&variantId=(variantId)]<br>'+
      '/gpstobuilding?caseId=(caseId)[&variantId=(variantId)]&lat=(lat)&lon=(lon)<br>'+
      '<br>'+
      'return json array of buildings<br>'+
      //'<a href="http://localhost/EcoRestApi/EcoRestApi.dll/gpstobuilding?caseId=57b428a6cef25a0a0d6681ac&lat=51.1891682839691&lon=4.37702442308985">test</a><br>'+
    '</body>' +
    '</html>';
end;

procedure TWebModule1.WebModule1GPSToBuildingHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  caseId: string;
  variantId: string;
  schema: string;
  query: TFDQuery;
  jsonArray: TJSONArray;
  rowObject: TJSONObject;
  lat: Double;
  lon: Double;
  jsonResponse: TJSONObject;
  propertiesObject: TJSONObject;
begin
  try
    jsonResponse := TJSONObject.Create;
    try
      // decode parameters
      caseId := Request.QueryFields.Values['caseId'];
      if caseId<>'' then
      begin
        variantId := Request.QueryFields.Values['variantId'];
        lat := Double.Parse(Request.QueryFields.Values['lat'], dotFormat);
        lon := Double.Parse(Request.QueryFields.Values['lon'], dotFormat);
        // set correct schema
        schema := EcoDistrictSchemaId(caseId, variantId);
        // execute query
        jsonArray := TJSONArray.Create; // will become part of jsonResponse
        try
          query := TFDQuery.Create(nil);
          try
            query.Connection := fDBConnection;
            query.SQL.Text := FDSingleStringResultQuery(fDBConnection,
              'SELECT query '+
              'FROM '+EcoDistrictSchemaId(caseId)+'.di_restapi '+
              'WHERE name=''GPSToBuilding''').
                Replace('{case_id}',schema).
                Replace('{lat}', lat.toString(dotFormat)).
                Replace('{lon}', lon.toString(dotFormat));
//              'SELECT object_id, ST_AsGeoJSON(lod0footprint)::text, street_name, zip_code '+
//              'FROM '+schema+'.building '+
//              'WHERE point''('+lon.toString(dotFormat)+','+lat.toString(dotFormat)+')'' <@ polygon(lod0footprint)';
            query.Open;
            while not query.Eof do
            begin
              rowObject := TJSONObject.Create;
              try
                rowObject.AddPair('geometry', TJSONObject.ParseJSONValue(query.Fields[1].AsString));
                propertiesObject := TJSONObject.Create;
                try
                  propertiesObject.AddPair('id', TJSONString.Create(query.Fields[0].AsString));
                  propertiesObject.AddPair('street', TJSONString.Create(query.Fields[2].AsString));
                  propertiesObject.AddPair('zip', TJSONString.Create(query.Fields[3].AsString))
                finally
                  rowObject.AddPair('properties', propertiesObject);
                end;
              finally
                jsonArray.Add(rowObject);
              end;
              query.Next;
            end;
          finally
            query.Free;
          end;
        finally
          jsonResponse.AddPair('buildings', jsonArray);
        end;
        Response.StatusCode := HSC_SUCCESS_OK;
        Response.ContentType := 'application/json';
        Response.Content := jsonResponse.ToString;
        Handled := True;
      end
      else
      begin
        Response.StatusCode := HSC_ERROR_BADREQUEST;
        Response.ContentType := 'text/plain';
        Response.Content := 'No caseId specified for querying gps to building';
      end;
    finally
      jsonResponse.Free;
    end;
  except
    on E: Exception do
    begin
      Log.WriteLn('Exception in TWebModule1.WebModule1GPSToBuildingHandlerAction caseId: '+caseId+', variantId: '+variantId+', message: '+E.Message, llError);
      Response.StatusCode := HSC_ERROR_BADREQUEST;
      Response.ContentType := 'text/plain';
      Response.Content := 'Exception during querying gps to building for caseId: '+caseId+', variantId: '+variantId+', message: '+E.Message;
    end;
  end;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  Log.WriteLn('TWebModule1.WebModuleCreate');
  // establish connection
  InitPG;
  fDBConnection.Free;
  fDBConnection := TFDConnection.Create(nil);
  SetPGConnection(fDBConnection as TFDConnection,
    'User_Name='+GetSetting('EcoDBUserName', '')+';'+
    'Password='+GetSetting('EcoDBPassword', '')+';'+
    'Server='+GetSetting('EcoDBServer', '')+';'+
    'Port='+GetSetting('EcoDBPort', '5432')+';'+
    'Database='+GetSetting('EcoDBDatabase', 'Warsaw')+';'+
    'PGAdvanced=sslmode=require');
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  Log.WriteLn('TWebModule1.WebModuleDestroy');
  // cleanup
  FreeAndNil(fDBConnection);
  ExitPG;
end;

procedure TWebModule1.WebModuleException(Sender: TObject; E: Exception; var Handled: Boolean);
begin
  // log exception..
  Log.WriteLn('Exception in web module: '+E.Message, llError);
end;

initialization
  FileLogger.MakeModuleFileName;
  FileLogger.SetLogDef(AllLogLevels, [llsTime, llsThreadID]);
  Log.Start();
  // switch current folder to dll folder to load postgres dll's
  SetCurrentDir(ExtractFileDir(FileLogger.FileName));
  Log.WriteLn('Switched current folder to "'+ExtractFileDir(FileLogger.FileName)+'"');
finalization
  Log.Finish();
end.
