program Importer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  CmdLin, StdIni, IniFiles,
  Logger, LogFile, LogConsole,
  SessionServerDB,
  FireDAC.Comp.Client,
  System.Classes,
  System.SysUtils;

const
  TableNameSwitch = 'TableName';
    DefaultTableName = 'CSVImportedData';

  SchemaSwitch  = 'Schema';

  FieldNamesSection = 'FieldNames';
  FieldTypesSection = 'FieldTypes';
    DefaultFieldType = 'TEXT';
  FieldConversionsSection = 'FieldConversions';

  dotFormat: TFormatSettings = (DecimalSeparator: '.');
  CommaFormat: TFormatSettings = (DecimalSeparator: ',');


procedure ImportCSVFile(const aCSVFileName: string);
var
  ini: TIniFile;
  lines: TStringList;
  l: Integer;
  csvLine: TStringList;
  csvHeader: TStringList;
  f: Integer;
  sqlFileName: string;
  sqlFile: TextFile;
  fieldNames, fieldValues: string;
  tableName: string;
  fieldName: string;
  delimiter: string;
  fieldConversion: string;
  fieldType: string;
  fieldValue: string;
  fieldValueInt: Integer;
  fieldValueFloat: Double;
  iniFileName: string;
  fieldDef: string;
  pkFields: string;
  schema: string;
begin
  iniFileName := ExpandFileName(ChangeFileExt(aCSVFileName, '.ini'));
  Log.WriteLn('using ini file name: '+iniFileName);
  ini := TIniFile.Create(iniFilename);
  try
    sqlFileName := ChangeFileExt(aCSVFileName, '.sql');
    if sqlFileName=aCSVFileName
    then sqlFileName := ChangeFileExt(aCSVFileName, '')+'.out.sql';
    AssignFile(sqlFile, sqlFileName);
    Rewrite(sqlFile);
    try
      schema := ini.ReadString(SettingsSection, SchemaSwitch, '');
      schema := GetSetting(SchemaSwitch, schema);
      if schema<>'' then
      begin
        WriteLn(sqlFile, 'set schema '''+schema+''';');
        log.WriteLn('using schema '+schema);
      end
      else log.WriteLn('not using a specific schema', llWarning);
      lines := TStringList.Create;
      csvLine := TStringList.Create;
      csvHeader  := TStringList.Create;
      try
        lines.LoadFromFile(aCSVFileName);
        delimiter := ini.ReadString(SettingsSection, 'delimiter', 'tab').ToLower;
        if (delimiter='tab') or (delimiter='')
        then csvLine.Delimiter := #9
        else csvLine.Delimiter := delimiter[1];
        csvLine.StrictDelimiter := True;
        csvHeader.Delimiter := csvLine.Delimiter;
        csvHeader.StrictDelimiter := True;
        tableName := DefaultTableName;
        for l := 0 to lines.Count-1 do
        begin
          if l=0 then
          begin // header
            tableName := ini.ReadString(SettingsSection, TableNameSwitch, tableName);
            tableName := GetSetting(TableNameSwitch, tableName);
            Log.WriteLn('using table name: '+tableName);
            csvHeader.DelimitedText := lines[l];
            Log.WriteLn('header: '+csvHeader.Count.ToString);
            if not ini.SectionExists(FieldNamesSection) then
            begin
              ini.WriteString(SettingsSection, TableNameSwitch, tableName);
              if lines.Count>1
              then csvLine.DelimitedText := lines[l+1];
              for f := 0 to csvHeader.Count-1 do
              begin
                if lines.Count>1
                then Log.WriteLn(csvHeader[f]+': '+csvLine[f], llNormal, 1) // show example data
                else Log.WriteLn(csvHeader[f], llNormal, 1);
                ini.WriteString(FieldNamesSection, csvHeader[f], csvHeader[f]); // default use column as field name
                ini.WriteString(FieldTypesSection, csvHeader[f], DefaultFieldType); // default use column as field name
              end;
              Log.WriteLn('NO data processing, build conversion ini: '+ini.FileName, llWarning);
              Break;
            end
            else
            begin
              fieldNames := '';
              pkFields := '';
              for f := 0 to csvHeader.Count-1 do
              begin
                fieldName := ini.ReadString(FieldNamesSection, csvHeader[f], '');
                if fieldName<>'' then
                begin
                  fieldDef := '"'+fieldName+'" '+ini.ReadString(FieldTypesSection, csvHeader[f], DefaultFieldType);
                  if fieldNames='' then // todo: determine if pk field
                  begin
                    fieldDef := fieldDef+' not null';
                    if pkFields<>''
                    then pkFields := pkFields+',';
                    pkFields := pkFields+'"'+fieldName+'"';
                  end;
                  if fieldNames<>''
                  then fieldNames := fieldNames+','+fieldDef
                  else fieldNames := fieldDef;
                end;
              end;
              WriteLn(sqlFile, 'CREATE TABLE "'+tableName+'" ('+fieldNames+', CONSTRAINT "'+tableName+'_pkey" PRIMARY KEY ('+pkFields+'));');
              Log.WriteLn('added CREATE TABLE statement');
            end;
          end
          else
          begin // data
            csvLine.DelimitedText := lines[l];
            fieldValues := '';
            fieldNames := '';
            for f := 0 to csvLine.Count-1 do
            begin
              fieldName := ini.ReadString(FieldNamesSection, csvHeader[f], '');
              if fieldName<>'' then
              begin
                if fieldNames<>''
                then fieldNames := fieldNames+',';
                fieldNames := fieldNames+'"'+fieldName+'"';
                if fieldValues<>''
                then fieldValues := fieldValues+',';
                fieldValue := csvLine[f];
                fieldType := ini.ReadString(FieldTypesSection, csvHeader[f], DefaultFieldType);
                fieldConversion := ini.ReadString(FieldConversionsSection, csvHeader[f], '');
                // todo: fieldConversion (ie double , to .)
                if fieldType.ToLower='text' then
                begin
                  if (fieldValue<>'') and (fieldValue.Trim<>',') and (fieldValue.Trim<>'#N/A')
                  then fieldValues := fieldValues+''''+fieldValue+''''
                  else fieldValues := fieldValues+'NULL';
                end
                else if fieldType.ToLower='integer' then
                begin
                  fieldValue := fieldValue.Trim;
                  try
                    fieldValueInt := fieldValue.ToInteger;
                    fieldValue := fieldValueInt.ToString;
                  except
                    Log.WriteLn(l.toString+', '+fieldName+': NULL from int ('+fieldValue+')');
                    fieldValue := 'NULL';
                  end;
                  fieldValues := fieldValues+fieldValue;
                end
                else if fieldType.ToLower='numeric' then
                begin
                  fieldValue := fieldValue.Trim;
                  try
                    fieldValueFloat := StrToFloat(fieldValue, dotFormat);
                    fieldValue := fieldValueFloat.ToString(dotFormat);
                  except
                    try
                      fieldValueFloat := StrToFloat(fieldValue, CommaFormat);
                      fieldValue := fieldValueFloat.ToString(dotFormat);
                    except
                      Log.WriteLn(l.toString+', '+fieldName+': NULL from float ('+fieldValue+')');
                      fieldValue := 'NULL';
                    end;
                  end;
                  fieldValues := fieldValues+fieldValue;
                end
                else
                begin
                  Log.WriteLn(l.toString+', '+fieldName+': unknown type ('+fieldValue+')');
                  fieldValues := fieldValues+'"'+fieldValue+'"'
                end;
              end;
            end;
            WriteLn(sqlFile, 'INSERT INTO "'+tableName+'" ('+fieldNames+') VALUES ('+fieldValues+');');
            Log.Progress('line '+l.ToString);
          end;
        end;
      finally
        lines.Free;
        csvLine.Free;
        csvHeader.Free;;
      end;
    finally
      CloseFile(sqlFile);
    end;
  finally
    ini.free;
  end;
end;

var
  dbConnectString: string;
  dbConnection: TFDConnection;
  PGInited: Boolean = False;
  csvFileName: string;
begin
  try
    InitPG;
    dbConnectString :=
      'User_Name='+GetSetting('EcoDBUserName', '')+';'+
      'Password='+GetSetting('EcoDBPassword', '')+';'+
      'Server='+GetSetting('EcoDBServer', '')+';'+
      'Port='+GetSetting('EcoDBPort', '')+';'+
      'Database='+GetSetting('EcoDBDatabase', '')+';'+
      'PGAdvanced=sslmode=require';
    dbConnection := TFDConnection.Create(nil);
    try
      SetPGConnection(dbConnection as TFDConnection, dbConnectString);

      // read shape to table
      // todo: see procedure TNWBLiveFeedRoad.ConvertFromShape(aShape: TGIS_Shape);
      {
      fNWBShape := TGIS_LayerSHP.Create;
      // load shape file
      fNWBShape.Name := 'NWBWegvakken';
      fNWBShape.Path := aShapeFileName;
      fNWBShape.Open; // todo: call fNWBShape.Open; here so init is slow but first lookup is fast?
      // fill dictionary for lookups of wvk_id to uid in shape layer
      shape := fNWBShape.FindFirst(GisWholeWorld);
      objJSON := '';
      while Assigned(Shape) do
      begin
        fWVKID2UID.AddOrSetValue(TWDID(shape.GetField('WVK_ID')), shape.Uid);
        shape := fNWBShape.FindNext;
      end;

      shape := fNWBShape.GetShape(uid);
      }

      // read csv file to table
      commandLine.FindFirst;
      csvFileName := commandLine.FindNext;
      if csvFileName<>'' then
      begin
        ImportCSVFile(csvFileName);
      end;
    finally
      dbConnection.Free;
      ExitPG;
    end;
  except
    on E: Exception do
      Log.Writeln(E);
  end;
end.
