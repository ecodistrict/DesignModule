program DataConverter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  CmdLin,
  StdIni,
  Logger, LogFile, LogConsole,
  System.JSON,
  System.IOUtils,
  System.Classes,
  System.SysUtils;

procedure ConvertFile(const aInputFileName, aOutputFileName: string);
var
  outputLines: TStringList;
  jsonRoot: TJSONValue;
  jsonRootObject: TJSONObject;
  value: string;
  header: TStringList;
  features: TJSONArray;
  f: Integer;
  feature: TJSONValue;
  geometry: TJSONObject;
  properties: TJSONObject;
//  g: Integer;
  p: Integer;
//  geometryType: TJSONValue;
//  coordinates: TJSONArray;
//  jp: TJSONPair;
  name: string;
  geometryStr: string;
  csvLine: TStringList;
  jsonValue: TJSONValue;

  function checkColumn(const aName: string): Integer;
  begin
    Result := header.IndexOf(aName);
    if Result<0 then
    begin
      header.Add(aName);
      Result := header.Count-1;
    end;
  end;

  procedure setCSVValue(const aName, aValue: string);
  var
    c: Integer;
  begin
    c := checkColumn(aName);
    while csvLine.Count<=c
    do csvLine.Add('');
    csvLine[c] := aValue;
  end;

begin
  outputLines := TStringList.Create;
  header := TStringList.Create;
  try
    header.Delimiter := ';';
    header.StrictDelimiter := True;
    header.QuoteChar := #0;
    // add default columns
    checkColumn('objectID');
    checkColumn('geometry');
    // parse input
    if ExtractFileExt(aInputFileName).ToLower='.json' then
    begin
      Log.WriteLn('processing json file '+aInputFileName);
      jsonRoot :=  TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(TFile.ReadAllText(aInputFileName)), 0);
      try
        if jsonRoot is TJSONObject then
        begin
          jsonRootObject := jsonRoot as TJSONObject;
          if (jsonRootObject.TryGetValue<string>('type', value)) and (value='FeatureCollection') then
          begin
            if jsonRootObject.TryGetValue<TJSONArray>('features', features) then
            begin
              for f := 0 to features.Count-1 do
              begin
                feature := features.Items[f];
                csvLine := TStringList.Create;
                try
                  csvLine.Delimiter := ';';
                  csvLine.StrictDelimiter := True;
                  csvLine.QuoteChar := #0;
                  setCSVValue('objectID', f.ToString);
                  if feature.TryGetValue<TJSONObject>('geometry', geometry) then
                  begin
                    geometryStr := geometry.toString;
                    setCSVValue('geometry', geometryStr);
                    if feature.TryGetValue<TJSONObject>('properties', properties) then
                    begin

                      for p := 0 to properties.Count-1 do
                      begin
                        name := properties.pairs[p].JsonString.Value;
                        // skip some columns
                        if name.ToLower<>'wkt' then
                        begin
                          {
                          jsonValue := properties.pairs[p].JsonValue;
                          // ignore columns with no data
                          if jsonValue.Value<>'' then
                          begin
                            if jsonValue is TJSONNumber
                            then setCSVValue(name, jsonValue.value)
                            else setCSVValue(name, jsonValue.value);
                          end;
                          }
                        end;
                      end;

                    end;
                  end;
                  outputLines.Add(csvLine.DelimitedText)
                finally
                  csvLine.Free;
                end;
              end;
            end
            else Log.WriteLn('Could not find features in FeatureCollection in json file', llError);
          end
          else Log.WriteLn('Could not find FeatureCollection in json file', llError);
        end;
      finally
        jsonRoot.Free;
      end;
    end
    // else if..
    else Log.WriteLn('Input file extension not recognised: '+ExtractFileExt(aInputFileName), llError);
    // save output
    if outputLines.Count>0 then
    begin
      // add header
      outputLines.Insert(0, header.DelimitedText);
      // add type of objects
      outputLines.Insert(0, 'type;buildings');
      // add 2 empty lines to start with
      outputLines.Insert(0, ';');
      outputLines.Insert(0, ';');
      outputLines.SaveToFile(aOutputFileName)
    end
    else Log.WriteLn('Empty output file not written', llWarning);
  finally
    outputLines.Free;
    header.Free;
  end;
end;

var
  inputFileName: string;
  outputFileName: string;
begin
  try
    // convert
    CommandLine.FindFirst;
    inputFileName := CommandLine.FindNext;
    if inputFileName<>'' then
    begin
      outputFileName := CommandLine.FindNext;
      if outputFileName='' then
      begin
        outputFileName := inputFileName+'.csv';
        Log.WriteLn('No output file specified, using: '+outputFileName, llWarning);
      end;
      ConvertFile(inputFileName, outputFileName);
    end
    else WriteLn('No input file specified..');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
