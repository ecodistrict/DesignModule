unit USCopyLib;

interface

uses
  MyStr, StdIni, CmdLin, Classes,
  Logger, LogConsole, LogFile,
  Ora, OraSmart, MyOraLib, DB, MemData, OraClasses,
  SysUtils, DateUtils, Threading, Generics.Collections;

const
  tasksPerScenarioCopy = 4;

procedure CreateIndexesExceptPrimaryKey(const aTableNameSrc, aTableNameDst, aOwner: string; aCopySpatialIndex: Boolean; aSession: TOraSession);
procedure CreateIndexesIncludingPrimaryKey(const aTableNameSrc, aTableNameDst, aOwner: string; aCopySpatialIndex: Boolean; aSession: TOraSession);
procedure CreateIndex(const aIndexNameSrc, aTableNameSrc, aTableNameDst, aOwner: string; aSession: TOraSession);
procedure CreatePrimaryKey(const aIndexNameSrc, aTableNameSrc, aTableNameDst, aOwner: string; aSession: TOraSession);
function GetPrimaryKeyConstraintName(const aTableName, aOwner: string; aSession: TOraSession): string;
procedure CreateTableWithPrimaryKey(const aTableNameSrc, aTableNameDst, aOwner: string; aSession: TOraSession);
function CopyScenario(const aSrcID, aOwner: string; aSession: TOraSession; out aNewID, aNewDescription: string): Boolean;

implementation

procedure CreateIndexesExceptPrimaryKey(const aTableNameSrc, aTableNameDst, aOwner: string; aCopySpatialIndex: Boolean; aSession: TOraSession);
var
  PrimaryKeyConstraintName: string;
  QueryIndexNames: TOraQuery;
  IndexNameSrc: string;
  IndexNameDst: string;
  queryTexts: TStringList;
  i: Integer;
begin
  PrimaryKeyConstraintName := GetPrimaryKeyConstraintName(aTableNameSrc, aOwner, aSession);
  queryTexts := TStringList.Create;
  try
    QueryIndexNames := TOraQuery.Create(nil);
    try
      QueryIndexNames.Session := aSession;
      QueryIndexNames.SQL.Text :=
        'SELECT index_name, ityp_name '+
        'FROM user_indexes '+
        'WHERE table_name='''+aTableNameSrc+''' AND INDEX_TYPE<>''LOB''';
      QueryIndexNames.Execute;//SQL;
      if QueryIndexNames.FindFirst then
      begin
        repeat
          IndexNameSrc := QueryIndexNames.Fields[0].AsString;
          IndexNameDst := IndexNameSrc;
          if (aTableNameSrc<>aTableNameDst) and Contains(IndexNameDst, aTableNameSrc)
          then IndexNameDst := Replace(IndexNameDst, aTableNameSrc, aTableNameDst);
          if IndexNameSrc<>PrimaryKeyConstraintName then
          begin
            //if aDropIndexes
            //then WriteLn('DROP INDEX '++';');

            if QueryIndexNames.Fields[1].AsString='SPATIAL_INDEX' then
            begin
              if aCopySpatialIndex
              // copy entry from original
              then queryTexts.Add( 'INSERT INTO USER_SDO_GEOM_METADATA '+
                                'SELECT '''+aTableNameDst+''' AS TABLE_NAME, COLUMN_NAME, DIMINFO, SRID '+
                                'FROM USER_SDO_GEOM_METADATA '+
                                'WHERE table_name='''+aTableNameSrc+'''')
              // manually create entry from defaults
              else queryTexts.Add( 'INSERT INTO USER_SDO_GEOM_METADATA  (TABLE_NAME, COLUMN_NAME, DIMINFO) '+
                                'VALUES( '''+aTableNameDst+''', '+
                                         '''SHAPE'', '+
                                         'MDSYS.SDO_DIM_ARRAY( MDSYS.SDO_DIM_ELEMENT(''X'', 0, 300000, 0.005), '+
                                                          'MDSYS.SDO_DIM_ELEMENT(''Y'', 300000, 625000, 0.005))'+
                                         ')');
            end
            else
              CreateIndex(IndexNameSrc, aTableNameSrc, aTableNameDst, aOwner, aSession);
          end;
        until not QueryIndexNames.FindNext;
      end;
    finally
      QueryIndexNames.Free;
    end;
    for I := 0 to queryTexts.Count-1 do
      aSession.ExecSQL(queryTexts[i]);
    aSession.Commit;
  finally
    queryTexts.Free;
  end;
end;

procedure CreateIndexesIncludingPrimaryKey(const aTableNameSrc, aTableNameDst, aOwner: string; aCopySpatialIndex: Boolean; aSession: TOraSession);
var
  PrimaryKeyConstraintName: string;
  QueryIndexNames: TOraQuery;
  IndexNameSrc: string;
  IndexNameDst: string;
  TableTrimSrc: string;
  TableTrimDst: string;
  queryTexts: TStringList;
  i: Integer;
begin
  PrimaryKeyConstraintName := GetPrimaryKeyConstraintName(aTableNameSrc, aOwner, aSession);
  queryTexts := TStringList.Create;
  try
    QueryIndexNames := TOraQuery.Create(nil);
    try
      QueryIndexNames.Session := aSession;
      QueryIndexNames.SQL.Text :=
        'SELECT index_name, ityp_name '+
        'FROM user_indexes '+
        'WHERE table_name='''+aTableNameSrc+''' AND INDEX_TYPE<>''LOB''';
      QueryIndexNames.Execute;//SQL;
      if QueryIndexNames.FindFirst then
      begin
        repeat
          if Length(aTableNameSrc) > 27 then
            TableTrimSrc := aTableNameSrc.Substring(0, 27)
          else
            TableTrimSrc := aTableNameSrc;
          if Length(aTableNameDst) > 27 then
            TableTrimDst := aTableNameDst.Substring(0, 27)
          else
            TableTrimDst := aTableNameDst;
          IndexNameSrc := QueryIndexNames.Fields[0].AsString;
          IndexNameDst := IndexNameSrc;
          if (aTableNameSrc<>aTableNameDst) and Contains(IndexNameDst, TableTrimSrc)
          then IndexNameDst := Replace(IndexNameDst, TableTrimSrc, TableTrimDst);
          if IndexNameSrc<>PrimaryKeyConstraintName then
          begin
            if QueryIndexNames.Fields[1].AsString='SPATIAL_INDEX' then
            begin
              if aCopySpatialIndex
              // copy entry from original
              then queryTexts.Add( 'INSERT INTO USER_SDO_GEOM_METADATA '+
                                  'SELECT '''+aTableNameDst+''' AS TABLE_NAME, COLUMN_NAME, DIMINFO, SRID '+
                                  'FROM USER_SDO_GEOM_METADATA '+
                                  'WHERE table_name='''+aTableNameSrc+'''')
              // manually create entry from defaults
              else queryTexts.Add( 'INSERT INTO USER_SDO_GEOM_METADATA  (TABLE_NAME, COLUMN_NAME, DIMINFO) '+
                                  'VALUES( '''+aTableNameDst+''', '+
                                           '''SHAPE'', '+
                                           'MDSYS.SDO_DIM_ARRAY( MDSYS.SDO_DIM_ELEMENT(''X'', 0, 300000, 0.005), '+
                                                            'MDSYS.SDO_DIM_ELEMENT(''Y'', 300000, 625000, 0.005))'+
                                           ')');
            end
            else
              CreateIndex(IndexNameSrc, aTableNameSrc, aTableNameDst, aOwner, aSession);
          end
          else
          begin
            CreatePrimaryKey(IndexNameSrc, aTableNameSrc, aTableNameDst, aOwner, aSession);
          end;
        until not QueryIndexNames.FindNext;
      end;
    finally
      QueryIndexNames.Free;
    end;
    for I := 0 to queryTexts.Count-1 do
      aSession.ExecSQL(queryTexts[i]);
    aSession.Commit;
  finally
    queryTexts.Free;
  end;
end;


procedure CreateIndex(const aIndexNameSrc, aTableNameSrc, aTableNameDst, aOwner: string; aSession: TOraSession);
var
  QueryIndexDef: TOraQuery;
  p: Integer;
  queryText: string;
  TableTrimSrc, TableTrimDst: string;
begin
  queryText := '';
  if Length(aTableNameSrc) > 27 then
    TableTrimSrc := aTableNameSrc.Substring(0, 27)
  else
    TableTrimSrc := aTableNameSrc;
  if Length(aTableNameDst) > 27 then
    TableTrimDst := aTableNameDst.Substring(0, 27)
  else
    TableTrimDst := aTableNameDst;
  QueryIndexDef := TOraQuery.Create(nil);
  try
    QueryIndexDef.Session := aSession;
    QueryIndexDef.SQL.Text := 'SELECT dbms_metadata.get_ddl(''INDEX'', '''+aIndexNameSrc+''', '''+aOwner+''') FROM dual';
    QueryIndexDef.ExecSQL;
    aSession.Commit;
    try
      if QueryIndexDef.FindFirst then
      begin
        queryText := QueryIndexDef.Fields[0].AsString;
        queryText := Replace(Replace(Replace(Replace(queryText, ccLF, ' '), ccCR, ' '), ccTAB, ' '), '  ', ' ');
        // remove owner prefix from index and table name
        queryText := Replace(queryText, '"'+aOwner+'".', '');
        // rename index but use table name because table is in sql statement also
        if aTableNameSrc<>aTableNameDst
        then queryText := Replace(queryText, TableTrimSrc, TableTrimDst);
        // remove everything after (including) PCTFREE
        p := Pos('PCTFREE', queryText);
        if p<>0
        then queryText := Copy(queryText, 1, p-1);
        queryText := Trim(queryText);
      end;
    finally
      QueryIndexDef.Close;
    end;
  finally
    QueryIndexDef.Free;
  end;
  if queryText <> '' then
  begin
    aSession.ExecSQL(queryText);
    aSession.Commit;
  end;
end;

procedure CreatePrimaryKey(const aIndexNameSrc, aTableNameSrc, aTableNameDst, aOwner: string; aSession: TOraSession);
var
  QueryIndexDef: TOraQuery;
  p: Integer;
  queryText, keyText: string;
  TableTrimSrc, TableTrimDst: string;
  IndexNameDst: string;
begin
  queryText := '';
  if Length(aTableNameSrc) > 27 then
    TableTrimSrc := aTableNameSrc.Substring(0, 27)
  else
    TableTrimSrc := aTableNameSrc;
  if Length(aTableNameDst) > 27 then
    TableTrimDst := aTableNameDst.Substring(0, 27)
  else
    TableTrimDst := aTableNameDst;
  if aIndexNameSrc.Contains(TableTrimSrc) then
    IndexNameDst := aIndexNameSrc.Replace(TableTrimSrc, TableTrimDst)
  else
    IndexNameDst := aIndexNameSrc;
  QueryIndexDef := TOraQuery.Create(nil);
  try
    QueryIndexDef.Session := aSession;
    QueryIndexDef.SQL.Text := 'SELECT dbms_metadata.get_ddl(''INDEX'', '''+aIndexNameSrc+''', '''+aOwner+''') FROM dual';
    QueryIndexDef.ExecSQL;
    aSession.Commit;
    try
      if QueryIndexDef.FindFirst then
      begin
        queryText := QueryIndexDef.Fields[0].AsString;
        p := Pos('"'+aOwner+'"."'+aTableNameSrc+'" (', queryText);
        if p <> 0 then
        begin
          keyText := queryText.Substring(p + Length('"'+aOwner+'"."'+aTableNameSrc+'" ') - 1);
          p := Pos(')', keyText);
          keyText := keyText.Substring(0, p);
        end;
        queryText := 'alter table ' + aTableNameDst + ' add constraint ' + IndexNameDst + ' PRIMARY KEY ' + keyText;
      end;
    finally
      QueryIndexDef.Close;
    end;
  finally
    QueryIndexDef.Free;
  end;
  if queryText <> '' then
  begin
    aSession.ExecSQL(queryText);
    aSession.Commit;
  end;
end;


function GetPrimaryKeyConstraintName(const aTableName, aOwner: string; aSession: TOraSession): string;
var
  Query: TOraQuery;
begin
  Query := TOraQuery.Create(nil);
  try
    Query.Session := aSession;
    Query.SQL.Text :=
      'SELECT cons.constraint_name '+
      'FROM all_constraints cons, all_cons_columns cols '+
      'WHERE cols.table_name like '''+aTableName+''' '+
      ' AND cons.constraint_type = ''P'' '+
      ' AND cons.constraint_name = cols.constraint_name '+
      ' AND cons.owner = cols.owner '+
      ' AND cons.status=''ENABLED'' '+
      ' AND cons.owner='''+aOwner+''' '+
      'ORDER BY cols.table_name, cols.position';
    Query.ExecSQL;
    if Query.FindFirst
    then Result := Query.Fields[0].AsString
    else Result := '';
  finally
    Query.Free;
  end;
end;

procedure CreateTableWithPrimaryKey(const aTableNameSrc, aTableNameDst, aOwner: string; aSession: TOraSession);
var
  Query: TOraQuery;
  p: Integer;
  queryText, srcPrefix, dstPrefix: string;
begin
  queryText := '';
  Query := TOraQuery.Create(nil);
  try
    Query.Session := aSession;
    Query.SQL.Text := 'SELECT dbms_metadata.get_ddl(''TABLE'','''+aTableNameSrc+''','''+aOwner+''') from dual';
    Query.ExecSQL;
    if Query.FindFirst then
    begin
      queryText := Query.Fields[0].AsString;
      // remove control characters, make single line command
      queryText := Trim(Replace(Replace(Replace(Replace(queryText, ccLF, ' '), ccCR, ' '), ccTAB, ' '), '  ', ' '));
      // remove to specific index parameters (all after USING INDEX)
      p := Pos('USING INDEX', queryText);
      if p<>0
      then queryText := Copy(queryText, 1, p+Length('USING INDEX')-1)+')'
      else
      begin
        // no index is given
        // remove everything after (including) PCTFREE
        p := Pos('PCTFREE', queryText);
        if p<>0
        then queryText := Copy(queryText, 1, p-1);
      end;
      // remove owner prefix from table name
      queryText := Replace(queryText, '"'+aOwner+'".', '');
      // rename table
      if aTableNameSrc<>aTableNameDst
      then
      begin
        p := Pos('#', aTableNameSrc);
        if p <> 0 then
        begin
          srcPrefix := Copy(aTableNameSrc, 1, p);
          p := Pos('#', aTableNameDst);
          if p <> 0 then
          begin
            dstPrefix := Copy(aTableNameDst, 1, p);
            if srcPrefix <> dstPrefix then
              queryText := Replace(queryText, srcPrefix, dstPrefix);
          end;
        end;
      end;
    end
    else queryText := '';
  finally
    Query.Free;
  end;
  if queryText <> '' then
  begin
    aSession.ExecSQL(queryText);
    aSession.Commit;
  end;

end;

function CopyScenario(const aSrcID, aOwner: string; aSession: TOraSession; out aNewID, aNewDescription: string): Boolean;
var
  dstID, srcDescription, postfix, srcTable, dstTable: string;
  query: TOraQuery;
  succes: Boolean;
  start: TDateTime;
  tableNames: TList<string>;
begin
  //make meta_scenarios entry so we have the new id.
  Result := False;
  succes := False;
  start := Now;
  Log.WriteLn('Starting scenario copy', llNormal);
  aSession.StartTransaction;
  try
    query := TOraQuery.Create(nil);
    try
      query.Connection := aSession;
      query.SQL.Text := 'LOCK TABLE META_SCENARIOS IN EXCLUSIVE MODE';
      query.ExecSQL;
      //query.SQL.Text := 'SELECT * FROM ((SELECT (MAX(ID)+1) as NEWID FROM META_SCENARIOS) cross join (SELECT NOTES FROM META_SCENARIOS where ID = ' + aSrcID + '));';
      query.SQL.Text := 'SELECT (MAX(ID) + 1) as NEWID FROM META_SCENARIOS';
      query.ExecSQL;
      if query.FindFirst then
      begin
        dstID := query.FieldByName('NEWID').AsString;
        aNewID := dstID;
        query.SQL.Text := 'SELECT NOTES FROM META_SCENARIOS WHERE ID = :ID';
        query.ParamByName('ID').Value := aSrcID;
        query.ExecSQL;
        if query.FindFirst then
        begin
          srcDescription := query.FieldByName('NOTES').AsString;
          aNewDescription := 'Copy of V' + aSrcID + ' ' + srcDescription;
          query.SQL.Text := 'INSERT INTO META_SCENARIOS (ID, NAME, FEDERATE, PARENT_ID, BASE_ID, SCENARIO_STATUS, NOTES, PUBLISHED) ' +
                              'VALUES (:ID, :NAME, :FEDERATE, :PARENT_ID, :BASE_ID, :SCENARIO_STATUS, :NOTES, :PUBLISHED)';
          query.ParamByName('ID').Value := dstID;
          query.ParamByName('NAME').Value := 'V' + dstID;
          query.ParamByName('FEDERATE').Value := aSession.Username.ToUpper + '#V' + dstID;
          query.ParamByName('PARENT_ID').Value := aSrcID;
          query.ParamByName('BASE_ID').Value := aSrcID;
          query.ParamByName('SCENARIO_STATUS').Value := 'CLOSED';
          query.ParamByName('NOTES').Value := 'Copy of V' + aSrcID + ' ' + srcDescription;
          query.ParamByName('PUBLISHED').Value := '0';
          query.ExecSQL;
          succes := True;
        end;
      end;
    finally
      query.Free;
    end;
    aSession.Commit;
  except
    aSession.Rollback;
  end;
  //copy all tables
  //...
  if succes then
  begin
    query := TOraQuery.Create(nil);
    try
      tableNames := TList<string>.Create;
      try
        query.Connection := aSession;
        query.SQL.Text := 'SELECT TABLE_NAME FROM All_Tables WHERE OWNER=:OWNER and TABLE_NAME like :PREFIX ORDER BY TABLE_NAME';
        query.ParamByName('OWNER').Value := aSession.username.ToUpper;
        query.ParamByName('PREFIX').Value := 'V' + aSrcID + '#%';
        query.ExecSQL;
        query.First;
        while (not query.Eof) do
        begin
          tableNames.Add(query.FieldByName('TABLE_NAME').AsString);
          query.Next;
        end;
        if tableNames.Count > 0 then
        begin
          TParallel.&For(0, tableNames.Count - 1, procedure(I: Integer)
          var
            parOraSession: TOraSession;
            parSrcTable, parPostfix, parDstTable: string;
          begin
            parSrcTable := tableNames[i];
            parPostfix := parSrcTable.Substring(Length(aSrcID) + 2);
            parDstTable := 'V' + dstID + '#' + parPostfix;
            parOraSession := TORaSession.Create(nil);
            try
              parOraSession.ConnectString := aSession.userName+'/'+aSession.password+'@'+aSession.server;
              //CreateTableWithPrimaryKey(parSrcTable, parDstTable, aSession.Username.ToUpper, parOraSession, lockObject);
              //parOraSession.ExecSQL('INSERT INTO '+parDstTable+' SELECT * FROM '+parSrcTable);
              parOraSession.ExecSQL('CREATE TABLE '+parDstTable+' AS SELECT * FROM '+parSrcTable);
              parOraSession.Commit;
              //CreateIndexesExceptPrimaryKey(parSrcTable, parDstTable, aSession.Username.ToUpper, True, parOraSession, lockObject);
              CreateIndexesIncludingPrimaryKey(parSrcTable, parDstTable, aSession.Username.ToUpper, True, parOraSession);
            finally
              parOraSession.Free;
            end;
            Log.WriteLn('Created and copied data to: ' + parDstTable, llNormal);
          end);
        end;
        Log.WriteLn('Done creating and copying tables', llNormal);
        query.SQL.Text := 'UPDATE META_SCENARIOS ' +
                            'SET SCENARIO_STATUS = :SCENARIO_STATUS,' +
                            'PUBLISHED = :PUBLISHED WHERE ID = :ID';
        query.ParamByName('SCENARIO_STATUS').Value := 'OPEN';
        query.ParamByName('PUBLISHED').Value := '1';
        query.ParamByName('ID').Value := dstID;
        query.ExecSQL;
        aSession.Commit;
        Log.WriteLn('Finished copying scenario in: ' + DateUtils.MilliSecondsBetween(Now, start).ToString + ' milliseconds', llNormal);
        Result := True;
      finally
        FreeAndNil(tableNames);
      end;
    finally
      query.Free;
    end;
  end;
end;

end.
