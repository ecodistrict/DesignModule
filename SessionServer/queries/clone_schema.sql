-- Function: public.clone_schema(text, text, boolean)

-- DROP FUNCTION public.clone_schema(text, text, boolean);

CREATE OR REPLACE FUNCTION public.clone_schema(
    source_schema text,
    dest_schema text,
    include_recs boolean)
  RETURNS void AS
$BODY$

--  Last revision 2015-09-18

--  This function will clone all sequences, tables, indexes, rules, triggers, 

--  data(optional), views & functions from any existing schema to a new schema

-- SAMPLE CALL:

-- SELECT SELECT * FROM clone_schema('public', 'new_schema', TRUE);



DECLARE

  src_oid          oid;

  tbl_oid          oid;

  func_oid         oid;

  con_oid          oid;

  v_path           text;

  v_func           text;

  v_args           text;

  v_conname        text;

  v_rule           text;

  v_trig           text;

  object           text;

  buffer           text;

  srctbl           text;

  default_         text;

  v_column         text;

  qry              text;

  dest_qry         text;

  v_def            text;

  v_stat           integer;

  seqval           bigint;

  sq_last_value    bigint;

  sq_max_value     bigint;

  sq_start_value   bigint;

  sq_increment_by  bigint;

  sq_min_value     bigint;

  sq_cache_value   bigint;

  sq_log_cnt       bigint;

  sq_is_called     boolean;

  sq_is_cycled     boolean;

  sq_cycled        char(10);



BEGIN



-- Check that source_schema exists

  SELECT oid INTO src_oid

    FROM pg_namespace

   WHERE nspname = quote_ident(source_schema);

  IF NOT FOUND

    THEN 

    RAISE NOTICE 'source schema % does not exist!', source_schema;

    RETURN ;

  END IF;



  -- Check that dest_schema does not yet exist

  PERFORM nspname 

    FROM pg_namespace

   WHERE nspname = quote_ident(dest_schema);

  IF FOUND

    THEN 

    RAISE NOTICE 'dest schema % already exists!', dest_schema;

    RETURN ;

  END IF;



  EXECUTE 'CREATE SCHEMA ' || quote_ident(dest_schema) ;



  -- Add schema comment

  SELECT description INTO v_def

    FROM pg_description

   WHERE objoid = src_oid

       AND objsubid = 0;

  IF FOUND 

    THEN 

      EXECUTE 'COMMENT ON SCHEMA ' || quote_ident(dest_schema) || ' IS ' || quote_literal(v_def);

  END IF;

  

  -- Create sequences

  -- TODO: Find a way to make this sequence's owner is the correct table.

  FOR object IN

    SELECT sequence_name::text 

      FROM information_schema.sequences

     WHERE sequence_schema = quote_ident(source_schema)

  LOOP

    EXECUTE 'CREATE SEQUENCE ' || quote_ident(dest_schema) || '.' || quote_ident(object);

    srctbl := quote_ident(source_schema) || '.' || quote_ident(object);



    EXECUTE 'SELECT last_value, max_value, start_value, increment_by, min_value, cache_value, log_cnt, is_cycled, is_called 

              FROM ' || quote_ident(source_schema) || '.' || quote_ident(object) || ';' 

              INTO sq_last_value, sq_max_value, sq_start_value, sq_increment_by, sq_min_value, sq_cache_value, sq_log_cnt, sq_is_cycled, sq_is_called ; 



    IF sq_is_cycled 

      THEN 

        sq_cycled := 'CYCLE';

    ELSE

        sq_cycled := 'NO CYCLE';

    END IF;



    EXECUTE 'ALTER SEQUENCE '   || quote_ident(dest_schema) || '.' || quote_ident(object) 

            || ' INCREMENT BY ' || sq_increment_by

            || ' MINVALUE '     || sq_min_value 

            || ' MAXVALUE '     || sq_max_value

            || ' START WITH '   || sq_start_value

            || ' RESTART '      || sq_min_value 

            || ' CACHE '        || sq_cache_value 

            || sq_cycled || ' ;' ;



    buffer := quote_ident(dest_schema) || '.' || quote_ident(object);

    IF include_recs 

        THEN

            EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_last_value || ', ' || sq_is_called || ');' ; 

    ELSE

            EXECUTE 'SELECT setval( ''' || buffer || ''', ' || sq_start_value || ', ' || sq_is_called || ');' ;

    END IF;



  -- add sequence comments

    SELECT oid INTO tbl_oid

      FROM pg_class 

     WHERE relkind = 'S'

       AND relnamespace = src_oid

       AND relname = quote_ident(object);



    SELECT description INTO v_def

      FROM pg_description

     WHERE objoid = tbl_oid

       AND objsubid = 0;



    IF FOUND

      THEN      

        EXECUTE 'COMMENT ON SEQUENCE ' || quote_ident(dest_schema) || '.' || quote_ident(object)

                 || ' IS ''' || v_def || ''';';

    END IF;





  END LOOP;



-- Create tables 

  FOR object IN

    SELECT TABLE_NAME::text 

      FROM information_schema.tables 

     WHERE table_schema = quote_ident(source_schema)

       AND table_type = 'BASE TABLE'



  LOOP

    buffer := quote_ident(dest_schema) || '.' || quote_ident(object);

    EXECUTE 'CREATE TABLE ' || buffer || ' (LIKE ' || quote_ident(source_schema) || '.' || quote_ident(object) 

        || ' INCLUDING ALL)';



    -- Add table comment

    SELECT oid INTO tbl_oid

      FROM pg_class 

     WHERE relkind = 'r'

       AND relnamespace = src_oid

       AND relname = quote_ident(object);



    SELECT description INTO v_def

      FROM pg_description

     WHERE objoid = tbl_oid

       AND objsubid = 0;



    IF FOUND

      THEN      

        EXECUTE 'COMMENT ON TABLE ' || quote_ident(dest_schema) || '.' || quote_ident(object)

                 || ' IS ''' || v_def || ''';';

    END IF;

   

    IF include_recs 

      THEN 

      -- Insert records from source table

      EXECUTE 'INSERT INTO ' || buffer || ' SELECT * FROM ' || quote_ident(source_schema) || '.' || quote_ident(object) || ';';

    END IF;



    FOR v_column, default_ IN

      SELECT column_name::text, 

             REPLACE(column_default::text, quote_ident(source_schema) || '.', quote_ident(dest_schema) || '.' )

        FROM information_schema.COLUMNS 

       WHERE table_schema = dest_schema 

         AND TABLE_NAME = object 

         AND column_default LIKE 'nextval(%' || quote_ident(source_schema) || '%::regclass)'

    LOOP

      EXECUTE 'ALTER TABLE ' || buffer || ' ALTER COLUMN ' || v_column || ' SET DEFAULT ' || default_;



    END LOOP;



  END LOOP;



  -- set column statistics

  FOR tbl_oid, srctbl IN 

    SELECT oid, relname

      FROM pg_class

     WHERE relnamespace = src_oid

       AND relkind = 'r'



  LOOP



    FOR v_column, v_stat IN         

      SELECT attname, attstattarget

        FROM pg_attribute 

       WHERE attrelid = tbl_oid

         AND attnum > 0

    LOOP



      buffer := quote_ident(dest_schema) || '.' || quote_ident(srctbl);  

--      RAISE EXCEPTION 'ALTER TABLE % ALTER COLUMN % SET STATISTICS %', buffer, v_column, v_stat::text;

      EXECUTE 'ALTER TABLE ' || buffer || ' ALTER COLUMN ' || quote_ident(v_column) || ' SET STATISTICS ' || v_stat || ';';



    END LOOP;

  END LOOP;



--  add FK constraint

  FOR qry IN

    SELECT 'ALTER TABLE ' || quote_ident(dest_schema) || '.' || quote_ident(rn.relname) 

                          || ' ADD CONSTRAINT ' || quote_ident(ct.conname) || ' ' || pg_get_constraintdef(ct.oid) || ';'

      FROM pg_constraint ct

      JOIN pg_class rn ON rn.oid = ct.conrelid

     WHERE connamespace = src_oid

       AND rn.relkind = 'r'

       AND ct.contype = 'f'

         

    LOOP

      EXECUTE qry;



    END LOOP;



    -- Add constraint comment

    FOR con_oid IN

      SELECT oid

        FROM pg_constraint

       WHERE conrelid = tbl_oid



    LOOP

      SELECT conname INTO v_conname

        FROM pg_constraint

       WHERE oid = con_oid;



      SELECT description INTO v_def

        FROM pg_description

       WHERE objoid = con_oid;

       

       IF FOUND

          THEN 

            EXECUTE 'COMMENT ON CONSTRAINT ' || v_conname || ' ON ' || quote_ident(dest_schema) || '.' || quote_ident(object) 

                    || ' IS ''' || v_def || ''';';

       END IF; 



    END LOOP;





-- Create views 

  FOR object IN

    SELECT table_name::text,

           view_definition 

      FROM information_schema.views

     WHERE table_schema = quote_ident(source_schema)



  LOOP

    buffer := quote_ident(dest_schema) || '.' || quote_ident(object);

    SELECT view_definition INTO v_def

      FROM information_schema.views

     WHERE table_schema = quote_ident(source_schema)

       AND table_name = quote_ident(object);

     

    EXECUTE 'CREATE OR REPLACE VIEW ' || buffer || ' AS ' || v_def || ';' ;

  END LOOP;





  RETURN; 





END;



$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION public.clone_schema(text, text, boolean)
  OWNER TO ecodistrict;
