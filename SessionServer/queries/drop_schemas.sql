-- Function: public.drop_schemas(text)

-- DROP FUNCTION public.drop_schemas(text);

CREATE OR REPLACE FUNCTION public.drop_schemas(started_with text)
  RETURNS void AS
$BODY$

   DECLARE rec RECORD; 

   BEGIN

       -- Get all the schemas

        FOR rec IN

        SELECT DISTINCT schemaname

        FROM pg_catalog.pg_tables

        WHERE schemaname LIKE '' || started_with || '%'  AND schemaname != 'public' 

           LOOP

             EXECUTE 'DROP SCHEMA ' || rec.schemaname || ' CASCADE'; 

           END LOOP; 

           RETURN; 

   END;

   $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION public.drop_schemas(text)
  OWNER TO ecodistrict;
