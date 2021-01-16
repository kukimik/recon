CREATE TABLE recon_settings
 (
  main_package_name VARCHAR2(30) DEFAULT 'R_' NOT NULL
 ,types_package_name VARCHAR2(30) DEFAULT 'T_' NOT NULL
 ,default_schema VARCHAR2(30)
 ,rowtype_schema_param_name VARCHAR2(30) DEFAULT 'SCHEMA__' NOT NULL
 ,rowtype_defaults_param_name VARCHAR2(30) DEFAULT 'DEFAULTS__' NOT NULL
 ,rowtype_default_defaults VARCHAR2(30) DEFAULT 'NULL' NOT NULL
 ,record_schema_param_name VARCHAR2(30) DEFAULT 'SCHEMA__' NOT NULL
 ,record_package_param_name VARCHAR2(30) DEFAULT 'PACKAGE__' NOT NULL
 ,record_defaults_param_name VARCHAR2(30) DEFAULT 'DEFAULTS__' NOT NULL
 ,record_default_package VARCHAR2(30)
 ,record_default_defaults VARCHAR2(30) DEFAULT 'NULL'
 ,assarray_schema_param_name VARCHAR2(30) DEFAULT 'SCHEMA__' NOT NULL
 ,assarray_package_param_name VARCHAR2(30) DEFAULT 'PACKAGE__' NOT NULL
 ,assarray_default_package VARCHAR2(30)
 ,assarray_max_items NUMBER(3) DEFAULT 128 NOT NULL
 ,CONSTRAINT CK_recon_settings CHECK
    (
          rowtype_default_values_mode IN ('NONE','NULL','DEFAULT_AND_NONE','DEFAULT_AND_NULL')
      AND records_default_values_mode IN ('NONE','NULL','DEFAULT_AND_NONE','DEFAULT_AND_NULL')
      AND asarray_max_items BETWEEN 1 AND 1000
    )
 );

COMMENT ON TABLE recon_settings IS '';
COMMENT ON COLUMN recon_settings.main_package_name IS '';
COMMENT ON COLUMN recon_settings.types_package_name IS '';
COMMENT ON COLUMN recon_settings.rowtype_schema_param_name IS '';
COMMENT ON COLUMN recon_settings.rowtype_default_schema IS '';
COMMENT ON COLUMN recon_settings.rowtype_default_values_mode IS '';
COMMENT ON COLUMN recon_settings.records_schema_param_name IS '';
COMMENT ON COLUMN recon_settings.records_package_param_name IS '';
COMMENT ON COLUMN recon_settings.records_default_schema IS '';
COMMENT ON COLUMN recon_settings.records_default_package IS '';
COMMENT ON COLUMN recon_settings.records_default_values_mode IS '';
COMMENT ON COLUMN recon_settings.asarray_schema_param_name IS '';
COMMENT ON COLUMN recon_settings.asarray_package_param_name IS '';
COMMENT ON COLUMN recon_settings.asarray_default_schema IS '';
COMMENT ON COLUMN recon_settings.asarray_default_package IS '';

CREATE TABLE recon_tables
 (
  table_owner VARCHAR2(32) NOT NULL
 ,table_name VARCHAR2(32) NOT NULL
 ,CONSTRAINT PK_recon_tables PRIMARY KEY (table_owner, table_name) -- unnamed constraint possible?
 );

COMMENT ON TABLE recon_tables IS '';
COMMENT ON COLUMN recon_tables.table_owner IS '';
COMMENT ON COLUMN recon_tables.table_name IS '';

CREATE OR REPLACE PACKAGE recon
IS
  ex_table_does_not_exist EXCEPTION;
  PRAGMA EXCEPTION_INIT (ex_table_does_not_exist,  -20501);

  PROCEDURE add_table
    (
     p_table_owner recon_tables.table_owner%type DEFAULT sys_context('userenv', 'current_schema')
    ,p_table_name recon_tables.table_name%type
    );

  PROCEDURE remove_table
    (
     p_table_owner recon_tables.table_owner%type DEFAULT sys_context('userenv', 'current_schema')
    ,p_table_name recon_tables.table_name%type
    );

  PROCEDURE make_packages
    (
     p_package_name VARCHAR2 DEFAULT 'R_'
    ,p_schema_param_name VARCHAR2 DEFAULT 'SCHEMA__'
    ,p_default_schema VARCHAR2 DEFAULT sys_context('userenv', 'current_schema')
    );
END recon;
/

CREATE OR REPLACE PACKAGE BODY recon
IS
  TYPE t_table_column IS RECORD
    (
     column_name VARCHAR2(30)
    ,data_default VARCHAR2(32767) -- can fail for very long data_defaults!
    );

  TYPE t_table_columns IS TABLE OF recon.t_table_column;

  TYPE t_function IS RECORD
    (
     function_head CLOB
    ,function_body CLOB
    );


  PROCEDURE add_table
    (
     p_table_owner recon_tables.table_owner%type DEFAULT sys_context('userenv', 'current_schema')
    ,p_table_name recon_tables.table_name%type
    )
  IS
  BEGIN
    INSERT INTO recon_tables( table_owner, table_name )
    VALUES (p_table_owner, p_table_name);
  END add_table;

  PROCEDURE remove_table
    (
     p_table_owner recon_tables.table_owner%type DEFAULT sys_context('userenv', 'current_schema')
    ,p_table_name recon_tables.table_name%type
    )
  IS
  BEGIN
    DELETE FROM recon_tables WHERE table_owner = p_table_owner AND table_name = p_table_name;
  END remove_table;

  PROCEDURE make_packages
    (
     p_package_name VARCHAR2 DEFAULT 'R_'
    ,p_types_package_name VARCHAR2 DEFAULT 'T_'
    ,p_schema_param_name VARCHAR2 DEFAULT 'SCHEMA__'
    ,p_default_schema VARCHAR2 DEFAULT sys_context('userenv', 'current_schema')
    )
  IS
    c_package_name_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_package_name) );
    c_schema_param_name_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_schema_param_name) );
    c_default_schema_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_default_schema) );

    TYPE t_varchar2integer IS TABLE OF INTEGER INDEX BY VARCHAR2(32);
    v_schema2number t_varchar2integer;

    v_sql_head CLOB;
    v_sql_body CLOB;
    v_buffer CLOB;
  BEGIN
    v_types_package_data :=
      recon.make_types_package
        (
         p_types_package_name_enq => c_types_package_name_enq
        ,p_default_schema_enq => c_default_schema_enq
        );

    recon.make_main_package
      (
       p_package_name_enq => c_package_name_enq
      );
  END make_packages;

    v_sql_head := 'CREATE OR REPLACE PACKAGE '||c_package_name||' IS'||chr(10);
    v_sql_body := 'CREATE OR REPLACE PACKAGE BODY '||c_package_name||' IS'||chr(10);

    DECLARE
      v_sql_schemas CLOB;
    BEGIN
      FOR rec IN
        (
         SELECT DISTINCT dbms_assert.enquote_name(ret.table_owner) AS table_owner
         FROM recon_tables ret
         ORDER BY ret.table_owner
        )
      LOOP
        v_schema2number(rec.table_owner) := v_schema2number.COUNT;

        v_sql_head :=
            v_sql_head
          ||'TYPE t_'
          ||v_schema2number(rec.table_owner)
          ||' IS RECORD(dummy BOOLEAN);'
          ||chr(10);

        v_sql_schemas :=
            v_sql_schemas
          ||','
          ||rec.table_owner
          ||' '
          ||c_package_name
          ||'.t_'
          ||v_schema2number(rec.table_owner);
      END LOOP;

      v_sql_head :=
          v_sql_head
        ||'TYPE t_schemas IS RECORD('
        ||ltrim(v_sql_schemas,',')
        ||');'
        ||chr(10)
        ||'schemas CONSTANT '
        ||c_package_name
        ||'.t_schemas := CAST(NULL AS '
        ||c_package_name
        ||'.t_schemas);'
        ||chr(10);

      IF v_schema2number.EXISTS(c_default_schema_enq)
      THEN
        v_sql_head :=
          v_sql_head
        ||'default_schema CONSTANT '
        ||c_package_name
        ||'.'
        ||v_schema2number(c_default_schema_enq)
        ||':='
        ||c_package_name
        ||'.schemas.'
        ||c_default_schema_enq
        ||';'
        ||chr(10);
      END IF;
    END;

    FOR rec IN
      (
        SELECT
          ret.table_owner
         ,ret.table_name
         ,dbms_assert.enquote_name(ret.table_owner) AS table_owner_enq
         ,dbms_assert.enquote_name(ret.table_name) AS table_name_eqn
        FROM recon_tables ret
        ORDER BY
          ret.table_owner
         ,ret.table_name
      )
    LOOP
      DECLARE
        TYPE t_tab_columns IS TABLE OF VARCHAR2(32);
        v_tab t_tab_columns;
      BEGIN
        -- TODO: default values!
        -- * add parameter (default = no defaults/with defaults - parametrised by make_package option (or per-table?))
        -- * get defaults from all_tab_cols
        SELECT dbms_assert.enquote_name(ata.column_name)
        BULK COLLECT INTO v_tab
        FROM all_tab_cols ata
        WHERE ata.owner = rec.table_owner
          AND ata.table_name = rec.table_name
$IF DBMS_DB_VERSION.VERSION >= 11
$THEN
          AND ata.virtual_column = 'NO'
$END
$IF DBMS_DB_VERSION.VERSION >= 12
$THEN
          AND ata.hidden_column = 'NO'
$END
        ORDER BY ata.column_id;

        IF v_tab.COUNT = 0
        THEN
          raise_application_error( -20501, 'Table '||rec.table_owner_enq||'.'||rec.table_name_enq||' not found in ALL_TAB_COLUMNS.');
        ELSE
          DECLARE
            v_parameters CLOB;
            v_assignments CLOB;
            v_function_head CLOB;
            v_function_body CLOB;
            v_tab_rowtype VARCHAR2(32767);
          BEGIN
            v_tab_rowtype := rec.table_owner_enq||'.'||rec.table_name_enq||'%rowtype';

            FOR ii IN 1..v_tab.COUNT
            LOOP
              DECLARE
                c_column_name_enq CONSTANT VARCHAR2(32)
                  := v_tab(ii);
              BEGIN
                v_parameters :=
                    v_parameters
                  ||c_column_name_enq
                  ||' '
                  ||rec.table_owner_enq
                  ||'.'
                  ||rec.table_name_enq
                  ||'.'
                  ||c_column_name_enq
                  ||'%type:=NULL'
                  ||',';
                v_assignments :=
                    v_assignments
                  ||'v_r.'
                  ||c_column_name_enq
                  ||':='
                  ||c_column_name_enq
                  ||';';
              END;
            END LOOP;

            v_function_head :=
                'FUNCTION '
              ||rec.table_name_enq
              ||'('
              ||v_parameters
              ||c_schema_param_name
              ||' '
              ||c_package_name
              ||'.t_'
              ||v_schema2number(rec.table_owner_enq)
              ||':='
              ||c_package_name
              ||'.schemas.'
              ||rec.table_owner_enq
              ||')'
              ||'RETURN '
              ||v_tab_rowtype;

            v_function_body :=
                 ' IS v_r '
              ||v_tab_rowtype
              ||';BEGIN '
              ||v_assignments
              ||'RETURN v_r;END '
              ||rec.table_name_enq
              ||';';

            v_sql_head := v_sql_head||v_function_head||';'||chr(10);
            v_sql_body := v_sql_body||v_function_head||v_function_body||chr(10);
          END;
        END IF;
      END;
    END LOOP;

    v_sql_head := v_sql_head||chr(10)||'END '||c_package_name||';';
    v_sql_body := v_sql_body||chr(10)||'END '||c_package_name||';';

    EXECUTE IMMEDIATE v_sql_head;
    EXECUTE IMMEDIATE v_sql_body;
    --dbms_output.put_line(v_sql_head);
    --dbms_output.put_line(v_sql_body);
  END make_packages;

  FUNCTION make_types_package
    (

    )

  FUNCTION make_rowtype_constructor
    (
     schema_name VARCHAR2
    ,table_name VARCHAR2
    ,table_columns recon.t_table_columns

    ,schema_param_name VARCHAR2
    ,defaults_param_name VARCHAR2

    ,schema_type VARCHAR2
    ,defaults_type VARCHAR2

    ,mode recon.t_mode
    ,is_mode_set BOOLEAN
    )
  RETURN CLOB DETERMINISTIC
  IS
  BEGIN

  END make_rowtype_constructor;

  FUNCTION make_record_constructor
    (

    )
  RETURN CLOB
  IS
  BEGIN

  END make_record_constructor;

  FUNCTION make_assarray_constructor
    (

    )
  RETURN CLOB
  IS
  BEGIN

  END make_assarray_constructor;

END recon;
/