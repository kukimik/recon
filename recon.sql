CREATE OR REPLACE PACKAGE recon
IS
  ex_table_does_not_exist EXCEPTION;
  PRAGMA EXCEPTION_INIT (ex_table_does_not_exist,  -20501);

  -- type that describes possible default value "modes"
  -- (i.e. does the constructor have default values for fields and if so, what are these values?)
  SUBTYPE t_mode IS POSITIVE RANGE 1..4;

  SUBTYPE unquoted_id IS VARCHAR2(30) NOT NULL;

  -- number of items that in an associative array that a constructor can initialize
  SUBTYPE t_num_items_assarray IS POSITIVEN RANGE 1..1000;

  TYPE t_rowtype_data IS RECORD
    (
     schema_name recon.unquoted_id
    ,table_name recon.unquoted_id
    );

  TYPE t_record_data IS RECORD
    (
     schema_name recon.unquoted_id
    ,package_name recon.unquoted_id
    ,type_name recon.unquoted_id
    );

  TYPE t_assarray_data IS RECORD
    (
     schema_name recon.unquoted_id
    ,package_name recon.unquoted_id
    ,type_name recon.unquoted_id
    );

  TYPE t_tab_rowtype_data IS TABLE OF t_rowtype_data;
  TYPE t_tab_record_data IS TABLE OF t_record_data;
  TYPE t_tab_assarray_data IS TABLE OF t_assarray_data;

  TYPE t_pair_clob IS RECORD ( c1 CLOB, c2 CLOB );

  -- constants, implemented as functions to avoid trouble with referencing package constants from other sites
  FUNCTION mode_no_defaults             RETURN recon.t_mode DETERMINISTIC IS RETURN 1; END mode_no_defaults;
  FUNCTION mode_null_defaults           RETURN recon.t_mode DETERMINISTIC IS RETURN 2; END mode_null_defaults;
  FUNCTION mode_data_defaults           RETURN recon.t_mode DETERMINISTIC IS RETURN 3; END mode_data_defaults;
  FUNCTION mode_data_defaults_and_nulls RETURN recon.t_mode DETERMINISTIC IS RETURN 4; END mode_data_defaults_and_nulls;

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
  --------------------------------
  -- Private types
  --------------------------------
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

  --------------------------------
  -- Public functions
  --------------------------------

  PROCEDURE make_packages
    (
     p_constructors_package_name VARCHAR2 DEFAULT 'R_'
    ,p_types_package_name VARCHAR2 DEFAULT 'T_'
    ,p_default_schema VARCHAR2 DEFAULT sys_context('userenv', 'current_schema')
    ,p_type_param_name VARCHAR2 DEFAULT 'TYPE__'
    ,p_mode_param_name VARCHAR2 DEFAULT 'MODE__'
    ,p_rowtype_default_mode recon.t_mode DEFAULT recon.mode_null_defaults -- perhaps no default mode should be provided
    ,p_record_default_mode recon.t_mode DEFAULT recon.mode_data_defaults_and_nulls -- perhaps no default mode should be provided
    ,p_assarray_max_items recon.t_num_items_assarray DEFAULT 128
    ,p_rowtypes t_tab_rowtype_data
    ,p_records t_record_data
    ,p_assarrays t_assarray_data
    )
  IS
    c_package_name_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_package_name) );
    c_types_package_name_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_types_package_name) );
    c_type_param_name_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_object_param_name) );
    c_mode_param_name_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_mode_param_name) );
    c_default_schema_enq CONSTANT VARCHAR2(32)
      := dbms_assert.simple_sql_name( dbms_assert.enquote_name(p_default_schema) );
    c_rowtype_default_mode CONSTANT recon.t_mode := p_rowtype_default_mode;
    c_record_default_mode CONSTANT recon.t_mode := p_record_default_mode;
    c_assarray_max_items CONSTANT recon.t_num_items_assarray := p_assarray_max_items;
  BEGIN
    IF c_assarray_max_items IS NULL
    THEN
      RAISE value_error;
    END IF;

    recon.make_types_package
      (
       p_types_package_name_enq => c_types_package_name_enq
      ,p_default_schema_enq => c_default_schema_enq
      ,p_rowtypes => p_tables
      ,p_records => p_records
      ,p_assarrays => p_assarrays
      );

    recon.make_main_package
      (
       p_package_name_enq => c_package_name_enq
      ,p_types_package_name_enq => c_types_package_name_enq
      ,p_schema_param_name_enq => c_schema_param_name_enq
      ,p_package_param_name_enq => c_package_param_name_enq
      ,p_mode_param_name_enq => c_mode_param_name_enq
      ,p_rowtype_default_mode => c_rowtype_default_mode
      ,p_record_default_mode => c_record_default_mode
      ,p_assarray_max_items => c_assarray_max_items
      ,p_rowtypes => p_tables
      ,p_records => p_records
      ,p_assarrays => p_assarrays
      );
  END make_packages;

  PROCEDURE make_types_package
    (
     p_types_package_name_enq VARCHAR2
    ,p_default_schema_enq VARCHAR2
    ,p_rowtypes t_tab_rowtype_data
    ,p_records t_record_data
    ,p_assarrays t_assarray_data
    )
    IS
      SUBTYPE t_type_number IS POSITIVE;

      TYPE t_package_types IS TABLE OF t_type_number INDEX BY VARCHAR2(32);

      TYPE t_object IS RECORD
        (
         typenum t_type_number
        ,package_types t_package_types
        );

      TYPE t_objects IS TABLE OF t_object INDEX BY VARCHAR2(32);

      TYPE t_schema IS RECORD
        (
         typenum t_type_number
        ,objects t_objects
        );

      TYPE t_schemas IS TABLE OF t_schema INDEX BY VARCHAR2(32);

      v_schemas t_schemas;
      v_head CLOB;
      v_body CLOB;
    BEGIN
      <<fill_v_schemas>>
        DECLARE
          v_type_number NATURALN := 0;

          PROCEDURE add_to_schemas
            (
             p_schema VARCHAR2
            ,p_object VARCHAR2
            ,p_type VARCHAR2 DEFAULT NULL
            )
            IS
              c_schema_enq CONSTANT VARCHAR2(32) := dbms_assert.simple_sql_name(dbms_assert.enquote_name(p_schema));
              c_object_enq CONSTANT VARCHAR2(32) := dbms_assert.simple_sql_name(dbms_assert.enquote_name(p_object));
              c_type_enq CONSTANT VARCHAR2(32) := CASE WHEN p_type IS NOT NULL THEN dbms_assert.simple_sql_name(dbms_assert.enquote_name(p_type)) END;

              FUNCTION next_typenum
              RETURN t_type_number
                IS
                BEGIN
                  v_type_number := v_type_number + 1;
                  RETURN v_type_number;
                END next_typenum;
            BEGIN
              IF NOT v_schemas.EXISTS(c_schema_enq)
              THEN
                v_schemas(c_schema_enq).typenum := next_typenum;
              END IF;

              IF NOT v_schemas(c_schema_enq).objects.EXISTS(c_object_enq)
              THEN
                v_schemas(c_schema_enq)
                  .objects(c_object_enq).typenum := next_typenum;
              END IF;

              IF c_type_enq IS NOT NULL AND
                 NOT v_schemas(c_schema_enq).objects(c_object_enq).package_types.EXISTS(c_type_enq)
              THEN
                v_schemas(c_schema_enq).objects(c_object_enq).package_types(c_type_enq).typenum := next_typenum;
              END IF;
            END add_to_schemas;
        BEGIN
          IF p_rowtypes IS NOT NULL
          THEN
            DECLARE
              v_ii BINARY_INTEGER;
            BEGIN
              v_ii := p_rowtypes.FIRST;
              WHILE v_ii IS NOT NULL
              LOOP
                add_to_schemas(p_rowtypes(v_ii).schema_name, p_rowtypes(v_ii).table_name);
                v_ii := p_rowtypes.NEXT(v_ii);
              END LOOP;
            END;
          END IF;

          IF p_records IS NOT NULL
          THEN
            DECLARE
              v_ii BINARY_INTEGER;
            BEGIN
              v_ii := p_records.FIRST;
              WHILE v_ii IS NOT NULL
              LOOP
                add_to_schemas(p_records(v_ii).schema_name, p_records(v_ii).package_name, p_records(v_ii).type_name);
                v_ii := p_records.NEXT(v_ii);
              END LOOP;
            END;
          END IF;

          IF p_assarrays IS NOT NULL
          THEN
            DECLARE
              v_ii BINARY_INTEGER;
            BEGIN
              v_ii := p_assarrays.FIRST;
              WHILE v_ii IS NOT NULL
              LOOP
                add_to_schemas(p_assarrays(v_ii).schema_name, p_assarrays(v_ii).package_name, p_assarrays(v_ii).type_name);
                v_ii := p_assarrays.NEXT(v_ii);
              END LOOP;
            END;
          END IF;
        END fill_v_schemas;

        <<create_type_declarations>>
          DECLARE
            FUNCTION record_type_declaration
              (
               p_typenum t_type_number
               p_content CLOB
              )
            RETURN CLOB DETERMINISTIC
            IS
            BEGIN
              RETURN 'TYPE T_'||p_typenum||' IS RECORD ('||p_content||');'||chr(10);
            END record_type_declaration;

            FUNCTION record_field
              (
               p_typenum t_type_number
              ,p_field_name VARCHAR2
              ,p_types_package_name_enq VARCHAR2 DEFAULT make_types_package.p_types_package_name_enq
              )
            RETURN CLOB DETERMINISTIC
            IS
            BEGIN
              RETURN ','||p_field_name||' '||p_types_package_name_enq||'.T_'||p_typenum||chr(10);
            END append_field;

            PROCEDURE handle_type
              (
               p_typenum t_type_number
              ,p_field_name VARCHAR2
              ,p_content CLOB
              ,p_out_main_sql IN OUT CLOB
              ,p_out_field_list IN OUT CLOB
              )
            IS
            BEGIN
              p_out_main_sql
                := p_out_main_sql
                || record_type_declaration( p_typenum, p_content );
              p_out_field_list
                := ltrim(  p_out_field_list
                         ||record_field( p_typenum, p_field_name )
                        ,',');
            END handle_type;
          BEGIN
            DECLARE
              v_schema_fields CLOB;
              v_schema_name VARCHAR2(32);
            BEGIN
              v_schema_name := v_schemas.FIRST;
              WHILE v_schema_name IS NOT NULL
              LOOP
                DECLARE
                  c_schema CONSTANT t_schema
                    := v_schemas(v_schema_name);
                  v_object_fields CLOB;
                  v_object_name VARCHAR2(32);
                BEGIN
                  v_object_name := c_schema.objects.FIRST;
                  WHILE v_object_name IS NOT NULL
                  LOOP
                    DECLARE
                      c_object CONSTANT t_object
                        := c_schema.objects(v_object_name);
                      v_package_type_fields CLOB;
                      v_package_type_name VARCHAR2(32);
                    BEGIN
                      v_package_type_name := c_object.package_types.FIRST;
                      WHILE v_package_type_name IS NOT NULL
                      LOOP
                        handle_type
                          (
                           p_typenum => c_object.package_types(v_package_type_name)
                          ,p_field_name => v_package_type_name
                          ,p_content => 'dummy BOOLEAN'
                          ,p_out_main_sql => v_head
                          ,p_out_field_list => v_package_type_fields
                          );
                      END LOOP;

                      handle_type
                        (
                         p_typenum => c_object.typenum
                        ,p_field_name => v_object_name
                        ,p_content => v_package_type_fields
                        ,p_out_main_sql => v_head
                        ,p_out_field_list => v_object_fields
                        );
                    END;
                    v_object_name := c_schema.objects.NEXT(v_object_name);
                  END LOOP;

                  handle_type
                    (
                     p_typenum => c_schema.typenum
                    ,p_field_name => v_schema_name
                    ,p_content => v_object_fields
                    ,p_out_main_sql => v_head
                    ,p_out_field_list => v_schema_fields
                    );
                  v_schema_name := v_schemas.NEXT(v_schema_name);
                END;
              END LOOP;

              v_head
                := v_head
                || record_type_declaration
                    (
                     p_typenum => 0
                    ,p_content => v_schema_fields
                    );
            END;
          END create_type_declarations;

        <<create_constant_functions>>
          DECLARE
            PROCEDURE add_constant_function
              (
               p_function_name_enq VARCHAR2
              ,p_return_type VARCHAR2
              ,p_package_head IN OUT NOCOPY CLOB DEFAULT v_head
              ,p_package_body IN OUT NOCOPY CLOB DEFAULT v_body
              )
            IS
              v_function recon.t_pair_clob;
            BEGIN
              v_function :=
                recon.make_function
                  (
                   p_function_name_enq => p_function_name_enq
                  ,p_return_type => p_return_type' DETERMINISTIC'
                  ,p_return => 'NULL'
                  );
              p_package_head := p_package_head || v_function.c1 || chr(10);
              p_package_body := p_package_body || v_function.c2 || chr(10);
            END add_constant_function;
          BEGIN
            add_constant_function( '"OBJ"', 'T_0' );
            add_constant_function( '"DEFAULT_SCHEMA"', 'T_'||v_schemas(p_default_schema_enq).typenum );
            -- TODO: mode constants
          END create_constant_functions;

      <<execute_immediate_package_code>>
        DECLARE
          c_package CONSTANT recon.t_pair_clob
            := recon.make_package
                (
                 p_package_name_enq => c_types_package_name_enq
                ,p_head => v_head
                ,p_body => v_body
                );
        BEGIN
          EXECUTE IMMEDIATE c_package.c1;
          EXECUTE IMMEDIATE c_package.c2;
        END execute_immediate_package_code;
    END make_types_package;

  PROCEDURE make_main_package
    (
     p_package_name_enq VARCHAR2
    ,p_types_package_name_enq VARCHAR2
    ,p_schema_param_name_enq VARCHAR2
    ,p_package_param_name_enq VARCHAR2
    ,p_mode_param_name_enq VARCHAR2
    ,p_rowtype_default_mode t_mode
    ,p_record_default_mode t_mode
    ,p_assarray_max_items t_num_items_assarray
    ,p_rowtypes t_tab_rowtype_data
    ,p_records t_record_data
    ,p_assarrays t_assarray_data
    )
    IS
    BEGIN
      <<make_rowtype_constructors>>
      <<make_record_constructors>>
      <<make_assarray_constructors>>
    END make_main_package;

  FUNCTION make_function
    (
     p_function_name_enq VARCHAR2
    ,p_parameters CLOB DEFAULT NULL
    ,p_return_type VARCHAR2
    ,p_variable_declarations CLOB DEFAULT NULL
    ,p_body CLOB DEFAULT NULL
    ,p_return CLOB DEFAULT NULL
    )
    RETURN recon.t_pair_clob DETERMINISTIC
    IS
      v_f recon.t_pair_clob;
    BEGIN
      v_f.c1
        := 'FUNCTION '
        || p_function_name_enq
        || CASE
             WHEN p_parametrs IS NOT NULL
             THEN '('
               || p_parameters
               || ')'
           END
        || ' RETURN '
        || p_return_type;

      v_f.c2
        := v_f.c1
        || ' IS '
        || v_variable_declarations
        || 'BEGIN '
        || v_body
        || CASE
             WHEN p_return IS NOT NULL
             THEN 'RETURN '
               || v_return
               || ';'
           END
        || 'END '
        || p_function_name_enq
        || ';'
        || chr(10);

      v_f.c1 := v_f.c1||';'||chr(10);

      RETURN v_f;
    END make_function;

  FUNCTION make_package
    (
     p_package_name_enq VARCHAR2
    ,p_head CLOB
    ,p_body CLOB
    )
    RETURN recon.t_pair_clob DETERMINISTIC
    IS
      v_p recon.t_pair_clob;
    BEGIN
      v_p.c1
        := 'CREATE OR REPLACE PACKAGE '
        || p_package_name_enq
        || ' IS'
        || chr(10)
        || p_head
        || 'END '
        || p_package_name_enq
        || ';';

      v_p.c2
        := 'CREATE OR REPLACE PACKAGE BODY '
        || p_package_name_enq
        || ' IS'
        || chr(10)
        || p_body
        || 'END '
        || p_package_name_enq
        || ';';

      RETURN v_p;
    END make_function;

DECLARE
    v_sql_head CLOB;
    v_sql_body CLOB;
    v_buffer CLOB;
BEGIN

    v_sql_head := 'CREATE OR REPLACE PACKAGE '||c_package_name||' IS'||chr(10);
    v_sql_body := 'CREATE OR REPLACE PACKAGE BODY '||c_package_name||' IS'||chr(10);


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
              ||'.schemas.'
              ||rec.table_owner_enq
              ||'%type :='
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

  FUNCTION make_rowtype_constructor
    (
     p_schema_name_enq VARCHAR2
    ,p_table_name_enq VARCHAR2
    ,p_table_columns recon.t_table_columns

    ,p_schema_param_name_enq VARCHAR2
    ,p_mode_param_name_enq VARCHAR2

    ,p_schema_type VARCHAR2
    ,p_defaults_type VARCHAR2

    ,p_mode recon.t_mode
    ,p_is_mode_set BOOLEAN
    )
  RETURN recon.t_pair_clob DETERMINISTIC
  IS
    c_full_table_name CONSTANT VARCHAR2(100)
      := p_schema_name_enq||'.'||p_table_name_enq;
    c_rowtype CONSTANT VARCHAR2(100)
      := c_full_table_name||'%rowtype';
    v_parameters CLOB;
    v_assignments CLOB;
  BEGIN
    TYPE t_table_column IS RECORD
      (
       column_name VARCHAR2(30)
      ,data_default VARCHAR2(32767) -- can fail for very long data_defaults!
      );

    FOR ii IN 1..p_table_columns.COUNT
    LOOP
      DECLARE
        c_column_name_enq CONSTANT VARCHAR2(32)
          := dbms_assert.simple_sql_name(dbms_assert.enquote_name(p_table_columns(ii).column_name));
      BEGIN
        v_parameters :=
            v_parameters
          ||c_column_name_enq
          ||' '
          ||p_schema_name_enq
          ||'.'
          ||p_table_name_enq
          ||'.'
          ||c_column_name_enq
          ||'%type:=NULL'
          ||','
          ||chr(10);
        v_assignments :=
            v_assignments
          ||'v_r.'
          ||c_column_name_enq
          ||':='
          ||p_table_name_enq -- this is also the name of the constructor function
          ||'.'
          ||c_column_name_enq
          ||';'
          ||chr(10);
      END;
    END LOOP;

    RETURN recon.make_function
      (
       p_function_name_enq => p_table_name_enq
      ,p_parameters
        => v_parameters
        || !!! p_schema_param_name_enq VARCHAR2
           !!! p_mode_param_name_enq VARCHAR2
      ,p_return_type => c_rowtype
      ,p_body
         => 'DECLARE ' -- the inner block allows to avoid a name clash between the variable v_r and function parametrs
         || 'v_r '
         || c_rowtype
         || ';'
         || 'BEGIN '
         || v_assignments
         || 'RETURN v_r;'
         || 'END;'
      );
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