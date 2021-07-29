*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS : get_text REDEFINITION.
    CLASS-DATA : lv_result TYPE string.
ENDCLASS.                    "lcl_exception DEFINITION

CLASS lcl_local DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS get_data
      IMPORTING table_name TYPE tabname
                key_yn     TYPE char01
      RAISING lcx_exception.

  PRIVATE SECTION.
    METHODS get_tab_columns
      IMPORTING table_name TYPE tabname
                key_yn     TYPE char01
      RAISING lcx_exception.

    METHODS create_sel_popup RAISING lcx_exception.

    TYPES : BEGIN OF ty_where,
               where_clause TYPE char72,
            END OF ty_where.

    DATA : field_list_tab TYPE dd03ttyp,
           field_list     TYPE dd03p,
           fields_tab     TYPE STANDARD TABLE OF sval,
           fields         TYPE sval,
           lt_where       TYPE STANDARD TABLE OF ty_where,
           ls_where       TYPE ty_where.

    DATA : lt_cond        TYPE STANDARD TABLE OF hrcond,
           ls_cond        TYPE hrcond.

    DATA : lv_return      TYPE char01,
           lv_value       TYPE char100,
           lv_val         TYPE char100,
           lv_func        TYPE char30,
           lv_tabix       TYPE sy-tabix,
           lv_rows        TYPE char03.

    DATA : ref_table TYPE REF TO data,
           gr_table  TYPE REF TO cl_salv_table.

    CONSTANTS : lv_quote   TYPE char01 VALUE `'`.

ENDCLASS.                    "lcl_local DEFINITION
