*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcx_exception IMPLEMENTATION.
  METHOD  get_text.
    result = lv_result.
  ENDMETHOD.                    "get_text
ENDCLASS.                    "lcl_exception IMPLEMENTATION

CLASS lcl_local IMPLEMENTATION.
  METHOD get_data.

    REFRESH : lt_where.
    CLEAR : lv_func, lv_value.
    TRY.
        get_tab_columns( table_name = table_name
                         key_yn     = key_yn ).
        create_sel_popup( ).

      CATCH lcx_exception.
        RAISE EXCEPTION TYPE lcx_exception.
    ENDTRY.
  ENDMETHOD.                    "get_data

  METHOD get_tab_columns.
    REFRESH : field_list_tab, fields_tab.
    cl_reca_ddic_tabl=>get_field_list_x(
      EXPORTING
        id_name            = table_name
        if_suppress_mandt  = abap_true
        if_suppress_nonkey = key_yn
      IMPORTING
        et_field_list_x    = field_list_tab
      EXCEPTIONS
        not_found          = 1
        OTHERS             = 2 ).
    IF sy-subrc <> 0.
      lcx_exception=>lv_result = 'Error getting column list'.
      RAISE EXCEPTION TYPE lcx_exception.
    ENDIF.

    LOOP AT field_list_tab INTO field_list.
      MOVE-CORRESPONDING field_list TO fields.
      APPEND fields TO fields_tab.
    ENDLOOP.
  ENDMETHOD.                    "get_tab_columns

  METHOD create_sel_popup.
    lv_rows = lines( fields_tab ).
    IF lv_rows GT 30.
      lv_rows = 30.
    ENDIF.
    CALL FUNCTION 'POPUP_GET_VALUES_SET_MAX_FIELD'
      EXPORTING
        number_of_fields = lv_rows
      EXCEPTIONS
        out_of_range     = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
        lv_rows = 10.  "Default
    ENDIF.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
      EXPORTING
        popup_title     = 'Input screen'
        start_column    = '5'
        start_row       = '4'
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = fields_tab
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      lcx_exception=>lv_result = 'Error Generating selection screen'.
      RAISE EXCEPTION TYPE lcx_exception.
    ELSE.
      IF lv_return = 'A'.
        lcx_exception=>lv_result = 'Action cancelled by user'.
        RAISE EXCEPTION TYPE lcx_exception.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "create_sel_popup
ENDCLASS.                    "lcl_local IMPLEMENTATION
