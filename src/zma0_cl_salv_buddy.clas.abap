CLASS zma0_cl_salv_buddy DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_fields TYPE TABLE OF sval
    WITH NON-UNIQUE DEFAULT KEY.

    EVENTS on_columns_preparation
      EXPORTING VALUE(ex_columns) TYPE REF TO cl_salv_columns_table.

    METHODS constructor
      IMPORTING
        VALUE(im_data)      TYPE table
        VALUE(im_container) TYPE REF TO cl_gui_custom_container OPTIONAL.

    CLASS-METHODS factory
      IMPORTING
        VALUE(im_data)      TYPE table
        VALUE(im_container) TYPE REF TO cl_gui_custom_container OPTIONAL
      RETURNING
        VALUE(re_result)    TYPE REF TO zma0_cl_salv_buddy.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA salv_table TYPE REF TO cl_salv_table.

    DATA fields TYPE zma0_cl_salv_buddy=>tt_fields.

    METHODS extract_components
      IMPORTING
        VALUE(im_data)   TYPE table
      RETURNING
        VALUE(re_result) TYPE abap_compdescr_tab.

    METHODS extract_data_fields
      IMPORTING
        VALUE(im_data)   TYPE table
      RETURNING
        VALUE(re_result) TYPE zma0_cl_salv_buddy=>tt_fields.

    METHODS columns_preparation.

    METHODS enable_all_functions.

    METHODS toolbar_preparation.

    METHODS key_attributes_preparation
      IMPORTING
        VALUE(im_ucomm) TYPE syst-ucomm.

    METHODS handle_toolbar FOR EVENT user_command
    OF cl_gui_alv_grid IMPORTING e_ucomm.

    METHODS display_modal
      IMPORTING
        VALUE(im_ucomm) TYPE syst-ucomm.

    METHODS initialize_modal.

    METHODS set_values_modal.

ENDCLASS.



CLASS zma0_cl_salv_buddy IMPLEMENTATION.
  METHOD constructor.

    TRY.

        cl_salv_table=>factory(
          EXPORTING
            r_container    = im_container
          IMPORTING
            r_salv_table   = me->salv_table
          CHANGING
            t_table        = im_data
        ).

        me->extract_data_fields( im_data ).

        me->enable_all_functions( ).

        me->columns_preparation( ).

        me->toolbar_preparation( ).

        me->salv_table->display( ).

      CATCH cx_salv_msg.

    ENDTRY.

  ENDMETHOD.

  METHOD factory.
    re_result = NEW #(
      im_data = im_data
      im_container = im_container
    ).
  ENDMETHOD.

  METHOD toolbar_preparation.

    DATA(lo_toolbar) = me->salv_table->get_functions( ).

    lo_toolbar->set_all( abap_true ).

    TRY.

        lo_toolbar->add_function(
          EXPORTING
            name     = 'CREATE'    " ALV Function
            icon     = CONV #( icon_create )
            text     = 'Create'
            tooltip  = 'Create a new line'
            position = if_salv_c_function_position=>right_of_salv_functions
        ).

        lo_toolbar->add_function(
          EXPORTING
            name     = 'UPDATE'    " ALV Function
            icon     = CONV #( icon_change )
            text     = 'Update'
            tooltip  = 'Update the selected line'
            position =
              if_salv_c_function_position=>right_of_salv_functions
        ).

        lo_toolbar->add_function(
          EXPORTING
            name     = 'DELETE'    " ALV Function
            icon     = CONV #( icon_delete )
            text     = 'Delete'
            tooltip  = 'Delete the selected line'
            position =
              if_salv_c_function_position=>right_of_salv_functions
        ).

      CATCH cx_salv_wrong_call cx_salv_existing INTO DATA(lo_cx).
        MESSAGE lo_cx TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    SET HANDLER me->handle_toolbar FOR ALL INSTANCES.
  ENDMETHOD.

  METHOD columns_preparation.

    DATA(lr_columns) = me->salv_table->get_columns( ).

    TRY.
        lr_columns->get_column( 'MANDT' )->set_technical( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lr_columns->set_optimize( ).

    RAISE EVENT on_columns_preparation
      EXPORTING ex_columns = lr_columns.

  ENDMETHOD.

  METHOD enable_all_functions.

    me->salv_table->get_functions(  )->set_all( ).

  ENDMETHOD.

  METHOD handle_toolbar.

    DATA lv_return TYPE c.

    CASE e_ucomm.

      WHEN 'CREATE'.

        me->initialize_modal( ).
        me->key_attributes_preparation( e_ucomm ).
        me->display_modal( e_ucomm ).

      WHEN 'UPDATE'.

        me->set_values_modal( ).
        me->key_attributes_preparation( e_ucomm ).
        me->display_modal( e_ucomm ).

      WHEN 'DELETE'.

    ENDCASE.

  ENDMETHOD.

  METHOD extract_components.

    DATA lr_strucdesc TYPE REF TO cl_abap_structdescr.
    DATA lo_data_ref TYPE REF TO data.

    FIELD-SYMBOLS <data_ref_struct> TYPE any.

    CREATE DATA lo_data_ref LIKE LINE OF im_data.
    ASSIGN lo_data_ref->* TO <data_ref_struct>.

    CHECK <data_ref_struct> IS ASSIGNED.

    lr_strucdesc ?= cl_abap_typedescr=>describe_by_data(
        <data_ref_struct>
    ).

    re_result = lr_strucdesc->components[].

  ENDMETHOD.

  METHOD extract_data_fields.

    DATA(lo_columns) = me->salv_table->get_columns( ).

    LOOP AT me->extract_components( im_data ) INTO DATA(lw_component).

      CHECK lw_component-name NE 'MANDT'.

      TRY.

          DATA(lw_ddic_ref) = lo_columns->get_column(
            lw_component-name
          )->get_ddic_reference( ).

          APPEND VALUE #(
              tabname   = lw_ddic_ref-table
              fieldname = lw_ddic_ref-field
          ) TO me->fields.

        CATCH cx_salv_not_found.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD key_attributes_preparation.

    DATA lr_column TYPE REF TO cl_salv_column_table.

    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).

      TRY.
          lr_column ?= me->salv_table->get_columns( )->get_column(
              <field>-fieldname
          ).
        CATCH cx_salv_not_found.
      ENDTRY.

      CHECK lr_column->is_key( ).

      <field>-field_attr = COND #(
        WHEN im_ucomm = 'CREATE'
        THEN space ELSE '02'
      ).

      <field>-field_obl = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_modal.

    DATA lv_return TYPE c.

    DATA(lv_title) = COND #(
        WHEN im_ucomm = 'CREATE'
        THEN 'Create Data'
        ELSE 'Update Data'
    ).

    CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
      EXPORTING
        popup_title     = lv_title
        start_column    = '5'
        start_row       = '5'
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = me->fields
      EXCEPTIONS
        error_in_fields = 1
        error_message   = 2
        OTHERS          = 3.

  ENDMETHOD.

  METHOD initialize_modal.

    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).
      CLEAR <field>-value.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_values_modal.

    me->salv_table->get_metadata( ).
    DATA(lr_selections) = me->salv_table->get_selections( ).
    DATA(lr_selected_rows) = lr_selections->get_selected_rows( ).


  ENDMETHOD.

ENDCLASS.
