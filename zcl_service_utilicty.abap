class ZCL_SERVICES_UTILITY definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_data,
        sign   TYPE tvarvc-sign,
        option TYPE tvarvc-opti,
        low    TYPE tvarvc-low,
        high   TYPE tvarvc-high,
      END OF ty_data .
  types:
    tt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY .
  types:
    BEGIN OF ty_cache,
        name    TYPE tvarvc-name,
        lt_data TYPE tt_data,
      END OF ty_cache .
  types:
    BEGIN OF ty_export,
        objects   TYPE stxh-tdobject,
        names     TYPE stxh-tdname,
        ids       TYPE stxh-tdid,
        languages TYPE stxh-tdspras,
        line      TYPE string,
      END OF ty_export .
  types:
    tt_tlines TYPE STANDARD TABLE OF tline WITH EMPTY KEY .
  types:
    t_export TYPE STANDARD TABLE OF ty_export .
  types W_EXPORT type TY_EXPORT .
  types:
    tt_cache TYPE HASHED TABLE OF ty_cache WITH UNIQUE KEY name .
  types:
    tt_textpool TYPE TABLE OF textpool .
  types:
    tt_objects   TYPE RANGE OF stxh-tdobject .
  types:
    tt_names     TYPE RANGE OF stxh-tdname .
  types:
    tt_ids       TYPE RANGE OF stxh-tdid .
  types:
    tt_languages TYPE RANGE OF stxh-tdspras .

  class-data:
    it_export TYPE STANDARD TABLE OF ty_export .
  class-data WA_EXPORT type TY_EXPORT .
  class-data GC_I type CHAR1 read-only value 'I' ##NO_TEXT.
  class-data GC_E type CHAR1 read-only value 'E' ##NO_TEXT.
  class-data GC_EQ type CHAR2 read-only value 'EQ' ##NO_TEXT.
  class-data GC_BT type CHAR2 read-only value 'BT' ##NO_TEXT.
  class-data GT_CACHE type TT_CACHE .

  class-methods SUBMIT_ANY_REPORT
    importing
      value(IV_PROGRAMM) type PROGRAMM
      !IV_DISPLAY type FLAG optional
      !IV_METADATA type FLAG optional
      !IV_DATA type FLAG default 'X'
      value(IV_HDATA_FLAG) type FLAG
      value(IV_LDATA_FLAG) type FLAG
      !IS_H_STRUCTURE type TABNAME optional
      !IS_L_STRUCTURE type TABNAME optional
      !IT_SELECTION type RSSELECT_TAB optional
    exporting
      !ET_DATA_H type ref to DATA
      !ET_DATA_L type ref to DATA
    raising
      resumable(CX_SALV_BS_SC_RUNTIME_INFO) .
  class-methods SEND_NOTIFICATION
    importing
      value(IV_SUBJECT) type SO_OBJ_DES
      value(IV_CONTENT) type STRING optional
      value(IV_SENDER) type ADR6-SMTP_ADDR optional
      value(IV_TABLE) type ref to DATA optional
      value(IV_ATT_SUBJECT) type SOOD-OBJDES optional
      value(IV_ATT_TYPE) type SOODK-OBJTP optional
      value(IV_ATTACHMENT) type SOLIX_TAB optional
      value(IV_RECIPIENT) type AD_SMTPADR
      value(IV_COMMIT) type BOOLEAN default 'X'
    returning
      value(RV_ERROR_MSG) type STRING .
  class-methods APPLICATION_MESSG_LOGGER
    importing
      value(IV_OBJECT) type BALOBJ_D
      value(IV_SUBOBJ) type BALSUBOBJ
      value(IT_MESSAGES) type BAPIRET2_T optional .
  class-methods GET_APPLLICATION_MESSG_LOGS
    importing
      value(IV_APLOG) type BALOGNR
    returning
      value(RV_APLOG_MESSG) type CHAR70 .
  class-methods MASS_READ_TEXT
    importing
      value(IV_OBJECTS) type TT_OBJECTS
      value(IV_NAMES) type TT_NAMES
      value(IV_IDS) type TT_IDS
      value(IV_LANGUAGES) type TT_LANGUAGES
      value(IV_PACKAGE_SIZE) type I default 1000
      !IV_RBMOM type FLAG default ' '
    exporting
      !ET_LINES type T_EXPORT .
  class-methods ALV_SALV_TABLE_FACTORY
    importing
      !IV_FUNC type CHAR1 default 'X'
    changing
      !CT_OUTPUT type TABLE .
  class-methods ALV_SET_COLUMN_HEADER
    importing
      !IV_FIELDNAME type LVC_FNAME
      !IV_LONGTEXT type SCRTEXT_L
      !IV_MEDIUMTEXT type SCRTEXT_M optional
      !IV_SHORTTEXT type SCRTEXT_S optional
      value(IV_AUTO) type FLAG optional .
  class-methods ALV_SORT_FIELDS
    importing
      !IV_COLUMNNAME type LVC_FNAME
      !IV_POSITION type I optional
      !IV_SEQUENCE type SALV_DE_SORT_SEQUENCE default IF_SALV_C_SORT=>SORT_UP
      !IV_SUBTOTAL type SAP_BOOL default IF_SALV_C_BOOL_SAP=>FALSE .
  class-methods ALV_HIDE_FIELDS
    importing
      !IV_COLUMNNAME type LVC_FNAME .
  class-methods ALV_SALV_ADDITIONAL
    importing
      value(IV_SET) type FLAG optional
    changing
      !CV_SALV type ref to CL_SALV_TABLE .
  class-methods ALV_SALV_IDA_FCAT_EXTRACT
    importing
      value(IV_DDLNAME) type DDLNAME
    changing
      !CT_SALV_IDA type ref to IF_SALV_GUI_TABLE_IDA
    exceptions
      TEXT_MORE_THAN_40 .
  class-methods ALV_DISPLAY_ALV
    exceptions
      OBJ_SALV_NOT_BOUND .
  class-methods ALV_IDA_TOOL
    importing
      value(IV_TABLE_NAME) type DBTABL
      value(IV_SET_CDS_FLA) type FLAG optional
      !IV_XUOBJECT type XUOBJECT optional
      !IV_XUOBJECT_FIELDNAME type FIELDNAME optional
      !IT_RSSELECT_TAB type IF_SALV_SERVICE_TYPES=>YT_NAMED_RANGES optional
    raising
      CX_SALV_DB_CONNECTION
      CX_SALV_DB_TABLE_NOT_SUPPORTED
      CX_SALV_FUNCTION_NOT_SUPPORTED
      CX_SALV_PARAM_OUT_OF_BOUNDS
      CX_SALV_IDA_ASSOCIATE_INVALID
      CX_SALV_IDA_CONDITION_INVALID
      CX_SALV_IDA_UNKNOWN_NAME
      CX_SALV_CALL_AFTER_1ST_DISPLAY
      CX_SALV_IDA_DUPLICATE_NAME
      CX_SALV_IDA_CONTRACT_VIOLATION
      CX_SALV_KEY_FIGURE_DEFINITION .
  class-methods TVARVC_CHECK_VALUE
    importing
      !IV_NAME type SIMPLE
      !IV_VALUE type DATA
    returning
      value(RV_RESULT) type FLAG .
  class-methods TVARVC_GET_VALUE
    importing
      value(IV_NAME) type SIMPLE
      value(IV_DEFAULT) type DATA optional
    returning
      value(RV_VALUE) type CHAR255 .
  class-methods TVARVC_GET_RANGE
    importing
      !IV_NAME type SIMPLE
    returning
      value(RT_RANGE) type TT_DATA .
  class-methods BRF_GET_CONSTANT_VALUES
    importing
      !IV_RICEFW_NUM type CHAR12
      !IV_OBJECT_ID type CHAR30 optional
      !IV_FIELD_NAME type FIELDNAME optional
      !IV_CONSTANT type CHAR30 optional
    exporting
      !ET_ERROR_MESSAGES type IF_FDT_TYPES=>T_MESSAGE
      value(EV_VALUE) type ANY
    raising
      CX_SY_ITAB_LINE_NOT_FOUND .
  class-methods GET_F4_HELP_APPL_SERVER_FILE
    importing
      value(IV_SOURCE_PATH) type DXFIELDS-LONGPATH
    returning
      value(RV_FILE_NAME) type STRING .
  class-methods REGEX_MATCHER
    importing
      value(IV_PATTERN) type STRING
      value(IV_IGNORE_CASE) type FLAG default ABAP_TRUE
      value(IV_INPUT) type STRING
    returning
      value(RV_SUCCESS) type FLAG
    exceptions
      REGEX_ERROR
      MATCH_ERROR .
  class-methods FILE_NAME_VALIDATE
    importing
      value(IV_LGCL_FNAME) type FILENAME-FILEINTERN
      !IV_APPL_FNAME type PATH-PATHEXTERN
    returning
      value(RV_SUCCESS) type FLAG
    exceptions
      NO_FILEPATH .
  class-methods SAVE_FILE_APPL
    importing
      value(IV_LGCL_FNAME) type FILENAME-FILEINTERN optional
      !IV_APPL_FNAME type PATH-PATHEXTERN optional
      !IT_DATA type TTOCS_STRING
      value(IV_LONGTIMESTAMP) type CHAR1 optional
      value(IV_FLAG) type FLAG optional
    exceptions
      NO_FILEPATH
      ERROR_APPL
      CLOSE_ERROR
      VALIDATION_FAILED .
  class-methods XML_FORMAT_SAVE_FILE_APPL
    importing
      value(IV_LGCL_FNAME) type FILENAME-FILEINTERN
      value(IV_XML_HEADTAG) type CDOBJECTCL
      value(IV_MODIFY_FPATH) type FLAG optional
    changing
      !CT_DATA type TTOCS_STRING
    exceptions
      FILE_NOT_FOUND .
  class-methods SAP_WEB_REPOSITORY_SMW0
    importing
      value(IV_TEMPLATE_NAME) type TADIR-OBJ_NAME
      value(IV_EXTENSION) type CHAR10 default '.csv' .
  class-methods EXCEL_CSV_DOWNLOAD
    importing
      value(IT_TABLE) type TABLE
      !IT_TEXTPOOL type TT_TEXTPOOL optional
      !IV_FULLPATH type STRING
    exceptions
      INVALID_EXTENSION .
protected section.

  class-data GO_SALV type ref to CL_SALV_TABLE .
  class-data GO_EXC type ref to CX_ROOT .
  class-data GO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .

  class-methods TVARVC_GET_REF
    importing
      !IV_NAME type SIMPLE
    returning
      value(RV_CACHE) type ref to TY_CACHE .
  class-methods ONE_TEXT
    importing
      !IV_OBJECT type STXL-TDOBJECT
      !IV_NAME type STXL-TDNAME
      !IV_ID type STXL-TDID
      !IV_LANGUAGE type STXL-TDSPRAS
      !IT_LINES type TT_TLINES
    changing
      !CT_EXPORT type T_EXPORT
      !CS_EXPORT type T_EXPORT .
private section.
ENDCLASS.



CLASS ZCL_SERVICES_UTILITY IMPLEMENTATION.


  METHOD ALV_DISPLAY_ALV.

    IF go_salv IS BOUND.
      "Display alv
      CALL METHOD go_salv->display.
    ELSE.
      RAISE obj_salv_not_bound.
    ENDIF.

  ENDMETHOD.


  METHOD ALV_HIDE_FIELDS.
    " Call ALV Method to Hide the fields
    TRY.
        DATA(lo_column) = go_columns->get_column( columnname = iv_columnname ).
        lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      CATCH cx_salv_not_found INTO DATA(lo_salv_exc).
        DATA(lv_excmsg) = lo_salv_exc->get_text( ).
        MESSAGE lv_excmsg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD ALV_IDA_TOOL.
    DATA:
      iv_number_of_rows TYPE i VALUE '2000',
      iv_unrestricted   TYPE abap_bool VALUE ''
      .


    "Check the DB Capabilities
    CHECK
    cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = iv_table_name ).

    IF iv_set_cds_fla = abap_true."CDS Flag Set As 'X'
      "Create ALV For CDS View
      TRY.
          cl_salv_gui_table_ida=>create_for_cds_view(
            EXPORTING
              iv_cds_view_name      = iv_table_name
*            IO_GUI_CONTAINER      = IO_GUI_CONTAINER
*            IO_CALC_FIELD_HANDLER = IO_CALC_FIELD_HANDLER
            RECEIVING
              ro_alv_gui_table_ida  = DATA(ro_ida)
          ).
        CATCH cx_salv_db_connection INTO DATA(ls_salv_db_connection).          " Error connecting to database
          MESSAGE ls_salv_db_connection->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
        CATCH cx_salv_db_table_not_supported INTO DATA(ls_salv_db_table_not_supported). " DB table / view is not supported
          MESSAGE ls_salv_db_table_not_supported->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
        CATCH cx_salv_ida_contract_violation INTO DATA(ls_salv_ida_contract_violation). " IDA API contract violated by caller
          MESSAGE ls_salv_ida_contract_violation->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
        CATCH cx_salv_function_not_supported INTO DATA(ls_salv_function_not_supported). " Funcionality is not supported
          MESSAGE ls_salv_function_not_supported->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
      ENDTRY.
    ELSE. "Table
      "Create ALV for DB Table
      TRY.
          cl_salv_gui_table_ida=>create(
            EXPORTING
              iv_table_name         = iv_table_name         " Name of table for database access
*          IO_GUI_CONTAINER      = IO_GUI_CONTAINER      " Container for ALV display
*          IO_CALC_FIELD_HANDLER = IO_CALC_FIELD_HANDLER " IDA API: Handler for calculated fields
            RECEIVING
              ro_alv_gui_table_ida  = ro_ida  " ALV with integrated data access (IDA)
          ).
        CATCH cx_salv_db_connection INTO ls_salv_db_connection.          " Error connecting to database
          MESSAGE ls_salv_db_connection->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
        CATCH cx_salv_db_table_not_supported INTO ls_salv_db_table_not_supported. " DB table / view is not supported
          MESSAGE ls_salv_db_table_not_supported->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
        CATCH cx_salv_ida_contract_violation INTO ls_salv_ida_contract_violation. " IDA API contract violated by caller
          MESSAGE ls_salv_ida_contract_violation->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
      ENDTRY.
    ENDIF.
    "Allowed Maximum Row Recommanded

    IF cl_salv_gui_table_ida=>db_capabilities( )->is_max_rows_recommended( ).
      TRY.
          ro_ida->set_maximum_number_of_rows(
            EXPORTING
              iv_number_of_rows = iv_number_of_rows
              iv_unrestricted   = iv_unrestricted
          ).
        CATCH cx_salv_param_out_of_bounds INTO DATA(ls_salv_param_out_of_bounds). " Parameter is out of bounds
          MESSAGE ls_salv_param_out_of_bounds->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
      ENDTRY.
    ENDIF.

*{  Authorization Check
    IF iv_xuobject IS NOT INITIAL AND iv_xuobject_fieldname IS NOT INITIAL.
      TRY.
          ro_ida->add_authorization_for_object(
            EXPORTING
              iv_authorization_object = iv_xuobject
              it_activities           = VALUE if_salv_gui_types_ida=>yt_activities(
                                              ( auth_field = 'ACTVT' value = '03' ) )
              it_field_mapping        = VALUE if_salv_gui_types_ida=>yt_field_mapping(
                                              ( auth_field = iv_xuobject_fieldname view_field = iv_xuobject_fieldname ) )
          ).
        CATCH cx_salv_function_not_supported INTO ls_salv_function_not_supported. " Funcionality is not supported
          MESSAGE ls_salv_function_not_supported->get_text( ) TYPE sy-abcde+18(1) DISPLAY LIKE sy-abcde+4(1).
      ENDTRY.
    ENDIF.
*}  Authorization Check

*{  Apply Filter
    TRY.
        ro_ida->set_select_options(
          EXPORTING
            it_ranges    = it_rsselect_tab[]
*          IO_CONDITION = IO_CONDITION;
        ).
      CATCH cx_salv_ida_associate_invalid. " Superclass for all dynamic ALV IDA exceptions
      CATCH cx_salv_db_connection.         " Error connecting to database
      CATCH cx_salv_ida_condition_invalid. " Superclass for all dynamic ALV IDA exceptions
      CATCH cx_salv_ida_unknown_name.      " Unknown name: FieldName,DataElementName,...
    ENDTRY.
*}  Apply Filter
  ENDMETHOD.


  METHOD ALV_SALV_ADDITIONAL.
    IF iv_set = abap_true.
      go_salv = cv_salv.
    ELSE.
      cv_salv = go_salv.
    ENDIF.
  ENDMETHOD.


  METHOD ALV_SALV_IDA_FCAT_EXTRACT.
    CONSTANTS: lc_annoname   TYPE ddannotation_key VALUE 'ENDUSERTEXT.LABEL'.
    DATA: lv_field_name  TYPE if_salv_gui_types_ida=>y_field_name,
          lv_header_text TYPE if_salv_gui_types_ida=>y_header_text,
          lv_str         TYPE string.

    "Get Annotaions
    cl_dd_ddl_analyze=>get_annotations(
    EXPORTING
      ddlnames    =    VALUE #( ( iv_ddlname ) )      " DDL Source Names
      leaves_only = abap_true        " ABAP_TRUE: returns all sheet annotations
    IMPORTING
      headerannos = DATA(lt_header_anno)    " DD: Table for Header Annotations
      fieldannos  = DATA(lt_fields_anno)    " DD: Table for Generic Field Annotations on Fields
      parannos    = DATA(lt_param_anno)     " DD: Table for Generic Annotations to Parameters
    ).

    "Language other than english
    IF sy-langu <> sy-abcde+4(1).
      "Get Transaltions for Annotation maintained in SE63
      SELECT ddlname,
             strucobjn,
             nodename,
             ddlanguage,
             as4local,
             fieldname,
             depth,
             ddtext,
             fieldlabel,
             quickinfo
             FROM ddddlsrc03nt
             WHERE ddlname = @iv_ddlname
               AND ddlanguage = @sy-langu
             INTO TABLE @DATA(lt_field_translations).
      IF sy-subrc <> 0.
        CLEAR lt_field_translations.
      ENDIF.
    ENDIF.

    LOOP AT lt_fields_anno ASSIGNING FIELD-SYMBOL(<lfs_fields_anno>) WHERE annoname = lc_annoname."#EC CI_STDSEQ
*   Column Description
      TRY.
          lv_field_name =  <lfs_fields_anno>-fieldname.
          "Language other than english
          IF sy-langu <> sy-abcde+4(1).
            lv_str = VALUE #( lt_field_translations[ fieldname = <lfs_fields_anno>-fieldname ]-fieldlabel OPTIONAL )."#EC CI_STDSEQ
          ELSE.
            "For lang when english
            lv_str =  <lfs_fields_anno>-value.
            REPLACE ALL OCCURRENCES OF '''' IN lv_str WITH space.
          ENDIF.

          "Validate Length - Max length of Field Header is 40
          IF strlen( lv_str ) <= 40.
            lv_header_text = lv_str.
            "Set Column Name
            ct_salv_ida->field_catalog( )->set_field_header_texts(
                                                                    iv_field_name    = lv_field_name
                                                                    iv_header_text   = lv_header_text
                                                                  ).
          ELSE.
            RAISE text_more_than_40.
          ENDIF.
        CATCH cx_salv_ida_unknown_name
        cx_salv_call_after_1st_display.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD ALV_SALV_TABLE_FACTORY.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_salv
          CHANGING
            t_table      = ct_output.

      CATCH cx_salv_msg INTO go_exc.
        DATA(lv_excmsg)  = go_exc->if_message~get_text( ).
        MESSAGE lv_excmsg TYPE gc_e.
    ENDTRY.

    "Generic Functions
    IF iv_func = abap_true.
      go_salv->get_functions( )->set_all( abap_true ).
    ELSE.
      go_salv->get_functions( )->set_all( abap_false ).
    ENDIF.
  ENDMETHOD.


  METHOD ALV_SET_COLUMN_HEADER.
    "  Changing the column headers
    CALL METHOD go_salv->get_columns
      RECEIVING
        value = go_columns.

    IF go_columns IS BOUND.
      " Optimize the Column Width
      CALL METHOD go_columns->set_optimize
        EXPORTING
          value = abap_true.


      " Call ALV Method to set the Cloumn Headers
      TRY.
          DATA(lo_column) =  go_columns->get_column( iv_fieldname ).
        CATCH cx_salv_not_found INTO DATA(lo_exc_msg).

          IF lo_exc_msg IS BOUND.
            DATA(lv_exc_msg)  = lo_exc_msg->get_text( ).
            MESSAGE lv_exc_msg  TYPE 'I'.
          ENDIF.
      ENDTRY.

      IF iv_auto = abap_true.
        lo_column->set_long_text( iv_longtext ).
        lo_column->set_medium_text( iv_longtext+0(20) ).
        lo_column->set_short_text( iv_longtext+0(10) ).
      ELSE.
        lo_column->set_long_text( iv_longtext ).
        lo_column->set_medium_text( iv_mediumtext ).
        lo_column->set_short_text( iv_shorttext ).
      ENDIF.

      FREE lo_column.

    ENDIF.
  ENDMETHOD.


  METHOD ALV_SORT_FIELDS.
    DATA(lo_sort) = go_salv->get_sorts( ) .
    lo_sort->clear( ).

    TRY.
        lo_sort->add_sort(
       columnname = iv_columnname
       position   = iv_position
       subtotal   = iv_subtotal
       sequence   = iv_sequence ).
        "Exception Handling
      CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error INTO DATA(lo_salv_exp).
        DATA(lv_text) = lo_salv_exp->get_text( ).
        MESSAGE lv_text TYPE sy-abcde+4(1)." WITH lv_text.
    ENDTRY.
  ENDMETHOD.


  METHOD APPLICATION_MESSG_LOGGER.
    DATA:
      ls_log_handle   TYPE balloghndl,
      lv_timestamp    TYPE tzntstmps,
      lv_timezone     TYPE timezone VALUE 'UTC',
      ls_str_log      TYPE bal_s_log,
      ls_str_balmsg   TYPE bal_s_msg,
      ls_str_message  TYPE bapiret2,
      ls_msg_logged   TYPE boolean,
      lt_tab_messages TYPE bapiret2_t.

    CONVERT DATE sy-datum TIME sy-uzeit
    INTO TIME STAMP lv_timestamp TIME ZONE lv_timezone.

    ls_str_log-extnumber = lv_timestamp.
    CONDENSE ls_str_log-extnumber.
    ls_str_log-object = iv_object.
    ls_str_log-subobject = iv_subobj.
    ls_str_log-aldate_del = sy-datum + 5.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_str_log
      IMPORTING
        e_log_handle            = ls_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_str_message-message.
    ELSE.
      LOOP AT it_messages INTO ls_str_message.
        MOVE:
        ls_str_message-type       TO ls_str_balmsg-msgty,
        ls_str_message-id         TO ls_str_balmsg-msgid,
        ls_str_message-number     TO ls_str_balmsg-msgno,
        ls_str_message-message_v1 TO ls_str_balmsg-msgv1,
        ls_str_message-message_v2 TO ls_str_balmsg-msgv2,
        ls_str_message-message_v3 TO ls_str_balmsg-msgv3,
        ls_str_message-message_v4 TO ls_str_balmsg-msgv4.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = ls_log_handle
            i_s_msg          = ls_str_balmsg
          IMPORTING
            e_msg_was_logged = ls_msg_logged
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_str_message-message.
        ENDIF.
      ENDLOOP.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_save_all       = abap_true
          EXCEPTIONS
            log_not_found    = 1
            save_not_allowed = 2
            numbering_error  = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_str_message-message.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD BRF_GET_CONSTANT_VALUES.
*    TYPES: BEGIN OF TY_SELOPT,
*             SIGN   TYPE IM_SELOPT_SIGN,
*             OPTION TYPE IM_SELOPT_OPTION,
*             LOW    TYPE RVARI_VAL_255,
*             HIGH   TYPE RVARI_VAL_255,
*           END OF TY_SELOPT.
*
*    DATA: LO_FUCTION  TYPE REF TO IF_FDT_FUNCTION,
*          LO_CONTEXT  TYPE REF TO IF_FDT_CONTEXT,
*          LO_RESULT   TYPE REF TO IF_FDT_RESULT,
**          lt_result  TYPE REF TO data,
*          LO_MESSAGE  TYPE REF TO CX_FDT,
*          LT_VALUE    TYPE TT_CONSTANT,
*          LT_RANGE    TYPE STANDARD TABLE OF TY_SELOPT,
*          LDO_DATA    TYPE REF TO DATA,
*          LW_CONSTANT TYPE ZCRXS_CONSTANT.
*
*    FIELD-SYMBOLS: <FS_RANGE> TYPE ANY TABLE.
*
*    GET REFERENCE OF EP_VALUE INTO LDO_DATA.
*
*
*    IF GT_CONSTANTS[] IS INITIAL.
*      DATA(LV_GET_VALUE) = ABAP_TRUE.
*    ELSE.
*      IF LINE_EXISTS( GT_CONSTANTS[ ZL_RICEFW_NUM = IM_RICEFW_NUM ZL_OBJECT_ID = IM_OBJECT_ID ] ).
*        DATA(LT_CONSTANTS) = GT_CONSTANTS[].
*      ELSE.
*        LV_GET_VALUE = ABAP_TRUE.
*      ENDIF.
*    ENDIF.
*
*    IF LV_GET_VALUE = ABAP_TRUE.
*      CLEAR: GT_CONSTANTS[].
*
*      TRY .
*
*          "Get BRFplus function
*
*          LO_FUCTION ?= CL_FDT_FACTORY=>IF_FDT_FACTORY~GET_INSTANCE( )->GET_FUNCTION(  GC_CONST_ID  ).
*
*          "Set the BRFplus function context ( input variables )
*
*          LO_CONTEXT = LO_FUCTION->GET_PROCESS_CONTEXT( ).
*
*          "Call below line for no of input paramters, iv_name = inpur paramter and ia_value = its value
*
*          LO_CONTEXT->SET_VALUE(  IV_NAME  = 'ZL_RICEFW_NUM'
*                                  IA_VALUE = IM_RICEFW_NUM  ).
*
*          "Process the BRFplus function, pass input paramter values and fetch the output
*
*          LO_FUCTION->PROCESS(
*            EXPORTING
*              IO_CONTEXT   =  LO_CONTEXT
*            IMPORTING
*              EO_RESULT    =  LO_RESULT ).
*
*          "Retrieve the brfplus function result into field symbol <result>
*          LO_RESULT->GET_VALUE( IMPORTING EA_VALUE = LT_VALUE ).
*
*
*        CATCH CX_FDT INTO LO_MESSAGE.
*          "Populate Error Messages
*          CLEAR LT_VALUE.
*          ET_ERROR_MESSAGES[] = LO_MESSAGE->MT_MESSAGE[].
*      ENDTRY.
*      LOOP AT LT_VALUE INTO DATA(LW_DATA).
*        LW_CONSTANT = CORRESPONDING #( BASE ( LW_CONSTANT ) LW_DATA ).
*        LW_CONSTANT-ZL_RICEFW_NUM = IM_RICEFW_NUM.
*        APPEND LW_CONSTANT  TO GT_CONSTANTS.
*        CLEAR:LW_DATA, LW_CONSTANT.
*      ENDLOOP.
*
*      IF GT_CONSTANTS IS NOT INITIAL.
*        LT_CONSTANTS = GT_CONSTANTS[].
*      ENDIF.
*    ENDIF.
*
*    IF IM_OBJECT_ID IS SUPPLIED.
*      DELETE LT_CONSTANTS WHERE ZL_OBJECT_ID NE IM_OBJECT_ID.
*    ENDIF.
*    IF IM_FIELD_NAME IS SUPPLIED.
*      DELETE LT_CONSTANTS WHERE ZL_FIELD_NAME NE IM_FIELD_NAME.
*    ENDIF.
*    IF IM_CONSTANT IS SUPPLIED.
*      DELETE LT_CONSTANTS WHERE ZL_CONSTANT NE IM_CONSTANT.
*    ENDIF.
*
*    TRY.
*
*        DATA(LS_CONSTANTS) = LT_CONSTANTS[ 1 ].
*
**    describe table lt_constants lines data(lv_line).
**        IF ls_constants-ztype EQ 'S'.
*        IF LS_CONSTANTS-ZL_TYPE EQ SY-ABCDE+17(1). "'R'.
*          ASSIGN LDO_DATA->* TO <FS_RANGE>.
*
**----------------------------------------------------------------------*
** If more than one records exists for the field, then the field entry
** is a select option so the values are populated in range
**---------------- ------------------------------------------------------*
*          LT_RANGE = VALUE #( FOR <LFS_S_CONSTANTS> IN LT_CONSTANTS
*                             ( SIGN = <LFS_S_CONSTANTS>-ZL_SIGN
*                               OPTION = <LFS_S_CONSTANTS>-ZL_OPTION
*                               LOW = <LFS_S_CONSTANTS>-ZL_LOW
*                               HIGH = <LFS_S_CONSTANTS>-ZL_HIGH )
*                               ).
**
*          <FS_RANGE>[] = LT_RANGE[].
*
*          LOOP AT <FS_RANGE> ASSIGNING FIELD-SYMBOL(<LFS_RANGE>).
*
*            TRY .
*                DATA(LS_RANGE) = LT_RANGE[ SY-TABIX ].
*
*                ASSIGN COMPONENT 'LOW' OF STRUCTURE <LFS_RANGE> TO FIELD-SYMBOL(<LFV_VALUE>).
*                IF SY-SUBRC EQ 0.
*                  <LFV_VALUE> = LS_RANGE-LOW.
*                ENDIF.
*
*                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <LFS_RANGE> TO <LFV_VALUE>.
*                IF SY-SUBRC EQ 0.
*                  <LFV_VALUE> = LS_RANGE-HIGH.
*                ENDIF.
*
*              CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*                CLEAR: LS_RANGE.
*            ENDTRY.
*
*          ENDLOOP.
*
*          EP_VALUE = <FS_RANGE>[].
*
*        ELSE.
**----------------------------------------------------------------------*
** If one records exists for the field, then the field entry
** is a parameter and its value is passed to parameter field
**---------------- ------------------------------------------------------*
*          LS_CONSTANTS = LT_CONSTANTS[ 1 ].
*
*          EP_VALUE = LS_CONSTANTS-ZL_LOW.
*
*        ENDIF.
*
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*    ENDTRY.

  ENDMETHOD.


  METHOD EXCEL_CSV_DOWNLOAD.
    DATA:
      lr_data      TYPE REF TO data,
      lo_tool_xls  TYPE REF TO cl_salv_export_tool_xls,
      lo_tool_csv  TYPE REF TO cl_salv_export_tool_csv,
      lo_config    TYPE REF TO if_salv_export_configuration,
      lv_content   TYPE cl_salv_export_tool=>y_file_content,
      lo_exception TYPE REF TO  cx_salv_export_error,
      lo_table     TYPE REF TO cl_abap_tabledescr,
      lo_struct    TYPE REF TO cl_abap_structdescr,
*      lt_comp      TYPE abap_component_tab,
      lt_comp      TYPE TABLE OF abap_simple_componentdescr,
      lv_key       TYPE textpoolky,
      lv_count     TYPE numc5,
      lv_length    TYPE i,
      lt_stream    TYPE salv_xml_xline_tabtype,
      lv_ext       TYPE char5,
      lr_ext       TYPE RANGE OF string.

    CONSTANTS:
      lc_ftype TYPE char10 VALUE 'BIN',
      lc_csv   TYPE char3  VALUE 'CSV',
      lc_xls   TYPE char3  VALUE 'XLS',
      lc_xlsx  TYPE char4  VALUE 'XLSX',
      lc_z     TYPE char1  VALUE 'Z'.

    FIELD-SYMBOLS:
      <lfs_table> TYPE any,
      <lfs_data>  TYPE ANY TABLE.

***** NOTE
*/ Column names for Excel to be passed in IT_TEXTPOOL in order of fields in DataTable IT_TABLE
* Keys within IT_TEXTPOOL to be Z01,Z02 and so on..
*/

    "Validate Extension
    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = CONV localfile( iv_fullpath )
        uppercase = abap_true
      IMPORTING
        extension = lv_ext.

    "Allowed Extensions
    lr_ext = VALUE #( sign = gc_i option = gc_eq
                      ( low = lc_csv )
                      ( low = lc_xls )
                      ( low = lc_xlsx )
                     ).

    "Check for allowed extensions
    IF lr_ext IS NOT INITIAL AND lv_ext NOT IN lr_ext.
      RAISE invalid_extension.
    ENDIF.

    "Create an instance of the export tool for table a given table
    GET REFERENCE OF it_table INTO lr_data.

    "CSV Requested?
    IF lv_ext = lc_csv.
      "Instance of CSV
      lo_tool_csv = cl_salv_export_tool=>create_for_csv( lr_data ).
    ELSE.
      "Instance of XLS
      lo_tool_xls = cl_salv_export_tool=>create_for_excel( lr_data ).
    ENDIF.

    "Configure export properties
    lo_config = COND #( WHEN lv_ext = lc_csv THEN lo_tool_csv->configuration( )
                        ELSE lo_tool_xls->configuration( ) ).

    "Read Table type
    ASSIGN lr_data->* TO <lfs_table>.
    lo_table ?= cl_abap_typedescr=>describe_by_data( <lfs_table> ).
    "Get Structure type
    lo_struct ?= lo_table->get_table_line_type( ).
    "Get components of Structure
    lt_comp = lo_struct->get_included_view( ).

    "Build Excel Columns
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<lfs_comp>).
      "Increment Counter
      lv_count = lv_count + 1.
      "Dynamic Key
      lv_key = |{ lc_z }{ lv_count }|.

      lo_config->add_column( header_text  = CONV #( |{ VALUE #( it_textpool[ key = lv_key ]-entry "#EC CI_STDSEQ
                             DEFAULT cl_abap_char_utilities=>horizontal_tab ) }| )
                             field_name   = |{ <lfs_comp>-name }|
                             display_type = if_salv_export_column_conf=>display_types-text_view ).
    ENDLOOP.

    "Create the Excel Document
    TRY.
        "CSV Requested?
        IF lv_ext = lc_csv.
          lo_tool_csv->read_result( IMPORTING content = lv_content ).
        ELSE.
          lo_tool_xls->read_result( IMPORTING content = lv_content ).
        ENDIF.
      CATCH cx_salv_export_error INTO lo_exception.
        MESSAGE ID lo_exception->if_t100_message~t100key-msgid TYPE sy-abcde+4(1)
        NUMBER lo_exception->if_t100_message~t100key-msgno .
    ENDTRY.

    "Convert to Binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_content
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_stream.

    "Download
    cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize = lv_length
      filetype     = lc_ftype
      filename     = iv_fullpath
    CHANGING
      data_tab     = lt_stream
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24 ).
    IF sy-subrc <> 0.
##NO_TEXT      MESSAGE 'Error In File Download' TYPE 'S' DISPLAY LIKE gc_e.
    ELSE.
##NO_TEXT      MESSAGE 'File Download on given Path' TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD FILE_NAME_VALIDATE.
    DATA: lv_physcl_fpath TYPE path-pathextern,
          lv_filename     TYPE string,
          lv_filepath     TYPE string,
          lv_pattern      TYPE string,
          lv_string       TYPE string.

    CONSTANTS: lc_sep TYPE char1  VALUE '_',
               lc_cap TYPE char1  VALUE '^',
               lc_sub TYPE string VALUE '_+([0-9]\d{7})_+([0-9]\d{12})+.xml$'.

    "Get AL11 FilePath
    IF iv_lgcl_fname IS NOT INITIAL.
      CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
        EXPORTING
          logical_filename = iv_lgcl_fname  "#EC CI_VALPAR
        IMPORTING
          file_name        = lv_physcl_fpath
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.
      IF sy-subrc NE 0.
        RAISE no_filepath.
      ENDIF.
    ENDIF.

    "Spilt AL11 path into FileName and FilePath
    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = lv_physcl_fpath
      IMPORTING
        stripped_name = lv_filename
        file_path     = lv_filepath
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      CLEAR: lv_filename,lv_filepath.
    ENDIF.

    "Split at Seperators in File Name
    SPLIT lv_filename AT lc_sep INTO TABLE DATA(lt_tab).

    "Building Dynamic Pattern
    lv_pattern = |{ lc_cap }{ lv_filepath }{ VALUE #( lt_tab[ 1 ] OPTIONAL ) }{ lc_sub }|.


    lv_string =  iv_appl_fname.
    CONDENSE lv_string.

    "Pattern Matching Using Regex
    zcl_services_utility=>regex_matcher(
    EXPORTING
      iv_pattern     = lv_pattern   " Regex Pattern
      iv_ignore_case = abap_true    "Ignore Case
      iv_input       = lv_string    "Input to be Matched
    RECEIVING
      rv_success  =  rv_success    " Match Result ( 'X' - Success )
    EXCEPTIONS
      regex_error = 1              " Error Generating Regex
      match_error = 2              " Error Matching
      OTHERS      = 3
      ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD GET_APPLLICATION_MESSG_LOGS.

    DATA: ls_logn TYPE bal_s_logn,
          lr_logn TYPE bal_r_logn,
          ls_lfil TYPE bal_s_lfil,
          lt_head TYPE balhdr_t,
          lt_logh TYPE bal_t_logh,
          lt_msgh TYPE bal_t_msgh,
          ls_msgh LIKE LINE OF lt_msgh,
          lv_text TYPE char70.

    ls_logn-sign = gc_i.
    ls_logn-option = gc_eq.
    ls_logn-low = iv_aplog.
    APPEND ls_logn TO lr_logn.

    ls_lfil-lognumber[] = lr_logn[].

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter     = ls_lfil
      IMPORTING
        e_t_log_header     = lt_head
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = lt_head
      IMPORTING
        e_t_log_handle     = lt_logh
        e_t_msg_handle     = lt_msgh
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_msgh INTO ls_msgh.

      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = ls_msgh
        IMPORTING
          e_txt_msg      = lv_text
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.

    rv_aplog_messg = lv_text.
  ENDMETHOD.


  METHOD GET_F4_HELP_APPL_SERVER_FILE.
    DATA: lv_file_name TYPE dxfields-longpath.

    "Get Server Name
    DATA(lv_server_name) = cl_abap_syst=>get_instance_name( ).

    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = 'A'
        i_server        = lv_server_name
        i_path          = iv_source_path
        filemask        = '*.*'
        fileoperation   = 'R'
      IMPORTING
        o_path          = lv_file_name
      EXCEPTIONS
        rfc_error       = 1
        error_with_gui  = 2
        OTHERS          = 3.
    IF sy-subrc EQ 0.
      rv_file_name =  lv_file_name.
    ENDIF.
  ENDMETHOD.


  METHOD MASS_READ_TEXT.
    TYPES:
      BEGIN OF ty_stxl,
        relid    TYPE stxl-relid,
        tdobject TYPE stxl-tdobject,
        tdname   TYPE stxl-tdname,
        tdid     TYPE stxl-tdid,
        tdspras  TYPE stxl-tdspras,
        srtf2    TYPE stxl-srtf2,
        clustr   TYPE stxl-clustr,
        clustd   TYPE stxl-clustd,
      END OF ty_stxl,

      BEGIN OF ty_stxl_raw,
        clustr TYPE stxl-clustr,
        clustd TYPE stxl-clustd,
      END OF ty_stxl_raw.

    DATA:
      t_stxl        TYPE STANDARD TABLE OF ty_stxl,
      t_stxl_buffer TYPE STANDARD TABLE OF ty_stxl,
      t_stxl_raw    TYPE STANDARD TABLE OF ty_stxl_raw,
      w_stxl_raw    TYPE ty_stxl_raw,
      t_tline       TYPE tt_tlines,
      t_export      TYPE t_export,
      t_stxh        TYPE STANDARD TABLE OF stxh,
      w_stxh        TYPE stxh,
      s_stxl        TYPE ty_stxl,
      l_first_tabix TYPE sy-tabix,
      l_last_tabix  TYPE sy-tabix,
      subrc         TYPE sy-subrc,
      process       TYPE abap_bool,
      cursor        TYPE cursor.

    FIELD-SYMBOLS:
      <lfs_stxl>  TYPE ty_stxl,
      <lfs_tline> TYPE tline.

    OPEN CURSOR cursor FOR
       SELECT l~relid l~tdobject l~tdname l~tdid l~tdspras l~srtf2 l~clustr l~clustd
       FROM
         stxl  AS l JOIN
         stxh  AS h
               ON l~tdobject = h~tdobject AND
                  l~tdname = h~tdname AND
                  l~tdid = h~tdid AND
                  l~tdspras = h~tdspras
       WHERE l~relid = 'TX' ##no_text
         AND h~tdobject IN iv_objects
         AND h~tdname   IN iv_names
         AND h~tdid     IN iv_ids
         AND h~tdspras  IN iv_languages
       ORDER BY
         l~relid
         l~tdobject
         l~tdname
         l~tdid
         l~tdspras
         l~srtf2.

    DO.
      FETCH NEXT CURSOR cursor  "#EC CI_SEL_NESTED
              APPENDING TABLE t_stxl
              PACKAGE SIZE iv_package_size.
      subrc = sy-subrc.
      IF subrc = 4.
        IF lines( t_stxl ) > 0.
          process = abap_true.
        ELSE.
          process = abap_false.
        ENDIF.
      ELSEIF subrc = 0.
        IF lines( t_stxl ) < iv_package_size.
          process = abap_true.
        ELSE.
          " put lines of last key aside, as there may be other lines for the same key
          DESCRIBE TABLE t_stxl LINES l_last_tabix.
          READ TABLE t_stxl INDEX l_last_tabix INTO s_stxl.
          READ TABLE t_stxl INDEX 1 ASSIGNING <lfs_stxl>.
          IF <lfs_stxl>-relid           = s_stxl-relid
                AND <lfs_stxl>-tdobject = s_stxl-tdobject
                AND <lfs_stxl>-tdname   = s_stxl-tdname
                AND <lfs_stxl>-tdid     = s_stxl-tdid
                AND <lfs_stxl>-tdspras  = s_stxl-tdspras.
            " The whole package has same key -> load next lines
            process = abap_false.
          ELSE.
            process = abap_true.
            l_first_tabix = l_last_tabix.
            l_first_tabix = l_last_tabix.
            DO.
              SUBTRACT 1 FROM l_first_tabix.
              READ TABLE t_stxl INDEX l_first_tabix ASSIGNING <lfs_stxl>.
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              IF NOT ( <lfs_stxl>-relid    = s_stxl-relid
                   AND <lfs_stxl>-tdobject = s_stxl-tdobject
                   AND <lfs_stxl>-tdname   = s_stxl-tdname
                   AND <lfs_stxl>-tdid     = s_stxl-tdid
                   AND <lfs_stxl>-tdspras  = s_stxl-tdspras ).
                EXIT.
              ENDIF.
            ENDDO.

            ADD 1 TO l_first_tabix.
            APPEND LINES OF t_stxl FROM l_first_tabix TO l_last_tabix TO t_stxl_buffer.
            DELETE t_stxl FROM l_first_tabix TO l_last_tabix.
          ENDIF.
        ENDIF.
      ELSE.
        " can't happen
        ASSERT 0 = 1.
      ENDIF.

      IF process = abap_true.

        LOOP AT t_stxl ASSIGNING <lfs_stxl>.
          AT NEW tdspras.
            CLEAR t_stxl_raw.
          ENDAT.
          " uncompress text
          CLEAR w_stxl_raw.
          w_stxl_raw-clustr = <lfs_stxl>-clustr.
          w_stxl_raw-clustd = <lfs_stxl>-clustd.
          APPEND w_stxl_raw TO t_stxl_raw.

          AT END OF tdspras.
            IMPORT tline = t_tline FROM INTERNAL TABLE t_stxl_raw.
            zcl_services_utility=>one_text(
              EXPORTING
                iv_object   = <lfs_stxl>-tdobject
                iv_id       = <lfs_stxl>-tdid
                iv_name     = <lfs_stxl>-tdname
                iv_language = <lfs_stxl>-tdspras
                it_lines    = t_tline
            CHANGING
                ct_export = t_export
                cs_export  = t_export ).
            CLEAR t_stxl_raw.
          ENDAT.
        ENDLOOP.
      ENDIF.
      t_stxl = t_stxl_buffer.
      CLEAR t_stxl_buffer.
      IF subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.
    IF t_export[] IS NOT INITIAL.
      et_lines[] = t_export[].
      IF iv_rbmom = abap_true.
        FREE MEMORY ID 'ZREAD_TEXT'.
        EXPORT t_export FROM t_export TO MEMORY ID 'ZREAD_TEXT'.
      ENDIF.
    ENDIF.
    CLOSE CURSOR cursor.
  ENDMETHOD.


  METHOD ONE_TEXT.
    DATA: wa_export LIKE LINE OF it_export.
    CLEAR: wa_export.
    wa_export-objects = iv_object.
    wa_export-names = iv_name.
    wa_export-ids = iv_id.
    wa_export-languages = iv_language.

    LOOP AT it_lines ASSIGNING FIELD-SYMBOL(<lfs_tline>).
      CONCATENATE  wa_export-line <lfs_tline>-tdline INTO wa_export-line SEPARATED BY space.
      CONDENSE wa_export-line.
*      WRITE: / <lfs_tline>-TDFORMAT, <lfs_tline>-TDLINE.
    ENDLOOP.
    APPEND wa_export TO it_export.
    CLEAR: wa_export.
  ENDMETHOD.


  METHOD REGEX_MATCHER.
    TRY.
        "Creating Regex Expression
##REGEX_POSIX        DATA(regex) = NEW cl_abap_regex( pattern  = iv_pattern
                                         ignore_case = iv_ignore_case ).
      CATCH cx_sy_regex INTO DATA(lo_regex_excep).
        DATA(lv_text) = lo_regex_excep->get_text( ).
*        MESSAGE E000 WITH LV_TEXT RAISING REGEX_ERROR.
    ENDTRY.

    TRY.
        "Matching String with Pattern
        DATA(matcher) = regex->create_matcher( text = iv_input ).

        "Get Validate Results - 'X' - Success
        DATA(match) = matcher->match( ).

        "Return Match Results
        rv_success = match.

      CATCH cx_sy_matcher INTO DATA(lo_matcher_excep).
        DATA(lv_text1) = lo_matcher_excep->get_text( ).
*        MESSAGE E000 WITH LV_TEXT1 RAISING MATCH_ERROR.
    ENDTRY.
  ENDMETHOD.


  METHOD SAP_WEB_REPOSITORY_SMW0.
    CONSTANTS:
      lc_mi    TYPE wwwdata-relid VALUE 'MI',
      lc_r3tr  TYPE tadir-pgmid VALUE 'R3TR',
      lc_w3mi  TYPE tadir-object VALUE 'W3MI',
      lc_regex TYPE char1 VALUE '*'
      .

    DATA:
      ls_wwwdata_tab TYPE wwwdatatab,
      lv_filefilter  TYPE string,
      lv_filename    TYPE string,
      lv_path        TYPE string,
      lv_fullpath    TYPE string,
      lv_user_action TYPE i,
      lv_exte_regex  TYPE char11
      .
    lv_exte_regex = |{ lc_regex }{ iv_extension }|.

    "Make Sure template exist in SMW0 as binary repository object
    SELECT * FROM wwwdata INNER JOIN tadir  "#EC CI_NOORDER
      ON wwwdata~objid = tadir~obj_name
      INTO CORRESPONDING FIELDS OF ls_wwwdata_tab
      UP TO 1 ROWS
      BYPASSING BUFFER
      WHERE wwwdata~srtf2 = 0
      AND wwwdata~relid = lc_mi
      AND tadir~pgmid = lc_r3tr
      AND tadir~object = lc_w3mi
      AND tadir~obj_name = iv_template_name.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR ls_wwwdata_tab.
      RETURN.
    ENDIF.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        file_filter               = lv_filefilter
      CHANGING
        filename                  = lv_filename             " File Name to Save
        path                      = lv_path                 " Path to File
        fullpath                  = lv_fullpath             " Path + File Name
        user_action               = lv_user_action          " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
      EXCEPTIONS
        cntl_error                = 1                " Control error
        error_no_gui              = 2                " No GUI available
        not_supported_by_gui      = 3                " GUI does not support this
        invalid_default_file_name = 4                " Invalid default file name
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF NOT lv_fullpath CP lv_exte_regex.
      lv_fullpath = |{ lv_fullpath }{ iv_extension }|.
    ENDIF.

    "Download the Template
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = ls_wwwdata_tab
        destination = CONV rlgrap-filename( lv_fullpath ).
    .
  ENDMETHOD.


  METHOD SAVE_FILE_APPL.
    DATA: lv_physcl_fpath TYPE path-pathextern,
          lt_tab_messages TYPE bapiret2_t,
          ls_str_message  TYPE bapiret2,
          lv_timestamp    TYPE timestampl,
          lv_str          TYPE string,
          lv_param1       TYPE string.

    CONSTANTS: lc_object TYPE balobj_d VALUE 'ZCRX',
               lc_subobj TYPE balsubobj VALUE 'ZCRX_SAVE',
               lc_xmlext TYPE char4     VALUE '.xml'.


    "get AL11 Filename
    IF iv_lgcl_fname IS NOT INITIAL AND iv_flag = abap_true.
      CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
        EXPORTING
          logical_filename = iv_lgcl_fname  "#EC CI_VALPAR
        IMPORTING
          file_name        = lv_physcl_fpath
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.
      IF sy-subrc NE 0.
        RAISE no_filepath.
      ENDIF.
    ELSE.

      lv_physcl_fpath = iv_appl_fname.                   "#EC CI_VALPAR

      IF iv_longtimestamp = abap_true.
        GET TIME STAMP FIELD lv_timestamp.
        lv_str = lv_timestamp.
        lv_param1 = lv_str+8.
        REPLACE ALL OCCURRENCES OF '.' IN lv_param1 WITH space.
        CONDENSE lv_param1.
        SPLIT lv_physcl_fpath AT lc_xmlext INTO DATA(lv_file) DATA(lv_temp).
        lv_physcl_fpath = |{ lv_file }_{ lv_param1 }{ lc_xmlext }|.
      ENDIF.
    ENDIF.

    IF lv_physcl_fpath IS INITIAL.
      RAISE no_filepath.
    ELSE.
      "Handling Potential Risk - Security Checks for ABAP (CVA)
      "Potential directory traversal
      zcl_services_utility=>file_name_validate( EXPORTING iv_lgcl_fname = iv_lgcl_fname
                                                          iv_appl_fname = lv_physcl_fpath
                                                RECEIVING rv_success = DATA(lv_valid)
                                                EXCEPTIONS no_filepath = 1 ).
      IF sy-subrc = 1.
        RAISE no_filepath.
      ENDIF.
      IF lv_valid = abap_true OR iv_flag = abap_true.

        "Potential directory traversal Handled with custom handler above- Can be ignored
        OPEN DATASET lv_physcl_fpath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc NE 0.
          RAISE error_appl.
        ELSE.
          LOOP AT it_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
            TRANSFER <lfs_data> TO lv_physcl_fpath.
          ENDLOOP.
          TRY .
              CLOSE DATASET lv_physcl_fpath.
            CATCH  cx_sy_file_close INTO DATA(lv_file_close).
              DATA(lv_text) = lv_file_close->get_text( ).
              MESSAGE lv_text TYPE sy-abcde+4(1)."WITH lv_text INTO DATA(lv_messg).
              RAISE close_error.
          ENDTRY.
        ENDIF.

      ELSE.
        RAISE validation_failed.
      ENDIF.

    ENDIF.

    IF lv_text IS NOT INITIAL.
* -Building messages to Display in the log
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
        IMPORTING
          return = ls_str_message.
      APPEND ls_str_message TO lt_tab_messages.
      CLEAR ls_str_message.

      IF lt_tab_messages IS NOT INITIAL.
* -Call application message logger
        NEW zcl_services_utility( )->application_messg_logger(  iv_object = lc_object
                                                                iv_subobj = lc_subobj
                                                                it_messages = lt_tab_messages ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD SEND_NOTIFICATION.

    CONSTANTS: lc_codepage TYPE abap_encoding VALUE '4110'.
    DATA: lo_send_request   TYPE REF TO cl_bcs,
          lo_document       TYPE REF TO cl_document_bcs,
          lo_sender_uname   TYPE REF TO cl_sapuser_bcs,
          lo_sender         TYPE REF TO cl_cam_address_bcs,
          lo_recipient      TYPE REF TO if_recipient_bcs,
          lo_exception_info TYPE REF TO if_os_exception_info,
          lo_bcs_exception  TYPE REF TO cx_bcs,
          lt_hex            TYPE solix_tab,
          lv_html           TYPE string,
          lv_size           TYPE so_obj_len.

    DATA: lt_tabdescr    TYPE abap_keydescr_tab,
          lo_table_descr TYPE REF TO cl_abap_tabledescr,
          lo_struc_descr TYPE REF TO cl_abap_structdescr,
          lt_components  TYPE abap_component_tab.

    DATA: lt_html   TYPE TABLE OF w3html,
          lt_fields TYPE TABLE OF w3fields.

    FIELD-SYMBOLS: <lfs_table>  TYPE STANDARD TABLE.

    TRY .
*     Create persistent send request
        lo_send_request = cl_bcs=>create_persistent( ).

        lv_html = '<html>'.
        IF iv_content IS SUPPLIED.
          lv_html = iv_content && '<br />' && '<br />'.
        ENDIF.

        IF iv_table IS BOUND.
          ASSIGN iv_table->* TO <lfs_table>.
*       Include internal table in html content
##FM_OLDED          CALL FUNCTION 'WWW_ITAB_TO_HTML'
            EXPORTING
              all_fields = 'X'
            TABLES
              html       = lt_html
              fields     = lt_fields
              itable     = <lfs_table>.
          LOOP AT lt_html ASSIGNING FIELD-SYMBOL(<lfs_html>).
            lv_html = lv_html && <lfs_html>-line.
          ENDLOOP.
        ENDIF.

        lv_html = lv_html && '</html>'.
*     Convert String to Solix
        cl_bcs_convert=>string_to_solix( EXPORTING iv_string = lv_html
                                                   iv_codepage = lc_codepage
                                                   iv_add_bom = abap_true
                                         IMPORTING et_solix  = lt_hex
                                                   ev_size   = lv_size ).

        lo_document = cl_document_bcs=>create_document( i_type = 'HTM' i_hex = lt_hex i_subject = iv_subject ).

        IF iv_attachment IS SUPPLIED.
          lo_document->add_attachment( i_attachment_type    = iv_att_type
                                       i_attachment_subject = iv_att_subject
                                       i_att_content_hex    = iv_attachment ).
        ENDIF.


*     Add document to send request
        lo_send_request->set_document( lo_document ).

*     Set sender and receiver
        IF iv_sender IS NOT INITIAL.
          lo_sender = cl_cam_address_bcs=>create_internet_address( i_address_string = iv_sender
                                                                   i_address_name   = iv_sender ).
          lo_send_request->set_sender( lo_sender ).
        ELSE.
          lo_sender_uname = cl_sapuser_bcs=>create( sy-uname ).
          lo_send_request->set_sender( lo_sender_uname ).
        ENDIF.

        lo_recipient = cl_cam_address_bcs=>create_internet_address( iv_recipient ).
        lo_send_request->add_recipient( lo_recipient ).

*     Send document
        lo_send_request->send( ).

        IF iv_commit = abap_true.
          COMMIT WORK.
        ENDIF.
      CATCH cx_bcs INTO lo_bcs_exception.
        rv_error_msg  = lo_bcs_exception->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD SUBMIT_ANY_REPORT.

    DATA lt_selection_table     TYPE STANDARD TABLE OF rsparams.

    FIELD-SYMBOLS <lfs_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lfs_line> TYPE STANDARD TABLE.

    FREE MEMORY ID : 'M_H_DATA', 'M_L_DATA'.

*{  Get All Selection Screen Parameter in a table    }*
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = iv_programm
      TABLES
        selection_table = lt_selection_table
      EXCEPTIONS
        not_found       = 1
        no_report       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*{  Pass value from IPT_SELECTION table to LT_SELECTION_TABLE   }*
    LOOP AT lt_selection_table ASSIGNING FIELD-SYMBOL(<lfs_sel>).
      IF line_exists( it_selection[ fieldnm = <lfs_sel>-selname ] )."#EC CI_STDSEQ
        <lfs_sel>-sign    = it_selection[ fieldnm = <lfs_sel>-selname ]-sign .  "#EC CI_STDSEQ
        <lfs_sel>-option  = it_selection[ fieldnm = <lfs_sel>-selname ]-option ."#EC CI_STDSEQ
        <lfs_sel>-low     = it_selection[ fieldnm = <lfs_sel>-selname ]-low .   "#EC CI_STDSEQ
        <lfs_sel>-high    = it_selection[ fieldnm = <lfs_sel>-selname ]-high .  "#EC CI_STDSEQ
      ENDIF.
    ENDLOOP.

*{  Clear ALl Value }*
    cl_salv_bs_runtime_info=>clear_all( ).

*{  Set What is required  }*
    cl_salv_bs_runtime_info=>set(
      display   = iv_display
      metadata  = iv_metadata
      data      = iv_data
      structure      = is_h_structure      " Table Name
      structure_line = is_l_structure ). " Table Name

*{  Submit any program with all mandatory selection screen Parameter   }*
    SUBMIT (iv_programm)"#EC CI_SUBMIT
        EXPORTING LIST TO MEMORY
        WITH SELECTION-TABLE lt_selection_table
        AND RETURN.

*{  Get Header Data   }*
    IF iv_hdata_flag = abap_true.
      TRY.
          cl_salv_bs_runtime_info=>get_data_ref(
            IMPORTING
               r_data            = et_data_h
          ).
        CATCH cx_salv_bs_sc_runtime_info INTO DATA(lv_handler).
          MESSAGE lv_handler->get_text( ) TYPE sy-abcde+4(1).
      ENDTRY.

      IF et_data_h IS BOUND.
        ASSIGN et_data_h->* TO <lfs_data>.
*{      Export Data To memory ID  }*
        EXPORT <lfs_data> FROM <lfs_data> TO MEMORY ID 'M_H_DATA'.
      ENDIF.
    ENDIF.

*{  Get Line Data   }*
    IF iv_ldata_flag = abap_true.
      TRY.
          cl_salv_bs_runtime_info=>get_data_ref(
            IMPORTING
               r_data_line       = et_data_l
          ).
        CATCH cx_salv_bs_sc_runtime_info INTO DATA(lv_handler1).
          MESSAGE lv_handler1->get_text( ) TYPE sy-abcde+4(1).
      ENDTRY.

      IF et_data_l IS BOUND.
        ASSIGN et_data_l->* TO <lfs_line>.
*{      Export Data To memory ID  }*
        EXPORT <lfs_line> FROM <lfs_line> TO MEMORY ID 'M_L_DATA'.
      ENDIF.
    ENDIF.
    CLEAR lt_selection_table.

*{  Clear ALl Value }*
    cl_salv_bs_runtime_info=>clear_all( ).

  ENDMETHOD.


  METHOD TVARVC_CHECK_VALUE.
    CLEAR rv_result.

    DATA(lps_cache) = tvarvc_get_ref( iv_name ).
    IF lps_cache->lt_data[] IS NOT INITIAL AND iv_value IN lps_cache->lt_data.
      rv_result = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD TVARVC_GET_RANGE.
    FREE rt_range.

    DATA(lps_cache) = tvarvc_get_ref( iv_name ).

    MOVE-CORRESPONDING lps_cache->lt_data  TO rt_range.
  ENDMETHOD.


  METHOD TVARVC_GET_REF.
    READ TABLE gt_cache REFERENCE INTO rv_cache WITH KEY
   name = iv_name.
    CHECK sy-subrc NE 0.

    INSERT VALUE #(
      name = iv_name
    ) INTO TABLE gt_cache REFERENCE INTO rv_cache.

    SELECT sign
           opti AS option
           low
           high
           FROM tvarvc
           INTO CORRESPONDING FIELDS OF TABLE rv_cache->lt_data
           WHERE name = iv_name.
    IF sy-subrc = 0.
      LOOP AT rv_cache->lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
        IF <lfs_data>-sign IS INITIAL.
          <lfs_data>-sign = 'I'.
        ENDIF.
        IF <lfs_data>-option IS INITIAL.
          <lfs_data>-option = 'EQ'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD TVARVC_GET_VALUE.
    rv_value = iv_default.

    DATA(lps_cache) = tvarvc_get_ref( iv_name ).

    READ TABLE lps_cache->lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>) INDEX 1.
    IF sy-subrc = 0.
      rv_value = <lfs_data>-low.
    ENDIF.
  ENDMETHOD.


  METHOD XML_FORMAT_SAVE_FILE_APPL.
    DATA: lv_physcl_fpath TYPE path-pathextern,
          lv_xml          TYPE string,
          lv_timestamp    TYPE timestampl,
          lv_str          TYPE string,
          lv_param1       TYPE string,
          lv_xml_path     TYPE path-pathintern.

    CONSTANTS: lc_xmlstart  TYPE string  VALUE '<?xml version="1.0"?>',
               lc_start_tag TYPE string VALUE '?>',
               lc_less      TYPE char1  VALUE '<',
               lc_great     TYPE char1  VALUE '>',
               lc_slash     TYPE char1  VALUE '/',
               lc_xmlext    TYPE char4  VALUE '.xml',
               lc_i         TYPE char1  VALUE 'I',
               lc_eq        TYPE char2  VALUE 'EQ',
               lc_dot       TYPE char1  VALUE '.'.


    lv_xml_path  =  iv_lgcl_fname.

    "Format XML data
    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
      "Add Header MATERIALSET tag
      IF sy-tabix = 1.
        FIND FIRST OCCURRENCE OF lc_start_tag IN <lfs_data> RESULTS DATA(ls_header_tag).
        DATA(lv_char) = ls_header_tag-offset + ls_header_tag-length.
        DATA(lv_len) = strlen( <lfs_data> ) - lv_char.

        <lfs_data> = |{ <lfs_data>+0(lv_char) }{ lc_less }{ iv_xml_headtag }{ lc_great }{ <lfs_data>+lv_char(lv_len) }|.
      ELSE.
        "Remove Duplicate xml version tags.
        REPLACE ALL OCCURRENCES OF lc_xmlstart IN <lfs_data> WITH space.
      ENDIF.
    ENDLOOP.

    "Close HeaderSet tag
    lv_xml = |{ lc_less }{ lc_slash }{ iv_xml_headtag }{ lc_great }|.
    APPEND lv_xml  TO ct_data .
    CLEAR lv_xml.


    "Get the Physical file path
    CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
      EXPORTING
        logical_filename = lv_xml_path
      IMPORTING
        file_name        = lv_physcl_fpath
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      RAISE file_not_found.
    ENDIF.

    "Modify the File Path with ms from TimStamp
    IF iv_modify_fpath = abap_true AND
       lv_physcl_fpath IS NOT INITIAL.
      GET TIME STAMP FIELD lv_timestamp.
      lv_str = lv_timestamp.
      lv_param1 = lv_str+8.
      REPLACE ALL OCCURRENCES OF lc_dot IN lv_param1 WITH space.
      CONDENSE lv_param1.
      SPLIT lv_physcl_fpath AT lc_xmlext INTO DATA(lv_file) DATA(lv_temp).
      lv_physcl_fpath = |{ lv_file }_{ lv_param1 }{ lc_xmlext }|.
    ENDIF.

    "SAVE File in AL11
    IF ct_data[] IS NOT INITIAL.
      CALL METHOD zcl_services_utility=>save_file_appl
        EXPORTING
          iv_lgcl_fname     = lv_xml_path
          iv_appl_fname     = lv_physcl_fpath
          it_data           = ct_data[]
        EXCEPTIONS
          no_filepath       = 1
          error_appl        = 2
          close_error       = 3
          validation_failed = 4
          OTHERS            = 5.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
