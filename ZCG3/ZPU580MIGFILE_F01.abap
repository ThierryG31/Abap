*&---------------------------------------------------------------------*
*&  Include           ZPU580MIGFILE_F01
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_BIN
*&---------------------------------------------------------------------*
*       Upload in binary mode
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_upload_bin  USING    p_ws_ftappl TYPE rcgfiletr-ftappl.

  DATA ws_error_text TYPE string.

  sy-cprog = 'RC1TCG3Z'.                                  "#EC WRITE_OK
  CALL FUNCTION 'C13Z_FILE_UPLOAD_BINARY'
    EXPORTING
      i_file_front_end         = p_lfile
      i_file_appl              = p_ws_ftappl
      i_file_overwrite         = p_replac
*         IMPORTING
*           E_FLG_OPEN_ERROR         =
*           E_OS_MESSAGE             =
   EXCEPTIONS
     fe_file_not_exists       = 1
     fe_file_read_error       = 2
     ap_no_authority          = 3
     ap_file_open_error       = 4
     ap_file_exists           = 5
     ap_convert_error         = 6
     OTHERS                   = 7
            .
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1. ws_error_text = text-e03. " 'Front-end file not exists'.
      WHEN 2. ws_error_text = text-e04. " 'Front-end file read error'.
      WHEN 3. ws_error_text = text-e05. " 'Application Server no authority'.
      WHEN 4. ws_error_text = text-e06. " 'Application Server file open error'.
      WHEN 5. ws_error_text = text-e07. " 'Application Server file exists'.
      WHEN 6. ws_error_text = text-e08. " 'Application Server convert error'.
      WHEN 7. ws_error_text = text-e09. " 'Other error'.
    ENDCASE.
    CONCATENATE text-e10 ws_error_text INTO ws_error_text.
    MESSAGE ws_error_text TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE text-i01 TYPE 'S'.
  ENDIF.
ENDFORM.                    " F_UPLOAD_BIN

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_BIN
*&---------------------------------------------------------------------*
*       Download in binary mode
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_download_bin  USING    p_ws_ftappl TYPE rcgfiletr-ftappl.

  DATA ws_error_text TYPE string.

  sy-cprog = 'RC1TCG3Y'.                                  "#EC WRITE_OK
  CALL FUNCTION 'C13Z_FILE_DOWNLOAD_BINARY'
    EXPORTING
      i_file_front_end          = p_lfile
      i_file_appl               = p_ws_ftappl
      i_file_overwrite          = p_replac
*         IMPORTING
*           E_FLG_OPEN_ERROR          =
*           E_OS_MESSAGE              =
   EXCEPTIONS
     fe_file_open_error        = 1
     fe_file_exists            = 2
     fe_file_write_error       = 3
     ap_no_authority           = 4
     ap_file_open_error        = 5
     ap_file_empty             = 6
     OTHERS                    = 7
            .
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1. ws_error_text = text-e11. " 'Front-end file open error'.
      WHEN 2. ws_error_text = text-e12. " 'Front-end file exists'.
      WHEN 3. ws_error_text = text-e13. " 'Front-end file write error'.
      WHEN 4. ws_error_text = text-e05. " 'Application Server no authority'.
      WHEN 5. ws_error_text = text-e06. " 'Application Server file open error'.
      WHEN 6. ws_error_text = text-e14. " 'Application Server file empty'.
      WHEN 7. ws_error_text = text-e09. " 'Other error'.
    ENDCASE.
    CONCATENATE text-e10 ws_error_text INTO ws_error_text.
    MESSAGE ws_error_text TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE text-i01 TYPE 'S'.
  ENDIF.

ENDFORM.                    " F_DOWNLOAD_BIN

*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_ASC
*&---------------------------------------------------------------------*
*       Upload in ASCII mode
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_upload_asc  USING    p_ws_ftappl TYPE rcgfiletr-ftappl.

  DATA: ws_error_text TYPE string,
        ws_subrc      TYPE sy-subrc,
        ws_lfile      TYPE pathextern.

*  ws_lfile = p_lfile.
*  PERFORM f_check_file USING ws_lfile CHANGING ws_subrc.
*  IF ws_subrc IS INITIAL.
  CALL FUNCTION 'C13Z_FILE_UPLOAD_ASCII'
      EXPORTING
        i_file_front_end         = p_lfile
        i_file_appl              = p_ws_ftappl
        i_file_overwrite         = p_replac
*         IMPORTING
*           E_FLG_OPEN_ERROR         =
*           E_OS_MESSAGE             =
     EXCEPTIONS
       fe_file_not_exists       = 1
       fe_file_read_error       = 2
       ap_no_authority          = 3
       ap_file_open_error       = 4
       ap_file_exists           = 5
       ap_convert_error         = 6
       OTHERS                   = 7
              .
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1. ws_error_text = text-e03. " 'Front-end file_not exists'.
      WHEN 2. ws_error_text = text-e04. " 'Front-end file read error'.
      WHEN 3. ws_error_text = text-e05. " 'Application Server no authority'.
      WHEN 4. ws_error_text = text-e06. " 'Application Server file open error'.
      WHEN 5. ws_error_text = text-e07. " 'Application Server file exists'.
      WHEN 6. ws_error_text = text-e08. " 'Application Server convert error'.
      WHEN 7. ws_error_text = text-e09. " 'Other error'.
    ENDCASE.
    CONCATENATE text-e10 ws_error_text INTO ws_error_text.
    MESSAGE ws_error_text TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE text-i01 TYPE 'S'.
  ENDIF.
*  ELSE.
*    MESSAGE text-e03 TYPE 'S' DISPLAY LIKE 'E'.
*  ENDIF.

ENDFORM.                    " F_UPLOAD_ASC

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_ASC
*&---------------------------------------------------------------------*
*       Download in ASCII mode
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_download_asc  USING    p_ws_ftappl TYPE rcgfiletr-ftappl.

  DATA ws_error_text TYPE string.

  CALL FUNCTION 'C13Z_FILE_DOWNLOAD_ASCII'
     EXPORTING
       i_file_front_end          = p_lfile
       i_file_appl               = p_ws_ftappl
       i_file_overwrite          = p_replac
*         IMPORTING
*           E_FLG_OPEN_ERROR          =
*           E_OS_MESSAGE              =
    EXCEPTIONS
      fe_file_open_error        = 1
      fe_file_exists            = 2
      fe_file_write_error       = 3
      ap_no_authority           = 4
      ap_file_open_error        = 5
      ap_file_empty             = 6
      ap_path_error             = 7
      OTHERS                    = 8
             .
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1. ws_error_text = text-e11. " 'Front-end file open error'.
      WHEN 2. ws_error_text = text-e12. " 'Front-end file exists'.
      WHEN 3. ws_error_text = text-e13. " 'Front-end file write error'.
      WHEN 4. ws_error_text = text-e05. " 'Application Server no authority'.
      WHEN 5. ws_error_text = text-e06. " 'Application Server file open error'.
      WHEN 6. ws_error_text = text-e14. " 'Application Server file empty'.
      WHEN 7. ws_error_text = text-e15. " 'Application Server path error'.
      WHEN 8. ws_error_text = text-e09. " 'Other error'.
    ENDCASE.
    CONCATENATE text-e10 ws_error_text INTO ws_error_text.
    MESSAGE ws_error_text TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE text-i01 TYPE 'S'.
  ENDIF.

ENDFORM.                    " F_DOWNLOAD_ASC

*&---------------------------------------------------------------------*
*&      Form  F_DELETE_FTAPPL
*&---------------------------------------------------------------------*
*       Delete of SAP Application server file
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_delete_ftappl  USING    p_ws_ftappl TYPE rcgfiletr-ftappl.

  DATA: ws_subrc      TYPE sy-subrc,
        ws_answer     TYPE string.

*  check SAP Application server file exists
  PERFORM f_check_file USING p_ws_ftappl CHANGING ws_subrc.
  IF ws_subrc IS INITIAL.

    PERFORM f_confirm_delete USING p_ws_ftappl CHANGING ws_answer.
    IF ws_answer = 'J'.
*    delete
      DELETE DATASET p_ws_ftappl.
      IF sy-subrc NE 0.
        MESSAGE text-d04 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        CLOSE DATASET p_ws_ftappl.
        MESSAGE text-d06 TYPE 'S'.

      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    "f_delete_ftappl

*&---------------------------------------------------------------------*
*&      Form  f_check_file
*&---------------------------------------------------------------------*
*       Check SAP Application server file exists
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_check_file  USING    p_ftappl   TYPE eseftappl
                   CHANGING p_subrc    TYPE sy-subrc.

  CLEAR p_subrc.

  OPEN DATASET p_ftappl FOR INPUT IN BINARY MODE.
  IF sy-subrc NE 0.
    p_subrc = sy-subrc.
  ELSE.
    CLOSE DATASET p_ftappl.
  ENDIF.

ENDFORM.                    " f_check_file

*&---------------------------------------------------------------------*
*&      Form  F_CONFIRM_DELETE
*&---------------------------------------------------------------------*
*       Confirmation for delete of SAP Application Server file
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*      <--P_WS_ERROR_TEXT  text
*----------------------------------------------------------------------*
FORM f_confirm_delete  USING    p_ws_ftappl     TYPE rcgfiletr-ftappl
                       CHANGING p_answer        TYPE string.

  CLEAR p_answer.

  CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
    EXPORTING
      defaultoption = 'N'
      textline1     = text-d02
      textline2     = p_ws_ftappl
      titel         = text-d03
      start_column  = 25
      start_row     = 6
    IMPORTING
      answer        = p_answer
    EXCEPTIONS
      OTHERS        = 1.

  IF sy-subrc <> 0 OR p_answer <> 'J'.
    MESSAGE text-d05 TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.                    " F_CONFIRM_DELETE

*&---------------------------------------------------------------------*
*&      Form  F0_CHANGE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f0_change_screen.

*    IF p_delet = 'X' AND ( screen-name = '' OR screen-name = 'P_FTYPE' OR screen-name = 'P_REPLAC' ).

  LOOP AT SCREEN.

    IF screen-group1 = 'MUL'.
      IF p_multi = 'X'.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
    ENDIF.

    IF p_multi = 'X'
    AND (  screen-name = '%CF04009_1000'
        OR screen-name = '%CF01011_1000'
        OR screen-name = 'P_LFILE'
        OR screen-name = '%CF03028_1000'
        OR screen-name = 'P_SFILX' ).
      screen-active = '0'.
    ENDIF.

    CASE screen-name.

      WHEN 'P_LFILE'.
        screen-input = 1.
        IF p_delet = 'X' OR p_flist = 'X'.
          screen-input = 0.
        ENDIF.

      WHEN 'P_SFILX'.
        screen-input = 1.
        IF p_flist = 'X'.
          screen-input = 0.
        ENDIF.

      WHEN 'P_FTYPE'.
        screen-input = 1.
        IF p_delet = 'X' OR p_flist = 'X'.
          screen-input = 0.
        ENDIF.

      WHEN 'P_REPLAC'.
        screen-input = 1.
        IF p_delet = 'X' OR p_flist = 'X'.
          screen-input = 0.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.                    " F0_CHANGE_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F_LIST_DIR_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WS_FTAPPL  text
*----------------------------------------------------------------------*
FORM f_list_dir_files  USING  p_ws_ftappl TYPE rcgfiletr-ftappl.

  DATA : lt_flist   TYPE TABLE OF eps2fili,
         ls_flist   TYPE eps2fili,
         lv_dirname TYPE eps2filnam.

  lv_dirname = p_ws_ftappl.

  CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
    EXPORTING
      iv_dir_name               = lv_dirname
*     file_mask                 = iv_filter
    TABLES
      dir_list                  = lt_flist
 EXCEPTIONS
   invalid_eps_subdir           = 1
   sapgparam_failed             = 2
   build_directory_failed       = 3
   no_authorization             = 4
   read_directory_failed        = 5
   too_many_read_errors         = 6
   empty_directory_list         = 7
   OTHERS                       = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    WRITE :/ 'Directory List Files'.
    WRITE :/ '--------------------'.
    LOOP AT lt_flist INTO ls_flist.
      WRITE :/ ls_flist-name.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_LIST_DIR_FILES


*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_MULT
*&---------------------------------------------------------------------*
*       Download multiple files on PC
*----------------------------------------------------------------------*
FORM f_download_mult .
  DATA: lv_path  TYPE ty_path.
  DATA: lv_error TYPE xfeld.

  DATA: ls_files TYPE ty_files.
*******

  lv_path = ws_ftappl.

  PERFORM search_files USING    lv_path
                       CHANGING lv_error.
  CHECK lv_error IS INITIAL.


  LOOP AT gt_files INTO ls_files.

    CONCATENATE p_locrep
                '\'
                ls_files
           INTO p_lfile.
    CONCATENATE lv_path
                ls_files
           INTO ws_ftappl.

    CASE p_ftype.
      WHEN 'BIN'.
        PERFORM f_download_bin USING ws_ftappl.
      WHEN 'ASC'.
        PERFORM f_download_asc USING ws_ftappl.
    ENDCASE.

  ENDLOOP.



ENDFORM.                    " F_DOWNLOAD_MULT

*&---------------------------------------------------------------------*
*&      Form  search_files
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM search_files USING    p_app_path TYPE ty_path
                  CHANGING lv_error   TYPE xfeld.

  DATA:
    msg               LIKE gsux-message,
    save_subrc        LIKE sy-subrc,
    table_btcxpg      LIKE btcxpm OCCURS 0 WITH HEADER LINE.


  DATA:        lv_status         TYPE extcmdexex-status,
               lv_exitcode       TYPE extcmdexex-exitcode.
  CONSTANTS :  lc_erreur         TYPE c VALUE 'E'.
  DATA: cmdwnt            TYPE sxpgcolist-name,
        parameters        LIKE sxpgcolist-parameters.

  DATA: ls_files TYPE ty_files .

  RANGES: lr_files FOR p_files.
********


****NEW_ERP fonction SXPG remplace Z_CALL_SYSTEM_UNIX
  REFRESH table_btcxpg.

  parameters = p_app_path.
  cmdwnt = 'ZDIR'.

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname                   = cmdwnt
      additional_parameters         = parameters
      operatingsystem               = sy-opsys
    IMPORTING
      status                        = lv_status
      exitcode                      = lv_exitcode
    TABLES
      exec_protocol                 = table_btcxpg
    EXCEPTIONS
      no_permission                 = 1
      command_not_found             = 2
      parameters_too_long           = 3
      security_risk                 = 4
      wrong_check_call_interface    = 5
      program_start_error           = 6
      program_termination_error     = 7
      x_error                       = 8
      parameter_expected            = 9
      too_many_parameters           = 10
      illegal_command               = 11
      wrong_asynchronous_parameters = 12
      cant_enq_tbtco_entry          = 13
      jobcount_generation_error     = 14
      OTHERS                        = 15.

  MOVE sy-subrc                         TO save_subrc.
  IF lv_status = lc_erreur.
    REFRESH table_btcxpg.
  ENDIF.
  IF  save_subrc                        <> 0
  AND save_subrc                        <> 4.
    MESSAGE a899(zz)                  WITH 'SXPG_COMMAND_EXECUTE'
                                           'error rc:'
                                           save_subrc
                                           msg.
  ENDIF.


  IF sy-subrc = 0.

    IF p_files CS '*'.
      lr_files-option = 'CP'.
    ELSE.
      lr_files-option = 'EQ'.
    ENDIF.
    lr_files-sign = 'I'.
    lr_files-low  = p_files.
    APPEND lr_files.


    LOOP AT table_btcxpg.


      DATA: ls_file_temp_uppercase TYPE ty_files.

      ls_file_temp_uppercase = table_btcxpg-message.
      TRANSLATE ls_file_temp_uppercase TO UPPER CASE.

      IF ls_file_temp_uppercase IN lr_files.
        ls_files = table_btcxpg-message.


        IF ls_files CS '.ctl'.
          INSERT ls_files
            INTO TABLE gt_files_ctl.
        ELSE.
          APPEND ls_files
              TO gt_files.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ELSE.

    lv_error = 'X'.

  ENDIF.  " subrc Z_CALL_SYSTEM_UNIX

ENDFORM.                    " search_files


*&---------------------------------------------------------------------*
*&      Form  GET_LOCREP
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_locrep .

  CALL METHOD cl_gui_frontend_services=>directory_browse
*  EXPORTING
*    window_title         =
*    initial_folder       =
    CHANGING
      selected_folder      = p_locrep
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




ENDFORM.                    " GET_LOCREP