************************************************************************
* Report       : ZPU580MIGFILE
* Title        : NewERP management of SAP files
*
* Description  : Like CG3Y and CG3Z, this report allow to upload or download file
*& between Front-end PC and SAP Application Server
*&
*& In addition, there is a delete fonction for SAP Application Server files
*&
*& Linked with transaction ZCG3
*
* Author        : T.GOURDIN EX011958 AKKA
* Creation Date : 21/03/2016
************************************************************************
*    Updates
************************************************************************
* WHO         * DD/MM/YYYY * Reason
************************************************************************
* T.GOURDIN   * 21/03/2016 * Creation
* AM BRISARD  * 14/11/2016 * Modification (search AMB)
* EX011955    * 31/01/2017 * Multi-file download
************************************************************************

REPORT  zpu580migfile.

INCLUDE zpu580migfile_top.
INCLUDE zpu580migfile_sel.
INCLUDE zpu580migfile_f01.


*&------------------------------------------------------------------------------------------------------------------------------------------*

* Front-end file selection
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.

  DATA: ws_window_title TYPE string,
        wit_file_table  TYPE filetable,
        ws_rc           TYPE i.

  ws_window_title = text-f06.
  FREE wit_file_table.
  CLEAR ws_rc.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = ws_window_title
    CHANGING
      file_table              = wit_file_table
      rc                      = ws_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  READ TABLE wit_file_table INTO p_lfile INDEX 1.




*at selection-screen on value-request for p_spath.
*
*  call function '/SAPDMC/LSM_F4_SERVER_FILE'
*    EXPORTING
*      DIRECTORY              = ''
**     FILEMASK               = ' '
*    IMPORTING
*      SERVERFILE             = p_sfile
**   EXCEPTIONS
**     CANCELED_BY_USER       = 1
**     OTHERS                 = 2
*            .
*  if sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  endif.

*&------------------------------------------------------------------------------------------------------------------------------------------*


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_locrep.
  PERFORM get_locrep.


* Screen refresh after radiobutton update
AT SELECTION-SCREEN OUTPUT.
  IF p_sfile IS INITIAL.
    GET PARAMETER ID '***' FIELD p_sfile.
    IF p_sfile IS INITIAL.
      p_sfile = 'ZZ_U580_MIG_SD'.     " AMB (change pour SD)
    ENDIF.
  ENDIF.


* Récupération du nom de fichier
  DATA: lv_complete_filename TYPE string.
  DATA: lv_filename          TYPE string.
  DATA: lv_filename_noext    TYPE string.
  DATA: lt_filename_part     TYPE TABLE OF string WITH HEADER LINE.
  DATA: lv_filename_lines    TYPE i.

  lv_complete_filename = p_lfile.

  SPLIT lv_complete_filename AT '\' INTO TABLE lt_filename_part.
  DESCRIBE TABLE lt_filename_part LINES lv_filename_lines.
  READ TABLE lt_filename_part INDEX lv_filename_lines.
  IF sy-subrc = 0.
*    p_sfilx = lt_filename_part.       " -AMB
  ENDIF.

* Depending of options, deactivated some fields
  PERFORM f0_change_screen.

* Move cursor on first visible obligatory field
  IF p_sfilx IS INITIAL.
    SET CURSOR FIELD 'P_SFILX'.
  ENDIF.
  IF p_delet IS INITIAL AND p_lfile IS INITIAL.
    SET CURSOR FIELD 'P_LFILE'.
  ENDIF.

  CLEAR p_dsname.
  IF NOT p_sfile IS INITIAL.
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = p_sfile
      IMPORTING
        file_name        = p_dsname
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.

    IF sy-subrc IS INITIAL.
      CONCATENATE '(' p_dsname ')' INTO p_dsname.
    ENDIF.

  ENDIF.

*&------------------------------------------------------------------------------------------------------------------------------------------*

START-OF-SELECTION.

*  CHECK obligatory FIELDS
  IF ( p_lfile IS INITIAL AND ( p_upl = 'X' OR p_downl = 'X' ) ) OR
     ( p_sfilx IS INITIAL AND ( p_upl = 'X' OR p_downl = 'X' OR p_delet = 'X' ) ) OR
     ( p_sfile IS INITIAL AND ( p_flist = 'X' ) ).
    MESSAGE text-e02 TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.

* Get SAP Application filename from logical names
    CLEAR: ws_ftappl.

    IF NOT p_sfile IS INITIAL.
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = p_sfile
        IMPORTING
          file_name        = ws_ftappl
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        CLEAR ws_ftappl.
      ENDIF.
    ENDIF.

    CONCATENATE ws_ftappl p_sfilx INTO ws_ftappl.

* Delete SAP Application server file
    IF p_delet = 'X'.
      PERFORM f_delete_ftappl USING ws_ftappl.
    ENDIF.

* Upload front-end file to SAP Application server in binary mode

    IF p_upl = 'X' AND p_ftype = 'BIN'.
      PERFORM f_upload_bin USING ws_ftappl.
    ENDIF.

* Download SAP Application server file to front-end in binary mode

    IF p_downl = 'X' AND p_ftype = 'BIN'.
      PERFORM f_download_bin USING ws_ftappl.
    ENDIF.

* Upload front-end file to SAP Application server in ASCII mode

    IF p_upl = 'X' AND p_ftype = 'ASC'.
      PERFORM f_upload_asc USING ws_ftappl.
    ENDIF.

* Download SAP Application server file to front-end in ASCII mode

    IF p_downl = 'X' AND p_ftype = 'ASC'.
      PERFORM f_download_asc USING ws_ftappl.
    ENDIF.

* List of file in Directory Unix

    IF p_flist = 'X'.
      PERFORM f_list_dir_files USING ws_ftappl.
    ENDIF.

    IF p_multi = 'X'.
      PERFORM f_download_mult.
    ENDIF.

  ENDIF.

* End of report
*&------------------------------------------------------------------------------------------------------------------------------------------*