*&---------------------------------------------------------------------*
*&  Include           ZPZZZR_TR_LIST_0080
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZPZZZR_TR_LIST_0080                                        *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0080  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0080 OUTPUT.

  SET PF-STATUS 'BOUTONS'.


ENDMODULE.                 " STATUS_0080  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_0080  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_0080 OUTPUT.


  DATA: l_first_line(132) TYPE c,
        l_date(10) TYPE c,
        l_time(8) TYPE c.

  CLEAR : sy-ucomm, ok_code.
  REFRESH w_text_tab.

  IF container_text_f9 IS INITIAL.
    CREATE OBJECT :
    container_text_f9  EXPORTING container_name = 'EDITOR'.

    CREATE OBJECT text_f9
      EXPORTING
        parent        = container_text_f9
        wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = c_line_length_f9
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    CALL METHOD text_f9->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

    CALL METHOD text_f9->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

  ENDIF.

*--> init fields header
  w_header-tdid     = 'ZCOM'.
  w_header-tdname   = t_result-trkorr.
  w_header-tdspras  = sy-langu.
  w_header-tdobject = 'ZTR'.

*
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = w_header-tdid
      language                = w_header-tdspras
      name                    = w_header-tdname
      object                  = w_header-tdobject
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
    IMPORTING
      header                  = w_header
    TABLES
      lines                   = wt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
*
  IF sy-subrc = 0.
    t_result-zcom = wt_lines-tdline.
    LOOP AT wt_lines.
      APPEND wt_lines-tdline TO w_text_tab.
    ENDLOOP.
    w_text_tab_sov = w_text_tab.
  ENDIF.

  WRITE sy-uzeit TO l_time.
  WRITE sy-datum TO l_date.
  CONCATENATE text-024 sy-uname l_date l_time INTO l_first_line
              SEPARATED BY space.

  APPEND l_first_line TO w_text_tab.
*
*--> to load the CUSTOM CODE of the screen with the text
  CALL METHOD text_f9->set_text_as_r3table
    EXPORTING
      table           = w_text_tab
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.
*

  CALL METHOD text_f9->set_readonly_mode
    EXPORTING
      readonly_mode = 0.


ENDMODULE.                 " init_0080  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0080  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0080 INPUT.

  CASE sy-ucomm.
    WHEN 'ZEXIT' OR 'ZCANCEL' OR 'ZBACK' OR '&F03'.
      LEAVE TO SCREEN 0.

    WHEN '&DATA_SAVE'.

*--> recover text chapter 9 entered by user (CUSTOM CONTROL)
*
      REFRESH : w_text_tab.
      CLEAR   : w_text_tab.
*
      CALL METHOD text_f9->get_text_as_r3table
        IMPORTING
          table           = w_text_tab
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          OTHERS          = 3.
*

      IF w_text_tab NE w_text_tab_sov.

        REFRESH wt_lines.
        CLEAR   wt_lines.
*
        LOOP AT w_text_tab INTO wt_lines-tdline.
          wt_lines-tdformat = '*'.
          APPEND wt_lines.
        ENDLOOP.

*
        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
*   CLIENT                = SY-MANDT
            header              = w_header
            savemode_direct     = 'X'
*   INSERT                = ' '
*   OWNER_SPECIFIED       = ' '
*   LOCAL_CAT             = ' '
* IMPORTING
*   FUNCTION              =
*   NEWHEADER             =
          TABLES
            lines                 = wt_lines
         EXCEPTIONS
           id                    = 1
           language              = 2
           name                  = 3
           object                = 4
           OTHERS                = 5.

      ENDIF.
*
      READ TABLE wt_lines INDEX 1.
      READ TABLE t_result INDEX w_tabindex.
      t_result-zcom = wt_lines-tdline.
      MODIFY t_result INDEX w_tabindex.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0080  INPUT

----------------------------------------------------------------------

