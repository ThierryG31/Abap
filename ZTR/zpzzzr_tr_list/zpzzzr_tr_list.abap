*&--------------------------------------------------------------------*
*& Report  ZPZZZR_TR_LIST
*&
*&--------------------------------------------------------------------*
*&
*&
*&--------------------------------------------------------------------*

REPORT  zpzzzr_tr_list.
*---------------------------------------------------------------------*
* CHANGE HISTORY
*---------------------------------------------------------------------*
*  DATE      | NAME    |  DESCRIPTION                      | Reference
*---------------------------------------------------------------------*
* DD.MON.YYYY| Tx99999 | Xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx   |XYZDDMMMYY
*            |         | xxxxxxxxxxxxxxxxxxxxxxxxxx        |
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*



*<-- ChaRM --
TYPES:
  ty_charm_ch       TYPE char10,
  ty_originator_key TYPE char32,
  ty_trorder_number TYPE char20,
  ty_object_id      TYPE char10,
  ty_zz_extern_cr   TYPE char12.


TYPES:
  BEGIN OF ty_ot,
    originator_key TYPE ty_originator_key,
    trorder_number TYPE ty_trorder_number,
  END OF ty_ot,
  ty_t_ot TYPE TABLE OF ty_ot,

  BEGIN OF ty_cr_ext,
    object_id    TYPE ty_object_id   ,
    zz_extern_cr TYPE ty_zz_extern_cr,
  END OF ty_cr_ext,
  ty_t_cr_ext TYPE TABLE OF ty_cr_ext.


DATA: w_charm_ch TYPE ty_charm_ch,
      w_charm_conex_failed TYPE sy-subrc.

DATA: w_flag_vi2 TYPE i.


CONSTANTS :
  c_charm_rfc_dest TYPE char10 VALUE 'PS1CLNT001',"'PSMCLNT001',

* Field name in target table.
  BEGIN OF c_fielname_trordhc,
    charm_change TYPE char14 VALUE 'ORIGINATOR_KEY',
    order        TYPE char14 VALUE 'TRORDER_NUMBER',
  END OF c_fielname_trordhc,

  BEGIN OF c_fielname_crmd_orderadm_h,
    charm_change TYPE char14 VALUE 'OBJECT_ID',
    extern_cr    TYPE char14 VALUE 'ZZ_EXTERN_CR',
  END OF c_fielname_crmd_orderadm_h.
*-- ChaRM -->

DATA   wt_lines_result         LIKE STANDARD TABLE OF tline
                                    WITH HEADER LINE.
*----------------------------------------------------------------------
*--------------- UPDATE TEXT
TYPES:
  ito0t_text_head_rec LIKE thead,
*  ito0t_text_edit_rec LIKE rssource-line,
  ito0t_text_edit_rec(132),
  ito0t_text_edit_tab TYPE ito0t_text_edit_rec OCCURS 0.

DATA : container_text_f9       TYPE REF TO cl_gui_custom_container,
       text_f9                 TYPE REF TO cl_gui_textedit,
       ok_code                 LIKE sy-ucomm,
       c_line_length_f9        TYPE i VALUE 72,
       w_text_tab              TYPE ito0t_text_edit_tab,
       w_text_tab_sov          TYPE ito0t_text_edit_tab.


* Types
TYPE-POOLS : slis, ctslg.

* Constantes

DATA: l_attribute LIKE e070a-attribute VALUE 'SAP_CTS_PROJECT'.

* Tables

TABLES: e070, e07t, e070a, e070v,tstrfcofil, adrp, e071v, e071.

* Zones de lecture
DATA: ls_e070v LIKE e070v.

* Tables internes

DATA : t_e070a LIKE e070a OCCURS 0 WITH HEADER LINE,
       wa_e070a LIKE e070a.                                 "V2.1

DATA : BEGIN OF w_e070v,
        trkorr      LIKE e070v-trkorr,      " Ordre/Tache C40
        strkorr     LIKE e070v-strkorr,
        trfunction  LIKE e070v-trfunction,                  " Type C1,
        trstatus    LIKE e070v-trstatus,    " Status Released
        as4user     LIKE e070v-as4user,     " propriÃ©taire
        name_text   LIKE adrp-name_text,    " full name
        as4text     LIKE e070v-as4text,     " libellÃ©
        client      LIKE e070v-client,
        envir(20)   TYPE c,                     "Environnement actuel
        info(4)     TYPE c,                 "Additional information

        ie1rc(2)    TYPE c,"       like tstrfcofil-retcode, "RC IE1
        ie1date     LIKE tstrfcofil-trdate,
        ie1time     LIKE tstrfcofil-trtime,
        ve1rc(2)    TYPE c,                     "RC VE1 V2.0
        ve1date     LIKE tstrfcofil-trdate,                 " V2.0
        ve1time     LIKE tstrfcofil-trtime,                 " V2.0
        ve2rc(2)    TYPE c,                     "RC VE1 V2.0
        ve2date     LIKE tstrfcofil-trdate,                 " V2.0
        ve2time     LIKE tstrfcofil-trtime,                 " V2.0
        ve3rc(2)    TYPE c,                     "RC VE1 V2.0
        ve3date     LIKE tstrfcofil-trdate,                 " V2.0
        ve3time     LIKE tstrfcofil-trtime,                 " V2.0
        ve4rc(2)    TYPE c,                     "RC VE1 V2.0
        ve4date     LIKE tstrfcofil-trdate,                 " V2.0
        ve4time     LIKE tstrfcofil-trtime,                 " V2.0
        de2rc(2)    TYPE c,                     "RC VE1 V2.0
        de2date     LIKE tstrfcofil-trdate,                 " V2.0
        de2time     LIKE tstrfcofil-trtime,                 " V2.0
        pe1rc(2)    TYPE c,"       like tstrfcofil-retcode, "RC PE1
        pe1date     LIKE tstrfcofil-trdate,
        pe1time     LIKE tstrfcofil-trtime,


        as4date     LIKE e070v-as4date,
        as4time     LIKE e070v-as4time,

        qc_number   LIKE e070a-reference,        "ChaRM
        charm_ch    TYPE ty_charm_ch,            "ChaRM

        color       TYPE slis_t_specialcol_alv.

DATA : END OF w_e070v.

DATA : t_e070v LIKE w_e070v OCCURS 0 WITH HEADER LINE.
DATA : t_settings TYPE ctslg_settings OCCURS 0 WITH HEADER LINE.

DATA: t_e070v_request LIKE e070v OCCURS 0 WITH HEADER LINE.



DATA : BEGIN OF t_result OCCURS 0,
        trkorr      LIKE e070v-trkorr,      " Ordre/Tache C40
        strkorr     LIKE e070v-strkorr,
        trfunction  LIKE e070v-trfunction,                  " Type C1,
        trstatus    LIKE e070v-trstatus,    " Status Released
        as4user     LIKE e070v-as4user,     " propriÃ©taire
        name_text   LIKE adrp-name_text,    " full name
        as4text     LIKE e070v-as4text,     " libellÃ©
        client      LIKE e070v-client,
        envir(20)    TYPE c,                     "Environnement actuel
        info(4)     TYPE c,                 "Additional information

        ie1rc(2)    TYPE c,"   like tstrfcofil-retcode, "RC IE1
        ie1date     LIKE tstrfcofil-trdate,
        ie1time     LIKE tstrfcofil-trtime,
        ve1rc(2)    TYPE c,                     "RC VE1 V2.0
        ve1date     LIKE tstrfcofil-trdate,                 " V2.0
        ve1time     LIKE tstrfcofil-trtime,                 " V2.0
        ve2rc(2)    TYPE c,                     "RC VE1 V2.0
        ve2date     LIKE tstrfcofil-trdate,                 " V2.0
        ve2time     LIKE tstrfcofil-trtime,                 " V2.0
        ve3rc(2)    TYPE c,                     "RC VE1 V2.0
        ve3date     LIKE tstrfcofil-trdate,                 " V2.0
        ve3time     LIKE tstrfcofil-trtime,                 " V2.0
        ve4rc(2)    TYPE c,                     "RC VE1 V2.0
        ve4date     LIKE tstrfcofil-trdate,                 " V2.0
        ve4time     LIKE tstrfcofil-trtime,                 " V2.0
        de2rc(2)    TYPE c,                     "RC VE1 V2.0
        de2date     LIKE tstrfcofil-trdate,                 " V2.0
        de2time     LIKE tstrfcofil-trtime,                 " V2.0
        pe1rc(2)    TYPE c,"       like tstrfcofil-retcode, "RC PE1
        pe1date     LIKE tstrfcofil-trdate,
        pe1time     LIKE tstrfcofil-trtime,
        as4date     LIKE e070v-as4date,
        as4time     LIKE e070v-as4time,
        sp_number   LIKE e070a-reference,
        qc_number   LIKE e070a-reference,
        cts_project LIKE e070a-reference,

         zcom(100)  TYPE c, "Comment
*        text1(100)    TYPE c,
*        text2(100)    TYPE c,
*        text3(100)    TYPE c,
        charm_ch   TYPE ty_charm_ch,            "ChaRM
       color       TYPE slis_t_specialcol_alv.

DATA : END OF t_result.

* Paramétres pour l'édition ALV

DATA : str_is_variant TYPE disvariant,
       tab_fieldcat   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       it_comment     TYPE slis_t_listheader WITH HEADER LINE,
       gt_events      TYPE slis_t_event WITH HEADER LINE,
       t_color        TYPE slis_t_specialcol_alv WITH HEADER LINE,
       is_layout      TYPE slis_layout_alv.

* Varialbles

DATA: t_infos TYPE ctslg_cofile.
DATA: t_system TYPE ctslg_system.
DATA: t_step TYPE ctslg_step.
DATA: t_action TYPE ctslg_action.


DATA: w_and(3) TYPE c.
DATA: w_or(3) TYPE c.
DATA: w_tab(72) OCCURS 10 WITH HEADER LINE.
DATA: zdatelow(10) TYPE c.
DATA: zdatehigh(10) TYPE c.
DATA  w_grid_settings TYPE  lvc_s_glay.
DATA: w_projet(60) TYPE c.

* Datas for functions READ_TEXT and SAVE_TEXT
DATA: w_header          LIKE thead.
DATA: wt_lines          LIKE STANDARD TABLE OF tline
                                 WITH HEADER LINE.

DATA:  w_tabindex LIKE sy-index.
* Types d'objet
DATA: gt_object_texts      TYPE ko100 OCCURS 0.

* Transport requests for objects

DATA: BEGIN OF lt_order_object OCCURS 0,
       trkorr TYPE trkorr.
DATA: END OF lt_order_object.

RANGES: r_typ FOR e070-trfunction.

RANGES: r_strkorr FOR e070-strkorr.

*Order list selected without take into account ad option (TR linked).
RANGES: r_order_noaddopt FOR e070-trkorr.

DATA: zn1 TYPE i.

DATA: select_line TYPE flag.
DATA  w_strkorr LIKE e070-strkorr.
DATA: it_selections TYPE  trwbo_selections WITH HEADER LINE.

DATA fieldname TYPE fieldname.
FIELD-SYMBOLS <fieldsymbol> TYPE ANY.
DATA  w_trfunction LIKE e070v-trfunction.


* field-symbols
FIELD-SYMBOLS : <fs_e070v> LIKE w_e070v. "Charm

* Begin of V2.1
CONSTANTS : c_att_cts   TYPE trattr VALUE 'SAP_CTS_PROJECT',
            c_att_issd   TYPE trattr VALUE 'ISSD',
            c_att_qc   TYPE trattr VALUE 'QC',
            c_att_no_qc   TYPE trattr VALUE 'NO_QC'.
* End of V2.1

***********************************************************************
*         SELECTION SCREEN
***********************************************************************
**********************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
**********************************************
* Order
SELECT-OPTIONS r_order FOR e070-trkorr NO INTERVALS.

* Creation Date
SELECT-OPTIONS r_date FOR e070-as4date.

* Client
SELECT-OPTIONS r_client FOR e070v-client NO INTERVALS.

* Owner
SELECT-OPTIONS r_owner FOR e070v-as4user NO INTERVALS.
*PARAMETER p_owner LIKE e070v-as4user.

SELECT-OPTIONS r_cts FOR e070v-as4user NO INTERVALS
              MATCHCODE OBJECT cts_changeable_projects.
* End of V2.0

* Begin of V2.1
* SP Number
SELECT-OPTIONS r_sp_num FOR e070a-reference NO INTERVALS.
* QC number
SELECT-OPTIONS r_qc_num FOR e070a-reference NO INTERVALS.
* En of V2.1

* ChaRM change
SELECT-OPTIONS s_charm FOR w_charm_ch NO INTERVALS.     "ChaRM

* Project
SELECT-OPTIONS r_projet FOR w_projet NO INTERVALS.



* Object search

SELECTION-SCREEN BEGIN OF BLOCK b01.
*===================================
SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS checka LIKE sctsobject-checkbox  USER-COMMAND setcursor.
SELECTION-SCREEN COMMENT 1(30) text-021 FOR FIELD pgmida.
SELECTION-SCREEN POSITION 33.
PARAMETERS pgmida   LIKE sctsobject-pgmid
 MODIF ID out.
PARAMETERS objecta  LIKE sctsobject-object
 MODIF ID obj.
SELECTION-SCREEN POSITION 43.
PARAMETERS objtexta LIKE sctsobject-text VISIBLE LENGTH 19 LOWER CASE
                    MODIF ID out.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 33.

PARAMETERS objnamea(120) TYPE c MODIF ID obj.
PARAMETERS only_ca  LIKE sctsobject-only_compl NO-DISPLAY DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b01.

*<-- Begin V2.2 --
SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE text-029.
*============================================================
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER p_lktr AS CHECKBOX.  "Select linked transport req.
SELECTION-SCREEN COMMENT (10) FOR FIELD p_lktr.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS s_lktrob FOR e071-object NO INTERVALS.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b11.
*--  End   V2.2 -->

* Type
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
*============================================================

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-001 FOR FIELD p_workb.
SELECTION-SCREEN POSITION 17.
PARAMETER p_workb AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN COMMENT 27(15) text-012 FOR FIELD p_copy.
SELECTION-SCREEN POSITION 43.
PARAMETER p_copy AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN COMMENT 53(15) text-025 FOR FIELD p_repai.
SELECTION-SCREEN POSITION 69.
PARAMETER p_repai AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-002 FOR FIELD p_custo.
SELECTION-SCREEN POSITION 17.
PARAMETER p_custo AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN COMMENT 27(15) text-023 FOR FIELD p_dev .
SELECTION-SCREEN POSITION 43.
PARAMETER p_dev AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-026 FOR FIELD p_buff.
SELECTION-SCREEN POSITION 17.
PARAMETER p_buff AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN COMMENT 27(15) text-027 FOR FIELD p_wt_log.
SELECTION-SCREEN POSITION 43.
PARAMETER p_wt_log AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b2.


* Environnement actuel

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
*============================================================

SELECTION-SCREEN BEGIN OF LINE.
*-----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_pe1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-005 FOR FIELD p_pe1.
SELECTION-SCREEN END OF LINE.

* Begin of V2.0
SELECTION-SCREEN BEGIN OF LINE.
*-----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_ve1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-028 FOR FIELD p_ve1.
SELECTION-SCREEN END OF LINE.
* End of V2.0

* Begin of V2.0
SELECTION-SCREEN BEGIN OF LINE.
*-----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_ve2 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-030 FOR FIELD p_ve2.
SELECTION-SCREEN END OF LINE.
* End of V2.0

* Begin of V2.0
SELECTION-SCREEN BEGIN OF LINE.
*-----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_ve3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-031 FOR FIELD p_ve3.
SELECTION-SCREEN END OF LINE.
* End of V2.0

SELECTION-SCREEN BEGIN OF LINE.
*-----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_ve4 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-032 FOR FIELD p_ve4.
SELECTION-SCREEN END OF LINE.
* End of V2.0

SELECTION-SCREEN BEGIN OF LINE.
*-----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_de2 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-033 FOR FIELD p_de2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
*----------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_ie1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-006 FOR FIELD p_ie1.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF LINE.
*------------------------------
SELECTION-SCREEN POSITION 32.
PARAMETER p_de1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(3) text-007 FOR FIELD p_de1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.


* Dates de transport

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-008.
*============================================================

SELECT-OPTIONS p_pe1dat FOR tstrfcofil-trdate.
SELECT-OPTIONS p_ve1dat FOR tstrfcofil-trdate.
SELECT-OPTIONS p_ve2dat FOR tstrfcofil-trdate.
SELECT-OPTIONS p_ve3dat FOR tstrfcofil-trdate.
SELECT-OPTIONS p_ve4dat FOR tstrfcofil-trdate.
SELECT-OPTIONS p_ie1dat FOR tstrfcofil-trdate.
SELECT-OPTIONS p_de2dat FOR tstrfcofil-trdate.
SELECTION-SCREEN END OF BLOCK b4.
*
** Max display
*
*PARAMETER p_max LIKE rseumod-tbmaxsel DEFAULT 9999.
*
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zpzzzr_tr_list_0080. "Include for sreen 80 (Text)

**********************************************************************
*        INITIALIZATION                                              *
**********************************************************************
INITIALIZATION.

  PERFORM init_select_options.

  PERFORM read_object_table     TABLES gt_object_texts.



AT SELECTION-SCREEN OUTPUT.
*  PERFORM select_request_type           USING ' '.
  PERFORM at_selection_screen_output.


AT SELECTION-SCREEN ON objecta.
  PERFORM at_selection_screen_on_field    USING 'OBJECTA'.

AT SELECTION-SCREEN.



**********************************************************************
*           MAIN PROCEDURE                                           *
**********************************************************************
START-OF-SELECTION.

*--------------- Préparation du select en fonction des paramétres
  PERFORM f100_prepare_selection.

*--------------- Selection par les objects modifiés (tâches)
  PERFORM f104_select_for_object.

*-------------- Selection par les tâches de Repair
  PERFORM f106_select_for_repair.

*-------------- Selection par les SP number      "V2.1
  PERFORM f260_sp_number.

*-------------- Selection par les CR number      "V2.1
  PERFORM f270_cr_number.


  PERFORM get_tr_from_charm_change."ChaRM

*<-- Begin V2.2 --
  PERFORM f290_add_option.
*--  End   V2.2 -->


*-------------- Selection des ordres en table t_e070v
  PERFORM f111_select_transport_req.



  CLEAR t_e070v.

*--------------- Initialisation ALV
  PERFORM f200_init_alv.


* Begin of V2.0
*--------------- Filtre sur les CTS projets
  PERFORM f250_cts_project.

* End of V2.0


*----------------------------
* Selection des codes retours et dates associées / déduction de
* l'environnement en cours
*----------------------------
  PERFORM f300_select_additional_data.

*  break to42718.


*-------------------- User name selection
  PERFORM f350_select_user.

*--------------- Affichage de l'ALV
  PERFORM f400_display_result.




**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
*                                                                    *
*                     INCLUDES                                       *
*                                                                    *
**********************************************************************
**********************************************************************
*  INCLUDE zpzzzr_tr_list_stat001.
*
*  INCLUDE zpzzzi_tr_list_01.


**********************************************************************
**********************************************************************
*                                                                    *
*                     FORMS                                          *
*                                                                    *
**********************************************************************
**********************************************************************


*&-------------------------------------------------------------------*
*&      Form  f100_prepare_selection
*&-------------------------------------------------------------------*
*       Preparation du select et du header en fonction des parametres
*--------------------------------------------------------------------*

FORM f100_prepare_selection.

*--------- construction de it_comment
  w_and = ''.

  REFRESH it_comment.

* Titre

  CLEAR it_comment.
  it_comment-typ = 'H'.
  it_comment-info = 'Transport Requests Overview'.
  APPEND it_comment.

**********************************************************************
* Type d'ordre ( K = Workbench -           W = Custo -
*                T = Transport of copies - S = dev/correction)
*                R = Tâche Repair
**********************************************************************

  CLEAR r_typ.
  REFRESH r_typ.

  CLEAR r_strkorr.
  REFRESH r_strkorr.

  MOVE 'I' TO r_typ-sign.
  MOVE 'EQ' TO r_typ-option.


  IF p_repai = 'X'.
    MOVE 'R' TO r_typ-low.
    APPEND r_typ.
  ENDIF.


  IF p_workb = 'X'.
    MOVE 'K' TO r_typ-low.
    APPEND r_typ.

    IF p_dev = 'X' OR
       NOT objecta IS INITIAL.

      MOVE 'S' TO r_typ-low.
      APPEND r_typ.

      MOVE 'R' TO r_typ-low.
      APPEND r_typ.

    ELSEIF r_strkorr[] IS INITIAL.
      MOVE 'I' TO r_strkorr-sign.
      MOVE 'EQ' TO r_strkorr-option.
      MOVE ' ' TO r_strkorr-low.
      APPEND r_strkorr.
    ENDIF.

  ENDIF.

  IF p_custo = 'X'.
    MOVE 'W' TO r_typ-low.
    APPEND r_typ.

    IF p_dev = 'X' OR
       NOT objecta IS INITIAL.
      MOVE 'Q' TO r_typ-low.
      APPEND r_typ.
    ELSEIF r_strkorr[] IS INITIAL.
      MOVE 'I' TO r_strkorr-sign.
      MOVE 'EQ' TO r_strkorr-option.
      MOVE ' ' TO r_strkorr-low.
      APPEND r_strkorr.
    ENDIF.

  ENDIF.

  IF p_copy = 'X'.
    MOVE 'T' TO r_typ-low.
    APPEND r_typ.
  ENDIF.

***********************************************************************
** Projet / DEsignation dce l'ordre
* Recherche sur le texte
***********************************************************************
  IF NOT r_projet[] IS INITIAL.
    LOOP AT r_projet.
      IF r_projet-high IS INITIAL AND r_projet-option <> 'CP'.
        r_projet-option = 'CP'.
        CONCATENATE '*' r_projet-low '*' INTO r_projet-low.
        MODIFY r_projet.

      ENDIF.
    ENDLOOP.
  ENDIF.

***********************************************************************
* Environnement en cours
***********************************************************************
  w_or = ''.
  CLEAR it_comment.
  it_comment-typ = 'S'.
  it_comment-key = 'Environment : ' .

  IF p_pe1 = 'X'.
    it_comment-info = 'PE1'.
    w_or = ' or'.
  ENDIF.

  IF p_ve1 = 'X'.
    CONCATENATE it_comment-info w_or ' VE1' INTO it_comment-info.
    w_or = ' or'.
  ENDIF.

  IF p_de2 = 'X'.
    CONCATENATE it_comment-info w_or ' DE2' INTO it_comment-info.
    w_or = ' or'.
  ENDIF.

  IF p_ve2 = 'X'.
    CONCATENATE it_comment-info w_or ' VE2' INTO it_comment-info.
    w_or = ' or'.
  ENDIF.

  IF p_ve3 = 'X'.
    CONCATENATE it_comment-info w_or ' ve3' INTO it_comment-info.
    w_or = ' or'.
  ENDIF.

  IF p_ve4 = 'X'.
    CONCATENATE it_comment-info w_or ' ve4' INTO it_comment-info.
    w_or = ' or'.
  ENDIF.

  IF p_ie1 = 'X'.
    CONCATENATE it_comment-info w_or ' IE1' INTO it_comment-info.
    w_or = ' or'.
  ENDIF.

  IF p_de1 = 'X'.
    CONCATENATE it_comment-info w_or ' DE1' INTO it_comment-info.
  ENDIF.

  IF p_repai = 'X'.
    CONCATENATE it_comment-info ' (Repairs Only)' INTO it_comment-info.
  ENDIF.



  APPEND it_comment.



***********************************************************************
* Date de transport en IE1
***********************************************************************
  IF  ( NOT p_ie1dat-low IS INITIAL ) AND ( p_ie1dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'IE1 Transport Date : ' .

    CONCATENATE p_ie1dat-low+6(2) '.'
                p_ie1dat-low+4(2) '.'
                p_ie1dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_ie1dat-high IS INITIAL ) AND ( p_ie1dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'IE1 Transport Date : ' .

    CONCATENATE p_ie1dat-high+6(2) '.'
                p_ie1dat-high+4(2) '.'
                p_ie1dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_ie1dat-high IS INITIAL )
  AND ( NOT p_ie1dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'IE1 Transport Date : ' .

    CONCATENATE p_ie1dat-low+6(2) '.'
                p_ie1dat-low+4(2) '.'
                p_ie1dat-low(4) INTO zdatelow.

    CONCATENATE p_ie1dat-high+6(2) '.'
                p_ie1dat-high+4(2) '.'
                p_ie1dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

***********************************************************************
* Date de transport en VE1
***********************************************************************
  IF  ( NOT p_ve1dat-low IS INITIAL ) AND ( p_ve1dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'VE1 Transport Date : ' .

    CONCATENATE p_ve1dat-low+6(2) '.'
                p_ve1dat-low+4(2) '.'
                p_ve1dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_ve1dat-high IS INITIAL ) AND ( p_ve1dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'VE1 Transport Date : ' .

    CONCATENATE p_ve1dat-high+6(2) '.'
                p_ve1dat-high+4(2) '.'
                p_ve1dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.


  IF  ( NOT p_ve1dat-high IS INITIAL ) AND
       ( NOT p_ve1dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'VE1 Transport Date : ' .

    CONCATENATE p_ve1dat-low+6(2) '.'
                p_ve1dat-low+4(2) '.'
                p_ve1dat-low(4) INTO zdatelow.

    CONCATENATE p_ve1dat-high+6(2) '.'
                p_ve1dat-high+4(2) '.'
                p_ve1dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.
* End of V2.0

***********************************************************************
* Date de transport en VE2
***********************************************************************
  IF  ( NOT p_ve2dat-low IS INITIAL ) AND ( p_ve2dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'VE2 Transport Date : ' .

    CONCATENATE p_ve2dat-low+6(2) '.'
                p_ve2dat-low+4(2) '.'
                p_ve2dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_ve2dat-high IS INITIAL ) AND ( p_ve2dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'VE2 Transport Date : ' .

    CONCATENATE p_ve2dat-high+6(2) '.'
                p_ve2dat-high+4(2) '.'
                p_ve2dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.


  IF  ( NOT p_ve2dat-high IS INITIAL ) AND
       ( NOT p_ve2dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'VE2 Transport Date : ' .

    CONCATENATE p_ve2dat-low+6(2) '.'
                p_ve2dat-low+4(2) '.'
                p_ve2dat-low(4) INTO zdatelow.

    CONCATENATE p_ve2dat-high+6(2) '.'
                p_ve2dat-high+4(2) '.'
                p_ve2dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.
* End of V2.0

***********************************************************************
* Date de transport en ve3
***********************************************************************
  IF  ( NOT p_ve3dat-low IS INITIAL ) AND ( p_ve3dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 've3 Transport Date : ' .

    CONCATENATE p_ve3dat-low+6(2) '.'
                p_ve3dat-low+4(2) '.'
                p_ve3dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_ve3dat-high IS INITIAL ) AND ( p_ve3dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 've3 Transport Date : ' .

    CONCATENATE p_ve3dat-high+6(2) '.'
                p_ve3dat-high+4(2) '.'
                p_ve3dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.


  IF  ( NOT p_ve3dat-high IS INITIAL ) AND
       ( NOT p_ve3dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 've3 Transport Date : ' .

    CONCATENATE p_ve3dat-low+6(2) '.'
                p_ve3dat-low+4(2) '.'
                p_ve3dat-low(4) INTO zdatelow.

    CONCATENATE p_ve3dat-high+6(2) '.'
                p_ve3dat-high+4(2) '.'
                p_ve3dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.
* End of V2.0

***********************************************************************
* Date de transport en ve4
***********************************************************************
  IF  ( NOT p_ve4dat-low IS INITIAL ) AND ( p_ve4dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 've4 Transport Date : ' .

    CONCATENATE p_ve4dat-low+6(2) '.'
                p_ve4dat-low+4(2) '.'
                p_ve4dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_ve4dat-high IS INITIAL ) AND ( p_ve4dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 've4 Transport Date : ' .

    CONCATENATE p_ve4dat-high+6(2) '.'
                p_ve4dat-high+4(2) '.'
                p_ve4dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.


  IF  ( NOT p_ve4dat-high IS INITIAL ) AND
       ( NOT p_ve4dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 've4 Transport Date : ' .

    CONCATENATE p_ve4dat-low+6(2) '.'
                p_ve4dat-low+4(2) '.'
                p_ve4dat-low(4) INTO zdatelow.

    CONCATENATE p_ve4dat-high+6(2) '.'
                p_ve4dat-high+4(2) '.'
                p_ve4dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.
* End of V2.0

***********************************************************************
* Date de transport en DE2
***********************************************************************
  IF  ( NOT p_de2dat-low IS INITIAL ) AND ( p_de2dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'DE2 Transport Date : ' .

    CONCATENATE p_de2dat-low+6(2) '.'
                p_de2dat-low+4(2) '.'
                p_de2dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_de2dat-high IS INITIAL ) AND ( p_de2dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'DE2 Transport Date : ' .

    CONCATENATE p_de2dat-high+6(2) '.'
                p_de2dat-high+4(2) '.'
                p_de2dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.


  IF  ( NOT p_de2dat-high IS INITIAL ) AND
       ( NOT p_de2dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'DE2 Transport Date : ' .

    CONCATENATE p_de2dat-low+6(2) '.'
                p_de2dat-low+4(2) '.'
                p_de2dat-low(4) INTO zdatelow.

    CONCATENATE p_de2dat-high+6(2) '.'
                p_de2dat-high+4(2) '.'
                p_de2dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.
* End of V2.0

***********************************************************************
* Date de transport en PE1
***********************************************************************
  IF  ( NOT p_pe1dat-low IS INITIAL ) AND ( p_pe1dat-high IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'PE1 Transport Date : ' .

    CONCATENATE p_pe1dat-low+6(2) '.'
                p_pe1dat-low+4(2) '.'
                p_pe1dat-low(4) INTO zdatelow.

    CONCATENATE 'On' zdatelow INTO it_comment-info SEPARATED BY space.
    APPEND it_comment.

  ENDIF.

  IF  ( NOT p_pe1dat-high IS INITIAL ) AND ( p_pe1dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'PE1 Transport Date : ' .

    CONCATENATE p_pe1dat-high+6(2) '.'
                p_pe1dat-high+4(2) '.'
                p_pe1dat-high(4) INTO zdatehigh.

    CONCATENATE 'Before' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.


  IF  ( NOT p_pe1dat-high IS INITIAL ) AND
       ( NOT p_pe1dat-low IS INITIAL ).

    CLEAR it_comment.
    it_comment-typ = 'S'.
    it_comment-key = 'PE1 Transport Date : ' .

    CONCATENATE p_pe1dat-low+6(2) '.'
                p_pe1dat-low+4(2) '.'
                p_pe1dat-low(4) INTO zdatelow.

    CONCATENATE p_pe1dat-high+6(2) '.'
                p_pe1dat-high+4(2) '.'
                p_pe1dat-high(4) INTO zdatehigh.

    CONCATENATE 'Between' zdatelow 'and' zdatehigh INTO it_comment-info
                                      SEPARATED BY space.
    APPEND it_comment.

  ENDIF.



ENDFORM.                    "f100_prepare_selection
*---------------------------------------------------------------------*
*       FORM f104_select_for_object                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM f104_select_for_object.

* Selection for object
  IF ( NOT pgmida IS INITIAL AND
       NOT objecta  IS INITIAL AND
       NOT objnamea IS INITIAL )   OR
     (     objecta  = '*' AND
       NOT objnamea IS INITIAL ).
    REPLACE '*' WITH '%' INTO pgmida.
    REPLACE '*' WITH '%' INTO objecta.
    REPLACE '*' WITH '%' INTO objnamea.


    SELECT trkorr
          INTO CORRESPONDING FIELDS OF TABLE lt_order_object
              FROM e071
             WHERE pgmid      LIKE  pgmida
               AND object     LIKE  objecta
               AND obj_name   LIKE  objnamea.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i899(mg) WITH text-022 objnamea.
      EXIT.
    ELSE.
      IF r_order[] IS INITIAL.
        LOOP AT lt_order_object.

          SELECT SINGLE trkorr trfunction
          INTO (r_order-low, w_trfunction)
           FROM e070v
           WHERE trkorr = lt_order_object-trkorr.

          IF p_repai = 'X' AND w_trfunction <> 'R'.
            CONTINUE.
          ENDIF.

          IF sy-subrc IS INITIAL.
*            IF r_order-low IS INITIAL.

            SELECT SINGLE strkorr INTO r_order-low
              FROM e070v
              WHERE trkorr = lt_order_object-trkorr.

*            ENDIF.

            r_order-sign = 'I'.
            r_order-option = 'EQ'.
            APPEND r_order.

          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "f104_select_for_object
*---------------------------------------------------------------------*
*       FORM f106_select_for_repair                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM f106_select_for_repair.

  IF p_repai = 'X' AND  objecta IS INITIAL.

    IF r_order[] IS INITIAL.

      SELECT *
*      UP TO p_max ROWS
      FROM e070v
              WHERE as4text IN r_projet
              AND client IN r_client
              AND as4user IN r_owner
              AND as4date IN r_date
              AND trfunction = 'R'.

        r_order-sign = 'I'.
        r_order-option = 'EQ'.
        r_order-low = e070v-strkorr.
        APPEND r_order.

*        r_order-sign = 'I'.
*        r_order-option = 'EQ'.
*        r_order-low = e070v-trkorr.
*        APPEND r_order.


      ENDSELECT.

    ENDIF.

  ENDIF.

ENDFORM.                    "f106_select_for_repair
*---------------------------------------------------------------------*
*       FORM f111_select_for_description                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM f111_select_transport_req.

  DATA : w_cr_sp_flag TYPE flag.

  RANGES : lr_order FOR e070-trkorr.                        "+ST34735
  DATA : lw_range_low TYPE i VALUE 1,                       "+ST34735
         lw_range_high TYPE i VALUE 1,                      "+ST34735
         lw_leave_loop TYPE i.                              "+ST34735

* Selection du projet si renseignÃ©

  IF NOT r_order[] IS INITIAL OR NOT r_projet[] IS INITIAL.

    DO.
      lw_range_low = lw_range_high.
      lw_range_high = lw_range_low + 1000.
      DESCRIBE TABLE r_order LINES lw_leave_loop.
      IF lw_leave_loop < lw_range_low.  "Leave the DO when the r_order
        " is completly processed
        EXIT.
      ENDIF.
      REFRESH lr_order.
      LOOP AT r_order.
        IF sy-tabix => lw_range_low AND sy-tabix < lw_range_high.
          lr_order = r_order.
          APPEND lr_order.
        ENDIF.
      ENDLOOP.

*  * Select orders
      SELECT *
*      UP TO p_max ROWS
      FROM e070v
          WHERE
          trkorr IN lr_order
          AND client IN r_client
          AND as4user IN r_owner
          AND as4date IN r_date
          AND trfunction IN r_typ
          AND strkorr IN r_strkorr.

        MOVE e070v-as4text TO w_projet.
        TRANSLATE w_projet TO UPPER CASE.

        IF NOT r_qc_num[] IS INITIAL.                       "V2.1
          w_cr_sp_flag = 'X'.
        ENDIF.
        IF NOT r_sp_num[] IS INITIAL.                       "V2.1
          w_cr_sp_flag = 'X'.
        ENDIF.

        IF NOT w_cr_sp_flag IS INITIAL.
          "V2.1
*   Filtre supplémentaire
          IF NOT p_de1 IS INITIAL AND
                 p_ie1 IS INITIAL  AND
                 p_ve1 IS INITIAL AND                       " V2.0
                 p_ve2 IS INITIAL AND                       " V2.0
                 p_ve3 IS INITIAL AND                     " V2.0
                 p_ve4 IS INITIAL AND                     " V2.0
                 p_de2 IS INITIAL AND                       " V2.0
                 p_pe1 IS INITIAL AND
                 e070v-trstatus = 'R'.
            CONTINUE.
          ENDIF.


          MOVE-CORRESPONDING e070v TO t_e070v.
          APPEND t_e070v.

          IF p_dev = 'X'.
            SELECT * FROM e070v
            WHERE strkorr = t_e070v-trkorr.

              MOVE-CORRESPONDING e070v TO t_e070v.
              APPEND t_e070v.
            ENDSELECT.
          ENDIF.

        ELSE.
          IF w_projet IN r_projet.
*   Filtre supplémentaire
            IF NOT p_de1 IS INITIAL AND
                   p_ie1 IS INITIAL  AND
                   p_ve1 IS INITIAL AND                     " V2.0
                   p_ve2 IS INITIAL AND                     " V2.0
                   p_ve3 IS INITIAL AND                     " V2.0
                   p_ve4 IS INITIAL AND                     " V2.0
                   p_de2 IS INITIAL AND                     " V2.0
                   p_pe1 IS INITIAL AND
                   e070v-trstatus = 'R'.
              CONTINUE.
            ENDIF.


            MOVE-CORRESPONDING e070v TO t_e070v.
            APPEND t_e070v.

            IF p_dev = 'X'.
              SELECT * FROM e070v
              WHERE strkorr = t_e070v-trkorr.

                MOVE-CORRESPONDING e070v TO t_e070v.
                APPEND t_e070v.
              ENDSELECT.

            ENDIF.
          ENDIF.
        ENDIF.

      ENDSELECT.
    ENDDO.

    SORT t_e070v BY trkorr.

* Selection de l'object


  ELSEIF objecta IS INITIAL.

* Selection des valeurs de base

    SELECT *
*    UP TO p_max ROWS
    FROM e070v
            WHERE
            ( trkorr LIKE 'de1%'
            AND client IN r_client
            AND as4user IN r_owner
            AND as4date IN r_date
            AND trfunction IN r_typ
            AND strkorr IN r_strkorr )



            ORDER BY trkorr DESCENDING.


* Filtre supplémentaire
      IF NOT p_de1 IS INITIAL AND
             p_ie1 IS INITIAL  AND
             p_ve1 IS INITIAL AND                           " V2.0
             p_ve2 IS INITIAL AND                           " V2.0
             p_ve3 IS INITIAL AND                           " V2.0
             p_ve4 IS INITIAL AND                           " V2.0
             p_de2 IS INITIAL AND                           " V2.0
             p_pe1 IS INITIAL AND
             e070v-trstatus = 'R'.
        CONTINUE.
      ENDIF.



      MOVE-CORRESPONDING e070v TO t_e070v.

      APPEND t_e070v.

    ENDSELECT.


  ENDIF. " IF NOT r_order[] IS INITIAL OR NOT r_projet[] IS INITIAL.

*  break to42718.


*---Cas des ordres d'un autre user mais sous lequel on a une tâche

  IF NOT r_owner[] IS INITIAL AND r_order[] IS INITIAL.
    SELECT * FROM e070v
                WHERE
                ( trkorr LIKE 'de1%'
                AND client IN r_client
                AND as4user IN r_owner
                AND as4date IN r_date
                AND ( trfunction = 'R'
                   OR trfunction = 'S'
                   OR trfunction = 'Q' )
                AND strkorr LIKE 'de1K%' ).
      SELECT SINGLE * FROM e070v
      INTO ls_e070v
      WHERE trkorr = e070v-strkorr
      AND trfunction IN r_typ.

* Par rapport à l'ordre trouvé, il faut revérifier les critères de
* sélection !

      IF sy-subrc = 0.
        IF ls_e070v-as4user <> e070v-as4user.

          MOVE ls_e070v-as4text TO w_projet.
          TRANSLATE w_projet TO UPPER CASE.
          IF w_projet IN r_projet.

* Filtre supplémentaire
            IF NOT p_de1 IS INITIAL AND
                   p_ie1 IS INITIAL  AND
                   p_ve1 IS INITIAL AND                     " V2.0
                   p_ve2 IS INITIAL AND                     " V2.0
                   p_ve3 IS INITIAL AND                     " V2.0
                   p_ve4 IS INITIAL AND                     " V2.0
                   p_de2 IS INITIAL AND                     " V2.0
                   p_pe1 IS INITIAL AND
                   ls_e070v-trstatus = 'R'.
              CONTINUE.
            ENDIF. "NOT p_de1 IS INITIAL AND ...



            MOVE-CORRESPONDING ls_e070v TO t_e070v.
            APPEND t_e070v.

          ENDIF. "w_projet IN r_projet

        ENDIF. "ls_e070v-as4user <> e070v-as4user
      ENDIF. "sy-subrc = 0
    ENDSELECT.
  ENDIF.


*------------- Tri de la table t_070v  ordre sup / ordre
  SORT t_e070v BY strkorr trkorr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM t_e070v
  COMPARING trkorr.


ENDFORM.                    "f111_select_transport_req

*&--------------------------------------------------------------------*
*&      FORM f200_init_alv                                            *
*&--------------------------------------------------------------------*
*       Initialisation des ALV                                        *
*---------------------------------------------------------------------*

FORM f200_init_alv.

  str_is_variant-report = sy-repid.

* indication du champ correspondant Ã  la couleur

  is_layout-coltab_fieldname = 'COLOR'.

* indication du OK code choisi pour le 2x click

  is_layout-f2code          = 'PICK'.

  w_grid_settings-coll_top_p = 'X'.

ENDFORM.                    "f200_init_alv

* Begin of V2.0
*&--------------------------------------------------------------------*
*&      FORM f250_cts_project                                         *
*&--------------------------------------------------------------------*
*       Filtre sur les CTS Project                                    *
*---------------------------------------------------------------------*
FORM f250_cts_project.

  DATA lc_att_cts    TYPE trattr VALUE 'SAP_CTS_PROJECT'.
  DATA lt_e070a      TYPE TABLE OF e070a.
  DATA lwa_e070a     TYPE e070a.


* Si il y a des OTs déjà sélectionnés et qu'on veut filtrer sur le CTS
  IF NOT r_cts[] IS INITIAL.

    LOOP AT t_e070v.

*   Sélection des OTs pour les CTS project renseignés
      SELECT *
      FROM e070a
      WHERE attribute = lc_att_cts
      AND   reference IN r_cts
      AND   trkorr = t_e070v-trkorr.
      ENDSELECT.
*       Si la donnée n'existe pas, suppression
      IF sy-subrc <> 0.
        DELETE t_e070v.
      ENDIF.



    ENDLOOP.



*   Si aucune donnée n'a été retournée, on supprime toutes les données
    IF sy-subrc <> 0.
      REFRESH t_e070v.
      CLEAR t_e070v.

*   Sinon, on supprime toutes les données n'ayant pas été retournées
    ELSE.
      SORT lt_e070a.

    ENDIF.


  ENDIF.


ENDFORM.                    "f250_cts_project
* End of V2.0

* Begin of V2.1
*&--------------------------------------------------------------------*
*&      FORM f260_sp_number                                           *
*&--------------------------------------------------------------------*
*       Filter on SP number                                           *
*---------------------------------------------------------------------*
FORM f260_sp_number.
* Check the SP number
  IF NOT r_sp_num[] IS INITIAL.
    REFRESH t_e070a.
    SELECT trkorr reference INTO CORRESPONDING FIELDS OF TABLE t_e070a
                                FROM e070a
                                WHERE attribute = c_att_issd
                                AND   reference IN r_sp_num.

    IF sy-subrc = 0.
      LOOP AT t_e070a INTO wa_e070a.
        CLEAR : r_order.
        r_order-sign = 'I'.
        r_order-option = 'EQ'.
        r_order-low = wa_e070a-trkorr.
        APPEND r_order.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Initialize the search text with the SP
  IF r_projet[] IS INITIAL.
    LOOP AT r_sp_num.
      CLEAR r_projet.
      r_projet-sign = 'I'.
      r_projet-option = 'CP'.
      CONCATENATE '*' r_sp_num-low '*' INTO r_projet-low.
      APPEND r_projet.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f260_sp_number
*&--------------------------------------------------------------------*
*&      FORM f270_cr_number                                           *
*&--------------------------------------------------------------------*
*       Filter on CR number                                           *
*---------------------------------------------------------------------*
FORM f270_cr_number.
* Check the QC number
  IF NOT r_qc_num[] IS INITIAL.
    REFRESH t_e070a.
    SELECT trkorr reference INTO CORRESPONDING FIELDS OF TABLE t_e070a
                                FROM e070a
                                WHERE attribute = c_att_qc
                                AND   reference IN r_qc_num.

    IF sy-subrc = 0.
      LOOP AT t_e070a INTO wa_e070a.
        CLEAR : r_order.
        r_order-sign = 'I'.
        r_order-option = 'EQ'.
        r_order-low = wa_e070a-trkorr.
        APPEND r_order.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Initialize the search text with the CR
  IF r_projet[] IS INITIAL.
    LOOP AT r_qc_num.
      CLEAR r_projet.
      r_projet-sign = 'I'.
      r_projet-option = 'CP'.
      CONCATENATE '*' r_qc_num-low '*' INTO r_projet-low.
      APPEND r_projet.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f270_cr_number
*End of V2.1

*&--------------------------------------------------------------------*
*&      Form  f300_select_additional_data
*&--------------------------------------------------------------------*
*       Preparation du select et du header en fonction des parametres
*---------------------------------------------------------------------*

FORM f300_select_additional_data.

*  PERFORM read_ztr_texts.
  PERFORM get_charm_change_from_tr. "Charm

  LOOP AT t_e070v.

    w_flag_vi2 = 0.

***********************************************************************
* On exclu les tâches
***********************************************************************
    IF t_e070v-strkorr IS INITIAL. "Si ce n'est pas une tâche
*--- initialisation de l'environnement à  de1
      t_e070v-envir = 'DE1'.
      t_e070v-info = '@4A@'.
    ENDIF.

***********************************************************************
* Analyse des journaux de transport pour les ordres released
***********************************************************************
    IF  t_e070v-strkorr IS INITIAL AND "Ce n'est pas une tâche
        t_e070v-trstatus = 'R'. "Si l'ordre est libéré

      t_settings-detailed_depiction = 'X'.
      APPEND t_settings.

* Récupération des infos liées au(x) transport(s)
      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr   = t_e070v-trkorr
          is_settings = t_settings
        IMPORTING
          es_cofile   = t_infos.

      IF t_infos-systems[] IS INITIAL.
        t_e070v-envir = '???'.
        t_e070v-info = '@8O@'.
      ELSE.
        PERFORM f305_check_cofile_log.
      ENDIF.


    ENDIF.

***********************************************************************
* type d'ordre W -> C (Custo) K -> W (Workbench)
***********************************************************************
    IF t_e070v-trfunction = 'W'.
      t_e070v-trfunction = 'C'.
    ELSE.
      t_e070v-trfunction = 'W'.
    ENDIF.




***********************************************************************
* Cas particulier des tâches
***********************************************************************
    IF NOT t_e070v-strkorr IS INITIAL. "On est sur une tâche
      READ TABLE t_e070v WITH KEY trkorr = t_e070v-strkorr
      INTO w_e070v.
      IF sy-subrc = 0.
        t_e070v-envir = w_e070v-envir.
        t_e070v-info = w_e070v-info.
        t_e070v-ie1rc = w_e070v-ie1rc.
        t_e070v-ie1date = w_e070v-ie1date.
        t_e070v-ie1time = w_e070v-ie1time.

        t_e070v-ve1rc = w_e070v-ve1rc.
        t_e070v-ve1date = w_e070v-ve1date.
        t_e070v-ve1time = w_e070v-ve1time.

        t_e070v-ve2rc = w_e070v-ve2rc.
        t_e070v-ve2date = w_e070v-ve2date.
        t_e070v-ve2time = w_e070v-ve2time.

        t_e070v-ve3rc = w_e070v-ve3rc.
        t_e070v-ve3date = w_e070v-ve3date.
        t_e070v-ve3time = w_e070v-ve3time.

        t_e070v-ve4rc = w_e070v-ve4rc.
        t_e070v-ve4date = w_e070v-ve4date.
        t_e070v-ve4time = w_e070v-ve4time.

        t_e070v-de2rc = w_e070v-de2rc.
        t_e070v-de2date = w_e070v-de2date.
        t_e070v-de2time = w_e070v-de2time.

        t_e070v-pe1rc = w_e070v-pe1rc.
        t_e070v-pe1date = w_e070v-pe1date.
        t_e070v-pe1time = w_e070v-pe1time.
        t_e070v-trfunction = w_e070v-trfunction.
        t_e070v-color[] = w_e070v-color[].

      ENDIF.

    ENDIF.


***********************************************************************
* Mise à jour de la ligne
***********************************************************************
    MODIFY t_e070v.



***********************************************************************
* Mise en forme couleur en fonction de l'environnement trouvé
***********************************************************************
    PERFORM f310_set_colors_of_actual_env.


***********************************************************************
* Analyse des codes retour
***********************************************************************
    PERFORM f315_set_color_of_transport_rc.



***********************************************************************
* Alimentation de la table résultat
***********************************************************************
    CLEAR select_line.

*    IF t_e070v-strkorr IS INITIAL. "On n'est pas sur une tâche

    IF ( p_pe1 = 'X' AND t_e070v-envir CS 'PE1'  ) OR
       ( p_ve1 = 'X' AND t_e070v-envir CS 'VE1'  ) OR       " V2.0
       ( p_ve2 = 'X' AND t_e070v-envir CS 'VE2'  ) OR       " V2.0
       ( p_ve3 = 'X' AND t_e070v-envir CS 'VE3'  ) OR       " V2.0
       ( p_ve4 = 'X' AND t_e070v-envir CS 'VE4'  ) OR       " V2.0
       ( p_de2 = 'X' AND t_e070v-envir CS 'DE2'  ) OR       " V2.0
       ( p_ie1 = 'X' AND t_e070v-envir CS 'IE1'  ) OR
       ( p_de1 = 'X' AND t_e070v-envir CS 'DE1'  ) OR
       ( t_e070v-envir CS '???' ).                "OR

      IF t_e070v-ie1date IN p_ie1dat AND
         t_e070v-ve1date IN p_ve1dat AND                    " V2.0
         t_e070v-pe1date IN p_pe1dat.

        select_line = 'X'.


        IF t_e070v-info = '@8O@' AND p_wt_log IS INITIAL.
          select_line = ' '.
        ENDIF.

        IF t_e070v-info = '@11@' AND p_buff IS INITIAL.
          select_line = ' '.
        ENDIF.

      ENDIF.

    ENDIF.


    IF select_line = 'X'.
      MOVE-CORRESPONDING t_e070v TO t_result.
      APPEND t_result.
    ENDIF.

  ENDLOOP.

  PERFORM read_ztr_texts_result.
ENDFORM.                    "f300_select_additional_data

*---------------------------------------------------------------------*
*       FORM f305_check_cofile_log                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM f305_check_cofile_log.


***********************************************************************
* Recherche de l'import dans IE1
***********************************************************************

  PERFORM f307_check_import USING 'IE1'.


* Begin of V2.0
***********************************************************************
* Recherche de l'import dans VE1
***********************************************************************

  PERFORM f307_check_import USING 'VE1'.
* End of V2.0

* Begin of V2.0
***********************************************************************
* Recherche de l'import dans VE2
***********************************************************************

  PERFORM f307_check_import USING 'VE2'.
* End of V2.0

* Begin of V2.0
***********************************************************************
* Recherche de l'import dans VE3
***********************************************************************

  PERFORM f307_check_import USING 'VE3'.
* End of V2.0

* Begin of V2.0
***********************************************************************
* Recherche de l'import dans ve4
***********************************************************************

  PERFORM f307_check_import USING 'VE4'.
* End of V2.0

* Begin of V2.0
***********************************************************************
* Recherche de l'import dans DE2
***********************************************************************

  PERFORM f307_check_import USING 'DE2'.
* End of V2.0

***********************************************************************
* Recherche de l'import dans PE1
***********************************************************************

  PERFORM f307_check_import USING 'PE1'.

ENDFORM.                    "f305_check_cofile_log
*---------------------------------------------------------------------*
*       FORM f307_check_import                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  S_SYSID                                                       *
*---------------------------------------------------------------------*
FORM f307_check_import USING s_sysid TYPE trtarsys.
  READ TABLE t_infos-systems WITH KEY systemid = s_sysid INTO t_system.

  IF sy-subrc = 0.

    LOOP AT t_system-steps INTO t_step.

      IF t_step-stepid = 'I'.

*      READ TABLE t_system-steps WITH KEY stepid = 'I' INTO t_step.
*      IF sy-subrc = 0.
        t_e070v-envir = s_sysid.

*        t_e070v-ie1rc = t_system-rc.
        CONCATENATE 't_e070v-' s_sysid 'rc' INTO fieldname.
        ASSIGN (fieldname) TO <fieldsymbol>.
        <fieldsymbol> = t_system-rc.
        IF t_system-rc > '8'.
          t_e070v-info = '@0A@'.
        ELSEIF t_system-rc > '4'.
          t_e070v-info = '@09@'.
        ELSE.
          t_e070v-info = '@08@'.
        ENDIF.


        DESCRIBE TABLE t_step-actions LINES zn1.
        READ TABLE t_step-actions INDEX zn1 INTO t_action.

*        t_e070v-fgidate = t_action-date.
        CONCATENATE 't_e070v-' s_sysid 'date' INTO fieldname.
        ASSIGN (fieldname) TO <fieldsymbol>.
        <fieldsymbol> = t_action-date.



*        t_e070v-fgitime = t_action-time.
        CONCATENATE 't_e070v-' s_sysid 'time' INTO fieldname.
        ASSIGN (fieldname) TO <fieldsymbol>.
        <fieldsymbol> = t_action-time.


        CLEAR t_action.

*        EXIT. "Add GGI take the last date. JMOU to de-comment.


      ENDIF. "Imported in s_sysid

      IF t_step-stepid = '>'.
        t_e070v-info = '@11@'.
*        t_e070v-ie1rc = ''.

        CONCATENATE 't_e070v-' s_sysid 'rc' INTO fieldname.
        ASSIGN (fieldname) TO <fieldsymbol>.
        <fieldsymbol> = ''.
      ENDIF.

      CLEAR t_step.

    ENDLOOP.
  ENDIF.

  CLEAR t_system.

* Traitement transport manuel

  READ TABLE t_infos-systems WITH KEY systemid = 'VI2' INTO t_system.
  IF sy-subrc = 0.
    w_flag_vi2 = 1.
  ENDIF.
  READ TABLE t_infos-systems WITH KEY systemid = 'VIR' INTO t_system.
  IF sy-subrc = 0.
    w_flag_vi2 = 1.
  ENDIF.


ENDFORM.                    "f307_check_import
*---------------------------------------------------------------------*
*       FORM f310_set_colors_of_actual_env                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM f310_set_colors_of_actual_env.


  REFRESH t_color. CLEAR t_color.

* Begin of ST34735
* Color the defined TO in the selection screen
  IF p_lktr = 'X'.
    LOOP AT r_order_noaddopt.
      IF r_order_noaddopt-low = t_e070v-trkorr(10).
        t_color-fieldname = 'TRKORR'.
        t_color-color-col = 3.
        t_color-color-int = 0.
        t_color-color-inv = 0.
        APPEND t_color.
      ENDIF.
    ENDLOOP.
  ENDIF.
* End of ST34735


  IF t_e070v-envir CS 'PE1'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 1.
    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.

* Begin of V2.0
  ELSEIF  t_e070v-envir CS 'VE1'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 6.

* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.
* End of V2.0

* Begin of V2.0
  ELSEIF  t_e070v-envir CS 'VE2'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 6.

* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.
* End of V2.0

* Begin of V2.0
  ELSEIF  t_e070v-envir CS 'VE3'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 6.

* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.
* End of V2.0

* Begin of V2.0
  ELSEIF  t_e070v-envir CS 'VE4'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 6.

* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.
* End of V2.0

* Begin of V2.0
  ELSEIF  t_e070v-envir CS 'DE2'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 6.

* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.
* End of V2.0

  ELSEIF  t_e070v-envir CS 'IE1'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 5.

* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.

  ELSEIF  t_e070v-envir CS 'DE1'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 3.
* Traitement transport manuel
    IF w_flag_vi2 = 1.
      t_e070v-envir = 'PE1'.
      t_color-color-col = 7.
    ENDIF.

    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.

  ELSEIF  t_e070v-envir CS '???'.
    CLEAR t_color.
    t_color-fieldname = 'ENVIR'.
    t_color-color-col = 7.
    t_color-color-int = 0.
    t_color-color-inv = 0.
    APPEND t_color.

  ENDIF.


ENDFORM.                    "f310_set_colors_of_actual_env
*---------------------------------------------------------------------*
*       FORM f315_set_color_of_transport_rc                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM f315_set_color_of_transport_rc.
  CASE t_e070v-ie1rc.
    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'IE1RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'IE1RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.


* Begin of V2.0
  CASE t_e070v-ve1rc.
    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'VE1RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'VE1RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.
* End of V2.0

* Begin of V2.0
  CASE t_e070v-ve2rc.
    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'VE2RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'VE2RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.
* End of V2.0

* Begin of V2.0
  CASE t_e070v-ve3rc.
    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'VE3RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'VE3RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.
* End of V2.0

* Begin of V2.0
  CASE t_e070v-ve4rc.
    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'VE4RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'VE4RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.
* End of V2.0

*Begin of V2.0
  CASE t_e070v-de2rc.
    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'DE2RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'DE2RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.
* End of V2.0
*----------------------------------------------------------------------

  CASE t_e070v-pe1rc.

    WHEN 0.

    WHEN 4.
      CLEAR t_color.
      t_color-fieldname = 'PE1RC'.
      t_color-color-col = 3.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

    WHEN OTHERS.
      CLEAR t_color.
      t_color-fieldname = 'PE1RC'.
      t_color-color-col = 6.
      t_color-color-int = 1.
      t_color-color-inv = 0.
      APPEND t_color.

  ENDCASE.
*----------------------------------------------------------------------


  t_e070v-color = t_color[].
ENDFORM.                    "f315_set_color_of_transport_rc

*&--------------------------------------------------------------------*
*&      Form  f400_display_result
*&--------------------------------------------------------------------*
*       Affichage du rÃ©sultat sous forme d'une grille ALV
*---------------------------------------------------------------------*

FORM f400_display_result.

* Initialisation pour l'affichage de la grille ALV

  str_is_variant-report       = sy-repid.
  is_layout-colwidth_optimize = 'X'.

* Initialisation pour l'affichage de l'entête

  CLEAR gt_events.
  gt_events-name = slis_ev_top_of_page.    " évènement haut de page
  gt_events-form = 'TOP_OF_PAGE'.          " nom de la routine du prog
  APPEND gt_events.

* Préparation des colonnes à afficher

  REFRESH tab_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = str_is_variant-report
      i_internal_tabname     = 'T_RESULT'
      i_inclname             = str_is_variant-report
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = tab_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc NE 0.
*    message i398(00) with text-e03.
    STOP.
  ENDIF.

* Modification du titre des colonnes


  LOOP AT tab_fieldcat.

    CASE tab_fieldcat-fieldname.

      WHEN 'TRKORR'.
        tab_fieldcat-reptext_ddic = 'Order'.
        tab_fieldcat-seltext_l = 'Order'.
        tab_fieldcat-seltext_m = 'Order'.
        tab_fieldcat-seltext_s = 'Order'.
      WHEN 'STRKORR'.
        tab_fieldcat-reptext_ddic = 'Task'.
        tab_fieldcat-seltext_l = 'Task'.
        tab_fieldcat-seltext_m = 'Task'.
        tab_fieldcat-seltext_s = 'Task'.
      WHEN 'TRFUNCTION'.
        tab_fieldcat-reptext_ddic = 'Type'.
        tab_fieldcat-seltext_l = 'Type'.
        tab_fieldcat-seltext_m = 'Type'.
        tab_fieldcat-seltext_s = 'Type'.
      WHEN 'AS4USER'.
        tab_fieldcat-reptext_ddic = 'Owner'.
        tab_fieldcat-seltext_l = 'Owner'.
        tab_fieldcat-seltext_m = 'Owner'.
        tab_fieldcat-seltext_s = 'Owner'.
      WHEN 'NAME_TEXT'.
        tab_fieldcat-reptext_ddic = 'Name'.
        tab_fieldcat-seltext_l = 'Name'.
        tab_fieldcat-seltext_m = 'Name'.
        tab_fieldcat-seltext_s = 'Name'.
      WHEN 'AS4TEXT'.
        tab_fieldcat-reptext_ddic = 'Description'.
        tab_fieldcat-seltext_l = 'Description'.
        tab_fieldcat-seltext_m = 'Description'.
        tab_fieldcat-seltext_s = 'Description'.
      WHEN 'ENVIR'.
        tab_fieldcat-reptext_ddic = 'Actual Environnement'.
        tab_fieldcat-seltext_l = 'Actual Environnement'.
        tab_fieldcat-seltext_m = 'Actual Env'.
        tab_fieldcat-seltext_s = 'Env'.
      WHEN 'INFO'.
        tab_fieldcat-reptext_ddic = 'Additional Information'.
        tab_fieldcat-seltext_l = 'Additional Information'.
        tab_fieldcat-seltext_m = 'Add. Info.'.
        tab_fieldcat-seltext_s = 'Info'.
*Begin of SAGG++
      WHEN 'SP_NUMBER'.
        tab_fieldcat-reptext_ddic = 'SP Number'.
        tab_fieldcat-seltext_l = 'SP Number'.
        tab_fieldcat-seltext_m = 'SP Number'.
        tab_fieldcat-seltext_s = 'SP Number'.

      WHEN 'QC_NUMBER'.
        tab_fieldcat-reptext_ddic = 'QC Number'.
        tab_fieldcat-seltext_l = 'QC Number'.
        tab_fieldcat-seltext_m = 'QC Number'.
        tab_fieldcat-seltext_s = 'QC Number'.
        tab_fieldcat-hotspot = 'X'.

      WHEN 'CTS_PROJECT'.
        tab_fieldcat-reptext_ddic = 'CTS Project'.
        tab_fieldcat-seltext_l = 'CTS Project'.
        tab_fieldcat-seltext_m = 'CTS Project'.
        tab_fieldcat-seltext_s = 'CTS Project'.
*END of SAGG++

      WHEN 'IE1RC'.
        tab_fieldcat-reptext_ddic = 'IE1 Transport Return Code'.
        tab_fieldcat-seltext_l = 'IE1 RC'.
        tab_fieldcat-seltext_m = 'IE1 RC'.
        tab_fieldcat-seltext_s = 'IE1 RC'.
* Begin of Add GGI
      WHEN 'GGIRC'.
*        tab_fieldcat-reptext_ddic = 'GGI Transport Return Code'.
*        tab_fieldcat-seltext_l = 'GGI RC'.
*        tab_fieldcat-seltext_m = 'GGI RC'.
*        tab_fieldcat-seltext_s = 'GGI RC'.
* End of Add GGI
      WHEN 'FGIRC'.
*        tab_fieldcat-reptext_ddic = 'FGI Transport Return Code'.
*        tab_fieldcat-seltext_l = 'FGI RC'.
*        tab_fieldcat-seltext_m = 'FGI RC'.
*        tab_fieldcat-seltext_s = 'FGI RC'.
* Begin of V2.0
      WHEN 'VE1RC'.
        tab_fieldcat-reptext_ddic = 'VE1 Transport Return Code'.
        tab_fieldcat-seltext_l = 'VE1 RC'.
        tab_fieldcat-seltext_m = 'VE1 RC'.
        tab_fieldcat-seltext_s = 'VE1 RC'.
* End of V2.0
* Begin of V2.0
      WHEN 'VE2RC'.
        tab_fieldcat-reptext_ddic = 'VE2 Transport Return Code'.
        tab_fieldcat-seltext_l = 'VE2 RC'.
        tab_fieldcat-seltext_m = 'VE2 RC'.
        tab_fieldcat-seltext_s = 'VE2 RC'.
* End of V2.0
* Begin of V2.0
      WHEN 'VE3RC'.
        tab_fieldcat-reptext_ddic = 'VE3 Transport Return Code'.
        tab_fieldcat-seltext_l = 'VE3 RC'.
        tab_fieldcat-seltext_m = 'VE3 RC'.
        tab_fieldcat-seltext_s = 'VE3 RC'.
* End of V2.0
* Begin of V2.0
      WHEN 'VE4RC'.
        tab_fieldcat-reptext_ddic = 'VE4 Transport Return Code'.
        tab_fieldcat-seltext_l = 'VE4 RC'.
        tab_fieldcat-seltext_m = 'VE4 RC'.
        tab_fieldcat-seltext_s = 'VE4 RC'.
* End of V2.0
* Begin of V2.0
      WHEN 'DE2RC'.
        tab_fieldcat-reptext_ddic = 'DE2 Transport Return Code'.
        tab_fieldcat-seltext_l = 'DE2 RC'.
        tab_fieldcat-seltext_m = 'DE2 RC'.
        tab_fieldcat-seltext_s = 'DE2 RC'.
* End of V2.0
* BEGIN OF SAGG++
      WHEN 'BGIRC'.
*        tab_fieldcat-reptext_ddic = 'BGI Transport Return Code'.
*        tab_fieldcat-seltext_l = 'BGI RC'.
*        tab_fieldcat-seltext_m = 'BGI RC'.
*        tab_fieldcat-seltext_s = 'BGI RC'.
      WHEN 'CGIRC'.
*        tab_fieldcat-reptext_ddic = 'CGI Transport Return Code'.
*        tab_fieldcat-seltext_l = 'CGI RC'.
*        tab_fieldcat-seltext_m = 'CGI RC'.
*        tab_fieldcat-seltext_s = 'CGI RC'.
* END OF SAGG++
      WHEN 'PE1RC'.
        tab_fieldcat-reptext_ddic = 'PE1 Transport Return Code'.
        tab_fieldcat-seltext_l = 'PE1 RC'.
        tab_fieldcat-seltext_m = 'PE1 RC'.
        tab_fieldcat-seltext_s = 'PE1 RC'.
      WHEN 'IE1DATE'.
        tab_fieldcat-reptext_ddic = 'IE1 Transport Date'.
        tab_fieldcat-seltext_l = 'IE1 Date'.
        tab_fieldcat-seltext_m = 'IE1 Date'.
        tab_fieldcat-seltext_s = 'IE1 Date'.
* Begin of Add GGI
      WHEN 'GGIDATE'.
*        tab_fieldcat-reptext_ddic = 'GGI Transport Date'.
*        tab_fieldcat-seltext_l = 'GGI Date'.
*        tab_fieldcat-seltext_m = 'GGI Date'.
*        tab_fieldcat-seltext_s = 'GGI Date'.
* End of Add GGI
      WHEN 'FGIDATE'.
*        tab_fieldcat-reptext_ddic = 'FGI Transport Date'.
*        tab_fieldcat-seltext_l = 'FGI Date'.
*        tab_fieldcat-seltext_m = 'FGI Date'.
*        tab_fieldcat-seltext_s = 'FGI Date'.
* Begin of V2.0
      WHEN 'VE1DATE'.
        tab_fieldcat-reptext_ddic = 'VE1 Transport Date'.
        tab_fieldcat-seltext_l = 'VE1 Date'.
        tab_fieldcat-seltext_m = 'VE1 Date'.
        tab_fieldcat-seltext_s = 'VE1 Date'.
* End of V2.0
* Begin of V2.0
      WHEN 'VE2DATE'.
        tab_fieldcat-reptext_ddic = 'VE2 Transport Date'.
        tab_fieldcat-seltext_l = 'VE2 Date'.
        tab_fieldcat-seltext_m = 'VE2 Date'.
        tab_fieldcat-seltext_s = 'VE2 Date'.
* End of V2.0
* Begin of V2.0
      WHEN 'VE3DATE'.
        tab_fieldcat-reptext_ddic = 'VE3 Transport Date'.
        tab_fieldcat-seltext_l = 'VE3 Date'.
        tab_fieldcat-seltext_m = 'VE3 Date'.
        tab_fieldcat-seltext_s = 'VE3 Date'.
* End of V2.0
* Begin of V2.0
      WHEN 'VE4DATE'.
        tab_fieldcat-reptext_ddic = 'VE4 Transport Date'.
        tab_fieldcat-seltext_l = 'VE4 Date'.
        tab_fieldcat-seltext_m = 'VE4 Date'.
        tab_fieldcat-seltext_s = 'VE4 Date'.
* End of V2.0
* Begin of V2.0
      WHEN 'DE2DATE'.
        tab_fieldcat-reptext_ddic = 'DE2 Transport Date'.
        tab_fieldcat-seltext_l = 'DE2 Date'.
        tab_fieldcat-seltext_m = 'DE2 Date'.
        tab_fieldcat-seltext_s = 'DE2 Date'.
* End of V2.0
* BEGIN OF SAGG++
      WHEN 'BGIDATE'.
*        tab_fieldcat-reptext_ddic = 'BGI Transport Date'.
*        tab_fieldcat-seltext_l = 'BGI Date'.
*        tab_fieldcat-seltext_m = 'BGI Date'.
*        tab_fieldcat-seltext_s = 'BGI Date'.
      WHEN 'CGIDATE'.
*        tab_fieldcat-reptext_ddic = 'CGI Transport Date'.
*        tab_fieldcat-seltext_l = 'CGI Date'.
*        tab_fieldcat-seltext_m = 'CGI Date'.
*        tab_fieldcat-seltext_s = 'CGI Date'.
* END OF SAGG++
      WHEN 'PE1DATE'.
        tab_fieldcat-reptext_ddic = 'PE1 Transport Date'.
        tab_fieldcat-seltext_l = 'PE1 Date'.
        tab_fieldcat-seltext_m = 'PE1 Date'.
        tab_fieldcat-seltext_s = 'PE1 Date'.
      WHEN 'AS4DATE'.
        tab_fieldcat-reptext_ddic = 'de1 Creation/Change Date'.
        tab_fieldcat-seltext_l = 'DE1 Creation Date'.
        tab_fieldcat-seltext_m = 'DE1 Creation Date'.
        tab_fieldcat-seltext_s = 'DE1 Creation Date'.

      WHEN 'ZCOM'.
        tab_fieldcat-reptext_ddic = 'Comment'.
        tab_fieldcat-seltext_l = 'Comment'.
        tab_fieldcat-seltext_m = 'Comment'.
        tab_fieldcat-seltext_s = 'Comment'.
*      WHEN 'TEXT1'.
*        tab_fieldcat-reptext_ddic = 'Analysis'.
*        tab_fieldcat-seltext_l = 'Analysis'.
*        tab_fieldcat-seltext_m = 'Analysis'.
*        tab_fieldcat-seltext_s = 'Analysis'.
*
*      WHEN 'TEXT2'.
*        tab_fieldcat-reptext_ddic = 'Sequence Order'.
*        tab_fieldcat-seltext_l = 'Sequence Order'.
*        tab_fieldcat-seltext_m = 'Sequence Order'.
*        tab_fieldcat-seltext_s = 'Sequence Order'.
*
*      WHEN 'TEXT3'.
*        tab_fieldcat-reptext_ddic = 'Other ISSD'.
*        tab_fieldcat-seltext_l = 'Other ISSD'.
*        tab_fieldcat-seltext_m = 'Other ISSD'.
*        tab_fieldcat-seltext_s = 'Other ISSD'.


      WHEN OTHERS.

    ENDCASE.

    tab_fieldcat-just = 'C'.
    MODIFY tab_fieldcat.

  ENDLOOP.

*<-- ChaRM --
  CLEAR tab_fieldcat.
  tab_fieldcat-col_pos = '70'.
  tab_fieldcat-reptext_ddic = 'Charm Change'.
  tab_fieldcat-seltext_l = 'Charm Change'.
  tab_fieldcat-seltext_m = 'Charm Ch.'.
  tab_fieldcat-seltext_s = 'Change'.
  tab_fieldcat-ddictxt = 'Change'.
  tab_fieldcat-tabname = 'T_RESULTS'.
  tab_fieldcat-fieldname = 'CHARM_CH'.
  APPEND tab_fieldcat.
*-- ChaRM -->

*BEGIN OF SAGG++

*Table associant un OT à son CTS PROJECT.
  DATA : BEGIN OF t_reference OCCURS 0,
    trkorr LIKE e070a-trkorr,
    reference LIKE ctsproject-trkorr,
  END OF t_reference.

  CONSTANTS: c_separator(3) TYPE c VALUE ' | '.

*Table associant la représentation interne d'un CTS PROJECT à sa
*representation externe.
  DATA : BEGIN OF t_cts_external_internal OCCURS 0,
    internal LIKE ctsproject-trkorr,
    external LIKE ctsproject-externalid,
  END OF t_cts_external_internal.

*Récupere le couples OT-CTS PROJECT correspondant à chaque OT de la
*table t_result. Ces couples sont stockés dans la table t_reference.
  IF NOT t_result[] IS INITIAL.
    SELECT trkorr reference INTO TABLE t_reference
    FROM e070a FOR ALL ENTRIES IN t_result
    WHERE trkorr EQ t_result-trkorr
    AND attribute EQ 'SAP_CTS_PROJECT'.

*Pour chaque CTS PROJECT dans la table t_reference on recupere sa
*représentation externe. Le couple CTS PROJECT externe/interne est
*stocké dans la table t_cts_external_internal.
    IF NOT t_reference[] IS INITIAL.
      SELECT trkorr externalid INTO TABLE t_cts_external_internal
      FROM ctsproject FOR ALL ENTRIES IN t_reference
      WHERE trkorr EQ t_reference-reference.
    ENDIF.

*Dans la table t_reference on remplace la représentation interne du CTS
*PROJECT par sa représentation externe.
    LOOP AT t_reference.
      LOOP AT t_cts_external_internal.
        IF t_reference-reference EQ t_cts_external_internal-internal.
          t_reference-reference = t_cts_external_internal-external.
          MODIFY t_reference FROM t_reference.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*On ajoute dans la table t_result la représentation externe du
*CTS_PROJECT correspont à l'OT courrant.
    LOOP AT t_result.
      LOOP AT t_reference.
        IF t_result-trkorr EQ t_reference-trkorr.
          t_result-cts_project = t_reference-reference.
          MODIFY t_result FROM t_result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*On vide t_reference
    CLEAR t_reference.

*Récupere le couples OT-QC NUMBER correspondant à chaque OT de la
*table t_result. Ces couples sont stockés dans la table t_reference.
    SELECT trkorr reference INTO TABLE t_reference
    FROM e070a FOR ALL ENTRIES IN t_result
    WHERE trkorr EQ t_result-trkorr
    AND attribute EQ 'QC'.

*On ajoute dans la table t_result le QC NUMBER
*correspont à l'OT courrant.
    LOOP AT t_result.
      LOOP AT t_reference.
        IF t_result-trkorr EQ t_reference-trkorr.
          IF t_result-qc_number IS INITIAL.
            t_result-qc_number = t_reference-reference.
          ELSE.
            CONCATENATE t_reference-reference t_result-qc_number INTO
t_result-qc_number SEPARATED BY c_separator.
          ENDIF.
          MODIFY t_result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*On vide t_reference
    CLEAR t_reference.

*Récupere le couples OT-SP NUMBER correspondant à chaque OT de la
*table t_result. Ces couples sont stockés dans la table t_reference.
    SELECT trkorr reference INTO TABLE t_reference
    FROM e070a FOR ALL ENTRIES IN t_result
    WHERE trkorr EQ t_result-trkorr
    AND attribute EQ 'ISSD'.

*On ajoute dans la table t_result le SP NUMBER
*correspont à l'OT courrant.
    LOOP AT t_result.
      LOOP AT t_reference.
        IF t_result-trkorr EQ t_reference-trkorr.
          IF t_result-sp_number IS INITIAL.
            t_result-sp_number = t_reference-reference.
          ELSE.
            CONCATENATE t_reference-reference t_result-sp_number INTO
t_result-sp_number SEPARATED BY c_separator.
          ENDIF.
          MODIFY t_result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDIF.
*END OF SAGG++

* Affichage de la grille

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_buffer_active          = ' '
      i_callback_program       = str_is_variant-report
      is_layout                = is_layout
      it_fieldcat              = tab_fieldcat[]
      i_callback_pf_status_set = 'SET_STATUT'
      it_events                = gt_events[]
      i_grid_settings          = w_grid_settings
      i_callback_user_command  = 'USER_COMMAND'
      i_save                   = 'A'
    TABLES
      t_outtab                 = t_result
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc NE 0.
*    message i398(00) with text-e03.
    STOP.
  ENDIF.

ENDFORM.                    " f400_display_result


*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       Affichage du log                                              *
*---------------------------------------------------------------------*
*  -->  R_UCOMM                                                       *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CASE r_ucomm.

* A l'evenement 2x click on lance l'affichage du log

    WHEN 'ZREQUEST'. "OK CODE défini à l'initialisation
      READ TABLE t_result INDEX rs_selfield-tabindex.

      CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST'
        EXPORTING
          iv_trkorr = t_result-trkorr.

*  DEMO pour boutons
*     set pf-status 'BOUTONS' excluding 'ZTEST' immediately.
*      rs_selfield-refresh = 'X'.
*

    WHEN 'PICK' OR 'ZTEST' OR '&CHANGE_R'.

      w_tabindex = rs_selfield-tabindex.
      READ TABLE t_result INDEX rs_selfield-tabindex.

      CASE rs_selfield-fieldname.

        WHEN 'TRKORR'.
          CALL FUNCTION 'TR_PRESENT_REQUEST'
            EXPORTING
              iv_trkorr    = t_result-trkorr
              iv_highlight = 'X'
              iv_showonly  = ' '.

        WHEN 'AS4USER'.

          CLEAR it_selections.
          it_selections-reqfunctions = 'KW'.
          it_selections-reqstatus = 'ALDO'.
          it_selections-taskfunctions = 'QRSX'.
          it_selections-taskstatus = 'ALDRNO'.
*it_selections-SUPPRESS_REQ_SELECTION
          it_selections-connect_req_task_conditions = 'X'.
          it_selections-singletasks = 'X'.
          it_selections-freetasks_f = 'QRSX'.
          it_selections-freetasks_s = 'ALDNRO'.

          APPEND it_selections.

          CLEAR it_selections.
          it_selections-reqfunctions = 'KW'.
          it_selections-reqstatus = 'R'.
          it_selections-taskfunctions = 'QRSX'.
          it_selections-taskstatus = 'R'.
          it_selections-connect_req_task_conditions = 'X'.
          it_selections-singletasks = 'X'.
          it_selections-freetasks_f = 'QRSX'.
          it_selections-freetasks_s = 'R'.
          it_selections-fromdate = sy-datum - 730.
          it_selections-todate = sy-datum.

          APPEND it_selections.




          CALL FUNCTION 'TRINT_PRESENT_REQUESTS'
            EXPORTING
              iv_organizer_type = 'A'
              iv_username       = t_result-as4user
              it_selections     = it_selections[].
*BEGIN OF SAGG++
        WHEN 'QC_NUMBER'.
          TYPES : BEGIN OF lty_qc_number,
                    line(7) TYPE c,
                  END OF lty_qc_number.
          DATA : lw_url TYPE string,
                 lw_count TYPE i,
                 lw_qc_number TYPE lty_qc_number-line.
          DATA : lt_qc_number TYPE TABLE OF lty_qc_number.
          DATA : ls_fieldcat_qc_num TYPE slis_fieldcat_alv.
          DATA : lt_fieldcat_qc_num TYPE  slis_t_fieldcat_alv.
          DATA : lw_result_qc_num TYPE slis_selfield.

          CONSTANTS: c_separator(3) TYPE c VALUE ' | '.

          CLEAR lw_result_qc_num.
* Check if there is multplie QC number
          SPLIT rs_selfield-value AT c_separator
                                  INTO TABLE lt_qc_number.

          DESCRIBE TABLE lt_qc_number LINES lw_count.

* if there is multiple qc number, open a popup to choose qc number
          IF lw_count > 1.

* POPUP preparation
            CLEAR  ls_fieldcat_qc_num.
            ls_fieldcat_qc_num-fieldname     = 'LINE'.
            ls_fieldcat_qc_num-datatype      = 'C'.
            ls_fieldcat_qc_num-outputlen     = '10'.
            ls_fieldcat_qc_num-seltext_l     = 'QC number'.
            ls_fieldcat_qc_num-seltext_m     = 'QC number'.
            ls_fieldcat_qc_num-seltext_s     = 'QC num'.
            ls_fieldcat_qc_num-reptext_ddic =  'QC number'.
            ls_fieldcat_qc_num-hotspot = 'X'.
            APPEND ls_fieldcat_qc_num TO lt_fieldcat_qc_num.

            CLEAR lw_result_qc_num.
* Popup display
            CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
              EXPORTING
                i_title              = 'Multi-QC Number'
                i_selection          = 'X'
                i_zebra              = 'X'
                i_scroll_to_sel_line = 'X'
                i_tabname            = 'lt_qc_number'
                it_fieldcat          = lt_fieldcat_qc_num
              IMPORTING
                es_selfield          = lw_result_qc_num
              TABLES
                t_outtab             = lt_qc_number
              EXCEPTIONS
                program_error        = 1
                OTHERS               = 2.
* Get result
            IF NOT lw_result_qc_num IS INITIAL.
              lw_qc_number = lw_result_qc_num-value.
            ENDIF.
          ELSE.
            lw_qc_number = rs_selfield-value.
          ENDIF.


* If a QC number is filled, call URL
          IF NOT lw_qc_number IS INITIAL.
            CONCATENATE
*                  'testdirector:de0-vmpqc01.ham.de.eu.airbus.corp'
                  'testdirector:qc-lb-p01.ham.de.eu.airbus.corp'
                  '/qcbin,ARP,ARP_PROD_R_2_1,[AnyUser];2:'
                  lw_qc_number
                INTO lw_url.

            CALL METHOD cl_gui_frontend_services=>execute
              EXPORTING
                document = lw_url
              EXCEPTIONS
                OTHERS   = 1.

          ENDIF.





*END OF SAGG++
        WHEN 'ZCOM'.
          rs_selfield-refresh = 'X'.

          DATA lw_user TYPE sy-uname.
          SELECT SINGLE low INTO lw_user
            FROM tvarv
            WHERE name EQ 'Z_ANIS'
            AND low EQ sy-uname.
          IF sy-subrc = 0.
            CALL SCREEN 80.
          ELSE.
            MESSAGE i001(00)
            WITH 'You don''t have the required authorization'.

          ENDIF.

        WHEN OTHERS.

      ENDCASE.

    WHEN 'EXPA'.
      IF w_grid_settings-coll_top_p = ' '.
        w_grid_settings-coll_top_p = 'X'.
      ELSE.
        w_grid_settings-coll_top_p = ' '.
      ENDIF.


  ENDCASE.

ENDFORM.                    "user_command

*&--------------------------------------------------------------------*
*&      Form  top_of_page
*&--------------------------------------------------------------------*
*  Affiche l'entÃªte de la grille ALV
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_comment[].

ENDFORM.                    " top_of_page

*&--------------------------------------------------------------------*
*&      Form  set_status
*&--------------------------------------------------------------------*
*  recuperation des boutons
*---------------------------------------------------------------------*
FORM set_statut USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'BOUTONS' EXCLUDING rt_extab.

ENDFORM.                    "set_statut
*&--------------------------------------------------------------------*
*&      Form  f350_select_user
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM f350_select_user .

  DATA: l_user LIKE t_result-as4user,
        l_name_text LIKE adrp-name_text.

  SORT t_result BY as4user.

  LOOP AT t_result.

    IF t_result-strkorr IS INITIAL.
      t_result-strkorr = t_result-trkorr.
    ENDIF.

    IF l_user <> t_result-as4user.
      l_user = t_result-as4user.
      SELECT SINGLE name_text INTO l_name_text
      FROM usr21 INNER JOIN adrp
      ON usr21~persnumber = adrp~persnumber
      WHERE bname = l_user.
      IF sy-subrc IS INITIAL.
        t_result-name_text = l_name_text.
      ENDIF.
    ELSE.
      t_result-name_text = l_name_text.
    ENDIF.
    MODIFY t_result.
  ENDLOOP.

  SORT t_result BY strkorr trkorr ASCENDING.

  LOOP AT t_result.
    IF t_result-trkorr = t_result-strkorr.
      CLEAR t_result-strkorr.
    ELSE.
      w_strkorr = t_result-strkorr.
      t_result-strkorr = t_result-trkorr.
      t_result-trkorr = w_strkorr.
    ENDIF.
    MODIFY t_result.
  ENDLOOP.


ENDFORM.                    " f350_select_user



*&---------------------------------------------------------------------
*&      Form  read_ztr_texts
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
*FORM read_ztr_texts .
*
*  DATA: lt_stxh LIKE stxh OCCURS 0 WITH HEADER LINE.
*  DATA: BEGIN OF lt_temp OCCURS 0,
*          tdname LIKE stxh-tdname.
*  DATA: END OF lt_temp.
*
*
** Check if transport order selected
*
*  CHECK NOT t_e070v[] IS INITIAL.
*
*  LOOP AT t_e070v.
*    lt_temp-tdname = t_e070v-trkorr.
*    APPEND lt_temp.
*  ENDLOOP.
*
** Check for which entry a text exists;
*
*  SELECT * FROM stxh INTO TABLE lt_stxh
*  FOR ALL ENTRIES IN lt_temp
*  WHERE tdspras  = sy-langu AND
*        tdobject = 'ZTR' AND
*        tdname   = lt_temp-tdname.
*
** Read texts only for transport orders with at least one text
*  LOOP AT t_e070v.
*
*    READ TABLE lt_stxh WITH KEY tdname = t_e070v-trkorr.
*
*    CHECK sy-subrc IS INITIAL.
** Text Edit 1
**--> init fields header
*    w_header-tdid     = 'TXT1'.
*    w_header-tdname   = t_e070v-trkorr.
*    w_header-tdspras  = sy-langu.
*    w_header-tdobject = 'ZTR'.
*
**
*    CALL FUNCTION 'READ_TEXT'
*         EXPORTING
*              id                      = w_header-tdid
*              language                = w_header-tdspras
*              name                    = w_header-tdname
*              object                  = w_header-tdobject
*         IMPORTING
*              header                  = w_header
*         TABLES
*              lines                   = wt_lines
*         EXCEPTIONS
*              id                      = 1
*              language                = 2
*              name                    = 3
*              not_found               = 4
*              object                  = 5
*              reference_check         = 6
*              wrong_access_to_archive = 7
*              OTHERS                  = 8.
**
**
*    IF sy-subrc = 0.
*      LOOP AT wt_lines.
*        CONCATENATE t_e070v-text1 wt_lines-tdline
*                 INTO t_e070v-text1 SEPARATED BY space.
*        EXIT.
*      ENDLOOP.
*    ENDIF.
*
**
** Text Edit 2
**--> init fields header
*    w_header-tdid     = 'TXT2'.
*
*    CALL FUNCTION 'READ_TEXT'
*         EXPORTING
*              id                      = w_header-tdid
*              language                = w_header-tdspras
*              name                    = w_header-tdname
*              object                  = w_header-tdobject
*         IMPORTING
*              header                  = w_header
*         TABLES
*              lines                   = wt_lines
*         EXCEPTIONS
*              id                      = 1
*              language                = 2
*              name                    = 3
*              not_found               = 4
*              object                  = 5
*              reference_check         = 6
*              wrong_access_to_archive = 7
*              OTHERS                  = 8.
**
**
*    IF sy-subrc = 0.
*      LOOP AT wt_lines.
*        t_e070v-text2 = wt_lines-tdline.
*        EXIT.
*      ENDLOOP.
*    ENDIF.
*
**
** Text Edit 1
**--> init fields header
*    w_header-tdid     = 'TXT3'.
*
*    CALL FUNCTION 'READ_TEXT'
*         EXPORTING
*              id                      = w_header-tdid
*              language                = w_header-tdspras
*              name                    = w_header-tdname
*              object                  = w_header-tdobject
*         IMPORTING
*              header                  = w_header
*         TABLES
*              lines                   = wt_lines
*         EXCEPTIONS
*              id                      = 1
*              language                = 2
*              name                    = 3
*              not_found               = 4
*              object                  = 5
*              reference_check         = 6
*              wrong_access_to_archive = 7
*              OTHERS                  = 8.
**
**
*    IF sy-subrc = 0.
*      LOOP AT wt_lines.
*        t_e070v-text3 = wt_lines-tdline.
*        EXIT.
*      ENDLOOP.
*    ENDIF.
*
*    MODIFY t_e070v.
*
*  ENDLOOP.
*
*ENDFORM.                    " read_ztr_texts
*&--------------------------------------------------------------------
*&      Form  AT_SELECTION_SCREEN_ON_FIELD
*&--------------------------------------------------------------------
FORM at_selection_screen_on_field      USING pv_field TYPE c.

  DATA: ls_object_text       LIKE ko100.

  CASE pv_field.
    WHEN 'OBJECTA'.
      CASE objecta.
        WHEN space. CLEAR: pgmida, objtexta, objnamea.
        WHEN '*'. pgmida = '*'.
        WHEN OTHERS.
          READ TABLE gt_object_texts INTO ls_object_text
                                   WITH KEY object = objecta.
          IF sy-subrc <> 0.
            MESSAGE e870(tk).
*           Bitte wÃ¤hlen Sie einen gÃ¼ltigen Objekttyp aus
          ELSE.
            pgmida   = ls_object_text-pgmid.
            objtexta = ls_object_text-text.
          ENDIF.
      ENDCASE.


  ENDCASE.
ENDFORM.                               " AT_SELECTION_SCREEN_ON_FIELD
*&---------------------------------------------------------------------
*&      Form  READ_OBJECT_TABLE
*&---------------------------------------------------------------------
FORM read_object_table    TABLES pt_object_texts STRUCTURE ko100.
  DATA: lv_lines       TYPE i.

  DESCRIBE TABLE pt_object_texts LINES lv_lines.
  IF lv_lines < 1.
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = pt_object_texts.

    DELETE pt_object_texts WHERE pgmid <> 'R3TR'
                             AND pgmid <> 'R3OB'
                             AND pgmid <> 'LIMU'
                             AND pgmid <> 'CORR'.

    SORT pt_object_texts BY pgmid object.
  ENDIF.

ENDFORM.                               " READ_OBJECT_TABLE
*&--------------------------------------------------------------------
*&      Form  AT_SELECTION_SCREEN_OUTPUT
*&--------------------------------------------------------------------
FORM at_selection_screen_output.

  DATA: BEGIN OF lt_excl_okcode OCCURS 0,
            okcode LIKE sy-ucomm,
        END   OF lt_excl_okcode.

* exclude some functions in the menu
  lt_excl_okcode-okcode = 'PRIN'.
  APPEND lt_excl_okcode.
  lt_excl_okcode-okcode = 'SJOB'.
  APPEND lt_excl_okcode.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_excl_okcode.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'OUT'.
        screen-input      = '0'.
        IF screen-name = 'OBJTEXTA'.
          screen-display_3d = '0'.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'OBJ'.
        IF NOT r_order-low IS INITIAL OR
           NOT r_client-low IS INITIAL OR
           NOT r_owner-low IS INITIAL OR
           NOT r_projet-low IS INITIAL.

          screen-input      = '0'.
          MODIFY SCREEN.
        ELSE.
          screen-input      = '1'.
          MODIFY SCREEN.

        ENDIF.
      WHEN 'TR'.
        IF NOT objecta IS INITIAL OR NOT objnamea IS INITIAL.
          screen-input      = '0'.
          MODIFY SCREEN.
        ELSE.
          screen-input      = '1'.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

*  IF gv_objectfield IS NOT INITIAL.
*    SET CURSOR FIELD gv_objectfield.
*    CLEAR gv_objectfield.
*  ENDIF.
ENDFORM.                               " AT_SELECTION_SCREEN_OUTPUT


*&--------------------------------------------------------------------
*&      Form  init_select_options
*&--------------------------------------------------------------------
*       text
*---------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
FORM init_select_options.
  r_date-sign = 'I'.
  r_date-option = 'GE'.
  r_date-low = sy-datum - 730.
  APPEND r_date.

* Begin of V2.0 -
*  r_owner-sign = 'I'.
*  r_owner-option = 'EQ'.
*  r_owner-low = sy-uname.
*  APPEND r_owner.
* End of V2.0 -

ENDFORM.                    " init_select_options

*<-- Begin V2.2 --
*&--------------------------------------------------------------------
*&      Form  f290_add_option
*&--------------------------------------------------------------------
*  Additional option form
*---------------------------------------------------------------------
FORM f290_add_option.
  r_order_noaddopt[] = r_order[].

* Get TR linked to object of TR in selection screen
  IF p_lktr = 'X' AND NOT r_order IS INITIAL.
    PERFORM get_linked_tr.
  ENDIF.


ENDFORM.                    " f290_add_option

*&--------------------------------------------------------------------
*&      Form  GET_LINKED_TR
*&--------------------------------------------------------------------
*  Get TR linked to object of TR in selection screen
*---------------------------------------------------------------------
FORM get_linked_tr .

  TYPES: BEGIN OF ty_obj,
          object   TYPE e071-object,
          obj_name TYPE e071-obj_name,
         END OF ty_obj.

  DATA: lt_obj               TYPE TABLE OF ty_obj,
        lt_version_list      TYPE TABLE OF vrsd,
        lt_version_list_full TYPE TABLE OF vrsd,
        lt_lversnodummy      TYPE TABLE OF  vrsn,
        lw_obj_name          TYPE vrsd-objname.

  FIELD-SYMBOLS: <lwa_obj> LIKE LINE OF lt_obj,
                 <lwa_vers> LIKE LINE OF lt_version_list.

  SELECT object obj_name INTO TABLE lt_obj FROM e071
   WHERE trkorr IN r_order
     AND object IN s_lktrob.

  IF sy-subrc IS INITIAL.
    LOOP AT lt_obj ASSIGNING <lwa_obj>.
*     Get version management of object.
      lw_obj_name = <lwa_obj>-obj_name.
      CLEAR lt_version_list.
      CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
        EXPORTING
*         DESTINATION            = ' '
          objname                = lw_obj_name
          objtype                = <lwa_obj>-object
        TABLES
          lversno_list           = lt_lversnodummy
          version_list           = lt_version_list
        EXCEPTIONS
          no_entry               = 1
          communication_failure_ = 2
          system_failure         = 3
          OTHERS                 = 4.
      IF sy-subrc = 0.
        APPEND LINES OF lt_version_list TO lt_version_list_full.
      ENDIF.
    ENDLOOP.
    IF NOT lt_version_list_full IS INITIAL.
      SORT lt_version_list_full BY korrnum.
      DELETE ADJACENT DUPLICATES FROM lt_version_list_full
                            COMPARING korrnum.
      CLEAR : r_order.
      r_order-sign = 'I'.
      r_order-option = 'EQ'.
      LOOP AT lt_version_list_full ASSIGNING <lwa_vers>
                                   WHERE NOT korrnum IS INITIAL.
        CLEAR r_order-low.
        r_order-low = <lwa_vers>-korrnum.
        APPEND r_order.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_LINKED_TR
*--  End V2.2 -->

*&-----------------------------------------------------------------*
*&      Form  read_ztr_texts
*&-----------------------------------------------------------------*
*       text
*------------------------------------------------------------------*

FORM read_ztr_texts_result.

  DATA: lt_stxh LIKE stxh OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF lt_temp OCCURS 0,
          tdname LIKE stxh-tdname.
  DATA: END OF lt_temp.

  FIELD-SYMBOLS : <fs_result> LIKE LINE OF t_result.
  UNASSIGN <fs_result>.

* Check if transport order selected
  CHECK NOT t_result[] IS INITIAL.


  LOOP AT t_result.
    lt_temp-tdname = t_result-trkorr.
    APPEND lt_temp.
  ENDLOOP.

* Check for which entry a text exists;

  SELECT * FROM stxh INTO TABLE lt_stxh
  FOR ALL ENTRIES IN lt_temp
  WHERE tdspras  = sy-langu AND
        tdobject = 'ZTR' AND
        tdname   = lt_temp-tdname.

  CHECK NOT lt_stxh[] IS INITIAL.

* Read texts only for transport orders with at least one text
  LOOP AT t_result ASSIGNING <fs_result>.

    READ TABLE lt_stxh WITH KEY tdname = <fs_result>-trkorr.

    CHECK sy-subrc IS INITIAL.
* Text Edit 1
*--> init fields header
    w_header-tdid     = 'ZCOM'.
    w_header-tdname   = <fs_result>-trkorr.
    w_header-tdspras  = sy-langu.
    w_header-tdobject = 'ZTR'.

*
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = w_header-tdid
        language                = w_header-tdspras
        name                    = w_header-tdname
        object                  = w_header-tdobject
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
*
    IF sy-subrc = 0.
      LOOP AT wt_lines.
        CONCATENATE <fs_result>-zcom wt_lines-tdline
                 INTO <fs_result>-zcom SEPARATED BY space.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " read_ztr_texts_result


*<-- ChaRM --
*&--------------------------------------------------------------------
*&      Form  GET_TR_FROM_CHARM_CHANGE
*&--------------------------------------------------------------------
*  Get TR from Charm system (PSM)
*---------------------------------------------------------------------
FORM get_tr_from_charm_change.


  DATA:
  lt_where   TYPE TABLE OF rfc_db_opt,
  lw_where   LIKE LINE OF lt_where,
  lt_fields  TYPE TABLE OF rfc_db_fld,
  lw_fields  LIKE LINE OF lt_fields,
  lt_ot      TYPE TABLE OF ty_ot,
  lwa_ot     LIKE LINE OF lt_ot.


*** WHERE condition from change number.
  CHECK NOT s_charm[] IS INITIAL.

  CONCATENATE c_fielname_trordhc-charm_change ' in (' INTO lw_where.
  APPEND lw_where TO lt_where.
  LOOP AT s_charm.
    CLEAR w_charm_ch.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = s_charm-low
      IMPORTING
        output = w_charm_ch.

    AT LAST."Pas de virgule mais ')'.
      CONCATENATE '''' w_charm_ch ''')' INTO lw_where.
      APPEND lw_where TO lt_where.
      EXIT.
    ENDAT.

    CONCATENATE '''' w_charm_ch ''',' INTO lw_where.
    APPEND lw_where TO lt_where.

  ENDLOOP.


*Fields needed.
  lw_fields = c_fielname_trordhc-charm_change.
  APPEND lw_fields TO lt_fields.
  lw_fields = c_fielname_trordhc-order.
  APPEND lw_fields TO lt_fields.


*Call request in PSM system.
  CALL FUNCTION 'RFC_READ_TABLE' DESTINATION c_charm_rfc_dest
    EXPORTING
      query_table           = '/TMWFLOW/TRORDHC'
*     DELIMITER             = ' '
*     NO_DATA               = ' '
*     ROWSKIPS              = 0
*     ROWCOUNT              = 0
    TABLES
      OPTIONS               = lt_where
      fields                = lt_fields
      data                  = lt_ot
    EXCEPTIONS
      table_not_available   = 1
      table_without_data    = 2
      option_not_valid      = 3
      field_not_valid       = 4
      not_authorized        = 5
      data_buffer_exceeded  = 6
      system_failure        = 7
      communication_failure = 8
      resource_failure      = 9
      OTHERS                = 10.

  IF sy-subrc <> 0.
    w_charm_conex_failed = sy-subrc.
    MESSAGE i001(00) WITH
 'Error in communication change number was empty'.

  ELSE.
    CLEAR w_charm_conex_failed.
    LOOP AT lt_ot INTO lwa_ot.
      r_order-sign = 'I'.
      r_order-option = 'EQ'.
      r_order-low = lwa_ot-trorder_number.
      APPEND r_order.
    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE i001(00) WITH
      'No TR selected via change number if change number was empty'.
    ENDIF.
  ENDIF.

ENDFORM.                    "get_tr_from_charm_change

*&---------------------------------------------------------------------
*
*&      Form  GET_CHARM_CHANGE_FROM_TR
*&---------------------------------------------------------------------
*
*  Get Charm change (in PSM) from TR number
*  Get external CR number stock in ChaRM at the same time
*----------------------------------------------------------------------
*
FORM get_charm_change_from_tr.

  DATA:
  lt_where   TYPE TABLE OF rfc_db_opt,
  lw_where   LIKE LINE OF lt_where,
  lt_fields  TYPE TABLE OF rfc_db_fld,
  lw_fields  LIKE LINE OF lt_fields,
  lt_ot      TYPE ty_t_ot,
  lwa_ot     LIKE LINE OF lt_ot,
  lt_cr_ext  TYPE ty_t_cr_ext,
  lwa_cr_ext LIKE LINE OF lt_cr_ext.

  DATA: lw_from TYPE i,
        lw_to   TYPE i,
        lw_inc  TYPE i VALUE 900.
*** WHERE condition.
  CHECK NOT t_e070v[]  IS INITIAL.

*Fields needed.
  lw_fields = c_fielname_trordhc-charm_change.
  APPEND lw_fields TO lt_fields.
  lw_fields = c_fielname_trordhc-order.
  APPEND lw_fields TO lt_fields.

* Made sélection by packet of 900 TR.
  DATA : l46_nbline TYPE i.
  DESCRIBE TABLE t_e070v LINES l46_nbline.
  WHILE lw_to < l46_nbline.
    CLEAR: lw_where, lt_where.
    lw_from = lw_to + 1.
    lw_to = lw_from + lw_inc.

    CONCATENATE c_fielname_trordhc-order ' in (' INTO lw_where.
    APPEND lw_where TO lt_where.

    LOOP AT t_e070v ASSIGNING <fs_e070v> FROM lw_from TO lw_to.

      AT LAST."Pas de virgule mais ')'.
        CONCATENATE '''' <fs_e070v>-trkorr ''')' INTO lw_where.
        APPEND lw_where TO lt_where.
        EXIT.
      ENDAT.

      IF sy-tabix = lw_to. "Pas de virgule mais ')'.
        CONCATENATE '''' <fs_e070v>-trkorr ''')' INTO lw_where.
        APPEND lw_where TO lt_where.
      ELSE.
        CONCATENATE '''' <fs_e070v>-trkorr ''',' INTO lw_where.
        APPEND lw_where TO lt_where.
      ENDIF.

    ENDLOOP.

*Call request in PSM system.
    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION c_charm_rfc_dest
      EXPORTING
        query_table           = '/TMWFLOW/TRORDHC'
*       DELIMITER             = ' '
*       NO_DATA               = ' '
*       ROWSKIPS              = 0
*       ROWCOUNT              = 0
      TABLES
        OPTIONS               = lt_where
        fields                = lt_fields
        data                  = lt_ot
      EXCEPTIONS
        table_not_available   = 1
        table_without_data    = 2
        option_not_valid      = 3
        field_not_valid       = 4
        not_authorized        = 5
        data_buffer_exceeded  = 6
        system_failure        = 7
        communication_failure = 8
        resource_failure      = 9
        OTHERS                = 10.

    IF sy-subrc <> 0.
      w_charm_conex_failed = sy-subrc.

    ELSE.
*     Get CR extern from change in table lt_cr_ext
      PERFORM get_cr_extern_from_change USING lt_ot
                                     CHANGING lt_cr_ext.


      CLEAR w_charm_conex_failed.
      LOOP AT t_e070v ASSIGNING <fs_e070v> FROM lw_from TO lw_to.
        READ TABLE lt_ot INTO lwa_ot WITH KEY trorder_number =
<fs_e070v>-trkorr.
        IF sy-subrc IS INITIAL.
          <fs_e070v>-charm_ch = lwa_ot-originator_key.

*          Get CR extern from change
          READ TABLE lt_cr_ext INTO lwa_cr_ext WITH KEY object_id =
  <fs_e070v>-charm_ch.
          IF sy-subrc IS INITIAL.
            <fs_e070v>-qc_number = lwa_cr_ext-zz_extern_cr.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDWHILE.
ENDFORM.                    "get_charm_change_from_tr

*&---------------------------------------------------------------------
*
*&      Form  GET_CR_EXTERN_FROM_CHANGE
*&---------------------------------------------------------------------
*
*   Get CR extern from change from table CRMD_ORDERADM_H
*----------------------------------------------------------------------
*
*      -->PT_OT  List of change
*----------------------------------------------------------------------
*
FORM get_cr_extern_from_change  USING pt_ot     TYPE ty_t_ot
                             CHANGING pt_cr_ext TYPE ty_t_cr_ext.

  DATA:
  lt_where   TYPE TABLE OF rfc_db_opt,
  lw_where   LIKE LINE OF lt_where,
  lt_fields  TYPE TABLE OF rfc_db_fld,
  lw_fields  LIKE LINE OF lt_fields.

  FIELD-SYMBOLS: <lwa_ot> LIKE LINE OF pt_ot.


* Selected fields
  CLEAR  lt_fields.
  lw_fields = c_fielname_crmd_orderadm_h-charm_change.
  APPEND lw_fields TO lt_fields.
  lw_fields = c_fielname_crmd_orderadm_h-extern_cr.
  APPEND lw_fields TO lt_fields.

* Where clause
  CONCATENATE c_fielname_crmd_orderadm_h-charm_change ' in (' INTO
lw_where.
  APPEND lw_where TO lt_where.

  CHECK NOT pt_ot IS INITIAL.
  LOOP AT pt_ot ASSIGNING <lwa_ot>.

    AT LAST."Pas de virgule mais ')'.
      CONCATENATE '''' <lwa_ot>-originator_key ''')' INTO lw_where.
      APPEND lw_where TO lt_where.
      EXIT.
    ENDAT.

    CONCATENATE '''' <lwa_ot>-originator_key ''',' INTO lw_where.
    APPEND lw_where TO lt_where.

  ENDLOOP.

  CALL FUNCTION 'RFC_READ_TABLE' DESTINATION c_charm_rfc_dest
    EXPORTING
      query_table           = 'CRMD_ORDERADM_H'
*     DELIMITER             = ' '
*     NO_DATA               = ' '
*     ROWSKIPS              = 0
*     ROWCOUNT              = 0
    TABLES
      OPTIONS               = lt_where
      fields                = lt_fields
      data                  = pt_cr_ext
    EXCEPTIONS
      table_not_available   = 1
      table_without_data    = 2
      option_not_valid      = 3
      field_not_valid       = 4
      not_authorized        = 5
      data_buffer_exceeded  = 6
      system_failure        = 7
      communication_failure = 8
      resource_failure      = 9
      OTHERS                = 10.
  IF sy-subrc <> 0.
    w_charm_conex_failed = sy-subrc.
  ENDIF.

ENDFORM.                    " GET_CR_EXTERN_FROM_CHANGE

**&--------------------------------------------------------------------
**
**&      Form  GET_CHARM_CHANGE_FROM_CR
**&--------------------------------------------------------------------
**
**  Get Charm change from CR in selection.
** Add change in change for selection.
**---------------------------------------------------------------------
**
**  In : CR list in selection screen r_qc_num
**  Out : Change to select (s_charm)
**---------------------------------------------------------------------
**
*FORM get_charm_change_from_cr .
*
*
*  DATA:
*  lt_where   TYPE TABLE OF rfc_db_opt,
*  lw_where   LIKE LINE OF lt_where,
*  lt_fields  TYPE TABLE OF rfc_db_fld,
*  lw_fields  LIKE LINE OF lt_fields,
*  lt_cr_ext  TYPE ty_t_cr_ext,
*  lwa_charm  LIKE LINE OF s_charm.
*
*  FIELD-SYMBOLS: <lwa_qc_num> LIKE LINE OF r_qc_num,
*                 <lwa_cr_ext> LIKE LINE OF lt_cr_ext.
*
*
** Selected fields
*  CLEAR  lt_fields.
*  lw_fields = c_fielname_crmd_orderadm_h-charm_change.
*  APPEND lw_fields TO lt_fields.
*  lw_fields = c_fielname_crmd_orderadm_h-extern_cr.
*  APPEND lw_fields TO lt_fields.
*
** Where clause
* CONCATENATE c_fielname_crmd_orderadm_h-extern_cr ' in ('
*INTO lw_where
*      .
*  APPEND lw_where TO lt_where.
*
*  CHECK r_qc_num[] IS NOT INITIAL.
*  LOOP AT r_qc_num ASSIGNING <lwa_qc_num>.
*
*    AT LAST."Pas de virgule mais ')'.
*      CONCATENATE `'` <lwa_qc_num>-low `')` INTO lw_where.
*      APPEND lw_where TO lt_where.
*      EXIT.
*    ENDAT.
*
*    CONCATENATE `'` <lwa_qc_num>-low `',` into lw_where.
*    APPEND lw_where TO lt_where.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'RFC_READ_TABLE' DESTINATION c_charm_rfc_dest
*    EXPORTING
*      query_table           = 'CRMD_ORDERADM_H'
**     DELIMITER             = ' '
**     NO_DATA               = ' '
**     ROWSKIPS              = 0
**     ROWCOUNT              = 0
*    TABLES
*      options               = lt_where
*      fields                = lt_fields
*      data                  = lt_cr_ext
*    EXCEPTIONS
*      table_not_available   = 1
*      table_without_data    = 2
*      option_not_valid      = 3
*      field_not_valid       = 4
*      not_authorized        = 5
*      data_buffer_exceeded  = 6
*      system_failure        = 7
*      communication_failure = 8
*      RESOURCE_FAILURE      = 9
*      OTHERS                = 10.
*  IF sy-subrc <> 0.
*    w_charm_conex_failed = sy-subrc.
*  ELSE.
*    lwa_charm-sign = 'I'.
*    lwa_charm-option = 'EQ'.
*    LOOP AT lt_cr_ext ASSIGNING <lwa_cr_ext>.
*      CLEAR lwa_charm-low.
*      lwa_charm-low = <lwa_cr_ext>-object_id.
*      APPEND lwa_charm TO s_charm.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    " GET_CHARM_CHANGE_FROM_CR
*
*&--------------------------------------------------------------------*
*&      Form  F_LIMIT_SELECTOPTION
*&--------------------------------------------------------------------*
*   Personnalisation of select-option
*---------------------------------------------------------------------*
FORM f_limit_selectoption .
  TYPE-POOLS : sscr.

* Define the object to be passed to the RESTRICTION parameter
  DATA : restrict TYPE sscr_restrict,
* Auxiliary objects for filling RESTRICT
        opt_list TYPE sscr_opt_list,
        ass      TYPE sscr_ass.

* NOINTERVLS: BT and NB not allowed
  CLEAR opt_list.
  MOVE 'ONLY_EQUAL_LIST' TO opt_list-name.
  MOVE abap_true TO: opt_list-options-eq.

  APPEND opt_list TO restrict-opt_list_tab.

* Limitation de la recherche du poste de travail sans intervalle.
* On enève aussi la possibilité des CP
  CLEAR ass.
  MOVE: 'S'          TO ass-kind,
        'S_CHARM'    TO ass-name,
        'I'          TO ass-sg_main,
        ' '          TO ass-sg_addy,
        'ONLY_EQUAL_LIST' TO ass-op_main,
        'ONLY_EQUAL_LIST' TO ass-op_addy.
  APPEND ass TO restrict-ass_tab.

  CLEAR ass.
  MOVE: 'S'          TO ass-kind,
        'R_QC_NUM'   TO ass-name,
        'I'          TO ass-sg_main,
        ' '          TO ass-sg_addy,
        'ONLY_EQUAL_LIST' TO ass-op_main,
        'ONLY_EQUAL_LIST' TO ass-op_addy.
  APPEND ass TO restrict-ass_tab.

* Call function module
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_LIMIT_SELECTOPTION
*-- ChaRM -->

*Text elements
*----------------------------------------------------------
* 001 Workbench
* 002 Customizing
* 003 Type
* 004 Actual Environment
* 005 PE1
* 006 IE1
* 007 DE1
* 008 Transport Date
* 009 IEM
* 010 QEA
* 011 QEM
* 012 Tr. of copies
* 013 Do not display empty requests
* 015 PEM
* 016 IEM
* 017 DEM
* 020 QEM
* 021 Object
* 022 No data selected
* 023 Order tasks
* 024 Modified by
* 025 Repairs Only
* 026 Del. from buffer
* 027 Tr. without log
* 028 VE1
* 029 Additional options
* 030 VE2
* 031 VE3
* 032 VE4
* 033 DE2
* 050 FGI
* 100 Transport Requests overview
* 101 CGI
* 102 GGI


*Selection texts
*----------------------------------------------------------
* OBJECTA         Object
* OBJNAMEA         Obj. Name
* OBJTEXTA         Obj. Text
* PGMIDA         Pgm. Id
* P_BUFF         Del. From Buffer
* P_COPY         Copy
* P_CUSTO         Custo
* P_DE1         DE1
* P_DE2         DE2
* P_DE2DAT         In DE2
* P_DEV         Dev.
* P_IE1         IE1
* P_IE1DAT         in IE1
* P_LKTR         TR linked
* P_PE1         PE1
* P_PE1DAT         In PE1
* P_REPAI         Repairs Only
* P_VE1         VE1
* P_VE1DAT         in VE1
* P_VE2         VE2
* P_VE2DAT         in VE2
* P_VE3         VE3
* P_VE3DAT         in VE3
* P_VE4         ve4
* P_VE4DAT         in ve4
* P_WORKB         Workbench
* P_WT_LOG         without log
* R_CLIENT         Source client
* R_CTS         CTS
* R_DATE         Change date
* R_ORDER         Request/Task
* R_OWNER         Owner
* R_PROJET         Search Text
* R_QC_NUM         QC Number
* R_SP_NUM         SP Number
* S_CHARM         Change Number (ChaRM)
* S_LKTROB


*Messages
*----------------------------------------------------------
*
* Message class: 00
* 001 &1&2&3&4&5&6&7&8
*
* Message class: MG
* 899 & & & &
*
* Message class: TK
* 870 Sélectionnez un type d'objet valide (uniq. objets avec poss. de blocage)

----------------------------------------------------------------------

