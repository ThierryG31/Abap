*&---------------------------------------------------------------------*
*&  Include           ZPU580MIGFILE_SEL
*&---------------------------------------------------------------------*


************************************************************************
*                          Selection Screen                            *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.

* Radiobuttons
PARAMETERS: p_upl RADIOBUTTON GROUP gp1 DEFAULT 'X'
  USER-COMMAND rdb1.
PARAMETERS: p_downl RADIOBUTTON GROUP gp1.
PARAMETERS: p_delet RADIOBUTTON GROUP gp1.
PARAMETERS: p_flist RADIOBUTTON GROUP gp1.
PARAMETERS: p_multi RADIOBUTTON GROUP gp1.

SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.

* Front-end file
SELECTION-SCREEN COMMENT 1(30) text-f04.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 8(14) text-f01.
SELECTION-SCREEN POSITION 30.
PARAMETERS: p_lfile TYPE  rlgrap-filename. " OBLIGATORY. " DEFAULT '*.csv'.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_locrep TYPE ty_locrep
                          MODIF ID mul.
PARAMETERS: p_files  TYPE ty_files  default 'MIGR_U580*'
                          MODIF ID mul.


SELECTION-SCREEN SKIP 2.

* SAP Application Server file
SELECTION-SCREEN COMMENT 1(30) text-f05.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 8(14) text-f02.
SELECTION-SCREEN POSITION 30.
PARAMETERS: p_sfile TYPE filename-fileintern.
*parameters: p_sfile type filepath-pathintern.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 30(75) p_dsname.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 8(14) text-f03.
SELECTION-SCREEN POSITION 30.
PARAMETERS: p_sfilx TYPE rcgfiletr-ftappl. " OBLIGATORY. "filename-fileextern. "DEFAULT 'u580_dg7_s_g7gam.csv'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b03.

* Options
SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b04.
PARAMETERS: p_ftype TYPE rcgfiletr-ftftype OBLIGATORY DEFAULT 'BIN'.
PARAMETERS: p_replac TYPE rcgfiletr-iefow.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN END OF BLOCK b01.