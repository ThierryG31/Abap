PROGRAM zpu580_prog_download.
*program zdown.
*=======================================================================
*  Direct Download 46cd version 1.3.2 release.
*
*  THIS SOFTWARE IS FOR PERSONAL USE ONLY.
*  THIS PROGRAM IS FREEWARE AND IS PROVIDED ON AN AS-IS BASIS WITHOUT
*  WARRANTY OF ANY KIND.
*  THE PROVIDER SPECIFICALLY DISCLAIMS ANY OTHER WARRANTY, EXPRESS OR
*  IMPLIED, INCLUDING ANY WARRANTY OF
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
*
*  IN NO EVENT SHALL PROVIDER BE LIABLE FOR ANY CONSEQUENTIAL, INDIRECT,
*  SPECIAL OR INCIDENTAL DAMAGES,
*  EVEN IF PROVIDER HAS BEEN ADVISED BY CLIENT OF THE POSSIBILITY OF
*  SUCH POTENTIAL LOSS OR DAMAGE.
*  CLIENT AGREES TO HOLD PROVIDER HARMLESS FROM AND AGAINST ANY AND ALL
*  CLAIMS, LOSSES, LIABILITIES AND
*  EXPENSES.  BY INSTALLING OR RUNNING THIS PROGRAM YOU ARE AGREEING TO
*  THE TERMS AND CONDITONS STATED
*  ABOVE.
*
*  ----------------------------------------------------------------
*  PROGRAM DESCRIPTION & USE
*  Allows a user to download ABAPs, Functions DD definitions, etc to
*  the presentation server.  This version searches recursively for
*  nested includes and function modules, and allows you to download
*  the resulting code as standard text or HTML web pages within a
*  suitable directory structure.
*
*  You can either search by object name, using wildcards if you wish, or
*  a combination of Author and object name.  If you want all objects
*  returned for a particular author then select the author name and
*  choose the most suitable radiobutton.  All objects will be returned
*  if the fields to the right hand side of the radiobutton are left
*  completely blank.
*
*  Compatible with R/3 versions 4.6c and 4.6d
*
*  In order to be able to download files to the SAP server you must
*  first set up a logical filepath within transaction
*  'FILE', or use an existing one.  You must also create a external
*  operating system command in SM69 called ZMKDIR. This
*  will then be used to create any directories needed on the SAP server

*  This program is intended to allow a person to keep a visual
*  representation of a program for backup purposes only as
*  has not been designed to allow programs to be uploaded to SAP systems

*  ----------------------------------------------------------------

* PROGRAM HISTORY
* ---------------
* 1.3  Program backported from DDE version 1.3.  Version numbers brought
*      into line with DDE.
* 0.70 A version specifically for 4.6c & 4.6d back ported from Direct
*        Download Enteprise.  More reliable and more features than the
*        previous releases of direct download.
*
* AUTHOR          : E.G.Mellodew
*
*
* PROGRAM CONTACT : direct@dalestech.com
*                   www.dalestech.com
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*  SAP Tables
*-----------------------------------------------------------------------
TABLES: trdir, seoclass, tfdir, enlfdir, dd02l, usr02.

*-----------------------------------------------------------------------
*  Types
*-----------------------------------------------------------------------
* text element structure
TYPES: ttexttable LIKE textpool.
* GUI titles
TYPES: tguititle LIKE d347t.

* Message classes
TYPES: BEGIN OF tmessage,
         arbgb LIKE t100-arbgb,
         stext LIKE t100a-stext,
         msgnr LIKE t100-msgnr,
         text  LIKE t100-text,
       END OF tmessage.

* Screen flow.
TYPES: BEGIN OF tscreenflow,
         screen LIKE d020s-dnum,
         code LIKE d022s-line,
       END OF tscreenflow.

* Holds a table\structure definition
TYPES: BEGIN OF tdicttablestructure,
         fieldname LIKE dd03l-fieldname,
         position  LIKE dd03l-position,
         keyflag   LIKE dd03l-keyflag,
         rollname  LIKE dd03l-rollname,
         domname   LIKE dd03l-domname,
         datatype  LIKE dd03l-datatype,
         leng      LIKE dd03l-leng,
         ddtext    LIKE dd04t-ddtext,
       END OF tdicttablestructure.

* Holds a tables attributes + its definition
TYPES: BEGIN OF tdicttable,
         tablename    LIKE dd03l-tabname,
         tabletitle   LIKE dd02t-ddtext,
         istructure TYPE tdicttablestructure OCCURS 0,
       END OF tdicttable.

* Include program names
TYPES: BEGIN OF tinclude,
         includename LIKE trdir-name,
         includetitle LIKE tftit-stext,
       END OF tinclude.

* Exception class texts
TYPES: BEGIN OF tconcept,
         constname TYPE string,
         concept TYPE sotr_conc,
       END OF tconcept.

* Method
TYPES: BEGIN OF tmethod,
         cmpname LIKE vseomethod-cmpname,
         descript LIKE vseomethod-descript,
         exposure LIKE vseomethod-exposure,
         methodkey TYPE string,
       END OF tmethod.

* function modules
TYPES: BEGIN OF tfunction,
         functionname LIKE tfdir-funcname,
         functiongroup LIKE enlfdir-area,
         includenumber LIKE tfdir-include,
         functionmaininclude LIKE tfdir-funcname,
         functiontitle LIKE tftit-stext,
         topincludename LIKE tfdir-funcname,
         progname LIKE tfdir-pname,
         programlinkname LIKE tfdir-pname,
         messageclass LIKE t100-arbgb,
         itextelements TYPE ttexttable OCCURS 0,
         iselectiontexts TYPE ttexttable OCCURS 0,
         imessages TYPE tmessage OCCURS 0,
         iincludes TYPE tinclude OCCURS 0,
         idictstruct TYPE tdicttable OCCURS 0,
         iguititle TYPE tguititle OCCURS 0,
         iscreenflow TYPE tscreenflow OCCURS 0,
       END OF tfunction.

TYPES: BEGIN OF tprogram,
         prog LIKE trdir-name,
         programtitle LIKE tftit-stext,
         subc LIKE trdir-subc,
         cdat  LIKE d010sinf-cdat,
         udat  LIKE d010sinf-udat,
         messageclass LIKE t100-arbgb,
         imessages TYPE tmessage OCCURS 0,
         itextelements TYPE ttexttable OCCURS 0,
         iselectiontexts TYPE ttexttable OCCURS 0,
         iguititle TYPE tguititle OCCURS 0,
         iscreenflow TYPE tscreenflow OCCURS 0,
         iincludes TYPE tinclude OCCURS 0,
         idictstruct TYPE tdicttable OCCURS 0,
       END OF tprogram.

*-----------------------------------------------------------------------
*  Internal tables
*-----------------------------------------------------------------------
*  Dictionary object
DATA: idictionary TYPE STANDARD TABLE OF tdicttable WITH HEADER LINE.
* Function modules.
DATA: ifunctions TYPE STANDARD TABLE OF tfunction WITH HEADER LINE.
* Tree display structure.
DATA: itreedisplay TYPE STANDARD TABLE OF snodetext WITH HEADER LINE.
* Message class data
DATA: imessages TYPE STANDARD TABLE OF tmessage WITH HEADER LINE.
* Holds a single message class an all of its messages
DATA: isinglemessageclass TYPE STANDARD TABLE OF tmessage WITH HEADER
LINE.
* Holds program related data
DATA: iprograms TYPE STANDARD TABLE OF tprogram WITH HEADER LINE.
* Table of paths created on the SAP server
DATA: iserverpaths TYPE STANDARD TABLE OF string WITH HEADER LINE.

*-----------------------------------------------------------------------
*  Table prototypes
*-----------------------------------------------------------------------
DATA: dumidictstructure TYPE STANDARD TABLE OF tdicttablestructure.
DATA: dumitexttab TYPE STANDARD TABLE OF ttexttable.
DATA: dumiincludes TYPE STANDARD TABLE OF tinclude.
DATA: dumihtml TYPE STANDARD TABLE OF string.
DATA: dumiheader TYPE STANDARD TABLE OF string.
DATA: dumiscreen TYPE STANDARD TABLE OF tscreenflow .
DATA: dumiguititle TYPE STANDARD TABLE OF tguititle.
DATA: dumimethods TYPE STANDARD TABLE OF tmethod.
DATA: dumiconcepts TYPE STANDARD TABLE OF tconcept.

*-----------------------------------------------------------------------
*   Global objects
*-----------------------------------------------------------------------
DATA: objfile TYPE REF TO cl_gui_frontend_services.

*-----------------------------------------------------------------------
*  Constants
*-----------------------------------------------------------------------
CONSTANTS: versionno(5) VALUE '1.3.1'.
CONSTANTS: tables(6) VALUE 'TABLES'.
CONSTANTS: table(5) VALUE 'TABLE'.
CONSTANTS: like(4) VALUE 'LIKE'.
CONSTANTS: type(4) VALUE 'TYPE'.
CONSTANTS: typerefto(11) VALUE 'TYPE REF TO'.
CONSTANTS: structure(9) VALUE 'STRUCTURE'.
CONSTANTS: lowstructure(9) VALUE 'structure'.
CONSTANTS: occurs(6) VALUE 'OCCURS'.
CONSTANTS: function(8) VALUE 'FUNCTION'.
CONSTANTS: callfunction(14) VALUE ' CALL FUNCTION'.
CONSTANTS: message(7) VALUE 'MESSAGE'.
CONSTANTS: include(7) VALUE 'INCLUDE'.
CONSTANTS: lowinclude(7) VALUE 'include'.
CONSTANTS: destination(11) VALUE 'DESTINATION'.
CONSTANTS: is_table(1) VALUE 'T'.
CONSTANTS: is_program(1) VALUE 'P'.
CONSTANTS: is_screen(1) VALUE 'S'.
CONSTANTS: is_guititle(1) VALUE 'G'.
CONSTANTS: is_documentation(1) VALUE 'D'.
CONSTANTS: is_messageclass(2) VALUE 'MC'.
CONSTANTS: is_function(1) VALUE 'F'.
CONSTANTS: is_class(1) VALUE 'C'.
CONSTANTS: is_method(1) VALUE 'M'.
CONSTANTS: asterix(1) VALUE '*'.
CONSTANTS: comma(1) VALUE ','.
CONSTANTS: period(1) VALUE '.'.
CONSTANTS: dash(1) VALUE '-'.
CONSTANTS: true TYPE i VALUE 1.
CONSTANTS: false TYPE i VALUE 0.
CONSTANTS: lt(4) VALUE '&lt;'.
CONSTANTS: gt(4) VALUE '&gt;'.
CONSTANTS: unix(4) VALUE 'UNIX'.
CONSTANTS: non_unix(8) VALUE 'not UNIX'.
CONSTANTS: background_colour(7) VALUE '#FFFFE0'.
CONSTANTS: colour_white(7) VALUE '#FFFFFF'.
CONSTANTS: colour_black(7) VALUE '#000000'.
CONSTANTS: colour_yellow(7) VALUE '#FFFF00'.
CONSTANTS: comment_colour(7) VALUE '#0000FF'.
CONSTANTS: htmlextension(4) VALUE 'html'.
CONSTANTS: textextension(3) VALUE 'txt'.

*-----------------------------------------------------------------------
*  Global variables
*-----------------------------------------------------------------------
DATA: statusbarmessage(100).
DATA: forcedexit TYPE i VALUE 0.
DATA: starttime LIKE sy-uzeit.
DATA: runtime LIKE sy-uzeit.
DATA: downloadfileextension TYPE string.
DATA: downloadfolder TYPE string.
DATA: serverslashseparator TYPE string.
DATA: frontendslashseparator TYPE string.
DATA: slashseparatortouse TYPE string.
DATA: serverfilesystem TYPE filesys_d.
DATA: serverfolder TYPE string.
DATA: frontendopsystem TYPE string.
DATA: serveropsystem TYPE string.
DATA: customernamespace TYPE string.
RANGES: soprogramname FOR trdir-name.
*RANGES: soauthor FOR usr02-bname.
RANGES: sotablenames FOR dd02l-tabname.
RANGES: sofunctionname  FOR tfdir-funcname.
RANGES: soclassname FOR vseoclass-clsname.
RANGES: sofunctiongroup FOR enlfdir-area.
FIELD-SYMBOLS: <wadictstruct> TYPE tdicttable.

*-----------------------------------------------------------------------
*  Selection screen declaration
*-----------------------------------------------------------------------
* Author
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE tblock1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(23) tauth.
SELECT-OPTIONS: soauthor FOR usr02-bname. " MEMORY ID mauth.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(36) tpmod.
PARAMETERS: pmod AS CHECKBOX.
SELECTION-SCREEN END OF LINE.

* Local objects
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(36) t$tmp.
PARAMETERS: p$tmp AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE tblock2.
* Tables
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rtable RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 5(15) trtable.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(15) tptable.
SELECT-OPTIONS: sotable FOR dd02l-tabname.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(79) ttnote.
SELECTION-SCREEN END OF LINE.

* Message classes
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rmess RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 5(18) tpmes.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(18) tmname.
PARAMETERS: pmname LIKE t100-arbgb MEMORY ID mmname.
SELECTION-SCREEN END OF LINE.

* Function modules
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rfunc RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 5(30) trfunc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(15) tpfname.
SELECT-OPTIONS: sofname FOR tfdir-funcname.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(15) tfgroup.
SELECT-OPTIONS: sofgroup FOR enlfdir-area.
SELECTION-SCREEN END OF LINE.

* Programs / includes
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rprog RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 5(18) tprog.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(15) trpname.
SELECT-OPTIONS: soprog FOR trdir-name.
SELECTION-SCREEN END OF LINE.
PARAMETERS: pudat LIKE d010sinf-udat.

SELECTION-SCREEN SKIP.
* Language
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) tmlang.
PARAMETERS: pmlang LIKE t100-sprsl DEFAULT 'FR'.
SELECTION-SCREEN END OF LINE.

* Package
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) tpack.
PARAMETERS: ppack LIKE tadiv-devclass MEMORY ID mpack.
SELECTION-SCREEN END OF LINE.

* Customer objects
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(27) tcust.
PARAMETERS: pcust AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 32(25) tnrange.
PARAMETERS: pcname TYPE namespace MEMORY ID mnamespace.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.

* Additional things to download.
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE tblock3.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tptext.
PARAMETERS: ptext AS CHECKBOX DEFAULT 'X' MEMORY ID mtext.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tmess.
PARAMETERS: pmess AS CHECKBOX DEFAULT 'X' MEMORY ID mmess.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tpinc.
PARAMETERS: pinc AS CHECKBOX DEFAULT 'X' MEMORY ID minc.
SELECTION-SCREEN COMMENT 40(20) trecc.
PARAMETERS: preci AS CHECKBOX DEFAULT 'X' MEMORY ID mreci.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tpfunc.
PARAMETERS: pfunc AS CHECKBOX DEFAULT 'X' MEMORY ID mfunc.
SELECTION-SCREEN COMMENT 40(20) trecf.
PARAMETERS: precf AS CHECKBOX DEFAULT 'X' MEMORY ID mrecf.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tdoc.
PARAMETERS: pdoc AS CHECKBOX DEFAULT 'X' MEMORY ID mdoc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tpscr.
PARAMETERS: pscr AS CHECKBOX DEFAULT 'X' MEMORY ID mscr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tpdict.
PARAMETERS: pdict AS CHECKBOX DEFAULT 'X' MEMORY ID mdict.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tsortt.
PARAMETERS: psortt AS CHECKBOX DEFAULT ' ' MEMORY ID msortt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.

* File details
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE tblock4.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) tphtml.
PARAMETERS: phtml RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(29) tcomm.
PARAMETERS: pcomm AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(29) tback.
PARAMETERS: pback AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) tptxt.
PARAMETERS: ptxt RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

** Download to SAP server
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(25) tserv.
*PARAMETERS: pserv RADIOBUTTON GROUP g2.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 8(20) tspath.
*PARAMETERS: plogical LIKE filename-fileintern MEMORY ID mlogical.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN COMMENT /28(60) tsdpath.

* Download to PC
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) tpc.
*PARAMETERS: ppc RADIOBUTTON GROUP g2 DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 8(20) tppath.
PARAMETERS: pfolder LIKE rlgrap-filename MEMORY ID mfolder.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b4.

* Display options
SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE tblock5.
* Display final report
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) trep.
PARAMETERS: prep AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
* Display progress messages
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) tpromess.
PARAMETERS: ppromess AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b5.

*-----------------------------------------------------------------------
* Display a directory picker window
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pfolder.

  DATA: objfile TYPE REF TO cl_gui_frontend_services.
  DATA: pickedfolder TYPE string.
  DATA: initialfolder TYPE string.

  IF sy-batch IS INITIAL.
    CREATE OBJECT objfile.

    IF NOT pfolder IS INITIAL.
      initialfolder = pfolder.
    ELSE.
      CALL METHOD objfile->get_temp_directory
        CHANGING
          temp_dir     = initialfolder
        EXCEPTIONS
          cntl_error   = 1
          error_no_gui = 2.
    ENDIF.

    CALL METHOD objfile->directory_browse
      EXPORTING
        initial_folder  = initialfolder
      CHANGING
        selected_folder = pickedfolder
      EXCEPTIONS
        cntl_error      = 1
        error_no_gui    = 2.

    CALL METHOD cl_gui_cfw=>flush.

    IF sy-subrc = 0.
      pfolder = pickedfolder.
    ELSE.
      WRITE: / 'An error has occured picking a folder'.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
*-----------------------------------------------------------------------
*  CASE 'X'.
*    WHEN ppc.
  IF pfolder IS INITIAL.
*       User must enter a path to save to
    MESSAGE e000(oo) WITH 'You must enter a file path'.
  ENDIF.
*
*    WHEN pserv.
*      IF plogical IS INITIAL.
**       User must enter a logical path to save to
*        MESSAGE e000(oo) WITH 'You must enter a logical file name'.
*      ENDIF.
*  ENDCASE.

*-----------------------------------------------------------------------
*AT SELECTION-SCREEN ON plogical.
*-----------------------------------------------------------------------
*  IF NOT pserv IS INITIAL.
*    CALL FUNCTION 'FILE_GET_NAME'
*         EXPORTING
*              logical_filename = plogical
*         IMPORTING
*              file_name        = serverfolder
*         EXCEPTIONS
*              file_not_found   = 1
*              OTHERS           = 2.
*    IF sy-subrc = 0.
*      IF serverfolder IS INITIAL.
*        MESSAGE e000(oo)
*                WITH 'No file path returned from logical filename'.
*      ELSE.
**       Path to display on the selection screen
*        tsdpath = serverfolder.
**       Remove the trailing slash off the path as the subroutine
**       buildFilename will add an extra one
*        SHIFT serverfolder RIGHT DELETING TRAILING serverslashseparator
*.
*        SHIFT serverfolder LEFT DELETING LEADING space.
*      ENDIF.
*    ELSE.
*      MESSAGE e000(oo) WITH 'Logical filename does not exist'.
*    ENDIF.
*  ENDIF.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR soprog-low.
*-----------------------------------------------------------------------
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = 'PROG'
      object_name           = soprog-low
      suppress_selection    = 'X'
      use_alv_grid          = ''
      without_personal_list = ''
    IMPORTING
      object_name_selected  = soprog-low
    EXCEPTIONS
      cancel                = 1.

* ----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR soprog-high.
*-----------------------------------------------------------------------
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = 'PROG'
      object_name           = soprog-high
      suppress_selection    = 'X'
      use_alv_grid          = ''
      without_personal_list = ''
    IMPORTING
      object_name_selected  = soprog-high
    EXCEPTIONS
      cancel                = 1.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR sofname-low.
*-----------------------------------------------------------------------
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = 'FUNC'
      object_name           = sofname-low
      suppress_selection    = 'X'
      use_alv_grid          = ''
      without_personal_list = ''
    IMPORTING
      object_name_selected  = sofname-low
    EXCEPTIONS
      cancel                = 1.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR sofname-high.
*-----------------------------------------------------------------------
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = 'FUNC'
      object_name           = sofname-high
      suppress_selection    = 'X'
      use_alv_grid          = ''
      without_personal_list = ''
    IMPORTING
      object_name_selected  = sofname-high
    EXCEPTIONS
      cancel                = 1.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR sofgroup-low.
*-----------------------------------------------------------------------
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = 'FUGR'
      object_name           = sofgroup-low
      suppress_selection    = 'X'
      use_alv_grid          = ''
      without_personal_list = ''
    IMPORTING
      object_name_selected  = sofgroup-low
    EXCEPTIONS
      cancel                = 1.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR sofgroup-high.
*-----------------------------------------------------------------------
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = 'FUGR'
      object_name           = sofgroup-high
      suppress_selection    = 'X'
      use_alv_grid          = ''
      without_personal_list = ''
    IMPORTING
      object_name_selected  = sofgroup-high
    EXCEPTIONS
      cancel                = 1.

*-----------------------------------------------------------------------
* initialisation
*-----------------------------------------------------------------------
INITIALIZATION.
* Parameter screen texts.
  tblock1 = 'Author (Optional)'.
  t$tmp   = 'Programs only: include local objects'.
  tblock2 = 'Objects to download'.
  tblock3 =
    'Additional downloads for programs, function modules and classes'.
  tblock4 = 'Download parameters'.
  tblock5 = 'Display options'.
  tauth   = 'Author name'.
  tpmod   = 'Include programs modified by author'.
  tcust   = 'Only customer objects'.
  tnrange = 'Alt customer name range'.
  trtable = 'Tables / Structures'.
  tptable = 'Table name'.
  trfunc  = 'Function modules'.
  tpfname = 'Function name'.
  tfgroup = 'Function group'.
  tmess   = 'Message class'.
  tmname  = 'Class name'.
  tmlang  = 'Language'.
  tprog   = 'Programs'.
  trpname = 'Program name'.
  tpack   = 'Dev class'.
  tptxt   = 'Text document'.
  tphtml  = 'HTML document'.
  tcomm   = 'Highlight comments'.
  tback   = 'Include background colour'.
  tptext  = 'Text elements'.
  tpinc   = 'Include programs'.
  trecc   = 'Recursive search'.
  tppath  = 'File path'.
*  tspath  = 'Logical file name'.
  tpmes   = 'Message classes'.
  tpfunc  = 'Function modules'.
  tdoc    = 'Function module documentation'.
  trecf   = 'Recursive search'.
  tpscr   = 'Screens'.
  tpdict  = 'Dictionary structures'.
  tsortt  = 'Sort table fields alphabetically'.
*  tserv   = 'Download to server'.
  tpc     = 'Download to PC'.
  trep    = 'Display download report'.
  tpromess  = 'Display progress messages'.

* Determine the frontend operating system type.
  IF sy-batch IS INITIAL.
    PERFORM determinefrontendopsystem USING frontendslashseparator
                                            frontendopsystem.
  ENDIF.
  PERFORM determineserveropsystem USING serverslashseparator
                                        serverfilesystem
                                        serveropsystem.

* Determine if the external command exists.  If it doesn't then disable
* the server input field
*  PERFORM findexternalcommand.

*-----------------------------------------------------------------------
* start-of-selection.
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM checkcomboboxes.
  PERFORM fillselectionranges.
  starttime = sy-uzeit.

* Don't display status messages if we are running in the background
  IF NOT sy-batch IS INITIAL.
    ppromess = ''.
  ENDIF.

* Fool the HTML routines to stop them hyperlinking anything with a space
* in them
  IF pcname IS INITIAL.
    customernamespace  = '^'.
  ELSE.
    customernamespace = pcname.
  ENDIF.

** Determine which operating slash and download directory to use
*  CASE 'X'.
*    WHEN ppc.
  slashseparatortouse = frontendslashseparator.
  downloadfolder = pfolder.
*    WHEN pserv.
*      slashseparatortouse = serverslashseparator.
*      downloadfolder = serverfolder.
*  ENDCASE.

* Main program flow.
  CASE 'X'.
*   Select tables
    WHEN rtable.
      PERFORM retrievetables USING idictionary[]
                                   sotablenames[]
                                   soauthor[].

*   Select message classes tables
    WHEN rmess.
      PERFORM retrievemessageclass
                 USING imessages[]
                 soauthor[]      "Author
                 pmname          "Message class name
                 pmlang          "Message class language
                 pmod.           "Modified by author

*   Select function modules
    WHEN rfunc.
      PERFORM retrievefunctions
           USING sofunctionname[]   "Function name
                 sofunctiongroup[]  "Function group
                 ifunctions[]       "Found functions
                 soauthor[]         "Author
                 ptext              "Get text elements
                 pscr               "Get screens
                 pcust              "Customer data only
                 customernamespace. "Customer name range


      LOOP AT ifunctions.
*       Find Dict structures, messages, functions, includes etc.
        PERFORM scanforadditionalfuncstuff
                 USING ifunctions[]
                       preci       "Search for includes recursively
                       precf       "Search for functions recursively
                       pinc        "Search for includes
                       pfunc       "Search for functions
                       pdict       "search for dictionary objects
                       pmess       "Search for messages
                       pcust       "Customer data only
                       customernamespace.      "Customer name range
      ENDLOOP.

*   Select programs
    WHEN rprog.
      PERFORM retrieveprograms
                  USING iprograms[]
                        ifunctions[]
                        soprogramname[]    "Program name
                        soauthor[]         "Author
                        customernamespace  "Customer name range
                        pmod               "Also modifiedby author
                        pcust              "Customer object only
                        pmess              "Find messages
                        ptext              "Text Elements
                        pdict              "Dictionay structures
                        pfunc              "Get functions
                        pinc               "Get includes
                        pscr               "Get screens
                        precf              "recursively for functions
                        preci              "recursively for includes
                        p$tmp              "local objects
                        ppack.             "Package
  ENDCASE.

*-----------------------------------------------------------------------
* end-of-selection
*-----------------------------------------------------------------------
END-OF-SELECTION.

  IF forcedexit = 0.
*   Set the file extension and output type of the file
    IF ptxt IS INITIAL.
      downloadfileextension = htmlextension.
    ELSE.
      downloadfileextension = textextension.
    ENDIF.

*   Decide what to download
    CASE 'X'.
*     Download tables
      WHEN rtable.
        IF NOT ( idictionary[] IS INITIAL ).
          PERFORM downloadddstructures USING idictionary[]
                                             downloadfolder
                                             htmlextension
                                             space
                                             psortt
                                             slashseparatortouse
*                                             pserv
                                             ppromess.

*         Free up any memory used for caching HTML versions of tables
          LOOP AT idictionary.
            FREE MEMORY ID idictionary-tablename.
          ENDLOOP.

*         Display donwload report
          IF NOT prep IS INITIAL.
            GET TIME.
            runtime = sy-uzeit - starttime.
            PERFORM filltreenodetables USING idictionary[]
                                             itreedisplay[]
                                             runtime.
          ENDIF.

          CLEAR idictionary[].
        ENDIF.

*     Download message class
      WHEN rmess.
        IF NOT ( imessages[] IS INITIAL ).
          SORT imessages ASCENDING BY arbgb msgnr.
          LOOP AT imessages.
            APPEND imessages TO isinglemessageclass.
            AT END OF arbgb.
              PERFORM downloadmessageclass USING isinglemessageclass[]
                                                 imessages-arbgb
                                                 downloadfolder
                                                 downloadfileextension
                                                 phtml
                                                 space
                                                 pcomm
                                                 customernamespace
                                                 pinc
                                                 pdict
                                                 pmess
                                                 slashseparatortouse
*                                                 pserv
                                                 ppromess.
              CLEAR isinglemessageclass[].
            ENDAT.
          ENDLOOP.

*         Display download report
          IF NOT prep IS INITIAL.
            GET TIME.
            runtime = sy-uzeit - starttime.
            PERFORM filltreenodemessages USING imessages[]
                                               itreedisplay[]
                                               runtime.
          ENDIF.

          CLEAR imessages[].
        ENDIF.

*     Download functions
      WHEN rfunc.
        IF NOT ( ifunctions[] IS INITIAL ).
          PERFORM downloadfunctions USING ifunctions[]
                                          downloadfolder
                                          downloadfileextension
                                          space
                                          pdoc
                                          phtml
                                          pcomm
                                          customernamespace
                                          pinc
                                          pdict
                                          textextension
                                          htmlextension
                                          psortt
                                          slashseparatortouse
*                                          pserv
                                          ppromess.

*         Free up any memory used for caching HTML versions of tables
          LOOP AT ifunctions.
            LOOP AT ifunctions-idictstruct ASSIGNING <wadictstruct>.
              FREE MEMORY ID <wadictstruct>-tablename.
            ENDLOOP.
          ENDLOOP.

*         Display donwload report
          IF NOT prep IS INITIAL.
            GET TIME.
            runtime = sy-uzeit - starttime.
            PERFORM filltreenodefunctions USING ifunctions[]
                                                itreedisplay[]
                                                runtime.
          ENDIF.

          CLEAR ifunctions[].
        ENDIF.


*     Download programs
      WHEN rprog.
        IF NOT ( iprograms[] IS INITIAL ).
          PERFORM downloadprograms USING iprograms[]
                                         ifunctions[]
                                         downloadfolder
                                         downloadfileextension
                                         htmlextension
                                         textextension
                                         phtml
                                         pcomm
                                         customernamespace
                                         pinc
                                         pdict
                                         pdoc
                                         psortt
                                         slashseparatortouse
*                                         pserv
                                         ppromess.

*         Free up any memory used for caching HTML versions of tables
          LOOP AT ifunctions.
            LOOP AT ifunctions-idictstruct ASSIGNING <wadictstruct>.
              FREE MEMORY ID <wadictstruct>-tablename.
            ENDLOOP.
          ENDLOOP.

*         Free up any memory used for caching HTML versions of tables
          LOOP AT iprograms.
            LOOP AT iprograms-idictstruct ASSIGNING <wadictstruct>.
              FREE MEMORY ID <wadictstruct>-tablename.
            ENDLOOP.
          ENDLOOP.

*         Display donwload report
          IF NOT prep IS INITIAL.
            GET TIME.
            runtime = sy-uzeit - starttime.
            PERFORM filltreenodeprograms USING iprograms[]
                                               ifunctions[]
                                               itreedisplay[]
                                               runtime.
          ENDIF.

          CLEAR iprograms[].
          CLEAR ifunctions[].
        ENDIF.
    ENDCASE.

    IF NOT prep IS INITIAL.
      IF NOT ( itreedisplay[] IS INITIAL ).
        PERFORM displaytree USING itreedisplay[].
      ELSE.
        statusbarmessage = 'No items found matching selection criteria'.
        PERFORM displaystatus USING statusbarmessage 2.
      ENDIF.
    ENDIF.
  ENDIF.

*--- Memory IDs
* User name
*  SET PARAMETER ID 'MAUTH' FIELD pauth.
* Message class
  SET PARAMETER ID 'MMNAME' FIELD pmname.
* Customer namespace
  SET PARAMETER ID 'MNAMESPACE' FIELD pcname.
* Folder
  SET PARAMETER ID 'MFOLDER' FIELD pfolder.
* Logical filepath
*  SET PARAMETER ID 'MLOGICAL' FIELD plogical.
* Package
  SET PARAMETER ID 'MPACK' FIELD ppack.
* Text element checkbox
  SET PARAMETER ID 'MTEXT' FIELD ptext.
* Messages checkbox
  SET PARAMETER ID 'MMESS' FIELD pmess.
* Includes checkbox
  SET PARAMETER ID 'MINC' FIELD pinc.
* Recursive includes checkbox.
  SET PARAMETER ID 'MRECI' FIELD preci.
* Functions checkbox
  SET PARAMETER ID 'MFUNC' FIELD pfunc.
* Recursive functions checkbox
  SET PARAMETER ID 'MRECF' FIELD precf.
* Function module documntation checkbox
  SET PARAMETER ID 'MDOC' FIELD pdoc.
* Screens checkbox
  SET PARAMETER ID 'MSCR' FIELD pscr.
* Dictionary checkbox
  SET PARAMETER ID 'MDICT' FIELD pdict.
* Sort table ascending checkBox
  SET PARAMETER ID 'MSORTT' FIELD psortt.

************************************************************************
*****************************SUBROUTINES********************************
************************************************************************

*-----------------------------------------------------------------------
*  checkComboBoxes...  Check input parameters
*-----------------------------------------------------------------------
FORM checkcomboboxes.

  IF soauthor IS INITIAL.
    CASE 'X'.
      WHEN rtable.
        IF sotable[] IS INITIAL.
          statusbarmessage
            = 'You must enter either a table name or author.'.
        ENDIF.
      WHEN rfunc.
        IF ( sofname[] IS INITIAL ) AND ( sofgroup[] IS INITIAL ).
          IF sofname[] IS INITIAL.
            statusbarmessage
               = 'You must enter either a function name or author.'.
          ELSE.
            IF sofgroup[] IS INITIAL.
              statusbarmessage
         = 'You must enter either a function group, or an author name.'.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN rprog.
        IF soprog[] IS INITIAL.
          statusbarmessage =
           'You must enter either a program name or author name.'.
        ENDIF.
    ENDCASE.
* Check the user name of the person objects are to be downloaded for
  ELSE.
*    IF pauth = 'SAP*' OR pauth = 'SAP'.
*      statusbarmessage
*        = 'Sorry cannot download all objects for SAP standard user'.
*    ENDIF.
  ENDIF.

  IF NOT statusbarmessage IS INITIAL.
    PERFORM displaystatus USING statusbarmessage 3.
    forcedexit = 1.
    STOP.
  ENDIF.
ENDFORM.                                                "checkComboBoxes

*-----------------------------------------------------------------------
* fillSelectionRanges...      for selection routines
*-----------------------------------------------------------------------
FORM fillselectionranges.

  DATA: strlength TYPE i.

  strlength = STRLEN( pcname ).

*  IF NOT pauth IS INITIAL.
*    soauthor-sign = 'I'.
*    soauthor-option = 'EQ'.
*    soauthor-low = pauth.
*    APPEND soauthor.
*    soauthor = pauth.
*  ENDIF.

* Tables
  IF NOT sotable IS INITIAL.
    sotablenames[] = sotable[].
*   Add in the customer namespace if we need to
    IF NOT pcname IS INITIAL.
      LOOP AT sotablenames.
        IF sotablenames-low+0(strlength) <> pcname.
          CONCATENATE pcname sotablenames-low INTO sotablenames-low.
        ENDIF.

        IF sotablenames-high+0(strlength) <> pcname.
          CONCATENATE pcname sotablenames-high INTO sotablenames-high.
        ENDIF.

        MODIFY sotablenames.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Function names
  IF NOT sofname IS INITIAL.
    sofunctionname[] = sofname[].
*   Add in the customer namespace if we need to
    IF NOT pcname IS INITIAL.
      LOOP AT sofunctionname.
        IF sofunctionname-low+0(strlength) <> pcname.
          CONCATENATE pcname sofunctionname-low INTO sofunctionname-low.
        ENDIF.

        IF sofunctionname-high+0(strlength) <> pcname.
          CONCATENATE pcname sofunctionname-high
                      INTO sofunctionname-high.
        ENDIF.

        MODIFY sofunctionname.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Function group
  IF NOT sofgroup IS INITIAL.
    sofunctiongroup[] = sofgroup[].
*   Add in the customer namespace if we need to
    IF NOT pcname IS INITIAL.
      LOOP AT sofunctionname.
        IF sofunctiongroup-low+0(strlength) <> pcname.
          CONCATENATE pcname sofunctiongroup-low
                      INTO sofunctiongroup-low.
        ENDIF.

        IF sofunctiongroup-high+0(strlength) <> pcname.
          CONCATENATE pcname sofunctiongroup-high
                      INTO sofunctiongroup-high.
        ENDIF.

        MODIFY sofunctiongroup.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Program names
  IF NOT soprog IS INITIAL.
    soprogramname[] = soprog[].
*   Add in the customer namespace if we need to
    IF NOT pcname IS INITIAL.
      LOOP AT soprogramname.
        IF soprogramname-low+0(strlength) <> pcname.
          CONCATENATE pcname soprogramname-low INTO soprogramname-low.
        ENDIF.

        IF soprogramname-high+0(strlength) <> pcname.
          CONCATENATE pcname soprogramname-high INTO soprogramname-high.
        ENDIF.

        MODIFY soprogramname.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                                           " fillSelectionRanges

*-----------------------------------------------------------------------
*  retrieveTables...             Search for tables in dictionary
*-----------------------------------------------------------------------
FORM retrievetables USING ilocdictstructure LIKE idictionary[]
                          sotable LIKE sotable[]
                          soauthor LIKE soauthor[].

  DATA: wadictstructure TYPE tdicttable.

  SELECT tabname FROM dd02l INTO wadictstructure-tablename
                            WHERE tabname IN sotable
                              AND tabclass <> 'CLUSTER'
                              AND tabclass <> 'POOL'
                              AND tabclass <> 'VIEW'
                              AND as4user IN soauthor
                              AND as4local = 'A'.

    PERFORM findtabledescription USING wadictstructure-tablename
                                       wadictstructure-tabletitle.

    PERFORM findtabledefinition USING wadictstructure-tablename
                                      wadictstructure-istructure[].

    APPEND wadictstructure TO ilocdictstructure.
    CLEAR wadictstructure.
  ENDSELECT.
ENDFORM.                                                 "retrieveTables

*-----------------------------------------------------------------------
*  findTableDescription...  Search for table description in dictionary
*-----------------------------------------------------------------------
FORM findtabledescription USING value(tablename)
                                      tabledescription.

  SELECT SINGLE ddtext FROM dd02t INTO tabledescription
                                  WHERE tabname = tablename
                                    AND ddlanguage = sy-langu.
ENDFORM.                                           "findTableDescription

*-----------------------------------------------------------------------
*  findTableDefinition... Find the structure of a table from the SAP
*                         database.
*-----------------------------------------------------------------------
FORM findtabledefinition USING value(tablename)
                               idictstruct LIKE dumidictstructure[].

  DATA gotstate LIKE dcobjif-gotstate.
  DATA: definition TYPE STANDARD TABLE OF dd03p WITH HEADER LINE.
  DATA: wadictstruct TYPE tdicttablestructure.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = tablename
      state         = 'A'
      langu         = sy-langu
    IMPORTING
      gotstate      = gotstate
    TABLES
      dd03p_tab     = definition
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc = 0 AND gotstate = 'A'.
    LOOP AT definition.
      MOVE-CORRESPONDING definition TO wadictstruct.
      PERFORM removeleadingzeros CHANGING wadictstruct-position.
      PERFORM removeleadingzeros CHANGING wadictstruct-leng.
      APPEND wadictstruct TO idictstruct.
    ENDLOOP.
  ENDIF.
ENDFORM.                                            "findTableDefinition

*-----------------------------------------------------------------------
*  retrieveMessageClass...   Retrieve a message class from the SAP
*                            database
*-----------------------------------------------------------------------
FORM retrievemessageclass USING ilocmessages LIKE imessages[]
                                rangeauthor LIKE soauthor[]
                                value(messageclassname)
                                value(messageclasslang)
                                value(modifiedby).

  DATA: wamessage TYPE tmessage.

  IF NOT messageclassname IS INITIAL.
    SELECT * FROM t100
             APPENDING CORRESPONDING FIELDS OF TABLE ilocmessages
             WHERE sprsl = messageclasslang
               AND arbgb = messageclassname.

    LOOP AT ilocmessages INTO wamessage.
      SELECT SINGLE stext FROM t100a                   "#EC CI_BUFFJOIN
                          INTO wamessage-stext
                          WHERE arbgb = wamessage-arbgb.
      MODIFY ilocmessages FROM wamessage INDEX sy-tabix.
    ENDLOOP.
  ELSE.
    IF modifiedby IS INITIAL.
*     Select by author
      SELECT t100~arbgb                                "#EC CI_BUFFJOIN
             t100~msgnr
             t100~text
             t100a~stext
             APPENDING CORRESPONDING FIELDS OF TABLE ilocmessages
               FROM t100
               INNER JOIN t100a ON t100a~arbgb = t100~arbgb
               WHERE t100a~masterlang = messageclasslang
                 AND t100a~respuser IN rangeauthor[].
    ELSE.
*     Select also by the last person who modified the message class
      SELECT t100~arbgb                                "#EC CI_BUFFJOIN
             t100~msgnr
             t100~text
             t100a~stext
             APPENDING CORRESPONDING FIELDS OF TABLE ilocmessages
             FROM t100
             INNER JOIN t100a ON t100a~arbgb = t100~arbgb
             WHERE t100a~masterlang = messageclasslang
               AND t100a~respuser IN rangeauthor[]
               AND t100a~lastuser IN rangeauthor[].
    ENDIF.
  ENDIF.
ENDFORM.                                           "retrieveMessageClass

*-----------------------------------------------------------------------
*  retrieveFunctions...   Retrieve function modules from SAP DB.  May be
*                         called in one of two ways
*-----------------------------------------------------------------------
FORM retrievefunctions USING sofname LIKE sofunctionname[]
                             sofgroup LIKE sofunctiongroup[]
                             ilocfunctionnames LIKE ifunctions[]
                             value(solocauthor) LIKE soauthor[]
                             value(gettextelements)
                             value(getscreens)
                             value(customeronly)
                             value(customernamerange).

  RANGES: rangefuncname  FOR tfdir-funcname.
  RANGES: rangefuncgroup FOR enlfdir-area.
  DATA: wafunctionname TYPE tfunction.
  DATA: previousfg TYPE v_fdir-area.
  DATA: nogroupsfound TYPE i VALUE true.

  rangefuncname[] = sofname[].
  rangefuncgroup[] = sofgroup[].

  IF NOT solocauthor[] IS INITIAL.
*-- Need to select all function groups by author
    SELECT area FROM tlibv INTO rangefuncgroup-low
           WHERE uname IN solocauthor
           AND area IN sofgroup[].

      rangefuncgroup-sign = 'I'.
      rangefuncgroup-option = 'EQ'.
      APPEND rangefuncgroup.
      nogroupsfound = false.
    ENDSELECT.
  ELSE.
    nogroupsfound = false.
  ENDIF.

  IF nogroupsfound = false.
*   select by function name and/or function group.
    SELECT funcname area FROM v_fdir
                    INTO (wafunctionname-functionname,
                          wafunctionname-functiongroup)
                    WHERE funcname IN rangefuncname
                      AND area IN rangefuncgroup
                      AND generated = ''
                      ORDER BY area.

      APPEND wafunctionname TO ilocfunctionnames.
    ENDSELECT.
  ENDIF.

  LOOP AT ilocfunctionnames INTO wafunctionname.
    PERFORM retrievefunctiondetail USING wafunctionname-functionname
                                         wafunctionname-progname
                                         wafunctionname-includenumber
                                         wafunctionname-functiontitle.

    PERFORM findmainfunctioninclude USING
                                     wafunctionname-progname
                                     wafunctionname-includenumber
                                     wafunctionname-functionmaininclude.

    PERFORM findfunctiontopinclude USING wafunctionname-progname
                                         wafunctionname-topincludename.

*   Find all user defined includes within the function group
    PERFORM scanforfunctionincludes USING wafunctionname-progname
                                          customeronly
                                          customernamerange
                                          wafunctionname-iincludes[].
*   Find main message class
    PERFORM findmainmessageclass USING wafunctionname-progname
                                       wafunctionname-messageclass.

*   Find any screens declared within the main include
    IF NOT getscreens IS INITIAL.
      IF previousfg IS INITIAL
        OR previousfg <> wafunctionname-functiongroup.
        PERFORM findfunctionscreenflow USING wafunctionname.

*       Search for any GUI texts
        PERFORM retrieveguititles USING wafunctionname-iguititle[]
                                        wafunctionname-progname.
      ENDIF.
    ENDIF.

    IF NOT gettextelements IS INITIAL.
*     Find the program texts from out of the database.
      PERFORM retrieveprogramtexts
                            USING wafunctionname-iselectiontexts[]
                                  wafunctionname-itextelements[]
                                  wafunctionname-progname.
    ENDIF.

    previousfg = wafunctionname-functiongroup.
    MODIFY ilocfunctionnames FROM wafunctionname.
  ENDLOOP.
ENDFORM.                                              "retrieveFunctions

*-----------------------------------------------------------------------
*  retrieveFunctionDetail...   Retrieve function module details from SAP
*                              DB.
*-----------------------------------------------------------------------
FORM retrievefunctiondetail USING value(functionname)
                                         progname
                                        includename
                                        titletext.

  SELECT SINGLE pname include
                FROM tfdir
                INTO (progname, includename)
                WHERE funcname = functionname.

  IF sy-subrc = 0.
    SELECT SINGLE stext FROM tftit
                  INTO titletext
                  WHERE spras = sy-langu
                    AND funcname = functionname.
  ENDIF.
ENDFORM.                                         "retrieveFunctionDetail

*-----------------------------------------------------------------------
*  findMainFunctionInclude...  Find the main include that contains the
*                              source code
*-----------------------------------------------------------------------
FORM findmainfunctioninclude USING value(programname)
                                   value(includeno)
                                         internalincludename.
  DATA: newincludenumber(30).

  CONCATENATE '%U' includeno INTO newincludenumber.
  SELECT SINGLE include FROM d010inc
                        INTO internalincludename
                        WHERE master = programname
                          AND include LIKE newincludenumber.
ENDFORM.                                        "findMainFunctionInclude

*-----------------------------------------------------------------------
*  findFunctionTopInclude...  Find the top include for the function
*                             group
*-----------------------------------------------------------------------
FORM findfunctiontopinclude USING value(programname)
                                        topincludename.

  SELECT SINGLE include FROM d010inc
                        INTO topincludename
                        WHERE master = programname
                          AND include LIKE '%TOP'.
ENDFORM.                                         "findFunctionTopInclude

*-----------------------------------------------------------------------
* scanForAdditionalFuncStuff... Search for additional things relating to
*                               functions
*-----------------------------------------------------------------------
FORM scanforadditionalfuncstuff USING ilocfunctions LIKE ifunctions[]
                                      value(recursiveincludes)
                                      value(recursivefunctions)
                                      value(searchforincludes)
                                      value(searchforfunctions)
                                      value(searchfordictionary)
                                      value(searchformessages)
                                      value(customeronly)
                                      value(customernamerange).

  DATA: wafunction TYPE tfunction.
  DATA: wainclude TYPE tinclude.

  LOOP AT ilocfunctions INTO wafunction.
    IF NOT searchforincludes IS INITIAL.
*     Search in the main include
      PERFORM scanforincludeprograms USING
                                       wafunction-functionmaininclude
                                       recursiveincludes
                                       customeronly
                                       customernamerange
                                       wafunction-iincludes[].

*     Search in the top include
      PERFORM scanforincludeprograms USING wafunction-topincludename
                                           recursiveincludes
                                           customeronly
                                           customernamerange
                                           wafunction-iincludes[].
    ENDIF.

    IF NOT searchforfunctions IS INITIAL.
      PERFORM scanforfunctions USING wafunction-functionmaininclude
                                     wafunction-programlinkname
                                     recursiveincludes
                                     recursivefunctions
                                     customeronly
                                     customernamerange
                                     ilocfunctions[].
    ENDIF.

    MODIFY ilocfunctions FROM wafunction.
  ENDLOOP.

* Now we have everthing perhaps we had better find all the dictionary
* structures
  IF NOT searchfordictionary IS INITIAL.
    LOOP AT ilocfunctions INTO wafunction.
      PERFORM scanfortables USING wafunction-progname
                                  customeronly
                                  customernamerange
                                  wafunction-idictstruct[].

      PERFORM scanforlikeortype USING wafunction-progname
                                      customeronly
                                      customernamerange
                                      wafunction-idictstruct[].

      LOOP AT wafunction-iincludes INTO wainclude.
        PERFORM scanfortables USING wainclude-includename
                                    customeronly
                                    customernamerange
                                    wafunction-idictstruct[].

        PERFORM scanforlikeortype USING wainclude-includename
                                        customeronly
                                        customernamerange
                                        wafunction-idictstruct[].
      ENDLOOP.

      MODIFY ilocfunctions FROM wafunction.
    ENDLOOP.
  ENDIF.

* Now search for all messages
  IF NOT searchformessages IS INITIAL.
    LOOP AT ilocfunctions INTO wafunction.
      PERFORM scanformessages USING wafunction-progname
                                    wafunction-messageclass
                                    wafunction-imessages[].
      MODIFY ilocfunctions FROM wafunction.
    ENDLOOP.
  ENDIF.
ENDFORM.                                     "scanForAdditionalFuncStuff

*-----------------------------------------------------------------------
* scanForIncludePrograms... Search each program for include programs
*-----------------------------------------------------------------------
FORM scanforincludeprograms USING value(programname)
                                  value(recursiveincludes)
                                  value(customeronly)
                                  value(customernamerange)
                                        ilocincludes LIKE dumiincludes[]
.

*  DATA: iincludelines TYPE STANDARD TABLE OF text72 WITH HEADER LINE.
  DATA: iincludelines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: itokens TYPE STANDARD TABLE OF stoken WITH HEADER LINE.
  DATA: ikeywords TYPE STANDARD TABLE OF text20 WITH HEADER LINE.
  DATA: istatements TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE.
  DATA: watokens TYPE stoken.
  DATA: wainclude TYPE tinclude.
  DATA: waincludeexists TYPE tinclude.
  DATA: maxlines TYPE i.
  DATA: nextline TYPE i.
  DATA: castprogramname TYPE program.

* Read the program code from the textpool.
  castprogramname = programname.
  READ REPORT castprogramname INTO iincludelines.

  APPEND include TO ikeywords.
  SCAN ABAP-SOURCE iincludelines TOKENS INTO itokens
      WITH INCLUDES STATEMENTS INTO istatements KEYWORDS FROM ikeywords.

  CLEAR iincludelines[].

  DESCRIBE TABLE itokens LINES maxlines.
  LOOP AT itokens WHERE str = include AND type = 'I'.
    nextline = sy-tabix + 1.
    IF nextline <= maxlines.
      READ TABLE itokens INDEX nextline INTO watokens.

*      Are we only to find customer includes?
      IF NOT customeronly IS INITIAL.
        IF watokens-str+0(1) = 'Y' OR watokens-str+0(1) = 'Z'
          OR watokens-str CS customernamerange
          OR watokens-str+0(2) = 'MZ' OR watokens-str+0(2) = 'MY'.

        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      wainclude-includename = watokens-str.

*      Best find the program title text as well.
      PERFORM findprogramorincludetitle USING wainclude-includename
                                              wainclude-includetitle.

*      Don't append the include if we already have it listed
      READ TABLE ilocincludes INTO waincludeexists
                 WITH KEY includename = wainclude-includename.
      IF sy-subrc <> 0.
        APPEND wainclude TO ilocincludes.

        IF NOT recursiveincludes IS INITIAL.
*          Do a recursive search for other includes
          PERFORM scanforincludeprograms USING wainclude-includename
                                               recursiveincludes
                                               customeronly
                                               customernamerange
                                               ilocincludes[].
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                                         "scanForIncludePrograms

*-----------------------------------------------------------------------
* scanForFunctions... Search each program for function modules
*-----------------------------------------------------------------------
FORM scanforfunctions USING value(programname)
                            value(programlinkname)
                            value(recursiveincludes)
                            value(recursivefunctions)
                            value(customeronly)
                            value(customernamerange)
                                  ilocfunctions LIKE ifunctions[].

  DATA: iincludelines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: itokens TYPE STANDARD TABLE OF stoken WITH HEADER LINE.
  DATA: istatements TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE.
  DATA: watokens TYPE stoken.
  DATA: wafunction TYPE tfunction.
  DATA: wafunctioncomparison TYPE tfunction.
  DATA: maxlines TYPE i.
  DATA: nextline TYPE i.
  DATA: castprogramname TYPE program.
  DATA: skipthisloop TYPE i.
  DATA: watokenslength TYPE i VALUE 0.

* Read the program code from the textpool.
  castprogramname = programname.
  READ REPORT castprogramname INTO iincludelines.
  SCAN ABAP-SOURCE iincludelines TOKENS INTO itokens
    WITH INCLUDES STATEMENTS INTO istatements.
  CLEAR iincludelines[].

  DESCRIBE TABLE itokens LINES maxlines.
  LOOP AT itokens WHERE str = function AND type = 'I'.

    nextline = sy-tabix + 1.
    IF nextline <= maxlines.
      READ TABLE itokens INDEX nextline INTO watokens.

*      Are we only to find customer functions
      skipthisloop = false.
      IF NOT customeronly IS INITIAL.
        watokenslength = STRLEN( watokens-str ).
        IF watokenslength > 1.
          IF watokens-str+1(1) = 'Y' OR watokens-str+1(1) = 'Z'
             OR watokens-str+1(1) CS customernamerange.
          ELSE.
            CONTINUE.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF skipthisloop = false.
        wafunction-functionname = watokens-str.
        TRANSLATE wafunction-functionname USING ''''' '.
        CONDENSE wafunction-functionname.

*        Don't add a function if we alread have it listed.
        READ TABLE ilocfunctions WITH KEY functionname =
             wafunction-functionname INTO wafunctioncomparison.
        IF sy-subrc <> 0.
*          Add in the link name if the function is linked to a program
          wafunction-programlinkname = programlinkname.

*          Don't download functions which are called through an RFC
*          destination
          nextline = sy-tabix + 2.
          READ TABLE itokens INDEX nextline INTO watokens.
          IF watokens-str <> destination.

*            Find the function group
            SELECT SINGLE area
                          FROM v_fdir
                          INTO wafunction-functiongroup
                          WHERE funcname = wafunction-functionname.

            IF sy-subrc = 0.
*              Best find the function number as well.
              PERFORM retrievefunctiondetail
                                   USING wafunction-functionname
                                         wafunction-progname
                                         wafunction-includenumber
                                         wafunction-functiontitle.

              PERFORM findmainfunctioninclude
                                  USING wafunction-progname
                                        wafunction-includenumber
                                        wafunction-functionmaininclude.

              PERFORM findfunctiontopinclude
                                  USING wafunction-progname
                                        wafunction-topincludename.

*              Find main message class
              PERFORM findmainmessageclass
                                  USING wafunction-progname
                                        wafunction-messageclass.

              APPEND wafunction TO ilocfunctions.

*              Now lets search a little bit deeper and do a recursive
*              search for other includes
              IF NOT recursiveincludes IS INITIAL.
                PERFORM scanforincludeprograms
                                  USING wafunction-functionmaininclude
                                        recursiveincludes
                                        customeronly
                                        customernamerange
                                        wafunction-iincludes[].
              ENDIF.

*              Now lets search a little bit deeper and do a recursive
*              search for other functions
              IF NOT recursivefunctions IS INITIAL.
                PERFORM scanforfunctions
                                 USING wafunction-functionmaininclude
                                       space
                                       recursiveincludes
                                       recursivefunctions
                                       customeronly
                                       customernamerange
                                       ilocfunctions[].
              ENDIF.
              CLEAR wafunction.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR wafunction.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                                               "scanForFunctions

*-----------------------------------------------------------------------
*  scanForFunctionIncludes... Find all user defined includes within the
*                             function group
*-----------------------------------------------------------------------
FORM scanforfunctionincludes USING poolname
                                   value(customeronly)
                                   value(customernamerange)
                                   ilocincludes LIKE dumiincludes[].

  DATA: iincludelines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: itokens TYPE STANDARD TABLE OF stoken WITH HEADER LINE.
  DATA: ikeywords TYPE STANDARD TABLE OF text20 WITH HEADER LINE.
  DATA: istatements TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE.
  DATA: watokens TYPE stoken.
  DATA: wainclude TYPE tinclude.
  DATA: waincludeexists TYPE tinclude.
  DATA: maxlines TYPE i.
  DATA: nextline TYPE i.
  DATA: castprogramname TYPE program.

* Read the program code from the textpool.
  castprogramname = poolname.
  READ REPORT castprogramname INTO iincludelines.

  APPEND include TO ikeywords.
  SCAN ABAP-SOURCE iincludelines TOKENS INTO itokens
    WITH INCLUDES STATEMENTS INTO istatements KEYWORDS FROM ikeywords.

  CLEAR iincludelines[].

  DESCRIBE TABLE itokens LINES maxlines.
  LOOP AT itokens WHERE str = include AND type = 'I'.
    nextline = sy-tabix + 1.
    IF nextline <= maxlines.
      READ TABLE itokens INDEX nextline INTO watokens.

      IF watokens-str CP '*F++'.
*        Are we only to find customer includes?
        IF NOT customeronly IS INITIAL.
          IF watokens-str+0(2) = 'LY' OR watokens-str+0(2) = 'LZ'
             OR watokens-str CS customernamerange.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.

        wainclude-includename = watokens-str.

*        Best find the program title text as well.
        PERFORM findprogramorincludetitle USING wainclude-includename
                                                wainclude-includetitle.

*        Don't append the include if we already have it listed
        READ TABLE ilocincludes INTO waincludeexists
             WITH KEY includename = wainclude-includename.
        IF sy-subrc <> 0.
          APPEND wainclude TO ilocincludes.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                                        "scanForFunctionIncludes

*-----------------------------------------------------------------------
*  findProgramOrIncludeTitle...   Finds the title text of a program.
*-----------------------------------------------------------------------
FORM findprogramorincludetitle USING value(programname)
                                           titletext.

  SELECT SINGLE text FROM trdirt
                     INTO titletext
                     WHERE name = programname
                       AND sprsl = sy-langu.
ENDFORM.                                      "findProgramOrIncludeTitle

*-----------------------------------------------------------------------
* retrievePrograms...    find programs and sub objects from SAP DB
*-----------------------------------------------------------------------
FORM retrieveprograms USING ilocprogram LIKE iprograms[]
                            ilocfunctions LIKE ifunctions[]
                            rangeprogram LIKE soprogramname[]
                            rangeauthor LIKE soauthor[]
                            value(custnamerange)
                            value(alsomodifiedbyauthor)
                            value(customerprogsonly)
                            value(getmessages)
                            value(gettextelements)
                            value(getcustdictstructures)
                            value(getfunctions)
                            value(getincludes)
                            value(getscreens)
                            value(recursivefuncsearch)
                            value(recursiveincludesearch)
                            value(getlocalobjects)
                            value(package).

  DATA: warangeprogram LIKE LINE OF rangeprogram.

  IF rangeprogram[] IS INITIAL.
*   We are finding all programs by an author
    PERFORM findallprogramsforauthor USING ilocprogram[]
                                           rangeprogram[]
                                           rangeauthor[]
                                           custnamerange
                                           alsomodifiedbyauthor
                                           customerprogsonly
                                           getlocalobjects
                                           package.
  ELSE.
    READ TABLE rangeprogram INDEX 1 INTO warangeprogram.
    IF warangeprogram-low CS asterix.
      PERFORM findprogramsbywildcard USING ilocprogram[]
                                           rangeprogram[]
                                           rangeauthor[]
                                           custnamerange
                                           customerprogsonly
                                           getlocalobjects
                                           package.
    ELSE.
      PERFORM checkprogramdoesexist USING ilocprogram[]
                                          rangeprogram[].
    ENDIF.
  ENDIF.

  SORT ilocprogram.
  DELETE ADJACENT DUPLICATES FROM ilocprogram.

* Find extra items
  PERFORM scanforadditionalprogstuff USING ilocprogram[]
                                           ilocfunctions[]
                                           gettextelements
                                           getmessages
                                           getscreens
                                           getcustdictstructures
                                           getfunctions
                                           getincludes
                                           customerprogsonly
                                           custnamerange
                                           recursiveincludesearch
                                           recursivefuncsearch.
ENDFORM.                                               "retrievePrograms

*-----------------------------------------------------------------------
*  scanForAdditionalProgStuff...
*-----------------------------------------------------------------------
FORM scanforadditionalprogstuff USING ilocprogram LIKE iprograms[]
                                      ilocfunctions LIKE ifunctions[]
                                      value(gettextelements)
                                      value(getmessages)
                                      value(getscreens)
                                      value(getcustdictstructures)
                                      value(getfunctions)
                                      value(getincludes)
                                      value(customeronly)
                                      value(customernamerange)
                                      value(recursiveincludesearch)
                                      value(recursivefuncsearch).

  DATA: waprogram TYPE tprogram.
  DATA: wainclude TYPE tinclude.
  DATA: mytabix TYPE sytabix.

* Best to find all the includes used in a program first
  IF NOT getincludes IS INITIAL.
    LOOP AT ilocprogram INTO waprogram.
      mytabix = sy-tabix.
      PERFORM scanforincludeprograms USING waprogram-prog
                                           recursiveincludesearch
                                           customeronly
                                           customernamerange
                                           waprogram-iincludes[].

      MODIFY ilocprogram FROM waprogram INDEX mytabix.
    ENDLOOP.
  ENDIF.

* Once we have a list of all the includes we need to loop round them an
* select all the other objects
  LOOP AT ilocprogram INTO waprogram.
    mytabix = sy-tabix.
    PERFORM findprogramdetails USING waprogram-prog
                                     waprogram-subc
                                     waprogram-programtitle
                                     waprogram
                                     gettextelements
                                     getmessages
                                     getscreens
                                     getcustdictstructures
                                     customeronly
                                     customernamerange.

*   Find any screens
    IF NOT getscreens IS INITIAL.
      PERFORM findprogramscreenflow USING waprogram.
    ENDIF.

    LOOP AT waprogram-iincludes INTO wainclude.
      PERFORM findprogramdetails USING wainclude-includename
                                       'I'
                                       wainclude-includetitle
                                       waprogram
                                       gettextelements
                                       getmessages
                                       getscreens
                                       getcustdictstructures
                                       customeronly
                                       customernamerange.
    ENDLOOP.

    MODIFY ilocprogram FROM waprogram INDEX mytabix.
  ENDLOOP.

* Now we have all the program includes and details we need to find extra
* functions
  IF NOT getfunctions IS INITIAL.
    LOOP AT ilocprogram INTO waprogram.
*     Find any functions defined in the code
      PERFORM scanforfunctions USING waprogram-prog
                                     waprogram-prog
                                     space
                                     space
                                     customeronly
                                     customernamerange
                                     ilocfunctions[].
    ENDLOOP.
  ENDIF.

* We have a list of all the functions so lets go and find details and
* other function calls
  PERFORM scanforadditionalfuncstuff USING ilocfunctions[]
                                           recursiveincludesearch
                                           recursivefuncsearch
                                           getincludes
                                           getfunctions
                                           getcustdictstructures
                                           getmessages
                                           customeronly
                                           customernamerange.
ENDFORM.                                     "scanForAdditionalProgStuff

*-----------------------------------------------------------------------
*  findProgramDetails...
*-----------------------------------------------------------------------
FORM findprogramdetails USING value(programname)
                              value(programtype)
                                    programtitle
                                    waprogram TYPE tprogram
                              value(gettextelements)
                              value(getmessages)
                              value(getscreens)
                              value(getcustdictstructures)
                              value(customeronly)
                              value(customernamerange).

  PERFORM findprogramorincludetitle USING programname
                                          programtitle.

  IF NOT gettextelements IS INITIAL.
*   Find the program texts from out of the database.
    PERFORM retrieveprogramtexts USING waprogram-iselectiontexts[]
                                       waprogram-itextelements[]
                                       programname.
  ENDIF.

* Search for any GUI texts
  IF NOT getscreens IS INITIAL AND
    ( programtype = 'M' OR programtype = '1' ).
    PERFORM retrieveguititles USING waprogram-iguititle[]
                                    programname.
  ENDIF.

* Find individual messages
  IF NOT getmessages IS INITIAL.
    IF programtype = 'M' OR programtype = '1'.
      PERFORM findmainmessageclass USING programname
                                         waprogram-messageclass.
    ENDIF.

    PERFORM scanformessages USING programname
                                  waprogram-messageclass
                                  waprogram-imessages[].
  ENDIF.

  IF NOT getcustdictstructures IS INITIAL.
    PERFORM scanfortables USING programname
                                customeronly
                                customernamerange
                                waprogram-idictstruct[].

    PERFORM scanforlikeortype USING programname
                                    customeronly
                                    customernamerange
                                    waprogram-idictstruct[].
  ENDIF.
ENDFORM.                                             "findProgramDetails

*-----------------------------------------------------------------------
*  findAllProgramsForAuthor...
*-----------------------------------------------------------------------
FORM findallprogramsforauthor USING ilocprogram LIKE iprograms[]
                                    rangeprogram LIKE soprogramname[]
                                    rangeauthor LIKE soauthor[]
                                    value(custnamerange)
                                    value(alsomodifiedbyauthor)
                                    value(customerprogsonly)
                                    value(getlocalobjects)
                                    value(package).

  DATA: altcustomernamerange(2).
  FIELD-SYMBOLS: <waprogram> TYPE tprogram.
  DATA: genflag TYPE genflag.
  DATA: wainfoprog TYPE d010sinf.
  DATA: waprogram TYPE tprogram.

* build up the customer name range used for select statements
  CONCATENATE custnamerange '%' INTO altcustomernamerange.

* select by name and author
  IF NOT alsomodifiedbyauthor IS INITIAL.
*   Programs modified by author
*   Program to search for is an executable program
    IF customerprogsonly IS INITIAL.
*     Select all programs
      SELECT prog subc FROM d010sinf
                       INTO (wainfoprog-prog, wainfoprog-subc)
                      WHERE prog IN rangeprogram
                        AND cnam IN rangeauthor
                        AND ( subc = '1' OR subc = 'M' ).

        waprogram-prog = wainfoprog-prog.
        waprogram-subc = wainfoprog-subc.
        APPEND waprogram TO ilocprogram.
      ENDSELECT.
    ELSE.
*     Select only customer specific programs
      SELECT prog subc FROM d010sinf
                       INTO (wainfoprog-prog, wainfoprog-subc)
                      WHERE prog  IN rangeprogram
                        AND ( prog LIKE altcustomernamerange
                              OR prog LIKE 'Z%'
                              OR prog LIKE 'Y%'
                              OR prog LIKE 'SAPMZ%'
                              OR prog LIKE 'SAPMY%' )
                        AND cnam IN rangeauthor
                        AND ( subc = '1' OR subc = 'M' ).

        waprogram-prog = wainfoprog-prog.
        waprogram-subc = wainfoprog-subc.
        APPEND waprogram TO ilocprogram.
      ENDSELECT.
    ENDIF.
  ELSE.
*   Programs created by author
    IF customerprogsonly IS INITIAL.
*     Select all programs
      SELECT prog subc FROM d010sinf
                       INTO (wainfoprog-prog, wainfoprog-subc)
                     WHERE prog IN rangeprogram
                     AND ( subc = '1' OR subc = 'M' )
                     AND ( cnam IN rangeauthor OR unam IN rangeauthor ).

        waprogram-prog = wainfoprog-prog.
        waprogram-subc = wainfoprog-subc.
        APPEND waprogram TO ilocprogram.
      ENDSELECT.
    ELSE.
*     Select only customer specific programs
      SELECT prog subc FROM d010sinf
                       INTO (wainfoprog-prog, wainfoprog-subc)
                   WHERE prog IN rangeprogram
                     AND ( prog LIKE altcustomernamerange
                           OR prog LIKE 'Z%'
                           OR prog LIKE 'Y%'
                           OR prog LIKE 'SAPMZ%'
                           OR prog LIKE 'SAPMY%' )
                     AND ( subc = '1' OR subc = 'M' )
                     AND ( cnam IN rangeauthor OR unam IN rangeauthor ).

        waprogram-prog = wainfoprog-prog.
        waprogram-subc = wainfoprog-subc.
        APPEND waprogram TO ilocprogram.
      ENDSELECT.
    ENDIF.
  ENDIF.

* Delete any programs which are local objects
  IF getlocalobjects IS INITIAL.
    LOOP AT ilocprogram ASSIGNING <waprogram>.
      SELECT SINGLE genflag FROM tadiv
                    INTO genflag
                    WHERE pgmid = 'R3TR'
                      AND object = 'PROG'
                      AND obj_name = <waprogram>-prog
                      AND devclass = '$TMP'.
      IF sy-subrc = 0.
        DELETE ilocprogram.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Delete any programs which are not in the specified package
  IF NOT package IS INITIAL.
    IF package CS '*'.
      TRANSLATE package USING '*%'.
    ENDIF.

    LOOP AT ilocprogram ASSIGNING <waprogram>.
      SELECT SINGLE genflag FROM tadiv
                    INTO genflag
                    WHERE pgmid = 'R3TR'
                      AND object = 'PROG'
                      AND obj_name = <waprogram>-prog
                      AND devclass LIKE package.
      IF sy-subrc <> 0.
        DELETE ilocprogram.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                                       "findAllProgramsForAuthor

*-----------------------------------------------------------------------
*  checkProgramDoesExist...
*-----------------------------------------------------------------------
FORM checkprogramdoesexist USING ilocprogram LIKE iprograms[]
                                 rangeprogram LIKE soprogramname[].

  DATA: waprogram TYPE tprogram.

*  Check to see if the program is an executable program
  SELECT prog subc
                INTO (waprogram-prog, waprogram-subc)
                FROM d010sinf
                WHERE prog IN rangeprogram
                  AND ( subc = '1' OR
                        subc = 'I' OR
                        subc = 'M' OR
                        subc = 'S' ).

    IF NOT waprogram-prog IS INITIAL.
      APPEND waprogram TO ilocprogram.
    ENDIF.
  ENDSELECT.
ENDFORM.                                          "checkProgramDoesExist

*-----------------------------------------------------------------------
*  findProgramsByWildcard.. Search in the system for programs
*-----------------------------------------------------------------------
FORM findprogramsbywildcard USING ilocprogram LIKE iprograms[]
                                value(rangeprogram) LIKE soprogramname[]
                                value(rangeauthor) LIKE soauthor[]
                                value(custnamerange)
                                value(customerprogsonly)
                                value(getlocalobjects)
                                value(package).

  DATA: altcustomernamerange(60).
  FIELD-SYMBOLS: <waprogram> TYPE tprogram.
  DATA: genflag TYPE genflag.
  DATA: walocprogram LIKE LINE OF ilocprogram.
  DATA: wad010sinf TYPE d010sinf.

  IF customerprogsonly IS INITIAL.
*   build up the customer name range used for select statements
    IF custnamerange <> '^'.
      CONCATENATE custnamerange '%' INTO altcustomernamerange.

      SELECT prog
             subc
             FROM d010sinf
             INTO (wad010sinf-prog, wad010sinf-subc)
             WHERE prog  IN rangeprogram
               AND prog LIKE altcustomernamerange
               AND ( subc = '1' OR subc = 'M' OR subc = 'S' )
               AND ( cnam IN rangeauthor OR unam IN rangeauthor ).
        IF sy-subrc = 0.
          walocprogram-prog = wad010sinf-prog.
          walocprogram-subc = wad010sinf-subc.
          APPEND walocprogram TO ilocprogram.
        ENDIF.
      ENDSELECT.
    ELSE.

      READ TABLE rangeprogram INTO altcustomernamerange INDEX 1.

      CLEAR sy-subrc.
      WHILE sy-subrc = 0.
        REPLACE '*' WITH '%' INTO altcustomernamerange.
      ENDWHILE.

      altcustomernamerange = altcustomernamerange+3(57).

      SELECT prog subc cdat udat
             FROM d010sinf
             INTO (wad010sinf-prog, wad010sinf-subc,
                   wad010sinf-cdat, wad010sinf-udat)
*             WHERE  prog  IN rangeprogram
        WHERE  prog  LIKE altcustomernamerange
          AND ( subc = '1' OR subc = 'M' OR subc = 'S' )
          AND udat >= pudat.
*          AND ( cnam IN rangeauthor OR unam IN rangeauthor ).
        IF sy-subrc = 0.
          walocprogram-prog = wad010sinf-prog.
          walocprogram-subc = wad010sinf-subc.
          walocprogram-cdat = wad010sinf-cdat.
          walocprogram-udat = wad010sinf-udat.
          APPEND walocprogram TO ilocprogram.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ELSE.
*   Only customer programs
    IF custnamerange <> '^'.
      CONCATENATE custnamerange '%' INTO altcustomernamerange.

      SELECT prog
             subc
             FROM d010sinf
             INTO (wad010sinf-prog, wad010sinf-subc)
             WHERE  prog  IN rangeprogram
               AND (  prog LIKE altcustomernamerange
                 OR  prog LIKE 'Z%' OR  prog LIKE 'Y%'
                 OR  prog LIKE 'SAPMZ%' OR  prog LIKE 'SAPMY%' )
               AND ( subc = '1' OR subc = 'M' OR subc = 'S' )
               AND ( cnam IN rangeauthor OR unam IN rangeauthor ).
        IF sy-subrc = 0.
          walocprogram-prog = wad010sinf-prog.
          walocprogram-subc = wad010sinf-subc.
          APPEND walocprogram TO ilocprogram.
        ENDIF.
      ENDSELECT.
    ELSE.
      SELECT prog
             subc
             INTO (wad010sinf-prog, wad010sinf-subc)
             FROM d010sinf
             WHERE  prog  IN rangeprogram
               AND (  prog LIKE 'Z%' OR  prog LIKE 'Y%'
                     OR  prog LIKE 'SAPMZ%'
                     OR  prog LIKE 'SAPMY%' )
               AND ( subc = '1' OR subc = 'M' OR subc = 'S' )
               AND ( cnam IN rangeauthor OR unam IN rangeauthor ).
        IF sy-subrc = 0.
          walocprogram-prog = wad010sinf-prog.
          walocprogram-subc = wad010sinf-subc.
          APPEND walocprogram TO ilocprogram.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ENDIF.

* Delete any programs which are local objects
  IF getlocalobjects IS INITIAL.
    LOOP AT ilocprogram ASSIGNING <waprogram>.
      SELECT SINGLE genflag FROM tadiv
                    INTO genflag
                    WHERE pgmid = 'R3TR'
                      AND object = 'PROG'
                      AND obj_name = <waprogram>-prog
                      AND devclass = '$TMP'.
      IF sy-subrc = 0.
        DELETE ilocprogram.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Delete any programs which are not in the specified package
  IF NOT package IS INITIAL.
    LOOP AT ilocprogram ASSIGNING <waprogram>.
      SELECT SINGLE genflag FROM tadiv
                    INTO genflag
                    WHERE pgmid = 'R3TR'
                      AND object = 'PROG'
                      AND obj_name = <waprogram>-prog
                      AND devclass <> package.
      IF sy-subrc = 0.
        DELETE ilocprogram.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                                         "findProgramsByWildcard

*-----------------------------------------------------------------------
*  retrieveProgramTexts... Find the text elements and selection texts
*  for a program
*-----------------------------------------------------------------------
FORM retrieveprogramtexts USING ilocselectiontexts LIKE dumitexttab[]
                                iloctextelements LIKE dumitexttab[]
                                value(programname).

  DATA: itexttable TYPE STANDARD TABLE OF ttexttable WITH HEADER LINE.
  DATA: watexts TYPE ttexttable.
  DATA: castprogramname(50).

  MOVE programname TO castprogramname.

  READ TEXTPOOL castprogramname INTO itexttable LANGUAGE sy-langu.
  DELETE itexttable WHERE key = 'R'.

* Selection texts.
  LOOP AT itexttable WHERE id = 'S'.
    MOVE itexttable-key TO watexts-key.
    MOVE itexttable-entry TO watexts-entry.
    APPEND watexts TO ilocselectiontexts.
    CLEAR watexts.
  ENDLOOP.

* Text elements.
  DELETE itexttable WHERE key = 'S'.
  LOOP AT itexttable WHERE id = 'I'.
    MOVE itexttable-key TO watexts-key.
    MOVE itexttable-entry TO watexts-entry.
    APPEND watexts TO iloctextelements.
  ENDLOOP.
ENDFORM.                                           "retrieveProgramTexts

*-----------------------------------------------------------------------
*  retrieveGUITitles...  Search for any GUI texts
*-----------------------------------------------------------------------
FORM retrieveguititles USING ilocguititle LIKE dumiguititle[]
                             value(programname).

  SELECT obj_code
         text
         FROM d347t
         APPENDING CORRESPONDING FIELDS OF TABLE ilocguititle
         WHERE progname = programname.
ENDFORM.                                              "retrieveGUITitles

*-----------------------------------------------------------------------
*   findMainMessageClass... find the message class stated at the top of
*                           program.
*-----------------------------------------------------------------------
FORM findmainmessageclass USING value(programname)
                                      messageclass.

  SELECT SINGLE msgid
                FROM trdire INTO messageclass
                WHERE report = programname.
ENDFORM.                                           "findMainMessageClass

*-----------------------------------------------------------------------
* scanForMessages... Search each program for messages
*-----------------------------------------------------------------------
FORM scanformessages USING value(programname)
                           value(mainmessageclass)
                                 ilocmessages LIKE imessages[].

  DATA: iincludelines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: itokens TYPE STANDARD TABLE OF stoken WITH HEADER LINE.
  DATA: istatements TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE.
  DATA: ikeywords TYPE STANDARD TABLE OF text20 WITH HEADER LINE.
  DATA: wamessage TYPE tmessage.
  DATA: wamessagecomparison TYPE tmessage.
  DATA: watokens TYPE stoken.
  DATA: nextline TYPE i.
  DATA: stringlength TYPE i VALUE 0.
  DATA: workingonmessage TYPE i VALUE false.
  DATA: castprogramname TYPE program.

* Read the program code from the textpool.
  castprogramname = programname.
  READ REPORT castprogramname INTO iincludelines.

  APPEND message TO ikeywords.
  SCAN ABAP-SOURCE iincludelines TOKENS INTO itokens
    WITH INCLUDES STATEMENTS INTO istatements KEYWORDS FROM ikeywords.

  CLEAR iincludelines[].

  LOOP AT itokens.
    IF itokens-str = message.
      workingonmessage = true.
      CONTINUE.
    ENDIF.

    IF workingonmessage = true.
      stringlength = STRLEN( itokens-str ).

*     Message declaration 1
      IF stringlength = 4 AND itokens-str+0(1) CA sy-abcde.
        wamessage-msgnr = itokens-str+1(3).
        wamessage-arbgb = mainmessageclass.
      ELSE.
        IF itokens-str CS '''' OR itokens-str CS '`'.
*         Message declaration 2
          TRANSLATE itokens-str USING ''' '.
          TRANSLATE itokens-str USING '` '.
          CONDENSE itokens-str.
          SHIFT itokens-str LEFT DELETING LEADING space.
          wamessage-text = itokens-str.
          wamessage-arbgb = 'Hard coded'.
        ELSE.
          IF itokens-str = 'ID'.
*           Message declaration 3
            nextline = sy-tabix + 1.
            READ TABLE itokens INDEX nextline INTO watokens.
            TRANSLATE watokens-str USING ''' '.
            CONDENSE itokens-str.
            SHIFT watokens-str LEFT DELETING LEADING space.
            IF NOT watokens-str = 'SY-MSGID'.
              wamessage-arbgb = watokens-str.

              nextline = nextline + 4.
              READ TABLE itokens INDEX nextline INTO watokens.
              TRANSLATE watokens-str USING ''' '.
              CONDENSE watokens-str.
              SHIFT watokens-str LEFT DELETING LEADING space.
              wamessage-msgnr = watokens-str.
            ELSE.
              workingonmessage = false.
            ENDIF.
          ELSE.
            IF stringlength >= 5 AND itokens-str+4(1) = '('.
*              Message declaration 4
              wamessage-msgnr = itokens-str+1(3).
              SHIFT itokens-str LEFT UP TO '('.
              REPLACE '(' INTO itokens-str WITH space.
              REPLACE ')' INTO itokens-str WITH space.
              CONDENSE itokens-str.
              wamessage-arbgb = itokens-str.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*      find the message text
      IF NOT wamessage-arbgb IS INITIAL
         AND NOT wamessage-msgnr IS INITIAL
         AND wamessage-text IS INITIAL.
        SELECT SINGLE text FROM t100 INTO wamessage-text
                           WHERE sprsl = sy-langu
                             AND arbgb = wamessage-arbgb
                             AND msgnr = wamessage-msgnr.
      ENDIF.

*      Append the message
      IF NOT wamessage IS INITIAL.
*        Don't append the message if we already have it listed
        READ TABLE ilocmessages WITH KEY arbgb = wamessage-arbgb
                                         msgnr = wamessage-msgnr
                                         INTO wamessagecomparison.
        IF sy-subrc <> 0.
          APPEND wamessage TO ilocmessages.
        ENDIF.
        CLEAR wamessage.
        workingonmessage = false.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                                                "scanForMessages

*-----------------------------------------------------------------------
* scanForTables... Search each program for dictionary tables
*-----------------------------------------------------------------------
FORM scanfortables USING value(programname)
                         value(customeronly)
                         value(customernamerange)
                               ilocdictionary LIKE idictionary[].

  DATA: iincludelines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: itokens TYPE STANDARD TABLE OF stoken WITH HEADER LINE.
  DATA: istatements TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE.
  DATA: ikeywords TYPE STANDARD TABLE OF text20 WITH HEADER LINE.
  DATA: wadictionary TYPE tdicttable.
  DATA: wadictionarycomparison TYPE tdicttable.
  DATA: castprogramname TYPE program.

* Read the program code from the textpool.
  castprogramname = programname.
  READ REPORT castprogramname INTO iincludelines.

  APPEND tables TO ikeywords.

  SCAN ABAP-SOURCE iincludelines TOKENS INTO itokens
    WITH INCLUDES STATEMENTS INTO istatements KEYWORDS FROM ikeywords.
  CLEAR iincludelines[].

  SORT itokens ASCENDING BY str.
  DELETE itokens WHERE str = tables.

  LOOP AT itokens.
    IF NOT customeronly IS INITIAL.
      IF ( itokens-str+0(1) <> 'Y'
        OR itokens-str+0(1) <> 'Z'
        OR itokens-str NS customernamerange ).
        CONTINUE.
      ENDIF.
    ENDIF.

    wadictionary-tablename = itokens-str.
*   Don't append the object if we already have it listed
    READ TABLE ilocdictionary INTO wadictionarycomparison
         WITH KEY tablename = wadictionary-tablename.
    IF sy-subrc <> 0.
      PERFORM findtabledescription USING wadictionary-tablename
                                         wadictionary-tabletitle.

      PERFORM findtabledefinition USING wadictionary-tablename
                                        wadictionary-istructure[].

      APPEND wadictionary TO ilocdictionary.
    ENDIF.
  ENDLOOP.
ENDFORM.                                                  "scanForTables

*-----------------------------------------------------------------------
*  findProgramScreenFlow...
*-----------------------------------------------------------------------
FORM findprogramscreenflow USING waprogram TYPE tprogram.

  DATA: iflow TYPE STANDARD TABLE OF tscreenflow WITH HEADER LINE.

  CALL FUNCTION 'DYNPRO_PROCESSINGLOGIC'
    EXPORTING
      rep_name  = waprogram-prog
    TABLES
      scr_logic = iflow.

  SORT iflow ASCENDING BY screen.
  DELETE ADJACENT DUPLICATES FROM iflow COMPARING screen.
  IF waprogram-subc <> 'M'.
    DELETE iflow WHERE screen >= '1000' AND screen <= '1099'.
  ENDIF.

  LOOP AT iflow.
    APPEND iflow TO waprogram-iscreenflow.
  ENDLOOP.
ENDFORM.                                          "findProgramScreenFlow

*-----------------------------------------------------------------------
*  findFunctionScreenFlow...
*-----------------------------------------------------------------------
FORM findfunctionscreenflow USING wafunction TYPE tfunction.

  DATA: iflow TYPE STANDARD TABLE OF tscreenflow WITH HEADER LINE.

  CALL FUNCTION 'DYNPRO_PROCESSINGLOGIC'
    EXPORTING
      rep_name  = wafunction-progname
    TABLES
      scr_logic = iflow.

  SORT iflow ASCENDING BY screen.
  DELETE ADJACENT DUPLICATES FROM iflow COMPARING screen.

  LOOP AT iflow.
    APPEND iflow TO wafunction-iscreenflow.
  ENDLOOP.
ENDFORM.                                         "findFunctionScreenFlow

*-----------------------------------------------------------------------
* scanForLikeOrType... Look for any dictionary objects referenced by a
*                      like or type statement
*-----------------------------------------------------------------------
FORM scanforlikeortype USING value(programname)
                             value(customeronly)
                             value(customernamerange)
                             ilocdictionary LIKE idictionary[].

  DATA ilines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: head TYPE string.
  DATA: tail TYPE string.
  DATA: junk TYPE string.
  DATA: linetype TYPE string.
  DATA: linelength TYPE i VALUE 0.
  DATA: endofline TYPE i VALUE true.
  DATA: wadictionary TYPE tdicttable.
  DATA: wadictionarycomparison TYPE tdicttable.
  DATA: waline LIKE LINE OF ilines.
  DATA: castprogramname TYPE program.


* Read the program code from the textpool.
  castprogramname = programname.
  READ REPORT castprogramname INTO ilines.

  LOOP AT ilines INTO waline.
*   Find custom tables.
    linelength = STRLEN( waline ).
    IF linelength > 0.
      IF waline(1) = asterix.
        CONTINUE.
      ENDIF.

      TRANSLATE waline TO UPPER CASE.

*     Determine the lineType.
      IF endofline = true.
        SHIFT waline UP TO like.
        IF sy-subrc = 0.
          linetype = like.
        ELSE.
          SHIFT waline UP TO type.
          IF sy-subrc = 0.
            IF waline NS 'BEGIN OF'.
              IF waline NS 'END OF'.
                IF waline NS 'VALUE'.
                  linetype = type.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            SHIFT waline UP TO include.
            IF sy-subrc = 0.
              SPLIT waline AT space INTO junk ilines.
            ENDIF.

            SHIFT waline UP TO structure.
            IF sy-subrc = 0.
              linetype = structure.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        linetype = comma.
      ENDIF.

      CASE linetype.
        WHEN like OR type OR structure.
*         Work on the appropriate lineType
          SHIFT waline UP TO space.
          SHIFT waline LEFT DELETING LEADING space.
          IF waline CS table.
            SPLIT waline AT table INTO head tail.
            SPLIT tail AT 'OF' INTO head tail.
            waline = tail.
            SHIFT waline LEFT DELETING LEADING space.
          ENDIF.
*         Are we only to download SAP dictionary structures.
          IF NOT customeronly IS INITIAL.
            IF  NOT waline IS INITIAL AND
               ( waline+0(1) = 'Y'
              OR waline+0(1) = 'Z'
              OR waline+0(1) CS customernamerange ).
            ELSE.
              linetype = ''.
              CONTINUE.
            ENDIF.
          ENDIF.

          IF waline CS comma.
            SPLIT waline AT comma INTO head tail.
            IF waline CS dash.
              SPLIT head AT dash INTO head tail.
            ENDIF.
            IF waline CS occurs.
              SPLIT waline AT space INTO head tail.
            ENDIF.
          ELSE.
            IF waline CS period.
              SPLIT waline AT period INTO head tail.
              IF waline CS dash.
                SPLIT head AT dash INTO head tail.
              ENDIF.
              IF waline CS occurs.
                SPLIT waline AT space INTO head tail.
              ENDIF.
            ELSE.
              SPLIT waline AT space INTO head tail.
              IF waline CS dash.
                SPLIT head AT dash INTO head tail.
              ENDIF.
            ENDIF.
          ENDIF.

          IF NOT head IS INITIAL.
            wadictionary-tablename = head.
*           Don't append the object if we already have it listed
            READ TABLE ilocdictionary INTO wadictionarycomparison
                            WITH KEY tablename = wadictionary-tablename.
            IF sy-subrc <> 0.
              PERFORM findtabledescription USING wadictionary-tablename
                                                 wadictionary-tabletitle
.

              PERFORM findtabledefinition USING wadictionary-tablename
                                              wadictionary-istructure[].


*             Only append if the item is a table and not a structure or
*             data element
              IF NOT wadictionary-istructure[] IS INITIAL.
                APPEND wadictionary TO ilocdictionary.
              ENDIF.
            ENDIF.
            CLEAR wadictionary.
          ENDIF.

          linetype = ''.
      ENDCASE.
    ENDIF.
  ENDLOOP.
ENDFORM.                                              "scanForLikeOrType


*-----------------------------------------------------------------------
*  displayStatus...
*-----------------------------------------------------------------------
FORM displaystatus USING value(message)
                         value(delay).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = message
    EXCEPTIONS
      OTHERS     = 1.

  IF delay > 0.
    WAIT UP TO delay SECONDS.
  ENDIF.
ENDFORM.                                                  "displayStatus

*-----------------------------------------------------------------------
*  removeLeadingZeros...
*-----------------------------------------------------------------------
FORM removeleadingzeros CHANGING myvalue.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = myvalue
    IMPORTING
      output = myvalue
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                                             "removeLeadingZeros

*-----------------------------------------------------------------------
* determineFrontendOPSystem.... Determine the frontend operating system
*                               type.
*-----------------------------------------------------------------------
FORM determinefrontendopsystem USING separator
                                     operatingsystem.

  DATA: platformid TYPE i VALUE 0.

  CREATE OBJECT objfile.

  CALL METHOD objfile->get_platform
    RECEIVING
      platform     = platformid
    EXCEPTIONS
      cntl_error   = 1
      error_no_gui = 2.
  CASE platformid.
    WHEN objfile->platform_windows95
         OR objfile->platform_windows98
         OR objfile->platform_nt351
         OR objfile->platform_nt40
         OR objfile->platform_nt50
         OR objfile->platform_mac
         OR objfile->platform_os2
         OR 14.      "XP
      separator = '\'.
      operatingsystem = non_unix.
    WHEN OTHERS.
      separator = '/'.
      operatingsystem = unix.
  ENDCASE.
ENDFORM.                                      "determineFrontendOpSystem

*-----------------------------------------------------------------------
* determineServerOPSystem.... Determine the server operating system type
.
*-----------------------------------------------------------------------
FORM determineserveropsystem USING separator
                                   serverfilesystem
                                   serveropsystem.

  DATA: tempfilesystem(40).

* Find the file system
  SELECT SINGLE filesys
                FROM opsystem
                INTO serverfilesystem
                WHERE opsys = sy-opsys.

  tempfilesystem = serverfilesystem.
  TRANSLATE tempfilesystem TO UPPER CASE.
  IF tempfilesystem CS 'WINDOWS'.
    separator = '\'.
    serveropsystem = non_unix.
  ELSE.
    IF tempfilesystem CS 'DOS'.
      separator = '\'.
      serveropsystem = non_unix.
    ELSE.
      separator = '/'.
      serveropsystem = unix.
    ENDIF.
  ENDIF.
ENDFORM.                                        "determineServerOpSystem

*-----------------------------------------------------------------------
* findExternalCommand.... Determine if the external command exists.  If
*                         it doesn't then disable the server input field
*-----------------------------------------------------------------------
*FORM findexternalcommand.
*
*  CALL FUNCTION 'SXPG_COMMAND_CHECK'
*       EXPORTING
*            commandname       = 'ZMKDIR'
*            operatingsystem   = sy-opsys
*       EXCEPTIONS
*            command_not_found = 1
*            OTHERS            = 0.
*
*  IF sy-subrc <> 0.
*    LOOP AT SCREEN.
*      IF screen-name = 'PLOGICAL'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF screen-name = 'PSERV'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF screen-name = 'PPC'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
*    MESSAGE s000(oo) WITH 'Download to server disabled,'
*                          'external command ZMKDIR not defined.'.
*  ENDIF.
*ENDFORM.

************************************************************************
**********************DOWNLOAD ROUTINES*********************************
************************************************************************
*-----------------------------------------------------------------------
* downloadDDStructures... download database objects to file
*-----------------------------------------------------------------------
FORM downloadddstructures USING ilocdictionary LIKE idictionary[]
                                value(pathname)
                                value(htmlfileextension)
                                value(subdir)
                                value(sorttablesasc)
                                value(slashseparator)
*                                value(savetoserver)
                                value(displayprogressmessage).

  FIELD-SYMBOLS: <wadictionary> TYPE tdicttable.
  DATA: tablefilename TYPE string.
  DATA: tablefilenamewithpath TYPE string.
  DATA: ihtmltable TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: newsubdirectory TYPE string.
  DATA: completesavepath TYPE string.

  LOOP AT ilocdictionary ASSIGNING <wadictionary>.
    PERFORM buildfilename USING pathname
                                subdir
                                <wadictionary>-tablename
                                space
                                space
                                htmlfileextension
                                is_table
*                                savetoserver
                                slashseparator
                                tablefilenamewithpath
                                tablefilename
                                newsubdirectory
                                completesavepath.

*   Try and import a converted table to memory as it will be much
*   quicker than converting it again
    IMPORT ihtmltable FROM MEMORY ID <wadictionary>-tablename.
    IF sy-subrc <> 0.
      CONCATENATE 'Converting table' <wadictionary>-tablename 'to html'
                  INTO statusbarmessage SEPARATED BY space.
      PERFORM displaystatus USING statusbarmessage 0.

      PERFORM convertddtohtml USING <wadictionary>-istructure[]
                                    ihtmltable[]
                                    <wadictionary>-tablename
                                    <wadictionary>-tabletitle
                                    sorttablesasc.

      EXPORT ihtmltable TO MEMORY ID <wadictionary>-tablename.
    ENDIF.

*    IF savetoserver IS INITIAL.
    PERFORM savefiletopc USING ihtmltable[]
                               tablefilenamewithpath
                               tablefilename
                               space
                               space
                               displayprogressmessage.
*    ELSE.
*      PERFORM savefiletoserver USING ihtmltable[]
*                                     tablefilenamewithpath
*                                     tablefilename
*                                     completesavepath
*                                     displayprogressmessage.
*    ENDIF.

    CLEAR ihtmltable[].
  ENDLOOP.
ENDFORM.                                           "downloadDDStructures

*-----------------------------------------------------------------------
* downloadMessageClass...
*-----------------------------------------------------------------------
FORM downloadmessageclass USING ilocmessages LIKE imessages[]
                                value(messageclassname)
                                value(userfilepath)
                                value(fileextension)
                                value(htmlfileflag)
                                      subdir
                                value(syntaxhighlightcomments)
                                value(customernamerange)
                                value(getincludes)
                                value(getdictstructures)
                                value(userhasselectedmessageclasses)
                                value(slashseparator)
*                                value(savetoserver)
                                value(displayprogressmessage).

  DATA: htmlpagename TYPE string.
  DATA: newfilenameonly TYPE string.
  DATA: newfilenamewithpath TYPE string.
  DATA: ihtmltable TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: newsubdirectory TYPE string.
  DATA: completesavepath TYPE string.

  PERFORM appendmessagestofile USING ilocmessages[]
                                     ihtmltable[]
                                     userhasselectedmessageclasses.


  CONCATENATE 'message class' messageclassname
              INTO htmlpagename SEPARATED BY space.

  IF htmlfileflag IS INITIAL.
    APPEND '' TO ihtmltable.
    APPEND
'----------------------------------------------------------------------'
 TO ihtmltable.

    PERFORM buildfootermessage USING 'TEXT'
                                     ihtmltable.
    APPEND ihtmltable.
  ELSE.
    PERFORM convertcodetohtml USING ihtmltable[]
                                    htmlpagename
                                    space
                                    is_messageclass
                                    ''
                                    false
                                    syntaxhighlightcomments
                                    fileextension
                                    customernamerange
                                    getincludes
                                    getdictstructures.
  ENDIF.

  PERFORM buildfilename USING userfilepath
                              subdir
                              messageclassname
                              space
                              space
                              fileextension
                              is_messageclass
*                              savetoserver
                              slashseparator
                              newfilenamewithpath
                              newfilenameonly
                              newsubdirectory
                              completesavepath.

*  IF savetoserver IS INITIAL.
  PERFORM savefiletopc USING ihtmltable[]
                             newfilenamewithpath
                             newfilenameonly
                             space
                             space
                             displayprogressmessage.
*  ELSE.
**     Save the file to the SAP server
*    PERFORM savefiletoserver USING ihtmltable[]
*                                   newfilenamewithpath
*                                   newfilenameonly
*                                   completesavepath
*                                   displayprogressmessage.
*  ENDIF.
ENDFORM.                                           "downloadMessageClass

*-----------------------------------------------------------------------
*  appendMessagesToFile
*-----------------------------------------------------------------------
FORM appendmessagestofile USING ilocmessages LIKE imessages[]
                                ilochtml LIKE dumihtml[]
                                value(userhasselectedmessageclasses).

  DATA: previousmessageid LIKE imessages-arbgb.
  FIELD-SYMBOLS: <wamessage> TYPE tmessage.
  DATA: wahtml TYPE string.

  SORT ilocmessages ASCENDING BY arbgb msgnr.

  IF NOT ilocmessages[] IS INITIAL.
    IF userhasselectedmessageclasses IS INITIAL.
*     Only add these extra lines if we are actually appending them to
*     the end of some program code
      APPEND wahtml TO ilochtml.
      APPEND wahtml TO ilochtml.

      APPEND '*Messages' TO ilochtml.
      APPEND
      '*----------------------------------------------------------'
      TO ilochtml.
    ENDIF.

    LOOP AT ilocmessages ASSIGNING <wamessage>.
      IF ( <wamessage>-arbgb <> previousmessageid ).

        IF userhasselectedmessageclasses IS INITIAL.
*         Only add this extra lines if we are actually appending them to
*         the end of some program code
          APPEND '*' TO ilochtml.
          CONCATENATE '* Message class:' <wamessage>-arbgb
                      INTO wahtml SEPARATED BY space.
          APPEND wahtml TO ilochtml.
        ENDIF.

        previousmessageid = <wamessage>-arbgb.
        CLEAR wahtml.
      ENDIF.

      IF userhasselectedmessageclasses IS INITIAL.
*       Only add this extra lines if we are actually appending them to
*       the end of some program code
        CONCATENATE '*' <wamessage>-msgnr <wamessage>-text
                                       INTO wahtml SEPARATED BY space.
      ELSE.
        CONCATENATE <wamessage>-msgnr  <wamessage>-text
                                       INTO wahtml SEPARATED BY space.
      ENDIF.

      APPEND wahtml TO ilochtml.
    ENDLOOP.
  ENDIF.
ENDFORM.                                           "appendMessagesToFile

*-----------------------------------------------------------------------
*  downloadFunctions...       Download function modules to file.
*-----------------------------------------------------------------------
FORM downloadfunctions USING ilocfunctions LIKE ifunctions[]
                             value(userfilepath)
                             value(fileextension)
                             value(subdir)
                             value(downloaddocumentation)
                             value(converttohtml)
                             value(syntaxhighlightcomments)
                             value(customernamerange)
                             value(getincludes)
                             value(getdictstruct)
                             value(textfileextension)
                             value(htmlfileextension)
                             value(sorttablesasc)
                             value(slashseparator)
*                             value(savetoserver)
                             value(displayprogressmessage).

  DATA: mainsubdir TYPE string.
  DATA: incsubdir TYPE string.
  FIELD-SYMBOLS: <wafunction> TYPE tfunction.
  FIELD-SYMBOLS: <wainclude> TYPE tinclude.
  DATA: iemptytextelements TYPE STANDARD TABLE OF ttexttable.
  DATA: iemptyselectiontexts TYPE STANDARD TABLE OF ttexttable.
  DATA: iemptymessages TYPE STANDARD TABLE OF tmessage.
  DATA: iemptyguititles TYPE STANDARD TABLE OF tguititle.
  DATA: functiondocumentationexists TYPE i VALUE false.

  LOOP AT ilocfunctions ASSIGNING <wafunction>.
    IF subdir IS INITIAL.
      incsubdir = <wafunction>-functionname.
      mainsubdir = ''.
    ELSE.
      CONCATENATE subdir <wafunction>-functionname
        INTO incsubdir SEPARATED BY slashseparator.
      mainsubdir = subdir.
    ENDIF.

    IF NOT downloaddocumentation IS INITIAL.
      PERFORM downloadfunctiondocs USING <wafunction>-functionname
                                         <wafunction>-functiontitle
                                         userfilepath
                                         fileextension
                                         converttohtml
                                         slashseparator
*                                         savetoserver
                                         displayprogressmessage
                                         mainsubdir
                                         functiondocumentationexists.
    ENDIF.

*   Download main source code
    PERFORM readfunctionanddownload USING <wafunction>-itextelements[]
                                          <wafunction>-iselectiontexts[]
                                          <wafunction>-imessages[]
                                          <wafunction>-functionname
                                        <wafunction>-functionmaininclude
                                          <wafunction>-functiontitle
                                          userfilepath
                                          fileextension
                                          mainsubdir
                                          converttohtml
                                          functiondocumentationexists
                                          syntaxhighlightcomments
                                          customernamerange
                                          getincludes
                                          getdictstruct
                                          slashseparator
*                                          savetoserver
                                          displayprogressmessage.

*   Download top include
    PERFORM readincludeanddownload USING iemptytextelements[]
                                         iemptyselectiontexts[]
                                         iemptymessages[]
                                         iemptyguititles[]
                                         <wafunction>-topincludename
                                         <wafunction>-functionname
                                         <wafunction>-functiontitle
                                         is_function
                                         userfilepath
                                         fileextension
                                         mainsubdir
                                         converttohtml
                                         syntaxhighlightcomments
                                         customernamerange
                                         getincludes
                                         getdictstruct
                                         slashseparator
*                                         savetoserver
                                         displayprogressmessage.

*   Download screens.
    IF NOT <wafunction>-iscreenflow[] IS INITIAL.
      PERFORM downloadscreens USING <wafunction>-iscreenflow[]
                                    <wafunction>-progname
                                    userfilepath
                                    textfileextension
                                    mainsubdir
                                    slashseparator
*                                    savetoserver
                                    displayprogressmessage.
    ENDIF.

*   Download GUI titles
    IF NOT <wafunction>-iguititle[] IS INITIAL.
      PERFORM downloadguititles USING <wafunction>-iguititle
                                      userfilepath
                                      textfileextension
                                      mainsubdir
                                      slashseparator
*                                      savetoserver
                                      displayprogressmessage.
    ENDIF.

*   Download all other includes
    LOOP AT <wafunction>-iincludes ASSIGNING <wainclude>.
      PERFORM readincludeanddownload USING iemptytextelements[]
                                           iemptyselectiontexts[]
                                           iemptymessages[]
                                           iemptyguititles[]
                                           <wainclude>-includename
                                           space
                                           <wainclude>-includetitle
                                           is_program
                                           userfilepath
                                           fileextension
                                           incsubdir
                                           converttohtml
                                           syntaxhighlightcomments
                                           customernamerange
                                           getincludes
                                           getdictstruct
                                           slashseparator
*                                           savetoserver
                                           displayprogressmessage.

    ENDLOOP.

*   Download all dictionary structures
    IF NOT <wafunction>-idictstruct[] IS INITIAL.
      PERFORM downloadddstructures USING <wafunction>-idictstruct[]
                                         userfilepath
                                         htmlfileextension
                                         incsubdir
                                         sorttablesasc
                                         slashseparator
*                                         savetoserver
                                         displayprogressmessage.
    ENDIF.
  ENDLOOP.
ENDFORM.                                              "downloadFunctions

*-----------------------------------------------------------------------
*   readIcludeAndDownload...
*-----------------------------------------------------------------------
FORM readincludeanddownload USING iloctextelements LIKE dumitexttab[]
                                  ilocselectiontexts LIKE dumitexttab[]
                                  ilocmessages LIKE imessages[]
                                  ilocguititles LIKE dumiguititle[]
                                  value(programname)
                                  value(functionname)
                                  value(shorttext)
                                  value(overideprogtype)
                                  value(userfilepath)
                                  value(fileextension)
                                  value(additionalsubdir)
                                  value(converttohtml)
                                  value(syntaxhighlightcomments)
                                  value(customernamerange)
                                  value(getincludes)
                                  value(getdictstructures)
                                  value(slashseparator)
*                                  value(savetoserver)
                                  value(displayprogressmessage).

  DATA: ilines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: localfilenamewithpath TYPE string.
  DATA: localfilenameonly TYPE string.
  DATA: newsubdirectory TYPE string.
  DATA: objectname TYPE string.
  DATA: completesavepath TYPE string.

  READ REPORT programname INTO ilines.

* Download GUI titles for main program
  IF NOT ilocguititles[] IS INITIAL.
    PERFORM appendguititles USING ilocguititles[]
                                  ilines[].
  ENDIF.

* Download text elements for main program
  IF NOT iloctextelements[] IS INITIAL.
    PERFORM appendtextelements USING iloctextelements[]
                                     ilines[].
  ENDIF.

* Download selection texts for main program
  IF NOT ilocselectiontexts[] IS INITIAL.
    PERFORM appendselectiontexts USING ilocselectiontexts[]
                                       ilines[].
  ENDIF.

* Download messages classes for main program.
  IF NOT ilocmessages[] IS INITIAL.
    PERFORM appendmessagestofile USING ilocmessages[]
                                       ilines[]
                                       space.
  ENDIF.

  IF converttohtml IS INITIAL.
    APPEND '' TO ilines.
    APPEND
'----------------------------------------------------------------------'
 TO ilines.
    PERFORM buildfootermessage USING 'TEXT'
                                     ilines.
    APPEND ilines.
  ELSE.
    PERFORM convertcodetohtml USING ilines[]
                                    programname
                                    shorttext
                                    overideprogtype
                                    space
                                    space
                                    syntaxhighlightcomments
                                    fileextension
                                    customernamerange
                                    getincludes
                                    getdictstructures.
  ENDIF.

  IF functionname IS INITIAL.
    objectname = programname.
  ELSE.
    objectname = functionname.
  ENDIF.

  PERFORM buildfilename USING userfilepath
                              additionalsubdir
                              objectname
                              space
                              programname
                              fileextension
                              overideprogtype
*                              savetoserver
                              slashseparator
                              localfilenamewithpath
                              localfilenameonly
                              newsubdirectory
                              completesavepath.

*  IF savetoserver IS INITIAL.
  PERFORM savefiletopc USING ilines[]
                             localfilenamewithpath
                             localfilenameonly
                             space
                             space
                             displayprogressmessage.
*  ELSE.
*    PERFORM savefiletoserver USING ilines[]
*                                   localfilenamewithpath
*                                   localfilenameonly
*                                   completesavepath
*                                   displayprogressmessage.
*  ENDIF.
ENDFORM.                                         "readIncludeAndDownload

*-----------------------------------------------------------------------
*   readFunctionAndDownload...
*-----------------------------------------------------------------------
FORM readfunctionanddownload USING iloctextelements LIKE dumitexttab[]
                                   ilocselectiontexts LIKE dumitexttab[]
                                   ilocmessages LIKE imessages[]
                                   value(functionname)
                                   value(functioninternalname)
                                   value(shorttext)
                                   value(userfilepath)
                                   value(fileextension)
                                   value(subdir)
                                   value(converttohtml)
                                   value(functiondocumentationexists)
                                   value(syntaxhighlightcomments)
                                   value(customernamerange)
                                   value(getincludes)
                                   value(getdictstructures)
                                   value(slashseparator)
*                                   value(savetoserver)
                                   value(displayprogressmessage).

  DATA: ilines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: localfilenamewithpath TYPE string.
  DATA: localfilenameonly TYPE string.
  DATA: newsubdirectory TYPE string.
  DATA: completesavepath TYPE string.

  READ REPORT functioninternalname INTO ilines.

* If we found any text elements for this function then we ought to
* append them to the main include.
  IF NOT iloctextelements[] IS INITIAL.
    PERFORM appendtextelements USING iloctextelements[]
                                     ilines[].
  ENDIF.

* If we found any message classes for this function then we ought to
* append them to the main include.
  IF NOT ilocmessages[] IS INITIAL.
    PERFORM appendmessagestofile USING ilocmessages[]
                                       ilines[]
                                       space.
  ENDIF.

  IF converttohtml IS INITIAL.
    APPEND '' TO ilines.
    APPEND
'----------------------------------------------------------------------'
 TO ilines.
    PERFORM buildfootermessage USING 'TEXT'
                                     ilines.
    APPEND ilines.
  ELSE.
    PERFORM convertfunctiontohtml USING ilines[]
                                        functionname
                                        shorttext
                                        is_function
                                        functiondocumentationexists
                                        true
                                        syntaxhighlightcomments
                                        fileextension
                                        customernamerange
                                        getincludes
                                        getdictstructures.
  ENDIF.

  PERFORM buildfilename USING userfilepath
                              subdir
                              functionname
                              space
                              space
                              fileextension
                              is_function
*                              savetoserver
                              slashseparator
                              localfilenamewithpath
                              localfilenameonly
                              newsubdirectory
                              completesavepath.

*  IF savetoserver IS INITIAL.
  PERFORM savefiletopc USING ilines[]
                             localfilenamewithpath
                             localfilenameonly
                             space
                             space
                             displayprogressmessage.
*  ELSE.
*    PERFORM savefiletoserver USING ilines[]
*                                   localfilenamewithpath
*                                   localfilenameonly
*                                   completesavepath
*                                   displayprogressmessage.
*  ENDIF.
ENDFORM.                                        "readFunctionAndDownload

*-----------------------------------------------------------------------
*  buildFilename...
*-----------------------------------------------------------------------
FORM buildfilename USING value(userpath)
                         value(additionalsubdirectory)
                         value(objectname)
                         value(mainfunctionno)
                         value(includename)
                         value(fileextension)
                         value(downloadtype)
*                         value(downloadtoserver)
                         value(slashseparator)
                               newfilenamewithpath
                               newfilenameonly
                               newsubdirectory
                               completepath.

* If we are running on a non UNIX environment we will need to remove
* forward slashes from the additional path.
*  IF downloadtoserver IS INITIAL.
  IF frontendopsystem = non_unix.
    IF NOT additionalsubdirectory IS INITIAL.
      TRANSLATE additionalsubdirectory USING '/_'.
      IF additionalsubdirectory+0(1) = '_'.
        SHIFT additionalsubdirectory LEFT BY 1 PLACES.
      ENDIF.
    ENDIF.
  ENDIF.
*  ELSE.
*    IF serveropsystem = non_unix.
*      IF NOT additionalsubdirectory IS INITIAL.
*        TRANSLATE additionalsubdirectory USING '/_'.
*        IF additionalsubdirectory+0(1) = '_'.
*          SHIFT additionalsubdirectory LEFT BY 1 PLACES.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  CASE downloadtype.
*   Programs
    WHEN is_program.
      IF additionalsubdirectory IS INITIAL.
        CONCATENATE userpath
                   slashseparator
                   objectname period
                   fileextension INTO newfilenamewithpath.
        CONCATENATE userpath slashseparator INTO completepath.
      ELSE.
        CONCATENATE userpath slashseparator additionalsubdirectory
                             slashseparator objectname period
                             fileextension
                             INTO newfilenamewithpath.
        CONCATENATE userpath slashseparator additionalsubdirectory
                    INTO completepath.
      ENDIF.

*   Function Modules
    WHEN is_function.
      IF additionalsubdirectory IS INITIAL.
        IF includename CS 'top' OR includename CS 'TOP'.
          CONCATENATE userpath slashseparator objectname
                               slashseparator 'Global-' objectname
                               period fileextension
                               INTO newfilenamewithpath.
        ELSE.
          IF includename CS mainfunctionno
            AND NOT mainfunctionno IS INITIAL.
            CONCATENATE userpath slashseparator objectname
                                 slashseparator objectname
                                 period fileextension
                                 INTO newfilenamewithpath.
          ELSE.
            CONCATENATE userpath slashseparator objectname
                                 slashseparator objectname
                                 period fileextension
                                 INTO newfilenamewithpath.
          ENDIF.
        ENDIF.
        newsubdirectory = objectname.
        CONCATENATE userpath slashseparator INTO completepath.
      ELSE.
        IF includename CS 'top' OR includename CS 'TOP'.
          CONCATENATE userpath slashseparator additionalsubdirectory
                               slashseparator objectname
                               slashseparator 'Global-' objectname
                               period fileextension
                               INTO newfilenamewithpath.
        ELSE.
          IF includename CS mainfunctionno
             AND NOT mainfunctionno IS INITIAL.
            CONCATENATE userpath slashseparator additionalsubdirectory
                                 slashseparator objectname
                                 slashseparator objectname
                                 period fileextension
                                 INTO newfilenamewithpath.
          ELSE.
            CONCATENATE userpath slashseparator additionalsubdirectory
                                 slashseparator objectname
                                 slashseparator objectname
                                 period fileextension
                                 INTO newfilenamewithpath.
          ENDIF.
        ENDIF.
        CONCATENATE additionalsubdirectory
                    slashseparator objectname
                    INTO newsubdirectory.
        CONCATENATE userpath slashseparator additionalsubdirectory
                    slashseparator objectname INTO completepath.
      ENDIF.

*   Table definition
    WHEN is_table.
      IF additionalsubdirectory IS INITIAL.
        CONCATENATE userpath slashseparator objectname
                             slashseparator 'Dictionary-'
                             objectname period fileextension
                             INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator objectname
                    INTO newsubdirectory.
        CONCATENATE userpath slashseparator objectname INTO completepath
                   .
      ELSE.
        CONCATENATE userpath slashseparator additionalsubdirectory
                             slashseparator objectname
                             slashseparator 'Dictionary-'
                             objectname period fileextension
                             INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator additionalsubdirectory
                    slashseparator objectname INTO newsubdirectory.
        CONCATENATE userpath slashseparator additionalsubdirectory
                    slashseparator objectname INTO completepath.
      ENDIF.

*   Program & Function documentation
    WHEN is_documentation.
      IF additionalsubdirectory IS INITIAL.
        CONCATENATE userpath slashseparator objectname
                             slashseparator 'Docs-'
                             objectname period
                             fileextension
                             INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator objectname
                    INTO newsubdirectory.
        CONCATENATE userpath slashseparator objectname
                    INTO completepath.
      ELSE.
        CONCATENATE userpath slashseparator additionalsubdirectory
                             slashseparator objectname
                             slashseparator 'Docs-'
                             objectname period fileextension
                             INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator
                   additionalsubdirectory
                   slashseparator objectname INTO newsubdirectory.
        CONCATENATE userpath slashseparator
                    additionalsubdirectory
                    slashseparator objectname INTO completepath.
      ENDIF.

*   Screens
    WHEN is_screen.
      IF additionalsubdirectory IS INITIAL.
        CONCATENATE userpath slashseparator 'Screens'
                             slashseparator 'screen_'
                             objectname period
                             fileextension INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator 'screens'
                    INTO newsubdirectory.
        CONCATENATE userpath slashseparator 'screens' INTO completepath.

      ELSE.
        CONCATENATE userpath slashseparator additionalsubdirectory
                             slashseparator 'Screens'
                             slashseparator 'screen_'
                             objectname period
                             fileextension INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator
                    additionalsubdirectory
                    slashseparator 'screens' INTO newsubdirectory.
        CONCATENATE userpath slashseparator additionalsubdirectory
                    slashseparator 'screens' INTO completepath.
      ENDIF.

*   GUI title
    WHEN is_guititle.
      IF additionalsubdirectory IS INITIAL.
        CONCATENATE userpath slashseparator 'Screens'
                             slashseparator 'gui_title_'
                             objectname period
                             fileextension INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator 'screens'
                    INTO newsubdirectory.
        CONCATENATE userpath slashseparator 'screens' INTO completepath.
      ELSE.
        CONCATENATE userpath slashseparator additionalsubdirectory
                             slashseparator 'Screens'
                             slashseparator 'gui_title_'
                             objectname period
                             fileextension INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator
                    additionalsubdirectory
                    slashseparator 'Screens' INTO newsubdirectory.
        CONCATENATE userpath slashseparator
                    additionalsubdirectory
                    slashseparator 'Screens' INTO completepath.
      ENDIF.

*   Message Class
    WHEN is_messageclass.
      IF additionalsubdirectory IS INITIAL.
        CONCATENATE userpath slashseparator objectname
                             slashseparator 'Message class-'
                             objectname period
                             fileextension
                             INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator objectname
                    INTO newsubdirectory.
        CONCATENATE userpath slashseparator objectname
                    INTO completepath.
      ELSE.
        CONCATENATE userpath slashseparator additionalsubdirectory
                             slashseparator objectname
                             slashseparator 'Message class-'
                             objectname period fileextension
                             INTO newfilenamewithpath.

        CONCATENATE userpath slashseparator additionalsubdirectory
                    slashseparator objectname INTO newsubdirectory.
        CONCATENATE userpath slashseparator additionalsubdirectory
                    slashseparator objectname INTO completepath.
      ENDIF.
  ENDCASE.

  TRANSLATE completepath TO LOWER CASE.
  CONCATENATE objectname period fileextension INTO newfilenameonly.
  TRANSLATE newfilenameonly TO LOWER CASE.
  TRANSLATE newfilenamewithpath TO LOWER CASE.
  TRANSLATE newsubdirectory TO LOWER CASE.

* If we are running on a non UNIX environment we will need to remove
* incorrect characters from the filename.
*  IF downloadtoserver IS INITIAL.
  IF frontendopsystem = non_unix.
    TRANSLATE newfilenameonly USING '/_'.
    TRANSLATE newfilenamewithpath USING '/_'.
    TRANSLATE newfilenameonly USING '< '.
    TRANSLATE newfilenamewithpath USING '< '.
    TRANSLATE newfilenameonly USING '> '.
    TRANSLATE newfilenamewithpath USING '> '.
    TRANSLATE newfilenameonly USING '? '.
    TRANSLATE newfilenamewithpath USING '? '.
    TRANSLATE newfilenameonly USING '| '.
    TRANSLATE newfilenamewithpath USING '| '.
    CONDENSE newfilenameonly NO-GAPS.
    CONDENSE newfilenamewithpath NO-GAPS.
  ENDIF.
*  ELSE.
*    IF serveropsystem = non_unix.
*      TRANSLATE newfilenameonly USING '/_'.
*      TRANSLATE newfilenamewithpath USING '/_'.
*      TRANSLATE newfilenameonly USING '< '.
*      TRANSLATE newfilenamewithpath USING '< '.
*      TRANSLATE newfilenameonly USING '> '.
*      TRANSLATE newfilenamewithpath USING '> '.
*      TRANSLATE newfilenameonly USING '? '.
*      TRANSLATE newfilenamewithpath USING '? '.
*      TRANSLATE newfilenameonly USING '| '.
*      TRANSLATE newfilenamewithpath USING '| '.
*      CONDENSE newfilenameonly NO-GAPS.
*      CONDENSE newfilenamewithpath NO-GAPS.
*    ENDIF.
*  ENDIF.
ENDFORM.                                                  "buildFilename

*-----------------------------------------------------------------------
*  saveFileToPc...    write an internal table to a file on the local PC
*-----------------------------------------------------------------------
FORM savefiletopc USING idownload TYPE STANDARD TABLE
                        value(filenamewithpath)
                        value(filename)
                        value(writefieldseparator)
                        value(truncatetrailingblanks)
                        value(displayprogressmessage).

  DATA: statusmessage TYPE string.
  DATA: objfile TYPE REF TO cl_gui_frontend_services.
  DATA: strsubrc TYPE string.

  IF NOT displayprogressmessage IS INITIAL.
    CONCATENATE 'Downloading:' filename
       INTO statusmessage SEPARATED BY space.
    PERFORM displaystatus USING statusmessage 0.
  ENDIF.

  CREATE OBJECT objfile.
  CALL METHOD objfile->gui_download
    EXPORTING
      filename                = filenamewithpath
      filetype                = 'ASC'
      write_field_separator   = writefieldseparator
      trunc_trailing_blanks   = truncatetrailingblanks
    CHANGING
      data_tab                = idownload[]
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
      control_flush_error     = 21.

  IF sy-subrc <> 0.
    strsubrc = sy-subrc.
    CONCATENATE 'File save error:' filename 'sy-subrc:' strsubrc
                INTO statusmessage SEPARATED BY space.
    PERFORM displaystatus USING statusmessage 3.
  ENDIF.
ENDFORM.                                                   "saveFileToPc

*-----------------------------------------------------------------------
*  saveFileToServer...    write an internal table to a file on the SAP
*                         server
*-----------------------------------------------------------------------
*FORM savefiletoserver USING idownload TYPE STANDARD TABLE
*                            value(filenamewithpath)
*                            value(filename)
*                            value(path)
*                            value(displayprogressmessage).
*
*  DATA: wadownload TYPE string.
*  DATA: statusmessage TYPE string.
*
*  IF NOT displayprogressmessage IS INITIAL.
*    CONCATENATE 'Downloading:' filename
*      INTO statusmessage SEPARATED BY space.
*    PERFORM displaystatus USING statusmessage 0.
*  ENDIF.
*
*  READ TABLE iserverpaths WITH KEY table_line = path.
*  IF sy-subrc <> 0.
*    PERFORM createserverdirectory USING path.
*    APPEND path TO iserverpaths.
*  ENDIF.
*
*  OPEN DATASET filenamewithpath FOR OUTPUT IN TEXT MODE.
*  IF sy-subrc = 0.
*    LOOP AT idownload INTO wadownload.
*      TRANSFER wadownload TO filenamewithpath.
*      IF sy-subrc <> 0.
*        MESSAGE e000(oo) WITH 'Error transferring data to file'.
*      ENDIF.
*    ENDLOOP.
*
*    CLOSE DATASET filenamewithpath.
*    IF sy-subrc <> 0.
*      MESSAGE e000(oo) WITH 'Error closing file'.
*    ENDIF.
*  ELSE.
**   Unable to create a file
*    MESSAGE e000(oo) WITH 'Error creating file on SAP server'
*                          'check permissions'.
*  ENDIF.
*ENDFORM.
*"saveFileToServer

*-----------------------------------------------------------------------
* createServerDirectory...
*-----------------------------------------------------------------------
FORM createserverdirectory USING value(path).

*  Parameters for remove command.
  DATA: param1 TYPE sxpgcolist-parameters.
*  Return status
  DATA: funcstatus TYPE extcmdexex-status.
*  Command line listing returned by the function
  DATA: iserveroutput TYPE STANDARD TABLE OF btcxpm.
  DATA: waserveroutput TYPE btcxpm.
*  Targetsystem type conversion variable.
  DATA: target TYPE rfcdisplay-rfchost.
* Operating system
  DATA: operatingsystem TYPE sxpgcolist-opsystem.
*  Head for split command.
  DATA: head TYPE string..
  DATA: tail TYPE string.

  param1 = path.
  target = sy-host.
  operatingsystem = sy-opsys.

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname                   = 'ZMKDIR'
      additional_parameters         = param1
      operatingsystem               = operatingsystem
      targetsystem                  = target
      stdout                        = 'X'
      stderr                        = 'X'
      terminationwait               = 'X'
    IMPORTING
      status                        = funcstatus
    TABLES
      exec_protocol                 = iserveroutput[]
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

  IF sy-subrc = 0.
*   Although the function succeded did the external command actually
*   work
    IF funcstatus = 'E'.
*     External command returned with an error
      IF sy-opsys CS 'Windows NT'.
        READ TABLE iserveroutput INDEX 1 INTO waserveroutput.
        IF waserveroutput-message NS 'already exists'.
*         An error occurred creating the directory on the server
          MESSAGE e000(oo)
            WITH 'An error occurred creating a directory'.
        ENDIF.
      ELSE.
        READ TABLE iserveroutput INDEX 2 INTO waserveroutput.
        SPLIT waserveroutput-message AT space INTO head tail.
        SHIFT tail LEFT DELETING LEADING space.
        IF tail <> 'Do not specify an existing file.'.
*         An error occurred creating the directory on the server
          MESSAGE e000(oo)
            WITH 'An error occurred creating a directory'.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    CASE sy-subrc.
      WHEN 1.
*       No permissions to run the command
        MESSAGE e000(oo)
          WITH 'No permissions to run external command ZMKDIR'.
      WHEN 2.
*       External command not found
        MESSAGE e000(oo) WITH 'External comand ZMKDIR not found'.

      WHEN OTHERS.
*       Unable to create the directory
        MESSAGE e000(oo) WITH 'An error occurred creating a directory'
                              ', subrc:'
                              sy-subrc.
    ENDCASE.
  ENDIF.
ENDFORM.                                          "createServerDirectory

*-----------------------------------------------------------------------
* appendTextElements...
*-----------------------------------------------------------------------
FORM appendtextelements USING iloctextelements LIKE dumitexttab[]
                              iloclines LIKE dumihtml[].

  DATA: tablelines TYPE i VALUE 0.
  FIELD-SYMBOLS: <watextelement> TYPE ttexttable.
  FIELD-SYMBOLS: <waselectiontext> TYPE ttexttable.
  DATA: waline TYPE string.

  DESCRIBE TABLE iloctextelements LINES tablelines.
  IF tablelines > 0.
    APPEND '' TO iloclines.

    APPEND '*Text elements' TO iloclines.
    APPEND '*----------------------------------------------------------'
                              TO  iloclines.
    LOOP AT iloctextelements ASSIGNING <watextelement>.
      CONCATENATE '*' <watextelement>-key <watextelement>-entry
                  INTO waline SEPARATED BY space.
      APPEND waline TO iloclines.
    ENDLOOP.
  ENDIF.
ENDFORM.                                             "appendTextElements

*-----------------------------------------------------------------------
* appendGUITitles...
*-----------------------------------------------------------------------
FORM appendguititles USING ilocguititles LIKE dumiguititle[]
                           iloclines LIKE dumihtml[].

  DATA: tablelines TYPE i VALUE 0.
  FIELD-SYMBOLS: <waguititle> TYPE tguititle.
  DATA: waline TYPE string.

  DESCRIBE TABLE ilocguititles LINES tablelines.
  IF tablelines > 0.
    APPEND '' TO iloclines.

    APPEND '*GUI Texts' TO iloclines.
    APPEND '*----------------------------------------------------------'
                         TO  iloclines.
    LOOP AT ilocguititles ASSIGNING <waguititle>.
      CONCATENATE '*  ' <waguititle>-obj_code '-->'
                  <waguititle>-text INTO waline SEPARATED BY space.
      APPEND waline TO iloclines.
    ENDLOOP.
  ENDIF.
ENDFORM.                                                "appendGUITitles

*-----------------------------------------------------------------------
* appendSelectionTexts...
*-----------------------------------------------------------------------
FORM appendselectiontexts USING ilocselectiontexts LIKE dumitexttab[]
                                iloclines LIKE dumihtml[].

  DATA: tablelines TYPE i VALUE 0.
  FIELD-SYMBOLS: <watextelement> TYPE ttexttable.
  FIELD-SYMBOLS: <waselectiontext> TYPE ttexttable.
  DATA: waline TYPE string.

  DESCRIBE TABLE ilocselectiontexts LINES tablelines.
  IF tablelines > 0.
    APPEND '' TO iloclines.
    APPEND '' TO iloclines.

    APPEND '*Selection texts' TO iloclines.
    APPEND '*----------------------------------------------------------'
                              TO  iloclines.
    LOOP AT ilocselectiontexts ASSIGNING <waselectiontext>.
      CONCATENATE '*' <waselectiontext>-key <waselectiontext>-entry
                  INTO waline SEPARATED BY space.
      APPEND waline TO iloclines.
    ENDLOOP.
  ENDIF.
ENDFORM.                                           "appendSelectionTexts

*-----------------------------------------------------------------------
* downloadFunctionDocs...
*-----------------------------------------------------------------------
FORM downloadfunctiondocs USING value(functionname)
                                value(functiondescription)
                                value(userfilepath)
                                value(fileextension)
                                value(converttohtml)
                                value(slashseparator)
*                                value(savetoserver)
                                value(displayprogressmessage)
                                      subdir
                                      documentationdownloaded.

  DATA: ilines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: idocumentation TYPE STANDARD TABLE OF funct WITH HEADER LINE.
  DATA: iexceptions TYPE STANDARD TABLE OF rsexc WITH HEADER LINE.
  DATA: iexport TYPE STANDARD TABLE OF rsexp WITH HEADER LINE.
  DATA: iparameter TYPE STANDARD TABLE OF rsimp WITH HEADER LINE.
  DATA: itables TYPE STANDARD TABLE OF rstbl WITH HEADER LINE.
  DATA: iscriptlines TYPE STANDARD TABLE OF tline WITH HEADER LINE.
  DATA: htmlpagename TYPE string.
  DATA: newfilenamewithpath TYPE string.
  DATA: newfilenameonly TYPE string.
  DATA: object LIKE dokhl-object.
  DATA: stringlength TYPE i VALUE 0.
  DATA: newsubdirectory TYPE string.
  DATA: waline(255).
  DATA: completesavepath TYPE string.

  documentationdownloaded = false.
  object = functionname.

  CALL FUNCTION 'FUNCTION_IMPORT_DOKU'
    EXPORTING
      funcname           = functionname
    TABLES
      dokumentation      = idocumentation
      exception_list     = iexceptions
      export_parameter   = iexport
      import_parameter   = iparameter
      tables_parameter   = itables
    EXCEPTIONS
      error_message      = 1
      function_not_found = 2
      invalid_name       = 3
      OTHERS             = 4.

  CALL FUNCTION 'DOCU_GET'
    EXPORTING
      id                     = 'FU'
      langu                  = sy-langu
      object                 = object
      typ                    = 'T'
      version_active_or_last = 'L'
    TABLES
      line                   = iscriptlines
    EXCEPTIONS
      no_docu_on_screen      = 1
      no_docu_self_def       = 2
      no_docu_temp           = 3
      ret_code               = 4
      OTHERS                 = 5.

  IF sy-subrc = 0 AND NOT ( iscriptlines[] IS INITIAL ).
    APPEND 'SHORT TEXT' TO ilines.
    CONCATENATE space functiondescription INTO functiondescription
                SEPARATED BY space.
    APPEND functiondescription TO ilines.
    APPEND space TO ilines.
    LOOP AT iscriptlines.
      MOVE iscriptlines-tdline TO ilines.
      CONCATENATE space ilines INTO ilines SEPARATED BY space.
      WHILE ilines CP '&*' OR ilines CP '*&'.
        REPLACE '&' INTO ilines WITH space.
        SHIFT ilines LEFT DELETING LEADING space.
      ENDWHILE.
      APPEND ilines.
    ENDLOOP.

    CLEAR ilines.
    IF NOT ( idocumentation[] IS INITIAL ).
      APPEND ilines.
      APPEND 'PARAMETER DOCUMENTATION' TO ilines.
      APPEND '-----------------------' TO ilines.
      APPEND ilines.

      DESCRIBE FIELD idocumentation-parameter LENGTH stringlength IN CHARACTER MODE.
      stringlength = stringlength + 3.
      LOOP AT idocumentation.
        MOVE idocumentation-parameter TO waline.
        MOVE idocumentation-stext TO waline+stringlength.
        APPEND waline TO ilines.
      ENDLOOP.
    ENDIF.

    CONCATENATE 'Documentation - ' functionname
       INTO htmlpagename SEPARATED BY space.

    IF converttohtml IS INITIAL.
      APPEND ilines.
      APPEND
'----------------------------------------------------------------------'
 TO ilines.
      APPEND ilines.
      PERFORM buildfootermessage USING 'TEXT'
                                       ilines.
      APPEND ilines.
    ELSE.
      PERFORM convertcodetohtml USING ilines[]
                                      htmlpagename
                                      space
                                      is_documentation
                                      true
                                      space
                                      space
                                      space
                                      space
                                      space
                                      space.
    ENDIF.

    PERFORM buildfilename USING userfilepath
                                subdir
                                functionname
                                space
                                space
                                fileextension
                                is_documentation
*                                savetoserver
                                slashseparator
                                newfilenamewithpath
                                newfilenameonly
                                newsubdirectory
                                completesavepath.

*    IF savetoserver IS INITIAL.
    PERFORM savefiletopc USING ilines[]
                               newfilenamewithpath
                               newfilenameonly
                               space
                               space
                               displayprogressmessage.
*    ELSE.
*      PERFORM savefiletoserver USING ilines[]
*                                     newfilenamewithpath
*                                     newfilenameonly
*                                     completesavepath
*                                     displayprogressmessage.
*    ENDIF.

    documentationdownloaded = true.
  ENDIF.
ENDFORM.                                           "downloadFunctionDocs

*-----------------------------------------------------------------------
*  downloadScreens...
*-----------------------------------------------------------------------
FORM downloadscreens USING ilocscreenflow LIKE dumiscreen[]
                           value(programname)
                           value(userfilepath)
                           value(textfileextension)
                           value(subdir)
                           value(slashseparator)
*                           value(savetoserver)
                           value(displayprogressmessage).


  TABLES: d020t.
  DATA: header LIKE d020s.
  DATA: ifields TYPE STANDARD TABLE OF d021s WITH HEADER LINE.
  DATA: iflowlogic TYPE STANDARD TABLE OF d022s WITH HEADER LINE.
  FIELD-SYMBOLS <wascreen> TYPE tscreenflow.
  DATA: wacharheader TYPE scr_chhead.
  DATA: iscreenchar TYPE STANDARD TABLE OF scr_chfld WITH HEADER LINE.
  DATA: ifieldschar TYPE STANDARD TABLE OF scr_chfld WITH HEADER LINE.
  DATA: stars(72) VALUE
  '****************************************************************'.
  DATA: comment1(72) VALUE
   '*   This file was generated by Direct Download Enterprise.     *'.
  DATA: comment2(72) VALUE
   '*   Please do not change it manually.                          *'.
  DATA: dynprotext(8) VALUE '%_DYNPRO'.
  DATA: headertext(8) VALUE '%_HEADER'.
  DATA: paramstex(8) VALUE '%_PARAMS'.
  DATA: descriptiontext(13) VALUE '%_DESCRIPTION'.
  DATA: fieldstext(8) VALUE '%_FIELDS'.
  DATA: flowlogictext(11) VALUE '%_FLOWLOGIC'.
  DATA: programlength TYPE string.
  DATA: newsubdirectory TYPE string.
  DATA: newfilenamewithpath TYPE string.
  DATA: newfilenameonly TYPE string.
  DATA: completesavepath TYPE string.

  LOOP AT ilocscreenflow ASSIGNING <wascreen>.
    CALL FUNCTION 'RS_IMPORT_DYNPRO'
      EXPORTING
        dylang = sy-langu
        dyname = programname
        dynumb = <wascreen>-screen
      IMPORTING
        header = header
      TABLES
        ftab   = ifields
        pltab  = iflowlogic.

    CALL FUNCTION 'RS_SCRP_HEADER_RAW_TO_CHAR'
      EXPORTING
        header_int  = header
      IMPORTING
        header_char = wacharheader
      EXCEPTIONS
        OTHERS      = 1.

*   Add in the top comments for the file
    APPEND stars TO iscreenchar .
    APPEND comment1 TO iscreenchar.
    APPEND comment2 TO iscreenchar.
    APPEND stars TO iscreenchar.

*   Screen identification
    APPEND dynprotext TO iscreenchar.
    APPEND wacharheader-prog TO iscreenchar.
    APPEND wacharheader-dnum TO iscreenchar.
    APPEND sy-saprl TO iscreenchar.
    DESCRIBE FIELD d020t-prog LENGTH programlength IN CHARACTER MODE.
    iscreenchar+16 = programlength.
    APPEND iscreenchar.

*   Header
    APPEND headertext TO iscreenchar.
    APPEND wacharheader TO iscreenchar.

*   Description text
    APPEND descriptiontext TO iscreenchar.
    SELECT SINGLE dtxt FROM d020t INTO iscreenchar
                       WHERE prog = programname
                             AND dynr = <wascreen>-screen
                             AND lang = sy-langu.
    APPEND iscreenchar.

*   Fieldlist text
    APPEND fieldstext TO iscreenchar.

    CALL FUNCTION 'RS_SCRP_FIELDS_RAW_TO_CHAR'
      TABLES
        fields_int  = ifields[]
        fields_char = ifieldschar[]
      EXCEPTIONS
        OTHERS      = 1.

    LOOP AT ifieldschar.
      MOVE-CORRESPONDING ifieldschar TO iscreenchar.
      APPEND iscreenchar.
    ENDLOOP.

*   Flowlogic text
    APPEND flowlogictext TO iscreenchar.
*   Flow logic.
    LOOP AT iflowlogic.
      APPEND iflowlogic TO iscreenchar.
    ENDLOOP.

    PERFORM buildfilename USING userfilepath
                                subdir
                                wacharheader-dnum
                                space
                                space
                                textfileextension
                                is_screen
*                                savetoserver
                                slashseparator
                                newfilenamewithpath
                                newfilenameonly
                                newsubdirectory
                                completesavepath.

*    IF savetoserver IS INITIAL.
*     Save the screen to the local computer
    PERFORM savefiletopc USING iscreenchar[]
                               newfilenamewithpath
                               newfilenameonly
                               'X'
                               'X'
                               displayprogressmessage.
*    ELSE.
**     Save the screen to the SAP server
*      PERFORM savefiletoserver USING iscreenchar[]
*                                     newfilenamewithpath
*                                     newfilenameonly
*                                     completesavepath
*                                     displayprogressmessage.
*    ENDIF.

    CLEAR header. CLEAR wacharheader.
    CLEAR iscreenchar[].
    CLEAR ifieldschar[].
    CLEAR ifields[].
    CLEAR iflowlogic[].
  ENDLOOP.
ENDFORM.                                                "downloadScreens

*-----------------------------------------------------------------------
*  downloadGUITitles..
*-----------------------------------------------------------------------
FORM downloadguititles USING ilocguititles LIKE dumiguititle[]
                             value(userfilepath)
                             value(textfileextension)
                             value(subdir)
                             value(slashseparator)
*                             value(savetoserver)
                             value(displayprogressmessage).

  DATA: ilines TYPE STANDARD TABLE OF string WITH HEADER LINE.
  FIELD-SYMBOLS: <waguititle> TYPE tguititle.
  DATA: newsubdirectory TYPE string.
  DATA: newfilenamewithpath TYPE string.
  DATA: newfilenameonly TYPE string.
  DATA: completesavepath TYPE string.

  LOOP AT ilocguititles ASSIGNING <waguititle>.
    APPEND <waguititle>-text TO ilines.

    PERFORM buildfilename USING userfilepath
                                subdir
                                <waguititle>-obj_code
                                space
                                space
                                textfileextension
                                is_guititle
*                                savetoserver
                                slashseparator
                                newfilenamewithpath
                                newfilenameonly
                                newsubdirectory
                                completesavepath.

*    IF savetoserver IS INITIAL.
    PERFORM savefiletopc USING ilines[]
                               newfilenamewithpath
                               newfilenameonly
                               space
                               space
                               displayprogressmessage.
*    ELSE.
*      PERFORM savefiletoserver USING ilines[]
*                                     newfilenamewithpath
*                                     newfilenameonly
*                                     completesavepath
*                                     displayprogressmessage.
*    ENDIF.

    CLEAR ilines[].
  ENDLOOP.
ENDFORM.                                              "downloadGUITitles

*-----------------------------------------------------------------------
*  downloadPrograms..
*-----------------------------------------------------------------------
FORM downloadprograms USING ilocprogram LIKE iprograms[]
                            ilocfunctions LIKE ifunctions[]
                            value(userfilepath)
                            value(fileextension)
                            value(htmlfileextension)
                            value(textfileextension)
                            value(converttohtml)
                            value(syntaxhighlightcomments)
                            value(customernamerange)
                            value(getincludes)
                            value(getdictstruct)
                            value(downloaddocumentation)
                            value(sorttablesasc)
                            value(slashseparator)
*                            value(savetoserver)
                            value(displayprogressmessage).


  DATA: iprogfunctions TYPE STANDARD TABLE OF tfunction WITH HEADER LINE.
  FIELD-SYMBOLS: <waprogram> TYPE tprogram.
  FIELD-SYMBOLS: <wainclude> TYPE tinclude.
  DATA: iemptytextelements TYPE STANDARD TABLE OF ttexttable.
  DATA: iemptyselectiontexts TYPE STANDARD TABLE OF ttexttable.
  DATA: iemptymessages TYPE STANDARD TABLE OF tmessage.
  DATA: iemptyguititles TYPE STANDARD TABLE OF tguititle.
  DATA: locconverttohtml(1).
  DATA: locfileextension TYPE string.

  SORT ilocprogram ASCENDING BY  prog.

  LOOP AT ilocprogram ASSIGNING <waprogram>.
*   if the program to download is this program then always download as
*   text otherwise you will get a rubbish file
    IF <waprogram>-prog = sy-cprog.
      locconverttohtml = ''.
      locfileextension = textextension.
    ELSE.
      locconverttohtml = converttohtml.
      locfileextension = fileextension.
    ENDIF.

*   Download the main program
    PERFORM readincludeanddownload USING <waprogram>-itextelements[]
                                         <waprogram>-iselectiontexts[]
                                         <waprogram>-imessages[]
                                         <waprogram>-iguititle[]
                                         <waprogram>-prog
                                         space
                                         <waprogram>-programtitle
                                         is_program
                                         userfilepath
                                         locfileextension
                                         <waprogram>-prog
                                         locconverttohtml
                                         syntaxhighlightcomments
                                         customernamerange
                                         getincludes
                                         getdictstruct
                                         slashseparator
*                                         savetoserver
                                         displayprogressmessage.

*   Download screens.
    IF NOT <waprogram>-iscreenflow[] IS INITIAL.
      PERFORM downloadscreens USING <waprogram>-iscreenflow[]
                                    <waprogram>-prog
                                    userfilepath
                                    textfileextension
                                    <waprogram>-prog
                                    slashseparator
*                                    savetoserver
                                    displayprogressmessage.
    ENDIF.

*   Download GUI titles
    IF NOT <waprogram>-iguititle[] IS INITIAL.
      PERFORM downloadguititles USING <waprogram>-iguititle
                                      userfilepath
                                      textfileextension
                                      <waprogram>-prog
                                      slashseparator
*                                      savetoserver
                                      displayprogressmessage.
    ENDIF.

*   Download all other includes
    LOOP AT <waprogram>-iincludes ASSIGNING <wainclude>.
      PERFORM readincludeanddownload USING iemptytextelements[]
                                           iemptyselectiontexts[]
                                           iemptymessages[]
                                           iemptyguititles[]
                                           <wainclude>-includename
                                           space
                                           <wainclude>-includetitle
                                           is_program
                                           userfilepath
                                           fileextension
                                           <waprogram>-prog
                                           converttohtml
                                           syntaxhighlightcomments
                                           customernamerange
                                           getincludes
                                           getdictstruct
                                           slashseparator
*                                           savetoserver
                                           displayprogressmessage.

    ENDLOOP.

*   Download all dictionary structures
    IF NOT <waprogram>-idictstruct[] IS INITIAL.
      PERFORM downloadddstructures USING <waprogram>-idictstruct[]
                                         userfilepath
                                         htmlfileextension
                                         <waprogram>-prog
                                         sorttablesasc
                                         slashseparator
*                                         savetoserver
                                         displayprogressmessage.
    ENDIF.

*   Download any functions used by these programs
    LOOP AT ilocfunctions INTO iprogfunctions
                          WHERE programlinkname = <waprogram>-prog.
      APPEND iprogfunctions.
    ENDLOOP.

    IF NOT iprogfunctions[] IS INITIAL.
      PERFORM downloadfunctions USING iprogfunctions[]
                                      userfilepath
                                      fileextension
                                      <waprogram>-prog
                                      downloaddocumentation
                                      converttohtml
                                      syntaxhighlightcomments
                                      customernamerange
                                      getincludes
                                      getdictstruct
                                      textfileextension
                                      htmlfileextension
                                      sorttablesasc
                                      slashseparator
*                                      savetoserver
                                      displayprogressmessage.
      CLEAR iprogfunctions[].
    ENDIF.
  ENDLOOP.
ENDFORM.                                               "downloadPrograms

*-----------------------------------------------------------------------
*  convertDDToHTML...   Convert text description to HTML
*-----------------------------------------------------------------------
FORM convertddtohtml USING ilocdictstructure LIKE dumidictstructure[]
                           ilochtml LIKE dumihtml[]
                           value(tablename)
                           value(tabletitle)
                           value(sorttablesasc).

  DATA: icolumncaptions TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: wadictionary TYPE tdicttablestructure.
  DATA: wahtml TYPE string.
  DATA: title TYPE string.

  PERFORM buildcolumnheaders USING icolumncaptions[].

* Add a html header to the table
  CONCATENATE 'Dictionary object-' tablename INTO
            title SEPARATED BY space.
  PERFORM addhtmlheader USING ilochtml[]
                              title.

  CONCATENATE '<h2>' tablename '</h2>' INTO wahtml.
  APPEND wahtml TO ilochtml.
  APPEND '' TO ilochtml.

  CONCATENATE '<h3>' tabletitle '</h3>' INTO wahtml.
  APPEND wahtml TO ilochtml.
  APPEND '' TO ilochtml.

* Do we need to sort the fields into alphabetical order
  IF NOT sorttablesasc IS INITIAL.
    SORT ilocdictstructure ASCENDING BY fieldname.
  ENDIF.

  PERFORM convertitabtohtml USING icolumncaptions[]
                                  ilocdictstructure[]
                                  ilochtml
                                  'X'
                                  colour_black
                                  ''
                                  colour_yellow
                                  ''
                                  background_colour
                                  'Arial'
                                  'green'
                                  '1'
                                  '1'.

* Add a html footer to the table
  APPEND '<br>' TO ilochtml.
  PERFORM addhtmlfooter USING ilochtml[].
ENDFORM.                                                "convertDDToHTML

*-----------------------------------------------------------------------
*  convertITABtoHtml... produces a html table from an internal table
*-----------------------------------------------------------------------
FORM convertitabtohtml USING ilocheader LIKE dumiheader[]
                             ilocdictstructure LIKE dumidictstructure[]
                             ilochtml LIKE dumihtml[]
                             value(includerowcount)
                             headingbackcolour
                             headingfontname
                             headingfontcolour
                             headingfontsize
                             bodybackcolour
                             bodyfontname
                             bodyfontcolour
                             bodyfontsize
                             bordersize.

* Holds one cell from the internal table
  FIELD-SYMBOLS: <fsfield>.
* The value of one cell form the internal table
  DATA: wtextcell TYPE string.
* work area for putting the CSV value into
  DATA: wacsvtable TYPE string.
* Have we used any font tags in the html code
  DATA: usedafontattribute TYPE i VALUE 0.
* Work area for HTML table
  DATA: wahtml TYPE string.
* Loop counter for adding row numbers onto the output table
  DATA: loopcounter TYPE string.
* Work area for header table
  FIELD-SYMBOLS: <waheader> TYPE string.
  FIELD-SYMBOLS: <ilocdictstructure> TYPE tdicttablestructure.

  CONCATENATE '<table border="' bordersize '">' INTO wahtml.
  APPEND wahtml TO ilochtml.

  IF NOT ilocheader[] IS INITIAL.
    APPEND '<tr>' TO ilochtml.
  ENDIF.

  LOOP AT ilocheader ASSIGNING <waheader>.
    IF headingbackcolour IS INITIAL.
      wahtml = '<th>'.
    ELSE.
      CONCATENATE '<th bgcolor="' headingbackcolour '">' INTO wahtml.
    ENDIF.

    IF NOT headingfontname IS INITIAL
      OR NOT headingfontcolour IS INITIAL
      OR NOT headingfontsize IS INITIAL.
      CONCATENATE wahtml '<font' INTO wahtml.

*      Add the font name
      IF NOT headingfontname IS INITIAL.
        CONCATENATE wahtml ' face ="' INTO wahtml.
        CONCATENATE wahtml headingfontname '"' INTO wahtml.
      ENDIF.

*      Add the font colour
      IF NOT headingfontcolour IS INITIAL.
        CONCATENATE wahtml ' color ="' INTO wahtml.
        CONCATENATE wahtml headingfontcolour '"' INTO wahtml.
      ENDIF.

*      Add the fontsize
      IF NOT headingfontsize IS INITIAL.
        CONCATENATE wahtml' size ="' INTO wahtml.
        CONCATENATE wahtml  headingfontsize '"' INTO wahtml.
      ENDIF.

      CONCATENATE wahtml '>' INTO wahtml.
      usedafontattribute = true.
    ENDIF.

*   Add the caption name
    CONCATENATE wahtml <waheader> INTO wahtml.

    IF usedafontattribute = true.
      CONCATENATE wahtml '</font>' INTO wahtml.
      usedafontattribute = false.
    ENDIF.

    CONCATENATE wahtml '</th>' INTO wahtml.
    APPEND wahtml TO ilochtml.
  ENDLOOP.

  APPEND '</tr>' TO ilochtml.
  FREE ilocheader.


*  Line item data
  LOOP AT ilocdictstructure ASSIGNING <ilocdictstructure>.

    loopcounter = sy-tabix.

    APPEND '' TO ilochtml.
    APPEND '<tr>' TO ilochtml.

*   Add the row count
    IF NOT includerowcount IS INITIAL.
      IF bodybackcolour IS INITIAL.
        wahtml = '<td>'.
      ELSE.
        CONCATENATE '<td bgcolor="' bodybackcolour '">' INTO wahtml.
      ENDIF.

      IF NOT bodyfontname IS INITIAL
        OR NOT bodyfontcolour IS INITIAL
        OR NOT bodyfontsize IS INITIAL.
        CONCATENATE wahtml '<font' INTO wahtml.

*        Add the font name
        IF NOT bodyfontname IS INITIAL.
          CONCATENATE wahtml ' face ="' INTO wahtml.
          CONCATENATE wahtml bodyfontname '"' INTO wahtml.
        ENDIF.

*        Add the font colour
        IF NOT bodyfontcolour IS INITIAL.
          CONCATENATE wahtml ' color ="' INTO wahtml.
          CONCATENATE wahtml bodyfontcolour '"' INTO wahtml.
        ENDIF.

*        Add the fontsize
        IF NOT bodyfontsize IS INITIAL.
          CONCATENATE wahtml ' size ="' INTO wahtml.
          CONCATENATE wahtml bodyfontsize '"' INTO wahtml.
        ENDIF.

        CONCATENATE wahtml '>' INTO wahtml.
        usedafontattribute = true.
      ENDIF.

*     Add the row number into the table
      CONCATENATE wahtml loopcounter INTO wahtml.


      IF usedafontattribute = true.
        CONCATENATE wahtml '</font>' INTO wahtml.
        usedafontattribute = false.
      ENDIF.

      CONCATENATE wahtml '</td>' INTO wahtml.
      APPEND wahtml TO ilochtml.
    ENDIF.

    DO.
*     Assign each field in the table to the field symbol
      ASSIGN COMPONENT sy-index
        OF STRUCTURE <ilocdictstructure> TO <fsfield>.
      IF sy-subrc = 0.
        MOVE <fsfield> TO wtextcell.

*       Cell data processing
        IF bodybackcolour IS INITIAL.
          wahtml = '<td>'.
        ELSE.
          CONCATENATE '<td bgcolor="' bodybackcolour '">' INTO wahtml.
        ENDIF.

        IF NOT bodyfontname IS INITIAL
          OR NOT bodyfontcolour IS INITIAL
          OR NOT bodyfontsize IS INITIAL.
          CONCATENATE wahtml '<font' INTO wahtml.

*          Add the font name
          IF NOT bodyfontname IS INITIAL.
            CONCATENATE wahtml ' face ="' INTO wahtml.
            CONCATENATE wahtml bodyfontname '"' INTO wahtml.
          ENDIF.

*          Add the font colour
          IF NOT bodyfontcolour IS INITIAL.
            CONCATENATE wahtml ' color ="' INTO wahtml.
            CONCATENATE wahtml bodyfontcolour '"' INTO wahtml.
          ENDIF.

*          Add the fontsize
          IF NOT bodyfontsize IS INITIAL.
            CONCATENATE wahtml ' size ="' INTO wahtml.
            CONCATENATE wahtml bodyfontsize '"' INTO wahtml.
          ENDIF.

          CONCATENATE wahtml '>' INTO wahtml.
          usedafontattribute = true.
        ENDIF.

*       Add the caption name
        IF wtextcell IS INITIAL.
          CONCATENATE wahtml '&nbsp;' INTO wahtml.
        ELSE.
          CONCATENATE wahtml wtextcell INTO wahtml.
        ENDIF.

        IF usedafontattribute = true.
          CONCATENATE wahtml '</font>' INTO wahtml.
          usedafontattribute = false.
        ENDIF.

        CONCATENATE wahtml '</td>' INTO wahtml.
        APPEND wahtml TO ilochtml.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    APPEND '</tr>' TO ilochtml.
  ENDLOOP.

  APPEND '</table>' TO ilochtml.
ENDFORM.                                              "convertITABtoHtml

*-----------------------------------------------------------------------
*  convertCodeToHtml... Builds an HTML table based upon a text table.
*-----------------------------------------------------------------------
FORM convertcodetohtml USING icontents LIKE dumihtml[]
                             value(programname)
                             value(shortdescription)
                             value(sourcecodetype)
                             value(functiondocumentationexists)
                             value(ismainfunctioninclude)
                             value(syntaxhighlightcomments)
                             value(htmlextension)
                             value(customernamerange)
                             value(getincludes)
                             value(getdictstructures).

  DATA: htmltable TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: listingname TYPE string.
  DATA: descriptionname TYPE string.
  DATA: head(255).
  DATA: tail(255).
  DATA: mytabix TYPE sytabix.
  DATA: nextline TYPE sytabix.
  DATA: hyperlinkname TYPE string.
  DATA: copyofcurrentline TYPE string.
  DATA: currentlinelength TYPE i VALUE 0.
  DATA: copylinelength TYPE i VALUE 0.
  DATA: ignorefuturelines TYPE i VALUE false.
  DATA: foundasterix TYPE i VALUE false.
  DATA: lowercaselink TYPE string.
  DATA: wanextline TYPE string.
  DATA: wacontent(255).

  listingname = 'Description:'.

* Add a html header to the table
  PERFORM addhtmlheader USING htmltable[]
                              programname.

  CONCATENATE listingname programname INTO
              listingname SEPARATED BY space.
  CONCATENATE '<font size="3" face = "Arial" color="' colour_black
  '"><b>' listingname '</b></font>' INTO htmltable.
  APPEND htmltable.

  IF NOT shortdescription IS INITIAL.
    APPEND '<br>' TO htmltable.
    CONCATENATE descriptionname shortdescription
                INTO descriptionname SEPARATED BY space.
    CONCATENATE '<font size="3" face = "Arial" color="' colour_black
    '"><b>' descriptionname '</b></font>' INTO htmltable.
    APPEND htmltable.
  ENDIF.

  htmltable = '<hr>'.
  APPEND htmltable.

  htmltable = '<pre width="150">'.
  APPEND htmltable.

  LOOP AT icontents INTO wacontent.
    mytabix = sy-tabix.

    IF NOT ( icontents IS INITIAL ).
      WHILE ( wacontent CS '<' OR wacontent CS '>' ).
        REPLACE '<' WITH lt INTO wacontent.
        REPLACE '>' WITH gt INTO wacontent.
      ENDWHILE.

      IF wacontent+0(1) <> asterix.
        currentlinelength = STRLEN( wacontent ).
        copyofcurrentline = wacontent.

*       Don't hyperlink anything for files of type documentation
        IF sourcecodetype <> is_documentation.
*         Check for any functions to highlight
          IF ( wacontent CS callfunction )
            AND ( wacontent <> 'DESTINATION' ).
            nextline = mytabix + 1.
            READ TABLE icontents INTO wanextline INDEX nextline.
            TRANSLATE wanextline TO UPPER CASE.
            IF wanextline NS 'DESTINATION'.
              SHIFT copyofcurrentline LEFT DELETING LEADING space.

              copylinelength = STRLEN( copyofcurrentline ).

              SPLIT copyofcurrentline AT space INTO head tail.
              SPLIT tail AT space INTO head tail.
              SPLIT tail AT space INTO head tail.
*             Function name is now in head
              TRANSLATE head USING ''' '.
              SHIFT head LEFT DELETING LEADING space.

              IF head+0(1) = 'Y' OR head+0(1) = 'Z'
                OR head+0(1) = 'y'
                OR head+0(1) = 'z'
                OR head CS customernamerange.
*               Definately a customer function module
                hyperlinkname = head.

                IF sourcecodetype = is_function.
                  copyofcurrentline = 'call function <a href ="../'.
                ELSE.
                  copyofcurrentline = 'call function <a href ="'.
                ENDIF.

                lowercaselink = hyperlinkname.
                TRANSLATE lowercaselink TO LOWER CASE.
*               If we are running on a non UNIX environment we will
*               need to remove forward slashes
                IF frontendopsystem = non_unix.
                  TRANSLATE lowercaselink USING '/_'.
                ENDIF.

                CONCATENATE copyofcurrentline
                            lowercaselink     "hyperlinkName
                            '/'
                            lowercaselink     "hyperlinkName
                            period htmlextension '">'
                            ''''
                            hyperlinkname
                            ''''
                            '</a>'
                            tail INTO copyofcurrentline.

*               Pad the string back out with spaces
                WHILE copylinelength < currentlinelength.
                  SHIFT copyofcurrentline RIGHT BY 1 PLACES.
                  copylinelength = copylinelength + 1.
                ENDWHILE.

                wacontent = copyofcurrentline.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

*       Check for any customer includes to hyperlink
        IF wacontent CS include OR wacontent CS lowinclude.
          SHIFT copyofcurrentline LEFT DELETING LEADING space.
          copylinelength = STRLEN( copyofcurrentline ).

          SPLIT copyofcurrentline AT space INTO head tail.
          SHIFT tail LEFT DELETING LEADING space.

          IF ( tail+0(1) = 'Y'
            OR tail+0(1) = 'Z'
            OR tail+0(1) = 'y'
            OR tail+0(1) = 'z'
            OR tail CS customernamerange
            OR tail+0(2) = 'mz'
            OR tail+0(2) = 'MZ' )
            AND NOT getincludes IS INITIAL
            AND  tail NS structure
            AND tail NS lowstructure.

*           Hyperlink for program includes
            CLEAR wacontent.
            SHIFT tail LEFT DELETING LEADING space.
            SPLIT tail AT period INTO hyperlinkname tail.
            copyofcurrentline = 'include <a href ="'.

            lowercaselink = hyperlinkname.
            TRANSLATE lowercaselink TO LOWER CASE.

*           If we are running on a non UNIX environment we will need
*           to remove forward slashes
            IF frontendopsystem = non_unix.
              TRANSLATE lowercaselink USING '/_'.
            ENDIF.

            CONCATENATE copyofcurrentline
                        lowercaselink       "hyperlinkName
                        period htmlextension '">'
                        hyperlinkname
                        '</a>'
                        period tail INTO copyofcurrentline.

*           Pad the string back out with spaces
            WHILE copylinelength < currentlinelength.
              SHIFT copyofcurrentline RIGHT BY 1 PLACES.
              copylinelength = copylinelength + 1.
            ENDWHILE.
            wacontent = copyofcurrentline.
          ELSE.
            IF NOT getdictstructures IS INITIAL.
*            Hyperlink for structure include e.g. "include structure
*            zfred."
              copylinelength = STRLEN( copyofcurrentline ).
              SPLIT copyofcurrentline AT space INTO head tail.
              SHIFT tail LEFT DELETING LEADING space.
              SPLIT tail AT space INTO head tail.

              IF tail+0(1) = 'Y'
                OR tail+0(1) = 'Z'
                OR tail+0(1) ='y'
                OR tail+0(1) = 'z'
                OR tail CS customernamerange.
                CLEAR wacontent.
                SHIFT tail LEFT DELETING LEADING space.
                SPLIT tail AT period INTO hyperlinkname tail.
                copyofcurrentline = 'include structure <a href ='.

                lowercaselink = hyperlinkname.
                TRANSLATE lowercaselink TO LOWER CASE.
*              If we are running on a non UNIX environment we will
*              need to remove forward slashes
                IF frontendopsystem = non_unix.
                  TRANSLATE lowercaselink USING '/_'.
                ENDIF.
                CONCATENATE copyofcurrentline
                            '"'
                            lowercaselink    "hyperlinkName
                            '/'
                            'dictionary-'
                            lowercaselink    "hyperlinkName
                            period htmlextension
                            '">'
                            hyperlinkname
                            '</a>'
                            period tail INTO copyofcurrentline.

*              Pad the string back out with spaces
                WHILE copylinelength < currentlinelength.
                  SHIFT copyofcurrentline RIGHT BY 1 PLACES.
                  copylinelength = copylinelength + 1.
                ENDWHILE.
                wacontent = copyofcurrentline.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        IF NOT syntaxhighlightcomments IS INITIAL
        AND wacontent+0(1) = asterix.
          CONCATENATE '<font color ="' comment_colour '">' INTO head.
          CONCATENATE head wacontent '</font>' INTO tail.
          wacontent = tail.
        ENDIF.
      ENDIF.

      htmltable = wacontent.

    ELSE.
      htmltable = ''.
    ENDIF.
    APPEND htmltable.
  ENDLOOP.

  htmltable = '</pre>'.
  APPEND htmltable.

* Add a html footer to the table
  PERFORM addhtmlfooter USING htmltable[].

  icontents[] = htmltable[].
ENDFORM.                                              "convertCodeToHtml

*-----------------------------------------------------------------------
*  convertFunctionToHtml... Builds an HTML table based upon a text table
*-----------------------------------------------------------------------
FORM convertfunctiontohtml USING icontents LIKE dumihtml[]
                                 value(functionname)
                                 value(shortdescription)
                                 value(sourcecodetype)
                                 value(functiondocumentationexists)
                                 value(ismainfunctioninclude)
                                 value(syntaxhighlightcomments)
                                 value(htmlextension)
                                 value(customernamerange)
                                 value(getincludes)
                                 value(getdictstructures).

  DATA: htmltable TYPE STANDARD TABLE OF string WITH HEADER LINE.
  DATA: listingname TYPE string.
  DATA: descriptionname TYPE string.
  DATA: head(255).
  DATA: tail(255).
  DATA: mytabix TYPE sytabix.
  DATA: nextline TYPE sytabix.
  DATA: hyperlinkname TYPE string.
  DATA: copyofcurrentline TYPE string.
  DATA: currentlinelength TYPE i VALUE 0.
  DATA: copylinelength TYPE i VALUE 0.
  DATA: ignorefuturelines TYPE i VALUE false.
  DATA: foundasterix TYPE i VALUE false.
  DATA: lowercaselink TYPE string.
  DATA: wanextline TYPE string.
  DATA: wacontent(255).

  descriptionname = 'Description:'.
  listingname =  'Code listing for function:'.

* Add a html header to the table
  PERFORM addhtmlheader USING htmltable[]
                              functionname.

  CONCATENATE listingname functionname
              INTO listingname SEPARATED BY space.
  CONCATENATE '<font size="3" face = "Arial" color="' colour_black
              '"><b>' listingname '</b></font>' INTO htmltable.
  APPEND htmltable.

  IF NOT shortdescription IS INITIAL.
    APPEND '<br>' TO htmltable.
    CONCATENATE descriptionname shortdescription
               INTO descriptionname SEPARATED BY space.
    CONCATENATE '<font size="3" face = "Arial" color="' colour_black
                '"><b>' descriptionname '</b></font>' INTO htmltable.
    APPEND htmltable.
  ENDIF.

  htmltable = '<hr>'.
  APPEND htmltable.

  htmltable = '<pre width="150">'.
  APPEND htmltable.

  LOOP AT icontents INTO wacontent.
    mytabix = sy-tabix.

*   Extra code for adding global and doc hyperlinks to functions
    IF sourcecodetype = is_function AND ismainfunctioninclude = true.
      IF sy-tabix > 1.
        IF wacontent+0(1) = asterix AND ignorefuturelines = false.
          foundasterix = true.
        ELSE.
          IF foundasterix = true.
*           Lets add our extra HTML lines in here
            APPEND '' TO htmltable.

*           Global data hyperlink
            IF NOT syntaxhighlightcomments IS INITIAL.
              CONCATENATE '<font color ="' comment_colour '">'
                          INTO copyofcurrentline.
            ENDIF.

            CONCATENATE copyofcurrentline '*       <a href ="'
                        INTO copyofcurrentline.
            lowercaselink = functionname.
            TRANSLATE lowercaselink TO LOWER CASE.
*           If we are running on a non UNIX environment we will need to
*           remove forward slashes
            IF frontendopsystem = non_unix.
              TRANSLATE lowercaselink USING '/_'.
            ENDIF.

            CONCATENATE copyofcurrentline 'global-' lowercaselink
                        period htmlextension '">'
                        'Global data declarations'
                        '</a>' INTO copyofcurrentline.

            IF NOT syntaxhighlightcomments IS INITIAL.
              CONCATENATE copyofcurrentline '</font>'
                          INTO copyofcurrentline.
            ENDIF.

            APPEND copyofcurrentline TO htmltable.

*           Documentation hyperlink.
            IF functiondocumentationexists = true.
              IF NOT syntaxhighlightcomments IS INITIAL.
                CONCATENATE '<font color ="' comment_colour '">'
                            INTO copyofcurrentline.
              ENDIF.

              CONCATENATE copyofcurrentline '*       <a href ="'
                          INTO copyofcurrentline.

              lowercaselink = functionname.
              TRANSLATE lowercaselink TO LOWER CASE.
*             If we are running on a non UNIX environment we will need
*             to remove forward slashes
              IF frontendopsystem = non_unix.
                TRANSLATE lowercaselink USING '/_'.
              ENDIF.

              CONCATENATE copyofcurrentline
                          'docs-'
                          lowercaselink  "functionName
                          period htmlextension '">'
                          'Function module documentation'
                          '</a>'
                          INTO copyofcurrentline.

              IF NOT pcomm IS INITIAL.
                CONCATENATE copyofcurrentline '</font>'
                            INTO copyofcurrentline.
              ENDIF.
              APPEND copyofcurrentline TO htmltable.
            ENDIF.

            foundasterix = false.
            ignorefuturelines = true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*   Carry on as normal
    IF NOT ( icontents IS INITIAL ).
      WHILE ( wacontent CS '<' OR wacontent CS '>' ).
        REPLACE '<' WITH lt INTO wacontent.
        REPLACE '>' WITH gt INTO wacontent.
      ENDWHILE.

      IF wacontent+0(1) <> asterix.
        currentlinelength = STRLEN( wacontent ).

*       Don't hyperlink anything for files of type documentation
        IF sourcecodetype <> is_documentation.
*       Check for any functions to highlight
          IF ( wacontent CS callfunction )
              AND ( wacontent <> 'DESTINATION' ).
            nextline = mytabix + 1.
            READ TABLE icontents INTO wanextline INDEX nextline.
            TRANSLATE wanextline TO UPPER CASE.
            IF wanextline NS 'DESTINATION'.
              copyofcurrentline = wacontent.
              SHIFT copyofcurrentline LEFT DELETING LEADING space.

              copylinelength = STRLEN( copyofcurrentline ).

              SPLIT copyofcurrentline AT space INTO head tail.
              SPLIT tail AT space INTO head tail.
              SPLIT tail AT space INTO head tail.
*             Function name is now in head
              TRANSLATE head USING ''' '.
              SHIFT head LEFT DELETING LEADING space.

              IF head+0(1) = 'Y'
                 OR head+0(1) = 'Z'
                 OR head+0(1) = 'y'
                 OR head+0(1) = 'z'
                 OR head CS customernamerange.

*                 Definately a customer function module
                hyperlinkname = head.

                IF sourcecodetype = is_function.
                  copyofcurrentline = 'call function <a href ="../'.
                ELSE.
                  copyofcurrentline = 'call function <a href ="'.
                ENDIF.

                lowercaselink = hyperlinkname.
                TRANSLATE lowercaselink TO LOWER CASE.
*                 If we are running on a non UNIX environment we will
*                 need to remove forward slashes
                IF frontendopsystem = non_unix.
                  TRANSLATE lowercaselink USING '/_'.
                ENDIF.

                CONCATENATE copyofcurrentline
                            lowercaselink     "hyperlinkName
                            '/'
                            lowercaselink     "hyperlinkName
                            period htmlextension '">'
                            ''''
                            hyperlinkname
                            ''''
                            '</a>'
                            tail INTO copyofcurrentline.

*                 Pad the string back out with spaces
                WHILE copylinelength < currentlinelength.
                  SHIFT copyofcurrentline RIGHT BY 1 PLACES.
                  copylinelength = copylinelength + 1.
                ENDWHILE.

                wacontent = copyofcurrentline.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

*       Check for any customer includes to hyperlink
        IF wacontent CS include OR wacontent CS lowinclude.
          copyofcurrentline = wacontent.

          SHIFT copyofcurrentline LEFT DELETING LEADING space.
          copylinelength = STRLEN( copyofcurrentline ).

          SPLIT copyofcurrentline AT space INTO head tail.
          SHIFT tail LEFT DELETING LEADING space.


          IF ( tail+0(1) = 'Y'
            OR tail+0(1) = 'Z'
            OR tail+0(1) = 'y'
            OR tail+0(1) = 'z'
            OR tail CS customernamerange
            OR tail+0(2) = 'mz'
            OR tail+0(2) = 'MZ' )
            AND NOT getincludes IS INITIAL.

*             Hyperlink for program includes
            CLEAR wacontent.
            SHIFT tail LEFT DELETING LEADING space.
            SPLIT tail AT period INTO hyperlinkname tail.
            copyofcurrentline = 'include <a href ="'.

            lowercaselink = hyperlinkname.
            TRANSLATE lowercaselink TO LOWER CASE.
*             If we are running on a non UNIX environment we will need
*             to remove forward slashes
            IF frontendopsystem = non_unix.
              TRANSLATE lowercaselink USING '/_'.
            ENDIF.

            CONCATENATE copyofcurrentline
                        lowercaselink       "hyperlinkName
                        period htmlextension '">'
                        hyperlinkname
                        '</a>'
                        period tail INTO copyofcurrentline.

*             Pad the string back out with spaces
            WHILE copylinelength < currentlinelength.
              SHIFT copyofcurrentline RIGHT BY 1 PLACES.
              copylinelength = copylinelength + 1.
            ENDWHILE.
            wacontent = copyofcurrentline.
          ELSE.
            IF NOT getdictstructures IS INITIAL.
*               Hyperlink for structure include
              copylinelength = STRLEN( copyofcurrentline ).
              SPLIT copyofcurrentline AT space INTO head tail.
              SHIFT tail LEFT DELETING LEADING space.
              SPLIT tail AT space INTO head tail.

              IF tail+0(1) = 'Y'
                OR tail+0(1) = 'Z'
                OR tail+0(1) = 'y'
                OR tail+0(1) = 'z'
                OR tail CS customernamerange.
                CLEAR wacontent.
                SHIFT tail LEFT DELETING LEADING space.
                SPLIT tail AT period INTO hyperlinkname tail.
                copyofcurrentline = 'include structure <a href ='.

                lowercaselink = hyperlinkname.
                TRANSLATE lowercaselink TO LOWER CASE.
*                   If we are running on a non UNIX environment we will
*                   need to remove forward slashes
                IF frontendopsystem = non_unix.
                  TRANSLATE lowercaselink USING '/_'.
                ENDIF.

                CONCATENATE copyofcurrentline
                            '"'
                            lowercaselink    "hyperlinkName
                            '/'
                            'dictionary-'
                            lowercaselink    "hyperlinkName
                            period htmlextension
                            '">'
                            hyperlinkname
                            '</a>'
                            period tail INTO copyofcurrentline.

*                   Pad the string back out with spaces
                WHILE copylinelength < currentlinelength.
                  SHIFT copyofcurrentline RIGHT BY 1 PLACES.
                  copylinelength = copylinelength + 1.
                ENDWHILE.
                wacontent = copyofcurrentline.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        IF NOT syntaxhighlightcomments IS INITIAL
          AND wacontent+0(1) = asterix.
          CONCATENATE '<font color ="' comment_colour '">' INTO head.
          CONCATENATE head wacontent '</font>' INTO tail.
          wacontent = tail.
        ENDIF.
      ENDIF.

      htmltable = wacontent.

    ELSE.
      htmltable = ''.
    ENDIF.
    APPEND htmltable.
  ENDLOOP.

  htmltable = '</pre>'.
  APPEND htmltable.

* Add a html footer to the table
  PERFORM addhtmlfooter USING htmltable[].

  icontents[] = htmltable[].
ENDFORM.                                          "convertFunctionToHtml

*-----------------------------------------------------------------------
*  buildColumnHeaders... build table column names
*-----------------------------------------------------------------------
FORM buildcolumnheaders USING iloccolumncaptions LIKE dumihtml[].

  APPEND 'Row' TO iloccolumncaptions.
  APPEND 'Field name' TO iloccolumncaptions.
  APPEND 'Position' TO iloccolumncaptions.
  APPEND 'Key' TO iloccolumncaptions.
  APPEND 'Data element' TO iloccolumncaptions.
  APPEND 'Domain' TO iloccolumncaptions.
  APPEND 'Datatype' TO iloccolumncaptions.
  APPEND 'Length' TO iloccolumncaptions.
  APPEND 'Domain text' TO iloccolumncaptions.
ENDFORM.                                             "buildColumnHeaders

*-----------------------------------------------------------------------
* addHTMLHeader...  add a html formatted header to our output table
*-----------------------------------------------------------------------
FORM addhtmlheader USING ilocheader LIKE dumihtml[]
                         value(title).

  DATA: waheader TYPE string.

  APPEND '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
    TO ilocheader.
  APPEND '<html>' TO ilocheader.
  APPEND '<head>' TO ilocheader.

  CONCATENATE '<title>' title '</title>' INTO waheader.
  APPEND waheader TO ilocheader.

  APPEND '</head>' TO ilocheader.

  IF NOT pback IS INITIAL.
    CONCATENATE '<body bgcolor="' background_colour '">' INTO waheader.
  ELSE.
    CONCATENATE '<body bgcolor="' colour_white '">' INTO waheader.
  ENDIF.

  APPEND waheader TO ilocheader.
ENDFORM.                                                  "addHTMLHeader

*-----------------------------------------------------------------------
* addHTMLFooter...  add a html formatted footer to our output table
*-----------------------------------------------------------------------
FORM addhtmlfooter USING ilocfooter LIKE dumihtml[].

  DATA: footermessage TYPE string.
  DATA: wafooter TYPE string.

  PERFORM buildfootermessage USING 'HTML'
                                   footermessage.

  APPEND '<hr>' TO ilocfooter.
  CONCATENATE '<font size="2" face = "Sans Serif">' footermessage INTO
wafooter.
  APPEND wafooter TO ilocfooter.
  APPEND '</font>' TO ilocfooter.
  APPEND '</body>' TO ilocfooter.
  APPEND '</html>' TO ilocfooter.
ENDFORM.                                                  "addHTMLFooter

*-----------------------------------------------------------------------
* buildFooterMessage...Returns a footer message based on the output file
*                      type.
*-----------------------------------------------------------------------
FORM buildfootermessage USING filetype
                              returnmessage.

  IF filetype = 'HTML'.
    CONCATENATE 'Extracted by Direct Download 46cd version'
                 versionno '- E.G.Mellodew. 1998-2005 UK. Sap Release'
                 sy-saprl
                INTO returnmessage SEPARATED BY space.
  ELSE.
  ENDIF.
ENDFORM.                                             "buildFooterMessage

************************************************************************
****************************DISPLAY ROUTINES****************************
************************************************************************

*-----------------------------------------------------------------------
*  fillTreeNodeTables...
*-----------------------------------------------------------------------
FORM filltreenodetables USING ilocdictionary LIKE idictionary[]
                              iloctreedisplay LIKE itreedisplay[]
                              value(runtime).

  DATA: tablelines TYPE i.
  DATA: watreedisplay LIKE snodetext.
  FIELD-SYMBOLS: <wadictionary> TYPE tdicttable.
  DATA: tablelinesstring TYPE string.
  DATA: runtimechar(10).
  DATA: sublevel TYPE string.

  DESCRIBE TABLE ilocdictionary LINES tablelines.
  tablelinesstring = tablelines.

  IF tablelines = 1.
    CONCATENATE tablelinesstring 'table downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ELSE.
    CONCATENATE tablelinesstring 'tables downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ENDIF.

  WRITE runtime TO runtimechar.
  CONCATENATE watreedisplay-text2 '- runtime' runtimechar
              INTO watreedisplay-text2 SEPARATED BY space.

* include header display record.
  watreedisplay-tlevel = '1'.
  watreedisplay-tlength2  = 60.
  watreedisplay-tcolor2    = 1.
  APPEND watreedisplay TO iloctreedisplay.

  LOOP AT ilocdictionary ASSIGNING <wadictionary>.
    watreedisplay-tlevel = '2'. .
    watreedisplay-text2 = <wadictionary>-tablename.
    watreedisplay-tcolor2    = 3.
    watreedisplay-tlength3   = 80.
    watreedisplay-tcolor3    = 3.
    watreedisplay-tpos3      = 60.
    CONCATENATE 'Dictionary:' <wadictionary>-tabletitle
                INTO watreedisplay-text3 SEPARATED BY space.

    APPEND watreedisplay TO iloctreedisplay.
  ENDLOOP.
ENDFORM.                                             "fillTreeNodeTables

*-----------------------------------------------------------------------
*  fillTreeNodeMessages...
*-----------------------------------------------------------------------
FORM filltreenodemessages USING ilocmessages LIKE imessages[]
                                iloctreedisplay LIKE itreedisplay[]
                                value(runtime).

  DATA: tablelines TYPE i.
  DATA: watreedisplay LIKE snodetext.
  FIELD-SYMBOLS: <wamessage> TYPE tmessage.
  DATA: tablelinesstring TYPE string.
  DATA: runtimechar(10).

  SORT ilocmessages ASCENDING BY arbgb.

  LOOP AT ilocmessages ASSIGNING <wamessage>.
    AT NEW arbgb.
      tablelines = tablelines + 1.
    ENDAT.
  ENDLOOP.
  tablelinesstring = tablelines.

  IF tablelines = 1.
    CONCATENATE tablelinesstring 'message class downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ELSE.
    CONCATENATE tablelinesstring 'message classes downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ENDIF.

  WRITE runtime TO runtimechar.
  CONCATENATE watreedisplay-text2 '- runtime' runtimechar
              INTO watreedisplay-text2 SEPARATED BY space.

* include header display record.
  watreedisplay-tlevel = '1'.
  watreedisplay-tlength2 = 60.
  watreedisplay-tcolor2 = 1.
  APPEND watreedisplay TO iloctreedisplay.

  LOOP AT ilocmessages ASSIGNING <wamessage>.
    AT NEW arbgb.
      watreedisplay-tlevel = '2'.
      watreedisplay-text2 = <wamessage>-arbgb.
      watreedisplay-tcolor2    = 5.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 5.
      watreedisplay-tpos3      = 60.
      watreedisplay-text3 = <wamessage>-stext.
      CONCATENATE 'Message class:'  watreedisplay-text3
                  INTO watreedisplay-text3 SEPARATED BY space.
      APPEND watreedisplay TO iloctreedisplay.
    ENDAT.
  ENDLOOP.
ENDFORM.                                           "fillTreeNodeMessages

*-----------------------------------------------------------------------
*  fillTreeNodeFunctions...
*-----------------------------------------------------------------------
FORM filltreenodefunctions USING ilocfunctions LIKE ifunctions[]
                                 iloctreedisplay LIKE itreedisplay[]
                                 value(runtime).

  DATA: tablelines TYPE i.
  DATA: watreedisplay LIKE snodetext.
  FIELD-SYMBOLS: <wafunction> TYPE tfunction.
  FIELD-SYMBOLS: <wascreen> TYPE tscreenflow.
  FIELD-SYMBOLS: <waguititle> TYPE tguititle.
  FIELD-SYMBOLS: <wadictionary> TYPE tdicttable.
  FIELD-SYMBOLS: <wainclude> TYPE tinclude.
  FIELD-SYMBOLS: <wamessage> TYPE tmessage.
  DATA: tablelinesstring TYPE string.
  DATA: runtimechar(10).

  SORT ilocfunctions ASCENDING BY functionname.

  DESCRIBE TABLE ilocfunctions LINES tablelines.
  tablelinesstring = tablelines.

  IF tablelines = 1.
    CONCATENATE tablelinesstring 'function downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ELSE.
    CONCATENATE tablelinesstring 'functions downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ENDIF.

  WRITE runtime TO runtimechar.

  CONCATENATE watreedisplay-text2 '- runtime' runtimechar
              INTO watreedisplay-text2 SEPARATED BY space.
* include header display record.
  watreedisplay-tlevel = '1'.
  watreedisplay-tlength2  = 60.
  watreedisplay-tcolor2    = 1.
  APPEND watreedisplay TO iloctreedisplay.

* Lets fill the detail in
  LOOP AT ilocfunctions ASSIGNING <wafunction>.
    watreedisplay-tlevel = 2.
    watreedisplay-text2 = <wafunction>-functionname.
    watreedisplay-tcolor2    = 7.
    watreedisplay-tlength3   = 80.
    watreedisplay-tcolor3    = 7.
    watreedisplay-tpos3      = 60.
    CONCATENATE 'Function:' <wafunction>-functionname
                INTO watreedisplay-text3 SEPARATED BY space.
    APPEND watreedisplay TO iloctreedisplay.

*   Screens.
    LOOP AT <wafunction>-iscreenflow ASSIGNING <wascreen>.
      watreedisplay-tlevel = '2'.
      watreedisplay-text2 = <wascreen>-screen.
      watreedisplay-tcolor2    = 6.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 6.
      watreedisplay-tpos3      = 60.
      watreedisplay-text3 = 'Screen'.
      APPEND watreedisplay TO itreedisplay.
    ENDLOOP.

*   GUI Title.
    LOOP AT <wafunction>-iguititle ASSIGNING <waguititle>.
      watreedisplay-tlevel = '2'.
      watreedisplay-text2 = <waguititle>-obj_code.
      watreedisplay-tcolor2    = 6.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 6.
      watreedisplay-tpos3      = 60.
      watreedisplay-text3 = 'GUI Title'.
      APPEND watreedisplay TO itreedisplay.
    ENDLOOP.

*   Fill in the tree with include information
    LOOP AT <wafunction>-iincludes ASSIGNING <wainclude>.
      watreedisplay-tlevel = 3.
      watreedisplay-text2 =  <wainclude>-includename.
      watreedisplay-tcolor2    = 4.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 4.
      watreedisplay-tpos3      = 60.
      CONCATENATE 'Include:   ' <wainclude>-includetitle
                  INTO watreedisplay-text3 SEPARATED BY '    '.
      APPEND watreedisplay TO iloctreedisplay.
    ENDLOOP.

*   fill in the tree with dictionary information
    LOOP AT <wafunction>-idictstruct ASSIGNING <wadictionary>.
      watreedisplay-tlevel = 3.
      watreedisplay-text2 =  <wadictionary>-tablename.
      watreedisplay-tcolor2    = 3.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 3.
      watreedisplay-tpos3      = 60.
      CONCATENATE 'Dictionary:' <wadictionary>-tabletitle
                  INTO watreedisplay-text3 SEPARATED BY space.
      APPEND watreedisplay TO iloctreedisplay.
    ENDLOOP.

*   fill in the tree with message information
    SORT <wafunction>-imessages[] ASCENDING BY arbgb.
    LOOP AT <wafunction>-imessages ASSIGNING <wamessage>.
      AT NEW arbgb.
        watreedisplay-tlevel = 3.
        watreedisplay-text2 = <wamessage>-arbgb.
        watreedisplay-tcolor2    = 5.
        watreedisplay-tlength3   = 80.
        watreedisplay-tcolor3    = 5.
        watreedisplay-tpos3      = 60.

*       Select the message class text if we do not have it already
        IF <wamessage>-stext IS INITIAL.
          SELECT SINGLE stext FROM t100a
                              INTO <wamessage>-stext
                              WHERE arbgb = <wamessage>-arbgb.
        ENDIF.

        watreedisplay-text3 = <wamessage>-stext.
        CONCATENATE 'Message class:'  watreedisplay-text3
                    INTO watreedisplay-text3 SEPARATED BY space.
        APPEND watreedisplay TO iloctreedisplay.
      ENDAT.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                                          "fillTreeNodeFunctions

*-----------------------------------------------------------------------
*  fillTreeNodePrograms
*-----------------------------------------------------------------------
FORM filltreenodeprograms USING ilocprograms LIKE iprograms[]
                                ilocfunctions LIKE ifunctions[]
                                iloctreedisplay LIKE itreedisplay[]
                                value(runtime).

  DATA: tablelines TYPE i.
  DATA: watreedisplay LIKE snodetext.
  FIELD-SYMBOLS: <waprogram> TYPE tprogram.
  FIELD-SYMBOLS: <wascreen> TYPE tscreenflow.
  FIELD-SYMBOLS: <wafunction> TYPE tfunction.
  FIELD-SYMBOLS: <wadictionary> TYPE tdicttable.
  FIELD-SYMBOLS: <wainclude> TYPE tinclude.
  FIELD-SYMBOLS: <wamessage> TYPE tmessage.
  DATA: tablelinesstring TYPE string.
  DATA: runtimechar(10).

  DESCRIBE TABLE ilocprograms LINES tablelines.
  tablelinesstring = tablelines.

  IF tablelines = 1.
    CONCATENATE tablelinesstring 'program downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ELSE.
    CONCATENATE tablelinesstring  'programs downloaded'
                INTO watreedisplay-text2 SEPARATED BY space.
  ENDIF.

  WRITE runtime TO runtimechar.

  CONCATENATE watreedisplay-text2 '- runtime' runtimechar
              INTO watreedisplay-text2 SEPARATED BY space.
* include header display record.
  watreedisplay-tlevel = '1'.
  watreedisplay-tlength2  = 60.
  watreedisplay-tcolor2    = 1.
  APPEND watreedisplay TO itreedisplay.

  LOOP AT ilocprograms ASSIGNING <waprogram>.
*   Main programs.
    watreedisplay-tlevel = '2'.
    watreedisplay-text2 = <waprogram>-prog.
    watreedisplay-tcolor2    = 1.
*   Description
    watreedisplay-tlength3   = 80.
    watreedisplay-tcolor3    = 1.
    watreedisplay-tpos3      = 60.
    CONCATENATE 'Program:' <waprogram>-programtitle
                INTO watreedisplay-text3 SEPARATED BY space.
    APPEND watreedisplay TO itreedisplay.
*   Screens.
    LOOP AT <waprogram>-iscreenflow ASSIGNING <wascreen>.
      watreedisplay-tlevel = '3'.
      watreedisplay-text2 = <wascreen>-screen.
      watreedisplay-tcolor2    = 6.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 6.
      watreedisplay-tpos3      = 60.
      watreedisplay-text3 = 'Screen'.
      APPEND watreedisplay TO itreedisplay.
    ENDLOOP.
*   fill in the tree with message information
    SORT <waprogram>-imessages[] ASCENDING BY arbgb.
    LOOP AT <waprogram>-imessages ASSIGNING <wamessage>.
      AT NEW arbgb.
        watreedisplay-tlevel = 3.
        watreedisplay-text2 = <wamessage>-arbgb.
        watreedisplay-tcolor2    = 5.
        watreedisplay-tlength3   = 80.
        watreedisplay-tcolor3    = 5.
        watreedisplay-tpos3      = 60.

*       Select the message class text if we do not have it already
        IF <wamessage>-stext IS INITIAL.
          SELECT SINGLE stext FROM t100a
                              INTO <wamessage>-stext
                              WHERE arbgb = <wamessage>-arbgb.
        ENDIF.

        watreedisplay-text3 = <wamessage>-stext.
        CONCATENATE 'Message class:'  watreedisplay-text3
                    INTO watreedisplay-text3 SEPARATED BY space.
        APPEND watreedisplay TO iloctreedisplay.
      ENDAT.
    ENDLOOP.
*   Fill in the tree with include information
    LOOP AT <waprogram>-iincludes ASSIGNING <wainclude>.
      watreedisplay-tlevel = 3.
      watreedisplay-text2 =  <wainclude>-includename.
      watreedisplay-tcolor2    = 4.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 4.
      watreedisplay-tpos3      = 60.
      CONCATENATE 'Include:' <wainclude>-includetitle
                  INTO watreedisplay-text3 SEPARATED BY '   '.
      APPEND watreedisplay TO iloctreedisplay.
    ENDLOOP.
*   fill in the tree with dictionary information
    LOOP AT <waprogram>-idictstruct ASSIGNING <wadictionary>.
      watreedisplay-tlevel = 3.
      watreedisplay-text2 =  <wadictionary>-tablename.
      watreedisplay-tcolor2    = 3.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 3.
      watreedisplay-tpos3      = 60.
      CONCATENATE 'Dictionary:' <wadictionary>-tabletitle
                  INTO watreedisplay-text3 SEPARATED BY '    '.
      APPEND watreedisplay TO iloctreedisplay.
    ENDLOOP.

*   Function Modules
    LOOP AT ilocfunctions ASSIGNING <wafunction>
            WHERE programlinkname = <waprogram>-prog.
      watreedisplay-tlevel = 3.
      watreedisplay-text2 = <wafunction>-functionname.
      watreedisplay-tcolor2    = 7.
      watreedisplay-tlength3   = 80.
      watreedisplay-tcolor3    = 7.
      watreedisplay-tpos3      = 60.
      CONCATENATE 'Function:' <wafunction>-functionname
                  INTO watreedisplay-text3 SEPARATED BY '      '.
      APPEND watreedisplay TO iloctreedisplay.

*     Fill in the tree with include information
      LOOP AT <wafunction>-iincludes ASSIGNING <wainclude>.
        watreedisplay-tlevel = 4.
        watreedisplay-text2 =  <wainclude>-includename.
        watreedisplay-tcolor2    = 4.
        watreedisplay-tlength3   = 80.
        watreedisplay-tcolor3    = 4.
        watreedisplay-tpos3      = 60.
        CONCATENATE 'Include:' <wainclude>-includetitle
                    INTO watreedisplay-text3 SEPARATED BY '       '.
        APPEND watreedisplay TO iloctreedisplay.
      ENDLOOP.

*     fill in the tree with dictionary information
      LOOP AT <wafunction>-idictstruct ASSIGNING <wadictionary>.
        watreedisplay-tlevel = 4.
        watreedisplay-text2 =  <wadictionary>-tablename.
        watreedisplay-tcolor2    = 3.
        watreedisplay-tlength3   = 80.
        watreedisplay-tcolor3    = 3.
        watreedisplay-tpos3      = 60.
        CONCATENATE 'Dictionary:' <wadictionary>-tabletitle
                    INTO watreedisplay-text3 SEPARATED BY '    '.
        APPEND watreedisplay TO iloctreedisplay.
      ENDLOOP.

*     fill in the tree with message information
      SORT <wafunction>-imessages[] ASCENDING BY arbgb.
      LOOP AT <wafunction>-imessages ASSIGNING <wamessage>.
        AT NEW arbgb.
          watreedisplay-tlevel = 4.
          watreedisplay-text2 = <wamessage>-arbgb.
          watreedisplay-tcolor2    = 5.
          watreedisplay-tlength3   = 80.
          watreedisplay-tcolor3    = 5.
          watreedisplay-tpos3      = 60.

*         Select the message class text if we do not have it already
          IF <wamessage>-stext IS INITIAL.
            SELECT SINGLE stext FROM t100a
                                INTO <wamessage>-stext
                                WHERE arbgb = <wamessage>-arbgb.
          ENDIF.

          watreedisplay-text3 = <wamessage>-stext.
          CONCATENATE 'Message class:'  watreedisplay-text3
                      INTO watreedisplay-text3 SEPARATED BY '  '.
          APPEND watreedisplay TO iloctreedisplay.
        ENDAT.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                                           "fillTreeNodePrograms

*-----------------------------------------------------------------------
* displayTree...
*-----------------------------------------------------------------------
FORM displaytree USING iloctreedisplay LIKE itreedisplay[].

  DATA: watreedisplay TYPE snodetext.

* build up the tree from the internal table node
  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    TABLES
      nodetab            = itreedisplay
    EXCEPTIONS
      tree_failure       = 1
      id_not_found       = 2
      wrong_relationship = 3
      OTHERS             = 4.

* get the first index and expand the whole tree
  READ TABLE iloctreedisplay INTO watreedisplay INDEX 1.
  CALL FUNCTION 'RS_TREE_EXPAND'
    EXPORTING
      node_id   = watreedisplay-id
      all       = 'X'
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

* now display the tree
  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
    EXPORTING
      callback_program      = sy-cprog
      callback_user_command = 'CB_USER_COMMAND'
      callback_text_display = 'CB_text_DISPLAY'
      callback_top_of_page  = 'TOP_OF_PAGE'
    EXCEPTIONS
      OTHERS                = 1.
ENDFORM.                                                    "displayTree

*-----------------------------------------------------------------------
*  topOfPage... for tree display routines.
*-----------------------------------------------------------------------
FORM topofpage.

ENDFORM.                    "topofpage


*Messages
*----------------------------------------------------------
*
* Message class: OO
* 000 & & & &

----------------------------------------------------------------------

