*&---------------------------------------------------------------------*
*&  Include           ZPU580MIGFILE_TOP
*&---------------------------------------------------------------------*

TYPES: ty_locrep    TYPE string.


TYPES: ty_path      TYPE rcgfiletr-ftappl.
TYPES: ty_files     TYPE ty_path.


DATA: ws_ftappl     TYPE ty_path.



DATA: gt_files       TYPE TABLE OF ty_files.
DATA: gt_files_ctl   TYPE HASHED TABLE OF ty_files
                          WITH UNIQUE KEY table_line .