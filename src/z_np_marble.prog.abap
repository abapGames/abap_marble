REPORT z_np_marble NO STANDARD PAGE HEADING.

" origin https://wiki.scn.sap.com/wiki/display/Snippets/Game+-+Marbles+in+ABAP?original_fqdn=wiki.sdn.sap.com

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* D A T A   D E F I N I T I O N
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*....Types
TYPES: BEGIN OF ty_score,
         srl   TYPE i,
         uname TYPE sy-uname,
         score TYPE i,
         time  TYPE i,
       END   OF ty_score.
*....Internal Tables
DATA: BEGIN OF itab OCCURS 0,
        c1,  c2,  c3,
        c4,  c5,  c6,
        c7,  c8,  c9,
      END   OF itab.
DATA: BEGIN OF it_print OCCURS 0,
        c1(4), c2(4), c3(4),
        c4(4), c5(4), c6(4),
        c7(4), c8(4), c9(4),
      END   OF it_print.
DATA: it_score TYPE STANDARD TABLE OF ty_score.
*....Work Areas
DATA: wa_itab  LIKE itab,
      wa_print LIKE it_print,
      w_score  TYPE ty_score.
*....Globle Variables
DATA: l_no_m      TYPE i,
      l_no_s      TYPE i,
      l_no_c      TYPE char10,
      l_half      TYPE i,
      l_half_half TYPE i,
      l_print     TYPE flag,
      l_name      TYPE char20,
      l_mod       TYPE i.
DATA: w_on        TYPE flag,
      w_sel_line  TYPE i,
      w_sel_col   TYPE char1,
      w_dest_ok   TYPE flag,
      w_dest_line TYPE i,
      w_dest_col  TYPE char1,
      w_field     TYPE char20,
      w_line      TYPE i,
      w_game_over TYPE flag,
      w_total     TYPE i,
      w_rem       TYPE i,
      w_gone      TYPE i,
      w_st_time   TYPE i,
      w_end_time  TYPE i,
      w_exported  TYPE flag.
*....Field symbols
FIELD-SYMBOLS: <f> TYPE any.
*....Constants
CONSTANTS: icon_0(40) TYPE c VALUE icon_wd_radio_button_empty,
           icon_1(40) TYPE c VALUE icon_radiobutton,
           icon_2(40) TYPE c VALUE icon_color.
*.. Some systems don't have above listed ICONs You can use:
**....Constants
*CONSTANTS: ICON_0(40) TYPE C VALUE ICON_AVERAGE,      " ICON_WD_RADIO_BUTTON_EMPTY,
*           ICON_1(40) TYPE C VALUE ICON_POSITIVE,     " ICON_RADIOBUTTON,
*           ICON_2(40) TYPE C VALUE ICON_COLOR.

DATA not_grey TYPE abap_bool.
*....Ranges
RANGES: r_not_grey FOR not_grey.
*....Macros
DEFINE conv_i_c.
  &2 = &1.
  CONDENSE &2.
END-OF-DEFINITION.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* S E L E C T I O N   S C R E E N
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
PARAMETERS: p_num TYPE i DEFAULT 7.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* A T   S E L E C T I O N - S C R E E N .
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
AT SELECTION-SCREEN.
  IF  p_num GT 9
  OR  p_num LT 5.
    MESSAGE e398(00) WITH 'Currently allowed only: 5, 7, 9'.
  ENDIF.
  l_mod = p_num MOD 2.
  IF l_mod = 0.
    MESSAGE e398(00) WITH 'Only odd numbers are allowed'.
  ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* S T A R T   O F   S E L E C T I O N
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
START-OF-SELECTION.
  GET TIME FIELD w_st_time.
  PERFORM fill_marbles.
  PERFORM fill_print_table.
  PERFORM write_marbles.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* A T   L I N E   S E L E C T I O N
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
AT LINE-SELECTION.
  GET CURSOR FIELD w_field.
  PERFORM calculate_marbles.
  PERFORM fill_print_table.
  sy-lsind = 0.
  PERFORM write_marbles.
  PERFORM check_game_over.
*&---------------------------------------------------------------------*
*&      Form  fill_marbles
*&---------------------------------------------------------------------*
*       Fillup the initial table for the marbles
*----------------------------------------------------------------------*
FORM fill_marbles .
* Grey cells
  l_half = floor( p_num / 2 ).
  l_half_half = l_half / 2.
  r_not_grey-sign   = 'I'.
  r_not_grey-option = 'BT'.
  r_not_grey-low  = ( l_half - l_half_half ) + 1.
  r_not_grey-high = ( l_half + l_half_half ) - 1.
  APPEND r_not_grey.
  CLEAR  r_not_grey.
* Filling up the table
  DO p_num TIMES.
    l_no_m = sy-index.
    NEW-LINE.
    DO p_num TIMES.
      l_no_s = sy-index.
      CLEAR: l_print.
      IF l_no_m IN r_not_grey.
        l_print = 'X'.
      ENDIF.
      IF l_no_s IN r_not_grey.
        l_print = 'X'.
      ENDIF.
      IF l_print = 'X'.
        conv_i_c l_no_s l_no_c.
        CONCATENATE 'WA_ITAB-C' l_no_c INTO l_name.
        ASSIGN (l_name) TO <f>.
        IF  l_no_s = l_half
        AND l_no_m = l_half.
          <f> = '0'.
          w_total = w_total - 1.
        ELSE.
          <f> = '1'.
          w_total = w_total + 1.
        ENDIF.
      ELSE.
        WRITE: ' '.
      ENDIF.
    ENDDO.
    APPEND wa_itab TO itab.
    CLEAR  wa_itab.
  ENDDO.
  w_rem = w_total.
ENDFORM.                    " fill_marbles
*&---------------------------------------------------------------------*
*&      Form  fill_print_Table
*&---------------------------------------------------------------------*
*       Convert ITAB value to PRINT table value .
*----------------------------------------------------------------------*
FORM fill_print_table .
  FIELD-SYMBOLS: <f1> TYPE any.
  REFRESH it_print.
  LOOP AT itab INTO wa_itab.
    l_no_m = sy-index.
    DO p_num TIMES.
      conv_i_c sy-index l_no_c.
      CLEAR l_name.
      CONCATENATE 'WA_ITAB-C' l_no_c INTO l_name.
      ASSIGN (l_name) TO <f>.
      CLEAR l_name.
      CONCATENATE 'WA_PRINT-C' l_no_c INTO l_name.
      ASSIGN (l_name) TO <f1>.
      CASE <f>.
        WHEN '1'.
          <f1> = icon_1.
        WHEN '0'.
          <f1> = icon_0.
        WHEN '2'.
          <f1> = icon_2.
        WHEN OTHERS.
      ENDCASE.
    ENDDO.
    APPEND wa_print TO it_print.
    CLEAR  wa_print.
  ENDLOOP.
ENDFORM.                    " fill_print_Table
*&---------------------------------------------------------------------*
*&      Form  write_marbles
*&---------------------------------------------------------------------*
*       Write marbles from the PRINT table
*----------------------------------------------------------------------*
FORM write_marbles .
  FIELD-SYMBOLS: <f1> TYPE any.
  IF w_game_over IS INITIAL.
    LOOP AT it_print INTO wa_print.
      SKIP 1.
      w_line = sy-tabix.
      WRITE: (2) w_line.
      HIDE  w_line.
      DO p_num TIMES.
        conv_i_c sy-index l_no_c.
        CLEAR l_name.
        CONCATENATE 'WA_PRINT-C' l_no_c INTO l_name.
        ASSIGN (l_name) TO <f1>.
        IF NOT <f1> IS INITIAL.
          WRITE: (2) <f1> AS ICON HOTSPOT ON, (2) ' '.
        ELSE.
          WRITE: (2) ' ', (2) ' '.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.
  SKIP 4.
  WRITE: /(30) 'Total Marbles:',    w_total.
  WRITE: /(30) 'Remaining Marbles', w_rem.
  SKIP 4.
  PERFORM write_5_high_score.
ENDFORM.                    " write_marbles
*&---------------------------------------------------------------------*
*&      Form  calculate_marbles
*&---------------------------------------------------------------------*
*       Calculate the marbles after the user input in line selection
*----------------------------------------------------------------------*
FORM calculate_marbles .
* No marble has been selected
  IF w_on IS INITIAL.
    PERFORM validate_input.
  ELSE.
* remove the seleced marble
    PERFORM deselect_marble.
    IF w_on = 'X'.
*     Check destination cell, if the same marble has not been selected
      PERFORM check_destination.
    ENDIF.
  ENDIF.
* Destination is ok ..? rearrange the marbles in ITAB
  IF w_dest_ok = 'X'.
    PERFORM rearrange_marbles.
  ENDIF.
ENDFORM.                    " calculate_marbles
*&---------------------------------------------------------------------*
*&      Form  validate_input
*&---------------------------------------------------------------------*
*       Validating the selected marble, is it movable or not
*       if marble is movable, highlight it
*----------------------------------------------------------------------*
FORM validate_input .
  DATA: l_sel_field(20),
        l_tmp_field(20),
        l_sel_col(1),
        l_tmp_col(2),
        l_tmp_line      TYPE i,
        l_ok            TYPE flag.
  FIELD-SYMBOLS: <f1> TYPE any,
                 <f2> TYPE any.
  READ TABLE itab INTO wa_itab INDEX w_line.
  l_sel_field = w_field.
  l_sel_col   = w_field+10(1).
  REPLACE 'PRINT' INTO l_sel_field WITH 'ITAB'.
  CONDENSE l_sel_field.
  ASSIGN (l_sel_field) TO <f1>.
* value = 0 >> No marble
  IF <f1> = '0'.
    MESSAGE s398(00) WITH 'No marble to select.!'.
    EXIT.
  ENDIF.
* Check right
  l_tmp_col = l_sel_col + 2.
  IF l_tmp_col < 9.
    CONCATENATE 'WA_ITAB-C' l_tmp_col INTO l_tmp_field.
    CONDENSE l_tmp_field.
    ASSIGN (l_tmp_field) TO <f2>.
    IF <f2> = '0'.
      l_ok = 'X'.
    ENDIF.
  ENDIF.
* Check left
  IF l_ok IS INITIAL.
    l_tmp_col = l_sel_col - 2.
    IF l_tmp_col > 0.
      CONCATENATE 'WA_ITAB-C' l_tmp_col INTO l_tmp_field.
      CONDENSE l_tmp_field.
      ASSIGN (l_tmp_field) TO <f2>.
      IF <f2> = '0'.
        l_ok = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
* check Above
  IF l_ok IS INITIAL.
    l_tmp_line = w_line - 2.
    IF l_tmp_line > 0.
      READ TABLE itab INTO wa_itab INDEX l_tmp_line.
      IF <f1> = '0'.
        l_ok = 'X'.
      ENDIF.
      READ TABLE itab INTO wa_itab INDEX w_line.
    ENDIF.
  ENDIF.
* Check underneath
  IF l_ok IS INITIAL.
    l_tmp_line = w_line + 2.
    IF l_tmp_line < 9.
      READ TABLE itab INTO wa_itab INDEX l_tmp_line.
      IF <f1> = '0'.
        l_ok = 'X'.
      ENDIF.
      READ TABLE itab INTO wa_itab INDEX w_line.
    ENDIF.
  ENDIF.
* Ok .. than ON
  IF l_ok = 'X'.
    w_on = 'X'.
    <f1> = '2'.
    MODIFY itab FROM wa_itab INDEX w_line.
    w_sel_line = w_line.
    w_sel_col  = l_sel_col .
  ELSE.
    MESSAGE s398(00) WITH 'No marble to select.!' ' ' ' ' ' '.
  ENDIF.
ENDFORM.                    " validate_input
*&---------------------------------------------------------------------*
*&      Form  deselect_marble
*&---------------------------------------------------------------------*
*       Deselect the marble if the same marble is selected again
*----------------------------------------------------------------------*
FORM deselect_marble .
  DATA: l_sel_field(20).
  FIELD-SYMBOLS: <f1> TYPE any.
  READ TABLE itab INTO wa_itab INDEX w_line.
  l_sel_field = w_field.
  REPLACE 'PRINT' INTO l_sel_field WITH 'ITAB'.
  CONDENSE l_sel_field.
  ASSIGN (l_sel_field) TO <f1>.
  IF <f1> = '2'.
    <f1> = '1'.
    MODIFY itab FROM wa_itab INDEX w_line.
    MESSAGE s398(00) WITH 'Marble was deselected..!!'.
    CLEAR: w_on, w_sel_line, w_sel_col.
  ENDIF.
ENDFORM.                    " deselect_marble
*&---------------------------------------------------------------------*
*&      Form  check_destination
*&---------------------------------------------------------------------*
*       Check the destination cell, it should not be empty and distnce
*       between selected cell and destination cell must be 2
*----------------------------------------------------------------------*
FORM check_destination .
  DATA: l_dest_field(20),
        l_dest_col(1),
        l_dest_not_ok    TYPE flag,
        l_tmp_line       TYPE i,
        l_tmp_col        TYPE c.
  DATA: l_itab_dest LIKE itab.
  FIELD-SYMBOLS: <f1> TYPE any.
  READ TABLE itab INTO l_itab_dest INDEX w_line.
  l_dest_field = w_field.
  l_dest_col   = w_field+10(1).
  REPLACE 'WA_PRINT' INTO l_dest_field WITH 'L_ITAB_DEST'.
  CONDENSE l_dest_field.
  ASSIGN (l_dest_field) TO <f1>.
* Destination should be empty
  IF <f1> <> '0'.
    l_dest_not_ok = 'X'.
  ENDIF.
* Calcualate the distance between selected marble and destination
  IF l_dest_not_ok IS INITIAL.
    IF w_sel_line <> w_line.
      l_tmp_line = abs( w_sel_line - w_line ).
      IF l_tmp_line <> '2'.
        l_dest_not_ok = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF l_dest_not_ok IS INITIAL.
    IF w_sel_col <> l_dest_col.
      l_tmp_col = abs( w_sel_col - l_dest_col ).
      IF l_tmp_col <> '2'.
        l_dest_not_ok = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
* destination not ok
  IF l_dest_not_ok = 'X'.
    MESSAGE s398(00) WITH 'Destination is not GOOD' ' ' ' ' ' '.
  ELSE.
    w_dest_ok = 'X'.
    w_dest_line = w_line.
    w_dest_col  = l_dest_col.
  ENDIF.
ENDFORM.                    " check_destination
*&---------------------------------------------------------------------*
*&      Form  rearrange_marbles
*&---------------------------------------------------------------------*
*       Rearrange marbles
*     1 Remove the marble which is inbetween the selected & destination
*     2 Remove the marble from the selected cell
*     3 Put marble on the destination cell
*----------------------------------------------------------------------*
FORM rearrange_marbles .
  DATA: l_field(20),
        l_tmp_line  TYPE i,
        l_tmp_col   TYPE char1,
        l_no_move   TYPE flag,
        l_itab      LIKE itab.

  FIELD-SYMBOLS: <f1> TYPE any.
* Make the inbetween column as 0 if both lines are same
  IF w_sel_line = w_dest_line.
    IF w_sel_col > w_dest_col.
      l_tmp_col = w_dest_col + 1.
    ELSE.
      l_tmp_col = w_sel_col + 1.
    ENDIF.
    READ TABLE itab INTO l_itab INDEX w_sel_line.
    l_field = w_field.
    REPLACE 'WA_PRINT' INTO l_field WITH 'L_ITAB'.
    CONDENSE l_field.
    l_field+8(1) = l_tmp_col.
    ASSIGN (l_field) TO <f1>.
    IF <f1> = 1.
      <f1> = '0'.
      w_gone = w_gone + 1.
      MODIFY itab FROM l_itab INDEX w_sel_line.
    ELSE.
      l_no_move = 'X'.
    ENDIF.
    CLEAR  l_itab.
  ENDIF.
* Make the inbetween line as 0 if both lines are same
  IF w_sel_col = w_dest_col.
    IF w_sel_line > w_dest_line.
      l_tmp_line = w_dest_line + 1.
    ELSE.
      l_tmp_line = w_sel_line + 1.
    ENDIF.
    READ TABLE itab INTO l_itab INDEX l_tmp_line.
    l_field = w_field.
    REPLACE 'WA_PRINT' INTO l_field WITH 'L_ITAB'.
    CONDENSE l_field.
    l_field+8(1) = w_sel_col.
    ASSIGN (l_field) TO <f1>.
    IF <f1> = 1.
      <f1> = '0'.
      w_gone = w_gone + 1.
      MODIFY itab FROM l_itab INDEX l_tmp_line.
    ELSE.
      l_no_move = 'X'.
    ENDIF.
    CLEAR  l_itab.
  ENDIF.
  IF l_no_move IS INITIAL.
*   Make Destination = 1
    READ TABLE itab INTO l_itab INDEX w_dest_line.
    l_field = w_field.
    REPLACE 'WA_PRINT' INTO l_field WITH 'L_ITAB'.
    CONDENSE l_field.
    l_field+8(1) = w_dest_col.
    ASSIGN (l_field) TO <f1>.
    <f1> = '1'.
    MODIFY itab FROM l_itab INDEX w_dest_line.
    CLEAR  l_itab.
*   Make Selected = 0.
    READ TABLE itab INTO l_itab INDEX w_sel_line.
    l_field = w_field.
    REPLACE 'WA_PRINT' INTO l_field WITH 'L_ITAB'.
    CONDENSE l_field.
    l_field+8(1) = w_sel_col.
    ASSIGN (l_field) TO <f1>.
    <f1> = '0'.
    MODIFY itab FROM l_itab INDEX w_sel_line.
    CLEAR  l_itab.
  ELSE.
*   Make Selected = 1 when no movement
    READ TABLE itab INTO l_itab INDEX w_sel_line.
    l_field = w_field.
    REPLACE 'WA_PRINT' INTO l_field WITH 'L_ITAB'.
    CONDENSE l_field.
    l_field+8(1) = w_sel_col.
    ASSIGN (l_field) TO <f1>.
    <f1> = '1'.
    MODIFY itab FROM l_itab INDEX w_sel_line.
    CLEAR  l_itab.
  ENDIF.
  w_rem = w_total - w_gone.
  CLEAR: w_dest_ok, w_dest_line, w_dest_col,
  w_on,      w_sel_line,  w_sel_col.

ENDFORM.                    " rearrange_marbles
*&---------------------------------------------------------------------*
*&      Form  check_game_over
*&---------------------------------------------------------------------*
*       Check game over when the remaining marbles are half than
*         the original marbles. Check for all cells with the marbles and
*         check adjacent cells (right, left, above and underneath cells)
*         with the value. If the marble found in any adjacent cell than
*         GAME is NOT OVER
*----------------------------------------------------------------------*
FORM check_game_over .
  STATICS: l_tot_half TYPE i.
  DATA: l_itab     LIKE itab,
        l_itab_tmp LIKE itab.
  DATA: l_tmp_field(20),
        l_sel_col(1),
        l_tmp_col(2),
        l_ok            TYPE flag,
        l_line          TYPE i,
        l_tmp_line      TYPE i.
  CLEAR: l_ok.
  FIELD-SYMBOLS: <f1> TYPE any,
                 <f2> TYPE any.
  CHECK w_on IS INITIAL.
  l_tot_half = abs( w_total / 2 ).
  CHECK w_rem < l_tot_half.
  LOOP AT itab INTO l_itab.
    l_line = sy-tabix.
    l_itab_tmp = l_itab.
    DO p_num TIMES.
      l_itab = l_itab_tmp.
      l_sel_col = sy-index.
      conv_i_c sy-index l_no_c.
      CLEAR l_name.
      CONCATENATE 'L_ITAB-C' l_no_c INTO l_name.
      ASSIGN (l_name) TO <f1>.
      IF <f1> IS INITIAL
      OR <f1> = '0'.
        CONTINUE.
      ENDIF.
*      IF <F1> = '1'.
*        l_ok = 'X'.
*        exit.
*      ENDIF.
*     right neighbour
      l_tmp_col = l_sel_col + 1.
      IF l_tmp_col < 9.
        CONCATENATE 'L_ITAB-C' l_tmp_col INTO l_tmp_field.
        CONDENSE l_tmp_field.
        ASSIGN (l_tmp_field) TO <f2>.
        IF <f2> = '1'.
          l_tmp_col = l_sel_col + 2.
          IF l_tmp_col < 9.
            CONCATENATE 'L_ITAB-C' l_tmp_col INTO l_tmp_field.
            CONDENSE l_tmp_field.
            ASSIGN (l_tmp_field) TO <f2>.
            IF <f2> = '0'.
              l_ok = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*     Check left
      l_tmp_col = l_sel_col - 1.
      IF l_tmp_col > 0.
        CONCATENATE 'L_ITAB-C' l_tmp_col INTO l_tmp_field.
        CONDENSE l_tmp_field.
        ASSIGN (l_tmp_field) TO <f2>.
        IF <f2> = '1'.
          l_tmp_col = l_sel_col - 2.
          IF l_tmp_col > 0.
            CONCATENATE 'L_ITAB-C' l_tmp_col INTO l_tmp_field.
            CONDENSE l_tmp_field.
            ASSIGN (l_tmp_field) TO <f2>.
            IF <f2> = '0'.
              l_ok = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      CONCATENATE 'L_ITAB-C' l_no_c INTO l_tmp_field.
      CONDENSE l_tmp_field.
      ASSIGN (l_tmp_field) TO <f2>.
*     check Above
      l_tmp_line = l_line - 1.
      IF l_tmp_line > 0.
        CLEAR l_itab.
        READ TABLE itab INTO l_itab INDEX l_tmp_line.
        IF <f2> = '1'.
          l_tmp_line = l_line - 2.
          IF l_tmp_line > 0.
            CLEAR l_itab.
            READ TABLE itab INTO l_itab INDEX l_tmp_line.
            IF <f2> = '0'.
              l_ok = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*     Check underneath
      l_tmp_line = l_line + 1.
      IF l_tmp_line < 9.
        CLEAR l_itab.
        READ TABLE itab INTO l_itab INDEX l_tmp_line.
        IF <f2> = '1'.
          l_tmp_line = l_line + 2.
          IF l_tmp_line < 9.
            CLEAR l_itab.
            READ TABLE itab INTO l_itab INDEX l_tmp_line.
            IF <f2> = '0'.
              l_ok = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
    IF l_ok = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF l_ok IS INITIAL.
    w_game_over = 'X'.
    MESSAGE s398(00) WITH 'Game Over. Socre:' w_rem.
    PERFORM export_high_score.
*    leave program.
  ENDIF.
ENDFORM.                    " check_game_over
*&---------------------------------------------------------------------*
*&      Form  export_high_score
*&---------------------------------------------------------------------*
*       Export High Score to memory when the game is over
*----------------------------------------------------------------------*
FORM export_high_score .
  DATA: l_time TYPE i.
  CHECK w_exported IS INITIAL.
  GET TIME FIELD w_end_time.
  l_time = w_end_time - w_st_time.
  w_score-uname = sy-uname.
  w_score-score = w_rem.
  w_score-time  = l_time.
  APPEND w_score TO it_score.
  SORT it_score BY score time.
  LOOP AT it_score INTO w_score.
    w_score-srl = sy-tabix.
    MODIFY it_score FROM w_score.
    CLEAR  w_score.
  ENDLOOP.
  DELETE it_score WHERE srl > 5.
  EXPORT it_score = it_score TO DATABASE indx(zz)
  ID 'ZGAME_MAR'.
  w_exported = 'X'.
ENDFORM.                    " export_high_score
*&---------------------------------------------------------------------*
*&      Form  write_5_high_score
*&---------------------------------------------------------------------*
*       Write 5 high scores
*----------------------------------------------------------------------*
FORM write_5_high_score .
  IMPORT it_score = it_score FROM DATABASE indx(zz)
  ID 'ZGAME_MAR'.
  WRITE: /(12) 'User',
  (10) 'Score' RIGHT-JUSTIFIED ,
  (10) 'Time'  RIGHT-JUSTIFIED.
  WRITE: /(34) sy-uline.
  LOOP AT it_score INTO w_score.
    WRITE: /(12) w_score-uname,
    (10) w_score-score,
    (10) w_score-time.
  ENDLOOP.
ENDFORM.                    " write_5_high_score
