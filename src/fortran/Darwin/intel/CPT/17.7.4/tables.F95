! Author: Simon Mason
Module tables
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ng
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: table_cv, table_c2, table_ra
!
! Arrays
!
! Integer arrays
   Integer, Private :: ctbl(ng,ng) ! - contingency table -
!
! Scalars
!
! Integer scalars
   Integer, Private :: ioutt0 ! - station details output unit number -
   Integer, Private :: ioutt1 ! - frequency-table output unit number -
   Integer, Private :: ioutt2 ! - contingency-table output unit number -
!
Contains
!
!
 Function table_cv()
!
! Opens a window for contingency tables for cross-validated predictions
!
! Function result
   Integer :: table_cv
!
! Executable Statements
!
! Cross-validated contingency tables
#if GUI == 1
   table_cv = table('Cross-validated contingency tables', calc_cvtable, new_cvtable)
#else
   table_cv = table('Cross-validated contingency tables', calc_cvtable)
#endif
!
   Return
 End Function table_cv
!
!
!
 Function table_c2()
!
! Opens a window for contingency tables for double cross-validated predictions
!
! Function result
   Integer :: table_c2
!
! Executable Statements
!
! Double cross-validated contingency tables
#if GUI == 1
   table_c2 = table('Double cross-validated contingency tables', calc_c2table, new_c2table)
#else
   table_c2 = table('Double cross-validated contingency tables', calc_c2table)
#endif
!
   Return
 End Function table_c2
!
!
!
 Function table_ra()
!
! Opens a window for contingency tables for retroactive predictions
!
! Function result
   Integer :: table_ra
!
! Executable Statements
!
! Retroactive contingency tables
#if GUI == 1
   table_ra = table('Retroactive contingency tables', calc_rtable, new_rtable)
#else
   table_ra = table('Retroactive contingency tables', calc_rtable)
#endif
!
   Return
 End Function table_ra
!
!
!
#if GUI == 1
 Function table(title, calc_table, new_table)
#else
 Function table(title, calc_table)
#endif
!
! Opens a window for contingency tables
!
! Modules
#if GUI == 1
   Use gui,         Only: set_cursor_waiting
   Use windows_gui, Only: init_window_graphic_0
#else
   Use gui,        Only: get_output_unit, init_win, print_text
   Use cpt_output, Only: print_results
#endif
!
! Function result
   Integer :: table
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: title ! - window title -
!
! - procedure arguments -
   Integer, External :: calc_table ! - table calculation function -
#if GUI == 1
   Integer, External :: new_table  ! - new table calculation function -
#endif
!
! Executable Statements
!
#if GUI == 1
! Set cursor
   Call set_cursor_waiting (1)
!
! Calculate tables
   table = calc_table()
   Call set_cursor_waiting (0)
!
! Open results window
   table = init_window_graphic_0(title, new_table, show_table, &
         cxy='Y', iloc=ioutt0)
!
! Print tables
! - print frequency table -
   table=write_table()
!
#else
! Print tables
   Call init_win (title=title)
   Call print_text (' ')
   ioutt0 = get_output_unit()
   ioutt1 = get_output_unit()
   ioutt2 = get_output_unit()
   table = print_results(write_table, &
           f_calc=calc_table)
#endif
!
   Return
 End Function table
!
!
!
#if GUI == 1
 Function show_table() &
          Result (show)
!
! Opens ClearWin+ windows for contingency tables
!
! Modules
   Use gui, Only: box_close, boxes_open, open_clearwin, print_text
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: iwid ! - width -
!
! Executable Statements
!
! Determine appropriate width
   iwid = get_width_table() + 1
!
! Format window
   Call boxes_open (2, 1, .true., .false.)
! - open frequency table windows -
   Call print_text (' ')
   Call open_clearwin (iwid, 11, .true., ioutt1, &
        cbox='Frequency table')
! - close first column, first row box -
   Call box_close (.false.)
! - open contingency table windows -
   Call print_text (' ')
   Call open_clearwin (iwid, 11, .true., ioutt2, &
        cbox='Contingency table')
! - close second column, second row box -
   Call box_close (.false.)
   show = 1
!
   Return
 End Function show_table
!
!
!
#endif
 Function new_cvtable() &
          Result (new_table)
!
! Calculates contingency tables for cross-validated predictions for a new location
!
! Function result
   Integer :: new_table
!
! Executable Statements
!
! Calculate tables
   new_table = calc_cvtable()
!
! Re-print tables
   new_table = write_table()
!
   Return
 End Function new_cvtable
!
!
!
 Function new_c2table() &
          Result (new_table)
!
! Calculates contingency tables for double cross-validated predictions for a new location
!
! Function result
   Integer :: new_table
!
! Executable Statements
!
! Calculate tables
   new_table = calc_c2table()
!
! Re-print tables
   new_table = write_table()
!
   Return
 End Function new_c2table
!
!
!
 Function new_rtable() &
          Result (new_table)
!
! Calculates contingency tables for retroactive predictions for a new location
!
! Function result
   Integer :: new_table
!
! Executable Statements
!
! Calculate tables
   new_table = calc_rtable()
!
! Re-print tables
   new_table = write_table()
!
   Return
 End Function new_rtable
!
!
!
 Function write_table ()
!
! Print a contingency table
!
! Modules
   Use data_numbers, Only: zero
   Use labels,       Only: ca_cat_a, &
                           l_fcast, &
                           ls_cats
   Use fields,       Only: update_grid
!
! Function result
   Integer :: write_table
!
! Locals
!
! Local scalars
   Integer :: j    ! - indices -
   Integer :: itot ! - number of cases -
!
   Real(Kind=rp) :: df  ! - number of cases -
   Real(Kind=rp) :: tot ! - number of forecasts in category -
!
   Character(Len=8) :: c_all ! - "All" -
!
! Local arrays
   Integer, Dimension(ng) :: orf ! - observed relative frequencies -
   Integer, Dimension(ng) :: frf ! - forecast relative frequencies -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustR, Nint, Real, Sum, Trim
!
! Executable Statements
!
! Construct and add coordinate label for current point
   Call update_grid (ioutt0)
!
! Print tables
   Do j = 1, ng
      orf(j) = Sum(ctbl(j,:))
      frf(j) = Sum(ctbl(:,j))
   End Do
   itot = Sum(orf(:))
   df = Real(itot, Kind=rp)
! - print frequency table -
   Write (Unit=ioutt1, Fmt='(A)') ' '
   Write (Unit=ioutt1, Fmt='(A)') '                     '//Trim(l_fcast%c)
   Write (Unit=ioutt1, Fmt='(A)') ' '
   Write (Unit=ioutt1, Fmt='(13X,3(5X,A),A)') (ca_cat_a(j), j=1,ng), '   Total'
   Write (Unit=ioutt1, Fmt='(A)') ' '
   Write (Unit=ioutt1, Fmt='(2A,3I6,I8)') '            ', ca_cat_a(3), (ctbl(3,j), j=1,ng), orf(3)
   Write (Unit=ioutt1, Fmt='(2A,3I6,I8)') ' Observed   ', ca_cat_a(2), (ctbl(2,j), j=1,ng), orf(2)
   Write (Unit=ioutt1, Fmt='(2A,3I6,I8)') '            ', ca_cat_a(1), (ctbl(1,j), j=1,ng), orf(1)
   Write (Unit=ioutt1, Fmt='(A)') ' '
   Write (Unit=ioutt1, Fmt='(A,3I6,I8)') '        Total', (frf(j), j=1,ng), itot
   Write (Unit=ioutt1, Fmt='(A)') ' '
! - calculate observed relative frequencies -
   Do j = 1, ng
      orf(j) = Nint(Real(100*orf(j), Kind=rp)/df)
      frf(j) = Nint(Real(100*frf(j), Kind=rp)/df)
      tot = Real(Sum(ctbl(:,j)), Kind=rp)
      If (tot > zero) ctbl(:,j) = Nint(Real(100*ctbl(:,j), Kind=rp)/tot)
   End Do
! - print contingency table -
   c_all = Trim(ls_cats(0)%c)
   Write (Unit=ioutt2, Fmt='(A)') ' '
   Write (Unit=ioutt2, Fmt='(A)') '                     '//Trim(l_fcast%c)
   Write (Unit=ioutt2, Fmt='(A)') ' '
   Write (Unit=ioutt2, Fmt='(13X,3(5X,A),A)') (ca_cat_a(j), j=1,ng), AdjustR(c_all)
   Write (Unit=ioutt2, Fmt='(A)') ' '
   Write (Unit=ioutt2, Fmt='(2A,3(I5,A),I7,A)') '            ', ca_cat_a(3), (ctbl(3,j), '%', j=1,ng), orf(3), '%'
   Write (Unit=ioutt2, Fmt='(2A,3(I5,A),I7,A)') ' Observed   ', ca_cat_a(2), (ctbl(2,j), '%', j=1,ng), orf(2), '%'
   Write (Unit=ioutt2, Fmt='(2A,3(I5,A),I7,A)') '            ', ca_cat_a(1), (ctbl(1,j), '%', j=1,ng), orf(1), '%'
   Write (Unit=ioutt2, Fmt='(A)') ' '
   Write (Unit=ioutt2, Fmt='(2A,3(I5,A),I7,A)') '     ', AdjustR(c_all), (frf(j), '%', j=1,ng), 100, '%'
   Write (Unit=ioutt2, Fmt='(A)') ' '
   write_table = 2
!
   Return
 End Function write_table
!
!
!
 Function get_width_table() &
          Result (iwid)
!
! Calculates required window width to print contingency table
!
! Modules
   Use data_cpt_constants, Only: ng
!
! Function result
   Integer :: iwid
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len
!
! Executable Statements
!
! Calculate required width
   iwid = Len(' Observed   ')
   iwid = iwid + 1 + ng*6 + 8
!
   Return
 End Function get_width_table
!
!
!
 Function calc_cvtable() &
          Result (calc_table)
!
! Calculate results for a cross-validated contingency table
!
! Modules
   Use settings,   Only: iva, nu
   Use categories, Only: ifor_c, iobs_c
!
! Function result
   Integer :: calc_table
!
! Locals
!
! Local scalars
   Integer :: k ! - indices -
!
! Executable Statements
!
! Initialise table
   ctbl(:,:) = 0
!
! Construct frequency table
   Do k = 1, nu
      ctbl(iobs_c(iva,k),ifor_c(iva,k)) = ctbl(iobs_c(iva,k),ifor_c(iva,k)) + 1
   End Do
   calc_table = 2
!
   Return
 End Function calc_cvtable
!
!
!
 Function calc_c2table() &
          Result (calc_table)
!
! Calculate results for a double cross-validated contingency table
!
! Modules
   Use settings,   Only: iva, nu
   Use categories, Only: ifor_2, iobs_2
!
! Function result
   Integer :: calc_table
!
! Locals
!
! Local scalars
   Integer :: k ! - indices -
!
! Executable Statements
!
! Initialise table
   ctbl(:,:) = 0
!
! Construct frequency table
   Do k = 1, nu
      ctbl(iobs_2(iva,k),ifor_2(iva,k)) = ctbl(iobs_2(iva,k),ifor_2(iva,k)) + 1
   End Do
   calc_table = 2
!
   Return
 End Function calc_c2table
!
!
!
 Function calc_rtable()
!
! Calculate results for a retroactive contingency table
!
! Modules
   Use settings,   Only: iva, nur
   Use categories, Only: ifor_r, iobs_r
!
! Function result
   Integer :: calc_rtable
!
! Locals
!
! Local scalars
   Integer :: k ! - indices -
!
! Executable Statements
!
! Initialise table
   ctbl(:,:) = 0
!
! Construct frequency table
   Do k = 1, nur
      ctbl(iobs_r(iva,k),ifor_r(iva,k)) = ctbl(iobs_r(iva,k),ifor_r(iva,k)) + 1
   End Do
   calc_rtable = 2
!
   Return
 End Function calc_rtable
End Module tables
