! Author: Simon Mason
Module progress_meter
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cpt_progress_meter, close_progress_meter, end_progress_meter, set_progress_increment, set_progress_meter, &
             start_progress_meter, stop_progress_meter, update_progress_meter
!
! Scalars
!
! Integer scalars
   Integer, Public :: ic_pm = 0 ! - progress meter control variable -
!
! Real scalars
   Real(Kind=rp), Public :: prog ! - progress meter -
!
   Real(Kind=rp), Private :: dprog ! - incremental progress -
!
! Logical scalars
   Logical, Public :: lcalc ! - calculation active? -
   Logical, Public :: lstop ! - stop calculation? -
!
Contains
!
!
 Subroutine cpt_progress_meter ()
!
! Creates a progress meter
!
#if GUI == 1
!
! Executable Statements
!
! Create progress meter
   Call open_progress_meter ()
#else
   Continue
#endif
!
   Return
 End Subroutine cpt_progress_meter
!
!
!
#if GUI == 1
 Subroutine open_progress_meter ()
!
! Creates a progress meter window
!
! Modules
   Use labels, Only: cg_progress, &
                     l_abort
   Use gui,    Only: add_button, add_prog_bar, gui_centre, gui_creturn, init_win, leave_window, print_text
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Create progress meter
   Call init_win (c_opts='no_maxminbox,no_sysmenu')
   Call gui_centre ()
   Call print_text (Trim(cg_progress)//':')
   Call gui_creturn (.false.)
   Call add_prog_bar (25, prog)
   Call gui_creturn (.false.)
   Call gui_creturn (.true.)
   Call add_button ('bt', 1, l_abort, &
       cbf=cpt_abort)
   Call leave_window (ic_pm, &
       lowned=.true.)
!
   Return
 End Subroutine open_progress_meter
!
!
!
#endif
 Function cpt_abort()
!
! Aborts calculations and closes progress meter
!
! Modules
   Use gui, Only: set_cursor_waiting
!
! Function result
   Integer :: cpt_abort
!
! Executable Statements
!
! Abort calculations
   lstop = .true.
! - close progress meter -
   Call close_progress_meter ()
! - restore cursor -
   Call set_cursor_waiting (0)
   cpt_abort=0
!
   Return
 End Function cpt_abort
!
!
!
 Subroutine close_progress_meter ()
!
! Closes progress meter
!
! Modules
   Use screen, Only: window_update
!
! Executable Statements
!
! Close progress meter
   ic_pm = 0
   Call window_update (ic_pm)
!
  Return
 End Subroutine close_progress_meter
!
!
!
 Subroutine end_progress_meter (lcursor, lpdone)
!
! Ends progress meter
!
! Modules
   Use data_numbers, Only: one
   Use labels,       Only: cg_done
   Use screen,       Only: window_update
   Use gui,          Only: set_cursor_waiting
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent (In) :: lcursor ! - update cursor? -
   Logical, Intent (In) :: lpdone   ! - print "done"? -
!
! Executable Statements
!
! End progress meter
   prog = one
   Call window_update (prog)
   If (lcursor) Call set_cursor_waiting (0)
   lcalc = .false.
   Call close_progress_meter ()
   If (lpdone) Then
      Print *, ' '
      Print *, Trim(cg_done)//'!'
   End If
!
   Return
 End Subroutine end_progress_meter
!
!
!
 Subroutine set_progress_increment (rprog, ladjust)
!
! Initialises progress increment
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent (In) :: rprog ! - progress increment -
!
   Logical, Intent (In) :: ladjust ! - adjustment indicator -
!
! Executable Statements
!
! Set progress meter increment
   If (ladjust) Then
      dprog = dprog*rprog
   Else
      dprog = rprog
   End If
!
   Return
 End Subroutine set_progress_increment
!
!
!
 Subroutine set_progress_meter (rprog)
!
! Sets progress meter to value indicated by rprog
!
! Modules
   Use screen, Only: window_update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent (In) :: rprog ! - progress -
!
! Executable Statements
!
! Set progress meter
   prog = rprog
   Call window_update (prog)
!
   Return
 End Subroutine set_progress_meter
!
!
!
 Subroutine start_progress_meter (msg, lcursor)
!
! Starts progress meter
!
! Modules
   Use data_numbers, Only: zero
   Use screen,       Only: window_update
   Use gui,          Only: set_cursor_waiting
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent (In) :: msg ! - message -
!
   Logical, Intent (In) :: lcursor ! - update cursor -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim
!
! Executable Statements
!
! Print message
   If (Len_Trim(msg) > 1) Then
      Print *, ' '
      Print *, msg//' ...'
   End If
!
! Start progress meter
   lstop = .false.
   lcalc = .true.
   prog = zero
#if GUI == 1
! - create meter -
   Call cpt_progress_meter ()
#endif
! - set initial value to zero -
   Call window_update (prog)
   If (lcursor) Call set_cursor_waiting (1)
!
   Return
 End Subroutine start_progress_meter
!
!
!
 Subroutine stop_progress_meter (lcursor)
!
! Stops progress meter
!
! Modules
   Use data_numbers, Only: zero
   Use gui,          Only: set_cursor_waiting
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent (In) :: lcursor ! - update cursor -
!
! Executable Statements
!
! Stop progress meter
   prog = zero
   Call close_progress_meter ()
   If (lcursor) Call set_cursor_waiting (0)
!
   Return
 End Subroutine stop_progress_meter
!
!
!
 Function update_progress_meter(lcursor, &
          ifail)
!
! Update progress meter
!
! Modules
   Use screen, Only: window_update
   Use gui,    Only: set_cursor_waiting
!
! Function result
   Integer :: update_progress_meter
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent (In) :: lcursor ! - update cursor -
!
! Optional arguments
! - input/output scalars -
   Integer, Intent (InOut), Optional :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present
!
! Executable Statements
!
! Update meter
   prog = prog + dprog
   Call set_cursor_waiting (0)
   Call window_update (prog)
   If (lcursor) Call set_cursor_waiting (1)
!
! Check for stoppage
   If (lstop) Then
      update_progress_meter = 1
      If (Present(ifail)) ifail = -1
   Else
      update_progress_meter = 0
   End If
!
   Return
 End Function update_progress_meter
End Module progress_meter
