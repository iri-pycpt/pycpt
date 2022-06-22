! Author: Simon Mason
Module markers
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
#if GUI == 1
   Public :: draw_cross,draw_marker,init_cross
#endif
   Public :: init_marker,proj_read_markers,proj_write_markers
!
! Derived types
!
! Derived type definitions
! - marker -
   Public marker
   Type marker
      Sequence
      Integer :: imrk         ! - marker type -
      Integer :: ixd          ! - X marker dimension -
      Integer :: iyd          ! - Y marker dimension -
!
      Real(Kind=rp) :: rscale ! - marker scaling -
   End Type marker
!
! Derived type scalars
   Type(marker), Public :: gmarker ! - marker -
!
Contains
!
!
 Subroutine init_marker ()
!
! Initialises markers
!
! Modules
   Use data_numbers, Only: one
!
! Executable Statements
!
! Define default marker settings
   gmarker%imrk=3     ! - marker type -
   gmarker%ixd=1      ! - X marker dimension -
   gmarker%iyd=1      ! - Y marker dimension -
!
   gmarker%rscale=one ! - marker scaling -
!
   Return
 End Subroutine init_marker
!
!
!
#if GUI == 1
 Subroutine init_cross (x_axis, y_axis, ix_pixel, iy_pixel)
!
! Initialises crosses
!
! Modules
   Use data_numbers, Only: onehth
   Use axes,         Only: axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(axis), Intent(In) :: x_axis ! - x-axis -
   Type(axis), Intent(In) :: y_axis ! - x-axis -
!
! - procedure arguments -
   Integer, External :: ix_pixel ! - x-axis function -
   Integer, External :: iy_pixel ! - y-axis function -
!
! Executable Statements
!
! Define marker dimensions
   gmarker%ixd = ix_pixel(x_axis%amin + (x_axis%amax - x_axis%amin)*onehth) - x_axis%i_min
   gmarker%iyd = iy_pixel(y_axis%amax - (y_axis%amax - y_axis%amin)*onehth) - y_axis%i_max
!
   Return
 End Subroutine init_cross
!
!
!
 Subroutine draw_marker (ix0, iy0, lw, icol)
!
! Draws a marker
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix0, iy0 ! - centre of cross -
   Integer, Intent(In) :: lw       ! - line width -
   Integer, Intent(In) :: icol     ! - colour -
!
! Executable Statements
!
! Draw marker
   Select Case (gmarker%imrk)
    Case (1) ! - cross -
      Call draw_cross (ix0, iy0, lw, icol)
    Case (2) ! - stars -
      Call draw_star (ix0, iy0, lw, icol)
    Case (3) ! - circle -
      Call draw_circle (ix0, iy0, icol)
   End Select
!
   Return
 End Subroutine draw_marker
!
!
!
 Subroutine draw_circle (ix0, iy0, icol)
!
! Draws a circle
!
! Modules
   Use clrwin$, Only: draw_filled_ellipse$
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix0, iy0 ! - centre of circle -
   Integer, Intent(In) :: icol     ! - colour -
!
! Executable Statements
!
! Draw circle
   Call draw_filled_ellipse$ (ix0, iy0, 2*gmarker%ixd, 2*gmarker%iyd, icol)
!
   Return
 End Subroutine draw_circle
!
!
!
 Subroutine draw_cross (ix0, iy0, lw, icol)
!
! Draws a cross
!
! Modules
   Use clrwin$, Only: draw_line_between$, set_line_width$
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix0, iy0 ! - centre of cross -
   Integer, Intent(In) :: lw       ! - line width -
   Integer, Intent(In) :: icol     ! - colour -
!
! Locals
!
! Local scalars
   Integer :: ix1 ! - start X plotting position -
   Integer :: ix2 ! - end X plotting position -
   Integer :: iy1 ! - start Y plotting position -
   Integer :: iy2 ! - end Y plotting position -
!
! Executable Statements
!
! Draw cross
   Call set_line_width$ (lw)
   ix1 = ix0 - gmarker%ixd
   ix2 = ix0 + gmarker%ixd
   iy1 = iy0 - gmarker%iyd
   iy2 = iy0 + gmarker%iyd
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol)
   iy1 = iy0 + gmarker%iyd
   iy2 = iy0 - gmarker%iyd
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol)
   Call set_line_width$ (1)
!
   Return
 End Subroutine draw_cross
!
!
!
 Subroutine draw_star (ix0, iy0, lw, icol)
!
! Draws a star
!
! Modules
   Use clrwin$,      Only: draw_line_between$, set_line_width$
   Use data_numbers, Only: sqrt3
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix0, iy0 ! - centre of star -
   Integer, Intent(In) :: lw       ! - line width -
   Integer, Intent(In) :: icol     ! - colour -
!
! Locals
!
! Local scalars
   Integer :: ix1 ! - start X plotting position -
   Integer :: ix2 ! - end X plotting position -
   Integer :: iy1 ! - start Y plotting position -
   Integer :: iy2 ! - end Y plotting position -
!
! Executable Statements
!
! Draw star
   Call set_line_width$ (lw)
   ix1 = ix0 - gmarker%ixd
   ix2 = ix0 + gmarker%ixd
   iy1 = iy0
   iy2 = iy0
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol)
   ix1 = ix0 - gmarker%ixd/2
   ix2 = ix0 + gmarker%ixd/2
   iy1 = iy0 + gmarker%iyd*sqrt3/2
   iy2 = iy0 - gmarker%iyd*sqrt3/2
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol)
   ix1 = ix0 + gmarker%ixd/2
   ix2 = ix0 - gmarker%ixd/2
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol)
   Call set_line_width$ (1)
!
   Return
 End Subroutine draw_star
!
!
!
#endif
 Subroutine proj_read_markers (iin, ifail)
!
! Reads marker settings from project file
!
! Modules
   Use data_numbers, Only: zero, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read marker settings
   Read (Unit=iin, Err=1, End=2) gmarker
   If (gmarker%imrk < 1) gmarker%imrk = 1
   If (gmarker%rscale <= zero) gmarker%rscale = one
!
   ifail = 0
   Return
!
! Errors in project file
! - problem reading file -
1  ifail = 1
   Return
!
! - end of file -
2  ifail = 2
   Return
!
 End Subroutine proj_read_markers
!
!
!
 Subroutine proj_write_markers (iout, ifail)
!
! Writes screen marker to project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write marker settings
   Write (Unit=iout, Err=1) gmarker
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_markers
End Module markers
