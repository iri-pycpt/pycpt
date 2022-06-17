! Author: Simon Mason
Module screen
!
! Modules
   Use data_numbers, Only: rp
#if GUI == 1
   Use data_numbers, Only: one
#endif
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
#if GUI == 1
   Public :: gfont_size, init_fonts, init_view, ix_pixel, iy_pixel, margins, pixel_ix, pixel_iy, reset_gfonts, set_gfonts
#endif
   Public :: init_gfonts, init_screen, proj_read_screen, proj_write_screen, window_update
!
! Derived types
!
! Derived type definitions
! - font -
   Public :: gfont
   Type gfont
      Sequence
      Real(Kind=rp) :: width  ! - font width -
      Real(Kind=rp) :: height ! - font height -
      Real(Kind=rp) :: fscale ! - scaling factor -
   End Type gfont
!
#if GUI == 1
! - frame -
   Type frame
      Sequence
      Integer :: left   ! - left -
      Integer :: right  ! - right -
      Integer :: length ! - length -
      Integer :: top    ! - top -
      Integer :: bottom ! - bottom -
      Integer :: depth  ! - depth -
   End Type frame
!
! - view -
   Type viewport
      Sequence
      Real(Kind=rp) :: xmin   ! - minimum horizontal value -
      Real(Kind=rp) :: xmax   ! - maximum horizontal value -
      Real(Kind=rp) :: length ! - viewport length -
      Real(Kind=rp) :: ymin   ! - minimum vertical value -
      Real(Kind=rp) :: ymax   ! - maximum vertical value -
      Real(Kind=rp) :: height ! - viewport height -
   End Type viewport
!
! Scalars
!
! Integer scalars
   Integer, Public :: igsize     ! - graphics area dimension -
   Integer, Public :: igsz        ! - graphics/map dimension -
   Integer, Public :: ix_font     ! - horizontal font size in pixels -
   Integer, Public :: iy_font     ! - vertical font size in pixels -
#endif
   Integer, Public :: ihght       ! - screen height -
   Integer, Public :: ihsize      ! - reduced map graphics dimension -
   Integer, Public :: isize       ! - minimum screen dimension -
   Integer, Public :: iwdth = 80  ! - screen width -
   Integer, Public :: iwdthd = 80 ! - default screen width -
!
#if GUI == 1
! Real scalars
   Real(Kind=rp), Public :: dts           ! - default text-size -
   Real(Kind=rp), Public :: dts_adj = one ! - default text-size adjustment -
   Real(Kind=rp), Public :: f_scale = one ! - font scaling -
!
! Character scalars
   Character(Len= 32), Public :: cdfont ! - default font -
   Character(Len= 32), Public :: cffont ! - fixed-width font -
!
#else
   Logical, Public :: lwu ! - update windows? -
#endif
! Derived type scalars
   Type(gfont), Public :: font1 ! - default font -
   Type(gfont), Public :: font2 ! - title font -
   Type(gfont), Public :: font3 ! - numeric labels font -
!
#if GUI == 1
   Type(frame), Public :: margin ! - margins -
!
   Type(viewport), Public :: view ! - viewport -
!
#endif
! Interfaces
!
! Generic interfaces
   Interface window_update
      Module Procedure window_update_i
      Module Procedure window_update_r
      Module Procedure window_update_c
      Module Procedure window_update_l
   End Interface window_update
!
Contains
!
!
 Subroutine init_screen ()
!
! Initialises screen constants
!
#if GUI == 1
! Modules
   Use clrwin$, Only: clearwin_info$
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Int, Min
!
#endif
! Executable Statements
!
! Initialise screen size settings
#if GUI == 1
   iwdth = Int(clearwin_info$('screen_width'))
   ihght = Int(clearwin_info$('screen_depth'))
   isize = Min(iwdth, ihght)
! - set default graph/map graphics area -
   igsize = isize/2.1
! - set default small map graphics area -
   ihsize = 0.85*igsize
#else
   iwdth = iwdthd
#endif
!
   Return
 End Subroutine init_screen
!
!
!
#if GUI == 1
 Subroutine init_fonts ()
!
! Initialises windows fonts
!
! Modules
   Use clrwin$,            Only: get_font_name$
   Use data_gui_constants, Only: dfont, ffont, iwid_dfont, iwid_ffont, nfont, nffont
!
! Locals
!
! Local scalars
   Integer :: ifont ! - font identifier -
   Integer :: jfont ! - default font identifier -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Check for availability of default font
   jfont = 1
   ifont = 0
   Do
      ifont = ifont + 1
      Call get_font_name$ (cdfont, ifont)
      If (Trim(cdfont) == dfont(jfont)) Exit
      If (cdfont == '') Then
         If (jfont < nfont) Then
            jfont = jfont + 1
            ifont = 0
         Else
            Call get_font_name$ (cdfont, ifont-1)
            Exit
         End If
      End If
   End Do
!
! Check for availability of fixed-width font
   jfont = 1
   ifont = 0
   Do
      ifont = ifont + 1
      Call get_font_name$ (cffont, ifont)
      If (Trim(cffont) == ffont(jfont)) Exit
      If (cffont == '') Then
         If (jfont < nffont) Then
            jfont = jfont + 1
            ifont = 0
         Else
            Call get_font_name$ (cffont, ifont-1)
            ifont = 0
            Exit
         End If
      End If
   End Do
!
! Set default text size
   dts = -0.15_rp + 0.0012_rp*Real(isize, Kind=rp)
   dts = dts*dts_adj
!
! Calculate font-scaling
   If (ifont > 0) Then
      f_scale = Real(iwid_ffont(jfont),Kind=rp)/Real(iwid_dfont,Kind=rp)
   Else
      f_scale = one
   End If
!
   Return
 End Subroutine init_fonts
!
!
!
 Subroutine margins (x1, x2, y1, y2, isize)
!
! Sets margin for graphics area
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isize ! - size of graphic (in pixels) -
!
   Real(Kind=rp), Intent(In) :: x1 ! - left margin -
   Real(Kind=rp), Intent(In) :: x2 ! - right margin -
   Real(Kind=rp), Intent(In) :: y1 ! - top margin -
   Real(Kind=rp), Intent(In) :: y2 ! - bottom margin -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Initialise graphic size
   igsz = isize
!
! Set plotting region
   margin%left = Nint(Real(igsz, Kind=rp)*x1)
   margin%right = igsz - Nint(Real(igsz, Kind=rp)*x2)
   margin%top = Nint(Real(igsz, Kind=rp)*y1)
   margin%bottom = igsz - Nint(Real(igsz, Kind=rp)*y2)
!
! Set viewport border widths
   margin%length = margin%right + 1 - margin%left
   margin%depth = margin%bottom + 1 - margin%top
!
   Return
 End Subroutine margins
!
!
!
 Subroutine init_view (x1, x2, y1, y2)
!
! Sets viewport border widths
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: x1 ! - x minimum -
   Real(Kind=rp), Intent(In) :: x2 ! - x maximum -
   Real(Kind=rp), Intent(In) :: y1 ! - y minimum -
   Real(Kind=rp), Intent(In) :: y2 ! - y maximum -
!
! Executable Statements
   view%xmin = x1
   view%xmax = x2
   view%length= x2 - x1
   view%ymin = y1
   view%ymax = y2
   view%height = y2 - y1
!
   Return
 End Subroutine init_view
!
!
!
 Function ix_pixel (x)
!
! Converts x-value to a pixel
!
! Function result
   Integer :: ix_pixel
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: x ! - x value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Calculate position in pixels
   ix_pixel = margin%left + Nint(Real(margin%length, Kind=rp)*(x - view%xmin)/view%length)
!
   Return
 End Function ix_pixel
!
!
!
 Function iy_pixel (y)
!
! Converts y-value to a pixel
!
! Function result
   Integer :: iy_pixel
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: y ! - y value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Calculate position in pixels
   iy_pixel = margin%bottom - Nint(Real(margin%depth, Kind=rp)*(y - view%ymin)/view%height)
!
   Return
 End Function iy_pixel
!
!
!
 Function pixel_ix (ix)
!
! Converts pixel to an x-value
!
! Function result
   Real(Kind=rp) :: pixel_ix
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix ! - x pixel -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate position in x units
   pixel_ix = Real(ix - margin%left, Kind=rp)*view%length/Real(margin%length, Kind=rp) + view%xmin
!
   Return
 End Function pixel_ix
!
!
!
 Function pixel_iy (iy)
!
! Converts pixel to a y-value
!
! Function result
   Real(Kind=rp) :: pixel_iy
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iy ! - y pixel -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate position in y units
   pixel_iy = Real(margin%bottom-iy, Kind=rp)*view%height/Real(margin%depth, Kind=rp) + view%ymin
!
   Return
 End Function pixel_iy
!
!
!
#endif
 Subroutine init_gfonts ()
!
! Initialises graphics font scaling
!
! Modules
   Use data_numbers, Only: one
!
! Executable Statements
!
! Initialise font sizes
! - default font -
   font1%fscale = one
! - title font -
   font2%fscale = one
! - numeric labels font -
   font3%fscale = one
!
   Return
 End Subroutine init_gfonts
!
!
!
#if GUI == 1
 Function reset_gfonts()
!
! Resets graphics font scaling
!
! Function result
   Integer :: reset_gfonts
!
! Executable Statements
!
! Reset font sizes
   Call init_gfonts ()
!
! Update screen
   Call window_update (font1%fscale)
   Call window_update (font2%fscale)
   Call window_update (font3%fscale)
!
   reset_gfonts = 2
!
   Return
 End Function reset_gfonts
!
!
!
 Subroutine set_gfonts ()
!
! Sets graphics font sizes
!
! Modules
   Use clrwin$, Only: select_font$
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Initialise font sizes
! - default font -
   font1%width = Real(0.014, Kind=rp)*font1%fscale
   font1%height = Real(0.035, Kind=rp)*font1%fscale
! - title font -
   font2%width = Real(0.016, Kind=rp)*font2%fscale
   font2%height = Real(0.040, Kind=rp)*font2%fscale
! - numeric labels font -
   font3%width = Real(0.012, Kind=rp)*font3%fscale
   font3%height = Real(0.030, Kind=rp)*font3%fscale
!
! Initialise font
   Call gfont_size (font1)
   Call select_font$ (Trim(cdfont))
!
   Return
 End Subroutine set_gfonts
!
!
!
 Subroutine gfont_size (f)
!
! Sets font size
!
! Modules
   Use clrwin$, Only: size_in_pixels$
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(gfont), Intent(In) :: f ! - font -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Calculate position in pixels
   ix_font = Nint(Real(igsz, Kind=rp)*f%width)
   iy_font = Nint(Real(igsz, Kind=rp)*f%height)
!
! Set size
   Call size_in_pixels$ (iy_font, ix_font)
!
   Return
 End Subroutine gfont_size
!
!
!
#endif
 Subroutine window_update_i (i)
!
! Updates an integer value in a ClearWin+ window
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: i ! - integer -
!
#if GUI == 1
! Functions and Subroutines
!
! Silverfrost routines
#if FTN95 == 1
   External :: window_update@
#else
   External :: window_update$
#endif
!
#endif
! Executable Statements
!
! Update value
#if GUI == 1
#if FTN95 == 1
   Call window_update@ (i)
#else
   Call window_update$ (i)
#endif
#else
   lwu = .true.
#endif
!
   Return
 End Subroutine window_update_i
!
!
!
 Subroutine window_update_r (r)
!
! Updates a real value in a ClearWin+ window
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: r ! - real -
!
#if GUI == 1
! Functions and Subroutines
!
! Silverfrost routines
#if FTN95 == 1
   External :: window_update@
#else
   External :: window_update$
#endif
!
#endif
! Executable Statements
!
! Update value
#if GUI == 1
#if FTN95 == 1
   Call window_update@ (r)
#else
   Call window_update$ (r)
#endif
#else
   lwu = .true.
#endif
!
   Return
 End Subroutine window_update_r
!
!
!
 Subroutine window_update_c (c)
!
! Updates a character value in a ClearWin+ window
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: c ! - character -
!
#if GUI == 1
! Functions and Subroutines
!
! Silverfrost routines
#if FTN95 == 1
   External :: window_update@
#else
   External :: window_update$
#endif
!
#endif
! Executable Statements
!
! Update value
#if GUI == 1
#if FTN95 == 1
   Call window_update@ (c)
#else
   Call window_update$ (c)
#endif
#else
   lwu = .true.
#endif
!
   Return
 End Subroutine window_update_c
!
!
!
 Subroutine window_update_l (l)
!
! Updates a logical value in a ClearWin+ window
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: l ! - logical -
!
#if GUI == 1
! Functions and Subroutines
!
! Silverfrost routines
#if FTN95 == 1
   External :: window_update@
#else
   External :: window_update$
#endif
!
#endif
! Executable Statements
!
! Update value
#if GUI == 1
#if FTN95 == 1
   Call window_update@ (l)
#else
   Call window_update$ (l)
#endif
#else
   lwu = .true.
#endif
!
   Return
 End Subroutine window_update_l
!
!
!
 Subroutine proj_read_screen (iin, ifail)
!
! Reads screen settings from project file
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
! Read screen settings
   Read (Unit=iin, Err=1, End=2) font1%fscale, font2%fscale, font3%fscale
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
 End Subroutine proj_read_screen
!
!
!
 Subroutine proj_write_screen (iout, ifail)
!
! Writes screen settings to project file
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
! Write screen settings
   Write (Unit=iout, Err=1) font1%fscale, font2%fscale, font3%fscale
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_screen
End Module screen
