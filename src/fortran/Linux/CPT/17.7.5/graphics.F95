! Author: Simon Mason
Module graphics
!
! Modules
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: mtitle
#if GUI == 1
   Use data_gui_constants, Only: i_handle, mcol
   Use data_io_constants,  Only: ldir
   Use iofiles,            Only: cptfile
#endif
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: init_exceed
#if GUI == 1
   Public :: contours, draw_bar, draw_exceed, draw_line_graph, draw_map, draw_rel, draw_rhit, draw_roc, draw_scatter,  &
             draw_simple_map, draw_tend, export_montage, identify_location, init_image, init_location, reset_graphics, &
             save_graphic, select_location
!
! Parameters
!
! Local parameters
   Integer, Parameter, Private :: ml = 13431 ! - maximum number of latitudes / longitudes -
!
! Scalars
!
! Integer scalars
   Integer, Public :: icross = 1   ! - markers on graph flag -
   Integer, Public :: imskld = 0   ! - mask land flag -
   Integer, Public :: imsklk = 0   ! - mask lakes flag -
   Integer, Public :: iscore = 1   ! - scores on graphs flag -
   Integer, Public :: ishdland = 0 ! - shade land flag -
   Integer, Public :: ishdsea = 0  ! - shade land flag -
   Integer, Public :: itsh = 1     ! - tercile shading flag -
   Integer, Public :: ivl = 1      ! - vertical lines on graph flag -
   Integer, Public :: ipic         ! - graphics type identifier -
   Integer, Public :: ipis = 0     ! - prediction intervals on graph flag -
!
! Real scalars
   Real(Kind=rp), Public :: gszs ! - graphics size scaling -
!
   Real(Kind=rp), Private :: rrlge ! - rescaled eastern domain limit -
!
! Character scalars
   Character(Len=ldir), Public :: gdir ! - default graphics file directory -
!
   Character(Len=1), Private :: lbd ! - label-bar direction -
!
! Logical scalars
   Logical, Private :: lsvg = .false. ! - SVG output? -
!
#endif
!
! Derived types
!
! Derived type definitions
! - image -
   Public :: image
   Type image
      Sequence
#if GUI == 1
      Integer :: id      ! - image identifier -
      Integer :: isize   ! - image size -
#endif
      Integer :: ncs     ! - number of contours -
#if GUI == 1
      Integer(i_handle) :: ihandle ! - image handle -
#endif
!
      Real(Kind=rp) :: ymin  ! - y-axis minimum -
      Real(Kind=rp) :: ymax  ! - y-axis maximum -
      Real(Kind=rp) :: scale ! - scaling -
!
      Character(Len=mtitle) :: title ! - image title -
#if GUI == 1
!
      Logical :: lcontour ! - contours set? -
!
      Real(Kind=rp) :: contour(mcol) ! - contour levels -
#endif
   End Type image
!
#if GUI == 1
! - graphics files -
   Public gfile
   Type gfile
      Sequence
      Type(cptfile) :: cfile ! - file format -
!
      Real(Kind=rp) :: qual  ! - JPEG quality -
   End Type gfile
!
! Derived type scalars
   Type(gfile), Private, Save :: pic     ! - graphics output file -
   Type(gfile), Private, Save :: pic_old ! - backup greaphics output file -
!
#endif
Contains
!
!
#if GUI == 1
 Subroutine reset_graphics ()
!
! Resets graphics settings
!
! Modules
   Use data_gui_constants, Only: djqual
!
! Executable Statements
!
! Reset graphics directory
   pic%cfile%fdir = gdir
!
! Reset graphics file quality
   pic%qual = djqual
!
   Return
 End Subroutine reset_graphics
!
!
!
 Subroutine draw_line_graph (img, cxlab, cylab, laxis, n, m, y, &
            ix1, d1, kuse,                           &
            ix2, d2, k2use, n2, m2, ilw2, y2, y2is,  &
            ixf, df, kfuse, nf, mf, icolf, yf, yfis, &
            ivert, nse, nts, tobs, ylow, yhgh, ymin, ymax)
!
! Draws a graph of Y against dates or a linearly increasing X
!
! Modules
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use maths,              Only: interval
   Use time,               Only: pdate, &
                                 set_refdate
   Use screen,             Only: init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol
   Use axes,               Only: x_axis, y_axis, &
                                 get_xaxis_limits, get_yaxis_limits, set_axis
   Use markers,            Only: gmarker, &
                                 init_cross
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of lines -
!
   Character(Len=*), Intent(In) :: cxlab ! - x-axis label -
   Character(Len=*), Intent(In) :: cylab ! - y-axis label -
!
   Logical, Intent(In) :: laxis ! - logarithmic axis? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - y-data; shape(n, m) -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graph -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: icolf ! - colour of forecast crosses -
   Integer, Intent(In), Optional :: ilw2  ! - width of added line -
   Integer, Intent(In), Optional :: ivert ! - plot vertical lines flag -
   Integer, Intent(In), Optional :: ix1   ! - first X value -
   Integer, Intent(In), Optional :: ix2   ! - first X value for overlay data -
   Integer, Intent(In), Optional :: ixf   ! - first X value for new data -
   Integer, Intent(In), Optional :: m2    ! - number of overlay variables -
   Integer, Intent(In), Optional :: n2    ! - number of values, overlay variables -
   Integer, Intent(In), Optional :: mf    ! - number of new data variables -
   Integer, Intent(In), Optional :: nf    ! - number of new data -
   Integer, Intent(In), Optional :: nse   ! - number of subseasons -
   Integer, Intent(In), Optional :: nts   ! - number of thresholds -
!
   Real(Kind=rp), Intent(In), Optional :: ylow ! - lowest y-value (soft limits) -
   Real(Kind=rp), Intent(In), Optional :: yhgh ! - highest y-value (soft limits) -
   Real(Kind=rp), Intent(In), Optional :: ymin ! - minimum value (hard limit) -
   Real(Kind=rp), Intent(In), Optional :: ymax ! - maximum value (hard limit) -
!
   Type(pdate), Intent(In), Optional :: d1 ! - first date on x-axis -
   Type(pdate), Intent(In), Optional :: d2 ! - first date for overlay data -
   Type(pdate), Intent(In), Optional :: df ! - first date for new data -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tobs ! - thresholds -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: y2 ! - overlay data -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: yf ! - new data -
!
   Logical, Dimension(:), Intent(In), Optional :: kuse  ! - cases used? -
   Logical, Dimension(:), Intent(In), Optional :: k2use ! - overlay cases used? -
   Logical, Dimension(:), Intent(In), Optional :: kfuse ! - new cases used? -
!
   Type(interval), Dimension(:), Intent(In), Optional :: y2is ! - hindcast intervals -
   Type(interval), Dimension(:), Intent(In), Optional :: yfis ! - intervals on new data -
!
! Locals
!
! Local scalars
   Integer :: i     ! - ensemble member index -
   Integer :: ivtl  ! - plot vertical lines flag -
   Integer :: iymin ! - earliest year -
!
   Logical :: ldate ! - date axis? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Min, Trim
!
! Executable Statements
!
! Initialise
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Obtain graph limits
! - x-axis -
   If (Present(d1)) Then
      iymin = d1%iyr
      If (Present(d2)) iymin = Min (iymin, d2%iyr)
      If (Present(df)) iymin = Min (iymin, df%iyr)
      Call set_refdate (iymin - 1)
   End If
   Call get_xaxis_limits (x_axis, n, ldate,  &
        ix1=ix1, ix2=ix2, ixf=ixf, d1=d1, d2=d2, df=df, n2=n2, nf=nf)
! - y-axis -
   Call get_yaxis_limits (img%lcontour, laxis, y_axis, y, img%ymin, img%ymax, img%scale, &
        y2=y2, yf=yf, ylow=ylow, yhgh=yhgh, ymin=ymin, ymax=ymax)
!
! Initialise axes
! - y-axis -
   y_axis%laxis = laxis
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, Trim(cxlab), .false., ldate, ix_pixel)
   Call set_axis (y_axis, Trim(cylab), laxis, .false., iy_pixel)
!
! Draw graph base
   If (Present(ivert)) Then
      ivtl = ivert
   Else
      ivtl = ivl
   End If
   Call draw_graph_base (img%title, x_axis, y_axis, ivtl, &
        nse=nse, nty=nts, ty=tobs)
!
! Define marker dimensions
   If (icross == 1) Call init_cross (x_axis, y_axis, ix_pixel, iy_pixel)
!
! Plot data
   Do i = 1, m
      Call add_line (laxis, n, y(:,i), i, 2, &
           ixf=ix1, df=d1, kuse=kuse)
   End Do
! - add additional data -
   If (Present(y2)) Then
      Do i = 1, m2
         Call add_line (laxis, n2, y2(:,i), 2, ilw2, &
              ixf=ix2, df=d2, kuse=k2use)
      End Do
! - add intervals -
      If (Present(y2is) .and. ipis == 1) Then
         Call add_errors (n2, y2is(:), 2, 1, gmarker%ixd, &
              ixf=ix2, df=d2, kuse=k2use)
      End If
   End If
!
! Add forecasts
   If (Present(yf)) Then
      Call add_crosses (nf, mf, yf(:,:), icol(icolf), 2, &
           ixf=ixf, df=df, kuse=kfuse)
! - add intervals -
      If (Present(yfis) .and. ipis == 1) Then
         Call add_errors (nf, yfis(:), icolf, 1, gmarker%ixd, &
              ixf=ixf, df=df, kuse=kfuse)
      End If
   End If
!
   Return
 End Subroutine draw_line_graph
!
!
!
 Subroutine draw_scatter (img, xlab, ylab, n, nts, x, y, leqal, &
                          tobs, ilink, b0, b1, xmin, xmax, ylow, yhgh, ymin, ymax, t)
!
! Draws a scatter diagram
!
! Modules
   Use mswinprm$,          Only: ps_dashdot, ps_solid
   Use clrwin$,            Only: draw_filled_rectangle$, draw_line_between$, set_line_style$, set_line_width$
   Use data_cpt_constants, Only: il_idt
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol
   Use regression,         Only: glm_finv_link
   Use axes,               Only: x_axis, y_axis, &
                                 get_yaxis_limits, set_axis
   Use markers,            Only: draw_cross, init_cross
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nts ! - number of thresholds -
!
   Character(Len=*), Intent(In) :: xlab ! - x-axis label -
   Character(Len=*), Intent(In) :: ylab ! - y-axis label -
!
   Logical, Intent(In) :: leqal ! - equal axis limits? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - x-values -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - y-values -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graph -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ilink ! - link function -
!
   Real(Kind=rp), Intent(In), Optional :: b0   ! - regression constant -
   Real(Kind=rp), Intent(In), Optional :: b1   ! - regression coefficient -
   Real(Kind=rp), Intent(In), Optional :: xmin ! - minimum X value (hard limit) -
   Real(Kind=rp), Intent(In), Optional :: xmax ! - maximum X value (hard limit) -
   Real(Kind=rp), Intent(In), Optional :: ylow ! - lowest Y value (soft limits) -
   Real(Kind=rp), Intent(In), Optional :: yhgh ! - highest Y value (soft limits) -
   Real(Kind=rp), Intent(In), Optional :: ymin ! - minimum Y value (hard limit) -
   Real(Kind=rp), Intent(In), Optional :: ymax ! - maximum Y value (hard limit) -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t    ! - binomial coefficients -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tobs ! - observed thresholds -
!
! Locals
!
! Local scalars
   Integer :: nrp = 2**8 ! - number of points on regression curve -
!
! Local scalars
   Integer :: i       ! - indices -
   Integer :: ir      ! - link function -
   Integer :: ix1,ix2 ! - X plotting positions -
   Integer :: iy1,iy2 ! - Y plotting positions -
!
   Real(Kind=rp) :: xlow ! - lowest x-value -
   Real(Kind=rp) :: xhgh ! - highest x-value -
   Real(Kind=rp) :: xinc ! - x-value increment -
   Real(Kind=rp) :: xx   ! - current x-value -
!
! Local workspace
   Real(Kind=rp), Dimension(1,1) :: yy ! - current y-value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal, Min, MinVal, Present, Real, Reshape
!
! Executable Statements
!
! Initialise
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Obtain graph limits
   If (leqal) Then ! - equal limits -
      Call get_yaxis_limits (img%lcontour, .false., y_axis, Reshape(y(1:n), [n, 1]), img%ymin, img%ymax, img%scale, &
           y2=Reshape(x(1:n), [n, 1]), ylow=ylow, yhgh=yhgh, ymin=ymin, ymax=ymax)
      x_axis%amin = y_axis%amin
      x_axis%amax = y_axis%amax
      x_axis%tick1 = y_axis%tick1
      x_axis%tick2 = y_axis%tick2
      x_axis%scale = y_axis%scale
   Else ! - independent limits -
      If (.not.Present(xmin) .and. .not.Present(xmax)) Then
         Call get_yaxis_limits (img%lcontour, .false., x_axis, Reshape(x(1:n), [n, 1]), img%ymin, img%ymax, img%scale)
      End If
      Call get_yaxis_limits (img%lcontour, .false., y_axis, Reshape(y(1:n), [n, 1]), img%ymin, img%ymax, img%scale)
   End if
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, xlab, .false., .false., ix_pixel)
   Call set_axis (y_axis, ylab, .false., .false., iy_pixel)
!
! Add category shading
   If (Present(tobs) .and. (itsh == 1)) Then
      Call set_line_width$ (0)
! - below/below -
      ix1 = x_axis%i_min
      ix2 = Min(Max(ix_pixel(tobs(1)*x_axis%scale), x_axis%i_min), x_axis%i_max)
      iy1 = y_axis%i_min
      iy2 = Max(Min(iy_pixel(tobs(1)*y_axis%scale), y_axis%i_min), y_axis%i_max)
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(11))
! - normal/below -
      If (nts == 2) Then
         ix1 = ix2
         ix2 = Min(Max(ix_pixel(tobs(2)*x_axis%scale), x_axis%i_min), x_axis%i_max)
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(15))
      End If
! - above/below -
      ix1 = x_axis%i_max
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(16))
! - below/normal -
      If (nts == 2) Then
         ix1 = x_axis%i_min
         ix2 = Min(Max(ix_pixel(tobs(1)*x_axis%scale), x_axis%i_min), x_axis%i_max)
         iy1 = iy2
         iy2 = Max(Min(iy_pixel(tobs(2)*y_axis%scale), y_axis%i_min), y_axis%i_max)
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(15))
! - normal/normal -
         ix1 = ix2
         ix2 = Min(Max(ix_pixel(tobs(2)*x_axis%scale), x_axis%i_min), x_axis%i_max)
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(12))
! - above/normal -
         ix1 = x_axis%i_max
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(14))
      End If
! - above/above -
      iy1 = y_axis%i_max
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(13))
! -- above/normal --
      If (nts == 2) Then
         ix1 = ix2
         ix2 = Min(Max(ix_pixel(tobs(1)*x_axis%scale), x_axis%i_min), x_axis%i_max)
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(14))
      End If
! -- above/below --
      ix1 = x_axis%i_min
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(16))
      Call set_line_width$ (1)
   End If
!
! Draw graph base
   Call draw_graph_base (img%title, x_axis, y_axis, 0)
!
! Add observed categories
   If (Present(tobs)) Then
      Call set_line_style$ (ps_dashdot)
! - X-axis -
      ix1 = Min(Max(ix_pixel(tobs(1)*x_axis%scale), x_axis%i_min), x_axis%i_max)
      ix2 = ix1
      iy1 = y_axis%i_min
      iy2 = y_axis%i_max
      Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
      If (nts == 2) Then
         ix1 = Min(Max(ix_pixel(tobs(2)*x_axis%scale), x_axis%i_min), x_axis%i_max)
         ix2 = ix1
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
      End If
! - Y-axis -
      ix1 = x_axis%i_min
      ix2 = x_axis%i_max
      iy1 = Max(Min(iy_pixel(tobs(1)*y_axis%scale), y_axis%i_min), y_axis%i_max)
      iy2 = iy1
      Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
      If (nts == 2) Then
         iy1 = Max(Min(iy_pixel(tobs(2)*y_axis%scale), y_axis%i_min), y_axis%i_max)
         iy2 = iy1
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
      End If
      Call set_line_style$ (ps_solid)
   End If
!
! Add regression line
   If (Present(b0) .and. Present(b1)) Then
      If (Present(ilink)) Then
         ir = ilink
      Else
         ir = il_idt
      End If
      xlow = MinVal(x(:))
      xhgh = MaxVal(x(:))
      ix1 = ix_pixel(xlow*x_axis%scale)
      Call set_line_width$ (2)
      Select Case (ir)
       Case (il_idt)
         iy1 = iy_pixel((b0+b1*xlow)*y_axis%scale)
         ix2 = ix_pixel(xhgh*x_axis%scale)
         iy2 = iy_pixel((b0+b1*xhgh)*y_axis%scale)
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(4))
       Case Default
         xinc = (xhgh - xlow)/Real(nrp, Kind=rp)
         xx = xlow
         yy = b0 + b1*xx
         Call glm_finv_link (ir, 1, yy(:,:), &
              t=t)
         iy1 = iy_pixel(yy(1,1)*y_axis%scale)
         Do i = 1, nrp
            xx = xx + xinc
            ix2 = ix_pixel(xx*x_axis%scale)
            yy = b0 + b1*xx
            Call glm_finv_link (ir, 1, yy(:,:), &
                 t=t)
            iy2 = iy_pixel(yy(1,1)*y_axis%scale)
            Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(4))
            ix1 = ix2
            iy1 = iy2
         End Do
      End Select
      Call set_line_width$ (1)
   End If
!
! Plot data
   Call init_cross (x_axis, y_axis, ix_pixel, iy_pixel)
   Do i = 1, n
      Call draw_cross (ix_pixel(x(i)*x_axis%scale), iy_pixel(y(i)*y_axis%scale), 2, icol(3))
   End Do
!
   Return
 End Subroutine draw_scatter
!
!
!
 Subroutine draw_exceed (img, xlab, ylab, n, v, nep, f, e, c, &
            nts, tobs)
!
! Draws an exceedence probability graph
!
! Modules
   Use clrwin$,            Only: draw_line_between$, set_line_width$
   Use data_numbers,       Only: zero, one, ten, twenty, oneh
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: iy_font, &
                                 init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol, &
                                 set_cursor_waiting
   Use axes,               Only: x_axis, y_axis, &
                                 draw_xaxis, draw_yaxis, get_yaxis_limits, set_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nep ! - number of points on exceedance probability curve -
!
   Character(Len=*), Intent(In) :: xlab ! - x-axis label -
   Character(Len=*), Intent(In) :: ylab ! - y-axis label -
!
! - input arrays -
   Real(Kind=rp), Dimension( :), Intent(In) :: v ! - series (sorted in ascending order) -
   Real(Kind=rp), Dimension(0:), Intent(In) :: f ! - probabilities of exceedance given forecast -
   Real(Kind=rp), Dimension(0:), Intent(In) :: e ! - empirical probabilities of exceedance -
   Real(Kind=rp), Dimension(0:), Intent(In) :: c ! - climatological probabilities of exceedance -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graph -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nts ! - number of thresholds -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tobs ! - thresholds -
!
! Locals
!
! Local scalars
   Integer :: i        ! - index -
   Integer :: ix1, ix2 ! - X plotting positions -
   Integer :: iy1, iy2 ! - Y plotting positions for climatology -
   Integer :: iy3, iy4 ! - Y plotting positions for fitted climatology -
   Integer :: iz1, iz2 ! - Y plotting positions -
!
   Real(Kind=rp) :: x      ! - x-value -
   Real(Kind=rp) :: xinc   ! - x-value increment -
   Real(Kind=rp) :: xmin   ! - minimum x-value -
   Real(Kind=rp) :: xmax   ! - maximum x-value -
   Real(Kind=rp) :: xscale ! - x-scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Initialise
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Obtain graph limits
! - x-axis -
   xmin = img%ymin
   xmax = img%ymax
   xscale = img%scale
   Call get_yaxis_limits (img%lcontour, .false., x_axis, Reshape(v(1:n), [n, 1]), xmin, xmax, xscale, &
        ymin=img%ymin, ymax=img%ymax)
   x_axis%scale = img%scale
! - y-axis -
   y_axis%amin = zero
   y_axis%amax = oneh
   y_axis%scale = one
   y_axis%tick1 = twenty
   y_axis%tick2 = ten
!
! Initialise axes
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, xlab, .false., .false., ix_pixel)
   Call set_axis (y_axis, ylab, .false., .false., iy_pixel)
!
! Draw graph base
   Call draw_graph_base (img%title, x_axis, y_axis, 0, &
        ntx=nts, tx=tobs)
!
! Add additional axes
! - top x axis -
   y_axis%aint = oneh
   Call draw_xaxis (x_axis, 1, .false., iy_font)
! - right y axis -
   x_axis%aint = x_axis%amax
   Call draw_yaxis (y_axis, -1, .false., iy_font)
!
! Plot data
   xinc = (x_axis%amax - x_axis%amin)/Real(nep, Kind=rp)
   x = x_axis%amin
   ix1 = x_axis%i_min
   iy1 = iy_pixel(e(0))
   iy3 = iy_pixel(c(0))
   iz1 = iy_pixel(f(0))
   Do i = 1, nep
      Call set_cursor_waiting (1)
      x = x_axis%amin + Real(i, Kind=rp)*xinc
      ix2 = ix_pixel(x)
! - empirical climatology -
      iy2 = iy_pixel(e(i))
      Call set_line_width$ (1)
      Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(2))
! - fitted climatology -
      iy4 = iy_pixel(c(i))
      Call set_line_width$ (4)
      Call draw_line_between$ (ix1, iy3, ix2, iy4, icol(2))
! - forecast -
      iz2 = iy_pixel(f(i))
      Call draw_line_between$ (ix1, iz1, ix2, iz2, icol(3))
      ix1 = ix2
      iy1 = iy2
      iy3 = iy4
      iz1 = iz2
      Call set_cursor_waiting (0)
   End Do
   Call set_line_width$ (1)
!
   Return
 End Subroutine draw_exceed
!
!
!
#endif
 Subroutine init_exceed (img, n, v, &
            xmin, xmax)
!
! Initialises an exceedence probability graph
!
! Modules
#if GUI == 1
   Use data_numbers,       Only: one
#endif
   Use data_gui_constants, Only: mnnt, mxnt
   Use maths,              Only: calc_limits
#if GUI == 1
   Use maths,              Only: get_intervals
   Use axes,               Only: x_axis
#endif
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graph -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - series (sorted in ascending order) -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: xmin ! - absolute minimum x-value -
   Real(Kind=rp), Intent(In), Optional :: xmax ! - absolute maximum x-value -
!
#if GUI == 0
! Locals
!
! Local scalars
   Real(Kind=rp) :: tick ! - tick mark intervals -
!
#endif
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Obtain graph limits
   If (Present(xmin) .and. Present(xmax)) Then
#if GUI == 1
      x_axis%amin = xmin
      x_axis%amax = xmax
      x_axis%scale = one
      Call get_intervals (x_axis%amin, x_axis%amax, mnnt, mxnt, x_axis%tick1)
#endif
   Else
#if GUI == 1
      Call calc_limits (v(1), v(n), mnnt, mxnt, x_axis%amin, x_axis%amax, x_axis%scale, x_axis%tick1)
      img%ymin = x_axis%amin
      img%ymax = x_axis%amax
      img%scale = x_axis%scale
#else
      Call calc_limits (v(1), v(n), mnnt, mxnt, img%ymin, img%ymax, img%scale, tick)
#endif
   End If
!
   Return
 End Subroutine init_exceed
!
!
!
#if GUI == 1
 Subroutine draw_bar (img, nv, nva, y, iuse, ylab, &
            ylow, yhgh, ymin, ymax)
!
! Draws a bar chart
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$
   Use data_numbers,       Only: zero, one
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol
   Use axes,               Only: x_axis, y_axis, &
                                 get_xaxis_limits, get_yaxis_limits, set_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv  ! - number of cases -
   Integer, Intent(In) :: nva ! - number of available cases -
!
   Character(Len=*), Intent(In) :: ylab ! - y-axis label -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used cases -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - series -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graph -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: ylow ! - lowest y-value -
   Real(Kind=rp), Intent(In), Optional :: yhgh ! - highest y-value -
   Real(Kind=rp), Intent(In), Optional :: ymin ! - absolute minimum y-value -
   Real(Kind=rp), Intent(In), Optional :: ymax ! - absolute maximum y-value -
!
! Locals
!
! Local scalars
   Integer :: i        ! - variable index -
   Integer :: ix1, ix2 ! - X plotting positions -
   Integer :: iy1, iy2 ! - Y plotting positions -
!
   Real(Kind=rp) :: ylw ! - low y-value -
   Real(Kind=rp) :: yhg ! - high y-value -
!
   Logical :: ldum ! - dummy argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal, Min, MinVal, Present, Real
!
! Executable Statements
!
! Initialise
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Obtain graph limits
! - x-axis -
   Call get_xaxis_limits (x_axis, nv, ldum,  &
        ix1=0)
! - y-axis -
   If (Present(ymin) .and. Present(ymax)) Then
      ylw = ymin
      yhg = ymax
   Else If (Present(ylow) .and. Present(yhgh)) Then
      ylw = ylow
      yhg = yhgh
   Else
      ylw = Min(zero, MinVal(y(1:nva)))
      yhg = Max(zero, MaxVal(y(1:nva)))
   End If
   If (img%lcontour) Then
      y_axis%amin = img%ymin
      y_axis%amax = img%ymax
      y_axis%scale = one
   End If
   Call get_yaxis_limits (img%lcontour, .false., y_axis, Reshape(y(1:nva), [nva, 1]), y_axis%amin, y_axis%amax, y_axis%scale, &
        ylow=ylw, yhgh=yhg, ymin=ymin, ymax=ymax)
   img%ymin = y_axis%amin
   img%ymax = y_axis%amax
   img%scale = y_axis%scale
!
! Initialise axes
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, ' ', .false., .false., ix_pixel)
   Call set_axis (y_axis, ylab, .false., .false., iy_pixel)
!
! Draw graph base
   Call draw_graph_base (img%title, x_axis, y_axis, 0)
!
! Plot data
   If (y_axis%amin > zero) Then
      iy1 = y_axis%i_min
   Else If (y_axis%amax < zero) Then
      iy1 = y_axis%i_max
   Else
      iy1 = iy_pixel(zero)
   End If
   Do i = 1, nva
      ix1 = ix_pixel(Real(iuse(i)-1, Kind=rp)+0.1_rp)
      ix2 = ix_pixel(Real(iuse(i), Kind=rp)-0.1_rp)
      iy2 = iy_pixel(y(i)*y_axis%scale)
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(2))
   End Do
!
   Return
 End Subroutine draw_bar
!
!
!
 Subroutine draw_roc (img, n, nr, hit, far, roca, ccat)
!
! Draws an ROC diagram
!
! Modules
   Use clrwin$,            Only: draw_characters$, draw_line_between$, get_text_size$, set_line_width$
   Use data_numbers,       Only: zero, one, half
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: iy_font, &
                                 init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol
   Use axes,               Only: x_axis, y_axis, &
                                 draw_xaxis, draw_yaxis, set_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: nr ! - number of ROC curves -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: roca  ! - ROC areas -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: hit ! - hit rates -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: far ! - false alarm rates -
!
   Character(Len=*), Dimension(:), Intent(In) :: ccat ! - category labels -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graphic -
!
! Locals
!
! Local scalars
   Integer :: i             ! - indices -
   Integer :: j             ! - ROC curve index -
   Integer :: ix1, ix2, ix3 ! - X plotting positions -
   Integer :: iy1, iy2, iy3 ! - Y plotting positions -
   Integer :: isw, iswmx    ! - string width -
   Integer :: isd           ! - string depth -
   Integer :: ic            ! - colour index -
!
   Character(Len=18) :: lbl ! - category label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustR, Real, Sum
!
! Executable Statements
!
! Initialise
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Initialise axes
   x_axis%amin = zero
   x_axis%amax = one
   x_axis%scale = one
   x_axis%tick1 = 0.2_rp
   x_axis%tick2 = 0.1_rp
   y_axis%amin = zero
   y_axis%amax = one
   y_axis%scale = one
   y_axis%tick1 = 0.2_rp
   y_axis%tick2 = 0.1_rp
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, 'False-alarm rate', .false., .false., ix_pixel)
   Call set_axis (y_axis, 'Hit rate', .false., .false., iy_pixel)
!
! Draw graph base
   Call draw_graph_base (img%title, x_axis, y_axis, 0)
!
! Add additional axes
! - top x axis -
   y_axis%aint = one
   Call draw_xaxis (x_axis, 1, .false., iy_font)
! - right y axis -
   x_axis%aint = one
   Call draw_yaxis (y_axis, -1, .false., iy_font)
!
! Plot diagonal
   ix1 = x_axis%i_min
   ix2 = x_axis%i_max
   iy1 = y_axis%i_min
   iy2 = y_axis%i_max
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
!
! Plot ROCs
   Call set_line_width$ (2)
   ic = 0
   Do j = 1, nr
      ix1 = x_axis%i_min
      iy1 = y_axis%i_min
      Do i = 1, n
         ix2 = ix_pixel(far(i,j))
         iy2 = iy_pixel(hit(i,j))
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(6+j+ic))
         ix1 = ix2
         iy1 = iy2
      End Do
      If (nr == 2) ic = 1
   End Do
!
! Add line labels
! - determine size of largest label -
   iswmx = 0
   Do j = 1, nr
      Select Case (iscore)
       Case (0)
         Write (lbl, Fmt='(A10)') AdjustR(ccat(j)(1:10))
       Case (1)
         Write (lbl, Fmt='(A10,A,F5.3,A)') AdjustR(ccat(j)(1:10)), ' (', roca(j), ')'
      End Select
      Call get_text_size$ (lbl, isw, isd)
      If (isw > iswmx) iswmx = isw
   End Do
! - determine whether to locate labels in the top left or the bottom right of the graph -
   iy3 = 1.2_rp*isd ! - set vertical spacing to 20% more than height of letters -
   If (Sum(roca(1:nr))/Real(nr, Kind=rp) < half) Then
      ix1 = ix_pixel(0.1_rp)
      ix2 = ix_pixel(0.2_rp)
      ix3 = ix2 + isd/4
      iy1 = iy_pixel(0.95_rp) + iy3*nr
   Else
      ix3 = ix_pixel(one) - (iswmx + isd/2)
      ix2 = ix3 - isd/4
      ix1 = ix2 - (ix_pixel(0.1_rp) - ix_pixel(zero))
      iy1 = iy_pixel(0.05_rp) + iy3
   End If
! - add lines and categories -
   ic = 0
   Do j = 1, nr
      Select Case (iscore)
       Case (0)
         Write (lbl, Fmt='(A10)') AdjustR(ccat(j)(1:10))
       Case (1)
         Write (lbl, Fmt='(A10,A,F5.3,A)') AdjustR(ccat(j)(1:10)), ' (', roca(j), ')'
      End Select
      iy1 = iy1 - iy3
      Call draw_line_between$ (ix1, iy1, ix2, iy1, icol(6+j+ic))
      iy2 = iy1 + isd/2
      Call draw_characters$ (lbl, ix3, iy2, icol(1))
      If (nr == 2) ic = 1
   End Do
   Call set_line_width$ (1)
!
   Return
 End Subroutine draw_roc
!
!
!
 Subroutine draw_rhit (img, ng, n, d1, rhit, kuse, cxaxis)
!
! Draws a ranked hits diagram
!
! Modules
   Use clrwin$,      Only: draw_characters$, draw_line_between$, get_text_size$, set_line_width$
   Use data_numbers, Only: zero, one, oneh
   Use time,         Only: pdate
   Use labels,       Only: l_rhitsdiag
   Use screen,       Only: iy_pixel
   Use gui,          Only: icol
   Use axes,         Only: x_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: n  ! - number of cases -
!
   Character(Len=*), Intent(In) :: cxaxis ! - length of X-axis -
!
   Type(pdate), Intent(In) :: d1 ! - first date -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graphic -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rhit ! - ranked hits -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
! Locals
!
! Local scalars
   Integer :: i                  ! - category index -
   Integer :: isw                ! - string width -
   Integer :: isd                ! - string depth -
   Integer :: ix1, ix2, ix3, ix4 ! - X plotting positions -
   Integer :: iy1, iy2, iy3      ! - Y plotting positions -
!
   Character(Len=1) :: lbl ! - label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Draw ranked hits
   img%ymin = zero
   img%ymax = oneh
   img%scale = one
   img%lcontour = .true.
   Call draw_line_graph (img, cxaxis, Trim(l_rhitsdiag%c), .false., n, ng, rhit(:,:), &
        d1=d1, kuse=kuse(:))
!
! Add labels
! - determine where to locate labels -
   ix1 = x_axis%i_min + (x_axis%i_max - x_axis%i_min)*0.1_rp
   ix2 = x_axis%i_min + (x_axis%i_max - x_axis%i_min)*0.2_rp
! - determine size of largest label -
   Call get_text_size$ ('2', isw, isd)
   ix4 = ix2 + isd/2 + isw
   ix3 = ix4 - isw
! - determine where to locate labels -
   iy3 = 1.2_rp*isd
   iy1 = iy_pixel(0.95_rp*oneh)
   iy2 = iy1 + isd/2
! - below-normal -
   Call set_line_width$ (2)
   Do i = 1, ng
      Call draw_line_between$ (ix1, iy1, ix2, iy1, icol(i+1))
      Write (Unit=lbl, Fmt='(I1)') i
      Call draw_characters$ (lbl, ix3, iy2, icol(1))
      iy1 = iy1 + iy3
      iy2 = iy2 + iy3
   End Do
   Call set_line_width$ (1)
!
   Return
 End Subroutine draw_rhit
!
!
!
 Subroutine draw_rel (img, nb, x, y, f, c, b0, b1, cs, s, ic)
!
! Draws an attributes diagram
!
! Modules
   Use mswinprm$,          Only: ps_dashdot, ps_solid
   Use clrwin$,            Only: draw_characters$, draw_filled_rectangle$, draw_line_between$, get_text_size$, set_line_style$, &
                                 set_line_width$
   Use data_numbers,       Only: zero, one, two, ten, twenty, oneh
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: iy_font, &
                                 init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol
   Use axes,               Only: x_axis, y_axis, &
                                 draw_xaxis, draw_yaxis, set_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nb ! - number of probability bins -
   Integer, Intent(In) :: ic ! - color index -
!
   Real(Kind=rp), Intent(In) :: c  ! - climatological observed relative frequency -
   Real(Kind=rp), Intent(In) :: b0 ! - intercept -
   Real(Kind=rp), Intent(In) :: b1 ! - slope -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - averaged binned probabilities -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - observed relative frequencies -
   Real(Kind=rp), Dimension(:), Intent(In) :: f ! - forecast relative frequencies -
   Real(Kind=rp), Dimension(:), Intent(In) :: s ! - scores -
!
   Character(Len=*), Dimension(:), Intent(In) :: cs ! - score -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graphic -
!
! Locals
!
! Local scalars
   Integer :: i                ! - probability bin index -
   Integer :: i1               ! - starting index -
   Integer :: ix1, ix2         ! - X plotting positions -
   Integer :: iy1, iy2, iy3    ! - Y plotting positions -
   Integer :: isw1, isw2, isw3 ! - string width -
   Integer :: isd              ! - string depth -
!
   Real(Kind=rp) :: pbt ! - number of probability bin thresholds -
   Real(Kind=rp) :: x1  ! - first bin position -
!
   Character(Len=128) :: lbl ! - category label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Real, Trim
!
! Executable Statements
!
! Initialise reliability graph
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Initialise axes
   x_axis%amin = zero
   x_axis%amax = oneh
   x_axis%scale = one
   x_axis%tick1 = twenty
   x_axis%tick2 = ten
   y_axis%amin = zero
   y_axis%amax = oneh
   y_axis%scale = one
   y_axis%tick1 = twenty
   y_axis%tick2 = ten
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, 'Forecast probabilities (%)', .false., .false., ix_pixel)
   Call set_axis (y_axis, 'Observed relative frequencies (%)', .false., .false., iy_pixel)
!
! Draw graph base
   Call draw_graph_base (img%title, x_axis, y_axis, 0)
!
! Add additional axes
! - top x axis -
   y_axis%aint = y_axis%amax
   Call draw_xaxis (x_axis, 1, .false., iy_font)
! - right y axis -
   x_axis%aint=x_axis%amax
   Call draw_yaxis (y_axis, -1, .false., iy_font)
!
! Plot frequencies
   x1 = oneh/Real(2*(nb-1), Kind=rp)
   pbt = oneh/Real(nb-1, Kind=rp)
   ix1 = ix_pixel(zero)
   ix2 = ix_pixel(x1 - one)
   iy1 = iy_pixel(zero)
   iy2 = iy_pixel(f(1)*y_axis%scale)
   Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(5))
   Do i = 2, nb-1
      ix1 = ix_pixel(x1 + pbt*Real(i-2, Kind=rp) + one)
      ix2 = ix_pixel(x1 + pbt*Real(i-1, Kind=rp) - one)
      iy2 = iy_pixel(f(i)*y_axis%scale)
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(5))
   End Do
   ix1 = ix_pixel(oneh - x1 + one)
   ix2 = ix_pixel(oneh)
   iy2 = iy_pixel(f(nb)*y_axis%scale)
   Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(5))
!
! Plot diagonal
   ix1 = x_axis%i_min
   ix2 = x_axis%i_max
   iy1 = y_axis%i_min
   iy2 = y_axis%i_max
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
!
! Plot "no-skill" line
   iy1 = iy_pixel(c/two)
   iy2 = iy_pixel((c + y_axis%amax)/two)
   Call set_line_style$ (ps_dashdot)
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
   Call set_line_style$ (ps_solid)
!
! Plot climatology
   iy1 = iy_pixel(c)
   iy2 = iy1
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
   ix1 = ix_pixel(c)
   ix2 = ix1
   iy1 = y_axis%i_min
   iy2 = y_axis%i_max
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
!
! Plot reliability curve
   Call set_line_width$ (2)
   Do i = 1, nb
      If (f(i) > zero) Then
         i1 = i
         If (i1 == nb) i1 = i1 - 1
         ix1 = ix_pixel(x(i))
         iy1 = iy_pixel(y(i))
         Exit
      End If
   End Do
   Do i = i1+1, nb
      If (f(i) > zero) Then
         ix2 = ix_pixel(x(i))
         iy2 = iy_pixel(y(i))
         Call draw_line_between$ (ix1, iy1, ix2, iy2, ic)
         ix1 = ix2
         iy1 = iy2
      End If
   End Do
   Call set_line_width$ (1)
! - add regression fit -
   Do i = 1, nb
      If (f(i) > zero) Then
         i1 = i
         ix1 = ix_pixel(x(i))
         iy1 = iy_pixel(b0 + b1*x(i))
         Exit
      End If
   End Do
   Do i = nb, i1, -1
      If (f(i) > zero) Then
         If (i > i1) Then
            ix2 = ix_pixel(x(i))
            iy2 = iy_pixel(b0 + b1*x(i))
            Call draw_line_between$ (ix1, iy1, ix2, iy2, ic)
         End If
         Exit
      End If
   End Do
!
! Add scores
   If (iscore == 1) Then
      Call get_text_size$ (Trim(cs(1)), isw1, isd)
      Call get_text_size$ (Trim(cs(2)), isw2, isd)
      Call get_text_size$ ('Slope', isw3, isd)
      ix1 = ix_pixel( 3.0_rp)
      iy1 = iy_pixel(94.0_rp)
      Call draw_characters$ (cs(1), ix1, iy1, icol(1))
      iy2 = iy1 + 1.2_rp*isd
      Call draw_characters$ (cs(2), ix1, iy2, icol(1))
      iy3 = iy2 + 1.2_rp*isd
      Call draw_characters$ ('Slope', ix1, iy3, icol(1))
      ix2 = ix1 + Max(isw1, isw2)*1.1_rp
      Write (lbl, Fmt='(F7.3)') s(1)
      Call draw_characters$ (lbl, ix2, iy1, icol(1))
      Write (lbl, Fmt='(F7.3)') s(2)
      Call draw_characters$ (lbl, ix2, iy2, icol(1))
      Write (lbl, Fmt='(F6.2,A)') b1, '%'
      Call draw_characters$ (lbl, ix2, iy3, icol(1))
   End If
!
   Return
 End Subroutine draw_rel
!
!
!
 Subroutine draw_tend (img, ng, cs, t)
!
! Draws a tendency diagram
!
! Modules
   Use clrwin$,            Only: draw_characters$, draw_filled_rectangle$, get_text_size$
   Use data_numbers,       Only: zero, tenth, half, one, ten, twenty, oneh
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: init_view, ix_pixel, iy_pixel
   Use gui,                Only: icol
   Use axes,               Only: x_axis, y_axis, &
                                 set_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of categories -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: t ! - tendencies -
!
   Character(Len=*), Dimension(:), Intent(In) :: cs ! - categories -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - graphic -
!
! Locals
!
! Local scalars
   Integer :: i             ! - probability bin index -
   Integer :: ix1, ix2, ix3 ! - X plotting positions -
   Integer :: iy1, iy2, iy3 ! - Y plotting positions -
!
   Real(Kind=rp) :: x1, x2, x3 ! - bin positions -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Initialise reliability graph
   Call init_graphic (img, dleft, dright, dbottom, dtop)
!
! Initialise axes
   x_axis%amin = zero
   x_axis%amax = Real(ng)
   x_axis%scale = one
   x_axis%tick1 = zero
   x_axis%tick2 = zero
   y_axis%amin = zero
   y_axis%amax = oneh
   y_axis%scale = one
   y_axis%tick1 = twenty
   y_axis%tick2 = ten
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
   Call set_axis (x_axis, 'Category', .false., .false., ix_pixel)
   Call set_axis (y_axis, 'Percentage (observed - grey; forecast - black)', .false., .false., iy_pixel)
!
! Draw graph base
   Call draw_graph_base (img%title, x_axis, y_axis, 0)
!
! Add category labels
   x1 = half
   iy1 = iy_pixel(zero)
   Call get_text_size$ (Trim(cs(1)), ix2, iy2)
   iy1 = iy1 + iy2 + 2
   Do i = 1, ng
      ix1 = ix_pixel(x1)
      Call get_text_size$ (Trim(cs(i)), ix2, iy2)
      ix1 = ix1 - half*Real(ix2, Kind=rp)
      Call draw_characters$ (cs(i),ix1,iy1, icol(1))
      x1=x1+one
   End Do
!
! Plot frequencies
   x1=tenth
   x2=half
   x3 = one-tenth
   iy1 = iy_pixel(zero)
   Do i = 1, ng
      ix1 = ix_pixel(x1)
      ix2 = ix_pixel(x2)
      ix3 = ix_pixel(x3)
      iy2 = iy_pixel(t(1,i))
      iy3 = iy_pixel(t(2,i))
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(5))
      Call draw_filled_rectangle$ (ix2, iy1, ix3, iy3, icol(6))
      x1 = x1 + one
      x2 = x2 + one
      x3 = x3 + one
   End Do
!
   Return
 End Subroutine draw_tend
!
!
!
 Subroutine draw_simple_map (isize, mlim, alim, ctitle, ifail)
!
! Draws a map
!
! Modules
   Use data_numbers, Only: zero
   Use gui,          Only: get_handle
   Use space,        Only: area
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isize ! - image size -
!
   Character(Len=*), Intent(In) :: ctitle ! - map title -
!
   Type(area), Intent(In) :: mlim ! - map domain -
   Type(area), Intent(In) :: alim ! - selected domain -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Type(image) :: img_tmp ! - image -
!
! Executable Statements
!
! Initialize image
   img_tmp%ihandle = get_handle()
   img_tmp%isize = isize
   img_tmp%ncs = 0
   img_tmp%title = ctitle
   img_tmp%lcontour = .true.
!
! Draw map
   Call draw_map (img_tmp, [0], [zero], 0, 0, 0, mlim%rltn, mlim%rlts, mlim%rlgw, mlim%rlge, .false., 3, 0, &
        [alim%rltn, alim%rlts], [alim%rlgw, alim%rlge], ifail)
!
   Return
 End Subroutine draw_simple_map
!
!
!
 Subroutine draw_map (img, iuse, v, nlt, nlg, nva, rltn, rlts, rlgw, rlge, lnclr, imap, ncb, rlat, rlng, ifail, &
            vlow, vhgh, vmin, vmax, rmiss, ioff)
!
! Draws a map
!
! Modules
   Use mswinprm$,         Only: ps_dot, ps_solid
   Use clrwin$,           Only: draw_characters$, draw_filled_polygon$, draw_filled_rectangle$, draw_line_between$, &
                                draw_polyline$, get_text_size$, rgb$, set_line_style$
   Use data_numbers,      Only: zero, half, three, oneh, r180, r360, ihuge
   Use data_io_constants, Only: ddir, iin
   Use labels,            Only: ls_cats
   Use screen,            Only: ix_pixel, iy_pixel, &
                                font3, &
                                gfont_size
   Use gui,               Only: icol, icoldom, nclr, &
                                get_cols
   Use axes,              Only: x_axis, y_axis
   Use markers,           Only: gmarker, &
                                draw_marker
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlt  ! - number of latitudes -
   Integer, Intent(In) :: nlg  ! - number of longitudes -
   Integer, Intent(In) :: nva  ! - number of used locations -
   Integer, Intent(In) :: imap ! - grid / station map flag -
   Integer, Intent(In) :: ncb  ! - number of colour bars -
!
   Real(Kind=rp), Intent(In) :: rltn ! - northern area limit -
   Real(Kind=rp), Intent(In) :: rlts ! - southern area limit -
   Real(Kind=rp), Intent(In) :: rlgw ! - western area limit -
   Real(Kind=rp), Intent(In) :: rlge ! - eastern area limit -
!
   Logical, Intent(In) :: lnclr ! - compute number of colours? -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - map -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variable indices -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: v    ! - data -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ioff ! - colour index offset -
!
   Real(Kind=rp), Intent(In), Optional :: vlow  ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh  ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin  ! - absolute minimum value -
   Real(Kind=rp), Intent(In), Optional :: vmax  ! - absolute maximum value -
   Real(Kind=rp), Intent(In), Optional :: rmiss ! - missing values -
!
! Locals
!
! Local scalars
   Integer :: i, j, k            ! - indices -
   Integer :: ij, ija            ! - indices -
   Integer :: ig                 ! - grid offset index -
   Integer :: ic                 ! - grid corner index -
   Integer :: icol_land          ! - land colour -
   Integer :: ix1, ix2, ix3, ix4 ! - X plotting positions -
   Integer :: iy1, iy2, iy3      ! - Y plotting positions -
   Integer :: ixl                ! - lefthand limit of map and labels -
   Integer :: iyb                ! - bottom limit of map and labels -
   Integer :: indx               ! - colour index -
   Integer :: indx0              ! - colour index offset -
   Integer :: ios                ! - IO status -
   Integer :: isw                ! - string width -
   Integer :: isd                ! - string depth -
   Integer :: ncoor              ! - number of coordinates per country -
   Integer :: ncls               ! - number of contour levels -
   Integer :: nskip              ! - number of contour labels to skip -
   Integer :: iskip              ! - number of contour labels skipped -
!
   Real(Kind=rp) :: xadj ! - longitude shift -
   Real(Kind=rp) :: rlt  ! - latitudes -
   Real(Kind=rp) :: rlg  ! - longitudes -
   Real(Kind=rp) :: rlg1 ! - first longitude -
   Real(Kind=rp) :: rlg2 ! - second longitude -
   Real(Kind=rp) :: x    ! - current longitude -
   Real(Kind=rp) :: y    ! - current latitude -
!
   Character(Len=4) :: clab ! - coordinate label -
!
   Logical :: loffe ! - map offset east? -
   Logical :: loffw ! - map offset west? -
   Logical :: ld1   ! - polygon falls at least partly in domain? -
   Logical :: lde   ! - east shifted polygon falls at least partly in domain? -
   Logical :: ldw   ! - west shifted polygon falls at least partly in domain? -
   Logical :: le    ! - file existence? -
!
! Local arrays
   Integer, Dimension(ml,2) :: ixs ! - X plotting positions -
   Integer, Dimension(ml)   :: iys ! - Y plotting positions -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal, Min, MinVal, Mod, Nint, Present, Real, Size, Trim
!
! Executable Statements
!
! Initialise
   Call init_map (img, rltn, rlts, rlgw, rlge)
! - check for longitude shift -
   loffe=(rrlge > r180)
   loffw=(rlgw < -r180)
!
! Calculate grid-spacing
   x_axis%tick1 = gridsp(rltn, rlts, rlgw, rrlge)
   x_axis%tick2 = x_axis%tick1
   y_axis%tick1 = x_axis%tick1
   y_axis%tick2 = y_axis%tick1
!
! Determine map limits
   x_axis%i_min = ix_pixel(rlgw)
   x_axis%i_max = ix_pixel(rrlge)
   y_axis%i_min = iy_pixel(rlts)
   y_axis%i_max = iy_pixel(rltn)
!
! Determine contours
   If (.not.img%lcontour) Then
     If (Present(vmin) .and. Present(vmax)) Then
         Call contours (img, &
              zmin=vmin, zmax=vmax)
      Else If (Present(vlow) .and. Present(vhgh)) Then
         Call contours (img, &
              zlow=vlow, zhgh=vhgh)
      Else
         Call contours (img, &
              zlow=MinVal(v), zhgh=MaxVal(v))
      End If
! - set colours -
      ncls = img%ncs
   Else If (.not.lnclr) Then
      ncls = img%ncs
   Else
      ncls = img%ncs + 1
      If (Present(ioff)) ncls = ncls + ioff*2
   End If
   ifail = get_cols(ncls)
   If (ifail /= 0) Then
      ifail = 5
      Return
   End If
! - centre colour indices based on number of contours -
   indx0 = 20 + (nclr - img%ncs)/2
   If (Present(ioff)) indx0 = indx0 + ioff
!
! Pre-shade the continents and/or sea
   ifail = 0
   Inquire (File=Trim(ddir)//'boundaries_high.dat', Exist=le)
   If (le) Then
      If (ishdland == 1 .or. ishdsea == 1) Then
         Select Case (ishdsea)
          Case (1)
            icol_land = rgb$(200, 200, 200)
            ix1 = x_axis%i_min
            ix2 = x_axis%i_max
            iy1 = y_axis%i_min
            iy2 = y_axis%i_max
            Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol_land) ! - shade everywhere -
         End Select
         Select Case (ishdland)
          Case (1)
            icol_land = rgb$(200, 200, 200)
          Case (0)
            icol_land = icol(0)
         End Select
         Open (Unit=iin, File=Trim(ddir)//'boundaries_high.dat', Access='sequential', Action='read', Form='unformatted', &
               Status='old')
         read_coors_1: Do
            Read (Unit=iin, IOstat=ios) ncoor
            If (ios /= 0) Exit read_coors_1
            ld1=.false.
            lde=.false.
            ldw=.false.
            Do i = 1, ncoor
               Read (Unit=iin, Err=1, End=1) rlg,rlt
               ixs(i,1) = ix_pixel(rlg)
               iys(i) = iy_pixel(rlt)
               If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                   ixs(i,1) >= x_axis%i_min .and. ixs(i,1) <= x_axis%i_max) ld1=.true.
               If (loffe) Then
                  ixs(i,2) = ix_pixel(rlg+r360)
                  If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                      ixs(i,2) >= x_axis%i_min .and. ixs(i,2) <= x_axis%i_max) lde=.true.
               End If
               If (loffw) Then
                  ixs(i,2) = ix_pixel(rlg-r360)
                  If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                      ixs(i,2) >= x_axis%i_min .and. ixs(i,2) <= x_axis%i_max) ldw=.true.
               End If
            End Do
            If (ld1) Call draw_filled_polygon$ (ixs(:,1), iys(:), ncoor, icol_land)
            If (lde) Call draw_filled_polygon$ (ixs(:,2), iys(:), ncoor, icol_land)
            If (ldw) Call draw_filled_polygon$ (ixs(:,2), iys(:), ncoor, icol_land)
            Cycle read_coors_1
1           ifail = 1 ! - problem reading boundaries file -
            Exit read_coors_1
         End Do read_coors_1
         Close (Unit=iin)
      Else
         icol_land = icol(0)
      End If
   Else
      ifail = 2
   End If
!
! Shade data values
   Select Case (imap)
    Case (1)
      ija = 1
      shade: Do i = 1, nlt
         If (i > 1) Then
            iys(1) = iy_pixel((rlat(i) + rlat(i-1))*half)
         Else If (nlt > 1) Then
            iys(1) = iy_pixel((three*rlat(i) - rlat(i+1))*half)
         Else
            iys(1) = iy_pixel(rltn)
         End If
         If (i < nlt) Then
            iys(2) = iy_pixel((rlat(i) + rlat(i+1))*half)
         Else If (nlt > 1) Then
            iys(2) = iy_pixel((three*rlat(i) - rlat(i-1))*half)
         Else
            iys(2) = iy_pixel(rlts)
         End If
         Do j = 1, nlg
            ij = (i - 1)*nlg + j
            If (iuse(ija) == ij) Then
               If (v(ija) /= rmiss) Then
                  If (j > 1) Then
                     rlg1 = (rlng(j) + rlng(j-1))*half
                  Else If (nlg > 1) Then
                     rlg1 = rlng(1) - (rlng(2) - rlng(1))*half
                  Else
                     rlg1 = rlgw
                  End If
                  If (j < nlg) Then
                     rlg2 = (rlng(j) + rlng(j+1))*half
                  Else If (nlg > 1) Then
                     rlg2 = rlng(nlg) + (rlng(nlg) - rlng(nlg-1))*half
                  Else
                     rlg2 = rlge
                  End If
! - identify colour to use -
                  Do k = 1, img%ncs
                     If (v(ija) < img%contour(k)) Exit
                  End Do
                  indx = indx0 + k
! - shade -
                  ixs(1,1) = ix_pixel(rlg1)
                  ixs(1,2) = ix_pixel(rlg2)
                  ixs(2,1) = ix_pixel(rlg1 + r360)
                  ixs(2,2) = ix_pixel(rlg2 + r360)
                  ixs(3,1) = ix_pixel(rlg1 - r360)
                  ixs(3,2) = ix_pixel(rlg2 - r360)
                  Do ig = 1, 3
                     ld1 = .false.
                     Do ic = 1, 2
                        If (ixs(ig,ic) >= x_axis%i_min .and. ixs(ig,ic) <= x_axis%i_max) ld1 = .true.
                     End Do
                     If (ld1) Call draw_filled_rectangle$ (ixs(ig,1), iys(1), ixs(ig,2), iys(2), icol(indx))
                  End Do
               End If
               ija = ija + 1
               If (ija == 0 .or. ija > Size(iuse)) Exit shade
            End If
         End Do
      End Do shade
!
! Mark station values
    Case (2)
      ix2 = Nint(Real(x_axis%i_max-x_axis%i_min, Kind=rp)*0.007_rp*gmarker%rscale)
      iy2 = Nint(Real(y_axis%i_min-y_axis%i_max, Kind=rp)*0.007_rp*gmarker%rscale)
      gmarker%ixd = Max(ix2, iy2)
      gmarker%iyd = gmarker%ixd
      Do i = 1, nva
         ij = iuse(i)
         If (v(i) == rmiss) Cycle
! - identify colour to use -
         Do k = 1, img%ncs
            If (v(i) < img%contour(k)) Exit
         End Do
         indx=indx0+k
! - identify location on map -
         iy1 = iy_pixel(rlat(ij))
         rlg = rlng(ij)
! - draw marker -
         ix1 = ix_pixel(rlg)
         If (ix1 >= x_axis%i_min .and. ix1 <= x_axis%i_max) Call draw_marker (ix1, iy1, 4, icol(indx))
         ix1 = ix_pixel(rlg-r360)
         If (ix1 >= x_axis%i_min .and. ix1 <= x_axis%i_max) Call draw_marker (ix1, iy1, 4, icol(indx))
         ix1 = ix_pixel(rlg+360)
         If (ix1 >= x_axis%i_min .and. ix1 <= x_axis%i_max) Call draw_marker (ix1, iy1, 4, icol(indx))
      End Do
!
! Mark domain
    Case (3)
      iys(1) = iy_pixel(rlat(1))
      iys(2) = iy_pixel(rlat(2))
      If (rlng(2) > rlng(1)) Then
         xadj = zero
      Else
         xadj = r360
      End If
      ixs(1,1) = ix_pixel(rlng(1))
      ixs(1,2) = ix_pixel(rlng(2) + xadj)
      ixs(2,1) = ix_pixel(rlng(1) + r360)
      ixs(2,2) = ix_pixel(rlng(2) + xadj + r360)
      ixs(3,1) = ix_pixel(rlng(1) - r360)
      ixs(3,2) = ix_pixel(rlng(2) + xadj - r360)
      Do ig = 1, 3
         ld1 = .false.
         If (ixs(ig,1) <= x_axis%i_min .and. ixs(ig,2) >= x_axis%i_max) Then
            ld1 = .true.
         Else
            Do ic = 1, 2
               If (ixs(ig,ic) >= x_axis%i_min .and. ixs(ig,ic) <= x_axis%i_max) ld1 = .true.
            End Do
         End If
         If (ld1) Call draw_filled_rectangle$ (ixs(ig,1), iys(1), ixs(ig,2), iys(2), icoldom)
      End Do
   End Select
 if (lsvg) return
!
! Read boundary coordinate
   ifail = 0
   If (le) Then
      Open (Unit=iin, File=Trim(ddir)//'boundaries_high.dat', Access='sequential', Action='read', Form='unformatted', Status='old')
      read_coors_2: Do
         Read (Unit=iin, IOstat=ios) ncoor
         If (ios /= 0) Exit read_coors_2
         ld1=.false.
         lde=.false.
         ldw=.false.
         Do i = 1, ncoor
            Read (Unit=iin, Err=2, End=2) rlg,rlt
            ixs(i,1) = ix_pixel(rlg)
            iys(i) = iy_pixel(rlt)
            If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                ixs(i,1) >= x_axis%i_min .and. ixs(i,1) <= x_axis%i_max) ld1=.true.
            If (loffe) Then
               ixs(i,2) = ix_pixel(rlg+r360)
               If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                   ixs(i,2) >= x_axis%i_min .and. ixs(i,2) <= x_axis%i_max) lde=.true.
            End If
            If (loffw) Then
               ixs(i,2) = ix_pixel(rlg-r360)
               If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                   ixs(i,2) >= x_axis%i_min .and. ixs(i,2) <= x_axis%i_max) ldw=.true.
            End If
         End Do
         If (imskld == 1) Then
            If (ld1) Call draw_filled_polygon$ (ixs(:,1), iys(:), ncoor, icol_land)
            If (lde) Call draw_filled_polygon$ (ixs(:,2), iys(:), ncoor, icol_land)
            If (ldw) Call draw_filled_polygon$ (ixs(:,2), iys(:), ncoor, icol_land)
         End If
         If (ld1) Call draw_polyline$ (ixs(:,1), iys(:), ncoor, icol(1))
         If (lde) Call draw_polyline$ (ixs(:,2), iys(:), ncoor, icol(1))
         If (ldw) Call draw_polyline$ (ixs(:,2), iys(:), ncoor, icol(1))
         Cycle read_coors_2
2        ifail = 1 ! - problem reading boundaries file -
         Exit read_coors_2
      End Do read_coors_2
      Close (Unit=iin)
   Else
      ifail = 2
   End If
!
! Read lake coordinates
! - confirm existence of file -
   Inquire (File=Trim(ddir)//'lakes_high.dat', Exist=le)
   If (le) Then
      Open (Unit=iin, File=Trim(ddir)//'lakes_high.dat', Access='sequential', Action='read', Form='unformatted', Status='old')
      read_lakes: Do
         Read (Unit=iin, IOstat=ios) ncoor
         If (ios /= 0) Exit read_lakes
         ld1 = .false.
         lde = .false.
         ldw = .false.
         Do i = 1, ncoor
            Read (Unit=iin, Err=3, End=3) rlg, rlt
            ixs(i,1) = ix_pixel(rlg)
            iys(i) = iy_pixel(rlt)
            If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                ixs(i,1) >= x_axis%i_min .and. ixs(i,1) <= x_axis%i_max) ld1 = .true.
            If (loffe) Then
               ixs(i,2) = ix_pixel(rlg + r360)
               If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                   ixs(i,2) >= x_axis%i_min .and. ixs(i,2) <= x_axis%i_max) lde = .true.
            End If
            If (loffw) Then
               ixs(i,2) = ix_pixel(rlg - r360)
               If (  iys(i) >= y_axis%i_max .and.   iys(i) <= y_axis%i_min .and.  &
                   ixs(i,2) >= x_axis%i_min .and. ixs(i,2) <= x_axis%i_max) ldw = .true.
            End If
         End Do
         If (imsklk == 1) Then
            If (ld1) Call draw_filled_polygon$ (ixs(:,1), iys(:), ncoor, icol(0))
            If (lde) Call draw_filled_polygon$ (ixs(:,2), iys(:), ncoor, icol(0))
            If (ldw) Call draw_filled_polygon$ (ixs(:,2), iys(:), ncoor, icol(0))
         End If
         If (ld1) Call draw_polyline$ (ixs(:,1), iys(:), ncoor, icol(1))
         If (lde) Call draw_polyline$ (ixs(:,2), iys(:), ncoor, icol(1))
         If (ldw) Call draw_polyline$ (ixs(:,2), iys(:), ncoor, icol(1))
         Cycle read_lakes
3        ifail = 3 ! - problem reading lakes file -
         Exit read_lakes
      End Do read_lakes
      Close (Unit=iin)
   Else
      ifail = 4
   End If
!
! Clip
   Call draw_filled_rectangle$ (             0,              0, x_axis%i_min-1,   y_axis%i_min, icol(0))
   Call draw_filled_rectangle$ (             0, y_axis%i_min+1,   x_axis%i_max,      img%isize, icol(0))
   Call draw_filled_rectangle$ (x_axis%i_max+1,   y_axis%i_max,      img%isize,      img%isize, icol(0))
   Call draw_filled_rectangle$ (  x_axis%i_min,              0,      img%isize, y_axis%i_max-1, icol(0))
!
! Draw border
   ix1 = x_axis%i_min
   ix2 = x_axis%i_min
   iy1 = y_axis%i_min
   iy2 = y_axis%i_max
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
   ix1 = x_axis%i_max
   iy1 = y_axis%i_max
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
   ix2 = x_axis%i_max
   iy2 = y_axis%i_min
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
   ix1 = x_axis%i_min
   iy1 = y_axis%i_min
   Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
!
! Add meridians and parallels, and latitude/longitude labels
   Call gfont_size (font3)
   Call set_line_style$ (ps_dot)
! - meridians -
   iy1 = y_axis%i_min
   iy2 = y_axis%i_max
   Call get_text_size$ ('0', isw, isd)
   iy3 = y_axis%i_min + 1.2*isd
   x = rlgw - Mod(rlgw,x_axis%tick1)
   ix3 = -9999
   nskip = 0
   iskip = 0
   Do
      If (.not.x < rrlge) Exit
      If (x > rlgw) Then
         ix1 = ix_pixel(x)
         ix2 = ix1
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
         If (x < r180) Then
            clab = make_coor('lon', x)
         Else
            clab = make_coor('lon', x-r360)
         End If
         Call get_text_size$ (Trim(clab), isw, isd)
         ix1 = ix1 - isw/2
         If (ix1 > ix3) Then
            If (iskip == nskip) Then
               Call draw_characters$ (Trim(clab), ix1, iy3, icol(1))
               ix3 = ix1 + isw
               iskip = 0
            Else
               iskip = iskip + 1
            End If
         Else
            If (iskip >= nskip) nskip = nskip + 1
            iskip = iskip + 1
         End If
      End If
      x = x + x_axis%tick1
   End Do
   iyb = iy3
! - parallels -
   ix1 = x_axis%i_min
   ix2 = x_axis%i_max
   Call get_text_size$ ('0', isw, isd)
   ix4 = x_axis%i_min - 0.2*isd
   y = rlts - Mod(rlts, y_axis%tick1)
   ixl = x_axis%i_min
   iy3 = ihuge
   nskip = 0
   iskip = 0
   Do
      If (.not.y < rltn) Exit
      If (y > rlts) Then
         iy1 = iy_pixel(y)
         iy2 = iy1
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
         clab = make_coor('lat', y)
         Call get_text_size$ (Trim(clab), isw, isd)
         ix3 = ix4 - isw
         iy1 = iy1 + isd/2
         If (iy1 < iy3) Then
            If (iskip == nskip) Then
               Call draw_characters$ (Trim(clab), ix3, iy1, icol(1))
               iy3 = iy1 - isd
               iskip = 0
            Else
               iskip = iskip + 1
            End If
         Else
            If (iskip >= nskip) nskip = nskip + 1
            iskip = iskip + 1
         End If
         ixl = Min(ixl, ix3)
      End If
      y = y + y_axis%tick1
   End Do
   Call set_line_style$ (ps_solid)
!
! Add label-bar
   Select Case (ncb)
    Case (0)
      Continue
    Case (1)
      Call label_bar (lbd, x_axis%i_min, x_axis%i_max, y_axis%i_min, y_axis%i_max, img%ncs, indx0, img%contour, ixl, iyb)
    Case (3)
      Select Case (lbd)
       Case ('H', 'h')
         Call label_bar (lbd,                            x_axis%i_min,     (3*x_axis%i_min + 2*x_axis%i_max)/5, &
                                                         y_axis%i_min,                            y_axis%i_max, &
              img%ncs/2 - 2,                 indx0,          -img%contour(1:img%ncs/2-2), ixl, iyb, &
              ctxt=Trim(ls_cats(1)%c))
         Call label_bar (lbd, (59*x_axis%i_min + 41*x_axis%i_max)/100, (59*x_axis%i_max + 41*x_axis%i_min)/100, &
                                                         y_axis%i_min,                            y_axis%i_max, &
                          3, indx0 + img%ncs/2 - 1,   img%contour(img%ncs/2:img%ncs/2+2), ixl, iyb, &
              ctxt=Trim(ls_cats(2)%c))
         Call label_bar (lbd,     (3*x_axis%i_max + 2*x_axis%i_min)/5,                            x_axis%i_max, &
                                                         y_axis%i_min,                            y_axis%i_max, &
              img%ncs/2 - 2, indx0 + img%ncs/2 + 3,     img%contour(img%ncs/2+4:) - oneh, ixl, iyb, &
              ctxt=Trim(ls_cats(3)%c))
       Case ('V', 'v')
         Call label_bar (lbd,                            x_axis%i_min,                            x_axis%i_max, &
                                                         y_axis%i_min,       (3*y_axis%i_min+2*y_axis%i_max)/5, &
              img%ncs/2 - 2,                 indx0,          -img%contour(1:img%ncs/2-2), ixl, iyb, &
              ctxt=Trim(ls_cats(1)%c))
         Call label_bar (lbd,                            x_axis%i_min,                            x_axis%i_max, &
                              (59*y_axis%i_min + 41*y_axis%i_max)/100, (59*y_axis%i_max + 41*y_axis%i_min)/100, &
                          3, indx0 + img%ncs/2 - 1,   img%contour(img%ncs/2:img%ncs/2+2), ixl, iyb, &
              ctxt=Trim(ls_cats(2)%c))
         Call label_bar (lbd,                            x_axis%i_min,                            x_axis%i_max, &
                                    (3*y_axis%i_max+2*y_axis%i_min)/5,                            y_axis%i_max, &
              img%ncs/2 - 2, indx0 + img%ncs/2 + 3,     img%contour(img%ncs/2+4:) - oneh, ixl, iyb, &
              ctxt=Trim(ls_cats(3)%c))
      End Select
   End Select
!
! Add title
   Call add_title (Trim(img%title))
!
   Return
!
 Contains
!
!
  Function gridsp (rltn, rlts, rlgw, rlge)
!
! Defines an appropriate grid spacing
!
! Function result
   Real(Kind=rp) :: gridsp
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rltn ! - north map limit -
   Real(Kind=rp), Intent(In) :: rlts ! - south map limit -
   Real(Kind=rp), Intent(In) :: rlgw ! - west map limits -
   Real(Kind=rp), Intent(In) :: rlge ! - east map limits -
!
! Locals
!
! Local parameters
   Integer, Parameter :: ns = 6 ! - number of grid-spacing options -
!
   Real(Kind=rp), Dimension(ns), Parameter :: gss = & ! - grid-spacing options -
      [   2.0_rp,   5.0_rp,  10.0_rp,  15.0_rp,  30.0_rp,  45.0_rp]
   Real(Kind=rp), Dimension(ns), Parameter :: gsl = & ! - grid-spacing limits -
      [  10.0_rp,  20.0_rp,  40.0_rp,  70.0_rp,  90.0_rp, 120.0_rp]
!
! Local scalars
   Integer :: i ! - indices -
!
   Real(Kind=rp) :: rmap ! - largest map extent -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max
!
! Executable Statements
!
! Identify largest map extent
   If (rlgw < rlge) Then
      rmap = Max(rltn-rlts, rlge-rlgw)
   Else
      rmap = Max(rltn-rlts, r360-(rlgw-rlge))
   End If
!
! Identify appropriate spacing
   gridsp = gss(1)
   Do i = 1, ns
      If (rmap >= gsl(i)) Then
         gridsp = gss(i)
      Else
         Exit
      End If
   End Do
!
   Return
  End Function gridsp
!
!
!
  Function make_coor(latlng, r)
!
! Constructs coordinate label
!
! Modules
   Use data_numbers, Only: zero, r180, r360
   Use maths,        Only: get_cnumber
!
! Function result
   Character(Len=4) :: make_coor
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: r ! - latitude / longitude -
!
   Character(Len=3), Intent(In) :: latlng ! - latitude / longitude flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Construct coordinate label
! - latitudes -
   Select Case (latlng)
    Case ('lat')
      If (r > zero) Then
         make_coor = Trim(get_cnumber(r, 0))//'N'
      Else If (r < zero) Then
         make_coor = Trim(get_cnumber(-r, 0))//'S'
      Else
         make_coor= '0'
      End If
! - longitudes -
    Case ('lon')
      If (r > zero) Then
         If (r < r180) Then
            make_coor = Trim(get_cnumber(r, 0))//'E'
         Else If (r > r180) Then
            make_coor = Trim(get_cnumber(r-r360, 0))//'W'
         Else
            make_coor = '180'
         End If
      Else If (r < zero) Then
         If (r > -r180) Then
            make_coor = Trim(get_cnumber(-r, 0))//'W'
         Else
            make_coor = '180'
         End If
      Else
         make_coor = '0'
      End If
   End Select
!
   Return
  End Function make_coor
 End Subroutine draw_map
!
!
!
 Subroutine init_map (img, rltn, rlts, rlgw, rlge)
!
! Initialises a map
!
! Modules
   Use data_numbers, Only: zero, half, r360
   Use screen,       Only: init_view
   Use axes,         Only: x_axis, y_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rltn ! - northern area limit -
   Real(Kind=rp), Intent(In) :: rlts ! - southern area limit -
   Real(Kind=rp), Intent(In) :: rlgw ! - western area limit -
   Real(Kind=rp), Intent(In) :: rlge ! - eastern area limit -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - map -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: d    ! - half latitudinal/longitudinal map dimension disparity -
!
! Executable Statements
!
! Define map limits
! - correct if western limit is east of eastern limit -
   If (rlge > rlgw) Then
      rrlge = rlge
   Else
      rrlge = rlge + r360
   End If
!
! Maintain perspective ratio
   d = half*((rrlge - rlgw) - (rltn - rlts))
   If (d > zero) Then
      lbd ='h'
      x_axis%amin = rlgw
      x_axis%amax = rrlge
      y_axis%amin = rlts - d
      y_axis%amax = rltn + d
   Else If (d < zero) Then
      lbd = 'v'
      x_axis%amin = rlgw + d
      x_axis%amax = rrlge - d
      y_axis%amin = rlts
      y_axis%amax = rltn
   Else
      lbd = 'h'
      x_axis%amin = rlgw
      x_axis%amax = rrlge
      y_axis%amin = rlts
      y_axis%amax = rltn
   End If
!
! Initialise
   Select Case (lbd)
    Case ('H', 'h')
      Call init_graphic (img, 0.15_rp, 0.05_rp, 0.08_rp, 0.15_rp)
    Case ('V', 'v')
      Call init_graphic (img, 0.22_rp, 0.05_rp, 0.08_rp, 0.10_rp)
   End Select
   Call init_view (x_axis%amin, x_axis%amax, y_axis%amin, y_axis%amax)
!
   Return
 End Subroutine init_map
!
!
!
 Subroutine label_bar (lbd, ix_min, ix_max, iy_min, iy_max, ncs, indx0, contour, ixl, iyb, &
            ctxt)
!
! Draws a label bar
!
! Modules
   Use clrwin$,      Only: draw_characters$, draw_filled_rectangle$, draw_line_between$, get_text_size$, rotate_font$
   Use data_numbers, Only: zero, one, ten, r90
   Use maths,        Only: iprec, &
                           magnitude
   Use gui,          Only: icol
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix_min ! - minimum X plotting position -
   Integer, Intent(In) :: ix_max ! - maximum X plotting position -
   Integer, Intent(In) :: iy_min ! - minimum Y plotting position -
   Integer, Intent(In) :: iy_max ! - maximum Y plotting position -
   Integer, Intent(In) :: ncs    ! - number of contours -
   Integer, Intent(In) :: indx0  ! - colour index offset -
   Integer, Intent(In) :: ixl    ! - lefthand limit of map and labels -
   Integer, Intent(In) :: iyb    ! - bottom limit of map and labels -
!
   Character(Len=1), Intent(In) :: lbd ! - label-bar direction -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: contour ! - contour intervals -
!
! Optional arguments
! - optional input scalars
   Character(Len=*), Intent(In), Optional :: ctxt ! - label-bar text -
!
! Locals
!
! Local parameters
   Integer, Parameter :: mprec = 3 ! - maximum precision required -
   Integer, Parameter :: miom = 4  ! - maximum order of magnitude -
!
! Local scalars
   Integer :: k                  ! - current contour -
   Integer :: nprec              ! - precision -
   Integer :: ixw                ! - longitudinal plotting width -
   Integer :: iyw                ! - latitudinal plotting width -
   Integer :: ix1, ix2, ix3, ix4 ! - X plotting positions -
   Integer :: iy1, iy2, iy3      ! - Y plotting positions -
   Integer :: isw, isd           ! - string width and depth -
   Integer :: nbin               ! - number of bins -
   Integer :: nskip              ! - number of contour labels to skip -
   Integer :: iskip              ! - number of contour labels skipped -
   Integer :: iom                ! - order of magnitude -
   Integer :: ioma               ! - order of magnitude -
!
   Real(Kind=rp) :: rscale ! - contour rescaling -
!
   Character(Len= 8) :: cfmt   ! - format statement -
   Character(Len= 8) :: clab   ! - contour label -
   Character(Len=15) :: cscale ! - scaling label -
!
! Local arrays
   Real(Kind=rp), Dimension(ncs) :: ci ! - contour intervals -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, AdjustL, Max, Nint, Present, Trim
!
! Executable Statements
!
! Determine positioning
   Call get_text_size$ ('0', isw, isd)
   nbin = ncs + 1
!
! Determine scaling
   iom = magnitude(contour(ncs))
   ioma = magnitude(contour(1))
   If (Abs(ioma) > Abs(iom)) iom = ioma
   rscale = one
1  If (iom > 0) Then
      If (iom > miom) Then
         Write(cscale, Fmt='(A,I0,A)') '(x 10^', iom-1, ')'
         rscale = ten**(iom-1)
      End If
   Else If (iom < 0) Then
      If (iom < -miom) Then
         Write(cscale, Fmt='(A,I0,A)') '(/ 10^', -iom-1, ')'
         rscale = ten**(iom-1)
      End If
   End If
!
! Rescale contours
   ci(:) = contour(1:ncs)/rscale
!
! Identify precision
   If (ncs > 1) Then
      nprec = Max(iprec(ci(1), mprec), iprec(ci(2), mprec))
      If (nprec == 0) Then
         If (iom < 0 .and. rscale == one) Then
            nprec = Max(iprec(ci(1), mprec+miom), iprec(ci(2), mprec+miom))
            iom = -nprec
            GoTo 1
         End If
      End If
   Else
      nprec = iprec(ci(1), mprec)
   End If
   iom = Abs(iom)
!
! Draw horizontal labelbar
   ixw = ix_max - ix_min
   iyw = iy_min - iy_max
   Select Case (lbd)
    Case ('h')
      iy1 = iyb + 0.75*isd
      iy2 = iyb + 2.00*isd
      iy3 = iyb + 3.20*isd
      ix1 = ix_min
      ix3 = -9999
      nskip = 0
      iskip = 0
      Do k = 1, ncs
         ix2 = ix_min + ixw*k/nbin
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(indx0+k))
         Call draw_line_between$ (ix1,iy1,ix1,iy2, icol(1))
         Select Case (nprec)
          Case (0)
            Write (clab, Fmt='(I0)') Nint(ci(k))
          Case (1:)
            Write (cfmt, Fmt='(A,I1,A)') '(F0.', nprec, ')'
            Write (clab, Fmt=cfmt) ci(k)
         End Select
         clab = AdjustL(clab)
         Call get_text_size$ (Trim(clab), isw, isd)
         ix1 = ix2 - isw/2
         If (ix1 > ix3+2) Then
            If (iskip == nskip) Then
               Call draw_characters$ (Trim(clab), ix1, iy3, icol(1))
               ix3 = ix1 + isw
               iskip = 0
            Else
               iskip = iskip + 1
            End If
         Else
            If (iskip >= nskip) nskip = nskip + 1
            iskip = iskip + 1
         End If
         ix1 = ix2
      End Do
      ix2 = ix_max
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(indx0+nbin))
      Call draw_line_between$ (ix1, iy1, ix1, iy2, icol(1))
      Call draw_line_between$ (ix2, iy1, ix2, iy2, icol(1))
      ix1 = ix_min
      Call draw_line_between$ (ix1, iy1, ix2, iy1, icol(1))
      Call draw_line_between$ (ix1, iy2, ix2, iy2, icol(1))
! - indicate contour scaling -
      If (Abs(iom) > miom) Then
         Call get_text_size$ (Trim(cscale)//'  ', isw, isd)
         ix1 = ix_min - isw
         Call draw_characters$ (Trim(cscale), ix1, iy3, icol(1))
      End If
! - add text -
      If (Present(ctxt)) Then
         Call get_text_size$ (ctxt, isw, isd)
         ix1 = (ix_max + ix_min)/2
         Call draw_characters$ (ctxt, ix1-isw/2, iy3+isd, icol(1))
      End If
!
! Draw vertical labelbar
    Case ('v')
      ix2 = ixl - 0.75*isd
      ix1 = ix2 - 1.25*isd
      ix3 = ix1 - 0.20*isd
      iy1 = iy_min
      Do k = 1, ncs
         iy2 = iy_min - iyw*k/nbin
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(indx0+k))
         Call draw_line_between$ (ix1, iy1, ix2, iy1, icol(1))
         Select Case (nprec)
          Case (0)
            Write (clab, Fmt='(I7)') Nint(ci(k))
          Case (1:)
            Write (cfmt, Fmt='(A,I1,A)') '(F7.', nprec, ')'
            Write (clab, Fmt=cfmt) ci(k)
         End Select
         Call get_text_size$ (clab, isw, isd)
         ix4 = ix3 - isw
         iy1 = iy2 + isd/2
         Call draw_characters$ (clab, ix4, iy1, icol(1))
         iy1 = iy2
      End Do
      Call draw_line_between$ (ix1, iy1, ix2, iy1, icol(1))
      iy2 = iy_max
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(indx0+nbin))
      Call draw_line_between$ (ix1, iy1,ix2, iy1, icol(1))
      Call draw_line_between$ (ix1, iy2, ix2, iy2, icol(1))
      iy1 = iy_min
      Call draw_line_between$ (ix1, iy1, ix1, iy2, icol(1))
      Call draw_line_between$ (ix2, iy1, ix2, iy2, icol(1))
! - indicate contour scaling -
      If (Abs(iom) > miom) Then
         Call get_text_size$ (Trim(cscale)//'  ', isw, isd)
         ix1 = ix2 - isw
         iy3 = iy_min + 1.2*isd
         Call draw_characters$ (Trim(cscale), ix1, iy3, icol(1))
      End If
! - add text -
      If (Present(ctxt)) Then
         Call rotate_font$ (r90)
         Call get_text_size$ (ctxt, isw, isd)
         iy1 = (iy_max + iy_min)/2
         Call draw_characters$ (ctxt, ix3-isd, iy1+isw/2, icol(1))
         Call rotate_font$ (zero)
      End If
   End Select
   Call draw_characters$ (' ', 1, 1, icol(0))
!
   Return
 End Subroutine label_bar
!
!
!
 Subroutine draw_graph_base (title, x_axis, y_axis, ivert, &
            nse, ntx, tx, nty, ty)
!
! Draws a blank graph
!
! Modules
   Use mswinprm$, Only: ps_dashdot, ps_solid
   Use clrwin$,   Only: draw_filled_rectangle$, draw_line_between$, set_line_style$, set_line_width$
   Use time,      Only: pdate
   Use screen,    Only: iy_font, &
                        ix_pixel, iy_pixel
   Use gui,       Only: icol, &
                        get_cols
   Use axes,      Only: axis, &
                        inc_date_tick_marks, draw_xaxis, draw_yaxis, init_tick_marks
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ivert ! - vertical lines on graph flag -
!
   Character(Len=*), Intent(In) :: title ! - graph title -
!
   Type(axis), Intent(In) :: x_axis ! - x-axis -
   Type(axis), Intent(In) :: y_axis ! - y-axis -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nse ! - number of seasons -
   Integer, Intent(In), Optional :: ntx ! - number of X-thresholds -
   Integer, Intent(In), Optional :: nty ! - number of Y-thresholds -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tx ! - X-thresholds -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: ty ! - Y-thresholds -
!
! Locals
!
! Local scalars
   Integer :: i        ! - threshold index -
   Integer :: iinc     ! - increment -
   Integer :: ix1, ix2 ! - X plotting positions -
   Integer :: iy1, iy2 ! - Y plotting positions -
!
   Real(Kind=rp) :: xinc ! - increment -
   Real(Kind=rp) :: x    ! - current position on x-axis -
!
   Logical :: ldaily ! - reset to daily intervals? -
   Logical :: lmon   ! - monthly intervals? -
!
   Type(pdate) :: adate ! - current date -
   Type(pdate) :: bdate ! - current offset date -
   Type(pdate) :: ddate ! - current dekadal offset date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Min, Present
!
! Executable Statements
!
! Add category shading
   i = get_cols(0)
   If (Present(tx) .and. (itsh == 1)) Then
      Call set_line_width$ (0)
      iy1 = y_axis%i_min
      iy2 = y_axis%i_max
! - below -
      ix1 = x_axis%i_min
      ix2 = Min(Max(ix_pixel(tx(1)*x_axis%scale), x_axis%i_min), x_axis%i_max)
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(11))
! - normal -
      If (ntx == 2) Then
         ix1 = ix2
         ix2 = Min(Max(ix_pixel(tx(2)*x_axis%scale), x_axis%i_min), x_axis%i_max)
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(12))
      End If
! - above -
      ix1 = x_axis%i_max
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(13))
      Call set_line_width$ (1)
   End If
!
! Add category shading
   If (Present(ty) .and. (itsh == 1)) Then
      Call set_line_width$ (0)
      ix1 = x_axis%i_min
      ix2 = x_axis%i_max
! - below -
      iy1 = y_axis%i_min
      iy2 = Max(Min(iy_pixel(ty(1)*y_axis%scale), y_axis%i_min), y_axis%i_max)
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(11))
! - normal -
      If (nty == 2) Then
         iy1 = iy2
         iy2 = Max(Min(iy_pixel(ty(2)*y_axis%scale), y_axis%i_min), y_axis%i_max)
         Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(12))
      End If
! - above -
      iy1 = y_axis%i_max
      Call draw_filled_rectangle$ (ix1, iy1, ix2, iy2, icol(13))
      Call set_line_width$ (1)
   End If
!
! Draw axes
! - x axis -
   Call draw_xaxis (x_axis, 1, .true., iy_font, &
        inum=1, iano=1, nse=nse)
! - y axis -
   Call draw_yaxis (y_axis, -1, .true., iy_font, &
        inum=-1, iano=-1)
!
! Add categories
   If (Present(tx)) Then
      Call set_line_style$ (ps_dashdot)
      iy1 = y_axis%i_min
      iy2 = y_axis%i_max
      Do i = 1, ntx
         ix1 = ix_pixel(tx(i)*x_axis%scale)
         If (ix1 > x_axis%i_min .and. ix1 < x_axis%i_max) Then
            ix2 = ix1
            Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
         End If
      End Do
      Call set_line_style$ (ps_solid)
   End If
!
! Add categories
   If (Present(ty)) Then
      Call set_line_style$ (ps_dashdot)
      ix1 = x_axis%i_min
      ix2 = x_axis%i_max
      Do i = 1, nty
         iy1 = iy_pixel(ty(i)*y_axis%scale)
         If (iy1 < y_axis%i_min .and. iy1 > y_axis%i_max) Then
            iy2 = iy1
            Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
         End If
      End Do
      Call set_line_style$ (ps_solid)
   End If
!
! Add vertical year lines
   If (ivert == 1 .and. x_axis%amax > x_axis%tick1) Then
      Call set_line_style$ (ps_dashdot)
      iy1 = y_axis%i_min
      iy2 = y_axis%i_max
      xinc = x_axis%tick1
      Call init_tick_marks (x_axis, xinc, x, iinc, lmon, ldaily, adate, bdate, ddate)
      Do
         x = x + xinc
         If (x < x_axis%amin) Cycle
         If (x > x_axis%amax) Exit
         ix1 = ix_pixel(x)
         ix2 = ix1
         Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(1))
! - increment dates -
         If (x_axis%ldate) Call inc_date_tick_marks (lmon, ldaily, adate, bdate, ddate, iinc, xinc)
      End Do
      Call set_line_style$ (ps_solid)
   End If
!
! Add title
   Call add_title (title)
!
   Return
 End Subroutine draw_graph_base
!
!
!
 Subroutine add_line (laxis, n, y, iln, ilw, &
            ixf, df, kuse)
!
! Adds a line to a graph
!
! Modules
   Use clrwin$, Only: draw_line_between$, set_line_width$
   Use time,    Only: pdate, &
                      Operator(+), Operator(<), Operator(<=), &
                      lslim, ssnlims
   Use screen,  Only: iy_pixel
   Use gui,     Only: icol
   Use axes,    Only: y_axis
   Use markers, Only: draw_cross
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of points -
   Integer, Intent(In) :: iln ! - line number -
   Integer, Intent(In) :: ilw ! - line width -
!
   Logical, Intent(In) :: laxis ! - logarithmic axis? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - series -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ixf ! - first value on x-axis -
!
   Type(pdate), Intent(In), Optional :: df ! - first date on x-axis -
!
! - optional input arrays -
   Logical, Dimension(:), Intent(In), Optional :: kuse ! - cases used? -
!
! Locals
!
! Local scalars
   Integer :: ik       ! - time-sequence multiplier -
   Integer :: k        ! - case index -
   Integer :: kk       ! - available case index -
   Integer :: ix       ! - X-axis value -
   Integer :: ix1, ix2 ! - X plotting positions -
   Integer :: iy1, iy2 ! - Y plotting positions -
   Integer :: nmax     ! - maximum number of points to plot -
!
   Logical :: lfirst ! - first point? -
   Logical :: lsend  ! - end of season? -
   Logical :: lsl    ! - use season limits? -
!
   Type(pdate) :: d0 ! - current season limit -
   Type(pdate) :: d1 ! - previous available date -
   Type(pdate) :: d2 ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log10, Present
!
! Executable Statements
!
! Check for valid data range on log axis
   If (laxis) Then
      If (Log10(y(n)*y_axis%scale) > y_axis%amin) Then
         nmax = n
      Else
         nmax = n - 1
      End If
   Else
      nmax = n
   End If
!
! Identify origin
   Call set_origin (ix, kk, ik, &
        ixf=ixf, df=df)
!
! Plot data
   lfirst = .true.
   lsend = .false.
   lsl = (lslim .and. Present(df))
   Call set_line_width$ (ilw)
   Do k = 1, nmax
      If (next_point(k, ik, ix, kk, ix2, kuse=kuse) /= 0) Cycle
      If (laxis) Then
         iy2 = iy_pixel(Log10(y(kk)*y_axis%scale))
      Else
         iy2 = iy_pixel(y(kk)*y_axis%scale)
      End If
! - do not connect for first available case -
      If (lfirst) Then
         lfirst = .false.
         If (lsl) d1 = df + (k - 1)
      Else
! - identify whether there is a season limit between the current and the previous available case -
         If (lsl) Then
            d2 = df + (k - 1)
            d0 = ssnlims%sdate
            d0%iyr = d1%iyr
            If (d0 <= d1) Then
               If (ssnlims%sdate%imn > ssnlims%edate%imn .or. & ! - season limit spans year-end -
                  (ssnlims%sdate%imn == ssnlims%edate%imn .and. ssnlims%sdate%idy > ssnlims%edate%idy)) d0%iyr = d0%iyr + 1
            Else
               If (ssnlims%sdate%imn < ssnlims%edate%imn .or. & ! - season limit does not span year-end -
                  (ssnlims%sdate%imn == ssnlims%edate%imn .and. ssnlims%sdate%idy < ssnlims%edate%idy)) d0%iyr = d0%iyr - 1
            End If
            d0%imn = ssnlims%edate%imn
            d0%idy = ssnlims%edate%idy
            lsend = (d0 < d2)
         End If
! - connect only if previous case is non-missing and there is no intervening season limit -
         If (Present(kuse)) Then
            If (kuse(k-1)) Then
               If (.not.lsend) Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(iln+1))
               If (lsl) d1 = d2
            End If
! - connect points -
         Else
            Call draw_line_between$ (ix1, iy1, ix2, iy2, icol(iln+1))
            If (lsl) d1 = d2
         End If
      End If
      If (icross == 1) Then
         Call draw_cross (ix2, iy2, 1, icol(iln+1))
         Call set_line_width$ (ilw)
      End If
      ix1 = ix2
      iy1 = iy2
   End Do
   Call set_line_width$ (1)
!
   Return
 End Subroutine add_line
!
!
!
 Subroutine add_crosses (n, m, y, icc, ilw, &
            ixf, df, kuse)
!
! Adds crosses to a graph
!
! Modules
   Use time,    Only: pdate
   Use screen,  Only: iy_pixel
   Use axes,    Only: y_axis
   Use markers, Only: draw_cross
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of points -
   Integer, Intent(In) :: m   ! - number of crosses per point -
   Integer, Intent(In) :: icc ! - cross colour -
   Integer, Intent(In) :: ilw ! - line width -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - series -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ixf ! - first value on x-axis -
!
   Type(pdate), Intent(In), Optional :: df ! - first date on x-axis -
!
! - optional input arrays -
   Logical, Dimension(:), Intent(In), Optional :: kuse ! - cases used? -
!
! Locals
!
! Local scalars
   Integer :: i   ! - cross index -
   Integer :: ik  ! - increment -
   Integer :: k   ! - case index -
   Integer :: kk  ! - available case index -
   Integer :: ix  ! - X-axis value -
   Integer :: ix1 ! - X plotting position -
   Integer :: iy1 ! - Y plotting position -
!
! Executable Statements
!
! Identify origin
   Call set_origin (ix, kk, ik, &
        ixf=ixf, df=df)
!
! Plot crosses
   Do k = 1, n
      If (next_point(k, ik, ix, kk, ix1, kuse=kuse) /= 0) Cycle
      Do i = 1, m
         iy1 = iy_pixel(y(kk,i)*y_axis%scale)
         Call draw_cross (ix1, iy1, ilw, icc)
      End Do
   End Do
!
   Return
 End Subroutine add_crosses
!
!
!
 Subroutine add_errors (n, errs, icol, ilw, ixd, &
            ixf, df, kuse)
!
! Adds error bars to a graph
!
! Modules
   Use clrwin$, Only: draw_line_between$, set_line_width$
   Use maths,   Only: interval
   Use time,    Only: pdate
   Use screen,  Only: iy_pixel
   Use axes,    Only: y_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n    ! - number of points -
   Integer, Intent(In) :: icol ! - line colour -
   Integer, Intent(In) :: ilw  ! - line width -
   Integer, Intent(In) :: ixd  ! - horizontal marker dimension -
!
! - input arrays -
   Type(interval), Dimension(:), Intent(In) :: errs ! - errors -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ixf ! - first value on x-axis -
!
   Type(pdate), Intent(In), Optional :: df ! - first date on x-axis -
!
! - optional input arrays -
   Logical, Dimension(:), Intent(In), Optional :: kuse ! - cases used? -
!
! Locals
!
! Local scalars
   Integer :: ik  ! - increment -
   Integer :: k   ! - case index -
   Integer :: kk  ! - available case index -
   Integer :: ix  ! - X-axis value -
   Integer :: ix1 ! - X plotting position -
   Integer :: iy1 ! - lower error limit Y plotting position -
   Integer :: iy2 ! - upper error limit Y plotting position -
   Integer :: ixh ! - half horizontal marker dimension -
!
! Executable Statements
!
! Identify origin
   Call set_origin (ix, kk, ik, &
        ixf=ixf, df=df)
!
! Set width of error bar end-points
   ixh = ixd/2
!
! Plot error bars
   Call set_line_width$ (ilw)
   Do k = 1, n
      If (next_point(k, ik, ix, kk, ix1, kuse=kuse) /= 0) Cycle
      iy1 = iy_pixel(errs(k)%lower*y_axis%scale)
      iy2 = iy_pixel(errs(k)%upper*y_axis%scale)
      Call draw_line_between$ (    ix1, iy1,     ix1, iy2, icol)
      Call draw_line_between$ (ix1-ixh, iy1, ix1+ixh, iy1, icol)
      Call draw_line_between$ (ix1-ixh, iy2, ix1+ixh, iy2, icol)
   End Do
   Call set_line_width$ (1)
!
   Return
 End Subroutine add_errors
!
!
!
 Subroutine set_origin (ix, kk, ik, &
            ixf, df)
!
! Initialises plotting at the origin
!
! Modules
   Use data_time_constants, Only: isq_dy, isq_yr
   Use time,                Only: pdate, &
                                  iseq, lslim, &
                                  pdate_to_idate
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ix ! - starting value -
   Integer, Intent(Out) :: kk ! - number of available cases -
   Integer, Intent(Out) :: ik ! - increment -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ixf ! - first value on x-axis -
!
   Type(pdate), Intent(In), Optional :: df ! - first date on x-axis -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Identify origin
   kk = 0
   ik = 1
   If (Present(ixf)) Then
      ix = ixf - 1
   Else If (Present(df)) Then
      Select Case (iseq)
       Case (isq_yr)
         ix = df%iyr - 1
       Case Default
         If (lslim .or. iseq <= 1 .or. iseq == 10) Then
            ix = pdate_to_idate(iseq, df) - 1
         Else ! - adjust to daily -
            ix = pdate_to_idate(isq_dy, df) - 1
            ik = iseq
         End If
      End Select
   Else
      ix = 0
   End If
!
   Return
 End Subroutine set_origin
!
!
!
 Function next_point(k, ik, ix, kk, ixp, &
          kuse)
!
! Advances to next point for plotting
!
! Modules
   Use screen, Only: ix_pixel
!
! Function result
   Integer :: next_point
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) ::  k ! - current case number -
   Integer, Intent(In) :: ik ! - increment -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ix  ! - current x-axis value -
   Integer, Intent(InOut) :: kk  ! - available case index -
   Integer, Intent(InOut) :: ixp ! - current x-axis location -
!
! Optional arguments
! - optional input arrays -
   Logical, Dimension(:), Intent(In), Optional :: kuse ! - cases used? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Present
!
! Executable Statements
!
! Increment X-value
   ix = ix + ik
!
! Check whether next point is available
   If (Present(kuse)) Then
      If (.not.kuse(k)) Then
         next_point = 1
         Return
      End If
   End If
!
! Advance to next point for plotting
   kk = kk + 1
   ixp = ix_pixel(Real(ix, Kind=rp))
   next_point = 0
!
   Return
 End Function next_point
!
!
!
 Subroutine add_title (ctitle)
!
! Adds a title
!
! Modules
   Use clrwin$,Only: bold_font$, draw_characters$, get_text_size$
   Use screen, Only: font1, font2, &
                     gfont_size
   Use gui,    Only: icol
   Use axes,   Only: x_axis, y_axis
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctitle ! - title -
!
! Locals
!
! Local scalars
   Integer :: isw ! - title width -
   Integer :: isd ! - title depth -
   Integer :: ix1 ! - X plotting position -
   Integer :: iy1 ! - Y plotting position -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max
!
! Executable Statements
!
! Set font
   Call gfont_size (font2)
   Call bold_font$ (1)
!
! Determine title size and position
   Call get_text_size$ (ctitle, isw, isd)
   ix1 = (x_axis%i_min + x_axis%i_max - isw)/2
   iy1 = Max((y_axis%i_max+isd)/2, y_axis%i_max-isd)
!
! Add title
   Call draw_characters$ (ctitle, ix1, iy1, icol(1))
!
! Reset font
   Call bold_font$ (0)
   Call gfont_size (font1)
!
   Return
 End Subroutine add_title
!
!
!
 Subroutine contours (img, &
            zmin, zmax, zlow, zhgh)
!
! Defines contour intervals
!
! Modules
   Use data_gui_constants, Only: mnnc, mxnc
   Use maths,              Only: calc_limits, get_intervals
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: zmin ! - absolute minimum value -
   Real(Kind=rp), Intent(In), Optional :: zmax ! - absolute maximum value -
   Real(Kind=rp), Intent(In), Optional :: zlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: zhgh ! - highest value -
!
! Locals
!
! Local scalars
   Integer :: i ! - contour index -
!
   Real(Kind=rp) :: zint ! - contour interval -
   Real(Kind=rp) :: zmn  ! - minimum -
   Real(Kind=rp) :: zmx  ! - maximum -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Present
!
! Executable Statements
!
! Define contour intervals
   If (Present(zmin) .and. Present(zmax)) Then
      Call get_intervals (zmin, zmax, mnnc+1, mxnc+1+1, zint)
      zmn = zmin*img%scale
      zmx = zmax*img%scale
      zint = zint*img%scale
   Else
      Call calc_limits (zlow, zhgh, mnnc+1, mxnc+1, zmn, zmx, img%scale, zint)
   End If
!
! Define contour levels
   img%ncs = Nint((zmx-zmn)/zint) - 1
   img%contour(1) = zmn + zint
   Do i = 2, img%ncs
      img%contour(i) = img%contour(i-1) + zint
   End Do
   img%ymin = zmn
   img%ymax = zmx
!
! Rescale contours
   img%contour(1:img%ncs) = img%contour(1:img%ncs)/img%scale
!
   Return
 End Subroutine contours
!
!
!
 Subroutine init_image (img, isize)
!
! Initialises an image
!
! Modules
   Use data_numbers, Only: zero, one
   Use gui,          Only: get_handle
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isize ! - graphics area dimension -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Executable Statements
!
! Image identifier
   img%id = get_handle()
!
! Image handle
   img%ihandle = 0
!
! Initialise graph size
   img%isize = isize*gszs
!
! Number of contours
   img%ncs = 1
!
! Initialise axes and scaling
   img%ymin = zero
   img%ymax = zero
   img%scale = one
!
! Title
   img%title = ' '
!
! Preset contour calculations
   img%lcontour = .false.
!
! Preset contour calculations
   img%contour(:) = zero
!
   Return
 End Subroutine init_image
!
!
!
 Subroutine init_graphic (img, x1, x2, y1, y2)
!
! Initialises a graphic
!
! Modules
   Use screen, Only: margins, &
                     set_gfonts
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: x1 ! - left margin -
   Real(Kind=rp), Intent(In) :: x2 ! - right margin -
   Real(Kind=rp), Intent(In) :: y1 ! - top margin -
   Real(Kind=rp), Intent(In) :: y2 ! - bottom margin -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Executable Statements
!
! Initialise viewport
   Call margins (x1, x2, y1, y2, img%isize)
!
! Initialise fonts
   Call set_gfonts ()
!
   Return
 End Subroutine init_graphic
!
!
!
 Function init_location(img) &
          Result (init)
!
! Initialises location settings
!
! Modules
   Use data_numbers,       Only: zero, one
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_gui_constants, Only: dbottom, dleft, dright, dtop
   Use screen,             Only: init_view
   Use iofiles,            Only: yfile
   Use fields,             Only: iffy, yfield
   Use axes,               Only: x_axis, &
                                 calc_xaxis_limits
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Executable Statements
!
! Reinitialise screen settings for current image
   Select Case (yfile%idstr)
    Case (id_grd, id_stn) ! - reinitialise map -
      Call init_map (img, yfield(iffy)%region%alim%rltn, yfield(iffy)%region%alim%rlts, yfield(iffy)%region%alim%rlgw, &
           yfield(iffy)%region%alim%rlge)
    Case (id_ind) ! - reinitialise bar graph -
      Call init_graphic (img, dleft, dright, dbottom, dtop)
      Call calc_xaxis_limits (x_axis, yfield(iffy)%nv, &
           ix1=0)
      Call init_view (x_axis%amin, x_axis%amax, zero, one)
   End Select
   init = 1
!
   Return
 End Function init_location
!
!
!
 Function get_location(lmouse)
!
! Determinds data coordinates from a mouse click
!
! Modules
   Use mswinprm$,          Only: mk_lbutton, mk_rbutton
   Use clrwin$,            Only: display_popup_menu$, get_mouse_info$
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use screen,             Only: pixel_ix, pixel_iy
   Use iofiles,            Only: yfile
   Use space,              Only: which_grid, which_index, which_station
   Use fields,             Only: iffy, yfield
!
! Function result
   Integer :: get_location
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical :: lmouse ! - query mouse action? -
!
! Locals
!
! Local scalars
   Integer :: ix ! - longitude of mouse click -
   Integer :: iy ! - latitude of mouse click -
   Integer :: ib ! - mouse state -
!
   Real(Kind=rp) :: xsp ! - longitude of selected point -
   Real(Kind=rp) :: ysp ! - latitude of selected point -
!
! Executable Statements
!
! Get coordinates of selected point in pixels
   get_location = 0
   Call get_mouse_info$(ix, iy, ib)
   If (.not.lmouse) ib = mk_lbutton
   If (ib == mk_lbutton) Then
      xsp = pixel_ix(ix)
      ysp = pixel_iy(iy)
!
! Convert to data coordinates
      Select Case (yfile%idstr)
       Case (id_grd) ! - gridded data -
         get_location = which_grid(xsp, ysp, iffy, yfield(iffy)%nlt, yfield(1:)%nv, yfield(iffy)%region)
       Case (id_stn) ! - gridded data -
         get_location = which_station(xsp, ysp, iffy, yfield(1:)%nv, yfield(1:)%nva, yfield(iffy)%region)
       Case (id_ind) ! - gridded data -
         get_location = which_index(xsp, iffy, yfield(1:)%nv)
      End Select
!
! Invoke pop-up menu if right-mouse button is depressed
   Else If (ib == mk_rbutton) Then
      Call display_popup_menu$ ()
   End If
!
   Return
 End Function get_location
!
!
!
 Function identify_location()
!
! Identifies station near mouse
!
! Modules
   Use screen,  Only: window_update
   Use arrays,  Only: iusey
   Use iofiles, Only: mya
   Use space,   Only: cstnc, cstndy
   Use fields,  Only: iffy
!
! Function result
   Integer :: identify_location
!
! Locals
!
! Local scalars
   Integer :: isv           ! - selected station -
   Integer, Save :: ipv = 0 ! - previous station -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Get mouse position
   isv = get_location(.false.)
   If (isv /= 0) Then
      If (Any(iusey(1:mya) == isv)) Then
         If (isv /= ipv) Then
            cstnc = cstndy(isv,iffy)
            Call window_update (cstnc)
         End If
      End If
   Else
      If (isv /= ipv) Then
         cstnc = ' '
         Call window_update (cstnc)
      End If
   End If
   ipv = isv
   identify_location = 2
!
   Return
 End Function identify_location
!
!
!
 Function select_location(img, fcb)
!
! Modules
   Use arrays,   Only: iusey
   Use iofiles,  Only: mya
   Use settings, Only: iv
   Use fields,   Only: set_iv
!
! Function result
   Integer :: select_location
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img
!
! - procedure arguments -
   Integer, External :: fcb
!
! Locals
!
! Local scalars
   Integer :: isv ! - selected variable -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Get mouse position
   select_location = init_location(img)
   isv = get_location(.true.)
   If (isv /= 0) Then
      If (Any(iusey(1:mya) == isv)) Then
         iv = isv
         select_location = set_iv()
         select_location = fcb()
      End If
   End If
   select_location = 1
!
   Return
 End Function select_location
!
!
!
 Function save_graphic(cgraph, ig, &
          plot_f, imap, mmap)
!
! Saves a graphic
!
! Modules
#if FTN95 == 1
   Use clrwin,             Only: close_svg@, open_svg@
#else
   use ISO_C_BINDING
#endif
   Use clrwin$,            Only: export_image$, select_graphics_object$, set_jpeg_quality$
   Use data_numbers,       Only: onehth, one
   Use data_gui_constants, Only: gext
   Use maths,              Only: magnitude
   Use gui,                Only: iw, jq, &
                                 add_slider_bar, box_close, box_open, boxes_open, gui_centre, gui_creturn, init_win, prompt_file, &
                                 prompt_real, set_cursor_waiting, win_prompt
   Use errors,             Only: cpt_error
   Use iofiles,            Only: check_new_file
!
! Function result
   Integer :: save_graphic
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cgraph ! - name of graph -
!
   Type(image), Intent(In) :: ig ! - graphic -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: imap ! - current map number -
   Integer, Intent(In), Optional :: mmap ! - maximum map number -
!
! - optional procedure arguments -
   Integer, External, Optional :: plot_f ! - plotting function -
!
! Locals
!
! Local scalars
   Integer :: inf  ! - new file flag -
   Integer :: ierr ! - error indicator -
   Integer :: iom  ! - order of magnitude -
!
#if FTN95 == 1
   Integer(i_handle) :: ig_handle ! - image handle -
!
#endif
   Character(Len=16) :: cfmt ! - format statement -
!
#if FTN95 == 1
   C_External open_svg1@ '__open_svg1'(instring, val, val, val):Integer*4
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Present, Trim
!
! Executable Statements
!
! Identify order of magnitude
   If (Present(imap)) Then
      If (Present(mmap)) Then
         iom = magnitude(mmap)
      Else
         iom = magnitude(imap)
      End If
!
! Create graph title
      Write (cfmt, '(A,2(I1,A))') '(A,I0.', iom, ')'
      Write (pic%cfile%fname, Fmt=cfmt) Trim(cgraph)//'_', imap
   Else
      pic%cfile%fname = Trim(cgraph)
   End If
!
! Set file extension
   pic%cfile%fext = gext(ipic)
!
! Construct default filename
   pic%cfile%ffile = Trim(pic%cfile%fdir)//Trim(pic%cfile%fname)//pic%cfile%fext
!
! Backup graphics file
   pic_old = pic
!
! Prompt for graphics filename
1  inf = 0
   Call init_win (title='Save graphic')
   CALL gui_creturn (.false.)
   Call boxes_open (1, 3, .true., .false.)
   Call box_open (.true., .false., &
        title='Graphics file')
   Call prompt_file (get_graphic, 'Graphics file', 1, pic%cfile%fname, inf)
   Call box_close (.true.)
   Call box_close (.false.)
   CALL gui_creturn (.true.)
   Call box_close (.false.)
!
! Prompt for graphic quality
   If (ipic == 1) Then
      Call box_open (.true., .false., &
           title='JPEG quality')
      Call gui_centre ()
      Call add_slider_bar (40, pic%qual, onehth, one, &
           c_min='Minimum', c_max='Maximum')
      Call gui_creturn (.true.)
      Call gui_centre ()
      Call prompt_real ('Quality', pic%qual, &
           rfly=onehth, rlow=onehth, rhgh=one, lbold=.false.)
      Call box_close (.true.)
   End If
   Call box_close (.false.)
!
! Confirm settings
   iw=win_prompt(.true.)
   If (iw == 2) Then
!
! Check for existing file if using default
      If (inf == 0) Then
         inf = check_new_file(pic%cfile%ffile)
         If (inf /= 0) GoTo 1
      End If
!
! Switch on hour-glass cursor
      Call set_cursor_waiting (1)
!
! Set JPEG quality
      If (ipic == 1) jq = set_jpeg_quality$(pic%qual)
!
! Save graphics
      ierr = select_graphics_object$(Int(ig%ihandle))
      Select Case (pic%cfile%fext)
       Case Default
         ierr = export_image$(Trim(pic%cfile%ffile))
       Case ('.svg')
#if FTN95 == 1
!        ierr = open_svg1@(pic%cfile%ffile, ig%isize, ig%isize, ig%ihandle)
         ierr = open_svg@(pic%cfile%ffile, ig%isize, ig%isize)
 lsvg = .true.
         If (Present(plot_f)) ierr = plot_f()
 lsvg = .false.
!        ierr = close_svg@(0)
 ig_handle = 0
         ierr = close_svg@(ig_handle)
#endif
      End Select
!
! Switch off hour-glass cursor
      If (ierr /= 0) Then
         save_graphic = 0
         Call cpt_error ('save_graphic', .false., save_graphic, &
              c_arg1=Trim(pic%cfile%fname)//Trim(pic%cfile%fext), c_arg2=Trim(pic%cfile%fdir))
      Else
         save_graphic = 1
         Call cpt_error ('save_graphic', .false., save_graphic)
      End If
      Call set_cursor_waiting (0)
!
! Restore graphics file
   Else
      pic = pic_old
   End If
!
   save_graphic = 1
!
   Return
 End Function save_graphic
!
!
!
 Function get_graphic()
!
! Prompts for filename for a graphic
!
! Modules
   Use labels,  Only: ls_graphfmts
   Use screen,  Only: window_update
   Use iofiles, Only: get_new_file
!
! Function result
   Integer :: get_graphic
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Local arrays
   Character(Len=16), Dimension(1) :: c_fts ! - file types -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Define output file
   c_fts(1) = Trim(ls_graphfmts(ipic)%c)//' files'
   Call get_new_file ('Graphics file', .false., pic%cfile%fext, c_fts(:), ['*'//pic%cfile%fext], 1, &
        pic%cfile%fdir, pic%cfile%ffile, pic%cfile%fname, ifail)
!
! Confirm selection
   If (ifail == 0) Then
      Call window_update (pic%cfile%fname)
   Else
      pic = pic_old
   End If
   get_graphic = 2
!
   Return
 End Function get_graphic
!
!
!
 Function export_montage(ctitle, img_1, img_2, &
          lsquare, img_3, img_4) &
          Result (export)
!
! Creates and exports a montage
!
! Modules
   Use mswinprm$, Only: srccopy
   Use clrwin$,   Only: copy_graphics_region$, create_graphics_region$, delete_graphics_region$
   Use gui,       Only: get_handle
!
! Function result
   Integer :: export
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctitle ! - title -
!
   Type(image), Intent(In) :: img_1 ! - first image -
   Type(image), Intent(In) :: img_2 ! - second image -
!
! Optional arguments
! - optional input scalars -
   Logical, Intent(In), Optional :: lsquare ! - orient in a square? -
!
   Type(image), Intent(In), Optional :: img_3 ! - third image -
   Type(image), Intent(In), Optional :: img_4 ! - fourth image -
!
! Locals
!
! Local scalars
   Type(image) :: img_montage ! - montage -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Present
!
! Executable Statements
!
! Create montage
   img_montage = img_1
   img_montage%ihandle = get_handle()
   If (Present(img_4)) Then
      export = create_graphics_region$(Int(img_montage%ihandle), img_1%isize+img_2%isize, img_1%isize+img_3%isize)
      export = copy_graphics_region$(Int(img_montage%ihandle),           0,           0, img_1%isize, img_1%isize, &
                                           Int(img_1%ihandle),           0,           0, img_1%isize, img_1%isize, srccopy)
      export = copy_graphics_region$(Int(img_montage%ihandle), img_1%isize,           0, img_2%isize, img_2%isize, &
                                           Int(img_2%ihandle),           0,           0, img_2%isize, img_2%isize, srccopy)
      export = copy_graphics_region$(Int(img_montage%ihandle),           0, img_1%isize, img_3%isize, img_3%isize, &
                                           Int(img_3%ihandle),           0,           0, img_3%isize, img_3%isize, srccopy)
      export = copy_graphics_region$(Int(img_montage%ihandle), img_3%isize, img_2%isize, img_4%isize, img_4%isize, &
                                           Int(img_4%ihandle),           0,           0, img_4%isize, img_4%isize, srccopy)
   Else If (Present(img_3)) Then
      If (lsquare) Then
         export = create_graphics_region$(Int(img_montage%ihandle), img_1%isize+img_2%isize, img_1%isize+img_3%isize)
         export = copy_graphics_region$(Int(img_montage%ihandle), img_2%isize, img_2%isize, img_1%isize, img_1%isize, &
                                              Int(img_1%ihandle),           0,           0, img_1%isize, img_1%isize, srccopy)
         export = copy_graphics_region$(Int(img_montage%ihandle),           0, img_1%isize, img_2%isize, img_2%isize, &
                                              Int(img_2%ihandle),           0,           0, img_2%isize, img_2%isize, srccopy)
         export = copy_graphics_region$(Int(img_montage%ihandle), img_2%isize,           0, img_3%isize, img_3%isize, &
                                              Int(img_3%ihandle),           0,           0, img_3%isize, img_3%isize, srccopy)
      Else
         export = create_graphics_region$(Int(img_montage%ihandle), img_1%isize+img_2%isize+img_3%isize, img_montage%isize)
         export = copy_graphics_region$(Int(img_montage%ihandle),                       0, 0, img_1%isize, img_1%isize, &
                                              Int(img_1%ihandle),                       0, 0, img_1%isize, img_1%isize, srccopy)
         export = copy_graphics_region$(Int(img_montage%ihandle),             img_1%isize, 0, img_2%isize, img_2%isize, &
                                              Int(img_2%ihandle),                       0, 0, img_2%isize, img_2%isize, srccopy)
         export = copy_graphics_region$(Int(img_montage%ihandle), img_1%isize+img_2%isize, 0, img_3%isize, img_3%isize, &
                                              Int(img_3%ihandle),                       0, 0, img_3%isize, img_3%isize, srccopy)
      End If
   Else
      export = create_graphics_region$(Int(img_montage%ihandle), img_1%isize+img_2%isize, img_montage%isize)
      export = copy_graphics_region$(Int(img_montage%ihandle),           0, 0, img_1%isize, img_1%isize, &
                                           Int(img_1%ihandle),           0, 0, img_1%isize, img_1%isize, srccopy)
      export = copy_graphics_region$(Int(img_montage%ihandle), img_1%isize, 0, img_2%isize, img_2%isize, &
                                           Int(img_2%ihandle),           0, 0, img_2%isize, img_2%isize, srccopy)
   End If
!
! Export
   export = save_graphic(ctitle, img_montage)
   export = delete_graphics_region$(Int(img_montage%ihandle))
!
   Return
 End Function export_montage
#endif
End Module graphics
