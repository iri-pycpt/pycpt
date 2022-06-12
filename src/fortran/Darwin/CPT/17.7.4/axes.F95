! Author: Simon Mason
Module axes
!
! Declarations
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: calc_xaxis_limits, draw_xaxis, draw_yaxis, get_xaxis_limits, get_yaxis_limits, inc_date_tick_marks, init_tick_marks, &
             set_axis
!
! Derived type definitions
! - axis -
   Public :: axis
   Type axis
      Sequence
      Integer :: i_min ! - minimum plotting position -
      Integer :: i_max ! - maximum plotting position -
!
      Real(Kind=rp) :: amin   ! - axis minimum -
      Real(Kind=rp) :: amax   ! - axis maximum -
      Real(Kind=rp) :: length ! - axis length -
      Real(Kind=rp) :: aint   ! - intercept -
      Real(Kind=rp) :: tick1  ! - major tick-mark interval -
      Real(Kind=rp) :: tick2  ! - minor tick-mark interval -
      Real(Kind=rp) :: scale  ! - scaling -
!
      Character(Len=64) :: label ! - label -
!
      Logical :: laxis ! - logaritihmic axis? -
      Logical :: ldate ! - date axis? -
   End Type axis
!
! Derived type scalars
   Type(axis), Public :: x_axis ! - x-axis -
   Type(axis), Public :: y_axis ! - y-axis -
!
Contains
!
!
 Subroutine set_axis (aaxis, clab, laxis, ldate, i_pixel)
!
! Initialises axis settings
!
! Modules
   Use data_numbers, Only: zero, one
   Use maths,        Only: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: clab ! - axis label -
!
   Logical, Intent(In) :: laxis ! - logarithmic axis? -
   Logical, Intent(In) :: ldate ! - date axis? -
!
! - input/output scalars -
   Type(axis), Intent(InOut) :: aaxis ! - axis -
!
! - procedure arguments -
   Interface
      Integer Function i_pixel (z)
         Use data_numbers, Only: rp
         Real(Kind=rp), Intent(In) :: z
      End Function i_pixel
   End Interface
!
! Locals
!
! Local scalars
   Integer :: iom  ! - order of magnitude -
!
   Character(Len=15) :: cscale ! - scaling label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Trim
!
! Executable Statements
!
! Set axis flags
   aaxis%laxis = laxis
   aaxis%ldate = ldate
!
! Set intercept
   If (.not.ldate) Then
! - extremes both negative -
      If (aaxis%amin < zero .and. aaxis%amax < zero) Then
         aaxis%aint = aaxis%amax
! - extremes are of opposite signs -
      Else If (aaxis%amin < zero) Then
         aaxis%aint = zero
! - extremes both positive -
      Else
         aaxis%aint = aaxis%amin
      End If
! - date on X-axis -
   Else
      aaxis%aint = aaxis%amin
   End If
!
! Define limits
   aaxis%i_min = i_pixel(aaxis%amin)
   aaxis%i_max = i_pixel(aaxis%amax)
!
! Construct scaling label
   If (aaxis%scale /= one) Then
      iom = magnitude(aaxis%scale)
      If (iom > 0) Then
         If (iom <= 3) Then
            Write (Unit=cscale, Fmt='(A,I0,A)') '(x ', Nint(aaxis%scale), ')'
         Else
            Write (Unit=cscale, Fmt='(A,I0,A)') '(x 10^', iom-1, ')'
         End If
         aaxis%label = clab//' '//Trim(cscale)
      Else If (iom < 0) Then
         If (iom >= -3) Then
            Write (Unit=cscale, Fmt='(A,I0,A)') '(/ ', Nint(one/aaxis%scale), ')'
         Else
            Write (Unit=cscale, Fmt='(A,I0,A)') '(/ 10^', -iom-1, ')'
         End If
         aaxis%label = clab//' '//Trim(cscale)
      Else
         aaxis%label = clab
      End If
   Else
      aaxis%label = clab
   End If
!
   Return
 End Subroutine set_axis
!
!
!
 Subroutine get_xaxis_limits (x_axis, n1, ldate, &
            ix1, ix2, ixf, d1, d2, df, n2, nf)
!
! Sets appropriate axis limits for dates or for sequential data
!
! Modules
   Use data_time_constants, Only: isq_yr
   Use time,                Only: pdate, &
                                  Operator(<), &
                                  iseq, &
                                  date_diff
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - number of cases -
!
! - input/output scalars -
   Type(axis), Intent(InOut) :: x_axis ! - axis -
!
! - output scalars -
   Logical, Intent(Out) :: ldate ! - date axis? -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ix1 ! - first X value -
   Integer, Intent(In), Optional :: ix2 ! - first X value for overlay data -
   Integer, Intent(In), Optional :: ixf ! - first X value for new data -
   Integer, Intent(In), Optional :: n2  ! - number of values, overlay variables -
   Integer, Intent(In), Optional :: nf  ! - number of new data -
!
   Type(pdate), Intent(In), Optional :: d1 ! - first date on x-axis -
   Type(pdate), Intent(In), Optional :: d2 ! - first date for overlay data -
   Type(pdate), Intent(In), Optional :: df ! - first date for new data -
!
! Locals
!
! Local scalars
   Integer :: ix  ! - X index -
   Integer :: nmx ! - maximum number of forecasts -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Max, Min, Present
!
! Executable Statements
!
! Set appropriate limits for sequential data
   If (Present(ix1)) Then
      ldate = .false.
      If (Present(ix2)) Then
         If (Present(ixf)) Then
            ix = Min(ix1, ix2, ixf)
            Call calc_xaxis_limits (x_axis,Max(ix1+n1, ix2+n2, ixf+nf)-ix, &
                 ix1=ix)
         Else
            Call calc_xaxis_limits (x_axis,Max(n1, n2, ix2+n2-ix1, ix1+n1-ix2), &
                 ix1=Min(ix1, ix2))
         End If
      Else
         If (Present(ixf)) Then
            Call calc_xaxis_limits (x_axis,Max(n1, nf, ixf+nf-ix1, ix1+n1-ixf), &
                 ix1=Min(ix1, ixf))
         Else
            Call calc_xaxis_limits (x_axis, n1, &
                 ix1=ix1)
         End If
      End If
!
! Set appropriate limits for dates
   Else If (Present(d1)) Then
      ldate = .true.
      If (.not.Present(d2) .and. .not.Present(df)) Then
         Call calc_xaxis_limits (x_axis, n1, &
              d1=d1)
      Else If (Present(d2) .and. .not.Present(df)) Then
         Select Case (iseq)
          Case (isq_yr) ! - annual sequencing -
            ix = Min(d1%iyr, d2%iyr)
            nmx = Max(d1%iyr+n1-ix, d2%iyr+n2-ix)
            Call calc_xaxis_limits (x_axis, nmx, &
                 ix1=ix)
          Case (1:) ! - sub-seasonl sequencing -
            nmx = n1 + Abs(date_diff(d1, d2, iseq))
            If (d1 < d2) Then
               Call calc_xaxis_limits (x_axis, nmx, &
                    d1=d1)
            Else
               Call calc_xaxis_limits (x_axis, nmx, &
                    d1=d2)
            End If
         End Select
      Else If (Present(df) .and. .not.Present(d2)) Then
         Select Case (iseq)
          Case (isq_yr) ! - annual sequencing -
            ix = Min(d1%iyr, df%iyr)
            nmx = Max(d1%iyr+n1-ix, df%iyr+nf-ix)
            Call calc_xaxis_limits (x_axis, nmx, &
                 ix1=ix)
          Case (1:) ! - daily sequencing -
            nmx = n1 + Abs(date_diff(d1, df, iseq))
            If (d1 < df) Then
               Call calc_xaxis_limits (x_axis, nmx, &
                    d1=d1)
            Else
               Call calc_xaxis_limits (x_axis, nmx, &
                    d1=df)
            End If
         End Select
      Else If (Present(d2) .and. Present(df)) Then
         Select Case (iseq)
          Case (isq_yr) ! - annual sequencing -
            ix = Min(d1%iyr, d2%iyr, df%iyr)
            nmx = Max(d1%iyr+n1-ix, d2%iyr+n2-ix, df%iyr+nf-ix)
            Call calc_xaxis_limits (x_axis, nmx, &
                 ix1=ix)
          Case (1:) ! - daily sequencing -
            nmx = n1 + Abs(date_diff(d1, df, iseq))
            If (d1 < df) Then
               Call calc_xaxis_limits (x_axis, nmx, &
                    d1=d1)
            Else
               Call calc_xaxis_limits (x_axis, nmx, &
                    d1=df)
            End If
         End Select
      End If
   End If
!
   Return
 End Subroutine get_xaxis_limits
!
!
!
 Subroutine get_yaxis_limits (lpreset, laxis, y_axis, y, img_min, img_max, img_scale, &
            y2, yf, ylow, yhgh, ymin, ymax)
!
! Sets appropriate axis limits for continuous data
!
! Modules
   Use data_numbers,       Only: one
   Use data_gui_constants, Only: mnnt, mxnt
   Use maths,              Only: calc_limits, get_intervals
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lpreset ! - preset limits? -
   Logical, Intent(In) :: laxis   ! - logarithmic axis? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - y-data; shape(n, m) -
!
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: img_min   ! - image minimum -
   Real(Kind=rp), Intent(InOut) :: img_max   ! - image maximum -
   Real(Kind=rp), Intent(InOut) :: img_scale ! - image scaling -
!
   Type(axis), Intent(InOut) :: y_axis ! - axis -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: ylow ! - lowest y-value (soft limits) -
   Real(Kind=rp), Intent(In), Optional :: yhgh ! - highest y-value (soft limits) -
   Real(Kind=rp), Intent(In), Optional :: ymin ! - minimum value (hard limit) -
   Real(Kind=rp), Intent(In), Optional :: ymax ! - maximum value (hard limit) -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: y2 ! - overlay data -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: yf ! - new data -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: ylw ! - lowest y-value -
   Real(Kind=rp) :: yhg ! - highest y-value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Floor, Log10, Max, MaxVal, Min, MinVal, Present
!
! Executable Statements
!
! Set appropriate limits for continuous data
   If (lpreset) Then ! - preset limits -
      y_axis%amin = img_min
      y_axis%amax = img_max
      y_axis%scale = img_scale
      Call get_intervals (y_axis%amin, y_axis%amax, mnnt, mxnt, y_axis%tick1)
   Else ! - calculate limits -
      If (Present(ymin)) Then
         ylw = ymin
      Else If (Present(ylow)) Then
         ylw = ylow
      Else
         ylw = MinVal(y(:,:))
         If (Present(y2)) ylw = Min(ylw, MinVal(y2(:,:)))
         If (Present(yf)) ylw = Min(ylw, MinVal(yf(:,:)))
      End If
      If (Present(ymax)) Then
         yhg = ymax
      Else If (Present(yhgh)) Then
         yhg = yhgh
      Else
         yhg = MaxVal(y(:,:))
         If (Present(y2)) yhg = Max(yhg, MaxVal(y2(:,:)))
         If (Present(yf)) yhg = Max(yhg, MaxVal(yf(:,:)))
      End If
      If (laxis) Then ! - log axis -
         y_axis%amin = Floor(Log10(ylw))
         y_axis%amax = Ceiling(Log10(yhg))
         If (y_axis%amin == y_axis%amax) y_axis%amin = y_axis%amin - 1
         y_axis%scale = one
         y_axis%tick1 = one
      Else ! - linear axis -
         Call calc_limits (ylw, yhg, mnnt, mxnt, y_axis%amin, y_axis%amax, y_axis%scale, y_axis%tick1)
         If (Present(ymin)) y_axis%amin = ymin
         If (Present(ymax)) y_axis%amax = ymax
      End If
      img_min = y_axis%amin
      img_max = y_axis%amax
      img_scale = y_axis%scale
   End If
   y_axis%tick2 = y_axis%tick1
!
   Return
 End Subroutine get_yaxis_limits
!
!
!
 Subroutine calc_xaxis_limits (aaxis, n, &
            ix1, d1)
!
! Calculates appropriate axis limits when data are years
!
! Modules
   Use data_numbers,        Only: one, three, five, six, ten
   Use data_gui_constants,  Only: mxnt
   Use data_time_constants, Only: isq_dy, isq_mn, isq_sn, isq_so, isq_yr, lfeb, mdm, mdy, nmn
   Use maths,               Only: get_intervals
   Use time,                Only: pdate, &
                                  Operator(+), Operator(<), Operator(>), &
                                  iseq, lslim, nse, ssnlims, &
                                  ndays, pdate_to_idate, reset_iseq
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of years -
!
! - input/output scalars -
   Type(axis), Intent(InOut) :: aaxis ! - axis -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ix1 ! - first x value -
!
   Type(pdate), Intent(In), Optional :: d1 ! - first date -
!
! Locals
!
! Local scalars
   Integer :: ixf ! - first X value -
   Integer :: ixn ! - last X value -
!
   Type(pdate) :: adate ! - parsed date -
   Type(pdate) :: bdate ! - parsed date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Mod, Nint, Present, Real
!
! Executable Statements
!
! Round years to nearest 5
   If (Present(ix1)) Then
      ixf = ix1 - Mod(ix1, 5)
      ixn = ix1 + n
      Call rounding1 (5, 10)
!
! Identify appropriate day limits
   Else If (Present(d1)) Then
      Select Case (iseq)
       Case (isq_yr) ! - annual sequencing -
         ixf = d1%iyr - Mod(d1%iyr, 5)
         ixn = d1%iyr + n
         Call rounding1 (5, 10)
         aaxis%tick1 = Real(Nint(aaxis%tick1), Kind=rp)
       Case (isq_sn) ! - seasonal sequencing -
         ixf = pdate_to_idate(iseq, d1)
         ixn = ixf + n
         Call rounding2 (nmn/2, nmn)
       Case (isq_mn, isq_so) ! - monthly sequencing -
         ixf = pdate_to_idate(iseq, d1)
         ixn = ixf + n
         Call rounding2 (nmn, nmn*2)
       Case (10) ! - dekadal sequencing -
         If (n <= 10) Then
            ixf = pdate_to_idate(iseq, d1)
            ixn = ixf + n
            aaxis%tick1 = one
            aaxis%tick2 = aaxis%tick1
         Else If (n <= 3*nmn) Then
            adate%iyr = d1%iyr
            adate%imn = d1%imn
            adate%idy = 1
            ixf = pdate_to_idate(iseq, adate)
            ixn = Mod(n, 3)
            Select Case (ixn)
             Case (0)
               adate = d1 + n
               ixn = pdate_to_idate(iseq, adate)
             Case (1)
               adate=d1 + (n + 2)
             Case (2)
               adate=d1 + (n + 1)
            End Select
            ixn = pdate_to_idate(iseq,adate)
            If (n <= 3*nmn/2) Then
               aaxis%tick1 = three
            Else
               aaxis%tick1 = six
            End If
            aaxis%tick2 = one
         Else
            adate%iyr = d1%iyr
            adate%imn = 1
            adate%idy = 1
            ixf = pdate_to_idate(iseq, adate)
            adate = d1 + n
            If (adate%imn > 1 .or. adate%idy > 1) adate%iyr = adate%iyr + 1
            adate%imn = 1
            adate%idy = 1
            ixn = pdate_to_idate(iseq, adate)
            Call rounding2 (3, 3*nmn)
            aaxis%tick2 = three
         End If
       Case Default ! - daily, pentad, weekly and other sub-seasonl sequencing -
         If (lslim) Then ! - season limits -
            adate = ssnlims%sdate
            adate%iyr = d1%iyr
            If (adate > d1) adate%iyr = adate%iyr - 1
            ixf = pdate_to_idate(iseq, adate)
            adate = d1 + (n - 1)
            bdate = ssnlims%edate
            bdate%iyr = adate%iyr
            If (bdate < adate) bdate%iyr = bdate%iyr + 1
            ixn = pdate_to_idate(iseq, bdate)
            Call get_intervals (one, Real(n, Kind=rp)/Real(nse, Kind=rp), 1, mxnt, aaxis%tick1)
            aaxis%tick1 = aaxis%tick1*Real(nse, Kind=rp)
            aaxis%tick2 = Real(nse, Kind=rp)
         Else If (n <= mdy/iseq) Then ! - few cases -
            adate = d1
            adate%idy = 1
            ixf = pdate_to_idate(isq_dy, adate)
            adate = d1 + (n - 1)
            adate%idy = ndays(adate%iyr, adate%imn)
            If (iseq > 0) Call reset_iseq (isq_new=isq_dy)
            adate = adate + 1
            If (iseq > 0) Call reset_iseq ()
            ixn = pdate_to_idate(isq_dy, adate)
            If (n <= 5) Then
               aaxis%tick1 = Max(five, Real(iseq, Kind=rp))
               aaxis%tick2 = one
            Else If (n <= mdm/iseq) Then
               aaxis%tick1 = Max(ten, Real(iseq, Kind=rp))
               aaxis%tick2 = one
            Else
               aaxis%tick1 = Real(lfeb, Kind=rp)
               aaxis%tick2 = aaxis%tick1
            End If
         Else  ! - many cases -
            adate = d1
            adate%imn = 1
            adate%idy = 1
            ixf = pdate_to_idate(isq_dy, adate)
            adate = d1 + (n - 1)
            adate%imn = nmn
            adate%idy = ndays(adate%iyr, adate%imn)
            ixn = pdate_to_idate(isq_dy, adate)
            aaxis%tick1 = Real(mdy, Kind=rp)
            aaxis%tick2 = aaxis%tick1
         End If
      End Select
   Else
      ixf = 0
      ixn = 1
      aaxis%tick1 = one
      aaxis%tick2 = one
   End If
!
! Set axis limits and scaling
   If (Present(ix1)) Then
      aaxis%amin = Real(ixf, Kind=rp)
      aaxis%amax = Real(ixn, Kind=rp)
   Else If (Present(d1)) Then
      aaxis%amin = Real(ixf, Kind=rp)
      aaxis%amax = Real(ixn, Kind=rp)
   End If
   aaxis%scale = one
!
   Return
!
 Contains
!
!
  Subroutine rounding1 (n1, n2)
!
! Modules
   Use data_numbers, Only: tol, one, two, four, ten
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - smaller number -
   Integer, Intent(In) :: n2 ! - larger number -
!
! Locals
!
! Local scalars
   Integer :: iom ! - order of magnitude -
   Integer :: ixa ! - rounding -
   Integer :: ixm ! - maximum number of values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod, Real
!
! Executable Statements
!
! Identify sensible tick-mark interval
! - major tick-marks -
   ixa = Mod(ixn, 5)
   If (ixa > 0) ixn = ixn + 5 - ixa
   iom = 0
   Do
      ixa = n1*n2**iom
      ixm = n1*n2**(iom+1)
      If (ixn - ixf >= ixm) Then
         If (Mod(ixf, n2) == ixa) ixf = ixf - ixa
         If (Mod(ixn, n2) == ixa) ixn = ixn + ixa
      Else
         If (iom == 0) Then
            aaxis%tick1 = Real(n1, Kind=rp)
         Else
            aaxis%tick1 = Real(n2, Kind=rp)**iom
         End If
         Exit
      End If
      iom = iom + 1
   End Do
   If (Real(ixn - ixf, Kind=rp)/aaxis%tick1 > ten) aaxis%tick1 = aaxis%tick1*two
   If (Real(ixn - ixf, Kind=rp)/aaxis%tick1 < four) aaxis%tick1 = aaxis%tick1/two
! - major tick-marks -
   If (n <= 30) Then
      aaxis%tick2 = one
   Else If (Mod(aaxis%tick1, two) < tol) Then
      aaxis%tick2 = aaxis%tick1/two
   Else
      aaxis%tick2 = aaxis%tick1
   End If
!
   Return
  End Subroutine rounding1
!
!
!
  Subroutine rounding2 (n1, n2)
!
! Modules
   Use data_numbers, Only: two, four, ten
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - smaller number -
   Integer, Intent(In) :: n2 ! - larger number -
!
! Locals
!
! Local scalars
   Integer :: iom ! - order of magnitude -
   Integer :: ir  ! - range -
   Integer :: ixa ! - rounding -
   Integer :: ixm ! - maximum number of values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod, Nint, Real
!
! Executable Statements
!
! Identify sensible tick-mark interval
   iom = 0
   Do
      ixa = n1*n2*iom
      ixm = n1*n2*(iom + 1)
      If (ixn - ixf >= ixm) Then
         If (Mod(ixf, n2) == ixa) ixf = ixf - ixa
         If (Mod(ixn, n2) == ixa) ixn = ixn + ixa
      Else
         If (iom == 0) Then
            aaxis%tick1 = Real(n1, Kind=rp)
         Else
            aaxis%tick1 = Real(ixa, Kind=rp)
         End If
         Exit
      End If
      iom = iom + 1
   End Do
   ir = ixn - ixf
   Do
      If (Real(ir, Kind=rp)/aaxis%tick1 > ten) Then
         aaxis%tick1=aaxis%tick1*two
      Else If (Real(ir, Kind=rp)/aaxis%tick1 < four) Then
         aaxis%tick1 = aaxis%tick1/Real(n1, Kind=rp)
      Else
         Exit
      End If
   End Do
   aaxis%tick1 = Real(Nint(aaxis%tick1), Kind=rp)
!
   Return
  End Subroutine rounding2
 End Subroutine calc_xaxis_limits
!
!
!
 Subroutine draw_xaxis (x_axis, idir, llab, iy_font, &
            inum, iano, nse)
!
! Draws an x-axis
!
! Modules
   Use clrwin$, Only: draw_characters$, draw_line_between$, get_text_size$
   Use screen,  Only: font1, &
                      gfont_size, iy_pixel
   Use gui,     Only: icol
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: idir    ! - tick-mark direction (<0 down, >0 up) -
   Integer, Intent(In) :: iy_font ! - vertical font size in pixels -
!
   Logical, Intent(In) :: llab ! - draw labels? -
!
   Type(axis), Intent(In) :: x_axis ! - x-axis -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: inum ! - numerical labels positioning indicator (<0 left, >0 right) -
   Integer, Intent(In), Optional :: iano ! - annotation positioning indicator (<0 left, >0 right) -
   Integer, Intent(In), Optional :: nse  ! - number of seasons -
!
! Locals
!
! Local scalars
   Integer :: ilpos ! - position of Y-axis label -
   Integer :: isw   ! - string width -
   Integer :: isd   ! - string depth -
   Integer :: ix1   ! - X-axis X coordinate at minimum  -
   Integer :: ix2   ! - X-axis X coordinate at maximum -
   Integer :: iy1   ! - X-axis Y coordinate -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Plot axis
   ix1 = x_axis%i_min
   ix2 = x_axis%i_max
   iy1 = iy_pixel(y_axis%aint)
   Call draw_line_between$ (ix1, iy1, ix2, iy1, icol(1))
!
! Add tick-marks
   Call add_tick_marks ('X', x_axis, idir, iy1, iy_font, ilpos, &
        inum=inum, nse=nse)
!
! Add label
   If (Present(inum)) Call gfont_size (font1)
   If (llab) Then
      Call get_text_size$ (Trim(x_axis%label), isw, isd)
      ix1 = (x_axis%i_min + x_axis%i_max - isw)/2
      iy1 = ilpos + iano*(isd + 1)
      Call draw_characters$ (Trim(x_axis%label), ix1, iy1, icol(1))
   End If
!
   Return
 End Subroutine draw_xaxis
!
!
!
 Subroutine draw_yaxis (y_axis, idir, llab, iy_font, &
            inum, iano)
!
! Draws a y-axis
!
! Modules
   Use clrwin$,      Only: draw_characters$, draw_line_between$, get_text_size$, rotate_font$
   Use data_numbers, Only: zero, r90, r270
   Use screen,       Only: font1, &
                           gfont_size, ix_pixel
   Use gui,          Only: icol
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: idir    ! - tick-mark direction (<0 left, >0 right) -
   Integer, Intent(In) :: iy_font ! - vertical font size in pixels -
!
   Logical, Intent(In) :: llab ! - draw labels? -
!
   Type(axis), Intent(In) :: y_axis ! - y-axis -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: inum ! - numerical labels positioning indicator (<0 left, >0 right) -
   Integer, Intent(In), Optional :: iano ! - annotation positioning indicator (<0 left, >0 right) -
!
! Locals
!
! Local scalars
   Integer :: ilpos ! - position of Y-axis label -
   Integer :: isw   ! - string width -
   Integer :: isd   ! - string depth -
   Integer :: ix1   ! - Y-axis X coordinate -
   Integer :: iy1   ! - Y-axis Y coordinate at minimum -
   Integer :: iy2   ! - Y-axis Y coordinate at maximum -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Plot axis
   ix1 = ix_pixel(x_axis%aint)
   iy1 = y_axis%i_min
   iy2 = y_axis%i_max
   Call draw_line_between$ (ix1, iy1, ix1, iy2, icol(1))
!
! Add tick-marks
   Call add_tick_marks ('Y', y_axis, idir, ix1, iy_font, ilpos, &
        inum=inum)
!
! Add label
   If (Present(inum)) Call gfont_size (font1)
   If (llab) Then
      If (y_axis%amax > y_axis%amin) Then
         Call rotate_font$ (r90)
      Else
         Call rotate_font$ (r270)
      End If
      Call get_text_size$ (Trim(y_axis%label), isw, isd)
      ix1 = ilpos + iano*isd/4
      iy1 = (y_axis%i_min + y_axis%i_max + isw)/2
      Call draw_characters$ (Trim(y_axis%label), ix1, iy1, icol(1))
      Call rotate_font$ (zero)
   End If
!
   Return
 End Subroutine draw_yaxis
!
!
!
 Subroutine add_tick_marks (cxy, aaxis, idir, ixy, iy_font, ilpos, &
            inum, nse)
!
! Adds numerical or date tick-marks to an X- or Y-axis
! - NB. dates on Y-axis not working -
!
! Modules
   Use clrwin$,             Only: draw_characters$, draw_line_between$, get_text_size$
   Use data_numbers,        Only: zero, one, two, three, ten
   Use data_text,           Only: ilang
   Use data_time_constants, Only: cma, cmon, isq_mn, isq_sn, isq_so, isq_yr
   Use maths,               Only: iprec
   Use time,                Only: pdate, &
                                  iseq
   Use screen,              Only: font3, &
                                  gfont_size, ix_pixel, iy_pixel
   Use gui,                 Only: icol
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ixy     ! - Y/X coordinate of X/Y-axis -
   Integer, Intent(In) :: idir    ! - tick-mark direction (<0 up/left, >0 down/right) -
   Integer, Intent(In) :: iy_font ! - vertical font size in pixels -
!
   Character, Intent(In) :: cxy ! - X/Y-axis -
!
   Type(axis), Intent(In) :: aaxis ! - axis -
!
! - output scalars -
   Integer, Intent(Out), Optional :: ilpos ! - position of axis label -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: inum ! - numerical labels positioning indicator (<0 up/left, >0 down/right) -
   Integer, Intent(In), Optional :: nse  ! - number of seasons -
!
! Locals
!
! Local parameters
   Integer, Parameter :: lfmt = 12  ! - length of format statement -
   Integer, Parameter :: lclab = 16 ! - length of axis label -
!
! Local scalars
   Integer :: i         ! - tick-mark index -
   Integer :: icpos     ! - Y/X coordinate of X/Y-axis labels -
   Integer :: idateo    ! - date offset -
   Integer :: ilp       ! - label position -
   Integer :: ilfty     ! - left-most position of current Y-axis label -
   Integer :: imaj      ! - Y/X coordinate of X/Y-axis major tick-marks -
   Integer :: imin      ! - Y/X coordinate of X/Y-axis minor tick-marks -
   Integer :: ip        ! - precision on numerical labels -
   Integer :: isd       ! - string depth -
   Integer :: isw       ! - string width -
   Integer :: itick     ! - tick increment -
   Integer :: itp       ! - tick position -
   Integer :: mprec = 4 ! - maximum precision on numerical labels -
   Integer :: nminor    ! - number of minor tick-marks -
!
   Real(Kind=rp) :: a     ! - X/Y-value -
   Real(Kind=rp) :: b     ! - X/Y-value for minor tick-marks -
   Real(Kind=rp) :: amax  ! - maximum X/Y-value -
   Real(Kind=rp) :: amin  ! - minimum X/Y-value -
   Real(Kind=rp) :: tick  ! - tick-mark interval -
   Real(Kind=rp) :: atick ! - absolute major tick-mark interval -
   Real(Kind=rp) :: btick ! - absolute minor tick-mark interval -
!
   Character(Len= lfmt) :: cfmt ! - format statement -
   Character(Len=lclab) :: clab ! - value label -
!
   Logical :: ldaily ! - reset to daily intervals? -
   Logical :: lmon   ! - monthly intervals? -
   Logical :: lsl    ! - season limits? -
!
   Type(pdate) :: adate ! - current date -
   Type(pdate) :: bdate ! - current offset date -
   Type(pdate) :: ddate ! - current dekadal offset date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, AdjustL, Len_trim, Log10, Min, Max, Nint, Present, Real, Repeat, Sign, Trim
!
! Executable Statements
!
! Initialise font and tick-mark sizes
   If (Present(inum)) Call gfont_size (font3)
! - linear -
   If (.not.aaxis%laxis) Then
      tick = aaxis%tick1*Real(idir, Kind=rp)
      atick = Abs(tick)
      If (aaxis%tick2 > zero) Then
         nminor = Nint(aaxis%tick1/aaxis%tick2)
      Else
         nminor = 1
      End If
      If (nminor > 1) Then
         btick = aaxis%tick2*Real(idir, Kind=rp)
         btick = Abs(btick)
      Else
         btick = atick
      End If
! - logarithmic -
   Else
      tick = one
      atick = one
      nminor = 10
   End If
   If (.not.(atick > zero .or. Present(inum))) Return
!
! Identify maximum and minimum values
   amin = Min(aaxis%amin, aaxis%amax)
   amax = Max(aaxis%amin, aaxis%amax)
!
! Initialise tick-mark positions
   imaj = ixy + Nint(Sign(three, tick))
   If (nminor > 1) imin = ixy + Nint(Sign(two, tick))
   If (Present(inum)) Then
      icpos = ixy + 6*inum
      Select Case (cxy)
       Case ('X', 'x') ! - offset X-axis labels for font height -
         If (icpos > ixy) icpos = icpos + iy_font
      End Select
! - initialise numerical label format -
      ip = Max(iprec(atick, mprec), iprec(amin, mprec))
      Select Case (ip)
       Case (0)
         cfmt = '(I10)'
       Case Default
         Write (Unit=cfmt, Fmt='(A,I1,A)') '(F10.', ip, ')'
      End Select
      clab = Repeat(' ', lclab)
   Else
      icpos = ixy
   End If
   ilpos = icpos
!
! Identify starting point
   Call init_tick_marks (aaxis, atick, a, itick, lmon, ldaily, adate, bdate, ddate, &
        idateo=idateo)
   lsl = Present(nse)
   If (lsl) lsl = (nse > 1)
!
! Draw major tick-marks
! - repeat until end of the axis -
   tmajor: Do
      a = a + atick
      If (a < amin .or. a > amax .or. atick == zero) GoTo 1
! - draw major tick-marks -
      Select Case (cxy)
       Case ('X', 'x')
         itp = ix_pixel(a)
         If (atick > zero) Call draw_line_between$ (itp, ixy, itp, imaj, icol(1))
       Case ('Y', 'y')
         itp = iy_pixel(a)
         If (atick > zero) Call draw_line_between$ (ixy, itp, imaj, itp, icol(1))
      End Select
!
! Add tick-mark labels
! - construct label -
      If (Present(inum)) Then
         If (.not.aaxis%laxis) Then ! - linear axes -
            If (.not.aaxis%ldate) Then ! - numerical labels -
               If (ip > 0) Then
                  Write (Unit=clab, Fmt=cfmt) a
               Else
                  Write (Unit=clab, Fmt=cfmt) Nint(a)
               End If
               clab = AdjustL(clab)
            Else ! - date labels -
               Select Case (iseq)
                Case (isq_yr) ! - annual -
                  Write (Unit=clab, Fmt='(I4)') adate%iyr
                Case (isq_sn, isq_so) ! - seasonal -
                  If (itick > 1) Then
                     If (adate%imn == 1) Then
                        Write (Unit=clab, Fmt='(I4)') adate%iyr
                     Else
                        Write (Unit=clab, Fmt='(A)') ' '
                     End If
                  Else
                     Write (Unit=clab, Fmt='(A)') cmon(adate%imn:adate%imn+2, ilang)
                  End If
                Case (isq_mn) ! - monthly -
                  If (itick > 1) Then
                     If (adate%imn == 1) Then
                        Write (Unit=clab, Fmt='(I4)') adate%iyr
                     Else
                        Write (Unit=clab, Fmt='(A)') ' '
                     End If
                  Else
                     Write (Unit=clab, Fmt='(A)') cmon(adate%imn, ilang)
                  End If
                Case (10) ! - dekadal -
                  If (lsl) Then ! - season limits -
                    Write (Unit=clab, Fmt='(I4)') bdate%iyr
                  Else ! - no season limits -
                    Select Case (itick/3)
                     Case (12:)
                        Write (Unit=clab, Fmt='(I4)') bdate%iyr
                     Case (2:6)
                        Write (Unit=clab, Fmt='(A,1X,I4)') cma(ilang)(ddate%imn:ddate%imn+itick/3-1), ddate%iyr
                     Case (1)
                        Write (Unit=clab, Fmt='(A,1X,I4)') cmon(adate%imn, ilang), bdate%iyr
                     End Select
                  End If
                Case Default ! - subseasonal -
                  If (lsl) Then ! - season limits -
                    Write (Unit=clab, Fmt='(I4)') bdate%iyr
                  Else ! - no season limits -
                     If (idateo > 0) Then
                        Write (Unit=clab, Fmt='(A,1X,I4)') cmon(bdate%imn, ilang), bdate%iyr
                     Else
                        Write (Unit=clab, Fmt='(I2.2,1X,A,1X,I4)') adate%idy, cmon(adate%imn, ilang), adate%iyr
                     End If
                  End If
               End Select
            End If
         Else ! - log axes -
            If (a < zero) Then
               Write (cfmt, Fmt='(A,2(I2.2,A))') '(T', lclab+Nint(a)-2, ',A,I', 1-Nint(a), ')'
               Write (clab, Fmt=cfmt) '1/', Nint(ten**(-a))
               clab = AdjustL(clab)
            Else
               Write (clab, Fmt='(I0)') Nint(ten**a)
            End If
         End If
! - add label -
         If (Len_Trim(clab) > 0) Then
            Call get_text_size$ (Trim(clab), isw, isd)
            Select Case (cxy)
             Case ('X', 'x')
               ilp = itp - idateo
               If (ilp >= aaxis%i_min .and. ilp <= aaxis%i_max) Call draw_characters$ (Trim(clab), ilp-isw/2, icpos, icol(1))
             Case ('Y', 'y')
               ilfty = icpos - isw
               Call draw_characters$ (Trim(clab), ilfty, itp+iy_font/2, icol(1))
               ilpos = Min(ilpos, ilfty)
            End Select
         End If
      End If
!
! Add minor tick-marks
1     If (nminor > 1 .and. btick > zero) Then
         b = a
         tminor: Do i = nminor-1, 1, -1
            If (aaxis%laxis) Then
               b = (a - atick) + Log10(Real(i+1, Kind=rp))
            Else
               b = b - btick
            End If
            If (b < amin) Exit tminor
            If (b > amax) Cycle tminor
            Select Case (cxy)
             Case ('X', 'x')
               itp = ix_pixel(b)
               If (atick > zero) Call draw_line_between$ (itp, ixy, itp, imin, icol(1))
             Case ('Y', 'y')
               itp = iy_pixel(b)
               If (atick > zero) Call draw_line_between$ (ixy, itp, imin, itp, icol(1))
            End Select
         End Do tminor
      End If
      If (a > amax .or. atick == zero) Exit
!
! Increment dates
      If (aaxis%ldate) Call inc_date_tick_marks (lmon, ldaily, adate, bdate, ddate, itick, atick)
   End Do tmajor
!
   Return
 End Subroutine add_tick_marks
!
!
!
 Subroutine init_tick_marks (aaxis, atick, a, itick, lmon, ldaily, adate, bdate, ddate, &
            idateo)
!
! Arguments
!
! Modules
   Use data_numbers,        Only: zero, half, two
   Use data_time_constants, Only: isq_dy, isq_yr, lfeb
   Use time,                Only: pdate, &
                                  Assignment(=), Operator(-), &
                                  iseq, lslim, &
                                  idate_to_pdate, reset_iseq
   Use screen,              Only: ix_pixel
!
! Dummy arguments
! - input scalars -
   Type(axis), Intent(In) :: aaxis ! - axis -
!
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: atick ! - tick-mark increment -
!
! - output scalars -
   Integer, Intent(Out) :: itick ! - tick-mark increment -
!
   Real(Kind=rp), Intent(Out) :: a ! - starting value -
!
   Logical, Intent(Out) :: ldaily ! - reset to daily intervals? -
   Logical, Intent(Out) :: lmon   ! - monthly intervals? -
!
   Type(pdate), Intent(Out) :: adate ! - current date -
   Type(pdate), Intent(Out) :: bdate ! - current offset date -
   Type(pdate), Intent(Out) :: ddate ! - current dekadal offset date -
!
! Optional arguments
! - optional output scalars -
   Integer, Intent(Out), Optional :: idateo ! - date offset -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: amin ! - minimum axis-value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Mod, Nint, Present, Real
!
! Executable Statements
!
! Initialise
   amin = Min(aaxis%amin, aaxis%amax)
   itick = Nint(atick, Kind=rp)
   lmon = .false.
   ldaily = .false.
   adate = 0
   bdate = 0
   ddate = 0
!
! Identify starting point
! - linear axes -
   a = amin
   If (.not.aaxis%laxis) Then
      If (.not.aaxis%ldate) Then ! - numerical -
         If (Present(idateo)) idateo = 0
         If (atick > zero) a = a - Mod(a, atick) + Mod(amin, atick)
      Else ! - dates -
         Select Case (iseq)
          Case (isq_yr)
            adate%iyr = Nint(amin)
            adate%imn = 0
            adate%idy = 0
            a = a + half
            If (atick > zero) a = a - Mod(a, atick) + Mod(amin, atick)
            If (Present(idateo)) idateo = 0
          Case (1:)
            ldaily = (.not.lslim .and. iseq > 1 .and. iseq /= 10)
            If (ldaily) Call reset_iseq (isq_new=isq_dy)
            adate = idate_to_pdate(Nint(amin))
            bdate = idate_to_pdate(Nint(amin - atick/two))
            If (iseq == 10) ddate = adate - Nint(atick)
            If (ldaily) Call reset_iseq ()
            If (itick == lfeb) Then
               lmon = .true.
               itick = lfeb
               atick = Real(itick, Kind=rp)
            End If
            If (Present(idateo)) Then
               If (aaxis%tick1 < Real(lfeb, Kind=rp)) Then
                  If (iseq /= 10) Then
                     idateo = 0
                  Else If (aaxis%tick1 < 3) Then
                     idateo = 0
                  Else
                     idateo = (ix_pixel(a+atick) - ix_pixel(a))/2 ! - offset label to middle of current period -
                  End If
               Else
                  idateo = (ix_pixel(a+atick) - ix_pixel(a))/2 ! - offset label to middle of current period -
               End If
            End If
          Case Default
            adate = idate_to_pdate(Nint(amin))
            a = a + half
            If (Present(idateo)) idateo = 0
         End Select
      End If
   End If
   a = a - atick
!
   Return
 End Subroutine init_tick_marks
!
!
!
 Subroutine inc_date_tick_marks (lmon, ldaily, adate, bdate, ddate, inc, ainc)
!
! Increments date tick-marks
!
! Modules
   Use data_time_constants, Only: isq_dy
   Use time,                Only: pdate, &
                                  Operator(+), &
                                  ndays, reset_iseq
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lmon   ! - monthly intervals? -
   Logical, Intent(In) :: ldaily ! - reset to daily intervals? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: inc ! - increment -
!
   Real(Kind=rp), Intent(InOut) :: ainc ! - increment -
!
   Type(pdate), Intent(InOut) :: adate ! - current date -
   Type(pdate), Intent(InOut) :: bdate ! - current offset date -
   Type(pdate), Intent(InOut) :: ddate ! - current dekadal offset date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Increment dates
   If (lmon) Then
      inc = ndays(adate%iyr, adate%imn)
      ainc = Real(inc, Kind=rp)
   End If
   If (ldaily) Call reset_iseq (isq_new=isq_dy)
   adate = adate + inc
   bdate = bdate + inc
   ddate = ddate + inc
   If (ldaily) Call reset_iseq ()
!
   Return
 End Subroutine inc_date_tick_marks
End Module axes
