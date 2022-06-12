! Author: Simon Mason
Module maths
!
! Declarations:
!    definition of interval derived types (for confidence and prediction intervals)
! Calculates:
!    data precision and order of magnitude
!    random numbers
!    intervals for binning data
!    approximate equality
! Performs:
!    conversion of latitudes and longitudes and ordinal numbers to text
!    rounding
!    data bounding
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: approx, bin_prob, calc_limits, check_data_bounds, find_index, force_limits, force_odd, get_cnumber, get_flag,   &
             get_intervals, get_cordn, init_numbers, iprec, magnitude, newton_raphson, norm_rand, prob_rounding, random_int, &
             set_data_bounds, set_flag
!
! Derived Type Definitions
!
! - interval -
   Public interval
   Type interval
      Sequence
      Real(Kind=rp) :: lower ! - lower limit -
      Real(Kind=rp) :: upper ! - upper limit -
   End Type interval
!
! Explicit Interfaces
!
! Generic interfaces
   Interface magnitude
      Module Procedure magnitude_int
      Module Procedure magnitude_sp
      Module Procedure magnitude_dp
   End Interface magnitude
!
Contains
!
!
 Subroutine init_numbers ()
!
! Initialises various machine constants and defines some irrational numbers
!
! Modules
   Use data_numbers, Only: one, &
                           base, bt, elim
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Digits, Int, MaxExponent, Radix, Real
!
! Executable Statements
!
! Determine machine constants
   base = Real(Radix(one), Kind=rp)                     ! - base -
   elim = Real(Int(0.693_rp*MaxExponent(one)), Kind=rp) ! - maximum argument to EXP -
   bt = base**(Digits(one) - 1)                         ! - machine precision -
!
   Return
 End Subroutine init_numbers
!
!
!
 Subroutine newton_raphson (f, fmin, fmax, root)
!
! Newton-Raphson root-finding
!
! Modules
   Use data_numbers, Only: tol, two
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: fmin ! - minimum value -
   Real(Kind=rp), Intent(In) :: fmax ! - maximum value -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: root ! - root -
!
! - procedure arguments -
   External :: f ! - function -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: fval ! - current function value -
   Real(Kind=rp) :: fder ! - derivative of function value -
   Real(Kind=rp) :: df   ! - small difference in function value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Use midpoint as initial guess
   root = (fmin + fmax)/two
!
! Iterate
   Do
      Call f (root, fval, fder)
      df = fval/fder
      root = root - df
      If (Abs(df) < tol) Exit
   End Do
!
   Return
 End Subroutine newton_raphson
!
!
!
 Function approx(r1, r2, &
          utol)
!
! Indicates whether r1 is approximately equal to r2
!
! Modules
   Use data_numbers, Only: tol, one
!
! Function result
   Logical :: approx
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: r1 ! - value -
   Real(Kind=rp), Intent(In) :: r2 ! - target value -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: utol ! - tolerance -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: rtol ! - tolerance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Present
!
! Executable Statements
!
! Determine tolerance
   If (Present(utol)) Then
      rtol = utol
   Else If (Abs(r1) > one) Then
      rtol = tol*r1
   Else
      rtol = tol
   End If
   rtol = Abs(rtol)
!
! Identify whether r1 is close to r2
   approx = (r1 >= r2-rtol .and. r1 <= r2+rtol)
!
   Return
 End Function approx
!
!
!
 Pure Function magnitude_int(ix) &
               Result (magnitude)
!
! Calculates order of magnitude of an integer value
!
! Function result
   Integer :: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ix
!
! Locals
!
! Local scalars
   Integer :: iax ! - absolute value of argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Identify order of magnitude
   iax = Abs(ix)
   If (iax > 0) Then
      magnitude = 1
      Do
         If (iax < 10**magnitude) Exit
         magnitude = magnitude + 1
      End Do
   Else 
      magnitude = 0
   End If
!
   Return
 End Function magnitude_int
!
!
!
 Pure Function magnitude_sp(x) &
          Result (magnitude)
!
! Calculates order of magnitude of a single precision value
!
! Modules
   Use data_numbers, Only: sp, zero=>zero_sp, one=>one_sp, ten=>ten_sp
!
! Function result
   Integer :: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=sp), Intent(In) :: x
!
! Locals
!
! Local scalars
   Real(Kind=sp) :: ax ! - absolute value of argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Identify order of magnitude
   ax = Abs(x)
   If (ax < one) Then
      If (ax > zero) Then
         magnitude = -1
         Do
            If (ax > ten**magnitude) Exit
            magnitude = magnitude - 1
         End Do
      Else
         magnitude = 0
      End If
   Else
      magnitude = 1
      Do
         If (ax < ten**magnitude) Exit
         magnitude = magnitude + 1
      End Do
   End If
!
   Return
 End Function magnitude_sp
!
!
!
 Pure Function magnitude_dp(x) &
          Result (magnitude)
!
! Calculates order of magnitude of a double precision value
!
! Modules
   Use data_numbers, Only: dp, zero=>zero_dp, one=>one_dp, ten=>ten_dp
!
! Function result
   Integer :: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=dp), Intent(In) :: x
!
! Locals
!
! Local scalars
   Real(Kind=dp) :: ax ! - absolute value of argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Identify order of magnitude
   ax = Abs(x)
   If (ax < one) Then
      If (ax > zero) Then
         magnitude = -1
         Do
            If (ax > ten**magnitude) Exit
            magnitude = magnitude - 1
         End Do
      Else
         magnitude = 0
      End If
   Else
      magnitude = 1
      Do
         If (ax < ten**magnitude) Exit
         magnitude = magnitude + 1
      End Do
   End If
!
   Return
 End Function magnitude_dp
!
!
!
 Pure Elemental Function iprec(r, mprec)
!
! Returns number of decimal places
!
! Modules
   Use data_numbers, Only: ten
!
! Function result
   Integer :: iprec
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: mprec ! - maximum precision required -
!
   Real(Kind=rp), Intent(In) :: r ! - value -
!
! Locals
!
! Local scalars
   Integer :: ip ! - current precision -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod, Nint
!
! Executable Statements
!
! Identify precision
   iprec = 0
   Do ip = mprec, 1, -1
      If (Mod(Nint(r*ten**mprec), 10**ip) == 0) Exit
      iprec = iprec + 1
   End Do
!
   Return
 End Function iprec
!
!
!
 Function get_cnumber(r, mprec, &
          nprec) &
          Result (c)
!
! Converts real value to a character string with minimal number of decimal places
!
! Function result
   Character(Len=32) :: c
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: mprec ! - maximum precision required -
!
   Real(Kind=rp), Intent(In) :: r ! - number to convert -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nprec ! - minimum precision required -
!
! Locals
!
! Local scalars
   Integer :: ip ! - precision -
!
   Character(Len=8) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Nint, Present
!
! Executable Statements
!
! Convert to character
! - determine precision -
   ip = iprec(r, mprec)
   If (Present(nprec)) ip = Max(nprec, ip)
   Select Case (ip)
    Case (0)
      Write (Unit=c, Fmt='(I0)') Nint(r)
    Case (1:9)
      Write (Unit=cfmt, Fmt='(A,I1,A)') '(F0.', ip, ')'
      Write (Unit=c, Fmt=cfmt) r
    Case Default
      Write (Unit=cfmt, Fmt='(A,I2,A)') '(F0.', ip, ')'
      Write (Unit=c, Fmt=cfmt) r
   End Select
!
   Return
 End Function get_cnumber
!
!
!
 Function random_int(lower, upper)
!
! Generates a random number between the specified limits
!
! Function result
   Integer :: random_int
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lower ! - lower integer limit -
   Integer, Intent(In) :: upper ! - upper integer limit -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: r ! - random number -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Random_Number, Real
!
! Executable Statements
!
! Generate a random number
   Call Random_Number (r)
!
! Convert to range
   random_int = lower + Nint((Real(upper,Kind=rp) - Real(lower,Kind=rp))*r)
!
  Return
 End Function random_int
!
!
!
 Function norm_rand(xbar, xvar)
!
! Returns a normally-distributed random number
! Based on the algorithm of Rao et al. (2011). Indian Journal of Scientific Research, 2, 83-85
!
! Modules
   Use data_numbers, Only: one
!
! Function result
   Real(Kind=rp) :: norm_rand
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: xbar ! - mean -
   Real(Kind=rp), Intent(In) :: xvar ! - variance -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: r ! - uniform random number -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Random_Number, Sqrt
!
! Executable Statements
!
! Calculate random numbers
   Call Random_Number (r)
   norm_rand = -Log(one/r - one)/1.702_rp
   norm_rand = xbar + Sqrt(xvar)*norm_rand
!
   Return
 End Function norm_rand
!
!
!
 Function get_cordn (i)
!
! Returns the appropriate suffix for an ordinal number
!
! Modules
   Use data_numbers, Only: cordns
!
! Function result
   Character(Len=2) :: get_cordn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: i ! - number -
!
! Locals
   Integer :: i100 ! - Mod(i,100) -
   Integer :: i10  ! - Mod(i,10) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Executable Statements
!
! Determine appropriate suffix
   i100 = Mod(i,100)
   Select Case (i100)
    Case (0:3)
      get_cordn = cordns(i100)
    Case (4:20)
      get_cordn = cordns(0)
    Case Default
      i10 = Mod(i100,10)
      Select Case (i10)
       Case (0:3)
         get_cordn = cordns(i10)
       Case (4:9)
         get_cordn = cordns(0)
      End Select
   End Select
!
   Return
 End Function get_cordn
!
!
!
 Subroutine force_limits (ilow, ihgh, i, iadj)
!
! Forces a value to lie within specified limits, lopping if out of range by 1
! On exit:
!    iadj =  0 Value was within range
!    iadj =  1 Value was looped
!    iadj =  2 Value was outside of range
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilow ! - lower limit -
   Integer, Intent(In) :: ihgh ! - upper limit -
!
! - input/output scalars -
   Integer, Intent(InOut) :: i ! - value -
!
! - output scalars -
   Integer, Intent(Out) :: iadj ! - adjustment indicator -
!
! Executable Statements
!
! Loop if necessary
   If (i >= ilow .and. i <= ihgh) Then
      iadj = 0
      Return
   Else If (i == ilow-1) Then
      iadj = 1
      i = ihgh
   Else If (i == ihgh+1) Then
      iadj = 1
      i = 1
   Else If (i < ilow-1) Then
      iadj = 2
      i = 1
   Else If (i > ihgh+1) Then
      iadj = 2
      i = ihgh
   End If
!
   Return
 End Subroutine force_limits
!
!
!
 Subroutine force_odd (ioldv, inewv)
!
! Forces value to be odd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ioldv ! - old value -
!
! - input/output scalars -
   Integer, Intent(InOut) :: inewv ! - new value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Executable Statements
!
! Force lcw to be odd
   If (Mod(inewv,2) == 0 .and. inewv > 0) Then
      If (inewv > ioldv) Then
         inewv = inewv + 1
      Else
         inewv = inewv - 1
      End If
   End If
!
   Return
 End Subroutine force_odd
!
!
!
 Subroutine calc_limits (zlow, zhgh, minint, maxint, zmin, zmax, zscale, zint)
!
! Calculates appropriate limits given data limits
!
! Modules
   Use data_numbers, Only: zero, half, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: minint ! - minimum number of intervals -
   Integer, Intent(In) :: maxint ! - maximum number of intervals -
!
   Real(Kind=rp), Intent(In) :: zlow ! - lowest value -
   Real(Kind=rp), Intent(In) :: zhgh ! - highest value -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: zmin   ! - minimum -
   Real(Kind=rp), Intent(Out) :: zmax   ! - maximum -
   Real(Kind=rp), Intent(Out) :: zscale ! - scaling -
   Real(Kind=rp), Intent(Out) :: zint   ! - interval -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: arange ! - axis range -
   Real(Kind=rp) :: r      ! - ratio -
   Real(Kind=rp) :: r_old  ! - old ratio -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Get preliminary estimates for limits
   If (Abs(zlow) < Abs(zhgh)) Then
      zmin = rescaled(zlow, 'l', zscale)
      zmax = rescaled(zhgh, 'u', zscale)
   Else
      zmax = rescaled(zhgh, 'u', zscale)
      zmin = rescaled(zlow, 'l', zscale)
   End If
   If (.not.(zmin < zmax .or. zmin > zmax)) Then
      zmin = zmin - one
      zmax = zmax + one
   End If
   zmin = zmin*zscale
   zmax = zmax*zscale
!
! Reset if x=0 is nearby
   r = -one
   arange = zhgh - zlow
! - extremes both negative -
1  If (zlow < zero .and. zhgh < zero) Then
      If (Abs(arange) > zero) Then
         If (arange > zlow .or. -zhgh/arange < 0.2_rp) zmax = zero
      End If
! - extremes both positive -
   Else If (zlow > zero) Then
      If (Abs(arange) > zero) Then
         If (arange > zhgh .or. zlow/arange < 0.2_rp) zmin = zero
      End If
   End If
!
! Define appropriate intervals
   Call get_intervals (zmin, zmax, minint, maxint, zint)
!
! Compare range to data range, and reduce range if data range is too small
   r_old = r
   If (Abs(arange) > zero) Then
      r = Abs(((zhgh - zlow)*zscale)/(zmax - zmin))
   End If
   If (r < half .and. r > zero .and. r /= r_old) Then
      If (zhgh > zlow) Then
         Do
            If (zmax-zint > zhgh*zscale) Then
               zmax = zmax - zint
            Else
               Exit
            End If
         End Do
          Do
            If (zmin+zint < zlow*zscale) Then
               zmin = zmin + zint
            Else
               Exit
            End If
         End Do
      Else
         Do
            If (zmax+zint < zhgh*zscale) Then
               zmax = zmax + zint
            Else
               Exit
            End If
         End Do
         Do
            If (zmin-zint > zlow*zscale) Then
               zmin = zmin - zint
            Else
               Exit
            End If
         End Do
      End If
      GoTo 1
   End If
!
   Return
 End Subroutine calc_limits
!
!
!
 Subroutine get_intervals (zmin, zmax, minint, maxint, zint)
!
! Calculates appropriate intervals for dividing the data evenly
!
! Modules
   Use data_numbers, Only: zero, one, two, three, four, five, ten, oneh
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: minint ! - minimum number of intervals -
   Integer, Intent(In) :: maxint ! - maximum number of intervals -
!
   Real(Kind=rp), Intent(In) :: zmin ! - minimum -
   Real(Kind=rp), Intent(In) :: zmax ! - maximum -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: zint ! - interval -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: z ! - absolute value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Mod, Nint
!
! Executable Statements
!
! Define appropriate interval
   z = Abs(zmax-zmin)
   zint = one
   If (z > zero) Then
      interval: Do
         If (z <= one) Then
            zint = zint/ten
            z = z*ten
            Cycle interval
         Else If (z >= oneh) Then
            If (Mod(Nint(z), 100) == 0) Then
               zint = z/ten
               z = z/zint
               Cycle interval
            End If
            zint = zint*oneh
            z = z/oneh
         End If
         If (Nint(z) < minint) Then
            zint = zint/two
            z = z*two
         Else If (Nint(z) > maxint) Then
            If (Mod(z,five) == zero) Then
                zint = zint*five
                z = z/five
            Else If (Mod(z,four) == zero) Then
                zint = zint*four
                z = z/four
            Else If (Mod(z,three) == zero) Then
                zint = zint*three
                z = z/three
            Else
                zint = zint*five
                z = z/five
            End If
         Else
            Exit interval
         End If
      End Do interval
   End If
!
   Return
 End Subroutine get_intervals
!
!
!
 Function rescaled(z, ext, zscale)
!
! Rounds number up or down to an appropriate value
!
! Modules
   Use data_numbers, Only: zero, onettth, one, oneh, tent
!
! Function result
   Real(Kind=rp) :: rescaled
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: z ! - value -
!
   Character(Len=1), Intent(In) :: ext ! - tail -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: zscale ! - scaling factor -
!
! Locals
!
! Local scalars
   Integer :: i ! - rounded absolute value -
!
   Real(Kind=rp) :: a ! - absolute value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Int, Mod, Real, Sign
!
! Executable Statements
!
! Rescale
   a = Abs(z)
   zscale = one
   If (a > zero) Then
      rescale2: Do
         If (a < one) Then
            zscale = zscale*oneh
            a = a*oneh
         Else If (a > tent) Then
            zscale = zscale*onettth
            a = a*onettth
         Else
            Exit rescale2
         End If
      End Do rescale2
   End If
!
! Define appropriate limit
   i = Int(a)
   If ((z < zero .and. ext == 'l') .or. (z > zero .and. ext == 'u')) Then
      i = i + 1
   Else If ((z > zero .and. ext == 'l') .or. (z < zero .and. ext == 'u')) Then
      i = i - 1
   End If
   Select Case (i)
    Case (0:15)
      Continue
    Case (16:40)
      If (Mod(i, 5) /= 0) Then
         If ((z < zero .and. ext == 'l') .or. (z > zero .and. ext == 'u')) Then
            i = i + 5 - Mod(i, 5)
         Else
            i = i - Mod(i, 5)
         End If
      End If
    Case Default
      If (Mod(i, 10) /= 0) Then
         If ((z < zero .and. ext == 'l') .or. (z > zero .and. ext == 'u')) Then
            i = i + 10 - Mod(i, 10)
         Else
            i = i - Mod(i, 10)
         End If
      End If
   End Select
   rescaled = Real(i, Kind=rp)/zscale
   rescaled = Sign(rescaled, z)
!
   Return
 End Function rescaled
!
!
!
 Function bin_prob (nb, p)
!
! Bins probabilities to the central value
!
! Modules
   Use data_numbers, Only: half
!
! Function result
   Real(Kind=rp) :: bin_prob
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nb ! - number of bins -
!
   Real(Kind=rp), Intent(In) :: p ! - probability -
!
! Locals
!
! Local scalars
   Integer :: j ! - bin index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Check for binning
   If (nb == 0) Then
      bin_prob = p
!
! Bin the probability
   Else
      j = Nint(p*Real(nb-1, Kind=rp)) ! - identify probability bin -
      bin_prob = (half + Real(j, Kind=rp))/Real(nb-1, Kind=rp)
   End If
!
   Return
 End Function bin_prob
!
!
!
 Subroutine prob_rounding (iround, nc, c, p)
!
! Rounds probabilities to the nearest specified value keeping the total probability unchanged.
! Rounded probabilities have the best ignorance score.
! Climatological probabilities are returned if they yield a better ignorance score.
! Input probabilities are temporarily converted to sum to 100%. There is no check on the input sum.
!
! Modules
   Use data_numbers, Only: zero, oneh
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iround ! - value to round to (e.g., to round to nearest 5%, set iround=5) - 
   Integer, Intent(In) :: nc     ! - number of probabilities - 
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: c ! - climatological probabilities -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:), Intent(InOut) :: p ! - probabilities to round -
!
! Locals
!
! Local arrays
   Integer, Dimension(nc) :: ir  ! - rounding direction -
   Integer, Dimension(nc) :: iro ! - optimal rounding direction -
!
   Real(Kind=rp), Dimension(nc,2) :: pr ! - probabilities rounded up (1) and down (2) -
!
! Local scalars
   Integer :: ic ! - category index -
   Integer :: i  ! - category index -
   Integer :: j  ! - category index -
   Integer :: ki ! - rounding index (1=up, 2=down) -
   Integer :: kj ! - rounding index (1=up, 2=down) -
!
   Real(Kind=rp) :: ei   ! - expected ignorance -
   Real(Kind=rp) :: emin ! - minimum ignorance -
   Real(Kind=rp) :: rrp  ! - probability to round to -
   Real(Kind=rp) :: sp   ! - sum of probabilities -
   Real(Kind=rp) :: scp  ! - sum of climatological probabilities -
!
   Logical :: lczero ! - final category always zero? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic All, Int, Log, Nint, Real, Sum
!
! Executable Statements
!
! Rescale probabilities
   sp = Sum(p(:))
   p(:) = oneh*p(:)/sp
   rrp = Real(iround, Kind=rp)
!
! Round each probability up and down
   Do ic = 1, nc-1
      pr(ic,2) = Nint(rrp*Real(Int(p(ic)/rrp), Kind=rp)) ! - round down -
      pr(ic,1) = pr(ic,2) + rrp                          ! - round up -
      If (pr(ic,2) <= zero) pr(ic,2) = rrp
   End Do
!
! Consider climatological probabilities
   scp = Sum(c(:))
   emin = zero ! - calculate expected ignorance -
   Do ic = 1, nc
      If (c(ic) > zero) emin = emin - p(ic)*Log(c(ic)/scp)
   End Do
!
! Select option with the smallest expected ignorance
   iro(:) = 0
   ir(nc) = 1
   lczero = .true.
   Do i = 1, nc-2
      Do ki = 1, 2
         ir(i) = ki
         Do j = 2, nc-1
            Do kj = 1, 2
               ir(j) = kj
               pr(nc,1) = oneh ! - set probability of final category -
               Do ic = 1, nc-1
                  pr(nc,1) = pr(nc,1) - pr(ic,ir(ic))
               End Do
               If (pr(nc,1) <= zero) Cycle
               lczero = .false.
               ei = zero ! - calculate expected ignorance -
               Do ic = 1, nc
                  If (pr(ic,ir(ic)) > zero) ei = ei - p(ic)*Log(pr(ic,ir(ic))/oneh)
               End Do
               If (ei < emin) Then
                  emin = ei
                  iro(:) = ir(:)
               End If
            End Do
         End Do
      End Do
   End Do
!
! Correct if always rounding to zero
   If (lczero) Then
      pr(nc,1) = rrp
      Do ic = 1, nc-1
         If (All(p(:) <= p(ic))) Then
            pr(ic,2) = oneh - rrp*Real(nc-1, Kind=rp)
         Else
            pr(ic,2) = rrp
         End If
      End Do
      ei = zero ! - calculate expected ignorance -
      Do ic = 1, nc
         If (pr(ic,ir(ic)) > zero) ei = ei - p(ic)*Log(pr(ic,ir(ic))/oneh)
      End Do
      If (ei < emin) Then
         emin = ei
         iro(:) = ir(:)
      End If
   End If
!
! Apply optimal rounding
   If (iro(1) > 0) Then
      Do ic = 1, nc-1
         p(ic) = pr(ic,iro(ic))
      End Do
! - set probability of final category -
      p(nc) = oneh
      Do ic = 1, nc-1
         p(nc) = p(nc) - p(ic)
      End Do
! - apply climatological probabilities if they cannot be beaten -
   Else
      p(:) = sp*c(:)/scp
   End If
!
 End Subroutine prob_rounding
!
!
!
 Subroutine set_data_bounds (igto, igauss, iregr, izero, ibound, vmin, vmax, &
            t)
!
! Sets data bounds
!
! On exit:
!    ibound = 0 no bounds
!    ibound = 1 zero-bound, no gamma transformation of data
!    ibound = 2 zero-bound, gamma transformation of data
!    ibound = 3 zero-bound, link function applied
!
! Modules
   Use data_numbers,       Only: rp, zero, one
   Use data_cpt_constants, Only: ir_ols, ir_log, ir_bin, ir_poi, ir_gam
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igto   ! - gaussian transformation option -
   Integer, Intent(In) :: igauss ! - transform to gaussian flag -
   Integer, Intent(In) :: iregr  ! - regression flag -
   Integer, Intent(In) :: izero  ! - zero-bound flag -
!
! - output scalars -
   Integer, Intent(Out) :: ibound ! - data bounds indicator -
!
   Real(Kind=rp), Intent(Out) :: vmin ! - lower bound -
   Real(Kind=rp), Intent(Out) :: vmax ! - upper bound -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: t ! - binomial coefficient -
!
! Executable Statements
!
! Set tolerance for checking missing values
   Select Case (iregr)
    Case (ir_ols) ! - ordinary least squares regression -
      If (igto == 2 .and. igauss > 0) Then
         vmin = zero
         vmax = vmin
         ibound = 2
      Else If (izero == 1) Then
         vmin = zero
         vmax = vmin
         ibound = 1
      Else
         ibound = 0
      End If
    Case (ir_log) ! - logistic regression (bernoulli GLM) -
      vmin = zero
      vmax = one
      ibound = 3
    Case (ir_bin) ! - binomial GLM -
      vmin = zero
      vmax = t
      ibound = 3
    Case (ir_poi, ir_gam) ! - poisson and gamma GLM -
      vmin = zero
      vmax = vmin
      ibound = 3
   End Select
!
   Return
 End Subroutine set_data_bounds
!
!
!
 Subroutine check_data_bounds (n, v, iloc, &
            vmin, vmax, rmiss)
!
! Checks whether data lie within bounds
!
! On exit:
!    iloc(:) =  0 Successful
!    iloc(:) <  0 value at v(iloc) < vmin
!    iloc(:) >  0 value at v(iloc) > vmax
!
! Modules
   Use data_numbers, Only: rp, onem
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data -
!
! - output scalars -
   Integer, Intent(Out) :: iloc ! - location of first value outside of bounds -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vmin  ! - lower bound -
   Real(Kind=rp), Intent(In), Optional :: vmax  ! - upper bound -
   Real(Kind=rp), Intent(In), Optional :: rmiss ! - missing value -
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
   Real(Kind=rp) :: tol ! - missing value rounding tolerance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Present
!
! Executable Statements
!
! Set tolerance for checking missing values
   If (Present(rmiss)) tol = Abs(rmiss)/onem
!
! Search for values outside of bounds
! - lower bound -
   If (Present(vmin)) Then
      check_lower: Do i = 1, n
         If (v(i) < vmin) Then
            If (Present(rmiss)) Then
               If (approx(v(i), rmiss, utol=tol)) Cycle check_lower ! - permit missing values outside of bounds -
            End If
            iloc = i
            Return
         End If
      End Do check_lower
   End If
! - upper bound -
   If (Present(vmax)) Then
      check_upper: Do i = 1, n
         If (v(i) > vmax) Then
            If (Present(rmiss)) Then
               If (approx(v(i), rmiss, utol=tol)) Cycle check_upper ! - permit missing values outside of bounds -
            End If
            iloc = i
            Return
         End If
      End Do check_upper
   End If
   iloc = 0
!
   Return
 End Subroutine check_data_bounds
!
!
!
 Function get_flag(iflags)
!
! Identifies the index of the first flagged item in an integer array
!
! Function result
   Integer :: get_flag
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Dimension(:), Intent(In) :: iflags ! - flags -
!
! Executable Statements
!
! Identify flagged index
   get_flag = 1
   Do
      If (iflags(get_flag) == 1) Exit
      get_flag = get_flag + 1
   End Do
!
   Return
 End Function get_flag
!
!
!
 Subroutine set_flag (iflag, iflags)
!
! Sets the index in a flagged integer array
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iflag ! - flag -
!
! - output arrays -
   Integer, Dimension(:), Intent(Out) :: iflags ! - flags -
!
! Executable Statements
!
! Set flagged index
   iflags(:) = 0
   iflags(iflag) = 1
!
   Return
 End Subroutine set_flag
!
!
!
 Function find_index(n, iflag, ns) &
          Result (find)
!
! Finds the index of ns where iflag equals ns
!
! Function result
   Integer :: find
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n     ! - maximum number of values to search -
   Integer, Intent(In) :: iflag ! - flag -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ns ! - non-sequential numbers (dimension: n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Executable Statements
!
! Set flagged index
   find = 0
   Do i = 1, n
      If (iflag == ns(i)) Then
         find = i
         Exit
      End If
   End Do
!
   Return
 End Function find_index
End Module maths
