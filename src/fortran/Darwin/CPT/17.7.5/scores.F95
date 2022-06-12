! Author: Simon Mason
Module scores
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ncorr, ng, nps, nscore, nts
   Use maths,              Only: interval
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: calc_rel, calc_pscores, calc_scores, close_dscores, close_pscores, close_scores, dir_goodness, effective_interest, &
             get_corrs, get_pscores, get_scores, get_width_dscores, goodness, hbrier, init_attrib, init_correls, init_dscores,  &
             init_pscores, init_skill, linear_prob, print_dscores, profits, ranked_hits, ranked_prob_score, reset_scores, roc,  &
             rocp, set_dscore_range, set_pscore_range
!
! Scalars
!
! Integer scalars
   Integer, Public :: icorr  ! - correlation selection -
   Integer, Public :: ips    ! - probabilistic score selection -
   Integer, Public :: iskill ! - skill score selection -
!
! Logical scalars
   Logical, Public :: lerror ! - error score? -
   Logical, Public :: lcorr  ! - correlations calculated? -
!
! Arrays
!
! Integer arrays
   Integer, Dimension( ncorr), Public :: icorrs  ! - correlation selections -
   Integer, Dimension(   nps), Public :: ipss    ! - probabilistic score selections -
   Integer, Dimension(nscore), Public :: iskills ! - skill score selections -
!
   Integer, Dimension(:,:), Allocatable, Public :: ifq ! - frequencies -
!
! Real arrays
   Real(Kind=rp), Dimension(:), Allocatable, Public :: b0s     ! - intercepts -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: b1s     ! - slopes -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: corrs   ! - correlations -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: corrmsk ! - masking correlations -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: pscores ! - probabilistic verification scores -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: pvalues ! - p-values -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: rnko    ! - observation ranks -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: skills  ! - skill scores -
!
   Real(Kind=rp), Dimension(:), Allocatable, Private :: rnkf ! - forecast ranks -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: afp   ! - binned average forecast probabilities -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: bss   ! - Brier scores -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: far   ! - false alarm rates -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: fars  ! - false alarm rates -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: gm    ! - goodness metric -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hit   ! - hit rates -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hits  ! - hit rates -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: orf   ! - observed relative frequencies -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: rhit  ! - ranked hit scores -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tends ! - tendencies -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: wrlts ! - weather roulette scores -
!
   Real(Kind=rp), Dimension(nscore), Public :: score ! - scores -
   Real(Kind=rp), Dimension(nts),    Public :: roca  ! - ROC areas (individual points) -
   Real(Kind=rp), Dimension(ng),     Public :: rocas ! - ROC areas (all points) -
!
   Real(Kind=rp), Dimension(ng,ng), Private :: cs ! - LEPS coefficients -
   Real(Kind=rp), Dimension(ng,ng), Private :: gs ! - Gerrity coefficients -
!
! Derived-type arrays
   Type(interval), Dimension(:), Allocatable, Public :: psbcl ! - probabilistic scores bootstrap confidence limits -
!
! Explicit Interfaces
!
! Generic interfaces
   Interface p_corr
      Module Procedure p_corr1
      Module Procedure p_corr21
      Module Procedure p_corr22
   End Interface p_corr
!
   Interface heidke
      Module Procedure heidke1
      Module Procedure heidke2
   End Interface heidke
!
   Interface two_afc_mp
      Module Procedure two_afc_mp_1
      Module Procedure two_afc_mp_2
   End Interface two_afc_mp
!
Contains
!
!
 Subroutine reset_scores ()
!
! Resets score-selection parameters to defaults
!
! Executable Statements
!
! Score selections
   iskill = 1
   ips = 1
!
! Correlation selections
  icorr = 1
  lcorr = .false.
!
   Return
 End Subroutine reset_scores
!
!
!
 Function init_correls(m) &
          Result (init)
!
! Initialises memory for calculating correlations
!
! Modules
   Use errors, Only: cpt_error
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of locations -
!
! Locals
!
! Local scalars
   Integer :: istat ! - error flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Allocate additional workspace
! - correlations -
   If (.not.Allocated(corrs)) Then
      Allocate (corrs(m), Stat=istat)
      If (istat /= 0) GoTo 1
   End If
!
! Initialise correlation identifiers
   icorrs(:) = 0
   icorrs(icorr) = 1
!
   init = 0
   Return
!
! Errors
1  init = 1
   Call cpt_error ('init_correls', .false., init)
   If (Allocated(corrs)) Deallocate (corrs)
!
   Return
 End Function init_correls
!
!
!
 Function init_attrib() &
          Result (init)
!
! Initialises memory for constructing attributes diagrams
!
! Modules
   Use settings, Only: nb
!
! Function result
   Integer :: init
!
! Locals
!
! Local scalars
   Integer :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Size
!
! Executable Statements
!
! Initialise data space
   init = 1
! - frequencies -
   If (.not.Allocated(ifq)) Then
      Allocate (ifq(nb,0:ng), Stat=istat)
      If (istat /= 0) Return
   Else If (Size(ifq, Dim=1) /= nb) Then
      Deallocate (ifq)
      Allocate (ifq(nb,0:ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - average binned forecast probabilities -
   If (.not.Allocated(afp)) Then
      Allocate (afp(nb,0:ng), Stat=istat)
      If (istat /= 0) Return
   Else If (Size(afp, Dim=1) /= nb) Then
      Deallocate (afp)
      Allocate (afp(nb,0:ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - observed relative frequencies -
   If (.not.Allocated(orf)) Then
      Allocate (orf(nb,0:ng), Stat=istat)
      If (istat /= 0) Return
   Else If (Size(orf, Dim=1) /= nb) Then
      Deallocate (orf)
      Allocate (orf(nb,0:ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - tendencies -
   If (.not.Allocated(tends)) Then
      Allocate (tends(2,0:ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - intercepts -
   If (.not.Allocated(b0s)) Then
      Allocate (b0s(0:ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - slopes -
   If (.not.Allocated(b1s)) Then
      Allocate (b1s(0:ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - Brier scores -
   If (.not.Allocated(bss)) Then
      Allocate (bss(0:ng,2), Stat=istat)
      If (istat /= 0) Return
   End If
!
   init = 0
!
   Return
 End Function init_attrib
!
!
!
 Function init_pscores(m, &
          n, ibcl) &
          Result (init)
!
! Initialises memory for calculating probabilistic verification scores
!
! Modules
   Use errors, Only: cpt_error
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of locations -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: n    ! - number of cases -
   Integer, Intent(In), Optional :: ibcl ! - calculate bootstrap confidence limits flag -
!
! Locals
!
! Local scalars
   Integer :: istat ! - error flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Present, Size
!
! Executable Statements
!
! Allocate workspace
   If (Present(n)) Then
      If (init_dscores(n) /= 0) GoTo 1
   End If
!
! Allocate additional workspace
! - probabilistic scores -
   If (.not.Allocated(pscores)) Then
      Allocate (pscores(m), Stat=istat)
      If (istat /= 0) GoTo 1
   Else If (Size(pscores) < m) Then
      Deallocate (pscores)
      Allocate (pscores(m), Stat=istat)
      If (istat /= 0) GoTo 1
   End If
! -  bootstrap confidence intervals -
   If (Present(ibcl)) Then
      If (.not.Allocated(psbcl) .and. ibcl == 1) Then
         Allocate (psbcl(m), Stat=istat)
         If (istat /= 0) GoTo 1
      End If
   End If
!
! Initialise probabilistic scores identifiers
   ipss(:) = 0
   ipss(ips) = 1
!
   init = 0
   Return
!
! Errors
1  init = 1
   Call cpt_error ('init_pscores', .false., init)
   If (Present(ibcl)) Then
      If (Allocated(psbcl)) Deallocate (psbcl)
   End If
   If (Allocated(pscores)) Deallocate (pscores)
!
   Return
 End Function init_pscores
!
!
!
 Function init_dscores(n) &
          Result (init)
!
! Initialises memory for calculating deterministic verification scores
!
! Modules
   Use errors, Only: cpt_error
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! Locals
!
! Local scalars
   Integer :: istat ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Size
!
! Executable Statements
!
! Allocate workspace
! - observation ranks -
1  If (.not.Allocated(rnko)) Then
      Allocate (rnko(n), Stat=istat)
      If (istat /= 0) GoTo 2
   Else If (Size(rnko) < n) Then
      init = close_scores()
      GoTo 1
   End If
! - forecast ranks -
   If (.not.Allocated(rnkf)) Then
      Allocate (rnkf(n), Stat=istat)
      If (istat /= 0) GoTo 2
   End If
! - hit rates -
   If (.not.Allocated(hit)) Then
      Allocate (hit(n,2), Stat=istat)
      If (istat /= 0) GoTo 2
   End If
! - false alarm rates -
   If (.not.Allocated(far)) Then
      Allocate (far(n,2), Stat=istat)
      If (istat /= 0) GoTo 2
   End If
!
   init = 0
   Return
!
! Errors
2  init = 1
   Call cpt_error ('init_dscores', .false., init)
   init = 1 - close_dscores()
!
   Return
 End Function init_dscores
!
!
!
 Function init_skill(m, n, ipval) &
          Result (init)
!
! Initialises memory for skill maps
!
! Modules
   Use errors, Only: cpt_error
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m     ! - number of locations -
   Integer, Intent(In) :: n     ! - number of cases -
   Integer, Intent(In) :: ipval ! - p-values calculation flag -
!
! Locals
!
! Local scalars
   Integer :: istat ! - error flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Allocate workspace
   If (init_dscores(n) /= 0) GoTo 1
!
! Allocate additional workspace
! - skill scores -
   If (.not.Allocated(skills)) Then
      Allocate (skills(m), Stat=istat)
      If (istat /= 0) GoTo 1
   End If
   If (.not.Allocated(pvalues) .and. ipval == 1) Then
      Allocate (pvalues(m), Stat=istat)
      If (istat /= 0) GoTo 1
   End If
!
! Initialise skill score identifiers
   iskills(:) = 0
   iskills(iskill) = 1
!
   init = 0
   Return
!
! Errors
1  init = 1
   Call cpt_error ('init_skill', .false., init)
   If (Allocated(pvalues)) Deallocate (pvalues)
   If (Allocated(skills)) Deallocate (skills)
!
   Return
 End Function init_skill
!
!
!
 Function p_corr1(n, x, y) &
          Result (r)
!
! Calculates Pearson's product moment correlation (scalar version)
!
! Modules
   Use data_numbers, Only: zero, eps, one, ten
   Use maths,        Only: magnitude
!
! Function result
   Real(Kind=rp) :: r
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: imx ! - order of magnitude of X variance -
   Integer :: imy ! - order of magnitude of Y variance -
!
   Real(Kind=rp) :: df         ! - number of cases -
   Real(Kind=rp) :: xbar, ybar ! - means -
   Real(Kind=rp) :: sx2, sy2   ! - sums of squares -
   Real(Kind=rp) :: s2         ! - product of sums of squares -
   Real(Kind=rp) :: sxy        ! - sum of cross products -
   Real(Kind=rp) :: sclx       ! - X scaling -
   Real(Kind=rp) :: scly       ! - Y scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sqrt, Sum
!
! Executable Statements
!
! Calculate number of cases
   df = Real(n, Kind=rp)
!
! Calculate correlation
   xbar = Sum(x(1:n))/df
   ybar = Sum(y(1:n))/df
   sx2 = Sum((x(1:n) - xbar)**2)
   sy2 = Sum((y(1:n) - ybar)**2)
   If (sx2 > eps .and. sy2 > eps) Then
      sxy = Sum((x(1:n) - xbar)/Sqrt(sx2)*(y(1:n) - ybar)/Sqrt(sy2))
      r = sxy
 ! - rescale if variances are small -
   Else ! - set to similar orders of magnitude -
      imx = magnitude(sx2)
      imy = magnitude(sy2)
      If (imx > 1) Then
         sclx = ten**(1 - imx)
      Else If (imx < 0) Then
         sclx = ten**(-imx)
      Else
         sclx = one
      End If
      If (imy > 1) Then
         scly = ten**(1 - imy)
      Else If (imy < 0) Then
         scly = ten**(-imy)
      Else
         scly = one
      End If
      sx2 = sx2*sclx
      sy2 = sy2*scly
      sxy = Sum((x(1:n) - xbar)*Sqrt(sclx)*(y(1:n) - ybar)*Sqrt(scly))
      s2 = sx2*sy2
      If (s2 > eps) Then
         r = sxy/Sqrt(s2)
      Else  ! - if covariance is still too small -
         r = zero
      End If
   End If
!
   Return
 End Function p_corr1
!
!
!
 Function p_corr21(m, n, x, y) &
          Result (r)
!
! Calculates Pearson's product moment correlation (array version)
!
! Modules
   Use data_numbers, Only: zero, eps, one, ten
   Use maths,        Only: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of variables -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - independent variables; shape(m,n) -
!
! Function result
   Real(Kind=rp), Dimension(Size(x, Dim=1)) :: r
!
! Locals
!
! Local scalars
   Integer :: i   ! - index -
   Integer :: imx ! - order of magnitude of X variance -
   Integer :: imy ! - order of magnitude of Y variance -
!
   Real(Kind=rp) :: df         ! - number of cases -
   Real(Kind=rp) :: xbar, ybar ! - means -
   Real(Kind=rp) :: sx2, sy2   ! - sums of squares -
   Real(Kind=rp) :: s2         ! - product of sums of squares -
   Real(Kind=rp) :: sxy        ! - sum of cross products -
   Real(Kind=rp) :: sclx       ! - X scaling -
   Real(Kind=rp) :: scly       ! - Y scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Size, Sqrt, Sum
!
! Executable Statements
!
! Calculate number of cases
   df = Real(n, Kind=rp)
!
! Calculate correlation
   ybar = Sum(y(1:n))/df
   sy2 = Sum((y(1:n) - ybar)**2)
   If (sy2 > eps) Then
      imy = magnitude(sy2)
      If (imy > 1) Then
         scly = ten**(1 - imy)
      Else If (imy < 0) Then
         scly = ten**(-imy)
       Else
         scly = one
      End If
   End If
!
! Calculate correlation
   Do i = 1, m
      xbar = Sum(x(i,1:n))/df
      sx2 = Sum((x(i,1:n) - xbar)**2)
      If (sx2 > eps .and. sy2 > eps) Then
         sxy = Sum((x(i,1:n) - xbar)/Sqrt(sx2)*(y(1:n) - ybar)/Sqrt(sy2))
         r(i) = sxy
      Else ! - rescale if variances are small -
         imx = magnitude(sx2)
         If (imx > 1) Then
            sclx = ten**(1 - imx)
         Else If (imx < 0) Then
            sclx = ten**(-imx)
         Else
            sclx = one
         End If
         sx2 = sx2*sclx
         sy2 = sy2*scly
         sxy = Sum((x(i,1:n) - xbar)*Sqrt(sclx)*(y(1:n) - ybar)*Sqrt(scly))
         s2 = sx2*sy2
         If (s2 > eps) Then
            r(i) = sxy/Sqrt(s2)
         Else
            r(i) = zero
         End If
      End If
   End Do
!
   Return
 End Function p_corr21
!
!
!
 Function p_corr22(m, n, x, y) &
          Result (r)
!
! Calculates Pearson's product moment correlation (array version)
!
! Modules
   Use data_numbers, Only: zero, eps, one, ten
   Use maths,        Only: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of variables -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - dependent variables; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - independent variables; shape(m,n) -
!
! Function result
   Real(Kind=rp), Dimension(Size(x, Dim=1)) :: r
!
! Locals
!
! Local scalars
   Integer :: i   ! - index -
   Integer :: imx ! - order of magnitude of X variance -
   Integer :: imy ! - order of magnitude of Y variance -
!
   Real(Kind=rp) :: df        ! - number of cases -
   Real(Kind=rp) :: xbar,ybar ! - means -
   Real(Kind=rp) :: sx2,sy2   ! - sums of squares -
   Real(Kind=rp) :: s2        ! - product of sums of squares -
   Real(Kind=rp) :: sxy       ! - sum of cross products -
   Real(Kind=rp) :: sclx      ! - X scaling -
   Real(Kind=rp) :: scly      ! - Y scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real,Size,Sqrt,Sum
!
! Executable Statements
!
! Calculate number of cases
   df = Real(n, Kind=rp)
!
! Calculate correlation
   Do i = 1, m
      xbar = Sum(x(i,1:n))/df
      ybar = Sum(y(i,1:n))/df
      sx2 = Sum((x(i,1:n) - xbar)**2)
      sy2 = Sum((y(i,1:n) - ybar)**2)
      If (sx2 > eps .and. sy2 > eps) Then
         sxy = Sum((x(i,1:n) - xbar)/Sqrt(sx2)*(y(i,1:n) - ybar)/Sqrt(sy2))
         r(i) = sxy
      Else ! - rescale if variances are small -
         imx = magnitude(sx2)
         imy = magnitude(sy2)
         If (imx > 1) Then
            sclx = ten**(1 - imx)
         Else If (imx < 0) Then
            sclx = ten**(-imx)
         Else
            sclx = one
         End If
         If (imy > 1) Then
            scly = ten**(1 - imy)
         Else If (imy < 0) Then
            scly = ten**(-imy)
         Else
            scly = one
         End If
         sx2 = sx2*sclx
         sy2 = sy2*scly
         sxy = Sum((x(i,1:n) - xbar)*Sqrt(sclx)*(y(i,1:n) - ybar)*Sqrt(scly))
         s2 = sx2*sy2
         If (s2 > eps) Then
            r(i) = sxy/Sqrt(s2)
         Else
            r(i) = zero
         End If
      End If
   End Do
!
   Return
 End Function p_corr22
!
!
!
 Function s_corr(n, rx, ry) &
          Result (s)
!
! Calculates Spearman's rank-order correlation
!
! Modules
   Use data_numbers, Only: one, six
!
! Function result
   Real(Kind=rp) :: s
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rx ! - ranks of independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ry ! - ranks of dependent variables; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: df  ! - number of cases -
   Real(Kind=rp) :: sd2 ! - sum of squared rank differences -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate number of cases
   df = Real(n, Kind=rp)
!
! Calculate correlation
   sd2 = Sum((rx(1:n) - ry(1:n))**2)
   s = one - six*sd2/(df*(df**2 - one))
!
   Return
 End Function s_corr
!
!
!
 Function k_tau(n, x, y) &
          Result (tau)
!
! Calculates Kendall's tau
!
! Modules
   Use data_numbers, Only: zero, one, two, oneh
!
! Function result
   Real(Kind=rp) :: tau
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Executable Statements
!
! Calculate Kendall's tau
   If (n > 1) Then
      tau = two_afc_cc(n, x(:), y(:))*two/oneh - one
   Else
      tau = zero
   ENd If
!
   Return
 End Function k_tau
!
!
!
 Function mserror(n, x, y) &
          Result (err)
!
! Calculates mean squared-error
!
! Function result
   Real(Kind=rp) :: err
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real,Sum
!
! Executable Statements
!
! Calculate mean squared-error
   df = Real(n, Kind=rp)
   err = Sum((x(1:n) - y(1:n))**2)/df
!
   Return
 End Function mserror
!
!
!
 Function maerror(n, x, y) &
          Result (err)
!
! Calculates mean absolute-error
!
! Function result
   Real(Kind=rp) :: err
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Real, Sum
!
! Executable Statements
!
! Calculate mean absolute-error
   df = Real(n, Kind=rp)
   err = Sum(Abs(x(1:n) - y(1:n)))/df
!
   Return
 End Function maerror
!
!
!
 Function var_ratio(n, x, y)
!
! Calculates ratio of forecasts to observations
!
! Modules
   Use data_numbers, Only: zero, eps, one
!
! Function result
   Real(Kind=rp) :: var_ratio
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - forecasts; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - observations; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: df   ! - number of cases -
   Real(Kind=rp) :: xbar ! - mean -
   Real(Kind=rp) :: ybar ! - mean -
   Real(Kind=rp) :: xss  ! - sum of squares -
   Real(Kind=rp) :: yss  ! - sum of squares -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate degrees of freedom
   df = Real(n, Kind=rp)
!
! Calculate variance ratio
   xbar = Sum(x(1:n))/df
   ybar = Sum(y(1:n))/df
   xss = Sum((x(1:n) - xbar)**2)
   yss = Sum((y(1:n) - ybar)**2)
   If (yss > eps) Then
      var_ratio = Sum((x(1:n) - xbar)**2)/Sum((y(1:n) - ybar)**2)
   Else If (xss < eps) Then
      var_ratio = one
   Else
      var_ratio = zero
   End If
!
   Return
 End Function var_ratio
!
!
!
 Pure Function mbias(n, x, y)
!
! Calculates mean bias
!
! Function result
   Real(Kind=rp) :: mbias
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate mean bias
   df = Real(n, Kind=rp)
   mbias = (Sum(y(1:n)) - Sum(x(1:n)))/df
!
   Return
 End Function mbias
!
!
!
 Subroutine heidke1 (n, ng, iobs, ifor, cps, hs, hss)
!
! Calculates Heidke skill scores
!
! Modules
   Use data_numbers, Only: eps, third, half, oneh
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: ng ! - number of categories -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - verification data; shape(n) -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecast data; shape(n) -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: cps ! - climatological probabilities; shape(ng) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: hs  ! - hit score -
   Real(Kind=rp), Intent(Out) :: hss ! - hit skill score -
!
! Locals
!
! Local scalars
   Integer :: i ! - category index -
   Integer :: j ! - category index -
!
   Real(Kind=rp) :: ehit ! - expected number of hits -
   Real(Kind=rp) :: ahit ! - actual number of hits -
   Real(Kind=rp) :: df   ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Real, Sum
!
! Executable Statements
!
! Calculate expected number of hits from random guessing
   df = Real(n, Kind=rp)
   ehit = Sum(cps(:)**2)
   ehit = df*ehit
!
! Calculate actual number of hits
   ahit = Real(Count(ifor(1:n) == iobs(1:n)), Kind=rp)
!
! Calculate partial hits for ties
   Do i = 1, ng-1
      Do j = i+1, ng
         ahit = ahit + half*Real(Count(-j-ifor(1:n) == iobs(1:n)), Kind=rp)
      End Do
   End Do
   ahit = ahit + third*Real(Count(ifor(1:n) == -ng*(ng+1)/2), Kind=rp)
!
! Calculate hit scores
   hs = oneh*ahit/df
!
! Calculate Heidke skill scores
   If (ahit >= ehit) Then
      If (ehit < df) Then
         hss = oneh*(ahit - ehit)/(df - ehit)
      Else
         hss = oneh*(ahit - df)/df
      End If
   Else
      If (ehit > eps) Then
         hss = oneh*(ahit - ehit)/ehit
      Else
         hss = oneh*ahit/df
      End If
   End If
!
   Return
 End Subroutine heidke1
!
!
!
 Subroutine heidke2 (m, n, ng, iobs, ifor, cps, &
            hs, hss)
!
! Calculates Heidke skill scores
!
! Modules
   Use data_numbers, Only: eps, third, half, oneh
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m  ! - number of variables -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: ng ! - number of categories -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - verification data; shape(m,n) -
   Integer, Dimension(:,:), Intent(In) :: ifor ! - forecast data; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities; shape(m,ng) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: hs  ! - hit score -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: hss ! - hit skill score -
!
! Locals
!
! Local scalars
   Integer :: ij ! - location index -
   Integer :: i  ! - category index -
   Integer :: j  ! - category index -
!
   Real(Kind=rp) :: ehit ! - expected number of hits -
   Real(Kind=rp) :: ahit ! - actual number of hits -
   Real(Kind=rp) :: df   ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Present, Real, Sum
!
! Executable Statements
!
! Calculate expected number of hits from random guessing
   Do ij = 1, m
      df = Real(n, Kind=rp)
      ehit = Sum(cps(ij,:)**2)
      ehit = df*ehit
!
! Calculate actual number of hits
      ahit = Real(Count(ifor(ij,1:n) == iobs(ij,1:n)), Kind=rp)
!
! Calculate partial hits for ties
      Do i = 1, ng-1
         Do j = i+1, ng
            ahit = ahit + half*Real(Count(-j-ifor(ij,1:n) == iobs(ij,1:n)), Kind=rp)
         End Do
      End Do
      ahit = ahit + third*Real(Count(ifor(ij,1:n) == -ng*(ng+1)/2), Kind=rp)
!
! Calculate hit scores
      If (Present(hs)) hs(ij) = oneh*ahit/df
!
! Calculate Heidke skill scores
      If (Present(hss)) Then
         If (ahit >= ehit) Then
            If (ehit < df) Then
               hss(ij) = oneh*(ahit - ehit)/(df - ehit)
            Else
               hss(ij) = oneh*(ahit - df)/df
            End If
         Else
            If (ehit > eps) Then
               hss(ij) = oneh*(ahit - ehit)/ehit
            Else
               hss(ij) = oneh*ahit/df
            End If
         End If
      End If
   End Do
!
   Return
 End Subroutine heidke2
!
!
!
 Subroutine leps_coeffs (ng, cps, cs)
!
! Calculates coefficients for LEPS tables
!
! Modules
   Use data_numbers, Only: zero, eps, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of groups -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: cps ! - climatological probabilities; shape(ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: cs ! - LEPS coefficients; shape(ng,ng) -
!
! Locals
!
! Local scalars
   Integer :: i,j ! - indices -
!
   Real(Kind=rp) :: sp ! - sum of perfect scores -
!
! Local arrays
   Real(Kind=rp), Dimension(ng+1) :: cbnd ! - category boundaries -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Set up bounds
   cbnd(1) = zero
   Do i = 1, ng-1
      cbnd(i+1) = cbnd(i) + cps(i)
   End Do
   cbnd(ng+1) = one
!
! Calculate LEPS2 coefficients
   Do Concurrent (i = 1:ng)
      Do Concurrent (j = 1:ng)
         cs(j,i) = cint(cbnd(i), cbnd(i+1), cbnd(j), cbnd(j+1))
      End Do
   End Do
!
! Calculate re-scaled scores
! - re-scale perfect scores to sum to nc-1 -
   sp = zero
   Do i = 1, ng
      sp = sp + cs(i,i)
   End Do
   If (sp > eps) cs(:,:) = cs(:,:)*Real(ng - 1)/sp
!
   Return
!
 Contains
!
!
  Pure Function cint (x1, x2, y1, y2)
!
! Initialises scoring coefficients
!
! Modules
   Use data_numbers, Only: two, three
!
! Function result
   Real(Kind=rp) :: cint
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: x1, x2, y1, y2 ! - limits -
!
! Executable Statements
!
! Calculate score
   cint = (x2 - x1)*(y2 - y1)*two/three   &
        + (y2 - y1)*(x2**3 - x1**3)/three &
        + (x2 - x1)*(y2**3 - y1**3)/three
!
! Check for overlap
   If (y1 >= x2) Then
      cint = cint - (x2 - x1)*(y2**2 - y1**2)
   Else If (y2 >= x1) Then
      cint = cint - (y2 - y1)*(x2**2 - x1**2)
   Else
      cint = cint + two*(x1*x2**2 - two*x2**3/three - x1**3/three)
   End If
!
   Return
  End Function cint
 End Subroutine leps_coeffs
!
!
!
 Function lepscat(n, iobs, ifor, cs)
!
! Calculates LEPS score for categorical forecasts
!
! Modules
   Use data_numbers, Only: zero, eps, oneh
!
! Function result
   Real(Kind=rp) :: lepscat
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - verification categories; shape(n) -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecast categories; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cs ! - LEPS coefficients; shape(max(iobs),max(ifor)) -
!
! Locals
!
! Local scalars
   Integer :: k ! - case index -
!
   Real(Kind=rp) :: sf ! - score -
   Real(Kind=rp) :: sp ! - perfect score -
!
! Executable Statements
!
! Calculate LEPS score
   sf = zero
   sp = zero
   Do k = 1, n
      If (iobs(k) == 0 .or. ifor(k) == 0) Cycle
      sf = sf + cs(iobs(k),ifor(k))
      sp = sp + cs(iobs(k),iobs(k))
   End Do
!
   If (sp > eps) Then
      lepscat = sf*oneh/sp
   Else
      lepscat = zero
   End If
!
   Return
 End Function lepscat
!
!
!
 Subroutine gerrity_coeffs (ng, n, iobs, cs)
!
! Calculates coefficients for Gerrity tables
!
! Modules
   Use data_numbers, Only: zero, eps, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of groups -
   Integer, Intent(In) :: n  ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observed categories; shape(n) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: cs  ! - Gerrity coefficients; shape(ng,ng) -
!
! Locals
!
! Local scalars
   Integer :: i,j ! - indices -
!
   Real(Kind=rp) :: df    ! - number of tresholds -
   Real(Kind=rp) :: asum  ! - sum of weights -
   Real(Kind=rp) :: aisum ! - sum of inverse of weights -
!
! Local arrays
   Real(Kind=rp), Dimension(ng) :: ps ! - sample probablities -
   Real(Kind=rp), Dimension(ng) :: a  ! - weights -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate sample probablities
   ps(:) = zero
   Do Concurrent (i = 1:n)
      ps(iobs(i)) = ps(iobs(i)) + one
   End Do
   Do j = 2, ng-1
      ps(j) = ps(j) + ps(j-1)
   End Do
   ps(:) = ps(:)/Real(n, Kind=rp)
!
! Calculate weights
   df = Real(ng-1, Kind=rp)
   asum = zero
   Do i = 1, ng-1
      If (ps(i) > eps) Then
         a(i) = (one - ps(i))/ps(i)
      Else
         a(i) = zero
      End If
      asum = asum + a(i)
   End Do
   a(ng) = zero
!
! Calculate Gerrity coefficients
! - diagonals -
   aisum = zero
   Do i = 1, ng-1
      cs(i,i) = (aisum + asum)/df
      asum = asum - a(i)
      If (a(i) > eps) aisum = aisum + one/a(i)
   End Do
   cs(ng,ng) = (aisum + asum)/df
! - off-diagonals -
   aisum = zero
   Do i = 1, ng-1
      asum = Sum(a(i+1:ng))
      Do j = i+1, ng
         cs(j,i) = (aisum + asum - Real(j-i, Kind=rp))/df
         cs(i,j) = cs(j,i)
         asum = asum - a(j)
      End Do
      If (a(i) > eps) aisum = aisum + one/a(i)
   End Do
!
   Return
 End Subroutine gerrity_coeffs
!
!
!
 Function gerrity(n, iobs, ifor, gs)
!
! Calculates Gerrity score for categorical forecasts
!
! Modules
   Use data_numbers, Only: zero, oneh
!
! Function result
   Real(Kind=rp) :: gerrity
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - verification categories; shape(n) -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecast categories; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: gs ! - Gerrity coefficients; shape(max(iobs),max(ifor)) -
!
! Locals
!
! Local scalars
   Integer :: k ! - case index -
!
! Executable Statements
!
! Calculate Gerrity score
   gerrity = zero
   Do k = 1, n
      gerrity = gerrity + gs(iobs(k),ifor(k))
   End Do
   gerrity = gerrity*oneh/Real(n, Kind=rp)
!
   Return
 End Function gerrity
!
!
!
 Subroutine roc (n, ig, iobs, rnks, roca, hit, far)
!
! Calculates relative operating characteristics (ROC) from ordinal forecasts
!
! Modules
   Use data_numbers, Only: zero, half, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of forecasts -
   Integer, Intent(In) :: ig ! - category of interest -
!
   Real(Kind=rp), Intent(Out) :: roca ! - ROC area -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - verification categories; shape(n) -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rnks ! - ranked forecasts; shape(n) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: hit ! - hit rate; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(Out) :: far ! - false alarm rate; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k    ! - indices -
   Integer :: nes  ! - number of events -
   Integer :: indx ! - rank -
!
   Real(Kind=rp) :: dhit ! - incremental hit rate -
   Real(Kind=rp) :: dfar ! - incremental false alarm rate -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Nint, Real
!
! Executable Statements
!
! Calculate number of events
   nes = Count(iobs(1:n) == ig)
!
! Initialise
   hit(1:n) = zero
   far(1:n) = zero
   If (nes > 0) Then
      dhit = one/Real(nes, Kind=rp)
   Else
      dhit = zero
   End If
   If (nes < n) Then
      dfar = one/Real(n-nes, Kind=rp)
   Else
      dfar = zero
   End If
   roca = zero
!
! Calculate hit and false alarm rates
   Select Case (ig)
    Case (1)
      Do k = 1, n
         indx = Nint(rnks(k))
         If (iobs(k) == ig) Then ! - hit -
            hit(indx:n) = hit(indx:n) + dhit
         Else                    ! - false alarm -
            far(indx:n) = far(indx:n) + dfar
         End If
      End Do
    Case (ng)
      Do k = 1, n
         indx = n + 1 - Nint(rnks(k))
         If (iobs(k) == ig) Then ! - hit -
            hit(indx:n) = hit(indx:n) + dhit
         Else                    ! - false alarm -
            far(indx:n) = far(indx:n) + dfar
         End If
      End Do
   End Select
!
! Calculate skill area
   roca = half*hit(1)*far(1)
   Do k = 2, n
      roca = roca + half*(hit(k-1) + hit(k))*(far(k) - far(k-1))
   End Do
!
   Return
 End Subroutine roc
!
!
!
 Subroutine rocp (m, nf, ng, nb, iobs, fps, roca, hit, far)
!
! Calculates ROC given probabilistic forecasts.
!
! Modules
   Use data_numbers, Only: zero, half, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m  ! - number of locations -
   Integer, Intent(In) :: nf ! - number of forecasts -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: nb ! - number of probability bins -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,nf) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,nf,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: roca ! - ROC area; shape(ng) -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: hit ! - hit rates; shape(nb,ng) -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: far ! - false-alarm rates; shape(nb,ng) -
!
! Locals
!
! Local scalars
   Integer :: i   ! - category index -
   Integer :: j   ! - probability threshold index -
   Integer :: k   ! - forecast index -
   Integer :: l   ! - location index -
   Integer :: nfa ! - number of non-missing forecasts -
   Integer :: nev ! - number of events -
!
   Real(Kind=rp) :: pbt ! - number of probability bin thresholds -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Nint, Real
!
! Executable Statements
!
! Count number of non-missing forecasts
   nfa = Count(fps(:,:,1) >= zero)
!
! Calculate scaling factor
   Call ps_scaling (nf, m, fps(:,:,:), pbt)
   pbt = Real(nb-1, Kind=rp)/pbt
!
! Calculate ROC
   Do i = 1, ng
      nev = 0                                        ! - calculate number of events -
      Do k = 1, nf
         Do l = 1, m
            If (fps(l,k,i) >= zero) Then
               If (iobs(l,k) == i) nev = nev + 1
            End If
         End Do
      End Do
      If (nev > 0 .and. nev < nfa) Then
         far(1:nb,i) = zero
         hit(1:nb,i) = zero
         Do k = 1, nf                                ! - repeat for each forecast -
            Do l = 1, m
               If (fps(l,k,i) >= zero) Then
                  j = nb - Nint(fps(l,k,i)*pbt)      ! - identify probability bin -
                  If (iobs(l,k) == i) Then
                     hit(j:nb,i) = hit(j:nb,i) + one ! - accumulate hit rates -
                  Else
                     far(j:nb,i) = far(j:nb,i) + one ! - accumulate false-alarm rates -
                  End If
               End If
            End Do
         End Do
         hit(1:nb,i) = hit(1:nb,i)/Real(nev, Kind=rp)
         far(1:nb,i) = far(1:nb,i)/Real(nfa-nev, Kind=rp)
!
! Calculate skill area
         roca(i) = half*hit(1,i)*far(1,i)
         Do j = 2, nb
            roca(i) = roca(i) + half*(hit(j-1,i) + hit(j,i))*(far(j,i) - far(j-1,i))
         End Do
!
! No events / non-events
      Else
         Do j = 1, nb
            hit(j,i) = Real(j-1, Kind=rp)/Real(nb-1, Kind=rp)
            far(j,i) = hit(j,i)
         End Do
         roca(i) = half
      End If
   End Do
!
   Return
 End Subroutine rocp
!
!
!
 Subroutine calc_scores (n, x, y, ifor, iobs, cps, score, ifail, &
            ornk)
!
! Calculates validation performance measures
!
! Modules
   Use data_numbers, Only: oneh
   Use arrays,       Only: rnky=>rwk, &
                           rank_data
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecast categories; shape(n) -
   Integer, Dimension(:), Intent(In) :: iobs ! - observed categories; shape(n) -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: x   ! - forecasts; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observations; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: cps ! - climatological probabilities; shape(ng) -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: score ! - scores; shape(nscore) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: ornk ! - observed ranks; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - score index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Sqrt
!
! Executable Statements
!
! Calculate ranks
   ifail = -1
! - observations -
   If (Present(ornk)) Then
      rnky(1:n) = ornk(1:n)
   Else
      Call rank_data ('a', n, y(:), rnky(:))
   End If
! - forecasts -
   Call rank_data ('a', n, x(:), rnkf(:))
!
! Calculate scores
! - Pearson's correlation -
   i = 1
   score(i) = p_corr(n, x(:), y(:))
! - Spearman's correlation -
   i = i + 1
   score(i) = s_corr(n, rnkf(:), rnky(:))
! - 2AFC -
   i = i + 1
   score(i) = two_afc_cc(n, y(:), x(:))
! - % variance -
   i = i + 1
   score(i) = oneh*score(1)**2
! - variance ratio -
   i = i + 1
   score(i) = var_ratio(n, x(:), y(:))
! - mean bias -
   i = i + 1
   score(i) = mbias(n, y(:), x(:))
! - root mean squared-error -
   i = i + 1
   score(i) = Sqrt(mserror(n, y(:), x(:)))
! - mean absolute error -
   i = i + 1
   score(i) = maerror(n, y(:), x(:))
! - hit and hit skill scores -
   i = i + 1
   Call heidke (n, ng, iobs(:), ifor(:), cps(:), score(i), score(i+1))
! - LEPS scores -
   i = i + 2
   Call leps_coeffs (ng, cps(:), cs(:,:))
   score(i) = lepscat(n, iobs(:), ifor(:), cs(:,:))
! - Gerrity score -
   i = i + 1
   Call gerrity_coeffs (ng, n, iobs(:), gs(:,:))
   score(i) = gerrity(n, iobs(:), ifor(:), gs(:,:))

! - 2AFC (categorical forecasts) -
   i = i + 1
   score(i) = two_afc_mm(n, ng, iobs(:), ifor(:))
! - 2AFC (continuous forecasts) -
   i = i + 1
   score(i) = two_afc_mc(n, ng, iobs(:), x(:))
! - ROC areas -
   i = i + 1
   Call roc (n, 1, iobs(:), rnkf(:), score(i), hit(1:n,1), far(1:n,1))
   roca(1) = score(i)
   i = i + 1
   Call roc (n, ng, iobs(:), rnkf(:), score(i), hit(1:n,2), far(1:n,2))
   roca(2) = score(i)
   ifail = 0
!
   Return
 End Subroutine calc_scores
!
!
!
 Subroutine calc_pscores (n, m, nb, ibs, iobs, cps, fps, rmiss, pscores, ifail)
!
! Calculates probabilistic verification scores
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: npall, npcat
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: m   ! - number of variables -
   Integer, Intent(In) :: nb  ! - number of probability bins -
   Integer, Intent(In) :: ibs ! - bin forecast probabilities? no if ibs  ==  0, yes if ibs  ==  1 -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: pscores ! - scores -
!
! Locals
!
! Local scalars
   Integer :: i ! - score index -
   Integer :: j ! - category index -
!
! Executable Statements
!
! Calculate all-category scores
   ifail = -1
! - all-category ignorance score -
   i = 1
   Call ignorance (n, m, nb, 0, ibs, iobs(:,:), fps(:,:,:), rmiss, pscores(i))
! - effective interest rate -
   i = i + 1
   pscores(i) = effective_interest(n, m, iobs(:,:), fps(:,:,:), cps(:,:), rmiss)
! - RPS and RPSS -
   i = i + 1
   Call ranked_prob_score (n, m, ng, ibs*nb, iobs(:,:), fps(:,:,:), cps(:,:), rmiss, pscores(i), pscores(i+1))
! - resolution slope -
   i = i + 2
   If (init_attrib() /= 0) Return
   Call calc_rel (n, m, ng, nb, iobs(:,:), fps(:,:,:), rmiss, ifq(:,:), afp(:,:), orf(:,:), tends(:,:), b0s(:), b1s(:))
   pscores(i) = b1s(0)
! - 2AFC -
   i = i + 1
   pscores(i) = two_afc_mp(n, m, ng, iobs(:,:), fps(:,:,:), rmiss)
! - linear probability score -
   i = i + 1
   pscores(i) = linear_prob(n, m, iobs(:,:), fps(:,:,:), rmiss)
! - ranked hits -
   i = i + 1
   Call ranked_hits (n, m, ng, iobs(:,:), fps(:,:,:), rmiss, pscores(i:))
!
! Calculate category-specific scores
! - ignorance score and its decomposition -
   Do j = 1, ng
      Call ignorance (n, m, nb, j, ibs, iobs(:,:), fps(:,:,:), rmiss, pscores(npall+npcat*(j-1)+1), &
           rel=pscores(npall+npcat*(j-1)+2), res=pscores(npall+npcat*(j-1)+3))
   End Do
! - Brier scores -
   Call hbrier (n, m, ng, nb, ibs, iobs(:,:), fps(:,:,:), cps(:,:), rmiss, pscores(npall+4:npall+npcat*(ng-1)+4:npcat), &
        pscores(npall+5:npall+npcat*(ng-1)+5:npcat), &
        rel=pscores(npall+6:npall+npcat*(ng-1)+6:npcat), res=pscores(npall+7:npall+npcat*(ng-1)+7:npcat))
! - resolution slopes -
   pscores(npall+8:npall+npcat*(ng-1)+8:npcat) = b1s(1:ng)
! - ROC areas -
   Call two_afc_2p (n, m, ng, iobs(:,:), fps(:,:,:), rmiss, pscores(npall+9:npall+npcat*(ng-1)+9:npcat), &
        nb=nb, ibs=ibs)
   ifail = 0
!
   Return
 End Subroutine calc_pscores
!
!
!
 Subroutine calc_rel (n, m, ng, nb, iobs, fps, rmiss, ifq, afp, orf, tends, b0s, b1s)
!
! Calculates results for a reliability diagram
!
! Modules
   Use data_numbers, Only: zero, one, oneh, eps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of forecasts -
   Integer, Intent(In) :: m  ! - number of gridpoints/stations -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: nb ! - number of probability bins -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(n) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output arrays -
   Integer, Dimension(:,0:), Intent(Out) :: ifq ! - probability bin frequencies; shape(nb,0:ng) -
!
   Real(Kind=rp), Dimension(0:), Intent(Out) :: b0s ! - intercepts -
   Real(Kind=rp), Dimension(0:), Intent(Out) :: b1s ! - slopes -
!
   Real(Kind=rp), Dimension(:,0:), Intent(Out) :: afp   ! - average bin probability; shape(nb,0:ng) -
   Real(Kind=rp), Dimension(:,0:), Intent(Out) :: orf   ! - observed relative frequencies; shape(nb,0:ng) -
   Real(Kind=rp), Dimension(:,0:), Intent(Out) :: tends ! - tendencies; shape (2,0:ng) -
!
! Locals
!
! Local scalars
   Integer :: ic ! - category index -
   Integer :: ib ! - probability bin index -
   Integer :: j  ! - gridpoint/station index -
   Integer :: k  ! - forecast index -
!
   Real(Kind=rp) :: df  ! - number of non-missing cases -
   Real(Kind=rp) :: pbt ! - number of probability bin thresholds -
!
! Local arrays
   Real(Kind=rp), Dimension(0:ng) :: xbar ! - mean -
   Real(Kind=rp), Dimension(0:ng) :: ybar ! - mean -
   Real(Kind=rp), Dimension(0:ng) :: sxx  ! - sum of squares of forecast probabilities -
   Real(Kind=rp), Dimension(0:ng) :: sxy  ! - sum of cross-products -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Dot_Product, Nint, Real, Sum
!
! Executable Statements
!
! Initialise arrays
   xbar(:) = zero
   ybar(:) = zero
   sxx(:) = zero
   sxy(:) = zero
   afp(:,0:) = zero
   orf(:,0:) = zero
   ifq(:,0:) = 0
!
! Calculate scaling factor for identifying probability bin
   Call ps_scaling (n, m, fps(:,:,:), pbt)
   If (pbt < eps) Then
      b1s(0) = rmiss
      b0s(0) = rmiss
      Return
   End If
   pbt = Real(nb-1, Kind=rp)/pbt
!
! Calculate sample conditional relative frequencies
   Do ic = 1, ng                                                  ! - repeat for each category -
      Do k = 1, n                                                 ! - repeat for each forecast -
         Do j = 1, m                                              ! - repeat for each gridpoint/station -
            If (fps(j,k,ic) >= zero) Then
               ib = 1 + Nint(fps(j,k,ic)*pbt)                     ! - identify probability bin -
               afp(ib,ic) = afp(ib,ic) + fps(j,k,ic)              ! - calculate average forecast probability in each bin -
               ifq(ib,ic) = ifq(ib,ic) + 1                        ! - count frequency for each bin -
               If (iobs(j,k) == ic) orf(ib,ic) = orf(ib,ic) + one ! - count observed frequencies -
            End If
         End Do
      End Do
      Do ib = 1, nb                                               ! - repeat for each probability bin -
         If (ifq(ib,ic) > 0) Then
            df = Real(ifq(ib,ic), Kind=rp)
            orf(ib,ic) = oneh*orf(ib,ic)/df                       ! - calculate observed relative frequencies -
            afp(ib,ic) = afp(ib,ic)/df                            ! - calculate average forecast probability in each bin -
         Else
            orf(ib,ic) = -one
            afp(ib,ic) = Real(ib-1, Kind=rp)/pbt
         End If
      End Do
   End Do
!
! Calculate average
   ifq(:,0) = Sum(ifq(:,1:ng), Dim=2)
   Do ib = 1, nb
      If (ifq(ib,0) > 0) Then
         df = Real(ifq(ib,0), Kind=rp)
         afp(ib,0) = Dot_Product(afp(ib,1:ng), ifq(ib,1:ng))/df
         orf(ib,0) = Dot_Product(orf(ib,1:ng), ifq(ib,1:ng))/df
      Else
         afp(ib,0) = Real(ib-1, Kind=rp)/pbt
         orf(ib,0) = -one
      End If
   End Do
!
! Calculate tendencies
   tends(1,0) = oneh/Real(ng, Kind=rp)
   tends(2,0) = tends(1,0)
   Do ic = 1, ng
      df = Real(Sum(ifq(1:nb,ic)), Kind=rp)
      If (df > zero) Then
         tends(1,ic) = Sum(orf(1:nb,ic)*ifq(1:nb,ic))/df
         tends(2,ic) = Sum(afp(1:nb,ic)*ifq(1:nb,ic))/df
      Else
         tends(1,ic) = zero
         tends(2,ic) = zero
      End If
   End Do
!
! Calculate regression fit
   df = Real(Sum(ifq(:,0)), Kind=rp)
   If (df > zero) Then
      xbar(0) = Dot_Product(afp(:,0), Real(ifq(:,0), Kind=rp))/df
      ybar(0) = Dot_Product(orf(:,0), Real(ifq(:,0), Kind=rp))/df
      Do ic=1,ng
         df = Real(Sum(ifq(:,ic)), Kind=rp)
         xbar(ic) = Dot_Product(afp(:,ic), Real(ifq(:,ic), Kind=rp))/df
         ybar(ic) = Dot_Product(orf(:,ic), Real(ifq(:,ic), Kind=rp))/df
         Do k = 1, n
            Do j = 1, m
               If (fps(j,k,ic) >= zero) Then
                  sxx(0) = sxx(0) + (fps(j,k,ic) - xbar(0))**2
                  sxx(ic) = sxx(ic) + (fps(j,k,ic) - xbar(ic))**2
                  If (iobs(j,k) == ic) Then
                     sxy(0) = sxy(0) + (fps(j,k,ic) - xbar(0))*(oneh - ybar(0))
                     sxy(ic) = sxy(ic) + (fps(j,k,ic) - xbar(ic))*(oneh - ybar(ic))
                  Else
                     sxy(0) = sxy(0) - (fps(j,k,ic) - xbar(0))*ybar(0)
                     sxy(ic) = sxy(ic) - (fps(j,k,ic) - xbar(ic))*ybar(ic)
                  End If
               End If
            End Do
         End Do
         If (sxx(ic) > eps) Then
            b1s(ic) = sxy(ic)/sxx(ic)
            b0s(ic) = ybar(ic) - b1s(ic)*xbar(ic)
         Else
            b1s(ic) = zero
            b0s(ic) = ybar(ic)
         End If
      End Do
      If (sxx(0) > eps) Then
         b1s(0) = sxy(0)/sxx(0)
         b0s(0) = ybar(0) - b1s(0)*xbar(0)
      Else
         b1s(0) = zero
         b0s(0) = ybar(0)
      End If
   Else
      b1s(0) = rmiss
      b0s(0) = rmiss
   End If
!
   Return
 End Subroutine calc_rel
!
!
!
 Subroutine ps_scaling (n, m, fps, fpss, &
            cps, cpss)
!
! Checks scaling of probabilities
!
! Modules
   Use data_numbers, Only: zero
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of forecasts -
   Integer, Intent(In) :: m ! - number of gridpoints/stations -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: fpss ! - forecast probability scaling -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: cps ! - constant probabilities for skill score; shape(m,ng) -
!
! - optional output scalars -
   Real(Kind=rp), Intent(Out), Optional :: cpss ! - climatological probability scaling -
!
! Locals
!
! Local scalars
   Integer :: j ! - location index -
   Integer :: k ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Sum
!
! Executable Statements
!
! Calculate probability scaling
   fpss = zero
   If (Present(cpss)) cpss = zero
   search: Do j = 1, m
      Do k = 1, n
         If (fps(j,k,1) >= zero) Then
            fpss = Sum(fps(j,k,:))
            If (Present(cpss)) cpss = Sum(cps(j,:))
            Exit search
         End If
      End Do
   End Do search
!
   Return
 End Subroutine ps_scaling
!
!
!
 Subroutine hbrier (n, m, ng, nb, ibs, iobs, fps, cps, rmiss, bs, bss, &
            rel, res)
!
! Calculates half-Brier score and skill score against a fixed probability
!
! Modules
   Use data_numbers, Only: zero, one, oneh
   Use maths,        Only: bin_prob
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of forecasts -
   Integer, Intent(In) :: m   ! - number of gridpoints/stations -
   Integer, Intent(In) :: ng  ! - number of categories -
   Integer, Intent(In) :: nb  ! - number of probability bins -
   Integer, Intent(In) :: ibs ! - bin forecast probabilities? no if ibs  ==  0, yes if ibs  ==  1 -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed data; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - constant probabilities for skill score; shape(m,ng) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: bs  ! - Brier score; shape(ng) -
   Real(Kind=rp), Dimension(:), Intent(Out) :: bss ! - Brier skill score; shape(ng) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rel ! - reliability component -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: res ! - resolution component -
!
! Locals
!
! Local scalars
   Integer :: ic   ! - current category -
   Integer :: ibin ! - current probability bin -
   Integer :: j    ! - gridpoint/station index -
   Integer :: k    ! - case index -
!
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
   Real(Kind=rp) :: cpss ! - climatological probability scaling -
   Real(Kind=rp) :: pbt  ! - number of probability bin thresholds -
   Real(Kind=rp) :: p    ! - probability -
   Real(Kind=rp) :: ave  ! - average -
!
! Local arrays
   Integer, Dimension(0:nb) :: nn ! - number of non-missing forecasts -
!
   Real(Kind=rp), Dimension(nb) :: cobs ! - conditional average observations -
   Real(Kind=rp), Dimension(nb) :: cfor ! - conditional average forecasts -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Present, Real
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss, &
        cps=cps, cpss=cpss)
   If (fpss == zero) Then
      bs(:) = rmiss
      bss(:) = rmiss
      If (Present(rel)) rel(:) = rmiss
      If (Present(res)) res(:) = rmiss
      Return
   End If
!
! Initialise
   If (Present(rel) .or. Present(res)) Then
      pbt = Real(nb-1, Kind=rp)/fpss
      cobs(:) = zero
      cfor(:) = zero
      If (Present(rel)) rel(:) = zero
      If (Present(res)) res(:) = zero
   End If
!
! Calculate squared probability error
   Do ic = 1, ng
      bs(ic) = zero
      bss(ic) = zero
      nn(:) = 0
      If (Present(rel) .or. Present(res)) Then
         cobs(:) = zero
         cfor(:) = zero
      End If
      Do k = 1, n
         Do j = 1, m
            If (fps(j,k,ic) >= zero) Then
               nn(0) = nn(0) + 1
               p=bin_prob(ibs*nb, fps(j,k,ic)/fpss)
               If (iobs(j,k) == ic) Then
                  bs(ic) = bs(ic) + (p - one)**2
                  bss(ic) = bss(ic) + (cps(j,ic)/cpss - one)**2
               Else
                  bs(ic) = bs(ic) + p**2
                  bss(ic) = bss(ic) + (cps(j,ic)/cpss)**2
               End If
               If (Present(rel) .or. Present(res)) Then
                  ibin = 1 + Nint(fps(j,k,ic)*pbt) ! - identify probability bin -
                  nn(ibin) = nn(ibin)+1
                  cfor(ibin) = cfor(ibin)+p
                  If (iobs(j,k) == ic) cobs(ibin) = cobs(ibin)+one
               End If
            End If
         End Do
      End Do
      bs(ic) = bs(ic)/Real(nn(0), Kind=rp)
      bss(ic) = bss(ic)/Real(nn(0), Kind=rp)
!
! Calculate skill
      If (bss(ic) > zero) Then
         If (bss(ic) > bs(ic))  Then
            bss(ic) = one - bs(ic)/bss(ic)
         Else
            bss(ic) = (bss(ic) - bs(ic))/(one - bss(ic))
         End If
      Else
         bss(ic) = (bss(ic) - bs(ic))/(one - bss(ic))
      End If
      If (Present(rel) .or. Present(res)) Then
         If (nn(0) > 0) Then
            Where (nn(1:) > 0)
               cfor(:) = cfor(:)/nn(1:)
               cobs(:) = cobs(:)/nn(1:)
            End Where
            If (Present(rel)) Then
               rel(ic) = zero
               Do j = 1, nb
                  rel(ic) = rel(ic) + Real(nn(j), Kind=rp)*(cobs(j) - cfor(j))**2
               End Do
               rel(ic) = rel(ic)/Real(nn(0), Kind=rp)
            End If
            If (Present(res)) Then
               ave = zero
               Do j = 1, nb
                  ave = ave + Real(nn(j), Kind=rp)*cobs(j)
               End Do
               ave = ave/Real(nn(0), Kind=rp)
               res(ic) = zero
               Do j = 1, nb
                  res(ic) = res(ic) + Real(nn(j), Kind=rp)*(cobs(j) - ave)**2
               End Do
               res(ic) = res(ic)/Real(nn(0), Kind=rp)
            End If
         End If
      End If
   End Do
   bs(:) = bs(:)*oneh
   bss(:) = bss(:)*oneh
!
   Return
 End Subroutine hbrier
!
!
!
 Subroutine ranked_prob_score (n, m, ng, nbs, iobs, fps, cps, rmiss, rps, rpss)
!
! Calculates ranked probability score and skill score
!
! Modules
   Use data_numbers, Only: zero, one, oneh
   Use maths,        Only: bin_prob
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of forecasts -
   Integer, Intent(In) :: m   ! - number of gridpoints/stations -
   Integer, Intent(In) :: ng  ! - number of categories -
   Integer, Intent(In) :: nbs ! - number of  forecast probability bins? nbs  ==  0 probabilities are not to be binned -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed data; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - constant probabilities for skill score; shape(m,ng) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: rps  ! - ranked probability score -
   Real(Kind=rp), Intent(Out) :: rpss ! - ranked probability skill score -
!
! Locals
!
! Local scalars
   Integer :: ic  ! - current category -
   Integer :: j   ! - gridpoint/station index -
   Integer :: k   ! - case index -
   Integer :: nts ! - number of thresholds -
   Integer :: nn  ! - number of non-missing forecasts -
!
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
   Real(Kind=rp) :: cpss ! - climatological probability scaling -
   Real(Kind=rp) :: cpo  ! - cumulative probability of observations -
   Real(Kind=rp) :: cpf  ! - cumulative probability of forecasts -
   Real(Kind=rp) :: cpc  ! - cumulative probability of constant forecasts -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss, &
        cps=cps, cpss=cpss)
   If (fpss == zero) Then
      rps = rmiss
      rpss = rmiss
       Return
   End If
!
! Calculate cumulative probabilities
   nts = ng - 1
   nn = 0
   rps = zero
   rpss = zero
   Do k = 1, n
      Do j = 1, m
         cpo = zero
         cpf = zero
         cpc = zero
         Do ic = 1, nts
            If (fps(j,k,ic) >= zero) Then
               nn = nn + 1
               If (iobs(j,k) == ic) cpo = one
               cpf = cpf + bin_prob(nbs, fps(j,k,ic)/fpss)
               cpc = cpc + cps(j,ic)/cpss
!
! Calculate squared probability errors
               rps = rps + (cpf - cpo)**2
               rpss = rpss + (cpc - cpo)**2
            End If
         End Do
      End Do
   End Do
   rps = rps/Real(nn, Kind=rp)
   rpss = rpss/Real(nn, Kind=rp)
!
! Calculate skill
   If (rpss > zero) Then
      If (rpss > rps)  Then
         rpss = one - rps/rpss
      Else
         rpss = (rpss - rps)/(one - rpss)
      End If
   Else
      rpss = (rpss - rps)/(one - rpss)
   End If
   rps = rps*oneh
   rpss = rpss*oneh
!
   Return
 End Subroutine ranked_prob_score
!
!
!
 Subroutine ignorance (n, m, nb, icat, ibs, iobs, fps, rmiss, ign, &
            rel, res)
!
! Calculates ignorance score
!
! Modules
   Use data_numbers, Only: zero, one, ln2, rhuge
   Use maths,        Only: bin_prob
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n    ! - number of forecasts -
   Integer, Intent(In) :: m    ! - number of gridpoints/stations -
   Integer, Intent(In) :: nb   ! - number of probability bins -
   Integer, Intent(In) :: icat ! - category to verify (0 implies all; if icat  ==  0 res and rel are not calculated) -
   Integer, Intent(In) :: ibs  ! - bin forecast probabilities? no if ibs  ==  0, yes if ibs  == 1 -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: ign
!
! Optional arguments
! - optional output scalars -
   Real(Kind=rp), Intent(Out), Optional :: rel ! - reliability component -
   Real(Kind=rp), Intent(Out), Optional :: res ! - resolution component -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
   Integer :: j ! - bin index -
   Integer :: k ! - forecast index -
!
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
   Real(Kind=rp) :: pbt  ! - number of probability bin thresholds -
   Real(Kind=rp) :: p    ! - probability -
   Real(Kind=rp) :: ave  ! - average -
!
! Local arrays
   Integer, Dimension(0:nb) :: nn ! - number of non-missing forecasts -
!
   Real(Kind=rp), Dimension(nb) :: cobs ! - conditional average observations -
   Real(Kind=rp), Dimension(nb) :: cfor ! - conditional average forecasts -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Nint, Present, Real
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss)
   If (fpss == zero) Then
      ign = rmiss
      If (Present(rel)) rel = rmiss
      If (Present(res)) res = rmiss
      Return
   End If
!
! Initialise
   pbt = Real(nb-1, Kind=rp)/fpss
   If (Present(rel) .or. Present(res)) Then
      cobs(:) = zero
      cfor(:) = zero
      If (Present(rel)) rel = zero
      If (Present(res)) res = zero
   End If
!
! Calculate ignorance
   ign = zero
   nn(:) = 0
   Do k = 1, n
      Do i = 1, m
         If (iobs(i,k) > 0) Then
            If (fps(i,k,iobs(i,k)) >= zero) Then
               p = bin_prob(ibs*nb, fps(i,k,iobs(i,k))/fpss)
               If (p <= zero) Cycle
               nn(0) = nn(0) + 1
               If (icat == 0 .or. iobs(i,k) == icat) Then
                  ign = ign - Log(p)/ln2
               Else If (p < one) Then
                  ign = ign - Log(one - p)/ln2
               Else
                  ign = rhuge
               End If
               If (Present(rel) .or. Present(res)) Then
                  j = 1 + Nint(fps(i,k,iobs(i,k))*pbt) ! - identify probability bin -
                  nn(j) = nn(j) + 1
                  cfor(j) = cfor(j) + p
                  If (iobs(i,k) == icat) cobs(j) = cobs(j) + one
               End If
            End If
         End If
      End Do
   End Do
   If (nn(0) > 0) Then
      ign = ign/Real(nn(0), Kind=rp)
      If (Present(rel) .or. Present(res)) Then
         Where (nn(1:) > 0)
            cfor(:) = cfor(:)/nn(1:)
            cobs(:) = cobs(:)/nn(1:)
         End Where
         If (Present(rel)) Then
            rel = zero
            Do j = 1, nb
               If (      cfor(j) > zero .and.       cobs(j) > zero) &
                   rel = rel - Real(nn(j), Kind=rp)*Log(cobs(j)/cfor(j))/ln2
               If (one - cfor(j) > zero .and. one - cobs(j) > zero) &
                   rel = rel - Real(nn(j), Kind=rp)*Log((one - cobs(j))/(one - cfor(j)))/ln2
            End Do
            rel = rel/Real(nn(0), Kind=rp)
         End If
         If (Present(res)) Then
            ave = zero
            Do j = 1, nb
               ave = ave + Real(nn(j), Kind=rp)*cobs(j)
            End Do
            ave = ave/Real(nn(0), Kind=rp)
            res = zero
            Do j = 1, nb
               If (    cobs(j) > zero .and.       ave > zero) &
                   res = res + Real(nn(j), Kind=rp)*Log(cobs(j)/ave)/ln2
               If (one-cobs(j) > zero .and. one - ave > zero) &
                   res = res + Real(nn(j), Kind=rp)*Log((one - cobs(j))/(one - ave))/ln2
            End Do
            res = res/Real(nn(0), Kind=rp)
         End If
      End If
   End If
!
   Return
 End Subroutine ignorance
!
!
!
 Subroutine profits (n, m, iobs, fps, cps, rmiss, prft, cump, eir)
!
! Calculates cumulative profits
!
! Modules
   Use data_numbers, Only: zero, one, oneh
   Use arrays,       Only: cp=>rwk
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of forecasts -
   Integer, Intent(In) :: m ! - number of gridpoints/stations -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities; shape(m,max(iobs)) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(0:), Intent(Out) :: prft ! - profits; shape(0:n) -
   Real(Kind=rp), Dimension(0:), Intent(Out) :: cump ! - cumulative profits; shape(0:n) -
   Real(Kind=rp), Dimension(0:), Intent(Out) :: eir  ! - effective interest rates; shape(0:n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
   Integer :: k ! - forecast index -
!
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
   Real(Kind=rp) :: cpss ! - climatological probability scaling -
   Real(Kind=rp) :: ret  ! - return -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss, &
        cps=cps, cpss=cpss)
   If (fpss == zero) Then
      prft(0:) = rmiss
      cump(0:) = rmiss
      eir(0:) = rmiss
      Return
   End If
!
! Calculate cumulative profits
   prft(0:) = zero
   cump(0) = zero
   eir(0) = zero
   cp(1:m) = one
   Do k = 1, n
      Do i = 1, m
         If (iobs(i,k) > 0) Then
            If (fps(i,k,iobs(i,k)) >= zero) Then
               ret = (fps(i,k,iobs(i,k))/fpss)/(cps(i,iobs(i,k))/cpss)
               cp(i) = cp(i)*ret
            Else
               ret = one
            End If
         Else
            ret = one
         End If
         prft(k) = prft(k) + ret
      End Do
      prft(k) = prft(k)/Real(m, Kind=rp) - one
      cump(k) = Sum(cp(1:m))/Real(m, Kind=rp) - one
!
! Calculate effective interest rates
      eir(k) = (((cump(k) + one)**(one/Real(k, Kind=rp))) - one)*oneh
   End Do
!
   Return
 End Subroutine profits
!
!
!
 Function effective_interest(n, m, iobs, fps, cps, rmiss)
!
! Calculates effective interest rate
!
! Modules
   Use data_numbers, Only: zero, one, oneh
!
! Function result
   Real(Kind=rp) :: effective_interest
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of forecasts -
   Integer, Intent(In) :: m ! - number of gridpoints/stations -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities; shape(m,max(iobs)) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
   Integer :: k ! - forecast index -
!
   Real(Kind=rp) :: cump ! - cumulative profits -
   Real(Kind=rp) :: cp   ! - cumulative profit -
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
   Real(Kind=rp) :: cpss ! - climatological probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss, &
        cps=cps, cpss=cpss)
   If (fpss == zero) Then
      effective_interest = rmiss
      Return
   End If
!
! Calculate cumulative profits
   cump = zero
   Do i = 1, m
      cp = one
      Do k = 1, n
         If (iobs(i,k) > 0) Then
            If (fps(i,k,iobs(i,k)) >= zero) Then
               cp = cp*(fps(i,k,iobs(i,k))/fpss)/(cps(i,iobs(i,k))/cpss)
            End If
         End If
      End Do
      cump = cump + cp
   End Do
   cump = cump/Real(m, Kind=rp)
!
! Calculate effective interest rates
   effective_interest = ((cump**(one/Real(n, Kind=rp))) - one)*oneh
!
   Return
 End Function effective_interest
!
!
!
 Function linear_prob(n, m, iobs, fps, rmiss)
!
! Calculates linear probability score
!
! Modules
   Use data_numbers, Only: zero, oneh
!
! Function result
   Real(Kind=rp) :: linear_prob
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of forecasts -
   Integer, Intent(In) :: m ! - number of gridpoints/stations -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - location index -
   Integer :: k  ! - forecast index -
   Integer :: nn ! - number of non-missing forecasts -
!
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss)
   If (fpss == zero) Then
      linear_prob = rmiss
      Return
   End If
!
! Calculate cumulative profits
   linear_prob = zero
   nn=0
   Do k = 1, n
      Do i = 1, m
         If (fps(i,k,iobs(i,k)) >= zero) Then
            nn = nn + 1
            linear_prob = linear_prob + fps(i,k,iobs(i,k))/fpss
         End If
      End Do
   End Do
   linear_prob = oneh*linear_prob/Real(nn, Kind=rp)
!
   Return
 End Function linear_prob
!
!
!
 Subroutine ranked_hits (n, m, ng, iobs, fps, rmiss, rhits)
!
! Calculates ranked hits score
!
! Modules
   Use data_numbers, Only: zero, one, oneh, tol
   Use maths,        Only: approx
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of forecasts -
   Integer, Intent(In) :: m  ! - number of gridpoints/stations -
   Integer, Intent(In) :: ng ! - number of categories -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: rhits ! - ranked hits; shape(ng) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - location index -
   Integer :: j  ! - category index -
   Integer :: k  ! - forecast index -
   Integer :: nn ! - number of non-missing forecasts -
   Integer :: nl ! - number of larger probabilities -
   Integer :: ns ! - number of smaller probabilities -
   Integer :: ne ! - number of ties -
!
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Real
!
! Executable Statements
!
! Calculate probability scaling
   Call ps_scaling (n, m, fps(:,:,:), fpss)
   If (fpss == zero) Then
      rhits(:) = rmiss
      Return
   End If
!
! Calculate cumulative profits
   rhits(:) = zero
   nn = 0
   Do i = 1, m
      Do k = 1, n
         If (fps(i,k,iobs(i,k)) >= zero) Then
            nn = nn + 1
            nl = Count(fps(i,k,:) > fps(i,k,iobs(i,k))-tol)
            ns = Count(fps(i,k,:) < fps(i,k,iobs(i,k))-tol)
            ne = ng - nl - ns
            If (ne == 0) Then
               j = nl
               rhits(j) = rhits(j) + one
            Else
               Do j = 1, ng 
                  If (approx(fps(i,k,j), fps(i,k,iobs(i,k)), utol=tol)) rhits(:) = rhits(:) + one/Real(ne, Kind=rp)
               End Do
            End If
         End If
      End Do
   End Do
   rhits(:) = oneh*rhits(:)/Real(nn, Kind=rp)
!
   Return
 End Subroutine ranked_hits
!
!
!
 Subroutine two_afc_2p (n, m, ng, iobs, fps, rmiss, afc, &
            nb, ibs)
!
! Calculates 2AFC test score for dichotomous observations, discrete probabilistic forecasts
!
! Modules
   Use data_numbers, Only: zero, half, one, oneh
   Use maths,        Only: bin_prob
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: m  ! - number of locations -
   Integer, Intent(In) :: ng ! - number of forecast categories -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observations; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: afc ! - 2AFC scores; shape(ng) -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ibs ! - bin forecast probabilities? no if ibs  ==  0, yes if ibs  ==  1 -
   Integer, Intent(In), Optional :: nb  ! - number of probability bins -
!
! Locals
!
! Local scalars
   Integer :: i1  ! - first location -
   Integer :: i2  ! - second location -
   Integer :: io1 ! - category of first case -
   Integer :: io2 ! - category of second case -
   Integer :: j   ! - category index -
   Integer :: k1  ! - first case index -
   Integer :: k2  ! - second case index -
!
   Real(Kind=rp) :: p11  ! - probability -
   Real(Kind=rp) :: p21  ! - probability -
   Real(Kind=rp) :: p12  ! - probability -
   Real(Kind=rp) :: p22  ! - probability -
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
!
! Local arrays
   Integer, Dimension(ng) :: nn ! - total number of pairings -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real
!
! Executable Statements
!
! Calculate probability scaling
   If (Present(ibs) .and. Present(nb)) Then
      Call ps_scaling (n, m, fps(:,:,:), fpss)
      If (fpss == zero) Then
         afc(:) = rmiss
         Return
      End If
   End If
!
! Calculate 2afc test score
   afc(:) = zero
   nn(:) = 0
   Do i1 = 1, m
      Do k1 = 1, n
         If (fps(i1,k1,1) < zero) Cycle
         io1 = iobs(i1,k1)
         If (Present(ibs) .and. Present(nb)) Then
            p11 = bin_prob(ibs*nb, fps(i1,k1,io1)/fpss)
         Else
            p11 = fps(i1,k1,io1)
         End If
         Do i2 = i1, m
            Do k2 = 1, n
               If ((i2 == i1 .and. k2 >= k1) .or. fps(i2,k2,1) < zero) Cycle
               io2 = iobs(i2,k2)
               If (Present(ibs) .and. Present(nb)) Then
                  p21 = bin_prob(ibs*nb, fps(i2,k2,io1)/fpss)
                  p12 = bin_prob(ibs*nb, fps(i1,k1,io2)/fpss)
                  p22 = bin_prob(ibs*nb, fps(i2,k2,io2)/fpss)
               Else
                  p21 = fps(i2,k2,io1)
                  p12 = fps(i1,k1,io2)
                  p22 = fps(i2,k2,io2)
               End If
               If (io2 == io1) Cycle
               nn(io1) = nn(io1) + 1
               If (p11 > p21) Then
                  afc(io1) = afc(io1) + one
               Else If (.not.p11 < p21) Then
                  afc(io1) = afc(io1) + half
               End If
               nn(io2) = nn(io2) + 1
               If (p12 < p22) Then
                  afc(io2) = afc(io2) + one
               Else If (.not.p12 > p22) Then
                  afc(io2) = afc(io2) + half
               End If
            End Do
         End Do
      End Do
   End Do
   Do j = 1, ng
      If (nn(j) > 0) Then
         afc(j) = oneh*afc(j)/Real(nn(j), Kind=rp)
      Else
         afc(j) = rmiss
      End If
   End Do
!
   Return
 End Subroutine two_afc_2p
!
!
!
 Function two_afc_mm(n, ng, iobs, ifor) &
          Result (afc)
!
! Calculates 2AFC test score for polychotomous observations, polychotomous forecasts
!
! Modules
   Use data_numbers, Only: zero, half, fifty, oneh
!
! Function result
   Real(Kind=rp) :: afc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: ng ! - number of categories -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observations; shape(n) -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecasts; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - category index -
   Integer :: j  ! - category index -
   Integer :: k  ! - case index -
   Integer :: l  ! - case index -
   Integer :: ne ! - number of pairings -
!
! Local arrays
   Integer, Dimension(ng,0:ng) :: nn ! - numbers of forecasts per category -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Real, Sum
!
! Executable Statements
!
! Construct contingency table
   nn(:,:) = 0
   Do Concurrent (k = 1:n)
      nn(iobs(k),ifor(k)) = nn(iobs(k),ifor(k)) + 1
   End Do
   nn(:,0) = Sum(nn(:,1:), Dim=2)
!
! Calculate 2afc test score
   If (Count(nn(:,0) > 0) > 1) Then
      ne = 0
      afc = zero
      Do k = 1, ng-1
         Do l = k+1, ng
            ne = ne + nn(k,0)*nn(l,0)
            Do i = 1, ng-1
               Do j = i+1, ng
                  afc = afc + Real(nn(k,i)*nn(l,j), Kind=rp)
               End Do
               afc = afc + half*Real(nn(k,i)*nn(l,i), Kind=rp)
            End Do
            afc = afc + half*Real(nn(k,ng)*nn(l,ng), Kind=rp)
         End Do
      End Do
      afc = oneh*afc/Real(ne, Kind=rp)
   Else
      afc = fifty
   End If
!
   Return
 End Function two_afc_mm
!
!
!
 Function two_afc_mc(n, ng, iobs, for) &
          Result (afc)
!
! Calculates 2AFC test score for polychotomous observations, continuous forecasts
!
! Modules
   Use data_numbers, Only: zero, fifty, oneh
   Use arrays,       Only: rank_data
!
! Function result
   Real(Kind=rp) :: afc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: ng ! - number of categories -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observations; shape(n) -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: for ! - forecasts; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - case index -
   Integer :: k  ! - category index -
   Integer :: l  ! - category index -
   Integer :: ne ! - number of events -
   Integer :: nn ! - number of non-events -
   Integer :: nt ! - partial number of cases -
   Integer :: np ! - total number of pairings -
!
! Local arrays
   Real(Kind=rp), Dimension(n) :: f ! - partial copy of forecasts -
   Real(Kind=rp), Dimension(n) :: r ! - ranks -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Extract current categories
   np = 0
   afc = zero
   Do l = 2, ng
      ne = 0
      Do i = 1, n
         If (iobs(i) == l) Then ! - event -
            ne = ne + 1
            f(ne) = for(i)
         End If
      End Do
      Do k = 1, l-1
         nn = 0
         nt = ne
         Do i = 1, n
            If (iobs(i) == k) Then ! - non-event -
               nn = nn + 1
               nt = nt + 1
               f(nt) = for(i)
            End If
         End Do
!
! Rank forecasts
         Call rank_data ('a', nt, f(:), r)
!
! Calculate 2afc test score
         If (ne > 0 .and. nn > 0) Then
            afc = afc + Sum(r(1:ne)) - Real(ne*(ne+1)/2, Kind=rp)
            np = np + ne*nn
         End If
      End Do
   End Do
   If (np > 0) Then
      afc = oneh*afc/Real(np, Kind=rp)
   Else
      afc = fifty
   End If
!
   Return
 End Function two_afc_mc
!
!
!
 Function two_afc_mp_1(n, ng, iobs, fps, rmiss) &
          Result (afc)
!
! Calculates 2AFC test score for polychotomous observations, probabilistic forecasts
!
! Modules
   Use data_numbers, Only: zero, half, one, oneh
!
! Function result
   Real(Kind=rp) :: afc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: ng ! - number of categories -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observations; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: fps ! - forecast probabilities; shape(n,ng) -
!
! Locals
!
! Local scalars
   Integer :: io1 ! - category of first case -
   Integer :: io2 ! - category of second case -
   Integer :: k1  ! - first case index -
   Integer :: k2  ! - second case index -
   Integer :: l1  ! - first category index -
   Integer :: l2  ! - second category index -
   Integer :: nn  ! - total number of pairings -
!
   Real(Kind=rp) :: p    ! - probability -
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate probability scaling
   fpss = zero
   search: Do k1 = 1, n
      If (fps(k1,1) >= zero) Then
         fpss = Sum(fps(k1,:))
         Exit search
      End If
   End Do search
   If (fpss == zero) Then
      afc = rmiss
      Return
   End If
!
! Calculate 2afc test score
   afc = zero
   nn = 0
   Do k1 = 1, n-1
      If (fps(k1,1) < zero) Cycle
      io1 = iobs(k1)
      Do k2 = k1+1, n
         If (fps(k2,1) < zero) Cycle
         io2 = iobs(k2)
         If (io2 == io1) Cycle
         nn = nn + 1
         p = zero
         Do l1 = 1, ng-1
            Do l2 = l1+1, ng
               p = p + fps(k1,l1)*fps(k2,l2)/fpss**2
            End Do
         End Do
         If (p > half) Then
            If (io2 > io1) afc = afc + one
         Else If (p < half) Then
            If (io2 < io1) afc = afc + one
         Else
            afc = afc + half
         End If
      End Do
   End Do
   If (nn > 0) Then
      afc = oneh*afc/Real(nn, Kind=rp)
   Else
      afc = rmiss
   End If
!
   Return
 End Function two_afc_mp_1
!
!
!
 Function two_afc_mp_2(n, m, ng, iobs, fps, rmiss) &
          Result (afc)
!
! Calculates 2AFC test score for polychotomous observations, probabilistic forecasts
!
! Modules
   Use data_numbers, Only: zero, half, one, oneh
!
! Function result
   Real(Kind=rp) :: afc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: m  ! - number of locations -
   Integer, Intent(In) :: ng ! - number of categories -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observations; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! Locals
!
! Local scalars
   Integer :: i1  ! - first location -
   Integer :: i2  ! - second location -
   Integer :: io1 ! - category of first case -
   Integer :: io2 ! - category of second case -
   Integer :: k1  ! - first case index -
   Integer :: k2  ! - second case index -
   Integer :: l1  ! - first category index -
   Integer :: l2  ! - second category index -
   Integer :: nn  ! - total number of pairings -
!
   Real(Kind=rp) :: p    ! - probability -
   Real(Kind=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real,Sum
!
! Executable Statements
!
! Calculate probability scaling
   fpss = zero
   search: Do i1 = 1, m
      Do k1 = 1, n
         If (fps(i1,k1,1) >= zero) Then
            fpss = Sum(fps(i1,k1,:))
            Exit search
         End If
      End Do
   End Do search
   If (fpss == zero) Then
      afc = rmiss
      Return
   End If
!
! Calculate 2afc test score
   afc = zero
   nn = 0
   Do i1 = 1, m
      Do k1 = 1, n
         If (fps(i1,k1,1) < zero) Cycle
         io1 = iobs(i1,k1)
         Do i2 = i1, m
            Do k2 = 1, n
               If ((i2 == i1 .and. k2 >= k1) .or. fps(i2,k2,1) < zero) Cycle
               io2 = iobs(i2,k2)
               If (io2 == io1) Cycle
               nn = nn + 1
               p = zero
               Do l1 = 1, ng-1
                  Do l2 = l1+1, ng
                     p = p + fps(i1,k1,l1)*fps(i2,k2,l2)/fpss**2
                  End Do
               End Do
               If (p > half) Then
                  If (io2 > io1) afc = afc + one
               Else If (p < half) Then
                  If (io2 < io1) afc = afc + one
               Else
                  afc = afc + half
               End If
            End Do
         End Do
      End Do
   End Do
   If (nn > 0) Then
      afc = oneh*afc/Real(nn, Kind=rp)
   Else
      afc = rmiss
   End If
!
   Return
 End Function two_afc_mp_2
!
!
!
 Function two_afc_cc(n, obs, for) &
          Result (afc)
!
! Calculates 2AFC test score for continuous observations, continuous forecasts
!
! Modules
   Use data_numbers, Only: zero, half, one, two, oneh
!
! Function result
   Real(Kind=rp) :: afc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: obs ! - observations; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: for ! - forecasts; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - case index -
   Integer :: l ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Real, Sign
!
! Executable Statements
!
! Calculate 2afc test score
   afc = zero
   Do k = 1, n-1
      Do l = k+1, n
         If (obs(k) /= obs(l) .or. for(k) /= for(l)) Then
            afc = afc + Abs(Sign(one, obs(k)-obs(l)) + Sign(one, for(k)-for(l)))/two
         Else
            afc = afc + half
         End If
      End Do
   End Do
   afc = oneh*afc/Real(n*(n-1)/2, Kind=rp)
!
   Return
 End Function two_afc_cc
!
!
!
 Function aic(n, k, iregr, x, y, &
          t)
!
! Calculates Akaike Information Criterion (AIC)
!
! Modules
   Use data_numbers, Only: two
!
! Function result
   Real(Kind=rp) :: aic
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: k     ! - number of predictors -
   Integer, Intent(In) :: iregr ! - regression model -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Locals
!
! Local scalars
   Real (Kind=rp) :: dev ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate deviance
   dev = deviance(n, iregr, x, y, &
         t=t)
!
! Calculate Akaike Information Criterion
   aic = dev + two*Real(k, Kind=rp)
!
   Return
 End Function aic
!
!
!
 Function bic(n, k, iregr, x, y, &
          t)
!
! Calculates Bayesian Information Criterion (BIC)
!
! Function result
   Real(Kind=rp) :: bic
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: k     ! - number of predictors -
   Integer, Intent(In) :: iregr ! - regression model -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Locals
!
! Local scalars
   Real (Kind=rp) :: dev ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Real
!
! Executable Statements
!
! Calculate deviance
   dev = deviance(n, iregr, x, y, &
         t=t)
!
! Calculate Bayesian Information Criterion
   bic = dev + Real(k, Kind=rp)*Log(Real(n, Kind=rp))
!
   Return
 End Function bic
!
!
!
 Function hqc(n, k, iregr, x, y, &
          t)
!
! Calculates Hannan-Quinn Information Criterion (BIC)
!
! Modules
   Use data_numbers, Only: two
!
! Function result
   Real(Kind=rp) :: hqc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: k     ! - number of predictors -
   Integer, Intent(In) :: iregr ! - regression model -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Locals
!
! Local scalars
   Real (Kind=rp) :: dev ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Real
!
! Executable Statements
!
! Calculate deviance
   dev = deviance(n, iregr, x, y, &
         t=t)
!
! Calculate Hannan-Quinn Information Criterion
   hqc = dev + two*Real(k, Kind=rp)*Log(Log(Real(n, Kind=rp)))
!
   Return
 End Function hqc
!
!
!
 Function mic(n, nx, ny, nc, r)
!
! Calculates Mutual Information Criterion (MIC)
!
! Modules
   Use data_numbers, Only: zero, one
!
! Function result
   Real(Kind=rp) :: mic
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: nc ! - number of canonical correlations -
   Integer, Intent(In) :: nx ! - number of canonical correlations -
   Integer, Intent(In) :: ny ! - number of canonical correlations -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: r ! - canonical correlations; shape(nc) -
!
! Locals
!
! Local scalars
   Integer :: i ! - canonical correlation -
!
   Real (Kind=rp) :: pen ! - penalty term -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Real
!
! Executable Statements
!
! Calculate Mutual Information Criterion
   mic = zero
   Do i = 1, nc
      mic = mic + Log(one - r(i)**2)
   End Do
   pen = Real((nx + ny)*(n + 1), Kind=rp)/Real(n - nx - ny - 2, Kind=rp) &
         - Real(nx*(n + 1), Kind=rp)/Real(n - nx - 2, Kind=rp) &
         - Real(ny*(n + 1), Kind=rp)/Real(n - ny - 2, Kind=rp)
   mic = Real(n, Kind=rp)*(mic + pen)
!
   Return
 End Function mic
!
!
!
 Function deviance(n, iregr, x, y, &
          t) Result (dev)
!
! Modules
   Use data_cpt_constants, Only: ir_ols, ir_log, ir_bin, ir_poi, ir_gam
   Use regression,         Only: deviance_normal, deviance_bernoulli, deviance_binomial, deviance_poisson, deviance_gamma
!
! Calculates deviance
!
! Function result
   Real(Kind=rp) :: dev
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iregr ! - regression model -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent variables; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent variables; shape(n) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Executable Statements
!
! Calculate deviance
   Select Case (iregr)
    Case (ir_ols) ! - ordinary least squares regression -
      dev = deviance_normal(n, x, y)
    Case (ir_log) ! - logistic regression -
      dev = deviance_bernoulli(n, x, y)
    Case (ir_bin) ! - binomial regression -
      dev = deviance_binomial(n, x, y, t)
    Case (ir_poi) ! - poisson regression -
      dev = deviance_poisson(n, x, y)
    Case (ir_gam) ! - gamma regression -
      dev = deviance_gamma(n, x, y)
   End Select
!
   Return
 End Function deviance
!
!
!
 Function goodness(igood, iregr, n, m, nx, ny, nc, x, y, &
          cc, t)
!
! Calculates goodness of fit index, defined depending on the value of igood:
! igood = 1: the average of Pearson's correlations transformed to normal deviates, and then transformed back to a correlation
! igood = 2: the average of Spearman's correlations transformed to normal deviates, and then transformed back to a correlation
! igood = 3: the average of Kendall's correlations
! igood = 4: the average of Akaike's Information Criterion
! igood = 5: the average of Bayesian Information Criterion
! igood = 6: the average of Hannan-Quinn Information Criterion
! igood = 7: the average of Mutual Information Criterion
!
! Modules
   Use data_numbers, Only: zero, half, one, two
   Use arrays,       Only: rnks=>rwk, &
                           rank_data
!
! Function result
   Real(Kind=rp) :: goodness
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igood ! - goodness score flag -
   Integer, Intent(In) :: iregr ! - regression model -
   Integer, Intent(In) :: n     ! - number of cases -
   Integer, Intent(In) :: m     ! - number of pairs of variables -
   Integer, Intent(In) :: nx    ! - number of X EOFs / predictors -
   Integer, Intent(In) :: ny    ! - number of Y EOFs -
   Integer, Intent(In) :: nc    ! - number of canonical correlations -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - independent variables; shape(m,n) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - dependent variables; shape(m,n) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: cc ! - canonical correlations -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t  ! - binomial coefficients -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
!
   Real(Kind=rp) :: r ! - correlation / criterion -
   Real(Kind=rp) :: s ! - simple average of correlations / criteria -
   Real(Kind=rp) :: z ! - average of correlations transformed to normal deviates -
!
   Logical :: lperfect ! - perfect correlations? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Exp, Log, Present, Real
!
! Executable Statements
!
! Calculate average of scores
   z = zero
   s = zero
   lperfect = .false.
   Select Case (igood)
    Case (1) ! - Pearson's correlation -
      Do i = 1, m
         r = p_corr(n,x(i,:), y(i,:))
         s = s + r
         If (Abs(r) < one) Then
            z = z + half*Log((one + r)/(one - r))
         Else
            lperfect = .true.
         End If
      End Do
    Case (2) ! - Spearman's correlation -
      Do i = 1, m
         Call rank_data ('a', n, y(i,:), rnks(:))
         Call rank_data ('a', n, x(i,:), rnks(n+1:))
         r = s_corr(n, rnks(n+1:), rnks(:))
         s = s + r
         If (Abs(r) < one) Then
            z = z + half*Log((one + r)/(one - r))
          Else
            lperfect = .true.
         End If
      End Do
    Case (3) ! - Kendall's correlation -
      Do i = 1, m
         r = k_tau(n, x(i,:), y(i,:))
         s = s + r
      End Do
    Case (4) ! - Akaike Information Criterion -
      Do i = 1, m
         r = aic(n, nx, iregr, x(i,:), y(i,:), &
             t=t)
         s = s + r
      End Do
    Case (5) ! - Bayesian Information Criterion -
      Do i = 1, m
         r = bic(n, nx, iregr, x(i,:), y(i,:), &
             t=t)
         s = s + r
      End Do
    Case (6) ! - Hannan-Quinn Infomrmation Criterion -
      Do i = 1, m
         r = hqc(n, nx, iregr, x(i,:), y(i,:), &
             t=t)
         s = s + r
      End Do
    Case (7) ! - Mutual Information Criterion -
      If (Present(cc)) Then
         Do i = 1, m
            r = mic(n, nx, ny, nc, cc(:))
            s = s + r
         End Do
      Else
         Do i = 1, m
            r = p_corr(n, x(i,:), y(i,:))
            r = mic(n, nx, 1, 1, [r])
            s = s + r
         End Do
      End If
   End Select
!
! Revert to correlation
   If (.not.lperfect .and. igood <= 2) Then
      z = z/Real(m, Kind=rp)
      z = Exp(two*z)
      goodness = (z - 1)/(z + 1)
!
! Calculate simple average if any of the correlations are perfect
   Else
      s = s/Real(m, Kind=rp)
      goodness = s
   End If
!
   Return
 End Function goodness
!
!
!
 Function dir_goodness(igood) &
          Result (d)
!
! Sets direction of goodness index
!
! Modules
  Use data_numbers,       Only: one
  Use data_cpt_constants, Only: ncorr
!
! Function result
  Real(Kind=rp) :: d
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igood
!
! Exectuable Statements
!
! Sets direction of goodness index
   Select Case (igood)
    Case (1:ncorr) ! - correlations: positive -
      d = one
    Case Default ! - information criteria: negative -
      d = -one
   End Select
!
   Return
 End Function dir_goodness
!
!
!
 Subroutine set_dscore_range (ids, ymin, ymax, ncs, contours)
!
! Sets score limits and default contour intervals for deterministic verification scores
!
! Modules
   Use data_numbers, Only: zero, one, five, oneh
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ids ! - score identifier -
!
! - output scalars -
   Integer, Intent(Out) :: ncs ! - number of contours -
!
   Real(Kind=rp), Intent(Out) :: ymin ! - minimum score -
   Real(Kind=rp), Intent(Out) :: ymax ! - maximum score -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: contours ! - contour values -
!
! Locals
!
! Local scalars
   Integer :: i   ! - contour index -
   Integer :: int ! - contour interval -
!
   Real(Kind=rp) :: scl ! - scaling -
   Real(Kind=rp) :: y0  ! - zeroth contour value -
   Real(Kind=rp) :: yn  ! - nth contour value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Set range and contour intervals
   Select Case (ids)
    Case (10:12) ! - hit skill score, LEPS score, Gerrity score -
      ymin = -oneh
      ymax = oneh
      y0 = ymin - five
      yn = ymax + five
      scl = one
      ncs = 13
    Case (1:2) ! - Pearson, Spearman -
      ymin = -one
      ymax = one
      y0 = -oneh - five
      yn = oneh + five
      scl = oneh
      ncs = 13
    Case (3:4, 9, 13:14) ! - 2AFC, %variance, hit score -
      ymin = zero
      ymax = oneh
      y0 = ymin
      yn = ymax
      scl = one
      ncs = 9
    Case (15:16) ! - ROC -
      ymin = zero
      ymax = one
      y0 = ymin
      yn = oneh
      scl = oneh
      ncs = 9
    Case (5) ! - variance ratio -
      ymin = zero
      ymax = one
      y0 = ymin
      yn = oneh
      scl = oneh
      ncs = 9
    Case Default ! - error scores -
      ymin = zero
      ymax = zero
      y0 = ymin
      yn = one
      scl = one
      ncs = 9
   End Select
!
! Set contour values
   contours(:) = zero
   int = Nint(yn - y0)/(ncs + 1)
   Do Concurrent (i = 1:ncs)
      contours(i) = (y0 + Real(int*i, Kind=rp))/scl
   End Do
!
   Return
 End Subroutine set_dscore_range
!
!
!
 Subroutine set_pscore_range (ips, ymin, ymax, ncs, contours)
!
! Sets score limits and default contour intervals for probabilistic verification scores
!
! Modules
   Use data_numbers,       Only: zero, one, two, five, oneh
   Use data_cpt_constants, Only: npall
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ips ! - score identifier -
!
! - output scalars -
   Integer, Intent(Out) :: ncs ! - number of contours -
!
   Real(Kind=rp), Intent(Out) :: ymin ! - minimum score -
   Real(Kind=rp), Intent(Out) :: ymax ! - maximum score -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: contours ! - contour values -
!
! Locals
!
! Local scalars
   Integer :: i   ! - contour index -
   Integer :: int ! - contour interval -
!
   Real(Kind=rp) :: scl ! - scaling -
   Real(Kind=rp) :: y0  ! - zeroth contour value -
   Real(Kind=rp) :: yn  ! - nth contour value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Set range and contour intervals
   Select Case (ips)
    Case (2, 4, npall+5) ! - effective interest rate, RPSS, BSS  -
      ymin = -oneh
      ymax = oneh
      y0 = ymin - five
      yn = ymax + five
      scl = one
      ncs = 13
    Case (5, npall+8) ! - resolution slopes -
      ymin = -one
      ymax = one
      y0 = -oneh - five
      yn = oneh + five
      scl = oneh
      ncs = 13
    Case (npall+6, npall+7) ! - resolution (Brier), reliability (Brier) -
      ymin = zero
      ymax = one
      y0 = ymin
      yn = oneh
      scl = oneh
      ncs = 9
    Case (1, npall+1:npall+3) ! - ignorance and decomposition -
      ymin = zero
      ymax = two
      y0 = ymin
      yn = two*oneh
      scl = oneh
      ncs = 9
    Case Default
      ymin = zero
      ymax = oneh
      y0 = ymin
      yn = ymax
      scl = one
      ncs = 9
   End Select
!
! Set contour values
   contours(:) = zero
   int = Nint(yn - y0)/(ncs + 1)
   Do Concurrent (i = 1:ncs)
      contours(i) = (y0 + Real(int*i, Kind=rp))/scl
   End Do
!
   Return
 End Subroutine set_pscore_range
!
!
!
 Subroutine get_corrs (icor, n, m, x, y, r)
!
! Calculates correlations
!
! Modules
   Use arrays, Only: rnk=>rwk, &
                     rank_data
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icor ! - correlation (1 = Pearson; 2 = Spearman; 3 = Kendall) -
   Integer, Intent(In) :: n    ! - total number of cases -
   Integer, Intent(In) :: m    ! - total number of gridpoints -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent data; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - independent data; shape(m,n) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: r ! - correlations; shape(m) -
!
! Locals
!
! Local scalars
   Integer :: i   ! - grid index -
!
! Executable Statements
!
! Calculate skill
   Select Case (icor)
    Case (1) ! - Pearson's correlation -
      r(1:m) = p_corr(m, n, x(1:m,:), y(:))
    Case (2) ! - Spearman's correlation -
      Call rank_data ('a', n, y(:), rnk(:))
      Do i = 1, m
         Call rank_data ('a', n, x(i,:), rnk(n+1:))
         r(i) = s_corr(n, rnk(n+1:), rnk(:))
      End Do
    Case (3) ! - Kendall's tau -
      Do i = 1, m
         r(i) = k_tau(n, x(i,:), y(:))
      End Do
   End Select
   lcorr = .true.
!
   Return
 End Subroutine get_corrs
!
!
!
 Subroutine get_scores (iskill, n, m, x, y, ifor, iobs, cps, skill)
!
! Calculates skill scores
!
! Modules
   Use data_numbers, Only: oneh
   Use arrays,       Only: rank_data
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iskill ! - skill metric identifier -
   Integer, Intent(In) :: n      ! - total number of cases -
   Integer, Intent(In) :: m      ! - total number of gridpoints -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
   Integer, Dimension(:,:), Intent(In) :: ifor ! - forecast categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x   ! - forecasts; shape(m,n) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y   ! - observations; shape(m,n) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities; shape(m,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: skill ! - skill values; shape(m) -
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sqrt
!
! Executable Statements
!
! Calculate skill
! - Pearson's product moment correlation / % variance -
   Select Case (iskill)
    Case (1, 4)
      skill(:) = p_corr(m, n, x(1:m,:), y(1:m,:))
      If (iskill == 4) skill(:) = oneh*skill(:)**2
      lerror = .false.
! - Spearman's rank-order correlation -
    Case (2)
      Do i = 1, m
         Call rank_data ('a', n, x(i,:), rnkf(:))
         Call rank_data ('a', n, y(i,:), rnko(:))
         skill(i) = s_corr(n, rnkf(:), rnko(:))
      End Do
      lerror = .false.
! - 2AFC -
    Case (3)
      Do i = 1, m
         skill(i) = two_afc_cc(n, y(i,:), x(i,:))
      End Do
      lerror = .false.
! - variance ratio -
    Case (5)
      Do i = 1, m
         skill(i) = var_ratio(n, x(i,:), y(i,:))
      End Do
      lerror = .true.
! - mean bias -
    Case (6)
      Do i = 1, m
         skill(i) = mbias(n, y(i,:), x(i,:))
      End Do
      lerror = .true.
! - RMSE -
    Case (7)
      Do i = 1, m
         skill(i) = Sqrt(mserror(n, y(i,:), x(i,:)))
      End Do
      lerror = .true.
! - mean absolute error -
    Case (8)
      Do i = 1, m
         skill(i) = maerror(n, y(i,:), x(i,:))
      End Do
      lerror = .true.
! - hit score -
    Case (9)
      Call heidke (m, n, ng, iobs(:,1:n), ifor(:,1:n), cps(:,:), &
           hs=skill(:))
      lerror = .false.
! - hit skill score -
    Case (10)
      Call heidke (m, n, ng, iobs(:,1:n), ifor(:,1:n), cps(:,:), &
           hss=skill(:))
      lerror = .false.
! - LEPS score -
    Case (11)
      Do i = 1, m
         Call leps_coeffs (ng, cps(i,:), cs(:,:))
         skill(i) = lepscat(n, iobs(i,1:n), ifor(i,1:n), cs(:,:))
      End Do
      lerror = .false.
! - Gerrity score -
    Case (12)
      Do i = 1, m
         Call gerrity_coeffs (ng, n, iobs(i,1:n), gs(:,:))
         skill(i) = gerrity(n, iobs(i,1:n), ifor(i,1:n), gs(:,:))
      End Do
      lerror = .false.
! - 2AFC (categorical forecasts) -
    Case (13)
      Do i = 1, m
         skill(i) = two_afc_mm(n, ng, iobs(i,1:n), ifor(i,1:n))
      End Do
      lerror = .false.
! - 2AFC (continuous forecasts) -
    Case (14)
      Do i = 1, m
         skill(i) = two_afc_mc(n, ng, iobs(i,1:n), x(i,1:n))
      End Do
      lerror = .false.
! - ROC (below) -
    Case (15)
      Do i = 1, m
         Call rank_data ('a', n, x(i,1:n), rnkf(:))
         Call roc (n, 1, iobs(i,1:n), rnkf, skill(i), hit(1:n,1), far(1:n,1))
      End Do
      lerror = .false.
! - ROC (above) -
    Case (16)
      Do i = 1, m
         Call rank_data ('a', n, x(i,1:n), rnkf(:))
         Call roc (n, ng, iobs(i,1:n), rnkf, skill(i), hit(1:n,2), far(1:n,2))
      End Do
      lerror = .false.
   End Select
!
   Return
 End Subroutine get_scores
!
!
!
 Subroutine get_pscores (ips, icat, n, m, nb, ibs, iobs, fps, cps, rmiss, s)
!
! Calculates probabilistic scores
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: npall
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ips  ! - score identifier -
   Integer, Intent(In) :: icat ! - category identifier -
   Integer, Intent(In) :: n    ! - total number of cases -
   Integer, Intent(In) :: m    ! - total number of gridpoints -
   Integer, Intent(In) :: nb   ! - number of probability bins -
   Integer, Intent(In) :: ibs  ! - bin forecast probabilities? no if ibs  ==  0, yes if ibs  ==  1 -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories; shape(m,n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities; shape(m,n,ng) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities; shape(m,n,ng) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: s ! - probabilistic scores; shape(m) -
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
   Real(Kind=rp) :: dum ! - dummy argument -
!
! Local arrays
   Real(Kind=rp), Dimension(ng,4) :: ps ! - probabilistic scores -
!
! Executable Statements
!
! Calculate scores
! - all-categories scores -
   Select Case (ips)
    Case (1) ! - all-categories ignorance score -
      Do i = 1, m
         Call ignorance (n, 1, nb, 0, ibs, iobs(i:i,:), fps(i:i,:,:), rmiss, s(i))
      End Do
    Case (2) ! - effective interest rate -
      Do i = 1, m
         s(i) = effective_interest(n, 1, iobs(i:i,:), fps(i:i,:,:), cps(i:i,:), rmiss)
      End Do
    Case (3) ! - RPS -
      Do i = 1, m
         Call ranked_prob_score (n, 1, ng, ibs*nb, iobs(i:i,:), fps(i:i,:,:), cps(i:i,:), rmiss, s(i), dum)
      End Do
    Case (4) ! - RPSS -
      Do i = 1, m
         Call ranked_prob_score (n, 1, ng, ibs*nb, iobs(i:i,:), fps(i:i,:,:), cps(i:i,:), rmiss, dum, s(i))
      End Do
    Case (5) ! - resolution slope -
      If (init_attrib() == 0) Then
         Do i = 1, m
            Call calc_rel (n, 1, ng, nb, iobs(i:i,:), fps(i:i,:,:), rmiss, ifq(:,:), afp(:,:), orf(:,:), tends(:,:), &
                 b0s(:), b1s(:))
            s(i) = b1s(0)
         End Do
      Else
         s(:) = zero
      End If
    Case (6) ! - 2AFC -
      Do i = 1, m
         s(i) = two_afc_mp(n, 1, ng, iobs(i:i,:), fps(i:i,:,:), rmiss)
      End Do
    Case (7) ! - linear probability score -
      Do i = 1, m
         s(i) = linear_prob(n, 1, iobs(i:i,:), fps(i:i,:,:), rmiss)
      End Do
    Case (8:7+ng) ! - ranked hits -
      Do i = 1, m
         Call ranked_hits (n, 1, ng, iobs(i:i,:), fps(i:i,:,:), rmiss, ps(:,1))
         s(i) = ps(ips-7,1)
      End Do
!  - category-specific scores -
    Case (npall+1) ! - ignorance score -
      Do i = 1, m
         Call ignorance (n, 1, nb, icat, ibs, iobs(i:i,:), fps(i:i,:,:), rmiss, s(i))
      End Do
    Case (npall+2) ! - ignorance score (reliability component) -
      Do i = 1, m
         Call ignorance (n, 1, nb, icat, ibs, iobs(i:i,:), fps(i:i,:,:), rmiss, dum, &
              rel=s(i))
      End Do
    Case (npall+3) ! - ignorance score (resolution component) -
      Do i = 1, m
         Call ignorance (n, 1, nb, icat, ibs, iobs(i:i,:), fps(i:i,:,:), rmiss, dum, &
              res=s(i))
      End Do
    Case (npall+4,npall+5) ! - Brier scores and Brier skill scores -
      Do i = 1, m
         Call hbrier (n, 1, ng, nb, ibs, iobs(i:i,:), fps(i:i,:,:), cps(i:i,:), rmiss, ps(:,1), ps(:,2))
         s(i) = ps(icat,ips-(npall+3))
      End Do
    Case (npall+6,npall+7) ! - Brier score decomposition -
      Do i = 1, m
         Call hbrier (n, 1, ng, nb, ibs, iobs(i:i,:), fps(i:i,:,:), cps(i:i,:), rmiss, ps(:,1), ps(:,2), &
              rel=ps(:,3), res=ps(:,4))
         s(i) = ps(icat,ips-(npall+3))
      End Do
    Case (npall+8) ! - resolution slopes -
      If (init_attrib() == 0) Then
         Do i = 1, m
            Call calc_rel (n, 1, ng, nb, iobs(i:i,:), fps(i:i,:,:), rmiss, ifq(:,:), afp(:,:), orf(:,:), tends(:,:), &
                 b0s(:), b1s(:))
            s(i) = b1s(icat)
         End Do
      Else
         s(:) = zero
      End If
    Case (npall+9) ! - ROC areas -
      Do i = 1, m
         Call two_afc_2p (n, 1, ng, iobs(i:i,:), fps(i:i,:,:), rmiss, ps(:,1))
         s(i) = ps(icat,1)
      End Do
   End Select
!
   Return
 End Subroutine get_pscores
!
!
!
 Subroutine print_dscores (iout1, iout2, lhead, scores, &
            itab_adj, clb, bcls, pvals)
!
! Prints scores
!
! Modules
   Use data_cpt_constants, Only: lbmscore, ndcon, nscore
   Use maths,              Only: interval
   Use labels,             Only: ca_scores, &
                                 cg_na, cg_score
   Use settings,           Only: lexpert
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout1 ! - output unit number for continuous scores -
   Integer, Intent(In) :: iout2 ! - output unit number for categorical scores -
!
   Logical, Intent(In) :: lhead ! - print header? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: scores ! - scores -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: itab_adj ! - tab adjustment -
!
   Real(Kind=rp), Intent(In), Optional :: clb ! - bootstrap confidence level -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: pvals ! - p-values -
!
   Type(interval), Dimension(:), Intent(In), Optional :: bcls ! - bootstrap confidence limits -
!
! Locals
!
! Local scalars
   Integer :: j       ! - score index -
   Integer :: itab1   ! - tab position -
   Integer :: itab2   ! - tab position -
   Integer :: itab3   ! - tab position -
   Integer :: itab4   ! - tab position -
   Integer :: lscores ! - length of ca_scores -
   Integer :: iout    ! - output unit number -
!
   Character(Len= 5) :: cfmt  ! - format -
   Character(Len= 2) :: ctab1 ! - tab format -
   Character(Len= 2) :: ctab2 ! - tab format -
   Character(Len= 2) :: ctab3 ! - tab format -
   Character(Len= 2) :: ctab4 ! - tab format -
   Character(Len=31) :: csv   ! - score value -
!
   Logical :: lpc ! - add % sign? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Len, Len_trim, Present, Trim
!
! Executable Statements
!
! Print scores
   lscores = Len(ca_scores(1))
   If (Present(itab_adj)) lscores = lscores + itab_adj
   Do j = 1, nscore
      If (j == 1) Then
         iout = iout1
         If (lhead) Then
            itab1 = Len(ca_scores(j)) + 15 + 1 - Len_Trim(cg_score)
            Write (Unit=ctab1, Fmt='(I2)') itab1
            If (.not.Present(clb)) Then
               Write (Unit=iout, Fmt='(T'//ctab1//',A)') Trim(cg_score)
            Else
               itab2 = lscores + 15 + 10 + 1 - Len('P-value')
               itab3 = lscores + 15 + 10 + 1 + (2*15 + 4 - Len('Confidence limits'))/2
               itab4 = itab3 + 1
               Write (Unit=ctab2, Fmt='(I2)') itab2
               Write (Unit=ctab3, Fmt='(I2)') itab3
               Write (Unit=ctab4, Fmt='(I2)') itab4
               Write (Unit=iout, Fmt='(T'//ctab1//',A,T'//ctab2//',A,T'//ctab3//',A)') cg_score, 'P-value', 'Confidence limits'
               Write (Unit=iout, Fmt='(T'//ctab4//',A,F7.3,A)') '(level:', clb, '%)'
            End If
            Write (Unit=iout, Fmt='(A)') 'Continuous measures:'
         Else
            Write (Unit=iout, Fmt=*)
            Write (Unit=iout, Fmt=*)
         End If
      Else If (j == ndcon+1) Then
         iout = iout2
         If (lhead) Then
            Write (Unit=iout, Fmt='(A)') 'Categorical measures:'
         Else
            Write (Unit=iout, Fmt=*)
            Write (Unit=iout, Fmt=*)
         End If
      End If
      If (.not.lexpert) Then
         If (.not.lbmscore(j)) Cycle ! - skip if the score is not used in beginner mode -
      End If
      Select Case (j)
       Case (1, 2, 5, 15, 16)
         cfmt = 'F15.4'
         lpc = .false.
       Case (3, 4, 9:14)
         cfmt = 'F14.2'
         lpc = .true.
       Case (6:8)
         cfmt = 'F15.2'
         lpc = .false.
      End Select
      Write (Unit=iout, Fmt='(A)', Advance='no') ca_scores(j)(1:lscores)
      Write (Unit=iout, Fmt='('//cfmt//')', Advance='no') scores(j)
      If (lpc) Write (Unit=iout, Fmt='(A)', Advance='no') '%'
 ! - p-values -
      If (Present(pvals)) Then
         Select Case (j)
          Case (7, 8)
            Write (Unit=iout, Fmt='(7X,A)', Advance='no') cg_na
          Case Default
            Write (Unit=iout, Fmt='(F10.4)', Advance='no') pvals(j)
         End Select
      End If
! - confidence limits -
      If (Present(bcls)) Then
         Write (Unit=iout, Fmt='('//cfmt//')', Advance='no') bcls(j)%lower
         If (lpc) Write (Unit=iout, Fmt='(A)', Advance='no') '%'
         Write (Unit=iout, Fmt='(A)', Advance='no') ' to '
         Write (Unit=csv, Fmt='('//cfmt//')') bcls(j)%upper
         Write (Unit=iout, Fmt='(A)', Advance='no') Trim(AdjustL(csv))
         If (lpc) Write (Unit=iout, Fmt='(A)', Advance='no') '%'
      End If
      Write (Unit=iout, Fmt='(A)', Advance='yes') ' '
   End Do
!
   Return
 End Subroutine print_dscores
!
!
!
 Function get_width_dscores( &
          itab, lbcls, lpval) &
          Result (iwid)
!
! Calculates required window width to print deterministic scores
!
! Modules
   Use labels, Only: ca_scores
   Use gui,    Only: set_tabs
!
! Function result
   Integer :: iwid
!
! Arguments
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: itab ! - tab -
!
   Logical, Intent(In), Optional :: lbcls ! - bootstrap confidence limits? -
   Logical, Intent(In), Optional :: lpval ! - p-values? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len, Present
!
! Executable Statements
!
! Calculate required width
   iwid = Len(ca_scores)
!
! Set tabs
   If (Present(itab)) Then
      iwid = iwid + itab
      Call set_tabs (1, [iwid])
   End If
!
! Add width of data
   iwid = iwid + 15
!
! Add p-values
   If (Present(lpval)) Then
      If (lpval) iwid = iwid + 10
   End If
!
! Add bootstrap confidence limits
   If (Present(lbcls)) Then
      If (lbcls) iwid = iwid + 2*15 + 4
   End If
!
   Return
 End Function get_width_dscores
!
!
!
 Function close_scores() &
          Result (fclose)
!
! Frees memory allocated for scoring
!
! Function result
   Integer :: fclose
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free memory
   If (Allocated(corrs))   Deallocate (corrs)
   If (Allocated(pscores)) Deallocate (pscores)
   If (Allocated(pvalues)) Deallocate (pvalues)
   If (Allocated(skills))  Deallocate (skills)
   If (Allocated(wrlts))   Deallocate (wrlts)
   If (Allocated(fars))    Deallocate (fars)
   If (Allocated(hits))    Deallocate (hits)
   If (Allocated(far))     Deallocate (far)
   If (Allocated(hit))     Deallocate (hit)
   If (Allocated(rnkf))    Deallocate (rnkf)
   If (Allocated(rnko))    Deallocate (rnko)
   fclose = 0
!
   Return
 End Function close_scores
!
!
!
 Function close_dscores() &
          Result (fclose)
!
! Frees memory allocated for deterministic scores
!
! Function result
   Integer :: fclose
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free memory
   If (Allocated(far))  Deallocate (far)
   If (Allocated(hit))  Deallocate (hit)
   If (Allocated(rnkf)) Deallocate (rnkf)
   If (Allocated(rnko)) Deallocate (rnko)
   fclose = 0
!
   Return
 End Function close_dscores
!
!
!
 Function close_pscores() &
          Result (fclose)
!
! Frees memory allocated for probabilistic scores
!
! Function result
   Integer :: fclose
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free memory
   If (Allocated(pscores)) Deallocate (pscores)
   fclose = 0
!
   Return
 End Function close_pscores
End Module scores
