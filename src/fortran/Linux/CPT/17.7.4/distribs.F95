! Author: Simon Mason
Module distribs
!
! Modules
   Use data_numbers, Only: rp
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cdf_empirical, cdf_gamma, cdf_normal, close_distribs, get_gamma, gaussian, gaussian_inv, get_errvar, get_mean, &
             get_median, get_var, init_distribs, moments, proj_read_distribs, proj_write_distribs, q_empirical, q_normal,   &
             q_student_t_2, s_student_t, restdize, stdize, thom_gamma, transform_cv, ustdize, zero_bound
!
! Arrays
!
! Real arrays
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: alpha ! - gamma shape parameter -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: beta  ! - gamma scale parameter -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: pcen  ! - probability of censorship (i.e., too close to zero) -
!
Contains
!
!
 Subroutine init_distribs (m, lb, ns, ifail)
!
! Initialises memory for climatologies
!
! Modules
   Use data_cpt_constants, Only: it_gam
   Use settings,           Only: igauss
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lb ! - lower bound -
   Integer, Intent(In) :: m  ! - number of locations -
   Integer, Intent(In) :: ns ! - number of seasons -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - gamma parameters -
   If (igauss == it_gam) Then
      Allocate (alpha(m,lb:ns), Stat=ifail)
      If (ifail /= 0) GoTo 1
      Allocate (beta(m,lb:ns), Stat=ifail)
      If (ifail /= 0) GoTo 1
      Allocate (pcen(m,lb:ns), Stat=ifail)
      If (ifail /= 0) GoTo 1
   Else
      ifail = 0
   End If
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_distribs
!
!
!
 Function get_mean(n, v, &
          vmiss) &
          Result (vbar)
!
! Calculates arithmetic mean
!
! Modules
   Use data_numbers, Only: zero
!
! Function result
   Real(Kind=rp) :: vbar
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data; shape(n) -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vmiss ! - missing value -
!
! Locals
!
! Local scalars
   Integer :: k   ! - case index -
   Integer :: nnm ! - number of non-missing values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real, Sum
!
! Executable Statements
!
! Calculate mean
   If (Present(vmiss)) Then
      nnm = 0
      vbar = zero
      Do k = 1, n
         If (v(k) /= vmiss) Then
            nnm = nnm + 1
            vbar = vbar + v(k)
         End If
      End Do
   Else
      nnm = n
      vbar = Sum(v(1:n))
   End If
   vbar = vbar/Real(nnm, Kind=rp)
!
   Return
 End Function get_mean
!
!
!
 Function get_median(n, v, &
          vmiss) &
          Result (vmed)
!
! Calculates median
!
! Modules
   Use data_numbers, Only: half
   Use arrays,       Only: rwk, &
                           insertion_sort
!
! Function result
   Real(Kind=rp) :: vmed
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data; shape(n) -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vmiss ! - missing value -
!
! Locals
!
! Local scalars
   Integer :: k   ! - case index -
   Integer :: nnm ! - number of non-missing values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Calculate mean
   If (Present(vmiss)) Then
      nnm = 0
      Do k = 1, n
         If (v(k) /= vmiss) Then
            nnm = nnm + 1
            rwk(nnm) = v(k)
         End If
      End Do
   Else
      nnm = n
      rwk(1:nnm) = v(1:nnm)
   End If
   Call insertion_sort ('a', nnm, rwk(:))
   vmed = q_empirical(rwk(:), nnm, half)
!
   Return
 End Function get_median
!
!
!
 Function get_var(n, v, vbar, &
          vmiss) &
          Result (var)
!
! Calculates variance around vbar
!
! Modules
   Use data_numbers, Only: zero
!
! Function result
   Real(Kind=rp) :: var
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Real(Kind=rp), Intent(In) :: vbar ! - mean -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data; shape(n) -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vmiss ! - missing value -
!
! Locals
!
! Local scalars
   Integer :: k   ! - case index -
   Integer :: nnm ! - number of non-missing values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real, Sum
!
! Executable Statements
!
! Calculate variance
   If (Present(vmiss)) Then
      nnm = 0
      var = zero
      Do k = 1, n
         If (v(k) /= vmiss) Then
            nnm = nnm + 1
            var = var + (v(k) - vbar)**2
         End If
      End Do
   Else
      nnm = n
      var = Sum((v(1:n) - vbar)**2)
   End If
   var = var/Real(nnm-1, Kind=rp)
!
   Return
 End Function get_var
!
!
!
 Function get_errvar(n, m, v, p) &
          Result (var)
!
! Calculates prediction-error variance
!
! Modules
   Use data_numbers, Only: zero
!
! Function result
   Real(Kind=rp) :: var
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of predictors -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: p ! - predictions; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - verifications; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: ndofr ! - degrees of freedom -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Calculate prediction-error variance
   ndofr = n - m - 1
   If (ndofr > 0) Then
      var = Sum((p(1:n) - v(1:n))**2)/Real(ndofr)
   Else
      var = zero
   End If
!
   Return
 End Function get_errvar
!
!
!
 Pure Subroutine moments (m, n, v, vm, &
      vsd)
!
! Calculates arithmetic column means and standard deviations of a 2-D array
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of variables -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v ! - variables; shape(m,n) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: vm ! - means; shape(m) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: vsd ! - standard deviations; shape(m) -
!
! Locals
!
! Local scalars
   Integer :: i ! - indices -
!
   Real(Kind=rp) :: df  ! - number of cases -
   Real(Kind=rp) :: dof ! - degrees of freedom -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real, Sqrt, Sum
!
! Executable Statements
!
! Calculate degrees of freedom
   df = Real(n, Kind=rp)
   If (Present(vsd)) Then
      dof = Real(n-1, Kind=rp)
!
! Calculate means and standard deviations
      Do Concurrent (i = 1:m)
         vm(i) = Sum(v(i,1:n))/df
         vsd(i) = Sqrt(Sum((v(i,1:n) - vm(i))**2)/dof)
      End Do
   Else
!
! Calculate means and standard deviations
      vm(1:m) = Sum(v(1:m,1:n), Dim=2)/df
   End If
!
   Return
 End Subroutine moments
!
!
!
 Subroutine stdize (istd, igauss, isn, m, n, ave, sdev, v)
!
! Standardizes data
!
! Modules
   Use data_numbers,       Only: zero, eps, oneh
   Use data_cpt_constants, Only: is_non, is_ano, is_std, is_sti, is_pav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m      ! - number of variables -
   Integer, Intent(In) :: n      ! - number of cases -
   Integer, Intent(In) :: istd   ! - standardization option -
   Integer, Intent(In) :: igauss ! - transformation option -
   Integer, Intent(In) :: isn    ! - season -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: ave  ! - mean; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(In) :: sdev ! - standard deviation; shape(m) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data to be standardized; shape(m,n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: k ! - case index -
!
! Executable Statements
!
! Standardize data
   Select Case (istd)
    Case (is_non) ! - no standardization -
      Continue
    Case (is_ano) ! - anomalies
      Do Concurrent (k = 1:n)
         v(:,k) = v(:,k) - ave(:)
      End Do
    Case (is_std) ! - standardized anomalies -
      Do i = 1, m
         If (sdev(i) > eps) Then
            v(i,:) = (v(i,:) - ave(i))/sdev(i)
         Else
            v(i,:) = zero
         End If
      End Do
    Case (is_sti) ! - standardized indices -
      Call gaussian (igauss, isn, m, n, v(:,:))
    Case (is_pav) ! - % of average -
      Do i = 1, m
         If (ave(i) > eps) Then
            v(i,:) = oneh*v(i,:)/ave(i)
         Else
            v(i,:) = zero
         End If
      End Do
   End Select
!
   Return
 End Subroutine stdize
!
!
!
 Subroutine ustdize (istd, igauss, isn, m, n, ave, sdev, v, &
            iloc)
!
! Unstandardizes data
!
! Modules
   Use data_numbers,       Only: zero, eps, oneh
   Use data_cpt_constants, Only: is_non, is_ano, is_std, is_sti, is_pav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m      ! - number of variables -
   Integer, Intent(In) :: n      ! - number of cases -
   Integer, Intent(In) :: istd   ! - standardization option -
   Integer, Intent(In) :: igauss ! - transformation option -
   Integer, Intent(In) :: isn    ! - season -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: ave  ! - mean; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(In) :: sdev ! - standard deviation; shape(m) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data to be unstandardized; shape(m,n) -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: iloc ! - current location index -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: k ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present
!
! Executable Statements
!
! Unstandardize data
   Select Case (istd)
    Case (is_non) ! - no standardization -
      Continue
    Case (is_ano) ! - anomalies
      Do Concurrent (k = 1:n)
         v(:,k) = v(:,k) + ave(:)
      End Do
    Case (is_std) ! - standardized anomalies -
      Do i = 1, m
         If (sdev(i) > eps) Then
            v(i,:) = v(i,:)*sdev(i) + ave(i)
         Else
            v(i,:) = ave(i)
         End If
      End Do
    Case (is_sti) ! - standardized indices -
      If (Present(iloc)) Then
         Call gaussian_inv (igauss, 0, isn, m, n, v(:,:), &
              ave=ave, sdev=sdev, iloc=iloc)
      Else
         Call gaussian_inv (igauss, 0, isn, m, n, v(:,:), &
              ave=ave, sdev=sdev)
      End If
    Case (is_pav) ! - % of average -
      Do i = 1, m
         If (ave(i) > eps) Then
            v(i,:) = v(i,:)*ave(i)/oneh
         Else
            v(i,:) = zero
         End If
      End Do
   End Select
!
   Return
 End Subroutine ustdize
!
!
!
 Subroutine restdize (istd_old, istd_new, igauss, isn, m, n, ave, sdev, v)
!
! Restandardizes data
!
! Modules
   Use data_numbers,       Only: zero, eps, oneh
   Use data_cpt_constants, Only: is_non, is_ano, is_std, is_sti, is_pav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: istd_old ! - old standardization option -
   Integer, Intent(In) :: istd_new ! - new standardization option -
   Integer, Intent(In) :: igauss   ! - transformation option -
   Integer, Intent(In) :: isn      ! - season -
   Integer, Intent(In) :: m        ! - number of variables -
   Integer, Intent(In) :: n        ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: ave  ! - mean; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(In) :: sdev ! - standard deviation; shape(m) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data to be restandardized; shape(m,n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: k ! - case index -
!
! Executable Statements
!
! Restandardize data from no standardization
   Select Case (istd_old)
    Case (is_non)
      Call stdize (istd_new, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
!
! Restandardize data from anomalies
    Case (is_ano)
      Select Case (istd_new)
       Case (is_non) ! - to no standardization -
         Do Concurrent (k = 1:n)
            v(:,k) = v(:,k) + ave(:)
         End Do
       Case (is_ano) ! - to anomalies -
         Continue
       Case (is_std) ! - to standardized anomalies -
         Do i = 1, m
            If (sdev(i) > eps) Then
               v(i,:) = v(i,:)/sdev(i)
            End If
         End Do
       Case (is_sti) ! - to standardized indices -
         Call ustdize (istd_old, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
         Call gaussian (igauss, isn, m, n, v(:,:))
       Case (is_pav) ! - to % of average -
         Do i = 1, m
            If (ave(i) > eps) Then
               v(i,:) = oneh*(v(i,:) + ave(i))/ave(i)
            Else
               v(i,:) = zero
            End If
         End Do
      End Select
!
! Restandardize data from standardized anomalies
    Case (is_std)
      Select Case (istd_new)
       Case (is_non) ! - to no standardization -
         Do i = 1, m
            If (sdev(i) > eps) Then
               v(i,:) = v(i,:)*sdev(i) + ave(i)
            Else
               v(i,:) = ave(i)
            End If
         End Do
       Case (is_ano) ! - to anomalies -
         Do i = 1, m
            If (sdev(i) > eps) Then
               v(i,:) = v(i,:)*sdev(i)
            Else
               v(i,:) = zero
            End If
         End Do
       Case (is_std) ! - to standardized anomalies -
         Continue
       Case (is_sti) ! - to standardized indices -
         Call ustdize (istd_old, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
         Call gaussian (igauss, isn, m, n, v(:,:))
       Case (is_pav) ! - to % of average -
         Do i = 1, m
            If (ave(i) > eps) Then
               v(i,:) = oneh*(v(i,:)*sdev(i) + ave(i))/ave(i)
            Else
               v(i,:) = zero
            End If
         End Do
      End Select
!
! Restandardise data from standardized indices
    Case (is_sti)
      Call gaussian_inv (igauss, istd_new, isn, m, n, v(:,:), &
           ave=ave, sdev=sdev)
!
! Restandardise data from % of average
    Case (is_pav)
      Select Case (istd_new)
       Case (is_non) ! - to no standardization -
         Call ustdize (istd_old, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
       Case (is_ano) ! - to anomalies -
         Do k = 1, n
            v(1:m,k) = v(1:m,k)*ave(1:m)/oneh - ave(1:m)
         End Do
       Case (is_std) ! - to standardized anomalies -
         Call ustdize (istd_old, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
         Call stdize (istd_new, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
       Case (is_sti) ! - to standardized indices -
         Call ustdize (istd_old, igauss, isn, m, n, ave(:), sdev(:), v(:,:))
         Call gaussian (igauss, isn, m, n, v(:,:))
       Case (is_pav) ! - to % of average -
         Continue
      End Select
   End Select
!
   Return
 End Subroutine restdize
!
!
!
 Subroutine transform_cv (m, n, ncv, ave, sdev, z)
!
! Transforms data in cross-validation mode
!
! Modules
   Use data_cpt_constants, Only: it_emp, it_gam
   Use progress_meter,     Only: update_progress_meter
   Use settings,           Only: hcw, igauss, istd, izero
   Use arrays,             Only: y, ybk, yc, &
                                 get_cvsample, insertion_sort
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m   ! - number of locations -
   Integer, Intent(In) :: n   ! - number of time steps -
   Integer, Intent(In) :: ncv ! - number of time steps in cross-validation sample -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: ave  ! - climatological standard deviations; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(In) :: sdev ! - climatological means; shape(m) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: z ! - data to be transformed; shape(m,n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - case index -
!
! Executable Statements
!
! Transform
   If (igauss > 0) Then
      Do k = 1, n
         Select Case (igauss)
          Case (it_emp)
            Call get_cvsample (hcw, k, y(1:m,1:n,1), yc(1:m,1:n))
            Call insertion_sort ('a', m, ncv, yc(:,:))
          Case (it_gam)
            Call get_cvsample (hcw, k, ybk(1:m,1:n,1), yc(1:m,1:n))
            Call get_gamma (m, ncv, 1, yc(1:m,1:ncv))
            Call get_cvsample (hcw, k, y(1:m,1:n,1), yc(1:m,1:n))
         End Select
         Call gaussian_inv (igauss, istd, 1, m, 1, z(:,k:k), &
              nr=ncv, ref=yc(:,1:ncv), ave=ave(:), sdev=sdev(:))
      End Do
      If (update_progress_meter(.false.) /= 0) Return
   End If
!
! Apply zero-bound
   If (izero == 1) Then
      Call zero_bound (istd, m, n, ave, sdev, z(:,:))
      If (update_progress_meter(.false.) /= 0) Return
   End If
!
   Return
 End Subroutine transform_cv
!
!
!
 Subroutine zero_bound (istd, m, n, ave, sdev, v)
!
! Applies a zero-bound to a 2-dimensional array
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: is_non, is_ano, is_std, is_sti, is_pav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: istd ! - standardization option -
   Integer, Intent(In) :: m    ! - number of variables -
   Integer, Intent(In) :: n    ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: ave  ! - averages; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(In) :: sdev ! - standard deviations; shape(m) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - values; shape(m,n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
!
   Real(Kind=rp) :: szero ! - standardized zero -
!
! Executable Statements
!
! Apply zero-transform
   Select Case (istd)
    Case (is_non, is_pav) ! - no standardization, % of average -
      Where (v(:,1:n) < zero) v(:,1:n) = zero
    Case (is_ano) ! - anomalies -
      Do i = 1, m
         szero = -ave(i)
         Where (v(i,1:n) < szero) v(i,1:n) = szero
      End Do
    Case (is_std) ! - standardized anomalies -
      Do i = 1, m
         szero = -ave(i)/sdev(i)
         Where (v(i,1:n) < szero) v(i,1:n) = szero
      End Do
    Case (is_sti) ! - standardized indices -
      Continue
   End Select
!
   Return
 End Subroutine zero_bound
!
!
!
 Subroutine gaussian (igto, isn, m, n, y, &
            nr, ref)
!
! Converts data to a gaussian distribution via a uniform distribution
!
! Modules
   Use data_numbers,       Only: zero, eps, half, one, two, eps, rhuge
   Use data_cpt_constants, Only: it_emp, it_gam
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igto ! - gaussian transformation option -
   Integer, Intent(In) :: isn  ! - season -
   Integer, Intent(In) :: m    ! - number of locations -
   Integer, Intent(In) :: n    ! - number of cases -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - data to be transformed; shape(m,n) -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nr ! - number of reference cases -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: ref ! - reference data; shape(m,nr) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - location index -
   Integer :: k1 ! - case index -
   Integer :: k2 ! - case index -
   Integer :: nn ! - number of cases -
!
   Real(Kind=rp) :: df   ! - number of cases -
   Real(Kind=rp) :: ymin ! - minimum non-zero value -
!
! Local arrays
   Real(Kind=rp), Dimension(:), Allocatable :: g ! - workspace -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MinVal, Present, Real
!
! Executable Statements
!
! Convert to normal distribution
   If (Present(nr)) Then
      nn = Max(n, nr)
   Else
      nn = n
   End If
   Allocate (g(nn))
   Select Case (igto)
! - from empirical distribution -
    Case (it_emp)
      If (Present(nr) .and. Present(ref)) Then
         df = Real(nr+1, Kind=rp)
         Do i = 1, m
            g(1:n) = one
            Do k1 = 1, n ! - convert to percentiles -
               Do k2 = 1, nr
                  If (y(i,k1) > ref(i,k2)) Then
                     g(k1) = g(k1) + one
                  Else If (y(i,k1) < ref(i,k2)) Then
                     Continue
                  Else
                     g(k1) = g(k1) + half
                  End If
               End Do
               y(i,k1) = q_normal(g(k1)/df) ! - convert to normal -
            End Do
         End Do
      Else
         df = Real(n+1, Kind=rp)
         Do i = 1, m
            g(1:n) = one
            Do k1 = 1, n-1 ! - convert to percentiles -
               Do k2 = k1+1, n
                  If (y(i,k1) > y(i,k2)) Then
                     g(k1) = g(k1) + one
                  Else If (y(i,k1) < y(i,k2)) Then
                     g(k2) = g(k2) + one
                  Else
                     g(k1) = g(k1) + half
                     g(k2) = g(k2) + half
                  End If
               End Do
            End Do
            Do k1 = 1, n
               y(i,k1) = q_normal(g(k1)/df) ! - convert to normal -
            End Do
         End Do
      End If
! - from gamma distribution -
    Case (it_gam)
      Do i = 1, m
         If (beta(i,isn) > eps) Then
            ymin = rhuge
            Do k1 = 1, n
               If (y(i,k1) > zero) Then
                  g(k1) = cdf_gamma(y(i,k1)/beta(i,isn), alpha(i,isn), pcen(i,isn)) ! - convert to percentiles -
               Else
                  If (ymin == rhuge) ymin = MinVal(y(i,1:n), Mask=(y(i,1:n) > eps .and. y(i,1:n) < rhuge))
                  If (ymin == rhuge) ymin = eps
                  g(k1) = cdf_gamma(ymin/beta(i,isn), alpha(i,isn), pcen(i,isn))/two
               End If
               y(i,k1) = q_normal(g(k1)) ! - convert to normal -
            End Do
         Else
            y(i,1:n) = zero
         End If
      End Do
   End Select
   Deallocate (g)
!
   Return
 End Subroutine gaussian
!
!
!
 Subroutine gaussian_inv (igto, istd, isn, m, n, y, &
            iloc, nr, ref, ave, sdev)
!
! Converts data from a gaussian distribution to an empirical distribution via a uniform distribution
!
! Modules
   Use data_cpt_constants, Only: is_non, is_sti
   Use arrays,             Only: rwk, &
                                 insertion_sort
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igto ! - gaussian transformation option -
   Integer, Intent(In) :: istd ! - standardization option -
   Integer, Intent(In) :: isn  ! - season -
   Integer, Intent(In) :: m    ! - number of locations -
   Integer, Intent(In) :: n    ! - number of cases -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y   ! - data to be transformed; shape(m,n) -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: iloc ! - current location index -
   Integer, Intent(In), Optional :: nr   ! - number of cases in reference data -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: ave  ! - mean; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: sdev ! - standard deviation; shape(m) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: ref ! - reference empirical distribution; shape(m,nr) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - location index -
   Integer :: k  ! - case index -
   Integer :: nn ! - number of cases -
!
! Local arrays
   Real(Kind=rp), Dimension(:,:), Allocatable :: g ! - workspace -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Present
!
! Executable Statements
!
! Convert to uniform distribution
   If (Present(nr)) Then
      nn = Max(n,nr)
   Else
      nn = n
   End If
   Allocate (g(1,nn))
   If (igto == 0) Return
   Do i = 1, m
      Do k = 1, n
         y(i,k) = cdf_normal(y(i,k))
      End Do
!
! Convert to original distribution
      Select Case (igto)
       Case (1) ! - empirical distribution -
         Do k = 1, n
            rwk(1:nr) = ref(i,:)
            Call insertion_sort ('a', nr, rwk(1:nr))
            y(i,k) = q_empirical(rwk(1:nr), nr, y(i,k))
         End Do
       Case (2) ! - gamma distribution - 
         If (Present(iloc)) Then
            Do k = 1, n
               y(i,k) = q_gamma(y(i,k), alpha(iloc,isn), beta(iloc,isn), pcen(i,isn))
            End Do
         Else
            Do k = 1, n
               y(i,k) = q_gamma(y(i,k), alpha(i,isn), beta(i,isn), pcen(i,isn))
            End Do
         End If
! - standardise (untransform implies unstandardise for SPI and q_gamma is implicitly unstandardised) - 
         If (istd /= is_non .and. istd /= is_sti) Call stdize (istd, igto, 1, 1, n, ave(:), sdev(:), y(i:,1:n))
      End Select
   End Do
   Deallocate (g)
!
   Return
 End Subroutine gaussian_inv
!
!
!
 Function q_empirical(v, n, p) &
          Result (z)
!
! Calculates deviate from an empirical distribution corresponding to a given lower-tail area of P.
!
! Modules
   Use data_numbers, Only: half, one
!
! Function result
   Real(Kind=rp) :: z
! 
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Real(Kind=rp), Intent(In) :: p ! - lower-tail area -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data sorted in ascending order; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: indx ! - category boundary index -
!
   Real(Kind=rp) :: d    ! - distance to category boundary -
   Real(Kind=rp) :: df   ! - number of cases -
   Real(Kind=rp) :: rndx ! - category boundary index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Int
!
! Executable Statements
!
! Calculate deviate
   df = Real(n, Kind=rp)
   rndx = df*p + half
   indx = Int(rndx)
   d = rndx - Real(indx, Kind=rp)
   If (indx > 0 .and. indx < n) Then
      z = v(indx)*(one - d) + v(indx + 1)*d
   Else If (indx > 0) Then
      z = v(n) + (v(n) - v(n - 1))*d
   Else
      If (n > 1) Then
         z = v(1) - (v(2) - v(1))*(one - d)
      Else
         z = v(1)
      End If
   End If
!
   Return
 End Function q_empirical
!
!
!
 Function q_normal(p) &
          Result (z)
!
! Algorithm AS 241  Appl. Statist. 37 (3)
!
! Produces the normal deviate Z corresponding to a given lower-tail area of P;
! Z is accurate to about 1 part in 10**16.
!
! Modules
   Use data_numbers, Only: zero, half, one, five, nine, eps
!
! Function result
   Real(Kind=rp) :: z
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: p ! - lower-tail area -
!
! Locals
!
! Local parameters
   Real(Kind=rp), Parameter :: split1 = 0.425000_rp
   Real(Kind=rp), Parameter :: split2 = five
   Real(Kind=rp), Parameter :: const1 = 0.180625_rp
   Real(Kind=rp), Parameter :: const2  =1.600000_rp
!
   Real(Kind=rp), Parameter :: a0 = 3.3871328727963666080e0_rp  ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a1 = 1.3314166789178437745e+2_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a2 = 1.9715909503065514427e+3_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a3 = 1.3731693765509461125e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a4 = 4.5921953931549871457e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a5 = 6.7265770927008700853e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a6 = 3.3430575583588128105e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: a7 = 2.5090809287301226727e+3_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b1 = 4.2313330701600911252e+1_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b2 = 6.8718700749205790830e+2_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b3 = 5.3941960214247511077e+3_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b4 = 2.1213794301586595867e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b5 = 3.9307895800092710610e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b6 = 2.8729085735721942674e+4_rp ! - coefficients for P close to 0.5 -
   Real(Kind=rp), Parameter :: b7 = 5.2264952788528545610e+3_rp ! - coefficients for P close to 0.5 -
!
   Real(Kind=rp), Parameter :: c0 = 1.42343711074968357734e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c1 = 4.63033784615654529590e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c2 = 5.76949722146069140550e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c3 = 3.64784832476320460504e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c4 = 1.27045825245236838258e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c5 = 2.41780725177450611770e-1_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c6 = 2.27238449892691845833e-2_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: c7 = 7.74545014278341407640e-4_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d1 = 2.05319162663775882187e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d2 = 1.67638483018380384940e0_rp  ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d3 = 6.89767334985100004550e-1_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d4 = 1.48103976427480074590e-1_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d5 = 1.51986665636164571966e-2_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d6 = 5.47593808499534494600e-4_rp ! - coefficients for P not close to 0, 0.5 or 1 -
   Real(Kind=rp), Parameter :: d7 = 1.05075007164441684324e-9_rp ! - coefficients for P not close to 0, 0.5 or 1 -
!
   Real(Kind=rp), Parameter :: e0 = 6.65790464350110377720e0   ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e1 = 5.46378491116411436990e0   ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e2 = 1.78482653991729133580e0   ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e3 = 2.96560571828504891230e-1  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e4 = 2.65321895265761230930e-2  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e5 = 1.24266094738807843860e-3  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e6 = 2.71155556874348757815e-5  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: e7 = 2.01033439929228813265e-7  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f1 = 5.99832206555887937690e-1  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f2 = 1.36929880922735805310e-1  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f3 = 1.48753612908506148525e-2  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f4 = 7.86869131145613259100e-4  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f5 = 1.84631831751005468180e-5  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f6 = 1.42151175831644588870e-7  ! - coefficients for P near 0 or 1 -
   Real(Kind=rp), Parameter :: f7 = 2.04426310338993978564e-15 ! - coefficients for P near 0 or 1 -
!
! Local scalars
   Real(Kind=rp) :: q, r ! - constants -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Log, Sqrt
!
! Executable Statments
!
! Evaluate deviate
   If (p < eps) Then
      z = -nine
      Return
   Else If (p > one-eps) Then
      z = nine
      Return
   End If
   q = p - half
   If (Abs(q) <= split1) Then
       r = const1 - q*q
       z = q*(((((((a7*r + a6)*r + a5)*r + a4)*r + a3)*r + a2)*r + a1)*r + a0)/ &
             (((((((b7*r + b6)*r + b5)*r + b4)*r + b3)*r + b2)*r + b1)*r + one)
   Else
      If (q < zero) Then
         r = p
      Else
         r = one - p
      End If
      r = Sqrt(-Log(r))
      If (r <= split2) Then
         r = r - const2
         z = (((((((c7*r + c6)*r + c5)*r + c4)*r + c3)*r + c2)*r + c1)*r + c0)/ &
             (((((((d7*r + d6)*r + d5)*r + d4)*r + d3)*r + d2)*r + d1)*r + one)
      Else
         r = r - split2
         z = (((((((e7*r + e6)*r + e5)*r + e4)*r + e3)*r + e2)*r + e1)*r + e0)/ &
             (((((((f7*r + f6)*r + f5)*r + f4)*r + F3)*r + f2)*r + f1)*r + one)
      End If
      If (q < zero) z = -z
   End If
!
   Return
 End Function q_normal
!
!
!
 Function q_gamma(p, a, b, p0) &
          Result (z)
!
! Produces the gamma deviate corresponding to a given lower-tail area of P
! Uses relationship with the chi-squared distribution
!
! Modules
   Use data_numbers, Only: zero, one, two
!
! Function result
   Real(Kind=rp) :: z
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: p  ! - lower-tail area -
   Real(Kind=rp), Intent(In) :: a  ! - gamma shape parameter -
   Real(Kind=rp), Intent(In) :: b  ! - gamma scale parameter -
   Real(Kind=rp), Intent(In) :: p0 ! - probability of a zero -
!
! Locals
!
! Executable Statements
!
! If probability is less than probability of a zero
   If (p <= p0) Then
      z = zero
!
! Scale deviate from chi-squared distribution
   Else
      z = q_chi2((p - p0)/(one - p0), two*a)*b/two
   End If
!
   Return
 End Function q_gamma
!
!
!
 Function q_chi2(p, v) &
          Result (z)
!
! Algorithm AS 91  Appl. Statist. 24 (3)
! and Algorithm AS R85  Appl. Statist. 40 (1)
!
! Produces the chi-squared deviate Z corresponding to a given lower-tail area of P.
! Adapted from the version by Alan Miller
! http://jblevins.org/mirror/amiller/as91.f90
!
! Auxiliary routines required: PPND=AS 111 (or AS 241) and GAMMAD=AS 239.
!
! Modules
   Use data_numbers, Only: zero, half, one, two, three, six, tol
!
! Function result
   Real(Kind=rp) :: z
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: p ! - lower-tail area -
   Real(Kind=rp), Intent(In) :: v ! - degrees of freedom -
!
! Locals
!
! Local parameters
   Integer, Parameter :: maxit = 20 ! - maximum number of iterations -
!
   Real(Kind=rp), Parameter :: aa = 0.6931471806_rp
   Real(Kind=rp), Parameter :: e = 0.0000005_rp
   Real(Kind=rp), Parameter ::  c1=    0.01_rp,  c2=0.222222_rp,  c3=    0.32_rp,  c4=     0.4_rp,  c5=    1.24_rp, &
                               c6=     2.2_rp,  c7=    4.67_rp,  c8=    6.66_rp,  c9=    6.73_rp, c10=   13.32_rp, &
                              c11=    60.0_rp, c12=    70.0_rp, c13=    84.0_rp, c14=   105.0_rp, c15=   120.0_rp, &
                              c16=   127.0_rp, c17=   140.0_rp, c18=   175.0_rp, c19=   210.0_rp, c20=   252.0_rp, &
                              c21=   264.0_rp, c22=   294.0_rp, c23=   346.0_rp, c24=   420.0_rp, c25=   462.0_rp, &
                              c26=   606.0_rp, c27=   672.0_rp, c28=   707.0_rp, c29=   735.0_rp, c30=   889.0_rp, &
                              c31=   932.0_rp, c32=   966.0_rp, c33=  1141.0_rp, c34=  1182.0_rp, c35=  1278.0_rp, &
                              c36=  1740.0_rp, c37=  2520.0_rp, c38=  5040.0_rp
!
! Local scalars
   Integer :: i  ! - iteration index -
!
   Real(Kind=rp) :: a, b, c, g, p0, p1, p2, q, s1, s2, s3, s4, s5, s6, t, x, xx
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Exp, Log, Sqrt
!
! Executale Statements
!
! Test arguments and initialise
   z = -one
   If (v <= zero) Return
   If (p < tol) Then
     z = zero
      Return
   Else If (p > one-tol) Then
     p0 = one - tol
   Else
     p0 = p
   End If
   xx = half*v
   c = xx - one
   g = ln_gamma(v/two)
!
! Starting approximation for small chi-squared
   If (v < -c5*Log(p0)) Then
      z = (p0*xx*Exp(g + xx*aa))**(one/xx)
      If (z < e) Return
!
! Starting approximation for v less than or equal to 0.32
   Else If (v <= c3) Then
      z = c4
      a = Log(one - p0)
      Do
         q = z
         p1 = one + z*(c7 + z)
         p2 = z*(c9 + z*(c8 + z))
         t = -half + (c7 + two*z)/p1 - (c9 + z*(c10 + three*z))/p2
         z = z - (one - Exp(a + g + half*z + c*aa)*p2/p1)/t
         If (Abs(q/z - one) <= c1) Exit
      End Do
!
! Normal approximation
   Else
      x = q_normal(p0)
!
! Starting approximation using Wilson and Hilferty estimate
      p1 = c2/v
      z = v*(x*Sqrt(p1) + one - p1)**3
!
! Starting approximation for p tending to 1
      If (z > c6*v+six) z = -two*(Log(one - p0) - c*Log(half*z) + g)
   End If
!
! Calculation of seven-term Taylor series
   Do i = 1, maxit
      q = z
      p1 = half*z
      p2 = p0 - cdf_gamma(p1, xx, zero)
      t = p2*Exp(xx*aa + g + p1 - c*Log(z))
      b = t/z
      a = half*t - b*c
      s1 = (c19 + a*(c17 + a*(c14 + a*(c13 + a*(c12 + c11*a)))))/c24
      s2 = (c24 + a*(c29 + a*(c32 + a*(c33 + c35*a))))/c37
      s3 = (c19 + a*(c25 + a*(c28 + c31*a)))/c37
      s4 = (c20 + a*(c27 + c34*a) + c*(c22 + a*(c30 + c36*a)))/c38
      s5 = (c13 + c21*a + c*(c18 + c26*a))/c37
      s6 = (c15 + c*(c23 + c16*c))/c38
      z = z + t*(one + half*t*s1 - b*c*(s1 - b*(s2 - b*(s3 - b*(s4 - b*(s5 - b*s6))))))
      If (Abs(q/z - one) > e) Exit
   End Do
!
   Return
 End Function q_chi2
!
!
!
 Function cdf_empirical(v, n, t) &
          Result (cdf)
!
! Calculates the lower-tail area for a deviate, t, given an empirical distribution, v
!
! Modules
   Use data_numbers, Only: zero, half, one
!
! Function result
   Real(Kind=rp) :: cdf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Real(Kind=rp), Intent(In) :: t ! - deviate -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data sorted in ascending order; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: i    ! - case index -
   Integer :: indx ! - category boundary index -
!
   Real(Kind=rp) :: d ! - distance to category boundary -
   Real(Kind=rp) :: w ! - distance between sorted values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Exp, Real
!
! Executable Statements
!
! Calculate lower-tail area
   indx = 0
   Do i = 1, n
      If (v(i) < t) Then
         indx = indx + 1
      Else
         Exit
      End If
   End Do
   If (indx > 0 .and. indx < n) Then
      w = v(indx+1) - v(indx)
      If (w > zero) Then
         d = (t - v(indx))/w
         cdf = (Real(indx, Kind=rp) + d)/Real(n+1, Kind=rp)
      Else
         cdf = Real(indx, Kind=rp)/Real(n+1, Kind=rp)
      End If
   Else If (indx > 0) Then
      w = v(n) - v(n-1)
      If (w > zero) Then
         w = one - Exp((v(n) - t)/w)
      Else
         w = half
      End If
      cdf = (n + w)*(one/Real(n+1, Kind=rp))
   Else
      w = v(2) - v(1)
      If (w > zero) Then
         w = one - Exp((t - v(1))/w)
      Else
         w = half
      End If
      cdf = (one - w)*(one/Real(n+1, Kind=rp))
   End If
   If (cdf == zero) cdf = one/Real(n+2, Kind=rp)
   If (cdf == one) cdf = Real(n+1, Kind=rp)/Real(n+2, Kind=rp)
!
   Return
 End Function cdf_empirical
!
!
!
 Function cdf_normal(z) &
          Result (cdf)
!
! Produces the lower-tail area corresponding to a normal deviate.
!
! Modules
   Use data_numbers, Only: half, sqrt2
!
! Function result
   Real(Kind=rp) :: cdf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: z ! - normal deviate -
!
! Executable Statements
!
! Calculate tail area
   cdf = half*error_f(-z/sqrt2)
!
   Return
 End Function cdf_normal
!
!
!
 Function cdf_gamma(z, a, p0) &
          Result (cdf)
!
! Algorithm AS 239  Appl. Statist. 37 (3)
! Based on the ELF90-compatible version by Alan Miller:
! http://jblevins.org/mirror/amiller/as239.f90
!
! Produces the lower-tail area corresponding to a standardized gamma deviate
!
! Modules
   Use data_numbers, Only: zero, third, one, two, three, nine, onet, onehm, sfmax, smlnum, elim
!
! Function result
   Real(Kind=rp) :: cdf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: z  ! - gamma deviate -
   Real(Kind=rp), Intent(In) :: a  ! - gamma shape parameter -
   Real(Kind=rp), Intent(In) :: p0 ! - probability of a zero -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: aa ! - gamma parameter -
   Real(Kind=rp) :: an, b, c, pn1, pn2, pn3, pn4, pn5, pn6, rn
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Exp, Log, Min, Sqrt
!
! Executable Statements
!
! Calculate tail area
   If (z < zero .or. a <= zero) Then
      cdf = zero
   Else If (z > onehm) Then
      cdf = one
   Else If (a > onet) Then ! - use normal approximation for large parameter values -
      cdf = three*Sqrt(a)*((z/a)**(third) + one/(nine*a) - one)
      cdf = cdf_normal(cdf)
   Else If (z <= one .or. z < a) Then ! - Pearson's series expansion -
      c = one
      cdf = one
      aa = a
      Do
         aa = aa + one
         c = c*z/aa
         cdf = cdf + c
         If (c <= smlnum) Exit
      End Do
      If (z > zero) Then
         cdf = a*Log(z) - z - ln_gamma(a+one) + Log(cdf)
         If (cdf >= -elim) Then
            cdf = Exp(cdf)
         Else
            cdf = zero
         End If
      Else
         cdf = zero
      End If
   Else ! - continued fraction expansion -
      aa = one - a
      b = aa + z + one
      c = zero
      pn1 = one
      pn2 = z
      pn3 = z + one
      pn4 = z*b
      cdf = pn3/pn4
      Do
         aa = aa + one
         b = b + two
         c = c + one
         an = aa*c
         pn5 = b*pn3 - an*pn1
         pn6 = b*pn4 - an*pn2
         If (Abs(pn6) > zero) Then
           rn = pn5/pn6
           If (Abs(cdf-rn) <= Min(smlnum, smlnum*rn)) Exit
           cdf = rn
         End If
         pn1 = pn3
         pn2 = pn4
         pn3 = pn5
         pn4 = pn6
         If (Abs(pn5) >= sfmax) Then ! - re-scale terms in continued fraction if terms are large -
            pn1 = pn1/sfmax
            pn2 = pn2/sfmax
            pn3 = pn3/sfmax
            pn4 = pn4/sfmax
         End If
      End Do
      cdf = a*Log(z) - z - ln_gamma(a) + Log(cdf)
      If (cdf >= -elim) Then
         cdf = one - Exp(cdf)
      Else
         cdf = one
      End If
   End If
!
! Adjust for zero values
   cdf = p0 + (one - p0)*cdf
!
   Return
 End Function cdf_gamma
!
!
!
 Function ln_gamma(zval)
!
! Algorithm AS 245  Appl. Statist. 38 (2)
! Based on the ELF90-compatible version by Alan Miller:
! http://jblevins.org/mirror/amiller/as245.f90
!
! Produces the logarithm of the gamma function
!
! Modules
   Use data_numbers, Only: zero, half, one, onep5, four, ten, twelve, bignum, lnsqr2pi, rhuge
   Use maths,        Only: newton_raphson
!
! Function result
   Real(Kind=rp) :: ln_gamma
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: zval ! - gamma deviate -
!
! Locals
!
! Local parameters
! - coefficients of rational functions -
   Real(Kind=rp), Parameter :: r1(9) = [ -2.66685511495_rp, -24.4387534237_rp,  &
                                         -21.9698958928_rp,  11.1667541262_rp,  &
                                          3.13060547623_rp,  0.607771387771_rp, &
                                          11.9400905721_rp,  31.4690115749_rp,  &
                                          15.2346874070_rp]
   Real(Kind=rp), Parameter :: r2(9) = [ -78.3359299449_rp, -142.046296688_rp,  &
                                          137.519416416_rp,  78.6994924154_rp,  &
                                          4.16438922228_rp,  47.0668766060_rp,  &
                                          313.399215894_rp,  263.505074721_rp,  &
                                          43.3400022514_rp]
   Real(Kind=rp), Parameter :: r3(9) = [ -2.12159572323e5_rp,  2.30661510616e5_rp,  &
                                          2.74647644705e4_rp, -4.02621119975e4_rp,  &
                                         -2.29660729780e3_rp, -1.16328495004e5_rp,  &
                                         -1.46025937511e5_rp, -2.42357409629e4_rp,  &
                                         -5.70691009324e2_rp]
   Real(Kind=rp), Parameter :: r4(5) = [ 0.279195317918525_rp, 0.4917317610505968_rp, &
                                         0.0692910599291889_rp, 3.350343815022304_rp, &
                                         6.012459259764103_rp]
!
! Local scalars
   Real(Kind=rp) :: z, z1, z2, y
!
   Real(Kind=rp), Save :: gmax = -one ! - maximum gamma deviate -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Sqrt
!
! Executable Statements
!
! Set maximum gamma deviate
   If (gmax < zero) Call newton_raphson (get_gmax, ten, Sqrt(bignum), gmax) ! - maximum gamma argument -
   ln_gamma = zero
   z = zval
!
! Test for valid function argument
   If (z >= rhuge) Return
   If (z <= zero) Return
!
! Calculation for 0 < z < 0.5 and 0.5  <=  z < 1.5 combined
   If (z < onep5) Then
      If (z < half) Then
         ln_gamma = -Log(z)
         y = z + one
         If (y == one) Return ! - test whether z < machine epsilon -
      Else
         ln_gamma = zero
         y = z
         z = (z - half) - half
      End If
      ln_gamma = ln_gamma + z*((((r1(5)*y + r1(4))*y + r1(3))*y + r1(2))*y + r1(1))/ &
                              ((((y + r1(9))*y + r1(8))*y + r1(7))*y + r1(6))
       Return
   End If
!
! Calculation for 1.5  <=  z < 4.0
   If (z < four) Then
      y = (z - one) - one
      ln_gamma = y*((((r2(5)*z + r2(4))*z + r2(3))*z + r2(2))*z + r2(1))/ &
                         ((((z + r2(9))*z + r2(8))*z + r2(7))*z + r2(6))
       Return
    End If
!
! Calculation for 4.0  <=  z < 12.0
   If (z < twelve) Then
      ln_gamma = ((((r3(5)*z + r3(4))*z + r3(3))*z + r3(2))*z + r3(1))/ &
                       ((((z + r3(9))*z + r3(8))*z + r3(7))*z + r3(6))
       Return
   End If
!
! Calculation for z  >=  12.0
   y = Log(z)
   ln_gamma=z*(y - one) - half*y + lnsqr2pi
   If (z > gmax) Return
   z1 = one/z
   z2 = z1*z1
   ln_gamma = ln_gamma + z1*((r4(3)*z2 + r4(2))*z2 + r4(1))/ &
                                  ((z2 + r4(5))*z2 + r4(4))
!
   Return
 End Function ln_gamma
!
!
!
 Subroutine get_gmax (z, fval, fder)
!
! Calculates function value and derivative for estimating maximum gamma deviate
!
! Modules
   Use data_numbers, Only: half, two, six, twelve, bt, ln2pi
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: z ! - current value -
!
! - output scalars -
   Real(Kind=rp) :: fval ! - current function value -
   Real(Kind=rp) :: fder ! - current derivative -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log
!
! Calculate arithmetic and geometric means
   fval = twelve*z*((z - half)*Log(z) - z + half*ln2pi) - bt
   fder = twelve*((two*z - half)*Log(z) - z + half*ln2pi) - six
!
   Return
 End Subroutine get_gmax
!
!
!
 Subroutine get_gamma (m, n, isn, ref)
!
! Calculates gamma distribution parameters
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isn  ! - season -
   Integer, Intent(In) :: m    ! - number of locations -
   Integer, Intent(In) :: n    ! - number of cases -
!
! Optional arguments
   Real(Kind=rp), Dimension(:,:), Intent(In) :: ref ! - reference data; shape(m,n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
!
! Executable Statements
!
! Unstandardise the data
   Do i = 1, m
      Call thom_gamma (n, ref(i,:), alpha(i,isn), beta(i,isn), pcen(i,isn))
   End Do
!
   Return
 End Subroutine get_gamma
!
!
!
 Subroutine thom_gamma (n, x, alpha, beta, pcen)
!
! Calculates Thom (1958) estimates of the gamma distribution parameters
! Thom, H. C. S., 1958: A note on the gamma distribution. Mon. Wea. Rev., 86, 117-122.
! Values less than eps are censored, and the proportion of censored values is returned
! Uses method of moments if skewness is negative
!
! Modules
   Use data_numbers, Only: zero, one, three, four, eps, tol
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of observations -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - observational data; shape(n) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: alpha ! - gamma shape parameter -
   Real(Kind=rp), Intent(Out) :: beta  ! - gamma scale parameter -
   Real(Kind=rp), Intent(Out) :: pcen  ! - probability of censorship (i.e., too close to zero) -
!
! Locals
!
! Local scalars
   Integer :: k  ! - index -
   Integer :: nc ! - number of censored values -
   Integer :: nu ! - number of uncensored values -
!
   Real(Kind=rp) :: slogx ! - sum of logs -
   Real(Kind=rp) :: sx    ! - sum -
   Real(Kind=rp) :: xbar  ! - arithmetic mean -
   Real(Kind=rp) :: xlbar ! - geometic mean -
   Real(Kind=rp) :: xvar  ! - variance -
   Real(Kind=rp) :: cen   ! - censorship level -
   Real(Kind=rp) :: a     ! - difference beween log mean and mean log -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Real, Sqrt
!
! Calculate arithmetic and geometric means
   cen = eps
   sx = zero
   slogx = zero
   pcen = zero
   nu = 0
   Do k = 1, n
      If (x(k) > cen) Then
         nu = nu + 1
         sx = sx + x(k)
         slogx = slogx + Log(x(k))
      End If
   End Do
!
! Check number of uncensored cases
   nc = n - nu
   pcen = Real(nc, Kind=rp)/Real(n, Kind=rp)
   If (nu == 0) Then
      alpha = zero
      beta = zero
      Return
   End If
   xbar = sx/Real(nu, Kind=rp)
   xlbar = slogx/Real(nu, Kind=rp)
!
! Calculate ML estimates of parameters
   a = four*(Log(xbar) - xlbar)
   If (a > tol) Then
      alpha = (one + Sqrt(one + a/three))/a
      beta = xbar/alpha
!
! Calculate moment estimates
   Else
      xvar = zero
      Do k = 1, n
         If (x(k) > cen) Then
            xvar = xvar + (x(k) - xbar)**2
         End If
      End Do
      xvar = xvar/Real(nu-1, Kind=rp)
      If (xvar > eps) Then
         beta = xvar/xbar
         alpha = xbar/beta
      Else
         beta = zero
         alpha = zero
      End If
   End If
!
   Return
 End Subroutine thom_gamma
!
!
!
 Function s_student_t(t, dof)
!
! Algorithm AS 27  Appl. Statist. 19 (1)
!
! Calculate the upper-tail area under Student's t-distribution
!
! Modules
   Use data_numbers, Only: zero, half, one
!
! Function result
   Real(Kind=rp) :: s_student_t
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: t   ! - Student's t-deviate -
   Real(Kind=rp), Intent(In) :: dof ! - degrees of freedom -
!
! Locals
!
! Local parameters
   Real(Kind=rp), Parameter :: a1=0.09979441_rp,  a2=-0.581821_rp,   a3=1.390993_rp,    a4=-1.222452_rp, a5=2.151185_rp
   Real(Kind=rp), Parameter :: b1=5.537409_rp,    b2=11.42343_rp
   Real(Kind=rp), Parameter :: c1=0.04431742_rp,  c2=-0.2206018_rp,  c3=-0.03317253_rp, c4=5.679969_rp,  c5=-12.96519_rp
   Real(Kind=rp), Parameter :: d1=5.166733_rp,    d2=13.49862_rp
   Real(Kind=rp), Parameter :: e1=0.009694901_rp, e2=-0.1408854_rp,  e3=1.88993_rp,     e4=-12.75532_rp, e5=25.77532_rp
   Real(Kind=rp), Parameter :: f1=4.233736_rp,    f2=14.3963_rp
   Real(Kind=rp), Parameter :: g1=-9.187228d-5,   g2=0.03789901_rp,  g3=-1.280346_rp,   g4=9.249528_rp,  g5=-19.08115_rp
   Real(Kind=rp), Parameter :: h1=2.777816_rp,    h2=16.46132_rp
   Real(Kind=rp), Parameter :: i1=5.79602d-4,     i2=-0.02763334_rp, i3=0.4517029_rp,   i4=-2.657697_rp, i5=5.127212_rp
   Real(Kind=rp), Parameter :: j1=0.5657187_rp,   j2=21.83269_rp
!
! Local scalars
   Real(Kind=rp) :: abst ! - absolute Student's t-deviate -
   Real(Kind=rp) :: v    ! - inverse degrees of freedom -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statments
!
! Evaluate series
   v = one/dof
   abst = Abs(t)
   s_student_t = half*(one + abst*(((a1 + v*(a2 + v*(a3 + v*(a4 + v*a5))))/(one - v*(b1 - v*b2)))+ &
                             abst*(((c1 + v*(c2 + v*(c3 + v*(c4 + v*c5))))/(one - v*(d1 - v*d2)))+ &
                             abst*(((e1 + v*(e2 + v*(e3 + v*(e4 + v*e5))))/(one - v*(f1 - v*f2)))+ &
                             abst*(((g1 + v*(g2 + v*(g3 + v*(g4 + v*g5))))/(one - v*(h1 - v*h2)))+ &
                             abst* ((i1 + v*(i2 + v*(i3 + v*(i4 + v*i5))))/(one - v*(j1 - v*j2))))))))**(-8)
   If (t < zero) s_student_t = one - s_student_t
!
   Return
 End Function s_student_t
!
!
!
 Function q_student_t_2(p2t, dof)
!
! Algorithm ACM 396  Comm. ACM 13 (10)
!
! Calculates positive quantiles of the Student's t-distribution at the two-tailed probability p2t
!
! Modules
   Use data_numbers, Only: half, one, two, three, four, five, six, seven, twelve, hpi
!
! Function result
   Real(Kind=rp) :: q_student_t_2
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: p2t ! - two tailed area -
   Real(Kind=rp), Intent(In) :: dof ! - degrees of freedom -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: a, b, c, d, x, y ! - constants -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Cos, Exp, Sin, Sqrt
!
! Executable Statments
!
! Evaluate quantile
   If (dof == one) Then
      a = p2t*hpi
      q_student_t_2 = Cos(a)/Sin(a)
   Else If (dof == two) Then
      q_student_t_2 = Sqrt(two/(p2t*(two - p2t)) - two)
   Else
      a = one/(dof - half)
      b = 48.0_rp/a**2
      c = ((20700.0_rp*a/b - 98.0_rp)*a - 16.0_rp)*a + 96.36_rp
      d = ((94.5_rp/(b + c) - three)/b + one)*Sqrt(a*hpi)*dof
      x = d*p2t
      y = x**(two/dof)
      If (y > 0.05_rp + a) Then
         x = q_normal(p2t*half)
         y = x**2
         If (dof < five) c=c+0.3_rp*(dof-4.5_rp)*(x+0.6_rp)
         c = (((0.05_rp*d*x - five)*x - seven)*x - two)*x + b + c
         y = (((((0.4_rp*y + 6.3_rp)*y + 36.0_rp)*y + 94.5_rp)/c - y - three)/b + one)*x
         y = a*y**2
         If (y > 0.002_rp) Then
            y = Exp(y) - one
         Else
            y = ((y + four)*y + twelve)*y*y/24.0_rp + y
         End If
      Else
         y = ((one/(((dof + six)/(dof*y) - 0.089_rp*d - 0.822_rp)* &
             (dof + two)*three) + half/(dof+four))*y - one)*(dof + one)/(dof + two) + one/y
      End If
      q_student_t_2 = Sqrt(dof*y)
   End If
!
   Return
 End Function q_student_t_2
!
!
!
 Function error_f(x)
!
! Computes approximate values for error_f(x).
!
! The main computation evaluates near-minimax approximations from "Rational Chebyshev approximations for the error function" by 
! W. J. Cody, Math. Comp., 1969, PP. 631-638. This transportable program uses rational functions that theoretically approximate  
! error_f(x)  to at least 18 significant decimal digits.  The accuracy achieved depends on the arithmetic system, the compiler, 
! the intrinsic functions, and proper selection of the machine-dependent constants.
!
! Written by: W. J. Cody
!             Mathematics and Computer Science Division
!             Argonne National Laboratory
!             Argonne, IL 60439
!
! Written: January 8, 1985
! Latest modification: March 19, 1990
!
! Modules
   Use data_numbers, Only: zero, one, two, four, sixten, sqrpi, eps
!
! Function result
   Real(Kind=rp) :: error_f
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: x ! - error function argument -
!
! Locals
!
! Local parameters
   Real(Kind=rp), Parameter :: thresh = 0.46875_rp
   Real(Kind=rp), Parameter :: xbig =  26.543_rp ! - largest argument acceptable to ERFC -
   Real(Kind=rp), Parameter :: a(5) = & ! - coefficients for approximation to error_f in first interval -
      [3.16112374387056560e00_rp, 1.13864154151050156e02_rp, 3.77485237685302021e02_rp, 3.20937758913846947e03_rp, &
       1.85777706184603153e-1_rp]
   Real(Kind=rp), Parameter :: b(4) = & ! - coefficients for approximation to error_f in first interval -
      [2.36012909523441209e01_rp, 2.44024637934444173e02_rp, 1.28261652607737228e03_rp, 2.84423683343917062e03_rp]
   Real(Kind=rp), Parameter :: c(9) = & ! - coefficients for approximation to error_f in second interval -
      [5.64188496988670089e-1_rp, 8.88314979438837594e00_rp, 6.61191906371416295e01_rp, 2.98635138197400131e02_rp, &
       8.81952221241769090e02_rp, 1.71204761263407058e03_rp, 2.05107837782607147e03_rp, 1.23033935479799725e03_rp, &
       2.15311535474403846e-8_rp]
   Real(Kind=rp), Parameter :: d(8) = & ! - coefficients for approximation to error_f in second interval -
      [1.57449261107098347e01_rp, 1.17693950891312499e02_rp, 5.37181101862009858e02_rp, 1.62138957456669019e03_rp, &
       3.29079923573345963e03_rp, 4.36261909014324716e03_rp, 3.43936767414372164e03_rp, 1.23033935480374942e03_rp]
   Real(Kind=rp), Parameter :: p(6) = & ! - coefficients for approximation to error_f in third interval -
      [3.05326634961232344e-1_rp, 3.60344899949804439e-1_rp, 1.25781726111229246e-1_rp, 1.60837851487422766e-2_rp, &
       6.58749161529837803e-4_rp, 1.63153871373020978e-2_rp]
   Real(Kind=rp), Parameter :: q(5) = & ! - coefficients for approximation to error_f in third interval -
      [2.56852019228982242e00_rp, 1.87295284992346047e00_rp, 5.27905102951428412e-1_rp, 6.05183413124413191e-2_rp, &
       2.33520497626869185e-3_rp]
!
! Local scalars
   Integer :: i ! - polynomial index -
!
   Real(Kind=rp) :: y ! - absolute value of argument -
   Real(Kind=rp) :: del
   Real(Kind=rp) :: xnum
   Real(Kind=rp) :: xden
   Real(Kind=rp) :: ysq
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Aint, Exp
!
! Executable Statements
!
! Evaluate error_f for |X|  <=  0.46875
   y = Abs(x)
   If (y < thresh) Then
      If (y > eps) Then
         ysq = y*y
      Else
         ysq = zero
      End If
      xnum = a(5)*ysq
      xden = ysq
      Do i = 1, 3
         xnum = (xnum + a(i))*ysq
         xden = (xden + b(i))*ysq
      End Do
      error_f = x*(xnum + a(4))/(xden + b(4))
      error_f = one - error_f
      Return
!
! Evaluate error_f for 0.46875  <=  |X|  <=  4.0
   Else If (y < four) Then
      xnum = c(9)*y
      xden = y
      Do i = 1, 7
         xnum = (xnum + c(i))*y
         xden = (xden + d(i))*y
      End Do
      error_f = (xnum + c(8))/(xden + d(8))
      ysq = Aint(y*sixten)/sixten
      del = (y - ysq)*(y + ysq)
      error_f = Exp(-ysq*ysq)*Exp(-del)*error_f
!
! Evaluate error_f for |X| > 4.0
   Else
      error_f = zero
      If (y < xbig) Then
         ysq = one/(y*y)
         xnum = p(6)*ysq
         xden = ysq
         Do i = 1, 4
            xnum = (xnum + p(i))*ysq
            xden = (xden + q(i))*ysq
         End Do
         error_f = ysq*(xnum + p(5))/(xden + q(5))
         error_f = (sqrpi - error_f)/y
         ysq = Aint(y*sixten)/sixten
         del = (y - ysq)*(y + ysq)
         error_f = Exp(-ysq*ysq)*Exp(-del)*error_f
      End If
   End If
!
! Fix up for negative argument
   If (x < zero) error_f = two - error_f
!
   Return
 End Function error_f
!
!
!
 Function close_distribs() &
          Result (fclose)
!
! Frees memory allocated for distributions
!
! Function result
   Integer :: fclose
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Free memory allocated for distributions
   fclose = 0
   If (Allocated(pcen))  Deallocate (pcen)
   If (Allocated(beta))  Deallocate (beta)
   If (Allocated(alpha)) Deallocate (alpha)
!
   Return
 End Function close_distribs
!
!
!
 Subroutine proj_read_distribs (iin, igauss, ifail)
!
! Reads distribution parameters from project file
!
! Modules
   Use data_cpt_constants, Only: it_gam
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: igauss ! - transformation option -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read parameters
   If (igauss == it_gam) Then
      Read (Unit=iin, Err=1, End=2) alpha
      Read (Unit=iin, Err=1, End=2) beta
      Read (Unit=iin, Err=1, End=2) pcen
   End If
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
 End Subroutine proj_read_distribs
!
!
!
 Subroutine proj_write_distribs (iout, igauss, ifail)
!
! Writes distribution parameters to project file
!
! Modules
   Use data_cpt_constants, Only: it_gam
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout   ! - output file unit number -
   Integer, Intent(In) :: igauss ! - transformation option -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write distribution parameters
   If (igauss == it_gam) Then
      Write (Unit=iout, Err=1) alpha
      Write (Unit=iout, Err=1) beta
      Write (Unit=iout, Err=1) pcen
   End If
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_distribs
End Module distribs
