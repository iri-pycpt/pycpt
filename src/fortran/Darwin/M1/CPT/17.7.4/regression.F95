! Linear regression and generalized linear models (GLMs)
! Author: Simon Mason
Module regression
!
! Declarations
!
! Modules
   Use data_numbers, Only: rp
   Use maths,        Only: interval
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: calc_pesd, calc_reg_probs, deviance_normal, deviance_bernoulli, deviance_binomial, deviance_poisson, deviance_gamma, &
             fit_glm_bernoulli, fit_glm_binomial, fit_glm_gamma, fit_glm_poisson, fit_regr1, fit_regrm, glm_finv_link,            &
             proj_read_regression, proj_write_regression, reset_regression, varadjust_b
!
! Scalars
!
! Integer scalars
   Integer, Private :: lmn        ! - minimum of nxa and nu -
   Integer, Private :: miter = 20 ! - maximum number of iterations for IRLS -
   Integer, Private :: mr = 0     ! - current m -
   Integer, Private :: nr = 0     ! - current n -
!
! Real scalars
   Real(Kind=rp), Public :: dofr ! - degrees of freedom for regression -
!
! Arrays
!
! Real arrays
   Real(Kind=rp), Dimension(:), Allocatable, Public :: c   ! - climatological probabilities of exceedance -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: e   ! - empirical probabilities of exceedance -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: f   ! - probabilities of exceedance given forecast -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: ys2 ! - variance of hindcast errors -
!
   Real(Kind=rp), Dimension(:), Allocatable, Private :: br  ! - principal component regression coefficients -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: eta ! - linear model -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: svr ! - singular values -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: wt  ! - weights -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: ymu ! - estimated values (mu) -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: yk  ! - copy of dependent data -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: z   ! - adjusted dependent variable -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: b  ! - regression coefficients -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: b2 ! - regression coefficients -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: b0 ! - regression constants -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: fcastt ! - transformed forecast values -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: pesd_p ! - standard deviations of prediction errors -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: pesd_r ! - standard deviations of retroactive prediction errors -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: pesd_2 ! - standard deviations of double cross-validated prediction errors -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: xvp    ! - error variance from predictors -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: ycvt   ! - transformed cross-validated hindcasts -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: ycv2t  ! - transformed double cross-validated predictions -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: yfitt  ! - transformed fitted values of response variables -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: yrett  ! - transformed retroactive predictions -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Private :: eofr ! - loadings -
   Real(Kind=rp), Dimension(:,:), Allocatable, Private :: tsr  ! - time-scores (transposed) -
   Real(Kind=rp), Dimension(:,:), Allocatable, Private :: xk   ! - copy of independent data -
!
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ens_p ! - ensemble forecasts -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ens_r ! - retroactive ensemble predictions -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ens_2 ! - double cross-validated ensemble predictions -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: fcast ! - forecast values -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: u     ! - workspace for xvp calculation -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ycv   ! - cross-validated hindcasts -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ycv2  ! - double cross-validated predictions -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: yfit  ! - fitted values of response variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: yopt  ! - cross-validated hindcasts for optimization -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: yret  ! - retroactive predictions -
!
! Derived-type arrays
   Type(interval), Dimension(:,:), Allocatable, Public :: pls_p ! - prediction limits -
   Type(interval), Dimension(:,:), Allocatable, Public :: pls_r ! - retroactive forecast prediction limits -
   Type(interval), Dimension(:,:), Allocatable, Public :: pls_2 ! - double cross-validated forecast prediction limits -
!
! Workspace
!
! Workspace dimensions
   Integer, Public :: liwk ! - integer workspace dimensions -
   Integer, Public :: lrwk ! - real workspace dimensions -
!
! Integer workspace
   Integer, Dimension(:), Allocatable, Private :: iwk ! - integer workspace -
!
! Real workspace
   Real(Kind=rp), Dimension(:), Allocatable, Private :: rwk ! - default precision workspace -
!
! Explicit Interfaces
!
Contains
!
!
 Subroutine reset_regression ()
!
! Initialises regression
   Use data_numbers, Only: zero
!
! Executable Statements
!
! Initialise regression
   dofr = zero
!
   Return
 End Subroutine reset_regression
!
!
!
 Function init_regr(m, n) &
          Result (init)
!
! Initialises memory for multiple regression
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of locations / variables -
   Integer, Intent(In) :: n ! - number of time-steps -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Calculate minimum dimension
   mr = m + 1
   nr = n
   lmn = Min(mr, nr)
!
! Allocate memory
! - copy of independent data -
   Allocate (xk(mr,nr), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - copy of dependent data -
   Allocate (yk(nr), Stat=ifail)
   If (ifail /= 0) GoTo 1
! Initialise memory to calculate principal components
   Allocate (svr(lmn), Stat=ifail)
   If (ifail /= 0) GoTo 1
! 	- independent variables EOF patterns -
   Allocate (eofr(mr,lmn), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - independent variables EOF scores -
   Allocate (tsr(lmn,nr), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - PC regression coefficients -
   Allocate (br(lmn), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
! Allocate workspace
! - real workspace -
   lrwk = 4*lmn**2 + 7*lmn 
   Allocate (rwk(lrwk), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - integer workspace -
   liwk = 8*lmn
   Allocate (iwk(liwk), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
1  init = ifail
!
   Return
 End Function init_regr
!
!
!
 Function init_glm(n) &
          Result (init)
!
! Initialises generalised linear modelling
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
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - linear model -
   Allocate (eta(n), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - estimated values (mu) -
   Allocate (ymu(n), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - weights -
   Allocate (wt(n), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - adjusted dependent variable -
   Allocate (z(n), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   init = 0
   Return
!
! Errors
1  init = 1
!
   Return
 End Function init_glm
!
!
!
 Subroutine fit_regr1 (n, x, y, b0, b1, &
            wt)
!
! Estimates simple linear regression parameters
!
! Modules
   Use data_numbers, Only: zero, eps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - independent data; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent data; shape(n) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
   Real(Kind=rp), Intent(Out) :: b1 ! - regression coefficient -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: wt ! - weights; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: xbar  ! - mean -
   Real(Kind=rp) :: ybar  ! - mean -
   Real(Kind=rp) :: df    ! - degrees of freedom -
   Real(Kind=rp) :: sxx   ! - sum of squares of independent variable -
   Real(Kind=rp) :: sxy   ! - sum of cross products -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Dot_Product, Present, Real, Sum
!
! Executable Statements
!
! Fit regression line
! - unweighted -
   If (.not.Present(wt)) Then
      df = Real(n, Kind=rp)
      xbar = Sum(x(1:n))/df
      ybar = Sum(y(1:n))/df
      sxx = Dot_Product((x(1:n) - xbar), (x(1:n) - xbar))
      sxy = Dot_Product((x(1:n) - xbar), (y(1:n) - ybar))
! - weighted -
   Else
      df = Sum(wt(1:n))
      If (df < eps) GoTo 1
      xbar = Dot_Product(x(1:n), wt(1:n))/df
      ybar = Dot_Product(y(1:n), wt(1:n))/df
      sxx = Dot_Product((x(1:n) - xbar)**2, wt(1:n))
      sxy = Dot_Product((x(1:n) - xbar)*(y(1:n) - ybar), wt(1:n))
   End If
! - calculate regression parameters -
   If (sxx > eps) Then
      b1 = sxy/sxx
      b0 = ybar - b1*xbar
   Else
      b1 = zero
      b0 = ybar
   End If
!
   Return
!
! Unsolvable (sum of weights is too small)
1  b1 = zero
   b0 = zero
!
   Return
 End Subroutine fit_regr1
!
!
 Subroutine fit_regrm (m, n, x, y, b0, b, ifail, &
            wt, u)
!
! Estimates multiple linear regression parameters
!
! On exit:
!    ifail =  0 No errors
!    ifail =  1 Insufficieint memory allocated
!
! Modules
   Use data_numbers, Only: zero, tol, one
   Use cpt_lapack,   Only: gesdd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of independent variables -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - dependent data; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - independent data; shape(m,n) -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b ! - regression coefficient; shape(m) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: wt ! - weights; shape(n) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values; shape(m+1,m+1) -
!
! Locals
!
! Local scalars
   Integer :: ie  ! - singular value index -
   Integer :: npe ! - number of positive eigenvalues -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic All, Count, Dot_Product, MatMul, Present, Real, Sqrt, Sum
!
! Executable Statements
!
! Allocate memory
   b0 = zero
   b(1:m) = zero
   ifail = 1
   If (init_regr(m, n) /= 0) GoTo 1
!
! Perform multiple linear regression
   xk(1,:) = one
   xk(2:,:) = x(1:m,1:n)
! - check for variance -
   If (All(y(2:n) == y(1))) GoTo 1
! - apply weights -
   If (Present(wt)) Then
      yk(:) = y(1:n)
      Call apply_weight (mr, wt(1:n), xk(:,:), yk(:)) ! - apply weights -
   End If
   svr(:) = zero
! - perform SVD -
   ifail = 0
   Call gesdd ('S', mr, n, xk(:,:), mr, svr(:), eofr(:,:), mr, tsr(:,:), lmn, rwk(:), lrwk, iwk(:), ifail)
   If (ifail /= 0) GoTo 1
   npe = Count(svr(1:lmn) > tol)
! - scale the temporal scores -
   Do ie = 1, npe
      tsr(ie,:) = tsr(ie,:)/svr(ie)
   End Do
! - calculate regression coefficients -
   If (.not.Present(wt)) Then
      br(1:npe) = MatMul(tsr(1:npe,:), y(1:n))
   Else
      br(1:npe) = MatMul(tsr(1:npe,:), yk(:))
   End If
   b0 = Dot_Product(eofr(1,1:npe), br(1:npe))
   b(:) = MatMul(eofr(2:,1:npe), br(1:npe))
! - calculate scaled loadings if required -
   If (Present(u)) Then
      If (Present(wt)) svr(:) = svr(:)*Sqrt(Real(n, Kind=rp)/Sum(wt(1:n)))
      u(1:mr,1:npe) = eofr(1:mr,1:npe)
      Do ie = 1, npe
         u(1:mr,ie) = u(1:mr,ie)/svr(ie)
      End Do
      If (npe < mr) u(1:mr,ie) = zero
   End If
!
! Clear memory
1  ie = close_regr()
!
! Errors
   If (ifail /= 0) Then
      b0 = Sum(y(1:n))/Real(n, Kind=rp)
      b(:) = zero
      If (Present(u)) Then
         u(:,:) = zero
         u(1,1) = one/Sqrt(Real(n, Kind=rp))
      End If
      ifail = 1
      Return
   End If
!
   Return
!
 Contains
!
!
  Subroutine apply_weight (m, wt, x, y)
!
! Apply weights for weighted regression
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of x variables -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: wt ! - weights; shape(n) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:), Intent(InOut) :: y ! - dependent variable; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: x ! - explanatory variables; shape(m,n) -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sqrt
!
! Executabe Statements
!
! Rescale using the square-root of the weights
   y(:) = Sqrt(wt(:))*y(:)
   Do i = 1, m
      x(i,:) = Sqrt(wt(:))*x(i,:)
   End Do
!
   Return
  End Subroutine apply_weight
 End Subroutine fit_regrm
!
!
!
 Subroutine fit_glm_bernoulli (m, n, x, y, b0, b, ifail, &
            u)
!
! Constructs a GLM with bernoulli errors using a logit link function
!
! Modules
   Use data_numbers, Only: tol, one, rhuge
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of x spatial points -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - response variables; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables; shape(m,n) -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b ! - regression coefficients; shape(m) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: iter ! - iteration index -
!
   Real(Kind=rp) :: dmin ! - minimum deviance -
   Real(Kind=rp) :: d    ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Real, Sum
!
! Executable Statements
!
! Initialise
   If (init_glm(n) /= 0) Stop
   dmin = rhuge
   ymu(:) = Sum(y(1:n))/Real(n, Kind=rp)
   eta(:) = f_link_logit(ymu(:), one)
!
! Perform iterative re-weighted least squares regression
   Do iter = 1, miter
      wt(:) = ymu(:)*(one - ymu(:))                              ! - define weights -
      z(:) = eta(:) + (y(1:n) - ymu(:))/wt(:)                    ! - calculate adjusted dependent variable -
      Call fit_regrm (m, n, x(1:m,1:n), z(:), b0, b(:), ifail, & ! - perform weighted regression -
           wt=wt(:), u=u)
      eta(:) = MatMul(b(1:m), x(1:m,1:n)) + b0                   ! - calculate linear model 
      ymu(:) = finv_link_logit(eta(:), one)                      ! - invert link function -
      d = deviance_bernoulli(n, y(1:n), ymu(:))                  ! - calculate deviance -
      If (d <= dmin .and. dmin - d < tol) Exit                   ! - exit if deviance is not reduced much -
      dmin = d
   End Do
   ifail = close_glm()
!
   Return
 End Subroutine fit_glm_bernoulli
!
!
!
 Subroutine fit_glm_binomial (m, n, x, y, t, b0, b, ifail, &
            u)
!
! Constructs a GLM with binomial errors using a logit link function
!
! Modules
   Use data_numbers, Only: eps, tol, rhuge
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of x spatial points -
   Integer, Intent(In) :: n ! - number of cases -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: t ! - binomial denominator; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - response variables; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables; shape(m,n) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b ! - regression coefficients; shape(m) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: iter  ! - iteration index -
   Integer :: niter ! - number of iterations to use -
!
   Real(Kind=rp) :: dmin ! - minimum deviance -
   Real(Kind=rp) :: d    ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Real, Sum
!
! Executable Statements
!
! Initialise
   If (init_glm(n) /= 0) Stop
   dmin = rhuge
   ymu(:) = Sum(y(1:n))/Real(n, Kind=rp)
   eta(:) = f_link_logit(ymu(:), t(:))
   If (ymu(1) < eps) Then ! - check whether there are too few counts -
      ymu(:) = 1.0d0/Real(n+1, Kind=rp)
      niter = 1
   Else
      niter = miter
   End If
!
! Perform iterative re-weighted least squares regression
   Do iter = 1, niter
      wt(:) = ymu(:)*(t(:) - ymu(:))/t(:)                        ! - define weights -
      Where (wt(:) < eps) wt(:) = eps
      z(:) = eta(:) + (y(1:n) - ymu(:))/wt(:)                    ! - calculate adjusted dependent variable -
      Call fit_regrm (m, n, x(1:m,1:n), z(:), b0, b(:), ifail, & ! - perform weighted regression -
           wt=wt(:), u=u)
      eta(:) = MatMul(b(1:m), x(1:m,1:n)) + b0                   ! - calculate linear model 
      ymu(:) = finv_link_logit(eta(:), t(:))                     ! - invert link function -
      d = deviance_binomial(n, y(1:n), ymu(:), t(:))             ! - calculate deviance -
      If (d <= dmin .and. dmin - d < tol) Exit                   ! - exit if deviance is not reduced much -
      dmin = d
   End Do
   ifail = close_glm()
!
   Return
 End Subroutine fit_glm_binomial
!
!
!
 Subroutine fit_glm_gamma (ilink, m, n, x, y, b0, b, ifail, &
            u)
!
! Constructs a GLM with gamma errors using an inverse link function
!
! Modules
   Use data_numbers,       Only: eps, tol, one, two, rhuge
   Use data_cpt_constants, Only: il_idt, il_inv, il_log, il_sqr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilink ! - link function -
   Integer, Intent(In) :: m     ! - number of x spatial points -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - response variables; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables; shape(m,n) -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b ! - regression coefficients; shape(m) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: k    ! - case index -
   Integer :: iter ! - iteration index -
!
   Real(Kind=rp) :: dmin    ! - minimum deviance -
   Real(Kind=rp) :: d       ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Sqrt
!
! Executable Statements
!
! Initialise
   If (init_glm(n) /= 0) Stop
   dmin = rhuge
   ymu(:) = y(1:n)
   Where (ymu(:) < eps) ymu(:) = eps
   Select Case (ilink) ! - apply link function -
    Case (il_idt) ! - identity
      eta(:) = ymu(:)
      z(:) = y(1:n)
    Case (il_inv) ! - inverse -
      eta(:) = f_link_inverse(ymu(:))
    Case (il_log) ! - logarithmic -
      eta(:) = f_link_log(ymu(:))
      wt(:) = one
    Case (il_sqr) ! - square root -
      eta(:) = f_link_sqr(ymu(:))
   End Select
!
! Perform iterative re-weighted least squares regression
! - define weights -
   Do iter = 1, miter
      Select Case (ilink)
       Case (il_idt) ! - identity
         Where (ymu(:) > eps)
            wt(:) = one/ymu(:)**2
         Else Where
            wt(:) = one/eps
         End Where
       Case (il_inv) ! - inverse -
         wt(:) = ymu(:)**2
         Do k = 1, n ! - calculate adjusted dependent variable -
            If (wt(k) > eps) Then
               z(k) = eta(k) - (y(k) - ymu(k))/wt(k)
            Else
               z(k) = eta(k) - (y(k) - ymu(k))/eps
            End If
         End Do
       Case (il_log) ! - logarithmic -
         z(:) = eta(:) + (y(1:n) - ymu(:))/ymu(:) ! - calculate adjusted dependent variable -
       Case (il_sqr) ! - square root -
         Where (ymu(:) > eps)
            wt(:) = one/ymu(:)
         Else Where
            wt(:) = one/eps
         End Where
         z(:) = eta(:) + (y(1:n) - ymu(:))/(two*Sqrt(ymu(:))) ! - calculate adjusted dependent variable -
      End Select
! - perform weighted regression -
      Call fit_regrm (m, n, x(1:m,1:n), z(:), b0, b(:), ifail, &
           wt=wt(:), u=u)
! - calculate linear model -
      eta(:) = MatMul(b(1:m), x(1:m,1:n)) + b0
! - invert link function -
      Select Case (ilink)
       Case (il_idt) ! - identity
         Where (eta(:) < eps) eta(:) = eps
         ymu(:) = eta(:)
       Case (il_inv) ! - inverse -
         Where (eta(:) < eps) eta(:) = eps
         ymu(:) = finv_link_inverse(eta(:))
       Case (il_log) ! - logarithmic -
         ymu(:) = finv_link_log(eta(:))
       Case (il_sqr) ! - logarithmic -
         ymu(:) = finv_link_sqr(eta(:))
      End Select
      Where (ymu(:) < eps) ymu(:) = eps
! - calculate adjusted deviance -
      d = adj_deviance_gamma(n, y(1:n), ymu(:))
! - exit if deviance is not reduced much -
      If (d <= dmin .and. dmin - d < tol) Exit
      dmin = d
   End Do
   ifail = close_glm()
!
   Return
 End Subroutine fit_glm_gamma
!
!
!
 Subroutine fit_glm_poisson (ilink, m, n, x, y, b0, b, ifail, &
            u)
!
! Constructs a GLM with poisson errors using a log-link function
!
! Modules
   Use data_numbers,       Only: eps, tol, one, two, rhuge
   Use data_cpt_constants, Only: il_idt, il_inv, il_log, il_sqr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilink ! - link function -
   Integer, Intent(In) :: m     ! - number of x spatial points -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - response variables; shape(n) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables; shape(m,n) -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b ! - regression coefficients; shape(m) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: iter ! - iteration index -
!
   Real(Kind=rp) :: dmin ! - minimum deviance -
   Real(Kind=rp) :: d    ! - deviance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Sqrt
!
! Executable Statements
!
! Initialise
   If (init_glm(n) /= 0) Stop
   dmin = rhuge
   ymu(:) = y(1:n)
   Where (ymu(:) < eps) ymu(:) = eps
   Select Case (ilink) ! - apply link function -
    Case (il_idt) ! - identity
      eta(:) = ymu(:)
      z(:) = y(1:n)
    Case (il_inv) ! - inverse -
      eta(:) = f_link_inverse(ymu(:))
    Case (il_log) ! - logarithmic -
      eta(:) = f_link_log(ymu(:))
    Case (il_sqr) ! - square root -
      wt(:) = one
      eta(:) = f_link_sqr(ymu(:))
   End Select
!
! Perform iterative re-weighted least squares regression
! - define weights -
   Do iter = 1, miter
      Select Case (ilink)
       Case (il_idt) ! - identity
         wt(:) = one/ymu(:)
       Case (il_inv) ! - inverse -
         wt(:) = ymu(:)**3
         z(:) = eta(:) - (y(1:n) - ymu(:))/ymu(:)**2 ! - calculate adjusted dependent variable -
       Case (il_log) ! - logarithmic -
         wt(:) = ymu(:)
         z(:) = eta(:) + (y(1:n) - ymu(:))/wt(:) ! - calculate adjusted dependent variable -
       Case (il_sqr) ! - square root -
         z(:) = eta(:) + (y(1:n) - ymu(:))/(two*Sqrt(ymu(:))) ! - calculate adjusted dependent variable -
      End Select
! - perform weighted regression -
      Call fit_regrm (m, n, x(1:m,1:n), z(:), b0, b(:), ifail, &
           wt=wt(:), u=u)
! - calculate linear model -
      eta(:) = MatMul(b(1:m), x(1:m,1:n)) + b0
! - invert link function -
      Select Case (ilink)
       Case (il_idt) ! - identity
         Where (eta(:) < eps) eta(:) = eps
         ymu(:) = eta(:)
       Case (il_inv) ! - inverse -
         Where (eta(:) < eps) eta(:) = eps
         ymu(:) = finv_link_inverse(eta(:))
       Case (il_log) ! - logarithmic -
         ymu(:) = finv_link_log(eta(:))
       Case (il_sqr) ! - logarithmic -
         ymu(:) = finv_link_sqr(eta(:))
      End Select
      Where (ymu(:) < eps) ymu(:) = eps
! - calculate deviance -
      d = deviance_poisson(n, y(1:n), ymu(:))
! - exit if deviance is not reduced much -
      If (d <= dmin .and. dmin - d < tol) Exit
      dmin = d
   End Do
   ifail = close_glm()
!
   Return
 End Subroutine fit_glm_poisson
!
!
!
 Function deviance_bernoulli(n, y, ymu) &
          Result (deviance)
!
! Calculates deviance
!
! Modules
   Use data_numbers, Only: zero, eps, one, two, rhuge
!
! Function result
   Real(Kind=rp) :: deviance
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observed values; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ymu ! - esimated values; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - index -
!
   Real(Kind=rp) :: yc   ! - complement of y -
   Real(Kind=rp) :: ymuc ! - complement of ymu -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log
!
! Executable Statements
!
! Calculate deviance
   deviance = zero
   Do k = 1, n
      yc = one - y(k)
      ymuc = one - ymu(k)
      If (y(k) > eps) Then
         If (ymu(k) > eps) Then
            deviance = deviance + y(k)*Log(y(k)/ymu(k))
         Else
            deviance = rhuge
            Exit
         End If
      End If
      If (yc > eps) Then
         If (ymuc > eps) then
            deviance = deviance + yc*Log(yc/ymuc)
         Else
            deviance = rhuge
           Exit
         End If
      End If
   End Do
   deviance = two*deviance
!
   Return
 End Function deviance_bernoulli
!
!
!
 Function deviance_binomial(n, y, ymu, t) &
          Result (deviance)
!
! Calculates deviance
!
! Modules
   Use data_numbers, Only: zero, eps, two, rhuge
!
! Function result
   Real(Kind=rp) :: deviance
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: t   ! - binomial denominator; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observed values; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ymu ! - esimated values; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - index -
!
   Real(Kind=rp) :: yc   ! - complement of y -
   Real(Kind=rp) :: ymuc ! - complement of ymu -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log
!
! Executable Statements
!
! Calculate deviance
   deviance = zero
   Do k = 1, n
      yc = t(k) - y(k)
      ymuc = t(k) - ymu(k)
      If (y(k) > eps) Then
         If (ymu(k) > eps) Then
            deviance = deviance + y(k)*Log(y(k)/ymu(k))
         Else
            deviance = rhuge
            Return
         End If
      End If
      If (yc > eps) Then
         If (ymuc > eps) then
            deviance = deviance + yc*Log(yc/ymuc)
         Else
            deviance = rhuge
            Return
         End If
      End If
   End Do
   deviance = two*deviance
!
   Return
 End Function deviance_binomial
!
!
!
 Function deviance_gamma(n, y, ymu) &
          Result (deviance)
!
! Calculates deviance
!
! Modules
   Use data_numbers, Only: zero, eps, two
!
! Function result
   Real(Kind=rp) :: deviance
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observed values; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ymu ! - esimated values; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Log
!
! Executable Statements
!
! Calculate deviance
   deviance = zero
   Do k = 1, n
      If (ymu(k) > eps) Then
         deviance = deviance + (y(k) - ymu(k))/ymu(k)
      Else If (Abs(y(k) - ymu(k)) > eps) Then
         deviance = deviance + (y(k) - ymu(k))/eps
      End If
      If (y(k) > eps) Then
         If (ymu(k) > eps) Then
            deviance = deviance - Log(y(k)/ymu(k))
         Else
            deviance = deviance - Log(y(k)/eps)
         End If
      End If
   End Do
   deviance = two*deviance
!
   Return
 End Function deviance_gamma
!
!
!
 Function adj_deviance_gamma(n, y, ymu) &
          Result (deviance)
!
! Calculates deviance adjusted for y = 0
!
! Modules
   Use data_numbers, Only: zero, eps, two
!
! Function result
   Real(Kind=rp) :: deviance
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observed values; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ymu ! - esimated values; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log
!
! Executable Statements
!
! Calculate deviance
   deviance = zero
   Do k = 1, n
      If (ymu(k) > eps) Then
         deviance = deviance + y(k)/ymu(k) + Log(ymu(k))
      Else
         deviance = deviance + y(k)/eps + Log(eps)
      End If
   End Do
   deviance = two*deviance
!
   Return
 End Function adj_deviance_gamma
!
!
!
 Function deviance_normal(n, y, ymu) &
          Result (deviance)
!
! Calculates deviance
!
! Function result
   Real(Kind=rp) :: deviance
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observed values; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ymu ! - esimated values; shape(n) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Calculate deviance
   deviance = Sum((y(1:n) - ymu(1:n))**2)
!
   Return
 End Function deviance_normal
!
!
!
 Function deviance_poisson(n, y, ymu) &
          Result (deviance)
!
! Calculates deviance
!
! Modules
   Use data_numbers, Only: eps, two
!
! Function result
   Real(Kind=rp) :: deviance
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y   ! - observed values; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: ymu ! - esimated values; shape(n) -
!
! Locals
!
! Local scalars
   Integer :: k ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log, Sum
!
! Executable Statements
!
! Calculate deviance
   deviance = Sum(y(:) - ymu(:))
   Do k = 1, n
      If (y(k) > eps) Then
         If (ymu(k) > eps) Then
            deviance = deviance + y(k)*Log(y(k)/ymu(k))
         Else
            deviance = deviance + y(k)*Log(y(k)/eps)
         End If
      End If
   End Do
   deviance = two*deviance
!
   Return
 End Function deviance_poisson
!
!
!
 Elemental Function f_link_logit(y, t) &
           Result (f_link)
!
! Logit link function
!
! Modules
   Use data_numbers, Only: eps
!
! Function result
   Real(Kind=rp) :: f_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: t ! - binomial denominator -
   Real(Kind=rp), Intent(In) :: y ! - value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log
!
! Executable Statements
!
! Apply link function
   If (y > eps) Then
      If (t - y > eps) Then
         f_link = Log(y/(t - y))
      Else
         f_link = Log(t/(t - eps))
      End If
   Else
      f_link = Log(eps/(t - eps))
   End If
!
   Return
 End Function f_link_logit
!
!
!
 Elemental Function f_link_inverse(y) &
           Result (f_link)
!
! Inverse link function
!
! Modules
   Use data_numbers, Only: eps, one
!
! Function result
   Real(Kind=rp) :: f_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: y ! - value -
!
! Executable Statements
!
! Apply link function
   If (y > eps) Then
      f_link = one/y
   Else
      f_link = one/eps
   End If
!
   Return
 End Function f_link_inverse
!
!
!
 Elemental Function f_link_log(y) &
           Result (f_link)
!
! Logarithmic link function
!
! Modules
   Use data_numbers, Only: eps
!
! Function result
   Real(Kind=rp) :: f_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: y ! - value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Log
!
! Executable Statements
!
! Apply link function
   If (y > eps) Then
      f_link = Log(y)
   Else
      f_link = Log(eps)
   End If
!
   Return
 End Function f_link_log
!
!
!
 Elemental Function f_link_sqr(y) &
           Result (f_link)
!
! Square-root link function
!
! Modules
   Use data_numbers, Only: eps
!
! Function result
   Real(Kind=rp) :: f_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: y ! - value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sqrt
!
! Executable Statements
!
! Apply link function
   If (y > eps) Then
      f_link = Sqrt(y)
   Else
      f_link = eps
   End If
!
   Return
 End Function f_link_sqr
!
!
!
 Subroutine glm_finv_link (ilink, n, y, &
            t)
!
! Apply inverse link function
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: il_idt, il_lgt, il_lgm, il_inv, il_log, il_sqr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilink ! - link function -
   Integer, Intent(In) :: n     ! - number of cases -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - data -
!
! Optional arguments
! - input scalars -
   Real(Kind=rp), Intent(In), Dimension(:), Optional :: t ! - binomial coefficients -
!
! Locals
!
! Local scalars
   Integer :: k ! - index -
!
! Executable Statements
!
! Apply inverse link function
   Select Case (ilink)
    Case (il_idt)
      Continue
    Case (il_lgt) ! - logit -
      Do k = 1, n
         y(:,k) = finv_link_logit(y(:,k), one)
      End Do
    Case (il_lgm) ! - multinomial logit -
      Do k = 1, n
         y(:,k) = finv_link_logit(y(:,k), t(k))
      End Do
    Case (il_inv) ! - inverse -
      Do k = 1, n
         y(:,k) = finv_link_inverse(y(:,k))
      End Do
    Case (il_log) ! - logarithmic -
      Do k = 1, n
         y(:,k) = finv_link_log(y(:,k))
      End Do
    Case (il_sqr) ! - square-root -
      Do k = 1, n
         y(:,k) = finv_link_sqr(y(:,k))
      End Do
   End Select
!
   Return
 End Subroutine glm_finv_link
!
!
!
 Elemental Function finv_link_logit(eta, t) &
           Result (finv_link)
!
! Inverse logit link function
!
! Modules
   Use data_numbers, Only: one, elim
!
! Function result
   Real(Kind=rp) :: finv_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: eta ! - linear predictor -
   Real(Kind=rp), Intent(In) :: t   ! - binomial denominator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Exp
!
! Executable Statements
!
! Apply inverse link function
   If (Abs(eta) < elim) Then
      finv_link = t/(one + Exp(-eta))
   Else
      finv_link = t/(one + Exp(-elim))
   End if
!
   Return
 End Function finv_link_logit
!
!
!
 Elemental Function finv_link_inverse(eta) &
           Result (finv_link)
!
! Inverse inverse link function
!
! Modules
   Use data_numbers, Only: eps, one
!
! Function result
   Real(Kind=rp) :: finv_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: eta ! - linear predictor -
!
! Executable Statements
!
! Apply link function
   If (eta > eps) Then
      finv_link = one/eta
   Else
      finv_link = one/eps
   End If
!
   Return
 End Function finv_link_inverse
!
!
!
 Elemental Function finv_link_log(eta) &
           Result (finv_link)
!
! Logarithmic inverse link function
!
! Modules
   Use data_numbers, Only: elim
!
! Function result
   Real(Kind=rp) :: finv_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: eta ! - linear predictor -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Exp
!
! Executable Statements
!
! Apply inverse link function
   If (eta < elim) Then
      finv_link = Exp(eta)
   Else
      finv_link = Exp(elim)
   End If
!
   Return
 End Function finv_link_log
!
!
!
 Elemental Function finv_link_sqr(eta) &
           Result (finv_link)
!
! Square-root inverse link function
!
! Modules
   Use data_numbers, Only: eps
!
! Function result
   Real(Kind=rp) :: finv_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: eta ! - linear predictor -
!
! Executable Statements
!
! Apply inverse link function
   If (eta > eps) Then
      finv_link = eta**2
   Else
      finv_link = eps
   End If
!
   Return
 End Function finv_link_sqr
!
!
!
 Subroutine calc_pesd (n, m, nf, ny, lgcm, s2, xvp, dofr, pesd)
!
! Calculates standard deviations of prediction errors
!
! Modules
  Use data_numbers, Only: zero, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m  ! - number of independent variables -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: nf ! - number of forecasts -
   Integer, Intent(In) :: ny ! - number of predictands -
!
   Logical, Intent(In) :: lgcm ! - GCM analysis? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: s2 ! - variance of prediction errors; shape(ny) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: xvp ! - standard error from predictors; shape(1 or ny,nf) -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: dofr ! - degrees of freedom for regression -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: pesd  ! - standard deviations of prediction errors; shape(ny,nf) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - predictand index -
   Integer :: ii ! - variable index -
   Integer :: k  ! - forecast index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real, Sqrt
!
! Executable Statements
!
! Calculate standard deviations of prediction errors
   ii = 1
   Do i = 1, ny
      Do k = 1, nf
         If (s2(i) > zero) Then
            pesd(i,k) = Sqrt(s2(i)*(one + xvp(ii,k)))
         Else
            pesd(i,k) = zero
         End If
      End Do
      If (lgcm) ii = ii + 1
   End Do
!
! Calculate degrees of freedom
   dofr = Real(n - m - 1, Kind=rp)
!
   Return
 End Subroutine calc_pesd
!
!
!
 Subroutine calc_reg_probs (nf, ng, ny, dofr, clf, pesd, fcast, tobst, fps, fpls, &
            nc, nenf, clim, corrmsk, pobs, fens)
!
! Calculates forecast probabilities and prediction intervals
!
! Modules
   Use data_numbers,   Only: zero, eps, one, two, oneh
   Use maths,          Only: interval
   Use progress_meter, Only: set_progress_increment, update_progress_meter
   Use settings,       Only: rskmask
   Use arrays,         Only: rwk, &
                             insertion_sort
   Use distribs,       Only: q_empirical, q_student_t_2, s_student_t
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf ! - number of forecasts -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: ny ! - number of predictands -
!
   Real(Kind=rp), Intent(In) :: dofr ! - degrees of freedom for regression -
   Real(Kind=rp), Intent(In) :: clf  ! - forecast confidence level -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: fcast ! - forecast values -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: tobst ! - thresholds -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: pesd  ! - prediction-error standard deviations -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: fps ! - forecast probabilities -
!
   Type(interval), Dimension(:,:), Intent(Out) :: fpls ! - prediction limits -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nc   ! - size of climatology -
   Integer, Intent(In), Optional :: nenf ! - number of ensemble members -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: corrmsk ! - ensemble forecasts -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: clim ! - climatological data -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: pobs ! - climatological probabilities -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out), Optional :: fens ! - ensemble forecasts -
!
! Locals
!
! Local scalars
   Integer :: i  ! - variable index -
   Integer :: j  ! - category index -
   Integer :: k  ! - forecast index -
   Integer :: l  ! - category index -
   Integer :: ie ! - ensemble member index -
!
   Logical :: lfps ! - calculate probabilities? -
!
   Real(Kind=rp) :: t  ! - Student's t-deviate -
   Real(Kind=rp) :: t0 ! - lower tail Student's t-deviate -
   Real(Kind=rp) :: p  ! - lower tail Student's t-deviate -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Present, Real
!
! Executable Statements
!
! Calculate critical t-statistic
   If (dofr > zero) Then
      t0 = q_student_t_2(one-clf/oneh, dofr)
   Else
      t0 = zero
   End If
!
! Calculate goodness of fit statistics
   Call set_progress_increment (one/Real(Min(nf, ny), Kind=rp), .true.)
!
! Calculate forecast probabilities
   Do i = 1, ny
      If (Present(corrmsk)) Then
         lfps = (corrmsk(i) > rskmask)
      Else
         lfps = .true.
      End If
      If (lfps) Then
         Do k = 1, nf
            If (pesd(i,k) > eps) Then
               Do j = 1, ng-1
                  t = (tobst(i,j) - fcast(i,k))/pesd(i,k)
                  fps(i,k,j+1) = s_student_t(t, dofr)*oneh
               End Do
               fps(i,k,1) = oneh
               Do j = 1, ng-1
                  fps(i,k,j) = fps(i,k,j) - fps(i,k,j+1)
               End Do
            Else ! - assign 100% to category of deterministic forecast if prediction-error variance is small -
               fps(i,k,:) = zero
               l = 1
               Do j = 1, ng-1
                  If (fcast(i,k) > tobst(i,j)) Then ! - identify category of deterministic forecast -
                     l = l + 1
                  Else
                     Exit
                  End If
               End Do
               fps(i,k,l) = oneh
            End If
!
! Calculate prediction intervals
            t = t0*pesd(i,k)
            fpls(i,k)%lower = fcast(i,k) - t
            fpls(i,k)%upper = fcast(i,k) + t
            If (Present(nenf)) Then
               Do ie = nenf/2+1, nenf
                  p = one - Real(ie, Kind=rp)/Real(nenf+1, Kind=rp)
                  If (dofr > zero) Then
                     t = q_student_t_2(two*p, dofr)*pesd(i,k)
                  Else
                     t = zero
                  End If
                  fens(i,k,ie) = fcast(i,k) + t
                  fens(i,k,nenf+1-ie) = fcast(i,k) - t
               End Do
            End If
!
! Update progress meter
            If (update_progress_meter(.false.) /= 0) Return
         End Do
!
! Assign climatological probabilities if skill is insufficient
      Else
         Do Concurrent (k = 1:nf)
            fps(i,k,:) = pobs(i,:)*oneh
         End Do
         rwk(1:nc) = clim(i,1:nc)
         Call insertion_sort ('a', nc, rwk(1:nc))
         p = (one - clf/oneh)/two
         fpls(i,:)%lower = q_empirical(rwk, nc, p)
         p = one - p
         fpls(i,:)%upper = q_empirical(rwk, nc, p)
         If (Present(nenf)) Then
            Do ie = 1, nenf
               p = Real(ie, Kind=rp)/Real(nenf+1, Kind=rp)
               fens(i,:,ie) = q_empirical(rwk, nc, p)
            End Do
         End If
      End If
   End Do
   Call set_progress_increment (Real(Min(nf, ny), Kind=rp), .true.)
!
   Return
 End Subroutine calc_reg_probs
!
!
!
 Subroutine varadjust_b (xm, ym, xsd, ysd, b0, b1)
!
! Calculates regression parameters for variance adjustment
!
! Modules
   Use data_numbers, Only: rp, zero, eps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: xm  ! - forecast mean -
   Real(Kind=rp), Intent(In) :: ym  ! - observed mean -
   Real(Kind=rp), Intent(In) :: xsd ! - forecast standard deviation -
   Real(Kind=rp), Intent(In) :: ysd ! - observed standard deviation -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: b0 ! - regression constant -
   Real(Kind=rp), Intent(Out) :: b1 ! - regression coefficient -
!
! Executable Statements
!
! Calculate regression parameters for variance adjustment
   If (xsd > eps) Then
      b1 = ysd/xsd
   Else
      b1 = zero
   End If
   b0 = ym - xm*b1
!
   Return
 End Subroutine varadjust_b
!
!
!
 Function close_regr() &
          Result (fclose)
!
! Clears memory for multiple regression
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
! Clear memory
   If (Allocated(iwk))  Deallocate (iwk)
   If (Allocated(rwk))  Deallocate (rwk)
   If (Allocated(br))   Deallocate (br)
   If (Allocated(tsr))  Deallocate (tsr)
   If (Allocated(eofr)) Deallocate (eofr)
   If (Allocated(svr))  Deallocate (svr)
   If (Allocated(yk))   Deallocate (yk)
   If (Allocated(xk))   Deallocate (xk)
!
! Reset memory settings
   mr = 0
   nr = 0
!
   fclose = 0
!
   Return
 End Function close_regr
!
!
!
 Function close_glm() &
          Result (fclose)
!
! Frees memory allocated for GLM
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
   If (Allocated(z))    Deallocate (z)
   If (Allocated(wt))   Deallocate (wt)
   If (Allocated(ymu))  Deallocate (ymu)
   If (Allocated(eta))  Deallocate (eta)
!
   fclose = 0
!
   Return
 End Function close_glm
!
!
!
 Subroutine proj_read_regression (iin, ianal, igauss, ihind, icv2, iretro, ifail)
!
! Reads results from project file
!
! Modules
   Use data_cpt_constants, Only: ia_pcr, ia_mlr, ia_gcm
   Use arrays,             Only: yt
   Use pcs,                Only: bz
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: ianal  ! - analysis indicator -
   Integer, Intent(In) :: igauss ! - transform to gaussian flag -
   Integer, Intent(In) :: ihind  ! - hindcasts flag -
   Integer, Intent(In) :: icv2   ! - double cross-validation flag -
   Integer, Intent(In) :: iretro ! - retroactive forecast flag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read results
   If (ihind == 1) Then
      Read (Unit=iin, Err=1, End=2) ycv
      Read (Unit=iin, Err=1, End=2) yfit
      If (icv2 /= 0) Then
         Read (Unit=iin, Err=1, End=2) ycv2
         Read (Unit=iin, Err=1, End=2) pls_2
      End If
      If (iretro == 1) Then
         Read (Unit=iin, Err=1, End=2) yret
         Read (Unit=iin, Err=1, End=2) pls_r
      End If
      If (igauss > 0) Then
         Read (Unit=iin, Err=1, End=2) yt
         Read (Unit=iin, Err=1, End=2) ycvt
         Read (Unit=iin, Err=1, End=2) yfitt
         If (iretro == 1) Read (Unit=iin, Err=1, End=2) yrett
         If (icv2 /= 0) Read (Unit=iin, Err=1, End=2) ycv2t
      End If
      Read (Unit=iin, Err=1, End=2) ys2
   End If
!
! Read regression coefficients
   Read (Unit=iin, Err=1, End=2) dofr
   Select Case (ianal)
    Case (ia_pcr) ! - PCR -
      Read (Unit=iin, Err=1, End=2) bz
      Read (Unit=iin, Err=1, End=2) b
      Read (Unit=iin, Err=1, End=2) b0
    Case (ia_mlr, ia_gcm) ! - MLR, GCM -
      Read (Unit=iin, Err=1, End=2) b
      Read (Unit=iin, Err=1, End=2) b0
    End Select
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
 End Subroutine proj_read_regression
!
!
!
 Subroutine proj_write_regression (iout, ianal, igauss, ihind, icv2, iretro, ifail)
!
! Writes results to project file
!
! Modules
   Use data_cpt_constants, Only: ia_pcr, ia_mlr, ia_gcm
   Use arrays,             Only: yt
   Use pcs,                Only: bz
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout   ! - output file unit number -
   Integer, Intent(In) :: ianal  ! - analysis indicator -
   Integer, Intent(In) :: igauss ! - transform to gaussian flag -
   Integer, Intent(In) :: ihind  ! - hindcasts flag -
   Integer, Intent(In) :: icv2   ! - double cross-validation flag -
   Integer, Intent(In) :: iretro ! - retroactive forecast flag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write results
   If (ihind == 1) Then
      Write (Unit=iout, Err=1) ycv
      Write (Unit=iout, Err=1) yfit
      If (icv2 /= 0) Then
         Write (Unit=iout, Err=1) ycv2
         Write (Unit=iout, Err=1) pls_2
      End If
      If (iretro == 1) Then
         Write (Unit=iout, Err=1) yret
         Write (Unit=iout, Err=1) pls_r
      End If
      If (igauss > 0) Then
         Write (Unit=iout, Err=1) yt
         Write (Unit=iout, Err=1) ycvt
         Write (Unit=iout, Err=1) yfitt
         If (iretro == 1) Write (Unit=iout, Err=1) yrett
         If (icv2 /= 0) Write (Unit=iout, Err=1) ycv2t
      End If
      Write (Unit=iout, Err=1) ys2
   End If
!
! Write regression coefficients
   Write (Unit=iout, Err=1) dofr
   Select Case (ianal)
    Case (ia_pcr) ! - PCR -
      Write (Unit=iout, Err=1) bz
      Write (Unit=iout, Err=1) b
      Write (Unit=iout, Err=1) b0
    Case (ia_mlr, ia_gcm) ! - MLR, GCM -
      Write (Unit=iout, Err=1) b
      Write (Unit=iout, Err=1) b0
    End Select
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_regression
End Module regression
