! Multiple Linear Regression (MLR)
! Author: Simon Mason
Module mlr
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cv_mlr, do_mlr, full_mlr, mlr_predict
!
Contains
!
!
 Subroutine full_mlr (nt, nx, x, ny, y, b0, b, u, ifail, &
            t)
!
! Performs MLR using all data
!
! Modules
   Use progress_meter, Only: update_progress_meter
   Use settings,       Only: ilink, iregr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt ! - number of cases -
   Integer, Intent(In) :: nx ! - number of x spatial points -
   Integer, Intent(In) :: ny ! - number of y spatial points -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: x ! - explanatory variables; shape(nx,nt) -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - response variables; shape(ny,nt) -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b0 ! - regression constants -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: b ! - regression coefficients -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: u ! - loadings standardized by singular values -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! Executable Statements
!
! Perform MLR
   ifail = 1
   Call do_mlr (iregr, ilink, nt, nx, x(:,1:nt), ny, y(:,1:nt), b0(:), b(:,:), &
        u=u(:,:), t=t)
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   ifail = 0
!
   Return
 End Subroutine full_mlr
!
!
!
 Subroutine cv_mlr (nt, ncv, nx, x, ny, y, ir, b0, b, ycv, &
            t)
!
! Performs cross-validated MLR
!
! Modules
   Use data_cpt_constants, Only: it_emp, it_gam, it_non
   Use data_io_constants,  Only: iout
   Use labels,             Only: l_goodness
   Use gui,                Only: print_rvalue
   Use progress_meter,     Only: lstop, &
                                 update_progress_meter
   Use settings,           Only: hcw, igauss, igood, ilink, iregr, istd, istdr, lzb
   Use arrays,             Only: tc, xc, ybk, yc, &
                                 get_cvsample
   Use iofiles,            Only: gifile
   Use distribs,           Only: gaussian, get_gamma, zero_bound
   Use regression,         Only: yopt
   Use scores,             Only: gm, &
                                 goodness
   Use climate,            Only: ave, sdev
   Use cpt_output,         Only: get_gi_fmt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: ir  ! - retroactive step -
   Integer, Intent(In) :: ncv ! - length of cross-validated training period -
   Integer, Intent(In) :: nx  ! - number of x spatial points -
   Integer, Intent(In) :: ny  ! - number of y spatial points -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b0 ! - regression constants -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: b   ! - regression coefficients -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: ycv ! - cross-validated hindcasts -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! Locals
!
! Local scalars
   Integer :: it ! - cross-validation time-step -
!
   Real(Kind=rp) :: gscale ! - goodness scaling -
!
   Character(Len=4) :: cfmt ! - format for goodness index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Cross-validate
   time_step: Do it = 1, nt
      Select Case (igauss)
       Case (it_non)
         If (.not.Present(t)) Then
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         Else
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v1=t(1:nt), v1cv=tc(1:nt), v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         End If
       Case (it_emp)
         If (.not.Present(t)) Then
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         Else
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v1=t(1:nt), v1cv=tc(1:nt), v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         End If
         Call gaussian (igauss, 1, ny, ncv, yc(1:ny,1:ncv))
       Case (it_gam)
         If (.not.Present(t)) Then
            Call get_cvsample (hcw, it, ybk(:,1:nt,1), yc(:,1:nt), &
                 v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         Else
            Call get_cvsample (hcw, it, ybk(:,1:nt,1), yc(:,1:nt), &
                 v1=t(1:nt), v1cv=tc(1:nt), v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         End If
         Call get_gamma (ny, ncv, 1, yc(:,1:ncv))
         Call gaussian (igauss, 1, ny, ncv, yc(1:ny,1:ncv))
      End Select
!
! Perform MLR
      If (.not.Present(t)) Then
         Call do_mlr (iregr, ilink, ncv, nx, xc(:,1:ncv,1), ny, yc(:,1:ncv), b0(:), b(1:nx,1:ny))
         If (update_progress_meter(.false.) /= 0) Return
         Call mlr_predict (ilink, 1, nx, x(1:nx,it:it), b0(:), b(1:nx,1:ny), yopt(1:ny,it:it,1)) ! - predict anomaly -
      Else
         Call do_mlr (iregr, ilink, ncv, nx, xc(:,1:ncv,1), ny, yc(:,1:ncv), b0(:), b(1:nx,1:ny), &
              t=tc(1:ncv))
         If (update_progress_meter(.false.) /= 0) Return
         Call mlr_predict (ilink, 1, nx, x(1:nx,it:it), b0(:), b(1:nx,1:ny), yopt(1:ny,it:it,1), & ! - predict anomaly -
              t=t(it:it))
      End If
      If (lzb) Call zero_bound (istdr, ny, 1, ave(:,1), sdev(:,1), yopt(1:ny,it:it,1))
   End Do time_step
   If (update_progress_meter(.false.) /= 0) Return
!
! Transform observations to calculate goodness
   If (igauss > 0) Then
      yc(1:ny,1:nt) = y(1:ny,1:nt)
      If (istd /= 3) Call gaussian (igauss, 1, ny, nt, yc(:,:)) ! - NB strictly, this step should be cross-validated (as below) -
   End If
!
! Calculate and print goodness metric
   Select Case (igauss)
    Case (it_non)
      gm(1,ir) = goodness(igood, iregr, nt, ny, nx, 1, 1, yopt(:,:,1), y(:,1:nt), &
                 t=t)
    Case Default
      gm(1,ir) = goodness(igood, iregr, nt, ny, nx, 1, 1, yopt(:,:,1), yc(:,:), &
                 t=t)
   End Select
   Call get_gi_fmt (igood, 18, gm(1,ir), gscale, cfmt)
   Write (Unit=*, Fmt='(1X,A,F'//cfmt//')') Trim(l_goodness%c)//': ', gm(1,ir)*gscale
   If (gifile%cfile%lset) Call print_rvalue (Trim(l_goodness%c), gm(1,ir), &
                               iout=iout)
   ycv(:,1:nt) = yopt(:,1:nt,1)
   If (lstop) Return
   If (update_progress_meter(.false.) /= 0) Return
!
   Return
 End Subroutine cv_mlr
!
!
!
 Subroutine do_mlr (iregr, ilink, nt, nx, x, ny, y, b0, b, &
            t, u)
!
! Performs MLR
!
! Modules
   Use data_cpt_constants, Only: ir_ols, ir_log, ir_bin, ir_poi, ir_gam
   Use regression,         Only: fit_glm_bernoulli, fit_glm_binomial, fit_glm_gamma, fit_glm_poisson, fit_regrm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilink ! - link function -
   Integer, Intent(In) :: iregr ! - regression option -
   Integer, Intent(In) :: nt    ! - number of cases -
   Integer, Intent(In) :: nx    ! - number of x spatial points -
   Integer, Intent(In) :: ny    ! - number of y spatial points -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables; shape(nx,nt) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables; shape(ny,nt) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: b0 ! - regression constants -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: b ! - regression coefficients -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: i     ! - location index -
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Compute regression coefficients
   Select Case (iregr)
    Case (ir_ols)
      Do i = 1, ny
         Call fit_regrm (nx, nt, x(1:nx,1:nt), y(i,1:nt), b0(i), b(1:nx,i), ifail, &
              u=u)
      End Do
    Case (ir_log)
      Do i = 1, ny
         Call fit_glm_bernoulli (nx, nt, x(1:nx,1:nt), y(i,1:nt), b0(i), b(1:nx,i), ifail, &
              u=u)
      End Do
    Case (ir_bin)
      Do i = 1, ny
         Call fit_glm_binomial (nx, nt, x(1:nx,1:nt), y(i,1:nt), t(1:nt), b0(i), b(1:nx,i), ifail, &
              u=u)
      End Do
    Case (ir_gam)
      Do i = 1, ny
         Call fit_glm_gamma (ilink, nx, nt, x(1:nx,1:nt), y(i,1:nt), b0(i), b(1:nx,i), ifail, &
              u=u)
      End Do
    Case (ir_poi)
      Do i = 1, ny
         Call fit_glm_poisson (ilink, nx, nt, x(1:nx,1:nt), y(i,1:nt), b0(i), b(1:nx,i), ifail, &
              u=u)
      End Do
   End Select
!
   Return
 End Subroutine do_mlr
!
!
!
 Subroutine mlr_predict (ilink, nf, nx, x, b0, b, fcast, &
            t, u, xvp)
!
! Calculates predictions given new predictor values
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: il_idt
   Use progress_meter,     Only: update_progress_meter
   Use regression,         Only: glm_finv_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilink ! - link function -
   Integer, Intent(In) :: nf    ! - number of forecasts -
   Integer, Intent(In) :: nx    ! - number of X variables -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: b0 ! - regression constants; dimension(*) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - new predictor values; dimension(nx,nf) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: b ! - regression coefficients; dimension(nx,*) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: fcast ! - forecast values; dimension(*,nf) -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: u ! - standardized loadings; dimension(nx+1,ne) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: xvp ! - standard error from predictors; dimension(nf) -
!
! Locals
!
! Local scalars
   Integer :: ie ! - EOF mode index -
   Integer :: k  ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Dot_Product, MatMul, Present
!
! Executable Statements
!
! Predict anomalies
   Do k = 1, nf
      fcast(:,k) = MatMul(x(:,k), b(:,:)) + b0(:)
!
! Calculate prediction-error variance
      If (Present(xvp)) Then
         xvp(k) = zero
         Do ie = 1, nx+1 ! - repeat for each principal component predictor -
            xvp(k) = xvp(k) + (u(1,ie) + Dot_Product(x(1:nx,k), u(2:nx+1,ie)))**2
         End Do
      End If
      If (update_progress_meter(.false.) /= 0) Return
   End Do
!
! Transform linear predictor for GLMs
   If (ilink /= il_idt) Then
      Call glm_finv_link (ilink, nf, fcast(:,:), &
           t=t)
   End If
!
   Return
 End Subroutine mlr_predict
End Module mlr
