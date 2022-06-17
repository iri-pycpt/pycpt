! General Circulation Model (GCM) validation
! Author: Simon Mason
Module gcm
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cv_gcm, full_gcm, gcm_predict, num_predictors
!
Contains
!
!
 Subroutine full_gcm (imc, nms, nt, ny, ig0, x, y, b0, b1, u, &
            t)
!
! Performs GCM using all data
!
! Modules
   Use progress_meter, Only: update_progress_meter
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imc ! - model combination identifier -
   Integer, Intent(In) :: nms ! - number of models -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: ny  ! - number of y spatial points -
   Integer, Intent(In) :: ig0 ! - lower index limit -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - response variables -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(InOut) :: x ! - explanatory variables -
!
! - output arrays -
   Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b0 ! - regression constants -
   Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b1 ! - regression coefficients -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: u ! - loadings standardized by singular values -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Executable Statements
!
! Calculate combination/calibration parameters
   Call do_gcm (imc, nms, nt, ny, ig0, x(:,:,:), y(:,:), b0(ig0:,:), b1(ig0:,:), &
        u=u(:,:,:), t=t)
   If (update_progress_meter(.false.) /= 0) Return
!
   Return
 End Subroutine full_gcm
!
!
!
 Subroutine cv_gcm (imc, nms, nt, ncv, nv, ig0, x, y, ir, b0, b1, ycv, &
            t)
!
! Performs cross-validated GCM
!
! Modules
   Use data_cpt_constants, Only: it_emp, it_gam
   Use data_io_constants,  Only: iout
   Use labels,             Only: l_goodness
   Use gui,                Only: print_rvalue
   Use progress_meter,     Only: lstop, &
                                 update_progress_meter
   Use settings,           Only: hcw, igauss, igood, iregr, istdr, lzb
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
   Integer, Intent(In) :: imc ! - model combination identifier -
   Integer, Intent(In) :: ir  ! - retroactive step -
   Integer, Intent(In) :: nms ! - number of models -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: ncv ! - length of cross-validated training period -
   Integer, Intent(In) :: nv  ! - number of y spatial points -
   Integer, Intent(In) :: ig0 ! - lower index limit -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - explanatory variables -
!
! - output arrays -
   Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b0  ! - regression constants -
   Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b1  ! - regression coefficients -
   Real(Kind=rp), Dimension(   :,:), Intent(Out) :: ycv ! - cross-validated hindcasts -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
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
       Case Default
         If (.not.Present(t)) Then
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v3=x(:,1:nt,:), v3cv=xc(:,1:nt,:))
         Else
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v1=t(1:nt), v1cv=tc(1:nt), v3=x(:,1:nt,:), v3cv=xc(:,1:nt,:))
         End If
       Case (it_emp)
         If (.not.Present(t)) Then
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v3=x(:,1:nt,:), v3cv=xc(:,1:nt,:))
         Else
            Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
                 v1=t(1:nt), v1cv=tc(1:nt), v3=x(:,1:nt,:), v3cv=xc(:,1:nt,:))
         End If
         Call gaussian (igauss, 1, nv, ncv, yc(1:nv,1:ncv))
       Case (it_gam)
         If (.not.Present(t)) Then
            Call get_cvsample (hcw, it, ybk(:,1:nt,1), yc(:,1:nt), &
                 v3=x(:,1:nt,:), v3cv=xc(:,1:nt,:))
         Else
            Call get_cvsample (hcw, it, ybk(:,1:nt,1), yc(:,1:nt), &
                 v1=t(1:nt), v1cv=tc(1:nt), v3=x(:,1:nt,:), v3cv=xc(:,1:nt,:))
         End If
         Call get_gamma (nv, ncv, 1, yc(:,1:ncv))
         Call gaussian (igauss, 1, nv, ncv, yc(1:nv,1:ncv))
      End Select
!
! Transform
      If (igauss > 0) Call gaussian (igauss, 1, nv, ncv, yc(1:nv,1:ncv))
      If (update_progress_meter(.false.) /= 0) Return
!
! Calculate combination/calibration parameters
      If (.not.Present(t)) Then
         Call do_gcm (imc, nms, ncv, nv, ig0, xc(:,1:ncv,:), yc(:,1:ncv), b0(ig0:,:), b1(ig0:,:))
         Call gcm_predict (nms, 1, nv, ig0, imc, x(:,it:it,:), b0(ig0:,:), b1(ig0:,:), yopt(:,it:it,1))
      Else
         Call do_gcm (imc, nms, ncv, nv, ig0, xc(:,1:ncv,:), yc(:,1:ncv), b0(ig0:,:), b1(ig0:,:), &
              t=tc(1:ncv))
         Call gcm_predict (nms, 1, nv, ig0, imc, x(:,it:it,:), b0(ig0:,:), b1(ig0:,:), yopt(:,it:it,1), &
              t=t(it:it))
      End If
      If (lzb) Call zero_bound (istdr, nv, 1, ave(:,1), sdev(:,1), yopt(:,it:it,1))
      If (update_progress_meter(.false.) /= 0) Return
   End Do time_step
!
! Calculate and print goodness metric
   gm(1,ir) = goodness(igood, iregr, nt, nv, nms, 1, 1, yopt(:,:,1), y(:,1:nt), &
              t=t)
   Call get_gi_fmt (igood, 18, gm(1,ir), gscale, cfmt)
   Write (Unit=*, Fmt='(1X,A,F'//cfmt//')') Trim(l_goodness%c)//': ', gm(1,ir)*gscale
   If (gifile%cfile%lset) Call print_rvalue (Trim(l_goodness%c), gm(1,ir), &
                               iout=iout)
   ycv(:,1:nt) = yopt(:,1:nt,1)
   If (lstop) Return
   If (update_progress_meter(.false.) /= 0) Return
!
   Return
 End Subroutine cv_gcm
!
!
!
 Subroutine do_gcm (imc, nms, nt, ny, ig0, x, yc, b0, b1, &
            t, u)
!
! Performs GCM
!
! Modules
   Use data_numbers,       Only: zero, one, rhuge
   Use data_cpt_constants, Only: ic_non, ic_ave, ic_var, ic_skl, ic_mul, ic_loc, ic_bst, ic_pcr, &
                                 ig_non, ig_ave, ig_var, ig_skl, il_idt, ir_ols
   Use settings,           Only: dgood, igcms, igood, ilink, iregr
   Use arrays,             Only: xwk=>rwk2, xm, xsd, ym, ysd
   Use distribs,           Only: moments
   Use regression,         Only: fit_regrm, varadjust_b
   Use scores,             Only: goodness
   Use mlr,                Only: do_mlr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imc ! - model combination identifier -
   Integer, Intent(In) :: nms ! - number of models -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: ny  ! - number of y spatial points -
   Integer, Intent(In) :: ig0 ! - lower index limit -
!
! - onput/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: yc ! - response variables -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(InOut) :: x ! - explanatory variables -
!
! - output arrays -
   Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b0 ! - regression constants -
   Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b1 ! - regression coefficients -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: i      ! - location index -
   Integer :: ibest  ! - best model -
   Integer :: ilinku ! - used link function -
   Integer :: imdl   ! - current model -
   Integer :: iregru ! - used regression model -
   Integer :: ifail  ! - error indicator -
!
   Real(Kind=rp) :: gm    ! - goodness metric -
   Real(Kind=rp) :: gbest ! - best score -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real
!
! Executable Statements
!
! Calculate climatology
   Call moments (ny, nt, yc(:,:), ym(:), &
        vsd=ysd(:))
!
! Recalibrate individual models
   If (Present(u)) u(:,:,:) = zero
   Do imdl = 1, nms
! - calculate model climatologies -
      Call moments (ny, nt, x(:,1:nt,imdl), xm(:), &
           vsd=xsd(:))
! - no correction -
      Select Case (igcms)
       Case (ig_non)
         b0(imdl,:) = zero
         b1(imdl,:) = one
! - correct mean biases -
       Case (ig_ave)
         b1(imdl,:) = one
         b0(imdl,:) = ym(:) - xm(:)
! - correct mean and variance biases -
       Case (ig_var)
         If (nms == 1 .and. Present(u)) Then ! - calculate dummy parameters to estimate error variance -
            Do i = 1, ny
               Call fit_regrm (1, nt, x(i:i,1:nt,imdl), yc(i,1:nt), b0(imdl,i), b1(imdl:imdl,i), ifail, &
                    u=u(i,1:2,1:2))
            End Do
         End If
         Do i = 1, ny
            Call varadjust_b (xm(i), ym(i), xsd(i), ysd(i), b0(imdl,i), b1(imdl,i))
         End Do
! - correct for skill -
       Case (ig_skl)
         If (nms == 1 .and. Present(u)) Then
            Do i = 1, ny
               Call do_mlr (iregr, ilink, nt, 1, x(i:i,1:nt,imdl), 1, yc(i:i,1:nt), b0(imdl,i:i), b1(imdl:imdl,i:i), &
                    u=u(i,1:2,1:2), t=t)
            End Do
         Else
            If (nms == 1) Then
               iregru = iregr
               ilinku = ilink
            Else
               iregru = ir_ols
               ilinku = il_idt
            End If
            Do i = 1, ny
               Call do_mlr (iregru, ilinku, nt, 1, x(i:i,1:nt,imdl), 1, yc(i:i,1:nt), b0(imdl,i:i), b1(imdl:imdl,i:i), &
                    t=t)
            End Do
         End If
      End Select
   End Do
!
! Calculate error variance for single model
   If (nms == 1) Then
      If (.not.Present(u)) Return
      Select Case (igcms)
       Case (ig_non, ig_ave)
         Call get_u1 (igcms, &
              t=t)
      End Select
!
! Combine models
   Else
      Select Case (igcms)
       Case (ig_var)
         iregru = ir_ols
         ilinku = il_idt
       Case (ig_skl)
         iregru = iregr
         ilinku = ilink
      End Select
      Select Case (imc)
! - uncalibrated average -
       Case (ic_non)
         b0(0,:) = zero
         b1(0,:) = one/Real(nms, Kind=rp)
         If (Present(u)) Then
            Select Case (igcms)
             Case (ig_non)
               u(:,1,1) = zero
             Case (ig_ave)
               u(:,1,1) = one/Real(nt, Kind=rp)
             Case (ig_var, ig_skl) ! - treat error variance like multiple regression -
               Call get_mme (.false., &
                    t=t, u=u(1:ny,1:nms+1,1:nms+1))
            End Select
         End If
! - mean-corrected average -
       Case (ic_ave)
         Call simple_combine (ny, nms, b0(1:,:), b1(1:,:), x(:,1:nt,:), xwk(1:ny,1:nt))
         Call moments (ny, nt, xwk(1:ny,1:nt), xm(:))
         b0(0,:) = ym(:) - xm(:)
         b1(0,:) = one/Real(nms, Kind=rp)
         If (Present(u)) Then
            Select Case (igcms)
             Case (ig_non, ig_ave)
               u(:,1,1) = one/Real(nt, Kind=rp)
             Case (ig_var, ig_skl) ! - treat error variance like multiple regression -
               Call get_mme (.false., &
                    t=t, u=u(1:ny,1:nms+1,1:nms+1))
            End Select
         End If
! - mean- and variance-corrected average -
       Case (ic_var)
         Call simple_combine (ny, nms, b0(1:,:), b1(1:,:), x(:,1:nt,:), xwk(1:ny,1:nt))
         Call moments (ny, nt, xwk(1:ny,1:nt), xm(:), &
              vsd=xsd(:))
         Do i = 1, ny
            Call varadjust_b (xm(i), ym(i), xsd(i), ysd(i), b0(0,i), b1(0,i))
         End Do
         b1(0,:) = b1(0,:)/Real(nms, Kind=rp)
         If (Present(u)) Then
            Select Case (igcms)
             Case (ig_non, ig_ave) ! - calculate u as if for a single model -
               Call get_u1 (ig_var, &
                    t=t)
             Case (ig_var, ig_skl) ! - treat error variance like multiple regression -
               Call get_mme (.false., &
                    t=t, u=u(1:ny,1:nms+1,1:nms+1))
            End Select
         End If
! - skill-corrected average -
       Case (ic_skl)
         Call simple_combine (ny, nms, b0(1:,:), b1(1:,:), x(:,1:nt,:), xwk(1:ny,1:nt))
         Call moments (ny, nt, xwk(1:ny,1:nt), xm(:), &
              vsd=xsd(:))
         Select Case (igcms)
          Case (ig_non, ig_ave)
            If (.not.Present(u)) Then
               Do i = 1, ny
                  Call do_mlr (iregru, ilinku, nt, 1, xwk(i:i,1:nt), 1, yc(i:i,:), b0(0,i:i), b1(0:0,i:i), &
                       t=t)
               End Do
            Else
               Do i = 1, ny
                  Call do_mlr (iregru, ilinku, nt, 1, xwk(i:i,1:nt), 1, yc(i:i,:), b0(0,i:i), b1(0:0,i:i), &
                       t=t, u=u(i,1:2,1:2))
               End Do
            End If
            b1(0,:) = b1(0,:)/Real(nms, Kind=rp)
          Case (ig_var, ig_skl) ! - treat error variance like multiple regression -
            Do i = 1, ny
               Call do_mlr (iregr, ilink, nt, 1, xwk(i:i,1:nt), 1, yc(i:i,:), b0(0,i:i), b1(0:0,i:i), &
                    t=t)
            End Do
            b1(0,:) = b1(0,:)/Real(nms, Kind=rp)
            If (Present(u)) Then
               Call get_mme (.false., &
                    t=t, u=u(1:ny,1:nms+1,1:nms+1))
            End If
         End Select
! - skill-weighted average -
       Case (ic_mul)
         If (.not.Present(u)) Then
            Call get_mme (.true., &
                 t=t)
         Else
            Call get_mme (.true., &
                 t=t, u=u(1:ny,1:nms+1,1:nms+1))
         End If
! - best model by location -
       Case (ic_loc)
         Do i = 1, ny ! - identify best model at each location -
            gbest = -rhuge
            ibest = 0
            Do imdl = 1, nms
               gm = goodness(igood, iregr, nt, 1, 1, 1, 1, x(i:i,1:nt,imdl), yc(i:i,1:nt), &
                    t=t)
               If (gm*dgood > gbest) Then
                  gbest = gm
                  ibest = imdl
               End If
            End Do
            Do imdl = 1, nms
               If (imdl == ibest) Cycle
               b0(imdl,i) = zero
               b1(imdl,i) = zero
            End Do
         End Do
         b0(0,:) = zero ! - set combination parameters to neutral -
         b1(0,:) = one
         If (Present(u)) Call get_u1 (igcms, & ! - calculate u as if for a single model -
                              t=t)
! - best overall model -
       Case (ic_bst)
         gbest = -rhuge
         ibest = 0
         Do imdl = 1, nms ! - identify best model -
            gm = goodness(igood, iregr, nt, ny, 1, 1, 1, x(:,1:nt,imdl), yc(:,1:nt), &
                 t=t)
            If (gm*dgood > gbest) Then
               gbest = gm
               ibest = imdl
            End If
         End Do
         Do imdl = 1, nms
            If (imdl == ibest) Cycle
            b0(imdl,:) = zero
            b1(imdl,:) = zero
         End Do
         b0(0,:) = zero ! - set combination parameters to neutral -
         b1(0,:) = one
         If (Present(u)) Call get_u1 (igcms, & ! - calculate u as if for a single model -
                              t=t)
! - PCR combination -
!       Case (ic_pcr)
!         If (nms == 1 .and. Present(u)) Then ! - calculate dummy parameters to estimate error variance -
!         Else
!            Call pcr_combn (.true., &
!                 t=t)
!         End If
      End Select
   End If
!
   Return
!
 Contains
!
!
  Subroutine get_u1 (ig, &
             t)
!
! Sets U for a single model
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ig ! - recalibration identifier -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
!
! Local arrays
   Real(Kind=rp), Dimension(1) :: b0loc ! - local b0 -
!
   Real(Kind=rp), Dimension(1,1) :: b1loc ! - local b -
!
! Executable Statements
!
! Set U for a single model
   Select Case (ig)
    Case (ig_non) ! - no correction -
      u(:,1,1) = zero
    Case (ig_ave) ! - correct mean biases -
      u(:,1,1) = one/Real(nt, Kind=rp) ! - variance of mean -
    Case (ig_var, ig_skl) ! - correct mean and variance biases, correct for skill -
      Call get_combined (ny, nt, b0(:,:), b1(:,:), x(:,:,:), xwk(:,:))
      Do i = 1, ny
         Call do_mlr (ir_ols, il_idt, nt, 1, xwk(i:i,:), 1, yc(i:i,:), b0loc(1:1), b1loc(1:1,1:1), &
              t=t, u=u(i,1:2,1:2)) ! - calculate U as if for a singe ensemble -
      End Do
   End Select
!
   Return
  End Subroutine get_u1
!
!
!
  Subroutine get_mme (lbs, &
             t, u)
!
! Performs a multi-model combination
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical :: lbs ! - return regression coefficients? -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out), Optional :: u ! - loadings standardized by singular values -
!
! Locals
!
! Local scalars
   Integer :: i    ! - location index -
   Integer :: imdl ! - model index -
!
! Local arrays
   Real(Kind=rp), Dimension(1) :: b0loc ! - local b0 -
!
   Real(Kind=rp), Dimension(nms,1) :: b1loc ! - local b -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Set U for multiple models
! - recalibrate individual models -
   Do i = 1, ny
      Do imdl = 1, nms
         xwk(imdl,1:nt) = b0(imdl,i) + b1(imdl,i)*x(i,1:nt,imdl)
      End Do
! - fit regression model using recalibrated data -
      If (.not.Present(u)) Then
         Call do_mlr (iregru, ilinku, nt, nms, xwk(1:nms,1:nt), 1, yc(i:i,1:nt), b0loc(1:1), b1loc(1:nms,1:1), &
              t=t)
      Else
         Call do_mlr (iregru, ilinku, nt, nms, xwk(1:nms,1:nt), 1, yc(i:i,1:nt), b0loc(1:1), b1loc(1:nms,1:1), &
              t=t, u=u(i,:,:))
      End If
!
! Calculate adjusted coefficients
      If (lbs) Then
         b0(0,i) = b0loc(1)
         b1(0,i) = one
         Do imdl = 1, nms
            b0(imdl,i) = b0(imdl,i)*b1loc(imdl,1)
            b1(imdl,i) = b1(imdl,i)*b1loc(imdl,1)
         End Do
      End If
   End Do
!
   Return
  End Subroutine get_mme
!
!
!
!  Subroutine pcr_combn (lbs, &
!             t)
!
! Performs a PCR combination
!
! Arguments
!
! Dummy arguments
! - input scalars -
!   Logical :: lbs ! - return regression coefficients? -
!
! Optional arguments
! - optional input arrays -
!   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! Locals
!
! Local scalars
!   Integer :: i    ! - location index -
!   Integer :: imdl ! - model index -
!
! Local arrays
!   Real(Kind=rp), Dimension(1) :: b0loc ! - local b0 -
!
!   Real(Kind=rp), Dimension(nms,1) :: b1loc ! - local b -
!
! Functions and Subroutines
!
! Intrinsic functions
!   Intrinsic Present
!
! Executable Statements
!
! Set U for multiple models
! - recalibrate individual models -
!   Do i = 1, ny
!      Do imdl = 1, nms
!         xwk(imdl,1:nt) = b0(imdl,i) + b1(imdl,i)*x(i,1:nt,imdl)
!      End Do
! - fit PCR model using recalibrated data -
!      Call do_pcr (nt, nms, xwk(1:nms,1:nt), 1, yc(i:i,1:nt), ieofx, mxe, svx(:), eofx(:,:), tsx(:,:), bz(0:,:), .true., &
!           mxe, npx, xm(:), xsd(:), ym(:), ifail, &
!           t=t)
!
! Calculate adjusted coefficients
!      If (lbs) Then
!         b0(0,i) = b0loc(1)
!         b1(0,i) = one
!         Do imdl = 1, nms
!            b0(imdl,i) = b0(imdl,i)*b1loc(imdl,1)
!            b1(imdl,i) = b1(imdl,i)*b1loc(imdl,1)
!         End Do
!      End If
!   End Do
!
!   Return
!  End Subroutine pcr_combn
 End Subroutine do_gcm
!
!
!
 Subroutine gcm_predict (nms, nf, ny, ig0, imc, x, b0, b, fcast, &
            t, u, xvp)
!
! Calculates predictions given new predictor values
!
! Modules
   Use data_cpt_constants, Only: ic_non, ic_ave, ic_var, ic_skl, ic_mul, ic_loc, ic_bst, ig_non, ig_ave, ig_var, ig_skl, il_idt
   Use progress_meter,     Only: update_progress_meter
   Use settings,           Only: igcms, ilink
   Use regression,         Only: glm_finv_link
   Use mlr,                Only: mlr_predict
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nms ! - number of models -
   Integer, Intent(In) :: nf  ! - number of forecasts -
   Integer, Intent(In) :: ny  ! - number of Y variables -
   Integer, Intent(In) :: ig0 ! - lower index limit -
   Integer, Intent(In) :: imc ! - model combination identifier -
!
! - input arrays -
   Real(Kind=rp), Dimension(ig0:,:), Intent(In) :: b0 ! - regression constants -
   Real(Kind=rp), Dimension(ig0:,:), Intent(In) :: b  ! - regression coefficients -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - new predictor values -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: fcast ! - forecast values -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In), Optional :: u ! - standardized loadings -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: xvp ! - predictors over forecast period -
!
! Locals
!
! Local scalars
   Integer :: i      ! - variable index -
   Integer :: ilinku ! - used link function -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Transpose
!
! Executable Statements
!
! Predict anomalies for single models
   Select Case (igcms)
    Case Default
      ilinku = il_idt
    Case (ig_skl)
      ilinku = ilink
   End Select
   If (nms == 1) Then
      If (.not.Present(xvp)) Then ! - no error variance -
         Do i = 1, ny ! - repeat for each location because predictors are location-specific -
            Call mlr_predict (ilinku, nf, 1, Transpose(x(i,:,1:1)), b0(1,i:i), b(1:1,i:i), fcast(i:i,:), &
                 t=t)
         End Do
      Else ! - calculate error variance -
         Select Case (igcms)
          Case Default ! - if not correcting for skill, error variance must be specified -
            Do i = 1, ny ! - repeat for each location because predictors are location-specific -
               Call mlr_predict (ilinku, nf, 1, Transpose(x(i,:,1:1)), b0(1,i:i), b(1:1,i:i), fcast(i:i,:), &
                    t=t)
               Call copy_u ()
            End Do
          Case (ig_skl)  ! - if correcting for skill, error variance is from standard regression theory -
            Do i = 1, ny ! - repeat for each location because predictors are location-specific -
               Call mlr_predict (ilinku, nf, 1, Transpose(x(i,:,1:1)), b0(1,i:i), b(1:1,i:i), fcast(i:i,:), &
                    t=t, u=u(i,:,:), xvp=xvp(i,:))
            End Do
         End Select
      End If
!
! Predict anomalies for multiple models
   Else
      Call get_combined (ny, nf, b0(:,:), b(:,:), x(:,:,:), fcast(:,:))
      If (Present(xvp)) Then
         Select Case (imc)
! - simple average -
          Case (ic_non)
            Select Case (igcms)
             Case (ig_non, ig_ave) ! - no correction, mean-bias correction -
               Call copy_u ()
             Case (ig_var, ig_skl) ! - mean- and variance-bias correction, skill correction -
               Call u_to_xvp (ny, nf, nms, x(:,:,:))
            End Select
! - mean-adjusted average -
          Case (ic_ave)
            Select Case (igcms)
             Case (ig_non, ig_ave) ! - no correction, mean-bias correction -
               Call copy_u ()
             Case (ig_var, ig_skl) ! - mean- and variance-bias correction, skill correction -
               Call u_to_xvp (ny, nf, nms, x(:,:,:))
            End Select
! - mean- and variance-adjusted average -
          Case (ic_var)
            Select Case (igcms)
             Case (ig_non, ig_ave) ! - no correction, mean-bias correction -
               Call u_to_xvp_fcst (ny, nf, fcast(:,:))
             Case (ig_var, ig_skl) ! - mean- and variance-bias correction, skill correction -
               Call u_to_xvp (ny, nf, nms, x(:,:,:))
            End Select
! - skill-adjusted average -
          Case (ic_skl)
            Select Case (igcms)
             Case (ig_non, ig_ave) ! - no correction, mean-bias correction -
               Call u_to_xvp_fcst (ny, nf, fcast(:,:))
             Case (ig_var, ig_skl) ! - mean-bias correction -
               Call u_to_xvp (ny, nf, nms, x(:,:,:))
            End Select
! - multi-model combination -
          Case (ic_mul)
            Call u_to_xvp (ny, nf, nms, x(:,:,:))
! - best models -
          Case (ic_loc, ic_bst)
            Select Case (igcms)
             Case (ig_non, ig_ave) ! - no correction, mean-bias correction -
               Call copy_u ()
             Case (ic_var, ic_skl) ! - mean- and variance-bias correction, skill correction -
               Call u_to_xvp_fcst (ny, nf, fcast(:,:))
            End Select
         End Select
      End If
! - transform linear predictor for GLMs -
      If (ilinku /= il_idt) Then
         Call glm_finv_link (ilink, nf, fcast(:,:), &
                 t=t)
      End If
   End If
   If (update_progress_meter(.false.) /= 0) Return
!
   Return
!
 Contains
!
!
  Subroutine copy_u ()
!
! Copies u into xvp
!
! Locals
!
! Local scalars
   Integer :: k ! - forecast index -
!
! Executable Statements
!
! Copy u into xvp
   Do k = 1, nf
      xvp(:,k) = u(:,1,1)
   End Do
!
   Return
  End Subroutine copy_u
!
!
!
  Subroutine u_to_xvp (n1, n2, m, x)
!
! Calculates xvp from u
!
! Modules
   Use data_numbers, Only: zero
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - number of locations -
   Integer, Intent(In) :: n2 ! - number of forecasts -
   Integer, Intent(In) :: m  ! - number of predictors -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - new predictor values; dimension(n1,n2,m) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - location index -
   Integer :: ie ! - principal component index -
   Integer :: k  ! - forecast index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Dot_Product
!
! Executable Statements
!
! Calculate xvp from u
   Do i = 1, n1
      Do k = 1, n2
         xvp(i,k) = zero
         Do ie = 1, m+1 ! - repeat for each principal component predictor -
            xvp(i,k) = xvp(i,k) + (u(i,1,ie) + Dot_Product(x(i,k,1:m), u(i,2:m+1,ie)))**2
         End Do
      End Do
   End Do
!
   Return
  End Subroutine u_to_xvp
!
!
!
  Subroutine u_to_xvp_fcst (n1, n2, f)
!
! Calculates xvp from u using combined data
!
! Modules
   Use data_numbers, Only: zero
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - number of locations -
   Integer, Intent(In) :: n2 ! - number of forecasts -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: f ! - combined predictor values; dimension(n1,n2) -
!
! Locals
!
! Local scalars
   Integer :: i  ! - location index -
   Integer :: ie ! - principal component index -
   Integer :: k  ! - forecast index -
!
! Executable Statements
!
! Calculate xvp from u using f
   Do i = 1, n1
      Do k = 1, n2
         xvp(i,k) = zero
         Do ie = 1, 2 ! - repeat for each principal component predictor -
            xvp(i,k) = xvp(i,k) + (u(i,1,ie) + f(i,k)*u(i,2,ie))**2
         End Do
      End Do
   End Do
!
   Return
  End Subroutine u_to_xvp_fcst
 End Subroutine gcm_predict
!
!
!
 Subroutine simple_combine (n1, m, b0, b1, x, c)
!
! Calculates average of recalibrated models
!
! Modules
   Use data_numbers, Only: rp, zero
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - number of locations -
   Integer, Intent(In) :: m  ! - number of models -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: b0 ! - regression constants; dimension(m,n2) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: b1 ! - regression coefficients; dimension(m,n2) -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - model predictions -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: c ! - combined predictions -
!
! Locals
!
! Local scalars
   Integer :: i    ! - location index -
   Integer :: imdl ! - model index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate average of recalibrated models
   Do i = 1, n1
      c(i,:) = zero
      Do imdl = 1, m
         c(i,:) = c(i,:) + b0(imdl,i) + b1(imdl,i)*x(i,:,imdl)
      End Do
      c(i,:) = c(i,:)/Real(m, Kind=rp)
   End Do
!
   Return
 End Subroutine simple_combine
!
!
!
 Subroutine get_combined (n1, n2, b0, b, x, c)
!
! Combines model predictions
!
! Modules
   Use data_numbers, Only: rp
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1 ! - number of locations -
   Integer, Intent(In) :: n2 ! - number of forecasts -
!
! - input arrays -
   Real(Kind=rp), Dimension(0:,:), Intent(In) :: b0 ! - regression constants -
   Real(Kind=rp), Dimension(0:,:), Intent(In) :: b  ! - regression coefficients -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - model predictions -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: c ! - combined predictions -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
   Integer :: k ! - forecast index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Dot_Product, Sum
!
! Executable Statements
!
! Recalibrate individual models
   Do i = 1, n1
      Do k = 1, n2
         c(i,k) = Sum(b0(1:,i)) + Dot_Product(b(1:,i), x(i,k,:))
      End Do
      c(i,:) = b0(0,i) + b(0,i)*c(i,:)
   End Do
!
   Return
 End Subroutine get_combined
!
!
!
 Function num_predictors(nms) &
          Result (num)
!
! Determines effective number of predictors
!
! Modules
   Use data_cpt_constants, Only: ic_non, ic_ave, ic_var, ic_skl, ic_mul, ic_loc, ic_bst, ig_non, ig_ave
   Use settings,           Only: igcms, imc
!
! Function result
  Integer :: num
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nms ! - number of models -
!
! Executable Statements
!
! Determine effective number of predictors
   Select Case (imc)
    Case (ic_non) ! - simple average combination -
      Select Case (igcms)
       Case (ig_non)
         num = 0
       Case (ig_ave)
         num = 1
       Case Default
         num = nms
      End Select
    Case (ic_ave, ic_var, ic_skl) ! - average-, variance- and skill-corrected combination -
      Select Case (igcms)
       Case (ig_non, ig_ave)
         num = 1
       Case Default
         num = nms
      End Select
    Case (ic_mul) ! - multi-model combination -
      num = nms
    Case (ic_bst, ic_loc) ! - best models -
      Select Case (igcms)
       Case (ig_non)
         num = 0
       Case Default
         num = 1
      End Select
   End Select
!
   Return
 End Function num_predictors
End Module gcm
