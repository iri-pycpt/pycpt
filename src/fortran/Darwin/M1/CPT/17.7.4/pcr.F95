! Principal Components Regression (PCR)
! Author: Simon Mason
Module pcr
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cv_pcr, full_pcr, pcr_predict
!
Contains
!
!
 Subroutine full_pcr (nt, nx, x, ny, y, ieofx, nxe, svx, eofx, tsx, bz, npx, ifail, &
            t, b0, b)
!
! Performs PCR using all data
!
! Modules
   Use progress_meter, Only: update_progress_meter
   Use arrays,         Only: xm, xsd, ym
   Use pcs,            Only: lxt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt    ! - number of cases -
   Integer, Intent(In) :: nx    ! - number of x spatial points -
   Integer, Intent(In) :: ny    ! - number of y spatial points -
   Integer, Intent(In) :: nxe   ! - number of X EOF modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: x ! - explanatory variables; shape(nx,nt) -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - response variables; shape(ny,nt) -
!
! - output scalars -
   Integer, Intent(Out) :: npx   ! - number of positive eigenvalues -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: svx ! - singular values of x -
!
   Real(Kind=rp), Dimension( :,:), Intent(Out) :: eofx ! - x EOF patterns -
   Real(Kind=rp), Dimension( :,:), Intent(Out) :: tsx  ! - time-series of x EOFs (transposed) -
   Real(Kind=rp), Dimension(0:,:), Intent(Out) :: bz   ! - principal component regression coefficients -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: b0 ! - regression constants -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: b ! - regression coefficients -
!
! Locals
!
! Local scalars
   Integer :: ie ! - EOF mode index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Perform PCR
   ifail = 1
   Call do_pcr (nt, nx, x(:,1:nt), ny, y(:,1:nt), ieofx, nxe, svx(:), eofx(:,:), tsx(:,:), bz(0:,:), .true., lxt, npx, &
        xm(:), xsd(:), ym(:), ifail, &
        t=t)
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Compute regression coefficients
   If (Present(b0) .and. Present(b)) Then
      Call bz_to_b (eofx(1:nx,1:nxe), xm(1:nx), ym(1:ny), bz(0:nxe,1:ny), b(1:nx,1:ny), nxe,&
           b0=b0(1:ny))
   End If
! - rescale EOF time series -
   Do ie = 1, npx
      tsx(ie,1:nt) = tsx(ie,1:nt)*svx(ie)**2
   End Do
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   ifail = 0
!
   Return
 End Subroutine full_pcr
!
!
!
 Subroutine cv_pcr (nt, ncv, nx, x, ny, y, ieofx, nxe, mxe, ir, lopt, ycv, &
            t, nxo)
!
! Performs cross-validated PCR
!
! Modules
   Use data_cpt_constants, Only: it_emp, it_gam, it_non
   Use data_io_constants,  Only: iout
   Use labels,             Only: l_goodness
   Use gui,                Only: print_rvalue
   Use progress_meter,     Only: lstop, &
                                 update_progress_meter
   Use settings,           Only: dgood, hcw, igauss, igood, iregr, istd, istdr, lexpert, lzb
   Use arrays,             Only: tc, xc, ybk, yc, xm, xsd, ym, &
                                 get_cvsample
   Use iofiles,            Only: gifile
   Use distribs,           Only: gaussian, get_gamma, zero_bound
   Use pcs,                Only: bz, eofx, npx, svx, tsx
   Use regression,         Only: yopt
   Use scores,             Only: gm, &
                                 goodness
   Use climate,            Only: ave, sdev
   Use cpt_output,         Only: get_gi_fmt, output_value
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt    ! - number of cases -
   Integer, Intent(In) :: ir    ! - retroactive step -
   Integer, Intent(In) :: ncv   ! - length of cross-validated training period -
   Integer, Intent(In) :: nx    ! - number of x spatial points -
   Integer, Intent(In) :: ny    ! - number of y spatial points -
   Integer, Intent(In) :: nxe   ! - minimum number of X EOF modes -
   Integer, Intent(In) :: mxe   ! - maximum number of X EOF modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: ycv ! - cross-validated hindcasts -
!
   Logical, Dimension(:), Intent(Out) :: lopt ! - optimised? -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! - optional output scalars -
   Integer, Intent(Out), Optional :: nxo ! - optimal number of X EOF modes -
!
! Locals
!
! Local scalars
   Integer :: it    ! - cross-validation time-step -
   Integer :: ixe   ! - X EOF mode index -
   Integer :: ixu   ! - used X EOF mode index -
   Integer :: iopt  ! - optimization step index -
   Integer :: jopt  ! - index of optimized settings -
   Integer :: ifail ! - error indicator -
!
   Real(Kind=rp) :: gscale ! - goodness scaling -
!
   Character(Len=4) :: cfmt ! - format for goodness index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Present, Trim
!
! Executable Statements
!
! Set optimization checks
   lopt(:) = .true.
!
! Cross-validate
   time_step: Do it = 1, nt
      Select Case (igauss)
       Case Default
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
! Transform
      If (igauss > 0) Call gaussian (igauss, 1, ny, ncv, yc(1:ny,1:ncv))
!
! Perform PCR
      If (.not.Present(t)) Then
         Call do_pcr (ncv, nx, xc(:,1:ncv,1), ny, yc(:,1:ncv), ieofx, mxe, svx(:), eofx(:,:), tsx(:,:), bz(0:,:), .false., &
              mxe, npx, xm(:), xsd(:), ym(:), ifail)
      Else
         Call do_pcr (ncv, nx, xc(:,1:ncv,1), ny, yc(:,1:ncv), ieofx, mxe, svx(:), eofx(:,:), tsx(:,:), bz(0:,:), .false., &
              mxe, npx, xm(:), xsd(:), ym(:), ifail, &
              t=tc(1:ncv))
      End If
! - update progress meter -
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Calculate regression coefficients
      If (ifail == 0) Then
         iopt = 0
         Do ixe = nxe, mxe
            iopt = iopt + 1
            ixu = Min(ixe, npx)
!
! Predict anomaly
            If (.not.Present(t)) Then
               Call pcr_predict (1, nx, ixu, ieofx, x(1:nx,it:it), svx(:), eofx(:,:), bz(0:ixu,1:ny), yopt(1:ny,it:it,iopt))
            Else
               Call pcr_predict (1, nx, ixu, ieofx, x(1:nx,it:it), svx(:), eofx(:,:), bz(0:ixu,1:ny), yopt(1:ny,it:it,iopt), &
                    t=t(it:it))
            End If
            If (lzb) Call zero_bound (istdr, ny, 1, ave(:,1), sdev(:,1), yopt(1:ny,it:it,iopt))
         End Do
!
! Supply mean if PCR failed
      Else
         iopt = 0
         Do ixe = nxe, mxe
            iopt = iopt + 1
            yopt(1:ny,it,iopt) = ym(1:ny)
            If (update_progress_meter(.false., ifail=ifail) /= 0) Return
         End Do
      End If
   End Do time_step
!
! Transform observations to calculate goodness
   If (igauss > 0) Then
      yc(1:ny,1:nt) = y(1:ny,1:nt)
      If (istd /= 3) Call gaussian (igauss, 1, ny, nt, yc(:,:)) ! - NB strictly, this step should be cross-validated (as below) -
   End If
!
! Calculate and print goodness metric
   If (Present(nxo)) Then
      If (lexpert) Then
         Print *, '               CURRENT                         OPTIMUM'
         Print *, ' '
         Print *, '      Number of Modes    Goodness      Number of Modes    Goodness'
         Print *, '                            Index                            Index'
         Print *, ' '
      End If
      If (gifile%cfile%lset) Then
         ifail = output_value(iout, &
                 c='CURRENT', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='OPTIMUM', ldelim=.true., ladv=.true.)
         ifail = output_value(iout, &
                 c='X modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='Goodness', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='X modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='Goodness', ldelim=.true., ladv=.true.)
      End If
      iopt = 0
      jopt = 1
      Do ixe = nxe, mxe
         iopt = iopt + 1
         Select Case (igauss)
          Case (it_non)
            gm(iopt,ir) = goodness(igood, iregr, nt, ny, ixe, 1, 1, yopt(:,:,iopt), y(:,1:nt), &
                          t=t)
          Case Default
            gm(iopt,ir) = goodness(igood, iregr, nt, ny, ixe, 1, 1, yopt(:,:,iopt), yc(:,:), &
                          t=t)
         End Select
         If (iopt == 1 .or. dgood*gm(iopt,ir) > dgood*gm(jopt,ir)) Then
            jopt = iopt
            nxo = ixe
         End If
         If (lexpert) Write (Unit=*, Fmt='(1X,2(10X,I5,F18.3))') ixe, gm(iopt,ir), nxo, gm(jopt,ir)
         If (gifile%cfile%lset) Write (Unit=iout, Fmt='(1X,2(10X,I5,F18.3))') ixe, gm(iopt,ir), nxo, gm(jopt,ir)
      End Do
      If (lexpert) Then ! - print only the optimal results if not running expert level -
         Call get_gi_fmt (igood, 18, gm(jopt,ir), gscale, cfmt)
         Write (Unit=*, Fmt='(1X,A,F'//cfmt//',A,I0,A)') &
               Trim(l_goodness%c)//': ', gm(jopt,ir)*gscale, ' (using ', nxo, ' X-modes)'
      End If
   Else
      jopt = 1
      Select Case (igauss)
       Case (it_non)
         gm(jopt,ir) = goodness(igood, iregr, nt, ny, mxe, 1, 1, yopt(:,:,jopt), y(:,1:nt), &
                       t=t)
       Case Default
         gm(jopt,ir) = goodness(igood, iregr, nt, ny, mxe, 1, 1, yopt(:,:,jopt), yc(:,:), &
                       t=t)
      End Select
      Call get_gi_fmt (igood, 18, gm(jopt,ir), gscale, cfmt)
      Write (Unit=*, Fmt='(1X,A,F'//cfmt//')') Trim(l_goodness%c)//': ', gm(jopt,ir)*gscale
      If (gifile%cfile%lset) Call print_rvalue (Trim(l_goodness%c), gm(jopt,ir), &
                                  iout=iout)
   End If
   ycv(:,1:nt) = yopt(:,1:nt,jopt)
   If (lstop) Return
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
   Return
 End Subroutine cv_pcr
!
!
!
 Subroutine do_pcr (nt, nx, x, ny, y, ieofx, nxe, svx, eofx, tsx, bz, lpos, mpx, npx, xm, xsd, ym, ifail, &
            t)
!
! Performs PCR
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: id_grd, ir_ols, is_ano
   Use settings,           Only: igauss, ilink, iregr
   Use arrays,             Only: iusex
   Use space,              Only: rlatx
   Use iofiles,            Only: xfile
   Use fields,             Only: xfield
   Use distribs,           Only: stdize, &
                                 moments
   Use pcs,                Only: lxt, &
                                 get_pcs
   Use mlr,                Only: do_mlr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt    ! - number of cases -
   Integer, Intent(In) :: nx    ! - number of x spatial points -
   Integer, Intent(In) :: ny    ! - number of y spatial points -
   Integer, Intent(In) :: nxe   ! - number of X EOF modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
   Integer, Intent(In) :: mpx   ! - minimum number of positive eigenvalues -
!
   Logical, Intent(In) :: lpos ! - force largest loading to be positive? -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: x ! - explanatory variables; shape(nx,nt) -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - response variables; shape(ny,nt) -
!
! - output scalars -
   Integer, Intent(Out) :: npx   ! - number of positive eigenvalues -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: svx ! - singular values of x -
   Real(Kind=rp), Dimension(:), Intent(Out) :: xm  ! - X means; shape(nx) -
   Real(Kind=rp), Dimension(:), Intent(Out) :: xsd ! - X standard deviations; shape(nx) -
   Real(Kind=rp), Dimension(:), Intent(Out) :: ym  ! - Y means; shape(ny) -
!
   Real(Kind=rp), Dimension( :,:), Intent(Out) :: eofx ! - x EOF patterns -
   Real(Kind=rp), Dimension( :,:), Intent(Out) :: tsx  ! - time-series of x EOFs (transposed) -
   Real(Kind=rp), Dimension(0:,:), Intent(Out) :: bz   ! - principal component regression coefficients -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! Locals
!
! Local scalars
   Integer :: ie ! - EOF mode index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Min, Transpose
!
! Executable Statements
!
! Calculate EOFs
   If (xfile%idstr == id_grd) Then
      Call get_pcs (lpos, nt, nx, lxt, ieofx, mpx, x(:,:), svx(:), eofx(:,:), tsx(:,:), npx, ifail, &
           vm=xm(:), vsd=xsd(:), ndom=xfile%nfl, nlts=xfield(:)%nlt, iuse=iusex(:), rlat=rlatx(:,:), regions=xfield(:)%region)
   Else
      Call get_pcs (lpos, nt, nx, lxt, ieofx, mpx, x(:,:), svx(:), eofx(:,:), tsx(:,:), npx, ifail, &
           vm=xm(:), vsd=xsd(:))
   End If
! - rescale EOF time series -
   Do ie = 1, npx
      tsx(ie,1:nt) = tsx(ie,1:nt)/svx(ie)
   End Do
!
! Compute regression coefficients
! - calculate Y means -
   Call moments (ny, nt, y, ym)
! - calculate coefficients -
   Select Case (iregr)
    Case (ir_ols)
      Call stdize (is_ano, igauss, 1, ny, nt, ym(1:ny), [zero], y(1:ny,1:nt)) ! - calculate Y anomalies -
      bz(1:nxe,1:ny) = MatMul(tsx(1:nxe,1:nt), Transpose(y(1:ny,1:nt))     )  ! - principal component regression coefficients -
      bz(0,1:ny) = ym(:)
    Case Default
      Do ie = 1, Min(nxe, npx)
         tsx(ie,1:nt) = tsx(ie,1:nt)*svx(ie)**2 ! - scale EOFs -
      End Do
      Call do_mlr (iregr, ilink, nt, nxe, tsx(:,1:nt), ny, y(1:ny,1:nt), bz(0,:), bz(1:nxe,:), &
           t=t)
      Do ie = 1, Min(nxe, npx)
         tsx(ie,1:nt) = tsx(ie,1:nt)/svx(ie)**2 ! - unscale EOFs -
      End Do
   End Select
   If (nxe < lxt) bz(nxe+1:,:) = zero
!
   Return
 End Subroutine do_pcr
!
!
!
 Subroutine pcr_predict (nf, nx, nxe, ieofx, x, svx, eofx, bz, fcast, &
            nt, t, xvp, fsx)
!
! Calculates predictions given new predictor values
!
! Modules
   Use data_numbers,       Only: zero, one, tol
   Use data_cpt_constants, Only: il_idt
   Use progress_meter,     Only: update_progress_meter
   Use settings,           Only: ilink
   Use arrays,             Only: xc, xm, xsd
   Use distribs,           Only: stdize
   Use pcs,                Only: mxe
   Use regression,         Only: glm_finv_link
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf    ! - number of forecasts -
   Integer, Intent(In) :: nx    ! - number of X variables -
   Integer, Intent(In) :: nxe   ! - number of X EOF modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: svx ! - singular values of x -
!
   Real(Kind=rp), Dimension( :,:), Intent(In) :: eofx ! - x EOF patterns -
   Real(Kind=rp), Dimension( :,:), Intent(In) :: x    ! - new predictor values -
   Real(Kind=rp), Dimension(0:,:), Intent(In) :: bz   ! - PC regression coefficients -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: fcast ! - forecast values -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nt ! - number of cases in training period -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients; shape(nt) -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: xvp ! - standard error from predictors -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fsx ! - predictor time scores -
!
! Locals
!
! Local scalars
   Integer :: i  ! - EOF mode index -
   Integer :: k  ! - forecast index -
   Integer :: me ! - maximum nuber of EOF modes -
!
! Local arrays
   Real(Kind=rp), Dimension(:,:), Allocatable :: tsxf ! - predictor time scores -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Max, Present, Real, Transpose
!
! Executable Statements
!
! Convert predictors to EOF scores
   If (Present(fsx)) Then
      me = Max(mxe, nxe)
   Else
      me = nxe
   End If
   Allocate (tsxf(me,nf))
! - standard error from predictors -
   If (Present(nt) .and. Present(xvp)) xvp(1:nf) = one/Real(nt, Kind=rp)
! - standardize predictors -
   xc(:,1:nf,1) = x(:,:)
   Call stdize (ieofx, 0, 1, nx, nf, xm(:), xsd(:), xc(:,1:nf,1))
! - calculate EOF scores -
   tsxf(1:me,1:nf) = MatMul(Transpose(eofx(:,1:me)), xc(:,1:nf,1))
   Do i = 1, me
      If (svx(i) > tol) Then
         If (Present(xvp) .and. i <= nxe) xvp(:) = xvp(:) + (tsxf(i,:)/svx(i))**2 ! - calculate standard error from predictors -
      Else
         tsxf(i,:) = zero
      End If
   End Do
   If (Present(fsx)) fsx(1:mxe,1:nf) = tsxf(:,:) ! - save EOF scores -
! - update progress meter -
   If (update_progress_meter(.false.) /= 0) Return
!
! Predict anomalies
   Do k = 1, nf
      fcast(:,k) = MatMul(tsxf(1:nxe,k), bz(1:nxe,:)) + bz(0,:)
   End Do
!
! Transform linear predictor for GLMs
   If (ilink /= il_idt) Then
      Call glm_finv_link (ilink, nf, fcast(:,:), &
           t=t)
   End If
! End of copy
!
   Deallocate (tsxf)
!
   Return
 End Subroutine pcr_predict
!
!
!
 Subroutine bz_to_b (eofx, xm, ym, bz, b, nxe,&
            b0)
!
! Converts PC regression coefficients to regular coefficients
!
! Modules
   Use data_cpt_constants, Only: ir_ols
   Use settings,           Only: iregr
!
! Dummy arguments
integer,intent(in)::nxe
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: xm ! - X-means -
   Real(Kind=rp), Dimension(:), Intent(In) :: ym ! - Y-means -
!
   Real(Kind=rp), Dimension( :,:), Intent(In) :: eofx ! - x EOF patterns -
   Real(Kind=rp), Dimension(0:,:), Intent(In) :: bz   ! - principal component regression coefficients -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: b ! - regression coefficients -
!
! Optional arguments
! - optional output scalars -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: b0 ! - regression constants -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Present
!
! Executable Statements
!
! Convert PC regression coefficients to regular coefficients
!print*,'bz',shape(bz(1:nxe,:))
!print*,'bz',shape(bz(1:,:)) ! FTN95 bug - see problem 72 -
!print*,'bz',shape(bz)
   b(:,:) = MatMul(eofx(:,:), bz(1:nxe,:))
   If (Present(b0)) Then
      b0(:) = MatMul(xm(:), b(:,:))
      If (iregr == ir_ols) Then
         b0(:) = ym(:) - b0(:)
      Else
         b0(:) = bz(0,:) - b0(:)
      End If
   End If
!
   Return
 End Subroutine bz_to_b
End Module pcr
