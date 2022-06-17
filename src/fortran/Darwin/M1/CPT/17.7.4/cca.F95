! Canonical Correlation Analysis (CCA)
! Author: Simon Mason
Module cca
!
! Modules
    Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cca_predict, cv_cca, full_cca
!
Contains
!
!
 Subroutine full_cca (nt, nx, x, ny, y, ieofx, nxe, svx, eofx, tsx, ieofy, nye, svy, eofy, tsy, ncc, mu, r, s, ifail, &
            hx_map, hy_map, hx_ser, hy_ser)
!
! Performs CCA using all data
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: id_grd
   Use cpt_lapack,         Only: gesdd
   Use progress_meter,     Only: update_progress_meter
   Use arrays,             Only: iusex, iusey, iwk, lrwk, rwk, xm, xsd, ym, ysd
   Use space,              Only: rlatx, rlaty
   Use iofiles,            Only: xfile, yfile
   Use fields,             Only: xfield, yfield
   Use pcs,                Only: ce, lc1, lr1, lxt, lxyt, lyt, mxe, npx, npy, &
                                 get_pcs
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt    ! - number of cases -
   Integer, Intent(In) :: nx    ! - number of x spatial points -
   Integer, Intent(In) :: ny    ! - number of y spatial points -
   Integer, Intent(In) :: nxe   ! - number of X EOF modes -
   Integer, Intent(In) :: nye   ! - number of Y EOF modes -
   Integer, Intent(In) :: ncc   ! - number of CCA modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
   Integer, Intent(In) :: ieofy ! - Y EOF option -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: x ! - explanatory variables -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - response variables -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: mu  ! - canonical correlations -
   Real(Kind=rp), Dimension(:), Intent(Out) :: svx ! - singular values of x -
   Real(Kind=rp), Dimension(:), Intent(Out) :: svy ! - singular values of y -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: eofx ! - x EOF patterns -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: eofy ! - y EOF patterns -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: tsx  ! - time-series of x EOFs (transposed) -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: tsy  ! - time-series of y EOFs (transposed) -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: r    ! - canonical Y EOF weights -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: s    ! - canonical X EOF weights (transposed) -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hx_map ! - X homogeneous covariance maps -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hy_map ! - Y homogeneous covariance maps -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hx_ser ! - X homogeneous covariance maps time series -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hy_ser ! - Y homogeneous covariance maps time series -
!
! Locals
!
! Local scalars
   Integer :: ie  ! - EOF mode index -
   Integer :: mnc ! - maximum number of CCA modes to be counted -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Min, Present, Transpose
!
! Executable Statements
!
! Calculate EOFs
   ifail = 1
! - X EOFs -
   If (xfile%idstr == id_grd) Then
      Call get_pcs (.true., nt, nx, lxt, ieofx, lxt, x(:,:), svx(:), eofx(:,:), tsx(:,:), npx, ifail, &
           vm=xm(:), vsd=xsd(:), ndom=xfile%nfl, nlts=xfield(:)%nlt, iuse=iusex(:), rlat=rlatx(:,:), regions=xfield(:)%region)
   Else
      Call get_pcs (.true., nt, nx, lxt, ieofx, lxt, x(:,:), svx(:), eofx(:,:), tsx(:,:), npx, ifail, &
           vm=xm(:), vsd=xsd(:))
   End If
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
! - Y EOFs -
   If (yfile%idstr == id_grd) Then
      Call get_pcs (.true., nt, ny, lyt, ieofy, lyt, y(:,:), svy(:), eofy(:,:), tsy(:,:), npy, ifail, &
           vm=ym(:), vsd=ysd(:), ndom=yfile%nfl, nlts=yfield(:)%nlt, iuse=iusey(:), rlat=rlaty(:,:), regions=yfield(:)%region)
   Else
      Call get_pcs (.true., nt, ny, lyt, ieofy, lyt, y(:,:), svy(:), eofy(:,:), tsy(:,:), npy, ifail, &
           vm=ym(:), vsd=ysd(:))
   End If
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Compute CCA
   ce(1:nye,1:nxe) = MatMul(tsy(1:nye,1:nt), Transpose(tsx(1:nxe,1:nt)))
   Call gesdd ('S', nye, nxe, ce, lc1, mu, r, lr1, s, lxyt, rwk, lrwk, iwk, ifail)
   If (ifail /= 0) Return
   mnc = Min(nxe, nye)
   If (lxyt > mnc) Then
      mu(mnc+1:) = zero
      r(:,mnc+1:) = zero
      If (lr1 > nye) r(nye+1:,1:mnc) = zero
      s(mnc+1:,:) = zero
      If (mxe > nxe) s(1:mnc,nxe+1:) = zero
   End If
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Compute homogeneous covariance maps
! - X maps -
   If (Present(hx_map)) Then
      Call hcov_maps (nx, nt, nxe, ncc, svx, eofx, tsx, s, hx_map, hx_ser, rwk(1:nxe*ncc))
! - update progress meter -
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   End If
! - Y maps -
   If (Present(hy_map)) Then
      Call hcov_maps (ny, nt, nye, ncc, svy, eofy, tsy, Transpose(r(1:nye,1:ncc)), hy_map, hy_ser, rwk(1:nye*ncc))
! - update progress meter -
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   End If
!
! Rescale EOF time series
! - X EOFs -
   Do ie = 1, npx
      tsx(ie,1:nt) = tsx(ie,1:nt)*svx(ie)
   End Do
! - Y EOFs -
   Do ie = 1, npy
      tsy(ie,1:nt) = tsy(ie,1:nt)*svy(ie)
   End Do
! - update progress meter -
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   ifail = 0
!
   Return
 End Subroutine full_cca
!
!
!
 Subroutine hcov_maps (nv, nt, ne, nc, sv, eof, ts, rs, hmap, hser, rwk)
!
! Constructs CCA maps
!
! Modules
   Use data_numbers, Only: zero
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv ! - number of variables -
   Integer, Intent(In) :: nt ! - number of cases -
   Integer, Intent(In) :: ne ! - number of EOF modes -
   Integer, Intent(In) :: nc ! - number of CCA modes -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: sv ! - singular values -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: eof ! - spatial loadings -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: ts  ! - temporal scores -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rs  ! - canonical EOF weights -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: hmap ! - homogeneous covariance maps -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: hser ! - homogeneous covariance map time series -
!
! Workspace
!
! Workspace arrays
   Real(Kind=rp) :: rwk(:) ! - real workspace -
!
! Locals
!
! Local scalars
   Integer :: i1, i2 ! - indices -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Reshape, Size
!
! Executable Statements
!
! Calculate homogeneous covariance maps
   Do Concurrent (i2 = 1:nc)
      Do Concurrent (i1 = 1:ne)
         rwk((i2-1)*ne+i1) = sv(i1)*rs(i2,i1)
      End Do
   End Do
   hmap(1:nv,1:nc) = MatMul(eof(1:nv,1:ne), Reshape(rwk(1:ne*nc), [ne,nc]))
!
! Calculate corresponding time series
   hser(1:nc,1:nt) = MatMul(rs(1:nc,1:ne), ts(1:ne,1:nt))
!
! Assign unalloted modes
   If (nc < Size(hmap,Dim=2)) Then
      hmap(:,nc+1:) = zero
      hser(nc+1:,:) = zero
   End If
!
   Return
 End Subroutine hcov_maps
!
!
!
 Subroutine cv_cca (nt, ncv, nx, x, ny, y, ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, lopt, ycv, &
            nxo, nyo, nco)
!
! Performs cross-validated CCA
!
! Modules
   Use data_numbers,       Only: one, tol
   Use data_cpt_constants, Only: ia_cca, id_grd, ir_ols, it_emp, it_gam, it_non
   Use data_io_constants,  Only: iout
   Use cpt_lapack,         Only: gesdd
   Use labels,             Only: ca_atypes_a, cg_na, l_goodness
   Use gui,                Only: print_rvalue
   Use progress_meter,     Only: lstop, &
                                 update_progress_meter
   Use settings,           Only: dgood, igauss, igood, istd, istdr, hcw, lexpert, lzb
   Use arrays,             Only: iusex, iusey, iwk, lrwk, rwk, xc, xm, xsd, ybk, yc, ym, ysd, &
                                 get_cvsample
   Use space,              Only: rlatx, rlaty
   Use iofiles,            Only: gifile, xfile, yfile
   Use fields,             Only: xfield, yfield
   Use distribs,           Only: gaussian, get_gamma, zero_bound
   Use pcs,                Only: ce, eofx, eofy, icco, lc1, lr1, lxt, lxyt, lyt, npx, npy, mu, r, s, svx, svy, tsx, tsy, &
                                 get_pcs
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
   Integer, Intent(In) :: nye   ! - minimum number of Y EOF modes -
   Integer, Intent(In) :: mye   ! - maximum number of Y EOF modes -
   Integer, Intent(In) :: ncc   ! - minimum number of CCA modes -
   Integer, Intent(In) :: mcc   ! - maximum number of CCA modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
   Integer, Intent(In) :: ieofy ! - Y EOF option -
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
! - optional output scalars -
   Integer, Intent(Out), Optional :: nxo ! - optimal number of X EOF modes -
   Integer, Intent(Out), Optional :: nyo ! - optimal number of Y EOF modes -
   Integer, Intent(Out), Optional :: nco ! - optimal number of CCA modes -
!
! Locals
!
! Local scalars
   Integer :: it    ! - cross-validation time-step -
   Integer :: ixe   ! - X EOF mode index -
   Integer :: iye   ! - Y EOF mode index -
   Integer :: icc   ! - current number of CCA modes -
   Integer :: ixu   ! - used X EOF mode index -
   Integer :: iyu   ! - used Y EOF mode index -
   Integer :: icu   ! - current number of used CCA modes -
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
   Intrinsic AdjustR, Any, MatMul, Min, Present, Transpose, Trim
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
         Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
              v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
       Case (it_emp)
         Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
              v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         Call gaussian (igauss, 1, ny, ncv, yc(1:ny,1:ncv))
       Case (it_gam)
         Call get_cvsample (hcw, it, ybk(:,1:nt,1), yc(:,1:nt), &
              v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
         Call get_gamma (ny, ncv, 1, yc(:,1:ncv))
         Call gaussian (igauss, 1, ny, ncv, yc(1:ny,1:ncv))
      End Select
!
! Calculate EOFs
! - X EOFs -
      If (xfile%idstr == id_grd) Then
         Call get_pcs (.true., ncv, nx, lxt, ieofx, lxt, xc(:,1:ncv,1), svx(:), eofx(:,:), tsx(:,:), npx, ifail, &
              vm=xm(:), vsd=xsd(:), ndom=xfile%nfl, nlts=xfield(:)%nlt, iuse=iusex(:), rlat=rlatx(:,:), regions=xfield(:)%region)
      Else
         Call get_pcs (.true., ncv, nx, lxt, ieofx, lxt, xc(:,1:ncv,1), svx(:), eofx(:,:), tsx(:,:), npx, ifail, &
              vm=xm(:), vsd=xsd(:))
      End If
! - update progress meter -
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
      If (npx == 0) Then
         ifail = 1
         GoTo 1
      End If
! - Y EOFs -
      If (yfile%idstr == id_grd) Then
         Call get_pcs (.true., ncv, ny, lyt, ieofy, lyt, yc(:,1:ncv), svy(:), eofy(:,:), tsy(:,:), npy, ifail, &
              vm=ym(:), vsd=ysd(:), ndom=yfile%nfl, nlts=yfield(:)%nlt, iuse=iusey(:), rlat=rlaty(:,:), regions=yfield(:)%region)
      Else
         Call get_pcs (.true., ncv, ny, lyt, ieofy, lyt, yc(:,1:ncv), svy(:), eofy(:,:), tsy(:,:), npy, ifail, &
              vm=ym(:), vsd=ysd(:))
      End If
! - update progress meter -
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
      If (npy == 0) ifail = 1
!
! Compute CCA
1     iopt = 0
      If (ifail == 0) Then
         Do ixe = nxe, mxe
            ixu = Min(ixe, npx)
            Do iye = nye, mye
               iyu = Min(iye, npy)
               ce(1:iyu,1:ixu) = MatMul(tsy(1:iyu,1:ncv), Transpose(tsx(1:ixu,1:ncv)))
               Call gesdd ('S', iyu, ixu, ce, lc1, mu, r, lr1, s, lxyt, rwk, lrwk, iwk, ifail)
!
! Check for over-fitting
               If (icco == 0 .and. Any(mu(1:Min(ixu, iyu)) > one - tol)) Then
                  Do icc = ncc, Min(mcc, ixe, iye)
                     iopt = iopt + 1
                     lopt(iopt) = .false.
                     yopt(1:ny,it,iopt) = ym(1:ny)
                     If (update_progress_meter(.false., ifail=ifail) /= 0) Return
                  End Do
                  Cycle
               Else
!
! Predict anomaly
                  Do icc = ncc, Min(mcc, ixe, iye)
                     icu = Min(icc, ixu, iyu)
                     iopt = iopt + 1
                     If (icco == 1 .or. lopt(iopt)) Then
                        Call cca_predict (1, nx, ixu, iyu, icu, ieofx, x(1:nx,it:it), ieofy, yopt(1:ny,it:it,iopt))
                        If (lzb) Call zero_bound (istdr, ny, 1, ave(:,1), sdev(:,1), yopt(1:ny,it:it,iopt))
                     Else
                        yopt(1:ny,it,iopt) = ym(1:ny)
                        If (update_progress_meter(.false., ifail=ifail) /= 0) Return
                     End If
                  End Do
               End If
            End Do
         End Do
!
! Supply mean if CCA failed
      Else
         Do ixe = nxe, mxe
            Do iye = nye, mye
               Do icc = ncc, Min(mcc, ixe, iye)
                  iopt = iopt + 1
                  yopt(1:ny,it,iopt) = ym(1:ny)
                  If (update_progress_meter(.false., ifail=ifail) /= 0) Return
               End Do
            End Do
         End Do
      End If
      If (lstop) Return
   End Do time_step
!
! Transform observations to calculate goodness
   If (igauss > 0) Then
      yc(1:ny,1:nt) = y(1:ny,1:nt)
      If (istd /= 3) Call gaussian (igauss, 1, ny, nt, yc(:,:)) ! - NB strictly, this step should be cross-validated (as below) -
   End If
!
! Calculate and print goodness metric
! - print headers -
   If ((Present(nxo) .or. Present(nyo) .or. Present(nco)) .and. Any(lopt(:))) Then
      If (lexpert) Then
         Print *, '              CURRENT                         OPTIMUM'
         Print *, ' '
         Print *, '     Number of Modes    Goodness      Number of Modes    Goodness'
         Print *, '        X    Y   '//ca_atypes_a(ia_cca)//'       Index         X    Y   '//ca_atypes_a(ia_cca)//'       Index'
         Print *, ' '
      End If
      If (gifile%cfile%lset) Then
         ifail = output_value(iout, &
                 c='CURRENT', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='OPTIMUM', ldelim=.true., ladv=.true.)
         ifail = output_value(iout, &
                 c='X modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='Y modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='CCA modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='Goodness', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='X modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='Y modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='CCA modes', ldelim=.true., ladv=.false.)
         ifail = output_value(iout, &
                 c='Goodness', ldelim=.true., ladv=.true.)
      End If
! - calculate goodness -
      iopt = 0
      jopt = 1
      Do ixe = nxe, mxe
         Do iye = nye, mye
            Do icc = ncc, Min(mcc, ixe, iye)
               iopt = iopt + 1
               If (gifile%cfile%lset) Then
                  ifail = output_value(iout, &
                          i=ixe, ldelim=.true., ladv=.false.)
                  ifail = output_value(iout, &
                          i=iye, ldelim=.true., ladv=.false.)
                  ifail = output_value(iout, &
                          i=icc, ldelim=.true., ladv=.false.)
               End If
               If (lopt(iopt)) Then
                  Select Case (igauss)
                   Case (it_non)
                     gm(iopt,ir) = goodness(igood, ir_ols, nt, ny, ixe, iye, icc, yopt(:,:,iopt), y, &
                                   cc=mu(:))
                   Case Default
                     gm(iopt,ir) = goodness(igood, ir_ols, nt, ny, ixe, iye, icc, yopt(:,:,iopt), yc, &
                                   cc=mu(:))
                  End Select
                  If (iopt == 1 .or. dgood*gm(iopt,ir) > dgood*gm(jopt,ir)) Then
                     jopt = iopt
                     nxo = ixe
                     nyo = iye
                     nco = icc
                  End If
! - determine appropriate precision -
                  Call get_gi_fmt (igood, 12, gm(iopt,ir), gscale, cfmt)
! - output goodness -
                  If (lexpert) Write (Unit=*, Fmt='(2(5X,2I5,I6,F'//cfmt//'))') &
                                      ixe, iye, icc, gm(iopt,ir)*gscale, nxo, nyo, nco, gm(jopt,ir)*gscale
                  If (gifile%cfile%lset) ifail = output_value(iout, &
                                                              r=gm(iopt,ir), ldelim=.true., ladv=.false.)
               Else
                  If (lexpert) Write (Unit=*, Fmt='(5X,2I5,I6,A12,5X,2I5,I6,F'//cfmt//')') &
                                      ixe, iye, icc, AdjustR(cg_na), nxo, nyo, nco, gm(jopt,ir)
                  If (gifile%cfile%lset)  ifail = output_value(iout, &
                                                               c=cg_na, ldelim=.true., ladv=.false.)
               End If
               If (gifile%cfile%lset) Then
                  ifail = output_value(iout, &
                          i=nxo, ldelim=.true., ladv=.false.)
                  ifail = output_value(iout, &
                          i=nyo, ldelim=.true., ladv=.false.)
                  ifail = output_value(iout, &
                          i=nco, ldelim=.true., ladv=.false.)
                  ifail = output_value(iout, &
                          r=gm(jopt,ir), ldelim=.true., ladv=.true.)
               End If
            End Do
         End Do
         If (lstop) Return
      End Do
      If (lexpert) Then ! - print only the optimal results if not running expert level -
         Call get_gi_fmt (igood, 18, gm(jopt,ir), gscale, cfmt)
         Write (Unit=*, Fmt='(1X,A,F'//cfmt//',A,I0,A,I0,A,I0,A)') &
               Trim(l_goodness%c)//': ', gm(jopt,ir)*gscale, ' (using ', nxo, ' X-,', nyo, ' Y-, and ', nco, ' CCA-modes)'
      End If
   Else
      jopt = 1
      Select Case (igauss)
       Case (it_non)
         gm(jopt,ir) = goodness(igood, ir_ols, nt, ny, ixe, iye, icc, yopt(:,:,iopt), y, &
                       cc=mu(:))
       Case Default
         gm(jopt,ir) = goodness(igood, ir_ols, nt, ny, ixe, iye, icc, yopt(:,:,iopt), yc, &
                       cc=mu(:))
      End Select
      Call get_gi_fmt (igood, 18, gm(jopt,ir), gscale, cfmt)
      Write (Unit=*, Fmt='(1X,A,F'//cfmt//')') Trim(l_goodness%c)//': ', gm(jopt,ir)*gscale
      If (gifile%cfile%lset) Call print_rvalue (Trim(l_goodness%c), gm(jopt,ir), &
                                  iout=iout)
! - set optimized values if there were no successful calculations -
      If (Present(nxo)) nxo = 1
      If (Present(nyo)) nyo = 1
      If (Present(nco)) nco = 1
   End If
   ycv(:,1:nt) = yopt(:,1:nt,jopt)
   If (lstop) Return
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
   Return
 End Subroutine cv_cca
!
!
!
 Subroutine cca_predict (nf, nx, nxe, nye, ncc, ieofx, x, ieofy, fcast, &
            nt, xvp, fsx, fsxc)
!
! Calculates predictions given new predictor values
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: is_ano, is_std
   Use progress_meter,     Only: update_progress_meter
   Use arrays,             Only: rwk, xc, xm, xsd, ym, ysd
   Use distribs,           Only: stdize
   Use pcs,                Only: eofx, eofy, mxe, mu, prjc, r, s, svx, svy
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf    ! - number of forecasts -
   Integer, Intent(In) :: nx    ! - number of X variables -
   Integer, Intent(In) :: nxe   ! - number of X EOF modes -
   Integer, Intent(In) :: nye   ! - number of Y EOF modes -
   Integer, Intent(In) :: ncc   ! - number of CCA modes -
   Integer, Intent(In) :: ieofx ! - X EOF option -
   Integer, Intent(In) :: ieofy ! - Y EOF option -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - new predictor values -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: fcast ! - forecast values -
!
! Optional Arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nt  ! - number of cases in training period -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: xvp ! - standard error from predictors -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fsx    ! - predictor time scores -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fsxc   ! - predictor CCA time scores -
!
! Locals
!
! Local scalars
   Integer :: i ! - EOF mode index -
   Integer :: k ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MatMul, Present, Real, Sum, Transpose
!
! Executable Statements
!
! Standardize predictors
   xc(:,1:nf,1) = x(:,:)
   Call stdize (ieofx, 0, 1, nx, nf, xm, xsd, xc(:,1:nf,1))
!
! Calculate standard error from predictors
   If (Present(xvp)) xvp(1:nf) = one/Real(nt, Kind=rp)
!
! Project X anomalies onto the X EOFs and X CCA modes
   Do k = 1, nf
      rwk(1:nxe) = MatMul(Transpose(eofx(:,1:nxe)), xc(:,k,1))
      If (Present(fsx)) Then
         fsx(1:nxe,k) = rwk(1:nxe)
         Do i = nxe+1, mxe ! - calculate unused principal component scores in case they are wanted -
            fsx(i,k) = Sum(eofx(:,i)*xc(:,k,1))
         End Do
      End If
      If (Present(fsxc)) fsxc(1:ncc,k) = MatMul(s(1:ncc,1:nxe), fsx(1:nxe,k)/svx(1:nxe))
! - standardise to unit variance by scaling by inverse of singular values -
      rwk(1:nxe) = rwk(1:nxe)/svx(1:nxe)
!
! Project X EOFs onto the CCA modes
      prjc(1:ncc) = MatMul(s(1:ncc,1:nxe), rwk(1:nxe))
! - update standard error -
      If (Present(xvp)) xvp(k) = xvp(k) + Sum(prjc(1:ncc))**2
! - scale by canonical correlations -
      prjc(1:ncc) = prjc(1:ncc)*mu(1:ncc)
      If (update_progress_meter(.false.) /= 0) Return
!
! Project CCA modes back onto the Y EOF modes
      rwk(1:nye) = MatMul(r(1:nye,1:ncc), prjc(1:ncc))
! - scale by singular values -
      rwk(1:nye) = rwk(1:nye)*svy(1:nye)
!
! Project Y EOF modes back onto the Y anomalies
      fcast(:,k) = MatMul(eofy(:,1:nye), rwk(1:nye))
!
! Rescale
      Select Case (ieofy)
       Case (is_std)
         fcast(:,k) = fcast(:,k)*ysd(:) + ym(:)
       Case (is_ano)
         fcast(:,k) = fcast(:,k) + ym(:)
      End Select
      If (update_progress_meter(.false.) /= 0) Return
   End Do
!
   Return
 End Subroutine cca_predict
End Module cca
