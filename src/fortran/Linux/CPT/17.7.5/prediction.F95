! Author: Simon Mason
Module prediction
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: add_persistence, calc_errvar, calc_fcast, calc_pexc, calc_predictions_d, calc_predictions_p, close_hps, close_pexc, &
             close_predict, forecast_bounds, get_fprds, init_pexc, init_predict, parameter_write_predict, proj_read_predict,     &
             proj_write_predict, read_predict, reset_predict, set_fcst, set_ifcast, skillmask, untransform_f, update_fprds
#if GUI == 1
   Public :: init_hps
#endif
!
! Scalars
!
! Integer scalars
   Integer, Public :: ifc    ! - recalculate forecast flag -
!                               (0 = allocate and calculate forecast;
!                                1 = calculate forecast;
!                                2 = do nothing (forecast has been calculated already);
!                               -1 = deallocate first)
   Integer, Public :: ifcast ! - operational forecast flag -
   Integer, Public :: ifm    ! - index of current forecast -
!
! Routines
!
Contains
!
!
 Subroutine reset_predict ()
!
! Resets prediction settings
!
! Modules
   Use menus, Only: set_menu_greying
!
! Executable Statements
!
! Reset prediction settings
   ifcast = 0
   ifc = 0
   Call set_menu_greying (ifcast=ifcast)
!
   Return
 End Subroutine reset_predict
!
!
!
 Function init_predict(ianalc, ihind, lproj) &
          Result (init)
!
! Initialises memory for prediction
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ir_bin, ng
   Use settings,           Only: igauss, iregr, nenf, nf, nu
   Use arrays,             Only: tf, xc, xiny, z, ziny, zyp
   Use iofiles,            Only: mxa, mya, nx, ny, zfile
   Use pcs,                Only: fsx, fsxc, mxe, nco
   Use regression,         Only: ens_p, fcast, fcastt, pesd_p, pls_p, xvp
   Use season,             Only: lprst, lsn
   Use missing,            Only: kfuse
   Use categories,         Only: fps_p, odds_p
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianalc ! - analysis identifier -
   Integer, Intent(In) :: ihind  ! - hindcasts identifier -
!
   Logical, Intent(In) :: lproj ! - initiate from project opening? -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - memory allocation flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Ubound
!
! Executable Statements
!
! Allocate workspace
! - forecast data -
   Allocate (z(nx,nf,zfile%ngs), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - copy of x -
   Allocate (xc(mxa,Max(nu,nf),1), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - binomial coefficients -
   If (ihind == 1) Then
      If (iregr == ir_bin) Then
         Allocate (tf(nf), Stat=ifail)
         If (ifail /= 0) GoTo 1
      End If
! - forecasts -
      If (.not.lprst) Then
         Allocate (fcast(mya,nf,1), Stat=ifail)
      Else
         Allocate (fcast(mya,nf,0:2), Stat=ifail)
      End If
! - ensemble forecasts -
      Allocate (ens_p(mya,nf,nenf), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - transformed forecasts -
      If (igauss > 0) Then
         Allocate (fcastt(mya,nf), Stat=ifail)
         If (ifail /= 0) GoTo 1
      End If
! - prediction-error variance -
      Allocate (pesd_p(mya,nf), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - standard error from predictors -
      Select Case (ianalc)
       Case (ia_cca, ia_pcr, ia_mlr)
         Allocate (xvp(1,nf), Stat=ifail)
         If (ifail /= 0) GoTo 1
       Case (ia_gcm)
         Allocate (xvp(mya,nf), Stat=ifail)
         If (ifail /= 0) GoTo 1
      End Select
! - forecast prediction limits -
      Allocate (pls_p(mya,nf), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - forecast probabilities -
   Allocate (fps_p(mya,nf,ng), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - odds -
   Allocate (odds_p(mya,nf,ng,2), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - projections onto X EOFs -
   Select Case (ianalc)
    Case (ia_cca)
      Allocate (fsx(mxe,nf), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - projections onto X EOFs -
      Allocate (fsxc(nco,nf), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - projections onto X EOFs -
    Case (ia_pcr)
      Allocate (fsx(mxe,nf), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - forecast data -
    Case (ia_gcm)
      Allocate (ziny(mya,nf,Ubound(xiny,Dim=3)), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - forecast data -
    Case (ia_pfv)
      Allocate (ziny(mya,nf,zfile%ngs), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End Select
! - used forecast flag -
   Allocate (kfuse(nf), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - persistence component (not needed when reading results from project file) -
   If (lprst .and. .not.lproj) Then
      Allocate (zyp(ny,nf,0:lsn(0)), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
!
1  init = ifail
!
   Return
 End Function init_predict
!
!
!
 Function init_pexc() &
          Result (init)
!
! Initialises memory for exceedence probabilities
!
! Modules
   Use data_cpt_constants, Only: nep
   Use arrays,             Only: tex
   Use regression,         Only: c, e, f
!
! Function result
   Integer :: init
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Set forecast probability contours
   init = 1
   If (.not.Allocated(f)) Allocate (f(0:nep), Stat=ifail)
   If (ifail /= 0) Return
   If (.not.Allocated(e)) Allocate (e(0:nep), Stat=ifail)
   If (ifail /= 0) Return
   If (.not.Allocated(c)) Allocate (c(0:nep), Stat=ifail)
   If (ifail /= 0) Return
   If (.not.Allocated(tex)) Allocate (tex(2), Stat=ifail)
   If (ifail /= 0) Return
   init = 0
!
   Return
 End Function init_pexc
!
!
!
#if GUI == 1
 Function init_hps() &
          Result (init)
!
! Initialises memory for highest probability mapping
!
! Modules
   Use settings,   Only: nf
   Use iofiles,    Only: mya
   Use categories, Only: hps
!
! Function result
   Integer :: init
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Set forecast probability contours
   init = 1
   If (.not.Allocated(hps)) Allocate (hps(mya,nf), Stat=ifail)
   If (ifail /= 0) Return
   init = 0
!
   Return
 End Function init_hps
!
!
!
#endif
 Subroutine set_ifcast ( &
            ifc)
!
! Sets Tools ~ Forecast menu item grey state
!
! Modules
   Use menus,   Only: set_menu_greying
   Use iofiles, Only: zfile
!
! Arguments
!
! Optional arguments
! - input scalars -
   Integer, Intent(In), Optional :: ifc ! - forecast calculation flag -
!
! Executable Statements
!
! Set Tools ~ Forecast menu item grey state
   If (zfile%cfile%lset) Then
      ifcast = 1
   Else
      ifcast = 0
   End If
   Call set_menu_greying (ifcast=ifcast, ifc=ifc)
!
   Return
 End Subroutine set_ifcast
!
!
!
 Function set_fcst(iopen)
!
! Sets forecasting to ready state
!
! Modules
   Use screen,   Only: window_update
   Use settings, Only: nf
   Use iofiles,  Only: bkfile, mza, nz, zfile, &
                       init_ifile
   Use fields,   Only: tfield, xfield, zfield
!
! Function result
   Integer :: set_fcst
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iopen ! - forecast file is open (0 = false; 1 = true) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Associated, Size
!
! Executable Statements
!
! Prompt for forecast file
   set_fcst = iopen
   Select Case (set_fcst)
    Case (0)
      If (zfile%cfile%lset) Then
         If (zfile%cfile%ffile == bkfile%cfile%ffile) Then
            ifc = -1
         Else
            ifc = 0
         End If
! - reset if cancelled -
      Else
         zfield(:) = tfield(:)
         ifc = 2
      End If
    Case Default
      set_fcst = close_predict()
      Call init_ifile (zfile)
      ifc = 0
      If (set_fcst == 1) Then
         If (Associated(tfield)) Nullify (tfield)
         Allocate (zfield(Size(xfield)))
         zfield(:) = xfield(:)
      End If
   End Select
   If (ifc == 0) ifc = close_predict()
! - set Tools ~ Forecast menu item grey state -
   Call set_ifcast ()
!
! Update window
   If (set_fcst == 0) Then
      Call window_update (nf)
      Call window_update (nz)
      Call window_update (mza)
   End If
   Call window_update (ifcast)
   set_fcst = 2
!
   Return
 End Function set_fcst
!
!
!
 Function read_predict(ianal, ihind)
!
! Reads data for forecasting
!
! Modules
   Use data_numbers,        Only: rp, one
   Use data_time_constants, Only: isq_yr, nmn
   Use time,                Only: Operator(+), &
                                  iseq, &
                                  date_diff
   Use progress_meter,      Only: lstop, &
                                  end_progress_meter, set_progress_increment, start_progress_meter
   Use settings,            Only: nf, nmf, &
                                  set_it1
   Use arrays,              Only: z, zyp
   Use space,               Only: idomx, idomy
   Use iofiles,             Only: mya, mfile, pfile, zfile
   Use fields,              Only: mfield, yfield, zfield
   Use regression,          Only: fcast
   Use season,              Only: lsn, lprst, nsn, &
                                  seasonal_zyp
   Use missing,             Only: kavy, kavz, &
                                  replace_missing_z, replace_missing_zyp
   Use cpt_input,           Only: num_read, read_data
!
! Function result
   Integer :: read_predict
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: ihind ! - hindcasts identifier -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Check first date of interest
   ifail = 3
   zfile%bdate = zfile%fdate
   zfile%it1 = set_it1(zfile%iseq, zfield(1)%tssn%tprd(1)%sdate, zfile%bdate)
   If (lprst) Then
      ifail = 4
      pfile%it1 = set_it1(pfile%iseq, mfile%prd1%sdate, pfile%bdate)
!
! Determine number of months to read
      Select Case (iseq)
       Case (isq_yr)
         nmf = (nf - 1)*nmn + lsn(2)
       Case Default
         nmf = date_diff(pfile%bdate, pfile%fdate+(nf-1), pfile%iseq) + 1
      End Select
   Else
      nmf = nf
   End If
!
! Initialise progress meter
   Call start_progress_meter (' ', .false.)
   Call set_progress_increment &
      (one/Real(num_read(zfile%idstr, zfile%lstack, zfile%nt, nmf, Sum(zfield(:)%nlt), &
                         zfile%nfs, zfile%nls, zfile%ngs, zfile%it1), Kind=rp), .false.)
!
! Allocate workspace
   If (init_predict(ianal, ihind, .false.) > 0) Then
      ifail = 1
      GoTo 1
   End If
!
! Read updated predictors
   Call read_data (zfile, zfield(:), ianal, nf, 1, 1, idomx(:,:), kavz(:,:), z(:,:,:), ifail)
   If (ifail > 0) Then
      ifail = 2
      GoTo 1
   End If
!
! Read persistence data
   If (lprst) Then
      Call read_data (pfile, mfield(:), ianal, nf, 1, lsn(0), idomy(:,:), kavy(:,:), zyp(:,:,1:), ifail)
      If (ifail > 0) Then
         ifail = 2
         GoTo 1
      End If
!
! Replace missing monthly persistence data
      If (lsn(0) > 1) ifail = replace_missing_zyp(1, lsn(0))
!
! Calculate seasonal aggregates
      Call seasonal_zyp (yfield(:)%tssn%tprd(0)%sdate%imn, yfield(:)%rmiss)
   End If
!
! Replace missing values and check for missing variables
   ifail = replace_missing_z(ianal, nsn)
   If (ifail /= 0) Then
      ifail = 5 - ifail
      GoTo 1
   End If
! - replace missing persisted component -
   If (lprst) Then
      ifail = replace_missing_zyp(0, 0)
      If (ifail /= 0) Then
         ifail = 5
         GoTo 1
      End If
      fcast(:,:,0) = zyp(1:mya,:,0)
      Deallocate (zyp) ! - clear memory -
   End If
!
! Set initial values
   ifm = 1
!
1  If (lstop) ifail = -1
   read_predict = ifail
!
   Call end_progress_meter (.false., .false.)
!
   Return
 End Function read_predict
!
!
!
 Function calc_fcast(ianal, &
          t) Result (calc)
!
! Calculate forecasts
!
! Modules
   Use data_numbers,       Only: rp, one
   Use data_cpt_constants, Only: ia_mlr, ia_gcm, ia_pfv, ie_fit, is_sti, it_non
   Use progress_meter,     Only: lstop, &
                                 end_progress_meter, set_progress_increment, start_progress_meter
   Use settings,           Only: iev, igauss, intp, iskmask, istd, istdr, izero, nenf, nf, nu
   Use arrays,             Only: iusex, iusey, y, z, ziny
   Use space,              Only: get_interpolated
   Use iofiles,            Only: mxa, mya, yfile, zfile
   Use fields,             Only: yfield, zfield
   Use pcs,                Only: nxo
   Use regression,         Only: dofr, fcast, fcastt, ens_p, pls_p, pesd_p, u, xvp, ys2, &
                                 calc_pesd
   Use season,             Only: isnc, lb, lprst
   Use categories,         Only: fps_p, odds_p
   Use climate,            Only: clim, ncu
!
! Function result
   Integer :: calc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: dprog ! - progess increment -
!
   Logical :: lstd ! - restandardize? (if standardization is not the same as at run-time) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Initialise progress meter
   calc = -1
   Call start_progress_meter ('Calculating forecasts', .false.)
   Select Case (igauss)
    Case (it_non)
      dprog = Real(2*(nf+izero), Kind=rp)
    Case Default
      dprog = Real(2*(nf+1+izero), Kind=rp)
   End Select
   If (iev == ie_fit) dprog = dprog + Real(nu+izero, Kind=rp)
   Call set_progress_increment (one/dprog, .false.)
   lstd = (istd /= istdr)
!
! Calculate deterministic forecasts
   If (ianal /= ia_pfv) Then
      Select Case (ianal)
       Case Default
         Call calc_predictions_d (ianal, nf, iskmask, .true., lstd, z(1:mxa,:,1:1), fcast(:,:,1), &
              nt=nu, xvp1=xvp(1,:), t=t)
       Case (ia_mlr)
         Call calc_predictions_d (ianal, nf, iskmask, .true., lstd, z(1:mxa,:,1:1), fcast(:,:,1), &
              nt=nu, xvp1=xvp(1,:), u2=u(1,:,:), t=t)
       Case (ia_gcm)
         Call get_interpolated (ianal, zfile%idstr, yfile%idstr, intp, mya, iusex, iusey, zfile%nms, yfile%nfl, nf, zfile%ngs, &
              zfield(:)%nv, yfield(1:)%nva, z(1:mxa,:,:), yfield(1:)%rmiss, zfield(:)%region, yfield(1:)%region, ziny(:,:,1:))
         Call calc_predictions_d (ianal, nf, iskmask, .true., lstd, ziny(:,:,:), fcast(:,:,1), &
              nt=nu, xvp2=xvp(:,:), u3=u(:,:,:), t=t)
      End Select
      If (lstop) GoTo 1
!
! Add persistence component
      If (lprst) Then
         Select Case (igauss)
          Case (it_non) ! - no transformation -
            Call add_persistence (nf, .false., .true., .true., yfile%bdate, fcast(:,:,0), fcast(:,:,1), fcast(:,:,2))
          Case Default ! - transformation -
            Call add_persistence (nf, .true., (istd /= is_sti), .true., yfile%bdate, fcast(:,:,0), fcast(:,:,1), fcast(:,:,2), &
                 nr=nu, clim=y(1:mya,1:nu,1:), ft=fcastt(:,:))
         End Select
      Else If (igauss /= 0) Then
         fcastt(:,:) = fcast(:,:,1)
      End If
!
! Calculate prediction-error variance
      Call calc_pesd (nu, nxo, nf, mya, (ianal == ia_gcm), ys2(:), xvp(:,:), dofr, pesd_p(:,:))
      If (lstop) Return
   End If
!
! Calculate probabilities and prediction intervals
   Select Case (igauss)
    Case (it_non) ! - no transformation -
      Call calc_predictions_p (ianal, nf, nenf, ncu(1), .true., dofr, pesd_p(:,:), clim(:,:,1), fcast(:,:,isnc), ens_p(:,:,:), &
           fps_p(:,:,:), pls_p(:,:), odds_p(:,:,:,:))
    Case Default ! - transformation -
      Call calc_predictions_p (ianal, nf, nenf, ncu(1), .true., dofr, pesd_p(:,:), clim(:,:,1), fcastt(:,:), ens_p(:,:,:), &
           fps_p(:,:,:), pls_p(:,:), odds_p(:,:,:,:))
!
! Untransform the forecasts
      If (istd /= is_sti) Call untransform_f (nu, nf, nenf, lb, y(1:mya,1:nu,lb:), fcast(:,:,lb:), pls_p(:,:), ens_p(:,:,:))
   End Select
!
! Apply bounds
   If (ianal /= ia_pfv) Then
      If (izero == 1) Call forecast_bounds (nf, nenf, fcast(:,:,1:), pls_p(:,:), ens_p(:,:,:))
   End If
!
1  Call end_progress_meter (.false., .true.)
   calc = 0
!
   Return
 End Function calc_fcast
!
!
!
 Function calc_pexc(ymin, ymax, yscale) &
          Result (calc)
!
! Calculate exceedence probabilities
!
! Modules
   Use data_numbers,       Only: rp, zero, one, oneh
   Use data_cpt_constants, Only: nep, is_non, is_ano, is_std, is_sti, is_pav, it_non, it_emp, it_gam
   Use gui,                Only: set_cursor_waiting
   Use progress_meter,     Only: set_progress_increment
   Use settings,           Only: iexc, igauss, istd, iva, nu
   Use arrays,             Only: rwk, tex, y, &
                                 insertion_sort
   Use distribs,           Only: alpha, beta, pcen, &
                                 cdf_gamma, cdf_empirical, cdf_normal, q_normal, s_student_t, ustdize
   Use regression,         Only: c, dofr, e, f, fcast, fcastt, pesd_p
   Use season,             Only: isnc
   Use climate,            Only: ave, clim, nc, sdev
!
! Function result
   Integer :: calc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: ymax   ! - y maximum -
   Real(Kind=rp), Intent(In) :: ymin   ! - y minimum -
   Real(Kind=rp), Intent(In) :: yscale ! - y scaling -
!
! Locals
!
! Local scalars
   Integer :: i ! - probability of exceedance index -
!
   Real(Kind=rp) :: x    ! - x-value -
   Real(Kind=rp) :: xinc ! - x-value increment -
   Real(Kind=rp) :: xmin ! - x minimum -
   Real(Kind=rp) :: df   ! - number of cases -
   Real(Kind=rp) :: vbar ! - mean -
   Real(Kind=rp) :: vsd  ! - standard deviation -
!
! Local arrays
   Real(Kind=rp), Dimension(1,1) :: xx ! - x-value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Real
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Determine mean and variance
   Select Case (istd)
    Case (is_non) ! - no standardization -
      vbar = ave(iva,isnc)
      vsd = sdev(iva,isnc)
    Case (is_ano) ! - anomalies -
      vbar = zero
      vsd = sdev(iva,isnc)
    Case (is_std, is_sti) ! - standardizde anomaly and standardized index (SPI) -
      vbar = zero
      vsd = one
    Case (is_pav) ! - % of average -
      vbar = oneh
      vsd = oneh*sdev(iva,isnc)/ave(iva,isnc)
   End Select
!
! Calculate probabilities of exceedance
   Call set_progress_increment (zero, .false.)
   df = Real(nc, Kind=rp)
   xinc = (ymax - ymin)/(Real(nep, Kind=rp)*yscale)
   xmin = ymin/yscale
   tex(1) = xmin
   tex(2) = ymax/yscale
   x = xmin
   e(0) = Real(100*Count(clim(iva,1:nc,isnc) > x), Kind=rp)/df
   Select Case (igauss)
! - no transformaion -
    Case (it_non)
      f(0) = oneh*s_student_t((x - fcast(iva,ifm,isnc))/pesd_p(iva,ifm), dofr)
      c(0) = oneh - oneh*cdf_normal((x - vbar)/vsd)
      Do i = 1, nep
         Call set_cursor_waiting (1)
         x = (xmin + Real(i, Kind=rp)*xinc)
         c(i) = oneh - oneh*cdf_normal((x - vbar)/vsd)                            ! - climatological probability -
         e(i) = Real(100*Count(clim(iva,1:nc,isnc) > x), Kind=rp)/df              ! - empirical probability -
         f(i) = oneh*s_student_t((x - fcast(iva,ifm,isnc))/pesd_p(iva,ifm), dofr) ! - forecast probability -
         Call set_cursor_waiting (0)
      End Do
      Call set_cursor_waiting (1)
! - empirical distribution -
    Case (it_emp)
      If (nc < nu) Then
         rwk(1:nu) = y(iva,1:nu,isnc)
         Call insertion_sort ('a', nu, rwk)
      Else
         rwk(1:nu) = clim(iva,1:nu,isnc)
      End If
      x = cdf_empirical(rwk(1:nu), nu, x)
      c(0) = oneh - oneh*x
      x = q_normal(x)
      f(0) = oneh*s_student_t((x - fcastt(iva,ifm))/pesd_p(iva,ifm), dofr)
      Do i = 1, nep
         Call set_cursor_waiting (1)
         x = xmin + Real(i, Kind=rp)*xinc
         e(i) = Real(100*Count(clim(iva,1:nc,isnc) > x), Kind=rp)/df          ! - empirical probability -
         x = cdf_empirical(clim(iva,1:nc,isnc), nc, x)
         c(i) = oneh - oneh*x                                                 ! - climatological probability -
         x = q_normal(x)
         f(i) = oneh*s_student_t((x - fcastt(iva,ifm))/pesd_p(iva,ifm), dofr) ! - forecast probability -
         Call set_cursor_waiting (0)
      End Do
      Call set_cursor_waiting (1)
! - gamma distribution -
    Case (it_gam)
      xx(1,1) = x
      Call ustdize (istd, igauss, isnc, 1, 1, ave(iva:iva,isnc), sdev(iva:iva,isnc), xx)
      x = cdf_gamma(xx(1,1)/beta(iva,isnc), alpha(iva,isnc), pcen(iva,isnc))
      c(0) = oneh - oneh*x
      x = q_normal(x)
      f(0) = oneh*s_student_t((x - fcast(iva,ifm,isnc))/pesd_p(iva,ifm), dofr)
      Do i = 1, nep
         Call set_cursor_waiting (1)
         x = xmin + Real(i, Kind=rp)*xinc
         e(i) = Real(100*Count(clim(iva,1:nc,isnc) > x), Kind=rp)/df          ! - empirical probability -
         xx(1,1) = x
         Call ustdize (istd, igauss, isnc, 1, 1, ave(iva:iva,isnc), sdev(iva:iva,isnc), xx)
         x = cdf_gamma(xx(1,1)/beta(iva,isnc), alpha(iva,isnc), pcen(iva,isnc))
         c(i) = oneh - oneh*x                                                 ! - climatological probability -
         x = q_normal(x)
         f(i) = oneh*s_student_t((x - fcastt(iva,ifm))/pesd_p(iva,ifm), dofr) ! - forecast probability -
         Call set_cursor_waiting (0)
      End Do
      Call set_cursor_waiting (1)
   End Select
!
! Convert to non-exceedance
   If (iexc == 1) Then
      c(:) = oneh - c(:)
      e(:) = oneh - e(:)
      f(:) = oneh - f(:)
   End If
   calc = 1
!
   Return
 End Function calc_pexc
!
!
!
 Subroutine calc_errvar (iev, nx, n, nr)
!
! Calculates error variance of hindcasts
!
! Modules
   Use data_cpt_constants, Only: ie_cvh, ie_cv2, ie_ret, ie_fit, it_non
   Use settings,           Only: igauss
   Use arrays,             Only: y, yt
   Use iofiles,            Only: mya
   Use distribs,           Only: get_errvar
   Use regression,         Only: ycv, ycvt, ycv2, ycv2t, yfit, yfitt, yret, yrett, ys2
   Use season,             Only: nsnc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iev ! - error-variance option -
   Integer, Intent(In) :: nx  ! - total number of predictors -
   Integer, Intent(In) :: n   ! - total number of values -
   Integer, Intent(In) :: nr  ! - number of retroactive values -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
!
! Executable Statements
!
! Calculate fitted values
   Select Case (iev)
    Case (ie_cvh) ! - cross-validated error-variance -
      Select Case (igauss)
       Case (it_non)
         Do i = 1, mya
            ys2(i) = get_errvar(n, nx, y(i,1:n,nsnc), ycv(i,1:n,nsnc))
         End Do
       Case Default
         Do i = 1, mya
            ys2(i) = get_errvar(n, nx, yt(i,1:n), ycvt(i,1:n))
         End Do
      End Select
    Case (ie_cv2) ! - double cross-validated error-variance -
      Select Case (igauss)
       Case (it_non)
         Do i = 1, mya
            ys2(i) = get_errvar(n, nx, y(i,1:n,nsnc), ycv2(i,1:n,nsnc))
         End Do
       Case Default
         Do i = 1, mya
            ys2(i) = get_errvar(n, nx, yt(i,1:n), ycv2t(i,1:n))
         End Do
      End Select
    Case (ie_fit) ! - fitted error variance -
      Select Case (igauss)
       Case (it_non)
         Do i = 1, mya
            ys2(i) = get_errvar(n, nx, y(i,1:n,nsnc), yfit(i,1:n,nsnc))
         End Do
       Case Default
         Do i = 1, mya
            ys2(i) = get_errvar(n, nx, yt(i,1:n), yfitt(i,1:n))
         End Do
      End Select
    Case (ie_ret) ! - retroactive error-variance -
      Select Case (igauss)
       Case (it_non)
         Do i = 1, mya
            ys2(i) = get_errvar(nr, nx, y(i,n+1-nr:n,nsnc), yret(i,1:nr,nsnc))
         End Do
       Case Default
         Do i = 1, mya
            ys2(i) = get_errvar(nr, nx, yt(i,n+1-nr:n), yrett(i,1:nr))
         End Do
      End Select
   End Select
!
   Return
 End Subroutine calc_errvar
!
!
!
 Subroutine skillmask (iev, n, nr)
!
! Calculates correlations for skill-masking
!
! Modules
   Use data_cpt_constants, Only: ie_cvh, ie_cv2, ie_ret, ie_fit
   Use settings,           Only: iskmaski
   Use arrays,             Only: y
   Use iofiles,            Only: mya
   Use regression,         Only: ycv, ycv2, yfit, yret
   Use scores,             Only: corrmsk, &
                                 get_corrs
   Use season,             Only: nsnc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iev ! - error-variance option -
   Integer, Intent(In) :: n   ! - total number of values -
   Integer, Intent(In) :: nr  ! - number of retroactive values -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
!
! Executable Statements
!
! Calculate correlations for skill-masking
   Select Case (iev)
    Case (ie_cvh) ! - cross-validated error-variance -
      Do i = 1, mya
         Call get_corrs (iskmaski,  n, 1, y(i:i,     1:n,nsnc), ycv(i, 1:n,nsnc), corrmsk(i:i))
      End Do
    Case (ie_cv2) ! - double cross-validated error-variance -
      Do i = 1, mya
         Call get_corrs (iskmaski,  n, 1, y(i:i,     1:n,nsnc), ycv2(i, 1:n,nsnc), corrmsk(i:i))
      End Do
    Case (ie_fit) ! - fitted error variance -
      Do i = 1, mya
         Call get_corrs (iskmaski,  n, 1, y(i:i,     1:n,nsnc), yfit(i, 1:n,nsnc), corrmsk(i:i))
      End Do
    Case (ie_ret) ! - retroactive error-variance -
      Do i = 1, mya
         Call get_corrs (iskmaski, nr, 1, y(i:i,n+1-nr:n,nsnc), yret(i,1:nr,nsnc), corrmsk(i:i))
      End Do
   End Select
!
   Return
 End Subroutine skillmask
!
!
!
 Subroutine calc_predictions_d (ianal, nf, iskmask, lfcast, lrstd, z, f, &
            nt, xvp1, xvp2, u2, u3, t)
!
! Calculates deterministic predictions
!
! Modules
   Use data_numbers,       Only: rp, zero
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, it_non
   Use settings,           Only: igauss, ig0, ilink, imc, istd, istdr, rskmask
   Use iofiles,            Only: mxa, mya, xfile
   Use distribs,           Only: get_median, restdize
   Use pcs,                Only: bz, eofx, fsx, fsxc, ieofx, ieofy, nco, nxo, nyo, svx
   Use regression,         Only: b, b0
   Use scores,             Only: corrmsk
   Use climate,            Only: ave, clim, ncu, sdev
   Use cca,                Only: cca_predict
   Use mlr,                Only: mlr_predict
   Use pcr,                Only: pcr_predict
   Use gcm,                Only: gcm_predict
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal   ! - analysis identifier -
   Integer, Intent(In) :: iskmask ! - skill-mask -
   Integer, Intent(In) :: nf      ! - number of forecasts -
!
   Logical, Intent(In) :: lfcast ! - forecast? -
   Logical, Intent(In) :: lrstd  ! - restandardize? (if standardization is not the same as at run-time) -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(InOut) :: z ! - new predictor values -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: f ! - forecasts -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nt ! - length of training period -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: u2 ! - standardized principal component weights -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In), Optional :: u3 ! - standardized principal component weights -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: xvp1 ! - standard error from predictors; shape(1 or ny,nf) -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: xvp2 ! - standard error from predictors; shape(1 or ny,nf) -
!
! Locals
!
! Local scalars
   Integer :: i ! - location index -
!
! Executable Statements
!
! Calculate deterministic predictions
   Select Case (ianal)
    Case (ia_cca) ! - CCA -
      If (lfcast) Then
         Call cca_predict (nf, mxa, nxo, nyo, nco, ieofx, z(:,:,1), ieofy, f(:,:), &
              nt=nt, xvp=xvp1, fsx=fsx(:,:), fsxc=fsxc(:,:))
      Else
         Call cca_predict (nf, mxa, nxo, nyo, nco, ieofx, z(:,:,1), ieofy, f(:,:), &
              nt=nt, xvp=xvp1)
      End If
    Case (ia_pcr) ! - PCR -
      If (lfcast) Then
         Call pcr_predict (nf, mxa, nxo, ieofx, z(:,:,1), svx(:), eofx(:,:), bz(0:,:), f(:,:), &
              nt=nt, xvp=xvp1, fsx=fsx(:,:), t=t)
      Else
         Call pcr_predict (nf, mxa, nxo, ieofx, z(:,:,1), svx(:), eofx(:,:), bz(0:,:), f(:,:), &
              nt=nt, xvp=xvp1, t=t)
      End If
    Case (ia_mlr) ! - MLR -
      Call mlr_predict (ilink, nf, mxa, z(:,:,1), b0(:,1), b(:,:), f(:,:), &
           u=u2, xvp=xvp1, t=t)
    Case (ia_gcm) ! - GCM -
      Call gcm_predict (xfile%nms, nf, mya, ig0, imc, z(:,:,:), b0(ig0:,:), b(ig0:,:), f(:,:), &
           u=u3, xvp=xvp2, t=t)
   End Select
!
! Reassign to climatological values if skill is insufficient
   If (iskmask > 0) Then
      Select Case (igauss)
       Case (it_non) ! - no transformation -
         Do i = 1, mya
            If (corrmsk(i) < rskmask) f(i,:) = get_median(ncu(1), clim(i,1:ncu(1),1))
         End Do
       Case Default ! - transformation -
         Do i = 1, mya
            If (corrmsk(i) < rskmask) f(i,:) = zero
         End Do
      End Select
   End If
!
! Restandardize if the standardization option has changed
   If (lrstd .and. igauss == 0) Then
      Call restdize (istdr, istd, igauss, 1, mya, nf, ave(:,1), sdev(:,1), f(:,:))
   End If
!
   Return
 End Subroutine calc_predictions_d
!
!
!
 Subroutine add_persistence (n, lutrns, lustd, lrstd, b, obc, prc, f, &
            nr, clim, ft)
!
! Adds persistence component to forecasts
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: is_non
   Use time,               Only: pdate, &
                                 add_to_month, ndays
   Use settings,           Only: igauss, istd
   Use iofiles,            Only: mya
   Use distribs,           Only: gaussian, gaussian_inv, stdize, ustdize
   Use season,             Only: icsv, lsn
   Use climate,            Only: ave, sdev
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of forecasts -
!
   Logical, Intent(In) :: lrstd  ! - restandardize? -
   Logical, Intent(In) :: lustd  ! - unstandardize the predicted component? -
   Logical, Intent(In) :: lutrns ! - untransform forecasts on entry? -
!
   Type(pdate), Intent(In) :: b ! - beginning date of first persistence component -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: obc ! - observed (persisted) component (always unstandardized) -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: prc ! - predicted component -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: f ! - observed + forecast component -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nr ! - length of reference period -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(In), Optional :: clim ! - climatological data for gamma fit -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: ft ! - transformed forecasts -
!
! Locals
!
! Local scalars
   Integer :: k   ! - year counter -
   Integer :: l   ! - month counter -
   Integer :: imn ! - current month -
   Integer :: iyr ! - current year -
!
! Local arrays
   Integer, Dimension(0:1) :: lsd ! - lengths of persistence (0) and forecast (1) seasons -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real
!
! Executable Statements
!
! Unstandardise the predicted component before adding the persistence component
! - untransform predicted component -
   f(:,:) = prc(:,:)
   If (lutrns) Then
      Call gaussian_inv (igauss, is_non, 1, mya, n, f(:,:), & ! - istd == is_non so as not to standardise the untransformed forecast -
           nr=nr, ref=clim(:,:,1), ave=ave(:,1), sdev=sdev(:,1))
   Else
! - unstandardise predicted component -
      If (lustd .and. istd /= is_non) Call ustdize (istd, igauss, 1, mya, n, ave(:,1), sdev(:,1), f(:,:))
   End If
!
! Add persistence component
   Select Case (icsv)
    Case (1) ! - averages -
      Do k = 1, n
         iyr = b%iyr + k - 1
         imn = add_to_month(b%imn, -1, &
               iyr=iyr)
         lsd(:) = 0
         Do l = 1, lsn(0)
            imn = add_to_month(imn, 1, &
                  iyr=iyr)
            lsd(0) = lsd(0) + ndays(iyr, imn)
         End Do
         Do l = 1, lsn(1)
            imn = add_to_month(imn, 1, &
                  iyr=iyr)
            lsd(1) = lsd(1) + ndays(iyr, imn)
         End Do
         f(:,k) = (f(:,k)*Real(lsd(0), Kind=rp) + obc(:,k)*Real(lsd(1), Kind=rp))/Real(lsd(0)+lsd(1), Kind=rp)
      End Do
    Case (2) ! - totals -
      f(:,:) = f(:,:) + obc(:,:)
   End Select
! - restandardise -
   If (Present(ft)) Then
      ft(:,:) = f(:,:) ! - back-up the untransformed forecast with persistence added -
      Call gaussian (igauss, 2, mya, n, ft(:,:), &
           nr=nr, ref=clim(:,:,2))
   End If
   If (lrstd .and. istd /= is_non) Call stdize (istd, igauss, 2, mya, n, ave(:,2), sdev(:,2), f(:,:))
!
   Return
 End Subroutine add_persistence
!
!
!
 Subroutine calc_predictions_p (ianal, nf, nenf, nc, linterp, dofr, pesd, clim, f, fens, fps, pls, odds)
!
! Calculates probabilities, odds, and prediction intervals
!
! Modules
   Use data_numbers,       Only: rp, one
   Use data_cpt_constants, Only: ia_pfv, ng
   Use maths,              Only: interval
   Use progress_meter,     Only: lstop
   Use settings,           Only: clf, iskmask
   Use arrays,             Only: iusex, iusey, z
   Use space,              Only: get_interpolated
   Use iofiles,            Only: mya, yfile, zfile
   Use fields,             Only: imfs, yfield, zfield
   Use regression,         Only: calc_reg_probs
   Use scores,             Only: corrmsk
   Use categories,         Only: pobs, tobst, &
                                 calc_odds
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: nc    ! - length of climatological period -
   Integer, Intent(In) :: nf    ! - number of forecasts -
   Integer, Intent(In) :: nenf  ! - number of ensemble forecasts -
!
   Logical, Intent(In) :: linterp ! - interpolate predictors? -
!
   Real(Kind=rp), Intent(In) :: dofr ! - degrees of freedom for regression -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: f    ! - forecasts -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: pesd ! - prediction-error standard deviations -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: clim ! - climatological data for skill masking -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: fens ! - ensemble forecasts -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: fps  ! - forecast probabilities -
!
   Real(Kind=rp), Dimension(:,:,:,:), Intent(Out) :: odds ! - odds -
!
   Type(interval), Dimension(:,:), Intent(Out) :: pls ! - prediction limits -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: r ! - missing value -
!
! Executable Statements
!
! Calculate probabilities and prediction intervals for CCA, PCR, MLR, and GCM
   Select Case (ianal)
    Case Default
!      If (iensc == 1) Then
      If (iskmask == 0) Then   
         Call calc_reg_probs (nf, ng, mya, dofr, clf, pesd(:,:), f(:,:), tobst(:,:), fps(:,:,:), pls(:,:), &
              nenf=nenf, fens=fens(:,:,:))
      Else
         Call calc_reg_probs (nf, ng, mya, dofr, clf, pesd(:,:), f(:,:), tobst(:,:), fps(:,:,:), pls(:,:), &
              nc=nc, clim=clim(:,:), pobs=pobs(:,:), corrmsk=corrmsk(:), nenf=nenf, fens=fens(:,:,:))
      End If
!      Else
!         Call calc_ens_probs ()
!      End If
!
! Interpolate probabilities for PFV
    Case (ia_pfv)
      If (linterp) Then
         Call get_interpolated (ianal, zfile%idstr, yfile%idstr, 0, mya, iusex, iusey, zfile%nfl, yfile%nfl, nf, zfile%ngs, &
              zfield(:)%nv, yfield(1:)%nva, z(:,:,:), zfield(:)%rmiss, zfield(:)%region, yfield(1:)%region, fps(:,:,:),     &
              imfs=imfs(:))
      End If
      r = zfield(1)%rmiss
      Where (fps(:,:,:) == r) fps(:,:,:) = -one
!
   End Select
   If (lstop) Return
!
! Calculate odds and relative odds
   Call calc_odds (nf, ng, mya, pobs(:,:), fps(:,:,:), odds(:,:,:,:))
!
   Return
 End Subroutine calc_predictions_p
!
!
!
 Subroutine untransform_f (nt, nf, nenf, lb, clim, f, fpls, fens)
!
! Untransforms and standardizes deterministic predictions
!
! Modules
   Use data_numbers,   Only: rp
   Use maths,          Only: interval
   Use progress_meter, Only: update_progress_meter
   Use settings,       Only: igauss, istd
   Use iofiles,        Only: mya
   Use distribs,       Only: gaussian_inv
   Use season,         Only: isnc
   Use climate,        Only: ave, sdev
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt   ! - length of training period (full training period used for gamma-parameter estimation) -
   Integer, Intent(In) :: nf   ! - number of forecasts -
   Integer, Intent(In) :: nenf ! - number of ensemble forecasts -
   Integer, Intent(In) :: lb   ! - lower-bound -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:,lb:), Intent(In) :: clim ! - climatological data for gamma fit -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:,lb:), Intent(InOut) :: f    ! - forecasts -
   Real(Kind=rp), Dimension(:,:,  :), Intent(InOut) :: fens ! - ensemble forecasts -
!
! - input/output arrays -
   Type(interval), Dimension(:,:), Intent(InOut) :: fpls ! - prediction limits -
!
! Locals
!
! Local scalars
   Integer :: ie ! - ensemble member index -
!
! Executable Statements
!
! Untransform the predictions
! - forecasts (only the predicted component, because fcastt contains the transformed combined component) -
   Call gaussian_inv (igauss, istd,   1, mya, nf, f(:,:,1), &
        nr=nt, ref=clim(:,:,1), ave=ave(:,1), sdev=sdev(:,1))
! - ensemble forecasts -
   Do ie = 1, nenf
      Call gaussian_inv (igauss, istd, isnc, mya, nf, fens(:,:,ie), &
           nr=nt, ref=clim(:,:,isnc), ave=ave(:,1), sdev=sdev(:,isnc))
   End Do
! - prediction intervals -
   Call gaussian_inv (igauss, istd, isnc, mya, nf, fpls(:,:)%lower, &
        nr=nt, ref=clim(:,:,isnc), ave=ave(:,isnc), sdev=sdev(:,isnc))
   Call gaussian_inv (igauss, istd, isnc, mya, nf, fpls(:,:)%upper, &
        nr=nt, ref=clim(:,:,isnc), ave=ave(:,isnc), sdev=sdev(:,isnc))
   If (update_progress_meter(.false.) /= 0) Return
!
   Return
 End Subroutine untransform_f
!
!
!
 Subroutine forecast_bounds (nf, nenf, f, fpls, fens)
!
! Applies bounds to determinstic forecasts, prediction limits, and ensemble forecasts
!
! Modules
   Use data_numbers, Only: rp
   Use maths,        Only: interval
   Use settings,     Only: istd
   Use iofiles,      Only: mya
   Use distribs,     Only: zero_bound
   Use season,       Only: isnc, nsnc
   Use climate,      Only: ave, sdev
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf   ! - number of forecasts -
   Integer, Intent(In) :: nenf ! - number of ensemble forecasts -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(InOut) :: f    ! - forecasts -
   Real(Kind=rp), Dimension(:,:,:), Intent(InOut) :: fens ! - ensemble forecasts -
!
! Optional arguments
! - input/output arrays -
   Type(interval), Dimension(:,:), Intent(InOut), Optional :: fpls ! - prediction limits -
!
! Locals
!
! Local scalars
   Integer :: isn ! - season component index -
   Integer :: ie  ! - ensemble member index -
!
! Executable Statements
!
! Apply bounds
! - forecasts -
   Do isn = 1, nsnc
      Call zero_bound (istd, mya, nf, ave(:,isn), sdev(:,isn), f(:,:,isn))
   End Do
! - ensemble forecasts -
   Do ie = 1, nenf
      Call zero_bound (istd, mya, nf, ave(:,isnc), sdev(:,isnc), fens(:,:,ie))
   End Do
! - prediction intervals -
   Call zero_bound (istd, mya, nf, ave(:,isnc), sdev(:,isnc), fpls(:,:)%lower)
   Call zero_bound (istd, mya, nf, ave(:,isnc), sdev(:,isnc), fpls(:,:)%upper)
!
   Return
 End Subroutine forecast_bounds
!
!
!
 Subroutine get_fprds (nf, ifmt)
!
! Calculates target periods
!
! Modules
   Use time,     Only: Assignment(=), Operator(+), &
                       iseq, &
                       date_diff, get_prds, set_pssn, set_bdate
   Use settings, Only: set_it1
   Use arrays,   Only: cfprds, fprds
   Use iofiles,  Only: xfile, yfile, zfile
   Use season,   Only: lb, lprst, lsn, nsn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf   ! - number of forecasts -
   Integer, Intent(In) :: ifmt ! - format -
!
! Locals
!
! Local scalars
   Integer :: it1 ! - index of first date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Size
!
! Executable Statements
!
! Allocate memory
   If (Allocated(fprds)) Then
      If (Size(fprds, Dim=1) /= nf) Then
         Deallocate (fprds)
         Allocate (fprds(nf))
      End If
   Else
      Allocate (fprds(nf))
   End If
   If (Allocated(cfprds)) Then
      If (Size(cfprds, Dim=1) /= nf) Then
         Deallocate (cfprds)
         Allocate (cfprds(nf))
      End If
   Else
      Allocate (cfprds(nf))
   End If
!
! Calculate target periods
! - set first target period -
   it1 = set_it1(yfile%iseq, yfile%prd1%sdate, yfile%fdate) + date_diff(xfile%fdate, zfile%fdate, iseq)
   fprds(1)%tprd(1) = yfile%prd1 + (it1 - 1)
! - adjust for persistence component -
   If (lprst) Then
      Call set_bdate (lb, lsn, nsn, yfile%prd1%sdate, fprds(1)%tprd(1)%sdate, fprds(1)%tprd(2)%sdate, &
                      cprdbs=cfprds(1))
      Call set_pssn (fprds(1), &
           ls0=lsn(0))
   Else
      fprds(1)%tprd(0) = 0
      fprds(1)%tprd(2) = fprds(1)%tprd(1)
   End if
! - set subsequent target periods -
   Call get_prds (nf, ifmt, fprds(:), cfprds(:))
!
   Return
 End Subroutine get_fprds
!
!
!
 Function update_fprds() &
          Result (update)
!
! Updates forecast periods
!
! Modules
   Use screen,   Only: window_update
   Use settings, Only: nf
   Use arrays,   Only: cfprds
   Use iofiles,  Only: pfile, yfile, &
                       set_pfile
   Use season,   Only: lsn
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Update season selection indicator
   If (yfile%idstr > 0) Then
      If (.not.pfile%cfile%lset) update = set_pfile(nf, lsn(0))
      Call get_fprds(1, 2)
      Call window_update (cfprds(1)%cssn(2))
   End If
   update = 2
!
   Return
 End Function update_fprds
!
!
!
 Subroutine proj_read_predict (iin, ianal, igauss, ihind, ifail)
!
! Reads prediction settings from project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  4 Problem allocating memory
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ir_bin
   Use menus,              Only: set_menu_greying
   Use settings,           Only: iregr, nf
   Use arrays,             Only: tf, z, ziny
   Use pcs,                Only: fsx, fsxc
   Use regression,         Only: fcast, fcastt, pls_p, pesd_p, u, xvp
   Use missing,            Only: kfuse
   Use categories,         Only: fps_p, odds_p
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: ianal  ! - analysis identifier -
   Integer, Intent(In) :: igauss ! - transform Y data flag -
   Integer, Intent(In) :: ihind  ! - hindcasts flag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read prediction settings
   Read (Unit=iin, Err=1, End=2) ifc, ifcast
   ifm = 1
!
! Read prediction results
   If (ifc > 0) Then
      If (init_predict(ianal, ihind, .true.) /= 0) GoTo 4
      Read (Unit=iin, Err=1, End=2) z
      If (ihind == 1) Then
         If (iregr == ir_bin) Read (Unit=iin, Err=1, End=2) tf
         Read (Unit=iin, Err=1, End=2) fcast
         If (igauss > 0) Read (Unit=iin, Err=1, End=2) fcastt
         Read (Unit=iin, Err=1, End=2) pesd_p(:,:)
         Read (Unit=iin, Err=1, End=2) pls_p
      End If
      Read (Unit=iin, Err=1, End=2) fps_p
      Read (Unit=iin, Err=1, End=2) odds_p(:,:,:,2)
      Read (Unit=iin, Err=1, End=2) odds_p(:,:,:,1)
      Select Case (ianal)
       Case (ia_cca)
         Read (Unit=iin, Err=1, End=2) xvp
         Read (Unit=iin, Err=1, End=2) fsx
         Read (Unit=iin, Err=1, End=2) fsxc
       Case (ia_pcr)
         Read (Unit=iin, Err=1, End=2) xvp
         Read (Unit=iin, Err=1, End=2) fsx
       Case (ia_mlr)
         Read (Unit=iin, Err=1, End=2) xvp
         Read (Unit=iin, Err=1, End=2) u
       Case (ia_gcm)
         Read (Unit=iin, Err=1, End=2) xvp
         Read (Unit=iin, Err=1, End=2) u
         Read (Unit=iin, Err=1, End=2) ziny
       Case (ia_pfv)
         Read (Unit=iin, Err=1, End=2) ziny
      End Select
      Read (Unit=iin, Err=1, End=2) kfuse
      Call get_fprds (nf, 4)
   End If
   Call set_menu_greying (ifc=ifc)
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
! - problem allocating memory -
4  ifail = 4
   Return
!
 End Subroutine proj_read_predict
!
!
!
 Subroutine proj_write_predict (iout, ianal, igauss, ihind, ifail)
!
! Writes prediction settings to project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem writing to file
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ir_bin
   Use settings,           Only: iregr
   Use arrays,             Only: tf, z, ziny
   Use pcs,                Only: fsx, fsxc
   Use regression,         Only: fcast, fcastt, pls_p, pesd_p, u, xvp
   Use missing,            Only: kfuse
   Use categories,         Only: fps_p, odds_p
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout   ! - output file unit number -
   Integer, Intent(In) :: ianal  ! - analysis identifier -
   Integer, Intent(In) :: igauss ! - transform Y data flag -
   Integer, Intent(In) :: ihind  ! - hindcasts flag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write prediction settings
   Write (Unit=iout, Err=1) ifc, ifcast
   If (ifc > 0) Then
      Write (Unit=iout, Err=1) z
      If (ihind == 1) Then
         If (iregr == ir_bin) Write (Unit=iout, Err=1) tf
         Write (Unit=iout, Err=1) fcast
         If (igauss > 0) Write (Unit=iout, Err=1) fcastt
         Write (Unit=iout, Err=1) pesd_p(:,:)
         Write (Unit=iout, Err=1) pls_p
      End If
      Write (Unit=iout, Err=1) fps_p
      Write (Unit=iout, Err=1) odds_p(:,:,:,2)
      Write (Unit=iout, Err=1) odds_p(:,:,:,1)
      Select Case (ianal)
       Case (ia_cca)
         Write (Unit=iout, Err=1) xvp
         Write (Unit=iout, Err=1) fsx
         Write (Unit=iout, Err=1) fsxc
       Case (ia_pcr)
         Write (Unit=iout, Err=1) xvp
         Write (Unit=iout, Err=1) fsx
       Case (ia_mlr)
         Write (Unit=iout, Err=1) xvp
         Write (Unit=iout, Err=1) u
       Case (ia_gcm)
         Write (Unit=iout, Err=1) xvp
         Write (Unit=iout, Err=1) u
         Write (Unit=iout, Err=1) ziny
       Case (ia_pfv)
         Write (Unit=iout, Err=1) ziny
      End Select
      Write (Unit=iout, Err=1) kfuse
   End If
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = close_predict()
   ifail = 1
   Return
!
 End Subroutine proj_write_predict
!
!
!
 Subroutine parameter_write_predict (iout, ifail)
!
! Writes program settings to parameter file
!
! Modules
   Use labels,         Only: ca_evs, cg_conflev
   Use menus,          Only: mn_opts_fcast
   Use parameter_file, Only: output_parameter
   Use settings,       Only: clf, iev, iexc, ifpr, iodds, iprecf, nenf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output-file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Character :: cyn ! - Y/N -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Write project settings
   If (mn_opts_fcast%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_fcast%l_lab%c), i=mn_opts_fcast%id) /= 0) GoTo 1
      If (output_parameter(iout, & ! - prediction confidence interval -
                           cdesc=Trim(cg_conflev), r=clf) /= 0) GoTo 1
      If (output_parameter(iout, & ! - error variance -
                           cdesc=Trim(ca_evs(iev)), i=iev) /= 0) GoTo 1
      If (output_parameter(iout, & ! - error variance -
                           cdesc='Number of ensemble forecasts', i=nenf) /= 0) GoTo 1
      If (iodds == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If 
      If (output_parameter(iout, & ! - odds? -
                           cdesc='Show as odds relative to climatology?', c=cyn) /= 0) GoTo 1
      If (iexc == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If 
      If (output_parameter(iout, & ! - non-exceedance probabilities? -
                           cdesc='Show as non-exceedance probabilities?', c=cyn) /= 0) GoTo 1
      If (output_parameter(iout, & ! - precision -
                           cdesc='Number of decimal places', i=iprecf) /= 0) GoTo 1
      If (output_parameter(iout, & ! - forecast probability rounding -
                           cdesc='Forecast probability rounding', i=ifpr) /= 0) GoTo 1
   End If
!
   ifail = 0
   Return
!
! Errors in parameter file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine parameter_write_predict
!
!
!
 Function close_predict() &
          Result (fclose)
!
! Frees memory allocated for prediction
!
! Modules
   Use arrays,     Only: cfprds, fprds, tf, xc, z, ziny, zyp
   Use pcs,        Only: fsx, fsxc
   Use regression, Only: fcast, fcastt, ens_p, pls_p, pesd_p, xvp
   Use missing,    Only: kfuse
   Use categories, Only: fps_p, odds_p
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
   fclose = close_pexc()
   fclose = close_hps()
   If (Allocated(cfprds)) Deallocate (cfprds)
   If (Allocated(fprds))  Deallocate (fprds)
   If (Allocated(zyp))    Deallocate (zyp)
   If (Allocated(kfuse))  Deallocate (kfuse)
   If (Allocated(ziny))   Deallocate (ziny)
   If (Allocated(fsxc))   Deallocate (fsxc)
   If (Allocated(fsx))    Deallocate (fsx)
   If (Allocated(odds_p)) Deallocate (odds_p)
   If (Allocated(fps_p))  Deallocate (fps_p)
   If (Allocated(pls_p))  Deallocate (pls_p)
   If (Allocated(xvp))    Deallocate (xvp)
   If (Allocated(pesd_p)) Deallocate (pesd_p)
   If (Allocated(fcastt)) Deallocate (fcastt)
   If (Allocated(ens_p))  Deallocate (ens_p)
   If (Allocated(fcast))  Deallocate (fcast)
   If (Allocated(tf))     Deallocate (tf)
   If (Allocated(xc))     Deallocate (xc)
   If (Allocated(z))      Deallocate (z)
!
!  Reset flags
   Call set_ifcast (ifc=0)
   fclose = 0
!
   Return
 End Function close_predict
!
!
!
 Function close_pexc() &
          Result (fclose)
!
! Frees memory allocated for probabilities of exceedence
!
! Modules
   Use arrays,     Only: tex
   Use regression, Only: c, e, f
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
   If (Allocated(tex)) Deallocate (tex)
   If (Allocated(c))   Deallocate (c)
   If (Allocated(e))   Deallocate (e)
   If (Allocated(f))   Deallocate (f)
   fclose = 0
!
   Return
 End Function close_pexc
!
!
!
 Function close_hps() &
          Result (fclose)
!
! Frees memory allocated for highest probability mapping
!
! Modules
   Use categories, Only: hps
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
   If (Allocated(hps)) Deallocate (hps)
   fclose = 0
!
   Return
 End Function close_hps
End Module prediction
