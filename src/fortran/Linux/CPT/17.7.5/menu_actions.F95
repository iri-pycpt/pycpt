! Author: Simon Mason
Module menu_actions
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: actions_calculate, actions_reset, parameter_write_action, reset, reset_mem
#if GUI == 1
   Public :: check_reset
#endif
!
Contains
!
!
 Function actions_calculate() &
          Result (action)
!
! Reads data and fits selected model
!
! Modules
#if GUI == 1
   Use clrwin$,             Only: clear_window$
#endif
   Use data_numbers,        Only: rp, zero, one
   Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ir_bin, ir_ols, it_non, mnt, ng, nts
   Use data_io_constants,   Only: io_com, lprd
   Use data_time_constants, Only: isq_dy, isq_mn, isq_sn, isq_so, nmn
   Use maths,               Only: set_data_bounds
   Use labels,              Only: ca_atypes_a, ca_regrs, cg_to_l, &
                                  l_climper
   Use time,                Only: pprd, &
                                  Operator(+), Operator(-), Operator(>), Operator(<), &
                                  iseq, lslim, nss, &
                                  date_diff, get_cdate, get_cssn, get_prds, ndays, nyears, reset_iseq, set_pssn
   Use screen,              Only: window_update
   Use gui,                 Only: set_cursor_waiting
#if GUI == 1
   Use gui,                 Only: ih_con
#endif
   Use progress_meter,      Only: lstop, prog, &
                                  end_progress_meter, set_progress_increment, start_progress_meter, stop_progress_meter, &
                                  update_progress_meter
   Use errors,              Only: cpt_error
   Use settings,            Only: icv2, ieav, igauss, igto, iregr, iretro, istd, istdr, izero, lcw, lzb, &
                                  nt, ntr, nu, nur, nyr, &
                                  check_n, get_cv, get_retro, record_results, set_nused
   Use arrays,              Only: cxprds, cyprds, t, vcopy, vmax, vmin, x, xin, xprds, y, ybk, yin, yprds
   Use space,               Only: cstndy
   Use iofiles,             Only: mfile, mxa, mya, xfile, yfile
#if GUI == 1
   Use fields,              Only: iffx, iffy, ilimx0, ilimx1, ilimx2, ilimy0, ilimy1, ilimy2, &
                                  set_ilims
#endif
   Use fields,              Only: xfield, yfield
   Use distribs,            Only: stdize
   Use pcs,                 Only: lcde, &
                                  check_pcs, get_cca_settings, get_eofx_settings, get_eofy_settings, set_scree
   Use season,              Only: icsv, isnc, lb, lensn, lprst, lsn, lssr, nsn, nsnc, &
                                  available_ssn
   Use missing,             Only: kcuse, kincl, ktuse, &
                                  close_missing, compress, replace_missing_xy
   Use categories,          Only: iobs_r, pthr, fps_r, tobs, tobst, &
                                  calc_thresholds, close_categories, init_thresholds, set_cv_categories, set_c2_categories, &
                                  set_ra_categories
   Use climate,             Only: ave, clim, climate_per, icex, itrain1, itrain2, nc, ncu, ntc, sdev, &
                                  calc_climates, check_climates, close_climate, get_clim_limits, init_climate, set_climate, &
                                  valid_climate
   Use ensemble,            Only: ensemble_ave
   Use cpt_input,           Only: get_data
   Use prediction,          Only: ifc, &
                                  add_persistence, set_ifcast
   Use analysis,            Only: ianal, icalc, &
                                  analysis_flags, close_analysis, close_calc, close_cv2, init_cv2, init_cv2results, init_results, &
                                  perform_analysis
   Use pfv,                 Only: perform_pfv
   Use cpt_view,            Only: grey_menu_flags
   Use forecast,            Only: get_cv2_forecast
   Use verif,               Only: irv, &
                                  n_verif
!
! Function result
   Integer :: action
!
! Locals
!
! Local scalars
   Integer :: j      ! - location index -
   Integer :: k      ! - time index -
   Integer :: kk     ! - time index -
   Integer :: iarg1  ! - first integer error argument -
   Integer :: icv2i  ! - double cross-validation index -
   Integer :: ierr   ! - error parameter -
   Integer :: isn    ! - season index -
   Integer :: is0    ! - zeroth season -
   Integer :: it1    ! - index of first date of training period (relative to combined climate and training dataset) -
   Integer :: it2    ! - index of last date of training period (relative to combined climate and training dataset) -
   Integer :: ibound ! - data-bounds indicator -
   Integer :: nact   ! - number of actions -
   Integer :: nems   ! - total number of ensemble members -
   Integer :: nn     ! - number of cases -
   Integer :: ntcy   ! - number of years in climatological and training periods -
   Integer :: nuy    ! - number of available years in training period -
   Integer :: ifail  ! - error indicator -
!
   Logical :: kinclbk  ! - backup include current training case? -
   Logical :: lcsv     ! - calculate seasonal totals? -
   Logical :: lpfv     ! - PFV? -
   Logical :: lslim_bk ! - seasonal limits backup? -
!
   Character(Len=  32) ::  cproc ! - error procedure -
   Character(Len=  64) ::  carg1 ! - first character error argument -
   Character(Len=  64) ::  carg2 ! - second character error argument -
   Character(Len=  64) ::  carg3 ! - third character error argument -
   Character(Len=  64) ::  carg4 ! - fourth character error argument -
   Character(Len=lprd) :: cv2dat ! - double cross-validation date -
!
   Type(pprd) :: error_prd ! - period of erroneous data -
   Type(pprd) :: yprd      ! - period of Y-data -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Max, MaxVal, Real, Sum, Trim
#if FTN95 == 1
   Intrinsic Size
#endif
!
! Executable Statements
!
! Initialise error flags
   action = 1
   iarg1 = 0
   carg1 = ' '
   carg2 = ' '
   carg3 = ' '
   carg4 = ' '
!
! Check for invalid length of training period
! - check for too few -
   lpfv = (ianal == ia_pfv)
   ifail = check_n(nt, lcw, .true., ianal, icv2, iretro, ierr)
   If (ifail /= 0) Then
      If (ifail /= 2 .and. .not.lpfv) Then
         Call window_update (lcw)
         Call cpt_error ('check_n', .false., ifail, &
              i_arg1=ierr)
         Return
      End If
   End If
!
! Check for cancelled retroactive calculations
   Select Case (iretro)
    Case (0) ! - cross-validation -
      action = get_cv()
    Case (1) ! - retroactive -
      If (.not.lpfv) Then
         action = get_retro()
         If (iretro == -1) Then
            iretro = 0
            action = 1
            Return
         End If
      End If
   End Select
   action = 1
!
! Initialise analysis
! - switch on hour glass -
   Call set_cursor_waiting (1)
! - initialise calculation flags -
   Call analysis_flags ('on')
   ifc = 0
!
! Check climatologies
! - set climatological probabilities for PFV -
   If (lpfv) Then
      If (Sum(xfield(1)%prob(:)) /= zero) Then
         pthr(1) = xfield(1)%prob(1)
         Do j = 2, nts
            pthr(j) = pthr(j-1) + xfield(1)%prob(j)
         End Do
      Else
         xfield(1)%prob(1) = pthr(1)
         Do j = 2, nts
            xfield(1)%prob(j) = pthr(j) - pthr(j-1)
         End Do
         xfield(1)%prob(ng) = one - pthr(nts)
      End If
   End If
! - set initial climatological period settings -
   Call check_climates (yfile, mfile, lsn(0), lpfv)
!
! Read data
   Call start_progress_meter ('Reading data', .false.)
   ifail = get_data(icsv, icex, ianal, .true.)
   If (ifail /= 0 .or. lstop) GoTo 3
   Call end_progress_meter (.false., .false.)
!
! Adjust number of cases for seasonal sequencing
   ntcy = nyears(ntc, .false.)
!
! Initialise binomial coefficients
   If (iregr == ir_bin) Then
      yprd = yfile%prd1 + (yfile%it1 - 1)
      If (yprd%sdate%idy == 0) Then
         yprd%sdate%idy = 1
         yprd%edate%idy = ndays(yprd%edate%iyr, yprd%edate%imn)
      End If
      lslim_bk = lslim
      lslim = .false.
      Do k = 1, nt
         yprd = yprd + 1
         t(k) = date_diff(yprd%sdate, yprd%edate, isq_dy) + 1
      End Do
      lslim = lslim_bk
!
! Check for data bounds
! - set bounds -
      Call set_data_bounds (igto, igauss, iregr, izero, ibound, vmin, vmax, &
           t=MaxVal(t(:)))
   Else
      Call set_data_bounds (igto, igauss, iregr, izero, ibound, vmin, vmax)
   End if
! - search for invalid values if data bounds are on -
   If (ibound /= 0) Then
      ifail = check_bounds(ibound, ntcy)
      If (ifail /= 0) GoTo 2
   End If
!
! Identify training period
   Select Case (icex)
    Case (0, 2, -2)
      itrain1 = 1
    Case (1, 3) ! - climatological period starts before training period -
      itrain1 = date_diff(climate_per%d1, yfile%bdate, yfile%iseq) + 1
    Case (-1) ! - climatological period is before training period (gap) -
      itrain1 = nc + 1
   End Select
!
! Initialise if running double cross-validation
   If (icv2 > 0) Then
      Call init_cv2 (ifail)
      If (ifail /= 0) Then
         ifail = 5
         cproc = 'actions_calculate'
         carg1 = ca_atypes_a(ia_pfv)
         GoTo 2
      End If
   End If
   icv2i = 0
1  nyr = nyears(nt, .false.)
   If (icv2 > 0) Then
      x(:,:,:) = xin(:,:,:)
#if FTN95 == 1
      Deallocate (y)
      Allocate (y(Size(yin,Dim=1),Size(yin,Dim=2),Size(yin,Dim=3)))
      y(:,:,:) = yin(:,:,:)
#else
      If (icsv > 0) Deallocate (y) ! - reassign Y if seasonal totals have been calculated -
      y = yin
#endif
      Print *, ' '
      If (icv2i < nt) Then ! - advance to next double cross-validation step -
         icv2i = icv2i + 1
          cv2dat = Trim(get_cdate(yfile%bdate + (icv2i - 1), 1, io_com))
         Print *, 'Double cross-validation for ', Trim(cv2dat), ' ...'
!      If (kincl(icv2i)) GoTo ?
         kinclbk = kincl(icv2i)
         kincl(icv2i) = .false.
      Else ! - after double cross-validation, proceed to regular cross-validation -
         Print *, 'Regular cross-validation ...'
         icv2i = 0
         ifail = close_cv2()
      End If
      ifail = valid_climate() ! - reset climate period to include all data -
   End If
!
! Replace missing values and check for missing variables
   Call start_progress_meter ('Checking for missing values', .false.)
   Call available_ssn (mfile%iseq, ntc)
   lcsv = (icsv > 0 .and. lensn > 1)
   ifail = replace_missing_xy(ianal, icex, nt, ntc, ntcy, itrain1, nsn, lensn,  1, lssr(:), &
                              .true., .not.lcsv, lcsv, .false., nu)
   Deallocate (lssr)
   If (ifail /= 0 .or. lstop) GoTo 3
!
! Calculate seasonal values
   If (lcsv) Then
      Call calculate_seasonals (ntcy)
! - repeat missing value estimation for seasonal values -
      Call start_progress_meter ('Checking for missing seasonal values', .false.)
      ifail = replace_missing_xy(ianal, icex, nt, ntc, ntcy, itrain1, nsn,     1, lb, lssr(1:nsn), &
                                 .false., .true., .false., lprst, nu)
      Deallocate (lssr)
      If (ifail /= 0 .or. lstop) GoTo 3
   End If
!
! Identify number of non-missing years
   Select Case (iseq)
    Case Default
      nyr = Count(kcuse(itrain1:itrain1+nt-1))
    Case (isq_sn, isq_so) ! - seasonal data have not yet been compressed -
      nyr = nyears(nt, .true.)
      nur = nt
      nu = nt
   End Select
!
! Double check for invalid length of training period given missing values
   If (nu < nt) Then
      ifail = check_n(nu, lcw, .false., ianal, icv2, iretro, ierr)
      Select Case (ifail)
       Case (0)
         Continue
       Case Default
         If (lpfv .and. ifail == 2) Then
            Continue
         Else
            ifail = 5
            cproc = 'check_n'
            iarg1 = ierr
            GoTo 2
         End If
      End Select
   End If
   ifail = set_nused((iseq /= isq_sn .and. iseq /= isq_so), kcuse(itrain1:))
   If (ifail /= 0) Then
      cproc = 'set_nused'
      iarg1 = mnt - 1
      GoTo 2
   End If
   Call end_progress_meter (.false., .false.)
!
! Set dates
! - X dates -
   xprds(1) = xfile%prd1 + (xfile%it1 - 1)
   Call get_prds (nu, 2, xprds(:), cxprds(:))
! - Y dates -
   yprds(1)%tprd(1) = yfile%prd1 + (yfile%it1 - 1)
   Call set_pssn (yprds(1), lsn(0))
   Call get_prds (nu, 2, yprds(:), cyprds(:))
!
! Calculate ensemble average
   If (ieav == 1 .and. .not.lpfv) Then
      If (xfile%nem > 1 .or. xfile%nms > 1) Then ! - average ensemble members -
         nems = Max(1,xfile%nem)*Max(1,xfile%nms)
         Call ensemble_ave (xfile%nfs/nems, xfile%nls, nu, xfield(:)%nv, nems, x(:,:,1))
      End If
   End If
!
! Initialise progress meter
   Call start_progress_meter ('Calculating climatologies and thresholds', .false.)
   nact = 1              ! - initial checks -
   nact = nact + mya + 1 ! - calc_climate -
   nact = nact + 1       ! - standardize -
   nact = nact + mya     ! - set_percentiles -
   Select Case (igauss)  ! - set_thresholds -
    Case (it_non)
      nact = nact + mya
    Case Default
      nact = nact + mya + 1
   End Select
   Call set_progress_increment (one/Real(nact+1, Kind=rp), .false.)
!
! Set climatology
! - initialise -
   Call init_climate (lb, nsnc, ifail)
   If (lstop) GoTo 3
   If (ifail /= 0) Then
      cproc = 'init_climate'
      GoTo 2
   End If
! - set thresholds -
   If (init_thresholds(nsn) /= 0) Then
      cproc = 'init_thresholds'
      GoTo 2
   End If
! - set climatological period -
   Call set_climate (1, lsn(0))
   Print *, Trim(l_climper%c)//': '//Trim(climate_per%clim1)//' '//Trim(cg_to_l)//' '//Trim(climate_per%clim2)
   If (update_progress_meter(.false.) /= 0) GoTo 3
   nuy = nyears(nu, .false.)
! - add persistence and predicted components of season -
   If (lprst) Then
      Select Case (icex)
       Case (0)
         nn = nuy
       Case Default
         nn = ncu(0)
      End Select
      Call add_persistence (nn, .false., .false., .false., yfile%bdate, y(1:mya,1:nn,0), y(1:mya,1:nn,1), y(1:mya,1:nn,2))
   End If
! - backup separate components so that repeated re-standardization is unnecessary -
   Allocate (ybk(mya,nuy,lb:nsnc), Stat=ifail)
   If (ifail /= 0) Then
      ifail = 4
      cproc = 'actions_calculate'
      carg1 = 'performing analysis'
      GoTo 2
   End If
   ybk(:,:,:) = y(1:mya,1:nuy,:)
! - calculate climatologies -
   Call calc_climates (.false., mya, lb, nsnc, y(1:mya,:,lb:), ncu(lb:), clim(1:mya,:,lb:), ave(:,lb:), sdev(:,lb:), ifail)
   If (lstop) GoTo 3
   If (ifail /= 0) GoTo 3
   If (lprst) ncu(2) = ncu(1)
!
! Isolate training period if climatological period extends outside it
   If (icex /= 0) Then
      Allocate (vcopy(mya,nuy,lb:nsnc), Stat=ifail)
      If (ifail /= 0) Then
         ifail = 4
         cproc = 'actions_calculate'
         carg1 = 'climatological period outside of training period'
         GoTo 2
      End If
      vcopy(:,:,:) = y(1:mya,itrain1:itrain2,lb:nsnc)
      Deallocate (y)
      Allocate (y(mya,nuy,lb:nsnc), Stat=ifail)
      If (ifail /= 0) Then
         ifail = 4
         cproc = 'actions_calculate'
         carg1 = 'climatological period outside of training period'
         GoTo 2
      End If
      y(:,:,:) = vcopy(:,:,:)
      Deallocate (vcopy)
   End If
   Call get_clim_limits (icex, isq_mn, &
        itrain1=it1)
   it2 = it1 + nt - 1
   Allocate (ktuse(nt))
   ktuse(:) = kcuse(it1:it2)
!
! Standardize
   If (.not.lpfv) Then ! - for PFV, istd = 0 so standardization is unnecessary -
      Do isn = 1, nsnc ! - start at 1 to leave combined season unstandardized -
         Call stdize (istd, igauss, isn, mya, nuy, ave(:,isn), sdev(:,isn), y(1:mya,1:nuy,isn))              ! - training data -
         Call stdize (istd, igauss, isn, mya, ncu(isn), ave(:,isn), sdev(:,isn), clim(1:mya,1:ncu(isn),isn)) ! - climatological data -
      End Do
      If (lprst) Call stdize (istd, igauss, 0, mya, ncu(0), ave(:,0), sdev(:,0), clim(1:mya,1:ncu(0),0)) ! - climatological data -
      If (update_progress_meter(.false.) /= 0) GoTo 3
!
! Calculate thresholds
      Call calc_thresholds (igauss, mya, 1, ncu(isnc:), clim(:,:,isnc:isnc), tobs, tobst, ifail)
   Else
      Call calc_thresholds (igauss, mya, nsn, ncu(1:), clim(:,:,1:), tobs, tobst, ifail)
   End If
   If (ifail /= 0) GoTo 3
   If (update_progress_meter(.false.) /= 0) GoTo 3
!
! Temporarily remove persistence component
   If (lprst) Then
      y(1:mya,1:nuy,1) = ybk(:,:,1)
      Call stdize (istd, igauss, 1, mya, nuy, ave(:,1), sdev(:,1), y(1:mya,1:nuy,1))
   End If
   If (update_progress_meter(.false.) /= 0) GoTo 3
!
! Check validity of EOF and CCA settings
   Call start_progress_meter ('Beginning analysis',.false.)
   If (.not.lcde) Then
      ifail = check_pcs(ianal)
      If (ifail == -1) GoTo 3
   Else ! - calculate mode numbers -
      Select Case (ianal)
       Case (ia_cca) ! - CCA -
         ifail = get_eofx_settings()
         ifail = get_eofy_settings()
         ifail = get_cca_settings()
       Case (ia_pcr) ! - PCR -
         ifail = get_eofx_settings()
      End Select
      ifail = 0
   End If
!
! Perform analysis
   istdr = istd
   lzb = (igauss == it_non .and. izero == 1 .and. iregr == ir_ols)
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr, ia_gcm) ! - CCA, PCR, MLR, GCM -
      ifail = perform_analysis()
    Case (ia_pfv) ! - PFV -
      ifail = perform_pfv(ierr)
   End Select
   If (lstop) GoTo 3
   Select Case (ifail)
    Case (0)
      Continue
    Case (-1)
      lstop = .true.
      GoTo 3
    Case (11:)
      ifail = ifail - 10
      cproc = 'match_fields'
      carg1 = 'PFV'
      carg2 = yfield(ierr)%var
      carg3 = yfield(ierr)%cssn%cssn(1)
      iarg1 = ierr
      GoTo 2
    Case Default
      cproc = 'perform_'//ca_atypes_a(ianal)
      carg1 = ca_atypes_a(ianal)
      GoTo 2
   End Select
!
! Add predicted component to persistence season
   If (lprst) Then
      yfield(:)%tssn%tprd(2)%sdate = yfield(:)%tssn%tprd(0)%edate
      yfield(:)%tssn%tprd(2)%edate = yfield(:)%tssn%tprd(1)%edate
      yfield(1)%cssn%cssn(0) = get_cssn(yfield(1)%tssn%tprd(0), 1, [1])
      yfield(1)%cssn%cssn(2) = get_cssn(yfield(1)%tssn%tprd(2), 1, [1])
   End If
!
! Allocate additional memory for results
   Call init_results (ianal, Max(MaxVal(xfield(:)%nlt), MaxVal(yfield(:)%nlt)), Max(MaxVal(xfield(:)%nlg), MaxVal(yfield(:)%nlg)), &
        ifail)
   If (ifail /= 0) Then
      ifail = 1
      cproc = 'perform_'//ca_atypes_a(ianal)
      carg1 = ca_atypes_a(ianal)
      GoTo 2
   End If
!
! Determine forecast categories
   Print *, 'Identifying categories ...'
! - cross-validated categories - 
   If (.not.lpfv) Then
      If (iretro == 1) Then
         Call set_progress_increment ((one - prog)*Real(nu, Kind=rp)/Real(mya*(nu + nur), Kind=rp),.false.)
         is0 = 0
      Else
         Call set_progress_increment ((one - prog)/Real(mya, Kind=rp),.false.)
      End If
      If (set_cv_categories() /= 0) GoTo 3
   End If
! - retroactive categories - 
   If (iretro == 1) Then
      Call set_progress_increment ((one - prog)/Real(mya, Kind=rp),.false.)
      If (.not.lpfv) Then
         If (set_ra_categories(ianal, 1, 0, ncu(isnc:isnc), clim(:,:,isnc:isnc)) /= 0) GoTo 3
      Else
         If (nsn /= nmn) Then
            is0 = 0
         Else
            is0 = xfile%fdate%imn - 1
         End If
         If (set_ra_categories(ianal, nsn, is0, ncu(:), clim(:,:,:)) /= 0) GoTo 3
      End If
   End If
   If (lstop) GoTo 3
!
! Restructure seasonal data
! - compress seasonal data (not previously compressed) -
   Select Case (iseq)
    Case (isq_sn, isq_so)
      Call compress (nt, kcuse(:), nur, &
           ia=iobs_r(1:mya,:), aa=fps_r(1:mya,:,:))
! - reduce seasons and year to one time dimension -
      Allocate (vcopy(mya,nuy,nsn), Stat=ifail)
      If (ifail /= 0) Then
         ifail = 4
         cproc = 'actions_calculate'
         carg1 = ca_atypes_a(ia_pfv)
         GoTo 2
      End If
      vcopy(:,:,:) = y(1:mya,1:nuy,:)
      Deallocate (y)
      Allocate (y(mya,nss*nuy,1), Stat=ifail)
      kk = 0
      Do k = 1, nuy
         Do isn = 1, nss
            kk = kk + 1
            y(:,kk,1) = vcopy(:,k,isn)
         End Do
      End Do
      Deallocate (vcopy)
   End Select
!
! Permit forecasts if Z-data file is set
   Call set_ifcast ()
!
! Predict omitted year if running double cross-validation
   If (icv2i > 0) Then
      If (icv2i == 1) Then
         Call init_cv2results (ifail)
         If (ifail /= 0) Then
            ifail = 5
            cproc = 'actions_calculate'
            carg1 = ca_atypes_a(ia_pfv)
            GoTo 2
         End If
      End If
      kincl(icv2i) = kinclbk
      ifail = get_cv2_forecast (icv2i)
! - clear memory before repeating analysis for next time step -
      ifail = close_analysis()
      ifail = close_categories()
      ifail = close_climate()
      ifail = close_missing()
      GoTo 1
   End If
! - set double cross-validated categories - 
   If (icv2 /= 0) Then
      Call set_progress_increment ((one - prog)/Real(mya, Kind=rp),.false.)
      If (set_c2_categories() /= 0) GoTo 3
   End If
!
! Permit scree plots if number of modes is more than one
   Call set_scree ()
!
#if GUI == 1
! Initialise variable limits
   Call set_ilims (xfield(:)%nv, xfield(:)%nva, iffx, ilimx0, ilimx1, ilimx2)
   Call set_ilims (yfield(1:)%nv, yfield(1:)%nva, iffy, ilimy0, ilimy1, ilimy2)
!
#endif
! End analysis
   irv = iretro
   Call window_update (irv)
   Call n_verif ()
! - set length of verification period -
   Select Case (iseq)
    Case (isq_sn, isq_so)
      ntr = nt
   End Select
! - switch off calculation flags -
   Call analysis_flags ('end')
   Call window_update (icalc)
! - update grey menu flags -
   Call grey_menu_flags (icalc, xfile%idstr, yfile%idstr)
! - record change -
   ifail = record_results()
! - end progress meter -
   Call end_progress_meter (.true., .true.)
   ifail = 0
   action = 2
   Return
!
! Terminate analysis
2  Call cpt_error (Trim(cproc), .false., ifail, &
        c_arg1=Trim(carg1), c_arg2=Trim(carg2), c_arg3=Trim(carg3), c_arg4=Trim(carg4), i_arg1=iarg1)
3  If (lstop) Then
      mxa = 0
      mya = 0
      Call window_update (mxa)
      Call window_update (mya)
   End If
   Call analysis_flags ('off')
   ifail = close_calc() ! - clear memory -
#if GUI == 1
   Call clear_window$ (ih_con)
#endif
   Write (Unit=*, Fmt=*)
   Call stop_progress_meter (.true.)
!
   Return
!
 Contains
!
!
 Function check_bounds(ibound, ntcy) &
          Result (check)
!
! Checks whether data are within bounds
!
! Modules
   Use data_cpt_constants, Only: id_grd
   Use data_io_constants,  Only: io_com
   Use maths,              Only: check_data_bounds
!
! Function result
   Integer :: check
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ibound ! - data-bounds indicator -
   Integer, Intent(In) :: ntcy   ! - number of years in climatological and training periods -
!
! Locals
!
! Local scalars
   Integer :: i1   ! - first dimension index -
   Integer :: i3   ! - third dimension index -
   Integer :: j    ! - location index -
   Integer :: l    ! - field index -
   Integer :: iloc ! - location of first negative value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Trim, Ubound
!
! Executable Statements
!
! Check whether data are within bounds
   Do i3 = 1, Ubound(y(:,:,:), Dim=3)
      i1 = 0
      Do l = 1, yfile%nfl
         Do j = 1, yfield(l)%nv
            i1 = i1 + 1
            If (vmax > vmin) Then
               Call check_data_bounds (ntcy, y(i1,:,i3), iloc, &
                    vmin=vmin, vmax=vmax, rmiss=yfield(l)%rmiss)
            Else
               Call check_data_bounds (ntcy, y(i1,:,i3), iloc, &
                    vmin=vmin, rmiss=yfield(l)%rmiss)
            End If
  ! - invalid values found -
            If (iloc /= 0) Then
               If (icsv == 0) Then
                  error_prd = yfile%prd1 + (yfile%it1 + Abs(iloc) - 1) - 1
               Else
                  Call reset_iseq (isq_new=isq_mn)
                  error_prd = mfile%prd1 + (mfile%it1 + Abs(iloc) - 1) - 1
                  Call reset_iseq ()
               End If
               check = ibound
               If (y(i1,iloc,i3) < vmin) Then
                  iarg1 = -iloc
               Else
                  iarg1 = iloc
               End If
               cproc = 'check_bounds'
               carg1 = Trim(yfile%cfile%ffile)
               carg2 = get_cdate(error_prd, lsn(1), io_com)
               carg3 = ca_regrs(iregr)
               If (yfile%idstr /= id_grd) carg4 = cstndy(j,l)
               Return
            End If
         End Do
      End Do
   End Do
   check = 0
!
  End Function check_bounds
!
!
!
  Subroutine calculate_seasonals (ntcy)
!
! Calculates seasonal values
!
! Modules
   Use iofiles, Only: mfile, yfile
   Use season,  Only: available_ssn, mon_to_ssn
   Use climate, Only: climate_per, icex, ncy, ntc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ntcy ! - number of years in climatological and training periods -
!
! Locals
!
! Local scalars
   Integer :: k ! - time index -
!
! Local arrays
   Integer, Dimension(:), Allocatable :: iyrs ! - years of data -
!
! Executable Statements
!
! Calculate seasonal values
   Print *, 'Calculating seasonal values ...'
   Call available_ssn (yfile%iseq, ntc)
   Select Case (icex) ! - use nt or ntc instead of nu because data have not yet been compressed -
    Case (0)
      Call mon_to_ssn (mfile%bdate, nyr)
    Case (1, 3)
      Call mon_to_ssn (climate_per%d1, ntcy)
    Case (2)
      Call mon_to_ssn (mfile%bdate, ntcy)
    Case (-1)
      Allocate (iyrs(ntcy))
      iyrs(1) = climate_per%d1%iyr
      Do k = 2, ncy
         iyrs(k) = iyrs(k-1) + 1
      End Do
      iyrs(ncy+1) = mfile%bdate%iyr
      Do k = ncy+2, ntcy
         iyrs(k) = iyrs(k-1) + 1
      End Do
      Call mon_to_ssn (climate_per%d1, ntcy, &
           iyrs=iyrs)
      Deallocate (iyrs)
    Case (-2)
      Allocate (iyrs(ntcy))
      iyrs(1) = mfile%bdate%iyr
      Do k = 2, nyr
         iyrs(k) = iyrs(k-1) + 1
      End Do
      iyrs(nyr+1) = climate_per%d1%iyr
      Do k = nyr+2, ntcy
         iyrs(k) = iyrs(k-1) + 1
      End Do
      Call mon_to_ssn (climate_per%d1, ntcy, &
           iyrs=iyrs)
      Deallocate (iyrs)
   End Select
!
   Return
  End Subroutine calculate_seasonals
 End Function actions_calculate
!
!
!
 Function actions_reset() &
          Result (action)
!
! Resets CPT ready for calculation
!
! Function result
   Integer :: action
!
! Executable Statements
!
! Reset
   action = reset('Resetting', .true.)
!
   Return
 End Function actions_reset
!
!
!
 Function reset(msg, lflags)
!
! Clears memory for recalculation
!
! Modules
#if GUI == 1
   Use clrwin$,        Only: clear_window$
#endif
   Use data_numbers,   Only: zero
   Use screen,         Only: window_update
   Use gui,            Only: prompt_yesno
#if GUI == 1
   Use gui,            Only: ih_con, &
                             close_windows
#endif
   Use menus,          Only: isave, &
                             reset_menus, set_menu_greying
   Use progress_meter, Only: set_progress_meter
   Use iofiles,        Only: mxa, mya, xfile, yfile
   Use analysis,       Only: icalc, &
                             analysis_flags
   Use cpt_view,       Only: grey_menu_flags
!
! Function result
   Integer :: reset
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: msg ! - message -
!
   Logical, Intent(In) :: lflags ! - reset analysis flags? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim
!
! Executable Statements
!
! Check for reset
   If (icalc == 1) Then
      reset = 1
      If (Len_Trim(msg) > 0) Then
         If (prompt_yesno(.true., .true., .false., msg//' will clear the current results.', &
                          msg2='Do you wish to Continue?') /= 1) Return
      End If
      If (isave > 0) Then
         isave = isave - 2
         Call set_menu_greying (isave=isave)
      End If
!
! Close existing windows and clear memory
#if GUI == 1
      reset = close_windows()
      Call clear_window$ (ih_con)
#endif
      reset = reset_mem()
!
! Reset numbers of used variables
      mxa = 0
      mya = 0
      Call window_update (mxa)
      Call window_update (mya)
!
! Reset progress meter
      Call set_progress_meter (zero)
      Print *, ' '
      If (lflags) Call analysis_flags ('off')
!
! Reset grey menu flags
      Call grey_menu_flags (0, xfile%idstr, yfile%idstr)
!
! Reset menu activations
      Call reset_menus (lactreset=.true.)
   End If
   reset = 2
!
   Return
 End Function reset
!
!
!
#if GUI == 1
 Function check_reset() &
          Result (check)
!
! Prompts for reset
!
! Modules
   Use screen,   Only: window_update
   Use gui,      Only: ic, ih_nt
   Use menus,    Only: record_change
   Use settings, Only: nt, nt_old
   Use analysis, Only: icalc
!
! Function result
   Integer :: check
!
! Executable Statements
!
! Check for reset
   check = 1
   If (icalc == 1) Then
      If (ic == ih_nt) Then ! - length of training period -
         check = reset('Modifying length of training period', .true.)
      End If
      If (check == 1) Then
         nt = nt_old
         Call window_update (nt)
         check = 2
         Return
      End If
   End If
!
! Update backups
   nt_old = nt
   check = record_change()
!
   Return
 End Function check_reset
#endif
!
!
!
 Function reset_mem()
!
! Clears memory
!
! Modules
   Use prediction, Only: close_predict
   Use analysis,   Only: close_calc
   Use bootstrap,  Only: close_boot, close_pboot
!
! Function result
   Integer :: reset_mem
!
! Executable Statements
!
! Clear memory
   reset_mem = close_pboot()
   reset_mem = close_boot()
   reset_mem = close_predict()
   reset_mem = close_calc()
!
   reset_mem = 2
!
   Return
 End Function reset_mem
!
!
!
 Subroutine parameter_write_action (iout, ifail)
!
! Writes menu activations to parameter file
!
! Modules
   Use labels,         Only: cg_lenitp, cg_tperiodui
   Use menus,          Only: mn_acts_cv, mn_acts_retro, mn_acts_verify
   Use parameter_file, Only: output_parameter
   Use settings,       Only: nretro, nt1
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
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Write project settings
   If (mn_acts_cv%iact > 0) Then ! - cross-validation -
      If (output_parameter(iout, &
                           cdesc=Trim(mn_acts_cv%l_lab%c), i=mn_acts_cv%id) /= 0) GoTo 1
   Else If (mn_acts_retro%iact > 0) Then ! - retroactive validation -
      If (output_parameter(iout, &
                           cdesc=Trim(mn_acts_retro%l_lab%c), i=mn_acts_retro%id) /= 0) GoTo 1
      If (output_parameter(iout, & ! - length of initial training period -
                           cdesc=Trim(cg_lenitp), i=nt1) /= 0) GoTo 1
      If (output_parameter(iout, & ! - update interval -
                           cdesc=Trim(cg_tperiodui), i=nretro) /= 0) GoTo 1
   Else If (mn_acts_verify%iact > 0) Then ! - verification -
      If (output_parameter(iout, &
                           cdesc=Trim(mn_acts_verify%l_lab%c), i=mn_acts_verify%id) /= 0) GoTo 1
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
 End Subroutine parameter_write_action
End Module menu_actions
