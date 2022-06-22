! Author: Simon Mason
Module menu_options
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: options_boot, options_cca, options_climatology, options_cv, options_data_missing, options_eofx, options_eofy,    &
             options_exclude, options_forecast, options_gcm, options_goodness, options_language, options_list, options_masks, &
             options_regression, options_tailoring, options_time, options_transform, options_verif
#if GUI == 1
   Public :: options_config_console, options_config_text, options_graph_font, options_graph_marker, options_graph_size
#else
   Public :: get_nf, get_nt, options_errors, options_linkf
#endif
!
! Scalars
!
! Integer scalars
   Integer, Public :: imask ! - masking option -
!
Contains
!
!
#if GUI == 0
 Function get_nt()
!
! Prompts for length of training period
!
! Modules
   Use labels,     Only: cg_lentp
   Use settings,   Only: nt
   Use iofiles,    Only: xfile, yfile
   Use season,     Only: icsv, lensn, lb, lsn, nsn
   Use cpt_update, Only: reset_limits_nt
!
! Function result
   Integer :: get_nt
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Determine maximum length of training period
   nt = xfile%nt
   Call reset_limits_nt (icsv, lensn, lsn, lb, nsn)
!
! Get training data settings
   Call get_n (Trim(cg_lentp), (xfile%nt > 0 .and. yfile%nt > 0), nt)
!
! Indicate activation
   get_nt = 0
!
   Return
 End Function get_nt
!
!
!
 Function get_nf ()
!
! Prompts for number of forecasts
!
! Modules
   Use labels,     Only: cg_nof, &
                         l_fcasts, l_lab, &
                         make_label
   Use settings,   Only: nf
   Use iofiles,    Only: zfile
   Use season,     Only: icsv, lb, lprst, lsn
   Use cpt_update, Only: reset_limits_nf
!
! Function result
   Integer :: get_nf
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Determine maximum number of forecasts
   nf = zfile%nt
   Call reset_limits_nf (icsv, lprst, .false., lsn, lb)
!
! Get number of forecasts
   l_lab%c = make_label('L', l_fcasts)
   l_lab%c = Trim(cg_nof)//' '//Trim(l_lab%c)
   Call get_n (Trim(l_lab%c), .false., nf)
!
! Indicate activation
   get_nf = 0
!
   Return
 End Function get_nf
!
!
!
 Subroutine get_n (cprompt, lmnu, n)
!
! Prompts for number
!
! Modules
   Use gui,      Only: prompt_n
   Use settings, Only: mnu, &
                       set_options
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cprompt ! - prompt -
!
   Logical, Intent(In) :: lmnu ! - set minimum to mnu? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: n ! - number -
!
! Locals
!
! Local scalars
   Integer :: i  ! - dummy value -
   Integer :: mn ! - minimum value -
   Integer :: mx ! - maximum value -
!
! Executable Statements
!
! Determine maximum number
   mx = n
!
! Determine minimum number
   If (lmnu) Then
      mn = mnu
   Else
      mn = 0
   End If
!
! Get number
   i = set_options (i1=n)            ! - backup current settings -
   Call prompt_n(cprompt, mn, mx, n) ! - prompt -
   i = set_options (iw=1, i1=n)      ! - confirm changes -
!
   Return
 End Subroutine get_n
!
#endif
!
!
 Function options_boot() &
          Result (opts)
!
! Prompts for bootstrap settings
!
! Modules
   Use gui,          Only: iw
   Use menus,        Only: set_menu_label
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_boot_opts
   Use analysis,     Only: icalc, ihind
   Use bootstrap,    Only: clb, ibcl, ipval, nboot, nperm
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for bootstrap settings
   opts = set_options(i1=ibcl, i2=ipval, i3=nboot, i4=nperm, r1=clb)        ! - backup resampling settings -
   Call get_boot_opts (icalc, ihind, ibcl, ipval, nboot, nperm, clb, iw)    ! - prompt -
   opts = set_options(iw=iw, i1=ibcl, i2=ipval, i3=nboot, i4=nperm, r1=clb) ! - confirm changes -
   Call set_menu_label (ipval=ipval)
   opts = 2
!
   Return
 End Function options_boot
!
!
!
 Function options_cca() &
          Result (opts)
!
! Sets CCA options
!
! Modules
   Use pcs, Only: lcde, &
                  get_cca_settings
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for CCA options
   lcde = .false.
   opts = get_cca_settings()
!
   Return
 End Function options_cca
!
!
!
 Function options_climatology() &
          Result (opts)
!
! Redefines climatological period
!
! Modules
   Use data_cpt_constants,  Only: ia_pfv, nsag
   Use data_io_constants,   Only: io_iso, ioutstd, lprd
   Use data_time_constants, Only: isq_mn, nmn
   Use labels,              Only: cg_to_l
   Use time,                Only: pdate, &
                                  Operator(==), Operator(+), Operator(-), Operator(<), &
                                  nss, &
                                  get_cdate, reset_iseq
   Use gui,                 Only: iw
   Use settings,            Only: nt, &
                                  set_options
   Use opts_prompts,        Only: get_climate_opts
   Use iofiles,             Only: xfile, yfile
   Use fields,              Only: yfield
   Use season,              Only: icsv, lprst, lsn, nsnc
   Use climate,             Only: climate_per, icex, ieclim, &
                                  print_climatology, update_climate, valid_climate
   Use prediction,          Only: ifc
   Use analysis,            Only: ianal, icalc
   Use menu_actions,        Only: reset
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Integer :: iy2 ! - year in two-digit format -
   Integer :: nm  ! - month multiplier -
!
   Character(Len=128) :: c_lim  ! - current date limits -
   Character(Len= 64) :: c_loop ! - end-of-year loop text -
!
   Logical :: ldif1 ! - start of climatological period is changed? -
   Logical :: ldif2 ! - end of climatological period is changed? -
!
! Local arrays
   Integer, Dimension(0:nsag) :: iscs ! - seasonal calculation options -
!
   Character(Len=lprd), Dimension(2) :: climits ! - date limits -
!
   Type(pdate), Dimension(2) :: dlimits ! - date limits -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Mod, Trim
!
! Executable Statements
!
! Prevent change if results are results are calculated and the climatological period extends beyond the training period
   opts = 1
   If (icalc == 1 .and. icex == 1) Then
      If (reset('Adjusting the climatological period', .true.) == 1) Return
   End If
!
! Set default climatology if necesssary
   icsv = icsv + 1
   opts = set_options(i1=ieclim, o1=icsv, oa1=iscs(0:), d1=climate_per%d1, d2=climate_per%d2)
   If (lprst) Then
      Call reset_iseq (isq_new=isq_mn)
      nm = nmn
   Else
      nm = 1
   End If
   If (ianal /= ia_pfv) Then
      If (climate_per%d1 == 0) climate_per%d1 = yfile%fdate - lsn(0)
      If (climate_per%d2 == 0) climate_per%d2 = yfile%fdate + ((nt - 1)*nm - lsn(0))
   Else
      If (ieclim == 1 .and. yfile%prd1%sdate < xfile%prd1%sdate) Then
         If (climate_per%d1 == 0) climate_per%d1 = yfile%prd1%sdate
         If (climate_per%d2 == 0) climate_per%d2 = yfile%fdate - nss
      Else
         If (climate_per%d1 == 0) climate_per%d1 = yfile%prd1%sdate
         If (climate_per%d2 == 0) climate_per%d2 = yfile%prdn%sdate
      End If
   End If
!
! Calculate limits for climatological period
   Select Case (icalc)
    Case (0)
      dlimits(1) = yfile%prd1%sdate - lsn(0)
      dlimits(2) = yfile%prdn%sdate
    Case (1)
      dlimits(1) = yfile%bdate
      dlimits(2) = yfile%bdate + (nt - 1)*nm
   End Select
   climits(1) = get_cdate(dlimits(1), 1, io_iso)
   climits(2) = get_cdate(dlimits(2), 1, io_iso)
   Select Case (icalc)
    Case (0)
      c_lim = '(Data limits are '//Trim(climits(1))//' '//Trim(cg_to_l)//' '//Trim(climits(2))//')'
    Case (1)
      c_lim = '(Current data limits are '//Trim(climits(1))//' '//Trim(cg_to_l)//' '//Trim(climits(2))//')'
   End Select
!
! Create text to illustrate end-of-year looping
   If (yfile%prd1%edate%iyr /= yfile%prd1%sdate%iyr) Then
      iy2 = Mod(yfile%fdate%iyr+1, 100)
      Write (Unit=c_loop, Fmt='(3A,I4,A,I2.2,A,I4)') &
           'For example, for ', Trim(yfile%cssn),' ', yfile%fdate%iyr,'/', iy2,' enter ', yfile%fdate%iyr
   Else
      c_loop = ' '
   End If
!
! Prompt for climatological period
   Call get_climate_opts ((icsv > 1 .and. icalc == 0), nsag, yfile%iseq, Trim(c_lim), Trim(c_loop), &
        Trim(yfield(1)%cssn%cssn(nsnc)), Trim(yfield(nss)%cssn%cssn(nsnc)), &
        ieclim, iscs(0:), climate_per%d1, climate_per%d2, dlimits, iw, valid_climate)
!
! Reset climatological period
   If (lprst) Call reset_iseq ()
   opts = set_options(iw=iw, i1=ieclim, o1=icsv, oa1=iscs(0:), d1=climate_per%d1, d2=climate_per%d2, ldif_d1=ldif1, ldif_d2=ldif2)
   icsv = icsv - 1
   If (iw == 1) Then
      If (ldif1 .or. ldif2) Then
         opts = update_climate(icalc)
         If (ifc == 2) ifc = 1
      End If
!
! Confirm original climatological period
   Else
      If (Len_Trim(climate_per%clim1) > 0) Call print_climatology (.true., &
                                                iout=ioutstd)
   End If
   opts = 2
!
   Return
 End Function options_climatology
!
!
!
#if GUI == 1
 Function options_config_console() &
          Result (opts)
!
! Prompts for console size adjustments
!
! Modules
   Use gui,          Only: icon_h, icon_v, iw
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_console_opts
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for console size adjustments
   opts = set_options(i1=icon_h, i2=icon_v)        ! - backup current setting -
   Call get_console_opts (icon_h, icon_v, iw)      ! - prompt -
   opts = set_options(iw=iw, i1=icon_h, i2=icon_v) ! - confirm changes -
!
   opts = 1
!
   Return
 End Function options_config_console
!
!
!
 Function options_config_text() &
          Result (opts)
!
! Prompts for text scaling
!
! Modules
   Use screen,       Only: dts_adj
   Use gui,          Only: iw
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_text_opts
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for text scaling
   opts = set_options(r1=dts_adj)        ! - backup current settings -
   Call get_text_opts (dts_adj, iw)      ! - prompt -
   opts = set_options(iw=iw, r1=dts_adj) ! - confirm changes -
!
   opts = 1
!
   Return
 End Function options_config_text
!
!
!
#endif
 Function options_cv() &
          Result (opts)
!
! Prompts for length of cross-validation period
!
! Modules
   Use gui,          Only: iw
   Use settings,     Only: lcw, lcw_old, &
                           set_options
   Use opts_prompts, Only: get_cv_opts
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Get cross-validation options
   lcw_old = lcw                     ! - remember previous cross-validation window length -
   opts = set_options(i1=lcw)        ! - back-up current settings -
   Call get_cv_opts (lcw, iw)        ! - prompt -
   opts = set_options(iw=iw, i1=lcw) ! - confirm changes -
!
   Return
 End Function options_cv
!
!
!
 Function options_exclude() &
          Result (opts)
!
! Prompts to exclude specific observations and/or variables
!
! Modules
   Use time,         Only: Operator(+), &
                           date_diff, get_prds
#if GUI == 1
   Use gui,          Only: iw
#endif
   Use settings,     Only: nt
   Use opts_prompts, Only: get_exclude_opts
   Use arrays,       Only: cxprds, xprds
   Use iofiles,      Only: xfile
   Use missing,      Only: kincl, &
                           point_kinclude
!
! Function result
   Integer :: opts
!
! Locals
!
#if GUI == 1
! Local arrays
   Integer, Dimension(:), Allocatable :: iincl ! - inclusion indicators -
!
#endif
! Executable Statements
!
! Update pointers in case of change in training period
   Call point_kinclude ()
!
! Allocate memory
   Allocate (xprds(nt))
   Allocate (cxprds(nt))
#if GUI == 1
   Allocate (iincl(nt))
!
! Convert inclusion indicators to integer
   Where (kincl(1:nt))
      iincl(:) = 1
   ElseWhere
      iincl(:) = 0
   End Where
#endif
!
! Identify dates
   xprds(1) = xfile%prd1 + date_diff(xfile%prd1%sdate, xfile%fdate, xfile%iseq)
   Call get_prds (nt, 2, xprds(:), cxprds(:))
!
! Prompt to exclude observations
#if GUI == 1
1  Call get_exclude_opts (nt, cxprds(:), iincl(:), iw)
   Select Case (iw)
    Case (1) ! - Ok -
      Where (iincl(:) == 0)
         kincl(1:nt) = .false.
      ElseWhere
         kincl(1:nt) = .true.
      End Where
    Case (2) ! - Reset -
      iincl(:) = 1
      GoTo 1
   End Select
#else
   Call get_exclude_opts (nt, cxprds(:), kincl(:))
#endif
!
! Clear memory
#if GUI == 1
   Deallocate (iincl)
#endif
   Deallocate (cxprds)
   Deallocate (xprds)
   opts = 1
!
   Return
 End Function options_exclude
!
!
!
 Function options_masks() &
          Result (opts)
!
! Prompts for masking settings
!
! Modules
   Use data_cpt_constants, Only: ncorr
   Use gui,                Only: iw
   Use settings,           Only: idry, iskmask, iskmaski, rdry, rskmask, &
                                 set_options
   Use opts_prompts,       Only: get_masks_opts
   Use analysis,           ONly: icalc
   Use cpt_update,         Only: update_mask
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Logical :: ldif_dry  ! - dry masking switched on? -
   Logical :: ldif_mask ! - new masking correlation? -
!
! Local arrays
   Integer, Dimension(ncorr) :: iopts ! - options -
!
! Executable Statements
!
! Prompt for masking settings
   opts = set_options(i1=idry, i2=iskmask, r1=rdry, r2=rskmask, o1=iskmaski, oa1=iopts(:)) ! - backup current settings -
   Call get_masks_opts (imask, icalc, idry, iskmask, rdry, rskmask, iopts(:), iw)          ! - prompt -
   opts = set_options(iw=iw, i1=idry, ldif_i1=ldif_dry, i2=iskmask, ldif_i2=ldif_mask, &   ! - confirm changes -
          r1=rdry, r2=rskmask, o1=iskmaski, oa1=iopts(:))
!
! Update masking correlations if the selected correlation has changed or if dry masking is switched on
   If (imask == 1 .or. idry == 0 .or. iw /= 1) Return
   ldif_mask = ldif_dry .or. ldif_mask
   Call update_mask (ldif_mask)
!
   Return
 End Function options_masks
!
!
!
 Function options_data_missing() &
          Result (opts)
!
! Prompts for missing-value settings and options
!
! Modules
   Use data_cpt_constants, Only: ifile_x, ifile_y, nmo
   Use gui,                Only: iw
   Use settings,           Only: set_options
   Use opts_prompts,       Only: get_missing_opts
   Use iofiles,            Only: xfile, yfile
   Use fields,             Only: xfield, yfield
   Use missing,            Only: imissf, immx, immy, ipmx, ipmy, ipvx, ipvy, nnsx, nnsy
   Use analysis,           Only: ianal
!
! Function result
   Integer :: opts
!
! Locals
!
! Local arrays
   Integer :: irmx(nmo) ! - replace missing X option flags -
   Integer :: irmy(nmo) ! - replace missing Y option flags -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max
!
! Executable Statements
!
! Backup missing-value settings
   Select Case (imissf)
    Case (0)
      opts = set_options(i1=ipmx, i2=ipvx, i3=nnsx, r1=xfield(1)%rmiss, o1=immx, oa1=irmx(:), &
                         i4=ipmy, i5=ipvy, i6=nnsy, r2=yfield(1)%rmiss, o2=immy, oa2=irmy(:))
    Case (ifile_x)
      immx = Max(1, immx)
      opts = set_options(i1=ipmx, i2=ipvx, i3=nnsx, r1=xfield(1)%rmiss, o1=immx, oa1=irmx(:))
    Case (ifile_y)
      immy = Max(1, immy)
      opts = set_options(i1=ipmy, i2=ipvy, i3=nnsy, r1=yfield(1)%rmiss, o1=immy, oa1=irmy(:))
   End Select
!
! Prompt for missing-value options
   Call get_missing_opts (imissf, ianal, xfile%idstr, yfile%idstr, &
        ipmx, ipvx, nnsx, xfield(1)%rmiss, irmx(:), &
        ipmy, ipvy, nnsy, yfield(1)%rmiss, irmy(:), iw)
!
! Confirm settings
   Select Case (imissf)
    Case (0)
      opts = set_options(iw=iw, i1=ipmx, i2=ipvx, i3=nnsx, r1=xfield(1)%rmiss, o1=immx, oa1=irmx(:), &
                                i4=ipmy, i5=ipvy, i6=nnsy, r2=yfield(1)%rmiss, o2=immy, oa2=irmy(:))
    Case (ifile_x)
      opts = set_options(iw=iw, i1=ipmx, i2=ipvx, i3=nnsx, r1=xfield(1)%rmiss, o1=immx, oa1=irmx(:))
    Case (ifile_y)
      opts = set_options(iw=iw, i1=ipmy, i2=ipvy, i3=nnsy, r1=yfield(1)%rmiss, o1=immy, oa1=irmy(:))
   End Select
!
! Duplicate missing-value flag for additional fields
   If (opts /= 3) Then
      If (xfile%nfl > 1) xfield(2:)%rmiss = xfield(1)%rmiss
      If (yfile%nfl > 1) yfield(2:)%rmiss = yfield(1)%rmiss
   End If
!
   Return
 End Function options_data_missing
!
!
!
 Function options_eofx() &
          Result (opts)
!
! Calls function to set X EOF options
!
! Modules
   Use pcs, Only: lcde, &
                  get_eofx_settings
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for X EOF options
   lcde = .false.
   opts = get_eofx_settings()
!
   Return
 End Function options_eofx
!
!
!
 Function options_eofy() &
          Result (opts)
!
! Calls function to set Y EOF options
!
! Modules
   Use pcs, Only: lcde, &
                  get_eofy_settings
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for Y EOF options
   lcde = .false.
   opts = get_eofy_settings()
!
   Return
 End Function options_eofy
!
!
!
#if GUI == 0
 Function options_errors() &
          Result (opts)
!
! Prompts for user-error options
!
! Modules
   Use data_cpt_constants, Only: nueo
   Use gui,                Only: iprompt
   Use labels,             Only: l_errhand, &
                                 ls_uerropts
!
! Function result
   Integer :: opts
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for user-error handling
   iprompt = iprompt + 1
   Call get_option_list (Trim(l_errhand%c), 'Select option for how to handle user input errors:', nueo, ls_uerropts(:)%c, iprompt)
   iprompt = iprompt - 1
   opts = 2
!
   Return
 End Function options_errors
!
!
!
#endif
 Function options_forecast() &
          Result (opts)
!
! Prompts for forecast settings
!
! Modules
   Use data_cpt_constants, Only: ie_cvh, ie_cv2, ie_ret, ievs, nev
   Use maths,              Only: find_index
   Use gui,                Only: iw
   Use settings,           Only: clf, icv2, iev, iexc, ifpr, iodds, iprecf, iretro, nenf, &
                                 set_options
   Use opts_prompts,       Only: get_forecast_opts
   Use analysis,           Only: icalc
   Use cpt_update,         Only: update_errvar
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Integer :: i    ! - index -
   Integer :: neva ! - available number of error-variance options -
!
   Logical :: ldif_iev ! - changed error-variance option? -
!
! Local arrays
   Integer, Dimension(nev) :: ievi ! - available error-variance options indices -
   Integer, Dimension(nev) :: ieva ! - error-variance options flags -
!
! Executable Statements
!
! Determine available number of error-variance options
   Do Concurrent (i = 1:nev)
      ievi(i) = i
   End Do
   If (icalc == 0) Then
      neva = nev
   Else ! - at least retroactive or double cross-validated will be unavailable -
      neva = nev - 1
      If (icv2 > 0) Then
         ievi(neva) = ievi(neva) + 1
      Else If (iretro == 0) Then ! - retroactive and double cross-validation are unavailable -
         neva = neva - 1
      End If
      ievi(neva+1:) = 0
   End If
!
! Reset error-variance option if it is unavailable -
   opts = set_options(i1=iev)
   If (icalc == 1) Then
      If (  icv2 == 0 .and. iev == ie_cv2) iev = ie_cvh ! - remove double-cross-validation option if it is unavailable -
      If (iretro == 0 .and. iev == ie_ret) iev = ie_cvh ! - remove retroactive option if it is unavailable -
   End If
   iev = find_index(nev, iev, ievs(ievi(1:neva)))
!
! Backup forecast settings
   opts = set_options(i2=iodds, i3=iexc, i4=nenf, i5=iprecf, i6=ifpr, o1=iev, oa1=ieva(:), r1=clf)
!
! Open forecast options prompt
   Call get_forecast_opts (neva, ievi(1:neva), clf, iexc, ifpr, iodds, iprecf, nenf, ieva(1:neva), iw)
!
! Identify error-variance type
   opts = set_options(iw=iw, i2=iodds, i3=iexc, i4=nenf, o1=iev, oa1=ieva(:), r1=clf)
   iev = ievs(ievi(iev))
   opts = set_options(iw=iw, i1=iev, ldif_i1=ldif_iev, i5=iprecf, i6=ifpr)
   If (opts == 2) Call update_errvar (ldif_iev)
   opts = 2
!
   Return
 End Function options_forecast
!
!
!
 Function options_gcm() &
          Result (opts)
!
! Sets GCM options
!
! Modules
   Use data_cpt_constants, Only: ig_skl, ngc, ninterp, nmcs
   Use gui,                Only: iw
   Use settings,           Only: igauss, igauss_bk, igcms, imc, intp, &
                                 set_options
   Use opts_prompts,       Only: get_gcm_opts
   Use pcs,                Only: ih_eof
!
! Function result
   Integer :: opts
!
! Locals
!
! Local arrays
   Integer, Dimension(ninterp) :: intps  ! - interpolation options -
!   Integer, Dimension(  nensc) :: ienscs ! - ensemble forecasting flags -
   Integer, Dimension(   nmcs) :: imcs   ! - model combination options flags -
   Integer, Dimension(    ngc) :: igcs   ! - correction options flags -
!
! Executable Statements
!
! Identify standardization option
   intp = 2 - intp
   igcms = igcms + 1
   imc = imc + 1
!
! Prompt for GCM options
   opts = set_options(o1=intp, oa1=intps(:), o2=igcms, oa2=igcs(:), o3=imc, oa3=imcs(:))        ! - backup current settings -
   Call get_gcm_opts (intps(:), igcs(:), imcs(:), iw, ih_eof)                                   ! - prompt -
   opts = set_options(iw=iw, o1=intp, oa1=intps(:), o2=igcms, oa2=igcs(:), o3=imc, oa3=imcs(:)) ! - confirm changes -
!
! Set options
   intp = 2 - intp
   imc = imc - 1
   igcms = igcms - 1
!
! Disable transformation, if appropriate
   If (iw == 1) Then
      If (igcms == ig_skl) Then
         igauss = igauss_bk
      Else
         igauss_bk = igauss
         igauss = 0
      End If
   End If
!
   Return
 End Function options_gcm
!
!
!
 Function options_goodness() &
          Result (opts)
!
! Prompts for correlation coefficient to calculate goodness index
!
! Modules
   Use data_cpt_constants, Only: ngood
   Use labels,             Only: ca_goodi, &
                                 l_goodness
   Use settings,           Only: igood
!
! Function result
   Integer :: opts
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for goodness index correlation coefficient
   Call get_option_list (Trim(l_goodness%c), 'Select score for calculating goodness index:', ngood, ca_goodi, igood)
   opts = 1
!
   Return
 End Function options_goodness
!
!
!
#if GUI == 1
 Function options_graph_font() &
          Result (opts)
!
! Prompts for graphics font scaling
!
! Modules
   Use screen,       Only: font1, font2, font3
   Use gui,          Only: iw
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_gfont_opts
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for graphics font scalings
   opts = set_options(r1=font1%fscale, r2=font1%fscale, r3=font3%fscale)        ! - backup current setting -
   Call get_gfont_opts (font1%fscale, font2%fscale, font3%fscale, iw)           ! - prompt -
   opts = set_options(iw=iw, r1=font1%fscale, r2=font1%fscale, r3=font3%fscale) ! - confirm changes -
!
   opts = 1
!
   Return
 End Function options_graph_font
!
!
!
 Function options_graph_marker() &
          Result (opts)
!
! Prompts for station markers
!
! Modules
   Use data_gui_constants, Only: nmrk
   Use gui,                Only: iw
   Use settings,           Only: set_options
   Use opts_prompts,       Only: get_gmarker_opts
   Use markers,            Only: gmarker
!
! Function result
   Integer :: opts
!
! Locals
!
! Local arrays
   Integer, Dimension(nmrk) :: jmrks ! - marker type flags -
!
! Executable Statements
!
! Prompt for station markers
   opts = set_options(o1=gmarker%imrk, oa1=jmrks(:), r1=gmarker%rscale)        ! - backup current setting -
   Call get_gmarker_opts (nmrk, gmarker%rscale, jmrks(:), iw)                  ! - prompt -
   opts = set_options(iw=iw, o1=gmarker%imrk, oa1=jmrks(:), r1=gmarker%rscale) ! - confirm changes -
!
   opts = 1
!
   Return
 End Function options_graph_marker
!
!
!
 Function options_graph_size() &
          Result (opts)
!
! Prompts for graphics scaling
!
! Modules
   Use gui,          Only: iw
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_gsize_opts
   Use graphics,     Only: gszs
   Use cpt_update,   Only: init_image_sizes
!
! Function result
   Integer :: opts
!
! Executable Statements
!
! Prompt for graphics scaling
   opts = set_options(r1=gszs)        ! - backup current setting -
   Call get_gsize_opts (gszs, iw)     ! - prompt -
   opts = set_options(iw=iw, r1=gszs) ! - confirm changes -
!
! Update graphics sizes
   If (iw == 1) Call init_image_sizes ()
   opts = 1
!
   Return
 End Function options_graph_size
!
!
!
#endif
 Function options_language() &
          Result (opts)
!
! Changes language of CPT prompts
!
! Modules
#if GUI == 0
   Use data_text,         Only: nlang
#endif
   Use data_text,         Only: ilang
   Use data_io_constants, Only: ddir
   Use labels,            Only: init_labels
   Use errors,            Only: cpt_error
#if GUI == 0
   Use opts_prompts,      Only: get_language
#endif
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
#if GUI == 0
   Integer :: ilang_bk ! - backup language index -
#endif
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for language preferences
1  opts = 0
#if GUI == 0
   ilang_bk = ilang
   Call get_language (nlang, ilang)
   Select Case (ilang)
! - quit -
    Case (0)
      ilang = ilang_bk
! - set language -
    Case (1:nlang)
#endif
      Call init_labels (ilang, Trim(ddir), ifail)
      If (ifail /= 0) Then
         Call cpt_error ('init_labels', .false., ifail)
         opts = 1
      End If
! - error -
#if GUI == 0
      If (ifail /= 0) GoTo 1
    Case Default
      GoTo 1
   End Select
!
#endif
   Return
 End Function options_language
!
!
!
 Function options_regression() &
          Result (opts)
!
! Sets regression options

! Modules
   Use data_cpt_constants, Only: il_idt, il_lgt, il_lgm, ir_ols, ir_log, ir_bin, ir_gam, ir_poi, nregr
   Use gui,                Only: iw
   Use menus,              Only: set_menu_greying
   Use settings,           Only: igauss, igauss_bk, ilink, ilink_bk, iregr, iregr_bk, istd, istd_bk, &
                                 set_options
   Use opts_prompts,       Only: get_regression_opts
   Use analysis,           Only: ianal, icalc
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Integer :: icont ! - continue? -
!
! Local arrays
   Integer, Dimension(nregr) :: iregrs ! - regression options flags -
!
! Executable Statements
!
! Prompt for regression option
   opts = set_options(o1=iregr, oa1=iregrs(:))     ! - backup current settings -
   Call get_regression_opts (nregr, iregrs(:), iw) ! - prompt
!
! Set option
   If (iw == 1 .or. iw == 2) Then
      icont = 1
   Else
      icont = 0
   End If
   opts = set_options(iw=icont, o1=iregr, oa1=iregrs(:)) ! - confirm changes -
   If (icont == 0) Return
!
! Set default link function
   If (iregr /= iregr_bk) Then
      Select Case (iregr)
       Case (ir_ols)
         ilink = il_idt
       Case (ir_log)
         ilink = il_lgt
       Case (ir_bin)
         ilink = il_lgm
       Case (ir_gam, ir_poi)
         ilink = il_idt
      End Select
   End If
   If (iw == 2) opts = options_linkf()
!
! Disable standardization and transformation if appropriate
   Select Case (iregr)
    Case (ir_ols) ! - restore standardization and transformation if reverting to OLS -
      igauss = igauss_bk
      istd = istd_bk
    Case Default
      igauss_bk = igauss
      igauss = 0
      istd_bk = istd
      istd = 0
   End Select
   Call set_menu_greying (ianal=ianal, icalc=icalc, iregr=iregr, istd=istd)
   iregr_bk = iregr
   ilink_bk = ilink
!
   Return
 End Function options_regression
!
!
!
 Function options_linkf() &
          Result (opts)
!
! Sets link function
!
! Modules
   Use data_cpt_constants, Only: il_idt, il_inv, il_lgt, il_lgm, il_log, il_sqr, ilinks, ir_ols, ir_log, ir_bin, ir_gam, ir_poi, &
                                 nlink
   Use maths,              Only: find_index
   Use gui,                Only: iw
   Use settings,           Only: ilink, iregr, &
                                 set_options
   Use opts_prompts,       Only: get_link_opts
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Integer :: nlinks ! - number of available link functions -
!
! Local arrays
   Integer, Dimension(nlink-1) :: ilinka ! - available link functions -
   Integer, Dimension(nlink-1) :: ilinki ! - link function options flags -
!
! Executable Statements
!
! Identify current and available link options
   opts = set_options(i1=ilink) ! - backup original link -
   Select Case (iregr)
    Case (ir_ols)
      nlinks = 1
      ilinki(1) = find_index(nlink, il_idt, ilinks(:))
    Case (ir_log)
      nlinks = 1
      ilinki(1) = find_index(nlink, il_lgt, ilinks(:))
    Case (ir_bin)
      nlinks = 1
      If (ilink == il_lgm) ilink = il_lgt
      ilinki(1) = find_index(nlink, il_lgt, ilinks(:))
    Case (ir_gam)
      nlinks = 4
      ilinki(1) = find_index(nlink, il_idt, ilinks(:))
      ilinki(2) = find_index(nlink, il_inv, ilinks(:))
      ilinki(3) = find_index(nlink, il_log, ilinks(:))
      ilinki(4) = find_index(nlink, il_sqr, ilinks(:))
    Case (ir_poi)
      nlinks = 4
      ilinki(1) = find_index(nlink, il_idt, ilinks(:))
      ilinki(2) = find_index(nlink, il_inv, ilinks(:))
      ilinki(3) = find_index(nlink, il_log, ilinks(:))
      ilinki(4) = find_index(nlink, il_sqr, ilinks(:))
   End Select
   ilink = find_index(nlinks, ilink, ilinks(ilinki(1:nlinks)))
!
! Prompt for link function
   opts = set_options(o1=ilink, oa1=ilinka(:))                         ! - backup current settings -
   Call get_link_opts (nlinks, ilinki(1:nlinks), ilinka(1:nlinks), iw) ! - prompt -
   opts = set_options(iw=iw, o1=ilink, oa1=ilinka(:))                  ! - confirm changes -
   If (iw == 1) Then
      ilink = ilinki(ilink)
      If (iregr == ir_bin .and. ilink == il_lgt) ilink = il_lgm
   Else
      opts = set_options(iw=iw, i1=ilink)
   End If
   opts = 2
!
   Return
 End Function options_linkf
!
!
!
 Function options_tailoring () &
          Result (opts)
!
! Prompts for threshold and other tailoring settings
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: nstd, nthr, nts
   Use time,               Only: pdate, &
                                 Operator(+), Operator(==)
   Use gui,                Only: iw
   Use errors,             Only: cpt_error
   Use settings,           Only: istd, izero, &
                                 set_options
   Use opts_prompts,       Only: get_tailoring_opts
   Use iofiles,            Only: yfile
   Use categories,         Only: ithr, pthr, refs, thr, &
                                 check_refdates
   Use cpt_update,         Only: update_tailoring
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Integer :: j        ! - threshold index -
   Integer :: istd_old ! - previous standardization option -
   Integer :: ifail    ! - error indicator -
!
   Logical :: ldif_istd ! - standardization option changed? -
!
! Local arrays
   Integer, Dimension(nstd) :: istds ! - standardization options flags -
   Integer, Dimension(nthr) :: ithrs ! - threshold options flags -
!
   Type(pdate), Dimension(2) :: dlimits ! - date limits -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Backup settings
   If (yfile%cfile%lset) Then
      Do j = 1, nts
         If (refs(j)%iyr == 0) refs(j) = yfile%fdate + (j-1)
      End Do
   End If
   istd_old = istd
   istd = istd + 1
   opts = set_options(o1=istd, oa1=istds(:), o2=ithr, oa2=ithrs(:), ra1=thr(:), ra2=pthr(:), d1=refs(1), d2=refs(2))
!
! Convert upper threshold to right-tail probability
   pthr(2) = one - pthr(2)
!
! Prompt for threshold settings
1  If (yfile%fdate%iyr < yfile%prdn%sdate%iyr) Then
      dlimits(1) = yfile%fdate
      dlimits(2) = yfile%prdn%sdate
      Call get_tailoring_opts (nstd-(1-izero), nthr, istds(:), ithrs(:), pthr(:), refs(:), thr(:), iw, check_refdates, &
           dlimits=dlimits(:))
   Else
      Call get_tailoring_opts (nstd-(1-izero), nthr, istds(:), ithrs(:), pthr(:), refs(:), thr(:), iw, check_refdates)
   End If
!
! Confirm changes
   opts = set_options(iw=iw, o1=istd, oa1=istds(:), ldif_o1=ldif_istd)
   istd = istd - 1
   If (iw == 1) Then
      If (Sum(pthr(:)) > one) Then ! - check for valid probability thresholds -
         ifail = 1
         Call cpt_error ('threshold_opts', .false., ifail)
         GoTo 1
      End If
      pthr(2) = one - pthr(2) ! - convert back to left-tail probability -
      opts = set_options(iw=iw, o2=ithr, oa2=ithrs(:), d1=refs(1), d2=refs(2), ra1=thr(:), ra2=pthr(:))
      Call update_tailoring (istd_old, (opts == 2), ldif_istd, ifail) ! - if opts is 2 somrthing has changed -
      If (ifail /= 0) GoTo 2
   Else
      GoTo 2
   End If
   opts = 2
!
   Return
!
! Errors - restore backups
2  opts = set_options(iw=0, o1=istd, oa1=istds(:), o2=ithr, oa2=ithrs(:), ra1=thr(:), ra2=pthr(:))
!
   Return
 End Function options_tailoring
!
!
!
 Function options_time() &
          Result (opts)
!
! Prompts for handling of lengths of months
!
! Modules
   Use data_cpt_constants, Only: ntime
   Use time,               Only: itime
   Use gui,                Only: iw
   Use settings,           Only: set_options
   Use opts_prompts,       Only: get_time_opts
!
! Function result
   Integer :: opts
!
! Locals
!
! Local arrays
   Integer, Dimension(ntime) :: itimes ! - time options -
!
! Executable Statements
!
! Prompt for time settings
   opts = set_options(o1=itime, oa1=itimes(:))        ! - backup current settings -
   Call get_time_opts (ntime, itimes(:), iw)          ! - prompt -
   opts = set_options(iw=iw, o1=itime, oa1=itimes(:)) ! - confirm changes -
!
   opts = 2
!
   Return
 End Function options_time
!
!
!
 Function options_transform() &
          Result (opts)
!
! Prompts for Y-data transformation options
!
! Modules
   Use data_cpt_constants, Only: it_non, nto
   Use labels,             Only: ca_tos, &
                                 l_transforms
   Use settings,           Only: igauss, igto
!
! Function result
   Integer :: opts
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for transformation option
   Call get_option_list (Trim(l_transforms%c), 'Select distribution for transformation:', nto, ca_tos(:), igto)
   If (igauss /= it_non) igauss = igto
   opts = 2
!
   Return
 End Function options_transform
!
!
!
 Function options_verif() &
          Result (opts)
!
! Prompts for verification options
!
! Modules
   Use data_cpt_constants, Only: nbs, nbss
   Use gui,                Only: iw
   Use settings,           Only: ibi, ibs, nb, &
                                 set_options
   Use opts_prompts,       Only: get_verif_opts
!
! Function result
   Integer :: opts
!
! Locals
!
! Local scalars
   Integer :: i ! - bin index -
   Integer :: n ! - size of bins -
!
! Local arrays
   Character(Len=16), Dimension(nbs) :: cnbs ! - number of probability bins -
!
! Executable Statements
!
! Construct labels
   Do i = 1, nbs
      n = 100/(nbss(i) - 1)
      Write (Unit=cnbs(i), Fmt='(A,I0,A)') '(', n, '% bins)'
   End Do
!
! Prompt for number of probability bins
   opts = set_options(i1=ibi, i2=nb, i3=ibs)        ! - backup current settings -
   Call get_verif_opts (cnbs(:), ibi, ibs, iw)      ! -  prompt -
   nb = nbss(ibi)
   opts = set_options(iw=iw, i1=ibi, i2=nb, i3=ibs) ! - confirm changes -
   opts = 2
!
   Return
 End Function options_verif
!
!
!
 Subroutine get_option_list (ctitle, cdesc, nopt, copts, iopt)
!
! Prompts for option from a list
!
! Modules
   Use gui,          Only: iw
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_list_opts
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nopt ! - number of options -
!
   Character(Len=*), Intent(In) :: ctitle ! - title -
   Character(Len=*), Intent(In) :: cdesc  ! - description -
!
! - input arrays -
   Character(Len=*), Dimension(:), Intent(In) :: copts ! - list of options -
!
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Locals
!
! Local arrays
   Integer, Dimension(nopt) :: iopts ! - options -
!
! Executable Statements
!
! Prompt for option
   iw = set_options(o1=iopt, oa1=iopts(:))                          ! - backup current settings -
   Call get_list_opts (ctitle, cdesc, nopt, copts(:), iopts(:), iw) ! - prompt -
   iw = set_options(iw=iw, o1=iopt, oa1=iopts(:))                   ! - confirm changes -
!
   Return
 End Subroutine get_option_list
!
!
!
 Function options_list() &
          Result (opts)
!
! List all CPT options
!
! Modules
#if GUI == 1
   Use clrwin$,             Only: winio$
#endif
   Use data_text,           Only: ca_lang, ialphabet, ilang, i_cyrillic, llab6
   Use data_version,        Only: cpt_ver
   Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ir_ols, is_sti, it_non, iv_ple, iv_abs, iv_ref, ng, nts
   Use data_io_constants,   Only: ca_del, ca_faccs, ca_ffmts, ca_prcs, io_iso, rdir
   Use data_time_constants, Only: isq_mn, isq_so, isq_sn, isq_yr, iuseq
#if GUI == 1
   Use labels,              Only: ca_markers, &
                                  cg_and_l, cg_screen, &
                                  l_bw, l_catshade, l_console, l_crosses, l_graphics, l_graphscal, l_masklake, l_maskland, &
                                  l_pintervals, l_reversecols, l_scorgrphs, l_shadeland, l_shadesea, l_vlines, l_txtscal
#else
   Use labels,              Only: l_errhand, &
                                  cg_errors, &
                                  ls_uerropts
#endif
   Use labels,              Only: c_lab, &
                                  ca_atypes_a, ca_corrects, ca_corrs, ca_dstruct_l, ca_evs, ca_goodi, ca_interps, ca_links,        &
                                  ca_mcs, ca_pccos, ca_regrs, ca_seqs, ca_seqs_l, ca_ssnagg, ca_stds_t, ca_thrs, ca_times, ca_tos, &
                                  cg_conflev, cg_fpround, cg_interpn, cg_lcv, cg_lenitp, cg_lenspi, cg_linkf, cg_maxnof,           &
                                  cg_minnof, cg_missmethod, cg_model, cg_modelcomb, cg_mxpmissdat, cg_missvalflag, cg_nnearns,     &
                                  cg_obs, cg_output, cg_persist, cg_ref, cg_rootdir, cg_runtime_l, cg_seasonlims, cg_threshtype,   &
                                  cg_time, cg_timeseq, cg_tperiod, cg_tperiodui, cg_version, &
                                  l_analysis, l_climatol, l_config, l_cvn2, l_data, l_datdom, l_drymask, l_ensave, l_fcasts,    &
                                  l_goodness, l_lang, l_missings, l_models, l_modes, l_no, l_predictandy, l_predictorx,         &
                                  l_resamples, l_showopts, l_skillmask, l_sortens, l_stdn, l_synchps, l_timesets, l_transformy, &
                                  l_verifn, l_zerob, &
                                  ls_analyses, ls_cats, ls_threshs, &
                                  lowcase, make_label, upcase
   Use time,                Only: Operator(+), &
                                  pprd, &
                                  iseq, itime, lslim, ssnlims, &
                                  date_diff, get_cdate, get_cssn
#if GUI == 1
   Use screen,              Only: dts_adj, font1, font2, font3
   Use gui,                 Only: icbw, icon_h, icon_v, icrev, iw
#if FTN95 == 1
   Use gui,                 Only: print_prompt
#endif
#else
   Use gui,                 Only: iprompt
#endif
   Use gui,                 Only: iw, &
                                  box_close, box_open, gui_creturn, init_psheet, init_win, print_cvalue, print_ivalue, &
                                  print_rvalue, print_yesno, win_prompt, window_end
   Use settings,            Only: clf, ibs, icv2, idry, ieav, iev, iexc, ifpr, igauss, igcms, igood, ilink, imc, intp, iodds, &
                                  iprecf, iregr, iretro, isem, iskmask, iskmaski, istd, istdr, isynch, izero, lcw, nb, nenf,  &
                                  nretro, nt1, rdry, rskmask
   Use arrays,              Only: vmax, vmin
   Use space,               Only: make_coors
   Use iofiles,             Only: cformat, icd, ipreco, itags, xfile, yfile
   Use fields,              Only: xfield, yfield
   Use pcs,                 Only: icco, mcc, mxe, mye, ncc, nxe, nye, ieofx, ieofy
   Use season,              Only: icsv, ispi, lprst
   Use missing,             Only: immx, ipmx, ipvx, immy, ipmy, ipvy, nnsx, nnsy, xmiss, ymiss
   Use categories,          Only: ithr, pthr, refs, thr
   Use climate,             Only: climate_per, ieclim
   Use analysis,            Only: ianal, icalc
#if GUI == 1
   Use markers,             Only: gmarker
   Use graphics,            Only: gszs, icross, imskld, imsklk, iscore, ishdland, ishdsea, itsh, ivl, ipis
#endif
   Use bootstrap,           Only: clb, ibcl, ipval, nboot, nperm
!
! Function result
   Integer :: opts
!
! Scalars
!
! Integer scalars
   Integer :: ihp1 = 1  ! - property sheet 1 (Configuration) -
   Integer :: ihp2 = 2  ! - property sheet 2 (Data) -
   Integer :: ihp3 = 3  ! - property sheet 3 (Missing Values) -
   Integer :: ihp4 = 4  ! - property sheet 4 (Climatology) -
   Integer :: ihp5 = 5  ! - property sheet 5 (Forecasts) -
   Integer :: ihp6 = 6  ! - property sheet 6 (Verification) -
   Integer :: ihp7 = 7  ! - property sheet 6 (Output) -
#if GUI == 1
   Integer :: ihp8 = 8  ! - property sheet 8 (Screen and Graphics) -
#endif
   Integer :: itab      ! - tab position -
   Integer :: j         ! - category index -
!
   Character(Len=   32) :: cout         ! - output -
   Character(Len=llab6) :: c_conflev    ! - 'Confidence level' label -
   Character(Len=llab6) :: c_cvn2       ! - 'DOuble cross-validation' label -
   Character(Len=llab6) :: c_domx       ! - 'X domain' label -
   Character(Len=llab6) :: c_domy       ! - 'Y domain' label -
   Character(Len=llab6) :: c_drymask    ! - 'Dry mask' label -
   Character(Len=llab6) :: c_ensave     ! - 'Ensemble average' label -
#if GUI == 0
   Character(Len=llab6) :: c_errhand    ! - 'Error handling' label -
#endif
   Character(Len=llab6) :: c_goodness   ! - 'Goodness index' label -
   Character(Len=llab6) :: c_seqs       ! - time sequencing label -
   Character(Len=llab6) :: c_skillmask  ! - 'Skill mask' label -
   Character(Len=llab6) :: c_sortens    ! - 'Sort ensembles' label -
   Character(Len=llab6) :: c_ssnlims    ! - 'Season limits' label -
   Character(Len=llab6) :: c_stdopt     ! - 'Standardization option' label -
   Character(Len=llab6) :: c_stdoptr    ! - 'Run-time standardization option' label -
   Character(Len=llab6) :: c_synchps    ! - 'Synchronous predictors' label -
   Character(Len=llab6) :: c_timesets   ! - 'Time settings' label -
   Character(Len=llab6) :: c_transformy ! - 'Transform Y' label -
   Character(Len=llab6) :: c_zero       ! - 'Zero bound' label -
!
   Logical :: lslim_bk ! - backup use season limits? -
!
   Type(pprd) :: ssn ! - season -
!
! Local arrays
   Character(Len=llab6), Dimension(xfile%nfs) :: c_domsx ! - X domain labels -
   Character(Len=llab6), Dimension(yfile%nfs) :: c_domsy ! - Y domain labels -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
#if GUI == 0
! Create window
   Call init_win (title=Trim(l_showopts%c), &
        lresize=.true.)
!
#endif
! Set tab position
   Select Case (ialphabet(ilang))
    Case Default
      itab = 55
    Case (i_cyrillic)
      itab = 70
   End Select
!
! Configuration
   c_lab = make_label('U', l_config, &
           lak=.false.)
   Call init_psheet (ihp1, Trim(c_lab))
   Call box_open (.false., .true.)
! - CPT version -
   Call box_open (.true., .false., &
        title=Trim(cg_version), icentre=-1)
   Call print_cvalue (Trim(cg_version), Trim(cpt_ver), &                  ! - CPT version -
        itab=itab)
   Call print_cvalue (Trim(l_lang%c), Trim(ca_lang(ilang)), &             ! - language -
        itab=itab)
   Call print_cvalue (Trim(cg_rootdir), Trim(rdir), &                     ! - root directory -
        itab=itab)
   Call box_close (.true.)
   Call gui_creturn (.false.)
#if GUI == 0
! - errors -
   Call box_open (.true., .false., &
        title=Trim(cg_errors), icentre=-1)
   c_errhand = make_label('S', l_errhand, &                               ! - error handling -
               lak=.false.)
   Call print_cvalue (Trim(c_errhand), Trim(ls_uerropts(iprompt+1)%c), &
        itab=itab)
   Call box_close (.true.)
   Call gui_creturn (.false.)
#endif
! - time settings -
   Call box_open (.true., .false., &
        title=Trim(cg_time), icentre=-1)
   c_timesets = make_label('S', l_timesets, &                             ! - months -
                lak=.false.)
   Call print_cvalue (Trim(c_timesets), Trim(ca_times(itime)), &
        itab=itab)
   If (ianal /= ia_pfv) Then
      c_synchps = make_label('S', l_synchps, &                            ! - allow synchronous predictors -
                  lak=.false.)
      Call print_yesno (Trim(c_synchps), isynch, &
           itab=itab)
   End If
   If (iseq /= 0) Then
      Call print_cvalue (Trim(cg_timeseq), Trim(ca_seqs(iuseq(iseq))), &  ! - time sequencing -
           itab=itab)
      Call print_yesno (Trim(cg_persist), lprst, &                        ! - persistence -
           itab=itab)
      If (istd == is_sti) Then
         Call print_ivalue (Trim(cg_lenspi), ispi, &                      ! - SPI period -
              itab=itab)
      End If
      If (lslim) Then
         lslim_bk = lslim
         lslim = .false.
         ssn%sdate = ssnlims%sdate
         ssn%edate = ssnlims%edate + date_diff(yfile%prd1%sdate, yfile%prd1%edate, iseq)
         c_ssnlims = get_cssn(ssn, 1, [1])
         lslim = lslim_bk
         j = Index(c_ssnlims, ' / ')
         If (j > 0) c_ssnlims(j+1:j+1) = '-'
         Call print_cvalue (Trim(cg_seasonlims), Trim(c_ssnlims), &
              itab=itab)
      End If
   End If
   Call box_close (.true.)
   Call gui_creturn (.false.)
! - analysis -
   Call box_open (.true., .false., &
        title=Trim(l_analysis%c), icentre=-1)
   Call print_cvalue (Trim(l_analysis%c), Trim(ls_analyses(ianal)%c), &
        itab=itab)
   Select Case (ianal)
    Case (ia_pcr, ia_mlr, ia_gcm)
      Call print_cvalue (Trim(cg_model), Trim(ca_regrs(iregr)), &                ! - regression model -
           itab=itab)
      Call print_cvalue (Trim(cg_linkf), Trim(ca_links(ilink)), &                ! - link function -
           itab=itab)
   End Select
   If (ianal /= ia_pfv) Then
      c_goodness = make_label('S', l_goodness, &
                   lak=.false.)
      Call print_cvalue (Trim(c_goodness), Trim(ca_goodi(igood)), &              ! - goodness index -
           itab=itab)
   End If
   Call box_close (.true.)
   Call gui_creturn (.false.)
   If (ianal /= ia_pfv) Then
! - GCMs -
      Call box_open (.true., .false., &
           title=Trim(l_models%c), icentre=-1)
      If (ianal == ia_gcm) Then
         Call print_cvalue ('Model recalibration', Trim(ca_corrects(igcms+1)), & ! - GCM recalibration option -
              itab=itab)
         Call print_cvalue (Trim(cg_modelcomb), Trim(ca_mcs(imc+1)), &           ! - GCM combination -
              itab=itab)
      End If
      c_ensave = make_label('S', l_ensave, &                                     ! - ensemble average -
                 lak=.false.)
      Call print_yesno (Trim(c_ensave), ieav, &
           itab=itab)
      c_sortens = make_label('S', l_sortens, &                                   ! - ensemble average -
                  lak=.false.)
      Call print_yesno (Trim(c_sortens), isem, &
           itab=itab)
      Call box_close (.true.)
      Call gui_creturn (.false.)
   End If
! - close Configuration property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Data settings
   c_lab = make_label('U', l_data, &
           lak=.false.)
   Call init_psheet (ihp2, Trim(c_lab))
   Call box_open (.false., .true.)
! - training period -
   If (ianal /= ia_pfv) Then
      Call box_open (.true., .false., &
           title=Trim(cg_tperiod), icentre=-1)
      Call print_ivalue (Trim(cg_lcv), lcw, &                            ! - length of cross-validation window -
           itab=itab)
      c_cvn2 = make_label('S', l_cvn2, &                                 ! - double cross-validation -
              lak=.false.)
      Call print_yesno (Trim(c_cvn2), icv2, &
           itab=itab)
      If (iretro == 1) Then
         Call print_ivalue (Trim(cg_lenitp), nt1, &                      ! - length of initial training period -
              itab=itab)
         Call print_ivalue (Trim(cg_tperiodui), nretro, &                ! - training period update interval -
              itab=itab)
      End If
      Call box_close (.true.)
      Call gui_creturn (.false.)
! - data -
      Call box_open (.true., .false., &
           title=Trim(l_data%c), icentre=-1)
      c_transformy = make_label('S', l_transformy, &                     ! - transformation option -
                     lak=.false.)
      If (igauss == it_non) Then
         Call print_cvalue (Trim(c_transformy), Trim(l_no%c), &
              itab=itab)
      Else
         Call print_cvalue (Trim(c_transformy), Trim(ca_tos(igauss)), &
              itab=itab)
      End If
      c_stdopt = ca_stds_t(istd+1)                                       ! - standardization option -
      Call lowcase (c_stdopt(2:Len_Trim(c_stdopt)))
      Call print_cvalue (Trim(l_stdn%c), Trim(c_stdopt), &
           itab=itab)
      If (icalc == 1) Then
         c_stdoptr = Trim(l_stdn%c)//' ('//Trim(cg_runtime_l)//')'
         Call print_cvalue (Trim(c_stdoptr), Trim(ca_stds_t(istdr+1)), & ! - run-time standardization option -
              itab=itab)
      End If
      If (iregr == ir_ols) Then
         c_zero = make_label('S', l_zerob, &                             ! - zero-bound -
                  lak=.false.)
         Call print_yesno (Trim(c_zero), izero, &
              itab=itab)
      Else If (icalc == 1) Then
         Call print_rvalue ('Lower bound', vmin, &                       ! - data limits -
              itab=itab, idec=1)
         Call print_rvalue ('Upper bound', vmax, &
              itab=itab, idec=1)
      End If
      c_drymask = make_label('S', l_drymask, &                           ! - dry masking -
              lak=.false.)
      Call print_yesno (Trim(c_drymask), idry, &
           itab=itab)
      If (idry /= 0) Then
         Call print_rvalue (Trim(c_drymask), rdry, &                     ! - dry mask -
              itab=itab)
      End If
      Call box_close (.true.)
      Call gui_creturn (.false.)
   End If
   Select Case (ianal)
    Case (ia_gcm, ia_pfv)
      Call box_open (.true., .false., &
           title=Trim(cg_interpn), icentre=-1)
      Call print_cvalue (Trim(cg_interpn), Trim(ca_interps(intp+1)), &   ! - interpolation option -
           itab=itab)
      Call box_close (.true.)
      Call gui_creturn (.false.)
   End Select
! - domain -
   If (xfile%igeog /= 0 .or. yfile%igeog /= 0) Then
      Call box_open (.true., .false., &
           title=Trim(l_datdom%c), icentre=-1)
      If (xfile%igeog /= 0) Then
         c_domx = Trim(l_datdom%c)//' (X)'
         Do j = 1, xfile%nfs
            c_domsx(j) = Trim(make_coors(xfield(j)%region%alim%rltn, xfield(j)%region%alim%rlgw))//' - '//&
                         Trim(make_coors(xfield(j)%region%alim%rlts, xfield(j)%region%alim%rlge))
            Call print_cvalue (Trim(c_domx), Trim(c_domsx(j)), &
                 itab=itab)
         End Do
      End If
      If (yfile%igeog /= 0) Then
         c_domy = Trim(l_datdom%c)//' (Y)'
         Do j = 1, yfile%nfs
            c_domsy(j) = Trim(make_coors(yfield(j)%region%alim%rltn, yfield(j)%region%alim%rlgw))//' - '//&
                         Trim(make_coors(yfield(j)%region%alim%rlts, yfield(j)%region%alim%rlge))
            Call print_cvalue (Trim(c_domy), Trim(c_domsy(j)), &
                 itab=itab)
         End Do
      End If
      Call box_close (.true.)
      Call gui_creturn (.false.)
   End If
! - modes settings -
      Select Case (ianal)
       Case (ia_cca, ia_pcr)
         Call box_open (.true., .false., &
              title=Trim(l_modes%c), icentre=-1)
         c_lab = Trim(cg_maxnof)//' '//Trim(l_modes%c)// ' (X)'                            ! - maximum number of X modes -
         Call print_ivalue (Trim(c_lab), mxe, &
              itab=itab)
         c_lab = Trim(cg_minnof)//' '//Trim(l_modes%c)// ' (X)'                            ! - minimum number of X modes -
         Call print_ivalue (Trim(c_lab), nxe, &
              itab=itab)
         If (ianal == ia_cca) Then
            c_lab = Trim(cg_maxnof)//' '//Trim(l_modes%c)// ' (Y)'                         ! - maximum number of Y modes -
            Call print_ivalue (Trim(c_lab), mye, &
                 itab=itab)
            c_lab = Trim(cg_minnof)//' '//Trim(l_modes%c)// ' (Y)'                         ! - minimum number of Y modes -
            Call print_ivalue (Trim(c_lab), nye, &
                 itab=itab)
            c_lab = Trim(cg_maxnof)//' '//Trim(l_modes%c)// ' ('//ca_atypes_a(ia_cca)//')' ! - maximum number of CCA modes -
            Call print_ivalue (Trim(c_lab), mcc, &
                 itab=itab)
            c_lab = Trim(cg_minnof)//' '//Trim(l_modes%c)// ' ('//ca_atypes_a(ia_cca)//')' ! - minimum number of CCA modes -
            Call print_ivalue (Trim(c_lab), ncc, &
                 itab=itab)
         End If
         Call print_cvalue ('X EOF option', Trim(ca_pccos(ieofx)), &                       ! - X EOF calculation option - 
              itab=itab)
         If (ianal == ia_cca) Then
            Call print_cvalue ('Y EOF option', Trim(ca_pccos(ieofy)), &                    ! - Y EOF calculation option -
                 itab=itab)
            Call print_yesno ('Ignore ''perfect'' CCA modes?', icco, &                     ! - ignore CCA modes with perfect correlation -
                 itab=itab)
         End If
         Call box_close (.true.)
         Call gui_creturn (.false.)
      End Select
! - close Data property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Missing values
   c_lab = make_label('U', l_missings, &
           lak=.false.)
   Call init_psheet (ihp3, Trim(c_lab))
   Call box_open (.false., .true.)
! - X-data / forecasts missing-values -
   If (ianal /= ia_pfv) Then
      Call box_open (.true., .false., &
           title=Trim(l_predictorx%c), icentre=-1)
   Else
      Call box_open (.true., .false., &
           title=Trim(l_fcasts%c), icentre=-1)
   End If
   Call print_rvalue (Trim(cg_missvalflag)//' (X)', xmiss, &                ! - X missing-value flag -
        itab=itab)
   Call print_ivalue (Trim(cg_mxpmissdat)//' (X)', ipmx, &                  ! - maximum % of X missing data -
        itab=itab)
   c_lab = 'Maximum % of missing '//Trim(ca_dstruct_l(xfile%idstr))//' (X)' ! - maximum % of X missing variables -
   Call print_ivalue (Trim(c_lab), ipvx, &
        itab=itab)
   Call print_ivalue (Trim(cg_nnearns)//' (X)', nnsx, &                     ! - number of X near-neighbours -
        itab=itab)
   Call print_ivalue (Trim(cg_missmethod)//' (X)', immx, &                  ! - method for replacing X missing values -
        itab=itab)
   Call box_close (.true.)
   Call gui_creturn (.false.)
! - Y-data / observations -
   If (ianal /= ia_pfv) Then
      Call box_open (.true., .false., &
           title=Trim(l_predictandy%c), icentre=-1)
   Else
      Call box_open (.true., .false., &
           title=Trim(cg_obs), icentre=-1)
   End If
   Call print_rvalue (Trim(cg_missvalflag)//' (Y)', ymiss, &                ! - Y missing-value flag -
        itab=itab)
   Call print_ivalue (Trim(cg_mxpmissdat)//' (Y)', ipmy, &                  ! - maximum % of Y missing data -
        itab=itab)
   c_lab = 'Maximum % of missing '//Trim(ca_dstruct_l(xfile%idstr))//' (Y)' ! - maximum % of Y missing variables -
   Call print_ivalue (Trim(c_lab), ipvy, &
        itab=itab)
   Call print_ivalue (Trim(cg_nnearns)//' (Y)', nnsy, &                     ! - number of Y near-neighbours -
        itab=itab)
   Call print_ivalue (Trim(cg_missmethod)//' (Y)', immy, &                  ! - method for replacing Y missing values -
        itab=itab)
   Call box_close (.true.)
! - close Missing Values property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Climatology settings
   c_lab = make_label('U', l_climatol, &
           lak=.false.)
   Call init_psheet (ihp4, Trim(c_lab))
   Call box_open (.false., .true.)
   Call box_open (.true., .false., &
        title=Trim(l_climatol%c), icentre=-1)
! - climatological period -
   Select Case (yfile%iseq)
    Case (isq_yr, isq_mn, isq_sn, isq_so) ! - yearly, seasonal, and monthly sequences -
      c_seqs = ca_seqs_l(iuseq(isq_yr))
    Case (1:) ! - daily, pentad, weekly, dekadal sequences -
      c_seqs = ca_seqs_l(iuseq(0))
    Case Default
      c_seqs = ca_seqs_l(iuseq(0))
   End Select
! - beginning of climatological period -
   If (Len_Trim(climate_per%clim1) > 0) Then
      Call print_cvalue ('First '//Trim(c_seqs), climate_per%clim1, &
           itab=itab)
   End If
! - end of climatological period -
   If (Len_Trim(climate_per%clim2) > 0) Then
      Call print_cvalue ('Last '//Trim(c_seqs), climate_per%clim2, &
           itab=itab)
   End If
! - climatological period extends beyond training period -
   Call print_yesno ('Can extend beyond training period', ieclim, &
        itab=itab)
! - seasonal aggregation -
   If (icsv > 0) Then
      Call print_cvalue ('Seasonal aggregations', ca_ssnagg(icsv), &
           itab=itab)
   End If
! - categories -
   Call print_cvalue (Trim(cg_threshtype), ca_thrs(ithr), &
        itab=itab)
! - thresholds -
   Select Case (ithr)
    Case (iv_ple) ! - percentile -
      Call print_rvalue (Trim(ls_cats(1)%c), pthr(1), &
           itab=itab)
      Call print_rvalue (Trim(ls_cats(ng)%c), pthr(2), &
           itab=itab)
    Case (iv_abs) ! - absolute thresholds -
      Do j = 1, nts
         Call print_rvalue (Trim(ls_threshs(j)%c), thr(j), &
              itab=itab)
      End Do
    Case (iv_ref) ! - references -
      Do j = 1, nts
         cout = get_cdate(refs(j), io_iso)
         Call print_cvalue (Trim(cg_ref), Trim(cout), &
              itab=itab)
      End Do
   End Select
   Call box_close (.true.)
! - close Climatology property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Forecast settings
   c_lab = make_label('U', l_fcasts, &
           lak=.false.)
   Call init_psheet (ihp5, Trim(c_lab))
   Call box_open (.false., .true.)
! - forecast settings -
   Call box_open (.true., .false., &
        title=Trim(l_fcasts%c), icentre=-1)
   Call print_cvalue ('Error variance', Trim(ca_evs(iev)), &                   ! - error-variance flag -
        itab=itab)
   c_conflev = cg_conflev
   Call lowcase(c_conflev)
   Call print_rvalue ('Prediction-interval, '//Trim(c_conflev)//' (%)', clf, & ! - confidence level -
        itab=itab)
   Call print_ivalue ('Number of ensemble forecasts', nenf, &                  ! - ensemble size -
        itab=itab)
   Call print_ivalue ('Forecast precision', iprecf, &                          ! - forecast precision -
        itab=itab)
   Call print_ivalue (Trim(cg_fpround), ifpr, &                                ! - forecast probability rounding -
        itab=itab)
   Call print_yesno ('Show as odds relative to climatology', iodds, &          ! - odds -
        itab=itab)
   Call print_yesno ('Show as non-exceedance probabilities', iexc, &           ! - exceedance probabilities -
        itab=itab)
   c_skillmask = make_label('S', l_skillmask, &                                ! - skill masking -
           lak=.false.)
   Call print_yesno (Trim(c_skillmask), iskmask, &
        itab=itab)
   If (iskmask /= 0) Then
      Call print_cvalue ('Masking correlation', ca_corrs(iskmaski), &          ! - masking correlation -
           itab=itab)
      Call print_rvalue (Trim(c_skillmask), rskmask, &                         ! - skill mask -
           itab=itab, idec=3)
   End If
   Call box_close (.true.)
! - close Forecasts property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Verification settings
   c_lab = make_label('U', l_verifn, &
           lak=.false.)
   Call init_psheet (ihp6, Trim(c_lab))
   Call box_open (.false., .true.)
! - verification settings -
   Call box_open (.true., .false., &
        title=Trim(l_verifn%c), icentre=-1)
   Call print_ivalue ('Number of probability bins', nb, &                  ! - number of probability bins -
        itab=itab)
   Call print_yesno ('Binned probabilities', ibs, &                        ! - number of probability bins -
        itab=itab)
   Call box_close (.true.)
   Call gui_creturn (.false.)
! - resampling settings -
   Call box_open (.true., .false., &
        title=Trim(l_resamples%c), icentre=-1)
   Call print_yesno ('Calculate confidence intervals', ibcl, &             ! - confidence intervals -
        itab=itab)
   Call print_yesno ('Calculate p-values', ipval, &                        ! - p-values -
        itab=itab)
   Call print_ivalue ('Number of permutations', nperm, &                   ! - number of permutations -
        itab=itab)
   Call print_ivalue ('Number of bootstrap samples', nboot, &              ! - number of bootstrap samples -
        itab=itab)
   Call print_rvalue (Trim(cg_conflev)//' (%)', clb, &                     ! - confidence level -
        itab=itab)
   Call box_close (.true.)
! - close Verification property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Output settings
   c_lab = Trim(cg_output)
   Call upcase(c_lab)
   Call init_psheet (ihp7, Trim(c_lab))
   Call box_open (.false., .true.)
   Call box_open (.true., .false., &
        title=Trim(cg_output), icentre=-1)
! - delimiters -
   Call print_cvalue ('Delimiter', Trim(ca_del(icd)), &
        itab=itab)
! - delimiters -
   Call print_yesno ('Include CPT tags', itags, &
        itab=itab)
! - output precision -
   If (ipreco >= 0) Then
      Call print_ivalue ('Decimal places', ipreco, &
           itab=itab)
   Else
      Call print_cvalue ('Decimal places', 'Automatic', &
           itab=itab)
   End If
! - output precision -
   Call print_cvalue ('Data precision', Trim(ca_prcs(cformat%iprc)), &
        itab=itab)
! - file format -
   Call print_cvalue ('File format', Trim(ca_ffmts(cformat%ifmt)), &
        itab=itab)
! - file access -
   Call print_cvalue ('File access', Trim(ca_faccs(cformat%iacc)), &
        itab=itab)
   Call box_close (.true.)
! - close Output property sheet
   Call box_close (.false.)
   Call window_end ()
!
#if GUI == 1
! Screen settings
   c_lab = Trim(cg_screen)//' '//Trim(cg_and_l)//' '//Trim(l_graphics%c)
   Call upcase(c_lab)
   Call init_psheet (ihp8, Trim(c_lab))
   Call box_open (.false., .true.)
! - screen settings -
   Call box_open (.true., .false., &
        title=Trim(cg_screen), icentre=-1)
   Call print_ivalue (Trim(l_console%c)//'- Height adjustment', icon_v, & ! - console height adjustment -
        itab=itab)
   Call print_ivalue (Trim(l_console%c)//'- Width adjustment', icon_h, &  ! - console width adjustment -
        itab=itab)
   Call print_rvalue (Trim(l_txtscal%c), dts_adj, &                       ! - text scaling -
        itab=itab)
   Call box_close (.true.)
   Call gui_creturn (.false.)
! - graphics options -
   Call box_open (.true., .false., &
        title=Trim(l_graphics%c), icentre=-1)
   Call print_rvalue (Trim(l_graphscal%c), gszs, &                        ! - graphics scaling -
        itab=itab)
   Call print_rvalue ('Default font-scaling', font1%fscale, &             ! - default font -
        itab=itab)
   Call print_rvalue ('Title font-scaling', font2%fscale, &               ! - title font -
        itab=itab)
   Call print_rvalue ('Numeric labels font-scaling', font3%fscale, &      ! - numeric-labels font -
        itab=itab)
   Call print_cvalue ('Station marker', Trim(ca_markers(gmarker%imrk)), & ! - station marker -
        itab=itab)
   Call print_rvalue ('Station-marker scaling', gmarker%rscale, &         ! - station-marker scaling -
        itab=itab)
   c_lab = make_label('S', l_crosses, &                                   ! - crosses on graph -
           lak=.false.)
   Call print_yesno (Trim(c_lab), icross, &
        itab=itab)
   c_lab = make_label('S', l_scorgrphs, &                                 ! - scores on graph -
           lak=.false.)
   Call print_yesno (Trim(c_lab), iscore, &
        itab=itab)
   c_lab = make_label('S', l_shadeland, &                                 ! - shade land -
           lak=.false.)
   Call print_yesno (Trim(c_lab), ishdland, &
        itab=itab)
   c_lab = make_label('S', l_shadesea, &                                  ! - shade sea -
           lak=.false.)
   Call print_yesno (Trim(c_lab), ishdsea, &
        itab=itab)
   c_lab = make_label('S', l_maskland, &                                  ! - mask land -
           lak=.false.)
   Call print_yesno (Trim(c_lab), imskld, &
        itab=itab)
   c_lab = make_label('S', l_masklake, &                                  ! - mask land -
           lak=.false.)
   Call print_yesno (Trim(c_lab), imsklk, &
        itab=itab)
   c_lab = make_label('S', l_bw, &                                        ! - black and white -
           lak=.false.)
   Call print_yesno (Trim(c_lab), icbw, &
        itab=itab)
   c_lab = make_label('S', l_reversecols, &                               ! - reverse colours -
           lak=.false.)
   Call print_yesno (Trim(c_lab), icrev, &
        itab=itab)
   c_lab = make_label('S', l_pintervals, &                                ! - prediction intervals on graph flag -
           lak=.false.)
   Call print_yesno (Trim(c_lab), ipis, &
        itab=itab)
   c_lab = make_label('S', l_vlines, &                                    ! - vertical lines on graph -
           lak=.false.)
   Call print_yesno (Trim(c_lab), ivl, &
        itab=itab)
   c_lab = make_label('S', l_catshade, &                                  ! - category shading on graph -
           lak=.false.)
   Call print_yesno (Trim(c_lab), itsh, &
        itab=itab)
   Call box_close (.true.)
! - close Screen property sheet
   Call box_close (.false.)
   Call window_end ()
!
! Create window
   Call init_win (title=Trim(l_showopts%c), &
        lresize=.true.)
   iw = winio$('%nl%nl%8ps&', ihp1, ihp2, ihp3, ihp4, ihp5, ihp6, ihp7, ihp8)
#endif
!
   iw = win_prompt(.false.)
!
   opts = 2
!
   Return
 End Function options_list
End Module menu_options
