! Author: Simon Mason
Module opts_prompts
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: get_boot_opts, get_climate_opts, get_cv_opts, get_exclude_opts, get_forecast_opts, get_gcm_opts, get_link_opts, &
             get_list_opts, get_masks_opts, get_missing_opts, get_mode_opts, get_regression_opts, get_tailoring_opts,        &
             get_time_opts, get_verif_opts
#if GUI == 1
   Public :: get_console_opts, get_gfont_opts, get_gmarker_opts, get_gsize_opts, get_text_opts
#else
   Public :: get_language
#endif
!
Contains
!
!
 Subroutine get_boot_opts (icalc, ihind, ibcl, ipval, nboot, nperm, clb, iw)
!
! Prompts for resampling settings. Specifically:
!   ibcl  ! - calculate bootstrap confidence limits flag -
!   ipval ! - p-values calculation flag -
!   nboot ! - size of bootstrap sample -
!   nperm ! - size of permutation sample -
!   clb   ! - bootstrap confidence level -
!
! Modules
   Use data_numbers, Only: rp, zero, oneh
   Use labels,       Only: cg_conflev, cg_perms, l_lab, l_resamples, l_scoresp, l_skillmaps, l_resamples, &
                           make_label
   Use gui,          Only: add_check_button, box_close, box_open, boxes_open, init_win, prompt_integer, prompt_real, &
                           win_prompt
#if GUI == 1
   Use gui,          Only: gui_creturn
#endif
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icalc ! - calculation completed flag (0=no, 1=yes) -
   Integer, Intent(In) :: ihind ! - hindcasts flag -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ibcl  ! - calculate bootstrap confidence limits flag -
   Integer, Intent(InOut) :: ipval ! - p-values calculation flag -
   Integer, Intent(InOut) :: nboot ! - size of bootstrap sample -
   Integer, Intent(InOut) :: nperm ! - size of permutation sample -
!
   Real(Kind=rp), Intent(InOut) :: clb ! - bootstrap confidence level -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for resampling settings
   Call init_win (title=Trim(l_resamples%c), &
        lresize=.true.)
! - probabilistic score confidence intervals / skill map p-values -
   Call boxes_open (1, 1 + (1 - icalc) + 2*ihind, .false., .true.)
   If (icalc == 0) Then
      Call box_open (.true., .false., &
           title=Trim(l_scoresp%c))
#if GUI == 1
      Call gui_creturn (.true.)
#endif
      Call add_check_button ('Calculate confidence intervals', ibcl, .true.)
      Call box_close (.true.)  ! - probabilistic score confidence intervals box -
      Call box_close (.false.) ! - formatting box -
   End If
! - skill map p-values -
   If (ihind == 1) Then
      l_lab%c = make_label('S', l_skillmaps)
      Call box_open (.true., .false., &
           title=Trim(l_lab%c))
#if GUI == 1
      Call gui_creturn (.true.)
#endif
      Call add_check_button ('Calculate p-values', ipval, .true.)
      Call box_close (.true.)  ! - skill-maps box -
      Call box_close (.false.) ! - formatting box -
! - permutation settings -
      Call box_open (.true., .false., &
           title=Trim(cg_perms))
      Call prompt_integer ('Number of permutations', nperm, &
           itab=32, ilow=100, lfocus=.true.)
      Call box_close (.true.)  ! - permutations box -
      Call box_close (.false.) ! - formatting box -
   End If
! - bootstrap settings -
   Call box_open (.true., .false., &
        title='Bootstrapping')
   Call prompt_integer ('Number of bootstrap samples', nboot, &
        itab=32, ilow=100, lfocus=.true.)
   Call prompt_real (Trim(cg_conflev)//' (%)', clb, &
        itab=32, elow=zero, ehgh=oneh, lfocus=.true.)
   Call box_close (.true.)  ! - bootstrapping box -
   Call box_close (.false.) ! - outer box -
! - prompt to confirm -
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_boot_opts
!
!
!
 Subroutine get_climate_opts (lsagg, nsag, isq, c_lim, c_loop, c_ssn1, c_ssn2, ieclim, iscs, clim_d1, clim_d2, dlimits, iw, fok)
!
! Prompts for climatological period. Specifically:
!   ieclim  ! - climatological period can extend beyond training period flag -
!   clim_d1 ! - first date of climatology -
!   clim_d2 ! - last date of climatology -
!   iscs    ! - seasonal calculation options flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
   Use labels,              Only: ca_ssnagg, cg_date1, cg_daten, cg_year1, cg_yearn, &
                                  l_climper
   Use time,                Only: pdate, &
                                  iseq
   Use gui,                 Only: add_check_button, box_close, box_open, ganged_rbs, init_win, print_text, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nsag ! - number of seasonal aggregations -
   Integer, Intent(In) :: isq  ! - time sequencing -
!
   Character(Len=*) :: c_lim  ! - current date limits -
   Character(Len=*) :: c_loop ! - end-of-year loop text -
   Character(Len=*) :: c_ssn1 ! - first season -
   Character(Len=*) :: c_ssn2 ! - last season -
!
   Logical, Intent(In) :: lsagg ! - prompt for seasonal aggregation -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ieclim ! - climatological period can extend beyond training period flag -
!
   Type(pdate), Intent(InOut) :: clim_d1 ! - first date of climatology -
   Type(pdate), Intent(InOut) :: clim_d2 ! - last date of climatology -
!
! - input/output arrays -
   Integer, Dimension(0:), Intent(InOut) :: iscs ! - seasonal calculation options flags -
!
   Type(pdate), Dimension(:), Intent(InOut) :: dlimits ! - date limits -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! - procedure arguments -
   Integer, External :: fok ! - function on selecting OK -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Trim
!
! Executable Statements
!
! Prompt for climatological period
   Call init_win (title=Trim(l_climper%c))
   Call print_text ('Please specify climatological period:', &
        lbold=.true.)
   If (Len_Trim(c_loop) > 0) Then
      Call print_text ('The year should be for the first month of the season.')
      Call print_text (Trim(c_loop))
   End If
! - set climatological limits to within training period if results are calculated -
   Call print_text (c_lim)
   Call print_text (' ')
   Select Case (isq)
    Case (isq_yr, isq_mn) ! - yearly, and monthly sequences -
      Call get_climate_dates (Trim(cg_year1), Trim(cg_yearn), isq_yr, dlimits(:), clim_d1, clim_d2)
    Case (isq_sn, isq_so) ! - seasonal sequences -
      Call get_climate_dates (Trim(cg_year1)//' ('//c_ssn1//')', &
                              Trim(cg_yearn)//' ('//c_ssn2//')', isq_yr, dlimits(:), clim_d1, clim_d2)
    Case (1:) ! - daily, pentad, weekly, dekadal sequences -
      Call get_climate_dates (Trim(cg_date1), Trim(cg_daten), iseq, dlimits(:), clim_d1, clim_d2)
   End Select
! - prompt for extension beyond training period -
   Call print_text (' ')
   Call add_check_button ('Climatological period can extend beyond training period?', ieclim, .false.)
! - prompt for seasonal aggregation -
   If (lsagg) Then
      Call print_text (' ')
      Call print_text (' ')
      Call box_open (.true., .false., &
           title='Seasonal aggregations')
      Call ganged_rbs (nsag, iscs(1:), &
           crbns=ca_ssnagg(:))
      Call box_close (.false.)
   End If
   iw = win_prompt(.true., &
        fok=fok)
!
   Return
!
 Contains
!
!
  Subroutine get_climate_dates (cprompt1, cprompt2, isq, dlimits, clim_d1, clim_d2)
!
! Prompts for date limits of climatological period
!
! Modules
   Use time, Only: pdate
   Use gui,  Only: prompt_pdate
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq ! - time sequencing -
!
   Character(Len=*), Intent(In) :: cprompt1 ! - prompt for first date -
   Character(Len=*), Intent(In) :: cprompt2 ! - prompt for last date -
!
! - input-output scalars -
   Type(pdate), Intent(InOut) :: clim_d1 ! - first date of climatology -
   Type(pdate), Intent(InOut) :: clim_d2 ! - last date of climatology -
!
! - input/output arrays -
   Type(pdate), Dimension(:), Intent(InOut) :: dlimits ! - date limits -
!
! Executable Statements
!
! Prompt for date limits of climatological period
   Call prompt_pdate (cprompt1, isq, clim_d1, &
        itab=20, dlimits=dlimits, lfocus=.true.)
#if GUI == 0
   dlimits(1) = clim_d1
#endif
   Call prompt_pdate (cprompt2, isq, clim_d2, &
        itab=20, dlimits=dlimits, lfocus=.true.)
!
   Return
  End Subroutine get_climate_dates
 End Subroutine get_climate_opts
!
!
!
#if GUI == 1
 Subroutine get_console_opts (icon_h, icon_v, iw)
!
! Prompts for console size adjustments. Specifically:
!   icon_h ! - console window width adjustment -
!   icon_v ! - console window height adjustment -
!
! Modules
   Use labels, Only: c_def=>c_lab, &
                     l_defaults, l_console, &
                     make_label
   Use gui,    Only: init_win, print_text, prompt_integer, win_prompt
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: icon_h ! - console window width adjustment -
   Integer, Intent(InOut) :: icon_v ! - console window height adjustment -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for console scaling
   Call init_win (title=Trim(l_console%c))
   Call print_text ('Set console size adjustments:', &
        lbold=.true.)
   Call print_text (' ')
   Call prompt_integer ('Height adjustment', icon_v, &
        itab=25, lbold=.false.)
   Call prompt_integer ('Width adjustment', icon_h, &
        itab=25, lbold=.false.)
   c_def = make_label('T', l_defaults, &
           lak=.true.)
   iw = win_prompt(.true., &
        cextra=Trim(c_def), iset1=icon_h, ival1=0, iset2=icon_v, ival2=0)
!
   Return
 End Subroutine get_console_opts
!
!
!
#endif
 Subroutine get_cv_opts (lcw, iw)
!
! Prompts for length of cross-validation period
!   lcw ! - length of cross-validation window -
!
! Modules
   Use labels,   Only: cg_lcv, &
                       l_cvopts
   Use gui,      Only: init_win, prompt_integer, win_prompt
   Use settings, Only: force_odd_lcw
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: lcw ! - length of cross-validation window -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Get cross-validation settings
   Call init_win (title=Trim(l_cvopts%c))
   Call prompt_integer (Trim(cg_lcv)//' (must be odd)', lcw, &
        ifly=2, ilow=1, fcb2=force_odd_lcw)
! - prompt to confirm -
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_cv_opts
!
!
!
#if GUI == 1
 Subroutine get_exclude_opts (nt, cxprds, iincl, iw)
#else
 Subroutine get_exclude_opts (nt, cxprds, kincl)
#endif
!
! Prompts to exclude specific observations and/or variables. Specifically:
!   iincl ! - inclusion indicators in the GUI version, and -
!   kincl ! - include training case? in the batch version -
!
! Modules
#if GUI == 0
   Use labels, Only: cg_done
#endif
   Use labels, Only: l_exclude, l_reset
   Use gui,    Only: init_win
#if GUI == 1
   Use gui,    Only: add_check_button, gui_creturn, print_text, win_prompt
#else
   Use gui,    Only: print_item, print_text, prompt_integer
#endif
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt ! - number of training cases -
!
! - input arrays -
   Character(Len=*), Dimension(:), Intent(In) :: cxprds ! - X training dates -
!
! - input/output arrays -
#if GUI == 1
   Integer, Dimension(:), Intent(InOut) :: iincl ! - inclusion indicators -
#else
   Logical, Dimension(:), Intent(InOut) :: kincl ! - include training case? -
#endif
!
#if GUI == 1
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
#endif
! Locals
!
! Local scalars
   Integer :: k ! - index of kincl -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt to exclude observationsi
1  Call init_win (title=Trim(l_exclude%c))
#if GUI == 1
   Call print_text ('Uncheck X-dates to be excluded')
   Do k = 1, nt
      Call add_check_button (cxprds(k), iincl(k), .false.)
      Call gui_creturn (.false.)
   End Do
! - prompt to confirm -
   iw = win_prompt(.true., &
        cextra=Trim(l_reset%c))
#else
   Call print_text ('Enter indices of dates to be excluded.')
   Do k = 1, nt
      If (kincl(k)) Call print_item (k, cxprds(k))
   End Do
   Call print_text (' ')
   Write (Unit=*, Fmt='(A)') ' 0. '//Trim(cg_done)
   Write (Unit=*, Fmt='(A)') '-1. '//Trim(l_reset%c)
   Call print_text (' ')
   Do
      Call prompt_integer (' ', k)
      Select Case (k)
       Case (0) ! - done -
         Exit
       Case (-1) ! - reset -
         kincl(:) = .true.
         GoTo 1
       Case Default
         If (k >= 1 .and. k <= nt) kincl(k) = .false.
         Cycle
      End Select
   End Do
#endif
!
   Return
 End Subroutine get_exclude_opts
!
!
!
 Subroutine get_forecast_opts (neva, ievi, clf, iexc, ifpr, iodds, iprecf, nenf, ieva, iw)
!
! Prompts for forecast options, Specifically:
!   iexc   ! - exceedance (0) or non-exceedance (1) probabilities -
!   ifpr   ! - forecast probability rounding -
!   iodds  ! - odds ratio flag -
!   iprecf ! - forecast precision -
!   nenf   ! - number of ensemble forecasts -
!   clf    ! - forecast confidence level -
!   ieva   ! - error-variance options flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers,       Only: rp, zero, oneh
   Use data_cpt_constants, Only: ievs
   Use labels,             Only: c_lab, ca_evs, cg_conflev, cg_pinterval_t, &
                                 l_exceedps, l_fcastsets, l_odds, &
                                 make_label
   Use gui,                Only: add_check_button, box_close, box_open, ganged_rbs, init_win, print_subtitle, prompt_integer, &
                                 gui_creturn, prompt_real, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: neva ! - available number of error-variance options -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ievi ! - available error-variance options indices -
!
! - input/output scalars -
   Integer, Intent(InOut) :: iexc   ! - exceedance (0) or non-exceedance (1) probabilities -
   Integer, Intent(InOut) :: ifpr   ! - forecast probability rounding -
   Integer, Intent(InOut) :: iodds  ! - odds ratio flag -
   Integer, Intent(InOut) :: iprecf ! - forecast precision -
   Integer, Intent(InOut) :: nenf   ! - number of ensemble forecasts -
!
   Real(Kind=rp), Intent(InOut) :: clf ! - forecast confidence level -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: ieva ! - error-variance options flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for forecast settings
   Call init_win (title=Trim(l_fcastsets%c), &
        lresize=.true.)
! - prediction confidence interval -
   Call box_open (.false., .false.)
   Call box_open (.true., .false., &
        title=Trim(cg_pinterval_t))
   Call print_subtitle ('Prediction interval width')
   Call prompt_real (Trim(cg_conflev)//' (%)', clf, &
        itab=32, elow=zero, ehgh=oneh, lfocus=.true., lbold=.false.)
! - error variance -
   Call print_subtitle ('Error variance')
   Call ganged_rbs (neva, ca_evs(ievi(:)), ieva(:), &
        ns=ievs(ievi(:)))
! - ensemble size -
   Call print_subtitle ('Ensemble size')
   Call prompt_integer ('Number of ensemble forecasts', nenf, &
        itab=32, ilow=1, lbold=.false.)
! - odds -
   Call print_subtitle (Trim(l_odds%c))
   Call add_check_button ('Show as odds relative to climatology', iodds, .false.)
   Call gui_creturn (.true.)
! - exceedance probabilities -
   c_lab = make_label('S', l_exceedps, &
           lak=.false.)
   Call print_subtitle (Trim(c_lab))
   Call add_check_button ('Show as non-exceedance probabilities', iexc, .false.)
   Call gui_creturn (.true.)
! - precision -
   Call print_subtitle ('Precision')
   Call prompt_integer ('Number of decimal places (Max 8)', iprecf, &
        itab=32, ilow=0, ihgh=8, lbold=.false.)
   Call prompt_integer ('Forecast probability rounding', ifpr, &
        itab=32, ilow=1, ihgh=10, lbold=.false.)
   Call box_close (.true.)
   Call box_close (.false.)
! - prompt to confirm -
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_forecast_opts
!
!
!
 Subroutine get_gcm_opts (intps, igcs, imcs, iw, ih)
!
! Sets GCM options. Specifically:
!   intps  ! - interpolation options flags (returned as array of 0s with the selected option as a 1) -
!   imcs   ! - model combination options flags (returned as array of 0s with the selected option as a 1) -
!   igcs   ! - correction options flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_cpt_constants, Only: ngc, ninterp, nmcs
   Use data_gui_constants, Only: i_handle
   Use labels,             Only: ca_corrects, ca_interps, ca_mcs, cg_selmethod, l_gcmopts
   Use gui,                Only: box_close, box_open, ganged_rbs, init_win, print_text, set_tabs, win_prompt
!
! Arguments
!
! Dummy arguments
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: intps  ! - interpolation options flags -
   Integer, Dimension(:), Intent(InOut) :: imcs   ! - model combination options flags -
   Integer, Dimension(:), Intent(InOut) :: igcs   ! - correction options flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
   Integer(i_handle), Intent(Out) :: ih ! - Windows handle -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for GCM options
   Call init_win (title=Trim(l_gcmopts%c), &
        ihandle=ih) ! - get window handle -
   Call set_tabs (1, [37])
! - interpolation option -
   Call box_open (.false., .false., &
        title='Model grid')
   Call print_text (Trim(cg_selmethod)//':')
   Call ganged_rbs (ninterp, intps(:), &
        crbns=ca_interps(:))
   Call box_close (.false.)
! - model correction option -
   Call box_open (.false., .false., &
        title='Model climatology')
   Call print_text (Trim(cg_selmethod)//':')
   Call ganged_rbs (ngc, igcs(:), &
        crbns=ca_corrects(:))
   Call box_close (.false.)
! - model combination -
   Call box_open (.false., .false., &
        title='Model combination')
   Call print_text (Trim(cg_selmethod)//':')
   Call ganged_rbs (nmcs, imcs(:), &
        crbns=ca_mcs(:))
   Call box_close (.false.)
! - ensemble counting -
!$$$$$$   Call box_open ('Ensemble forecasting', .false.)
!$$$$$$   Call print_text (Trim(cg_selmethod)//':')
!$$$$$$   Call ganged_rbs (nensc, ienscs, &
!$$$$$$        crbns=censc(:))
!$$$$$$   Call box_close (.false.)
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_gcm_opts
!
!
!
#if GUI == 1
 Subroutine get_gfont_opts (fscale1, fscale2, fscale3, iw)
!
! Prompts for graphics font scaling. Specifically:
!   fscale1 ! - scaling of default font -
!   fscale2 ! - scaling of title font -
!   fscale3 ! - scaling of numeric labels font -
!
! Modules
   Use data_numbers, Only: rp, zero, tenth
   Use labels,       Only: c_def=>c_lab, &
                           l_defaults, l_fntscal, &
                           make_label
   Use screen,       Only: reset_gfonts
   Use gui,          Only: init_win, print_text, prompt_real, win_prompt
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: fscale1 ! - scaling of default font -
   Real(Kind=rp), Intent(InOut) :: fscale2 ! - scaling of title font -
   Real(Kind=rp), Intent(InOut) :: fscale3 ! - scaling of numeric labels font -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for font scaling
   Call init_win (title=Trim(l_fntscal%c))
   Call print_text ('Set font-scaling:', &
        lbold=.true.)
   Call print_text (' ')
   Call prompt_real ('Default font', fscale1, &
        itab=25, rfly=tenth, elow=zero, lbold=.false.)
   Call prompt_real ('Title font',fscale2, &
        itab=25, rfly=tenth, elow=zero, lbold=.false.)
   Call prompt_real ('Numeric labels font', fscale3, &
        itab=25, rfly=tenth, elow=zero, lbold=.false.)
   c_def = make_label('T', l_defaults, &
           lak=.true.)
   iw = win_prompt(.true., &
        cextra=Trim(c_def), fextra=reset_gfonts)
!
   Return
 End Subroutine get_gfont_opts
!
!
!
 Subroutine get_gmarker_opts (nmrk, gscale, jmrks, iw)
!
! Prompts for station markers, Specifically
!   gscale ! - scaling of default font -
!   jmrks  ! - marker type flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers, Only: rp, zero, tenth
   Use labels,       Only: ca_markers, &
                           l_stnmrks
   Use gui,          Only: ganged_rbs, init_win, print_text, prompt_real, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nmrk ! - number of station markers -
!
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: gscale ! - scaling of default font -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: jmrks ! - marker type flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for station marker
   Call init_win (title=Trim(l_stnmrks%c))
   Call print_text ('Select station marker:', &
        lbold=.true.)
   Call print_text (' ')
   Call ganged_rbs (nmrk, jmrks(:), &
        crbns=ca_markers(:))
!
! Prompt for scaling
   Call print_text (' ')
   Call prompt_real ('Scaling', gscale, &
        rfly=tenth, elow=zero, lbold=.true.)
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_gmarker_opts
!
!
!
 Subroutine get_gsize_opts (gszs, iw)
!
! Prompts for graphics scaling. Specifically:
!   gszs ! - graphics size scaling (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers, Only: rp, zero, tenth
   Use labels,       Only: l_graphscal
   Use gui,          Only: init_win, prompt_real, win_prompt
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: gszs ! - graphics size scaling -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Backup current setting
   Call init_win (title=Trim(l_graphscal%c))
   Call prompt_real (Trim(l_graphscal%c), gszs, &
        rfly=tenth, elow=zero, lbold=.true.)
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_gsize_opts
!
!
!
#else
 Subroutine get_language (nlang, ilang)
!
! Prompts for language. Specifically:
!   ilang ! - language identifier -
!
! Modules
   Use data_text, Only: ca_lang
   Use labels,    Only: l_exit
   Use gui,       Only: prompt_integer
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlang ! - number of languages -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ilang ! - language identifier -
!
! Locals
!
! Local scalars
   Integer :: il ! - language index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for language
   Print *, ' '
   Do il = 1, nlang
      Write (Unit=*, Fmt='(1X,I1,2A)') il, '.  ', ca_lang(il)
   End Do
   Write (Unit=*, Fmt='(1X,A)') '0.  '//Trim(l_exit%c)
   Call prompt_integer (' ', ilang)
!
   Return
 End Subroutine get_language
!
!
!
#endif
 Subroutine get_link_opts (nlinks, ilinki, ilinks, iw)
!
! Prompts for link function. Specifically:
!
! Modules
   Use labels,   Only: ca_links, ca_regrs, cg_selmethod, l_linkfs
   Use gui,      Only: box_close, box_open, ganged_rbs, init_win, print_text, set_tabs, win_prompt
   Use settings, Only: ilink, iregr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlinks ! - number of available link functions -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ilinki ! - available error-variance options indices -
!
! - input/output arrays -
   Integer, Dimension(:) :: ilinks ! - link function options flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompts for link function
   Call init_win (title=Trim(l_linkfs%c))
   Call set_tabs (1, [37])
   Select Case (nlinks)
    Case (1)
      Call print_text ('Only the '//Trim(ca_links(ilinki(ilink)))//' link is currently available for '//Trim(ca_regrs(iregr))//'.')
    Case Default
      Call box_open (.false., .false., &
           title=Trim(l_linkfs%c))
      Call print_text (Trim(cg_selmethod)//':')
      Call ganged_rbs (nlinks, ca_links(ilinki(:)), ilinks(:), &
           ns=ilinki(:))
      Call box_close (.false.)
   End Select
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_link_opts
!
!
!
 Subroutine get_list_opts (ctitle, cdesc, nopt, copts, iopts, iw)
!
! Prompts for option from a list
!
! Modules
   Use gui, Only: ganged_rbs, init_win, print_text, win_prompt
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
   Integer, Dimension(:), Intent(InOut) :: iopts ! - opyion flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Executable Statements
!
! Prompt for option
   Call init_win (title=ctitle)
   Call print_text (cdesc)
   Call ganged_rbs (nopt, iopts(:), &
        crbns=copts(:))
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_list_opts
!
!
!
 Subroutine get_masks_opts (imask, icalc, idry, iskmask, rdry, rskmask, isopts, iw)
!
! Prompts for masking settings. Specifically:
!   idry    ! - dry-mask -
!   iskmask ! - skill-mask -
!   rdry    ! - dry-mask threshold -
!   rskmask ! - dry-mask threshold -
!   isopts  ! - flags of score options for masking (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers, Only: rp
   Use labels,       Only: l_masksets
   Use gui,          Only: box_close, boxes_open, init_win, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icalc ! - calculation completed flag (0=no, 1=yes) -
   Integer, Intent(In) :: imask ! - masking option -
!
! - input/output scalars -
   Integer, Intent(InOut) :: idry    ! - dry-mask -
   Integer, Intent(InOut) :: iskmask ! - skill-mask -
!
   Real(Kind=rp), Intent(InOut) :: rdry    ! - dry-mask threshold -
   Real(Kind=rp), Intent(InOut) :: rskmask ! - dry-mask threshold -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: isopts ! - flags of score options for masking -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Executable Statements
!
! Prompt for masking settings
   Call init_win (title=Trim(l_masksets%c))
! - prompt for dry mask -
   Select Case (imask)
    Case (0)
      Call boxes_open (1, 2, .true., .false.)
      If (icalc == 0) Call options_masks_dry (idry, rdry)
      Call box_close (.false.)
      Call options_masks_skill (iskmask, rskmask, isopts(:))
      Call box_close (.false.)
    Case (1)
      Call options_masks_dry (idry, rdry)
    Case (2)
      Call options_masks_skill (iskmask, rskmask, isopts(:))
   End Select
! - prompt to confirm -
   iw = win_prompt(.true.)
!
   Return
!
 Contains
!
!
  Subroutine options_masks_dry (idry, rdry)
!
! Prompts for dry-masking settings. Specifically:
!   idry ! - dry-mask -
!   rdry ! - dry-mask threshold -
!
! Modules
   Use data_numbers, Only: rp
   Use labels,       Only: l_drymask, l_lab, &
                           make_label
   Use gui,          Only: add_check_button, box_close, box_open, prompt_real
#if GUI == 1
   Use gui,          Only: gui_creturn
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: idry ! - dry-mask -
!
   Real(Kind=rp), Intent(InOut) :: rdry ! - dry-mask threshold -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for dry-masking settings
   l_lab%c = make_label('S', l_drymask)
   Call box_open (.true., .false., &
        title=Trim(l_lab%c))
#if GUI == 1
   Call gui_creturn (.true.)
#endif
   Call add_check_button (Trim(l_lab%c)//'?', idry, .true.)
#if GUI == 1
   Call gui_creturn (.true.)
   Call gui_creturn (.true.)
#else
   If (idry /= 0) Then
#endif
      Call prompt_real (Trim(l_lab%c), rdry)
#if GUI == 0
   End If
#endif
   Call box_close (.true.)
!
   Return
  End Subroutine options_masks_dry
!
!
!
  Subroutine options_masks_skill (iskmask, rskmask, isopts)
!
! Prompts for skill-masking settings. Specifically:
!   iskmask ! - skill-mask -
!   rskmask ! - dry-mask threshold -
!   isopts  ! - flags of score options for masking (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ncorr
   Use labels,             Only: ca_corrs, l_skillmask, l_lab, &
                                 make_label
   Use gui,                Only: add_check_button, box_close, box_open, ganged_rbs, print_text, prompt_real
#if GUI == 1
   Use gui,                Only: gui_creturn
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iskmask ! - skill-mask -
!
   Real(Kind=rp), Intent(InOut) :: rskmask ! - dry-mask threshold -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: isopts ! - flags of score options for masking -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for skill-masking settings
   l_lab%c = make_label('S', l_skillmask)
   Call box_open (.true., .false., &
        title=Trim(l_lab%c))
#if GUI == 1
   Call gui_creturn (.true.)
#endif
   Call add_check_button (Trim(l_lab%c)//'?', iskmask, .true.)
#if GUI == 1
   Call gui_creturn (.true.)
   Call gui_creturn (.true.)
#else
   If (iskmask /= 0) Then
#endif
      Call print_text ('Select score for masking:')
      Call ganged_rbs (ncorr, isopts(:), &
           crbns=ca_corrs(1:ncorr))
#if GUI == 1
      Call gui_creturn (.true.)
#endif
      Call prompt_real (Trim(l_lab%c), rskmask)
#if GUI == 0
   End If
#endif
   Call box_close (.true.)
!
   Return
  End Subroutine options_masks_skill
 End Subroutine get_masks_opts
!
!
!
 Subroutine get_missing_opts (imissf, ianal, idstrx, idstry, ipmx, ipvx, nnsx, rmissx, irmx, ipmy, ipvy, nnsy, rmissy, irmy, iw)
!
! Prompts for missing-value settings and options. Specifically:
!   ipmx   ! - Maximum % of X missing data -
!   ipmy   ! - Maximum % of Y missing data -
!   ipvx   ! - Maximum % of X missing variables -
!   ipvy   ! - Maximum % of Y missing variables -
!   nnsx   ! - number of X near-neighbours -
!   nnsy   ! - number of Y near-neighbours -
!   rmissx ! - X missing-value flag -
!   rmissy ! - Y missing-value flag -
!   irmx   ! - X missing-value replacement option flags (returned as array of 0s with the selected option as a 1) -
!   irmy   ! - Y missing-value replacement option flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ia_pfv, ifile_x, ifile_y
   Use labels,             Only: l_fcasts, l_missings, l_obs, l_predictandy, l_predictorx
#if GUI == 1
   Use gui,                Only: print_text
#endif
   Use gui,                Only: box_close, boxes_open, init_win, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imissf ! - missing data file identifier -
   Integer, Intent(In) :: ianal  ! - analysis identifier -
   Integer, Intent(In) :: idstrx ! - X data structure -
   Integer, Intent(In) :: idstry ! - Y data structure -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ipmx ! - Maximum % of X missing data -
   Integer, Intent(InOut) :: ipmy ! - Maximum % of Y missing data -
   Integer, Intent(InOut) :: ipvx ! - Maximum % of X missing variables -
   Integer, Intent(InOut) :: ipvy ! - Maximum % of Y missing variables -
   Integer, Intent(InOut) :: nnsx ! - number of X near-neighbours -
   Integer, Intent(InOut) :: nnsy ! - number of Y near-neighbours -
!
   Real(KIND=rp), Intent(InOut) :: rmissx ! - X missing-value flag -
   Real(KIND=rp), Intent(InOut) :: rmissy ! - Y missing-value flag -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: irmx ! - X missing-value replacement option flags -
   Integer, Dimension(:), Intent(InOut) :: irmy ! - Y missing-value replacement option flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for missing-value options
   Call init_win (title=Trim(l_missings%c))
!
! X variables
   If (imissf == 0 .or. imissf == ifile_x) Then
      Call boxes_open (2, 1, .true., .false.)
      If (ianal /= ia_pfv) Then
         Call missing_opts (Trim(l_predictorx%c), .true., idstrx, rmissx, ipmx, ipvx, nnsx, irmx(:))
      Else
         Call missing_opts (Trim(l_fcasts%c), .false., idstrx, rmissx, ipmx, ipvx, nnsx, irmx(:))
      End If
      Call box_close (.false.)
#if GUI == 1
      Call print_text (' ', &
           ladvance=.false.)
#endif
   End If
!
! Y variables
   If (imissf == 0 .or. imissf == ifile_y) Then
      If (ianal /= ia_pfv) Then
         Call missing_opts (Trim(l_predictandy%c), .true., idstry, rmissy, ipmy, ipvy, nnsy, irmy(:))
      Else
         Call missing_opts (Trim(l_obs%c), .true., idstry, rmissy, ipmy, ipvy, nnsy, irmy(:))
      End If
      Call box_close (.false.)
   End If
   iw = win_prompt(.true.)
!
   Return
!
 Contains
!
!
  Subroutine missing_opts (title, lreplace, idstr, rmiss, ipmv, ipvv, nnsv, irmv)
!
! Prompts for missing-value options. Specifically:
!   ipmv  ! - Maximum % of missing data -
!   ipvv  ! - Maximum % of missing variables -
!   nnsv  ! - number of near-neighbours -
!   rmiss ! - missing-value flag -
!   irmv  ! - missing-value replacement option flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ipm
   Use labels,             Only: ca_dstruct_l, cg_missmethod, cg_missvalflag, cg_mxpmissdat, cg_nnearns, cg_selmethod
   Use screen,             Only: iwdth
   Use gui,                Only: box_close, box_open, ganged_rbs, print_text, prompt_integer, prompt_real
#if GUI == 1
   Use gui,                Only: gui_creturn
#endif
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: idstr ! - data structure flag -
!
   Character(Len=*), Intent(In) :: title ! - title -
!
   Logical, Intent(In) :: lreplace ! - prompt for replacement options? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ipmv ! - Maximum % of missing data -
   Integer, Intent(InOut) :: ipvv ! - Maximum % of missing variables -
   Integer, Intent(InOut) :: nnsv ! - number of near-neighbours -
!
   Real(KIND=rp), Intent(InOut) :: rmiss ! - missing-value flag -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: irmv ! - missing-value replacement option flags -
!
! Locals
!
! Local scalars
   Integer :: itab ! - tab position -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for missing-value options
   Call box_open (.true., .false., &
        title=title)
   If (iwdth >= 1024) Then
      itab = 32
   Else
      itab = 24
   End If
   Call prompt_real (Trim(cg_missvalflag), rmiss, &
        itab=itab, iwid=10, lfocus=.true.)
   Call prompt_integer (Trim(cg_mxpmissdat), ipmv, &
        itab=itab, ilow=0, ihgh=ipm, iwid=10, lfocus=.true.)
   Call prompt_integer ('Maximum % of missing '//Trim(ca_dstruct_l(idstr)), ipvv, &
        itab=itab, ilow=0, ihgh=ipm, iwid=10, lfocus=.true.)
   If (lreplace) Then
      Call prompt_integer (Trim(cg_nnearns), nnsv, &
           itab=itab, ilow=1, iwid=10, lfocus=.true.)
#if GUI == 1
      Call gui_creturn (.true.)
#endif
      Call box_open (.false., .false., &
           title=Trim(cg_missmethod))
      Call print_text (Trim(cg_selmethod)//':')
      Call ganged_rbs (4, irmv, &
           crbns=['Long-term means     ', 'Long-term medians   ', 'Random numbers      ', 'Best near-neighbours'])
      Call box_close (.false.)
   End If
   Call box_close (.true.)
!
   Return
  End Subroutine missing_opts
 End Subroutine get_missing_opts
!
!
!
#if GUI == 1
 Subroutine get_mode_opts (ctitle, cxyc, ne, me, mm, iw, ih, fadvanced)
#else
 Subroutine get_mode_opts (ctitle, cxyc, ne, me, mm, iw, ih)
#endif
!
! Prompts for EOF options. Specifically:
!   ne ! - minimum number of modes -
!   me ! - maximum number of modes -
!   mm ! - maximum number of modes -
!
! Modules
   Use data_gui_constants, Only: i_handle
#if GUI == 1
   Use labels,             Only: l_advanced
#endif
   Use labels,             Only: cg_maxnof, cg_minnof, cg_nof, &
                                 c_lab=>c_lab, &
                                 l_modes, &
                                 make_label
   Use gui,                Only: box_close, box_open, init_win, prompt_integer, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctitle ! - title -
   Character(Len=*), Intent(In) :: cxyc   ! - X/Y/CCA mode -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ne ! - minimum number of modes -
   Integer, Intent(InOut) :: me ! - maximum number of modes -
   Integer, Intent(InOut) :: mm ! - maximum number of modes -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
   Integer(i_handle), Intent(Out) :: ih ! - handle for options window -
!
#if GUI == 1
! - procedure arguments -
   Integer, External :: fadvanced ! - advanced callback function -
!
#endif
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for mode options
   Call init_win (title=ctitle, &
        ihandle=ih)
   c_lab = make_label('L', l_modes)
   Call box_open (.false., .false., &
        title=Trim(cg_nof)//' '//Trim(c_lab)//' ('//Trim(cxyc)//')')
#if GUI == 1
   Call prompt_integer (Trim(cg_maxnof)//' '//Trim(c_lab), me, &
        itab=37, ilow=1, ihgh=mm)
#endif
   Call prompt_integer (Trim(cg_minnof)//' '//Trim(c_lab), ne, &
        itab=37, ilow=1, ihgh=mm)
#if GUI == 0
   Call prompt_integer (Trim(cg_maxnof)//' '//Trim(c_lab), me, &
        ilow=ne, ihgh=mm)
#endif
   Call box_close (.false.)
#if GUI == 1
   c_lab = make_label('T', l_advanced, &
           lak=.true.)
   iw = win_prompt(.true., &
        cextra=Trim(c_lab), fextra=fadvanced)
#else
   iw = win_prompt(.true.)
#endif
!
   Return
 End Subroutine get_mode_opts
!
!
!
 Subroutine get_regression_opts (nregr, iregrs, iw)
!
! Sets regression options. Specifically:
!   iregrs ! - regression options flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use labels, Only: ca_regrs, cg_selmethod, &
                     c_lab, &
                     l_advanced, l_regrmods, l_regropts, &
                     make_label
   Use gui,    Only: box_close, box_open, ganged_rbs, init_win, print_text, set_tabs, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nregr ! - number of regression options -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: iregrs ! - regression options flags -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for regression option
   Call init_win (title=Trim(l_regropts%c))
   Call set_tabs (1, [37])
   Call box_open (.false., .false., &
        title=Trim(l_regrmods%c))
   Call print_text (Trim(cg_selmethod)//':')
   Call ganged_rbs (nregr, iregrs(:), &
        crbns=ca_regrs(:))
   Call box_close (.false.)
   c_lab = make_label('T', l_advanced, &
           lak=.true.)
   iw = win_prompt(.true., &
        cextra=Trim(c_lab))
#if GUI == 0
   iw = 2
#endif
!
   Return
 End Subroutine get_regression_opts
!
!
!
 Subroutine get_tailoring_opts (nstds, nthrs, istds, ithrs, pthr, refs, thr, iw, fcheck, &
            dlimits)
!
! Prompts for threshold settings. Specifically:
!   istds ! - standardization options flags (returned as array of 0s with the selected option as a 1) -
!   ithrs ! - threshold options flags (returned as array of 0s with the selected option as a 1) -
!
! Modules
   Use data_numbers,       Only: rp, zero, one, onehth, sfmax
   Use data_cpt_constants, Only: ir_ols, ng, nts
#if GUI == 0
   Use data_cpt_constants, Only: iv_ple, iv_abs, iv_ref
   Use maths,              Only: get_flag
   Use labels,             Only: cg_threshtype
#endif
   Use labels,             Only: c_lab, &
                                 ca_stds_t, ca_thrs, &
                                 cg_ref, cg_selmethod, &
                                 l_probs, l_stdn, l_tailor, l_threshs, &
                                 ls_cats, ls_threshs, &
                                 make_label
   Use time,               Only: pdate, &
                                 iseq
   Use gui,                Only: box_close, box_open, boxes_open, ganged_rbs, init_win, print_text, prompt_pdate, prompt_real, &
                                 win_prompt
#if GUI == 1
   Use gui,                Only: radio_button
#endif
   Use settings,           Only: iregr
!
! Arguments
!
! Dummy arguments
! - input scalrs -
   Integer, Intent(In) :: nstds ! - number of standardization options -
   Integer, Intent(In) :: nthrs ! - number of threshold options -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: istds ! - standardization options flags -
   Integer, Dimension(:), Intent(InOut) :: ithrs ! - threshold options flags -
!
   Real(Kind=rp), Dimension(:), Intent(InOut) :: pthr ! - percentile thresholds -
   Real(Kind=rp), Dimension(:), Intent(InOut) :: thr  ! - absolute thresholds -
!
   Type(pdate), Dimension(nts), Intent(InOut) :: refs ! - reference dates -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! - procedure arguments -
   Integer, External :: fcheck
!
! Optional arguments
! - optional input arguments -
   Type(pdate), Dimension(:), Intent(In), Optional :: dlimits ! - date limits -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Locals
!
! Local scalars
   Integer :: j ! - threshold index -
!
   Character(Len=16) :: cref ! - reference year -
!
! Executable Statements
!
! Prompt for threshold settings
   Call init_win (title=Trim(l_tailor%c))
   Call boxes_open (1,2, .true., .false.)
! - standardization -
   If (iregr == ir_ols) Then
      Call box_open (.true., .false., &
           title=Trim(l_stdn%c))
      Call print_text (Trim(cg_selmethod)//':')
      Call ganged_rbs (nstds, istds(:), &
           crbns=ca_stds_t(:))
      Call box_close (.true.)
   End If
   Call box_close (.false.)
! - threshold types -
   Call box_open (.true., .false., &
        title=Trim(l_threshs%c))
   Call boxes_open (1, nthrs, .true., .false.)
#if GUI == 1
   Call ganged_rbs (nthrs, ithrs(:))
#else
   Call print_text (Trim(cg_threshtype))
   Call ganged_rbs (nthrs, ca_thrs(:), ithrs(:))
   Select Case (get_flag(ithrs(:)))
! - percentile thresholds -
    Case (iv_ple)
#endif
      Call box_open (.true., .false., &
           title=Trim(l_probs%c))
#if GUI == 1
      Call radio_button (Trim(ca_thrs(1)), ithrs(1))
#endif
      c_lab = make_label('L', ls_cats(ng))
      Call prompt_real ('     '//Trim(c_lab), pthr(2), &
           itab=35, iwid=10, rfly=onehth, rlow=zero, rhgh=one, lbold=.false.)
      c_lab = make_label('L', ls_cats(1))
      Call prompt_real ('     '//Trim(c_lab), pthr(1), &
           itab=35, iwid=10, rfly=onehth, rlow=zero, rhgh=one, lbold=.false.)
      Call box_close (.true.)
      Call box_close (.false.)
! - absolute thresholds -
#if GUI == 0
    Case (iv_abs)
#endif
      Call box_open (.true., .false., &
           title=Trim(ca_thrs(2)))
#if GUI == 1
      Call radio_button (Trim(ca_thrs(2)), ithrs(2))
#endif
      Do j = nts, 1, -1
         Call prompt_real ('     '//Trim(ls_threshs(j)%c), thr(j), &
              itab=35, iwid=10, rlow=-sfMax, rhgh=sfMax, lbold=.false.)
      End Do
      Call box_close (.true.)
      Call box_close (.false.)
! - references -
#if GUI == 0
    Case (iv_ref)
#endif
      Call box_open (.true., .false., &
           title=Trim(ca_thrs(3)))
#if GUI == 1
      Call radio_button (Trim(ca_thrs(3)), ithrs(3))
#else
1     Continue
#endif
      Do j = 1, nts
         Write (Unit=cref, Fmt='(5X,A,I2)') Trim(cg_ref), j
         Call prompt_pdate (cref, iseq, refs(j), &
              itab=39, dlimits=dlimits, lbold=.false., lfocus=.true.)
      End Do
      Call box_close (.true.)
      Call box_close (.false.)
#if GUI == 0
     If (fcheck() /= 0) GoTo 1
   End Select
#endif
   Call box_close (.true.)
   Call box_close (.false.)
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_tailoring_opts
!
!
!
#if GUI == 1
 Subroutine get_text_opts (dts_adj, iw)
!
! Prompts for text scaling. Specifically:
!   dts_adj ! - default text-size adjustment -
!
! Modules
   Use data_numbers, Only: rp, zero, tenth, one
   Use labels,       Only: c_def=>c_lab, &
                           l_defaults, l_txtscal, &
                           make_label
   Use gui,          Only: init_win, print_text, prompt_real, win_prompt
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: dts_adj ! - default text-size adjustment -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for font scaling
   Call init_win (title=Trim(l_txtscal%c))
   Call print_text ('Set text-scaling:', &
        lbold=.true.)
   Call print_text (' ')
   Call prompt_real ('Default text-scaling', dts_adj, &
        itab=25, rfly=tenth, elow=zero, lbold=.false.)
   c_def = make_label('T', l_defaults, &
           lak=.true.)
   iw = win_prompt(.true., &
        cextra=Trim(c_def), rset1=dts_adj, rval1=one)
!
   Return
 End Subroutine get_text_opts
!
!
!
#endif
 Subroutine get_time_opts (ntime, itimes, iw)
!
! Prompts for time settings. Specifically:
!   itimes ! - time options -
!
! Modules
   Use labels, Only: ca_times, &
                     l_timesets
   Use gui,    Only: ganged_rbs, init_win, print_text, win_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ntime ! - number of time settings -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: itimes ! - time options -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for time settings
   Call init_win (title=Trim(l_timesets%c))
   Call print_text ('Select option for lengths of months:')
   Call ganged_rbs (ntime, itimes(:), &
        crbns=ca_times(:))
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_time_opts
!
!
!
 Subroutine get_verif_opts (cnbs, ibi, ibs, iw)
!
! Prompts for verification options. Specifically:
!   ibi ! - number of probability bins identifier -
!   ibs ! - binned probabilities for verification scores? -
!
! Modules
   Use data_cpt_constants, Only: nbs, nbss
   Use labels,             Only: l_verifsets
   Use gui,                Only: init_win, print_text, prompt_boolean, prompt_list, win_prompt
!
! Arguments
!
! Dummy arguments
! - input arrays -
   Character(Len=*), Dimension(:) :: cnbs ! - number of probability bins -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ibi ! - number of probability bins identifier -
   Integer, Intent(InOut) :: ibs ! - binned probabilities for verification scores? -
!
! - output scalars -
   Integer, Intent(Out) :: iw ! - window argument -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for probability settings
   Call init_win (title=Trim(l_verifsets%c))
! - number of probability bins -
   Call prompt_list ('Number of probability bins', .false., cnbs(:), nbs, ibi, &
        ns=nbss(:))
   Call print_text (' ')
! - binned probabilities -
   Call prompt_boolean ('Calculate scores using binned probabilities', ibs)
! - prompt to confirm -
   iw = win_prompt(.true.)
!
   Return
 End Subroutine get_verif_opts
End Module opts_prompts
