! Author: Simon Mason
Module validate
!
! Modules
   Use data_numbers,       Only: rp
#if GUI == 1
   Use data_gui_constants, Only: i_handle
   Use graphics,           Only: image
#endif
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
#if GUI == 1
   Public :: init_img_validate
#else
   Public :: roc_cv, roc_c2, roc_ra, save_roc
#endif
   Public :: validate_cv, validate_c2, validate_ra, view_good
!
! Scalars
!
! Integer scalars
   Integer, Private :: ioutv0 ! - validation output unit number -
   Integer, Private :: ioutv1 ! - validation output unit number -
   Integer, Private :: ioutv2 ! - validation output unit number -
   Integer, Private :: ioutg1 ! - goodness index output unit number -
#if GUI == 1
   Integer, Private :: itab_adj = 0 ! - tab adjustment -
   Integer, Private :: iretw  ! - retroactive window? -
!
   Integer(Kind=i_handle), Private :: ih_g ! - goodness index window handle -
!
! Arrays
!
! Derived type scalars
   Type(image), Dimension(2), Private :: img_cv ! - cross-validated hindcasts and ROC graphs -
   Type(image), Dimension(2), Private :: img_c2 ! - double cross-validated hindcasts and ROC graphs -
   Type(image), Dimension(2), Private :: img_ra ! - retroactive hindcasts and ROC graph -
!
#endif
Contains
!
!
#if GUI == 1
 Subroutine init_img_validate (igsize)
!
! Initialises validation plots
!
! Modules
   Use graphics, Only: init_image
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igsize ! - graphics area dimension -
!
! Executable Statements
!
! Initialise graph sizes
   Call init_image (img_cv(1), igsize)
   Call init_image (img_cv(2), igsize)
   Call init_image (img_c2(1), igsize)
   Call init_image (img_c2(2), igsize)
   Call init_image (img_ra(1), igsize)
   Call init_image (img_ra(2), igsize)
!
   Return
 End Subroutine init_img_validate
!
!
!
#endif
 Function validate_cv() &
          Result (validate)
!
! Initialises for cross-validated predictions
!
#if GUI == 1
! Modules
   Use labels, Only: cg_roc_t
!
#endif
! Function result
   Integer :: validate
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Validate cross-validated forecasts
   iretw = 0
   img_cv(1)%title = 'Observations and Cross-Validated Hindcasts'
   img_cv(2)%title = Trim(cg_roc_t)
   validate = validates('Cross-validated scores', img_cv(:), plot_cv)
#else
   validate = validates('Cross-validated scores', update_cv)
#endif
!
   Return
 End Function validate_cv
!
!
!
 Function validate_c2() &
          Result (validate)
!
! Initialises for double cross-validated predictions
!
#if GUI == 1
! Modules
   Use labels, Only: cg_roc_t
!
#endif
! Function result
   Integer :: validate
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Validate cross-validated forecasts
   iretw = 0
   img_c2(1)%title = 'Observations and Double Cross-Validated Hindcasts'
   img_c2(2)%title = Trim(cg_roc_t)
   validate = validates('Double cross-validated scores', img_c2(:), plot_c2)
#else
   validate = validates('Double cross-validated scores', update_c2)
#endif
!
   Return
 End Function validate_c2
!
!
!
 Function validate_ra()
!
! Initialises for retroactive predictions
!
#if GUI == 1
! Modules
   Use labels, Only: cg_roc_t
!
#endif
! Function result
   Integer :: validate_ra
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Validate retroactive forecasts
   iretw = 1
   img_ra(1)%title='Observations and Retroactive Hindcasts'
   img_ra(2)%title = Trim(cg_roc_t)
   validate_ra = validates('Retroactive scores', img_ra(:), plot_ra)
#else
   validate_ra = validates('Retroactive scores', update_ra)
#endif
!
   Return
 End Function validate_ra
!
!
!
#if GUI == 1
 Function validates(ctitle, img_val, plot_validation)
!
! Initialises and creates a validation window
!
! Modules
   Use labels,      Only: l_axislimsy, l_hindplot, l_lab, l_rocdiag
   Use gui,         Only: set_cursor_waiting
   Use settings,    Only: lexpert, nu
   Use scores,      Only: init_dscores
   Use windows_gui, Only: imgid, &
                          init_window_graphic
!
! Function result
   Integer :: validates
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctitle ! - window title -
!
! - input/output arrays -
   Type(image), Dimension(:), Intent(InOut) :: img_val ! - hindcasts plot and ROC diagram -
!
! - procedure arguments -
   Integer, External :: plot_validation ! - plotting function -
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Initialise memory
   validates = 1
   If (init_dscores(nu) /= 0) Then
      Call set_cursor_waiting (0)
      Return
   End If
!
! Create results window
   If (lexpert) Then ! - expert mode -
      l_lab%c = ' '
      l_lab%iak = 0
      validates = init_window_graphic(2, img_val(:), imgid, ctitle, [l_hindplot, l_rocdiag], &
                  plot_validation, title_val, export_val,                                    &
                  cxy='Y', iloc=ioutv0, add_win_h=show_validations, limits_grph=limits_val, &
                  llims=[l_axislimsy, l_lab], f_help=help_valid, c_custom='Adjust tabs', f_custom=adjust_tabs)
   Else ! - beginner mode -
      validates = init_window_graphic(1, img_val(:), imgid, ctitle,[l_hindplot], &
                  plot_validation, title_val, export_val,                        &
                  cxy='Y', iloc=ioutv0, add_win_v=show_validations, limits_grph=limits_val, &
                  llims=[l_axislimsy], f_help=help_valid, c_custom='Adjust tabs', f_custom=adjust_tabs)
   End If
   validates = 1
!
   Return
 End Function validates
!
!
!
 Function adjust_tabs() &
          Result (adj)

!
! Prompts for console size adjustments
!
! Modules
   Use gui,      Only: iw, &
                       init_win, prompt_integer, win_prompt
   Use settings, Only: set_options
!
! Function result
   Integer :: adj
!
! Executable Statements
!
! Backup current setting
   adj = set_options(i1=itab_adj)
!
! Prompt for console scaling
   Call init_win (title='Tab adjustments')
   Call prompt_integer ('Tab adjustment', itab_adj, &
        itab=25, lbold=.false.)
   iw = win_prompt(.true.)
!
! Confirm changes
   adj = set_options(iw=iw, i1=itab_adj)
   adj = 1
!
   Return
 End Function adjust_tabs
!
!
!
#else
 Function validates(title, validations)
!
! Initialises and prints validation scores
!
! Modules
   Use gui,        Only: get_output_unit, init_win
   Use settings,   Only: nu
   Use scores,     Only: init_dscores
   Use cpt_output, Only: print_results
!
! Function result
   Integer :: validates
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: title ! - window title -
!
! - procedure arguments -
   Integer, External :: validations ! - validation function -
!
! Executable Statements
!
! Initialise memory
   validates = 1
   If (init_dscores(nu) /= 0) Return
!
! Print validation results
   Call init_win (title=title)
   ioutv0 = get_output_unit()
   validates = show_validations()
   validates = print_results(validations)
!
   Return
 End Function validates
!
!
!
#endif
 Function show_validations() &
          Result (show)
!
! Formats validation results
!
! Modules
   Use data_cpt_constants, Only: lbmscore, ndcat, ndcon
#if GUI == 1
   Use labels,             Only: l_pintervals, l_saveroc
#endif
   Use screen,             Only: iwdth
   Use gui,                Only: boxes_open, open_clearwin
#if GUI == 1
   Use gui,                Only: box_close, print_text
   Use menus,              Only: add_menu
#endif
   Use settings,           Only: lexpert
   Use scores,             Only: get_width_dscores
#if GUI == 1
   Use graphics,           Only: ipis
#endif
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: iwid ! - width of window -
   Integer :: ns1  ! - number of lines of scores (1 + number of scores) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Max
!
! Executable Statements
!
#if GUI == 1
! Add extra menu items
! - prediction intervals -
   If (iretw == 1) Then
      Call add_menu ('pm', 1, -1, l_pintervals, &
           icheck=ipis, itoggle=ipis, cbf=plot_hin_ra)
   End If
! - save ROC -
   Call add_menu ('pm', 1, 3, l_saveroc, &
        cbf=save_roc)
!
#endif
! Identify number of scores to show
   If (lexpert) Then ! - expert mode -
      ns1 = Max(ndcat, ndcon) + 1
   Else ! - beginner mode -
      ns1 = Max(Count(lbmscore(1:ndcon)), Count(lbmscore(ndcon+1:))) + 1
      Call boxes_open (2, 1, .true., .false.)
   End If
!
! Create validation statistics windows
   If (iwdth >= 768) Then
      iwid = get_width_dscores(itab=3)
   Else
      iwid = get_width_dscores(itab=0)
   End If
! - continuous forecast validation measures -
#if GUI == 1
   Call open_clearwin (iwid, ns1, .true., ioutv1, &
        cbox='Continuous measures')
   Call box_close (.false.) ! - close first row first column box -
   Call print_text (' ', &
        ladvance=.false.)
#else
   Call open_clearwin (iwid, ns1, .true., ioutv1)
#endif
! - categorical forecast validation measures -
#if GUI == 1
   Call open_clearwin (iwid, ns1, .true., ioutv2, &
        cbox='Categorical measures')
   Call box_close (.false.) ! - close first row second column box -
#else
   Call open_clearwin (iwid, ns1, .true., ioutv2)
#endif
   show = 1
!
   Return
 End Function show_validations
!
!
!
#if GUI == 1
 Function plot_cv() &
           Result (plot)
!
! Plots graphs for cross-validated predictions
!
! Modules
   Use settings, Only: lexpert
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Calculate new validation statistics
   plot = update_cv()
!
! Plot cross-validated graphs
   plot = plot_hin_cv()
   If (lexpert) plot = plot_roc_cv()
!
   Return
 End Function plot_cv
!
!
!
 Function plot_hin_cv() &
          Result (plot)
!
! Plots cross-validated hindcasts
!
! Modules
   Use arrays,     Only: y, yprds
   Use settings,   Only: iva, nt, nu
   Use regression, Only: ycv
   Use season,     OnlY: isnc
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot cross-validated graphs
   plot = plot_hin(img_cv(1), nt, nu,   y(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, &
                              nt, nu, ycv(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, 1)
!
   Return
 End Function plot_hin_cv
!
!
!
 Function plot_roc_cv() &
          Result (plot)
!
! Plots cross-validated ROC graph
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: nu
   Use scores,             Only: far, hit, roca
   Use plotting,           Only: plot_roc
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot cross-validated graphs
   plot = plot_roc(img_cv(2), nu, nts, hit, far, roca)
   plot = 2
!
   Return
 End Function plot_roc_cv
!
!
!
 Function plot_c2() &
           Result (plot)
!
! Plots graphs for double cross-validated predictions
!
! Modules
   Use settings, Only: lexpert
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Calculate new validation statistics
   plot = update_c2()
!
! Plot double cross-validated graphs
   plot = plot_hin_c2()
   If (lexpert) plot = plot_roc_c2()
!
   Return
 End Function plot_c2
!
!
!
 Function plot_hin_c2() &
          Result (plot)
!
! Plots double cross-validated hindcasts
!
! Modules
   Use arrays,     Only: y, yprds
   Use settings,   Only: iva, nt, nu
   Use regression, Only: pls_2, ycv2
   Use season,     OnlY: isnc
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot cross-validated graphs
   plot = plot_hin(img_c2(1), nt, nu,    y(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, &
                              nt, nu, ycv2(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, 1, &
                              rpls=pls_2(iva,1:nu))
!
   Return
 End Function plot_hin_c2
!
!
!
 Function plot_roc_c2() &
          Result (plot)
!
! Plots double cross-validated ROC graph
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: nu
   Use scores,             Only: far, hit, roca
   Use plotting,           Only: plot_roc
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot cross-validated graphs
   plot = plot_roc(img_c2(2), nu, nts, hit, far, roca)
   plot = 2
!
   Return
 End Function plot_roc_c2
!
!
!
 Function plot_ra() &
          Result (plot)
!
! Plots graphs for retroactive predictions
!
! Modules
   Use settings, Only: lexpert
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Calculate new validation statistics
   plot = update_ra()
!
! Plot retroactive graphs
   plot = plot_hin_ra()
   If (lexpert) plot = plot_roc_ra()
!
   Return
 End Function plot_ra
!
!
!
 Function plot_hin_ra() &
          Result (plot)
!
! Plots retroactive hindcasts
!
! Modules
   Use arrays,     Only: y, yprds
   Use settings,   Only: iva, nt, ntr, nt1, nu, nur
   Use regression, Only: pls_r, yret
   Use season,     Only: isnc
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot retroactive hindcasts
   plot = plot_hin(img_ra(1),  nt,  nu,    y(iva,1:nu, isnc), yprds(    1)%tprd(isnc)%sdate, &
                              ntr, nur, yret(iva,1:nur,isnc), yprds(nt1+1)%tprd(isnc)%sdate, nt1+1, &
                              rpls=pls_r(iva,1:nur))
!
   Return
 End Function plot_hin_ra
!
!
!
 Function plot_roc_ra() &
          Result (plot)
!
! Plots retroactive ROC graph
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: nur
   Use scores,             Only: far, hit, roca
   Use plotting,           Only: plot_roc
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot retroactive ROC
   plot = plot_roc(img_ra(2), nur, nts, hit, far, roca)
   plot = 2
!
   Return
 End Function plot_roc_ra
!
!
!
 Function plot_hin(img, n1, nu1, y, date1, n2, nu2, y2, date2, i2, &
          rpls) &
          Result (plot)
!
! Draws a hindcast plot
!
! Modules
   Use maths,   Only: interval
   Use labels,  Only: cg_black_l, cg_green_l, cg_grey_l, cg_hinds, cg_red_l, &
                      l_obs
   Use time,    Only: pdate, &
                      nse
   Use gui,     Only: icbw, &
                      set_cursor_waiting
  Use plotting, Only: plot_series
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1  ! - number of observations -
   Integer, Intent(In) :: nu1 ! - number of used observationss -
   Integer, Intent(In) :: n2  ! - number of hindcasts -
   Integer, Intent(In) :: nu2 ! - number of used hindcasts -
   Integer, Intent(In) :: i2  ! - offest for hindcasts -
!
   Type(pdate), Intent(In) :: date1 ! - first date of observations -
   Type(pdate), Intent(In) :: date2 ! - first date of hindcasts -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y  ! - observed values -
   Real(Kind=rp), Dimension(:), Intent(In) :: y2 ! - forecast values -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - hindcasts plot -
!
! Optional arguments
! - optional input arrays -
   Type(interval), Dimension(:), Intent(In), Optional :: rpls ! - prediction intervals -
!
! Locals
!
! Local scalars
   Character(Len=64) :: cylab ! - y-axis label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set appropriate y-axis label
   Call set_cursor_waiting (1)
   Select Case (icbw)
    Case (0)
      cylab = Trim(l_obs%c)//' ('//Trim(cg_red_l)//') / '//Trim(cg_hinds)//' ('//Trim(cg_green_l)//')'
    Case (1)
      cylab = Trim(l_obs%c)//' ('//Trim(cg_black_l)//') / '//Trim(cg_hinds)//' ('//Trim(cg_grey_l)//')'
   End Select
!
! Update hindcasts graph
   If (nse == 1) Then
      plot = plot_series(img, Trim(cylab), n1, nu1, y(:), date1, n2, nu2, y2(:), date2, i2, &
             y2is=rpls)
   Else
      plot = plot_series(img, Trim(cylab), n1, nu1, y(:), date1, n2, nu2, y2(:), date2, i2, &
             y2is=rpls, nse=nse)
   End If
!
   Return
 End Function plot_hin
#endif
!
!
!
 Function update_cv() &
          Result (update)
!
! Prepares for cross-validation results
!
! Modules
   Use settings,   Only: iva, nu
   Use arrays,     Only: y
   Use regression, Only: ycv
   Use season,     Only: isnc
   Use categories, Only: ifor_c, iobs_c, pobs
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Calculate validation statistics
   update = update_validations(nu, y(iva,:,isnc), ycv(iva,:,isnc), iobs_c(iva,:), ifor_c(iva,:), pobs(iva,:))
!
   Return
 End Function update_cv
!
!
!
 Function update_c2() &
          Result (update)
!
! Prepares for double cross-validation results
!
! Modules
   Use settings,   Only: iva, nu
   Use arrays,     Only: y
   Use regression, Only: ycv2
   Use season,     Only: isnc
   Use categories, Only: ifor_2, iobs_2, pobs
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Calculate validation statistics
   update = update_validations(nu, y(iva,:,isnc), ycv2(iva,:,isnc), iobs_2(iva,:), ifor_2(iva,:), pobs(iva,:))
!
   Return
 End Function update_c2
!
!
!
 Function update_ra() &
          Result (update)
!
! Prepares for retroactive results
!
! Modules
   Use settings,   Only: iva, nu, nur, nu1
   Use arrays,     Only: y
   Use regression, Only: yret
   Use season,     Only: isnc
   Use categories, Only: ifor_r, iobs_r, pobs
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Calculate validation statistics
   update = update_validations(nur, y(iva,nu1+1:nu,isnc), yret(iva,:,isnc), iobs_r(iva,:), ifor_r(iva,:), pobs(iva,:))
!
   Return
 End Function update_ra
!
!
!
 Function update_validations(n, y, yhat, iobs, ifor, cps) &
          Result (update)
!
! Calculates and prints results for current location
!
! Modules
   Use data_numbers,   Only: one
   Use progress_meter, Only: end_progress_meter, set_progress_increment, start_progress_meter
   Use fields,         Only: update_grid
   Use scores,         Only: score, &
                             calc_scores, print_dscores
!
! Function result
   Integer :: update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observed categories -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecast categories -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: y    ! - observed values -
   Real(Kind=rp), Dimension(:), Intent(In) :: yhat ! - forecast values -
   Real(Kind=rp), Dimension(:), Intent(In) :: cps  ! - climatological probabilities -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Initialise progress meter
   update = 1
#if GUI == 1
   Call start_progress_meter ('Calculating validation statistics', .true.)
#else
   Call start_progress_meter (' ', .true.)
#endif
   Call set_progress_increment (one, .false.)
!
! Construct and add coordinate label for current point
   Call update_grid (ioutv0)
!
! Calculate scores
   Call calc_scores (n, yhat(:), y(:), ifor(:), iobs(:), cps(:), score(:), ifail)
   If (ifail /= 0) Return
!
! Print scores
#if GUI == 1
   Call print_dscores (ioutv1, ioutv2, .false., score(:), &
        itab_adj=itab_adj)
#else
   Call print_dscores (ioutv1, ioutv2, .true., score(:))
#endif
!
! Update progress meter
#if GUI == 1
   Call end_progress_meter (.false., .true.)
#else
   Call end_progress_meter (.false., .false.)
#endif
   update = 0
!
   Return
 End Function update_validations
!
!
!
#if GUI == 1
 Function title_val() &
          Result (title)
!
! Updates graphic title
!
! Modules
   Use plotting, Only: get_title
!
! Function result
   Integer :: title
!
! Executable Statements
!
! Prompt for title
   title = replot_val(get_title)
!
   Return
 End Function title_val
!
!
!
 Function limits_val() &
          Result (limits)
!
! Prompts for axis limits
!
! Modules
   Use plotting, Only: get_limits
!
! Function result
   Integer :: limits
!
! Executable Statements
!
! Prompt for y-axis limits
   limits = replot_val(get_limits)
!
   Return
 End Function limits_val
!
!
!
 Function replot_val(get_custom) &
          Result (replot)
!
! Updates axis limits
!
! Modules
   Use plotting,    Only: replot_graph
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: replot
!
! Arguments
!
! Dummy arguments
! - procedure arguments -
   Interface
      Subroutine get_custom (img, lreset)
         Use graphics, Only: image
         Type(image), Intent(InOut) :: img ! - image -
         Logical, Intent(Out) :: lreset ! - reset? -
      End Subroutine get_custom
   End Interface
!
! Executable Statements
!
! Prompt for y-axis limits
   If (img_cv(1)%id == imgid) Then
      replot = replot_graph(img_cv(1), get_custom, plot_hin_cv)
   Else If (img_cv(2)%id == imgid) Then
      replot = replot_graph(img_cv(2), get_custom, plot_roc_cv)
   Else If (img_c2(1)%id == imgid) Then
      replot = replot_graph(img_c2(1), get_custom, plot_hin_c2)
   Else If (img_c2(2)%id == imgid) Then
      replot = replot_graph(img_c2(2), get_custom, plot_roc_c2)
   Else If (img_ra(1)%id == imgid) Then
      replot = replot_graph(img_ra(1), get_custom, plot_hin_ra)
   Else If (img_ra(2)%id == imgid) Then
      replot = replot_graph(img_ra(2), get_custom, plot_roc_ra)
   End If
!
   Return
 End Function replot_val
!
!
!
 Function export_val() &
          Result (export)
!
! Saves a plot
!
! Modules
   Use settings,    Only: iv
   Use iofiles,     Only: ny
   Use graphics,    Only: save_graphic
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: export
!
! Executable Statements
!
! Prompt for montage title
   If (imgid < 0) Then
      export = export_montage_val(-imgid)
      Return
   End If
!
! Save graph
   If (img_cv(1)%id == imgid) Then
      export = save_graphic('Hind_cv', img_cv(1), &
             imap=iv, mmap=ny)
   Else If (img_cv(2)%id == imgid) Then
      export = save_graphic('ROC_cv', img_cv(2), &
             imap=iv, mmap=ny)
   Else If (img_c2(1)%id == imgid) Then
      export = save_graphic('Hind_cv2', img_c2(1), &
             imap=iv, mmap=ny)
   Else If (img_c2(2)%id == imgid) Then
      export = save_graphic('ROC_cv2', img_c2(2), &
             imap=iv, mmap=ny)
   Else If (img_ra(1)%id == imgid) Then
      export = save_graphic('Hind_retro', img_ra(1), &
             imap=iv, mmap=ny)
   Else If (img_ra(2)%id == imgid) Then
      export = save_graphic('ROC_retro', img_ra(2), &
             imap=iv, mmap=ny)
   End If
!
   Return
 End Function export_val
!
!
!
 Function export_montage_val(imgid) &
          Result (export)
!
! Modules
   Use graphics, Only: export_montage
!
! Function result
   Integer :: export
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imgid ! - image ID -
!
! Executable Statements
!
! Save images
   If (img_cv(1)%id == imgid) Then
      export = export_montage('Cross_validated', img_cv(1), img_cv(2))
   Else If (img_c2(1)%id == imgid) Then
      export = export_montage('Double_cross_validated', img_c2(1), img_c2(2))
   Else If (img_ra(1)%id == imgid) Then
      export = export_montage('Retroactive', img_ra(1), img_ra(2))
   End If
!
   Return
 End Function export_montage_val
!
!
!
 Function help_valid()
!
! Modules
   Use help,        Only: help_file
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: help_valid
!
! Executable Statements
!
! Open context-specific help pages
   If (img_cv(1)%id == imgid .or. img_cv(2)%id == imgid .or. &
       img_c2(1)%id == imgid .or. img_c2(2)%id == imgid .or. &
       img_ra(1)%id == imgid .or. img_ra(2)%id == imgid) Then
      Call help_file ('CPT_use_results_validation.html')
   End If
   help_valid = 2
!
   Return
 End Function help_valid
#else
!
!
!
 Function roc_cv() &
          Result (roc)
!
! Prepares to print cross-validated ROC
!
! Modules
   Use settings, Only: nu
!
! Function result
   Integer :: roc
!
! Executable Statements
!
! Cross-validated ROC
   roc = print_roc('Cross-validated ROC', nu, update_roc_cv)
!
   Return
 End Function roc_cv
!
!
!
 Function roc_c2() &
          Result (roc)
!
! Prepares to print double cross-validated ROC
!
! Modules
   Use settings, Only: nu
!
! Function result
   Integer :: roc
!
! Executable Statements
!
! Cross-validated ROC
   roc = print_roc('Double cross-validated ROC', nu, update_roc_c2)
!
   Return
 End Function roc_c2
!
!
!
 Function roc_ra() &
          Result (roc)
!
! Prepares to print retroactive ROC
!
! Modules
   Use settings, Only: nur
!
! Function result
   Integer :: roc
!
! Executable Statements
!
! Retroactive ROC
   roc = print_roc('Retroactive ROC', nur, update_roc_ra)
!
   Return
 End Function roc_ra
!
!
!
 Function print_roc(title, n, update_roc)
!
! Prints ROC results
!
! Modules
   Use gui,        Only: init_win
#if GUI == 0
   Use gui,        Only: get_output_unit
#endif
   Use scores,     Only: close_dscores, init_dscores
   Use cpt_output, Only: print_results
!
! Function result
   Integer :: print_roc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Character(Len=*), Intent(In) :: title ! - window title -
!
! - procedure arguments -
   Integer, External :: update_roc ! - ROC function -
!
! Executable Statements
!
! Allocate workspace
   print_roc = 1
   If (init_dscores(n) /= 0) Return
#if GUI == 0
   ioutv0 = get_output_unit()
   ioutv1 = get_output_unit()
#endif
!
! Print ROC results
   Call init_win (title=title)
   print_roc = print_results(update_roc)
   print_roc = close_dscores()
!
   Return
 End Function print_roc
!
!
!
 Function update_roc_cv() &
          Result (update)
!
! Prepares for cross-validated ROC
!
! Modules
   Use settings,   Only: iva, nu
   Use regression, Only: ycv
   Use season,     Only: isnc
   Use categories, Only: iobs_c
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Calculate ROC
   update = update_roc(nu, iobs_c(iva,:), ycv(iva,:,isnc))
!
   Return
 End Function update_roc_cv
!
!
!
 Function update_roc_c2() &
          Result (update)
!
! Prepares for double cross-validated ROC
!
! Modules
   Use settings,   Only: iva, nu
   Use regression, Only: ycv2
   Use season,     Only: isnc
   Use categories, Only: iobs_2
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Calculate ROC
   update = update_roc(nu, iobs_2(iva,:), ycv2(iva,:,isnc))
!
   Return
 End Function update_roc_c2
!
!
!
 Function update_roc_ra() &
          Result (update)
!
! Prepares for retroactive ROC
!
! Modules
   Use settings,   Only: iva, nur
   Use regression, Only: yret
   Use season,     Only: isnc
   Use categories, Only: iobs_r
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Calculate ROC
   update = update_roc(nur, iobs_r(iva,:), yret(iva,:,isnc))
!
   Return
 End Function update_roc_ra
!
!
!
 Function update_roc(n, iobs, yhat) &
          Result (update)
!
! Calculates and prints ROC results for current location
!
! Modules
   Use data_cpt_constants, Only: ng
   Use labels,             Only: ca_cat_a
   Use arrays,             Only: rnkf=>rwk, &
                                 rank_data
   Use fields,             Only: update_grid
   Use scores,             Only: far, hit, roca, &
                                 roc
!
! Function result
   Integer :: update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observed categories -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: yhat ! - forecasts -
!
! Locals
!
! Local scalars
   Integer :: k ! - case index -
!
! Executable Statements
!
! Calculate ROC
   Call rank_data ('a', n, yhat(:), rnkf)
   Call roc (n, 1, iobs(:), rnkf(:), roca(1), hit(1:n,1), far(1:n,1))
   Call roc (n, ng, iobs(:), rnkf(:), roca(2), hit(1:n,2), far(1:n,2))
!
! Construct and add coordinate label for current point
   Call update_grid (ioutv0)
!
! Print ROC
   Write (Unit=ioutv1, Fmt='(A,F10.3)') ' ROC area ('//ca_cat_a( 1)//'):', roca(1)
   Write (Unit=ioutv1, Fmt='(A,F10.3)') ' ROC area ('//ca_cat_a(ng)//'):', roca(2)
   Write (Unit=ioutv1, Fmt=*)
   Write (Unit=ioutv1, Fmt='(4A)') '            Hit rates ('//ca_cat_a( 1)//')', '    False-alarm rates ('//ca_cat_a( 1)//')', &
                                  '            Hit rates ('//ca_cat_a(ng)//')', '    False-alarm rates ('//ca_cat_a(ng)//')'
   Do k = 1, n
      Write (Unit=ioutv1, Fmt='(4F25.3)') hit(k,1), far(k,1), hit(k,2), far(k,2)
   End Do
   Write (Unit=ioutv1, Fmt=*)
   update = 0
!
   Return
 End Function update_roc
#endif
!
!
!
 Function save_roc()
!
! Saves ROC results
!
! Modules
   Use iofiles,    Only: rofile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: save_roc
!
! Executable Statements
!
! Save requested results
   save_roc = save_results(ianal, rofile)
!
   Return
 End Function save_roc
!
!
!
 Function view_good()
!
! Creates results window for goodness index optimization results
!
! Modules
   Use labels,      Only: l_goodness
   Use errors,      Only: cpt_error
   Use analysis,    Only: lopt
   Use windows_gui, Only: init_window_graphic_0
!
! Function result
   Integer :: view_good
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
! Open results window
   If (Allocated(lopt)) Then
      view_good = init_window_graphic_0(l_goodness%c, update_good, show_good)
      Return
   End if
!
! Indicate if results are unavailable
   ifail = 1
   Call cpt_error ('view_good', .false., ifail)
   view_good = 2
!
   Return
 End Function view_good
!
!
!
 Function show_good() &
          Result (show)
!
! Creates ClearWin+ window for goodness index results
!
! Modules
   Use gui, Only: open_clearwin
!
! Function result
   Integer :: show
!
! Executable Statements
!
! Create results window
#if GUI == 1
   Call open_clearwin (88, 22, .true., ioutg1, &
        ih=ih_g, lvs=.true.)
#else
   Call open_clearwin (88, 22, .true., ioutg1)
#endif
   show = 1
!
   Return
 End Function show_good
!
!
!
 Function update_good() &
          Result (update)
!
! Prints goodness index optimization results
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm
   Use data_io_constants,  Only: io_com, lprd
   Use labels,             Only: ca_atypes_a, cg_na, cg_to_l, cg_tperiod, l_goodness
   Use time,               Only: Operator(+), &
                                 get_cdate
   Use gui,                Only: print_text
   Use settings,           Only: iretro, nret, nretro, nt, nt1
   Use iofiles,            Only: yfile
   Use pcs,                Only: mcc, mxe, mye, ncc, nxe, nye
   Use scores,             Only: gm
   Use analysis,           Only: ianal, lopt
!
! Function result
   Integer :: update
!
! Locals
!
! Local scalars
   Integer :: ir   ! - retroactice index -
   Integer :: irl  ! - index of last retroactive training-period date -
   Integer :: irmx ! - index of last retroactive training-period update -
   Integer :: ixe  ! - X EOF mode index -
   Integer :: iye  ! - Y EOF mode index -
   Integer :: icc  ! - current number of CCA modes -
   Integer :: nxo  ! - optimal number of X EOF modes -
   Integer :: nyo  ! - optimal number of Y EOF modes -
   Integer :: nco  ! - optimal number of CCA modes -
   Integer :: iopt ! - optimization step index -
   Integer :: jopt ! - index of optimized settings -
!
   Character(Len=lprd) :: cprd1 ! - period -
   Character(Len=lprd) :: cprd2 ! - period -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustR, Min, Trim
!
! Executable Statements
!
! Print headers for each retroactice step
   cprd1 = get_cdate(yfile%prd1 + (yfile%it1 - 1), 1, io_com)
   irl = nt1
   irmx = nret*iretro + 1
   Do ir = 1, irmx
      If (ir == irmx) Then
         cprd2 = get_cdate(yfile%prd1 + (yfile%it1 + nt - 2), 1, io_com)
      Else
         cprd2 = get_cdate(yfile%prd1 + (yfile%it1 + irl - 2), 1, io_com)
      End If
      Call print_text (Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2), &
           iout=ioutg1)
      Select Case (ianal)
       Case (ia_cca, ia_pcr)
         Call print_text (' ', &
              iout=ioutg1)
         Call print_text ('               CURRENT                         OPTIMUM', &
              iout=ioutg1)
         Call print_text (' ', &
              iout=ioutg1)
         Call print_text ('      Number of Modes    Goodness      Number of Modes    Goodness', &
              iout=ioutg1)
      End Select
      Select Case (ianal)
       Case (ia_cca)
         Call print_text ('         X    Y   '//ca_atypes_a(ia_cca)//'       Index'//&
                         &'         X    Y   '//ca_atypes_a(ia_cca)//'       Index', &
             iout=ioutg1)
       Case (ia_pcr)
         Call print_text ('         X                  Index         X                  Index', &
             iout=ioutg1)
      End Select
      Call print_text (' ', &
           iout=ioutg1)
      iopt = 0
      jopt = 1
!
! Print optimization
      Do ixe = nxe, mxe
         Do iye = nye, mye
            Do icc = ncc, Min(mcc, ixe, iye)
               iopt = iopt + 1
               If (lopt(iopt,ir)) Then
                  If (iopt == 1 .or. gm(iopt,ir) > gm(jopt,ir)) Then
                     jopt = iopt
                     nxo = ixe
                     nyo = iye
                     nco = icc
                  End If
                  Select Case (ianal)
                   Case (ia_cca)
                     Write (Unit=ioutg1, Fmt='(2(5X,2I5,I6,F12.3))') &
                                      ixe, iye, icc, gm(iopt,ir), nxo, nyo, nco, gm(jopt,ir)
                   Case (ia_pcr)
                     Write (Unit=ioutg1, Fmt='(2(5X,I5,11X,F12.3))') &
                                      ixe, gm(iopt,ir), nxo, gm(jopt,ir)
                  End Select
               Else
                  Select Case (ianal)
                   Case (ia_cca)
                     Write (Unit=ioutg1, Fmt='(5X,2I5,I6,A12,5X,2I5,I6,F12.3)') &
                                      ixe, iye, icc, AdjustR(cg_na), nxo, nyo, nco, gm(jopt,ir)
                   Case (ia_pcr)
                     Write (Unit=ioutg1, Fmt='(5X,I5,11X,A12,5X,I5,11X,F12.3)') &
                                      ixe, AdjustR(cg_na), nxo, gm(jopt,ir)
                   Case (ia_mlr, ia_gcm)
                     Write (Unit=ioutg1, Fmt='(A,F12.3)') &
                                      Trim(l_goodness%c)//':', gm(jopt,ir)
                  End Select
               End If
            End Do
         End Do
      End Do
      If (ir < nret*iretro + 1) Then
         Call print_text (' ', &
              iout=ioutg1)
         Call print_text (' ', &
              iout=ioutg1)
      End If
      irl = irl + nretro
   End Do
   update = 1
!
   Return
 End Function update_good
End Module validate
