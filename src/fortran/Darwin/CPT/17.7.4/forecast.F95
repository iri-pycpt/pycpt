! Author: Simon Mason
Module forecast
!
! Modules
   Use data_cpt_constants, Only: ng
   Use data_gui_constants, Only: i_handle
   Use graphics,           Only: image
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: change_fcast, fcst_fens, fcst_fps, fcst_fser, fcst_fval, fcst_odds, fcst_pexc, get_cv2_forecast, &
             parameter_write_forecast
#if GUI == 1
   Public :: fcst_hps, init_img_fcst, prompt_forecast
#endif
!
! Scalars
!
! Integer scalars
#if GUI == 1
   Integer, Private :: ifps    ! - forecast probabilities diagram index -
   Integer, Private :: ifvl    ! - forecast values diagram index -
   Integer, Private :: iodd    ! - forecast odds diagram index -
#endif
   Integer, Private :: ioutf0  ! - forecast output unit number -
   Integer, Private :: ioutf1  ! - forecast output unit number -
   Integer, Private :: ioutf2  ! - forecast output unit number -
   Integer, Private :: ioutf3  ! - forecast output unit number -
   Integer, Private :: ioutf4  ! - forecast output unit number -
   Integer, Private :: ioutf5  ! - forecast output unit number -
   Integer, Private :: ioutf6  ! - forecast output unit number -
   Integer, Private :: ioutf7  ! - forecast output unit number -
   Integer, Private :: ioutf8  ! - forecast output unit number -
   Integer, Private :: ioutf10 ! - forecast output unit number -
!
   Integer(Kind=i_handle), Private :: ih_f1  ! - forecasts window handle -
   Integer(Kind=i_handle), Private :: ih_f2  ! - forecast ranges window handle -
   Integer(Kind=i_handle), Private :: ih_f3  ! - map forecasts window handle -
   Integer(Kind=i_handle), Private :: ih_f4  ! - map forecasts window handle -
   Integer(Kind=i_handle), Private :: ih_f5  ! - map forecasts window handle -
   Integer(Kind=i_handle), Private :: ih_f8  ! - forecast ensembles window handle -
   Integer(Kind=i_handle), Private :: ih_f10 ! - forecast odds window handle -
!
! Character scalars
   Character(Len=32), Private :: cfmt ! - format statement -
!
! Derived type scalars
   Type(image), Dimension(1), Private :: img_pexc ! - probability of exceedance graph -
!
#if GUI == 1
! Derived type arrays
   Type(image), Dimension( 1), Private :: img_fser ! - forecasts graph -
   Type(image), Dimension( 1), Private :: img_fens ! - forecast ensemble graph -
   Type(image), Dimension( 3), Private :: img_fval ! - forecasts map -
   Type(image), Dimension( 3), Private :: img_hps  ! - probabilistic forecast map -
   Type(image), Dimension(ng), Private :: img_fps  ! - forecast probabilities maps -
   Type(image), Dimension(ng), Private :: img_odds ! - forecast odds map -
!
#endif
Contains
!
!
#if GUI == 1
 Subroutine init_img_fcst (igsize, ihsize)
!
! Initialises forecast images
!
! Modules
   Use graphics, Only: init_image
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igsize ! - graphics area dimension -
   Integer, Intent(In) :: ihsize ! - reduced graphics area dimension -
!
! Locals
!
! Local scalars
   Integer :: i ! - diagram index -
!
! Executable Statements
   Call init_image (img_fser(1), (igsize*4)/3)
   Call init_image (img_fens(1), (igsize*4)/3)
   Call init_image (img_pexc(1), (igsize*3)/2)
   Call init_image (img_hps(1), (igsize*3)/2)
   Do i = 1, 3
      Call init_image (img_fval(i), ihsize)
   End Do
   Do i = 1, ng
      Call init_image (img_fps(i), ihsize)
   End Do
   Do i = 1, ng
      Call init_image (img_odds(i), ihsize)
   End Do
!
   Return
 End Subroutine init_img_fcst
!
!
!
 Function reset_img_pexc() &
          Result (reset)
!
! Resets probability of exceedance images
!
! Modules
   Use data_numbers, Only: zero, one
!
! Function result
   Integer :: reset
!
! Executable Statements
!
! Set y-axis limits
   img_pexc(:)%lcontour = .false.
   img_pexc(:)%ymin = zero
   img_pexc(:)%ymax = one
   img_pexc(:)%scale = one
   reset = 0
!
   Return
 End Function reset_img_pexc
!
!
!
 Function reset_img_fps() &
          Result (reset)
!
! Resets forecast probability images
!
! Modules
   Use data_numbers,       Only: rp, zero, one, oneh
   Use data_cpt_constants, Only: id_grd, id_stn
   Use iofiles,            Only: yfile
!
! Function result
   Integer :: reset
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Set forecast probability contours
   img_fps(:)%lcontour= .true.
   Select Case (yfile%idstr)
    Case (id_grd, id_stn)
      img_fps(:)%ncs = 9
      Do i = 1, img_fps(1)%ncs
         img_fps(:)%contour(i) = Real(i*10, Kind=rp)
      End Do
   End Select
   img_fps(:)%ymin = zero
   img_fps(:)%ymax = oneh
   img_fps(:)%scale = one
   reset = 0
!
   Return
 End Function reset_img_fps
!
!
!
 Function reset_img_hps() &
           Result (reset)
!
! Modules
   Use data_numbers, Only: rp, one
!
! Function result
   Integer :: reset
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Set forecast probability contours
   img_hps(:)%lcontour = .true.
   img_hps(:)%ncs = 21
   Do i = 1, img_hps(1)%ncs/2-2
      img_hps(:)%contour(img_hps(1)%ncs/2-1-i) = Real(-35-i*5, Kind=rp)
   End Do
   img_hps(:)%contour(img_hps(1)%ncs/2-1) = Real(-33, Kind=rp)
   img_hps(:)%contour(img_hps(1)%ncs/2) = Real(40, Kind=rp)
   img_hps(:)%contour(img_hps(1)%ncs/2+1) = Real(45, Kind=rp)
   img_hps(:)%contour(img_hps(1)%ncs/2+2) = Real(50, Kind=rp)
   img_hps(:)%contour(img_hps(1)%ncs/2+3) = Real(133, Kind=rp)
   Do i = 1, img_hps(1)%ncs/2-2
      img_hps(:)%contour(img_hps(1)%ncs/2+3+i) = Real(135+i*5, Kind=rp)
   End Do
   img_hps(:)%scale = one
   reset = 0
!
   Return
 End Function reset_img_hps
!
!
!
#endif
 Function get_forecast() &
          Result (get)
!
! Checks whether forecasts need to be (re-)calculated
!
! Modules
   Use time,           Only: Operator(+), Operator(-), Operator(>), Operator(<)
   Use gui,            Only: set_cursor_waiting
   Use menus,          Only: set_menu_greying
   Use progress_meter, Only: lcalc, lstop
   Use errors,         Only: cpt_error
   Use settings,       Only: nf
   Use arrays,         Only: lconvert, t
   Use iofiles,        Only: zfile, &
                             set_pfile
   Use season,         Only: lprst, lsn
   Use categories,     Only: fps_p
   Use prediction,     Only: ifc, ifm, &
                             calc_fcast, close_predict, get_fprds, read_predict
   Use analysis,       Only: ianal, ihind, &
                             analysis_flags, close_calc
!
! Function result
   Integer :: get
!
! Locals
!
! Local scalars
   Integer :: ierr  ! - error indicator -
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Trim
!
! Executable Statements
!
! Initialise flags
   Call set_cursor_waiting (1)
! -  calculation flags -
   lcalc = .true.
! -  stop calculation flag -
   lstop = .false.
!
! Check that number of forecasts is not 0
   If (nf < 1) Then
      ifail = 2
      GoTo 1
   End If
!
! Initialise forecasts if necessary
   If (.not.Allocated(fps_p)) ifc = 0
!
! Initialise
! - check for invalid number of forecasts -
   If (ifc <= 0) Then
      If (zfile%fdate+(nf-1) > zfile%prdn%sdate) Then
         ifail = 3
         GoTo 1
      End If
! - check for availability of persistence component -
      If (lprst) Then
         ifail = set_pfile(nf, lsn(0))
         If (ifail /= 0) GoTo 1
      End If
! - clear memory -
      If (ifc == -1) Then
         lcalc = .false.
         ifail = close_predict()
         lcalc = .true.
      End If
! - initialise -
      ifail = read_predict(ianal, ihind, lconvert)
      Select Case (ifail)
       Case (0)
         Continue
       Case (3, 4, 5)
         Call cpt_error ('read_predict', .true., ifail, &
              i_arg1=zfile%iseq)
       Case (6:)
         ierr = ifail - 5
         ifail = 6
         Call cpt_error ('read_predict', .true., ifail, &
              c_arg1=Trim(zfile%cfile%ffile), i_arg2=ierr)
         GoTo 1
       Case Default
         GoTo 1
      End Select
   End If
   If (ifm == 0) ifm = 1
! - calculate forecasts -
   If (ifc <= 1) Then
      Call get_fprds (nf, 4)
      If (.not.Allocated(t)) Then
         If (calc_fcast(ianal) /= 0) GoTo 1
      Else
         If (calc_fcast(ianal, t=t(:)) /= 0) GoTo 1
      End If
   End If
   ifc = 2
   Call set_menu_greying (ifc=1)
   lcalc = .false.
   ifail = 0
!
! Errors
1  Call set_cursor_waiting (0)
   Select Case (ifail)
    Case (0)
      get = 0
      Return
    Case (-1)
      Call analysis_flags ('off')
      get = close_predict()
      get = close_calc()
      get = 2
    Case (1)
      Call cpt_error ('get_forecast', .false., ifail)
      lcalc = .false.
      get = close_predict()
      get = 1
    Case (2)
      lcalc = .false.
      get = close_predict()
      get = 1
    Case (3, 4, 5)
      Call cpt_error ('get_forecast', .false., ifail)
      lcalc = .false.
      Call set_menu_greying (ifc=0)
      get = 1
    Case Default
      lcalc = .false.
      get = close_predict()
      get = 1
   End Select
!
   Return
 End Function get_forecast
!
!
!
 Function get_cv2_forecast(icv2i) &
          Result (get)
!
! Calculates a doubly cross-validated forecast
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm
   Use time,               Only: Operator(+)
   Use settings,           Only: igauss
   Use arrays,             Only: xin, xc, yin, z, zyp
   Use iofiles,            Only: mxa, mya, xfile, zfile, &
                                 set_pfile
   Use fields,             Only: yfield
   Use regression,         Only: ens_p, ens_2, fcast, fcastt, pesd_p, pesd_2, pls_p, pls_2, xvp, ycv2, ycv2t
   Use season,             Only: lprst, lsn, nsn, &
                                 seasonal_zyp
   Use missing,            Only: replace_missing_z, replace_missing_zyp
   Use categories,         Only: fps_p, fps_2, odds_p, odds_2
   Use prediction,         Only: ifc, &
                                 init_predict
   Use analysis,           Only: ianal
!
! Function result
   Integer :: get
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icv2i  ! - double cross-validation index -
!
! Executable Statements
!
! set start date
   zfile%fdate = xfile%fdate + (icv2i -1)
!
! Initialise
   If (icv2i == 1) Then
      get = init_predict(ianal, 1, .false.)
   Else
      Allocate (xc(mxa,1,1))
      Select Case (ianal)
       Case (ia_cca, ia_pcr, ia_mlr)
         Allocate (xvp(1,1))
       Case (ia_gcm)
         Allocate (xvp(mya,1))
      End Select
   End If
!
! Copy predictors
! - copy X data into Z - 
   z(:,1,1) = xin(:,icv2i,1)
! - replace missing predictors -
   get = replace_missing_z(ianal, nsn)
!
! Copy persistence data
! - copy Y data into ZYP -
   If (lprst) Then
      get = set_pfile(1, lsn(0))
      zyp(:,1,1:lsn(0)) = yin(:,icv2i,1:lsn(0))
! - replace missing monthly persistence values -
      If (lsn(0) > 1) get = replace_missing_zyp(1, lsn(0))
! - calculate seasonal persistence -
      Call seasonal_zyp (yfield(:)%tssn%tprd(0)%sdate%imn, yfield(:)%rmiss)
! - replace missing persistence values -
      get = replace_missing_zyp(0, 0)
! - copy persistence component of the forecast -
      fcast(:,:,0) = zyp(1:mya,:,0)
   End If
!
! Calculate forecast
   ifc = 1 ! - set to forecast only -
   get = get_forecast()
!
! Copy results
! - double cross-validated predictions -
   ycv2(:,icv2i,:) = fcast(:,1,:)
! - double cross-validated ensemble predictions -
   ens_2(:,icv2i,:) = ens_p(:,1,:)
! - transformed double cross-validated predictions -
   If (igauss > 0) ycv2t(:,icv2i) = fcastt(:,1)
! - prediction-error variance -
   pesd_2(:,icv2i) = pesd_p(:,1)
! - double cross-validated prediction limits -
   pls_2(:,icv2i) = pls_p(:,1)
! - souble cross-validated forecast probabilities -
   fps_2(:,icv2i,:) = fps_p(:,1,:)
! - double cross-validated odds -
   odds_2(:,icv2i,:,:) = odds_p(:,1,:,:)
!
   Return
 End Function get_cv2_forecast
!
!
!
 Function fcst_fser() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Series
!
! Modules
#if GUI == 1
   Use data_cpt_constants, Only: is_non, is_ano, is_std, is_sti, is_pav
   Use labels,             Only: l_axislimsy, l_fcasts, l_lab
#endif
   Use gui,                Only: set_cursor_waiting
#if GUI == 1
   Use settings,           Only: istd
   Use windows_gui,        Only: imgid, &
                                 init_window_graphic
#else
   Use gui,                Only: get_output_unit
   Use cpt_output,         Only: print_results
#endif
!
! Function result
   Integer :: fcst
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
#if GUI == 1
! Set image title
   Select Case (istd)
    Case (is_non)
      img_fser(:)%title = 'Forecasts and Cross-Validated Hindcasts'
    Case (is_ano)
      img_fser(:)%title = 'Forecast and Cross-Validated Hindcast Anomalies'
    Case (is_std)
      img_fser(:)%title = 'Standardized Forecasts and Cross-Validated Hindcasts'
    Case (is_sti)
      img_fser(:)%title = 'Forecast and Cross-Validated Hindcast SPIs'
    Case (is_pav)
      img_fser(:)%title = 'Forecast and Cross-Validated Hindcast %s of Average'
   End Select
!
! Create window
   fcst = init_window_graphic(1, img_fser(:), imgid, l_fcasts%c, [l_lab], &
          update_fser, f_title, f_export,                                 &
          cxy='Y', iloc=ioutf0, limits_grph=f_limits, llims=[l_axislimsy], add_win_h=show_fser, f_help=help_fcast)
#else
!
! Print forecasts
   ioutf0 = get_output_unit()
   fcst = show_fser()
   fcst = print_results(update_fser)
#endif
   fcst = 2
!
   Return
 End Function fcst_fser
!
!
!
 Function fcst_fens() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Ensemble
!
! Modules
#if GUI == 1
   Use data_cpt_constants, Only: is_non, is_ano, is_std, is_sti, is_pav
   Use labels,             Only: l_axislimsy, l_lab
#endif
   Use gui,                Only: set_cursor_waiting
#if GUI == 1
   Use settings,           Only: istd
   Use windows_gui,        Only: imgid, &
                                 init_window_graphic
#else
   Use gui,                Only: get_output_unit
   Use cpt_output,         Only: print_results
#endif
!
! Function result
   Integer :: fcst
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
#if GUI == 1
! Set image title
   Select Case (istd)
    Case (is_non)
      img_fens(:)%title='Ensemble Forecasts and Cross-Validated Hindcasts'
    Case (is_ano)
      img_fens(:)%title='Ensemble Forecasts and Cross-Validated Hindcast Anomalies'
    Case (is_std)
      img_fens(:)%title='Standardized Ensemble Forecasts and Cross-Validated Hindcasts'
    Case (is_sti)
      img_fens(:)%title='Ensemble Forecasts and Cross-Validated Hindcast SPIs'
    Case (is_pav)
      img_fens(:)%title='Ensemble Forecasts and Cross-Validated Hindcast %s of Average'
   End Select
!
! Create window
   fcst = init_window_graphic(1, img_fens(:), imgid, 'Forecast Ensembles', [l_lab], &
          update_fens, f_title, f_export,                                           &
          cxy='Y', iloc=ioutf7, limits_grph=f_limits, llims=[l_axislimsy], extra_prompt=prompt_forecast, &
          add_win_h=show_fens, f_help=help_fcast)
#else
!
! Print forecasts
   ioutf7 = get_output_unit()
   fcst = show_fens()
   fcst = print_results(update_fens)
#endif
   fcst = 2
!
   Return
 End Function fcst_fens
!
!
!
 Function fcst_pexc() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Exceedence Probabilities
!
! Modules
#if GUI == 1
   Use labels,      Only: l_axislimsx, l_exceedps, l_lab
#endif
   Use gui,         Only: set_cursor_waiting
#if GUI == 1
   Use windows_gui, Only: imgid, &
                          init_window_graphic
   Use settings,    Only: iexc
#else
   Use gui,         Only: get_output_unit
   Use cpt_output,  Only: print_results
#endif
   Use prediction,  Only: close_pexc, init_pexc
!
! Function result
   Integer :: fcst
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
#endif
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
!
! Initialise memory
   If (init_pexc() /= 0) Return
   Call set_cursor_waiting (1)
!
#if GUI == 1
! Initialise graphic
   Select Case (iexc)
    Case (0)
      img_pexc(:)%title = Trim(l_exceedps%c)
    Case (1)
      img_pexc(:)%title = 'Non-exceedance probabilities'
   End Select
!
! Create window
   fcst = init_window_graphic(1, img_pexc(:), imgid, Trim(img_pexc(1)%title), [l_lab], &
          update_pexc, f_title, f_export,                                              &
          reset_img=reset_img_pexc, save_rslt=save_pexc, close_grph=close_pexc, cxy='Y', iloc=ioutf6, &
          extra_prompt=prompt_forecast, limits_grph=f_limits, llims=[l_axislimsx], f_help=help_fcast)
!
#else
! Print forecasts
   ioutf6 = get_output_unit()
   fcst = print_results(update_pexc)
   fcst = close_pexc()
#endif
   fcst = 2
!
   Return
 End Function fcst_pexc
!
!
!
 Function fcst_fval() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Maps ~ Values
!
! Modules
#if GUI == 1
   Use labels,      Only: l_contourlims, l_fcasts, &
                          ls_threshs
#endif
   Use gui,         Only: set_cursor_waiting
#if GUI == 1
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#endif
!
! Function result
   Integer :: fcst
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
!
! Create window
#if GUI == 1
   fcst = init_window_graphic(3, img_fval(:), imgid, l_fcasts%c, [ls_threshs(1), l_fcasts, ls_threshs(2)], &
          update_fval, f_title, f_export, &
          cxy='Y', select_grph=select_fval, extra_prompt=prompt_forecast, add_win_h=show_fval, &
          limits_grph=f_limits, llims=[l_contourlims, l_contourlims, l_contourlims], f_help=help_fcast)
#else
!
! Print results
   fcst = show_fval()
   fcst = update_fval()
#endif
   fcst = 2
!
   Return
 End Function fcst_fval
!
!
 Function fcst_fps() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Maps ~ Probabilities
!
! Modules
#if GUI == 1
   Use labels,      Only: l_contourlims, &
                          ls_cats
#endif
   Use gui,         Only: set_cursor_waiting
#if GUI == 1
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#endif
!
! Function result
   Integer :: fcst
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
!
#if GUI == 1
! Create window
   fcst = init_window_graphic(3, img_fps(:), imgid, 'Forecast probabilities maps', ls_cats(1:), update_fps, f_title, f_export, &
          cxy='Y', select_grph=select_fps, extra_prompt=prompt_forecast, add_win_h=show_fps, &
          reset_img=reset_img_fps, limits_grph=f_limits, llims=[l_contourlims, l_contourlims, l_contourlims], &
          f_help=help_fcast)
#else
!
! Print results
   fcst = show_fps()
   fcst = update_fps()
#endif
   fcst = 2
!
   Return
 End Function fcst_fps
!
!
!
 Function fcst_odds() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Maps ~ Odds
!
! Modules
#if GUI == 1
   Use labels,      Only: l_contourlims, &
                          ls_cats
#endif
   Use gui,         Only: set_cursor_waiting
#if GUI == 1
   Use settings,    Only: iodds
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#endif
!
! Function result
   Integer :: fcst
!
#if GUI == 1
! Locals
!
! Local scalars
   Character(Len=27) :: codds ! - odds / relative odds -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
#endif
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
!
#if GUI == 1
! Plot odds
   Select Case (iodds)
    Case (0)
      codds = 'Forecast odds maps'
    Case Default
      codds = 'Forecast relative odds maps'
   End Select
!
! Create window
   fcst = init_window_graphic(3, img_odds(:), imgid, Trim(codds), ls_cats(1:), update_odds, f_title, f_export, &
          cxy='Y', select_grph=select_odds, extra_prompt=prompt_forecast, add_win_h=show_odds, &
          limits_grph=f_limits, llims=[l_contourlims, l_contourlims, l_contourlims], f_help=help_fcast)
#else
!
! Print results
   fcst = show_odds()
   fcst = update_odds()
#endif
   fcst = 2
!
   Return
 End Function fcst_odds
!
!
!
#if GUI == 1
 Function fcst_hps() &
          Result (fcst)
!
! Create window for Tools ~ Forecast ~ Maps ~ Probabilistic
!
! Modules
   Use data_numbers, Only: one, oneh
   Use labels,       Only: l_lab
   Use gui,          Only: set_cursor_waiting
   Use settings,     Only: iskmask, rskmask
   Use iofiles,      Only: mya
   Use fields,       Only: yfield
   Use scores,       Only: corrmsk
   Use categories,   Only: fps_p, hps
   Use prediction,   Only: close_hps, init_hps
   Use windows_gui,  Only: imgid, &
                           init_window_graphic
!
! Function result
   Integer :: fcst
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Check whether forecasts require initialization
   fcst = 1
   If (get_forecast() /= 0) Return
!
! Initialise memory
   If (init_hps() /= 0) Return
!
! Identify highest probabilities
   hps(:,:) = MaxVal(fps_p(:,:,:), Dim=3)
   Where (fps_p(:,:,1) == -one) ! - missing values -
      hps(:,:) = yfield(1)%rmiss
   Else Where (hps(:,:) == fps_p(:,:,1)) ! - below-normal -
      hps(:,:) = -hps(:,:)
   Else Where (hps(:,:) == fps_p(:,:,3)) ! - above-normal -
      hps(:,:) = hps(:,:) + oneh
   End Where
! - mask areas with insufficient skill -
   If (iskmask > 0) Then
      Do i = 1, mya
         If (corrmsk(i) < rskmask) hps(i,:) = yfield(1)%rmiss
      End Do
   End If
!
! Set title
   img_hps(:)%title = 'Probabilistic forecasts'
!
! Plot probabilistic forecasts
   fcst = init_window_graphic(1, img_hps(:), imgid, 'Probabilistic forecast map', [l_lab], &
          update_hps, f_title, f_export, &
          reset_img=reset_img_hps, close_grph=close_hps, cxy='Y', select_grph=select_hps, extra_prompt=prompt_forecast, &
          f_help=help_fcast)
   fcst = 2
!
   Return
 End Function fcst_hps
!
!
!
#endif
 Function show_fser() &
          Result (show)
!
! Creates output window for Forecast ~ Series results
!
! Modules
#if GUI == 1
   Use labels,   Only: l_climatol, l_fcasts, l_pintervals, l_probs, l_threshs
#endif
   Use gui,      Only: open_clearwin
#if GUI == 1
   Use gui,      Only: box_close, box_open, gui_creturn, print_text
   Use menus,    Only: add_menu
#endif
   Use settings, Only: nf
#if GUI == 1
   Use climate,  Only: print_climatology
   Use graphics, Only: ipis
#endif
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: maxc = 8 + ng*4 + 2*(1 + ng*6) ! - maximum number of columns -
   Integer :: maxl = 8                       ! - maximum number of lines -
   Integer :: nl                             ! - number of lines -
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Add pop-up menu
   Call add_menu ('pm', 1, -1, l_pintervals, &
        icheck=ipis, itoggle=ipis, cbf=plot_fser)
!
! Print climatology
   Call box_open (.true., .false., &
        title=Trim(l_climatol%c))
   Call print_climatology (.true.)
   Call gui_creturn (.false.)
#endif
! - open Clearwin+ windows -
   Call open_clearwin (maxc, 3, .true., ioutf1, &
        ih=ih_f1)
!
! Print thresholds
#if GUI == 1
   Call gui_creturn (.false.)
   Call print_text (Trim(l_threshs%c)//':', &
        lcolour=.true.)
   Call gui_creturn (.false.)
#endif
   Call open_clearwin (maxc, 9, .true., ioutf2, &
        ih=ih_f2)
#if GUI == 1
   Call box_close (.true.)
!
! Print forecast information
   Call gui_creturn (.false.)
   Call gui_creturn (.true.)
   Call box_open (.true., .false., &
        title=Trim(l_fcasts%c))
   Call print_text (Trim(l_probs%c)//':', &
        lcolour=.true.)
! - open Clearwin+ windows -
   Call gui_creturn (.false.)
#endif
   nl = nf + 3
   If (nl <= maxl) Then
      Call open_clearwin (maxc, nl, .true., ioutf3, &
           ih=ih_f3)
   Else
      Call open_clearwin (maxc, maxl, .true., ioutf3, &
           ih=ih_f3, maxl=nl, lvs=.true.)
   End If
#if GUI == 1
!
! Print forecast ranges
   Call gui_creturn (.false.)
   Call gui_creturn (.true.)
   Call print_text ('Forecast ranges:', &
        lcolour=.true.)
! - open Clearwin+ windows -
   Call gui_creturn (.false.)
#endif
   If (nl <= maxl) Then
      Call open_clearwin (maxc, nl, .true., ioutf4, &
           ih=ih_f4)
   Else
      Call open_clearwin (maxc, maxl, .true., ioutf4, &
           ih=ih_f4, maxl=nl, lvs=.true.)
   End If
#if GUI == 1
   Call box_close (.true.)
#endif
   show = 1
!
   Return
 End Function show_fser
!
!
!
 Function show_fens() &
          Result (show)
!
! Creates output window for Forecast ~ Ensemble results
!
! Modules
#if GUI == 1
   Use labels,   Only: l_fcasts
#endif
   Use gui,      Only: open_clearwin
#if GUI == 1
   Use gui,      Only: box_close, box_open, gui_creturn, print_text
#endif
   Use settings, Only: nenf
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: maxc = 28 ! - maximum number of columns -
   Integer :: maxl = 14 ! - maximum number of lines -
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Print forecast information
   Call box_open (.true., .false., &
        title=Trim(l_fcasts%c))
   Call print_text ('Ensemble members:', &
        lcolour=.true.)
! - open Clearwin+ windows -
   Call gui_creturn (.false.)
#endif
   If (nenf+3 <= 14) Then
      Call open_clearwin (maxc, maxl, .true., ioutf8, &
           ih=ih_f8)
   Else
      Call open_clearwin (maxc, maxl, .true., ioutf8, &
           ih=ih_f8, maxl=nenf+3, lvs=.true.)
   End If
#if GUI == 1
   Call box_close (.true.)
#endif
   show = 1
!
   Return
 End Function show_fens
!
!
!
 Function show_fval() &
          Result (show)
!
! Creates output window for Forecast ~ Values results
!
! Modules
   Use labels,      Only: l_fcasts
   Use fields,      Only: yfield
   Use windows_gui, Only: show_results
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: maxc = 18 + 3*11 ! - maximum number of columns -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, Trim
!
! Executable Statements
!
! Print forecast information
   show = show_results(Trim(l_fcasts%c), maxc, MaxVal(yfield(:)%nva)+2, ioutf4, ih_f4)
!
   Return
 End Function show_fval
!
!
!
 Function show_fps() &
          Result (show)
!
! Creates output window for Forecast ~ Probabilities results
!
! Modules
   Use labels,      Only: cg_fcastprobs
   Use fields,      Only: yfield
   Use windows_gui, Only: show_results
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: maxc = 18 + ng*10 ! - maximum number of columns -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, Trim
!
! Executable Statements
!
! Print forecast information
   show = show_results(Trim(cg_fcastprobs), maxc, MaxVal(yfield(:)%nva)+2, ioutf5, ih_f5)
!
   Return
 End Function show_fps
!
!
!
 Function show_odds() &
          Result (show)
!
! Creates output window for Forecast ~ Odds results
!
! Modules
   Use labels,      Only: l_odds, l_oddsrel
   Use settings,    Only: iodds
   Use fields,      Only: yfield
   Use windows_gui, Only: show_results
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: maxc = 18 + ng*10 ! - maximum number of columns -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, Trim
!
! Executable Statements
!
! Print forecast information
   Select Case (iodds)
    Case (0)
      show = show_results(Trim(l_odds%c), maxc, MaxVal(yfield(:)%nva)+2, ioutf10, ih_f10)
    Case Default
      show = show_results(Trim(l_oddsrel%c), maxc, MaxVal(yfield(:)%nva)+2, ioutf10, ih_f10)
   End Select
!
   Return
 End Function show_odds
!
!
!
 Function update_fser() &
          Result (update)
!
! Updates Forecast ~ Series results and graphs
!
! Modules
#if GUI == 1
   Use clrwin$,             Only: clear_window$
#endif
   Use data_numbers,        Only: rp, zero, eps, one, oneh
   Use data_cpt_constants,  Only: is_non, is_ano, is_std, is_sti, is_pav, it_gam, iv_ref, nts
   Use data_io_constants,   Only: io_iso, lprd
   Use data_time_constants, Only: isq_yr, iuseq
   Use maths,               Only: prob_rounding
   Use labels,              Only: ca_cat_a, ca_seq, &
                                  cg_empirical, cg_na, cg_theoretical, &
                                  l_fcast, l_odds, l_oddsrel, l_probs, &
                                  ls_cats, ls_threshs, &
                                  make_label
#if GUI == 0
   Use labels,              Only: l_fcasts, l_threshs
#endif
   Use time,                Only: Operator(+), &
                                  iseq, &
                                  get_cdate
#if GUI == 0
   Use gui,                 Only: print_subtitle, print_text
#endif
   Use gui,                 Only: set_cursor_waiting
   Use settings,            Only: ifpr, igauss, iprecf, istd, iva, nf
   Use arrays,              Only: fprds, y
   Use fields,              Only: update_grid
   Use distribs,            Only: alpha, beta, pcen, &
                                  cdf_gamma, cdf_normal, get_mean, get_var, ustdize
   Use regression,          Only: fcast, pls_p
   Use season,              Only: isnc
   Use categories,          Only: fps_p, irefs, ithr, odds_p, pobs, refs, tobs
   Use climate,             Only: ave, clim, nc, sdev
#if GUI == 0
   Use climate,             Only: print_climatology
#endif
!
! Function result
   Integer :: update
!
! Locals
!
! Local scalars
   Integer :: i    ! - index -
   Integer :: j    ! - threshold/category index -
   Integer :: k    ! - forecast index -
   Integer :: jj   ! - threshold index -
   Integer :: jmin ! - minimum threshold index -
!
   Real(Kind=rp) :: av   ! - average -
   Real(Kind=rp) :: sd   ! - standard deviation -
   Real(Kind=rp) :: terr ! - threshold difference -
   Real(Kind=rp) :: tmin ! - minimum threshold difference -
!
   Character(Len=   8) :: c_cat ! - category -
   Character(Len=lprd) :: cdate ! - date -
!
! Local arrays
   Real(Kind=rp), Dimension(ng) :: rps   ! - rounded probabilities -
   Real(Kind=rp), Dimension(ng) :: tpobs ! - theoretical probabilities -
!
   Real(Kind=rp), Dimension(ng,2) :: codds ! - climatological odds -
   Real(Kind=rp), Dimension( 1,1) :: rwk   ! - workspace -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, AdjustR, All, Huge, Len_Trim, Nint, Sqrt, Trim
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
!
! Construct and add coordinate label for initial point
   Call set_cursor_waiting (1)
   Call update_grid (ioutf0)
!
! Update climatology
#if GUI == 1
   Call clear_window$ (ih_f1)
   Call clear_window$ (ih_f2)
#else
   Call print_subtitle (Trim(l_fcasts%c), &
        iout=ioutf0)
   Call print_text (' ', ioutf0)
   Call print_climatology (.true., &
        iout=ioutf0)
#endif
   Write (Unit=cfmt, Fmt='(A,I1,A)') '(3X,A8,F12.', iprecf, ')'
   Select Case (istd)
    Case (is_non) ! - no standardization -
      av = ave(iva,isnc)
      sd = sdev(iva,isnc)
    Case (is_ano) ! - anomalies -
      av = zero
      sd = sdev(iva,isnc)
    Case (is_std) ! - standardized anomalies -
      av = zero
      sd = one
    Case (is_sti) ! - standardized indices -
      av = get_mean(nc, clim(iva,1:nc,isnc))
      sd = Sqrt(get_var(nc, clim(iva,1:nc,isnc), av))
    Case (is_pav) ! - percent of average -
      av = oneh
      sd = oneh*sdev(iva,isnc)/ave(iva,isnc)
   End Select
   Write (Unit=ioutf1, Fmt=cfmt) 'Average', av
   Write (Unit=ioutf1, Fmt=cfmt) 'Std dev', sd
!
! Update thresholds
#if GUI == 0
   Call print_subtitle (Trim(l_threshs%c), &
        iout=ioutf0)
#endif
   If (ithr /= iv_ref) Then
      Write (Unit=cfmt, Fmt='(A,I1,A)') '(4X,A8,F12.', iprecf, ')'
      Do j = nts, 1, -1
         Write (Unit=ioutf2, Fmt=cfmt) ls_threshs(j)%c, tobs(iva,j)
      End Do
   Else
      Write (Unit=cfmt, Fmt='(A,I1,A)') '(4X,A8,F12.', iprecf, ',A,I0,A)'
      Do j = nts, 1, -1
         jmin = 0
         tmin = Huge(tmin)
         Do jj = 1, nts
            terr = Abs(tobs(iva,j) - y(iva,irefs(jj),isnc))
            If (terr < tmin) Then
               jmin = jj
               tmin = terr
            End If
         End Do
         Write (Unit=ioutf2, Fmt=cfmt) ls_threshs(j)%c, tobs(iva,j), '  (', refs(jmin)%iyr, ')'
      End Do
   End If
#if GUI == 1
   Write (Unit=ioutf2, Fmt=*)
#endif
!
! Calculate climatological odds
! - empirical -
   If (All(pobs(iva,:) > eps)) Then
      codds(:,1 ) = pobs(iva,:)/(one - pobs(iva,:))
   Else
      codds(:,1) = -one
   End If
! - theoretical -
   If (igauss == it_gam) Then
      tpobs(ng) = one
      If (istd == is_sti) Then ! - theoretical (using SPI) -
         Do j = 1, ng-1
            tpobs(j) = cdf_normal(tobs(iva,j))
         End Do
      Else ! - theoretical (using gamma) -
         Do j = 1, ng-1
            rwk(1,1) = tobs(iva,j)
            Call ustdize (istd, igauss, isnc, 1, 1, ave(iva:iva,isnc), sdev(iva:iva,isnc), rwk(:,:))
            tpobs(j) = cdf_gamma(rwk(1,1)/beta(iva,isnc), alpha(iva,isnc), pcen(iva,isnc))
         End Do
      End If
      Do j = ng, 2, -1
         tpobs(j) = tpobs(j) - tpobs(j-1)
      End Do
      If (All(tpobs(:) > eps)) Then
         codds(:,2) = tpobs(:)/(one - tpobs(:))
      Else
         codds(:,2) = -one
      End If
   End If
!
! Update climatological probabilities and odds
#if GUI == 0
   Call print_subtitle ('Climatological probabilities and odds', &
        iout=ioutf2)
#endif
   Select Case (igauss)
    Case Default
      Write (Unit=ioutf2, Fmt='(T15,A,8X,A)') l_probs%c(1:4), Trim(l_odds%c)
      If (codds(1,1) >= zero) Then
         Do j = ng, 1, -1
            c_cat = Trim(make_label('T', ls_cats(j)))
            Write (Unit=ioutf2, Fmt='(4X,A,I5,A,F12.2)') AdjustR(c_cat), Nint(pobs(iva,j)*oneh), '%', codds(j,1)
         End Do
      Else
         Do j = ng, 1, -1
            c_cat = Trim(make_label('T', ls_cats(j)))
            Write (Unit=ioutf2, Fmt='(4X,A,I5,A,9X,A)') AdjustR(c_cat), Nint(pobs(iva,j)*oneh), '%', cg_na
         End Do
      End If
    Case (it_gam) ! - print theoretical probabilities and odds if the gamma distribution is used -
      Write (Unit=ioutf2, Fmt='(T15,A,T33,A)') Trim(cg_empirical), Trim(cg_theoretical)
      Write (Unit=ioutf2, Fmt='(T15,A,8X,A,T33,A,8X,A)') l_probs%c(1:4), Trim(l_odds%c), l_probs%c(1:4), Trim(l_odds%c)
      If (codds(1,1) >= zero) Then
         Do j = ng, 1, -1
            c_cat = Trim(make_label('T', ls_cats(j)))
            Write (Unit=ioutf2, Fmt='(4X,A,2(I5,A,F12.2))') AdjustR(c_cat), Nint(pobs(iva,j)*oneh), '%', codds(j,1), &
                                                                            Nint(   tpobs(j)*oneh), '%', codds(j,2)
         End Do
      Else
         Do j = ng, 1, -1
            c_cat = Trim(make_label('T', ls_cats(j)))
            Write (Unit=ioutf2, Fmt='(4X,A,2(I5,A,9X,A))') AdjustR(c_cat), Nint(pobs(iva,j)*oneh), '%', cg_na, &
                                                                           Nint(   tpobs(j)*oneh), '%', cg_na
         End Do
      End If
   End Select
!
! Update forecast probabilities
#if GUI == 1
   Call clear_window$ (ih_f3)
   Write (cfmt, '(3(A,I2.2),A)') &
         '(A,T', 20-Len_Trim(l_probs%c), ',A,T', 39-Len_Trim(l_odds%c), ',A,T', 58-Len_Trim(l_oddsrel%c), ',A)'
#else
   Write (Unit=ioutf3, Fmt=*)
   Write (cfmt, '(3(A,I2.2),A)') &
         '(1X,A,T', 37-Len_Trim(l_probs%c), ',A,T', 57-Len_Trim(l_odds%c), ',A,T', 77-Len_Trim(l_oddsrel%c), ',A)'
#endif
   Write (Unit=ioutf3, Fmt=cfmt) ca_seq(iuseq(iseq)), Trim(l_probs%c), Trim(l_odds%c), Trim(l_oddsrel%c)
#if GUI == 1
   Write (Unit=ioutf3, Fmt='(7X,3(3X,A),2(1X,3(5X,A)))') ((ca_cat_a(j), j = 1, ng), k = 1, 3)
   cfmt='(A4,3X,3I4,2(1X,3F6.2))'
#else
   Write (Unit=ioutf3, Fmt='(T25,3(3X,A),2(2X,3(5X,A)))') ((ca_cat_a(j), j = 1, ng), k = 1, 3)
   cfmt='(1X,A,T25,3I4,2(2X,3F6.2))'
#endif
   Do k = 1, nf
      cdate = Trim(get_cdate(fprds(k)%tprd(isnc), 1, io_iso))
      rps(:) = fps_p(iva,k,:)
      Call prob_rounding (ifpr, ng, pobs(iva,:), rps(:))
      Write (Unit=ioutf3, Fmt=cfmt) &
         Trim(cdate), (Nint(rps(j)), j=1,ng), ((odds_p(iva,k,j,i), j = 1, ng), i = 1, 2)
   End Do
!
! Update forecasts
#if GUI == 1
   Call clear_window$ (ih_f4)
#else
   Write (Unit=ioutf4, Fmt=*)
#endif
   Write (cfmt, '(3(A,I2.2),A)') &
         '(1X,A,T', 17-Len_Trim(l_fcast%c), ',A,T', 28-Len_Trim(ls_threshs(1)%c), ',A,T', 39-Len_Trim(ls_threshs(2)%c), ',A)'
   Write (Unit=ioutf4, Fmt=cfmt) ca_seq(iuseq(iseq)), Trim(l_fcast%c), Trim(ls_threshs(1)%c), Trim(ls_threshs(2)%c)
   Select Case (iseq)
    Case (isq_yr)
      Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,I4,3F11.', iprecf, ')'
      Do k = 1, nf
         Write (Unit=ioutf4, Fmt=cfmt) fprds(k)%tprd(isnc)%sdate%iyr, fcast(iva,k,isnc), pls_p(iva,k)%lower, pls_p(iva,k)%upper
      End Do
    Case Default
      Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,A4,3F11.', iprecf, ')'
      Do k = 1, nf
         cdate = get_cdate(fprds(k)%tprd(isnc)%sdate, io_iso)
         Write (Unit=ioutf4, Fmt=cfmt) Trim(cdate), fcast(iva,k,isnc), pls_p(iva,k)%lower, pls_p(iva,k)%upper
      End Do
   End Select
!
! Update graph
#if GUI == 1
   update = plot_fser()
#else
   Write (Unit=ioutf4, Fmt=*)
   update = 0
#endif
   Call set_cursor_waiting (0)
!
   Return
 End Function update_fser
!
!
!
 Function update_fens() &
          Result (update)
!
! Updates Forecast ~ Ensemble results and graphs
!
! Modules
#if GUI == 1
   Use clrwin$,      Only: clear_window$
#endif
   Use data_numbers, Only: rp
#if GUI == 0
   Use labels,       Only: cg_ensemble, &
                           l_fcasts
   Use gui,          Only: print_subtitle
#endif
   Use gui,          Only: print_text, set_cursor_waiting
   Use settings,     Only: iprecf, iva, nenf
#if GUI == 0
   Use settings,     Only: nf
#endif
   Use fields,       Only: update_grid
   Use regression,   Only: ens_p
#if GUI == 0
   Use climate,      Only: print_climatology
#endif
   Use prediction,   Only: ifm
!
! Function result
   Integer :: update
!
! Locals
!
! Local scalars
   Integer :: i ! - ensemble member -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
#if GUI == 0
   Intrinsic Trim
#endif
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
!
! Construct and add coordinate label for initial point
   Call set_cursor_waiting (1)
   Call update_grid (ioutf7)
!
! Update forecasts
   Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,I6,F10.3,F11.', iprecf, ')'
#if GUI == 1
   Call clear_window$ (ih_f8)
#else
   Call print_subtitle (Trim(l_fcasts%c), &
        iout=ioutf7)
   Call print_text (' ', &
        iout=ioutf7)
   Call print_climatology (.true., &
        iout=ioutf7)
   Call print_subtitle (Trim(cg_ensemble), &
        iout=ioutf7)
   Do ifm = 1, nf
      update = change_ifm()
#endif
      Call print_text ('Member  Quantile   Forecast', &
           iout=ioutf8)
      Call print_text (' ', &
           iout=ioutf8)
      Do i = 1, nenf
         Write (Unit=ioutf8, Fmt=cfmt) i, Real(i, Kind=rp)/Real(nenf+1, Kind=rp), ens_p(iva,ifm,i)
      End Do
!
#if GUI == 1
! Update graph
      update = plot_fens()
#else
      Call print_text (' ', &
           iout=ioutf8)
   End Do
!
   update = 0
#endif
   Call set_cursor_waiting (0)
!
   Return
 End Function update_fens
!
!
!
 Function update_pexc() &
          Result (update)
!
! Updates Forecast ~ Probability of exceedance results and graphs
!
! Modules
#if GUI == 0
   Use data_cpt_constants, Only: nep
   Use labels,             Only: ca_stds_t
   Use gui,                Only: print_text
#endif
   Use gui,                Only: set_cursor_waiting
#if GUI == 0
   Use settings,           Only: iexc, istd, iva, nf
#endif
   Use fields,             Only: update_grid
#if GUI == 0
   Use regression,         Only: c, e, f
   Use season,             Only: isnc
   Use climate,            Only: clim, nc
   Use prediction,         Only: ifm, &
                                 calc_pexc
   Use graphics,           Only: init_exceed
#endif
!
! Function result
   Integer :: update
!
#if GUI == 0
! Locals
!
! Local scalars
   Integer :: i ! - probability of exceedance index -
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
#endif
! Construct and add coordinate label for initial point
   Call update_grid (ioutf6)
!
#if GUI == 1
! Update graph
   update = plot_pexc()
!
#else
! Repeat for each forecast
   Do ifm = 1, nf
      update = change_ifm()
!
! Calculate probabilities of exceedance
      Call init_exceed (img_pexc(1), nc, clim(iva,1:nc,isnc))
      update = calc_pexc(img_pexc(1)%ymin, img_pexc(1)%ymax, img_pexc(1)%scale)
!
! Print probabilities of exceedance
      Call print_text (Trim(ca_stds_t(istd+1)))
      Select Case (iexc)
       Case (0)
         Call print_text ('Probabilities of exceedance:')
       Case (1)
         Call print_text ('Probabilities of non-exceedance:')
      End Select
      Call print_text ('     Given forecast   Empirical   Climatological')
      Do i = 1, nep
         Write (Unit=*, Fmt=*) i, f(i), e(i), c(i)
      End Do
      Call print_text (' ')
   End Do
#endif
   Call set_cursor_waiting (0)
   update = 1
!
   Return
 End Function update_pexc
!
!
!
 Function update_fval() &
          Result (update)
!
! Updates Forecast ~ Values results and maps
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use labels,             Only: ca_dstruct, cg_lat, cg_lng, l_fcast, ls_threshs
#if GUI == 1
   Use labels,             Only: ca_stds_t, &
                                 ls_threshs
#endif
   Use gui,                Only: set_cursor_waiting
#if GUI == 1
   Use settings,           Only: clf, istd
#else
   Use settings,           Only: nf
#endif
   Use settings,           Only: iprecf
   Use iofiles,            Only: yfile
   Use cpt_output,         Only: update_results
   Use prediction,         Only: ifm
!
! Function result
   Integer :: update
!
   Character(Len= 8) :: c_lat ! - latitude -
   Character(Len= 9) :: c_lng ! - longitude -
   Character(Len=11) :: c_for ! - forecast -
   Character(Len=11) :: c_low ! - lower -
   Character(Len=11) :: c_upp ! - upper -
   Character(Len=64) :: chead ! - header line -
#if GUI == 1
   Character(Len= 6) :: cclf  ! - forecast confidence level -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustR, Trim
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
! Set titles
   Write (Unit=cclf, Fmt='(F6.3)') clf
   If (cclf(1:1) /= '*') Then
      img_fval(3)%title = Trim(ls_threshs(2)%c)//' ('//cclf//'%)'
      img_fval(1)%title = Trim(ls_threshs(1)%c)//' ('//cclf//'%)'
   Else
      img_fval(3)%title = Trim(ls_threshs(2)%c)//' (>99.999%)'
      img_fval(1)%title = Trim(ls_threshs(1)%c)//' (>99.999%)'
   End If
   img_fval(2)%title = ca_stds_t(istd+1)
!
#else
   Do ifm = 1, nf
      update = change_ifm()
#endif
! Print forecasts
   c_lat = Trim(cg_lat(1:3))
   c_lng = Trim(cg_lng(1:3))
   c_for = Trim(l_fcast%c)
   c_low = Trim(ls_threshs(1)%c)
   c_upp = Trim(ls_threshs(2)%c)
   Write (Unit=cfmt, Fmt='(A,I1,A)') '(A18,3F11.', iprecf, ')'
   Select Case (yfile%idstr)
    Case (id_grd)
      Write (Unit=chead, Fmt='(A)') AdjustR(c_lat)//AdjustR(c_lng)//AdjustR(c_for)//AdjustR(c_low)//AdjustR(c_upp)
    Case (id_stn, id_ind)
      Write (Unit=chead, Fmt='(1X,2A)') ca_dstruct(yfile%idstr), ' '//AdjustR(c_for)//AdjustR(c_low)//AdjustR(c_upp)
   End Select
   update = update_results(ioutf4, ih_f4, ifm, Trim(chead), .true., print_fval)
!
! Plot maps
#if GUI == 1
   update = plot_fvals()
#else
   End Do
!
   update = 0
#endif
   Call set_cursor_waiting (0)
!
   Return
 End Function update_fval
!
!
!
 Function update_fps() &
          Result (update)
!
! Updates Forecast ~ Probabilities results and maps
!
! Modules
#if GUI == 1
   Use clrwin$,            Only: clear_window$
#endif
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use labels,             Only: ca_dstruct, cg_lat, cg_lng, ls_cats
   Use gui,                Only: set_cursor_waiting
#if GUI == 0
   Use settings,           Only: nf
#endif
   Use iofiles,            Only: yfile
   Use cpt_output,         Only: update_results
   Use prediction,         Only: ifm
!
! Function result
   Integer :: update
!
! Locals
!
! Local scalars
#if GUI == 1
   Integer :: i ! - image index -
!
#endif
   Character(Len= 8) :: c_lat ! - latitude -
   Character(Len= 9) :: c_lng ! - longitude -
   Character(Len=10) :: c_b   ! - below -
   Character(Len=10) :: c_n   ! - normal -
   Character(Len=10) :: c_a   ! - above -
   Character(Len=64) :: chead ! - header line -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustR, Trim
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
#if GUI == 1
! Clear window
   Call clear_window$ (ih_f5)
!
! Set titles
   Do i = 1, ng
      img_fps(i)%title = Trim(ls_cats(i)%c)
   End Do
!
#else
   Do ifm = 1, nf
      update = change_ifm()
#endif
! Print forecasts
   c_lat = Trim(cg_lat(1:3))
   c_lng = Trim(cg_lng(1:3))
   c_b = Trim(ls_cats(1)%c)
   c_n = Trim(ls_cats(2)%c)
   c_a = Trim(ls_cats(3)%c)
   Select Case (yfile%idstr)
    Case (id_grd)
      cfmt = '(A18,3(I9,A))'
      Write (Unit=chead, Fmt='(A)') AdjustR(c_lat)//AdjustR(c_lng)//AdjustR(c_b)//AdjustR(c_n)//AdjustR(c_a)
    Case (id_stn, id_ind)
      cfmt = '(A,T19,3(I9,A))'
      Write (Unit=chead, Fmt='(A)') AdjustR(ca_dstruct(yfile%idstr))//AdjustR(c_b)//AdjustR(c_n)//AdjustR(c_a)
   End Select
   update = update_results(ioutf5, ih_f5, ifm, Trim(chead), .true., print_fps)
!
! Plot maps
#if GUI == 1
   update = plot_fpss()
#else
   End Do
   update = 0
#endif
   Call set_cursor_waiting (0)
!
   Return
 End Function update_fps
!
!
!
 Function update_odds() &
          Result (update)
!
! Updates Forecast ~ Odds results and maps
!
! Modules
#if GUI == 1
   Use clrwin$,            Only: clear_window$
#endif
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use labels,             Only: ca_dstruct, cg_lat, cg_lng, ls_cats
   Use gui,                Only: set_cursor_waiting
#if GUI == 0
   Use settings,           Only: nf
#endif
   Use iofiles,            Only: yfile
   Use cpt_output,         Only: update_results
   Use prediction,         Only: ifm
!
! Function result
   Integer :: update
!
! Locals
!
! Local scalars
#if GUI == 1
   Integer :: i ! - image index -
!
#endif
   Character(Len= 8) :: c_lat ! - latitude -
   Character(Len= 9) :: c_lng ! - longitude -
   Character(Len=10) :: c_b   ! - below -
   Character(Len=10) :: c_n   ! - normal -
   Character(Len=10) :: c_a   ! - above -
   Character(Len=64) :: chead ! - header line -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustR, Trim
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
#if GUI == 1
! Clear window
   Call clear_window$ (ih_f10)
!
! Set titles
   Do i = 1, ng
      img_odds(i)%title = Trim(ls_cats(i)%c)
   End Do
!
#else
   Do ifm = 1, nf
      update = change_ifm()
#endif
! Print forecast odds
   c_lat = cg_lat(1:3)
   c_lng = cg_lng(1:3)
   c_b = ls_cats(1)%c
   c_n = ls_cats(2)%c
   c_a = ls_cats(3)%c
   Select Case (yfile%idstr)
    Case (id_grd)
      cfmt = '(A18,3F10.3)'
      Write (Unit=chead, Fmt='(A)') AdjustR(c_lat)//AdjustR(c_lng)//AdjustR(c_b)//AdjustR(c_n)//AdjustR(c_a)
    Case (id_stn, id_ind)
      cfmt = '(A,T19,3F10.3)'
      Write (Unit=chead, Fmt='(A)') AdjustR(ca_dstruct(yfile%idstr))//AdjustR(c_b)//AdjustR(c_n)//AdjustR(c_a)
   End Select
   update = update_results(ioutf10, ih_f10, ifm, Trim(chead), .true., print_odds)
!
! Plot maps
#if GUI == 1
   update = plot_oddss()
#else
   End Do
   update = 0
#endif
   Call set_cursor_waiting (0)
!
   Return
 End Function update_odds
!
!
!
#if GUI == 1
 Function update_hps() &
          Result (update)
!
! Updates Forecast ~ Probabilistic results and maps
!
! Modules
   Use gui, Only: set_cursor_waiting
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Check whether forecasts require initialization
   update = 1
   If (get_forecast() /= 0) Return
   Call set_cursor_waiting (1)
!
! Plot maps
   update = plot_hps()
   Call set_cursor_waiting (0)
!
   Return
 End Function update_hps
!
!
!
#endif
 Function print_fval(ioutf, ija, ifm, istn, ifld) &
          Result (printf)
!
! Prints Forecast ~ Values
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use space,              Only: coor, cstndy
   Use iofiles,            Only: yfile
   Use regression,         Only: fcast, pls_p
   Use season,             Only: isnc
!
! Function result
   Integer :: printf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ioutf ! - output unit number -
   Integer, Intent(In) :: ija   ! - location index -
   Integer, Intent(In) :: ifm   ! - forecast index -
   Integer, Intent(In) :: istn  ! - station index -
   Integer, Intent(In) :: ifld  ! - field index -
!
! Executable Statements
!
! Print forecast values
   Select Case (yfile%idstr)
    Case (id_grd)
      Write (Unit=ioutf, Fmt=cfmt) coor, fcast(ija,ifm,isnc), pls_p(ija,ifm)%lower, pls_p(ija,ifm)%upper
    Case (id_stn, id_ind)
      Write (Unit=ioutf, Fmt=cfmt) cstndy(istn,ifld), fcast(ija,ifm,isnc), pls_p(ija,ifm)%lower, pls_p(ija,ifm)%upper
   End Select
   printf = 0
!
   Return
 End Function print_fval
!
!
!
 Function print_fps(ioutf, ija, ifm, istn, ifld) &
          Result (printf)
!
! Prints Forecast ~ Probabilities
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use space,              Only: coor, cstndy
   Use iofiles,            Only: yfile
   Use categories,         Only: fps_p
!
! Function result
   Integer :: printf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ioutf ! - output unit number -
   Integer, Intent(In) :: ija   ! - location index -
   Integer, Intent(In) :: ifm   ! - forecast index -
   Integer, Intent(In) :: istn  ! - station index -
   Integer, Intent(In) :: ifld  ! - field index -
!
! Locals
!
! Local scalars
   Integer :: j ! - forecast index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint
!
! Executable Statements
!
! Print forecast probabilities
   Select Case (yfile%idstr)
    Case (id_grd)
      Write (Unit=ioutf, Fmt=cfmt) coor, (Nint(fps_p(ija,ifm,j)), '%', j=1,ng)
    Case (id_stn, id_ind)
      Write (Unit=ioutf, Fmt=cfmt) cstndy(istn,ifld), (Nint(fps_p(ija,ifm,j)), '%', j=1,ng)
   End Select
   printf = 0
!
   Return
 End Function print_fps
!
!
!
 Function print_odds(ioutf, ija, ifm, istn, ifld) &
          Result (printf)
!
! Prints Forecast ~ Odds
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use settings,           Only: iodds
   Use space,              Only: coor, cstndy
   Use iofiles,            Only: yfile
   Use categories,         Only: odds_p
!
! Function result
   Integer :: printf
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ioutf ! - output unit number -
   Integer, Intent(In) :: ija   ! - location index -
   Integer, Intent(In) :: ifm   ! - forecast index -
   Integer, Intent(In) :: istn  ! - station index -
   Integer, Intent(In) :: ifld  ! - field index -
!
! Locals
!
! Local scalars
   Integer :: j ! - forecast index -
!
! Functions and Subroutines
!
! Executable Statements
!
! Print forecast odds
   Select Case (yfile%idstr)
    Case (id_grd)
      Write (Unit=ioutf, Fmt=cfmt) coor, (odds_p(ija,ifm,j,iodds+1), j=1,ng)
    Case (id_stn, id_ind)
      Write (Unit=ioutf, Fmt=cfmt) cstndy(istn,ifld), (odds_p(ija,ifm,j,iodds+1), j=1,ng)
   End Select
   printf = 0
!
   Return
 End Function print_odds
!
!
!
 Function change_fcast() &
          Result (chnge)
!
! Determines whether a change in the forecast date requires forecast recalculation
!
! Modules
   Use menus,      Only: record_change
   Use iofiles,    Only: zfile
   Use prediction, Only: ifc
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Check for changes
   If (zfile%cfile%lset) Then
      ifc = record_change()
      ifc = -1
   End If
   chnge = 2
!
   Return
 End Function change_fcast
!
!
!
 Function change_ifm() &
          Result (chnge)
!
! Changes current forecast date
!
! Modules
#if GUI == 0
   Use gui,        Only: print_text
   Use arrays,     Only: cfprds
   Use prediction, Only: ifm
#endif
!
! Function result
   Integer :: chnge
!
#if GUI == 0
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
#endif
!
! Executable Statements
!
#if GUI == 1
! Update year
   chnge = 2
#else
   Call print_text (' ')
   Call print_text ('Date: '//Trim(cfprds(ifm)%cssn(2)))
   chnge = 0
#endif
!
   Return
 End Function change_ifm
!
!
!
#if GUI == 1
 Function prompt_forecast(f_callback) &
          Result (prompt)
!
! Prompts for forecast date
!
! Modules
   Use data_io_constants, Only: lprd
   Use labels,            Only: l_fcast
   Use gui,               Only: print_cvalue, prompt_list
   Use settings,          Only: nf
   Use arrays,            Only: cfprds
   Use prediction,        Only: ifm
!
! Function result
   Integer :: prompt
!
! Arguments
!
! Procedure arguments
   Integer, External :: f_callback ! - callback function -
!
! Locals
!
! Local arrays
   Character(Len=lprd), Dimension(:), Allocatable :: cssns ! - field variables -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for forecast
   If (nf > 1) Then
      Allocate (cssns(nf))
      cssns(:) = cfprds(:)%cssn(2)
      Call prompt_list (Trim(l_fcast%c), .true., cssns, nf, ifm, &
           itab=15, iwdth=lprd, cbf1=change_ifm, cbf2=f_callback)
      Deallocate (cssns)
   Else
      Call print_cvalue (Trim(l_fcast%c), Trim(cfprds(ifm)%cssn(2)), &
           itab=15)
   End If
   prompt = 1
!
   Return
 End Function prompt_forecast
!
!
!
 Function save_pexc()
!
! Modules
   Use iofiles,    Only: exfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: save_pexc
!
! Executable Statements
!
! Save requested results
   save_pexc = save_results(ianal, exfile)
!
   Return
 End Function save_pexc
!
!
!
 Function plot_fser() &
          Result (plot)
!
! Plots forecast series graph
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot graph
   plot = plot_fcasts(img_fser(1), 1)
!
   Return
 End Function plot_fser
!
!
!
 Function plot_fens() &
          Result (plot)
!
! Plots forecast ensembles graph
!
! Modules
   Use settings, Only: nenf
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graph
   plot = plot_fcasts(img_fens(1), nenf)
!
   Return
 End Function plot_fens
!
!
!
 Function plot_fcasts(img, mf) &
          Result (plot)
!
! Plots forecast series graph
!
! Modules
   Use time,       Only: Operator(+), &
                         nse
   Use labels,     Only: cg_black_l, cg_green_l, cg_grey_l, cg_red_l, &
                         l_fcasts
   Use gui,        Only: icbw
   Use settings,   Only: iva, nf, nt, nu
   Use arrays,     Only: fprds, y, yprds
   Use regression, Only: ens_p, fcast, pls_p, ycv
   Use season,     Only: isnc
   Use plotting,   Only: plot_series
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: mf ! - number of ensemble members -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
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
   Select Case (icbw)
    Case (0)
      cylab = 'Observations ('//Trim(cg_red_l)//') / '//Trim(l_fcasts%c)//' ('//Trim(cg_green_l)//')'
    Case (1)
      cylab = 'Observations ('//Trim(cg_black_l)//') / '//Trim(l_fcasts%c)//' ('//Trim(cg_grey_l)//')'
   End Select
!
! Plot graph
   Select Case (mf)
    Case (1)
      If (nse == 1) Then
         plot = plot_series(img, Trim(cylab), nt, nu,   y(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate,    &
                                              nt, nu, ycv(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, 1, &
                df=fprds(1)%tprd(isnc)%sdate, nf=nf, mf=mf, yf=fcast(iva,1:nf,isnc:isnc), yfis=pls_p(iva,1:nf))
      Else
         plot = plot_series(img, Trim(cylab), nt, nu,   y(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate,    &
                                              nt, nu, ycv(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, 1, &
                df=fprds(1)%tprd(isnc)%sdate, nf=nf, mf=mf, yf=fcast(iva,1:nf,isnc:isnc), yfis=pls_p(iva,1:nf), nse=nse)
      End If
    Case Default
      If (nse == 1) Then
         plot = plot_series(img, Trim(cylab), nt, nu,   y(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate,    &
                                              nt, nu, ycv(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, 1, &
                df=fprds(1)%tprd(isnc)%sdate, nf=nf, mf=mf, yf=ens_p(iva,1:nf,1:mf), yfis=pls_p(iva,1:nf))
      Else
         plot = plot_series(img, Trim(cylab), nt, nu,   y(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate,    &
                                              nt, nu, ycv(iva,1:nu,isnc), yprds(1)%tprd(isnc)%sdate, 1, &
                df=fprds(1)%tprd(isnc)%sdate, nf=nf, mf=mf, yf=ens_p(iva,1:nf,1:mf), yfis=pls_p(iva,1:nf), nse=nse)
      End If
   End Select
!
   Return
 End Function plot_fcasts
!
!
!
 Function plot_pexc() &
          Result (plot)
!
! Plots probability of exceedance graph
!
! Modules
   Use data_cpt_constants, Only: nep, nts
   Use labels,             Only: ca_stds_t
   Use settings,           Only: iexc, istd, iva
   Use regression,         Only: c, e, f
   Use season,             Only: isnc
   Use categories,         Only: tobs
   Use climate,            Only: clim, nc, ncu
   Use prediction,         Only: calc_pexc
   Use graphics,           Only: init_exceed
   Use plotting,           Only: plot_exceed
!
! Function result
   Integer :: plot
!
   Character(Len=64) :: cylab ! - y-axis label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Calculate probabilities of exceedence
   If (.not.img_pexc(1)%lcontour) Call init_exceed (img_pexc(1), nc, clim(iva,1:nc,isnc))
   plot = calc_pexc(img_pexc(1)%ymin, img_pexc(1)%ymax, img_pexc(1)%scale)
!
! Prompt for modifications
   Select Case (iexc)
    Case (0)
      cylab = 'Probability of exceedance (%)'
    Case (1)
      cylab = 'Probability of non-exceedance (%)'
   End Select
   plot = plot_exceed(img_pexc(1), Trim(ca_stds_t(istd+1)), Trim(cylab), nep, f(:), e(:), c(:), ncu(1), clim(iva,1:ncu(1),1), &
          nts=nts, tobs=tobs(iva,:))
   plot = 2
!
   Return
 End Function plot_pexc
!
!
!
 Function plot_fvals() &
          Result (plot)
!
! Plots Forecast ~ Maps ~ Values results
!
! Modules
   Use data_numbers, Only: rp
   Use regression,   Only: fcast
   Use season,       Only: isnc
!
! Function result
   Integer :: plot
!
! Locals
!
! Local scalars
   Integer :: i ! - image index -
!
   Real(Kind=rp) :: vlow ! - minimum value -
   Real(Kind=rp) :: vhgh ! - maximum value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, MinVal
!
! Executable Statements
!
! Calculate data limits
   vlow = MinVal(fcast(:,:,isnc))
   vhgh = MaxVal(fcast(:,:,isnc))
!
! Plot forecast maps
   Do i = 1, 3
      plot = plot_fval(i, &
             vlow=vlow, vhgh=vhgh)
   End Do
   plot = 1
!
   Return
 End Function plot_fvals
!
!
!
 Function plot_fval(img_n, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use data_numbers,       Only: rp
   Use labels,             Only: l_fcasts
   Use settings,           Only: iskmask, rskmask
   Use arrays,             Only: fvalm=>rwk
   Use iofiles,            Only: mya
   Use fields,             Only: yfield
   Use scores,             Only: corrmsk
   Use regression,         Only: fcast, pls_p
   Use season,             Only: isnc
   Use prediction,         Only: ifm
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: img_n ! - image number -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - absolute minimum -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - absolute maximum -
!
! Locals
!
! Local scalars
   Character(Len=128) :: ctitle ! - image title -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Mask areas with insufficient skill
   Select Case (img_n)
    Case (1)
      fvalm(1:mya) = pls_p(:,ifm)%lower
      ctitle = 'Lower prediction limits'
    Case (2)
      fvalm(1:mya) = fcast(:,ifm,isnc)
      ctitle = Trim(l_fcasts%c)
    Case (3)
      fvalm(1:mya) = pls_p(:,ifm)%upper
      ctitle = 'Upper prediction limits'
   End Select
   If (iskmask > 0) Then
      Where (corrmsk(:) < rskmask) fvalm(1:mya) = yfield(1)%rmiss
   End If
!
! Plot forecast maps
   plot = plot_ydata(img_fval(img_n), icol_sk, 0, 1, fvalm(:), Trim(ctitle), &
          vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
   plot = 1
!
   Return
 End Function plot_fval
!
!
!
 Function replot_fval() &
          Result (plot)
!
! Modules
   Use regression, Only: fcast
   Use season,     Only: isnc
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, MinVal
!
! Executable Statements
!
! Replot forecast values
   If (img_fval(ifvl)%ymin == img_fval(ifvl)%ymax) Then
      plot = plot_fval(ifvl, &
             vlow=MinVal(fcast(:,:,isnc)), vhgh=MaxVal(fcast(:,:,isnc)))
   Else
      plot = plot_fval(ifvl, &
             vmin=img_fval(ifvl)%ymin, vmax=img_fval(ifvl)%ymax)
   End If
!
   Return
 End Function replot_fval
!
!
!
 Function plot_fpss() &
          Result (plot)
!
! Function result
   Integer plot
!
! Locals
!
! Local scalars
   Integer :: i ! - image index -
!
! Executable Statements
!
! Plot forecast maps
   Do i = 1, ng
      plot = plot_fps(i)
   End Do
   plot = 1
!
   Return
 End Function plot_fpss
!
!
!
 Function plot_fps(img_n, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_p2
   Use data_numbers,       Only: rp, one
   Use labels,             Only: ls_cats
   Use settings,           Only: iskmask, rskmask
   Use arrays,             Only: fpsm=>rwk
   Use iofiles,            Only: mya
   Use fields,             Only: iffy, yfield
   Use scores,             Only: corrmsk
   Use categories,         Only: fps_p
   Use prediction,         Only: ifm
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: img_n ! - image number -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - absolute minimum -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - absolute maximum -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: tmiss ! - missing value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Reset missing values
   tmiss = yfield(iffy)%rmiss
   yfield(iffy)%rmiss = -one
!
! Mask areas with insufficient skill
   fpsm(1:mya) = fps_p(:,ifm,img_n)
   If (iskmask > 0) Then
      Where (corrmsk(:) < rskmask) fpsm(1:mya) = yfield(1)%rmiss
   End If
!
! Plot forecast maps
   plot = plot_ydata(img_fps(img_n), icol_p2, 2, 1, fpsm(1:mya), 'Probabilities of '//Trim(ls_cats(img_n)%c), &
          vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
   yfield(iffy)%rmiss = tmiss
!
   Return
 End Function plot_fps
!
!
!
 Function replot_fps() &
          Result (plot)
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Replot forecast probabilities
   If (img_fps(ifps)%ymin == img_fps(ifps)%ymax) Then
      plot = reset_img_fps()
   End If
   plot = plot_fps(ifps, &
          vmin=img_fps(ifps)%ymin, vmax=img_fps(ifps)%ymax)
!
   Return
 End Function replot_fps
!
!
!
 Function plot_oddss() &
          Result (plot)
!
! Modules
   Use data_numbers, Only: rp
   Use settings,     Only: iodds
   Use categories,   Only: odds_p
!
! Function result
   Integer plot
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: vlow ! - minimum value -
   Real(Kind=rp) :: vhgh ! - maximum value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, MinVal
!
! Executable Statements
!
! Calculate data limits
   vlow = MinVal(odds_p(:,:,:,iodds+1))
   vhgh = MaxVal(odds_p(:,:,:,iodds+1))
!
! Plot forecast odds maps
   Do iodd = 1, ng
      plot = plot_odds(vlow=vlow, vhgh=vhgh)
   End Do
   plot = 1
!
   Return
 End Function plot_oddss
!
!
!
 Function plot_odds(vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use data_numbers,       Only: rp, one
   Use settings,           Only: iodds, iskmask, rskmask
   Use arrays,             Only: oddsm=>rwk
   Use iofiles,            Only: mya
   Use fields,             Only: iffy, yfield
   Use scores,             Only: corrmsk
   Use categories,         Only: odds_p
   Use prediction,         Only: ifm
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - minimum value -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - maximum value -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: tmiss ! - backup missing value -
!
   Character(Len=26) :: codds ! - odds / relative odds -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Reset missing values
   tmiss = yfield(iffy)%rmiss
   yfield(iffy)%rmiss = -one
!
! Mask areas with insufficient skill
   oddsm(1:mya) = odds_p(:,ifm,iodd,iodds+1)
   If (iskmask > 0) Then
      Where (corrmsk(:) < rskmask) oddsm(1:mya) = yfield(1)%rmiss
   End If
!
! Plot forecast odds maps
   Select Case (iodds)
    Case (0)
      codds = 'Forecast odds'
    Case Default
      codds = 'Forecast relative odds'
   End Select
   plot = plot_ydata(img_odds(iodd), icol_sk, 0, 1, oddsm(1:mya), Trim(codds), &
          vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
   yfield(iffy)%rmiss = tmiss
   plot = 1
!
   Return
 End Function plot_odds
!
!
!
 Function replot_odds() &
          Result (plot)
!
! Modules
   Use settings,   Only: iodds
   Use categories, Only: odds_p
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, MinVal
!
! Executable Statements
!
! Plot map
   If (img_odds(iodd)%ymin == img_odds(iodd)%ymax) Then
      plot = plot_odds(vlow=MinVal(odds_p(:,:,:,iodds+1)), vhgh=MaxVal(odds_p(:,:,:,iodds+1)))
   Else
      plot = plot_odds(vmin=img_odds(iodd)%ymin, vmax=img_odds(iodd)%ymax)
   End If
!
   Return
 End Function replot_odds
!
!
!
 Function plot_hps() &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_p3
   Use categories,         Only: hps
   Use prediction,         Only: ifm
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer plot
!
! Executable Statements
!
! Plot forecast maps
   plot = plot_ydata(img_hps(1), icol_p3, 0, 3, hps(:,ifm), ' ')
!
   Return
 End Function plot_hps
!
!
!
 Function select_fval() &
          Result (selectf)
!
! Opens Forecast ~ Series for a selected point on a forecast values map
!
! Modules
   Use graphics,    Only: select_location
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: selectf
!
! Executable Statements
!
! Select location
   selectf = select_location(img_fval(imgid+1-img_fval(1)%id), fcst_fser)
!
   Return
 End Function select_fval
!
!
!
 Function select_fps() &
          Result (selectf)
!
! Opens Forecast ~ Series for a selected point on a forecast probabilities map
!
! Modules
   Use graphics,    Only: select_location
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: selectf
!
! Executable Statements
!
! Select location
   selectf=select_location(img_fps(imgid+1-img_fps(1)%id),fcst_fser)
!
   Return
 End Function select_fps
!
!
!
 Function select_odds() &
          Result (selectf)
!
! Opens Forecast ~ Series for a selected point on a forecast odds map
!
! Modules
   Use graphics,    Only: select_location
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: selectf
!
! Executable Statements
!
! Select location
   selectf = select_location(img_odds(imgid+1-img_odds(1)%id), fcst_fser)
!
   Return
 End Function select_odds
!
!
!
 Function select_hps() &
          Result (selectf)
!
! Opens Forecast ~ Series for a selected point on a probabilistic forecast map
!
! Modules
   Use graphics, Only: select_location
!
! Function result
   Integer :: selectf
!
! Executable Statements
!
! Select location
   selectf = select_location(img_hps(1), fcst_fser)
!
   Return
 End Function select_hps
!
!
!
 Function f_title() &
          Result (title)
!
! Prompts for new title
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
   title = f_replot(.false., get_title)
!
   Return
 End Function f_title
!
!
!
 Function f_limits() &
          Result (limits)
!
! Prompts for contour limits
!
! Modules
   Use plotting, Only: get_limits
!
! Function result
   Integer :: limits
!
! Executable Statements
!
! Prompt for contour limits
   limits = f_replot(.true., get_limits)
!
   Return
 End Function f_limits
!
!
!
 Function f_replot(lcontour, get_custom) &
          Result (replot)
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
! - input scalars -
   Logical, Intent(In) :: lcontour ! - recalculate contours? -
!
! - procedure arguments -
   Interface
      Subroutine get_custom (img, lreset)
         Use graphics, Only: image
         Type(image), Intent(InOut) :: img ! - image -
         Logical, Intent(Out) :: lreset ! - reset? -
      End Subroutine get_custom
   End Interface
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Replot customised forecast diagrams
   If (img_fser(1)%id == imgid) Then ! - Forecast ~ Series -
      img_fser(1)%lcontour = lcontour
      replot = replot_graph(img_fser(1), get_custom, plot_fser)
   Else If (img_fens(1)%id == imgid) Then ! - Forecast ~ Ensembles -
      img_fens(1)%lcontour = lcontour
      replot = replot_graph(img_fens(1), get_custom, plot_fens)
   Else If (img_pexc(1)%id == imgid) Then ! - Forecast ~ Exceedance -
      img_pexc(1)%lcontour = lcontour
      replot = replot_graph(img_pexc(1), get_custom, plot_pexc)
   Else If (Any(img_fval(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Values -
      ifvl = imgid + 1 - img_fval(1)%id
      img_fval(ifvl)%lcontour = lcontour
      replot = replot_graph(img_fval(ifvl), get_custom, replot_fval)
   Else If (Any(img_fps(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Probabilities -
      ifps = imgid + 1 - img_fps(1)%id
      img_fps(ifps)%lcontour = lcontour
      replot = replot_graph(img_fps(ifps), get_custom, replot_fps)
   Else If (img_hps(1)%id == imgid) Then ! - Forecast ~ Maps ~ Probabilistic -
      img_hps(1)%lcontour = lcontour
      replot = replot_graph(img_hps(1), get_custom, plot_hps)
   Else If (Any(img_odds(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Odds -
      iodd = imgid + 1 - img_odds(1)%id
      img_odds(iodd)%lcontour = lcontour
      replot = replot_graph(img_odds(iodd), get_custom, replot_odds)
   End If
!
   Return
 End Function f_replot
!
!
!
 Function f_export() &
          Result (export)
!
! Saves images
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so, isq_yr
   Use labels,              Only: cg_ensemble, &
                                  l_fcast, &
                                  ls_cats
   Use time,                Only: iseq
   Use settings,            Only: iv
   Use arrays,              Only: fprds
   Use iofiles,             Only: ny
   Use season,              Only: isnc
   Use prediction,          Only: ifm
   Use graphics,            Only: save_graphic
   Use windows_gui,         Only: imgid
!
! Function result
   Integer :: export
!
! Locals
!
! Local scalars
   Integer :: ifmy ! - current forecast index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any, Trim
!
! Executable Statements
!
! Prompt for montage title
   If (imgid < 0) Then
      export = f_export_montage(-imgid)
      Return
   End If
!
! Identify index
   Select Case (iseq)
    Case (isq_yr)
      ifmy = fprds(ifm)%tprd(isnc)%sdate%iyr
    Case (isq_so, isq_sn)
      ifmy = fprds(ifm)%tprd(isnc)%sdate%iyr*100 + fprds(ifm)%tprd(isnc)%sdate%imn
    Case Default
      ifmy = fprds(ifm)%tprd(isnc)%sdate%iyr*10000 + fprds(ifm)%tprd(isnc)%sdate%imn*100 + fprds(ifm)%tprd(isnc)%sdate%idy
   End Select 
!
! Save graph
   If (imgid == img_fser(1)%id) Then ! - Forecast ~ Series -
      export = save_graphic(Trim(l_fcast%c), img_fser(1), &
               imap=iv, mmap=ny)
   Else If (imgid == img_pexc(1)%id) Then ! - Forecast ~ Ensembles -
      export = save_graphic('Exceedance_probability', img_pexc(1), &
               imap=ifmy)
   Else If (imgid == img_fens(1)%id) Then ! - Forecast ~ Ensembles -
      export = save_graphic(Trim(cg_ensemble), img_fens(1), &
               imap=iv, mmap=ny)
   Else If (imgid == img_fval(1)%id) Then ! - Forecast ~ Maps ~ Values -
      ifvl = 1
      export = save_graphic('Lower_map', img_fval(ifvl), &
               imap=ifmy)
   Else If (imgid == img_fval(2)%id) Then
      ifvl = 2
      export = save_graphic('Forecast_map', img_fval(ifvl), &
               imap=ifmy)
   Else If (imgid == img_fval(3)%id) Then
      ifvl = 3
      export = save_graphic('Upper_map', img_fval(ifvl), &
               imap=ifmy)
   Else If (Any(img_fps(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Probabilities -
      ifps = imgid + 1 - img_fps(1)%id
      export = save_graphic(Trim(ls_cats(ifps)%c)//'_map', img_fps(ifps), &
               imap=ifmy)
   Else If (imgid == img_hps(1)%id) Then ! - Forecast ~ Maps ~ Probabilistic -
      export = save_graphic('Probabilistic_map', img_hps(1), &
               imap=ifmy)
   Else If (Any(img_odds(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Odds -
      iodd = imgid + 1 - img_odds(1)%id
      export = save_graphic(Trim(ls_cats(iodd)%c)//'_map', img_odds(iodd), &
               imap=ifmy)
   End If
!
   Return
 End Function f_export
!
!
!
 Function f_export_montage(imgid) &
          Result (export)
!
! Modules
   Use labels,   Only: l_fcasts, l_odds, l_probs
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
   If (imgid == img_fval(1)%id) Then ! - Forecast ~ Maps ~ Values -
      export = export_montage(l_fcasts%c, img_fval(1), img_fval(2), &
               lsquare=.true., img_3=img_fval(3))
   Else If (img_fps(1)%id == imgid) Then ! - Forecast ~ Maps ~ Probabilities -
      export = export_montage(l_probs%c, img_fps(1), img_fps(2), &
               lsquare=.true., img_3=img_fps(3))
   Else If (img_odds(1)%id == imgid) Then ! - Forecast ~ Maps ~ Odds -
      export = export_montage(l_odds%c, img_odds(1), img_odds(2), &
               lsquare=.true., img_3=img_odds(3))
   End If
!
   Return
 End Function f_export_montage
!
!
!
 Function help_fcast()
!
! Modules
   Use help,        Only: help_file
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: help_fcast
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Open context-specific help pages
   If (img_fser(1)%id == imgid) Then ! - Forecast ~ Series -
      Call help_file ('Tools_Forecast_Series.html')
   Else If (img_fens(1)%id == imgid) Then ! - Forecast ~ Ensembles -
      Call help_file ('Tools_Forecast_Ensemble.html')
   Else If (img_pexc(1)%id == imgid) Then ! - Forecast ~ Exceedance -
      Call help_file ('Tools_Forecast_Exceed.html')
   Else If (Any(img_fval(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Values -
      Call help_file ('Tools_Forecast_Values.html')
   Else If (Any(img_fps(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Probabilities -
      Call help_file ('Tools_Forecast_Probs.html')
   Else If (img_hps(1)%id == imgid) Then ! - Forecast ~ Maps ~ Probabilistic -
      Call help_file ('Tools_Forecast_Prob.html')
   Else If (Any(img_odds(:)%id == imgid)) Then ! - Forecast ~ Maps ~ Odds -
      Call help_file ('Tools_Forecast_Odds.html')
   End If
   help_fcast = 2
!
   Return
 End Function help_fcast
#endif
!
!
!
 Subroutine parameter_write_forecast (iout, ifail)
!
! Writes menu activations to parameter file
!
! Modules
   Use menus,          Only: mn_tool_fser, mn_tool_fens, mn_tool_pexc, mn_tool_fval, mn_tool_fps, mn_tool_odds
   Use parameter_file, Only: output_parameter
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
! Indicate activated menu items
   If (mn_tool_fser%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_tool_fser%l_lab%c), i=mn_tool_fser%id) /= 0) GoTo 1
   End If
   If (mn_tool_fens%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_tool_fens%l_lab%c), i=mn_tool_fens%id) /= 0) GoTo 1
   End If
   If (mn_tool_pexc%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_tool_pexc%l_lab%c), i=mn_tool_pexc%id) /= 0) GoTo 1
   End If
   If (mn_tool_fval%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_tool_fval%l_lab%c), i=mn_tool_fval%id) /= 0) GoTo 1
   End If
   If (mn_tool_fps%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_tool_fps%l_lab%c), i=mn_tool_fps%id) /= 0) GoTo 1
   End If
   If (mn_tool_odds%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_tool_odds%l_lab%c), i=mn_tool_odds%id) /= 0) GoTo 1
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
 End Subroutine parameter_write_forecast
End Module forecast
