! Author: Simon Mason
Module cpt_output
!
! Modules
   Use data_numbers,      Only: rp
   Use data_io_constants, Only: lvar
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: get_gi_fmt, output_value, save_data, save_good, save_results, update_results, write_results
#if GUI == 0
   Public :: print_results, tags_lab
#endif
!
! Scalars
!
! Integer scalars
   Integer, Private :: ihp1 = 1 ! - property sheet 1 -
   Integer, Private :: ihp2 = 2 ! - property sheet 2 -
   Integer, Private :: ihp3 = 3 ! - property sheet 3 -
   Integer, Private :: ihp4 = 4 ! - property sheet 4 -
   Integer, Private :: ihp5 = 5 ! - property sheet 5 -
   Integer, Private :: ihp6 = 6 ! - property sheet 6 -
!
   Integer, Public :: ipreclx ! - precision for X latitudes and longitudes -
   Integer, Public :: iprecly ! - precision for Y latitudes and longitudes -
!
   Character(Len=lvar+1), Private :: cout ! - output field -
!
Contains
!
!
 Function save_data(ianal, ihind, ifc)
!
! Prompts for output files and saves requested results
!
! Modules
#if GUI == 1
   Use clrwin$,            Only: update_window$, winio$
   Use data_numbers,       Only: one, two
   Use data_cpt_constants, Only: ig_skl
#endif
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, it_gam
   Use labels,             Only: cg_indata_t, &
                                 l_data, l_fcasts, l_modes, l_outres
#if GUI == 1
   Use labels,             Only: l_close, l_edit, l_file, l_options
   Use gui,                Only: ih_con, iw, &
                                 set_text_size, win_prompt
#else
   Use labels,             Only: l_exit
   Use gui,                Only: print_text, print_warning, prompt_integer
#endif
   Use gui,                Only: box_close, box_open, gui_creturn, init_psheet, init_win, window_end
#if GUI == 1
   Use menus,              Only: add_menu
#endif
   Use settings,           Only: icv2, igauss, iretro
#if GUI == 1
   Use settings,           Only: igcms
   Use iofiles,            Only: itags,  &
                                 files_reset, get_fmt
#else
   Use iofiles,            Only: new_ids, nfile, &
                                 geto_file
#endif
   Use iofiles,            Only: avfile, ccfile, cofile, cvfile, erfile, e2file, fcfile, fefile, fofile, fpfile, fsfile, fvfile, &
                                 fxfile, gtfile, lpfile, lrfile, l2file, ocfile, orfile, o2file, pbfile, pcfile, prfile, p2file, &
                                 rbfile, rcfile, sdfile, thfile, tyfile, vrfile, v2file, xdfile, xefile, xifile, xlfile, xmfile, &
                                 xsfile, xtfile, ycfile, ydfile, yefile, yffile, yhfile, ylfile, ymfile, yofile, ypfile, yrfile, &
                                 ysfile, ytfile, y2file, zifile, &
                                 prompt_results_file
   Use scores,             Only: lcorr
   Use season,             Only: lprst
!
! Function result
   Integer :: save_data
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: ifc   ! - recalculate forecast flag -
   Integer, Intent(In) :: ihind ! - hindcasts flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
#if GUI == 0
   Intrinsic Ubound
!
! Executable Statements
!
! Get output files
1  Call init_win (title=Trim(l_outres%c), &
        lresize=.true.)
#endif
!
! Create input data property sheet
   Call init_psheet (ihp1, Trim(cg_indata_t))
   Call box_open (.false., .true.)
   Call box_open (.true., .false., &
        title=Trim(l_data%c))
   Call prompt_results_file (.true., xdfile)        ! - X data -
   If (ianal == ia_gcm .or. ianal == ia_pfv) Then
      Call prompt_results_file (.false., xifile)    ! - interpolated X data -
   End If
   Call prompt_results_file (.false., ydfile)       ! - Y data -
   If (lprst) Then
      Call prompt_results_file (.false., yofile)    ! - Y data observed component -
      Call prompt_results_file (.false., ypfile)    ! - Y data persisted + predicted component -
   End If
   If (igauss /= 0) Then
      Call prompt_results_file (.false., tyfile)    ! - transformed Y data -
      If (igauss == it_gam) Then
         Call prompt_results_file (.false., gtfile) ! - gamma transformation parameters Y data -
      End If
   End If
   Select Case (ianal)
    Case Default
      Call prompt_results_file (.false., ycfile)    ! - Y categories -
      If (iretro == 1) Then
         Call prompt_results_file (.false., rcfile) ! - retroactive Y categories -
      End If
    Case (ia_pfv)
      Call prompt_results_file (.false., rcfile)    ! - target period Y categories -
   End Select
   Call box_close (.true.)
   Call box_close (.false.)
   Call window_end ()
!
! Create hindcasts property sheet
   If (ihind == 1) Then
      Call init_psheet (ihp2, 'Hindcasts')
      Call box_open (.false., .true.)
      Call box_open (.true., .false., &
           title='Predictions')
      Call prompt_results_file (.true., yhfile)     ! - cross-validated predictions -
      Call prompt_results_file (.false., yffile)    ! - fitted values -
      If (iretro == 1) Then
         Call prompt_results_file (.false., yrfile) ! - retroactive predictions -
         Call prompt_results_file (.false., erfile) ! - retroactive ensemble predictions -
         Call prompt_results_file (.false., lrfile) ! - retroactive prediction limits -
         Call prompt_results_file (.false., prfile) ! - retroactive forecast probabilities -
         Call prompt_results_file (.false., orfile) ! - retroactive odds -
         Call prompt_results_file (.false., vrfile) ! - retroactive prediction error variances -
      End If
      If (icv2 > 0) Then
         Call prompt_results_file (.false., y2file) ! - double cross-validated predictions -
         Call prompt_results_file (.false., e2file) ! - double cross-validated ensemble predictions -
         Call prompt_results_file (.false., l2file) ! - double cross-validated prediction limits -
         Call prompt_results_file (.false., p2file) ! - double cross-validated probabilitistic predictions -
         Call prompt_results_file (.false., o2file) ! - double cross-validated odds -
         Call prompt_results_file (.false., v2file) ! - double cross-validated prediction error variances -
      End If
      Call box_close (.true.)
      Call box_close (.false.)
      Call window_end ()
   End If
!
! Create EOF property sheet
   Select Case (ianal)
    Case (ia_cca, ia_pcr)
      Call init_psheet (ihp3, Trim(l_modes%c))
! - X EOF files -
      Call box_open (.false., .true.)
      Call box_open (.true., .false., &
           title='Prefiltering')
      Call box_open (.false., .false., &
           title='X Modes files')
      Call prompt_results_file (.true., xefile)     ! - X eigenvalues -
      Call prompt_results_file (.false., xlfile)    ! - X spatial loadings -
      Call prompt_results_file (.false., xsfile)    ! - X temporal scores -
      Call box_close (.false.)
! - Y EOF files -
      If (ianal == ia_cca) Then
         Call gui_creturn (.false.)
         Call box_open (.false., .false., &
              title='Y Modes files')
         Call prompt_results_file (.true., yefile)  ! - Y eigenvalues -
         Call prompt_results_file (.false., ylfile) ! - Y spatial loadings -
         Call prompt_results_file (.false., ysfile) ! - Y temporal scores -
         Call box_close (.false.)
      End If
      Call box_close (.true.)
      Call box_close (.false.)
      Call window_end ()
   End Select
!
! Create CCA property sheet
   Select Case (ianal)
    Case (ia_cca)
      Call init_psheet (ihp4, 'CCA Results')
      Call box_open (.false., .true.)
      Call box_open (.true., .false., &
           title='CCA files')
      Call box_open (.false., .false., &
           title='Canonical correlations')
      Call prompt_results_file (.true., ccfile)  ! - canonical correlations -
      Call box_close (.false.)
      Call gui_creturn (.false.)
      Call box_open (.false., .false., &
           title='X CCA maps')
      Call prompt_results_file (.true., xmfile)  ! - X homogeneous covariance maps -
      Call prompt_results_file (.false., xtfile) ! - X homogeneous covariance maps time series -
      Call box_close (.false.)
      Call gui_creturn (.false.)
      Call box_open (.false., .false., &
           title='Y CCA maps')
      Call prompt_results_file (.true., ymfile)  ! - Y homogeneous covariance maps -
      Call prompt_results_file (.false., ytfile) ! - Y homogeneous covariance maps time series -
      Call box_close (.false.)
      Call box_close (.true.)
      Call box_close (.false.)
      Call window_end ()
!
! Create PCR property sheet
    Case (ia_pcr)
      Call init_psheet (ihp4, 'PCR Results')
! - regression output files -
      Call box_open (.false., .true.)
      Call box_open (.true., .false., &
           title='Regression files')
      Call prompt_results_file (.true., rbfile)  ! - regression coefficients -
      Call prompt_results_file (.false., pbfile) ! - PC regression coefficients -
      Call box_close (.true.)
      Call box_close (.false.)
      Call window_end ()
!
! Create MLR/GCM property sheet
    Case (ia_mlr, ia_gcm)
      If (ianal == ia_mlr) Then
         Call init_psheet (ihp4, 'MLR Results')
      Else
         Call init_psheet (ihp4, 'GCM Results')
      End If
! - regression output files -
      Call box_open (.false., .true.)
      Call box_open (.true., .false., &
           title='Regression files')
      Call prompt_results_file (.true., rbfile)  ! - regression coefficients -
      Call box_close (.true.)
      Call box_close (.false.)
      Call window_end ()
   End Select
!
! Create forecasts property sheet
   If (ifc == 2) Then
      Call init_psheet (ihp5, Trim(l_fcasts%c))
      Call box_open (.false., .true.)
      Call box_open (.true., .false., &
           title=Trim(l_fcasts%c))
      Call box_open (.false., .false., &
           title='Probabilistic forecasts')
      Call prompt_results_file (.true., fpfile)        ! - forecast probabilities -
      Call prompt_results_file (.false., fofile)       ! - forecast odds -
      Call box_close (.false.)
      If (ihind == 1) Then
         Call gui_creturn (.false.)
         Call box_open (.false., .false., &
              title='Deterministic forecasts')
         Call prompt_results_file (.true., fvfile)     ! - forecasts -
         If (lprst) Then
            Call prompt_results_file (.false., ocfile) ! - observed component -
            Call prompt_results_file (.false., pcfile) ! - predicted component -
         End If
         Call prompt_results_file (.false., fsfile)    ! - forecast ensembles -
         Call prompt_results_file (.false., lpfile)    ! - prediction limits -
         Call prompt_results_file (.false., fefile)    ! - prediction-error variances -
         Call box_close (.false.)
         Call gui_creturn (.false.)
         Call box_open (.false., .false., &
              title='Predictors')
         Select Case (ianal)
          Case (ia_cca)
            Call prompt_results_file (.true., fxfile)  ! - predictor PC scores -
            Call prompt_results_file (.false., fcfile) ! - predictor CCA time scores -
          Case (ia_pcr)
            Call prompt_results_file (.true., fxfile)  ! - predictor PC scores -
          Case (ia_gcm)
            Call prompt_results_file (.true., zifile)  ! - interpolated X data -
         End Select
         Call box_close (.false.)
      End If
      Call box_close (.true.)
      Call box_close (.false.)
      Call window_end ()
   End If
!
! Create climatologies property sheet
   Call init_psheet (ihp6, 'Climatologies')
   Call box_open (.false., .true.)
   Call box_open (.true., .false., &
        title='Climatologies')
   Call prompt_results_file (.true., thfile)     ! - thresholds -
   Call prompt_results_file (.false., avfile)    ! - averages -
   Call prompt_results_file (.false., sdfile)    ! - standard deviations -
   Call prompt_results_file (.false., cvfile)    ! - coefficients of variation -
   If (lcorr) Then
      Call prompt_results_file (.false., cofile) ! - correlations -
   End If
   Call box_close (.true.)
   Call box_close (.false.)
   Call window_end ()
!
#if GUI == 1
! Add menu bar
   Call init_win (title=Trim(l_outres%c), &
        lresize=.true.)
! - file menu -
   Call add_menu ('mn', 1,  -1, l_file)
   Call add_menu ('mn', 2,   0, l_close, &
        scbf='Exit')
! - edit menu -
   Call add_menu ('mn', 1,  -1, l_edit)
   Call add_menu ('mn', 2, 131, '&File formats', &
        cbf=get_fmt)
! - options menu -
   Call add_menu ('mn', 1,  -1, l_options)
   Call add_menu ('mn', 2, 133, 'Include CPT &tags', &
        icheck=itags, itoggle=itags)
!
! Get output files
   Call set_text_size (two)
   iw = winio$('%cn%bfOutput files%`bf&')
   Call set_text_size (one)
!
! Add property sheets
   Select Case (ianal)
    Case (ia_cca, ia_pcr) ! - CCA, PCR -
      If (ifc == 2) Then
         iw = winio$('%nl%nl%6ps&', ihp1, ihp6, ihp3, ihp4, ihp2, ihp5)
      Else
         iw = winio$('%nl%nl%5ps&', ihp1, ihp6, ihp3, ihp4, ihp2)
      End If
    Case (ia_mlr) ! - MLR -
      If (ifc == 2) Then
         iw = winio$('%nl%nl%5ps&', ihp1, ihp6, ihp4, ihp2, ihp5)
      Else
         iw = winio$('%nl%nl%4ps&', ihp1, ihp6, ihp4, ihp2)
      End If
    Case (ia_gcm) ! - GCM -
      If (igcms == ig_skl) Then
         If (ifc == 2) Then
            iw = winio$('%nl%nl%5ps&', ihp1, ihp6, ihp4, ihp2, ihp5)
         Else
            iw = winio$('%nl%nl%4ps&', ihp1, ihp6, ihp4, ihp2)
         End If
      Else
         If (ifc == 2) Then
            iw = winio$('%nl%nl%4ps&', ihp1, ihp6, ihp2, ihp5)
         Else
            iw = winio$('%nl%nl%3ps&', ihp1, ihp6, ihp2)
         End If
      End If
    Case (ia_pfv) ! - PFV -
      If (igcms == ig_skl) Then
         If (ifc == 2) Then
            iw = winio$('%nl%nl%4ps&', ihp1, ihp6, ihp4, ihp5)
         Else
            iw = winio$('%nl%nl%3ps&', ihp1, ihp6, ihp4)
         End If
      Else
         If (ifc == 2) Then
            iw = winio$('%nl%nl%3ps&', ihp1, ihp6, ihp5)
         Else
            iw = winio$('%nl%nl%2ps&', ihp1, ihp6)
         End If
      End If
   End Select
!
! Prompt
   iw = win_prompt(.true.)
#else
   Call print_text (' ')
   Write (Unit=*, Fmt='(A)') ' 0. '//Trim(l_exit%c)
   Call print_text (' ')
#endif
!
#if GUI == 1
! Save requested results
   Select Case (iw)
    Case (1)
      Call write_results (ianal)
    Case Default
      Call files_reset ()
   End Select
!
! Restore console
   Call update_window$ (ih_con)
   save_data = 2
!
#else
! Identify file
   Call prompt_integer (' ', nfile)
2  If (nfile == 0) Then
      Call write_results (ianal)
      save_data = 0
      Return
! - convert old responses -
   Else if (nfile > 0 .and. nfile <= Ubound(new_ids, Dim=1)) Then
      nfile = new_ids(nfile)
      Call print_warning (.true., 'Old file identifier has been converted.', &
           nopause=.true.)
      GoTo 2
! - prevent unavailable files being opened -
   Else
      If (nfile == xifile%nfile) Then      ! - interpolated X -
         If (ianal /= ia_gcm .and. ianal /= ia_pfv) GoTo 1
      Else If (nfile == yofile%nfile) Then ! - Y input data observed component -
         If (.not.lprst) GoTo 1
      Else If (nfile == ypfile%nfile) Then ! - Y input data persisted + predicted component -
         If (.not.lprst) GoTo 1
      Else If (nfile == tyfile%nfile) Then ! - transformed Y input data -
         If (igauss == 0) GoTo 1
      Else If (nfile == gtfile%nfile) Then ! - gamma transformation parameters -
         If (igauss /= it_gam) GoTo 1
      Else If (nfile == ycfile%nfile) Then ! - Y categories -
         If (ianal == ia_pfv) GoTo 1
      Else If (nfile == rcfile%nfile) Then ! - retroactive Y categories -
         If (iretro /= 1) GoTo 1
      Else If (nfile == yhfile%nfile) Then ! - cross-validated predictions -
         If (ihind /= 1) GoTo 1
      Else If (nfile == yffile%nfile) Then ! - fitted values -
         If (ihind /= 1) GoTo 1
      Else If (nfile == yrfile%nfile) Then ! - retroactive predictions -
         If (iretro /= 1 .or. ihind /= 1) GoTo 1
      Else If (nfile == erfile%nfile) Then ! - retroactive ensemble predictions -
         If (iretro /= 1 .or. ihind /= 1) GoTo 1
      Else If (nfile == lrfile%nfile) Then ! - retroactive prediction limits -
         If (iretro /= 1 .or. ihind /= 1) GoTo 1
      Else If (nfile == prfile%nfile) Then ! - retroactive forecast probabilities -
         If (iretro /= 1 .or. ihind /= 1) GoTo 1
      Else If (nfile == orfile%nfile) Then ! - retroactive odds -
         If (iretro /= 1 .or. ihind /= 1) GoTo 1
      Else If (nfile == vrfile%nfile) Then ! - retroactive prediction error variances -
         If (iretro /= 1 .or. ihind /= 1) GoTo 1
      Else If (nfile == y2file%nfile) Then ! - double cross-validated predictions -
         If (icv2 == 0 .or. ihind /= 1) GoTo 1
      Else If (nfile == e2file%nfile) Then ! - double cross-validated ensemble predictions -
         If (icv2 == 0 .or. ihind /= 1) GoTo 1
      Else If (nfile == l2file%nfile) Then ! - double cross-validated prediction limits -
         If (icv2 == 0 .or. ihind /= 1) GoTo 1
      Else If (nfile == p2file%nfile) Then ! - double cross-validated probabilitistic predictions -
         If (icv2 == 0 .or. ihind /= 1) GoTo 1
      Else If (nfile == o2file%nfile) Then ! - double cross-validated odds -
         If (icv2 == 0 .or. ihind /= 1) GoTo 1
      Else If (nfile == v2file%nfile) Then ! - double cross-validated prediction error variances -
         If (icv2 == 0 .or. ihind /= 1) GoTo 1
      Else If (nfile == xefile%nfile) Then ! - X eigenvalues -
         If (ianal /= ia_cca .and. ianal /= ia_pcr) GoTo 1
      Else If (nfile == xlfile%nfile) Then ! - X spatial loadings -
         If (ianal /= ia_cca .and. ianal /= ia_pcr) GoTo 1
      Else If (nfile == xsfile%nfile) Then ! - X temporal scores -
         If (ianal /= ia_cca .and. ianal /= ia_pcr) GoTo 1
      Else If (nfile == yefile%nfile) Then ! - Y eigenvalues -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == ylfile%nfile) Then ! - Y spatial loadings -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == ysfile%nfile) Then ! - Y temporal scores -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == ccfile%nfile) Then ! - canonical correlations -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == xmfile%nfile) Then ! - X homogeneous covariance maps -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == xtfile%nfile) Then ! - X homogeneous covariance maps time series -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == ymfile%nfile) Then ! - Y homogeneous covariance maps -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == ytfile%nfile) Then ! - Y homogeneous covariance maps time series -
         If (ianal /= ia_cca) GoTo 1
      Else If (nfile == rbfile%nfile) Then ! - regression coefficients -
         If (ianal /= ia_pcr .and. ianal /= ia_mlr .and. ianal /= ia_gcm) GoTo 1
      Else If (nfile == pbfile%nfile) Then ! - PC regression coefficients -
         If (ianal /= ia_pcr) GoTo 1
      Else If (nfile == fpfile%nfile) Then ! - forecast probabilities -
         If (ifc /= 2) GoTo 1
      Else If (nfile == fofile%nfile) Then ! - forecast odds -
         If (ifc /= 2) GoTo 1
      Else If (nfile == fvfile%nfile) Then ! - forecasts -
         If (ifc /= 2 .or. ihind /= 1) GoTo 1
      Else If (nfile == ocfile%nfile) Then ! - observed component of forecasts -
         If (ifc /= 2 .or. ihind /= 1 .or. .not.lprst) GoTo 1
      Else If (nfile == pcfile%nfile) Then ! - predicted component of forecasts -
         If (ifc /= 2 .or. ihind /= 1 .or. .not.lprst) GoTo 1
      Else If (nfile == fsfile%nfile) Then ! - forecast ensembles -
         If (ifc /= 2 .or. ihind /= 1) GoTo 1
      Else If (nfile == lpfile%nfile) Then ! - prediction limits -
         If (ifc /= 2 .or. ihind /= 1) GoTo 1
      Else If (nfile == fefile%nfile) Then ! - prediction-error variances -
         If (ifc /= 2 .or. ihind /= 1) GoTo 1
      Else If (nfile == fxfile%nfile) Then ! - predictor time scores -
         If (ifc /= 2 .or. (ianal /= ia_cca .and. ianal /= ia_pcr)) GoTo 1
      Else If (nfile == zifile%nfile) Then ! - interpolated forecast predictors -
         If (ifc /= 2 .or. ianal /= ia_gcm) GoTo 1
      Else If (nfile == fcfile%nfile) Then ! - predictor CCA time scores -
         If (ifc /= 2 .or. ianal /= ia_cca) GoTo 1
      Else If (nfile == cofile%nfile) Then ! - correlations -
         If (.not.lcorr) GoTo 1
      End If
      save_data = geto_file()
      GoTo 1
   End If
!
#endif
   Return
 End Function save_data
!
!
!
 Function save_good()
!
! Prompts for goodness index output file
!
! Modules
   Use iofiles, Only: gifile, &
                      geto_gen
!
! Function result
   Integer :: save_good
!
! Prompt for goodness index
   Call geto_gen (gifile)
   save_good = 2
!
   Return
 End Function save_good
!
!
!
#if GUI == 1
 Function save_results(ianal, outfile)
!
! Prompts for results files
!
! Modules
   Use clrwin$, Only: update_window$
   Use labels,  Only: l_close, l_edit, l_file, l_outres
   Use gui,     Only: ih_con, iw, &
                      init_win, prompt_file, win_prompt
   Use menus,   Only: add_menu
   Use iofiles, Only: ofile, &
                      nfile, &
                      files_reset, get_fmt, geto_file
!
! Function result
   Integer :: save_results
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: outfile
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Open window
   Call init_win (title=Trim(l_outres%c), &
        lresize=.true.)
!
! Add menu bar
   Call add_menu ('mn', 1,  -1, l_file)
   Call add_menu ('mn', 2,   0, l_close, &
        scbf='EXIT')
   Call add_menu ('mn', 1,  -1, l_edit)
   Call add_menu ('mn', 2, 133, '&File format', &
        iset1=nfile, ival1=outfile%nfile, cbf=get_fmt)
!
! Get output file
   Call prompt_file (geto_file, Trim(outfile%cfile%desc), outfile%nfile, outfile%cfile%fname, nfile)
! - prompt -
   iw = win_prompt(.true.)
!
! Save requested results
   Select Case (iw)
    Case (2)
      Call write_results (ianal)
    Case Default
      Call files_reset ()
   End Select
!
! Restore console
   Call update_window$ (ih_con)
   save_results = 2
!
   Return
 End Function save_results
!
!
!
#else
 Function save_results(ianal, outfile1, &
          outfile2)
!
! Prompts for results files
!
! Modules
   Use iofiles, Only: ofile, &
                      geto_gen
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: outfile1
!
! Optional arguments
! - optional input/output scalars -
   Type(ofile), Intent(InOut), Optional :: outfile2
!
! Function result
   Integer :: save_results
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Get output file
   Call geto_gen (outfile1)
   If (Present(outfile2)) Call geto_gen (outfile2)
!
! Save requested results
   Call write_results (ianal)
   save_results = 0
!
   Return
 End Function save_results
!
!
!
 Function print_results (f_update, &
          f_calc)
!
! Prints results to the screen
!
! Modules
   Use data_io_constants, Only: ioutstd
   Use gui,               Only: print_text
   Use iofiles,           Only: yfile
   Use fields,            Only: yfield, &
                                check_ivf, print_field
!
! Function result
   Integer :: print_results
!
! Arguments
!
! Dummy arguments
! - procedure arguments -
   Integer, External :: f_update ! - update function -
!
! Optional arguments
! - optional procedure arguments -
   Integer, External, Optional :: f_calc ! - calculation function -
!
! Locals
!
! Local scalars
   Integer :: iffy ! - current Y field / lagged field -
   Integer :: ivfa ! - current available series by field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Repeat for each field and variable
   Do iffy = 1, yfile%nfl
      Call print_field (ioutstd, yfield(iffy), yfile%nfs, yfile%nls, yfile%nms)
      Call print_text (' ')
      Do ivfa = 1, yfield(iffy)%nva
         print_results = check_ivf(iffy, ivfa)
         If (Present(f_calc)) print_results = f_calc()
         print_results = f_update()
      End Do
   End Do
   print_results = 0
!
   Return
 End Function print_results
!
!
!
#endif
 Function update_results(ioutr, ih_r, ir, chead, lpclim, print_res) &
          Result (update)
!
! Prints updated results
!
! Modules
#if GUI == 1
   Use clrwin$,            Only: clear_window$
#endif
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_gui_constants, Only: i_handle
   Use gui,                Only: print_text, set_cursor_waiting
   Use arrays,             Only: iuse=>iusey
   Use space,              Only: coor, rlaty, rlngdy, &
                                 make_map_coor
   Use iofiles,            Only: yfile
   Use fields,             Only: iffy, yfield
   Use climate,            Only: print_climatology
!
! Function result
   Integer :: update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ioutr ! - output unit number -
   Integer, Intent(In) :: ir    ! - array index -
!
   Integer(Kind=i_handle), Intent(In) :: ih_r ! - handle -
!
   Character(Len=*), Intent(In) :: chead ! - heading -
!
   Logical, Intent(In) :: lpclim ! - print cliamtology -
!
! - procedure arguments -
   Integer, External :: print_res
!
! Locals
!
! Local scalars
   Integer :: i       ! - latitude index -
   Integer :: j       ! - longitude index -
   Integer :: ij, ija ! - indices -
   Integer :: ilat    ! - latitude index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
#if GUI == 1
! Clear window
   Call clear_window$ (ih_r)
!
#endif
! Print climatology
   If (lpclim) Then
#if GUI == 1
      Call print_climatology (.false., &
           iout=ioutr)
#else
      Call print_climatology (.true., &
           iout=ioutr)
#endif
   End If
!
! Calculate offsets
   If (iffy == 1) Then
      ij = 0
      ija = 1
   Else
      ij = Sum(yfield(1:iffy-1)%nv)
      ija = Sum(yfield(1:iffy-1)%nva) + 1
   End If
!
! Print results
! - gridded data -
   Select Case (yfile%idstr)
    Case (id_grd)
      Call print_text (chead, &
           iout=ioutr)
      Call print_text (' ', &
           iout=ioutr)
      Do i = 1, yfield(iffy)%region%nlts
         ilat = yfield(iffy)%region%nlt1 + i - 1
         Do j = 1, yfield(iffy)%region%nlgs
            Call set_cursor_waiting (1)
            ij = ij + 1
            If (iuse(ija) == ij) Then
               coor = make_map_coor(rlaty(ilat,iffy), rlngdy(j,iffy))
               update = print_res(ioutr, ija, ir, 0, iffy)
               ija = ija + 1
            End If
            Call set_cursor_waiting (0)
         End Do
      End Do
! - station and unreferenced data -
    Case (id_stn, id_ind)
      Call print_text (chead, &
           iout=ioutr)
      Call print_text (' ', &
           iout=ioutr)
      Do j = 1, yfield(iffy)%nva
         Call set_cursor_waiting (1)
         update = print_res(ioutr, ija, ir, iuse(ija)-ij, iffy)
         ija = ija + 1
         Call set_cursor_waiting (0)
      End Do
   End Select
   update=0
!
   Return
 End Function update_results
!
!
!
 Subroutine write_results (ianal)
!
! Writes output to files
!
! Modules
   Use data_numbers,        Only: zero, one
   Use data_cpt_constants,  Only: ia_gcm, ia_non, id_ind, nep, ng, npall, nts, nwr
   Use data_io_constants,   Only: lstn
   Use data_time_constants, Only: isq_mn
   Use labels,              Only: ca_pvs_t, ca_scores, &
                                  cg_na, cg_nofsave, &
                                  l_correls, &
                                  ls_cats, ls_wrlts
   Use time,                Only: Assignment(=), Operator(+), &
                                  iseq, nss, &
                                  date_diff
   Use progress_meter,      Only: end_progress_meter, set_progress_increment, start_progress_meter, stop_progress_meter
   Use settings,            Only: clf, iexc, iodds, nb, nenf, nf, nt, ntr, nt1, nu, nur, nvrf, nvrf1
   Use arrays,              Only: iusex, iusey, lwk, tex, x, xiny, y, yt, ziny
   Use space,               Only: cstndx, cstndy, rlatdx, rlatdy, rlngdx, rlngdy, &
                                  set_iprecl
   Use iofiles,             Only: ifile, &
                                  mxa, mya, nx, ny, nz, &
                                  atfile, avfile, ccfile, cofile, cvfile, erfile, exfile, e2file, fcfile, fefile, fofile, fpfile, &
                                  fsfile, fvfile, fxfile, gtfile, lpfile, lrfile, l2file, mxfile, ocfile, orfile, o2file, pbfile, &
                                  pcfile, prfile, psfile, pvfile, p2file, rbfile, rcfile, rhfile, rofile, rrfile, sdfile, skfile, &
                                  thfile, tyfile, vrfile, v2file, wrfile, xdfile, xefile, xifile, xlfile, xmfile, xsfile, xtfile, &
                                  ycfile, ydfile, yefile, yffile, yhfile, ylfile, ymfile, yofile, ypfile, yrfile, ysfile, ytfile, &
                                  y2file, zifile, &
                                  mfile, xfile, yfile, zfile, &
                                  files_reset
   Use fields,              Only: sfield, xfield, yfield, zfield, &
                                  set_field
   Use distribs,            Only: alpha, beta, pcen
   Use pcs,                 Only: bz, eofx, eofy, fsx, fsxc, hx_map, hy_map, hx_ser, hy_ser, lxt, lyt, mcc, mxe, mye, mu, &
                                  nco, nxo, svx, svy, tsx, tsy
   Use regression,          Only: b, b0, c, dofr, e, ens_p, ens_r, ens_2, f, fcast, pesd_p, pesd_r, pesd_2, pls_p, pls_r, pls_2, &
                                  yfit, ycv, ycv2, yret
   Use scores,              Only: afp, b0s, b1s, corrs, far, fars, hit, hits, ifq, ips, iskill, orf, pscores, pvalues, rhit, roca, &
                                  rocas, skills, tends, wrlts
   Use season,              Only: isnc, lprst, nsn, nsnc
   Use missing,             Only: kfuse, ktuse
   Use categories,          Only: fps_p, fps_r, fps_2, icatc, iobs_c, iobs_r, odds_p, odds_r, odds_2, tobs
   Use climate,             Only: ave, climate_per, sdev
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
!
! Locals
!
! Local scalars
   Integer :: i     ! - index -
   Integer :: it0_f ! - time 0 for forecasts -
   Integer :: it0_r ! - time 0 for retractive predictands -
   Integer :: it0_v ! - time 0 for verification -
   Integer :: it0_x ! - time 0 for predictors -
   Integer :: it0_y ! - time 0 for predictands -
   Integer :: it0_z ! - time 0 for predictors in forecast period -
   Integer :: ifail ! - error indicator -
   Integer :: ns    ! - number of seasons -
   Integer :: nsave ! - number of files to save -
!
   Character(Len= 1) :: ctype ! - output Type -
   Character(Len=64) :: cvar  ! - field name -
!
   Logical :: lout   ! - output? -
   Logical :: lprint ! - print message? -
!
   Type(ifile) :: dfile ! - descriptor file -
!
! Local arrays
   Integer, Dimension(:), Allocatable :: iwk ! - workspace -
!
   Real(Kind=rp), Dimension(:,:), Allocatable :: cv ! - coefficients of variation -
!
   Character(Len=lstn), Dimension(:,:), Allocatable :: cvars ! - variable names -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real, Reshape, Shape, Transpose, Trim
!
! Executable Statements
!
! Update console
   Call start_progress_meter (' ', .true.)
! - count number of files to save -
   nsave = 0
   If (xdfile%cfile%lset) nsave = nsave + 1
   If (xifile%cfile%lset) nsave = nsave + 1
   If (ydfile%cfile%lset) nsave = nsave + 1
   If (yofile%cfile%lset) nsave = nsave + 1
   If (ypfile%cfile%lset) nsave = nsave + 1
   If (tyfile%cfile%lset) nsave = nsave + 1
   If (gtfile%cfile%lset) nsave = nsave + 1
   If (ycfile%cfile%lset) nsave = nsave + 1
   If (rcfile%cfile%lset) nsave = nsave + 1
   If (mxfile%cfile%lset) nsave = nsave + 1
   If (yhfile%cfile%lset) nsave = nsave + 1
   If (yffile%cfile%lset) nsave = nsave + 1
   If (yrfile%cfile%lset) nsave = nsave + 1
   If (erfile%cfile%lset) nsave = nsave + 1
   If (lrfile%cfile%lset) nsave = nsave + 1
   If (prfile%cfile%lset) nsave = nsave + 1
   If (orfile%cfile%lset) nsave = nsave + 1
   If (vrfile%cfile%lset) nsave = nsave + 1
   If (y2file%cfile%lset) nsave = nsave + 1
   If (e2file%cfile%lset) nsave = nsave + 1
   If (l2file%cfile%lset) nsave = nsave + 1
   If (p2file%cfile%lset) nsave = nsave + 1
   If (o2file%cfile%lset) nsave = nsave + 1
   If (v2file%cfile%lset) nsave = nsave + 1
   If (xefile%cfile%lset) nsave = nsave + 1
   If (yefile%cfile%lset) nsave = nsave + 1
   If (xlfile%cfile%lset) nsave = nsave + 1
   If (ylfile%cfile%lset) nsave = nsave + 1
   If (xsfile%cfile%lset) nsave = nsave + 1
   If (ysfile%cfile%lset) nsave = nsave + 1
   If (ccfile%cfile%lset) nsave = nsave + 1
   If (xmfile%cfile%lset) nsave = nsave + 1
   If (ymfile%cfile%lset) nsave = nsave + 1
   If (xtfile%cfile%lset) nsave = nsave + 1
   If (ytfile%cfile%lset) nsave = nsave + 1
   If (rbfile%cfile%lset) nsave = nsave + 1
   If (pbfile%cfile%lset) nsave = nsave + 1
   If (fpfile%cfile%lset) nsave = nsave + 1
   If (fofile%cfile%lset) nsave = nsave + 1
   If (fvfile%cfile%lset) nsave = nsave + 1
   If (ocfile%cfile%lset) nsave = nsave + 1
   If (pcfile%cfile%lset) nsave = nsave + 1
   If (fsfile%cfile%lset) nsave = nsave + 1
   If (lpfile%cfile%lset) nsave = nsave + 1
   If (fefile%cfile%lset) nsave = nsave + 1
   If (exfile%cfile%lset) nsave = nsave + 1
   If (fxfile%cfile%lset) nsave = nsave + 1
   If (zifile%cfile%lset) nsave = nsave + 1
   If (fcfile%cfile%lset) nsave = nsave + 1
   If (thfile%cfile%lset) nsave = nsave + 1
   If (avfile%cfile%lset) nsave = nsave + 1
   If (sdfile%cfile%lset) nsave = nsave + 1
   If (cvfile%cfile%lset) nsave = nsave + 1
   If (cofile%cfile%lset) nsave = nsave + 1
   If (skfile%cfile%lset) nsave = nsave + 1
   If (pvfile%cfile%lset) nsave = nsave + 1
   If (rofile%cfile%lset) nsave = nsave + 1
   If (psfile%cfile%lset) nsave = nsave + 1
   If (rrfile%cfile%lset) nsave = nsave + 1
   If (atfile%cfile%lset) nsave = nsave + 1
   If (rhfile%cfile%lset) nsave = nsave + 1
   If (wrfile%cfile%lset) nsave = nsave + 1
   If (nsave == 0) Then
      Print *, Trim(cg_nofsave)//'.'
      Call files_reset ()
      Call stop_progress_meter (.true.)
      Return
   End If
   Call set_progress_increment (one/Real(nsave, Kind=rp), .false.)
!
! Calculate time-0
   it0_x = xfile%it1 - 1
   it0_y = yfile%it1 - 1
   it0_z = zfile%it1 - 1
   it0_r = yfile%it1 + nt1 - 1
   it0_v = yfile%it1 + nvrf1 - 1
   it0_f = yfile%it1 + date_diff(xfile%fdate, yfile%fdate, iseq) - date_diff(zfile%fdate, yfile%fdate, iseq) - 1
!
! Identify precision for latitudes and longitudes
   If (xfile%igeog == 1) Then
      If (ipreclx == 0) ipreclx = set_iprecl (xfile%nfl, xfield(:)%nlt, xfield(:)%nlg, rlatdx(:,:), rlngdx(:,:))
   End If
   If (yfile%igeog == 1) Then
      If (iprecly == 0) iprecly = set_iprecl (yfile%nfl, yfield(:)%nlt, yfield(:)%nlg, rlatdy(:,:), rlngdy(:,:))
   End If
!
! Determine whether to print message
   lprint = (ianal /= ia_non)
!
! Save input data with missing values estimated
! - X input data -
   If (xdfile%cfile%lset) Then
      If (xfile%ngs == 1) Then
         Call write_cptdata (.true., .true., lprint, 'T', xdfile, xfile, xfield(1:), xfield(1)%tssn%tprd(1), nt, nx, ipreclx, &
              x(:,:,1), iusex(:), ktuse(:), rlatdx(:,:), rlngdx(:,:), cstndx(:,:), ifail, &
              it0=it0_x)
      Else
         Call write_cptprobs (xdfile, xfile, xfield(1:), xfield(1)%tssn%tprd(1), lprint, it0_x, nt, nx, ipreclx, &
              x(:,:,:), iusex(:), ktuse(:), rlatdx(:,:), rlngdx(:,:), cstndx(:,:), ifail)
      End If
      If (ifail < 0) GoTo 1
   End If
! - interpolated X data -
   If (xifile%cfile%lset) Then
      If (xfile%nms == 1) Then
         Allocate (sfield(xfile%nfl))
         sfield(:) = xfield(:)
         sfield(:)%nlt = yfield(:)%nlt
         sfield(:)%nlg = yfield(:)%nlg
         sfield(:)%region = yfield(:)%region
      Else
         Allocate (sfield(xfile%nms))
         sfield(:) = xfield(:)
         sfield(:)%nlt = yfield(1)%nlt
         sfield(:)%nlg = yfield(1)%nlg
         sfield(:)%region = yfield(1)%region
      End If
      If (xfile%ngs == 1 .and. xfile%nms == 1) Then ! - interpolated GCM fields -
         Call write_cptdata (.true., .true., lprint, 'T', xifile, xfile, sfield(1:), xfield(1)%tssn%tprd(1), nt, ny, iprecly, &
              xiny(:,:,1), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
              it0=it0_x)
      Else
         dfile = xfile
         dfile%nfs = yfile%nfs
         dfile%nls = yfile%nls
         dfile%nfl = yfile%nfl
         If (xfile%nms > 1) Then ! - interpolated multiple models -
            Do i = 1, xfile%nms
               Call write_cptdata ((i == 1), (i == xfile%nms), lprint, 'T', xifile, dfile, sfield(i:i), xfield(1)%tssn%tprd(1), &
                    nt, ny, iprecly,                                                                                            &
                    xiny(:,:,i), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
                    it0=it0_x)
            End Do
         Else ! - multiple categories, i.e., forecast probabilities -
            Call write_cptprobs (xifile, dfile, sfield(1:), xfield(1)%tssn%tprd(1), lprint, it0_x, nt, nx, iprecly, &
                 fps_r(:,:,:), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
         End If
      End If
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
! - Y input data -
   If (ydfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', ydfile, yfile, yfield(:), yfield(1)%tssn%tprd(1), nt, ny, iprecly, &
           y(:,:,1), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - Y input data observed component -
   If (yofile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', yofile, yfile, yfield(:), yfield(1)%tssn%tprd(0), nt, ny, iprecly, &
           y(:,:,0), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - Y input data persisted + predicted component -
   If (ypfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', ypfile, yfile, yfield(:), yfield(1)%tssn%tprd(2), nt, ny, iprecly, &
           y(:,:,2), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - transformed Y input data -
   If (tyfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', tyfile, yfile, yfield(:), yfield(1)%tssn%tprd(1), nt, ny, iprecly, &
           yt(:,:), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - gamma transformation parameters -
   If (gtfile%cfile%lset) Then
      If (lprst) Then
         ns = 3
      Else
         ns = nsn
      End If
      Call write_cptparams ('gamma', gtfile, yfile, yfield(:), lprint, 3, ny, ns, iprecly, &
           Reshape([alpha(:,:), beta(:,:), pcen(:,:)], [Shape(alpha),3]), ['alpha     ','beta      ','% censored'], &
           iusey(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - Y categories -
   If (ycfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', ycfile, yfile, yfield(:), yfield(1)%tssn%tprd(isnc), nt, ny, iprecly, &
           Real(iobs_c(:,:), Kind=rp), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - retroactive categories -
   If (rcfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', rcfile, yfile, yfield(:), yfield(1)%tssn%tprd(isnc), ntr, ny, iprecly, &
           Real(iobs_r(:,:), Kind=rp), iusey(:), ktuse(nt1+1:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_r)
      If (ifail < 0) GoTo 1
   End If
! - merged input data -
   If (mxfile%cfile%lset) Then
      Call write_cptdata (.true., .false., lprint, 'T', mxfile, xfile, xfield(:), xfield(1)%tssn%tprd(1), nt, nx, ipreclx, &
           x(:,:,1), iusex(:), ktuse(:), rlatdx(:,:), rlngdx(:,:), cstndx(:,:), ifail, &
           it0=it0_x, nofs=xfile%nfs+yfile%nfs)
      If (ifail < 0) GoTo 1
      Call write_cptdata (.false., .true., lprint, 'T', mxfile, yfile, yfield(:), yfield(1)%tssn%tprd(1), nt, ny, iprecly, &
           y(:,:,1), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
!
! Save hindcasts
! - cross-validated predictions -
   If (yhfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', yhfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), nt, ny, iprecly, &
           ycv(:,:,nsnc), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - fitted values -
   If (yffile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', yffile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), nt, ny, iprecly, &
           yfit(:,:,nsnc), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - retroactive predictions -
   If (yrfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', yrfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), ntr, ny, iprecly, &
           yret(:,:,nsnc), iusey(:), ktuse(nt1+1:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_r)
      If (ifail < 0) GoTo 1
   End If
! - retroactive ensemble predictions -
   If (erfile%cfile%lset) Then
      Call write_cptdata3 ('E', erfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_y, ntr, ny, nenf, iprecly, &
           ens_r(:,:,:), iusey(:), ktuse(nt1+1:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*nenf)
      If (ifail < 0) GoTo 1
   End If
! - retroactive prediction limits -
   If (lrfile%cfile%lset) Then
      Call write_cptdata3 ('L', lrfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_r, ntr, ny, 2, iprecly, &
           Reshape([pls_r(:,:)%lower, pls_r(:,:)%upper], [mya, nur, 2]), iusey(:), ktuse(nt1+1:),                        &
           rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*2, cl=clf)
      If (ifail < 0) GoTo 1
   End If
! - retroactive forecast probabilities -
   If (prfile%cfile%lset) Then
      Call write_cptprobs (prfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_r, ntr, ny, iprecly, &
           fps_r(:,:,:), iusey(:), ktuse(nt1+1:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - retroactive forecast odds -
   If (orfile%cfile%lset) Then
      Select Case (iodds)
       Case (0)
         ctype='O'
       Case (1)
         ctype='R'
      End Select
      Call write_cptdata3 (ctype, orfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_r, ntr, ny, ng, iprecly, &
           odds_r(:,:,:,iodds+1), iusey(:), ktuse(nt1+1:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*ng)
      If (ifail < 0) GoTo 1
   End If
! - retroactive prediction error variances -
   If (vrfile%cfile%lset) Then
      Allocate (sfield(yfile%nfl))
      sfield(:) = yfield(1:)
      Do i = 1, yfile%nfl
         sfield(i)%var = Trim(sfield(i)%var)//' retroactive_pred_err_var'
         If (Trim(sfield(i)%unitc) /= cg_na .and. Trim(sfield(i)%unitc) /= 'N/A') &
             sfield(i)%unitc = Trim(sfield(i)%unitc)//' (squared)'
      End Do
      Call write_cptdata (.true., .true., lprint, 'T', vrfile, yfile, sfield(:), yfield(1)%tssn%tprd(nsnc), ntr, ny, iprecly, &
           pesd_r(:,:)**2, iusey(:), ktuse(nt1+1:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_f, ci_tags=['dof'], i_tags=[Nint(dofr)])
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
! - double cross-validated predictions -
   If (y2file%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', y2file, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), nt, ny, iprecly, &
           ycv2(:,:,nsnc), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_y)
      If (ifail < 0) GoTo 1
   End If
! - double cross-validated ensemble predictions -
   If (e2file%cfile%lset) Then
      Call write_cptdata3 ('E', e2file, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_y, nf, ny, nenf, iprecly, &
           ens_2(:,:,:), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*nenf)
      If (ifail < 0) GoTo 1
   End If
! - double cross-validated prediction limits -
   If (l2file%cfile%lset) Then
      Call write_cptdata3 ('L', l2file, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_y, nt, ny, 2, iprecly, &
           Reshape([pls_2(:,:)%lower, pls_2(:,:)%upper], [mya, nu, 2]), iusey(:), ktuse(:),                             &
           rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*2, cl=clf)
      If (ifail < 0) GoTo 1
   End If
! - double cross-validated forecast probabilities -
   If (p2file%cfile%lset) Then
      Call write_cptprobs (p2file, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_y, nt, ny, iprecly, &
           fps_2(:,:,:), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - double cross-validated forecast odds -
   If (o2file%cfile%lset) Then
      Select Case (iodds)
       Case (0)
         ctype='O'
       Case (1)
         ctype='R'
      End Select
      Call write_cptdata3 (ctype, o2file, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_y, nt, ny, ng, iprecly, &
           odds_2(:,:,:,iodds+1), iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*ng)
      If (ifail < 0) GoTo 1
   End If
! - double cross-validated prediction error variances -
   If (v2file%cfile%lset) Then
      Allocate (sfield(yfile%nfl))
      sfield(:) = yfield(1:)
      Do i = 1, yfile%nfl
         sfield(i)%var = Trim(sfield(i)%var)//' 2cv_pred_err_var'
         If (Trim(sfield(i)%unitc) /= cg_na .and. Trim(sfield(i)%unitc) /= 'N/A') &
             sfield(i)%unitc = Trim(sfield(i)%unitc)//' (squared)'
      End Do
      Call write_cptdata (.true., .true., lprint, 'T', v2file, yfile, sfield(:), yfield(1)%tssn%tprd(nsnc), nt, ny, iprecly, &
           pesd_2(:,:)**2, iusey(:), ktuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_f, ci_tags=['dof'], i_tags=[Nint(dofr)])
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
!
! Save PCA and CCA results
! - eigenvalues -
   If (xefile%cfile%lset) Then ! - X eigenvalues -
      Call write_cpteigs (xefile, lprint, lxt, nt, svx, ifail)
      If (ifail < 0) GoTo 1
   End If
   If (yefile%cfile%lset) Then ! - Y eigenvalues -
      Call write_cpteigs (yefile, lprint, lyt, nt, svy, ifail)
      If (ifail < 0) GoTo 1
   End If
! - spatial loadings -
   If (xlfile%cfile%lset) Then ! - X spatial loadings -
      Call write_cptload ('X_loadings', xlfile, xfile, xfield(:), lprint, mxe, nx, ipreclx, &
           eofx(:,:), iusex(:), rlatdx(:,:), rlngdx(:,:), cstndx(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
   If (ylfile%cfile%lset) Then ! - Y spatial loadings -
      Call write_cptload ('Y_loadings', ylfile, yfile, yfield(1:), lprint, mye, ny, iprecly, &
           eofy(:,:), iusey(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - temporal scores -
   If (xsfile%cfile%lset) Then ! - X scores -
      Call write_cptscor ('X scores', xsfile, xfile%prd1, lprint, it0_x, nt, mxe, xfield(1)%rmiss, tsx(:,:), ktuse(:), ifail)
      If (ifail < 0) GoTo 1
   End If
   If (ysfile%cfile%lset) Then ! - Y scores -
      Call write_cptscor ('Y scores', ysfile, yfile%prd1, lprint, it0_y, nt, mye, yfield(1)%rmiss, tsy(:,:), ktuse(:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - canonical correlations -
   If (ccfile%cfile%lset) Then
      dfile%idstr = id_ind
      dfile%nfl = 1
      dfile%nfs = 1
      dfile%nls = 1
      Allocate (sfield(1))
      sfield(1)%nv = 1
      sfield(1)%nva = 1
      sfield(1)%var = 'correlation'
      sfield(1)%unitc = cg_na
      sfield(1)%rmiss = -999.0_rp
      sfield(1)%tssn%tprd(:)%sdate%iyr = 1
      sfield(1)%tssn%tprd(:)%sdate%imn = 0
      sfield(1)%tssn%tprd(:)%sdate%idy = 0
      sfield(1)%tssn%tprd(:)%edate = sfield(1)%tssn%tprd(:)%sdate
      Allocate (lwk(mcc))
      lwk(:) = .true.
      Allocate (cvars(1,1))
      cvars(:,:) = 'correlation'
      Call write_cptdata (.true., .true., lprint, 'Mode', ccfile, dfile, sfield(:), sfield(1)%tssn%tprd(1), mcc, 1, iprecly, &
           Reshape(mu(1:mcc), [1,mcc]), [1], lwk, rlatdy(:,:), rlatdy(:,:), cvars(:,:), ifail)
      Deallocate (cvars)
      Deallocate (lwk)
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
! - X homogeneous maps -
   If (xmfile%cfile%lset) Then
      Call write_cptload ('X_CCA_loadings', xmfile, xfile, xfield(:), lprint, nco, nx, ipreclx, &
           hx_map(:,:), iusex(:), rlatdx(:,:), rlngdx(:,:), cstndx(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - Y homogeneous maps -
   If (ymfile%cfile%lset) Then
      Call write_cptload ('Y_CCA_loadings', ymfile, yfile, yfield(1:), lprint, nco, ny, iprecly, &
           hy_map(:,:), iusey(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - X homogeneous map series -
   If (xtfile%cfile%lset) Then
      Call write_cptscor ('X_CCA_scores', xtfile, xfile%prd1, lprint, it0_x, nt, nco, xfield(1)%rmiss, hx_ser(:,:), ktuse(:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - Y homogeneous map series -
   If (ytfile%cfile%lset) Then
      Call write_cptscor ('Y_CCA_scores', ytfile, yfile%prd1, lprint, it0_y, nt, nco, yfield(1)%rmiss, hy_ser(:,:), ktuse(:), ifail)
      If (ifail < 0) GoTo 1
   End If
!
! Save regression coefficients
! - regression coefficients -
   If (rbfile%cfile%lset) Then
      Select Case (ianal)
       Case Default
         Call write_cptregr (lprint, ipreclx, iprecly, rbfile, xfile, xfield, cstndx(:,:), iusex(:), b0(:,1), b(:,:), ifail)
       Case (ia_gcm)
         Call write_cptload ('constants', rbfile, yfile, yfield(1:), lprint, 1, ny, iprecly, b0(:,:), iusey, rlatdy, rlngdy, &
              cstndy, ifail, &
              cload2='coefficients', v2=b(:,:))
      End Select
      If (ifail < 0) GoTo 1
   End If
! - PC regression coefficients -
   If (pbfile%cfile%lset) Then
      dfile = xfile
      dfile%idstr = id_ind
      dfile%nfl = 1
      dfile%nfs = 1
      dfile%nls = 1
      Allocate (sfield(1))
      Call set_field (xfield(1)%rmiss, sfield(1))
      sfield(1)%nv = nxo
      sfield(1)%nva = nxo
      sfield(1)%var = 'PCR_coefficients'
      sfield(1)%unitc = cg_na
      sfield(1)%rmiss = -999.0_rp
      Allocate (cvars(nxo,1))
      Allocate (iwk(nxo))
      Do i = 1, nxo
         Write (Unit=cvars(i,1), Fmt='(A,I0)') 'PC', i
         iwk(i) = i
      End Do
      Call write_cptregr (lprint, ipreclx, iprecly, pbfile, dfile, sfield(:), cvars(:,:), iwk(:), bz(0,:), bz(1:,:), ifail)
      Deallocate (iwk)
      Deallocate (cvars)
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
!
! Save forecasts
! - forecast probabilities -
   If (fpfile%cfile%lset) Then
      Call write_cptprobs (fpfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_f, nf, ny, iprecly, &
           fps_p(:,:,:), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - forecast odds -
   If (fofile%cfile%lset) Then
      Select Case (iodds)
       Case (0)
         ctype='O'
       Case (1)
         ctype='R'
      End Select
      Call write_cptdata3 (ctype, fofile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_f, nf, ny, ng, iprecly, &
           odds_p(:,:,:,iodds+1), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*ng)
      If (ifail < 0) GoTo 1
   End If
! - deterministic forecasts -
   If (fvfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', fvfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), nf, ny, iprecly, &
           fcast(:,:,nsnc), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_f)
      If (ifail < 0) GoTo 1
   End If
! - observed component of forecasts -
   If (ocfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', ocfile, yfile, yfield(:), yfield(1)%tssn%tprd(0), nf, ny, iprecly, &
           fcast(:,:,0), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_f)
      If (ifail < 0) GoTo 1
   End If
! - predicted component of forecasts -
   If (pcfile%cfile%lset) Then
      Call write_cptdata (.true., .true., lprint, 'T', pcfile, yfile, yfield(:), yfield(1)%tssn%tprd(1), nf, ny, iprecly, &
           fcast(:,:,1), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_f)
      If (ifail < 0) GoTo 1
   End If
! - forecast ensembles -
   If (fsfile%cfile%lset) Then
      Call write_cptdata3 ('E', fsfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_f, nf, ny, nenf, iprecly, &
           ens_p(:,:,:), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*nenf)
      If (ifail < 0) GoTo 1
   End If
! - prediction limits -
   If (lpfile%cfile%lset) Then
      Call write_cptdata3 ('L', lpfile, yfile, yfield(:), yfield(1)%tssn%tprd(nsnc), lprint, it0_f, nf, ny, 2, iprecly, &
           Reshape([pls_p(:,:)%lower, pls_p(:,:)%upper], [mya, nf, 2]), iusey(:), kfuse(:),                             &
           rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           nofs=yfile%nfs*2, cl=clf)
      If (ifail < 0) GoTo 1
   End If
! - prediction-error variances -
   If (fefile%cfile%lset) Then
      Allocate (sfield(yfile%nfl))
      sfield(:) = yfield(1:)
      Do i = 1, yfile%nfl
         sfield(i)%var = Trim(sfield(i)%var)//' pred_err_var'
         If (Trim(sfield(i)%unitc) /= cg_na .and. Trim(sfield(i)%unitc) /= 'N/A') &
             sfield(i)%unitc = Trim(sfield(i)%unitc)//' (squared)'
      End Do
      Call write_cptdata (.true., .true., lprint, 'T', fefile, yfile, sfield(:), yfield(1)%tssn%tprd(nsnc), nf, ny, iprecly, &
           pesd_p(:,:)**2, iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
           it0=it0_f, ci_tags=['dof'], i_tags=[Nint(dofr)])
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
! - exceedence probabilities -
   If (exfile%cfile%lset) Then
      Call write_cptexcs (exfile, lprint, iexc, nep, c, e, f, tex(1), tex(2), ifail)
      If (ifail < 0) GoTo 1
   End If
! - predictor time scores -
   If (fxfile%cfile%lset) Then
      Call write_cptscor ('X_scores', fxfile, zfile%prd1, lprint, it0_z, nf, mxe, xfield(1)%rmiss, fsx(:,:), kfuse(:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - interpolated Z data -
   If (zifile%cfile%lset) Then
      If (zfile%nms == 1) Then
         Allocate (sfield(xfile%nfl))
         sfield(:) = zfield(:)
         sfield(:)%nlt = yfield(:)%nlt
         sfield(:)%nlg = yfield(:)%nlg
         sfield(:)%region = yfield(:)%region
      Else
         Allocate (sfield(xfile%nms))
         sfield(:) = zfield(:)
         sfield(:)%nlt = yfield(1)%nlt
         sfield(:)%nlg = yfield(1)%nlg
         sfield(:)%region = yfield(1)%region
      End If
      If (zfile%ngs == 1 .and. zfile%nms == 1) Then ! - interpolated GCM fields -
         Call write_cptdata (.true., .true., lprint, 'T', zifile, zfile, sfield(:), zfield(1)%tssn%tprd(1), nf, ny, iprecly, &
              ziny(:,:,1), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
              it0=it0_z)
      Else
         dfile = zfile
         dfile%nfs = yfile%nfs
         dfile%nls = yfile%nls
         dfile%nfl = yfile%nfl
         If (zfile%nms > 1) Then ! - interpolated multiple models -
            Do i = 1, zfile%nms
               Call write_cptdata ((i == 1), (i == zfile%nms), lprint, 'T', zifile, dfile, sfield(i:i), zfield(1)%tssn%tprd(1), &
                    nf, ny, iprecly, ziny(:,:,i), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail, &
                    it0=it0_z)
            End Do
         Else ! - multiple categories, i.e., forecast probabilities -
            Call write_cptprobs (zifile, dfile, sfield(:), zfield(1)%tssn%tprd(1), lprint, it0_z, nf, nz, iprecly, &
                 fps_r(:,:,:), iusey(:), kfuse(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
         End If
      End If
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
! - predictor CCA time scores -
   If (fcfile%cfile%lset) Then
      Call write_cptscor ('X_CCA_scores', fcfile, zfile%prd1, lprint, it0_z, nf, nco, xfield(1)%rmiss, fsxc(:,:), kfuse(:), ifail)
      If (ifail < 0) GoTo 1
   End If
!
! Save climatologies
   lout = (thfile%cfile%lset .or. avfile%cfile%lset .or. sdfile%cfile%lset .or. cvfile%cfile%lset)
   If (lout) Then
      dfile = yfile
      If (mfile%iseq == isq_mn .and. nss > 1) Then
         dfile%nfl = dfile%nfl*nss
      Else
         dfile%nls = dfile%nls/nss
      End If
      Allocate (sfield(dfile%nfl))
      sfield(:) = yfield(:)
! - set dates to climatological dates -
      sfield(:)%tssn%tprd(1)%sdate%iyr = climate_per%d1%iyr
      sfield(:)%tssn%tprd(1)%edate%iyr = climate_per%d2%iyr
      If (dfile%nfl > 1) Then
         Do i = 2, dfile%nfl
            If (yfield(i)%tssn%tprd(1)%sdate%imn < yfield(i-1)%tssn%tprd(1)%sdate%imn) Then
               sfield(i:)%tssn%tprd(1)%sdate%iyr = sfield(i:)%tssn%tprd(1)%sdate%iyr + 1
            End If
         End Do
         Do i = dfile%nfl-1, 1, -1
            If (yfield(i)%tssn%tprd(1)%sdate%imn > yfield(i+1)%tssn%tprd(1)%sdate%imn) Then
               sfield(:i)%tssn%tprd(1)%sdate%iyr = sfield(:i)%tssn%tprd(1)%sdate%iyr - 1
            End If
         End Do
      End If
      Allocate (lwk(nsn))
      lwk(:) = .true.
   End If
! - thresholds -
   If (thfile%cfile%lset) Then
      Call write_cptdata3 ('T', thfile, dfile, sfield(:), sfield(1)%tssn%tprd(1), lprint, 0, nsn, ny, nts, iprecly, &
           Reshape(tobs, [mya, nsn, nts]), iusey(:), lwk(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - averages -
   If (avfile%cfile%lset) Then
      Do i = 1, nss
         sfield(i)%var = Trim(sfield(i)%var)//' '//Trim(sfield(i)%cssn%cssn(isnc))//' average'
         Call write_cptdata ((i==1), (i==nss), lprint, 'T', avfile, dfile, sfield(i:), sfield(i)%tssn%tprd(isnc), 1, ny, iprecly, &
              ave(:,i:), iusey(:), lwk(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
         If (ifail < 0) GoTo 1
      End Do
   End If
! - standard deviations -
   If (sdfile%cfile%lset) Then
      Do i = 1, nss
         sfield(i)%var = Trim(sfield(i)%var)//' '//Trim(sfield(i)%cssn%cssn(isnc))//' standard deviation'
         Call write_cptdata ((i==1), (i==nss), lprint, 'T', sdfile, dfile, sfield(i:), sfield(i)%tssn%tprd(isnc), 1, ny, iprecly, &
              sdev(:,i:), iusey(:), lwk(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
         If (ifail < 0) GoTo 1
      End Do
   End If
! - coefficients of variation -
   If (cvfile%cfile%lset) Then
      Allocate (cv(mya,nsn))
      Do i = 1, nss
         Where (ave(:,i:) > zero)
            cv(:,i:) = sdev(:,i:)/ave(:,i:)
         Else Where
            cv(:,i:) = zero
         End Where
         sfield(i)%var = Trim(sfield(i)%var)//' '//Trim(sfield(i)%cssn%cssn(isnc))//' coefficient of variation'
         Call write_cptdata ((i==1), (i==nss), lprint, 'T', cvfile, dfile, sfield(i:), sfield(i)%tssn%tprd(isnc), 1, ny, iprecly, &
              cv(:,i:), iusey(:), lwk(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
         Deallocate (cv)
         If (ifail < 0) GoTo 1
      End Do
   End If
   If (lout) Then
      Deallocate (lwk)
      Deallocate (sfield)
   End If
! - correlations -
   If (cofile%cfile%lset) Then
      Allocate (sfield(xfile%nfs))
      sfield(:) = xfield(:)
      sfield(:)%tssn%tprd(1)%sdate%iyr = 1
      sfield(:)%tssn%tprd(1)%sdate%imn = 0
      sfield(:)%tssn%tprd(1)%sdate%idy = 0
      sfield(:)%tssn%tprd(1)%edate = sfield(:)%tssn%tprd(1)%sdate
      Call write_cptdata (.true., .true., lprint, Trim(l_correls%c), cofile, xfile, sfield(:), yfield(1)%tssn%tprd(isnc), &
           1, nx, ipreclx, Reshape(corrs, [mxa,1]), iusex(:), [.true.], rlatdx(:,:), rlngdx(:,:), cstndx(:,:), ifail)
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
!
! Save scores
   lout=(skfile%cfile%lset .or. pvfile%cfile%lset .or. psfile%cfile%lset)
   If (lout) Then
      Allocate (sfield(yfile%nfs))
      sfield(:) = yfield(1:yfile%nfs)
      sfield(:)%unit  = 'unitless'
      sfield(:)%unitc = 'unitless'
   End If
! - deterministic skill scores -
   If (skfile%cfile%lset) Then
      Call write_cptload (Trim(ca_scores(iskill)), skfile, yfile, sfield(:), lprint, 1, ny, iprecly, &
           Reshape(skills, [mya, 1]), iusey(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - p-values -
   If (pvfile%cfile%lset) Then
      Call write_cptload ('p-values', pvfile, yfile, sfield(:), lprint, 1, ny, iprecly, &
           Reshape(pvalues,[mya, 1]), iusey(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - ROC results -
   If (rofile%cfile%lset) Then ! - individual point -
      Call write_cptroc (rofile, lprint, nt, 2, hit, far, roca, ifail)
      If (ifail < 0) GoTo 1
   End If
! - probabilistic scores -
   If (psfile%cfile%lset) Then
      cvar = ca_pvs_t(ips)
      If (ips > npall) cvar = Trim(cvar)//' ('//Trim(ls_cats(icatc)%c)//')'
      Call write_cptload (Trim(cvar), psfile, yfile, sfield(:), lprint, 1, ny, iprecly, &
           Reshape(pscores,[mya, 1]), iusey(:), rlatdy(:,:), rlngdy(:,:), cstndy(:,:), ifail)
      If (ifail < 0) GoTo 1
   End If
   If (lout) Deallocate (sfield)
! - ROC results -
   If (rrfile%cfile%lset) Then ! - all points -
      Call write_cptroc (rrfile, lprint, nb, ng, hits, fars, rocas, ifail)
      If (ifail < 0) GoTo 1
   End If
! - attributes diagram -
   If (atfile%cfile%lset) Then
      Call write_cptrel (atfile, lprint, ng, nb, afp, orf, ifq, tends, b0s, b1s, ifail)
      If (ifail < 0) GoTo 1
   End If
! - ranked-hits -
   If (rhfile%cfile%lset) Then
      Call write_cptscor ('Ranked_hits', rhfile, yfile%prd1+nvrf1, lprint, it0_v, nvrf, ng, yfield(1)%rmiss, Transpose(rhit), &
           ktuse(nvrf1+1:), ifail)
      If (ifail < 0) GoTo 1
   End If
! - weather roulette results -
   If (wrfile%cfile%lset) Then
      dfile%idstr = id_ind
      dfile%nfl = 1
      dfile%nfs = 1
      dfile%nls = 1
      Allocate (sfield(1))
      sfield(1) = yfield(1)
      sfield(1)%nv = nwr
      sfield(1)%nva = nwr
      sfield(1)%var = 'weather_roulette'
      sfield(1)%unitc = cg_na
      sfield(1)%tssn%tprd(1) = yfile%prd1 + (yfile%it1 - 2)
      Allocate (iwk(nwr))
      Do i = 1, nwr
         iwk(i) = i
      End Do
      Allocate (lwk(nvrf+1))
      lwk(1) = .true.
      lwk(2:nvrf+1) = ktuse(nvrf1+1:)
      Allocate (cvars(nwr,1))
      cvars(:,1) = ls_wrlts(:)%c
      Call write_cptdata (.true., .true., lprint, 'T', wrfile, dfile, sfield(:), yfield(1)%tssn%tprd(nsnc)+(yfile%it1-2), &
           nvrf+1, nwr, iprecly, wrlts(:,0:), iwk(:), lwk(:), rlatdy(:,:), rlngdy(:,:), cvars(:,:), ifail, &
           it0=it0_v)
      Deallocate (lwk)
      Deallocate (iwk)
      Deallocate (sfield)
      If (ifail < 0) GoTo 1
   End If
!
! Update console
1  Call end_progress_meter (.true., (.true. .and. lprint))
!
   Return
!
 End Subroutine write_results
!
!
!
 Subroutine write_cptdata (lopen, lclose, lprint, cpt_row, afile, dfile, afield, prd1, nt, nv, ip, v, iuse, kuse, &
            rlat, rlng, cstn, ifail, &
            it0, nofs, ci_tags, i_tags, cc_tags, c_tags)
!
! Outputs data in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_io_constants,  Only: iout, lstn
   Use time,               Only: pprd, &
                                 Operator(/=)
   Use iofiles,            Only: ifile, ofile
   Use fields,             Only: field, &
                                 tfield
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ip    ! - latitude and longitude output precision -
   Integer, Intent(In) :: nt    ! - number of cases -
   Integer, Intent(In) :: nv    ! - number of variables -
!
   Logical, Intent(In) :: lopen  ! - open file? -
   Logical, Intent(In) :: lclose ! - close file? -
   Logical, Intent(In) :: lprint ! - print message? -
!
   Character(Len=*), Intent(In) :: cpt_row ! - cpt_row tag -
!
   Type(ifile), Intent(In) :: dfile ! - descriptor file -
!
   Type(pprd), Intent(In) :: prd1 ! - start date -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v    ! - data -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: it0  ! - time 0 -
   Integer, Intent(In), Optional :: nofs ! - number of output fields -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i_tags ! - extra integer tags values -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: ci_tags ! - extra integer tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cc_tags ! - extra character tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: c_tags  ! - extra character tags values -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Size
!
! Executable Statements
!
! Open output file
   ifail = 0
   If (lopen) Then
      If (Present(nofs)) Then
         Call open_output (iout, lprint, afile, nofs, dfile%idstr, nv, ifail)
      Else
         Call open_output (iout, lprint, afile, dfile%nfs, dfile%idstr, nv, ifail)
      End If
   End If
   If (ifail == 0) Then
!
! Set field information
      Allocate (tfield(Size(afield, Dim=1)))
      tfield(:) = afield(:)
      tfield(:)%tssn%tprd(1) = prd1 ! - set start date for possible persistence component -
!
! Print data
      Select Case (dfile%idstr)
       Case (id_grd) ! - gridded -
         Call write_grid (iout, afile, tfield(:), dfile%nfs, dfile%nls, nt, nv, ip, &
              v(:,:), iuse(:), kuse(:), rlat(:,:), rlng(:,:), ifail, &
              it0=it0, ci_tags=ci_tags, i_tags=i_tags, cc_tags=cc_tags, c_tags=c_tags)
       Case (id_stn) ! - station -
         Call write_stns (iout, afile, tfield(:), cpt_row, dfile%nfs, dfile%nls, nt, nv, ip, &
              v(:,:), iuse(:), kuse(:), rlat(:,:), rlng(:,:), cstn(:,:), ifail, &
              it0=it0, ci_tags=ci_tags, i_tags=i_tags, cc_tags=cc_tags, c_tags=c_tags)
       Case (id_ind) ! - index -
         Call write_unrf (iout, afile, tfield(:), cpt_row, dfile%nfs, dfile%nls, nt, nv, &
              v(:,:), iuse(:), kuse(:), cstn(:,:), ifail, &
              it0=it0, ci_tags=ci_tags, i_tags=i_tags, cc_tags=cc_tags, c_tags=c_tags)
      End Select
      Deallocate (tfield)
   End If
!
! Close file and reset
   If (lclose) Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptdata
!
!
!
 Subroutine write_cptdata3 (ctype, afile, dfile, afield, prd1, lprint, it0, nt, nv, n3, ip, v, iuse, kuse, rlat, rlng, cstn, &
            ifail, &
            nofs, cl)
!
! Outputs 3-Dimensional data in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_io_constants,  Only: iout, lstn
   Use time,               Only: pprd
   Use iofiles,            Only: ifile, ofile
   Use fields,             Only: field, &
                                 tfield
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ip  ! - latitude and longitude output precision -
   Integer, Intent(In) :: it0 ! - time 0 -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: nv  ! - number of stations -
   Integer, Intent(In) :: n3  ! - third dimension -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
   Character(Len=1), Intent(In) :: ctype ! - output Type -
!
   Type(ifile), Intent(In) :: dfile ! - descriptor file -
!
   Type(pprd), Intent(In) :: prd1 ! - start date -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v    ! - data -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nofs ! - number of output fields -
!
   Real(Kind=rp), Intent(In), Optional :: cl ! - confidence level -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Open output file
   ifail = 0
   If (Present(nofs)) Then
      Call open_output (iout, lprint, afile, nofs, dfile%idstr, nv, ifail)
   Else
      Call open_output (iout, lprint, afile, dfile%nfs, dfile%idstr, nv, ifail)
   End If
   If (ifail == 0) Then
!
! Set field information
      Allocate (tfield(dfile%nfl))
      tfield(:) = afield(:)
      tfield(:)%tssn%tprd(1) = prd1 ! - set start date for possible persistence component -
!
! Print data
      Select Case (dfile%idstr)
       Case (id_grd) ! - gridded data -
         Call write_grids (ctype, iout, afile, tfield(:), dfile%nfs, dfile%nls, it0, nt, nv, n3, ip, &
              v(:,:,:), iuse(:), kuse(:), rlat(:,:), rlng(:,:), ifail, &
              cl=cl)
       Case (id_stn) ! - station data -
         Call write_stnss (ctype, iout, afile, tfield(:), dfile%nfs, dfile%nls, it0, nt, nv, n3, ip, &
              v(:,:,:), iuse(:), kuse(:), rlat(:,:), rlng(:,:), cstn(:,:), ifail, &
              cl=cl)
       Case (id_ind) ! - index data -
         Call write_unrfs (ctype, iout, afile, tfield(:), dfile%nfs, dfile%nls, it0, nt, nv, n3, &
              v(:,:,:), iuse(:), kuse(:), cstn(:,:), ifail, &
              cl=cl)
      End Select
      Deallocate (tfield)
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptdata3
!
!
!
 Subroutine write_cptscor (cscore, afile, prd1, lprint, it0, nt, nv, rmiss, v, kuse, ifail)
!
! Outputs temporal scores in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_io_constants, Only: iout
   Use time,              Only: pprd
   Use iofiles,           Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: it0 ! - time 0 -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: nv  ! - number of grids -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
   Character(Len=*), Intent(In) :: cscore ! - score description -
!
   Type(pprd), Intent(In) :: prd1 ! - first period -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v ! - data -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, 1, 3, nt, ifail)
!
! Print data
   If (ifail == 0) Call write_scor (iout, cscore, afile, nt, it0, prd1, nv, kuse(:), rmiss, v(:,:), ifail)
!
! CLose file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptscor
!
!
!
 Subroutine write_cptload (cload, afile, dfile, afield, lprint, ne, nv, ip, v, iuse, rlat, rlng, cstn, ifail, &
            cload2, v2)
!
! Outputs loadings in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_io_constants,  Only: iout, lstn
   Use iofiles,            Only: ifile, ofile
   Use fields,             Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ip ! - latitude and longitude output precision -
   Integer, Intent(In) :: ne ! - number of loadings -
   Integer, Intent(In) :: nv ! - number of variables -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
   Character(Len=*), Intent(In) :: cload ! - loadings description -
!
   Type(ifile), Intent(In) :: dfile ! - descriptor file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v    ! - data -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: cload2 ! - second loadings description -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: v2 ! - additional data -
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, dfile%nfs, dfile%idstr, nv, ifail)
   If (ifail == 0) Then
!
! Print data
      Select Case (dfile%idstr)
       Case (id_grd) ! - gridded data -
         Call write_load (iout, afile, afield(:), dfile%idstr, dfile%nfs, dfile%nls, ne, nv, ip, iuse(:), v(:,:), cload, ifail, &
              rlat=rlat(:,:), rlng=rlng(:,:))
         If (Present(cload2) .and. Present(v2)) Then
            Call write_load (iout, afile, afield(:), dfile%idstr, dfile%nfs, dfile%nls, ne, nv, ip, iuse(:), v2(:,:), cload2, &
                 ifail, &
                 rlat=rlat(:,:), rlng=rlng(:,:))
         End If
       Case (id_stn) ! - station data -
         Call write_load (iout, afile, afield(:), dfile%idstr, dfile%nfs, dfile%nls, ne, nv, ip, iuse(:), v(:,:), cload, ifail, &
              rlat=rlat(:,:), rlng=rlng(:,:), cstn=cstn(:,:))
         If (Present(cload2) .and. Present(v2)) Then
            Call write_load (iout, afile, afield(:), dfile%idstr, dfile%nfs, dfile%nls, ne, nv, ip, iuse(:), v2(:,:), cload2, &
                 ifail, &
                 rlat=rlat(:,:), rlng=rlng(:,:), cstn=cstn(:,:))
         End If
       Case (id_ind) ! - index data -
         Call write_load (iout, afile, afield(:), dfile%idstr, dfile%nfs, dfile%nls, ne, nv, ip, iuse(:), v(:,:), cload, ifail, &
              cstn=cstn(:,:))
         If (Present(cload2) .and. Present(v2)) Then
            Call write_load (iout, afile, afield(:), dfile%idstr, dfile%nfs, dfile%nls, ne, nv, ip, iuse(:), v2(:,:), cload2, &
                 ifail, &
                 cstn=cstn(:,:))
         End If
      End Select
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptload
!
!
!
 Subroutine write_cpteigs (afile, lprint, ne, nt, v, ifail)
!
! Outputs eigenvalues in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_io_constants, Only: iout
   Use iofiles,           Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ne ! - total number of eigenvalues -
   Integer, Intent(In) :: nt ! - number of cases -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, 1, 3, 3, ifail)
!
! Print data
   If (ifail == 0) Then
      Call write_eigs (iout, afile, ne, v(:), nt, ifail)
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cpteigs
!
!
!
 Subroutine write_cptparams (cdist, afile, dfile, afield, lprint, np, nv, ns, ip, v, cpars, iuse, rlat, rlng, cstn, ifail)
!
! Outputs parameters in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_io_constants,  Only: iout, lprd, lstn
   Use iofiles,            Only: ifile, ofile
   Use fields,             Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ip ! - latitude and longitude output precision -
   Integer, Intent(In) :: np ! - number of parameters -
   Integer, Intent(In) :: nv ! - number of variables -
   Integer, Intent(In) :: ns ! - number of seasons -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
   Character(Len=*), Intent(In) :: cdist ! - distribution -
!
   Type(ifile), Intent(In) :: dfile ! - descriptor file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v ! - data -
!
   Character(Len=*), Dimension(:), Intent(In) :: cpars ! - parameter descriptions -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: isn ! - season index -
   Integer :: i   ! - season identifier -
!
! Local arrays
   Character(Len=lprd), Dimension(2) :: ctags ! - cpt tags -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim
!
! Executable Statements
!
! Open output file
   Call open_output (iout, lprint, afile, dfile%nfs, dfile%idstr, nv, ifail)
!
! Print data
   If (ifail == 0) Then
      If (Len_Trim(afield(1)%cssn%cssn(0)) > 0) Then ! - persistence season -
         i = 0
      Else
         i = 1
      End If
      ctags(1) = cdist
      Do isn = 1, ns
         ctags(2) = afield(1)%cssn%cssn(i)
         Select Case (dfile%idstr)
          Case (id_grd) ! - gridded data -
            Call write_grid (iout, afile, afield(:), dfile%nfs*dfile%nls, 1, np, nv, ip, &
                 v(:,isn,:), iuse(:), [.true., .true., .true.], rlat(:,:), rlng(:,:), ifail, &
                 cblock='parameter', cblocks=cpars(:), cc_tags=['dist  ', 'season'], c_tags=ctags)
          Case (id_stn) ! - station data -
            Call write_stns (iout, afile, afield(:), 'parameter', dfile%nfs*dfile%nls, 1, np, nv, ip, &
                 v(:,isn,:), iuse(:), [.true., .true., .true.], rlat(:,:), rlng(:,:), cstn(:,:), ifail, &
                 crows=cpars(:), cc_tags=['dist  ', 'season'], c_tags=ctags)
           Case (id_ind) ! - index data -
            Call write_unrf (iout, afile, afield(:), 'parameter', dfile%nfs*dfile%nls, 1, np, nv, &
                 v(:,isn,:), iuse(:), [.true., .true., .true.], cstn(:,:), ifail, &
                 crows=cpars(:), cc_tags=['dist  ', 'season'], c_tags=ctags)
         End Select
         i = i + 1
      End Do
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptparams
!
!
!
 Subroutine write_cptprobs (afile, dfile, afield, prd1, lprint, it0, nt, nv, ip, v, iuse, kuse, rlat, rlng, cstn, ifail)
!
! Outputs probabilities in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind, ng, nts
   Use data_io_constants,  Only: iout, lstn
   Use time,               Only: pprd, &
                                 nss
   Use iofiles,            Only: ifile, ofile
   Use fields,             Only: field, &
                                 tfield
   Use categories,         Only: pthr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ip  ! - latitude and longitude output precision -
   Integer, Intent(In) :: it0 ! - time 0 -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: nv  ! - number of variables -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
   Type(ifile), Intent(In) :: dfile ! - descriptor file -
!
   Type(pprd), Intent(In) :: prd1 ! - start date -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v ! - data -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
   Real(Kind=rp), Dimension(ng) :: probs ! - climatological probabilities -
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, dfile%nfs, dfile%idstr, nv, ifail, &
        ng=ng)
   If (ifail == 0) Then
!
! Calculate category probabilities
      probs(1) = pthr(1)
      probs(2) = pthr(2) - pthr(1)
      probs(ng) = one - pthr(nts)
!
! Set field information
      Allocate (tfield(dfile%nfl))
      tfield(:) = afield(:)
      tfield(:)%rmiss = -one
      tfield(:)%tssn%tprd(1) = prd1 ! - set start date for possible persistence component -
!
! Print data
      Select Case (dfile%idstr)
       Case (id_grd) ! - gridded data -
         Call write_grids ('P', iout, afile, tfield(:), dfile%nfs, dfile%nls/nss, it0, nt, nv, ng, ip, &
              v(:,:,:), iuse(:), kuse(:), rlat(:,:), rlng(:,:), ifail, &
              prob=probs)
        Case (id_stn) ! - station data -
         Call write_stnss ('P', iout, afile, tfield(:), dfile%nfs, dfile%nls/nss, it0, nt, nv, ng, ip, &
              v(:,:,:), iuse(:), kuse(:), rlat(:,:), rlng(:,:), cstn(:,:), ifail, &
              prob=probs)
        Case (id_ind) ! - index data -
         Call write_unrfs ('P', iout, afile, tfield(:), dfile%nfs, dfile%nls/nss, it0, nt, nv, ng, &
              v(:,:,:), iuse(:), kuse(:), cstn(:,:), ifail, &
              prob=probs)
      End Select
      Deallocate (tfield)
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptprobs
!
!
!
 Subroutine write_cptroc (afile, lprint, nb, ng, hit, far, roca, ifail)
!
! Prints ROC results
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: iout
   Use iofiles,           Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nb ! - number of bins -
   Integer, Intent(In) :: ng ! - number of categories -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: roca ! - ROC areas -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: hit ! - hit rates -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: far ! - false-alarm rates -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: j ! - category index -
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, 1, 3, nb*ng, ifail)
!
! Print data
   If (ifail == 0) Then
      Do j = 1, ng
         Call write_cols (iout, afile, nb, ifail, &
              d1='Hit rates', r1=hit(:,j), d2='False-alarm rates', r2=far(:,j),               &
              cpt_field='ROC', cpt_nrow=nb, cpt_ncol=2, cpt_row='threshold', cpt_col='index', &
              cpt_tagsr=['area'], cpt_rtags=[roca(j)])
         If (ifail /= 0) Exit
      End Do
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptroc
!
!
!
 Subroutine write_cptexcs (afile, lprint, iexc, nep, c, e, f, tmin, tmax, ifail)
!
! Prints exceedence probabilities
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: iout
   Use labels,            Only: cg_thresh
   Use iofiles,           Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iexc ! - exceedance (0) or non-exceedance (1) probabilities -
   Integer, Intent(In) :: nep  ! - number of exceedance probabilities -
!
   Real(Kind=rp), Intent(In) :: tmin ! - minimum threshold -
   Real(Kind=rp), Intent(In) :: tMax ! - Maximum threshold -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
! - input arrays -
   Real(Kind=rp), Dimension(0:), Intent(In) :: c ! - climatological probabilities -
   Real(Kind=rp), Dimension(0:), Intent(In) :: e ! - empirical probabilities -
   Real(Kind=rp), Dimension(0:), Intent(In) :: f ! - forecast probabilities -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i ! - threshold index -
!
   Real(Kind=rp) :: tinc ! - threshold increment -
!
   Character(Len=32) :: cfield ! - CPT field -
!
! Local arrays
   Real(Kind=rp), Dimension(0:nep) :: t ! - thresholds -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, 1, 3, nep+1, ifail)
!
! Set appropriate X-increment
   If (ifail == 0) Then
      tinc = (tmax - tmin)/Real(nep, Kind=rp)
      t(0) = tmin
      Do i = 1, nep
         t(i) = t(i-1) + tinc
      End Do
!
! Select title
   Select Case (iexc)
    Case (0)
      cfield = 'exceedance-probs'
    Case (1)
      cfield = 'nonexceedance-probs'
   End Select
!
! Print data
      Call write_cols (iout, afile, nep+1, ifail, &
           d1=Trim(cg_thresh), r1=t(0:), d2='Climatological', r2=c(0:), d3='Empirical', r3=e(0:), d4='Forecast', r4=f(0:), &
           cpt_field=Trim(cfield), cpt_nrow=nep, cpt_ncol=4, cpt_row='threshold', cpt_col='prob')
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptexcs
!
!
!
 Subroutine write_cptrel (afile, lprint, ng, nb, afp, orf, ifq, tends, b0s, b1s, ifail)
!
! Prints reliability diagram results
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,      Only: half, one, fifty, oneh
   Use data_io_constants, Only: iout
   Use labels,            Only: c_lab, ls_cats, &
                                make_label
   Use iofiles,           Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: nb ! - number of probability bins -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
! - input arrays -
   Integer, Dimension(:,0:), Intent(In) :: ifq ! - frequency -
!
   Real(Kind=rp), Dimension(0:), Intent(In) :: b0s ! - intercepts -
   Real(Kind=rp), Dimension(0:), Intent(In) :: b1s ! - slopes -
!
   Real(Kind=rp), Dimension(:,0:), Intent(In) :: afp   ! - bin-averaged forecast probability -
   Real(Kind=rp), Dimension(:,0:), Intent(In) :: orf   ! - observed relative frequency -
   Real(Kind=rp), Dimension(:,0:), Intent(In) :: tends ! - tendencies -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: j   ! - category index -
   Integer :: k   ! - probability bin index -
   Integer :: kmn ! - lowest populated probability bin -
   Integer :: kmx ! - largest populated probability bin -
!
   Real(Kind=rp) :: b0  ! - intercept for no-skill line -
   Real(Kind=rp) :: tfq ! - total frequency -
!
! Local arrays
   Real(Kind=rp), Dimension(nb) :: ffq ! - fitted frequencies -
   Real(Kind=rp), Dimension(nb) :: rfq ! - relative frequencies -
   Real(Kind=rp), Dimension(nb) :: sk0 ! - zero-skill -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, 1, 3, nb*(ng+1), ifail)
!
! Calculate outputs
   If (ifail == 0) Then
      Do j = 0, ng
! - total frequency -
         tfq = Real(Sum(ifq(:,j)), Kind=rp)
         b0 = half*tends(1,j)
! - minimum frequency -
         Do k = 1, nb
            If (ifq(k,j) > 0) Then
               kmn = k
               Exit
            End If
         End Do
! - Maximum frequency -
         Do k = nb, kmn, -1
            If (ifq(k,j) > 0) Then
               kmx = k
               Exit
            End If
         End Do
! - relative frequencies -
         Do k = 1, nb
            rfq(k) = Real(100*ifq(k,j), Kind=rp)/tfq
! - fitted frequencies -
            If (k >= kmn .and. k <= kmx) Then
               ffq(k) = b0s(j) + b1s(j)*afp(k,j)
            Else
               ffq(k) = -one
            End If
! - zero-skill -
            If (ifq(k,j) > 0) Then
               sk0(k) = b0 + fifty*afp(k,j)/oneh
            Else
               sk0(k) = b0 + fifty*Real(k-1)/Real(nb-1, Kind=rp)
            End If
         End Do
!
! Print data
! - print data -
         c_lab = make_label('L', ls_cats(j))
         Call write_cols (iout, afile, nb, ifail, &
              d1='Forecast probability (x)', r1=afp(:,j),                                                  &
              d2='Observed relative frequency (y)', r2=orf(:,j),                                           &
              d3='Frequency', i3=ifq(:,j),                                                                 &
              d4='Relative frequency (%)', r4=rfq(:),                                                      &
              d5='Fitted frequency (%)', r5=ffq(:),                                                        &
              d6='No skill (%)', r6=sk0(:),                                                                &
              cpt_field='attributes', cpt_nrow=nb, cpt_ncol=4, cpt_row='probability', cpt_col='attribute', &
              cpt_c=j, cpt_tagsc=['category'], cpt_ctags=[c_lab])
! - print scores -
         Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'Observed frequency'
         If (output_value(iout, r=tends(1,j), ldelim=.true.) /= 0) GoTo 1
         Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'Average forecast  '
         If (output_value(iout, r=tends(2,j), ldelim=.true.) /= 0) GoTo 1
         Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'Intercept (%)     '
         If (output_value(iout, r=b0s(j), ldelim=.true.) /= 0) GoTo 1
         Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'Slope (%)         '
         If (output_value(iout, r=b1s(j), ldelim=.true.) /= 0) GoTo 1
      End Do
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_cptrel
!
!
!
 Subroutine write_cptregr (lprint, ipx, ipy, afile, bfile, bfield, bvars, iuseb, b0, b, ifail)
!
! Outputs regression coefficients in CPT format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!   -1 If output should be halted for all subsequent files
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_io_constants,  Only: iout
   Use arrays,             Only: iusey
   Use space,              Only: cstndy, rlatdx, rlatdy, rlngdx, rlngdy
   Use iofiles,            Only: ifile, ofile, &
                                 nx, ny, yfile
   Use fields,             Only: field, &
                                 yfield
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ipx ! - X latitude and longitude output precision -
   Integer, Intent(In) :: ipy ! - Y latitude and longitude output precision -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
   Type(ifile), Intent(InOut) :: bfile ! - predictors file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuseb ! - used variable indicators -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: b0 ! - intercepts -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: b ! - coefficients -
!
   Character(Len=*), Dimension(:,:), Intent(In) :: bvars ! - predictor variables -
!
   Type(field), Dimension(:), Intent(In) :: bfield ! - predictor fields -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Open output file
   ifail = 0
   Call open_output (iout, lprint, afile, bfile%nfs, yfile%idstr, ny, ifail)
!
! Print data
   If (ifail == 0) Then
      Select Case (bfile%idstr)
       Case (id_grd) ! - gridded data -
         Select Case (yfile%idstr)
          Case (id_grd) ! - gridded data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb(:), yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 rlatx=rlatdx(:,:), rlngx=rlngdx(:,:), rlaty=rlatdy(:,:), rlngy=rlngdy(:,:))
          Case (id_stn) ! - station data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb(:), yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 rlatx=rlatdx(:,:), rlngx=rlngdx(:,:), rlaty=rlatdy(:,:), rlngy=rlngdy(:,:), cstny=cstndy(:,:))
          Case (id_ind) ! - index data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb(:), yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 rlatx=rlatdx(:,:), rlngx=rlngdx(:,:), cstny=cstndy(:,:))
         End Select
       Case (id_stn) ! - station data -
         Select Case (yfile%idstr)
          Case (id_grd) ! - gridded data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb, yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 rlatx=rlatdx(:,:), rlngx=rlngdx(:,:), cstnx=bvars(:,:), rlaty=rlatdy(:,:), rlngy=rlngdy(:,:))
          Case (id_stn) ! - station data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb, yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 rlatx=rlatdx(:,:), rlngx=rlngdx(:,:), cstnx=bvars(:,:), rlaty=rlatdy(:,:), rlngy=rlngdy(:,:), cstny=cstndy(:,:))
          Case (id_ind) ! - index data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb, yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 rlatx=rlatdx(:,:), rlngx=rlngdx(:,:), cstnx=bvars(:,:), cstny=cstndy(:,:))
         End Select
       Case (id_ind) ! - index data -
         Select Case (yfile%idstr)
          Case (id_grd) ! - gridded data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb(:), yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 cstnx=bvars(:,:), rlaty=rlatdy(:,:), rlngy=rlngdy(:,:), b0=b0(:))
          Case (id_stn) ! - station data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb(:), yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 cstnx=bvars(:,:), rlaty=rlatdy(:,:), rlngy=rlngdy(:,:), cstny=cstndy(:,:), b0=b0(:))
          Case (id_ind) ! - index data -
            Call write_regr (iout, ipx, ipy, afile, bfile, bfield(:), nx, iuseb(:), yfile, yfield(1:), ny, iusey(:), b(:,:), &
                 ifail, &
                 cstnx=bvars(:,:), cstny=cstndy(:,:), b0=b0(:))
         End Select
      End Select
   End If
!
! Close file and reset
   Call close_output (iout, afile, ifail)
!
   Return
 End Subroutine write_cptregr
!
!
!
 Subroutine write_grid (iout, afile, afield, nfs, nls, nt, nv, ip, v, iuse, kuse, rlat, rlng, ifail, &
            it0, cblock, cblocks, ci_tags, i_tags, cc_tags, c_tags)
!
! Outputs data in gridded format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: ca_ffmts, io_iso, lprd
   Use time,              Only: pdate, &
                                Assignment(=), Operator(==), Operator(+), &
                                get_cdate
   Use iofiles,           Only: ofile, &
                                itags
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ip   ! - latitude and longitude output precision -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of grids -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v    ! - data -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: it0   ! - time 0 -
!
   Character(Len=*), Intent(In), Optional :: cblock ! - block tag -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i_tags ! - extra integer tags values -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: cblocks ! - block indicators -
   Character(Len=*), Dimension(:), Intent(In), Optional :: ci_tags ! - extra integer tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cc_tags ! - extra character tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: c_tags  ! - extra character tags values -
!
! Locals
!
! Local scalars
   Integer :: k   ! - time index -
   Integer :: l   ! - field / lagged field index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: ii  ! - available series index -
   Integer :: ij  ! - current variable -
   Integer :: kk  ! - available time index -
!
   Character(Len=lprd) :: csp ! - output label for latitude-longitude space -
!
   Type(pdate) :: mdate ! - start date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Present, Sum, Trim
!
! Executable Statements
!
! Print gridded data
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Do ifd = 1, nfs
         Do ilf = 1, nls
            l = (ifd - 1)*nls + ilf
            If (l > 1) Then
               ij = Sum(afield(1:l-1)%nv)
               ii = Sum(afield(1:l-1)%nva) + 1
            Else
               ij = 0
               ii = 1
            End If
            If (afield(l)%mdate == 0) Then
               mdate = 0
            Else
               If (.not.Present(it0)) mdate=0
            End If
            kk = 0
            Do k = 1, nt
               If (kuse(k)) kk = kk + 1
               If ((.not.kuse(k) .and. k > 2) .or. kk == 0) Cycle
               If (itags == 1) Then
                  If (Present(it0)) Then
                     If (.not.(afield(l)%mdate == 0)) mdate = afield(l)%mdate + (it0 + k - 1)
                     If (k == 1) Then
                        If (ilf == 1) Then
                           Call write_tag (iout, ifail, &
                                           cpt_field=afield(l)%var, cpt_z=afield(l)%z, cpt_m=afield(l)%member,             &
                                           cpt_model=afield(l)%model, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1), &
                                           cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs,                 &
                                           cpt_row='Y', cpt_col='X', cpt_units=afield(l)%unitc,                            &
                                           cpt_missing=afield(l)%rmiss,                                                    &
                                           cpt_tagsi=ci_tags, cpt_itags=i_tags, cpt_tagsc=cc_tags, cpt_ctags=c_tags)
                        Else
                           Call write_tag (iout, ifail, &
                                           cpt_model=afield(l)%model, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1), &
                                           cpt_tagsi=ci_tags, cpt_itags=i_tags, cpt_tagsc=cc_tags, cpt_ctags=c_tags)
                        End If
                     Else
                        Call write_tag (iout, ifail, &
                                        cpt_model=afield(l)%model, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1), &
                                        cpt_tagsi=ci_tags, cpt_itags=i_tags, cpt_tagsc=cc_tags, cpt_ctags=c_tags)
                     End If
                  Else ! - no dates -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_model=afield(l)%model,                   &
                                     cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs,       &
                                     cpt_row='Y', cpt_col='X', cpt_units=afield(l)%unitc,                  &
                                     cpt_missing=afield(l)%rmiss, cpt_cblock=cblock, cpt_block=cblocks(k), &
                                     cpt_tagsi=ci_tags, cpt_itags=i_tags, cpt_tagsc=cc_tags, cpt_ctags=c_tags)
                  End If
                  csp = ' '
                  If (ifail /= 0) GoTo 1
               Else
                  If (Present(it0)) Then
                     csp = get_cdate(afield(l)%tssn%tprd(1)+(it0+k-1), 1, io_iso)
                  Else
                     Write (Unit=csp, Fmt=*) afield(l)%rmiss
                     csp = AdjustL(csp)
                  End If
               End If
               If (kuse(kk)) Then
                  Call write_gridded_block_v10 (iout, csp, afield(l), rlat(:,l), rlng(:,l), ij, ip, ifail, &
                       v=v(ii:,kk), iuse=iuse(ii:))
               Else
                  Call write_gridded_block_v10 (iout, csp, afield(l), rlat(:,l), rlng(:,l), ij, ip, ifail)
               End If
               If (ifail /= 0) GoTo 1
            End Do
         End Do
      End Do
    Case ('unformatted')
      Call write_unform (iout, afile, nt, nv, v, iuse, kuse, afield(1)%rmiss, ifail)
      If (ifail /= 0) Return
    Case ('GrADS')
      Call write_unform (iout, afile, nt, nv, v, iuse, kuse, afield(1)%rmiss, ifail)
      If (ifail /= 0) Return
      Call write_grads_grid_ctrl (iout+1, afile, afield(:), nfs, nls, nt, 1, rlat(:,:), rlng(:,:), ifail)
      If (ifail /= 0) Return
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_grid
!
!
!
 Subroutine write_stns (iout, afile, afield, cpt_row, nfs, nls, nt, nv, ip, v, iuse, kuse, rlat, rlng, cstn, ifail, &
            it0, crows, ci_tags, i_tags, cc_tags, c_tags)
!
! Outputs data in station format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: ca_ffmts, io_iso, lstn
   Use time,              Only: Operator(+), &
                                get_cdate
   Use iofiles,           Only: ofile
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ip   ! - latitude and longitude output precision -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of stations -
!
   Character(Len=*), Intent(In) :: cpt_row ! - CPT row -
!
   Type(ofile), Intent(In) :: afile  ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v    ! - data -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: it0  ! - time 0 -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i_tags ! - extra integer tags values -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: ci_tags ! - extra integer tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cc_tags ! - extra character tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: c_tags  ! - extra character tags values -
   Character(Len=*), Dimension(:), Intent(In), Optional :: crows   ! - row indicators -
!
! Locals
!
! Local scalars
   Integer :: i   ! - station index -
   Integer :: k   ! - time index -
   Integer :: l   ! - field / lagged field index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: ii  ! - available series index -
   Integer :: ij  ! - current variable -
   Integer :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Sum, Trim
!
! Executable Statements
!
! Print cpt tags and station information
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Do ifd = 1, nfs
         l = (ifd - 1)*nls + 1
         Call write_stn_tags (iout, afield(l), cpt_row, nt*nls, ip, rlat(:,l), rlng(:,l), cstn(:,l), ifail, &
              ci_tags=ci_tags, i_tags=i_tags, cc_tags=cc_tags, c_tags=c_tags)
!
! Print station data
         kk = 0
         Do k = 1, nt
            If (kuse(k)) kk = kk + 1
            If (ifd == 1) Then
               ii = 1
               ij = 0
            Else
               ii = Sum(afield(1:l-1)%nva) + 1
               ij = Sum(afield(1:l-1)%nv)
            End If
            Do ilf = 1, nls
               If (Present(it0)) Then ! - dates -
                  cout = get_cdate(afield(l)%tssn%tprd(1)+(it0+k-1), 1, io_iso)
               Else If (Present(crows)) Then ! - passed as an argument -
                  cout = Trim(crows(k))
               Else ! - counter -
                  Write (Unit=cout, Fmt='(i0)') k
               End If
               If (output_value(iout, c=cout, ladv=.false.) /= 0) GoTo 1
               Do i = 1, afield(l)%nv
                  If (kuse(k)) Then
                     ij = ij + 1
                     If (iuse(ii) == ij) Then
                        If (output_value(iout, r=v(ii,kk), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                        ii = ii + 1
                     Else
                        If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                     End If
                  Else
                     If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                  End If
                  If (ii > nv) Exit
               End Do
               Write (Unit=iout, Fmt='()', Advance='yes', Err=1)
            End Do
         End Do
      End Do
    Case ('unformatted')
      Call write_unform (iout, afile, nt, nv, v, iuse, kuse, afield(1)%rmiss, ifail)
      If (ifail /= 0) Return
    Case ('GrADS')
      Call write_grads_stns (iout, afile, nt, nls, afield(1)%nva, v, afield(:)%tssn%tprd(1)%sdate, &
           cstn(:,1), rlat(:,1), rlng(:,1), iuse, kuse, afield(1)%rmiss,ifail)
      If (ifail /= 0) Return
      Call write_grads_stns_ctrl (iout+1, afile, afield(:), nfs,nls, nt, 1, ifail)
      If (ifail /= 0) Return
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_stns
!
!
!
 Subroutine write_stn_tags (iout, afield, cpt_row, nt, ip, rlat, rlng, cstn, ifail, &
            ci_tags, i_tags, cc_tags, c_tags)
!
! Outputs header and tagline for station format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: lstn
   Use iofiles,           Only: itags
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ip   ! - latitude and longitude output precision -
   Integer, Intent(In) :: nt   ! - number of cases -
!
   Character(Len=*), Intent(In) :: cpt_row ! - CPT row -
!
   Type(field), Intent(In) :: afield ! - field -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
   Character(Len=lstn), Dimension(:), Intent(In) :: cstn ! - station names -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i_tags ! - extra integer tags values -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: ci_tags ! - extra integer tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cc_tags ! - extra character tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: c_tags  ! - extra character tags values -
!
! Locals
!
! Local scalars
   Integer :: i ! - station index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Print tag line
   If (itags == 1) Then
      Call write_tag (iout, ifail, &
           cpt_field=afield%var, cpt_nrow=nt, cpt_ncol=afield%nv,            &
           cpt_row=cpt_row, cpt_col='station', cpt_units=Trim(afield%unitc), &
           cpt_missing=afield%rmiss,                                         &
           cpt_tagsi=ci_tags, cpt_itags=i_tags, cpt_tagsc=cc_tags, cpt_ctags=c_tags)
      If (ifail /= 0) GoTo 1
   End If
!
! Print station information
   Do i = 1, afield%nv
      If (output_value(iout, c=cstn(i), ldelim=.true., ladv=(i==afield%nv)) /= 0) GoTo 1
   End Do
   If (itags == 1) Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
   Do i = 1, afield%nv
      If (output_value(iout, r=rlat(i), ldelim=.true., ladv=(i==afield%nv), iprecs=ip) /= 0) GoTo 1
   End Do
   If (itags == 1) Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
   Do i = 1, afield%nv
      If (output_value(iout, r=rlng(i), ldelim=.true., ladv=(i==afield%nv), iprecs=ip) /= 0) GoTo 1
   End Do
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_stn_tags
!
!
!
 Subroutine write_unrf (iout, afile, afield, cpt_row, nfs, nls, nt, nv, v, iuse, kuse, cstn, ifail, &
            it0, crows, ci_tags, i_tags, cc_tags, c_tags)
!
! Outputs data in unreferenced format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: ca_ffmts, io_iso
   Use time,              Only: Operator(+), &
                                get_cdate
   Use iofiles,           Only: ofile
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of series -
!
   Character(Len=*), Intent(In) :: cpt_row ! - CPT row -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v ! - data -
!
   Character(Len=*), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: it0   ! - time 0 -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i_tags ! - extra integer tags values -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: ci_tags ! - extra integer tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cc_tags ! - extra character tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: c_tags  ! - extra character tags values -
   Character(Len=*), Dimension(:), Intent(In), Optional :: crows   ! - row indicators -
!
! Locals
!
! Local scalars
   Integer :: i   ! - series index -
   Integer :: k   ! - time index -
   Integer :: l   ! - field / lagged field index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: ii  ! - available series index -
   Integer :: ij  ! - current variable -
   Integer :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Sum, Trim
!
! Executable Statements
!
! Print tag line
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Do ifd = 1, nfs
         l = (ifd - 1)*nls + 1
         Call write_index_tags (iout, afield(l), cpt_row, nt*nls, cstn(:,l), ifail, &
              ci_tags=ci_tags, i_tags=i_tags, cc_tags=cc_tags, c_tags=c_tags)
!
! Print unreferenced data
         kk = 0
         Do k = 1, nt
            If (kuse(k)) kk = kk + 1
            If (ifd == 1) Then
               ii = 1
               ij = 0
            Else
               ii = Sum(afield(1:l-1)%nva) + 1
               ij = Sum(afield(1:l-1)%nv)
            End If
            Do ilf = 1, nls
               If (Present(it0)) Then ! - dates -
                  cout = get_cdate(afield(l)%tssn%tprd(1)+(it0+k-1), 1, io_iso)
               Else If (Present(crows)) Then ! - passed as an argument -
                  cout = Trim(crows(k))
               Else ! - counter -
                  Write (Unit=cout, Fmt='(i0)') k
               End If
               If (output_value(iout, c=cout, ladv=.false.) /= 0) GoTo 1
               Do i = 1, afield(l)%nv
                  If (kuse(k)) Then
                     ij = ij + 1
                     If (iuse(ii) == ij) Then
                        If (output_value(iout, r=v(ii,kk), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                        ii = ii + 1
                     Else
                        If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                     End If
                  Else
                     If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                  End If
                  If (ii > nv) Exit
               End Do
               Write (Unit=iout, Fmt='()', Advance='yes', Err=1)
            End Do
         End Do
      End Do
    Case ('unformatted')
      Call write_unform (iout, afile, nt, nv, v, iuse, kuse, afield(1)%rmiss, ifail)
      If (ifail /= 0) Return
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_unrf
!
!
!
 Subroutine write_index_tags (iout, afield, cpt_row, nt, cstn, ifail, &
            ci_tags, i_tags, cc_tags, c_tags)
!
! Outputs header and tagline for index format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: lstn
   Use iofiles,           Only: itags
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nt   ! - number of cases -
!
   Character(Len=*), Intent(In) :: cpt_row ! - CPT row -
!
   Type(field), Intent(In) :: afield ! - field -
!
! - input arrays -
   Character(Len=lstn), Dimension(:), Intent(In) :: cstn ! - station names -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i_tags ! - extra integer tags values -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: ci_tags ! - extra integer tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cc_tags ! - extra character tags -
   Character(Len=*), Dimension(:), Intent(In), Optional :: c_tags  ! - extra character tags values -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Print tag line
   If (itags == 1) Then
      Call write_tag (iout, ifail, &
           cpt_field=afield%var, cpt_nrow=nt, cpt_ncol=afield%nv,          &
           cpt_row=cpt_row, cpt_col='index', cpt_units=Trim(afield%unitc), &
           cpt_missing=afield%rmiss,                                       &
           cpt_tagsi=ci_tags, cpt_itags=i_tags, cpt_tagsc=cc_tags, cpt_ctags=c_tags)
      If (ifail /= 0) GoTo 1
   End If
!
! Print index information
   Do i = 1, afield%nv
      If (output_value(iout, c=cstn(i), ldelim=.true., ladv=(i==afield%nv)) /= 0) GoTo 1
   End Do
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_index_tags
!
!
!
 Subroutine write_grids (ctype, iout, afile, afield, nfs, nls, it0, nt, nv, n3, ip, v, iuse, kuse, rlat, rlng, ifail, &
            prob, cl)
!
! Outputs data in gridded format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: ca_ffmts, io_iso, lprd
   Use labels,            Only: c_thresh=>c_lab, &
                                ls_cats, ls_threshs, &
                                make_label
   Use time,              Only: pdate, &
                                Assignment(=), Operator(==), Operator(+), &
                                get_cdate
   Use iofiles,           Only: ofile, &
                                itags
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ip   ! - latitude and longitude output precision -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: it0  ! - time 0 -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of grids -
   Integer, Intent(In) :: n3   ! - third Dimension -
!
   Character(Len=1), Intent(In) :: ctype ! - output Type -
!                                            'E' - ensemble -
!                                            'L' - prediction limits -
!                                            'M' - Models -
!                                            'T' - thresholds -
!                                            'O' - odds -
!                                            'P' - probabilities -
!                                            'R' - relative odds -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v ! - data -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: cl ! - confidence level -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: prob ! - climatological probabilities -
!
! Locals
!
! Local scalars
   Integer :: k   ! - time index -
   Integer :: l   ! - field / lagged field index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: i3  ! - third Dimension index -
   Integer :: ii  ! - available series index -
   Integer :: ij  ! - current variable -
   Integer :: kk  ! - available time index -
!
   Character(Len=lprd) :: csp ! - output label for latitude-longitude space -
!
   Type(pdate) :: mdate ! - start date -
!
! Local arrays
   Character(Len=16), Dimension(n3) :: c_thrs ! - thresholds -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum, Trim
!
! Executable Statements
!
! Print gridded data
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Do ifd = 1, nfs
         Do ilf = 1, nls
            l = (ifd - 1)*nls + ilf
            If (l > 1) Then
               ij = Sum(afield(1:l-1)%nv)
               ii = Sum(afield(1:l-1)%nva) + 1
            Else
               ij = 0
               ii = 1
            End If
            If (afield(l)%mdate == 0) mdate = 0
            kk = 0
            Do k = 1, nt
               If (kuse(k)) kk = kk + 1
               If ((.not.kuse(k) .and. k > 2) .or. kk == 0) Cycle
               If (.not.afield(l)%mdate == 0) mdate = afield(l)%mdate + (it0 + k - 1)
               Do i3 = 1, n3
                  If (itags == 1) Then
                     If (i3 == 1) Then
                        If (k == 1) Then
                           If (ilf == 1) Then
                              Select Case (ctype)
                               Case ('E') ! - ensemble -
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(l)%var, cpt_z=afield(l)%z, cpt_m=i3,           &
                                                 cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1),            &
                                                 cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs, &
                                                 cpt_row='Y', cpt_col='X', cpt_units=afield(l)%unitc,            &
                                                 cpt_missing=afield(l)%rmiss)
                               Case ('L') ! - prediction limits -
                                 c_thresh = make_label('L', ls_threshs(i3))
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(l)%var, cpt_z=afield(l)%z,                     &
                                                 cpt_m=afield(l)%member, cpt_clev=cl, cpt_limit=c_thresh,        &
                                                 cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1),            &
                                                 cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs, &
                                                 cpt_row='Y', cpt_col='X', cpt_units='%',                        &
                                                 cpt_missing=afield(l)%rmiss)
                               Case ('M') ! - Models -
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(i3)%var, cpt_model=afield(i3)%model,             &
                                                 cpt_z=afield(i3)%z, cpt_m=afield(i3)%member,                      &
                                                 cpt_s=mdate, cpt_t=afield(i3)%tssn%tprd(1)+(it0+k-1),             &
                                                 cpt_nrow=afield(i3)%region%nlts, cpt_ncol=afield(i3)%region%nlgs, &
                                                 cpt_row='Y', cpt_col='X', cpt_units=afield(i3)%unitc,             &
                                                 cpt_missing=afield(i3)%rmiss)
                               Case ('O') ! - odds -
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(l)%var, cpt_c=i3, cpt_z=afield(l)%z,                        &
                                                 cpt_m=afield(l)%member, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1), &
                                                 cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs,              &
                                                 cpt_row='Y', cpt_col='X', cpt_units='odds',                                  &
                                                 cpt_missing=afield(l)%rmiss)
                               Case ('P') ! - probabilities -
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(l)%var, cpt_c=i3, cpt_prob=prob(i3),           &
                                                 cpt_z=afield(l)%z, cpt_m=afield(l)%member, cpt_s=mdate,         &
                                                 cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1),                         &
                                                 cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs, &
                                                 cpt_row='Y', cpt_col='X', cpt_units='probability (%)',          &
                                                 cpt_missing=afield(l)%rmiss)
                               Case ('R') ! - relative odds -
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(l)%var, cpt_c=i3, cpt_z=afield(l)%z,                        &
                                                 cpt_m=afield(l)%member, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1), &
                                                 cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs,              &
                                                 cpt_row='Y', cpt_col='X', cpt_units='relative odds',                         &
                                                 cpt_missing=afield(l)%rmiss)
                               Case ('T') ! - thresholds -
                                 c_thresh = make_label('L', ls_threshs(i3))
                                 Call write_tag (iout, ifail, &
                                                 cpt_field=afield(l)%var, cpt_z=afield(l)%z,                     &
                                                 cpt_m=afield(l)%member, cpt_limit=c_thresh,                     &
                                                 cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1),            &
                                                 cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs, &
                                                 cpt_row='Y', cpt_col='X', cpt_units=afield(l)%unitc,            &
                                                 cpt_missing=afield(l)%rmiss)
                              End Select
                           Else
                              Select Case (ctype)
                               Case ('E') ! - ensemble -
                                 Call write_tag (iout, ifail, &
                                                 cpt_m=i3, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                               Case ('L', 'T') ! - prediction limits, and thresholds -
                                 c_thresh = make_label('L', ls_threshs(i3))
                                 Call write_tag (iout, ifail, &
                                                 cpt_limit=c_thresh, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                               Case ('M') ! - Model -
                                 Call write_tag (iout, ifail, &
                                                 cpt_model=afield(i3)%model, &
                                                 cpt_s=mdate, cpt_t=afield(i3)%tssn%tprd(1)+(it0+k-1))
                               Case ('O', 'R') ! - odds, and relative odds -
                                 Call write_tag (iout, ifail, &
                                                 cpt_c=i3, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                               Case ('P') ! - probabilities -
                                 Call write_tag (iout, ifail, &
                                                 cpt_c=i3, cpt_prob=prob(i3), cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                              End Select
                           End If
                        Else
                           Select Case (ctype)
                            Case ('E') ! - ensemble -
                              Call write_tag (iout, ifail, &
                                              cpt_m=i3, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                            Case ('L', 'T') ! - prediction limits, and thresholds -
                              c_thresh = make_label('L', ls_threshs(i3))
                              Call write_tag (iout, ifail, &
                                              cpt_limit=c_thresh, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                            Case ('M') ! - Model -
                              Call write_tag (iout, ifail, &
                                              cpt_field=Trim(afield(i3)%var), cpt_model=Trim(afield(i3)%model), &
                                              cpt_s=mdate, cpt_t=afield(i3)%tssn%tprd(1)+(it0+k-1))
                            Case ('O', 'R') ! - odds, and relative odds -
                              Call write_tag (iout, ifail, &
                                              cpt_c=i3, cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                            Case ('P') ! - probabilities -
                              Call write_tag (iout, ifail, &
                                              cpt_c=i3, cpt_prob=prob(i3), cpt_s=mdate, cpt_t=afield(l)%tssn%tprd(1)+(it0+k-1))
                           End Select
                        End If
                     Else
                        Select Case (ctype)
                         Case ('E') ! - ensemble -
                           Call write_tag (iout, ifail, &
                                           cpt_m=i3)
                         Case ('L', 'T') ! - prediction limits, and thresholds -
                           c_thresh = make_label('L', ls_threshs(i3))
                           Call write_tag (iout, ifail, &
                                           cpt_limit=c_thresh)
                         Case ('M') ! - Model -
                           Call write_tag (iout, ifail, &
                                           cpt_field=Trim(afield(i3)%var), cpt_model=Trim(afield(i3)%model), &
                                           cpt_s=mdate, cpt_t=afield(i3)%tssn%tprd(1)+(it0+k-1))
                         Case ('O', 'R') ! - odds, and relative odds -
                           Call write_tag (iout, ifail, &
                                           cpt_c=i3)
                         Case ('P') ! - probabilities -
                           Call write_tag (iout, ifail, &
                                           cpt_c=i3, cpt_prob=prob(i3))
                        End Select
                     End If
                     csp = ' '
                     If (ifail /= 0) GoTo 1
                  Else
                     csp = get_cdate(afield(l)%tssn%tprd(1)+(it0+k-1), 1, io_iso)
                  End If
                  If (kuse(kk)) Then
                     Call write_gridded_block_v10 (iout, csp, afield(l), rlat(:,l), rlng(:,l), ij, ip, ifail, &
                          v=v(ii:,kk,i3), iuse=iuse(ii:))
                  Else
                     Call write_gridded_block_v10 (iout, csp, afield(l), rlat(:,l), rlng(:,l), ij, ip, ifail)
                  End If
                  If (ifail /= 0) GoTo 1
               End Do
            End Do
         End Do
      End Do
    Case ('unformatted')
      Do i3 = 1, n3
         Call write_unform (iout, afile, nt, nv, v(:,:,i3), iuse, kuse, afield(1)%rmiss, ifail)
         If (ifail /= 0) Return
      End Do
    Case ('GrADS')
      Do i3 = 1, n3
         Call write_unform (iout, afile, nt, nv, v(:,:,i3), iuse, kuse, afield(1)%rmiss, ifail)
         If (ifail /= 0) Return
      End Do
      Select Case (ctype)
       Case ('E') ! - ensemble -
       Case ('L', 'T') ! - prediction limits, and thresholds -
         Do i3 = 1, n3
            c_thrs(i3) = Trim(make_label('L', ls_threshs(i3)))
         End Do
         Call write_grads_grid_ctrl (iout+1, afile, afield(:), nfs, nls, nt, n3, rlat(:,:), rlng(:,:), ifail, &
              cvar=c_thrs(:))
       Case ('M') ! - Models -
       Case ('O', 'P', 'R') ! - odds, probabilities, relative odds -
         Call write_grads_grid_ctrl (iout+1, afile, afield(:), nfs, nls, nt, n3, rlat(:,:), rlng(:,:), ifail, &
              cvar=ls_cats(1:)%c)
      End Select
      If (ifail /= 0) Return
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_grids
!
!
!
 Subroutine write_stnss (ctype, iout, afile, afield, nfs, nls, it0, nt, nv, n3, ip, v, iuse, kuse, rlat, rlng, cstn, ifail, &
            prob, cl)
!
! Outputs data in station format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: ca_ffmts, io_iso, lstn
   Use labels,            Only: c_thresh=>c_lab, &
                                ls_cats, ls_threshs, &
                                make_label
   Use time,              Only: Operator(+), &
                                get_cdate
   Use iofiles,           Only: ofile, &
                                itags
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ip   ! - latitude and longitude output precision -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: it0  ! - time 0 -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of stations -
   Integer, Intent(In) :: n3   ! - third Dimension -
!
   Character(Len=1), Intent(In) :: ctype ! - output Type -
!                                            'E' - ensemble -
!                                            'L' - prediction limits -
!                                            'M' - Models -
!                                            'T' - thresholds -
!                                            'O' - odds -
!                                            'P' - probabilities -
!                                            'R' - relative odds -
!
   Type(ofile), Intent(In) :: afile  ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v ! - data -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: cl ! - confidence level -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: prob ! - climatological probabilities -
!
! Locals
!
! Local scalars
   Integer :: i   ! - station index -
   Integer :: k   ! - time index -
   Integer :: l   ! - field / lagged field index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: i3  ! - third Dimension index -
   Integer :: ii  ! - available series index -
   Integer :: ij  ! - current variable -
   Integer :: kk  ! - available time index -
!
! Local arrays
   Character(Len=16), Dimension(n3) :: c_thrs ! - thresholds -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum, Trim
!
! Executable Statements
!
! Print tag line
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Do ifd = 1, nfs
         l = (ifd - 1)*nls + 1
         Do i3 = 1, n3
            If (itags == 1) Then
               If (i3 == 1) Then
                  Select Case (ctype)
                   Case ('E') ! - ensemble -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_m=i3,                                      &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units=afield(l)%unitc, cpt_missing=afield(l)%rmiss)
                   Case ('L') ! - prediction limits -
                     c_thresh = make_label('L', ls_threshs(i3))
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var,                                                &
                                     cpt_clev=cl, cpt_limit=c_thresh,                                        &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units=afield(l)%unitc, cpt_missing=afield(l)%rmiss)
                   Case ('M') ! - Models -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(i3)%var,                                                &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(i3)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units=afield(i3)%unitc, cpt_missing=afield(i3)%rmiss)
                   Case ('O') ! - odds -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_c=i3,                                      &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units='odds', cpt_missing=afield(l)%rmiss)
                   Case ('P') ! - probabilities -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_c=i3, cpt_prob=prob(i3),                   &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units='probability (%)', cpt_missing=afield(l)%rmiss)
                   Case ('R') ! - relative odds -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_c=i3,                                      &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units='relative odds', cpt_missing=afield(l)%rmiss)
                   Case ('T') ! - thresholds -
                     c_thresh = make_label('L', ls_threshs(i3))
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var,                                                &
                                     cpt_limit=c_thresh,                                                     &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units=afield(i3)%unitc, cpt_missing=afield(i3)%rmiss)
                  End Select
               Else
                  Select Case (ctype)
                   Case ('E') ! - ensemble -
                     Call write_tag (iout, ifail, &
                                     cpt_m=i3)
                   Case ('L', 'T') ! - prediction limits, and thresholds -
                     c_thresh = make_label('L', ls_threshs(i3))
                     Call write_tag (iout, ifail, &
                                     cpt_limit=c_thresh)
                   Case ('M') ! - Models -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(i3)%var,                                                &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(i3)%nv, cpt_row='T', cpt_col='station', &
                                     cpt_units=afield(i3)%unitc, cpt_missing=afield(i3)%rmiss)
                   Case ('O', 'R') ! - odds, and relative odds -
                     Call write_tag (iout, ifail, &
                                     cpt_c=i3)
                   Case ('P') ! - probabilities -
                     Call write_tag (iout, ifail, &
                                     cpt_c=i3, cpt_prob=prob(i3))
                  End Select
               End If
               If (ifail /= 0) GoTo 1
            End If
!
! Print station information
            Do i = 1, afield(l)%nv
               If (output_value(iout, c=cstn(i,l), ldelim=.true., ladv=(i==afield(l)%nv)) /= 0) GoTo 1
            End Do
            If (itags == 1) Then
               If (output_value(iout, c='cpt:Y', ladv=.false.) /= 0) GoTo 1
            End If
            Do i = 1, afield(l)%nv
               If (output_value(iout, r=rlat(i,l), ldelim=.true., ladv=(i==afield(l)%nv), iprecs=ip) /= 0) GoTo 1
            End Do
            If (itags == 1) Then
               If (output_value(iout, c='cpt:X', ladv=.false.) /= 0) GoTo 1
            End If
            Do i = 1, afield(l)%nv
               If (output_value(iout, r=rlng(i,l), ldelim=.true., ladv=(i==afield(l)%nv), iprecs=ip) /= 0) GoTo 1
            End Do
!
! Print station data
            kk = 0
            Do k = 1, nt
               If (kuse(k)) kk = kk + 1
               If (ifd == 1) Then
                  ii = 1
                  ij = 0
               Else
                  ii = Sum(afield(1:l-1)%nva) + 1
                  ij = Sum(afield(1:l-1)%nv)
               End If
               Do ilf = 1, nls
                  cout = get_cdate(afield(l)%tssn%tprd(1)+(it0+k-1), 1, io_iso)
                  If (output_value(iout, c=cout, ladv=.false.) /= 0) GoTo 1
                  Do i = 1, afield(l)%nv
                     If (kuse(k)) Then
                        ij = ij + 1
                        If (iuse(ii) == ij) Then
                           If (output_value(iout, r=v(ii,kk,i3), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                           ii = ii + 1
                        Else
                           If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                        End If
                     Else
                        If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                     End If
                  End Do
                  Write (Unit=iout, Fmt='()', Advance='yes', Err=1)
               End Do
            End Do
         End Do
      End Do
    Case ('unformatted')
      Do i3 = 1, n3
         Call write_unform (iout, afile, nt, nv, v(:,:,i3), iuse, kuse, afield(1)%rmiss, ifail)
         If (ifail /= 0) Return
      End Do
    Case ('GrADS')
      Call write_grads_stns3 (iout, afile, nt, nls, afield(1)%nva, n3, v(:,:,:), afield(:)%tssn%tprd(1)%sdate, &
           cstn(:,1), rlat(:,1), rlng(:,1), iuse, kuse, afield(1)%rmiss, ifail)
      If (ifail /= 0) Return
      Select Case (ctype)
       Case ('E') ! - ensemble -
       Case ('L', 'T') ! - prediction limits, and thresholds -
         Do i3 = 1, n3
            c_thrs(i3) = Trim(make_label('L', ls_threshs(i3)))
         End Do
         Call write_grads_stns_ctrl (iout+1, afile, afield(:), nfs, nls, nt, n3, ifail, &
              cvar=c_thrs(:))
       Case ('M') ! - Models -
       Case ('O', 'P', 'R') ! - odds, probabilities, relative odds -
         Call write_grads_stns_ctrl (iout+1, afile, afield(:), nfs, nls, nt, n3, ifail, &
              cvar=ls_cats(1:)%c)
      End Select
      If (ifail /= 0) Return
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_stnss
!
!
!
 Subroutine write_unrfs (ctype, iout, afile, afield, nfs, nls, it0, nt, nv, n3, v, iuse, kuse, cstn, ifail, &
             prob, cl)
!
! Outputs data in unreferenced format
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants, Only: ca_ffmts, io_iso, lstn
   Use labels,            Only: c_thresh=>c_lab, &
                                ls_threshs, &
                                make_label
   Use time,              Only: Operator(+), &
                                get_cdate
   Use iofiles,           Only: ofile, &
                                itags
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: it0  ! - time 0 -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of series -
   Integer, Intent(In) :: n3   ! - third Dimension -
!
   Character(Len=1), Intent(In) :: ctype ! - output Type -
!                                            'E' - ensemble -
!                                            'L' - prediction limits -
!                                            'M' - Models -
!                                            'T' - thresholds -
!                                            'O' - odds -
!                                            'P' - probabilities -
!                                            'R' - relative odds -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v ! - data -
!
   Character(Len=lstn), Dimension(:,:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: cl ! - confidence level -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: prob ! - climatological probabilities -
!
! Locals
!
! Local scalars
   Integer :: i   ! - series index -
   Integer :: k   ! - time index -
   Integer :: l   ! - lagged field index -
   Integer :: i3  ! - third Dimension index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged-field index -
   Integer :: ii  ! - available series index -
   Integer :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Print tag line
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Do ifd = 1, nfs
         l = (ifd - 1)*nfs + 1
         Do i3 = 1, n3
            If (itags == 1) Then
               If (i3 == 1) Then
                  Select Case (ctype)
                   Case ('E') ! - ensemble -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_m=i3,                                    &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units=afield(l)%unitc, cpt_missing=afield(l)%rmiss)
                   Case ('L') ! - prediction limits -
                     c_thresh = make_label('L', ls_threshs(i3))
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var,                                              &
                                     cpt_clev=cl, cpt_limit=c_thresh,                                      &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units=Trim(afield(l)%unitc), cpt_missing=afield(l)%rmiss)
                   Case ('M') ! - Models -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(i3)%var,                                              &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(i3)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units=afield(i3)%unitc, cpt_missing=afield(i3)%rmiss)
                   Case ('O') ! - odds -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_c=i3,                                    &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units='odds', cpt_missing=afield(l)%rmiss)
                   Case ('P') ! - probabilities -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_c=i3, cpt_prob=prob(i3),                 &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units='probability (%)', cpt_missing=afield(l)%rmiss)
                   Case ('R') ! - relative odds -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var, cpt_c=i3,                                    &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units='relative odds', cpt_missing=afield(l)%rmiss)
                   Case ('T') ! - thresholds -
                     c_thresh = make_label('L', ls_threshs(i3))
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(l)%var,                                              &
                                     cpt_limit=c_thresh,                                                   &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(l)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units=afield(i3)%unitc, cpt_missing=afield(i3)%rmiss)
                  End Select
               Else
                  Select Case (ctype)
                   Case ('E') ! - ensemble -
                     Call write_tag (iout, ifail, &
                                     cpt_m=i3)
                   Case ('L', 'T') ! - prediction limits, and thresholds -
                     c_thresh = make_label('L', ls_threshs(i3))
                     Call write_tag (iout, ifail, &
                                     cpt_limit=c_thresh)
                   Case ('M') ! - Models -
                     Call write_tag (iout, ifail, &
                                     cpt_field=afield(i3)%var,                                              &
                                     cpt_nrow=nt*nls, cpt_ncol=afield(i3)%nv, cpt_row='T', cpt_col='index', &
                                     cpt_units=afield(i3)%unitc, cpt_missing=afield(i3)%rmiss)
                   Case ('O', 'R') ! - odds, and relative odds -
                     Call write_tag (iout, ifail, &
                                     cpt_c=i3)
                   Case ('P') ! - probabilities -
                     Call write_tag (iout, ifail, &
                                     cpt_c=i3, cpt_prob=prob(i3))
                  End Select
               End If
               If (ifail /= 0) GoTo 1
            End If
!
! Print index names
            Do i = 1, afield(l)%nv
               If (output_value(iout, c=cstn(i,l), ldelim=.true., ladv=(i==afield(l)%nv)) /= 0) GoTo 1
            End Do
!
! Print unreferenced data
            kk = 0
            Do k = 1, nt
               If (kuse(k)) kk = kk + 1
               ii = 1
               Do ilf = 1, nls
                  l = (ifd - 1)*nfs + ilf
                  cout = get_cdate(afield(l)%tssn%tprd(1)+(it0+k-1), 1, io_iso)
                  If (output_value(iout, c=cout, ladv=.false.) /= 0) GoTo 1
                  Do i = 1, afield(l)%nv
                     If (kuse(k)) Then
                        If (iuse(ii) == i) Then
                           If (output_value(iout, r=v(ii,kk,i3), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                           ii = ii + 1
                        Else
                           If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                        End If
                     Else
                        If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                     End If
                  End Do
                  Write (Unit=iout, Fmt='()', Advance='yes', Err=1)
               End Do
            End Do
         End Do
      End Do
    Case ('unformatted')
      Do i3 = 1, n3
         Call write_unform (iout, afile, nt, nv, v(:,:,i3), iuse, kuse, afield(1)%rmiss, ifail)
         If (ifail /= 0) Return
      End Do
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_unrfs
!
!
!
 Subroutine write_eigs (iout, afile, ne, sv, nt, ifail)
!
! Prints eigenvalues
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,      Only: ap, dp, sp, zero, oneh
   Use data_io_constants, Only: ca_prcs, ca_faccs, ca_ffmts
   Use iofiles,           Only: ofile, &
                                itags
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ne   ! - total number of eigenvalues -
   Integer, Intent(In) :: nt   ! - number of cases -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: sv ! - singular values -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i  ! - EOF index -
   Integer :: up ! - precision -
!
   Real(Kind=rp) :: df     ! - number of cases -
   Real(Kind=rp) :: evalue ! - eigenvalue -
   Real(Kind=rp) :: tvar   ! - total variance -
   Real(Kind=rp) :: pvar   ! - percentage variance -
   Real(Kind=rp) :: svar   ! - cumulative variance -
   Real(Kind=rp) :: spvar  ! - cumulative percentage variance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum, Trim
!
! Executable Statements
!
! Calculate total variance
   df = Real(nt-1, Kind=rp)
   tvar = Sum(sv(1:ne)**2/df)
!
! Calculate and print eigenvalues
   svar = zero
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      If (itags == 1) Then
         Call write_tag (iout, ifail, &
                         cpt_field='eigenvalues', cpt_nrow=ne, cpt_ncol=3, cpt_row='Mode', cpt_col='index')
         If (ifail /= 0) GoTo 1
      End If
      If (output_value(iout, c='Mode', ladv=.false.) /= 0) GoTo 1
      If (output_value(iout, c='Eigenvalue', ldelim=.true., ladv=.false.) /= 0) GoTo 1
      If (output_value(iout, c='% variance', ldelim=.true., ladv=.false.) /= 0) GoTo 1
      If (output_value(iout, c='Cum. % variance', ldelim=.true., ladv=.true.) /= 0) GoTo 1
      Do i = 1, ne
         evalue = sv(i)**2/df
         svar = svar + evalue
         pvar = oneh*evalue/tvar
         spvar = oneh*svar/tvar
         Write (Unit=iout, Fmt='(I0)', Advance='no', Err=1) i
         If (output_value(iout, r=evalue, ldelim=.true., ladv=.false.) /= 0) GoTo 1
         If (output_value(iout, r=pvar,   ldelim=.true., ladv=.false.) /= 0) GoTo 1
         If (output_value(iout, r=spvar,  ldelim=.true., ladv=.true. ) /= 0) GoTo 1
      End Do
    Case ('unformatted')
      Select Case (Trim(ca_faccs(afile%cfile%ffmt%iacc)))
       Case ('sequential', 'transparent', 'stream')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            Do i = 1, ne
               evalue = sv(i)**2/df
               svar = svar + evalue
               pvar = oneh*evalue/tvar
               spvar = oneh*svar/tvar
               Write (Unit=iout, Err=1) evalue, pvar, spvar
            End Do
         Else
            Do i = 1, ne
               evalue = sv(i)**2/df
               svar = svar + evalue
               pvar = oneh*evalue/tvar
               spvar = oneh*svar/tvar
               Write (Unit=iout, Err=1) Real(evalue, Kind=ap), Real(pvar, Kind=ap), Real(spvar, Kind=ap)
            End Do
         End If
       Case ('direct')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            Do i = 1, ne
               evalue = sv(i)**2/df
               svar = svar + evalue
               pvar = oneh*evalue/tvar
               spvar = oneh*svar/tvar
               Write (Unit=iout, Err=1, Rec=i) evalue, pvar, spvar
            End Do
         Else
            Do i = 1, ne
               evalue = sv(i)**2/df
               svar = svar + evalue
               pvar = oneh*evalue/tvar
               spvar = oneh*svar/tvar
               Write (Unit=iout, Err=1, Rec=i) Real(evalue, Kind=ap), Real(pvar, Kind=ap), Real(spvar, Kind=ap)
            End Do
         End If
      End Select
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_eigs
!
!
!
 Subroutine write_load (iout, afile, afield, idstr, nfs, nls, ne, nv, ip, iuse, eof, cfld, ifail, rlat, rlng, cstn)
!
! Prints spatial loadings
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_cpt_constants,  Only: id_grd, id_stn, id_ind
   Use data_io_constants,   Only: ca_ffmts, lstn
   Use data_time_constants, Only: nmn
   Use labels,              Only: cg_undefined_l
   Use iofiles,             Only: ofile, &
                                  itags
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output unit number -
   Integer, Intent(In) :: idstr ! - data structure -
   Integer, Intent(In) :: ip    ! - latitude and longitude output precision -
   Integer, Intent(In) :: nfs   ! - number of fields -
   Integer, Intent(In) :: nls   ! - number of lagged fields -
   Integer, Intent(In) :: ne    ! - number of EOF Modes -
   Integer, Intent(In) :: nv    ! - number of variables -
!
   Character(Len=*), Intent(In) :: cfld ! - field description -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: eof ! - spatial loadings -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlng ! - longitudes -
!
   Character(Len=lstn), Dimension(:,:), Intent(In), Optional :: cstn ! - station names -
!
! Locals
!
! Local arrays
   Logical, Dimension(ne) :: luse ! - dummy array -
!
! Local scalars
   Integer :: j   ! - longitude/station/series index -
   Integer :: ie  ! - EOF index -
   Integer :: l   ! - field index -
   Integer :: ii  ! - grid index -
   Integer :: ij  ! - available grid index -
   Integer :: ilf ! - lagged field index -
   Integer :: ifd ! - field index -
!
   Character(Len=nmn+10) :: cmode ! - Mode string -
   Character(Len=     8) :: csp   ! - output label for latitude-longitude space -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum, Trim
!
! Executable Statements
!
! Print formatted data
   luse(:) = .true.
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Select Case (idstr)
! - gridded data -
       Case (id_grd)
         Do l = 1, nfs*nls
            Do ie = 1, ne
               If (itags == 1) Then
                  If (Trim(afield(l)%var) /= Trim(cg_undefined_l) .and. Trim(afield(l)%var) /= 'undefined') Then
                     If (nls > 1) Then
                        Write (Unit=cmode, Fmt='(I0,A)') ie,'_('//Trim(afield(l)%cssn%cssn(1))//')'
                        Call write_tag (iout, ifail, &
                             cpt_field=Trim(afield(l)%var)//'_'//Trim(cfld), cpt_cmode=cmode, cpt_nrow=afield(l)%region%nlts, &
                             cpt_ncol=afield(l)%region%nlgs, cpt_row='Y', cpt_col='X')
                     Else
                        Call write_tag (iout, ifail, &
                             cpt_field=Trim(afield(l)%var)//'_'//Trim(cfld), cpt_mode=ie, cpt_nrow=afield(l)%region%nlts, &
                             cpt_ncol=afield(l)%region%nlgs, cpt_row='Y', cpt_col='X')
                     End If
                  Else
                     If (nls > 1) Then
                        Write (Unit=cmode, Fmt='(I0,A)') ie,'_('//Trim(afield(l)%cssn%cssn(1))//')'
                        Call write_tag (iout, ifail, &
                             cpt_field=Trim(cfld), cpt_cmode=cmode, cpt_nrow=afield(l)%region%nlts, &
                             cpt_ncol=afield(l)%region%nlgs, cpt_row='Y', cpt_col='X')
                     Else
                        Call write_tag (iout, ifail, &
                             cpt_field=Trim(cfld), cpt_mode=ie, cpt_nrow=afield(l)%region%nlts, cpt_ncol=afield(l)%region%nlgs, &
                             cpt_row='Y', cpt_col='X')
                     End If
                  End If
                  If (ifail /= 0) GoTo 1
                  csp = ' '
               Else
                  Write (Unit=csp, Fmt='(I0)') ie
               End If
               If (l>1) Then
                  ij = Sum(afield(1:l-1)%nv)
                  ii = Sum(afield(1:l-1)%nva) + 1
               Else
                  ij = 0
                  ii = 1
               End If
               Call write_gridded_block_v10 (iout, csp, afield(l), rlat(:,l), rlng(:,l), ij, ip, ifail, &
                    v=eof(ii:,ie),iuse=iuse(ii:))
            End Do
         End Do
! - station data -
       Case (id_stn)
         ii = 0
         ij = 1
         Do ifd = 1, nfs
            l = (ifd - 1)*nls + 1
            If (itags == 1) Then
               If (Trim(afield(l)%var) /= Trim(cg_undefined_l) .and. Trim(afield(l)%var) /= 'undefined') Then
                  Call write_tag (iout, ifail, &
                       cpt_field=Trim(afield(l)%var)//'_'//Trim(cfld), cpt_nrow=afield(l)%nv*nls, cpt_ncol=ne, cpt_row='station', &
                       cpt_col='Mode')
               Else
                  Call write_tag (iout, ifail, &
                       cpt_field=Trim(cfld), cpt_nrow=afield(l)%nv*nls, cpt_ncol=ne, cpt_row='station', cpt_col='Mode')
               End If
            End If
            Do ilf = 1, nls
               l = (ifd - 1)*nls + ilf
               If (output_value(iout, c='Station', ladv=.false.) /= 0) GoTo 1
               If (output_value(iout, c='Latitude', ldelim=.true., ladv=.false.) /= 0) GoTo 1
               If (output_value(iout, c='Longitude', ldelim=.true., ladv=.false.) /= 0) GoTo 1
               If (nls > 1) Then
                  If (output_value(iout, c='Season', ldelim=.true., ladv=.false.) /= 0) GoTo 1
               End If
               Do ie = 1, ne
                  If (output_value(iout, i=ie, ldelim=.true., ladv=(ie==ne)) /= 0) GoTo 1
               End Do
               Do j = 1, afield(l)%nv
                  ii = ii + 1
                  If (output_value(iout, c=cstn(j,l), ladv=.false.) /= 0) GoTo 1
                  If (output_value(iout, r=rlat(j,l), ldelim=.true., ladv=.false., iprecs=ip) /= 0) GoTo 1
                  If (output_value(iout, r=rlng(j,l), ldelim=.true., ladv=.false., iprecs=ip) /= 0) GoTo 1
                  If (nls > 1) Then
                     If (output_value(iout, c=afield(l)%cssn%cssn(1), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                  End If
                  If (iuse(ij) == ii) Then
                     Do ie = 1, ne
                        If (output_value(iout, r=eof(ij,ie), ldelim=.true., ladv=(ie==ne)) /= 0) GoTo 1
                     End Do
                     ij = ij + 1
                  Else
                     Do ie = 1, ne
                        If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=(ie==ne)) /= 0) GoTo 1
                     End Do
                  End If
               End Do
            End Do
         End Do
! - index data -
       Case (id_ind)
         If (itags == 1) Then
            If (Trim(afield(1)%var) /= Trim(cg_undefined_l) .and. Trim(afield(1)%var) /= 'undefined') Then
               Call write_tag (iout, ifail, &
                    cpt_field=Trim(afield(1)%var)//'_'//Trim(cfld), cpt_nrow=afield(1)%nv, cpt_ncol=ne, cpt_row='index', &
                    cpt_col='Mode')
            Else
               Call write_tag (iout, ifail, &
                    cpt_field=Trim(cfld), cpt_nrow=nv, cpt_ncol=ne, cpt_row='index', cpt_col='Mode')
            End If
         End If
         ii = 0
         ij = 1
         Do l = 1, nls
            If (output_value(iout, c='Index', ladv=.false.) /= 0) GoTo 1
            If (nls > 1) Then
               If (output_value(iout, c='Season', ldelim=.true., ladv=.false.) /= 0) GoTo 1
            End If
            Do ie = 1, ne
               If (output_value(iout, i=ie, ldelim=.true., ladv=(ie==ne)) /= 0) GoTo 1
            End Do
            Do j = 1, afield(l)%nv
               ii = ii + 1
               If (output_value(iout, c=cstn(j,l), ladv=.false.) /= 0) GoTo 1
               If (nls > 1) Then
                  If (output_value(iout, c=afield(l)%cssn%cssn(1), ldelim=.true., ladv=.false.) /= 0) GoTo 1
               End If
               If (iuse(ij) == ii) Then
                  Do ie = 1, ne
                     If (output_value(iout, r=eof(ij,ie), ldelim=.true., ladv=(ie==ne)) /= 0) GoTo 1
                  End Do
                  ij = ij + 1
               Else
                  Do ie = 1, ne
                     If (output_value(iout, r=afield(l)%rmiss, ldelim=.true., ladv=(ie==ne)) /= 0) GoTo 1
                  End Do
               End If
            End Do
         End Do
      End Select
    Case ('unformatted')
      Call write_unform (iout, afile, ne, nv, eof, iuse, luse, afield(1)%rmiss, ifail)
      If (ifail /= 0) Return
    Case ('GrADS')
      Select Case (idstr)
       Case (id_grd) ! - gridded data -
         Call write_unform (iout, afile, ne, nv, eof, iuse, luse, afield(1)%rmiss, ifail)
         If (ifail /= 0) Return
         Call write_grads_grid_ctrl (iout+1, afile, afield(:), nfs, nls, ne, 1, rlat(:,:), rlng(:,:), ifail)
         If (ifail /= 0) Return
       Case (id_stn) ! - station data -
         Call write_grads_stns (iout, afile, ne, nls, afield(1)%nva, eof, afield(:)%tssn%tprd(1)%sdate, &
              cstn(:,1), rlat(:,1), rlng(:,1), iuse(:), luse(:), afield(1)%rmiss, ifail)
         If (ifail /= 0) Return
         Call write_grads_stns_ctrl (iout+1, afile, afield(:), nfs, nls, ne, 1, ifail)
         If (ifail /= 0) Return
      End Select
      If (ifail /= 0) Return
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_load
!
!
!
 Subroutine write_scor (iout, cscore, afile, nt, it0, prd1, ne, kuse, miss, ts, ifail)
!
! Prints temporal scores
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,      Only: ap, dp, sp
   Use data_io_constants, Only: ca_prcs, ca_faccs, ca_ffmts, io_iso
   Use time,              Only: pprd, &
                                Operator(+), &
                                get_cdate
   Use arrays,            Only: awk, rwk
   Use iofiles,           Only: ofile, &
                                itags
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: ne   ! - number of EOF Modes -
   Integer, Intent(In) :: it0  ! - index of 0th case -
!
   Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
   Character(Len=*), Intent(In) :: cscore ! - score description -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
   Type(pprd), Intent(In) :: prd1 ! - first period -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: ts ! - time scores -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i  ! - EOF index -
   Integer :: k  ! - case index -
   Integer :: kk ! - available case index -
   Integer :: up ! - precision -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Print headers
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      If (itags == 1) Then
         Call write_tag (iout, ifail, &
                         cpt_field=cscore, cpt_nrow=nt, cpt_ncol=ne, cpt_row='T', cpt_col='index')
         If (ifail /= 0) GoTo 1
      End If
!
! Print temporal scores
      Do i = 1, ne
         If (output_value(iout, i=i, ldelim=.true., ladv=(i==ne)) /= 0) GoTo 1
      End Do
      kk = 0
      Do k = 1, nt
         If (kuse(k)) kk = kk + 1
         cout = get_cdate(prd1+(it0+k-1), 1, io_iso)
         If (output_value(iout, c=cout, ladv=.false.) /= 0) GoTo 1
         If (kuse(k)) Then
            Do i = 1, ne
               If (output_value(iout, r=ts(i,kk), ldelim=.true., ladv=(i==ne)) /= 0) GoTo 1
            End Do
         Else
            Do i = 1, ne
               If (output_value(iout, r=miss, ldelim=.true., ladv=(i==ne)) /= 0) GoTo 1
            End Do
         End If
      End Do
    Case ('unformatted')
      Select Case (Trim(ca_faccs(afile%cfile%ffmt%iacc)))
       Case ('sequential', 'transparent', 'stream')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            Do i = 1, ne
               kk = 0
               Do k = 1, nt
                  If (kuse(k)) Then
                     kk = kk + 1
                     rwk(k) = ts(i,kk)
                  Else
                     rwk(k) = miss
                  End If
               End Do
               Write (Unit=iout, Err=1) (rwk(k), k=1,nt)
            End Do
         Else
            Do i = 1, ne
               kk = 0
               Do k = 1, nt
                  If (kuse(k)) Then
                     kk = kk + 1
                     awk(k) = Real(ts(i,kk), Kind=ap)
                  Else
                     awk(k) = Real(miss, Kind=ap)
                  End If
               End Do
               Write (Unit=iout, Err=1) (awk(k), k=1,nt)
            End Do
         End If
       Case ('direct')
         Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            Do i = 1, ne
               kk = 0
               Do k = 1, nt
                  If (kuse(k)) Then
                     kk = kk + 1
                     rwk(k) = ts(i,kk)
                  Else
                     rwk(k) = miss
                  End If
               End Do
               Write (Unit=iout, Rec=i, Err=1) (rwk(k), k=1,nt)
            End Do
         Else
            Do i = 1, ne
               kk = 0
               Do k = 1, nt
                  If (kuse(k)) Then
                     kk = kk + 1
                     awk(k) = Real(ts(i,kk), Kind=ap)
                  Else
                     awk(k) = Real(miss, Kind=ap)
                  End If
               End Do
               Write (Unit=iout, Rec=i, Err=1) (awk(k),k=1,nt)
            End Do
         End If
      End Select
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_scor
!
!
!
 Subroutine write_cols (iout, afile, nrow, ifail, &
            d1, d2, d3, d4, d5, d6, i1, i2, i3, i4, i5, i6, r1, r2, r3, r4, r5, r6, &
            cpt_field, cpt_nrow, cpt_ncol, cpt_row, cpt_col,                        &
            cpt_c, cpt_tagsi, cpt_itags, cpt_tagsr, cpt_rtags, cpt_tagsc, cpt_ctags)
!
! Prints columns
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,      Only: ap, dp, sp
   Use data_io_constants, Only: ca_prcs, ca_faccs, ca_ffmts
   Use iofiles,           Only: ofile, &
                                itags
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout     ! - output unit number -
   Integer, Intent(In) :: nrow     ! - number of rows -
   Integer, Intent(In) :: cpt_nrow ! - number of rows -
   Integer, Intent(In) :: cpt_ncol ! - number of columns -
!
   Character(Len=*), Intent(In) :: cpt_field ! - field -
   Character(Len=*), Intent(In) :: cpt_row   ! - rows -
   Character(Len=*), Intent(In) :: cpt_col   ! - columns -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: cpt_c    ! - current category -
!
   Character(Len=*), Intent(In), Optional :: d1       ! - first column description -
   Character(Len=*), Intent(In), Optional :: d2       ! - second column description -
   Character(Len=*), Intent(In), Optional :: d3       ! - third column description -
   Character(Len=*), Intent(In), Optional :: d4       ! - fourth column description -
   Character(Len=*), Intent(In), Optional :: d5       ! - fifth column description -
   Character(Len=*), Intent(In), Optional :: d6       ! - sixth column description -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: i1 ! - first column (integer) -
   Integer, Dimension(:), Intent(In), Optional :: i2 ! - second column (integer) -
   Integer, Dimension(:), Intent(In), Optional :: i3 ! - third column (integer) -
   Integer, Dimension(:), Intent(In), Optional :: i4 ! - fourth column (integer) -
   Integer, Dimension(:), Intent(In), Optional :: i5 ! - fifth column (integer) -
   Integer, Dimension(:), Intent(In), Optional :: i6 ! - sixth column (integer) -
   Integer, Dimension(:), Intent(In), Optional :: cpt_itags ! - extra integer tag value -
!
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: cpt_rtags ! - extra Real tag value -
!
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: r1 ! - first column (real) -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: r2 ! - second column (real) -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: r3 ! - third column (real) -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: r4 ! - fourth column (real) -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: r5 ! - fifth column (real) -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: r6 ! - sixth column (real) -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_tagsi ! - extra integer tag -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_tagsr ! - extra Real tag -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_tagsc ! - extra Character tag -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_ctags ! - extra Character tag value -
!
! Locals
!
! Local scalars
   Integer :: i  ! - row index -
   Integer :: up ! - precision -
!
   Logical :: ladv1 ! - advance after first columm? -
   Logical :: ladv2 ! - advance after second columm? -
   Logical :: ladv3 ! - advance after third columm? -
   Logical :: ladv4 ! - advance after fourth columm? -
   Logical :: ladv5 ! - advance after fifth columm? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real, Trim
!
! Executable Statements
!
! Determine when to advance
   If (Present(d2) .or. Present(i2) .or. Present(r2)) Then
      ladv1 = .false.
   Else
      ladv1 = .true.
   End If
   If (Present(d3) .or. Present(i3) .or. Present(r3)) Then
      ladv1 = .false.
      ladv2 = .false.
   Else
      ladv2 = .true.
   End If
   If (Present(d4) .or. Present(i4) .or. Present(r4)) Then
      ladv1 = .false.
      ladv2 = .false.
      ladv3 = .false.
   Else
      ladv3 = .true.
   End If
   If (Present(d5) .or. Present(i5) .or. Present(r5)) Then
      ladv1 = .false.
      ladv2 = .false.
      ladv3 = .false.
      ladv4 = .false.
   Else
      ladv4 = .true.
   End If
   If (Present(d6) .or. Present(i6) .or. Present(r6)) Then
      ladv1 = .false.
      ladv2 = .false.
      ladv3 = .false.
      ladv4 = .false.
      ladv5 = .false.
   Else
      ladv5 = .true.
   End If
!
! Print tag line
   If (itags == 1) Then
      Call write_tag (iout, ifail, &
                      cpt_field=cpt_field, cpt_nrow=cpt_nrow, cpt_ncol=cpt_ncol, cpt_row=cpt_row, cpt_col=cpt_col,     &
                      cpt_c=cpt_c, cpt_tagsi=cpt_tagsi, cpt_itags=cpt_itags, cpt_tagsr=cpt_tagsr, cpt_rtags=cpt_rtags, &
                      cpt_tagsc=cpt_tagsc, cpt_ctags=cpt_ctags)
      If (ifail /= 0) GoTo 1
   End If
!
! Print columns
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      If (output_value(iout, c=d1, ladv=ladv1) /= 0) GoTo 1
      If (Present(d2)) Then
         If (output_value(iout, c=d2, ldelim=.true., ladv=ladv2) /= 0) GoTo 1
      End If
      If (Present(d3)) Then
         If (output_value(iout, c=d3, ldelim=.true., ladv=ladv3) /= 0) GoTo 1
      End If
      If (Present(d4)) Then
         If (output_value(iout, c=d4, ldelim=.true., ladv=ladv4) /= 0) GoTo 1
      End If
      If (Present(d5)) Then
         If (output_value(iout, c=d5, ldelim=.true., ladv=ladv5) /= 0) GoTo 1
      End If
      If (Present(d6)) Then
         If (output_value(iout, c=d6, ldelim=.true., ladv=.true.) /= 0) GoTo 1
      End If
      Do i = 1, nrow
         If (Present(i1)) Then
            If (output_value(iout, i=i1(i), ldelim=.true., ladv=ladv1) /= 0) GoTo 1
         Else If (Present(r1)) Then
            If (output_value(iout, r=r1(i), ldelim=.true., ladv=ladv1) /= 0) GoTo 1
         End If
         If (Present(i2)) Then
            If (output_value(iout, i=i2(i), ldelim=.true., ladv=ladv2) /= 0) GoTo 1
         Else If (Present(r2)) Then
            If (output_value(iout, r=r2(i), ldelim=.true., ladv=ladv2) /= 0) GoTo 1
         End If
         If (Present(i3)) Then
            If (output_value(iout, i=i3(i), ldelim=.true., ladv=ladv3) /= 0) GoTo 1
         Else If (Present(r3)) Then
            If (output_value(iout, r=r3(i), ldelim=.true., ladv=ladv3) /= 0) GoTo 1
         End If
         If (Present(i4)) Then
            If (output_value(iout, i=i4(i), ldelim=.true., ladv=ladv4) /= 0) GoTo 1
         Else If (Present(r4)) Then
            If (output_value(iout, r=r4(i), ldelim=.true., ladv=ladv4) /= 0) GoTo 1
         End If
         If (Present(i5)) Then
            If (output_value(iout, i=i5(i), ldelim=.true., ladv=ladv4) /= 0) GoTo 1
         Else If (Present(r5)) Then
            If (output_value(iout, r=r5(i), ldelim=.true., ladv=ladv5) /= 0) GoTo 1
         End If
         If (Present(i6)) Then
            If (output_value(iout, i=i6(i), ldelim=.true., ladv=.true.) /= 0) GoTo 1
         Else If (Present(r6)) Then
            If (output_value(iout, r=r6(i), ldelim=.true., ladv=.true.) /= 0) GoTo 1
         End If
      End Do
    Case ('unformatted')
      Select Case (Trim(ca_faccs(afile%cfile%ffmt%iacc)))
       Case ('sequential', 'transparent', 'stream')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            If (Present(i1)) Write (Unit=iout, Err=1) i1(:)
            If (Present(r1)) Write (Unit=iout, Err=1) r1(:)
            If (Present(i2)) Write (Unit=iout, Err=1) i2(:)
            If (Present(r2)) Write (Unit=iout, Err=1) r2(:)
            If (Present(i3)) Write (Unit=iout, Err=1) i3(:)
            If (Present(r3)) Write (Unit=iout, Err=1) r3(:)
            If (Present(i4)) Write (Unit=iout, Err=1) i4(:)
            If (Present(r4)) Write (Unit=iout, Err=1) r4(:)
            If (Present(i5)) Write (Unit=iout, Err=1) i5(:)
            If (Present(r5)) Write (Unit=iout, Err=1) r5(:)
            If (Present(i6)) Write (Unit=iout, Err=1) i6(:)
            If (Present(r6)) Write (Unit=iout, Err=1) r6(:)
         Else
            If (Present(i1)) Write (Unit=iout, Err=1) Real(i1(:), Kind=ap)
            If (Present(r1)) Write (Unit=iout, Err=1) Real(r1(:), Kind=ap)
            If (Present(i2)) Write (Unit=iout, Err=1) Real(i2(:), Kind=ap)
            If (Present(r2)) Write (Unit=iout, Err=1) Real(r2(:), Kind=ap)
            If (Present(i3)) Write (Unit=iout, Err=1) Real(i3(:), Kind=ap)
            If (Present(r3)) Write (Unit=iout, Err=1) Real(r3(:), Kind=ap)
            If (Present(i4)) Write (Unit=iout, Err=1) Real(i4(:), Kind=ap)
            If (Present(r4)) Write (Unit=iout, Err=1) Real(r4(:), Kind=ap)
            If (Present(i5)) Write (Unit=iout, Err=1) Real(i5(:), Kind=ap)
            If (Present(r5)) Write (Unit=iout, Err=1) Real(r5(:), Kind=ap)
            If (Present(i6)) Write (Unit=iout, Err=1) Real(i6(:), Kind=ap)
            If (Present(r6)) Write (Unit=iout, Err=1) Real(r6(:), Kind=ap)
         End If
       Case ('direct')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            If (Present(i1)) Write (Unit=iout, Err=1, Rec=2) i1(:)
            If (Present(r1)) Write (Unit=iout, Err=1, Rec=2) r1(:)
            If (Present(i2)) Write (Unit=iout, Err=1, Rec=3) i2(:)
            If (Present(r2)) Write (Unit=iout, Err=1, Rec=3) r2(:)
            If (Present(i3)) Write (Unit=iout, Err=1, Rec=3) i3(:)
            If (Present(r3)) Write (Unit=iout, Err=1, Rec=3) r3(:)
            If (Present(i4)) Write (Unit=iout, Err=1, Rec=3) i4(:)
            If (Present(r4)) Write (Unit=iout, Err=1, Rec=3) r4(:)
            If (Present(i5)) Write (Unit=iout, Err=1, Rec=3) i5(:)
            If (Present(r5)) Write (Unit=iout, Err=1, Rec=3) r5(:)
            If (Present(i6)) Write (Unit=iout, Err=1, Rec=3) i6(:)
            If (Present(r6)) Write (Unit=iout, Err=1, Rec=3) r6(:)
         Else
            If (Present(i1)) Write (Unit=iout, Err=1, Rec=2) Real(i1(:), Kind=ap)
            If (Present(r1)) Write (Unit=iout, Err=1, Rec=2) Real(r1(:), Kind=ap)
            If (Present(i2)) Write (Unit=iout, Err=1, Rec=3) Real(i2(:), Kind=ap)
            If (Present(r2)) Write (Unit=iout, Err=1, Rec=3) Real(r2(:), Kind=ap)
            If (Present(i3)) Write (Unit=iout, Err=1, Rec=3) Real(i3(:), Kind=ap)
            If (Present(r3)) Write (Unit=iout, Err=1, Rec=3) Real(r3(:), Kind=ap)
            If (Present(i4)) Write (Unit=iout, Err=1, Rec=3) Real(i4(:), Kind=ap)
            If (Present(r4)) Write (Unit=iout, Err=1, Rec=3) Real(r4(:), Kind=ap)
            If (Present(i5)) Write (Unit=iout, Err=1, Rec=3) Real(i5(:), Kind=ap)
            If (Present(r5)) Write (Unit=iout, Err=1, Rec=3) Real(r5(:), Kind=ap)
            If (Present(i6)) Write (Unit=iout, Err=1, Rec=3) Real(i6(:), Kind=ap)
            If (Present(r6)) Write (Unit=iout, Err=1, Rec=3) Real(r6(:), Kind=ap)
         End If
      End Select
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_cols
!
!
!
 Subroutine write_regr (iout, ipx, ipy, afile, bfile, bfield, nx, iuseb, yfile, yfield, ny, iusey, b, ifail, &
            rlatx, rlngx, cstnx, rlaty, rlngy, cstny, b0)
!
! Prints regression coefficients
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,       Only: ap, dp, sp
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use data_io_constants,  Only: ca_prcs, ca_faccs, ca_ffmts, lstn
   Use labels,             Only: cg_none_l
   Use arrays,             Only: awk, rwk
   Use space,              Only: make_coors
   Use iofiles,            Only: ifile, ofile, &
                                 itags
   Use fields,             Only: field, tfield
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ipx  ! - X latitude and longitude output precision -
   Integer, Intent(In) :: ipy  ! - Y latitude and longitude output precision -
   Integer, Intent(In) :: nx   ! - number of X variables -
   Integer, Intent(In) :: ny   ! - number of Y variables -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
   Type(ifile), Intent(In) :: bfile ! - predictors input file -
   Type(ifile), Intent(In) :: yfile ! - Y input file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuseb ! - predictor variable flags -
   Integer, Dimension(:), Intent(In) :: iusey ! - Y variable flags -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: b ! - regression coefficients -
!
   Type(field), Dimension(:), Intent(In) :: bfield ! - predictors field -
   Type(field), Dimension(:), Intent(In) :: yfield ! - Y field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: b0 ! - regression constants -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlatx ! - X latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlngx ! - X longitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlaty ! - Y latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlngy ! - Y longitudes -
!
   Character(Len=lstn), Dimension(:,:), Intent(In), Optional :: cstnx ! - X station names -
   Character(Len=lstn), Dimension(:,:), Intent(In), Optional :: cstny ! - Y station names -
!
! Locals
!
! Local scalars
   Integer :: i, j   ! - indices -
   Integer :: ib0    ! - b0 Present indicator -
   Integer :: ifdx   ! - X field index -
   Integer :: ifdy   ! - Y field index -
   Integer :: ilfx   ! - X lagged field index -
   Integer :: ilfy   ! - Y lagged field index -
   Integer :: lx     ! - X field / lagged field index -
   Integer :: ly     ! - Y field / lagged field index -
   Integer :: i1, j1 ! - indices -
   Integer :: i2, j2 ! - indices -
   Integer :: iy, jy ! - indices -
   Integer :: up     ! - precision -
!
   Real(Kind=ap) :: miss ! - missing value flag -
!
   Character(Len=16) :: cxcol ! - X column -
   Character(Len=16) :: cyrow ! - Y row -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real, Trim
!
! Executable Statements
!
! Print regression coefficients
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Allocate (tfield(bfile%nfl))
      tfield(:) = bfield(:)
      tfield(:)%unit = Trim(cg_none_l)
      tfield(:)%unitc = Trim(cg_none_l)
      Select Case (bfile%idstr)
       Case (id_grd) ! - gridded predictor data -
         i2 = 1
         i1 = 0
         Select Case (yfile%idstr)
          Case (id_grd) ! - gridded Y data -
            Do ifdy = 1, yfile%nfs
               Do ilfy = 1, yfile%nls
                  ly = (ifdy - 1)*yfile%nls + ilfy
                  Do iy = 1, yfield(ly)%region%nlts
                     Do jy = 1, yfield(ly)%region%nlgs
                        i1 = i1 + 1
                        If (yfile%nfs > 1) Then
                           If (yfile%nls > 1) Then
                              cout = Trim(make_coors(rlaty(iy,ly), rlngy(jy,ly)))//'; '//Trim(yfield(ly)%var)//&
                                                                                  &'; '//Trim(yfield(ly)%cssn%cssn(1))
                           Else
                              cout = Trim(make_coors(rlaty(iy,ly), rlngy(jy,ly)))//'; '//Trim(yfield(ly)%var)
                           End If
                        Else
                           If (yfile%nls > 1) Then
                              cout = Trim(make_coors(rlaty(iy,ly), rlngy(jy,ly)))//'; '//Trim(yfield(ly)%cssn%cssn(1))
                           Else
                              cout = Trim(make_coors(rlaty(iy,ly), rlngy(jy,ly)))
                           End If
                        End If
                        Call write_regr_grid (ifail)
                        If (ifail /= 0) GoTo 1
                     End Do
                  End Do
               End Do
            End Do
          Case (id_stn, id_ind) ! - nongridded Y data -
            Do ifdy = 1, yfile%nfs
               Do ilfy = 1, yfile%nls
                  ly = (ifdy - 1)*yfile%nls + ilfy
                  Do i = 1, yfield(ly)%nv
                     i1 = i1 + 1
                     If (yfile%nfs > 1) Then
                        If (yfile%nls > 1) Then
                           cout = Trim(cstny(i,ly))//'; '//Trim(yfield(ly)%var)//'; '//Trim(yfield(ly)%cssn%cssn(1))
                        Else
                           cout = Trim(cstny(i,ly))//'; '//Trim(yfield(ly)%var)
                        End If
                     Else
                        If (yfile%nls > 1) Then
                           cout = Trim(cstny(i,ly))//'; '//Trim(yfield(ly)%cssn%cssn(1))
                        Else
                           cout = Trim(cstny(i,ly))
                        End If
                     End If
                     Call write_regr_grid (ifail)
                     If (ifail /= 0) GoTo 1
                  End Do
               End Do
            End Do
         End Select
       Case (id_stn, id_ind) ! - nongridded predictor data -
         Select Case (bfile%idstr)
          Case (id_stn) ! - station X data -
            cxcol = 'X station'
          Case (id_ind) ! - unreferenced X data -
            cxcol = 'X index'
         End Select
         If (Present(b0)) Then
            ib0 = 1
         Else
            ib0 = 0
         End If
         Select Case (yfile%idstr)
          Case (id_grd) ! - gridded Y data -
            cyrow = 'Y grid'
            Do ifdx = 1, bfile%nfs
               Do ilfx = 1, bfile%nls
                  lx = (ifdx - 1)*bfile%nls + ilfx
                  Call write_regr_headers (ifail)
                  If (ifail /= 0) GoTo 1
                  Select Case (bfile%idstr)
                   Case (id_stn) ! - station X data -
                     If (output_value(iout, c='cpt:Y', ldelim=.true., ladv=.false.) /= 0) GoTo 1
                     Do i = 1, bfield(lx)%nv
                        If (output_value(iout, r=rlatx(i,lx), ldelim=.true., ladv=(i==bfield(lx)%nv), iprecs=ipx) /= 0) GoTo 1
                     End Do
                     If (output_value(iout, c='cpt:X', ldelim=.true., ladv=.false.) /= 0) GoTo 1
                     Do i = 1, bfield(lx)%nv
                        If (output_value(iout, r=rlngx(i,lx), ldelim=.true., ladv=(i==bfield(lx)%nv), iprecs=ipx) /= 0) GoTo 1
                     End Do
                  End Select
                  iy = 1
                  jy = 0
                  Do ifdy = 1, yfile%nfs
                     Do ilfy = 1, yfile%nls
                        ly = (ifdy - 1)*yfile%nls + ilfy
                        Do i = 1, yfield(ly)%region%nlts
                           Do j = 1, yfield(ly)%region%nlgs
                              jy = jy + 1
                              If (output_value(iout, r=rlaty(i,ly),                ladv=.false., iprecs=ipy) /= 0) GoTo 1
                              If (output_value(iout, r=rlngy(i,ly), ldelim=.true., ladv=.false., iprecs=ipy) /= 0) GoTo 1
                              If (yfile%nfs > 1) Then
                                 If (output_value(iout, c=yfield(ly)%var, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                              End If
                              If (yfile%nls > 1) Then
                                 If (output_value(iout, c=yfield(ly)%cssn%cssn(1), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                              End If
                              Call write_regr_nongrid (ifail)
                              If (ifail /= 0) GoTo 1
                           End Do
                        End Do
                     End Do
                  End Do
               End Do
            End Do
          Case (id_stn, id_ind) ! - nongridded Y data -
            Select Case (yfile%idstr)
             Case (id_stn) ! - station Y data -
               cyrow = 'Y station'
             Case (id_ind) ! - unreferenced Y data -
               cyrow = 'Y index'
            End Select
            Do ifdx = 1, bfile%nfs
               Do ilfx = 1, bfile%nls
                  lx = (ifdx - 1)*bfile%nls + ilfx
                  Call write_regr_headers (ifail)
                  If (ifail /= 0) GoTo 1
                  iy = 1
                  jy = 0
                  Do ifdy = 1, yfile%nfs
                     Do ilfy = 1, yfile%nls
                        ly = (ifdy - 1)*yfile%nls + ilfy
                        Do j = 1, yfield(ly)%nv
                           jy = jy + 1
                           If (output_value(iout, c=cstny(j,ly), ladv=.false.) /= 0) GoTo 1
                           Select Case (yfile%idstr)
                            Case (id_stn) ! - station Y data -
                              If (output_value(iout, r=rlaty(j,ly), ldelim=.true., ladv=.false., iprecs=ipy) /= 0) GoTo 1
                              If (output_value(iout, r=rlngy(j,ly), ldelim=.true., ladv=.false., iprecs=ipy) /= 0) GoTo 1
                           End Select
                           If (yfile%nfs > 1) Then
                              If (output_value(iout, c=yfield(ly)%var, ldelim=.true., ladv=.false.) /= 0) GoTo 1
                           End If
                           If (yfile%nls > 1) Then
                              If (output_value(iout, c=yfield(ly)%cssn%cssn(1), ldelim=.true., ladv=.false.) /= 0) GoTo 1
                           End If
                           Call write_regr_nongrid (ifail)
                           If (ifail /= 0) GoTo 1
                        End Do
                     End Do
                  End Do
               End Do
            End Do
         End Select
      End Select
      Deallocate (tfield)
    Case ('unformatted')
      Select Case (Trim(ca_faccs(afile%cfile%ffmt%iacc)))
       Case ('sequential', 'transparent', 'stream')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            i2 = 1
            Do i1 = 1, ny
               If (iusey(i2) == i1) Then
                  j2 = 1
                  Do j1 = 1, nx
                     If (iuseb(j2) == j1) Then
                        rwk(j1) = b(j2,i2)
                        j2 = j2 + 1
                     Else
                        rwk(j1) = bfield(1)%rmiss
                     End If
                  End Do
                  Write (Unit=iout, Err=1) (rwk(j1), j1=1,nx)
                  i2 = i2 + 1
               Else
                  Write (Unit=iout, Err=1) (bfield(1)%rmiss, j1=1,nx)
               End If
            End Do
         Else
            miss = Real(bfield(1)%rmiss, Kind=ap)
            i2 = 1
            Do i1 = 1, ny
               If (iusey(i2) == i1) Then
                  j2 = 1
                  Do j1 = 1, nx
                     If (iuseb(j2) == j1) Then
                        awk(j1) = Real(b(j2,i2), Kind=ap)
                        j2 = j2 + 1
                     Else
                        awk(j1) = miss
                     End If
                  End Do
                  Write (Unit=iout, Err=1) (awk(j1), j1=1,nx)
                  i2 = i2 + 1
               Else
                  Write (Unit=iout, Err=1) (miss, j1=1,nx)
               End If
            End Do
         End If
       Case ('direct')
         Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
          Case ('double')
            up = dp
          Case ('single')
            up = sp
         End Select
         If (up == rp) Then
            i2 = 1
            Do i1 = 1, ny
               If (iusey(i2) == i1) Then
                  j2 = 1
                  Do j1 = 1, nx
                     If (iuseb(j2) == j1) Then
                        rwk(j1) = b(j2,i2)
                        j2 = j2 + 1
                     Else
                        rwk(j1) = bfield(1)%rmiss
                     End If
                  End Do
                  Write (Unit=iout, Rec=i1, Err=1) (rwk(j1), j1=1,nx)
                  i2 = i2 + 1
               Else
                  Write (Unit=iout, Rec=i1, Err=1) (bfield(1)%rmiss, j1=1,nx)
               End If
            End Do
         Else
            miss = Real(bfield(1)%rmiss, Kind=ap)
            i2 = 1
            Do i1 = 1, ny
               If (iusey(i2) == i1) Then
                  j2 = 1
                  Do j1 = 1, nx
                     If (iuseb(j2) == j1) Then
                        awk(j1) = Real(b(j2,i2), Kind=ap)
                        j2 = j2 + 1
                     Else
                        awk(j1) = miss
                     End If
                  End Do
                  Write (Unit=iout, Rec=i1, Err=1) (awk(j1), j1=1,nx)
                  i2 = i2 + 1
               Else
                  Write (Unit=iout, Rec=i1, Err=1) (miss, j1=1,nx)
               End If
            End Do
         End If
      End Select
   End Select
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
!
 Contains
!
!
  Subroutine write_regr_headers (ifail)
!
! Prints headers for regression data
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i ! - field index -
!
   Logical :: ldlm ! - add delimitor? -
!
! Executable Statements
!
! Print headers for regression data
   If (itags == 1) Then
      Call write_tag (iout, ifail, &
           cpt_field=bfield(lx)%var, cpt_nrow=ny, cpt_ncol=bfield(lx)%nv+ib0, &
           cpt_row=Trim(cyrow), cpt_col=Trim(cxcol), cpt_units=Trim(cg_none_l), cpt_missing=bfield(lx)%rmiss)
      If (ifail /= 0) GoTo 1
   End If
   ldlm = .false.
   Select Case (yfile%idstr)
    Case (id_grd, id_stn) ! - gridded and station Y data -
      If (yfile%idstr == id_stn) Then
         If (output_value(iout, c='Station', ldelim=ldlm, ladv=.false.) /= 0) GoTo 1
         ldlm = .true.
      End If
      If (output_value(iout, c='Latitude',  ldelim=ldlm, ladv=.false.) /= 0) GoTo 1
      If (output_value(iout, c='Longitude', ldelim=.true., ladv=.false.) /= 0) GoTo 1
    Case (id_ind) ! - unreferenced Y data -
      If (output_value(iout, c='Index', ldelim=ldlm, ladv=.false.) /= 0) GoTo 1
   End Select
   If (yfile%nfs > 1) Then
      If (output_value(iout, c='Field', ldelim=.true., ladv=.false.) /= 0) GoTo 1
   End If
   If (yfile%nls > 1) Then
      If (output_value(iout, c='Season', ldelim=.true., ladv=.false.) /= 0) GoTo 1
   End If
   If (Present(b0)) Then
      If (output_value(iout, c='Constant', ldelim=.true., ladv=.false.) /= 0) GoTo 1
   End If
   Do i = 1, bfield(lx)%nv
      If (output_value(iout, c=cstnx(i,lx), ldelim=.true., ladv=(i==bfield(lx)%nv)) /= 0) GoTo 1
   End Do
   Select Case (bfile%idstr)
    Case (id_stn) ! - station X data -
      If (output_value(iout, c='cpt:Y', ldelim=.true., ladv=.false.) /= 0) GoTo 1
      Do i = 1, bfield(lx)%nv
         If (output_value(iout, r=rlatx(i,lx), ldelim=.true., ladv=(i==bfield(lx)%nv), iprecs=ipx) /= 0) GoTo 1
      End Do
      If (output_value(iout, c='cpt:X', ldelim=.true., ladv=.false.) /= 0) GoTo 1
      Do i = 1, bfield(lx)%nv
         If (output_value(iout, r=rlngx(i,lx), ldelim=.true., ladv=(i==bfield(lx)%nv), iprecs=ipx) /= 0) GoTo 1
      End Do
   End Select
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
!
   Return
  End Subroutine write_regr_headers
!
!
!
  Subroutine write_regr_grid (ifail)
!
! Prints gridded regression data
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Print gridded regression data
   If (itags == 1) Call write_tag (iout, ifail, &
                        cpt_name=cout)
   If (i1 == iusey(i2)) Then
      Call write_grid (iout, afile, tfield, bfile%nfs, bfile%nls, 1, nx, ipx, b(:,i2:i2), iuseb, [.true.], &
           rlatx, rlngx, ifail)
      i2 = i2 + 1
   Else
      Call write_grid (iout, afile, tfield, bfile%nfs, bfile%nls, 1, nx, ipx, b(:,1:1), iuseb, [.false.], &
           rlatx, rlngx, ifail)
   End If
!
   Return
  End Subroutine write_regr_grid
!
!
!
  Subroutine write_regr_nongrid (ifail)
!
! Prints nongridded regression data
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: ij ! - indices -
   Integer :: ix ! - indices -
   Integer :: jx ! - indices -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Sum
!
! Executable Statements
!
! Print nongridded regression data
   If (iusey(iy) == jy) Then
      If (Present(b0)) Then
         If (output_value(iout, r=b0(iy), ldelim=.true., ladv=.false.) /= 0) GoTo 1
      End If
      ix = 1
      jx = 0
      If (lx > 1) Then
         ix = ix + Sum(bfield(1:lx-1)%nva)
         jx = jx + Sum(bfield(1:lx-1)%nv)
      End If
      Do ij = 1, bfield(lx)%nv
         jx = jx + 1
         If (iuseb(ix) == jx) Then
            If (output_value(iout, r=b(ix,iy), ldelim=.true., ladv=(ij==bfield(lx)%nv)) /= 0) GoTo 1
            ix = ix + 1
         Else
            If (output_value(iout, r=bfield(lx)%rmiss, ldelim=.true., ladv=(ij==bfield(lx)%nv)) /= 0) GoTo 1
         End If
      End Do
      iy = iy + 1
   Else
      Do ix = 1, bfield(lx)%nv + ib0
         If (output_value(iout, r=bfield(lx)%rmiss, ldelim=.true., ladv=(ix==bfield(lx)%nv+ib0)) /= 0) GoTo 1
      End Do
   End If
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
!
   Return
  End Subroutine write_regr_nongrid
 End Subroutine write_regr
!
!
!
 Subroutine write_gridded_block_v10 (iout, csp, afield, rlat, rlng, ioff, ip, ifail, &
            v, iuse)
!
! Outputs a block of gridded data in v10 format without the tag line
!
! On exit Returns:
!    0 If no errors
!    1 If problem writing to file
!
! Modules
   Use data_numbers, Only: zero, r180, r360
   Use fields,       Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: ioff ! - offset -
   Integer, Intent(In) :: ip   ! - latitude and longitude output precision -
!
   Character(Len=*), Intent(In) :: csp ! - output label for latitude-longitude space -
!
   Type(field), Intent(In) :: afield ! - field -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i  ! - latitude index -
   Integer :: j  ! - longitude index -
   Integer :: ii ! - available series index -
   Integer :: ij ! - current variable -
!
   Real(Kind=rp) :: roff ! - longitude offset -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic All, Present
!
! Executable Statements
!
! Print gridded data
   ij = ioff
   ii = 1
   If (All(rlng(1:afield%region%nlgs) < -r180)) Then
      roff = -r360
   Else If (All(rlng(1:afield%region%nlgs) < r360)) Then
      roff = zero
   Else
      roff = r360
   End If
   If (output_value(iout, c=csp, ladv=.false.) /= 0) GoTo 1
   Do j = 1, afield%region%nlgs
      If (output_value(iout, r=rlng(j)-roff, ldelim=.true., ladv=(j==afield%region%nlgs), iprecs=ip) /= 0) GoTo 1
   End Do
   Do i = 1, afield%region%nlts
      If (output_value(iout, r=rlat(i), ldelim=.false., ladv=.false., iprecs=ip) /= 0) GoTo 1
      Do j = 1, afield%region%nlgs
         If (Present(v)) Then
            ij = ij + 1
            If (iuse(ii) == ij) Then
               If (output_value(iout, r=v(ii), ldelim=.true., ladv=(j==afield%region%nlgs)) /= 0) GoTo 1
               ii = ii + 1
            Else
               If (output_value(iout, r=afield%rmiss, ldelim=.true., ladv=(j==afield%region%nlgs)) /= 0) GoTo 1
            End If
         Else
            If (output_value(iout, r=afield%rmiss, ldelim=.true., ladv=(j==afield%region%nlgs)) /= 0) GoTo 1
         End If
      End Do
   End Do
   ifail = 0
!
   Return
!
! Error
1  ifail = 1
!
   Return
 End Subroutine write_gridded_block_v10
!
!
!
 Subroutine write_unform (iout, afile, nt, nv, v, iuse, kuse, miss, ifail)
!
! Outputs unformatted data
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,      Only: ap, dp, sp
   Use data_io_constants, Only: ca_prcs, ca_faccs
   Use arrays,            Only: rwk
   Use iofiles,           Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of series -
!
   Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v ! - data -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i  ! - series index -
   Integer :: j  ! - available series index -
   Integer :: k  ! - time index -
   Integer :: kk ! - available time index -
   Integer :: up ! - precision -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Identify precision
   Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
    Case ('double')
      up = dp
    Case ('single')
      up = sp
   End Select
!
! Print unformatted data
   kk = 0
   Do k = 1, nt
      If (kuse(k)) kk = kk + 1
      j = 1
      Do i = 1, nv
         If (kuse(k)) Then
            If (iuse(j) == i) Then
               rwk(i) = v(j,kk)
               j = j + 1
            Else
               rwk(i) = miss
            End If
         Else
            rwk(i) = miss
         End If
      End Do
      Select Case (Trim(ca_faccs(afile%cfile%ffmt%iacc)))
       Case ('sequential', 'transparent', 'stream')
         If (up == rp) Then
            Write (Unit=iout, Err=1) (rwk(i),i=1,nv)
         Else
            Write (Unit=iout, Err=1) (Real(rwk(i), Kind=ap),i=1,nv)
         End If
       Case ('direct')
         If (up == rp) Then
            Write (Unit=iout, Rec=k, Err=1) (rwk(i),i=1,nv)
         Else
            Write (Unit=iout, Rec=k, Err=1) (Real(rwk(i), Kind=ap),i=1,nv)
         End If
      End Select
   End Do
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_unform
!
!
!
 Subroutine write_grads_stns (iout, afile, nt, nls, nst, v, sdate, cstn, rlat, rlng, iuse, kuse, miss, ifail)
!
! Outputs station data in unformatted GrADS format (assumes that the stations are identical for each field and lagged field)
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,        Only: ap, dp, sp, zero
   Use data_io_constants,   Only: ca_prcs
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, nmn
   Use time,                Only: pdate, &
                                  iseq, &
                                  date_diff
   Use iofiles,             Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nls  ! - number of lagged-fields -
   Integer, Intent(In) :: nst  ! - number of stations -
!
   Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v ! - data -
!
   Character(Len=*), Dimension(:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(pdate), Dimension(:), Intent(In) :: sdate ! - start dates -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i   ! - series index -
   Integer :: ii  ! - series index -
   Integer :: ilf ! - lagged-field index -
   Integer :: j   ! - available series index -
   Integer :: k   ! - time index -
   Integer :: kk  ! - available time index -
   Integer :: up  ! - precision -
!
   Real(Kind=rp) :: d    ! - data -
   Real(Kind=rp) :: rlag ! - lag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Identify precision
!
! Identify precision
   Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
    Case ('double')
      up = dp
    Case ('single')
      up = sp
   End Select
!
! Print unformatted data
   kk = 0
   Do k = 1, nt
      If (kuse(k)) kk = kk + 1
      rlag = zero
      Do ilf = 1, nls
         If (ilf > 1) Then
            Select Case (iseq)
             Case (isq_yr) ! - yearly -
               rlag = date_diff(sdate(1), sdate(ilf), isq_mn)/Real(nmn, Kind=rp)
             Case (isq_sn) ! - seasonal -
               rlag = date_diff(sdate(1), sdate(ilf), isq_mn)/Real(3, Kind=rp)
             Case (isq_mn, isq_so) ! - monthly -
               rlag = date_diff(sdate(1), sdate(ilf), isq_mn)
             Case (1:) ! - daily and sub-seasonal -
               rlag = date_diff(sdate(1), sdate(ilf), 1)
            End Select
         End If
         Do i = 1, nst
            j = iuse(i)
            If (up == rp) Then
               Write (Unit=iout) cstn(j)(1:8), rlat(j), rlng(j), zero, 1, 1
            Else
               Write (Unit=iout) cstn(j)(1:8), Real(rlat(j), Kind=ap), Real(rlng(j), Kind=ap), Real(zero, Kind=ap), 1, 1
            End If
            If (kuse(k)) Then
               ii = nst*(ilf - 1) + i
               d = v(ii,kk)
            Else
               d = miss
            End If
            If (up == rp) Then
               Write (Unit=iout, Err=1) d
            Else
               Write (Unit=iout, Err=1) Real(d, Kind=ap)
            End If
         End Do
      End Do
      If (up == rp) Then
         Write (Unit=iout) cstn(1)(1:8), rlat(1), rlng(1), rlag, 0, 1
      Else
         Write (Unit=iout) cstn(1)(1:8), Real(rlat(1), Kind=ap), Real(rlng(1), Kind=ap), Real(rlag, Kind=ap), 0, 1
      End If
   End Do
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_grads_stns
!
!
!
 Subroutine write_grads_stns3 (iout, afile, nt, nls, nst, n3, v, sdate, cstn, rlat, rlng, iuse, kuse, miss, ifail)
!
! Outputs station data in unformatted GrADS format (assumes that the stations are identical for each field and lagged field)
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_numbers,        Only: ap, dp, sp, zero
   Use data_io_constants,   Only: ca_prcs
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, nmn
   Use time,                Only: pdate, &
                                  iseq, &
                                  date_diff
   Use iofiles,             Only: ofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nls  ! - number of lagged-fields -
   Integer, Intent(In) :: nst  ! - number of stations -
   Integer, Intent(In) :: n3   ! - third dimension -
!
   Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
   Type(ofile), Intent(In) :: afile ! - output file -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: v ! - data -
!
   Character(Len=*), Dimension(:), Intent(In) :: cstn ! - station names -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - cases used? -
!
   Type(pdate), Dimension(:), Intent(In) :: sdate ! - start dates -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i   ! - series index -
   Integer :: ii  ! - series index -
   Integer :: i3  ! - third dimension index -
   Integer :: ilf ! - lagged-field index -
   Integer :: j   ! - available series index -
   Integer :: k   ! - time index -
   Integer :: kk  ! - available time index -
   Integer :: up  ! - precision -
!
   Real(Kind=rp) :: d    ! - data -
   Real(Kind=rp) :: rlag ! - lag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Identify precision
!
! Identify precision
   Select Case (Trim(ca_prcs(afile%cfile%ffmt%iprc)))
    Case ('double')
      up = dp
    Case ('single')
      up = sp
   End Select
!
! Print unformatted data
   kk = 0
   Do k = 1, nt
      If (kuse(k)) kk = kk + 1
      rlag = zero
      Do ilf = 1, nls
         If (ilf > 1) Then
            Select Case (iseq)
             Case (isq_yr) ! - yearly -
               rlag = date_diff(sdate(1), sdate(ilf), isq_mn)/Real(nmn, Kind=rp)
             Case (isq_sn) ! - seasonal -
               rlag = date_diff(sdate(1), sdate(ilf), isq_mn)/Real(3, Kind=rp)
             Case (isq_mn, isq_so) ! - monthly -
               rlag = date_diff(sdate(1), sdate(ilf), isq_mn)
             Case (1:) ! - daily and sub-seasonal -
               rlag = date_diff(sdate(1), sdate(ilf), 1)
            End Select
         End If
         Do i = 1, nst
            j = iuse(i)
            If (up == rp) Then
               Write (Unit=iout) cstn(j)(1:8), rlat(j), rlng(j), zero, 1, 1
            Else
               Write (Unit=iout) cstn(j)(1:8), Real(rlat(j), Kind=ap), Real(rlng(j), Kind=ap), Real(zero, Kind=ap), 1, 1
            End If
            Do i3 = 1, n3
               If (kuse(k)) Then
                  ii = nst*(ilf - 1) + i
                  d = v(ii,kk,i3)
               Else
                  d = miss
               End If
               If (up == rp) Then
                  Write (Unit=iout, Err=1) d
               Else
                  Write (Unit=iout, Err=1) Real(d, Kind=ap)
               End If
           End Do
         End Do
      End Do
      If (up == rp) Then
         Write (Unit=iout) cstn(1)(1:8), rlat(1), rlng(1), rlag, 0, 1
      Else
         Write (Unit=iout) cstn(1)(1:8), Real(rlat(1), Kind=ap), Real(rlng(1), Kind=ap), Real(rlag, Kind=ap), 0, 1
      End If
   End Do
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_grads_stns3
!
!
!
 Subroutine write_grads_grid_ctrl (iout, cfile, cfield, nfs, nls, nt, nvs, rlat, rlng, ifail, &
            cvar)
!
! Outputs a GrADS control file for gridded data
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants,   Only: cendian, grads_ext, io_gra
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
   Use time,                Only: iseq, &
                                  get_cdate
   Use iofiles,             Only: ofile
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nvs  ! - number of variables -
!
   Type(ofile), Intent(In) :: cfile ! - control file -
!
! - input arrays -
   Real(Kind=rp), Intent(In) :: rlat(:,:) ! - latitudes -
   Real(Kind=rp), Intent(In) :: rlng(:,:) ! - longitudes -
!
   Type(field), Intent(In) :: cfield(:) ! - field specifications -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cvar ! - variable names -
!
! Locals
!
! Local scalars
   Character(Len=18) :: cdate ! - format statement -
   Character(Len=32) :: cout1 ! - output field -
   Character(Len=32) :: cout2 ! - output field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Trim
!
! Executable Statements
!
! Open control file
   Open (Unit = iout, File = Trim(cfile%cfile%fdir)//Trim(cfile%cfile%fname)//grads_ext, &
                    Action = 'write', &
                    IOstat = ifail, &
                      Form = 'formatted', &
                    Status = 'unknown')
   If (ifail /= 0) Then
      ifail = 1
      Return
   End If
!
! Write control file
   Write (Unit=iout, Fmt='(A)', Err=1) 'DSET    '//Trim(cfile%cfile%ffile)
   Write (Unit=iout, Fmt='(A)', Err=1) 'TITLE   '//Trim(cfile%cfile%desc)
   Write (Unit=cout1, Fmt=*) cfield(1)%rmiss
   Write (Unit=iout, Fmt='(2A)', Err=1) 'UNDEF   ', Trim(AdjustL(cout1))
   Write (Unit=iout, Fmt='(A)', Err=1) 'OPTIONS yrev sequential '//cendian//'_endian'
   Write (Unit=cout1, Fmt=*) rlng(1,1)
   Write (Unit=cout2, Fmt=*) rlng(2,1) - rlng(1,1)
   Write (Unit=iout, Fmt='(A,I0,4A)', Err=1) &
         'XDEF    ', cfield(1)%region%nlgs, ' LINEAR ', Trim(AdjustL(cout1)), ' ', Trim(AdjustL(cout2))
   Write (Unit=cout1, Fmt=*) rlat(cfield(1)%region%nlts,1)
   Write (Unit=cout2, Fmt=*) rlat(1,1) - rlat(2,1)
   Write (Unit=iout, Fmt='(A,I0,4A)', Err=1) &
         'YDEF    ', cfield(1)%region%nlts, ' LINEAR ', Trim(AdjustL(cout1)), ' ', Trim(AdjustL(cout2))
   cdate = get_cdate(cfield(1)%tssn%tprd(1)%sdate, io_gra)
   Select Case (iseq)
    Case (isq_yr) ! - yearly -
      Write (Unit=iout, Fmt=     '(A,I0,3A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), ' 1yr'
    Case (isq_sn) ! - seasonal -
      Write (Unit=iout, Fmt=     '(A,I0,3A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), ' 3mn'
    Case (isq_mn, isq_so) ! - monthly -
      Write (Unit=iout, Fmt=     '(A,I0,3A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), ' 1mn'
    Case (1:) ! - daily and sub-seasonal -
      Write (Unit=iout, Fmt='(A,I0,2A,I0,A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate)//' ', iseq, 'dy'
   End Select
   Write (Unit=iout, Fmt='(A)', Err=1) 'ZDEF    1 LINEAR 1 1'
!
! Output variables list
   Call write_grads_vars_ctrl (iout, cfield, nfs, nls, nvs, ifail, &
        cvar=cvar)
   GoTo 2
!
! Error
1  ifail = 3
!
2  Close (Unit=iout)
!
   Return
 End Subroutine write_grads_grid_ctrl
!
!
!
 Subroutine write_grads_stns_ctrl (iout, cfile, cfield, nfs, nls, nt, nvs, ifail, &
            cvar)
!
! Outputs a GrADS control file for station data
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_io_constants,   Only: cendian, grads_ext, io_gra
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
   Use time,                Only: iseq, &
                                  get_cdate
   Use iofiles,             Only: ofile
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nt   ! - number of cases -
   Integer, Intent(In) :: nvs  ! - number of variables -
!
   Type(ofile), Intent(In) :: cfile ! - data file -
!
! - input arrays -
   Type(field), Intent(In) :: cfield(:) ! - field specifications -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cvar ! - variable names -
!
! Locals
!
! Local scalars
   Character(Len=18) :: cdate ! - format statement -
   Character(Len=32) :: cout1 ! - output field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Trim
!
! Executable Statements
!
! Open control file
   Open (Unit = iout, File = Trim(cfile%cfile%fdir)//Trim(cfile%cfile%fname)//grads_ext, &
                    Action = 'write', &
                      Form = 'formatted', &
                    Status = 'unknown')
   If (ifail /= 0) Then
      ifail = 1
      Return
   End If
!
! Write control file
   Write (Unit=iout, Fmt='(A)', Err=1) 'DSET    '//Trim(cfile%cfile%ffile)
   Write (Unit=iout, Fmt='(A)', Err=1) 'DType   station'
   Write (Unit=iout, Fmt='(A)', Err=1) 'STNMAP  '//Trim(cfile%cfile%fdir)//Trim(cfile%cfile%fname)//'.map'
   Write (Unit=iout, Fmt='(A)', Err=1) 'TITLE   '//Trim(cfile%cfile%desc)
   Write (Unit=cout1, Fmt=*) cfield(1)%rmiss
   Write (Unit=iout, Fmt='(2A)', Err=1) 'UNDEF   ', Trim(AdjustL(cout1))
   Write (Unit=iout, Fmt='(A)', Err=1) 'OPTIONS '//cendian//'_endian'
   cdate = get_cdate(cfield(1)%tssn%tprd(1)%sdate, io_gra)
   Select Case (iseq)
    Case (isq_yr) ! - yearly -
      Write (Unit=iout, Fmt=     '(A,I0,3A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), ' 1yr'
    Case (isq_sn) ! - seasonal -
      Write (Unit=iout, Fmt=     '(A,I0,3A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), ' 3mn'
    Case (isq_mn, isq_so) ! - monthly -
      Write (Unit=iout, Fmt=     '(A,I0,3A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), ' 1mn'
    Case (1:) ! - daily and sub-seasonal -
      Write (Unit=iout, Fmt='(A,I0,2A,I0,A)', Err=1) 'TDEF    ', nt, ' LINEAR ', Trim(cdate), iseq, 'dy'
   End Select
!
! Output variables list
   Call write_grads_vars_ctrl (iout, cfield, nfs, nls, nvs, ifail, &
        cvar=cvar)
   GoTo 2
!
! Error
1  ifail = 3
!
2  Close (Unit=iout)
!
   Return
 End Subroutine write_grads_stns_ctrl
!
!
!
 Subroutine write_grads_vars_ctrl (iout, cfield, nfs, nls, nvs, ifail, &
            cvar)
!
! Outputs variables list in a GrADS control file
!
! On exit Returns:
!    0 If no errors
!    3 If problem writing to file
!
! Modules
   Use data_text, Only: c_letters_l, nlet
   Use labels,    Only: cg_na
   Use fields,    Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nvs  ! - number of variables -
!
! - input arrays -
   Type(field), Intent(In) :: cfield(:) ! - field specifications -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cvar ! - variable names -
!
! Locals
!
! Local scalars
   Integer :: ia, ib ! - ASCII collating sequence indicators -
   Integer :: ifd    ! - field index -
   Integer :: ilf    ! - lagged-field index -
   Integer :: iv     ! - variable index -
   Integer :: l      ! - field index -
   Integer :: nfl    ! - total number of fields and lagged-fields -
!
   Character :: c1 ! - first letter -
   Character :: c2 ! - second letter -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Output variables list
   nfl = nfs*nls*nvs
   Write (Unit=iout, Fmt='(A,I0)', Err=1) 'VARS ', nfl
   If (nfl <= nlet) Then
      ia = 1
      c1 = c_letters_l(ia:ia)
      Do iv = 1, nvs
         Do ifd = 1, nfs
            Do ilf = 1, nls
               l = (ifd - 1)*nls + ilf
               If (Present(cvar)) Then
                  If (Trim(cfield(l)%unitc) == cg_na .and. Trim(cfield(l)%unitc) == 'N/A') Then
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1, 0, 99, &
                           Trim(cfield(l)%var)//'_'//Trim(cfield(l)%cssn%cssn(1))//'_'//Trim(cvar(iv))
                  Else
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1, 0, 99, &
                           Trim(cfield(l)%var)//'_'//Trim(cfield(l)%cssn%cssn(1))//'_'//Trim(cvar(iv))//' '//Trim(cfield(l)%unitc)
                  End If
               Else
                  If (Trim(cfield(l)%unitc) == cg_na .and. Trim(cfield(l)%unitc) == 'N/A') Then
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1, 0, 99, &
                           Trim(cfield(l)%var)//'_'//cfield(l)%cssn%cssn(1)
                  Else
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1, 0, 99, &
                           Trim(cfield(l)%var)//'_'//cfield(l)%cssn%cssn(1)//' '//Trim(cfield(l)%unitc)
                  End If
               End If
               ia = ia + 1
               c1 = c_letters_l(ia:ia)
            End Do
         End Do
      End Do
   Else
      ia = 1
      ib = 1
      c1 = c_letters_l(ia:ia)
      c2 = c_letters_l(ib:ib)
      Do iv = 1, nvs
         Do ifd = 1, nfs
            Do ilf = 1, nls
               l = (ifd - 1)*nls + ilf
               If (Present(cvar)) Then
                  If (Trim(cfield(l)%unitc) == cg_na .and. Trim(cfield(l)%unitc) == 'N/A') Then
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1//c2, 0, 99, &
                           Trim(cfield(l)%var)//'_'//cfield(l)%cssn%cssn(1)//'_'//Trim(cvar(iv))
                  Else
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1//c2, 0, 99, &
                           Trim(cfield(l)%var)//'_'//cfield(l)%cssn%cssn(1)//'_'//Trim(cvar(iv))//' '//Trim(cfield(l)%unitc)
                  End If
               Else
                  If (Trim(cfield(l)%unitc) == cg_na .and. Trim(cfield(l)%unitc) == 'N/A') Then
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1//c2, 0, 99, &
                           Trim(cfield(l)%var)//'_'//cfield(l)%cssn%cssn(1)
                  Else
                     Write (Unit=iout, Fmt='(2X,A,I2,I3,1X,A)', Err=1) c1//c2, 0, 99, &
                           Trim(cfield(l)%var)//'_'//cfield(l)%cssn%cssn(1)//' '//Trim(cfield(l)%unitc)
                  End If
               End If
               ib = ib + 1
               If (ib > nlet) Then
                  ib = 1
                  ia = ia + 1
                  c1 = c_letters_l(ia:ia)
               End If
               c2 = c_letters_l(ib:ib)
            End Do
         End Do
      End Do
   End If
   Write (Unit=iout, Fmt='(A)', Err=1) 'ENDVARS'
!
   ifail = 0
!
   Return
!
! Error
1  ifail = 3
!
   Return
 End Subroutine write_grads_vars_ctrl
!
!
!
 Subroutine open_output (iout, lprint, afile, nfs, idstr, lrec, ifail, &
            ng)
!
! Opens CPT output file and prints XMLNS header
!
! On exit Returns:
!    0 If no errors
!    3 If problem creating output file
!
! Modules
   Use data_cpt_constants, Only: id_stn
   Use data_io_constants,  Only: ca_faccs, ca_ffmts, cxmlns_cpt
#ifdef NAGFOR
   Use data_io_constants,  Only: lfli
#endif
   Use iofiles,            Only: ofile, &
                                 itags, &
                                 set_fmt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output unit number -
   Integer, Intent(In) :: nfs   ! - number of fields -
   Integer, Intent(In) :: idstr ! - data structure identifier -
   Integer, Intent(In) :: lrec  ! - record length -
!
   Logical, Intent(In) :: lprint ! - print message? -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ng ! - number of categories -
!
! Locals
!
! Local scalars
   Character(Len=11) :: cformat ! - format -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Set format
   Call set_fmt (lrec, afile%cfile%ffmt)
   If (Trim(ca_ffmts(afile%cfile%ffmt%ifmt))/='GrADS') Then
      cformat = ca_ffmts(afile%cfile%ffmt%ifmt)
   Else
      cformat = 'unformatted'
      If (idstr == id_stn) afile%cfile%ffmt%iacc=3
   End If
!
! Open output file
   Select Case (Trim(ca_faccs(afile%cfile%ffmt%iacc)))
    Case ('sequential')
      If (lprint) Print *, 'Saving: '//Trim(afile%cfile%ffile)
      Open (Unit = iout, File = Trim(afile%cfile%ffile), &
                       Access = 'sequential', &
                       Action = 'Write', &
                         Form = Trim(cformat), &
#ifdef NAGFOR
                       IOstat = ifail, &
                         Recl = lfli, &
                       Status = 'unknown')
#else
                       IOstat = ifail, &
                       Status = 'unknown')
#endif
    Case ('direct')
      If (afile%cfile%ffmt%lrec == 0) afile%cfile%ffmt%lrec = lrec*afile%cfile%ffmt%iprc*4
      Write (Unit=*, Fmt='(3A,I0,A)') 'Saving: ', Trim(afile%cfile%ffile),' (record length ',afile%cfile%ffmt%lrec,')'
      Open (Unit=iout, File = Trim(afile%cfile%ffile), &
                     Access = 'direct', &
                     Action = 'Write', &
                       Form = Trim(cformat), &
                     IOstat = ifail, &
                       Recl = afile%cfile%ffmt%lrec, &
                     Status = 'unknown')
    Case ('transparent', 'stream')
      If (lprint) Print *, 'Saving: '//Trim(afile%cfile%ffile)
      Open (Unit=iout, File = Trim(afile%cfile%ffile), &
                     Access = Trim(ca_faccs(afile%cfile%ffmt%iacc)), &
                     Action = 'Write', &
                       Form = Trim(cformat), &
                     IOstat = ifail, &
                     Status = 'unknown')
   End Select
!
! Error
   If (ifail /= 0) Then
      ifail = 1
      Return
   End If
!
! Omit CPT tags
   If (itags == 0) Return
!
! Print XMLNS header
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      Write (Unit=iout, Fmt='(A)',IOstat=ifail) 'xmlns:cpt='//cxmlns_cpt
      If (ifail /= 0) GoTo 1
!
! Print number of fields
      If (nfs > 1) Then
         Call write_tag (iout, ifail, &
                         cpt_nfields=nfs)
      End If
      If (ifail /= 0) GoTo 1
      If (Present(ng)) Call write_tag (iout, ifail, &
                            cpt_ncats=ng)
   End Select
!
! Error
1  If (ifail /= 0) ifail = 3
!
   Return
 End Subroutine open_output
!
!
!
 Subroutine close_output (iout, afile, ifail)
!
! Closes CPT output file and prints XMLNS header
!
! On exit Returns:
!    0 If no errors
!   -1 If problem updating progress meter
!
! Modules
   Use progress_meter, Only: update_progress_meter
   Use errors,         Only: cpt_error
   Use iofiles,        Only: ofile, &
                             file_reset
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output unit number -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ifail ! - error indicator -
!
   Type(ofile), Intent(InOut) :: afile ! - output file -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Close file
   Close (Unit=iout)
!
! Report errors
   If (ifail /= 0) Call cpt_error ('write_results', .true., ifail, &
                        c_arg1=Trim(afile%cfile%desc), c_arg2=Trim(afile%cfile%ffile))
   Call file_reset (afile%cfile%lset, afile%cfile%fname)
!
! Update progress meter
   If (update_progress_meter(.false.) /= 0) ifail = -1
!
   Return
 End Subroutine close_output
!
!
!
 Subroutine write_tag (iout, ifail, &
                       cpt_nfields, cpt_ncats, cpt_name, cpt_field, cpt_model, cpt_c, cpt_prob, cpt_cmode, cpt_mode,       &
                       cpt_missing, cpt_t, cpt_s, cpt_z, cpt_m, cpt_clev, cpt_limit, cpt_nrow, cpt_ncol, cpt_row, cpt_col, &
                       cpt_units, &
                       cpt_cblock, cpt_block, &
                       cpt_tagsi, cpt_itags, cpt_tagsr, cpt_rtags, cpt_tagsc, cpt_ctags)
!
! Prints a CPT tag line
!
! On exit Returns:
!    0 If no errors
!    1 If problem writing to file
!
! Modules
   Use data_numbers,      Only: one
   Use data_io_constants, Only: io_iso, lvar
   Use maths,             Only: iprec
   Use labels,            Only: cg_na, cg_none_l
   Use time,              Only: pdate, pprd, &
                                Operator(==), &
                                get_cdate
   Use space,             Only: level
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: cpt_nfields ! - number of fields -
   Integer, Intent(In), Optional :: cpt_ncats   ! - number of categories -
   Integer, Intent(In), Optional :: cpt_c       ! - current category -
   Integer, Intent(In), Optional :: cpt_mode    ! - current Mode -
   Integer, Intent(In), Optional :: cpt_m       ! - ensemble member -
   Integer, Intent(In), Optional :: cpt_nrow    ! - number of rows -
   Integer, Intent(In), Optional :: cpt_ncol    ! - number of columns -
!
   Real(Kind=rp), Intent(In), Optional :: cpt_prob    ! - climatological probability -
   Real(Kind=rp), Intent(In), Optional :: cpt_clev    ! - confidence level -
   Real(Kind=rp), Intent(In), Optional :: cpt_missing ! - missing values flag -
!
   Character(Len=*), Intent(In), Optional :: cpt_field  ! - field -
   Character(Len=*), Intent(In), Optional :: cpt_model  ! - Model -
   Character(Len=*), Intent(In), Optional :: cpt_name   ! - name -
   Character(Len=*), Intent(In), Optional :: cpt_cmode  ! - current Mode -
   Character(Len=*), Intent(In), Optional :: cpt_row    ! - rows -
   Character(Len=*), Intent(In), Optional :: cpt_limit  ! - confidence limit -
   Character(Len=*), Intent(In), Optional :: cpt_col    ! - columns -
   Character(Len=*), Intent(In), Optional :: cpt_units  ! - units -
   Character(Len=*), Intent(In), Optional :: cpt_cblock ! - block -
   Character(Len=*), Intent(In), Optional :: cpt_block  ! - block value -
!
   Type(level), Intent(In), Optional :: cpt_z ! - level -
!
   Type(pprd), Intent(In), Optional :: cpt_t ! - date -
!
   Type(pdate), Intent(In), Optional :: cpt_s ! - start date -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: cpt_itags ! - extra integer tag value -
!
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: cpt_rtags ! - extra Real tag value -
!
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_ctags  ! - extra character tag value -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_tagsi  ! - extra integer tag -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_tagsr  ! - extra Real tag -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cpt_tagsc  ! - extra character tag -
!
! Locals
!
! Local scalars
   Integer :: i ! - extra tag counter -
! 
   Character(Len=    15) :: cfmt ! - format statement -
   Character(Len=lvar+1) :: cout ! - output field -
!
   Logical :: lfirst ! - first output field? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Len_Trim, Max, Present, Size, Trim
!
! Executable Statements
!
! Print nfields
   ifail = 0
   If (Present(cpt_nfields)) Then
      Write (Unit=iout, Fmt='(A,I0)', Err=1) 'cpt:nfields=', cpt_nfields
      Return
   End If
!
! Print ncats
   If (Present(cpt_ncats)) Then
      Write (Unit=iout, Fmt='(A,I0)', Err=1) 'cpt:ncats=', cpt_ncats
      Return
   End If
!
! Print name
   If (Present(cpt_name)) Then
      Write (Unit=iout, Fmt='(A)', Err=1) 'cpt:Name='//Trim(cpt_name)
      Return
   End If
!
! Print tags line
   lfirst = .true.
   If (Present(cpt_field)) Then
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:field='//Trim(cpt_field)
      lfirst = .false.
   End If
   If (Present(cpt_cblock) .and. Present(cpt_block)) Then
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:'//Trim(cpt_cblock)//'='//Trim(cpt_block)
      lfirst = .false.
   End If
   If (Present(cpt_tagsi) .and. Present(cpt_itags)) Then
      Do i = 1, Size(cpt_itags)
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:'//Trim(cpt_tagsi(i))//'=', cpt_itags(i)
         lfirst = .false.
      End Do
   End If
   If (Present(cpt_tagsr) .and. Present(cpt_rtags)) Then
      Do i = 1, Size(cpt_rtags)
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         Write (Unit=cout, Fmt=*) cpt_rtags(i)
         Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:'//Trim(cpt_tagsr(i))//'='//Trim(AdjustL(cout))
         lfirst = .false.
      End Do
   End If
   If (Present(cpt_tagsc) .and. Present(cpt_ctags)) Then
      Do i = 1, Size(cpt_ctags)
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:'//Trim(cpt_tagsc(i))//'='//Trim(cpt_ctags(i))
         lfirst = .false.
      End Do
   End If
   If (Present(cpt_model)) Then
      If (Len_Trim(cpt_model) > 0 .and. Trim(cpt_model) /= cg_na .and. Trim(cpt_model) /= 'N/A') Then
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:Model='//Trim(cpt_model)
         lfirst = .false.
      End If
   End If
   If (Present(cpt_c)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:C=', cpt_c
      lfirst = .false.
   End If
   If (Present(cpt_prob)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=cout, Fmt=*) cpt_prob
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:clim_prob='//Trim(AdjustL(cout))
      lfirst = .false.
   End If
   If (Present(cpt_cmode)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:Mode='//Trim(cpt_cmode)
      lfirst = .false.
   End If
   If (Present(cpt_mode)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:Mode=', cpt_mode
      lfirst = .false.
   End If
   If (Present(cpt_m)) Then
      If (cpt_m > 0) Then
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:M=', cpt_m
         lfirst = .false.
      End If
   End If
   If (Present(cpt_limit)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(2A)', Err=1, Advance='no') 'cpt:climit=', Trim(cpt_limit)
      lfirst = .false.
   End If
   If (Present(cpt_clev)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:clev='
      Write (Unit=cfmt, Fmt='(A,2(I1,A))') '(F', iprec(cpt_clev,3)+3, '.', iprec(cpt_clev,3), ')'
      Write (Unit=iout, Fmt=cfmt, Err=1, Advance='no') cpt_clev
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') '%'
      lfirst = .false.
   End If
   If (Present(cpt_z)) Then
      If (Trim(cpt_z%unit) /= Trim(cg_none_l) .and. Trim(cpt_z%unit) /= 'none') Then
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         If (cpt_z%hght > one) Then
            Write (Unit=cfmt, Fmt='(A,I1,A)') '(F24.', Max(1, iprec(cpt_z%hght, 3)), ')'
         Else
            Write (Unit=cfmt, Fmt='(A,I1,A)') '(F24.', iprec(cpt_z%hght, 9), ')'
         End If
         Write (Unit=cout, Fmt=cfmt) cpt_z%hght
         Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:Z='//Trim(AdjustL(cout))//' '//Trim(cpt_z%unit)
         lfirst = .false.
      End If
   End If
   If (Present(cpt_t)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      cout = get_cdate(cpt_t, 1, io_iso)
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:T='//Trim(cout)
      lfirst = .false.
   End If
   If (Present(cpt_s)) Then
      If (.not.(cpt_s == 0)) Then
         If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
         cout = get_cdate(cpt_s, io_iso)
         Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:S='//Trim(cout)
         lfirst = .false.
      End If
   End If
   If (Present(cpt_nrow)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:nrow=', cpt_nrow
      lfirst = .false.
   End If
   If (Present(cpt_ncol)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:ncol=', cpt_ncol
      lfirst = .false.
   End If
   If (Present(cpt_row)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:row='//cpt_row
      lfirst = .false.
   End If
   If (Present(cpt_col)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:col='//cpt_col
      lfirst = .false.
   End If
   If (Present(cpt_units)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:units='//Trim(cpt_units)
      lfirst = .false.
   End If
   If (Present(cpt_missing)) Then
      If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
      Write (Unit=cout, Fmt=*) cpt_missing
      Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:missing='//AdjustL(cout)
      lfirst = .false.
   End If
   Write (Unit=iout, Fmt='()', Err=1, Advance='yes')
!
   Return
!
1  ifail = 1
   Return
 End Subroutine write_tag
!
!
!
 Function output_value (iout, &
          c, i, r, ladv, ldelim, iprecs)
!
! Prints a delimited output
!
! Modules
   Use iofiles,  Only: cdelim, ipreco
!
! Function result
   Integer :: output_value
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: i      ! - output value -
   Integer, Intent(In), Optional :: iprecs ! - specified precision -
!
   Real(Kind=rp), Intent(In), Optional :: r ! - output value -
!
   Character(Len=*), Intent(In), Optional :: c ! - output value -
!
   Logical, Intent(In), Optional :: ladv   ! - advance? -
   Logical, Intent(In), Optional :: ldelim ! - include delimitor? -
!
! Locals
!
! Local scalars
   Integer :: ip ! - precision -
!
   Character(Len=4) :: cadv ! - advance? -
   Character(Len=8) :: cfmt ! - output format -
#if FTN95 == 1
   Character(Len=32) :: cout ! - output value -
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Present, Trim
!
! Executable Statements
!
! Identify whether to advance
   If (Present(ladv)) Then
      If (ladv) Then
         cadv = 'yes'
      Else
         cadv = 'no'
      End If
   Else
      cadv = 'yes'
   End If
!
! Identify whether to include delimitor
   If (Present(ldelim)) Then
      If (ldelim) Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) cdelim
   End If
!
! Print delimited output
! - integers -
   If (Present(i)) Then
      Write (Unit=iout, Fmt='(I0)', Advance=Trim(cadv), Err=1) i
! - reals -
   Else If (Present(r)) Then
      If (Present(iprecs)) Then
         ip = iprecs
      Else
         ip = ipreco
      End If
      If (ip >= 0) Then
#if FTN95 == 1
         Write (Unit=cfmt, Fmt='(A,I0,A)') '(F32.', ip, ')'  ! - FTN95 bug -
         Write (Unit=cout, Fmt=cfmt, Err=1) r
         Write (Unit=iout, Fmt='(A)', Advance=Trim(cadv), Err=1) Trim(AdjustL(cout))
#else
         Write (Unit=cfmt, Fmt='(A,I0,A)') '(F0.', ip, ')'
         Write (Unit=iout, Fmt=cfmt, Advance=Trim(cadv), Err=1) r
#endif
      Else
         Write (cout, Fmt=*) r
         Write (Unit=iout, Fmt='(A)', Advance=Trim(cadv), Err=1) Trim(AdjustL(cout))
      End If
! - characters -
   Else If (Present(c)) Then
      Write (Unit=iout, Fmt='(A)', Advance=Trim(cadv), Err=1) Trim(AdjustL(c))
   End If
!
   output_value = 0
   Return
!
! Errors
1  output_value = 1
   Return
 End Function output_value
!
!
!
 Subroutine get_gi_fmt (igood, iwid, g, gscale, cfmt)
!
! Sets an appropriate format for the goodness index
!
! Modules
   Use data_numbers,       Only: one, ten
   Use data_cpt_constants, Only: ncorr
   Use maths,              Only: magnitude
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igood ! - goodness score flag -
   Integer, Intent(In) :: iwid  ! - width -
!
   Real(Kind=rp), Intent(In) :: g ! - example goodness -
!
! - output scalars -
   Real(Kind=rp), Intent(Out) :: gscale ! - goodness scaling -
!
   Character(Len=4), Intent(Out) :: cfmt ! - format for goodness index -
!
! Locals
!
! Local scalars
   Integer :: idec ! - decimal places -
   Integer :: im   ! - magnitude -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Set format
   If (igood <= ncorr) Then
      gscale = one
      idec = 3
   Else
      im = magnitude(g)
      Select Case (im)
       Case (-1:10)
         gscale = one
         idec = Min(3, 10-im)
       Case (11:)
         gscale = one/ten**(im-3)
         idec = 3
       Case (:-2)
         gscale = ten**(im+3)
         idec = 3
      End Select
   End If
   Write (Unit=cfmt, Fmt='(I2,A,I1)') iwid, '.', idec
!
   Return
 End Subroutine get_gi_fmt
#if GUI == 0
!
!
!
 Function tags_lab()
!
! Sets File ~ Output CPT tags label
!
! Modules
   Use menus,   Only: set_menu_label
   Use iofiles, Only: itags
!
! Function result
   Integer :: tags_lab
!
! Executable Statements
!
! Set File ~ Output CPT tags label
   Call set_menu_label ( &
        itags=itags)
   tags_lab = 0
!
   Return
 End Function tags_lab
#endif
End Module cpt_output
