! Author: Simon Mason
Module menus
!
! Modules
   Use labels, Only: label
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: add_menu, init_menus, record_change, reset_menus, set_menu_greying, set_menu_label, set_mn_opts_trnss
!
! Derived-type definitions
!
! - menu item -
   Public :: menu
   Type menu
      Sequence
      Integer :: id        ! - identifier -
      Integer :: ion       ! - grey flag (0 = greyed; 1 = shown) -
      Integer :: iact      ! - activated (0 = no; 1 = yes)  -
      Type(label) :: l_lab ! - menu label -
   End Type menu
!
#if GUI == 0
! Parameters
!
! Integer parameters
   Integer, Parameter, Private :: lmax = 9 ! - maximum number of menu levels -
!
#endif
! Scalars
!
! Integer scalars
   Integer, Public :: isave      ! - change made to project settings flag -
!                                   (0 = no changes;
!                                    1 = changes made to settings;
!                                2,4,6 = results calculated, no changes to settings;
!                                3,5,7 = results calculated, changes to settings;
!                                   -1 = project file needs updating)
#if GUI == 1
   Integer, Public :: iexit      ! - exit program flag -
#else
   Integer, Public :: imenus = 1 ! - print menus? (default is on) -
!
! Logical scalars
   Logical, Public :: lmnact ! - menu activated? -
#endif
!
! Derived type scalars
   Type(menu), Public :: mn_file          ! - File -
   Type(menu), Public :: mn_file_closex   ! - File ~ Close X file -
   Type(menu), Public :: mn_file_closey   ! - File ~ Close Y file -
   Type(menu), Public :: mn_file_closez   ! - File ~ Close Forecast file -
   Type(menu), Public :: mn_file_datout   ! - File ~ Output Results ~ Output Results -
   Type(menu), Public :: mn_file_savegi   ! - File ~ Output Results ~ Save Goodness Index -
   Type(menu), Public :: mn_file_viewgi   ! - File ~ Output Results ~ View Goodness Index -
   Type(menu), Public :: mn_file_format   ! - File ~ Output-file format -
   Type(menu), Public :: mn_file_new      ! - File ~ New project -
   Type(menu), Public :: mn_file_open     ! - File ~ Open project -
   Type(menu), Public :: mn_file_outres   ! - File ~ Output Results -
   Type(menu), Public :: mn_file_prec     ! - File ~ Output-data precision -
   Type(menu), Public :: mn_file_save     ! - File ~ Save project -
   Type(menu), Public :: mn_file_saveas   ! - File ~ Save project as -
   Type(menu), Public :: mn_file_savepar  ! - File ~ Save as Parameter File -
   Type(menu), Public :: mn_file_tags     ! - File ~ CPT output-tags -
#if GUI == 0
   Type(menu), Public :: mn_file_show     ! - File ~ Show files -
#endif
!
   Type(menu), Public :: mn_edit          ! - Edit -
   Type(menu), Public :: mn_edit_datdom   ! - Edit ~ Data Domain -
   Type(menu), Public :: mn_edit_datdomx  ! - Edit ~ Data domain (X) -
   Type(menu), Public :: mn_edit_datdomy  ! - Edit ~ Data domain (Y) -
   Type(menu), Public :: mn_edit_ssn      ! - Edit ~ Target Season -
   Type(menu), Public :: mn_edit_startat  ! - Edit ~ Start-at -
   Type(menu), Public :: mn_edit_startatx ! - Edit ~ X Start-at -
   Type(menu), Public :: mn_edit_startaty ! - Edit ~ Y Start-at -
   Type(menu), Public :: mn_edit_startatz ! - Edit ~ Z Start-at -
   Type(menu), Public :: mn_edit_tssn     ! - Edit ~ Target Season -
!
   Type(menu), Public :: mn_acts          ! - Actions -
   Type(menu), Public :: mn_acts_calc     ! - Actions ~ Calculate -
   Type(menu), Public :: mn_acts_cv       ! - Actions ~ Calculate ~ Cross-validate -
   Type(menu), Public :: mn_acts_cv2      ! - Actions ~ Calculate ~ Double cross-validate -
   Type(menu), Public :: mn_acts_reset    ! - Actions ~ Reset -
   Type(menu), Public :: mn_acts_retro    ! - Actions ~ Calculate ~ Retro-active -
   Type(menu), Public :: mn_acts_verify   ! - Actions ~ Calculate ~ Verify -
!
   Type(menu), Public :: mn_tool          ! - Tools -
   Type(menu), Public :: mn_tool_attribs  ! - Tools ~ Verification ~ Attributes Diagram -
   Type(menu), Public :: mn_tool_aves     ! - Tools ~ Climatological Maps ~ Averages -
   Type(menu), Public :: mn_tool_climmap  ! - Tools ~ Climatological Maps -
   Type(menu), Public :: mn_tool_corrs    ! - Tools ~ Climatological Maps ~ Correlations -
   Type(menu), Public :: mn_tool_cv       ! - Tools ~ Validation ~ Cross-Validation -
   Type(menu), Public :: mn_tool_cvars    ! - Tools ~ Climatological Maps ~ Coefficients of Variation -
   Type(menu), Public :: mn_tool_cvboot   ! - Tools ~ Validation ~ Cross-Validation ~ Bootstrap -
   Type(menu), Public :: mn_tool_cvperf   ! - Tools ~ Validation ~ Cross-Validation ~ Performance Measures -
   Type(menu), Public :: mn_tool_cvtable  ! - Tools ~ Validation ~ Cross-Validation ~ Contingency Tables -
   Type(menu), Public :: mn_tool_cvmaps   ! - Tools ~ Validation ~ Cross-Validation ~ Skill Maps -
   Type(menu), Public :: mn_tool_c2       ! - Tools ~ Validation ~ Double Cross-Validation -
   Type(menu), Public :: mn_tool_c2boot   ! - Tools ~ Validation ~ Double Cross-Validation ~ Bootstrap -
   Type(menu), Public :: mn_tool_c2perf   ! - Tools ~ Validation ~ Double Cross-Validation ~ Performance Measures -
   Type(menu), Public :: mn_tool_c2table  ! - Tools ~ Validation ~ Double Cross-Validation ~ Contingency Tables -
   Type(menu), Public :: mn_tool_c2maps   ! - Tools ~ Validation ~ Double Cross-Validation ~ Skill Maps -
   Type(menu), Public :: mn_tool_fcasts   ! - Tools ~ Forecasts -
   Type(menu), Public :: mn_tool_fens     ! - Tools ~ Forecasts ~ Ensembles -
   Type(menu), Public :: mn_tool_fmaps    ! - Tools ~ Forecasts ~ Maps -
   Type(menu), Public :: mn_tool_fps      ! - Tools ~ Forecasts ~ Probabilities -
   Type(menu), Public :: mn_tool_fser     ! - Tools ~ Forecasts ~ Series -
   Type(menu), Public :: mn_tool_fval     ! - Tools ~ Forecasts ~ Values -
   Type(menu), Public :: mn_tool_models   ! - Tools ~ Models -
   Type(menu), Public :: mn_tool_odds     ! - Tools ~ Forecasts ~ Odds -
   Type(menu), Public :: mn_tool_pexc     ! - Tools ~ Forecasts ~ Exceedance Probabilities -
   Type(menu), Public :: mn_tool_pscores  ! - Tools ~ Verification ~ Probabilistic Scores -
   Type(menu), Public :: mn_tool_ra       ! - Tools ~ Validation ~ Retroactive -
   Type(menu), Public :: mn_tool_raperf   ! - Tools ~ Validation ~ Retroactive ~ Performance Measures -
   Type(menu), Public :: mn_tool_raboot   ! - Tools ~ Validation ~ Retroactive ~ Bootstrap -
   Type(menu), Public :: mn_tool_ramaps   ! - Tools ~ Validation ~ Retroactive ~ Skill Maps -
   Type(menu), Public :: mn_tool_ratable  ! - Tools ~ Validation ~ Retroactive ~ Contingency Tables -
   Type(menu), Public :: mn_tool_regress  ! - Tools ~ Regression -
   Type(menu), Public :: mn_tool_rhits    ! - Tools ~ Verification ~ Ranked Hits Diagram -
   Type(menu), Public :: mn_tool_screes   ! - Tools ~ Models ~ Scree Plots -
   Type(menu), Public :: mn_tool_stdevs   ! - Tools ~ Climatological Maps ~ Standard Deviations -
   Type(menu), Public :: mn_tool_tend     ! - Tools ~ Verification ~ Tendency Diagram -
   Type(menu), Public :: mn_tool_threshs  ! - Tools ~ Climatological Maps ~ Thresholds -
   Type(menu), Public :: mn_tool_validn   ! - Tools ~ Validation -
   Type(menu), Public :: mn_tool_verifn   ! - Tools ~ Verification -
   Type(menu), Public :: mn_tool_vroc     ! - Tools ~ Verification ~ ROC Diagram -
   Type(menu), Public :: mn_tool_vskill   ! - Tools ~ Verification ~ Skill Maps -
   Type(menu), Public :: mn_tool_wrlt     ! - Tools ~ Verification ~ Weather Roulette -
   Type(menu), Public :: mn_tool_xmap     ! - Tools ~ Climatological Maps ~ Predictors (X) -
   Type(menu), Public :: mn_tool_ymap     ! - Tools ~ Climatological Maps ~ Predictands (Y) -
   Type(menu), Public :: mn_tool_zmap     ! - Tools ~ Climatological Maps ~ Predictors (Z) -
#if GUI == 1
   Type(menu), Public :: mn_tool_cca      ! - Tools ~ Models ~ CCA -
   Type(menu), Public :: mn_tool_cvscat   ! - Tools ~ Validation ~ Cross-Validation ~ Scatter Diagrams -
   Type(menu), Public :: mn_tool_c2scat   ! - Tools ~ Validation ~ Double Cross-Validation ~ Scatter Diagrams -
   Type(menu), Public :: mn_tool_hps      ! - Tools ~ Forecasts ~ Probabilistic -
   Type(menu), Public :: mn_tool_rascat   ! - Tools ~ Validation ~ Retroactive ~ Scatter Diagrams -
   Type(menu), Public :: mn_tool_xeof     ! - Tools ~ Models ~ X EOFs -
   Type(menu), Public :: mn_tool_yeof     ! - Tools ~ Models ~ Y EOFs -
#else
   Type(menu), Public :: mn_tool_cvroc    ! - Tools ~ Validation ~ Cross-Validation ~ ROC Diagrams -
   Type(menu), Public :: mn_tool_c2roc    ! - Tools ~ Validation ~ DOuble Cross-Validation ~ ROC Diagrams -
   Type(menu), Public :: mn_tool_raroc    ! - Tools ~ Validation ~ Retroactive ~ ROC Diagrams -
#endif
!
   Type(menu), Public :: mn_opts          ! - Options -
   Type(menu), Public :: mn_opts_aveens   ! - Options ~ Data ~ Average Ensemble Members -
   Type(menu), Public :: mn_opts_begin    ! - Options ~ Expertise Level ~ Beginner -
   Type(menu), Public :: mn_opts_boot     ! - Options ~ Resampling Settings -
   Type(menu), Public :: mn_opts_cca      ! - Options ~ CCA Options -
   Type(menu), Public :: mn_opts_ccaadv   ! - Options ~ CCA Modes (Advanced) -
   Type(menu), Public :: mn_opts_climper  ! - Options ~ Climatological Period -
   Type(menu), Public :: mn_opts_cv       ! - Options ~ Cross-validation Options -
   Type(menu), Public :: mn_opts_data     ! - Options ~ Data -
   Type(menu), Public :: mn_opts_elev     ! - Options ~ Expertise Level -
   Type(menu), Public :: mn_opts_exclude  ! - Options ~ Data ~ Exclude -
   Type(menu), Public :: mn_opts_expert   ! - Options ~ Expertise Level ~ Expert -
   Type(menu), Public :: mn_opts_fcast    ! - Options ~ Forecast Settings -
   Type(menu), Public :: mn_opts_gcm      ! - Options ~ GCM Options -
   Type(menu), Public :: mn_opts_good     ! - Options ~ Goodness Index Options -
   Type(menu), Public :: mn_opts_lang     ! - Options ~ Language -
   Type(menu), Public :: mn_opts_link     ! - Options ~ Link-Function Options -
   Type(menu), Public :: mn_opts_mask     ! - Options ~ Masking Settings -
   Type(menu), Public :: mn_opts_maskdry  ! - Options ~ Dry-Masking Settings -
   Type(menu), Public :: mn_opts_maskskll ! - Options ~ Skill-Masking Settings -
   Type(menu), Public :: mn_opts_mdlcca   ! - Options ~ Models ~ CCA -
   Type(menu), Public :: mn_opts_mdlpcr   ! - Options ~ Models ~ PCR -
   Type(menu), Public :: mn_opts_mdlmlr   ! - Options ~ Models ~ MLR -
   Type(menu), Public :: mn_opts_mdlgcm   ! - Options ~ Models ~ GCM -
   Type(menu), Public :: mn_opts_miss     ! - Options ~ Data ~ Missing Values -
   Type(menu), Public :: mn_opts_missx    ! - Options ~ Data ~ X Missing Values -
   Type(menu), Public :: mn_opts_missy    ! - Options ~ Data ~ Y Missing Values -
   Type(menu), Public :: mn_opts_models   ! - Options ~ Models -
   Type(menu), Public :: mn_opts_pval     ! - Options ~ Skill Map P-Values -
   Type(menu), Public :: mn_opts_regr     ! - Options ~ Regression Options -
   Type(menu), Public :: mn_opts_show     ! - Options ~ Show Options -
   Type(menu), Public :: mn_opts_sortens  ! - Options ~ Data ~ Sort Ensemble Members -
   Type(menu), Public :: mn_opts_synch    ! - Options ~ Data ~ Synchronous Predictors -
   Type(menu), Public :: mn_opts_tailor   ! - Options ~ Tailoring Options -
   Type(menu), Public :: mn_opts_time     ! - Options ~ Time Settings -
   Type(menu), Public :: mn_opts_trnss    ! - Options ~ Transform Settings -
   Type(menu), Public :: mn_opts_trnsy    ! - Options ~ Data ~ Transform Y -
   Type(menu), Public :: mn_opts_verif    ! - Options ~ Verification Options -
   Type(menu), Public :: mn_opts_xmodes   ! - Options ~ X Modes -
   Type(menu), Public :: mn_opts_xmodeadv ! - Options ~ X Modes (Advanced) -
   Type(menu), Public :: mn_opts_ymodes   ! - Options ~ Y Modes -
   Type(menu), Public :: mn_opts_ymodeadv ! - Options ~ Y Modes (Advanced) -
   Type(menu), Public :: mn_opts_zerob    ! - Options ~ Data ~ Zero-Bound
#if GUI == 1
   Type(menu), Public :: mn_opts_bw       ! - Options ~ Graphics ~ Black and White -
   Type(menu), Public :: mn_opts_config   ! - Options ~ Configuration -
   Type(menu), Public :: mn_opts_console  ! - Options ~ Configuration ~ Console -
   Type(menu), Public :: mn_opts_cross    ! - Options ~ Graphics ~ Crosses -
   Type(menu), Public :: mn_opts_fntscal  ! - Options ~ Graphics ~ Font Scaling -
   Type(menu), Public :: mn_opts_graph    ! - Options ~ Graphics -
   Type(menu), Public :: mn_opts_grphscal ! - Options ~ Graphics ~ Graphics Scaling -
   Type(menu), Public :: mn_opts_masklak  ! - Options ~ Graphics ~ Mask Lakes -
   Type(menu), Public :: mn_opts_masklnd  ! - Options ~ Graphics ~ Mask Land -
   Type(menu), Public :: mn_opts_revcols  ! - Options ~ Graphics ~ Reverse Colours -
   Type(menu), Public :: mn_opts_scorgrph ! - Options ~ Graphics ~ Scores on Graphs -
   Type(menu), Public :: mn_opts_shadecat ! - Options ~ Graphics ~ Shade Categories -
   Type(menu), Public :: mn_opts_shadelnd ! - Options ~ Graphics ~ Shade Land -
   Type(menu), Public :: mn_opts_shadesea ! - Options ~ Graphics ~ Shade Sea -
   Type(menu), Public :: mn_opts_stnmrks  ! - Options ~ Graphics ~ Station Markers -
   Type(menu), Public :: mn_opts_txtscal  ! - Options ~ Configuration ~ Text-Scaling -
   Type(menu), Public :: mn_opts_vlines   ! - Options ~ Graphics ~ Vertical Lines -
#else
   Type(menu), Public :: mn_opts_errors   ! - Options ~ Error Handling -
   Type(menu), Public :: mn_opts_menus    ! - Options ~ Print Menus -
#endif
!
   Type(menu), Public :: mn_view          ! - View -
   Type(menu), Public :: mn_view_cca      ! - View ~ CCA -
   Type(menu), Public :: mn_view_diagnost ! - View ~ Diagnostics -
   Type(menu), Public :: mn_view_fcast    ! - View ~ Forecast -
   Type(menu), Public :: mn_view_gcm      ! - View ~ GCM -
   Type(menu), Public :: mn_view_mlr      ! - View ~ MLR -
   Type(menu), Public :: mn_view_pcr      ! - View ~ PCR -
   Type(menu), Public :: mn_view_pfv      ! - View ~ PFV -
   Type(menu), Public :: mn_view_verifn   ! - View ~ Verification -
!
   Type(menu), Public :: mn_help          ! - Help -
   Type(menu), Public :: mn_help_about    ! - Help ~ About -
   Type(menu), Public :: mn_help_help     ! - Help ~ Help -
!
   Type(menu), Public :: mn_data          ! - Data -
   Type(menu), Public :: mn_data_dload    ! - Data ~ Download -
   Type(menu), Public :: mn_data_merge    ! - Data ~ Merge -
   Type(menu), Public :: mn_data_update   ! - Data ~ Update -
!
   Type(menu), Public :: mn_screen_lentp  ! - Screen ~ Length of training period -
   Type(menu), Public :: mn_screen_nf     ! - number of forecasts -
#if GUI == 0
   Type(menu), Public :: mn_screen_openx  ! - Open X file -
   Type(menu), Public :: mn_screen_openy  ! - Open Y file -
   Type(menu), Public :: mn_screen_openz  ! - Open forecast file -
! Arrays
!
! Logical arrays
   Logical, Dimension(lmax), Private :: lgreys ! - greyed menu items? -
!
#endif
! Interfaces
!
! Generic interfaces
   Interface add_menu
      Module Procedure add_menu_lab
      Module Procedure add_menu_menu
      Module Procedure add_menu_txt1
#if FTN95 == 0 && GFORTRAN == 0
      Module Procedure add_menu_txt3
#endif
!      Module Procedure add_menu_txt
   End Interface add_menu
!
Contains
!
!
 Subroutine init_menus ()
!
! Initialises menu identifiers
!
! Modules
   Use labels, Only: ls_analyses
#if GUI == 0
!
! Locals
!
! Local scalars
   Integer :: itab ! - tab position -
#endif
!
#if GUI == 0
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, MaxVal
!
#endif
! Executable Statements
!
#if GUI == 0
! Set tab position
   itab = MaxVal(Len_Trim(ls_analyses(:)%c)) + 1
!
#endif
! Initialise menu identifiers and labels
! - screen -
   Call init_menus_screen ()
! - File -
   Call init_menus_file ()
! - Edit -
   Call init_menus_edit ()
! - Actions -
   Call init_menus_acts ()
! - Tools -
   Call init_menus_tool ()
! - Options -
   Call init_menus_opts ()
! - View -
   Call init_menus_view ()
! - Help -
   Call init_menus_help ()
! - Data -
   Call init_menus_data ()
!
! Reset all menus
   Call reset_menus ()
!
   Return
!
 Contains
!
!
  Subroutine init_menus_screen ()
!
! Initialises screen menu identifiers
!
! Modules
   Use labels, Only: cg_lentp, cg_nof, &
                     l_fcasts, &
                     make_label
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Initialise menu identifiers and labels
#if GUI == 0
   mn_screen_openx%id  =   1 ! - Open X file -
   mn_screen_openx%l_lab%c   =  'Open X input file'
   mn_screen_openy%id  =   2 ! - Open Y file -
   mn_screen_openy%l_lab%c   =  'Open Y input file'
   mn_screen_openz%id  =   3 ! - Open forecast file -
   mn_screen_openz%l_lab%c   =  'Open forecast file'
#endif
   mn_screen_lentp%id  =   7 ! - length of training period -
   mn_screen_lentp%l_lab%c   = Trim(cg_lentp)
   mn_screen_nf%id  =      9 ! - number of forecasts -
   mn_screen_nf%l_lab%c      = Trim(cg_nof)//' '//Trim(make_label('L', l_fcasts))
!
   Return
  End Subroutine init_menus_screen
!
!
!
  Subroutine init_menus_file ()
!
! Initialises File menu identifiers
!
! Modules
   Use labels, Only: l_close, l_fcast, l_file, l_new, l_open, l_outres, l_save, l_saveas, l_savegi, l_savepar, l_viewgi
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Trim
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_file%id          = 100 ! - File -
   mn_file%l_lab             = l_file
   mn_file_outres%id   = 110 ! - File ~ Output Results -
   mn_file_outres%l_lab      = l_outres
   mn_file_datout%id   = 111 ! - File ~ Output Results ~ Output Results -
   mn_file_datout%l_lab      = l_outres
   mn_file_savegi%id   = 112 ! - File ~ Output Results ~ Save Goodness Index -
   mn_file_savegi%l_lab      = l_savegi
   mn_file_viewgi%id   = 113 ! - File ~ Output Results ~ View Goodness Index -
   mn_file_viewgi%l_lab      = l_viewgi
   mn_file_closex%id   = 121 ! - Close X File -
   mn_file_closex%l_lab%c    = Trim(l_close%c)//' X'
   mn_file_closex%l_lab%iak  = Len_Trim(l_close%c) + 2
   mn_file_closey%id   = 122 ! - Close Y File -
   mn_file_closey%l_lab%c    = Trim(l_close%c)//' Y'
   mn_file_closey%l_lab%iak  = Len_Trim(l_close%c) + 2
   mn_file_closez%id   = 123 ! - Close Forecast File -
   mn_file_closez%l_lab%c    = Trim(l_close%c)//' '//Trim(l_fcast%c)
   mn_file_closez%l_lab%iak  = Len_Trim(l_close%c) + l_fcast%iak + 1
   mn_file_format%id   = 131 ! - Output-file format -
   mn_file_format%l_lab%c    = 'Output-file format'
   mn_file_format%l_lab%iak  = 13
   mn_file_prec%id     = 132 ! - Output-data precision -
   mn_file_prec%l_lab%c      = 'Output-data precision'
   mn_file_prec%l_lab%iak    = 13
#if GUI == 0
   mn_file_tags%id     = 133 ! - CPT output-tags -
   Call set_menu_label (itags=0)
   mn_file_show%id     = 140 ! - Show files -
   mn_file_show%l_lab%c      = 'Show files'
   mn_file_show%l_lab%iak    = 1
#endif
   mn_file_new%id      = 141 ! - New project -
   mn_file_new%l_lab         = l_new
   mn_file_open%id     = 142 ! - Open project -
   mn_file_open%l_lab        = l_open
   mn_file_save%id     = 143 ! - Save project -
   mn_file_save%l_lab        = l_save
   mn_file_saveas%id   = 144 ! - Save project as -
   mn_file_saveas%l_lab      = l_saveas
   mn_file_savepar%id  = 145 ! - Save as Parameter File -
   mn_file_savepar%l_lab     = l_savepar
!
   Return
  End Subroutine init_menus_file
!
!
!
  Subroutine init_menus_edit ()
!
! Initialises Edit menu identifiers
!
! Modules
   Use labels, Only: l_datdom, l_edit, l_predictandy, l_predictorx, l_predictorz, l_startat, l_tseason
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_edit%id          = 200 ! - Edit -
   mn_edit%l_lab             = l_edit
   mn_edit_datdom%id   = 210 ! - Edit ~ Data domain -
   mn_edit_datdom%l_lab      = l_datdom
   mn_edit_datdomx%id  = 211 ! - Edit ~ Data domain (X) -
   mn_edit_datdomx%l_lab     = l_predictorx
   mn_edit_datdomy%id  = 212 ! - Edit ~ Data domain (Y) -
   mn_edit_datdomy%l_lab     = l_predictandy
   mn_edit_startat%id  = 220 ! - Edit ~ Start at -
   mn_edit_startat%l_lab     = l_startat
   mn_edit_startatx%id = 221 ! - Edit ~ Start at (x) -
   mn_edit_startatx%l_lab    = l_predictorx
   mn_edit_startaty%id = 222 ! - Edit ~ Start at (Y) -
   mn_edit_startaty%l_lab    = l_predictandy
   mn_edit_startatz%id = 223 ! - Edit ~ Start at (Z) -
   mn_edit_startatz%l_lab    = l_predictorz
   mn_edit_ssn%id      = 230 ! - Edit ~ Target Season -
   mn_edit_ssn%l_lab         = l_tseason
   mn_edit_tssn%id     = 231 ! - Edit ~ Target Season -
   mn_edit_tssn%l_lab        = l_tseason
!
   Return
  End Subroutine init_menus_edit
!
!
!
  Subroutine init_menus_acts ()
!
! Initialises Actions menu identifiers
!
! Modules
   Use labels, Only: l_actions, l_calculate, l_cvn, l_cvn2, l_reset, l_retro, l_verify
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_acts%id          = 300 ! - Actions -
   mn_acts%l_lab             = l_actions
   mn_acts_calc%id     = 310 ! - Actions ~ Calculate -
   mn_acts_calc%l_lab        = l_calculate
   mn_acts_cv%id       = 311 ! - Actions ~ Calculate ~ Cross-validate -
   mn_acts_cv%l_lab          = l_cvn
   mn_acts_retro%id    = 312 ! - Actions ~ Calculate ~ Retro-active -
   mn_acts_retro%l_lab       = l_retro
   mn_acts_verify%id   = 313 ! - Actions ~ Calculate ~ Verify -
   mn_acts_verify%l_lab      = l_verify
   mn_acts_cv2%id      = 314 ! - Actions ~ Calculate ~ Double cross-validate -
   mn_acts_cv2%l_lab         = l_cvn2
   mn_acts_reset%id    = 321 ! - Actions ~ Reset -
   mn_acts_reset%l_lab       = l_reset
!
   Return
  End Subroutine init_menus_acts
!
!
!
  Subroutine init_menus_tool ()
!
! Initialises Tools menu identifiers
!
! Modules
   Use labels, Only: l_attribs, l_averages, l_boot, l_climmaps, l_correls, l_ctables, l_cvars, l_cvn, l_cvn2, l_ensembles,      &
                     l_exceedps, l_fcasts, l_maps, l_models, l_odds, l_performs, l_predictandy, l_predictorx, l_predictorz,     &
                     l_probs, l_regress, l_retro, l_rhitsdiag, l_rocdiag, l_scoresp, l_screes, l_series, l_skillmaps, l_stdevs, &
                     l_tenddiag, l_threshs, l_tools, l_validn, l_values, l_verifn, l_wrlt
#if GUI == 1
   Use labels, Only: l_ccamaps, l_loadsx, l_loadsy, l_probc, l_scatters
#endif
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_tool%id          = 400 ! - Tools -
   mn_tool%l_lab             = l_tools
! - validation -
   mn_tool_validn%id   = 410 ! - Tools ~ Validation -
   mn_tool_validn%l_lab      = l_validn
! - cross-validation -
   mn_tool_cv%id       = 4110 ! - Tools ~ Validation ~ Cross-Validation -
   mn_tool_cv%l_lab          = l_cvn
   mn_tool_cvperf%id   = 411 ! - Tools ~ Validation ~ Cross-Validation ~ Performance Measures -
   mn_tool_cvperf%l_lab      = l_performs
   mn_tool_cvboot%id   = 412 ! - Tools ~ Validation ~ Cross-Validation ~ Bootstrap -
   mn_tool_cvboot%l_lab      = l_boot
   mn_tool_cvmaps%id   = 413 ! - Tools ~ Validation ~ Cross-Validation ~ Skill Maps -
   mn_tool_cvmaps%l_lab      = l_skillmaps
#if GUI == 0
   mn_tool_cvroc%id    = 414 ! - Tools ~ Validation ~ Cross-Validation ~ ROC Diagrams -
   mn_tool_cvroc%l_lab       = l_rocdiag
#endif
   mn_tool_cvtable%id  = 415 ! - Tools ~ Validation ~ Cross-Validation ~ Contingency Tables -
   mn_tool_cvtable%l_lab     = l_ctables
#if GUI == 1
   mn_tool_cvscat%id   = 416 ! - Tools ~ Validation ~ Cross-Validation ~ Scatter Diagrams -
   mn_tool_cvscat%l_lab      = l_scatters
#endif
! - double cross-validation -
   mn_tool_c2%id       = 4120 ! - Tools ~ Validation ~ Double Cross-Validation -
   mn_tool_c2%l_lab          = l_cvn2
   mn_tool_c2perf%id   = 4121 ! - Tools ~ Validation ~ Double Cross-Validation ~ Performance Measures -
   mn_tool_c2perf%l_lab      = l_performs
   mn_tool_c2boot%id   = 4122 ! - Tools ~ Validation ~ Double Cross-Validation ~ Bootstrap -
   mn_tool_c2boot%l_lab      = l_boot
   mn_tool_c2maps%id   = 4123 ! - Tools ~ Validation ~ Double Cross-Validation ~ Skill Maps -
   mn_tool_c2maps%l_lab      = l_skillmaps
#if GUI == 0
   mn_tool_c2roc%id    = 4124 ! - Tools ~ Validation ~ Double Cross-Validation ~ ROC Diagrams -
   mn_tool_c2roc%l_lab       = l_rocdiag
#endif
   mn_tool_c2table%id  = 4125 ! - Tools ~ Validation ~ Double Cross-Validation ~ Contingency Tables -
   mn_tool_c2table%l_lab     = l_ctables
#if GUI == 1
   mn_tool_c2scat%id   = 4126 ! - Tools ~ Validation ~ Double Cross-Validation ~ Scatter Diagrams -
   mn_tool_c2scat%l_lab      = l_scatters
#endif
! - retroactive -
   mn_tool_ra%id       = 420 ! - Tools ~ Validation ~ Retroactive -
   mn_tool_ra%l_lab          = l_retro
   mn_tool_raperf%id   = 421 ! - Tools ~ Validation ~ Retroactive ~ Performance Measures -
   mn_tool_raperf%l_lab      = l_performs
   mn_tool_raboot%id   = 422 ! - Tools ~ Validation ~ Retroactive ~ Bootstrap -
   mn_tool_raboot%l_lab      = l_boot
   mn_tool_ramaps%id   = 423 ! - Tools ~ Validation ~ Retroactive ~ Skill Maps -
   mn_tool_ramaps%l_lab      = l_skillmaps
#if GUI == 0
   mn_tool_raroc%id    = 424 ! - Tools ~ Validation ~ Retroactive ~ ROC Diagrams -
   mn_tool_raroc%l_lab       = l_rocdiag
#endif
   mn_tool_ratable%id  = 425 ! - Tools ~ Validation ~ Retroactive ~ Contingency Tables -
   mn_tool_ratable%l_lab      = l_ctables
#if GUI == 1
   mn_tool_rascat%id   = 426 ! - Tools ~ Validation ~ Retroactive ~ Scatter Diagrams -
   mn_tool_rascat%l_lab       = l_scatters
#endif
! - verification -
   mn_tool_verifn%id   = 430 ! - Tools ~ Verification -
   mn_tool_verifn%l_lab      = l_verifn
   mn_tool_attribs%id  = 431 ! - Tools ~ Verification ~ Attributes Diagram -
   mn_tool_attribs%l_lab     = l_attribs
   mn_tool_vroc%id     = 432 ! - Tools ~ Verification ~ ROC Diagram -
   mn_tool_vroc%l_lab        = l_rocdiag
   mn_tool_pscores%id  = 433 ! - Tools ~ Verification ~ Probabilistic Scores -
   mn_tool_pscores%l_lab     = l_scoresp
   mn_tool_wrlt%id     = 434 ! - Tools ~ Verification ~ Weather Roulette -
   mn_tool_wrlt%l_lab        = l_wrlt
   mn_tool_tend%id     = 435 ! - Tools ~ Verification ~ Tendency Diagram -
   mn_tool_tend%l_lab        = l_tenddiag
   mn_tool_rhits%id    = 436 ! - Tools ~ Verification ~ Ranked Hits Diagram -
   mn_tool_rhits%l_lab       = l_rhitsdiag
   mn_tool_vskill%id   = 437 ! - Tools ~ Verification ~ Skill Maps -
   mn_tool_vskill%l_lab      = l_skillmaps
! - modes -
   mn_tool_models%id   = 440 ! - Tools ~ Models -
   mn_tool_models%l_lab      = l_models
   mn_tool_screes%id   = 441 ! - Tools ~ Models ~ Scree Plots -
   mn_tool_screes%l_lab      = l_screes
#if GUI == 1
   mn_tool_xeof%id     = 442 ! - Tools ~ Models ~ X EOFs -
   mn_tool_xeof%l_lab        = l_loadsx
   mn_tool_yeof%id     = 443 ! - Tools ~ Models ~ Y EOFs -
   mn_tool_yeof%l_lab        = l_loadsy
   mn_tool_cca%id      = 444 ! - Tools ~ Models ~ CCA -
   mn_tool_cca%l_lab         = l_ccamaps
#endif
! - forecasts -
   mn_tool_fcasts%id   = 450 ! - Tools ~ Forecasts -
   mn_tool_fcasts%l_lab      = l_fcasts
   mn_tool_fmaps%id    = 450 ! - Tools ~ Forecasts ~ Maps -
   mn_tool_fmaps%l_lab       = l_maps
   mn_tool_fser%id     = 451 ! - Tools ~ Forecasts ~ Series -
   mn_tool_fser%l_lab        = l_series
   mn_tool_fens%id     = 452 ! - Tools ~ Forecasts ~ Ensembles -
   mn_tool_fens%l_lab        = l_ensembles
   mn_tool_pexc%id     = 453 ! - Tools ~ Forecasts ~ Exceedance Probabilities -
   mn_tool_pexc%l_lab        = l_exceedps
   mn_tool_fval%id     = 454 ! - Tools ~ Forecasts ~ Values -
   mn_tool_fval%l_lab        = l_values
   mn_tool_fps%id      = 455 ! - Tools ~ Forecasts ~ Probabilities -
   mn_tool_fps%l_lab         = l_probs
   mn_tool_odds%id     = 456 ! - Tools ~ Forecasts ~ Odds -
   mn_tool_odds%l_lab        = l_odds
#if GUI == 1
   mn_tool_hps%id      = 457 ! - Tools ~ Forecasts ~ Probabilistic -
   mn_tool_hps%l_lab         = l_probc
#endif
! - climatological maps -
   mn_tool_climmap%id  = 460 ! - Tools ~ Climatological Maps -
   mn_tool_climmap%l_lab     = l_climmaps
   mn_tool_threshs%id  = 461 ! - Tools ~ Climatological Maps ~ Thresholds -
   mn_tool_threshs%l_lab     = l_threshs
   mn_tool_aves%id     = 462 ! - Tools ~ Climatological Maps ~ Averages -
   mn_tool_aves%l_lab        = l_averages
   mn_tool_corrs%id    = 463 ! - Tools ~ Climatological Maps ~ Correlations -
   mn_tool_corrs%l_lab       = l_correls
   mn_tool_stdevs%id   = 464 ! - Tools ~ Climatological Maps ~ Standard Deviations -
   mn_tool_stdevs%l_lab      = l_stdevs
   mn_tool_cvars%id    = 465 ! - Tools ~ Climatological Maps ~ Coefficients of Variation -
   mn_tool_cvars%l_lab       = l_cvars
   mn_tool_xmap%id     = 466 ! - Tools ~ Climatological Maps ~ Predictors (X) -
   mn_tool_xmap%l_lab        = l_predictorx
   mn_tool_ymap%id     = 467 ! - Tools ~ Climatological Maps ~ Predictands (Y) -
   mn_tool_ymap%l_lab        = l_predictandy
   mn_tool_zmap%id     = 468 ! - Tools ~ Climatological Maps ~ Predictors (Z) -
   mn_tool_zmap%l_lab        = l_predictorz
! - regression -
   mn_tool_regress%id  = 471 ! - Tools ~ Models ~ Regression -
   mn_tool_regress%l_lab     = l_regress
!
! Initialise grey menu items
! - Tools -
   mn_tool_zmap%ion = 0 ! - Tools ~ Climatological Maps -
!
   Return
  End Subroutine init_menus_tool
!
!
!
  Subroutine init_menus_opts ()
!
! Initialises Options menu identifiers
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm
   Use labels,             Only: ca_atypes_a, &
                                 l_beginner, l_ccaopts, l_climper, l_cvopts, l_data, l_drymask, l_ensave, l_exclude, l_expert,    &
                                 l_expertlev, l_fcastsets, l_gcmopts, l_goodness, l_lang, l_linkfs, l_masksets, l_missings,       &
                                 l_models, l_modeoptsx, l_modeoptsy, l_options, l_regropts, l_resamples, l_showopts, l_skillmask, &
                                 l_sortens, l_synchps, l_tailor, l_timesets, l_transforms, l_transformy, l_verifsets, l_zerob
#if GUI == 1
   Use labels,             Only: l_bw, l_catshade, l_config, l_console, l_crosses, l_fntscal, l_graphics, l_graphscal, l_maskland, &
                                 l_masklake, l_reversecols, l_scorgrphs, l_shadeland, l_shadesea, l_stnmrks, l_txtscal, l_vlines
#else
   Use labels,             Only: l_errhand, l_printmenus
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Trim
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_opts%id          = 500 ! - Options -
   mn_opts%l_lab             = l_options
   mn_opts_lang%id     = 511 ! - Options ~ Language-
   mn_opts_lang%l_lab        = l_lang
   mn_opts_show%id     = 512 ! - Options ~ Show Options -
   mn_opts_show%l_lab        = l_showopts
   mn_opts_elev%id     = 513 ! - Options ~ Expertise Level -
   mn_opts_elev%l_lab        = l_expertlev
   mn_opts_begin%id    = 5131 ! - Options ~ Expertise Level ~ Beginner -
   mn_opts_begin%l_lab       = l_beginner
   mn_opts_expert%id   = 5132 ! - Options ~ Expertise Level ~ Expert -
   mn_opts_expert%l_lab      = l_expert
#if GUI == 1
   mn_opts_config%id   = 514 ! - Options ~ Configuration -
   mn_opts_config%l_lab      = l_config
   mn_opts_txtscal%id  = 5141 ! - Options ~ Configuration ~ Text-Scaling -
   mn_opts_txtscal%l_lab     = l_txtscal
   mn_opts_console%id  = 5142 ! - Options ~ Configuration ~ Console -
   mn_opts_console%l_lab     = l_console
#endif
   mn_opts_xmodes%id   = 521 ! - Options ~ X Modes -
#if GUI == 1
   mn_opts_xmodes%l_lab      = l_modeoptsx
#else
   mn_opts_xmodes%l_lab%c    = 'Number of X modes'
   mn_opts_xmodes%l_lab%iak  = 11
   mn_opts_xmodeadv%id = 522 ! - Options ~ X Modes (Advanced) -
   mn_opts_xmodeadv%l_lab    = l_modeoptsx
#endif
   mn_opts_ymodes%id   = 523 ! - Options ~ Y Modes -
#if GUI == 1
   mn_opts_ymodes%l_lab      = l_modeoptsy
#else
   mn_opts_ymodes%l_lab%c    = 'Number of Y modes'
   mn_opts_ymodes%l_lab%iak  = 11
   mn_opts_ymodeadv%id = 524 ! - Options ~ Y Modes (Advanced) -
   mn_opts_ymodeadv%l_lab    = l_modeoptsy
#endif
   mn_opts_cca%id      = 525 ! - Options ~ CCA Options -
#if GUI == 1
   mn_opts_cca%l_lab         = l_ccaopts
#else
   mn_opts_cca%l_lab%c       = 'Number of CCA modes'
   mn_opts_cca%l_lab%iak     = 11
   mn_opts_ccaadv%id   = 526 ! - Options ~ CCA Modes (Advanced) -
   mn_opts_ccaadv%l_lab      = l_ccaopts
#endif
   mn_opts_gcm%id      = 527 ! - Options ~ GCM Options -
   mn_opts_gcm%l_lab         = l_gcmopts
   mn_opts_models%id   = 528 ! - Options ~ Models -
   mn_opts_models%l_lab      = l_models
   mn_opts_mdlcca%id   = 5281 ! - Options ~ Models ~ CCA -
   mn_opts_mdlcca%l_lab%c    = add_suffix(Trim(ls_analyses(ia_cca)%c), ca_atypes_a(ia_cca))
   mn_opts_mdlcca%l_lab%iak  = ls_analyses(ia_cca)%iak
   mn_opts_mdlpcr%id   = 5282 ! - Options ~ Models ~ PCR -
   mn_opts_mdlpcr%l_lab%c    = add_suffix(Trim(ls_analyses(ia_pcr)%c), ca_atypes_a(ia_pcr))
   mn_opts_mdlpcr%l_lab%iak  = ls_analyses(ia_pcr)%iak
   mn_opts_mdlmlr%id   = 5283 ! - Options ~ Models ~ MLR -
   mn_opts_mdlmlr%l_lab%c    = add_suffix(Trim(ls_analyses(ia_mlr)%c), ca_atypes_a(ia_mlr))
   mn_opts_mdlmlr%l_lab%iak  = ls_analyses(ia_mlr)%iak
   mn_opts_mdlgcm%id   = 5284 ! - Options ~ Models ~ GCM -
   mn_opts_mdlgcm%l_lab%c    = add_suffix(Trim(ls_analyses(ia_gcm)%c), ca_atypes_a(ia_gcm))
   mn_opts_mdlgcm%l_lab%iak  = ls_analyses(ia_gcm)%iak

   mn_opts_good%id     = 531 ! - Options ~ Goodness Index Options -
   mn_opts_good%l_lab        = l_goodness
   mn_opts_climper%id  = 532 ! - Options ~ Climatological Period -
   mn_opts_climper%l_lab     = l_climper
   mn_opts_tailor%id   = 533 ! - Options ~ Tailoring Options -
   mn_opts_tailor%l_lab      = l_tailor
   mn_opts_cv%id       = 534 ! - Options ~ Cross-Validation Options -
   mn_opts_cv%l_lab          = l_cvopts
   mn_opts_regr%id     = 535 ! - Options ~ Regression Options -
   mn_opts_regr%l_lab        = l_regropts
   mn_opts_link%id     = 536 ! - Options ~ Link-Function Options -
   mn_opts_link%l_lab        = l_linkfs
   mn_opts_mask%id     = 537 ! - Options ~ Masking Settings -
   mn_opts_mask%l_lab        = l_masksets
   mn_opts_maskdry%id  = 5371 ! - Options ~ Dry-Masking Settings -
   mn_opts_maskdry%l_lab     = l_drymask
   mn_opts_maskskll%id = 5372 ! - Options ~ Dry-Masking Settings -
   mn_opts_maskskll%l_lab    = l_skillmask
   mn_opts_data%id     = 540 ! - Options ~ Data -
   mn_opts_data%l_lab        = l_data
   mn_opts_trnsy%id    = 541 ! - Options ~ Data ~ Transform Y -
   mn_opts_trnsy%l_lab       = l_transformy
   mn_opts_zerob%id    = 542 ! - Options ~ Data ~ Zero-Bound -
   mn_opts_zerob%l_lab       = l_zerob
   mn_opts_sortens%id  = 543 ! - Options ~ Data ~ Sort Ensemble Members -
   mn_opts_sortens%l_lab     = l_sortens
   mn_opts_miss%id     = 544 ! - Options ~ Data ~ Missing Values -
   mn_opts_miss%l_lab        = l_missings
   mn_opts_missx%id    = 5441 ! - Options ~ Data ~ X Missing Values -
   mn_opts_missx%l_lab%c     = Trim(l_missings%c)//' (X)'
   mn_opts_missx%l_lab%iak   = Len_Trim(l_missings%c) - 1
   mn_opts_missy%id    = 5442 ! - Options ~ Data ~ Y Missing Values -
   mn_opts_missy%l_lab%c     = Trim(l_missings%c)//' (Y)'
   mn_opts_missy%l_lab%iak   = Len_Trim(l_missings%c) - 1
   mn_opts_synch%id    = 545 ! - Options ~ Data ~ Synchronous Predictors -
   mn_opts_synch%l_lab       = l_synchps
   mn_opts_aveens%id   = 546 ! - Options ~ Data ~ Average Ensemble Members -
   mn_opts_aveens%l_lab      = l_ensave
   mn_opts_exclude%id  = 547 ! - Options ~ Data ~ Exclude -
   mn_opts_exclude%l_lab     = l_exclude
   mn_opts_boot%id     = 551 ! - Options ~ Resampling Settings -
   mn_opts_boot%l_lab        = l_resamples
   mn_opts_fcast%id    = 552 ! - Options ~ Forecast Settings -
   mn_opts_fcast%l_lab       = l_fcastsets
   mn_opts_verif%id    = 553 ! - Options ~ Verification Settings -
   mn_opts_verif%l_lab       = l_verifsets
   mn_opts_trnss%id    = 554 ! - Options ~ Transformation Settings -
   mn_opts_trnss%l_lab       = l_transforms
   mn_opts_time%id     = 555 ! - Options ~ Time Settings -
   mn_opts_time%l_lab        = l_timesets
   mn_opts_pval%id     = 561 ! - Options ~ Skill Map P-Values -
   mn_opts_pval%l_lab%c      = 'Skill map p-values'
   mn_opts_pval%l_lab%iak    = 11
#if GUI == 0
   mn_opts_errors%id   = 571 ! - Options ~ Error Handling -
   mn_opts_errors%l_lab      = l_errhand
   mn_opts_menus%id    = 572 ! - Options ~ Print Menus -
   mn_opts_menus%l_lab       = l_printmenus
#else
   mn_opts_graph%id    = 580 ! - Options ~ Graphics -
   mn_opts_graph%l_lab       = l_graphics
   mn_opts_stnmrks%id  = 581 ! - Options ~ Graphics ~ Station Markers -
   mn_opts_stnmrks%l_lab     = l_stnmrks
   mn_opts_fntscal%id  = 582 ! - Options ~ Graphics ~ Font Scaling -
   mn_opts_fntscal%l_lab     = l_fntscal
   mn_opts_grphscal%id = 583 ! - Options ~ Graphics ~ Graphics Scaling -
   mn_opts_grphscal%l_lab    = l_graphscal
   mn_opts_cross%id    = 584 ! - Options ~ Graphics ~ Crosses -
   mn_opts_cross%l_lab       = l_crosses
   mn_opts_scorgrph%id = 585 ! - Options ~ Graphics ~ Scores on Graphs -
   mn_opts_scorgrph%l_lab    = l_scorgrphs
   mn_opts_vlines%id   = 586 ! - Options ~ Graphics ~ Vertical Lines -
   mn_opts_vlines%l_lab      = l_vlines
   mn_opts_bw%id       = 591 ! - Options ~ Graphics ~ Black and White -
   mn_opts_bw%l_lab          = l_bw
   mn_opts_revcols%id  = 592 ! - Options ~ Graphics ~ Reverse Colours -
   mn_opts_revcols%l_lab     = l_reversecols
   mn_opts_shadecat%id = 593 ! - Options ~ Graphics ~ Shade Categories -
   mn_opts_shadecat%l_lab    = l_catshade
   mn_opts_shadelnd%id = 5941 ! - Options ~ Graphics ~ Shade Land -
   mn_opts_shadelnd%l_lab    = l_shadeland
   mn_opts_shadesea%id = 5942 ! - Options ~ Graphics ~ Shade Sea -
   mn_opts_shadesea%l_lab    = l_shadesea
   mn_opts_masklak%id  = 5951 ! - Options ~ Graphics ~ Mask Lakes -
   mn_opts_masklak%l_lab     = l_masklake
   mn_opts_masklnd%id  = 5952 ! - Options ~ Graphics ~ Mask Land -
   mn_opts_masklnd%l_lab     = l_maskland
#endif
!
   Return
  End Subroutine init_menus_opts
!
!
!
  Subroutine init_menus_view ()
!
! Initialises View menu identifiers
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
   Use labels,             Only: ca_atypes_a, &
                                 l_diagnostics, l_fcast, l_verifn, l_view, &
                                 ls_analyses
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_view%id          = 600 ! - View -
   mn_view%l_lab             = l_view
   mn_view_cca%id      = 611 ! - View ~ CCA -
   mn_view_cca%l_lab%c       = add_suffix(Trim(ls_analyses(ia_cca)%c), ca_atypes_a(ia_cca))
   mn_view_cca%l_lab%iak     = ls_analyses(ia_cca)%iak
   mn_view_pcr%id      = 612 ! - View ~ PCR -
   mn_view_pcr%l_lab%c       = add_suffix(Trim(ls_analyses(ia_pcr)%c), ca_atypes_a(ia_pcr))
   mn_view_pcr%l_lab%iak     = ls_analyses(ia_pcr)%iak
   mn_view_mlr%id      = 613 ! - View ~ MLR -
   mn_view_mlr%l_lab%c       = add_suffix(Trim(ls_analyses(ia_mlr)%c), ca_atypes_a(ia_mlr))
   mn_view_mlr%l_lab%iak     = ls_analyses(ia_mlr)%iak
   mn_view_gcm%id      = 614 ! - View ~ GCM -
   mn_view_gcm%l_lab%c       = add_suffix(Trim(ls_analyses(ia_gcm)%c), ca_atypes_a(ia_gcm))
   mn_view_gcm%l_lab%iak     = ls_analyses(ia_gcm)%iak
   mn_view_pfv%id      = 621 ! - View ~ PFV -
   mn_view_pfv%l_lab%c       = add_suffix(Trim(ls_analyses(ia_pfv)%c), ca_atypes_a(ia_pfv))
   mn_view_pfv%l_lab%iak     = ls_analyses(ia_pfv)%iak
   mn_view_verifn%id   = 621 ! - View ~ Verification -
   mn_view_verifn%l_lab      = l_verifn
   mn_view_fcast%id    = 631 ! - View ~ Forecast -
   mn_view_fcast%l_lab       = l_fcast
   mn_view_diagnost%id = 632 ! - View ~ Diagnostics -
   mn_view_diagnost%l_lab    = l_diagnostics
!
   Return
  End Subroutine init_menus_view
!
!
!
 Subroutine init_menus_help ()
!
! Initialises Help menu identifiers
!
! Modules
   Use labels, Only: l_about, l_help
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_help%id          = 700 ! - Help -
   mn_help%l_lab             = l_help
   mn_help_about%id    = 711 ! - Help ~ About -
   mn_help_about%l_lab       = l_about
   mn_help_help%id     = 712 ! - Help ~ Help -
   mn_help_help%l_lab        = l_help
!
   Return
  End Subroutine init_menus_help
!
!
!
 Subroutine init_menus_data ()
!
! Initialises Data menu identifiers
!
! Modules
   Use labels, Only: l_data, l_download, l_merge, l_update
!
! Executable Statements
!
! Initialise menu identifiers and labels
   mn_data%id          = 800 ! - Data -
   mn_data%l_lab             = l_data
   mn_data_merge%id    = 801 ! - Data ~ Merge -
   mn_data_merge%l_lab       = l_merge
   mn_data_update%id   = 802 ! - Data ~ Update -
   mn_data_update%l_lab      = l_update
   mn_data_dload%id    = 811 ! - Data ~ Download -
   mn_data_dload%l_lab       = l_download
!
   Return
  End Subroutine init_menus_data
!
!
!
  Function add_suffix(c_lab, c_suf) &
           Result (lab)
!
! Adds a parenthetical suffix to a label
!
! Modules
   Use data_text, Only: llab6
#if GUI == 1
   Use data_text, Only: c_tab
#endif
!
! Function result
   Character(Len=llab6) :: lab
!
! Arguments
!
! Input scalars
   Character(Len=*) :: c_lab ! - label text -
   Character(Len=*) :: c_suf ! - suffix -
!
#if GUI == 0
! Functions and Subroutiness
!
! Intrinsic functions
   Intrinsic Len, Repeat
!
#endif
! Executable Statements
!
! Add suffix in parentheses
#if GUI == 1
   lab = c_lab//c_tab//'('//c_suf//')'
#else
   lab = c_lab//Repeat(' ', itab-Len(c_lab))//'('//c_suf//')'
#endif
!
   Return
  End Function add_suffix
 End Subroutine init_menus
!
!
!
 Subroutine reset_menus ( &
            lactreset)
!
! Resets activated menus
!
! Arguments
!
! Optional arguments
! - optional input scalars -
   Logical, Intent(In), Optional :: lactreset ! - actions reset? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Reset activated menus
   If (.not.Present(lactreset)) Then
      Call reset_menus_screen () ! - screen -
      Call reset_menus_file ()   ! - File -
      Call reset_menus_edit ()   ! - Edit -
      Call reset_menus_acts ()   ! - Actions -
      Call reset_menus_tool ()   ! - Tools -
      Call reset_menus_opts ()   ! - Options -
!      Call reset_menus_data ()   ! - Data -
   Else If (Present(lactreset)) Then
      If (lactreset) Then
         Call reset_menus_acts () ! - Actions -
         Call reset_menus_tool () ! - Tools -
      End If
   End If
!
   Return
!
 Contains
!
!
  Subroutine reset_menus_screen ()
!
! Resets activated Screen menus
!
! Executable Statements
!
! Reset activated Screen menus
#if GUI == 0
   mn_screen_openx%iact = 0 ! - 1: Open X file -
   mn_screen_openy%iact = 0 ! - 2: Open Y file -
   mn_screen_openz%iact = 0 ! - 3: Open forecast file -
#endif
   mn_screen_lentp%iact = 0 ! - 7: length of training period -
   mn_screen_nf%iact    = 0 ! - 9: number of forecasts -
!
   Return
  End Subroutine reset_menus_screen
!
!
!
  Subroutine reset_menus_file ()
!
! Resets activated File menus
!
! Executable Statements
!
! Reset activated File menus
   mn_file_datout%iact = 0 ! - 111: File ~ Output Results ~ Output Results -
   mn_file_savegi%iact = 0 ! - 112: File ~ Output Results ~ Save Goodness Index -
   mn_file_format%iact = 0 ! - 131: Output-file format -
   mn_file_prec%iact   = 0 ! - 132: Output-data precision -
#if GUI == 0
   mn_file_tags%iact   = 0 ! - 133: CPT output-tags -
#endif
   mn_file_open%iact   = 0 ! - 142: Open project -
!
   Return
  End Subroutine reset_menus_file
!
!
!
  Subroutine reset_menus_edit ()
!
! Resets activated Edit menus
!
! Executable Statements
!
! Reset activated Edit menus
   mn_edit_startatx%iact = 0 ! - 221: Edit ~ Start at (x) -
   mn_edit_startaty%iact = 0 ! - 222: Edit ~ Start at (Y) -
   mn_edit_startatz%iact = 0 ! - 223: Edit ~ Start at (Z) -
   mn_edit_tssn%iact     = 0 ! - 231: Edit ~ Target Season -
!
   Return
  End Subroutine reset_menus_edit
!
!
!
  Subroutine reset_menus_acts ()
!
! Resets activated Actions menus
!
! Executable Statements
!
! Reset activated Actions menus
   mn_acts_cv%iact     = 0 ! - 311: Actions ~ Calculate ~ Cross-validate -
   mn_acts_retro%iact  = 0 ! - 312: Actions ~ Calculate ~ Retro-active -
   mn_acts_verify%iact = 0 ! - 313: Actions ~ Calculate ~ Verify -
   mn_acts_cv2%iact    = 0 ! - 314: Actions ~ Calculate ~ Double cross-validate -
!
   Return
  End Subroutine reset_menus_acts
!
!
!
  Subroutine reset_menus_tool ()
!
! Resets activated Tools menus
!
! Executable Statements
!
! Reset activated Tools menus
   mn_tool_cvperf%iact  = 0 ! - 411: Tools ~ Validation ~ Cross-Validation ~ Performance Measures -
   mn_tool_cvboot%iact  = 0 ! - 412: Tools ~ Validation ~ Cross-Validation ~ Bootstrap -
   mn_tool_cvmaps%iact  = 0 ! - 413: Tools ~ Validation ~ Cross-Validation ~ Skill Maps -
#if GUI == 0
   mn_tool_cvroc%iact   = 0 ! - 414: Tools ~ Validation ~ Cross-Validation ~ ROC Diagrams -
#endif
   mn_tool_cvtable%iact = 0 ! - 415: Tools ~ Validation ~ Cross-Validation ~ Contingency Tables -
#if GUI == 1
   mn_tool_cvscat%iact  = 0 ! - 416: Tools ~ Validation ~ Cross-Validation ~ Scatter Diagrams -
#endif
   mn_tool_c2perf%iact  = 0 ! - 421: Tools ~ Validation ~ Double Cross-Validation ~ Performance Measures -
   mn_tool_c2boot%iact  = 0 ! - 422: Tools ~ Validation ~ Double Cross-Validation ~ Bootstrap -
   mn_tool_c2maps%iact  = 0 ! - 423: Tools ~ Validation ~ Double Cross-Validation ~ Skill Maps -
#if GUI == 0
   mn_tool_c2roc%iact   = 0 ! - 424: Tools ~ Validation ~ Double Cross-Validation ~ ROC Diagrams -
#endif
   mn_tool_c2table%iact = 0 ! - 425: Tools ~ Validation ~ Double Cross-Validation ~ Contingency Tables -
#if GUI == 1
   mn_tool_c2scat%iact  = 0 ! - 426: Tools ~ Validation ~ Double Cross-Validation ~ Scatter Diagrams -
#endif
   mn_tool_raperf%iact  = 0 ! - 421: Tools ~ Validation ~ Retroactive ~ Performance Measures -
   mn_tool_raboot%iact  = 0 ! - 422: Tools ~ Validation ~ Retroactive ~ Bootstrap -
   mn_tool_ramaps%iact  = 0 ! - 423: Tools ~ Validation ~ Retroactive ~ Skill Maps -
#if GUI == 0
   mn_tool_raroc%iact   = 0 ! - 424: Tools ~ Validation ~ Retroactive ~ ROC Diagrams -
#endif
   mn_tool_ratable%iact = 0 ! - 425: Tools ~ Validation ~ Retroactive ~ Contingency Tables -
#if GUI == 1
   mn_tool_rascat%iact  = 0 ! - 426: Tools ~ Validation ~ Retroactive ~ Scatter Diagrams -
#endif
   mn_tool_attribs%iact = 0 ! - 431: Tools ~ Verification ~ Attributes Diagram -
   mn_tool_vroc%iact    = 0 ! - 432: Tools ~ Verification ~ ROC Diagram -
   mn_tool_pscores%iact = 0 ! - 433: Tools ~ Verification ~ Probabilistic Scores -
   mn_tool_wrlt%iact    = 0 ! - 434: Tools ~ Verification ~ Weather Roulette -
   mn_tool_tend%iact    = 0 ! - 435: Tools ~ Verification ~ Tendency Diagram -
   mn_tool_rhits%iact   = 0 ! - 436: Tools ~ Verification ~ Ranked Hits Diagram -
   mn_tool_vskill%iact  = 0 ! - 437: Tools ~ Verification ~ Skill Maps -
   mn_tool_screes%iact  = 0 ! - 441: Tools ~ Modes ~ Scree Plots -
#if GUI == 1
   mn_tool_xeof%iact    = 0 ! - 442: Tools ~ Modes ~ X EOFs -
   mn_tool_yeof%iact    = 0 ! - 443: Tools ~ Modes ~ Y EOFs -
   mn_tool_cca%iact     = 0 ! - 444: Tools ~ Modes ~ CCA -
#endif
   mn_tool_fser%iact    = 0 ! - 451: Tools ~ Forecasts ~ Series -
   mn_tool_fens%iact    = 0 ! - 452: Tools ~ Forecasts ~ Ensembles -
   mn_tool_pexc%iact    = 0 ! - 453: Tools ~ Forecasts ~ Exceedance Probabilities -
   mn_tool_fval%iact    = 0 ! - 454: Tools ~ Forecasts ~ Values -
   mn_tool_fps%iact     = 0 ! - 455: Tools ~ Forecasts ~ Probabilities -
   mn_tool_odds%iact    = 0 ! - 456: Tools ~ Forecasts ~ Odds -
#if GUI == 1
   mn_tool_hps%iact     = 0 ! - 457: Tools ~ Forecasts ~ Probabilistic -
#endif
   mn_tool_threshs%iact = 0 ! - 461: Tools ~ Climatological Maps ~ Thresholds -
   mn_tool_aves%iact    = 0 ! - 462: Tools ~ Climatological Maps ~ Averages -
   mn_tool_corrs%iact   = 0 ! - 463: Tools ~ Climatological Maps ~ Correlations -
   mn_tool_stdevs%iact  = 0 ! - 464: Tools ~ Climatological Maps ~ Standard Deviations -
   mn_tool_cvars%iact   = 0 ! - 465: Tools ~ Climatological Maps ~ Coefficients of Variation -
   mn_tool_xmap%iact    = 0 ! - 466: Tools ~ Climatological Maps ~ Predictors (X) -
   mn_tool_ymap%iact    = 0 ! - 467: Tools ~ Climatological Maps ~ Predictands (Y) -
   mn_tool_zmap%iact    = 0 ! - 468: Tools ~ Climatological Maps ~ Predictors (Z) -
   mn_tool_regress%iact = 0 ! - 471: Tools ~ Regression -
!
   Return
  End Subroutine reset_menus_tool
!
!
!
  Subroutine reset_menus_opts ()
!
! Resets activated Options menus
!
! Executable Statements
!
! Reset activated Options menus
   mn_opts_show%iact     = 0 ! - 512: Options ~ Show Options -
#if GUI == 1
   mn_opts_config%iact   = 0 ! - 514: Options ~ Configuration -
   mn_opts_txtscal%iact  = 0 ! - 5141: Options ~ Configuration ~ Text-Scaling -
   mn_opts_console%iact  = 0 ! - 5142: Options ~ Configuration ~ Console -
#endif
   mn_opts_xmodes%iact   = 0 ! - 521: Options ~ X Modes -
#if GUI == 0
   mn_opts_xmodeadv%iact = 0 ! - 522: Options ~ X Modes (Advanced) -
#endif
   mn_opts_ymodes%iact   = 0 ! - 523: Options ~ Y Modes -
#if GUI == 0
   mn_opts_ymodeadv%iact = 0 ! - 524: Options ~ Y Modes (Advanced) -
#endif
   mn_opts_cca%iact      = 0 ! - 525: Options ~ CCA Options -
#if GUI == 0
   mn_opts_ccaadv%iact   = 0 ! - 526: Options ~ CCA Modes (Advanced) -
#endif
   mn_opts_gcm%iact      = 0 ! - 527: Options ~ GCM Options -
   mn_opts_models%iact   = 0 ! - 528: Options ~ Models -
   mn_opts_mdlcca%iact   = 0 ! - 5281: Options ~ Models ~ CCA -
   mn_opts_mdlpcr%iact   = 0 ! - 5282: Options ~ Models ~ PCR -
   mn_opts_mdlmlr%iact   = 0 ! - 5283: Options ~ Models ~ MLR -
   mn_opts_mdlgcm%iact   = 0 ! - 5284: Options ~ Models ~ GCM -

   mn_opts_good%iact     = 0 ! - 531: Options ~ Goodness Index Options -
   mn_opts_climper%iact  = 0 ! - 532: Options ~ Climatological Period -
   mn_opts_tailor%iact   = 0 ! - 533: Options ~ Tailoring Options -
   mn_opts_cv%iact       = 0 ! - 534: Options ~ Cross-Validation Options -
   mn_opts_regr%iact     = 0 ! - 535: Options ~ Regression Options -
   mn_opts_link%iact     = 0 ! - 536: Options ~ Link-Function Options -
   mn_opts_mask%iact     = 0 ! - 537: Options ~ Masking Settings -
#if GUI == 0
   mn_opts_maskdry%iact  = 0 ! - 5371: Options ~ Dry-Masking Settings -
   mn_opts_maskskll%iact = 0 ! - 5372: Options ~ Dry-Masking Settings -
#endif
   mn_opts_trnsy%iact    = 0 ! - 541: Options ~ Data ~ Transform Y -
   mn_opts_zerob%iact    = 0 ! - 542: Options ~ Data ~ Zero-Bound -
   mn_opts_sortens%iact  = 0 ! - 543: Options ~ Data ~ Sort Ensemble Members -
   mn_opts_miss%iact     = 0 ! - 544: Options ~ Data ~ Missing Values -
   mn_opts_missx%iact    = 0 ! - 5441: Options ~ Data ~ X Missing Values -
   mn_opts_missy%iact    = 0 ! - 5442: Options ~ Data ~ Y Missing Values -
   mn_opts_synch%iact    = 0 ! - 545: Options ~ Data ~ Synchronous Predictors -
   mn_opts_aveens%iact   = 0 ! - 546: Options ~ Data ~ Average Ensemble Members -
   mn_opts_exclude%iact  = 0 ! - 547: Options ~ Data ~ Exclude -
   mn_opts_boot%iact     = 0 ! - 551: Options ~ Resampling Settings -
   mn_opts_fcast%iact    = 0 ! - 552: Options ~ Forecast Settings -
   mn_opts_verif%iact    = 0 ! - 553: Options ~ Verification Settings -
   mn_opts_trnss%iact    = 0 ! - 554: Options ~ Transformation Settings -
   mn_opts_time%iact     = 0 ! - 555: Options ~ Time Settings -
   mn_opts_pval%iact     = 0 ! - 561: Options ~ Skill Map P-Values -
#if GUI == 0
   mn_opts_errors%iact   = 0 ! - 571: Options ~ Error Handling -
   mn_opts_menus%iact    = 0 ! - 572: Options ~ Print Menus -
#else
   mn_opts_stnmrks%iact  = 0 ! - 581: Options ~ Graphics ~ Station Markers -
   mn_opts_fntscal%iact  = 0 ! - 582: Options ~ Graphics ~ Font Scaling -
   mn_opts_grphscal%iact = 0 ! - 583: Options ~ Graphics ~ Graphics Scaling -
   mn_opts_cross%iact    = 0 ! - 584: Options ~ Graphics ~ Crosses -
   mn_opts_scorgrph%iact = 0 ! - 585: Options ~ Graphics ~ Scores on Graphs -
   mn_opts_vlines%iact   = 0 ! - 586: Options ~ Graphics ~ Vertical Lines -
   mn_opts_bw%iact       = 0 ! - 591: Options ~ Graphics ~ Black and White -
   mn_opts_revcols%iact  = 0 ! - 592: Options ~ Graphics ~ Reverse Colours -
   mn_opts_shadecat%iact = 0 ! - 593: Options ~ Graphics ~ Shade Categories -
   mn_opts_shadelnd%iact = 0 ! - 5941: Options ~ Graphics ~ Shade Land -
   mn_opts_shadesea%iact = 0 ! - 5942: Options ~ Graphics ~ Shade Sea -
   mn_opts_masklak%iact  = 0 ! - 5951: Options ~ Graphics ~ Mask Lakes -
   mn_opts_masklnd%iact  = 0 ! - 5952: Options ~ Graphics ~ Mask Land -
#endif
!
   Return
  End Subroutine reset_menus_opts
 End Subroutine reset_menus
!
!
!
 Subroutine add_menu_menu (cmn, lvmenu, mn_menu, &
            cbf, scbf, acc_key, lsep, lgrey, icheck, itoggle, ival1, iset1, ival2, iset2, ival3, iset3)
!
! Adds a menu item
!
! Modules
   Use labels, Only: c_lab, &
                     make_label
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lvmenu ! - level of menu -
!
   Character(Len=*), Intent(In) :: cmn ! - menu type -
!
! - input/output scalars -
   Type(menu), Intent(InOut) :: mn_menu ! - menu -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ival1 ! - value to set to iset1 -
   Integer, Intent(In), Optional :: ival2 ! - additional value to set to iset2 -
   Integer, Intent(In), Optional :: ival3 ! - additional value to set to iset3 -
!
   Character(Len=*), Intent(In), Optional :: acc_key ! - accelerator key -
   Character(Len=*), Intent(In), Optional :: scbf    ! - Silverfrost call-back function -
!
   Logical, Intent(In), Optional :: lgrey ! - allow greyed menu? -
   Logical, Intent(In), Optional :: lsep  ! - include separator? -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: icheck  ! - check mark -
   Integer, Intent(InOut), Optional :: itoggle ! - value to toggle -
   Integer, Intent(InOut), Optional :: iset1   ! - variable to set -
   Integer, Intent(InOut), Optional :: iset2   ! - additional variable to set -
   Integer, Intent(InOut), Optional :: iset3   ! - additional variable to set -
!
! - optional procedure arguments -
   Integer, External, Optional :: cbf ! - call-back function -
!
! Locals
!
! Local scalars
   Character(Len=1) :: cmnfmt ! - menu format -
!
   Logical :: llak ! - include accelerator key? -
   Logical :: lgry ! - include grey? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Construct label
#if GUI == 1
   cmnfmt = 'T'
   llak = .true.
#else
   Select Case (lvmenu)
    Case (1)
      cmnfmt = 'U'
    Case Default
      cmnfmt = 'T'
   End Select
   llak = .false.
#endif
   c_lab = make_label(cmnfmt, mn_menu%l_lab, &
           lak=llak)
!
! Determine whether to add greying (no need to check for iact because it will not be used if there are no actions)
   lgry = Present(lgrey)
   If (lgry) lgry = lgrey
!
! Add menu item
   If (lgry) Then
      Call add_menu_txt (cmn, lvmenu, mn_menu%id, c_lab, &
           lsep=lsep, iact=mn_menu%iact, igrey=mn_menu%ion, icheck=icheck, itoggle=itoggle, &
           iset1=iset1, ival1=ival1, iset2=iset2, ival2=ival2, iset3=iset3, ival3=ival3, &
           scbf=scbf, cbf=cbf, acc_key=acc_key)
   Else
      Call add_menu_txt (cmn, lvmenu, mn_menu%id, c_lab, &
           lsep=lsep, iact=mn_menu%iact, icheck=icheck, itoggle=itoggle, &
           iset1=iset1, ival1=ival1, iset2=iset2, ival2=ival2, iset3=iset3, ival3=ival3, &
           scbf=scbf, cbf=cbf, acc_key=acc_key)
   End If
!
   Return
 End Subroutine add_menu_menu
!
!
!
 Subroutine add_menu_lab (cmn, lvmenu, imenu, lab, &
            cbf, scbf, acc_key, lsep, igrey, icheck, itoggle, ival1, iset1, ival2, iset2, ival3, iset3)
!
! Adds a menu item
!
! Modules
   Use labels, Only: label, &
                     c_lab, &
                     make_label
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imenu  ! - menu identifier -
   Integer, Intent(In) :: lvmenu ! - level of menu -
!
   Character(Len=*), Intent(In) :: cmn ! - menu type -
!
   Type(label), Intent(In) :: lab ! - label -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: igrey ! - toggle for greyed menu -
   Integer, Intent(In), Optional :: ival1 ! - value to set to iset1 -
   Integer, Intent(In), Optional :: ival2 ! - additional value to set to iset2 -
   Integer, Intent(In), Optional :: ival3 ! - additional value to set to iset3 -
!
   Character(Len=*), Intent(In), Optional :: acc_key ! - accelerator key -
   Character(Len=*), Intent(In), Optional :: scbf    ! - Silverfrost call-back function -
!
   Logical, Intent(In), Optional :: lsep ! - include separator? -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: icheck  ! - check mark -
   Integer, Intent(InOut), Optional :: itoggle ! - value to toggle -
   Integer, Intent(InOut), Optional :: iset1   ! - variable to set -
   Integer, Intent(InOut), Optional :: iset2   ! - additional variable to set -
   Integer, Intent(InOut), Optional :: iset3   ! - additional variable to set -
!
! - optional procedure arguments -
   Integer, External, Optional :: cbf ! - call-back function -
!
! Locals
!
! Local scalars
   Character(Len=1) :: cmnfmt ! - menu format -
!
   Logical :: llak ! - include accelerator key? -
!
! Executable Statements
!
! Construct label
#if GUI == 1
   cmnfmt = 'T'
   llak = .true.
#else
   Select Case (lvmenu)
    Case (1)
      cmnfmt = 'U'
    Case Default
      cmnfmt = 'T'
   End Select
   llak = .false.
#endif
   c_lab = make_label(cmnfmt, lab, &
           lak=llak)
!
! Add menu item
   Call add_menu_txt (cmn, lvmenu, imenu, c_lab, &
        lsep=lsep, igrey=igrey, icheck=icheck, itoggle=itoggle, &
        iset1=iset1, ival1=ival1, iset2=iset2, ival2=ival2, iset3=iset3, ival3=ival3, &
        scbf=scbf, cbf=cbf, acc_key=acc_key)
!
   Return
 End Subroutine add_menu_lab
!
!
!
 Subroutine add_menu_txt1 (cmn, lmenu, imenu, cmenu, &
            lsep, igrey, icheck, itoggle, iset1, ival1, iset2, ival2, iset3, ival3, scbf, cbf, acc_key)
!
! Adds a menu item
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imenu ! - menu identifier -
   Integer, Intent(In) :: lmenu ! - level of menu -
!
   Character(Len=*), Intent(In) :: cmn   ! - menu type -
!
   Character(Len=*), Intent(In) :: cmenu ! - menu label -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: igrey ! - toggle for greyed menu -
   Integer, Intent(In), Optional :: ival1 ! - value to set to iset1 -
   Integer, Intent(In), Optional :: ival2 ! - additional value to set to iset2 -
   Integer, Intent(In), Optional :: ival3 ! - additional value to set to iset3 -
!
   Character(Len=*), Intent(In), Optional :: acc_key ! - accelerator key -
   Character(Len=*), Intent(In), Optional :: scbf    ! - Silverfrost call-back function -
!
   Logical, Intent(In), Optional :: lsep ! - include separator? -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: icheck  ! - check mark -
   Integer, Intent(InOut), Optional :: itoggle ! - value to toggle -
   Integer, Intent(InOut), Optional :: iset1   ! - variable to set -
   Integer, Intent(InOut), Optional :: iset2   ! - additional variable to set -
   Integer, Intent(InOut), Optional :: iset3   ! - additional variable to set -
!
! - optional procedure arguments -
   Integer, External, Optional :: cbf ! - call-back function -
!
! Executable Statements
!
! Add menu item
   Call add_menu_txt (cmn, lmenu, imenu, cmenu, &
        lsep=lsep, igrey=igrey, icheck=icheck, itoggle=itoggle, &
        iset1=iset1, ival1=ival1, iset2=iset2, ival2=ival2, iset3=iset3, ival3=ival3, &
        scbf=scbf, cbf=cbf, acc_key=acc_key)
!
   Return
 End Subroutine add_menu_txt1
#if FTN95 == 0 && GFORTRAN == 0
!
! This implementation simply converts Character(Kind=3) to Character(Kind=1) for GUI == 1
!
 Subroutine add_menu_txt3 (cmn, lmenu, imenu, cmenu, &
            lsep, igrey, icheck, itoggle, iset1, ival1, iset2, ival2, iset3, ival3, scbf, cbf, acc_key)
!
! Adds a menu item
!
! Modules
   Use data_text, Only: i_cyrillic
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imenu ! - menu identifier -
   Integer, Intent(In) :: lmenu ! - level of menu -
!
   Character(Len=*), Intent(In) :: cmn   ! - menu type -
!
   Character(Len=*, Kind=i_cyrillic), Intent(In) :: cmenu ! - menu label -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: igrey ! - toggle for greyed menu -
   Integer, Intent(In), Optional :: ival1 ! - value to set to iset1 -
   Integer, Intent(In), Optional :: ival2 ! - additional value to set to iset2 -
   Integer, Intent(In), Optional :: ival3 ! - additional value to set to iset3 -
!
   Character(Len=*), Intent(In), Optional :: acc_key ! - accelerator key -
   Character(Len=*), Intent(In), Optional :: scbf    ! - Silverfrost call-back function -
!
   Logical, Intent(In), Optional :: lsep ! - include separator? -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: icheck  ! - check mark -
   Integer, Intent(InOut), Optional :: itoggle ! - value to toggle -
   Integer, Intent(InOut), Optional :: iset1   ! - variable to set -
   Integer, Intent(InOut), Optional :: iset2   ! - additional variable to set -
   Integer, Intent(InOut), Optional :: iset3   ! - additional variable to set -
!
! - optional procedure arguments -
   Integer, External, Optional :: cbf ! - call-back function -
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
   Character(Len=128) :: c1menu ! - menu label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Char, Ichar, Len_Trim
!
! Executable Statements
!
! Add menu item
   Do i = 1, Len_Trim(cmenu)
      c1menu(i:i) = Char(Ichar(cmenu(i:i)))
   End Do
   Call add_menu_txt (cmn, lmenu, imenu, c1menu, &
        lsep=lsep, igrey=igrey, icheck=icheck, itoggle=itoggle, &
        iset1=iset1, ival1=ival1, iset2=iset2, ival2=ival2, iset3=iset3, ival3=ival3, &
        scbf=scbf, cbf=cbf, acc_key=acc_key)
!
   Return
 End Subroutine add_menu_txt3
#endif
!
!
!
 Subroutine add_menu_txt (cmn, lvmenu, imenu, cmenu, &
            cbf, scbf, acc_key, lsep, iact, icheck, igrey, itoggle, ival1, iset1, ival2, iset2, ival3, iset3)
!
! Adds a menu item
!
! Modules
#if GUI == 1
   Use clrwin$,   Only: winio$
   Use data_text, Only: c_tab
   Use gui,       Only: iw
#else
   Use labels,    Only: cg_turnoff_l, cg_turnon_l
   Use gui,       Only: print_text
#endif
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imenu  ! - menu identifier -
   Integer, Intent(In) :: lvmenu ! - level of menu -
!
   Character(Len=*), Intent(In) :: cmn   ! - menu type -
   Character(Len=*), Intent(In) :: cmenu ! - menu label -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: igrey ! - toggle for greyed menu -
   Integer, Intent(In), Optional :: ival1 ! - value to set to iset1 -
   Integer, Intent(In), Optional :: ival2 ! - additional value to set to iset2 -
   Integer, Intent(In), Optional :: ival3 ! - additional value to set to iset3 -
!
   Character(Len=*), Intent(In), Optional :: acc_key ! - accelerator key -
   Character(Len=*), Intent(In), Optional :: scbf    ! - Silverfrost call-back function -
!
   Logical, Intent(In), Optional :: lsep ! - include separator? -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: iact    ! - menu activation counter -
   Integer, Intent(InOut), Optional :: icheck  ! - check mark -
   Integer, Intent(InOut), Optional :: itoggle ! - value to toggle -
   Integer, Intent(InOut), Optional :: iset1   ! - variable to set -
   Integer, Intent(InOut), Optional :: iset2   ! - additional variable to set -
   Integer, Intent(InOut), Optional :: iset3   ! - additional variable to set -
!
! - optional procedure arguments -
   Integer, External, Optional :: cbf ! - call-back function -
!
! Locals
!
! Local scalars
#if GUI == 0
   Character(Len=  8) :: cmnum ! - menu number -
#else
   Character(Len=256) :: cfmt  ! - menu format text -
!
   Logical :: lact ! - monitor whether menu was activated? -
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
#if GUI == 1
   Intrinsic Repeat
#else
   Intrinsic All
#endif
!
! Executable Statements
!
! Construct menu item
#if GUI == 1
   cfmt = '%'//cmn//Repeat('[', lvmenu)                  ! - open levels -
   If (Present(lsep)) Then
      If (lsep) cfmt = Trim(cfmt)//'|, '                 ! - add separator -
   End If
   If (Present(icheck)) Then
      cfmt = Trim(cfmt)//'#'                             ! - add check mark -
   End If
   If (Present(igrey)) Then
      cfmt = Trim(cfmt)//'~'                             ! - add greying -
   End If
   cfmt = Trim(cfmt)//Trim(cmenu)                        ! - add menu item -
   If (Present(acc_key)) Then
      cfmt = Trim(cfmt)//c_tab//'('//Trim(acc_key)//')'  ! - add accelerator key indicator -
   End If
   If (.not.(Present(cbf) .or. Present(scbf) .or. Present(icheck) .or. Present(itoggle) .or. &
            (Present(iset1) .and. Present(ival1)))) Then
      cfmt = Trim(cfmt)//'[]'                            ! - add empty level if there is no callback -
      lact = .false.
   Else
      lact = Present(iact)
   End If
   cfmt = Trim(cfmt)//Repeat(']', lvmenu)//'&'           ! - close levels -
!
! Add menu item
   If (lact) Then
      If (Present(icheck)) Then
         If (Present(igrey)) Then
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, record_change)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', 'INC', iact, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', 'INC', iact, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', 'INC', iact, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', 'INC', iact, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   'INC', iact)
                           End If
                        End If
               End If
            End If
         Else ! - no grey -
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, & 
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', 'INC', iact, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', 'INC', iact, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', 'INC', iact, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', 'INC', iact, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   'INC', iact, &
                                   icheck)
                           End If
                        End If
               End If
            End If
         End If
      Else ! - no check -
         If (Present(igrey)) Then
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', 'INC', iact, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', 'INC', iact, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', 'INC', iact, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', 'INC', iact, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   'INC', iact)
                           End If
                        End If
               End If
            End If
         Else ! - no grey -
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', 'INC', iact, &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', 'INC', iact, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', 'INC', iact, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', 'INC', iact, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', 'INC', iact, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', 'INC', iact, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   'INC', iact)
                           End If
                        End If
               End If
            End If
         End If
      End If
   Else ! - no activation check -
      If (Present(icheck)) Then
         If (Present(igrey)) Then
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   '+', &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, igrey)
                           End If
                        End If
               End If
            End If
         Else ! - no grey -
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, & 
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   '+', &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   icheck, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   icheck)
                           End If
                        End If
               End If
            End If
         End If
      Else ! - no check -
         If (Present(igrey)) Then
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   '+', &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   igrey, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   igrey)
                           End If
                        End If
               End If
            End If
         Else ! - no grey -
            If (Present(itoggle)) Then
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'TOGGLE', itoggle, record_change, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', &
                                   'TOGGLE', itoggle, record_change)
                           End If
                        End If
               End If
            Else ! - no toggle -
               If (Present(iset1) .and. Present(ival1)) Then
                  If (Present(iset2) .and. Present(ival2)) Then
                     If (Present(iset3) .and. Present(ival3)) Then
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   'SET', iset3, ival3)
                           End If
                        End If
                     Else ! - no iset3 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   'SET', iset2, ival2)
                           End If
                        End If
                     End If
                  Else ! - no iset2 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', '+', &
                                   'SET', iset1, ival1, &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', &
                                   'SET', iset1, ival1, &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   'SET', iset1, ival1)
                           End If
                        End If
                  End If
               Else ! - no iset1 -
                        If (Present(cbf)) Then
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   '+', &
                                   cbf, scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt), &
                                   cbf)
                           End If
                        Else ! - no user callback -
                           If (Present(scbf)) Then
                              iw = winio$(Trim(cfmt), &
                                   scbf)
                           Else ! - no Silverfrost callback -
                              iw = winio$(Trim(cfmt))
                           End If
                        End If
               End If
            End If
         End If
      End If
   End If
!
! Set accelerator key
   If (Present(acc_key)) Then
      If (lact) Then
         If (Present(iset1) .and. Present(ival1)) Then
            If (Present(iset2) .and. Present(ival2)) Then
               If (Present(iset3) .and. Present(ival3)) Then
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then  ! - no user callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3, &
                             scbf)
                     Else
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3)
                     End If
                  End If
               Else ! - no iset3 -
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then  ! - no user callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             scbf)
                     Else
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2)
                     End If
                  End If
               End If
            Else ! - no iset2 -
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then  ! - no user callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', 'INC', iact, &
                             'SET', iset1, ival1, &
                             scbf)
                     Else
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', 'INC', iact, &
                             'SET', iset1, ival1)
                     End If
                  End If
            End If
         Else ! - no iset1 -
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', 'INC', iact, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', 'INC', iact, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', 'INC', iact, &
                             scbf)
                     End If
                  End If
         End If
      Else ! - no activation coutner -
         If (Present(iset1) .and. Present(ival1)) Then
            If (Present(iset2) .and. Present(ival2)) Then
               If (Present(iset3) .and. Present(ival3)) Then
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then  ! - no user callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3, &
                             scbf)
                     Else
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             'SET', iset3, ival3)
                     End If
                  End If
               Else ! - no iset3 -
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then  ! - no user callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2, &
                             scbf)
                     Else
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', &
                             'SET', iset1, ival1, &
                             'SET', iset2, ival2)
                     End If
                  End If
               End If
            Else ! - no iset2 -
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', '+', &
                             'SET', iset1, ival1, &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', &
                             'SET', iset1, ival1, &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then  ! - no user callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', &
                             'SET', iset1, ival1, &
                             scbf)
                     Else
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             'SET', iset1, ival1)
                     End If
                  End If
            End If
         Else ! - no iset1 -
                  If (Present(cbf)) Then
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             '+', &
                             scbf, cbf)
                     Else ! - no Silverfrost callback -
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             cbf)
                     End If
                  Else ! - no user callback -
                     If (Present(scbf)) Then
                        iw = winio$('%ac['//Trim(acc_key)//']&', &
                             scbf)
                     End If
                  End If
         End If
      End If
   End If
  
#else
! Set greying
   If (Present(igrey)) Then
      lgreys(lvmenu) = (igrey > 0)
   Else
      lgreys(lvmenu) = .true.
   End If
!
! Print menu
   If (All(lgreys(1:lvmenu))) Then
     Select Case (lvmenu)
      Case (1)
        Call print_text (Trim(cmenu))
      Case Default
         Write (Unit=cmnum, Fmt='(I5,A)') imenu,'.  '
         If (Present(icheck)) Then
            If (icheck == 0) Then
               Call print_text (cmnum//Trim(cmenu)//' ('//Trim(cg_turnon_l)//')', &
                    itab=5*(lvmenu-1))
            Else
               Call print_text (cmnum//Trim(cmenu)//' ('//Trim(cg_turnoff_l)//')', &
                    itab=5*(lvmenu-1))
            End If
         Else
            Call print_text (cmnum//Trim(cmenu), &
                 itab=5*(lvmenu-1))
         End If
      End Select
   End If
#endif
!
   Return
 End Subroutine add_menu_txt
!
!
!
 Subroutine set_menu_greying ( &
            ianal, icalc, icca, icsv, icv2, idia, idstrx, idstry, idstrz, ifc, ifcast, ifor, igcm, igeog, igeogx, igeogy, ihind, &
            imlr, ipcr, ipfv, iregr, iretro, isave, istd, izero, &
            lclim, ldone, leofx, leofy, lexpert, lmodes, lnofiles, lopen, lscree, ltitle, lxyopen)
!
! Sets grey menu flags
!
! Modules
   Use data_cpt_constants, Only: ia_pcr, ia_mlr, ia_gcm, ia_pfv
!
! Arguments
!
! Optional arguments
! - input scalars -
   Integer, Intent(In), Optional :: ianal  ! - analysis -
   Integer, Intent(In), Optional :: icalc  ! - calculation -
   Integer, Intent(In), Optional :: icca   ! - CCA flag - 
   Integer, Intent(In), Optional :: icsv   ! - calculate seasonal values flag - 
   Integer, Intent(In), Optional :: icv2   ! - double cross-validation flag - 
   Integer, Intent(In), Optional :: idia   ! - (beginners) diagnostics flag -
   Integer, Intent(In), Optional :: idstrx ! - X-file structure -
   Integer, Intent(In), Optional :: idstry ! - Y-file structure -
   Integer, Intent(In), Optional :: idstrz ! - forecast-file structure -
   Integer, Intent(In), Optional :: ifc    ! - forecast calculation flag -
   Integer, Intent(In), Optional :: ifcast ! - operational forecast flag -
   Integer, Intent(In), Optional :: igcm   ! - GCM flag -
   Integer, Intent(In), Optional :: igeog  ! - spatial domain flag -
   Integer, Intent(In), Optional :: igeogx ! - X-file spatial domain flag -
   Integer, Intent(In), Optional :: igeogy ! - Y-file spatial domain flag -
   Integer, Intent(In), Optional :: ifor   ! - (beginners) forecast flag -
   Integer, Intent(In), Optional :: ihind  ! - hindcasts -
   Integer, Intent(In), Optional :: imlr   ! - MLr flag -
   Integer, Intent(In), Optional :: ipcr   ! - PCR flag -
   Integer, Intent(In), Optional :: ipfv   ! - PFV flag -
   Integer, Intent(In), Optional :: iregr  ! - regression flag -
   Integer, Intent(In), Optional :: iretro ! - retroactive validation flag -
   Integer, Intent(In), Optional :: isave  ! - save -
   Integer, Intent(In), Optional :: istd   ! - standardization flag -
   Integer, Intent(In), Optional :: izero  ! - zero-bound -
!
   Logical, Intent(In), Optional :: lclim    ! - setable climatological period? -
   Logical, Intent(In), Optional :: ldone    ! - calculation complete? -
   Logical, Intent(In), Optional :: leofx    ! - X EOF flag -
   Logical, Intent(In), Optional :: leofy    ! - Y EOF flag -
   Logical, Intent(In), Optional :: lexpert  ! - expert user? -
   Logical, Intent(In), Optional :: lmodes   ! - modes? -
   Logical, Intent(In), Optional :: lnofiles ! - no files open? -
   Logical, Intent(In), Optional :: lopen    ! - X and/or Y files open? -
   Logical, Intent(In), Optional :: lscree   ! - scree plots? -
   Logical, Intent(In), Optional :: ltitle   ! - title screen? -
   Logical, Intent(In), Optional :: lxyopen  ! - X and Y files open? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
#if GUI == 0
! Set screen menu greying
! - browse-
   If (Present(idstrz)) Then
      mn_screen_openz%ion = idstrz
      mn_screen_nf%ion = idstrz
   End If
! - length of training period (available only if results have not been calculated) -
   If (Present(ldone)) Then
      If (ldone) Then
         mn_screen_lentp%ion = 0
      Else
         mn_screen_lentp%ion = 1
      End If
   End If
!
#endif
! Set File menu greying
! - Save -
   If (Present(isave)) Then
      mn_file_save%ion = isave
   End If
! - Close -
   If (Present(idstrx)) mn_file_closex%ion = idstrx
   If (Present(idstry)) mn_file_closey%ion = idstry
   If (Present(idstrz)) mn_file_closez%ion = idstrz
! - Save and View Goodness Index (depends on whether results have been calculated) -
   If (Present(ldone)) Then
      If (ldone) Then
         mn_file_savegi%ion = 0
         mn_file_viewgi%ion = 1
         mn_file_datout%ion = 1
      Else
         mn_file_savegi%ion = 1
         mn_file_viewgi%ion = 0
         mn_file_datout%ion = 0
      End If
   End If
!
! Set Data menu greying
! - merge and update -
   If (Present(lnofiles)) Then
      If (lnofiles) Then
         mn_data_merge%ion = 1
         mn_data_update%ion = 1
      Else
         mn_data_merge%ion = 0
         mn_data_update%ion = 0
      End If
   End If
!
! Set Edit menu greying
! - start-at dates -
   If (Present(lopen)) Then
      If (lopen) Then
         mn_edit%ion = 1
         mn_edit_startat%ion = 1
      Else
         mn_edit%ion = 0
         mn_edit_startat%ion = 0
      End If
   End If
   If (Present(idstrx)) mn_edit_startatx%ion = idstrx ! - X start-at date -
   If (Present(idstry)) mn_edit_startaty%ion = idstry ! - Y start-at date -
   If (Present(idstrz)) mn_edit_startatz%ion = idstrz ! - forecast start-at date -
! - domains -
   If (Present(igeog))  mn_edit_datdom%ion  = igeog  ! - domains -
   If (Present(igeogx)) mn_edit_datdomx%ion = igeogx ! - X domain -
   If (Present(igeogy)) mn_edit_datdomy%ion = igeogy ! - Y domain -
   If (Present(icalc)) Then
      If (icalc == 1) Then        ! - disable edit menu when results are calculated -
         mn_edit_datdom%ion = 0   ! - edit data-domain -
         mn_edit_tssn%ion = 0     ! - edit target season -
         mn_edit_startatx%ion = 0 ! - X start-at date -
         mn_edit_startaty%ion = 0 ! - Y start-at date -
      End If
! - target season -
      If (Present(icsv)) Then
         Select Case (ianal)
          Case Default ! - CCA, PCR, MLR, GCM -
            mn_edit_tssn%ion = icsv*(1 - icalc)
          Case (ia_pfv) ! - PFV -
            mn_edit_tssn%ion = 0
         End Select
      End If
   End If
!
! Set Actions menu greying
   If (Present(lxyopen)) Then
      If (lxyopen) Then
         mn_acts%ion = 1
      Else
         mn_acts%ion = 0
      End If
   End If
! - calculate and reset (depend on whether results have been calculated) -
   If (Present(ldone)) Then
      If (ldone) Then
         mn_acts_calc%ion = 0
         mn_acts_reset%ion = 1
      Else
         mn_acts_calc%ion = 1
         mn_acts_reset%ion = 0
      End If
   End If
! - CV, retroactive, and verify -
   If (Present(ianal)) Then
      Select Case (ianal)
       Case Default ! - CCA, PCR, MLR, GCM -
         mn_acts_cv%ion = 1
         mn_acts_cv2%ion = 1
         mn_acts_retro%ion = 1
         mn_acts_verify%ion = 0
       Case (ia_pfv) ! - PFV -
         mn_acts_cv%ion = 0
         mn_acts_cv2%ion = 0
         mn_acts_retro%ion = 0
         mn_acts_verify%ion = 1
      End Select
   End If
!
! Set Tools menu greying
   If (Present(ldone)) Then
      If (ldone) Then
         mn_tool%ion = 1
      Else
         mn_tool%ion = 0
      End If
   End If
! - validation -
   If (Present(ihind)) mn_tool_validn%ion = ihind
   If (Present(icv2)) Then
      mn_tool_c2%ion = icv2
      If (Present(iretro)) Then
         mn_tool_verifn%ion = icv2 + iretro
      Else
         mn_tool_verifn%ion = icv2
      End If
   End If
   If (Present(iretro)) Then
      mn_tool_ra%ion = iretro
      If (Present(icv2)) Then
         mn_tool_verifn%ion = icv2 + iretro
      Else
         mn_tool_verifn%ion = iretro
      End If
   End If
! - modes -
   If (Present(lmodes)) Then
      If (lmodes) Then
         mn_tool_models%ion = 1
      Else
         mn_tool_models%ion = 0
      End If
   End If
! - scree plots -
   If (Present(lscree)) Then
      If (lscree) Then
         mn_tool_screes%ion = 1
      Else
         mn_tool_screes%ion = 0
      End If
   End If
#if GUI == 1
! - CCA modes -
   If (Present(leofy)) Then
      If (leofy) Then
         mn_tool_cca%ion = 1
      Else
         mn_tool_cca%ion = 0
      End If
   End If
#endif
! - regression (only available for PCR, MLR, and GCM) -
   If (Present(ianal) .and. Present(icalc)) Then
      Select Case (ianal)
       Case (ia_pcr, ia_mlr, ia_gcm) ! - PCR, MLR, GCM -
         mn_tool_regress%ion = icalc
       Case Default
         mn_tool_regress%ion = 0
      End Select
   End If
! - coefficients of variation (assumes a zero bound) -
   If (Present(izero)) mn_tool_cvars%ion = izero
! - forecast predictors map -
   If (Present(ifc)) mn_tool_zmap%ion = ifc
! - forecast -
   If (Present(ifcast)) mn_tool_fcasts%ion = ifcast
! - forecast series, ensembles, probability of exceedance, and values -
   If (Present(ihind)) Then
      mn_tool_corrs%ion = ihind
      mn_tool_xmap%ion = ihind
      mn_tool_ymap%ion = ihind
      mn_tool_fser%ion = ihind
      mn_tool_fens%ion = ihind
      mn_tool_pexc%ion = ihind
      mn_tool_fval%ion = ihind
   End If
#if GUI == 1
! - probabilistic forecasts -
   If (Present(igeogy)) mn_tool_hps%ion = igeogy
#endif
!
! Set Options menu greying
! - expertise level -
   If (Present(lexpert)) Then
      If (lexpert) Then
         mn_opts_begin%ion = 1
         mn_opts_expert%ion = 0
         mn_opts_models%ion = 0
      Else
         mn_opts_begin%ion = 0
         mn_opts_expert%ion = 1
         mn_opts_models%ion = 1
      End If
   End If
! - modes -
   If (Present(ihind)) mn_opts_cv%ion = ihind
   If (Present(icalc)) Then
      If (icalc == 1) Then
         mn_opts_cv%ion = 0
         mn_opts_cca%ion = 0
         mn_opts_ccaadv%ion = 0
         mn_opts_xmodes%ion = 0
         mn_opts_ymodes%ion = 0
         mn_opts_xmodeadv%ion = 0
         mn_opts_ymodeadv%ion = 0
         mn_opts_regr%ion = 0
      End If
   End If
! - data, goodness index, drymask, time (available only if results have not been calculated) -
   If (Present(ldone)) Then
      If (ldone) Then
         mn_opts_data%ion = 0
         mn_opts_good%ion = 0
         mn_opts_time%ion = 0
         mn_opts_maskdry%ion = 0
      Else
         mn_opts_data%ion = 1
         mn_opts_good%ion = 1
         mn_opts_time%ion = 1
         mn_opts_maskdry%ion = 1
      End If
   End If
! - exclude -
   If (Present(idstry)) Then
      If (idstry > 0) Then
         mn_opts_exclude%ion = 1
      Else
         mn_opts_exclude%ion = 0
      End If
   End If
! - X, Y and CCA modes -
   If (Present(leofx)) Then
      If (leofx) Then
         mn_opts_xmodes%ion = 1
#if GUI == 0
         mn_opts_xmodeadv%ion = 1
#endif
      Else
         mn_opts_xmodes%ion = 0
#if GUI == 0
         mn_opts_xmodeadv%ion = 0
#endif
      End If
   End If
   If (Present(leofy)) Then
      If (leofy) Then
         mn_opts_ymodes%ion = 1
         mn_opts_cca%ion = 1
#if GUI == 0
         mn_opts_ymodeadv%ion = 1
         mn_opts_ccaadv%ion = 1
#endif
      Else
         mn_opts_ymodes%ion = 0
         mn_opts_cca%ion = 0
#if GUI == 0
         mn_opts_ymodeadv%ion = 0
         mn_opts_ccaadv%ion = 0
#endif
      End If
   End If
! - climatological period -
   If (Present(lclim)) Then
      If (lclim) Then
         mn_opts_climper%ion = 1
      Else
         mn_opts_climper%ion = 0
      End If
   End If
! - GCM, regression (only available for PCR, MLR, and GCM) -
   If (Present(ianal) .and. Present(icalc)) Then
      Select Case (ianal)
       Case (ia_gcm) ! - GCM -
         mn_opts_gcm%ion = 1 - icalc
       Case Default
         mn_opts_gcm%ion = 0
      End Select
      Select Case (ianal)
       Case (ia_pcr, ia_mlr, ia_gcm) ! - PCR, MLR, GCM -
         mn_opts_regr%ion = 1 - icalc
       Case Default
         mn_opts_regr%ion = 0
      End Select
      mn_opts_link%ion = mn_opts_regr%ion
      If (Present(iregr) .and. Present(istd)) Then
! - transformation option flag -
         mn_opts_trnss%ion = set_mn_opts_trnss(ianal, icalc, iregr, istd)
! - Y transformation option flag -
         mn_opts_trnsy%ion = mn_opts_trnss%ion
! - zero-bound option flag -
         mn_opts_zerob%ion = mn_opts_trnss%ion
      End If
! - verification options flag -
      mn_opts_verif%ion = set_mn_opts_verif(ianal, &
                          icv2=icv2, iretro=iretro)
   End If
! - tailoring -
   If (Present(ihind)) mn_opts_tailor%ion = ihind
!
! Set View menu greying
   If (Present(ldone)) Then
      If (ldone) Then
         mn_view%ion = 0
      Else
         mn_view%ion = 1
      End If
   End If
   If (Present(ltitle)) Then
      If (ltitle) mn_view%ion = 1
   End If
   If (Present(icca)) Then
      mn_view_cca%ion = icca
      mn_opts_mdlcca%ion = icca
   End If
   If (Present(ipcr)) Then
      mn_view_pcr%ion = ipcr
      mn_opts_mdlpcr%ion = ipcr
   End If
   If (Present(imlr)) Then
      mn_view_mlr%ion = imlr
      mn_opts_mdlmlr%ion = imlr
   End If
   If (Present(igcm)) Then
      mn_view_gcm%ion = igcm
      mn_opts_mdlgcm%ion = igcm
   End If
   If (Present(ipfv)) Then
      mn_view_pfv%ion = ipfv
      mn_view_verifn%ion = ipfv
   End If
   If (Present(idia)) Then
      mn_view_diagnost%ion = idia
   End If
   If (Present(ifor)) Then
      mn_view_fcast%ion = ifor
   End If
!
   Return
 End Subroutine set_menu_greying
!
!
!
 Function set_mn_opts_trnss(ianal, icalc, iregr, istd) &
          Result (ion)
!
! Sets grey state for Option ~ Transformation Settings
!
! Modules
   Use data_cpt_constants, Only: ia_non, ia_pfv, ir_ols, is_sti
!
! Function result
   Integer :: ion
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis flag -
   Integer, Intent(In) :: icalc ! - calculation completed flag -
   Integer, Intent(In) :: iregr ! - regression option -
   Integer, Intent(In) :: istd  ! - standardization option -
!
! Executable Statements
!
! Set grey state for Option ~ Transformation Settings
   Select Case (ianal)
    Case Default
      Select Case (istd)
       Case Default
         Select Case (iregr)
          Case (ir_ols)
            ion = 1 - icalc
          Case Default
            ion = 0
         End Select
       Case (is_sti)
         ion = 0
      End Select
    Case (ia_pfv, ia_non)
      ion = 0
   End Select
!
   Return
 End Function set_mn_opts_trnss
!
!
!
 Function set_mn_opts_verif(ianal, &
          icv2, iretro) Result (ion)
!
! Sets grey state for Option ~ Verification Settings (not actually used to grey)
!
! Modules
   Use data_cpt_constants, Only: ia_pfv
!
! Function result
   Integer :: ion
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis flag -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: icv2   ! - double cross-validation analysis flag -
   Integer, Intent(In), Optional :: iretro ! - retroactive analysis flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Set grey state for Option ~ Verification Settings
   If (ianal /= ia_pfv) Then
      If (Present(icv2)) Then
         ion = icv2
      Else If (Present(iretro)) Then
         ion = iretro
      Else
         ion = 0
      End If
   Else
      ion = 1
   End If
!
   Return
 End Function set_mn_opts_verif
!
!
!
 Subroutine set_menu_label ( &
            ipval, itags)
!
! Sets conditional menu labels
!
! Modules
!
! Modules
#if GUI == 0
   Use labels, Only: l_pvalues, l_skillmaps
#endif
!
! Arguments
!
! Optional arguments
! - input scalars -
   Integer, Intent(In), Optional :: ipval ! - p-value flag - 
   Integer, Intent(In), Optional :: itags ! - output tags flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Tools ~ Validation ~ Skill Maps
#if GUI == 0
   If (Present(ipval)) Then
      Select Case (ipval)
       Case (0)
          mn_tool_cvmaps%l_lab = l_skillmaps
          mn_tool_c2maps%l_lab = l_skillmaps
          mn_tool_ramaps%l_lab = l_skillmaps
       Case (1)
          mn_tool_cvmaps%l_lab = l_pvalues
          mn_tool_c2maps%l_lab = l_pvalues
          mn_tool_ramaps%l_lab = l_pvalues
      End Select
   End If
#endif
!
! File ~ Output CPT tags
   If (Present(itags)) Then
      Select Case (itags)
       Case (0)
         mn_file_tags%l_lab%c = 'Include CPT output tags'
       Case (1)
         mn_file_tags%l_lab%c = 'Exclude CPT output tags'
      End Select
      mn_file_tags%l_lab%iak = 20
   End If
!
   Return
 End Subroutine set_menu_label
!
!
!
 Function record_change() &
          Result(chnge)
!
! Records a change to be saved in project file
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Record change
   isave = 1
   Call set_menu_greying (isave=isave)
#if GUI == 0
   lmnact = .true.
#endif
   chnge = 2
!
   Return
 End Function record_change
End Module menus
