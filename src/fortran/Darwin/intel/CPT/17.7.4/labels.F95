! Author: Simon Mason
Module labels
!
! Modules
   Use data_text,           Only: llab3, llab4, llab5, llab6, nlang
   Use data_cpt_constants,  Only: na, ncorr, nel, nev, ng, ngc, ngood, nic, ninterp, nlink, nmcs, npall, npcat, npo, nps, nregr, &
                                  nsag, nscore, nstd, nstruct, nthr, ntime, nto, nts, nwr
#if GUI == 0
   Use data_cpt_constants,  Only: nueo
#else
   Use data_gui_constants,  Only: ngf, nmrk
#endif
   Use data_io_constants,   Only: lstr
   Use data_time_constants, Only: nsq
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: init_labels, lowcase, make_label, upcase
!
! Derived type definitions
!
! - label -
   Public :: label
   Type label
      Sequence
      Character(Len=llab6) :: c ! - label text -
      Integer :: iak            ! - accelerator-key position -
   End Type label
!
! Scalars
!
! Character scalars
   Character(Len=llab6), Public :: c_lab ! - label -
!
! Label scalars
   Type(label), Public :: l_lab         ! - label -
!
! - menu labels -
   Type(label), Public :: l_abort       ! - Abort -
   Type(label), Public :: l_about       ! - About -
   Type(label), Public :: l_actions     ! - Actions -
   Type(label), Public :: l_advanced    ! - Advanced -
   Type(label), Public :: l_analysis    ! - analysis -
#if GUI == 1
   Type(label), Public :: l_applyall    ! - Apply to All -
   Type(label), Public :: l_applyalls   ! - Apply to All Subsequent -
#endif
   Type(label), Public :: l_attribs     ! - Attributes Diagrams -
   Type(label), Public :: l_averages    ! - Averages -
#if GUI == 1
   Type(label), Public :: l_axislims    ! - Axis Limits -
   Type(label), Public :: l_axislimsx   ! - X-Axis Limits -
   Type(label), Public :: l_axislimsy   ! - Y-Axis Limits -
#endif
   Type(label), Public :: l_beginner    ! - Beginner -
   Type(label), Public :: l_boot        ! - Bootstrap -
#if GUI == 1
   Type(label), Public :: l_broken      ! - Broken Stick -
   Type(label), Public :: l_bw          ! - Black and White -
#endif
   Type(label), Public :: l_calculate   ! - Calculate -
   Type(label), Public :: l_cancel      ! - Cancel -
#if GUI == 1
   Type(label), Public :: l_catshade    ! - Category Shading -
#endif
   Type(label), Public :: l_ccamaps     ! - CCA Maps -
   Type(label), Public :: l_ccaopts     ! - CCA Options -
   Type(label), Public :: l_ccas        ! - CCA Scores -
   Type(label), Public :: l_ccax        ! - X CCA Maps -
   Type(label), Public :: l_ccay        ! - Y CCA Maps -
   Type(label), Public :: l_climatol    ! - Climatology -
   Type(label), Public :: l_climmaps    ! - Climatological Maps -
   Type(label), Public :: l_climper     ! - Climatological Period -
   Type(label), Public :: l_close       ! - Close -
   Type(label), Public :: l_config      ! - Configuration -
#if GUI == 1
   Type(label), Public :: l_console     ! - Console -
   Type(label), Public :: l_contourlims ! - Contour Limits -
#endif
   Type(label), Public :: l_correls     ! - Correlations -
#if GUI == 1
   Type(label), Public :: l_crosses     ! - Crosses on Graph -
#endif
   Type(label), Public :: l_ctables     ! - Contingency Tables -
#if GUI == 1
   Type(label), Public :: l_cumulat     ! - Cumulative -
#endif
   Type(label), Public :: l_custom      ! - Customise -
   Type(label), Public :: l_cvars       ! - Coefficients of Variation -
   Type(label), Public :: l_cvn         ! - Cross-validation -
   Type(label), Public :: l_cvn2        ! - Double cross-validation -
   Type(label), Public :: l_cvopts      ! - Cross-validation Options -
   Type(label), Public :: l_data        ! - Data -
   Type(label), Public :: l_datdom      ! - Data Domain -
#if GUI == 1
   Type(label), Public :: l_datlimits   ! - Data Limits -
#endif
   Type(label), Public :: l_defaults    ! - Defaults -
   Type(label), Public :: l_diagnostics ! - Diagnostics -
   Type(label), Public :: l_download    ! - Download -
   Type(label), Public :: l_drymask     ! - Dry Mask -
   Type(label), Public :: l_edit        ! - Edit -
   Type(label), Public :: l_ensave      ! - Ensemble Average -
   Type(label), Public :: l_ensembles   ! - Ensembles -
   Type(label), Public :: l_enso        ! - ENSO Indices -
   Type(label), Public :: l_errhand     ! - Error Handling -
   Type(label), Public :: l_exceedps    ! - Exceedance Probabilities -
   Type(label), Public :: l_exclude     ! - Exclude -
   Type(label), Public :: l_exit        ! - Exit -
   Type(label), Public :: l_expert      ! - Expert -
   Type(label), Public :: l_expertlev   ! - Expertise Level -
   Type(label), Public :: l_export      ! - Export -
   Type(label), Public :: l_fcast       ! - Forecast -
   Type(label), Public :: l_fcasts      ! - Forecasts -
   Type(label), Public :: l_fcastsets   ! - Forecast Settings -
   Type(label), Public :: l_file        ! - File -
#if GUI == 1
   Type(label), Public :: l_fntscal     ! - Font Scaling -
#endif
   Type(label), Public :: l_gcmouts     ! - GCM outputs -
   Type(label), Public :: l_gcmopts     ! - GCM Options -
   Type(label), Public :: l_goodness    ! - Goodness Index -
   Type(label), Public :: l_gph         ! - Geopotential Heights -
   Type(label), Public :: l_graphics    ! - Graphics -
#if GUI == 1
   Type(label), Public :: l_graphscal   ! - Graphics Scaling -
#endif
   Type(label), Public :: l_help        ! - Help -
   Type(label), Public :: l_hindplot    ! - Hindcast Plot -
   Type(label), Public :: l_lang        ! - Language -
   Type(label), Public :: l_linkfs      ! - Link Functions -
   Type(label), Public :: l_loads       ! - Loadings -
   Type(label), Public :: l_loadsx      ! - X Loadings and Scores -
   Type(label), Public :: l_loadsy      ! - Y Loadings and Scores -
#if GUI == 1
   Type(label), Public :: l_logaxis     ! - Logarithmic Axis -
#endif
   Type(label), Public :: l_maps        ! - Maps -
   Type(label), Public :: l_masklake    ! - Mask Lakes -
   Type(label), Public :: l_maskland    ! - Mask Land -
   Type(label), Public :: l_masksets    ! - Masks Settings -
   Type(label), Public :: l_merge       ! - Merge -
   Type(label), Public :: l_missings    ! - Missing Values -
   Type(label), Public :: l_models      ! - Models -
   Type(label), Public :: l_modeopts    ! - Mode Options -
   Type(label), Public :: l_modeoptsx   ! - X Mode Options -
   Type(label), Public :: l_modeoptsy   ! - Y Mode Options -
   Type(label), Public :: l_modes       ! - Modes -
   Type(label), Public :: l_montage     ! - Montage -
   Type(label), Public :: l_mslp        ! - MSLP -
   Type(label), Public :: l_new         ! - New -
#if GUI == 1
   Type(label), Public :: l_next        ! - Next -
#endif
   Type(label), Public :: l_no          ! - No -
   Type(label), Public :: l_obs         ! - Observations -
   Type(label), Public :: l_odds        ! - Odds -
   Type(label), Public :: l_oddsrel     ! - Relative Odds -
   Type(label), Public :: l_open        ! - Open -
   Type(label), Public :: l_options     ! - Options -
   Type(label), Public :: l_outres      ! - Output Results -
   Type(label), Public :: l_performs    ! - Performance Measures -
#if GUI == 1
   Type(label), Public :: l_pintervals  ! - Prediction Intervals -
#endif
   Type(label), Public :: l_plevs       ! - Pressure Levels -
#if GUI == 0
   Type(label), Public :: l_printmenus  ! - Print Menus -
#endif
   Type(label), Public :: l_prcp        ! - Precipitation -
   Type(label), Public :: l_predictandy ! - Predictands (Y) -
   Type(label), Public :: l_predictorx  ! - Predictors (X) -
   Type(label), Public :: l_predictorz  ! - New Predictors (Z) -
   Type(label), Public :: l_probc       ! - Probabilistic -
   Type(label), Public :: l_probs       ! - Probabilities -
   Type(label), Public :: l_pvalues     ! - P-values -
   Type(label), Public :: l_reanalyses  ! - Reanalyses -
#if GUI == 1
   Type(label), Public :: l_redrawmap   ! - Redraw Map -
#endif
   Type(label), Public :: l_regress     ! - Regression -
   Type(label), Public :: l_regrid      ! - Regrid -
   Type(label), Public :: l_regrmods    ! - Regression Models -
   Type(label), Public :: l_regropts    ! - Regression Options -
   Type(label), Public :: l_resamples   ! - Resampling Settings -
   Type(label), Public :: l_resdefs     ! - Restore Defaults -
   Type(label), Public :: l_reset       ! - Reset -
   Type(label), Public :: l_residual    ! - Residuals Plot -
   Type(label), Public :: l_retro       ! - Retroactive -
#if GUI == 1
   Type(label), Public :: l_reversecols ! - Reverse Colours -
#endif
   Type(label), Public :: l_rhitsdiag   ! - Ranked-Hits Diagram  -
   Type(label), Public :: l_rocdiag     ! - ROC Diagram -
#if GUI == 1
   Type(label), Public :: l_sameprev    ! - Same as Previous -
#endif
   Type(label), Public :: l_save        ! - Save -
   Type(label), Public :: l_saveas      ! - Save As -
   Type(label), Public :: l_savegi      ! - Save Goodness Index -
   Type(label), Public :: l_saveres     ! - Save Results -
#if GUI == 1
   Type(label), Public :: l_saveroc     ! - Save ROC -
#endif
   Type(label), Public :: l_savepar     ! - Save as Parameter File -
   Type(label), Public :: l_scatter     ! - Scatter Plot -
   Type(label), Public :: l_scatters    ! - Scatter Plots -
   Type(label), Public :: l_scores      ! - Scores -
   Type(label), Public :: l_scoresp     ! - Probabilistic Scores -
#if GUI == 1
   Type(label), Public :: l_scorgrphs   ! - Score on Graphs -
#endif
   Type(label), Public :: l_screes      ! - Scree Plots -
   Type(label), Public :: l_screex      ! - X Scree Plot -
   Type(label), Public :: l_screey      ! - Y Scree Plot -
   Type(label), Public :: l_selsubdom   ! - Select Sub-domain -
   Type(label), Public :: l_series      ! - Series -
   Type(label), Public :: l_shadeland   ! - Shade Land -
   Type(label), Public :: l_shadesea    ! - Shade Sea -
   Type(label), Public :: l_showopts    ! - Show Options -
   Type(label), Public :: l_skillmaps   ! - Skill Maps -
   Type(label), Public :: l_skillmask   ! - Skill Mask -
   Type(label), Public :: l_sortens     ! - Sort Ensemble Members -
   Type(label), Public :: l_sst         ! - SST -
   Type(Label), Public :: l_startat     ! - Start at -
   Type(label), Public :: l_stdevs      ! - Standard Deviations -
   Type(label), Public :: l_stdn        ! - Standardization -
#if GUI == 1
   Type(label), Public :: l_stnmrks     ! - Station Markers -
#endif
   Type(label), Public :: l_synchps     ! - Synchronous Predictors -
   Type(label), Public :: l_tailor      ! - Tailoring -
   Type(label), Public :: l_tenddiag    ! - Tendency Diagram -
   Type(label), Public :: l_threshs     ! - Thresholds -
   Type(label), Public :: l_timesets    ! - Time Settings -
   Type(label), Public :: l_title       ! - Title -
   Type(label), Public :: l_tmax        ! - Minimum Temperature -
   Type(label), Public :: l_tmean       ! - Mean Temperature -
   Type(label), Public :: l_tmin        ! - Maximum Temperature -
   Type(label), Public :: l_tools       ! - Tools -
   Type(label), Public :: l_transforms  ! - Transformation Settings -
   Type(label), Public :: l_transformy  ! - Transform Y Data -
   Type(label), Public :: l_tseason     ! - Target Season -
#if GUI == 1
   Type(label), Public :: l_txtscal     ! - Text Scaling -
#endif
   Type(label), Public :: l_u           ! - U-Component -
   Type(label), Public :: l_update      ! - Update -
   Type(label), Public :: l_uv          ! - Wind Components -
   Type(label), Public :: l_v           ! - V-Component -
   Type(label), Public :: l_validn      ! - Validation -
   Type(label), Public :: l_values      ! - Values -
   Type(label), Public :: l_verifn      ! - Verification -
   Type(label), Public :: l_verifsets   ! - Verification Settings -
   Type(label), Public :: l_verify      ! - Verify -
   Type(label), Public :: l_view        ! - View -
   Type(label), Public :: l_viewgi      ! - View Goodness Index -
#if GUI == 1
   Type(label), Public :: l_vlines      ! - Vertical Lines on Graphs -
#endif
   Type(label), Public :: l_wetdays     ! - Wet Days -
   Type(label), Public :: l_wrlt        ! - Weather Roulette -
   Type(label), Public :: l_yes         ! - Yes -
   Type(label), Public :: l_zerob       ! - Zero-Bound -
!
! Arrays
!
! Label arrays
! - menu labels -
   Type(label), Dimension(  na), Public :: ls_analyses  ! - analysis types -
   Type(label), Dimension(0:ng), Public :: ls_cats      ! - category labels -
#if GUI == 1
   Type(label), Dimension( ngf), Public :: ls_graphfmts ! - graphic file formats -
#endif
   Type(label), Dimension( nts), Public :: ls_threshs   ! - thresholds -
#if GUI == 0
   Type(label), Dimension(nueo), Public :: ls_uerropts  ! - user-error options -
#endif
   Type(label), Dimension( nwr), Public :: ls_wrlts     ! - weather roulette options -
!
! Scalars
!
! - text labels -
   Character(Len=llab3), Public :: cg_advisory     ! - Advisory -
   Character(Len=llab3), Public :: cg_and_l        ! - and -
   Character(Len=llab6), Public :: cg_approxdlb    ! - Approximate data limits in brackets -
   Character(Len=llab5), Public :: cg_attrib_t     ! - Attributes Diagram -
#if GUI == 1
   Character(Len=llab3), Public :: cg_black_l      ! - black -
#endif
   Character(Len=llab4), Public :: cg_browse_l     ! - browse -
   Character(Len=llab5), Public :: cg_cancor_t     ! - Canonical Correlation -
   Character(Len=llab5), Public :: cg_ccacopts_t   ! - CCA Calculation Options -
   Character(Len=llab5), Public :: cg_conflev      ! - Confidence level -
   Character(Len=llab4), Public :: cg_correl       ! - Correlation -
   Character(Len=llab5), Public :: cg_cselection   ! - Current selection -
   Character(Len=llab5), Public :: cg_data1        ! - First data -
   Character(Len=llab5), Public :: cg_datan        ! - Last data -
   Character(Len=llab4), Public :: cg_date1        ! - First date -
   Character(Len=llab4), Public :: cg_daten        ! - Last date -
   Character(Len=llab3), Public :: cg_domain       ! - Domain -
   Character(Len=llab5), Public :: cg_domlims      ! - Domain limits -
   Character(Len=llab4), Public :: cg_done         ! - Done -
   Character(Len=llab4), Public :: cg_empirical    ! - Empirical -
   Character(Len=llab4), Public :: cg_ensemble     ! - Ensemble -
   Character(Len=llab3), Public :: cg_error        ! - Error -
   Character(Len=llab3), Public :: cg_errors       ! - Errors -
   Character(Len=llab5), Public :: cg_fcastprobs   ! - Forecast probabilities -
   Character(Len=llab3), Public :: cg_field        ! - Field -
   Character(Len=llab3), Public :: cg_fields_l     ! - fields -
   Character(Len=llab4), Public :: cg_filename     ! - File name -
   Character(Len=llab6), Public :: cg_fpround      ! - Forecast-probability rounding -
#if GUI == 1
   Character(Len=llab3), Public :: cg_green_l      ! - green -
   Character(Len=llab3), Public :: cg_grey_l       ! - grey -
#endif
   Character(Len=llab4), Public :: cg_hinds        ! - Hindcasts -
   Character(Len=llab5), Public :: cg_indata_t     ! - Input Data -
   Character(Len=llab5), Public :: cg_infiles_t    ! - Input Files -
   Character(Len=llab4), Public :: cg_initin_l     ! - initialised in -
   Character(Len=llab4), Public :: cg_interpn      ! - Interpolation -
   Character(Len=llab4), Public :: cg_lagfield     ! - Lagged field -
   Character(Len=llab4), Public :: cg_lags_l       ! - lags -
   Character(Len=llab3), Public :: cg_lat          ! - Latitude -
   Character(Len=llab5), Public :: cg_latnmost     ! - Northernmost latitude -
   Character(Len=llab5), Public :: cg_latsmost     ! - Southernmost latitude -
   Character(Len=llab6), Public :: cg_lcv          ! - Length of cross-validation window -
   Character(Len=llab6), Public :: cg_lencp        ! - Length of climatological period -
   Character(Len=llab6), Public :: cg_lenitp       ! - Length of initial training period -
   Character(Len=llab4), Public :: cg_lenspi       ! - Length of SPI -
   Character(Len=llab6), Public :: cg_lentp        ! - Length of training period -
   Character(Len=llab3), Public :: cg_limits       ! - Limits -
   Character(Len=llab5), Public :: cg_linkf        ! - Link function -
   Character(Len=llab4), Public :: cg_lng          ! - Longitude -
   Character(Len=llab5), Public :: cg_lngemost     ! - Easternmost longitude -
   Character(Len=llab5), Public :: cg_lngwmost     ! - Westernmost longitude -
   Character(Len=llab5), Public :: cg_maxnof       ! - Maximum number of -
   Character(Len=llab5), Public :: cg_minnof       ! - Minimum number of -
   Character(Len=llab6), Public :: cg_missmethod   ! - Method for replacing missing data -
   Character(Len=llab5), Public :: cg_missvalflag  ! - Missing-value flag -
   Character(Len=llab3), Public :: cg_mode         ! - Mode -
   Character(Len=llab5), Public :: cg_modecopts_t  ! - Mode Calculation Options -
   Character(Len=llab3), Public :: cg_model        ! - Model -
   Character(Len=llab5), Public :: cg_modelcomb    ! - Model combination -
   Character(Len=llab5), Public :: cg_mxpmissdat   ! - Maximum % of missing data -
   Character(Len=    3), Public :: cg_na           ! - N/A -
   Character(Len=llab6), Public :: cg_neglatlng    ! - Southern latitudes and western longitudes negatives -
   Character(Len=llab4), Public :: cg_nino_t       ! - NINO Indices -
   Character(Len=llab5), Public :: cg_nnearns      ! - Number of near-neighbours -
   Character(Len=llab4), Public :: cg_nof          ! - Number of -
   Character(Len=llab5), Public :: cg_nofsave      ! - Number of files to save -
   Character(Len=llab3), Public :: cg_none         ! - None -
   Character(Len=llab3), Public :: cg_none_l       ! - none -
   Character(Len=llab5), Public :: cg_nused        ! - Number used -
   Character(Len=llab4), Public :: cg_obs          ! - Observations -
   Character(Len=llab3), Public :: cg_output       ! - Output -
   Character(Len=llab4), Public :: cg_password_l   ! - password -
   Character(Len=llab4), Public :: cg_perms        ! - Permutations -
   Character(Len=llab4), Public :: cg_persist      ! - Persistence -
   Character(Len=llab5), Public :: cg_pinterval_t  ! - Prediction Interval -
   Character(Len=llab4), Public :: cg_progress     ! - Progress -
   Character(Len=llab5), Public :: cg_pseason      ! - Persistence season -
#if GUI == 1
   Character(Len=llab3), Public :: cg_red_l        ! - red -
#endif
   Character(Len=llab5), Public :: cg_ref          ! - Reference -
   Character(Len=llab5), Public :: cg_regreqs      ! - Regression equations -
   Character(Len=llab6), Public :: cg_roc_t        ! - Relative Operating Characteristics -
   Character(Len=llab5), Public :: cg_rootdir      ! - Root directory -
   Character(Len=llab5), Public :: cg_runtime_l    ! - runtime -
   Character(Len=llab3), Public :: cg_score        ! - Score -
   Character(Len=llab3), Public :: cg_screen       ! - Screen -
   Character(Len=llab4), Public :: cg_seasons_l    ! - seasons -
   Character(Len=llab5), Public :: cg_seasonlims   ! - Season limits -
   Character(Len=llab5), Public :: cg_selmethod    ! - Select method -
   Character(Len=llab5), Public :: cg_seloption    ! - Select option -
   Character(Len=llab5), Public :: cg_stddata      ! - Standardized data -
   Character(Len=llab4), Public :: cg_theoretical  ! - Theoretical -
   Character(Len=llab4), Public :: cg_thresh       ! - Threshold -
   Character(Len=llab5), Public :: cg_threshslow   ! - Lower thresholds -
   Character(Len=llab5), Public :: cg_threshsupp   ! - Upper thresholds -
   Character(Len=llab4), Public :: cg_threshtype   ! - Type of threshold -
   Character(Len=llab3), Public :: cg_time         ! - Time -
   Character(Len=llab5), Public :: cg_timeseq      ! - Time sequencing -
   Character(Len=llab3), Public :: cg_to_l         ! - to -
   Character(Len=llab5), Public :: cg_tperiod      ! - Training period -
   Character(Len=llab5), Public :: cg_tperiodui    ! - Training-period update interval -
   Character(Len=llab5), Public :: cg_traindata    ! - Training Data -
   Character(Len=llab4), Public :: cg_turnoff_l    ! - turn off -
   Character(Len=llab3), Public :: cg_turnon_l     ! - turn on -
   Character(Len=llab4), Public :: cg_undefined_l  ! - undefined -
   Character(Len=llab5), Public :: cg_uvars_l      ! - used variables -
   Character(Len=llab4), Public :: cg_valmax       ! - Maximum value -
   Character(Len=llab4), Public :: cg_valmin       ! - Minimum value -
   Character(Len=llab3), Public :: cg_version      ! - Version -
   Character(Len=llab4), Public :: cg_warning      ! - Warning -
   Character(Len=llab4), Public :: cg_year1        ! - First year -
   Character(Len=llab4), Public :: cg_yearn        ! - Last year -
!
! - labels -
   Character(Len=10), Public :: cls_l ! - lags/seasons -
!
! Arrays
! - text labels -
   Character(Len=     3), Dimension(       na), Public :: ca_atypes_a  ! - analysis abbreviations -
   Character(Len=     1), Dimension(       ng), Public :: ca_cat_a     ! - category abbreviations -
   Character(Len= llab6), Dimension(      ngc), Public :: ca_corrects  ! - correction options -
   Character(Len= llab5), Dimension(    ncorr), Public :: ca_corrs     ! - correlations -
   Character(Len=lstr-1), Dimension(0:nstruct), Public :: ca_dstruct   ! - data structure descriptions -
   Character(Len=  lstr), Dimension(0:nstruct), Public :: ca_dstruct_l ! - data structure descriptions -
   Character(Len= llab4), Dimension(      nel), Public :: ca_elevels   ! - expertise levels -
!   Character(Len= llab5), Dimension(    nensc), Public :: ca_ensc      ! - ensemble forecasting options -
!      ['None               ', 'Ensemble counting  ', 'Fitted distribution']
   Character(Len= llab6), Dimension(      nev), Public :: ca_evs       ! - error-variance options -
   Character(Len= llab5), Dimension(  nstruct), Public :: ca_ftype     ! - file structure types -
   Character(Len= llab6), Dimension(      nic), Public :: ca_ic        ! - information criteria -
   Character(Len= llab5), Dimension(  ninterp), Public :: ca_interps   ! - interpolation options -
   Character(Len= llab5), Dimension(    nlink), Public :: ca_links     ! - link functions -
#if GUI == 1
   Character(Len= llab4), Dimension(  nmrk), Public :: ca_markers      ! - markers -
#endif
   Character(Len= llab6), Dimension(  nmcs), Public :: ca_mcs          ! - model combination options -
   Character(Len= llab6), Dimension( npall), Public :: ca_pavscores_t  ! - all-category probabilistic verification scores -
   Character(Len= llab6), Dimension(   npo), Public :: ca_pccos        ! - principal components calculation options -
   Character(Len= llab6), Dimension( npcat), Public :: ca_pcvscores_t  ! - category-specific probabilistic verification scores -
   Character(Len= llab6), Dimension(   nps), Public :: ca_pvs_t        ! - probabilistic verification scores -
   Character(Len= llab6), Dimension( nregr), Public :: ca_regrs        ! - regression options -
   Character(Len= llab6), Dimension(nscore), Public :: ca_scores       ! - verification measures -
   Character(Len= llab4), Dimension( nsq+1), Public :: ca_seq          ! - date sequence types -
   Character(Len= llab4), Dimension( nsq+1), Public :: ca_seqs         ! - date sequence types -
   Character(Len= llab4), Dimension( nsq+1), Public :: ca_seq_l        ! - date sequence types -
   Character(Len= llab4), Dimension( nsq+1), Public :: ca_seqs_l       ! - date sequence types -
   Character(Len= llab5), Dimension(  nsag), Public :: ca_ssnagg       ! - seasonal aggregation options -
   Character(Len= llab5), Dimension(  nstd), Public :: ca_stds_t       ! - standardizations -
   Character(Len= llab5), Dimension(  nthr), Public :: ca_thrs         ! - threshold options -
   Character(Len= llab6), Dimension( ntime), Public :: ca_times        ! - time options -
   Character(Len= llab5), Dimension(   nto), Public :: ca_tos          ! - transformation options -
!
   Character(Len= llab4), Dimension(0:ng,2), Public :: ca_bss         ! - Brier, ranked probability score and skill scores -
!
! - constructed labels -
   Character(Len= llab6), Dimension(ngood), Public :: ca_goodi ! - goodness indices -
!
! Interfaces
!
! Generic interfaces
   Interface read_labels
      Module Procedure read_label
      Module Procedure read_labels1
      Module Procedure read_label0
      Module Procedure read_label1
      Module Procedure read_label2
   End Interface read_labels
!
Contains
!
!
 Subroutine init_labels (ilang, ddir, ifail)
!
! Initialises GUI labels
!
! Modules
#if GUI == 1
   Use clrwin$,           Only: enable_utf8$
#endif
   Use data_io_constants, Only: iin
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ilang ! - language identifier -
!
   Character(Len=*), Intent(In) :: ddir ! - data directory -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Scalars
   Integer :: il ! - language index -
!
! Executable Statements
!
! Initialise blank label
   l_lab%c = ' '
   l_lab%iak = 0
!
! Enable UTF-8 encoding
#if GUI == 1
   Call enable_utf8$ (1)
#endif
!
! Open labels file
   Open (Unit=iin, File=ddir//'labels.txt', &
         Access='sequential', Action='read', Form='formatted', IOstat=ifail, Status='old')
   Select Case (ifail)
    Case (0)
      ifail = 0
    Case (128)
      ifail = 1
    Case (134)
      ifail = 2
    Case Default
      ifail = 3
   End Select
   If (ifail /= 0) Return
!
! Skip to requested language
   If (ilang > 1) Then
      Do il = 1, ilang-1
         Read (Unit=iin, Fmt=*)
      End Do
   End If
!
! Read menu labels
! - scalars -
   Call read_labels (iin, ilang, 'l_abort', l_abort, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_about', l_about, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_actions', l_actions, ifail)
   If (ifail /= 0) GoTo 1
!
  Call read_labels (iin, ilang, 'l_advanced', l_advanced, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_analysis', l_analysis, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_applyall', l_applyall, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_applyalls', l_applyalls, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_attribs', l_attribs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_averages', l_averages, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_axislims', l_axislims, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_axislimsx', l_axislimsx, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_axislimsy', l_axislimsy, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_beginner', l_beginner, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_boot', l_boot, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_broken', l_broken, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_bw', l_bw, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_calculate', l_calculate, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_cancel', l_cancel, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_catshade', l_catshade, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_ccamaps', l_ccamaps, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_ccaopts', l_ccaopts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_ccas', l_ccas, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_ccax', l_ccax, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_ccay', l_ccay, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_climatol', l_climatol, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_climmaps', l_climmaps, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_climper', l_climper, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_close', l_close, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_config', l_config, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_console', l_console, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_contourlims', l_contourlims, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_correls', l_correls, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_crosses', l_crosses, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_ctables', l_ctables, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_cumulat', l_cumulat, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_custom', l_custom, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_cvars', l_cvars, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_cvn', l_cvn, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_cvn2', l_cvn2, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_cvopts', l_cvopts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_data', l_data, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_datdom', l_datdom, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_datlimits', l_datlimits, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_defaults', l_defaults, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_diagnostics', l_diagnostics, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_download', l_download, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_drymask', l_drymask, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_edit', l_edit, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_ensave', l_ensave, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_ensembles', l_ensembles, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_enso', l_enso, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_errhand', l_errhand, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_exceedps', l_exceedps, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_exclude', l_exclude, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_exit', l_exit, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_expert', l_expert, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_expertlev', l_expertlev, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_export', l_export, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_fcast', l_fcast, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_fcasts', l_fcasts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_fcastsets', l_fcastsets, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_file', l_file, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_fntscal', l_fntscal, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_gcmopts', l_gcmopts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_gcmouts', l_gcmouts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_goodness', l_goodness, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_gph', l_gph, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_graphics', l_graphics, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_graphscal', l_graphscal, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_help', l_help, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_hindplot', l_hindplot, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_lang', l_lang, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_linkfs', l_linkfs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_loads', l_loads, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_loadsx', l_loadsx, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_loadsy', l_loadsy, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_logaxis', l_logaxis, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_maps', l_maps, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_masklake', l_masklake, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_maskland', l_maskland, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_masksets', l_masksets, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_merge', l_merge, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_missings', l_missings, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_models', l_models, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_modeopts', l_modeopts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_modeoptsx', l_modeoptsx, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_modeoptsy', l_modeoptsy, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_modes', l_modes, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_montage', l_montage, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_mslp', l_mslp, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_new', l_new, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_next', l_next, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_no', l_no, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_obs', l_obs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_odds', l_odds, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_oddsrel', l_oddsrel, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_open', l_open, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_options', l_options, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_outres', l_outres, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_performs', l_performs, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_pintervals', l_pintervals, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_plevs', l_plevs, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 0
   Call read_labels (iin, ilang, 'l_printmenus', l_printmenus, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_prcp', l_prcp, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_predictandy', l_predictandy, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_predictorx', l_predictorx, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_predictorz', l_predictorz, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_probc', l_probc, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_probs', l_probs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_pvalues', l_pvalues, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_reanalyses', l_reanalyses, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_redrawmap', l_redrawmap, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_regress', l_regress, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_regrid', l_regrid, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_regrmods', l_regrmods, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_regropts', l_regropts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_resamples', l_resamples, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_resdefs', l_resdefs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_reset', l_reset, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_residual', l_residual, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_retro', l_retro, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_reversecols', l_reversecols, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_rhitsdiag', l_rhitsdiag, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_rocdiag', l_rocdiag, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_sameprev', l_sameprev, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_save', l_save, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_saveas', l_saveas, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_savegi', l_savegi, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_savepar', l_savepar, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_saveres', l_saveres, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_saveroc', l_saveroc, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_scatter', l_scatter, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_scatters', l_scatters, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_scores', l_scores, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_scoresp', l_scoresp, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_scorgrphs', l_scorgrphs, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_screes', l_screes, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_screex', l_screex, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_screey', l_screey, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_selsubdom', l_selsubdom, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_series', l_series, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_shadeland', l_shadeland, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_shadesea', l_shadesea, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_showopts', l_showopts, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_skillmaps', l_skillmaps, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_skillmask', l_skillmask, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_sortens', l_sortens, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_sst', l_sst, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_startat', l_startat, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_stdevs', l_stdevs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_stdn', l_stdn, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_stnmrks', l_stnmrks, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_synchps', l_synchps, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tailor', l_tailor, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tenddiag', l_tenddiag, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_threshs', l_threshs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_timesets', l_timesets, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_title', l_title, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tmax', l_tmax, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tmean', l_tmean, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tmin', l_tmin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tools', l_tools, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_transforms', l_transforms, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_transformy', l_transformy, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_tseason', l_tseason, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_txtscal', l_txtscal, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_u', l_u, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_update', l_update, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_uv', l_uv, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_v', l_v, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_validn', l_validn, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_values', l_values, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_verifn', l_verifn, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_verifsets', l_verifsets, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_verify', l_verify, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_view', l_view, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_viewgi', l_viewgi, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'l_vlines', l_vlines, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'l_wetdays', l_wetdays, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_wrlt', l_wrlt, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_yes', l_yes, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'l_zerob', l_zerob, ifail)
   If (ifail /= 0) GoTo 1
!
! - arrays -
   Call read_labels (iin, ilang, 'ls_analyses', na, ls_analyses(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ls_cats', ng+1, ls_cats(0:), ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'ls_graphfmts', ngf, ls_graphfmts(:), ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'ls_threshs', nts, ls_threshs(:), ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 0
   Call read_labels (iin, ilang, 'ls_uerropts', nueo, ls_uerropts(:), ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'ls_wrlts', nwr, ls_wrlts(:), ifail)
   If (ifail /= 0) GoTo 1
!
! Read text labels
! - arrays -
   Call read_labels (iin, ilang, 'ca_atypes_a', na, ca_atypes_a(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_bss', ng+1, 2, ca_bss(0:,:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_cat_a', ng, ca_cat_a(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_corrects', ngc, ca_corrects(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_corrs', ncorr, ca_corrs(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_dstruct', nstruct+1, ca_dstruct(0:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_dstruct_l', nstruct+1, ca_dstruct_l(0:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_elevels', nel, ca_elevels(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_evs', nev, ca_evs(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_ftype', nstruct, ca_ftype(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_ic', nic, ca_ic(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_interps', ninterp, ca_interps(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_links', nlink, ca_links(:), ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'ca_markers', nmrk, ca_markers(:), ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'ca_mcs', nmcs, ca_mcs(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_pavscores_t', npall, ca_pavscores_t(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_pccos', npo, ca_pccos(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_pcvscores_t', npcat, ca_pcvscores_t(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_regrs', nregr, ca_regrs(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_scores', nscore, ca_scores(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_seq', nsq+1, ca_seq(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_seq_l', nsq+1, ca_seq_l(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_seqs', nsq+1, ca_seqs(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_seqs_l', nsq+1, ca_seqs_l(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_ssnagg', nsag, ca_ssnagg(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_stds_t', nstd, ca_stds_t(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_thrs', nthr, ca_thrs(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_times', ntime, ca_times(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'ca_tos', nto, ca_tos(:), ifail)
   If (ifail /= 0) GoTo 1
!
! - scalars -
   Call read_labels (iin, ilang, 'cg_advisory', cg_advisory, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_and_l', cg_and_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_approxdlb', cg_approxdlb, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_attrib_t', cg_attrib_t, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'cg_black_l', cg_black_l, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'cg_browse_l', cg_browse_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_cancor_t', cg_cancor_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_ccacopts_t', cg_ccacopts_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_conflev', cg_conflev, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_correl', cg_correl, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_cselection', cg_cselection, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_data1', cg_data1, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_datan', cg_datan, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_date1', cg_date1, ifail)
   If (ifail == 1) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_daten', cg_daten, ifail)
   If (ifail == 1) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_domain', cg_domain, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_domlims', cg_domlims, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_done', cg_done, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_empirical', cg_empirical, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_ensemble', cg_ensemble, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_error', cg_error, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_errors', cg_errors, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_fcastprobs', cg_fcastprobs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_field', cg_field, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_fields_l', cg_fields_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_filename', cg_filename, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_fpround', cg_fpround, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'cg_green_l', cg_green_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_grey_l', cg_grey_l, ifail)
   If (ifail /= 0) GoTo 1
!
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
#endif
   Call read_labels (iin, ilang, 'cg_hinds', cg_hinds, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_indata_t', cg_indata_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_infiles_t', cg_infiles_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_initin_l', cg_initin_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_interpn', cg_interpn(:), ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lagfield', cg_lagfield, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lags_l', cg_lags_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lat', cg_lat, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_latnmost', cg_latnmost, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_latsmost', cg_latsmost, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lcv', cg_lcv, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lencp', cg_lencp, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lenitp', cg_lenitp, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lenspi', cg_lenspi, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lentp', cg_lentp, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_limits', cg_limits, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_linkf', cg_linkf, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lng', cg_lng, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lngemost', cg_lngemost, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_lngwmost', cg_lngwmost, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_maxnof', cg_maxnof, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_minnof', cg_minnof, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_missmethod', cg_missmethod, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_missvalflag', cg_missvalflag, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_mode', cg_mode, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_modecopts_t', cg_modecopts_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_model', cg_model, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_modelcomb', cg_modelcomb, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_mxpmissdat', cg_mxpmissdat, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_na', cg_na, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_neglatlng', cg_neglatlng, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_nino_t', cg_nino_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_nnearns', cg_nnearns, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_nof', cg_nof, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_nofsave', cg_nofsave, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_none', cg_none, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_none_l', cg_none_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_nused', cg_nused, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_obs', cg_obs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_output', cg_output, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_password_l', cg_password_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_perms', cg_perms, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_persist', cg_persist, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_pinterval_t', cg_pinterval_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_progress', cg_progress, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_pseason', cg_pseason, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_labels (iin, ilang, 'cg_red_l', cg_red_l, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'cg_ref', cg_ref, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_regreqs', cg_regreqs, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_roc_t', cg_roc_t, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_rootdir', cg_rootdir, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_runtime_l', cg_runtime_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_score', cg_score, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_screen', cg_screen, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_seasonlims', cg_seasonlims, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_seasons_l', cg_seasons_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_selmethod', cg_selmethod, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_seloption', cg_seloption, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_stddata', cg_stddata, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_theoretical', cg_theoretical, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_thresh', cg_thresh, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_threshslow', cg_threshslow, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_threshsupp', cg_threshsupp, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_threshtype', cg_threshtype, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_time', cg_time, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_timeseq', cg_timeseq, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_to_l', cg_to_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_tperiod', cg_tperiod, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_tperiodui', cg_tperiodui, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_traindata', cg_traindata, ifail)
   If (ifail /= 0) GoTo 1
!
#if GUI == 1
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_skip (iin, ifail)
   If (ifail /= 0) GoTo 1
#else
   Call read_labels (iin, ilang, 'cg_turnoff_l', cg_turnoff_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_turnon_l', cg_turnon_l, ifail)
   If (ifail /= 0) GoTo 1
#endif
!
   Call read_labels (iin, ilang, 'cg_undefined_l', cg_undefined_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_uvars_l', cg_uvars_l, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_valmax', cg_valmax, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_valmin', cg_valmin, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_version', cg_version, ifail)
   If (ifail /= 0) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_warning', cg_warning, ifail)
   If (ifail == 1) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_year1', cg_year1, ifail)
   If (ifail == 1) GoTo 1
!
   Call read_labels (iin, ilang, 'cg_yearn', cg_yearn, ifail)
   If (ifail == 1) GoTo 1
   ifail = 0
!
! Construct label sets
! - goodness indices -
  ca_goodi(1:ncorr) = ca_corrs(:)
  ca_goodi(ncorr+1:ncorr+nic) = ca_ic(:)
!
! Close file
1  Close (Unit=iin)
   If (ifail /= 0) ifail = ifail + 3
!
   Return
 End Subroutine init_labels
!
!
!
 Subroutine read_label (iin, ilang, ctag, lab, ifail)
!
! Reads a single label
!
! Modules
   Use data_text, Only: ca_lang_a
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input unit number -
   Integer, Intent(In) :: ilang ! - language identifier -
!
   Character(Len=*), Intent(In) :: ctag ! - label tag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Type(label), Intent(Out) :: lab ! - label -
!
! Locals
!
! Local scalars
   Integer :: il ! - language index -
   Integer :: i1 ! - starting point -
   Integer :: i2 ! - accelerator key position separator -
!
   Character(Len=2*llab6) :: cstring ! - current input string -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
! Read input
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cstring
! - error if current tag is not found -
   If (Index(cstring, Trim(ctag)) == 0) GoTo 1
! - error if current language is not found -
   If (Index(cstring, '.'//ca_lang_a(ilang)) /= Len_Trim(ctag)+1) GoTo 1 ! FTN95 BUG reading UNICODE
!
! Read label for requested language
   i1 = Index(cstring, "=") + 1
   i2 = Index(cstring, ";") + 1
   Read (Unit=cstring(i1:), Fmt=*, Err=1) lab%c
   Read (Unit=cstring(i2:), Fmt=*, Err=1) lab%iak
!
! Skip unused languages
   Do il = 1, nlang-1
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
   Return
!
2  ifail = 2
   Return
!
 End Subroutine read_label
!
!
!
 Subroutine read_labels1 (iin, ilang, ctag, n, lab, ifail)
!
! Reads a one-dimensional array of labels
!
! Modules
   Use data_text, Only: ca_lang_a
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input unit number -
   Integer, Intent(In) :: ilang ! - language identifier -
   Integer, Intent(In) :: n     ! - number of labels -
!
   Character(Len=*), Intent(In) :: ctag ! - label tag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Type(label), Dimension(:), Intent(Out) :: lab ! - label -
!
! Locals
!
! Local scalars
   Integer :: il ! - language index -
   Integer :: i  ! - label index -
   Integer :: i1 ! - starting point -
   Integer :: i2 ! - accelerator key position separator -
!
   Character(Len=8*llab6) :: cstring ! - current input string -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
! Read input
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cstring
! - error if current tag is not found -
   If (Index(cstring, Trim(ctag)) == 0) GoTo 1
! - error if current language is not found -
   If (Index(cstring, '.'//ca_lang_a(ilang)) /= Len_Trim(ctag)+1) GoTo 1
!
! Read labels for requested language
   Do i = 1, n
      If (i == 1) Then
         i1 = Index(cstring, "=") + 1
      Else
         i1 = Index(cstring, "'")
      End if
      i2 = Index(cstring, "';") + 2
      Read (Unit=cstring(i1:), Fmt=*, Err=1) lab(i)%c
      Read (Unit=cstring(i2:), Fmt=*, Err=1) lab(i)%iak
      If (i < n) cstring = cstring(i2+2:)
   End Do
!
! Skip unused languages
   Do il = 1, nlang-1
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
   Return
!
2  ifail = 2
   Return
!
 End Subroutine read_labels1
!
!
!
 Subroutine read_label0 (iin, ilang, ctag, clabel, ifail)
!
! Reads a single label
!
! Modules
   Use data_text, Only: ca_lang_a
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input unit number -
   Integer, Intent(In) :: ilang ! - language identifier -
!
   Character(Len=*), Intent(In) :: ctag ! - label tag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: clabel ! - label tag -
!
! Locals
!
! Local scalars
   Integer :: il ! - language index -
   Integer :: i1 ! - starting point -
!
   Character(Len=512) :: cstring ! - current input string -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
! Read labels for requested language
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cstring
! - error if current tag is not found -
   If (Index(cstring, Trim(ctag)) == 0) GoTo 1
! - error if current language is not found -
   If (Index(cstring, '.'//ca_lang_a(ilang)) /= Len_Trim(ctag)+1) GoTo 1
   i1 = Index(cstring, "=") + 1
   Read (Unit=cstring(i1:), Fmt=*, Err=1) clabel
!
! Skip unused languages
   Do il = 1, nlang-1
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
   Return
!
2  ifail = 2
   Return
!
 End Subroutine read_label0
!
!
!
 Subroutine read_label1 (iin, ilang, ctag, n, clabel, ifail)
!
! Reads a one-dimensional array of labels
!
! Modules
   Use data_text, Only: ca_lang_a
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input unit number -
   Integer, Intent(In) :: ilang ! - language identifier -
   Integer, Intent(In) :: n     ! - number of labels -
!
   Character(Len=*), Intent(In) :: ctag ! - label tag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Character(Len=*), Dimension(:), Intent(Out) :: clabel ! - label tag -
!
! Locals
!
! Local scalars
   Integer :: il ! - language index -
   Integer :: i1 ! - starting point -
   Integer :: i  ! - label index -
!
   Character(Len=1024) :: cstring ! - current input string -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
! Read labels for each language
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cstring
! - error if current tag is not found -
   If (Index(cstring, Trim(ctag)) == 0) GoTo 1
! - error if current language is not found -
   If (Index(cstring, '.'//ca_lang_a(ilang)) /= Len_Trim(ctag)+1) GoTo 1
   i1 = Index(cstring, "=") + 1
   Read (Unit=cstring(i1:), Fmt=*, Err=1) (clabel(i), i=1,n)
!
! Skip unused languages
   Do il = 1, nlang-1
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
   Return
!
2  ifail = 2
   Return
!
 End Subroutine read_label1
!
!
!
 Subroutine read_label2 (iin, ilang, ctag, n1, n2, clabel, ifail)
!
! Reads a two-dimensional array of labels
!
! Modules
   Use data_text, Only: ca_lang_a
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input unit number -
   Integer, Intent(In) :: ilang ! - language identifier -
   Integer, Intent(In) :: n1    ! - number of labels -
   Integer, Intent(In) :: n2    ! - number of labels -
!
   Character(Len=*), Intent(In) :: ctag ! - label tag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Character(Len=*), Dimension(:,:), Intent(Out) :: clabel ! - label tag -
!
! Locals
!
! Local scalars
   Integer :: il ! - language index -
   Integer :: i1 ! - starting point -
   Integer :: i  ! - label index -
   Integer :: j  ! - label index -
!
   Character(Len=1024) :: cstring ! - current input string -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
! Read labels for each language
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cstring
! - error if current tag is not found -
   If (Index(cstring, Trim(ctag)) == 0) GoTo 1
! - error if current language is not found -
   If (Index(cstring, '.'//ca_lang_a(ilang)) /= Len_Trim(ctag)+1) GoTo 1
   i1 = Index(cstring, "=") + 1
   Read (Unit=cstring(i1:), Fmt=*, Err=1) ((clabel(j,i), j=1,n1), i=1,n2)
!
! Skip unused languages
   Do il =1, nlang-1
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
   Return
!
2  ifail = 2
   Return
!
 End Subroutine read_label2
!
!
!
 Subroutine read_skip (iin, ifail)
!
! Skips a line
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: il ! - index -
!
! Executable Statements
!
! Skip unused languages
   Do il = 1, nlang
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
   Return
!
2  ifail = 2
   Return
!
 End Subroutine read_skip
!
!
!
 Subroutine upcase (c)
!
! Converts text to upper case
!
! Modules
   Use data_text, Only: c_cyrillic_l, c_cyrillic_u, c_letters_l, c_letters_u, ialphabet, ilang, i_cyrillic
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: c ! - text -
!
! Locals
!
! Local scalars
   Integer :: i ! - letter counter -
   Integer :: j ! - letter identifier -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim
!
! Exectuable Statements
!
! Convert to uppercase
   Select Case (ialphabet(ilang))
    Case Default ! - Latin alphabet -
      Do i = 1, Len_Trim(c)
         j = Index(c_letters_l, c(i:i))
         If (j > 0) c(i:i) = c_letters_u(j:j)
      End Do
    Case (i_cyrillic) ! - Cyrillic alphabet -
      Do i = 1, Len_Trim(c)
         j = Index(c_cyrillic_l, c(i:i))
         If (j > 0) c(i:i) = c_cyrillic_u(j:j)
      End Do
   End Select
!
   Return
 End Subroutine upcase
!
!
!
 Subroutine lowcase (c)
!
! Converts text to lower case
!
! Modules
   Use data_text, Only: c_cyrillic_l, c_cyrillic_u, c_letters_l, c_letters_u, ialphabet, ilang, i_cyrillic
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: c ! - text -
!
! Locals
!
! Local scalars
   Integer :: i ! - letter counter -
   Integer :: j ! - letter identifier -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim
!
! Exectuable Statements
!
! Convert to uppercase
   Select Case (ialphabet(ilang))
    Case Default ! - Latin alphabet -
      Do i = 1, Len_Trim(c)
         j = Index(c_letters_u, c(i:i))
         If (j > 0) c(i:i) = c_letters_l(j:j)
      End Do
    Case (i_cyrillic) ! - Cyrillic alphabet -
      Do i = 1, Len_Trim(c)
         j = Index(c_cyrillic_u, c(i:i))
         If (j > 0) c(i:i) = c_cyrillic_l(j:j)
      End Do
   End Select
!
   Return
 End Subroutine lowcase
!
!
!
 Function make_label(labfmt, lab, &
          lak) &
          Result (c_lab)
!
! Constructs a label
!
! Modules
   Use data_text, Only: c_akey
!
! Function result
   Character(Len=llab6) :: c_lab
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=1), Intent(In) :: labfmt ! - label format; 'T' title, 'S' sentence, 'L' lower case, 'U' upper case-
!
   Type(label), Intent(In) :: lab ! - label -
!
! Optional arguments
! - optional input scalars -
   Logical, Intent(In), Optional :: lak ! - add accelerator key? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Construct label
   Select Case (labfmt)
! - title -
    Case ('T', 't')
      c_lab = lab%c
! - sentence -
    Case ('S', 's')
      c_lab = lab%c
      Call lowcase (c_lab(2:))
! - upper case -
    Case ('U', 'u')
      c_lab = lab%c
      Call upcase (c_lab)
! - lower case -
    Case ('L', 'l')
      c_lab = lab%c
      Call lowcase (c_lab)
   End Select
!
! Add accelerator key
   If (Present(lak)) Then
      If (lak) Then
         Select Case (lab%iak)
          Case(1)
            c_lab = TRIM(c_akey)//Trim(c_lab)
          Case (2:)
            c_lab = c_lab(1:lab%iak-1)//Trim(c_akey)//Trim(c_lab(lab%iak:))
         End Select
      End If
   End If
!
   Return
 End Function make_label
End Module labels
