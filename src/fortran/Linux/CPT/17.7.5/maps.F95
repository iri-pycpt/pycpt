! Author: Simon Mason
Module maps
!
! Modules
   Use data_numbers, Only: rp
#if GUI == 1
   Use graphics,     Only: image
#endif
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: map_averages, map_correls, map_cvars, map_pscores, map_skill_cv, map_skill_c2, map_skill_ra, map_stdevs, &
             map_stdx, map_stdy, map_stdz, map_thresholds
#if GUI == 1
   Public :: init_img_maps
!
! Scalars
!
! Integer scalars
   Integer, Private :: ioutm ! - correlation maps output unit number -
!
! Arrays
!
! Derived-type arrays
   Type(image), Dimension(1), Private :: img_aves ! - averages -
   Type(image), Dimension(1), Private :: img_cors ! - correlations -
   Type(image), Dimension(1), Private :: img_cvar ! - coefficients of variation -
   Type(image), Dimension(1), Private :: img_cvsk ! - cross-validated skill map -
   Type(image), Dimension(1), Private :: img_c2sk ! - double cross-validated skill map -
   Type(image), Dimension(1), Private :: img_prss ! - probabilistic scores map -
   Type(image), Dimension(1), Private :: img_rask ! - retroactive skill map -
   Type(image), Dimension(1), Private :: img_sdev ! - standard deviation -
   Type(image), Dimension(1), Private :: img_stdx ! - standardized X anomalies -
   Type(image), Dimension(1), Private :: img_stdy ! - standardized Y anomalies -
   Type(image), Dimension(1), Private :: img_stdz ! - standardized Z anomalies -
!
   Type(image), Dimension(2), Private :: img_thrs ! - upper (1) and lower (2) thresholds -
#endif
!
Contains
!
!
#if GUI == 1
 Subroutine init_img_maps (igsize)
!
! Initialises map graphics
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
! Initialise images
   Call init_image (img_aves(1), (igsize*3)/2)
   Call init_image (img_cors(1), (igsize*3)/2)
   Call init_image (img_cvsk(1), (igsize*3)/2)
   Call init_image (img_c2sk(1), (igsize*3)/2)
   Call init_image (img_cvar(1), (igsize*3)/2)
   Call init_image (img_prss(1), (igsize*3)/2)
   Call init_image (img_rask(1), (igsize*3)/2)
   Call init_image (img_sdev(1), (igsize*3)/2)
   Call init_image (img_stdx(1), (igsize*3)/2)
   Call init_image (img_stdy(1), (igsize*3)/2)
   Call init_image (img_stdz(1), (igsize*3)/2)
   Call init_image (img_thrs(1), (igsize*3)/2)
   Call init_image (img_thrs(2), (igsize*3)/2)
!
   Return
 End Subroutine init_img_maps
!
!
!
#endif
 Function map_averages() &
          Result (maps)
!
#if GUI == 1
! Creates window for a climatological average map
#else
! Saves climatological average map data
#endif
!
! Modules
#if GUI == 1
   Use labels,      Only: l_lab, &
                          l_averages, l_contourlims
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#else
   Use iofiles,     Only: avfile
   Use cpt_output,  Only: save_results
   Use analysis,    Only: ianal
#endif
!
! Function result
   Integer :: maps
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Reset image
   img_aves(:)%title = Trim(l_averages%c)
!
! Open results window
   maps = init_window_graphic(1, img_aves(:), imgid, l_averages%c, [l_lab], plot_aves, title_maps, export_maps, &
          cxy='Y', limits_grph=limits_maps, llims=[l_contourlims], f_help=help_maps)
!
#else
! Prompt for output file
   maps = save_results(ianal, avfile)
!
#endif
   Return
 End Function map_averages
!
!
!
 Function map_cvars() &
          Result (maps)
!
#if GUI == 1
! Creates window for a climatological coefficients of variation map
#else
! Saves climatological coefficients of variation map data
#endif
!
! Modules
#if GUI == 1
   Use labels,      Only: l_lab, &
                          l_cvars, l_contourlims
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#else
   Use iofiles,     Only: cvfile
   Use cpt_output,  Only: save_results
   Use analysis,    Only: ianal
#endif
!
! Function result
   Integer :: maps
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Reset image
   img_cvar(:)%title = Trim(l_cvars%c)
!
! Open results window
   maps = init_window_graphic(1, img_cvar(:), imgid, l_cvars%c, [l_lab], plot_cvar, title_maps, export_maps, &
          cxy='Y', limits_grph=limits_maps, llims=[l_contourlims], f_help=help_maps)
!
#else
! Prompt for output file
   maps = save_results(ianal, cvfile)
!
#endif
   Return
 End Function map_cvars
!
!
!
 Function map_stdevs() &
          Result (maps)
!
#if GUI == 1
! Creates window for a climatological standard deviations map
#else
! Saves climatological standard deviations map data
#endif
!
! Modules
#if GUI == 1
   Use labels,      Only: l_lab, &
                          l_stdevs, l_contourlims
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#else
   Use iofiles,     Only: sdfile
   Use cpt_output,  Only: save_results
   Use analysis,    Only: ianal
#endif
!
! Function result
   Integer :: maps
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Reset image
   img_sdev(:)%title = Trim(l_stdevs%c)
!
! Open results window
   maps = init_window_graphic(1, img_sdev(:), imgid, l_stdevs%c, [l_lab], plot_sdev, title_maps, export_maps, &
          cxy='Y', limits_grph=limits_maps, llims=[l_contourlims], f_help=help_maps)
!
#else
! Prompt for output file
   maps = save_results(ianal, sdfile)
!
#endif
   Return
 End Function map_stdevs
!
!
!
 Function map_stdx() &
          Result (maps)
!
#if GUI == 1
! Creates window for standardized X data maps
#else
! Saves standardized X data
#endif
!
! Modules
#if GUI == 1
   Use labels,  Only: l_predictorx
#else
   Use iofiles, Only: vxfile
#endif
!
! Function result
   Integer :: maps
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Create window for standardized X data maps
   maps = map_std('X', Trim(l_predictorx%c), img_stdx(:), plot_stdx, prompt_itx)
#else
! Saves standardized X data
   maps = map_std(vxfile, plot_stdx, prompt_itx)
#endif
!
   Return
 End Function map_stdx
!
!
!
 Function map_stdy() &
          Result (maps)
!
#if GUI == 1
! Creates window for standardized Y data maps
#else
! Saves standardized Y data
#endif
!
! Modules
#if GUI == 1
   Use labels,  Only: l_predictandy
#else
   Use iofiles, Only: vyfile
#endif
!
! Function result
   Integer :: maps
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Create window for standardized Y data maps
   maps = map_std('Y', Trim(l_predictandy%c), img_stdy(:), plot_stdy, prompt_ity)
#else
! Saves standardized Y data
   maps = map_std(vyfile, plot_stdy, prompt_ity)
#endif
!
   Return
 End Function map_stdy
!
!
!
 Function map_stdz() &
          Result (maps)
!
#if GUI == 1
! Creates window for standardized Z data maps
#else
! Saves standardized Z data
#endif
!
! Modules
#if GUI == 1
   Use labels,     Only: l_predictorz
#else
   Use iofiles,    Only: vzfile
#endif
   Use prediction, Only: ifm
   Use forecast,   Only: prompt_forecast
!
! Function result
   Integer :: maps
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Create window for standardized Z data maps
   maps = map_std('X', Trim(l_predictorz%c), img_stdz(:), plot_stdz, prompt_forecast, &
          ifm=ifm)
#else
! Saves standardized Z data
   maps = map_std(vzfile, plot_stdz, prompt_forecast, &
          ifm=ifm)
#endif
!
   Return
 End Function map_stdz
!
!
!
#if GUI == 1
 Function map_std(cxy, ctitle, img, plot_std, prompt_dat, &
          ifm) &
          Result (maps)
!
! Creates window for standardized data maps
#else
 Function map_std(vfile, plot_std, prompt_dat, &
          ifm) &
          Result (maps)
!
! Saves standardized data
#endif
!
! Modules
#if GUI == 1
   Use labels,      Only: l_lab, &
                          l_contourlims
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#else
   Use iofiles,     Only: ofile
   Use cpt_output,  Only: save_results
   Use analysis,    Only: ianal
#endif
!
! Function result
   Integer :: maps
!
! Arguments
!
! Dummy arguments
#if GUI == 1
! - input scalars -
! - optional input scalars -
   Character(Len=*), Intent(In) :: cxy    ! - X/Y -
   Character(Len=*), Intent(In) :: ctitle ! - title -
!
! - input/output arrays -
   Type(image), Dimension(:), Intent(InOut) :: img
#else
! - input/output scalars -
   Type(ofile), Intent(InOut) :: vfile
#endif
!
! - procedure arguments -
   Integer, External :: plot_std
   Integer, External :: prompt_dat
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ifm ! - index of current forecast -
!
! Executable Statements
!
#if GUI == 1
! Set image title
   img(:)%title = ctitle
!
! Open results window
   maps = init_window_graphic(1, img(:), imgid, ctitle, [l_lab], plot_std, title_maps, export_maps, &
          cxy=cxy, extra_prompt=prompt_dat, limits_grph=limits_maps, llims=[l_contourlims], f_help=help_maps)
!
#else
! Prompt for date
   maps = prompt_dat(plot_std) ! - plot_std is an unused argument -
!
! Standardise
   maps = plot_std()
!
! Prompt for output file
   maps = save_results(ianal, vfile, &
          ifm=ifm)
!
#endif
   Return
 End Function map_std
!
!
!
 Function map_thresholds() &
          Result (maps)
!
#if GUI == 1
! Creates window for a climatological thresholds map
#else
! Saves climatological thresholds map data
#endif
!
! Modules
#if GUI == 1
   Use labels,      Only: l_contourlims, l_threshs, &
                          ls_threshs
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#else
   Use iofiles,     Only: thfile
   Use cpt_output,  Only: save_results
   Use analysis,    Only: ianal
#endif
!
! Function result
   Integer :: maps
!
! Executable Statements
!
#if GUI == 1
! Open results window
   maps = init_window_graphic(2, img_thrs(:), imgid, l_threshs%c, [ls_threshs(2), ls_threshs(1)], &
          plot_thresholds, title_maps, export_maps,                                               &
          cxy='Y', limits_grph=limits_maps, llims=[l_contourlims, l_contourlims], f_help=help_maps)
!
#else
! Prompt for output file
   maps = save_results(ianal, thfile)
!
#endif
   Return
 End Function map_thresholds
!
!
!
 Function map_correls() &
          Result (maps)
!
! Creates window for a climatological correlations map
!
! Modules
#if GUI == 1
   Use labels,             Only: l_lab, &
                                 l_contourlims, l_correls
#else
   Use data_cpt_constants, Only: ncorr
   Use labels,             Only: ca_corrs, &
                                 l_correls
   Use gui,                Only: box_close, ganged_rbs, init_win
#endif
   Use iofiles,            Only: mxa
#if GUI == 1
   Use scores,             Only: init_correls
   Use windows_gui,        Only: imgid, &
                                 init_window_graphic
#else
   Use iofiles,            Only: cofile
   Use scores,             Only: icorrs, &
                                 init_correls
   Use cpt_output,         Only: save_results
   Use analysis,           Only: ianal
#endif
!
! Function result
   Integer :: maps
!
#if GUI == 0
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
#endif
! Executable Statements
!
! Allocate memory
   maps = 1
   If (init_correls(mxa) /= 0) Return
!
! Open results window
#if GUI == 1
   maps = init_window_graphic(1, img_cors(:), imgid, l_correls%c, [l_lab], update_correls, title_maps, export_maps, &
          cxy='Y', iloc=ioutm, add_win_h=add_correls, limits_grph=limits_maps, llims=[l_contourlims], f_help=help_maps)
#else
   Call init_win (title=Trim(l_correls%c))
   icorrs(:) = 0
   Call ganged_rbs (ncorr, ca_corrs(:), icorrs(:))
! - close scores box -
   Call box_close (.true.)
!
! Plot correlation maps
   maps = update_correls()
!
! Construct and add coordinate label for current point
   maps = save_results(ianal, cofile)
#endif
!
   Return
 End Function map_correls
!
!
!
#if GUI == 1
 Function add_correls() &
          Result (adds)
!
! Adds correlations to results window
!
! Modules
   Use data_cpt_constants, Only: ncorr
   Use labels,             Only: ca_corrs, &
                                 l_scores
   Use gui,                Only: box_open, box_close,ganged_rbs, print_text
   Use fields,             Only: prompt_fields
   Use scores,             Only: icorrs
!
! Function result
   Integer :: adds
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Prompt for score
   Call box_open (.true., .false., &
        title=Trim(l_scores%c))
   Call ganged_rbs (ncorr, icorrs(:), &
        crbns=ca_corrs(:), frb=update_correls)
! - close scores box -
   Call box_close (.true.)
!
! Prompt for X field and lagged fields
   Call print_text ('  ', &
        ladvance=.false.)
   Call prompt_fields ('X', update_correls)
   adds = 2
!
   Return
 End Function add_correls
!
!
!
#endif
 Function map_skill_cv() &
          Result (maps)
!
! Function result
   Integer :: maps
!
! Executable Statements
!
! Update maps
#if GUI == 1
   maps = map_skill('Cross-validated results', img_cvsk(:), update_skill_cv, prompt_skill_cv, select_skill_cv)
#else
   maps = map_skill('Cross-validated results', update_skill_cv, prompt_skill_cv)
#endif
!
   Return
 End Function map_skill_cv
!
!
!
 Function map_skill_c2() &
          Result (maps)
!
! Function result
   Integer :: maps
!
! Executable Statements
!
! Update maps
#if GUI == 1
   maps = map_skill('Double cross-validated results', img_cvsk(:), update_skill_c2, prompt_skill_c2, select_skill_c2)
#else
   maps = map_skill('Double cross-validated results', update_skill_c2, prompt_skill_c2)
#endif
!
   Return
 End Function map_skill_c2
!
!
!
 Function map_skill_ra() &
          Result (maps)
!
! Function result
   Integer :: maps
!
! Executable Statements
!
! Update maps
#if GUI == 1
   maps = map_skill('Retroactive results', img_rask(:), update_skill_ra, prompt_skill_ra, select_skill_ra)
#else
   maps = map_skill('Retroactive results', update_skill_ra, prompt_skill_ra)
#endif
!
   Return
 End Function map_skill_ra
!
!
!
#if GUI == 1
 Function map_skill(title, img_sk, update_skill, prompt_skill, select_skill) &
          Result (maps)
#else
 Function map_skill(title, update_skill, prompt_skill) &
          Result (maps)
#endif
!
! Modules
   Use scores,    Only: init_skill
   Use bootstrap, Only: ipval
!
! Function result
   Integer :: maps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: title ! - window title -
!
#if GUI == 1
! - input/output arrays -
   Type(image), Dimension(:), Intent(InOut) :: img_sk ! - skill map -
!
! - procedure arguments -
   Integer, External :: select_skill
#endif
   Integer, External :: prompt_skill
   Integer, External :: update_skill
!
! Executable Statements
!
! Map skill scores
#if GUI == 1
   maps = map_scores(title, img_sk(:), ipval, &
          init_skill, update_skill, prompt_skill, save_skill, select_skill, &
          save_pvalues=save_pvalues)
#else
   maps = map_scores(title, ipval, init_skill, update_skill, prompt_skill)
#endif
!
   Return
 End Function map_skill
!
!
!
 Function map_pscores() &
          Result (maps)
!
! Plots probabilistic scores maps
!
! Modules
   Use labels,    Only: l_scoresp
   Use scores,    Only: init_pscores
   Use bootstrap, Only: ibcl
!
! Function result
   Integer :: maps
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Map probabilistic scores
#if GUI == 1
   maps = map_scores(Trim(l_scoresp%c), img_prss(:), ibcl, &
          init_pscores, update_pscores, prompt_pscores, save_pscores, select_pscores, &
          extra_prompt=prompt_cat)
#else
   maps = map_scores(Trim(l_scoresp%c), ibcl, init_pscores, update_pscores, prompt_pscores)
   maps = 1
#endif
!
   Return
 End Function map_pscores
!
!
!
#if GUI == 1
 Function map_scores(title, img, isig, init_scores, update_scores, prompt_scores, save_scores, select_scores, &
          save_pvalues, extra_prompt) &
          Result (maps)
#else
 Function map_scores(title, isig, init_scores, update_scores, prompt_scores) &
          Result (maps)
#endif
!
! Plots scores maps
!
! Modules
#if GUI == 1
   Use labels,      Only: l_contourlims, l_lab
#endif
   Use settings,    Only: nu, nur
   Use iofiles,     Only: mya
#if GUI == 1
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#endif
!
! Function result
   Integer :: maps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: title ! - window title -
!
! - input/output scalars -
   Integer, Intent(InOut) :: isig ! - significance test -
!
#if GUI == 1
! - input/output arrays -
   Type(image), Dimension(:), Intent(InOut) :: img ! - image -
!
#endif
! - procedure arguments -
   Integer, External :: init_scores
   Integer, External :: prompt_scores
#if GUI == 1
   Integer, External :: save_scores
   Integer, External :: select_scores
#endif
   Integer, External :: update_scores
!
#if GUI == 1
! Optional arguments
! - optional procedure arguments -
   Integer, External, Optional :: save_pvalues
   Integer, External, Optional :: extra_prompt
!
#endif
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max
!
! Executable Statements
!
! Allocate memory
   maps = 1
   If (init_scores(mya, Max(nu, nur), isig) /= 0) Return
!
#if GUI == 1
! Open results window
   maps = init_window_graphic(1, img(:), imgid, title, [l_lab], update_scores, title_maps, export_maps, &
          select_grph=select_scores, save_rslt=save_scores, save_pvals=save_pvalues, ipv=isig, cxy='Y', &
          limits_grph=limits_maps, llims=[l_contourlims], add_win_h=prompt_scores, extra_prompt=extra_prompt, &
          f_help=help_maps)
!
#else
   Print*, title
   maps = prompt_scores()
   maps = update_scores()
#endif
   Return
 End Function map_scores
!
!
!
 Function prompt_cat(f_callback) &
          Result (prompt)
!
! Prompts for category
!
! Modules
   Use data_cpt_constants, Only: ng
#if GUI == 1
   Use gui,                Only: prompt_list
   Use labels,             Only: ls_cats
#else
   Use gui,                Only: prompt_integer
#endif
   Use categories,         Only: icatc, &
                                 change_cat
!
! Function result
   Integer :: prompt
!
! Arguments
!
! Procedure arguments
   Integer, External :: f_callback ! - callback function -
!
! Executable Statements
!
! Prompt for mode
#if GUI == 1
   Call prompt_list ('Category', .true., ls_cats(1:)%c, ng, icatc, &
        itab=17, cbf1=change_cat, cbf2=f_callback)
#else
   Call prompt_integer ('Category', icatc, &
        ilow=1, ihgh=ng, itab=17, fcb1=change_cat)
#endif
   prompt = 0
!
   Return
 End Function prompt_cat
!
!
!
 Function update_correls() &
          Result (update)
!
! Updates a climatological correlations map for a new location
!
! Modules
#if GUI == 1
   Use data_numbers,       Only: one
#endif
   Use data_cpt_constants, Only: ncorr
   Use labels,             Only: ca_corrs, &
                                 cg_done
#if GUI == 1
   Use labels,             Only: cg_field, cg_lagfield
#endif
   Use gui,                Only: set_cursor_waiting
   Use settings,           Only: iva, nu
   Use arrays,             Only: x, y
   Use iofiles,            Only: mxa
#if GUI == 1
   Use iofiles,            Only: yfile
   Use fields,             Only: ilfy, ifdy, &
                                 update_grid
#endif
   Use scores,             Only: corrs, icorr, icorrs, &
                                 get_corrs
   Use season,             Only: isnc
!
! Function result
   Integer :: update
!
#if GUI == 1
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
#endif
   Intrinsic Trim
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Identify metric
   Do icorr = 1, ncorr
      If (icorrs(icorr) == 1) Exit
   End Do
!
! Calculate correlations
   Print *, 'Calculating '//Trim(ca_corrs(icorr))//' ...'
   Call get_corrs (icorr, nu, mxa, x(1:mxa,:,1), y(iva,:,isnc), corrs(:))
   Print *, Trim(cg_done)//'!'
!
#if GUI == 1
! Construct and add coordinate label for current point
   Call update_grid (ioutm)
!
! Set contours
   img_cors(:)%lcontour = .true.
   img_cors(:)%ncs = 13
   Do i = 1, img_cors(1)%ncs
      img_cors(:)%contour(i) = -0.90_rp + 0.15_rp*Real(i-1, Kind=rp)
   End Do
   img_cors(:)%ymin = -one
   img_cors(:)%ymax = one
   If (yfile%nfs > 1) Then
      If (yfile%nls > 1) Then
         Write (img_cors(1)%title, Fmt='(4A,I0,3A,I0)') Trim(ca_corrs(icorr)), '; ', Trim(cg_lagfield), ': ', ifdy, ', ', &
                                                        Trim(cg_lagfield), ': ', ilfy
      Else
         Write (img_cors(1)%title, Fmt='(4A,I0)') Trim(ca_corrs(icorr)), '; ', Trim(cg_field), ': ', ifdy
      End If
   Else If (yfile%nls > 1) Then
      Write (img_cors(1)%title, Fmt='(4A,I0)') Trim(ca_corrs(icorr)), '; ', Trim(cg_lagfield), ': ', ilfy
   Else
      img_cors(1)%title = ca_corrs(icorr)
   End If
   img_cors(:)%scale = one
!
! Plot correlations map
   update = plot_correl()
#else
   update = 0
#endif
!
   Return
 End Function update_correls
!
!
!
 Function update_skill_cv() &
          Result (update)
!
! Modules
   Use settings,   Only: nu
   Use arrays,     Only: y
   Use iofiles,    Only: mya
   Use regression, Only: ycv
   Use season,     Only: isnc
   Use categories, Only: ifor_c, iobs_c, pobs
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Plot skill
#if GUI == 1
   update = update_skills(img_cvsk(1), mya, nu, ycv(:,:,isnc), y(:,:,isnc), ifor_c(:,:), iobs_c(:,:), pobs(:,:), plot_skill_cv)
#else
   update = update_skills(mya, nu, ycv(:,:,isnc), y(:,:,isnc), ifor_c(:,:), iobs_c(:,:), pobs(:,:))
#endif
!
   Return
 End Function update_skill_cv
!
!
!
 Function update_skill_c2() &
          Result (update)
!
! Modules
   Use settings,   Only: nu
   Use arrays,     Only: y
   Use iofiles,    Only: mya
   Use regression, Only: ycv2
   Use season,     Only: isnc
   Use categories, Only: ifor_2, iobs_2, pobs
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Plot skill
#if GUI == 1
   update = update_skills(img_cvsk(1), mya, nu, ycv2(:,:,isnc), y(:,:,isnc), ifor_2(:,:), iobs_2(:,:), pobs(:,:), plot_skill_cv)
#else
   update = update_skills(mya, nu, ycv2(:,:,isnc), y(:,:,isnc), ifor_2(:,:), iobs_2(:,:), pobs(:,:))
#endif
!
   Return
 End Function update_skill_c2
!
!
!
 Function update_skill_ra() &
          Result (update)
!
! Modules
   Use settings,   Only: nu, nur, nu1
   Use arrays,     Only: y
   Use iofiles,    Only: mya
   Use regression, Only: yret
   Use season,     Only: isnc
   Use categories, Only: ifor_r, iobs_r, pobs
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Plot skill
#if GUI == 1
   update = update_skills(img_rask(1), mya, nur, yret(:,:,isnc), y(1:mya,nu1+1:nu,isnc), ifor_r(:,:), iobs_r(:,:), pobs(:,:), &
            plot_skill_ra)
#else
   update = update_skills(mya, nur, yret(:,:,isnc), y(1:mya,nu1+1:nu,isnc), ifor_r(:,:), iobs_r(:,:), pobs(:,:))
#endif
!
   Return
 End Function update_skill_ra
!
!
!
#if GUI == 1
 Function update_skills(img_sk, m, n, yhat, y, ifor, iobs, pobs, plot_skill) &
          Result (update)
#else
 Function update_skills(m, n, yhat, y, ifor, iobs, pobs) &
          Result (update)
#endif
!
! Modules
#if GUI == 1
   Use data_numbers, Only: one
#endif
   Use maths,        Only: get_flag
   Use labels,       Only: ca_scores, &
                           cg_done
   Use gui,          Only: set_cursor_waiting
   Use settings,     Only: nu, nur
   Use iofiles,      Only: mya
   Use scores,       Only: iskill, iskills, pvalues, skills, &
                           get_scores, init_skill
#if GUI == 1
   Use scores,       Only: set_dscore_range
#endif
   Use bootstrap,    Only: ipval, &
                           get_pvalues
!
! Function result
   Integer :: update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of variables -
   Integer, Intent(In) :: n ! - number of cases -
!
#if GUI == 1
! - input/output scalars -
   Type(image), Intent(InOut) :: img_sk ! - skill map -
!
#endif
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: ifor ! - estimated categories -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y    ! - observed values -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: yhat ! - estimated values -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: pobs ! - climatological probabilities -
!
#if GUI == 1
! - procedure arguments -
   Integer, External :: plot_skill ! - skill map -
!
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Max, Trim
!
! Executable Statements
!
! Set cursor
   update = 1
   Call set_cursor_waiting (1)
!
! Identify metric
   iskill = get_flag(iskills)
!
! Calculate new skill scores
   Print *, 'Calculating ', Trim(ca_scores(iskill)), ' ...'
   Call get_scores (iskill, n, m, yhat(:,:), y(:,:), ifor(:,:), iobs(:,:), pobs(:,:), skills(:))
   If (ipval == 1) Then
      If (.not.Allocated(pvalues)) Then
         If (init_skill(mya, Max(nu, nur), ipval) /= 0) Return
      End If
      Call get_pvalues (iskill, n, m, yhat(:,:), y(:,:), ifor(:,:), iobs(:,:), pobs(:,:), skills(:), pvalues(:))
   End If
   Print *, Trim(cg_done)//'!'
!
#if GUI == 1
! Set contours
   img_sk%lcontour = .true.
   img_sk%scale = one
   Call set_dscore_range (iskill, img_sk%ymin, img_sk%ymax, img_sk%ncs, img_sk%contour)
!
! Plot scores
   Call init_score_plot (img_sk, ca_scores(iskill))
   update = plot_skill()
#else
   update = save_skill()
#endif
!
   Return
 End Function update_skills
!
!
!
 Function update_pscores() &
          Result (update)
!
! Modules
#if GUI == 1
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: npall
#endif
   Use maths,              Only: get_flag
   Use labels,             Only: ca_pvs_t, &
                                 cg_done
   Use gui,                Only: set_cursor_waiting
   Use settings,           Only: ibs, icv2, iretro, nb, nvrfu
   Use iofiles,            Only: mya
   Use fields,             Only: yfield
   Use scores,             Only: ips, ipss, pscores, &
                                 get_pscores
#if GUI == 1
   Use scores,             Only: set_pscore_range
#endif
   Use categories,         Only: icatc, fps_r, fps_2, iobs_r, iobs_2, pobs
!
! Function result
   Integer :: update
!
#if GUI == 1
! Locals
!
! Local scalars
   Integer :: ipsp = 0
!
#endif
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set cursor
   Call set_cursor_waiting (1)
!
! Identify metric
   ips = get_flag(ipss)
#if GUI == 1
   If (ips == ipsp .and. ips <= npall) Then
      Call set_cursor_waiting (0)
      update = 2
      Return
   End If
   ipsp = ips
#endif
!
! Calculate new probabilistic scores
   Print *, 'Calculating ', Trim(ca_pvs_t(ips)), ' ...'
   If (iretro == 1) Then
      Call get_pscores (ips, icatc, nvrfu, mya, nb, ibs, iobs_r(:,:), fps_r(:,:,:), pobs(:,:), yfield(1)%rmiss, pscores(:))
   Else If (icv2 /= 0) Then
      Call get_pscores (ips, icatc, nvrfu, mya, nb, ibs, iobs_2(:,:), fps_2(:,:,:), pobs(:,:), yfield(1)%rmiss, pscores(:))
   End If
   Print *, Trim(cg_done)//'!'
!
#if GUI == 1
! Set contours
   img_prss%lcontour = .true.
   img_prss%scale = one
   Call set_pscore_range (ips, img_prss(1)%ymin, img_prss(1)%ymax, img_prss(1)%ncs, img_prss(1)%contour)
!
! Plot scores
   If (ips > npall) Then
      Call init_score_plot (img_prss(1), ca_pvs_t(ips), &
           icat=icatc)
   Else
      Call init_score_plot (img_prss(1), ca_pvs_t(ips))
   End If
   update = plot_pscore()
#else
   update = save_pscores()
#endif
!
   Return
 End Function update_pscores
!
!
!
#if GUI == 1
 Subroutine init_score_plot (img, ca_scores_t, &
            icat)
!
! Sets a default image title
!
! Modules
   Use labels,  Only: cg_field, cg_lagfield, &
                      ls_cats
   Use iofiles, Only: yfile
   Use fields,  Only: ilfy, ifdy
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ca_scores_t ! - score name -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: icat ! - category identifier -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Set title
   If (yfile%nfs > 1) Then
      If (yfile%nls > 1) Then
         Write (img%title, Fmt='(4A,I0,3A,I0)') Trim(ca_scores_t), '; ', Trim(cg_lagfield), ': ', ifdy, ', ', &
                                                Trim(cg_lagfield), ': ', ilfy
      Else
         Write (img%title, Fmt='(4A,I0)') Trim(ca_scores_t), '; ', Trim(cg_field), ':', ifdy
      End If
   Else If (yfile%nls > 1) Then
      Write (img%title, Fmt='(4A,I0)') Trim(ca_scores_t), '; ', Trim(cg_lagfield), ': ', ilfy
   Else
      img%title = ca_scores_t
   End If
!
! Add category
   If (Present(icat)) img%title = Trim(img%title)//' ('//Trim(ls_cats(icat)%c)//')'
!
   Return
 End Subroutine init_score_plot
!
!
!
 Function plot_aves() &
          Result (plot)
!
! Plots a climatological averages map
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use labels,             Only: l_averages
   Use gui,                Only: icrev
   Use climate,            Only: ave
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot averages map
   icrev = 1 - icrev ! - reverse colour order -
   plot = plot_ydata(img_aves(1), icol_sk, 0, 1, ave(:,1), Trim(l_averages%c))
   icrev = 1 - icrev ! - restore colour order -
!
   Return
 End Function plot_aves
!
!
!
 Function plot_cvar() &
          Result (plot)
!
! Plots a climatological coefficients of variation map
!
! Modules
   Use data_numbers,       Only: zero
   Use data_gui_constants, Only: icol_mc
   Use labels,             Only: l_cvars
   Use arrays,             Only: cvar=>rwk
   Use climate,            Only: ave, sdev
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot coefficients of variation map
   Where (ave(:,1) > zero)
      cvar(:) = sdev(:,1)/ave(:,1)
   Else Where
      cvar(:) = zero
   End Where
   plot = plot_ydata(img_cvar(1), icol_mc, 0, 1, cvar(:), Trim(l_cvars%c))
!
   Return
 End Function plot_cvar
!
!
!
 Function plot_sdev() &
          Result (plot)
!
! Plots a climatological standard deviations map
!
! Modules
   Use data_gui_constants, Only: icol_mc
   Use labels,             Only: l_stdevs
   Use climate,            Only: sdev
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot standard deviations map
   plot = plot_ydata(img_sdev(1), icol_mc, 0, 1, sdev(:,1), Trim(l_stdevs%c))
!
   Return
 End Function plot_sdev
!
!
!
#endif
 Function plot_stdx() &
          Result (plot)
!
! Plots a standardized X data map
!
! Modules
   Use labels,   Only: l_predictorx
   Use settings, Only: itx
   Use arrays,   Only: xm, xsd, x
   Use iofiles,  Only: mxa
   Use pcs,      Only: ieofx
#if GUI == 1
   Use plotting, Only: plot_xdata
#endif
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot standardized X data
#if GUI == 1
   plot = plot_std(img_stdx(1), Trim(l_predictorx%c), mxa, ieofx, x(1:mxa,itx,1), xm(1:mxa), xsd(1:mxa), plot_xdata)
#else
   plot = plot_std(Trim(l_predictorx%c), mxa, ieofx, x(1:mxa,itx,1), xm(1:mxa), xsd(1:mxa))
#endif
!
   Return
 End Function plot_stdx
!
!
!
 Function plot_stdy() &
          Result (plot)
!
! Plots a standardized Y data map
!
! Modules
   Use labels,   Only: l_predictandy
   Use settings, Only: ity
   Use arrays,   Only: ym, ysd, y
   Use iofiles,  Only: mya
   Use pcs,      Only: ieofy
#if GUI == 1
   Use plotting, Only: plot_ydata
#endif
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot standardized Y data
#if GUI == 1
   plot = plot_std(img_stdy(1), Trim(l_predictandy%c), mya, ieofy, y(1:mya,ity,1), ym(1:mya), ysd(1:mya), plot_ydata)
#else
   plot = plot_std(Trim(l_predictandy%c), mya, ieofy, y(1:mya,ity,1), ym(1:mya), ysd(1:mya))
#endif
!
   Return
 End Function plot_stdy
!
!
!
 Function plot_stdz() &
          Result (plot)
!
! Plots a standardized Z data map
!
! Modules
   Use labels,     Only: l_predictorz
   Use arrays,     Only: xm, xsd, z
   Use iofiles,    Only: mxa
   Use pcs,        Only: ieofx
   Use prediction, Only: ifm
#if GUI == 1
   Use plotting,   Only: plot_xdata
#endif
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot standardized Z data
#if GUI == 1
   plot = plot_std(img_stdz(1), Trim(l_predictorz%c), mxa, ieofx, z(1:mxa,ifm,1), xm(1:mxa), xsd(1:mxa), plot_xdata)
#else
   plot = plot_std(Trim(l_predictorz%c), mxa, ieofx, z(1:mxa,ifm,1), xm(1:mxa), xsd(1:mxa))
#endif
!
   Return
 End Function plot_stdz
!
!
!
#if GUI == 1
 Function plot_std(img, ctitle, m, ieof, v, vave, vsd, plot_vdata) &
#else
 Function plot_std(ctitle, m, ieof, v, vave, vsd) &
#endif
          Result (plot)
!
! Plots a standardized data map
!
! Modules
#if GUI == 1
   Use data_gui_constants, Only: icol_sk
#endif
   Use arrays,             Only: vs=>rwk2
   Use distribs,           Only: stdize
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m    ! - number of locations -
   Integer, Intent(In) :: ieof ! - EOF standardization -
!
   Character(Len=*), Intent(In) :: ctitle ! - title -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: vave ! - climatological averages -
   Real(Kind=rp), Dimension(:), Intent(In) :: vsd  ! - climatological standard deviations -
   Real(Kind=rp), Dimension(:), Intent(In) :: v    ! - data to be standardized -
!
#if GUI == 1
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! - procedure arguments -
   Interface
      Function plot_vdata(img, icols, ioff, ncb, x, c, &
               vlow, vhgh, vmin, vmax) Result (plot)
         Integer :: plot
         Integer, Intent(In) :: icols, ioff, ncb
         Character(Len=*), Intent(In) :: c
         Real(Kind=rp), Dimension(:), Intent(In) :: x
         Type(image), Intent(InOut) :: img
         Real(Kind=rp), Intent(In), Optional :: vlow, vhgh, vmin, vmax
      End Function plot_vdata
   End Interface
#endif
!
! Executable Statements
!
! Standardise data
   vs(1:m,1) = v(:)
   Call stdize (ieof, 0, 1, m, 1, vave(:), vsd(:), vs(1:m,1:1))
!
! Plot standardized data
#if GUI == 1
   plot = plot_vdata(img, icol_sk, 0, 1, vs(1:m,1), ctitle)
#else
   plot = 0
#endif
!
   Return
 End Function plot_std
!
!
!
#if GUI == 1
 Function plot_thresholds() &
          Result (plot)
!
! Plots a climatological thresholds map
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use labels,             Only: cg_threshslow, cg_threshsupp
   Use gui,                Only: icrev
   Use categories,         Only: tobs
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
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
   Intrinsic MaxVal, MinVal, Trim
!
! Executable Statements
!
! Calculate data limits
   vlow = MinVal(tobs(:,1))
   vhgh = MaxVal(tobs(:,2))
!
! Set titles
   img_thrs(1)%title = Trim(cg_threshsupp)
   img_thrs(2)%title = Trim(cg_threshslow)
!
! Plot thresholds maps
   icrev = 1 - icrev ! - reverse colour order -
   plot = plot_ydata(img_thrs(2), icol_sk, 0, 1, tobs(:,1), Trim(img_thrs(2)%title), &
          vlow=vlow, vhgh=vhgh)
   plot = plot_ydata(img_thrs(1), icol_sk, 0, 1, tobs(:,2), Trim(img_thrs(1)%title), &
          vlow=vlow, vhgh=vhgh)
   icrev = 1 - icrev ! - restore colour order -
!
   Return
 End Function plot_thresholds
!
!
!
 Function plot_tlow() &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use labels,             Only: cg_threshslow
   Use gui,                Only: icrev
   Use categories,         Only: tobs
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot lower thresholds map
   icrev = 1 - icrev ! - reverse colour order -
   plot = plot_ydata(img_thrs(2), icol_sk, 0, 1, tobs(:,1), Trim(cg_threshslow))
   icrev = 1 - icrev ! - restore colour order -
!
   Return
 End Function plot_tlow
!
!
!
 Function plot_tupp() &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use labels,             Only: cg_threshsupp
   Use gui,                Only: icrev
   Use categories,         Only: tobs
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Replot upper thresholds map
   icrev = 1 - icrev ! - reverse colour order -
   plot = plot_ydata(img_thrs(1), icol_sk, 0, 1, tobs(:,2), Trim(cg_threshsupp))
   icrev = 1 - icrev ! - restore colour order -
!
   Return
 End Function plot_tupp
!
!
!
 Function plot_skill_cv() &
          Result (plot)
!
! Modules
   Use scores, Only: skills
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot skill map
   If (img_cvsk(1)%ymin == img_cvsk(1)%ymax) Then
      plot = plot_score(img_cvsk(1), skills(:))
   Else
      plot = plot_score(img_cvsk(1), skills(:), &
             vmin=img_cvsk(1)%ymin, vmax=img_cvsk(1)%ymax)
   End If
!
   Return
 End Function plot_skill_cv
!
!
!
 Function plot_skill_c2() &
          Result (plot)
!
! Modules
   Use scores, Only: skills
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot skill map
   If (img_c2sk(1)%ymin == img_c2sk(1)%ymax) Then
      plot = plot_score(img_c2sk(1), skills(:))
   Else
      plot = plot_score(img_c2sk(1), skills(:), &
             vmin=img_c2sk(1)%ymin, vmax=img_c2sk(1)%ymax)
   End If
!
   Return
 End Function plot_skill_c2
!
!
!
 Function plot_skill_ra() &
          Result (plot)
!
! Modules
   Use scores, Only: skills
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Replot skill map
   If (img_rask(1)%ymin == img_rask(1)%ymax) Then
      plot = plot_score(img_rask(1), skills(:))
   Else
      plot = plot_score(img_rask(1), skills(:), &
             vmin=img_rask(1)%ymin, vmax=img_rask(1)%ymax)
   End If
!
   Return
 End Function plot_skill_ra
!
!
!
 Function plot_pscore() &
          Result (plot)
!
! Modules
   Use scores, Only: pscores
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Replot skill map
   If (img_prss(1)%ymin == img_prss(1)%ymax) Then
      plot = plot_score(img_prss(1), pscores(:))
   Else
      plot = plot_score(img_prss(1), pscores(:), &
             vmin=img_prss(1)%ymin, vmax=img_prss(1)%ymax)
   End If
!
   Return
 End Function plot_pscore
!
!
!
 Function plot_score(img, s, &
          vmin, vmax, vlow, vhgh) &
          Result (plot)
!
! Modules
   Use graphics, Only: image
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! i -input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: s ! - scores -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - absolute minimum -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - absolute maximum -
!
! - optional input/output scalars -
   Real(Kind=rp), Intent(InOut), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(InOut), Optional :: vhgh ! - highest value -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: v1 ! - lowest value -
   Real(Kind=rp) :: v2 ! - highest value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, MinVal, Present
!
! Executable Statements
!
! Update skill map
   If (Present(vlow) .and. Present(vhgh)) Then
      plot = plot_scores(img, s(:), &
             vlow=vlow, vhgh=vhgh)
   Else If (Present(vmin) .and. Present(vmax)) Then
      plot = plot_scores(img, s(:), &
             vmin=vmin, vmax=vmax)
   Else If (img%ymin == img%ymax) Then
      v1 = MinVal(s(:))
      v2 = MaxVal(s(:))
      img%lcontour = .false.
      plot = plot_scores(img, s(:), &
             vlow=v1, vhgh=v2)
   Else
      plot = plot_scores(img, s(:))
   End If
!
   Return
 End Function plot_score
!
!
!
 Function plot_scores(img, s, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_mc, icol_sk
   Use labels,             Only: l_scores
   Use scores,             Only: lerror
   Use graphics,           Only: image
   Use plotting,           Only: plot_ydata
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: s ! - scores -
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
   Integer :: icol ! - colour scheme -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Update scores map
   If (lerror) Then
      icol = icol_mc
   Else
      icol = icol_sk
   End If
   plot = plot_ydata(img, icol, 0, 1, s(:), Trim(l_scores%c), &
          vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
!
   Return
 End Function plot_scores
!
!
!
 Function plot_correl() &
          Result (plot)
!
! Modules
   Use data_gui_constants, Only: icol_sk
   Use labels,             Only: cg_correl
   Use scores,             Only: corrs
   Use plotting,           Only: plot_xdata
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Update correlation map
   plot = plot_xdata(img_cors(1), icol_sk, 0, 1, corrs, Trim(cg_correl))
!
   Return
 End Function plot_correl
#endif
!
!
!
 Function save_skill() &
          Result (saves)
!
! Modules
   Use iofiles,    Only: skfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: saves
!
! Executable Statements
!
! Save requested results
   saves = save_results(ianal, skfile)
!
   Return
 End Function save_skill
!
!
!
 Function save_pscores() &
          Result (saves)
!
! Modules
   Use iofiles,    Only: psfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: saves
!
! Executable Statements
!
! Save requested results
   saves = save_results(ianal, psfile)
!
   Return
 End Function save_pscores
!
!
!
#if GUI == 1
 Function save_pvalues() &
          Result (saves)
!
! Modules
   Use iofiles,    Only: pvfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: saves
!
! Executable Statements
!
! Save requested results
   saves = save_results(ianal, pvfile)
!
   Return
 End Function save_pvalues
!
!
!
 Function select_skill_cv() &
          Result (selects)
!
! Modules
   Use graphics, Only: select_location
   Use validate, Only: validate_cv
!
! Function result
   Integer :: selects
!
! Executable Statements
!
! Get mouse position
   selects = select_location(img_cvsk(1), validate_cv)
!
   Return
 End Function select_skill_cv
!
!
!
 Function select_skill_c2() &
          Result (selects)
!
! Modules
   Use graphics, Only: select_location
   Use validate, Only: validate_c2
!
! Function result
   Integer :: selects
!
! Executable Statements
!
! Get mouse position
   selects = select_location(img_c2sk(1), validate_c2)
!
   Return
 End Function select_skill_c2
!
!
!
 Function select_skill_ra() &
          Result (selects)
!
! Modules
   Use graphics, Only: select_location
   Use validate, Only: validate_ra
!
! Function result
   Integer :: selects
!
! Executable Statements
!
! Get mouse position
   selects = select_location(img_rask(1), validate_ra)
!
   Return
 End Function select_skill_ra
!
!
!
 Function select_pscores() &
          Result (selects)
!
! Modules
   Use graphics, Only: select_location
   Use validate, Only: validate_ra
!
! Function result
   Integer :: selects
!
! Executable Statements
!
! Get mouse position
   selects = select_location(img_prss(1), validate_ra)
!
   Return
 End Function select_pscores
!
!
!
#endif
 Function prompt_skill_cv() &
          Result (prompt)
!
! Prompts for scores
!
! Function result
   Integer :: prompt
!
! Executable Statements
!
! Prompt for score
   prompt = prompt_skill(update_skill_cv)
!
   Return
 End Function prompt_skill_cv
!
!
!
 Function prompt_skill_c2() &
          Result (prompt)
!
! Prompts for scores
!
! Function result
   Integer :: prompt
!
! Executable Statements
!
! Prompt for score
   prompt = prompt_skill(update_skill_c2)
!
   Return
 End Function prompt_skill_c2
!
!
!
 Function prompt_skill_ra() &
          Result (prompt)
!
! Prompts for scores
!
! Function result
   Integer :: prompt
!
! Executable Statements
!
! Prompt for score
   prompt = prompt_skill(update_skill_ra)
!
   Return
 End Function prompt_skill_ra
!
!
!
 Function prompt_skill(update_skill) &
          Result (prompt)
!
! Prompts for scores
!
! Modules
   Use data_cpt_constants, Only: nscore
   Use labels,             Only: ca_scores, &
                                 l_scores
   Use gui,                Only: prompt_bullet
   Use scores,             Only: iskills
!
! Function result
   Integer :: prompt
!
! Arguments
!
! Dummy arguments
! - procedure arguments -
   Integer, External :: update_skill
!
! Executable Statements
!
! Prompt for score
   prompt = prompt_bullet(update_skill, l_scores%c, nscore, ca_scores(:), iskills(:))
!
   Return
 End Function prompt_skill
!
!
!
 Function prompt_pscores() &
          Result (prompt)
!
! Prompts for scores
!
! Modules
   Use data_cpt_constants, Only: ipsi, npall, npcat
#if GUI == 1
   Use data_cpt_constants, Only: nps
#endif
   Use labels,             Only: ca_pavscores_t, ca_pcvscores_t
   Use gui,                Only: prompt_bullet
#if GUI == 1
   Use gui,                Only: box_close, boxes_open, ganged_rbs, print_text
#else
   Use gui,                Only: prompt_integer
#endif
   Use scores,             Only: ipss
!
! Function result
   Integer :: prompt
!
#if GUI == 0
! Locals
!
! Local scalars
   Integer :: i     ! - selected score -
   Integer :: ifail ! - error indicator -
#endif
! Executable Statements
!
#if GUI == 1
! Format
   Call boxes_open (1, 2, .false., .false.)
!
#endif
! Prompt for scores
! - prompt for all-category scores -
   prompt = prompt_bullet(update_pscores, 'All-category scores', npall, ca_pavscores_t(:), ipss(1:npall), &
            lprompt=.false., ns=ipsi(1:npall))
#if GUI == 1
   Call box_close (.false.)
   Call print_text (' ')
#endif
! - prompt for category-specific scores -
   prompt = prompt_bullet(update_pscores, 'Category-specific scores', npcat, ca_pcvscores_t, ipss(npall+1:), &
            lprompt=.false., ns=ipsi(npall+1:))
#if GUI == 0
   Call prompt_integer ('Score', i, ifail, &
        ns=ipsi(:), lzero=.true.)
   If (i > 0) ipss(i) = 1
! - prompt for category -
   If (i > npall) prompt = prompt_cat(update_pscores)
#else
   Call box_close (.false.)
!
! Gang the two sets of scores
   Call ganged_rbs (nps, ipss(:))
#endif
!
   Return
 End Function prompt_pscores
!
!
!
 Function prompt_itx (f_callback) &
          Result (prompt)
!
! Prompts for X training-period date
!
! Modules
   Use data_io_constants,   Only: lprd
   Use data_time_constants, Only: iuseq
   Use labels,              Only: ca_seq
   Use gui,                 Only: print_cvalue, prompt_list
   Use settings,            Only: itx, nu
   Use arrays,              Only: cxprds
!
! Function result
   Integer :: prompt
!
! Arguments
!
! Procedure arguments
   Integer, External :: f_callback ! - callback function -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for X training period date
   If (nu > 1) Then
!       Call prompt_list (Trim(l_fcast%c), .true., cxprds(:), nu, itx, & ! - FTN95 bug -
      Call prompt_list (Trim(ca_seq(iuseq(0))), .true., cxprds, nu, itx, &
           itab=15, iwdth=lprd, cbf1=f_callback)
   Else
      Call print_cvalue (Trim(ca_seq(iuseq(0))), Trim(cxprds(itx)), &
           itab=15)
   End If
   prompt = 1
!
   Return
 End Function prompt_itx
!
!
!
 Function prompt_ity (f_callback) &
          Result (prompt)
!
! Prompts for Y training-period date
!
! Modules
   Use data_io_constants,   Only: lprd
   Use data_time_constants, Only: iuseq
   Use labels,              Only: ca_seq
   Use gui,                 Only: print_cvalue, prompt_list
   Use settings,            Only: ity, nu
   Use arrays,              Only: cyprds
   Use season,              Only: isnc
!
! Function result
   Integer :: prompt
!
! Arguments
!
! Procedure arguments
   Integer, External :: f_callback ! - callback function -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for Y training period date
   If (nu > 1) Then
!       Call prompt_list (Trim(l_fcast%c), .true., cyprds(:), nu, ity, & ! - FTN95 bug -
      Call prompt_list (Trim(ca_seq(iuseq(0))), .true., cyprds%cssn(isnc), nu, ity, &
           itab=15, iwdth=lprd, cbf1=f_callback)
   Else
      Call print_cvalue (Trim(ca_seq(iuseq(0))), Trim(cyprds(ity)%cssn(isnc)), &
           itab=15)
   End If
   prompt = 1
!
   Return
 End Function prompt_ity
!
!
!
#if GUI == 1
 Function title_maps() &
          Result (title)
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
   title = replot_maps(.false., get_title)
!
   Return
 End Function title_maps
!
!
!
 Function limits_maps() &
          Result (limits)
!
! Modules
   Use plotting, Only: get_limits
!
! Function result
   Integer :: limits
!
! Executable Statements
!
! Prompt for limits
   limits = replot_maps(.true., get_limits)
!
   Return
 End Function limits_maps
!
!
!
 Function replot_maps(lcontour, get_custom) &
          Result (replot)
!
! Modules
   Use windows_gui, Only: imgid
   Use plotting,    Only: replot_graph
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
! Executable Statements
!
! Replot customised maps
! - climatological maps -
   If (img_aves(1)%id == imgid) Then
      img_aves(1)%lcontour = lcontour
      replot = replot_graph(img_aves(1), get_custom, plot_aves)
   Else If (img_cvar(1)%id == imgid) Then
      img_cvar(1)%lcontour = lcontour
      replot = replot_graph(img_cvar(1), get_custom, plot_cvar)
   Else If (img_sdev(1)%id == imgid) Then
      img_sdev(1)%lcontour = lcontour
      replot = replot_graph(img_sdev(1), get_custom, plot_sdev)
   Else If (img_stdx(1)%id == imgid) Then
      img_stdx(1)%lcontour = lcontour
      replot = replot_graph(img_stdx(1), get_custom, plot_stdx)
   Else If (img_stdy(1)%id == imgid) Then
      img_stdy(1)%lcontour = lcontour
      replot = replot_graph(img_stdy(1), get_custom, plot_stdy)
   Else If (img_stdz(1)%id == imgid) Then
      img_stdz(1)%lcontour = lcontour
      replot = replot_graph(img_stdz(1), get_custom, plot_stdz)
   Else If (img_thrs(1)%id == imgid) Then
      img_thrs(1)%lcontour = lcontour
      replot = replot_graph(img_thrs(1), get_custom, plot_tupp)
   Else If (img_thrs(2)%id == imgid) Then
      img_thrs(2)%lcontour = lcontour
      replot = replot_graph(img_thrs(2), get_custom, plot_tlow)
   Else If (img_cors(1)%id == imgid) Then
      img_cors(1)%lcontour = lcontour
      replot = replot_graph(img_cors(1), get_custom, plot_correl)
! - skill maps -
   Else If (img_cvsk(1)%id == imgid) Then
      img_cvsk(1)%lcontour = lcontour
      replot = replot_graph(img_cvsk(1), get_custom, plot_skill_cv)
   Else If (img_c2sk(1)%id == imgid) Then
      img_c2sk(1)%lcontour = lcontour
      replot = replot_graph(img_c2sk(1), get_custom, plot_skill_c2)
   Else If (img_rask(1)%id == imgid) Then
      img_rask(1)%lcontour = lcontour
      replot = replot_graph(img_rask(1), get_custom, plot_skill_ra)
   Else If (img_prss(1)%id == imgid) Then
      img_prss(1)%lcontour = lcontour
      replot = replot_graph(img_prss(1), get_custom, plot_pscore)
   End If
!
   Return
 End Function replot_maps
!
!
!
 Function export_maps() &
          Result (export)
!
! Modules
   Use labels,      Only: l_averages, l_correls, l_cvars, l_predictandy, l_predictorx, l_predictorz, l_stdevs
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
      export = export_montage_maps(-imgid)
      Return
   End If
!
! Save graph
! - climatological maps -
   If (img_aves(1)%id == imgid) Then
      export = save_graphic(l_averages%c, img_aves(1), plot_aves)
   Else If (img_cvar(1)%id == imgid) Then
      export = save_graphic(l_cvars%c, img_cvar(1), plot_cvar)
   Else If (img_sdev(1)%id == imgid) Then
      export = save_graphic(l_stdevs%c, img_sdev(1), plot_sdev)
   Else If (img_stdx(1)%id == imgid) Then
      export = save_graphic(l_predictorx%c, img_stdx(1), plot_stdx)
   Else If (img_stdy(1)%id == imgid) Then
      export = save_graphic(l_predictandy%c, img_stdy(1), plot_stdy)
   Else If (img_stdz(1)%id == imgid) Then
      export = save_graphic(l_predictorz%c, img_stdz(1), plot_stdz)
   Else If (img_thrs(1)%id == imgid) Then
      export = save_graphic('Upper_thresholds', img_thrs(1))
   Else If (img_thrs(2)%id == imgid) Then
      export = save_graphic('Lower_thresholds', img_thrs(2))
   Else If (img_cors(1)%id == imgid) Then
      export = save_graphic(l_correls%c, img_cors(1))
! - skill maps -
   Else If (img_cvsk(1)%id == imgid) Then
      export = save_graphic('Skill_cv', img_cvsk(1))
   Else If (img_c2sk(1)%id == imgid) Then
      export = save_graphic('Skill_cv2', img_c2sk(1))
   Else If (img_rask(1)%id == imgid) Then
      export = save_graphic('Skill_retro', img_rask(1))
   Else If (img_prss(1)%id == imgid) Then
      export = save_graphic('Skill_retro', img_prss(1))
   End If
!
   Return
 End Function export_maps
!
!
!
 Function export_montage_maps(imgid) &
          Result (export)
!
! Modules
   Use labels,   Only: l_threshs
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
! - climatological maps -
   If (img_thrs(1)%id == imgid) Then
      export = export_montage(l_threshs%c, img_thrs(2), img_thrs(1))
   End If
!
   Return
 End Function export_montage_maps
!
!
!
 Function help_maps()
!
! Modules
   Use help,        Only: help_file
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: help_maps
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Open context-specific help pages
! - climatological maps -
   If (img_aves(1)%id == imgid) Then
      Call help_file ('Tools_Climate_Averages.html')
   Else If (img_cvar(1)%id == imgid) Then
      Call help_file ('Tools_Climate_CoeffsVar.html')
   Else If (img_sdev(1)%id == imgid) Then
      Call help_file ('Tools_Climate_StdDevs.html')
   Else If (Any(img_thrs(:)%id == imgid)) Then
      Call help_file ('Tools_Climate_Thresholds.html')
   Else If (img_cors(1)%id == imgid) Then
      Call help_file ('Tools_Climate_Correlations.html')
   Else If (img_stdx(1)%id == imgid) Then
      Call help_file ('Tools_Climate_X.html')
   Else If (img_stdy(1)%id == imgid) Then
      Call help_file ('Tools_Climate_Y.html')
   Else If (img_stdz(1)%id == imgid) Then
      Call help_file ('Tools_Climate_Z.html')
! - skill maps -
   Else If (img_cvsk(1)%id == imgid .or. img_c2sk(1)%id == imgid .or. img_rask(1)%id == imgid) Then
      Call help_file ('Tools_Validate_Map.html')
   Else If (img_prss(1)%id == imgid) Then
      Call help_file ('Tools_Verify_PSkillMaps.html')
   End If
   help_maps = 2
!
   Return
 End Function help_maps
#endif
End Module maps
