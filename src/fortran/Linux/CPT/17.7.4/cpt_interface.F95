! Author: Simon Mason
Module cpt_interface
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cpt_window, title_page
!
#if GUI == 0
   Integer, Private :: ishow = 1 ! - show options flag -
!
   Logical, Private :: lsubm = .false. ! - sub-menu? -
!
   Character(Len=3) :: ctabp ! - tab position -

#endif
Contains
!
!
 Subroutine title_page ()
!
! Creates CPT title page
!
! Modules
#if GUI == 1
   Use data_numbers,       Only: rp, one, two
   Use data_text,          Only: ilang
#endif
   Use data_version,       Only: cyr1, cyr2
#if GUI == 0
   Use data_version,       Only: ver
   Use data_cpt_constants, Only: ia_cca, ia_nul, ia_pfv
#endif
   Use data_cpt_constants, Only: ia_non, na
#if GUI == 1
   Use screen,             Only: init_fonts
   Use gui,                Only: cwtitle, &
                                 add_bitmap, init_win, set_text_size, window_end
#else
   Use gui,                Only: iprompt, &
                                 prompt_integer
#endif
   Use gui,                Only: print_text
   Use menus,              Only: set_menu_greying
#if GUI == 0
   Use menus,              Only: imenus, lmnact
#endif
   Use iofiles,            Only: files_open
   Use analysis,           Only: ianal, ianaln
   Use cpt_view,           Only: change_view
#if GUI == 1
   Use menu_options,       Only: options_language
#endif
   Use user,               Only: cpt_ini, &
                                 save_ini
   Use projects,           Only: pdir, this_proj, &
                                 reset_project
!
! Locals
!
! Local scalars
   Integer :: ioc      ! - option changed flag -
   Integer :: iop      ! - project file opened flag -
   Integer :: iopt = 0 ! - option -
   Integer :: ifail    ! - error indicator -
!
#if GUI == 1
   Logical :: lnl ! - new language? -
#else
!
   Character(Len=13) :: ctxt ! - text -
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Check whether a project has been opened as a command-level argument
   If (ianal /= ia_non) Return
!
#if GUI == 1
! Initialise new language check
   lnl = (ilang /= 1)
!
! Initialise labels
1  If (lnl) Then
      If (options_language() /= 0) Stop
   Else
      lnl = .true.
   End If
!
! Create window
! - initialise window -
   ioc = 0
   iop = 0
   Call init_win (title=Trim(cwtitle), &
        lresize=.true.)
!
! Print title page
   Call set_text_size (3.5_rp)
#else
   If (imenus == 0) GoTo 2
   Call print_text (' ')
#endif
   Call print_text (' ')
#if GUI == 1
   Call print_text ('CLIMATE PREDICTABILITY', &
        lbold=.true., lcentre=.true., lpad=.true.)
   Call print_text ('TOOL', &
        lbold=.true., lcentre=.true.)
   Call set_text_size (two)
#else
   Call print_text ('C L I M A T E   P R E D I C T A B I L I T Y   T O O L', &
        lcentre=.true.)
   Write (Unit=ctxt, Fmt='(A,F5.2)') 'Version ',ver
   Call print_text (ctxt, &
        lcentre=.true.)
#endif
   Call print_text (' ')
   Call print_text ('Evaluating seasonal climate predictability', &
        lcentre=.true.)
   Call print_text ('Designed for MOS applications', &
        lcentre=.true.)
   Call print_text (' ')
#if GUI == 1
   Call set_text_size (one)
#endif
   Call print_text (' ')
   Call print_text ('Copyright '//cyr1//'-'//cyr2//' International Research Institute for Climate and Society', &
        lcentre=.true.)
   Call print_text (' ')
   Call print_text (' ')
#if GUI == 1
   Call add_bitmap ('IRI_logo')
#else
   Call print_text ('III RRRR  III                                                                 ', &
        lcentre=.true.)
   Call print_text (' I  R   R  I                                                                  ', &
        lcentre=.true.)
   Call print_text (' I  R   R  I                                                                  ', &
        lcentre=.true.)
   Call print_text (' I  RRRR   I                                                                  ', &
        lcentre=.true.)
   Call print_text (' I  R   R  I   I N T E R N A T I O N A L   R E S E A R C H   I N S T I T U T E', &
        lcentre=.true.)
   Call print_text ('III R   R III  F O R         C L I M A T E         A N D         S O C I E T Y', &
        lcentre=.true.)
1  Call print_text (' ')
   Call print_text (' ')
#endif
!
! Add menus
2  iopt = 0
   Call add_menus (iopt, &
                   ioc=ioc, iop=iop)
#if GUI == 1
   Call window_end ()
#else
!
! Prompt for option
3  Call prompt_integer (' ', iopt)
   lsubm = submenu(iopt) ! - check whether option is a sub-menu -
   Select Case (iopt)
! - open project -
    Case (100)
      iopt = -iopt
      Call cpt_menu_file (iopt)
      iopt = -iopt
    Case (101:199)
      Call cpt_menu_file (iopt)
      If (ianal > 0) GoTo 4 ! - project file opened -
! - data menu items -
    Case (800)
      iopt = -iopt
      Call cpt_menu_data (iopt)
      iopt = -iopt
    Case (801:899)
      Call cpt_menu_data (iopt)
! - view menu items -
    Case (600)
      iopt = -iopt
      Call cpt_menu_view (iopt)
      iopt = -iopt
    Case (0:na) ! - use old analysis codes -
      ianal = iopt
      GoTo 4
    Case (611:614)
      ianal = iopt - ia_nul
      GoTo 4
    Case (631:634) ! - beginner mode options -
      Call cpt_menu_view (iopt)
      GoTo 4
    Case (621)
      ianal = ia_pfv
      GoTo 4
! - options menu items -
    Case (500)
      iopt = -iopt
      Call cpt_menu_opts (iopt)
      iopt = -iopt
    Case (501:599, 11, 5001:5999)
      Call cpt_menu_opts (iopt)
! - help menu items -
    Case (700)
      iopt = -iopt
      Call cpt_menu_help (iopt)
      iopt = -iopt
    Case (701:799)
      Call cpt_menu_help (iopt)
! - error -
    Case Default
      iopt = -1
   End Select
   lmnact = .false.
   If (iopt == -1) Then
      Select Case (iprompt)
       Case (0)
         GoTo 3
       Case (1)
         ianal = ia_cca
       Case (2)
         Stop
      End Select
   End If
   GoTo 1
#endif
!
! Check for changed options
4  Continue
#if GUI == 0
   lmnact = .false.
#endif
   If (ioc == 1) Then
      Call save_ini (Trim(cpt_ini), pdir, ifail)
#if GUI == 1
      Call init_fonts ()
#endif
      GoTo 1
   End If
   Select Case (ianal)
! - quit -
    Case (0)
      If (iop == 1) Then
         GoTo 1
      Else
         Stop
      End If
! - set analysis type -
    Case (1:na)
      If (.not.this_proj%lproj) Then
         Call reset_project (this_proj) ! - apply project settings -
         If (files_open(0, 0)) Continue ! - no files open -
      End If
      ianaln = ianal
      ianal = 0
      If (change_view() /= 2) Stop
      Call set_menu_greying (ldone=.false.)
   End Select
!
   Return
 End Subroutine title_page
!
!
!
#if GUI == 1
 Subroutine cpt_window ()
!
! Creates CPT window
!
! Modules
   Use data_gui_constants, Only: i_handle
   Use screen,             Only: ihght, iwdth
   Use gui,                Only: cwtitle, ic_par, &
                                 add_mdi_frame, init_win, leave_window
   Use settings,           Only: iregr, iretro, istd, isynch
   Use iofiles,            Only: xfile, yfile, zfile
   Use season,             Only: icsv, lb, lensn, lsn, lprst, nsn
   Use prediction,         Only: close_predict
   Use analysis,           Only: ianal, icalc, &
                                 close_calc
   Use cpt_view,           Only: set_menuflags
   Use projects,           Only: close_project
   Use cpt_update,         ONly: reset_limits
!
! Locals
!
! Local scalars
   Integer :: iopt ! - option -
!
   Integer(i_handle) :: ih_host ! - host window handle -
!
! Executable Statements
!
! Set grey menu items
   Call set_menuflags (ianal, icalc, icsv, iregr, istd, isynch, &
                       ixdstr=xfile%idstr, iydstr=yfile%idstr, izdstr=zfile%idstr, iretro=iretro)
!
! Open results window
   Call init_win (title=cwtitle, &
        lresize=.true., ihandle=ih_host, c_opts='maximise,no_border', &
        fclose=close_predict, fclose2=close_calc, fclose3=close_project)
! - add menus -
   iopt = 1
   Call add_menus (iopt)
! - create MDI frame -
   Call add_mdi_frame (iwdth-10, ihght-700)
   Call leave_window (ic_par)
!
! Prepare console window
   Call console_init (400)
!
! Reset integer limits if project file is opened from introductory window
   Call reset_limits (icsv, lprst, .false., lensn, lsn, lb, nsn)
!
   Return
!
Contains
!
!
 Subroutine console_init (nlines)
!
! Initialises console for runtime advisories
!
! Modules
   Use clrwin$,      Only: add_cursor_monitor$, set_max_lines$
   Use data_numbers, Only: one
   Use labels,       Only: ca_seqs_l, &
                           cg_infiles_t, cg_lencp, &
                           l_actions, l_predictandy, l_predictorx, l_predictorz
   Use screen,       Only: dts
   Use gui,          Only: ic_con, icon_h, icon_v, ih_con, ih_nf, ih_nt, &
                           box_close, box_open, gui_creturn, ic_current, init_win, open_clearwin, print_ivalue, print_text, &
                           prompt_integer, set_tabs, set_text_size
   Use menus,        Only: mn_screen_lentp, mn_screen_nf
   Use settings,     Only: nf, nt
   Use iofiles,      Only: mxa, mya, mza, nx, ny, nz, mfile, xfile, yfile, zfile
   Use climate,      Only: ncy
   Use forecast,     Only: change_fcast
   Use menu_actions, Only: check_reset
   Use cpt_file,     Only: input_files
   Use user,         Only: iwin
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlines ! - number of lines -
!
! Locals
!
! Local scalars
   Integer :: ioutc ! - output unit number of console -
   Integer :: ncol  ! - number of columns -
   Integer :: nrow  ! - number of rows -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Nint, Real, Trim
!
! Executable Statements
!
! Add control monitor
   Call add_cursor_monitor$ (ic_current)
!
! Create console window
   Call init_win (ipar=ic_par, c_opts='no_frame,no_caption')
! - input files panel -
   Call box_open (.false., .false.)
   Call print_text (Trim(cg_infiles_t), &
        lbold=.true., lcentre=.true., lcolour=.true.)
! - X input file -
   Call box_open (.false., .false.)
   Call input_files ('X', Trim(l_predictorx%c), xfile, &
        nv=nx, nva=mxa)
   Call gui_creturn (.false.)
   Call gui_creturn (.true.)
   Call print_text (' ', &
        lcentre=.true., ladvance=.false.)
   Call prompt_integer (' '//Trim(mn_screen_lentp%l_lab%c), nt, &
        ilow=0, fcb1=check_reset, iact=mn_screen_lentp%iact, lbold=.true., lfocus=.false., ihandle=ih_nt)
   Call box_close (.false.)
! - Y input file -
   Call box_open (.false., .false.)
   Call input_files ('Y', Trim(l_predictandy%c), yfile, &
        nv=ny, nva=mya, cprd1=mfile%cprd1, cprdn=mfile%cprdn)
   Call gui_creturn (.false.)
   Call gui_creturn (.true.)
   Call print_text (' ', &
        lcentre=.true., ladvance=.false.)
   Call print_ivalue (' '//Trim(cg_lencp)//' ('//Trim(ca_seqs_l(1))//')', ncy)
   Call box_close (.false.)
! - Z input file -
   Call box_open (.false., .false.)
   Call input_files ('Z', Trim(l_predictorz%c), zfile, &
        iopen=xfile%idstr, nv=nz, nva=mza)
   Call gui_creturn (.false.)
   Call gui_creturn (.true.)
   Call print_text (' ', &
        lcentre=.true., ladvance=.false.)
   Call prompt_integer (' '//Trim(mn_screen_nf%l_lab%c), nf, &
        ilow=0, fcb1=change_fcast, iact=mn_screen_nf%iact, lbold=.true., lfocus=.false., ihandle=ih_nf)
   Call box_close (.false.)
! - close input box -
   Call box_close (.false.)
!
! Add actions
! - set text size -
   Call set_text_size (1.5d0)
! - tab setting -
   Call set_tabs (2, [3, 15])
! - set actions list -
   Call gui_creturn (.false.)
   Call print_text (' '//Trim(l_actions%c), &
        ladvance=.false.)
   Call set_text_size (one)
! - open Clearwin+ window -
   Call gui_creturn (.true.)
   Call gui_creturn (.true.)
   If (iwin == 10) Then
      ncol = Nint(Real(iwdth)*0.1) - 3
      nrow = Nint(-14.5 + 0.0555*Real(ihght) + dts*Real(-17))
   Else
      ncol = Nint(Real(iwdth - 33)/7.95)
      nrow = Max(1, Nint(Real(ihght - 465)/16.5))
   End If
   Call open_clearwin (Max(1,ncol+icon_h), Max(1,nrow+icon_v), .false., ioutc, &
        i_out=0, ih=ih_con, lhs=.true., lvs=.true.)
! - leave open  -
   Call leave_window (ic_con)
!
   Call set_max_lines$ (ih_con, nlines)
   Print *, ' '
!
   Return
  End Subroutine console_init
 End Subroutine cpt_window
#else
!
!
!
 Subroutine cpt_window ()
!
! Indicates CPT prompt options
!
! Modules
   Use data_text,           Only: ialphabet, i_cyrillic, ilang
   Use data_cpt_constants,  Only: ifile_x, ifile_y, ifile_z
   Use data_time_constants, Only: iuseq
   Use labels,              Only: ca_seqs_l, &
                                  cg_lencp, cg_nof, cg_seloption, cg_traindata, &
                                  c_lab, &
                                  l_fcast, l_fcasts, &
                                  make_label
   Use screen,              Only: iwdth, lwu
   Use gui,                 Only: cwtitle, &
                                  print_text, prompt_integer
   Use menus,               Only: imenus, lmnact, &
                                  mn_file, mn_data, mn_edit, mn_acts, mn_tool, mn_opts, mn_opts_cv, mn_view, mn_help, &
                                  mn_edit_startatx, mn_edit_startaty, mn_edit_startatz, &
                                  mn_screen_lentp, mn_screen_nf, mn_screen_openx, mn_screen_openy, mn_screen_openz, &
                                  add_menu
   Use settings,            Only: icv2, iregr, iretro, istd, isynch, nf, nt
   Use iofiles,             Only: mxa, mya, nx, ny, nz, &
                                  mfile, xfile, yfile, zfile
   Use season,              Only: icsv
   Use climate,             Only: ncy
   Use prediction,          Only: close_predict
   Use analysis,            Only: ianal, icalc, &
                                  close_calc
   Use cpt_view,            Only: set_menuflags
   Use cpt_file,            Only: i_file, &
                                  get_file
   Use menu_options,        Only: get_nf, get_nt
!
! Locals
!
! Locals scalars
   Integer :: iopt ! - analysis option -
!
   Character(Len=     16) :: cfmt  ! - format statement -
   Character(Len=iwdth-1) :: ctxt  ! - text -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Repeat, Trim
!
! Executable Statements
!
! Set grey menu items
   Call set_menuflags (ianal, icalc, icsv, iregr, istd, isynch, &
        icv2=icv2, ixdstr=xfile%idstr, iydstr=yfile%idstr, izdstr=zfile%idstr, iretro=iretro)
!
! Set tab positioning
   Select Case (ialphabet(ilang))
    Case Default
      ctabp = 'T45'
    Case (i_cyrillic)
      ctabp = 'T70'
   End Select
!
! Print title
   Do
      If (imenus == 0 .and. ishow == 1) GoTo 1
      If (lwu) Then
         Call print_text (' ')
         ctxt = Repeat('_', iwdth-1)
         Call print_text (ctxt, &
              lcentre=.true.)
         Call print_text (' ')
         Call print_text (Trim(cwtitle))
!
! Indicate current settings
         Call file_settings (            'X', xfile, nx, mxa, ianal)
         Call file_settings (            'Y', yfile, ny, mya, ianal, &
              cprd1=mfile%cprd1, cprdn=mfile%cprdn)
         Call file_settings (Trim(l_fcast%c), zfile, nz, mxa, ianal)
!
         Print *, ' '
         Call print_text (Trim(cg_traindata))                                                         ! - Training Data -
         cfmt = '(1X,A,'//ctabp//',I10)'
         Print cfmt, Trim(mn_screen_lentp%l_lab%c)//' ('//Trim(ca_seqs_l(iuseq(yfile%iseq)))//')', nt ! - Length of training period -
         Print cfmt, Trim(cg_lencp)//' ('//Trim(ca_seqs_l(1))//')', ncy                               ! - Length of climatological period -
         c_lab = make_label('L', l_fcasts)
         Print cfmt, Trim(cg_nof)//' '//Trim(c_lab), nf ! - Number of forecasts -
      End If
      Call print_text (ctxt, &
           lcentre=.true.)
      lwu = .false.
!
! Indicate options
      If (ishow == 0) GoTo 2
      Call print_text (' ')
      Call print_text (Trim(cg_seloption)//':')
! - data files -
      Call add_menu ('mn', 1,   0, ' ')
      Call add_menu ('mn', 2, mn_screen_openx, &
           iset1=i_file, ival1=1, cbf=get_file)
      Call add_menu ('mn', 2, mn_screen_openy, &
           iset1=i_file, ival1=2, cbf=get_file)
      Call add_menu ('mn', 2, mn_screen_openz, &
           lgrey=.true., iset1=i_file, ival1=3, cbf=get_file)
! - length of training period -
      Call add_menu ('mn', 2, mn_screen_lentp, &
           lgrey=.true., cbf=get_nt)
! Option 8 (length of cross-validation window) replaced by 534
! - number of forecasts -
      Call add_menu ('mn', 2, mn_screen_nf, &
           lgrey=.true., cbf=get_nf)
!
! Add menus
1     iopt = 1
      Call add_menus (iopt)
2     ishow = 1
      Call prompt_integer (' ', iopt)
3     lsubm = submenu(iopt) ! - check whether option is a sub-menu -
      lmnact = .false.
!
! Perform action
      If (iopt == mn_screen_openx%id) Then ! - Open X input file -
         i_file = ifile_x
         iopt = get_file()
      Else If (iopt == mn_screen_openy%id) Then ! - Open Y input file -
         i_file = ifile_y
         iopt = get_file()
      Else If (iopt == mn_screen_openz%id) Then ! - Open forecast file -
         If (mn_screen_openz%ion == 0) Cycle
         i_file = ifile_z
         iopt = get_file()
      Else If (iopt == 4) Then ! - X training period settings (included for backward compatibility) -
         iopt = mn_edit_startatx%id
         GoTo 3
      Else If (iopt == 5) Then ! - Y training period settings (included for backward compatibility) -
         iopt = mn_edit_startaty%id
         GoTo 3
      Else If (iopt == 6) Then ! - Forecast period settings (included for backward compatibility) -
         iopt = mn_edit_startatz%id
         GoTo 3
      Else If (iopt == mn_screen_lentp%id) Then ! - Length of training period -
         If (mn_screen_lentp%ion == 0) Cycle
         iopt = get_nt()
         If (lmnact) mn_screen_lentp%iact = mn_screen_lentp%iact + 1
      Else If (iopt == 8) Then ! - length of cross-validation window (included for backward compatibility) -
         iopt = mn_opts_cv%id
         GoTo 3
      Else If (iopt == 9) Then ! - Number of forecasts -
         If (mn_screen_nf%ion == 0) Cycle
         iopt = get_nf()
         If (lmnact) mn_screen_nf%iact = mn_screen_nf%iact + 1
      Else If (iopt/100 == mn_file%id/100) Then ! - File menu -
         Call cpt_menu_file (iopt)
      Else If (iopt/100 == mn_data%id/100) Then ! - Data menu -
         Call cpt_menu_data (iopt)
      Else If (iopt/100 == mn_edit%id/100) Then ! - Edit menu -
         Call cpt_menu_edit (iopt)
      Else If (iopt/100 == mn_acts%id/100) Then ! - Actions menu -
         Call cpt_menu_acts (iopt)
      Else If (iopt/100 == mn_tool%id/100 .or. &
               iopt/1000 == mn_tool%id/100) Then ! - Tools menu -
         Call cpt_menu_tool (iopt)
      Else If (iopt/100 == mn_opts%id/100 .or. &
               iopt/1000 == mn_opts%id/100) Then ! - Options menu -
         Call cpt_menu_opts (iopt)
      Else If (iopt/100 == mn_view%id/100) Then ! - View menu -
         Call cpt_menu_view (iopt)
      Else If (iopt/100 == mn_help%id/100) Then ! - Help menu -
         Call cpt_menu_help (iopt)
      Else If (iopt == 0) Then ! - Exit -
         If (icalc == 1) Then
            iopt = close_predict()
            iopt = close_calc()
         End If
         Exit
      Else
         Cycle
      End If
      lmnact = .false.
      If (iopt /= 0 .and. submenu(iopt)) Call prompt_integer (' ', iopt)
      If (lsubm) GoTo 3 ! - repeat actions search when responding to a sub-menu -
   End Do
   Return
!
Contains
!
!
 Subroutine file_settings (cxyz, afile, nv, mva, ianal, &
            cprd1, cprdn)
!
! Prints CPT input file settings
!
! Modules
   Use data_cpt_constants, Only: ia_pfv
   Use labels,             Only: cg_data1, cg_datan, cg_fields_l, cg_indata_t, cg_lags_l, cg_nof, cg_seasons_l, cg_uvars_l, &
                                 l_file, l_startat
   Use gui,                Only: print_text
   Use iofiles,            Only: ifile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv    ! - number of variables -
   Integer, Intent(In) :: mva   ! - number of variables -
   Integer, Intent(In) :: ianal ! - analysis indicator -
!
   Character(Len=*), Intent(In) :: cxyz ! - X/Y/Z flag -
!
   Type(ifile), Intent(In) :: afile ! - input file -
!
! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: cprd1 ! - first period available -
   Character(Len=*), Intent(In), Optional :: cprdn ! - last period available -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Print file settings
   Print *, ' '
   Call print_text (cxyz//' '//Trim(l_file%c), &
        lcentre=.true.)
   Print *, ' '
   cfmt = '(1X,A,'//ctabp//',A)'
   Print cfmt, cg_indata_t, Trim(afile%cfile%fname)
   If (Present(cprd1)) Then
      Print cfmt, Trim(cg_data1), Trim(cprd1)
   Else
      Print cfmt, Trim(cg_data1), Trim(afile%cprd1)
   End If
   If (Present(cprd1)) Then
      Print cfmt, Trim(cg_datan), Trim(cprdn)
   Else
      Print cfmt, Trim(cg_datan), Trim(afile%cprdn)
   End If
   Print cfmt, Trim(l_startat%c), Trim(afile%cprdb)
   cfmt = '(1X,A,'//ctabp//',I0)'
   Print cfmt, Trim(cg_nof)//' '//Trim(cg_fields_l), afile%nfs
   If (ianal == ia_pfv) Then
      Print cfmt, Trim(cg_nof)//' '//Trim(cg_lags_l), afile%nls
   Else
      Print cfmt, Trim(cg_nof)//' '//Trim(cg_seasons_l), afile%nls
   End If
   Print cfmt, Trim(cg_nof)//' '//Trim(afile%cgss), nv
   Print cfmt, Trim(cg_nof)//' '//Trim(cg_uvars_l), mva
!
   Return
 End Subroutine file_settings
 End Subroutine cpt_window
#endif
!
!
!
 Function submenu(iopt)
!
! Checks whether option is for a submenu
!
! Function result
   Logical :: submenu
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iopt ! - option -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Executable Statements
!
! Check whether option is for a submenu
   submenu = (Mod(iopt, 10) == 0)
!
   Return
 End Function submenu
!
!
!
 Subroutine add_menus (iopt, &
            ioc, iop)
!
! Adds windows menus
!
! Modules
#if GUI == 0
   Use labels, Only: l_exit
   Use gui,    Only: print_text
   Use menus,  Only: imenus
#endif
   Use menus,  Only: mn_file, mn_edit, mn_acts, mn_tool, mn_opts, mn_view, mn_help, mn_data
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Optional arguments
! - optional output scalars -
   Integer, Intent(Out), Optional :: ioc ! - changed option -
   Integer, Intent(Out), Optional :: iop ! - project file opened flag -
!
#if GUI == 0
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Return if menus are not required
   If (imenus == 0 .or. lsubm) Then
      If (Present(ioc)) ioc = 0
      If (Present(iop)) iop = 0
      Return
   End If
#endif
!
! Add shortened windows menu
! - file menu -
   Select Case (iopt)
    Case (0)
      iopt = -mn_file%id ! - -100 -
      Call cpt_menu_file (iopt, &
           iop=iop)
! - data menu -
      iopt = -mn_data%id ! - -800 -
      Call cpt_menu_data (iopt)
! - options menu -
      iopt = -mn_opts%id ! - -500 -
      Call cpt_menu_opts (iopt, &
           ioc=ioc)
! - view menu -
      iopt = -mn_view%id ! - -600 -
      Call cpt_menu_view (iopt)
! - help menu -
      iopt = -mn_help%id ! - -700 -
      Call cpt_menu_help (iopt)
!
! Add extended windows menus
! - file -
    Case (1)
      iopt = mn_file%id ! - 100 -
      Call cpt_menu_file (iopt)
! - data -
      iopt = mn_data%id ! - 800 -
      Call cpt_menu_data (iopt)
! - edit -
      iopt = mn_edit%id ! - 200 -
      Call cpt_menu_edit (iopt)
! - actions -
      iopt = mn_acts%id ! - 300 -
      Call cpt_menu_acts (iopt)
! - tools -
      iopt = mn_tool%id ! - 400 -
      Call cpt_menu_tool (iopt)
! - options -
      iopt = mn_opts%id ! - 500 -
      Call cpt_menu_opts (iopt)
! - view -
      iopt = mn_view%id ! - 600 -
      Call cpt_menu_view (iopt)
! - help -
      iopt = mn_help%id ! - 700 -
      Call cpt_menu_help (iopt)
   End Select
!
#if GUI == 0
! Add exit prompt
   Call print_text (' ')
   Call print_text (' ')
   Call print_text ('  0.  '//Trim(l_exit%c))
   Call print_text (' ')
!
#endif
!
   iopt = 0
!
   Return
 End Subroutine add_menus
!
!
!
 Subroutine cpt_menu_file (iopt, &
            iop)
!
! Constructs File menu
!
! Modules
   Use data_cpt_constants, Only: ifile_x, ifile_y, ifile_z
#if GUI == 1
   Use labels,             Only: l_exit, l_resdefs
#else
   Use screen,             Only: lwu
   Use menus,              Only: mn_file_format, mn_file_prec, mn_file_show, mn_file_tags
#endif
   Use menus,              Only: mn_file, mn_file_closex, mn_file_closey, mn_file_closez, mn_file_datout, mn_file_new,        &
                                 mn_file_open, mn_file_outres, mn_file_save, mn_file_saveas, mn_file_savegi, mn_file_savepar, &
                                 mn_file_viewgi, &
                                 add_menu
#if GUI == 0
   Use iofiles,            Only: cformat, itags, &
                                 output_format, output_precision
   Use cpt_output,         Only: tags_lab
#endif
   Use cpt_file,           Only: ixyz, &
                                 close_file
   Use projects,           Only: jproj, &
                                 new_project, open_project, save_project, saveas_project
#if GUI == 1
   Use projects,           Only: restore_defs
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Optional arguments
! - optional output scalars -
   Integer, Intent(Out), Optional :: iop ! - project file opened flag -
!
! Locals
!
! Local scalars
   Integer :: ilev  ! - current menu-level -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
#if GUI == 0
   Intrinsic Present
#endif
!
! Executable Statements
!
! Construct File menu
   If (Abs(iopt) == mn_file%id) Then
      Call add_menu ('mn', 1, mn_file)
#if GUI == 0
      If (iopt == mn_file%id) Then
         Call add_menu ('mn', 2, mn_file_show, &
              iset1=ishow, ival1=0)
      End If
#endif
      Call add_menu ('mn', 2, mn_file_new, &
           cbf=new_project, acc_key='Ctrl+N')
      Call add_menu ('mn', 2, mn_file_open, &
           iset1=iop, ival1=1, cbf=open_project, acc_key='Ctrl+O')
      Call add_menu ('mn', 2, mn_file_save, &
           iset1=iop, ival1=1, lgrey=.true., cbf=save_project, acc_key='Ctrl+S')
      Call add_menu ('mn', 2, mn_file_saveas, &
           iset1=iop, ival1=1, cbf=saveas_project, acc_key='Ctrl+Shift+S')
      Call add_menu ('mn', 2, mn_file_savepar, &
           icheck=jproj, itoggle=jproj)
      If (iopt == mn_file%id) Then
         Call add_menu ('mn', 2, mn_file_closex, &
              lsep=.true., lgrey=.true., iset1=ixyz, ival1=ifile_x, cbf=close_file)
         Call add_menu ('mn', 2, mn_file_closey, &
              lgrey=.true., iset1=ixyz, ival1=ifile_y, cbf=close_file)
         Call add_menu ('mn', 2, mn_file_closez, &
              lgrey=.true., iset1=ixyz, ival1=ifile_z, cbf=close_file)
         ilev = 2
         Call cpt_menu_file_outres ()
      End If
#if GUI == 1
      If (iopt == -mn_file%id) Then
         Call add_menu ('mn', 2,  -2, l_resdefs, &
              lsep=.true., cbf=restore_defs)
      End If
      Call add_menu ('mn', 2,   0, l_exit, &
           lsep=.true., scbf='STOP', acc_key='Ctrl+Q')
#else
      Call add_menu ('mn', 2, mn_file_format, &
           cbf=output_format)
      Call add_menu ('mn', 2, mn_file_prec, &
           cbf=output_precision)
      Call add_menu ('mn', 2, mn_file_tags, &
           itoggle=itags, cbf=tags_lab)
      If (Present(iop)) iop = 0
   Else If (iopt == mn_file_outres%id .or. iopt == mn_file_datout%id .or. &
            iopt == mn_file_savegi%id .or. iopt == mn_file_viewgi%id) Then ! - output results -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_file_outres ()
   Else If (iopt == mn_file_closex%id) Then  ! - close X file -
      If (mn_file_closex%ion /= 0) Then
         ixyz = ifile_x
         iopt = close_file()
      End If
   Else If (iopt == mn_file_closey%id) Then  ! - close Y file -
      If (mn_file_closey%ion /= 0) Then
         ixyz = ifile_y
         iopt = close_file()
      End If
   Else If (iopt == mn_file_closez%id) Then  ! - close forecast file -
      If (mn_file_closez%ion /= 0) Then
         ixyz = ifile_z
         iopt = close_file()
      End If
   Else If (iopt == mn_file_format%id) Then  ! - file output format -
      iopt = output_format()
   Else If (iopt == mn_file_prec%id) Then    ! - data precision -
      If (cformat%ifmt /= 3) iopt = output_precision()
   Else If (iopt == mn_file_tags%id) Then    ! - CPT output-tags -
      itags = 1 - itags
   Else If (iopt == mn_file_show%id) Then    ! - show files -
      ishow = 0
      lsubm = .false.
      lwu = .true.
      iopt = -2
   Else If (iopt == mn_file_new%id) Then     ! - new project -
      iopt = new_project()
   Else If (iopt == mn_file_open%id) Then    ! - open project -
      iopt = open_project()
   Else If (iopt == mn_file_save%id) Then    ! - save project -
      iopt = save_project()                  ! - deliberately not greyed since save and save-as are equivalent in the batch version -
   Else If (iopt == mn_file_saveas%id) Then  ! - save project as -
      iopt = saveas_project()
   Else If (iopt == mn_file_savepar%id) Then ! - save as parameter file -
      jproj = 1 - jproj
   Else
      iopt = -1
#endif
   End If
!
   Return
!
 Contains
!
!
  Subroutine cpt_menu_file_outres ()
!
! File ~ Output Results sub-menu
!
! Modules
#if GUI == 0
   Use gui,        Only: prompt_integer
#endif
   Use cpt_output, Only: save_good
   Use validate,   Only: view_good
!
! Executable Statements
!
! Construct File ~ Output Results menu
   If (iopt == mn_file%id .or. iopt == mn_file_outres%id) Then
      Call add_menu ('mn', ilev, mn_file_outres)
      Call add_menu ('mn', ilev+1, mn_file_savegi, &
           lgrey=.true., cbf=save_good)
      Call add_menu ('mn', ilev+1, mn_file_viewgi, &
           lgrey=.true., cbf=view_good)
      Call add_menu ('mn', ilev+1, mn_file_datout, &
           lsep=.true., lgrey=.true., cbf=output_results)
!
#if GUI == 0
! Prompt for option
      If (iopt == mn_file_outres%id) Call prompt_integer (' ', iopt)
!
! Perform actions
   Else If (iopt == mn_file_savegi%id) Then ! - save goodness index -
      If (mn_file_savegi%ion /= 0) iopt = save_good()
   Else If (iopt == mn_file_viewgi%id) Then ! - view goodness index -
      If (mn_file_viewgi%ion /= 0) iopt = view_good()
   Else If (iopt == mn_file_datout%id) Then ! - data output -
      If (mn_file_datout%ion /= 0) iopt = output_results()
#endif
   End If
!
   Return
  End Subroutine cpt_menu_file_outres
 End Subroutine cpt_menu_file
!
!
!
 Subroutine cpt_menu_data (iopt)
!
! Constructs Data menu
!
! Modules
   Use data_iridl, Only: id_gcm, id_obs, id_rea, &
                         ig_enso, ig_gphs, ig_mslp, ig_ssts, ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_uvwd, ig_uwnd, ig_vwnd, ig_wetd
   Use labels,     Only: l_enso, l_gcmouts, l_gph, l_mslp, l_obs, l_plevs, l_prcp, l_reanalyses, l_sst, l_tmax, &
                         l_tmean, l_tmin, l_u, l_uv, l_v, l_wetdays
#if GUI == 0
   Use labels,     Only: cg_turnoff_l, cg_turnon_l, &
                         l_cancel, l_regrid, l_selsubdom
   Use gui,        Only: print_item, print_text, prompt_integer
#endif
   Use menus,      Only: mn_data, mn_data_merge, mn_data_update, mn_data_dload, &
                         add_menu
   Use cpt_file,   Only: merge_files, update_file
   Use downloads,  Only: iddt, igrib, &
                         download_data
#if GUI == 0
   Use downloads,  Only: lregrid, lsubdom
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
#if GUI == 0
! Locals
!
! Local scalars
   Integer :: i     ! - response index -
   Integer :: idown ! - download -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Trim
!
#endif
! Executable Statements
!
! Construct Data menu
#if GUI == 0
   idown = 0
   If (Abs(iopt) == mn_data%id) Then
#endif
      Call add_menu ('mn', 1, mn_data)
      Call add_menu ('mn', 2, mn_data_merge, &
           lgrey=.true., cbf=merge_files)
      Call add_menu ('mn', 2, mn_data_update, &
           lgrey=.true., cbf=update_file)
#if GUI == 1
      Call add_menu ('mn', 2, mn_data_dload)
#else
      Call add_menu ('mn', 2, mn_data_dload, &
           scbf='CONTINUE')
#endif
! - observations -
#if GUI == 1
   Call add_menu ('mn', 3,  id_obs, l_obs)
   Call add_menu ('mn', 4, ig_enso, l_enso, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_enso, cbf=download_data)
   Call add_menu ('mn', 4, ig_ssts, l_sst, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_ssts, cbf=download_data)
   Call add_menu ('mn', 4, ig_tmax, l_tmax, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_tmax, cbf=download_data)
   Call add_menu ('mn', 4, ig_tmin, l_tmin, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_tmin, cbf=download_data)
   Call add_menu ('mn', 4, ig_tave, l_tmean, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_tave, cbf=download_data)
   Call add_menu ('mn', 4, ig_prcp, l_prcp, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_prcp, cbf=download_data)
   Call add_menu ('mn', 4, ig_wetd, l_wetdays, &
        iset1=iddt, ival1=1, iset2=igrib, ival2=ig_wetd, cbf=download_data)
! - reanalysys -
   Call add_menu ('mn', 3,  id_rea, l_reanalyses)
   Call add_menu ('mn', 4, ig_mslp, l_mslp, &
        iset1=iddt, ival1=2, iset2=igrib, ival2=ig_mslp, cbf=download_data)
   Call add_menu ('mn', 4, ig_gphs, l_gph, &
        iset1=iddt, ival1=2, iset2=igrib, ival2=ig_gphs, cbf=download_data)
   Call add_menu ('mn', 4, ig_uwnd, l_u, &
        iset1=iddt, ival1=2, iset2=igrib, ival2=ig_uwnd, cbf=download_data)
   Call add_menu ('mn', 4, ig_vwnd, l_v, &
        iset1=iddt, ival1=2, iset2=igrib, ival2=ig_vwnd, cbf=download_data)
   Call add_menu ('mn', 4, ig_uvwd, l_uv, &
        iset1=iddt, ival1=2, iset2=igrib, ival2=ig_uvwd, cbf=download_data)
! - GCM outputs -
   Call add_menu ('mn', 3,  id_gcm, l_gcmouts)
   Call add_menu ('mn', 4, ig_ssts, l_sst, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_ssts, cbf=download_data)
   Call add_menu ('mn', 4, ig_tmax, l_tmax, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_tmax, cbf=download_data)
   Call add_menu ('mn', 4, ig_tmin, l_tmin, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_tmin, cbf=download_data)
   Call add_menu ('mn', 4, ig_tave, l_tmean, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_tave, cbf=download_data)
   Call add_menu ('mn', 4, ig_prcp, l_prcp, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_prcp, cbf=download_data)
   Call add_menu ('mn', 4,      -1, l_plevs)
   Call add_menu ('mn', 5, ig_gphs, l_gph, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_gphs, cbf=download_data)
   Call add_menu ('mn', 5, ig_uwnd, l_u, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_uwnd, cbf=download_data)
   Call add_menu ('mn', 5, ig_vwnd, l_v, &
        iset1=iddt, ival1=3, iset2=igrib, ival2=ig_vwnd, cbf=download_data)
#else
!
! Actions
! - merge files -
   Else If (iopt == mn_data_merge%id) Then
      If (mn_data_merge%ion /= 0) iopt = merge_files()
! - update files -
   Else If (iopt == mn_data_update%id) Then
      If (mn_data_update%ion /= 0) iopt = update_file()
! - data download -
   Else If (iopt == mn_data_dload%id) Then
1     Call add_menu ('mn', 2, mn_data_dload)
      Call print_item (id_obs, Trim(l_obs%c), &
           itab=8, iwid=3)
      Call print_item (id_rea, Trim(l_reanalyses%c), &
           itab=8, iwid=3)
      Call print_item (id_gcm, Trim(l_gcmouts%c), &
           itab=8, iwid=3)
      If (.not.lsubdom) Then
         Call print_item (   821, Trim(l_selsubdom%c)//' ('//Trim(cg_turnon_l)//')', &
              itab=8, iwid=3)
      Else
         Call print_item (   821, Trim(l_selsubdom%c)//' ('//Trim(cg_turnoff_l)//')', &
              itab=8, iwid=3)
      End If
      If (.not.lregrid) Then
         Call print_item (   822, Trim(l_regrid%c)//' ('//Trim(cg_turnon_l)//')', &
              itab=8, iwid=3)
      Else
         Call print_item (   822, Trim(l_regrid%c)//' ('//Trim(cg_turnoff_l)//')', &
              itab=8, iwid=3)
      End If
      Call print_item (     0, Trim(l_cancel%c), &
           itab=8, iwid=3)
      Call prompt_integer (' ', i, &
           n=iddt, ns=[0, 1, 2, 3, 821, 822])
      Select Case (iddt)
       Case (821) ! - select subdomain -
         lsubdom = .not.lsubdom
         GoTo 1
       Case (822) ! - regrid -
         lregrid = .not.lregrid
         GoTo 1
       Case (id_obs) ! - observational data -
         getgrib1: Do
            Call print_text (Trim(l_obs%c)//':')
            Call print_item (ig_enso, Trim(l_enso%c), &
                 itab=9, iwid=4)
            Call print_item (ig_ssts, Trim(l_sst%c), &
                 itab=9, iwid=4)
            Call print_item (ig_tmax, Trim(l_tmax%c), &
                 itab=9, iwid=4)
            Call print_item (ig_tmin, Trim(l_tmin%c), &
                 itab=9, iwid=4)
            Call print_item (ig_tave, Trim(l_tmean%c), &
                 itab=9, iwid=4)
            Call print_item (ig_prcp, Trim(l_prcp%c), &
                 itab=9, iwid=4)
            Call print_item (ig_wetd, Trim(l_wetdays%c), &
                 itab=9, iwid=4)
            Call print_item (      0, Trim(l_cancel%c), &
                 itab=9, iwid=4)
            Call prompt_integer (' ', igrib)
            Select Case (igrib)
             Case (ig_enso, ig_ssts, ig_tmax, ig_tmin, ig_tave, ig_prcp, ig_wetd)
               idown = 1
               Exit getgrib1
             Case (0)
               idown = 0
               Exit getgrib1
             Case Default
               Cycle getgrib1
            End Select
         End Do getgrib1
       Case (id_rea) ! - reanalysis data -
         getgrib2: Do
            Call print_text (Trim(l_reanalyses%c)//':')
            Call print_item (ig_mslp, Trim(l_mslp%c), &
                 itab=9, iwid=4)
            Call print_item (ig_gphs, Trim(l_gph%c), &
                 itab=9, iwid=4)
            Call print_item (ig_uwnd, Trim(l_u%c), &
                 itab=9, iwid=4)
            Call print_item (ig_vwnd, Trim(l_v%c), &
                 itab=9, iwid=4)
            Call print_item (ig_uvwd, Trim(l_uv%c), &
                 itab=9, iwid=4)
            Call print_item (      0, Trim(l_cancel%c), &
                 itab=9, iwid=4)
            Call prompt_integer (' ', igrib)
            Select Case (igrib)
             Case (ig_uwnd, ig_vwnd, ig_uvwd, ig_mslp, ig_gphs)
               idown = 1
               Exit getgrib2
             Case (0)
               idown = 0
               Exit getgrib2
             Case Default
               Cycle getgrib2
            End Select
         End Do getgrib2
       Case (id_gcm) ! - GCM outputs -
         getgrib3: Do
            Call print_text (Trim(l_gcmouts%c)//':')
            Call print_item (ig_ssts, Trim(l_sst%c), &
                 itab=9, iwid=4)
            Call print_item (ig_tave, Trim(l_tmean%c), &
                 itab=9, iwid=4)
            Call print_item (ig_tmax, Trim(l_tmax%c), &
                 itab=9, iwid=4)
            Call print_item (ig_tmin, Trim(l_tmin%c), &
                 itab=9, iwid=4)
            Call print_item (ig_prcp, Trim(l_prcp%c), &
                 itab=9, iwid=4)
            Call print_item (ig_gphs, Trim(l_plevs%c)//': '//Trim(l_gph%c), &
                 itab=9, iwid=4)
            Call print_item (ig_uwnd, Trim(l_plevs%c)//': '//Trim(l_u%c), &
                 itab=9, iwid=4)
            Call print_item (ig_vwnd, Trim(l_plevs%c)//': '//Trim(l_v%c), &
                 itab=9, iwid=4)
            Call print_item (ig_uvwd, Trim(l_plevs%c)//': '//Trim(l_uv%c), &
                 itab=9, iwid=4)
            Call print_item (      0, Trim(l_cancel%c), &
                 itab=9, iwid=4)
            Call prompt_integer (' ', igrib)
            Select Case (igrib)
             Case (ig_ssts, ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_uvwd, ig_uwnd, ig_vwnd, ig_gphs)
               idown = 1
               Exit getgrib3
             Case (0)
               idown = 0
               Exit getgrib3
             Case Default
               Cycle getgrib3
            End Select
         End Do getgrib3
      End Select
      If (idown == 1) idown = download_data() ! - download data -
   Else
      iopt = -1
   End If
#endif
!
   Return
 End Subroutine cpt_menu_data
!
!
!
 Subroutine cpt_menu_edit (iopt)
!
! Constructs Edit menu
!
! Modules
   Use menus, Only: mn_edit, mn_edit_datdom, mn_edit_datdomx, mn_edit_datdomy, mn_edit_startat, mn_edit_startatx, &
                    mn_edit_startaty, mn_edit_startatz, mn_edit_ssn, mn_edit_tssn, &
                    add_menu
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Executable Statements
!
! Locals
!
! Local scalars
   Integer :: ilev ! - current menu-level -
!
! Executbale Statements
!
! Construct Edit menu
#if GUI == 0
   If (iopt == mn_edit%id) Then
#endif
      Call add_menu ('mn', 1, mn_edit, &
           lgrey=.true.)
      ilev = 2
      Call cpt_menu_edit_domain ()
      Call cpt_menu_edit_startat ()
      Call cpt_menu_edit_season ()
#if GUI == 0
   Else If (iopt == mn_edit_datdom%id .or. iopt == mn_edit_datdomx%id .or. iopt == mn_edit_datdomy%id) Then ! - edit domain -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_edit_domain ()
   Else If (iopt == mn_edit_startat%id .or. &
            iopt == mn_edit_startatx%id .or. iopt == mn_edit_startaty%id .or. iopt == mn_edit_startatz%id) Then ! - start-at dates -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_edit_startat ()
   Else If (iopt == mn_edit_ssn%id .or. iopt == mn_edit_tssn%id) Then ! - target season -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_edit_season ()
   Else
      iopt = -1
   End If
#endif
!
   Return
!
 Contains
!
!
  Subroutine cpt_menu_edit_domain ()
!
! Edit ~ Data Domain sub-menu
!
! Modules
   Use fields, Only: edit_domain_x, edit_domain_y
!
! Executable Statements
!
! Construct Edit ~ Data Domain sub-menu
   If (iopt == mn_edit%id .or. iopt == mn_edit_datdom%id) Then
      Call add_menu ('mn', ilev, mn_edit_datdom, &
           lgrey=.true.)
      Call add_menu ('mn', ilev+1, mn_edit_datdomx, &
           lgrey=.true., cbf=edit_domain_x)
      Call add_menu ('mn', ilev+1, mn_edit_datdomy, &
           lgrey=.true., cbf=edit_domain_y)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_edit_datdomx%id) Then ! - Change X data domain -
      If (mn_edit_datdom%ion /= 0 .and. mn_edit_datdomx%ion /= 0) iopt = edit_domain_x()
   Else If (iopt == mn_edit_datdomy%id) Then ! - Change Y data domain -
      If (mn_edit_datdom%ion /= 0 .and. mn_edit_datdomy%ion /= 0) iopt = edit_domain_y()
#endif
   End If
!
   Return
  End Subroutine cpt_menu_edit_domain
!
!
!
  Subroutine cpt_menu_edit_startat ()
!
! Edit ~ Start-at sub-menu
!
! Modules
   Use data_cpt_constants, Only: ifile_x, ifile_y, ifile_z
#if GUI == 0
   Use menus,              Only: lmnact
#endif
   Use cpt_file,           Only: ixyz
   Use menu_edit,          Only: edit_startat
!
! Executable Statements
!
! Construct Edit ~ Start-at sub-menu
   If (iopt == mn_edit%id .or. iopt == mn_edit_startat%id) Then
      Call add_menu ('mn', ilev, mn_edit_startat, &
           lgrey=.true.)
      Call add_menu ('mn', ilev+1, mn_edit_startatx, &
           lgrey=.true., iset1=ixyz, ival1=ifile_x, cbf=edit_startat)
      Call add_menu ('mn', ilev+1, mn_edit_startaty, &
           lgrey=.true., iset1=ixyz, ival1=ifile_y, cbf=edit_startat)
      Call add_menu ('mn', ilev+1, mn_edit_startatz, &
           lgrey=.true., iset1=ixyz, ival1=ifile_z, cbf=edit_startat)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_edit_startatx%id) Then ! - Change X start-at date -
      If (mn_edit_startat%ion /= 0 .and. mn_edit_startatx%ion /= 0) Then
         ixyz = ifile_x
         iopt = edit_startat()
         If (lmnact) mn_edit_startatx%iact = mn_edit_startatx%iact + 1
      End If
   Else If (iopt == mn_edit_startaty%id) Then ! - Change Y start-at date -
      If (mn_edit_startat%ion /= 0 .and. mn_edit_startaty%ion /= 0) Then
         ixyz = ifile_y
         iopt = edit_startat()
         If (lmnact) mn_edit_startaty%iact = mn_edit_startaty%iact + 1
      End If
   Else If (iopt == mn_edit_startatz%id) Then ! - Change Z start-at date -
      If (mn_edit_startat%ion /= 0 .and. mn_edit_startatz%ion /= 0) Then
         ixyz = ifile_z
         iopt = edit_startat()
         If (lmnact) mn_edit_startatz%iact = mn_edit_startatz%iact + 1
      End If
#endif
   End If
!
   Return
  End Subroutine cpt_menu_edit_startat
!
!
!
  Subroutine cpt_menu_edit_season ()
!
! Edit ~ Season-limits sub-menu
!
! Modules
#if GUI == 0
   Use menus,     Only: lmnact
#endif
   Use menu_edit, Only: edit_targetssn
!
! Executable Statements
!
! Construct Edit ~ Season-limits sub-menu
   If (iopt == mn_edit%id .or. iopt == mn_edit_ssn%id) Then
      Call add_menu ('mn', 2, mn_edit_tssn, &
           lsep=.true., lgrey=.true., cbf=edit_targetssn)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_edit_tssn%id) Then ! - set target season -
      If (mn_edit%ion /= 0 .and. mn_edit_tssn%ion /= 0) Then
         iopt = edit_targetssn()
         If (lmnact) mn_edit_tssn%iact = mn_edit_tssn%iact + 1
      End If
#endif
   End If
!
   Return
  End Subroutine cpt_menu_edit_season
 End Subroutine cpt_menu_edit
!
!
!
 Subroutine cpt_menu_acts (iopt)
!
! Constructs Actions menu
!
! Modules
   Use menus,        Only: mn_acts, mn_acts_calc, mn_acts_cv, mn_acts_cv2, mn_acts_retro, mn_acts_reset, mn_acts_verify, &
                           add_menu
   Use menu_actions, Only: actions_reset
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Locals
!
! Local scalars
   Integer :: ilev ! - current menu-level -
!
! Executable Statements
!
! Construct Actions menu
#if GUI == 0
   If (iopt == mn_acts%id) Then
#endif
      Call add_menu ('mn', 1, mn_acts, &
           lgrey=.true.)
      ilev = 2
      Call cpt_menu_acts_calculate ()
      Call add_menu ('mn', 2, mn_acts_reset, &
           lgrey=.true., cbf=actions_reset)
#if GUI == 0
   Else If (iopt == mn_acts_calc%id .or. iopt == mn_acts_cv%id .or. iopt == mn_acts_cv2%id .or. iopt == mn_acts_retro%id .or. &
            iopt == mn_acts_verify%id) Then ! - Calculate or Verify -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_acts_calculate ()
   Else If (iopt == mn_acts_reset%id) Then ! - Reset -
      If (mn_acts%ion /= 0 .and. mn_acts_reset%ion /= 0) iopt = actions_reset()
   Else
      iopt = -1
   End If
#endif
!
   Return
!
 Contains
!
!
  Subroutine cpt_menu_acts_calculate ()
!
! Actions ~ Calculate sub-menu
!
! Modules
   Use settings,     Only: icv2, iretro
   Use menu_actions, Only: actions_calculate
!
! Executable Statements
!
! Construct File ~ Output Results menu
   If (iopt == mn_acts%id .or. iopt == mn_acts_calc%id) Then
      Call add_menu ('mn', ilev, mn_acts_calc, &
           lgrey=.true.)
      Call add_menu ('mn', ilev+1, mn_acts_cv, &
           lgrey=.true., iset1=iretro, ival1=0, iset2=icv2, ival2=0, cbf=actions_calculate)
      Call add_menu ('mn', ilev+1, mn_acts_cv2, &
           lgrey=.true., iset1=iretro, ival1=0, iset2=icv2, ival2=1, cbf=actions_calculate)
      Call add_menu ('mn', ilev+1, mn_acts_retro, &
           lgrey=.true., iset1=iretro, ival1=1, iset2=icv2, ival2=0, cbf=actions_calculate)
      Call add_menu ('mn', ilev+1, mn_acts_verify, &
           lsep=.true., lgrey=.true., iset2=icv2, ival2=0, cbf=actions_calculate)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_acts_cv%id) Then ! - perform cross-validated analysis -
      If (mn_acts%ion /= 0 .and. mn_acts_calc%ion /= 0 .and. mn_acts_cv%ion /= 0) Then
         icv2 = 0
         iretro = 0
         iopt = actions_calculate()
         If (iopt /= 1) mn_acts_cv%iact = mn_acts_cv%iact + 1
      End If
   Else If (iopt == mn_acts_cv2%id) Then ! - perform double cross-validated analysis -
      If (mn_acts%ion /= 0 .and. mn_acts_calc%ion /= 0 .and. mn_acts_cv2%ion /= 0) Then
         icv2 = 1
         iretro = 0
         iopt = actions_calculate()
         If (iopt /= 1) mn_acts_cv2%iact = mn_acts_cv2%iact + 1
      End If
   Else If (iopt == mn_acts_retro%id) Then ! - perform retroactive analysis -
      If (mn_acts%ion /= 0 .and. mn_acts_calc%ion /= 0 .and. mn_acts_retro%ion /= 0) Then
         icv2 = 0
         iretro = 1
         iopt = actions_calculate()
         If (iopt /= 1) mn_acts_retro%iact = mn_acts_retro%iact + 1
      End If
   Else If (iopt == mn_acts_verify%id) Then ! - verify -
      If (mn_acts%ion /= 0 .and. mn_acts_calc%ion /= 0 .and. mn_acts_verify%ion /= 0) Then
         icv2 = 0
         iopt = actions_calculate()
         If (iopt /= 1) mn_acts_verify%iact = mn_acts_verify%iact + 1
      End If
#endif
   End If
!
   Return
  End Subroutine cpt_menu_acts_calculate
 End Subroutine cpt_menu_acts
!
!
!
 Subroutine cpt_menu_tool (iopt)
!
! Constructs Tools menu
!
! Modules
   Use menus,    Only: mn_tool, mn_tool_attribs, mn_tool_aves, mn_tool_climmap, mn_tool_corrs, mn_tool_cvars, &
                       mn_tool_cv, mn_tool_cvboot, mn_tool_cvmaps, mn_tool_cvperf, mn_tool_cvtable, &
                       mn_tool_c2, mn_tool_c2boot, mn_tool_c2maps, mn_tool_c2perf, mn_tool_c2table, &
                       mn_tool_ra, mn_tool_raboot, mn_tool_ramaps, mn_tool_raperf, mn_tool_ratable, &
                       mn_tool_fcasts, mn_tool_fens, mn_tool_fps, mn_tool_fser, mn_tool_fval, mn_tool_models, mn_tool_odds, &
                       mn_tool_pexc, mn_tool_pscores, mn_tool_regress, mn_tool_rhits, mn_tool_screes, mn_tool_stdevs,       &
                       mn_tool_tend, mn_tool_threshs, mn_tool_validn, mn_tool_verifn, mn_tool_vroc, mn_tool_vskill,         &
                       mn_tool_wrlt, mn_tool_xmap, mn_tool_ymap, mn_tool_zmap, &
                       add_menu
#if GUI == 1
   Use menus,    Only: mn_tool_cvscat, mn_tool_c2scat, mn_tool_rascat
#else
   Use menus,    Only: mn_tool_cvroc, mn_tool_c2roc, mn_tool_raroc
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Locals
!
! Local scalars
   Integer :: ilev ! - current menu-level -
!
! Executable Statements
!
! Construct Tools menu
#if GUI == 0
   If (iopt == mn_tool%id) Then
#endif
! - Tools -
      Call add_menu ('mn', 1, mn_tool, &
           lgrey=.true.)
! - Validation
      ilev = 2
      Call cpt_menu_tool_validn ()
! - Verification -
      ilev = 2
      Call cpt_menu_tool_verifn ()
! - Modes -
      ilev = 2
      Call cpt_menu_tool_models ()
! - Climatological Maps -
      ilev = 2
      Call cpt_menu_tool_clim ()
! - Forecasts -
      ilev = 2
      Call cpt_menu_tool_fcast ()
#if GUI == 0
   Else If (iopt == mn_tool_validn%id .or. &
            iopt == mn_tool_cv%id     .or. iopt == mn_tool_cvperf%id .or. iopt == mn_tool_cvboot%id  .or. &
            iopt == mn_tool_cvmaps%id .or. iopt == mn_tool_cvroc%id  .or. iopt == mn_tool_cvtable%id .or. &
            iopt == mn_tool_c2%id     .or. iopt == mn_tool_c2perf%id .or. iopt == mn_tool_c2boot%id  .or. &
            iopt == mn_tool_c2maps%id .or. iopt == mn_tool_c2roc%id  .or. iopt == mn_tool_c2table%id .or. &
            iopt == mn_tool_ra%id     .or. iopt == mn_tool_raperf%id .or. iopt == mn_tool_raboot%id  .or. &
            iopt == mn_tool_ramaps%id .or. iopt == mn_tool_raroc%id  .or. iopt == mn_tool_ratable%id) Then ! - Validation -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_tool_validn ()
   Else If (iopt == mn_tool_verifn%id  .or. iopt == mn_tool_attribs%id .or. iopt == mn_tool_vroc%id .or. &
            iopt == mn_tool_pscores%id .or. iopt == mn_tool_wrlt%id    .or. iopt == mn_tool_tend%id .or. &
            iopt == mn_tool_rhits%id   .or. iopt == mn_tool_vskill%id) Then ! - Verification
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_tool_verifn ()
   Else If (iopt == mn_tool_models%id .or. iopt == mn_tool_screes%id .or. iopt == mn_tool_regress%id) Then ! - Models -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_tool_models ()
   Else If (iopt == mn_tool_fcasts%id .or. iopt == mn_tool_fser%id .or. iopt == mn_tool_fens%id .or. iopt == mn_tool_pexc%id .or. &
            iopt == mn_tool_fval%id   .or. iopt == mn_tool_fps%id  .or. iopt == mn_tool_odds%id) Then ! - Forecasts -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_tool_fcast ()
   Else If (iopt == mn_tool_climmap%id .or. iopt == mn_tool_threshs%id .or. iopt == mn_tool_aves%id  .or. &
            iopt == mn_tool_stdevs%id  .or. iopt == mn_tool_cvars%id   .or. iopt == mn_tool_corrs%id .or. &
            iopt == mn_tool_xmap%id    .or. iopt == mn_tool_ymap%id    .or. iopt == mn_tool_zmap%id) Then ! - Climatological Maps -
      If (submenu(iopt)) ilev = 1
      Call cpt_menu_tool_clim ()
   Else
      iopt = -1
   End If
#endif
!
   Return
!
 Contains
!
!
  Subroutine cpt_menu_tool_validn ()
!
! Tools ~ Validation sub-menu
!
! Modules
#if GUI == 0
   Use validate,  Only: roc_cv, roc_c2, roc_ra
#endif
   Use validate,  Only: validate_cv, validate_c2, validate_ra
   Use bootstrap, Only: bootstrap_cv, bootstrap_c2, bootstrap_ra
   Use maps,      Only: map_skill_cv, map_skill_c2, map_skill_ra
#if GUI == 1
   Use scatter,   Only: scatter_cv, scatter_c2, scatter_ra
#endif
   Use tables,    Only: table_cv, table_c2, table_ra
!
! Executable Statements
!
! Construct Tools ~ Validation menu
   If (iopt == mn_tool%id .or. iopt == mn_tool_validn%id .or. &
       iopt == mn_tool_cv%id .or. iopt == mn_tool_c2%id .or. iopt == mn_tool_ra%id) Then
      If (iopt /= mn_tool_c2%id .and. iopt /= mn_tool_ra%id) Then
         Call add_menu ('mn',   ilev, mn_tool_validn, &
              lgrey=.true.)
! - cross-validated -
         Call add_menu ('mn', ilev+1, mn_tool_cv)
         Call add_menu ('mn', ilev+2, mn_tool_cvperf, &
              cbf=validate_cv)
         Call add_menu ('mn', ilev+2, mn_tool_cvboot, &
              cbf=bootstrap_cv)
         Call add_menu ('mn', ilev+2, mn_tool_cvmaps, &
              cbf=map_skill_cv)
#if GUI == 0
         Call add_menu ('mn', ilev+2, mn_tool_cvroc, &
              cbf=roc_cv)
#else
         Call add_menu ('mn', ilev+2, mn_tool_cvscat, &
              cbf=scatter_cv)
#endif
         Call add_menu ('mn', ilev+2, mn_tool_cvtable, &
              cbf=table_cv)
      End If
! - double cross-validated -
#if GUI == 0
      If (mn_tool_c2%ion /= 0) Then
#endif
         Call add_menu ('mn', ilev+1, mn_tool_c2, &
              lgrey=.true.)
         Call add_menu ('mn', ilev+2, mn_tool_c2perf, &
              cbf=validate_c2)
         Call add_menu ('mn', ilev+2, mn_tool_c2boot, &
              cbf=bootstrap_c2)
         Call add_menu ('mn', ilev+2, mn_tool_c2maps, &
              cbf=map_skill_c2)
#if GUI == 0
         Call add_menu ('mn', ilev+2, mn_tool_c2roc, &
              cbf=roc_c2)
#else
         Call add_menu ('mn', ilev+2, mn_tool_c2scat, &
              cbf=scatter_c2)
#endif
         Call add_menu ('mn', ilev+2, mn_tool_c2table, &
              cbf=table_c2)
#if GUI == 0
      End If
! - retroactive -
      If (mn_tool_ra%ion /= 0) Then
#endif
         Call add_menu ('mn', ilev+1, mn_tool_ra, &
              lgrey=.true.)
         Call add_menu ('mn', ilev+2, mn_tool_raperf, &
              cbf=validate_ra)
         Call add_menu ('mn', ilev+2, mn_tool_raboot, &
              cbf=bootstrap_ra)
         Call add_menu ('mn', ilev+2, mn_tool_ramaps, &
              cbf=map_skill_ra)
#if GUI == 0
         Call add_menu ('mn', ilev+2, mn_tool_raroc, &
              cbf=roc_ra)
#else
         Call add_menu ('mn', ilev+2, mn_tool_rascat, &
              cbf=scatter_ra)
#endif
         Call add_menu ('mn', ilev+2, mn_tool_ratable, &
              cbf=table_ra)
#if GUI == 0
      End If
!
! Perform actions
! - cross-validated -
   Else If (iopt == mn_tool_cvperf%id) Then  ! - performance measures -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0) iopt = validate_cv()
   Else If (iopt == mn_tool_cvboot%id) Then  ! - bootstrapping and significance testing -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0) iopt = bootstrap_cv()
   Else If (iopt == mn_tool_cvmaps%id) Then  ! - skill maps -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0) iopt = map_skill_cv()
   Else If (iopt == mn_tool_cvroc%id) Then   ! - ROC -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0) iopt = roc_cv()
   Else If (iopt == mn_tool_cvtable%id) Then ! - contingency tables -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0) iopt = table_cv()
! - double cross-validated -
   Else If (iopt == mn_tool_c2perf%id) Then  ! - performance measures -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_c2%ion /= 0) iopt = validate_c2()
   Else If (iopt == mn_tool_c2boot%id) Then  ! - bootstrapping and significance testing -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_c2%ion /= 0) iopt = bootstrap_c2()
   Else If (iopt == mn_tool_c2maps%id) Then  ! - skill maps -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_c2%ion /= 0) iopt = map_skill_c2()
   Else If (iopt == mn_tool_c2roc%id) Then   ! - ROC -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_c2%ion /= 0) iopt = roc_c2()
   Else If (iopt == mn_tool_c2table%id) Then ! - contingency tables -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_c2%ion /= 0) iopt = table_c2()
! - retroactive -
   Else If (iopt == mn_tool_raperf%id) Then  ! - performance measures -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_ra%ion /= 0) iopt = validate_ra()
   Else If (iopt == mn_tool_raboot%id) Then  ! - bootstrapping and significance testing -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_ra%ion /= 0) iopt = bootstrap_ra()
   Else If (iopt == mn_tool_ramaps%id) Then  ! - skill maps -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_ra%ion /= 0) iopt = map_skill_ra()
   Else If (iopt == mn_tool_raroc%id) Then   ! - ROC -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_ra%ion /= 0) iopt = roc_ra()
   Else If (iopt == mn_tool_ratable%id) Then ! - contingency tables -
      If (mn_tool%ion /= 0 .and. mn_tool_validn%ion /= 0 .and. mn_tool_ra%ion /= 0) iopt = table_ra()
#endif
   End If
!
   Return
  End Subroutine cpt_menu_tool_validn
!
!
!
  Subroutine cpt_menu_tool_verifn ()
!
! Tools ~ Verification sub-menu
!
! Modules
#if GUI == 1
   Use data_cpt_constants, Only: nwr
   Use labels,             Only: ls_wrlts
#endif
   Use maps,               Only: map_pscores
   Use verif,              Only: attrib_diagram, rhit_diagram, prob_scores, roc_diagram, tendency_diagram, wrlt_diagram
#if GUI == 1
   Use verif,              Only: iwrlt
!
! Locals
!
! Local scalars
   Integer :: i ! - weather roulette index -
!
#endif
! Executable Statements
!
! Construct Tools ~ Verification menu
   If (iopt == mn_tool%id .or. iopt == mn_tool_verifn%id) Then
      Call add_menu ('mn',   ilev, mn_tool_verifn, &
           lgrey=.true.)
      Call add_menu ('mn', ilev+1, mn_tool_attribs, &
           cbf=attrib_diagram)
      Call add_menu ('mn', ilev+1, mn_tool_vroc, &
           cbf=roc_diagram)
      Call add_menu ('mn', ilev+1, mn_tool_pscores, &
           cbf=prob_scores)
      Call add_menu ('mn', ilev+1, mn_tool_vskill, &
           cbf=map_pscores)
#if GUI == 1
      Call add_menu ('mn', ilev+1, mn_tool_wrlt)
      Do i = 1, nwr
         Call add_menu ('mn', ilev+2, i, ls_wrlts(i), &
              iset1=iwrlt, ival1=i, cbf=wrlt_diagram)
      End Do
#else
      Call add_menu ('mn', ilev+1, mn_tool_wrlt, &
           cbf=wrlt_diagram)
#endif
      Call add_menu ('mn', ilev+1, mn_tool_tend, &
           cbf=tendency_diagram)
      Call add_menu ('mn', ilev+1, mn_tool_rhits, &
           cbf=rhit_diagram)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_tool_attribs%id) Then ! - Attributes diagram -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = attrib_diagram()
   Else If (iopt == mn_tool_vroc%id) Then ! - ROC diagram -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = roc_diagram()
   Else If (iopt == mn_tool_pscores%id) Then ! - Probabilistic verification scores -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = prob_scores()
   Else If (iopt == mn_tool_wrlt%id) Then ! - Weather roulette -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = wrlt_diagram()
   Else If (iopt == mn_tool_tend%id) Then ! - tendency diagram -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = tendency_diagram()
   Else If (iopt == mn_tool_rhits%id) Then ! - ranked hits diagram -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = rhit_diagram()
   Else If (iopt == mn_tool_vskill%id) Then ! - probabilistic skill scores maps -
      If (mn_tool%ion /= 0 .and. mn_tool_verifn%ion /= 0) iopt = map_pscores()
#endif
   End If
!
   Return
  End Subroutine cpt_menu_tool_verifn
!
!
!
  Subroutine cpt_menu_tool_models ()
!
! Tools ~ Models sub-menu
!
! Modules
#if GUI == 1
   Use menus,   Only: mn_tool_cca, mn_tool_xeof, mn_tool_yeof
#endif
   Use eofs,    Only: scree_plots
#if GUI == 1
   Use eofs,    Only: map_ccas, map_xeofs, map_yeofs
#endif
   Use scatter, Only: regress
!
! Executable Statements
!
! Construct Tools ~ Models menu
   If (iopt == mn_tool%id .or. iopt == mn_tool_models%id) Then
      Call add_menu ('mn', ilev,   mn_tool_models, &
           lgrey=.true.)
      Call add_menu ('mn', ilev+1, mn_tool_screes, &
           lgrey=.true., cbf=scree_plots)
#if GUI == 1
      Call add_menu ('mn', ilev+1, mn_tool_xeof, &
           cbf=map_xeofs)
      Call add_menu ('mn', ilev+1, mn_tool_yeof, &
           lgrey=.true., cbf=map_yeofs)
      Call add_menu ('mn', ilev+1, mn_tool_cca, &
           lgrey=.true., cbf=map_ccas)
#endif
! - regression -
      Call add_menu ('mn', ilev+1, mn_tool_regress, &
           lsep=.true., lgrey=.true., cbf=regress)
!
#if GUI == 1
! Perform actions
   Else If (iopt == mn_tool_screes%id) Then ! - Scree plots -
      If (mn_tool%ion /= 0 .and. mn_tool_screes%ion /= 0) iopt = scree_plots()
   Else If (iopt == mn_tool_regress%id) Then ! - Regression -
      If (mn_tool%ion /= 0 .and. mn_tool_regress%ion /= 0) iopt = regress()
#endif
   End If
!
   Return
  End Subroutine cpt_menu_tool_models
!
!
!
  Subroutine cpt_menu_tool_fcast ()
!
! Tools ~ Forecasts sub-menu
!
! Modules
#if GUI == 1
   Use menus,      Only: mn_tool_hps
#endif
   Use menus,      Only: mn_tool_fmaps
   Use forecast,   Only: fcst_fens, fcst_fps, fcst_fser, fcst_fval, fcst_odds, fcst_pexc
#if GUI == 1
   Use forecast,   Only: fcst_hps
#endif
!
! Executable Statements
!
! Construct Tools ~ Forecasts menu
   If (iopt == mn_tool%id .or. iopt == mn_tool_fcasts%id) Then
      Call add_menu ('mn', 2, mn_tool_fcasts, &
           lgrey=.true.)
      Call add_menu ('mn', 3, mn_tool_fser, &
           lgrey=.true., cbf=fcst_fser)
      Call add_menu ('mn', 3, mn_tool_fens, &
           lgrey=.true., cbf=fcst_fens)
      Call add_menu ('mn', 3, mn_tool_pexc, &
           lgrey=.true., cbf=fcst_pexc)
      Call add_menu ('mn', 3, mn_tool_fmaps)
      Call add_menu ('mn', 4, mn_tool_fval, &
           lgrey=.true., cbf=fcst_fval)
      Call add_menu ('mn', 4, mn_tool_fps, &
           cbf=fcst_fps)
#if GUI == 1
      Call add_menu ('mn', 4, mn_tool_hps, &
           lgrey=.true., cbf=fcst_hps)
#endif
      Call add_menu ('mn', 4, mn_tool_odds, &
           cbf=fcst_odds)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_tool_fser%id) Then ! - Forecast series -
      If (mn_tool%ion /= 0 .and. mn_tool_fcasts%ion /= 0 .and. mn_tool_fser%ion /= 0) Then
         iopt = fcst_fser()
         If (iopt /= 1) mn_tool_fser%iact = mn_tool_fser%iact + 1 
      End If
   Else If (iopt == mn_tool_fens%id) Then ! - Forecast ensembles -
      If (mn_tool%ion /= 0 .and. mn_tool_fcasts%ion /= 0 .and. mn_tool_fens%ion /= 0) Then
         iopt = fcst_fens()
         If (iopt /= 1) mn_tool_fens%iact = mn_tool_fens%iact + 1 
      End If
   Else If (iopt == mn_tool_pexc%id) Then ! - Exceedance probabilities -
      If (mn_tool%ion /= 0 .and. mn_tool_fcasts%ion /= 0 .and. mn_tool_pexc%ion /= 0) Then
         iopt = fcst_pexc()
         If (iopt /= 1) mn_tool_pexc%iact = mn_tool_pexc%iact + 1 
      End If
   Else If (iopt == mn_tool_fval%id) Then ! - Forecast values -
      If (mn_tool%ion /= 0 .and. mn_tool_fcasts%ion /= 0 .and. mn_tool_fval%ion /= 0) Then
         iopt = fcst_fval()
         If (iopt /= 1) mn_tool_fval%iact = mn_tool_fval%iact + 1 
      End If
   Else If (iopt == mn_tool_fps%id) Then ! - Forecast probabilities -
      If (mn_tool%ion /= 0 .and. mn_tool_fcasts%ion /= 0) Then
         iopt = fcst_fps()
         If (iopt /= 1) mn_tool_fps%iact = mn_tool_fps%iact + 1 
      End If
   Else If (iopt == mn_tool_odds%id) Then ! - Forecast odds -
      If (mn_tool%ion /= 0 .and. mn_tool_fcasts%ion /= 0) Then
         iopt = fcst_odds()
         If (iopt /= 1) mn_tool_odds%iact = mn_tool_odds%iact + 1 
      End If
#endif
   End If
!
   Return
  End Subroutine cpt_menu_tool_fcast
!
!
!
  Subroutine cpt_menu_tool_clim ()
!
! Tools ~ Climatological Maps sub-menu
!
! Modules
   Use maps, Only: map_averages, map_correls, map_cvars, map_stdevs, map_stdx, map_stdy, map_stdz, map_thresholds
!
! Executable Statements
!
! Construct Tools ~ Climatological Maps menu
   If (iopt == mn_tool%id .or. iopt == mn_tool_climmap%id) Then
      Call add_menu ('mn',   ilev, mn_tool_climmap)
      Call add_menu ('mn', ilev+1, mn_tool_threshs, &
           cbf=map_thresholds)
      Call add_menu ('mn', ilev+1, mn_tool_aves, &
           cbf=map_averages)
      Call add_menu ('mn', ilev+1, mn_tool_stdevs, &
           cbf=map_stdevs)
      Call add_menu ('mn', ilev+1, mn_tool_cvars, &
           lgrey=.true., cbf=map_cvars)
      Call add_menu ('mn', ilev+1, mn_tool_corrs, &
           lgrey=.true., cbf=map_correls)
      Call add_menu ('mn', ilev+1, mn_tool_xmap, &
           lsep=.true., lgrey=.true., cbf=map_stdx)
      Call add_menu ('mn', ilev+1, mn_tool_ymap, &
           lgrey=.true., cbf=map_stdy)
      Call add_menu ('mn', ilev+1, mn_tool_zmap, &
           lgrey=.true., cbf=map_stdz)
!
#if GUI == 0
! Perform actions
   Else If (iopt == mn_tool_threshs%id) Then ! - Climatological thresholds maps -
      If (mn_tool%ion /= 0) iopt = map_thresholds()
   Else If (iopt == mn_tool_aves%id) Then ! - Climatological averages maps -
      If (mn_tool%ion /= 0) iopt = map_averages()
   Else If (iopt == mn_tool_stdevs%id) Then ! - Climatological standard deviations maps -
      If (mn_tool%ion /= 0) iopt = map_stdevs()
   Else If (iopt == mn_tool_cvars%id) Then ! - Climatological coefficients of variation maps -
      If (mn_tool%ion /= 0 .and. mn_tool_cvars%ion /= 0) iopt = map_cvars()
   Else If (iopt == mn_tool_corrs%id) Then ! - Correlation maps -
      If (mn_tool%ion /= 0 .and. mn_tool_corrs%ion /= 0) iopt = map_correls()
   Else If (iopt == mn_tool_xmap%id) Then ! - Standardized new predictors -
      If (mn_tool%ion /= 0 .and. mn_tool_xmap%ion /= 0) iopt = map_stdx()
   Else If (iopt == mn_tool_ymap%id) Then ! - Standardized predictands -
      If (mn_tool%ion /= 0 .and. mn_tool_ymap%ion /= 0) iopt = map_stdy()
   Else If (iopt == mn_tool_zmap%id) Then ! - Standardized new predictors -
      If (mn_tool_zmap%ion /= 0) iopt = map_stdz()
#endif
   End If
!
   Return
  End Subroutine cpt_menu_tool_clim
 End Subroutine cpt_menu_tool
!
!
!
 Subroutine cpt_menu_opts (iopt, &
            ioc)
!
! Constructs Options menu
!
! Modules
#if GUI == 1
   Use data_text,          Only: ca_langlist, ilang, nlang
#endif
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm
#if GUI == 0
   Use data_cpt_constants, Only: ia_non
   Use menus,              Only: imenus, lmnact, &
                                 mn_opts_cca, mn_opts_ccaadv, mn_opts_errors, mn_opts_link, mn_opts_maskdry, mn_opts_maskskll, &
                                 mn_opts_menus, mn_opts_missx, mn_opts_missy, mn_opts_pval, mn_opts_xmodeadv, mn_opts_ymodeadv
#endif
   Use menus,              Only: mn_opts, mn_opts_aveens, mn_opts_begin, mn_opts_boot, mn_opts_cca, mn_opts_climper, mn_opts_cv, &
                                 mn_opts_data, mn_opts_elev, mn_opts_exclude, mn_opts_expert, mn_opts_fcast, mn_opts_gcm,        &
                                 mn_opts_good, mn_opts_lang, mn_opts_mask, mn_opts_mdlcca, mn_opts_mdlgcm, mn_opts_mdlmlr,       &
                                 mn_opts_mdlpcr, mn_opts_models, mn_opts_miss, mn_opts_regr, mn_opts_show, mn_opts_sortens,      &
                                 mn_opts_synch, mn_opts_tailor, mn_opts_time, mn_opts_trnss, mn_opts_trnsy, mn_opts_verif,       &
                                 mn_opts_xmodes, mn_opts_ymodes, mn_opts_zerob, &
                                 add_menu
#if GUI == 0
   Use iofiles,            Only: cxy
   Use pcs,                Only: ieofp, ieofx, ieofy, &
                                 advanced_cca, advanced_eof
   Use analysis,           Only: ianal
#endif
   Use analysis,           Only: ianaln
   Use cpt_view,           Only: change_view
#if GUI == 0
   Use bootstrap,          Only: ipval
#endif
   Use cpt_update,         Only: toggle_elev
   Use menu_options,       Only: imask, &
                                 options_boot, options_cca, options_climatology, options_cv, options_eofx, options_eofy,           &
                                 options_forecast, options_gcm, options_goodness, options_list, options_masks, options_regression, &
                                 options_tailoring, options_time, options_transform, options_verif
#if GUI == 0
   Use menu_options,       Only: options_errors, options_language, options_linkf
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Optional arguments
! - optional output scalars -
   Integer, Intent(Out), Optional :: ioc ! - changed option -
!
! Locals
!
! Local scalars
   Integer :: ilev ! - current menu-level -
#if GUI == 1
   Integer :: i    ! - index -
#else
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
#endif
!
! Executable Statements
!
! Construct Options menu
! - short menu -
   If (iopt == -mn_opts%id) Then
      Call add_menu ('mn', 1, mn_opts)
#if GUI == 1
      Call add_menu ('mn', 2, mn_opts_lang)
!     Do i = 1, nlang ! - disable Russian language in Windows version for now -
      Do i = 1, nlang-1
         Call add_menu ('mn', 3,   i, ca_langlist(i), &
              iset1=ioc, ival1=1, iset2=ilang, ival2=i, scbf='EXIT')
      End Do
#else
      Call add_menu ('mn', 2, mn_opts_lang, &
           cbf=options_language)
#endif
      Call add_menu ('mn', 2, mn_opts_elev)
      Call add_menu ('mn', 3, mn_opts_begin, &
           lgrey=.true., iset1=ioc, ival1=1, cbf=toggle_elev, scbf='EXIT')
      Call add_menu ('mn', 3, mn_opts_expert, &
           lgrey=.true., iset1=ioc, ival1=1, cbf=toggle_elev, scbf='EXIT')
#if GUI == 1
      ilev = 2
      Call cpt_menu_opts_config ()
#else
      Call add_menu ('mn', 2, mn_opts_errors, &
           cbf=options_errors)
      Call add_menu ('mn', 2, mn_opts_menus, &
           icheck=imenus, itoggle=imenus)
      If (Present(ioc)) ioc = 0
#endif
   Else If (iopt == mn_opts%id) Then
      Call add_menu ('mn', 1, mn_opts)
#if GUI == 0
      Call add_menu ('mn', 2, mn_opts_lang, &
           cbf=options_language)
#endif
      Call add_menu ('mn', 2, mn_opts_show, &
           cbf=options_list)
      Call add_menu ('mn', 2, mn_opts_xmodes, &
           lsep=.true., lgrey=.true., cbf=options_eofx)
#if GUI == 0
      Call add_menu ('mn', 2, mn_opts_xmodeadv, &
           lgrey=.true., cbf=advanced_eof)
#endif
      Call add_menu ('mn', 2, mn_opts_ymodes, &
           lgrey=.true., cbf=options_eofy)
#if GUI == 0
      Call add_menu ('mn', 2, mn_opts_ymodeadv, &
           lgrey=.true., cbf=advanced_eof)
#endif
      Call add_menu ('mn', 2, mn_opts_cca, &
           lgrey=.true., cbf=options_cca)
#if GUI == 0
      Call add_menu ('mn', 2, mn_opts_ccaadv, &
           lgrey=.true., cbf=advanced_cca)
#endif
      Call add_menu ('mn', 2, mn_opts_gcm, &
           lgrey=.true., cbf=options_gcm)
      Call add_menu ('mn', 2, mn_opts_regr, &     ! - regression options -
           lgrey=.true., cbf=options_regression)
      Call add_menu ('mn', 2, mn_opts_models, &
           lgrey=.true.)
      Call add_menu ('mn', 3, mn_opts_mdlcca, &
           lgrey=.true., iset1=ianaln, ival1=ia_cca, cbf=change_view)
      Call add_menu ('mn', 3, mn_opts_mdlpcr, &
           lgrey=.true., iset1=ianaln, ival1=ia_pcr, cbf=change_view)
      Call add_menu ('mn', 3, mn_opts_mdlmlr, &
           lgrey=.true., iset1=ianaln, ival1=ia_mlr, cbf=change_view)
      Call add_menu ('mn', 3, mn_opts_mdlgcm, &
           lgrey=.true., iset1=ianaln, ival1=ia_gcm, cbf=change_view)
      Call add_menu ('mn', 2, mn_opts_good, &     ! - goodness index options -
           lsep=.true., lgrey=.true., cbf=options_goodness)
      Call add_menu ('mn', 2, mn_opts_climper, &  ! - climatological period options -
           lsep=.true., lgrey=.true., cbf=options_climatology)
      Call add_menu ('mn', 2, mn_opts_tailor, &   ! - tailoring options -
           lgrey=.true., cbf=options_tailoring)
      Call add_menu ('mn', 2, mn_opts_cv, &       ! - cross-validation options -
           lgrey=.true., cbf=options_cv)
#if GUI == 0
      Call add_menu ('mn', 2, mn_opts_link, &     ! - link functions -
           lgrey=.true., cbf=options_linkf)
#endif
      Call add_menu ('mn', 2, mn_opts_mask, &     ! - masking options -
           iset1=imask, ival1=0, cbf=options_masks)
#if GUI == 0
      Call add_menu ('mn', 3, mn_opts_maskdry, &  ! - dry-masking options -
           lgrey=.true., iset1=imask, ival1=1, cbf=options_masks)
      Call add_menu ('mn', 3, mn_opts_maskskll, & ! - skill-masking options -
           iset1=imask, ival1=2, cbf=options_masks)
#endif
      Call add_menu ('mn', 2, mn_opts_boot, &     ! - resampling settings -
           cbf=options_boot)
      Call add_menu ('mn', 2, mn_opts_fcast, &    ! - forecast settings -
           cbf=options_forecast)
      Call add_menu ('mn', 2, mn_opts_verif, &    ! - verification settings -
           cbf=options_verif)
      Call add_menu ('mn', 2, mn_opts_trnss, &    ! - transformation settings -
           lgrey=.true., cbf=options_transform)
      Call add_menu ('mn', 2, mn_opts_time, &     ! - time settings -
           lgrey=.true., cbf=options_time)
      ilev = 2
      Call cpt_menu_opts_data ()
#if GUI == 1
      ilev = 2
      Call cpt_menu_opts_graph ()
#else
      Call add_menu ('mn', 2, mn_opts_pval, &
           icheck=ipval, itoggle=ipval)
      Call add_menu ('mn', 2, mn_opts_errors, &
           cbf=options_errors)
      Call add_menu ('mn', 2, mn_opts_menus, &
           icheck=imenus, itoggle=imenus)
   Else If (iopt == mn_opts_lang%id .or. iopt == 11) Then ! - Change language -
      iopt = options_language()
   Else If (iopt == mn_opts_elev%id) Then ! - Expert level -
      iopt = toggle_elev()
   Else If (iopt == mn_opts_begin%id) Then ! - Beginner -
      If (mn_opts_begin%ion /= 0) iopt = toggle_elev()
   Else If (iopt == mn_opts_expert%id) Then ! - Expert -
      If (mn_opts_expert%ion /= 0) iopt = toggle_elev()
   Else If (iopt == mn_opts_show%id) Then ! - List options -
      If (ianal /= ia_non) iopt = options_list()
   Else If (iopt == mn_opts_xmodes%id) Then ! - Change numbers of X modes / GCM settings -
      If (mn_opts_xmodes%ion /= 0) iopt = options_eofx()
   Else If (iopt == mn_opts_xmodeadv%id) Then ! - Change X modes options -
      If (mn_opts_xmodeadv%ion /= 0) Then
         cxy = 'X'
         ieofp => ieofx
         iopt = advanced_eof()
         If (lmnact) mn_opts_xmodeadv%iact = mn_opts_xmodeadv%iact + 1
      End If
   Else If (iopt == mn_opts_ymodes%id) Then ! - Change numbers of Y modes -
      If (mn_opts_ymodes%ion /= 0) iopt = options_eofy()
   Else If (iopt == mn_opts_ymodeadv%id) Then ! - Change Y modes options -
      If (mn_opts_ymodeadv%ion /= 0) Then
         cxy = 'Y'
         ieofp => ieofy
         iopt = advanced_eof()
         If (lmnact) mn_opts_ymodeadv%iact = mn_opts_ymodeadv%iact + 1
      End If
   Else If (iopt == mn_opts_cca%id) Then ! - Change numbers of CCA modes -
      If (mn_opts_cca%ion /= 0) iopt = options_cca()
   Else If (iopt == mn_opts_ccaadv%id) Then ! - Change CCA options -
      If (mn_opts_ccaadv%ion /= 0) Then
         iopt = advanced_cca()
         If (lmnact) mn_opts_ccaadv%iact = mn_opts_ccaadv%iact + 1
      End If
   Else If (iopt == mn_opts_gcm%id) Then ! - Change GCM options -
      If (mn_opts_gcm%ion /= 0) Then
         iopt = options_gcm()
         If (lmnact) mn_opts_gcm%iact = mn_opts_gcm%iact + 1
      End If
   Else If (iopt == mn_opts_regr%id) Then ! - regression options -
      If (mn_opts_regr%ion /= 0) Then
         iopt = options_regression()
         If (lmnact) mn_opts_regr%iact = mn_opts_regr%iact + 1
      End If
   Else If (iopt == mn_opts_good%id) Then ! - Goodness index options -
      If (ianal /= ia_non .and. mn_opts_good%ion /= 0) Then
         iopt = options_goodness()
         If (lmnact) mn_opts_good%iact = mn_opts_good%iact + 1
      End If
   Else If (iopt == mn_opts_climper%id) Then ! - Set climatological period -
      If (mn_opts_climper%ion /= 0) Then
         iopt = options_climatology()
         If (lmnact) mn_opts_climper%iact = mn_opts_climper%iact + 1
      End If
   Else If (iopt == mn_opts_tailor%id) Then ! - Tailoring -
      If (mn_opts_tailor%ion /= 0) Then
         iopt = options_tailoring()
         If (lmnact) mn_opts_tailor%iact = mn_opts_tailor%iact + 1
      End If
   Else If (iopt == mn_opts_cv%id) Then ! - Length of cross-validation window -
      If (mn_opts_cv%ion /= 0) Then
         iopt = options_cv()
         If (lmnact) mn_opts_cv%iact = mn_opts_cv%iact + 1
      End If
   Else If (iopt == mn_opts_link%id) Then ! - link functions -
      If (mn_opts_link%ion /= 0) Then
         iopt = options_linkf()
         If (lmnact) mn_opts_link%iact = mn_opts_link%iact + 1
      End If
   Else If (iopt == mn_opts_mask%id) Then ! - set masking -
      If (ianal /= ia_non) Then
         imask = 0
         iopt = options_masks()
         If (lmnact) mn_opts_mask%iact = mn_opts_mask%iact + 1
      End If
   Else If (iopt == mn_opts_maskdry%id) Then ! - set dry-mask settings -
      If (mn_opts_maskdry%ion /= 0) Then
         imask = 1
         iopt = options_masks()
         If (lmnact) mn_opts_maskdry%iact = mn_opts_maskdry%iact + 1
      End If
   Else If (iopt == mn_opts_maskskll%id) Then ! - set skill-mask settings -
      imask = 2
      iopt = options_masks()
      If (lmnact) mn_opts_maskskll%iact = mn_opts_maskskll%iact + 1
   Else If (iopt == mn_opts_data%id) Then ! - Data -
      ilev = 1
      Call cpt_menu_opts_data ()
   Else If (iopt == mn_opts_trnsy%id  .or. iopt == mn_opts_zerob%id .or. iopt == mn_opts_sortens%id .or. &
            iopt == mn_opts_aveens%id .or. iopt == mn_opts_synch%id  .or. iopt == mn_opts_exclude%id .or. &
            iopt == mn_opts_miss%id .or. iopt == mn_opts_missx%id  .or. iopt == mn_opts_missy%id) Then ! - Data -
      Call cpt_menu_opts_data ()
   Else If (iopt == mn_opts_boot%id) Then  ! - bootstrap settings -
      If (ianal /= ia_non) Then
         iopt = options_boot()
         If (lmnact) mn_opts_boot%iact = mn_opts_boot%iact + 1
      End If
   Else If (iopt == mn_opts_fcast%id) Then ! - forecast settings -
      If (ianal /= ia_non) Then
         iopt = options_forecast()
         If (lmnact) mn_opts_fcast%iact = mn_opts_fcast%iact + 1
      End If
   Else If (iopt == mn_opts_verif%id) Then ! - verification settings -
      If (ianal /= ia_non) Then
         iopt = options_verif()
         If (lmnact) mn_opts_verif%iact = mn_opts_verif%iact + 1
      End If
   Else If (iopt == mn_opts_trnss%id) Then ! - transformation settings -
      If (mn_opts_trnss%ion /= 0) Then
         iopt = options_transform()
         If (lmnact) mn_opts_trnss%iact = mn_opts_trnss%iact + 1
      End If
   Else If (iopt == mn_opts_time%id) Then ! - time settings -
      If (mn_opts_time%ion /= 0) Then
         iopt = options_time()
         If (lmnact) mn_opts_time%iact = mn_opts_time%iact + 1
      End If
   Else If (iopt == mn_opts_pval%id) Then ! - p-values -
      If (ianal /= ia_non) Then
         ipval = 1 - ipval
         mn_opts_pval%iact = mn_opts_pval%iact + 1
      End If
   Else If (iopt == mn_opts_errors%id) Then ! - error settings -
      iopt = options_errors()
      If (lmnact) mn_opts_errors%iact = mn_opts_errors%iact + 1
   Else If (iopt == mn_opts_menus%id) Then ! - print menu? -
      imenus = 1 - imenus
      mn_opts_menus%iact = mn_opts_menus%iact + 1
   Else
      iopt = -1
#endif
   End If
!
   Return
!
 Contains
!
!
  Subroutine cpt_menu_opts_data ()
!
! Options ~ Data sub-menu
!
! Modules
#if GUI == 0
   Use data_cpt_constants, Only: ifile_x, ifile_y
#endif
   Use settings,           Only: ieav, igauss, isem, isynch, izero, &
                                 change_igauss, set_zero
   Use missing,            Only: imissf
   Use menu_options,       Only: options_data_missing, options_exclude
!
! Executable Statements
!
! Construct Options ~ Data menu
   If (iopt == mn_opts%id .or. iopt == mn_opts_data%id) Then
      Call add_menu ('mn',   ilev, mn_opts_data, &
           lsep=.true., lgrey=.true.)
      Call add_menu ('mn', ilev+1, mn_opts_trnsy, &
           icheck=igauss, lgrey=.true., cbf=change_igauss)
      Call add_menu ('mn', ilev+1, mn_opts_zerob, &
           icheck=izero, lgrey=.true., cbf=set_zero)
      Call add_menu ('mn', ilev+1, mn_opts_sortens, &
           icheck=isem, itoggle=isem)
      Call add_menu ('mn', ilev+1, mn_opts_aveens, &
           icheck=ieav, itoggle=ieav)
      Call add_menu ('mn', ilev+1, mn_opts_synch, &
           icheck=isynch, itoggle=isynch)
      Call add_menu ('mn', ilev+1, mn_opts_exclude, &
           cbf=options_exclude)
      Call add_menu ('mn', ilev+1, mn_opts_miss, &
           iset1=imissf, ival1=0, cbf=options_data_missing)
#if GUI == 0
      Call add_menu ('mn', ilev+1, mn_opts_missx, &
           iset1=imissf, ival1=ifile_x, cbf=options_data_missing)
      Call add_menu ('mn', ilev+1, mn_opts_missy, &
           iset1=imissf, ival1=ifile_y, cbf=options_data_missing)
!
! Perform actions
   Else If (iopt == mn_opts_trnsy%id) Then ! - transform Y data -
      If (mn_opts_data%ion /= 0 .and. mn_opts_trnsy%ion /= 0) Then
         iopt = change_igauss()
         If (lmnact) mn_opts_trnsy%iact = mn_opts_trnsy%iact + 1
      End If
   Else If (iopt == mn_opts_zerob%id) Then ! - zero-bound -
      If (mn_opts_data%ion /= 0 .and. mn_opts_zerob%ion /= 0) Then
         iopt = set_zero()
         If (lmnact) mn_opts_zerob%iact = mn_opts_zerob%iact + 1
      End If
   Else If (iopt == mn_opts_sortens%id) Then ! - set sort-ensemble members flag -
      If (mn_opts_data%ion /= 0) Then
         isem = 1 - isem
         mn_opts_sortens%iact = mn_opts_sortens%iact + 1
      End If
   Else If (iopt == mn_opts_miss%id) Then ! - missing value options -
      If (mn_opts_data%ion /= 0) Then
         imissf = 0
         iopt = options_data_missing()
         If (lmnact) mn_opts_miss%iact = mn_opts_miss%iact + 1
      End If
   Else If (iopt == mn_opts_missx%id) Then ! - missing value options -
      If (mn_opts_data%ion /= 0) Then
         imissf = ifile_x
         iopt = options_data_missing()
         If (lmnact) mn_opts_missx%iact = mn_opts_missx%iact + 1
      End If
   Else If (iopt == mn_opts_missy%id) Then ! - missing value options -
      If (mn_opts_data%ion /= 0) Then
         imissf = ifile_y
         iopt = options_data_missing()
         If (lmnact) mn_opts_missy%iact = mn_opts_missy%iact + 1
      End If
   Else If (iopt == mn_opts_synch%id) Then ! - synchronous predictors flag -
      If (mn_opts_data%ion /= 0) Then
         isynch = 1 - isynch
         mn_opts_synch%iact = mn_opts_synch%iact + 1
      End If
   Else If (iopt == mn_opts_aveens%id) Then ! - set ensemble-average flag -
      If (mn_opts_data%ion /= 0) Then
         ieav = 1 - ieav
         mn_opts_aveens%iact = mn_opts_aveens%iact + 1
      End If
   Else If (iopt == mn_opts_exclude%id) Then ! - set ensemble-average flag -
      If (mn_opts_exclude%ion /= 0) Then
         iopt = options_exclude()
         If (lmnact) mn_opts_exclude%iact = mn_opts_exclude%iact + 1
      End If
#endif
   End If
!
   Return
  End Subroutine cpt_menu_opts_data
#if GUI == 1
!
!
!
  Subroutine cpt_menu_opts_config ()
!
! Options ~ Configuration sub-menu
!
! Modules
   Use menus,        Only: mn_opts_config, mn_opts_console, mn_opts_txtscal
   Use menu_options, Only: options_config_console, options_config_text
!
! Executable Statements
!
! Construct Options ~ Configuration menu
   Call add_menu ('mn', ilev, mn_opts_config)
   Call add_menu ('mn', ilev+1, mn_opts_txtscal, &
        iset1=ioc, ival1=1, cbf=options_config_text, scbf='EXIT')
   Call add_menu ('mn', ilev+1, mn_opts_console, &
        iset1=ioc, ival1=1, cbf=options_config_console, scbf='EXIT')
!
   Return
  End Subroutine cpt_menu_opts_config
!
!
!
  Subroutine cpt_menu_opts_graph ()
!
! Options ~ Graphics sub-menu
!
! Modules
   Use gui,          Only: icbw, icrev
   Use menus,        OnlY: mn_opts_bw, mn_opts_cross, mn_opts_fntscal, mn_opts_graph, mn_opts_grphscal, mn_opts_masklak, &
                           mn_opts_masklnd, mn_opts_revcols, mn_opts_scorgrph, mn_opts_shadecat, mn_opts_shadelnd,       &
                           mn_opts_shadesea, mn_opts_stnmrks, mn_opts_vlines
   Use graphics,     Only: icross, imskld, imsklk, iscore, ishdland, ishdsea, itsh, ivl
   Use menu_options, Only: options_graph_font, options_graph_marker, options_graph_size
!
! Executable Statements
!
! Construct Options ~ Graphics menu
   Call add_menu ('mn',   ilev, mn_opts_graph, &
        lsep=.true.)
   Call add_menu ('mn', ilev+1, mn_opts_stnmrks, &
        cbf=options_graph_marker)
   Call add_menu ('mn', ilev+1, mn_opts_fntscal, &
        cbf=options_graph_font)
   Call add_menu ('mn', ilev+1, mn_opts_grphscal, &
        cbf=options_graph_size)
   Call add_menu ('mn', ilev+1, mn_opts_cross, &
        lsep=.true., icheck=icross, itoggle=icross)
   Call add_menu ('mn', ilev+1, mn_opts_scorgrph, &
        icheck=iscore, itoggle=iscore)
   Call add_menu ('mn', ilev+1, mn_opts_vlines, &
        icheck=ivl, itoggle=ivl)
   Call add_menu ('mn', ilev+1, mn_opts_bw, &
        icheck=icbw, itoggle=icbw)
   Call add_menu ('mn', ilev+1, mn_opts_revcols, &
        icheck=icrev, itoggle=icrev)
   Call add_menu ('mn', ilev+1, mn_opts_shadecat, &
        icheck=itsh, itoggle=itsh)
   Call add_menu ('mn', ilev+1, mn_opts_shadelnd, &
        icheck=ishdland, itoggle=ishdland)
   Call add_menu ('mn', ilev+1, mn_opts_shadesea, &
        icheck=ishdsea, itoggle=ishdsea)
   Call add_menu ('mn', ilev+1, mn_opts_masklnd, &
        icheck=imskld, itoggle=imskld)
   Call add_menu ('mn', ilev+1, mn_opts_masklak, &
        icheck=imsklk, itoggle=imsklk)
!
   Return
  End Subroutine cpt_menu_opts_graph
#endif
 End Subroutine cpt_menu_opts
!
!
!
 Subroutine cpt_menu_view (iopt)
!
! Constructs View menu
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, &
                                 ia_dia, ia_for
   Use menus,              Only: mn_view, mn_view_cca, mn_view_pcr, mn_view_mlr, mn_view_gcm, mn_view_pfv, &
                                 mn_view_verifn, mn_view_fcast, mn_view_diagnost, &
                                 add_menu
   Use settings,           Only: isynch, lexpert
   Use analysis,           Only: ianal, ianaln
   Use cpt_view,           Only: change_view
   Use cpt_update,         Only: toggle_fordia
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
! Executable Statements
!
! Construct View menu
! - short menu -
   If (iopt == -mn_view%id) Then
      Call add_menu ('mn', 1, mn_view)
      If (lexpert) Then ! - expert level -
         Call add_menu ('mn', 2, mn_view_cca, &
              iset1=ianal, ival1=ia_cca, scbf='EXIT')
         Call add_menu ('mn', 2, mn_view_pcr, &
              iset1=ianal, ival1=ia_pcr, scbf='EXIT')
         Call add_menu ('mn', 2, mn_view_mlr, &
              iset1=ianal, ival1=ia_mlr, scbf='EXIT')
         Call add_menu ('mn', 2, mn_view_gcm, &
              iset1=ianal, ival1=ia_gcm, scbf='EXIT')
         Call add_menu ('mn', 2, mn_view_pfv, &
              iset1=ianal, ival1=ia_pfv, scbf='EXIT')
      Else ! - beginner level -
         Call add_menu ('mn', 2, mn_view_fcast, &
              iset1=ianal, ival1=ia_cca, iset2=isynch, ival2=0, scbf='EXIT')
         Call add_menu ('mn', 2, mn_view_diagnost, &
              iset1=ianal, ival1=ia_cca, iset2=isynch, ival2=1, scbf='EXIT')
         Call add_menu ('mn', 2, mn_view_verifn, &
              iset1=ianal, ival1=ia_pfv, scbf='EXIT')
      End If
#if GUI == 0
      ianal = 0
#endif
! - extended menu -
   Else If (iopt == mn_view%id) Then
      Call add_menu ('mn', 1, mn_view, &
           lgrey=.true.)
      If (lexpert) Then ! - expert level -
         Call add_menu ('mn', 2, mn_view_cca, &
              lgrey=.true., iset1=ianaln, ival1=ia_cca, cbf=change_view)
         Call add_menu ('mn', 2, mn_view_pcr, &
              lgrey=.true., iset1=ianaln, ival1=ia_pcr, cbf=change_view)
         Call add_menu ('mn', 2, mn_view_mlr, &
              lgrey=.true., iset1=ianaln, ival1=ia_mlr, cbf=change_view)
         Call add_menu ('mn', 2, mn_view_gcm, &
              lgrey=.true., iset1=ianaln, ival1=ia_gcm, cbf=change_view)
         Call add_menu ('mn', 2, mn_view_pfv, &
              lgrey=.true., iset1=ianaln, ival1=ia_pfv, cbf=change_view)
      Else ! - beginner level -
         Call add_menu ('mn', 2, mn_view_fcast, &
              lgrey=.true., iset1=ianaln, ival1=ia_for, cbf=toggle_fordia)
         Call add_menu ('mn', 2, mn_view_diagnost, &
              lgrey=.true., iset1=ianaln, ival1=ia_dia, cbf=toggle_fordia)
         Call add_menu ('mn', 2, mn_view_verifn, &
              lgrey=.true., iset1=ianaln, ival1=ia_pfv, cbf=change_view)
      End If
#if GUI == 0
   Else If (iopt == mn_view_cca%id) Then ! - CCA -
      If (lexpert .and. mn_view%ion /= 0 .and. mn_view_cca%ion /= 0) iopt = view_change(ia_cca)
   Else If (iopt == mn_view_pcr%id) Then ! - PCR -
      If (lexpert .and. mn_view%ion /= 0 .and. mn_view_pcr%ion /= 0) iopt = view_change(ia_pcr)
   Else If (iopt == mn_view_mlr%id) Then ! - MLR -
      If (lexpert .and. mn_view%ion /= 0 .and. mn_view_mlr%ion /= 0) iopt = view_change(ia_mlr)
   Else If (iopt == mn_view_gcm%id) Then ! - GCM -
      If (lexpert .and. mn_view%ion /= 0 .and. mn_view_gcm%ion /= 0) iopt = view_change(ia_gcm)
   Else If (iopt == mn_view_pfv%id) Then ! - PFV -
      If (lexpert .and. mn_view%ion /= 0 .and. mn_view_pfv%ion /= 0) iopt = view_change(ia_pfv)
   Else If (iopt == mn_view_fcast%id) Then ! - Forecast -
      If (.not.lexpert .and. mn_view%ion /= 0 .and. mn_view_fcast%ion /= 0) Then
         ianal = ia_cca
         isynch = 0
         iopt = toggle_fordia()
      End If
   Else If (iopt == mn_view_diagnost%id) Then ! - Diagnostics -
      If (.not.lexpert .and. mn_view%ion /= 0 .and. mn_view_diagnost%ion /= 0) Then
         ianal = ia_cca
         isynch = 1
         iopt = toggle_fordia()
      End If
   Else If (iopt == mn_view_verifn%id) Then ! - Verification -
      If (.not.lexpert .and. mn_view%ion /= 0 .and. mn_view_verifn%ion /= 0) Then
         ianal = ia_pfv
         iopt = view_change(ia_pfv)
      End If
   Else
      iopt = -1
#endif
   End If
!
   Return
#if GUI == 0
!
 Contains
!
!
  Function view_change(ia) &
           Result (view)
!
! Change view
!
! Function result
   Integer :: view
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ia ! - analysis indicator -
!
! Executable Statements
!
! Change view
   ianaln = ia
   view = change_view()
!
   Return
  End Function view_change
#endif
 End Subroutine cpt_menu_view
!
!
!
 Subroutine cpt_menu_help (iopt)
!
! Modules
   Use menus,  Only: mn_help, mn_help_about, &
                     add_menu
#if GUI == 1
   Use menus,  Only: mn_help_help
   Use help,   Only: help_all
#endif
   Use help,   Only: help_about
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: iopt ! - selected option -
!
#if GUI == 0
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
#endif
! Executable Statements
!
! Construct Help menu
#if GUI == 0
   If (Abs(iopt) == mn_help%id) Then
#endif
      Call add_menu ('mn', 1, mn_help)
#if GUI == 1
      Call add_menu ('mn', 2, mn_help_help, &
           cbf=help_all)
#endif
      Call add_menu ('mn', 2, mn_help_about, &
           cbf=help_about)
#if GUI == 0
   Else If (iopt == mn_help_about%id) Then ! - help about -
      iopt = help_about()
   Else
      iopt = -1
   End If
#endif
!
   Return
 End Subroutine cpt_menu_help
!
!
!
 Function output_results()
!
! Outputs results
!
! Modules
   Use cpt_output, Only: save_data
   Use prediction, Only: ifc
   Use analysis,   Only: ianal, ihind
!
! Function result
   Integer :: output_results
!
! Executbale Statements
!
! Output results
   output_results = save_data(ianal, ihind, ifc)
!
   Return
 End Function output_results
End Module cpt_interface
