! Author: Simon Mason
Module projects
!
! Modules
   Use data_numbers,       Only: sp
   Use data_cpt_constants, Only: na
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: close_project, get_project_startup, init_sets, new_project, open_project, save_project, saveas_project, &
             reset_project
#if GUI == 1
   Public :: restore_defs
#endif
!
! Parameters
!
   Integer, Parameter, Private :: sup_ver = 16.0_sp ! - earliest supported version -
!
! Derived type definitions
!
! - project files -
   Public :: proj
   Type proj
      Sequence
      Character(Len=186) :: pfile ! - file -
      Character(Len=150) :: pdir  ! - file directory -
      Character(Len= 36) :: pname ! - file name -
!
      Logical :: lproj            ! - file set? -
   End Type proj
!
! Arrays
!
! Character arrays
   Character(Len= 21), Private :: pdescs(na) ! - project file descriptions -
   Character(Len=  5), Private :: pfilts(na) ! - project filters -
!
! Scalars
!
! Integer scalars
   Integer, Public :: jproj  ! - save project file as (1) Parameter File or (0) CPT Project -
   Integer, Public :: ianalp ! - project analysis type -
!
! Character scalars
   Character(Len=150), Private :: p_dir ! - project file directory -
   Character(Len= 21), Private :: sdesc ! - parameter file description -
   Character(Len=  5), Private :: sfilt ! - parameter file filter -
!
   Character(Len=150), Public :: pdir ! - default project file directory -
!
! Derived type scalars
   Type(proj), Public :: this_proj ! - current project name -
   Type(proj), Public :: new_proj  ! - new project file -
!
Contains
!
!
 Subroutine init_projects (lnew)
!
! Initialises project file
!
! Modules
   Use labels, Only: ca_atypes_a
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lnew ! - reset new project? -
!
! Locals
!
! Local scalars
   Integer :: i ! - project type index -
!
! Executable Statements
!
! Initialise project file settings
! - generic project -
   pdescs(1) = 'CPT Project File'
   pfilts(1) = '*.CPT'
! - other projects -
   Do i = 1, na-1
      pdescs(i+1) = 'Old '//ca_atypes_a(i)//' Project Files'
      pfilts(i+1) = '*.'//ca_atypes_a(i)
   End Do
! - Parameter Files -
   sdesc = 'CPT Parameter File'
   sfilt = '*.txt'
!
! Initialise current project
   p_dir = pdir
   Call reset_project (this_proj)
   this_proj%pdir = p_dir
   If (lnew) Then
      Call reset_project (new_proj)
      new_proj%pdir = p_dir
   End If
!
   Return
 End Subroutine init_projects
!
!
!
Subroutine init_sets ()
!
! Initialises CPT default settings
!
! Modules
   Use time,       Only: reset_time
   Use screen,     Only: init_gfonts, window_update
   Use gui,        Only: set_win_title
#if GUI == 1
   Use gui,        Only: reset_jpg
#endif
   Use settings,   Only: iregr, istd, isynch, nf, nt, &
                         reset_settings, training_refresh
   Use space,      Only: reset_space
   Use iofiles,    Only: xfile, yfile, zfile, &
                         reset_iofiles
   Use fields,     Only: nmfd, &
                         reset_fields
   Use pcs,        Only: init_pcs
   Use regression, Only: reset_regression
   Use scores,     Only: reset_scores
   Use season,     Only: icsv, &
                         reset_season
   Use missing,    Only: xmiss,ymiss
   Use categories, Only: init_refdates, init_icat
   Use climate,    Only: ncy, &
                         reset_climate
   Use prediction, Only: reset_predict
   Use analysis,   Only: ianal, icalc, &
                         reset_analysis
   Use markers,    Only: init_marker
#if GUI == 1
   Use graphics,   Only: reset_graphics
#endif
   Use cpt_view,   Only: set_menuflags
   Use bootstrap,  Only: reset_bootstrap
   Use verif,      Only: reset_verif
   Use cpt_update, Only: reset_limits
#if GUI == 1
   Use cpt_file,   Only: files_refresh
#endif
   Use downloads,  Only: init_download
   Use user,       Only: cpt_ini, &
                         read_ini
!
! Implicit declarations
   Implicit None
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Read customised defaults
   Call read_ini (Trim(cpt_ini), pdir)
!
! Update window
   Call training_refresh (ncy)
!
! Initialise settings
   Call reset_settings (ianal)
!
! Reset input file settings
   Call reset_iofiles ()
!
! Reset field settings
   Call reset_fields (xmiss, ymiss, ifail)
   If (ifail /= 0) Return
   nmfd = 0
!
! Reset space settings
   Call reset_space ()
!
#if GUI == 1
! Reset graphics
   Call reset_graphics ()

! Update window
   Call files_refresh ()
!
#endif
! Reset graphics font scaling
   Call init_gfonts ()
!
! Initialise regression
   Call reset_regression ()
!
! Initialise analysis settings
   Call reset_analysis ()
!
! Current EOFs
   Call init_pcs ()
!
! Scores selections
   Call reset_scores ()
!
! Switch off climatological period
   Call reset_climate ()
!
! Initialise category indicator
   Call init_icat ()
!
! Switch off reference years
   Call init_refdates ()
!
! Reset marker settings
   Call init_marker ()
!
! Reset verification settings
   Call reset_verif ()
!
! Reset forecast settings
   Call reset_predict ()
!
! Reset time settings
   Call reset_time (.true.)
!
! Initialise seasonal values settings
   Call reset_season (.false., .true.)
   Call reset_season (.true., .false.)
!
! Initialise download settings
   Call init_download ()
!
! Initialise resampling settings
   Call reset_bootstrap ()
!
! Set output as CPT project file 
   jproj = 0
!
#if GUI == 1
! Initialise JPEG quality
   Call reset_jpg ()
#endif
!
! Set view flags
   Call set_menuflags (ianal, icalc, icsv, iregr, istd, isynch, &
        ixdstr=xfile%idstr, iydstr=yfile%idstr, izdstr=zfile%idstr)
!
! Reset window caption and update training period settings
   Call set_win_title (ianal)
   If (ianal > 0) Then
      Call reset_limits (0, .false., .false., 0, [0], 0, 0)
      Call window_update (nt)
      Call window_update (nf)
   End If
!
   Return
End Subroutine init_sets
#if GUI == 1
!
!
!
 Function restore_defs()
!
! Deletes user copy of CPT.ini and creates a new version with latest defaults
!
! Modules
   Use data_io_constants, Only: iin
   Use gui,               Only: print_advisory, print_warning
   Use user,              Only: cpt_ini, &
                                get_user
!
! Function result
   Integer :: restore_defs
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Exectuable Statements
!
! Delete user copy of CPT.ini
   Open (Unit=iin, File=cpt_ini, Status='old')
   Close (Unit=iin, Status='delete', IOStat=ifail)
!
   If (ifail == 0) Then
      Call get_user (pdir)
      Call print_advisory ('Defaults restored')
   Else
      Call print_warning (.true., 'Unable to restore defaults')
   End If
   restore_defs = 1
!
   Return
 End Function restore_defs
#endif
!
!
!
 Subroutine get_project_startup ()
!
! Identifies project as a command-line argument
!
! Modules
   Use iofiles, Only: parse_file
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Character(Len=8) :: cdum ! - dummy argument to remove a possible project file extension -
!
   Logical :: le ! - file exists? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Get_Command_Argument, Len_Trim
!
! Executable Statements
!
! Check for comman-line argument
   new_proj%pfile = ' '
   Call Get_Command_Argument (1, &
        value=new_proj%pfile)
   If (Len_Trim(new_proj%pfile) > 0) Then
!
! Identifier whether argument is an existing project file
      Inquire (File=new_proj%pfile, Exist=le)
      If (.not.le) GoTo 1
      Call parse_file (new_proj%pfile, new_proj%pdir, new_proj%pname, ifail, &
           fext_new=cdum)
      If (ifail /= 0) GoTo 1
!
! Attempt to read project file
      If (get_project() /= 0) GoTo 1
   End If
   Call init_projects (.false.)
   Return
!
! Errors
1  Call init_projects (.true.)
   Return
 End Subroutine get_project_startup
!
!
!
 Function open_project()
!
! Opens a project
!
! Modules
#if GUI == 1
   Use clrwin$,            Only: clear_window$
#endif
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: na, na10
   Use screen,             Only: window_update
#if GUI == 1
   Use gui,                Only: ih_con
#endif
   Use gui,                Only: set_cursor_waiting, set_win_title
   Use menus,              Only: isave, &
                                 set_menu_greying
   Use progress_meter,     Only: set_progress_meter
   Use settings,           Only: training_refresh
   Use iofiles,            Only: fext, nx, ny, xfile, yfile, &
                                 get_old_file
   Use climate,            Only: ncy
   Use analysis,           Only: ianal, icalc
   Use menu_actions,       Only: reset
   Use cpt_file,           Only: files_refresh
!
! Function result
   Integer :: open_project
!
! Locals
!
! Local scalars
   Integer :: ifail    ! - error indicator -
   Integer :: icalc_bk ! - backup calculation flag -
!
   Logical :: lts ! - title screen? -
!
! Executable Statements
!
! Copy settings
   open_project = 1
   ianalp = ianal
   Select Case (ianal)
    Case (1:na)
      lts = .false.
      If (isave >= 2) Then
         isave = isave + 2
         Call set_menu_greying (isave=isave)
      End If
      icalc_bk = icalc
      If (close_project() == 1) Return
      icalc = icalc_bk
      open_project = reset(' ', .true.)
      icalc = 0
    Case Default
      lts = .true.
      open_project = 0
   End Select
   Call reset_project (new_proj)
!
! Select project file
   open_project = 2
   Select Case (ianal)
    Case (1:na10)
      Call get_old_file (pdescs(1), pdescs(1:ianal+1:ianal), pfilts(1:ianal+1:ianal), 2, new_proj%pdir, &
           new_proj%pfile, new_proj%pname, ifail, &
           fext=fext)
    Case (na10+1:na)
      Call get_old_file (pdescs(1), [pdescs(1)], [pfilts(1)], 1, new_proj%pdir, &
           new_proj%pfile, new_proj%pname, ifail, &
           fext=fext)
    Case Default
      Call get_old_file (pdescs(1), pdescs, pfilts, na10+1, new_proj%pdir, &
           new_proj%pfile, new_proj%pname, ifail, &
           fext=fext)
   End Select
   If (ifail /= 0) Return
!
! Open project file
   If (get_project() /= 0) Return
!
! Update window
   Call files_refresh ()
   Call training_refresh (ncy)
   Call files_refresh () ! - duplicate call for FTN95 BUG -
   If (icalc == 1) Call set_progress_meter (one)
   If (lts) Then
      open_project = 0
   Else
#if GUI == 1
      Call clear_window$ (ih_con)
#endif
      Print *, ' '
!
! Indicate numbers of variables
      If (xfile%cfile%lset) Call window_update (nx)
      If (yfile%cfile%lset) Call window_update (ny)
      open_project = 1
   End If
   Call window_update (this_proj%pname)
!
! Update CPT window title
   Call set_win_title (ianal, &
        cproj=this_proj%pname)
   Call set_cursor_waiting (0)
!
   Return
 End Function open_project
!
!
!
 Function get_project()
!
! Opens a project
!
! Modules
   Use data_numbers,       Only: sp, zero_sp, tolv
   Use data_version,       Only: ver
   Use data_cpt_constants, Only: ifile_x, ifile_y
   Use data_io_constants,  Only: iin, lfil
   Use labels,             Only: ca_dstruct
   Use time,               Only: iseq
   Use gui,                Only: set_cursor_waiting, set_win_title
   Use menus,              Only: isave, &
                                 set_menu_greying
   Use errors,             Only: cpt_error
   Use settings,           Only: icv2, iregr, iregr_bk, iretro, istd, isynch
   Use iofiles,            Only: xfile, yfile, zfile, &
                                 files_open, open_iofile
   Use fields,             Only: dsdy
   Use season,             Only: icsv, lb, lensn, lprst, lsn, nsn
   Use cpt_output,         Only: ipreclx, iprecly
   Use prediction,         Only: ifc, &
                                 set_ifcast
   Use analysis,           Only: ianal, icalc, &
                                 analysis_flags
   Use cpt_view,           Only: grey_menu_flags, set_menuflags
   Use verif,              Only: n_verif
   Use menu_actions,       Only: reset_mem
   Use cpt_update,         Only: reset_limits
   Use cpt_file,           Only: ixyz, &
                                 close_file
!
! Function result
   Integer :: get_project
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Real(Kind=sp) :: pver ! - project version number -
!
   Character(Len=lfil) :: cfail ! - project file read location error -
!
   Logical :: lwarn ! - warning? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Open project file
   get_project = 1
   Call set_cursor_waiting (1)
   Call open_iofile (iin, new_proj%pfile, .true., .false., .false., ifail)
   If (ifail /= 0) Then
      Call cpt_error ('open_iofile', .false., ifail, &
           c_arg1=new_proj%pfile)
      Return
   End If
!
! Read version number
   pver = zero_sp
   cfail = ' '
   Read (Unit=iin, Err=1, End=2) pver
! - project files are newer than CPT -
   If (pver > ver+tolv) Then
      ifail = 2
      GoTo 3
! - supported files -
   Else If (pver > sup_ver) Then
      isave = 0
      Call set_menu_greying (isave=isave)
      ifail = read_project(pver, cfail)
! - old files -
   Else
      ifail = 7
      cfail = ' '
      GoTo 3
   End If
! - errors -
   Select Case (ifail)
    Case (0) ! - project read successfully -
      If (icalc == 1) Then
         Call analysis_flags ('end')
      Else
         Call analysis_flags ('off')
      End If
      lwarn = .false.
    Case (3) ! - problem reading project file -
      Call analysis_flags ('off')
      get_project = reset_mem()
      GoTo 3
    Case (4) ! - problem reading project file results -
      Call analysis_flags ('off')
      get_project = reset_mem()
      ifail = 0
      lwarn = .false.
    Case (5) ! - input file is protected -
      Call analysis_flags ('off')
      get_project = reset_mem()
      ifail = 8
      GoTo 3
    Case (9) ! - old monthly file -
      ixyz = ifile_y
      get_project = close_file()
      Call analysis_flags ('off')
      lwarn = .true.
    Case (10) ! - old subseasonal file(s) -
      ixyz = ifile_x
      get_project = close_file()
      ixyz = ifile_y
      get_project = close_file()
      Call analysis_flags ('off')
      lwarn = .true.
    Case Default
      Call analysis_flags ('off')
      get_project = reset_mem()
      ifail = ifail + 2
      GoTo 3
   End Select
   Close (Unit=iin)
!
! Reset analysis type if necessary
   If (ianalp /= 0) Then
      ianal = ianalp
   Else If (ianal > 0) Then
      isave = 1
      Call set_menu_greying (isave=isave)
   End If
!
! Initialise latitude-longitude output precision
   ipreclx = 0
   iprecly = 0
!
! Update settings
   Call set_menuflags (ianal, icalc, icsv, iregr, istd, isynch, &
        lscree=.true., icv2=icv2, ixdstr=xfile%idstr, iydstr=yfile%idstr, izdstr=zfile%idstr, iretro=iretro)
   iregr_bk = iregr
   new_proj%lproj = .true.
   this_proj = new_proj
   If (yfile%cfile%lset) Then
      dsdy = ca_dstruct(yfile%idstr)
      Call set_menu_greying (lclim=.true.)
      iseq = yfile%iseq
   Else If (xfile%cfile%lset) Then
      iseq = xfile%iseq
   End If
   If (files_open(xfile%idstr, yfile%idstr)) Continue
   Call grey_menu_flags (icalc, xfile%idstr, yfile%idstr)
   If (icalc == 1) Call set_ifcast (ifc=ifc)
   Call n_verif ()
!
! Reset training- and forecast-period limits
   Call reset_limits (icsv, lprst, .false., lensn, lsn(lb:), lb, nsn)
!
! Update CPT window title
   Call set_win_title (ianal, &
        cproj=this_proj%pname)
   Call set_cursor_waiting (0)
   get_project = 0
   If (lwarn) GoTo 4
!
   Return
!
! Errors in project file
1  ifail = 3
   GoTo 3
!
2  ifail = 4
!
3  Close (Unit=iin)
!
! Reset
   get_project = new_project()
!
4  If (pver > zero_sp) Then
      Call cpt_error ('get_project', .false., ifail, &
           r_arg1=pver, c_arg1=Trim(cfail))
   Else
      Call cpt_error ('get_project', .false., ifail, &
           c_arg1=Trim(cfail))
   End If
!
   Return
 End Function get_project
!
!
!
 Function read_project(pver, cfail)
!
! Reads a CPT project
!
! Modules
   Use data_numbers,        Only: sp, tolv
   Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_gcm, ia_pfv
   Use data_io_constants,   Only: iin
   Use data_time_constants, Only: isq_mn
   Use time,                Only: iafter, nse, nss
   Use screen,              Only: proj_read_screen
#if GUI == 1
   Use gui,                 Only: set_colour_direction
#endif
   Use settings,            Only: icv2, igauss, iretro, nt, nur, xfd_old, yfd_old, &
                                  proj_read_settings
   Use arrays,              Only: proj_read_results
   Use space,               Only: proj_read_interp, proj_read_space
   Use iofiles,             Only: xfile, yfile, zfile, &
                                  proj_read_iofiles
   Use fields,              Only: xfield, yfield, &
                                  proj_read_fields
   Use distribs,            Only: proj_read_distribs
   Use pcs,                 Only: proj_read_pcs
   Use regression,          Only: dofr, &
                                  proj_read_regression
   Use season,              Only: lb, lsn, nsn, nsnc, &
                                  proj_read_season
   Use missing,             Only: xmiss, ymiss, &
                                  proj_read_missing1, proj_read_missing2
   Use categories,          Only: proj_read_categories
   Use climate,             Only: proj_read_climate
   Use cpt_input,           Only: proj_read_data
   Use prediction,          Only: proj_read_predict
   Use analysis,            Only: icalc, ihind, &
                                  proj_read_analysis
   Use pfv,                 Only: proj_read_pfv
   Use markers,             Only: proj_read_markers
   Use bootstrap,           Only: proj_read_bootstrap
   Use verif,               Only: proj_read_verif
!
! Function result
   Integer :: read_project
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=sp), Intent(In) :: pver ! - project version number -
!
! - output scalars -
   Character(Len=*), Intent(Out) :: cfail ! - project file read location error -
!
! Locals
!
! Local scalars
   Integer :: iold   ! - old monthly input file flag -
   Integer :: nur_bk ! - backup number -
   Integer :: ifail  ! - error indicator -
!
   Logical :: lpfv ! - PFV? -
!
! Executable Statements
!
! Read analysis type
   lpfv = (ianalp == ia_pfv)
   cfail = ' '
   Call proj_read_analysis (iin, .false., ianalp, ifail)
   If (ifail /= 0) Then
      cfail = 'analysis settings'
      GoTo 1
   End If
!
! Read program settings
   Call proj_read_settings (iin, ianalp, icalc, pver, dofr, ifail)
   If (ifail /= 0) Then
      cfail = 'program settings'
      GoTo 1
   End If
!
! Read bootstrap settings
   Call proj_read_bootstrap (iin, ifail)
   If (ifail /= 0) Then
      cfail = 'bootstrap settings'
      GoTo 1
   End If
!
! Read input file settings
   Call proj_read_iofiles (iin, pver, iafter, iold, xfd_old, yfd_old, ifail)
   Select Case (ifail)
    Case (0)
      Continue
    Case (6, 7)
      icalc = 0
    Case Default
      cfail = 'input file settings'
      GoTo 1
   End Select
!
! Read season settings
   Call proj_read_season (iin, ianalp, pver, ifail)
   If (ifail /= 0) Then
      cfail = 'season settings'
      GoTo 1
   End If
!
! Initialise and read field settings
   Call proj_read_fields (iin, pver, lpfv, xfile%nfl*nse, yfile%nfl*nsn*nse, zfile%nfl*nse, xmiss, ymiss, xmiss, ifail)
   If (ifail /= 0) Then
      cfail = 'field settings'
      GoTo 1
   End If
#if GUI == 1
! - restore colour direction -
   If (yfile%cfile%lset) Call set_colour_direction (yfield(1)%var)
#endif
!
! Initialise and read domain settings
   Call proj_read_space (iin, xfile%cfile%lset, xfile%idstr, xfile%igeog, xfile%nfl, xfield(:)%nlt, xfield(:)%nlg, &
                              yfile%cfile%lset, yfile%idstr, yfile%igeog, yfile%nfl, yfield(:)%nlt, yfield(:)%nlg, &
        ifail)
   If (ifail /= 0) Then
      cfail = 'domain settings'
      GoTo 1
   End If
!
! Initialise and read missing value settings
   Call proj_read_missing1 (iin, nss, pver, xfile%cfile%lset, xfile%ntm, xfile%nfl, &
                                            yfile%cfile%lset, yfile%ntm, yfile%nfl, &
                                            zfile%cfile%lset, zfile%ntm, zfile%nfl, ifail)
   If (ifail /= 0) Then
      cfail = 'missing value settings'
      GoTo 1
   End If
!
! Read climatology settings
   Call proj_read_climate (iin, lb, nt, nsnc, lsn(0), .false., ifail)
   If (ifail /= 0) Then
      cfail='climatology settings'
      GoTo 1
   End If
!
! Read category settings
   Call proj_read_categories (iin, 0, 0, 0, nsn, lpfv, .false., ifail)
   If (ifail /= 0) Then
      cfail = 'category settings'
      GoTo 1
   End If
!
! Read GUI settings
   Call proj_read_pcs (iin, ianalp, icalc, .false., ifail)
   If (ifail /= 0) Then
      cfail = 'GUI settings'
      GoTo 1
   End If
!
! Read verification settings
   Call proj_read_verif (iin, ifail)
   If (ifail /= 0) Then
      cfail = 'verification settings'
      GoTo 1
   End If
!
! Read screen settings
   Call proj_read_screen (iin, ifail)
   If (ifail /= 0) Then
      cfail = 'screen settings'
      GoTo 1
   End If
!
! Read marker settings
   Call proj_read_markers (iin, ifail)
   If (ifail /= 0) Then
      cfail = 'marker settings'
      GoTo 1
   End If
!
! Do not read results if old monthly file is open
   Select Case (iold)
    Case (0)
      Continue
    Case (isq_mn) ! - old monthly file -
      icalc = 0
      cfail = yfile%cfile%ffile
      ifail = 9
      GoTo 3
    Case (1:) ! - old subseasonal file(s) -
      icalc = 0
      If (xfile%cfile%lset) Then
         cfail = xfile%cfile%ffile
      Else
         cfail = yfile%cfile%ffile
      End If
      ifail = 10
      GoTo 3
   End Select
!
! Read input data
   If (icalc == 1) Then
      If (pver < 17.04_sp-tolv) Then
         cfail = 'results'
         GoTo 4
      End If
      Call proj_read_data (iin, ianalp, lb, nss, ifail)
      If (ifail /= 0) Then
         cfail = 'input data'
         GoTo 4
      End If
!
! Read missing value indicators
      Call proj_read_missing2 (iin, ifail)
      If (ifail /= 0) Then
         cfail = 'missing value indicators'
         GoTo 4
      End If
!
! Read climatological data
      Call proj_read_climate (iin, lb, nt, nsnc, lsn(0), .true., ifail)
      If (ifail /= 0) Then
         cfail = 'climatological data'
         GoTo 4
      End If
!
! Allocate memory and read optimization settings
      If (.not.lpfv) Then
         Call proj_read_analysis (iin, .true., ianalp, ifail)
         If (ifail /= 0) Then
            cfail = 'optimization results'
            GoTo 4
         End If
      Else
         Call proj_read_pfv (ifail)
         nur_bk = nur
      End If
!
! Read category data
      Call proj_read_categories (iin, ihind, icv2, iretro, nsn, .false., .true., ifail)
      If (ifail /= 0) Then
         cfail = 'categories'
         GoTo 4
      End If
!
! Read distribution parameters
      Call proj_read_distribs (iin, igauss, ifail)
      If (ifail /= 0) Then
         cfail = 'distribution parameters'
         GoTo 4
      End If
!
! Read modes
      Select Case (ianalp)
       Case (ia_cca, ia_pcr)
         Call proj_read_pcs (iin, ianalp, icalc, .true., ifail)
         If (ifail /= 0) Then
            cfail = 'GUI results'
            GoTo 4
         End If
!
! Read interpolation settings
       Case (ia_gcm, ia_pfv)
         Call proj_read_interp (iin, ifail)
         If (ifail /= 0) Then
            cfail = 'interpolation settings'
            GoTo 4
         End If
      End Select
!
! Read results
      Call proj_read_results (iin, ianalp, ifail)
      If (ifail /= 0) Then
         cfail = 'results'
         GoTo 4
      End If
      Call proj_read_regression (iin, ianalp, igauss, ihind, icv2, iretro, ifail)
      If (ifail /= 0) Then
         cfail = 'regression'
         GoTo 4
      End If
      Call proj_read_predict (iin, ianalp, igauss, ihind, ifail)
      If (ifail /= 0) Then
         cfail = 'forecasts'
         GoTo 5
      End If
      If (lpfv) nur = nur_bk
   End If
!
   read_project = 0
   Return
!
! Errors in project file
! - problems initializing memory -
1  read_project = ifail
   Return
!
! - old input file(s) -
3  read_project = ifail
   Return
!
! - problem initializing memory for results -
4  read_project = 4
   Return
!
! - problem initializing memory for forecasts -
5  read_project = 0
   Return
!
 End Function read_project
!
!
!
 Function save_project() &
          Result (saves)
!
! Determines whether to save or save-as a project
!
! Modules
   Use menus, Only: isave
!
! Function result
   Integer :: saves
!
! Executable Statements
!
! Check whether project is active
   If (this_proj%lproj) Then
      Select Case (jproj)
       Case (0) ! - CPT project file -
         If (isave > 0) Then
            saves = write_project()
         End If
       Case (1) ! - parameter file -
         saves = write_parameter()
      End Select
   Else
      Select Case (jproj)
       Case (0) ! - CPT project file -
         If (isave == -1) Then
            saves = write_project()
         Else
            saves = saveas_project()
         End If
       Case (1) ! - parameter file -
         If (isave == -1) Then
            saves = write_parameter()
         Else
            saves = saveas_parameter()
         End If
      End Select
   End If
!
   saves = 2
!
   Return
 End Function save_project
!
!
!
 Function saveas_project() &
          Result (saves)
!
! Prompts for a project name and saves the project (i.e., save as)
!
! Modules
   Use gui,      Only: set_win_title
   Use iofiles,  Only: get_new_file
   Use analysis, Only: ianal
!
! Function result
   Integer :: saves
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Select project file
   Call get_new_file (Trim(pdescs(1)), .true., pfilts(1), ['CPT Project Files'], [pfilts(1)], 1, &
        this_proj%pdir, this_proj%pfile, this_proj%pname, ifail)
!
! Update CPT window title
   If (ifail == 0) Then
      this_proj%lproj = .true.
      Call set_win_title (ianal, &
           cproj=this_proj%pname)
      saves = write_project()
   End If
   saves = 2
!
   Return
 End Function saveas_project
!
!
!
 Function saveas_parameter() &
          Result (saves)
!
! Prompts for a project name and saves the project (i.e., save as)
!
! Modules
   Use iofiles, Only: get_new_file
!
! Function result
   Integer :: saves
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Select project file
   Call get_new_file (Trim(sdesc), .true., sfilt, ['CPT Parameter Files'], [sfilt], 1, &
        this_proj%pdir, this_proj%pfile, this_proj%pname, ifail)
!
! Save
   If (ifail == 0) saves = write_parameter()
   saves = 2
!
   Return
 End Function saveas_parameter
!
!
!
 Function write_project() &
          Result (psave)
!
! Saves a project
!
! Modules
   Use data_version,       Only: ver
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_gcm, ia_pfv
   Use data_io_constants,  Only: iout
   Use time,               Only: iafter, nse
   Use screen,             Only: proj_write_screen
   Use menus,              Only: isave, &
                                 set_menu_greying
   Use errors,             Only: cpt_error
   Use settings,           Only: icv2, igauss, iretro, &
                                 proj_write_settings
   Use arrays,             Only: proj_write_results
   Use space,              Only: proj_write_interp, proj_write_space
   Use iofiles,            Only: xfile, yfile, zfile, &
                                 open_iofile, proj_write_iofiles
   Use fields,             Only: proj_write_fields
   Use distribs,           Only: proj_write_distribs
   Use pcs,                Only: proj_write_pcs
   Use regression,         Only: proj_write_regression
   Use season,             Only: nsn, &
                                 proj_write_season
   Use missing,            Only: proj_write_missing1, proj_write_missing2
   Use categories,         Only: proj_write_categories
   Use climate,            Only: proj_write_climate
   Use cpt_input,          Only: proj_write_data
   Use prediction,         Only: proj_write_predict
   Use analysis,           Only: ianal, icalc, ihind, &
                                 proj_write_analysis
   Use markers,            Only: proj_write_markers
   Use bootstrap,          Only: proj_write_bootstrap
   Use verif,              Only: proj_write_verif
!
! Function result
   Integer :: psave
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Open project file
   Call open_iofile (iout, this_proj%pfile, .false., .false., .false., ifail)
   If (ifail /= 0) Then
      Call cpt_error ('open_iofile', .false., ifail, &
           c_arg1=this_proj%pfile)
      psave = 2
      Return
   End If
!
! Save version number
   Write (Unit=iout, Err=1) ver
!
! Save settings
! - analysis settings -
   Call proj_write_analysis (iout, .false., ifail)
   If (ifail /= 0) GoTo 1
! - program settings -
   Call proj_write_settings (iout, icalc, ifail)
   If (ifail /= 0) GoTo 1
! - bootstrap settings -
   Call proj_write_bootstrap (iout, ifail)
   If (ifail /= 0) GoTo 1
! - input files -
   Call proj_write_iofiles (iout, iafter, ifail)
   If (ifail /= 0) GoTo 1
! - season settings -
   Call proj_write_season (iout, ifail)
   If (ifail /= 0) GoTo 1
! - field settings -
   Call proj_write_fields (iout, xfile%nfl*nse, yfile%nfl*nsn*nse, zfile%nfl*nse, ifail)
   If (ifail /= 0) GoTo 1
! - domain settings -
   Call proj_write_space (iout, xfile%cfile%lset, xfile%idstr, xfile%igeog, yfile%cfile%lset, yfile%idstr, yfile%igeog, ifail)
   If (ifail /= 0) GoTo 1
! - missing value settings -
   Call proj_write_missing1 (iout, xfile%cfile%lset, yfile%cfile%lset, zfile%cfile%lset, ifail)
   If (ifail /= 0) GoTo 1
! - climatology settings -
   Call proj_write_climate (iout, .false., ifail)
   If (ifail /= 0) GoTo 1
! - category settings -
   Call proj_write_categories (iout, 0, 0, 0, .false., ifail)
   If (ifail /= 0) GoTo 1
! - GUI settings -
   Call proj_write_pcs (iout, ianal, icalc, .false., ifail)
   If (ifail /= 0) GoTo 1
! - verification settings -
   Call proj_write_verif (iout, ifail)
   If (ifail /= 0) GoTo 1
! - screen settings -
   Call proj_write_screen (iout, ifail)
   If (ifail /= 0) GoTo 1
! - marker settings -
   Call proj_write_markers (iout, ifail)
   If (ifail /= 0) GoTo 1
! - input data -
   If (icalc == 1) Then
      Call proj_write_data (iout, ifail)
      If (ifail /= 0) GoTo 1
! - missing value indicators -
      Call proj_write_missing2 (iout, ifail)
      If (ifail /= 0) GoTo 1
! - climatological data -
      Call proj_write_climate (iout, .true., ifail)
      If (ifail /= 0) GoTo 1
! - optimization results -
      If (ianal /= ia_pfv) Then
         Call proj_write_analysis (iout, .true., ifail)
         If (ifail /= 0) GoTo 1
      End If
! - category data -
      Call proj_write_categories (iout, ihind, icv2, iretro, .true., ifail)
      If (ifail /= 0) GoTo 1
! - distribution parameters -
      Call proj_write_distribs (iout, igauss, ifail)
      If (ifail /= 0) GoTo 1
! - modes -
      Select Case (ianal)
       Case (ia_cca, ia_pcr)
         Call proj_write_pcs (iout, ianal, icalc, .true., ifail)
! - interpolation settings -
       Case (ia_gcm, ia_pfv)
         Call proj_write_interp (iout, ifail)
      End Select
      If (ifail /= 0) GoTo 1
! - results -
      Call proj_write_results (iout, ianal, ifail)
      If (ifail /= 0) GoTo 1
! - regression results -
      Call proj_write_regression (iout, ianal, igauss, ihind, icv2, iretro, ifail)
      If (ifail /= 0) GoTo 1
! - forecasts -
      Call proj_write_predict (iout, ianal, igauss, ihind, ifail)
      If (ifail /= 0) GoTo 1
   End If
   Close (Unit=iout)
!
! Confirm
   psave = 5
   Call cpt_error ('write_project', .false., psave, &
        c_arg1=Trim(this_proj%pname)//pfilts(1)(2:), c_arg2=Trim(this_proj%pdir))
   p_dir = this_proj%pdir
   isave = 0
   Call set_menu_greying (isave=isave)
!
   psave = 2
   Return
!
! Error saving project file
1  Close (Unit=iout)
   ifail = 4
   Call cpt_error ('write_project', .false., ifail)
   psave = 2
!
   Return
 End Function write_project
!
!
!
 Function write_parameter() &
          Result (psave)
!
! Saves a project as a parameter file
!
! Modules
   Use data_version,       Only: cpt_ver
   Use data_cpt_constants, Only: ia_cca, ia_non, ia_nul, ia_pcr
   Use data_io_constants,  Only: iout
   Use labels,             Only: ls_analyses
#if GUI == 0
   Use labels,             Only: ls_uerropts
   Use gui,                Only: iprompt
   Use menus,              Only: imenus, mn_opts_errors, mn_opts_menus
#endif
   Use menus,              Only: isave, &
                                 set_menu_greying
   Use errors,             Only: cpt_error
   Use parameter_file,     Only: output_parameter
   Use settings,           Only: lcw, nt, &
                                 parameter_write_settings
   Use iofiles,            Only: nx, ny, &
                                 open_iofile, parameter_write_iofiles
   Use fields,             Only: xfield, yfield
   Use pcs,                Only: lcde, nxe, mxe, nye, mye, ncc, mcc, &
                                 max_cca_modes, max_eof_modes, parameter_write_pcs
   Use season,             Only: icsv, ispi, lsn
   Use missing,            Only: parameter_write_missing
   Use categories,         Only: parameter_write_categories
   Use climate,            Only: parameter_write_climate
   Use prediction,         Only: parameter_write_predict
   Use analysis,           Only: ianal, icalc
   Use bootstrap,          Only: parameter_write_boot
   Use forecast,           Only: parameter_write_forecast
   Use menu_actions,       Only: parameter_write_action
!
! Function result
   Integer :: psave
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Logical :: lxmodes ! - prompt for X EOF modes? -
   Logical :: lymodes ! - prompt for Y EOF modes? -
   Logical :: lcmodes ! - prompt for CCA modes? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Open parameter file
   psave = 1
   Call open_iofile (iout, this_proj%pfile, .false., .true., .false., ifail)
   If (ifail /= 0) Then
      Call cpt_error ('open_iofile', .false., ifail, &
           c_arg1=this_proj%pfile)
      psave = 2
      Return
   End If
!
! Save parameter settings
! - CPT version -
   If (output_parameter(iout, cdesc='CPT version', c='# '//Trim(cpt_ver)) /= 0) GoTo 1
#if GUI == 0
! - menu printing -
   If (imenus == 0) Then
      If (output_parameter(iout, cdesc=Trim(mn_opts_menus%l_lab%c), i=mn_opts_menus%id) /= 0) GoTo 1
   End If
! - error handling -
   If (mn_opts_errors%iact > 0) Then
      If (output_parameter(iout, cdesc=Trim(mn_opts_errors%l_lab%c), i=mn_opts_errors%id) /= 0) GoTo 1
      If (output_parameter(iout, cdesc=Trim(ls_uerropts(iprompt+1)%c), i=iprompt) /= 0) GoTo 1
   End If
#endif
! - analysis - 
   If (ianal == ia_non) Return
   If (output_parameter(iout, cdesc=Trim(ls_analyses(ianal)%c), i=ia_nul+ianal) /= 0) GoTo 1
! - input files -
   Select Case (ianal)
    Case (ia_cca)
      lxmodes = (max_eof_modes(lcw, nt, nx) > 1 .and. .not.lcde) 
      lymodes = (max_eof_modes(lcw, nt, ny) > 1 .and. .not.lcde) 
      lcmodes = (max_cca_modes() > 1 .and. .not.lcde) 
      Call parameter_write_iofiles (iout, xfield(:)%region%alim, yfield(:)%region%alim, yfield(1)%tssn%tprd(1)%sdate%imn, &
           lsn(1), ispi, ifail, &
           lxmodes=lxmodes, nxe=nxe, mxe=mxe, lymodes=lymodes, nye=nye, mye=mye, lcmodes=lcmodes, ncc=ncc, mcc=mcc)
    Case (ia_pcr)
      lxmodes = (max_eof_modes(lcw, nt, nx) > 1 .and. .not.lcde) 
      Call parameter_write_iofiles (iout, xfield(:)%region%alim, yfield(:)%region%alim, yfield(1)%tssn%tprd(1)%sdate%imn, &
           lsn(1), ispi, ifail, &
           lxmodes=lxmodes, nxe=nxe, mxe=mxe)
    Case Default
      Call parameter_write_iofiles (iout, xfield(:)%region%alim, yfield(:)%region%alim, yfield(1)%tssn%tprd(1)%sdate%imn, &
           lsn(1), ispi, ifail)
   End Select
   If (ifail /= 0) GoTo 1
! - Edit ~ Domain and Target Season (redundant because they are set when files are opened) -
! - Edit ~ Start at (handled in parameter_write_iofiles) -
! - Length of training period and Options -
   Call parameter_write_settings (iout, ifail)
   If (ifail /= 0) GoTo 1
! - Options ~ Climatological Period -
   Call parameter_write_climate (iout, icsv, ifail)
   If (ifail /= 0) GoTo 1
! - Options ~ categoies -
   Call parameter_write_categories (iout, ifail)
   If (ifail /= 0) GoTo 1
! - Options ~ Data ~ Missing Values -
   Call parameter_write_missing (iout, ianal, ifail)
   If (ifail /= 0) GoTo 1
! - Options ~ GCM Options or Advanced CCA, PCR Options (Regression are handled in settings) -
   Call parameter_write_pcs (iout, ifail)
   If (ifail /= 0) GoTo 1
! - Options ~ Forecast Settings -
   Call parameter_write_predict (iout, ifail)
   If (ifail /= 0) GoTo 1
! - Options ~ Resampling Settings -
   Call parameter_write_boot (iout, ifail)
   If (ifail /= 0) GoTo 1
! - Actions ~ Calculate -
   If (icalc > 0) Then
      Call parameter_write_action (iout, ifail)
      If (ifail /= 0) GoTo 1
      Call parameter_write_forecast (iout, ifail)
      If (ifail /= 0) GoTo 1
   End If
!
! Close parameters file
   Close (Unit=iout)
!
! Confirm
   psave = 5
   Call cpt_error ('write_parameter', .false., psave, &
        c_arg1=Trim(this_proj%pname)//pfilts(1)(2:), c_arg2=Trim(this_proj%pdir))
   p_dir = this_proj%pdir
   isave = 0
   Call set_menu_greying (isave=isave)
!
   psave = 2
   Return
!
! Error saving project file
1  Close (Unit=iout)
   ifail = 4
   Call cpt_error ('write_parameter', .false., ifail)
   psave = 2
!
   Return
 End Function write_parameter
!
!
!
 Function new_project()
!
! Clears existing project settings
!
! Modules
   Use menus,        Only: isave, &
                           set_menu_greying
   Use iofiles,      Only: files_open
   Use analysis,     Only: ianal, icalc
   Use menu_actions, Only: reset
!
! Function result
   Integer :: new_project
!
! Locals
!
! Local scalars
   Integer :: icalc_bk ! - backup calculation flag -
!
! Executable Statements
!
! Check for completed calculations
   new_project = 2
   If (ianal > 0) Then
      If (isave >= 2) Then
         isave = isave + 4
         Call set_menu_greying (isave=isave)
      End If
   End If
!
! Check for changes
   icalc_bk = icalc
   new_project = close_project()
   If (new_project == 1) Return
!
! Free memory
   icalc = icalc_bk
   new_project = reset(' ', .false.)
!
! Reset all fields to defaults
   Call init_sets ()
!
! All files are closed
   If (files_open(0, 0)) Continue
!
! Reset project settings
   new_project = clear_project()
!
   Return
 End Function new_project
!
!
!
 Function clear_project()
!
! Removes project name from window
!
! Modules
#if GUI == 1
   Use clrwin$,  Only: clear_window$
   Use gui,      Only: ih_con
#endif
   Use gui,      Only: set_win_title
   Use analysis, Only: ianal
!
! Function result
   Integer :: clear_project
!
! Executable Statements
!
! Reset project settings
   clear_project = 2
   Call reset_project (this_proj)
!
! Update CPT window title
   If (ianal > 0) Then
      Call set_win_title (ianal)
#if GUI == 1
      Call clear_window$ (ih_con)
#endif
      Print *, ' '
   End If
!
   clear_project = 2
!
   Return
 End Function clear_project
!
!
!
 Subroutine reset_project (proj_file)
!
! Resets project name
!
! Modules
   Use menus, Only: isave, &
                    set_menu_greying
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(proj), Intent(InOut) :: proj_file ! - project file -
!
! Executable Statements
!
! Reset project file
   proj_file%pdir = p_dir
   proj_file%pfile = ' '
   proj_file%pname = ' '
   proj_file%lproj = .false.
   isave = 0
   Call set_menu_greying (isave=isave)
!
   Return
 End Subroutine reset_project
!
!
!
 Function close_project() &
          Result (fclose)
!
! Closes a project
!
! Modules
#if GUI == 1
   Use clrwin$,        Only: clear_window$
#endif
   Use data_numbers,   Only: zero
   Use time,           Only: iseq
#if GUI == 1
   Use gui,            Only: ih_con, &
                             prompt_yesno
#endif
   Use gui,            Only: iw
   Use menus,          Only: isave
   Use progress_meter, Only: set_progress_meter
   Use iofiles,        Only: xfile, yfile
   Use analysis,       Only: icalc, &
                             analysis_flags
   Use menu_actions,   Only: reset_mem
!
! Function result
   Integer :: fclose
!
! Executable Statements
!
! Confirm loss of changes
   If (this_proj%lproj) Then
      If (isave /= 0) Then
#if GUI == 1
         Select Case (isave)
          Case (1)
            iw = prompt_yesno(.true., .false., .true., &
                 'Project File settings have been changed. Do you want to save the changes?')
          Case (2)
            iw = prompt_yesno(.true., .false., .true., &
                 'Do you want to save the results in the Project File?')
          Case (3)
            iw = prompt_yesno(.true., .false., .true., &
                 'Project File settings have been changed. Do you want to save the changes and the results?')
          Case (4)
            iw = prompt_yesno(.true., .false., .true., &
                 'Opening another project will clear current results.', &
                 msg2='Do you want to save these results in the current Project File?')
          Case (5)
            iw = prompt_yesno(.true., .false., .true., &
                 'Opening another project will clear current results.', &
                 msg2='Do you want to save these results and the changes to the settings in the current Project File?')
          Case (6)
            iw = prompt_yesno(.true., .false., .true., &
                 'Starting a new project will clear current results.', &
                 msg2='Do you want to save the results in the current Project File?')
          Case (7)
            iw = prompt_yesno(.true., .false., .true., &
                 'Starting a new project will clear current results.', &
                 msg2='Do you want to save the results and the changes to the settings in the current Project File?')
          Case (-1)
            iw = prompt_yesno(.true., .false., .true., &
                 'Project File is old. Do you want to update the file?')
          Case Default
            iw = 2
         End Select
#else
         iw = 2
#endif
         Select Case (iw)
          Case (1) ! - yes -
            fclose = write_project()
            fclose = 0
          Case (0) ! - no -
            isave = 0
            fclose = 0
          Case Default
            If (icalc == 0) Then
               Call set_progress_meter (zero)
#if GUI == 1
               Call clear_window$ (ih_con)
#endif
               Print *, ' '
            Else
               Select Case (isave)
                Case (4, 5)
                  isave = isave - 2
                Case (6, 7)
                  isave = isave - 4
               End Select
            End If
            fclose = 1
         End Select
         If (fclose == 0) Then
            this_proj%lproj = .false.
            If (.not.(xfile%cfile%lset .and. yfile%cfile%lset)) iseq = 0
            If (icalc == 1) Then
               fclose = reset_mem()
               Call analysis_flags ('off')
               fclose = clear_project()
               fclose = 0
            End If
         End If
      Else
         fclose = 0
      End If
   Else
      fclose = 0
   End If
!
   Return
 End Function close_project
End Module projects
