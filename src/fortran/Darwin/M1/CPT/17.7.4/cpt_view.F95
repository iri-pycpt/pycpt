! Author: Simon Mason
Module cpt_view
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: change_view, grey_menu_flags, set_grey_view, set_menuflags
!
Contains
!
!
 Function change_view()
!
! Changes analysis option
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_dia, ia_for, ia_pcr, ia_mlr, ia_gcm, ia_pfv, il_idt, ir_ols, iv_ple, mnt
   Use labels,             Only: cls_l
   Use screen,             Only: window_update
   Use gui,                Only: cwtitle, &
                                 set_win_title
   Use settings,           Only: icv2, iensc, iensc_bk, igauss, igauss_bk, ilink, ilink_bk, iregr, iregr_bk, iregr, iretro, istd, &
                                 istd_bk, isynch, mnu
   Use iofiles,            Only: xfile, yfile, zfile
   Use pcs,                Only: npx, npy
   Use season,             Only: icsv
   Use categories,         Only: ithr, ithr_bk
   Use climate,            Only: ieclim, ieclim_bk
   Use analysis,           Only: ianal, ianaln, icalc
!
! Function result
   Integer :: change_view
!
! Locals
!
! Local scalars
   Integer :: ltitle ! - length of window title -
   Integer :: ititle ! - start of project title -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index
!
! Executable Statements
!
! Convert from beginner mode
   change_view = 2
   Select Case (ianaln)
    Case (ia_dia, ia_for)
      If (ianal == ia_pfv) Then
         ianaln = ia_cca
      Else
         ianaln = ianal
      End If
      If (ianal == ia_dia) Then
         isynch = 1
      Else
         isynch = 0
      End If
   End Select
!
! Return if no change in analysis
   If (ianaln == ianal) Return
!
! Update CPT window
   Select Case (ianaln)
    Case (ia_cca, ia_pcr) ! - CCA and PCR -
      If (ianal == ia_pfv) Then
         igauss = igauss_bk
         ieclim = ieclim_bk
         istd = istd_bk
         ithr = ithr_bk
      End If
      If (ianaln == ia_cca) Then
         iregr_bk = iregr
         iregr = ir_ols
         ilink_bk = ilink
         ilink = il_idt
      End If
      If (ianal == ia_gcm) iensc_bk = iensc
      iensc = 1
      mnu = mnt
    Case (ia_mlr) ! - MLR -
      If (ianal == ia_pfv) Then
         igauss = igauss_bk
         ieclim = ieclim_bk
         istd = istd_bk
         ithr = ithr_bk
      End If
      If (ianal == ia_gcm) iensc_bk = iensc
      iensc = 1
      mnu = mnt
    Case (ia_gcm) ! - GCM -
      npx = 0
      npy = 0
      If (ianal == ia_pfv) Then
         ieclim = ieclim_bk
         istd = istd_bk
         ithr = ithr_bk
      End If
      iensc = iensc_bk
      mnu = mnt
    Case (ia_pfv) ! - PFV -
      igauss_bk = igauss
      igauss = 0
      npx = 0
      npy = 0
      ieclim_bk = ieclim
      ieclim = 1
      If (ianal == ia_gcm) iensc_bk = iensc
      iensc = 1
      istd_bk = istd
      istd = 0
      ithr_bk = ithr
      ithr = iv_ple
      mnu = 1
   End Select
   ianal = ianaln
   Call set_menuflags (ianal, icalc, icsv, iregr, istd, isynch, &
                       icv2=icv2, ixdstr=xfile%idstr, iydstr=yfile%idstr, izdstr=zfile%idstr, iretro=iretro)
   Call grey_menu_flags (icalc, xfile%idstr, yfile%idstr)
!
! Update window title
   ltitle = Index(cwtitle, ' (', Back=.true.) - 1
   If (ltitle > 0) Then
      ititle = Index(cwtitle, ' - ', Back=.false.) + 3
      Call set_win_title (ianal, &
           cproj=cwtitle(ititle:ltitle))
   Else
      Call set_win_title (ianal)
   End If
!
! Update lags/seasons label
   Call window_update (cls_l)
!
   Return
 End Function change_view
!
!
!
 Subroutine set_menuflags (ianal, icalc, icsv, iregr, istd, isynch, &
            lscree, icv2, ixdstr, iydstr, izdstr, iretro)
!
! Sets which menu items to display or grey
! (called after a change in View, file status, and at initialization)
!
! Modules
   Use data_cpt_constants, Only: ia_non, ia_pfv
   Use labels,             Only: cg_lags_l, cg_seasons_l, &
                                 cls_l
   Use menus,              Only: set_menu_greying
   Use space,              Only: igeog, &
                                 get_geog
   Use iofiles,            Only: set_grey_files
   Use pcs,                Only: set_grey_modes
   Use analysis,           Only: ihind
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal  ! - analysis flag -
   Integer, Intent(In) :: icalc  ! - calculation completed flag -
   Integer, Intent(In) :: icsv   ! - calculate seasonal values option -
   Integer, Intent(In) :: iregr  ! - regression option -
   Integer, Intent(In) :: istd   ! - standardization option -
   Integer, Intent(In) :: isynch ! - allow synchronous predictots flag -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: icv2   ! - retroactive analysis flag -
   Integer, Intent(In), Optional :: ixdstr ! - X file structure -
   Integer, Intent(In), Optional :: iydstr ! - Y file structure -
   Integer, Intent(In), Optional :: izdstr ! - Z file structure -
!
   Logical, Intent(In), Optional :: lscree ! - determine scree plot settings? -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: iretro ! - retroactive analysis flag -
!
! Locals
!
! Local scalars
   Logical :: lscr ! - determine scree plot settings? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Set grey View menu items
   If (Present(ixdstr) .and. Present(iydstr)) Then
      Call set_grey_view (ianal, isynch, ixdstr)
      Select Case (ianal)
       Case Default
         ihind = 1
       Case (ia_pfv, ia_non)
         ihind = 0
      End Select
!
! Set grey Modes menu items
     If (Present(lscree)) Then
        lscr = lscree
     Else
        lscr = .false.
     End If
     Call set_grey_modes (ianal, ixdstr, lscrees=lscr)
!
! Set grey menu items
      Select Case (ianal)
       Case Default ! - CCA, PCR, MLR, GCM -
         cls_l = Trim(cg_lags_l)
       Case (ia_pfv) ! - PFV -
         cls_l = Trim(cg_seasons_l)
         If (Present(iretro)) iretro = 1
      End Select
      Call set_menu_greying (ianal=ianal, icalc=icalc, icsv=icsv, icv2=icv2, ihind=ihind, iretro=iretro, iregr=iregr, istd=istd)
      Call set_grey_files (ixdstr, iydstr, igeog)
!
! Set options flags
      Call set_menu_greying (idstrx=ixdstr*(1 - icalc), idstry=iydstr*(1 - icalc), idstrz=izdstr)
      Call set_menu_greying (igeogx=get_geog(ixdstr), igeogy=get_geog(iydstr))
   Else
      If (ianal == ia_non) Call set_grey_view (ianal, isynch, ixdstr)
      Call set_menu_greying ()
      Call set_menu_greying ()
   End If
!
   Return
 End Subroutine set_menuflags
!
!
!
 Subroutine set_grey_view (ianal, isynch, ixdstr)
!
! Sets which View menu items to display or grey
!
! Modules
   Use data_cpt_constants, Only: ia_non, ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, id_non, id_ind
   Use menus,              Only: set_menu_greying
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal  ! - analysis flag -
   Integer, Intent(In) :: isynch ! - allow synchronous predictors flag -
   Integer, Intent(In) :: ixdstr ! - X file structure -
!
! Locals
!
! Local scalars
   Integer :: icca ! - CCA accessibility -
   Integer :: ipcr ! - PCR accessibility -
   Integer :: imlr ! - PCR accessibility -
   Integer :: igcm ! - GCM accessibility -
   Integer :: ipfv ! - PFV accessibility -
   Integer :: idia ! - diagnostics accessiblity -
   Integer :: ifor ! - forecast accessibility -
!
! Executable Statements
!
! Set grey View menu items
   icca = 1
   ipcr = 1
   imlr = 1
   igcm = 1
   ipfv = 1
   Select Case (ianal)
    Case (ia_cca) ! - CCA -
      icca = 0
    Case (ia_pcr) ! - PCR -
      ipcr = 0
    Case (ia_mlr) ! - MLR -
      imlr = 0
    Case (ia_gcm) ! - GCM -
      igcm = 0
    Case (ia_pfv) ! - PFV -
      ipfv = 0
   End Select
!
! Modify View flags depending on whether input files are open
   If (ianal /= ia_pfv) Then
      Select Case (ixdstr)
! - no X file -
       Case (id_non)
         ipfv = 1
! - index X file -
       Case (id_ind)
         ipfv = 0                      ! - disable PFV because a (non-probabilistic) X file is open -
! - station or gridded X file -
       Case Default
         If (ianal /= ia_mlr) imlr = 0 ! - disable MLR -
         ipfv = 0                      ! - disable PFV because a (non-probabilistic) X file is open -
      End Select
      If (ianal /= ia_non) Then
         idia = 1 - isynch
         ifor = isynch
      Else
         idia = 1
         ifor = 1
      End If
!
! Modify View flags depending on whether input files are open
! - PFV -
   Else
      Select Case (ixdstr)
! - X file is open and must be probabilistic because ianal == ia_pfv -
       Case Default ! - disable other options because only PFV can use probabilistic X files -
         icca = 0
         ipcr = 0
         imlr = 0
         igcm = 0
         idia = 0
         ifor = 0
! - no X file -
       Case (id_non)
         idia = 1
         ifor = 1
      End Select
   End If
!
! Grey menu items
   Call set_menu_greying (ianal=ianal)                                                                 ! - Actions -
   Call set_menu_greying (icca=icca, ipcr=ipcr, imlr=imlr, igcm=igcm, ipfv=ipfv, idia=idia, ifor=ifor) ! - View -
!
   Return
 End Subroutine set_grey_view
!
!
!
 Subroutine grey_menu_flags (icalc, ixdstr, iydstr)
!
! Sets menu items to grey as appropriate
! (called after a change in View or calculation status)
!
! Modules
   Use menus,    Only: set_menu_greying
   Use settings, Only: icv2, iregr, iretro, istd, izero
   Use space,    Only: igeog
   Use pcs,      Only: set_grey_modes
   Use season,   Only: icsv
   Use analysis, Only: ianal, ihind
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icalc  ! - calculation completed flag -
   Integer, Intent(In) :: ixdstr ! - X file structure -
   Integer, Intent(In) :: iydstr ! - X file structure -
!
! Executable Statements
!
! Set grey menu items
   Select Case (icalc)
    Case (1)
      Call set_menu_greying (ianal=ianal, icalc=icalc, icv2=icv2, iretro=iretro, iregr=iregr, istd=istd)
    Case (0)
      Call set_menu_greying (ianal=ianal, icalc=icalc, idstrx=ixdstr, iregr=iregr, istd=istd, idstry=iydstr, icsv=icsv)
      Call set_menu_greying (igeog=igeog, ihind=ihind)
      Call set_grey_modes (ianal, ixdstr, .false.)
   End Select
   Call set_menu_greying (izero=izero)
!
   Return
 End Subroutine grey_menu_flags
End Module cpt_view
