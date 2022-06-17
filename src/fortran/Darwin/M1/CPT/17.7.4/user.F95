! Author: Simon Mason
Module user
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: get_user, read_ini, save_ini
!
! Scalars
!
! Integer scalars
#if GUI == 1
   Integer(Kind=2), Public :: iwin ! - Windows version -
!
! Character scalars
   Character(Len=  128), Public :: userprofile ! - user home directory -
   Character(Len=  256), Public :: cpt_dir     ! - CPT directory -
#endif
   Character(Len=256+8), Public :: cpt_ini     ! - CPT initialization file -
!
   Character(Len=  256), Private :: user_dir   ! - user directory -
!
Contains
!
!
 Subroutine get_user (pdir)
!
! Identifies current user and sets directories
!
! Modules
#if GUI == 1
   Use data_numbers,      Only: cdigits
   Use data_text,         Only: ilang
#endif
   Use data_io_constants, Only: rdir
#if GUI == 1
   Use data_io_constants, Only: cdir, default_ini, iin
   Use errors,            Only: cpt_error
#endif
   Use iofiles,           Only: idir, odir
#if GUI == 1
   Use graphics,          Only: gdir
#endif
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: pdir ! - project file directory -
!
#if GUI == 1
! Locals
!
! Local scalars
   Integer(Kind=2) :: ifail2 ! - error indicator -
!
   Integer :: i1    ! - start of version number -
   Integer :: i2    ! - end of version number -
   Integer :: ifail ! - error indicator -
!
   Character(Len=256) :: bdir             ! - backup directory -
   Character(Len=128) :: win_ver          ! - windows version -
!
   Character(Len=  *), Parameter :: win_ver_file = & ! - temporary windows version file -
      'windows_version.tmp'
!
   Logical :: le   ! - cpt.ini file exists? -
   Logical :: lesp ! - Spanish Windows? -
   Logical :: lpor ! - Portuguese Windows? -
   Logical :: lver ! - Windows version file exists? -
!
#endif
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
#if GUI == 1
   Intrinsic execute_command_line, get_environment_variable, Index, Scan, Verify
!
! Silverfrost functions
   Character(Len=256), External :: curdir$
!
! Silverfrost routines
   External :: attach$, mkdir$
!
#endif
! Executable Statements
!
#if GUI == 1
! Identify current user and directory
   Call get_environment_variable ('USERPROFILE', userprofile)
   iwin = 0
   If (Index(userprofile, 'Users') /= 0) Then
      iwin = 6
   Else If(Index(userprofile, 'Documents and Settings') /= 0) Then
      iwin = 5
   End If
   If (iwin /= 0) Then
      lesp = .false.
      lpor = .false.
      bdir = curdir$()
      Select Case (iwin)
       Case (6)
         cpt_dir = Trim(userprofile)//cdir//'AppData'//cdir//'Roaming'//cdir
         user_dir = Trim(userprofile)//cdir//'Documents'//cdir
       Case (5)
         cpt_dir = Trim(userprofile)//cdir//'Application Data'//cdir
         user_dir = Trim(userprofile)//cdir//'My Documents'//cdir
      End Select
1     Call attach$ (Trim(cpt_dir), ifail2)
! - check for foreign versions of Windows -
      If (ifail2 /= 0) Then
         If (.not.lesp) Then ! - Spanish -
            cpt_dir = Trim(userprofile)//cdir//'Datos de Programa'//cdir
            ilang = 2
            lesp = .true.
         Else If (.not.lpor) Then ! - Portuguese -
            cpt_dir = Trim(userprofile)//cdir//'Dados de aplicativos'//cdir
            lpor = .true.
         Else
            Call mkdir$ (Trim(userprofile)//cdir//'Application Data'//cdir, ifail2)
            If (ifail2 /= 0 .and. ifail2 /= 183) GoTo 2
         End If
         GoTo 1
      End If
      Call attach$ (Trim(bdir), ifail2)
      cpt_dir = Trim(cpt_dir)//'CPT'//cdir
      cpt_ini = Trim(cpt_dir)//default_ini
! - check for new user -
      Inquire (File=cpt_ini, Exist=le)
      If (.not.le) Then
         Call read_ini (default_ini, pdir)
         Call mkdir$ (Trim(cpt_dir), ifail2)
         If (ifail2 /= 0 .and. ifail2 /= 183) GoTo 2
         pdir = Trim(cpt_dir)//'Projects'//cdir
         Call mkdir$ (Trim(pdir), ifail2)
         If (ifail2 /= 0 .and. ifail2 /= 183) GoTo 2
         idir = Trim(user_dir)
         odir = Trim(user_dir)
         gdir = Trim(user_dir)
         ifail2 = 0
         Call save_ini (Trim(cpt_ini), pdir, ifail)
         If (ifail /= 0) Then
            ifail = 1
            GoTo 3
         End If
      End If
! - set default for single-user version -
   Else
      cpt_ini = Trim(rdir)//default_ini
   End If
!
! Detect Windows version
   Call execute_command_line ('ver > '//Trim(user_dir)//win_ver_file)
   Inquire (File=Trim(user_dir)//win_ver_file, exist=lver)
   If (ifail2 == 0 .and. lver) Then
      Open (Unit=iin, File=Trim(user_dir)//win_ver_file, Action='read', Form='formatted', Status='old')
      Read (Unit=iin, Fmt=*)
      Read (Unit=iin, Fmt='(A)') win_ver
      Close (Unit=iin, Status='Delete')
      i1 = Scan(win_ver, cdigits)
      i2 = Verify(win_ver(i1:), cdigits) + i1 - 2
      Read (Unit=win_ver(i1:i2), Fmt=*) iwin
   End If
   Return
!
! Errors
2  If (ifail2 /= 0) ifail = 1
3  If (ifail /= 0) Then
      Call cpt_error ('get_user', .false., ifail, &
           c_arg1=cpt_dir)
      Stop
   End If
   cpt_ini = default_ini
#else
!
! Identify initialization file
   user_dir = rdir
   idir = Trim(user_dir)
   odir = Trim(user_dir)
   cpt_ini = Trim(rdir)//'cpt.ini'
#endif
!
   Return
 End Subroutine get_user
!
!
!
 Subroutine save_ini (cpt_ini, pdir, ifail)
!
! Saves CPT initialization file
!
! Modules
   Use data_numbers,      Only: one
   Use data_text,         Only: ilang
   Use data_version,      Only: ver
   Use data_io_constants, Only: iout
   Use screen,            Only: iwdthd
#if GUI == 1
   Use screen,            Only: dts_adj
   Use gui,               Only: icon_h, icon_v
#endif
   Use gui,               Only: mcw
   Use settings,          Only: clf, iensc, igood, igto, intp, iprecf, lcw, lexpert, nenf
   Use space,             Only: xarea, yarea
   Use iofiles,           Only: idir, ipreco, odir
   Use pcs,               Only: nxe, nye, ncc, mxe, mye,mcc
   Use missing,           Only: immx, immy, ipmx, ipmy, ipvx, ipvy, nnsx, xmiss, ymiss
   Use categories,        Only: ithr, thr, pthr
   Use climate,           Only: idclimyr1, idclimyr2
#if GUI == 1
   Use graphics,          Only: gdir, gszs
#endif
   Use bootstrap,         Only: nboot, clb, nperm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cpt_ini ! - CPT initialization file -
   Character(Len=*), Intent(In) :: pdir    ! - project file directory -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Save default settings
   Open (Unit=iout, File=cpt_ini, Access='sequential', Action='write', Form='formatted', IOstat=ifail, Status='unknown')
   If (ifail /= 0) Return
!
! Save default settings
   Write (Unit=iout, Fmt='(A,F8.2)')    'Version                                 ', ver
   Write (Unit=iout, Fmt='(2A)')        'Project directory                       ', Trim(pdir)
   Write (Unit=iout, Fmt='(2A)')        'Input directory                         ', Trim(idir)
   Write (Unit=iout, Fmt='(2A)')        'Output directory                        ', Trim(odir)
#if GUI == 1
   Write (Unit=iout, Fmt='(2A)')        'Graphics output directory               ', Trim(gdir)
#else
   Write (Unit=iout, Fmt='(2A)')        'Graphics output directory               ', Trim(odir)
#endif
   Write (Unit=iout, Fmt='(A,I8)')      'Length of cross-validation window       ', lcw
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum number of X modes               ', mxe
   Write (Unit=iout, Fmt='(A,I8)')      'Minimum number of X modes               ', nxe
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum number of Y modes               ', mye
   Write (Unit=iout, Fmt='(A,I8)')      'Minimum number of Y modes               ', nye
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum number of CCA modes             ', mcc
   Write (Unit=iout, Fmt='(A,I8)')      'Minimum number of CCA modes             ', ncc
   Write (Unit=iout, Fmt='(A,I8)')      'Method for replacing missing X data     ', immx
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum % of missing X data             ', ipmx
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum % of missing X variables        ', ipvx
   Write (Unit=iout, Fmt='(A,G24.12)')  'Missing X data flag                     ', xmiss
   Write (Unit=iout, Fmt='(A,I8)')      'Method for replacing missing Y data     ', immy
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum % of missing Y data             ', ipmy
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum % of missing Y variables        ', ipvy
   Write (Unit=iout, Fmt='(A,G24.12)')  'Missing Y data flag                     ', ymiss
   Write (Unit=iout, Fmt='(A,G24.12)')  'Northern X domain limit                 ', xarea%rltn
   Write (Unit=iout, Fmt='(A,G24.12)')  'Southern X domain limit                 ', xarea%rlts
   Write (Unit=iout, Fmt='(A,G24.12)')  'Western X domain limit                  ', xarea%rlgw
   Write (Unit=iout, Fmt='(A,G24.12)')  'Eastern X domain limit                  ', xarea%rlge
   Write (Unit=iout, Fmt='(A,G24.12)')  'Northern Y domain limit                 ', yarea%rltn
   Write (Unit=iout, Fmt='(A,G24.12)')  'Southern Y domain limit                 ', yarea%rlts
   Write (Unit=iout, Fmt='(A,G24.12)')  'Western Y domain limit                  ', yarea%rlgw
   Write (Unit=iout, Fmt='(A,G24.12)')  'Eastern Y domain limit                  ', yarea%rlge
   Write (Unit=iout, Fmt='(A,I8)')      'Size of bootstrap sample                ', nboot
   Write (Unit=iout, Fmt='(A,G24.12)')  'Bootstrap confidence level              ', clb
   Write (Unit=iout, Fmt='(A,I8)')      'Size of permutation sample              ', nperm
   Write (Unit=iout, Fmt='(A,G24.12)')  'Forecast confidence level               ', clf
   Write (Unit=iout, Fmt='(A,I8)')      'Threshold type                          ', ithr
   Write (Unit=iout, Fmt='(A,G24.12)')  'Probability of below                    ', pthr(1)
   Write (Unit=iout, Fmt='(A,G24.12)')  'Probability of above                    ', one - pthr(2)
   Write (Unit=iout, Fmt='(A,G24.12)')  'Absolute threshold (lower)              ', thr(1)
   Write (Unit=iout, Fmt='(A,G24.12)')  'Absolute threshold (upper)              ', thr(2)
   Write (Unit=iout, Fmt='(A,I8)')      'Forecast error variance flag            ', 1
   Write (Unit=iout, Fmt='(A,I8)')      'Forecast standardization flag           ', 0
   Write (Unit=iout, Fmt='(A,I8)')      'Precision of forecast                   ', iprecf
   Write (Unit=iout, Fmt='(A,I8)')      'Number of ensemble members              ', nenf
   Write (Unit=iout, Fmt='(A,I8)')      'Maximum number of results windows       ', mcw
   Write (Unit=iout, Fmt='(A,I8)')      'Language                                ', ilang
   Write (Unit=iout, Fmt='(A,I8)')      'Interpolation option                    ', intp
   Write (Unit=iout, Fmt='(A,I8)')      'Goodness index flag                     ', igood
   Write (Unit=iout, Fmt='(A,I8)')      'Model combination options               ', 1
   Write (Unit=iout, Fmt='(A,I8)')      'Number of near-neighbours               ', nnsx
   Write (Unit=iout, Fmt='(A,I8)')      'Default screen width for text output    ', iwdthd
   Write (Unit=iout, Fmt='(A,I8)')      'Ensemble forecasting option             ', iensc
   Write (Unit=iout, Fmt='(A,I8)')      'Gaussian transformation option          ', igto
   If (lexpert) Then
      Write (Unit=iout, Fmt='(A,I8)')      'Expertise level                         ', 1
   Else
      Write (Unit=iout, Fmt='(A,I8)')      'Expertise level                         ', 0
   End If
#if GUI == 1
   Write (Unit=iout, Fmt='(A,G24.12)')  'Graphics size scaling                   ', gszs
   Write (Unit=iout, Fmt='(A,G24.12)')  'Font size scaling adjustment            ', dts_adj
   Write (Unit=iout, Fmt='(A,I8)')      'Console window width adjustment         ', icon_h
   Write (Unit=iout, Fmt='(A,I8)')      'Console window height adjustment        ', icon_v
#else
   Write (Unit=iout, Fmt='(A,G24.12)')  'Graphics size scaling                   ', one
   Write (Unit=iout, Fmt='(A,G24.12)')  'Font size scaling adjustment            ', one
   Write (Unit=iout, Fmt='(A,I8)')      'Console window width adjustment         ', 0
   Write (Unit=iout, Fmt='(A,I8)')      'Console window height adjustment        ', 0
#endif
   Write (Unit=iout, Fmt='(A,I8)')      'Precision of output                     ', ipreco
   Write (Unit=iout, Fmt='(A,I8)')      'Default climatological period start     ', idclimyr1
   Write (Unit=iout, Fmt='(A,I8)')      'Default climatological period end       ', idclimyr2
!
   Close (Unit=iout)
!
   Return
 End Subroutine save_ini
!
!
!
  Recursive Subroutine read_ini (cpt_ini, pdir)
!
! Reads CPT initialization file
!
! Modules
   Use data_numbers,       Only: sp, one
   Use data_text,          Only: ilang
   Use data_cpt_constants, Only: ia_pfv
   Use data_io_constants,  Only: cdir, default_ini, iin
#if GUI == 0
   Use data_io_constants,  Only: rdir
#else
   Use screen,             Only: dts_adj
#endif
   Use screen,             Only: iwdthd
   Use gui,                Only: mcw
#if GUI == 1
   Use gui,                Only: icon_h, icon_v
   Use errors,             Only: query
#endif
   Use errors,             Only: cpt_error
   Use settings,           Only: clf, iensc, iensc_bk, igood, igto, intp, iprecf, lcw, lexpert, nenf
   Use space,              Only: xarea, yarea
   Use iofiles,            Only: idir, ipreco, odir
   Use pcs,                Only: mcc, mxe, mye, ncc, nxe, nye
   Use missing,            Only: immx, immy, ipmx, ipmy, ipvx, ipvy, nnsx, nnsy, xmiss, ymiss
   Use categories,         Only: ithr, ithr_bk, pthr, thr
   Use climate,            Only: idclimyr1, idclimyr2, ieclim, ieclim_bk
   Use analysis,           Only: ianal
#if GUI == 1
   Use graphics,           Only: gdir, gszs
#endif
   Use bootstrap,          Only: clb, nboot, nperm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cpt_ini ! - CPT initialization file -
!
! - output scalars -
   Character(Len=*), Intent(Out) :: pdir ! - project file directory -
!
! Locals
!
! Local scalars
   Integer :: iexpert ! - expertise level -
   Integer :: ifail   ! - error indicator -
!
   Real(Kind=sp) :: ver_this ! - version number of .ini file -
!
   Character(Len=256) :: crdir ! - current directory -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
#if GUI == 1
! Silverfrost functions
   Character(Len=256), External :: curdir$
!
#endif
!
! Executable Statements
!
! Read customised defaults
   Open (Unit=iin, File=cpt_ini, Access='sequential', Action='read', Form='formatted', &
         Err=1, Status='old')
   Read (Unit=iin, Fmt='(40X,F8.2)', IOstat=ifail) ver_this
   If (ifail /= 0 .or. ver_this < 10.0_sp) Then ! - update old .ini files -
      Close (Unit=iin)
      If (Trim(cpt_ini) /= default_ini) Then
#if GUI == 1
         If (query('read_ini', 1, .false.) /= 1) Stop
#endif
         Call read_ini (default_ini, pdir)
         odir = Trim(user_dir)
         idir = Trim(user_dir)
#if GUI == 1
         pdir = Trim(cpt_dir)//'Projects'//cdir
         gdir = Trim(user_dir)
#else
         pdir = '.'//cdir
#endif
         Call save_ini (cpt_ini, pdir, ifail)
         Return
      Else
         GoTo 3
      End If
   End If
   Read (Unit=iin, Fmt='(40X,A)', Err=2, End=2) pdir
   Read (Unit=iin, Fmt='(40X,A)', Err=2, End=2) idir
   Read (Unit=iin, Fmt='(40X,A)', Err=2, End=2) odir
#if GUI == 1
   Read (Unit=iin, Fmt='(40X,A)', Err=2, End=2) gdir
#else
   Read (Unit=iin, Fmt=        *, Err=2, End=2)
#endif
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) lcw
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) mxe
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) nxe
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) mye
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) nye
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) mcc
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) ncc
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) immx
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) ipmx
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) ipvx
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) xmiss
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) immy
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) ipmy
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) ipvy
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) ymiss
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) xarea%rltn
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) xarea%rlts
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) xarea%rlgw
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) xarea%rlge
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) yarea%rltn
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) yarea%rlts
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) yarea%rlgw
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) yarea%rlge
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) nboot
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) clb
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) nperm
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) clf
   Read (Unit=iin, Fmt='(40X,I8)', Err=2, End=2) ithr
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) pthr(1)
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) pthr(2)
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) thr(1)
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=2, End=2) thr(2)
   Read (Unit=iin, Fmt='(40X,I8)', Err=101)
   Read (Unit=iin, Fmt='(40X,I8)', Err=101)
   Read (Unit=iin, Fmt='(40X,I8)', Err=101) iprecf
   Read (Unit=iin, Fmt='(40X,I8)', Err=102) nenf
   Read (Unit=iin, Fmt='(40X,I8)', Err=103) mcw
   Read (Unit=iin, Fmt='(40X,I8)', Err=104) ilang
   Read (Unit=iin, Fmt='(40X,I8)', Err=105) intp
   Read (Unit=iin, Fmt='(40X,I8)', Err=106) igood
   Read (Unit=iin, Fmt='(40X,I8)', Err=101)
   Read (Unit=iin, Fmt='(40X,I8)', Err=107) nnsx
   Read (Unit=iin, Fmt='(40X,I8)', Err=108) iwdthd
   Read (Unit=iin, Fmt='(40X,I8)', Err=109) iensc
   Read (Unit=iin, Fmt='(40X,I8)', Err=110) igto
   Read (Unit=iin, Fmt='(40X,I8)', Err=111) iexpert
#if GUI == 1
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=112) gszs
   Read (Unit=iin, Fmt='(40X,G24.12)', Err=113) dts_adj
   Read (Unit=iin, Fmt='(40X,I8)', Err=114) icon_h
   Read (Unit=iin, Fmt='(40X,I8)', Err=115) icon_v
#else
   Read (Unit=iin, Fmt=        *, Err=2, End=2)
   Read (Unit=iin, Fmt=        *, Err=2, End=2)
   Read (Unit=iin, Fmt=        *, Err=2, End=2)
   Read (Unit=iin, Fmt=        *, Err=2, End=2)
#endif
   Read (Unit=iin, Fmt='(40X,I8)', Err=116) ipreco
   Read (Unit=iin, Fmt='(40X,I8)', Err=117) idclimyr1
   Read (Unit=iin, Fmt='(40X,I8)', Err=118) idclimyr2
   GoTo 100
!
! Defaults
101 iprecf = 3
102 nenf = 9
103 mcw = 100
104 ilang = 1
105 intp = 0
106 igood = 3
107 nnsx = 1
108 iwdthd = 80
109 iensc = 1
110 igto = 1
111 iexpert = 1
#if GUI == 1
112 gszs = one
113 dts_adj = one
114 icon_h = 0
115 icon_v = 0
#endif
116 ipreco = -1
117 idclimyr1 = 1991
118 idclimyr2 = 2020
   ifail = -1
!
! Close file
100 Close (Unit=iin)
!
! Set climatological extension option
   If (ianal /= ia_pfv) Then
      ieclim = 0
   Else
      ieclim = 1
   End If
   ieclim_bk = 0
!
! Set backups
   ithr_bk = ithr   ! - threshold option -
   iensc_bk = iensc ! - ensemble option -
!
! Set number of Y near-neighbours
   nnsy = nnsx
!
! Apply threshold settings
   pthr(2) = one - pthr(2)
!
! Set expertise level
   lexpert = (iexpert /= 0)
!
! Save if out of date
   If (ifail == -1) Call save_ini (cpt_ini, pdir, ifail)
   Return
!
! Errors
1  ifail = 1
#if GUI == 1
   crdir = curdir$()
#else
   crdir = rdir
#endif
   Call cpt_error ('read_ini', .false., ifail, &
        c_arg1=Trim(cpt_ini), c_arg2=Trim(crdir))
   Stop
!
2  If (Trim(cpt_ini) == default_ini) Then
      ifail = 2
   Else
      ifail = 3
   End If
   Call cpt_error ('read_ini', .false., ifail, &
        c_arg1=Trim(cpt_ini))
   Stop
!
3  ifail = 4
   Call cpt_error ('read_ini', .false., ifail, &
        c_arg1=default_ini)
   Stop
!
 End Subroutine read_ini
End Module user
