! Author: Simon Mason
Module settings
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: change_igauss, change_setting, check_n, check_nt, force_odd_lcw, get_cv, get_maxn, get_retro, record_results, &
             reset_settings, set_it1, set_nused, set_options, set_zero, training_refresh
   Public :: parameter_write_settings, proj_read_settings, proj_write_settings
!
! Scalars
!
! Integer scalars
   Integer, Public :: hcw       ! - half cross-validation window -
   Integer, Public :: ibi       ! - number of probability bins identifier -
   Integer, Public :: ibs       ! - binned probabilities for verification scores? -
   Integer, Public :: icv2      ! - double cross-validation flag -
   Integer, Public :: idry      ! - dry-mask -
   Integer, Public :: ieav      ! - ensemble-average flag -
   Integer, Public :: iensc     ! - ensemble counting flag -
   Integer, Public :: iensc_bk  ! - backup ensemble counting flag -
   Integer, Public :: iev       ! - error-variance flag -
   Integer, Public :: iexc      ! - exceedance (0) or non-exceedance (1) probabilities -
   Integer, Public :: ifpr      ! - forecast probability rounding -
   Integer, Public :: igauss    ! - transform to gaussian flag -
   Integer, Public :: igauss_bk ! - backup transform to gaussian flag -
   Integer, Public :: igcms     ! - GCM recalibration option -
   Integer, Public :: igcms_bk  ! - backup GCM recalibration option -
   Integer, Public :: igood     ! - goodness index flag -
   Integer, Public :: igto      ! - gaussian transformation option -
   Integer, Public :: ig0       ! - lower index limit -
   Integer, Public :: ilink     ! - link-function identifier -
   Integer, Public :: ilink_bk  ! - backup link-function identifier -
   Integer, Public :: imc       ! - model combination option -
   Integer, Public :: intp      ! - interpolation option -
   Integer, Public :: iodds     ! - odds ratio flag -
   Integer, Public :: iprecf    ! - forecast precision -
   Integer, Public :: iregr     ! - regression flag -
   Integer, Public :: iregr_bk  ! - backup regression flag -
   Integer, Public :: iretro    ! - retroactive forecast flag -
   Integer, Public :: iskmask   ! - skill-mask -
   Integer, Public :: iskmaski  ! - skill-mask score -
   Integer, Public :: isem      ! - sort ensemble members flag -
   Integer, Public :: istd      ! - standardization option -
!                                   0 = no standardization
!                                   1 = anomalies
!                                   2 = standardized anomalies
!                                   3 = SPI
!                                   4 = % average
   Integer, Public :: istdr     ! - standardization option at Actions ~ Calculate -
   Integer, Public :: istd_bk   ! - backup standardization option -
   Integer, Public :: isynch    ! - allow synchronous predictors -
   Integer, Public :: itx       ! - current X training date -
   Integer, Public :: ity       ! - current X training date -
   Integer, Public :: iv        ! - current series -
   Integer, Public :: iva       ! - current available series -
   Integer, Public :: ivf       ! - current series by field -
   Integer, Public :: ivf_old   ! - previous series by field -
   Integer, Public :: ivfa      ! - current available series by field -
   Integer, Public :: izero     ! - zero-bound flag -
   Integer, Public :: lcw       ! - length of cross-validation window -
   Integer, Public :: lcw_old   ! - backup length of cross-validation window -
   Integer, Public :: m         ! - number of variables -
   Integer, Public :: mntu      ! - minimum length of training period to use -
   Integer, Public :: mnu       ! - minimum length of training period -
   Integer, Public :: nb        ! - number of probability bins -
   Integer, Public :: ncv       ! - length of cross-validated training period -
   Integer, Public :: nenf      ! - number of ensemble forecasts -
   Integer, Public :: nf        ! - number of forecasts -
   Integer, Public :: nfbk      ! - number of backup forecasts -
   Integer, Public :: nmf       ! - number of forecasts in monthly file -
   Integer, Public :: ntm       ! - number of cases in monthly file to read -
   Integer, Public :: nret      ! - number of retroactive iterations -
   Integer, Public :: nretro    ! - model update interval -
   Integer, Public :: nt        ! - number of training cases -
   Integer, Public :: nt_old    ! - previous number of training cases -
   Integer, Public :: ntr       ! - number of retroactive verification cases -
   Integer, Public :: nt1       ! - initial number of training cases -
   Integer, Public :: nu        ! - number of used training cases -
   Integer, Public :: nur       ! - number of used retroactive verification cases -
   Integer, Public :: nu1       ! - initial number of used training cases -
   Integer, Public :: nvrf      ! - verification sample size -
   Integer, Public :: nvrfu     ! - used verification sample size -
   Integer, Public :: nvrf1     ! - number of cases in initial training period -
   Integer, Public :: nyr       ! - number of years -
   Integer, Public :: xfd_old   ! - backup X first year of interest -
   Integer, Public :: yfd_old   ! - backup Y first year of interest -
!
! Real scalars
   Real(Kind=rp), Public :: clf     ! - forecast confidence level -
   Real(Kind=rp), Public :: dgood   ! - direction of goodness index flag -
   Real(Kind=rp), Public :: rdry    ! - dry-mask threshold -
   Real(Kind=rp), Public :: rskmask ! - dry-mask threshold -
!
! Logical scalars
   Logical, Public :: lexpert ! - expert user? -
   Logical, Public :: lzb     ! - apply zero bounds? -
!
Contains
!
!
 Subroutine reset_settings (ianal)
!
! Resets program settings
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, ic_non, il_idt, ir_ols, is_non, mnt, nbss
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis indicator
!
! Executable Statements
!
! Reset length of training period and number of forecasts
   nt = 0
   nt_old = 0
   nf = 1
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr, ia_gcm)
      mnu = mnt
    Case (ia_pfv)
      mnu = 1
    Case Default
      mnu = 1
   End Select
   itx = 1
   ity = 1
!
! Use OLS regression
   iregr = ir_ols
   iregr_bk = iregr
!
! Use identity link
   ilink = il_idt
   ilink_bk = ilink
!
! Double cross-validation settings
   icv2 = 0
!
! Retroactive forecast settings
   iretro = 0
   nretro = 1
   nt1 = 0
!
! Set transformation flag
   igauss = 0
   igauss_bk = 0
!
! Set zero-bound flag
   izero = 0
!
! Allow synchronous predictors flag
   isynch = 0
!
! Switch off masks
   idry = 0
   rdry = zero
   iskmaski = 1
   iskmask = 0
   rskmask = zero
!
! Set average ensemble members flag
   ieav = 0
!
! Set sort ensemble members flag
   isem = 1
!
! Set zero-bound flag
   izero = 0
!
! Initialise odds flag
   iodds = 0
!
! Initialise probability of exceedance
   iexc = 0
!
! Set model combination option
   imc = ic_non
!
! Set standardization option
   istd = is_non
   istdr = istd
   istd_bk = istd
!
! Set GCM standardization option
   igcms = istd
   igcms_bk = igcms
!
! Set error variance option
   iev = 1
!
! Set terification settings
   ibi = 1
   nb = nbss(ibi)
   ibs = 0
!
! Forecast probablity rounding
   ifpr = 1
!
! Backups
   xfd_old = 0
   yfd_old = 0
!
   Return
 End Subroutine reset_settings
!
!
!
 Function record_results()
!
! Records calculation of results to be saved in project file
!
! Modules
   Use menus, Only: isave, &
                    set_menu_greying
!
! Function result
   Integer :: record_results
!
! Executable Statements
!
! Record calculation of results
   isave = isave + 2
   Call set_menu_greying (isave=isave)
   record_results = 2
!
   Return
 End Function record_results
!
!
!
 Subroutine training_refresh (ncy)
!
! Refreshes training period settings
!
! Modules
   Use screen, Only: window_update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ncy ! - length of climatological period (in years) -
!
! Executable Statements
!
! Refresh file information
   Call window_update (nt)
   Call window_update (ncy)
   Call window_update (nf)
!
   Return
 End Subroutine training_refresh
!
!
!
 Function change_setting(i, ip, mn, mx) &
          Result (chnge)
!
! Change a setting
!
! Modules
   Use maths,  Only: force_limits
   Use screen, Only: window_update
!
! Function result
   Integer :: chnge
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: mn ! - minimum value -
   Integer, Intent(In) :: mx ! - maximum value -
!
! - input/output scalars -
   Integer, Intent(InOut) :: i  ! - value -
   Integer, Intent(InOut) :: ip ! - previous value -
!
! Locals
!
! Local scalars
   Integer :: iadj ! - adjustment indicator -
!
! Executable Statements
!
! Check for double call
   chnge = 1
   If (i == ip) Return
!
! Loop if necessary
   Call force_limits (mn, mx, i, iadj)
   If (iadj /= 0) Call window_update (i)
   ip = i
   chnge = 2
!
   Return
 End Function change_setting
!
!
!
 Function change_igauss() &
          Result (chnge)
!
! Toggles Y transformation flag
!
! Modules
   Use data_cpt_constants, Only: it_non
   Use menus,              Only: record_change
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Record change
  chnge = record_change()
!
! Toggle Y-transformation flag
#if GUI == 1
   chnge = 2
#else
   chnge = 0
#endif
   Select Case (igauss)
    Case (it_non)
      igauss = igto
    Case Default
      igauss = 0
   End Select
   igauss_bk = igauss
!
   Return
 End Function change_igauss
!
!
!
 Function set_options( &
          iw, lrc, i1, i2, i3, i4, i5, i6, i7, i8, r1, r2, r3, l1, d1, d2, o1, o2, o3, o4, oa1, oa2, oa3, oa4, ra1, ra2, &
          ldif_i1, ldif_i2, ldif_i3, ldif_i4, ldif_i5, ldif_i6, ldif_i7, ldif_i8, ldif_r1, ldif_r2, ldif_r3, ldif_l1, &
          ldif_d1, ldif_d2, &
          ldif_o1, ldif_o2, ldif_o3, ldif_o4, ldif_ra1, ldif_ra2)
!
! Sets options. If iw is not present, backups are stored. If iw is present then:
!                   if iw == 1 changes are confirmed;
!                   if iw /= 1 backups are restored. 
! set_options = 0 if backing up
!               1 if no change is recorded
!               2 if change is identified (and recorded if lrc/ = .false.)
!               3 any changes are cancelled
!
! Modules
   Use maths, Only: get_flag, set_flag
   Use time,  Only: pdate
!
! Function result
   Integer :: set_options
!
! Arguments
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: iw ! - windows response (iw == 1 if action is to be taken)  -
!
   Logical, Intent(In), Optional :: lrc ! - record change? (default = .true.) -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: i1 ! - first integer argument -
   Integer, Intent(InOut), Optional :: i2 ! - second integer argument -
   Integer, Intent(InOut), Optional :: i3 ! - third integer argument -
   Integer, Intent(InOut), Optional :: i4 ! - fourth integer argument -
   Integer, Intent(InOut), Optional :: i5 ! - fifth integer argument -
   Integer, Intent(InOut), Optional :: i6 ! - sixth integer argument -
   Integer, Intent(InOut), Optional :: i7 ! - seventh integer argument -
   Integer, Intent(InOut), Optional :: i8 ! - eighth integer argument -
   Integer, Intent(InOut), Optional :: o1 ! - first option index argument -
   Integer, Intent(InOut), Optional :: o2 ! - second option index argument -
   Integer, Intent(InOut), Optional :: o3 ! - third option index argument -
   Integer, Intent(InOut), Optional :: o4 ! - fourth option index argument -
!
   Real(Kind=rp), Intent(InOut), Optional :: r1 ! - first real argument -
   Real(Kind=rp), Intent(InOut), Optional :: r2 ! - second real argument -
   Real(Kind=rp), Intent(InOut), Optional :: r3 ! - third real argument -
!
   Logical, Intent(InOut), Optional :: l1 ! - first logical argument -
!
   Type(pdate), Intent(InOut), Optional :: d1 ! - first date argument -
   Type(pdate), Intent(InOut), Optional :: d2 ! - second date argument -
!
! - optional input/output arrays -
   Integer, Dimension(:), Intent(InOut), Optional :: oa1 ! - first option flags -
   Integer, Dimension(:), Intent(InOut), Optional :: oa2 ! - second option flags -
   Integer, Dimension(:), Intent(InOut), Optional :: oa3 ! - third option flags -
   Integer, Dimension(:), Intent(InOut), Optional :: oa4 ! - fourth option flags -
!
   Real(Kind=rp), Dimension(:), Intent(InOut), Optional :: ra1 ! - first real array -
   Real(Kind=rp), Dimension(:), Intent(InOut), Optional :: ra2 ! - second real array -
!
! - optional output scalars -
   Logical, Intent(InOut), Optional :: ldif_i1  ! - first integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i2  ! - second integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i3  ! - third integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i4  ! - fourth integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i5  ! - fifth integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i6  ! - sixth integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i7  ! - seventh integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_i8  ! - eighth integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_o1  ! - first option index argument changed? -
   Logical, Intent(InOut), Optional :: ldif_o2  ! - second option index argument changed? -
   Logical, Intent(InOut), Optional :: ldif_o3  ! - third option index argument changed? -
   Logical, Intent(InOut), Optional :: ldif_o4  ! - fourth option index argument changed? -
   Logical, Intent(InOut), Optional :: ldif_r1  ! - first real argument changed? -
   Logical, Intent(InOut), Optional :: ldif_r2  ! - second real argument changed? -
   Logical, Intent(InOut), Optional :: ldif_r3  ! - third real argument changed? -
   Logical, Intent(InOut), Optional :: ldif_l1  ! - first logical argument changed? -
   Logical, Intent(InOut), Optional :: ldif_d1  ! - first date argument changed? -
   Logical, Intent(InOut), Optional :: ldif_d2  ! - second date argument changed? -
   Logical, Intent(InOut), Optional :: ldif_ra1 ! - first real array changed? -
   Logical, Intent(InOut), Optional :: ldif_ra2 ! - second real array changed? -
!
! Locals
!
! Local scalars
   Integer, Save :: i1_bk ! - backup first integer argument -
   Integer, Save :: i2_bk ! - backup second integer argument -
   Integer, Save :: i3_bk ! - backup third integer argument -
   Integer, Save :: i4_bk ! - backup fourth integer argument -
   Integer, Save :: i5_bk ! - backup fifth integer argument -
   Integer, Save :: i6_bk ! - backup sixth integer argument -
   Integer, Save :: i7_bk ! - backup seventh integer argument -
   Integer, Save :: i8_bk ! - backup eighth integer argument -
   Integer, Save :: o1_bk ! - backup first option index argument -
   Integer, Save :: o2_bk ! - backup second option index argument -
   Integer, Save :: o3_bk ! - backup third option index argument -
   Integer, Save :: o4_bk ! - backup fourth option index argument -
!
   Real(Kind=rp), Save :: r1_bk ! - backup first real argument -
   Real(Kind=rp), Save :: r2_bk ! - backup second real argument -
   Real(Kind=rp), Save :: r3_bk ! - backup third real argument -
!
   Logical, Save :: l1_bk ! - backup first logical argument -
!
   Type(pdate), Save :: d1_bk ! - backup first date argument -    FTN95 bug
   Type(pdate), Save :: d2_bk ! - backup second date argument -    FTN95 bug
!
! Local arrays
   Real(Kind=rp), Dimension(:), Allocatable, Save :: ra1_bk ! - backup first real array -
   Real(Kind=rp), Dimension(:), Allocatable, Save :: ra2_bk ! - backup second real array -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated, Present, Size
!
! Executable Statements
!
! Back up arguments
   If (.not.Present(iw)) Then
      If (Present(i1)) i1_bk = i1
      If (Present(i2)) i2_bk = i2
      If (Present(i3)) i3_bk = i3
      If (Present(i4)) i4_bk = i4
      If (Present(i5)) i5_bk = i5
      If (Present(i6)) i6_bk = i6
      If (Present(i7)) i7_bk = i7
      If (Present(i8)) i8_bk = i8
      If (Present(r1)) r1_bk = r1
      If (Present(r2)) r2_bk = r2
      If (Present(r3)) r3_bk = r3
      If (Present(l1)) l1_bk = l1
      If (Present(d1)) d1_bk = d1
      If (Present(d2)) d2_bk = d2
      If (Present(o1) .and. Present (oa1)) Then
         o1_bk = o1
         Call set_flag (o1, oa1(:))
      End If
      If (Present(o2) .and. Present (oa2)) Then
         o2_bk = o2
         Call set_flag (o2, oa2(:))
      End If
      If (Present(o3) .and. Present (oa3)) Then
         o3_bk = o3
         Call set_flag (o3, oa3(:))
      End If
      If (Present(o4) .and. Present (oa4)) Then
         o4_bk = o4
         Call set_flag (o4, oa4(:))
      End If
      If (Present(ra1)) Then
         If (Allocated(ra1_bk)) Deallocate (ra1_bk)
         Allocate (ra1_bk(Size(ra1)))
         ra1_bk(:) = ra1(:)
      End If
      If (Present(ra2)) Then
         If (Allocated(ra2_bk)) Deallocate (ra2_bk)
         Allocate (ra2_bk(Size(ra2)))
         ra2_bk(:) = ra2(:)
      End If
      set_options = 0
!
! Check for changes
   Else
      If (iw == 1) Then
         set_options = 1
         If (Present(lrc)) Then
            If (.not.lrc) Then
               set_options = 2
               Return
            End If
         End If
         Call check_change (i=i1, i_bk=i1_bk, ldif_i=ldif_i1)
         Call check_change (i=i2, i_bk=i2_bk, ldif_i=ldif_i2)
         Call check_change (i=i3, i_bk=i3_bk, ldif_i=ldif_i3)
         Call check_change (i=i4, i_bk=i4_bk, ldif_i=ldif_i4)
         Call check_change (i=i5, i_bk=i5_bk, ldif_i=ldif_i5)
         Call check_change (i=i6, i_bk=i6_bk, ldif_i=ldif_i6)
         Call check_change (i=i7, i_bk=i7_bk, ldif_i=ldif_i7)
         Call check_change (i=i8, i_bk=i8_bk, ldif_i=ldif_i8)
         Call check_change (r=r1, r_bk=r1_bk, ldif_r=ldif_r1)
         Call check_change (r=r2, r_bk=r2_bk, ldif_r=ldif_r2)
         Call check_change (r=r3, r_bk=r3_bk, ldif_r=ldif_r3)
         Call check_change (l=l1, l_bk=l1_bk, ldif_l=ldif_l1)
         Call check_change (d=d1, d_bk=d1_bk, ldif_d=ldif_d1)
         Call check_change (d=d2, d_bk=d2_bk, ldif_d=ldif_d2)
         If (Present(o1) .and. Present(oa1)) o1 = get_flag(oa1(:))
         Call check_change (i=o1, i_bk=o1_bk, ldif_i=ldif_o1)
         If (Present(o2) .and. Present(oa2)) o2 = get_flag(oa2(:))
         Call check_change (i=o2, i_bk=o2_bk, ldif_i=ldif_o2)
         If (Present(o3) .and. Present(oa3)) o3 = get_flag(oa3(:))
         Call check_change (i=o3, i_bk=o3_bk, ldif_i=ldif_o3)
         If (Present(o4) .and. Present(oa4)) o4 = get_flag(oa4(:))
         Call check_change (i=o4, i_bk=o4_bk, ldif_i=ldif_o4)
         Call check_change (ra=ra1, ra_bk=ra1_bk, ldif_ra=ldif_ra1)
         Call check_change (ra=ra2, ra_bk=ra2_bk, ldif_ra=ldif_ra2)
      Else
         If (Present(i1)) i1 = i1_bk
         If (Present(i2)) i2 = i2_bk
         If (Present(i3)) i3 = i3_bk
         If (Present(i4)) i4 = i4_bk
         If (Present(i5)) i5 = i5_bk
         If (Present(i6)) i6 = i6_bk
         If (Present(i7)) i7 = i7_bk
         If (Present(i8)) i8 = i8_bk
         If (Present(r1)) r1 = r1_bk
         If (Present(r2)) r2 = r2_bk
         If (Present(r3)) r3 = r3_bk
         If (Present(l1)) l1 = l1_bk
         If (Present(d1)) d1 = d1_bk
         If (Present(d2)) d2 = d2_bk
         If (Present(o1)) o1 = o1_bk
         If (Present(o2)) o2 = o2_bk
         If (Present(o3)) o3 = o3_bk
         If (Present(o4)) o4 = o4_bk
         If (Present(ra1)) ra1(:) = ra1_bk(:)
         If (Present(ra2)) ra2(:) = ra2_bk(:)
         set_options = 3
      End If
   End If
!
   Return
!
 Contains
!
!
  Subroutine check_change ( &
             i, i_bk, ldif_i, &
             r, r_bk, ldif_r, &
             l, l_bk, ldif_l, &
             d, d_bk, ldif_d, &
             ra, ra_bk, ldif_ra)
!
! Checks whether argument has changed
!
! Modules
   Use time,  Only: Operator(/=)
   Use menus, Only: record_change
!
! Arguments
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: i    ! - integer argument -
   Integer, Intent(In), Optional :: i_bk ! - backup integer argument -
!
   Real(Kind=rp), Intent(In), Optional :: r    ! - real argument -
   Real(Kind=rp), Intent(In), Optional :: r_bk ! - backup real argument -
!
   Logical, Intent(In), Optional :: l    ! - logical argument -
   Logical, Intent(In), Optional :: l_bk ! - backup logical argument -
!
   Type(pdate), Intent(In), Optional :: d    ! - date argument -
   Type(pdate), Intent(In), Optional :: d_bk ! - backup date argument -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: ra    ! - real array -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: ra_bk ! - backup real array -
!
! - optional output scalars -
   Logical, Intent(InOut), Optional :: ldif_i  ! - integer argument changed? -
   Logical, Intent(InOut), Optional :: ldif_r  ! - real argument changed? -
   Logical, Intent(InOut), Optional :: ldif_l  ! - logical argument changed? -
   Logical, Intent(InOut), Optional :: ldif_d  ! - date argument changed? -
   Logical, Intent(InOut), Optional :: ldif_ra ! - real array changed? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Any, Present
!
! Executable Statements
!
! Check whether argument has changed
! - integer arguments -
   If (Present(i)) Then
      If (i /= i_bk) set_options = record_change()
      If (Present(ldif_i)) ldif_i = (i /= i_bk)
   End If
! - real arguments -
   If (Present(r)) Then
      If (r /= r_bk) set_options = record_change()
      If (Present(ldif_r)) ldif_r = (r /= r_bk)
   End If
! - logical arguments -
   If (Present(l)) Then
      If (l .neqv. l_bk) set_options = record_change()
      If (Present(ldif_l)) ldif_l = (l .neqv. l_bk)
   End If
! - date arguments -
   If (Present(d)) Then
      If (d /= d_bk) set_options = record_change()
      If (Present(ldif_d)) ldif_d = (d /= d_bk)
   End If
! - real array arguments -
   If (Present(ra)) Then
      If (Any(ra(:) /= ra_bk(:))) set_options = record_change()
      If (Present(ldif_ra)) ldif_ra = Any(ra(:) /= ra_bk(:))
   End If
!
   Return
  End Subroutine check_change
 End Function set_options
!
!
!
 Function set_zero()
!
! Toggles zero-bound option, and resets standardization option if necessary
!
! Modules
   Use menus,  Only: record_change
   Use errors, Only: cpt_error
!
! Function result
   Integer :: set_zero
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Toggle zero-bound option
   izero = 1 - izero
!
! Check for invalid standardization setting
   set_zero = 2
   If (izero == 1) Return
   Select Case (istd)
    Case (3) ! - SPI -
      ifail = 1
    Case (4) ! - % average -
      ifail = 2
    Case Default
      set_zero = record_change()
      Return
   End Select
!
! Errors
   izero = 1
   Call cpt_error ('set_zero', .false., ifail)
!
   Return
 End Function set_zero
!
!
!
 Function get_cv()
!
! Initialises settings for cross-validated analysis
!
! Modules
   Use data_cpt_constants, Only: ie_cvh, ie_ret
!
! Function result
   Integer :: get_cv
!
! Executable Statements
!
! Set retroactive flag
   iretro = 0
   nretro = 1
   nt1 = nt
   nret = 1
   ntr = 0
   nur = 0
!
! Disable retroactive error variance
   If (iev == ie_ret) iev = ie_cvh
   get_cv = 0
!
   Return
 End Function get_cv
!
!
!
 Function get_retro()
!
! Initialises settings for retroactive analysis
!
! Modules
   Use labels, Only: cg_lenitp, cg_tperiodui
   Use gui,    Only: init_win, prompt_integer, win_prompt
!
! Function result
   Integer :: get_retro
#if GUI == 0
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Character(Len=64) :: cprmpt ! - prompt -
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
#if GUI == 0
   Intrinsic Ceiling, Real, Trim
#endif
!
! Executable Statements
!
! Calculate estimate of initial training period length
   If (nt1 > nt+1-mnu .or. nt1 < mnu) Then
      If (Mod(nt,2) == 0) Then
         nt1 = nt/2
      Else
         nt1 = nt/2 + 1
      End If
   End If
!
! Prompt for initial training period length
   iretro = 0
   Call init_win (title='Retroactive forecasts', &
        fclose=close_retro)
#if GUI == 1
   If (nt-mnu > mnu) Then
      Call prompt_integer (Trim(cg_lenitp), nt1, &
           itab=35, ilow=1, ihgh=nt+1-mnu, lfocus = .true.)
   Else
      Call prompt_integer (Trim(cg_lenitp), nt1, &
           itab=35)
   End If
   Call prompt_integer (Trim(cg_tperiodui), nretro, &
        itab=35, ilow=1)
   get_retro = win_prompt(.true., &
               fok=check_nt1)
#else
   Write (Unit=cprmpt, Fmt='(A,I0,A)') Trim(cg_lenitp)//' (suggested value ', nt1, ')'
   Call prompt_integer (Trim(cprmpt), nt1, &
        ifail=ifail, itab=35, ilow=1, ihgh=nt+1-mnu, lzero=.true., fcb1=check_nt1)
   If (ifail /= 0) GoTo 1
   Call prompt_integer (Trim(cg_tperiodui), nretro, &
        ifail=ifail, itab=35, ilow=1, lzero=.true.)
   If (ifail /= 0) GoTo 1
   get_retro = win_prompt(.true.)
#endif
   nret = Ceiling((Real(ntr)-0.5)/Real(nretro))
   get_retro = 0
   Return
!
#if GUI == 0
! Errors
1  iretro = -1
   get_retro = 0
!
   Return
#endif
 End Function get_retro
!
!
!
 Function close_retro() &
          Result (fclose)
!
! Halts retroactive analysis
!
! Function result
   Integer :: fclose
!
! Executable Statements
!
! Set retroactive flag
   If (iretro /= 1) iretro = -1
   fclose = 0
!
   Return
 End Function close_retro
!
!
!
 Function check_nt1() &
          Result (check)
!
! Checks initial length of training period
!
! Modules
   Use screen, Only: window_update
   Use gui,    Only: set_cursor_waiting
   Use errors, Only: cpt_error
!
! Function result
   Integer :: check
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Ceiling
  Intrinsic Real
!
! Executable Statements
!
! Force nt1 to be at least lcw+mnu-1
1 If (nt1 < mntu) Then
      ifail = 1
      Call cpt_error ('check_nt1', .false., ifail, &
           i_arg1=mnu-1)
#if GUI == 1
      check = 1
#else
      check = ifail
      If (ifail == 1) Return
#endif
      nt1 = mntu
#if GUI == 0
      GoTo 1
#endif
      Call window_update (nt1)
      Call set_cursor_waiting (0)
      iretro = 0
   Else
      ntr = nt - nt1
      nret = Ceiling((Real(ntr)-0.5)/Real(nretro))
      iretro = 1
      check = 0
   End If
!
   Return
 End Function check_nt1
!
!
!
 Function check_nt(nt, it1, n) &
          Result (check)
!
! Checks for availability of data for training period
!
! Function result
   Integer :: check
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt  ! - length of training period -
   Integer, Intent(In) :: it1 ! - first case to use -
   Integer, Intent(In) :: n   ! - number of cases in file -
!
! Executable Statements
!
! Determine whether there are sufficient data
   If (it1+nt-1 <= n) Then
      check = 0
   Else
      check = n + 1 - it1
   End If
!
   Return
 End Function check_nt
!
!
!
 Function check_n(n, lcw, lacw, ianal, icv2, iretro, ifail) &
          Result (check)
!
! Checks for sufficient number of cases and that number of cases is sufficiently more than length of cross-validation window.
!
! On exit:
!    check == 0 All checks passed
!    check == 1 n < mnu, and iretro == 0
!    check == 2 n < 2*mnu, and iretro == 1
!    check == 3 lcw > n-3, and iretro == 0; lcw reset to n-3
!    check == 4 lcw > n-mnu*2, and iretro == 1; lcw reset to n-mnu*2
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, mnt
   Use menus,              Only: record_change
!
! Function result
   Integer :: check
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n      ! - number of cases -
   Integer, Intent(In) :: ianal  ! - analysis indicator -
   Integer, Intent(In) :: icv2 ! - double cross-validation flag -
   Integer, Intent(In) :: iretro ! - retroactive flag -
!
   Logical, Intent(In) :: lacw ! - adjust cross-validation window? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: lcw    ! - length of cross-validation window -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error flag -
!
! Locals
!
! Local scalars
   Integer :: mn ! - minimum number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Mod
!
! Executable Statements
!
! Set minimum number of cases
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr, ia_gcm)
      mnu = mnt
      mntu = mnt + lcw - 1
    Case (ia_pfv)
      mnu = 1
      mntu = 1
   End Select
!
! Determine whether there are sufficient data
   Select Case (iretro)
    Case (0) ! - cross-validation -
      mn = mnu + icv2
      If (n < mn) Then
         ifail = mn
         check = 1
         Return
      End If
    Case (1) ! - retroactive -
      mn = 2*mnu - 1
      If (n < mn) Then
         ifail = mn
         check = 2
         Return
      End If
   End Select
!
! Force lcw to leave at least 3
   Select Case (iretro)
    Case (0) ! - cross-validation -
      mn = n - (mnu + icv2 - 1)
      If (lcw > mn) Then
         If (lacw) Then
            lcw = mn
            If (Mod(lcw,2) == 0) lcw = lcw - 1
         End If
         ifail = 0
         check = record_change()
         check = 3
         Return
      End If
    Case (1) ! - retroactive -
      mn = n - 2*(mnu - 1)
      If (lcw > mn) Then
         If (lacw) Then
            lcw = mn
            If (Mod(lcw,2) == 0) lcw = lcw - 1
         End If
         ifail = 2*mnu - 1
         check = record_change()
         check = 4
         Return
      End If
   End Select
!
   check = 0
!
   Return
 End Function check_n
!
!
!
 Subroutine get_maxn (icsv, lsetx, lsety, lprst, iseqx, iseqy, iseqm, lensn, lsn0, lsnn, fdatex, fdatey, bdatem, &
            gdatex, gdatey, gdatem, nt, ntm, &
            ih, mn)
!
! Calculates number of remaining pairs of data given start dates
!
! Modules
#if GUI == 1
   Use clrwin$,             Only: set_integer_limits$
   Use data_numbers,        Only: ihuge
#endif
   Use data_gui_constants,  Only: i_handle
   Use data_time_constants, Only: isq_sn, isq_so, isq_yr, nmn
   Use time,                Only: pdate, &
                                 iseq, &
                                 date_diff
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icsv  ! - calculate seasonal values flag -
   Integer, Intent(In) :: iseqx ! - X-file time-sequencing -
   Integer, Intent(In) :: iseqy ! - Y-file time-sequencing -
   Integer, Intent(In) :: iseqm ! - M-file time-sequencing -
   Integer, Intent(In) :: lensn ! - length of season -
   Integer, Intent(In) :: lsn0  ! - length of persistence seasons -
   Integer, Intent(In) :: lsnn  ! - length of last season -
!
   Logical, Intent(In) :: lprst ! - persistence component? -
   Logical, Intent(In) :: lsetx ! - X-file set? -
   Logical, Intent(In) :: lsety ! - Y-file set? -
!
   Type(pdate), Intent(In) :: fdatex ! - start-at date in X-file -
   Type(pdate), Intent(In) :: fdatey ! - start-at date in Y-file -
   Type(pdate), Intent(In) :: bdatem ! - beginning date in M-file -
   Type(pdate), Intent(In) :: gdatex ! - last date in X-file -
   Type(pdate), Intent(In) :: gdatey ! - last date in Y-file -
   Type(pdate), Intent(In) :: gdatem ! - last date in M-file -
!
! - input/output scalars -
   Integer, Intent(InOut) :: nt  ! - number of cases available -
   Integer, Intent(InOut) :: ntm ! - number of monthly cases available -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: mn ! - minimum number -
!
   Integer(Kind=i_handle), Optional :: ih ! - handle for prompt -
!
! Locals
!
! Local scalars
   Integer :: lssn ! - length of season -
   Integer :: ntx  ! - number of remaining cases in X-file -
   Integer :: nty  ! - number of remaining cases in Y-file -
   Integer :: n    ! - number of cases available -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Floor, Min, Mod, Real
#if GUI == 1
   Intrinsic Present
#endif
!
! Executable Statements
!
! Calculate remaining number of cases given start dates 
   If (lsetx) Then
      ntx = 1 + date_diff(fdatex, gdatex, iseqx)
   Else
      ntx = 0
   End If
   Select Case (icsv)
    Case (0) ! - no seasonal calculations -
      If (lsety) Then
         n = 1 + date_diff(fdatey, gdatey, iseqy)
         If (lsetx) n = Min(ntx, n)
      Else
         n = ntx
      End If
      ntm = n
    Case Default ! - seasonal calculations -
      If (.not.lprst) Then
         lssn = lensn
      Else
         If (iseq /= isq_so) Then
            lssn = lensn - lsn0
         Else
            lssn = lsnn
         End If
      End If
      ntm = 1 + date_diff(bdatem, gdatem, iseqm)
      nty = 1 + date_diff(fdatey, gdatey, iseqy)
      Select Case (iseq)
       Case (isq_yr) ! - yearly sequencing -
         If (ntm >= lssn) Then
            n = 1 + Floor(Real(ntm - lssn)/Real(nmn))
         Else
            n = 0
         End If
         n = Min(ntx, n)
       Case (isq_sn) ! - seasonal sequencing -
         n = Min(ntx, nty)
       Case (isq_so) ! - seasonally overlapping sequencing -
         If (ntm > lssn) Then
            n = ntm - Mod(lssn, nmn)
         Else
            n = ntm
         End If
         n = Min(ntx, n)
       Case Default ! - monthly sequencing -
         If (ntx > ntm) Then
            n = ntm - Mod(lssn, nmn)
         Else
            n = Min(ntx + Mod(lssn, nmn), ntm)
         End If
      End Select
   End Select
!
#if GUI == 1
! Set limits
   If (Present(ih)) Then
      If (lsetx .or. lsety) Then
         If (n > 0) Then
            Call set_integer_limits$ (ih, mn, n)
         Else
            Call set_integer_limits$ (ih, 0, 0)
         End If
      Else
         Call set_integer_limits$ (ih, 0, ihuge)
      End If
   End If
!
#endif
! Reset if smaller than current nt
   If (nt /= 0) Then
      nt = Min(n, nt)
   Else
      nt = n
   End If
!
   Return
 End Subroutine get_maxn
!
!
!
 Function set_nused(lset, kuse)
!
! Adjusts indices based on data availability
!
! Function result
   Integer :: set_nused
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lset ! - set nur? -
!
! - input arrays -
   Logical, Dimension(:), Intent(In) :: kuse ! - used case? -
!
! Locals
!
! Local scalars
   Integer :: k  ! - case index -
   Integer :: kk ! - available case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Count
!
! Executable Statements
!
! Check retroactive settings
   If (iretro == 1) Then
      If (mnu > 1) Then
         mntu = lcw + mnu - 1
      Else ! - PFV -
         nt1 = nt
         mntu = 1
      End If
      nu1 = nt1 - Count(.not.kuse(1:nt1))
      If (nu1 >= mntu) Then
         set_nused = 0
      Else
         nu1 = mntu
         kk = 0
         Do k = 1, nt
            If (kuse(k)) Then
               kk = kk + 1
               If (kk == nu1) Then
                  nt1 = k
                  Exit
               End If
            End If
         End Do
         Select Case (mntu)
          Case (2:)
            set_nused = 1
          Case (1)
            set_nused = 2
         End Select
      End If
      If (lset) nur = nu - nu1
   Else
      nu1 = 0
      set_nused = 0
   End If
!
   Return
 End Function set_nused
!
!
!
 Function set_it1(isq, sdate, fdate) &
          Result (it1)
!
! Checks that first date is available.
!
! Modules
   Use time, Only: pdate, &
                   date_diff
!
! Function result
   Integer :: it1
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq ! - sequence -
!
   Type(pdate), Intent(In) :: sdate ! - first date available -
   Type(pdate), Intent(In) :: fdate ! - first date of interest -
!
! Executable Statements
!
! Force first date of interest not to be before first date available
   it1 = 1 + date_diff(sdate, fdate, isq)
!
   Return
 End Function set_it1
!
!
!
 Function force_odd_lcw()
!
! Forces length of cross-validation window to be odd
!
! Modules
   Use maths,    Only: force_odd
   Use screen,   Only: window_update
#if GUI == 0
   Use gui,      Only: print_warning
#endif
!
! Function result
   Integer :: force_odd_lcw
!
#if GUI == 0
!
! Locals
!
! Local scalars
   Integer :: lcw_prv ! - previous value -
!
   Character(Len=40) :: cmsg ! - warning message -
!
#endif
! Executable Statements
!
! Force length of cross-validation window to be odd
#if GUI == 0
   lcw_prv = lcw
#endif
   Call force_odd (lcw_old, lcw)
   Call window_update (lcw)
   lcw_old = lcw
#if GUI == 0
   If (lcw /= lcw_prv) Then
      Write (Unit=cmsg, Fmt=*) 'Length has been changed to:', lcw
      Call print_warning (.true., 'Length of cross-validation window must be odd.', &
           msg2=cmsg)
   End If
#endif
   force_odd_lcw = 2
!
   Return
 End Function force_odd_lcw
!
!
!
 Subroutine proj_read_settings (iin, ianalp, icalc, pver, dofr, ifail)
!
! Reads program settings from project file
!
! Modules
   Use data_numbers,       Only: sp, tolv, zero
   Use data_cpt_constants, Only: ia_gcm, ia_pfv, ic_non, ir_ols, nbss
   Use time,               Only: itime, lead
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: ianalp ! - project analysis -
   Integer, Intent(In) :: icalc  ! - results available flag -
!
   Real(Kind=sp), Intent(In) :: pver ! - project version number -
!
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: dofr ! - degrees of freedom for regression -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: idum1 ! - dummy variable -
   Integer :: idum2 ! - dummy variable -
   Integer :: idum3 ! - dummy variable -
!
! Executable Statements
!
! Read program settings
   istd_bk = istd
   If (pver > 17.05_sp-tolv) Then
      Read (Unit=iin, Err=1, End=2) clf, ibi, ibs, idry, ieav, iensc, iev, iexc, ifpr, igauss,                   igcms,           &
                                    igood, igto, ilink, imc, intp, iodds, iprecf, iregr, isem, iskmask, iskmaski, istd, istdr,    &
                                    isynch, itime, izero, lcw, nenf, nf, nretro, nt, nt_old, nt1, rdry, rskmask
   Else If (pver > 17.02_sp-tolv) Then
      Read (Unit=iin, Err=1, End=2) clf, ibi, ibs,       ieav, iensc, iev, iexc, ifpr, igauss,                   igcms,           &
                                    igood, igto, ilink, imc, intp, iodds, iprecf, iregr, isem,                    istd, istdr,    &
                                    isynch, itime, izero, lcw, nenf, nf, nretro, nt, nt_old, nt1
   Else If (pver > 17.00_sp) Then
      Read (Unit=iin, Err=1, End=2) clf, ibi, ibs,       ieav, iensc, iev, iexc, ifpr, igauss,     idum1, idum2, igcms,    idum3, &
                                    igood, igto, ilink, imc, intp, iodds, iprecf, iregr, isem,                    istd, istdr,    &
                                    isynch, itime, izero, lcw, nenf, nf, nretro, nt, nt_old, nt1
   Else If (pver > 16.02_sp-tolv) Then
      Read (Unit=iin, Err=1, End=2) clf, ibi, ibs,       ieav, iensc, iev, iexc, ifpr, igauss,     idum1,        igcms,    idum3, &
                                    igood, igto,             intp, iodds, iprecf,        isem,                    istd, istdr,    &
                                    isynch, itime, izero, lcw, nenf, nf, nretro, nt, nt_old, nt1,       xfd_old, yfd_old
   Else
      Read (Unit=iin, Err=1, End=2) clf, ibi, ibs,       ieav, iensc, iev,       ifpr, igauss,     idum1,        igcms,    idum3, &
                                    igood, igto,             intp, iodds, iprecf,        isem,                    istd, istdr,    &
                                    isynch, itime, izero, lcw, nenf, nf, nretro, nt, nt_old, nt1,       xfd_old, yfd_old
   End If
   If (pver < 16.00_sp)      ieav = 0
   If (pver < 16.02_sp-tolv) iexc = 0
   If (pver < 17.00_sp-tolv) then
      ilink = 1
      imc = ic_non
      iregr = ir_ols
   End If
   If (pver < 17.05_sp-tolv) then
      idry = 0
      iskmask = 0
      iskmaski = 1
      rdry = zero
      rskmask = zero
   End If
   nb = nbss(ibi)
!
! Set backups
   Select Case (ianalp)
    Case (ia_gcm)
      igauss_bk = igauss
      igcms_bk = igcms
    Case (ia_pfv)
      igauss_bk = igauss
      istd_bk = istd
   End Select
   nt_old = nt
!
! Read results settings if saved
   If (icalc == 1) Then
      lzb = (igauss == 0 .and. izero == 1 .and. iregr == ir_ols)
      If (pver > 17.07_sp) Then
         Read (Unit=iin, Err=1, End=2)       icv2, iretro, iv, iva, ivf, ivf_old, ivfa, lead, &
                                       nret, ntr, nu, nur, nu1
      Else If (pver > 17.0_sp) Then
         Read (Unit=iin, Err=1, End=2)             iretro, iv, iva, ivf, ivf_old, ivfa, lead, &
                                       nret, ntr, nu, nur, nu1
      Else If (pver > 16.02_sp-tolv) Then
         Read (Unit=iin, Err=1, End=2) dofr,       iretro, iv, iva, ivf, ivf_old, ivfa, lead, &
                                       nret, ntr, nu, nur, nu1
      Else
         Read (Unit=iin, Err=1, End=2) dofr,       iretro, iv, iva, ivf, ivf_old, ivfa, lead, idum1, idum2, idum3, &
                                       nret, ntr, nu, nur, nu1
      End If
   End If
!
   ifail = 0
   Return
!
! Errors in project file
! - problem reading file -
1  ifail = 1
   Return
!
! - end of file -
2  ifail = 2
   Return
!
 End Subroutine proj_read_settings
!
!
!
 Subroutine proj_write_settings (iout, icalc, ifail)
!
! Writes program settings to project file
!
! Modules
   Use time, Only: itime, lead
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output file unit number -
   Integer, Intent(In) :: icalc ! - results available flag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write program settings
   Write (Unit=iout, Err=1) clf, ibi, ibs, idry, ieav, iensc, iev, iexc, ifpr, igauss,                  igcms,         &
                            igood, igto, ilink, imc, intp, iodds, iprecf, iregr, isem, iskmask, iskmaski, istd, istdr, &
                            isynch, itime, izero, lcw, nenf, nf, nretro, nt, nt_old, nt1, rdry, rskmask
!
! Write results settings if saved
   If (icalc == 1) Write (Unit=iout, Err=1) icv2, iretro, iv, iva, ivf, ivf_old, ivfa, lead, nret, ntr, nu, nur, nu1
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_settings
!
!
!
 Subroutine parameter_write_settings (iout, ifail)
!
! Writes program settings to parameter file
!
! Modules
   Use data_cpt_constants, Only: ir_gam, ir_poi, nbss
   Use labels,             Only: ca_corrects, ca_goodi, ca_interps, ca_links, ca_mcs, ca_regrs, ca_times, ca_tos, &
                                 cg_lcv, &
                                 l_drymask, l_skillmask
   Use time,               Only: itime
   Use menus,              Only: mn_opts_aveens, mn_opts_cv, mn_opts_gcm, mn_opts_good, mn_opts_link, mn_opts_mask, &
                                 mn_opts_maskdry, mn_opts_maskskll, mn_opts_pval, mn_opts_regr, mn_opts_sortens, &
                                 mn_opts_synch, mn_opts_time, mn_opts_trnss, mn_opts_trnsy, mn_opts_verif, mn_opts_zerob, &
                                 mn_screen_lentp
   Use parameter_file,     Only: output_parameter
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
! Locals
!
! Local scalars
   Character :: cyn ! - Y/N -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Write project settings
! - length of training period -
   If (mn_screen_lentp%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_screen_lentp%l_lab%c), i=mn_screen_lentp%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           i=nt) /= 0) GoTo 1
   End If
! - length of cross-validation window -
   If (mn_opts_cv%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_cv%l_lab%c), i=mn_opts_cv%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc=Trim(cg_lcv), i=lcw) /= 0) GoTo 1
   End If
! - time settings
   If (mn_opts_time%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_time%l_lab%c), i=mn_opts_time%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc=Trim(ca_times(itime)), i=itime) /= 0) GoTo 1
   End If
! - transformation options -
   If (mn_opts_trnss%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_trnss%l_lab%c), i=mn_opts_trnss%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc=Trim(ca_tos(igto)), i=igto) /= 0) GoTo 1
   End If
! - transform Y -
   If (mn_opts_trnsy%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_trnsy%l_lab%c), i=mn_opts_trnsy%id) /= 0) GoTo 1
   End If
! - zero-bound -
   If (mn_opts_zerob%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_zerob%l_lab%c), i=mn_opts_zerob%id) /= 0) GoTo 1
   End If
! - synchronous predictors -
   If (mn_opts_synch%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_synch%l_lab%c), i=mn_opts_synch%id) /= 0) GoTo 1
   End If
! - set sort-ensemble members flag -
   If (mn_opts_sortens%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_sortens%l_lab%c), i=mn_opts_sortens%id) /= 0) GoTo 1
   End If
! - ensemble-average -
   If (mn_opts_aveens%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_aveens%l_lab%c), i=mn_opts_aveens%id) /= 0) GoTo 1
   End If
! - GCM options - 
   If (mn_opts_gcm%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_gcm%l_lab%c), i=mn_opts_gcm%id) /= 0) GoTo 1
      If (output_parameter(iout, & ! - interpolation option -
                           cdesc=Trim(ca_interps(2-intp)), i=2-intp) /= 0) GoTo 1
      If (output_parameter(iout, & ! - model correction option -
                           cdesc=Trim(ca_corrects(igcms+1)), i=igcms+1) /= 0) GoTo 1
      If (output_parameter(iout, & ! - model combination -
                           cdesc=Trim(ca_mcs(imc+1)), i=imc+1) /= 0) GoTo 1
   End If
! - regression - 
   If (mn_opts_regr%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_regr%l_lab%c), i=mn_opts_regr%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc=Trim(ca_regrs(iregr)), i=iregr) /= 0) GoTo 1
   End If
   Select Case (iregr)
    Case (ir_poi, ir_gam)
      If (mn_opts_regr%iact > 0) Then ! - link function is automatically requested -
         If (output_parameter(iout, &
                              cdesc=Trim(ca_links(ilink)), i=ilink) /= 0) GoTo 1
      Else If (mn_opts_link%iact > 0) Then ! - link function is automatically requested -
         If (output_parameter(iout, &
                              cdesc=Trim(mn_opts_link%l_lab%c), i=mn_opts_link%id) /= 0) GoTo 1
         If (output_parameter(iout, &
                              cdesc=Trim(ca_links(ilink)), i=ilink) /= 0) GoTo 1
      End If
   End Select
! - goodness index -
   If (mn_opts_good%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_good%l_lab%c), i=mn_opts_good%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc=Trim(ca_goodi(igood)), i=igood) /= 0) GoTo 1
   End If
! - masking -
   If (mn_opts_mask%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_mask%l_lab%c), i=mn_opts_mask%id) /= 0) GoTo 1
   Else
      If (mn_opts_maskdry%iact > 0) Then
         If (output_parameter(iout, &
                              cdesc=Trim(mn_opts_maskdry%l_lab%c), i=mn_opts_maskdry%id) /= 0) GoTo 1
      End If
      If (mn_opts_maskskll%iact > 0) Then
         If (output_parameter(iout, &
                              cdesc=Trim(mn_opts_maskskll%l_lab%c), i=mn_opts_maskskll%id) /= 0) GoTo 1
      End If
   End If
   If (mn_opts_mask%iact > 0 .or. mn_opts_maskdry%iact > 0) Then ! - dry masking -
      If (idry == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If 
      If (output_parameter(iout, &
                           cdesc=Trim(l_drymask%c), c=cyn) /= 0) GoTo 1
      If (idry /= 0) Then
         If (output_parameter(iout, &
                              r=rdry) /= 0) GoTo 1
      End If
   End If
   If (mn_opts_mask%iact > 0 .or. mn_opts_maskskll%iact > 0) Then ! - skill masking -
      If (iskmask == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If 
      If (output_parameter(iout, &
                           cdesc=Trim(l_skillmask%c), c=cyn) /= 0) GoTo 1
      If (idry /= 0) Then
         If (output_parameter(iout, &
                              r=rskmask) /= 0) GoTo 1
      End If
   End If
! - p-values -
   If (mn_opts_pval%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_pval%l_lab%c), i=mn_opts_pval%id) /= 0) GoTo 1
   End If
! - verification options -
   If (mn_opts_verif%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_verif%l_lab%c), i=mn_opts_verif%id) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc='Number of probability bins', i=nbss(ibi)) /= 0) GoTo 1
      If (ibs == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If 
      If (output_parameter(iout, &
                           cdesc='Calculate scores using binned probabilities?', c=cyn) /= 0) GoTo 1
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
 End Subroutine parameter_write_settings
End Module settings
