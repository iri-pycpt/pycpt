! Author: Simon Mason
Module climate
!
! Modules
   Use data_numbers,      Only: rp
   Use data_io_constants, Only: lprd
   Use time,              Only: pdate
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: calc_climates, check_climates, close_climate, get_clim_limits, init_climate, init_climatep, parameter_write_climate, &
             print_climatology, proj_read_climate, proj_write_climate, reset_climate, set_climate, update_climate, valid_climate
!
! Arrays
!
! Integer arrays
   Integer, Dimension(:), Allocatable, Public :: ncu ! - number of used climatological cases -
!
! Real arrays
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: ave   ! - climatological averages -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: climx ! - model climatological (sorted) data -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: sdev  ! - climatological standard deviations -
!
   Real(Kind=rp), Dimension(:,:,:), Pointer, Public :: clim => Null() ! - climatological (sorted) response data -
!
! Scalars
!
! Integer scalars
   Integer, Public :: icex = 0  ! - climatological period lies outside the range of the training period flag -
!                                   icex = :
!                                      0 climatological period lies within the range of the training period -
!                                      1 climatological period starts before the training period -
!                                      2 climatological period ends after the training period -
!                                      3 climatological period spans the range of the training period -
!                                     -1 climatological period starts and ends before the training period with a gap -
!                                     -2 climatological period starts and ends after the training period with a gap -
   Integer, Public :: idclimyr1 ! - default climatological period start -
   Integer, Public :: idclimyr2 ! - default climatological period end -
   Integer, Public :: ieclim    ! - climatological period can extend beyond training period flag -
   Integer, Public :: ieclim_bk ! - climatological period can extend beyond training period backup flag -
   Integer, Public :: itrain1   ! - index of first date of training period (relative to combined climate and training dataset) -
   Integer, Public :: itrain2   ! - index of last date of training period (relative to combined climate and training dataset) -
   Integer, Public :: nc        ! - length of climatological period -
   Integer, Public :: ncy       ! - length of climatological period in years -
   Integer, Public :: ntc       ! - length of climatological period with length of training period -
!
   Integer, Private :: iclim1 ! - index of first date of climatological period -
   Integer, Private :: iclim2 ! - index of last date of climatological period -
   Integer, Private :: lcsn   ! - length of climatological season -
!
! Derived types
!
! Derived type definitions
! - climatological period -
   Public clim_per
   Type clim_per
      Sequence
      Type(pdate) :: d1        ! - first date of climatology -
      Type(pdate) :: d2        ! - last date of climatology -
!
      Character(lprd) :: clim1 ! - first date of climatology -
      Character(lprd) :: clim2 ! - last date of climatology -
   End Type clim_per
!
! Interface operators
   Private :: Operator(==)
!
   Public :: Operator(/=)
!
   Interface Operator(==)
      Module Procedure same_clim_per
   End Interface
   Interface Operator(/=)
      Module Procedure diff_clim_per
   End Interface
!
! Derived type scalars
   Type(clim_per), Public :: climate_per ! - climatological period -
   Type(clim_per), Public :: climate_bak ! - backup climatological period -
!
Contains
!
!
 Function same_clim_per(cp1, cp2) &
          Result(same)
!
! Are climate periods identical?
!
! Modules
   Use time, Only: Operator(==)
!
! Function result
   Logical same
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(clim_per), Intent(In) :: cp1 ! - first climatological period -
   Type(clim_per), Intent(In) :: cp2 ! - second climatological period -
!
! Executable Statements
!
! Test whether climate periods are identical
   same = (cp1%d1 == cp2%d1 .and. cp1%d2 == cp2%d2)
!
   Return
 End Function same_clim_per
!
!
!
 Function diff_clim_per(cp1, cp2) &
          Result(differ)
!
! Are climate periods different?
!
! Modules
   Use time, Only: Operator(==)
!
! Function result
   Logical differ
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(clim_per), Intent(In) :: cp1 ! - first climatological period -
   Type(clim_per), Intent(In) :: cp2 ! - second climatological period -
!
! Executable Statements
!
! Test whether climate periods are different
   differ = .not.(cp1%d1 == cp2%d1 .and. cp1%d2 == cp2%d2)
!
   Return
 End Function diff_clim_per
!
!
!
 Subroutine reset_climate ()
!
! Resets climatological period
!
! Modules
   Use data_time_constants, Only: isq_yr
   Use time,                Only: Operator(==), &
                                  date_diff
   Use menus,               Only: set_menu_greying
!
! Executable Statements
!
! Switch off climatological period
   Call set_menu_greying (lclim=.false.)
   iclim1 = 0
   iclim2 = 0
   itrain1 = 0
   itrain2 = 0
   lcsn = 0
   ncy = 0
   ntc = 0
   Call init_climatep ()
   If (climate_per%d1 == 0) Then
      nc = 0
   Else
      nc = date_diff(climate_per%d1, climate_per%d2, isq_yr) + 1
   End If
!
   Return
 End Subroutine reset_climate
!
!
!
 Subroutine init_climatep ()
!
! Initialises climatological period
!
! Modules
   Use time, Only: Assignment(=)
!
! Executable Statements
!
! Initialise climatological period
   climate_per%d1 = 0      ! - first date of climatology -
   climate_per%d1%iyr = idclimyr1
   climate_per%d2 = 0      ! - last date of climatology -
   climate_per%d2%iyr = idclimyr2
   climate_per%clim1 = ' ' ! - first period of climatology -
   climate_per%clim2 = ' ' ! - last period of climatology -
!
   Return
 End Subroutine init_climatep
!
!
!
 Subroutine init_climate (lb, ns, ifail)
!
! Initialises memory for climatologies
! For persistence, the last dimension is set as:
!     0 = persiste season
!     1 = predicted season
!     2 = combined season
!
! Modules
   Use arrays,   Only: rwk
   Use iofiles,  Only: mya
   Use distribs, Only: init_distribs
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lb ! - lower bound -
   Integer, Intent(In) :: ns ! - number of seasons -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free existing memory
   If (Allocated(rwk)) Deallocate (rwk)
!
! Allocate memory
! - climatological data -
   Allocate (clim(mya,nc,lb:ns), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - number of used climatological cases -
   Allocate (ncu(lb:ns), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - climatological averages -
   Allocate (ave(mya,lb:ns), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - climatological standard deviations -
   Allocate (sdev(mya,lb:ns), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - distribution parameters -
   Call init_distribs (mya, lb, ns, ifail)
   If (ifail /= 0) GoTo 1
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_climate
!
!
!
 Function update_climate(icalc) &
          Result (update)
!
! Updates climatological period
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so, isq_yr, nmn
   Use time,                Only: add_to_month
   Use iofiles,             Only: yfile
   Use fields,              Only: yfield
   Use season,              Only: lprst, lsn, nsn, nsnc
!
! Function result
   Integer :: update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icalc ! - calculation completed flag -
!
! Executable Statements
!
! Update climatological period
   Select Case (yfile%iseq)
    Case (isq_yr)
      climate_per%d1%imn = add_to_month(yfile%prd1%sdate%imn, -lsn(0))
      climate_per%d1%idy = 0
      climate_per%d2%imn = climate_per%d1%imn
      climate_per%d2%idy = 0
    Case (isq_sn)
      climate_per%d1%idy = 0
      climate_per%d2%imn = yfield(nsn)%tssn%tprd(1)%sdate%imn
      climate_per%d2%idy = 0
    Case (isq_so)
      climate_per%d1%idy = 0
      climate_per%d2%imn = add_to_month(climate_per%d2%imn, nmn-1)
      climate_per%d2%idy = 0
   End Select
!
! Recalculate climatology
   update = 0
   If (icalc == 0) Then
      Call set_climate (icalc, lsn(0))
   Else
      If (icex == 0) Then
         update = redefine_clim(icalc, lsn(0), nsnc, lprst)
      Else
         Call set_climate (icalc, lsn(0))
         Call label_climate (lsn(0))
      End If
   End If
!
   Return
 End Function update_climate
!
!
!
 Function redefine_clim(icalc, lsn0, ns, lprst)
!
! Redefines climatological period
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: it_non, iv_ple, iv_abs, iv_ref
   Use progress_meter,     Only: end_progress_meter, set_progress_increment, start_progress_meter
   Use settings,           Only: igauss, nu
   Use arrays,             Only: y
   Use iofiles,            Only: mya
   Use season,             Only: isnc, lb
   Use categories,         Only: ithr, tobs, tobst, &
                                 set_percentiles, set_thresholds
!
! Function result
   Integer :: redefine_clim
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icalc ! - calculation completed flag -
   Integer, Intent(In) :: lsn0  ! - length of persistence seasons -
   Integer, Intent(In) :: ns    ! - number of seasons -
!
   Logical, Intent(In) :: lprst ! - include persistence component? -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Recalculate climatology
   Call set_climate (icalc, lsn0)
   If (iclim1 < 1 .or. iclim2 > nu) Then
      redefine_clim = -1
      Return
   End If
   Select Case (ithr)
    Case (iv_ple, iv_ref) ! - percentile and reference thresholds -
      Select Case (igauss)
       Case (it_non)
         Call set_progress_increment (one/Real(3, Kind=rp), .false.)
       Case Default
         Call set_progress_increment (one/Real(4, Kind=rp), .false.)
      End Select
    Case (iv_abs) ! - absolute thresholds -
      Select Case (igauss)
       Case (it_non)
         Call set_progress_increment (one/Real(2, Kind=rp), .false.)
       Case Default
         Call set_progress_increment (one/Real(3, Kind=rp), .false.)
      End Select
   End Select
   Call start_progress_meter ('Calculating climatologies', .false.)
   Call calc_climates (.true., mya, lb, ns, y(1:mya,:,lb:), ncu(lb:), clim(1:mya,:,lb:), ave(:,lb:), sdev(:,lb:), ifail)
   If (lprst) ncu(2) = ncu(1)
!
! Calculate thresholds
   If (ifail == 0) Then
      Select Case (ithr)
        Case (iv_ple) ! - percentile thresholds -
         Call set_thresholds (igauss, mya, 1, ncu(isnc:isnc), clim(:,:,isnc:isnc), tobs, tobst, ifail)
         redefine_clim = ifail
        Case (iv_abs) ! - absolute thresholds -
         redefine_clim = set_percentiles(1, ncu(isnc:isnc), clim(:,:,isnc:isnc))
         Call set_thresholds (igauss, mya, 1, ncu(:), clim(:,:,isnc:isnc), tobs, tobst, ifail)
         redefine_clim = ifail
       Case (iv_ref) ! - reference thresholds -
         redefine_clim = set_percentiles(1, ncu(isnc:isnc), clim(:,:,isnc:isnc))
      End Select
      If (redefine_clim /= 0) Then
         climate_per = climate_bak
         Call end_progress_meter (.false., .false.)
         redefine_clim = 1
      End If
      Call end_progress_meter (.false., .true.)
      redefine_clim = 0
   Else
      climate_per = climate_bak
      Call end_progress_meter (.false., .false.)
      redefine_clim = 1
   End If
!
   Return
 End Function redefine_clim
!
!
!
 Function valid_climate()
!
! Checks that end of climatological period is later than beginning
!
! Modules
   Use time,     Only: Operator(+), &
                       date_diff, nyears
   Use screen,   Only: window_update
   Use errors,   Only: cpt_error
   Use settings, Only: nt
   Use iofiles,  Only: yfile
!
! Function result
   Integer :: valid_climate
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for valid settings
   nc = date_diff(climate_per%d1, climate_per%d2, yfile%iseq) + 1
   ncy = nyears(nc, .true.)
   If (nc <= 0) Then
      climate_per%d2 = yfile%prd1%sdate + (yfile%it1 + nt - 2)
      Call window_update (climate_per%d2%iyr)
      ifail = 1
      Call cpt_error ('valid_climate', .false., ifail, &
           i_arg1=yfile%iseq)
      valid_climate = 2
   Else
      valid_climate = -1
   End If
   Call window_update (ncy)
!
   Return
 End Function valid_climate
!
!
!
 Subroutine check_climates (cfile, mfile, lsn0, lpfv)
!
! Checks that climate period is within data range
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so
   Use time,                Only: Operator(+), Operator(-), Operator(>), Operator(<), Operator(>=), Operator(<=), Operator(==), &
                                  iseq, nss, &
                                  add_months, adjust_pdate, date_diff, nyears
   Use screen,              Only: window_update
   Use settings,            Only: nt
   Use iofiles,             Only: ifile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lsn0 ! - length of persistence season -
!
   Logical, Intent(In) :: lpfv ! - PFV? -
!
   Type(ifile), Intent(In) :: cfile ! - file containing climatological data -
   Type(ifile), Intent(In) :: mfile ! - file containing monthly climatological data -
!
! Locals
!
! Local scalars
   Integer :: idc ! - difference between first and last seasons of climatology -
!
   Type(pdate) :: dbdate ! - default beginning date -
   Type(pdate) :: dldate ! - default last date -
   Type(pdate) :: d1     ! - first date -
   Type(pdate) :: d2     ! - second date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Executable Statements
!
! Set default climatological date limits
   Select Case (ieclim)
! - set default climatological period to the training period -
    Case (0)
      dbdate = cfile%bdate
      dldate = cfile%bdate + (nt - 1)
    Case (1)
! - set default climatological period immediately before training period, if possible -
      dbdate = cfile%prd1%sdate
      If (lpfv) Then
         dldate = cfile%bdate - 1
         If (dldate <= dbdate) dldate = cfile%prdn%sdate ! - set to the end of the full period if end of period is before start -
! - set default climatological period to the full period -
      Else
         dldate = cfile%prdn%sdate
      End If
   End Select
!
! Set to default if climatological period has not been previously set
   If (nc <= 1) Then
      climate_per%d1 = dbdate
      climate_per%d2 = dldate
!
! Apply season settings to climatology
   Else
      climate_per%d1%imn = cfile%prd1%sdate%imn
      If (lsn0 > 0) Call add_months (-lsn0, climate_per%d1) ! - adjust climatology start date for persistence component -
      Select Case (ieclim)
       Case (0)
         If (climate_per%d1 < mfile%bdate) Call adjust_pdate (cfile%iseq, .true., mfile%bdate, climate_per%d1)
       Case (1)
         If (climate_per%d1 < mfile%prd1%sdate) Call adjust_pdate (cfile%iseq, .true., mfile%prd1%sdate, climate_per%d1)
      End Select
      climate_per%d2%imn = cfile%prdn%sdate%imn
      If (climate_per%d2 > mfile%prdn%edate) Call adjust_pdate (cfile%iseq, .false., mfile%prdn%edate, climate_per%d2)
! - check that previously set limits are within range of data -
      If (climate_per%d1 < dbdate) climate_per%d1 = dbdate
      If (climate_per%d2 > dldate) climate_per%d2 = dldate
      If (climate_per%d1 > climate_per%d2) Then
         climate_per%d1 = dbdate
         climate_per%d2 = dldate
      End If
   End If
! - adjust end date to ensure equal number of each season -
   Select Case (iseq)
    Case (isq_sn, isq_so)
      idc = date_diff(climate_per%d1, climate_per%d2, iseq) + 1
      idc = Mod(idc, nss)
      If (idc /= 0) climate_per%d2 = climate_per%d2 - idc
   End Select
!
! Check whether climatological period extends outside of training period
   icex = get_icex(cfile%bdate, cfile%bdate+(nt-1))
   Select Case (icex)
    Case (0)
      Continue
    Case (-1) ! - climatological period ends before start of training period -
      iclim1 = 1
    Case (1) ! - climatological period starts before training period -
      iclim1 = 1
      d1 = climate_per%d1
      d2 = cfile%bdate + (nt - 1)
    Case (3) ! - climatological period contains training period -
      iclim1 = 1
      d1 = climate_per%d1
      d2 = climate_per%d2
    Case (-2) ! - climatological period ends after training period -
      iclim1 = nt + 1
    Case (2) ! - climatological period ends after training period -
      iclim1 = date_diff(cfile%bdate, climate_per%d1, cfile%iseq) + 1
      d1 = cfile%bdate
      d2 = climate_per%d2
   End Select
!
! Determine length of climatological and combined periods
   nc =  date_diff(climate_per%d1, climate_per%d2, iseq) + 1
   Select Case (icex)
    Case (0)
      ntc = nt
    Case (1:)
      ntc = date_diff(d1, d2, iseq) + 1
    Case (:-1)
      ntc = nt + nc
   End Select
   ncy = nyears(nc, .true.)
   Call window_update (ncy)
!
   Return
 End Subroutine check_climates
!
!
!
 Subroutine set_climate (icalc, lsn0)
!
! Sets climatological parameters given date ranges
!
! Modules
   Use data_time_constants, Only: isq_dy
   Use time,                Only: Operator(+), Operator(-), &
                                  iseq, lslim, &
                                  date_diff, reset_iseq
   Use settings,            Only: nt
   Use iofiles,             Only: yfile
   Use fields,              Only: yfield
   Use season,              Only: len_pprd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icalc ! - calculation completed flag -
   Integer, Intent(In) :: lsn0  ! - length of persistence seasons -
!
! Locals
!
! Local scalars
   Logical :: ls_bk ! - backup use season limits? -
!
! Executable Statements
!
! Identify climatological data
   icex = get_icex(yfile%bdate, yfile%bdate+(nt-1))
   Call get_clim_limits (icex, iseq, &
        iclim1=iclim1, iclim2=iclim2, itrain1=itrain1, itrain2=itrain2)
! - adjust for compression if analysis is running -
   If (icalc == 1) Call set_nc ()
!
! Calculate length of climatological season
   Select Case (yfile%nse)
    Case Default
      lcsn = len_pprd(yfile%prd1, yfile%iseq)
    Case (2:)
      Call reset_iseq (isq_new=isq_dy)
      ls_bk = lslim
      lslim = .false.
      lcsn = date_diff(yfield(1)%tssn%tprd(1)%sdate, yfield(yfile%nse)%tssn%tprd(1)%edate, isq_dy) + 1
      lslim = ls_bk
      Call reset_iseq ()
   End Select
!
! Construct climatological period labels
   Call label_climate (lsn0)
!
   Return
 End Subroutine set_climate
!
!
!
 Subroutine set_nc ()
!
! Sets length of climatological period
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so
   Use time,                Only: iseq, &
                                  nyears
   Use settings,            Only: nu
   Use missing,             Only: kcuse
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count
!
! Executable Statements
!
! Identify length of available climatological data
   If (iseq /= isq_sn .and. iseq /= isq_so) Then
      If (iclim1 >= 1) Then
         iclim1 = iclim1 - Count(.not.kcuse(1:iclim1))
         If (iclim1 == 0) iclim1 = 1
      End If
      iclim2 = iclim2 - Count(.not.kcuse(1:iclim2))
      nc = iclim2 + 1 - iclim1
      ncu(:) = nc
      If (itrain1 >= 1) Then
         itrain1 = itrain1 - Count(.not.kcuse(1:itrain1))
         If (itrain1 == 0) itrain1 = 1
      End If
      itrain2 = itrain1 + nu - 1
   Else
! - calculate separately for each season (data are not yet compressed) -
      ncu(:) = nyears(nc, .false.)
   End If
!
   Return
 End Subroutine set_nc
!
!
!
 Function get_icex(d1, d2, &
          lsn0) &
          Result (icex)
!
! Identifies whether climatological period lies outside training period
!
! Modules
   Use data_time_constants, Only: isq_mn
   Use time,                Only: Operator(+), Operator(-), Operator(>), Operator(<), &
                                  date_diff
!
! Function result
   Integer :: icex
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(pdate), Intent(In) :: d1 ! - first date of training period -
   Type(pdate), Intent(In) :: d2 ! - end date training period -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: lsn0 ! - length of persistence season -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Identify whether climatological period lies outside training period
   icex = 0
   If (climate_per%d1 < d1) Then
      icex = 1
   Else If (Present(lsn0)) Then
      If (lsn0 > 0) Then ! - adjust for persistence component -
         If (date_diff(climate_per%d1, d1, isq_mn) > lsn0) icex = 1
      End If
   End If
   If (climate_per%d2 > d2) icex = icex + 2
!
! Identify whether there is a gap between the climatological and training periods
   Select Case (icex)
    Case (1) ! - climatological period is before training period -
      If (climate_per%d2 < (d1 - 1)) icex = -icex
    Case (2) ! - climatological period is after training period -
      If (climate_per%d1 > (d2 + 1)) icex = -icex
   End Select
!
   Return
 End Function get_icex
!
!
!
 Subroutine get_clim_limits (icex, isq, &
            iclim1, iclim2, itrain1, itrain2)
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so, isq_yr
   Use time,                Only: date_diff, nyears
   Use settings,            Only: nt
   Use iofiles,             Only: yfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icex ! - climatological period extends beyond training period flag -
   Integer, Intent(In) :: isq  ! - time-sequencing -
!
! Optional arguments
! - optional output scalars -
   Integer, Intent(Out), Optional :: iclim1  ! - index of first date of climatological period -
   Integer, Intent(Out), Optional :: iclim2  ! - index of last date of climatological period -
   Integer, Intent(Out), Optional :: itrain1 ! - index of first date of training period (relative to combined climate and training dataset) -
   Integer, Intent(Out), Optional :: itrain2 ! - index of last date of training period (relative to combined climate and training dataset) -
!
! Local scalars
   Integer :: i1 ! - index of first date -
!
! Executable Statements
!
! Identify limits of the climatological and training periods
   Select Case (isq)
    Case Default
      Select Case (icex)
       Case (0)
         If (Present(iclim1)) iclim1 = date_diff(yfile%bdate, climate_per%d1, yfile%iseq) + 1
         If (Present(iclim2)) iclim2 = date_diff(yfile%bdate, climate_per%d2, yfile%iseq) + 1
         If (Present(itrain1)) itrain1 = 1
         If (Present(itrain2)) itrain2 = nt
       Case (1) ! - climatological period is before training period (no gap) -
         If (Present(iclim1)) iclim1 = 1
         If (Present(iclim2)) iclim2 = nc
         If (Present(itrain1) .or. Present(itrain2)) i1 = date_diff(climate_per%d1, yfile%bdate, yfile%iseq) + 1
         If (Present(itrain1)) itrain1 = i1
         If (Present(itrain2)) itrain2 = i1 + nt - 1
       Case (-1) ! - climatological period is before training period (gap) -
         If (Present(iclim1)) iclim1 = 1
         If (Present(iclim2)) iclim2 = nc
         If (Present(itrain1)) itrain1 = nc + 1
         If (Present(itrain2)) itrain2 = ntc
       Case (2) ! - climatological period is after training period (no gap) -
         If (Present(iclim1) .or. Present(iclim2)) i1 = date_diff(yfile%bdate, climate_per%d1, yfile%iseq) + 1
         If (Present(iclim1)) iclim1 = i1
         If (Present(iclim2)) iclim2 = i1 + nc - 1
         If (Present(itrain1)) itrain1 = 1
         If (Present(itrain2)) itrain2 = nt
       Case (-2) ! - climatological period is after training period (gap) -
         If (Present(iclim1)) iclim1 = nt + 1
         If (Present(iclim2)) iclim2 = ntc
         If (Present(itrain1)) itrain1 = 1
         If (Present(itrain2)) itrain2 = nt
       Case (3) ! - training period is within climatological period -
         If (Present(iclim1)) iclim1 = 1
         If (Present(iclim2)) iclim2 = ntc
         If (Present(itrain1)) itrain1 = date_diff(climate_per%d1, yfile%bdate, yfile%iseq) + 1
         If (Present(itrain2)) itrain2 = itrain1 + nt - 1
      End Select
! - correct for seasonally sequenced data -
    Case (isq_sn, isq_so)
      Select Case (icex)
       Case (0)
         If (Present(iclim1)) iclim1 = date_diff(yfile%bdate, climate_per%d1, isq_yr) + 1
         If (Present(iclim2)) iclim2 = date_diff(yfile%bdate, climate_per%d2, isq_yr) + 1
         If (Present(itrain1)) itrain1 = 1
         If (Present(itrain2)) itrain2 = nyears(nt, .false.)
       Case (1) ! - climatological period is before training period (no gap) -
         If (Present(iclim1)) iclim1 = 1
         If (Present(iclim2)) iclim2 = iclim1 + nyears(nc, .false.) - 1
         If (Present(itrain1)) itrain1 = date_diff(climate_per%d1, yfile%bdate, isq_yr) + 1
         If (Present(itrain2)) itrain2 = itrain1 + nyears(nt, .false.) - 1
       Case (-1) ! - climatological period is before training period (gap) -
         If (Present(iclim1)) iclim1 = 1
         If (Present(iclim2)) iclim2 = nyears(nc, .false.)
         If (Present(itrain1)) itrain1 = nyears(nc, .false.) + 1
         If (Present(itrain2)) itrain2 = nyears(ntc, .false.)
       Case (2) ! - climatological period is after training period (no gap) -
         If (Present(iclim1) .or. Present(iclim2)) i1 = date_diff(yfile%bdate, climate_per%d1, isq_yr) + 1
         If (Present(iclim1)) iclim1 = i1
         If (Present(iclim2)) iclim2 = i1 + nyears(nc, .false.) - 1
         If (Present(itrain1)) itrain1 = 1
         If (Present(itrain2)) itrain2 = nyears(nt, .false.)
       Case (-2) ! - climatological period is after training period (gap) -
         If (Present(iclim1)) iclim1 = nyears(nt, .false.) + 1
         If (Present(iclim2)) iclim2 = nyears(ntc, .false.)
         If (Present(itrain1)) itrain1 = 1
         If (Present(itrain2)) itrain2 = nyears(nt, .false.)
       Case (3) ! - training period is within climatological period -
         If (Present(iclim1)) iclim1 = 1
         If (Present(iclim2)) iclim2 = nyears(ntc, .false.)
         If (Present(itrain1) .or. Present(itrain2)) i1 = date_diff(climate_per%d1, yfile%bdate, isq_yr) + 1
         If (Present(itrain1)) itrain1 = i1
         If (Present(itrain2)) itrain2 = i1 + nyears(nt, .false.) - 1
      End Select
   End Select
!
   Return
 End Subroutine get_clim_limits
!
!
!
 Subroutine label_climate (lsn0)
!
! Constructs climatological period labels
!
! Modules
   Use data_io_constants,   Only: io_com
   Use data_time_constants, Only: isq_dy, isq_mn, isq_sn, nmn
   Use time,                Only: pdate, &
                                  iseq, lslim, nse, nss, &
                                  date_diff, get_cdate, reset_iseq, same_ssn
   Use iofiles,             Only: yfile
   Use fields,              Only: yfield
   Use season,              Only: which_season
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lsn0 ! - length of persistence seasons -
!
! Locals
!
! Local scalars
   Integer :: isn ! - season index -
   Integer :: l   ! - season counter -
   Integer :: ll  ! - season counter -
   Integer :: ls1 ! - length of season -
   Integer :: ls2 ! - length of season -
!
   Logical :: ls_bk ! - backup use season limits? -
   Logical :: lseq  ! - reset time sequencing -
!
   Type(pdate) :: d2 ! - end date of climatology -
!
! Executable Statements
!
! Determine length of climatological season
   lseq = (iseq > 0)
   If (lseq) Then
      Call reset_iseq (isq_new=isq_dy)
      If (nse > 0) Then
         ls_bk = lslim
         lslim = .false.
         ls1 = date_diff(yfile%prd1%sdate, yfield(yfile%nse)%tssn%tprd(1)%edate, isq_dy) + 1
         ls1 = lcsn + lsn0
      Else
         ls1 = iseq
      End If
   Else If (lcsn <= nmn) Then
      ls1 = lcsn + lsn0
   Else
      ls1 = lcsn + lsn0 + 1 - nmn
   End If
   Select Case (iseq)
    Case Default
      ls2 = ls1
      d2 = climate_per%d2
    Case (isq_sn)
      isn = which_season(yfile%fdate, nss, yfield(:)%tssn%tprd(1)%sdate) ! - identify first season used -
      isn = isn - 1                                                      ! - extend climatological year to the previous period -
      If (isn == 0) isn = nss
      lcsn = date_diff(yfield(isn)%tssn%tprd(1)%sdate, yfield(isn)%tssn%tprd(1)%edate, isq_mn) + 1
      If (lcsn <= nmn) Then ! - there is no persistence component with seasonal data -
         ls2 = lcsn
      Else
         ls2 = lcsn + 1 - nmn
      End If
      d2 = climate_per%d2
    Case (1:)
      If (nse > 1) Then
         l = 0
         Do ll = 1, nse
            l = l + 1
            If (same_ssn(climate_per%d2, yfield(ll)%tssn%tprd(1)%sdate)) Exit
         End Do
         If (l == 0) l = nse
         ls2 = date_diff(yfield(1)%tssn%tprd(1)%sdate, yfield(l)%tssn%tprd(1)%sdate, isq_dy) + &
               date_diff(yfield(1)%tssn%tprd(1)%sdate, yfield(1)%tssn%tprd(1)%edate, isq_dy) + 1
         d2 = yfield(1)%tssn%tprd(1)%sdate
         d2%iyr = climate_per%d2%iyr - (yfield(l)%tssn%tprd(1)%sdate%iyr - yfield(1)%tssn%tprd(1)%sdate%iyr)
      Else
         d2 = climate_per%d2
         ls2 = iseq
      End If
   End Select
!
! Construct climatological period labels
   climate_per%clim1 = get_cdate(climate_per%d1, ls1, io_com)
   climate_per%clim2 = get_cdate(d2, ls2, io_com)
   If (lseq) Then
      Call reset_iseq ()
      If (nse > 0) lslim = ls_bk
   End If
!
   Return
 End Subroutine label_climate
!
!
!
 Subroutine calc_climates (lstd, m, lb, ns, y, ncu, clim, ybar, ystd, ifail)
!
! Calculates mean and variance for a climate period
!
! Modules
   Use data_numbers,       Only: zero, one
   Use data_cpt_constants, Only: is_non, it_gam
   Use progress_meter,     Only: update_progress_meter
   Use settings,           Only: icv2, igauss, iretro, istd, nu, nur
   Use arrays,             Only: insertion_sort
   Use distribs,           Only: alpha, beta, pcen, &
                                 stdize, thom_gamma, ustdize
   Use regression,         Only: ycv, ycv2, yret, pls_2, pls_r
   Use season,             Only: isnc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m  ! - number of variables -
   Integer, Intent(In) :: lb ! - lower bound -
   Integer, Intent(In) :: ns ! - number of seasons -
!
   Logical, Intent(In) :: lstd ! - restandardise climatology? -
!
! - input arrays -
   Integer, Dimension(lb:), Intent(In) :: ncu ! - number of used climatological cases -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:,lb:), Intent(InOut) :: y ! - data -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,lb:), Intent(Out) :: ybar ! - climatological mean -
   Real(Kind=rp), Dimension(:,lb:), Intent(Out) :: ystd ! - climatological standard deviation -
!
   Real(Kind=rp), Dimension(:,:,lb:), Intent(Out) :: clim ! - climatological (sorted) data -
!
! Locals
!
! Local scalars
   Integer :: i   ! - variable index -
   Integer :: isn ! - season index -
!
   Logical :: lr2l !- restandardise double cross-validated limits? -
   Logical :: lrrl !- restandardise retroactive limits? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Size, Sqrt, Sum
!
! Executable Statements
!
! Unstandardise, if requested
   ifail = -1
   lr2l = (lstd .and. istd /= is_non .and. icv2 /= 0)
   lrrl = (lstd .and. istd /= is_non .and. iretro == 1)
! - prediciton limits -
   If (lr2l) Then
      Call ustdize (istd, igauss, isnc, m, nu, ybar(:,isnc), ystd(:,isnc), pls_2(:,:)%lower)
      Call ustdize (istd, igauss, isnc, m, nu, ybar(:,isnc), ystd(:,isnc), pls_2(:,:)%upper)
   End If
   If (lrrl) Then
      Call ustdize (istd, igauss, isnc, m, nur, ybar(:,isnc), ystd(:,isnc), pls_r(:,:)%lower)
      Call ustdize (istd, igauss, isnc, m, nur, ybar(:,isnc), ystd(:,isnc), pls_r(:,:)%upper)
   End If
! - observations and hindcasts -
   Do isn = lb, ns
      If (lstd .and. istd /= is_non) Then
         Call ustdize (istd, igauss, isn, m, nu, ybar(:,isn), ystd(:,isn), y(:,:,isn))
         Call ustdize (istd, igauss, isn, m,  nu, ybar(:,isn), ystd(:,isn), ycv(:,:,isn))
         If (icv2 /= 0) Call ustdize (istd, igauss, isn, m,  nu, ybar(:,isn), ystd(:,isn), ycv2(:,:,isn))
         If (iretro == 1) Call ustdize (istd, igauss, isn, m, nur, ybar(:,isn), ystd(:,isn), yret(:,:,isn))
      End If
!
! Extract climatological data
      Do i = 1, m
         clim(i,1:ncu(isn),isn) = y(i,iclim1:iclim2,isn)
!
! Calculate climatological moments
! - means -
         ybar(i,isn) = Sum(clim(i,1:ncu(isn),isn))/Real(ncu(isn), Kind=rp)
! - standard deviations -
         If (nc > 1) Then
            ystd(i,isn) = Sum((clim(i,1:ncu(isn),isn) - ybar(i,isn))**2)/Real(ncu(isn)-1, Kind=rp)
            If (ystd(i,isn) > zero) Then
               ystd(i,isn) = Sqrt(ystd(i,isn))
            Else
               ystd(i,isn) = one
            End If
         Else
            ystd(i,isn) = one
         End If
!
! Create climatology
! - sort data -
         Call insertion_sort ('a', ncu(isn), clim(i,1:ncu(isn),isn))
! - calculate gamma parameters -
         If (igauss == it_gam) Call thom_gamma (ncu(isn), clim(i,1:ncu(isn),isn), alpha(i,isn), beta(i,isn), pcen(i,isn))
!
! Update progress meter
         If (update_progress_meter(.false., ifail=ifail) /= 0) Return
      End Do
! - fill unused memory -
      If (ncu(isn) < Size(clim(:,:,:), Dim=2)) clim(:,ncu(isn)+1:,isn) = zero
!
! Restandardise, if requested
      If (lstd .and. istd /= is_non) Then
         Call stdize (istd, igauss, isn, m,       nu, ybar(:,isn), ystd(:,isn), y(:,:,isn))
         Call stdize (istd, igauss, isn, m, ncu(isn), ybar(:,isn), ystd(:,isn), clim(:,1:ncu(isn),isn))
         Call stdize (istd, igauss, isn, m,  nu, ybar(:,isn), ystd(:,isn), ycv(:,:,isn))
         If (icv2 /= 0) Call stdize (istd, igauss, isn, m,  nu, ybar(:,isn), ystd(:,isn), ycv2(:,:,isn))
         If (iretro == 1) Call stdize (istd, igauss, isn, m, nur, ybar(:,isn), ystd(:,isn), yret(:,:,isn))
      End If
!
! Update progress meter
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   End Do
!
! Restandardise prediciton limits, if requested
   If (lr2l) Then
      Call stdize (istd, igauss, isnc, m, nu, ybar(:,isnc), ystd(:,isnc), pls_2(:,:)%lower)
      Call stdize (istd, igauss, isnc, m, nu, ybar(:,isnc), ystd(:,isnc), pls_2(:,:)%upper)
   End If
   If (lrrl) Then
      Call stdize (istd, igauss, isnc, m, nur, ybar(:,isnc), ystd(:,isnc), pls_r(:,:)%lower)
      Call stdize (istd, igauss, isnc, m, nur, ybar(:,isnc), ystd(:,isnc), pls_r(:,:)%upper)
   End If
   ifail = 0
!
   Return
 End Subroutine calc_climates
!
!
!
 Subroutine print_climatology (lprmpt, &
            iout)
!
! Prints climatology
!
! Modules
   Use labels, Only: l_climatol
   Use gui,    Only: print_text
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In), Optional :: lprmpt ! - print prompt? -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: iout ! - output unit number -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Print climatology
   If (Present(iout)) Then
      If (lprmpt) Then
         Call print_text (Trim(l_climatol%c)//':', &
              iout=iout)
         Call print_text ('    '//Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2), &
              iout=iout)
      Else
         Call print_text (Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2), &
              iout=iout)
      End If
   Else
      If (lprmpt) Then
         Call print_text (Trim(l_climatol%c)//':', &
              lcolour=.true.)
         Call print_text ('    '//Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2))
      Else
         Call print_text (Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2))
      End If
   End If
!
   Return
 End Subroutine print_climatology
!
!
!
 Subroutine proj_read_climate (iin, lb, nt, ns, lsn0, lres, ifail)
!
! Reads climatology settings from project file
!
! Modules
   Use time,    Only: Operator(+)
   Use iofiles, Only: yfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin  ! - input file unit number -
   Integer, Intent(In) :: lb   ! - lower bound -
   Integer, Intent(In) :: lsn0 ! - length of persistence season -
   Integer, Intent(In) :: ns   ! - number of seasons -
   Integer, Intent(In) :: nt   ! - number of cases in training period -
!
   Logical, Intent(In) :: lres  ! - read results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read climatology settings
   If (.not.lres) Then
      Read (Unit=iin, Err=1, End=2) climate_per
      Read (Unit=iin, Err=1, End=2) ieclim
!
! Check whether climatological period extends beyond training period
      If (yfile%cfile%lset) Then
         icex = get_icex(yfile%fdate, yfile%fdate+(nt-1), &
                lsn0=lsn0)
      Else
         icex = 0
      End If
!
! Read results
   Else
      Read (Unit=iin, Err=1, End=2) nc
      Call init_climate (lb, ns, ifail)
      If (ifail /= 0) GoTo 3
      Read (Unit=iin, Err=1, End=2) ave(:,:)
      Read (Unit=iin, Err=1, End=2) clim(:,:,:)
      Read (Unit=iin, Err=1, End=2) sdev(:,:)
      Read (Unit=iin, Err=1, End=2) ncu(:)
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
! - memory allocation error -
3  ifail = 3
   Return
!
 End Subroutine proj_read_climate
!
!
!
 Subroutine proj_write_climate (iout, lres, ifail)
!
! Writes climatology settings to project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
!
   Logical, Intent(In) :: lres ! - write results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write climatology settings
   If (.not.lres) Then
      Write (Unit=iout, Err=1) climate_per
      Write (Unit=iout, Err=1) ieclim
!
! Write results
   Else
      Write (Unit=iout, Err=1) nc
      Write (Unit=iout, Err=1) ave(:,:)
      Write (Unit=iout, Err=1) clim(:,:,:)
      Write (Unit=iout, Err=1) sdev(:,:)
      Write (Unit=iout, Err=1) ncu(:)
   End If
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_climate
!
!
!
 Subroutine parameter_write_climate (iout, icsv, ifail)
!
! Writes program settings to parameter file
!
! Modules
   Use labels,         Only: cg_year1, cg_yearn
   Use time,           Only: iseq
   Use menus,          Only: mn_opts_climper
   Use parameter_file, Only: output_parameter
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output-file unit number -
   Integer, Intent(In) :: icsv ! - calculate seasonal values -
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
! Write climatological period settings
   If (mn_opts_climper%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_climper%l_lab%c), i=mn_opts_climper%id) /= 0) GoTo 1
! - climatological period dates -
      Select Case (iseq)
       Case Default
         If (output_parameter(iout, &
                              cdesc=Trim(cg_year1), i=climate_per%d1%iyr) /= 0) GoTo 1
         If (output_parameter(iout, &
                              cdesc=Trim(cg_yearn), i=climate_per%d2%iyr) /= 0) GoTo 1
       Case (1:)
         If (output_parameter(iout, &
                              i=climate_per%d1%idy) /= 0) GoTo 1
         If (output_parameter(iout, &
                              i=climate_per%d1%imn) /= 0) GoTo 1
         If (output_parameter(iout, &
                              i=climate_per%d1%iyr) /= 0) GoTo 1
         If (output_parameter(iout, &
                              i=climate_per%d2%idy) /= 0) GoTo 1
         If (output_parameter(iout, &
                              i=climate_per%d2%imn) /= 0) GoTo 1
         If (output_parameter(iout, &
                              i=climate_per%d2%iyr) /= 0) GoTo 1
      End Select
! - climatological period can extend beyond training period? -
      If (ieclim == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If
      If (output_parameter(iout, &
                           cdesc='Climatological period can extend beyond training period?', c=cyn) /= 0) GoTo 1
! - seasonal aggregation -
      If (icsv > 0) Then
         If (output_parameter(iout, &
                              cdesc='Seasonal aggregations', i=icsv) /= 0) GoTo 1
      End If
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
 End Subroutine parameter_write_climate
!
!
!
 Function close_climate() &
          Result (fclose)
!
! Frees memory allocated for climate
!
! Modules
   Use distribs, Only: close_distribs
!
! Function result
   Integer :: fclose
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Associated
!
! Executable Statements
!
! Free memory allocated for climate
   fclose = close_distribs ()
   If (Allocated(sdev))  Deallocate (sdev)
   If (Allocated(ave))   Deallocate (ave)
   If (Allocated(ncu))   Deallocate (ncu)
   If (Associated(clim)) Then
      Deallocate (clim)
      NullIfY (clim)
   End If
   fclose = 0
!
   Return
 End Function close_climate
End Module climate
