! Author: Simon Mason
Module menu_edit
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: edit_startat, edit_targetssn
!
Contains
!
!
 Function edit_startat() &
          Result (edit)
!
! Prompts for start-at date
!
! Modules
   Use data_cpt_constants,  Only: c_file
   Use data_io_constants,   Only: io_com, lprd
   Use data_time_constants, Only: isq_mn
   Use time,                Only: pdate, &
                                  Operator(-), Operator(+), &
                                  iseq, &
                                  date_diff, get_cdate, reset_iseq
   Use screen,              Only: window_update
#if GUI == 0
   Use menus,               Only: lmnact
#endif
   Use iofiles,             Only: mfile, xfile, yfile, zfile
   Use season,              Only: icsv, lb, lensn, lsn, lprst, nsn, &
                                  len_ssn
   Use climate,             Only: ncy
   Use forecast,            Only: change_fcast
   Use cpt_update,          Only: reset_limits, reset_limits_nf
   Use cpt_file,            Only: ixyz
!
! Function result
   Integer :: edit
!
! Locals
!
! Local scalars
   Integer :: ilim1 ! - first limit adjustment -
   Integer :: ilim2 ! - second limit adjustment -
   Integer :: ifail ! - error indicator -
!
   Character(Len=lprd) :: czprd1 ! - first possible Z period -
   Character(Len=lprd) :: czprdn ! - last possible Z period -
!
   Type(pdate) :: date1 ! - first possible Y date -
   Type(pdate) :: date2 ! - last possible Y date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Min
!
! Exeutable Statements
!
! Prompt for start-at date
   edit = 1
#if GUI == 0
   lmnact = .false.
#endif
   Select Case (c_file(ixyz))
    Case ('X', 'x')
      Call get_startat ('X', 1, [len_ssn(xfile%prd1%sdate, xfile%prd1%edate, xfile%iseq)], xfile, ifail)
    Case ('Y', 'y')
      Call get_startat ('Y', lb, lsn(lb:), yfile, ifail, &
           cprd1=mfile%cprd1, cprdn=mfile%cprdn)
      If (ifail == 0) Then
         mfile%bdate = yfile%bdate
         mfile%fdate = yfile%fdate
      End If
    Case ('Z', 'z')
      If (.not.lprst) Then
         ilim1 = 0
         ilim2 = 0
         czprd1 = zfile%cprd1
         czprdn = zfile%cprdn
      Else ! - restrict limits based on availability of persistence data -
         Call reset_iseq (isq_new=isq_mn)
         date1 = zfile%prd1%sdate + date_diff(xfile%bdate, yfile%bdate, isq_mn)
         date2 = zfile%prdn%sdate + (date_diff(xfile%bdate, yfile%bdate, isq_mn) + lsn(0) - 1)
         Call reset_iseq ()
         ilim1 = Max(0, date_diff(date1, yfile%bdate, iseq))
         ilim2 = Min(0, date_diff(date2, mfile%prdn%sdate, iseq))
         czprd1 = get_cdate(zfile%prd1+ilim1, 1, io_com)
         czprdn = get_cdate(zfile%prdn+ilim2, 1, io_com)
      End If
      Call get_startat ('Forecast', 1, [len_ssn(zfile%prd1%sdate, zfile%prd1%edate, zfile%iseq)], zfile, ifail, &
           cprd1=czprd1, cprdn=czprdn, ilima1=ilim1, ilima2=ilim2, itp=yfile%idstr, cbf=change_fcast)
    Case ('1')
      Call get_startat ('First', 1, [len_ssn(xfile%prd1%sdate, xfile%prd1%edate, xfile%iseq)], xfile, ifail)
    Case ('2')
      Call get_startat (' Second', 1, [len_ssn(yfile%prd1%sdate, yfile%prd1%edate, yfile%iseq)], yfile, ifail)
    Case ('3')
      Call get_startat ('Update', 1, [len_ssn(zfile%prd1%sdate, zfile%prd1%edate, zfile%iseq)], zfile, ifail)
   End Select
!
! Reset training period limits
   Select Case (c_file(ixyz))
    Case ('X', 'x', 'Y', 'y')
      Call reset_limits (icsv, lprst, .false., lensn, lsn(lb:), lb, nsn)
    Case ('Z', 'z')
      Call reset_limits_nf (icsv, lprst, .false., lsn(lb:), lb)
   End Select
!
! Reset climatolgoical period
   ncy = 0
   Call window_update (ncy)
!
   Return
!
 Contains
!
!
  Subroutine get_startat (cxyz, lb, lsn, afile, ifail, &
             cprd1, cprdn, ilima1, ilima2, itp, cbf)
!
! Modules
   Use data_io_constants, Only: lprd
   Use labels,            Only: cg_data1, cg_datan, &
                                l_startat
   Use time,              Only: Operator(+), &
                                iseq, &
                                set_bdate
   Use screen,            Only: window_update
   Use gui,               Only: iw, &
                                init_win, print_cvalue, prompt_pdate, win_prompt
   Use settings,          Only: set_options
   Use arrays,            Only: cfprds
   Use iofiles,           Only: ifile
   Use prediction,        Only: update_fprds
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lb ! - lower-bound -
!
   Character(Len=*), Intent(In) :: cxyz ! - file type -
!
! - input arrays -
   Integer, Dimension(lb:), Intent(In) :: lsn ! - length of season -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ilima1 ! - first limit adjustment -
   Integer, Intent(In), Optional :: ilima2 ! - second limit adjustment -
   Integer, Intent(In), Optional :: itp    ! - target period indicator -
!
   Character(Len=*), Intent(In), Optional :: cprd1 ! - first period of data -
   Character(Len=*), Intent(In), Optional :: cprdn ! - last period of data -
!
! - optional procedure arguments -
   Integer, External, Optional :: cbf ! - call-back function -
!
! Locals
!
! Local scalars
   Integer :: itab ! - tab setting -
   Integer :: iwid ! - width -
   Integer :: ilim1 ! - first limit adjustment -
   Integer :: ilim2 ! - second limit adjustment -
!
   Logical :: ltp ! - add target period? -
!
   Character(Len=lprd) :: cp1 ! - first period of data -
   Character(Len=lprd) :: cpn ! - last period of data -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Max, Present, Trim
!
! Exeutable Statements
!
! Backup current date
   ifail = 1
   edit = set_options(i1=afile%fdate%iyr, i2=afile%fdate%imn, i3=afile%fdate%idy)
!
! Configure prompt
   Call init_win (title=Trim(l_startat%c)//' ('//cxyz//')')
   itab = 30
! - date limits -
   If (Present(cprd1)) Then
      cp1 = cprd1
   Else
      cp1 = afile%cprd1
   End If
   If (Present(cprdn)) Then
      cpn = cprdn
   Else
      cpn = afile%cprdn
   End If
   iwid = Max(Len_Trim(cp1), Len_Trim(cpn))
   Call print_cvalue (Trim(cg_data1), cp1, &
        iwid=iwid, itab=itab)
   Call print_cvalue (Trim(cg_datan), cpn, &
        iwid=iwid, itab=itab)
! - show target period? -
   If (Present (itp)) Then
      If (itp > 0) Then
         ltp = .true.
      Else
         ltp = .false.
      End If
   Else
      ltp = .false.
   End If
! - limit adjustment -
   If (Present (ilima1)) Then
      ilim1 = ilima1
   Else
      ilim1 = 0
   End If
   If (Present (ilima2)) Then
      ilim2 = ilima2
   Else
      ilim2 = 0
   End If
!
! Prompt for start-at date
   If (.not.ltp) Then
      Call prompt_pdate (Trim(l_startat%c), iseq, afile%fdate, &
           itab=itab, lbold=.true., lfocus=.true., dlimits=[afile%prd1%sdate+ilim1, afile%prdn%sdate+ilim2], &
           modify2=cbf)
   Else
      iw = update_fprds()
      Call prompt_pdate (Trim(l_startat%c), iseq, afile%fdate, &
           itab=itab, lbold=.true., lfocus=.false., dlimits=[afile%prd1%sdate+ilim1, afile%prdn%sdate+ilim2], &
           modify1=update_fprds, modify2=cbf)
      Call print_cvalue ('Target period', cfprds(1)%cssn(2))
   End If
! - prompt to confirm -
   iw = win_prompt(.true.)
!
! Restore old settings if cancelled
   edit = set_options(iw=iw, i1=afile%fdate%iyr, i2=afile%fdate%imn, i3=afile%fdate%idy)
!
! Reset beginning dates
   If (iw /= 1) Return
   Call set_bdate (lb, lsn(lb:), nsn, afile%prd1%sdate, afile%fdate, afile%bdate, &
        cprdb=afile%cprdb)
   Call window_update (afile%cprdb)
   ifail = 0
!
   Return
  End Subroutine get_startat
 End Function edit_startat
!
!
!
 Function edit_targetssn() &
          Result (edit)
!
! Prompts for target season
!
! Modules
   Use fields,     Only: yfield
   Use season,     Only: icsv, lb, lensn, lprst, lsn, nsn, &
                         prompt_season, set_seasons
   Use cpt_update, Only: reset_limits
   Use cpt_file,   Only: check_zfdate
!
! Function result
   Integer :: edit
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Exeutable Statements
!
! Identify season
   edit = 1
   nsn = 1
   Call prompt_season (lb, yfield, ifail)
   If (ifail /= 0) Return
!
! Set seasons
   Call set_seasons (nsn, yfield(:))
!
! Check Z start-at date for persistence data availability
   If (lprst) Call check_zfdate ()
!
! Set default length of training period and number of forecasts
   Call reset_limits (icsv, lprst, .false., lensn, lsn(lb:), lb, nsn)
!
   Return
 End Function edit_targetssn
End Module menu_edit
