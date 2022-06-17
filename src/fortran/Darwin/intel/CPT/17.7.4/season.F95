! Author: Simon Mason
Module season
!
! Modules
   Use data_numbers, Only: rp
   Use time,         Only: pprd
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: available_ssn, close_season, get_cssel, get_seasons, len_ssn, mon_to_ssn, proj_read_season, proj_write_season, &
             prompt_season, reset_season, restore_season, seasonal_zyp, set_icsv, set_seasons, set_ssnlimits, which_season
!
! Parameters
!
! Local parameters
   Integer, Parameter, Public :: lssel = 40 ! - length of current season selection -
!
! Arrays
!
! Integer arrays
   Integer, Dimension(:), Allocatable, Public :: lsn ! - length of season -
!
   Integer, Dimension(:), Allocatable, Private :: lsn_bk ! - backup length of season -
!
! Real arrays
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Private :: vs ! - seasonal aggregates -
!
! Logical arrays
   Logical, Dimension(:), Allocatable, Public :: lssr ! - seasons have shortened records? -
!
   Logical, Dimension(:), Allocatable, Private :: ldsn ! - distinct season? -
!
! Scalars
!
! Integer scalars
   Integer, Public :: icsv  ! - calculate seasonal values flag -
!                               (0 = no;
!                                1 = averages;
!                                2 = totals)
   Integer, Public :: isnc  ! - season component index -
!                               (0 = persisted season;
!                                1 = predicted season;
!                                2 = persisted + predicted season)
   Integer, Public :: ispi  ! - SPI period -
   Integer, Public :: lensn ! - total length of season -
   Integer, Public :: lb    ! - lower-bound of lsn -
   Integer, Public :: lu    ! - upper-bound of lsn -
   Integer, Public :: nsn   ! - number of seasons -
   Integer, Public :: nsnc  ! - number of predicted season components -
!
   Integer, Private :: icsv_bk  ! - backup calculate seasonal values flag -
   Integer, Private :: isnc_bk  ! - season index -
   Integer, Private :: ispi_bk  ! - backup SPI period -
   Integer, Private :: lensn_bk ! - backup total length of season -
   Integer, Private :: lb_bk    ! - backup lower-bound of lsn -
   Integer, Private :: lu_bk    ! - backup upper-bound of lsn -
   Integer, Private :: nsn_bk   ! - backup number of seasons -
   Integer, Private :: nsnc_bk  ! - backup number of season components -
   Integer, Private :: l0       ! - first month of persistence component -
   Integer, Private :: nfd      ! - total number of fields -
!
! Character scalars
   Character(Len=lssel), Public :: cssel
!
! Logical scalars
   Logical, Public :: lprst ! - include persistence component? -
!
   Logical, Private :: lprst_bk ! - backup include persistence component? -
!
! Derived-type scalars
   Type(pprd), Pointer, Private :: pperiod => Null()  ! - pointer period -
!
Contains
!
!
 Function len_ssn(sdate, edate, isq_in)
!
! Calculates season length
!
! Modules
   Use data_time_constants, Only: isq_mn
   Use time,                Only: pdate, &
                                  lslim, &
                                  date_diff
!
! Function result
   Integer :: len_ssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq_in ! - input sequence -
!
   Type(pdate), Intent(In) :: sdate ! - start date -
   Type(pdate), Intent(In) :: edate ! - end date -
!
! Locals
!
! Local scalars
   Integer :: isq ! - time sequencing -
!
   Logical :: lslim_bk ! - backup use season limits? -
!
! Executable Statements
!
! Return appropriate sequencing
   Select Case (isq_in)
    Case (:0)
      isq = isq_mn
    Case Default
      isq = isq_in
   End Select
! 
! Calculate season length
   lslim_bk = lslim
   lslim = .false.
   len_ssn = date_diff(sdate, edate, isq) + 1
   lslim = lslim_bk
!
   Return
 End Function len_ssn
!
!
!
 Subroutine reset_season (lbk, lresetlims)
!
! Resets season definitions
!
! Modules
   Use data_cpt_constants, Only: ncps
   Use time,               Only: Assignment(=), &
                                 ssnlims, ssnlims_bk
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lbk
   Logical, Intent(In) :: lresetlims
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Backup
   If (lbk) Then
      ssnlims_bk = ssnlims
      icsv_bk = icsv
      isnc_bk = isnc
      ispi_bk = ispi
      lb_bk = lb
      lu_bk = lu
      lensn_bk = lensn
      nsn_bk = nsn
      nsnc_bk = nsnc
      lprst_bk = lprst
      If (Allocated(lsn)) Then
         If (Allocated(lsn_bk)) Deallocate (lsn_bk)
         Allocate (lsn_bk(0:lu))
         lsn_bk(:) = lsn(:)
      End If
   End If
!
! Reset limits
   If (lresetlims) ssnlims = 0
!
! Reset season settings
   icsv = 0
   ispi = 0
   isnc = 1
   lb = 1
   lu = ncps
   lensn = 0
   nsn = 1
   nsnc = 1
   lprst = .false.
!
! Reset memory
   If (.not.lbk) Then
      If (Allocated(lsn_bk)) Deallocate (lsn_bk)
   End If
   If (Allocated(lsn)) Deallocate (lsn)
   Allocate (lsn(0:lu))
   lsn(0) = 0
   lsn(1:) = 1
!
   Return
 End Subroutine reset_season
!
!
!
 Subroutine restore_season ()
!
! Restores season definitions
!
! Modules
   Use time, Only: Assignment(=), &
                   ssnlims, ssnlims_bk
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Restore season definitions
   ssnlims = ssnlims_bk
   icsv = icsv_bk
   isnc = isnc_bk
   ispi = ispi_bk
   lb = lb_bk
   lu = lu_bk
   lensn = lensn_bk
   nsn = nsn_bk
   nsnc = nsnc_bk
   lprst = lprst_bk
   If (Allocated(lsn_bk)) Then
      If (Allocated(lsn)) Deallocate (lsn)
      Allocate (lsn(0:lu))
      lsn(:) = lsn_bk(:)
   End If
!
   Return
 End Subroutine restore_season
!
!
!
 Function which_season(ssn, nsn, ssns) &
          Result (issn)
!
! Identifies which season the current one is in a sequence
!
! Modules
   Use time, Only: pdate, &
                  same_ssn
!
! Function result
   Integer :: issn 
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nsn ! - number of seasons -
!
   Type(pdate), Intent(In) :: ssn ! - current season -
!
! - input arrays -
   Type(pdate), Dimension(:), Intent(In) :: ssns ! - seasons -
!
! Locals
!
! Local scalars
   Integer :: isn ! - season index -
!
! Exeutable Statements
!
! Compare seasons
   issn = 0
   Do isn = 1, nsn
      If (same_ssn(ssn, ssns(isn))) Then
         issn = isn
         Exit
      End If
   End Do
!
   Return
 End Function which_season
!
!
!
 Subroutine get_seasons (ianal, xfield, yfield, ifail)
!
! Identifies (PFV and model predictors) or prompts (otherwise) for target season
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Cancelled
!    ifail =  1 Problem allocating memory
!
! Modules
   Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_pfv, ncps
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so
   Use time,                Only: Operator(==), Operator(+), &
                                  iafter, iseq, &
                                  same_ssn
   Use errors,              Only: cpt_error
   Use settings,            Only: isynch
   Use iofiles,             Only: mfile, xfile, yfile
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis indicator -
!
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: xfield ! - X field settings -
   Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: lsn_loc ! - back-up length of season -
   Integer :: ns      ! - number of seasons -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size, Ubound
!
! Exeutable Statements
!
! Compare seasons
   If (ianal /= ia_pfv .and. isynch == 0 .and. xfile%cfile%lset) Then
      If (xfield(1)%mdate == 0) Then
         iafter = 1
      Else If (.not.same_ssn(xfield(1)%tssn%tprd(1), yfield(1)%tssn%tprd(1))) Then ! - force a lag if Y season is incompatible with X -
         If (xfile%iseq == mfile%iseq) Then ! - seasons are necessarily incompatible if the sequencing is identical -
            iafter = 1
         Else If (mfile%iseq == isq_mn .and. xfile%iseq < 0) Then ! - no incompatibility if Y-file is monthly -
            iafter = 0
         Else
            iafter = 0
         End If
      Else
         iafter = 0
      End If
   Else
      iafter = 0
   End If
!
! Count number of different seasons
   If (xfile%cfile%lset) Then
      Call count_seasons (xfield)
   Else
      Call count_seasons (yfield)
   End If
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr)
      If (xfield(1)%mdate == 0) nsn = 1
   End Select
!
! Allocate workspace
   If (yfile%cfile%lset) Then
      Select Case (ianal)
       Case Default
         ns = ncps
       Case (ia_pfv)
         ns = nsn
      End Select
      If (Ubound(lsn, Dim=1) /= ns) Then
         lsn_loc = lsn(1)
         Deallocate (lsn)
         Allocate (lsn(0:ns))
         lsn(1) = lsn_loc
      End If
   Else
      Select Case (iseq)
       Case Default
         ns = ncps
       Case (isq_sn)
         ns = Count(ldsn(1:nfd))
       Case (isq_so)
         ns = nsn
      End Select
      If (Ubound(lsn, Dim=1) /= ns) Then
         Deallocate (lsn)
         Allocate (lsn(0:ns))
      End If
!
! Set X seasons if Y file is unopened
      Call set_lsn (iseq, xfile%iseq, nfd, xfield(:)%tssn%tprd(1), &
           ldsn)
   End If
!
! Set seasons and start-at dates
   ns = Size(yfield)
   Select Case (icsv)
    Case (0) ! - seasons are fixed -
      ifail = set_fixed_seasons (ns, yfield(:)%tssn%tprd(1))
    Case Default ! - seasons are to be calculated from monthly data -
      ifail = set_unfixed_seasons (ns, xfield, yfield)
      Select Case (ifail)
       Case (0) ! - set seasons -
         Call set_seasons (nsn, yfield(:))
       Case (-1) ! - cancelled -
         Return
       Case Default ! - problem allocating memory -
         ifail = 1
         Call cpt_error ('get_seasons', .false., ifail)
      End Select
   End Select
!
   Return
!
 Contains
!
!
 Subroutine count_seasons (afield)
!
! Counts number of seasons
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so, isq_yr, nmn
   Use time,                Only: iseq, nss
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: afield ! - field settings -
!
! Locals
!
! Local scalars
   Integer :: isn ! - season/field index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Any, Size
!
! Exectuable Statements
!
! Count number of seasons
   nfd = Size(afield)
   If (Allocated(ldsn)) Deallocate (ldsn)
   Allocate (ldsn(nfd))
   ldsn(1) = .true.
   Select Case (iseq)
    Case (isq_yr, isq_sn)
      nsn = 1
      If (nfd > 1) Then
         Do isn = 2, nfd
            If (Any(afield(1:isn-1)%cssn%cssn(1) == afield(isn)%cssn%cssn(1))) Then
               ldsn(isn) = .false.
            Else
               ldsn(isn) = .true.
               nsn = nsn + 1
            End If
         End Do
      End If
      If (iseq == isq_sn) nss = nsn
    Case (isq_so)
      nsn = nmn
      nss = 1
   End Select
!
   Return
  End Subroutine count_seasons
!
!
!
  Subroutine set_lsn (isq, isqf, nfsn, tssn, &
             ldsn)
!
! Sets length of seasons for seasonal data
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so, nmn
   Use time,                Only: pprd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq  ! - time sequencing -
   Integer, Intent(In) :: isqf ! - time sequencing of file -
   Integer, Intent(In) :: nfsn ! - number of field seasons -
!
! - input arrays -
   Type(pprd), Dimension(:), Intent(In) :: tssn ! - seasons -
!
! Optional arguments
! - input arrays -
   Logical, Dimension(:), Intent(In), Optional :: ldsn ! - distinct season? -
!
! Locals
!
! Local scalars
   Integer :: isn ! - season/field index -
   Integer :: l   ! - month index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Exeutable Statements
!
! Set length of seasons for seasonal data
   Select Case (isq)
    Case (isq_sn)
      isn = 0
      Do l = 1, nfsn
         If (Present(ldsn)) Then
            If (.not.ldsn(l)) Cycle
         End If
         isn = isn + 1
         lsn(isn) = len_ssn(tssn(l)%sdate, tssn(l)%edate, isqf)
      End Do
    Case (isq_so)
      lsn(:) = len_ssn(tssn(1)%sdate, tssn(1)%edate, isqf)
      lensn = nmn + lsn(1) - 1
   End Select
!
   Return
  End Subroutine set_lsn
!
!
!
  Function set_fixed_seasons(nfsn, tssn) &
           Result (set_ssn)
!
! Sets seasons when they are not to be calculated
!
! Modules
   Use data_time_constants, Only: isq_mn
   Use time,                Only: pprd
   Use iofiles,             Only: mfile, xfile, yfile
!
! Function result
   Integer :: set_ssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfsn ! - number of field seasons -
!
! - input arrays -
   Type(pprd), Dimension(:), Intent(In) :: tssn ! - seasons -
!
! Executable Statements
!
! Set seasons when they are not to be calculated
! - beginning daate is the same as start date -
   xfile%bdate = xfile%fdate
! - switch off persistence -
   lprst = .false.
   lsn(0) = 0
   lb = 1
   isnc = 1
   nsnc = 1
! - set length of seasons for seasonal data -
   lensn = 0
   If (xfile%cfile%lset) Call set_lsn (mfile%iseq, isq_mn, nfsn, tssn(:))
! - disable persistence in Y-file if X-file is not yet open -
   If (.not.xfile%cfile%lset) yfile%bdate = yfile%fdate
! - label the seasons -
   Call label_seasons (1, lsn(1:))
   set_ssn = 0
!
   Return
  End Function set_fixed_seasons
!
!
!
  Function set_unfixed_seasons(nfsn, xfield, yfield) &
           Result (set_ssn)
!
! Sets seasons when they are to be calculated from monthly data
!
! Modules
   Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
   Use data_time_constants, Only: isq_mn, isq_yr
   Use time,                Only: Operator(==), &
                                  iseq
   Use iofiles,             Only: mfile
   Use fields,              Only: field, &
                                  mfield, nmfd, &
                                  init_field
!
! Function result
   Integer :: set_ssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfsn ! - number of field seasons -
!
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: xfield ! - X field settings -
   Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
!
! Executable Statements
!
! Backup Y fields as monthly fields
   Nullify (mfield)
   Select Case (mfile%iseq)
    Case (isq_yr, isq_mn)
      nmfd = nfsn
    Case (1:)
      nmfd = mfile%nse
    Case Default
      nmfd = nsn
   End Select
   Call init_field (nmfd, yfield(:)%rmiss, mfield, ifail)
   set_ssn = ifail
   If (ifail /= 0) Return
   Select Case (iseq)
    Case (isq_yr)
      mfield(:) = yfield(:)
    Case Default
      mfield(1) = yfield(1)
      mfield(2:) = mfield(1)
   End Select
!
! Identify season
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr, ia_gcm)
      If (xfield(1)%mdate == 0 .or. lb == 0) Then ! - prompt for season -
         nsn = 1
         Call prompt_season (1, yfield, ifail)
      Else ! - identify season if it is specified in the X-file -
         Call init_field (nsn, mfield(1:1)%rmiss, yfield, ifail) ! - re-initialise Y fields -
         If (ifail == 0) Then
            yfield(:) = mfield(1)
            Call identify_seasons (nsn, xfield(:)%tssn%tprd(1), yfield(:)%tssn%tprd(1))
         End If
      End If
    Case (ia_pfv)
      Call init_field (nsn, mfield(1:1)%rmiss, yfield, ifail) ! - re-initialise Y fields -
      If (ifail == 0) Then
         yfield(:) = mfield(1)
         Call identify_seasons (nsn, xfield(:)%tssn%tprd(1), yfield(:)%tssn%tprd(1))
      End If
   End Select
!
   set_ssn = ifail
!
   Return
  End Function set_unfixed_seasons
 End Subroutine get_seasons
!
!
!
 Subroutine prompt_season (lby, yfield, ifail)
!
! Prompts for season definitions
!
! Modules
   Use data_cpt_constants,  Only: is_sti, ncps
   Use data_time_constants, Only: isq_mn, nmn
   Use labels,              Only: cg_cselection, cg_lenspi, cg_pseason, l_tseason, &
                                  make_label
   Use time,                Only: pprd, &
                                  Operator(+), Operator(-), &
                                  adjust_pprd, get_cssn, reset_iseq
   Use gui,                 Only: iw, &
                                  box_close, box_open, init_win, print_cvalue, print_text, prompt_integer, win_prompt
#if GUI == 1
   Use gui,                 Only: print_advisory, prompt_boolean
#endif
   Use errors,              Only: cpt_error
#if GUI == 1
   Use settings,            Only: istd
#endif
   Use settings,            Only: set_options
   Use iofiles,             Only: mfile, xfile
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lby ! - lower bound -
!
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: iprst ! - use persistence component -
   Integer :: istdu ! - used standardization option -
!
   Character(Len=128) :: cse        ! - error seasons -
   Character(Len= 64) :: cl_tseason ! - target season -
!
! Local arrays
   Type(pprd), Dimension(0:1) :: essn ! - error season -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Backup season settings
   Call reset_iseq (isq_new=isq_mn)
   If (ispi == 0) ispi = 1
   ifail = set_options(i1=yfield(1)%tssn%tprd(1)%sdate%imn, i2=lsn(1), l1=lprst)
   If (lprst) Then
      iprst = 1
   Else
      iprst = 0
   End If
!
! Prompt for season
   cssel = get_cssel(0, yfield(1)%tssn%tprd(1)%sdate%imn, lsn(1), 1, 0)
   pperiod => yfield(1)%tssn%tprd(1)
   Call init_win (title=Trim(l_tseason%c))
   Call box_open (.false., .false., &
        title='Set season')
   Call prompt_integer ('First month of season to forecast', yfield(1)%tssn%tprd(1)%sdate%imn, &
        itab=60, ilow=1, ihgh=nmn, fcb1=update_ctsel, lfocus = .false.)
!
! Prompt for season
   Call prompt_integer ('Length of season to forecast', lsn(1), &
        itab=60, ilow=1, ihgh=nmn, fcb1=update_ctsel, lfocus = .false.)
!
! Prompt for persistence component
#if GUI == 1
   Call prompt_boolean ('Add persistence component?', iprst)
#endif
   Call print_text (' ')
   Call print_cvalue (Trim(cg_cselection), cssel, &
        iwid=lssel)
   Call print_text (' ')
   Call box_close (.false.)
   iw = win_prompt(.true.)
!
! Set option
   lprst = (iprst == 1)
   If (set_options(iw=iw, i1=yfield(1)%tssn%tprd(1)%sdate%imn, i2=lsn(1), l1=lprst) >= 3) Then
      ifail = -1
      GoTo 2
   End If
!
! Prompt for persistence component
1  Continue
#if GUI == 1
   If (lprst) Then
      If (lsn(1) < nmn) Then
         istdu = istd
#else
         istdu = is_sti
#endif
         l0 = yfield(1)%tssn%tprd(lby)%sdate%imn
         If (istdu /= is_sti) Then
            ispi = pperiod%sdate%imn - l0
            If (ispi < 0) ispi = ispi + nmn
            ispi = ispi + lsn(1)
         End If
         cssel = get_cssel(0, pperiod%sdate%imn, lsn(1), 1, ispi)
         ifail = set_options(i1=ispi, i2=l0)
#if GUI == 1
         Call init_win (title=Trim(l_tseason%c))
#endif
         Call box_open (.false., .false., &
              title='Persistence component')
         Select Case (istdu)
          Case Default
            Call prompt_integer ('First month of persistence component', l0, &
                 ilow=1, ihgh=nmn, fcb1=update_cl0, lfocus = .false.)
          Case (is_sti) ! - SPI standardization -
            Call prompt_integer (Trim(cg_lenspi), ispi, &
                 ilow=lsn(1), ihgh=nmn, fcb1=update_cspi, lfocus = .false.)
         End Select
         Call print_text (' ')
         Call print_cvalue (Trim(cg_cselection), cssel, &
              iwid=lssel)
         Call box_close (.false.)
         iw = win_prompt(.true.)
         If (istdu /= is_sti) Then
            ispi = pperiod%sdate%imn - l0
            If (ispi < 0) ispi = ispi + nmn
            ispi = ispi + lsn(1)
         End If
         If (set_options(iw=iw, i1=ispi, i2=l0) >= 3) ispi = lsn(1)
#if GUI == 1
      Else
         Call print_advisory ('It is not possible to add a persistence component when the length of the season is one year')
         ispi = lsn(1)
      End If
   Else
      ispi = lsn(1)
   End If
#endif
! - check for invalid persistence period -
   If (ispi > nmn) Then
      ifail = 1
      essn(1)%sdate = yfield(1)%tssn%tprd(1)%sdate
      essn(1)%edate = essn(1)%sdate + (lsn(1) - 1)
      essn(0)%sdate = essn(1)%edate - (ispi - 1)
      essn(0)%edate = essn(1)%sdate - 1
      lsn(0) = ispi - lsn(1)
      cl_tseason = make_label('S', l_tseason)
      cse = Trim(cg_pseason)//': '//Trim(get_cssn(essn(0), 1, [1]))//'; '//&
           &Trim(cl_tseason)//': '//Trim(get_cssn(essn(1), 1, [1]))
      Call cpt_error ('prompt_season', .false., ifail, &
           c_arg1=Trim(cse))
      ispi = lsn(1)
      GoTo 1
! - check for persistence component -
   Else If (ispi > lsn(1)) Then
      lsn(0) = ispi - lsn(1)
      lprst = .true.
      lb = 0
      isnc = ncps
      nsnc = ncps
! - no persistence
   Else
      lsn(0) = 0
      lprst = .false.
      lb = 1
      isnc = 1
      nsnc = 1
   End If
   lu = ncps
   lsn(2) = lsn(0) + lsn(1)
!
! Adjust start year if month is unavailable
   yfield(1)%tssn%tprd(1)%edate = yfield(1)%tssn%tprd(1)%sdate + (lsn(1) - 1)
   Call adjust_pprd (xfile%iseq, .true., mfile%prd1%sdate, yfield(1)%tssn%tprd(1))
!
   ifail = 0
!
! Restore time sequencing
2  Call reset_iseq ()
!
   Return
 End Subroutine prompt_season
!
!
!
 Subroutine set_field_dates (lprst, afield)
!
! Sets field dates and date-labels
!
! Modules
   Use data_cpt_constants,  Only: ncps
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so
   Use time,                Only: Assignment(=), Operator(-), &
                                  iseq, &
                                  reset_iseq
   Use iofiles,             Only: xfile
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lprst ! - persistence component? -
!
! - input/output scalars -
   Type(field), Dimension(:), Intent(InOut) :: afield ! - Y field -
!
! Locals
!
! Local scalars
   Integer :: i    ! - persisted seasonal component index -
   Integer :: ilsn ! - season length -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
!
! Executable Statements
!
! Set field dates
   If (lprst) Then
! - persistence component -
      Call reset_iseq (isq_new=isq_mn)
      afield(1)%tssn%tprd(0)%sdate = afield(1)%tssn%tprd(1)%sdate - lsn(0)
      afield(1)%tssn%tprd(0)%edate = afield(1)%tssn%tprd(1)%sdate - 1
      Call reset_iseq ()
! - combined component -
      afield(1)%tssn%tprd(2)%sdate = afield(1)%tssn%tprd(0)%sdate
      afield(1)%tssn%tprd(2)%edate = afield(1)%tssn%tprd(1)%edate
! - multiple fields (must have the same dates if persistence component is on) - 
      If (Size(afield) > 1) Then
         Do i = 0, ncps
            afield(2:)%tssn%tprd(i) = afield(1)%tssn%tprd(i)
         End Do
      End If
   Else
      afield(:)%tssn%tprd(0) = 0
      If (Size(afield) > 1 .and. nsn == 1) afield(:)%tssn%tprd(1) = afield(1)%tssn%tprd(1)
      afield(:)%tssn%tprd(2) = afield(1)%tssn%tprd(1)
   Endif
!
! Set date-labels
   Select Case (iseq)
    Case Default
      ilsn = len_ssn(xfile%prd1%sdate, xfile%prd1%edate, xfile%iseq)
      Call label_seasons (0, [ilsn], &
           ilsn=lsn(1), tperiod=afield(1)%tssn%tprd(:), cssny=afield(1)%cssn%cssn(:))
    Case (isq_sn, isq_so)
      Call label_seasons (1, lsn(1:), &
           ilsn=1, tperiod=afield(:)%tssn%tprd(1), cssny=afield(:)%cssn%cssn(1))
           afield(:)%tssn%tprd(0) = 0
           afield(:)%tssn%tprd(2) = afield(:)%tssn%tprd(1)
           afield(:)%cssn%cssn(0) = ' '
           afield(:)%cssn%cssn(2) = afield(:)%cssn%cssn(1)
   End Select
!
   Return
 End Subroutine set_field_dates
!
!
!
 Subroutine set_ssnlimits (date1, nses, isq, ssnlims)
!
! Sets season limits (for sub-seasonal data)
!
! Modules
   Use data_time_constants, Only: isq_dy
   Use time,                Only: pdate, pprd, &
                                  Operator(+), &
                                  lslim, nse, &
                                  reset_iseq
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq  ! - time sequencing -
   Integer, Intent(In) :: nses ! - number of sub-seasons -
!
   Type(pdate), Intent(In) :: date1 ! - first date -
!
! - output scalars -
   Type(pprd), Intent(Out) :: ssnlims ! - season limits -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Exeutable Statements
!
! Set season limits
   Call reset_iseq (isq_new=isq_dy)
   ssnlims%sdate%iyr = 0
   ssnlims%sdate%imn = date1%imn
   ssnlims%sdate%idy = date1%idy
   Select Case (isq)
    Case Default
      ssnlims%edate = ssnlims%sdate + (nses*isq - 1)
    Case (3, 4)
      If (Mod(nses,2) == 0) Then
         ssnlims%edate = ssnlims%sdate + ((nses/2)*7 - 2)
      Else
         ssnlims%edate = ssnlims%sdate + ((nses/2)*7 + 2)
      End If
   End Select
   ssnlims%edate%iyr = 0
   lslim = .true.
   nse = nses
   Call reset_iseq ()
!
   Return
 End Subroutine set_ssnlimits
!
!
!
 Subroutine identify_seasons (nsn, xprd, yprd)
!
! Identifies season definitions from X field settings
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_so
   Use time,                Only: pprd, &
                                  Operator(+), Operator(>), &
                                  iseq, &
                                  adjust_pprd, reset_iseq
   Use iofiles,             Only: mfile, xfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nsn ! - number of distinct seasons -
!
! - input/output arrays -
   Type(pprd), Dimension(:), Intent(InOut) :: xprd ! - X field target periods -
   Type(pprd), Dimension(:), Intent(InOut) :: yprd ! - Y field target periods -
!
! Locals
!
! Local scalars
   Integer :: ifd    ! - field index -
   Integer :: isn    ! - season index -
   Integer :: isn2   ! - season index -
   Integer :: iyd    ! - year difference -
!
   Logical :: lso ! - seasonally-overlapping sequencing? -
!
! Executable Statements
!
! Identify season
   lso = (iseq == isq_so)
   Call reset_iseq (isq_new=isq_mn)
   isn = 0
   Do ifd = 1, nfd
      If (.not.ldsn(ifd)) Cycle
      isn = isn + 1
      lsn(isn) = len_ssn(xprd(ifd)%sdate, xprd(ifd)%edate, xfile%iseq)
      yprd(isn)%sdate%imn = xprd(isn)%sdate%imn
      yprd(isn)%edate = yprd(isn)%sdate + (lsn(isn) - 1)
      If (ifd == 1) Then
         If (mfile%prd1%sdate > xprd(ifd)%sdate) Then
            Call adjust_pprd (xfile%iseq, .true., mfile%prd1%sdate, yprd(isn))
         End If
      Else
         iyd = xprd(isn)%sdate%iyr - xprd(1)%sdate%iyr
         yprd(isn)%sdate%iyr = yprd(1)%sdate%iyr + iyd
         yprd(isn)%edate = yprd(isn)%sdate + (lsn(isn) - 1)
         If (iyd < 0) Then ! - adjust if season is earlier than first -
            Call adjust_pprd (xfile%iseq, .true., mfile%prd1%sdate, yprd(isn))
            iyd = xprd(1)%sdate%iyr - xprd(isn)%sdate%iyr
            Do isn2 = 1, isn-1
               yprd(isn2)%sdate%iyr = yprd(isn2)%sdate%iyr + iyd
               yprd(isn2)%edate = yprd(isn2)%sdate + (lsn(isn2) - 1)
            End Do
         End If
      End If
   End Do
!
! Set overlapping seasons
   If (lso) Then
      lsn(2:) = lsn(1)
      Do isn = 2, nsn
         yprd(isn) = yprd(isn-1) + 1
      End Do
   End If
   Call reset_iseq ()
!
! Switch off persistence component
   ispi = lsn(1)
   lsn(0) = 0
   lprst = .false.
!
   Return
 End Subroutine identify_seasons
!
!
!
 Subroutine set_seasons (nsn, afield)
!
! Sets season definitions
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
   Use time,                Only: Operator(==), Operator(<), Operator(>), Operator(-), Operator(+), &
                                  iafter, iseq, &
                                  add_months, adjust_pprd, date_diff, set_bdate, set_fdate
   Use iofiles,             Only: mfile, xfile, yfile
   Use fields,              Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nsn ! - number of seasons -
!
! - input/output scalars -
   Type(field), Dimension(:), Intent(InOut) :: afield ! - Y field -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
!
! Executable Statements
!
! Set Y-file season
   yfile%prd1%sdate = afield(1)%tssn%tprd(1)%sdate
   yfile%prd1%edate = afield(1)%tssn%tprd(1)%edate
   If (lprst) Then
      lensn = ispi
   Else
      If (nsn == 1) Then
         lensn = lsn(1)
      Else
         lensn = date_diff(afield(1)%tssn%tprd(1)%sdate, afield(nsn)%tssn%tprd(1)%edate, isq_mn) + 1
      End If
   End If
   If (nsnc == 1 .and. Size(afield) > 1) afield(2:)%tssn%tprd(1) = afield(1)%tssn%tprd(1) ! - apply to any additional fields -
!
! Determine last season available
   Select Case (iseq)
    Case Default
      yfile%prdn%edate%imn = yfile%prd1%edate%imn
      yfile%prdn%sdate = yfile%prdn%edate
      Call add_months (1-lensn, yfile%prdn%sdate)
      Call adjust_pprd (xfile%iseq, .false., mfile%prdn%edate, yfile%prdn)
    Case (isq_sn, isq_so)
      yfile%prdn = yfile%prd1
      Do
         If (yfile%prdn%edate < mfile%prdn%edate) Then
            yfile%prdn = yfile%prdn + 1
         Else
            Exit
         End If
      End Do
   End Select
!
! Set appropriate start date
   yfile%fdate%imn = yfile%prd1%sdate%imn
   yfile%bdate = yfile%fdate
   If (xfile%fdate%imn == 0 .and. iseq == isq_yr) Then
      xfile%fdate%imn = xfile%prd1%sdate%imn
      xfile%fdate%idy = xfile%prd1%sdate%idy
      xfile%bdate%imn = xfile%prd1%sdate%imn
      xfile%bdate%idy = xfile%prd1%sdate%idy
   End If
! - set appropriate lag if necessary -
   Call set_bdate (lb, lsn(lb:), nsn, yfile%prd1%sdate, yfile%fdate, yfile%bdate)
   Call set_fdate (iafter, xfile%iseq, xfile%fdate, mfile%prd1%sdate, yfile%bdate, yfile%fdate, ifail)
   If (ifail /= 0) Call set_fdate (-iafter, xfile%iseq, yfile%fdate, xfile%prd1%sdate, xfile%bdate, xfile%fdate, ifail)
   mfile%bdate = yfile%bdate
   mfile%fdate = yfile%fdate
!
! Set season labels
   Call set_field_dates ((lb == 0), afield)
!
   Return
 End Subroutine set_seasons
!
!
!
 Subroutine available_ssn (isq, nt)
!
! Identifies months / seasons with fewer cases than implied by number of years
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, nmn
   Use time,                Only: iseq, nss, &
                                  date_diff
   Use fields,              Only: yfield
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq ! - time sequencing -
   Integer, Intent(In) :: nt  ! - number of cases -
!
! Locals
!
! Local scalars
   Integer :: i1 ! - lower bound -
   Integer :: ns ! - number of seasons -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Mod
!
! Executable Statements
!
! Identify any seasons with fewer cases
! - identify number of seasons -
   Select Case (isq)
    Case (isq_mn)
      ns = Min(nmn, lensn)
      i1 = 1
    Case (isq_sn)
      ns = nss
      i1 = 1
    Case (isq_so)
      ns = nmn
      i1 = 1
    Case Default
      ns = 1
      i1 = lb
   End Select
   Allocate (lssr(i1:ns))
   lssr(:) = .false.
! - some seasons may have fewer years if there are multiple seasons -
   If (ns > 1) Then
      Select Case (iseq)
       Case (isq_sn)
         i1 = Mod(nt, nss)
         If (i1 == 0) i1 = nsn
         Select Case (isq)
          Case (isq_mn)
            i1 = date_diff(yfield(1)%tssn%tprd(1)%sdate, yfield(i1)%tssn%tprd(1)%edate, isq_mn) + 1
            i1 = Mod(i1, lensn)
         End Select
       Case (isq_so)
         Select Case (isq)
          Case (isq_mn)
            i1 = Mod(nt+lensn-nmn, nmn)
          Case (isq_so)
            i1 = Mod(nt, nmn)
         End Select
       Case (isq_mn)
         i1 = Mod(nt, nmn)
       Case Default
         i1 = 0
      End Select
      If (i1 > 0) lssr(i1+1:) = .true.
   End If
!
   Return
 End Subroutine available_ssn
!
!
!
 Subroutine label_seasons (lb, lsns, &
            ilsn, tperiod, cssny)
!
! Labels seasons
!
! Modules
   Use data_cpt_constants, Only: ncps
   Use data_io_constants,  Only: io_com
   Use time,               Only: Operator(+), &
                                 lslim, &
                                 date_diff, get_cdate, get_cprd, get_cssn
   Use screen,             Only: window_update
   Use iofiles,            Only: mfile, xfile, yfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lb ! - lower bound -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: lsns ! - lengths of X seasons -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ilsn  ! - season length -
!
! - optional input arrays -
   Type(pprd), Dimension(lb:), Intent(In), Optional :: tperiod ! - Y-field target periods -
!
! - optional output arrays -
   Character(Len=*), Dimension(lb:), Intent(Out), Optional :: cssny ! - Y-field seasons -
!
! Locals
!
! Local scalars
   Integer :: isn      ! - season index -
   Integer :: lse      ! - length of subseason -
   Logical :: lslim_bk ! - backup use season limits? -
!
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Label seasons
   If (nsn == 1) Then
      xfile%cssn = get_cssn(xfile%prd1, nsn, [1])
   Else
      xfile%cssn = get_cssn(xfile%prd1, 1, lsns(1:1))
      Do isn = 2, nsn
         xfile%cssn = Trim(xfile%cssn)//' '//get_cssn(xfile%prd1+(isn-1), 1, lsns(isn:isn))
      End Do
   End If
   If (xfile%cfile%lset) Then
      If (xfile%iseq > 0) Then
         lslim_bk = lslim
         lslim = .false.
         lse = date_diff(xfile%prd1%sdate, xfile%prd1%edate, 1) + 1
         lslim = lslim_bk
      Else
         lse = 0
      End If
      xfile%cprdb = get_cprd(xfile%bdate, xfile%prd1%sdate, xfile%iseq, lse,  1,  lsns(:nsn), nsn)
   End If
   If (yfile%cfile%lset) Then
      If (yfile%iseq > 0) Then
         lse = date_diff(yfile%prd1%sdate, yfile%prd1%edate, 1) + 1
      Else
         lse = 0
      End If
      yfile%cprdb = get_cprd(yfile%bdate, yfile%prd1%sdate, yfile%iseq, lse, lb, lsn(lb:nsn), nsn)
   End If
   Call window_update (xfile%cprdb)
   Call window_update (yfile%cprdb)
   If (Present(cssny)) Then
      yfile%cprd1 = get_cdate(yfile%prd1, ilsn, io_com)
      yfile%cprdn = get_cdate(yfile%prdn, ilsn, io_com)
      yfile%cssn = get_cssn(yfile%prd1, nsn, lsn(1:))
      mfile%cssn = get_cssn(mfile%prd1, 1, [1])
      Do isn = lb, ncps
         cssny(isn) = get_cssn(tperiod(isn), 1, lsn(isn:isn))
      End Do
   End If
!
   Return
 End Subroutine label_seasons
!
!
!
 Function get_cssel(imd0, imd1, ls, nld, ispi) &
          Result (cssel)
!
! Returns current season selection
!
! Modules
   Use data_text,           Only: ilang
   Use data_time_constants, Only: cma, cmon, nmn
   Use labels,              Only: cg_initin_l, cg_na, cg_persist, l_fcast
   Use time,                Only: add_to_month
!
! Function result
   Character(Len=lssel) :: cssel
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imd0 ! - month of forecast initialization -
   Integer, Intent(In) :: imd1 ! - first month of season -
   Integer, Intent(In) :: ls   ! - length of season -
   Integer, Intent(In) :: nld  ! - number of seasons -
   Integer, Intent(In) :: ispi ! - length of SPI -
!
! Locals
!
! Local scalars
   Integer :: l  ! - season index -
   Integer :: lp ! - length of persistence season -
   Integer :: m0 ! - zeroth month -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Current season(s)
   If (ls == 1) Then
      cssel = cmon(imd1, ilang)
   Else
      cssel = cma(ilang)(imd1:imd1+ls-1)
   End If
   Select Case (nld)
    Case (1)
      Continue
    Case (2:4)
      Do l = 2, nld
         If (ls == 1) Then
            If (imd1+l-1 <= nmn) Then
               cssel = Trim(cssel)//', '//cmon(imd1+l-1,ilang)
            Else
               cssel = Trim(cssel)//', '//cmon(imd1+l-(nmn+1),ilang)
            End If
         Else
            m0 = add_to_month(imd1, (l-1)*ls-1)
            cssel = Trim(cssel)//', '//cma(ilang)(m0+1:m0+ls)
         End If
      End Do
    Case Default
      If (ls == 1) Then
         If (imd1+nld-1 <= nmn) Then
            cssel = Trim(cssel)//' - '//cmon(imd1+nld-1,ilang)
         Else
            cssel = Trim(cssel)//' - '//cmon(imd1+nld-(nmn+1),ilang)
         End If
      Else
         m0 = add_to_month(imd1, (nld-1)*ls-1)
         cssel = Trim(cssel)//' - '//cma(ilang)(m0+1:m0+ls)
      End If
   End Select
!
! Add start date
   If (imd0 > 0) cssel = Trim(cssel)//' '//Trim(cg_initin_l)//' '//cmon(imd0,ilang)
   If (ispi > 0) Then
      cssel = Trim(l_fcast%c)//': '//Trim(cssel)
      lp = ispi - ls
      m0 = add_to_month(imd1, -(lp+1))
      If (m0 ==12) m0 = 0
      Select Case (lp)
       Case (:0)
         cssel = Trim(cg_persist)//': '//cg_na//'; '//Trim(cssel)
       Case (1)
         cssel = Trim(cg_persist)//': '//cmon(m0+1,ilang)//'; '//Trim(cssel)
       Case Default
         cssel = Trim(cg_persist)//': '//cma(ilang)(m0+1:m0+lp)//'; '//Trim(cssel)
      End Select
   End If
!
   Return
 End Function get_cssel
!
!
!
 Function update_ctsel() &
          Result (update)
!
! Updates target season selection indicator
!
! Modules
   Use screen, Only: window_update
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Update season selection indicator
   cssel = get_cssel(0, pperiod%sdate%imn, lsn(1), 1, 0)
   Call window_update (cssel)
   update = 2
!
   Return
 End Function update_ctsel
!
!
!
 Function update_cl0() &
          Result (update)
!
! Updates target season selection indicator given first month of persistence
!
! Modules
   Use data_time_constants, Only: nmn
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Update season selection indicator
   ispi = pperiod%sdate%imn - l0
   If (ispi < 0) ispi = ispi + nmn
   ispi = ispi + lsn(1)
   update = update_cspi()
!
   Return
 End Function update_cl0
!
!
!
 Function update_cspi() &
          Result (update)
!
! Updates target season selection indicator given SPI length
!
! Modules
   Use screen, Only: window_update
!
! Function result
   Integer :: update
!
! Executable Statements
!
! Update season selection indicator
   cssel = get_cssel(0, pperiod%sdate%imn, lsn(1), 1, ispi)
   Call window_update (cssel)
   update = 2
!
   Return
 End Function update_cspi
!
!
!
 Subroutine set_icsv (units, icsv)
!
! Determines whether to calculate seasonal averages or seasonal aggregations
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: units ! - field units -
!
! - input/output scalars -
   Integer, Intent(InOut) :: icsv ! - calculate seasonal values flag -
!
! Executable Statements
!
! Determine appropriate means of seasonal aggregation
   Select Case (Trim(units))
    Case ('K', 'Kelvin_scale', 'C', 'degree_Celsius', 'Celsius_scale', 'F', 'Farenheit') ! - temperatures -
      icsv = 1
    Case ('m/s', 'meters/second', 'm s-1', 'mm/day', 'cm/day') ! - rates -
      icsv = 1
    Case ('m', 'meters', 'cm', 'mm', 'mm/month') ! - depths and monthly rates -
      icsv = 2
    Case ('counts') ! - counts -
      icsv = 2
    Case Default
      If (icsv == 0) icsv = 1
   End Select
!
   Return
 End Subroutine set_icsv
!
!
!
 Subroutine mon_to_ssn (bdate, n, &
            iyrs)
!
! Converts monthly to seasonal data
!
! Modules
   Use data_numbers,        Only: zero
   Use data_time_constants, Only: isq_sn, isq_so, isq_yr
   Use time,                Only: pdate, &
                                  Operator(+), Operator(-), &
                                  iseq
   Use settings,            Only: ntm
   Use arrays,              Only: y, &
                                  accum_units
   Use iofiles,             Only: ny
   Use fields,              Only: yfield
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Type(pdate), Intent(In) :: bdate ! - first date of data -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: iyrs ! - years of data -
!
! Locals
!
! Local scalars
   Integer :: isn   ! - season index -
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Calculate seasonal aggregates
   Allocate (vs(ny,n,lb:nsn), Stat=ifail)
   If (ifail /= 0) GoTo 1
   If (lprst) Then
      Call calc_seasonal (icsv, lprst, ny, n, 1, lssr(0:), lsn(0:), 0, yfield(1)%tssn%tprd(0:)%sdate%imn, bdate, &
           yfield(:)%rmiss, y(:,:,:), vs(:,:,0:), &
           iyrs=iyrs)
   Else
      Call calc_seasonal (icsv, lprst, ny, n, nsnc, lssr(lb:), lsn(lb:), 1, yfield(:)%tssn%tprd(1)%sdate%imn, bdate, &
           yfield(:)%rmiss, y(:,:,:), vs(:,:,:), &
           iyrs=iyrs)
   End If
!
! Reallocate memory
   Deallocate (y)
   Allocate (y(ny,n,lb:nsnc), Stat=ifail)
   If (ifail /= 0) GoTo 1
   y(:,:,lb:nsn) = vs(:,:,lb:nsn)
   If (nsn < nsnc) y(:,:,nsn+1:) = zero ! - initialise unused portion -
   Deallocate (vs)
!
! Adjust units if monthly data are accumulated
   If (icsv == 2) Then
      Do isn = 1, nsn
         Call accum_units (lsn(isn), yfield(isn)%unit)
         Call accum_units (lsn(isn), yfield(isn)%unitc)
      End Do
   End If
!
! Reset numbers of cases once seasonal totals are calculated
   Select Case (iseq)
    Case (isq_yr)
      If (.not.Any(lssr(:))) ntm = n
    Case (isq_sn, isq_so)
      ntm = n
   End Select
!
   Return
!
1  ifail = 1
   Return
 End Subroutine mon_to_ssn
!
!
!
 Subroutine seasonal_zyp (imn1, rmiss)
!
! Converts monthly to seasonal data for persistence components of forecasts
!
! Modules
   Use data_time_constants, Only: isq_yr
   Use time,                Only: Operator(+), Operator(-), &
                                  iseq, &
                                  nyears
   Use settings,            Only: nmf, nf
   Use arrays,              Only: zyp
   Use iofiles,             Only: ny, pfile
!
! Arguments
!
! Dummy arguments
! - input arrays -
   Integer, Dimension(0:), Intent(In) :: imn1 ! - first month of season -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rmiss ! - missing values -
!
! Locals
!
! Local scalars
   Integer :: nyr ! - number of years -
!
! Executable Statements
!
! Calculate seasonal aggregates from training data
   nyr = nyears(nf, .false.)
   Call calc_seasonal (icsv, lprst, ny, nyr, 0, [.false.], lsn(0:), 0, imn1(0:), pfile%bdate, rmiss(:), zyp(:,:,1:), zyp(:,:,0:0))
   nf = nyr
   If (iseq == isq_yr) nmf = nyr
!
   Return
 End Subroutine seasonal_zyp
!
!
!
 Subroutine calc_seasonal (icsv, lcs, m, nyr, nsn, lssr, lsn, lb, imn1, d1, rmiss, vm, vs, &
            iyrs)
!
! Calculates seasonal averages/totals
!
! Modules
   Use data_numbers,        Only: zero
   Use data_time_constants, Only: nmn
   Use maths,               Only: approx
   Use time,                Only: pdate, &
                                  add_to_month, ndays
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icsv ! - seasonal aggregation method -
   Integer, Intent(In) :: m    ! - number of locations -
   Integer, Intent(In) :: nyr  ! - number of years -
   Integer, Intent(In) :: nsn  ! - number of seasons -
   Integer, Intent(In) :: lb   ! - lower bound -
!
   Logical, Intent(In) :: lcs ! - consecutive seasons? -
!
   Type(pdate), Intent(In) :: d1 ! - first date available -
!
! - input arrays -
   Integer, Dimension(lb:), Intent(In) :: lsn  ! - length of season -
   Integer, Dimension(lb:), Intent(In) :: imn1 ! - first month of season -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rmiss ! - missing values -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: vm ! - monthly values (dimensions: m,nyr,lensn) -
!
   Logical, Dimension(lb:), Intent(In) :: lssr ! - season has shortened record? -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,lb:), Intent(Out) :: vs ! - seasonal values (dimensions: m,nyr,nsn) -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: iyrs ! - years of data -
!
! Locals
!
! Local scalars
   Integer :: i   ! - location index -
   Integer :: k   ! - year index -
   Integer :: kk  ! - year index -
   Integer :: l   ! - month index -
   Integer :: ll  ! - month index -
   Integer :: ism ! - season index -
   Integer :: isn ! - season index -
   Integer :: is1 ! - index of first month of season -
   Integer :: isl ! - index of last month of season -
   Integer :: imn ! - current month -
   Integer :: iyr ! - current year -
   Integer :: lmn ! - length of month -
   Integer :: lsd ! - length of season in days -
   Integer :: nya ! - number of years available -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real
!
! Executable Statements
!
! Initialise
! - set multipliers for seasonal totals -
   If (icsv == 2) Then
      lmn = 1
      lsd = 1
   End If
! - initialise start and length of season -
   Do isn = lb, nsn
      If (lb == 1) Then
         ism = isn
      Else
         ism = 1
      End If
      vs(:,:,isn) = rmiss(ism)
      If (lcs .and. isn > lb) Then
         is1 = isl + 1
      Else
         is1 = add_to_month(imn1(isn), 1-d1%imn)
      End If
      isl = is1 + lsn(isn) - 1
!
! Calculate seasonal aggregate
      nya = nyr
      If (lssr(isn)) nya = nya - 1
      Do k = 1, nya
         If (.not.Present(iyrs)) Then
            iyr = d1%iyr + k - 1
         Else
            iyr = iyrs(k)
         End If
         imn = add_to_month(imn1(isn), -1, &
               iyr=iyr)
         If (icsv == 1) lsd = 0
         vs(:,k,isn) = zero
         Do l = is1, isl
            imn = add_to_month(imn, 1, &
                  iyr=iyr)
            If (icsv == 1) Then ! - seasonal average -
               lmn = ndays(iyr, imn)
               lsd = lsd + lmn
            End If
            Select Case (l)
             Case (1:nmn)
               ll = l
               kk = k
             Case (nmn+1:)
               ll = l - nmn
               kk = k + 1
            End Select
            Do i = 1, m
               If (approx(vm(i,kk,ll), rmiss(ism))) Then
                  vs(i,k,isn) = rmiss(ism)
               Else If (vs(i,k,isn) /= rmiss(ism)) Then
                  vs(i,k,isn) = vs(i,k,isn) + vm(i,kk,ll)*Real(lmn, Kind=rp)
               End If
            End Do
         End Do
         Where (vs(:,k,isn) /= rmiss(ism)) vs(:,k,isn) = vs(:,k,isn)/Real(lsd, Kind=rp)
      End Do
   End Do
!
   Return
 End Subroutine calc_seasonal
!
!
!
 Subroutine proj_read_season (iin, ianalp, pver, ifail)
!
! Reads program settings from project file
!
! Modules
   Use data_numbers,       Only: sp, tolv
   Use data_cpt_constants, Only: ia_pfv, ncps
   Use time,               Only: iseq, lslim, nse, nss, ssnlims
   Use iofiles,            Only: xfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: ianalp ! - project analysis -
!
   Real(Kind=sp), Intent(In) :: pver ! - project file version -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Clear existing settings
   Call close_season ()
!
! Read program settings
   If (pver > 17.02_sp-tolv) Then
      Read (Unit=iin, Err=1, End=2) icsv, ispi, lb, lensn, lprst, nsn, nss, nse, iseq, lslim, ssnlims
   Else If (pver > 16.0_sp) Then
      Read (Unit=iin, Err=1, End=2) icsv, ispi, lb, lensn, lprst, lensn, nsn, nss, nse, iseq, lslim, ssnlims
   Else
      Read (Unit=iin, Err=1, End=2) icsv, ispi, lb, lensn, lprst, lensn, nsn, nss, iseq
      nse = 0
      lslim = .false.
   End If
!
! Determine unsaved settings
   If (.not.lprst) Then
      Select Case (ianalp)
       Case Default
         nsnc = 1
         lu = ncps
       Case (ia_pfv)
         nsnc = nsn
         lu = nsn
      End Select
      isnc = 1
   Else
      nsnc = ncps
      isnc = ncps
      lu = ncps
   End If
!
! Read season settings if saved
   If (icsv > 0) Then
      nfd = xfile%nfs*nss
      Allocate (ldsn(nfd), Stat=ifail)
      If (ifail /= 0) GoTo 3
      Deallocate (lsn)
      Allocate (lsn(0:lu), Stat=ifail)
      If (ifail /= 0) GoTo 3
      Read (Unit=iin, Err=1, End=2) ldsn, lsn
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
! - problem initializing memory -
3  ifail = 3
   Return
!
 End Subroutine proj_read_season
!
!
!
 Subroutine proj_write_season (iout, ifail)
!
! Writes program settings to project file
!
! Modules
   Use time, Only: iseq, lslim, nse, nss, ssnlims
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write program settings
   Write (Unit=iout, Err=1) icsv, ispi, lb, lensn, lprst, nsn, nss, nse, iseq, lslim, ssnlims
!
! Write season settings if saved
   If (icsv > 0) Write (Unit=iout, Err=1) ldsn, lsn
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_season
!
!
!
 Subroutine close_season ()
!
! Frees memory allocated for calculating seasonal aggregates
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free memory allocated for calculating seasonal averages
   Call reset_season (.false., .false.)
   If (Allocated(ldsn)) Deallocate (ldsn)
   If (Allocated(lssr)) Deallocate (lssr)
!
   Return
 End Subroutine close_season
End Module season
