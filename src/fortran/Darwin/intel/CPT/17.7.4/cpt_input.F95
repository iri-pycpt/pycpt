! Author: Simon Mason
Module cpt_input
!
! Modules
   Use data_numbers,      Only: rp
   Use data_io_constants, Only: ltag
   Use time,              Only: pprd
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: get_data, init_read, num_read, proj_read_data, proj_write_data, read_data
!
! Scalars
!
! Integer scalars
   Integer :: ilat0  ! - last successfully read latitude -
   Integer :: irskip ! - number of records to skip -
   Integer :: nread  ! - number of cases in training period read successfully -
!
! Character scalars
   Character(Len=ltag), Private :: ctag0 ! - previous tag line -
   Character(Len=ltag), Private :: ctag1 ! - current tag line -
!
! Derived-type scalars
   Type(pprd) :: prd0 ! - last successfully read period -
!
Contains
!
!
 Function get_data(icsv, icex, ianal, lfail)
!
! Gets X and Y data and sets up default forecast file
!
! On exit returns:
!    0 if no errors
!   -1 if cancelled
!    1 if insufficient memory to read data
!    2 if first year of X training period is before first year in file
!    3 if first year of Y training period is before first year in file
!
! Modules
   Use data_numbers,        Only: one
   Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
   Use data_time_constants, Only: isq_mn, isq_so, nmn
   Use time,                Only: Operator(+), Operator(-), Operator(<), &
                                  iseq, nss, &
                                  date_diff, nyears
   Use progress_meter,      Only: lcalc, &
                                  set_progress_increment
   Use errors,              Only: cpt_error
#if GUI == 1
   Use errors,              Only: query
#endif
   Use settings,            Only: nt, ntm, &
                                  set_it1
   Use arrays,              Only: x, y
   Use space,               Only: idomx, idomy
   Use iofiles,             Only: mfile, nx, ny, tfile=>bkfile, xfile, yfile
   Use fields,              Only: xfield, yfield
   Use season,              Only: lensn
   Use missing,             Only: kavx, kavy
   Use climate,             Only: climate_per, nc
!
! Function result
   Integer :: get_data
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: icsv  ! - calculate seasonal values flag -
   Integer, Intent(In) :: icex  ! - climatological period lies outside the range of the training period flag -
   Integer, Intent(In) :: ianal ! - analysis identifier -
!
   Logical, Intent(In) :: lfail ! - return if error reading Y data? -
!
! Locals
!
! Local scalars
   Integer :: l     ! - current field -
   Integer :: il    ! - field offset -
   Integer :: nn    ! - number of time steps -
   Integer :: ifail ! - error indicator -
   Integer :: k1    ! - first date index -
   Integer :: m1    ! - number of grids / ensemble members -
   Integer :: n     ! - number of cases to read in Y file -
   Integer :: nact  ! - number of actions -
   Integer :: nexm  ! - number of extra months -
   Integer :: nexs  ! - number of extra seasons -
   Integer :: nyr   ! - number of years -
   Integer :: nyrr  ! - number of complete years to read -
!
   Character(Len=10) :: cproc ! - procedure -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Max, MaxVal, Min, Mod, Real, Sum
!
! Executable Statements
!
! Identify number of categories / ensemble members
   get_data = -1
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr, ia_pfv)
      m1 = xfile%ngs
    Case (ia_gcm)
      m1 = Max(1, xfile%nem)
    Case Default
      m1 = MaxVal([1, xfile%ngs, xfile%nem])
   End Select
!
! Identify appropriate Y-input file
   If (icsv == 0) Then
      tfile = yfile
   Else
      tfile = mfile
   End If
! - reassign seasonally overlapping sequencing of Y-file to avoid collating -
   If (tfile%iseq == isq_so) tfile%iseq = isq_mn
   tfile%nls = tfile%nls/nss
!
! Identify number of cases to read
   n = get_n(icex)
! - set earlier start date if climatological period starts before training period -
   Select Case (icex)
    Case (1) ! - read from the beginning of the climatological period to the end of the training period -
      tfile%bdate = climate_per%d1
    Case (3) ! - read the climatological period -
      tfile%bdate = climate_per%d1
    Case (-1) ! - read the climatological period and then the training period -
      tfile%bdate = climate_per%d1
   End Select
! - number of monthly -
   nyr = nyears(n, .false.)
   nexs = Mod(n, nss)
   If (nexs == 0) Then
      nyrr = nyr
      nexm = lensn
   Else ! - adjust if the last year is incomplete -
      nyrr = nyr - 1
      nexm = date_diff(yfield(1)%tssn%tprd(1)%sdate, yfield(nexs)%tssn%tprd(1)%edate, isq_mn) + 1
   End If
   ntm = calculate_ntm(icsv, iseq, nyrr, nexm, nexs)
!
! Check read settings
   xfile%it1 = set_it1(xfile%iseq, xfile%prd1%sdate, xfile%bdate)
   tfile%it1 = set_it1(tfile%iseq, tfile%prd1%sdate, tfile%bdate)
   yfile%it1 = set_it1(yfile%iseq, yfile%prd1%sdate, yfile%fdate)
   Call check_read (ianal, nn, cproc, ifail)
   Select Case (ifail)
    Case (0)
      nn = 1
    Case (1:2)
      GoTo 2
    Case Default
      If (lcalc) Then
#if GUI == 1
         If (1-query(cproc, -ifail, .true.) /= 0) Return
#endif
      Else ! - force cancellation for file merging -
         ifail = 1 - ifail
         GoTo 2
      End If
   End Select
   cproc = ' '
!
! Initialise progress increment for reading data
   nact = num_read(mfile%idstr, mfile%lstack, mfile%nt,  n, Sum(yfield(1:)%nlt), mfile%nfs, mfile%nls, mfile%ngs, mfile%it1) + &
          num_read(xfile%idstr, xfile%lstack, xfile%nt, nt, Sum(xfield(:)%nlt),  xfile%nfs, xfile%nls, xfile%ngs, xfile%it1)
   Call set_progress_increment (one/Real(nact+1, Kind=rp), .false.)
!
! Initialise memory
   If (icsv > 0) Then
      ifail = init_read(nx, ny, m1, nt, n, nyr, nss, &
              lns=Min(lensn, nmn))
   Else If (yfile%iseq == isq_so) Then
      ifail = init_read(nx, ny, m1, nt, n, nyr, nmn)
   Else
      ifail = init_read(nx, ny, m1, nt, n, nyr, nss)
   End If
   If (ifail /= 0) Then
      cproc = 'init_read'
      GoTo 2
   End If
!
! Initialise all data as missing values
   If (.not.lfail) Then
      il = 0
      Do l = 1, tfile%nfl
         y(il+1:il+yfield(l)%nva,:,:) = yfield(l)%rmiss
         il = il + yfield(l)%nva
      End Do
   End If
!
! Read Y data
   Select Case (icex)
    Case (-1) ! - read the climatological period and then the training period -
      n = nc
    Case (-2) ! - read the training period and then the climatological period -
      n = nt
   End Select
   nexs = Mod(n, nss)
   n = Ceiling(Real(n)/Real(nss))
   k1 = 1
1  If (icsv == 0) Then
      Call read_data (tfile, yfield(1:), ianal, n, nss, lensn, idomy(:,:), kavy(:,:), y(:,k1:,:), ifail)
   Else
      If (icex < 0) ntm = calculate_ntm(icsv, iseq, n, lensn, nexs)
      Call read_data (tfile, yfield( :), ianal, n,   1, lensn, idomy(:,:), kavy(:,:), y(:,k1:,:), ifail)
   End If
   If (ifail /= 0 .and. lfail) Return
! - re-read if there is a separate climatological period -
   If (k1 == 1 .and. icex < 0) Then
      k1 = n + 1
      Select Case (icex)
       Case (-1) ! - read the climatological period and then the training period -
         n = nt
         tfile%bdate = yfile%bdate
       Case (-2) ! - read the training period and then the climatological period -
         n = nc
         tfile%bdate = climate_per%d1
      End Select
      tfile%it1 = set_it1(tfile%iseq, tfile%prd1%sdate, tfile%bdate)
      n = Ceiling(Real(n)/Real(nss))
      GoTo 1
   End If
!
! Read X data
   If (.not.lfail) Then
      il = 0
      Do l = 1, xfile%nfl
         x(il+1:il+xfield(l)%nva,:,:) = xfield(l)%rmiss
         il = il + xfield(l)%nva
      End Do
   End If
   xfile%nls = xfile%nls/nss
   Call read_data (xfile, xfield(:), ianal, nt, 1, 1, idomx(:,:), kavx(:,:), x(:,:,:), ifail)
   xfile%nls = xfile%nls*nss
   If (ifail /= 0) Return
!
   get_data = 0
   Return
!
! Errors
2  get_data = ifail
   Call cpt_error (cproc, .false., ifail, &
        i_arg1=nn)
!
   Return
!
 Contains
!
!
  Subroutine check_read (ianal, nn, cproc, ifail)
!
! Checks reading settings
!
! On exit:
!    ifail =  0 if all checks passed
!    ifail =  1 if Insufficient X data
!    ifail =  2 if Insufficient Y data
!    ifail = -1 if negative lag
!    ifail = -2 if synchronous predictors and predictands with synchronous predictors switched off
!    ifail = -3 if large lag
!    ifail = -4 if inconsistent target period
!
! Modules
   Use data_cpt_constants, Only: ia_pfv
   Use time,               Only: check_lead
   Use settings,           Only: isynch, &
                                 check_nt
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis flag -
!
! - output scalars -
   Integer, Intent(Out) :: nn    ! - number of cases available -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure -
!
! Executable Statements
!
! Check that data for training period are available
   cproc = 'check_read'
   ifail = check_nt(nt, xfile%it1, xfile%nt)
   If (ifail /= 0) Then
      nn = ifail
      ifail = 1
      Return
   End If
   ifail = check_nt(ntm, tfile%it1, tfile%ntm)
   If (ifail /= 0) Then
      If (ntm /= nt) Then
         ntm = ifail
         nn = ifail/nmn
      Else
         nn = ifail
      End If
      ifail = 2
      Return
   Else
      mfile%it1 = tfile%it1
   End If
!
! Check for negative leads
   Select Case (ianal)
    Case Default
      Call check_lead (xfile%fdate, yfile%fdate, xfield(1)%mdate, xfile%it1, xfile%iseq, isynch, ifail)
    Case (ia_pfv)
      Call check_lead (xfile%fdate, yfile%fdate, xfield(1)%mdate, xfile%it1, xfile%iseq,      1, ifail)
   End Select
   ifail = -ifail
   nn = 0
!
   Return
  End Subroutine check_read
!
!
!
  Function calculate_ntm(icsv, iseq, n, lensn, nexs) &
            Result (ntm)
!
! Calculates number of monthly/seasonal cases
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_so
!
! Function result
   Integer :: ntm
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: icsv  ! - calculate seasonal values flag -
   Integer, Intent(In) :: iseq  ! - time sequencing -
   Integer, Intent(In) :: n     ! - number of time steps -
   Integer, Intent(In) :: lensn ! - total length of season -
   Integer, Intent(In) :: nexs  ! - number of extra seasons -
!
! Executable Statements
!
! Calculate number of monthly/seasonal cases
   If (icsv == 0 .or. iseq == isq_so) Then
      ntm = n
   Else If (iseq == isq_sn) Then
      ntm = (n - 1)*nss
      If (nexs > 0) Then
         ntm = ntm + nexs
      Else
         ntm = ntm + nss
      End If
   Else
      ntm = (n - 1)*nmn + lensn
   End If
!
   Return
  End Function calculate_ntm
 End Function get_data
!
!
!
 Function init_read(nx, ny, ngs, nt, n, nyr, nss, &
           lb, lns, ns1) &
           Result (init)
!
! Initialises memory and settings for reading data
!
! On exit, returns:
!    0 if no errors
!    1 if problem allocating memory
!
! Modules
   Use data_cpt_constants, Only: ir_bin
   Use settings,           Only: iregr
   Use arrays,             Only: cxprds, cyprds, rwk, t, x, xprds, y, yprds
   Use fields,             Only: xfield, yfield
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: nx  ! - number of X variables -
   Integer, Intent(In) :: ny  ! - number of Y variables -
   Integer, Intent(In) :: ngs ! - number of X categories -
   Integer, Intent(In) :: nt  ! - length of training period -
   Integer, Intent(In) :: n   ! - length of training period with climatology -
   Integer, Intent(In) :: nss ! - length of training period with climatology -
   Integer, Intent(In) :: nyr ! - number of years -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: lb  ! - lower bound -
   Integer, Intent(In), Optional :: lns ! - length of season -
   Integer, Intent(In), Optional :: ns1 ! - number of seasons -
!
! Locals
!
! Local scalars
   Integer :: mlgx  ! - maximum number of X longitudes -
   Integer :: mlgy  ! - maximum number of Y longitudes -
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal, Present
!
! Executable Statements
!
! Allocate data space
   init = 1
! - X data -
   Allocate (x(nx, nt, ngs), Stat=ifail)
   If (ifail /= 0) Return
! - Y data -
   If (ny > 0) Then
      If (Present(lb) .and. Present(ns1)) Then
         Allocate (y(ny, n, lb:ns1), Stat=ifail)
      Else If (Present(lns)) Then
         Allocate (y(ny, nyr, lns), Stat=ifail)
      Else
         Allocate (y(ny, nyr, nss), Stat=ifail)
      End If
      If (ifail /= 0) Return
      mlgy = MaxVal(yfield(:)%nlg)
   Else
      mlgy = 0
   End If
   mlgx = MaxVal(xfield(:)%nlg)
! - binomial coefficients -
   If (iregr == ir_bin) Then
      Allocate (t(n), Stat=ifail)
      If (ifail /= 0) Return
   End If
! - dates -
   Allocate (xprds(nt), Stat=ifail)
   If (ifail /= 0) Return
   Allocate (yprds(nt), Stat=ifail)
   If (ifail /= 0) Return
   Allocate (cxprds(nt), Stat=ifail)
   If (ifail /= 0) Return
   Allocate (cyprds(nt), Stat=ifail)
   If (ifail /= 0) Return
! - workspace -
   Allocate (rwk(Max(nt, Max(mlgx, mlgy))), Stat=ifail)
   If (ifail /= 0) Return
!
   init = 0
!
   Return
 End Function init_read
!
!
!
 Function num_read(idstr, lstack, n, nt, nlt, nfs, nls, ngs, it1)
!
! Estimates number of steps to read data for progress meter
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
!
! Function result
   Integer :: num_read
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: idstr ! - grid identifier -
   Integer, Intent(In) :: n     ! - total number of cases in file -
   Integer, Intent(In) :: nt    ! - number of cases in training period -
   Integer, Intent(In) :: nlt   ! - number of latitudes -
   Integer, Intent(In) :: nfs   ! - number of fields -
   Integer, Intent(In) :: nls   ! - number of lagged fields -
   Integer, Intent(In) :: ngs   ! - number of categories -
   Integer, Intent(In) :: it1   ! - index of first year of interest in file -
!
   Logical, Intent(In) :: lstack ! - stacked fields? -
!
! Locals
!
! Local scalars
   Integer :: nn ! - number of time steps -
!
! Executable Statements
!
! Calculate number of input steps
   nn = it1 - 1 + nt
   Select Case (idstr)
    Case (id_grd)
      num_read = 1 + nfs*nls*(it1 - 1)
      num_read = num_read + ngs*(nfs*nls + nlt)*nt
      If (lstack) num_read = num_read + (nfs - 1)*nls*(n - nn)
    Case (id_stn)
      num_read = nfs*it1 + ngs*nlt*nt + (nfs - 1)*(n - nt - it1 + 1)
    Case (id_ind)
      num_read = nfs*it1 + ngs*nlt*nt + (nfs - 1)*(n - nt - it1 + 1)
   End Select
!
   Return
 End Function num_read
!
!
!
 Function get_n(icex) &
          Result (n)
!
! Identifies number of cases to read
!
! Modules
   Use settings, Only: nt
   Use climate,  Only: ntc
!
! Function result
   Integer :: n
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: icex ! - climatological period lies outside the range of the training period flag -
!
! Executable Statements
!
! Identify number of cases to read
   Select Case (icex)
    Case (0)
      n = nt
    Case Default
      n = ntc
   End Select
!
   Return
 End Function get_n
!
!
!
 Subroutine read_data (afile, afield, ianal, n, n2y, lensn, idom, kav, v, ifail)
!
! Reads an input file
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Problem opening file
!    ifail =  4 File has been modified
!
! Modules
   Use data_cpt_constants,  Only: ia_non, id_grd, id_stn, id_ind
   Use data_io_constants,   Only: iin, io_com, lprd
   Use data_time_constants, Only: isq_mn, isq_sn, iuseq, nmn
   Use maths,               Only: get_cordn
   Use labels,              Only: ca_seqs_l
   Use time,                Only: Assignment(=), &
                                  nss, &
                                  get_cdate
   Use errors,              Only: cpt_error
   Use settings,            Only: isem
   Use space,               Only: Operator(==)
   Use iofiles,             Only: ifile, &
                                  file_mtime, open_iofile
   Use fields,              Only: field
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: n     ! - number of cases -
   Integer, Intent(In) :: n2y   ! - number of cases to read per year (excluding lagged fields) -
   Integer, Intent(In) :: lensn ! - length of season -
!
   Type(ifile), Intent(In) :: afile ! - input file -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used variables -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: nms ! - number of months per year to read -
   Integer :: nny ! - number of cases per year (excluding lagged fields) -
!
   Character(Len= 128) :: cprog ! - progress -
   Character(Len=lprd) :: cdate ! - date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Trim
!
! Executable Statements
!
! Open file
   If (ianal /= ia_non) Print *, 'Reading '//Trim(afile%cfile%ffile)//' ...'
   Call open_iofile (iin, afile%cfile%ffile, .true., .true., .true., ifail)
   If (ifail /= 0) Then
      Call cpt_error ('open_iofile', .true., ifail, &
           c_arg1=Trim(afile%cfile%ffile))
      Return
   End If
!
! Check whether file has been modified
   If (file_mtime(afile%cfile%ffile) > afile%mtime) Then
      ifail = 4
      Call cpt_error ('open_iofile', .true., ifail, &
           c_arg1=Trim(afile%cfile%ffile))
      Return
   End If
!
! Initialise
   Select Case (afile%iseq)
    Case Default
      nny = 1
    Case (isq_mn)
      nny = nmn
    Case (isq_sn)
      nny = nss
   End Select
   Select Case (afile%iseq)
    Case Default
      nms = n2y
     Case (isq_mn)
      nms = lensn
   End Select
   nms = Min(nms, nmn)
   irskip = afile%it1 - 1
   nread = 0
   prd0 = 0
   ilat0 = 0
   ctag0 = 'None'
   ifail = -1
!
! Read data
   Select Case (afile%idstr)
    Case (id_grd)
      Call read_grid (afile, afield(:), n, nms, nny, idom(:,:), kav(:,:), v(:,:,:), ifail)
    Case (id_stn)
      Call read_stns (afile, afield(:), n, nms, nny, idom(:,:), kav(:,:), v(:,:,:), ifail)
    Case (id_ind)
      Call read_unrf (afile, afield(:), n, nms, nny, idom(:,:), kav(:,:), v(:,:,:), ifail)
   End Select
!
! Close file
   Close (Unit=iin)
!
! Operate on ensemble
   Select Case (ifail)
    Case  (0) ! - no error -
      If (afile%lensemble .and. (isem == 1)) Then ! - sort ensemble members -
         Call sort_ensemble (afile%nfs/afile%nem, afile%nls, n, afield(:)%nv, afile%nem, v(:,:,:))
      End If
!
! Errors
    Case (-1) ! - aborted -
      Return
    Case Default ! - errors -
      If (prd0%sdate%iyr > 0) Then
         cdate = get_cdate(prd0, 1, io_com)
         If (nread > 0) Then
            If (ilat0 > 0) Then
               Write (Unit=cprog, Fmt='(A,I0,4A,I0,3A)') &
                  'Data up to the ', ilat0, get_cordn(ilat0), ' latitude for', Trim(cdate), &
                  ' (', nread, ' ', Trim(ca_seqs_l(iuseq(afile%iseq))), ' of training period) read successfully.'
            Else
               Write (Unit=cprog, Fmt='(3A,I0,3A)') &
                  'Data up to ', Trim(cdate), &
                  ' (', nread, ' ', Trim(ca_seqs_l(iuseq(afile%iseq))), ' of training period) read successfully.'
            End If
         Else
            If (ilat0 > 0) Then
               Write (Unit=cprog, Fmt='(A,I0,4A)') &
                  'Data up to the ', ilat0, get_cordn(ilat0), ' latitude for ', Trim(cdate), ' read successfully.'
            Else
               Write (Unit=cprog, Fmt='(3A)') &
                  'Data up to ', Trim(cdate), ' read successfully.'
            End If
         End If
         Call cpt_error ('read_data', .true., ifail, &
              i_arg1=nread, c_arg1=Trim(afile%cfile%ffile), c_arg2=Trim(cprog))
      Else
         Call cpt_error ('read_data', .true., ifail, &
              c_arg1=Trim(afile%cfile%ffile))
      End If
   End Select
!
   Return
!
 Contains
!
!
  Subroutine sort_ensemble (nfs, nls, n, nv, nem, v)
!
! Sorts ensemble members
!
! Modules
   Use arrays, Only: rwk, &
                     insertion_sort
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: nfs ! - number of fields -
   Integer, Intent(In) :: nls ! - number of lagged-fields -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nem ! - number of ensemble members -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv ! - number of variables -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(InOut) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i   ! - variable index -
   Integer :: ie  ! - ensemble member index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: iv  ! - current variable -
   Integer :: iv1 ! - variable offset -
   Integer :: k   ! - time index -
   Integer :: l   ! - field/lagged field index -
!
! Executable Statements
!
! Sort ensemble members
   iv1 = 0
   Do ifd = 1, nfs/nem
      Do ilf = 1, nls
         l = (ifd - 1)*nem*nls + ilf
         Do i = 1, nv(l)
            Do k = 1, n
               Do ie = 1, nem
                  iv = iv1 + ((ie - 1)*nls + (ilf - 1))*nv(l) + i
                  rwk(ie) = v(iv,k,1)
               End Do
               Call insertion_sort ('a', nem, rwk(:))
               Do ie = 1, nem
                  iv = iv1 + ((ie - 1)*nls + (ilf - 1))*nv(l) + i
                  v(iv,k,1) = rwk(ie)
               End Do
            End Do
         End Do
      End Do
      l = (ifd - 1)*nem*nls
      iv1 = iv1 + nem*nls*nv(l)
   End Do
!
   Return
  End Subroutine sort_ensemble
 End Subroutine read_data
!
!
!
 Subroutine read_grid (afile, afield, n, nms, nny, idom, kav, v, ifail)
!
! Reads gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use data_io_constants, Only: iin
   Use iofiles,           Only: ifile
   Use fields,            Only: field
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nms ! - number of months per year to read -
   Integer, Intent(In) :: nny ! - number of cases per year (excluding lagged fields) -
!
   Type(ifile), Intent(In) :: afile ! - input file -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Executable Statements
!
! Read gridded data
   ifail = 1
   Select Case (afile%cfile%ffmt%iver)
    Case (9)
      Call read_grid_v9 (n, afield(:), idom(:,:), kav(:,:), v(:,:,1), ifail)
    Case (10)
      If (afile%lstack) Then
         Call read_grid_v10 (afile%iseq, afile%ntag, afile%nfs,         1, afile%nls, afile%ngs, afile%ntm, &
              nms, n, nny, afield(:), idom(:,:), kav(:,:), v(:,:,:), ifail)
      Else
         Call read_grid_v10 (afile%iseq, afile%ntag,         1, afile%nfl,         1, afile%ngs, afile%ntm, &
              nms, n, nny, afield(:), idom(:,:), kav(:,:), v(:,:,:), ifail)
      End If
   End Select
!
   Return
!
 Contains
!
!
  Subroutine read_grid_v9 (n, afield, idom, kav, v, ifail)
!
! Reads formatted gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use data_time_constants, Only: lmon
   Use time,                Only: pdate, &
                                  get_month
   Use progress_meter,      Only: update_progress_meter
   Use arrays,              Only: rwk
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i    ! - latitude index -
   Integer :: i1   ! - first latitude index -
   Integer :: i2   ! - last latitude index -
   Integer :: iinc ! - latitude index increment -
   Integer :: j    ! - longitude index -
   Integer :: j0   ! - initial longitude offset -
   Integer :: jj   ! - grid index -
   Integer :: k    ! - time index -
!
   Real(Kind=rp) :: rlat ! - latitudes -
!
   Character(Len=lmon) :: cmon ! - current month -
!
   Type(pdate) :: date1 ! - current date -
!
! Executable Statements
!
! Skip first few records if required
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   If (irskip > 0) Then
      Do k = 1, irskip
         If (.not.kav(k,1)) Cycle
         Read (Unit=iin, Fmt=*, Err=1, End=2) date1%idy, cmon, date1%iyr
         date1%imn = get_month(cmon)
         Do i = 1, afield(1)%nlt
            Read (Unit=iin, Fmt=*, Err=1, End=2)
         End Do
         If (update_progress_meter(.false., ifail=ifail) /= 0) Return
         prd0%sdate = date1
      End Do
   End If
!
! Determine direction
   If (afield(1)%ln2s) Then
      i1 = 1
      i2 = afield(1)%nlt
      iinc = 1
      j0 = 0
   Else
      i1 = afield(1)%nlt
      i2 = 1
      iinc = -1
      j0 = afield(1)%region%nlgs*(afield(1)%region%nlts - 1)
   End If
!
! Read data
   Do k = 1, n
      If (kav(irskip+k,1)) Then
         Read (Unit=iin, Fmt=*, Err=1, End=2) date1%idy, cmon, date1%iyr
         date1%imn = get_month(cmon)
         If (update_progress_meter(.false., ifail=ifail) /= 0) Return
         jj = j0
         Do i = i1, i2, iinc
            If (i >= afield(1)%region%nlt1 .and. i <= afield(1)%region%nlt2) Then
               Read (Unit=iin, Fmt=*, Err=1, End=2) rlat, (rwk(j), j=1,afield(1)%nlg)
               v(jj+1:jj+afield(1)%region%nlgs,k) = rwk(idom(1:afield(1)%region%nlgs,1))
               jj = jj + afield(1)%region%nlgs*iinc
            Else
               Read (Unit=iin, Fmt=*, Err=1, End=2)
            End If
            If (update_progress_meter(.false., ifail=ifail) /= 0) Return
            ilat0 = i
         End Do
         ilat0 = 0
         prd0%sdate = date1
         nread = nread + 1
      Else
         v(1:afield(1)%nv,k) = afield(1)%rmiss
      End If
   End Do
!
! No errors
   ifail = 0
   Return
!
! Error
1  ifail = 1
   GoTo 3
!
! End of file
2  ifail = 2
!
3  If (ilat0 > 0) prd0%sdate = date1
   prd0%edate = prd0%sdate
   Return
!
  End Subroutine read_grid_v9
!
!
!
  Subroutine read_grid_v10 (iseq, ntag, nstk, nutk, nls, ngs, nt, nms, n, nny, afield, idom, kav, v, ifail)
!
! Reads formatted gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn
   Use time,                Only: get_pdate
   Use progress_meter,      Only: update_progress_meter
   Use arrays,              Only: rwk
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: iseq ! - time sequencing -
   Integer, Intent(In) :: n    ! - number of cases to read -
   Integer, Intent(In) :: ngs  ! - number of groups -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nms  ! - number of months per year to read -
   Integer, Intent(In) :: nny  ! - number of cases per year (excluding lagged fields) -
   Integer, Intent(In) :: nstk ! - number of stacked fields -
   Integer, Intent(In) :: nt   ! - number of cases in file -
   Integer, Intent(In) :: ntag ! - number of XML namespace headers and tag lines -
   Integer, Intent(In) :: nutk ! - number of unstacked fields -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i     ! - latitude index -
   Integer :: i1    ! - first latitude index -
   Integer :: i2    ! - last latitude index -
   Integer :: iinc  ! - latitude index increment -
   Integer :: igf   ! - category index -
   Integer :: igm   ! - category / month index -
   Integer :: ik    ! - case counter -
   Integer :: ilm   ! - lagged-field / month index -
   Integer :: istk  ! - stack index -
   Integer :: iutk  ! - un-stacked field index -
   Integer :: j     ! - longitude index -
   Integer :: jj    ! - grid index -
   Integer :: k     ! - time index -
   Integer :: kk    ! - year index -
   Integer :: l     ! - stacked / unstacked field index -
   Integer :: lm    ! - field / lagged field index -
   Integer :: lmn   ! - last month -
   Integer :: nlm   ! - number of lagged fields / months per season -
   Integer :: nskip ! - number of records to skip -
   Integer :: ierr  ! - error indicator -
!
   Real(Kind=rp) :: rlat ! - latitudes -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Sum
!
! Executable Statements
!
! Skip v10 XML namespace headers and tags
   xml: Do k = 1, ntag
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End Do xml
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Skip first few records if required
   nlm = Max(nls, nms)
   Do istk = 1, nstk
      nskip = irskip
      ik = 0
      If (irskip > 0) Then
         If (nls > 1) nskip = nskip/nls ! - if any years are skipped all lagged fields for that year are skipped -
         Call skip_grid_v10 (nskip, istk, nutk, ngs, nls, 1, afield(:)%nlt, kav(:,:), ik, ifail)
         If (ifail /= 0) GoTo 3
         ctag0 = ctag1
      End If
!
! Read data
      kk = 0
      read_cases: Do k = 1, n
         Do iutk = 1, nutk ! - repeat for each unstacked field -
            Do ilm = 1, nlm ! - repeat for each lagged field or month / season -
               Call set_ft_counters (istk, iutk, nutk, nls, ilm, ik, kk, l, lm) ! - determine field and time counters -
               If (nms == 1) Then
                  igm = 0
               Else
                  igm = ilm - 1
               End If
! - determine direction -
               If (afield(lm)%ln2s) Then
                  i1 = 1
                  i2 = afield(lm)%nlt
                  iinc = 1
               Else
                  i1 = afield(lm)%nlt
                  i2 = 1
                  iinc = -1
               End If
! - read tag line -
               If (kav(ik,lm)) Then
                  Do igf = 1, ngs
                     igm = igm + 1
                     If (afield(lm)%ln2s) Then
                        jj = 0
                     Else
                        jj = afield(lm)%region%nlgs*(afield(lm)%region%nlts - 1)
                     End If
                     If (lm > 1 .and. iseq /= isq_sn) jj = jj + Sum(afield(1:lm-1)%region%nlts*afield(1:lm-1)%region%nlgs)
                     Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
                     Read (Unit=iin, Fmt=*, Err=1, End=2)
                     If (update_progress_meter(.false., ifail=ifail) /= 0) Return
! - read data -
                     Do i = i1, i2, iinc
                        If (i >= afield(lm)%region%nlt1 .and. i <= afield(lm)%region%nlt2) Then
                           Read (Unit=iin, Fmt=*, Err=1, End=2) rlat, (rwk(j), j=1,afield(lm)%nlg)
                           v(jj+1:jj+afield(lm)%region%nlgs,kk,igm) = rwk(idom(1:afield(lm)%region%nlgs,lm))
                           jj = jj + afield(lm)%region%nlgs*iinc
                        Else
                           Read (Unit=iin, Fmt=*, Err=1, End=2)
                        End If
                        If (update_progress_meter(.false., ifail=ifail) /= 0) Return
                        ilat0 = i
                     End Do
                     ilat0 = 0
                  End Do
                  ctag0 = ctag1
! - indicate missing data -
               Else
                  Do igf = 1, ngs
                     igm = igm + 1
                     If (afield(lm)%ln2s) Then
                        jj = 0
                     Else
                        jj = afield(lm)%region%nlgs*(afield(lm)%region%nlts - 1)
                     End If
                     If (lm > 1 .and. iseq /= isq_sn) jj = jj + Sum(afield(1:lm-1)%region%nlts*afield(1:lm-1)%region%nlgs)
                     v(jj+1:jj+afield(lm)%region%nlts*afield(lm)%region%nlgs,kk,igm) = afield(lm)%rmiss
                     Do i = 1, afield(lm)%nlt+1
                        If (update_progress_meter(.false., ifail=ifail) /= 0) Return
                     End Do
                  End Do
               End If
               nread = nread + 1
               If (ik == nt .and. iutk == nutk) Then
                  If (ilm < nlm) v(:,kk,igm+1:) = afield(lm)%rmiss ! - set incomplete seasons to missing -
                  Exit read_cases
               End If
            End Do
!
! Skip any unused months
            If (iseq == isq_mn) Then ! - includes isq_so Y-files -
               If (k == n .or. nms == nny) Cycle
               lmn = nny - nms
               If (ik + lmn > nt) lmn = nt - ik
               Call skip_grid_v10 (lmn, istk, nutk, ngs, nls, 1, afield(:)%nlt, kav(:,:), ik, ifail)
               If (ifail /= 0) GoTo 3
            End If
         End Do
      End Do read_cases
!
! Skip any additional data if there are additional stacked fields
      If (istk < nstk .and. ik < nt) Then
         Call skip_grid_v10 (nt-ik, istk, nutk, ngs, nls, 1, afield(:)%nlt, kav(:,:), ik, ifail)
         If (ifail /= 0) GoTo 3
      End If
   End Do
!
! No errors
   ifail = 0
   Return
!
! Error reading file
1  ifail = 1
   GoTo 3
!
! End of file
2  ifail = 2
!
! Get latest date
3  If (ilat0 > 0) ctag0 = ctag1
   If (ctag0(1:4) /= 'None') Then
      Call get_pdate ('T', ctag0, prd0%sdate, ierr, &
           edate=prd0%edate)
   End If
!
   Return
  End Subroutine read_grid_v10
!
!
!
  Subroutine skip_grid_v10 (nskip, istk, nutk, ngs, nls, nlm, nlt, kav, ik, ifail)
!
! Skips formatted gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use progress_meter, Only: update_progress_meter
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: istk  ! - current stack index -
   Integer, Intent(In) :: nutk  ! - number of unstacked fields -
   Integer, Intent(In) :: ngs   ! - number of groups -
   Integer, Intent(In) :: nls   ! - number of lagged fields -
   Integer, Intent(In) :: nlm   ! - number of lagged fields / months to read -
   Integer, Intent(In) :: nskip ! - number of times to skip -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nlt ! - number of latitudes -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ik ! - time counter -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i    ! - latitude index -
   Integer :: igf  ! - category index -
   Integer :: ilm  ! - lag / month index -
   Integer :: iutk ! - field index -
   Integer :: k    ! - time index -
   Integer :: kk   ! - year index -
   Integer :: l    ! - field counter -
   Integer :: lm   ! - field and lagged-field counter -
!
! Executable Statements
!
! Skip records
   kk = 0
   Do k = 1, nskip
      Do iutk = 1, nutk ! - repeat for each unstacked field -
         Do ilm = 1, nlm ! - repeat for each lagged field or month / season -
            Call set_ft_counters (istk, iutk, nutk, nls, ilm, ik, kk, l, lm) ! - determine field and time counters -
            If (.not.kav(ik,lm)) Cycle
            Do igf = 1, ngs
               Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
               Read (Unit=iin, Fmt=*, Err=1, End=2)
               Do i = 1, nlt(lm)
                  Read (Unit=iin, Fmt=*, Err=1, End=2)
               End Do
            End Do
            ctag0 = ctag1
            If (update_progress_meter(.false., ifail=ifail) /= 0) Return
         End Do
      End Do
   End Do
!
! No errors
   ifail = 0
   Return
!
! Error reading file
1  ifail = 1
   Return
!
! End of file
2  ifail = 2
   Return
!
  End Subroutine skip_grid_v10
 End Subroutine read_grid
!
!
!
 Subroutine read_stns (afile, afield, n, nms, nny, idom, kav, v, ifail)
!
! Selects appropriate reading routine based on file format
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use iofiles, Only: ifile
   Use fields,  Only: field
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nms ! - number of months per season -
   Integer, Intent(In) :: nny ! - number of cases per year (excluding lagged fields) -
!
   Type(ifile), Intent(In) :: afile ! - input file -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Executable Statements
!
! Read station data
   Select Case (afile%cfile%ffmt%iver)
    Case (9)
      Call read_nongrid_v9 (afile%iseq, afile%lmax, afield(1)%nlt, afield(1)%region%nlts, n, afield(1)%rmiss, kav(:,:), v(:,:,1), &
           ifail, &
           idom=idom)
    Case (10)
      Call read_nongrid_v10 (afile%iseq, afile%lmax, afile%ntag, afile%nfs, afile%nls, afile%ngs, afile%ntm, nms, n, nny, &
           afield(:), idom(:,:), kav(:,:), v(:,:,:), ifail)
   End Select
!
   Return
 End Subroutine read_stns
!
!
!
 Subroutine read_unrf (afile, afield, n, nms, nny, idom, kav, v, ifail)
!
! Selects appropriate reading routine based on file format
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use iofiles, Only: ifile
   Use fields,  Only: field
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nms ! - number of months per season -
   Integer, Intent(In) :: nny ! - number of cases per year (excluding lagged fields) -
!
   Type(ifile), Intent(In) :: afile ! - input file -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Executable Statements
!
! Read ungridded data
   Select Case (afile%cfile%ffmt%iver)
    Case (9)
      Call read_nongrid_v9 (afile%iseq, afile%lmax, afield(1)%nv, afield(1)%nv, n, afield(1)%rmiss, kav(:,:), v(:,:,1), ifail)
    Case (10)
      Call read_nongrid_v10 (afile%iseq, afile%lmax, afile%ntag, afile%nfs, afile%nls, afile%ngs, afile%ntm, nms, n, nny, &
           afield(:), idom(:,:), kav(:,:), v(:,:,:), ifail)
   End Select
!
   Return
 End Subroutine read_unrf
!
!
!
 Subroutine read_nongrid_v9 (iseq, lmax, nv, nvu, n, rmiss, kav, v, ifail, &
            idom)
!
! Reads formatted station data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use data_numbers,        Only: one
   Use data_io_constants,   Only: iin
   Use data_time_constants, Only: isq_yr
   Use time,                Only: get_pdate
   Use progress_meter,      Only: set_progress_increment, update_progress_meter
   Use arrays,              Only: rwk
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: iseq ! - time sequencing -
   Integer, Intent(In) :: lmax ! - maximum width of file -
   Integer, Intent(In) :: n    ! - number of cases -
   Integer, Intent(In) :: nv   ! - number of variables -
   Integer, Intent(In) :: nvu  ! - number of variables -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
! - input arrays -
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: v ! - data -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:,:), Intent(In), Optional :: idom ! - used gridpoints -
!
! Locals
!
! Local scalars
   Integer :: i      ! - station index -
   Integer :: k      ! - time index -
   Integer :: icskip ! - number of initial spaces to pick -
   Integer :: lcmin  ! - minimum of length of ctag and cline -
!
   Character(Len=lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Present, Real, Trim
!
! Executable Statements
!
! Skip station labels and coordinates / variable names
   ctag1 = ' '
   lcmin = Min(ltag, lmax)
   Read (Unit=iin, Fmt=*, Err=1, End=2)
   If (Present(idom)) Then
      Read (Unit=iin, Fmt=*, Err=1, End=2)
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End If
   If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Skip first few records if required
   If (irskip > 0) Then
      Do k = 1, irskip
         If (.not.kav(k,1)) Cycle
         Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
         ctag0 = ctag1
         If (update_progress_meter(.false., ifail=ifail) /= 0) Return
      End Do
   End If
!
! Read data
   Call set_progress_increment (Real(nv, Kind=rp), .true.)
   Do k = 1, n
      If (kav(irskip+k,1)) Then
         Call parse_line (iin, cline, icskip, ifail)
         If (nread == 0 .or. (iseq /= isq_yr .and. iseq /= 10)) Then
             If (ifail /= 0) GoTo 3
         End If
         If (Present(idom)) Then
            Read (Unit=cline(icskip+1:), Fmt=*, Err=1) (rwk(i),i = 1, nv)
            Do i = 1, nvu
               v(i,k) = rwk(idom(i,1))
            End Do
         Else
            Read (Unit=cline(icskip+1:), Fmt=*, Err=1) (v(i,k),i = 1, nv)
         End If
         ctag0(1:lcmin) = cline(1:lcmin)
         nread = nread + 1
      Else
         v(1:nvu,k) = rmiss
      End If
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   End Do
   Call set_progress_increment (one/Real(nv, Kind=rp), .true.)
!
! No errors
   ifail = 0
   Return
!
! Error reading file
1  ifail = 1
   GoTo 3
!
! End of file
2  ifail = 2
!
! Get latest date
3  If (ctag0(1:4) /= 'None') Then
      Call get_pdate (' ',Trim(ctag0), prd0%sdate, ifail, &
           edate=prd0%edate)
   End If
!
   Return
  End Subroutine read_nongrid_v9
!
!
!
 Subroutine read_nongrid_v10 (iseq, lmax, ntag, nfs, nls, ngs, nt, nms, n, nny, afield, idom, kav, v, ifail)
!
! Reads formatted station or unreferenced data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use data_numbers,        Only: one
   Use data_io_constants,   Only: iin
   Use data_time_constants, Only: isq_mn, isq_yr
   Use time,                Only: get_pdate
   Use progress_meter,      Only: set_progress_increment, update_progress_meter
   Use arrays,              Only: rwk
   Use fields,              Only: field
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: iseq ! - time sequencing -
   Integer, Intent(In) :: lmax ! - maximum width of file -
   Integer, Intent(In) :: n    ! - number of cases -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: ngs  ! - number of groups -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nms  ! - number of months per season -
   Integer, Intent(In) :: nny  ! - number of cases per year (excluding lagged fields) -
   Integer, Intent(In) :: nt   ! - number of cases in file -
   Integer, Intent(In) :: ntag ! - number of XML namespace headers and tag lines -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: idom ! - used gridpoints -
!
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
   Type(field), Dimension(:), Intent(In) ::  afield ! - fields -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i      ! - station/variable index -
   Integer :: k      ! - time index -
   Integer :: kk     ! - year index -
   Integer :: l      ! - field / lagged field index -
   Integer :: lm     ! - field / month index -
   Integer :: lmn    ! - last month -
   Integer :: ifd    ! - field index -
   Integer :: ilm    ! - lagged-field / month index -
   Integer :: igf    ! - category index -
   Integer :: igm    ! - category / month index -
   Integer :: ij     ! - available station index -
   Integer :: ik     ! - case counter -
   Integer :: jj     ! - available station/field index -
   Integer :: icskip ! - number of initial spaces to pick -
   Integer :: nskip  ! - number of records to skip -
   Integer :: nlm    ! - number of lagged fields / months per season -
   Integer :: ierr   ! - error indicator -
!
   Character(Len=lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Max, Real, Trim
!
! Executable Statements
!
! Read v10 XML namespace headers and tags
   xml: Do
      Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
      If (Index(ctag1,'cpt:field=') > 0) Then
         Exit xml
      Else
         Cycle xml
      End If
   End Do xml
!
! Skip station/index names and tags
   jj = 0
   nlm = Max(nls, nms)
   Do ifd = 1, nfs
      l = (ifd - 1)*nls + 1
      Do igf = 1, ngs
         If (ifd /= 1 .or. igf /= 1) Read (Unit=iin, Fmt=*, Err=1, End=2) ctag1
         Do k = 1, ntag
            Read (Unit=iin, Fmt=*, Err=1, End=2) ctag1
         End Do
         ctag0 = ctag1
         If (update_progress_meter(.false., ifail=ifail) /= 0) Return
!
! Skip first few records if required
         nskip = irskip
         ik = 0
         If (irskip > 0) Then
            If (nls > 1) nskip = nskip/nls ! - if any years are skipped all lagged fields for that year are skipped -
            Call skip_nongrid_v10 (nskip, ifd, nls, 1, kav(:,:), ik, ifail)
            If (ifail /= 0) GoTo 3
         End If
!
! Read data
         nread = 0
         kk = 0
         Do k = 1, n
            ij = 0
            Do ilm = 1, nlm
               Call set_ft_counters (ifd, 1, 1, nls, ilm, ik, kk, l, lm) ! - determine field and time counters -
! $$$$ FTN95 BUG
!              If (l > 1) ij = Sum(afield(1:l-1)%region%nlts)
               If (nls > 1 .and. ilm > 1) Then
                  ij = 0
                  Do i = (ifd-1)*nls+1, (ifd-1)*nls+ilm-1
                     ij = ij + afield(i)%region%nlts
                  End Do
               End If
! $$$$ End of FTN95 BUG patch
               If (nms == 1) Then
                  igm = 1
               Else
                  igm = ilm
               End If
               If (kav(ik,lm)) Then
                  Call parse_line (iin, cline, icskip, ifail)
                  If (nread == 0 .or. nlm > 1 .or. (iseq /= isq_yr .and. iseq /= 10)) Then
                      If (ifail /= 0) GoTo 3
                  End If
                  Read (Unit=cline(icskip+1:), Fmt=*, Err=1) (rwk(i),i = 1, afield(lm)%nlt)
!
! Extract stations within domain
                  Do i = 1, afield(lm)%region%nlts
                     v(jj+ij+i,kk,igm) = rwk(idom(i,lm))
                  End Do
                  ctag0 = cline
               Else
                  v(jj+ij+1:jj+ij+afield(lm)%region%nlts,kk,igm) = afield(lm)%rmiss
               End If
! - update progress meter -
               Call set_progress_increment (Real(afield(lm)%nlt, Kind=rp), .true.)
               If (update_progress_meter(.false., ifail=ifail) /= 0) Return
               Call set_progress_increment (Real(one/afield(lm)%nlt, Kind=rp), .true.)
               nread = nread + 1
            End Do
!
! Skip any unused months
            If (iseq == isq_mn) Then ! - includes isq_so Y-files -
               If (k == n .or. nms == nny) Cycle
               lmn = nny - nms
               If (ik + lmn > nt) lmn = nt - ik
               Call skip_nongrid_v10 (lmn, ifd, nls, 1, kav(:,:), ik, ifail)
               If (ifail /= 0) GoTo 3
            End If
         End Do
!
! Skip any additional data
         If ((ifd < nfs .or. igf < ngs) .and. ik < nt) Then
            Call skip_nongrid_v10 (nt-ik, ifd, nls, 1, kav(:,:), ik, ifail)
            If (ifail /= 0) GoTo 3
         End If
      End Do
      jj = jj + afield(ifd)%nv
   End Do
!
! No errors
   ifail = 0
   Return
!
! Error reading file
1  ifail = 1
   GoTo 3
!
! End of file
2  ifail = 2
!
! Get latest date
3  If (ctag0(1:4) /= 'None') Then
      Call get_pdate (' ', Trim(ctag0), prd0%sdate, ierr, &
           edate=prd0%edate)
   End If
!
   Return
!
 Contains
!
!
  Subroutine skip_nongrid_v10 (nskip, ifd, nls, nlm, kav, ik, ifail)
!
! Skips formatted non-gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Aborted
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: ifd   ! - current field index -
   Integer, Intent(In) :: nls   ! - number of lagged fields -
   Integer, Intent(In) :: nlm   ! - number of lagged fields / months to read -
   Integer, Intent(In) :: nskip ! - number of times to skip -
!
! - input arrays -
   Logical, Dimension(:,:), Intent(In) :: kav ! - cases available? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ik ! - time counter -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: ilm ! - lag / month index -
   Integer :: k   ! - time index -
   Integer :: kk  ! - year index -
   Integer :: l   ! - field counter -
   Integer :: lm  ! - field and lagged-field counter -
!
! Executable Statements
!
! Skip records
   kk = 0
   Do k = 1, nskip
      Do ilm = 1, nlm ! - repeat for each lagged field or month / season -
         Call set_ft_counters (ifd, 1, 1, nls, ilm, ik, kk, l, lm) ! - determine field and time counters -
         If (.not.kav(ik,lm)) Cycle
         Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag1
         ctag0 = ctag1
      End Do
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   End Do
!
! No errors
   ifail = 0
   Return
!
! Error reading file
1  ifail = 1
   Return
!
! End of file
2  ifail = 2
   Return
!
  End Subroutine skip_nongrid_v10
 End Subroutine read_nongrid_v10
!
!
!
 Subroutine set_ft_counters (istk, iutk, nutk, nls, ilm, ik, kk, l, lm)
!
! Sets field and time counters
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: ilm  ! - lagged-field / month index -
   Integer, Intent(In) :: istk ! - current stacked field index -
   Integer, Intent(In) :: iutk ! - current unstacked field index -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: nutk ! - number of unstacked fields -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ik ! - time counter -
   Integer, Intent(InOut) :: kk ! - year counter -
!
! - output scalars -
   Integer, Intent(Out) :: l  ! - field counter -
   Integer, Intent(Out) :: lm ! - field and lagged-field counter -
!
! Executable Statements
!
! Set counters
   l = ((istk - 1)*nutk + (iutk - 1))*nls + 1
   If (ilm == 1 .and. iutk ==1) kk = kk + 1     ! - increment year if ilm is a new month and lagged field -
   If (nls <= 1) Then ! - no lagged fields          ilm is a new month or season rather than a lagged field -
      If (iutk == 1) ik = ik + 1                ! - increment case number after all unstacked fields -
      lm = l
   Else ! - lagged fields                           ilm is a lagged field rather than a new month or season -
      If (iutk == 1 .and. ilm == 1) ik = ik + 1 ! - increment case number after all unstacked and lagged fields -
      lm = l + ilm - 1                          ! - count new lagged field -
   End If
!
    Return
  End Subroutine set_ft_counters
!
!
!
 Subroutine parse_line (iin, cline, icskip, ifail)
!
! Reads a line from an input file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
   Use data_numbers, Only: cdigits
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
! - output scalars -
   Integer, Intent(Out) :: icskip ! - number of initial spaces to skip -
   Integer, Intent(Out) :: ifail  ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cline ! - line -
!
! Locals
!
! Local scalars
   Integer :: i1 ! - locator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Scan, Verify
!
! Executable Statements
!
! Read first case
   ifail = 0
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
   i1 = Scan(cline, cdigits//'/-T:')
   icskip = i1 + Verify(cline(i1:), cdigits//'/-T:') - 2
   Return
!
! Error reading file
1  ifail = 1
   Return
!
! End of file
2  ifail = 2
!
   Return
 End Subroutine parse_line
!
!
!
 Subroutine proj_read_data (iin, ianal, lb, nsn, ifail)
!
! Reads input data from project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  4 Problem allocating memory
!
! Modules
   Use data_cpt_constants,  Only: ia_cca, ia_gcm, ia_pcr, ia_mlr, ia_pfv, ir_bin
   Use data_time_constants, Only: isq_sn, isq_so
   Use time,                Only: iseq
   Use settings,            Only: iregr, nt
   Use arrays,              Only: t, x, y
   Use iofiles,             Only: mxa, mya, xfile
   Use climate,             Only: icex
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input file unit number -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: lb    ! - lower bound -
   Integer, Intent(In) :: nsn   ! - number of seasons -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: n ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max
!
! Executable Statements
!
! Calculate memory requirements
! - set earlier start date if climatological period starts before training period -
   n = get_n(icex)
!
! Allocate membory
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr)
      If (init_read(mxa, mya, xfile%ngs, nt, nt, n, 1, &
                    lb=lb, ns1=nsn) /= 0) GoTo 4
    Case (ia_gcm)
      If (init_read(mxa, mya, Max(xfile%nem, 1), nt, nt, n, 1, &
                    lb=lb, ns1=nsn) /= 0) GoTo 4
    Case (ia_pfv)
      Select Case (iseq)
       Case Default
         If (init_read(mxa, mya, xfile%ngs, nt, nt, n, 1, &
                       lb=lb, ns1=1) /= 0) GoTo 4
       Case (isq_sn, isq_so)
         If (init_read(mxa, mya, xfile%ngs, nt, nt, n, 1, &
                       lb=lb, ns1=1) /= 0) GoTo 4
      End Select
   End Select
!
! Read input data
   Read (Unit=iin, Err=1, End=2) x
   Read (Unit=iin, Err=1, End=2) y
   If (iregr == ir_bin) Then
      Read (Unit=iin, Err=1, End=2) t
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
! - memory allocation problem -
4  ifail = 4
   Return
!
 End Subroutine proj_read_data
!
!
!
 Subroutine proj_write_data (iout, ifail)
!
! Writes input data to project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem writing to file
!
! Modules
   Use data_cpt_constants, Only: ir_bin
   Use settings,           Only: iregr
   Use arrays,             Only: t, x, y
   Use iofiles,            Only: mxa, mya
!
! Arguments
!
! Dummy aruments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write input data
   Write (Unit=iout, Err=1) x(1:mxa,:,:)
   Write (Unit=iout, Err=1) y(1:mya,:,:)
   If (iregr == ir_bin) Then
      Write (Unit=iout, Err=1) t(:)
   End If
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing to file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_data
End Module cpt_input
