! Author: Simon Mason
Module file_structure
!
! Modules
   Use data_numbers, Only: rp
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: get_structure
!
Contains
!
!
 Subroutine get_structure (lms, lss, lbfile, rmiss, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, kav, ifail)
!
! Determines structure of an input file, including its size, field settings, and missing cases
!
! On exit:
!    ifail  = 0 Successful
!    ifail /= 1 Depends on value of cproc
!
! Arguments
!
! Modules
   Use data_io_constants, Only: iin, io_iso, lprd, lstn
   Use labels,            Only: ca_dstruct_l
   Use time,              Only: pprd, &
                                Assignment(=), Operator(/=), &
                                get_cdate
   Use errors,            Only: cproc, &
                                cpt_error
   Use iofiles,           Only: ifile, &
                                file_version, open_iofile
   Use fields,            Only: field
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
   Logical, Intent(In) :: lbfile ! - other file open? -
   Logical, Intent(In) :: lms    ! - permit monthly-sequenced files? -
   Logical, Intent(In) :: lss    ! - permit seasonally-sequenced files? -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
   Type(field), Dimension(:), Pointer :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: iarg1 ! - error optional argument -
   Integer :: iarg2 ! - error optional argument -
   Integer :: iarg3 ! - error optional argument -
   Integer :: ifd   ! - current field -
   Integer :: ilf   ! - current lagged-field -
   Integer :: igf   ! - current category field -
!
   Character(Len=lprd) :: cldate ! - last date read successfully -
   Character(Len=lprd) :: cfail  ! - error indicator -
!
   Type(pprd) :: pdlast ! - last period read successfully -
   Type(pprd) :: prd0   ! - null period -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Associated, Trim
!
! Executable Statements
!
! Initialise
   iarg1 = 0
   iarg2 = 0
   iarg3 = 0
   cldate = ' '
   cfail = ' '
!
! Check for version 10+ format
   afile%cfile%ffmt%iver = file_version(afile%cfile%ffile)
!
! Open file
   Call open_iofile (iin, afile%cfile%ffile, .true., .true., .true., ifail)
   If (ifail /= 0) Then
      cproc = 'open_iofile'
      GoTo 1
   End If
!
! Determine file structure
   Select Case (afile%cfile%ffmt%iver)
    Case (9)
      Call get_structure_v9 (rmiss, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, &
           idom, kav, ifail, cproc, pdlast, cfail)
    Case (10)
      Call get_structure_v10 (lms, lss, lbfile, rmiss, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, &
           idom, kav, ifd, ilf, igf, ifail, cproc, pdlast, cfail)
      iarg1 = ifd
      iarg2 = ilf
      iarg3 = igf
    Case Default
      cproc = 'file_version'
      ifail = 5
      iarg1 = afile%cfile%ffmt%iver
   End Select
!
! Update file settings
   afile%cgss = Trim(ca_dstruct_l(afile%idstr))
!
! Errors
1  If (ifail /= 0) Then
      prd0 = 0
      If (pdlast /= prd0) cldate = get_cdate(pdlast, 1, io_iso) ! - identify last date read -
      If (Associated(kav)) Then ! - free workspace -
         Deallocate (kav)
         Nullify (kav)
      End If
      Call cpt_error (Trim(cproc), .false., ifail, &
           c_arg1=Trim(afile%cfile%ffile), c_arg2=Trim(cldate), c_arg3=Trim(cfail), i_arg1=iarg1, i_arg2=iarg2, i_arg3=iarg3)
   Else
      afield(:)%unitc = afield%unit
   End If
   Close (Unit=iin)
!
   Return
 End Subroutine get_structure
!
!
!
 Subroutine get_structure_v9 (rmiss, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, kav, ifail, cproc, pdlast, cfail)
!
! Determines structure of a version-9 input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_structure_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!    ifail =  2 Problem reading file. Data up to pdlast read successfully
!    ifail =  3 Premature end of file reached. Data up to pdlast read successfully
!    ifail =  4 File is not in a valid CPT format
!
! Modules
   Use data_cpt_constants,  Only: id_grd, id_stn, id_ind
   Use data_io_constants,   Only: iin, lprd, lstn
   Use data_time_constants, Only: isq_mn, lmon
   Use labels,              Only: upcase
   Use time,                Only: pprd, &
                                  Assignment(=), &
                                  date_diff, get_month
   Use space,               Only: get_geog
   Use iofiles,             Only: ifile
   Use fields,              Only: field, &
                                  init_field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
   Type(field), Dimension(:), Pointer :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc  ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail  ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Locals
!
! Local scalars
   Integer :: idy ! - current day -
!
   Character(Len=lmon) :: ccmon ! - current month -
   Character(Len=lprd) :: ctag  ! - tags -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_structure_v9'
   cfail = ' '
   pdlast = 0
!
! Clear field information
   afile%nms = 1
   afile%nfs = 1
   afile%nls = 1
   afile%nfl = 1
   afile%ngs = 1
   Call init_field (1, [rmiss], afield, ifail)
   If (ifail /= 0) Then
      ifail = 1
      GoTo 3
   End If
!
! Determine structure of dataset
   Read (Unit=iin, Fmt=*, Err=1, End=2) ctag
   Call upcase (ctag)
   Select Case (Trim(ctag))
    Case ('STATION', 'STATIONS', 'STN') ! - station dataset -
      afile%idstr = id_stn
    Case ('YEARS', 'YEAR', 'NAME') ! - unreferenced -
      afile%idstr = id_ind
    Case Default ! - gridded -
      Rewind (Unit=iin)
      Read (Unit=iin, Fmt=*, Err=1, End=2) idy, ccmon
      If (get_month(ccmon) == 0) Then
         ifail = 4
         GoTo 3
      End If
      afile%idstr = id_grd
   End Select
   afile%igeog = get_geog(afile%idstr)
!
! Determine structure of gridded dataset
   Select Case (afile%idstr)
    Case (id_grd)
! - determine numbers of latitudes and longitudes -
      Call get_gridded_v9 (iin, afile, afield(1), rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, kav, ifail, cproc, cfail, pdlast)
      afile%lmax = 0
!
! Determine structure of station and unreferenced dataset
    Case (id_stn, id_ind)
      Call get_nongridded_v9 (iin, afile, afield(1), rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, kav, ifail, cproc, cfail, pdlast)
   End Select
   If (ifail /= 0) GoTo 3
!
! Set number of monthly cases
   Select Case (afile%iseq)
    Case Default
      afile%ntm = afile%nt
    Case (isq_mn)
      afile%ntm = date_diff(afile%prd1%sdate, afile%prdn%sdate, isq_mn) + 1
   End Select
!
! Set field information and number of tag lines
   afield(1)%tssn%tprd(1) = afile%prd1
   afield(1)%mdate = 0
   afile%ntag = 0
!
   Close (Unit=iin)
   ifail = 0
   Return
!
! Errors
! - problem reading file -
1  Close (Unit=iin)
   ifail = 2
   Return
! - problem reading file -
2  Close (Unit=iin)
   ifail = 3
   Return
!
! - other errors -
3  Close (Unit=iin)
!
   Return
 End Subroutine get_structure_v9
!
!
!
 Subroutine get_gridded_v9 (iin, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, kav, ifail, cproc, cfail, pdlast)
!
! Determines structure of a version-9 gridded input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_gridded_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
   Use data_io_constants, Only: lstn
   Use time,              Only: pprd
   Use space,             Only: reset_grids
   Use iofiles,           Only: ifile
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
   Type(field), Intent(InOut) :: afield ! - field -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_gridded_v9'
!
! Determine numbers of latitudes and longitudes
   Call get_gridded_dimensions_v9 (iin, afield%nlg, afield%nlt, afield%ln2s, ifail, cfail)
   If (ifail /= 0) Then
      cproc = 'get_gridded_dimensions_v9'
      Return
   End If
!
! Read latitudes and longitudes
   Call reset_grids (afile%idstr, 1, [afield%nlt], rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
        nlg=[afield%nlg])
   If (ifail /= 0) Then
      ifail = 1
      Return
   End If
   Call get_gridded_latlons_v9 (iin, afield%nlg, afield%nlt, afield%ln2s, rlng(:,1), rlat(:,1), ifail)
   If (ifail /= 0) Then
      cproc = 'get_gridded_latlons_v9'
      Return
   End If
!
! Determine number of time steps and sequencing
   If (Size(rlatd, Dim=1) > Size(rlngd, Dim=1)) Then
      Call get_gridded_nt_v9 (iin, afile, afield%ln2s, afield%nlt, afield%nlg, &
           rlat(:,1), rlng(:,1), kav, rlatd(:,1), ifail, cproc, cfail, pdlast)
   Else
      Call get_gridded_nt_v9 (iin, afile, afield%ln2s, afield%nlt, afield%nlg, &
           rlat(:,1), rlng(:,1), kav, rlngd(:,1), ifail, cproc, cfail, pdlast)
   End If
!
   Return
!
 Contains
!
!
  Subroutine get_gridded_dimensions_v9 (iin, nlg, nlt, ln2s, ifail, cfail)
!
! Determines numbers of latitudes and longitudes in a version-9 gridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 First line of data is too long. Data are probably truncated
!    ifail =  2 Unable to determine number of latitudes/longitudes (as indicated by cfail)
!    ifail =  3 Problem reading file
!
! Modules
   Use data_io_constants,   Only: lfli
   Use data_time_constants, Only: lmon
   Use time,                Only: get_month
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
! - output scalars -
   Integer, Intent(Out) :: nlg   ! - number of longitudes -
   Integer, Intent(Out) :: nlt   ! - number of latitudes -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Logical, Intent(Out) :: ln2s ! - north to south latitudes? -
!
! Locals
!
! Local scalars
   Integer :: j    ! - longitude index -
   Integer :: idy  ! - day -
   Integer :: iyr  ! - year -
   Integer :: inc  ! - increment -
   Integer :: llen ! - line length -
!
   Real(Kind=rp) :: dum  ! - dummy argument -
   Real(Kind=rp) :: rlt1 ! - first latitude -
   Real(Kind=rp) :: rlt2 ! - last latitude -
!
   Character(Len=lfli) :: fli   ! - first line of input -
   Character(Len=lmon) :: ccmon ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim
!
! Executable Statements
!
! Initialise routine
   cfail = ' '
!
! Determine length of first line of data
   Rewind (Unit=iin)
   llen = lfli
   Read (Unit=iin, Fmt='(A)', ERR=4, End=4) fli(1:llen)
   llen = Len_Trim(fli(1:llen))
! - return if line is too long -
   If (llen == lfli) Then
      ifail = 1
      Return
   End If
!
! Determine number of longitudes
   nlg = 1
   inc = 64
   Do
      Read (Unit=fli(1:llen), Fmt=*, Err=1, End=1) idy, ccmon, iyr, (dum, j=1,nlg)
      nlg = nlg + inc
      Cycle
1     nlg = nlg - inc
      If (inc == 1) Exit
      inc = inc/8
   End Do
! - return if unsuccessful -
   If (nlg < 1) Then
      ifail = 2
      cfail = 'longitudes'
      Return
   End If
!
! Determine number of latitudes
   nlt = 0
   Read (Unit=iin, Fmt=*, Err=3, End=2) rlt1, ccmon
   nlt = 1
! - return if unsuccessful -
2  If (nlt == 0) Then
      ifail = 2
      cfail = 'latitudes'
      Return
   End If
! - return if next line indicates a new date, implying no data -
   If (get_month(ccmon) > 0) Then
      ifail = 2
      cfail = 'latitudes'
      Return
   End If
! - read new lines until next date is reached -
   Do
      Read (Unit=iin, Fmt=*, Err=4, End=3) dum, ccmon
      If (get_month(ccmon) > 0) Exit
      rlt2 = dum
      nlt = nlt + 1
   End Do
!
! Check latitude ordering
3  If (nlt > 1) Then
      If (rlt1 > rlt2) Then
         ln2s = .true.
      Else
         ln2s = .false.
      End If
   Else
      ln2s = .true.
   End If
   ifail = 0
   Return
!
! Errors
4  ifail = 3
   Return
  End Subroutine get_gridded_dimensions_v9
!
!
!
  Subroutine get_gridded_latlons_v9 (iin, nlg, nlt, ln2s, rlng, rlat, ifail)
!
! Determines latitudes and longitudes in a version-9 gridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file reached
!    ifail =  3 Latitudes are not consecutive
!    ifail =  4 Duplicate latitudes
!
! Modules
   Use data_time_constants, Only: lmon
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
   Integer, Intent(In) :: nlg ! - number of longitudes -
   Integer, Intent(In) :: nlt ! - number of latitudes -
!
   Logical, Intent(In) :: ln2s ! - north to south latitudes? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: rlng ! - longitudes -
   Real(Kind=rp), Dimension(:), Intent(Out) :: rlat ! - latitudes -
!
! Locals
!
! Local scalars
   Integer :: i    ! - latitude index -
   Integer :: i1   ! - first latitude index -
   Integer :: i2   ! - last latitude index -
   Integer :: j    ! - longitude index -
   Integer :: idy  ! - day -
   Integer :: iyr  ! - year -
   Integer :: iinc ! - latitude increment -
!
   Character(Len=lmon) :: ccmon ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Read longitudes
   Rewind (Unit=iin)
   Read (Unit=iin, Fmt=*, Err=1, End=2) idy, ccmon, iyr, (rlng(j), j=1,nlg)
!
! Read latitudes
! - set latitude ordering -
   If (ln2s) Then
      i1 = 1
      i2 = nlt
      iinc = 1
   Else
      i1 = nlt
      i2 = 1
      iinc = -1
   End If
! - read latitudes -
   Do i = i1, i2, iinc
      Read (Unit=iin, Fmt=*, Err=1, End=2) rlat(i)
   End Do
! - check latitudes -
   ifail = 0
   If (Any(rlat(1:nlt-1)<rlat(2:nlt))) ifail = 3
   If (ifail == 3) Then
      If (Any(rlat(1:nlt-1) == rlat(2:nlt))) ifail = 4
      Return
   End If
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   Return
!
! - end of file -
2  ifail = 2
   Return
!
  End Subroutine get_gridded_latlons_v9
!
!
!
  Subroutine get_gridded_nt_v9 (iin, afile, ln2s, nlt, nlg, rlat, rlng, kav, dwk, ifail, cproc, cfail, pdlast)
!
! Determines numbers of cases in a gridded dataset
!
! On exit (if cproc = 'get_gridded_nt_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully
!    ifail =  2 Premature end of file reached. Data up to pdlast read successfully
!    ifail =  3 Inconsistent latitudes or longitudes (depending on cfail). Data up to pdlast read successfully
!    ifail =  4 Invalid date sequencing (as indicated by cfail)
!    ifail =  5 Problem reading date (possible incorrect number of latitudes). Data up to pdlast read successfully
!    ifail =  6 Problem allocating memory
!
! Modules
   Use data_time_constants, Only: lmon
   Use time,                Only: Assignment(=), &
                                  date_diff, get_month, get_sequence, valid_pdate
   Use missing,             Only: init_kav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
   Integer, Intent(In) :: nlt ! - number of latitudes -
   Integer, Intent(In) :: nlg ! - number of longitudes -
!
   Logical, Intent(In) :: ln2s ! - north to south latitudes? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - pointer arrays -
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! - workspace arrays -
   Real(Kind=rp), Dimension(:), Intent(InOut) :: dwk ! - workspace -
!
! Locals
!
! Local scalars
   Integer :: i    ! - latitude index -
   Integer :: j    ! - longitude index -
   Integer :: k    ! - case index -
   Integer :: dd   ! - date difference -
   Integer :: i1   ! - first latitude index -
   Integer :: i2   ! - last latitude index -
   Integer :: iinc ! - latitude increment -
!
   Character(Len=lmon) :: ccmon ! - current month -
!
   Type(pprd) :: prdp ! - previous date -
   Type(pprd) :: prdc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_gridded_nt_v9'
   cfail = ' '
   pdlast = 0
!
! Rewind to first date
   Rewind (Unit=iin)
!
! Read first date
   Read (Unit=iin, Fmt=*, Err=2, End=3) afile%prd1%sdate%idy, ccmon, afile%prd1%sdate%iyr
   afile%prd1%sdate%imn = get_month(ccmon)
   ifail = valid_pdate(afile%prd1%sdate)
   If (ifail /= 0) Then
      cproc = 'valid_pdate'
      Return
   End If
   afile%prd1%edate = afile%prd1%sdate
! - skip data -
   Do i = 1, nlt
      Read (Unit=iin, Fmt=*, Err=2, End=3)
   End Do
   pdlast = afile%prd1
   afile%nt = 1
!
! Read second date
   Read (Unit=iin, Fmt=*, Err=2, End=1) prdc%sdate%idy, ccmon, prdc%sdate%iyr, (dwk(j), j=1,nlg)
   prdc%sdate%imn = get_month(ccmon)
   ifail = valid_pdate(prdc%sdate)
   If (ifail /= 0) Then
      cproc = 'valid_pdate'
      Return
   End If
   prdc%edate=prdc%sdate
! - check for consistency of longitudes -
   If (Any(dwk(1:nlg) /= rlng(1:nlg))) Then
      ifail = 3
      cfail = 'Longitudes'
      Return
   End If
! - read latitudes -
   Do i = 1, nlt
      Read (Unit=iin, Fmt=*, Err=2, End=3) dwk(i)
   End Do
! - check for consistency of latitudes -
   If (ln2s) Then
      i1 = 1
      i2 = nlt
      iinc = 1
   Else
      i1 = nlt
      i2 = 1
      iinc = -1
   End If
   If (Any(dwk(i1:i2:iinc) /= rlat(1:nlt))) Then
      ifail = 3
      cfail = 'Latitudes'
      Return
   End If
   afile%nt = afile%nt + 1
!
! Identify date sequencing
   Call get_sequence (afile%prd1, prdc, afile%iseq, ifail)
   Select Case (ifail)
    Case (0)
      pdlast = prdc
    Case Default
      cproc = 'get_sequence'
      Return
   End Select
!
! Read subsequent dates
   get_nt: Do
      prdp = pdlast
      Read (Unit=iin, Fmt=*, Err=2, End=1) prdc%sdate%idy, ccmon, prdc%sdate%iyr, (dwk(j), j=1,nlg)
      prdc%sdate%imn = get_month(ccmon)
      If (prdc%sdate%imn == 0) Then
         ifail = 5
         Return
      End If
      prdc%edate=prdc%sdate
! - check for consistency of longitudes -
      If (Any(dwk(1:nlg) /= rlng(1:nlg))) Then
         ifail = 3
         cfail = 'Longitudes'
         Return
      End If
! - read latitudes -
      Do i = 1, nlt
         Read (Unit=iin, Fmt=*, Err=2, End=3) dwk(i)
      End Do
! - check for consistency of latitudes -
      If (Any(dwk(i1:i2:iinc) /= rlat(1:nlt))) Then
         ifail = 3
         cfail = 'Latitudes'
         Return
      End If
      pdlast = prdc
   End Do get_nt
! - determine total number of cases from difference between start and end dates -
1  afile%nt = date_diff(afile%prd1%sdate, pdlast%sdate, afile%iseq) + 1
!
! Assign memory for available cases flags
   Call init_kav (afile%nt, 1, 1, kav, ifail)
   Allocate (kav(afile%nt,1), Stat=ifail)
   If (ifail /= 0) Then
      ifail = 6
      Return
   End If
!
! Return if there is only one case
   afile%prdn = pdlast
   kav(1,1) = .true.
   If (afile%nt == 1) Return
!
! Rewind to second date
   Rewind (Unit=iin)
   Do k = 1, nlt+1
      Read (Unit=iin, Fmt=*, Err=2, End=3)
   End Do
!
! Check sequencing
   pdlast = afile%prd1
   dd = 1
   Do k = 2, afile%nt
      Select Case (dd) ! - identify whether to account for missing records -
! - read next record -
       Case (1)
         prdp = pdlast
         Read (Unit=iin, Fmt=*, Err=2, End=3) prdc%sdate%idy, ccmon, prdc%sdate%iyr
         Do i = 1, nlt
            Read (Unit=iin, Fmt=*, Err=2, End=3)
         End Do
         prdc%sdate%imn = get_month(ccmon)
         If (prdc%sdate%imn == 0) Then
            ifail = 5
            Return
         End If
         prdc%edate = prdc%sdate
         dd = date_diff(prdp%sdate, prdc%sdate, afile%iseq) ! - check whether next record is in sequence -
         If (dd == 1) Then
            kav(k,1) = .true.
            pdlast = prdc
         Else If (dd > 1) Then
            kav(k,1) = .false.
            Cycle
         Else
            ifail = 7
            Return
         End If
! - current record is missing, but prepare to read next record -
       Case (2)
         kav(k,1) = .true.
         pdlast = prdc
         dd = dd - 1
! - current record is missing -
       Case (3:)
         kav(k,1) = .false.
         pdlast = prdc
         dd = dd - 1
      End Select
   End Do
   ifail = 0
   Return
!
! Errors
! - problem reading file -
2  ifail = 1
   Return
!
! - end of file -
3  ifail = 2
   Return
!
  End Subroutine get_gridded_nt_v9
 End Subroutine get_gridded_v9
!
!
!
 Subroutine get_nongridded_v9 (iin, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, kav, ifail, cproc, cfail, pdlast)
!
! Determines structure of a version-9 gridded input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_nongridded_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
   Use data_cpt_constants, Only: id_stn
   Use data_io_constants,  Only: lstn
   Use time,               Only: pprd, &
                                 Assignment(=)
   Use space,              Only: reset_grids
   Use iofiles,            Only: ifile
   Use fields,             Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
   Type(field), Intent(InOut) :: afield ! - field -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - realigned used latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_nongridded_v9'
   cfail = ' '
   pdlast = 0
!
! Determine numbers of stations
   If (afile%idstr == id_stn) Then
      Call get_nongridded_dimensions_v9 (iin, afield%nlt, 4, afile%lmax, ifail)
   Else
      Call get_nongridded_dimensions_v9 (iin, afield%nlt, 2, afile%lmax, ifail)
   End If
   If (ifail /= 0) Then
      cproc = 'get_nongridded_dimensions_v9'
      If (afile%idstr == id_stn) Then
         cfail = 'stations'
      Else
         cfail = 'indices'
      End If
      Return
   End If
   afield%nlg = afield%nlt
!
! Read coordinates
   Call reset_grids (afile%idstr, 1, [afield%nlt], rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail)
   If (ifail /= 0) Then
      ifail = 1
      Return
   End If
   If (afile%idstr == id_stn) Then
      Call get_nongridded_latlons_v9 (iin, afield%nlt, cstn(:,1), ifail, &
           rlng=rlng(:,1), rlat=rlat(:,1))
   Else
      Call get_nongridded_latlons_v9 (iin, afield%nlt, cstn(:,1), ifail)
   End If
   If (ifail /= 0) Then
      cproc = 'get_nongridded_latlons_v9'
      Return
   End If
!
! Determine number of time steps and sequencing
! - station data -
   If (afile%idstr == id_stn) Then
      Call get_nongridded_nt_v9 (iin, 3, afile, kav, ifail, cproc, cfail, pdlast)
! - unreferenced data -
   Else
      Call get_nongridded_nt_v9 (iin, 1, afile, kav, ifail, cproc, cfail, pdlast)
   End If
   Return
!
 Contains
!
!
  Subroutine get_nongridded_dimensions_v9 (iin, nst, idat, lmax, ifail)
!
! Determines numbers of stations in a version-9 nongridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 File is too wide; data are probably truncated
!    ifail =  3 Unable to determine number of stations
!
! Modules
   Use data_numbers,      Only: cdigits
   Use data_io_constants, Only: lfli
   Use iofiles,           Only: max_width
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin  ! - input unit number -
   Integer, Intent(In) :: idat ! - first line of data -
!
! - output scalars -
   Integer, Intent(Out) :: nst   ! - number of stations -
   Integer, Intent(Out) :: lmax  ! - maximum length of line -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i   ! - station index -
   Integer :: inc ! - increment -
   Integer :: i1  ! - locator -
!
   Real(Kind=rp) :: dum ! - dummy argument -
!
   Character(Len=lfli) :: cline ! - line of input -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Scan, Verify
!
! Executable Statements
!
! Determine maximum length of line of data
   Rewind (Unit=iin)
   Call max_width (iin, lmax, ifail, &
        idat=idat, cline=cline)
   If (ifail == 1) Return
!
! Determine number of stations
   nst = 1
   inc = 64
   i1 = Scan(cline(1:lmax), cdigits)
   i1 = i1 + Verify(cline(i1+1:lmax), cdigits//'-/T:')
   Do
      Read (Unit=cline(i1:lmax), Fmt=*, Err=1, End=1) (dum, i=1,nst)
      nst = nst + inc
      Cycle
1     nst = nst - inc
      If (inc == 1) Exit
      inc = inc/8
   End Do
   If (nst == 0) ifail = 3
!
   Return
  End Subroutine get_nongridded_dimensions_v9
!
!
!
  Subroutine get_nongridded_latlons_v9 (iin, nst, cstn, ifail, &
             rlng, rlat)
!
! Determines latitudes and longitudes in a version-9 nongridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file reached
!
! Modules
   Use data_io_constants, Only: ldat
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
   Integer, Intent(In) :: nst ! - number of stations -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Character(Len=*), Dimension(:), Intent(Out) :: cstn ! - names of stations / indices -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rlng ! - longitudes -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: rlat ! - latitudes -
!
! Locals
!
! Local scalars
   Integer :: i ! - station index -
!
   Character(Len=ldat) :: ctag ! - tags -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Read stations
   Rewind (Unit=iin)
   Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (cstn(i), i=1,nst)
!
! Read latitudes and longitudes
   If (Present(rlat) .and. Present(rlng)) Then
      Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rlat(i), i=1,nst)
      Select Case (ctag(1:3))
       Case ('LAT', 'Lat', 'lat')
         Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rlng(i), i=1,nst)
       Case ('LON', 'Lon', 'lon', 'LNG', 'Lng', 'lng')
         rlng(:) = rlat(:)
         Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rlat(i), i=1,nst)
       Case Default
         ifail = 3
         Return
      End Select
   End If
   ifail = 0
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   Return
!
! - end of file -
2  ifail = 2
   Return
!
  End Subroutine get_nongridded_latlons_v9
!
!
!
  Subroutine get_nongridded_nt_v9 (iin, iskip, afile, kav, ifail, cproc, cfail, pdlast)
!
! Determines numbers of cases in a nongridded dataset
!
! On exit (if cproc = 'get_nongridded_nt_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully
!    ifail =  2 Premature end of file reached. Data up to pdlast read successfully
!    ifail =  3 Inconsistent latitudes or longitudes (depending on cfail). Data up to pdlast read successfully
!    ifail =  4 Invalid date sequencing (as indicated by cfail)
!    ifail =  5 Problem reading date (possible incorrect number of latitudes). Data up to pdlast read successfully
!    ifail =  6 Problem allocating memory
!
! Modules
   Use data_io_constants,   Only: lprd
   Use data_time_constants, Only: isq_mn, isq_sn
   Use time,                Only: date_diff, get_pdate, get_sequence
   Use missing,             Only: init_kav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input unit number -
   Integer, Intent(In) :: iskip ! - number of lines to skip -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - pointer arrays -
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Locals
!
! Local scalars
   Integer :: k  ! - case index -
   Integer :: dd ! - date difference -
!
   Character(Len=lprd) :: ctag1 ! - date tags -
   Character(Len=lprd) :: ctag2 ! - date tags -
!
   Type(pprd) :: prdp ! - previous date -
   Type(pprd) :: prdc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Len_Trim, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_nongridded_nt_v9'
   cfail = ' '
   pdlast = 0
!
! Rewind to first date
   Rewind (Unit=iin)
   If (iskip > 0) Then
      Do k = 1, iskip
         Read (Unit=iin, Fmt=*, Err=3, End=3)
      End Do
   End If
!
! Read first date
   afile%nt = 0
   Read (Unit=iin, Fmt='(A)', Err=3, End=3) ctag1
   ctag1 = AdjustL(ctag1)
   Call get_pdate (' ',Trim(ctag1),afile%prd1%sdate, ifail, &
        edate=afile%prd1%edate)
   If (ifail /= 0) Then
      cproc = 'get_pdate'
      Return
   End If
   pdlast = afile%prd1
   afile%nt = afile%nt + 1
!
! Read second date
   Read (Unit=iin, Fmt='(A)', Err=3, End=2) ctag1
   ctag1 = AdjustL(ctag1)
   Call get_pdate (' ', Trim(ctag1), prdc%sdate, ifail, &
        edate=prdc%edate)
   If (ifail /= 0) Then
      cproc = 'get_pdate'
      Return
   End If
   afile%nt = afile%nt + 1
!
! Identify date sequencing
   Call get_sequence (afile%prd1, prdc, afile%iseq, ifail)
   Select Case (ifail)
    Case (0)
      If (afile%iseq == isq_mn) Then ! - block monthly sequencing in version 9 files -
         ifail = 2
         cfail = 'Monthly'
         Return
      Else If (afile%iseq == isq_sn) Then ! - block seasonal sequencing in version 9 files -
         ifail = 2
         cfail = 'Seasonal'
         Return
      End If
    Case Default
      cproc = 'get_sequence'
      Return
   End Select
!
! Skip to last date
   pdlast = prdc
   Do
      Read (Unit=iin, Fmt='(A)', Err=1, End=1) ctag2
      If (Len_Trim(ctag2) == 0) Exit
      ctag1 = ctag2
   End Do
1  ctag1 = AdjustL(ctag1)
   Call get_pdate (' ', Trim(ctag1), prdc%sdate, ifail, &
        edate=prdc%edate)
   If (ifail /= 0) Return
   pdlast = prdc
   afile%prdn = prdc
! - determine total number of cases from difference between start and end dates -
   afile%nt = date_diff(afile%prd1%sdate, afile%prdn%sdate, afile%iseq) + 1
!
! Assign memory for available cases flags
2  Call init_kav (afile%nt, 1, 1, kav, ifail)
   If (ifail /= 0) Then
      ifail = 3
      Return
   End If
! - return if there is only one case -
   pdlast = afile%prd1
   kav(1,1) = .true.
   If (afile%nt == 1) Then
      afile%prdn = afile%prd1
      Return
   End If
!
! Rewind to second date
   Rewind (Unit=iin)
   If (iskip > 0) Then
      Do k = 1, iskip+1
         Read (Unit=iin, Fmt=*, Err=3, End=3)
      End Do
   End If
!
! Check sequencing
   dd = 1
   Do k = 2, afile%nt
      Select Case (dd) ! - identify whether to account for missing records -
! - read next record -
       Case (1)
         prdp = pdlast
         Read (Unit=iin, Fmt='(A)', Err=3, End=3) ctag1
         ctag1 = AdjustL(ctag1)
         Call get_pdate (' ', Trim(ctag1), prdc%sdate, ifail, &
              edate=prdc%edate)
         If (ifail /= 0) Then
            cproc = 'get_pdate'
            Return
         End If
         dd = date_diff(prdp%sdate, prdc%sdate, afile%iseq) ! - check whether next record is in sequence -
         If (dd == 1) Then
            kav(k,1) = .true.
            pdlast = prdc
         Else If (dd > 1) Then
            kav(k,1) = .false.
            Cycle
         Else
            ifail = 4
            Return
         End If
! - current record is missing, but prepare to read next record -
       Case (2)
         kav(k,1) = .true.
         pdlast = prdc
         dd = dd - 1
! - current record is missing -
       Case (3:)
         kav(k,1) = .false.
         pdlast = prdc
         dd = dd - 1
      End Select
   End Do
   afile%prdn = pdlast
   ifail = 0
   Return
!
! Errors
! - problem reading file -
3  ifail = 1
   Return
!
  End Subroutine get_nongridded_nt_v9
 End Subroutine get_nongridded_v9
!
!
!
 Subroutine get_structure_v10 (lms, lss, lbfile, rmiss, afile, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, &
            kav, ifd, ilf, igf, ifail, cproc, pdlast, &
            cfail)
!
! Determines structure of a version 10 input file, including its size, field settings, and missing cases
!
! On exit (if cproc = 'get_structure_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Unrecognized header line
!    ifail =  4 Unrecognised line
!    ifail =  5 Dates are not in sequence
!    ifail =  6 Problem allocating memory
!
! Modules
   Use data_cpt_constants,  Only: id_grd, id_stn, id_ind
   Use data_io_constants,   Only: cxmlns, iin, lstn, ltag
   Use data_time_constants, Only: isq_sn, isq_yr
   Use time,                Only: pprd, &
                                  Assignment(=), Operator(/=), &
                                  iseq, lslim, nse, nss, ssnlims
   Use space,               Only: get_geog, reset_grids
   Use iofiles,             Only: ifile
   Use fields,              Only: field, &
                                  init_field
   Use season,              Only: set_ssnlimits
   Use missing,             Only: init_kav
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
   Logical, Intent(In) :: lbfile ! - other file open? -
   Logical, Intent(In) :: lms    ! - permit monthly-sequenced files? -
   Logical, Intent(In) :: lss    ! - permit seasonally-sequenced files? -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - used longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - realigned used longitudes -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - names of stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - names of stations within domain -
!
   Logical, Dimension(:,:), Pointer :: kav ! - cases available? -
!
   Type(field), Dimension(:), Pointer :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifd   ! - field at error -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: igf   ! - category field at error -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc  ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail  ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Locals
!
! Local scalars
   Integer :: i1     ! - locator -
   Integer :: igrd   ! - data structure indicator -
   Integer :: iseqbk ! - backup time sequence indicator -
   Integer :: isq1   ! - first time-sequencing -
   Integer :: nl     ! - number lagged fields -
   Integer :: nlh    ! - number of header lines -
   Integer :: nt     ! - number of time steps -
   Integer :: ntn    ! - number of time steps -
   Integer :: ntag   ! - number of tag lines -
   Integer :: ny1    ! - number of cases in first year -
!
   Character(Len=ltag) :: cline ! - line -
!
   Logical :: lfend ! - end of file? -
!
   Type(field) :: tfield ! - field information -
!
   Type(pprd) :: ssnlims_bk ! - backup season limits -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Associated, Index, Len, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_structure_v10'
   cfail = ' '
   pdlast = 0
   ifd = 0
   ilf = 0
   igf = 0
   ifail = 1
!
! Initialise
   If (Associated(afield)) Nullify (afield)
   afile%nfs = 1
   nlh = 0
   afile%ntag = 0
   afile%nfs = 1
   afile%ngs = 1
!
! Read header lines
   headers: Do
      Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
      nlh = nlh + 1
!
! Read XML namespace headers
      If (Index(cline,cxmlns//':') > 0) Then
         afile%ntag = afile%ntag + 1
!
! Read number of fields
      Else If (Index(cline, 'cpt:nfields=') > 0) Then
         i1 = Index(cline, 'cpt:nfields=') + Len('cpt:nfields=')
         Read (Unit=cline(i1:), Fmt=*, Err=4) afile%nfs
         afile%ntag = afile%ntag + 1
!
! Read number of categories
      Else If (Index(cline, 'cpt:ncats=') > 0) Then
         i1 = Index(cline, 'cpt:ncats=') + Len('cpt:ncats=')
         Read (Unit=cline(i1:), Fmt=*, Err=4) afile%ngs
         afile%ntag = afile%ntag + 1
!
! First field
      Else If (Index(cline, 'cpt:field=') > 0) Then
         Exit headers
!
! Skip unused tags
      Else If (Index(cline, 'cpt:') > 0) Then
         afile%ntag = afile%ntag + 1
!
! Unrecognised lines
      Else
         ifail = 4
         GoTo 4
      End If
   End Do headers
!
! Determine structure of dataset
   Call read_tags (Trim(cline)//' ', 4, ['col    ', 'row    ', 'ncol   ', 'nrow   '], .false., tfield, ifail, cfail, &
        idstr=igrd, nt=nt)
   If (ifail /= 0) Then
      cproc = 'read_tags'
      GoTo 4
   End If
! - disable multiple fields for unreferenced data -
   afile%idstr = igrd
   afile%igeog = get_geog(afile%idstr)
!
! Backup settings for sub-seasonal data
   iseqbk = iseq
   If (iseq > 0) Then
      iseq = 0
      If (lbfile) Then
         ssnlims_bk = ssnlims
         ssnlims = 0
      End If
   End If
   lslim = .false.
!
! Check for stacked fields and lagged-fields
   Select Case (afile%idstr)
! - gridded fields -
    Case (id_grd)
      Call get_gridded_nls_v10 (iin, lms, lss, afile, isq1, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
! - non-gridded fields -
    Case (id_stn, id_ind)
      Call get_nongridded_nls_v10 (iin, lms, lss, nt, afile, ntag, isq1, ifail, cproc, pdlast)
   End Select
   If (ifail /= 0) GoTo 3
   cproc = 'get_structure_v10'
!
! Reassign seasonal sequencing
   Call reassign_seas_seq (lss, isq1, afile)
!
! Initialise fields
   Call init_field (afile%nfl*afile%nse, [rmiss], afield, ifail)
   If (ifail /= 0) Then
      ifail = 5
      GoTo 3
   End If
!
! Determine season limits
   iseq = afile%iseq
   If (afile%iseq > 0 .and. afile%nse > 1) Then
      Call set_ssnlimits (afile%prd1%sdate, afile%nse, afile%iseq, pdlast)
      ssnlims = pdlast
      If (lbfile) Then
#if FTN95 == 1
         If (diff_pprds(ssnlims,ssnlims_bk)) Then ! FTN95 BUG
#else
         If (ssnlims /= ssnlims_bk) Then
#endif
            ifail = 6
            GoTo 3
         End If
      End If
   Else
      nse = 1
   End If
!
! Get fields and maximum number of cases
   Select Case (afile%idstr)
! - gridded fields -
    Case (id_grd)
      Call get_gridded_fields_v10 (iin, rmiss, afile, afield, lfend, ntn, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
      If (afile%lstack) Then
         nl = 0
      Else
         nl = afile%nls
      End If
! - non-gridded fields -
    Case (id_stn, id_ind)
      Call get_nongridded_fields_v10 (iin, ntag, rmiss, afile, afield, lfend, ntn, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
      nl = 0
   End Select
   If (ifail /= 0) GoTo 3
   cproc = 'get_structure_v10'
!
! Calculate number of cases
   Call calculate_nt (afile%iseq, afile%lstack, afile%nls, afield(1)%tssn%tprd(1)%sdate, afile%prdn%sdate, ntn, nl, afile%nt, &
        afile%ntm, ifail)
   If (ifail /= 0) Then
      ifail = 7
      GoTo 3
   End If
   If (afile%nt == 1 .and. afile%iseq < 0 .and. .not.lss) afile%iseq = isq_yr
!
! Initialise latitudes and longitudes
   Select Case (afile%idstr)
! - gridded fields -
    Case (id_grd)
      Call reset_grids (afile%idstr, afile%nfl*afile%nse, afield(:)%nlt, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
           nlg=afield(:)%nlg)
! - non-gridded fields -
    Case (id_stn, id_ind)
      Call reset_grids (afile%idstr, afile%nfl*afile%nse, afield(:)%nlt, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail)
   End Select
   If (ifail /= 0) Then
      ifail = 5
      GoTo 3
   End If
!
! Initialise available cases flags
   Call init_kav (afile%ntm, afile%nfl, nss, kav, ifail)
   If (ifail /= 0) GoTo 3
!
! Determine number of cases per year
   Select Case (afile%iseq)
    Case Default
      ny1 = 1
    Case (1:)
      ny1 = afile%nse
    Case (isq_sn)
      ny1 = afile%nls
   End Select
!
! Get latitudes and longitudes, and identify available cases
   Select Case (afile%idstr)
! - gridded fields -
    Case (id_grd)
      Call get_gridded_info_v10 (iin, ny1, lss, lfend, afile, afield, kav, rlat, rlng, rlatd, rlngd, ifail, &
           ifd, ilf, igf, cproc, cfail, pdlast)
!
! Get station/index names and coordinates, and identify available cases
    Case (id_stn, id_ind)
      Call get_nongridded_info_v10 (iin, ntag, ny1, lss, lfend, afield, afile, kav, rlat, rlng, cstn, rlatd, ifail, &
           ifd, ilf, igf, cproc, cfail, pdlast)
   End Select
   If (ifail /= 0) GoTo 3
   iseq = iseqbk
   Select Case (afile%idstr)
    Case (id_grd)
      afile%lmax = 0
    Case (id_stn, id_ind)
      afile%ntag = ntag
   End Select
   cproc = ' '
!
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   GoTo 4
!
! - end of file -
2  ifail = 2
   GoTo 4
!
! Restore sequencing
3  iseq = iseqbk
   If (iseq > 0 .and. lbfile) ssnlims = ssnlims_bk
!
! - other errors -
4  If (afile%nfs == 1) ifd = 0
   If (afile%nls <= 1) ilf = 0
   If (afile%ngs == 1) igf = 0
!
   Return
 End Subroutine get_structure_v10
!
!
!
 Subroutine get_gridded_nls_v10 (iin, lms, lss, afile, isq1, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines stacking of any multiple fields, and number of lagged-fields or sub-seasons in a version-10 gridded input file
!
! On exit (if cproc = 'get_gridded_nls_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Category fields are not consecutive
!
! Modules
   Use data_numbers,        Only: zero
   Use data_time_constants, Only: isq_mn
   Use time,                Only: pprd, &
                                  Assignment(=), Operator(/=), &
                                  ims, mpsn
   Use iofiles,             Only: ifile
   Use fields,              Only: field, &
                                  Operator(/=), &
                                  set_field
   Use season,              Only: len_ssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
   Logical, Intent(In) :: lms ! - permit monthly seasonally-sequenced files? -
   Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
   Integer, Intent(Out) :: ifd   ! - field at error -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: igf   ! - category field at error -
   Integer, Intent(Out) :: isq1  ! - first time sequencing -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Locals
!
! Local scalars
   Type(field) :: fieldc ! - current field -
   Type(field) :: fieldf ! - first field -
   Type(field) :: fieldn ! - new field -
   Type(field) :: fieldp ! - previous field -
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_gridded_nls_v10'
   ifd = 0
   ilf = 0
   igf = 0
   isq1 = 0
   cfail = ' '
   pdlast = 0
   afile%nls = 0
   afile%nse = 0
   Call set_field (zero, fieldc)
   Call set_field (zero, fieldp)
!
! Rewind to first date
   Call read_headers (iin, afile%ntag, .true., ifail)
   If (ifail /= 0) Return
!
! Read first blocks
   ifd = 1
   ilf = 1
   igf = 1
   Call get_gridded_block_v10 (iin, igf, .true., fieldc, ifail, cproc, cfail)
   If (ifail /= 0) GoTo 1
   fieldp = fieldc
   fieldf = fieldc
   afile%prd1 = fieldc%tssn%tprd(1)
   pdlast = fieldc%tssn%tprd(1)
!
! Read additional categories
   If (afile%ngs > 1) Then
      Do igf = 2, afile%ngs
         Call get_gridded_block_v10 (iin, igf, .false., fieldc, ifail, cproc, cfail, &
              bfield=fieldp)
         Select Case (ifail)
          Case (0)
            Continue
          Case (2)
            If (igf < afile%ngs) GoTo 1
          Case Default
            GoTo 1
         End Select
! - check for category ordering -
         If (fieldc%icat(igf) /= igf) Then
            cproc = 'get_gridded_nls_v10'
            ifail = 4
            GoTo 1
! - check for consistency -
         Else If (fieldc /= fieldp .or. fieldc%tssn%tprd(1) /= fieldp%tssn%tprd(1)) Then ! - check for consistency of fields -
            cproc = 'get_gridded_nls_v10'
            ifail = 5
            GoTo 1
         End If
      End Do
   End If
   igf = 1
   If (lss) Then
      ims(ilf) = fieldc%tssn%tprd(1)%sdate%imn
      mpsn(ilf) = len_ssn(fieldc%tssn%tprd(1)%sdate, fieldc%tssn%tprd(1)%edate, isq_mn)
   End If
!
! Read subsequent blocks
   afile%nt = 0
   afile%nls = 1
   afile%nse = 1
   get_nls: Do
      Call get_gridded_block_v10 (iin, igf, .false., fieldc, ifail, cproc, cfail, &
           bfield=fieldp)
      Select Case (ifail)
       Case (0)
         Continue
       Case (-1) ! - allow for incomplete field information on subsequent fields -
         ifail = 0
       Case (2) ! - check for end of file -
         If (ifd == afile%nfs) Then
            fieldc = fieldp
            afile%nt = 1
            afile%lensemble = .false.
            afile%lstack = .true.
            cproc = 'get_gridded_nls_v10'
            Exit get_nls
         Else
            Return
         End If
       Case Default
         If (afile%nfs > 1) ifd = ifd + 1
         If (afile%nls > 1) ilf = ilf + 1
         If (afile%ngs > 1) igf = igf + 1
         Return
      End Select
      pdlast = fieldc%tssn%tprd(1)
!
! Check for stacked fields
      If (ifd == 1 .and. ilf == 1 .and. afile%nse == 1) Then
         If (afile%nls == 1) Then
            If (afile%nfs > 1) Then
               If (fieldp%member == fieldc%member) Then
                  afile%lensemble = .false.
                  If (fieldp%var /= fieldc%var .or. fieldp%z%hght /= fieldc%z%hght) Then
                     afile%lstack = .false.
                  Else
                     afile%lstack = .true.
                  End If
               Else
                  afile%lensemble = .true.
                  afile%lstack = .false.
               End If
            Else
               afile%lensemble = .false.
               afile%lstack = .true.
            End If
         End If
      End If
!
! Skip remaining categories
      If (afile%ngs > 1) Then
         fieldn = fieldc
         Do igf = 2, afile%ngs
            Call get_gridded_block_v10 (iin, igf, .false., fieldc, ifail, cproc, cfail, &
                 bfield=fieldn)
            If (ifail /= 0) Return
            pdlast = fieldc%tssn%tprd(1)
         End Do
         igf = 1
      End If
!
! Skip remaining unstacked fields
      If (.not.afile%lstack) Then
         If (afile%nfs > 2) Then
            Do ifd = 3, afile%nfs
               Do igf = 1, afile%ngs
                  Call get_gridded_block_v10 (iin, igf, .false., fieldc, ifail, cproc, cfail, &
                       bfield=fieldp)
                  If (ifail /= 0) Return
                  pdlast = fieldc%tssn%tprd(1)
               End Do
            End Do
            igf = 1
         End If
! - read next time step if necessary -
         Call get_gridded_block_v10 (iin, igf, .true., fieldc, ifail, cproc, cfail, &
              bfield=fieldp)
         Select Case (ifail)
          Case (0)
            Continue
          Case (2) ! - check for end of file -
            fieldc = fieldp
            afile%nt = 1
            cproc = 'get_gridded_nls_v10'
            Exit get_nls
          Case Default
            Return
         End Select
      End If
!
! Identify date-sequencing
      get_seq: Do
         Call determine_sequence (fieldp%tssn%tprd(1), fieldc%tssn%tprd(1), lms, lss, (isq1==0), isq1, afile%iseq, &
              afile%nls, afile%nse, pdlast, ifail, &
              ilf=ilf)
         Select Case (ifail)
          Case (0)
            Exit get_nls
          Case (1)
            Cycle get_nls
          Case (2)
            fieldp = fieldf
            If (isq1 > 0 .and. afile%nfs > 1) Then ! - check for multi-field sub-seasonal sequencing -
               ifd = ifd + 1
               Cycle get_nls
            Else
               Cycle get_seq
            End If
          Case Default
            cproc = 'get_sequence'
            Return
         End Select
      End Do get_seq
   End Do get_nls
!
   cproc = ' '
   ifail = 0
   Return
!
! Errors
1  If (afile%ngs > 1) igf = igf - 1
!
   Return
 End Subroutine get_gridded_nls_v10
!
!
!
 Subroutine get_gridded_fields_v10 (iin, rmiss, afile, afield, lfend, n, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines field settings and number of cases
!
! On exit (if cproc = 'get_gridded_fields_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  3 Season for last date of ifd field, ilf lagged-field is inconsistent with earlier dates
!    ifail =  4 Climatological probability for the igf category, ifd field, ilf lagged-field is inconsistent with earlier dates
!    ifail =  5 Information for last date of ifd field, ilf lagged-field is inconsistent with earlier information
!
! Modules
   Use data_time_constants, Only: isq_sn, isq_yr
   Use labels,              Only: cg_na
   Use time,                Only: pprd, &
                                  Assignment(=), Operator(==), Operator(/=), &
                                  check_pprd, get_cssn
   Use iofiles,             Only: ifile
   Use fields,              Only: field, &
                                  Operator(==), &
                                  set_field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - input/output arrays -
   Type(field), Dimension(:), Intent(InOut) :: afield ! - field information -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
   Integer, Intent(Out) :: ifd   ! - field at error -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: igf   ! - category field at error -
   Integer, Intent(Out) :: n     ! - number of read time steps -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Logical, Intent(Out) :: lfend ! - end of file? -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! Locals
!
! Local scalars
   Integer :: l   ! - field/lagged-field index -
   Integer :: ll  ! - field/lagged-field index -
   Integer :: nsl ! - number of sub-seasons or lagged seasons -
!
   Type(field) :: tfield ! - temporary field information -
   Type(field) :: pfield ! - previous field information -
   Type(field) :: nfield ! - new information -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any, Len_Trim, MaxVal, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_gridded_fields_v10'
   ifd = 0
   ilf = 0
   igf = 0
   cfail = ' '
   pdlast = 0
   Call set_field (rmiss, tfield)
   Call set_field (rmiss, pfield)
!
! Rewind to first date
   Call read_headers (iin, afile%ntag, .true., ifail)
   If (ifail /= 0) Return
!
! Read first stacked fields, lagged fields, and category fields
   afile%prdn = afile%prd1
   If (afile%lstack) Then
      nsl = afile%nse*afile%nls
      afile%nt = 1
      Do ifd = 1, afile%nfs
         Select Case (afile%iseq)
          Case Default
            n = 1
          Case (isq_sn, 1:)
            n = nsl
         End Select
         Do ilf = 1, nsl
            l = (ifd - 1)*nsl + ilf
            If (ifd == 1 .or. ilf /= 1) Then
               Call next_block (ifail)
               If (ifail /= 0) Return
            End If
         End Do
         pfield = afield(l)
!
! Continue to read blocks until new stacked field is found
         new_stack: Do
            Do ilf = 1, nsl
               l = (ifd - 1)*nsl + ilf
               igf = 1
               Call get_gridded_block_v10 (iin, igf, .true., tfield, ifail, cproc, cfail, &
                    bfield=afield(l))
               Select Case (ifail)
                Case (0)
                  Continue
                Case (2)
                  If (ifd < afile%nfs .or. Trim(cproc) /= 'get_gridded_block_v10') Then
                     Return
                  Else
                     cproc = 'get_gridded_fields_v10'
                     ifail = 1
                     Exit new_stack
                  End If
                Case Default
                  Return
               End Select
               tfield%cssn%cssn(1) = get_cssn(tfield%tssn%tprd(1), 1, [1])
! - check for changes in field variable -
               If (tfield%member /= afield(l)%member .or. tfield%var /= afield(l)%var .or.  &
                   tfield%z%hght /= afield(l)%z%hght .or. tfield%model /= afield(l)%model) Then
                  If (ifd < afile%nfs) Then
                     l = ifd*nsl + 1
                     afield(l) = tfield
                     Exit new_stack
                  Else ! - error if already at last field -
                     cproc = 'get_gridded_fields_v10'
                     ifail = 2
                     Return
                  End If
! - check for lagged fields as separate fields -
               Else If (afile%iseq == isq_yr .and. tfield%cssn%cssn(1) /= afield(l)%cssn%cssn(1)) Then
                  If (l < nsl) Then
                     l = l + 1
                     Cycle
                  Else If (ifd < afile%nfs) Then
                     l = ifd*nsl + 1
                     afield(l) = tfield
                     Exit new_stack
                  Else ! - error if season is inconsistent for last field -
                     cproc = 'get_gridded_fields_v10'
                     ifail = 3
                     Return
                  End If
! - check for identical fields -
               Else If (tfield%var == afield(l)%var .and. tfield%tssn%tprd(1) == afield(l)%tssn%tprd(1)) Then
                  If (ifd < afile%nfs) Then
                     l = ifd*nsl + 1
                     afield(l) = tfield
                     Exit new_stack
                  End If
               End If
! - check for consistency of climatological probabilities -
               If (afile%ngs > 1) Then
                  If (tfield%prob(igf) /= afield(l)%prob(igf)) Then
                     cproc = 'get_gridded_fields_v10'
                     igf = afile%ngs
                     ifail = 4
                     Return
                  End If
               End If
! - check for consistency of periods -
               ifail = check_pprd(afield(l)%cssn%cssn(1), tfield%cssn%cssn(1), afield(l)%tssn%tprd(1), tfield%tssn%tprd(1), &
                                  afile%iseq)
               If (ifail /= 0) Then
                  cproc = 'check_pprd'
                  Return
               End If
               pdlast = tfield%tssn%tprd(1)
               pfield = tfield
               n = n + 1
! - read additional categories -
               If (afile%ngs > 1) Then
                  Do igf = 2, afile%ngs
                     Call get_gridded_block_v10 (iin, igf, .false., tfield, ifail, cproc, cfail, &
                          bfield=afield(l))
                     If (ifail /= 0) Return
! - check for consistency of climatological probabilities -
                     If (tfield%prob(igf) /= afield(l)%prob(igf)) Then
                        cproc = 'get_gridded_fields_v10'
                        ifail = 4
                        Return
                     End If
                  End Do
                  tfield%tssn%tprd(1) = pdlast
               End If
               If (ifd == 1) afile%prdn = tfield%tssn%tprd(1)
            End Do
         End Do new_stack
         pdlast = tfield%tssn%tprd(1)
         igf = 1
!
! Check whether last case is for last lagged-field
         If (n > 1) Then
            lfend = .false.
            tfield = pfield
            If (afile%iseq == isq_yr) Then
               Call check_last (nsl, (ifd - 1)*nsl, tfield%tssn%tprd(1), afield(:), lfend, ilf, ifail)
               If (ifail /= 0) Then
                  cproc = 'get_gridded_fields_v10'
                  ifail = 5
                  Return
               End If
            End If
         Else
            ifail = 0
            lfend = .false.
         End If
      End Do
!
! Read first unstacked fields, lagged-fields, and category fields
   Else
      Do ifd = 1, afile%nfs
         Do ilf = 1, afile%nls
            l = (ifd - 1)*afile%nls + ilf
            Call next_block (ifail)
            If (ifail /= 0) Return
         End Do
      End Do
!
! Read to end of file
      n = 0
      file_end: Do
         Do ilf = 1, afile%nls
            Do ifd = 1, afile%nfs
               l = (ifd - 1)*afile%nls + ilf
               Do igf = 1, afile%ngs
                  If (l == 1) Then
                     Call get_gridded_block_v10 (iin, igf, .true., tfield, ifail, cproc, cfail, &
                          bfield=afield(l))
                  Else
                     If (afield(l)%tssn%tprd(1) /= afield(l-1)%tssn%tprd(1)) Then
                        Call get_gridded_block_v10 (iin, igf, .false., tfield, ifail, cproc, cfail, &
                             bfield=afield(l))
                     Else
                        nfield = afield(l)
                        nfield%tssn%tprd(1) = tfield%tssn%tprd(1)
                        Call get_gridded_block_v10 (iin, igf, .false., tfield, ifail, cproc, cfail, &
                             bfield=nfield)
                     End If
                  End If
                  Select Case (ifail)
                   Case (0)
                     pdlast = tfield%tssn%tprd(1)
                     pfield = tfield
                     n = n + 1
                   Case (2)
                     cproc = 'get_gridded_fields_v10'
                     If (igf == afile%ngs) Then
                        ifail = 0
                        Exit file_end
                     Else
                        ifail = 1
                        Return
                     End If
                   Case Default
                     Return
                  End Select
                  If (afile%ngs > 1) Then ! - read additional categories -
                     If (tfield%prob(igf) /= afield(l)%prob(igf)) Then ! - check for consistency of climatological probabilities -
                        cproc = 'get_gridded_fields_v10'
                        ifail = 4
                        Return
                     End If
                  End If
                  If (ilf == 1 .and. ifd == 1 .and. igf == 1) afile%prdn = tfield%tssn%tprd(1)
               End Do
            End Do
         End Do
      End Do file_end
!
! Check whether last case is for last unstacked field
      igf = 1
      tfield = pfield
      lfend = .false.
      Do ifd = afile%nfs, 1, -1
         l = ifd*afile%nls
         If (tfield%member == afield(l)%member .or. tfield%var /= afield(l)%var .or. tfield%z%hght /= afield(l)%z%hght) Then
            Exit
         Else If (ifd > 1) Then
            lfend = .true.
         Else
            cproc = 'get_gridded_fields_v10'
            ifail = 5
            Return
         End If
      End Do
!
! Check whether last case is for last lagged-field
      If (.not.lfend) Then
         If (afile%iseq == isq_yr .and. n > 0) Then
            Call check_last (afile%nls, 0, tfield%tssn%tprd(1), afield(:), lfend, ilf, ifail)
            If (ifail /= 0) Then
               cproc = 'get_gridded_fields_v10'
               ifail = 5
               Return
            End If
         End If
      Else
         ifail = 0
      End If
   End If
!
! Identify number of models
   afile%nms = 1
   If (afile%nfl > 1) Then
      Do l = 2, afile%nfl
         If (afield(l)%model /= cg_na .and. afield(l)%model /= 'N/A' .and. Len_Trim(afield(l)%model) > 0) Then
            If (Any(afield(1:l-1)%model == afield(l)%model)) Cycle
            afile%nms = afile%nms + 1
         Else ! - assume identical fields are different models -
            Do ll = 1, l-1
               If (afield(l) == afield(ll)) Then
                  afile%nms = afile%nms + 1
                  Exit
               End If
            End Do
         End If
      End Do
   End If
!
! Identify number of ensemble members
   If (afile%lensemble) Then
      afile%nem = MaxVal(afield(:)%member)
   Else
      afile%nem = 0
   End If
!
   cproc = ' '
   ifail = 0
   Return
!
 Contains
!
!
  Subroutine next_block (ifail)
!
! Reads next block
!
! Modules
   Use data_numbers, Only: zero, one, oneh, tolp
   Use maths,        Only: approx
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: sp ! - sum of probabilities -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Read next block
   igf = 1
   If (l == 1) Then
      Call get_gridded_block_v10 (iin, igf, .true., afield(l), ifail, cproc, cfail)
   Else
      Call get_gridded_block_v10 (iin, igf, .false., afield(l), ifail, cproc, cfail, &
           bfield=afield(l-1))
   End If
   If (ifail /= 0) Return
   afield(l)%cssn%cssn(1) = get_cssn(afield(l)%tssn%tprd(1), 1, [1])
   pdlast = afield(l)%tssn%tprd(1)
   If (afile%ngs > 1) Then
      Do igf = 2, afile%ngs
         Call get_gridded_block_v10 (iin, igf, .false., tfield, ifail, cproc, cfail, &
              bfield=afield(l))
         If (ifail /= 0) Return
         afield(l)%icat(igf) = tfield%icat(igf)
         afield(l)%prob(igf) = tfield%prob(igf)
      End Do
      sp = Sum(afield(l)%prob(:))
      If (.not.(approx(     sp, zero, utol=tolp) .or. &
                approx(     sp,  one, utol=tolp) .or. &
                approx(sp/oneh,  one, utol=tolp))) Then
         ifail = 6
         cproc = 'get_gridded_fields_v10'
         igf = 1
         Return
      End If
   End If
!
   Return
  End Subroutine next_block
 End Subroutine get_gridded_fields_v10
!
!
!
 Subroutine get_gridded_info_v10 (iin, ny1, lss, lfend, afile, afield, kav, rlat, rlng, rlatd, rlngd, ifail, &
            ifd, ilf, igf, cproc, cfail, pdlast)
!
! Reads latitudes and longitudes, checking for consistency of fields. Identifies available cases
!
! On exit (if cproc = 'get_gridded_info_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!    ifail =  3 Unable to read latitudes and/or longitudes (as indicated by cfail) for ifd field, ilf lagged-field
!    ifail =  4 Dates are not in sequence. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field
!
! Modules
   Use time,    Only: pprd, &
                      Assignment(=), Operator(/=)
   Use iofiles, Only: ifile
   Use fields,  Only: field, &
                      init_field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
   Integer, Intent(In) :: ny1 ! - number of cases in first year -
!
   Logical, Intent(In) :: lfend ! - end of file? -
   Logical, Intent(In) :: lss   ! - permit seasonally-sequenced files? -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - input/output arrays -
   Type(field), Dimension(:), Intent(InOut) :: afield ! - field information -
!
   Logical, Dimension(:,:), Intent(InOut) :: kav ! - cases available? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
   Integer, Intent(Out) :: ifd   ! - field at error -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: igf   ! - category field at error -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last date read successfully -
!
! - workspace arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: rlatd ! - test latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: rlngd ! - test longitudes -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: rlng ! - longitudes -
!
! Locals
!
! Local scalars
   Integer :: k      ! - case index -
   Integer :: kl     ! - year index -
   Integer :: l      ! - field/lagged-field index -
   Integer :: ll     ! - field/lagged-field index (ignoring seasons) -
   Integer :: ls     ! - field/lagged-field/sub-season index -
   Integer :: ifd_bk ! - backup field index -
   Integer :: ilf_bk ! - backup lagged-field index -
!
!
   Logical :: lread ! - read current case? -
!
   Type(pprd) :: pdlastt ! - last period read successfully -
!
! Local arrays
   Type(pprd), Dimension(afile%nfl*afile%nse) :: prdps ! - last successfully read periods -
!
   Type(field), Dimension(:), Pointer :: tfield ! - temporary field information -
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_gridded_info_v10'
   ifd = 0
   ilf = 0
   igf = 0
   l = 0
   cfail = ' '
   pdlast = 0
   pdlastt = 0
!
! Set initial dates
   prdps(:) = afield(:)%tssn%tprd(1)
!
! Initialise temporary field
   Nullify (tfield)
   Call init_field (1, afield(:)%rmiss, tfield, ifail)
!
! Rewind
   Call read_headers (iin, afile%ntag, .true., ifail)
   Select Case (ifail)
    Case (0)
      Continue
    Case (1) ! - problem reading file -
      GoTo 2
    Case (2) ! - premature end of file -
      GoTo 3
   End Select
!
! Read latitudes and longitudes for stacked fields
   If (afile%lstack) Then
      Do ifd = 1, afile%nfs
         lread = .true.
         kl = 0
         Do k = 1, afile%ntm
            Do ilf = 1, afile%nls
               l = (ifd - 1)*afile%nls + ilf
               Call set_tf_counters (afile%iseq, afile%nse, .true., k, l, kl, ll, ls)
               Call advance_grid (.true., lread, igf, ifail)
               Select Case (ifail)
                Case (0)
                  Continue
                Case (1)
                  GoTo 1
                Case (2)
                  GoTo 4
               End Select
            End Do
         End Do
      End Do
!
! Read latitudes and longitudes for unstacked fields
   Else
      lread = .true.
      kl = 0
      Do k = 1, afile%ntm
         Do ilf = 1, afile%nls
            Do ifd = 1, afile%nfs
               l = (ifd - 1)*afile%nls + ilf
               Call set_tf_counters (afile%iseq, afile%nse, (ifd == 1), k, l, kl, ll, ls)
               If (ls == 1) Then
                  Call advance_grid (.true., lread, igf, ifail)
               Else
                  Call advance_grid ((afield(ls)%tssn%tprd(1) /= afield(ls-1)%tssn%tprd(1)), lread, igf, ifail)
               End If
               Select Case (ifail)
                Case (0)
                  Continue
                Case (1)
                  GoTo 1
                Case (2)
                  GoTo 4
               End Select
            End Do
         End Do
      End Do
   End If
!
! Invert latitudes of south to north fields
1  Do l = 1, afile%nfl
      If (.not.afield(l)%ln2s) Then
         rlatd(afield(l)%nlt:1:-1,l) = rlat(1:afield(l)%nlt,l)
         rlat(1:afield(l)%nlt,l) = rlatd(1:afield(l)%nlt,l)
      End If
   End Do
   If (lread) Then
      cproc = ' '
      ifail = 0
   Else
      cproc = 'get_gridded_info_v10'
      ifail = 4
      GoTo 4
   End If
!
   Return
!
! Errors
! - problem reading file -
2  ifail = 1
   GoTo 4
!
! - end of file -
3  If (lfend .and. l == afile%nfl) Then
      ifail = 0
      Return
   Else
      ifail = 2
   End If
!
! Identify last date read
4  If (.not.lread) Then
      ifd = ifd_bk
      ilf = ilf_bk
   End If
   igf = igf - 1
   If (igf == 0) Then
      igf = afile%ngs
      If (afile%lstack) Then
         ilf = ilf - 1
         If (ilf == 0) Then
            ilf = afile%nls
            If (k == 1) Then
               ifd = ifd - 1
               If (ifd == 0) Then
                  ilf = 0
                  igf = 0
              End If
            End If
         End If
      Else
         ifd = ifd - 1
         If (ifd == 0) Then
            ifd = afile%nfs
            ilf = ilf - 1
            If (ilf == 0) Then
               If (k > 1) Then
                  ilf = afile%nls
               Else
                  ifd = 0
                  ilf = 0
                  igf = 0
               End If
            End If
         End If
      End If
   End If
   rlat(:,:) = afield(1)%rmiss
   rlng(:,:) = afield(1)%rmiss
!
   Return
!
 Contains
!
!
  Subroutine advance_grid (lfirst, lread, igf, ifail)
!
! Arguments
!
! Modules
   Use space, Only: check_latitude_ordering
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lfirst ! - first? -
!
! - input/output scalars -
   Logical, Intent(InOut) :: lread ! - read current case? -
!
! - output scalars -
   Integer, Intent(Out) :: igf   ! - category field at error -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Type(field) :: nfield ! - new field information -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Real
!
! Executable Statements
!
! Get the next gridded block
   If (lread) Then
      Do igf = 1, afile%ngs
         If (igf == 1 .and. lfirst) Then
            Call get_gridded_block_v10 (iin, igf, .false., tfield(1), ifail, cproc, cfail, &
                 bfield=afield(ls), rlat=rlatd(:,ls), rlng=rlngd(:,ls))
         Else
            If (.not.afile%lstack) Then
               nfield = afield(ls)
               nfield%tssn%tprd(1) = tfield(1)%tssn%tprd(1)
            End If
            Call get_gridded_block_v10 (iin, igf, .false., tfield(1), ifail, cproc, cfail, &
                 bfield=nfield, rlat=rlatd(:,ls), rlng=rlngd(:,ls))
         End If
!
! Check for errors
         Select Case (ifail)
          Case (0)
            Continue
! - allow for end of file -
          Case (2)
            If (lss .and. k >= Ceiling(Real(afile%nt)/Real(afile%nls)) .and. igf == 1) Then
               ifail = 1
               Return
! - adjust number of cases if data are subseasonal and are for part of the year only -
            Else If (afile%iseq > 0 .and. igf == 1) Then
               afile%nt = k
               ifail = 1
               Return
            Else
               Return
            End If
          Case Default
            Return
         End Select
!
! Check latitude ordering
         If (kl <= ny1 .and. igf == 1) Then
            Call check_latitude_ordering (afield(ls)%nlt, rlatd(:,ls), afield(ls)%ln2s, ifail)
            rlat(1:afield(ls)%nlt,ls) = rlatd(1:afield(ls)%nlt,ls)
            rlng(1:afield(ls)%nlg,ls) = rlngd(1:afield(ls)%nlg,ls)
!
! Check for consistency of fields
         Else
            Call check_gridded_block_v10 (tfield(1), afield(ls), rlatd(1:afield(ls)%nlt,ls), rlat(1:afield(ls)%nlt,ls), &
                 rlngd(1:afield(ls)%nlg,ls), rlng(1:afield(ls)%nlg,ls), cfail, cproc, ifail)
            If (ifail /= 0) Return
         End If
         If (igf == 1 .and. afile%ngs > 1) nfield = tfield(1)
         pdlast = tfield(1)%tssn%tprd(1)
      End Do
   Else
      igf = 1
   End If
!
! Check for missing dates
   pdlast = pdlastt
   cproc = 'get_gridded_info_v10'
   Call check_dates (lss, lread, afile%iseq, afile%nse, afile%nls, ny1, kl, 1, kav(kl,ll), &
                    prdps(ls), tfield(1)%tssn%tprd(1), cproc, ifail)
   If (ifail /= 0) Return
   If (.not.lread) Then
      ifd_bk = ifd
      ilf_bk = ilf
   End If
   pdlastt = tfield(1)%tssn%tprd(1)
   pdlast = pdlastt
!
   Return
  End Subroutine advance_grid
!
!
!
  Subroutine check_gridded_block_v10 (tfield, rfield, tlat, rlat, tlng, rlng, cfail, cproc, ifail)
!
! Checks for consistency of field information
!
! On exit (if cproc = 'check_gridded_block_v10'):
!    ifail =  0 Successful
!    ifail =  1 Inconsistent tag as indicated by cfail
!    ifail =  2 Inconsistent latitudes or longitudes as indicated by cfail
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(field), Intent(In) :: tfield ! - test field -
   Type(field), Intent(In) :: rfield ! - reference field -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: tlat ! - test latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - reference longitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: tlng ! - test latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - reference longitudes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Initialise routine
   cproc = 'check_gridded_block_v10'
   cfail = ' '
!
! Check for consistent field settings
! - number of latitudes -
   If (tfield%nlt /= rfield%nlt) Then
      cfail = 'ncol'
      ifail = 1
      Return
   End If
! - number of longitudes -
   If (tfield%nlg /= rfield%nlg) Then
      cfail = 'nrow'
      ifail = 1
      Return
   End If
! - ensemble member -
   If (tfield%member /= rfield%member) Then
      cfail = 'member'
      ifail = 1
      Return
   End If
! - missing values -
   If (tfield%rmiss /= rfield%rmiss) Then
      cfail = 'missing'
      ifail = 1
      Return
   End If
! - variable -
   If (tfield%var /= rfield%var) Then
      cfail = 'var'
      ifail = 1
      Return
   End If
! - units -
   If (tfield%unit /= rfield%unit) Then
      cfail = 'units'
      ifail = 1
      Return
   End If
! - geopotential height -
   If (tfield%z%hght /= rfield%z%hght) Then
      cfail = 'z'
      ifail = 1
      Return
   End If
!
! Compare latitudes
   If (Any(tlat(:) /= rlat(:))) Then
      cfail = 'latitudes'
      ifail = 2
      Return
   End If
!
! Compare longitudes
   If (Any(tlng(:) /= rlng(:))) Then
      cfail = 'longitudes'
      ifail = 2
      Return
   End If
   cproc = ' '
   ifail = 0
!
   Return
  End Subroutine check_gridded_block_v10
 End Subroutine get_gridded_info_v10
!
!
!
 Subroutine get_gridded_block_v10 (iin, igf, lctagt, afield, ifail, cproc, cfail, &
            bfield, rlat, rlng)
!
! Reads field information for a block of data
!
! On exit (if cproc = 'get_gridded_block_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Unable to identify tag line
!    ifail =  4 Field structure is not gridded
!    ifail =  5 Category numbering is incorrect
!
! Modules
   Use data_io_constants, Only: ltag
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
   Integer, Intent(In) :: igf ! - current gridded field -
!
   Logical, Intent(In) :: lctagt ! - cpt:T tag required? -
!
! - input/output scalars -
   Type(field), Intent(InOut) :: afield ! - field information -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Type(field), Intent(In), Optional :: bfield ! - second field -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(InOut), Optional :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(InOut), Optional :: rlng ! - longitudes -
!
! Locals
!
! Local scalars
   Integer :: i    ! - latitude index -
   Integer :: j    ! - longitude index -
   Integer :: igrd ! - structure -
!
   Character(Len=ltag) :: ctag ! - tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Present, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_gridded_block_v10'
   cfail = ' '
!
! Read tag line
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
   If (Len_Trim(ctag) == 0) GoTo 2
   If (Index(ctag, 'cpt:') == 0) Then
      ifail = 3
      Return
   End If
!
! Parse tag line
   Call read_tags (Trim(ctag)//' ',15,                                                                           &
        ['T        ', 'S        ', 'F        ', 'Z        ', 'C        ', 'clim_prob', 'M        ', 'model    ', &
         'col      ', 'row      ', 'ncol     ', 'nrow     ', 'field    ', 'units    ', 'missing  '],             &
        lctagt, afield, ifail, cfail,                                                                            &
        bfield=bfield, icat=igf, idstr=igrd)
   If (ifail /= 0) Then
      cproc = 'read_tags'
      Return
   End If
!
! Return if field is not gridded
   If (.not.Present(bfield)) Then
      If (igrd /= 1) Then
         ifail = 4
         Return
      End If
!
! Return if category is incorrect
      If (afield%icat(igf) /= igf .and. afield%icat(igf) > 0) Then
         ifail = 5
         Return
      End If
   End If
!
! Read longitudes
   If (Present(rlng)) Then
      Read (Unit=iin, Fmt=*, Err=1, End=2) (rlng(j), j=1,afield%nlg)
   Else
      Read (Unit=iin, Fmt=*, Err=1, End=2)
   End If
!
! Read latitudes
   If (Present(rlat)) Then
      Do i = 1, afield%nlt
         Read (Unit=iin, Fmt=*, Err=1, End=2) rlat(i)
      End Do
   Else
      Do i = 1, afield%nlt
         Read (Unit=iin, Fmt=*, Err=1, End=2)
      End Do
   End If
!
   cproc = ' '
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   Return
!
! - end of file -
2  ifail = 2
   Return
!
 End Subroutine get_gridded_block_v10
!
!
!
 Subroutine get_nongridded_nls_v10 (iin, lms, lss, nt, afile, ntag, isq1, ifail, cproc, pdlast)
!
! Determines number of lagged-fields in a version-10 station or unreferenced input file
!
! On exit (if cproc = 'get_nongridded_nls_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  4 File is too wide
!
! Modules
   Use data_io_constants,   Only: lfli
   Use data_time_constants, Only: isq_mn
   Use time,                Only: pprd, &
                                  Assignment(=), &
                                  ims, mpsn, &
                                  get_pdate
!  Use iofiles,             Only: max_width
   Use iofiles,             Only: ifile
   Use season,              Only: len_ssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input unit number -
!
   Logical, Intent(In) :: lms ! - permit monthly-sequenced files? -
   Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: nt ! - number of time steps -
!
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - output scalars -
   Integer, Intent(Out) :: isq1  ! - first time sequencing -
   Integer, Intent(Out) :: ntag  ! - number of tag lines -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
!
   Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Locals
!
! Local scalars
   Integer :: k    ! - case index -
   Integer :: emax ! - maximum length of line -
   Integer :: llen ! - length of line -
   Integer :: ilf  ! - lagged field index -
!
   Character(Len=lfli) :: ctag ! - tag line -
!
   Type(pprd) :: prdp ! - previous date -
   Type(pprd) :: prdc ! - current date -
   Type(pprd) :: prdf ! - first date -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Index, Len_Trim, Min, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_nongridded_nls_v10'
   pdlast = 0
   isq1 = 0
!
! Skip station names and tags
   Read (Unit=iin, Fmt=*, Err=1, End=2)
   ntag = 1
   Do
      Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
      If (Index(ctag, 'cpt:') == 0) Exit
      ntag = ntag + 1
   End Do
!
! Read first date
   ctag = AdjustL(ctag)
   Call get_pdate (' ', Trim(ctag), afile%prd1%sdate, ifail, &
        edate=afile%prd1%edate)
   If (ifail /= 0) Then
      cproc = 'get_pdate'
      Return
   End If
   pdlast = afile%prd1
   prdf = afile%prd1
   ilf = 1
   If (lss) Then
      ims(ilf) = afile%prd1%sdate%imn
      mpsn(ilf) = len_ssn(afile%prd1%sdate, afile%prd1%edate, isq_mn)
   End If
!
! Check for lagged-fields
! - read subsequent dates -
   afile%nls = 1
   afile%nse = 1
   prdp = afile%prd1
   If (nt > 1) Then
      get_nls: Do k = 1, nt-1
         Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
         ctag=AdjustL(ctag)
         Call get_pdate (' ', Trim(ctag), prdc%sdate, ifail, &
              edate=prdc%edate)
         If (ifail /= 0) Then
            cproc = 'get_pdate'
            Return
         End If
! - identify date sequencing -
         get_seq: Do
            Call determine_sequence (prdp, prdc, lms, lss, (isq1==0), isq1, afile%iseq, afile%nls, afile%nse, pdlast, ifail)
            Select Case (ifail)
             Case (0)
               Exit get_nls
             Case (1)
               Cycle get_nls
             Case (2)
               prdp = prdf
               Cycle get_seq
             Case Default
               cproc = 'get_sequence'
               Return
            End Select
         End Do get_seq
      End Do get_nls
   End If
!
! Identify longest line
   Rewind (Unit=iin)
!  Call max_width (iin, afile%lmax, ifail) ! - bug in FTN95 -
   afile%lmax = 0
   emax = 100
   Do
      Read (Unit=iin, Fmt='(A)', Err=3, End=3) ctag(1:emax)
      llen = Len_Trim(ctag(1:emax))
      If (llen <= afile%lmax) Then ! - current line is shorter -
         Cycle
      Else ! - extend if line is too short -
         afile%lmax = llen
         If (afile%lmax >= lfli-100) Then ! - return if line is too long -
            ifail = 4
            Return
         End If
         emax = Min(afile%lmax+100, lfli)
         If (emax < 2**13) Then
            Backspace (Unit=iin)
         Else
            Rewind (Unit=iin)
         End If
      End If
   End Do
!
3  cproc = ' '
   ifail = 0
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   Return
! - end of file -
2  ifail = 2
!
   Return
 End Subroutine get_nongridded_nls_v10
!
!
!
 Subroutine get_nongridded_fields_v10 (iin, ntag, rmiss, afile, afield, lfend, ntn, ifail, ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines field settings and maximum number of cases
!
! On exit (if cproc = 'get_nongridded_fields_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  3 Season for last date of ifd field, ilf lagged-field, igf category is inconsistent with earlier dates
!    ifail =  4 Categories are not in order
!    ifail =  5 Field information for different categories is inconsistent
!    ifail =  6 Starting date for different categories is inconsistent
!
! Modules
   Use data_io_constants,   Only: ltag
   Use data_time_constants, Only: isq_yr
   Use time,                Only: pprd, &
                                  Assignment(=), Operator(/=), &
                                  get_cssn, get_pdate
   Use iofiles,             Only: ifile
   Use fields,              Only: field, &
                                  Operator(/=), &
                                  set_field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin  ! - input unit number -
   Integer, Intent(In) :: ntag ! - number of tag lines -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - input/output arrays -
   Type(field), Dimension(:), Intent(InOut) :: afield ! - field information -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
   Integer, Intent(Out) :: ifd   ! - field at error -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: igf   ! - category field at error -
   Integer, Intent(Out) :: ntn   ! - number of time steps -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Logical, Intent(Out) :: lfend ! - end of file? -
!
   Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Locals
!
! Local scalars
   Integer :: k   ! - case index -
   Integer :: l   ! - field/lagged-field index -
   Integer :: nsl ! - number of sub-seasons or lagged seasons -
!
   Character(Len=ltag) :: ctag ! - tag line -
!
   Type(field) :: tfield ! - field information -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_nongridded_fields_v10'
   ifd = 0
   ilf = 0
   igf = 0
   cfail = ' '
   pdlast = 0
   Call set_field (rmiss, tfield)
!
! Rewind to first field
   Call read_headers (iin, afile%ntag, .true., ifail)
   Select Case (ifail)
    Case (0)
      Continue
    Case (1) ! - problem reading file -
      GoTo 1
    Case (2) ! - premature end of file -
      GoTo 2
   End Select
!
! Read fields
   nsl = afile%nse*afile%nls
   afile%nt = 1
   Do ifd = 1, afile%nfs
      l = (ifd - 1)*nsl + 1
      Do igf = 1, afile%ngs
         Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
         ctag = AdjustL(ctag)
         If (igf == 1) Then
            If (l == 1) Then
               Call read_tags (Trim(ctag)//' ', 9,                                    &
                    ['C        ', 'clim_prob', 'col      ', 'row      ', 'ncol     ', &
                     'nrow     ', 'field    ', 'units    ', 'missing  '],             &
                    .false., tfield, ifail, cfail,                                    &
                    icat=igf, nt=ntn)
            Else
               Call read_tags (Trim(ctag)//' ', 9,                                    &
                    ['C        ', 'clim_prob', 'col      ', 'row      ', 'ncol     ', &
                     'nrow     ', 'field    ', 'units    ', 'missing  '],             &
                    .false.,tfield, ifail,cfail,                                      &
                    bfield=afield(l-1), icat=igf, nt=ntn)
            End If
         Else
            Call read_tags (Trim(ctag)//' ', 9,                                    &
                 ['C        ', 'clim_prob', 'col      ', 'row      ', 'ncol     ', &
                  'nrow     ', 'field    ', 'units    ', 'missing  '],             &
                 .false., tfield, ifail, cfail,                                    &
                 bfield=afield(l), icat=igf, nt=ntn)
         End If
         If (ifail /= 0) Then
            cproc = 'read_tags'
            Return
         End If
         If (tfield%icat(igf) /= igf .and. tfield%icat(igf) > 0) Then ! - check for correct ordering of categories -
            ifail = 4
            GoTo 3
         End If
         If (igf == 1) Then
            afield(l) = tfield
            afield(l)%mdate = 0
         Else
            If (tfield /= afield(l)) Then ! - check for consistency of fields -
               ifail = 5
               GoTo 3
            End If
            afield(l)%icat(igf) = igf
            afield(l)%prob(igf) = tfield%prob(igf)
         End If
! - copy information into domain -
         afield(l)%region%nlts = afield(l)%nlt
         afield(l)%region%nlgs = afield(l)%nlg
         afield(l)%region%nlt2 = afield(l)%nlt
         afield(l)%region%nlg2 = afield(l)%nlg
!
! Skip additional tag lines
         Do k = 1, ntag
            Read (Unit=iin, Fmt=*, Err=1, End=2)
         End Do
!
! Set lagged-fields
         Do ilf = 1, nsl
            l = (ifd - 1)*nsl + ilf
            If (ilf > 1 .and. igf == 1) afield(l) = afield(l-1)
            Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
            ctag = AdjustL(ctag)
            Call get_pdate (' ', Trim(ctag), tfield%tssn%tprd(1)%sdate, ifail, &
                 edate=tfield%tssn%tprd(1)%edate)
            If (ifail /= 0) Then
               cproc = 'get_pdate'
               Return
            End If
            If (igf == 1) Then
               afield(l)%tssn%tprd(1) = tfield%tssn%tprd(1)
               afield(l)%cssn%cssn(1) = get_cssn(tfield%tssn%tprd(1), 1, [1])
            Else
               If (tfield%tssn%tprd(1) /= afield(l)%tssn%tprd(1)) Then
                  ifail = 6
                  GoTo 3
               End If
            End If
            pdlast = tfield%tssn%tprd(1)
         End Do
!
! Read last case
         If (ntn > nsl) Then
            If (ntn-nsl-1 > 0) Then
               Do k = nsl+1, ntn-1
                  Read (Unit=iin, Fmt=*, Err=1, End=2)
               End Do
            End If
            l = ifd*nsl
            Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
            ctag = AdjustL(ctag)
            Call get_pdate (' ',Trim(ctag), tfield%tssn%tprd(1)%sdate, ifail, &
                 edate=tfield%tssn%tprd(1)%edate)
            If (ifail /= 0) Then
               cproc = 'get_pdate'
               Return
            End If
            pdlast = tfield%tssn%tprd(1)
!
! Check whether last case is for last lagged-field
            lfend = .false.
            If (afile%iseq == isq_yr) Then
               Call check_last (afile%nls, afile%nls*(ifd-1), tfield%tssn%tprd(1), afield(:), lfend, ilf, ifail)
               If (ifail /= 0) Then
                  ifail = 3
                  Return
               End If
            End If
            If (ifd == 1 .and. igf == 1) afile%prdn = tfield%tssn%tprd(1)
         Else
            afile%prdn = tfield%tssn%tprd(1)
         End If
      End Do
   End Do
!
! Set number of models
   afile%nms = 1
!
   cproc = ' '
   ifail = 0
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   GoTo 3
!
! - end of file -
2  Backspace (Unit=iin)
   Backspace (Unit=iin)
   Read (Unit=iin, Fmt='(A)', Err=3) ctag
   ctag = AdjustL(ctag)
   Call get_pdate (' ',Trim(ctag), pdlast%sdate, ifail, &
        edate=pdlast%edate)
   If (ifail == 0) ifail = 2
!
! Category errors
3  igf = igf - 1
   If (afile%ngs > 1) Then
      If (igf == 0) Then
         If (ifd > 1) Then
            ifd = ifd - 1
            igf = afile%ngs
         Else
            pdlast = 0
         End If
      End If
   Else
      ifd = ifd - 1
   End If
!
   Return
 End Subroutine get_nongridded_fields_v10
!
!
!
 Subroutine get_nongridded_info_v10 (iin, ntag, ny1, lss, lfend, afield, afile, kav, rlat, rlng, cstn, rwk, ifail, &
            ifd, ilf, igf, cproc, cfail, pdlast)
!
! Determines variable names and coordinates of station of stations. Identifies available cases.
!
! On exit (if cproc = 'get_nongridded_info_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  2 Premature end of file. Data up to pdlast read successfully. Problem in ifd field, ilf lagged-field, igf category
!    ifail =  3 Unable to read latitudes and/or longitudes (as indicated by cfail) for ifd field, ilf lagged-field, igf category
!    ifail =  4 Dates for ifd field, ilf lagged-field, igf category are not in sequence. Data up to pdlast read successfully
!
! Modules
   Use data_io_constants, Only: lstn, ltag
   Use time,              Only: pprd, &
                                Assignment(=), Operator(+), &
                                check_pprd, get_cssn, get_pdate, &
                                nss
   Use iofiles,           Only: ifile
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin  ! - input unit number -
   Integer, Intent(In) :: ntag ! - number of tag lines -
   Integer, Intent(In) :: ny1  ! - number of cases in first year -
!
   Logical, Intent(In) :: lfend ! - end of file? -
   Logical, Intent(In) :: lss   ! - permit seasonally-sequenced files? -
!
! - input arrays -
   Type(field), Dimension(:), Intent(In) :: afield ! - field information -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! - input/output arrays -
   Logical, Dimension(:,:), Intent(InOut) :: kav ! - cases available? -
!
! - pointer arrays -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng ! - longitudes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
   Integer, Intent(Out) :: ifd   ! - field at error -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: igf   ! - category field at error -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
   Character(Len=*), Intent(Out) :: cfail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! - output arrays -
   Character(Len=lstn), Dimension(:,:), Intent(Out) :: cstn ! - names of stations/indices -
!
! - workspace arrays -
   Real(Kind=rp), Dimension(:,:), Pointer :: rwk ! - workspace -
!
! Locals
!
! Local scalars
   Integer :: i     ! - station index -
   Integer :: k     ! - case index -
   Integer :: kl    ! - year index -
   Integer :: l     ! - field/lagged-field index -
   Integer :: ll    ! - field/lagged-field index (ignoring seasons) -
   Integer :: ls    ! - field/lagged-field/sub-season index -
   Integer :: nl    ! - number of lags -
   Integer :: nskip ! - number of lines to skip -
!
   Character(Len=ltag) :: ctag ! - tag line -
!
   Logical :: llx   ! - longitudes read? -
   Logical :: lly   ! - longitudes read? -
   Logical :: lread ! - read current case? -
!
   Type(pprd) :: prdc ! - current date -
!
! Local arrays
   Type(pprd), Dimension(afile%nfl*afile%nse) :: prdps ! - last successfully read periods -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic AdjustL, Any, Ceiling, Index, Real, Trim
!
! Executable Statements
!
! Initialise routine
   cproc = 'get_nongridded_info_v10'
   ifd = 0
   ilf = 0
   igf = 0
   cfail = ' '
   pdlast = 0
   nl = afile%nls
   If (lss) nl = nl/nss
!
! Rewind
   Call read_headers (iin, afile%ntag, .true., ifail)
   Select Case (ifail)
    Case (0)
      Continue
    Case (1) ! - problem reading file -
      Return
    Case (2) ! - premature end of file -
      GoTo 2
   End Select
!
! Set initial dates
   lread = .true.
   read_fields: Do ifd = 1, afile%nfs
      Do igf = 1, afile%ngs
         l = (ifd - 1)*afile%nls + 1
         prdps(:) = afield(:)%tssn%tprd(1)
!
! Read station names
         If (lread) Read (Unit=iin, Fmt=*, Err=1, End=2)
         Read (Unit=iin, Fmt=*, Err=1, End=2) (cstn(i,l), i=1,afield(l)%nlt)
!
! Read latitudes and longitudes
         nskip = 1
         If (afile%igeog == 1) Then
            lly = .false.
            llx = .false.
            Do
               Read (Unit=iin, Fmt=*, Err=1, End=2) ctag, (rwk(i,l), i=1,afield(l)%nlt)
               If (Index(ctag, 'cpt:Y') > 0) Then
                  lly = .true.
                  nskip = nskip + 1
                  If (igf == 1) Then
                     rlat(1:afield(l)%nlt,l) = rwk(1:afield(l)%nlt,l)
                  Else If (Any(rwk(1:afield(l)%nlt,l) /= rlat(1:afield(l)%nlt,l))) Then ! - check for consistency -
                     cfail = 'latitudes'
                     GoTo 3
                  End If
                  If (llx) Exit
               Else If (Index(ctag, 'cpt:X') > 0) Then
                  llx = .true.
                  nskip = nskip + 1
                  If (igf == 1) Then
                     rlng(1:afield(l)%nlt,l) = rwk(1:afield(l)%nlt,l)
                  Else If (Any(rwk(1:afield(l)%nlt,l) /= rlng(1:afield(l)%nlt,l))) Then ! - check for consistency -
                     cfail = 'longitudes'
                     GoTo 3
                  End If
                  If (lly) Exit
               Else If (Index(ctag, 'cpt:') > 0) Then
                  nskip = nskip + 1
               Else
                  Exit
               End If
            End Do
!
! Check for missing coordinates
            If (llx .and. lly) Then
               Continue
            Else If (.not.(llx .and. lly)) Then
               cfail = 'latitudes and longitudes'
               ifail = 3
               Return
            Else If (.not.lly) Then
               cfail = 'latitudes'
               ifail = 3
               Return
            Else If (.not.llx) Then
               cfail = 'longitudes'
               ifail = 3
               Return
            End If
!
! Skip tag lines in unreferenced files
         Else
            nskip = nskip + 1
            Do
               Read (Unit=iin, Fmt=*, Err=1, End=2) ctag
               If (Index(ctag, 'cpt:') > 0) Then
                  nskip = nskip + 1
               Else
                  Exit
               End If
            End Do
         End If
!
! Skip past first dates
         If (afile%nt*nl > 1) Then
            Do k = 1, afile%nls+ntag-nskip
               Read (Unit=iin, Fmt=*, Err=1, End=2) ctag
            End Do
            l = ifd*nl
            pdlast = afield(l)%tssn%tprd(1)
!
! Check for missing dates
            If (afile%nt > nss) Then
               lread = .true.
               kl = afile%nls
               Do k = afile%nls+1, afile%ntm
                  Do ilf = 1, nl
                     l = (ifd - 1)*afile%nls + ilf
                     Call set_tf_counters (afile%iseq, afile%nse, .true., k, l, kl, ll, ls)
                     If (lread) Then
                        Read (Unit=iin, Fmt='(A)', Err=1, End=2) ctag
                        ctag = AdjustL(ctag)
! - check for next field in case of missing last values -
                        If (Index(ctag, 'cpt:') > 0) Then
                           lread = .false.
                           Cycle read_fields
                        End If
                        Call get_pdate (' ', Trim(ctag), prdc%sdate, ifail, &
                             edate=prdc%edate)
                        Select Case (ifail)
                         Case (0)
                           Continue
                         Case (2)
                           If (lss .and. k >= Ceiling(Real(afile%nt)/Real(afile%nls)) .and. igf == 1) Then
                              ifail = 0
                              Exit read_fields
                           Else
                              cproc = 'get_pdate'
                              Return
                           End If
                         Case Default
                           cproc = 'get_pdate'
                           Return
                        End Select
                     End If
                     ctag = get_cssn(prdc, 1, [1])
! - check for consistency of periods -
                     ifail = check_pprd(afield(l)%cssn%cssn(1), ctag, afield(l)%tssn%tprd(1), prdc, afile%iseq)
                     If (ifail /= 0) Then
                        cproc = 'check_pprd'
                        Return
                     End If
! - check for advancing dates -
                     Call check_dates (lss, lread, afile%iseq, afile%nse, afile%nls, ny1, kl, igf, kav(kl,ll), &
                          prdps(ls), prdc, cproc, ifail)
                     Select Case (ifail)
                      Case (0)
                        Continue
                      Case (1)
                        Exit
                      Case (2)
                        GoTo 3
                     End Select
                  End Do
               End Do
            End If
         Else If (igf < afile%ngs) Then ! - read single forecasts -
            Read (Unit=iin, Fmt=*, End=2)
         End If
      End Do
!
! Copy field information for lagged variables
      If (afile%nls > 1) Then
         Do ilf = 2, afile%nls
            l = (ifd - 1)*afile%nls + ilf
            If (ilf > 1) Then
               If (afile%igeog == 1) Then
                  rlat(:,l) = rlat(:,l+1-ilf)
                  rlng(:,l) = rlng(:,l+1-ilf)
               End If
               cstn(:,l) = cstn(:,l+1-ilf)
            End If
         End Do
      End If
   End Do read_fields
!
! Errors
   If (lread) Then
      cproc = ' '
      ifail = 0
   Else
      ifail = 4
   End If
   Return
!
! - problem reading file -
1  ifail = 1
   Return
!
! - problem reading file -
2  If (lfend .and. l == afile%nfl) Then
      ifail = 0
   Else
      ifail = 2
   End If
   Return
!
! Trap inconsistency errors
3  ifail = 5
   If (afile%ngs > 1) Then
      igf = igf - 1
      If (igf == 0) Then
         If (ifd > 1) Then
            ifd = ifd - 1
            igf = afile%ngs
         Else
            pdlast = 0
         End If
      End If
   Else
      ifd = ifd - 1
   End If
!
   Return
 End Subroutine get_nongridded_info_v10
!
!
!
  Subroutine check_last (nls, ifds, tprd, afield, lfend, ilf, ifail)
!
! Checks whether last case is for last lagged-field
!
! Modules
   Use time,   Only: pprd
   Use fields, Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
   Integer, Intent(In) :: ifds ! - field offset -
!
   Type(pprd), Intent(In) :: tprd ! - current date -
!
! - input arrays -
   Type(field), Dimension(:), Intent(In) :: afield ! - field information -
!
! - input/output scalars -
   Logical, Intent(InOut) :: lfend ! - end of file? -
!
! - output scalars -
   Integer, Intent(Out) :: ilf   ! - lagged-field at error -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: l ! - field/lagged-field index -
!
! Executable Statements
!
! Check whether last case is for last lagged-field
   Do ilf = nls, 1, -1
      l = ifds + ilf
      If (tprd%sdate%imn == afield(l)%tssn%tprd(1)%sdate%imn .and.  &
          tprd%sdate%idy == afield(l)%tssn%tprd(1)%sdate%idy .and.  &
          tprd%edate%imn == afield(l)%tssn%tprd(1)%edate%imn .and.  &
          tprd%edate%idy == afield(l)%tssn%tprd(1)%edate%idy) Then
         Exit
      Else If (ilf > 1) Then
         lfend = .true.
      Else
         ifail = 1
         Return
      End If
   End Do
   ifail = 0
!
   Return
  End Subroutine check_last
!
!
!
 Subroutine reassign_seas_seq (lss, isq1, afile)
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, nmn
   Use time,                Only: nss
   Use iofiles,             Only: ifile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq1 ! - first time sequencing -
!
   Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Executable Statements
!
! Reassign seasonal sequencing
   If (isq1 == isq_mn .and. afile%nls == nmn) Then ! - reassign monthly lagged fields -
      afile%iseq = isq_mn
      afile%nls = 1
   Else If (isq1 == isq_so .and. afile%nls <= nmn) Then
      afile%iseq = isq_so
      afile%nls = 1
   Else If (isq1 == isq_sn .and. afile%nls <= nmn) Then
      afile%iseq = isq_sn
   Else If (isq1 > 0 .and. afile%nse > 1) Then
      If (afile%nt == 1) Then ! - permit data from a single year -
         afile%nt = afile%nse
         afile%nse = 1
      Else
         afile%iseq = isq1
      End If
   End If
   If (lss) nss = afile%nls
   afile%nfl = afile%nfs*afile%nls
!
 End Subroutine reassign_seas_seq
!
!
!
 Subroutine set_tf_counters (isq, nse, laddk, k, l, kl, ll, ls)
!
! Sets time and field counters
!
! Modules
   Use data_time_constants, Only: isq_sn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isq ! - time sequencing -
   Integer, Intent(In) :: k   ! - time counter -
   Integer, Intent(In) :: l   ! - field counter -
   Integer, Intent(In) :: nse ! - number of sub-seasons -
!
   Logical, Intent(In) :: laddk ! - allow year increment? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: kl ! - year counter -
   Integer, Intent(InOut) :: ll ! - field/lagged-field index (ignoring seasons) -
   Integer, Intent(InOut) :: ls ! - field/lagged-field/sub-season index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Executable Statements
!
! Set counters
   Select Case (isq)
    Case Default
      ll = l
      ls = l
      kl = k
    Case (isq_sn)
      ll = 1
      ls = l
      kl = kl + 1
    Case (1:)
      ll = l
      ls = l + Mod(k-1, nse)
      If (laddk) kl = kl + 1
   End Select
!
   Return
 End Subroutine set_tf_counters
!
!
!
 Subroutine check_dates (lss, lread, iseq, nse, nls, ny1, ik, igf, kav, p1, p2, cproc, ifail)
!
! Checks date sequencing
!
! Modules
   Use data_time_constants, Only: isq_dy, isq_yr, mdy, ndw
   Use time,                Only: pprd, &
                                  Operator(+), &
                                  lslim, &
                                  date_diff, day_of_week, day_of_year
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igf  ! - category field at error -
   Integer, Intent(In) :: ik   ! - current case -
   Integer, Intent(In) :: iseq ! - time sequencing -
   Integer, Intent(In) :: nse  ! - number of sub-seasons -
   Integer, Intent(In) :: nls  ! - number of lagged seasons -
   Integer, Intent(In) :: ny1  ! - number of cases in first year -
!
   Logical, Intent(In) :: lss ! - permit seasonally-sequenced files? (for PFV X input files) -
!
   Type(pprd), Intent(In) :: p2 ! - current target period -
!
! - input/output scalars -
   Logical, Intent(InOut) :: lread ! - read current case? -
   Logical, Intent(InOut) :: kav   ! - case available? -
!
   Type(pprd), Intent(InOut) :: p1 ! - last successfully read period -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: cproc ! - procedure in error -
!
! Locals
!
! Local scalars
   Integer :: dd  ! - date difference -
   Integer :: dd1 ! - daily date difference -
   Integer :: isq ! - time sequencing -
!
   Logical :: ls_bk ! - backup use season limits? -
!
! Executable Statements
!
! Check time sequencing
   cproc = 'check_dates'
   If (ik > ny1) Then
      If ((lss .or. iseq > 0) .and. (nls > 1 .or. nse > 1)) Then
         isq = isq_yr
      Else
         isq = iseq
      End If
!
! Check whether dates are advancing
      ls_bk = lslim
      lslim = .false.
      dd = date_diff(p1%sdate, p2%sdate, isq)
      If (iseq == 1) Then
         dd1 = date_diff(p1%sdate, p2%sdate, iseq) + 1
         If (dd1 >= 2*(mdy - 1) - ny1) dd = dd + 1
      End If
      lslim = ls_bk
      If (dd == 1) Then ! - dates are consecutive -
         p1 = p2
         lread = .true.
         kav = .true.
         ifail = 0
      Else If (dd > 1) Then ! - dates have skipped -
         If (iseq < 0) Then
            p1 = p1 + 1
         Else
            p1%sdate%iyr = p1%sdate%iyr + 1
            p1%edate%iyr = p1%edate%iyr + 1
         End If
         lread = .false.
         kav = .false.
         ifail = 0
      Else ! - second date is mis-aligned -
         ifail = 1
      End If
      Select Case (ifail)
       Case (0)
         Continue
       Case Default
         If (igf > 1 .and. kav) GoTo 1 ! - ignore if correct dates were available in first category -
         If (nse > 1) Then ! - check for valid start of season differences -
            If (day_of_year(p1%sdate) == day_of_year(p2%sdate)) Then ! - check for common day-of-the-year start -
               lread = .true.
               p1 = p2
               kav = .true.
            Else
               dd = date_diff(p1%sdate, p2%sdate, isq_dy)
               If (dd >= mdy-ndw .and. dd <= mdy+ndw-1) Then
                  If (day_of_week(p1%sdate) == day_of_week(p2%sdate)) Then ! - check for common day-of-the-week start -
                     lread = .true.
                     p1 = p2
                     kav = .true.
                  Else
                     Return
                  End If
               Else
                  Return
               End If
            End If
         Else
            Return
         End If
      End Select
   Else
      kav = .true.
   End If
!
1  cproc = ' '
   ifail = 0
!
   Return
 End Subroutine check_dates
!
!
!
 Subroutine determine_sequence (prd1, prd2, lms, lss, lsq1, isq1, isq, nls, nse, pdlast, ifail, &
            ilf)
!
! Determines date sequencing, and counts lagged fields
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Lagged fields may be present; test next sequence
!    ifail =  2 Error
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, ndw, nmn
   Use time,                Only: pprd, &
                                  Operator(==), &
                                  idywk, ims, mpsn, &
                                  day_of_week, get_sequence
   Use season,              Only: len_ssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lms  ! - permit monthly-sequenced files? -
   Logical, Intent(In) :: lss  ! - permit seasonally-sequenced files? -
   Logical, Intent(In) :: lsq1 ! - reset isq1? -
!
   Type(pprd), Intent(In) :: prd2 ! - period 2 -
!
! - input/output scalars -
   Integer, Intent(InOut) :: isq1 ! - initial time sequencing -
   Integer, Intent(InOut) :: isq  ! - time sequencing -
   Integer, Intent(InOut) :: nls  ! - number of lagged fields -
   Integer, Intent(InOut) :: nse  ! - number of sub-seasons -
!
   Type(pprd), Intent(InOut) :: prd1 ! - period 1 -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Type(pprd), Intent(Out) :: pdlast ! - last period read successfully -
!
! Optional arguments
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: ilf ! - lagged field -
!
! Locals
!
! Local scalars
   Integer :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Determine sequencing
   Call get_sequence (prd1, prd2, isq, ifail)
   Select Case (ifail)
    Case (0)
      pdlast = prd2
! - assume a lagged-field if sequencing is monthly or seasonal, unless all months / seasons are present -
      Select Case (isq)
       Case (isq_mn, isq_sn, isq_so)
         nls = nls + 1
         If (Present(ilf)) ilf = ilf + 1
         ifail = 1
! - check for yearly sequencing -
       Case (isq_yr)
         If (lss) Then
            If (prd2%sdate == prd2%edate) Then
               If (prd1%sdate%imn == prd2%sdate%imn) Then
                  If (nls == nmn .and. lms) Then
                     isq = isq_mn
                  Else
                     isq = isq_yr
                  End If
               Else
                  isq = isq_mn
               End If
! - allow monthly sequencing if all months are present -
            Else If (nls == nmn) Then
               isq = isq_so
            Else
               If (prd1%sdate%imn == prd2%sdate%imn) Then
                  isq = isq_yr
               Else
                  isq = isq_sn
               End If
            End If
            If (isq1 /= isq_sn) nls = 1
         End If
         ifail = 0
      End Select
    Case Default
      ifail = 2
   End Select
!
   If (lsq1) isq1 = isq
   Select Case (ifail)
    Case (0)
      If (isq > 0) Then ! - check for number of subseasons for daily to dekadal timescales -
        If (nse <= ndw) idywk(nse) = day_of_week(prd1%sdate)
        nse = nse + 1
        prd1 = prd2
        ifail = 1
      End If
    Case (1)
      If (lss) Then
         If (Present(ilf)) Then
            i = ilf
         Else
            i = 1
         End If
         ims(i) = prd2%sdate%imn
         mpsn(i) = len_ssn(prd2%sdate, prd2%edate, isq_mn)
      End If
    Case Default
      If (isq1 > 0) Then
         ifail = 2
      Else
         ifail = 3
      End If
   End Select
!
   Return
 End Subroutine determine_sequence
!
!
!
 Subroutine calculate_nt (iseq, lstack, nls, d2, d1, n, isn, nt, ntm, ifail)
!
! Calculates number of cases
!
! Modules
   Use data_time_constants, Only: isq_mn, isq_sn, isq_yr
   Use time,                Only: pdate, &
                                  date_diff
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iseq ! - time sequencing -
   Integer, Intent(In) :: isn  ! - season -
   Integer, Intent(In) :: n    ! - number of time steps -
   Integer, Intent(In) :: nls  ! - number of lagged fields -
!
   Logical, Intent(In) :: lstack ! - stacked fields? -
!
   Type(pdate), Intent(In) :: d1 ! - previous date -
   Type(pdate), Intent(In) :: d2 ! - current date -
!
! - input/output scalars -
   Integer, Intent(InOut) :: nt ! - number of cases -
!
! - output scalars -
   Integer, Intent(Out) :: ntm   ! - number of monthly cases -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: isq_u ! - time sequencing -
   Integer :: ntt   ! - number of time steps -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max
!
! Executable Statements
!
! Calculate number of cases
   ifail = 1
   If (n > nls .and. iseq /= 0) Then
      Select Case (iseq)
       Case Default
         isq_u = iseq
       Case (isq_mn)
         isq_u = isq_yr
      End Select
!
! Calculate number of cases
      ntt = date_diff(d2, d1, isq_u) + 1
      Select Case (iseq)
       Case Default
         If (ntt < 1) Return
       Case (1:)
         Continue
       Case (isq_sn)
         If (lstack) Then
            ntt = n
         Else If (isn > 0) Then
            ntt = ntt + isn - 1
         End If
      End Select
      nt = Max(nt, ntt)
   Else
      nt = 1
   End If
!
! Set number of monthly cases
   Select Case (iseq)
    Case Default
      ntm = nt
    Case (isq_mn)
      ntm = date_diff(d2, d1, isq_mn) + 1
   End Select
   ifail = 0
!
   Return
 End Subroutine calculate_nt
!
!
!
 Subroutine read_headers (iin, nhead, lrewind, ifail)
!
! Reads header lines
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input-file unit number -
   Integer, Intent(In) :: nhead ! - number of header lines -
!
   Logical, Intent(In) :: lrewind ! - rewind? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: k ! - time index -
!
! Executable Statements
!
! Rewind
   If (lrewind) Rewind (Unit=iin)
!
! Read header lines
   If (nhead > 0) Then
      Do k = 1, nhead
         Read (Unit=iin, Fmt=*, Err=1, End=2)
      End Do
   End If
!
   ifail = 0
   Return
!
! Errors
! - problem reading file -
1  ifail = 1
   Return
!
! - end of file -
2  ifail = 2
   Return
!
 End Subroutine read_headers
!
!
!
 Subroutine read_tags (ctag, ntags, ctags, lctagt, afield, ifail, cfail, &
            bfield, icat, idstr, nt)
!
! Reads CPT file tag values
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Invalid combination of tags
!    ifail = -2 Missing optional argument nt for nrow and/or ncol tags
!    ifail = -3 Missing optional argument icat for C and/or clim_prob tags
!    ifail =  1 Unable to read cfail tag
!    ifail =  2 Unable to find cfail tag (if tag is a required field, otherwise ifail = 0 and default value is specified)
!    ifail =  3 Invalid value for cfail tag
!    ifail =  4 Value for crow is icompatible with ccol='X' or ccol='lon'
!    ifail =  5 Value for crow is icompatible with ccol='station'
!    ifail =  6 Value for crow is icompatible with ccol='index'
!    ifail =  7 Unknown value for ccol
!
! Modules
   Use data_numbers,      Only: zero
   Use data_io_constants, Only: ldat
   Use labels,            Only: cg_na, cg_none_l, cg_undefined_l
   Use time,              Only: get_pdate
   Use fields,            Only: field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ntags ! - number of desired tags -
!
   Character(Len=*), Intent(In) :: ctag ! - tag line -
!
   Logical, Intent(In) :: lctagt ! - cpt:T tag required? -
!
! - input arrays -
   Character(Len=*), Dimension(:), Intent(In) :: ctags ! - desired tags -
!
! - input/output scalars -
   Type(field), Intent(InOut) :: afield ! - new field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: icat ! - current category (category 1 is used if unspecified) -
!
   Type(field), Intent(In), Optional :: bfield ! - default field -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: nt ! - number of time steps -
!
! - optional output scalars -
   Integer, Intent(Out), Optional :: idstr ! - dataset structure indicator -
!
   Character(Len=*), Intent(Out) :: cfail ! - problem tag -
!
! Locals
   Integer :: i    ! - tag index -
   Integer :: i1   ! - locator -
   Integer :: i2   ! - locator -
   Integer :: i3   ! - trimmed length -
   Integer :: ic   ! - category -
   Integer :: ncol ! - number of columns -
   Integer :: nrow ! - number of rows -
   Integer :: ierr ! - error indicator -
!
   Integer, SAVE :: igrd ! - data structure indicator -
!
   Real(Kind=rp) :: r ! - real input value -
!
   Character(Len=ldat) :: ccol ! - columns -
   Character(Len=ldat) :: crow ! - rows -
!
   Logical :: lcol   ! - columns set? -
   Logical :: lrow   ! - rows set? -
   Logical :: lncol  ! - number of columns set? -
   Logical :: lnrow  ! - number of rows set? -
   Logical :: lsdate ! - start date set? -
   Logical :: lfdate ! - forecast date set? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Iachar, Index, Len, Len_Trim, MaxVal, Nint, Present, Scan, Trim
!
! Executable Statements
!
! Initialise
   lcol = .false.
   lrow = .false.
   lncol = .false.
   lnrow = .false.
   lsdate = .false.
   lfdate = .false.
   i3 = Len_Trim(ctag)
   Do
      If (Iachar(ctag(i3:i3)) == 9 .or. ctag(i3:i3) == '"') Then ! - ignore trailing tabs and inverted commas -
         i3 = i3 - 1
      Else
         Exit
      End If
   End Do
   If (Present(icat)) Then
      ic = icat
   Else
      ic = 1
   End If
   If (Present(bfield)) afield = bfield
!
! Repeat for each tag
   ifail = 1
   Do i = 1, ntags
      cfail = ctags(i)
      Select Case (Trim(ctags(i)))
!
! Current date
       Case ('T')
         Call get_pdate ('T', ctag(1:i3), afield%tssn%tprd(1)%sdate, ierr, &
              edate=afield%tssn%tprd(1)%edate)
         Select Case (ierr)
          Case (0)
            Continue
          Case (1)
            If (lctagt) Then
               ifail = 2
               Return
            Else
               If (Present(bfield)) afield%tssn%tprd(1) = bfield%tssn%tprd(1)
            End If
          Case (2)
            ifail = 1
            Return
          Case (3)
            ifail = ierr
            Return
         End Select
!
! Start date for model data
       Case ('S')
         If (lfdate) Cycle
         Call get_pdate ('S', ctag(1:i3), afield%mdate, ierr)
         Select Case (ierr)
          Case (0)
            lsdate = .true.
          Case (1)
            If (Present(bfield)) afield%mdate = bfield%mdate
            lsdate = .true.
          Case (2)
            ifail = 1
            Return
          Case (3)
            ifail = ierr
            Return
         End Select
!
! Forecast date
       Case ('F')
         If (lsdate) Cycle
         Call get_pdate ('F', ctag(1:i3), afield%mdate, ierr)
         Select Case (ierr)
          Case (0)
            lfdate = .true.
          Case (1)
            If (Present(bfield)) afield%mdate = bfield%mdate
            lfdate = .true.
          Case (2)
            ifail = 1
            Return
          Case (3)
            ifail = ierr
            Return
         End Select
!
! Category
       Case ('C')
         If (.not.Present(icat)) Then
            ifail = -3
            Return
         End If
         i1 = Index(ctag(1:), 'cpt:C=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:C=')
            i2 = i1 + Scan(ctag(i1:), ', ') - 2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt=*, Err=1) afield%icat(ic)
         Else
            If (Present(bfield)) Then
               afield%icat(ic) = bfield%icat(ic)
            Else
               afield%icat(ic) = 0
            End If
         End If
!
! Climatological probability
       Case ('clim_prob')
         If (.not.Present(icat)) Then
            ifail = -3
            Return
         End If
         i1 = Index(ctag(1:), 'cpt:clim_prob=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:clim_prob=')
            i2 = i1 + Scan(ctag(i1:), ', ') - 2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt=*, Err=1) afield%prob(ic)
         Else
            If (Present(bfield)) Then
               afield%prob(ic) = bfield%prob(ic)
            Else
               afield%prob(ic) = zero
            End If
         End If
!
! Level
       Case ('Z')
         i1 = MaxVal([Index(ctag(1:), 'cpt:Z='), Index(ctag(1:), 'cpt:zlev='), Index(ctag(1:), 'cpt:P=')])
         If (i1 > 0) Then
            i1 = i1 + Index(ctag(i1:), '=')
            i2 = i1 + Scan(ctag(i1:), ', ') - 1
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt=*, IOstat=ifail) afield%z%hght, afield%z%unit
            If (ifail /= 0) Then
               Read (Unit=ctag(i1:i2), Fmt=*, Err=1) afield%z%hght
               afield%z%unit = Trim(cg_none_l)
            End If
            ifail = 1
         Else
            If (Present(bfield)) Then
               afield%z%hght = bfield%z%hght
               afield%z%unit = bfield%z%unit
            Else
               afield%z%hght = zero
               afield%z%unit = Trim(cg_none_l)
            End If
         End If
!
! Ensemble member
       Case ('M')
         i1 = Index(ctag(1:), 'cpt:M=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:M=')
            i2 = i1 + Scan(ctag(i1:), ', ')-2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt=*, Err=1) r
            afield%member = Nint(r)
         Else
            If (Present(bfield)) Then
               afield%member = bfield%member
            Else
               afield%member = 0
            End If
         End If
!
! Model
       Case ('model')
         i1 = Index(ctag(1:), 'cpt:model=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:model=')
            i2 = i1 + Scan(ctag(i1:), ', ') - 2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) afield%model
         Else
            If (Present(bfield)) Then
               afield%model = bfield%model
            Else
               afield%model = cg_na
            End If
         End If
!
! Data structure
! - columns -
       Case ('col')
         i1 = Index(ctag(1:), 'cpt:col=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:col=')
            i2 = i1 + Scan(ctag(i1:), ', ') - 2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) ccol
            lcol = .true.
         Else
            If (.not.Present(bfield)) ifail = -2
         End If
! - rows -
       Case ('row')
         i1 = Index(ctag(1:), 'cpt:row=')
         If (i1 > 0) Then
            i1 =i1 + Len('cpt:row=')
            i2 =i1 + Scan(ctag(i1:), ', ') - 2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) crow
            lrow = .true.
         Else
            If (.not.Present(bfield)) ifail = -2
         End If
!
! Identify number of latitudes and longitudes / number of stations / indices
! - number of columns -
       Case ('ncol')
         i1 = Index(ctag(1:), 'cpt:ncol=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:ncol=')
            Read (Unit=ctag(i1:), Fmt=*, Err=1) ncol
            lncol = .true.
         Else
            If (.not.Present(bfield)) ifail = 2
         End If
! - number of rows -
       Case ('nrow')
         i1 = Index(ctag(1:), 'cpt:nrow=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:nrow=')
            Read (Unit=ctag(i1:), Fmt=*, Err=1) nrow
            lnrow = .true.
         Else
            If (.not.Present(bfield)) ifail = 2
         End If
!
! Field variable
       Case ('field')
         i1 = Index(ctag(1:), 'cpt:field=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:field=')
            i2 = i1 + Scan(ctag(i1:), ', ')-2
            If (i2 < i1) i2=i3
            Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) afield%var
         Else
            If (Present(bfield)) Then
               afield%var = bfield%var
            Else
               afield%var = Trim(cg_undefined_l)
            End If
         End If
!
! Variable units
       Case ('units')
         i1 = Index(ctag(1:), 'cpt:units=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:units=')
            i2 = i1 + Scan(ctag(i1:), ', ')-2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i2), Fmt='(A)', Err=1) afield%unit
         Else
            If (Present(bfield)) Then
               afield%unit = bfield%unit
            Else
               afield%unit = Trim(cg_undefined_l)
            End If
         End If
!
! Missing value flag
       Case ('missing')
         i1 = Index(ctag(1:), 'cpt:missing=')
         If (i1 > 0) Then
            i1 = i1 + Len('cpt:missing=')
            i2 = i1 + Scan(ctag(i1:), ', ') - 2
            If (i2 < i1) i2 = i3
            Read (Unit=ctag(i1:i3), Fmt=*, Err=1) afield%rmiss
         End If
      End Select
   End Do
   If (ifail /= 1) Return
   cfail = ' '
!
! Check for valid combination of requested tags
   If ((lrow .eqv. lcol) .and. (lnrow .eqv. lncol)) Then
      If (lcol) Then
         Call get_idstr (ccol, crow, igrd, ifail)
         If (ifail /= 0) Then
            ifail = ifail + 3
            Return
         End If
         If (Present(idstr)) idstr = igrd
      End If
      If (lcol .or. Present(bfield)) Then
         Select Case (igrd)
          Case (1) ! - gridded data -
            If (lncol) afield%nlg =ncol
            If (lnrow) afield%nlt = nrow
          Case (2) ! - station data -
            If (lncol) Then
               afield%nlt = ncol
               afield%nlg = ncol
            End If
            If (lnrow) nt = nrow
          Case (3) ! - station and unreferenced data -
            If (lncol) Then
               afield%nlt = ncol
               afield%nlg = ncol
            End If
            If (lnrow) Then
               If (.not.Present(nt)) Then
                  ifail = -2
                  Return
               End If
               nt = nrow
            End If
         End Select
         ifail = 0
      Else If (lncol .and. .not.Present(bfield)) Then
         ifail = -1
      Else
         ifail = 0
      End If
   Else If (lnrow .or. lncol) Then
      If (Present(bfield)) Then
         Select Case (igrd)
          Case (1) ! - gridded data -
            If (lncol) afield%nlg = ncol
            If (lnrow) afield%nlt = nrow
          Case (2) ! - station data -
            If (lncol) Then
               afield%nlt = ncol
               afield%nlg = ncol
            End If
            If (lnrow) nt = nrow
          Case (3) ! - station and unreferenced data -
            If (lncol) Then
               afield%nlt = ncol
               afield%nlg = ncol
            End If
            If (lnrow) Then
               If (.not.Present(nt)) Then
                  ifail = -2
                  Return
               End If
               nt = nrow
            End If
         End Select
      Else
         ifail = -1
      End If
   Else
      ifail = -1
   End If
!
! Errors
1  Return
!
 Contains
!
!
 Subroutine get_idstr (ccol, crow, idstr, ifail)
!
! Determines file structure
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Value for crow is icompatible with ccol='X' or ccol='lon'
!    ifail =  2 Value for crow is icompatible with ccol='station'
!    ifail =  3 Value for crow is icompatible with ccol='index'
!    ifail =  4 Unknown value for ccol
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=ldat), Intent(In) :: ccol ! - columns -
   Character(Len=ldat), Intent(In) :: crow ! - rows -
!
! - output scalars -
   Integer, Intent(Out) :: idstr ! - file structure -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Determine structure
   idstr = 0
   Select Case (Trim(ccol))
    Case ('X', 'lon')
      Select Case (Trim(crow))
       Case ('Y', 'lat') ! - gridded file -
         idstr = id_grd
       Case Default
         ifail = 1
         Return
      End Select
    Case ('station')
      Select Case (Trim(crow))
       Case ('T') ! - station file -
         idstr = id_stn
       Case Default
         ifail = 2
         Return
      End Select
    Case ('index')
      Select Case (Trim(crow))
       Case ('T') ! - unreferenced file -
         idstr = id_ind
       Case Default
         ifail = 3
         Return
      End Select
    Case Default
      ifail = 4
      Return
   End Select
   ifail = 0
!
   Return
  End Subroutine get_idstr
 End Subroutine read_tags
#if FTN95 == 1
! FTN95 bug
!
!
 Function diff_pprds(p1, p2) &
          Result (differ)
!
! Different parsed periods?
!
! Modules
   Use time, Only: pprd, &
                   same_pprd
!
! Function result
   Logical :: differ
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(pprd), Intent(In) :: p1 ! - first period -
   Type(pprd), Intent(In) :: p2 ! - second period -
!
! Executable Statements
!
! Compare periods
   differ = .true.
   If (p1%sdate%iyr /= p2%sdate%iyr) Return
   If (p1%sdate%imn /= p2%sdate%imn) Return
   If (p1%sdate%idy /= p2%sdate%idy) Return
   If (p1%edate%iyr /= p2%edate%iyr) Return
   If (p1%edate%imn /= p2%edate%imn) Return
   If (p1%edate%idy /= p2%edate%idy) Return
   differ = .false.
!
   Return
 End Function diff_pprds
#endif
End Module file_structure
