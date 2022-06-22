												!
! CPT_formatV11 Module of Subroutines is used to save data in CPT V11 formats. 
! The output data are ASCII values. The user can Call the following Subroutines 
! to create appropriate data file.
! 
! 1. Write_cpt_grid_v11  -->  gridded data
! 2. Write_cpt_stns_v11  -->  station data
! 3. Write_cpt_unrf_v11  -->  unreferenced data
!
! The data can be provided in single or Double precision. different versions of
! each routine are provided depending upon whether the data contain multiple fields
! and or lagged fields.
!
! Use of these Subroutines:
! Include the following statement at the beginning of Subroutines or Modules.
!
!   Use CPT_formatV11
!
! Each routine requests information about the date for the first data in the file.
! In Case of seasonal averages the date format involves a start and end date. For
! example, If the data are DJF averages from 1971/72 to 2000/01, Then the first 
! data are for DJF 1971/72. The start date for the first data is therefore December 
! 1971 and the end date is Feburary 1972. The following values would Then be set:
! 
!  period1_sdate_iyr = 1971
!  period1_sdate_imn = 12
!  period1_sdate_idy = 0
!  period1_edate_iyr = 1972
!  period1_edate_imn = 2
!  period1_edate_idy = 0
!
! BecaUse it is implicit that the whole of December and the whole of February
! are included the days of the month are not needed and so can be set to 0.
!
! See the CPT help pages for more details on the dates.
!
! If there are dates with only missing values, Then the output files can be saved
! with these dates omitted to save space. In this Case, the Optional argument, kuse,
! should be passed, and the argument nt set to the total number of Cases including
! the ones to be omitted. The input data, v, should be compressed (i.e., it should 
! not contain these missing Cases), but kuse should be set for each Case (.true. for
! the Cases that are present, and .false. for those that are missing and should be
! omitted). Note that for daily data, leap years are Counted according to the 
! British implementation of the Gregorian calendar (see Function ndays).
!
! These Subroutines and Functions are written by Simon Mason and ModIfied by Lulin
! Song.
!
! This file was first created by Lulin Song on April 1, 2010.
! ModIfied by Simon Mason on 24 September, 2021.
! ModIfied by Simon Mason on 20 April, 2010.
! ModIfied by Simon Mason on 13 May, 2010.
! ModIfied by Simon Mason on 16 May, 2011.
! ModIfied by Simon Mason on 03 October, 2011.
! ModIfied by Simon Mason on 11 May, 2011.
!
! $Id$
Module numbers
!
! Implicit declarations
  Implicit None
!
! Parameters
!
! - Private Parameters -
  Integer, Parameter, Public :: sp = Kind(1.0e0)          ! - single precision Kind Parameter -
  Integer, Parameter, Public :: dp = Kind(1.0d0)          ! - Double precision Kind Parameter -
!
! Real Parameters
  Real(Kind=sp), Parameter, Public :: zero_sp =   0.0_sp  ! - zero -
  Real(Kind=sp), Parameter, Public ::  one_sp =   1.0_sp  ! - one -
  Real(Kind=sp), Parameter, Public ::  ten_sp =  10.0_sp  ! - ten -
  Real(Kind=sp), Parameter, Public :: r360_sp = 360.0_sp  ! - 360 degrees -
!
  Real(Kind=dp), Parameter, Public :: zero_dp =   0.0_dp  ! - zero -
  Real(Kind=dp), Parameter, Public ::  one_dp =   1.0_dp  ! - one -
  Real(Kind=dp), Parameter, Public ::  ten_dp =  10.0_dp  ! - ten -
  Real(Kind=dp), Parameter, Public :: r360_dp = 360.0_dp  ! - 360 degrees -
!
End Module numbers
!
!
!
Module CPT_formatV11
!
! Modules
  Use numbers
!
! Implicit declarations
  Implicit None
!
! Parameters
!
! Integer Parameters
! - Public Parameters -
  Integer, Parameter, Public :: lvar = 32 ! - maximum length of variable name -
  Integer, Parameter, Public :: lstn = 16 ! - maximum length of station name -
!
! - Private Parameters -
  Integer, Parameter, Private ::  mwu = 29 ! - maximum width of output field -
  Integer, Parameter, Private ::  nmn = 12 ! - number of months -
  Integer, Parameter, Private :: ldat = 12 ! - maximum length of date -
  Integer, Parameter, Private :: lprd = 25 ! - maximum length of period -
!
! Character Parameters
  Character(Len=32), Parameter, Private :: cxmlns_cpt = & ! - CPT XML namespace
     'http://iri.columbia.edu/CPT/v10/'
!
! Scalars
!
! Integer scalars
  Integer, Private :: iseq ! - time sequence identIfier -
  Integer, Private :: iout ! - output unit number -
!
! Character scalars
  Character(Len=mwu), Private :: cout ! - output field -
!
! Logical scalars
  Logical, Private :: lopen ! - file opened flag -
!
! Derived Type definitions
!
! - date -
  Type date
     Integer :: iyr ! - year -
     Integer :: imn ! - month -
     Integer :: idy ! - day -
  End Type date
!
! - period -
  Type period
     Type(date) :: sdate ! - start date -
     Type(date) :: edate ! - End date -
  End Type period
!
! - level -
  Type level
     Real(Kind=dp) :: hght    ! - height -
!
     Character(Len=5) :: unit ! - units -
  End Type level
!
! Interfaces
!
! Interface Operators
  Interface Operator( == )
     Module Procedure same_date
     Module Procedure equal_date
  End Interface
!
  Interface Operator( < )
     Module Procedure lt_date
  End Interface
!
  Interface Operator( > )
     Module Procedure gt_date
  End Interface
!
  Interface Operator( + )
     Module Procedure add_date
  End Interface
!
  Interface Operator( + )
     Module Procedure add_period
  End Interface
!
! Generic Interfaces
  Interface Write_cpt_grid_v11
    Module Procedure Write_cpt_grid_v11_sp
    Module Procedure Write_cpt_grid_v11_dp
    Module Procedure Write_cpt_grid_lags_v11_sp
    Module Procedure Write_cpt_grid_lags_v11_dp
    Module Procedure Write_cpt_grid_fields_v11_sp
    Module Procedure Write_cpt_grid_fields_v11_dp
    Module Procedure Write_cpt_grid_fields_lags_v11_sp
    Module Procedure Write_cpt_grid_fields_lags_v11_dp
  End Interface
!
  Interface Write_cpt_stns_v11
    Module Procedure Write_cpt_stns_v11_sp
    Module Procedure Write_cpt_stns_v11_dp
    Module Procedure Write_cpt_stns_lags_v11_sp
    Module Procedure Write_cpt_stns_lags_v11_dp
    Module Procedure Write_cpt_stns_fields_v11_sp
    Module Procedure Write_cpt_stns_fields_v11_dp
    Module Procedure Write_cpt_stns_fields_lags_v11_sp
    Module Procedure Write_cpt_stns_fields_lags_v11_dp
  End Interface
!
  Interface Write_cpt_unrf_v11
    Module Procedure Write_cpt_unrf_v11_sp
    Module Procedure Write_cpt_unrf_v11_dp
    Module Procedure Write_cpt_unrf_lags_v11_sp
    Module Procedure Write_cpt_unrf_lags_v11_dp
  End Interface
!
  Interface iprec
    Module Procedure iprec_sp
    Module Procedure iprec_dp
  End Interface iprec
!
  Interface get_cdate
    Module Procedure get_cdate_date
    Module Procedure get_cdate_period
  End Interface get_cdate
!
Contains
!
!
 Subroutine Write_cpt_grid_v11_sp ( outfile, nt, nlt, nlg, tseq, v, miss, rlat, rlng, var, unit, &
                                    period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                    period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                    ifail, &
                                    mdate_iyr, mdate_imn, mdate_idy, &
                                    z_hght, z_unit, &
                                    kuse &
                                  )
!
! Modules
  Use numbers, Only: rp=>sp, dp, r360=>r360_sp
!
! Outputs gridded data
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Integer, Intent(In) :: period1_sdate_iyr ! - start year of first period -
  Integer, Intent(In) :: period1_sdate_imn ! - start month of first period -
  Integer, Intent(In) :: period1_sdate_idy ! - start day of first period -
  Integer, Intent(In) :: period1_edate_iyr ! - End year of first period -
  Integer, Intent(In) :: period1_edate_imn ! - End month of first period -
  Integer, Intent(In) :: period1_edate_idy ! - End day of first period -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! - Optional input scalars -
  Integer, Intent(In), Optional :: mdate_iyr ! - year made ('start date' for model forecasts) -
  Integer, Intent(In), Optional :: mdate_imn ! - month made ('start date' for model forecasts) -
  Integer, Intent(In), Optional :: mdate_idy ! - day made ('start date' for model forecasts) -
!
  Real(Kind=rp), Intent(In), Optional :: z_hght    ! - atmoshperic level -- height -
!
  Character(Len=*), Intent(In), Optional :: z_unit ! - atmoshperic level -- units -
!
! Input arrays
  Real(Kind=rp), Intent(In) :: v(:,:,:) ! - data (minimum dimensions: nlg, nlt, nn) -
  Real(Kind=rp), Intent(In) :: rlat(:)  ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)  ! - longitudes (minimum dimensions: nlg) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: i  ! - latitude index -
  Integer :: j  ! - longitude index -
  Integer :: k  ! - time index -
  Integer :: nn ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
  Type(date) :: mdate1 ! - first forecast date -
!
  Type(level) ::  z ! - level -
!
  Type(period) :: period1 ! - first period -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq  =  tseq
!
! Initialise starting period
  period1%sdate%iyr = period1_sdate_iyr
  period1%sdate%imn = period1_sdate_imn
  period1%sdate%idy = period1_sdate_idy
  period1%edate%iyr = period1_edate_iyr
  period1%edate%imn = period1_edate_imn
  period1%edate%idy = period1_edate_idy
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     mdate1%iyr = mdate_iyr
     mdate1%imn = mdate_imn
     mdate1%idy = mdate_idy
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If (Present(z_hght) .and. Present(z_unit)) Then
     z%hght = z_hght
     z%Unit = z_unit
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), 1, ifail)
  If (ifail /= 0) Return
!       
! Print gridded data
  nn = 0
  Do k = 1, nt
     If (Present(kuse)) Then
        If (.not.kuse(k)) Cycle
     End If
     nn = nn + 1
     If (lmdate) Then
        If (lz) Then
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_z=z, cpt_s = mdate1+(k-1), cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
        Else
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_s=mdate1+(k-1), cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
        End If
     Else
        If (lz) Then
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_z=z, cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
        Else
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
        End If
     End If
     If (ifail /= 0) GoTo 1
     Do j = 1, nlg
        If (rlng(j) > r360) Then
           Write (cout, Fmt=*) rlng(j) - r360
        Else
           Write (cout, Fmt=*) rlng(j)
        End If
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
     Do i = 1, nlt
        Write (cout, Fmt=*) rlat(i)
        Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
        Do j = 1, nlg
           Write (cout, Fmt=*) v(j,i,nn)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_v11_sp
!
!
!
 Subroutine Write_cpt_grid_v11_dp ( outfile, nt, nlt, nlg, tseq, v, miss, rlat, rlng, var, unit, &
                                    period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                    period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                    ifail, &
                                    mdate_iyr, mdate_imn,mdate_idy, &
                                    z_hght, z_unit, &
                                    kuse &
                                  )
!
! Modules
  Use numbers, Only: rp=>dp, r360=>r360_dp
!
! Outputs gridded data
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Integer, Intent(In) :: period1_sdate_iyr ! - start year of first period -
  Integer, Intent(In) :: period1_sdate_imn ! - start month of first period -
  Integer, Intent(In) :: period1_sdate_idy ! - start day of first period -
  Integer, Intent(In) :: period1_edate_iyr ! - End year of first period -
  Integer, Intent(In) :: period1_edate_imn ! - End month of first period -
  Integer, Intent(In) :: period1_edate_idy ! - End day of first period -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! - Optional input scalars -
  Integer, Intent(In), Optional :: mdate_iyr ! - year made ('start date' for model forecasts) -
  Integer, Intent(In), Optional :: mdate_imn ! - month made ('start date' for model forecasts) -
  Integer, Intent(In), Optional :: mdate_idy ! - day made ('start date' for model forecasts) -
!
  Real(Kind=rp), Intent(In), Optional :: z_hght    ! - atmoshperic level -- height -
!
  Character(Len=*), Intent(In), Optional :: z_unit ! - atmoshperic level -- units -
!
! Input arrays
  Real(Kind=rp), Intent(In) :: v(:,:,:) ! - data (minimum dimensions: nlg, nlt, nn) -
  Real(Kind=rp), Intent(In) :: rlat(:)  ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)  ! - longitudes (minimum dimensions: nlg) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: i  ! - latitude index -
  Integer :: j  ! - longitude index -
  Integer :: k  ! - time index -
  Integer :: nn ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
  Type(date) :: mdate1 ! - first forecast date -
!
  Type(level) ::  z ! - level -
!
  Type(period) :: period1 ! - first period -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  period1%sdate%iyr = period1_sdate_iyr
  period1%sdate%imn = period1_sdate_imn
  period1%sdate%idy = period1_sdate_idy
  period1%edate%iyr = period1_edate_iyr
  period1%edate%imn = period1_edate_imn
  period1%edate%idy = period1_edate_idy
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     mdate1%iyr = mdate_iyr
     mdate1%imn = mdate_imn
     mdate1%idy = mdate_idy
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     z%hght = z_hght
     z%Unit = z_unit
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), 1, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  nn = 0
  Do k = 1, nt
     If (Present(kuse)) Then
        If (.not.kuse(k)) Cycle
     End If
     nn = nn + 1
     If (lmdate) Then
        If (lz) Then
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_z=z, cpt_s=mdate1+(k-1), cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=miss)
        Else
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_s=mdate1+(k-1), cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=miss)
        End If
     Else
        If (lz) Then
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_z=z, cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=miss)
        Else
           Call write_tag (iout,ifail, &
                           cpt_field=Trim(var), cpt_t=period1+(k-1), &
                           cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                           cpt_units=Trim(unit), cpt_missing=miss)
        End If
     End If
     If (ifail /= 0) GoTo 1
     Do j = 1, nlg
        If (rlng(j) > r360) Then
           Write (cout, Fmt=*) rlng(j) - r360
        Else
           Write (cout, Fmt=*) rlng(j)
        End If
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
     Do i = 1, nlt
        Write (cout, Fmt=*) rlat(i)
        Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
        Do j = 1, nlg
           Write (cout, Fmt=*) v(j,i,nn)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_v11_dp
!
!
!
 Subroutine Write_cpt_grid_lags_v11_sp ( outfile, nt, nlt, nlg, nls, tseq, v, miss, rlat, rlng, var, unit, &
                                         period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                         period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                         ifail, &
                                         mdate_iyr, mdate_imn, mdate_idy, &
                                         z_hght, z_unit, &
                                         kuse &
                                       )
!
! Modules
  Use numbers, Only: rp=>sp, dp, r360=>r360_sp
!
! Outputs gridded data with additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: nls  ! - number of lags -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! - Optional input scalars -
  Real(Kind=rp), Intent(In), Optional :: z_hght    ! - atmoshperic level -- height -
!
  Character(Len=*), Intent(In), Optional :: z_unit ! - atmoshperic level -- units -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each lag (minimum dimensions: nls) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:) ! - data (minimum dimensions: nlg, nlt, nn, nls) -
  Real(Kind=rp), Intent(In) :: rlat(:)    ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)    ! - longitudes (minimum dimensions: nlg) -
!
! - Optional input arrays -
  Integer, Intent(In), Optional :: mdate_iyr(:) ! - year made ('start date' for model forecasts) (minimum dimensions: nls) -
  Integer, Intent(In), Optional :: mdate_imn(:) ! - month made ('start date' for model forecasts) (minimum dimensions: nls) -
  Integer, Intent(In), Optional :: mdate_idy(:) ! - day made ('start date' for model forecasts) (minimum dimensions: nls) -
!
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(date) :: mdate1(nls) ! - first forecast date -
!
  Type(period) :: period1(nls) ! - first period -
!
! Local scalars
  Integer :: i  ! - latitude index -
  Integer :: j  ! - longitude index -
  Integer :: k  ! - time index -
  Integer :: l  ! - lag index -
  Integer :: nn ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
  Type(level) ::  z ! - level -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do l = 1, nls
     period1(l)%sdate%iyr = period1_sdate_iyr(l)
     period1(l)%sdate%imn = period1_sdate_imn(l)
     period1(l)%sdate%idy = period1_sdate_idy(l)
     period1(l)%edate%iyr = period1_edate_iyr(l)
     period1(l)%edate%imn = period1_edate_imn(l)
     period1(l)%edate%idy = period1_edate_idy(l)
  End Do
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     Do l = 1, nls
        mdate1(l)%iyr = mdate_iyr(l)
        mdate1(l)%imn = mdate_imn(l)
        mdate1(l)%idy = mdate_idy(l)
     End Do
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     z%hght = z_hght
     z%Unit = z_unit
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), 1, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  nn = 0
  Do k = 1, nt
     Do l = 1, nls
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        If (lmdate) Then
           If (lz) Then
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), cpt_z=z, &
                              cpt_s=mdate1(l)+(k-1), cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
           Else
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), &
                              cpt_s=mdate1(l)+(k-1), cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
           End If
        Else
           If (lz) Then
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), cpt_z=z, &
                              cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
           Else
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), &
                              cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
           End If
        End If
        If (ifail /= 0) GoTo 1
        Do j = 1, nlg
           If (rlng(j) > r360) Then
              Write (cout, Fmt=*) rlng(j) - r360
           Else
              Write (cout, Fmt=*) rlng(j)
           End If
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        Do i = 1, nlt
           Write (cout, Fmt=*) rlat(i)
           Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
           Do j = 1, nlg
              Write (cout, Fmt=*) v(j,i,nn,l)
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_lags_v11_sp
!
!
!
 Subroutine Write_cpt_grid_lags_v11_dp ( outfile, nt, nlt, nlg, nls, tseq, v, miss, rlat, rlng, var, unit, &
                                         period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                         period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                         ifail, &
                                         mdate_iyr, mdate_imn, mdate_idy, &
                                         z_hght,z_unit, &
                                         kuse &
                                       )
!
! Modules
  Use numbers, Only: rp=>dp, r360=>r360_dp
!
! Outputs gridded data with additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: nls  ! - number of lags -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! - Optional input scalars -
  Real(Kind=rp), Intent(In), Optional :: z_hght    ! - atmoshperic level -- height -
!
  Character(Len=*), Intent(In), Optional :: z_unit ! - atmoshperic level -- units -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each lag (minimum dimensions: nls) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:) ! - data (minimum dimensions: nlg, nlt, nn, nls) -
  Real(Kind=rp), Intent(In) :: rlat(:)    ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)    ! - longitudes (minimum dimensions: nlg) -
!
! - Optional input arrays -
  Integer, Intent(In), Optional :: mdate_iyr(:) ! - year made ('start date' for model forecasts) (minimum dimensions: nls) -
  Integer, Intent(In), Optional :: mdate_imn(:) ! - month made ('start date' for model forecasts) (minimum dimensions: nls) -
  Integer, Intent(In), Optional :: mdate_idy(:) ! - day made ('start date' for model forecasts) (minimum dimensions: nls) -
!
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(date) :: mdate1(nls) ! - first forecast date -
!
  Type(period) :: period1(nls) ! - first period -
!
! Local scalars
  Integer :: i  ! - latitude index -
  Integer :: j  ! - longitude index -
  Integer :: k  ! - time index -
  Integer :: l  ! - lag index -
  Integer :: nn ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
  Type(level) ::  z ! - level -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do l = 1, nls
     period1(l)%sdate%iyr = period1_sdate_iyr(l)
     period1(l)%sdate%imn = period1_sdate_imn(l)
     period1(l)%sdate%idy = period1_sdate_idy(l)
     period1(l)%edate%iyr = period1_edate_iyr(l)
     period1(l)%edate%imn = period1_edate_imn(l)
     period1(l)%edate%idy = period1_edate_idy(l)
  End Do
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     Do l = 1, nls
        mdate1(l)%iyr = mdate_iyr(l)
        mdate1(l)%imn = mdate_imn(l)
        mdate1(l)%idy = mdate_idy(l)
     End Do
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     z%hght = z_hght
     z%unit = z_unit
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), 1, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  nn = 0
  Do k = 1, nt
     Do l = 1, nls
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        If (lmdate) Then
           If (lz) Then
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), cpt_z=z, &
                              cpt_s=mdate1(l)+(k-1), cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=miss)
           Else
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), &
                              cpt_s=mdate1(l)+(k-1), cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=miss)
           End If
        Else
           If (lz) Then
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), &
                              cpt_z=z, cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=miss)
           Else
              Call write_tag (iout,ifail, &
                              cpt_field=Trim(var), &
                              cpt_t=period1(l)+(k-1), &
                              cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                              cpt_units=Trim(unit), cpt_missing=miss)
           End If
        End If
        If (ifail /= 0) GoTo 1
        Do j = 1, nlg
           If (rlng(j) > r360) Then
              Write (cout, Fmt=*) rlng(j) - r360
           Else
              Write (cout, Fmt=*) rlng(j)
           End If
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        Do i = 1, nlt
           Write (cout, Fmt=*) rlat(i)
           Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
           Do j = 1, nlg
              Write (cout, Fmt=*) v(j,i,nn,l)
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_lags_v11_dp
!
!
!
 Subroutine Write_cpt_grid_fields_v11_sp ( outfile, nt, nlt, nlg, nfs, tseq, v, miss, rlat, rlng, var, unit, &
                                           period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                           period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                           lensemble, ifail, &
                                           mdate_iyr, mdate_imn, mdate_idy, &
                                           z_hght, z_unit, &
                                           kuse &
                                         )
!
! Modules
  Use numbers, Only: rp=>sp, dp, r360=>r360_sp
!
! Outputs gridded data with multiple fields
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
  Logical, Intent(In) :: lensemble ! - set to .true. If fields rePresent ensemble members -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each field (minimum dimensions: nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:) ! - data (minimum dimensions: nlg, nlt, nn, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:)    ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)    ! - longitudes (minimum dimensions: nlg) -
!
  Character(Len=*), Intent(In) :: var(:)  ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:) ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Integer, Intent(In), Optional :: mdate_iyr(:) ! - year made ('start date' for model forecasts) (minimum dimensions: nfs) -
  Integer, Intent(In), Optional :: mdate_imn(:) ! - month made ('start date' for model forecasts) (minimum dimensions: nfs) -
  Integer, Intent(In), Optional :: mdate_idy(:) ! - day made ('start date' for model forecasts) (minimum dimensions: nfs) -
!
  Real(Kind=rp), Intent(In), Optional :: z_hght(:) ! - atmoshperic level -- height (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In), Optional :: z_unit(:) ! - atmoshperic level -- units (minimum dimensions: nfs) -
!
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(date) :: mdate1(nfs) ! - first forecast date -
!
  Type(period) :: period1(nfs) ! - first period -
!
  Type(level) ::  z(nfs) ! - level -
!
! Local scalars
  Integer :: i   ! - latitude index -
  Integer :: j   ! - longitude index -
  Integer :: k   ! - time index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     period1(ifd)%sdate%iyr = period1_sdate_iyr(ifd)
     period1(ifd)%sdate%imn = period1_sdate_imn(ifd)
     period1(ifd)%sdate%idy = period1_sdate_idy(ifd)
     period1(ifd)%edate%iyr = period1_edate_iyr(ifd)
     period1(ifd)%edate%imn = period1_edate_imn(ifd)
     period1(ifd)%edate%idy = period1_edate_idy(ifd)
  End Do
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     Do ifd = 1, nfs
        mdate1(ifd)%iyr = mdate_iyr(ifd)
        mdate1(ifd)%imn = mdate_imn(ifd)
        mdate1(ifd)%idy = mdate_idy(ifd)
     End Do
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     Do ifd = 1, nfs
        z(ifd)%hght = z_hght(ifd)
        z(ifd)%unit = z_unit(ifd)
     End Do
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  Do ifd = 1, nfs
     nn = 0
     Do k = 1, nt
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        If (lmdate) Then
           If (lz) Then
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           Else
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           End If
        Else
           If (lz) Then
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           Else
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           End If
        End If
        If (ifail /= 0) GoTo 1
        Do j = 1, nlg
           If (rlng(j) > r360) Then
              Write (cout, Fmt=*) rlng(j) - r360
           Else
              Write (cout, Fmt=*) rlng(j)
           End If
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        Do i = 1, nlt
           Write (cout, Fmt=*) rlat(i)
           Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
           Do j = 1, nlg
              Write (cout, Fmt=*) v(j,i,nn,ifd)
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_fields_v11_sp
!
!
!
 Subroutine Write_cpt_grid_fields_v11_dp ( outfile, nt, nlt, nlg, nfs, tseq, v, miss, rlat, rlng, var, unit, &
                                           period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                           period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                           lensemble,ifail, &
                                           mdate_iyr, mdate_imn, mdate_idy, &
                                           z_hght, z_unit, &
                                           kuse &
                                         )
!
! Modules
  Use numbers, Only: rp=>dp, r360=>r360_dp
!
! Outputs gridded data with multiple fields
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
  Logical, Intent(In) :: lensemble ! - set to .true. If fields rePresent ensemble members -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each field (minimum dimensions: nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:) ! - data (minimum dimensions: nlg, nlt, nn, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:)    ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)    ! - longitudes (minimum dimensions: nlg) -
!
  Character(Len=*), Intent(In) :: var(:)  ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:) ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Integer, Intent(In), Optional :: mdate_iyr(:) ! - year made ('start date' for model forecasts) (minimum dimensions: nfs) -
  Integer, Intent(In), Optional :: mdate_imn(:) ! - month made ('start date' for model forecasts) (minimum dimensions: nfs) -
  Integer, Intent(In), Optional :: mdate_idy(:) ! - day made ('start date' for model forecasts) (minimum dimensions: nfs) -
!
  Real(Kind=rp), Intent(In), Optional :: z_hght(:) ! - atmoshperic level -- height (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In), Optional :: z_unit(:) ! - atmoshperic level -- units (minimum dimensions: nfs) -
!
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(date) :: mdate1(nfs) ! - first forecast date -
!
  Type(period) :: period1(nfs) ! - first period -
!
  Type(level) ::  z(nfs) ! - level -
!
! Local scalars
  Integer :: i   ! - latitude index -
  Integer :: j   ! - longitude index -
  Integer :: k   ! - time index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     period1(ifd)%sdate%iyr = period1_sdate_iyr(ifd)
     period1(ifd)%sdate%imn = period1_sdate_imn(ifd)
     period1(ifd)%sdate%idy = period1_sdate_idy(ifd)
     period1(ifd)%edate%iyr = period1_edate_iyr(ifd)
     period1(ifd)%edate%imn = period1_edate_imn(ifd)
     period1(ifd)%edate%idy = period1_edate_idy(ifd)
  End Do
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     Do ifd = 1, nfs
        mdate1(ifd)%iyr = mdate_iyr(ifd)
        mdate1(ifd)%imn = mdate_imn(ifd)
        mdate1(ifd)%idy = mdate_idy(ifd)
     End Do
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     Do ifd = 1, nfs
        z(ifd)%hght = z_hght(ifd)
        z(ifd)%unit = z_unit(ifd)
     End Do
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  Do ifd = 1, nfs
     nn = 0
     Do k = 1, nt
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        If (lmdate) Then
           If (lz) Then
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           Else
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), &
                                 cpt_s=mdate1(ifd)+(k-1), cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           End If
        Else
           If (lz) Then
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           Else
              If (lensemble) Then
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              Else
                 Call write_tag (iout,ifail, &
                                 cpt_field=Trim(var(ifd)), &
                                 cpt_t=period1(ifd)+(k-1), &
                                 cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                 cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
              End If
           End If
        End If
        If (ifail /= 0) GoTo 1
        Do j = 1, nlg
           If (rlng(j) > r360) Then
              Write (cout, Fmt=*) rlng(j) - r360
           Else
              Write (cout, Fmt=*) rlng(j)
           End If
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        Do i = 1, nlt
           Write (cout, Fmt=*) rlat(i)
           Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
           Do j = 1, nlg
              Write (cout, Fmt=*) v(j,i,nn,ifd)
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_fields_v11_dp
!
!
!
 Subroutine Write_cpt_grid_fields_lags_v11_sp ( outfile, nt, nlt, nlg, nls, nfs, tseq, v, miss, rlat, rlng, var, unit, &
                                                period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                                period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                                lensemble, ifail, &
                                                mdate_iyr, mdate_imn, mdate_idy, &
                                                z_hght, z_unit, &
                                                kuse &
                                              )
!
! Modules
  Use numbers, Only: rp=>sp, dp, r360=>r360_sp
!
! Outputs gridded data with multiple fields and additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: nls  ! - number of lags -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
!
  Logical, Intent(In) :: lensemble ! - set to .true. If fields rePresent ensemble members -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:,:) ! - start year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:,:) ! - start month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:,:) ! - start day of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:,:) ! - End year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_imn(:,:) ! - End month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_idy(:,:) ! - End day of first period for each field (minimum dimensions: nls, nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:,:) ! - data (minimum dimensions: nlg, nlt, nn, nls, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:)      ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)      ! - longitudes (minimum dimensions: nlg) -
!
  Character(Len=*), Intent(In) :: var(:)  ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:) ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Integer, Intent(In), Optional :: mdate_iyr(:,:) ! - year made ('start date' for model forecasts) (minimum dimensions: nls, nfs) -
  Integer, Intent(In), Optional :: mdate_imn(:,:) ! - month made ('start date' for model forecasts) (minimum dimensions: nls, nfs) -
  Integer, Intent(In), Optional :: mdate_idy(:,:) ! - day made ('start date' for model forecasts) (minimum dimensions: nls, nfs) -
!
  Real(Kind=rp), Intent(In), Optional :: z_hght(:) ! - atmoshperic level -- height (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In), Optional :: z_unit(:) ! - atmoshperic level -- units (minimum dimensions: nfs) -
!
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(date) :: mdate1(nls,nfs) ! - first forecast date -
!
  Type(period) :: period1(nls,nfs) ! - first period -
!
  Type(level) ::  z(nfs) ! - level -
!
! Local scalars
  Integer :: i   ! - latitude index -
  Integer :: j   ! - longitude index -
  Integer :: k   ! - time index -
  Integer :: l   ! - lag index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     Do l = 1, nls
        period1(l,ifd)%sdate%iyr = period1_sdate_iyr(l,ifd)
        period1(l,ifd)%sdate%imn = period1_sdate_imn(l,ifd)
        period1(l,ifd)%sdate%idy = period1_sdate_idy(l,ifd)
        period1(l,ifd)%edate%iyr = period1_edate_iyr(l,ifd)
        period1(l,ifd)%edate%imn = period1_edate_imn(l,ifd)
        period1(l,ifd)%edate%idy = period1_edate_idy(l,ifd)
     End Do
  End Do
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     Do ifd = 1, nfs
        Do l = 1, nls
           mdate1(l,ifd)%iyr = mdate_iyr(l,ifd)
           mdate1(l,ifd)%imn = mdate_imn(l,ifd)
           mdate1(l,ifd)%idy = mdate_idy(l,ifd)
        End Do
     End Do
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     Do ifd = 1, nfs
        z(ifd)%hght = z_hght(ifd)
        z(ifd)%unit = z_unit(ifd)
     End Do
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  Do ifd = 1, nfs
     nn = 0
     Do k = 1, nt
        Do l = 1, nls
           If (Present(kuse)) Then
              If (.not.kuse(k)) Cycle
           End If
           nn = nn + 1
           If (lmdate) Then
              If (lz) Then
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              Else
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              End If
           Else
              If (lz) Then
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              Else
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              End If
           End If
           If (ifail /= 0) GoTo 1
           Do j = 1, nlg
              If (rlng(j) > r360) Then
                 Write (cout, Fmt=*) rlng(j) - r360
              Else
                 Write (cout, Fmt=*) rlng(j)
              End If
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
           Do i = 1, nlt
              Write (cout, Fmt=*) rlat(i)
              Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
              Do j = 1, nlg
                 Write (cout, Fmt=*) v(j,i,nn,l,ifd)
                 Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
              End Do
              Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
           End Do
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_fields_lags_v11_sp
!
!
!
 Subroutine Write_cpt_grid_fields_lags_v11_dp ( outfile, nt, nlt, nlg, nls, nfs, tseq, v, miss, rlat, rlng, var, unit, &
                                                period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                                period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                                lensemble, ifail, &
                                                mdate_iyr, mdate_imn, mdate_idy, &
                                                z_hght, z_unit, &
                                                kuse &
                                              )
!
! Modules
  Use numbers, Only: rp=>dp, r360=>r360_dp
!
! Outputs gridded data with multiple fields and additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nlt  ! - total number of latitudes of each field and lead-time -
  Integer, Intent(In) :: nlg  ! - total number of longitudes of each field and lead-time -
  Integer, Intent(In) :: nls  ! - number of lags -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
  Logical, Intent(In) :: lensemble ! - set to .true. If fields rePresent ensemble members -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:,:) ! - start year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:,:) ! - start month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:,:) ! - start day of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:,:) ! - End year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_imn(:,:) ! - End month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_idy(:,:) ! - End day of first period for each field (minimum dimensions: nls, nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:,:) ! - data (minimum dimensions: nlg, nlt, nn, nls, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:)      ! - latitudes (minimum dimensions: nlt) -
  Real(Kind=rp), Intent(In) :: rlng(:)      ! - longitudes (minimum dimensions: nlg) -
!
  Character(Len=*), Intent(In) :: var(:)  ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:) ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Integer, Intent(In), Optional :: mdate_iyr(:,:) ! - year made ('start date' for model forecasts) (minimum dimensions: nls, nfs) -
  Integer, Intent(In), Optional :: mdate_imn(:,:) ! - month made ('start date' for model forecasts) (minimum dimensions: nls, nfs) -
  Integer, Intent(In), Optional :: mdate_idy(:,:) ! - day made ('start date' for model forecasts) (minimum dimensions: nls, nfs) -
!
  Real(Kind=rp), Intent(In), Optional :: z_hght(:) ! - atmoshperic level -- height (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In), Optional :: z_unit(:) ! - atmoshperic level -- units (minimum dimensions: nfs) -
!
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(date) :: mdate1(nls,nfs) ! - first forecast date -
!
  Type(period) :: period1(nls,nfs) ! - first period -
!
  Type(level) ::  z(nfs) ! - level -
!
! Local scalars
  Integer :: i   ! - latitude index -
  Integer :: j   ! - longitude index -
  Integer :: k   ! - time index -
  Integer :: l   ! - lag index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
  Logical :: lmdate ! - forecast date flag -
  Logical :: lz     ! - atmospheric level flag -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     Do l = 1, nls
        period1(l,ifd)%sdate%iyr = period1_sdate_iyr(l,ifd)
        period1(l,ifd)%sdate%imn = period1_sdate_imn(l,ifd)
        period1(l,ifd)%sdate%idy = period1_sdate_idy(l,ifd)
        period1(l,ifd)%edate%iyr = period1_edate_iyr(l,ifd)
        period1(l,ifd)%edate%imn = period1_edate_imn(l,ifd)
        period1(l,ifd)%edate%idy = period1_edate_idy(l,ifd)
     End Do
  End Do
!
! Initialise Optional field variables
  If (Present(mdate_iyr) .and. Present(mdate_imn) .and. Present(mdate_idy)) Then
     Do ifd = 1, nfs
        Do l = 1, nls
           mdate1(l,ifd)%iyr = mdate_iyr(l,ifd)
           mdate1(l,ifd)%imn = mdate_imn(l,ifd)
           mdate1(l,ifd)%idy = mdate_idy(l,ifd)
        End Do
     End Do
     lmdate = .true.
  Else
     lmdate = .false.
  End If
  If(Present(z_hght) .and. Present(z_unit) ) Then
     Do ifd = 1, nfs
        z(ifd)%hght = z_hght(ifd)
        z(ifd)%unit = z_unit(ifd)
     End Do
     lz = .true.
  Else
     lz = .false.
  End If
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
       
! Print gridded data
  Do ifd = 1, nfs
     nn = 0
     Do k = 1, nt
        Do l = 1, nls
           If (Present(kuse)) Then
              If (.not.kuse(k)) Cycle
           End If
           nn = nn + 1
           If (lmdate) Then
              If (lz) Then
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              Else
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), &
                                    cpt_s=mdate1(l,ifd)+(k-1), cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              End If
           Else
              If (lz) Then
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), cpt_m=ifd, &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_z=z(ifd), &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              Else
                 If (lensemble) Then
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), cpt_m=ifd, &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 Else
                    Call write_tag (iout,ifail, &
                                    cpt_field=Trim(var(ifd)), &
                                    cpt_t=period1(l,ifd)+(k-1), &
                                    cpt_nrow=nlt, cpt_ncol=nlg, cpt_row='Y', cpt_col='X', &
                                    cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss,Kind=dp))
                 End If
              End If
           End If
           If (ifail /= 0) GoTo 1
           Do j = 1, nlg
              If (rlng(j) > r360) Then
                 Write (cout, Fmt=*) rlng(j) - r360
              Else
                 Write (cout, Fmt=*) rlng(j)
              End If
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
           Do i = 1, nlt
              Write (cout, Fmt=*) rlat(i)
              Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) Trim(AdjustL(cout))
              Do j = 1, nlg
                 Write (cout, Fmt=*) v(j,i,nn,l,ifd)
                 Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
              End Do
              Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1)
           End Do
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_grid_fields_lags_v11_dp
!
!
!
 Subroutine Write_cpt_stns_v11_sp ( outfile, nv, nt, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                    period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                    period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                    ifail, &
                                    kuse &
                                  )
!
! Modules
  Use numbers, Only: rp=>sp, dp
!
! Outputs station data with no additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nv   ! - number of stations -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Integer, Intent(In) :: period1_sdate_iyr ! - start year of first period -
  Integer, Intent(In) :: period1_sdate_imn ! - start month of first period -
  Integer, Intent(In) :: period1_sdate_idy ! - start day of first period -
  Integer, Intent(In) :: period1_edate_iyr ! - End year of first period -
  Integer, Intent(In) :: period1_edate_imn ! - End month of first period -
  Integer, Intent(In) :: period1_edate_idy ! - End day of first period -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! Input arrays
  Real(Kind=rp), Intent(In) :: v(:,:)  ! - data (minimum dimensions: nv, nn) -
  Real(Kind=rp), Intent(In) :: rlat(:) ! - latitudes (minimum dimensions: nv) -
  Real(Kind=rp), Intent(In) :: rlng(:) ! - longitudes (minimum dimensions: nv) -
!
  Character(Len=lstn), Intent(In) :: cstn(:) ! - station names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: i  ! - station index -
  Integer :: k  ! - time index -
  Integer :: nn ! - number of non-missing Cases -
!
  Type(period) :: period1 ! - first period -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  period1%sdate%iyr = period1_sdate_iyr
  period1%sdate%imn = period1_sdate_imn
  period1%sdate%idy = period1_sdate_idy
  period1%edate%iyr = period1_edate_iyr
  period1%edate%imn = period1_edate_imn
  period1%edate%idy = period1_edate_idy
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn, cpt_ncol=nv, cpt_row='T', cpt_col='station', &
                  cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
  If (ifail /= 0) GoTo 1
!
! Print station information
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
  Do i = 1, nv
     Write (cout, Fmt=*) rlat(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
  Do i = 1, nv
     Write (cout, Fmt=*) rlng(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
  nn = 0
  Do k = 1, nt
     If (Present(kuse)) Then
        If (.not.kuse(k)) Cycle
     End If
     nn = nn + 1
     cout = get_cdate(period1+(k-1))
     Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
     Do i = 1, nv
        Write (cout, Fmt=*) v(i,nn)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_v11_sp
!
!
!
 Subroutine Write_cpt_stns_v11_dp ( outfile, nv, nt, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                    period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                    period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                    ifail, &
                                    kuse &
                                  )
!
! Modules
  Use numbers, Only: rp=>dp
!
! Outputs station data with no additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nv   ! - number of stations -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Integer, Intent(In) :: period1_sdate_iyr ! - start year of first period -
  Integer, Intent(In) :: period1_sdate_imn ! - start month of first period -
  Integer, Intent(In) :: period1_sdate_idy ! - start day of first period -
  Integer, Intent(In) :: period1_edate_iyr ! - End year of first period -
  Integer, Intent(In) :: period1_edate_imn ! - End month of first period -
  Integer, Intent(In) :: period1_edate_idy ! - End day of first period -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! Input arrays
  Real(Kind=rp), Intent(In) :: v(:,:)  ! - data (minimum dimensions: nv, nn) -
  Real(Kind=rp), Intent(In) :: rlat(:) ! - latitudes (minimum dimensions: nv) -
  Real(Kind=rp), Intent(In) :: rlng(:) ! - longitudes (minimum dimensions: nv) -
!
  Character(Len=lstn), Intent(In) :: cstn(:) ! - station names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: i  ! - station index -
  Integer :: k  ! - time index -
  Integer :: nn ! - number of non-missing Cases -
!
  Type(period) :: period1 ! - first period -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  period1%sdate%iyr = period1_sdate_iyr
  period1%sdate%imn = period1_sdate_imn
  period1%sdate%idy = period1_sdate_idy
  period1%edate%iyr = period1_edate_iyr
  period1%edate%imn = period1_edate_imn
  period1%edate%idy = period1_edate_idy
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn, cpt_ncol=nv, cpt_row='T', cpt_col='station', &
                  cpt_units=Trim(unit), cpt_missing=miss)
  If (ifail /= 0) GoTo 1
!
! Print station information
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
  Do i = 1, nv
     Write (cout, Fmt=*) rlat(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
  Do i = 1, nv
     Write (cout, Fmt=*) rlng(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
  nn = 0
  Do k = 1, nt
     If (Present(kuse)) Then
        If (.not.kuse(k)) Cycle
     End If
     nn = nn + 1
     cout = get_cdate(period1+(k-1))
     Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
     Do i = 1, nv
        Write (cout, Fmt=*) v(i,nn)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_v11_dp
!
!
!
 Subroutine Write_cpt_stns_lags_v11_sp ( outfile, nv, nt, nls, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                         period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                         period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                         ifail, &
                                         kuse &
                                       )
!
! Modules
  Use numbers, Only: rp=>sp, dp
!
! Outputs station data with additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nv   ! - number of stations -
  Integer, Intent(In) :: nls  ! - number of lead-times -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each lag (minimum dimensions: nls) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:) ! - data (minimum dimensions: nv, nn, nls) -
  Real(Kind=rp), Intent(In) :: rlat(:)  ! - latitudes (minimum dimensions: nv) -
  Real(Kind=rp), Intent(In) :: rlng(:)  ! - longitudes (minimum dimensions: nv) -
!
  Character(Len=lstn), Intent(In) :: cstn(:) ! - station names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nls) ! - first period -
!
! Local scalars
  Integer :: i  ! - station index -
  Integer :: k  ! - time index -
  Integer :: l  ! - lag index -
  Integer :: nn ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do l = 1, nls
     period1(l)%sdate%iyr = period1_sdate_iyr(l)
     period1(l)%sdate%imn = period1_sdate_imn(l)
     period1(l)%sdate%idy = period1_sdate_idy(l)
     period1(l)%edate%iyr = period1_edate_iyr(l)
     period1(l)%edate%imn = period1_edate_imn(l)
     period1(l)%edate%idy = period1_edate_idy(l)
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn*nls, cpt_ncol=nv, cpt_row='T', cpt_col='station', &
                  cpt_units=Trim(unit), cpt_missing=Real(miss,Kind=dp))
  If (ifail /= 0) GoTo 1
!
! Print station information
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
  Do i = 1, nv
     Write (cout, Fmt=*) rlat(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
  Do i = 1, nv
     Write (cout, Fmt=*) rlng(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
  nn = 0
  Do k = 1, nt
     Do l = 1, nls
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        cout = get_cdate(period1(l)+(k-1))
        Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
        Do i = 1, nv
           Write (cout, Fmt=*) v(i,nn,l)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_lags_v11_sp
!
!
!
 Subroutine Write_cpt_stns_lags_v11_dp ( outfile, nv, nt, nls, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                         period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                         period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                         ifail, &
                                         kuse &
                                       )
!
! Modules
  Use numbers, Only: rp=>dp
!
! Outputs station data with additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nv   ! - number of stations -
  Integer, Intent(In) :: nls  ! - number of lead-times -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
  Character(Len=*), Intent(In) :: unit    ! - field units -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each lag (minimum dimensions: nls) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:) ! - data (minimum dimensions: nv, nn, nls) -
  Real(Kind=rp), Intent(In) :: rlat(:)  ! - latitudes (minimum dimensions: nv) -
  Real(Kind=rp), Intent(In) :: rlng(:)  ! - longitudes (minimum dimensions: nv) -
!
  Character(Len=lstn), Intent(In) :: cstn(:) ! - station names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nls) ! - first period -
!
! Local scalars
  Integer :: i  ! - station index -
  Integer :: k  ! - time index -
  Integer :: l  ! - lag index -
  Integer :: nn ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do l = 1, nls
     period1(l)%sdate%iyr = period1_sdate_iyr(l)
     period1(l)%sdate%imn = period1_sdate_imn(l)
     period1(l)%sdate%idy = period1_sdate_idy(l)
     period1(l)%edate%iyr = period1_edate_iyr(l)
     period1(l)%edate%imn = period1_edate_imn(l)
     period1(l)%edate%idy = period1_edate_idy(l)
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn*nls, cpt_ncol=nv, cpt_row='T', cpt_col='station', &
                  cpt_units=Trim(unit), cpt_missing=miss)
  If (ifail /= 0) GoTo 1
!
! Print station information
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
  Do i = 1, nv
     Write (cout, Fmt=*) rlat(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
  Do i = 1, nv
     Write (cout, Fmt=*) rlng(i)
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
  nn = 0
  Do k = 1, nt
     Do l = 1, nls
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        cout = get_cdate(period1(l)+(k-1))
        Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
        Do i = 1, nv
           Write (cout, Fmt=*) v(i,nn,l)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_lags_v11_dp
!
!
!
 Subroutine Write_cpt_stns_fields_v11_sp ( outfile, nv, nt, nfs, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                           period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                           period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                           ifail, &
                                           kuse &
                                         )
!
! Modules
  Use numbers, Only: rp=>sp, dp
!
! Outputs station data with multiple fields, but no additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
! Input arrays
  Integer, Intent(In) :: nv(:) ! - number of stations per field (minimum dimensions: nfs) -
!
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each field (minimum dimensions: nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:)  ! - data (minimum dimensions: nv, nn, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:,:) ! - latitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: rlng(:,:) ! - longitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: miss(:)   ! - missing value flags (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In) :: cstn(:,:) ! - station names (minimum dimensions: nv, nfs) -
  Character(Len=*), Intent(In) :: var(:)    ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:)   ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nfs) ! - first period -
!
! Local scalars
  Integer :: i   ! - station index -
  Integer :: k   ! - time index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     period1(ifd)%sdate%iyr = period1_sdate_iyr(ifd)
     period1(ifd)%sdate%imn = period1_sdate_imn(ifd)
     period1(ifd)%sdate%idy = period1_sdate_idy(ifd)
     period1(ifd)%edate%iyr = period1_edate_iyr(ifd)
     period1(ifd)%edate%imn = period1_edate_imn(ifd)
     period1(ifd)%edate%idy = period1_edate_idy(ifd)
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Do ifd = 1, nfs
     Call write_tag (iout, ifail, &
                     cpt_field=Trim(var(ifd)), cpt_nrow=nn, cpt_ncol=nv(ifd), cpt_row='T', cpt_col='station', &
                     cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss(ifd),Kind=dp))
     If (ifail /= 0) GoTo 1
!
! Print station information
     Do i = 1, nv(ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i,ifd)))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlat(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlng(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
     nn = 0
     Do k = 1, nt
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        cout = get_cdate(period1(ifd)+(k-1))
        Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
        Do i = 1, nv(ifd)
           Write (cout, Fmt=*) v(i,nn,ifd)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_fields_v11_sp
!
!
!
 Subroutine Write_cpt_stns_fields_v11_dp ( outfile, nv, nt, nfs, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                           period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                           period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                           ifail, &
                                           kuse &
                                         )
!
! Modules
  Use numbers, Only: rp=>dp
!
! Outputs station data with multiple fields, but no additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
! Input arrays
  Integer, Intent(In) :: nv(:) ! - number of stations per field (minimum dimensions: nfs) -
!
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period for each field (minimum dimensions: nfs) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period for each field (minimum dimensions: nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:)  ! - data (minimum dimensions: nv, nn, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:,:) ! - latitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: rlng(:,:) ! - longitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: miss(:)   ! - missing value flags (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In) :: cstn(:,:) ! - station names (minimum dimensions: nv, nfs) -
  Character(Len=*), Intent(In) :: var(:)    ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:)   ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nfs) ! - first period -
!
! Local scalars
  Integer :: i   ! - station index -
  Integer :: k   ! - time index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period

!
! Initialise sequencing and field information
  iseq = tseq
  Do ifd = 1, nfs
     period1(ifd)%sdate%iyr = period1_sdate_iyr(ifd)
     period1(ifd)%sdate%imn = period1_sdate_imn(ifd)
     period1(ifd)%sdate%idy = period1_sdate_idy(ifd)
     period1(ifd)%edate%iyr = period1_edate_iyr(ifd)
     period1(ifd)%edate%imn = period1_edate_imn(ifd)
     period1(ifd)%edate%idy = period1_edate_idy(ifd)
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Do ifd = 1, nfs
     Call write_tag (iout,ifail, &
                     cpt_field=Trim(var(ifd)), cpt_nrow=nn, cpt_ncol=nv(ifd), cpt_row='T', cpt_col='station', &
                     cpt_units=Trim(unit(ifd)), cpt_missing=miss(ifd))
     If (ifail /= 0) GoTo 1
!
! Print station information
     Do i = 1, nv(ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i,ifd)))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlat(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlng(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
     nn = 0
     Do k = 1, nt
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        cout = get_cdate(period1(ifd)+(k-1))
        Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
        Do i = 1, nv(ifd)
           Write (cout, Fmt=*) v(i,nn,ifd)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_fields_v11_dp
!
!
!
 Subroutine Write_cpt_stns_fields_lags_v11_sp ( outfile, nv, nt, nls, nfs, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                                period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                                period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                                ifail, &
                                                kuse &
                                              )
!
! Modules
  Use numbers, Only: rp=>sp, dp
!
! Outputs station data with multiple fields, but no additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nls  ! - number of lags -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
! Input arrays
  Integer, Intent(In) :: nv(:) ! - number of stations per field (minimum dimensions: nfs) -
!
  Integer, Intent(In) :: period1_sdate_iyr(:,:) ! - start year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:,:) ! - start month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:,:) ! - start day of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:,:) ! - End year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_imn(:,:) ! - End month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_idy(:,:) ! - End day of first period for each field (minimum dimensions: nls, nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:) ! - data (minimum dimensions: nv, nn, nls, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:,:)  ! - latitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: rlng(:,:)  ! - longitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: miss(:)    ! - missing value flags (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In) :: cstn(:,:) ! - station names (minimum dimensions: nv, nfs) -
  Character(Len=*), Intent(In) :: var(:)    ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:)   ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nls,nfs) ! - first period -
!
! Local scalars
  Integer :: i   ! - station index -
  Integer :: k   ! - time index -
  Integer :: l   ! - lag index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     Do l = 1, nls
        period1(l,ifd)%sdate%iyr = period1_sdate_iyr(l,ifd)
        period1(l,ifd)%sdate%imn = period1_sdate_imn(l,ifd)
        period1(l,ifd)%sdate%idy = period1_sdate_idy(l,ifd)
        period1(l,ifd)%edate%iyr = period1_edate_iyr(l,ifd)
        period1(l,ifd)%edate%imn = period1_edate_imn(l,ifd)
        period1(l,ifd)%edate%idy = period1_edate_idy(l,ifd)
     End Do
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Do ifd = 1, nfs
     Call write_tag (iout, ifail, &
                     cpt_field=Trim(var(ifd)), cpt_nrow=nn*nls, cpt_ncol=nv(ifd), cpt_row='T', cpt_col='station', &
                     cpt_units=Trim(unit(ifd)), cpt_missing=Real(miss(ifd),Kind=dp))
     If (ifail /= 0) GoTo 1
!
! Print station information
     Do i = 1, nv(ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i,ifd)))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlat(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlng(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
     nn = 0
     Do k = 1, nt
        Do l = 1, nls
           If (Present(kuse)) Then
              If (.not.kuse(k)) Cycle
           End If
           nn = nn + 1
           cout = get_cdate(period1(l,ifd)+(k-1))
           Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
           Do i = 1, nv(ifd)
              Write (cout, Fmt=*) v(i,nn,l,ifd)
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_fields_lags_v11_sp
!
!
!
 Subroutine Write_cpt_stns_fields_lags_v11_dp ( outfile, nv, nt, nls, nfs, tseq, v, miss, rlat, rlng, cstn, var, unit, &
                                                period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                                period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                                ifail, &
                                                kuse &
                                              )
!
! Modules
  Use numbers, Only: rp=>dp
!
! Outputs station data with multiple fields, but no additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nls  ! - number of lags -
  Integer, Intent(In) :: nfs  ! - number of fields -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Character(Len=   *), Intent(In) :: outfile ! - file name with full path -
!
! Input arrays
  Integer, Intent(In) :: nv(:) ! - number of stations per field (minimum dimensions: nfs) -
!
  Integer, Intent(In) :: period1_sdate_iyr(:,:) ! - start year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_imn(:,:) ! - start month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_sdate_idy(:,:) ! - start day of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_iyr(:,:) ! - End year of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_imn(:,:) ! - End month of first period for each field (minimum dimensions: nls, nfs) -
  Integer, Intent(In) :: period1_edate_idy(:,:) ! - End day of first period for each field (minimum dimensions: nls, nfs) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:,:) ! - data (minimum dimensions: nv, nn, nls, nfs) -
  Real(Kind=rp), Intent(In) :: rlat(:,:)  ! - latitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: rlng(:,:)  ! - longitudes (minimum dimensions: nv, nfs) -
  Real(Kind=rp), Intent(In) :: miss(:)    ! - missing value flags (minimum dimensions: nfs) -
!
  Character(Len=*), Intent(In) :: cstn(:,:) ! - station names (minimum dimensions: nv, nfs) -
  Character(Len=*), Intent(In) :: var(:)    ! - field variable (minimum dimensions: nfs) -
  Character(Len=*), Intent(In) :: unit(:)   ! - field units (minimum dimensions: nfs) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nls,nfs) ! - first period -
!
! Local scalars
  Integer :: i   ! - station index -
  Integer :: k   ! - time index -
  Integer :: l   ! - lag index -
  Integer :: ifd ! - field index -
  Integer :: nn  ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do ifd = 1, nfs
     Do l = 1, nls
        period1(l,ifd)%sdate%iyr = period1_sdate_iyr(l,ifd)
        period1(l,ifd)%sdate%imn = period1_sdate_imn(l,ifd)
        period1(l,ifd)%sdate%idy = period1_sdate_idy(l,ifd)
        period1(l,ifd)%edate%iyr = period1_edate_iyr(l,ifd)
        period1(l,ifd)%edate%imn = period1_edate_imn(l,ifd)
        period1(l,ifd)%edate%idy = period1_edate_idy(l,ifd)
     End Do
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout, Trim(outfile), nfs, ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Do ifd = 1, nfs
     Call write_tag (iout, ifail, &
                     cpt_field=Trim(var(ifd)), cpt_nrow=nn*nls, cpt_ncol=nv(ifd), cpt_row='T', cpt_col='station', &
                     cpt_units=Trim(unit(ifd)), cpt_missing=miss(ifd))
     If (ifail /= 0) GoTo 1
!
! Print station information
     Do i = 1, nv(ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cstn(i,ifd)))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:Y'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlat(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     Write (Unit=iout, Fmt='(A)', Advance='no', Err=1) 'cpt:X'
     Do i = 1, nv(ifd)
        Write (cout, Fmt=*) rlng(i,ifd)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print station data
     nn = 0
     Do k = 1, nt
        Do l = 1, nls
           If (Present(kuse)) Then
              If (.not.kuse(k)) Cycle
           End If
           nn = nn + 1
           cout = get_cdate(period1(l,ifd)+(k-1))
           Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
           Do i = 1, nv(ifd)
              Write (cout, Fmt=*) v(i,nn,l,ifd)
              Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
           End Do
           Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
        End Do
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_stns_fields_lags_v11_dp
!
!
!
 Subroutine Write_cpt_unrf_v11_sp ( outfile, nv, nt, tseq, v, miss, var, cvar, &
                                    period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                    period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                    ifail, &
                                    kuse &
                                  )
!
! Modules
  Use numbers, Only: rp=>sp, dp
!
! Outputs unreferenced data with no additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nv   ! - number of series -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Integer, Intent(In) :: period1_sdate_iyr ! - start year of first period -
  Integer, Intent(In) :: period1_sdate_imn ! - start month of first period -
  Integer, Intent(In) :: period1_sdate_idy ! - start day of first period -
  Integer, Intent(In) :: period1_edate_iyr ! - End year of first period -
  Integer, Intent(In) :: period1_edate_imn ! - End month of first period -
  Integer, Intent(In) :: period1_edate_idy ! - End day of first period -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Real(Kind=rp), Intent(In) :: v(:,:) ! - data (minimum dimensions: nv, nn)
!
  Character(Len=*), Intent(In) :: cvar(:) ! - variable names (minimum dimensions: nv)
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Locals
!
! Local scalars
  Integer :: i  ! - series index -
  Integer :: k  ! - time index -
  Integer :: nn ! - number of non-missing Cases -
!
  Type(period) :: period1 ! - first period -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  period1%sdate%iyr = period1_sdate_iyr
  period1%sdate%imn = period1_sdate_imn
  period1%sdate%idy = period1_sdate_idy
  period1%edate%iyr = period1_edate_iyr
  period1%edate%imn = period1_edate_imn
  period1%edate%idy = period1_edate_idy
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn, cpt_ncol=nv, cpt_row='T', cpt_col='index', &
                  cpt_missing=Real(miss,Kind=dp))
  If (ifail /= 0) GoTo 1
!
! Print index names
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cvar(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print unreferenced data
  nn = 0
  Do k = 1, nt
     If (Present(kuse)) Then
        If (.not.kuse(k)) Cycle
     End If
     nn = nn + 1
     cout = get_cdate(period1+(k-1))
     Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
     Do i = 1, nv
        Write (cout, Fmt=*) v(i,nn)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_unrf_v11_sp
!
!
!
 Subroutine Write_cpt_unrf_v11_dp ( outfile, nv, nt, tseq, v, miss, var, cvar, &
                                    period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                    period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                    ifail, &
                                    kuse &
                                  )
!
! Modules
  Use numbers, Only: rp=>dp
!
! Outputs unreferenced data with additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nv   ! - number of series -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Integer, Intent(In) :: period1_sdate_iyr ! - start year of first period -
  Integer, Intent(In) :: period1_sdate_imn ! - start month of first period -
  Integer, Intent(In) :: period1_sdate_idy ! - start day of first period -
  Integer, Intent(In) :: period1_edate_iyr ! - End year of first period -
  Integer, Intent(In) :: period1_edate_imn ! - End month of first period -
  Integer, Intent(In) :: period1_edate_idy ! - End day of first period -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Real(Kind=rp), Intent(In) :: v(:,:) ! - data (minimum dimensions: nv, nn) -
!
  Character(Len=*), Intent(In) :: cvar(:) ! - index names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Locals
!
! Local scalars
  Integer :: i  ! - series index -
  Integer :: k  ! - time index -
  Integer :: nn ! - number of non-missing Cases -
!
  Type(period) :: period1 ! - first period -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  period1%sdate%iyr = period1_sdate_iyr
  period1%sdate%imn = period1_sdate_imn
  period1%sdate%idy = period1_sdate_idy
  period1%edate%iyr = period1_edate_iyr
  period1%edate%imn = period1_edate_imn
  period1%edate%idy = period1_edate_idy
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn, cpt_ncol=nv, cpt_row='T', cpt_col='index', &
                  cpt_missing=miss)
  If (ifail /= 0) GoTo 1
!
! Print index names
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cvar(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print unreferenced data
  nn = 0
  Do k = 1, nt
     If (Present(kuse)) Then
        If (.not.kuse(k)) Cycle
     End If
     nn = nn + 1
     cout = get_cdate(period1+(k-1))
     Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
     Do i = 1, nv
        Write (cout, Fmt=*) v(i,nn)
        Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
     End Do
     Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_unrf_v11_dp
!
!
!
 Subroutine Write_cpt_unrf_lags_v11_sp ( outfile, nv, nt, nls, tseq, v, miss, var, cvar, &
                                         period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                         period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                         ifail, &
                                         kuse &
                                       )
!
! Modules
  Use numbers, Only: rp=>sp, dp
!
! Outputs unreferenced data with additional lags
! Single precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nv   ! - number of series -
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nls  ! - number of lead-times -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period of each lag (minimum dimensions: nls) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:) ! - data (minimum dimensions: nv, nn, nls) -
!
  Character(Len=*),Intent(In) :: cvar(:) ! - variable names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nls) ! - first period -
!
! Local scalars
  Integer :: i  ! - series index -
  Integer :: k  ! - time index -
  Integer :: l  ! - lag index -
  Integer :: nn ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Real, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do l = 1, nls
     period1(l)%sdate%iyr = period1_sdate_iyr(l)
     period1(l)%sdate%imn = period1_sdate_imn(l)
     period1(l)%sdate%idy = period1_sdate_idy(l)
     period1(l)%edate%iyr = period1_edate_iyr(l)
     period1(l)%edate%imn = period1_edate_imn(l)
     period1(l)%edate%idy = period1_edate_idy(l)
  End Do
!
! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn*nls, cpt_ncol=nv, cpt_row='T', cpt_col='index', &
                  cpt_missing=Real(miss,Kind=dp))
  If (ifail /= 0) GoTo 1
!
! Print index names
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cvar(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print unreferenced data
  nn = 0
  Do k = 1, nt
     Do l = 1, nls
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        cout = get_cdate(period1(l)+(k-1))
        Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
        Do i = 1, nv
           Write (cout, Fmt=*) v(i,nn,l)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_unrf_lags_v11_sp
!
!
!
 Subroutine Write_cpt_unrf_lags_v11_dp ( outfile, nv, nt, nls, tseq, v, miss, var, cvar, &
                                         period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy, &
                                         period1_edate_iyr, period1_edate_imn, period1_edate_idy, &
                                         ifail, &
                                         kuse &
                                       )
!
! Modules
  Use numbers, Only: rp=>dp
!
! Outputs unreferenced data with additional lags
! Double precision version
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nv   ! - number of series -
  Integer, Intent(In) :: nt   ! - number of Cases -
  Integer, Intent(In) :: nls  ! - number of lead-times -
  Integer, Intent(In) :: tseq ! - time sequence identIfier -
                              ! - 1 -> time sequence increase by year -
                              ! - 2 -> time sequence increase by month -
                              ! - 3 -> time sequence increase daily -
!
  Real(Kind=rp), Intent(In) :: miss ! - missing value flag -
!
  Character(Len=*), Intent(In) :: outfile ! - file name with full path -
  Character(Len=*), Intent(In) :: var     ! - field variable -
!
! Input arrays
  Integer, Intent(In) :: period1_sdate_iyr(:) ! - start year of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_imn(:) ! - start month of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_sdate_idy(:) ! - start day of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_iyr(:) ! - End year of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_imn(:) ! - End month of first period of each lag (minimum dimensions: nls) -
  Integer, Intent(In) :: period1_edate_idy(:) ! - End day of first period of each lag (minimum dimensions: nls) -
!
  Real(Kind=rp), Intent(In) :: v(:,:,:) ! - data (minimum dimensions: nv, nn, nls) -
!
  Character(Len=*),Intent(In) :: cvar(:) ! - variable names (minimum dimensions: nv) -
!
! - Optional input arrays -
  Logical, Intent(In), Optional :: kuse(:) ! - missing Cases indicator (minimum dimensions: nt)
                                           ! - The second dimension of v can be nn where nn == Count(kuse(:))
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local arrays
  Type(period) :: period1(nls) ! - first period -
!
! Local scalars
  Integer :: i  ! - series index -
  Integer :: k  ! - time index -
  Integer :: l  ! - lag index -
  Integer :: nn ! - number of non-missing Cases -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Achar, AdjustL, Count, Present, Trim
!
! Executable Statements
!
! Initialise time sequencing
  iseq = tseq
!
! Initialise starting period
  Do l = 1, nls
     period1(l)%sdate%iyr = period1_sdate_iyr(l)
     period1(l)%sdate%imn = period1_sdate_imn(l)
     period1(l)%sdate%idy = period1_sdate_idy(l)
     period1(l)%edate%iyr = period1_edate_iyr(l)
     period1(l)%edate%imn = period1_edate_imn(l)
     period1(l)%edate%idy = period1_edate_idy(l)
  End Do

! Open file and Write out first two lines
  Call open_output (iout,Trim(outfile),1,ifail)
  If (ifail /= 0) Return
!
! Print tag line
  If (Present(kuse)) Then
     nn = Count(kuse(1:nt))
  Else
     nn = nt
  End If
  Call write_tag (iout, ifail, &
                  cpt_field=Trim(var), cpt_nrow=nn*nls, cpt_ncol=nv, cpt_row='T', cpt_col='index', &
                  cpt_missing=miss)
  If (ifail /= 0) GoTo 1
!
! Print index names
  Do i = 1, nv
     Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cvar(i)))
  End Do
  Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
!
! Print unreferenced data
  nn = 0
  Do k = 1, nt
     Do l = 1, nls
        If (Present(kuse)) Then
           If (.not.kuse(k)) Cycle
        End If
        nn = nn + 1
        cout = get_cdate(period1(l)+(k-1))
        Write (Unit=iout, Advance='no', Fmt='(A)', Err=1) Trim(AdjustL(cout))
        Do i = 1, nv
           Write (cout, Fmt=*) v(i,nn,l)
           Write (Unit=iout, Fmt='(2A)', Advance='no', Err=1) Achar(9), Trim(AdjustL(cout))
        End Do
        Write (Unit=iout, Fmt='(A)', Advance='yes', Err=1) ' '
     End Do
  End Do
  ifail = 0
!
  Close (Unit=iout)
  Return
!
! Error
1 ifail = 3
!
  Close (iout)
  Return
 End Subroutine Write_cpt_unrf_lags_v11_dp
!
!
!
 Function same_date(d1,d2)
!
! Function Type
  Logical :: same_date
!
! Arguments
!
! Input scalars
  Type(date), Intent(In) :: d1 ! - first date -
  Type(date), Intent(In) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  same_date = .false.
  If (d1%iyr /= d2%iyr) Return
  If (d1%imn /= d2%imn) Return
  If (d1%idy /= d2%idy) Return
  same_date = .true.
!
  Return
 End Function same_date
!
!
!
 Function equal_date(d1,i1)
!
! Function Type
  Logical :: equal_date
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: i1 ! - constant -
!
  Type(date), Intent(In) :: d1 ! - first date -
!
! Executable Statements
!
! Compare dates
  equal_date = .false.
  If (d1%iyr /= i1) Return
  If (d1%imn /= i1) Return
  If (d1%idy /= i1) Return
  equal_date = .true.
!
  Return
 End Function equal_date
!
!
!
 Function lt_date(d1,d2)
!
! Function Type
  Logical :: lt_date
!
! Arguments
!
! Input scalars
  Type(date), Intent(In) :: d1 ! - first date -
  Type(date), Intent(In) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  lt_date = .true.
  If (d1%iyr < d2%iyr) Then
     Return
  Else If (d1%iyr == d2%iyr) Then
     If (d1%imn < d2%imn) Then
        Return
     Else If (d1%imn == d2%imn) Then
        If (d1%idy < d2%idy) Return
     End If
  End If
  lt_date = .false.
!
  Return
 End Function lt_date
!
!
!
 Function gt_date(d1,d2)
!
! Function Type
  Logical :: gt_date
!
! Arguments
!
! Input scalars
  Type(date), Intent(In) :: d1 ! - first date -
  Type(date), Intent(In) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  gt_date = .true.
  If (d1%iyr>d2%iyr) Then
     Return
  Else If (d1%iyr == d2%iyr) Then
     If (d1%imn>d2%imn) Then
        Return
     Else If (d1%imn == d2%imn) Then
        If (d1%idy>d2%idy) Return
     End If
  End If
  gt_date = .false.
!
  Return
 End Function gt_date
!
!
!
 Function add_date(d,i)
!
! Function Type
  Type(date) :: add_date
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: i ! - increment -
!
  Type(date), Intent(In) :: d ! - date -
!
! Executable Statements
!
! Increment date
  add_date = d
  Select Case (iseq)
   Case (1) ! - year increment -
     add_date%iyr = d%iyr + i
   Case (3) ! - day increment -
     add_date%idy = d%idy + i
     If (add_date%idy < 1) Then
        Do
           add_date%idy = add_date%idy+ndays(add_date%iyr,add_date%imn)
           add_date%imn = add_date%imn - 1
           If (add_date%imn < 1) Then
              add_date%iyr = add_date%iyr - 1
              add_date%imn = add_date%imn+nmn
           End If
           If (add_date%idy >= 1) Exit
        End Do
     Else If (add_date%idy>ndays(add_date%iyr,add_date%imn)) Then
        Do
           add_date%idy = add_date%idy-ndays(add_date%iyr,add_date%imn)
           add_date%imn = add_date%imn + 1
           If (add_date%imn>nmn) Then
              add_date%iyr = add_date%iyr + 1
              add_date%imn = add_date%imn - nmn
           End If
           If (add_date%idy <= ndays(add_date%iyr,add_date%imn)) Exit
        End Do
     End If
  End Select
!
  Return
 End Function add_date
!
!
!
 Function add_period(p, i)
!
! Function Type
  Type(period) :: add_period
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: i ! - increment -
!
  Type(period), Intent(In) :: p ! - period -
!
! Executable Statements
!
! Increment period
  add_period%sdate = p%sdate+i
  add_period%edate = p%edate+i
!
  Return
 End Function add_period
!
!
!
 Function ndays(iyr, imn)
!
! Calculates number of days in the month
! NB - assumes the Gregorian calendard as implemented by Britain and the British Empire
!
! Function Type
  Integer :: ndays
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iyr ! - year -
  Integer, Intent(In) :: imn ! - month -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Mod
!
! Executable Statements
!
! Define number of days
  Select Case (imn)
   Case (1)  ! January
     ndays = 31
   Case (3)  ! March
     ndays = 31
   Case (4)  ! April
     ndays = 30
   Case (5)  ! May
     ndays = 31
   Case (6)  ! June
     ndays = 30
   Case (7)  ! July
     ndays = 31
   Case (8)  ! August
     ndays = 31
   Case (9)  ! September
     If (iyr /= 1752) Then
        ndays = 30
     Else
        ndays = 19
     End If
   Case (10) ! October
     ndays = 31
   Case (11) ! November
     ndays = 30
   Case (12) ! December
     ndays = 31
!
! Check for leap years
   Case (2)  ! February
     If (Mod(iyr,4) == 0) Then
        If (Mod(iyr,100) == 0) Then
           If ((Mod(iyr,400) == 0) .and. (iyr > 1752)) Then
              ndays = 29
           Else
              ndays = 28
           End If
        Else
           ndays = 29
        End If
     Else
        ndays = 28
     End If
  End Select
!
  Return
 End Function ndays
!
!
!
 Function date_diff(d1, d2, iseq)
!
! Function Type
  Integer :: date_diff
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iseq ! - date sequence -
!
  Type(date), Intent(In) :: d1 ! - first date -
  Type(date), Intent(In) :: d2 ! - second date -
!
! Locals
!
! Local scalars
  Integer :: iya ! - current year -
  Integer :: ima ! - current month -
  Integer :: id  ! - direction -
!
! Executable Statements
!
! Compare dates
  Select Case (iseq)
   Case (1) ! - yearly -
     date_diff = d2%iyr-d1%iyr
     If ((d1%imn > 0) .and. (d2%imn > 0)) Then
        If (d2%imn-d1%imn < 0) Then
           date_diff = date_diff - 1
        Else If ((d1%idy > 0) .and. (d2%idy > 0)) Then
           If (d2%imn-d1%imn < 0) Then
              If (d2%idy-d1%idy < 0) date_diff = date_diff - 1
           End If
        End If
     End If
   Case (3) ! - daily -
     date_diff = d2%idy - d1%idy
     If (d2 > d1) Then
        id = 1
     Else If (d2 < d1) Then
        id = -1
     Else
        Return
     End If
     iya = d1%iyr
     ima = d1%imn
     Do
        date_diff = date_diff+ndays(iya,ima)
        ima = ima + id
        If ((ima > nmn) .or. (ima < 1)) Then
           iya = iya + id
           ima = ima - nmn*id
        End If
        If ((iya == d2%iyr) .and. (ima == d2%imn)) Exit
     End Do
   Case Default
     date_diff = 0
  End Select
!
  Return
 End Function date_diff
!
!
!
 Function get_cdate_date(adate) &
          Result (get_cdate)
!
! Creates date as a character string
!
! ISO format
!
! Function Type
  Character(Len=ldat) :: get_cdate
!
! Arguments
!
  Type(date), Intent(In) :: adate ! - date -
!
! Executable Statements
!
! Create date
  If (adate%imn>0) Then
     If (adate%idy>0) Then
           Write (Unit=get_cdate, Fmt='(I4,A,I2.2,A,I2.2)') adate%iyr,'-',adate%imn,'-',adate%idy
     Else
           Write (Unit=get_cdate, Fmt='(I4,A,I2.2)') adate%iyr,'-',adate%imn
     End If
  Else
     Write (Unit=get_cdate, Fmt='(I4)') adate%iyr
  End If
!
  Return
 End Function get_cdate_date
!
!
!
 Function get_cdate_period(aperiod) &
          Result (get_cdate)
!
! Creates period as a character string
!
! ISO format
!
! Function Type
  Character(Len=lprd) :: get_cdate
!
! Arguments
!
  Type(period), Intent(In) :: aperiod ! - date -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Trim
!
! Executable Statements
!
! Create date
  If (aperiod%edate == aperiod%sdate) Then
     get_cdate = get_cdate_date(aperiod%sdate)
  Else
     If (aperiod%edate%iyr>aperiod%sdate%iyr) Then
        get_cdate = Trim(get_cdate_date(aperiod%sdate))//'/'//get_cdate_date(aperiod%edate)
     Else If (aperiod%edate%imn /= aperiod%sdate%imn) Then
        If (aperiod%sdate%idy > 0) Then
           Write (Unit=get_cdate, Fmt='(I4,4(A,I2.2))') &
              aperiod%sdate%iyr, '-', aperiod%sdate%imn, '-', aperiod%sdate%idy, '/', aperiod%edate%imn, '-', aperiod%edate%idy
        Else
           Write (Unit=get_cdate, Fmt='(I4,2(A,I2.2))') aperiod%sdate%iyr, '-', aperiod%sdate%imn, '/', aperiod%edate%imn
        End If
     Else
        Write (Unit=get_cdate, Fmt='(I4,3(A,I2.2))') &
           aperiod%sdate%iyr, '-', aperiod%sdate%imn, '-', aperiod%sdate%idy, '/', aperiod%edate%idy
     End If
  End If
!
  Return
 End Function get_cdate_period
!
!
!
 Subroutine open_output (iout, afile, nfs, ifail)
!
! Opens CPT output file and Prints XMLNS header
!
! Arguments
!
! Dummy arguments
! - input scalars -
  Integer, Intent(In) :: nfs  ! - number of fields -
!
  Character(Len=*), Intent(In) :: afile ! - output file name with full path -
!
! - input-output scalars -
  Integer, Intent(InOut) :: iout ! - output unit number -
!
! - output scalars -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: ioutd = 21 ! - default output unit number -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Trim
!
! Identify output unit number -
  iout = ioutd
  Do
     Inquire (Unit=iout, Opened=lopen)
     If (.not.lopen) Exit
     iout = iout + 1
  End Do
!
! Open output file as 'sequential'
  Open (Unit=iout, File=Trim(afile), Access='sequential', Action='Write', Form='formatted', &
        IOStat=ifail, Status='unknown')
!
! Error
  If (ifail /= 0) Then
     ifail = 1
     Return
  End If
!
! Print XMLNS header 'formatted'
  Write (Unit=iout, Fmt='(A)', IOStat=ifail) 'xmlns:cpt='//cxmlns_cpt
  If (ifail /= 0) GoTo 1
!
! Print number of fields
  Call write_tag (iout, ifail, &
                  cpt_nfields=nfs)
!
! Error
1 If (ifail /= 0) Then
     ifail = 3
     Print *,'OPEN file ', afile, 'failed. Exit.'
     Close (iout)
     Return
  End If
!
  Return
 End Subroutine open_output
!
!
!
 Subroutine write_tag (iout, ifail, &
                       cpt_nfields, cpt_ncats, cpt_name, cpt_field, cpt_c, cpt_prob, cpt_cmode, cpt_mode, cpt_t, cpt_s, &
                       cpt_z, cpt_m, cpt_clev, cpt_limit, cpt_nrow, cpt_ncol, cpt_row, cpt_col, cpt_units, cpt_missing)
!
! Modules
  Use numbers, Only: rp=>dp, one=>one_dp
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iout ! - output unit number -
!
! - Optional input scalars -
  Integer, Intent(In), Optional :: cpt_nfields ! - number of fields -
  Integer, Intent(In), Optional :: cpt_ncats   ! - number of categories -
  Integer, Intent(In), Optional :: cpt_c       ! - current category -
  Integer, Intent(In), Optional :: cpt_mode    ! - current mode -
  Integer, Intent(In), Optional :: cpt_m       ! - ensemble member -
  Integer, Intent(In), Optional :: cpt_nrow    ! - number of rows -
  Integer, Intent(In), Optional :: cpt_ncol    ! - number of columns -
!
  Real(Kind=rp), Intent(In), Optional :: cpt_prob    ! - climatoLogical probability -
  Real(Kind=rp), Intent(In), Optional :: cpt_clev    ! - confidence level -
  Real(Kind=rp), Intent(In), Optional :: cpt_missing ! - missing values flag -
!
  Character(Len=*), Intent(In), Optional :: cpt_field ! - field -
  Character(Len=*), Intent(In), Optional :: cpt_name  ! - name -
  Character(Len=*), Intent(In), Optional :: cpt_cmode ! - current mode -
  Character(Len=*), Intent(In), Optional :: cpt_row   ! - rows -
  Character(Len=*), Intent(In), Optional :: cpt_limit ! - confidence limit -
  Character(Len=*), Intent(In), Optional :: cpt_col   ! - columns -
  Character(Len=*), Intent(In), Optional :: cpt_units ! - units -
!
  Type(level), Intent(In), Optional :: cpt_z ! - level -
!
  Type(period), Intent(In), Optional :: cpt_t ! - date -
!
  Type(date), Intent(In), Optional :: cpt_s ! - start date -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Character(Len=    15) :: cfmt ! - format statement -
  Character(Len=lvar+1) :: cout ! - output field -
!
  Logical :: lfirst ! - first output field -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic AdjustL, Present, Trim
!
! Executable Statements
!
! Print nfields
  If (Present(cpt_nfields)) Then
     Write (Unit=iout, Fmt='(A,I0)', Err=1) 'cpt:nfields=', cpt_nfields
  End If
!
! Print ncats
  If (Present(cpt_ncats)) Then
     Write (Unit=iout, Fmt='(A,I0)', Err=1) 'cpt:ncats=', cpt_ncats
  End If
!
! Print name
  If (Present(cpt_name)) Then
     Write (Unit=iout, Fmt='(A)', Err=1) 'cpt:Name='//Trim(cpt_name)
  End If
!
! Print tags line
  lfirst = .true.
  If (Present(cpt_field)) Then
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:field='//Trim(cpt_field)
     lfirst = .false.
  End If
  If (Present(cpt_c)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:C=', cpt_c
     lfirst = .false.
  End If
  If (Present(cpt_prob)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=cout, Fmt=*) cpt_prob
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:clim_prob='//Trim(AdjustL(cout))
     lfirst = .false.
  End If
  If (Present(cpt_cmode)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:mode='//Trim(cpt_cmode)
     lfirst = .false.
  End If
  If (Present(cpt_mode)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:mode=', cpt_mode
     lfirst = .false.
  End If
  If (Present(cpt_m)) Then
     If (cpt_m > 0) Then
        If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
        Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:M=', cpt_m
        lfirst = .false.
     End If
  End If
  If ((Present(cpt_clev)) .and. (Present(cpt_limit))) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(3A)', Err=1, Advance='no') 'cpt:climit=', Trim(cpt_limit), ' ('
     Write (Unit=cfmt, Fmt='(A,2(I1,A))') '(F', iprec(cpt_clev,3)+3, '.', iprec(cpt_clev,3), ')'
     Write (Unit=iout, Fmt=cfmt, Err=1, Advance='no') cpt_clev
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') '%)'
     lfirst = .false.
  End If
  If (Present(cpt_z)) Then
     If (Trim(cpt_z%unit)/='none') Then
        If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
        If (cpt_z%hght > one) Then
           Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:Z=', cpt_z
        Else
           Write (Unit=cout, Fmt=*) cpt_z%hght
           Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:Z='//AdjustL(cout)//' '//Trim(cpt_z%unit)
        End If
        lfirst = .false.
     End If
  End If
  If (Present(cpt_t)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     cout = get_cdate(cpt_t)
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:T='//Trim(cout)
     lfirst = .false.
  End If
  If (Present(cpt_s)) Then
     If (.not.(cpt_s == 0)) Then
        If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
        cout = get_cdate(cpt_s)
        Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:S='//Trim(cout)
        lfirst = .false.
     End If
  End If
  If (Present(cpt_nrow)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:nrow=', cpt_nrow
     lfirst = .false.
  End If
  If (Present(cpt_ncol)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A,I0)', Err=1, Advance='no') 'cpt:ncol=', cpt_ncol
     lfirst = .false.
  End If
  If (Present(cpt_row)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:row='//cpt_row
     lfirst = .false.
  End If
  If (Present(cpt_col)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:col='//cpt_col
     lfirst = .false.
  End If
  If (Present(cpt_units)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:units='//cpt_units
     lfirst = .false.
  End If
  If (Present(cpt_missing)) Then
     If (.not.lfirst) Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') ', '
     Write (Unit=cout, Fmt=*) cpt_missing
     Write (Unit=iout, Fmt='(A)', Err=1, Advance='no') 'cpt:missing='//AdjustL(cout)
     lfirst = .false.
  End If
  Write (Unit=iout, Fmt='(A)', Err=1, Advance='yes') ' '
!
  ifail = 0
  Return
!
1 ifail = 1
  Return
 End Subroutine write_tag
!
!
!
 Function iprec_sp (r, mprec) &
          Result (iprec)
!
! Returns number of decimal places
! Single precision version
!
! Modules
  Use numbers, Only: rp=>sp, ten=>ten_sp
!
! Function Type
  Integer :: iprec
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: mprec ! - maximum precision required -
!
  Real(Kind=rp), Intent(In) :: r ! - value -
!
! Locals
!
! Local scalars
  Integer :: ip ! - current precision -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Mod, Nint
!
! Executable Statements
!
! IdentIfy precision
  iprec = 0
  Do ip = mprec, 1, -1
     If (Mod(Nint(r*ten**mprec),10**ip) == 0) Exit
     iprec = iprec + 1
  End Do
!
  Return
 End Function iprec_sp
!
!
!
 Function iprec_dp (r, mprec) &
          Result (iprec)
!
! Returns number of decimal places
! Double precision version
!
! Modules
  Use numbers, Only: rp=>dp, ten=>ten_dp
!
! Function Type
  Integer :: iprec
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: mprec ! - maximum precision required -
!
  Real(Kind=rp), Intent(In) :: r ! - value -
!
! Locals
!
! Local scalars
  Integer :: ip ! - current precision -
!
! Functions and Subroutines
!
! Intrinsic Functions
  Intrinsic Mod, Nint
!
! Executable Statements
!
! IdentIfy precision
  iprec = 0
  Do ip = mprec, 1, -1
     If (Mod(Nint(r*ten**mprec),10**ip) == 0) Exit
     iprec = iprec + 1
  End Do
!
  Return
 End Function iprec_dp
END Module CPT_formatV11
