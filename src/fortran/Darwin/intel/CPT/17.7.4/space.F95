! Author: Simon Mason
Module space
!
! Modules
   Use data_numbers,      Only: rp
   Use data_io_constants, Only: lstn
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: check_grid_domain, check_latitude_ordering, check_stn_domain, close_space, full_domain, get_geog, get_interpolated, &
             init_space, latitude_weight, make_coors, make_map_coor, nullify_grids, prompt_domain, proj_read_interp,             &
             proj_write_interp, proj_read_space, proj_write_space, reset_grids, reset_space, set_iprecl, set_nearest_grids
#if GUI == 1
   Public :: select_domain, which_grid, which_station, which_index
#endif
!
! Derived type definitions
!
! - area -
   Public :: area
   Type area
      Sequence
      Real(Kind=rp) :: rltn ! - northern area limit -
      Real(Kind=rp) :: rlts ! - southern area limit -
      Real(Kind=rp) :: rlgw ! - western area limit -
      Real(Kind=rp) :: rlge ! - eastern area limit -
   End Type area
!
! - domain -
   Public :: domain
   Type domain
      Sequence
      Integer :: nlts    ! - number of latitudes in domain -
      Integer :: nlgs    ! - number of longitudes in domain -
      Integer :: nlt1    ! - northern latitude domain limit index -
      Integer :: nlt2    ! - southern latitude domain limit index -
      Integer :: nlg1    ! - western longitude domain limit index -
      Integer :: nlg2    ! - eastern longitude domain limit index -
!
      Type(area) :: alim ! - area limits -
   End Type domain
!
! - level -
   Public :: level
   Type level
      Sequence
      Real(Kind=rp) :: hght    ! - height -
!
      Character(Len=5) :: unit ! - units -
   End Type level
!
! Arrays
!
! Integer arrays
   Integer, Dimension(:,:), Pointer, Public :: idom1 => Null() ! - first variables within domain -
   Integer, Dimension(:,:), Pointer, Public :: idom2 => Null() ! - second variables within domain -
   Integer, Dimension(:,:), Pointer, Public :: idomx => Null() ! - X variables within domain -
   Integer, Dimension(:,:), Pointer, Public :: idomy => Null() ! - Y variables within domain -
   Integer, Dimension(:,:), Pointer, Public :: idomz => Null() ! - Z variables within domain -
!
! Real arrays
   Real(Kind=rp), Dimension(:,:), Allocatable, Private :: rltnr ! - nearest latitudes -
   Real(Kind=rp), Dimension(:,:), Allocatable, Private :: rlgnr ! - nearest longitudes -
!
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlat1 => Null()  ! - first latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlat2 => Null()  ! - second latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatx => Null()  ! - X latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlaty => Null()  ! - Y latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatz => Null()  ! - Z latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlng1 => Null()  ! - first longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlng2 => Null()  ! - second longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngx => Null()  ! - X longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngy => Null()  ! - Y longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngz => Null()  ! - Z longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatd1 => Null() ! - first latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatd2 => Null() ! - second latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatdx => Null() ! - X latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatdy => Null() ! - Y latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlatdz => Null() ! - Z latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngd1 => Null() ! - first longitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngd2 => Null() ! - second longitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngdx => Null() ! - X longitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngdy => Null() ! - Y longitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer, Public :: rlngdz => Null() ! - Z longitudes within domain-
!
! Character arrays
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstn1 => Null()  ! - names of first stations -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstn2 => Null()  ! - names of second stations -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstnx => Null()  ! - names of X stations -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstny => Null()  ! - names of Y stations -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstnz => Null()  ! - names of Z stations -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstnd1 => Null() ! - names of first stations within domain -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstnd2 => Null() ! - names of second stations within domain -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstndx => Null() ! - names of X stations within domain -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstndy => Null() ! - names of Y stations within domain -
   Character(Len=lstn), Dimension(:,:), Pointer, Public :: cstndz => Null() ! - names of Z stations within domain -
!
! Scalars
!
! Integer scalars
   Integer, Public :: igeog ! - spatial domain flag -
   Integer, Public :: ilaty ! - current latitude -
   Integer, Public :: ilngy ! - current longitude -
#if GUI == 1
!
! Real scalars
   Real(Kind=rp), Public :: x1 ! - longitude of first corner -
   Real(Kind=rp), Public :: y1 ! - latitude of first corner -
   Real(Kind=rp), Public :: x2 ! - longitude of second corner -
   Real(Kind=rp), Public :: y2 ! - latitude of second corner -
#endif
!
! Character scalars
   Character(Len=  30), Public :: coor  ! - coordinates -
   Character(Len=lstn), Public :: cstnc ! - current station label -
!
#if GUI == 1
! Logical scalars
   Logical, Public :: lmds ! - mouse domain selection? -
!
#endif
! Derived type scalars
   Type(area), Public :: xarea     ! - default X area -
   Type(area), Public :: yarea     ! - default Y area -
   Type(area), Public :: xarea_old ! - backup default X area -
   Type(area), Public :: yarea_old ! - backup default Y area -
!
! Interfaces
!
! Interface assignments
   Private :: Assignment(=)
   Interface Assignment(=)
      Module Procedure assign_area
      Module Procedure assign_domain
   End Interface
!
! Interface Operators
   Public :: Operator(==)
   Interface Operator(==)
      Module Procedure same_domains
   End Interface
!
   Public :: Operator(/=)
   Interface Operator(/=)
      Module Procedure areas_differ
      Module Procedure domains_differ
   End Interface
!
Contains
!
!
 Subroutine assign_area (a1, r)
!
! Assign a scalar to all elements of derived type area
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: r ! - assignment value -
!
! - output scalars -
   Type(area), Intent(Out) :: a1 ! - area -
!
! Executable Statements
!
! Assign area
   a1%rltn = r ! - northern area limit -
   a1%rlts = r ! - southern area limit -
   a1%rlgw = r ! - western area limit -
   a1%rlge = r ! - eastern area limit -
!
  Return
 End Subroutine assign_area
!
!
!
 Subroutine assign_domain (d1, i)
!
! Assign a scalar to all elements of derived type domain
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: i ! - assignment value -
!
! - output scalars -
   Type(domain), Intent(Out) :: d1 ! - domain -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Assign domain
   d1%nlts = i ! - number of latitudes in domain -
   d1%nlgs = i ! - number of longitudes in domain -
   d1%nlt1 = i ! - northern latitude domain limit index -
   d1%nlt2 = i ! - southern latitude domain limit index -
   d1%nlg1 = i ! - western longitude domain limit index -
   d1%nlg2 = i ! - eastern longitude domain limit index -
!
   d1%alim = Real(i, Kind=rp) ! - area limits -
!
   Return
 End Subroutine assign_domain
!
!
!
 Function same_domains(adomain, bdomain) &
          Result(same)
!
! Are domains the same?
!
! Function result
   Logical :: same
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(domain), Intent(In) :: adomain ! - first domain -
   Type(domain), Intent(In) :: bdomain ! - second domain -
!
! Executable Statements
!
! Compare domain settings
   same = .false.
   If (adomain%nlts /= bdomain%nlts) Return
   If (adomain%nlgs /= bdomain%nlgs) Return
   If (adomain%nlt1 /= bdomain%nlt1) Return
   If (adomain%nlt2 /= bdomain%nlt2) Return
   If (adomain%nlg1 /= bdomain%nlg1) Return
   If (adomain%nlg2 /= bdomain%nlg2) Return
   If (adomain%alim /= bdomain%alim) Return
   same = .true.
!
   Return
 End Function same_domains
!
!
!
 Function areas_differ(aarea, barea) &
          Result(differ)
!
! Do areas differ?
!
! Function result
   Logical :: differ
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(area), Intent(In) :: aarea ! - first area -
   Type(area), Intent(In) :: barea ! - second area -
!
! Executable Statements
!
! Compare area settings
   differ = .true.
   If (aarea%rltn /= barea%rltn) Return
   If (aarea%rlts /= barea%rlts) Return
   If (aarea%rlgw /= barea%rlgw) Return
   If (aarea%rlge /= barea%rlge) Return
   differ = .false.
!
   Return
 End Function areas_differ
!
!
!
 Function domains_differ(adomain, bdomain) &
          Result(differ)
!
! Do domains differ?
!
! Function result
   Logical :: differ
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(domain), Intent(In) :: adomain ! - first domain -
   Type(domain), Intent(In) :: bdomain ! - second domain -
!
! Executable Statements
!
! Compare domain settings
   differ = .true.
   If (adomain%nlts /= bdomain%nlts) Return
   If (adomain%nlgs /= bdomain%nlgs) Return
   If (adomain%nlt1 /= bdomain%nlt1) Return
   If (adomain%nlt2 /= bdomain%nlt2) Return
   If (adomain%nlg1 /= bdomain%nlg1) Return
   If (adomain%nlg2 /= bdomain%nlg2) Return
   If (adomain%alim /= bdomain%alim) Return
   differ = .false.
!
   Return
 End Function domains_differ
!
!
!
 Subroutine reset_space ()
!
! Reset current location indices
!
! Executable Statements
!
! Reset space
   ilaty = 1
   ilngy = 1
!
   Return
 End Subroutine reset_space
!
!
!
 Function init_space(nlt, nlg, ndim) &
          Result (init)
!
! Initialises memory for space
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlt  ! - number of latitudes -
   Integer, Intent(In) :: nlg  ! - number of longitudes -
   Integer, Intent(In) :: ndim ! - number of dimensions -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - nearest latitudes -
   Allocate (rltnr(nlt,ndim), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - nearest longitudes -
   Allocate (rlgnr(nlg,ndim), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   init = 0
   Return
!
! Errors
1  init = 1
   Return
 End Function init_space
!
!
!
 Subroutine nullify_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom)
!
! Reset latitudes and longitudes, assigning memory if optional arguments are present
!
! Arguments
!
! Pointer arrays
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - longitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - longitudes within domain -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - station labels -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - station labels within domain -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Associated
!
! Executable Statements
!
! Clear pointers
   If (Associated(idom)) Then
      Deallocate (idom)
      NullIfY (idom)
   End If
   If (Associated(cstnd)) Then
      Deallocate (cstnd)
      NullIfY (cstnd)
   End If
   If (Associated(rlngd)) Then
      Deallocate (rlngd)
      NullIfY (rlngd)
   End If
   If (Associated(rlatd)) Then
      Deallocate (rlatd)
      NullIfY (rlatd)
   End If
   If (Associated(cstn)) Then
      Deallocate (cstn)
      NullIfY (cstn)
   End If
   If (Associated(rlng)) Then
      Deallocate (rlng)
      NullIfY (rlng)
   End If
   If (Associated(rlat)) Then
      Deallocate (rlat)
      NullIfY (rlat)
   End If
!
   Return
 End Subroutine nullify_grids
!
!
!
 Subroutine reset_grids (idstr, ndom, nlt, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
                         nlg)
!
! Reset latitudes and longitudes, assigning memory if optional arguments are present
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: idstr ! - data structure -
   Integer, Intent(In) :: ndom  ! - number of domains -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nlt ! - number of latitudes -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - longitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - longitudes within domain -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - station labels -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - station labels within domain -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: nlg ! - number of longitudes -
!
! Locals
!
! Local scalars
   Integer :: ios ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal, Present
!
! Executable Statements
!
! Clear pointers
   Call nullify_grids (rlat, rlng, cstn, rlatd, rlngd, cstnd, idom)
!
! Reset
   Select Case (idstr)
    Case (id_grd) ! - gridded data -
      If (.not.Present(nlg)) Then
         ifail = 1
         Return
      End If
      Allocate (rlat(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlng(MaxVal(nlg(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (cstn(1, 1), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlatd(Max(2, MaxVal(nlt(:))), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlngd(Max(2, MaxVal(nlg(:))), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (cstnd(1, 1), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (idom(MaxVal(nlg(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      rlatd(:,:) = zero
      rlngd(:,:) = zero
    Case (id_stn) ! - station data -
      Allocate (rlat(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlng(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (cstn(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlatd(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlngd(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (cstnd(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (idom(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      rlatd(:,:) = zero
      rlngd(:,:) = zero
      cstnd(:,:) = ' '
    Case (id_ind) ! - unreferenced data -
      Allocate (rlat(1, 1), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlng(1, 1), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (cstn(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlatd(1, 1), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (rlngd(1, 1), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (cstnd(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      Allocate (idom(MaxVal(nlt(:)), ndom), Stat=ios)
      If (ios /= 0) GoTo 1
      cstnd(:,:) = ' '
   End Select
   idom(:,:) = 0
!
   ifail = 0
   Return
!
! Errors
1  Call nullify_grids(rlat, rlng, cstn, rlatd, rlngd, cstnd, idom)
   ifail = 1
!
   Return
 End Subroutine reset_grids
!
!
!
 Function get_geog(idstr) &
          Result (geog)
!
! Identifies whether a data structure has a spatial component
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn
!
! Function result
   Integer :: geog
!
! Arguments
!
! Dummy arguments
   Integer, Intent(In) :: idstr ! - data structure -
!
! Executable Statements
!
! Identify whether data structure has a spatial component
   Select case (idstr)
    Case (id_grd, id_stn)
      geog = 1
    Case Default
      geog = 0
   End Select
!
   Return
 End Function get_geog
!
!
!
 Subroutine prompt_domain (ctitle, alim, &
            dlim)
!
! Prompts for a spatial domain
!
! Modules
   Use data_numbers, Only: r90, r360
   Use labels,       Only: cg_approxdlb, cg_latnmost, cg_latsmost, cg_lngwmost, cg_lngemost, cg_neglatlng
   Use gui,          Only: print_text
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctitle ! - title -
!
   Type(area), Intent(InOut) :: alim ! - domain limits -
!
! Optional arguments
! - optional input scalars -
   Type(area), Intent(In), Optional :: dlim ! - data limits -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable domain
!
! Print prompt information
   Call print_text (ctitle//':', &
        lbold=.true.)
   Call print_text ('('//Trim(cg_neglatlng)//')')
   If (Present(dlim)) Call print_text ('('//Trim(cg_approxdlb)//')')
   Call print_text (' ')
!
! Prompt for latitude and longitude limits
! - northernmost latitude -
   Call prompt_latlon (Trim(cg_latnmost), -r90, r90, alim%rltn, &
        dlim=dlim%rltn)
! - southernmost latitude -
   Call prompt_latlon (Trim(cg_latsmost), -r90, r90, alim%rlts, &
        dlim=dlim%rlts)
! - westernmost longitude -
   Call prompt_latlon (Trim(cg_lngwmost), -r360, r360, alim%rlgw, &
        dlim=dlim%rlgw)
! - easternmost longitude -
   Call prompt_latlon (Trim(cg_lngemost), -r360, r360, alim%rlge, &
        dlim=dlim%rlge)
!
   Return
!
 Contains
!
!
  Subroutine prompt_latlon (cltln, rlow, rhgh, r, &
             dlim)
!
! Prompts for a latitude or longitude
!
! Modules
   Use gui, Only: prompt_real
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rlow ! - lower limit -
   Real(Kind=rp), Intent(In) :: rhgh ! - upper limit -
!
   Character(Len=*), Intent(In) :: cltln ! - latitude / longitude identifier -
!
! - input/output scalars -
   Real(Kind=rp), Intent(InOut) :: r ! - latitude/longitude -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: dlim ! - data limit -
!
! Locals
!
! Local scalars
   Character(Len=64) :: cprmpt ! - prompt -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Present, Trim
!
! Executable Statements
!
! Prompt for latitude / longitude
! - indicate latitude or longitude -
   cprmpt = cltln
! - add limit -
   If (Present(dlim)) Write (Unit=cprmpt, Fmt='(A,I4,A)') Trim(cprmpt)//' (', Nint(dlim), ')'
! - prompt -
   Call prompt_real (Trim(cprmpt), r, &
        itab=32, rlow=rlow, rhgh=rhgh)
!
   Return
  End Subroutine prompt_latlon
 End Subroutine prompt_domain
!
!
!
 Subroutine check_grid_domain (nlt, nlg, rlat, rlng, region, rlatd, rlngd, idom, nv, ifail)
!
! Checks whether there are any grids within the domain
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Southern domain limit is north of northern domain limit
!    ifail =  2 Northern domain limit is south of southernmost data
!    ifail =  3 Southern domain limit is north of northernmost data
!    ifail =  4 Longitudinal extent is too small
!    ifail =  5 Invalid longitudinal extent
!
! Modules
   Use data_numbers, Only: zero, r180, r360
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlt ! - number of latitudes -
   Integer, Intent(In) :: nlg ! - number of longitudes -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
! - input/output scalars -
   Type(domain), Intent(InOut) :: region ! - domain -
!
! - input/output arrays -
   Integer, Dimension(:), Intent(InOut) :: idom ! - gridpoints within domain -
!
   Real(Kind=rp), Dimension(:), Intent(InOut) :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:), Intent(InOut) :: rlngd ! - longitudes within domain -
!
! - output scalars -
   Integer, Intent(Out) :: nv    ! - number of grids -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i  ! - latitude index -
   Integer :: j  ! - longitude index -
   Integer :: ii ! - current latitude -
   Integer :: jj ! - current longitude -
!
   Real(Kind=rp) :: rrlgw ! - rescaled western domain limit -
   Real(Kind=rp) :: rrlge ! - rescaled eastern domain limit -
   Real(Kind=rp) :: dlng  ! - longitudinal resolution -
   Real(Kind=rp) :: dmap  ! - longitudinal map extent -
   Real(Kind=rp) :: rwadj ! - western longitudinal shift -
   Real(Kind=rp) :: readj ! - eastern longitudinal shift -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Reset longitudes limits
   If (region%alim%rlgw >= rlng(1) .and. region%alim%rlgw <= rlng(nlg)) Then
      rwadj = zero
   Else If (region%alim%rlgw-r360 >= rlng(1) .and. region%alim%rlgw-r360 <= rlng(nlg)) Then
      rwadj = -r360
   Else If (region%alim%rlgw+r360 >= rlng(1) .and. region%alim%rlgw+r360 <= rlng(nlg)) Then
      rwadj = r360
   Else If (region%alim%rlgw<rlng(1) .and. region%alim%rlge >= rlng(1)) Then
      rwadj = zero
   Else
      ifail = 5
      Return
   End If
   rrlgw = region%alim%rlgw + rwadj
   readj = rwadj
   Do
      If (.not.region%alim%rlge+readj < rrlgw) Exit
      readj = readj + r360
   End Do
   rrlge = region%alim%rlge + readj
!
! Identify indices
! - northernmost -
   If (region%alim%rltn < region%alim%rlts) Then
      ifail = 1
      Return
   End If
   northernmost: Do i = 1, nlt
      If (.not.region%alim%rltn < rlat(i)) Then
         region%nlt1 = i
         Exit northernmost
      End If
   End Do northernmost
   If (i > nlt .or. i < 1) Then
      ifail = 2
      Return
   End If
! - southernmost -
   If (region%alim%rlts > rlat(region%nlt1)) Then
      ifail = 3
      Return
   Else
      southernmost: Do i = nlt, region%nlt1, -1
         If (.not.region%alim%rlts > rlat(i)) Then
            region%nlt2 = i
            Exit southernmost
         End If
      End Do southernmost
   End If
! - westernmost -
   region%nlg1 = nlg
   westernmost: Do j = 1, nlg
      If (.not.rrlgw > rlng(j)) Then
         region%nlg1 = j
         Exit westernmost
      End If
   End Do westernmost
! - easternmost -
   If (rrlge /= rrlgw .and. rrlge-rrlgw < r360) Then
      easternmost: Do
        Do j = region%nlg1+1, nlg
            If (rrlge+readj < rlng(j)) Then
               region%nlg2 = j - 1
               Exit easternmost
            End If
         End Do
         readj = readj - r360
         Do j = 1, region%nlg1
            If (rrlge+readj < rlng(j)) Then
               If (j > 1) Then
                  region%nlg2 = j - 1
               Else
                  region%nlg2 = nlg
               End If
               Exit easternmost
            End If
         End Do
      End Do easternmost
   Else If (region%nlg1 > 1) Then
      region%nlg2 = region%nlg1 - 1
   Else
      region%nlg2 = nlg
   End If
!
! Calculate number of grids
   region%nlts = region%nlt2 + 1 - region%nlt1
   If (region%nlg2 >= region%nlg1) Then
      region%nlgs = region%nlg2 + 1 - region%nlg1
   Else
      region%nlgs = region%nlg2 + nlg + 1 - region%nlg1
   End If
   If (region%nlg1 /= 1) Then
      dlng = rlng(region%nlg1) - rlng(region%nlg1-1)
   Else If (region%alim%rlge > region%alim%rlgw .and. .not.(region%alim%rlge < rlng(1))) Then
      If (nlg > 1) Then
         dlng = rlng(2) - rlng(1)
      Else
         dlng = zero
      End If
   Else
      dlng = r360 + rlng(1) - rlng(nlg)
   End If
   If (region%alim%rlge < zero) Then
      dmap = region%alim%rlgw - region%alim%rlge
      If (dmap > zero) Then
         dmap = r360 - dmap
      Else
         dmap = -dmap
      End If
   Else
      dmap = region%alim%rlge - region%alim%rlgw
   End If
   If (region%nlgs == nlg .and. dlng > dmap .and. dmap > zero) Then
      ifail = 4
      Return
   End If
   nv = region%nlgs*region%nlts
!
! Determine which latitudes are within the domain
   ii = 0
   Do i = region%nlt1, region%nlt2
      ii = ii + 1
      rlatd(ii) = rlat(i)
   End Do
!
! Determine which longitudes are within the domain
   jj = 0
   If (region%nlg1 <= region%nlg2) Then
      Do j = region%nlg1, region%nlg2
         jj = jj + 1
         idom(jj) = j
         rlngd(jj) = rlng(j)
      End Do
   Else
     Do j = region%nlg1, nlg
        jj = jj + 1
        idom(jj) = j
        rlngd(jj) = rlng(j)
      End Do
      ii = jj
      Do j = 1, region%nlg2
         jj = jj + 1
         idom(jj) = j
         rlngd(jj) = rlng(j)
      End Do
      check_rlng: Do
         If (Any(rlngd(1:region%nlgs-1) > rlngd(2:region%nlgs))) Then
           Do j = 2, region%nlgs
               If (rlngd(j-1) > rlngd(j)) Then
                  rlngd(j:region%nlgs) = rlngd(j:region%nlgs) + r360
                  Exit
               End If
            End Do
         Else If (Any(rlngd(1:region%nlgs) > r360)) Then
            rlngd(1:region%nlgs) = rlngd(1:region%nlgs) - r360
         Else
            Exit check_rlng
         End If
      End Do check_rlng
   End If
   If (Any(rlngd(1:region%nlgs) < -r180)) rlngd(1:region%nlgs) = rlngd(1:region%nlgs) + r360
   ifail = 0
!
   Return
 End Subroutine check_grid_domain
!
!
!
 Subroutine check_stn_domain (mst, rlatn, rlats, alim, rlat, rlng, cstn, nst, rlatd, rlngd, cstnd, idom, ifail)
!
! Checks whether there are any stations within the domain
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Southern domain limit is north of northern domain limit
!    ifail =  2 Northern domain limit is south of southernmost data
!    ifail =  3 Southern domain limit is north of northernmost data
!    ifail =  4 No stations within domain
!
! Modules
   Use data_numbers, Only: r360
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: mst ! - total number of stations -
!
   Real(Kind=rp), Intent(In) :: rlatn ! - northernmost data latitude -
   Real(Kind=rp), Intent(In) :: rlats ! - southernmost data latitude -
!
   Type(area), Intent(In) :: alim ! - area limits -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
!
   Character(Len=*), Dimension(:), Intent(In) :: cstn ! - station labels -
!
! - output scalars -
   Integer, Intent(Out) :: nst   ! - number of stations within domain -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Integer, Dimension(:), Intent(Out) :: idom ! - stations within domain -
!
   Real(Kind=rp), Dimension(:), Intent(Out) :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:), Intent(Out) :: rlngd ! - longitudes within domain -
!
   Character(Len=*), Dimension(:), Intent(Out) :: cstnd ! - station labels within domain -
!
! Locals
!
! Local scalars
   Integer :: i ! - latitude index -
!
   Real(Kind=rp) :: rrlge ! - rescaled eastern domain limit -
!
! Executable Statements
!
! Identify stations within domain
   nst = 0
   If (alim%rlgw < alim%rlge) Then
      rrlge = alim%rlge
   Else
      rrlge = alim%rlge + r360
   End If
   Do i = 1, mst
      If (rlat(i)      <= alim%rltn .and. rlat(i)      >= alim%rlts .and.  &
        ((rlng(i)      >= alim%rlgw .and. rlng(i)      <= rrlge) .or.  &
         (rlng(i)+r360 >= alim%rlgw .and. rlng(i)+r360 <= rrlge) .or.  &
         (rlng(i)-r360 >= alim%rlgw .and. rlng(i)-r360 <= rrlge))) Then
         nst = nst + 1
         rlatd(nst) = rlat(i)
         rlngd(nst) = rlng(i)
         cstnd(nst) = cstn(i)
         idom(nst) = i
      End If
   End Do
!
! Diagnose lack of stations
   If (nst > 0) Then
      ifail = 0
   Else
      If (alim%rltn < alim%rlts) Then
         ifail = 1
      Else If (alim%rltn < rlats) Then
         ifail = 2
      Else If (alim%rlts > rlatn) Then
         ifail = 3
      Else
         ifail = 4
      End If
   End If
!
   Return
 End Subroutine check_stn_domain
!
!
!
#if GUI == 1
 Function select_domain()
!
! Identifies a domain from a mouse box selection
!
! Modules
   Use mswinprm$, Only: mk_lbutton
   Use clrwin$,   Only: clearwin_info$, get_graphics_selected_area$
   Use screen,    Only: pixel_ix, pixel_iy
!
! Function result
   Integer :: select_domain
!
! Locals
!
! Local scalars
   Integer :: ix1   ! - longitude of first corner -
   Integer :: ix2   ! - longitude of second corner -
   Integer :: iy1   ! - latitude of first corner -
   Integer :: iy2   ! - latitude of second corner -
   Integer :: iflag ! - mouse state -
!
   Logical :: isup           ! - mouse is up? -
   Logical :: wsdn = .false. ! - mouse was down? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic IAnd, Int
!
! Executable Statements
!
! Get coordinates of selected area
   iflag = Int(clearwin_info$('graphics_mouse_flags'))
   isup = (IAnd(iflag, mk_lbutton) == 0)
   If (isup .and. wsdn) Then
      Call get_graphics_selected_area$ (ix1, iy1, ix2, iy2)
      x1 = pixel_ix(ix1)
      x2 = pixel_ix(ix2)
      y1 = pixel_iy(iy1)
      y2 = pixel_iy(iy2)
      lmds = .true.
   End If
   wsdn = .not.isup
   select_domain = 2
!
   Return
 End Function select_domain
!
!
!
#endif
 Subroutine full_domain (nv, nlt, nlg, region, idom)
!
! Assigns settings for a complete domain
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv  ! - number of variables within domain -
   Integer, Intent(In) :: nlt ! - number of latitudes within domain -
   Integer, Intent(In) :: nlg ! - number of longitudes within domain -
!
! - input/output scalars -
   Type(domain), Intent(InOut) :: region ! - domain settings -
!
! - output arrays -
   Integer, Dimension(:), Intent(Out) :: idom ! - variable is within domain -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
!
! Executable Statements
!
! Set domain limits
   region%nlts = nlt ! - number of latitudes in domain -
   region%nlgs = nlg ! - number of longitudes in domain -
   region%nlt1 = 1   ! - northern latitude domain limit index -
   region%nlt2 = nlt ! - southern latitude domain limit index -
   region%nlg1 = 1   ! - western longitude domain limit index -
   region%nlg2 = nlg ! - eastern longitude domain limit index -
!
! Indicate used variables
   Do i = 1, nv
     idom(i) = i
   End Do
!
   Return
 End Subroutine full_domain
!
!
!
 Function set_iprecl(m, nlat, nlng, rlat, rlng) &
          Result (iprecl)
!
! Calculates maximum precision for latitudes and longitudes
!
! Modules
   Use maths, Only: iprec
!
! Function result
   Integer :: iprecl
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of fields -
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nlat ! - numbers of latitudes -
   Integer, Dimension(:), Intent(In) :: nlng ! - numbers of longitudes -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitude -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlng ! - longitude -
!
! Locals
!
! Local parameters
   Integer, Parameter :: mprec = 6 ! - maximum precision required -
!
! Local scalars
   Integer :: i ! - field index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal
!
! Executable Statements
!
! Identify precision
   iprecl = Max(MaxVal(iprec(rlat(1:nlat(1),1), mprec)), MaxVal(iprec(rlng(1:nlng(1),1), mprec)))
   If (m > 1) Then
      Do i = 2, m
         iprecl = Max(iprecl, Max(MaxVal(iprec(rlat(1:nlat(i),i), mprec)), MaxVal(iprec(rlng(1:nlng(i),i), mprec))))
      End Do
   End If
!
   Return
 End Function set_iprecl
!
!
!
 Function make_coors(rlat, rlng)
!
! Constructs coordinate labels from latitude and longitude
!
! Modules
   Use data_numbers,      Only: zero, r180, r360
   Use data_io_constants, Only: lcoo
   Use maths,             Only: get_cnumber, iprec
!
! Function result
   Character(Len=lcoo) :: make_coors
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rlat ! - latitude -
   Real(Kind=rp), Intent(In) :: rlng ! - longitude -
!
! Locals
!
! Local parameters
   Integer, Parameter :: mprec = 2 ! - maximum precision required -
!
! Local scalars
   Integer :: nprec ! - precision -
!
   Real(Kind=rp) :: rw    ! - longitude in western hemisphere -
   Real(Kind=rp) :: rrlng ! - realigned longitude -
!
   Character(Len=6) :: clat ! - latitude -
   Character(Len=7) :: clng ! - longitude -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Trim
!
! Executable Statements
!
! Realign longitude if necessary
   rrlng = rlng
   Do
      If (rrlng < -r180) Then
         rrlng = rrlng + r360
      Else If (rrlng > r360) Then
         rrlng = rrlng - r360
      Else
         Exit
      End If
   End Do
!
! Identify precision
   nprec = Max(iprec(rlat, mprec), iprec(rlng, mprec))
!
! Construct coordinate label
! - latitudes -
   If (rlat > zero) Then
      clat = Trim(get_cnumber(rlat, nprec, nprec))//'N'
   Else If (rlat < zero) Then
      clat = Trim(get_cnumber(-rlat, nprec, nprec))//'S'
   Else
      clat = '0'
   End If
! - longitudes -
   If (rrlng > zero) Then
      If (rrlng < r180) Then
         clng = Trim(get_cnumber(rlng, nprec, nprec))//'E'
      Else If (rrlng > r180) Then
         rw = rrlng - r360
         clng = Trim(get_cnumber(-rw, nprec, nprec))//'W'
      Else
         clng = '180'
      End If
   Else If (rrlng < zero) Then
      clng = Trim(get_cnumber(-rrlng, nprec, nprec))//'W'
   Else
      clng = '0'
   End If
   make_coors = Trim(clat)//', '//Trim(clng)
!
   Return
 End Function make_coors
!
!
!
 Function make_map_coor(rlat, rlng)
!
! Constructs map coordinate labels from latitude and longitude
!
! Modules
   Use data_numbers, Only: zero, r180, r360
!
! Function result
   Character(Len=18) :: make_map_coor
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rlat ! - latitude -
   Real(Kind=rp), Intent(In) :: rlng ! - longitude -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: rlt   ! - latitude -
   Real(Kind=rp) :: rlg   ! - longitude -
   Real(Kind=rp) :: rrlng ! - realigned longitude -
!
   Character(Len=1) :: clt ! - latitude -
   Character(Len=1) :: clg ! - longitude -
!
! Executable Statements
!
! Realign longitude if necessary
   rrlng = rlng
   Do
      If (rrlng < -r180) Then
         rrlng = rrlng + r360
      Else If (rrlng > r360) Then
         rrlng = rrlng - r360
      Else
         Exit
      End If
   End Do
!
! Construct coordinate label
! - latitudes -
   If (rlat > zero) Then
      rlt = rlat
      clt = 'N'
   Else If (rlat < zero) Then
      rlt = -rlat
      clt = 'S'
   Else
      rlt = zero
      clt = ' '
   End If
   If (rrlng > zero) Then
      If (rrlng > r180) Then
         rlg = r360 - rrlng
         clg = 'W'
      Else If (rrlng < r180) Then
         rlg = rrlng
         clg = 'E'
      Else
         rlg = r180
         clg = ' '
      End If
   Else If (rrlng < zero) Then
      rlg = -rlng
      clg = 'W'
   Else
      rlg = zero
      clg = ' '
   End If
   Write (Unit=make_map_coor, Fmt='(2(F8.2,A))') rlt, clt, rlg, clg
!
   Return
 End Function make_map_coor
!
!
!
 Subroutine check_latitude_ordering (nlt, rlat, ln2s, ifail)
!
! Checks whether latitudes are oriented north-to-south or south-to-north
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlt ! - number of latitudes -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Logical, Intent(Out) :: ln2s ! - north-to-south latitude ordering? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Check ordering of latitudes
   ifail = 0
   If (rlat(1) > rlat(nlt)) Then
      ln2s = .true.
      If (Any(rlat(1:nlt-1) < rlat(2:nlt))) ifail = 1
   Else
      ln2s = .false.
      If (Any(rlat(1:nlt-1) > rlat(2:nlt))) ifail = 1
   End If
!
   Return
 End Subroutine check_latitude_ordering
!
!
!
 Subroutine latitude_weight (ndom, nlt, region, rlat, iuse, nt, v)
!
! Weights gridded data by cosine of latitude
!
! Modules
   Use data_numbers, Only: three, pi, r180, r360
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ndom ! - number of domains -
   Integer, Intent(In) :: nt   ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nlt  ! - numbers of latitudes per domain -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables indices -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rlat ! - latitudes -
!
   Type(domain), Dimension(:), Intent(In) :: region ! - domain settings -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i  ! - latitude index -
   Integer :: j  ! - longitude index -
   Integer :: ij ! - latitude/longitude index -
   Integer :: ii ! - available series index -
   Integer :: id ! - domain index -
!
   Real(Kind=rp) :: r1  ! - first latitude limit -
   Real(Kind=rp) :: r2  ! - second latitude limit -
   Real(Kind=rp) :: wt  ! - latitude weighting -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Cos, Sin, Sqrt
!
! Executable Statements
!
! Set increment
   ii = 1
   ij = 0
   Do id = 1, ndom
!
! Weight by cosine of latitude
      Select Case (nlt(id))
       Case (1) ! - no weighting if only one line of latitude -
         Continue
       Case (2) ! - simple cosine weighting if two lines of latitude -
         Do i = region(id)%nlt1, region(id)%nlt2
            wt = Cos(rlat(i,id)*pi/r180)
            wt = Sqrt(wt)
            Do j = 1, region(id)%nlgs
               ij = ij + 1
               If (iuse(ii) == ij) Then
                  v(ii,1:nt) = v(ii,1:nt)*wt
                  ii = ii + 1
               End If
            End Do
         End Do
      Case (3:) ! - integrated cosine weighting if multiple lines of latitude -
         Do i = region(id)%nlt1, region(id)%nlt2
            If (i == 1) Then
               r1 = (three*rlat(1,id) - rlat(2,id))*pi/r360
               r2 = (rlat(1,id) + rlat(2,id))*pi/r360
            Else If (i < nlt(id)) Then
               r1 = (rlat(i,id) + rlat(i-1,id))*pi/r360
               r2 = (rlat(i,id) + rlat(i+1,id))*pi/r360
            Else
               r1 = (rlat(nlt(id),id) + rlat(nlt(id)-1,id))*pi/r360
               r2 = (three*rlat(nlt(id),id) - rlat(nlt(id)-1,id))*pi/r360
            End If
            wt = (Sin(r1) - Sin(r2))/(r1 - r2)
            wt = Sqrt(Abs(wt))
            Do j = 1, region(id)%nlgs
               ij = ij + 1
               If (iuse(ii) == ij) Then
                  v(ii,1:nt) = v(ii,1:nt)*wt
                  ii = ii + 1
               End If
            End Do
         End Do
      End Select
   End Do
!
   Return
 End Subroutine latitude_weight
!
!
!
 Subroutine set_nearest_grids (iydstr, intp, ndx, ndy, nltx, nlgx, nvay, xregion, yregion, ifail, &
            imfs)
!
! Identifies nearest grid boxes.
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: id_grd, id_stn
   Use progress_meter,     Only: set_progress_increment, update_progress_meter
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iydstr ! - Y data structure -
   Integer, Intent(In) :: intp   ! - interpolation option -
   Integer, Intent(In) :: ndx    ! - number of X domains -
   Integer, Intent(In) :: ndy    ! - number of Y domains -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nltx ! - number of X latitudes -
   Integer, Dimension(:), Intent(In) :: nlgx ! - number of X longitudes -
   Integer, Dimension(:), Intent(In) :: nvay ! - numbers of available Y variables -
!
   Type(domain), Dimension(:), Intent(In) :: xregion ! - X domains -
   Type(domain), Dimension(:), Intent(In) :: yregion ! - Y domains -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Optional :: imfs ! - matched X fields  -
!
! Locals
!
! Local scalars
   Integer :: i     ! - latitude index -
   Integer :: j     ! - longitude index -
   Integer :: idx   ! - X domain index -
   Integer :: idy   ! - Y domain index -
   Integer :: idi   ! - domain index -
   Integer :: ii    ! - latitude index -
   Integer :: ij    ! - station index -
   Integer :: imin1 ! - nearest latitude -
   Integer :: imin2 ! - second nearest latitude -
   Integer :: jmin1 ! - nearest longitude -
   Integer :: jmin2 ! - second nearest longitude -
!
   Real(Kind=rp) :: aprog ! - progress increment adjustment -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real, Sign
!
! Executable Statements
!
! Identify nearest gridpoints
   ifail = -1
   Select Case (iydstr)
    Case (id_grd)
      aprog = Real(yregion(1)%nlts*yregion(1)%nlgs, Kind=rp)/Real(yregion(1)%nlts+yregion(1)%nlgs, Kind=rp)
      Call set_progress_increment (aprog, .true.)
! - latitudes -
      Do idy = 1, ndy
         Do i = yregion(idy)%nlt1, yregion(idy)%nlt2
            ii = i + 1 - yregion(idy)%nlt1
            Do idx = 1, ndx
               If (Present(imfs)) Then
                  If (idx /= imfs(idy)) Cycle
                  idi = idy
               Else
                  idi = idx
               End If
               Call get_nearest_lat (rlaty(i,idy), nltx(idx), xregion(idx)%nlt1, xregion(idx)%nlt2, rlatx(:,idx), imin1, imin2)
! - interpolate -
               If (imin1 > 0) Then
                  rltnr(ii,idi) = Real(imin1, Kind=rp)
                  If (intp == 1 .and. imin1 /= imin2) Then
                     rltnr(ii,idi) = rltnr(ii,idi) +  &
                          frac_lat(rlaty(i,idy), rlatdx(imin1,idx), rlatdx(imin2,idx))*Real(Sign(1, imin2-imin1), Kind=rp)
                  End If
! - X domain is too small -
               Else
                  ifail = -imin1
                  Return
               End If
               If (update_progress_meter(.false., ifail=ifail) /= 0) Return
            End Do
         End Do
! - longitudes -
        Do idx = 1, ndx
            If (Present(imfs)) Then
               If (idx /= imfs(idy)) Cycle
                  idi = idy
               Else
                  idi = idx
            End If
            Do j = 1, yregion(idy)%nlgs
               Call get_nearest_lng (rlngdy(j,idy), nlgx(idx), xregion(idx)%nlg1, xregion(idx)%nlg2, rlngx(:,idx), jmin1, jmin2)
! - interpolate -
               If (jmin1 > 0) Then
                  rlgnr(j,idi) = Real(jmin1, Kind=rp)
                  If (intp == 1 .and. jmin1 /= jmin2) Then
                     rlgnr(j,idi) = rlgnr(j,idi) +  &
                          frac_lng(rlngdy(j,idy), rlngdx(jmin1,idx), rlngdx(jmin2,idx))*Real(Sign(1, jmin2-jmin1), Kind=rp)
                  End If
! - X domain is too small -
               Else
                  ifail = 2 - jmin1
                  Return
               End If
               If (update_progress_meter(.false., ifail=ifail) /= 0) Return
            End Do
         End Do
      End Do
      Call set_progress_increment (one/aprog, .true.)
!
! Identify nearest gridpoints to stations
   Case (id_stn)
     Do idy = 1, ndy
        Do idx = 1, ndx
            If (Present(imfs)) Then
               If (idx /= imfs(idy)) Cycle
               idi = idy
            Else
               idi = idx
            End If
            Do ij = 1, nvay(idy)
! - latitudes -
               Call get_nearest_lat (rlatdy(ij,idy), nltx(idx), xregion(idx)%nlt1, xregion(idx)%nlt2, rlatx(:,idx), imin1, imin2)
               If (imin1 < 0) Then
                  ifail = -imin1
                  Return
               End If
! - longitudes -
               Call get_nearest_lng (rlngdy(ij,idy), nlgx(idx), xregion(idx)%nlg1, xregion(idx)%nlg2, rlngx(:,idx), jmin1, jmin2)
               If (jmin1 < 0) Then
                  ifail = 2 - jmin1
                  Return
               End If
! - interpolate -
               rltnr(ij,idi) = Real(imin1, Kind=rp)
               rlgnr(ij,idi) = Real(jmin1, Kind=rp)
               If (intp == 1) Then
                  If (imin1 /= imin2) Then
                     rltnr(ij,idi) = rltnr(ij,idi) + &
                          frac_lat(rlatdy(ij,idy), rlatdx(imin1,idx), rlatdx(imin2,idx))*Real(Sign(1, imin2-imin1), Kind=rp)
                  End If
                  If (jmin1 /= jmin2) Then
                     rlgnr(ij,idi) = rlgnr(ij,idi) + &
                          frac_lng(rlngdy(ij,idy), rlngdx(jmin1,idx), rlngdx(jmin2,idx))*Real(Sign(1, jmin2-jmin1), Kind=rp)
                  End If
               End If
               If (update_progress_meter(.false., ifail=ifail) /= 0) Return
            End Do
         End Do
      End Do
   End Select
   ifail = 0
!
   Return
!
 Contains
!
!
  Subroutine get_nearest_lat (rlt, nlt, nlt1, nlt2, rlts, imin1, imin2)
!
! Identifies nearest two latitudes
!
! Modules
   Use data_numbers, Only: zero, r180
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlt  ! - number of latitudes -
   Integer, Intent(In) :: nlt1 ! - northernmost latitude -
   Integer, Intent(In) :: nlt2 ! - southernmost latitude -
!
   Real(Kind=rp), Intent(In) :: rlt ! - latitude -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlts ! - latitudes -
!
! - output scalars -
   Integer, Intent(Out) :: imin1 ! - nearest latitude -
   Integer, Intent(Out) :: imin2 ! - second nearest latitude -
!
! Locals
!
! Local scalars
   Integer :: i ! - latitude index -
!
   Real(Kind=rp) :: d     ! - distance -
   Real(Kind=rp) :: dmin1 ! - minimum distance -
   Real(Kind=rp) :: dmin2 ! - second minimum distance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Find nearest latitudes
   dmin1 = r180
   dmin2 = r180
   imin1 = 1
   imin2 = 1
   get_lat: Do i = 1, nlt
      d = Abs(rlt-rlts(i))
      If (d == zero) Then
         imin1 = i
         imin2 = i
         Exit get_lat
      End If
      If (d < dmin1) Then
         dmin2 = dmin1
         imin2 = imin1
         dmin1 = d
         imin1 = i
      Else If (d < dmin2) Then
         dmin2 = d
         imin2 = i
      Else
         Exit get_lat
      End If
   End Do get_lat
!
! Check whether latitude is within the domain
   If (imin1 >= nlt1 .and. imin1 <= nlt2 .and. imin2 >= nlt1 .and. imin2 <= nlt2) Then
      imin1 = imin1 + 1 - nlt1
      imin2 = imin2 + 1 - nlt1
   Else If (imin1 < nlt1 .or. imin2 < nlt1) Then
      imin1 = -1
   Else
      imin1 = -2
   End If
!
   Return
  End Subroutine get_nearest_lat
!
!
!
  Subroutine get_nearest_lng (rlg, nlg, nlg1, nlg2, rlgs, jmin1, jmin2)
!
! Identifies nearest two longitudes
!
! Modules
   Use data_numbers, Only: zero, r180, r360
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nlg  ! - number of longitudes -
   Integer, Intent(In) :: nlg1 ! - westernmost longitude -
   Integer, Intent(In) :: nlg2 ! - easternmost longitude -
!
   Real(Kind=rp), Intent(In) :: rlg ! - longitude -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlgs ! - longitudes -
!
! - output scalars -
   Integer, Intent(Out) :: jmin1 ! - nearest longitude -
   Integer, Intent(Out) :: jmin2 ! - second nearest longitude -
!
! Locals
!
! Local scalars
   Integer :: j    ! - longitude index -
!
   Real(Kind=rp) :: d     ! - distance -
   Real(Kind=rp) :: dmin1 ! - minimum distance -
   Real(Kind=rp) :: dmin2 ! - second minimum distance -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Find nearest longitudes
   dmin1 = r180
   dmin2 = r180
   jmin1 = 0
   jmin2 = 0
   Do j = 1, nlg
      d = Abs(rlg-rlgs(j))
      Do
         If (d < r360) Exit
         d = d - r360
      End Do
      If (d > r180) d = r360 - d
      If (d == zero) Then
         jmin1 = j
         jmin2 = j
         Exit
      End If
      If (d < dmin1) Then
         dmin2 = dmin1
         jmin2 = jmin1
         dmin1 = d
         jmin1 = j
      Else If (d < dmin2) Then
         dmin2 = d
         jmin2 = j
      End If
   End Do
!
! Check whether longitude is within the domain
   If (nlg1 < nlg2) Then
      If (jmin1 >= nlg1 .and. jmin1 <= nlg2 .and. jmin2 >= nlg1 .and. jmin2 <= nlg2) Then
         jmin1 = jmin1 + 1 - nlg1
         jmin2 = jmin2 + 1 - nlg1
      Else If (jmin1 < nlg1 .or. jmin2 < nlg1) Then
         jmin1 = -1
      Else
         jmin1 = -2
      End If
   Else
      If ((jmin1 >= nlg1 .or. jmin1 <= nlg2) .and. (jmin2 >= nlg1 .or. jmin2 <= nlg2)) Then
         If (jmin1 >= nlg1) Then
            jmin1 = jmin1 + 1 - nlg1
         Else
            jmin1 = jmin1 + nlg + 1 - nlg1
         End If
         If (jmin2 >= nlg1) Then
            jmin2 = jmin2 + 1 - nlg1
         Else
            jmin2 = jmin2 + nlg + 1 - nlg1
         End If
      Else If (jmin1 < nlg1 .or. jmin2 < nlg1) Then
         jmin1 = -1
      Else
         jmin1 = -2
      End If
   End If
!
   Return
  End Subroutine get_nearest_lng
!
!
!
  Function frac_lat(c1, c2, c3)
!
! Calculates proportional distance from c1 to c2 compared to the distance c2 to c3
!
! Function result
   Real(Kind=rp) :: frac_lat
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: c1 ! - first coordinate -
   Real(Kind=rp), Intent(In) :: c2 ! - second coordinate -
   Real(Kind=rp), Intent(In) :: c3 ! - third coordinate -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: d12 ! - distance from c1 to c2 -
   Real(Kind=rp) :: d13 ! - distance from c1 to c3 -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Check for identical locations
   d12 = Abs(c1-c2)
   d13 = Abs(c3-c1)
!
! Calculate proportional distance
   frac_lat = d12/(d12 + d13)
!
   Return
  End Function frac_lat
!
!
!
  Function frac_lng(c1, c2, c3)
!
! Calculates proportional distance from c1 to c2 compared to the distance c2 to c3
!
! Modules
   Use data_numbers, Only: r180, r360
!
! Function result
   Real(Kind=rp) :: frac_lng
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: c1 ! - first coordinate -
   Real(Kind=rp), Intent(In) :: c2 ! - second coordinate -
   Real(Kind=rp), Intent(In) :: c3 ! - third coordinate -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: d12 ! - distance from c1 to c2 -
   Real(Kind=rp) :: d13 ! - distance from c1 to c3 -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs
!
! Executable Statements
!
! Check for identical locations
   d12 = Abs(c2-c1)
   Do
      If (d12 <= r180) Exit
      d12 = Abs(r360-d12)
   End Do
   d13 = Abs(c1 - c3)
   Do
      If (d13 <= r180) Exit
      d13 = Abs(r360-d13)
   End Do
!
! Calculate proportional distance
   frac_lng = d12/(d12 + d13)
!
   Return
  End Function frac_lng
 End Subroutine set_nearest_grids
!
!
!
 Subroutine get_interpolated (ianal, ixdstr, iydstr, intp, mya, iusex, iusey, ndx, ndy, nu, ngs, nvx, nvay, &
            x, rmiss, xregion, yregion, xiny, &
            imfs)
!
! Interpolates to new grid or station locations.
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: ia_gcm, ia_pfv, id_grd, id_stn, id_ind
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal  ! - analysis identifier -
   Integer, Intent(In) :: ixdstr ! - X data structure -
   Integer, Intent(In) :: iydstr ! - Y data structure -
   Integer, Intent(In) :: intp   ! - interpolation option -
   Integer, Intent(In) :: mya    ! - total number of used Y variables -
   Integer, Intent(In) :: ndx    ! - number of X domains -
   Integer, Intent(In) :: ndy    ! - number of Y domains -
   Integer, Intent(In) :: nu     ! - number of used cases -
   Integer, Intent(In) :: ngs    ! - number of categories -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iusey ! - indices of used Y variables -
   Integer, Dimension(:), Intent(In) :: iusex ! - indices of used X variables -
   Integer, Dimension(:), Intent(In) :: nvx   ! - numbers of X variables -
   Integer, Dimension(:), Intent(In) :: nvay  ! - numbers of available Y variables -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rmiss ! - output missing values -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - X data -
!
   Type(domain), Dimension(:), Intent(In) :: yregion ! - Y domains -
   Type(domain), Dimension(:), Intent(In) :: xregion ! - X domains -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: xiny ! - interpolated X data -
!
! Optional arguments
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: imfs ! - matched X fields  -
!
! Locals
!
! Local scalars
   Integer :: i    ! - latitude index -
   Integer :: j    ! - longitude index -
   Integer :: idx  ! - X domain index -
   Integer :: idy  ! - Y domain index -
   Integer :: idi   ! - domain index -
   Integer :: i1   ! - first latitude index -
   Integer :: i2   ! - second latitude index -
   Integer :: j1   ! - first longitude index -
   Integer :: j2   ! - second longitude index -
   Integer :: ij   ! - station index -
   Integer :: ij1  ! - first location index -
   Integer :: ij2  ! - second location index -
   Integer :: ij3  ! - third location index -
   Integer :: ij4  ! - fourth location index -
   Integer :: igf  ! - category index -
   Integer :: iox  ! - X domain offset -
!
   Real(Kind=rp) :: wti1 ! - first latitude weight -
   Real(Kind=rp) :: wti2 ! - second longitude weight -
   Real(Kind=rp) :: wtj1 ! - first latitude weight -
   Real(Kind=rp) :: wtj2 ! - second longitude weight -
   Real(Kind=rp) :: wt1  ! - first gridpoint weight -
   Real(Kind=rp) :: wt2  ! - second gridpoint weight -
   Real(Kind=rp) :: wt3  ! - third gridpoint weight -
   Real(Kind=rp) :: wt4  ! - fourth gridpoint weight -
   Real(Kind=rp) :: swt  ! - sum of weights -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Floor, Nint, Present, Real
!
! Executable Statements
!
! Select nearest gridpoints
   Select Case (ixdstr)
    Case (id_grd)
      Select Case (intp)
       Case (0)
         Select Case (iydstr)
          Case (id_grd) ! - gridded -
            Do idy = 1, ndy
               iox = 0
               Do idx = 1, ndx
                  If (idx > 1) iox = iox + nvx(idx-1)
                  If (Present(imfs)) Then
                     If (idx /= imfs(idy)) Cycle
                     If (idy == 1) ij = 1
                     idi = idy
                  Else
                     ij = 1
                     idi = idx
                  End If
                  Do i = 1, yregion(1)%nlts
                     i1 = Nint(rltnr(i,idi))
                     Do j = 1, yregion(1)%nlgs
                        If (iusey(ij) /= (i-1)*yregion(1)%nlgs+j) Cycle
                        j1 = Nint(rlgnr(j,idi))
                        ij1 = check_iv(iox, i1, j1, xregion(idx)%nlgs, iusex)
                        Select Case (ianal)
                         Case (ia_gcm)
                           If (ij1 > 0) Then
                              xiny(ij,1:nu,idx) = x(ij1,1:nu,1)
                           Else
                              xiny(ij,1:nu,idx) = rmiss(idx)
                           End If
                         Case (ia_pfv)
                           If (ij1 > 0) Then
                              xiny(ij,1:nu,:) = x(ij1,1:nu,:)
                           Else
                              xiny(ij,1:nu,:) = rmiss(idx)
                           End If
                        End Select
                        ij = ij + 1
                     End Do
                  End Do
               End Do
            End Do
          Case (id_stn) ! - station -
            Do idy = 1, ndy
               iox = 0
               Do idx = 1, ndx
                  If (idx > 1) iox = iox + nvx(idx-1)
                  If (Present(imfs)) Then
                     If (idx /= imfs(idy)) Cycle
                     If (idy == 1) ij = 1
                     idi = idy
                  Else
                     ij = 1
                     idi = idx
                  End If
                  Do i = 1, nvay(idy)
                     i1 = Nint(rltnr(i,idi))
                     j1 = Nint(rlgnr(i,idi))
                     ij1 = check_iv(iox, i1, j1, xregion(idx)%nlgs, iusex)
                     Select Case (ianal)
                      Case (ia_gcm)
                        If (ij1 > 0) Then
                           xiny(ij,1:nu,idx) = x(ij1,1:nu,1)
                        Else
                           xiny(ij,1:nu,idx) = rmiss(idx)
                        End If
                      Case (ia_pfv)
                        If (ij1 > 0) Then
                           xiny(ij,1:nu,:) = x(ij1,1:nu,:)
                        Else
                           xiny(ij,1:nu,:) = rmiss(idx)
                        End If
                     End Select
                     ij = ij + 1
                  End Do
               End Do
            End Do
          Case (id_ind) ! - unreferenced -
            Select Case (ianal)
             Case (ia_gcm)
               Do idx = 1, ndx
                  xiny(:,:,idx) = x(:,1:nu,1)
               End Do
             Case (ia_pfv)
               xiny(:,:,:) = x(1:mya,1:nu,:)
            End Select
         End Select
!
! Interpolate
       Case (1)
         idx = 1
         Select Case (iydstr)
! - interpolate to grid -
          Case (id_grd)
            iox = 0
            Do idx = 1, ndx
               ij = 1
               Do i = 1, yregion(1)%nlts
                  i1 = Floor(rltnr(i,idx))
                  i2 = Ceiling(rltnr(i,idx))
                  wti1 = Real(i2, Kind=rp) - rltnr(i,idx)
                  wti2 = one - wti1
                  Do j = 1, yregion(1)%nlgs
                     If (iusey(ij) /= (i-1)*yregion(1)%nlgs+j) Cycle
                     j1 = Floor(rlgnr(j,idx))
                     j2 = Ceiling(rlgnr(j,idx))
                     wtj1 = Real(j2, Kind=rp) - rlgnr(j,idx)
                     wtj2 = one - wti1
                     If (j1 < 1) j1 = xregion(idx)%nlgs
                     If (j2 > xregion(idx)%nlgs) j2 = 1
                     ij1 = check_iv(iox, i1, j1, xregion(idx)%nlgs, iusex)
                     ij2 = check_iv(iox, i1, j2, xregion(idx)%nlgs, iusex)
                     ij3 = check_iv(iox, i2, j1, xregion(idx)%nlgs, iusex)
                     ij4 = check_iv(iox, i2, j2, xregion(idx)%nlgs, iusex)
                     wt1 = wti1*wtj1
                     wt2 = wti1*wtj2
                     wt3 = wti2*wtj1
                     wt4 = wti2*wtj2
                     swt = wt1 + wt2 + wt3 + wt4
                     Select Case (ianal)
                      Case (ia_gcm)
                        xiny(ij,1:nu,idx) = (x(ij1,1:nu,1)*wt1 + x(ij2,1:nu,1)*wt2 + &
                                             x(ij3,1:nu,1)*wt3 + x(ij4,1:nu,1)*wt4)/swt
                      Case (ia_pfv)
                        Do igf = 1, ngs
                           xiny(ij,1:nu,igf) = (x(ij1,1:nu,igf)*wt1 + x(ij2,1:nu,igf)*wt2 + &
                                                x(ij3,1:nu,igf)*wt3 + x(ij4,1:nu,igf)*wt4)/swt
                        End Do
                     End Select
                     ij = ij + 1
                  End Do
               End Do
               If (idx < ndx) iox = iox + nvx(idx)
            End Do
! - interpolate to stations -
          Case (id_stn)
            iox = 0
            Do idx = 1, ndx
               Do ij = 1,nvay(1)
                  i1 = Floor(rltnr(ij,idx))
                  i2 = Ceiling(rltnr(ij,idx))
                  wti1 = Real(i2, Kind=rp) - rltnr(ij,idx)
                  wti2 = one - wti1
                  j1 = Floor(rlgnr(ij,idx))
                  j2 = Ceiling(rlgnr(ij,idx))
                  wtj1 = Real(j2, Kind=rp) - rltnr(ij,idx)
                  wtj2 = one - wti1
                  If (j1 < 1) j1 = xregion(idx)%nlgs
                  If (j2 > xregion(idx)%nlgs) j2 = 1
                  ij1 = check_iv(iox, i1, j1, xregion(idx)%nlgs, iusex)
                  ij2 = check_iv(iox, i1, j2, xregion(idx)%nlgs, iusex)
                  ij3 = check_iv(iox, i2, j1, xregion(idx)%nlgs, iusex)
                  ij4 = check_iv(iox, i2, j2, xregion(idx)%nlgs, iusex)
                  wt1 = wti1*wtj1
                  wt2 = wti1*wtj2
                  wt3 = wti2*wtj1
                  wt4 = wti2*wtj2
                  swt = wt1 + wt2 + wt3 + wt4
                  Do igf = 1, ngs
                     xiny(ij,1:nu,igf) = (x(ij1,1:nu,igf)*wt1 + x(ij2,1:nu,igf)*wt2 + &
                                          x(ij3,1:nu,igf)*wt3 + x(ij4,1:nu,igf)*wt4)/swt
                  End Do
               End Do
               If (idx < ndx) iox = iox + nvx(idx)
            End Do
         End Select
      End Select
!
! Copy stations / indices
    Case Default
      ij = 1
      Do idy = 1, ndy
         iox = 0
         Do idx = 1, ndx
            If (idx > 1) iox = iox + nvx(idx-1)
            If (Present(imfs)) Then
               If (idx /= imfs(idy)) Cycle
               If (idy == 1) ij = 1
            Else
               If (idx == 1) ij = 1
            End If
            Do i = 1, nvay(idy)
               If (i <= nvx(idx)) Then
                  ij1 = check_iv(iox, 1, i, nvx(idx), iusex)
                  If (ij1 > 0) Then
                     xiny(ij,1:nu,:) = x(ij1,1:nu,:)
                  Else
                     xiny(ij,1:nu,:) = rmiss(idx)
                  End If
               Else
                  xiny(ij,1:nu,:) = rmiss(idx)
               End If
               ij = ij + 1
            End Do
         End Do
      End Do
   End Select
!
   Return
!
 Contains
!
!
  Function check_iv(ioff, i, j, nlg, iuse) &
           Result (check)
!
! Identifies used variable
!
! Function result
   Integer :: check
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ioff ! - domain offset -
   Integer, Intent(In) :: i    ! - latitude index -
   Integer, Intent(In) :: j    ! - longitude index -
   Integer, Intent(In) :: nlg  ! - number of longitudes -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - indices of used variables -
!
! Locals
!
! Local scalars
   Integer :: ij ! - location index -
   Integer :: iv ! - variable index -
!
! Functions and Subroutines
!
! Executable Statements
!
! Identify used variable
   check = 0
   iv = ioff + (i - 1)*nlg + j
   Do ij = 1, iv
      If (iuse(ij) == iv) Then
         check = ij
         Exit
      Else If (iuse(ij) > iv) Then
         Exit
      End If
   End Do
!
   Return
  End Function check_iv
 End Subroutine get_interpolated
!
!
!
#if GUI == 1
 Function which_grid(xsp, ysp, id, nlt, nv, region)
!
! Identifies grid for current point
!
! Modules
   Use data_numbers, Only: half, three, r360
!
! Function result
   Integer :: which_grid
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: id  ! - domain index -
   Integer, Intent(In) :: nlt ! - number of latitudes -
!
   Real(Kind=rp), Intent(In) :: xsp ! - longitude of selected point -
   Real(Kind=rp), Intent(In) :: ysp ! - latitude of selected point -
!
   Type(domain), Intent(In) :: region ! - domain -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv ! - number of variables -
!
! Locals
!
! Local scalars
   Integer :: i  ! - latitude index -
   Integer :: j  ! - longitude index -
   Integer :: ii ! - index of selected latitude -
   Integer :: jj ! - index of selected longitude -
!
   Real(Kind=rp) :: rlt1 ! - south latitude of current grid -
   Real(Kind=rp) :: rlt2 ! - north latitude of current grid -
   Real(Kind=rp) :: rlg1 ! - west longitude of current grid -
   Real(Kind=rp) :: rlg2 ! - east longitude of current grid -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Get grid of selected point
   which_grid = 0
   ii = 0
   Do i = 1, region%nlts
      If (i > 1) Then
         rlt2 = (rlatdy(i,id) + rlatdy(i-1,id))*half
      Else
         rlt2 = (three*rlatdy(i,id) - rlatdy(i+1,id))*half
      End If
      If (i < nlt) Then
         rlt1 = (rlatdy(i,id) + rlatdy(i+1,id))*half
      Else
         rlt1 = (three*rlatdy(i,id) - rlatdy(i-1,id))*half
      End If
      If (ysp > rlt1 .and. ysp <= rlt2) Then
         ii = i
         Exit
      End If
   End Do
   If (ii == 0) Return
! - identify longitude -
   jj = 0
   Do j = 1, region%nlgs
      If (j > 1) Then
         rlg1 = (rlngdy(j,id) + rlngdy(j-1,id))*half
      Else
         rlg1 = rlngdy(1,id) - (rlngdy(2,id) - rlngy(1,id))*half
      End If
      If (j < region%nlgs) Then
         rlg2 = (rlngdy(j,id) + rlngdy(j+1,id))*half
      Else If (region%nlgs > 1) Then
         rlg2 = rlngdy(region%nlgs,id) + (rlngdy(region%nlgs,id) - &
                rlngdy(region%nlgs-1,id))*half
      Else
         rlg2 = 2*rlngdy(1,id) - rlg1
      End If
      If (rlg2 < rlg1) rlg2 = rlg2 + r360
      If ((xsp > rlg1 .and. xsp <= rlg2) .or.  &
          (xsp-r360 > rlg1 .and. xsp-r360 <= rlg2) .or.  &
          (xsp+r360 > rlg1 .and. xsp+r360 <= rlg2)) Then
         jj=j
         Exit
      End If
   End Do
   If (jj == 0) Return
!
! Determine grid
   which_grid = (ii - 1)*region%nlgs + jj
   If (id > 1) which_grid = which_grid + Sum(nv(1:id-1))
!
   Return
 End Function which_grid
!
!
!
 Function which_station(xsp, ysp, id, nv, nva, region)
!
! Identifies station at current point
!
! Modules
   Use data_numbers, Only: zero, oneh, r360
   Use maths,        Only: approx
   Use arrays,       Only: iusey
!
! Function result
   Integer :: which_station
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: id ! - domain index -
!
   Real(Kind=rp), Intent(In) :: xsp ! - longitude of selected point -
   Real(Kind=rp), Intent(In) :: ysp ! - latitude of selected point -
!
   Type(domain), Intent(In) :: region ! - domain -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv  ! - number of variables -
   Integer, Dimension(:), Intent(In) :: nva ! - number of available variables -
!
! Locals
!
! Local scalars
   Integer :: i  ! - station index -
   Integer :: ij ! - used station index -
   Integer :: iy ! - available station index -
   Integer :: i0 ! - offset -
   Integer :: j0 ! - offset -
!
   Real(Kind=rp) :: rlg1 ! - slightly west longitude of current point -
   Real(Kind=rp) :: rlg2 ! - slightly east longitude of current point -
   Real(Kind=rp) :: dlng ! - longitudinal extent of domain -
   Real(Kind=rp) :: tol  ! - tolerance distance for near miss -
   Real(Kind=rp) :: radj ! - longitude adjustment -
   Real(Kind=rp) :: d    ! - distance from point to station -
   Real(Kind=rp) :: dmin ! - distance from point to nearest station -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Sum
!
! Executable Statements
!
! Set tolerance distance for near miss
   which_station = 0
   dlng = region%alim%rlge - region%alim%rlgw
   If (dlng < zero) dlng = dlng - r360
   tol = Min(region%alim%rltn-region%alim%rlts, dlng)/oneh
!
! Get station at selected point
   If (id == 1) Then
      i0 = 0
      j0 = 0
   Else
      i0 = Sum(nva(1:id-1))
      j0 = Sum(nv(1:id-1))
   End If
   iy = 0
   Do i = 1, nva(id)
      ij = iusey(i+i0)-j0
      rlg1 = rlngdy(ij,id) - tol
      rlg2 = rlngdy(ij,id) + tol
      If (approx(ysp, rlatdy(ij,id), utol=tol)) Then
         If (approx(xsp, rlngdy(ij,id), utol=tol)) Then
            radj = 0
         Else If (xsp > rlg1-r360 .and. xsp < rlg2-r360) Then
            radj = -r360
         Else If (xsp > rlg1+r360 .and. xsp < rlg2+r360) Then
            radj = r360
         Else
            Cycle
         End If
         If (iy == 0) Then
            iy = i
            dmin = gcd(ysp, xsp, rlatdy(ij,id)+radj, rlngdy(ij,id)+radj)
         Else ! - find nearest station if two stations are near the selected point -
            d = gcd(ysp, xsp, rlatdy(ij,id)+radj, rlngdy(ij,id)+radj)
            If (d < dmin) Then
               iy = i           
               dmin = d
            End If
         End If
      End If
   End Do
   If (iy == 0) Return
!
! Determine station
   If (id > 1) iy = iy + Sum(nva(1:id-1))
   which_station = iusey(iy)
!
   Return
 End Function which_station
!
!
!
 Function which_index(xsp, id, nv)
!
! Identifies index at current point
!
! Function result
   Integer :: which_index
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: id ! - domain index -
!
   Real(Kind=rp), Intent(In) :: xsp ! - longitude of selected point -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv ! - number of variables -
!
! Locals
!
! Local scalars
   Integer :: iy ! - current used index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Sum
!
! Executable Statements
!
! Get index at selected point
   iy = Ceiling(xsp)
   If (iy < 1 .or. iy > nv(id)) Then
      which_index = 0
      Return
   End If
!
! Determine index
   If (id > 1) iy = iy + Sum(nv(1:id-1))
   which_index = iy
!
   Return
 End Function which_index
!
!
!
#endif
 Function gcd(dlt1, dlg1, dlt2, dlg2)
!
! Calculates great circle distance (m) between 2 points
!
! Modules
   Use data_numbers, Only: zero, pi, r180, re
!
! Function result
   Real(Kind=rp) :: gcd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: dlt1 ! - latitude of first point (in degrees) -
   Real(Kind=rp), Intent(In) :: dlg1 ! - longitude of first point (in degrees) -
   Real(Kind=rp), Intent(In) :: dlt2 ! - latitude of second point (in degrees) -
   Real(Kind=rp), Intent(In) :: dlg2 ! - longitude of second point (in degrees) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: rlt1 ! - latitude of first point (in radians) -
   Real(Kind=rp) :: rlt2 ! - latitude of second point (in radians) -
   Real(Kind=rp) :: rlgd ! - longitude difference (radians) -
   Real(Kind=rp) :: rnum ! - numerator of Atan2 -
   Real(Kind=rp) :: rden ! - denominator of Atan2 -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Atan2, Cos, Sin, Sqrt
!
! Executable Statements
!
! Check for identical locations
   If (dlt1 == dlt2 .and. dlg1 == dlg2) Then
      gcd = zero
      Return
   End If
!
! Convert latitudes to radians
   rlt1 = dlt1*pi/r180
   rlt2 = dlt2*pi/r180
!
! Calculate longitude difference in radians
   rlgd = (dlg1 - dlg2)*pi/r180
!
! Calculate distance
   rnum = Sqrt((Cos(rlt2)*Sin(rlgd))**2 + (Cos(rlt1)*Sin(rlt2) - Sin(rlt1)*Cos(rlt2)*Cos(rlgd))**2)
   rden = Sin(rlt1)*Sin(rlt2) + Cos(rlt1)*Cos(rlt2)*Cos(rlgd)
   gcd = Atan2(rnum, rden)
   gcd = gcd*re
!
   Return
 End Function gcd
!
!
!
 Subroutine proj_read_space (iin, lsetx, idstrx, igeogx, nflx, nltx, nlgx, lsety, idstry, igeogy, nfly, nlty, nlgy, ifail)
!
! Reads space settings from project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: idstrx ! - X data structure flag -
   Integer, Intent(In) :: igeogx ! - X geographical reference flag -
   Integer, Intent(In) :: nflx   ! - number of X fields -
   Integer, Intent(In) :: idstry ! - Y data structure flag -
   Integer, Intent(In) :: igeogy ! - Y geographical reference flag -
   Integer, Intent(In) :: nfly   ! - number of Y fields -
!
   Logical, Intent(In) :: lsetx ! - X file set? -
   Logical, Intent(In) :: lsety ! - Y file set? -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nltx ! - number of X latitudes -
   Integer, Dimension(:), Intent(In) :: nlgx ! - number of X longitudes -
   Integer, Dimension(:), Intent(In) :: nlty ! - number of Y latitudes -
   Integer, Dimension(:), Intent(In) :: nlgy ! - number of Y longitudes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read domain settings
   ifail = 0
   If (lsetx) Call proj_read_aspace (iin, idstrx, igeogx, nflx, nltx(:), nlgx(:), &
                  rlatx, rlngx, rlatdx, rlngdx, cstnx, cstndx, idomx, ifail)
   If (ifail /= 0) Return
   If (lsety) Call proj_read_aspace (iin, idstry, igeogy, nfly, nlty(:), nlgy(:), &
                  rlaty, rlngy, rlatdy, rlngdy, cstny, cstndy, idomy, ifail)
   If (ifail /= 0) Return
!
   Return
!
 Contains
!
!
  Subroutine proj_read_aspace (iin, idstr, igeog, nfl, nlt, nlg, rlat, rlng, rlatd, rlngd, cstn, cstnd, idom, ifail)
!
! Reads space settings from project file
!
! Modules
   Use data_cpt_constants, Only: id_grd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input file unit number -
   Integer, Intent(In) :: idstr ! - data structure flag -
   Integer, Intent(In) :: igeog ! - geographical reference flag -
   Integer, Intent(In) :: nfl   ! - number of fields -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nlt ! - number of latitudes -
   Integer, Dimension(:), Intent(In) :: nlg ! - number of longitudes -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - longitudes within domain -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - stations within domain -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read latitudes and longitudes
   Call reset_grids (idstr, nfl, nlt(:), rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, ifail, &
        nlg=nlg(:))
   If (ifail /= 0) GoTo 3
   If (igeog == 1) Then
      Read (Unit=iin, Err=1, End=2) rlat(:,:)
      Read (Unit=iin, Err=1, End=2) rlatd(:,:)
      Read (Unit=iin, Err=1, End=2) rlng(:,:)
      Read (Unit=iin, Err=1, End=2) rlngd(:,:)
   End If
!
! Read index / station names
   If (idstr /= id_grd) Then
      Read (Unit=iin, Err=1, End=2) cstn(:,:)
      Read (Unit=iin, Err=1, End=2) cstnd(:,:)
   End If
!
! Read domain variables
   Read (Unit=iin, Err=1, End=2) idom(:,:)
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
  End Subroutine proj_read_aspace
 End Subroutine proj_read_space
!
!
!
 Subroutine proj_write_space (iout, lsetx, idstrx, igeogx, lsety, idstry, igeogy, ifail)
!
! Writes space settings to project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout   ! - output file unit number -
   Integer, Intent(In) :: idstrx ! - X data structure flag -
   Integer, Intent(In) :: igeogx ! - X geographical reference flag -
   Integer, Intent(In) :: idstry ! - Y data structure flag -
   Integer, Intent(In) :: igeogy ! - Y geographical reference flag -
!
   Logical, Intent(In) :: lsetx ! - X file set? -
   Logical, Intent(In) :: lsety ! - Y file set? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write domain settings
   ifail = 0
   If (lsetx) Call proj_write_aspace (iout, idstrx, igeogx, rlatx, rlngx, rlatdx, rlngdx, cstnx, cstndx, idomx, ifail)
   If (ifail /= 0) Return
   If (lsety) Call proj_write_aspace (iout, idstry, igeogy, rlaty, rlngy, rlatdy, rlngdy, cstny, cstndy, idomy, ifail)
!
   Return
!
 Contains
!
!
  Subroutine proj_write_aspace (iout, idstr, igeog, rlat, rlng, rlatd, rlngd, cstn, cstnd, idom, ifail)
!
! Writes space settings to project file
!
! Modules
   Use data_cpt_constants, Only: id_grd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output file unit number -
   Integer, Intent(In) :: idstr ! - data structure flag -
   Integer, Intent(In) :: igeog ! - geographical reference flag -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - variables within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat  ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng  ! - longitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - longitudes within domain -
!
   Character(Len=lstn), Dimension(:,:), Pointer :: cstn  ! - stations -
   Character(Len=lstn), Dimension(:,:), Pointer :: cstnd ! - stations within domain -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write latitudes and longitudes

   If (igeog == 1) Then
      Write (Unit=iout, Err=1) rlat(:,:)
      Write (Unit=iout, Err=1) rlatd(:,:)
      Write (Unit=iout, Err=1) rlng(:,:)
      Write (Unit=iout, Err=1) rlngd(:,:)
   End If
!
! Write index / station names
   If (idstr /= id_grd) Then
      Write (Unit=iout, Err=1) cstn(:,:)
      Write (Unit=iout, Err=1) cstnd(:,:)
   End If
!
! Write domain variables
   Write (Unit=iout, Err=1) idom(:,:)
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
  End Subroutine proj_write_aspace
 End Subroutine proj_write_space
!
!
!
 Subroutine proj_read_interp (iin, ifail)
!
! Reads interpolation settings from project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
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
! Write latitudes and longitudes
   If (Allocated(rltnr)) Then
      Read (Unit=iin, Err=1, End=2) rltnr(:,:)
      Read (Unit=iin, Err=1, End=2) rlgnr(:,:)
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
 End Subroutine proj_read_interp
!
!
!
 Subroutine proj_write_interp (iout, ifail)
!
! Writes interpolation settings to project file
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
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Write latitudes and longitudes
   If (Allocated(rltnr)) Then
      Write (Unit=iout, Err=1) rltnr(:,:)
      Write (Unit=iout, Err=1) rlgnr(:,:)
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
 End Subroutine proj_write_interp
!
!
!
 Function close_space() &
          Result (fclose)
!
! Frees memory allocated for space
!
! Function result
   Integer :: fclose
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free memory allocated for space
   If (Allocated(rltnr)) Deallocate (rltnr)
   If (Allocated(rlgnr)) Deallocate (rlgnr)
   fclose = 0
!
   Return
 End Function close_space
End Module space
