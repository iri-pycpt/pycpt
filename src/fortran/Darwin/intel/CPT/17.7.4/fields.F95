! Author: Simon Mason
Module fields
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ng
   Use data_io_constants,  Only: lstr, lvar
   Use time,               Only: cpssn, pdate, pprd, pssn
   Use space,              Only: area, domain, level
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: check_ivf, first_field, get_area, get_domains, get_grid_coors, edit_domain_x, edit_domain_y, init_field, &
             match_fields, proj_read_fields, proj_write_fields, reset_fields, set_field, set_ilims, set_mfields,      &
             set_mfields_region, update_grid
#if GUI == 1
   Public :: prompt_fields, prompt_location, set_iv
#else
   Public :: print_field
#endif
!
! Derived type definitions
!
! - field -
   Public :: field
   Type field
      Sequence
      Integer :: nlt               ! - total number of latitudes -
      Integer :: nlg               ! - total number of longitudes -
      Integer :: nv                ! - total number of variables -
      Integer :: nva               ! - total number of used variables -
      Integer :: member            ! - ensemble member -
      Integer :: icat(ng)          ! - categories -
!
      Real(Kind=rp) :: prob(ng)    ! - climatological probabilities -
      Real(Kind=rp) :: rmiss       ! - missing value -
!
      Character(Len=lvar) :: var   ! - field variable -
      Character(Len=lvar) :: unit  ! - field units -
      Character(Len=lvar) :: unitc ! - converted field units -
      Character(Len=lvar) :: model ! - model -
!
      Logical :: ln2s              ! - north to south latitudes? -
!
      Type( level) :: z            ! - atmospheric level -
      Type(domain) :: region       ! - domain of interest -
      Type( pdate) :: mdate        ! - date made ('start date' for model forecasts in Data Library) -
      Type(  pssn) :: tssn         ! - target season -
      Type( cpssn) :: cssn         ! - field season -
   End Type field
!
! - old field -
   Type old_field
      Sequence
      Integer :: nlt               ! - total number of latitudes -
      Integer :: nlg               ! - total number of longitudes -
      Integer :: nv                ! - total number of variables -
      Integer :: nva               ! - total number of used variables -
      Integer :: member            ! - ensemble member -
      Integer :: icat(ng)          ! - categories -
!
      Real(Kind=rp) :: prob(ng)    ! - climatological probabilities -
      Real(Kind=rp) :: rmiss       ! - missing value -
!
      Character(Len=lvar) :: var   ! - field variable -
      Character(Len=lvar) :: unit  ! - field units -
      Character(Len=lvar) :: unitc ! - converted field units -
      Character(Len=  21) :: cssn  ! - field season -
      Character(Len=lvar) :: model ! - model -
!
      Logical :: ln2s              ! - north to south latitudes? -
!
      Type( level) :: z            ! - atmospheric level -
      Type(  pprd) :: tprd         ! - target period -
      Type( pdate) :: mdate        ! - date made ('start date' for model forecasts in Data Library) -
      Type(domain) :: region       ! - domain of interest -
   End Type old_field
!
! Scalars
!
! Integer scalars
   Integer, Public :: ifdx     ! - current X field -
   Integer, Public :: ifdy     ! - current Y field -
   Integer, Public :: ilfx     ! - current X lagged field -
   Integer, Public :: ilfy     ! - current Y lagged field -
   Integer, Public :: iffx     ! - current X field / lagged field -
   Integer, Public :: iffy     ! - current Y field / lagged field -
   Integer, Public :: ilimx0   ! - 0th X index -
   Integer, Public :: ilimx1   ! - first available X index for current field -
   Integer, Public :: ilimx2   ! - last available X index for current field -
   Integer, Public :: ilimy0   ! - 0th Y index -
   Integer, Public :: ilimy1   ! - first available Y index for current field -
   Integer, Public :: ilimy2   ! - last available Y index for current field -
   Integer, Public :: nmfd = 1 ! - number of mfields -
!
! Character scalars
   Character(Len=lstr), Public :: dsdy    ! - Y data structure item -
   Character(Len=  30), Public :: y_coors ! - Y-grid coordinates -
!
! Logical scalars
   Logical, Public :: larea = .true. ! - prompt for area? -
!
! Arrays
!
! Integer arrays
   Integer, Dimension(:), Allocatable, Public :: imfs ! - matched X fields  -
!
! Derived type arrays
   Type(field), Dimension(:), Pointer, Public :: mfield => Null() ! - monthly Y fields -
   Type(field), Dimension(:), Pointer, Public :: sfield => Null() ! - temporary fields -
   Type(field), Dimension(:), Pointer, Public :: tfield => Null() ! - temporary fields -
   Type(field), Dimension(:), Pointer, Public :: xfield => Null() ! - X fields -
   Type(field), Dimension(:), Pointer, Public :: yfield => Null() ! - Y fields -
   Type(field), Dimension(:), Pointer, Public :: zfield => Null() ! - Z fields -
!
! Interfaces
!
! Assignment Operators
   Public :: Assignment(=)
   Interface Assignment(=)
      Module Procedure old2new_field
   End Interface
!
! Interface Operators
   Public :: Operator(/=)
   Interface Operator(/=)
      Module Procedure fields_differ
   End Interface
!
   Public :: Operator(==)
   Interface Operator(==)
      Module Procedure identical_fields
   End Interface
!
Contains
!
!
 Function fields_differ(afield, bfield) &
          Result (differ)
!
! Do fields differ?
!
! Modules
   Use data_cpt_constants, Only: ncps
!
! Function result
   Logical :: differ
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(field), Intent(In) :: afield ! - first field -
   Type(field), Intent(In) :: bfield ! - second field -
!
! Locals
!
! Local scalars
   Integer :: i ! - persisted seasonal component index -
!
! Executable Statements
!
! Compare field settings
   differ = .true.
   If (afield%nlt /= bfield%nlt) Return
   If (afield%nlg /= bfield%nlg) Return
   If (afield%nv /= bfield%nv) Return
   If (afield%var /= bfield%var) Return
   If (afield%unit /= bfield%unit) Return
   If (afield%unitc /= bfield%unitc) Return
   If (afield%model /= bfield%model) Return
   Do i = 0, ncps
      If (afield%cssn%cssn(i) /= bfield%cssn%cssn(i)) Return
   End Do
   differ = .false.
!
   Return
 End Function fields_differ
!
!
!
 Function identical_fields(afield, bfield) &
          Result (identical)
!
! Do fields differ?
!
! Modules
   Use data_cpt_constants, Only: ncps
!
! Modules
   Use time,  Only: Operator(/=)
   Use space, Only: Operator(/=)
!
! Function result
   Logical :: identical
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(field), Intent(In) :: afield ! - first field -
   Type(field), Intent(In) :: bfield ! - second field -
!
! Locals
!
! Local scalars
   Integer :: i ! - persisted seasonal component index -
!
! Executable Statements
!
! Compare field settings
   identical = .false.
   If (afield%nlt /= bfield%nlt) Return
   If (afield%nlg /= bfield%nlg) Return
   If (afield%nv /= bfield%nv) Return
   If (afield%var /= bfield%var) Return
   If (afield%unit /= bfield%unit) Return
   If (afield%unitc /= bfield%unitc) Return
   If (afield%model /= bfield%model) Return
   If (afield%z%hght /= bfield%z%hght) Return
   If (afield%region /= bfield%region) Return
   Do i = 0, ncps
      If (afield%tssn%tprd(i) /= bfield%tssn%tprd(i)) Return
   End Do
   Do i = 0, ncps
      If (afield%cssn%cssn(i) /= bfield%cssn%cssn(i)) Return
   End Do
   identical = .true.
!
   Return
 End Function identical_fields
!
!
!
 Subroutine old2new_field (afield, ofield)
!
! Converts an old field to a new field
!
! Modules
   Use time, Only: Assignment(=)
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Type(old_field), Intent(In) :: ofield ! - old field -
!
! - output scalars -
   Type(field), Intent(Out) :: afield ! - new field -
!
! Executable Statements
!
! Set field settings
   afield%nlt = ofield%nlt
   afield%nlg = ofield%nlg
   afield%nv = ofield%nv
   afield%nva = ofield%nva
   afield%member = ofield%member
   afield%icat(:) = ofield%icat(:)
!
   afield%prob(:) = ofield%prob(:)
   afield%rmiss = ofield%rmiss
!
   afield%var = ofield%var
   afield%unit = ofield%unit
   afield%unitc = ofield%unitc
   afield%model = ofield%model
!
   afield%ln2s = ofield%ln2s
!
   afield%z%hght = ofield%z%hght
   afield%z%unit = ofield%z%unit
!
! Set initial domain limits
   afield%region%alim = ofield%region%alim
   afield%region%nlt1 = ofield%region%nlt1
   afield%region%nlt2 = ofield%region%nlt2
   afield%region%nlts = ofield%region%nlts
   afield%region%nlg1 = ofield%region%nlg1
   afield%region%nlg2 = ofield%region%nlg2
   afield%region%nlgs = ofield%region%nlgs
!
! Set dates
   afield%mdate = ofield%mdate
   afield%tssn%tprd(0) = 0
   afield%tssn%tprd(1:) = ofield%tprd
   afield%cssn%cssn(0) = ' '
   afield%cssn%cssn(1:) = ofield%cssn
!
   Return
 End Subroutine old2new_field
!
!
!
 Subroutine reset_fields (xmiss, ymiss, ifail)
!
! Reset X, Y, Z and M fields
!
! Modules
   Use errors, Only: cpt_error
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: xmiss ! - X missing values -
   Real(Kind=rp), Intent(In) :: ymiss ! - Y missing values -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! -error indicator -
!
! Executable Statements
!
! Reset X fields
   Call init_field (0, [xmiss], xfield, ifail)
   If (ifail /= 0) GoTo 1
   ifdx = 1
   ilfx = 1
   iffx = 1
!
! Reset Y fields
   Call init_field (0, [ymiss], yfield, ifail)
   If (ifail /= 0) GoTo 1
   ifdy = 1
   ilfy = 1
   iffy = 1
   Call init_field (0, [ymiss], mfield, ifail)
   If (ifail /= 0) GoTo 1
!
! Reset forecast fields
   Call init_field (0, [xmiss], zfield, ifail)
   If (ifail /= 0) GoTo 1
   Return
!
! Errors
1  Call cpt_error ('reset_fields', .false., ifail)
!
   Return
 End Subroutine reset_fields
!
!
!
 Subroutine init_field (nfl, rmiss, afield, ifail)
!
! Initialises memory for a field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfl ! - number of fields * lagged-fields -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: rmiss ! - missing values flags -
!
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: afield ! - input field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: l   ! - field index -
   Integer :: mfl ! - maximum number of fields -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Associated, Max, Size
!
! Executable Statements
!
! Initialise field
   If (Associated(afield)) Then
      Deallocate (afield)
      Nullify (afield)
   End If
   mfl = Max(1, nfl)
   Allocate (afield(mfl), Stat=ifail)
   If (ifail /= 0) Then
      ifail = 1
      Return
   End If
!
! Set field settings
   If (Size(rmiss) == Size(afield)) Then
      Do l = 1, mfl
         Call set_field (rmiss(l), afield(l))
      End Do
   Else
      Do l = 1, mfl
         Call set_field (rmiss(1), afield(l))
      End Do
   End If
   ifail = 0
!
   Return
 End Subroutine init_field
!
!
!
 Subroutine set_field (rmiss, afield)
!
! Initialises field settings
!
! Modules
   Use data_numbers, Only: zero
   Use labels,       Only: cg_na, cg_none_l, cg_undefined_l
   Use time,         Only: Assignment(=)
   Use space,        Only: xarea
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values flags -
!
! - output arrays -
   Type(field), Intent(Out) :: afield ! - field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set field settings
   afield%nlt = 2
   afield%nlg = 2
   afield%nv = 0
   afield%nva = 0
   afield%member = 0
   afield%icat(:) = 0
!
   afield%prob(:) = zero
   afield%rmiss = rmiss
!
   afield%var = Trim(cg_undefined_l)
   afield%unit = Trim(cg_none_l)
   afield%unitc = Trim(cg_none_l)
   afield%model = Trim(cg_na)
!
   afield%ln2s = .true.
!
   afield%z%hght = zero
   afield%z%unit = Trim(cg_none_l)
!
! Set initial domain limits
   afield%region%alim = xarea
   afield%region%nlt1 = 1
   afield%region%nlt2 = 2
   afield%region%nlts = 2
   afield%region%nlg1 = 1
   afield%region%nlg2 = 2
   afield%region%nlgs = 2
!
! Set dates
   afield%mdate = 0
   afield%tssn%tprd(:) = 0
   afield%cssn%cssn(:) = Trim(cg_na)
!
   Return
 End Subroutine set_field
!
!
!
  Subroutine set_mfields (nse)
!
! Copies Y-field settings for persistence component and for monthly fields
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nse ! - number of sub-seasons -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Kind, Size
!
! Executable Statements
!
! Copy Y-field settings
   If (Size(yfield) == Size(mfield)) Then
      Call set_mfields_region ()
  Else
      If (nse > 1) Call init_field (Size(yfield, Kind=Kind(0)), yfield(:)%rmiss, mfield, ifail)
      mfield(:)%region = yfield(1)%region
      mfield(:)%nlt = yfield(1)%nlt
      mfield(:)%nlg = yfield(1)%nlg
      mfield(:)%nv = yfield(1)%nv
      mfield(:)%nva = yfield(1)%nva
   End If
!
   Return
 End Subroutine set_mfields
!
!
!
 Subroutine set_mfields_region ()
!
! Copies Y-field settings into mfields
!
! Locals
!
! Local scalars
   Integer :: m ! - mfield dimension -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
!
! Executable Statements
!
! Copy Y-field settings
   m = Size(mfield)
   mfield(:)%region = yfield(1:m)%region
   mfield(:)%nlt = yfield(1:m)%nlt
   mfield(:)%nlg = yfield(1:m)%nlg
   mfield(:)%nv = yfield(1:m)%nv
   mfield(:)%nva = yfield(1:m)%nva
!
   Return
 End Subroutine set_mfields_region
!
!
!
 Subroutine match_fields (ixdstr, nflx, nfly, lmssn, imfs, ierr, ifail)
!
! Matches X and Y fields
! On exit:
!    ifail =  0 Successful
!    ifail =  1 No matching X field for the ierr-th Y field
!    ifail =  2 Non-matching station names in the ierr-th Y field 
!    ifail =  3 Non-matching station latitudes in the ierr-th Y field 
!    ifail =  4 Non-matching station longitudes in the ierr-th Y field 
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn
   Use labels,             Only: cg_na, cg_undefined_l
   Use space,              Only: cstndx, cstndy, rlatdx, rlatdy, rlngdx, rlngdy
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ixdstr ! - X data structure -
   Integer, Intent(In) :: nflx   ! - number of X fields -
   Integer, Intent(In) :: nfly   ! - number of Y fields -
!
   Logical, Intent(In) :: lmssn ! - match seasons? -
!
! - output scalars -
   Integer, Intent(Out) :: ierr  ! - unmatched Y field -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Integer, Dimension(:), Intent(Out) :: imfs ! - matched X fields  -
!
! Local scalars
   Integer :: i   ! - location index -
   Integer :: ifx ! - X field index -
   Integer :: ify ! - Y field index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Check for matching fields
   ifail = 0
   Do ify = 1, nfly
      imfs(ify) = 0
      Do ifx = 1, nflx
         If ((xfield(ifx)%cssn%cssn(1) == yfield(ify)%cssn%cssn(1) .or. &
              Trim(yfield(ify)%cssn%cssn(1)) == cg_na .or. Trim(yfield(ify)%cssn%cssn(1)) == 'N/A' .or. .not.lmssn) .and.  &
             (xfield(ifx)%var == yfield(ify)%var .or. &
              Trim(xfield(ifx)%var) == Trim(cg_undefined_l) .or. Trim(xfield(ifx)%var) == 'undefined' .or. &
              Trim(yfield(ify)%var) == Trim(cg_undefined_l) .or. Trim(yfield(ify)%var) == 'undefined')) Then
            imfs(ify) = ifx
! - check for matching stations / indices -
            If (ixdstr /= id_grd) Then
               Do i = 1, xfield(ifx)%nva
                  If (Any(cstndy(:,ify) == cstndx(i,ifx))) Then
                     Cycle
                  Else
                     ierr = ify
                     ifail = 2
                     Return
                  End If
               End Do
               If (ixdstr == id_stn) Then
                  Do i = 1, xfield(ifx)%nva
                     If (Any(rlatdy(:,ify) == rlatdx(i,ify))) Then
                        Continue
                     Else
                        ierr = ify
                        ifail = 3
                        Return
                     End If
                     If (Any(rlngdy(:,ify) == rlngdx(i,ifx))) Then
                        Continue
                     Else
                        ierr = ify
                        ifail = 4
                        Return
                     End If
                  End Do
               End If
            End If
            Exit
         End If
      End Do
      If (imfs(ify) == 0) Then
         ierr = ify
         ifail = 1
         Return
      End If
   End Do
   ierr = 0
!
   Return
 End Subroutine match_fields
!
! 
!
 Subroutine update_grid (iout)
!
! Updates information for current location
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use settings,           Only: ivf
   Use space,              Only: cstnc, cstndy
   Use iofiles,            Only: yfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Construct and add coordinate label for initial point
   Write (Unit=iout, Fmt=*)
   Write (Unit=iout, Fmt='(1X,2A)', Advance='no') Trim(dsdy), ': '
   Select Case (yfile%idstr)
    Case (id_grd)
      y_coors = get_grid_coors()
      Write (Unit=iout, Fmt='(A)') y_coors
    Case (id_stn)
      y_coors = get_stn_coors()
      cstnc = cstndy(ivf,iffy)
      Write (Unit=iout, Fmt='(3A)') Trim(cstnc), '  ',y_coors
    Case (id_ind)
      cstnc = cstndy(ivf,iffy)
      Write (Unit=iout, Fmt='(A)') cstnc
   End Select
!
   Return
 End Subroutine update_grid
!
!
!
 Subroutine get_domains (cxy, rlat, rlng, cstn, afile, afield, rlatd, rlngd, cstnd, idom, nv, ifail)
!
! Prompts for domains
!
! Error indicators:
!    ifail =  0 Successful
!    ifail =  1 Cancelled
!    ifail =  2 Invalid file structure
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use time,               Only: lssq, nss
   Use gui,                Only: set_cursor_waiting
   Use space,              Only: full_domain
   Use iofiles,            Only: ifile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=1), Intent(In) :: cxy ! - X or Y variables flag -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - stations within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng ! - longitudes -
!
   Character(Len=*), Dimension(:,:), Pointer :: cstn ! - station labels -
!
! - input/output scalars -
   Type(ifile), Intent(In) :: afile ! - input file -
!
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: afield ! - field -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - longitudes within domain -
!
   Character(Len=*), Dimension(:,:), Pointer :: cstnd ! - station labels within domain -
!
! - output scalars -
   Integer, Intent(Out) :: nv    ! - total number of variables -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: l ! - field index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal, Sum
!
! Executable Statements
!
! Prompt for domain if relevant
   Select Case (afile%idstr)
! - gridded and station data -
    Case (id_grd, id_stn)
      Call set_cursor_waiting (0)
      tfield => afield(:)
      Call get_area (cxy, afile%idstr, afile%nfs, Max(afile%nls, nss), afile%nms, afile%nem, lssq, tfield, rlat, rlng, cstn, &
           rlatd, rlngd, cstnd, idom, nv, ifail)
      Nullify (tfield)
      Call set_cursor_waiting (1)
      If (ifail /= 0) Return
! - unreferenced data -
    Case (id_ind)
      If (.not.lssq) Then
         nv = Sum(afield(:)%nlt)
      Else
         nv = MaxVal(afield(:)%nlt)
      End If
      afield(:)%nv = afield(:)%nlt
      cstnd(:,:) = cstn(:,:)
      Do l = 1, afile%nfl
         Call full_domain (afield(l)%nv, afield(l)%nlt, afield(l)%nlg, afield(l)%region, idom(:,l))
      End Do
      ifail = 0
! - cancel -
    Case Default
      ifail = 2
   End Select
!
   Return
 End Subroutine get_domains
!
!
!
 Function edit_domain_x() &
          Result (edit_domain)
!
! Prompts for X domain settings
!
! Modules
   Use time,    Only: nss
   Use screen,  Only: window_update
   Use menus,   Only: record_change
   Use space,   Only: Operator(/=), &
                      idomx, rlatx, rlngx, cstnx, rlatdx, rlngdx, cstndx
   Use iofiles, Only: nx, nz, xfile, zfile
!
! Function result
   Integer :: edit_domain
!
! Locals
!
! Local scalars
   Integer :: l     ! - current field / lagged field -
   Integer :: nfd   ! - total number of fields and lagged fields -
   Integer :: ifail ! - error indicator -
!
! Local arrays
   Type(field), Dimension(:), Pointer :: tmfield ! - temporary copy of field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
!
! Executable Statements
!
! Backup area settings
   edit_domain = 3
   Nullify (tmfield)
   nfd = Size(xfield)
   Call init_field (nfd, xfield(:)%rmiss, tmfield, ifail)
   If (ifail /= 0) Return
   tmfield(:) = xfield(:)
!
! Prompt for domain
   Call get_area ('X', xfile%idstr, xfile%nfs, xfile%nls, xfile%nms, xfile%nem, (nss>1), tmfield, rlatx, rlngx, cstnx, &
        rlatdx, rlngdx, cstndx, idomx, nx, ifail)
   If (ifail == 0) Then
      Do l = 1, nfd
         If (xfield(l)%region%alim /= tmfield(l)%region%alim) ifail = record_change()
      End Do
      xfield(:) = tmfield(:)
      If (zfile%cfile%lset) Then
         zfield(:)%region = xfield(:)%region
         zfield(:)%nlt = xfield(:)%nlt
         zfield(:)%nlg = xfield(:)%nlg
         zfield(:)%nv = xfield(:)%nv
         zfield(:)%nva = xfield(:)%nva
         nz = nx
         Call window_update (nz)
      End If
      edit_domain = 2
   End If
!
   Return
 End Function edit_domain_x
!
!
!
 Function edit_domain_y() &
          Result (edit_domain)
!
! Prompts for Y domain settings
!
! Modules
   Use time,    Only: nss
   Use menus,   Only: record_change
   Use space,   Only: Operator(/=), &
                      idomy, rlaty, rlngy, cstny, rlatdy, rlngdy, cstndy
   Use iofiles, Only: ny, yfile
!
! Function result
   Integer :: edit_domain
!
! Locals
!
! Local scalars
   Integer :: l     ! - current field / lagged field -
   Integer :: nfd   ! - total number of fields and lagged fields -
   Integer :: ifail ! - error indicator -
!
! Local arrays
   Type(field), Dimension(:), Pointer :: tmfield ! - temporary copy of field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
!
! Executable Statements
!
! Backup area settings
   edit_domain = 3
   Nullify (tmfield)
   nfd = Size(yfield)
   Call init_field (nfd, yfield(1:)%rmiss, tmfield, ifail)
   If (ifail /= 0) Return
   tmfield(:) = yfield(:)
!
! Prompt for domain
   Call get_area ('Y', yfile%idstr, yfile%nfs, yfile%nls*nss, yfile%nms, yfile%nem, (nss>1), tmfield, rlaty, rlngy, cstny, &
        rlatdy, rlngdy, cstndy, idomy, ny, ifail)
   If (ifail == 0) Then
      Do l = 1, nfd
         If (tmfield(l)%region%alim /= yfield(l)%region%alim) ifail = record_change()
      End Do
      yfield(:) = tmfield(:)
! - set corresponding monthly field -
      Call set_mfields_region ()
      edit_domain = 2
   End If
!
   Return
 End Function edit_domain_y
!
!
!
 Subroutine get_area (cxy, idstr, nfs, nls, nms, nem, lssq, afield, rlat, rlng, cstn, rlatd, rlngd, cstnd, idom, nv, ifail)
!
! Prompts for area of interest and identifies domain indices
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: id_grd, id_stn
   Use labels,             Only: cg_domain, cg_domlims, cg_model, l_datdom
#if GUI == 1
   Use labels,             Only: l_applyall, l_applyalls, l_cancel, l_datlimits, l_next, l_redrawmap, l_sameprev
   Use screen,             Only: ihsize
#endif
   Use screen,             Only: window_update
#if GUI == 1
   Use gui,                Only: add_button, add_graphic_area, gui_creturn, leave_window
#endif
   Use gui,                Only: box_close, box_open, boxes_open, init_win, print_cvalue, print_text, set_cursor_waiting
   Use errors,             Only: cpt_error
#if GUI == 1
   Use space,              Only: lmds, &
                                 select_domain
#endif
   Use space,              Only: check_grid_domain, check_stn_domain, prompt_domain
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: idstr ! - field structure -
   Integer, Intent(In) :: nfs   ! - number of fields -
   Integer, Intent(In) :: nls   ! - number of lagged fields -
   Integer, Intent(In) :: nms   ! - number of models -
   Integer, Intent(In) :: nem   ! - number of ensemble members -
!
   Character(Len=1), Intent(In) :: cxy ! - X or Y variables flag -
!
   Logical, Intent(In) :: lssq ! - permit seasonal-sequencing? -
!
! - pointer arrays -
   Integer, Dimension(:,:), Pointer :: idom ! - stations within domain -
!
   Real(Kind=rp), Dimension(:,:), Pointer :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlng ! - longitudes -
!
   Character(Len=*), Dimension(:,:), Pointer :: cstn ! - station labels -
!
   Type(field), Dimension(:), Pointer :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: nv    ! - total number of variables -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - pointer arrays -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Pointer :: rlngd ! - longitudes within domain -
!
   Character(Len=*), Dimension(:,:), Pointer :: cstnd ! - station labels within domain -
!
! Locals
!
! Local scalars
   Integer :: l    ! - current field / lagged field -
   Integer :: l1   ! - current field -
   Integer :: iopt ! - selected option -
   Integer :: ifd  ! - field index -
   Integer :: ilf  ! - lagged field index -
#if GUI == 1
   Integer :: icv  ! - windows control variable -
   Integer :: nes  ! - number of ensemble members -
#endif
   Integer :: nlu  ! - number of lagged fields -
!
   Real(Kind=rp) :: rlatn ! - northernmost data latitude -
   Real(Kind=rp) :: rlats ! - southernmost data latitude -
   Real(Kind=rp) :: rlngw ! - westernmost data longitude -
   Real(Kind=rp) :: rlnge ! - easternmost data longitude -
#if GUI == 1
   Real(Kind=rp) :: drlat ! - latitudinal extent -
   Real(Kind=rp) :: drlng ! - longitudinal extent -
!
#endif
   Character(Len=32) :: cprmpt ! - prompt -
   Character(Len=24) :: cdom   ! - domain -
!
   Type(area) :: dlim ! - data limits -
#if GUI == 1
   Type(area) :: mlim ! - map limits -
#endif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Floor, MaxVal, MinVal, Real, Sum, Trim
#if GUI == 1
   Intrinsic Max, Min
!
! External routines
   Interface
      Subroutine draw_domain (isize, mlim, alim, ctitle, ifail)
         Use space, Only: area
         Integer, Intent(In) :: isize
         Character(Len=*), Intent(In) :: ctitle
         Type(area), Intent(In) :: mlim, alim
         Integer, Intent(Out) :: ifail
      End Subroutine draw_domain
   End Interface
!
#endif
! Executable Statements
!
! Set domain label
   Select Case (cxy)
    Case ('x', 'X')
      cdom = Trim(l_datdom%c)//' (X)'
    Case ('y', 'Y')
      cdom = Trim(l_datdom%c)//' (Y)'
    Case Default
      cdom = cg_domain
   End Select
!
! Repeat for each field
#if GUI == 1
   nes = Max(1,nem)
#endif
   If (.not.lssq) Then
      nlu = nls
   Else
      nlu = 1
   End If
   iopt = 0
   afield(:)%nv = 0
   get_doms: Do ifd = 1, nfs
      l1 = (ifd - 1)*nls + 1
      Do ilf = 1, nls
         l = (ifd - 1)*nls + ilf
!
! Force identical domains
! - ensemble members -
         If (afield(l)%member > 1) Then
            afield(l)%region = afield(l1)%region
            Cycle
         End If
! - seasons for PFV -
         If (ilf > 1 .and. nlu == 1) Then
            afield(l)%region = afield(l1)%region
            Cycle
         End If
!
! Calculate domain limits
         rlatn = MaxVal(rlat(1:afield(l)%nlt,l))
         rlats = MinVal(rlat(1:afield(l)%nlt,l))
         If (rlats == rlatn) Then
            rlatn = rlatn + one
            rlats = rlats - one
         End If
         rlngw = MinVal(rlng(1:afield(l)%nlg,l))
         rlnge = MaxVal(rlng(1:afield(l)%nlg,l))
         If (rlnge == rlngw) Then
            rlngw = rlngw - one
            rlnge = rlnge + one
         End If
#if GUI == 1
         drlat = rlatn - rlats
         drlng = rlnge - rlngw
#endif
!
! Calculate data limits
         dlim%rltn = Real(Ceiling(rlatn), Kind=rp)
         dlim%rlts = Real(Floor(rlats), Kind=rp)
         dlim%rlgw = Real(Floor(rlngw), Kind=rp)
         dlim%rlge = Real(Ceiling(rlnge), Kind=rp)
#if GUI == 1
         mlim = dlim
#endif
!
! Prompt for area of interest
         Do
            If (iopt == 4) GoTo 1
            If (larea) Then
               iopt = 0
            Else
               afield(l)%region%alim = dlim
               iopt = 1
               GoTo 1 ! - skip to avoid area prompt -
            End If
            Call init_win (title=Trim(cdom))
#if GUI == 1
            lmds=.false.
#endif
            Call boxes_open (2,1, .true., .false.)
            If (nfs > 1) Then ! - multiple fields -
               Call print_cvalue ('Field', Trim(afield(l)%var), &
                    itab=15, iwid=15)
            End If
            If (nlu > 1) Then ! - multiple lagged fields -
               Call print_cvalue ('Lagged-field', Trim(afield(l)%cssn%cssn(1)), &
                    itab=15, iwid=15)
            End If
            If (nms > 1) Then ! - multiple models -
               Call print_cvalue (Trim(cg_model), Trim(afield(l)%model), &
                    itab=15, iwid=15)
            End If
            If (nfs*nlu > 1) Then
               Call print_text (' ')
               Write (Unit=cprmpt, Fmt='(A,I0,A)') Trim(cdom)//' (', l, ')'
            Else
               Write (Unit=cprmpt, Fmt='(A)') Trim(cdom)
            End If
            Call box_open (.false., .false., &
                 title=Trim(cprmpt))
            Call prompt_domain (Trim(cg_domlims), afield(l)%region%alim, &
                 dlim=dlim)
            Call box_close (.true.)
#if GUI == 1
! - map -
            Call box_close (.false.)
            Call print_text (' ', &
                 ladvance=.false.)
            Call add_graphic_area (ihsize, ihsize, &
                 lbox=.true., lmouse=.true., cbf=select_domain)
            Call draw_domain (ihsize, mlim, afield(l)%region%alim, Trim(cdom), ifail)
            Call box_close (.false.)
            Select Case (ifail)
             Case (0)
               Call gui_creturn (.false.)
               Call gui_creturn (.true.)
               If (nfs*nlu/nes == 1) Then
                  Call add_button ('bt', 1, '&OK', &
                       iset1=iopt, ival1=1, scbf='EXIT')
                  Call add_button ('bt', 2, l_datlimits, &
                       iset1=iopt, ival1=2, scbf='EXIT')
                  Call add_button ('bt', 3, l_redrawmap, &
                       ldef=.true., iset1=iopt, ival1=5, scbf='EXIT')
               Else If (l == 1) Then
                  Call add_button ('bt', 1, l_next, &
                       iset1=iopt, ival1=1, scbf='EXIT')
                  Call add_button ('bt', 2, l_datlimits, &
                       iset1=iopt, ival1=2, scbf='EXIT')
                  Call add_button ('bt', 3, l_applyall, &
                       iset1=iopt, ival1=4, scbf='EXIT')
                  Call add_button ('bt', 4, l_redrawmap, &
                       ldef=.true., iset1=iopt, ival1=5, scbf='EXIT')
               Else If (l+nes-1<nfs*nlu) Then
                  Call add_button ('bt', 1, l_next, &
                       iset1=iopt, ival1=1, scbf='EXIT')
                  Call add_button ('bt', 2, l_datlimits, &
                       iset1=iopt, ival1=2, scbf='EXIT')
                  Call add_button ('bt', 3, l_sameprev, &
                       iset1=iopt, ival1=3, scbf='EXIT')
                  Call add_button ('bt', 4, l_applyalls, &
                       iset1=iopt, ival1=4, scbf='EXIT')
                  Call add_button ('bt', 5, l_redrawmap, &
                       ldef=.true., iset1=iopt, ival1=5, scbf='EXIT')
               Else
                  Call add_button ('bt', 1, '&OK', &
                       iset1=iopt, ival1=1, scbf='EXIT')
                  Call add_button ('bt', 2, l_datlimits, &
                       iset1=iopt, ival1=2, scbf='EXIT')
                  Call add_button ('bt', 3, l_sameprev, &
                       iset1=iopt, ival1=3, scbf='EXIT')
                  Call add_button ('bt', 4, l_redrawmap, &
                       ldef=.true., iset1=iopt, ival1=5, scbf='EXIT')
               End If
               Call add_button ('bt', 0, l_cancel)
             Case Default ! - force window to close -
               Call leave_window (icv)
               icv = 0
               Call window_update (icv)
               Call cpt_error ('draw_map', .true., ifail)
               ifail = 1
               Exit get_doms
            End Select
#else
            iopt = 1
#endif
!
! Confirm selection
1           Select Case (iopt)
             Case (1) ! - OK -
#if GUI == 1
               Call get_drawn_domain (drlat, drlng, afield(l)%region%alim)
#endif
             Case (2) ! - data limits -
               afield(l)%region%alim = dlim
               Cycle
             Case (5) ! - redraw map -
#if GUI == 1
               Call get_drawn_domain (drlat, drlng, afield(l)%region%alim)
               mlim%rltn = Max(dlim%rltn, afield(l)%region%alim%rltn)
               mlim%rlts = Min(dlim%rlts, afield(l)%region%alim%rlts)
               mlim%rlgw = Min(dlim%rlgw, afield(l)%region%alim%rlgw)
               mlim%rlge = Max(dlim%rlge, afield(l)%region%alim%rlge)
#endif
               Cycle
             Case (3) ! - same as previous -
               afield(l)%region%alim = afield(l-1)%region%alim
             Case (4) ! - apply to all -
#if GUI == 1
               afield(l)%region%alim = afield(1)%region%alim
               Call get_drawn_domain (drlat, drlng, afield(l)%region%alim)
#endif
             Case Default ! - cancel -
               ifail = 1
               Call set_cursor_waiting (0)
               Exit get_doms
            End Select
#if GUI == 1
            icv = 0
            Call window_update (icv)
#endif
!
! Check domain
            Select Case (idstr)
             Case (id_grd)
               Call check_grid_domain (afield(l)%nlt, afield(l)%nlg, rlat(:,l), rlng(:,l), afield(l)%region, &
                    rlatd(:,l), rlngd(:,l), idom(:,l), afield(l)%nv, ifail)
             Case (id_stn)
               Call check_stn_domain (afield(l)%nlt, rlatn, rlats, afield(l)%region%alim, rlat(:,l), rlng(:,l), cstn(:,l), &
                    afield(l)%nv, rlatd(:,l), rlngd(:,l), cstnd(:,l), idom(:,l), ifail)
                    afield(l)%region%nlts = afield(l)%nv
                    afield(l)%region%nlgs = afield(l)%nv
            End Select
            If (ifail == 0) Exit
            iopt = 0
            Call correct_domain (ifail, rlatn, rlats, rlngw, rlnge, afield(l)%region%alim)
            Call cpt_error ('get_area', .false., ifail)
         End Do
      End Do
   End Do get_doms
!
! Update number of variables
   If (ifail == 0) Then
      nv = Sum(afield(1:)%nv)
      Call window_update (nv)
   End If
!
   Return
!
 Contains
!
!
#if GUI == 1
  Subroutine get_drawn_domain (drlat, drlng, alim)
!
! Attempts to correct domain
!
! Modules
   Use data_numbers, Only: thirty, r90, r360
   Use space,        Only: x1, x2, y1, y2
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: drlat ! - longitudinal extent -
   Real(Kind=rp), Intent(In) :: drlng ! - longitudinal extent -
!
! - output scalars -
   Type(area), Intent(Out) :: alim ! - domain -
!
! Executable Statements
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Min, Nint, Real
!
! Limit selected area to area of map
   If (lmds) Then
      If (y1 > y2) Then
         alim%rltn = Min(y1, r90)
         alim%rlts = Max(y2, -r90)
      Else
         alim%rltn = Min(y2, r90)
         alim%rlts = Max(y1, -r90)
      End If
      If (x1 < x2) Then
         alim%rlgw = x1
         alim%rlge = x2
      Else
         alim%rlgw = x2
         alim%rlge = x1
      End If
      If (alim%rlge > r360) Then
         If (alim%rlge - alim%rlgw < r360) Then
            alim%rlge = alim%rlge - r360
         Else
            alim%rlge = r360
         End If
      End If
      If (alim%rlge - alim%rlgw > r360) alim%rlgw = alim%rlge - r360
      If (drlat > thirty .or. drlng > thirty) Then
         alim%rltn = Real(Nint(alim%rltn), Kind=rp)
         alim%rlts = Real(Nint(alim%rlts), Kind=rp)
         alim%rlgw = Real(Nint(alim%rlgw), Kind=rp)
         alim%rlge = Real(Nint(alim%rlge), Kind=rp)
      End If
   End If
!
   Return
  End Subroutine get_drawn_domain
!
!
!
#endif
  Subroutine correct_domain (ifail, rlatn, rlats, rlngw, rlnge, alim)
!
! Attempts to correct domain
!
! Modules
   Use data_numbers, Only: r90, r360
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ifail ! - error indicator -
!
   Real(Kind=rp), Intent(In) :: rlatn ! - northernmost data latitude -
   Real(Kind=rp), Intent(In) :: rlats ! - southernmost data latitude -
   Real(Kind=rp), Intent(In) :: rlngw ! - westernmost data longitude -
   Real(Kind=rp), Intent(In) :: rlnge ! - easternmost data longitude -
!
! - output scalars -
   Type(area), Intent(Out) :: alim ! - domain -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Ceiling, Floor, Max, Min, Real
!
! Executable Statements
!
! Attempt to correct domain
   Select Case (ifail)
    Case (1) ! - northernmost latitude south of southernmost -
      If (rlatn > rlats) Then
         alim%rltn = Real(Ceiling(rlatn), Kind=rp)
         alim%rlts = Real(Floor(rlats), Kind=rp)
      Else
         alim%rltn = Min(Real(Ceiling(rlatn)+1, Kind=rp), r90)
         alim%rlts = Max(Real(Floor(rlats)-1, Kind=rp), -r90)
      End If
    Case (2) ! - northernmost latitude south of southern limit of map -
      If (rlatn > rlats) Then
         alim%rltn = Real(Ceiling(rlatn), Kind=rp)
      Else
         alim%rltn = Min(Real(Ceiling(rlatn)+1, Kind=rp), r90)
      End If
    Case (3) ! - southernmost latitude north of northern limit of map -
      If (rlats < rlatn) Then
         alim%rlts = Real(Floor(rlats), Kind=rp)
      Else
         alim%rlts = Max(Real(Floor(rlats)-1, Kind=rp), -r90)
      End If
    Case (4) ! - westernmost longitude out of range -
      If (rlngw < rlnge) Then
         alim%rlgw = Real(Floor(rlngw), Kind=rp)
         alim%rlge = Real(Ceiling(rlnge), Kind=rp)
      Else
         alim%rlgw = Max(Real(Floor(rlngw)-1, Kind=rp), -r360)
         alim%rlge = Min(Real(Ceiling(rlnge)+1, Kind=rp), r360)
      End If
     Case (5) ! - no data within domain -
      If (rlngw < rlnge) Then
         alim%rlgw = Real(Floor(rlngw), Kind=rp)
         alim%rlge = Real(Ceiling(rlnge), Kind=rp)
      Else
         alim%rlgw = Max(Real(Floor(rlngw)-1, Kind=rp), -r360)
         alim%rlge = Min(Real(Ceiling(rlnge)+1, Kind=rp), r360)
      End If
   End Select
!
   Return
  End Subroutine correct_domain
 End Subroutine get_area
!
!
!
 Function get_grid_coors()
!
! Identify grid coordinates 
!
! Modules
   Use data_io_constants, Only: lcoo
   Use settings,          Only: ivf
   Use space,             Only: rlatdy, rlngdy, &
                                make_coors
!
! Function result
   Character(Len=lcoo) :: get_grid_coors
!
! Locals
!
! Local scalars
   Integer :: ilt ! - latitude index -
   Integer :: jlg ! - longitude index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Mod
!
! Executable Statements
!
! Identify grid
   jlg = Mod(ivf,yfield(iffy)%region%nlgs)
   ilt = ivf/yfield(iffy)%region%nlgs
   If (jlg == 0) Then
      jlg = yfield(iffy)%region%nlgs
   Else
      ilt = ilt + 1
   End If
!
! Construct coordinate label
   get_grid_coors = make_coors(rlatdy(ilt,iffy), rlngdy(jlg,iffy))
!
   Return
 End Function get_grid_coors
!
!
!
 Function get_stn_coors()
!
! Identify station coordinates
!
! Modules
   Use data_io_constants, Only: lcoo
   Use settings,          Only: ivf
   Use space,             Only: rlatdy, rlngdy, &
                                make_coors
!
! Function result
   Character(Len=lcoo) :: get_stn_coors
!
! Executable Statements
!
! Construct coordinate label
   get_stn_coors = make_coors(rlatdy(ivf,iffy), rlngdy(ivf,iffy))
!
   Return
 End Function get_stn_coors
!
!
!
 Subroutine first_field ()
!
! Initialises first gridpoint and field
!
! Modules
   Use settings, Only: iv, iva, ivf, ivf_old, ivfa
   Use arrays,   Only: iusey
   Use space,    Only: ilaty, ilngy
!
! Executable Statements
!
! Identify initial grid point
   iva = 1
   iv = iusey(1)
   ivfa = iva
   ivf = iv
   ivf_old = ivf
!
! Set initial field
   iffx = 1
   ifdx = 1
   ilfx = 1
   iffy = 1
   ifdy = 1
   ilfy = 1
   ilaty = 1 + (ivf - 1)/yfield(1)%region%nlgs
   ilngy = ivf - (ilaty - 1)*yfield(1)%region%nlgs
!
   Return
 End Subroutine first_field
!
!
!
#if GUI == 1
 Function check_ivf() &
#else
 Function check_ivf(iffy, ivfa) &
#endif
          Result (check)
!
! Checks for next available variable
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use screen,             Only: window_update
   Use settings,           Only: iv, iva, ivf
#if GUI == 1
   Use settings,           Only: ivf_old, ivfa
#endif
   Use arrays,             Only: iuse=>iusey
   Use space,              Only: cstnc,cstny
   Use iofiles,            Only: yfile
!
! Function result
   Integer :: check
!
#if GUI == 0
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iffy ! - current Y field / lagged field -
   Integer, Intent(In) :: ivfa ! - current available series by field / lagged field -
!
#endif
! Locals
!
! Local scalars
#if GUI == 1
   Integer :: i      ! - variable index -
   Integer :: ivmin  ! - minimum available variable index for current field -
   Integer :: ivmax  ! - maximum available variable index for current field -
   Integer :: ivamin ! - minimum variable index for current field -
   Integer :: ivamax ! - maximum variable index for current field -
#endif
   Integer :: iv0    ! - index of zeroth variable for current field -
   Integer :: iva0   ! - index of zeroth available variable for current field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Define limits for current field
   If (iffy == 1) Then
#if GUI == 1
      ivamin = 1
#endif
      iv0 = 0
      iva0 = 0
   Else
#if GUI == 1
      ivamin = Sum(yfield(1:iffy-1)%nva) + 1
#endif
      iv0 = Sum(yfield(1:iffy-1)%nv)
      iva0 = Sum(yfield(1:iffy-1)%nva)
   End If
#if GUI == 1
   ivamax = Sum(yfield(1:iffy)%nva)
   ivmin = iuse(ivamin)
   ivmax = iuse(ivamax)
!
! Check whether next series is available
   iv = iv0 + ivf
   If (iv < ivmin) Then
      iv = ivmax
      iva = ivamax
   Else If (iv > ivmax) Then
      iv = ivmin
      iva = ivamin
   Else
      If (ivf >= ivf_old) Then
         Do i = ivamin, ivamax
            If (iv == iuse(i)) Then
               iva = i
               Exit
            Else If (iv < iuse(i)) Then
               iv = iuse(i)
               iva = i
               Exit
            End If
         End Do
      Else
         Do i = ivamax, ivamin, -1
            If (iv == iuse(i)) Then
               iva = i
               Exit
            Else If (iv > iuse(i)) Then
               iv = iuse(i)
               iva = i
               Exit
            End If
         End Do
      End If
   End If
   ivf = iv - iv0
   ivfa = iva - iva0
   ivf_old = ivf
#else
!
! Identify next series
   iva = iva0 + ivfa
   iv = iuse(iva)
   ivf = iv - iv0
#endif
   Call window_update (ivf)
!
! Update coordinates
   Select Case (yfile%idstr)
    Case (id_grd)
      y_coors = get_grid_coors()
      Call window_update (y_coors)
    Case (id_stn)
      y_coors = get_stn_coors()
      cstnc = cstny(ivf,iffy)
      Call window_update (y_coors)
      Call window_update (cstnc)
    Case (id_ind)
      cstnc = cstny(ivf,iffy)
      Call window_update (cstnc)
   End Select
   check = 2
!
   Return
 End Function check_ivf
!
!
!
#if GUI == 1
 Function change_ifdx() &
          Result (chnge)
!
! Changes the X field
!
! Modules
   Use iofiles, Only: xfile
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change the X field
   chnge = change_ifd(ifdx, xfile%nfs, xfile%nls, xfield(:)%nv, xfield(:)%nva, iffx, ilimx0, ilimx1, ilimx2, &
           ilf=ilfx)
!
   Return
 End Function change_ifdx
!
!
!
 Function change_ifdy() &
          Result (chnge)
!
! Changes the Y field
!
! Modules
   Use iofiles, Only: yfile
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change the Y field
   chnge = change_ifd(ifdy, yfile%nfs, yfile%nls, yfield(:)%nv, yfield(:)%nva, iffy, ilimy0, ilimy1, ilimy2, &
           ilf=ilfy)
!
   Return
 End Function change_ifdy
!
!
!
 Function change_ilfx() &
          Result (chnge)
!
! Changes the X lagged field
!
! Modules
   Use iofiles, Only: xfile
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change the X lagged field
   chnge = change_ifd(ilfx, xfile%nfs, xfile%nls, xfield(:)%nv, xfield(:)%nva, iffx, ilimx0, ilimx1, ilimx2, &
           ivf=ifdx)
!
   Return
 End Function change_ilfx
!
!
!
 Function change_ilfy() &
          Result (chnge)
!
! Changes the Y lagged field
!
! Modules
   Use iofiles, Only: yfile
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change the Y lagged field
   chnge = change_ifd(ilfy, yfile%nfs, yfile%nls, yfield(:)%nv, yfield(:)%nva, iffy, ilimy0, ilimy1, ilimy2, &
           ivf=ifdy)
!
   Return
 End Function change_ilfy
!
!
!
 Function change_ifd(ifd, nfs, nls, nv, nva, iff, ilim0, ilim1, ilim2, &
          ilf, ivf) Result (chnge)
!
! Change a field
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
   Integer, Intent(In) :: nfs ! - number of fields -
   Integer, Intent(In) :: nls ! - number of lagged fields -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv  ! - number of variables -
   Integer, Dimension(:), Intent(In) :: nva ! - number of available variables -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ifd ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: iff   ! - field/lagged field -
   Integer, Intent(Out) :: ilim0 ! - offset -
   Integer, Intent(Out) :: ilim1 ! - lower limit -
   Integer, Intent(Out) :: ilim2 ! - upper limit -
!
! Optional arguments
! - optional input scalar -
   Integer, Intent(In), Optional :: ilf ! - lagged field -
   Integer, Intent(In), Optional :: ivf ! - variable field -
!
! Locals
!
! Local scalars
   Integer :: iadj ! - adjustment indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present
!
! Executable Statements
!
! Loop if necessary
   If (Present(ilf)) Then
      Call force_limits (1, nfs, ifd, iadj)
      iff = (ifd - 1)*nls + ilf
   Else If (Present(ivf)) Then
      Call force_limits (1, nls, ifd, iadj)
      iff = (ivf - 1)*nls + ifd
   End If
   If (iadj /= 0) Call window_update (ifd)
!
! Identify variable limits
   Call set_ilims (nv(:), nva(:), iff, ilim0, ilim1, ilim2)
   chnge = 2
!
   Return
 End Function change_ifd
!
!
!
 Function change_ilaty() &
          Result (chnge)
!
! Change the Y latitude
!
! Modules
   Use maths,  Only: force_limits
   Use screen, Only: window_update
   Use space,  Only: ilaty
!
! Function result
   Integer :: chnge
!
! Locals
!
! Local scalars
   Integer :: iadj ! - adjustment indicator -
!
! Executable Statements
!
! Loop if necessary
   Call force_limits (1, yfield(iffy)%region%nlts, ilaty, iadj)
   If (iadj /= 0) Call window_update (ilaty)
   chnge = change_ivf()
!
   Return
 End Function change_ilaty
!
!
!
 Function change_ilngy() &
          Result (chnge)
!
! Change the Y longitude
!
! Modules
   Use maths,  Only: force_limits
   Use screen, Only: window_update
   Use space,  Only: ilngy
!
! Function result
   Integer :: chnge
!
! Locals
!
! Local scalars
   Integer :: iadj ! - adjustment indicator -
!
! Executable Statements
!
! Loop if necessary
   Call force_limits (1, yfield(iffy)%region%nlgs, ilngy, iadj)
   If (iadj /= 0) Call window_update (ilngy)
   chnge = change_ivf()
!
   Return
 End Function change_ilngy
!
!
!
 Function change_ivf() &
          Result (chnge)
!
! Change the current variable
!
! Modules
   Use screen,   Only: window_update
   Use arrays,   Only: iuse=>iusey
   Use space,    Only: ilaty, ilngy
   Use iofiles,  Only: mya
   Use settings, Only: iv, iva, ivf, ivf_old, ivfa
!
! Function result
   Integer :: chnge
!
! Locals
!
! Local scalars
   Integer :: i    ! - variable index -
   Integer :: iv0  ! - index of zeroth variable for current field -
   Integer :: iva0 ! - index of zeroth available variable for current field -
!
   Logical :: lmod ! - modification? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Define limits for current field
   If (iffy == 1) Then
      iv0 = 0
      iva0 = 0
   Else
      iv0 = Sum(yfield(1:iffy-1)%nv)
      iva0 = Sum(yfield(1:iffy-1)%nva)
   End If
!
! Identify current variable
   ivf = (ilaty - 1)*yfield(iffy)%region%nlgs + ilngy
   iv = ivf + iv0
!
! Check whether current variable is used
   lmod = .false.
   If (iv <= mya) Then
      Do i = 1, iv
         If (iuse(i) < iv) Then
            Cycle
         Else If (iuse(i) == iv) Then
            iva = i
            Exit
         Else If (i > 1) Then
            If (ivf >= ivf_old) Then
               iv = iuse(i)
               iva = i
            Else
               iv = iuse(i-1)
               iva = i - 1
            End If
            lmod = .true.
            Exit
         Else
            If (ivf >= ivf_old) Then
               iva = iva0 + 1
            Else
               iva = Sum(yfield(1:iffy)%nva)
            End If
            iv = iuse(iva)
            lmod = .true.
            Exit
         End If
      End Do
   Else
      If (ivf >= ivf_old) Then
         iva = iva0 + 1
      Else
         iva = Sum(yfield(1:iffy)%nva)
      End If
      iv = iuse(iva)
      lmod = .true.
   End If
   ivfa = iva - iva0
   ivf = iv - iv0
   ivf_old = ivf
!
! Recalculate grid if necessary
   If (lmod) Then
      ilaty = 1 + (ivf - 1)/yfield(iffy)%region%nlgs
      ilngy = ivf - (ilaty - 1)*yfield(iffy)%region%nlgs
      Call window_update (ilaty)
      Call window_update (ilngy)
   End If
!
! Update coordinates
   y_coors = get_grid_coors()
   Call window_update (y_coors)
   chnge = 2
!
   Return
 End Function change_ivf
!
!
!
 Function set_iv()
!
! Set the current variable
!
! Modules
   Use arrays,   Only: iuse=>iusey
   Use space,    Only: ilaty, ilngy
   Use settings, Only: iv, iva, ivf, ivf_old, ivfa
!
! Function result
   Integer :: set_iv
!
! Locals
!
! Local scalars
   Integer :: i    ! - variable index -
   Integer :: iv0  ! - index of zeroth variable for current field -
   Integer :: iva0 ! - index of zeroth available variable for current field -
   Integer :: iv1  ! - index of first variable for current field -
   Integer :: iva1 ! - index of first available variable for current field -
   Integer :: ivn  ! - index of last variable for current field -
   Integer :: ivan ! - index of last available variable for current field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Define limits for current field
   If (iffy == 1) Then
      iv0 = 0
      iva0 = 0
   Else
      iv0 = Sum(yfield(1:iffy-1)%nv)
      iva0 = Sum(yfield(1:iffy-1)%nva)
   End If
   iv1 = iv0 + 1
   iva1 = iuse(iva0+1)
   ivn = Sum(yfield(1:iffy)%nv)
   ivan = Sum(yfield(1:iffy)%nva)
!
! Identify used variable
   iva = 0
    Do i = 1, iv
      If (iuse(i) == iv) Then
         iva = i
         Exit
      Else If (iuse(i) > iv) Then
         iv = iuse(i)
         iva = i
         Exit
      End If
   End Do
   ivf = iv - iv0
   ivfa = iva - iva0
   ivf_old = ivf
!
! Prevent change of field
   If (iv < iv1) Then
      iv = iv1
      ivf = iva1
   Else If (iv > ivn) Then
      iv = ivn
      ivf = ivan
   End If
!
! Update coordinates
   ilaty = 1 + (ivf - 1)/yfield(iffy)%region%nlgs
   ilngy = ivf - (ilaty - 1)*yfield(iffy)%region%nlgs
   set_iv = 2
!
   Return
 End Function set_iv
!
!
!
#endif
 Subroutine set_ilims (nv, nva, ifd, ilim0, ilim1, ilim2)
!
! Set the variable limits for the current field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ifd ! - current field -
!
! - output scalars -
   Integer, Intent(Out) :: ilim0 ! - 0th variable index -
   Integer, Intent(Out) :: ilim1 ! - first available variable index for current field -
   Integer, Intent(Out) :: ilim2 ! - last available variable index for current field -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv  ! - number of variables per field -
   Integer, Dimension(:), Intent(In) :: nva ! - number of available variables per field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Sum
!
! Executable Statements
!
! Identify variable limits
   If (ifd == 1) Then
      ilim0 = 0
      ilim1 = 1
   Else
      ilim0 = Sum(nv(1:ifd-1))
      ilim1 = Sum(nva(1:ifd-1)) + 1
   End If
   ilim2 = Sum(nva(1:ifd))
!
   Return
 End Subroutine set_ilims
!
!
!
#if GUI == 1
 Subroutine prompt_fields (cxy, fcallback)
!
! Prompts for fields and lagged fields
!
! Modules
   Use labels,  Only: cg_field, cg_lagfield
   Use gui,     Only: prompt_integer, prompt_list
   Use iofiles, Only: xfile, yfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=1), Intent(In) :: cxy ! - X/Y -
!
! - procedure arguments -
   Integer, External :: fcallback ! - callback function -
!
! Locals
!
! Local arrays
   Character(Len=lvar), Dimension(:), Allocatable :: vars ! - field variables -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, MaxVal, Trim
!
! Executable Statements
!
! Copy variable labels (to avoid FTN95 %ls bug when reading characters from a derived type)
   If (xfile%nfs > 1 .or. xfile%nls > 1 .or. yfile%nfs > 1 .or. yfile%nls > 1) Then
      Allocate (vars(MaxVal([xfile%nfs, xfile%nls, yfile%nfs, yfile%nls])))
   End If
!
! Prompt for field and lagged field
   Select Case (cxy)
    Case ('X', 'x')
      If (xfile%nfs > 1 .and. xfile%nls > 1) Then ! - multiple fields and lagged fields -
         Call prompt_integer (Trim(cg_field)//' X', ifdx, &
              itab=15, fcb1=change_ifdx, fcb2=fcallback)
         Call prompt_integer (Trim(cg_lagfield)//' X',ilfx, &
              itab=15, fcb1=change_ilfx, fcb2=fcallback)
      Else If (xfile%nfs > 1) Then ! - multiple fields -
         vars(1:xfile%nfs) = xfield(:)%var
         Call prompt_list (Trim(cg_field)//' X', .true., vars(1:xfile%nfs), xfile%nfs, ifdx, &
              itab=15, iwdth=lvar, cbf1=change_ifdx, cbf2=fcallback)
      Else If (xfile%nls > 1) Then ! - multiple lagged fields -
         vars(1:xfile%nls) = xfield(:)%var
         Call prompt_list (Trim(cg_lagfield)//' X', .true., vars(1:xfile%nls), xfile%nls, ilfx, &
              itab=15, iwdth=lvar, cbf1=change_ilfx, cbf2=fcallback)
      End If
    Case ('Y', 'y')
      If (yfile%nfs > 1 .and. yfile%nls > 1) Then ! - multiple fields and lagged fields -
         Call prompt_integer (Trim(cg_field)//' Y',ifdy, &
              itab=15, fcb1=change_ifdy, fcb2=fcallback)
         Call prompt_integer (Trim(cg_lagfield)//' y',ilfy, &
              itab=15, fcb1=change_ilfy, fcb2=fcallback)
      Else If (yfile%nfs > 1) Then ! - multiple fields -
         vars(1:yfile%nfs) = yfield(:)%var
         Call prompt_list (Trim(cg_field)//' Y', .true., vars(1:yfile%nfs), yfile%nfs, ifdy, &
              itab=15, iwdth=lvar, cbf1=change_ifdy, cbf2=fcallback)
      Else If (yfile%nls > 1) Then ! - multiple lagged fields -
         vars(1:yfile%nls) = yfield(:)%var
         Call prompt_list (Trim(cg_lagfield)//' Y', .true., vars(1:yfile%nls), yfile%nls, ilfy, &
              itab=15, iwdth=lvar, cbf1=change_ilfy, cbf2=fcallback)
      End If
   End Select
   If (Allocated(vars)) Deallocate (vars)
!
   Return
 End Subroutine prompt_fields
!
!
!
 Subroutine prompt_location (fcallback)
!
! Prompts for fields and lagged fields
!
! Modules
   Use data_cpt_constants, Only: id_grd
   Use labels,             Only: cg_lat, cg_lng
   Use gui,                Only: prompt_integer
   Use space,              Only: ilaty, ilngy
   Use iofiles,            Only: yfile
   Use settings,           Only: ivf
!
! Arguments
!
! Procedure arguments
   Integer, External :: fcallback ! - callback function -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for locatoin
   Select Case (yfile%idstr)
    Case (id_grd)
      Call prompt_integer (Trim(cg_lat), ilaty, &
           itab=15, fcb1=change_ilaty, fcb2=fcallback)
      Call prompt_integer (Trim(cg_lng), ilngy, &
           itab=15, fcb1=change_ilngy, fcb2=fcallback)
    Case Default
      Call prompt_integer (Trim(dsdy), ivf, &
           itab=15, fcb1=check_ivf, fcb2=fcallback)
   End Select
!
   Return
 End Subroutine prompt_location
#else
!
!
!
 Subroutine print_field (iout, afield, nfs, nls, nms)
!
! Prints field information
!
! Modules
   Use gui, Only: print_text
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: nfs  ! - number of fields -
   Integer, Intent(In) :: nls  ! - number of lagged-fields -
   Integer, Intent(In) :: nms  ! - number of models -
!
   Type(field), Intent(In) :: afield ! - field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Print field information
   If (nfs > 1) Call print_text ('Field: '//Trim(afield%var), &
                     iout=iout)
   If (nls > 1) Call print_text ('Lag:   '//Trim(afield%cssn%cssn(1)), &
                     iout=iout)
   If (nms > 1) Call print_text ('Model: '//Trim(afield%model), &
                     iout=iout)
!
   Return
 End Subroutine print_field
#endif
!
!
!
 Subroutine proj_read_fields (iin, pver, lssq, nfx, nfy, nfz, xmiss, ymiss, zmiss, ifail)
!
! Reads field settings from project file
!
! Modules
   Use data_numbers,        Only: sp, tolv
   Use data_time_constants, Only: isq_mn
   Use time,                Only: ims, mpsn, &
                                  date_diff, reset_time
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
   Integer, Intent(In) :: nfx ! - number of X fields -
   Integer, Intent(In) :: nfy ! - number of Y fields -
   Integer, Intent(In) :: nfz ! - number of forecast fields -
!
   Real(Kind=sp), Intent(In) :: pver ! - project file version -
!
   Real(Kind=rp), Intent(In) :: xmiss ! - missing X data flag -
   Real(Kind=rp), Intent(In) :: ymiss ! - missing Y data flag -
   Real(Kind=rp), Intent(In) :: zmiss ! - missing forecast data flag -
!
   Logical, Intent(In) :: lssq ! - permit seasonally-sequenced files? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: l  ! - index -
   Integer :: lb ! - lower-bound of yfield -
!
   Logical :: lnew ! - new project file? -
!
! Executable Statements
!
! Read field settings
   lnew = (pver > 17.04_sp-tolv)
   Call proj_read_field (iin, lnew, nfx, 1, xmiss, xfield, ifail, &
        iffv=iffx, ilim0=ilimx0, ilim1=ilimx1, ilim2=ilimx2)
   If (ifail /= 0) Return
   If (.not.lnew) Read (Unit=iin, Err=1, End=2) lb
   Call proj_read_field (iin, lnew, nfy, lb, ymiss, yfield, ifail, &
        iffv=iffy, ilim0=ilimy0, ilim1=ilimy1, ilim2=ilimy2)
   If (ifail /= 0) Return
   Read (Unit=iin, Err=1, End=2) nmfd
   Call proj_read_field (iin, lnew, nmfd, 1, ymiss, mfield, ifail)
   If (ifail /= 0) Return
   Call proj_read_field (iin, lnew, nfz, 1, zmiss, zfield, ifail)
!
! Set season details
   If (lssq) Then
      Do l = 1, nfx
         ims(l) = xfield(l)%tssn%tprd(1)%sdate%imn
         mpsn(l) = date_diff(xfield(l)%tssn%tprd(1)%sdate, xfield(l)%tssn%tprd(1)%edate, isq_mn) + 1
      End Do
   Else
      Call reset_time (.false.)
   End If
!
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
 Contains
!
!
 Subroutine proj_read_field (iin, lnew, nfl, lb, rmiss, afield, ifail, &
            iffv, ilim0, ilim1, ilim2)
!
! Reads field settings from project file
!
! Modules
   Use time, Only: get_cssn, set_pssn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
   Integer, Intent(In) :: nfl ! - number of fields -
   Integer, Intent(In) :: lb  ! - lower bound -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
   Logical, Intent(In) :: lnew ! - new project file? -
!
! - pointer arrays -
   Type(field), Dimension(:), Pointer :: afield ! - field -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: iffv ! - current field / lagged field -
!
! - optional output scalars -
   Integer, Intent(Out), Optional :: ilim0 ! - 0th index -
   Integer, Intent(Out), Optional :: ilim1 ! - first available index for current field -
   Integer, Intent(Out), Optional :: ilim2 ! - last available index for current field -
!
! Locals
!
! Local scalars
   Integer :: l   ! - field index -
!
   Type(old_field) :: ofield
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Present
!
! Executable Statements
!
! Read input file settings
   Call init_field (nfl, [rmiss], afield, ifail)
   If (ifail /= 0) GoTo 3
   If (lnew) Then
      Do l = 1, Max(1,nfl)
         Read (Unit=iin, Err=1, End=2) afield(l)
      End Do
   Else ! - read old season component fields -
      If (lb >= 1) Then
         Do l = 1, Max(1,nfl)
            Read (Unit=iin, Err=1, End=2) ofield
            afield(l) = ofield
            afield(l) = ofield
         End Do
      Else ! - old persistence fields -
         Read (Unit=iin, Err=1, End=2) ofield ! - first field is for the combined season -
         afield(1) = ofield
         Read (Unit=iin, Err=1, End=2) ofield ! - second field is for the forecast season -
         afield(1)%tssn%tprd(1) = ofield%tprd
         afield(1)%cssn%cssn(1) = ofield%cssn
         Call set_pssn (afield(1)%tssn)
         afield(1)%cssn%cssn(0) = get_cssn(afield(1)%tssn%tprd(0), 1, [1])
      End If
   End If
!
! Set limits
   If (Present(iffv)) Call set_ilims (afield(:)%nv, afield(1:)%nva, iffv, ilim0, ilim1, ilim2)
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
  End Subroutine proj_read_field
 End Subroutine proj_read_fields
!
!
!
 Subroutine proj_write_fields (iout, nflx, nfly, nflz, ifail)
!
! Writes field settings to project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
   Integer, Intent(In) :: nflx ! - number of X fields -
   Integer, Intent(In) :: nfly ! - number of Y fields -
   Integer, Intent(In) :: nflz ! - number of Z fields -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: l  ! - field index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal
!
! Executable Statements
!
! Write field settings
! - field information -
   Do l = 1, MaxVal([1,nflx])
      Write (Unit=iout, Err=1) xfield(l)
   End Do
   Do l = 1, MaxVal([1,nfly])
      Write (Unit=iout, Err=1) yfield(l)
   End Do
   Write (Unit=iout, Err=1) nmfd
   Do l = 1, Max(1,nmfd)
      Write (Unit=iout, Err=1) mfield(l)
   End Do
   Do l = 1, Max(1,nflz)
      Write (Unit=iout, Err=1) zfield(l)
   End Do
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_fields
End Module fields
