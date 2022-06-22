! Probabilistic Forecast Verification (PFV)
! Author: Simon Mason
Module pfv
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: init_analysis_pfv, perform_pfv, proj_read_pfv
!
Contains
!
!
 Function perform_pfv(ierr)
!
! Performs Probabilistic Forecast Verification
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail =  7 No matching grids
!    ifail = 11 No matching X field for the ierr-th Y field
!    ifail = 12 Non-matching station/index name for the ierr-th Y field
!    ifail = 13 Non-matching latitude for the ierr-th Y field
!    ifail = 14 Non-matching latitude for the ierr-th Y field
!    ifail = -1 Terminated
!
! Modules
   Use data_numbers,       Only: rp, one, oneh, tolp
   Use data_cpt_constants, Only: id_grd, ng
   Use maths,              Only: approx
   Use time,               Only: nss
   Use progress_meter,     Only: set_progress_increment
   Use settings,           Only: iretro, izero, ncv, nret, nretro, nu, nur, nu1
   Use arrays,             Only: iusex, iusey, x
   Use space,              Only: get_interpolated, set_nearest_grids
   Use iofiles,            Only: mya, xfile, yfile
   Use fields,             Only: imfs, xfield, yfield, &
                                 match_fields
   Use categories,         Only: pobs, fps_r, odds_r, &
                                 calc_odds
   Use analysis,           Only: ianal, &
                                 num_calcs
!
! Function result
   Integer :: perform_pfv
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ierr ! - unmatched field -
!
! Locals
!
! Local scalars
   Integer :: i     ! - location index -
   Integer :: k     ! - case index -
   Integer :: nlt   ! - number of latitudes -
   Integer :: nlg   ! - number of longitudes -
   Integer :: ifail ! - error indicator -
!
   Real(Kind=rp) :: sp ! - sum of probabilities -
!
   Logical :: lmssn ! - match seasons? -
   Logical :: lpa   ! - probabilities available? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic MaxVal, Mod, Real, Sum
!
! Executable Statements
!
! Initialise analysis
   perform_pfv = -1
   ierr = 0
   Select Case (yfile%idstr)
    Case (id_grd)
      nlt = MaxVal(yfield(:)%region%nlts)
      nlg = MaxVal(yfield(:)%region%nlgs)
    Case Default
      nlt = MaxVal(yfield(:)%nva)
      nlg = nlt
   End Select
   Call init_analysis_pfv (ng, nu, nlt, nlg, yfile%nfl, ifail)
   If (ifail /= 0) Then
      perform_pfv = 1
      Return
   End If
   ncv = nu
!
! Check for matching fields
   lmssn = (Mod(xfile%it1,nss) == Mod(yfile%it1,nss)) ! - do not attempt to match fields if there are different start-date offsets -
   Call match_fields (xfile%idstr, xfile%nfl, yfile%nfl, lmssn, imfs, ierr, ifail)
   If (ifail /= 0) Then
       perform_pfv = ifail + 10
       Return
   End If
!
! Estimate number of calculations
   Call set_progress_increment (one/Real(num_calcs(ianal, iretro, 0, izero, 1, mya, nu, nret, nretro, nu1, nur, 0), Kind=rp), &
        ladjust=.true.)
!
! Identify nearest grids
   If (xfile%idstr == id_grd) Then
      Call set_nearest_grids (yfile%idstr, 0, xfile%nfl, yfile%nfl, xfield(:)%nlt, xfield(:)%nlg, yfield(1:)%nva, &
           xfield(:)%region, yfield(1:)%region, ifail, &
           imfs=imfs(:))
      Select Case (ifail)
       Case (0)
         Continue
       Case (-1)
         Return
       Case Default
         perform_pfv = ifail + 2
         Return
      End Select
   End If
!
! Calculate interpolated values
   Call get_interpolated (ianal, xfile%idstr, yfile%idstr, 0, mya, iusex, iusey, xfile%nfl, yfile%nfl, nu, xfile%ngs, &
        xfield(:)%nv, yfield(1:)%nva, x, xfield(:)%rmiss, xfield(:)%region, yfield(1:)%region, fps_r,                 &
        imfs=imfs)
!
! Reset any missing forecast probabilities
   lpa = .false.
   Do i = 1, mya
      Do k = 1, nu
         sp = Sum(fps_r(i,k,:))
         If (approx(sp, one, utol=tolp) .or. approx(sp/oneh, one, utol=tolp)) Then
            lpa = .true.
         Else
            fps_r(i,k,:) = -one
         End If
      End Do
   End Do
   If (.not.lpa) Then
      perform_pfv = 7
      Return
   End If
!
! Calculate odds and relative odds
   Call calc_odds (nu, ng, mya, pobs(:,:), fps_r(:,:,:), odds_r(:,:,:,:))
   perform_pfv = 0
!
   Return
 End Function perform_pfv
!
!
!
 Subroutine init_analysis_pfv (ng, n, nlt, nlg, ndom, ifail)
!
! Initialises memory for PFV
!
! Modules
   Use settings,   Only: nret, nt, ntr, nt1, nur
   Use arrays,     Only: rwk
   Use space,      Only: init_space
   Use iofiles,    Only: mya, nx, ny
   Use fields,     Only: imfs, &
                         first_field
   Use categories, Only: fps_r, odds_r
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng   ! - number of categories -
   Integer, Intent(In) :: n    ! - number of cases -
   Integer, Intent(In) :: nlt  ! - number of latitudes -
   Integer, Intent(In) :: nlg  ! - number of longitudes -
   Integer, Intent(In) :: ndom ! - number of domains -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Max
!
! Executable Statements
!
! Free existing memory
   If (Allocated(rwk)) Deallocate (rwk)
!
! Allocate additional memory
! - matched X fields -
   Allocate (imfs(ndom), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - retroactive forecast probabilities -
   Allocate (fps_r(mya*ndom,n,ng), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - retroactive odds -
   Allocate (odds_r(mya*ndom,n,ng,2), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - nearest latitudes and longitudes -
   ifail=init_space(nlt, nlg, ndom)
   If (ifail /= 0) GoTo 1
!
! Allocate workspace
   Allocate (rwk(Max(n, nx, ny, nlg, nlt)), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
! Identify initial grid point
   Call first_field ()
!
! Set retroactive period to the same as the training period
   nt1 = 0
   ntr = nt
   nur = n
   nret = 1
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_analysis_pfv
!
!
!
 Subroutine proj_read_pfv (ifail)
!
! Reads PFV data from project file and initialises memory to read results
!
! Modules
   Use data_cpt_constants, Only: ia_pfv, id_grd, ng
   Use settings,           Only: nt
   Use iofiles,            Only: yfile
   Use fields,             Only: xfield, yfield
   Use analysis,           Only: ihind, &
                                 init_results
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, MaxVal
!
! Executable Statements
!
! Initialise memory
   Select Case (yfile%idstr)
    Case (id_grd)
      Call init_analysis_pfv (ng, nt, MaxVal(yfield(:)%region%nlts), MaxVal(yfield(:)%region%nlgs), yfile%nfl, ifail)
    Case Default
      Call init_analysis_pfv (ng, nt, MaxVal(yfield(:)%nva), MaxVal(yfield(:)%nva), yfile%nfl, ifail)
   End Select
   If (ifail /= 0) GoTo 4
   ihind = 0
   Call init_results (ia_pfv, Max(MaxVal(xfield(:)%nlt), MaxVal(yfield(:)%nlt)), &
        Max(MaxVal(xfield(:)%nlg), MaxVal(yfield(:)%nlg)), ifail)
   If (ifail /= 0) GoTo 4
!
! Errors
! - problem initializing memory for results -
4  ifail = 4
   Return
!
 End Subroutine proj_read_pfv
End Module pfv
