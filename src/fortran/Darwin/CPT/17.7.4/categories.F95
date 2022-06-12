! Author: Simon Mason
Module categories
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ng, nts
   Use time,               Only: pdate
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: calc_odds, calc_thresholds, change_cat, check_refdates, close_categories, init_categories, init_icat, init_refdates, &
             init_thresholds, parameter_write_categories, proj_read_categories, proj_write_categories, set_cv_categories,         &
             set_c2_categories, set_percentiles, set_ra_categories, set_thresholds
!
! Scalars
!
! Integer scalars
   Integer, Public :: ithr    ! - threshold type -
   Integer, Public :: ithr_bk ! - backup threshold type -
   Integer, Public :: icatc   ! - current threshold -
   Integer, Public :: icatp   ! - previous threshold -
!
! Arrays
!
! Integer arrays
   Integer, Dimension(nts), Public :: irefs ! - reference-date indices -
!
   Integer, Dimension(:,:), Allocatable, Public :: ifor_c ! - forecast categories -
   Integer, Dimension(:,:), Allocatable, Public :: ifor_2 ! - double cross-validated forecast categories -
   Integer, Dimension(:,:), Allocatable, Public :: ifor_r ! - retroactive forecast categories -
   Integer, Dimension(:,:), Allocatable, Public :: iobs_c ! - observed categories -
   Integer, Dimension(:,:), Allocatable, Public :: iobs_2 ! - double cross-validated observed categories -
   Integer, Dimension(:,:), Allocatable, Public :: iobs_r ! - retroactive observed categories -
!
! Real arrays
   Real(Kind=rp), Dimension(nts), Public :: pthr ! - percentile thresholds -
   Real(Kind=rp), Dimension(nts), Public :: thr  ! - absolute thresholds -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hps   ! - highest forecast probabilities -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: pobs  ! - climatological percentile thresholds -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tobs  ! - climatological absolute thresholds -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tobst ! - transformed climatological absolute thresholds -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tobsx ! - model absolute thresholds -
!
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: efps  ! - ensemble forecast probabilities -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: fps_p ! - forecast probabilities -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: fps_r ! - retroactive forecast probabilities -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: fps_2 ! - double cross-validated forecast probabilities -
!
   Real(Kind=rp), Dimension(:,:,:,:), Allocatable, Public :: odds_p ! - forecast odds -
   Real(Kind=rp), Dimension(:,:,:,:), Allocatable, Public :: odds_r ! - retroactive odds -
   Real(Kind=rp), Dimension(:,:,:,:), Allocatable, Public :: odds_2 ! - double cross-validated odds -
!
! Derived-Type arrays
   Type(pdate), Dimension(nts), Public :: refs ! - reference dates -
!
Contains
!
!
 Subroutine init_icat ()
!
! Initialises category indicator
!
! Executable Statements
!
! Initialize category indicator
   icatc = 1
   icatp = 1
!
   Return
 End Subroutine init_icat
!
!
!
 Subroutine init_refdates ()
!
! Initialises reference dates
!
! Modules
   Use time, Only: Assignment(=)
!
! Executable Statements
!
! Initialize reference dates
   refs(:) = 0
   irefs(:) = 0
!
   Return
 End Subroutine init_refdates
!
!
!
 Function check_refdates() &
          Result (check)
!
! Checks reference dates
!
! Modules
   Use time,     Only: pdate, &
                       Operator(==), Operator(<), Operator(>), Operator(+), &
                       iseq, &
                       date_diff
   Use settings, Only: nt
   Use iofiles,  Only: yfile
   Use missing,  Only: ktuse
!
! Function result
   Integer :: check
!
! Locals
!
! Local scalars
   Integer :: j  ! - threshold index -
   Integer :: iy ! - year index -
!
   Type(pdate) :: ydate2 ! - last date of training period -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Count
!
! Executable Statements
!
! Initialize reference dates
   check = 0
   ydate2 = yfile%fdate + (nt - 1)
   If (.not.refs(1) == 0) Then
      Do j = 1, nts
! - year is too early -
         If (refs(j) < yfile%fdate) Then
            iy = 1
            irefs(j) = 0
            Call find_iref (1)
            refs(j) = yfile%fdate + (iy - 1)
            Select Case (check)
             Case (0)
               check = j
             Case Default
               check = 3
            End Select
         Else If (refs(j) > ydate2) Then ! - year is too late -
            iy = nt
            irefs(j) = Count(ktuse(1:nt)) + 1
            Call find_iref (-1)
            refs(j) = yfile%fdate + (iy - 1)
            Select Case (check)
             Case (0)
               check = j
             Case Default
               check = 3
            End Select
         Else ! - year is within limits -
            irefs(j) = date_diff(yfile%fdate, refs(j), iseq) + 1
            If (.not.Allocated(ktuse)) Return
            If (ktuse(irefs(j))) Then ! - year is available -
               irefs(j) = Count(ktuse(1:irefs(j)))
            Else ! - identify nearest available year -
               Select Case (j)
                Case (1)
                  iy = 1
                  irefs(j) = 0
                  Call find_iref (1)
                Case (2)
                  iy = nt
                  irefs(j) = Count(ktuse(1:nt)) + 1
                  Call find_iref (-1)
               End Select
               refs(j) = yfile%fdate + (iy - 1)
               If (j == 1) Then
                  check = 4
               Else
                  Select Case (check)
                   Case (0)
                     check = 5
                   Case (4)
                     check = 6
                   Case Default
                     check = 3
                  End Select
               End If
            End If
         End If
      End Do
   Else
      iy = 0
      Do j = 1, nts
         iy = iy + 1
         Do
            If (ktuse(iy)) Exit
            iy = iy + 1
         End Do
         irefs(j) = j
         refs(j) = yfile%fdate + (iy - 1)
      End Do
   End If
!
   Return
!
 Contains
!
!
  Subroutine find_iref (iinc)
!
! Identifies next available reference
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iinc ! - increment -
!
! Executble Statements
!
! Identify next available reference
   Do
      Do
         If (ktuse(iy)) Exit
         iy = iy + iinc
      End Do
      irefs(j) = irefs(j) + iinc
      If (irefs(1) /= irefs(2)) Exit
   End Do
!
   Return
  End Subroutine find_iref
 End Function check_refdates
!
!
!
 Function init_categories(ianalc, icv2, iretro, m, n, nr) &
          Result (init)
!
! Initialises memory for categories
!
! Modules
   Use data_cpt_constants, Only: ia_pfv
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianalc ! - analysis identifier -
   Integer, Intent(In) :: icv2   ! - double cross-validation flag -
   Integer, Intent(In) :: iretro ! - retroactive verification flag -
   Integer, Intent(In) :: m      ! - total number of variables -
   Integer, Intent(In) :: n      ! - number of Cases -
   Integer, Intent(In) :: nr     ! - number of retroactive Cases -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - observed categories -
   If (ianalc /= ia_pfv) Then
      Allocate (iobs_c(m,n), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - hindcast categories -
      Allocate (ifor_c(m,n), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - double cross-validated observed categories -
   If (icv2 /= 0) Then
      Allocate (iobs_2(m,n), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - double cross-validated forecast categories -
      Allocate (ifor_2(m,n), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - retroactive observed categories -
   If (iretro == 1) Then
      Allocate (iobs_r(m,nr), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - retroactive forecast categories -
      Allocate (ifor_r(m,nr), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
!
   init = 0
   Return
!
! Errors
1  init = 1
   Return
 End Function init_categories
!
!
!
 Function init_thresholds(nsn) &
          Result (init)
!
! Initialises memory for thresholds
!
! Modules
   Use data_cpt_constants, Only: ng, nts
   Use settings,           Only: iensc
   Use iofiles,            Only: mya
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nsn ! - number of seasons -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - percentiles / climatological probabilities -
   Allocate (pobs(nsn*mya,ng), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - absolute thresholds -
   Allocate (tobs(nsn*mya,nts), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - transformed absolute thresholds -
   Allocate (tobst(nsn*mya,nts), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - model absolute thresholds -
   If (iensc > 1) Then
      Allocate (tobsx(nsn*mya,nts), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
!
   init = 0
   Return
!
! Errors
1  init = 1
!
   Return
 End Function init_thresholds
!
!
!
 Subroutine calc_thresholds (igauss, mya, ns, nc, clim, tobs, tobst, ifail)
!
! Sets climatological thresholds and percentiles
!
! Modules
   Use data_cpt_constants, Only: iv_ple, iv_abs, iv_ref
   
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igauss ! - transform Y data flag -
   Integer, Intent(In) :: mya    ! - number of available variables -
   Integer, Intent(In) :: ns     ! - number of seasons -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nc ! - length of climatological period -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: clim ! - climatological data -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: tobs  ! - climatological absolute thresholds -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: tobst ! - transformed climatological absolute thresholds -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Sets climatological thresholds and percentiles -
   Select Case (ithr)
    Case (iv_ple, iv_abs) ! - set thresholds by percentile, or by absolute value -
      ifail = set_percentiles(ns, nc(:), clim(:,:,:))
      If (ifail /= 0) Return
      Call set_thresholds(igauss, mya, 1, nc(:), clim(:,:,:), tobs, tobst, ifail)
      If (ifail /= 0) Return
    Case (iv_ref) ! - set thresholds by reference dates -
      Call set_thresholds(igauss, mya, 1, nc(:), clim(:,:,:), tobs, tobst, ifail)
      If (ifail /= 0) Return
      ifail = set_percentiles(ns, nc(:), clim(:,:,:))
      If (ifail /= 0) Return
   End Select
!
   Return
 End Subroutine calc_thresholds
!
!
!
 Subroutine set_thresholds (igauss, mya, ns, nc, clim, tobs, tobst, ifail)
!
! Sets category thresholds
!
! Modules
   Use data_numbers,       Only: zero, one
   Use data_cpt_constants, Only: it_non, iv_ple, iv_abs, iv_ref
   Use progress_meter,     Only: set_progress_increment, update_progress_meter
   Use arrays,             Only: y, &
                                 insertion_sort
   Use distribs,           Only: q_empirical, q_normal
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igauss ! - transform Y data flag -
   Integer, Intent(In) :: mya    ! - number of available variables -
   Integer, Intent(In) :: ns     ! - number of seasons -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nc ! - length of climatological period -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: clim ! - climatological data -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: tobs  ! - climatological absolute thresholds -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: tobst ! - transformed climatological absolute thresholds -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i   ! - variable index -
   Integer :: j   ! - category index -
   Integer :: isn ! - season index -
!
   Real(Kind=rp) :: sp ! - cumulative probability -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Set thresholds by percentile
   ifail=-1
   Select Case (ithr)
    Case (iv_ple)
      Do isn = 1, ns
         Do i = 1, mya
            Do j = 1, nts
               tobs((isn-1)*mya+i,j) = q_empirical(clim(i,:,isn), nc(isn), pthr(j))
            End Do
            If (update_progress_meter(.false., ifail=ifail) /= 0) Return
         End Do
      End Do
!
! Set thresholds by absolute value
    Case (iv_abs)
      Do i = 1, mya*ns
         tobs(i,:) = thr(:)
         If (update_progress_meter(.false., ifail=ifail) /= 0) Return
      End Do
!
! Set thresholds by reference dates
    Case (iv_ref)
      If (ns > 0) Then
         Do isn = 1, ns
            Do i = 1, mya
               Do j = 1, nts
                  tobs((isn-1)*mya+i,j) = y(i,irefs(j),isn)
               End Do
            End Do
         End Do
      Else
         Do i = 1, mya
            Do j = 1, nts
               tobs(i,j) = y(i,irefs(j),0)
            End Do
         End Do
      End If
      Call insertion_sort ('a', mya*ns, nts, tobs(:,:))
      Call set_progress_increment (Real(mya, Kind=rp), .true.)
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
      Call set_progress_increment (one/Real(mya, Kind=rp), .true.)
   End Select
!
! Transform thresholds
   Select Case (igauss)
    Case (it_non)
      tobst(:,:) = tobs(:,:)
    Case Default
      Do i = 1, mya*ns
         sp = zero
         Do j = 1, nts
            sp = sp + pobs(i,j)
            tobst(i,j) = q_normal(sp)
         End Do
      End Do
      If (update_progress_meter(.false., ifail=ifail) /= 0) Return
   End Select
   ifail = 0
!
   Return
 End Subroutine set_thresholds
!
!
!
 Function set_percentiles(nsn, nc, clim)
!
! Sets category thresholds as percentiles
!
! Modules
   Use data_numbers,       Only: zero, one
   Use data_cpt_constants, Only: iv_ple, iv_abs, iv_ref, ng, nts
   Use progress_meter,     Only: update_progress_meter
   Use settings,           Only: igauss
   Use iofiles,            Only: mya
   Use distribs,           Only: cdf_empirical, q_normal
!
! Function result
   Integer :: set_percentiles
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nsn ! - number of seasons -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nc ! - length of climatological period -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: clim ! - climatological data (unused by PFv) -
!
! Locals
!
! Local scalars
   Integer :: i   ! - variable index -
   Integer :: ii  ! - variable index -
   Integer :: j   ! - category index -
   Integer :: isn ! - season index -
!
   Real(Kind=rp) :: sp ! - cumulative probability -
!
! Executable Statements
!
! Set percentiles
   set_percentiles = -1
   Select Case (ithr)
    Case (iv_ple)
      ii = 0
      Do isn = 1, nsn
         Do i = 1, mya
            ii = ii + 1
            pobs(ii,1:nts) = pthr(:)
            pobs(ii,ng) = one
            Do j = ng, 2, -1
               pobs(ii,j) = pobs(ii,j) - pobs(ii,j-1)
            End Do
            If (update_progress_meter(.false.) /= 0) Return
         End Do
      End Do
!
! Set percentiles from absolute value
    Case (iv_abs)
      ii = 0
      Do isn = 1, nsn
         Do i = 1, mya
            ii = ii + 1
            Do j = 1, nts
               pobs(ii,j) = cdf_empirical(clim(ii,:,isn), nc(isn), thr(j))
            End Do
            pobs(ii,ng) = one
            Do j = ng, 2, -1
               pobs(ii,j) = pobs(ii,j) - pobs(ii,j-1)
            End Do
            If (update_progress_meter(.false.) /= 0) Return
         End Do
      End Do
!
! Set percentiles from reference dates
    Case (iv_ref)
      Do i = 1, mya
         Do j = 1, nts
            pobs(i,j) = cdf_empirical(clim(i,:,1), nc(1), tobs(i,j))
         End Do
         pobs(i,ng) = one
         Do j = ng, 2, -1
            pobs(i,j) = pobs(i,j) - pobs(i,j-1)
         End Do
         If (update_progress_meter(.false.) /= 0) Return
      End Do
      If (igauss > 0) Then
         Do i = 1, mya
            sp = zero
            Do j = 1, nts
               sp = sp + pobs(i,j)
               tobst(i,j) = q_normal(sp)
            End Do
            pobs(i,ng) = one
            Do j = ng, 2, -1
               pobs(i,j) = pobs(i,j) - pobs(i,j-1)
            End Do
         End Do
      End If
   End Select
   set_percentiles = 0
!
   Return
 End Function set_percentiles
!
!
!
 Function set_cv_categories() &
          Result (set)
!
! Calculate cross-validated categories
!
! Modules
   Use settings,   Only: hcw, ncv, nu
   Use arrays,     Only: y
   Use iofiles,    Only: mya
   Use regression, Only: ycv
!
! Function result
   Integer :: set
!
! Executable Statements
!
! Set categories by percentile
   Call set_categories (mya, nu, 1, ncv, hcw, y(1:mya,1:nu,:), ycv(:,1:nu,:), iobs_c(:,:), ifor_c(:,:))
   set = 0
!
   Return
 End Function set_cv_categories
!
!
!
 Function set_c2_categories() &
          Result (set)
!
! Calculate double cross-validated categories
!
! Modules
   Use settings,   Only: nu
   Use arrays,     Only: y
   Use iofiles,    Only: mya
   Use regression, Only: ycv2
!
! Function result
   Integer :: set
!
! Executable Statements
!
! Set categories by percentile
   Call set_categories (mya, nu, 1, nu-1, 1, y(1:mya,1:nu,:), ycv2(:,1:nu,:), iobs_2(:,:), ifor_2(:,:))
   set = 0
!
   Return
 End Function set_c2_categories
!
!
!
 Function set_ra_categories(ianal, nsn, is0, nc, clim) &
          Result (set)
!
! Calculate retroactive categories
!
! Modules
   Use settings,   Only: nu, nur, nu1
   Use arrays,     Only: y
   Use iofiles,    Only: mya
   Use regression, Only: yret
!
! Function result
   Integer :: set
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: nsn   ! - number of seasons -
   Integer, Intent(In) :: is0   ! - zeroth season -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nc ! - length of climatological period -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: clim ! - climatological data -
!
! Executable Statements
!
! Set categories by percentile for all but PFV
   Call set_categories (mya, nur, nu1+1, 0, 0, y(1:mya,1:nu,:), yret(:,:,:), iobs_r(:,:), ifor_r(:,:), &
        ianal=ianal, nsn=nsn, is0=is0, nc=nc(:), clim=clim(:,:,:))
   set = 0
!
   Return
 End Function set_ra_categories
!
!
!
 Subroutine set_categories(m, n, i1, ncv, hcw, y, yhat, iobs, ifor, &
            ianal, nsn, is0, nc, clim)
!
! Calculate categories
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: ia_pfv, iv_ple, iv_abs, iv_ref
   Use progress_meter,     Only: set_progress_increment, update_progress_meter
   Use settings,           Only: nret
   Use arrays,             Only: rwk, &
                                 insertion_sort
   Use season,             Only: isnc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m   ! - number of variables -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: i1  ! - index of first case -
   Integer, Intent(In) :: ncv ! - length of cross-validated training period -
   Integer, Intent(In) :: hcw ! - half cross-validation window -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: y    ! - data -
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: yhat ! - hindcasts -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: iobs ! - observed categories -
   Integer, Dimension(:,:), Intent(Out) :: ifor ! - forecast categories -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ianal ! - analysis identifier -
   Integer, Intent(In), Optional :: nsn   ! - number of seasons -
   Integer, Intent(In), Optional :: is0   ! - zeroth season -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: nc ! - length of climatological period -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In), Optional :: clim ! - climatological data -
!
! Locals
!
! Local scalars
   Integer :: i   ! - grid / station index -
   Integer :: j   ! - category / threshold index -
   Integer :: npm ! - progress meter adjustment -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Real
!
! Executable Statements
!
! Set categories by percentile
   Select Case (ithr)
    Case (iv_ple)
      If (.not.Present(ianal)) Then
         npm = n
      Else
         Select Case (ianal)
          Case Default
            npm = nret
          Case (ia_pfv)
            npm = n
         End Select
      End If
      Call set_progress_increment (one/Real(npm, Kind=rp), .true.)
      If (.not.Present(ianal)) Then ! - cross-validated -
         Call set_categories_cv (m, n, ncv, hcw, y(:,:,isnc), yhat(:,:,isnc), pthr(:), iobs(:,:), ifor(:,:))
      Else ! - retroactive -
         Call set_categories_ra (ianal, m, n, i1, is0, nsn, y(:,:,:), yhat(:,:,:), pthr(:), nc(:), clim(:,:,:), &
              iobs(:,:), ifor(:,:))
      End If
      Call set_progress_increment (Real(npm, Kind=rp), .true.)
!
! Set categories by absolute value
    Case (iv_abs)
      Call categorise_abs (m, n, nts,    y(:,i1:,isnc), thr(:), iobs(:,:)) ! - categorise observations -
      Call categorise_abs (m, n, nts, yhat(:,  :,isnc), thr(:), ifor(:,:)) ! - categorise forecasts -
!
! Set categories by reference dates
    Case (iv_ref)
      Do i = 1, m
         Do j = 1, nts
            rwk(j) = y(i,irefs(j),1)
         End Do
         Call insertion_sort ('a', nts, rwk(:))
         Call categorise_abs (1, n, nts,    y(i:i,i1:,isnc), rwk(:), iobs(:,:)) ! - categorise observations -
         Call categorise_abs (1, n, nts, yhat(i:i,  :,isnc), rwk(:), ifor(:,:)) ! - categorise cross-validated predictions -
      End Do
   End Select
   If (update_progress_meter(.false.) /= 0) Return
!
   Return
!
 Contains
!
!
  Subroutine set_categories_cv (m, n, ncv, hcw, y, ycv, pthr, iobs, ifor)
!
! Cross-validates the percentile-based categories
!
! Modules
   Use progress_meter, Only: update_progress_meter
   Use arrays,         Only: rwk2, &
                             get_cvsample, insertion_sort
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m   ! - number of variables -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: ncv ! - length of cross-validated training period -
   Integer, Intent(In) :: hcw ! - half cross-validation window -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: pthr ! - percentile thresholds -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y   ! - data -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: ycv ! - cross-validated hindcasts -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: iobs ! - observed categories -
   Integer, Dimension(:,:), Intent(Out) :: ifor ! - forecast categories -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: k ! - time index -
!
! Exectutable Statements
!
! Cross-validate the percentile-based categories
   Do k = 1, n
      Call get_cvsample (hcw, k, y(:,:), rwk2(1:m,1:n))
      Do i = 1, m
         Call insertion_sort ('a', ncv, rwk2(i,:))
      End Do
! - categorise observations -
      Call categorise_per (m, 1, nts,   y(:,k:k), pthr(:), ncv, rwk2(1:m,1:ncv), iobs(:,k:k))
! - categorise cross-validated predictions -
      Call categorise_per (m, 1, nts, ycv(:,k:k), pthr(:), ncv, rwk2(1:m,1:ncv), ifor(:,k:k))
! - update progress meter -
      If (update_progress_meter(.false.) /= 0) Return
   End Do
!
   Return
  End Subroutine set_categories_cv
!
!
!
  Subroutine set_categories_ra(ianal, m, n, i1, is0, nsn, y, yret, pthr, nc, clim, iobs, ifor)
!
! Calculates retroactive percentile-based categories
!
! Modules
   Use data_numbers,       Only: zero
   Use data_cpt_constants, Only: ia_pfv
   Use progress_meter,     Only: update_progress_meter
   Use settings,           Only: nretro, nt, ntr, nt1, nyr
   Use arrays,             Only: rwk2, &
                                 insertion_sort
   Use season,             Only: isnc
   Use missing,            Only: ktuse
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis identifier -
   Integer, Intent(In) :: m     ! - number of variables -
   Integer, Intent(In) :: n     ! - number of cases -
   Integer, Intent(In) :: i1    ! - index of first case -
   Integer, Intent(In) :: nsn   ! - number of seasons -
   Integer, Intent(In) :: is0   ! - zeroth season -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nc ! - length of climatological period -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: pthr ! - percentile thresholds -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: clim ! - climatological data -
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: y    ! - data -
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: yret ! - retroactive hindcasts -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: iobs ! - observed categories -
   Integer, Dimension(:,:), Intent(Out) :: ifor ! - forecast categories -
!
! Locals
!
! Local scalars
   Integer :: i   ! - grid / station index -
   Integer :: j   ! - category / threshold index -
   Integer :: k   ! - Case index -
   Integer :: irl ! - index of last retroactive training-period date -
   Integer :: ir1 ! - index of first retroactive date -
   Integer :: irn ! - index of last retroactive date -
   Integer :: ir0 ! - index of first new date in retroactive training period -
   Integer :: isn ! - number of seasons -
   Integer :: isi ! - last season index -
   Integer :: is1 ! - season index -
   Integer :: nr  ! - total number of retroaoctive steps in current cycle -
   Integer :: nrt ! - length of training period in current retroactive cycle -
!
   Real(Kind=rp) :: fmax ! - maximum forecast probability -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Min, Sum
!
! Exectutable Statements
!
! Calculate retroactive percentile-based categories
   Select Case (ianal)
    Case Default
! - set retroactive indices -
      ir1 = 1
      irn = 0
      ir0 = 1
      nrt = i1 - 1
      retro: Do irl = nt1, nt-1, nretro
         nr = Count(ktuse(irl+1:Min(irl+nretro, nt))) ! - count number of available cases in current retroactive cycle -
         If (nr > 0) Then
            irn = irn + nr
            rwk2(1:m,ir0:nrt) = y(1:m,ir0:nrt,isnc) ! - append additional climatology -
            Do i = 1, m
               Call insertion_sort ('a', nrt, rwk2(i,:))
            End Do
! - categorise observations -
            Call categorise_per (m, nr, nts,    y(:,nrt+1:nrt+nr,isnc), pthr(:), nrt, rwk2(1:m,1:nrt), iobs(:,ir1:irn))
! - categorise forecasts -
            Call categorise_per (m, nr, nts, yret(:,  ir1:irn,   isnc), pthr(:), nrt, rwk2(1:m,1:nrt), ifor(:,ir1:irn))
         End If
! - update indices -
         ir1 = irn + 1
         ir0 = nrt + 1
         nrt = nrt + nr
! - update progress meter -
         If (update_progress_meter(.false.) /= 0) Return
      End Do retro
!
! Set categories for PFV
    Case (ia_pfv)
! - categorise observations -
      Do is1 = 1, nsn
         If (is1 > ntr) Exit
         isn = is0 + is1
         If (isn > nsn) isn = isn - nsn
         isi = nsn*(nyr - 1) + is1
         If (isi <= ntr) Then
            Call categorise_per (m, nyr, nts, y(:,1:nyr,isn), pthr(:), nc(isn), clim(:,1:nc(isn),isn), iobs(:,is1:isi:nsn))
         Else
            isi = isi - nsn
            Call categorise_per (m, nyr-1, nts, y(:,1:nyr-1,isn), pthr(:), nc(isn), clim(:,1:nc(isn),isn), iobs(:,is1:isi:nsn))
         End If
      End Do
! - identify category with highest probability -
      Do i = 1, m
         Do k = 1, n
            If (Sum(fps_r(i,k,:)) > zero) Then
               ifor(i,k) = 1
               fmax = fps_r(i,k,1)
               Do j = 2, ng
                  If (fps_r(i,k,j) > fmax) Then
                     ifor(i,k) = j
                     fmax = fps_r(i,k,j)
                  End If
               End Do
            Else
               ifor(i,k) = 0
            End If
! - update progress meter -
            If (update_progress_meter(.false.) /= 0) Return
         End Do
      End Do
   End Select
!
   Return
  End Subroutine set_categories_ra
 End Subroutine set_categories
!
!
!
 Subroutine categorise_per (m, n, nts, x, p, nc, c, icat)
!
! Categorises data given percentile thresholds
!
! Modules
   Use distribs, Only: q_empirical
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m   ! - number of variables -
   Integer, Intent(In) :: n   ! - number of Cases -
   Integer, Intent(In) :: nts ! - number of thresholds -
   Integer, Intent(In) :: nc  ! - length of reference climatology -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: p ! - percentiles -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - data to be categorised -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: c ! - reference climatology (sorted in ascending order) -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: icat ! - categories -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: j ! - category index -
!
! Local arrays
   Real(Kind=rp), Dimension(nts) :: t ! - thresholds -
!
! Executable Statements
!
! Set thresholds
   Do i = 1, m
      Do j = 1, nts
         t(j) = q_empirical(c(i,:), nc, p(j))
      End Do
!
! Determine category
      Call categorise_abs (1, n, nts, x(i:i,:), t(:), icat(i:i,:))
   End Do
!
   Return
 End Subroutine categorise_per
!
!
!
 Subroutine categorise_abs (m, n, nts, x, t, icat)
!
! Categorises data given threshold values
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m   ! - number of variables -
   Integer, Intent(In) :: n   ! - number of Cases -
   Integer, Intent(In) :: nts ! - number of thresholds -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: t ! - absolute thresholds -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - data to be categorised -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: icat ! - categories -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: j ! - category index -
   Integer :: k ! - Case index -
!
! Executable Statements
!
! Determine category
   Do i = 1, m
      Do k = 1, n
         icat(i,k)=1
         Do j = 1, nts
            If (t(j) > x(i,k)) Exit
            icat(i,k) = icat(i,k) + 1
         End Do
      End Do
   End Do
!
   Return
 End Subroutine categorise_abs
!
!
!
 Subroutine calc_odds (nf, ng, nv, pobs, fps, odds)
!
! Calculates odds and relative odds
!
! Modules
   Use data_numbers, Only: zero, one, oneh, eps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf ! - number of forecasts -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: nv ! - number of variables -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: pobs ! - climatological probabilities -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:,:,:), Intent(Out) :: odds ! - odds -
!
! Locals
!
! Local scalars
   Integer :: i ! - variable index -
   Integer :: j ! - category index -
   Integer :: k ! - forecast index -
!
! Local arrays
   Real(Kind=rp), Dimension(ng) :: codds ! - climatological odds -
!
! Executable Statements
!
! Calculate odds
   Do i = 1, nv
      Where (pobs(i,:) > zero)
         codds(:) = (one - pobs(i,:))/pobs(i,:) ! - inverted, to calculate relative odds by multiplication rather than division -
      Else Where
         codds(:) = zero
      End Where
      Do k = 1, nf
         Do j = 1, ng
            If (fps(i,k,j) > eps .and. fps(i,k,j) < oneh - eps) Then
               odds(i,k,j,1) = fps(i,k,j)/(oneh - fps(i,k,j))
            Else
               odds(i,k,j,1) = -one
            End If
!
! Calculate odds relative to climatology
            If (odds(i,k,j,1) > -one .and. codds(j) > zero) Then
               odds(i,k,j,2) = odds(i,k,j,1)*codds(j)
            Else
               odds(i,k,j,2) = -one
            End If
         End Do
      End Do
   End Do
!
   Return
 End Subroutine calc_odds
!
!
!
 Function change_cat() &
          Result (change)
!
! Changes category
!
! Modules
   Use settings, Only: change_setting
!
! Function result
   Integer :: change
!
! Executable Statements
!
! Change category
   change = change_setting(icatc, icatp, 1, ng)
#if Gui == 0
   change = 2
#endif
!
   Return
 End Function change_cat
!
!
!
 Subroutine proj_read_categories (iin, ihind, icv2, iretro, nsn, lpfv, lres, ifail)
!
! Reads category settings from project file
!
! Modules
   Use time, Only: Operator(+)
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin    ! - input file unit number -
   Integer, Intent(In) :: ihind  ! - hindcasts flag -
   Integer, Intent(In) :: icv2   ! - double cross-validation flag -
   Integer, Intent(In) :: iretro ! - retroactive forecast flag -
   Integer, Intent(In) :: nsn    ! - number of seasons -
!
   Logical, Intent(In) :: lpfv ! - PFV? -
   Logical, Intent(In) :: lres ! - read results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read category settings
   ithr_bk = ithr
   If (.not.lres) Then
      Read (Unit=iin, Err=1, End=2) refs, irefs, ithr, pthr, thr
      If (lpfv) ithr_bk = ithr
!
! Read results
   Else
      If (init_thresholds(nsn) /= 0) GoTo 4
      Read (Unit=iin, Err=1, End=2) pobs
      Read (Unit=iin, Err=1, End=2) tobs
      Read (Unit=iin, Err=1, End=2) tobst
      If (ihind == 1) Then
         Read (Unit=iin, Err=1, End=2) ifor_c
         Read (Unit=iin, Err=1, End=2) iobs_c
      End If
      If (icv2 /= 0) Then
         Read (Unit=iin, Err=1, End=2) iobs_2
         Read (Unit=iin, Err=1, End=2) ifor_2
         Read (Unit=iin, Err=1, End=2) fps_2
         Read (Unit=iin, Err=1, End=2) odds_2(:,:,:,2)
         Read (Unit=iin, Err=1, End=2) odds_2(:,:,:,1)
      End If
      If (iretro == 1) Then
         Read (Unit=iin, Err=1, End=2) iobs_r
         Read (Unit=iin, Err=1, End=2) ifor_r
         Read (Unit=iin, Err=1, End=2) fps_r
         Read (Unit=iin, Err=1, End=2) odds_r(:,:,:,2)
         Read (Unit=iin, Err=1, End=2) odds_r(:,:,:,1)
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
! - End of file -
2  ifail = 2
   Return
!
! - problem allocating memory -
4  ifail = 4
   Return
!
 End Subroutine proj_read_categories
!
!
!
 Subroutine proj_write_categories (iout, ihind, icv2, iretro, lres, ifail)
!
! Writes category settings to project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout   ! - output file unit number -
   Integer, Intent(In) :: ihind  ! - hindcasts flag -
   Integer, Intent(In) :: icv2   ! - double cross-validation flag -
   Integer, Intent(In) :: iretro ! - retroactive forecast flag -
!
   Logical, Intent(In) :: lres ! - read results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write category settings
   If (.not.lres) Then
      Write (Unit=iout, Err=1) refs, irefs, ithr, pthr, thr
!
! Read results
   Else
      Write (Unit=iout, Err=1) pobs
      Write (Unit=iout, Err=1) tobs
      Write (Unit=iout, Err=1) tobst
      If (ihind == 1) Then
         Write (Unit=iout, Err=1) ifor_c
         Write (Unit=iout, Err=1) iobs_c
      End If
      If (icv2 /= 0) Then
         Write (Unit=iout, Err=1) iobs_2
         Write (Unit=iout, Err=1) ifor_2
         Write (Unit=iout, Err=1) fps_2
         Write (Unit=iout, Err=1) odds_2(:,:,:,2)
         Write (Unit=iout, Err=1) odds_2(:,:,:,1)
      End If
      If (iretro == 1) Then
         Write (Unit=iout, Err=1) iobs_r
         Write (Unit=iout, Err=1) ifor_r
         Write (Unit=iout, Err=1) fps_r
         Write (Unit=iout, Err=1) odds_r(:,:,:,2)
         Write (Unit=iout, Err=1) odds_r(:,:,:,1)
      End If
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
 End Subroutine proj_write_categories
!
!
!
 Subroutine parameter_write_categories (iout, ifail)
!
! Writes program settings to parameter file
!
! Modules
   Use data_numbers,       Only: one
   Use data_cpt_constants, Only: ir_ols, ng, nts
   Use labels,             Only: ca_thrs, cg_ref, &
                                 l_stdn, ls_cats, ls_threshs
   Use time,               Only: iseq
   Use menus,              Only: mn_opts_tailor
   Use parameter_file,     Only: output_parameter
   Use settings,           Only: iregr, istd
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
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Write project settings
! - tailoring -
   If (mn_opts_tailor%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_tailor%l_lab%c), i=mn_opts_tailor%id) /= 0) GoTo 1
      If (iregr == ir_ols) Then
         If (output_parameter(iout, & ! - standardization -
                              cdesc=Trim(l_stdn%c), i=istd) /= 0) GoTo 1
      End If
      If (output_parameter(iout, &
                           cdesc=Trim(ca_thrs(ithr)), i=ithr) /= 0) GoTo 1
      Select Case (ithr)
       Case (1) ! - percentile thresholds -
         If (output_parameter(iout, &
                              cdesc=Trim(ls_cats(ng)%c), r=one-pthr(2)) /= 0) GoTo 1
         If (output_parameter(iout, &
                              cdesc=Trim(ls_cats(1)%c), r=pthr(1)) /= 0) GoTo 1

       Case (2) ! - absolute thresholds -
         If (output_parameter(iout, &
                              cdesc=Trim(ls_threshs(nts)%c), r=thr(2)) /= 0) GoTo 1
         If (output_parameter(iout, &
                              cdesc=Trim(ls_threshs(1)%c), r=thr(1)) /= 0) GoTo 1
       Case (3) ! - reference years -
         Select Case (iseq)
          Case Default
            If (output_parameter(iout, &
                                 cdesc=Trim(cg_ref), i=refs(nts)%iyr) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 cdesc=Trim(cg_ref), i=refs(1)%iyr) /= 0) GoTo 1
          Case (1:)
            If (output_parameter(iout, &
                                 i=refs(nts)%idy) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 i=refs(nts)%imn) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 i=refs(nts)%iyr) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 i=refs(1)%idy) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 i=refs(1)%imn) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 i=refs(1)%iyr) /= 0) GoTo 1
         End Select
      End Select
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
 End Subroutine parameter_write_categories
!
!
!
 Function close_categories() &
          Result (fclose)
!
! Frees memory allocated for categories
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
! Free memory allocated for categories
   If (Allocated(ifor_r)) Deallocate (ifor_r)
   If (Allocated(iobs_r)) Deallocate (iobs_r)
   If (Allocated(ifor_2)) Deallocate (ifor_2)
   If (Allocated(iobs_2)) Deallocate (iobs_2)
   If (Allocated(ifor_c)) Deallocate (ifor_c)
   If (Allocated(iobs_c)) Deallocate (iobs_c)
   If (Allocated(tobsx))  Deallocate (tobsx)
   If (Allocated(tobst))  Deallocate (tobst)
   If (Allocated(tobs))   Deallocate (tobs)
   If (Allocated(pobs))   Deallocate (pobs)
   fclose = 0
!
   Return
 End Function close_categories
End Module categories
