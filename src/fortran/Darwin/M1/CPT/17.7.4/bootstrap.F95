! Author: Simon Mason
Module bootstrap
!
! Declarations
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: mps, nscore
#if GUI == 1
   Use data_gui_constants, Only: i_handle
#endif
   Use maths,              Only: interval
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: boot_pscores, bootstrap_cv, bootstrap_c2, bootstrap_ra, close_boot, close_pboot, get_pvalues, init_pboot, &
             parameter_write_boot, proj_read_bootstrap, proj_write_bootstrap, reset_bootstrap
!
! Scalars
!
! Integer scalars
   Integer, Public :: ibcl  ! - calculate bootstrap confidence limits flag -
   Integer, Public :: ipval ! - p-values calculation flag -
   Integer, Public :: nboot ! - size of bootstrap sample -
   Integer, Public :: nperm ! - size of permutation sample -
!
   Integer, Private :: ioutb0 ! - bootstrap output unit number -
   Integer, Private :: ioutb1 ! - bootstrap output unit number -
   Integer, Private :: nbw    ! - number of bootstrap working samples -
!
#if GUI == 1
   Integer(Kind=i_handle), Private :: ih_b   ! - bootstrap window handle -
!
#endif
! Real scalars
   Real(Kind=rp), Public :: clb ! - bootstrap confidence level -
!
! Derived type scalars
   Type(interval), Private :: pcls ! - bootstrap confidence limits percentiles -
!
! Arrays
!
! Integer arrays
   Integer, Dimension(:), Allocatable, Private :: indices ! - permutation indices -
   Integer, Dimension(:), Allocatable, Private :: ixboot  ! - bootstrap sample -
   Integer, Dimension(:), Allocatable, Private :: iyboot  ! - bootstrap sample -
!
   Integer, Dimension(:,:), Allocatable, Private :: ipboot ! - bootstrap sample -
   Integer, Dimension(:,:), Allocatable, Private :: iperm  ! - p-value permutation sample -
!
! Real arrays
   Real(Kind=rp), Dimension(nscore), Private :: pval ! - p-values -
!
   Real(Kind=rp), Dimension(:), Allocatable, Private :: bootc ! - current bootstrapped scores -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: perm  ! - permuted scores -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: xboot ! - bootstrap sample -
   Real(Kind=rp), Dimension(:), Allocatable, Private :: yboot ! - bootstrap sample -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Private :: yperm ! - p-value permutation sample -
!
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Private :: pboot ! - bootstrap probabilities sample -
!
! Derived type arrays
   Type(interval), Dimension(nscore), Private :: boot_cls  ! - bootstrap confidence limits -
!
   Type(interval), Dimension(   mps), Public :: pboot_cls ! - bootstrap confidence limits -
!
   Type(interval), Dimension(:,:), Allocatable, Private :: boot ! - bootstrapped scores -
!
Contains
!
!
 Subroutine reset_bootstrap ()
!
! Resets bootrsapping options
!
! Executable Statements
!
! Reset bootstrap settings
   ibcl = 0
   ipval = 0
!
   Return
 End Subroutine reset_bootstrap
!
!
!
 Function init_boot(n) &
          Result (init)
!
! Initialises CPT for permutations and bootstrapping
!
! Modules
   Use data_numbers, Only: zero, one, two, oneh
   Use errors,       Only: cpt_error
   Use settings,     Only: nu
   Use scores,       Only: init_dscores
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! Locals
!
! Local scalars
   Integer :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Ceiling, Real
!
! Executable Statements
!
! Free memory
   If (Allocated(bootc))   Deallocate (bootc)
   If (Allocated(boot))    Deallocate (boot)
   If (Allocated(xboot))   Deallocate (xboot)
   If (Allocated(yboot))   Deallocate (yboot)
   If (Allocated(ixboot))  Deallocate (ixboot)
   If (Allocated(iyboot))  Deallocate (iyboot)
   If (Allocated(indices)) Deallocate (indices)
!
! Initialise memory for validation
   If(init_dscores(nu) /= 0) GoTo 1
!
! Initialise data space
! - permutation indices -
   Allocate (indices(n), Stat=istat)
   If (istat /= 0) GoTo 1
! - bootstrap sample -
   Allocate (ixboot(n), Stat=istat)
   If (istat /= 0) GoTo 1
   Allocate (iyboot(n), Stat=istat)
   If (istat /= 0) GoTo 1
   Allocate (xboot(n), Stat=istat)
   If (istat /= 0) GoTo 1
   Allocate (yboot(n), Stat=istat)
   If (istat /= 0) GoTo 1
! - bootstrapped scores -
   nbw = Ceiling(Real(nboot, Kind=rp)*(one - clb/oneh)/two) + 1
   Allocate (boot(nbw,nscore), Stat=istat)
   If (istat /= 0) GoTo 1
! - current bootstrapped scores -
   Allocate (bootc(nscore), Stat=istat)
   If (istat /= 0) GoTo 1
!
! Calculate confidence limit percentiles
   pcls%lower = (one - clb/oneh)/two
   pcls%upper = one - pcls%lower
!
! Initialize
   boot_cls(:)%lower = zero
   boot_cls(:)%upper = zero
   pval(:) = zero
   init = 0
   Return
!
! Errors
1  init = close_boot()
   init = 1
   Call cpt_error ('init_boot', .false., init)
!
   Return
 End Function init_boot
!
!
!
 Function bootstrap_cv()
!
! Prepares for bootstrapping of cross-validated scores
!
! Modules
   Use settings, Only: nu
!
! Function result
   Integer :: bootstrap_cv
!
! Executable Statements
!
! Bootstrap cross-validated scores
   bootstrap_cv = bootstraps('Cross-validated scores', nu, boots_cv)
!
   Return
 End Function bootstrap_cv
!
!
!
 Function bootstrap_c2()
!
! Prepares for bootstrapping of double cross-validated scores
!
! Modules
   Use settings, Only: nu
!
! Function result
   Integer :: bootstrap_c2
!
! Executable Statements
!
! Bootstrap cross-validated scores
   bootstrap_c2 = bootstraps('Double cross-validated scores', nu, boots_c2)
!
   Return
 End Function bootstrap_c2
!
!
!
 Function bootstrap_ra()
!
! Prepares for bootstrapping of retroactive scores
!
! Modules
   Use settings, Only: nur
!
! Function result
   Integer :: bootstrap_ra
!
! Executable Statements
!
! Bootstrap retroactive scores
   bootstrap_ra = bootstraps('Retroactive scores', nur, boots_ra)
!
   Return
 End Function bootstrap_ra
!
!
!
 Function bootstraps(title, n, boots)
!
! Creates window for bootstrapping results
!
! Modules
   Use gui,         Only: set_cursor_waiting
#if GUI == 1
   Use windows_gui, Only: init_window_graphic_0
#else
   Use gui,         Only: get_output_unit, init_win
   Use cpt_output,  Only: print_results
#endif
!
! Function result
   Integer :: bootstraps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Character(Len=*), Intent(In) :: title ! - window title -
!
! - procedure arguments -
   Integer, External :: boots
!
! Executable Statements
!
! Initialise bootstrapping
   Call set_cursor_waiting (1)
   bootstraps = 1
   If (init_boot(n) /= 0) Return
!
#if GUI == 1
! Open results window
   bootstraps = init_window_graphic_0(title, boots, show_boot, &
                close_win1=close_boot, cxy='Y', iloc=ioutb0)
#else
   Call init_win (title=title)
   ioutb0 = get_output_unit()
   bootstraps = show_boot()
!
   bootstraps = print_results(boots)
#endif
!
   Return
 End Function bootstraps
!
!
!
 Function show_boot() &
          Result (show)
!
! Creates ClearWin+ window for bootstrapping results
!
! Modules
   Use data_cpt_constants, Only: nscore
   Use gui,                Only: open_clearwin
   Use scores,             Only: get_width_dscores
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: iwid ! - width of window -
!
! Executable Statements
!
! Create results window
   iwid = get_width_dscores(lbcls=.true., lpval=.true.)
#if GUI == 1
   Call open_clearwin (iwid, nscore+6, .true., ioutb1, &
        ih=ih_b)
#else
   Call open_clearwin (iwid, nscore+6, .true., ioutb1)
#endif
   show = 1
!
   Return
 End Function show_boot
!
!
!
 Function boots_cv()
!
! Prepares for calculation of results from cross-validated predictions
!
! Modules
   Use settings,   Only: iva, nu
   Use arrays,     Only: y
   Use regression, Only: ycv
   Use season,     Only: isnc
   Use categories, Only: ifor_c, iobs_c, pobs
!
! Function result
   Integer :: boots_cv
!
! Executable Statements
!
! Calculate validation statistics
   boots_cv = update_boots(nu, y(iva,:,1), ycv(iva,:,isnc), iobs_c(iva,:), ifor_c(iva,:), pobs(iva,:))
!
   Return
 End Function boots_cv
!
!
!
 Function boots_c2()
!
! Prepares for calculation of results from cross-validated predictions
!
! Modules
   Use settings,   Only: iva, nu
   Use arrays,     Only: y
   Use regression, Only: ycv2
   Use season,     Only: isnc
   Use categories, Only: ifor_2, iobs_2, pobs
!
! Function result
   Integer :: boots_c2
!
! Executable Statements
!
! Calculate validation statistics
   boots_c2 = update_boots(nu, y(iva,:,1), ycv2(iva,:,isnc), iobs_2(iva,:), ifor_2(iva,:), pobs(iva,:))
!
   Return
 End Function boots_c2
!
!
!
 Function boots_ra()
!
! Prepares for calculation of results from retroactive predictions
!
! Modules
   Use settings,   Only: iva, nu, nur, nu1
   Use arrays,     Only: y
   Use regression, Only: yret
   Use season,     Only: isnc
   Use categories, Only: ifor_r, iobs_r, pobs
!
! Function result
   Integer :: boots_ra
!
! Executable Statements
!
! Calculate validation statistics
   boots_ra = update_boots(nur, y(iva,nu1+1:nu,isnc), yret(iva,:,isnc), iobs_r(iva,:), ifor_r(iva,:), pobs(iva,:))
!
   Return
 End Function boots_ra
!
!
!
 Function update_boots(n, y, yhat, iobs, ifor, pobs)
!
! Calculates results for a new location
!
! Modules
#if GUI == 1
   Use clrwin$,        Only: clear_window$
#endif
   Use data_numbers,   Only: one
   Use progress_meter, Only: end_progress_meter, set_progress_increment, start_progress_meter
   Use fields,         Only: update_grid
   Use scores,         Only: score, &
                             print_dscores
!
! Function result
   Integer :: update_boots
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observed categories -
   Integer, Dimension(:), Intent(In) :: ifor ! - predicted categories -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: y    ! - observations -
   Real(Kind=rp), Dimension(:), Intent(In) :: yhat ! - predictions -
   Real(Kind=rp), Dimension(:), Intent(In) :: pobs ! - observed percentile thresholds -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Logical :: lpdone ! - print "done"? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Initialise progress meter
   update_boots = 1
   Call start_progress_meter ('Calculating bootstrap statistics and p-values', .true.)
   Call set_progress_increment (one/Real(1+nboot+nscore+nperm, Kind=rp), .false.)
!
! Update coordinate label for current point
   Call update_grid (ioutb0)
!
! Calculate validation statistics
   Call calc_boots (n, y(:), yhat(:), iobs(:), ifor(:), pobs(:), ifail)
   lpdone = (ifail == 0)
   If (lpdone) Then
!
! Print results
#if GUI == 1
      Call clear_window$ (ih_b)
#endif
      Call print_dscores (ioutb1, ioutb1, .true., score(:), &
           clb=clb, bcls=boot_cls(:), pvals=pval(:))
!
! Update progress meter
   End If
   Call end_progress_meter (.true., lpdone)
!
   Return
 End Function update_boots
!
!
!
 Subroutine calc_boots (n, y, yhat, iobs, ifor, clim, ifail)
!
! Calculates bootstrapped scores
!
! Modules
   Use data_numbers,        Only: zero, one, bignum
   Use data_cpt_constants,  Only: lposcore, nscore
   Use progress_meter,      Only: update_progress_meter
   Use arrays,              Only: rank_data
   Use distribs,            Only: q_empirical
   Use scores,              Only: rnko, score, &
                                  calc_scores
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iobs ! - observational thresholds -
   Integer, Dimension(:), Intent(In) :: ifor ! - forecast thresholds -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: y    ! - observed values -
   Real(Kind=rp), Dimension(:), Intent(In) :: yhat ! - forecast values -
   Real(Kind=rp), Dimension(:), Intent(In) :: clim ! - climatological probabilities -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i ! - score index -
   Integer :: j ! - bootstrap index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Calculate sample statistics
   Call calc_scores (n, yhat(:), y(:), ifor(:), iobs(:), clim(:), score(:), ifail)
   If (ifail /= 0) Return
!
! Calculate bootstrap statistics
   boot(:,:)%lower = bignum
   boot(:,:)%upper = -bignum
   Do j = 1, nboot
      Call get_boot (n, yhat(:), y(:), ifor(:), iobs(:), xboot(:), yboot(:), ixboot(:), iyboot(:))
      Call calc_scores (n, xboot(:), yboot(:), ixboot(:), iyboot(:), clim(:), bootc(:), ifail)
      If (ifail /= 0) Return
      Do i = 1, nscore
         Call adjust_boot (nbw, bootc(i), boot(:,i))
      End Do
      If (update_progress_meter(.true., ifail=ifail) /= 0) GoTo 1
   End Do
!
! Calculate bootstrap confidence limits
   Do i = 1, nscore
      boot_cls(i)%lower = q_empirical(boot(:,i)%lower, nboot, pcls%lower)
      boot_cls(i)%upper = q_empirical(boot(:,i)%upper, nboot, pcls%lower)
      If (update_progress_meter(.true., ifail=ifail) /= 0) GoTo 1
   End Do
!
! Calculate p-values
   Call rank_data ('a', n, y(:), rnko(:))
   pval(:) = zero
   Do j = 1, nperm
      Call get_perm1 (n, yhat(:), xboot(:), ifor(:), ixboot(:))
      Call calc_scores (n, xboot(:), y(:), ixboot(:), iobs(:), clim(:), bootc(:), ifail, &
           ornk=rnko(:))
      If (ifail /= 0) Return
      Do i = 1, nscore
         If (lposcore(i)) Then
            If (bootc(i) < score(i)) pval(i) = pval(i) + one
         Else
            If (bootc(i) > score(i)) pval(i) = pval(i) + one
         End If
      End Do
      If (update_progress_meter(.true., ifail=ifail) /= 0) GoTo 1
   End Do
   pval(:) = one - pval(:)/Real(nperm, Kind=rp)
!
   ifail = 0
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine calc_boots
!
!
!
 Subroutine init_pval (n, m, ifail)
!
! Initialises CPT for calculating p-values
!
! Modules
   Use errors, Only: cpt_error
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of locations -
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
! Free memory
   If (Allocated(perm))    Deallocate (perm)
   If (Allocated(yperm))   Deallocate (yperm)
   If (Allocated(iperm))   Deallocate (iperm)
   If (Allocated(indices)) Deallocate (indices)
!
! Initialise data space
! - permutation indices -
   Allocate (indices(n), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - permutation sample -
   Allocate (iperm(m,n), Stat=ifail)
   If (ifail /= 0) GoTo 1
   Allocate (yperm(m,n), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - permuted scores -
   Allocate (perm(m), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   Return
!
! Errors
1  ifail = close_pval()
   ifail = 1
   Call cpt_error ('init_pval', .false., ifail)
!
   Return
 End Subroutine init_pval
!
!
!
 Subroutine get_pvalues (iskill, n, m, x, y, ifor, iobs, clim, skills, pvalues)
!
! Performs permutations
!
! Modules
   Use data_numbers,   Only: zero, one
   Use progress_meter, Only: end_progress_meter, set_progress_increment, start_progress_meter, update_progress_meter
   Use scores,         Only: get_scores
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iskill ! - skill metric identifier -
   Integer, Intent(In) :: n      ! - total number of cases -
   Integer, Intent(In) :: m      ! - total number of gridpoints -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories -
   Integer, Dimension(:,:), Intent(In) :: ifor ! - forecast categories -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: skills ! - skill values -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x    ! - forecasts -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y    ! - observations -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: clim ! - climatological probabilities -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: pvalues ! - p-values -
!
! Locals
!
! Local scalars
   Integer :: j     ! - permutation index -
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Initialise
   Call init_pval (n, m, ifail)
   If (ifail /= 0) Return
!
! Initialise progress meter
   Call start_progress_meter ('Calculating p-values', .true.)
   Call set_progress_increment (one/Real(1+nperm, Kind=rp), .false.)
!
! Calculate permuted statistics
   pvalues(:) = zero
   Do j = 1, nperm
      Call get_perm2 (n, m, y(:,:), yperm(:,:), ifor(:,:), iperm(:,:))
      Call get_scores (iskill, n, m, x, yperm, iperm, iobs, clim, perm(:))
      Where (perm(:) < skills(:)) pvalues(:) = pvalues(:) + one
      If (update_progress_meter(.true.) /= 0) Return
   End Do
   pvalues(:) = one - pvalues(:)/Real(nperm, Kind=rp)
!
! Update progress meter
   Call end_progress_meter (.true., .true.)
   ifail = close_pval()
!
   Return
 End Subroutine get_pvalues
!
!
!
 Function init_pboot(m, n, ng) &
          Result (init)
!
! Initialises CPT for bootstrapping
!
! Modules
   Use data_numbers, Only: zero, one, two, oneh
   Use errors,       Only: cpt_error
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m  ! - number of variables -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: ng ! - number of categories -
!
! Locals
!
! Local scalars
   Integer :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Ceiling, Real
!
! Executable Statements
!
! Free memory
   If (Allocated(bootc))  Deallocate (bootc)
   If (Allocated(boot))   Deallocate (boot)
   If (Allocated(pboot))  Deallocate (pboot)
   If (Allocated(ipboot)) Deallocate (ipboot)
!
! Initialise data space
! - bootstrap sample -
   Allocate (ipboot(m,n), Stat=istat)
   If (istat /= 0) GoTo 1
   Allocate (pboot(m,n,ng), Stat=istat)
   If (istat /= 0) GoTo 1
! - bootstrapped scores -
   nbw = Ceiling(Real(nboot, Kind=rp)*(one - clb/oneh)/two)
   Allocate (boot(nbw,mps), Stat=istat)
   If (istat /= 0) GoTo 1
! - current bootstrapped scores -
   Allocate (bootc(mps), Stat=istat)
   If (istat /= 0) GoTo 1
!
! Calculate confidence limit percentiles
   pcls%lower = (one - clb/oneh)/two
   pcls%upper = one - pcls%lower
!
! Initialise
   pboot_cls(:)%lower = zero
   pboot_cls(:)%upper = zero
   init = 0
   Return
!
! Errors
1  init = close_pboot()
   init = 1
   Call cpt_error ('init_pboot', .false., init)
!
   Return
 End Function init_pboot
!
!
!
 Subroutine boot_pscores (n, m, nb, iobs, cps, fps, rmiss, pboot_cls)
!
! Bootstraps probabilistic scores
!
! Modules
   Use data_numbers,   Only: rp, bignum
   Use maths,          Only: interval
   Use progress_meter, Only: lstop, &
                             update_progress_meter
   Use settings,       Only: ibs
   Use distribs,       Only: q_empirical
   Use scores,         Only: calc_pscores
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: m  ! - number of variables -
   Integer, Intent(In) :: nb ! - number of probability bins -
!
   Real(Kind=rp), Intent(In) :: rmiss ! - missing values -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: cps ! - climatological probabilities -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: fps ! - forecast probabilities -
!
! - output arrays -
   Type(interval), Dimension(:), Intent(Out) :: pboot_cls ! - probabilistic scores confidence limits -
!
! Locals
!
! Local scalars
   Integer :: i     ! - probabilistic score index -
   Integer :: j     ! - bootstrap sample index -
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Bootstrap probabilistic scores
   boot(:,:)%lower = bignum
   boot(:,:)%upper = -bignum
   Do j = 1, nboot
      Call get_pboot (n, iobs(:,:), ipboot(:,:), fps(:,:,:), pboot(:,:,:))
      If (lstop) Return
      Call calc_pscores (n, m, nb, ibs, ipboot, cps, pboot, rmiss, bootc, ifail)
      If (ifail /= 0 .or. lstop) Return
      Do i = 1, mps
         Call adjust_boot (nbw, bootc(i), boot(:,i))
      End Do
      ifail = update_progress_meter(.false.)
   End Do
!
! Identify confidence limits
   Do i = 1, mps
      pboot_cls(i)%lower = q_empirical(boot(:,i)%lower, nboot, pcls%lower)
      pboot_cls(i)%upper = q_empirical(boot(:,i)%upper, nboot, pcls%lower)
   End Do
!
   Return
 End Subroutine boot_pscores
!
!
!
 Subroutine get_boot (n, x, y, ix, iy, xboot, yboot, ixboot, iyboot)
!
! Sets up a bootstrap sample
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - size of bootstrap sample -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ix ! - sample 1 -
   Integer, Dimension(:), Intent(In) :: iy ! - sample 2 -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - sample 1 -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - sample 2 -
!
! - output arrays -
   Integer, Dimension(:), Intent(Out) :: ixboot ! - sample 1 -
   Integer, Dimension(:), Intent(Out) :: iyboot ! - sample 2 -
!
   Real(Kind=rp), Dimension(:), Intent(Out) :: xboot ! - bootstrap sample 1 -
   Real(Kind=rp), Dimension(:), Intent(Out) :: yboot ! - bootstrap sample 2 -
!
! Locals
!
! Local scalars
   Integer :: i    ! - case index -
   Integer :: indx ! - bootstrap sample index -
!
   Real(Kind=rp) :: r  ! - random number -
   Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Random_Number, Real
!
! Executable Statements
!
! Generate a bootstrap sample
   df = Real(n, Kind=rp)
   Do i = 1, n
      Call Random_Number (r)
      indx = Int(r*df) + 1
      xboot(i) = x(indx)
      yboot(i) = y(indx)
      ixboot(i) = ix(indx)
      iyboot(i) = iy(indx)
   End Do
!
   Return
 End Subroutine get_boot
!
!
!
 Subroutine get_pboot (n, iobs, ipboot, p, pboot)
!
! Sets up a bootstrap sample for probabilities
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - size of bootstrap sample -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: iobs ! - observed categories -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In) :: p ! - probabilities -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: ipboot ! - bootstrapped categories -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: pboot ! - bootstrapped probabilities -
!
! Locals
!
! Local scalars
   Integer :: k    ! - case index -
   Integer :: indx ! - bootstrap sample index -
!
   Real(Kind=rp) :: r  ! - random number -
   Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Random_Number, Real
!
! Executable Statements
!
! Generate a bootstrap sample
   df = Real(n, Kind=rp)
   Do k = 1, n
      Call Random_Number (r)
      indx = Int(r*df) + 1
      ipboot(:,k) = iobs(:,indx)
      pboot(:,k,:) = p(:,indx,:)
   End Do
!
   Return
 End Subroutine get_pboot
!
!
!
 Subroutine get_perm1 (n, x, xperm, ix, ixperm)
!
! Sets up a permutation sample
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - size of permutation sample -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ix ! - sample -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - sample -
!
! - output arrays -
   Integer, Dimension(:), Intent(Out) :: ixperm ! - permutation sample -
!
   Real(Kind=rp), Dimension(:), Intent(Out) :: xperm ! - permutation sample -
!
! Locals
!
! Local scalars
   Integer :: k     ! - case index -
   Integer :: indx  ! - permutation index -
   Integer :: nleft ! - number of cases remaining -
!
   Real(Kind=rp) :: r ! - random number -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Random_Number, Real
!
! Executable Statements
!
! Generate a permutation sample
   indices(:) = [(k, k=1,n)]
   Do k = 1, n-1
      Call Random_Number (r)
      nleft = n + 1 - k
      indx = Int(r*Real(nleft, Kind=rp)) + 1
      xperm(k) = x(indices(indx))
      ixperm(k) = ix(indices(indx))
      If (indx < nleft) indices(indx:nleft-1) = indices(indx+1:nleft)
   End Do
   xperm(n) = x(indices(1))
   ixperm(n) = ix(indices(1))
!
   Return
 End Subroutine get_perm1
!
!
!
 Subroutine get_perm2 (n, m, x, xperm, ix, ixperm)
!
! Sets up a permutation sample
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
   Integer, Intent(In) :: m ! - number of vraiables -
!
! - input arrays -
   Integer, Dimension(:,:), Intent(In) :: ix ! - sample -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - sample -
!
! - output arrays -
   Integer, Dimension(:,:), Intent(Out) :: ixperm ! - permutation sample -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: xperm ! - permutation sample -
!
! Locals
!
! Local scalars
   Integer :: k     ! - case index -
   Integer :: indx  ! - permutation index -
   Integer :: nleft ! - number of cases remaining -
!
   Real(Kind=rp) :: r  ! - random number -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Random_Number, Real
!
! Executable Statements
!
! Generate a permutation sample
   indices(:) = [(k, k=1,n)]
   Do k = 1, n-1
      Call Random_Number (r)
      nleft = n + 1 - k
      indx = Int(r*Real(nleft, Kind=rp)) + 1
      xperm(1:m,k) = x(1:m,indices(indx))
      ixperm(1:m,k) = ix(1:m,indices(indx))
      If (indx < nleft) indices(indx:nleft-1) = indices(indx+1:nleft)
   End Do
   xperm(1:m,n) = x(1:m,indices(1))
   ixperm(1:m,n) = ix(1:m,indices(1))
!
   Return
 End Subroutine get_perm2
!
!
!
 Subroutine adjust_boot (nbw, b, boot)
!
! Adjusts bootstrap confidence intervals given a new result
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nbw ! - number of bootstrap working samples -
!
   Real(Kind=rp), Intent(In) :: b ! - current score -
!
! - input/output arrays -
   Type(interval), Dimension(:), Intent(InOut) :: boot ! - working bootstrap values -
!
! Locals
!
! Local scalars
   Integer :: ib ! - working bootstrap index -
   Integer :: l  ! - current bootstrap -
!
! Executable Statements
!
! Adjust bootstrap confidence interval estimates
   If (b < boot(nbw)%lower) Then
      ib = nbw
      Do l = nbw-1, 1, -1
         If (b > boot(l)%lower) Exit
         ib = l
      End Do
      If (ib < nbw) boot(ib+1:nbw)%lower = boot(ib:nbw-1)%lower
      boot(ib)%lower = b
   End If
   If (b > boot(nbw)%upper) Then
      ib = nbw
      Do l = nbw-1, 1, -1
         If (b < boot(l)%upper) Exit
         ib = l
      End Do
      If (ib < nbw) boot(ib+1:nbw)%upper = boot(ib:nbw-1)%upper
      boot(ib)%upper = b
   End If
!
   Return
 End Subroutine adjust_boot
!
!
!
 Subroutine proj_read_bootstrap (iin, ifail)
!
! Reads boostrap settings from project file
!
! On exit returns:
!    0 if no errors
!    1 if problem reading file
!    2 if premature end of file
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
! Executable Statements
!
! Read bootstrap settings
   Read (Unit=iin, Err=1, End=2) clb, ibcl, ipval, nboot, nperm
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
 End Subroutine proj_read_bootstrap
!
!
!
 Subroutine proj_write_bootstrap (iout, ifail)
!
! Writes bootstrap settings to project file
!
! On exit returns:
!    0 if no errors
!    1 if problem writing to file
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
! Write bootstrap settings
   Write (Unit=iout, Err=1) clb, ibcl, ipval, nboot, nperm
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_bootstrap
!
!
!
 Subroutine parameter_write_boot (iout, ifail)
!
! Writes program settings to parameter file
!
! Modules
   Use labels,         Only: cg_conflev
   Use menus,          Only: mn_opts_boot
   Use parameter_file, Only: output_parameter
   Use analysis,       Only: ihind
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
! Write project settings
! - tailoring -
   If (mn_opts_boot%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_boot%l_lab%c), i=mn_opts_boot%id) /= 0) GoTo 1
      If (ibcl == 0) Then
         cyn = 'n'
      Else
         cyn = 'y'
      End If 
      If (output_parameter(iout, & ! - calculate confidence intervals? -
                           cdesc='Calculate confidence intervals?', c=cyn) /= 0) GoTo 1
      If (ihind == 1) Then
         If (ipval == 0) Then
            cyn = 'n'
         Else
            cyn = 'y'
         End If 
         If (output_parameter(iout, & ! - calculate p-values? -
                              cdesc='Calculate p-values?', c=cyn) /= 0) GoTo 1
         If (output_parameter(iout, & ! - number of permutations -
                              cdesc='Number of permutations?', i=nperm) /= 0) GoTo 1
      End If
      If (output_parameter(iout, & ! - number of bootstrap samples -
                           cdesc='Number of bootstrap samples?', i=nboot) /= 0) GoTo 1
      If (output_parameter(iout, & ! - number of bootstrap samples -
                           cdesc=Trim(cg_conflev), r=clb) /= 0) GoTo 1
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
 End Subroutine parameter_write_boot
!
!
!
 Function close_boot() &
          Result (fclose)
!
! Clears memory assigned for bootstrapping
!
! Modules
   Use gui, Only: set_cursor_waiting
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
! Free memory
   If (Allocated(bootc))   Deallocate (bootc)
   If (Allocated(boot))    Deallocate (boot)
   If (Allocated(xboot))   Deallocate (xboot)
   If (Allocated(yboot))   Deallocate (yboot)
   If (Allocated(ixboot))  Deallocate (ixboot)
   If (Allocated(iyboot))  Deallocate (iyboot)
   If (Allocated(indices)) Deallocate (indices)
   Call set_cursor_waiting (0)
   fclose = 0
!
   Return
 End Function close_boot
!
!
!
 Function close_pboot() &
          Result (fclose)
!
! Clears memory assigned for bootstrapping of probabilistic scores
!
! Modules
   Use gui, Only: set_cursor_waiting
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
! Free memory
   If (Allocated(bootc))  Deallocate (bootc)
   If (Allocated(boot))   Deallocate (boot)
   If (Allocated(pboot))  Deallocate (pboot)
   If (Allocated(ipboot)) Deallocate (ipboot)
   Call set_cursor_waiting (0)
   fclose = 0
!
   Return
 End Function close_pboot
!
!
!
 Function close_pval() &
          Result (fclose)
!
! Clears memory assigned for calculating p-values
!
! Modules
   Use gui, Only: set_cursor_waiting
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
! Free memory
   If (Allocated(perm))    Deallocate (perm)
   If (Allocated(yperm))   Deallocate (yperm)
   If (Allocated(iperm))   Deallocate (iperm)
   If (Allocated(indices)) Deallocate (indices)
   Call set_cursor_waiting (0)
   fclose = 0
!
   Return
 End Function close_pval
End Module bootstrap
