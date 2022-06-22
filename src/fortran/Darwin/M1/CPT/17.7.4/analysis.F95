! Author: Simon Mason
Module analysis
!
! Declarations
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: analysis_flags, close_analysis, close_calc, close_cv2, close_cv2results, init_cv2, init_cv2results, init_results, &
             num_calcs, perform_analysis, proj_read_analysis, proj_write_analysis, reset_analysis
!
! Scalars
!
! Integer scalars
   Integer, Public :: ianal = 0 ! - analysis identifier (1=CCA, 2=PCR, 3=MLR, 4=GCM, 5=PFV) -
   Integer, Public :: ianaln    ! - intended new analysis identifier (1=CCA, 2=PCR, 3=MLR, 4=GCM, 5=PFV) -
   Integer, Public :: icalc     ! - calculation completed flag (0=no, 1=yes) -
   Integer, Public :: ihind     ! - hindcasts flag -
   Integer, Public :: nopt      ! - total number of optimization steps -
!
! Arrays
!
! Logical arrays
   Logical, Dimension(:,:), Allocatable, Public :: lopt ! - optimized? -
!
Contains
!
!
 Subroutine reset_analysis ()
!
! Resets analysis flags
!
! Modules
   Use menus,  Only: set_menu_greying
   Use arrays, Only: lconvert
!
! Executable Statements
!
! Initialise analysis settings
   Call analysis_flags ('off')
   Call set_menu_greying (lxyopen=.false.)
   lconvert = .false.
!
   Return
 End Subroutine reset_analysis
!
!
!
 Subroutine analysis_flags (onoff)
!
! Switches calculation flags on or off
!
! Modules
   Use data_numbers,       Only: zero, one
   Use data_cpt_constants, Only: ia_non, ia_pfv
   Use menus,              Only: set_menu_greying
   Use progress_meter,     Only: lcalc, lstop, &
                                 close_progress_meter, set_progress_meter
   Use settings,           Only: iretro
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: onoff
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Random_Seed
!
! Executable Statements
!
! Set flags on
   Select Case (onoff)
    Case ('on', 'ON', 'On')
      icalc = 0                       ! - calculation completed flag -
      lcalc = .true.                  ! - calculation active? -
      lstop = .false.                 ! - stop calculation flag -
      Call set_progress_meter (zero)  ! - progress meter -
!
! - initialise random number generator -
      Call Random_Seed ()
!
! Set flags to indicate end of calculation
    Case ('end', 'END', 'End')
      icalc = 1                       ! - calculation completed flag -
      lcalc = .false.                 ! - calculation active? -
      Call set_progress_meter (one)   ! - progress meter -
      Call set_menu_greying (ldone=.true.)
!
! Set all flags off
    Case ('off', 'OFF', 'Off')
      icalc = 0                       ! - calculation completed flag -
      lcalc = .false.                 ! - calculation active? -
      lstop = .false.                 ! - stop calculation flag -
      If (ianal /= ia_pfv) iretro = 0 ! - retroactive calculation flag -
      nopt = 0                        ! - number of optimizations -
      Call close_progress_meter ()
      Call set_menu_greying (ldone=(ianal == ia_non), ltitle=(ianal == ia_non))
   End Select
!
! Update menu greying
!
   Return
 End Subroutine analysis_flags
!
!
!
 Subroutine init_cv2 (ifail)
!
! Initialises memory and settings to perform EOF prefiltering and model fitting
!
! Modules
   Use settings,   Only: nf, nfbk
   Use arrays,     Only: x, xin, y, yin
   Use iofiles,    Only: bkfile, xfile, zfile
   Use prediction, Only: ifm
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
#if FTN95 == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Size
#endif
! Executable Statements
!
! Allocate backup data space
! - X input data -
#if FTN95 == 1
   Allocate (xin(Size(x,Dim=1),Size(x,Dim=2),Size(x,Dim=3)), Stat=ifail)
   xin(:,:,:) = x(:,:,:)
#else
   Allocate (xin, Source=x, Stat=ifail)
#endif
   If (ifail /= 0) GoTo 1
! - Y input data -
#if FTN95 == 1
   Allocate (yin(Size(y,Dim=1),Size(y,Dim=2),Size(y,Dim=3)), Stat=ifail)
   yin(:,:,:) = y(:,:,:)
#else
   Allocate (yin, Source=y, Stat=ifail)
#endif
   If (ifail /= 0) GoTo 1
!
! Create backups of real-time predictors and initialise
   bkfile = zfile
   zfile = xfile
   nfbk = nf
   nf = 1
   ifm = 1
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_cv2
!
!
!
 Subroutine init_cv2results (ifail)
!
! Initialises memory for double cross-validation results
!
! Modules
   Use data_cpt_constants, Only: ng
   Use settings,           Only: igauss, nenf, nt
   Use iofiles,            Only: mya
   Use regression,         Only: ens_2, pesd_2, pls_2, ycv2, ycv2t
   Use season,             Only: lprst
   Use categories,         Only: fps_2, odds_2
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory for double cross-validation results
! - double cross-validated predictions -
   If (.not.lprst) Then
      Allocate (ycv2(mya,nt,1), Stat=ifail)
   Else
      Allocate (ycv2(mya,nt,0:2), Stat=ifail)
   End If
   If (ifail /= 0) GoTo 1
! - double cross-validated ensemble predictions -
   Allocate (ens_2(mya,nt,nenf), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - transformed double cross-validated predictions -
   If (igauss > 0) Then
      Allocate (ycv2t(mya,nt), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - prediction-error variance -
   Allocate (pesd_2(mya,nt), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - double cross-validated prediction limits -
   Allocate (pls_2(mya,nt), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - double cross-validated forecast probabilities -
   Allocate (fps_2(mya,nt,ng), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - double cross-validated odds -
   Allocate (odds_2(mya,nt,ng,2), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_cv2results
!
!
!
 Subroutine init_analysis1 (ianalc, lproj, ng, ns, nret, nenf, nopt, ndom, ifail, &
            nlt, nlg)
!
! Initialises memory and settings to perform EOF prefiltering and model fitting
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ic_mul, ic_pcr, ir_bin, it_non
   Use settings,           Only: iensc, igauss, ig0, imc, iregr, iretro, iskmask, hcw, lcw, nu, nur
   Use arrays,             Only: rwk, rwk2, iwk, liwk, lrwk, tc, xc, xiny, xm, xsd, yc, ym, ysd, yt
   Use space,              Only: init_space
   Use iofiles,            Only: mxa, mya
   Use fields,             Only: first_field
   Use pcs,                Only: lxt, lxyt, mcc, mxe, mye, ncc, nxe, nye, &
                                 init_ccamaps, init_svds, init_svdwk
   Use regression,         Only: b, b0, ens_r, pesd_r, pls_r, u, xvp, ycv, ycvt, yfit, yfitt, yopt, yret, yrett, ys2
   Use scores,             Only: corrmsk
   Use categories,         Only: efps, fps_r, odds_r
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianalc ! - analysis identifier -
   Integer, Intent(In) :: ng     ! - number of categories -
   Integer, Intent(In) :: ns     ! - number of seasons -
   Integer, Intent(In) :: nenf   ! - number of ensemble members -
   Integer, Intent(In) :: nret   ! - number of retroactive iterations -
   Integer, Intent(In) :: ndom   ! - number of domains -
!
   Logical, Intent(In) :: lproj ! - restore from project? -
!
! - output scalars -
   Integer, Intent(Out) :: nopt  ! - number of optimization steps -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nlt ! - number of latitudes -
   Integer, Intent(In), Optional :: nlg ! - number of longitudes -
!
! Locals
!
! Local scalars
   Integer :: lnn  ! - minimum of ndom, and nu -
   Integer :: mxt  ! - maximum of mxa, and nu -
   Integer :: mxyt ! - maximum of mxa, mya, and nu -
   Integer :: mopt ! - number of optimization steps -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Max, MaxVal, Min, Present
!
! Executable Statements
!
! Free existing memory
   If (Allocated(rwk)) Deallocate (rwk)
!
! Allocate additional memory
! - means -
   Select Case (ianalc)
    Case (ia_cca, ia_pcr, ia_mlr)
      Allocate (xm(mxa), Stat=ifail)
    Case (ia_gcm)
      Allocate (xm(mya), Stat=ifail)
   End Select
   If (ifail /= 0) GoTo 1
   Allocate (ym(mya), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - standard deviations -
   Select Case (ianalc)
    Case (ia_cca, ia_pcr, ia_mlr)
      Allocate (xsd(mxa), Stat=ifail)
    Case (ia_gcm)
      Allocate (xsd(mya), Stat=ifail)
   End Select
   If (ifail /= 0) GoTo 1
   Allocate (ysd(mya), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - cross-validated predictions -
   Allocate (ycv(mya,nu,ns), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - fitted values -
   Allocate (yfit(mya,nu,ns), Stat=ifail)
   If (ifail /= 0) GoTo 1
   If (igauss > 0) Then
! - transformed response data -
      Allocate (yt(mya,nu), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - cross-validated transformed predictions -
      Allocate (ycvt(mya,nu), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - cross-validated transformed fitted values -
      Allocate (yfitt(mya,nu), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - variance of hindcast errors -
   Allocate (ys2(mya), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - optimization results -
   If (nret > 0) Then
      Select Case (ianalc)
       Case (ia_cca)
         nopt = get_nopt(nxe, mxe, &
                nye=nye, mye=mye, ncc=ncc, mcc=mcc)
         ifail = init_ccamaps(Min(mxe, mye, mcc))
         If (ifail /= 0) GoTo 1
       Case (ia_pcr)
         nopt = get_nopt(nxe, mxe)
       Case (ia_mlr, ia_gcm)
         nopt = 1
       Case Default
         nopt = 0
      End Select
      If (nopt > 0) Then
         Call init_opt (nopt, nret,ifail)
         If (ifail /= 0) GoTo 1
      End If
   Else
      nopt = 0
   End If
! - ensemble forecast probabilities -
   If (ianalc == ia_gcm) Then
      If (iensc > 1) Then
         Allocate (efps(mya,nu,ng), Stat=ifail)
         If (ifail /= 0) GoTo 1
      End If
   End If
! - retroactive predictions -
   If (iretro == 1) Then
      Allocate (yret(mya,nur,ns), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - retroactive ensemble predictions -
      Allocate (ens_r(mya,nur,nenf), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - transformed retroactive predictions -
      If (igauss > 0) Then
         Allocate (yrett(mya,nur), Stat=ifail)
         If (ifail /= 0) GoTo 1
      End If
! - prediction-error variance -
      Allocate (pesd_r(mya,nur), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - retroactive prediction limits -
      Allocate (pls_r(mya,nur), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - retroactive forecast probabilities -
      Allocate (fps_r(mya,nur,ng), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - retroactive odds -
      Allocate (odds_r(mya,nur,ng,2), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - regression -
   Select Case (ianalc)
    Case (ia_cca)
      ifail = init_svds(ianalc, mxa, mya, nu) ! - SVDs -
      If (ifail /= 0) GoTo 1
    Case (ia_pcr)
      ifail = init_svds(ianalc, mxa, mya, nu) ! - SVDs -
      If (ifail /= 0) GoTo 1
      Allocate (b0(mya,1), Stat=ifail) ! - regression constants -
      If (ifail /= 0) GoTo 1
      Allocate (b(mxa,mya), Stat=ifail) ! - regression coefficients -
      If (ifail /= 0) GoTo 1
    Case (ia_mlr)
      lxt = Min(mxa+1, nu)
      Allocate (u(1,mxa+1,lxt), Stat=ifail) ! - workspace for xvp calculation -
      If (ifail /= 0) GoTo 1
      Allocate (b0(mya,1), Stat=ifail) ! - regression constants -
      If (ifail /= 0) GoTo 1
      Allocate (b(mxa,mya), Stat=ifail) ! - regression coefficients -
      If (ifail /= 0) GoTo 1
    Case (ia_gcm)
      If (ndom > 1) Then
         ig0 = 0
      Else
         ig0 = 1
      End If
      lxt = Min(ndom+1, nu)
      Allocate (u(mya,mxa+1,lxt), Stat=ifail) ! - workspace for xvp calculation -
      If (ifail /= 0) GoTo 1
      Allocate (xiny(mya,nu,ndom), Stat=ifail) ! - interpolated X values -
      If (ifail /= 0) GoTo 1
      Allocate (b0(ig0:ndom,mya), Stat=ifail) ! - regression constants -
      If (ifail /= 0) GoTo 1
      Allocate (b(ig0:ndom,mya), Stat=ifail) ! - regression coefficients -
      If (ifail /= 0) GoTo 1
      If (Present(nlt) .and. Present(nlg)) Then
         ifail = init_space(nlt, nlg, ndom) ! - nearest latitudes and longitudes -
         If (ifail /= 0) GoTo 1
      End If
   End Select
! - masking correlations -
   If (iskmask == 1) Then
      Allocate (corrmsk(mya), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - copy of training period independent data -
   Select Case (ianalc)
    Case (ia_cca, ia_pcr, ia_mlr)
      Allocate (xc(mxa,nu,1), Stat=ifail)
    Case (ia_gcm)
      Allocate (xc(mya,nu,ndom), Stat=ifail)
   End Select
   If (ifail /= 0) GoTo 1
! - copy of training period dependent data -
   Allocate (yc(mya,nu), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - copy of training period binomial coefficients -
   If (iregr == ir_bin) Then
      Allocate (tc(nu), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End If
! - standard error from predictors -
   If (iretro == 1) Then
      Select Case (ianalc)
       Case (ia_cca, ia_pcr)
         Allocate (xvp(1,nur), Stat=ifail)
       Case (ia_mlr)
         Allocate (xvp(1,nur), Stat=ifail)
       Case (ia_gcm)
         Allocate (xvp(mya,nur), Stat=ifail)
      End Select
      If (ifail /= 0) GoTo 1
   End If
! - SVD workspace -
   If (ianalc == ia_cca) ifail = init_svdwk()
   If (ifail /= 0) GoTo 1
!
! Calculate required workspace
   Select Case (ianalc)
    Case (ia_cca)
      mxyt = Max(mxa, mya, 2*nu - 3*lxyt)
      lrwk = lxyt*(6 + 4*lxyt) + mxyt
      liwk = 8*lxyt
    Case (ia_pcr, ia_mlr)
      mxt = Max(mxa, 2*nu - 3*lxt)
      lrwk = lxt*(6 + 4*lxt) + Max(mxt, mya)
      liwk = 8*lxt
    Case (ia_gcm)
      lxt = Min(ndom, nu)
      mxt = Max(mya, 2*nu - 3*lxt)
      lrwk = lxt*(6 + 4*lxt) + Max(mxt, mya)
      liwk = 8*lxt
      Select Case (imc)
       Case (ic_mul, ic_pcr) ! - add additional space for multi-model and PCR combination -
         lnn = Min(ndom, nu)
         lrwk = Max(lrwk, 3*lnn + MaxVal([ndom, nu, 4*lnn*(lnn + 1)]))
         liwk = Max(lrwk, 8*lnn)
      End Select
   End Select
!
! Allocate workspace
   Allocate (iwk(liwk), Stat=ifail)
   If (ifail /= 0) GoTo 1
   Allocate (rwk(lrwk), Stat=ifail)
   If (ifail /= 0) GoTo 1
   If (ianalc == ia_gcm) Then
      Allocate (rwk2(Max(ndom,mya),nu), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End if
! - hindcasts for optimization -
   If (lproj) Then
      mopt = 1
   Else
      mopt = Max (1, nopt)
   End If
   Select Case (igauss)
    Case (it_non)
      Allocate (yopt(mya,nu,mopt), Stat=ifail)
    Case Default
      Allocate (yopt(mya,nu,Max(2, mopt)), Stat=ifail)
   End Select
   If (ifail /= 0) GoTo 1
!
! Calculate half cross-validation window
   hcw = (lcw + 1)/2
!
! Identify initial grid point
   Call first_field ()
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_analysis1
!
!
!
 Subroutine init_analysis2 (ianal, ifail, &
            nco)
!
! Initialises additional memory for analysis
!
! Modules
   Use data_cpt_constants, Only: ia_cca
   Use pcs,                Only: close_ccamaps, init_ccamaps
   Use regression,         Only: yopt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis flag -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nco ! - optimal number of CCA modes -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Present
!
! Executable Statements
!
! Deallocate unneeded workspace
   ifail = 0
   If (Allocated(yopt)) Deallocate (yopt)
   If (ianal == ia_cca) ifail = close_ccamaps ()
!
! Allocate additional memory for CCA maps
   If (Present(nco)) Then
      ifail = init_ccamaps(nco)
      If (ifail /= 0) GoTo 1
   End If
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_analysis2
!
!
!
 Subroutine init_results (ianalc, nlt, nlg, ifail)
!
! Initialises memory for results
!
! Modules
   Use data_cpt_constants, Only: ia_pfv
   Use settings,           Only: icv2, iretro, nt, nu, nur
   Use arrays,             Only: awk, iwk, rwk, rwk2, tc, xc, ybk, yc
   Use iofiles,            Only: mya, nx, ny
   Use pcs,                Only: ce
   Use regression,         Only: xvp
   Use categories,         Only: init_categories
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianalc ! - analysis identifier -
   Integer, Intent(In) :: nlt    ! - maximum number of latitudes -
   Integer, Intent(In) :: nlg    ! - maximum number of longitudes -
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
! Deallocate unneeded workspace
   If (Allocated(rwk2)) Deallocate (rwk2)
   If (Allocated(rwk))  Deallocate (rwk)
   If (Allocated(iwk))  Deallocate (iwk)
   If (Allocated(ce))   Deallocate (ce)
   If (Allocated(xvp))  Deallocate (xvp)
   If (Allocated(tc))   Deallocate (tc)
   If (Allocated(yc))   Deallocate (yc)
   If (Allocated(xc))   Deallocate (xc)
   If (Allocated(ybk))  Deallocate (ybk)
!
! Allocate additional memory
   Select Case (ianalc)
    Case Default
      ifail = init_categories(ianalc, icv2, iretro, mya, nu, nur)
    Case (ia_pfv)
      ifail = init_categories(ianalc, icv2, iretro, mya, nu, nt)
   End Select
   If (ifail /= 0) GoTo 1
! - workspace -
   Allocate (awk(Max(nx, ny, nlg, nlt)), Stat=ifail)
   If (ifail /= 0) GoTo 1
   Allocate (rwk(Max(2*nu, nx, ny, nlg, nlt)), Stat=ifail)
   If (ifail /= 0) GoTo 1
   Allocate (rwk2(mya, nu), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   Return
!
! Errors
1  ifail = 1
!
   Return
 End Subroutine init_results
!
!
!
 Subroutine init_opt (nopt, nret, ifail)
!
! Initialises additional memory for optimization
!
! Modules
   Use settings, Only: iretro
   Use scores,   Only: gm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nopt ! - number of optimization steps -
   Integer, Intent(In) :: nret ! - number of retroactive steps -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Initialises additional memory for optimization
   Allocate (lopt(nopt,nret*iretro+1), Stat=ifail)
   If (ifail /= 0) Return
   Allocate (gm(nopt,nret*iretro+1), Stat=ifail)
   If (ifail /= 0) Return
!
   Return
 End Subroutine init_opt
!
!
!
 Function get_nopt(nxe, mxe, &
          nye, mye, ncc, mcc) &
          Result (nopt)
!
! Calculates number of optimization steps
!
! Function result
   Integer :: nopt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nxe ! - minimum number of X EOF modes -
   Integer, Intent(In) :: mxe ! - maximum number of X EOF modes -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nye ! - minimum number of Y EOF modes -
   Integer, Intent(In), Optional :: mye ! - maximum number of Y EOF modes -
   Integer, Intent(In), Optional :: ncc ! - minimum number of CCA modes -
   Integer, Intent(In), Optional :: mcc ! - maximum number of CCA modes -
!
! Locals
!
! Local scalars
   Integer :: ic ! - CCA mode index -
   Integer :: ix ! - X EOF index -
   Integer :: iy ! - Y EOF index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min, Present
!
! Executable Statements
!
! Calculate number of retroactive cross-validations
   If (Present(nye) .and. Present(mye) .and. Present(ncc) .and. Present(mcc)) Then
      nopt = 0
      Do ix = nxe, mxe
         Do iy = nye, mye
            Do ic = ncc, Min(mcc, ix, iy)
               nopt = nopt + 1
            End Do
         End Do
      End Do
   Else
      nopt = mxe + 1 - nxe
   End If
!
   Return
 End Function get_nopt
!
!
!
 Function num_calcs(ianal, iretro, igauss, izero, ifit, mya, nt, nret, nretro, nt1, ntr, nopt)
!
! Calculates total number of calculations to perform for progress meter
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, it_non
!
! Function result
   Integer :: num_calcs
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal  ! - analysis method -
   Integer, Intent(In) :: iretro ! - retroactive forecast flag -
   Integer, Intent(In) :: igauss ! - transform to gaussian flag -
   Integer, Intent(In) :: izero  ! - zero-bound flag -
   Integer, Intent(In) :: ifit   ! - calculate fitted values flag -
   Integer, Intent(In) :: mya    ! - number of available gridpoints -
   Integer, Intent(In) :: nt     ! - number of cases in training period -
   Integer, Intent(In) :: nret   ! - number of retroactive iterations -
   Integer, Intent(In) :: nretro ! - model update interval -
   Integer, Intent(In) :: nt1    ! - initial number of training cases -
   Integer, Intent(In) :: ntr    ! - number of cases in retroactive period -
   Integer, Intent(In) :: nopt   ! - number of optimization steps -
!
! Locals
!
! Local scalars
   Integer :: ncvr ! - total number of retroactive cross-validations -
   Integer :: itg  ! - transformation flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Nint, Real, Sqrt
!
! Executable Statements
!
! Calculate number of retroactive cross-validations
   ncvr = nret*nt1 + nret*(nret - 1)*nretro/2
!
! Calculate number of calculations to perform
   Select Case (igauss)
    Case (it_non)
      itg = 0
    Case Default
      itg = 1
   End Select
   num_calcs = 1
   Select Case (ianal)
    Case (ia_cca) ! - CCA -
      If (iretro == 1) Then
         num_calcs = num_calcs + ncvr*(1 + 2*nopt) + nret*(1 + (1 - itg)*izero) ! - cv_analysis -
         num_calcs = num_calcs + 6*nret                                         ! - full_analysis -
         num_calcs = num_calcs + 2*ntr                                          ! - analysis_prediction -
         num_calcs = num_calcs + ifit*2*ncvr                                    ! - fit_analysis -
         num_calcs = num_calcs + nret*(Max(mya, ntr) + itg + izero)             ! - calc_probs -
      End If
      num_calcs = num_calcs + nt*(1 + 2*nopt) + 1 + itg + izero                 ! - cv_analysis -
      num_calcs = num_calcs + 8                                                 ! - full_analysis -
    Case (ia_pcr, ia_mlr) ! - PCR, MLR -
      If (iretro == 1) Then
         num_calcs = num_calcs + ncvr*(1 + nopt) + nret*(1 + (1 - itg)*izero)   ! - cv_analysis -
         num_calcs = num_calcs + 2*nret                                         ! - full_analysis -
         num_calcs = num_calcs + ntr                                            ! - analysis_prediction -
         num_calcs = num_calcs + ifit*ncvr                                      ! - fit_analysis -
         num_calcs = num_calcs + nret*(Max(mya, ntr) + itg + izero)             ! - calc_probs -
      End If
      num_calcs = num_calcs + nt*(1 + nopt) + 1 + itg + izero                   ! - cv_analysis -
      num_calcs = num_calcs + 5                                                 ! - full_analysis -
    Case (ia_gcm) ! - GCM -
      num_calcs = num_calcs + mya                                               ! - get_nearest_grids -
      num_calcs = num_calcs + mya                                               ! - get_interpolated -
      If (iretro == 1) Then
         num_calcs = num_calcs + ncvr + nret*(1 + (1 - itg)*izero)              ! - cv_analysis -
         num_calcs = num_calcs + 2*nret                                         ! - full_analysis -
         num_calcs = num_calcs + ntr                                            ! - analysis_prediction -
         num_calcs = num_calcs + ifit*ncvr                                      ! - fit_analysis -
         num_calcs = num_calcs + nret*(Max(mya, ntr) + itg + izero)             ! - calc_probs -
      End If
      num_calcs = num_calcs + nt + 1 + itg + izero                              ! - cv_analysis -
      num_calcs = num_calcs + 2                                                 ! - full_analysis -
    Case (ia_pfv) ! - PFV -
      num_calcs = num_calcs + mya                                               ! - get_nearest_grids -
      num_calcs = num_calcs + mya                                               ! - get_interpolated -
   End Select
   If (ianal /= ia_pfv) num_calcs = num_calcs + Nint(Sqrt(Real(mya*nt)))        ! - set_cv_categories -
   If (iretro == 1) num_calcs = num_calcs + Nint(Sqrt(Real(mya*ntr)))           ! - set_ra_categories -
!
   Return
 End Function num_calcs
!
!
!
 Function perform_analysis() &
           Result (perform)
!
! Performs CCA, PCR, or GCM
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail = -1 Terminated
!
! Modules
   Use data_numbers,       Only: rp, one
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ic_skl, ic_mul, id_grd, id_stn, id_ind, &
                                 ie_cvh, ie_cv2, ie_fit, ie_ret, ig_skl, is_non, is_sti, it_non, ng
   Use data_io_constants,  Only: iout, io_com, lprd
   Use labels,             Only: ca_goodi, cg_to_l, cg_tperiod, l_goodness
   Use time,               Only: pdate, pprd, &
                                 Operator(+), &
                                 get_cdate
   Use gui,                Only: print_cvalue, print_text
   Use progress_meter,     Only: lstop, &
                                 set_progress_increment
   Use errors,             Only: query
   Use settings,           Only: dgood, iev, igauss, igcms, igood, ig0, imc, intp, iretro, iskmask, istd, izero, lcw, ncv, nenf, &
                                 nret, nretro, nt, nt1, nu, nur, nu1
   Use arrays,             Only: iusex, iusey, lconvert, t, x, xiny, xm, xsd, y, ybk, ym, ysd, yt, &
                                 do_convert, opposite_vars
   Use space,              Only: get_interpolated, set_nearest_grids
   Use iofiles,            Only: gifile, mxa, mya, xfile, yfile, &
                                 file_reset
   Use fields,             Only: xfield, yfield
   Use distribs,           Only: moments, stdize
   Use pcs,                Only: bz, eofx, eofy, iec, iey, mcc, mxe, mye, mu, ncc, nco, nccu, npx, npy, nxe, nxo, nye, nyo, &
                                 r, s, svx, svy, tsx, tsy
   Use regression,         Only: b, b0, dofr, ens_r, pesd_r, pls_r, u, xvp, ycv, ycvt, yfit, yfitt, yopt, yret, yrett, ys2, &
                                 calc_pesd
   Use scores,             Only: dir_goodness
   Use season,             Only: lprst, nsnc
   Use missing,            Only: ktuse
   Use categories,         Only: fps_r, odds_r
   Use climate,            Only: ave, clim, ncu, sdev
   Use gcm,                Only: num_predictors
   Use prediction,         Only: add_persistence, calc_errvar, calc_predictions_d, calc_predictions_p, forecast_bounds, &
                                 skillmask, untransform_f
!
! Function result
   Integer :: perform
!
! Locals
!
! Local scalars
   Integer :: i     ! - continuation flag -
   Integer :: i1    ! - first index -
   Integer :: i2    ! - last index -
   Integer :: ievc  ! - current error-variance option hindcasts -
   Integer :: iml   ! - model index -
   Integer :: ir    ! - index of current retroactive step -
   Integer :: irl   ! - index of last retroactive training-period date -
   Integer :: ir1   ! - index of first retroactive date -
   Integer :: irn   ! - index of last retroactive date -
   Integer :: iru   ! - index of last new date in retroactive training period -
   Integer :: mlg   ! - maximum number of longitudes -
   Integer :: mlt   ! - maximum number of latitudes -
   Integer :: nr    ! - total number of retroaoctive steps in current cycle -
   Integer :: ncru  ! - length of climatological period during current retroactive cycle -
   Integer :: ifail ! - error indicator -
!
   Character(Len=lprd) :: cprd1 ! - period -
   Character(Len=lprd) :: cprd2 ! - period -
   Character(Len=lprd) :: cprd3 ! - period -
!
   Type(pdate) :: bdate ! - current beginning date -
!
   Type(pprd) :: prdc ! - current period -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Count, MaxVal, Min, Real, Trim
!
! Executable Statements
!
! Check for consistent units
   perform = -1
   lconvert = .false.
   Select Case (ianal)
    Case (ia_gcm)
      i2 = 0
      Do iml = 1, xfile%nms
         i1 = i2 + 1
         i2 = i2 + xfield(iml)%nva
         Call do_convert (xfield(iml)%var, xfield(iml)%unit, yfield(1)%var, yfield(1)%unit, &
              xfield(iml)%unitc, x(i1:i2,1:nu,1), ifail)
      End Do
!
! Initialise analysis
      Select Case (yfile%idstr)
       Case (id_grd)
#if FTN95 == 1
         mlt = yfield(1)%region%nlts
         mlg = yfield(1)%region%nlgs
#else
         mlt = MaxVal(yfield(:)%region%nlts)
         mlg = MaxVal(yfield(:)%region%nlgs)
#endif
         Call init_analysis1 (ianal, .false., ng, nsnc, nret, nenf, nopt, xfile%nms, ifail, &
              nlt=mlt, nlg=mlg)
       Case (id_stn)
#if FTN95 == 1
         mlt = yfield(1)%nva
         mlg = yfield(1)%nva
#else
         mlt = MaxVal(yfield(:)%nva)
         mlg = MaxVal(yfield(:)%nva)
#endif
         Call init_analysis1 (ianal, .false., ng, nsnc, nret, nenf, nopt, xfile%nms, ifail, &
              nlt=mlt, nlg=mlg)
       Case (id_ind)
         Call init_analysis1 (ianal, .false., ng, nsnc, nret, nenf, nopt, xfile%nms, ifail)
      End Select
    Case Default
      Call init_analysis1 (ianal, .false., ng, nsnc, nret, nenf, nopt, xfile%nms, ifail)
   End Select
   If (ifail /= 0) Then
      perform = 1
      Return
   End If
!
! Estimate number of calculations
   Select Case (iev)
    Case Default
      Call set_progress_increment &
           (one/Real(num_calcs(ianal, iretro, igauss, izero, 0, mya, nu, nret, nretro, nu1, nur, nopt), Kind=rp), .false.)
    Case (ie_cv2)
      Call set_progress_increment &
           (one/(nu*Real(num_calcs(ianal, iretro, igauss, izero, 0, mya, nu, nret, nretro, nu1, nur, nopt), Kind=rp)), .false.)
    Case (ie_fit)
      Call set_progress_increment &
           (one/Real(num_calcs(ianal, iretro, igauss, izero, 1, mya, nu, nret, nretro, nu1, nur, nopt), Kind=rp), .false.)
   End Select
!
! Calculate optimization settings
   Select Case (ianal)
    Case (ia_cca) ! - CCA -
      If (nopt == 1) Then
         nopt = 0
         nxo = nxe
         nyo = nye
         nco = ncc
      End If
      iey = 1
      iec = 1
    Case (ia_pcr) ! - PCR -
      If (nopt == 1) Then
         nopt = 0
         nxo = nxe
      End If
    Case (ia_mlr) ! - MLR -
      nopt = 0
      nxo = mxa
    Case (ia_gcm) ! - GCM -
      nopt = 0
      nxo = num_predictors(xfile%nms)
!
! Identify nearest grids
      Call set_nearest_grids (yfile%idstr, intp, xfile%nms, yfile%nfl, xfield(:)%nlt, xfield(:)%nlg, yfield(:)%nva, &
           xfield(:)%region, yfield(:)%region, ifail)
      Select Case (ifail)
       Case (0)
         Continue
       Case (-1)
         Return
       Case Default
         perform = ifail + 2
         Return
      End Select
!
! Calculate interpolated values
      Call get_interpolated (ianal, xfile%idstr, yfile%idstr, intp, mya, iusex, iusey, xfile%nms, yfile%nfl, nu, xfile%ngs, &
           xfield(:)%nva, yfield(:)%nva, x(:,1:nu,:), xfield(:)%rmiss, xfield(:)%region, yfield(:)%region, xiny(:,:,:))
   End Select
!
! Open goodness index file
   If (gifile%cfile%lset) Then
!      Open (Unit=iout, File=gifile%cfile%ffile, Access='sequential', Action='write', Form=ca_ffmts(gifile%cfile%ffmt%ifmt), &
      Open (Unit=iout, File=gifile%cfile%ffile, Access='sequential', Action='write', Form='formatted', &
            Iostat=ifail, Status='unknown')
! - output index -
      If (ifail == 0) Then
         Call print_cvalue (Trim(l_goodness%c), Trim(ca_goodi(igood)), &
              iout=iout)
      Else
         Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
      End If
   End If
! - set goodness direction -
   dgood = dir_goodness(igood)
   If (ianal == ia_gcm) Then ! - switch direction if using uncalibrated GCM and X and Y units are opposing -
      If (igcms /= ig_skl .and. imc /= ic_skl .and. imc /= ic_mul) Then
         If (opposite_vars(xfield(1)%var, yfield(1)%var)) Then
            i = query('perform_analysis', 1, .true., &
                c_arg1=Trim(yfield(1)%var), c_arg2=Trim(xfield(1)%var))
            Select Case (i)
             Case (0) ! - do not apply goodness index orientation inversion -
               Continue
             Case (1) ! - apply goodness index orientation inversion -
               dgood = -dgood
             Case Default ! - cancel
               perform = -1
               Return
            End Select
         End If
      End If
   End If
!
! Perform retroactive calculations
   prdc = yfile%prd1 + (yfile%it1 - 1)
   cprd1 = get_cdate(prdc, 1, io_com)
! - optimise model for successive training periods -
   ir = 0
   If (iretro == 1) Then
      Select Case (iev)
       Case Default
         ievc = iev
       Case (ie_ret)
         ievc = ie_cvh
      End Select
      Print *, 'Calculating retroactive forecasts ...'
      ir1 = 1
      irn = 0
      iru = nu1
      retro: Do irl = nt1, nt-1, nretro
         ir = ir + 1
         irn = irn + Count(ktuse(irl+1:Min(irl+nretro, nt)))
         nr = irn + 1 - ir1
         If (nr > 0) Then
            ncv = iru - lcw
            bdate = yfile%bdate + (ir - 1)*nretro
            prdc = yfile%prd1 + (yfile%it1 + irl - 2)
            cprd2 = get_cdate(prdc, 1, io_com)
            Call print_training_period (irl)
! - cross-validate using different numbers of modes -
            If (nopt > 0) Then
               Select Case (igauss)
                Case (it_non)
                 Call opt_analysis (iru, .true., ycv(:,1:iru,1))
                Case Default
                 Call opt_analysis (iru, .true., ycv(:,1:iru,1), &
                      ycvt=ycvt(:,1:iru))
               End Select
! - cross-validate -
            Else
               Select Case (igauss)
                Case (it_non)
                  Call cv_analysis (iru, nxe, mxe, nye, mye, ncc, mcc, ycv(:,1:iru,1))
                Case Default
                  Call cv_analysis (iru, nxe, mxe, nye, mye, ncc, mcc, ycv(:,1:iru,1), &
                       ycvt=ycvt(:,1:iru))
               End Select
            End If
            If (lstop) Return
! - construct full model -
            Select Case (igauss)
             Case (it_non) ! - no transformation -
               Call full_analysis (iru, nco)
             Case Default ! - transformation -
               Call full_analysis (iru, nco, &
                    yt=yopt(:,1:iru,2))
            End Select
            If (lstop) Return
            If (ifail /= 0) Then
               perform = 2
               Return
            End If
            Select Case (ianal)
             Case (ia_cca, ia_pcr)
               If (npx < nxo) nxo = npx
               If (npy < nyo) nyo = npy
            End Select
! - identify current retroactive period -
            prdc = yfile%prd1 + (yfile%it1 + irl - 1)
            cprd3 = get_cdate(prdc, 1, io_com)
            If (nr > 1) Then
               prdc = yfile%prd1 + (yfile%it1 + irl + nr - 2)
               cprd2 = get_cdate(prdc, 1, io_com)
               Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)//' - '//Trim(cprd2)
            Else
               Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)
            End If
            If (lstop) Return
! - calculate fitted values -
            If (ievc == ie_fit) Then
               If (.not.Allocated(t)) Then
                  Call get_fitted (iru, .false.)
               Else
                  Call get_fitted (iru, .false., &
                       t=t(1:iru))
               End If
            End If
! - add persistence component to hindcasts -
            If (lprst) Then
               y(1:mya,1:iru,1) = ybk(:,1:iru,1) ! - restore untransformed observations -
               Select Case (igauss)
                Case (it_non) ! - no transformation -
                  Call add_persistence (iru, .false., .false., .true., yfile%bdate, &
                       ybk(:,1:iru,0), y(1:mya,1:iru,1), y(1:mya,1:iru,2))  ! - replace persistence component to observations -
                  If (ievc == ie_fit) Then
                     Call add_persistence (iru, .false.,  .true., .true., yfile%bdate, &
                          ybk(:,1:iru,0), yfit(:,1:iru,1), yfit(:,1:iru,2)) ! - fitted values -
                  End If
                  Call add_persistence (iru, .false.,  .true., .true., yfile%bdate, &
                       ybk(:,1:iru,0), ycv(:,1:iru,1), ycv(:,1:iru,2))      ! - hindcasts -
                Case Default ! - transformation -
                  ncru = Min(iru, ncu(0))
                  Call add_persistence (iru, .false., .false., .true., yfile%bdate, &
                       ybk(:,1:iru,0), y(1:mya,1:iru,1), y(1:mya,1:iru,2), &    ! - replace persistence component to observations -
                       nr=ncru, clim=clim(1:mya,1:ncru,0:), ft=yt(:,1:iru))
                  If (ievc == ie_fit) Then
                     Call add_persistence (iru,  .true.,  .true., .true., yfile%bdate, &
                          ybk(:,1:iru,0), yfit(:,1:iru, 1), yfit(:,1:iru, 2), & ! - fitted values -
                          nr=ncru, clim=clim(1:mya,1:ncru,0:), ft=yfitt(:,1:iru))
                  End If
                  Call add_persistence (iru, .false.,  .true., .true., yfile%bdate, &
                       ybk(:,1:iru,0), ycv(:,1:iru,1), ycv(:,1:iru,2), &        ! - hindcasts -
                       nr=ncru, clim=clim(1:mya,1:ncru,0:), ft=ycvt(:,1:iru))
               End Select
            Else If (igauss /= it_non) Then
               yt(:,1:iru) = yopt(:,1:iru,2)
            End If
! - calculate skill masking -
            If (iskmask > 0) Call skillmask (ievc, iru, nr)
! - calculate error-variance of hindcasts -
            Call calc_errvar (ievc, nxo, iru, nr)
! - produce retroactive deterministic forecasts -
            If (.not.Allocated(t)) Then
               Select Case (ianal)
                Case Default
                  Call calc_predictions_d (ianal, nr, iskmask, .false., .false., x(1:mxa,nu1+ir1:nu1+irn,1:1), yret(:,ir1:irn,1), &
                       nt=iru, xvp1=xvp(1,ir1:irn))
                Case (ia_mlr)
                  Call calc_predictions_d (ianal, nr, iskmask, .false., .false., x(1:mxa,nu1+ir1:nu1+irn,1:1), yret(:,ir1:irn,1), &
                       nt=iru, xvp1=xvp(1,ir1:irn), u2=u(1,:,:))
                Case (ia_gcm)
                  Call calc_predictions_d (ianal, nr, iskmask, .false., .false., xiny(1:mya,nu1+ir1:nu1+irn,:), yret(:,ir1:irn,1), &
                       nt=iru, xvp2=xvp(:,ir1:irn), u3=u(:,:,:))
               End Select
            Else
               Select Case (ianal)
                Case Default
                  Call calc_predictions_d (ianal, nr, iskmask, .false., .false., x(1:mxa,nu1+ir1:nu1+irn,1:1), yret(:,ir1:irn,1), &
                       nt=iru, t=t(nu1+ir1:nu1+irn), xvp1=xvp(1,ir1:irn))
                Case (ia_mlr)
                  Call calc_predictions_d (ianal, nr, iskmask, .false., .false., x(1:mxa,nu1+ir1:nu1+irn,1:1), yret(:,ir1:irn,1), &
                       nt=iru, t=t(nu1+ir1:nu1+irn), xvp1=xvp(1,ir1:irn), u2=u(1,:,:))
                Case (ia_gcm)
                  Call calc_predictions_d (ianal, nr, iskmask, .false., .false., xiny(1:mya,nu1+ir1:nu1+irn,:), yret(:,ir1:irn,1), &
                       nt=iru, t=t(nu1+ir1:nu1+irn), xvp2=xvp(:,ir1:irn), u3=u(:,:,:))
               End Select
            End If
            If (lstop) Return
! - add persistence component to retroactive forecasts -
            If (lprst) Then
               Select Case (igauss)
                Case (it_non) ! - no transformation -
                  Call add_persistence (nr, .false., .true., .true., bdate, &
                       ybk(:,nu1+ir1:nu1+irn,0), yret(:,ir1:irn,1), yret(:,ir1:irn,2))
                Case Default ! - transformation -
                  Call add_persistence (nr, .true., (istd /= is_sti), .true., bdate, &
                       ybk(:,nu1+ir1:nu1+irn,0), yret(:,ir1:irn,1), yret(:,ir1:irn,2), &
                       nr=ncru, clim=y(1:mya,1:ncru,1:), ft=yrett(:,ir1:irn))
               End Select
            Else If (igauss /= 0) Then
               yrett(:,ir1:irn) = yret(:,ir1:irn,1)
            End If
! - calculate prediction-error variance -
            Call calc_pesd (iru, nxo, nr, mya, (ianal == ia_gcm), ys2(:), xvp(:,ir1:irn), dofr, pesd_r(:,ir1:irn))
            If (lstop) Return
! - calculate probabilities and prediction intervals -
            Select Case (igauss)
             Case (it_non) ! - no transformation -
               Call calc_predictions_p (ianal, nr, nenf, iru, .false., dofr, pesd_r(:,ir1:irn), y(1:mya,1:iru,1), &
                    yret(:,ir1:irn,nsnc), ens_r(:,ir1:irn,1:), fps_r(:,ir1:irn,:), pls_r(:,ir1:irn), odds_r(:,ir1:irn,:,:))
             Case Default ! - transformation -
               Call calc_predictions_p (ianal, nr, nenf, iru, .false., dofr, pesd_r(:,ir1:irn), y(1:mya,1:iru,1), &
                    yrett(:,ir1:irn), ens_r(:,ir1:irn,1:), fps_r(:,ir1:irn,:), pls_r(:,ir1:irn), odds_r(:,ir1:irn,:,:))
! - untransform the forecasts, unless the SPI is used  -
               If (istd /= is_sti) Then
                     Call untransform_f (iru, nr, nenf, 1, y(1:mya,1:iru,1:), yret(:,ir1:irn,1:), &
                          pls_r(:,ir1:irn), ens_r(:,ir1:irn,:))
               End If
            End Select
            If (lstop) Return
! - apply data bounds -
            If (izero == 1) Call forecast_bounds (nr, nenf, yret(:,ir1:irn,1:), pls_r(:,ir1:irn), ens_r(:,ir1:irn,:))
            Print *, ' '
            If (gifile%cfile%lset) Call print_text (' ', &
                                        iout=iout)
         End If
! - update indices -
         iru = nu1 + irn
         ir1 = irn + 1
      End Do retro
! - restore standardized Y -
      If (lprst) Then
         If (istd /= is_non) Then
            y(1:mya,1:nu,1) = ybk(:,1:nu,1)
            Call stdize (istd, igauss, 1, mya, nu, ave(:,1), sdev(:,1), y(1:mya,1:nu,1))
         End If
      End If
   End If
!
! Optimise using full training period
   ncv = nu - lcw
   ir = ir + 1
   If (nopt > 0) Then
      Print *, 'Optimizing cross-validated performance ...'
      Call print_training_period (nt)
      Select Case (igauss)
       Case (it_non)
         Call opt_analysis (nu, .true., ycv(:,:,1))
       Case Default
         Call opt_analysis (nu, .true., ycv(:,:,1), &
              ycvt=ycvt(:,:))
      End Select
!
! Cross-validate optimal / chosen model
   Else
      Print *, 'Cross-validating model ...'
      Call print_training_period (nt)
      Select Case (igauss)
       Case (it_non)
         Call cv_analysis (nu, nxo, nxo, nyo, nyo, nco, nco, ycv(:,:,1))
       Case Default
         Call cv_analysis (nu, nxo, nxo, nyo, nyo, nco, nco, ycv(:,:,1), &
              ycvt=ycvt(:,:))
      End Select
   End If
   If (lstop) Return
!
! Close goodness file
   If (gifile%cfile%lset) Then
      Close (Unit=iout)
      Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
   End If
!
! Allocate additional memory
   Select Case (ianal)
    Case (ia_cca)
      nccu = Min(nxo, nyo, mcc)
      Call init_analysis2 (ianal, ifail, &
           nco=nccu)
    Case (ia_pcr, ia_mlr, ia_gcm)
      Call init_analysis2 (ianal, ifail)
   End Select
   If (ifail /= 0) Then
      perform = 1
      Return
   End If
!
! Fit model using all data
   Print *, 'Constructing model using full training period ('//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)//') ...'
   Select Case (igauss)
    Case (it_non) ! - no transformation -
      Call full_analysis (nu, nco)
    Case Default ! - transformation -
      Call full_analysis (nu, nco, &
           yt=yt(:,:))
   End Select
   If (lstop) Return
   If (ifail /= 0) Then
      perform = 2
      Return
   End If
!
! Calculate uncalculated moments
   Select Case (ianal)
    Case (ia_pcr)
      Call moments (mya, nu, y(1:mya,:,1), ym, &
           vsd=ysd(:)) ! - calculate Y standard deviation -
    Case (ia_mlr)
      Call moments (mxa, nu, x(1:mxa,:,1), xm, &
           vsd=xsd(:)) ! - calculate X mean and standard deviation -
      Call moments (mya, nu, y(1:mya,:,1), ym, &
           vsd=ysd(:)) ! - calculate Y standard deviation -
   End Select
!
! Calculate fitted values
   Print *, 'Calculating fitted values and variances of hindcast errors ...'
   If (.not.Allocated(t)) Then
      Call get_fitted (nu, .true.)
   Else
      Call get_fitted (nu, .true., &
           t=t(:))
   End If
!
! Add persistence component to hindcasts
   If (lprst) Then
      y(1:mya,1:nu,1) = ybk(:,:,1)                            ! - restore unstandardized value -
      Select Case (igauss)
       Case (it_non) ! - no transformation -
         Call add_persistence (nu, .false., .false., .true., yfile%bdate, &
              ybk(:,:,0), y(1:mya,1:nu,1), y(1:mya,1:nu,2))   ! - replace persistence component to observations -
         Call add_persistence (nu, .false.,  .true., .true., yfile%bdate, &
              ybk(:,:,0), yfit(:,:,1), yfit(:,:,2))           ! - fitted values -
         Call add_persistence (nu, .false.,  .true., .true., yfile%bdate, &
              ybk(:,:,0), ycv(:,:,1), ycv(:,:,2))             ! - hindcasts -
       Case Default ! - transformation -
         Call add_persistence (nu, .false., .false., .true., yfile%bdate, &
              ybk(:,:,0), y(1:mya,1:nu,1), y(1:mya,1:nu,2), & ! - replace persistence component to observations -
              nr=ncu(0), clim=clim(1:mya,1:ncu(0),0:), ft=yt(:,:))
         Call add_persistence (nu,  .true.,  .true., .true., yfile%bdate, &
              ybk(:,:,0), yfit(:,:,1), yfit(:,:,2), &         ! - fitted values -
              nr=ncu(0), clim=clim(1:mya,1:ncu(0),0:), ft=yfitt(:,:))
         Call add_persistence (nu, .false.,  .true.,  .true., yfile%bdate, &
              ybk(:,:,0), ycv(:,:,1), ycv(:,:,2), &           ! - hindcasts -
              nr=ncu(0), clim=clim(1:mya,1:ncu(0),0:), ft=ycvt(:,:))
      End Select
   Else If (igauss /= it_non) Then
      yfitt(:,:) = yfit(:,:,1)
   End If
!
! Calculate skill masking and variance of hindcast errors
   Select Case (iev)
    Case Default
      ievc = iev
    Case (ie_cv2)
      ievc = ie_cvh
   End Select
   If (iskmask > 0) Call skillmask (ievc, nu, nur)
   Call calc_errvar (ievc, nxo, nu, nur)
   perform = 0
!
   Return
!
 Contains
!
!
  Subroutine print_training_period (n)
!
! Prints training period
!
! Modules
   Use data_io_constants, Only: io_com
   Use time,              Only: Operator(+), &
                                get_cdate
   Use gui,               Only: print_text
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - length of training period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Identify training period
   prdc = yfile%prd1 + (yfile%it1 + n - 2)
   cprd2 = get_cdate(prdc, 1, io_com)
!
! Print training period
   Print *, Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)
   Print *, ' '
! - save to goodness index file -
   If (gifile%cfile%lset) Then
      Call print_text (Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2), &
           iout=iout)
      Call print_text (' ', &
           iout=iout)
   End If
!
   Return
  End Subroutine print_training_period
!
!
!
  Subroutine opt_analysis (n, lstd, ycv, &
             ycvt)
!
! Optimises cross-validated analysis
!
! Modules
   Use data_cpt_constants, Only: is_sti
   Use settings,           Only: istd
   Use distribs,           Only: transform_cv
   Use pcs,                Only: ieofx, ieofy
   Use climate,            Only: ave, sdev
   Use cca,                Only: cv_cca
   Use pcr,                Only: cv_pcr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Logical, Intent(In) :: lstd ! - transform? -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: ycv  ! - cross-validated hindcasts -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: ycvt ! - transformed cross-validated hindcasts -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Present
!
! Executable Statements
!
! Optimise cross-validated analysis
   Select Case (ianal)
    Case (ia_cca)
      Call cv_cca (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
           lopt(:,ir), ycv(:,:), &
           nxo=nxo, nyo=nyo, nco=nco)
    Case (ia_pcr)
      If (.not.Allocated(t)) Then
         Call cv_pcr (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, lopt(:,ir), ycv(:,:), &
              nxo=nxo)
      Else
         Call cv_pcr (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, lopt(:,ir), ycv(:,:), &
              nxo=nxo, t=t(:))
      End If
   End Select
   If (Present(ycvt)) ycvt(:,1:n) = ycv(:,1:n)
   If (lstd .and. istd /= is_sti) Call transform_cv (mya, n, ncv, ave(:,1), sdev(:,1), ycv(:,1:n)) ! - transform -
!
   Return
  End Subroutine opt_analysis
!
!
!
  Subroutine cv_analysis (n, nxe, mxe, nye, mye, ncc, mcc, ycv, &
             ycvt)
!
! Performs cross-validated analysis
!
! Modules
   Use data_cpt_constants, Only: is_sti
   Use settings,           Only: istd
   Use distribs,           Only: transform_cv
   Use pcs,                Only: ieofx, ieofy
   Use climate,            Only: ave, sdev
   Use cca,                Only: cv_cca
   Use mlr,                Only: cv_mlr
   Use pcr,                Only: cv_pcr
   Use gcm,                Only: cv_gcm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: mcc ! - maximum number of CCA modes -
   Integer, Intent(In) :: mxe ! - maximum number of X EOF modes -
   Integer, Intent(In) :: mye ! - maximum numr of Y EOF modes -
   Integer, Intent(In) :: ncc ! - number of CCA modes -
   Integer, Intent(In) :: nxe ! - number of X EOF modes -
   Integer, Intent(In) :: nye ! - number of Y EOF modes -
!
! - output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: ycv ! - cross-validated hindcasts -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: ycvt ! - transformed cross-validated hindcasts -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Present
!
! Executable Statements
!
! Perform cross-validated analysis
   Select Case (ianal)
    Case (ia_cca)
      Call cv_cca (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
           lopt(:,ir), ycv(:,:))
    Case (ia_pcr)
      If (.not.Allocated(t)) Then
         Call cv_pcr (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, lopt(:,ir), ycv(:,:))
      Else
         Call cv_pcr (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, lopt(:,ir), ycv(:,:), &
              t=t(:))
      End If
    Case (ia_mlr)
      If (.not.Allocated(t)) Then
         Call cv_mlr (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ir, b0(:,1), b(:,:), ycv(:,:))
      Else
         Call cv_mlr (n, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ir, b0(:,1), b(:,:), ycv(:,:), &
              t=t(:))
      End If
    Case (ia_gcm)
      If (.not.Allocated(t)) Then
         Call cv_gcm (imc, xfile%nms, n, ncv, mya, ig0, xiny(:,:,:), y(1:mya,:,1), ir, b0(ig0:,:), b(ig0:,:), ycv(:,:))
      Else
         Call cv_gcm (imc, xfile%nms, n, ncv, mya, ig0, xiny(:,:,:), y(1:mya,:,1), ir, b0(ig0:,:), b(ig0:,:), ycv(:,:), &
              t=t(:))
      End If
   End Select
   If (Present(ycvt)) ycvt(:,1:n) = ycv(:,1:n)
   If (istd /= is_sti) Call transform_cv (mya, n, ncv, ave(:,1), sdev(:,1), ycv(:,1:n)) ! - transform -
!
   Return
  End Subroutine cv_analysis
!
!
!
  Subroutine full_analysis (n, nco, &
             yt)
!
! Performs full analysis
!
! Modules
   Use data_cpt_constants, Only: it_emp, it_gam, it_non
   Use settings,           Only: igauss
   Use arrays,             Only: xc, ybk, yc
   Use distribs,           Only: gaussian, get_gamma
   Use pcs,                Only: ieofx, ieofy, hx_map, hx_ser, hy_map, hy_ser
   Use regression,         Only: b0, b
   Use climate,            Only: clim, nc
   Use cca,                Only: full_cca
   Use mlr,                Only: full_mlr
   Use pcr,                Only: full_pcr
   Use gcm,                Only: full_gcm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nco ! - number of CCA modes -
!
! Optional arguments
! - optional output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yt ! - transformed response variables -
!
! Functions and Subroutines
!
! Intrinsic routines
   Intrinsic Allocated
!
! Executable Statements
!
! Update gamma parameters
   If (igauss == it_gam) Then
      Call get_gamma (mya, n, 1, ybk(1:mya,1:n,1))
      If (lprst) Call get_gamma (mya, n, 2, ybk(1:mya,1:n,2))
   End If
!
! Backup and transform data
   If (ianal /= ia_gcm) xc(:,1:n,:) = x(1:mxa,1:n,:)
   Select Case (igauss)
    Case (it_non)
      yc(:,1:n) = y(1:mya,1:n,1)
    Case (it_emp)
      yc(:,1:n) = y(1:mya,1:n,1)
      Call gaussian (igauss, 1, mya, n, yc(:,1:n), &
           nr=nc, ref=clim(1:mya,1:nc,1))
    Case (it_gam)
      yc(:,1:n) = ybk(1:mya,1:n,1)
      Call gaussian (igauss, 1, mya, n, yc(:,1:n), &
           nr=nc, ref=clim(1:mya,1:nc,1))
   End Select
!
! Copy transformed data
   If (Present(yt)) yt(:,1:n) = yc(:,1:n)
!
! Perform full analysis
   Select Case (ianal)
    Case (ia_cca)
      Call full_cca (n, mxa, xc(:,1:n,1), mya, yc(:,1:n), ieofx, nxo, svx(:), eofx(:,:), tsx(:,:), &
           ieofy, nyo, svy(:), eofy(:,:), tsy(:,:), nco, mu(:), r(:,:), s(:,:), ifail, &
           hx_map=hx_map(:,:), hy_map=hy_map(:,:), hx_ser=hx_ser(:,:), hy_ser=hy_ser(:,:))
    Case (ia_pcr)
      If (.not.Allocated(t)) Then
         Call full_pcr (n, mxa, xc(:,1:n,1), mya, yc(:,1:n), ieofx, nxo, svx(:), eofx(:,:), tsx(:,:), bz(0:,:), npx, ifail, &
              b0=b0(:,1), b=b(:,:))
      Else
         Call full_pcr (n, mxa, xc(:,1:n,1), mya, yc(:,1:n), ieofx, nxo, svx(:), eofx(:,:), tsx(:,:), bz(0:,:), npx, ifail, &
              b0=b0(:,1), b=b(:,:), t=t(1:n))
      End If
    Case (ia_mlr)
      If (.not.Allocated(t)) Then
         Call full_mlr (n, mxa, xc(:,1:n,1), mya, yc(:,1:n), b0(:,1), b(:,:), u(1,:,:), ifail)
      Else
         Call full_mlr (n, mxa, xc(:,1:n,1), mya, yc(:,1:n), b0(:,1), b(:,:), u(1,:,:), ifail, &
              t=t(1:n))
      End If
    Case (ia_gcm)
      If (.not.Allocated(t)) Then
         Call full_gcm (imc, xfile%nms, n, mya, ig0, xiny(:,:,:), yc(:,1:n), b0(ig0:,:), b(ig0:,:), u(:,:,:))
      Else
         Call full_gcm (imc, xfile%nms, n, mya, ig0, xiny(:,:,:), yc(:,1:n), b0(ig0:,:), b(ig0:,:), u(:,:,:), &
              t=t(1:n))
      End If
   End Select
!
   Return
  End Subroutine full_analysis
 End Function perform_analysis
!
!
!
 Subroutine get_fitted (n, lstd, &
            t)
!
! Calculates fitted values
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm
   Use settings,           Only: istd, lzb
   Use arrays,             Only: x, xiny
   Use iofiles,            Only: mxa, mya
   Use distribs,           Only: zero_bound
   Use regression,         Only: yfit
   Use climate,            Only: ave, sdev
   Use prediction,         Only: calc_predictions_d
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of fitted values -
!
   Logical, Intent(In) :: lstd ! - transform? -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t ! - binomial coefficients -
!
! Executable Statements
!
! Calculate fitted values
   Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr) ! - CCA, PCR, MLR -
      Call calc_predictions_d (ianal, n, 0, .false., lstd, x(1:mxa,1:n,1:1), yfit(:,1:n,1), &
           t=t)
    Case (ia_gcm) ! - GCM -
      Call calc_predictions_d (ianal, n, 0, .false., lstd, xiny(1:mya,1:n,:), yfit(:,1:n,1), &
           t=t)
   End Select
   If (lzb) Call zero_bound (istd, mya, n, ave(:,1), sdev(:,1), yfit(:,1:n,1)) ! - apply zero-bound to fitted values -
!
   Return
 End Subroutine get_fitted
!
!
!
 Subroutine proj_read_analysis (iin, lres, ianalp, ifail)
!
! Reads program analysis from project file and initialises memory to read results
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, id_grd, id_stn, id_ind, ng
   Use settings,           Only: iskmask, nenf, nret
   Use arrays,             Only: lconvert
   Use iofiles,            Only: xfile, yfile
   Use fields,             Only: xfield, yfield
   Use pcs,                Only: mcc, nxo, nyo
   Use scores,             Only: corrmsk, gm
   Use season,             Only: nsnc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
!
   Logical, Intent(In) :: lres ! - read results? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ianalp ! - project analysis -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Max, MaxVal, Min
!
! Executable Statements
!
! Read analysis settings
   If (.not.lres) Then
      Read (Unit=iin, Err=1, End=2) ianalp, icalc, lconvert
!
! Initialise memory
   Else
      Select Case (ianalp)
       Case (ia_cca) ! - CCA -
         Call init_analysis1 (ianalp, .true., ng, nsnc, nret, nenf, nopt, xfile%nms, ifail)
         If (ifail /= 0) GoTo 4
         Call init_analysis2 (ianalp, ifail, &
              nco=Min(nxo, nyo, mcc))
         If (ifail /= 0) GoTo 4
         ihind = 1
       Case (ia_pcr, ia_mlr) ! - PCR, MLR -
         Call init_analysis1 (ianalp, .true., ng, nsnc, nret, nenf, nopt, xfile%nms, ifail)
         If (ifail /= 0) GoTo 4
         Call init_analysis2 (ianalp, ifail)
         If (ifail /= 0) GoTo 4
         ihind = 1
       Case (ia_gcm) ! - GCM -
         Select Case (yfile%idstr)
          Case (id_grd)
            Call init_analysis1 (ianalp, .true., ng, nsnc, 0, nenf, nopt, xfile%nms, ifail, &
                 nlt=MaxVal(yfield(:)%region%nlts), nlg=MaxVal(yfield(:)%region%nlgs))
          Case (id_stn)
            Call init_analysis1 (ianalp, .true., ng, nsnc, 0, nenf, nopt, xfile%nms, ifail, &
                 nlt=MaxVal(yfield(:)%nva), nlg=MaxVal(yfield(:)%nva))
          Case (id_ind)
            Call init_analysis1 (ianalp, .true., ng, nsnc, 0, nenf, nopt, xfile%nms, ifail)
         End Select
         If (ifail /= 0) GoTo 4
         Call init_analysis2 (ianalp, ifail)
         If (ifail /= 0) GoTo 4
         ihind = 1
      End Select
      Call init_results (ianalp, Max(MaxVal(xfield(:)%nlt), MaxVal(yfield(:)%nlt)), &
           Max(MaxVal(xfield(:)%nlg), MaxVal(yfield(:)%nlg)), ifail)
      If (ifail /= 0) GoTo 4
!
! Read optimization settings
      Read (Unit=iin, Err=1, End=2) nopt
      If (Allocated(lopt)) Deallocate (lopt)
      If (Allocated(gm))   Deallocate (gm)
      If (nopt > 1) Then
         Call init_opt (nopt, nret, ifail)
         If (ifail /= 0) GoTo 4
         Read (Unit=iin, Err=1, End=2) lopt
         Read (Unit=iin, Err=1, End=2) gm
      End If
!
! Read skill masking
      If (iskmask > 0) Read (Unit=iin, Err=1, End=2) corrmsk
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
! - problem initializing memory for results -
4  ifail = 4
   Return
!
 End Subroutine proj_read_analysis
!
!
!
 Subroutine proj_write_analysis (iout, lres, ifail)
!
! Writes analysis settings to project file
!
! Modules
   Use settings, Only: iskmask
   Use arrays,   Only: lconvert
   Use scores,   Only: corrmsk, gm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
!
   Logical, Intent(In) :: lres ! - write results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write analysis settings
   If (.not.lres) Then
      Write (Unit=iout, Err=1) ianal, icalc, lconvert
!
! Write optimization settings
   Else
      Write (Unit=iout, Err=1) nopt
      If (nopt > 1) Then
         Write (Unit=iout, Err=1) lopt
         Write (Unit=iout, Err=1) gm
      End If
!
! Read skill masking
      If (iskmask > 0) Write (Unit=iout, Err=1) corrmsk
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
 End Subroutine proj_write_analysis
!
!
!
 Function close_analysis() &
          Result (fclose)
!
! Frees memory allocated for analysis
!
! Modules
   Use arrays,     Only: awk, iwk, rwk, rwk2, tc, xc, xiny, xm, xsd, ybk, yc, ym, ysd, yt
   Use space,      Only: close_space
   Use fields,     Only: imfs
   Use pcs,        Only: close_svds
   Use regression, Only: b, b0, ens_r, pesd_r, pls_r, u, xvp, ycv, ycvt, yfit, yfitt, yopt, yret, yrett, ys2
   Use scores,     Only: corrmsk, gm, &
                         close_scores
   Use missing,    Only: close_missing
   Use categories, Only: efps, fps_r, odds_r, &
                         close_categories
   Use climate,    Only: close_climate
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
! Free memory allocated for scoring
   fclose = close_scores()
!
! Free other memory
   If (Allocated(rwk2))    Deallocate (rwk2)
   If (Allocated(rwk))     Deallocate (rwk)
   If (Allocated(awk))     Deallocate (awk)
   If (Allocated(iwk))     Deallocate (iwk)
   If (Allocated(yopt))    Deallocate (yopt)
   If (Allocated(xvp))     Deallocate (xvp)
   If (Allocated(tc))      Deallocate (tc)
   If (Allocated(yc))      Deallocate (yc)
   If (Allocated(xc))      Deallocate (xc)
   If (Allocated(ybk))     Deallocate (ybk)
   If (Allocated(corrmsk)) Deallocate (corrmsk)
   If (Allocated(b))       Deallocate (b)
   If (Allocated(b0))      Deallocate (b0)
   If (Allocated(xiny))    Deallocate (xiny)
   If (Allocated(u))       Deallocate (u)
   fclose = close_space()
   If (Allocated(odds_r))  Deallocate (odds_r)
   If (Allocated(fps_r))   Deallocate (fps_r)
   If (Allocated(imfs))    Deallocate (imfs)
   If (Allocated(pls_r))   Deallocate (pls_r)
   If (Allocated(pesd_r))  Deallocate (pesd_r)
   If (Allocated(yrett))   Deallocate (yrett)
   If (Allocated(ens_r))   Deallocate (ens_r)
   If (Allocated(yret))    Deallocate (yret)
   If (Allocated(efps))    Deallocate (efps)
   If (Allocated(gm))      Deallocate (gm)
   If (Allocated(lopt))    Deallocate (lopt)
   If (Allocated(ys2))     Deallocate (ys2)
   If (Allocated(yfitt))   Deallocate (yfitt)
   If (Allocated(ycvt))    Deallocate (ycvt)
   If (Allocated(yt))      Deallocate (yt)
   If (Allocated(yfit))    Deallocate (yfit)
   If (Allocated(ycv))     Deallocate (ycv)
   If (Allocated(ysd))     Deallocate (ysd)
   If (Allocated(xsd))     Deallocate (xsd)
   If (Allocated(ym))      Deallocate (ym)
   If (Allocated(xm))      Deallocate (xm)
!
! Free memory allocated for climatologies
   fclose = close_climate()
!
! Free memory allocated for categories
   fclose = close_categories()
!
! Free memory allocated for SVDs
   fclose = close_svds()
!
! Free memory allocated for missing values
   fclose = close_missing()
!
   fclose = 0
!
   Return
 End Function close_analysis
!
!
!
 Function close_calc() &
          Result (fclose)
!
! Frees memory allocated for data and analysis
!
! Modules
   Use arrays, Only: close_data
!
! Function result
   Integer :: fclose
!
! Executable Statements
!
! Free memory allocated for analysis
   fclose = close_analysis()
!
! Free memory allocated for double cross-validation
   fclose = close_cv2results()
!
! Free memory allocated for double cross-validation
   fclose = close_cv2()
!
! Free memory for dates and data
   fclose = close_data()
!
   fclose = 0
!
   Return
 End Function close_calc
!
!
!
 Function close_cv2() &
          Result (fclose)
!
! Frees memory for double cross-validation
!
! Modules
   Use arrays,     Only: xin, yin
   Use prediction, Only: close_predict
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
   fclose = close_predict()
   If (Allocated(yin)) Deallocate (yin)
   If (Allocated(xin)) Deallocate (xin)
!
   fclose = 0
!
   Return
 End Function close_cv2
!
!
!
 Function close_cv2results() &
          Result (fclose)
!
! Frees memory for double cross-validation results
!
! Modules
   Use regression, Only: ens_2, pesd_2, pls_2, ycv2, ycv2t
   Use categories, Only: fps_2, odds_2
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
   If (Allocated(odds_2)) Deallocate (odds_2)
   If (Allocated(fps_2))  Deallocate (fps_2)
   If (Allocated(pls_2))  Deallocate (pls_2)
   If (Allocated(pesd_2)) Deallocate (pesd_2)
   If (Allocated(ycv2t))  Deallocate (ycv2t)
   If (Allocated(ens_2))  Deallocate (ens_2)
   If (Allocated(ycv2))   Deallocate (ycv2)
!
   fclose = 0
!
   Return
 End Function close_cv2results
End Module analysis
