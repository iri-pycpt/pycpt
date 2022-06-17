! Author: Simon Mason
Module cpt_update
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: reset_limits, reset_limits_nf, reset_limits_nt, toggle_elev, toggle_fordia, update_errvar, update_mask, &
             update_tailoring
#if GUI == 1
   Public :: init_image_sizes
#endif
!
Contains
!
!
 Function toggle_elev() &
          Result (toggle)
!
! Toggles the expertise level and adjusts relevant menu flags
!
! Modules
   Use menus,    Only: set_menu_greying
   Use settings, Only: isynch, lexpert
   Use iofiles,  Only: xfile
   Use analysis, Only: ianal
   Use cpt_view, Only: set_grey_view
!
! Function result
   Integer :: toggle
!
! Executable Statements
!
! Toggle the expertise level
   lexpert = .not.lexpert
!
! Adjust relevant menu flags
   Call set_grey_view (ianal, isynch, xfile%idstr)
   Call set_menu_greying (lexpert=lexpert)
   toggle = 2
!
   Return
 End Function toggle_elev
!
!
!
 Function toggle_fordia() &
          Result (toggle)
!
! Toggles the forecast and diagnostics options
!
! Modules
   Use menus,    Only: set_menu_greying
   Use settings, Only: isynch
!
! Function result
   Integer :: toggle
!
! Executable Statements
!
! Toggle the forecast and diagnsotics options
   isynch = 1 - isynch
!
! Adjust relevant menu flags
   Call set_menu_greying (idia=1-isynch, ifor=isynch) ! - View -
   toggle = 2
!
   Return
 End Function toggle_fordia
!
!
!
 Subroutine reset_limits (icsv, lprst, lreset, lensn, lsn, lb, nsn)
!
! Resets training-period and forecast-period limits
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icsv  ! - calculate seasonal values flag -
   Integer, Intent(In) :: lb    ! - lower bound -
   Integer, Intent(In) :: lensn ! - length of season -
   Integer, Intent(In) :: nsn   ! - number of seasons -
!
   Logical, Intent(In) :: lprst  ! - persistence component? -
   Logical, Intent(In) :: lreset ! - attempt to reset dates if number of forecasts is zero -
!
! - input arrays -
   Integer, Dimension(lb:), Intent(In) :: lsn ! - lengths of seasons -
!
! Executable Statements
!
! Reset training-period limits
   Call reset_limits_nt (icsv, lensn, lsn, lb, nsn)
!
! Reset forecast-period limits
   Call reset_limits_nf (icsv, lprst, lreset, lsn, lb)
!
   Return
 End Subroutine reset_limits
!
!
!
 Subroutine reset_limits_nf (icsv, lprst, lreset, lsn, lb)
!
! Resets forecast-period limits
!
! Modules
#if GUI == 1
   Use clrwin$,             Only: set_integer_limits$
   Use data_numbers,        Only: ihuge
#endif
   Use data_io_constants,   Only: io_com
   Use data_time_constants, Only: isq_mn, nmn
   Use time,                Only: pdate, &
                                  Operator(+), Operator(<), &
                                  date_diff, get_cdate
   Use screen,              Only: window_update
   Use gui,                 Only: ih_nf
   Use settings,            Only: nf, &
                                  get_maxn
   Use iofiles,             Only: mfile, xfile, yfile, zfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icsv ! - calculate seasonal values flag -
   Integer, Intent(In) :: lb   ! - lower bound -
!
   Logical, Intent(In) :: lprst  ! - persistence component? -
   Logical, Intent(In) :: lreset ! - attempt to reset dates if number of forecasts is zero -
!
! - input arrays -
   Integer, Dimension(lb:), Intent(In) :: lsn ! - lengths of seasons -
!
! Locals
!
! Local scalars
   Integer :: nfm ! - number of cases in monthly file to read -
   Integer :: d   ! - difference betwen X and Z start-at dates -
!
   Type(pdate) :: d1 ! - new fdate -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Mod
!
! Executable Statements
!
! Reset training-period limits
   If (zfile%cfile%lset) Then
1     d = date_diff(xfile%fdate, zfile%fdate, xfile%iseq)
      If (.not.lprst) Then
         Call get_maxn (   0, zfile%cfile%lset,          .false.,   lprst, xfile%iseq, yfile%iseq, zfile%iseq,      0,       0,   &
                     0, zfile%fdate, yfile%fdate+d,   zfile%bdate, zfile%prdn%sdate, yfile%prdn%sdate, zfile%prdn%sdate, nf, nfm, &
              ih=ih_nf, mn=1)
      Else
         Call get_maxn (icsv, zfile%cfile%lset, yfile%cfile%lset, .false., xfile%iseq, yfile%iseq, mfile%iseq, lsn(0), lsn(lb),   &
                     0, zfile%fdate, yfile%fdate+d, mfile%bdate+d, zfile%prdn%sdate, yfile%prdn%sdate, mfile%prdn%sdate, nf, nfm, &
              ih=ih_nf, mn=1)
          If (lreset .and. nf < 1) Then ! - adjust Z start-at date if number of forecasts is 0 -
             d = -nfm/nmn
             If (Mod(-nfm,nmn) < lsn(lb)) d = d + 1
             d1 = zfile%fdate
             Select Case (d)
              Case (1:)
                d1%iyr = d1%iyr - d
              Case Default
                d1%iyr = d1%iyr - 1
             End Select
             If (d1 < zfile%prd1%sdate) Return
             zfile%fdate = d1
             zfile%bdate = d1
             zfile%cprdb = get_cdate(zfile%bdate, date_diff(zfile%prd1%sdate, zfile%prd1%edate, isq_mn)+1, io_com)
             Call window_update (zfile%cprdb)
             GoTo 1
          End If
      End If
   Else
#if GUI == 1
      Call set_integer_limits$ (ih_nf, 0, ihuge)
#endif
      nf = 0
   End If
   Call window_update (nf)
!
   Return
 End Subroutine reset_limits_nf
!
!
!
 Subroutine reset_limits_nt (icsv, lensn, lsn, lb, nsn)
!
! Resets training-period limits
!
! Modules
#if GUI == 1
   Use clrwin$,      Only: set_integer_limits$
   Use data_numbers, Only: ihuge
#endif
   Use screen,       Only: window_update
   Use gui,          Only: ih_nt
   Use settings,     Only: mnu, nt, ntm, &
                           get_maxn
   Use iofiles,      Only: mfile, xfile, yfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icsv  ! - calculate seasonal values flag -
   Integer, Intent(In) :: lb    ! - lower bound -
   Integer, Intent(In) :: lensn ! - length of season -
   Integer, Intent(In) :: nsn   ! - number of seasons -
!
! - input arrays -
   Integer, Dimension(lb:), Intent(In) :: lsn ! - lengths of seasons -
!
! Executable Statements
!
! Reset training-period limits
   If (xfile%cfile%lset .or. yfile%cfile%lset) Then
      Call get_maxn (icsv, xfile%cfile%lset, yfile%cfile%lset, .false., xfile%iseq, yfile%iseq, mfile%iseq, lensn, lsn(lb),  &
           lsn(nsn), xfile%fdate, yfile%fdate, mfile%bdate, xfile%prdn%sdate, yfile%prdn%sdate, mfile%prdn%sdate, nt, ntm, &
           ih=ih_nt, mn=mnu)
   Else
#if GUI == 1
      Call set_integer_limits$ (ih_nt, 0, ihuge)
#endif
      nt = 0
   End If
   Call window_update (nt)
!
   Return
 End Subroutine reset_limits_nt
!
!
!
#if GUI == 1
 Subroutine init_image_sizes ()
!
! Initialises CPT settings required before CPT can prompt for options
!
! Modules
   Use screen,   Only: igsize, ihsize
   Use validate, Only: init_img_validate
   Use maps,     Only: init_img_maps
   Use forecast, Only: init_img_fcst
   Use eofs,     Only: init_img_eofs
   Use scatter,  Only: init_img_scatter
   Use verif,    Only: init_img_verify
!
! Implicit declarations
   Implicit None
!
! Executable Statements
!
! Initialise validation plots
   Call init_img_validate (igsize)
!
! Initialise maps
   Call init_img_maps (igsize)
!
! Initialise forecast images
   Call init_img_fcst (igsize, ihsize)
!
! Initialise scree plots
   Call init_img_eofs (igsize, ihsize)
!
! Initialise scatter plots
   Call init_img_scatter (igsize, ihsize)
!
! Initialise verification plots
   Call init_img_verify (igsize, ihsize)
!
   Return
 End Subroutine init_image_sizes
!
!
!
#endif
 Subroutine update_mask (ldif_mask)
!
! Updates masking
!
! Modules
   Use errors,     Only: cpt_error
   Use settings,   Only: iev, iskmask, nu, nur
   Use iofiles,    Only: mya
   Use scores,     Only: corrmsk
   Use prediction, Only: ifc, &
                         skillmask
   Use analysis,   Only: icalc
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Logical, Intent(InOut) :: ldif_mask ! - new masking correlation? -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Update masking correlations if the selected correlation has changed
   If (icalc == 1) Then
      If (iskmask > 0) Then
         If (.not.Allocated(corrmsk)) Then
            Allocate (corrmsk(mya), Stat=ifail)
            If (ifail /= 0) GoTo 1
            ldif_mask = .true.
         End If
         If (ldif_mask) Call skillmask (iev, nu, nur)
      End If
      ifc = 1
   End If
!
   Return
!
! Errors
1  iskmask = 0
   ifail = 1
   Call cpt_error ('update_mask', .false., ifail)
   Return
!
 End Subroutine update_mask
!
!
!
 Subroutine update_errvar (ldif_iev)
!
! Updates error variance
!
! Modules
   Use labels,     Only: cg_done
   Use settings,   Only: iev, iskmask, nu, nur
   Use pcs,        Only: nxo
   Use prediction, Only: ifc, &
                         calc_errvar, skillmask
   Use analysis,   Only: icalc
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical :: ldif_iev ! - changed error-variance option? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Recalculate variance of hindcast errors
   If (ifc == 2) ifc = 1
   If (ldif_iev .and. icalc == 1) Then
      Print *, 'Recalculating masking correlations ...'
      If (iskmask > 0) Call skillmask (iev, nu, nur)
      Print *, 'Recalculating variance of hindcast errors ...'
      Call calc_errvar (iev, nxo, nu, nur)
      Print *, Trim(cg_done)
   End If
!
   Return
 End Subroutine update_errvar
!
!
!
 Subroutine update_tailoring (istd_old, ldif, ldif_istd, ifail)
!
! Prompts for threshold and other tailoring settings
!
! Modules
   Use data_cpt_constants, Only: is_sti, iv_ref, nts
   Use menus,              Only: mn_opts_trnss, &
                                 set_mn_opts_trnss
   Use progress_meter,     Only: lcalc
   Use errors,             Only: cpt_error
   Use settings,           Only: igauss, igauss_bk, igto, iregr, istd
   Use arrays,             Only: insertion_sort
   Use categories,         Only: ithr, thr, &
                                 check_refdates
   Use prediction,         Only: ifc
   Use analysis,           Only: ianal, icalc
   Use verif,              Only: irv
   Use menu_actions,       Only: reset
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: istd_old ! - previous standardization option -
!
   Logical, Intent(In) :: ldif_istd ! - standardization option changed? -
   Logical, Intent(In) :: ldif      ! - any other option changed? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Update settings
! - switch on Gaussian flag using gamma distribution for SPI -
   ifail = 1
   If (ldif_istd) Then
      If (istd == is_sti) Then
         If (icalc == 1 .and. igauss /= 2) Then ! - reset if SPI selected -
            If (reset('Selecting SPI requires recalculation, and', .true.) == 1) Return
         End If
         igto = 2
         igauss_bk = igauss
         igauss = igto
      End If
      mn_opts_trnss%ion = set_mn_opts_trnss(ianal, icalc, iregr, istd)
   End If
   If (ldif .or. ldif_istd) Then
      If (ifc == 2) ifc = 1
   Else
      Return
   End If
   Call insertion_sort ('a', nts, thr(:)) ! - ensure ascending order of thresholds -
   If (icalc == 1) Then
      If (ithr == iv_ref) Then
         ifail = check_refdates()
         If (ifail /= 0) Call cpt_error ('check_refdates', .true., ifail)
      End If
      lcalc = .true.
      If (ldif) Then
         ifail = tailoring('Recalculating thresholds and categories')
      Else
         ifail = tailoring('Standardizing data and recalculating error variance')
      End If
      If (ifail /= 0) Return
      irv = 0 ! - disable verification because retroactive forecasts need to be recalculated -
      lcalc = .false.
   End If
   ifail = 0
!
   Return
!
 Contains
!
!
  Function tailoring(cprmpt)
!
! Restandardises data based on tailoring options
!
! Modules
   Use data_numbers,       Only: rp, one
   Use data_cpt_constants, Only: it_non
   Use progress_meter,     Only: end_progress_meter, set_progress_increment, start_progress_meter, update_progress_meter
   Use settings,           Only: icv2, iev, igauss, iretro, istd, nenf, nf, nu, nur
   Use arrays,             Only: y
   Use iofiles,            Only: mya
   Use distribs,           Only: restdize
   Use pcs,                Only: nxo
   Use regression,         Only: fcast, pls_p, pls_2, pls_r, ycv, ycv2, yfit, yret
   Use season,             Only: isnc, lb, nsnc
   Use categories,         Only: tobs, tobst, &
                                 calc_thresholds, set_cv_categories, set_c2_categories, set_ra_categories
   Use climate,            Only: ave, clim, ncu, sdev
   Use prediction,         Only: calc_errvar
!
! Function result
   Integer :: tailoring
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cprmpt ! - start-meter prompt -
!
! Locals
!
! Local scalars
   Integer :: ie    ! - ensemble member index -
   Integer :: isn   ! - season index -
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Confirm settings
   tailoring = -1
   Call start_progress_meter (cprmpt, .true.)
   Select Case (igauss)
    Case (it_non)
      Call set_progress_increment (one/Real(3+mya*(iretro+1), Kind=rp), .false.)
    Case Default
      Call set_progress_increment (one/Real(4+mya*(iretro+1), Kind=rp), .false.)
   End Select
! - restandardise -
   If (ldif_istd) Then
      Do isn = lb, nsnc
         Call restdize (istd_old, istd, igauss, isn, mya, nu, ave(:,isn), sdev(:,isn), y(1:mya,1:nu,isn))
         Call restdize (istd_old, istd, igauss, isn, mya, ncu(isn), ave(:,isn), sdev(:,isn), clim(1:mya,:,isn))
      End Do
      Do isn = 1, nsnc
         Call restdize (istd_old, istd, igauss, isn, mya, nu, ave(:,isn), sdev(:,isn), ycv(:,:,isn))
         If (icv2 /= 0) Call restdize (istd_old, istd, igauss, isn, mya, nu, ave(:,isn), sdev(:,isn), ycv2(:,:,isn))
         If (iretro == 1) Call restdize (istd_old, istd, igauss, isn, mya, nur, ave(:,isn), sdev(:,isn), yret(:,:,isn))
      End Do
      If (igauss == it_non) Call restdize (istd_old, istd, igauss, lb, mya, nf, ave(:,1), sdev(:,1), yfit(:,:,nsnc))
      If (ifc == 2) Then
         Do ie = 0, nenf
            Call restdize (istd_old, istd, igauss, nsnc, mya, nf, ave(:,nsnc), sdev(:,nsnc), fcast(:,:,ie))
         End Do
         Call restdize (istd_old, istd, igauss, nsnc, mya, nf, ave(:,nsnc), sdev(:,nsnc), pls_p(:,:)%lower)
         Call restdize (istd_old, istd, igauss, nsnc, mya, nf, ave(:,nsnc), sdev(:,nsnc), pls_p(:,:)%upper)
         If (icv2 /= 0) Then
            Call restdize (istd_old, istd, igauss, nsnc, mya, nu, ave(:,nsnc), sdev(:,nsnc), pls_2(:,:)%lower)
            Call restdize (istd_old, istd, igauss, nsnc, mya, nu, ave(:,nsnc), sdev(:,nsnc), pls_2(:,:)%upper)
         End If
         If (iretro == 1) Then
            Call restdize (istd_old, istd, igauss, nsnc, mya, nur, ave(:,nsnc), sdev(:,nsnc), pls_r(:,:)%lower)
            Call restdize (istd_old, istd, igauss, nsnc, mya, nur, ave(:,nsnc), sdev(:,nsnc), pls_r(:,:)%upper)
         End If
      End If
! - recalculate hindcast error-variance -
      If (igauss == it_non) Then
         Call calc_errvar (iev, nxo, nu, nur)
      End If
   End If
   If (update_progress_meter(.false.) /= 0) Return
! - redefine categories and thresholds -
   If (set_cv_categories() /= 0) Return
   If (icv2 /= 0) Then
      If (set_c2_categories() /= 0) Return
   End If
   If (iretro == 1) Then
      If (set_ra_categories(ianal, 1, 1, ncu(isnc:isnc), clim(:,:,isnc:isnc)) /= 0) Return
   End If
   Call calc_thresholds (igauss, mya, 1, ncu(isnc:isnc), clim(:,:,isnc:isnc), tobs, tobst, ifail)
   If (ifail /= 0) Return
   Call end_progress_meter (.true., .true.)
   tailoring = 0
!
   Return
  End Function tailoring
 End Subroutine update_tailoring
End Module cpt_update
