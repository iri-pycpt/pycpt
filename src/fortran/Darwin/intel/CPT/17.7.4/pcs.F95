! Author: Simon Mason
Module pcs
!
! Modules
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: i_handle
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: advanced_cca, advanced_eof, check_pcs, close_ccamaps, close_svds, get_cca_settings, get_eofs_settings,        &
             get_eofx_settings, get_eofy_settings, get_pcs, init_ccamaps, init_eofcalc, init_pc, init_pcs, init_svds,      &
             init_svdwk, make_eof_title, max_cca_modes, max_eof_modes, parameter_write_pcs, proj_read_pcs, proj_write_pcs, &
             set_grey_modes, set_scree
#if GUI == 1
   Public :: change_cca, change_eofx, change_eofy, prompt_eofs
#endif
!
! Scalars
!
! Integer scalars
   Integer, Target, Public :: ieofx ! - X EOF option (1 = correlation; 2 = covariance; 3 = sums of squares and cross-products) -
   Integer, Target, Public :: ieofy ! - Y EOF option (1 = correlation; 2 = covariance; 3 = sums of squares and cross-products) -
! - NB - ieofx and ieofy are converted to and saved as standardization coefficients: 
! - 1 (correlation)                        => 2 (standardized anomalies)
! - 2 (covariance)                         => 1 (anomalies)
! - 3 (sums of squares and cross-products) => 0 (no standardization)
!
   Integer, Pointer, Public :: ieofp ! - EOF option -
!
   Integer, Public :: icco     ! - perfect canonical correlation option flag (0 = continue; 1 = avoid) -
   Integer, Public :: iec      ! - current CCA mode -
   Integer, Public :: iex      ! - current X EOF mode -
   Integer, Public :: iey      ! - current Y EOF mode -
   Integer, Public :: lc1      ! - first dimension of ce -
   Integer, Public :: lr1      ! - first dimension of r -
   Integer, Public :: lxt      ! - minimum of nxa and nu -
   Integer, Public :: lxyt     ! - Min(Max(nxa,nya),nu) -
   Integer, Public :: lyt      ! - minimum of nya and nu -
   Integer, Public :: mcc      ! - maximum number of CCA modes -
   Integer, Public :: mxe      ! - maximum number of X EOF modes -
   Integer, Public :: mye      ! - maximum number of Y EOF modes -
   Integer, Public :: ncc      ! - number of CCA modes -
   Integer, Public :: nco      ! - optimal number of CCA modes -
   Integer, Public :: nccu     ! - number of canonical modes to calculate -
   Integer, Public :: npx      ! - number of non-zero X eigenvalues -
   Integer, Public :: npy      ! - number of non-zero Y eigenvalues -
   Integer, Public :: nxe      ! - number of X EOF modes -
   Integer, Public :: nxo      ! - optimal number of X EOF modes -
   Integer, Public :: nye      ! - number of Y EOF modes -
   Integer, Public :: nyo      ! - optimal number of Y EOF modes -
!
#if GUI == 1
   Integer, Private :: iecp ! - previous CCA mode -
   Integer, Private :: iexp ! - previous X EOF mode -
   Integer, Private :: ieyp ! - previous Y EOF mode -
!
   Integer, Parameter, Private :: ilceofs = 8 ! - size of ceofs -
!
#endif
   Integer(i_handle), Public :: ih_eof ! - handle for EOF options window -
!
! Real scalars
   Real(Kind=rp), Public :: cancor ! - canonical correlation -
!
! Logical scalars
   Logical, Public :: lcde = .false. ! - calculate EOF options? -
!
   Logical, Private :: lscree ! - scree plots? -
!
! Arrays
!
! Real arrays
   Real(Kind=rp), Dimension(:), Allocatable, Public :: mu   ! - canonical correlations -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: prjc ! - projections onto CCA modes -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: svx  ! - singular values of x -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: svx2 ! - singular values of x -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: svy  ! - singular values of y -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: bz     ! - principal component regression coefficients -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: bz2    ! - principal component regression coefficients -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: ce     ! - EOF cross-correlations -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: eofx   ! - x EOF patterns -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: eofx2  ! - x EOF patterns -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: eofy   ! - y EOF patterns -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: fsx    ! - forecast time scores of X EOFs -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: fsxc   ! - forecast time scores of X CCA modes -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hx_map ! - X homogeneous covariance maps -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hx_ser ! - X homogeneous covariance map time series -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hy_map ! - Y homogeneous covariance maps -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: hy_ser ! - Y homogeneous covariance map time series -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: r      ! - canonical Y EOF weights -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: s      ! - canonical X EOF weights (transposed) -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tsx    ! - time-series of x EOFs (transposed) -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tsx2   ! - time-series of x EOFs (transposed) -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: tsy    ! - time-series of y EOFs (transposed) -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: xwk    ! - workspace -
!
#if GUI == 1
! Character arrays
   Character(Len=ilceofs), Dimension(:), Allocatable, Private :: ceofs ! - list of modes -
!
#endif
Contains
!
!
 Subroutine init_pcs ()
!
! Initialises PC settings
!
! Modules
   Use data_cpt_constants, Only: is_std
!
! Executable Statements
!
! Reset calculation options
   ieofx = is_std
   ieofy = is_std
   icco = 0
!
! Reset numbers of positive modes
   npx = 0
   npy = 0
!
! Reset current modes
   iex = 1
   iey = 1
   iec = 1
#if GUI == 1
   iexp = iex
   ieyp = iey
   iecp = iec
#endif
!
! Reset workspace settings
   lc1 = 0
   lr1 = 0
   nco = 0
   nccu = 0
   nxo = 0
   nyo = 0
!
! Set whether to calculate EOF mode settings
   Call init_eofcalc ()
!
   Return
 End Subroutine init_pcs
!
!
!
 Subroutine init_eofcalc ()
!
! Identifies whether to calculate EOF mode settings
!
! Modules
   Use settings, Only: lexpert
!
! Executable Statements
!
! Set whether to calculate EOF mode settings
   lcde = .not.lexpert
!
   Return
 End Subroutine init_eofcalc
!
!
!
 Function init_ccamaps(nco) &
          Result (init)
!
! Initialises memory and settings to calculate CCA maps
!
! Modules
   Use settings, Only: nu
   Use iofiles,  Only: mxa, mya
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nco ! - number of modes -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - X homogeneous maps -
   Allocate (hx_map(mxa,nco), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - Y homogeneous maps -
   Allocate (hy_map(mya,nco), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - X homogeneous maps time series -
   Allocate (hx_ser(nco,nu), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - Y homogeneous maps time series -
   Allocate (hy_ser(nco,nu), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   init = 0
   Return
!
! Errors
1  init = 1
!
   Return
 End Function init_ccamaps
!
!
!
 Function init_pc(j, k, l, sv, eof, ts) &
          Result (init)
!
! Initialises memory to calculate principal components
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: j ! - number of locations / variables -
   Integer, Intent(In) :: k ! - number of time-steps -
!
! - output scalars -
   Integer, Intent(Out) :: l ! - Min(j, k) -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Allocatable :: sv ! - singular values -
!
   Real(Kind=rp), Dimension(:,:), Allocatable :: eof ! - EOF patterns -
   Real(Kind=rp), Dimension(:,:), Allocatable :: ts  ! - time-series of EOFs (transposed) -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Calculate minimum dimension
   l = Min(j, k)
!
! Initialise memory to calculate principal components
   Allocate (sv(l), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - independent variables EOF patterns -
   Allocate (eof(j,l), Stat=ifail)
   If (ifail /= 0) GoTo 1
! - independent variables EOF scores -
   Allocate (ts(l,k), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
1  init = ifail
!
   Return
 End Function init_pc
!
!
!
 Function init_svds(ianalc, mx, my, n) &
          Result (init)
!
! Initialises memory and settings to perform SVDs
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianalc ! - analysis identifier -
   Integer, Intent(In) :: mx     ! - number of X variables -
   Integer, Intent(In) :: my     ! - number of Y variables -
   Integer, Intent(In) :: n      ! - number of time steps -
!
! Locals
!
! Local scalars
#if GUI == 1
   Integer :: i     ! - counter -
   Integer :: mxy   ! - maximum number of modes -
#endif
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
#if GUI == 1
   Intrinsic AdjustL
#endif
   Intrinsic Max, Min
!
! Executable Statements
!
! Allocate memory
! - independent variables SVD -
   If (ianalc==ia_cca .or. ianalc==ia_pcr) Then
      If (init_pc(mx, n, lxt, svx, eofx, tsx) /= 0) GoTo 1
   End If
!
   Select Case (ianalc)
    Case (ia_cca)
! - dependent variables SVD -
      If (init_pc(my, n, lyt, svy, eofy, tsy) /= 0) GoTo 1
      lxyt = Min(Max(mx, my), n)
#if GUI == 1
      mxy = Max(lxt, lyt)
#endif
! - canonical correlations -
      Allocate (mu(lxyt), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - canonical Y EOF weights -
      lr1 = mye
      Allocate (r(lr1,lxyt), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - canonical X EOF weights -
      Allocate (s(lxyt,mxe), Stat=ifail)
      If (ifail /= 0) GoTo 1
! - projections onto CCA modes -
      Allocate (prjc(mcc), Stat=ifail)
      If (ifail /= 0) GoTo 1
    Case (ia_pcr)
! - principal component regression coefficients -
#if GUI == 1
      mxy = lxt
#endif
      Allocate (bz(0:lxt,my), Stat=ifail)
      If (ifail /= 0) GoTo 1
   End Select
#if GUI == 1
! - list of modes -
   Allocate (ceofs(mxy), Stat=ifail)
   If (ifail /= 0) GoTo 1
   Do i = 1, mxy
      Write (Unit=ceofs(i), Fmt='(I8)') i
      ceofs(i) = AdjustL(ceofs(i))
   End Do
#endif
!
   init = 0
   Return
!
! Errors
1  init = 1
!
   Return
 End Function init_svds
!
!
!
 Function init_svdwk() &
          Result (init)
!
! Initialises workspace and settings to perform SVDs
!
! Function result
   Integer :: init
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Allocate memory
! - EOF cross-correlations -
   lc1 = mye
   Allocate (ce(lc1,mxe), Stat=ifail)
   If (ifail /= 0) GoTo 1
!
   init = 0
   Return
!
! Errors
1  init = 1
!
   Return
 End Function init_svdwk
!
!
!
 Function ieof_to_istd(ieof) &
          Result (istd)
!
! Converts an EOF-type index to a standardization index
!
! Modules
   Use data_cpt_constants, Only: ip_cor, ip_cov, ip_sxp, is_std, is_ano, is_non
!
! Function result
   Integer :: istd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(InOut) :: ieof ! - EOF index -
!
! Executable Statements
!
! Convert an EOF-type index to a standardization index
   Select Case (ieof)
    Case (ip_cor) ! - from correlation matrix to standardized anomalies -
      istd = is_std
    Case (ip_cov) ! - from variance-covariance matrix to anomalies -
      istd = is_ano
    Case (ip_sxp) ! - from sums-of-squares and cross-products matrix to no anomalies -
      istd = is_non
   End Select
!
   Return
 End Function ieof_to_istd
!
!
!
 Function istd_to_ieof(istd) &
          Result (ieof)
!
! Converts a standardization index to an EOF-type index
!
! Modules
   Use data_cpt_constants, Only: ip_cor, ip_cov, ip_sxp, is_std, is_ano, is_non
!
! Function result
   Integer :: ieof
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: istd ! - standardization index -
!
! Executable Statements
!
! Convert a standardization index to an EOF-type index
   Select Case (istd)
    Case (is_std) ! - from standardized anomalies to correlation matrix -
      ieof = ip_cor
    Case (is_ano) ! - from anomalies to variance-covariance matrix -
      ieof = ip_cov
    Case (is_non) ! - from no anomalies to sums-of-squares and cross-products matrix -
      ieof = ip_sxp
   End Select
!
   Return
 End Function istd_to_ieof
!
!
!
 Subroutine get_pcs (lpos, n, m, lvt, ieof, mpe, v, sv, eof, ts, npe, ifail, &
            regions, vm, vsd, ndom, nlts, iuse, rlat)
!
! Calculates principal components
!
! Modules
   Use data_numbers, Only: zero, tol
   Use cpt_lapack,   Only: gesdd
   Use settings,     Only: igauss
   Use arrays,       Only: iwk, lrwk, rwk
   Use space,        Only: domain, &
                           latitude_weight
   Use distribs,     Only: stdize, &
                           moments
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n    ! - number of cases -
   Integer, Intent(In) :: m    ! - number of variables -
   Integer, Intent(In) :: ieof ! - EOF option -
   Integer, Intent(In) :: lvt  ! - first dimension of ts -
   Integer, Intent(In) :: mpe  ! - minimum number of positive eigenvalues -
!
   Logical, Intent(In) :: lpos ! - force largest loading to be positive? -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data; shape(m,n) -
!
! - output scalars -
   Integer, Intent(Out) :: npe   ! - number of positive eigenvalues -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: sv ! - singular values of x -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: eof ! - EOF patterns -
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: ts  ! - time-series of x EOFs (transposed) -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ndom ! - number of domains -
!
! - optional input arrays -
   Integer, Dimension(:), Intent(In), Optional :: iuse ! - used variables -
   Integer, Dimension(:), Intent(In), Optional :: nlts ! - numbers of latitudes in each domain -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: rlat ! - latitudes -
!
   Type(domain), Dimension(:), Intent(In), Optional :: regions ! - domains -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: vm  ! - means; shape(m) -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: vsd ! - standard deviations; shape(m) -
!
! Locals
!
! Local scalars
   Integer :: ie ! - EOF mode index -
!
   Logical :: llatwt ! - weight by latitude? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Count, MaxVal, Min, MinVal, Present
!
! Executable Statements
!
! Calculate means and standard deviations
   If (Present(vm) .and. Present(vsd)) Then
      Call moments (m, n, v, vm(:), &
           vsd=vsd(:))
!
! Calculate anomalies
      Call stdize (ieof, igauss, 1, m, n, vm(1:m), vsd(1:m), v(1:m,1:n))
   End If
!
! Scale by latitude
   llatwt = (Present(ndom) .and. Present(nlts) .and. Present(iuse) .and. Present(rlat) .and. Present(regions))
   If (llatwt) Call latitude_weight (ndom, nlts, regions(:), rlat(:,:), iuse(:), n, v(1:m,1:n))
!
! Calculate EOFs using SVD
   Call gesdd ('S', m, n, v(1:m,:), m, sv(:), eof(1:m,:), m, ts(1:lvt,:), lvt, rwk(:), lrwk, iwk(:), ifail)
   If (ifail /= 0) Return
!
! Determine number of non-zero eigenvalues
   npe = Count(sv(1:Min(mpe, n-1)) > zero)
   If (npe > 2) Then
      If (sv(npe)*sv(npe - 2)/sv(npe - 1)**2 < tol) npe = npe - 1 ! - check for probable rounding errors -
   End If
!
! Rescale loadings
! - rescale loadings by latitude -
   If (llatwt) Call latitude_weight (ndom, nlts, regions(:), rlat(:,:), iuse(:), npe, eof(:,:))
! - ensure that largest absolute loading is positive -
   If (lpos) Then
      Do ie = 1, npe
         If (Abs(MaxVal(eof(1:m,ie))) < Abs(MinVal(eof(1:m,ie)))) Then
            eof(1:m,ie) = -eof(1:m,ie)
            ts(ie,1:n) = -ts(ie,1:n)
         End If
      End Do
   End If
   If (npe < mpe) Then
      sv(npe+1:mpe) = zero
      ts(npe+1:mpe,1:n) = zero
      eof(1:m,npe+1:mpe) = zero
   End If
!
   Return
 End Subroutine get_pcs
!
!
!
 Subroutine set_scree ()
!
! Sets scree plot flag
!
! Modules
   Use menus, Only: set_menu_greying
!
! Executable Statements
!
! Identify whether scree plot can be drawn
   If (npx > 1 .or. npy > 1) Then
      lscree = .true.
   Else
      lscree = .false.
   End If
   Call set_menu_greying (lscree=lscree)
!
   Return
 End Subroutine set_scree
!
!
!
 Function get_eofs_settings(cxy, ianal, lbf, feof) &
          Result (eofs)
!
! Sets EOF settings
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr
   Use gui,                Only: set_cursor_waiting
!
! Function result
   Integer :: eofs
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal ! - analysis indicator -
!
   Character(Len=*), Intent(In) :: cxy ! - X/Y indicator -
!
   Logical, Intent(In) :: lbf ! - both files set? -
!
! - procedure arguments -
   Integer, External :: feof ! - EOF function -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator
!
! Executable Statements
!
! Prompt for numbers of modes if relevant
   Select Case (ianal)
    Case (ia_cca) ! - CCA -
      Call set_cursor_waiting (0)
      ifail = feof()
      If (ifail /= 1 .and. lbf) ifail = get_cca_settings()
    Case (ia_pcr) ! - PCR -
      Select Case (cxy)
       Case ('X', 'x')
         Call set_cursor_waiting (0)
         ifail = feof()
       Case Default
         Continue
      End Select
    Case Default
      Continue
   End Select
   eofs = 2
!
   Return
 End Function get_eofs_settings
!
!
!
 Function get_eofx_settings() &
          Result (modes)
!
! Calls function to set X EOF options
!
! Modules
   Use labels,  Only: l_modeoptsx
   Use iofiles, Only: nx
!
! Function result
   Integer :: modes
!
! Executable Statements
!
! Prompt for EOF options
   modes = get_eof_settings(l_modeoptsx%c, 'X', nx, nxe, mxe, ieofx)
!
   Return
 End Function get_eofx_settings
!
!
!
 Function get_eofy_settings() &
          Result (modes)
!
! Calls function to set Y EOF options
!
! Modules
   Use labels,  Only: l_modeoptsy
   Use iofiles, Only: ny
!
! Function result
   Integer :: modes
!
! Executable Statements
!
! Prompt for EOF options
   modes = get_eof_settings(l_modeoptsy%c, 'Y', ny, nye, mye, ieofy)
!
   Return
 End Function get_eofy_settings
!
!
!
 Function get_eof_settings(cmodes, cxyc, nv, ne, me, ieof) &
          Result (modes)
!
! Calls function to set EOF options
!
! Function result
   Integer :: modes
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv ! - number of variables -
!
   Character(Len=*), Intent(In) :: cmodes ! - options title -
   Character(Len=*), Intent(In) :: cxyc   ! - X/Y variable flag -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ne ! - minimum number of modes -
   Integer, Intent(InOut) :: me ! - maximum number of modes -
!
   Integer, Intent(InOut), Target :: ieof ! - EOF options -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for EOF options
   modes = mode_settings(Trim(cmodes), cxyc, nv, ne, me, ieof, advanced_eof)
!
   Return
 End Function get_eof_settings
!
!
!
 Function get_cca_settings() &
          Result (modes)
!
! Sets CCA options
!
! Modules
   Use labels, Only: l_ccaopts
!
! Function result
   Integer :: modes
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Calculate maximum number of modes
   modes = 2
   modes = mode_settings(Trim(l_ccaopts%c), 'CCA', 0, ncc, mcc, iec, advanced_cca)
!
   Return
 End Function get_cca_settings
!
!
!
 Function mode_settings(ctitle, cxyc, nv, ne, me, ieof, fadvanced) &
          Result (modes)
!
! Sets EOF options
!
! Modules
   Use settings, Only: nt, lcw
   Use iofiles,  Only: cxy
!
! Function result
   Integer :: modes
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv ! - number of variables -
!
   Character(Len=*), Intent(In) :: ctitle ! - title -
   Character(Len=*), Intent(In) :: cxyc   ! - X/Y variable flag -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ne ! - minimum number of modes -
   Integer, Intent(InOut) :: me ! - maximum number of modes -
!
   Integer, Intent(InOut), Target :: ieof ! - EOF options -
!
! - procedure arguments -
   Integer, External :: fadvanced ! - advanced callback function -
!
! Locals
!
! Local scalars
   Integer :: mm ! - maximum number of modes -
!
! Executable Statements
!
! Calculate maximum number of modes
   modes = 2
   Select Case (cxyc)
    Case ('X', 'x', 'Y', 'y')
      mm = max_eof_modes(lcw, nt, nv)
    Case ('CCA')
      mm = max_cca_modes()
   End Select
   If (mm == 1) Then
      ne = 1
      me = 1
      Return
   End If
!
! Prompt for modes options
   If (.not.lcde) Then
      cxy = cxyc
      ieofp => ieof
      modes = get_mode_settings(ctitle, cxyc, ne, me, mm, fadvanced)
   Else
!
! Set CCA options
      me = mm
      Select Case (cxyc)
       Case ('X', 'x', 'Y', 'y')
         Call calc_default_nmodes (ne, me)
      End Select
   End If
!
   Return
 End Function mode_settings
!
!
!
 Function get_mode_settings(ctitle, cxyc, ne, me, mm, fadvanced) &
          Result (modes)
!
! Sets EOF options
!
! Modules
   Use gui,          Only: iw
#if GUI == 1
   Use errors,       Only: cpt_error
#endif
   Use settings,     Only: set_options
   Use opts_prompts, Only: get_mode_opts
!
! Function result
   Integer :: modes
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctitle ! - title -
   Character(Len=*), Intent(In) :: cxyc   ! - X/Y/CCA mode -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ne ! - minimum number of modes -
   Integer, Intent(InOut) :: me ! - maximum number of modes -
   Integer, Intent(InOut) :: mm ! - maximum number of modes -
!
! - procedure arguments -
   Integer, External :: fadvanced ! - advanced callback function -
!
#if GUI == 1
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
#endif
! Executable Statements
!
! Prompt for mode options
   modes = set_options(i1=me, i2=ne)
#if GUI == 1
   Do
      ifail = 0
      Call get_mode_opts (ctitle, cxyc, ne, me, mm, iw, ih_eof, fadvanced)
      If (me < ne) Then
         ifail = 1
         Call cpt_error ('get_mode_settings', .false., ifail, &
              c_arg1=cxyc)
         iw = 0
      End If
#else
   Call get_mode_opts (ctitle, cxyc, ne, me, mm, iw, ih_eof)
#endif
!
! Set options
      modes = set_options(iw=iw, i1=me, i2=ne)
      If (iw == 1) lcde = .false.
#if GUI == 1
      If (ifail == 0) Exit
   End Do
#endif
   modes = 2
!
   Return
 End Function get_mode_settings
!
!
!
 Function advanced_eof()
!
! Sets EOF options
!
! Modules
   Use data_text,          Only: ilang
   Use data_cpt_constants, Only: npo
   Use labels,             Only: ca_pccos, &
                                 cg_modecopts_t
   Use gui,                Only: iw, &
                                 box_close, box_open, ganged_rbs, gui_creturn, init_win, win_prompt
   Use settings,           Only: set_options
   Use iofiles,            Only: cxy
!
! Function result
   Integer :: advanced_eof
!
! Locals
!
! Local scalars
   Character(Len=64) :: ctitle ! - title -
!
! Local arrays
   Integer, Dimension(npo) :: ipo ! - principal components calculation option flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Identify current option
   ieofp = istd_to_ieof(ieofp) ! - convert standardization option to EOF option -
   advanced_eof = set_options(o1=ieofp, oa1=ipo(:))
!
! Prompt for EOF calculation options
   Select Case (ilang)
    Case (1, 3, 4)
      ctitle = cxy//' '//Trim(cg_modecopts_t)
    Case Default
      ctitle = Trim(cg_modecopts_t)//' '//cxy
   End Select
   Call init_win (title=Trim(ctitle), &
        idisable=ih_eof) ! - disable host window -
   Call gui_creturn (.false.) 
   Call box_open (.true., .false., &
        title=Trim(ctitle))
   Call ganged_rbs (npo, ipo, &
        crbns=ca_pccos(:))
   Call box_close (.true.)
   iw = win_prompt(.true.)
!
! Set option
   advanced_eof = set_options(iw=iw, o1=ieofp, oa1=ipo(:))
   ieofp = istd_to_ieof(ieofp) ! - convert standardization option back to EOF option -
   advanced_eof = 2
!
   Return
 End Function advanced_eof
!
!
!
 Function advanced_cca()
!
! Sets CCA options for too many combined numbers of modes
!
! Modules
   Use labels,   Only: cg_ccacopts_t
   Use gui,      Only: iw, &
                       box_close, box_open, ganged_rbs, init_win, win_prompt
   Use settings, Only: set_options
!
! Function result
   Integer :: advanced_cca
!
! Locals
!
! Local arrays
   Integer, Dimension(2) :: iccs ! - canonical correlation calculation option flags -
!
! Executable Statements
!
! Backup current option
   icco = 2 + icco
   advanced_cca = set_options(o1=icco, oa1=iccs(:))
!
! Prompt for CCA calculation options
   Call init_win (title=Trim(cg_ccacopts_t), &
        idisable=ih_eof) ! - disable host window -
   Call box_open (.false., .false., &
        title=Trim(cg_ccacopts_t))
   Call ganged_rbs (2, iccs(:), &
        crbns=['Ignore modes with perfect correlation', 'Use modes with perfect correlation   '])
   Call box_close (.false.)
   iw = win_prompt(.true.)
!
! Set option
   advanced_cca = set_options(iw=iw, o1=icco, oa1=iccs(:))
   icco = 2 - icco
   advanced_cca = 2
!
   Return
 End Function advanced_cca
!
!
!
 Function max_eof_modes(lcw, nt, nv) &
          Result (mn)
!
! Sets maximum number of EOF modes
!
! Modules
   Use data_numbers, Only: ihuge
!
! Function result
   Integer :: mn
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv  ! - number of variables -
   Integer, Intent(In) :: nt  ! - number of time steps -
   Integer, Intent(In) :: lcw ! - length of cross-validation window -
!
! Locals
!
! Local scalars
   Integer :: n ! - nt-lcw-1 -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Calculate maximum number of modes
   n = nt - lcw - 1
   If (nv > 0) Then
      If (n > 1) Then
         mn = Min(n, nv)
      Else
         mn = nv
      End If
   Else
      If (n > 1) Then
         mn = n
      Else
         mn = ihuge
      End If
   End If
!
   Return
 End Function max_eof_modes
!
!
!
 Function max_cca_modes() &
          Result (mn)
!
! Sets maximum number of CCA modes
!
! Function result
   Integer :: mn
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Calculate maximum number of modes
   mn = Min(mxe, mye)
!
   Return
 End Function max_cca_modes
!
!
!
 Subroutine calc_default_nmodes (ne, me)
!
! Sets default maximum number of EOF modes
!
! Modules
   Use data_cpt_constants, Only: mre
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Integer, Intent(InOut) :: ne ! - minimum number of modes -
   Integer, Intent(InOut) :: me ! - maximum number of modes -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Set reasonable number of modes
   ne = 1
   me = Min(me, mre)
!
   Return
 End Subroutine calc_default_nmodes
!
!
!
 Function check_pcs(ianalc) &
          Result (check)
!
! Checks PC settings
!
! On exit:
!    check =  0 All checks passed
!    check =  1 nxe > nt-lcw-1, nxe reset to nt-lcw-1
!    check =  2 nxe > mxa, nxe reset to mxa
!    check =  3 mxe > nxe, mxe reset to nxe
!    check =  4 mxe > nt-lcw-1, mxe reset to nt-lcw-1
!    check =  5 mxe > mxa, mxe reset to mxa
!    check =  6 nye > nt-lcw-1, nye reset to nt-lcw-1
!    check =  7 nye > mya, nye reset to mya
!    check =  8 mye > nye, mye reset to nye
!    check =  9 mye > nt-lcw-1, mye reset to nt-lcw-1
!    check = 10 mye > mya, mye reset to mya
!    check = 11 ncc > nxe, ncc reset to nxe
!    check = 12 ncc > nye, ncc reset to nye
!    check = 13 mcc < ncc, mcc reset to ncc
!    check = 14 mcc > mxe, mcc reset to mxe
!    check = 15 mcc > mye, mcc reset to mye
!    check = 16 mxe+mye > nt
!    check = -1 abort
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr
   Use errors,             Only: cpt_error
   Use settings,           Only: nt
   Use iofiles,            Only: mxa, mya
!
! Function result
   Integer :: check
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianalc ! - analysis indicator -
!
! Locals
!
! Local scalars
   Integer :: ierr  ! - error indicator -
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Check that number of modes is not greater than number of variables or number of cases
   check = -1
   ifail = 0
   If (ianalc == ia_cca .or. ianalc == ia_pcr) Then
      Call check_ne (mxa, nt, nxe, mxe, ifail)
      If (ifail /= 0) Then
         ierr = ifail
         Call cpt_error ('check_pcs', .false., ierr)
         If (ierr > 0) Return
      End If
      If (ianalc == ia_cca) Then
         Call check_ne (mya, nt, nye, mye, ifail)
         If (ifail /= 0) Then
            ifail = ifail + 5
            ierr = ifail
            Call cpt_error ('check_pcs', .false., ierr)
            If (ierr > 0) Return
         End If
!
! Check that number of CCA modes is not greater than number of EOF modes
         Call check_nc (nxe, mxe, nye, mye, ncc, mcc, ifail)
         If (ifail /= 0) Then
            ifail = ifail + 10
            ierr = ifail
            Call cpt_error ('check_pcs', .false., ierr)
            If (ierr > 0) Return
         End If
!
! Check that combined number of modes is not greater than number of cases
         Call check_nes (nt, mxe, mye, ifail)
         If (ifail /= 0) Then
            ifail = 16
            ierr = ifail
            Call cpt_error ('check_pcs', .false., ierr)
            If (ierr > 0) Return
         End If
      End If
   End If
!
   check = ifail
   Return
 End Function check_pcs
!
!
!
 Subroutine check_ne (nv, nt, ne, me, ifail)
!
! Checks that number of EOF modes is not greater than number of cases-1 or variables
! and that maximum number of modes is not less than minimum number.
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 ne > nt-lcw-1, ne reset to nt-lcw-1
!    ifail = 2 ne > nv, ne reset to nv
!    ifail = 3 me > ne, me reset to ne
!    ifail = 4 me > nt-lcw-1, me reset to nt-lcw-1
!    ifail = 5 me > nv, me reset to nv
!
! Modules
   Use settings, Only: lcw
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nv ! - number of variables -
   Integer, Intent(In) :: nt ! - number of cases -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ne ! - minimum number of modes -
   Integer, Intent(InOut) :: me ! - maximum number of modes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: lvt ! - minimum of nv and nt -
   Integer :: n   ! - number of cross-validation cases - 1  -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Set maximum number of modes
   n = nt - lcw - 1
   lvt = Min(nv, n)
!
! Force minimum number of modes to be no more than number of variables or number of cases
   If (ne > lvt) Then
      ne = lvt
      If (lvt == n) Then
         ifail = 1
      Else
         ifail = 2
      End If
      Return
   End If
!
! Force maximum number of modes to be at least the minimum number
   If (me < ne) Then
      me = ne
      ifail = 3
      Return
   End If
!
! Force maximum number of modes to be no more than number of variables or number of cases
   If (me > lvt) Then
      me = lvt
      If (lvt == n) Then
         ifail = 4
      Else
         ifail = 5
      End If
      Return
   End If
   ifail = 0
!
   Return
 End Subroutine check_ne
!
!
!
 Subroutine check_nes (nt, mxe, mye, ifail)
!
! Checks that combined number of EOF modes is not greater than number of cases.
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 mxe+mye > nt
!
! Modules
   Use settings, Only: lcw
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt  ! - number of cases -
   Integer, Intent(In) :: mxe ! - minimum number of X modes -
   Integer, Intent(In) :: mye ! - maximum number of Y modes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: n ! - number of cross-validation cases  -
   Integer :: m ! - combined number of EOF modes  -
!
! Executable Statements
!
! Identify maximum number of combined modes
   n = nt - lcw
   m = mxe + mye
!
! Warn if number of modes is too large
   If (m > n .and. icco == 1) Then
      ifail = 1
   Else
      ifail = 0
   End If
!
   Return
 End Subroutine check_nes
!
!
!
 Subroutine check_nc (nxe, mxe, nye, mye, ncc, mcc, ifail)
!
! Checks that number of CCA modes is not greater than number of EOF modes
! and that maximum number of modes is not less than minimum number
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 nxe > nxe, ncc reset to nxe
!    ifail = 2 ncc > nye, ncc reset to nye
!    ifail = 3 mcc < ncc, mcc reset to ncc
!    ifail = 4 mcc > mxe, mcc reset to mxe
!    ifail = 5 mcc > mye, mcc reset to mye
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nxe ! - minimum number of X EOF modes -
   Integer, Intent(In) :: nye ! - minimum number of Y EOF modes -
   Integer, Intent(In) :: mxe ! - maximum number of X EOF modes -
   Integer, Intent(In) :: mye ! - maximum number of Y EOF modes -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ncc ! - number of CCA modes -
   Integer, Intent(InOut) :: mcc ! - maximum number of CCA modes -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: lxy ! - minimum of nxe and nye -
!
! Executable Statements
!
! Force minimum number of CCA modes to be no more than minimum numbers of EOF modes
   If (nxe <= nye) Then
      lxy = nxe
      ifail = 1
   Else
      lxy = nye
      ifail = 2
   End If
   If (ncc > lxy) Then
      ncc = lxy
      Return
   End If
!
! Force maximum number of CCA modes to be at least the minimum number
   If (mcc < ncc) Then
      mcc = ncc
      ifail = 3
      Return
   End If
!
! Force maximum number of CCA modes to be no more than smallest of the maximum numbers of EOF modes
   If (mxe <= mye) Then
      lxy = mxe
      ifail = 4
   Else
      lxy = mye
      ifail = 5
   End If
   If (mcc > lxy) Then
      mcc = lxy
      Return
   End If
   ifail = 0
!
   Return
 End Subroutine check_nc
!
!
!
#if GUI == 1
 Function change_eofx() &
          Result (chnge)
!
! Change X mode
!
! Modules
   Use settings, Only: change_setting
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change number of X modes
   chnge = change_setting(iex, iexp, 1, npx)
!
   Return
 End Function change_eofx
!
!
!
 Function change_eofy() &
          Result (chnge)
!
! Change Y mode
!
! Modules
   Use settings, Only: change_setting
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change number of Y modes
   chnge = change_setting(iey, ieyp, 1, npy)
!
   Return
 End Function change_eofy
!
!
!
 Function change_cca() &
          Result (chnge)
!
! Change CCA mode
!
! Modules
   Use screen,   Only: window_update
   Use settings, Only: change_setting
!
! Function result
   Integer :: chnge
!
! Executable Statements
!
! Change number of CCA modes
   chnge = change_setting(iec, iecp, 1, nccu)
!
! Update canonical correlation
   cancor = mu(iec)
   Call window_update (cancor)
!
   Return
 End Function change_cca
!
!
!
 Function prompt_eofs(ieof, ne, change_eof, update_eof) &
          Result (prompt)
!
! Prompts for mode number
!
! Modules
   Use labels, Only: cg_mode
   Use gui,    Only: prompt_list
!
! Function result
   Integer :: prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(InOut) :: ieof ! - current mode -
   Integer, Intent(InOut) :: ne   ! - total number of modes -
!
! - procedure arguments -
   Integer, External :: change_eof
   Integer, External :: update_eof
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for mode
!   Call prompt_integer (Trim(cg_mode), ieof, &
!        itab=17, fcb1=change_eof, fcb2=update_eof)
   Call prompt_list (Trim(cg_mode), .true., ceofs, ne, ieof, &
        itab=17, iwdth=ilceofs, cbf1=change_eof, cbf2=update_eof)
   prompt = 0
!
   Return
 End Function prompt_eofs
!
!
!
#endif
 Subroutine make_eof_title (nfs, nes, ie, cvar, ceof, cssn, cmode, img_title)
!
! Constructs title for EOF map
!
! Modules
   Use data_time_constants, Only: isq_sn, iuseq
   Use labels,              Only: ca_seq, cg_field
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfs ! - number of fields -
   Integer, Intent(In) :: nes ! - number of EOF dimensions -
   Integer, Intent(In) :: ie  ! - current EOF -
!
   Character(Len=1), Intent(In) :: ceof  ! - EOF flag -
   Character(Len=*), Intent(In) :: cmode ! - mode flag -
   Character(Len=*), Intent(In) :: cvar  ! - variable -
   Character(Len=*), Intent(In) :: cssn  ! - season -
!
! - output scalars -
   Character(Len=*), Intent(Out) :: img_title ! - image title -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Construct default title
   If (nfs > 1 .and. nes > 1) Then
      Write (img_title, Fmt='(A,I0,A)') &
         ceof//' Spatial Loadings ('//cmode, ie, '); '//Trim(cg_field)//': '//cvar//'; '//Trim(ca_seq(iuseq(isq_sn)))//': '//cssn
   Else If (nfs > 1) Then
      Write (img_title, Fmt='(A,I0,A)') &
         ceof//' Spatial Loadings ('//cmode, ie, '); '//Trim(cg_field)//': '//cvar
   Else If (nes > 1) Then
      Write (img_title, Fmt='(A,I0,A)') &
         ceof//' Spatial Loadings ('//cmode, ie, '); '//Trim(ca_seq(iuseq(isq_sn)))//': '//cssn
   Else
      Write (img_title, Fmt='(A,I0,A)') &
         ceof//' Spatial Loadings ('//cmode, ie, ')'
   End If
!
   Return
 End Subroutine make_eof_title
!
!
!
 Subroutine set_grey_modes (ianal, ixdstr, lscrees)
!
! Sets which menu items to display or grey
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, id_non
   Use menus,              Only: set_menu_greying
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ianal  ! - analysis flag -
   Integer, Intent(In) :: ixdstr ! - X file structure -
!
   Logical, Intent(In) :: lscrees ! - determine scree plot settings? -
!
! Locals
!
! Local scalars
   Logical :: leofx  ! - X EOF flag -
   Logical :: leofy  ! - Y EOF flag -
   Logical :: lmodes ! - modes? -
!
! Executable Statements
!
! Set grey Modes menu items
   Select Case (ianal)
    Case (ia_cca) ! - CCA -
      lmodes = .true.
      If (lscrees) Call set_scree ()
      leofx = .true.
      leofy = .true.
    Case (ia_pcr) ! - PCR -
      lmodes = .true.
      If (lscrees) Call set_scree ()
      leofx = .true.
      leofy = .false.
    Case (ia_mlr, ia_gcm, ia_pfv) ! - MLR, GCM, PFV -
      lmodes = .false.
      lscree = .false.
      leofx = .false.
      leofy = .false.
    Case Default
      lmodes = .false.
      leofx = .false.
      leofy = .false.
   End Select
!
! Set greying
   If (ianal /= ia_pfv) Then
      Select Case (ixdstr)
       Case (id_non)
         lscree = .false.
      End Select
   Else
      lscree = .false.
   End If
   Call set_menu_greying (lmodes=lmodes, leofx=leofx, leofy=leofy, lscree=lscree)
!
   Return
 End Subroutine set_grey_modes
!
!
!
 Subroutine proj_read_pcs (iin, ianal, icalc, lres, ifail)
!
! Reads PC settings from project file
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input file unit number -
   Integer, Intent(In) :: ianal ! - analysis type -
   Integer, Intent(In) :: icalc ! - results available flag -
!
   Logical, Intent(In) :: lres ! - read results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read PC settings
   If (.not.lres) Then
      Read (Unit=iin, Err=1, End=2) icco, ieofx, ieofy, mcc, mxe, mye, ncc, nxe, nye
      If (icalc == 1) Read (Unit=iin, Err=1, End=2) lc1, lr1, nco, nccu, npx, npy, nxo, nyo
!
! Read PC results
   Else
      Select Case (ianal)
       Case (ia_cca) ! - CCA -
         Read (Unit=iin, Err=1, End=2) eofx
         Read (Unit=iin, Err=1, End=2) eofy
         Read (Unit=iin, Err=1, End=2) hx_map
         Read (Unit=iin, Err=1, End=2) hx_ser
         Read (Unit=iin, Err=1, End=2) hy_map
         Read (Unit=iin, Err=1, End=2) hy_ser
         Read (Unit=iin, Err=1, End=2) mu
         Read (Unit=iin, Err=1, End=2) r
         Read (Unit=iin, Err=1, End=2) s
         Read (Unit=iin, Err=1, End=2) svx
         Read (Unit=iin, Err=1, End=2) svy
         Read (Unit=iin, Err=1, End=2) tsx
         Read (Unit=iin, Err=1, End=2) tsy
       Case (ia_pcr) ! - PCR -
         Read (Unit=iin, Err=1, End=2) eofx
         Read (Unit=iin, Err=1, End=2) svx
         Read (Unit=iin, Err=1, End=2) tsx
      End Select
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
 End Subroutine proj_read_pcs
!
!
!
 Subroutine proj_write_pcs (iout, ianal, icalc, lres, ifail)
!
! Writes PC settings to project file
!
! Modules
   Use data_cpt_constants, Only: ia_cca, ia_pcr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output file unit number -
   Integer, Intent(In) :: ianal ! - analysis type -
   Integer, Intent(In) :: icalc ! - results available flag -
!
   Logical, Intent(In) :: lres ! - read results? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write PC settings
   If (.not.lres) Then
      Write (Unit=iout, Err=1) icco, ieofx, ieofy, mcc, mxe, mye, ncc, nxe, nye
      If (icalc == 1) Write (Unit=iout, Err=1) lc1, lr1, nco, nccu, npx, npy, nxo, nyo
!
! Read PC results
   Else
     Select Case (ianal)
       Case (ia_cca) ! - CCA -
         Write (Unit=iout, Err=1) eofx
         Write (Unit=iout, Err=1) eofy
         Write (Unit=iout, Err=1) hx_map
         Write (Unit=iout, Err=1) hx_ser
         Write (Unit=iout, Err=1) hy_map
         Write (Unit=iout, Err=1) hy_ser
         Write (Unit=iout, Err=1) mu
         Write (Unit=iout, Err=1) r
         Write (Unit=iout, Err=1) s
         Write (Unit=iout, Err=1) svx
         Write (Unit=iout, Err=1) svy
         Write (Unit=iout, Err=1) tsx
         Write (Unit=iout, Err=1) tsy
       Case (ia_pcr) ! - PCR -
         Write (Unit=iout, Err=1) eofx
         Write (Unit=iout, Err=1) svx
         Write (Unit=iout, Err=1) tsx
      End Select
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
 End Subroutine proj_write_pcs
!
!
!
 Subroutine parameter_write_pcs (iout, ifail)
!
! Writes program settings to parameter file
!
! Modules
   Use menus,          Only: mn_opts_ccaadv, mn_opts_xmodeadv, mn_opts_ymodeadv
   Use parameter_file, Only: output_parameter
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
! - CCA advanced settings (no need for numbers of modes) -
   If (mn_opts_ccaadv%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_ccaadv%l_lab%c), i=mn_opts_ccaadv%id) /= 0) GoTo 1
      If (output_parameter(iout, & ! - calculation options -
                           i=2-icco) /= 0) GoTo 1
   End If
! - EOF advanced settings (no need for numbers of modes) -
   If (mn_opts_xmodeadv%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_xmodeadv%l_lab%c), i=mn_opts_xmodeadv%id) /= 0) GoTo 1
      If (output_parameter(iout, & ! - calculation option -
                           i=istd_to_ieof(ieofx)) /= 0) GoTo 1
   End If
   If (mn_opts_ymodeadv%iact > 0) Then 
      If (output_parameter(iout, &
                           cdesc=Trim(mn_opts_ymodeadv%l_lab%c), i=mn_opts_ymodeadv%id) /= 0) GoTo 1
      If (output_parameter(iout, & ! - calculation option -
                           i=istd_to_ieof(ieofy)) /= 0) GoTo 1
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
 End Subroutine parameter_write_pcs
!
!
!
 Function close_ccamaps() &
          Result (fclose)
!
! Frees memory allocated for CCA maps
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
   If (Allocated(hy_ser)) Deallocate (hy_ser)
   If (Allocated(hx_ser)) Deallocate (hx_ser)
   If (Allocated(hy_map)) Deallocate (hy_map)
   If (Allocated(hx_map)) Deallocate (hx_map)
!
   fclose = 2
!
   Return
 End Function close_ccamaps
!
!
!
 Function close_svds() &
          Result (fclose)
!
! Frees memory allocated for SVDs
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
   fclose = close_ccamaps ()
   If (Allocated(ce))     Deallocate (ce)
#if GUI == 1
   If (Allocated(ceofs))  Deallocate (ceofs)
#endif
   If (Allocated(prjc))   Deallocate (prjc)
   If (Allocated(s))      Deallocate (s)
   If (Allocated(r))      Deallocate (r)
   If (Allocated(mu))     Deallocate (mu)
   If (Allocated(tsy))    Deallocate (tsy)
   If (Allocated(svy))    Deallocate (svy)
   If (Allocated(eofy))   Deallocate (eofy)
   If (Allocated(bz))     Deallocate (bz)
   If (Allocated(tsx))    Deallocate (tsx)
   If (Allocated(eofx))   Deallocate (eofx)
   If (Allocated(svx))    Deallocate (svx)
!
! Reset workspace settings
   lxt = 0
   lxyt = 0
   lyt = 0
!
   fclose = 2
!
   Return
 End Function close_svds
End Module pcs
