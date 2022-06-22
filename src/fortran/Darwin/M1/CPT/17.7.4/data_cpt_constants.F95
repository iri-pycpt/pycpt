! Author: Simon Mason
Module data_cpt_constants
!
! Contains declations and definitions of:
!    analysis types and options
!    alphabets
!    various language constants and identifiers
!    numbers of scores and identifiers
!    various other constants
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Parameters
!
! Integer parameters
! - analysis options (corresponding to ls_analyses in labels module) -
   Integer, Parameter, Public :: na = 5                 ! - number of analytical methods -
   Integer, Parameter, Public :: na10 = 3               ! - number of analytical methods up to CPT version 10 -
   Integer, Parameter, Public :: ia_nul = 610           ! - analysis menu category -
   Integer, Parameter, Public :: ia_non = 0             ! - no analysis -
   Integer, Parameter, Public :: ia_cca = 1             ! - CCA identifier -
   Integer, Parameter, Public :: ia_pcr = 2             ! - PCR identifier -
   Integer, Parameter, Public :: ia_mlr = 3             ! - MLR identifier -
   Integer, Parameter, Public :: ia_gcm = 4             ! - GCM identifier -
   Integer, Parameter, Public :: ia_pfv = 5             ! - PFV identifier -
   Integer, Parameter, Public :: ia_for = 11            ! - forecast identifier -
   Integer, Parameter, Public :: ia_dia = 12            ! - diagnostics identifier -
! - GCM combination options -
   Integer, Parameter, Public :: nmcs = 7               ! - number of model combination options -
!   Integer, Parameter, Public :: nmcs = 8               ! - number of model combination options -
   Integer, Parameter, Public :: ic_non = 0             ! - uncalibrated average -
   Integer, Parameter, Public :: ic_ave = 1             ! - mean-corrected average -
   Integer, Parameter, Public :: ic_var = 2             ! - mean- and variance-corrected average -
   Integer, Parameter, Public :: ic_skl = 3             ! - skill-corrected average -
   Integer, Parameter, Public :: ic_mul = 4             ! - skill-weighted average -
   Integer, Parameter, Public :: ic_loc = 5             ! - best model by location -
   Integer, Parameter, Public :: ic_bst = 6             ! - best model overall -
   Integer, Parameter, Public :: ic_pcr = 7             ! - principal component regression -
! - GCM combination options -
   Integer, Parameter, Public :: nstruct = 3            ! - number of data structures -
   Integer, Parameter, Public :: id_non = 0             ! - unspecified -
   Integer, Parameter, Public :: id_grd = 1             ! - gridded data -
   Integer, Parameter, Public :: id_stn = 2             ! - station data -
   Integer, Parameter, Public :: id_ind = 3             ! - index data -
! - EOF options (corresponding to ca_pccos in labels module) -
   Integer, Parameter, Public :: nev = 4                ! - number of error-variance options -
   Integer, Parameter, Public :: ie_cvh = 1             ! - cross-validated hindcasts -
   Integer, Parameter, Public :: ie_fit = 2             ! - fitted values -
   Integer, Parameter, Public :: ie_ret = 3             ! - retroactive hindcasts -
   Integer, Parameter, Public :: ie_cv2 = 4             ! - double cross-validated hindcasts -
! - GCM recalibration options -
   Integer, Parameter, Public :: ngc = 4                ! - number of GCM correction options -
   Integer, Parameter, Public :: ig_non = 0             ! - no correction -
   Integer, Parameter, Public :: ig_ave = 1             ! - correct mean biases -
   Integer, Parameter, Public :: ig_var = 2             ! - correct mean and variance biases -
   Integer, Parameter, Public :: ig_skl = 3             ! - correct for skill -
! - link functions (corresponding to ca_links in labels module) -
   Integer, Parameter, Public :: nlink = 6              ! - number of promptable regression options (excluding il_lgm) -
   Integer, Parameter, Public :: il_idt = 1             ! - identity -
   Integer, Parameter, Public :: il_lgt = 2             ! - logit -
   Integer, Parameter, Public :: il_inv = 3             ! - inverse -
   Integer, Parameter, Public :: il_log = 4             ! - logarithmic -
   Integer, Parameter, Public :: il_sqr = 5             ! - square root -
   Integer, Parameter, Public :: il_lgm = 6             ! - multinomial logit -
! - EOF options (corresponding to ca_pccos in labels module) -
   Integer, Parameter, Public :: npo = 3                ! - number of principal component calculation options -
   Integer, Parameter, Public :: ip_cor = 1             ! - correlation matrix -
   Integer, Parameter, Public :: ip_cov = 2             ! - variance-covariance matrix -
   Integer, Parameter, Public :: ip_sxp = 3             ! - sums-of-squares and cross-products matrix -
! - regression options (corresponding to ca_regrs in labels module) -
   Integer, Parameter, Public :: nregr = 5              ! - number of regression options -
   Integer, Parameter, Public :: ir_ols = 1             ! - ordinary least squares regression -
   Integer, Parameter, Public :: ir_log = 2             ! - logistic regression (bernoulli GLM) -
   Integer, Parameter, Public :: ir_bin = 3             ! - binomial GLM -
   Integer, Parameter, Public :: ir_poi = 4             ! - poisson GLM -
   Integer, Parameter, Public :: ir_gam = 5             ! - gamma GLM -
! - standardization options (corresponding to ca_stds_t in labels module) -
   Integer, Parameter, Public :: nstd = 5               ! - number of standardization options -
   Integer, Parameter, Public :: is_non = 0             ! - no standardization -
   Integer, Parameter, Public :: is_ano = 1             ! - anomalies -
   Integer, Parameter, Public :: is_std = 2             ! - standardized anomalies -
   Integer, Parameter, Public :: is_sti = 3             ! - standardized indices -
   Integer, Parameter, Public :: is_pav = 4             ! - percent of average -
! - transformation options (corresponding to ca_tos in labels module) -
   Integer, Parameter, Public :: nto = 2                ! - number of transformation options -
   Integer, Parameter, Public :: it_non = 0             ! - none -
   Integer, Parameter, Public :: it_emp = 1             ! - from empirical distribution -
   Integer, Parameter, Public :: it_gam = 2             ! - from gamma distribution -
! - threshold-value options (corresponding to ca_thrs in labels module) -
   Integer, Parameter, Public :: nthr = 3               ! - number of threshold options -
   Integer, Parameter, Public :: iv_ple = 1             ! - percentile -
   Integer, Parameter, Public :: iv_abs = 2             ! - absolute values -
   Integer, Parameter, Public :: iv_ref = 3             ! - references -
! - input file identifiers -
   Integer, Parameter, Public :: ifile_x = 1            ! - X-file identifier -
   Integer, Parameter, Public :: ifile_y = 2            ! - X-file identifier -
   Integer, Parameter, Public :: ifile_z = 3            ! - X-file identifier -
!
   Integer, Parameter, Public :: ipm = 100              ! - maximum percentage of missing values -
   Integer, Parameter, Public :: mnt = 4                ! - minimum length of training period -
   Integer, Parameter, Public :: mre = 8                ! - maximum reasonable number of EOF modes -
   Integer, Parameter, Public :: nbs = 2                ! - number of probability bin options -
   Integer, Parameter, Public :: ncorr = 3              ! - number of correlations -
   Integer, Parameter, Public :: ncps = 2               ! - number of components in a persisted season -
   Integer, Parameter, Public :: ndcon = 8              ! - number of continuous deterministic scores -
   Integer, Parameter, Public :: ndcat = 8              ! - number of categorised deterministic scores -
   Integer, Parameter, Public :: nel = 2                ! - number of expertise levels -
   Integer, Parameter, Public :: nensc = 3              ! - number of ensemble forecast options -
   Integer, Parameter, Public :: nep = 1000             ! - number of points on exceedance probability curve -
   Integer, Parameter, Public :: n_file = 3             ! - number of input files -
   Integer, Parameter, Public :: ng = 3                 ! - number of categories -
   Integer, Parameter, Public :: nic = 4                ! - number of information criteria -
   Integer, Parameter, Public :: ninterp = 2            ! - number of interpolation options -
   Integer, Parameter, Public :: nmo = 4                ! - number of missing value replacement options -
   Integer, Parameter, Public :: npall = 10             ! - number of all-category probabilistic scores -
   Integer, Parameter, Public :: npcat = 9              ! - number of category-specific probabilistic scores -
   Integer, Parameter, Public :: nsag = 2               ! - number of seasonal aggregation options -
   Integer, Parameter, Public :: ntime = 5              ! - number of time options -
#if GUI == 0
   Integer, Parameter, Public :: nueo = 3               ! - number of user-error options -
#endif
   Integer, Parameter, Public :: nwr = 3                ! - number of weather roulette options -
!
   Integer, Parameter, Public :: m_file = n_file + 3    ! - total number of input files -
   Integer, Parameter, Public :: ngood = ncorr + nic    ! - number of goodness indices -
   Integer, Parameter, Public :: nps = npall + npcat    ! - number of probabilistic scores -
   Integer, Parameter, Public :: mps = npall + npcat*ng ! - total number of probabilistic scores -
   Integer, Parameter, Public :: nscore = ndcon + ndcat ! - number of scores -
   Integer, Parameter, Public :: nts = ng - 1           ! - number of thresholds -
!
   Integer, Dimension(   nps), Parameter, Public :: ipsi = &  ! - probabilistic score identifiers -
      [101, 111, 121, 122, 125, 131, 141, 151, 152, 153, &
     & 201, 202, 203, 221, 222, 223, 224, 225, 231]
   Integer, Dimension(   nbs), Parameter, Public :: nbss = &  ! - number of probability bins -
      [11, 21]
   Integer, Dimension(nev), Parameter, Public :: ievs = &     ! - error-variance options -
      [ie_cvh, ie_fit, ie_ret, ie_cv2]
   Integer, Dimension(nlink), Parameter, Public :: ilinks = & ! - link function options -
      [il_idt, il_lgt, il_inv, il_log, il_sqr, il_lgm]
!
! Character parameters
   Character(Len=*), Dimension(m_file), Parameter, Public :: c_file = &    ! - input file types -
      ['X', 'Y', 'Z', '1', '2', '3']
!
! Logical parameters
   Logical, Dimension(nscore), Parameter, Public :: lposcore = & ! - positively oriented score? -
      [  .true.,  .true.,  .true.,  .true.,  .true., .false., .false., .false., &
         .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.]
   Logical, Dimension(nscore), Parameter, Public :: lbmscore = & ! - beginner mode score? -
      [  .true., .false.,  .true., .false., .false., .false., .false.,  .true., &
        .false., .false., .false., .false., .false.,  .true.,  .true.,  .true.]
!
End Module data_cpt_constants
