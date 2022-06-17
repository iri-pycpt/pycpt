! Author: Simon Mason
Module iofiles
!
! Modules
   Use data_io_constants, Only: ldir, ldsc, lext, lfil, lnam, lprd, lstr
   Use time,              Only: pdate, pprd
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: file_mtime, file_reset, file_version, files_open, files_reset, get_new_file, get_old_file, geto_gen, geto_file, &
             init_dirs, init_ifile, init_iofiles, init_ofiles, max_width, open_iofile, parameter_write_iofiles, parse_file,  &
             proj_read_iofiles, proj_write_iofiles, prompt_results_file, reset_iofiles, set_fmt, set_grey_files, set_mfile,  &
             set_pfile
#if GUI == 1
   Public :: check_new_file, get_fmt
#else
   Public :: output_format, output_precision
#endif
!
! Derived type definitions
!
! - file format -
   Public :: fformat
   Type fformat
      Sequence
      Integer :: iver ! - CPT file version number -
      Integer :: ifmt ! - file format -
      Integer :: iacc ! - file access -
      Integer :: iprc ! - data precision -
      Integer :: lrec ! - record length -
   End Type fformat
!
! - generic file -
   Public cptfile
   Type cptfile
      Sequence
      Character(Len=lfil) :: ffile ! - file -
      Character(Len=ldir) :: fdir  ! - file directory -
      Character(Len=lnam) :: fname ! - file name -
      Character(Len=lext) :: fext  ! - file extension -
      Character(Len=ldsc) :: desc  ! - file description -
!
      Type(fformat) :: ffmt        ! - file format -
!
      Logical :: lset              ! - file set? -
   End Type cptfile
!
! - input files -
   Public :: ifile
   Type ifile
      Sequence
      Type(cptfile) :: cfile
!
      Character(Len=lstr) :: cgss  ! - field structure -
      Character(Len=lprd) :: cprd1 ! - first period in file -
      Character(Len=lprd) :: cprdn ! - last period in file -
      Character(Len=lprd) :: cprdb ! - beginning date of interest -
      Character(Len=  21) :: cssn  ! - season -
!
      Integer :: idstr             ! - data structure flag -
      Integer :: igeog             ! - geographical reference flag -
      Integer :: iseq              ! - time sequence flag -
      Integer :: nms               ! - number of models -
      Integer :: nem               ! - number of ensemble members -
      Integer :: nfs               ! - number of fields (including ensemble members) -
      Integer :: nls               ! - number of lagged fields -
      Integer :: nfl               ! - total number of fields and lagged fields -
      Integer :: ngs               ! - number of categories -
      Integer :: nse               ! - number of sub-seasons -
      Integer :: nt                ! - number of time steps -
      Integer :: ntm               ! - number of time steps (monthly) -
      Integer :: nat               ! - number of available time steps -
      Integer :: it1               ! - index of first date of interest -
      Integer :: ntag              ! - number of additional tag lines -
      Integer :: mtime             ! - date and time modified -
      Integer :: lmax              ! - maximum length of line -
!
      Type(pprd) :: prd1           ! - period of first data -
      Type(pprd) :: prdn           ! - period of last data -
      Type(pdate) :: fdate         ! - first date of interest -
      Type(pdate) :: bdate         ! - beginning date of interest (including persistence component) -
!
      Logical :: lstack            ! - stacked fields? -
      Logical :: lensemble         ! - ensemble fields? -
   End Type ifile
!
! - output files -
   Public ofile
   Type ofile
      Sequence
      Type(cptfile) :: cfile ! - file format -
!
      Integer :: nfile       ! - file number -
   End Type ofile
!
! Scalars
!
! Integer scalars
   Integer, Target, Public :: nx ! - total number of X variables -
   Integer, Target, Public :: ny ! - total number of Y variables -
   Integer, Target, Public :: nz ! - total number of Z variables -
!
   Integer, Pointer, Public :: nv ! - total number of variables -
!
   Integer, Public :: icd       ! - delimiter indicator -
   Integer, Public :: ifmtd = 2 ! - file format identifier -
   Integer, Public :: iprcd = 2 ! - data precision identifier -
   Integer, Public :: ipreco    ! - output precision -
   Integer, Public :: itags     ! - include CPT tags in output file -
   Integer, Public :: mxa       ! - total number of available X variables -
   Integer, Public :: mya       ! - total number of available Y variables -
   Integer, Public :: mza       ! - total number of available Z variables -
   Integer, Public :: nfile     ! - file number -
!
#if GUI == 1
   Integer, Private :: ipg ! - data precision flag -
#endif
!
! Character scalars
   Character(Len=   1), Public :: cxy       ! - current input file -
   Character(Len=   1), Public :: cdelim    ! - delimiter -
   Character(Len=ldir), Public :: idir      ! - default input file directory -
   Character(Len=ldir), Public :: idir_old  ! - old data file directory -
   Character(Len=ldir), Public :: odir      ! - default output-file directory -
   Character(Len=ldir), Public :: odir_old  ! - old output-file directory -
   Character(Len=lnam), Public :: fname     ! - file name -
   Character(Len=lnam), Public :: fname_old ! - old file name -
   Character(Len=lfil), Public :: ffile     ! - file -
   Character(Len=lfil), Public :: ffile_old ! - old file -
   Character(Len=lext), Public :: fext      ! - file extension -
!
   Character(Len=ldir), Private :: lidir ! - latest input directory -
   Character(Len=ldir), Private :: lodir ! - latest output directory -
!
! Derived type scalars
   Type(fformat), Public :: cformat ! - current format -
!
   Type(ifile), Public :: bkfile ! - backup input file -
   Type(ifile), Public :: mfile  ! - monthly Y data input file -
   Type(ifile), Public :: pfile  ! - persistence data input file -
!
   Type(ifile), Public :: xfile  ! - X data input file -
   Type(ifile), Public :: yfile  ! - Y data input file -
   Type(ifile), Public :: zfile  ! - X forecast data input file -
!
   Type(ofile), Public :: atfile ! - attributes diagram output file -
   Type(ofile), Public :: avfile ! - averages output file -
   Type(ofile), Public :: ccfile ! - canonical correlations output file -
   Type(ofile), Public :: cofile ! - correlations output file -
   Type(ofile), Public :: cvfile ! - coefficients of variation output file -
   Type(ofile), Public :: exfile ! - exceedence probabilities output file -
   Type(ofile), Public :: erfile ! - retroactive ensemble predictions output file -
   Type(ofile), Public :: e2file ! - double cross-validated ensemble predictions output file -
   Type(ofile), Public :: fcfile ! - X CCA temporal scores for forecasts output file -
   Type(ofile), Public :: fefile ! - prediction-error variance output file -
   Type(ofile), Public :: fofile ! - forecast odds output file -
   Type(ofile), Public :: fpfile ! - forecast probabilities output file -
   Type(ofile), Public :: fsfile ! - forecast ensembles output file -
   Type(ofile), Public :: fvfile ! - forecasts output file -
   Type(ofile), Public :: fxfile ! - X temporal scores for forecasts output file -
   Type(ofile), Public :: gifile ! - goodness index output file -
   Type(ofile), Public :: gtfile ! - gamma transformation parameters output file -
   Type(ofile), Public :: lpfile ! - prediction limits output file -
   Type(ofile), Public :: lrfile ! - retroactive prediction limits output file -
   Type(ofile), Public :: l2file ! - double cross-validated prediction limits output file -
   Type(ofile), Public :: mxfile ! - merged input data output file -
   Type(ofile), Public :: ocfile ! - observed (i.e., persisted) component of forecast output file -
   Type(ofile), Public :: orfile ! - retroactive odds output file -
   Type(ofile), Public :: o2file ! - double cross-validated odds output file -
   Type(ofile), Public :: pbfile ! - PC regression coefficients output file -
   Type(ofile), Public :: pcfile ! - predicted component of forecast output file -
   Type(ofile), Public :: prfile ! - retroactive forecast probabilities output file -
   Type(ofile), Public :: psfile ! - probabilistic scores output file -
   Type(ofile), Public :: pvfile ! - p-values output file -
   Type(ofile), Public :: p2file ! - double cross-validated forecast probabilities output file -
   Type(ofile), Public :: rbfile ! - regression coefficients output file -
   Type(ofile), Public :: rcfile ! - retroactive categories output file -
   Type(ofile), Public :: rhfile ! - ranked-hits output file -
   Type(ofile), Public :: rofile ! - ROC output file (individual point) -
   Type(ofile), Public :: rrfile ! - ROC output file (all points) -
   Type(ofile), Public :: sdfile ! - standard deviations output file -
   Type(ofile), Public :: skfile ! - skill output file -
   Type(ofile), Public :: thfile ! - thresholds output file -
   Type(ofile), Public :: tyfile ! - transformed Y input data output file -
   Type(ofile), Public :: t2file ! - transformed double cross-validated predictions output file -
   Type(ofile), Public :: vrfile ! - retroactive prediction-error variance output file -
   Type(ofile), Public :: vxfile ! - standardized X values output file -
   Type(ofile), Public :: vyfile ! - standardized Y values output file -
   Type(ofile), Public :: vzfile ! - standardized Z values output file -
   Type(ofile), Public :: v2file ! - double cross-validated prediction-error variance output file -
   Type(ofile), Public :: wrfile ! - weather roulette output file -
   Type(ofile), Public :: xdfile ! - X input data output file -
   Type(ofile), Public :: xefile ! - X eigenvalues output file -
   Type(ofile), Public :: xifile ! - interpolated X data output file -
   Type(ofile), Public :: xlfile ! - X spatial loadings output file -
   Type(ofile), Public :: xmfile ! - X homogeneous covariance maps output file -
   Type(ofile), Public :: xsfile ! - X temporal scores output file -
   Type(ofile), Public :: xtfile ! - X homogeneous covariance maps time series output file -
   Type(ofile), Public :: xvfile ! - standardized X values output file -
   Type(ofile), Public :: ycfile ! - Y observed categories output file -
   Type(ofile), Public :: ydfile ! - Y input data output file -
   Type(ofile), Public :: yefile ! - Y eigenvalues output file -
   Type(ofile), Public :: yffile ! - fitted values output file -
   Type(ofile), Public :: yhfile ! - cross-validated predictions output file -
   Type(ofile), Public :: ylfile ! - Y spatial loadings output file -
   Type(ofile), Public :: ymfile ! - Y homogeneous covariance maps output file -
   Type(ofile), Public :: yofile ! - Y input data observed (persisted) component output file -
   Type(ofile), Public :: ypfile ! - Y input data persisted + predicted component output file -
   Type(ofile), Public :: yrfile ! - retroactive predictions output file -
   Type(ofile), Public :: ysfile ! - Y temporal scores output file -
   Type(ofile), Public :: ytfile ! - Y homogeneous covariance maps time series output file -
   Type(ofile), Public :: yvfile ! - standardized Y values file -
   Type(ofile), Public :: y2file ! - double cross-validated predictions output file -
   Type(ofile), Public :: zifile ! - interpolated Z data output file -
!
#if GUI == 0
! Arrays
!
! Integer arrays
   Integer, Dimension(42), Public :: new_ids ! - new file identifiers -
!
#endif
!
Contains
!
!
 Subroutine init_dirs (ifail)
!
! Sets working directories
!
! Modules
   Use data_io_constants, Only: cdir, ddir, default_ini, rdir
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: l ! - length of string -
!
   Logical :: le ! - file exists? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Trim
   Intrinsic get_environment_variable
!
! Executable Statements
!
! Set root directory
   Call get_environment_variable ('CPT_BIN_DIR', rdir)
   l = Len_Trim(rdir)
   If (Len_Trim(rdir) > 0) Then
      If (rdir(l:l) /= cdir) rdir = Trim(rdir)//cdir
      Inquire (File=Trim(rdir)//default_ini, Exist=le)
      If (.not.le) Then
         ifail = 1
         Return
      End If
   Else
      Inquire (File=default_ini, Exist=le)
      If (le) Then
         rdir = '.'//cdir
     Else
         Inquire (File='..'//cdir//default_ini, Exist=le)
         If (le) Then
            rdir = '..'//cdir
        Else
            Inquire (File='..'//cdir//'..'//cdir//default_ini, Exist=le)
            If (le) Then
               rdir = '..'//cdir//'..'//cdir
            Else
              ifail = 2
              Return
            End If
         End If
      End If
   End If
!
! Set data directory
   ddir = Trim(rdir)//'data'//cdir
   ifail = 0
!
   Return
 End Subroutine init_dirs
!
!
!
 Subroutine reset_iofiles ()
!
! Resets input and output files
!
! Modules
   Use data_io_constants, Only: ddir
!
! Executable Statements
!
! X input file settings
   Call init_ifile (xfile, &
        dir=idir)
!
! Y input file settings
   Call init_ifile (yfile, &
        dir=idir)
   Call init_ifile (mfile, &
        dir=idir)
   Call init_ifile (pfile, &
        dir=idir)
!
! Backup file settings
   Call init_ifile (bkfile, &
        dir=ddir)
!
! Forecasts data file settings
   Call init_ifile (zfile, &
        dir=idir)
!
! Reset output-file settings
   Call init_ofiles ()
!
! Reset delimiter
   icd = 1
   If (cf_delim()==0) Return
!
! Reset inclusion of CPT output tags
   itags = 1
!
! Reset latest directory
   lidir = ' '
   lodir = ' '
!
! Reset numbers of variables
   Call reset_iofiles_data ()
!
   Return
 End Subroutine reset_iofiles
!
!
!
 Subroutine reset_iofiles_data ()
!
! Resets numbers of variables
!
! Executable Statements
!
! Reset numbers of variables
   nx = 0
   mxa = 0
   ny = 0
   mya = 0
   nz = 0
   mza = 0
!
   Return
 End Subroutine reset_iofiles_data
!
!
!
 Function cf_delim()
!
! Sets delimiting character
!
! Modules
   Use data_text, Only: c_tab
!
! Function result
   Integer :: cf_delim
!
! Executable Statements
!
! Reset delimiter
   Select Case (icd)
    Case (1) ! - tab -
      cdelim = c_tab
    Case (2) ! - space -
      cdelim = ' '
    Case (3) ! - comma -
      cdelim = ','
   End Select
   cf_delim = 1
!
   Return
 End Function cf_delim
!
!
!
 Subroutine init_iofiles ()
!
! Initialises data structures and output-file numbers and descriptions
!
! Modules
   Use labels, Only: cg_indata_t, cg_stddata, &
                    l_averages, l_correls, l_cvars, l_exceedps, l_fcasts, l_goodness, l_scoresp, l_stdevs, l_threshs, l_wrlt
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Initialise default file format
   Call init_dformat ()
!
! Initialise input data output files
! - observations -
   xdfile%nfile = 101
   xdfile%cfile%desc = 'X '//Trim(cg_indata_t)
   ydfile%nfile = 102
   ydfile%cfile%desc = 'Y '//Trim(cg_indata_t)
   yofile%nfile = 103
   yofile%cfile%desc = 'Y '//Trim(cg_indata_t)//' (persisted component)'
   ypfile%nfile = 104
   ypfile%cfile%desc = 'Y '//Trim(cg_indata_t)//' (observed + predicted)'
! - interpolations -
   xifile%nfile = 111
   xifile%cfile%desc = 'Interpolated X Data'
! transformations -
   tyfile%nfile = 112
   tyfile%cfile%desc = 'Transformed Y Data'
! - gamma transformation parameters output file -
   gtfile%nfile = 113
   gtfile%cfile%desc = 'Gamma Transformation Parameters'
! - categories -
   ycfile%nfile = 121
   ycfile%cfile%desc = 'Y Categories Data'
   rcfile%nfile = 122
   rcfile%cfile%desc = 'Retroactive Categories'
! - merged data -
   mxfile%nfile = 131
   mxfile%cfile%desc = 'Merged Input Data'
!
! Initialise hindcasts output files
   yhfile%nfile = 201
   yhfile%cfile%desc = 'Cross-Validated Predictions'
   yffile%nfile = 205
   yffile%cfile%desc = 'Fitted Values'
   yrfile%nfile = 211
   yrfile%cfile%desc = 'Retroactive Predictions'
   erfile%nfile = 212
   erfile%cfile%desc = 'Retroactive Ensemble Predictions'
   lrfile%nfile = 213
   lrfile%cfile%desc = 'Retroactive Prediction Limits'
   prfile%nfile = 214
   prfile%cfile%desc = 'Retroactive Forecast Probabilities'
   orfile%nfile = 215
   orfile%cfile%desc = 'Retroactive Forecast Odds'
   vrfile%nfile = 216
   vrfile%cfile%desc = 'Retroactive Prediction Error Variances'
   y2file%nfile = 221
   y2file%cfile%desc = 'Double Cross-Validated Predictions'
   e2file%nfile = 222
   e2file%cfile%desc = 'Double Cross-Validated Ensemble Predictions'
   l2file%nfile = 223
   l2file%cfile%desc = 'Double Cross-Validated Prediction Limits'
   p2file%nfile = 224
   p2file%cfile%desc = 'Double Cross-Validated Forecast Probabilities'
   o2file%nfile = 225
   o2file%cfile%desc = 'Double Cross-Validated Forecast Odds'
   v2file%nfile = 226
   v2file%cfile%desc = 'Double Cross-Validated Prediction Error Variances'
!
! Initialise modes output files
! - EOFs -
   xefile%nfile = 301
   xefile%cfile%desc = 'X Eigenvalues'
   xlfile%nfile = 302
   xlfile%cfile%desc = 'X Spatial Loadings'
   xsfile%nfile = 303
   xsfile%cfile%desc = 'X Temporal Scores'
   yefile%nfile = 311
   yefile%cfile%desc = 'Y Eigenvalues'
   ylfile%nfile = 312
   ylfile%cfile%desc = 'Y Spatial Loadings'
   ysfile%nfile = 313
   ysfile%cfile%desc = 'Y Temporal Scores'
! - CCA -
   ccfile%nfile = 401
   ccfile%cfile%desc = 'Canonical Correlations'
   xmfile%nfile = 411
   xmfile%cfile%desc = 'X CCA Map Loadings'
   xtfile%nfile = 412
   xtfile%cfile%desc = 'X CCA Map Series'
   ymfile%nfile = 421
   ymfile%cfile%desc = 'Y CCA Map Loadings'
   ytfile%nfile = 422
   ytfile%cfile%desc = 'Y CCA Map Series'
! - MLR and PCR -
   rbfile%nfile = 431
   rbfile%cfile%desc = 'Regression Coefficients'
   pbfile%nfile = 432
   pbfile%cfile%desc = 'PC Regression Coefficients'
!
! Initialise forecasts output files
! - probabilistic forecasts -
   fpfile%nfile = 501
   fpfile%cfile%desc = 'Forecast Probabilities'
   fofile%nfile = 502
   fofile%cfile%desc = 'Forecast Odds'
! - deterministic forecasts -
   fvfile%nfile = 511
   fvfile%cfile%desc = Trim(l_fcasts%c)
   ocfile%nfile = 5111
   ocfile%cfile%desc = 'Observed component of forecasts'
   pcfile%nfile = 5112
   pcfile%cfile%desc = 'Predicted component of forecasts'
   fsfile%nfile = 512
   fsfile%cfile%desc = 'Forecast Ensembles'
   lpfile%nfile = 513
   lpfile%cfile%desc = 'Prediction Limits'
   fefile%nfile = 514
   fefile%cfile%desc = 'Prediction Error Variances'
! - flexible forecasts -
   exfile%nfile = 521
   exfile%cfile%desc = Trim(l_exceedps%c)
! - X and CCA mode predictors -
   fxfile%nfile = 531
   fxfile%cfile%desc = 'Predictor Time Scores'
   fcfile%nfile = 532
   fcfile%cfile%desc = 'Predictor CCA Time Scores'
! - interpolations -
   zifile%nfile = 541
   zifile%cfile%desc = 'Interpolated Z Data'
!
! Initialise climatologies output files
   thfile%nfile = 601
   thfile%cfile%desc = Trim(l_threshs%c)
   avfile%nfile = 602
   avfile%cfile%desc = Trim(l_averages%c)
   sdfile%nfile = 604
   sdfile%cfile%desc = Trim(l_stdevs%c)
   cvfile%nfile = 605
   cvfile%cfile%desc = Trim(l_cvars%c)
   cofile%nfile = 603
   cofile%cfile%desc = Trim(l_correls%c)
! - standardized observations -
   vxfile%nfile = 611
   vxfile%cfile%desc = 'X '//Trim(cg_stddata)
   vyfile%nfile = 612
   vyfile%cfile%desc = 'Y '//Trim(cg_stddata)
   vzfile%nfile = 613
   vzfile%cfile%desc = 'Predictor '//Trim(cg_stddata)
!
! Initialise skill scores output files
! - goodness index -
   gifile%nfile = 701
   gifile%cfile%desc = Trim(l_goodness%c)
! - deterministic scores -
   skfile%nfile = 711
   skfile%cfile%desc = 'Skill Scores'
   pvfile%nfile = 712
   pvfile%cfile%desc = 'P-values'
   rofile%nfile = 713
   rofile%cfile%desc = 'ROC Results'
! - probabilistic scores -
   psfile%nfile = 721
   psfile%cfile%desc = Trim(l_scoresp%c)
   rrfile%nfile = 723
   rrfile%cfile%desc = 'ROC Results'
   atfile%nfile = 724
   atfile%cfile%desc = 'Reliability Results'
   rhfile%nfile = 725
   rhfile%cfile%desc = 'Ranked-hits Results'
   wrfile%nfile = 726
   wrfile%cfile%desc = Trim(l_wrlt%c)
!
#if GUI == 0
! Set file ID conversions
   new_ids( 1) = xdfile%nfile ! - X input data -
   new_ids( 2) = xifile%nfile ! - Interpolated X Data -
   new_ids( 3) = ydfile%nfile ! - Y input data -
   new_ids( 4) = tyfile%nfile ! - Transformed Y Data -
   new_ids( 5) = yhfile%nfile ! - Cross-Validated Predictions -
   new_ids( 6) = rcfile%nfile ! - Retroactive Categories -
   new_ids( 7) = yrfile%nfile ! - Retroactive Predictions -
   new_ids( 8) = prfile%nfile ! - Retroactive Forecast Probabilities -
   new_ids( 9) = lrfile%nfile ! - Retroactive Prediction Limits -
   new_ids(10) = xefile%nfile ! - X Eigenvalues -
   new_ids(11) = xlfile%nfile ! - X Spatial Loadings -
   new_ids(12) = xsfile%nfile ! - X Temporal Scores -
   new_ids(13) = yefile%nfile ! - Y Eigenvalues -
   new_ids(14) = ylfile%nfile ! - Y Spatial Loadings -
   new_ids(15) = ysfile%nfile ! - Y Temporal Scores -
   new_ids(16) = ccfile%nfile ! - Canonical Correlations -
   new_ids(17) = xmfile%nfile ! - X CCA Map Loadings -
   new_ids(18) = xtfile%nfile ! - X CCA Map Series -
   new_ids(19) = ymfile%nfile ! - Y CCA Map Loadings -
   new_ids(20) = ytfile%nfile ! - Y CCA Map Series -
   new_ids(21) = rbfile%nfile ! - Regression Coefficients -
   new_ids(22) = pbfile%nfile ! - PC Regression Coefficients -
   new_ids(23) = fpfile%nfile ! - Forecast Probabilities -
   new_ids(24) = fofile%nfile ! - Forecast Odds -
   new_ids(25) = fvfile%nfile ! - Forecasts -
   new_ids(26) = fsfile%nfile ! - Forecast Ensembles -
   new_ids(27) = fefile%nfile ! - Prediction Error Variances -
   new_ids(28) = lpfile%nfile ! - Prediction Limits -
   new_ids(29) = fxfile%nfile ! - Predictor Time Scores -
   new_ids(30) = exfile%nfile ! - Exceedance Probabilities -
   new_ids(31) = skfile%nfile ! - Skill Scores -
   new_ids(32) = pvfile%nfile ! - P-values -
   new_ids(33) = rofile%nfile ! - ROC Results
   new_ids(34) = rrfile%nfile ! - ROC Results -
   new_ids(35) = atfile%nfile ! - Reliability Results -
   new_ids(36) = psfile%nfile ! - Probabilistic Scores -
   new_ids(37) = rhfile%nfile ! - Ranked-hits Results -
   new_ids(38) = wrfile%nfile ! - Weather Roulette -
   new_ids(39) = gifile%nfile ! - Goodness Index -
   new_ids(40) = thfile%nfile ! - Thresholds -
   new_ids(41) = avfile%nfile ! - Averages -
   new_ids(42) = cofile%nfile ! - Correlations -
!
#endif
   Return
 End Subroutine init_iofiles
!
!
!
 Subroutine init_dformat ()
!
! Initialises default file format
!
! Executable Statements
!
! Initialise default file format
   cformat%iver = 0
   cformat%ifmt = ifmtd
   cformat%iacc = 1
   cformat%iprc = iprcd
   cformat%lrec = 0
!
   Return
 End Subroutine init_dformat
!
!
!
 Subroutine init_ifile (afile, &
            dir)
!
! Initialises an input file
!
! Modules
   Use labels, Only: ca_dstruct_l, cg_na
   Use time,   Only: Assignment(=)
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - input file -
!
! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: dir ! - directory -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len, Present
!
! Executable Statements
!
! Set input file settings
   afile%cfile%ffile = ' '
   If (Present(dir)) afile%cfile%fdir=dir
   afile%cfile%fname = ' '
   afile%cfile%fext = ' '
   afile%cfile%desc = ' '
   afile%cfile%ffmt%iver = 0
   afile%cfile%ffmt%ifmt = 2
   afile%cfile%ffmt%iacc = 1
   afile%cfile%ffmt%iprc = 2
   afile%cfile%ffmt%lrec = 0
   afile%cfile%lset = .false.
   afile%cgss = ca_dstruct_l(0)(1:Len(afile%cgss))
   afile%cprd1 = cg_na
   afile%cprdn = cg_na
   afile%cprdb = ' '
   afile%cssn = ' '
   afile%idstr = 0
   afile%igeog = 0
   afile%iseq = 0
   afile%nms = 0
   afile%nem = 0
   afile%nfs = 0
   afile%nls = 0
   afile%nfl = 0
   afile%nse = 0
   afile%ngs = 0
   afile%nt = 0
   afile%ntm = 0
   afile%nat = 0
   afile%it1 = 0
   afile%ntag = 0
   afile%mtime = 0
   afile%lmax = 0
   afile%prd1 = 0
   afile%prdn = 0
   afile%fdate = 0
   afile%bdate = 0
   afile%lstack = .false.
   afile%lensemble = .false.
!
   Return
 End Subroutine init_ifile
!
!
!
 Subroutine init_ofiles ()
!
! Initialises output files
!
! Executable Statements
!
! Reset default format
   Call init_dformat ()
!
! Input files
! - X input data -
   Call init_file (xdfile%cfile)
! - interpolated X data -
   Call init_file (xifile%cfile)
! - Y input data -
   Call init_file (ydfile%cfile)
! - Y input data observed component -
   Call init_file (yofile%cfile)
! - Y input data persisted + predicted component -
   Call init_file (ypfile%cfile)
! - transformed Y input data -
   Call init_file (tyfile%cfile)
! - gamma transformation parameters output file -
   Call init_file (gtfile%cfile)
! - Y categories -
   Call init_file (ycfile%cfile)
! - retroactive categories -
   Call init_file (rcfile%cfile)
! - merged input data -
   Call init_file (mxfile%cfile)
!
! Prediction file settings
! - cross-validated predictions -
   Call init_file (yhfile%cfile)
! - fitted values -
   Call init_file (yffile%cfile)
! - retroactive predictions -
   Call init_file (yrfile%cfile)
! - retroactive ensemble predictions -
   Call init_file (erfile%cfile)
! - retroactive prediction limits -
   Call init_file (lrfile%cfile)
! - retroactive forecast probabilities -
   Call init_file (prfile%cfile)
! - retroactive forecast odds -
   Call init_file (orfile%cfile)
! - retroactive prediction error variances -
   Call init_file (vrfile%cfile)
! - double cross-validated predictions -
   Call init_file (y2file%cfile)
! - double cross-validated forecast ensembles -
   Call init_file (e2file%cfile)
! - double cross-validated prediction limits -
   Call init_file (l2file%cfile)
! - double cross-validated forecast probabilities -
   Call init_file (p2file%cfile)
! - double cross-validated forecast odds -
   Call init_file (o2file%cfile)
! - double cross-validated prediction error variances -
   Call init_file (v2file%cfile)
!
! X output-file settings
! - eigenvalues -
   Call init_file (xefile%cfile)
! - spatial loadings -
   Call init_file (xlfile%cfile)
! - temporal scores -
   Call init_file (xsfile%cfile)
! - homogeneous maps -
   Call init_file (xmfile%cfile)
! - homogeneous time series -
   Call init_file (xtfile%cfile)
!
! Y output-file settings
! - eigenvalues -
   Call init_file (yefile%cfile)
! - spatial loadings -
   Call init_file (ylfile%cfile)
! - temporal scores -
   Call init_file (ysfile%cfile)
! - homogeneous maps -
   Call init_file (ymfile%cfile)
! - homogeneous time series -
   Call init_file (ytfile%cfile)
!
! Canonical correlation output-file settings
   Call init_file (ccfile%cfile)
!
! Regression output-file settings
   Call init_file (rbfile%cfile)
!
! PC regression output-file settings
   Call init_file (pbfile%cfile)
!
! Forecast files
! - forecast probabilities -
   Call init_file (fpfile%cfile)
! - forecast odds -
   Call init_file (fofile%cfile)
! - forecasts -
   Call init_file (fvfile%cfile)
! - observed component of forecasts -
   Call init_file (ocfile%cfile)
! - predicted component of forecasts -
   Call init_file (pcfile%cfile)
! - forecast ensembles -
   Call init_file (fsfile%cfile)
! - prediction-error variances -
   Call init_file (fefile%cfile)
! - prediction limits -
   Call init_file (lpfile%cfile)
! - exceedence probabilities -
   Call init_file (exfile%cfile)
! - interpolated X data -
   Call init_file (zifile%cfile)
! - predictor time scores -
   Call init_file (fxfile%cfile)
! - predictor CCA time scores -
   Call init_file (fcfile%cfile)
!
! Skill scores and p-values files
   Call init_file (skfile%cfile)
   Call init_file (pvfile%cfile)
!
! Verification files
! - ROC files -
   Call init_file (rofile%cfile)
   Call init_file (rrfile%cfile)
! - attributes diagram file -
   Call init_file (atfile%cfile)
! - probabilistic scores file -
   Call init_file (psfile%cfile)
! - attributes diagram file -
   Call init_file (rhfile%cfile)
! - weather roulette file -
   Call init_file (wrfile%cfile)
! - goodness index file -
   Call init_file (gifile%cfile)
!
! Climatologies
! - thresholds file -
   Call init_file (thfile%cfile)
! - averages file -
   Call init_file (avfile%cfile)
! - standard deviations file -
   Call init_file (sdfile%cfile)
! - coefficients of variation file -
   Call init_file (cvfile%cfile)
! - correlations file -
   Call init_file (cofile%cfile)
! - standardized X data -
   Call init_file (vxfile%cfile)
! - standardized Y data -
   Call init_file (vyfile%cfile)
! - standardized Z data -
   Call init_file (vzfile%cfile)
!
   Return
 End Subroutine init_ofiles
!
!
!
 Subroutine init_file (f)
!
! Initialises a file
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(cptfile), Intent(InOut) :: f ! - output file -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len, Repeat
!
! Executable Statements
   f%lset = .false.
   f%fname = Repeat(' ', Len(f%fname))
   f%ffile = Repeat(' ', Len(f%ffile))
   f%fdir = odir
   f%fext = Repeat(' ', Len(f%fext))
   f%ffmt%ifmt = cformat%ifmt
   f%ffmt%iacc = 1
   f%ffmt%iprc = cformat%iprc
   f%ffmt%lrec = 0
!
   Return
 End Subroutine init_file
!
!
!
 Subroutine file_reset (lfile, fname)
!
! Resets a file's name and its setting flag
!
! Arguments
!
! Dummy arguments
! - output scalars -
   Character(Len=*), Intent(Out) :: fname ! - file name -
!
   Logical, Intent(Out) :: lfile ! - file set? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len, Repeat
!
! Executable Statements
!
! Reset file flag
   lfile = .false.
!
! Reset file name
   fname = Repeat(' ', Len(fname))
!
   Return
 End Subroutine file_reset
!
!
!
 Subroutine files_reset ()
!
! Resets output files
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim
!
! Executable Statements
!
! Input data with missing values estimated
! - X input data -
   If (xdfile%cfile%lset .or. Len_Trim(xdfile%cfile%fname) > 0) Call file_reset (xdfile%cfile%lset, xdfile%cfile%fname)
! - interpolated X data -
   If (xifile%cfile%lset .or. Len_Trim(xifile%cfile%fname) > 0) Call file_reset (xifile%cfile%lset, xifile%cfile%fname)
! - Y input data -
   If (ydfile%cfile%lset .or. Len_Trim(ydfile%cfile%fname) > 0) Call file_reset (ydfile%cfile%lset, ydfile%cfile%fname)
! - Y input data observed component -
   If (yofile%cfile%lset .or. Len_Trim(yofile%cfile%fname) > 0) Call file_reset (yofile%cfile%lset, yofile%cfile%fname)
! - Y input data persisted + predicted component -
   If (ypfile%cfile%lset .or. Len_Trim(ypfile%cfile%fname) > 0) Call file_reset (ypfile%cfile%lset, ypfile%cfile%fname)
! - transformed Y input data -
   If (tyfile%cfile%lset .or. Len_Trim(tyfile%cfile%fname) > 0) Call file_reset (tyfile%cfile%lset, tyfile%cfile%fname)
! - gamma transformation parameters output file -
   If (gtfile%cfile%lset .or. Len_Trim(gtfile%cfile%fname) > 0) Call file_reset (gtfile%cfile%lset, gtfile%cfile%fname)
! - Y categories -
   If (ycfile%cfile%lset .or. Len_Trim(ycfile%cfile%fname) > 0) Call file_reset (ycfile%cfile%lset, ycfile%cfile%fname)
! - retroactive categories -
   If (rcfile%cfile%lset .or. Len_Trim(rcfile%cfile%fname) > 0) Call file_reset (rcfile%cfile%lset, rcfile%cfile%fname)
! - merged input data -
   If (mxfile%cfile%lset .or. Len_Trim(mxfile%cfile%fname) > 0) Call file_reset (mxfile%cfile%lset, mxfile%cfile%fname)
!
! Predictions
! - cross-validated predictions -
   If (yhfile%cfile%lset .or. Len_Trim(yhfile%cfile%fname) > 0) Call file_reset (yhfile%cfile%lset, yhfile%cfile%fname)
! - fitted values -
   If (yffile%cfile%lset .or. Len_Trim(yffile%cfile%fname) > 0) Call file_reset (yffile%cfile%lset, yffile%cfile%fname)
! - retroactive predictions -
   If (yrfile%cfile%lset .or. Len_Trim(yrfile%cfile%fname) > 0) Call file_reset (yrfile%cfile%lset, yrfile%cfile%fname)
! - retroactive ensemble predictions -
   If (erfile%cfile%lset .or. Len_Trim(erfile%cfile%fname) > 0) Call file_reset (erfile%cfile%lset, erfile%cfile%fname)
! - retroactive prediction limits -
   If (lrfile%cfile%lset .or. Len_Trim(lrfile%cfile%fname) > 0) Call file_reset (lrfile%cfile%lset, lrfile%cfile%fname)
! - retroactive forecast probabilities -
   If (prfile%cfile%lset .or. Len_Trim(prfile%cfile%fname) > 0) Call file_reset (prfile%cfile%lset, prfile%cfile%fname)
! - retroactive forecast odds -
   If (orfile%cfile%lset .or. Len_Trim(orfile%cfile%fname) > 0) Call file_reset (orfile%cfile%lset, orfile%cfile%fname)
! - retroactive prediction error variances -
   If (vrfile%cfile%lset .or. Len_Trim(vrfile%cfile%fname) > 0) Call file_reset (vrfile%cfile%lset, vrfile%cfile%fname)
! - double cross-validated predictions -
   If (y2file%cfile%lset .or. Len_Trim(y2file%cfile%fname) > 0) Call file_reset (y2file%cfile%lset, y2file%cfile%fname)
! - double cross-validated forecast ensembles -
   If (e2file%cfile%lset .or. Len_Trim(e2file%cfile%fname) > 0) Call file_reset (e2file%cfile%lset, e2file%cfile%fname)
! - double cross-validated prediction limits -
   If (l2file%cfile%lset .or. Len_Trim(l2file%cfile%fname) > 0) Call file_reset (l2file%cfile%lset, l2file%cfile%fname)
! - double cross-validated forecast probabilities -
   If (p2file%cfile%lset .or. Len_Trim(p2file%cfile%fname) > 0) Call file_reset (p2file%cfile%lset, p2file%cfile%fname)
! - double cross-validated forecast odds -
   If (o2file%cfile%lset .or. Len_Trim(o2file%cfile%fname) > 0) Call file_reset (o2file%cfile%lset, o2file%cfile%fname)
! - double cross-validated prediction error variances -
   If (v2file%cfile%lset .or. Len_Trim(v2file%cfile%fname) > 0) Call file_reset (v2file%cfile%lset, v2file%cfile%fname)
!
! Eigenvalues
! - X eigenvalues -
   If (xefile%cfile%lset .or. Len_Trim(xefile%cfile%fname) > 0) Call file_reset (xefile%cfile%lset, xefile%cfile%fname)
! - Y eigenvalues -
   If (yefile%cfile%lset .or. Len_Trim(yefile%cfile%fname) > 0) Call file_reset (yefile%cfile%lset, yefile%cfile%fname)
!
! Spatial loadings
! - X spatial loadings -
   If (xlfile%cfile%lset .or. Len_Trim(xlfile%cfile%fname) > 0) Call file_reset (xlfile%cfile%lset, xlfile%cfile%fname)
! - Y spatial loadings -
   If (ylfile%cfile%lset .or. Len_Trim(ylfile%cfile%fname) > 0) Call file_reset (ylfile%cfile%lset, ylfile%cfile%fname)
!
! Temporal scores
! - X scores -
   If (xsfile%cfile%lset .or. Len_Trim(xsfile%cfile%fname) > 0) Call file_reset (xsfile%cfile%lset, xsfile%cfile%fname)
! - Y scores -
   If (ysfile%cfile%lset .or. Len_Trim(ysfile%cfile%fname) > 0) Call file_reset (ysfile%cfile%lset, ysfile%cfile%fname)
!
! CCA results
! - canonical correlations -
   If (ccfile%cfile%lset .or. Len_Trim(ccfile%cfile%fname) > 0) Call file_reset (ccfile%cfile%lset, ccfile%cfile%fname)
! - X homogeneous maps -
   If (xmfile%cfile%lset .or. Len_Trim(xmfile%cfile%fname) > 0) Call file_reset (xmfile%cfile%lset, xmfile%cfile%fname)
! - Y homogeneous maps -
   If (ymfile%cfile%lset .or. Len_Trim(ymfile%cfile%fname) > 0) Call file_reset (ymfile%cfile%lset, ymfile%cfile%fname)
! - X homogeneous map series -
   If (xtfile%cfile%lset .or. Len_Trim(xtfile%cfile%fname) > 0) Call file_reset (xtfile%cfile%lset, xtfile%cfile%fname)
! - Y homogeneous map series -
   If (ytfile%cfile%lset .or. Len_Trim(ytfile%cfile%fname) > 0) Call file_reset (ytfile%cfile%lset, ytfile%cfile%fname)
!
! Regression coefficients
! - regression coefficients -
   If (rbfile%cfile%lset .or. Len_Trim(rbfile%cfile%fname) > 0) Call file_reset (rbfile%cfile%lset, rbfile%cfile%fname)
! - PC regression coefficients -
   If (pbfile%cfile%lset .or. Len_Trim(pbfile%cfile%fname) > 0) Call file_reset (pbfile%cfile%lset, pbfile%cfile%fname)
!
! Forecasts
! - forecast probabilities -
   If (fpfile%cfile%lset .or. Len_Trim(fpfile%cfile%fname) > 0) Call file_reset (fpfile%cfile%lset, fpfile%cfile%fname)
! - forecast odds -
   If (fofile%cfile%lset .or. Len_Trim(fofile%cfile%fname) > 0) Call file_reset (fofile%cfile%lset, fofile%cfile%fname)
! - forecast values -
   If (fvfile%cfile%lset .or. Len_Trim(fvfile%cfile%fname) > 0) Call file_reset (fvfile%cfile%lset, fvfile%cfile%fname)
! - observed component of forecasts -
   If (ocfile%cfile%lset .or. Len_Trim(ocfile%cfile%fname) > 0) Call file_reset (ocfile%cfile%lset, ocfile%cfile%fname)
! - observed component of forecasts -
   If (pcfile%cfile%lset .or. Len_Trim(pcfile%cfile%fname) > 0) Call file_reset (pcfile%cfile%lset, pcfile%cfile%fname)
! - forecast ensembles -
   If (fsfile%cfile%lset .or. Len_Trim(fsfile%cfile%fname) > 0) Call file_reset (fsfile%cfile%lset, fsfile%cfile%fname)
! - prediction-error variances -
   If (fefile%cfile%lset .or. Len_Trim(fefile%cfile%fname) > 0) Call file_reset (fefile%cfile%lset, fefile%cfile%fname)
! - prediction limits -
   If (lpfile%cfile%lset .or. Len_Trim(lpfile%cfile%fname) > 0) Call file_reset (lpfile%cfile%lset, lpfile%cfile%fname)
! - exceedence probabilities -
   If (exfile%cfile%lset .or. Len_Trim(exfile%cfile%fname) > 0) Call file_reset (exfile%cfile%lset, exfile%cfile%fname)
! - predictor forecast scores -
   If (fxfile%cfile%lset .or. Len_Trim(fxfile%cfile%fname) > 0) Call file_reset (fxfile%cfile%lset, fxfile%cfile%fname)
! - predictor CCA forecast scores -
   If (fcfile%cfile%lset .or. Len_Trim(fcfile%cfile%fname) > 0) Call file_reset (fcfile%cfile%lset, fcfile%cfile%fname)
! - interpolated Z data -
   If (zifile%cfile%lset .or. Len_Trim(zifile%cfile%fname) > 0) Call file_reset (zifile%cfile%lset, zifile%cfile%fname)
!
! Skill scores and p-values
   If (skfile%cfile%lset .or. Len_Trim(skfile%cfile%fname) > 0) Call file_reset (skfile%cfile%lset, skfile%cfile%fname)
   If (pvfile%cfile%lset .or. Len_Trim(pvfile%cfile%fname) > 0) Call file_reset (pvfile%cfile%lset, pvfile%cfile%fname)
!
! Verification
! - ROC results -
   If (rofile%cfile%lset .or. Len_Trim(rofile%cfile%fname) > 0) Call file_reset (rofile%cfile%lset, rofile%cfile%fname)
   If (rrfile%cfile%lset .or. Len_Trim(rrfile%cfile%fname) > 0) Call file_reset (rrfile%cfile%lset, rrfile%cfile%fname)
! - reliability results -
   If (atfile%cfile%lset .or. Len_Trim(atfile%cfile%fname) > 0) Call file_reset (atfile%cfile%lset, atfile%cfile%fname)
! - probabilistic scores -
   If (psfile%cfile%lset .or. Len_Trim(psfile%cfile%fname) > 0) Call file_reset (psfile%cfile%lset, psfile%cfile%fname)
! - ranked hits -
   If (rhfile%cfile%lset .or. Len_Trim(rhfile%cfile%fname) > 0) Call file_reset (rhfile%cfile%lset, rhfile%cfile%fname)
! - weather roulette -
   If (wrfile%cfile%lset .or. Len_Trim(wrfile%cfile%fname) > 0) Call file_reset (wrfile%cfile%lset, wrfile%cfile%fname)
! - goodness index -
   If (gifile%cfile%lset .or. Len_Trim(gifile%cfile%fname) > 0) Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
!
! Climatologies
! - thresholds -
   If (thfile%cfile%lset .or. Len_Trim(thfile%cfile%fname) > 0) Call file_reset (thfile%cfile%lset, thfile%cfile%fname)
! - averages -
   If (avfile%cfile%lset .or. Len_Trim(avfile%cfile%fname) > 0) Call file_reset (avfile%cfile%lset, avfile%cfile%fname)
! - standard deviations -
   If (sdfile%cfile%lset .or. Len_Trim(sdfile%cfile%fname) > 0) Call file_reset (sdfile%cfile%lset, sdfile%cfile%fname)
! - coefficients of variation -
   If (cvfile%cfile%lset .or. Len_Trim(cvfile%cfile%fname) > 0) Call file_reset (cvfile%cfile%lset, cvfile%cfile%fname)
! - correlations -
   If (cofile%cfile%lset .or. Len_Trim(cofile%cfile%fname) > 0) Call file_reset (cofile%cfile%lset, cofile%cfile%fname)
! - standardized X data -
   If (vxfile%cfile%lset .or. Len_Trim(vxfile%cfile%fname) > 0) Call file_reset (vxfile%cfile%lset, vxfile%cfile%fname)
! - standardized Y data -
   If (vyfile%cfile%lset .or. Len_Trim(vyfile%cfile%fname) > 0) Call file_reset (vyfile%cfile%lset, vyfile%cfile%fname)
! - standardized Z data -
   If (vzfile%cfile%lset .or. Len_Trim(vzfile%cfile%fname) > 0) Call file_reset (vzfile%cfile%lset, vzfile%cfile%fname)
!
   Return
 End Subroutine files_reset
!
!
!
 Subroutine open_iofile (iin, ffile, lread, lfmt, lrecl, ifail)
!
! Opens an input file
!
! Modules
   Use data_io_constants, Only: lfli
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
!
   Character(Len=*), Intent(In) :: ffile ! - input file -
!
   Logical, Intent(In) :: lread ! - read access? -
   Logical, Intent(In) :: lfmt  ! - formatted file? -
   Logical, Intent(In) :: lrecl ! - record length? -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: ios ! - IO status -
!
   Character(Len=11) :: cfmt ! - formatting -
!
! Executabel Statements
!
! Set formatting
   If (lfmt) Then
     cfmt = 'formatted'
   Else
     cfmt = 'unformatted'
   End If
!
! Open file
   If (lread) Then
      If (lrecl) Then
         Open (Unit = iin, File = ffile, &
                         Access = 'sequential', &
                         Action = 'read', &
                           Form = cfmt, &
                         IOstat = ios, &
                           Recl = lfli, &
                         Status = 'old')
      Else
         Open (Unit = iin, File = ffile, &
                         Access = 'sequential', &
                         Action = 'read', &
                           Form = cfmt, &
                         IOstat = ios, &
                         Status = 'old')
      End If
   Else
      If (lrecl) Then
         Open (Unit = iin, File = ffile, &
                         Access = 'sequential', &
                         Action = 'write', &
                           Form = cfmt, &
                         IOstat = ios, &
                           Recl = lfli, &
                         Status = 'unknown')
      Else
         Open (Unit = iin, File = ffile, &
                         Access = 'sequential', &
                         Action = 'write', &
                           Form = cfmt, &
                         IOstat = ios, &
                         Status = 'unknown')
      End If
   End If
   Select Case (ios)
    Case (0)
      ifail = 0
    Case (128)
      ifail = 1
    Case (134)
      ifail = 2
    Case Default
      ifail = 3
   End Select
!
   Return
 End Subroutine open_iofile
!
!
!
 Function file_mtime(afile)
!
! Determines modification date and time of file
!
! Function result
   Integer :: file_mtime
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: afile ! - file -
!
! Locals
!
! Local scalars
#if FTN95 == 1
   Integer(Kind=2) :: mode, dev, rdev, nlink, ierr ! - dummy variables -
!
   Integer(Kind=3) :: isize ! - file size -
   Integer(Kind=3) :: atime ! - file access time -
   Integer(Kind=3) :: mtime ! - file modification time -
   Integer(Kind=3) :: ctime ! - file creation time -
#elif GFORTRAN == 1
   Integer :: ifail ! - error indicator -
!
! Local arrays
   Integer, Dimension(13) :: ifstat ! - file status values -
#endif
!
! Functions and Subroutines
!
#if FTN95 == 1
! Silverfrost library routines
   External :: fileinfo@
#elif GFORTRAN == 1
! GFortran routines
   Intrinsic stat
#endif
!
! Executable Statements
!
! Get file info
#if FTN95 == 1
   Call fileinfo@ (afile, mode, dev, rdev, nlink, isize, atime, mtime, ctime, ierr)
#elif GFORTRAN == 1
   Call stat (afile, ifstat, ifail)
#endif
!
! Convert to date format
#if FTN95 == 1
   file_mtime = mtime
#elif GFORTRAN == 1
   If (ifail == 0) Then
      file_mtime = ifstat(10)
   Else
      file_mtime = 0
   End If
#else
   file_mtime = 0
#endif
!
   Return
 End Function file_mtime
!
!
!
 Function file_version(afile)
!
! Determines CPT file format version
!
! Modules
   Use data_io_constants, Only: cxmlns, cxmlns_cpt, iin, ltag
!
! Function result
   Integer :: file_version
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: afile ! - filename -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Character(Len=ltag) :: ctag ! - CPT file tag -
!
   Logical :: lrecl ! - record length flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len
!
! Executable Statements
!
! Open file
   lrecl = .false.
1  Call open_iofile (iin, afile, .true., .true., lrecl, ifail)
   If (ifail /= 0) Then
      file_version = -ifail
      Return
   End If
!
! Identify file version
   Read (Unit=iin, Fmt='(A)', IOstat=ifail) ctag
   Close (Unit=iin)
   If (ifail /= 0) Then
      If (.not.lrecl) Then
         lrecl = .true.
         GoTo 1
      End If
      file_version=-4
      Return
   End If
!
! Check for CPT XML namespace flag
   If (Index(ctag,cxmlns//':cpt='//cxmlns_cpt(1:Len(cxmlns_cpt)-1)) > 0) Then
      file_version = 10
   Else
      file_version = 9
   End If
!
   Return
 End Function file_version
!
!
!
 Subroutine set_mfile (icsv, lnewy)
!
! Sets a monthly input file
!
! Modules
   Use data_io_constants, Only: io_com
   Use time,              Only: get_cdate
   Use screen,            Only: window_update
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icsv ! - calculate seasonal values flag -
!
   Logical, Intent(In) :: lnewy ! - new Y-file? -
!
! Locals
!
! Local scalars
   Integer :: mseq ! - time-sequencing -
!
   Type(pprd) :: mprd1 ! - period of first data -
   Type(pprd) :: mprdn ! - period of last data -
!
! Executable Statements
!
! Set monthly input file settings
   If (icsv == 0) Then
      mfile = yfile
      mfile%cfile%desc = 'M Input File'
  Else
! - backup current settings -
      If (lnewy) Then
         mfile = yfile
      Else
         mseq = mfile%iseq
         mprd1 = mfile%prd1
         mprdn = mfile%prdn
         mfile = yfile
! - restore previous settings -
         mfile%iseq = mseq
         mfile%prd1 = mprd1
         mfile%prdn = mprdn
         mfile%cprd1 = get_cdate(mfile%prd1, 1, io_com)
         mfile%cprdn = get_cdate(mfile%prdn, 1, io_com)
      End If
   End If
!
! Update settings
   Call window_update (mfile%cprd1)
   Call window_update (mfile%cprdn)
!
   Return
 End Subroutine set_mfile
!
!
!
 Function set_pfile(nf, lsn)
!
! Sets persistence file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  4 Insufficient persistence data for number of forecasts
!    ifail =  5 Persistence data unavailable for first forecasts
!
! Modules
   Use data_time_constants, Only: isq_mn
   Use time,                Only: Operator(+), Operator(>), Operator(<), &
                                  iseq, &
                                  date_diff, reset_iseq
!
! Function result
   Integer :: set_pfile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nf  ! - number of forecasts -
   Integer, Intent(In) :: lsn ! - length of season -
!
! Executable Statments
!
! Set persistence file
   pfile = mfile
   Select Case (iseq)
    Case (:-1)
      Call reset_iseq (isq_new=isq_mn)
    Case Default
      Call reset_iseq (isq_new=iseq)
   End Select
   pfile%bdate = zfile%fdate + date_diff(xfile%bdate, yfile%bdate, iseq)
   pfile%fdate = pfile%bdate + (lsn - 1)
   Call reset_iseq ()
   If (pfile%fdate + (nf-1) > pfile%prdn%sdate) Then
      set_pfile = 4
      Return
   End If
   Select Case (iseq)
    Case (:-1)
      Call reset_iseq (isq_new=isq_mn)
    Case Default
      Call reset_iseq (isq_new=iseq)
   End Select
   pfile%fdate = pfile%fdate + 1
   Call reset_iseq ()
   If (pfile%bdate < pfile%prd1%sdate) Then
     set_pfile = 5
     Return
   End If
!
   set_pfile = 0
!
   Return
 End Function set_pfile
!
!
!
 Subroutine max_width (iin, lmax, ifail, &
            afile, idat, cline)
!
! Determines maximum width of file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 File is too wide
!
! Modules
   Use data_io_constants, Only: lfli
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin  ! - input unit number -
!
! - output scalars -
   Integer, Intent(Out) :: lmax  ! - maximum length of line -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: idat ! - line mumber to return -
!
   Character(Len=*), Intent(In), Optional :: afile ! - file name -
!
! - optional output scalars -
   Character(Len=*), Intent(Out), Optional :: cline ! - last line of input -
!
! Locals
!
! Local scalars
   Integer :: i    ! - line counter -
   Integer :: l    ! - line number -
   Integer :: emax ! - estimated maximum length of line -
   Integer :: llen ! - current length of line -
!
   Character(Len=lfli) :: cl ! - line of input -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Min, Present
!
! Executable Statements
!
! Open file
   If (Present(afile)) Then
      Call open_iofile (iin, afile, .true., .true., .true., ifail)
      If (ifail /= 0) GoTo 2
   End If
!
! Determine maximum length of line of data
   lmax = 0
   emax = 100
   i = 1
   If (Present(idat)) Then
      l = idat
   Else
      l = 2
   End If
   cl = ' '
   Do
      Read (Unit=iin, Fmt='(A)', Err=2, End=1) cl(1:emax)
      llen = Len_Trim(cl(1:emax))
      If (llen <= lmax) Then ! - current line is shorter - 
         If (i == l) Then
            emax = Min(lmax+100, lfli)
            If (Present(cline)) cline = cl ! - return first line -
         End If
         i = i + 1
     Else ! - extend if line is too short -
         lmax = llen
         If (lmax >= lfli - 100) Then ! - return if line is too long -
            ifail = 4
            Return
         End If
         emax = Min(lmax+100, lfli)
         If (emax < 2**13) Then
            Backspace (Unit=iin)
         Else
            Rewind (Unit=iin)
         End If
      End If
   End Do
!
! Close file
1  If (Present(afile)) Close (Unit=iin)
   ifail = 0
   Return
!
! Errors
2 ifail = 1
   Return
 End Subroutine max_width
!
!
!
 Subroutine get_old_file (ftype, ftypes, ffilts, nfilts, fdir, ffile, fname, ifail, &
            fext)
!
! Gets an existing file
!
! Modules
#if GUI == 1
   Use clrwin$, Only: clearwin_option$
   Use errors,  Only: cpt_error
#endif
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfilts ! - number of file filters -
!
   Character(Len=*), Intent(In) :: ftype ! - file type -
!
! - input arrays -
   Character(Len=*), Dimension(:), Intent(In) :: ftypes ! - file types -
   Character(Len=*), Dimension(:), Intent(In) :: ffilts ! - file filters -
!
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: fdir ! - file directory -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: ffile ! - selected file -
   Character(Len=*), Intent(Out) :: fname ! - selected file name -
!
! Optional arguments
! - optional input/output scalars -
   Character(Len=*), Intent(InOut), Optional :: fext ! - selected file extension -
!
#if GUI == 0
! Locals
!
! Local scalars
   Logical :: le ! - file exists? -
!
#else
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim
!
! ClearWin+ routines
  External :: get_filtered_file$
!
#endif
! Executable Statements
!
! Default to latest directory if directory is unset
!
! Create window
   ffile = ' '
#if GUI == 1
   Call clearwin_option$ ('alt_open_save')
   If (Len_Trim(fdir) == 0 .and. Len_Trim(lidir) > 0) Then
      Call get_filtered_file$ ('Open '//ftype, ffile, lidir, ftypes, ffilts, nfilts, 1)
   Else
      Call get_filtered_file$ ('Open '//ftype, ffile, fdir, ftypes, ffilts, nfilts, 1)
   End If
#else
   Call get_filtered_file (ftype, 1, ffile)
#endif
!
#if GUI == 1
! Check for % sign in file name
   If (Index(ffile, '%') /= 0) Then
      ifail = 1
      Call cpt_error ('get_old_file', .false., ifail)
      ifail = 1
      Return
   End If
!
#else
! Check whether file exists
   Inquire (File=ffile, Exist=le, IOstat=ifail)
   If (ifail == 0) Then
      If (.not.le) Then
         ifail = 1
         Return
      End If
   Else
      ifail = 1
      Return
   End If
!
#endif
! Isolate file name
   Call parse_file (ffile, fdir, fname, ifail, &
        fext_new=fext)
   If (ifail == 0) lidir = fdir
!
   Return
 End Subroutine get_old_file
!
!
!
 Subroutine get_new_file (ftype, lnewname, fext, ftypes, ffilts, nfilts, fdir, ffile, fname, ifail)
!
! Prompts for a new file name
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfilts ! - number of file filters -
!
   Character(Len=*), Intent(In) :: ftype ! - file type -
   Character(Len=*), Intent(In) :: fext  ! - file extension -
!
   Logical, Intent(In) :: lnewname ! - new filename? -
!
! - input arrays -
   Character(Len=*), Dimension(:), Intent(In) :: ftypes ! - file types -
   Character(Len=*), Dimension(:), Intent(In) :: ffilts ! - file filters -
!
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: fdir ! - file directory -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: ffile ! - selected file -
   Character(Len=*), Intent(Out) :: fname ! - selected file name -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len, Len_Trim, Repeat
!
#if GUI == 1
! ClearWin+ routines
  External :: get_filtered_file$
!
#endif
! Executable Statements
!
! Select file window
   Do
      If (lnewname) ffile = Repeat(' ',Len(ffile))
#if GUI == 1
      If (Len_Trim(fdir)==0 .and. Len_Trim(lodir) > 0) Then
         Call get_filtered_file$ ('Define '//ftype, ffile, lodir, ftypes, ffilts, nfilts, 0)
      Else
         Call get_filtered_file$ ('Define '//ftype, ffile, fdir, ftypes, ffilts, nfilts, 0)
      End If
#else
      Call get_filtered_file (ftype, 0, ffile)
#endif
!
! Isolate file name
      If (Index(ffile, ffilts(1)(2:Len_Trim(ffilts(1))), Back=.true.) > 0) Then
         Call parse_file (ffile, fdir, fname, ifail, &
              fext_old=ffilts(1)(2:Len_Trim(ffilts(1))))
     Else
         Call parse_file (ffile, fdir, fname, ifail, &
              fext_old=fext)
      End If
      If (ifail == 1) Exit
      lodir = fdir
      ifail = check_new_file(ffile)
      If (ifail == 0) Exit
   End Do
!
   Return
 End Subroutine get_new_file
!
!
!
 Function check_new_file(ffile) &
          Result (check)
!
! Returns 0 if filename is OK to use.
!
! Modules
#if GUI == 1
   Use errors, Only: query
#else
   Use gui,    Only: print_warning
#endif
   Use errors, Only: cpt_error
!
! Function result
   Integer :: check
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ffile ! - file -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Logical :: le ! - file exists? -
!
! Executable Statements
!
! Check file name
! - check for existence of file -
   Inquire (File=ffile, Exist=le, IOstat=ifail)
   If (ifail == 0) Then
      If (le) Then
#if GUI == 1
         check = 1 - query('check_new_file', 1, .false.)
      Else
         check = 0
      End If
#else
         Call print_warning (.true., 'File already exists. Overwriting old file', &
              nopause=.true.)
      End If
      check = 0
#endif
!
! Invalid file name
  Else
      ifail = 1
      Call cpt_error ('check_new_file', .false., ifail)
      check = 1
   End If
!
   Return
 End Function check_new_file
!
!
!
 Subroutine parse_file (ffile, fdir, fname, ifail, &
            fext_old, fext_new)
!
! Parses a file into its directory, name, and extension
!
! Modules
   Use data_io_constants, Only: cdir
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: ffile ! - selected file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
   Character(Len=*), Intent(Out) :: fdir  ! - file directory -
   Character(Len=*), Intent(Out) :: fname ! - selected file name -
!

! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: fext_old ! - file extension -
!
! - optional output scalars -
   Character(Len=*), Intent(Out), Optional :: fext_new ! - file extension -
!
! Locals
!
! Local scalars
   Integer :: i1, i2, i3, i4 ! - position of filename -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Max, Present, Scan, Trim
!
! Executable Statements
!
! Isolate file name
   i3 = Len_Trim(ffile)
   If (i3 > 0) Then
! - identify directory -
      i1 = Scan(ffile, '/\', Back=.true.)
      If (i1 > 0) Then
         fdir = ffile(1:i1)
      Else
         fdir = '.'//cdir
      End If
! - ientify any file extension -
      If (.not.present(fext_old)) Then
         i2 = Index(ffile, '.', Back=.true.)
         If (i2 > i1) Then
            If (Present(fext_new)) Then
               fname = ffile(i1+1:i2-1)
               fext_new = ffile(i2+1:i3)
            Else
               fname = ffile(i1+1:i3)
            End If
         Else
            fname = ffile(i1+1:i3)
            If (Present(fext_new)) fext = ' '
         End If
      Else
! - remove file extension if present -
         i4 = Max(Index(fext_old, '.', Back=.true.), 1)
         i2 = Index(ffile(i1+1:i3), fext_old(i4:), Back=.true.)
         If (i2 > 0) Then
            i2 = Index(ffile(1:i3), fext_old(i4:), Back=.true.)
            i3 = i2 - 1
         Else
            i2 = Index(fext_old, '.', Back=.true.)
            If (i2 > 0) Then
               ffile = Trim(ffile)//fext_old(i2:)
            Else
               ffile = Trim(ffile)
            End If
         End If
         fname = ffile(i1+1:i3)
      End If
      ifail = 0
   Else
      ifail = 1
   End If
!
   Return
 End Subroutine parse_file
!
!
!
 Subroutine prompt_results_file (lfirst, rfile)
!
! Prompts for a results file
!
! Modules
   Use gui, Only: gui_creturn, prompt_file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lfirst ! - first file? -
!
! - input/output scalars -
   Type(ofile), Intent(InOut) :: rfile ! - results file -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for results file
   If (.not.lfirst) Call gui_creturn (.false.)
   Call prompt_file (geto_file, Trim(rfile%cfile%desc), rfile%nfile, rfile%cfile%fname, nfile, &
        itab=45)
!
   Return
 End Subroutine prompt_results_file
!
!
!
 Function geto_file()
!
! Prompts for output-file name corresponding to output-file-type number nfile
!
! Function result
   Integer :: geto_file
!
! Executable Statements
!
! Define output file
   If (nfile == xdfile%nfile) Then      ! - X input data -
      Call geto_gen (xdfile)
   Else If (nfile == xifile%nfile) Then ! - interpolated X data -
      Call geto_gen (xifile)
   Else If (nfile == ydfile%nfile) Then ! - Y input data -
      Call geto_gen (ydfile)
   Else If (nfile == yofile%nfile) Then ! - Y input data observed component -
      Call geto_gen (yofile)
   Else If (nfile == ypfile%nfile) Then ! - Y input data persisted + predicted component -
      Call geto_gen (ypfile)
   Else If (nfile == tyfile%nfile) Then ! - transformed Y input data -
      Call geto_gen (tyfile)
   Else If (nfile == gtfile%nfile) Then ! - gamma transformation parameters output file -
      Call geto_gen (gtfile)
   Else If (nfile == ycfile%nfile) Then ! - Y categories -
      Call geto_gen (ycfile)
   Else If (nfile == yhfile%nfile) Then ! - cross-validated predictions -
      Call geto_gen (yhfile)
   Else If (nfile == yffile%nfile) Then ! - fitted values -
      Call geto_gen (yffile)
   Else If (nfile == rcfile%nfile) Then ! - retroactive categories -
      Call geto_gen (rcfile)
   Else If (nfile == yrfile%nfile) Then ! - retroactive predictions -
      Call geto_gen (yrfile)
   Else If (nfile == erfile%nfile) Then ! - retroactive ensemble predictions -
      Call geto_gen (erfile)
   Else If (nfile == prfile%nfile) Then ! - retroactive forecast probabilities -
      Call geto_gen (prfile)
   Else If (nfile == lrfile%nfile) Then ! - retroactive prediction limits -
      Call geto_gen (lrfile)
   Else If (nfile == orfile%nfile) Then ! - retroactive forecast odds -
      Call geto_gen (orfile)
   Else If (nfile == vrfile%nfile) Then ! - retroactive prediction error variances -
      Call geto_gen (vrfile)
   Else If (nfile == y2file%nfile) Then ! - double cross-validated predictions -
      Call geto_gen (y2file)
   Else If (nfile == e2file%nfile) Then ! - double cross-validated forecast ensembles -
      Call geto_gen (e2file)
   Else If (nfile == p2file%nfile) Then ! - double cross-validated forecast probabilities -
      Call geto_gen (p2file)
   Else If (nfile == l2file%nfile) Then ! - double cross-validated prediction limits -
      Call geto_gen (l2file)
   Else If (nfile == o2file%nfile) Then ! - double cross-validated forecast odds -
      Call geto_gen (o2file)
   Else If (nfile == v2file%nfile) Then ! - double cross-validated prediction error variances -
      Call geto_gen (v2file)
   Else If (nfile == xefile%nfile) Then ! - X eigenvalues -
      Call geto_gen (xefile)
   Else If (nfile == xlfile%nfile) Then ! - X spatial loadings -
      Call geto_gen (xlfile)
   Else If (nfile == xsfile%nfile) Then ! - X temporal scores -
      Call geto_gen (xsfile)
   Else If (nfile == yefile%nfile) Then ! - Y eigenvalues -
      Call geto_gen (yefile)
   Else If (nfile == ylfile%nfile) Then ! - Y spatial loadings -
      Call geto_gen (ylfile)
   Else If (nfile == ysfile%nfile) Then ! - Y temporal scores -
      Call geto_gen (ysfile)
   Else If (nfile == ccfile%nfile) Then ! - canonical correlations -
      Call geto_gen (ccfile)
   Else If (nfile == xmfile%nfile) Then ! - X homogeneous covariance maps -
      Call geto_gen (xmfile)
   Else If (nfile == xtfile%nfile) Then ! - X homogeneous covariance maps time series -
      Call geto_gen (xtfile)
   Else If (nfile == ymfile%nfile) Then ! - Y homogeneous covariance maps -
      Call geto_gen (ymfile)
   Else If (nfile == ytfile%nfile) Then ! - Y homogeneous covariance maps time series -
      Call geto_gen (ytfile)
   Else If (nfile == rbfile%nfile) Then ! - regression coefficients -
      Call geto_gen (rbfile)
   Else If (nfile == pbfile%nfile) Then ! - PC regression coefficients -
      Call geto_gen (pbfile)
   Else If (nfile == fpfile%nfile) Then ! - forecast probabilities -
      Call geto_gen (fpfile)
   Else If (nfile == fofile%nfile) Then ! - forecast odds -
      Call geto_gen (fofile)
   Else If (nfile == fvfile%nfile) Then ! - forecasts -
      Call geto_gen (fvfile)
   Else If (nfile == ocfile%nfile) Then ! - observed component of forecasts -
      Call geto_gen (ocfile)
   Else If (nfile == pcfile%nfile) Then ! - predicted component of forecasts -
      Call geto_gen (pcfile)
   Else If (nfile == fsfile%nfile) Then ! - forecast ensembles -
      Call geto_gen (fsfile)
   Else If (nfile == fefile%nfile) Then ! - prediction-error variances -
      Call geto_gen (fefile)
   Else If (nfile == lpfile%nfile) Then ! - prediction limits -
      Call geto_gen (lpfile)
   Else If (nfile == exfile%nfile) Then ! - exceedence probabilities -
      Call geto_gen (exfile)
   Else If (nfile == zifile%nfile) Then ! - interpolated Z data -
      Call geto_gen (zifile)
   Else If (nfile == fxfile%nfile) Then ! - predictor time scores -
      Call geto_gen (fxfile)
   Else If (nfile == fcfile%nfile) Then ! - predictor CCA time scores -
      Call geto_gen (fcfile)
   Else If (nfile == skfile%nfile) Then ! - skill -
      Call geto_gen (skfile)
   Else If (nfile == pvfile%nfile) Then ! - p-values -
      Call geto_gen (pvfile)
   Else If (nfile == rofile%nfile) Then ! - ROC -
      Call geto_gen (rofile)
   Else If (nfile == rrfile%nfile) Then ! - ROC -
      Call geto_gen (rrfile)
   Else If (nfile == atfile%nfile) Then ! - attributes diagram -
      Call geto_gen (atfile)
   Else If (nfile == psfile%nfile) Then ! - probabilistic scores -
      Call geto_gen (psfile)
   Else If (nfile == rhfile%nfile) Then ! - ranked hits -
      Call geto_gen (rhfile)
   Else If (nfile == wrfile%nfile) Then ! - weather roulette -
      Call geto_gen (wrfile)
   Else If (nfile == gifile%nfile) Then ! - goodness index -
      Call geto_gen (gifile)
   Else If (nfile == thfile%nfile) Then ! - thresholds -
      Call geto_gen (thfile)
   Else If (nfile == avfile%nfile) Then ! - averages -
      Call geto_gen (avfile)
   Else If (nfile == sdfile%nfile) Then ! - standard deviations -
      Call geto_gen (sdfile)
   Else If (nfile == cvfile%nfile) Then ! - coefficients of variation -
      Call geto_gen (cvfile)
   Else If (nfile == cofile%nfile) Then ! - correlations -
      Call geto_gen (cofile)
   Else If (nfile == vxfile%nfile) Then ! - standardized X data -
      Call geto_gen (vxfile)
   Else If (nfile == vyfile%nfile) Then ! - standardized Y data -
      Call geto_gen (vyfile)
   Else If (nfile == vzfile%nfile) Then ! - standardized Z data -
      Call geto_gen (vzfile)
   Else If (nfile == mxfile%nfile) Then ! - merged input data -
      Call geto_gen (mxfile)
   End If
!
   geto_file=2
!
   Return
 End Function geto_file
!
!
!
#if GUI == 0
 Subroutine get_filtered_file (title, oldnew, ffile)
!
! Prompts for a file name, and checks whether it exists (corresponds to FTN95 file extension)
!
! Modules
   Use gui, Only: iprompt, &
                  print_prompt
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: oldnew ! - old file / new file flag -
! 
   Character(Len=*), Intent(In) :: title ! - file title -
! 
! - output scalars -
   Character(Len=*), Intent(Out) :: ffile ! - file -
!
! Locals
!
! Local scalars
   Logical :: le ! - file exists? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for file
1  Call print_prompt (title)
   Read (Unit=*, Fmt='(A)', Err=2, End=2) ffile
!
! Check for existence
   If (oldnew == 1) Then
      Inquire (File=Trim(ffile), Exist=le)
      If (.not.le) GoTo 2
   End If
!
   Return
!
2  Select Case (iprompt)
    Case (0)
      GoTo 1
    Case (1)
      Continue
    Case (2)
      Stop
   End Select
!
   Return
 End Subroutine get_filtered_file
!
!
!
#endif
 Subroutine geto_gen (afile)
!
! Prompts for an output file
!
! Modules
   Use data_io_constants, Only: ca_ffmts
   Use screen,            Only: window_update
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(ofile), Intent(InOut) :: afile ! - file -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Character(Len=24) :: cfiles ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Define output file
   Call set_fmt (0, afile%cfile%ffmt)
   Select Case (Trim(ca_ffmts(afile%cfile%ffmt%ifmt)))
    Case ('formatted')
      afile%cfile%fext = '.txt'
      cfiles='Formatted output files'
    Case ('unformatted')
      afile%cfile%fext = '.dat'
      cfiles='Unformatted output files'
    Case ('GrADS')
      afile%cfile%fext = '.dat'
      cfiles='GrADS output files'
   End Select
   Call get_new_file (Trim(afile%cfile%desc)//' Output file', .false., afile%cfile%fext, [cfiles(:)], ['*'//afile%cfile%fext], &
        1, afile%cfile%fdir, afile%cfile%ffile, afile%cfile%fname, ifail)
!
! Confirm selection
   If (ifail == 0) Then
      afile%cfile%lset = .true.
      Call window_update (afile%cfile%fname)
   Else
      Call init_file (afile%cfile)
   End If
!
   Return
 End Subroutine geto_gen
!
!
!
#if GUI == 1
 Function get_fmt()
!
! Sets file format options
!
! Modules
   Use data_io_constants, Only: ca_del, ca_faccs, ca_ffmts, ca_prcs, nacc, ndel, nfmt, nprc
   Use gui,               Only: gui_creturn, init_win, print_text, prompt_integer, prompt_list, win_prompt
!
! Function result
   Integer :: get_fmt
!
! Locals
!
! Local scalars
   Type(fformat) :: tformat ! - file format -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Backup settings
   tformat = cformat
!
! Set gray flags
   If (Trim(ca_ffmts(cformat%ifmt)) == 'GrADS') cformat%iprc = 1
1  ipg = 0
!
! Create window
   Call init_win (title='File format')
!
! Indicate and prompt for file options
   Call gui_creturn (.true.)
   Call print_text ('The following format options will be used to write the data:')
   Call gui_creturn (.true.)
   Call prompt_list ('FORMAT', .true., ca_ffmts, nfmt, cformat%ifmt, &
        iwdth=13, itab=15, cbf1=cf_ifmt)
   Call gui_creturn (.true.)
   Call prompt_list ('ACCESS', .true., ca_faccs, nacc, cformat%iacc, &
        iwdth=13, itab=0)
   Call gui_creturn (.true.)
   Select Case (cformat%ifmt)
    Case Default
      Call prompt_integer ('DECIMAL PLACES', ipreco, &
           itab=15, ilow=-1, ihgh=9, lbold=.true.)
    Case (1, 3)
      Call prompt_list ('DATA PRECISION', .true., ca_prcs, nprc, cformat%iprc, &
           iwdth=13, itab=0)
   End Select
   Call gui_creturn (.true.)
   Call print_text ('The following format character will be used to delimit items in formatted output files:')
   Call gui_creturn (.true.)
   Call prompt_list ('DELIMITER', .true., ca_del, ndel, icd, &
        iwdth=13, itab=0, cbf1=cf_delim)
   get_fmt = win_prompt (.true.)
!
! Update settings
   If (ipg == 1) GoTo 1
   If (get_fmt /= 1) cformat = tformat
   get_fmt = 2
!
   Return
 End Function get_fmt
!
!
!
 Function cf_ifmt()
!
! Sets file format identifier and corresponding precision, if appropriate
!
! Modules
   Use data_io_constants, Only: ca_ffmts
!
! Function result
   Integer :: cf_ifmt
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set gray data precision flag
   If (Trim(ca_ffmts(cformat%ifmt)) == 'formatted') Then
      cformat%iprc = 2
   Else If (Trim(ca_ffmts(cformat%ifmt)) == 'GrADS') Then
      cformat%iprc = 1
   End If
   ipg = 1
   cf_ifmt = 0
!
   Return
 End Function cf_ifmt
!
!
!
#else
 Function output_format()
!
! Prompts for output format
!
! Modules
   Use data_io_constants, Only: ca_ffmts, nfmt
   Use gui,               Only: ganged_rbs, print_text
!
! Function result
   Integer :: output_format
!
! Executable Statements
!
! Get file format
   Call print_text ('Output format:')
   Call ganged_rbs (nfmt, ca_ffmts(:), cformat%ifmt)
!
! Force single precision for GrADS output
   If (cformat%ifmt == 3) cformat%iprc = 1
   output_format = 1
!
   Return
 End Function output_format
!
!
!
 Function output_precision()
!
! Prompts for output precision
!
! Modules
   Use data_io_constants, Only: ca_prcs, nprc
   Use gui,               Only: ganged_rbs, print_text, prompt_integer
!
! Function result
   Integer :: output_precision
!
! Executable Statements
!
! Get file precision
  Select Case (cformat%ifmt)
   Case Default
      Call prompt_integer ('Number of decimal places', ipreco, &
           ilow=0, ihgh=9)
   Case (3)
      Call print_text ('Output data precision:')
      Call ganged_rbs (nprc, ca_prcs(:), cformat%iprc)
   End Select
   output_precision = 1
!
   Return
 End Function output_precision
!
!
!
#endif
 Subroutine set_fmt (nrec, ffmt)
!
! Sets file format
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nrec ! - number of records in one line -
!
! - input/output scalars -
   Type(fformat), Intent(InOut) :: ffmt ! - file format -
!
! Executable Statements
!
! Apply file format settings
   ffmt%ifmt = cformat%ifmt
   ffmt%iacc = cformat%iacc
   ffmt%iprc = cformat%iprc
   ffmt%lrec = nrec
!
   Return
 End Subroutine set_fmt
!
!
!
 Subroutine set_grey_files (ixdstr, iydstr, igeog)
!
! Sets which menu items to display or grey
!
! Modules
   Use menus, Only: set_menu_greying
   Use space, Only: get_geog
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ixdstr ! - X file structure -
   Integer, Intent(In) :: iydstr ! - Y file structure -
!
! - output scalars -
   Integer, Intent(Out) :: igeog ! - spatial domain flag -
!
! Locals
!
! Local scalars
   Logical :: lopen ! - file open flag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Min
!
! Executable Statements
!
! Set file open and edit menu flag
   lopen = files_open(ixdstr, iydstr)
   igeog = Min(1, get_geog(ixdstr) + get_geog(iydstr))
   Call set_menu_greying (lopen=lopen, lxyopen=(ixdstr > 0 .and. iydstr > 0), igeog=igeog)
!
   Return
 End Subroutine set_grey_files
!
!
!
 Function files_open(ixdstr, iydstr) &
          Result (lopen)
!
! Determines whether any input files are open
!
! Modules
   Use menus, Only: set_menu_greying
!
! Function result
   Logical :: lopen
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ixdstr ! - X file structure -
   Integer, Intent(In) :: iydstr ! - Y file structure -
!
! Executable Statements
!
! Determine whether any input files are open
   lopen = (ixdstr + iydstr > 0)
   Call set_menu_greying (lnofiles=.not.lopen)
!
   Return
 End Function files_open
!
!
!
 Subroutine proj_read_iofiles (iin, pver, iafter, iold, xfd_old, yfd_old, ifail)
!
! Reads input file settings from project file
!
! Modules
   Use data_numbers, Only: sp, tolv
   Use time,         Only: iseq
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
!
   Real(Kind=sp), Intent(In) :: pver ! - project file version -
!
! - output scalars -
   Integer, Intent(Out) :: iafter  ! - A's date should be before (-1) equal to (0) or after (1) B's date? -
   Integer, Intent(Out) :: iold    ! - old monthly input file? -
   Integer, Intent(Out) :: xfd_old ! - backup X first year of interest -
   Integer, Intent(Out) :: yfd_old ! - backup Y first year of interest -
   Integer, Intent(Out) :: ifail   ! - error indicator -
!
! Locals
!
! Local scalars
   Logical :: lex ! - X-file exists -
   Logical :: ley ! - Y-file exists -
   Logical :: lez ! - Z-file exists -
!
! Executable Statements
!
! Read input file settings
   iold = 0
   iafter = 0
   xfd_old = 0
   yfd_old = 0
   If (pver > 16.03_sp-tolv) Then
      Read (Unit=iin, Err=1, End=2) xfile
      Read (Unit=iin, Err=1, End=2) yfile
      Read (Unit=iin, Err=1, End=2) zfile
      Read (Unit=iin, Err=1, End=2) mfile
      Read (Unit=iin, Err=1, End=2) iafter, icd, itags, mxa, mya, mza, nx, ny, nz
   Else If (pver > 16.00_sp) Then
      Read (Unit=iin, Err=1, End=2) xfile
      Read (Unit=iin, Err=1, End=2) yfile
      Read (Unit=iin, Err=1, End=2) zfile
      Read (Unit=iin, Err=1, End=2) mfile
      Read (Unit=iin, Err=1, End=2) icd, itags, mxa, mya, mza, nx, ny, nz
   End If
   If (xfile%nem == 1) xfile%nem = 0
   If (yfile%nem == 1) yfile%nem = 0
   If (zfile%nem == 1) zfile%nem = 0
   If (yfile%cfile%lset) iseq = yfile%iseq
!
! Check that input files still exist
   If (xfile%cfile%lset) Then
      Inquire (File=xfile%cfile%ffile, Exist=lex)
   Else
      lex = .false.
   End If
   If (yfile%cfile%lset) Then
      Inquire (File=yfile%cfile%ffile, Exist=ley)
   Else
      ley = .false.
   End If
   If (zfile%cfile%lset) Then
      Inquire (File=zfile%cfile%ffile, Exist=lez)
   Else
      lez = .false.
   End If
!
! Reset maximum file widths
! - X file -
   Call reset_max_width (lex, xfile, ifail)
! - Y file -
   If (ifail == 0) Call reset_max_width (ley, yfile, ifail)
! - Z file -
   If (ifail == 0) Call reset_max_width (lez, zfile, ifail)
! - check for errors -
   Select Case (ifail)
    Case (0)
      nz = nx
    Case (1)
      GoTo 3
    Case Default
      GoTo 5
   End Select
   If (zfile%lmax == 0) nz = 0
!
! Set backups
   xfd_old = xfile%fdate%iyr
   yfd_old = yfile%fdate%iyr
!
! Check that files still exist
   If (xfile%cfile%lset .and. .not.lex) GoTo 6
   If (yfile%cfile%lset .and. .not.ley) GoTo 7
   zfile%cfile%lset = lez
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
! - problem determining file width -
3  ifail = 3
   Return
!
! - problem opening file -
5  ifail = 5
   Return
!
! - X-file does not exist -
6  ifail = 6
   Return
!
! - Y-file does not exist -
7  ifail = 7
   Return
!
 Contains
!
!
  Subroutine reset_max_width (le, afile, ifail)
!
! Resets maximum file width
!
! Modules
   Use data_cpt_constants, Only: id_grd
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: le ! - file exists? -
!
! - input/output scalars -
   Type(ifile), Intent(InOut) :: afile ! - file -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Reset maximum file width
   If (le .and. afile%cfile%lset .and. afile%idstr /= id_grd) Then
      Call max_width (iin+1, afile%lmax, ifail, &
           afile=afile%cfile%ffile)
   Else
      afile%lmax = 0
      ifail = 0
   End If
!
   Return
  End Subroutine reset_max_width
 End Subroutine proj_read_iofiles
!
!
!
 Subroutine proj_write_iofiles (iout, iafter, ifail)
!
! Writes input file settings to project file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iafter ! - A's date should be before (-1) equal to (0) or after (1) B's date? -
   Integer, Intent(In) :: iout   ! - output-file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write input file settings
   Write (Unit=iout, Err=1) xfile
   Write (Unit=iout, Err=1) yfile
   Write (Unit=iout, Err=1) zfile
   Write (Unit=iout, Err=1) mfile
   Write (Unit=iout, Err=1) iafter, icd, itags, mxa, mya, mza, nx, ny, nz
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_iofiles
!
!
!
 Subroutine parameter_write_iofiles (iout, xlim, ylim, imn1, lsn, ispi, ifail, &
            lxmodes, nxe, mxe, lymodes, nye, mye, lcmodes, ncc, mcc)
!
! Writes input file settings to parameter file
!
! Modules
   Use data_cpt_constants,  Only: ifile_x, ifile_y, ifile_z
   Use data_time_constants, Only: isq_mn
   Use labels,              Only: l_predictandy, l_predictorx, l_predictorz
   Use menus,               Only: mn_edit_startatx, mn_edit_startaty, mn_edit_startatz
   Use space,               Only: area
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output-file unit number -
   Integer, Intent(In) :: imn1 ! - first month of season to forecast -
   Integer, Intent(In) :: ispi ! - length of SPI -
   Integer, Intent(In) :: lsn  ! - length of season to forecast -
!
! - input arrays -
   Type(area), Dimension(:), Intent(In) :: xlim ! - limits of X domains -
   Type(area), Dimension(:), Intent(In) :: ylim ! - limits of Y domains -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nxe   ! - minimum number of X modes -
   Integer, Intent(In), Optional :: mxe   ! - maximum number of X modes -
   Integer, Intent(In), Optional :: nye   ! - minimum number of X modes -
   Integer, Intent(In), Optional :: mye   ! - maximum number of X modes -
   Integer, Intent(In), Optional :: ncc   ! - minimum number of CCA modes -
   Integer, Intent(In), Optional :: mcc   ! - maximum number of CCA modes -
!
   Logical, Intent(In), Optional :: lxmodes ! - prompt for X EOF modes? -
   Logical, Intent(In), Optional :: lymodes ! - prompt for Y EOF modes? -
   Logical, Intent(In), Optional :: lcmodes ! - prompt for CCA modes? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Write input file settings
! - X-file -
   ifail = 0
   If (xfile%cfile%lset) Then
      Call parameter_write_iofile (iout, 'X', Trim(l_predictorx%c), ifile_x, mn_edit_startatx, xfile, ifail, &
           alim=xlim(:), lemodes=lxmodes, ne=nxe, me=mxe)
      If (ifail /= 0) Return
   End If
! - Y-file -
   If (yfile%cfile%lset) Then
      If (mfile%iseq == isq_mn) Then
         Call parameter_write_iofile (iout, 'Y', Trim(l_predictandy%c), ifile_y, mn_edit_startaty, yfile, ifail, &
              alim=ylim(:), imn1=imn1, lsn=lsn, ispi=ispi, lemodes=lymodes, ne=nye, me=mye, lcmodes=lcmodes, ncc=ncc, mcc=mcc)
      Else
         Call parameter_write_iofile (iout, 'Y', Trim(l_predictandy%c), ifile_y, mn_edit_startaty, yfile, ifail, &
              alim=ylim(:), lemodes=lymodes, ne=nye, me=mye, lcmodes=lcmodes, ncc=ncc, mcc=mcc)
      End If
      If (ifail /= 0) Return
   End If
! - Z-file -
   If (zfile%cfile%ffile /= xfile%cfile%ffile) Then
      Call parameter_write_iofile (iout, 'Z', Trim(l_predictorz%c), ifile_z, mn_edit_startatz, zfile, ifail)
      If (ifail /= 0) Return
   End If
!
   Return
!
!
 Contains
!
  Subroutine parameter_write_iofile (iout, cxyz, cdesc, idfile, mn_startatxyz, afile, ifail, &
             alim, imn1, lsn, ispi, lemodes, ne, me, lcmodes, ncc, mcc)
!
! Writes input file settings to parameter file
!
! Modules
   Use data_cpt_constants,  Only: ia_cca
   Use data_time_constants, Only: isq_mn, isq_sn, isq_so
   Use labels,              Only: c_lab, &
                                  ca_atypes_a, &
                                  cg_latnmost, cg_latsmost, cg_lngwmost, cg_lngemost, cg_lenspi, cg_maxnof, cg_minnof, &
                                  l_modes, &
                                  make_label
   Use time,                Only: iseq
   Use menus,               Only: menu, &
                                  mn_edit_startat
   Use parameter_file,      Only: output_parameter
   Use space,               Only: area
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout   ! - output-file unit number -
   Integer, Intent(In) :: idfile ! - file identifier -
!
   Character, Intent(In) :: cxyz ! - file identifier -
!
   Character(Len=*), Intent(In) :: cdesc ! - file description -
!
   Type(ifile), Intent(In) :: afile ! - input file -
!
   Type(menu), Intent(In) :: mn_startatxyz ! - corresponding start-at menu -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars
   Integer, Intent(In), Optional :: imn1 ! - first month of season to forecast -
   Integer, Intent(In), Optional :: ispi ! - length of SPI -
   Integer, Intent(In), Optional :: lsn  ! - length of season to forecast -
   Integer, Intent(In), Optional :: ne   ! - minimum number of modes -
   Integer, Intent(In), Optional :: me   ! - maximum number of modes -
   Integer, Intent(In), Optional :: ncc  ! - minimum number of CCA modes -
   Integer, Intent(In), Optional :: mcc  ! - maximum number of CCA modes -
!
   Logical, Intent(In), Optional :: lemodes ! - prompt for EOF modes? -
   Logical, Intent(In), Optional :: lcmodes ! - prompt for CCA modes? -
!
! - optional input arrays -
   Type(area), Dimension(:), Intent(In), Optional :: alim ! - limits of domains -
!
! Locals
!
! Local scalars
   Integer :: l   ! - current field / lagged field -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Write input file settings
! - file name -
   If (output_parameter(iout, &
                        cdesc=cdesc, i=idfile) /= 0) GoTo 1
   If (output_parameter(iout, &
                        c=afile%cfile%ffile) /= 0) GoTo 1
! - season -
   If (Present(imn1) .and. Present(lsn) .and. Present(ispi)) Then
      If (output_parameter(iout, &
                           cdesc='First month of season to forecast', i=imn1) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc='Length of season to forecast', i=lsn) /= 0) GoTo 1
      If (output_parameter(iout, &
                           cdesc=Trim(cg_lenspi), i=ispi) /= 0) GoTo 1
   End If
! - domain -
   If (afile%igeog > 0 .and. Present(alim)) Then
      Do ifd = 1, afile%nfs
         Do ilf = 1, afile%nls
            l = (ifd - 1)*afile%nls + ilf
            If (output_parameter(iout, &
                                 cdesc=Trim(cg_latnmost), r=alim(l)%rltn) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 cdesc=Trim(cg_latsmost), r=alim(l)%rlts) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 cdesc=Trim(cg_lngwmost), r=alim(l)%rlgw) /= 0) GoTo 1
            If (output_parameter(iout, &
                                 cdesc=Trim(cg_lngemost), r=alim(l)%rlge) /= 0) GoTo 1
         End Do
      End Do
   End If
! - modes -
   If (Present(lemodes)) Then
      If (lemodes) Then
         c_lab = make_label('L', l_modes)
         If (output_parameter(iout, &
                              cdesc=Trim(cg_minnof)//' '//Trim(c_lab)//' ('//cxyz//')', i=ne) /= 0) GoTo 1
         If (output_parameter(iout, &
                              cdesc=Trim(cg_maxnof)//' '//Trim(c_lab)//' ('//cxyz//')', i=me) /= 0) GoTo 1
         If (Present(lcmodes)) Then
            If (lcmodes) Then
               If (output_parameter(iout, &
                                    cdesc=Trim(cg_minnof)//' '//Trim(c_lab)//' ('//ca_atypes_a(ia_cca)//')', i=ncc) /= 0) GoTo 1
               If (output_parameter(iout, &
                                    cdesc=Trim(cg_maxnof)//' '//Trim(c_lab)//' ('//ca_atypes_a(ia_cca)//')', i=mcc) /= 0) GoTo 1
            End If
         End If
      End If
   End If
!
! Set start-at dates if modified -
   If (mn_startatxyz%iact > 0) Then
      If (output_parameter(iout, &
                           cdesc=Trim(mn_edit_startat%l_lab%c)//' ('//cxyz//')', i=mn_startatxyz%id) /= 0) GoTo 1
      Select Case (iseq)
       Case (1:)
         If (output_parameter(iout, &
                              i=afile%fdate%idy) /= 0) GoTo 1
         If (output_parameter(iout, &
                              i=afile%fdate%imn) /= 0) GoTo 1
       Case (isq_mn, isq_sn, isq_so)
         If (output_parameter(iout, &
                              i=afile%fdate%imn) /= 0) GoTo 1
      End Select
      If (output_parameter(iout, &
                           i=afile%fdate%iyr) /= 0) GoTo 1
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
  End Subroutine parameter_write_iofile
 End Subroutine parameter_write_iofiles
End Module iofiles
