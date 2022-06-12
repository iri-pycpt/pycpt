! Author: Simon Mason
Module downloads
!
! Modules
   Use data_numbers, Only: rp
   Use space,        Only: area
   Use fields,       Only: field
!
! Implicit declarations
   Implicit None
!
! Declarations
!
! Accessibility
   Private
   Public :: download_data, get_subdomain, init_download
!
! Scalars
!
! Integer scalars
   Integer, Public :: iddt = 0    ! - download data-type index -
   Integer, Public :: igrib = 0   ! - download variable GRIB index -
   Integer, Public :: isource = 0 ! - download dataset identifier -
!
   Integer, Private :: imd0  ! - month of forecast initialization -
   Integer, Private :: imd1  ! - first month of data to download -
   Integer, Private :: imd2  ! - last month of data to download -
   Integer, Private :: inino ! - NINO index -
   Integer, Private :: iyd1  ! - first year of data to download -
   Integer, Private :: iyd2  ! - last year of data to download -
   Integer, Private :: lsd   ! - length of season to average -
   Integer, Private :: nld   ! - number of lags -
   Integer, Private :: nvar  ! - number of variables -
!
! Real scalars
   Real(Kind=rp), Private :: rlatnew ! - new latitudinal resolution -
   Real(Kind=rp), Private :: rlngnew ! - new longitudinal resolution -
!
! Logical scalars
   Integer, Private :: imonthly = 1 ! - monthly aggregation method -
!
   Logical, Public :: lsubdom ! - select sub-domain? -
   Logical, Public :: lregrid ! - re-grid? -
!
   Logical, Private :: ldaily ! - daily resolution? -
   Logical, Private :: ldset  ! - domain set? -
   Logical, Private :: lrg    ! - re-grid? -
!
! Character scalars
   Character(Len=  16), Private :: cg_data   ! - dataset -
   Character(Len=  16), Private :: csprev    ! - previous source -
   Character(Len=  64), Private :: url_dl    ! - IRI DL / portal home page -
   Character(Len=1024), Private :: url_dfile ! - download file URL -
!
! Derived-type scalars
   Type(area), Private :: rnewlim ! - new grid limits -
!
   Type(field), Dimension(:), Pointer, Private :: dfield => Null() ! - download field information -
!
Contains
!
!
 Subroutine init_download ()
!
! Initialises data download parameters
!
! Modules
   Use data_numbers, Only: one
   Use time,         Only: dnow
   Use fields,       Only: init_field
   Use missing,      Only: xmiss
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Initialise default dates
   iyd1 = 1
   iyd2 = dnow%iyr
   imd0 = 1
   imd1 = 1
   lsd = 1
   nld = 1
!
! Initialise indices
   inino = 3 ! - variable identifiers -
!
! Initialise source
   csprev = ' '
!
! Initialise data resolution
   rlatnew = one
   rlngnew = one
!
! Initialise field
   Call init_field (1, [xmiss], dfield, ifail)
!
! Initialise options
   lsubdom = .false.
   lregrid = .false.
!
   Return
 End Subroutine init_download
!
!
!
 Function download_data()
!
! Provides access to the IRI Data Library
!
! Modules
#if GUI == 0
   Use data_io_constants, Only: iout
#endif
   Use data_iridl,        Only: id_gcm, id_obs, id_rea, ig_enso, ig_gphs, ig_mslp, ig_ssts, ig_tave, ig_tmax, ig_tmin, ig_prcp, &
                                ig_uvwd, ig_uwnd, ig_vwnd, ig_wetd
   Use labels,            Only: cg_nino_t
   Use time,              Only: pdate
   Use gui,               Only: use_url
#if GUI == 0
#if PC == 1
   Use errors,            Only: cpt_error
#endif
   Use iofiles,           Only: ofile, &
                                get_new_file, open_iofile
#endif
!
! Function result
   Integer :: download_data
!
! Locals
!
! Local scalars
   Integer :: ivar    ! - variable indicator -
   Integer :: iymn    ! - first year available -
   Integer :: iymx    ! - last year available -
#if GUI == 0
   Integer :: ierr    ! - error indicator -
#endif
   Integer :: ifail   ! - error indicator -
   Integer :: ndats   ! - number of datasets -
   Integer :: nemh    ! - number of ensemble members in hindcasts -
   Integer :: nemf    ! - number of ensemble members in forecasts -
   Integer :: nplv    ! - number of pressure levels -
   Integer :: nsource ! - dataset index -
!
   Character(Len= 2) :: cpgrd   ! - pressure level grid code -
   Character(Len=64) :: url_src ! - source URL -
#if GUI == 0
#ifdef PC
   Character(Len=45) :: cmd_download = & ! - download command '
      'bitsadmin /transfer CPT_DL_Download /dynamic '
   Character(Len= 1) :: opts_download = & ! - download options '
      ' '
#else
   Character(Len= 5) :: cmd_download = & ! - download command '
      'wget '
   Character(Len= 4) :: opts_download = & ! - download options '
      ' -O '
#endif
#endif
!
#if GUI == 1
   Logical :: ltsv = .false. ! - point to .tsv file?
#else
   Logical :: ltsv = .true.  ! - point to .tsv file?
#endif
!
   Type(pdate) :: ds2 ! - first date of second dataset -
!
#if GUI == 0
   Type(ofile) :: rdfile ! - renamed download file -
!
#endif
! Local arrays
   Integer, Dimension(:), Allocatable :: ivars ! - variables -
   Integer, Dimension(:), Allocatable :: iplvs ! - pressure level selections -
!
   Character(Len=  64), Dimension(:), Allocatable :: ccvt  ! - unit conversions -
   Character(Len=  64), Dimension(:), Allocatable :: cplvs ! - pressure levels -
   Character(Len=  16), Dimension(:), Allocatable :: cvars ! - variables -
!
   Character(Len= 256), Dimension(:,:), Allocatable :: url_vars ! - variables URLs -
!
   Logical, Dimension(:), Allocatable :: llsum ! - sum rather than average? -
!
! Functions and Subroutines
!
! Intrinsic functions
#if GUI == 0
   Intrinsic Execute_Command_Line
#if PC == 1
   Intrinsic AdjustL
#endif
#endif
   Intrinsic Trim
!
! Executable Statements
!
! Read dataset configuration
   download_data = 2
   Call get_dataset (iddt, igrib, isource, ifail)
   If (ifail /= 0) GoTo 2
!
! Identify download file
   ivar = igrib
   Select Case (iddt)
! - observed data -
    Case (id_obs)
      Select Case (igrib)
       Case (ig_enso) ! - ENSO indices -
         Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .false., .false., ltsv, .false., url_vars, inino, ifail, &
              cbox=Trim(cg_nino_t), nvar=nvar, cvars=cvars)
       Case (ig_ssts, ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_wetd) ! - SSTs, T mean, T max, T min, precipitation, wet-days -
         Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .false., .false., ltsv, .true., url_vars, ivar, ifail, &
              llsum=llsum, dfield=dfield)
      End Select
! - reanalysis data -
    Case (id_rea)
      Select Case (igrib)
       Case (ig_mslp) ! - MSLP -
         Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .false., .false., ltsv, .true., url_vars, ivar, ifail, &
              dfield=dfield)
       Case (ig_uvwd) ! - U and V by pressure level -
         Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .false., .true., ltsv, .true., url_vars, ivar, ifail, &
              nplv=nplv, iplvs=iplvs, cplvs=cplvs, cpgrd=cpgrd, dfield=dfield)
       Case (ig_gphs, ig_uwnd, ig_vwnd) ! - other variables by pressure level -
         Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .false., .false., ltsv, .true., url_vars, ivar, ifail, &
              nplv=nplv, iplvs=iplvs, cplvs=cplvs, cpgrd=cpgrd, dfield=dfield)
      End Select
! - model output -
    Case (id_gcm)
      Select Case (ndats)
       Case (1) ! - models with a single dataset -
         Select Case (igrib)
          Case (ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_ssts) ! - surface and near-surface variables -
            Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .true., .true., ltsv, .true., url_vars, ivar, ifail, &
                 ccvt=ccvt, nemh=nemh, dfield=dfield)
          Case (ig_gphs, ig_uwnd, ig_vwnd) ! - other variables by pressure level -
            Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .true., .true., ltsv, .true., url_vars, ivar, ifail, &
                 ccvt=ccvt, nemh=nemh, nplv=nplv, iplvs=iplvs, cplvs=cplvs, cpgrd=cpgrd, dfield=dfield)
         End Select
       Case (2) ! - models with separate hindcast and realtime datasets -
         Select Case (igrib)
          Case (ig_tave, ig_tmax, ig_tmin, ig_prcp, ig_ssts) ! - surface and near-surface variables -
            Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .true., .true., ltsv, .true., url_vars, ivar, ifail, &
                 ccvt=ccvt, nemh=nemh, nemf=nemf, ds2=ds2, dfield=dfield)
          Case (ig_gphs, ig_uwnd, ig_vwnd) ! - other variables by pressure level -
            Call get_download_url (Trim(cg_data), url_src, iymn, iymx, .true., .true., ltsv, .true., url_vars, ivar, ifail, &
                 ccvt=ccvt, nemh=nemh, nemf=nemf, ds2=ds2, nplv=nplv, iplvs=iplvs, cplvs=cplvs, cpgrd=cpgrd, dfield=dfield)
         End Select
      End Select
   End Select
!
! Open browser
   If (ifail == 0) Call use_url (Trim(url_dfile))
!
#if GUI == 0
! Download in batch versions
   If (ltsv) Then
      rdfile%cfile%fext = '.tsv'
! - prompt for download file -
1     Call get_new_file ('Download file', .true., rdfile%cfile%fext, ['Download Files'], [rdfile%cfile%fext], 1, &
           rdfile%cfile%fdir, rdfile%cfile%ffile, rdfile%cfile%fname, ifail)
      If (ifail == 0) Then
#if PC
! - check for full output path in Windows batch -
      If (AdjustL(rdfile%cfile%ffile(2:3)) /= ':/' .and. AdjustL(rdfile%cfile%ffile(2:3)) /= ':\') Then
         ifail = 1
         Call cpt_error ('download_data', .false., ifail)
         GoTo 1
      End If
#endif
! - replace special characters -
         Call encode_url (url_dfile)
         Call Execute_Command_Line (cmd_download//Trim(url_dfile)//opts_download//Trim(rdfile%cfile%ffile), &
              cmdstat=ifail, exitstat=ierr)
         If (ifail /= 0 .or. ierr /= 0) Then
            Call open_iofile (iout, Trim(rdfile%cfile%ffile), .false., .true., .false., ifail)
            If (ifail == 0) Then
               Write (Unit=iout, Fmt='(A)') Trim(url_dfile)
               
               Write (Unit=iout, Fmt='(A)') ' '
               Write (Unit=iout, Fmt='(A)') cmd_download//Trim(url_dfile)//' '//Trim(rdfile%cfile%ffile)
               
               Close (Unit=iout)
               Print*, 'The URL has been copied into '//Trim(rdfile%cfile%ffile)
            End If
            Print*, 'Try copying the URL into a browser and downloading manually.'
         End If
      End If
   End If
#endif
!
! Reset identifiers
2  iddt = 0
   igrib = 0
   isource = 0
!
! Free memory
   Call download_mem_dealloc ()
!
   Return
!
 Contains
!
!
  Subroutine get_dataset (iddt, ivar, isource, ifail)
!
! Reads dataset settings
!
! Modules
   Use data_io_constants, Only: ddir, iin
   Use data_iridl,        Only: c_ddt, dfile
   Use labels,            Only: cg_na, cg_password_l
   Use time,              Only: dnow
   Use gui,               Only: iw, &
                                init_win, prompt_list, win_prompt
   Use errors,            Only: cpt_error
   Use iofiles,           Only: open_iofile
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iddt ! - download data-type index -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ivar ! - required variable -
!
! - output scalars -
   Integer, Intent(Out) :: isource ! - download dataset identifier -
   Integer, Intent(Out) :: ifail   ! - error indicator -
!
! Locals
!
! Local scalars
   Integer :: i    ! - index -
   Integer :: j    ! - dataset index -
   Integer :: k    ! - variable index -
   Integer :: isrc ! - source index -
!
   Character(Len=  5) :: cds     ! - dataset structure -
   Character(Len=256) :: cline   ! - input line -
   Character(Len= 16) :: cmodf   ! - model family -
   Character(Len=  8) :: ctres   ! - temporal resolution -
   Character(Len= 32) :: cpt_tag ! - CPT tag -
!
   Logical :: lpwdr ! - password required? -
!
! Local arrays
   Integer, Dimension(:,:), Allocatable :: isrcs ! - source identifiers -
!
   Character(Len=32), Dimension(:), Allocatable :: csrcs ! - sources -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len, Trim
!
! Executable Statements
!
! Open dataset settings file
   Call open_iofile (iin, Trim(ddir)//dfile, .true., .true., .false., ifail)
   If (ifail /= 0) GoTo 3
   isource = 0
!
! Search for required data-download category
4  ifail = 6
   Do
      Read (Unit=iin, Fmt='(A)', Err=1, End=3) cline
      i = Index(cline, 'cpt:ddt='//Trim(c_ddt(iddt)))
      If (i > 0) Exit
   End Do
!
! Search for required dataset
! - construct tag -
   Write (Unit=cpt_tag, Fmt='(A,I0)') 'cpt:ivar=', ivar
! - search for tag -
   ifail = 7
   Do
      Read (Unit=iin, Fmt='(A)', Err=1, End=3) cline
      i = Index(cline, Trim(cpt_tag))
      If (i > 0) Exit
   End Do
!
! Identify number of data sources
   i = Index(cline, 'cpt:nsource=')
   If (i == 0) Then
      ifail = 8
      GoTo 3
   End If
   i = i + Len('cpt:nsource=')
   Read (Unit=cline(i:), Fmt=*) nsource
!
! Prompt for source
   If (nsource > 1) Then
      If (isource == 0) Then
         Allocate (csrcs(nsource))
         Allocate (isrcs(nsource,2))
! - identify sources -
         ifail = 9
         Do isrc = 1, nsource
            Do
               Read (Unit=iin, Fmt='(A)', Err=1, End=3) cline
               i = Index(cline, 'cpt:isource=')
               If (i > 0) Then
                  i = i + Len('cpt:isource=')
                  Read (Unit=cline(i:), Fmt=*) isrcs(isrc,1)
                  i = Index(cline, 'cpt:ioldsource=') ! - old source file identifier -
                  If (i > 0) Then
                     i = i + Len('cpt:ioldsource=')
                     Read (Unit=cline(i:), Fmt=*) isrcs(isrc,2)
                  Else
                     isrcs(isrc,2) = isrcs(isrc,1)
                  End If
                  i = Index(cline, 'cpt:csource=')
                  If (i == 0) GoTo 3
                  i = i + Len('cpt:csource=')
                  Read (Unit=cline(i:), Fmt=*) csrcs(isrc)
                  Read (Unit=iin, Fmt=*, Err=1, End=2) lpwdr ! - password protected? -
                  If (iddt == id_gcm) Then
                     Read (Unit=iin, Fmt=*)
                     Read (Unit=iin, Fmt='(A)', Err=1, End=2) cmodf ! - model family -
                     If (Trim(cmodf) /= cg_na .and. Trim(cmodf) /= 'N/A') csrcs(isrc) = Trim(csrcs(isrc))//'   ('//Trim(cmodf)//')'
                  End If
                  If (lpwdr) csrcs(isrc) = Trim(csrcs(isrc))//' ('//Trim(cg_password_l)//')'
                  Exit
               End If
            End Do
         End Do
! - prompt for source -
         isrc = 1
         Call init_win (title='Data sources')
         Call prompt_list ('Data sources', .false., csrcs, nsource, isrc, &
              ns=isrcs(:,1), altns=isrcs(:,2))
         iw = win_prompt(.true.)
         If (iw == 1 .and. isrc /= 0) Then
            isource = isrcs(isrc,1)
         Else
            isource = 0
         End If
         Deallocate (isrcs)
         Deallocate (csrcs)
! - rewind to relevant data source -
         If (isource > 0) Then
            Rewind (Unit=iin)
            GoTo 4
         Else
            ifail = -1
            GoTo 5
         End If
! - search for requested dataset -
      Else
         Write (Unit=cpt_tag, Fmt='(A,I0)') 'cpt:isource=', isource
         Do
            Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
            i = Index(cline, Trim(cpt_tag))
            If (i > 0) Exit
         End Do
      End If
   Else
      Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
   End If
!
! Identify dataset
   i = Index(cline, 'cpt:csource=')
   If (i == 0) Then
      ifail = 9
      GoTo 3
   End If
   i = i + Len('cpt:csource=')
   Read (Unit=cline(i:), Fmt=*) cg_data                               ! - dataset -
!
! Determine structure
   Read (Unit=iin, Fmt=*, Err=1, End=2)                               ! - password protected? -
   Read (Unit=iin, Fmt=*, Err=1, End=2) cds                           ! - dataset structure -
   If (iddt == id_gcm) Read (Unit=iin, Fmt=*, Err=1, End=2)           ! - model family -
   Read (Unit=iin, Fmt=*, Err=1, End=2) ndats                         ! - number of datasets -
   Read (Unit=iin, Fmt=*, Err=1, End=2) iymn                          ! - first year of data -
   iymx = dnow%iyr
   If (ndats > 1) Read (Unit=iin, Fmt=*, Err=1, End=2) ds2            ! - first date of second dataset -
   Read (Unit=iin, Fmt=*, Err=1, End=2) ctres                         ! - temporal resolution -
   ldaily = (Index(ctres, 'daily') > 0)
!
! Read appropriate structure
! - index data -
   Select Case (cds)
    Case ('index')
      Read (Unit=iin, Fmt=*, Err=1, End=2) nvar                       ! - number of indices -
      nplv = 0
! - grid data -
    Case ('grid')
      If (iddt == 3) Then
         Read (Unit=iin, Fmt=*, Err=1, End=2) nemh                    ! - number of ensemble members in hindcasts -
         If (ndats == 2) Then
            Read (Unit=iin, Fmt=*, Err=1, End=2) nemf                 ! - number of ensemble members in forecasts -
         Else
            nemf = nemh
         End If
      End If
      Read (Unit=iin, Fmt=*, Err=1, End=2) nplv                       ! - number of pressure levels -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%nlt              ! - number of latitudes -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%nlg              ! - number of longitudes -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%region%alim%rltn ! - northern limit -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%region%alim%rlts ! - southern limit -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%region%alim%rlgw ! - western limit -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%region%alim%rlge ! - eastern limit -
      Read (Unit=iin, Fmt=*, Err=1, End=2) dfield(1)%ln2s             ! - north-south orientation? -
      nvar = 1
   End Select
!
! Allocate memory
   Call download_mem_alloc (nvar, ndats, nplv)
!
! Read variable controls
   Select Case (cds)
    Case ('grid')
! - summed variables? -
      Do k = 1, nvar
         Read (Unit=iin, Fmt=*, Err=1, End=2) llsum(k)
      End Do
! - unit conversions -
      Do k = 1, nvar
         Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
         i = Index(cline, ' ')
         Read (Unit=cline(:i), Fmt='(A)', Err=1, End=2) ccvt(k)
      End Do
! - variables -
    Case ('index')
      Do k = 1, nvar
         Read (Unit=iin, Fmt=*, Err=1, End=2) cvars(k)
      End Do
   End Select
!
! Read URLs
! - dataset portal -
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
   i = Index(cline, ' ')
   Read (Unit=cline(:i), Fmt='(A)', Err=1, End=2) url_dl
! - source URL -
   Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
   i = Index(cline, ' ')
   Read (Unit=cline(:i), Fmt='(A)', Err=1, End=2) url_src
! - variables URLs -
   Do j = 1, ndats
      Do k = 1, nvar
         Read (Unit=iin, Fmt='(A)', Err=1, End=2) cline
         i = Index(cline, ' ')
         Read (Unit=cline(:i), Fmt='(A)', Err=1, End=2) url_vars(k,j)
      End Do
   End Do
!
! Read pressure levels
   Select Case (cds)
    Case ('grid')
      If (nplv > 0) Read (Unit=iin, Fmt=*, Err=1, End=2) cpgrd, (cplvs(i), i=1,nplv)
   End Select
   Close (Unit=iin)
!
   ifail = 0
   Return
!
! Errors
1  ifail = 4
   GoTo 3
!
2  ifail = 5
!
3  Call cpt_error ('get_dataset', .false., ifail)
!
! Close dataset file
5  Close (Unit=iin)
   Return
!
  End Subroutine get_dataset
!
!
!
  Subroutine download_mem_alloc (nvar, ndats, nplv)
!
! Allocates memory for reading download parameters
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nvar  ! - number of variables -
   Integer, Intent(In) :: ndats ! - number of datasets -
   Integer, Intent(In) :: nplv  ! - number of pressure levels -
!
! Executable Statements
!
! Allocate memory
   Allocate (ivars(nvar))
   Allocate (url_vars(nvar,ndats))
   Allocate (ccvt(nvar))
   Allocate (cvars(nvar))
   Allocate (llsum(nvar))
   If (nplv > 0) Then
      Allocate (iplvs(nplv))
      iplvs(:) = 0
      Allocate (cplvs(nplv))
   End If
!
   Return
  End Subroutine download_mem_alloc
!
!
!
  Subroutine download_mem_dealloc ()
!
! Frees memory for reading download parameters
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Free memory
   If (Allocated(cplvs))    Deallocate (cplvs)
   If (Allocated(iplvs))    Deallocate (iplvs)
   If (Allocated(llsum))    Deallocate (llsum)
   If (Allocated(cvars))    Deallocate (cvars)
   If (Allocated(ccvt))     Deallocate (ccvt)
   If (Allocated(url_vars)) Deallocate (url_vars)
   If (Allocated(ivars))    Deallocate (ivars)
!
   Return
  End Subroutine download_mem_dealloc
!
!
!
  Subroutine encode_url (url)
!
! Replaces non-ASCII characters in URL
!
! Modules
   Use data_iridl, Only: c_invalid, c_replace, ncode
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: url ! - URL -
!
! Locals
!
! Local scalars
   Integer :: i ! - invalid character index -
   Integer :: j ! - invalid character location -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Trim
!
! Executable Statements
!
! Replace left brackets
   Do i = 1, ncode
      search_ic: Do
         j = Index(Trim(url), c_invalid(i))
         If (j == 0) Exit search_ic
         url = url(1:j-1)//c_replace(i)//Trim(url(j+1:))
      End Do search_ic
   End Do
!
   Return
  End Subroutine encode_url
 End Function download_data
!
!
!
 Subroutine get_download_url (cg_data, url_src, iymn, iymx, ls0, lfv10, ltsv, lgrid, url_vars, ivar, ifail, &
            llsum, ccvt, cbox, nvar, cvars, nplv, iplvs, cplvs, cpgrd, nemh, nemf, ds2, dfield)
!
! Constructs URL for the download file
!
! Modules
   Use data_numbers,        Only: rp, r360
   Use data_time_constants, Only: nmn
   Use data_iridl,          Only: dext, url_dcpt, url_grid, url_index, url_scpt10, url_sources, url_tcpt10, url_yrange
#if PC == 1
   Use data_iridl,          Only: url_http
#endif
   Use labels,              Only: l_download, l_plevs
#if GUI == 1
   Use labels,              Only: l_cancel, l_regrid, l_selsubdom
#endif
   Use time,                Only: pdate, pprd, &
                                  Operator(>=), Operator(<), Operator(>), &
                                  dnow, &
                                  add_months
   Use gui,                 Only: iw, &
                                  box_close, box_open, boxes_open, init_win, print_text, prompt_list, prompt_select, win_prompt
#if GUI == 1
   Use gui,                 Only: add_button, gui_creturn
#endif
   Use errors,              Only: cpt_error
   Use settings,            Only: set_options
   Use fields,              Only: field
   Use season,              Only: cssel, &
                                  get_cssel
   Use missing,             Only: xmiss
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iymn ! - first year available -
   Integer, Intent(In) :: iymx ! - last year available -
!
   Character(Len=*), Intent(In) :: cg_data ! - data -
   Character(Len=*), Intent(In) :: url_src ! - source URL -
!
   Logical, Intent(In) :: lfv10 ! - format in expert mode for v10? -
   Logical, Intent(In) :: ls0   ! - prompt for start date? -
   Logical, Intent(In) :: ltsv  ! - point to .tsv file?
   Logical, Intent(In) :: lgrid ! - gridded data?
!
! - input/output scalars -
   Integer, Intent(InOut) :: ivar ! - variable index -
!
! - input/output arrays -
   Character(Len=*), Dimension(:,:), Intent(InOut) :: url_vars ! - variable URL -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nvar ! - number of variables -
   Integer, Intent(In), Optional :: nplv ! - number of pressure levels -
   Integer, Intent(In), Optional :: nemh ! - number of ensemble members in hindcasts -
   Integer, Intent(In), Optional :: nemf ! - number of ensemble members in forecasts -
!
   Character(Len=*), Intent(In), Optional :: cbox  ! - box heading for variable prompt -
   Character(Len=*), Intent(In), Optional :: cpgrd ! - pressure level grid code -
!
! - optional input arrays -
   Character(Len=*), Dimension(:), Intent(In), Optional :: ccvt  ! - unit conversions -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cvars ! - variable -
   Character(Len=*), Dimension(:), Intent(In), Optional :: cplvs ! - pressure levels -
!
   Logical, Dimension(:), Intent(In), Optional :: llsum ! - sum rather than average? -
!
! - optional input/output scalars -
   Type(pdate), Intent(InOut), Optional :: ds2 ! - first date of second dataset -
!
! - optional input/output arrays -
   Integer, Dimension(:), Intent(InOut), Optional :: iplvs ! - pressure level selections -
!
! - optional pointer arrays -
   Type(field), Dimension(:), Pointer, Optional :: dfield ! - field -
!
! Locals
!
! Local scalars
   Integer :: iok ! - Ok indicator -
   Integer :: ids ! - dataset identifier -
   Integer :: np  ! - number of prompts -
!
   Real(Kind=rp) :: d ! - grid resolution -
!
   Logical :: ldom   ! - prompt for domain? -
   Logical :: lemns  ! - calculate separate ensemle means? -
   Logical :: lplv   ! - prompt for pressure level? -
   Logical :: lsprev ! - previous source? -
   Logical :: lsum   ! - sum rather than average? -
   Logical :: lvar   ! - prompt for variable? -
!
   Type(pdate) :: d1 ! - start date -
   Type( pprd) :: d2 ! - end period -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Count, Present, Sum, Trim
!
! Executable Statements
!
! Backup
   ifail = set_options(i1=iyd1, i2=iyd2, i3=imd0, i4=imd1, i5=lsd, i6=nld, i7=ivar)
!
! Identify which prompts to use
   ldom = Present(dfield) ! - domain -
   lvar = Present(nvar)   ! - variable -
   lplv = Present(nplv)   ! - pressure level -
   np = Count([lvar, lplv])
!
! Initialise domain
   If (ldom) Call init_domain (dfield)
!
! Initialise season
   If (.not.ls0) imd0 = 0
   If (imd0 > 0) Then
      cssel = get_cssel(imd0, imd1, lsd, 1, 0)
   Else
      cssel = get_cssel(imd0, imd1, lsd, nld, 0)
   End If
!
! Identify whether source is the same as previous
   lsprev = (Trim(csprev) == cg_data)
!
! Reset defaults if outside of range
   If (iyd1 < iymn .or. .not.lsprev) iyd1 = iymn
   If (iyd2 < iyd1) iyd2 = iyd1
!
! Prompt for download parameters
1  iok = 0
   Call init_win (title=Trim(l_download%c)//' - '//Trim(cg_data))
   If (.not.ls0) imd0 = 0
! - variable -
   If (np > 0) Then
      Call boxes_open (2, 1, .true., .false.)
      If (np > 1) Call boxes_open (1, 2, .true., .false.)
   End If
   If (lvar) Then
      Call box_open (.false., .false., &
           title=cbox)
      Call prompt_list ('Variable', .true., cvars, nvar, ivar)
      Call box_close (.false.)
      If (np > 1) Call box_close (.false.)
      Call print_text (' ')
   Else
      ivar = 1
   End If
! - pressure level -
   If (lplv) Then
      Call box_open (.false., .false., &
           title=Trim(l_plevs%c))
      Call prompt_select (Trim(l_plevs%c), nplv, cplvs, iplvs)
      Call box_close (.false.)
      If (np > 1) Call box_close (.false.)
   End If
   If (np > 0) Call box_close (.false.)
! - dates -
   Call prompt_dates (iymn, iymx, ls0)
   If (np > 0) Call box_close (.false.)
! - domain and re-gridding -
   If (ldom) Then
#if GUI == 1
      Call gui_creturn (.false.)
      Call gui_creturn (.true.)
      Call add_button ('bt', 1, '&OK', &
           iset1=iok, ival1=1, scbf='EXIT')
      Call add_button ('bt', 2, l_selsubdom, &
           cbf=get_subdomain)
      Call add_button ('bt', 3, l_regrid, &
           cbf=get_newgrid)
      Call add_button ('bt', 0, l_cancel)
      iw = iok
#else
      If (lsubdom) iw = get_subdomain()
      If (lregrid) iw = get_newgrid()
      iw = win_prompt(.true.)
#endif
      ldom = ldset
   Else
      iw = win_prompt(.true.)
   End If
!
! Set option
   If (set_options(iw=iw, i1=iyd1, i2=iyd2, i3=imd0, i4=imd1, i5=lsd, i6=nld, i7=ivar, lrc=.false.) >= 3) Then
      ifail = -1
      Return
   End If
!
! Check number of selections
   If (lplv) Then
      If (Sum(iplvs(1:nplv)) == 0) Then
          If (nplv == 1) Then
             iplvs(:) = 1
          Else
             ifail = 1
             Call cpt_error ('get_download_url', .false., ifail)
             GoTo 1
         End If
      End If
   End If
!
! Set date limits
   If (.not.ls0) imd0 = imd1
   d1%iyr = iyd1
   d1%imn = imd0
   d1%idy = 1
   d2%sdate%iyr = iyd2
   d2%sdate%imn = imd0
   d2%sdate%idy = 1
   d2%edate = d2%sdate
   If (.not.ls0) Call add_months (lsd*nld, d2%edate)
!
! Check for valid settings
! - last date is before first date -
   If (d2%sdate < d1) Then
      iyd2 = iyd1
      ifail = 2
      Call cpt_error ('get_download_url', .false., ifail)
      GoTo 1
! - first date is in the future -
   Else If (d1 > dnow) Then
      If (iyd1 == dnow%iyr) Then
         iyd1 = dnow%iyr - 1
      Else
         iyd1 = dnow%iyr
      End If
      d1 = d2%sdate
      ifail = 3
      Call cpt_error ('get_download_url', .false., ifail)
      GoTo 1
   End If
   iyd2 = d2%edate%iyr
!
! Check for need for two datasets
   ids = 1
   If (Present(ds2)) Then
      If (ds2%imn < 1) Then ! - determine relative date -
         ds2%iyr = dnow%iyr
         ds2%imn = dnow%imn + ds2%imn
         ds2%idy = 1
         Do
            If (ds2%imn > 0) Exit
            ds2%imn = ds2%imn + nmn
            ds2%iyr = ds2%iyr - 1
         End Do
      End If
      If (d2%sdate >= ds2) Then ! - second dataset is needed -
         If (d1 < ds2) Then ! - first and second datasets are needed -
            ids = 4
         Else ! - only the second dataset is needed -
            ids = 2
         End If
      End If
   End If
!
! Determine whether to sum units
   If (Present(llsum)) Then
      lsum = llsum(ivar)
   Else
      lsum = .false.
   End If
!
! Determine whether to calcualte separate ensemble means
   lemns = (Present(nemh) .and. Present(nemf))
   If (lemns) Then
      lemns = (nemh /= nemf)
   End If
!
! Construct URL
#if PC == 1
   url_dfile = url_http//Trim(url_dl)//url_sources//url_src
#else
   url_dfile = Trim(url_dl)//url_sources//url_src
#endif
   Select Case (ids)
    Case (1, 2) ! - one dataset -
      url_dfile = Trim(url_dfile)//Trim(url_vars(ivar,ids))
    Case (4) ! - both datasets -
      url_dfile = Trim(url_dfile)//Trim(url_vars(ivar,1))
   End Select
   If (ldaily) url_dfile = Trim(url_dfile)//get_url_daily2monthly()
   If (ls0) Then
      Select Case (ids)
       Case (1, 2) ! - first or second dataset -
         url_dfile = Trim(url_dfile)//get_url_s(iyd1,iyd2)            ! - select start dates -
       Case (4) ! - first and second datasets -
         url_dfile = Trim(url_dfile)//get_url_s(iyd1, ds2%iyr-1)      ! - select start dates -
         If (lemns) url_dfile = Trim(url_dfile)//get_url_m(nemh)      ! - calculate ensemble means prior to appendstream -
         url_dfile = Trim(url_dfile)//url_sources//Trim(url_src)      ! - add second source -
         url_dfile = Trim(url_dfile)//Trim(url_vars(ivar,2))          ! - add second dataset -
         url_dfile = Trim(url_dfile)//get_url_s(ds2%iyr, iyd2)        ! - select start dates -
         If (lemns) url_dfile = Trim(url_dfile)//get_url_m(nemf)      ! - calculate ensemble means prior to appendstream -
         url_dfile = Trim(url_dfile)//get_url_append()                ! - append datasets -
      End Select
      url_dfile = Trim(url_dfile)//get_url_l(imd0, imd1, lsd, .true.) ! - select target season by lag -
   Else
      url_dfile = Trim(url_dfile)//get_url_t(lsum)                    ! - select target dates -
   End If
! - add pressure level -
   If (lplv) Then
      If (nplv > 1) url_dfile = Trim(url_dfile)//get_url_p(cpgrd, nplv, iplvs, cplvs)
   End If
! - calculate ensemble mean -
   If (Present(nemh)) Then
      If (.not.lemns) url_dfile = Trim(url_dfile)//get_url_m(nemh)
   End If
! - regrid -
   If (lrg) url_dfile = Trim(url_dfile)//get_url_regrid()
! - add domain -
   If (ldom) url_dfile = Trim(url_dfile)//get_url_xy()
! - convert units -
   If (Present(ccvt)) Then
      If (Trim(ccvt(ivar)) /= 'N/A') url_dfile = Trim(url_dfile)//get_url_convert(Trim(ccvt(ivar)))
   End If
! - set missing value flag -
   If (Present(dfield)) Then
      url_dfile = Trim(url_dfile)//get_url_missing(dfield(1)%rmiss)
      If (.not.dfield(1)%ln2s) url_dfile = Trim(url_dfile)//url_yrange ! - add Y-range to force latitude ordering -
   Else
      url_dfile = Trim(url_dfile)//get_url_missing(xmiss)
   End If
! - request CPT10 format -
   If ((lfv10 .or. ltsv) .and. lgrid) Then
      url_dfile = Trim(url_dfile)//url_grid
      If (ls0) Then
         url_dfile = Trim(url_dfile)//url_scpt10
      Else
         url_dfile = Trim(url_dfile)//url_tcpt10
      End If
      If (.not.ltsv) Then
         url_dfile = Trim(url_dfile)//'/'
      Else
         url_dfile = Trim(url_dfile)//dext
      End If
   Else
      If (.not.ltsv) Then
         url_dfile = Trim(url_dfile)//url_dcpt
      Else
         url_dfile = Trim(url_dfile)//url_index//url_tcpt10//dext
      End If
   End If
!
! Reset previous data source
   csprev = cg_data
   ifail = 0
!
   Return
!
 Contains
!
!
  Subroutine prompt_dates (iymn, iymx, ls0)
!
! Prompt for date download parameters
!
! Modules
#if GUI == 1
   Use data_text,           Only: ilang
   Use data_time_constants, Only: cma, cmon
#endif
   Use data_time_constants, Only: nmn
   Use labels,              Only: ca_seqs, cg_cselection
   Use gui,                 Only: box_close, box_open, boxes_open, print_cvalue, print_text, prompt_integer
   Use season,              Only: cssel, lssel
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iymn ! - minimum year -
   Integer, Intent(In) :: iymx ! - maximum year -
!
   Logical, Intent(In) :: ls0 ! - prompt for start date? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for download date parameters
   Call box_open (.false., .false., &
        title='Date limits')
   Call boxes_open (1, 2, .true., .false.)
! - prompt for year limits -
   Call box_open (.false., .false., &
        title=Trim(ca_seqs(1)))
   Call prompt_integer ('First year of data to download', iyd1, &
        itab=60, ilow=iymn, ihgh=iymx, lfocus=.true.)
   Call prompt_integer ('Last year of data to download', iyd2, &
        itab=60, ilow=iymn, ihgh=iymx, lfocus=.true.)
   Call box_close (.false.)
   Call box_close (.false.)
! - prompt for months limits -
   Call box_open (.false., .false., &
        title=Trim(ca_seqs(2))//' / '//Trim(ca_seqs(3)))
   If (ls0) Then
      Call prompt_integer ('First month of target period', imd1, &
           itab=60, ilow=1, ihgh=nmn, lfocus=.false., fcb1=update_dssel)
   Else
      Call prompt_integer ('First month of data to download', imd1, &
           itab=60, ilow=1, ihgh=nmn, lfocus=.false., fcb1=update_dssel)
   End If
   Call prompt_integer ('Length of season to average', lsd, &
        itab=60, ilow=1, ihgh=nmn, lfocus=.false., fcb1=update_dssel)
   If (ls0) Then
      Call prompt_integer ('Month forecasts were initialised', imd0, &
           itab=60, ilow=1, ihgh=nmn, lfocus=.false., fcb1=update_dssel)
   Else
      Call prompt_integer ('Number of seasons', nld, &
           itab=60, ilow=1, ihgh=nmn, lfocus=.false., fcb1=update_dssel)
   End If
! - prompt for monthly aggregation option -
   If (ldaily) Then
! ???      
   End If
   Call print_text (' ')
   Call print_cvalue (Trim(cg_cselection), cssel, &
        iwid=lssel)
#if GUI == 1
   Call print_text (' ')
   Call print_text ('Examples:')
   If (ls0) Then
      Call print_text ('   For '//cmon(6,ilang)//' from '//cmon(5,ilang)//' data enter 6, 1, 5')
      Call print_text ('   For '//cma(ilang)(6:9)//' from '//cmon(5,ilang)//' data enter 6, 4, 5')
   Else
      Call print_text ('   For '//cmon(4,ilang)//' data enter 4, 1, 1')
      Call print_text ('   For '//cma(ilang)(4:6)//' data enter 4, 3, 1')
      Call print_text ('   For '//cmon(4,ilang)//', '//cmon(5,ilang)//', and '//cmon(6,ilang)//' data enter 4, 1, 3')
      Call print_text ('   For '//cma(ilang)(4:6)//', and '//cma(ilang)(7:9)//' data enter 4, 3, 2')
   End If
#endif
   Call box_close (.false.)
   Call box_close (.false.)
   Call box_close (.false.)
!
   Return
  End Subroutine prompt_dates
!
!
!
  Subroutine init_domain (dfield)
!
! Initialises domain download parameters prompt
!
! Modules
   Use space, Only: dlats=>rlat1, dlngs=>rlng1, cdstn=>cstn1, dlatsd=>rlatd1, dlngsd=>rlngd1, cdstnd=>cstnd1, idom=>idom1, &
                    reset_grids
!
! Arguments
!
! Dummy arguments
! - input arrays -
   Type(field), Dimension(:), Intent(In) :: dfield ! - field settings -
!
! Locals
!
! Local scalars
   Integer :: i ! - latitude/longitude index -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, Real
!
! Executable Statements
!
! Initialise domain arrays
   ldset = .false.
   lrg = .false.
   Call reset_grids (1, 1, dfield(:)%nlt, dlats, dlngs, cdstn, dlatsd, dlngsd, cdstnd, idom, ifail, &
        nlg=dfield(:)%nlg)
!
! Set latitudes
   dlats(1,1) = dfield(1)%region%alim%rltn
   If (dfield(1)%nlt > 2) Then
      d = Abs(dfield(1)%region%alim%rltn - dfield(1)%region%alim%rlts)/Real(dfield(1)%nlt-1, Kind=rp)
      Do i = 2, dfield(1)%nlt - 1
         dlats(i,1) = dlats(i-1,1) - d
      End Do
   End If
   dlats(dfield(1)%nlt,1) = dfield(1)%region%alim%rlts
!
! Set longitudes
   dlngs(1,1) = dfield(1)%region%alim%rlgw
   If (dfield(1)%nlg > 2) Then
      If (dfield(1)%region%alim%rlge > dfield(1)%region%alim%rlgw) Then
         d = (dfield(1)%region%alim%rlge - dfield(1)%region%alim%rlgw)/Real(dfield(1)%nlg-1, Kind=rp)
      Else
         d = (r360+dfield(1)%region%alim%rlge-dfield(1)%region%alim%rlgw)/Real(dfield(1)%nlg-1, Kind=rp)
      End If
      Do i = 2, dfield(1)%nlg-1
         dlngs(i,1) = dlngs(i-1,1) + d
      End Do
   End If
   dlngs(dfield(1)%nlg,1) = dfield(1)%region%alim%rlge
!
   Return
  End Subroutine init_domain
 End Subroutine get_download_url
!
!
!
 Function update_dssel()
!
! Updates download season selection indicator
!
! Modules
   Use screen, Only: window_update
   Use season, Only: cssel, &
                     get_cssel
!
! Function result
   Integer :: update_dssel
!
! Executable Statements
!
! Update season selection indicator
   If (imd0 > 0) Then
      cssel = get_cssel(imd0, imd1, lsd, 1, 0)
   Else
      cssel = get_cssel(imd0, imd1, lsd, nld, 0)
   End If
   Call window_update (cssel)
   update_dssel = 2
!
   Return
 End Function update_dssel
!
!
!
 Function get_subdomain() &
          Result (get)
!
! Prompts for domain settings
!
! Modules
   Use space,  Only: dlats=>rlat1, dlngs=>rlng1, cdstn=>cstn1, dlatsd=>rlatd1, dlngsd=>rlngd1, cdstnd=>cstnd1, idom=>idom1
   Use fields, Only: get_area
!
! Function result
   Integer :: get
!
! Locals
!
! Local scalars
   Integer :: nv    ! - number of variables -
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Prompt for domain
   Call get_area ('1', 1, 1, 1, 1, 1, .false., dfield, dlats, dlngs, cdstn, dlatsd, dlngsd, cdstnd, idom, nv, ifail)
   Select Case (ifail)
    Case (0)
      ldset = .true.
      get = 1
    Case Default
      get = 2
   End Select
!
   Return
 End Function get_subdomain
!
!
!
 Function get_newgrid() &
          Result (get)
!
! Prompts for new grid settings
!
! Modules
   Use data_numbers, Only: zero, tenth
   Use gui,          Only: iw, &
                           init_win, prompt_real, win_prompt
   Use space,        Only: prompt_domain
!
! Function result
   Integer :: get
!
! Executable Statements
!
! Prompt for regridding parameters
   rnewlim = dfield(1)%region%alim
   Call init_win (title='Regrid')
! - prompt for gridpoints -
   Call prompt_domain ('Centres of first and last grid points', rnewlim, &
        dlim=dfield(1)%region%alim)
! - prompt for resolution -
   Call prompt_real ('Resolution (latitudes)', rlatnew, &
            itab=25, rfly=tenth, elow=zero)
   Call prompt_real ('Resolution (longitudes)', rlngnew, &
            itab=25, rfly=tenth, elow=zero)
   iw = win_prompt(.true.)
   Select Case (iw)
    Case (1)
      lrg = .true.
      get = 1
    Case Default
      get = 2
   End Select
!
   Return
 End Function get_newgrid
!
!
!
 Function get_url_daily2monthly() &
          Result (url)
!
! Constructs URL for converting from daily to monthly resolution
!
! Function result
   Character(Len=32) :: url ! - URL -
!
! Executable Statements
!
! Add requested daily to monthly conversion
   Select Case (imonthly)
    Case (1) ! - monthly averages -
      url = 'T/monthlyAverage/'
    Case (2) ! - monthly totals -
      url = 'T/monthlyAverage/30/mul'
!    Case (3) ! - monthly exceedances (exclusive) -
!      Write (Unit=url, Fmt='(A,I0,A)') '/', i, '/flaggt'
!    Case (4) ! - monthly exceedances (inclusive) -
!      Write (Unit=url, Fmt='(A,I0,A)') '/', i, '/flagge'
!    Case (5) ! - monthly non-exceedances (exclusive) -
!      Write (Unit=url, Fmt='(A,I0,A)') '/', i, '/flaglt'
!    Case (6) ! - monthly non-exceedances (inclusive) -
!      Write (Unit=url, Fmt='(A,I0,A)') '/', i, '/flagle'
   End Select
!
   Return
 End Function get_url_daily2monthly

!
!
!
 Function get_url_append() &
          Result (url)
!
! Constructs unit conversion component of URL
!
! Modules
   Use data_iridl, Only: url_append
!
! Function result
   Character(Len=16) :: url ! - URL -
!
! Executable Statements
!
! Convert units
   url = url_append
!
   Return
 End Function get_url_append
!
!
!
 Function get_url_convert(cvt) &
          Result (url)
!
! Constructs unit conversion component of URL
!
! Modules
   Use data_iridl, Only: url_convert
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cvt ! - conversion units -
!
! Executable Statements
!
! Convert units
   url = cvt//url_convert
!
   Return
 End Function get_url_convert
!
!
!
 Function get_url_l(imd0, imd1, lsd, lave) &
          Result (url)
!
! Constructs time component of URL using lag
!
! Modules
   Use time, Only: add_to_month
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imd0 ! - month of forecast initialization -
   Integer, Intent(In) :: imd1 ! - first month of data to download -
   Integer, Intent(In) :: lsd  ! - length of season to average -
!
   Logical, Intent(In) :: lave  ! - average? -
!
! Locals
!
! Local scalars
   Integer :: l1 ! - first lag -
   Integer :: l2 ! - second lag -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Determine lags
   If (imd0 /= imd1) Then
      l1 = add_to_month(imd1, -imd0)
   Else
      l1 = 0
   End If
   l2 = l1 + lsd - 1
!
! Construct time component of URL
   Select Case (lsd)
    Case (1) ! - no seasonal averaging -
      Write (Unit=url, Fmt='(A,I0,A)') 'L/(', l1, '.5)VALUES/'
    Case Default ! - seasonal averaging -
      Write (Unit=url, Fmt='(A,I0,A,I0,A)') 'L/', l1, '.5/', l2, '.5/RANGE'
      If (lave) url = Trim(url)//'[L]//keepgrids/average'
      url = Trim(url)//'/'
   End Select
!
   Return
 End Function get_url_l
!
!
!
 Function get_url_m(nem) &
          Result (url)
!
! Constructs ensemble averaging component of URL
!
! Function result
   Character(Len=32) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nem ! - number of ensemble members -
!
! Executable Statements
!
! Construct time component of URL
   If (nem > 1) Then
      Write (Unit=url, Fmt='(A,I0,A)') 'M/1/', nem, '/RANGE[M]average/'
   Else
      url = ' '
   End If
!
   Return
 End Function get_url_m
!
!
!
 Function get_url_missing(rmiss) &
          Result (url)
!
! Constructs missing value component of URL
!
! Modules
   Use data_numbers, Only: rp
   Use data_iridl,   Only: url_missing
   Use maths,        Only: get_cnumber
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Real(Kind=rp), Intent(In) :: rmiss ! - missing value flag -
!
! Executable Statements
!
! Construct missing value component of URL
! - set missing value flag -
   url = get_cnumber(rmiss,6)
! - construct URL -
   url = Trim(url)//url_missing
!
   Return
 End Function get_url_missing
!
!
!
 Function get_url_p(cg, n, i, p) &
          Result (url)
!
! Constructs pressure level component of URL
!
! Function result
   Character(Len=256) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of pressure level -
!
   Character(Len=*), Intent(In) :: cg ! - grid code -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: i ! - selected levels -
!
   Character(Len=*), Dimension(:), Intent(In) :: p ! - pressure levels -
!
! Locals
!
! Local scalars
   Integer :: ilev ! - current level -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Construct pressure level component of URL
   Write (Unit=url, Fmt='(2A)') Trim(cg), '/'
! - add pressure levels -
   Do ilev = 1, n
      If (i(ilev) == 1) url=Trim(url)//'('//Trim(p(ilev))//')'
   End Do
   url = Trim(url)//'VALUES/'
!
   Return
 End Function get_url_p
!
!
!
 Function get_url_regrid() &
          Result (url)
!
! Constructs regridding component of URL
!
! Modules
   Use data_numbers, Only: r360
   Use maths,        Only: get_cnumber
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Locals
!
! Local scalars
   Character(Len=32) :: clatn ! - northernmost latitude -
   Character(Len=32) :: clats ! - southernmost latitude -
   Character(Len=32) :: clngw ! - westernmost longitude -
   Character(Len=32) :: clnge ! - easternmost longitude -
   Character(Len=32) :: clatr ! - latitude resolution -
   Character(Len=32) :: clngr ! - longitude resolution -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Construct latitude/longitude specifiers
   clatn = get_cnumber(rnewlim%rltn, 5)
   clats = get_cnumber(rnewlim%rlts, 5)
   clngw = get_cnumber(rnewlim%rlgw, 5)
   If (rnewlim%rlgw < rnewlim%rlge) Then
      clnge = get_cnumber(rnewlim%rlge, 5)
   Else
      clnge = get_cnumber(rnewlim%rlge+r360, 5)
   End If
   clatr = get_cnumber(rlatnew, 5)
   clngr = get_cnumber(rlngnew, 5)
!
! Construct regridding component of URL
   If (dfield(1)%ln2s) Then
      Write (Unit=url, Fmt='(14A)') &
        'Y ', Trim(clatn), ' ', Trim(clatr), ' ', Trim(clats), ' evengridAverage/', &
        'X ', Trim(clngw), ' ', Trim(clngr), ' ', Trim(clnge), ' evengridAverage/'
   Else
      Write (Unit=url, Fmt='(14A)') &
        'Y ', Trim(clats), ' ', Trim(clatr), ' ', Trim(clatn), ' evengridAverage/', &
        'X ', Trim(clngw), ' ', Trim(clngr), ' ', Trim(clnge), ' evengridAverage/'
   End If
!
   Return
 End Function get_url_regrid
!
!
!
 Function get_url_s(iyd1, iyd2) &
          Result (url)
!
! Constructs time component of URL using start date
!
! Modules
   Use data_time_constants, Only: cmon
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iyd1 ! - first year of data to download -
   Integer, Intent(In) :: iyd2 ! - last year of data to download -
!
! Executable Statements!
!
! Construct time component of URL
   Write (Unit=url, Fmt='(3A,I4,A,I4,A)') 'S/(0000 1 ', cmon(imd0,1), ' ', iyd1, '-', iyd2, ')VALUES/'
!
   Return
 End Function get_url_s
!
!
!
 Function get_url_t(lsum) &
          Result (url)
!
! Constructs time component of URL
!
! Modules
   Use data_time_constants, Only: cmon
   Use time,                Only: add_to_month
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Logical, Intent(In) :: lsum ! - sum rather than average? -
!
! Locals
!
! Local scalars
   Character(Len=24) :: cfmt ! - format statement -
   Character(Len= 7) :: cmul ! - multiply (for sum) -
!
! Executable Statements
!
! Construct time component of URL
   Select Case (lsd)
    Case (1) ! - no seasonal averaging -
      Select Case (nld)
       Case (1) ! - no lagged fields -
         Write (Unit=url, Fmt='(3A,I4,A,I4,A)') 'T/(', cmon(imd1,1), ' ', iyd1, '-', iyd2, ')VALUES/'
       Case Default ! - lagged fields -
         imd2 = add_to_month(imd1, nld-1)
         Write (Unit=url, Fmt='(5A,I4,A,I4,A)') 'T/(', cmon(imd1,1), '-', cmon(imd2,1), ' ', iyd1, '-', iyd2, ')VALUES/'
      End Select
    Case Default ! - seasonal averaging -
      Select Case (nld)
       Case (1) ! - no lagged fields -
         imd2 = add_to_month(imd1, lsd-1)
         If (lsd < 10) Then
            cfmt = '(3A,I4,3A,I4,A,I1,A)'
         Else
            cfmt = '(3A,I4,3A,I4,A,I2,A)'
         End If
         Write (Unit=url, Fmt=cfmt) &
           'T/(', cmon(imd1,1), ' ', iyd1, ')(', cmon(imd2,1), ' ', iyd2, ')RANGEEDGES/T/', lsd, '/boxAverage/T/12/STEP/'
       Case Default ! - lagged fields -
         imd2 = add_to_month(imd1, lsd*nld-1)
         If (lsd < 10) Then
            cfmt = '(3A,I4,3A,I4,A,I1,6A)'
         Else
            cfmt = '(3A,I4,3A,I4,A,I2,6A)'
         End If
         Write (Unit=url, Fmt=cfmt) &
           'T/(', cmon(imd1,1), ' ', iyd1, ')(', cmon(imd2,1), ' ', iyd2, ')RANGEEDGES/T/', lsd, '/boxAverage/'//&
          &'T/(', cmon(imd1,1), '-', cmon(imd2,1), ')VALUES/'
      End Select
   End Select
!
! Convert to sum if desired
   If (lsum .and. lsd > 1) Then
      If (lsd < 10) Then
         cfmt = '(I1,A)'
      Else
         cfmt = '(I2,A)'
      End If
      Write (Unit=cmul, Fmt=cfmt) lsd, '/mul/'
      url=Trim(url)//Trim(cmul)
   End If
!
   Return
 End Function get_url_t
!
!
!
 Function get_url_xy() &
          Result (url)
!
! Constructs space component of URL
!
! Modules
   Use data_numbers, Only: r360
   Use maths,        Only: get_cnumber
!
! Function result
   Character(Len=128) :: url ! - URL -
!
! Locals
!
! Local scalars
   Character(Len=32) :: clatn ! - northernmost latitude -
   Character(Len=32) :: clats ! - southernmost latitude -
   Character(Len=32) :: clngw ! - westernmost longitude -
   Character(Len=32) :: clnge ! - easternmost longitude -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Construct latitude/longitude specifiers
   clatn = get_cnumber(dfield(1)%region%alim%rltn, 5)
   clats = get_cnumber(dfield(1)%region%alim%rlts, 5)
   clngw = get_cnumber(dfield(1)%region%alim%rlgw, 5)
   If (dfield(1)%region%alim%rlgw < dfield(1)%region%alim%rlge) Then
      clnge = get_cnumber(dfield(1)%region%alim%rlge, 5)
   Else
      clnge = get_cnumber(dfield(1)%region%alim%rlge+r360, 5)
   End If
!
! Construct space component of URL
   Write (Unit=url, Fmt='(9A)') &
     'Y/(', Trim(clatn), ')(', Trim(clats), ')RANGEEDGES/X/(', Trim(clngw), ')(', Trim(clnge), ')RANGEEDGES/'
!
   Return
 End Function get_url_xy
End Module downloads
