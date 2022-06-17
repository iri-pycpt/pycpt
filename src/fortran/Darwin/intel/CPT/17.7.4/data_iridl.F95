! Author: Simon Mason
Module data_iridl
!
! Contains declarations and definitions of:
!    CPT data download options
!    IRI Data Library Ingrid code
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Parameters
!
! Integer parameters
   Integer, Parameter, Public ::  nddt = 3 ! - number of downloadable data types -
   Integer, Parameter, Public :: ncode = 9 ! - number of encoded characters -
! - dataset type codes -
   Integer, Parameter, Public :: id_obs = 1 ! - observations -
   Integer, Parameter, Public :: id_rea = 2 ! - reanalyses -
   Integer, Parameter, Public :: id_gcm = 3 ! - model outputs -
! - GRIB codes -
   Integer, Parameter, Public :: ig_enso = 300 ! - ENSO indices -
   Integer, Parameter, Public :: ig_ssts =  80 ! - SSTs -
   Integer, Parameter, Public :: ig_tave =  11 ! - temperature mean -
   Integer, Parameter, Public :: ig_tmax =  15 ! - temperature maximum -
   Integer, Parameter, Public :: ig_tmin =  16 ! - temperature minimum -
   Integer, Parameter, Public :: ig_prcp =  61 ! - precipitation -
   Integer, Parameter, Public :: ig_wetd = 193 ! - wetdays -
   Integer, Parameter, Public :: ig_mslp =   2 ! - MSLP -
   Integer, Parameter, Public :: ig_gphs =   7 ! - geopotential heights -
   Integer, Parameter, Public :: ig_uwnd =  33 ! - zonal wind component -
   Integer, Parameter, Public :: ig_vwnd =  34 ! - meridional wind component -
   Integer, Parameter, Public :: ig_uvwd =  32 ! - zonal and meridional wind components -
!
! Character parameters
! - download file -
   Character(Len=*), Parameter, Public :: dfile = & ! - downloads configuration -
      'download_IRIDL.txt'
! - download file extensions -
   Character(Len=*), Parameter, Public :: dext = & ! - file extension -
      '.tsv'
!
! - Ingrid functions -
   Character(Len=*), Parameter, Public :: url_append = &  ! - append -
      'appendstream/'
   Character(Len=*), Parameter, Public :: url_convert = & ! - convert units -
      'unitconvert/'
   Character(Len=*), Parameter, Public :: url_dcpt = &    ! - CPT download format -
      'downloadsCPT.html'
   Character(Len=*), Parameter, Public :: url_grid = &    ! - CPT gridded data format -
      '[X/Y]'
   Character(Len=*), Parameter, Public :: url_http = &    ! - http -
      'https://'
   Character(Len=*), Parameter, Public :: url_index = &   ! - CPT index data format -
      '[]'
   Character(Len=*), Parameter, Public :: url_missing = & ! - missing value -
      '/setmissing_value/'
   Character(Len=*), Parameter, Public :: url_sources = & ! - sources -
      'SOURCES/'
   Character(Len=*), Parameter, Public :: url_scpt10 = &  ! - CPT download format -
      '[S/L/add]cptv10'
   Character(Len=*), Parameter, Public :: url_tcpt10 = &  ! - CPT download format -
      '[T]cptv10'
   Character(Len=*), Parameter, Public :: url_yrange = &  ! - Y-range -
      'Y/high/low/RANGE/'
!
! Downloadable data
   Character(Len=*), Dimension(nddt), Parameter, Public :: c_ddt = & ! - downloadable data types -
      ['obs       ', 'reanalyses', 'gcmouts   ']
!
! Encoded characters
   Character(Len=*), Dimension(ncode), Parameter, Public :: c_invalid = & ! - encoded characters -
      [  '%',   ' ',   '&',   '(',   ')',   '[',   ']',   '{',   '}']
   Character(Len=*), Dimension(ncode), Parameter, Public :: c_replace = & ! - replacement codes -
      ['%25', '%20', '%26', '%28', '%29', '%5B', '%5D', '%7B', '%7D']
!
End Module data_iridl
