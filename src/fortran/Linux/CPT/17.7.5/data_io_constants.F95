! Author: Simon Mason
Module data_io_constants
!
! Contains declarations and definitions of:
!    input and output unit numbes
!    format parameters
!    sizes of various character constants
!    maximum file widths
!    some CPT file tags
!    initialization file
!    directories
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Parameters
!
! Integer parameters
! - file unit numbers -
   Integer, Parameter, Public ::     iin = 11 ! - input file unit number -
   Integer, Parameter, Public ::    iout = 21 ! - file output unit number -
   Integer, Parameter, Public ::  iinstd = 5  ! - standard input unit number -
   Integer, Parameter, Public :: ioutstd = 6  ! - standard output unit number -
!
! - date-output formats -
   Integer, Parameter, Public :: io_iso = 1   ! - ISO format - 
   Integer, Parameter, Public :: io_com = 2   ! - common format - 
   Integer, Parameter, Public :: io_gra = 3   ! - GrADS format - 
   Integer, Parameter, Public :: io_int = 4   ! - international text format - 
!
! - formatting options -
   Integer, Parameter, Public :: nacc = 3 ! - number of access options -
   Integer, Parameter, Public :: ndel = 3 ! - number of delimiter options -
   Integer, Parameter, Public :: nfmt = 3 ! - number of format options -
   Integer, Parameter, Public :: nprc = 2 ! - number of data precisions -
!
   Integer, Parameter, Public :: lcoo = 15             ! - maximum length of coordinate -
   Integer, Parameter, Public :: ldat = 12             ! - maximum length of date -
   Integer, Parameter, Public :: ldir = 256            ! - maximum length of directory -
   Integer, Parameter, Public :: ldsc = 37             ! - maximum length of file description -
   Integer, Parameter, Public :: lext = 4              ! - maximum length of file extension -
#ifdef PGI
   Integer, Parameter, Public :: lfli = 2**22          ! - maximum number of characters in input line -
#elif IFORT
   Integer, Parameter, Public :: lfli = 2**21          ! - maximum number of characters in input line -
#elif GFORTRAN
   Integer, Parameter, Public :: lfli = 2**24          ! - maximum number of characters in input line -
#elif NAGFOR
   Integer, Parameter, Public :: lfli = 2**24          ! - maximum number of characters in input line -
#else
   Integer, Parameter, Public :: lfli = 2**24          ! - maximum number of characters in input line -
#endif
   Integer, Parameter, Public :: lnam = 64             ! - maximum length of file name -
   Integer, Parameter, Public :: lprd = 32             ! - maximum length of period -
   Integer, Parameter, Public :: lstn = 64             ! - maximum length of station names -
   Integer, Parameter, Public :: lstr = 16             ! - maximum length of file structure description -
   Integer, Parameter, Public :: ltag = 2**14          ! - maximum length of tag line -
   Integer, Parameter, Public :: lvar = 32             ! - maximum length of variable names and units -
!
   Integer, Parameter, Public :: lfil = ldir+lnam+lext ! - maximum length of file  -
!
! Character parameters
#ifdef BIGEND
   Character(Len=*), Parameter, Public :: cendian = 'big'    ! - byte-ordering -
#else
   Character(Len=*), Parameter, Public :: cendian = 'little' ! - byte-ordering -
#endif
#ifdef PC
   Character(Len=*), Parameter, Public :: cdir = &        ! - directory marker -
      '\'
#else
   Character(Len=*), Parameter, Public :: cdir = &        ! - directory marker -
      '/'
#endif
   Character(Len=*), Parameter, Public :: cxmlns = &      ! - XML namespace -
      'xmlns'
   Character(Len=*), Parameter, Public :: cxmlns_cf = &   ! - cf XML namespace -
      'http://cf/'
   Character(Len=*), Parameter, Public :: cxmlns_cpt = &  ! - CPT XML namespace -
      'http://iri.columbia.edu/CPT/v10/'
   Character(Len=*), Parameter, Public :: cxmlns_wmo = &  ! - WMO XML namespace -
      'http://www.wmo.int/'
   Character(Len=*), Parameter, Public :: default_ini = & ! - default CPT initialization file -
      'cpt.ini'
   Character(Len=*), Parameter, Public :: grads_ext = &   ! - GrADS control file extensions -
      '.ctl'
!
   Character(Len=*), Parameter, Public :: ca_del(ndel) = &  ! - delimiters -
      ['tab  ', 'space', 'comma']
   Character(Len=*), Parameter, Public :: ca_prcs(nprc) = & ! - data precisions -
      ['single', 'double']
   Character(Len=*), Parameter, Public :: ca_faccs(nacc) = & ! - file accesses -
#if GUI == 1
      ['sequential ', 'direct     ', 'transparent']
#else
      ['sequential ', 'direct     ', 'stream     ']
#endif
   Character(Len=*), Parameter, Public :: ca_ffmts(nfmt) = & ! - file formats -
      ['unformatted', 'formatted  ', 'GrADS      ']
!
! Scalars
!
! Character scalars
   Character(Len=256), Public :: ddir ! - data directory -
   Character(Len=256), Public :: rdir ! - CPT root directory -
!
End Module data_io_constants
