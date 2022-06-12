! Author: Simon Mason
Module data_gui_constants
!
! Contains declations and definitions of:
!    colour parameters
!    fonts
!    plotting parameters
!    graphics file types
!
! Modules
#if GUI == 1
   Use clrwin$,      Only: cw_handle
   Use data_numbers, Only: rp, one
#endif
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Parameters
!
! Integer parameters
#if GUI == 1
   Integer, Parameter, Public ::    icol_gr = 1         ! - graph colour scaling -
   Integer, Parameter, Public ::    icol_p2 = 2         ! - 2-category probability colour scaling -
   Integer, Parameter, Public ::    icol_p3 = 3         ! - 3-category probability colour scaling -
   Integer, Parameter, Public ::    icol_sk = 4         ! - skill score colour scaling (central white) -
   Integer, Parameter, Public ::    icol_mc = 5         ! - score colour scaling (monochrome) -
   Integer, Parameter, Public ::       mcol = 22        ! - number of shading colours -
   Integer, Parameter, Public ::       mnnc = 7         ! - minimum number of contours -
   Integer, Parameter, Public ::       mxnc = mcol - 1  ! - maximum number of contours -
   Integer, Parameter, Public ::       nmrk = 3         ! - number of station markers -
   Integer, Parameter, Public ::   i_handle = cw_handle ! - handle kind-type parameter -
#else
   Integer, Parameter, Public ::   i_handle = Kind(1)   ! - handle kind-type parameter -
#endif
   Integer, Parameter, Public ::       mnnt = 4         ! - minimum number of axis tick marks -
   Integer, Parameter, Public ::       mxnt = 10        ! - maximum number of axis tick marks -
   Integer, Parameter, Public ::     mtitle = 62        ! - maximum length of title -
#if GUI == 1
   Integer, Parameter, Public ::        ngf = 7         ! - number of graphics file formats -
   Integer, Parameter, Public ::      nfont = 4         ! - number of default fonts -
   Integer, Parameter, Public ::     nffont = 4         ! - number of fixed-with fonts -
   Integer, Parameter, Public :: iwid_dfont = 68        ! - default font scaling -
!
   Integer, Dimension(     3), Parameter, Public :: col_bg = &     ! - background colour -
      [ 130, 180, 170]
   Integer, Dimension(     3), Parameter, Public :: col_white = &  ! - white -
      [ 255, 255, 255]
   Integer, Dimension(     3), Parameter, Public :: col_black = &  ! - black -
      [   0,   0,   0]
   Integer, Dimension(     3), Parameter, Public :: col_htext = &  ! - highlighted text colour -
      [   0,   0, 150]
   Integer, Dimension(     3), Parameter, Public :: col_domain = & ! - domain colour -
      [ 255, 110, 110]
   Integer, Dimension(nffont), Parameter, Public :: iwid_ffont = & ! - fixed-width font scaling -
      [ 62, 75, 81, 75]
!
! Real parameters
   Real(Kind=rp), Parameter, Public :: dleft   = 0.15_rp ! - default left margin -
   Real(Kind=rp), Parameter, Public :: dright  = 0.05_rp ! - default right margin -
   Real(Kind=rp), Parameter, Public :: dbottom = 0.10_rp ! - default bottom margin -
   Real(Kind=rp), Parameter, Public :: dtop    = 0.10_rp ! - default top margin -
   Real(Kind=rp), Parameter, Public :: djqual  = one     ! - default JPEG quality -
!
! Character parameters
   Character(Len=*), Dimension( nfont), Parameter, Public :: dfont = & ! - default fonts -
      ['Arial          ', 'Segoe UI       ', 'Tahoma         ', 'MS Sans Serif  ']
   Character(Len=*), Dimension(nffont), Parameter, Public :: ffont = & ! - fixed-width fonts -
      ['Consolas              ', 'Lucida Sans Typewriter', 'Lucida Console        ', 'Courier New           ']
   Character(Len=*), Dimension(   ngf), Parameter, Public :: gext = &  ! - graphic file extensions -
      ['.jpg', '.bmp', '.gif', '.pcx', '.png', '.emf', '.svg']
#endif
!
End Module data_gui_constants
