! Author: Simon Mason
!
! Climate Predictability Tool for MOS Downscaling
!
! Written by Dr Simon J. Mason, Dr Michael K. Tippett, Dr Ángel G. Muñoz, and Lulin Song
!
#if GUI == 1 && FTN95 == 1
Winapp
!
#endif
Program cpt
!
! Modules
   Use projects,      Only: get_project_startup
   Use cpt_interface, Only: cpt_window, title_page
!
! Implicit declarations
   Implicit None
!
! Executable Statements
!
! Initialise CPT
   Call init_cpt1 ()
!
! Check for project from command line
   Call get_project_startup ()
!
! Print out title page
   Call title_page ()
!
! Complete initialisation
   Call init_cpt2 ()
!
! Prompt for program settings
   Call cpt_window ()
!
Contains
!
!
 Subroutine init_cpt1 ()
!
! Initialises CPT settings required before CPT can start up
!
! Modules
   Use data_numbers,      Only: sp, oneh=>oneh_sp
   Use data_version,      Only: cpt_name, cpt_title, cpt_ver, ver, ver_major, ver_minor, ver_patch
   Use data_io_constants, Only: ddir
   Use maths,             Only: init_numbers
   Use labels,            Only: init_labels
   Use time,              Only: init_time
   Use screen,            Only: init_screen
#if GUI == 1
   Use screen,            Only: init_fonts
   Use gui,               Only: init_cols
#endif
   Use menus,             Only: init_menus, set_menu_greying
   Use errors,            Only: cpt_error, init_errorl
   Use settings,          Only: lexpert
   Use iofiles,           Only: init_dirs
#if GUI == 1
   Use help,              Only: init_help
#endif
   Use verif,             Only: init_verif
   Use user,              Only: get_user
   Use projects,          Only: pdir, &
                                init_sets
!
! Implicit declarations
   Implicit None
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
   Character(Len=11) :: cproc ! - procedure -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Initialise version
   ver = Real(ver_major, Kind=sp) + Real(ver_minor, Kind=sp)/oneh
   Write (Unit=cpt_title, Fmt='(A,I2)') cpt_name//' ', ver_major
   Write (Unit=cpt_ver, Fmt='(I0,A,I0,A,I0)') ver_major, '.', ver_minor, '.', ver_patch
!
! Initialise time
   Call init_time ()
!
! Initialise numbers
   Call init_numbers ()
!
! Initialise screen size settings
   Call init_screen ()
!
#if GUI == 1
! Initialise colours
   Call init_cols ()
!
! Initialise GUI settings
   Call init_fonts ()
!
#endif
! Initialise directories
   Call init_dirs (ifail)
   If (ifail /= 0) Then
      cproc = 'init_dirs'
      GoTo 1
   End If
!
! Initialise labels
   Call init_labels (1,Trim(ddir),ifail)
   If (ifail /= 0) Then
      cproc = 'init_labels'
      GoTo 1
   End If
!
! Initialise menus
   Call init_menus ()
!
! Initialise verification
   Call init_verif ()
!
! Identify user
   Call get_user (pdir)
!
! Initialise settings
   Call init_sets ()
!
! Set menu greying
   Call set_menu_greying (lexpert=lexpert)
!
#if GUI == 1
! Identify help file
   Call init_help ()
!
! Initialise font settings
   Call init_fonts () ! - re-initialise for GUI-version in case of text resizing -
#endif
!
!
   Return
!
! Errors
1  Call init_errorl ()
   Call cpt_error (cproc, .false., ifail)
   Stop
 End Subroutine init_cpt1
!
!
!
 Subroutine init_cpt2 ()
!
! Initialises CPT settings required before CPT can prompt for options
!
! Modules
#if GUI == 1
   Use gui,        Only: mcw, &
                         init_cw
   Use errors,     Only: cpt_error
#endif
   Use iofiles,    Only: init_iofiles
   Use pcs,        Only: init_eofcalc
#if GUI == 1
   Use cpt_update, Only: init_image_sizes
#endif
!
! Implicit declarations
   Implicit None
#if GUI == 1
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Initialise images
   Call init_image_sizes ()
!
! Initialise child windows
   Call init_cw (ifail)
   If (ifail /= 0) Then
      ifail = 1
      Call cpt_error ('init_cw', .false., ifail, &
           i_arg1=mcw)
      Stop
   End If
#endif
!
! Re-initialise EOF calculations
   Call init_eofcalc ()
!
! Initialise structures and output file descriptions
   Call init_iofiles ()
!
   Return
 End Subroutine init_cpt2
End Program cpt
!
!

#if GUI == 1
Subroutine draw_domain (isize, mlim, alim, ctitle, ifail)
!
! Modules
   Use space,    Only: area
   Use graphics, Only: draw_simple_map
!
! Implicit declarations
   Implicit None
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: isize ! - image size -
!
   Character(Len=*), Intent(In) :: ctitle ! - map title -
!
   Type(area), Intent(In) :: mlim ! - map domain -
   Type(area), Intent(In) :: alim ! - selected domain -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Draw map
   Call draw_simple_map (isize, mlim, alim, ctitle, ifail)
!
   Return
End Subroutine draw_domain
#endif
