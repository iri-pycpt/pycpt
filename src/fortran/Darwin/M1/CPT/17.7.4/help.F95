! Author: Simon Mason
Module help
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: help_about
#if GUI == 1
   Public :: help_all, help_file, init_help
!
! Scalars
!
! Character scalars
   Character(Len=256), Private :: hdir  ! - help directory -
   Character(Len= 16), Private :: hfile ! - help file -
#endif
!
Contains
!
!
#if GUI == 1
 Subroutine init_help ()
!
! Initialises help
!
! Modules
   Use data_io_constants, Only: cdir, rdir
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Silverfrost functions
   Character(Len=256), External :: curdir$
!
! Executable Statements
!
! Set default help page
   hdir = Trim(curdir$())//cdir//Trim(rdir)//'help'//cdir
   hfile = 'Index.html'
!
   Return
 End Subroutine init_help
!
!
!
#endif
 Function help_about()
!
! Prints contact and version information about CPT
!
! Modules
#if GUI == 1
   Use data_numbers, Only: rp, one
#endif
   Use data_version, Only: cpt_name, cpt_title, cpt_ver, cyr1, cyr2, date_this, date_ver1, cpt_email, cpt_url
   Use gui,          Only: init_win, print_cvalue, print_text, win_prompt
#if GUI == 1
   Use gui,          Only: set_font, set_text_size
#endif
!
! Function result
   Integer :: help_about
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Kind, Len, Trim
!
! Executable Statements
!
! Create window
   Call init_win (title=cpt_title)
!
! Print about message
#if GUI == 1
   Call set_font (ts=2.5_rp)
#endif
   Call print_text (' ')
   Call print_text (cpt_name//', version '//Trim(cpt_ver))
   Call print_text (' ')
   Call print_text (' ')
#if GUI == 1
   Call set_text_size (one)
#endif
   Call print_cvalue ('Written', date_ver1, &
        itab=25, iwid=Int(Len(date_ver1), Kind=Kind(0)))
!       itab=25, iwid=Len(date_ver1)) ! $$$ FTN95 64-bit bug
   Call print_cvalue ('This version written', date_this, &
        itab=25, iwid=Int(Len(date_this), Kind=Kind(0)))
!       itab=25, iwid=Len(date_this)) ! $$$ FTN95 64-bit bug
   Call print_cvalue ('Written by', 'Simon J. Mason, Michael K. Tippett, Lulin Song, and Ángel G. Muñoz', &
        itab=25, iwid=50)
   Call print_cvalue ('Email', cpt_email, &
        itab=25, iwid=Int(Len(cpt_email), Kind=Kind(0)))
!       itab=25, iwid=Len(cpt_email)) ! $$$ FTN95 64-bit bug
   Call print_cvalue ('Web', cpt_url, &
        itab=25, iwid=Int(Len(cpt_url), Kind=Kind(0)))
!       itab=25, iwid=Len(cpt_url)) ! $$$ FTN95 64-bit bug
   Call print_text (' ')
   Call print_text ('International Research Institute for Climate and Society')
   Call print_text ('The Earth Institute at Columbia University')
   Call print_text ('61 Route 9W')
   Call print_text ('PO Box 1000')
   Call print_text ('Palisades, NY 10964-8000')
   Call print_text ('USA')
   Call print_text (' ')
   Call print_text ('Copyright '//cyr1//' - '//cyr2//' International Research Institute for Climate and Society')
   help_about = win_prompt(.false.)
   help_about = 2
!
   Return
 End Function help_about
#if GUI == 1
!
!
!
 Subroutine help_file (h_file)
!
! Opens a browser to display a specific help page
!
! Modules
   Use gui, Only: use_url
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: h_file
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Open browser
   Call use_url (Trim(hdir)//Trim(h_file))
!
   Return
 End Subroutine help_file
!
!
!
 Function help_all()
!
! Opens a browser to display help pages
!
! Modules
   Use gui, Only: use_url
!
! Function result	
   Integer :: help_all
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Open browser
   Call use_url (Trim(hdir)//hfile)
   help_all = 2
!
   Return
 End Function help_all
#endif
End Module help
