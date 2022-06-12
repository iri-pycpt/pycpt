! Author: Simon Mason
Module windows_gui
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: init_window_graphic_0, show_results
#if GUI == 1
   Public :: init_window_graphic
#endif
!
! Scalars
!
! Integer scalars
   Integer, Public :: imgid ! - image identifier -
!
Contains
!
!
 Function init_window_graphic_0(win_title, update_win, add_clearwin, &
          save_rslt, close_win1, close_win2, cxy, iloc, extra_prompt) &
          Result (init)
!
! Initialises a window with no graphic
!
! Modules
#if GUI == 1
   Use labels,  Only: l_saveres
   Use menus,   Only: add_menu
#endif
   Use gui,     Only: ic_par, &
                      gui_creturn, init_win, open_clearwin, window_end
#if GUI == 1
   Use fields,  Only: prompt_fields, prompt_location
#endif
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: win_title ! - window title -
!
! - procedure arguments -
   Integer, External :: update_win   ! - update window function -
   Integer, External :: add_clearwin ! - open ClearWin+ function -
!
! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: cxy ! - X/Y -
!
! - optional output scalars -
   Integer, Intent(Out), Optional :: iloc ! - location output unit number -
!
! - optional procedure arguments -
   Integer, External, Optional :: save_rslt    ! - save data function -
   Integer, External, Optional :: close_win1   ! - close window callback function -
   Integer, External, Optional :: close_win2   ! - close window callback function -
   Integer, External, Optional :: extra_prompt ! - extra_prompt callback function -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Open results window
   Call init_win (title=Trim(win_title), &
        lch=.true., ipar=ic_par, fclose=close_win1, fclose2=close_win2)
!
#if GUI == 1
! Add pop-up menu
   If (Present(save_rslt)) Call add_menu ('pm',1, -1, l_saveres, &
                                cbf=save_rslt)
!
! Additional prompt
   If (Present(extra_prompt)) init = extra_prompt(update_win)
!
! Prompt for field and lagged fields
   If (Present(cxy)) Call prompt_fields (cxy, update_win)
!
#endif
! Prompt for location
   If (Present(iloc)) Then
#if GUI == 1
      Call prompt_location (update_win)
#endif
      Call open_clearwin (50, 2, .true., iloc) ! - add ClearWin+ window for station details -
      Call gui_creturn (.false.)
   End If
!
! Insert ClearWin+ window
   init = add_clearwin()
!
! Complete window
   Call window_end ()
!
! Update the window
   init = update_win()
   init = 1
!
   Return
 End Function init_window_graphic_0
!
!
!
#if GUI == 1
 Function init_window_graphic(nimg, img, img_id, win_title, l_graph, update_win, title_grph, export_grph, &
          cxy, iloc, add_win_h, add_win_v, limits_grph, llims, select_grph, close_grph, extra_prompt, save_rslt, &
          lsq3, ipv, save_pvals, reset_img, c_custom, f_custom, f_help) &
          Result (init)
!
! Initialises a window with upto three graphics
!
! Modules
   Use data_cpt_constants, Only: id_stn
   Use data_gui_constants, Only: ngf
   Use data_io_constants,  Only: lstn
   Use labels,             Only: label, &
                                 l_lab, &
                                 l_custom, l_data, l_export, l_montage, l_pvalues, l_saveas, l_saveres, l_title, &
                                 ls_graphfmts
   Use gui,                Only: ic_par, &
                                 add_graphic_area, box_close, box_open, boxes_open, get_handle, gui_centre, gui_creturn, init_win, &
                                 open_clearwin, print_cvalue, set_cursor_waiting, window_end
   Use menus,              Only: add_menu
   Use iofiles,            Only: yfile
   Use space,              Only: cstnc
   Use fields,             Only: dsdy, &
                                 prompt_fields, prompt_location
   Use graphics,           Only: image, &
                                 ipic, &
                                 identify_location
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nimg ! - number of images -
!
   Character(Len=*), Intent(In) :: win_title ! - window title -
!
! - input arrays -
   Type(label), Dimension(:), Intent(In) :: l_graph ! - menu labels; shape(nimg) -
!
   Type(image), Dimension(:), Intent(InOut) :: img ! - graphics; shape(nimg) -
!
! - input/output scalars -
   Integer, Intent(InOut) :: img_id ! - selected image number -
!
! - procedure arguments -
   Integer, External :: update_win  ! - update window function -
   Integer, External :: title_grph  ! - title change function -
   Integer, External :: export_grph ! - save plot function -
!
! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: cxy      ! - X/Y (for both cxy=2) -
   Character(Len=*), Intent(In), Optional :: c_custom ! - additional customise menu item -
!
   Logical, Intent(In), Optional :: lsq3 ! - orient three images in a square? (default  ==  .true.)-
!
! - optional input arrays -
   Type(label), Dimension(:), Intent(In), Optional :: llims ! - limits menu; shape(nimg) -
!
! - optional input/output scalars -
   Integer, Intent(InOut), Optional :: ipv ! - pvalues grey menu item -
!
! - optional output scalars -
   Integer, Intent(Out), Optional :: iloc ! - location output unit number -
!
! - optional procedure arguments -
   Integer, External, Optional :: add_win_h    ! - additional horizontal windows section function -
   Integer, External, Optional :: add_win_v    ! - additional vertical windows section function -
   Integer, External, Optional :: close_grph   ! - close graphic callback function -
   Integer, External, Optional :: extra_prompt ! - extra_prompt callback function -
   Integer, External, Optional :: f_custom     ! - custom pop-up menu function -
   Integer, External, Optional :: f_help       ! - help callback function -
   Integer, External, Optional :: limits_grph  ! - data limits callback function -
   Integer, External, Optional :: save_pvals   ! - save p-values function -
   Integer, External, Optional :: save_rslt    ! - save results function -
   Integer, External, Optional :: select_grph  ! - selection callback function -
   Integer, External, Optional :: reset_img    ! - selection callback function -
!
! Locals
!
! Local scalars
   Integer :: i    ! - graphic index -
   Integer :: j    ! - graphic format index -
   Integer :: i1   ! - first index -
   Integer :: i2   ! - last index -
   Integer :: im   ! - menu-level index -
   Integer :: idir ! - direction -
!
   Logical :: laddwin ! - add additional formatting? -
   Logical :: lnewbox ! - new box? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Present, Trim
!
! Executable Statements
!
! Reset images
   Call set_cursor_waiting (1)
   If (Present(reset_img)) Then
      init = reset_img(img(:))
   Else
      img(:)%lcontour = .false.
   End If
!
! Open results window
   Call init_win (title=Trim(win_title), &
        lch=.true., ipar=ic_par, fclose=close_grph)
!
! Add pop-up menu
   Select Case (nimg)
    Case (1)
      i1 = 1
      i2 = nimg
      idir = 1
    Case (2)
      i1 = 1
      i2 = nimg
      idir = 1
    Case (3)
      If (Present(lsq3)) Then
         If (lsq3) Then
            i1 = nimg
            i2 = 1
            idir = -1
         Else
            i1 = 1
            i2 = nimg
            idir = 1
         End If
      Else
         i1 = nimg
         i2 = 1
         idir = -1
      End If
    Case (4)
      i1 = nimg
      i2 = 1
      idir = -1
   End Select
! - help -
   If (Present(f_help)) Then
      Call add_menu ('pm', 1, 1, 'Help', &
           cbf=f_help, iset1=imgid, ival1=img(1)%id)
   End If
! - customise -
   If (nimg > 1) Then
      im = 3
   Else
      im = 2
   End If
   Call add_menu ('pm', 1, 1, l_custom)
   Do i = i1, i2, idir
      If (nimg > 1) Call add_menu ('pm', 2, i, l_graph(i))
      Call add_menu ('pm', im, 1, l_title, &
           cbf=title_grph, iset1=imgid, ival1=img(i)%id)
      If (Present(llims)) Then
         If (llims(i)%iak > 0) Call add_menu ('pm', im, 2, llims(i), &
                                    cbf=limits_grph, iset1=imgid, ival1=img(i)%id)
      End If
   End Do
! - additional customise menu -
   If (Present(f_custom)) Then
      Call add_menu ('pm', 2, i2+1, c_custom, &
           cbf = f_custom, lsep = .true.)
   End If
! - exports -
   Call add_menu ('pm', 1, 2, l_export)
   Do i = i1, i2, idir
      If (nimg > 1) Call add_menu ('pm', 2, i, l_graph(i))
      Do j = 1, ngf
         l_lab%c = Trim(l_saveas%c)//' '//Trim(ls_graphfmts(j)%c)
         l_lab%iak = Len_Trim(l_saveas%c) + 2
         Call add_menu ('pm', im, j, l_lab, &
              cbf=export_grph, iset1=ipic, ival1=j, iset2=imgid, ival2=img(i)%id)
      End Do
   End Do
   If (nimg > 1) Then
      Call add_menu ('pm', 2, 3, l_montage, &
           lsep = .true.)
      Do j = 1, ngf
         l_lab%c = Trim(l_saveas%c)//' '//Trim(ls_graphfmts(j)%c)
         l_lab%iak = Len_Trim(l_saveas%c) + 2
         Call add_menu ('pm', 3, j, l_lab, &
              cbf=export_grph, iset1=ipic, ival1=j, iset2=imgid, ival2=-img(1)%id)
      End Do
   End If
! - save results -
   If (Present(save_rslt)) Then
      If (Present(save_pvals)) Then
         Call add_menu ('pm', 1, 4, l_saveres)
         Call add_menu ('pm', 2, 1, l_data, &
              cbf=save_rslt)
         Call add_menu ('pm', 2, 2, l_pvalues, &
              cbf=save_rslt, igrey=ipv)
      Else
         Call add_menu ('pm', 1, 3, l_saveres, &
              cbf=save_rslt)
      End If
   End If
!
! Prompt for field and lagged fields
   If (Present(cxy)) Then
      Select Case (cxy)
       Case ('X', 'x', 'Y', 'y')
         Call prompt_fields (cxy, update_win)
       Case ('2')
         Call prompt_fields ('X', update_win)
         Call prompt_fields ('Y', update_win)
      End Select
   End If
!
! Prompt for location
   If (Present(iloc)) Then
      Call prompt_location (update_win)
      Call gui_creturn (.false.)
      Call open_clearwin (50, 2, .true., iloc) ! - add ClearWin+ window for station details -
      Call gui_creturn (.false.)
   End If
! - print current station -
   If (Present(select_grph)) Then
      If (yfile%idstr == id_stn) Then
         Call gui_creturn (.false.)
         Call print_cvalue (Trim(dsdy), cstnc, &
              itab=15, iwid=lstn)
      End If
   End If
!
! Additional prompt
   If (Present(extra_prompt)) init = extra_prompt(update_win)
!
! Insert additional windows sections
! - horizontally -
   If (Present(add_win_h)) Then
      laddwin = .true.
      If (nimg >= 2) Then
         Call boxes_open (2, 2, .true., .false.)
      Else
         Call boxes_open (2, 1, .true., .false.)
      End If
      init = add_win_h()
! - vertically -
   Else If (Present(add_win_v)) Then
      laddwin = .true.
      Call boxes_open (1, 2, .true., .false.)
      init = add_win_v()
! - none -
   Else
      laddwin = .false.
      Call gui_creturn (.false.)
      If (nimg == 4) Then
         Call boxes_open (2, 2, .true., .false.)
      Else If (nimg > 1) Then
         Call boxes_open (2, 1, .true., .false.)
      Else
         Call box_open (.false., .false.)
      End If
   End If
   If (laddwin) Then
      Select Case (nimg)
       Case (1, 3)
         Call box_close (.false.)
      End Select
   End If
!
! Create graphics frames
   Do i = 1, nimg
      img(i)%ihandle = get_handle()
   End Do
   lnewbox = .true.
   Do i = i1, i2, idir
      If (lnewbox) Call gui_centre ()
      lnewbox = .false.
      If (Present(select_grph)) Then
         If (yfile%idstr == id_stn) Then
            Call add_graphic_area (img(i)%isize, img(i)%isize, &
                 lmouse=.true., img_handle=img(i)%ihandle, iset=img_id, ival=img(i)%id, cbf=select_grph, cbf2=identify_location)
         Else
            Call add_graphic_area (img(i)%isize, img(i)%isize, &
                 img_handle=img(i)%ihandle, iset=img_id, ival=img(i)%id, cbf=select_grph)
         End If
      Else
         Call add_graphic_area (img(i)%isize, img(i)%isize, &
              img_handle=img(i)%ihandle)
      End If
      If (Present(lsq3)) Then
         If (.not.lsq3 .and. i /= i2) Cycle
      End If
      Call box_close (.false.)
      lnewbox = .true.
   End Do
   Call window_end ()
!
! Update window
   init = update_win()
   Call set_cursor_waiting (0)
   init = 1
!
   Return
 End Function init_window_graphic
!
!
!
#endif
 Function show_results(ctitle, ncol, myf, iout, ih_f) &
          Result (show)
!
! Creates output window for results
!
! Modules
   Use data_gui_constants, Only: i_handle
#if GUI == 1
   Use screen,             Only: isize
   Use gui,                Only: gui_creturn, set_tabs
#endif
   Use gui,                Only: box_close, box_open, get_handle, open_clearwin
!
! Function result
   Integer :: show
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ncol ! - maximum number of columns -
   Integer, Intent(In) :: myf  ! - maximum number of available variables per field -
!
   Character(Len=*), Intent(In) :: ctitle ! - box title -
!
! - output scalars -
   Integer, Intent(Out) :: iout ! - output unit number -
!
   Integer(Kind=i_handle), Intent(Out) :: ih_f ! - handle -
!
! Locals
!
! Local scalars
   Integer :: nrow ! - number of rows -
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Nint, Real
!
! Executable Statements
!
! Open Clearwin+ windows
   Call gui_creturn (.true.)
   Call set_tabs (1, [35])
   nrow = Nint(Real(isize-55)/53.0)
#else
   nrow = myf
#endif
   ih_f = get_handle()
   Call box_open (.true., .false., &
        title=ctitle)
   If (myf <= nrow) Then
      Call open_clearwin (ncol, nrow, .true., iout, &
           ih=ih_f)
   Else
      Call open_clearwin (ncol, nrow, .true., iout, &
           ih=ih_f, maxl=myf, lvs = .true.)
   End If
   Call box_close (.true.)
   show = 1
!
   Return
 End Function show_results
End Module windows_gui
