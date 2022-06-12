! Author: Simon Mason
Module verif
!
! Modules
   Use data_cpt_constants, Only: ng
#if GUI == 1
   Use graphics,           Only: image
#endif
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: attrib_diagram, init_verif, n_verif, prob_scores, proj_read_verif, proj_write_verif, reset_verif, rhit_diagram, &
             roc_diagram, tendency_diagram, wrlt_diagram
#if GUI == 1
   Public :: init_img_verify
#endif
!
! Arrays
!
! Integer arrays
   Integer, Dimension(ng), Private :: ioutv1 ! - verification output unit number -
!
! Scalars
!
! Integer scalars
   Integer, Private :: ireld  ! - attributes diagram index -
   Integer, Private :: ioutv2 ! - verification output unit number -
!
   Integer, Public :: irv   ! - retroactive verification results available flag -
#if GUI == 1
   Integer, Public :: iwrlt ! - weather roulette diagram index -
!
! Arrays
!
! Derived type arrays
   Type(image), Dimension(1), Private :: img_rhit ! - ranked hits diagram -
   Type(image), Dimension(1), Private :: img_rocs ! - ROC diagram -
   Type(image), Dimension(1), Private :: img_tend ! - tendency diagram -
   Type(image), Dimension(1), Private :: img_wrlt ! - weather roulette graph -
!
   Type(image), Dimension(ng+1), Private :: img_rels ! - attributes diagrams -
#endif
!
Contains
!
!
 Subroutine init_verif ()
!
! Initialises list of probabilistic verification scores
!
! Modules
   Use data_cpt_constants, Only: npall
   Use labels,             Only: ca_pavscores_t, ca_pcvscores_t, ca_pvs_t
!
! Executable Statements
!
! Construct list of available scores
   ca_pvs_t(1:npall) = ca_pavscores_t(:)
   ca_pvs_t(npall+1:) = ca_pcvscores_t(:)
!
   Return
 End Subroutine init_verif
!
!
!
 Subroutine reset_verif ()
!
! Resets retroactive verification flag
!
! Executable Statements

! Set retroactive verification flag
   irv = 0
!
   Return
 End Subroutine reset_verif
!
!
!
 Subroutine n_verif ()
!
! Identifies number of cases for verification results
!
! Modules
   Use settings, Only: icv2, iretro, nt, ntr, nt1, nu, nur, nvrf, nvrfu, nvrf1
!
! Executable Statements
!
! Identify number of cases for verification results
   If (iretro == 1) Then
      nvrf = ntr
      nvrf1 = nt1
      nvrfu = nur
   Else If (icv2 /= 0) Then
      nvrf = nt
      nvrf1 = 0
      nvrfu = nu
   Else
      nvrf = 0
      nvrf1 = 0
   End If
!
   Return
 End Subroutine n_verif
!
!
!
#if GUI == 1
 Subroutine init_img_verify (igsz1, igsz2)
!
! Initialises verification plots
!
! Modules
   Use graphics, Only: init_image
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: igsz1 ! - graphics area dimension -
   Integer, Intent(In) :: igsz2 ! - graphics area dimension -
!
! Locals
!
! Local scalars
   Integer :: i ! - image index -
!
! Executable Statements
!
! Initialise graph size
   Do i = 1, ng+1
      Call init_image (img_rels(i), igsz2)
   End Do
   Call init_image (img_rhit(1), igsz1*3/2)
   Call init_image (img_rocs(1), igsz1*3/2)
   Call init_image (img_tend(1), igsz1*3/2)
   Call init_image (img_wrlt(1), igsz1*3/2)
!
   Return
 End Subroutine init_img_verify
!
!
!
#endif
 Function attrib_diagram()
!
! Creates a window for an attributes diagram
!
! Modules
   Use data_numbers, Only: rp
#if GUI == 1
   Use labels,       Only: l_attribs, &
                           ls_cats
#endif
   Use errors,       Only: cpt_error
   Use settings,     Only: ibs, icv2, iretro, nb, nu, nur
   Use iofiles,      Only: mya
   Use fields,       Only: yfield
   Use scores,       Only: afp, bss, b0s, b1s, ifq, orf, tends, &
                           calc_rel, hbrier, init_attrib, ranked_prob_score
   Use categories,   Only: iobs_r, iobs_2, pobs, fps_r, fps_2
#if GUI == 1
   Use windows_gui,  Only: imgid, &
                           init_window_graphic
#endif
!
! Function result
   Integer :: attrib_diagram
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Initialise
   ifail = init_attrib()
   If (ifail /= 0) GoTo 1
!
! Calculate reliability statistics
   If (iretro == 1) Then
      Call calc_rel (nur, mya, ng, nb, iobs_r(:,:), fps_r(1:mya,1:nur,:), yfield(1)%rmiss, ifq(:,:), afp(:,:), orf(:,:), &
           tends(:,:), b0s(:), b1s(:))
   Else If (icv2 /= 0) Then
      Call calc_rel ( nu, mya, ng, nb, iobs_2(:,:), fps_2(1:mya,1:nu,:),  yfield(1)%rmiss, ifq(:,:), afp(:,:), orf(:,:), &
           tends(:,:), b0s(:), b1s(:))
   End If
!
! Calculate Brier scores and skill scores
   If (iretro == 1) Then
      Call hbrier (nur, mya, ng, nb, ibs, iobs_r(:,:), fps_r(1:mya,1:nur,:), pobs(:,:), -999.0_rp, bss(1:ng,1), bss(1:ng,2))
   Else If (icv2 /= 0) Then
      Call hbrier ( nu, mya, ng, nb, ibs, iobs_2(:,:), fps_2(1:mya,1:nu,:),  pobs(:,:), -999.0_rp, bss(1:ng,1), bss(1:ng,2))
   End If
!
! Calculate ranked probability score and skill score
   If (iretro == 1) Then
      Call ranked_prob_score (nur, mya, ng, ibs*nb, iobs_r(:,:), fps_r(1:mya,1:nur,:), pobs(:,:), -999.0_rp, bss(0,1), bss(0,2))
   Else If (icv2 /= 0) Then
      Call ranked_prob_score ( nu, mya, ng, ibs*nb, iobs_2(:,:), fps_2(1:mya,1:nu,:),  pobs(:,:), -999.0_rp, bss(0,1), bss(0,2))
   End If
!
#if GUI == 1
! Open results window
   attrib_diagram = init_window_graphic(4, img_rels(:), imgid, l_attribs%c, [ls_cats(1:), ls_cats(0)], &
          update_attrib, title_verif, export_verif, f_help=help_verif,                                 &
          save_rslt=save_attrib)
#else
! Print results
   attrib_diagram = update_attrib()
#endif
!
! Errors
1  Select Case (ifail)
    Case (0)
      attrib_diagram = 1
    Case (1)
      attrib_diagram = close_attrib()
      Call cpt_error ('attrib_diagram', .false., ifail)
   End Select
!
   Return
 End Function attrib_diagram
!
!
!
 Function update_attrib() &
          Result (update)
!
! Plots reliability diagrams
!
! Modules
#if GUI == 1
   Use labels, Only: cg_attrib_t, &
                     c_lab, &
                     ls_cats, &
                     make_label
#else
   Use gui,    Only: prompt_yesno
#endif
!
! Function result
   Integer :: update
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set default graph titles
   Do ireld = 1, ng
      c_lab = make_label('L', ls_cats(ireld))
      Write (img_rels(ireld)%title, Fmt='(2A)') Trim(cg_attrib_t)//': ', Trim(c_lab)
   End Do
   c_lab = make_label('L', ls_cats(0))
   Write (img_rels(ng+1)%title, Fmt='(2A)') Trim(cg_attrib_t)//': ', Trim(c_lab)
!
#endif
! Plot attributes diagram
   Do ireld = 1, ng+1
      update = plot_reld()
   End Do
!
#if GUI == 0
! Prompt to save results
   If (prompt_yesno(.true., .true., .false., 'Do you want to save the results?') == 1) update = save_attrib()
!
#endif
   Return
 End Function update_attrib
!
!
!
 Function plot_reld() &
          Result (plot)
!
! Plots a reliability diagram
!
! Modules
#if GUI == 0
   Use data_numbers, Only: rp
   Use labels,       Only: ls_cats
#endif
   Use settings,     Only: nb
   Use scores,       Only: afp, ifq, orf
#if GUI == 1
   Use scores,       Only: bss, b0s, b1s, tends
   Use plotting,     Only: plot_attrib
#endif
!
! Function result
   Integer :: plot
!
! Locals
!
! Local scalars
   Integer :: icat ! - category number -
#if GUI == 0
   Integer :: j ! - category index -
!
   Real(Kind=rp) :: tfq ! - total -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Sum, Trim
#endif
!
! Executable Statements
!
! Identify category
   Select Case (ireld)
    Case (1:ng)
      icat = ireld
    Case Default
      icat = 0
   End Select
!
#if GUI == 1
! Plot attributes diagram
   plot = plot_attrib(img_rels(ireld), nb, icat, tends(1,icat), b0s(icat), b1s(icat), &
          ifq(:,icat), afp(:,icat), orf(:,icat), bss(icat,:))
#else
! Print results
   Write (Unit=*, Fmt='(2A)') 'Category: ', Trim(ls_cats(icat)%c)
   Write (Unit=*, Fmt='(4A18)') 'Forecast prob. (%)', 'Obs. rel. freq.', 'Frequency', 'Rel. freq. (%)'
   tfq = Real(Sum(ifq(:,icat)), Kind=rp)
   Do j = 1, nb
      Write (Unit=*, Fmt='(2F18.3,I18,F18.3)') afp(j,icat), orf(j,icat), ifq(j,icat), Real(100*ifq(j,icat), Kind=rp)/tfq
   End Do
   plot = 1
!
#endif
   Return
 End Function plot_reld
!
!
!
 Function save_attrib()
!
! Saves results for an attributes diagram
!
! Modules
   Use iofiles,    Only: atfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: save_attrib
!
! Executable Statements
!
! Save requested results
   save_attrib = save_results(ianal, atfile)
!
   Return
 End Function save_attrib
!
!
!
 Function close_attrib() &
          Result (fclose)
!
! Frees memory for attributes diagrams
!
! Modules
   Use scores, Only: afp, bss, b0s, b1s, ifq, orf, tends
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
   If (Allocated(bss))   Deallocate (bss)
   If (Allocated(b1s))   Deallocate (b1s)
   If (Allocated(b0s))   Deallocate (b0s)
   If (Allocated(tends)) Deallocate (tends)
   If (Allocated(orf))   Deallocate (orf)
   If (Allocated(afp))   Deallocate (afp)
   If (Allocated(ifq))   Deallocate (ifq)
   fclose = 0
!
   Return
 End Function close_attrib
!
!
!
 Function tendency_diagram()
!
! Creates a window for a tendency diagram
!
! Modules
#if GUI == 1
   Use labels,      Only: l_lab, &
                          l_tenddiag
#else
   Use labels,      Only: ls_cats
#endif
   Use errors,      Only: cpt_error
   Use settings,    Only: icv2, iretro, nb, nu, nur
   Use iofiles,     Only: mya
   Use fields,      Only: yfield
   Use scores,      Only: afp, b0s, b1s, ifq, orf, tends, &
                          calc_rel, init_attrib
   Use categories,  Only: iobs_r, iobs_2, fps_r, fps_2
#if GUI == 1
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#endif
!
! Function result
   Integer :: tendency_diagram
!
! Locals
!
! Local scalars
#if GUI == 0
   Integer :: i     ! - category index -
#endif
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
#if GUI == 1
   Intrinsic Trim
#endif
!
! Executable Statements
!
! Initialise
   ifail = init_attrib()
   If (ifail /= 0) GoTo 1
!
! Calculate reliability statistics
   If (iretro == 1) Then
      Call calc_rel (nur, mya, ng, nb, iobs_r(:,:), fps_r(1:mya,1:nur,:), yfield(1)%rmiss, ifq(:,:), afp(:,:), orf(:,:), &
           tends(:,:), b0s(:), b1s(:))
   Else If (icv2 /= 0) Then
      Call calc_rel ( nu, mya, ng, nb, iobs_2(:,:), fps_2(1:mya,1:nu,:),  yfield(1)%rmiss, ifq(:,:), afp(:,:), orf(:,:), &
           tends(:,:), b0s(:), b1s(:))
   End If
!
! Set default graph title
#if GUI == 1
   img_tend(:)%title = Trim(l_tenddiag%c)
!
! Open results window
   tendency_diagram = init_window_graphic(1,img_tend(:), imgid, l_tenddiag%c, [l_lab], &
                      update_tend, title_verif, export_verif, &
                      f_help=help_verif, close_grph=close_attrib)
#else
   Write (Unit=*, Fmt='(A,2A19)') 'Category    ', 'Obs. rel. freq. (%)', 'Forecast prob. (%)'
   Do i = 1, ng
      Write (Unit=*, Fmt='(A12,2F19.3)') ls_cats(i)%c, tends(1,i), tends(2,i)
   End Do
#endif
!
! Errors
1  Select Case (ifail)
    Case (0)
      tendency_diagram = 1
    Case (1)
      tendency_diagram = close_attrib()
      Call cpt_error ('tendency_diagram', .false., ifail)
   End Select
!
   Return
 End Function tendency_diagram
!
!
!
#if GUI == 1
 Function update_tend() &
          Result (plot)
!
! Plots a tendency diagram
!
! Modules
   Use data_cpt_constants, Only: ng
   Use scores,             Only: tends
   Use plotting,           Only: plot_tend
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot attributes diagram
   plot = plot_tend(img_tend(1), ng, tends(:,1:))
!
   Return
 End Function update_tend
!
!
!
#endif
 Function prob_scores()
!
! Creates a window for probabilistic scores
!
! Modules
   Use data_numbers,       Only: rp, one
   Use data_cpt_constants, Only: mps
#if GUI == 1
   Use labels,             Only: l_scoresp
#else
   Use gui,                Only: get_output_unit
#endif
   Use progress_meter,     Only: lstop, &
                                 close_progress_meter, end_progress_meter, set_progress_increment, start_progress_meter
   Use errors,             Only: cpt_error
   Use settings,           Only: ibs, icv2, iretro, nb, nu, nur
   Use iofiles,            Only: mya
   Use fields,             Only: yfield
   Use scores,             Only: pscores, &
                                 calc_pscores, close_pscores, init_pscores
   Use categories,         Only: fps_r, fps_2, iobs_r, iobs_2, pobs
#if GUI == 1
   Use windows_gui,        Only: init_window_graphic_0
#endif
   Use bootstrap,          Only: ibcl, nboot, pboot_cls, &
                                 close_pboot, boot_pscores, init_pboot
!
! Function result
   Integer :: prob_scores
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
#else
   Integer :: i ! - category index -
#endif
! Executable Statements
!
! Initialise
   ifail = init_pscores (mps)
   If (ifail /= 0) GoTo 1
   If (ibcl == 1) Then
      If (iretro == 1) Then
         ifail = init_pboot(mya, nur, ng)
      Else If (icv2 /= 0) Then
         ifail = init_pboot(mya, nu, ng)
      End If
      If (ifail /= 0) GoTo 1
!
! Calculate probabilistic scores
      Call start_progress_meter ('Calculating confidence intervals', .true.)
      Call set_progress_increment (one/Real(nboot+1, Kind=rp), .false.)
      If (iretro == 1) Then
         Call boot_pscores (nur, mya, nb, iobs_r(:,:), pobs(:,:), fps_r(1:mya,1:nur,:), yfield(1)%rmiss, pboot_cls)
      Else If (icv2 /= 0) Then
         Call boot_pscores ( nu, mya, nb, iobs_2(:,:), pobs(:,:), fps_2(1:mya,1:nu,:),  yfield(1)%rmiss, pboot_cls)
      End If
      If (lstop) Then
         Call close_progress_meter ()
         ifail = -1
         GoTo 1
      End If
   End If
   If (iretro == 1) Then
      Call calc_pscores (nur, mya, nb, ibs, iobs_r(:,:), pobs(:,:), fps_r(1:mya,1:nur,:), yfield(1)%rmiss, pscores(:), ifail)
   Else If (icv2 /= 0) Then
      Call calc_pscores ( nu, mya, nb, ibs, iobs_2(:,:), pobs(:,:), fps_2(1:mya,1:nu,:),  yfield(1)%rmiss, pscores(:), ifail)
   End If
   If (ifail /= 0) GoTo 1
   If (ibcl == 1) Call end_progress_meter (.true., .true.)
!
#if GUI == 1
! Open results window
   prob_scores = init_window_graphic_0(Trim(l_scoresp%c), print_pscores, show_pscores, &
                 close_win1=close_pboot, close_win2=close_pscores)
#else
! - print scores table
   Do i = 1, ng
      ioutv1(i) = get_output_unit()
   End Do
   ioutv2 = get_output_unit()
   prob_scores = print_pscores()
#endif
   ifail = 0
!
! Errors
1  Select Case (ifail)
    Case (0)
      Continue
    Case (-1)
      prob_scores = close_pscores()
      If (ibcl == 1) prob_scores = close_pboot()
    Case (1)
      prob_scores = close_pscores()
      If (ibcl == 1) prob_scores = close_pboot()
      Call cpt_error ('prob_scores', .false., ifail)
   End Select
   prob_scores = 1
!
   Return
 End Function prob_scores
!
!
!
 Function show_pscores() &
          Result (show)
!
! Opens ClearWin+ windows for contingency tables
!
! Modules
   Use data_cpt_constants, Only: ng, npall, npcat
   Use labels,             Only: ls_cats
   Use gui,                Only: box_close, box_open, boxes_open, open_clearwin, print_text
   Use bootstrap,          Only: ibcl
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: i    ! - category index -
   Integer :: ncol ! - number of columns for output window -
!
! Executable Statements
!
! Determine appropriate size of windows
   If (ibcl == 1) Then
      ncol=70
   Else
      ncol=55
   End If
!
! Format window
   Call boxes_open (2, 1, .true., .false.)
! - open category-specific scores window -
   Call print_text (' ')
   Call box_open (.true., .false., &
        title='Category-specific scores')
   Call boxes_open (1, ng, .true., .false.)
   Do i = ng, 1, -1
      Call print_text (' ')
      Call open_clearwin (ncol, npcat+1, .true., ioutv1(i), &
           cbox=Trim(ls_cats(i)%c))
      Call box_close (.false.)
   End Do
   Call box_close (.true.)
! - close first column, first row box -
   Call box_close (.false.)
! - open all-category scores window -
   Call print_text (' ')
   Call open_clearwin (ncol, npall+1, .true., ioutv2, &
        cbox='All-category scores')
! - close second column, second row box -
   Call box_close (.false.)
   show=1
!
   Return
 End Function show_pscores
!
!
!
 Function print_pscores()
!
! Prints probabilistic scores
!
! Modules
   Use data_cpt_constants, Only: npall, npcat
   Use labels,             Only: ca_pavscores_t, ca_pcvscores_t
#if GUI == 0
   Use labels,             Only: ls_cats
#endif
   Use gui,                Only: print_text
   Use scores,             Only: pscores
   Use bootstrap,          Only: ibcl, pboot_cls
!
! Function result
   Integer :: print_pscores
!
! Locals
!
! Local scalars
   Integer :: i  ! - score index -
   Integer :: j  ! - category index -
   Integer :: ij ! - score index -
!
! Executable Statements
!
! Print scores
! - print all-category scores -
   Call print_text (' ', &
        iout=ioutv2)
#if GUI == 1
   Call print_text (' ', &
        iout=ioutv2)
#else
   Call print_text ('All-category scores', &
        iout=ioutv2)
#endif
   Do i = 1, npall
      Write (Unit=ioutv2, Fmt='(A,F7.3)', Advance='no') ca_pavscores_t(i), pscores(i)
      If (ibcl == 1) Write (Unit=ioutv2, Fmt='(2F7.3)', Advance='no') pboot_cls(i)%lower, pboot_cls(i)%upper
      Write (Unit=ioutv2, Fmt='(A)', Advance='yes') ' '
   End Do
! - print category-specific scores -
#if GUI == 0
   Call print_text (' ', &
        iout=ioutv1(1))
   Call print_text ('Category-specific scores', &
        iout=ioutv1(1))
#endif
   Do j = ng, 1, -1
      Call print_text (' ', &
           iout=ioutv1(j))
#if GUI == 1
      Call print_text (' ', &
           iout=ioutv1(j))
#else
      Call print_text (ls_cats(j)%c, &
           iout=ioutv1(j))
#endif
      Do i = 1, npcat
         ij = npall + (j - 1)*npcat + i
         Write (Unit=ioutv1(j), Fmt='(A,F7.3)', Advance='no') ca_pcvscores_t(i), pscores(ij)
         If (ibcl == 1) Write (Unit=ioutv1(j), Fmt='(2F7.3)', Advance='no') pboot_cls(ij)%lower, pboot_cls(ij)%upper
         Write (Unit=ioutv1(j), Fmt='(A)', Advance='yes') ' '
      End Do
   End Do
   print_pscores = 2
!
   Return
 End Function print_pscores
!
!
!
 Function roc_diagram()
!
! Creates a window for an ROC diagram
!
! Modules
#if GUI == 1
   Use labels,      Only: l_lab, &
                          l_rocdiag
#else
   Use labels,      Only: ca_cat_a
   Use gui,         Only: prompt_yesno
#endif
   Use errors,      Only: cpt_error
   Use settings,    Only: icv2, iretro, nb, nu, nur
   Use iofiles,     Only: mya
   Use scores,      Only: fars, hits, rocas, &
                          rocp
   Use categories,  Only: iobs_r, iobs_2, fps_r, fps_2
#if GUI == 1
   Use windows_gui, Only: imgid, &
                          init_window_graphic
#endif
!
! Function result
   Integer :: roc_diagram
!
! Locals
!
! Local scalars
#if GUI == 0
   Integer :: i     ! - bin index -
   Integer :: j     ! - category index -
#endif
   Integer :: ifail ! - error indicator -
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
#endif
! Executable Statements
!
! Initialise
   ifail = init_roc()
   If (ifail /= 0) GoTo 1
!
! Calculate ROC statistics
   If (iretro == 1) Then
      Call rocp (mya, nur, ng, nb, iobs_r(:,:), fps_r(1:mya,1:nur,:), rocas(:), hits(:,:), fars(:,:))
   Else If (icv2 /= 0) Then
      Call rocp (mya,  nu, ng, nb, iobs_2(:,:), fps_2(1:mya,1:nu,:),  rocas(:), hits(:,:), fars(:,:))
   End If
!
#if GUI == 1
! Set default graph title
   img_rocs(:)%title = Trim(l_rocdiag%c)
!
! Plot graph
   roc_diagram = init_window_graphic(1, img_rocs(:), imgid, l_rocdiag%c, [l_lab], &
                 plot_roc_verif, title_verif, export_verif, &
                 save_rslt=save_rocs, f_help=help_verif, close_grph=close_roc)
   roc_diagram=1
#else
!
! Print ROC
   Do j = 1, ng
      Write (Unit=*, Fmt='(3A,F10.3)') ' ROC area (', ca_cat_a(j), '):', rocas(j)
      Write (Unit=*, Fmt=*)
      Write (Unit=*, Fmt='(6A)') '           Hit rates', '   False-alarm rates'
      Do i = 1, nb
         Write (Unit=*, Fmt='(2F20.3)') hits(i,j), fars(i,j)
      End Do
      Write (Unit=*, Fmt=*)
   End Do
!
! Prompt to save results
   If (prompt_yesno(.true., .true., .false., 'Do you want to save the results?') == 1) ifail = save_rocs()
#endif
!
! Errors
1  Select Case (ifail)
    Case (0)
      roc_diagram = 1
    Case (1)
      roc_diagram = close_roc()
      Call cpt_error ('roc_diagram', .false., ifail)
   End Select
!
   Return
 End Function roc_diagram
!
!
!
#if GUI == 1
 Function plot_roc_verif() &
          Result (plot)
!
! Plots a ROC diagram
!
! Modules
   Use data_cpt_constants, Only: ng
   Use settings,           Only: nb
   Use scores,             Only: fars, hits, rocas
   Use plotting,           Only: plot_roc
!
! Function result
   Integer :: plot
!
! Executble Statements
!
! Plot ROC diagrams
   plot = plot_roc (img_rocs(1), nb, ng, hits, fars, rocas)
!
   Return
 End Function plot_roc_verif
!
!
!
#endif
 Function init_roc() &
          Result (init)
!
! Initialises memory for an ROC diagram
!
! Modules
   Use settings, Only: nb
   Use scores,   Only: fars, hits
!
! Function result
   Integer :: init
!
! Locals
!
! Local scalars
   Integer :: istat ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated, Size
!
! Executable Statements
!
! Allocate workspace
   init = 1
! - hit rates -
   If (.not.Allocated(hits)) Then
      Allocate (hits(nb,ng), Stat=istat)
      If (istat /= 0) Return
   Else If (Size(hits, Dim=1) /= nb) Then
      Deallocate (hits)
      Allocate (hits(nb,ng), Stat=istat)
      If (istat /= 0) Return
   End If
! - false alarm rates -
   If (.not.Allocated(fars)) Then
      Allocate (fars(nb,ng), Stat=istat)
      If (istat /= 0) Return
   Else If (Size(fars, Dim=1) /= nb) Then
      Deallocate (fars)
      Allocate (fars(nb,ng), Stat=istat)
      If (istat /= 0) Return
   End If
!
   init = 0
!
   Return
 End Function init_roc
!
!
!
 Function save_rocs()
!
! Saves ROC results
!
! Modules
   Use iofiles,    Only: rrfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: save_rocs
!
! Executable Statements
!
! Save requested results
   save_rocs = save_results(ianal, rrfile)
!
   Return
 End Function save_rocs
!
!
!
 Function close_roc() &
          Result (fclose)
!
! Frees memory for an ROC diagram
!
! Modules
   Use scores, Only: fars,hits
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
   If (Allocated(fars)) Deallocate (fars)
   If (Allocated(hits)) Deallocate (hits)
   fclose = 0
!
   Return
 End Function close_roc
!
!
!
 Function rhit_diagram()
!
! Creates a window for a ranked hits diagram
!
! Modules
   Use data_numbers, Only: zero
#if GUI == 1
   Use labels,       Only: l_rhitsdiag, &
                           l_lab
#else
   Use gui,          Only: prompt_yesno
#endif
   Use errors,       Only: cpt_error
   Use settings,     Only: icv2, iretro, nvrfu
#if GUI == 0
   Use settings,     Only: nt1
   Use arrays,       Only: cyprds
#endif
   Use iofiles,      Only: mya
   Use scores,       Only: rhit, &
                           ranked_hits
#if GUI == 0
   Use season,       Only: isnc
#endif
   Use categories,   Only: iobs_r, iobs_2, fps_r, fps_2
#if GUI == 1
   Use windows_gui,  Only: imgid, &
                           init_window_graphic
#endif
!
! Function result
   Integer :: rhit_diagram
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
   Integer :: k     ! - case index -
#if GUI == 0
   Integer :: it1   ! - first date index -
   Integer :: j     ! - category index -
#else
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
#endif
!
! Executable Statements
!
! Initialise
   Call n_verif ()
   ifail = init_rhit(nvrfu)
   If (ifail /= 0) GoTo 1
!
! Calculate hit scores
   If (iretro == 1) Then
      Do k = 1, nvrfu
         Call ranked_hits (1, mya, ng, iobs_r(:,k:k), fps_r(:,k:k,:), zero, rhit(k,:))
      End Do
   Else If (icv2 /= 0) Then
      Do k = 1, nvrfu
         Call ranked_hits (1, mya, ng, iobs_2(:,k:k), fps_2(:,k:k,:), zero, rhit(k,:))
      End Do
   End If
!
#if GUI == 1
! Set default graph title
   img_rhit(:)%title = Trim(l_rhitsdiag%c)
!
! Plot profits graph
   rhit_diagram = init_window_graphic(1, img_rhit(:), imgid, l_rhitsdiag%c, [l_lab], &
                  update_rhit, title_verif, export_verif, &
                  save_rslt=save_rhit, f_help=help_verif, close_grph=close_rhit)
#else
!
! Print results
   Write (Unit=*, Fmt=*)
   Write (Unit=*, Fmt='(A)') 'Ranked Hits'
   Write (Unit=*, Fmt='(A,3I20)') 'Date     ', (j, j = 1, ng)

   If (iretro == 1) Then
      it1 = nt1 + 1
   Else If (icv2 /= 0) Then
      it1 = 1
   End If
   Do k = 1, nvrfu
      Write (Unit=*, Fmt='(A,3F20.2)') cyprds(it1+k-1)%cssn(isnc), (rhit(k,j), j = 1, ng)
   End Do
   Write (Unit=*, Fmt=*)
!
! Prompt to save results
   If (prompt_yesno(.true., .true., .false., 'Do you want to save the results?') == 1) ifail = save_rhit()
#endif
!
! Errors
1  Select Case (ifail)
    Case (0)
      rhit_diagram = 1
    Case (1)
      rhit_diagram = close_rhit()
      Call cpt_error ('rhit_diagram', .false., ifail)
   End Select
!
   Return
!
 Contains
!
!
  Function init_rhit(n) &
           Result (init)
!
! Initialises memory for a ranked hits diagram
!
! Modules
   Use data_cpt_constants, Only: ng
   Use scores,             Only: rhit
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! number of cases -
!
! Locals
!
! Local scalars
   Integer :: istat ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Allocate workspace
   init = 1
! - ranked hits -
   If (.not.Allocated(rhit)) Then
      Allocate (rhit(n,ng), Stat=istat)
      If (istat /= 0) Return
   End If
   init = 0
!
   Return
  End Function init_rhit
 End Function rhit_diagram
!
!
!
#if GUI == 1
 Function update_rhit() &
          Result (plot)
!
! Plots a ranked hits diagram
!
! Modules
   Use time,     Only: Operator(+)
   Use settings, Only: icv2, iretro, nt, ntr, nt1
   Use iofiles,  Only: yfile
   Use scores,   Only: rhit
   Use missing,  Only: ktuse
   Use plotting, Only: plot_rhit
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Plot profits diagram
   If (iretro == 1) Then
      plot = plot_rhit(img_rhit(1), ng, ntr, yfile%fdate+nt1, rhit, ktuse(nt1+1:))
   Else If (icv2 /= 0) Then
      plot = plot_rhit(img_rhit(1), ng, nt, yfile%fdate, rhit, ktuse(:))
   End If
!
   Return
 End Function update_rhit
!
!
!
#endif
 Function save_rhit()
!
! Saves results for a ranked hits diagram
!
! Modules
   Use iofiles,    Only: rhfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: save_rhit
!
! Executable Statements
!
! Save requested results
   save_rhit = save_results(ianal, rhfile)
!
   Return
 End Function save_rhit
!
!
!
 Function close_rhit() &
          Result (fclose)
!
! Frees memory for a ranked hits diagram
!
! Modules
   Use scores, Only: rhit
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
   If (Allocated(rhit)) Deallocate (rhit)
   fclose = 0
!
   Return
 End Function close_rhit
!
!
!
 Function wrlt_diagram()
!
! Creates a window for a weather roulette diagram
!
! Modules
   Use data_numbers,       Only: zero
#if GUI == 1
   Use labels,             Only: l_lab, &
                                 l_wrlt, &
                                 ls_wrlts
#else
   Use data_cpt_constants, Only: nwr
   Use data_io_constants,  Only: io_com, lprd
   Use labels,             Only: ls_wrlts
   Use time,               Only: Operator(+), &
                                 get_cdate
   Use gui,                Only: prompt_yesno
#endif
   Use errors,             Only: cpt_error
   Use settings,           Only: icv2, iretro, nvrfu
#if GUI == 0
   Use settings,           Only: nt1
   Use iofiles,            Only: yfile
#endif
   Use iofiles,            Only: mya
   Use scores,             Only: wrlts, &
                                 profits
   Use categories,         Only: iobs_r, iobs_2, pobs, fps_r, fps_2
#if GUI == 1
   Use windows_gui,        Only: imgid, &
                                 init_window_graphic
#endif
!
! Function result
   Integer :: wrlt_diagram
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
#if GUI == 0
   Integer :: i     ! - score index -
   Integer :: it1   ! - first date index -
   Integer :: k     ! - case index -
!
   Character(Len=lprd) :: cout ! - output field -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
#endif
!
! Executable Statements
!
! Initialise
   Call n_verif ()
   ifail = init_wrlt(nvrfu)
   If (ifail /= 0) GoTo 1
!
! Calculate cumulative profits
   If (iretro == 1) Then
      Call profits (nvrfu, mya, iobs_r(:,:), fps_r(1:mya,1:nvrfu,:), pobs(:,:), zero, wrlts(1,0:), wrlts(2,0:), wrlts(3,0:))
   Else If (icv2 /= 0) Then
      Call profits (nvrfu, mya, iobs_2(:,:), fps_2(1:mya,1:nvrfu,:), pobs(:,:), zero, wrlts(1,0:), wrlts(2,0:), wrlts(3,0:))
   End If
!
#if GUI == 1
! Set default graph title
   img_wrlt(:)%title=Trim(ls_wrlts(iwrlt)%c)
!
! Plot profits graph
   wrlt_diagram = init_window_graphic(1, img_wrlt(:), imgid, l_wrlt%c, [l_lab], &
                  plot_wrlt, title_verif, export_verif, &
                  save_rslt=save_wrlt, f_help=help_verif, close_grph=close_wrlt)
#else
!
! Print results
   Write (Unit=*, Fmt=*)
   Write (Unit=*, Fmt='(A)') 'Weather Roulette'
   Write (Unit=*, Fmt='(A,T15,2A20,A25)') 'Date', (Trim(ls_wrlts(i)%c), i=1,nwr)
   If (iretro == 1) Then
      it1 = nt1 + 1
   Else If (icv2 /= 0) Then
      it1 = 1
   End If
   Do k = 0, nvrfu
      cout = get_cdate(yfile%prd1+(it1+k-2), 1, io_com)
      Write (Unit=*, Fmt='(A,T15,2F20.3,F25.3)') Trim(cout),(wrlts(i,k), i=1,nwr)
   End Do
   Write (Unit=*, Fmt=*)
!
! Prompt to save results
   If (prompt_yesno(.true., .true., .false., 'Do you want to save the results?') == 1) ifail = save_wrlt()
#endif
!
! Errors
1  Select Case (ifail)
    Case (0)
      wrlt_diagram = 1
    Case (1)
      wrlt_diagram = close_wrlt()
      Call cpt_error ('wrlt_diagram', .false., ifail)
   End Select
!
   Return
!
 Contains
!
!
  Function init_wrlt(n) &
           Result (init)
!
! Initialises memory for a weather roulette diagram
!
! Modules
   Use data_cpt_constants, Only: nwr
   Use scores,             Only: wrlts
!
! Function result
   Integer :: init
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! number of cases -
!
! Locals
!
! Local scalars
   Integer :: istat ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Allocated
!
! Executable Statements
!
! Allocate workspace
   init = 1
! - weather roulette scores -
   If (.not.Allocated(wrlts)) Then
      Allocate (wrlts(nwr,0:n), Stat=istat)
      If (istat /= 0) Return
   End If
   init = 0
!
   Return
  End Function init_wrlt
 End Function wrlt_diagram
!
!
!
#if GUI == 1
 Function plot_wrlt() &
          Result (plot)
!
! Plots a weather roulette diagram
!
! Modules
   Use data_time_constants, Only: iuseq
   Use time,                Only: Operator(+)
   Use labels,              Only: ca_seq, &
                                  ls_wrlts
   Use settings,            Only: nvrf, nvrf1
   Use iofiles,             Only: yfile
   Use scores,              Only: wrlts
   Use missing,             Only: ktuse
   Use plotting,            Only: plot_line_graph
!
! Function result
   Integer :: plot
 Logical, Dimension(:), Allocatable :: ku
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Transpose, Trim
!
! Executable Statements
!
! Plot profits diagram
   Select Case (iwrlt)
    Case (1)
      plot = plot_line_graph (img_wrlt(1), Trim(ca_seq(iuseq(yfile%iseq))), Trim(ls_wrlts(iwrlt)%c), .false., nvrf, 1, &
             Transpose(wrlts(iwrlt:iwrlt,1:)), &
             d1=yfile%fdate+nvrf1, kuse=ktuse(nvrf1+1:))
    Case (2, 3)
! - reallocate ktuse for FTN95 bug(?) -
 Allocate (ku(nvrf+1))
 ku(1)=.true.
 ku(2:)=ktuse(nvrf1+1:)
      plot = plot_line_graph (img_wrlt(1),Trim(ca_seq(iuseq(yfile%iseq))), Trim(ls_wrlts(iwrlt)%c), .false., nvrf+1, 1, &
             Transpose(wrlts(iwrlt:iwrlt,0:)), &
             d1=yfile%fdate+(nvrf1-1), kuse=ku(:))
 Deallocate(ku)
   End Select
!
   Return
 End Function plot_wrlt
!
!
!
#endif
 Function save_wrlt()
!
! Saves results for a weather roulette diagram
!
! Modules
   Use iofiles,    Only: wrfile
   Use cpt_output, Only: save_results
   Use analysis,   Only: ianal
!
! Function result
   Integer :: save_wrlt
!
! Executable Statements
!
! Save requested results
   save_wrlt = save_results(ianal, wrfile)
!
   Return
 End Function save_wrlt
!
!
!
 Function close_wrlt() &
          Result (fclose)
!
! Frees memory for a weather roulette diagram
!
! Modules
   Use scores, Only: wrlts
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
   If (Allocated(wrlts)) Deallocate (wrlts)
   fclose = 0
!
   Return
 End Function close_wrlt
#if GUI == 1
!
!
!
 Function title_verif() &
          Result (title)
!
! Modules
   Use plotting, Only: get_title
!
! Function result
   Integer :: title
!
! Executable Statements
!
! Prompt for title
   title = replot_verif(get_title)
!
   Return
 End Function title_verif
!
!
!
 Function replot_verif(get_custom) &
          Result (replot)
!
! Modules
   Use plotting,    Only: replot_graph
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: replot
!
! Arguments
!
! Dummy arguments
! - procedure arguments -
   Interface
      Subroutine get_custom (img, lreset)
         Use graphics, Only: image
         Type(image), Intent(InOut) :: img ! - image -
         Logical, Intent(Out) :: lreset ! - reset? -
      End Subroutine get_custom
   End Interface
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Replot customised verification diagrams
   If (Any(img_rels(:)%id == imgid)) Then
      ireld = imgid + 1 - img_rels(1)%id
      replot = replot_graph(img_rels(ireld), get_custom, plot_reld)
! - ranked hits -
   Else If (img_rhit(1)%id == imgid) Then
      replot = replot_graph(img_rhit(1), get_custom, update_rhit)
! - ROC -
   Else If (img_rocs(1)%id == imgid) Then
      replot = replot_graph(img_rocs(1), get_custom, plot_roc_verif)
! - tendency -
   Else If (img_tend(1)%id == imgid) Then
      replot = replot_graph(img_tend(1), get_custom, update_tend)
! - profits -
   Else If (img_wrlt(1)%id == imgid) Then
      replot = replot_graph(img_wrlt(1), get_custom, plot_wrlt)
   End If
!
   Return
 End Function replot_verif
!
!
!
 Function export_verif() &
          Result (export)
!
! Modules
   Use graphics,    Only: save_graphic
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: export
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Prompt for montage title
   If (imgid < 0) Then
      export = export_montage_verif(-imgid)
      Return
   End If
!
! Save graph
! - attributes -
   If (Any(img_rels(:)%id == imgid)) Then
      ireld = imgid + 1 - img_rels(1)%id
      export = save_graphic('Reliability_diagram', img_rels(ireld))
! - ranked hits -
   Else If (img_rhit(1)%id == imgid) Then
      export = save_graphic('Ranked_hits_diagram', img_rhit(1))
! - ROC -
   Else If (img_rocs(1)%id == imgid) Then
      export = save_graphic('ROC_diagram', img_rocs(1))
! - tendency -
   Else If (img_tend(1)%id == imgid) Then
      export = save_graphic('Tendency_diagram', img_tend(1))
! - profits -
   Else If (img_wrlt(1)%id == imgid) Then
      export = save_graphic('Profits', img_wrlt(1))
   End If
!
   Return
 End Function export_verif
!
!
!
 Function export_montage_verif(imgid) &
          Result (export)
!
! Modules
   Use graphics, Only: export_montage
!
! Function result
   Integer :: export
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: imgid ! - image ID -
!
! Executable Statements
!
! Save images
   If (img_rels(1)%id == imgid) Then
      export = export_montage('Attributes', img_rels(4), img_rels(3), &
               img_3=img_rels(2), img_4=img_rels(1))
   End If
!
   Return
 End Function export_montage_verif
!
!
!
 Function help_verif()
!
! Modules
   Use help,        Only: help_file
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: help_verif
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Any
!
! Executable Statements
!
! Open context-specific help pages
   If (Any(img_rels(:)%id == imgid)) Then
      Call help_file ('Tools_Verify_Attributes.html')
! - ranked hits -
   Else If (img_rhit(1)%id == imgid) Then
      Call help_file ('Tools_Verify_RHits.html')
! - ROC -
   Else If (img_rocs(1)%id == imgid) Then
      Call help_file ('Tools_Verify_ROC.html')
! - tendency -
   Else If (img_tend(1)%id == imgid) Then
      Call help_file ('Tools_Verify_Tendency.html')
! - profits -
   Else If (img_wrlt(1)%id == imgid) Then
      Call help_file ('Tools_Verify_WeatherRoulette.html')
   End If
   help_verif = 2
!
   Return
 End Function help_verif
#endif
!
!
!
 Subroutine proj_read_verif (iin, ifail)
!
! Reads verification settings from project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin ! - input file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read verification settings
   Read (Unit=iin, Err=1, End=2) irv
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
 End Subroutine proj_read_verif
!
!
!
 Subroutine proj_write_verif (iout, ifail)
!
! Writes verification settings to project file
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem writing to file
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output file unit number -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write verification settings
   Write (Unit=iout, Err=1) irv
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_verif
End Module verif
