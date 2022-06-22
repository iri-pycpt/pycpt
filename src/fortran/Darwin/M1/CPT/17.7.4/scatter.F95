! Author: Simon Mason
Module scatter
!
! Modules
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: i_handle
   Use data_io_constants,  Only: lstn
#if GUI == 1
   Use graphics,           Only: image
#endif
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: regress
#if GUI == 1
   Public :: init_img_scatter, scatter_cv, scatter_c2, scatter_ra
#endif
!
! Scalars
!
! Integer scalars
   Integer, Private :: ioutr0 ! - regression plot output unit number -
   Integer, Private :: ioutr1 ! - regression plot output unit number -
#if GUI == 1
   Integer, Private :: iouts0 ! - scatter plot output unit number -
#endif
!
! Real scalars
   Real(Kind=rp), Private :: b0regr ! - regression constant -
!
   Integer(Kind=i_handle), Private :: ih_r1  ! - regression equations window handle -
!
! Character scalars
   Character(Len=lstn), Private :: crxaxis ! - regression X-axis label -
   Character(Len=lstn), Private :: cryaxis ! - regression Y-axis label -
!
#if GUI == 1
! Arrays
!
! Derived type arrays
   Type(image), Dimension(2), Private :: img_cv ! - cross-validated graphs -
   Type(image), Dimension(2), Private :: img_c2 ! - double cross-validated graphs -
   Type(image), Dimension(2), Private :: img_ra ! - retroactive graphs -
   Type(image), Dimension(2), Private :: img_rg ! - regression scatter plot -
!
#endif
Contains
!
!
#if GUI == 1
 Subroutine init_img_scatter (igsz1, igsz2)
!
! Initialises scatter and regression plots
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
! Executable Statements
!
! Initialise graph sizes
   Call init_image (img_cv(1), igsz1)
   Call init_image (img_cv(2), igsz1)
   Call init_image (img_c2(1), igsz1)
   Call init_image (img_c2(2), igsz1)
   Call init_image (img_ra(1), igsz1)
   Call init_image (img_ra(2), igsz1)
   Call init_image (img_rg(1), igsz2)
   Call init_image (img_rg(2), igsz2)
!
   Return
 End Subroutine init_img_scatter
!
!
!
 Function scatter_cv() &
          Result (scatters)
!
! Plots cross-validated scatter graphs
!
! Modules
   Use labels, Only: l_cvn
!
! Function result
   Integer :: scatters
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot cross-validated graphs
   scatters = scatters_plot(Trim(l_cvn%c), img_cv(:), plot_cvs)
!
   Return
 End Function scatter_cv
!
!
!
 Function scatter_c2() &
          Result (scatters)
!
! Plots double cross-validated scatter graphs
!
! Modules
   Use labels, Only: l_cvn
!
! Function result
   Integer :: scatters
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot double cross-validated graphs
   scatters = scatters_plot(Trim(l_cvn%c), img_c2(:), plot_c2s)
!
   Return
 End Function scatter_c2
!
!
!
 Function scatter_ra() &
          Result (scatters)
!
! Plots retroactive scatter graphs
!
! Modules
   Use labels, Only: l_retro
!
! Function result
   Integer :: scatters
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Plot cross-validated graphs
   scatters = scatters_plot(Trim(l_retro%c), img_ra(:), plot_ras)
!
   Return
 End Function scatter_ra
!
!
!
 Function scatters_plot(ctype, img_rsp, plot_scatter_func) &
          Result (scatters)
!
! Plots a residuals plot and a scatter diagram
!
! Modules
   Use labels,      Only: l_axislims, l_axislimsy, l_residual, l_scatter
   Use windows_gui, Only: imgid, &
                          init_window_graphic
!
! Function result
   Integer :: scatters
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ctype ! - validation type -
!
! - input/output arrays -
   Type(image), Dimension(:), Intent(InOut) :: img_rsp ! - residuals/scatter plots -
!
! - procedure arguments -
   Integer, External :: plot_scatter_func ! - plotting function -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set image titles
   img_rsp(1)%title = Trim(ctype)//' Residuals'
   img_rsp(2)%title = 'Observations and '//Trim(ctype)//' Hindcasts'
!
! Plot graphs
   scatters = init_window_graphic(2,img_rsp(:), imgid, 'Graphs', [l_residual, l_scatter], &
              plot_scatter_func, f_title, f_export,                                       &
              cxy='Y', iloc=iouts0, limits_grph=f_limits, &
              llims=[l_axislimsy, l_axislims], f_help=help_scatter)
   scatters = 1
!
   Return
 End Function scatters_plot
!
!
!
#endif
 Function regress()
!
! Displays regression results
!
#if GUI == 1
! Modules
   Use data_cpt_constants, Only: ia_gcm
   Use labels,             Only: l_lab, &
                                 l_axislimsy, l_regress
   Use iofiles,            Only: mxa
   Use windows_gui,        Only: imgid, &
                                 init_window_graphic_0, init_window_graphic
   Use analysis,           Only: ianal
#else
   Use gui,                Only: get_output_unit
   Use cpt_output,         Only: print_results
#endif
!
! Function result
   Integer :: regress
!
#if GUI == 1
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Set image title
   If (mxa == 1 .or. ianal == ia_gcm) Then
      img_rg(:)%title = 'Regression Plot'
!
! Plot graphs
      regress = init_window_graphic(1, img_rg(:), imgid, l_regress%c, [l_lab], &
                update_regr, f_title, f_export, &
                cxy='Y', iloc=ioutr0, add_win_v=show_regr, limits_grph=f_limits, llims=[l_axislimsy])
   Else
      regress = init_window_graphic_0(Trim(l_regress%c), update_regr, show_regr, &
                cxy='Y')
   End If
!
#else
! Print results
   ioutr0 = get_output_unit()
   regress = show_regr()
   regress = print_results(update_regr)
#endif
   regress = 1
!
   Return
 End Function regress
!
!
!
#if GUI == 1
 Function plot_cvs() &
          Result (plot)
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graphs
   plot = plot_res_cv()
   plot = plot_sct_cv()
!
   Return
 End Function plot_cvs
!
!
!
 Function plot_res_cv() &
          Result (plot)
!
! Modules
   Use data_time_constants, Only: iuseq
   Use time,                Only: iseq
   Use labels,              Only: ca_seq
   Use settings,            Only: iva, nt, nu
   Use arrays,              Only: res=>rwk2, y
   Use iofiles,             Only: yfile
   Use regression,          Only: ycv
   Use season,              Only: isnc
   Use missing,             Only: ktuse
   Use plotting,            Only: plot_line_graph
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Calculate residuals
   Allocate (res(nu,1))
   res(:,1) = ycv(iva,1:nu,isnc) - y(iva,1:nu,isnc)
!
! Update graphs
   plot = plot_line_graph(img_cv(1), Trim(ca_seq(iuseq(iseq))), 'Hindcast - Observation', .false., nt, 1, res(:,:), &
          d1=yfile%fdate, kuse=ktuse(:))
   Deallocate (res)
!
   Return
 End Function plot_res_cv
!
!
!
 Function plot_sct_cv() &
          Result (plot)
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: iva, nu
   Use arrays,             Only: y
   Use regression,         Only: ycv
   Use season,             Only: isnc
   Use categories,         Only: tobs
   Use plotting,           Only: plot_scatter
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graphs
   plot = plot_scatter(img_cv(2), 'Hindcast', 'Observed', iouts0, nu, nts, ycv(iva,:,isnc), y(iva,:,isnc), .true., &
          tobs=tobs(iva,:))
!
   Return
 End Function plot_sct_cv
!
!
!
 Function plot_c2s() &
          Result (plot)
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graphs
   plot = plot_res_c2()
   plot = plot_sct_c2()
!
   Return
 End Function plot_c2s
!
!
!
 Function plot_res_c2() &
          Result (plot)
!
! Modules
   Use data_time_constants, Only: iuseq
   Use time,                Only: iseq
   Use labels,              Only: ca_seq
   Use settings,            Only: iva, nt, nu
   Use arrays,              Only: res=>rwk2, y
   Use iofiles,             Only: yfile
   Use regression,          Only: ycv2
   Use season,              Only: isnc
   Use missing,             Only: ktuse
   Use plotting,            Only: plot_line_graph
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Calculate residuals
   Allocate (res(nu,1))
   res(:,1) = ycv2(iva,1:nu,isnc) - y(iva,1:nu,isnc)
!
! Update graphs
   plot = plot_line_graph(img_cv(1), Trim(ca_seq(iuseq(iseq))), 'Hindcast - Observation', .false., nt, 1, res(:,:), &
          d1=yfile%fdate, kuse=ktuse(:))
   Deallocate (res)
!
   Return
 End Function plot_res_c2
!
!
!
 Function plot_sct_c2() &
          Result (plot)
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: iva, nu
   Use arrays,             Only: y
   Use regression,         Only: ycv2
   Use season,             Only: isnc
   Use categories,         Only: tobs
   Use plotting,           Only: plot_scatter
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graphs
   plot = plot_scatter(img_c2(2), 'Hindcast', 'Observed', iouts0, nu, nts, ycv2(iva,:,isnc), y(iva,:,isnc), .true., &
          tobs=tobs(iva,:))
!
   Return
 End Function plot_sct_c2
!
!
!
 Function plot_ras() &
          Result (plot)
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graphs
   plot = plot_res_ra()
   plot = plot_sct_ra()
!
   Return
 End Function plot_ras
!
!
!
 Function plot_res_ra() &
          Result (plot)
!
! Modules
   Use data_time_constants, Only: iuseq
   Use time,                Only: Operator(+), &
                                  iseq
   Use labels,              Only: ca_seq
   Use settings,            Only: iva, ntr, nt1, nu, nur, nu1
   Use arrays,              Only: res=>rwk2, y
   Use iofiles,             Only: yfile
   Use regression,          Only: yret
   Use season,              Only: isnc
   Use missing,             Only: ktuse
   Use plotting,            Only: plot_line_graph
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Calculate residuals
   Allocate (res(nur,1))
   res(:,1) = yret(iva,1:nur,isnc) - y(iva,nu1+1:nu,isnc)
!
! Update graphs
   plot = plot_line_graph(img_ra(1), Trim(ca_seq(iuseq(iseq))), 'Hindcast - Observation', .false., ntr, 1, res(:,:), &
          d1=yfile%fdate+(nt1-1), kuse=ktuse(nt1:))
   Deallocate (res)
!
   Return
 End Function plot_res_ra
!
!
!
 Function plot_sct_ra() &
          Result (plot)
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: iva, nu, nur, nu1
   Use arrays,             Only: y
   Use regression,         Only: yret
   Use season,             Only: isnc
   Use categories,         Only: tobs
   Use plotting,           Only: plot_scatter
!
! Function result
   Integer :: plot
!
! Executable Statements
!
! Update graphs
   plot = plot_scatter(img_ra(2), 'Hindcast', 'Observed', iouts0, nur, nts, yret(iva,:,isnc), y(iva,nu1+1:nu,isnc), .true., &
          tobs=tobs(iva,:))
!
   Return
 End Function plot_sct_ra
!
!
!
 Function plot_reg_scatter() &
          Result (plot)
!
! Modules
   Use data_cpt_constants, Only: nts
   Use settings,           Only: ilink, iva, nu
   Use arrays,             Only: x, y
   Use regression,         Only: b
   Use season,             Only: isnc
   Use axes,               Only: x_axis
   Use plotting,           Only: plot_scatter
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Update graph
   plot = plot_scatter(img_rg(1), Trim(crxaxis), Trim(cryaxis), ioutr0, nu, nts, x(1,:,1), y(iva,:,isnc), .false., &
          b0=b0regr, b1=b(1,iva), ilink=ilink, xmin=x_axis%amin, xmax=x_axis%amax, ymin=img_rg(1)%ymin, ymax=img_rg(1)%ymax)
!
   Return
 End Function plot_reg_scatter
!
!
!
 Function plot_regr() &
          Result (plot)
!
! Modules
   Use data_cpt_constants, Only: il_lgm, it_non, nts
   Use settings,           Only: igauss, ilink, iva, nu
   Use arrays,             Only: t, x, y
   Use regression,         Only: b
   Use season,             Only: isnc
   Use plotting,           Only: plot_scatter
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Update graphs
   Select Case (igauss)
    Case (it_non) ! - no transformation -
      If (ilink /= il_lgm) Then
         plot = plot_scatter(img_rg(1), Trim(crxaxis), Trim(cryaxis), ioutr0, nu, nts, x(1,:,1), y(iva,:,isnc), .false., &
                b0=b0regr, b1=b(1,iva), ilink=ilink)
      Else
         plot = plot_scatter(img_rg(1), Trim(crxaxis), Trim(cryaxis), ioutr0, nu, nts, x(1,:,1), y(iva,:,isnc), .false., &
                b0=b0regr, b1=b(1,iva), t=t(:), ilink=ilink)
      End If
    Case Default
      plot = plot_scatter(img_rg(1), Trim(crxaxis), Trim(cryaxis), ioutr0, nu, nts, x(1,:,1), y(iva,:,isnc), .false., &
             b0=b0regr)
   End Select
!
   Return
 End Function plot_regr
!
!
!
#endif
 Function show_regr() &
          Result (show)
!
! Modules
   Use data_cpt_constants, Only: ia_gcm
   Use labels,             Only: cg_regreqs
   Use gui,                Only: box_close, box_open, open_clearwin
#if GUI == 1
   Use gui,                Only: get_handle
#endif
   Use iofiles,            Only: mxa, mya
   Use analysis,           Only: ianal
!
! Function result
   Integer :: show
!
! Locals
!
! Local scalars
   Integer :: mlmx ! - maximum number of lines -
!
   Logical :: lhs ! - add horizontal scroll bar? -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Max, Min, Trim
!
! Executable Statements
!
! Print regression equations
   CALL box_open (.true., .false., &
        title=Trim(cg_regreqs))
! - open Clearwin+ windows -
#if GUI == 1
   ih_r1 = get_handle()
#endif
   If (mxa == 1 .or. ianal == ia_gcm) Then
      lhs = .false.
      mlmx = 2
   Else
      lhs = .true.
      mlmx = Min(Max(2, mya), 41)
   End If
   If (mya <= mlmx) Then
      CALL open_clearwin (88, mlmx, .true., ioutr1, &
           ih=ih_r1, lhs=lhs)
   Else
      CALL open_clearwin (88, mlmx, .true., ioutr1, &
           ih=ih_r1, maxl=mya, lhs=lhs, lvs = .true.)
   End If
   CALL box_close (.true.)
   show = 1
!
   Return
 End Function show_regr
!
!
!
 Function update_regr() &
          Result (update)
!
! Updates Tools ~ Regression results and graphs
!
! Modules
   Use data_cpt_constants, Only: ia_gcm, id_grd
#if GUI == 1
   Use labels,             Only: cg_na, cg_undefined_l
#endif
   Use gui,                Only: print_text, set_cursor_waiting
   Use settings,           Only: ilink, iva
   Use space,              Only: cstndx
   Use iofiles,            Only: mxa, mya, xfile
   Use fields,             Only: iffx, yfield, &
                                 update_grid
   Use regression,         Only: b0
   Use cpt_output,         Only: update_results
   Use analysis,           Only: ianal
!
! Function result
   Integer :: update
!
! Functions and Subroutines
!
! Intrinsic functions
#if GUI == 1
   Intrinsic Len_Trim
#endif
   Intrinsic Trim
!
! Executable Statements
!
! Identify regression constant
   Select Case (ianal)
    Case Default
      b0regr = b0(iva,1)
    Case (ia_gcm)
      b0regr = b0(1,iva)
   End Select
!
! Construct and add coordinate label for initial point, and regression components
   CALL set_cursor_waiting (1)
   If (xfile%idstr /= id_grd) Then
      crxaxis = Trim(cstndx(1,iffx))
   Else
      crxaxis = 'X'
   End If
   cryaxis = Trim(yfield(1)%var)
   If (mxa == 1 .or. ianal == ia_gcm) Then
      CALL update_grid (ioutr0)
!
! Update regression equation
      CALL print_text (' ', &
           iout=ioutr1)
      update = print_regr(ioutr1, ilink, Trim(cryaxis))
   Else If (mya > 1) Then
      update = update_results(ioutr1, ih_r1, 1, ' ', .false., print_regs)
   Else
      update = print_regr(ioutr1, ilink, Trim(cryaxis))
   End If
   CALL set_cursor_waiting (0)
!
#if GUI == 1
! Plot graph
   If (mxa == 1 .or. ianal == ia_gcm) Then
      If (Len_Trim(yfield(1)%unitc) > 0) Then
         If (Trim(yfield(1)%unitc) /= cg_na .and. Trim(yfield(1)%unitc) /= 'N/A' .and. &
             Trim(yfield(1)%unitc) /= Trim(cg_undefined_l) .and. Trim(yfield(1)%unitc) /= 'undefined') Then
            cryaxis = Trim(cryaxis)//' ('//Trim(yfield(1)%unitc)//')'
         End If
      End If
      update = plot_regr()
   End If
!
#endif
   update = 1
!
   Return
 End Function update_regr
!
!
!
 Function print_regs(iout, i2, i3, istn, ifld) &
          Result (printr)
!
! Prints regression equation with Y field information
!
! Modules
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use settings,           Only: ilink
   Use space,              Only: coor, cstndy
   Use iofiles,            Only: yfile
!
! Function result
   Integer :: printr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: istn ! - station index -
   Integer, Intent(In) :: ifld ! - field index -
!
! - dummy arguments -
   Integer, Intent(In) :: i2 ! - second index (included to match argument list) -
   Integer, Intent(In) :: i3 ! - third index (included to match argument list) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Print regression equation
   Select Case (yfile%idstr)
    Case (id_grd)
      printr = print_regr(iout, ilink, Trim(coor))
    Case (id_stn, id_ind)
      printr = print_regr(iout, ilink, Trim(cstndy(istn,ifld)))
   End Select
!
   Return
 End Function print_regs
!
!
!
 Function print_regr(iout, ilink, cryaxis) &
          Result (printr)
!
! Prints regression equation
!
! Modules
   Use data_numbers,       Only: rp, zero, one
   Use data_cpt_constants, Only: ia_gcm, ia_pcr, il_idt, il_inv, il_lgt, il_lgm, il_log, il_sqr
   Use settings,           Only: iprecf, iva
   Use space,              Only: cstndx
   Use iofiles,            Only: mxa
   Use fields,             Only: iffx
   Use pcs,                Only: bz, nxo
   Use regression,         Only: b
   Use analysis,           Only: ianal
!
! Function result
   Integer :: printr
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output unit number -
   Integer, Intent(In) :: ilink ! - link function -
!
#if GUI == 1
   Character(Len=lstn), Intent(In) :: cryaxis ! - regression Y-axis label -
#else
   Character(Len=*), Intent(In) :: cryaxis ! - regression Y-axis label -
#endif
!
! Locals
!
! Local scalars
   Integer :: i ! - predictor index -
   Integer :: m ! - number of predictors -
!
   Real(Kind=rp) :: s  ! - sign -
   Real(Kind=rp) :: sb ! - regression coefficient multiplied by sign -
!
   Character(Len=32) :: cfmt ! - format statement -
   Character(Len=32) :: ceqn ! - equation component -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Abs, AdjustL, Trim
!
! Executable Statements
!
! Print regression equation
! - LHS -
   Write (Unit=iout, Fmt='(A)', Advance='no') Trim(cryaxis)
   Write (Unit=iout, Fmt='(A)', Advance='no') ' = '
! - inverse-link function -
   Select Case (ilink)
    Case (il_idt) ! - identity -
      s = one
    Case (il_lgt) ! - logistic -
      Write (Unit=iout, Fmt='(A)', Advance='no') ' 1 / [ 1 + exp [ '
      s = -one
    Case (il_lgm) ! - multinomial logistic -
      Write (Unit=iout, Fmt='(A)', Advance='no') ' t / [ 1 + exp [ '
      s = -one
    Case (il_log) ! - logarithmic -
      Write (Unit=iout, Fmt='(A)', Advance='no') ' exp [ '
      s = one
    Case (il_inv) ! - inverse -
      Write (Unit=iout, Fmt='(A)', Advance='no') ' 1 / [ '
      s = one
    Case (il_sqr) ! - square root -
      Write (Unit=iout, Fmt='(A)', Advance='no') ' [ '
      s = one
   End Select
! - RHS -
   Write (Unit=cfmt, Fmt='(A,I1,A)') '(F11.', iprecf, ')'
   Select Case (ianal)
    Case Default
      Write (Unit=ceqn, Fmt=cfmt) s*b0regr
    Case (ia_gcm)
      Write (Unit=ceqn, Fmt=cfmt) s*b0regr
   End Select
   Write (Unit=iout, Fmt='(A)', Advance='no') Trim(AdjustL(ceqn))
   Select Case (ianal)
    Case Default
      m = mxa
    Case (ia_pcr)
      m = nxo
    Case (ia_gcm)
      m = 1
   End Select
   Do i = 1, m
      Select Case (ianal)
       Case Default
         sb = s*b(i,iva)
         Write (Unit=ceqn, Fmt=cfmt) Abs(b(i,iva))
       Case (ia_pcr)
         sb = s*bz(i,iva)
         Write (Unit=ceqn, Fmt=cfmt) Abs(bz(i,iva))
      End Select
      If (sb < zero) Then
         Write (Unit=iout, Fmt='(A)', Advance='no') '  -  '
      Else
         Write (Unit=iout, Fmt='(A)', Advance='no') '  +  '
      End If
      Select Case (ianal)
       Case Default
         Write (Unit=iout, Fmt='(3A)', Advance='no') Trim(AdjustL(ceqn)), ' * ', Trim(AdjustL(cstndx(i,iffx)))
       Case (ia_pcr)
         Write (Unit=iout, Fmt='(2A,I0)', Advance='no') Trim(AdjustL(ceqn)), ' * PC', i
       Case (ia_gcm)
         Write (Unit=iout, Fmt='(2A)', Advance='no') Trim(AdjustL(ceqn)), ' * X'
      End Select
   End Do
! - close link function -
   Select Case (ilink)
    Case (il_idt) ! - identity -
      Write (Unit=iout, Fmt='()', Advance='yes')
    Case (il_lgt, il_lgm) ! - logistic -
      Write (Unit=iout, Fmt='(A)', Advance='yes') ' ] ]'
    Case (il_log, il_inv) ! - logarithmic and inverse -
      Write (Unit=iout, Fmt='(A)', Advance='yes') ' ] '
    Case (il_sqr) ! - square root -
      Write (Unit=iout, Fmt='(A)', Advance='yes') ' ] ** 2'
   End Select
   printr = 0
!
   Return
 End Function print_regr
!
!
!
#if GUI == 1
 Function f_title() &
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
   title = replot_scatter(get_title)
!
   Return
 End Function f_title
!
!
!
 Function f_limits() &
          Result (limits)
!
! Modules
   Use plotting, Only: get_limits
!
! Function result
   Integer :: limits
!
! Executable Statements
!
! Prompt for limits
   limits = replot_scatter(get_limits)
!
   Return
 End Function f_limits
!
!
!
 Function replot_scatter(get_custom) &
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
      Subroutine get_custom (img,lreset)
         Use graphics, Only: image
         Type(image), Intent(InOut) :: img ! - image -
         Logical, Intent(Out) :: lreset ! - reset? -
      End Subroutine get_custom
   End Interface
!
! Executable Statements
!
! Replot customised scatter diagrams
! - cross-validated residuals diagram -
   If (img_cv(1)%id == imgid) Then
      replot = replot_graph(img_cv(1), get_custom, plot_res_cv)
! - cross-validated scatter diagram -
   Else If (img_cv(2)%id == imgid) Then
      replot = replot_graph(img_cv(2), get_custom, plot_sct_cv)
! - retroactive residuals diagram -
   Else If (img_ra(1)%id == imgid) Then
      replot = replot_graph(img_ra(1), get_custom, plot_res_ra)
! - retroactive scatter diagram -
   Else If (img_ra(2)%id == imgid) Then
      replot = replot_graph(img_ra(2), get_custom, plot_sct_ra)
! - regression scatter diagram -
   Else If (img_rg(1)%id == imgid) Then
      replot = replot_graph(img_rg(1), get_custom, plot_reg_scatter)
   End If
!
   Return
 End Function replot_scatter
!
!
!
 Function f_export() &
          Result (export)
!
! Modules
   Use settings,    Only: iv
   Use iofiles,     Only: ny
   Use graphics,    Only: save_graphic
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: export
!
! Executable Statements
!
! Prompt for montage title
   If (imgid<0) Then
      export = f_export_montage(-imgid)
      Return
   End If
!
! Save graph
! - cross-validated residuals diagram -
   If (img_cv(1)%id == imgid) Then
      export = save_graphic('Residuals_cv', img_cv(1), &
               imap=iv, mmap=ny)
! - cross-validated scatter diagram -
   Else If (img_cv(2)%id == imgid) Then
      export = save_graphic('Scatter_cv', img_cv(2), &
               imap=iv, mmap=ny)
! - retroactive residuals diagram -
   Else If (img_ra(1)%id == imgid) Then
      export = save_graphic('Residuals_retro', img_ra(1), &
               imap=iv, mmap=ny)
! - retroactive scatter diagram -
   Else If (img_ra(2)%id == imgid) Then
      export = save_graphic('Scatter_retro', img_ra(2), &
               imap=iv, mmap=ny)
! - regression scatter diagram -
   Else If (img_rg(1)%id == imgid) Then
      export = save_graphic('Regress_plot', img_rg(1), &
               imap=iv, mmap=ny)
   End If
!
   Return
 End Function f_export
!
!
!
 Function f_export_montage(imgid) &
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
   If (img_cv(1)%id == imgid) Then
      export = export_montage('Residuals_cv', img_cv(1), img_cv(2))
   Else If (img_ra(1)%id == imgid) Then
      export = export_montage('Residuals_ra', img_ra(1), img_ra(2))
   End If
!
   Return
 End Function f_export_montage
!
!
!
 Function help_scatter()
!
! Modules
   Use help,        Only: help_file
   Use windows_gui, Only: imgid
!
! Function result
   Integer :: help_scatter
!
! Executable Statements
!
! Open context-specific help pages
   If ((img_cv(1)%id == imgid) .or. (img_ra(1)%id == imgid)) Then
     Call help_file ('Tools_Validate_Scatter.html')
   End If
   help_scatter = 2
!
   Return
 End Function help_scatter
#endif
End Module scatter
