! Author: Simon Mason
Module plotting
!
#if GUI == 1
! Modules
   Use data_gui_constants, Only: mtitle
   Use graphics,           Only: image
!
#endif
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
#if GUI == 1
   Public :: get_limits, get_title, plot_attrib, plot_exceed, plot_line_graph, plot_rhit, plot_roc, plot_scatter, plot_series, &
             plot_tend, plot_xdata, plot_ydata, replot_graph
!
! Scalars
!
! Character scalars
   Character(Len=mtitle), Private :: title_old ! - backup image title -
!
! Logical scalars
   Logical, Private :: llimits ! - set limits? -
!
Contains
!
!
 Function plot_xdata(img, icols, ioff, ncb, x, c, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Plots data on the X domain
!
! Modules
   Use data_numbers, Only: rp
   Use arrays,       Only: iusex
   Use iofiles,      Only: xfile
   Use space,        Only: rlatdx, rlngdx
   Use fields,       Only: iffx, ilimx0, ilimx1, ilimx2, xfield
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icols ! - colour scale -
   Integer, Intent(In) :: ioff  ! - offset -
   Integer, Intent(In) :: ncb   ! - number of colour bars -
!
   Character(Len=*), Intent(In) :: c ! - title -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - X data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - minimum value -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - maximum value -
!
! Executable Statements
!
! Plot X data
   plot = plot_data(img, xfile%idstr, iffx, ilimx0, ilimx1, ilimx2, icols, ioff, ncb, iusex, xfield, rlatdx, rlngdx, x, c, &
          vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
!
   Return
 End Function plot_xdata
!
!
!
 Function plot_ydata(img, icols, ioff, ncb, y, c, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Plots data on the Y domain
!
! Modules
   Use data_numbers, Only: rp
   Use arrays,       Only: iusey
   Use iofiles,      Only: yfile
   Use space,        Only: rlatdy, rlngdy
   Use fields,       Only: iffy, ilimy0, ilimy1, ilimy2, yfield
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icols ! - colour scale -
   Integer, Intent(In) :: ioff  ! - offset -
   Integer, Intent(In) :: ncb   ! - number of colour bars -
!
   Character(Len=*), Intent(In) :: c ! - title -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - Y data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - minimum value -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - maximum value -
!
! Executable Statements
!
! Plot Y data
   plot = plot_data(img, yfile%idstr, iffy, ilimy0, ilimy1, ilimy2, icols, ioff, ncb, iusey, yfield, rlatdy, rlngdy, y, c, &
          vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
!
   Return
 End Function plot_ydata
!
!
!
 Function plot_data(img, idstr, iff, ilim0, ilim1, ilim2, icols, ioff, ncb, iuse, afield, rlatd, rlngd, z, c, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Modules
   Use data_numbers,       Only: rp
   Use data_cpt_constants, Only: id_grd, id_stn, id_ind
   Use gui,                Only: set_cursor_waiting
   Use fields,             Only: field
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: idstr ! - data structure -
   Integer, Intent(In) :: iff   ! - field counter -
   Integer, Intent(In) :: ilim0 ! - limit -
   Integer, Intent(In) :: ilim1 ! - limit -
   Integer, Intent(In) :: ilim2 ! - limit -
   Integer, Intent(In) :: icols ! - colour scale -
   Integer, Intent(In) :: ioff  ! - offset -
   Integer, Intent(In) :: ncb   ! - number of colour bars -
!
   Character(Len=*), Intent(In) :: c ! - title -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - indices of used Y variables -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: z ! - data -
!
   Type(field), Dimension(:), Intent(In) :: afield ! - field data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! - pointer arguments -
   Real(Kind=rp), Dimension(:,:), Intent(In), Pointer :: rlatd ! - latitudes within domain -
   Real(Kind=rp), Dimension(:,:), Intent(In), Pointer :: rlngd ! - longitudes within domain -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - minimum value -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - maximum value -
!
! Executable Statements
!
! Update skill map
   Select Case (idstr)
    Case (id_grd)
      plot = plot_map (img, afield(iff), rlatd(:,iff), rlngd(:,iff), z(ilim1:ilim2), &
             iuse(ilim1:ilim2)-ilim0, icols, ioff, ncb, .true., &
             vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
    Case (id_stn)
      plot = plot_stn (img, afield(iff), rlatd(:,iff), rlngd(:,iff), z(ilim1:ilim2), &
             iuse(ilim1:ilim2)-ilim0, icols, ioff, ncb, .true., &
             vlow=vlow,vhgh=vhgh,vmin=vmin,vmax=vmax)
    Case (id_ind)
      plot = plot_bar (img, afield(iff), z(ilim1:ilim2), &
             iuse(ilim1:ilim2)-ilim0, c, &
             vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax)
   End Select
!
! Reset cursor
   Call set_cursor_waiting (0)
!
   Return
 End Function plot_data
!
!
!
 Function plot_map(img, afield, rlat, rlng, v, iuse, icols, ioff, ncb, lnclr, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Draws a gridded map
!
! Modules
   Use clrwin$,      Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers, Only: rp
   Use gui,          Only: icol, icoli
   Use fields,       Only: field
   Use graphics,     Only: draw_map
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icols ! - colour scaling indicator -
   Integer, Intent(In) :: ioff  ! - colour offset -
   Integer, Intent(In) :: ncb   ! - number of colour bars -
!
   Logical, Intent(In) :: lnclr ! - compute number of colours? -
!
   Type(field), Intent(In) :: afield ! - field -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: v    ! - data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - absolute minimum -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - absolute maximum -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Plot map
   icoli = icols
   ifail = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_map (img, iuse, v, afield%region%nlts, afield%region%nlgs, afield%nva,                         &
        afield%region%alim%rltn, afield%region%alim%rlts, afield%region%alim%rlgw, afield%region%alim%rlge, &
        lnclr, 1, ncb, rlat, rlng, ifail,                                                                   &
        ioff=ioff, vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax, rmiss=afield%rmiss)
   Call perform_graphics_update$ ()
   plot = 1
!
   Return
 End Function plot_map
!
!
!
 Function plot_stn(img, afield, rlat, rlng, v, iuse, icols, ioff, ncb, lnclr, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Draws a station map
!
! Modules
   Use clrwin$,      Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers, Only: rp
   Use gui,          Only: icol, icoli
   Use fields,       Only: field
   Use graphics,     Only: draw_map
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: icols ! - colour scaling indicator -
   Integer, Intent(In) :: ioff  ! - colour offset -
   Integer, Intent(In) :: ncb   ! - number of colour bars -
!
   Logical, Intent(In) :: lnclr ! - compute number of colours? -
!
   Type(field), Intent(In) :: afield ! - field -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - used variables -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: rlat ! - latitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: rlng ! - longitudes -
   Real(Kind=rp), Dimension(:), Intent(In) :: v    ! - data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - absolute minimum -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - absolute maximum -
!
! Locals
!
! Local scalars
   Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Plot map (stations)
   icoli = icols
   ifail = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_map (img, iuse, v, afield%region%nlts, afield%region%nlgs, afield%nva,                         &
        afield%region%alim%rltn, afield%region%alim%rlts, afield%region%alim%rlgw, afield%region%alim%rlge, &
        lnclr, 2, ncb, rlat, rlng, ifail,                                                                   &
        ioff=ioff, vlow=vlow, vhgh=vhgh, vmin=vmin, vmax=vmax, rmiss=afield%rmiss)
   Call perform_graphics_update$ ()
   plot = 1
!
   Return
 End Function plot_stn
!
!
!
 Function plot_bar(img, afield, v, iuse, ylab, &
          vlow, vhgh, vmin, vmax) &
          Result (plot)
!
! Draws a bar graph
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use fields,             Only: field
   Use graphics,           Only: draw_bar
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: ylab ! - y-axis label -
!
   Type(field), Intent(In) :: afield ! - field -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: iuse ! - indices of used variables -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Real(Kind=rp), Intent(In), Optional :: vlow ! - lowest value -
   Real(Kind=rp), Intent(In), Optional :: vhgh ! - highest value -
   Real(Kind=rp), Intent(In), Optional :: vmin ! - absolute minimum -
   Real(Kind=rp), Intent(In), Optional :: vmax ! - absolute maximum -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Plot bar chart
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_bar (img, afield%nv, afield%nva, v, iuse, ylab, &
        ylow=vlow, yhgh=vhgh, ymin=vmin, ymax=vmax)
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_bar
!
!
!
 Function plot_series(img, cylab, n1, nu1, y, d1, n2, nu2, y2, d2, i2, &
          df, nf, mf, yf, yfis, y2is, nse) &
          Result (plot)
!
! Draws a time series of observations and hindcasts, and optionally forecasts
!
! Modules
   Use data_numbers,        Only: rp
   Use data_cpt_constants,  Only: nts
   Use data_time_constants, Only: iuseq
   Use maths,               Only: interval
   Use time,                Only: pdate
   Use labels,              Only: ca_seq
   Use iofiles,             Only: yfile
   Use settings,            Only: iva
   Use missing,             Only: kfuse, ktuse
   Use categories,          Only: tobs
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n1  ! - number of observations -
   Integer, Intent(In) :: nu1 ! - number of used observations -
   Integer, Intent(In) :: n2  ! - number of hindcasts -
   Integer, Intent(In) :: nu2 ! - number of used hindcasts -
   Integer, Intent(In) :: i2  ! - offset for hindcasts -
!
   Character(Len=*), Intent(In) :: cylab ! - y-axis label -
!
   Type(pdate), Intent(In) :: d1 ! - first date of observations -
   Type(pdate), Intent(In) :: d2 ! - first date of hindcasts -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: y  ! - observed values -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: y2 ! - hindcast values -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: mf  ! - number of new data variables -
   Integer, Intent(In), Optional :: nf  ! - number of new data -
   Integer, Intent(In), Optional :: nse ! - number of subseasons -
!
   Type(pdate), Intent(In), Optional :: df ! - first date of forecasts -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: yf ! - forecasts -
!
   Type(interval), Dimension(:), Intent(In), Optional :: yfis ! - prediction intervals -
   Type(interval), Dimension(:), Intent(In), Optional :: y2is ! - hindcast intervals -
!
! Locals
!
! Local scalars
   Character(Len=16) :: cxlab ! - x-axis label -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Reshape, Trim
!
! Executable Statements
!
! Determine appropriate x-axis label
   If (yfile%iseq < 0) Then
      cxlab = ca_seq(iuseq(yfile%iseq))
   Else
      cxlab = ca_seq(iuseq(0))
   End If
!
! Update graph
   If (Present(yf)) Then
      plot = plot_line_graph(img, Trim(cxlab), cylab, .false., n1, 1, Reshape(y(:), [nu1, 1]), &
             nse=nse, nts=nts, tobs=tobs(iva,:), d1=d1, kuse=ktuse(:),                 &
             d2=d2, n2=n2, m2=1, ilw2=2, k2use=ktuse(i2:), y2=Reshape(y2(:),[nu2, 1]), &
             df=df, nf=nf, mf=mf, icolf=3, kfuse=kfuse(:), yf=yf(:,:), yfis=yfis)
   Else
      plot = plot_line_graph(img, Trim(cxlab), cylab, .false., n1, 1, Reshape(y(:), [nu1, 1]), &
             nse=nse, nts=nts, tobs=tobs(iva,:), d1=d1, kuse=ktuse(:), &
             d2=d2, n2=n2, m2=1, ilw2=2, k2use=ktuse(i2:), y2=Reshape(y2(:), [nu2, 1]), y2is=y2is)
   End If
   plot = 1
!
   Return
 End Function plot_series
!
!
!
 Function plot_line_graph(img, cxlab, cylab, laxis, nt, m, y, &
          ix1, d1, kuse,                           &
          ix2, d2, k2use, n2, m2, ilw2, y2, y2is,  &
          ixf, df, kfuse, nf, mf, icolf, yf, yfis, &
          ivert, nse, nts, tobs, ylow, yhgh, ymin, ymax) &
          Result (plot)
!
! Plots a graph
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use maths,              Only: interval
   Use time,               Only: pdate
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use graphics,           Only: draw_line_graph
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nt ! - number of cases -
   Integer, Intent(In) :: m  ! - number of lines -
!
   Character(Len=*), Intent(In) :: cxlab ! - x-axis label -
   Character(Len=*), Intent(In) :: cylab ! - y-axis label -
!
   Logical, Intent(In) :: laxis ! - logarithmic axis? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - y-data -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: icolf ! - colour of forecast crosses -
   Integer, Intent(In), Optional :: ilw2  ! - width of added line -
   Integer, Intent(In), Optional :: ivert ! - plot vertical lines flag -
   Integer, Intent(In), Optional :: ix1   ! - first X value -
   Integer, Intent(In), Optional :: ix2   ! - first X value for overlay data -
   Integer, Intent(In), Optional :: ixf   ! - first X value for new data -
   Integer, Intent(In), Optional :: m2    ! - number of overlay variables -
   Integer, Intent(In), Optional :: n2    ! - number of values, overlay variables -
   Integer, Intent(In), Optional :: mf    ! - number of new data variables -
   Integer, Intent(In), Optional :: nf    ! - number of new data -
   Integer, Intent(In), Optional :: nse   ! - number of subseasons -
   Integer, Intent(In), Optional :: nts   ! - number of thresholds -
!
   Real(Kind=rp), Intent(In), Optional :: ylow ! - lowest value (soft limit) -
   Real(Kind=rp), Intent(In), Optional :: yhgh ! - highest value (soft limit) -
   Real(Kind=rp), Intent(In), Optional :: ymin ! - minimum value (hard limit) -
   Real(Kind=rp), Intent(In), Optional :: ymax ! - maximum value (hard limit) -
!
   Type(pdate), Intent(In), Optional :: d1 ! - first date on x-axis -
   Type(pdate), Intent(In), Optional :: d2 ! - first date for overlay data -
   Type(pdate), Intent(In), Optional :: df ! - first date for new data -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tobs ! - thresholds -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: y2 ! - overlay data -
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: yf ! - new data -
!
   Logical, Dimension(:), Intent(In), Optional :: kuse  ! - cases used? -
   Logical, Dimension(:), Intent(In), Optional :: k2use ! - overlay cases used? -
   Logical, Dimension(:), Intent(In), Optional :: kfuse ! - new cases used? -
!
   Type(interval), Dimension(:), Intent(In), Optional :: y2is ! - hindcast intervals -
   Type(interval), Dimension(:), Intent(In), Optional :: yfis ! - intervals on new data -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Plot graph
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_line_graph (img, cxlab, cylab, laxis, nt, m, y(:,:), &
        ix1=ix1, d1=d1, kuse=kuse,                                                &
        ix2=ix2, d2=d2, k2use=k2use, n2=n2, m2=m2, ilw2=ilw2, y2=y2, y2is=y2is,   &
        ixf=ixf, df=df, kfuse=kfuse, nf=nf, mf=mf, icolf=icolf, yf=yf, yfis=yfis, &
        ivert=ivert, nse=nse, nts=nts, tobs=tobs, ylow=ylow, yhgh=yhgh, ymax=ymax, ymin=ymin)
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_line_graph
!
!
!
 Function plot_exceed(img, cxlab, cylab, nep, f, e, c, nclim, clim, &
          nts, tobs) &
          Result (plot)
!
! Plots probability of exceedance graph
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$, select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use graphics,           Only: draw_exceed
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nep   ! - number of exceedance probabilities -
   Integer, Intent(In) :: nclim ! - number of climatological data -
!
   Character(Len=*), Intent(In) :: cxlab ! - x-axis label -
   Character(Len=*), Intent(In) :: cylab ! - y-axis label -
!
! - input arrays -
   Real(Kind=rp), Dimension( :), Intent(In) :: clim ! - climatological series (sorted in ascending order) -
   Real(Kind=rp), Dimension(0:), Intent(In) :: f    ! - probabilities of exceedance given forecast -
   Real(Kind=rp), Dimension(0:), Intent(In) :: e    ! - empirical probabilities of exceedance -
   Real(Kind=rp), Dimension(0:), Intent(In) :: c    ! - climatological probabilities of exceedance -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: nts ! - number of thresholds -
!
! - optional inputs arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tobs ! - observed thresholds; shape(n) -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Plot probabilities of exceedance
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call set_cursor_waiting (0)
   Call set_cursor_waiting (1)
   Call draw_exceed (img, cxlab, cylab, nclim, clim(:), nep, f(:), e(:), c(:), &
        nts=nts, tobs=tobs)
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_exceed
!
!
!
 Function plot_roc(img, n, na, hit, far, roca) &
          Result (plot)
!
! Draws an ROC diagram
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use labels,             Only: ls_cats
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use graphics,           Only: draw_roc
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n  ! - number of cases -
   Integer, Intent(In) :: na ! - number of areas -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: roca ! - ROC areas -
!
   Real(Kind=rp), Dimension(:,:), Intent(In) :: hit ! - hit rates -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: far ! - false-alarm rates -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - ROC diagram -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Update ROC graphs
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Select Case (na)
    Case (2) ! - outer categories only -
      Call draw_roc (img, n, na, hit, far, roca(1:na), [ls_cats(1)%c,ls_cats(3)%c])
    Case (3) ! - all categories -
      Call draw_roc (img, n, na, hit, far, roca(1:na), ls_cats(1:)%c)
   End Select
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_roc
!
!
!
 Function plot_scatter(img, cxaxis, cyaxis, iout, n, nts, x, y, leqal, &
          ilink, b0, b1, t, tobs, xmin, xmax, ymin, ymax) &
          Result (plot)
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use fields,             Only: update_grid
   Use regression,         Only: fit_regr1
   Use graphics,           Only: draw_scatter
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
   Integer, Intent(In) :: n    ! - number of cases -
   Integer, Intent(In) :: nts  ! - number of thresholds -
!
   Character(Len=*), Intent(In) :: cxaxis ! - X-axis label -
   Character(Len=*), Intent(In) :: cyaxis ! - Y-axis label -
!
   Logical, Intent(In) :: leqal ! - equal axis limits? -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: x ! - x data; shape(n) -
   Real(Kind=rp), Dimension(:), Intent(In) :: y ! - y data; shape(n) -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - scatter plot -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: ilink ! - link function -
!
   Real(Kind=rp), Intent(In), Optional :: b0   ! - regression constant -
   Real(Kind=rp), Intent(In), Optional :: b1   ! - regression coefficient -
   Real(Kind=rp), Intent(In), Optional :: xmin ! - absolute minimum x-value -
   Real(Kind=rp), Intent(In), Optional :: xmax ! - absolute maximum x-value -
   Real(Kind=rp), Intent(In), Optional :: ymin ! - absolute minimum y-value -
   Real(Kind=rp), Intent(In), Optional :: ymax ! - absolute maximum y-value -
!
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: t    ! - binomial coefficients -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: tobs ! - observed thresholds; shape(n) -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: b0l ! - regression constant -
   Real(Kind=rp) :: b1l ! - regression coefficient -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Present
!
! Executable Statements
!
! Construct and add coordinate label for initial point
   Call update_grid (iout)
!
! Update scatter graph
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   If (Present(b0)) Then
      Call draw_scatter (img, cxaxis, cyaxis, n, nts, x(:), y(:), leqal, &
           t=t, tobs=tobs, ilink=ilink, b0=b0, b1=b1, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
   Else
      Call fit_regr1 (n, x(:), y(:), b0l, b1l)
      Call draw_scatter (img, cxaxis, cyaxis, n, nts, x(:), y(:), leqal, &
           t=t, tobs=tobs, ilink=ilink, b0=b0l, b1=b1l, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
   End If
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_scatter
!
!
!
 Function plot_attrib(img, nb, icat, tend, b0, b1, ifq, afp, orf, bs) &
          Result (plot)
!
! Plots an attributes diagram
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$,perform_graphics_update$,select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use labels,             Only: ca_bss
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use graphics,           Only: draw_rel
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nb   ! - number of probability bins -
   Integer, Intent(In) :: icat ! - category number -
!
   Real(Kind=rp), Intent(In) :: tend ! - tendency -
   Real(Kind=rp), Intent(In) :: b0   ! - resolution intercept -
   Real(Kind=rp), Intent(In) :: b1   ! - resolution slope -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: ifq ! - forecast frequencies -
!
   Real(Kind=rp), Dimension(:), Intent(In) :: afp ! - average forecast probabilities (X values) -
   Real(Kind=rp), Dimension(:), Intent(In) :: orf ! - observed relative frequencies (Y values) -
   Real(Kind=rp), Dimension(:), Intent(In) :: bs  ! - Brier score and skill score -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - scatter plot -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Real, Sum
!
! Executable Statements
!
! Plot attributes diagram
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_rel (img, nb, afp(:), orf(:), Real(100*ifq(:), Kind=rp)/Real(Sum(ifq(:)), Kind=rp), &
        tend, b0, b1, ca_bss(icat,:), bs(:), icol(icat+6))
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_attrib
!
!
!
 Function plot_rhit(img, ng, nt, fdate, rhit, kuse) &
          Result (plot)
!
! Plots a ranked hits diagram
!
! Modules
   Use clrwin$,             Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers,        Only: rp
   Use data_time_constants, Only: iuseq
   Use labels,              Only: ca_seq
   Use time,                Only: pdate
   Use gui,                 Only: icol, &
                                  set_cursor_waiting
   Use iofiles,             Only: yfile
   Use graphics,            Only: draw_rhit
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of categories -
   Integer, Intent(In) :: nt ! - number of cases -
!
   Type(pdate), Intent(In) :: fdate ! - initial date -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: rhit ! - ranked hits -
!
   Logical, Dimension(:), Intent(In) :: kuse ! - used cases -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - scatter plot -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int, Trim
!
! Executable Statements
!
! Plot profits diagram
   Call set_cursor_waiting (1)
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_rhit (img, ng, nt, fdate, rhit(:,:), kuse(:), Trim(ca_seq(iuseq(yfile%iseq))))
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_rhit
!
!
!
 Function plot_tend(img, ng, tends) &
          Result (plot)
!
! Plots a tendency diagram
!
! Modules
   Use clrwin$,            Only: draw_filled_rectangle$, perform_graphics_update$, select_graphics_object$
   Use data_numbers,       Only: rp
   Use data_gui_constants, Only: icol_gr
   Use labels,             Only: ca_cat_a
   Use gui,                Only: icol, icoli, &
                                 set_cursor_waiting
   Use graphics,           Only: draw_tend
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ng ! - number of categories -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: tends ! - tendencies -
!
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - scatter plot -
!
! Function result
   Integer :: plot
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Int
!
! Executable Statements
!
! Plot attributes diagram
   Call set_cursor_waiting (1)
   icoli = icol_gr
   plot = select_graphics_object$(Int(img%ihandle))
   Call draw_filled_rectangle$ (1, 1, img%isize, img%isize, icol(0))
   Call draw_tend (img, ng, ca_cat_a, tends(:,:))
   Call perform_graphics_update$ ()
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function plot_tend
!
!
!
 Subroutine get_title (img, ltitle)
!
! Prompts for a graph title
!
! Modules
   Use gui, Only: iw, &
                  gui_creturn, init_win, prompt_cvalue, win_prompt
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! - output scalars -
   Logical, Intent(Out) :: ltitle ! - title reset? -
!
! Executable Statements
!
! Prompt for graph title
   title_old = img%title
   Call init_win (title='Graph title')
   Call gui_creturn (.false.)
   Call prompt_cvalue ('Graph title', img%title, &
        iwid=42)
!
! Confirm settings
   iw = win_prompt(.true.)
   If (iw==1) Then
      ltitle = .true.
      llimits = img%lcontour
      img%lcontour = .true.
   Else
      img%title = title_old
      ltitle = .false.
   End If
!
   Return
 End Subroutine get_title
!
!
!
 Subroutine get_limits (img, llimit)
!
! Prompts for contour or axis limits
!
! Modules
   Use data_numbers, Only: rp, zero, one
   Use labels,       Only: cg_limits, cg_valmax, cg_valmin, &
                           l_reset
#if GUI == 1
   Use labels,       Only: c_lab, &
                           make_label
#endif
   Use gui,          Only: iw, &
                           init_win, prompt_real, win_prompt
   Use graphics,     Only: contours
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! - output scalars -
   Logical, Intent(Out) :: llimit ! - limits reset? -
!
! Locals
!
! Local scalars
   Real(Kind=rp) :: zmin ! - minimum value -
   Real(Kind=rp) :: zmax ! - maximum value -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Prompt for limits
   zmax = img%ymax/img%scale
   zmin = img%ymin/img%scale
   Call init_win (title=Trim(cg_limits))
   Call prompt_real (Trim(cg_valmax), zmax, &
        itab=20, iwid=10, rfly=one, lfocus=.true., lbold=.true.)
   Call prompt_real (Trim(cg_valmin), zmin, &
        itab=20, iwid=10, rfly=one, lfocus=.true., lbold=.true.)
!
! Confirm settings
#if GUI == 1
   c_lab = make_label('T', l_reset, &
           lak=.true.)
   iw = win_prompt(.true., &
        cextra=c_lab)
#else
   iw = win_prompt(.true., &
        cextra=l_reset%c)
#endif
   Select Case (iw)
    Case (1)
      img%ymin = zmin*img%scale
      img%ymax = zmax*img%scale
      llimit = .true.
      llimits = .true.
      Call contours (img, &
           zmin=zmin, zmax=zmax)
    Case (2)
      img%ymin = zero
      img%ymax = zero
      img%scale = one
      llimit = .true.
      llimits = .false.
    Case Default
      llimit = .false.
      llimits = img%lcontour
   End Select
   img%lcontour = llimits
!
   Return
 End Subroutine get_limits
!
!
!
 Function replot_graph(img, fmod, replot) &
          Result (plot)
!
! Modules
   Use gui, Only: set_cursor_waiting
!
! Function result
   Integer :: plot
!
! Arguments
!
! Dummy arguments
! - input/output scalars -
   Type(image), Intent(InOut) :: img ! - image -
!
! - procedure arguments -
   Interface
      Subroutine fmod (img, lmod)
         Use graphics, Only: image
         Type(image), Intent(InOut) :: img ! - image -
         Logical, Intent(Out) :: lmod ! - modification? -
      End Subroutine fmod
   End Interface
   Interface
      Function replot()
         Integer :: replot
      End Function replot
   End Interface
!
! Locals
!
! Local scalars
   Logical :: lmod ! - modification? -
!
! Executable Statements
!
! Prompt for modifications
   Call fmod (img, lmod)
   If (lmod) Then
      Call set_cursor_waiting (1)
      plot = replot()
      img%lcontour = llimits
   End If
   Call set_cursor_waiting (0)
   plot = 1
!
   Return
 End Function replot_graph
#endif
End Module plotting
