! Author: Simon Mason
Module errors
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: cpt_error, init_errorl, query
!
! Scalars
!
! Character scalars
   Character(Len=32), Public :: cproc ! - procedure in error -
!
Contains
!
!
 Subroutine init_errorl ()
!
! Initialises error labels
!
! Modules
   Use labels, Only: cg_error
!
! Executable Statements
!
! Initialise error labels
   cg_error = 'Error'
!
   Return
 End Subroutine init_errorl
!
!
!
 Subroutine cpt_error (cproc, lwait, ifail, &
            i_arg1, i_arg2, i_arg3, r_arg1, c_arg1, c_arg2, c_arg3, c_arg4)
!
! Prints an error message corresponding to routine cproc and error indicator ifail
!
! Modules
   Use data_numbers,        Only: sp
   Use data_version,        Only: cpt_email
   Use data_time_constants, Only: iuseq
   Use maths,               Only: get_cordn
   Use labels,              Only: ca_ftype, ca_seq, ca_seq_l, ca_seqs_l
   Use gui,                 Only: print_advisory, print_error, print_warning, prompt_yesno, set_cursor_waiting
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: cproc ! - procedure -
!
   Logical, Intent(In) :: lwait ! - set to cursor waiting? -
!
! - input/output scalars -
   Integer, Intent(InOut) :: ifail ! - error number -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: i_arg1 ! - integer argument -
   Integer, Intent(In), Optional :: i_arg2 ! - integer argument -
   Integer, Intent(In), Optional :: i_arg3 ! - integer argument -
!
   Real(Kind=sp), Intent(In), Optional :: r_arg1 ! - real argument -
!
   Character(Len=*), Intent(In), Optional :: c_arg1 ! - character argument -
   Character(Len=*), Intent(In), Optional :: c_arg2 ! - character argument -
   Character(Len=*), Intent(In), Optional :: c_arg3 ! - character argument -
   Character(Len=*), Intent(In), Optional :: c_arg4 ! - character argument -
!
! Locals
!
! Local scalars
   Character(Len=1024) :: msg1 ! - error message -
   Character(Len=1024) :: msg2 ! - error message -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Present, Trim
!
! Executable Statements
!
! Identify error
   Call set_cursor_waiting (0)
   Select Case (cproc)
    Case ('actions_calculate')
      Select Case (ifail)
       Case (1)
         Call print_error ('Length of training period exceeds available data. Length of training period has been reset.')
       Case (4)
         Call print_error ('Insufficient memory for '//c_arg1)
       Case (5)
         Call print_error ('Insufficient memory for double cross-validation.')
       Case Default
         GoTo 1
      End Select
!
    Case ('attrib_diagram')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct attributes diagram')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_bounds')
      Select Case (ifail)
       Case (1, 2, 3)
         Select Case (ifail)
          Case (1)
            msg1 = 'Data contain negative values when zero-bound is set'
          Case (2)
            msg1 = 'Data contain negative values when trying to transform using gamma distribution'
          Case (3)
            msg1 = 'Data contain invalid values when trying to use '//Trim(c_arg3)
         End Select
         If (i_arg1 < 0) Then
            msg2 = 'Data contain values lower than the lower limit:'
         Else If (i_arg1 > 0) Then
            msg2 = 'Data contain values higher than the upper limit:'
         End If
         If (Present(c_arg4)) Then
            If (Len_Trim(c_arg4) > 0) Then
               msg2 = Trim(msg2)//' invalid value found at '//Trim(c_arg2)//' in '//Trim(c_arg4)
            Else
               msg2 = Trim(msg2)//' invalid value found at '//Trim(c_arg2)
            End If
         Else
            msg2 = Trim(msg2)//' invalid value found at '//Trim(c_arg2)
         End If
         Call print_error (Trim(msg1), &
              msg2=Trim(msg2), &
              msg3=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_dates')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not in sequence', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_ddom')
      Select Case (ifail)
       Case (1)
         Call print_error ('Southernmost X latitude is north of northernmost latitude.')
       Case (2)
         Call print_error ('Southernmost Y latitude is north of northernmost latitude.')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_gridded_block_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent ''cpt:'//Trim(c_arg3)//''' tag', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent '//Trim(c_arg3), &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_n')
      Select Case (ifail)
       Case (1)
         Write (Unit=msg1, Fmt='(A,I0,A)') 'Length of training period must be at least ', i_arg1, '.'
         Call print_error (Trim(msg1))
       Case (2)
         Write (Unit=msg1, Fmt='(A,I0,A)') &
               'Retroactive calculations can only be performed if training period is at least ', i_arg1, '.'
         Call print_error (Trim(msg1))
       Case (3)
         Call print_warning (.true., 'Length of training period must be greater than cross-validation window length + 2.', &
              msg2='Cross-validation window length has been reset.')
       Case (4)
         Write (msg1, Fmt='(A,I0,A)') 'Retroactive calculations can only be performed if training period is at least ', i_arg1, &
                                      ' more than length of cross-validation window.'
         Call print_warning (.true., Trim(msg1), &
              msg2='Cross-validation window length has been reset.')
       Case (5)
         Call print_error ('Too many missing cases. Try adjusting the missing value settings.')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_new_file')
      Select Case (ifail)
       Case (1)
         Call print_error ('Invalid file name. Please try another.')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_nt1')
      Select Case (ifail)
       Case (1)
         Write (msg1, Fmt='(A,I0,A)') &
              'Length of initial training period must be at least cross-validation window length + ', i_arg1, '.'
#if GUI == 1
         Call print_error (Trim(msg1), &
              msg2='Initial length of training period has been reset.')
#else
         ifail = 1 - prompt_yesno(.true., .true., .false., Trim(msg1), &
                     msg2='Initial length of training period has been reset. OK?')
#endif
       Case Default
         GoTo 1
      End Select
!
    Case ('check_pcs')
      Select Case (ifail)
       Case (1)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Minimum number of X EOF modes must not be more than number of cases-1.', &
                              msg2='Minimum number of X EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (2)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Minimum number of X EOF modes must not be more than number of variables.', &
                              msg2='Minimum number of X EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (3)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of X EOF modes must not be less than minimum number.', &
                              msg2='Maximum number of X EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (4)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of X EOF modes must not be more than number of cases - '//&
                             &'length of cross-validation window - 1.', &
                              msg2='Maximum number of X EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (5)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of X EOF modes must not be more than number of variables.', &
                              msg2='Maximum number of X EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (6)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Minimum number of Y EOF modes must not be more than number of cases - '//&
                             &'length of cross-validation window - 1.', &
                              msg2='Minimum number of Y EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (7)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Minimum number of Y EOF modes must not be more than number of variables.', &
                              msg2='Minimum number of Y EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (8)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of Y EOF modes must not be less than minimum number.', &
                              msg2='Maximum number of Y EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (9)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of Y EOF modes must not be more than number of cases - '//&
                             &'length of cross-validation window - 1.', &
                              msg2='Maximum number of Y EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (10)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of Y EOF modes must not be more than number of variables.', &
                              msg2='Maximum number of Y EOF modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (11)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Minimum number of CCA modes must not be more than minimum number of X EOF modes.', &
                              msg2='Minimum number of CCA modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (12)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Minimum number of CCA modes must not be more than minimum number of Y EOF modes.', &
                              msg2='Minimum number of CCA modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (13)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of CCA modes must not be less than minimum number.', &
                              msg2='Maximum number of CCA modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (14)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of CCA modes must not be more than maximum number of X EOF modes.', &
                              msg2='Maximum number of CCA modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (15)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of CCA modes must not be more than maximum number of Y EOF modes.', &
                              msg2='Maximum number of CCA modes has been reset.', &
                              msg3='Do you wish to continue?')
       Case (16)
         ifail = prompt_yesno(.true., .true., .false., &
                              'Maximum number of X modes + maximum number of Y modes exceeds number of cases - '//&
                             &'length of cross-validation window.', &
                              msg2='Spurious CCA modes are likely. It is recommended that:', &
                              msg3='1. the advanced CCA option be changed to ignore over-fitted modes, and/or', &
                              msg4='2. the maximum number of X and / or Y modes has been reduced.', &
                              msg5='Do you wish to continue anyway?')
       Case Default
         GoTo 1
      End Select
      ifail = 1 - ifail
!
    Case ('check_pprd')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent lengths of seasons', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent length of period', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid start date for dekad', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid end date for dekad', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Date is inconsistent with earlier dates', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_read')
      Select Case (ifail)
       Case (1)
         Write (msg1, Fmt='(A,I0)') 'Insufficient data in X file. Number of cases available for training period: ', i_arg1
         Call print_error (Trim(msg1))
       Case (2)
         Write (msg1, Fmt='(A,I0)') 'Insufficient data in Y file. Number of cases available for training period: ', i_arg1
         Call print_error (Trim(msg1))
       Case (3)
         Call print_error ('First date of Y data is before first date of X data.')
       Case (4)
         Call print_error ('Lag between first date of Y data and first date of X data is too large.')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_refdates')
      Select Case (ifail)
       Case (1)
         Call print_advisory &
             ('First reference year has been reset.')
       Case (2)
         Call print_advisory &
             ('Second reference year has been reset.')
       Case (3)
         Call print_advisory &
             ('Reference years have been reset.')
       Case (4)
         Call print_advisory &
             ('First reference year is unavailable. Year has been reset.')
       Case (5)
         Call print_advisory &
             ('Second reference year is unavailable. Year has been reset.')
       Case (6)
         Call print_advisory &
             ('Reference years are unavailable. Years have been reset.')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_xyfiles')
      Select Case (ifail)
       Case (1)
         Call print_error ('The '//Trim(c_arg1)//' input file is incompatible with the '//Trim(c_arg2)//' file')
       Case (2)
         Call print_error ('File sequencing must be consistent', &
                           msg2=Trim(ca_seq(iuseq(i_arg1)))//' sequencing in '//c_arg3, &
                           msg3=Trim(ca_seq(iuseq(i_arg2)))//' sequencing in '//c_arg4)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_xfile')
      Select Case (ifail)
       Case (4)
         Call print_error ('Insufficient number of '//Trim(ca_seqs_l(iuseq(i_arg1)))//' in file:', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_yfile')
      Select Case (ifail)
       Case (1)
         Call print_error ('Multi-model Y input files cannot be used for '//c_arg1)
       Case (2)
         Call print_error ('Probabilistic Y input files are not permitted.')
       Case (4)
         Call print_error ('Multi-field or multi-lagged input files cannot be used for '//c_arg1)
       Case (5)
         Call print_error ('Insufficient number of '//Trim(ca_seqs_l(iuseq(i_arg1)))//' in file:', &
              msg2=c_arg1)
       Case (6)
         Call print_error ('Insufficient number of '//Trim(ca_seqs_l(iuseq(i_arg1)))//' in Y file to calculate a climatology:', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('check_zfile')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Problem allocating memory', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call print_error ('The forecast file must be in the same CPT version format as the X file.', &
#if GUI == 1
              msg2='%bfForecast file:  %`bf'//c_arg1, &
              msg3='%bfX file:  %`bf'//c_arg2)
#else
              msg2='Forecast file:  '//c_arg1, &
              msg3='X file:         '//c_arg2)
#endif
       Case (3)
         Call print_error ('The forecast file must be in the same format as the X file.', &
#if GUI == 1
              msg2='%bfForecast file:  %`bf'//ca_ftype(i_arg1), &
              msg3='%bfX file:  %`bf'//ca_ftype(i_arg2))
#else
              msg2='Forecast file:  '//ca_ftype(i_arg1), &
              msg3='X file:         '//ca_ftype(i_arg2))
#endif
       Case (4)
         If (i_arg1 > 0 .and. i_arg2 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A,I0,A)') '''cpt:'//c_arg2//''' tags are inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field and the ', &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         Else If (i_arg1 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A)') '''cpt:'//c_arg2//''' tags are inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field in file:'
         Else
            Write (Unit=msg1, Fmt='(A,I0,A)') '''cpt:'//c_arg2//''' tags are inconsistent with X input file for the ', &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         End If
         Call print_error (Trim(msg1), &
              msg2=c_arg1)
       Case (5)
         If (i_arg1 > 0 .and. i_arg2 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A,I0,A)') 'Number of '//c_arg2//' is inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field and the ', &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         Else If (i_arg1 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A)') 'Number of '//c_arg2//' is inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field in file:'
         Else
            Write (Unit=msg1, Fmt='(A,I0,A)') 'Number of '//c_arg2//' is inconsistent with X input file for the ', &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         End If
         Call print_error (Trim(msg1), &
              msg2=c_arg1)
       Case (6)
         If (i_arg1 > 0 .and. i_arg2 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A,I0,A)') 'Latitude ordering is inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field and the ',                                            &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         Else If (i_arg1 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A)') 'Latitude ordering is inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field in file:'
         Else
            Write (Unit=msg1, Fmt='(A,I0,A)') 'Latitude ordering is inconsistent with X input file for the ', &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         End If
         Call print_error (Trim(msg1), &
              msg2=c_arg1)
       Case (7)
         If (i_arg1 > 0 .and. i_arg2 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A,I0,A)') c_arg2//' are inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field and the ',                                    &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         Else If (i_arg1 > 0) Then
            Write (Unit=msg1, Fmt='(A,I0,A)') c_arg2//' are inconsistent with X input file for the ', &
                   i_arg1, get_cordn(i_arg1)//' field in file:'
         Else
            Write (Unit=msg1, Fmt='(A,I0,A)') c_arg2//' are inconsistent with X input file for the ', &
                   i_arg2, get_cordn(i_arg2)//' lag in file:'
         End If
         Call print_error (Trim(msg1), &
              msg2=c_arg1)
       Case (8)
         Call print_error ('Season is inconsistent with X input file in file:', &
              msg2=c_arg1)
       Case (9)
         Call print_error ('Sub-season is inconsistent with X input file in file:', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('download_data')
      Select Case (ifail)
       Case (1)
         Call print_error ('Local file must be specified with its full pathname.', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('draw_map')
      Select Case (ifail)
       Case (1)
         Call print_error &
             ('Problem reading boundaries file. Country boundaries incomplete.', &
               msg2='Try reinstalling boundaries file in the data subdirectory.', email=.true.)
       Case (2)
         Call print_error &
             ('The requested boundaries file does not exist in the data subdirectory. Country boundaries not drawn.', &
              msg2='Reinstall boundaries file in the data subdirectory.', email=.true.)
       Case (3)
         Call print_error &
             ('Problem reading lakes file. Lakes incomplete.', &
              msg2='Try reinstalling lakes file in the data subdirectory.', email=.true.)
       Case (4)
         Call print_error &
             ('The requested lakes file does not exist in the data subdirectory. Lakes not drawn.', &
              msg2='Reinstal lakes file in the data subdirectory.', email=.true.)
       Case (5)
         Call print_error ('Problem opening colours file.', &
              msg2='Try reinstalling the colours file in the data subdirectory.', email=.true.)
       Case Default
         GoTo 1
      End Select
!
    Case ('file_version')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening file:', &
              msg2=c_arg1, &
              msg3='Cannot find file. Try checking file path and name.')
       Case (2)
         Call print_error ('Problem opening file:', &
              msg2=c_arg1, &
              msg3='File is protected. Try closing file in other applications.')
       Case (3)
         Call print_error ('Problem opening file:', &
              msg2=c_arg1)
       Case (4)
         Call print_error ('Problem reading file:', &
              msg2=c_arg1)
       Case (5)
         Write (Unit=msg2, Fmt='(A,I0,A)') 'File version number ', i_arg1, ' is invalid.'
         Call print_error ('Problem reading file:', &
              msg2=c_arg1, &
              msg3=Trim(msg2))
       Case Default
         GoTo 1
      End Select
!
    Case ('get_area')
      Select Case (ifail)
       Case (1)
         Call print_error ('Southern domain limit is north of northern domain limit.', &
              msg2='Latitudinal limits have been reset. Please check new settings.')
       Case (2)
         Call print_error ('Northern domain limit is south of southernmost data.', &
              msg2='Northern domain limit has been reset. Please check new settings.')
       Case (3)
         Call print_error ('Southern domain limit is north of northernmost data.', &
              msg2='Southern domain limit has been reset. Please check new settings.')
       Case (4)
         Call print_error ('Longitudinal extent is too small.', &
              msg2='Longitudinal limits have been reset. Please check new settings.')
       Case (5)
         Call print_error ('Invalid longitudinal extent.', &
              msg2='Longitudinal limits have been reset. Please check new settings.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_cols')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening colours file.', &
              msg2='Try reinstalling the colours file in the data subdirectory.', email=.true.)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_dataset')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening download initialization file.', &
              msg2='File does not exist. Try reinstalling the file in the data subdirectory.')
       Case (2)
         Call print_error ('Problem opening download initialization file.', &
              msg2='File is protected. Try restarting CPT.', &
              msg3='If problem persists try closing other copies of CPT, or reinstall the file in the data subdirectory.')
       Case (3)
         Call print_error ('Problem opening download initialization file.', &
              msg2='Try reinstalling the file in the data subdirectory.')
       Case (4)
         Call print_error ('Problem reading download initialization file.', &
              msg2='Try reinstalling the file in the data subdirectory.')
       Case (5)
         Call print_error ('Problem reading download initialization file.', &
              msg2='Premature end of file. Try reinstalling the file in the data subdirectory.')
       Case (6)
         Call print_error ('Problem reading download initialization file.', &
              msg2='Unable to locate requested data type.')
       Case (7)
         Call print_error ('Problem reading download initialization file.', &
              msg2='Unable to locate requested variable.')
       Case (8)
         Call print_error ('Problem reading download initialization file.', &
              msg2='Unable to determine number of dataset sources.')
       Case (9)
         Call print_error ('Problem reading download initialization file.', &
              msg2='Missing data-source name.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_download_url')
      Select Case (ifail)
       Case (1)
         Call print_error ('No pressure levels have been selected.')
       Case (2)
         Call print_advisory &
             ('Last year of download must not be before first year of download.', &
              msg2='Last date of download has been modified.')
       Case (3)
         Call print_advisory &
             ('Initial download dates cannot be in the future.', &
              msg2='First date of download has been modified.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_forecast')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to produce forecast')
       Case (2)
         GoTo 1
       Case (3)
         Call print_error ('Insufficient data for number of forecasts.', &
              msg2='Try reducing the number of forecasts and/or decreasing the first date from which to forecast.')
       Case (4)
         Call print_error ('Insufficient persistence data for number of forecasts.', &
              msg2='Try reducing the number of forecasts and/or decreasing the first date from which to forecast.')
       Case (5)
         Call print_error ('Persistence data unavailable for first forecasts.', &
              msg2='Try using a later first date from which to forecast.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_block_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Missing ''cpt:T'' tag', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'The data structure must be gridded', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Category numbering is incorrect', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_dimensions_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Maximum file width reached. File may have been truncated.', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Number of '//Trim(c_arg3)//' appears to be zero.', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Problem reading the first line in the file.', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_fields_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Premature end of file', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Season is inconsistent with earlier dates.', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Climatological probability is inconsistent with that for earlier dates.', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent field information.', &
              i_arg1, i_arg2, i_arg3)
       Case (6)
         Call read_error_msg (c_arg1, c_arg2, 'Climatological probabilities do not add to 1 or 100.', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_info_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached.', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Unable to read '//Trim(c_arg3), &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not in sequence.', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_latlons_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Latitudes are not consecutive', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Latitudes are not consecutive. Try checking for duplicate latitudes', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_nls_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Categories are not consecutive', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Categories are not ordered', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent fields', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_gridded_nt_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2,Trim(c_arg3)//' are inconsistent with those for earlier dates', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2,Trim(c_arg3)//' sequencing of data is not permitted in CPT version 9 input files', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Problem with date format', &
              i_arg1, i_arg2, i_arg3)
       Case (6)
         Call read_error_msg (c_arg1, c_arg2, 'Problem allocating memory', &
              i_arg1, i_arg2, i_arg3)
       Case (7)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not in sequence', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_mode_settings')
      Select Case (ifail)
       Case (1)
         Call print_error ('Maximum number of '//c_arg1//' modes cannot be less than minimum number.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_nongridded_dimensions_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Problem reading the  file', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Maximum file width reached. File may have been truncated', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Number of '//Trim(c_arg3)//' appears to be zero', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_nongridded_fields_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Season for last date is inconsistent with earlier dates', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Categories are not in order', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, &
              'Field information for the current category is inconsistent with that for the first', &
              i_arg1, i_arg2, i_arg3)
       Case (6)
         Call read_error_msg (c_arg1, c_arg2, 'Starting date for different categories is inconsistent', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_nongridded_info_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Unable to read '//c_arg3, &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not consecutive', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent '//c_arg3, &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_nongridded_latlons_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid keyword', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_nongridded_nls_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_nongridded_nt_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', i_arg1, &
             i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, Trim(c_arg3)//' sequencing of data is not permitted in CPT version 9 input files', &
             i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Problem allocating memory', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not in sequence', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_old_file')
      Select Case (ifail)
       Case (1)
         Call print_error ('Invalid file name. CPT cannot read files with the %% character in the file name.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_pdate')
      Select Case (ifail)
       Case (1)
         GoTo 1
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Problem reading date', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid month', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid day of month', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid month and day of month', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_project')
      Select Case (ifail)
       Case (1)
         Call print_error ('Not a valid project file.')
       Case (2)
         Write (Unit=msg1, Fmt='(A,F6.2)') 'Project file is newer than CPT version. Project file version: ',r_arg1
         Call print_error (Trim(msg1))
       Case (3)
         If (Len_Trim(c_arg1) > 0) Then
            If (Present(r_arg1)) Then
               Write (Unit=msg2, Fmt='(A,F6.2)') 'Project file version: ',r_arg1
               Call print_error ('Problem reading '//Trim(c_arg1)//' in project file.', &
                    msg2=Trim(msg2))
            Else
               Call print_error ('Problem reading '//Trim(c_arg1)//' in project file.')
            End If
         Else
            Call print_error ('Problem reading project file.')
         End If
       Case (4)
         Call print_error ('End of project file reached.')
       Case (5)
         Call print_error ('Insufficient memory to initialise project settings.', &
              msg2='Contact '//cpt_email)
       Case (6)
         Call print_error ('CPT version 9 project files are not fully supported.', &
              msg2='Please re-open input files.')
       Case (7)
         Call print_error ('Current version of Project File is old.', &
              msg2='Project File cannot be read.')
       Case (8)
         Call print_error ('Problem opening input file.', &
              msg2='File is protected. Try closing input file in other applications.')
       Case (9)
         Call print_error ('Problem reading '//Trim(c_arg1)//' in project file.', &
              msg2='Monthly input files must be re-opened.')
       Case (10)
         Call print_error ('Problem reading '//Trim(c_arg1)//' in project file.', &
              msg2='Sub-seasonal input files must be re-opened.')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_seasons')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem allocating memory')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_sequence')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are identical', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not advancing', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Unrecognizable sequencing of dates', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Inconsistent length of season', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_structure_v9')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Problem allocating memory', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'File is not a valid CPT format', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_structure_v10')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, ' ', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'End of file reached', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Unrecognised header line', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Unrecognised line', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Problem allocating memory', &
              i_arg1, i_arg2, i_arg3)
       Case (6)
         Call read_error_msg (c_arg1, c_arg2, 'Season limits in the X and Y files are incompatible', &
              i_arg1, i_arg2, i_arg3)
       Case (7)
         Call read_error_msg (c_arg1, c_arg2, 'Dates are not in sequence', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('get_transform_opts')
      Select Case (ifail)
       Case (1)
         Call print_error ('Transformation option cannot be changed with SPI standardization on')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_xfile')
      Select Case (ifail)
       Case (1)
         Call print_error ('Unable to open monthly X files')
       Case Default
         GoTo 1
      End Select
!
    Case ('get_user')
      Select Case (ifail)
       Case (1)
         Call print_error ('Unable to create new user. Check permissions for '//c_arg1, &
              msg2='Try running CPT as administrator, ', &
              msg3='or copy CPT.ini from the installation directory into '//c_arg1, &
              email=.true.)
       Case Default
         GoTo 1
      End Select
!
    Case ('init_boot')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to perform bootstrapping')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_climate')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to calculate climatologies')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_correls')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to calculate correlations')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_cw')
      Select Case (ifail)
       Case (1)
         Write (msg1, Fmt='(A,I0,A)') 'Maximum number of windows requested is ', i_arg1, '.'
         Call print_error ('Insufficient memory for results windows.', &
              msg2=Trim(msg1))
       Case Default
         GoTo 1
      End Select
!
    Case ('init_dirs')
      Select Case (ifail)
       Case (1)
         Call print_error ('Unable to locate cpt.ini.', &
              msg2='Check that the CPT_BIN_DIR environment variable points to the directory where CPT.x is located.')
       Case (2)
         Call print_error ('Unable to locate cpt.ini.', &
              msg2='Try setting the CPT_BIN_DIR environment variable to the directory where CPT.x is located.')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_dscores')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to perform validation')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_fcast')
      Select Case (ifail)
       Case (3)
         Call print_warning (.true., &
              'First '//Trim(ca_seq_l(iuseq(i_arg1)))//' from which to forecast is before first '//&
             &Trim(ca_seq_l(iuseq(i_arg1)))//' in file.',                                          &
              msg2='First '//Trim(ca_seq_l(iuseq(i_arg1)))//' from which to forecast has been reset.')
       Case (4)
         Call print_error ('Unable to convert units of data in the forecast file. Check for consistency of units with the X file.')
       Case (5)
         Call print_error ('Too many missing values in order to calculate any of the forecasts.')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_labels')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening labels file.', &
              msg2='File does not exist. Try reinstalling the labels file in the data subdirectory.')
       Case (2)
         Call print_error ('Problem opening labels file.', &
              msg2='File is protected. Try restarting CPT.', &
              msg3='If problem persists try closing other copies of CPT, or reinstall the file in the data subdirectory.')
       Case (3)
         Call print_error ('Problem opening labels file.', &
              msg2='Try reinstalling the labels file in the data subdirectory.')
       Case (4)
         Call print_error ('Problem reading labels file.', &
              msg2='Try reinstalling the labels file in the data subdirectory.')
       Case (5)
         Call print_error ('Problem reading labels file.', &
              msg2='Premature end of file. Try reinstalling the labels file in the data subdirectory.')
       Case (6)
         Call print_error ('Problem reading labels file.', &
              msg2='Labels file is out of date. Try reinstalling the labels file in the data subdirectory.')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_perm')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to calculate p-values')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_pscores')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct skill maps')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_read')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to read data')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_skill')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct skill maps')
       Case Default
         GoTo 1
      End Select
!
    Case ('init_thresholds')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to calculate thresholds')
       Case Default
         GoTo 1
      End Select
!
    Case ('match_fields')
      Select Case (ifail)
       Case (1)
         Write (Unit=msg1, Fmt='(A,I0,A)') 'No matching X field for the ',        i_arg1, get_cordn(i_arg1)//' Y field:'
       Case (2)
         Write (Unit=msg1, Fmt='(A,I0,A)') 'Non-matching station names in the ', i_arg1, get_cordn(i_arg1)//' Y field:'
       Case (3)
         Write (Unit=msg1, Fmt='(A,I0,A)') 'Non-matching latitudes in the ',      i_arg1, get_cordn(i_arg1)//' Y field:'
       Case (4)
         Write (Unit=msg1, Fmt='(A,I0,A)') 'Non-matching longitudes in the ',     i_arg1, get_cordn(i_arg1)//' Y field:'
       Case Default
         GoTo 1
      End Select
#if GUI == 1
      Call print_error (Trim(msg1), &
           msg2='     Variable:%ta'//Trim(c_arg2), &
           msg3='     Season:%ta'//Trim(c_arg3))
#else
      Call print_error (Trim(msg1), &
           msg2='     Variable: '//Trim(c_arg2), &
           msg3='     Season:   '//Trim(c_arg3))
#endif
!
    Case ('merge_files')
      Select Case (ifail)
       Case (1)
         Call print_error ('Files must have the same structure:', &
              msg2='   X file structure:  '//Trim(c_arg1), &
              msg3='   Y file structure:  '//Trim(c_arg2))
       Case (2)
         Call print_error ('Files must have the same sequencing:', &
              msg2='   X file sequencing:  '//Trim(ca_seq(iuseq(i_arg1))), &
              msg3='   Y file sequencing:  '//Trim(ca_seq(iuseq(i_arg2))))
       Case Default
         GoTo 1
      End Select
!
    Case ('open_iofile')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening file:', &
              msg2=c_arg1, &
              msg3='Cannot find file. Try checking file path and name.')
       Case (2)
         Call print_error ('Problem opening file:', &
              msg2=c_arg1, &
              msg3='File is protected. Try closing file in other applications.')
       Case (3)
         Call print_error ('Problem opening file:', &
              msg2=c_arg1)
       Case (4)
         Call print_error ('File has been modified. Please re-open input file:', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('perform_CCA', 'perform_ACC', &
          'perform_PCR', 'perform_RCP', &
          'perform_MLR', 'perform_RLM', &
          'perform_GCM', 'perform_MCG', &
          'perform_PFV')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to perform '//c_arg1)
       Case (2)
         Call print_error ('Problem performing '//c_arg1)
       Case (3)
         Call print_error ('Y locations are outside the restricted X domain. Try extending the northernmost X limits.')
       Case (4)
         Call print_error ('Y locations are outside the restricted X domain. Try extending the southernmost X limits.')
       Case (5)
         Call print_error ('Y locations are outside the restricted X domain. Try extending the westernmost X limits.')
       Case (6)
         Call print_error ('Y locations are outside the restricted X domain. Try extending the easternmost X limits.')
       Case (7)
         Call print_error ('No matching X and Y grids with non-missing data.')
       Case Default
         GoTo 1
      End Select
!
    Case ('prob_scores')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to calculate probabilistic scores')
       Case Default
         GoTo 1
      End Select
!
    Case ('prompt_season')
      Select Case (ifail)
       Case (1)
         Call print_error ('Total length of season (including persistence and target periods) must not be longer than one year.', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('read_data')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem reading file:', &
              msg2=c_arg1, &
              msg3=c_arg2)
       Case (2)
         Call print_error ('End of file reached:', &
              msg2=c_arg1, &
              msg3=c_arg2)
       Case Default
         GoTo 1
      End Select
!
    Case ('read_predict')
      Select Case (ifail)
       Case (6)
         Call print_error ('Problem with the probabilities:', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('read_ini')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening '//c_arg1//' file.', &
              msg2='Check that '//c_arg1//' is installed in the CPT directory:', &
              msg3=c_arg2, email=.true.)
       Case (2)
         Call print_error ('Problem reading '//c_arg1//'.', &
              msg2='Try reinstalling '//c_arg1//' in the CPT directory.', email=.true.)
       Case (3)
         Call print_error ('Problem reading '//c_arg1//'.', &
              msg2='Try deleting '//c_arg1//' to reinitialize current user.', email=.true.)
       Case (4)
         Call print_error ('Problem reading '//c_arg1//'.', &
              msg2='File version is inconsistent with the current version of CPT.', &
              msg3='Try reinstalling '//c_arg1//' in the CPT directory.', email=.true.)
       Case Default
         GoTo 1
      End Select
!
    Case ('read_tags')
      Select Case (ifail)
       Case (-1)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid combination of tags', &
              i_arg1, i_arg2, i_arg3)
       Case (-2, -3)
         GoTo 1
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Problem reading ''cpt:'//c_arg3//''' tag', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Missing ''cpt:'//c_arg3//''' tag', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid date at ''cpt:S'' tag', &
              i_arg1, i_arg2, i_arg3)
       Case (4)
         Call read_error_msg (c_arg1, c_arg2, 'Value for ''cpt:row'' tag is incompatible with ''cpt:col=X''', &
              i_arg1, i_arg2, i_arg3)
       Case (5)
         Call read_error_msg (c_arg1, c_arg2, 'Value for ''cpt:row'' tag is incompatible with ''cpt:col=station''', &
              i_arg1, i_arg2, i_arg3)
       Case (6)
         Call read_error_msg (c_arg1, c_arg2, 'Value for ''cpt:row'' tag is incompatible with ''cpt:col=index''', &
              i_arg1, i_arg2, i_arg3)
       Case (7)
         Call read_error_msg (c_arg1, c_arg2, 'Unknown value for ''cpt:col'' tag', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('replace_missing_xy')
      Select Case (ifail)
       Case (1)
         Call print_error ('The '//c_arg1//' file contains only constant fields', &
              msg2=c_arg2)
       Case (2)
         Call print_error ('The '//c_arg1//' file contains too many missing series', &
              msg2=c_arg2)
       Case (3)
         Call print_error ('The '//c_arg1//' file contains at least one field with too many missing series', &
              msg2=c_arg2)
       Case (4)
         Call print_advisory ('Insufficient series to use nearest neighbour; mean used instead')
       Case (5)
         Call print_advisory ('Insufficient memory to estimate missing values; mean used instead')
       Case (6)
         Call print_error ('Probabilities do not sum to 1 or 100', &
              msg2='Incorrect probabilities found in '//Trim(c_arg2), &
              msg3=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('reset_fields')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to initalise setting.', &
              msg2='Contact '//cpt_email)
       Case Default
         GoTo 1
      End Select
!
    Case ('rhit_diagram')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct ranked hits diagram')
       Case Default
         GoTo 1
      End Select
!
    Case ('roc_diagram')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct ROC diagram')
       Case Default
         GoTo 1
      End Select
!
    Case ('save_graphic')
      Select Case (ifail)
       Case (0)
         Call print_advisory ('Graphic saved as '//c_arg1, &
              msg2='In '//c_arg2)
       Case (1)
         Call print_error ('Unable to save graphic')
       Case Default
         GoTo 1
      End Select
!
    Case ('set_nused')
      Select Case (ifail)
       Case (1)
         Write (msg1, Fmt='(A,I0,A)') &
                  'Length of initial training period must be at least cross-validation window length + ', i_arg1, '.'
         Call print_error (Trim(msg1), &
              msg2='Initial length of training period has been reset.')
       Case (2)
         Call print_error ('Length of initial training period must be at least 1.', &
              msg2='Try adjusting missing values settings.')
       Case Default
         GoTo 1
      End Select
!
    Case ('set_zero')
      Select Case (ifail)
       Case (1)
         msg1='Zero-bound option cannot be switched off when the SPI standardization option is selected.'
       Case (2)
#if GUI == 1
         msg1='Zero-bound option cannot be switched off when the %% of average standardization option is selected.'
#else
         msg1='Zero-bound option cannot be switched off when the % of average standardization option is selected.'
#endif
       Case Default
         GoTo 1
      End Select
      Call print_warning (.true., Trim(msg1), &
           msg2='Zero-bound option is still on. Change tailoring standardization option if you want to turn zero-bound off.')
!
    Case ('tailoring')
      Select Case (ifail)
       Case (1)
         Write (Unit=msg1, Fmt='(A,I0,A)') 'Invalid reference: ', i_arg1, '. Date is omitted because of missing values.'
         Call print_error (Trim(msg1))
       Case Default
         GoTo 1
      End Select
!
    Case ('tendency_diagram')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct tendency diagram')
       Case Default
         GoTo 1
      End Select
!
    Case ('threshold_opts')
      Select Case (ifail)
       Case (1)
         Call print_error ('The sum of the probabilities of the above- and below-normal categories must be less than 1.0.', &
              msg2='Please reset.')
       Case Default
         GoTo 1
      End Select
!
    Case ('update_file')
      Select Case (ifail)
       Case (1)
         Call print_warning (.true., &
              'Start-at date of update file must not be before start-at date of primary file.')
       Case (2)
         Call print_warning (.true., &
              'Problem initialising memory to read input files.')
       Case (3)
         Call print_warning (.true., &
              'Problem reading file', &
              msg2=c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('update_mask')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to apply skill-masking.', &
              msg2='Masking has been disabled.')
       Case Default
         GoTo 1
      End Select
!
    Case ('valid_climate')
      Select Case (ifail)
       Case (1)
         Call print_warning (.true., &
              'Last '//Trim(ca_seq_l(iuseq(i_arg1)))//' of climatology is not after first '//Trim(ca_seq_l(iuseq(i_arg1)))//'.', &
              msg2='Last '//Trim(ca_seq_l(iuseq(i_arg1)))//' of climatology has been reset.')
       Case Default
         GoTo 1
      End Select
!
    Case ('valid_pdate')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid month', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid day of month', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid month and day of month', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('valid_pprd')
      Select Case (ifail)
       Case (1)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid length of period', &
              i_arg1, i_arg2, i_arg3)
       Case (2)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid start date for dekad', &
              i_arg1, i_arg2, i_arg3)
       Case (3)
         Call read_error_msg (c_arg1, c_arg2, 'Invalid end date for dekad', &
              i_arg1, i_arg2, i_arg3)
       Case Default
         GoTo 1
      End Select
!
    Case ('view_good')
      Select Case (ifail)
       Case (1)
         Call print_error ('Goodness results are unavailable.', &
              msg2='Try Actions ~ Reset, and then re-running Actions ~ Calculate')
       Case Default
         GoTo 1
      End Select
!
    Case ('write_parameter')
      Select Case (ifail)
       Case (4)
         Call print_error ('Problem saving parameter file.')
       Case (5)
         Call print_advisory ('Parameters saved as '//c_arg1, &
              msg2='In '//c_arg2)
       Case Default
         GoTo 1
      End Select
!
    Case ('write_project')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening project file:', &
              msg2=c_arg1, &
              msg3='Cannot find file. Try checking file path and name.')
       Case (2)
         Call print_error ('Problem opening project file:', &
              msg2=c_arg1, &
              msg3='File is protected. Try closing file in other applications.')
       Case (3)
         Call print_error ('Problem opening project file:', &
              msg2=c_arg1)
       Case (4)
         Call print_error ('Problem saving project file.')
       Case (5)
         Call print_advisory ('Project saved as '//c_arg1, &
              msg2='In '//c_arg2)
       Case Default
         GoTo 1
      End Select
!
    Case ('write_results')
      Select Case (ifail)
       Case (1)
         Call print_error ('Problem opening '//c_arg1//' file:', &
              msg2=c_arg2)
       Case (2)
         Call print_error ('Problem writing cpt:T tag in '//c_arg1//' file:', &
              msg2=c_arg2)
       Case (3)
         Call print_error ('Problem saving '//c_arg1//' file:', &
              msg2=c_arg2)
       Case Default
         GoTo 1
      End Select
!
    Case ('wrlt_diagram')
      Select Case (ifail)
       Case (1)
         Call print_error ('Insufficient memory to construct profits diagram')
       Case Default
         GoTo 1
      End Select
!
    Case Default
      GoTo 1
   End Select
!
   GoTo 2
!
! Incorrect call
1 Write (msg1, '(A,I2)') 'Error: ',ifail
   Call print_error ('Unknown error. Please contact '//cpt_email//' providing the following information:', &
        msg2='Routine: '//cproc, &
        msg3=Trim(msg1))
!
! Switch cursor waiting back on
2 If (lwait) Call set_cursor_waiting (1)
!
  Return
!
 Contains
!
!
  Subroutine read_error_msg (afile, last_date, c_msg, ifd, ilf, igf)
!
! Prints read error messages
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: ifd ! - last field read successfully -
   Integer, Intent(In) :: ilf ! - last lagged-field read successfully -
   Integer, Intent(In) :: igf ! - last category-field read successfully -
!
   Character(Len=*), Intent(In) :: afile     ! - problem file -
   Character(Len=*), Intent(In) :: last_date ! - last date read -
   Character(Len=*), Intent(In) :: c_msg     ! - error message -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Len_Trim, Trim
!
! Executable Statements
!
! Construct error message
   Write (Unit=msg1, Fmt='(A)') 'Problem reading file: '//afile
   If (Len_Trim(last_date) > 0) Then
      If (ifd > 0) Then
         If (ilf > 0) Then
            If (igf > 0) Then
               Write (Unit=msg2, Fmt='(A,I0,A,I0,A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',ifd, get_cordn(ifd)//' field, ', &
                                                                                 &ilf, get_cordn(ilf)//' lagged-field, and ', &
                                                                                 &igf, get_cordn(igf)//' category.'
            Else
               Write (Unit=msg2, Fmt='(A,I0,A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',ifd, get_cordn(ifd)//' field and ', &
                                                                                 &ilf, get_cordn(ilf)//' lagged-field.'
            End If
         Else
            If (igf > 0) Then
               Write (Unit=msg2, Fmt='(A,I0,A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',ifd, get_cordn(ifd)//' field and ', &
                                                                                 &igf, get_cordn(igf)//' category.'
            Else
               Write (Unit=msg2, Fmt='(A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',ifd, get_cordn(ifd)//' field.'
            End If
         End If
      Else
         If (ilf > 0) Then
            If (igf > 0) Then
               Write (Unit=msg2, Fmt='(A,I0,A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',ilf, get_cordn(ilf)//' lagged-field and ', &
                                                                                 &igf, get_cordn(igf)//' category.'
            Else
               Write (Unit=msg2, Fmt='(A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',ilf, get_cordn(ilf)//' lagged-field.'
            End If
         Else
            If (igf > 0) Then
               Write (Unit=msg2, Fmt='(A,I0,A)') &
                  'Last successfully read data for '//Trim(last_date)//' in the ',igf, get_cordn(igf)//' category.'
            Else
               Write (Unit=msg2, Fmt='(A)') &
                  'Last successfully read data for '//Trim(last_date)//'.'
            End If
         End If
      End If
!
! Print error message
      Call print_error (Trim(msg1), &
           msg2=Trim(c_msg), &
           msg3=Trim(msg2))
   Else
      Call print_error (Trim(msg1), &
           msg2=Trim(c_msg))
   End If
!
   Return
  End Subroutine read_error_msg
 End Subroutine cpt_error
!
!
!
 Function query(cproc, iq, lwait, &
          c_arg1, c_arg2)
!
! Returns 1 for 'yes', and 0 for 'no'.
!
! Modules
   Use data_version, Only: cpt_email
   Use gui,          Only: print_error, prompt_yesno, set_cursor_waiting
!
! Function result
   Integer :: query
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iq ! - query number -
!
   Character(Len=*), Intent(In) :: cproc ! - procedure -
!
   Logical, Intent(In) :: lwait ! - set to cursor waiting after querying? -
!
! Optional arguments
! - optional input scalars -
   Character(Len=*), Intent(In), Optional :: c_arg1 ! - character argument -
   Character(Len=*), Intent(In), Optional :: c_arg2 ! - character argument -
!
! Locals
!
! Local scalars
   Character(Len=128) :: msg ! - error message -
!
! Executable Statements
!
! Identify query
   Call set_cursor_waiting (0)
   Select Case (cproc)
    Case ('check_new_file')
      Select Case (iq)
       Case (1)
         query = prompt_yesno(.true., .true., .false., &
                 'File already exists. Do you want to overwrite the existing file?')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_read')
      Select Case (iq)
       Case (1)
         query = prompt_yesno(.true., .true., .false., &
                 'First date of Y training period is before first date of X training period.', &
                 msg2='Are you sure you want to analyse negative lags?')
       Case (2)
         query = prompt_yesno(.true., .true., .false., &
                 'Predictors and predictands are synchronous.', &
                 msg2='Because the synchronous predictors flag is off, forecast lead-time is negative.', &
                 msg3='Are you sure you want to analyse negative lags?')
       Case (3)
         query = prompt_yesno(.true., .true., .false., &
                 'Forecast lag is more than one.', &
                 msg2='Are you sure you want to analyse large lags?')
       Case (4)
         query = prompt_yesno(.true., .true., .false., &
                 'Target season is inconsistent with observed data.', &
                 msg2='Are you sure you want to analyse inconsistent seasons?')
       Case Default
         GoTo 1
      End Select
!
    Case ('check_xfile')
      Select Case (iq)
       Case (1)
         query = prompt_yesno(.true., .true., .false., &
                 'Only unreferenced X input files can be used for '//c_arg1//'.', &
                 msg2='CPT will change to '//c_arg2//'. Do you wish to continue?')
       Case (2)
         query = prompt_yesno(.true., .true., .false., &
                 'Probabilistic X input files are required for '//c_arg1//'.', &
                 msg2='CPT will change to '//c_arg2//'. Do you wish to continue?')
       Case (3)
         query = prompt_yesno(.true., .true., .false., &
                 'Probabilistic X input files are not permitted for '//c_arg1//'.', &
                 msg2='CPT will change to '//c_arg2//'. Do you wish to continue?')
       Case (4)
         Call print_error ('Multi-field or multi-lagged input files cannot be used for '//c_arg1)
       Case Default
         GoTo 1
      End Select
!
    Case ('perform_analysis')
      Select Case (iq)
       Case (1)
         msg = 'Some validation scores will have the opposite orientation, and the forecast error variance will be problematic.'
         query = prompt_yesno(.true., .true., .true., &
                 'The Y variable ('//c_arg1//') is inconsistent with the X variable ('//c_arg2//').', &
                 msg2=Trim(msg), &
                 msg3='It is recommended to cancel, and to apply a skill correction from Options ~ GCM Options.', &
                 msg4='If you continue, do you want CPT to invert the orientation of the goodness index?')
       Case Default
         GoTo 1
      End Select
!
    Case ('read_ini')
      Select Case (iq)
       Case (1)
         query = prompt_yesno(.true., .true., .false., &
                 'Current version of .ini file is old.', &
                 msg2='CPT will update the file. Do you wish to continue?')
       Case Default
         GoTo 1
      End Select
!
    Case Default
      GoTo 1
   End Select
!
   GoTo 2
!
! Incorrect call
1  Write (msg, '(A,I2)') '%bfQuery:%bf%ta', iq
   Call print_error ('Unknown query. Please contact '//cpt_email//' providing the following information:', &
        msg2='%bfRoutine:%`bf%ta'//cproc, &
        msg3=msg)
   query = 0
!
! Switch cursor waiting back on
2  If (lwait) Call set_cursor_waiting (1)
!
  Return
 End Function query
End Module errors
