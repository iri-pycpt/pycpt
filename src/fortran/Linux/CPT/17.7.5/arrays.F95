! Author: Simon Mason
Module arrays
!
! Declarations
!
! Modules
   Use data_numbers,      Only: ap, rp
   Use data_io_constants, Only: lprd
   Use time,              Only: cpssn, pprd, pssn
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: accum_units, close_data, convert_units, do_convert, get_cvsample, insertion_sort, opposite_vars, proj_read_results, &
             proj_write_results, rank_data
!
! Scalars
!
! Real scalars
   Real(Kind=rp), Public :: vmin ! - lower bound -
   Real(Kind=rp), Public :: vmax ! - upper bound -
!
! Arrays
!
! Integer arrays
   Integer, Dimension(:), Allocatable, Public :: iusex ! - indices of used X variables -
   Integer, Dimension(:), Allocatable, Public :: iusey ! - indices of used Y variables -
!
! Real arrays
   Real(Kind=rp), Dimension(:), Allocatable, Public :: t   ! - binomial coefficients -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: tc  ! - cross-validated binomial coefficients -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: tf  ! - forecast binomial coefficients -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: tex ! - probability of exceedance thresholds -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: xm  ! - means of explanatory variables -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: xsd ! - standard deviations of explanatory variables -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: ym  ! - means of response variables -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: ysd ! - standard deviations of response variables -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: yc ! - cross-validated training period response data -
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: yt ! - transformed response data -
!
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: vcopy ! - copy of data -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: x     ! - explanatory variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: xc    ! - cross-validated training period explanatory data -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: xin   ! - input explanatory variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: xiny  ! - predictors interpolated to predictand locations -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: y     ! - response variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ybk   ! - backup response variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: yin   ! - input response variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: z     ! - updated explanatory variables -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: ziny  ! - updated predictors interpolated to predictand locations -
   Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: zyp   ! - monthly persistence component for forecasts -
!
! Character arrays
   Character(Len=lprd), Dimension(:), Allocatable, Public :: cxprds ! - X training dates -
!
! Derived type arrays
   Type(pprd), Dimension(:), Allocatable, Public :: xprds ! - X dates -
!
   Type(pssn), Dimension(:), Allocatable, Public :: fprds ! - forecast target periods -
   Type(pssn), Dimension(:), Allocatable, Public :: yprds ! - Y dates -
!
   Type(cpssn), Dimension(:), Allocatable, Public :: cfprds ! - forecast dates -
   Type(cpssn), Dimension(:), Allocatable, Public :: cyprds ! - Y training dates -
!
! Scalars
!
! Logical scalars
   Logical, Public :: lconvert ! - convert units? -
!
! Workspace
!
! Workspace dimensions
   Integer, Public :: liwk ! - integer workspace dimensions -
   Integer, Public :: lrwk ! - real workspace dimensions -
!
! Integer workspace
   Integer, Dimension(:), Allocatable, Public :: iwk ! - integer workspace -
!
! Real workspace
! - single precision -
   Real(Kind=ap), Dimension(:), Allocatable, Public :: awk ! - alternative precision workspace -
! - double precision -
   Real(Kind=rp), Dimension(:), Allocatable, Public :: rwk ! - default precision workspace -
!
   Real(Kind=rp), Dimension(:,:), Allocatable, Public :: rwk2 ! - default precision workspace -
!
! Logical workspace
   Logical, Dimension(:), Allocatable, Public :: lwk ! - logical workspace -
!
! Interfaces
!
! Generic interfaces
   Interface insertion_sort
      Module Procedure insertion_sort_1
      Module Procedure insertion_sort_2
   End Interface insertion_sort
!
! Routines
!
Contains
!
!
 Subroutine get_cvsample (hcw, icv, v, vcv, &
            v1, v1cv, v2, v2cv, v3, v3cv)
!
! Generates a cross-validated sample (assuming Dim=2 is for time)
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: hcw ! - half cross-validation width -
   Integer, Intent(In) :: icv ! - cross-validation index -
!
! - input arrays -
   Real(Kind=rp), Dimension(:,:), Intent(In) :: v ! - sample data -
!
! Output arrays
   Real(Kind=rp), Dimension(:,:), Intent(Out) :: vcv ! - cross-validated sample data -
!
! Optional arguments
! - optional input arrays -
   Real(Kind=rp), Dimension(:), Intent(In), Optional :: v1 ! - additional 1-D sample data -
!
   Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: v2 ! - additional 2-D sample data -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(In), Optional :: v3 ! - additional 3-D sample data -
!
! - optional output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out), Optional :: v1cv ! - additional 1-D cross-validated sample data -
!
   Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: v2cv ! - additional 2-D cross-validated sample data -
!
   Real(Kind=rp), Dimension(:,:,:), Intent(Out), Optional :: v3cv ! - additional 3-D cross-validated sample data -
!
! Locals
!
! Local scalars
   Integer :: i1 ! - start of cross-validation sample -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Cshift, Present
!
! Executable Statements
!
! Identify start of cross-validation sample
   i1 = hcw + icv - 1
!
! Cross-validate
   vcv(:,:) = Cshift(v(:,:), i1, Dim=2)
   If (Present(v1)) v1cv(:) = Cshift(v1(:), i1)
   If (Present(v2)) v2cv(:,:) = Cshift(v2(:,:), i1, Dim=2)
   If (Present(v3)) v3cv(:,:,:) = Cshift(v3(:,:,:), i1, Dim=2)
!
   Return
 End Subroutine get_cvsample
!
!
!
 Subroutine insertion_sort_1 (order, n, v)
!
! Sorts data in ascending or descending order using insertion sort algorithm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Character(Len=1), Intent(In) :: order ! - order -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:), Intent(InOut) :: v ! - data to be sorted -
!
! Locals
!
! Local scalars
   Integer :: i,j ! - indices -
!
   Real(Kind=rp) :: swp ! - swapped value -
!
! Executable Statements
!
! Sort data
! - ascending -
   Select Case (order)
    Case ('A', 'a')
      Do i = 2, n
         swp = v(i)
         Do j = 1, i-1
            If (v(i) < v(j)) Exit
         End Do
         If (j < i) Then
            v(j+1:i) = v(j:i-1)
            v(j) = swp
         End If
      End Do
! - descending -
    Case ('D', 'd')
      Do i = 2, n
         swp = v(i)
         Do j = 1, i-1
            If (v(i) > v(j)) Exit
         End Do
         If (j < i) Then
            v(j+1:i) = v(j:i-1)
            v(j) = swp
         End If
      End Do
   End Select
!
   Return
 End Subroutine insertion_sort_1
!
!
!
 Subroutine insertion_sort_2 (order, m, n, v)
!
! Sorts 2-D data in ascending or descending order of second dimension using insertion sort algorithm
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: m ! - number of locations -
   Integer, Intent(In) :: n ! - number of cases -
!
   Character(Len=1), Intent(In) :: order ! - order -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data to be sorted -
!
! Locals
!
! Local scalars
   Integer :: i, j ! - indices -
   Integer :: i1   ! - indices -
!
   Real(Kind=rp) :: swp ! - swapped value -
!
! Executable Statements
!
! Sort data
! - ascending -
   Select Case (order)
    Case ('A', 'a')
      Do i1 = 1, m
         Do i = 2, n
            swp = v(i1,i)
            Do j = 1, i-1
               If (v(i1,i) < v(i1,j)) Exit
            End Do
            If (j < i) Then
               v(i1,j+1:i) = v(i1,j:i-1)
               v(i1,j) = swp
            End If
         End Do
      End Do
! - descending -
    Case ('D', 'd')
      Do i1 = 1, m
         Do i = 2, n
            swp = v(i1,i)
            Do j = 1, i-1
               If (v(i1,i) > v(i1,j)) Exit
            End Do
            If (j < i) Then
               v(i1,j+1:i) = v(i1,j:i-1)
               v(i1,j) = swp
            End If
         End Do
      End Do
   End Select
!
   Return
 End Subroutine insertion_sort_2
!
!
!
 Subroutine rank_data (order, n, v, r)
!
! Ranks data in ascending or descending order
!
! Modules
   Use data_numbers, Only: half, one
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - number of cases -
!
   Character(Len=1), Intent(In) :: order ! - order -
!
! - input arrays -
   Real(Kind=rp), Dimension(:), Intent(In) :: v ! - data to be ranked -
!
! - output arrays -
   Real(Kind=rp), Dimension(:), Intent(Out) :: r ! - ranks -
!
! Locals
!
! Local scalars
   Integer :: i, j ! - indices -
!
! Executable Statements
!
! Initialise ranks
   r(1:n) = one
!
! Rank data
! - ascending -
   Select Case (order)
    Case ('A', 'a')
      Do i = 1, n-1
         Do j = i+1, n
            If (v(j) > v(i)) Then
               r(j) = r(j) + one
            Else If (v(j) < v(i)) Then
               r(i) = r(i) + one
            Else
               r(i) = r(i) + half
               r(j) = r(j) + half
            End If
         End Do
      End Do
! - descending -
    Case ('D', 'd')
      Do i = 1, n-1
         Do j = i+1, n
            If (v(j) < v(i)) Then
               r(j) = r(j) + one
            Else If (v(j) > v(i)) Then
               r(i) = r(i) + one
            Else
               r(i) = r(i) + half
               r(j) = r(j) + half
            End If
         End Do
      End Do
   End Select
!
   Return
 End Subroutine rank_data
!
!
!
 Function opposite_vars(vara, varb) &
          Result (opposite)
!
! Checks whether variables are inconsistent
!
! Function type
   Logical :: opposite
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Character(Len=*), Intent(In) :: vara ! - original variable -
   Character(Len=*), Intent(In) :: varb ! - target variable -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Trim
!
! Executable Statements
!
! Check for inconsistent variables
   Select Case (Trim(vara))
    Case ('precipitation', 'prcp', 'prec', 'pre', 'pr', 'rain', 'rainfall')
      opposite = (Index(varb, 'dry') > 0)
    Case ('temperature', 'tave', 'temp', 'tmp', 'tmax', 'tmin', 'tref', 't2m')
      opposite = (Index(varb, 'cold') > 0)
    Case Default
      opposite = .false.
   End Select
!
   Return
 End Function opposite_vars
!
!
!
 Subroutine do_convert (vara, unita, lena, varb, unitb, unitca, a, ifail)
!
! Checks whether to, and performs, units of measurement conversions
!
! Modules
   Use labels, Only: cg_none_l
   Use gui,    Only: print_warning
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lena ! - length of season a -
!
   Character(Len=*), Intent(In) :: vara  ! - original variable -
   Character(Len=*), Intent(In) :: varb  ! - target variable -
   Character(Len=*), Intent(In) :: unita ! - original units -
   Character(Len=*), Intent(In) :: unitb ! - target units -
!
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: unitca ! - converted units -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: a ! - converted data -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Trim
!
! Executable Statements
!
! Check for consistent units
   If (Trim(vara) == Trim(varb)) Then
      If (Trim(unita) /= Trim(cg_none_l) .and. Trim(unita) /= 'none' .and. &
          Trim(unitb) /= Trim(cg_none_l) .and. Trim(unitb) /= 'none') Then
         If (Trim(unita) /= Trim(unitb)) Then
            unitca = unita
            Call convert_units (unitca, lena, unitb, a(:,:), ifail)
            If (ifail == 0) Then
               lconvert = .true.
            Else
               Call print_warning (.true., 'Unable to convert from '//Trim(unita)//' to '//Trim(unitb), &
                    lcursor=.true.)
            End If
         End If
      End If
   End If
!
   Return
 End Subroutine do_convert
!
!
!
 Subroutine convert_units (aunits, lena, bunits, v, ifail)
!
! Converts units of measurement
!
! Modules
   Use data_numbers,        Only: five, nine, ten, thirty, oneh, onet, zero_F, zero_K, h2okgm3
   Use data_time_constants, Only: hrpdy, minphr, secpmin
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: lena ! - length of season a -
!
   Character(Len=*), Intent(In) :: bunits ! - new units -
!
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: aunits ! - original units -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data to be converted -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real, Trim
!
! Executable Statements
!
! Temperature conversions
   Select Case (Trim(aunits))
! - from Kelvin -
    Case ('K', 'Kelvin', 'Kelvin_scale')
      Select Case (Trim(bunits))
       Case ('K', 'Kelvin', 'Kelvin_scale') ! - alternative labelling -
         Continue
       Case ('C', 'Celsius', 'Celsius_scale', 'degree_Celsius') ! - to Celsius -
         v(:,:) = v(:,:) - zero_K
       Case ('F', 'Farenheit', 'Farenheit_scale') ! - to Farenheit -
         v(:,:) = zero_F + (v(:,:) - zero_K)*nine/five
       Case Default
         GoTo 1
      End Select
! - from Celsius -
    Case ('C', 'Celsius', 'Celsius_scale', 'degree_Celsius')
      Select Case (Trim(bunits))
       Case ('C', 'Celsius', 'Celsius_scale', 'degree_Celsius') ! - alternative labelling -
         Continue
       Case ('K', 'Kelvin', 'Kelvin_scale') ! - to Kelvin -
         v(:,:) = zero_K + v(:,:)
       Case ('F', 'Farenheit', 'Farenheit_scale') ! - to Farenheit -
         v(:,:) = zero_F + v(:,:)*nine/five
       Case Default
         GoTo 1
      End Select
! - from Farenheit -
    Case ('F', 'Farenheit', 'Farenheit_scale')
      Select Case (Trim(bunits))
       Case ('F', 'Farenheit', 'Farenheit_scale') ! - alternative labelling -
         Continue
       Case ('K', 'Kelvin', 'Kelvin_scale') ! - to Kelvin -
         v(:,:) = zero_K + (v(:,:) - zero_F)*five/nine
       Case ('C', 'Celsius', 'Celsius_scale', 'degree_Celsius') ! - to Celsius -
         v(:,:) = (v(:,:) - zero_F)*nine/five
       Case Default
         GoTo 1
      End Select
!
! Rainfall conversions
! - from kilogrammes per metre squared per second -
    Case ('kg m-2 s-1')
      Select Case (Trim(bunits))
       Case ('m/s', 'meters/second', 'm s-1') ! - to metres per second -
         v(:,:) = v(:,:)/h2okgm3
       Case ('mm/day') ! - to mm per day -
         v(:,:) = v(:,:)*onet*secpmin*minphr*hrpdy/h2okgm3
       Case ('mm/month') ! - to mm per month -
         v(:,:) = v(:,:)*thirty*onet*secpmin*minphr*hrpdy/h2okgm3
       Case ('mm') ! - to mm -
         v(:,:) = v(:,:)*Real(lena,Kind=rp)*onet*secpmin*minphr*hrpdy/h2okgm3
       Case ('cm/day') ! - to cm per day -
         v(:,:) = v(:,:)*oneh*secpmin*minphr*hrpdy/h2okgm3
       Case Default
         GoTo 1
      End Select
! - from metres per second -
    Case ('m/s', 'meters/second', 'm s-1')
      Select Case (Trim(bunits))
       Case ('m/s', 'meters/second', 'm s-1') ! - alternative labelling -
         Continue
       Case ('kg m-2 s-1') ! - to kilogrammes per metre squared per second -
         v(:,:) = v(:,:)*h2okgm3
       Case ('mm/day') ! - to mm per day -
         v(:,:) = v(:,:)*onet*secpmin*minphr*hrpdy
       Case ('mm/month') ! - to mm per month -
         v(:,:) = v(:,:)*thirty*onet*secpmin*minphr*hrpdy
       Case ('mm') ! - to mm -
         v(:,:) = v(:,:)*Real(lena,Kind=rp)*onet*secpmin*minphr*hrpdy
       Case ('cm/day') ! - to cm per day -
         v(:,:) = v(:,:)*oneh*secpmin*minphr*hrpdy
       Case Default
         GoTo 1
      End Select
! - from mm per day -
    Case ('mm/day')
      Select Case (Trim(bunits))
       Case ('kg m-2 s-1') ! - to kilogrammes per metre squared per second -
         v(:,:) = v(:,:)*h2okgm3/(onet*hrpdy*minphr*secpmin)
       Case ('m/s', 'meters/second', 'm s-1') ! - to metres per second -
         v(:,:) = v(:,:)/(onet*hrpdy*minphr*secpmin)
       Case ('mm/month') ! - to mm per month -
         v(:,:) = v(:,:)*thirty
       Case ('mm') ! - to mm -
         v(:,:) = v(:,:)*Real(lena,Kind=rp)
       Case ('cm/day') ! - to cm per day -
         v(:,:) = v(:,:)/ten
       Case Default
         GoTo 1
      End Select
! - from mm per month -
    Case ('mm/month')
      v(:,:) = v(:,:)/thirty
      Select Case (Trim(bunits))
       Case ('kg m-2 s-1') ! - to kilogrammes per metre squared per second -
         v(:,:) = v(:,:)*h2okgm3/(onet*hrpdy*minphr*secpmin)
       Case ('m/s', 'meters/second', 'm s-1') ! - to metres per second -
         v(:,:) = v(:,:)/(onet*hrpdy*minphr*secpmin)
       Case ('mm') ! - to mm -
         v(:,:) = v(:,:)*Real(lena,Kind=rp)
       Case ('cm/day') ! - to cm per day -
         v(:,:) = v(:,:)/ten
       Case Default
         GoTo 1
      End Select
! - from mm -
    Case ('mm')
      v(:,:) = v(:,:)/Real(lena,Kind=rp)
      Select Case (Trim(bunits))
       Case ('kg m-2 s-1') ! - to kilogrammes per metre squared per second -
         v(:,:) = v(:,:)*h2okgm3/(onet*hrpdy*minphr*secpmin)
       Case ('m/s', 'meters/second', 'm s-1') ! - to metres per second -
         v(:,:) = v(:,:)/(onet*hrpdy*minphr*secpmin)
       Case ('mm/month') ! - to mm per month -
         v(:,:) = v(:,:)*thirty
       Case ('cm/day') ! - to cm per day -
         v(:,:) = v(:,:)/ten
       Case Default
         GoTo 1
      End Select
! - from cm per day -
    Case ('cm/day')
      Select Case (Trim(bunits))
       Case ('kg m-2 s-1') ! - to kilogrammes per metre squared per second -
         v(:,:) = v(:,:)*h2okgm3/(oneh*hrpdy*minphr*secpmin)
       Case ('m/s', 'meters/second', 'm s-1') ! - to metres per second -
         v(:,:) = v(:,:)/(oneh*hrpdy*minphr*secpmin)
       Case ('mm/day') ! - to mm per day -
         v(:,:) = v(:,:)*ten
       Case ('mm/month') ! - to mm per month -
         v(:,:) = v(:,:)*ten*thirty
       Case ('mm') ! - to mm -
         v(:,:) = v(:,:)*Real(lena,Kind=rp)
       Case Default
         GoTo 1
      End Select
    Case Default
      GoTo 1
   End Select
!
! Set new units
   aunits = bunits
   ifail = 0
   Return
!
! Unable to convert
1  ifail = 1
!
   Return
 End Subroutine convert_units
!
!
!
 Subroutine accum_units (n, cunits)
!
! Converts units indicator for monthly accumulations
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: n ! - length of n -
!
! - input/output scalars -
   Character(Len=*), Intent(InOut) :: cunits ! - units -
!
! Locals
!
! Local scalars
   Integer :: i     ! - locator -
   Integer :: ltrim ! - trimmed length -
!
   Character(Len=32) :: cprd ! - period -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Index, Len_Trim, Trim
!
! Executable Statements
!
! Indicate seasonal accumulation
   i = Index(cunits, '/')
   If (i > 0) Then
      ltrim = Len_Trim(cunits)
      cprd = cunits(i+1:ltrim)
      Write (cunits(i+1:), Fmt='(I0,2A)') n, '-', Trim(cprd)
   End If
!
   Return
 End Subroutine accum_units
!
!
!
 Function close_data() &
          Result (fclose)
!
! Frees memory allocated for data
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
! Free memory allocated for scoring
   If (Allocated(cyprds)) Deallocate (cyprds)
   If (Allocated(cxprds)) Deallocate (cxprds)
   If (Allocated(yprds))  Deallocate (yprds)
   If (Allocated(xprds))  Deallocate (xprds)
   If (Allocated(t))      Deallocate (t)
   If (Allocated(y))      Deallocate (y)
   If (Allocated(x))      Deallocate (x)
!
   fclose = 0
!
   Return
 End Function close_data
!
!
!
 Subroutine proj_read_results (iin, ianal, ifail)
!
! Reads results from project file
!
! Modules
   Use data_cpt_constants, Only: ia_gcm, ia_pfv, ncps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iin   ! - input file unit number -
   Integer, Intent(In) :: ianal ! - analysis indicator -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
    Integer :: i ! - season component -
!
! Executable Statements
!
! Read results
   If (ianal == ia_pfv) Return
   Read (Unit=iin, Err=1, End=2) xm
   Read (Unit=iin, Err=1, End=2) xsd
   Read (Unit=iin, Err=1, End=2) ym
   Read (Unit=iin, Err=1, End=2) ysd
   Select Case (ianal)
    Case (ia_gcm) ! - GCM -
      Read (Unit=iin, Err=1, End=2) xiny
   End Select
   Read (Unit=iin, Err=1, End=2) xprds
   Do i = 0, ncps
      Read (Unit=iin, Err=1, End=2) yprds(:)%tprd(i)
   End Do
   Read (Unit=iin, Err=1, End=2) cxprds
   Do i = 0, ncps
      Read (Unit=iin, Err=1, End=2) cyprds(:)%cssn(i)
   End Do
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
 End Subroutine proj_read_results
!
!
!
 Subroutine proj_write_results (iout, ianal, ifail)
!
! Writes results to project file
!
! Modules
   Use data_cpt_constants, Only: ia_gcm, ia_pfv, ncps
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout  ! - output file unit number -
   Integer, Intent(In) :: ianal ! - analysis indicator -
!
! - output scalars -
   Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
    Integer :: i ! - season component -
!
! Executable Statements
!
! Write results
   If (ianal == ia_pfv) Return
   Write (Unit=iout, Err=1) xm
   Write (Unit=iout, Err=1) xsd
   Write (Unit=iout, Err=1) ym
   Write (Unit=iout, Err=1) ysd
   Select Case (ianal)
    Case (ia_gcm) ! - GCM -
      Write (Unit=iout, Err=1) xiny
   End Select
   Write (Unit=iout, Err=1) xprds
   Do i = 0, ncps
      Write (Unit=iout, Err=1) yprds(:)%tprd(i)
   End Do
   Write (Unit=iout, Err=1) cxprds
   Do i = 0, ncps
      Write (Unit=iout, Err=1) cyprds(:)%cssn(i)
   End Do
!
   ifail = 0
   Return
!
! Errors in project file
! - problem writing file -
1  ifail = 1
   Return
!
 End Subroutine proj_write_results
End Module arrays
