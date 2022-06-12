! Author: Simon Mason
Module parameter_file
!
! Outputs settings to a parameter file for use with the batch version
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: output_parameter
!
Contains
!
!
 Function output_parameter (iout, &
          cdesc, c, i, r) Result (output)
!
! Outputs a parameter entry
!
! Modules
   Use data_numbers, Only: rp
   Use data_text,    Only: c_tab, llab6
!
! Function result
   Integer :: output
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: iout ! - output unit number -
!
! Optional arguments
! - optional input scalars -
   Integer, Intent(In), Optional :: i ! - output value -
!
   Real(Kind=rp), Intent(In), Optional :: r ! - output value -
!
   Character(Len=*), Intent(In), Optional :: c     ! - output value -
   Character(Len=*), Intent(In), Optional :: cdesc ! - description -
!
! Locals
!
! Local scalars
   Character :: ccoment = "!" ! - comment indicator -
!
   Character(Len=llab6) :: ccom ! - comment -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Present, Trim
!
! Executable Statements
!
! Construct description
   If (Present(cdesc)) Then
      ccom = c_tab//ccoment//' '//Trim(cdesc)
   Else
      ccom = ' '
   End If
!
! Print delimited output
! - integers -
   If (Present(i)) Then
      Write (Unit=iout, Fmt='(I0,1X,A)', Err=1) i, Trim(ccom)
! - reals -
   Else If (Present(r)) Then
      Write (Unit=iout, Fmt=*, Err=1) r, ' ', Trim(ccom)
! - characters -
   Else If (Present(c)) Then
      Write (Unit=iout, Fmt='(A,1X,A)', Err=1) c, Trim(ccom)
   End If
!
   output = 0
   Return
!
! Errors
1  output = 1
   Return
 End Function output_parameter
End Module parameter_file
