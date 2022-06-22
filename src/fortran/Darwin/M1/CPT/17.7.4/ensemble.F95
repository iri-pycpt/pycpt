Module ensemble
!
! Declarations
!
! Implicit declarations
   Implicit None
!
! Accessibility
   Private
   Public :: ensemble_ave
!
Contains
!
!
 Subroutine ensemble_ave (nfs, nls, n, nv, nem, v)
!
! Averages ensemble members
!
! Modules
   Use data_numbers, Only: rp, zero
!
! Arguments
!
! Dummy arguments
! - input scalars -
   Integer, Intent(In) :: nfs ! - number of fields -
   Integer, Intent(In) :: nls ! - number of lagged-fields -
   Integer, Intent(In) :: n   ! - number of cases -
   Integer, Intent(In) :: nem ! - number of ensemble members -
!
! - input arrays -
   Integer, Dimension(:), Intent(In) :: nv ! - number of variables -
!
! - input/output arrays -
   Real(Kind=rp), Dimension(:,:), Intent(InOut) :: v ! - data -
!
! Locals
!
! Local scalars
   Integer :: i   ! - variable index -
   Integer :: ie  ! - ensemble member index -
   Integer :: ifd ! - field index -
   Integer :: ilf ! - lagged field index -
   Integer :: iv  ! - current variable -
   Integer :: iv1 ! - variable offset -
   Integer :: k   ! - time index -
   Integer :: l   ! - field/lagged field index -
!
! Local arrays
   Real(Kind=rp), Dimension(n) :: eave ! - ensemble average -
!
! Functions and Subroutines
!
! Intrinsic functions
   Intrinsic Real
!
! Executable Statements
!
! Average ensemble members
   iv1 = 0
   Do ifd = 1, nfs
      Do ilf = 1, nls
         l = (ifd - 1)*nem*nls + ilf
         Do i = 1, nv(l)
            eave(:) = zero
            Do ie = 1, nem
               iv = iv1 + ((ie - 1)*nls + (ilf - 1))*nv(l) + i
               Do k = 1, n
                  eave(k) = eave(k) + v(iv,k)
               End Do
            End Do
            eave(:) = eave(:)/Real(nem, Kind=rp)
            Do ie = 1, nem
               iv = iv1 + ((ie - 1)*nls + (ilf - 1))*nv(l) + i
               Do k = 1, n
                  v(iv,k) = eave(k)
               End Do
            End Do
         End Do
      End Do
      l = ifd*nem*nls
      iv1 = iv1 + nem*nls*nv(l)
   End Do
!
   Return
 End Subroutine ensemble_ave
End Module ensemble
