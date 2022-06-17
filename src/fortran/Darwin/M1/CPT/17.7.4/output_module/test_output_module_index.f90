PROGRAM test_output_module_index
!
! Modules
  USE CPT_formatV11
!
! Implicit declarations
  IMPLICIT NONE
!
! Parameters
  INTEGER, PARAMETER :: iin=11    ! - input unit number -
  INTEGER, PARAMETER :: nv= 4     ! - number of stations -
  INTEGER, PARAMETER :: nt=10     ! - number of years -
  INTEGER, PARAMETER :: iyr1=1960 ! - first year -
  INTEGER, PARAMETER :: imn1=1    ! - first month -
  INTEGER, PARAMETER :: idy1=0    ! - first day -
!
  REAL, PARAMETER :: rmiss=-2. ! - missing values -
!
! Scalar
  INTEGER :: i     ! - station index -
  INTEGER :: k     ! - year index -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=512) :: ctag ! - CPT tag -
!
! Arrays
  REAL, DIMENSION(nv,nt) :: x ! - data -
!
  CHARACTER(LEN= 16), DIMENSION(nv) :: cvar ! - index names -
!
! Executable Statements
!
! Read index file
  OPEN (UNIT=iin,FILE='Example_index.tsv',ACTION='read',FORM='formatted',STATUS='old')
! - read the names of the indices -
  READ (UNIT=iin,FMT=*) (cvar(i),i=1,nv)
! - read the data -
  DO k=1,nt
     READ (UNIT=iin,FMT=*) ctag,(x(i,k),i=1,nv)
  END DO
  CLOSE (UNIT=iin)
!
! Output data
  CALL write_cpt_unrf_v11 ('CPT_index.txt',nv,nt,1,x,rmiss,'indices',cvar, &
                           iyr1,imn1,idy1,iyr1,imn1,idy1,ifail)
  PRINT *, ifail
END PROGRAM test_output_module_index
