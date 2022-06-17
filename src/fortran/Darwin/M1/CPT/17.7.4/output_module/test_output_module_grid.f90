PROGRAM test_output_module_grid
!
! Modules
  USE CPT_formatV11
!
! Implicit declarations
  IMPLICIT NONE
!
! Parameters
  INTEGER, PARAMETER :: iin=11    ! - input unit number -
  INTEGER, PARAMETER :: ny= 4     ! - number of latitudes -
  INTEGER, PARAMETER :: nx= 8     ! - number of longitudes -
  INTEGER, PARAMETER :: nt= 7     ! - number of years -
  INTEGER, PARAMETER :: iyr1=1979 ! - first year -
  INTEGER, PARAMETER :: imn1=2    ! - first month -
  INTEGER, PARAMETER :: idy1=0    ! - first day -
  INTEGER, PARAMETER :: lsn=3     ! - length of season -
!
  REAL, PARAMETER :: rmiss=-99. ! - missing values -
!
! Scalar
  INTEGER :: i     ! - latitude index -
  INTEGER :: j     ! - longitude index -
  INTEGER :: k     ! - year index -
  INTEGER :: ifail ! - error indicator -
!
! Arrays
  REAL, DIMENSION(nx,ny,nt) :: v ! - data -
  REAL, DIMENSION(ny) :: rlat    ! - latitudes -
  REAL, DIMENSION(nx) :: rlng    ! - longitudes -
!
! Executable Statements
!
! Read file
  OPEN (UNIT=iin,FILE='Example_grid.tsv',ACTION='read',FORM='formatted',STATUS='old')
! - read the data -
  DO k=1,nt
     READ (UNIT=iin,FMT=*)
     READ (UNIT=iin,FMT=*) (rlng(j),j=1,nx)
     DO i=1,ny
        READ (UNIT=iin,FMT=*) rlat(i),(v(j,i,k),j=1,nx)
     END DO
  END DO
  CLOSE (UNIT=iin)
!
! Output data
  CALL write_cpt_grid_v11 ('CPT_grid.txt',nt,nx,ny,1,v,rmiss,rlat,rlng,'rain','mm', &
                           iyr1,imn1,idy1,iyr1,imn1+imn1-1,idy1,ifail)
  PRINT *, ifail
END PROGRAM test_output_module_grid