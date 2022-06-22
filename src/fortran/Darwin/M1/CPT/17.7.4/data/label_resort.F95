! Author: Simon Mason
Program label_resort
!
  Integer, Parameter :: iin = 11  ! - input file -
  Integer, Parameter :: iout = 21 ! - output file -
  Integer, Parameter :: nlang = 5 ! - number of languages -
  Character(Len=512), Dimension(nlang) :: cstring ! - current input string -
!
! Open labels file
  Open (Unit=iin, File='labels.txt', &
        Access='sequential', Action='read', Form='formatted', Status='old')
  Open (Unit=iout, File='labels_new.txt', &
        Access='sequential', Action='write', Form='formatted', Status='unknown')
!
! Read labels
  Do
     Do il = 1, nlang
        Read (Unit=iin, Fmt='(A)', Err=1, End=2) cstring(il)
     End Do
!
! Resort labels
     Write (Unit=iout, Fmt=*) Trim(cstring(1))
     Write (Unit=iout, Fmt=*) Trim(cstring(3))
     Write (Unit=iout, Fmt=*) Trim(cstring(4))
     Write (Unit=iout, Fmt=*) Trim(cstring(5))
     Write (Unit=iout, Fmt=*) Trim(cstring(2))
  End Do
!
  Stop
!
1 Stop 1
!
2 Stop 2
!
 End Program label_resort
