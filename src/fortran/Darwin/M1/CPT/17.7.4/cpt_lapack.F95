Module cpt_lapack
!
! Defines:
!    interfaces to the LAPACK library
!
! Interfaces
!
! Explicit Interfaces
!
! Interface blocks
! - double precision -
   Interface gesdd
      Subroutine dgesdd ( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO )
         Character          JOBZ
         Integer            INFO, LDA, LDU, LDVT, LWORK, M, N
         Integer            IWORK( * )
         Double Precision   A( LDA, * ), S( * ), U( LDU, * ), VT( LDVT, * ), WORK( * )
      End Subroutine dgesdd
! - double precision -
      Subroutine sgesdd ( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO )
         Character          JOBZ
         Integer            INFO, LDA, LDU, LDVT, LWORK, M, N
         Integer            IWORK( * )
         Real               A( LDA, * ), S( * ), U( LDU, * ), VT( LDVT, * ), WORK( * )
      End Subroutine sgesdd
   End Interface gesdd
!
End Module cpt_lapack
