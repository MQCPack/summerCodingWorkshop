      module moduleExample_2_mod
      implicit none
!
!     Procedure Interfaces
!
      interface print_matrix
        module procedure print_matrix_integer
        module procedure print_matrix_real
      end interface print_matrix

!
!     Here are subroutines and functions for my program.
!
      CONTAINS
!
      subroutine print_matrix_integer(matrix,header)
!
!     This subroutine prints a square integer matrix whose dimension is less than 6.
!
      implicit none
      integer,dimension(:,:),intent(in)::matrix
      character(len=*),intent(in),optional::header
      integer::i
!
!     Print the matrix.
!
 1000 Format(I10,4x,I10,4x,I10,4x,I10,4x,I10,4x,I10)
 2000 Format(1x,A)
      if(PRESENT(header)) write(*,2000) TRIM(header)
      do i = 1,Size(matrix,1)
        write(*,1000) matrix(i,:)
      endDo
!
      return
      end subroutine print_matrix_integer


      subroutine print_matrix_real(matrix,header)
!
!     This subroutine prints a square real matrix whose dimension is less than 6.
!
      implicit none
      real,dimension(:,:),intent(in)::matrix
      character(len=*),intent(in),optional::header
      integer::i
!
!     Print the matrix.
!
 1000 Format(f10.5,4x,f10.5,4x,f10.5,4x,f10.5,4x,f10.5,4x,f10.5)
 2000 Format(1x,A)
      if(PRESENT(header)) write(*,2000) TRIM(header)
      do i = 1,Size(matrix,1)
        write(*,1000) matrix(i,:)
      endDo
!
      return
      end subroutine print_matrix_real
!
      end module moduleExample_2_mod


      program moduleExample_2
!
!     This is an example program demonstrating some uses of modules.
!
      USE moduleExample_2_mod
      implicit none
      integer,dimension(5,5)::matrixI,matrixJ,matrixK
      real,dimension(5,5)::matrixA,matrixB,matrixC
!
!     Fill real matrixA and matrixB with random numbers.
!
      call random_number(matrixA)
      call random_number(matrixB)
      write(*,*)' Matrix A:'
      write(*,*) matrixA
      write(*,*)' Here is Matrix A:'
      call print_matrix(matrixA)
      write(*,*)' Here is Matrix B:'
      call print_matrix(matrixB)
!
!     Fill integer matrixI and matrixJ with random numbers.
!
      call random_number(matrixC)
      matrixI = matrixC*100
      write(*,*)' Matrix I:'
      write(*,*) matrixI
      write(*,*)' Here is matrixI:'
      call print_matrix(matrixI)
!
!     Form matrixC = sqrt(matrixA)
!
      matrixC = SQRT(matrixA)
      write(*,*)' Here is matrixC:'
      call print_matrix(header='Here is matrixC in the print routine...',matrix=matrixC)
!
      end program moduleExample_2
