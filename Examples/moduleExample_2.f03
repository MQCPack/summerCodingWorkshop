      module moduleExample_2_mod
      implicit none
!
!     Here are subroutines and functions for my program.
!
      CONTAINS
!
      subroutine print_matrix(matrix)
!
!     This subroutine prints a square matrix whose dimension is less than 6.
!
      implicit none
      real,dimension(:,:),intent(in)::matrix
      integer::i
!
!     Print the matrix.
!
 1000 Format(f10.5,4x,f10.5,4x,f10.5,4x,f10.5,4x,f10.5,4x,f10.5)
      do i = 1,Size(matrix,1)
        write(*,1000) matrix(i,:)
      endDo
!
      return
      end subroutine print_matrix
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
!
      end program moduleExample_2


