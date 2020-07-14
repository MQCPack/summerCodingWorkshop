      program matrix01
!
!     USAGE:
!       ./matrix01.exe <n>
!
!     ABOUT:
!     This program reads a size n from the command line, fills two (n x n)
!     matrices with random numbers, and evaluates the matrix product using
!     explicit loops. The program tracks the time taken for the
!     multiplication steps and reports is at the end.
!
!
!     AUTHOR:
!     H. P. Hratchian, 2020.
!
!
!     Variable Declarations
!
      implicit none
      integer::n,i,j,k
      integer,parameter::iOut=6
      real::tStart,tEnd
      real,dimension(:,:),allocatable::A,B,C
      character(len=256)::commandLineArg
      logical::fail=.false.
!
 1000 Format('n = ',I10,'  Job Time: ',F10.3,' s.')
 9000 Format('Failure reading command line arguments...incorrect number.')
 9999 Format('The program FAILED!')
!
!     Read the user-specified matrix dimension, n, from the command line.
!
      if(COMMAND_ARGUMENT_COUNT().ne.1) then
        write(iOut,9000)
        fail = .true.
        goto 999
      endIf
      call GET_COMMAND_ARGUMENT(1,commandLineArg)
      read(commandLineArg,*) n
!
!     Allocate matrices A, B, and C. Then, fill A and B with random numbers.
!
      Allocate(A(n,n),B(n,n),C(n,n))
      call random_number(A)
      call random_number(B)
      C = 0
!
!     Carry out matrix multiplication using explicit nested loops.
!
      call CPU_TIME(tStart)
      do i = 1,n
        do j = 1,n
          do k = 1,n
            C(i,j) = C(i,j) + A(i,k)*B(k,j)
          endDo
        endDo
      endDo
      call CPU_TIME(tEnd)
      write(iOut,1000) n,tEnd-tStart
!
  999 if(fail) write(iOut,9999)
      end program matrix01
