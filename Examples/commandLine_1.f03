      program commandLine_01
!
!     This program demonstrates the two key functions available in Fortran for
!     getting command line arguments.
!
!     H. P. Hratchian, 2020.
!
!
      implicit none
      integer::nArguments
!
!     Find out how many command line arguments there are and report that.
!
      nArguments = COMMAND_ARGUMENT_COUNT()
      write(*,*)' The number of command line arguements is ',nArguments
      end program commandLine_01
