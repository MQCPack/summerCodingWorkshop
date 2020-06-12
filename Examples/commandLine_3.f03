      program commandLine_03
!
!     This program demonstrates the two key functions available in Fortran for
!     getting command line arguments.
!
!     H. P. Hratchian, 2020.
!
!
      implicit none
      integer::nArguments,i
      character(len=256)::commandLineArg
!
!     Find out how many command line arguments there are and report that.
!
      nArguments = COMMAND_ARGUMENT_COUNT()
      write(*,*)' The number of command line arguements is ',nArguments
      do i = 1,nArguments
        call GET_COMMAND_ARGUMENT(i,commandLineArg)
        write(*,*)' argument number ',i,' is:',TRIM(commandLineArg)
      endDo
!
      end program commandLine_03
