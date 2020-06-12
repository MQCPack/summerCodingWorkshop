      program commandLine_02
!
!     This program demonstrates the two key functions available in Fortran for
!     getting command line arguments.
!
!     H. P. Hratchian, 2020.
!
!
      implicit none
      integer::nArguments
      character(len=256)::commandLineArg
!
!     Find out how many command line arguments there are and report that.
!
      nArguments = COMMAND_ARGUMENT_COUNT()
      write(*,*)' The number of command line arguements is ',nArguments
      if(nArguments.ge.1) then
        call GET_COMMAND_ARGUMENT(1,commandLineArg)
        write(*,*)' The first command line argument is: ',TRIM(commandLineArg)
      else
        write(*,*)' No command line arguments to read!'
      endIf
!
      end program commandLine_02
