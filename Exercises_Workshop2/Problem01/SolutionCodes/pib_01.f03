INCLUDE 'pib_mod.f03'
      program pib_01
      USE pib_mod
!
!     This program is used as a unit test of the modified Particle-in-a-Box
!     (mPIB) problem. In this problem, the standard PIB potential is modified to
!     include a linear potential within the box (0 < x < L) given by V(x) = bx.
!
!     This specific unit test accepts command line arguments for the mass, box
!     length, slope parameter b, and two PIB quantum numbers (m and n). The
!     program reports kinetic energy, potential energy, and Hamiltonian matrix
!     elements for the system where standard PIB eigenfunctions m and n are
!     taken in the bra and ket of each matrix element, respectively.
!
!     All input and results are taken to be in atomic units.
!
!
!     H. P. Hratchian, 2020.
!
!
!     Variable Declarations
!
      implicit none
      integer::nArguments,qnM,qnN
      real::mass,boxLength,slope
      character(len=256)::commandLineArg
      logical::fail=.false.
!
!     Format Statements
!
 1000 Format('Modified Particle-in-a-Box Program.'/,'Unit Test 1.',/,/,  &
        'Input Parameters:',/,  &
        2x,'mass      = ',F10.3,/,  &
        2x,'boxLength = ',F10.3,/,  &
        2x,'slope     = ',F10.3,/,  &
        2x,'qnM       = ',I4,/,  &
        2x,'qnN       = ',I4,/)
 2000 Format('< m | T | n > = ',F12.5)
 2010 Format('< m | V | n > = ',F12.5)
 9000 Format('Incorrect number of command line arguments.',/,  &
        'Expected 5 but found ',I2,'.'/,  &
        'The list of arguments is: mass boxLength slope qnM qnN')
 9100 Format('Input parameter ',A,' must be greater than 0.')
 9999 Format(/,'PROGRAM FAILED!')
!
!     Find out how many command line arguments there are, ensure it's the right
!     number, read-in the values, and echo the input as output.
!
      nArguments = COMMAND_ARGUMENT_COUNT()
      if(nArguments.ne.5) then
        write(iOut,9000) nArguments
        fail = .true.
        goto 999
      endIf
      call GET_COMMAND_ARGUMENT(1,commandLineArg)
      read(commandLineArg,*) mass
      call GET_COMMAND_ARGUMENT(2,commandLineArg)
      read(commandLineArg,*) boxLength
      call GET_COMMAND_ARGUMENT(3,commandLineArg)
      read(commandLineArg,*) slope
      call GET_COMMAND_ARGUMENT(4,commandLineArg)
      read(commandLineArg,*) qnM
      call GET_COMMAND_ARGUMENT(5,commandLineArg)
      read(commandLineArg,*) qnN
      write(iOut,1000) mass,boxLength,slope,qnM,qnN
      if(mass.le.0) then
        write(iOut,9100) 'mass'
        fail = .true.
      endIf
      if(boxLength.le.0) then
        write(iOut,9100) 'boxLength'
        fail = .true.
      endIf
      if(slope.le.0) then
        write(iOut,9100) 'slope'
        fail = .true.
      endIf
      if(qnM.le.0) then
        write(iOut,9100) 'qnM'
        fail = .true.
      endIf
      if(qnN.le.0) then
        write(iOut,9100) 'qnN'
        fail = .true.
      endIf
      if(fail) goto 999
!
!     Calculate the kinetic energy matrix element.
!
      write(iOut,2000) kineticEnergy(mass,boxLength,qnM,qnN)
      write(iOut,2010) potentialEnergy(mass,boxLength,slope,qnM,qnN)
!
!     Complete the program...
!
  999 if(fail) write(iOut,9999)
      end program pib_01
