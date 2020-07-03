INCLUDE 'pib_mod.f03'
INCLUDE 'print_mod.f03'
      program pib_02
      USE pib_mod
      USE print_mod
!
!     USAGE:
!       ./pib_02.exe <mass> <boxLength> <slope> <qnM> <qnN> <nBasis>
!
!     ABOUT:
!     This program builds and prints the Hamiltonian for the modified Particle-
!     in-a-Box (mPIB) problem. In this problem, the standard PIB potential is
!     modified to include a linear potential within the box (0 < x < L) given by
!     V(x) = bx.
!
!     This program accepts command line arguments for the mass, box length,
!     slope parameter b, two PIB quantum numbers (m and n), and the number of
!     basis functions to use (nBasis; taken to mean the first nBasis PIB
!     eigenfunctions). The program reports kinetic energy, potential energy, and
!     Hamiltonian matrices.
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
      integer::i,j,nArguments,qnM,qnN,nBasis
      real::mass,boxLength,slope
      real,dimension(:,:),allocatable::kineticEnergyMatrix,  &
        potentialEnergyMatrix,hamiltonianMatrix
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
        2x,'qnN       = ',I4,/,  &
        2x,'nBasis    = ',I4,/)
 2000 Format('< m | T | n > = ',F12.5)
 2010 Format('< m | V | n > = ',F12.5)
 9000 Format('Incorrect number of command line arguments.',/,  &
        'Expected 5 but found ',I2,'.'/,  &
        'The list of arguments is:',/,  &
        3x,'<mass> <boxLength> <slope> <qnM> <qnN> <nBasis>')
 9100 Format('Input parameter ',A,' must be greater than 0.')
 9999 Format(/,'PROGRAM FAILED!')
!
!     Find out how many command line arguments there are, ensure it's the right
!     number, read-in the values, and echo the input as output.
!
      nArguments = COMMAND_ARGUMENT_COUNT()
      if(nArguments.ne.6) then
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
      call GET_COMMAND_ARGUMENT(6,commandLineArg)
      read(commandLineArg,*) nBasis
      write(iOut,1000) mass,boxLength,slope,qnM,qnN,nBasis
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
      if(qnN.le.0) then
        write(iOut,9100) 'nBasis'
        fail = .true.
      endIf
      if(fail) goto 999
!
!     Allocate memory for the kinetic, potential, and Hamiltonian matrices.
!
      Allocate(kineticEnergyMatrix(nBasis,nBasis),  &
        potentialEnergyMatrix(nBasis,nBasis),  &
        hamiltonianMatrix(nBasis,nBasis))
!
!     Calculate the kinetic energy matrix element.
!
      do i = 1,nBasis
        do j = 1,nBasis
          kineticEnergyMatrix(i,j) = kineticEnergy(mass,boxLength,i,j)
          potentialEnergyMatrix(i,j) = potentialEnergy(mass,boxLength,slope,i,j)
        endDo
      endDo
      hamiltonianMatrix = kineticEnergyMatrix + potentialEnergyMatrix
!
!     Print out the kinetic energy, potential energy, and Hamiltonian matrices.
!
      call print_matrix_full_real(kineticEnergyMatrix,nBasis,nBasis)
      call print_matrix_full_real(potentialEnergyMatrix,nBasis,nBasis)
      call print_matrix_full_real(hamiltonianMatrix,nBasis,nBasis)
!
!     Complete the program...
!
  999 if(fail) write(iOut,9999)
      end program pib_02
