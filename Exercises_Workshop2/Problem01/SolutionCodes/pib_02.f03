INCLUDE 'pib_mod.f03'
      program pib_02
      USE pib_mod
!
!     USAGE:
!       ./pib_02.exe <mass> <boxLength> <slope> <nBasis>
!
!     ABOUT:
!     This program builds and prints the Hamiltonian for the modified Particle-
!     in-a-Box (mPIB) problem. In this problem, the standard PIB potential is
!     modified to include a linear potential within the box (0 < x < L) given by
!     V(x) = bx.
!
!     This program accepts command line arguments for the mass, box length,
!     slope parameter b, and the number of basis functions to use (nBasis; taken
!     to mean the first nBasis PIB eigenfunctions). The program reports kinetic
!     energy, potential energy, and Hamiltonian matrices.
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
      integer::i,j,k,nArguments,nBasis,lapackInfo
      real::mass,boxLength,slope
      real,dimension(:),allocatable::hamiltonianEVals,lapackWork
      real,dimension(:,:),allocatable::kineticEnergyMatrix,  &
        potentialEnergyMatrix,hamiltonianMatrix,hamiltonianEVecs
      character(len=256)::commandLineArg
      logical::fail=.false.
!
!     Format Statements
!
 1000 Format('Modified Particle-in-a-Box Program.'/,'Unit Test 2.',/,/,  &
        'Input Parameters:',/,  &
        2x,'mass      = ',F10.3,/,  &
        2x,'boxLength = ',F10.3,/,  &
        2x,'slope     = ',F10.3,/,  &
        2x,'nBasis    = ',I4,/)
 2000 Format('< i | T | j > = ')
 2010 Format('< i | V | j > = ')
 2020 Format('< i | H | j > = ')
 9000 Format('Incorrect number of command line arguments.',/,  &
        'Expected 5 but found ',I2,'.'/,  &
        'The list of arguments is:',/,  &
        3x,'<mass> <boxLength> <slope> <nBasis>')
 9100 Format('Input parameter ',A,' must be greater than 0.')
 9200 Format('Failure solving for Hamiltonian eigensystem.')
 9999 Format(/,'PROGRAM FAILED!')
!
!     Find out how many command line arguments there are, ensure it's the right
!     number, read-in the values, and echo the input as output.
!
      nArguments = COMMAND_ARGUMENT_COUNT()
      if(nArguments.ne.4) then
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
      read(commandLineArg,*) nBasis
      write(iOut,1000) mass,boxLength,slope,nBasis
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
      write(iOut,2000)
      call print_matrix(kineticEnergyMatrix)
      write(iOut,2010)
      call print_matrix(potentialEnergyMatrix)
      write(iOut,2020)
      call print_matrix(hamiltonianMatrix)
!
!     Diagonalize the Hamiltonian and report eigenvectors and eigenvalues.
!
      allocate(hamiltonianEVecs(nbasis,nBasis))
      allocate(hamiltonianEVals(nBasis),lapackWork(3*nBasis))
      write(iOut,*)' Hrant - here is the linearized hamiltonian...'
      hamiltonianEVecs = hamiltonianMatrix
      call SSYEV('V','L',nBasis,hamiltonianEVecs,nBasis,hamiltonianEVals,  &
        lapackWork,3*nBasis,lapackInfo)
      write(iOut,*)' lapackInfo = ',lapackInfo
      if(lapackInfo.ne.0) then
        write(iOut,9200)
        fail = .true.
        goto 999
      endIf
      call print_matrix(RESHAPE(hamiltonianEVecs,[ nBasis,nBasis ]))
      call print_matrix(RESHAPE(hamiltonianEVals,[ 1,nBasis ]))
!
!     Complete the program...
!
  999 if(fail) write(iOut,9999)
      end program pib_02
