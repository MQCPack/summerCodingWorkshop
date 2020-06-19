      module ex17_mod
!
!     This is the module supporting program ex17.
!
      type myComplex
        real::realPart,imaginaryPart
      end type myComplex
!
!     Module Procedures...
!
      Contains
!
!
      subroutine print_myComplex(val)
!
!     This subroutine prints a myComplex type variable.
!
      implicit none
      type(myComplex),intent(in)::val
!
 1000 Format(1x,'Real Part:',f10.5,'  Imaginary Part:',f10.5)
!
      write(*,1000) val%realPart,val%imaginaryPart
!
      return
      end subroutine print_myComplex

      end module ex17_mod


      program ex17
      use ex17_mod
!
!     This program asks a user for two complex numbers, loads those into my
!     derived data type called myComplex, echos the numbers back to the user,
!     and then adds the numbers and reports the result to the user.
!
!
!     Variable Declarations
      implicit none
      type(myComplex)::user1,user2,sum12
!
!     Begin by asking the user for the two input complex numbers.
!
      write(*,*)' Please enter the FIRST complex number giving the ',  &
        'real part followed by the imaginary part.'
      read(*,*) user1%realPart,user1%imaginaryPart
      write(*,*)' Please enter the SECOND complex number giving the ',  &
        'real part followed by the imaginary part.'
      read(*,*) user2%realPart,user2%imaginaryPart
      write(*,*)' Here are the values you have given as input...'
      write(*,*)' user1:'
      call print_myComplex(user1)
      write(*,*)' user2:'
      call print_myComplex(user2)
!
      end program ex17
