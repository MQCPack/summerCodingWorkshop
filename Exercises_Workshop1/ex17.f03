      module ex17_mod
!
!     This is the module supporting program ex17.
!
      type myComplex
        real::realPart,imaginaryPart
      end type myComplex
!
!     Operator Interfaces...
!
      interface operator (+)
        module procedure do_myComplex_add
      end interface
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


      subroutine myComplex_add(val1,val2,mySum)
!
!     This subroutine forms the sum of two arguments, val1 and val2, which are
!     of type myComplex.
!
      implicit none
      type(myComplex),intent(in)::val1,val2
      type(myComplex),intent(out)::mySum
!
      mySum%realPart      = val1%realPart + val2%realPart
      mySum%imaginaryPart = val1%imaginaryPart + val2%imaginaryPart
!
      return
      end subroutine myComplex_add


      function do_myComplex_add(val1,val2) result(mySum)
!
!     This function forms the sum of two arguments, val1 and val2, which are
!     of type myComplex.
!
      implicit none
      type(myComplex),intent(in)::val1,val2
      type(myComplex)::mySum
!
      mySum%realPart      = val1%realPart + val2%realPart
      mySum%imaginaryPart = val1%imaginaryPart + val2%imaginaryPart
!
      return
      end function do_myComplex_add


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
!     Form the sum of user1 and user2. Then, print the result.
!
      call myComplex_add(user1,user2,sum12)
      write(*,*)' The sum of your two complex numbers is:'
      call print_myComplex(sum12)
!
!     Form the sum of user1 and user2 again; this time let's use the new
!     function do_myComplex_add.
!
      sum12 = do_myComplex_add(user1,user2)
      write(*,*)' The sum of your two complex numbers using the function, is:'
      call print_myComplex(sum12)
      write(*,*)' ...and again...'
      call print_myComplex(do_myComplex_add(user1,user2))
!
!     Take the sum user1 and sum12.
!
      write(*,*)' The sum of user1 and sum12 = user1 + user1 + user2...'
      sum12 = user1 + sum12
      call print_myComplex(sum12)
      write(*,*)' ...and again...'
      sum12 = user1 + user1 + user2
      call print_myComplex(sum12)
!
      end program ex17
