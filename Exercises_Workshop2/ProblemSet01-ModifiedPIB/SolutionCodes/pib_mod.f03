      module pib_mod
!
!     This module supports programs written to solve the modified
!     Particle-in-a-Box (mPIB) problem. In this problem, the standard PIB
!     potential is modified to include a linear potential within the box (0 < x
!     < L) given by V(x) = bx.
!
!     H. P. Hratchian, 2020.
!
!
!     Global Variable Declarations and Parameters
!       Global Parameters:
!       iOUt ... An integer with the stardard output unit number.
!       pi   ...  A real with the value of pi.
!
      integer,parameter::iOut=6
      real,parameter::pi=float(4)*ATan(1.0)
!
!     Procedure Interfaces
!
      Interface print_matrix
        module procedure print_matrix_full_real
      end Interface
!
!
!     Module Procedures
!
      Contains
!
!PROCEDURE kineticEnergy.
      function kineticEnergy(mass,boxLength,qnM,qnN) result(matrixElement)
!
!     This function returns the kinetic energy matrix element for <m|T|n> where
!     m and n are PIB eigenfunction labels and T is the 1D kinetic energy
!     operator.
!
!
!     Variable Delarations
!
      implicit none
      real,intent(in)::mass,boxLength
      integer,intent(in)::qnM,qnN
      real::matrixElement
!
!     Since the PIB eigenfunctions are eigenfunctions of the kinetic energy
!     operator, test to see if qnM and qnN are the same. If not, just set the
!     result to zero.
!
      if(qnM.eq.qnN) then
        matrixElement = (float(qnN**2)*pi**2)/(float(2)*mass*boxLength**2)
      else
        matrixElement = 0
      endIf
!
      return
      end function kineticEnergy
!
!PROCEDURE potentialEnergy.
      function potentialEnergy(mass,boxLength,slope,qnM,qnN) result(matrixElement)
!
!     This function returns the potential energy matrix element for <m|V|n>
!     where m and n are PIB eigenfunction labels and V is the potential energy
!     operator.
!
!
!     Variable Delarations
!
      implicit none
      real,intent(in)::mass,boxLength,slope
      integer,intent(in)::qnM,qnN
      real::matrixElement
      real::prefactorPlus,prefactorMinus
!
 9000 Format('*** PROBLEM IN POTENTIAL ENERGY FUNCTION! ***')
!
!     Since the PIB eigenfunctions are eigenfunctions of the kinetic energy
!     operator, test to see if qnM and qnN are the same. If not, just set the
!     result to zero.
!
      if(qnM.eq.qnN) then
        matrixElement = boxLength*slope/float(2)
      else
        select case (mod(qnM+qnN,2))
        case(0)
          matrixElement = float(0)
        case(1)
          prefactorPlus = float(1)/float(qnM+qnN)
          prefactorMinus = float(1)/float(qnM-qnN)
          matrixElement = (float(2)*boxLength*slope/pi**2)*  &
            (prefactorPlus**2-prefactorMinus**2)
        case default
          matrixElement = float(0)
          write(iOut,9000)
        end select
      endIf
!
      return
      end function potentialEnergy
!
!PROCEDURE print_matrix_full_real
      subroutine print_matrix_full_real(amat)
!
!     This subroutine prints a real matrix that is fully dimension - i.e., not
!     stored in packed form. AMat is the matrix.
!
!     The output of this routine is sent to unit number 6 (set by the local
!     parameter integer IOut).
!
!
!     Variable Declarations
!
      implicit none
      real,dimension(:,:),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      do ifirst = 1,SIZE(AMat,2),ncolumns
        ILast = Min(IFirst+NColumns-1,SIZE(AMat,2))
        write(IOut,2000) (i,i=IFirst,ILast)
        do i = 1,SIZE(AMat,1)
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      return
      end subroutine print_matrix_full_real
!
!
      end module pib_mod
