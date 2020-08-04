      module sparseMod
!
!     This module contains objects and procedures in support of sparse algebra
!     operations.
!
!
!
      implicit none
!
!     Make a SparseVector Class.
!
      type,public::sparseVector
        real,dimension(:),allocatable::realVector
        integer,dimension(:),allocatable::indexVector
        integer::nDimDense,nDimSparse
      CONTAINS
        procedure,public::set => sparseVector_set
        procedure,public::print => sparseVector_print
        procedure,public::scatter => scatter_sparseVector
        procedure,public::dot_product => sparseVector_dot_product
      end type sparseVector
!
!     Procedure Interfaces
!
      interface dot_product
        module procedure sparseVector_dot_product
      end interface dot_product

      CONTAINS
!
      subroutine sparseVector_set(mySparseVector,realVector,indexVector,nDimDense)
!
!     This subroutine is used to initialize and set up a sparseVector object
!     with it's data.
!
      implicit none
      class(sparseVector),intent(inOut)::mySparseVector
      real,dimension(:),intent(in)::realVector
      integer,dimension(:),intent(in)::indexVector
      integer,intent(in)::nDimDense
!
      mySparseVector%nDimDense = nDimDense
      mySparseVector%nDimSparse = SIZE(indexVector)
      ALLOCATE(mySparseVector%realVector(mySparseVector%nDimSparse),  &
        mySparseVector%indexVector(mySparseVector%nDimSparse))
      mySparseVector%realVector = realVector
      mySparseVector%indexVector = indexVector
!
      return
      end subroutine sparseVector_set


      subroutine sparseVector_print(mySparseVector,iOut,header)
!
!     This subroutine prints the contents of a sparseVector object.
!
      implicit none
      class(sparseVector),intent(in)::mySparseVector
      integer,optional,intent(in)::iOut
      character(len=*),optional,intent(in)::header
      integer::myIOut,i
!
 1000 format(3x,'i=',i4,' v(i)=',f12.5)
!
      myIOut = 6
      if(PRESENT(iOut)) myIOut = iOut
      if(PRESENT(header)) write(myIOut,'(A)') TRIM(header)
      do i = 1,mySparseVector%nDimSparse
        write(myIOut,1000) mySparseVector%indexVector(i),mySparseVector%realVector(i)
      endDo
!
      return
      end subroutine sparseVector_print


!
      function scatter_sparseVector(mySparseVector) result(denseVec)
!
!     This function performs a scatter operation on a sparse vector given by the
!     sparseVector object mySparseVector. The output vector is given as an
!     intrinsic Fortran real array.
!
      implicit none
      class(sparseVector),intent(in)::mySparseVector
      real,dimension(:),allocatable::denseVec
!
      ALLOCATE(denseVec(mySparseVector%nDimDense))
      denseVec = scatter_realVector(mySparseVector%realVector,  &
        mySparseVector%indexVector,mySparseVector%nDimDense)
!
      return
      end function scatter_sparseVector

!
      function scatter_realVector(sparseVec,sparseIndex,nDimDense) result(denseVec)
!
!     This function performs a scatter operation on the sparse vector sparseVec
!     into the output vector that is dense with dimension nDimDense.
!
      implicit none
      real,dimension(:),allocatable::denseVec
      real,dimension(:),intent(in)::sparseVec
      integer,dimension(:),intent(in)::sparseIndex
      integer,intent(in)::nDimDense
      integer::i
!
      ALLOCATE(denseVec(nDimDense))
      denseVec = float(0)
      do i = 1,SIZE(sparseIndex)
        denseVec(sparseIndex(i)) = sparseVec(i)
      endDo
!
      return
      end function scatter_realVector


      function gather_realVector(sparseIndex,denseVec) result(sparseVec)
!
!     This function performs a gather operation on a dense vector whose sparse
!     indexing is known.
!
      implicit none
      real,dimension(:),allocatable::sparseVec
      integer,dimension(:),intent(in)::sparseIndex
      real,dimension(:),intent(in)::denseVec
      integer::i
!
      ALLOCATE(sparseVec(SIZE(sparseIndex)))
      do i = 1,SIZE(sparseIndex)
        sparseVec(i) = denseVec(sparseIndex(i))
      endDo
!
      return
      end function gather_realVector


      function sparseVector_dot_product(mySparseVector,denseVector) result(dotProduct)
!
!     This function produces the dot-product between a sparse vector (sent as a
!     spaseVector object) and an intrinsic Fortran dense vector.
!
      implicit none
      class(sparseVector),intent(in)::mySparseVector
      real,dimension(:),intent(in)::denseVector
      real::dotProduct
      integer::i
!
      dotProduct = 0
      do i = 1,mySparseVector%nDimSparse
        dotProduct = dotProduct + mySparseVector%realVector(i)*  &
          denseVector(mySparseVector%indexVector(i))
      endDo
!
      return
      end function sparseVector_dot_product


      end module sparseMod
