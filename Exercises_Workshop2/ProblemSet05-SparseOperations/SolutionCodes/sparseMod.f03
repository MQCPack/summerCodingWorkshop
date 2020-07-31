      module sparseMod
!
!     This module contains objects and procedures in support of sparse algebra
!     operations.
!
!
      CONTAINS
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


      end module sparseMod
