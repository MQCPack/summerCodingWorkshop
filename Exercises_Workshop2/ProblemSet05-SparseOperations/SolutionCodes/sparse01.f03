include "sparseMod.f03"
      program sparse01
      USE sparseMod
!
!     This program will serve as a sandbox for learning about sparse vector and
!     sparse matrix algebra.
!
!
      implicit none
      integer::i,j
      integer::nDim,nDimSparse
      real,dimension(:),allocatable::vec1
      integer,dimension(:),allocatable::indexVec1
      real,dimension(:),allocatable::denseVec1
!
!     Let's get some values from the user...
!
      write(*,*)' What is the dimension of the vector space?'
      read(*,*) nDim
      write(*,*)' How many non-zero elements are there in the sparse vector?'
      read(*,*) nDimSparse
      ALLOCATE(vec1(nDimSparse),indexVec1(nDimSparse))
      write(*,*)'Please enter the non-zero vector elements and their values in pairs.'
      do i = 1,nDimSparse
        read(*,*) indexVec1(i),vec1(i)
      endDo
!
!     Echo the user input...
!
      write(*,*)' nDim = ',nDim
      write(*,*)' nDimSparse = ',nDimSparse
      do i = 1,nDimSparse
        write(*,*) 'i=',indexVec1(i),'  vec1(i) = ',vec1(i)
      endDo
!
!     Allocate space for a full-memory version of the sparse vector and then use
!     a scatter function.
!
      ALLOCATE(denseVec1(nDim))
      denseVec1 = scatter_realVector(vec1,indexVec1,nDim)
      do i = 1,nDim
        write(*,*) i,denseVec1(i)
      endDo
!
!     Scan over the values of the dense array and build a new index before we
!     run gather and rebuild the sparse array.
!
      j = 0
      do i = 1,nDim
        if (abs(denseVec1(i)).gt.0.001) then
          j = j+1
          if(j.gt.nDimSparse) STOP
          indexVec1(j) = i
        endIf
      endDo
      if(j.ne.nDimSparse) then
        write(*,*)' j = ',j,'  BUT nDimSparse = ',nDimSparse
        write(*,*)' YIKES!!!'
        STOP
      endIf
!
!     Test out the gather function.
!
      vec1 = 999.123123
      vec1 = gather_realVector(indexVec1,denseVec1)
      write(*,*)
      write(*,*)' Here''s the sparse vector after running the gather function...'
      do i = 1,nDimSparse
        write(*,*) 'i=',indexVec1(i),'  vec1(i) = ',vec1(i)
      endDo
!
      end program sparse01
