include 'linkedListMod1.f03'
      program linkedList04
      USE linkedListMod1
!
!     This is a linked-list program based on the one in Chapman (see Figure
!     15-14).
!
!
!     Defined Derived Types and Variable Declarations
!
      implicit none
      type(linkedListReal)::list
      character(len=256)::filename
      integer::nvals=0
      integer::istat
      real::temp
!
!
!     Get the name of the input file from the user and then open that file.
!
      write(*,*)'Enter the name of the file containing the input data.'
      read(*,'(A)') filename
      OPEN(UNIT=9,FILE=filename,STATUS='OLD',ACTION='READ',IOSTAT=istat)
!
!     As long as the file opened without error, then we proceed to read the data
!     in the file.
!
      if(istat==0) then
        do
          read(9,*,IOSTAT=istat) temp
          if(istat /= 0) exit
          nvals = nvals+1
          call linkedListReal_push(list,temp)
        endDo
!
!       Now, write out the data that we have read.
!
        write(*,*)' nVals = ',nvals
        write(*,*)' Printing the list in order.'
        do
          if(.not.linkedListReal_hasNext(list)) exit
          write(*,*) linkedListReal_getCurrent(list,incrementBy=1)
        endDo
        write(*,*) linkedListReal_getCurrent(list,incrementBy=1)
        write(*,*)
        write(*,*)' Printing the list in reverse order.'
        call linkedListReal_moveCurrent2Tail(list)
        do
          if(.not.linkedListReal_hasPrevious(list)) exit
          write(*,*) linkedListReal_getCurrent(list,incrementBy=-1)
        endDo
        write(*,*) linkedListReal_getCurrent(list,incrementBy=-1)
!
!     I am done with the work of the program when the file opened correctly.
!
      else
        write(*,'(1X,A,I6)')'File open failed--status = ',istat
      endIf
!
!     The program is done...tell the user...
!
      write(*,*)' The program is complete!'
      end program linkedList04
