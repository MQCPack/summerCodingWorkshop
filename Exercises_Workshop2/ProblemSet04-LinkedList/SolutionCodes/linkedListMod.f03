      module linkedListMod
!
!     This module supports a linked list class.
!
!
!     Define class.
!
      implicit none
      TYPE::real_value
        real::value
        type(real_value),pointer::prev,next
      end TYPE real_value
!
      TYPE::linkedListReal
        type(real_value),pointer::head=>null()
        type(real_value),pointer::tail=>null()
        type(real_value),pointer::current=>null()
        logical::init=.false.
      end TYPE linkedListReal
!
!
!     Procedure Subroutines and Functions
!
      CONTAINS
!
      subroutine linkedListReal_incrementCurrent(myLinkedList,incrementBy)
!
!     This subroutine is used to increment the current pointer of a
!     linkedListReal object. If current has not yet been set to point at the
!     linked list, then put the current pointer to the head of the list before
!     applying the increment. If current is at the end of the list, do NOT
!     increment further.
!
      implicit none
      type(linkedListReal)::myLinkedList
      integer,optional,intent(in)::incrementBy
      integer::myIncrementBy,i
!
!     Set up myIncrementBy.
!
      myIncrementBy = 1
      if(PRESENT(incrementBy)) myIncrementBy = incrementBy
!
      if(.not.ASSOCIATED(myLinkedList%current)) myLinkedList%current=>myLinkedList%head
      if(myIncrementBy.gt.0) then
        do i = 1,myIncrementBy
          if(ASSOCIATED(myLinkedList%current%next)) myLinkedList%current=>myLinkedList%current%next
        endDo
      elseIf(myIncrementBy.lt.0) then
        do i = 1,ABS(myIncrementBy),-1
          if(ASSOCIATED(myLinkedList%current%next)) myLinkedList%current=>myLinkedList%current%next
        endDo
      endIf
!
      return
      end subroutine linkedListReal_incrementCurrent


      function linkedListReal_getCurrent(myLinkedList)
!
!     This function returns an intrinsic real value equal to the current node
!     value. The current position in the linked list always begins at the head
!     of the list and increments each time this function is used. When it
!     reaches the tail of the list, it just stays at the tail.
!
      implicit none
      type(linkedListReal)::myLinkedList
      real::linkedListReal_getCurrent
!
      if(.not.ASSOCIATED(myLinkedList%current)) myLinkedList%current=>myLinkedList%head
      linkedListReal_getCurrent = myLinkedList%current%value
      call linkedListReal_incrementCurrent(myLinkedList)
!
      return
      end function linkedListReal_getCurrent


      function linkedListReal_hasNext(myLinkedList)
!
!     This function returns a logical (TRUE or FALSE) indicating whether or not
!     the current member of the linked list object has a "next" node to go to.
!
      implicit none
      type(linkedListReal)::myLinkedList
      logical::linkedListReal_hasNext
!
      if(.not.ASSOCIATED(myLinkedList%current)) myLinkedList%current=>myLinkedList%head
      linkedListReal_hasNext = ASSOCIATED(myLinkedList%current%next)
!
      return
      end function linkedListReal_hasNext


      subroutine linkedListReal_push(myLinkedList,newValue)
!
!     This subroutine executes push onto the linked list called myLinkedList.
!
!
      implicit none
      type(linkedListReal)::myLinkedList
      real,intent(in)::newValue
      type(real_value),pointer::ptr=>null()
      integer::istat
!
!     Put newValue at the end of the linked list...
!
      if(.not.ASSOCIATED(myLinkedList%head)) then
        ALLOCATE(myLinkedList%head,STAT=istat)
        myLinkedList%tail => myLinkedList%head
        NULLIFY(myLinkedList%tail%prev,myLinkedList%tail%next)
        myLinkedList%tail%value = newValue
      else
        ALLOCATE(myLinkedList%tail%next,STAT=istat)
        ptr => myLinkedList%tail
        myLinkedList%tail => myLinkedList%tail%next
        myLinkedList%tail%prev => ptr
        NULLIFY(myLinkedList%tail%next)
        myLinkedList%tail%value = newValue
      endIf
!
      return
      end subroutine linkedListReal_push

      end module linkedListMod
