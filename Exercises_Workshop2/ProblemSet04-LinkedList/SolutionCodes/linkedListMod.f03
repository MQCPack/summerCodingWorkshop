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
