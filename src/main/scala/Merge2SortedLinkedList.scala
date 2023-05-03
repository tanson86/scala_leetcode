object Merge2SortedLinkedList extends App {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    (list1,list2) match {
      case (_,null) => list1
      case (null,_) => list2
      case (l1,l2) => if(l1.x<l2.x) ListNode(l1.x, mergeTwoLists(l1.next,l2))
      else ListNode(l2.x, mergeTwoLists(l1,l2.next))
    }
  }

  def mergeTwoLists1(list1: ListNode, list2: ListNode): ListNode = {
    val merged = new ListNode(-1, null)
    var temp = merged

    def mergeTwoListsAux(list1: ListNode, list2: ListNode): ListNode = {
      if (list1 == null && list2 == null) merged.next
      else if (list1 == null && list2 != null) {
        temp.next = list2;
        merged
      } else if (list1 != null && list2 == null) {
        temp.next = list1
        merged
      } else {
        if (list1.x <= list2.x) {
          temp.next = list1;
          temp = temp.next;
          mergeTwoListsAux(list1.next, list2)
        } else {
          temp.next = list2;
          temp = temp.next;
          mergeTwoListsAux(list1, list2.next)
        }
      }

    }
    mergeTwoListsAux(list1, list2)
  }

  def mergeTwoLists2(list1: ListNode, list2: ListNode): ListNode = {
    val merged = new ListNode()
    var temp = merged
    def mergeTwoListsAux(list1: Option[ListNode], list2: Option[ListNode]): ListNode = {
      (list1,list2) match {
        case (None,None) => merged.next
        case (None, Some(l2)) => {
          temp.next=l2
          mergeTwoListsAux(None,None)
        }
        case (Some(l1),None) => {
          temp.next=l1
          mergeTwoListsAux(None,None)
        }
        case(Some(l1),Some(l2)) if(l1.x<l2.x) => {
          temp.next = l1
          temp = temp.next
          mergeTwoListsAux(Option(l1.next), Option(l2))
        }
        case(Some(l1),Some(l2)) => {
            temp.next=l2
            temp=temp.next
            mergeTwoListsAux(Option(l1),Option(l2.next))
        }
      }
    }
    mergeTwoListsAux(Option(list1),Option(list2))
  }
}