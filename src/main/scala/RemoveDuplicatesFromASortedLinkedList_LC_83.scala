import RemoveDuplicatesFromASortedLinkedList_LC_83.ListNode

import scala.annotation.tailrec
import scala.util.chaining._

object RemoveDuplicatesFromASortedLinkedList_LC_83 extends App {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
       var next: ListNode = _next
       var x: Int = _x
     }

  def deleteDuplicates(head: ListNode): ListNode = {
    Option(head).map(_.tap(checkAndDelete)).getOrElse(head)
  }
  // apply side effects
  def checkAndDelete(node: ListNode): Unit = {
    Option(node.next) match {
      case Some(next) if next.x == node.x =>
        node
          .tap(n => n.next = next.next)
          .tap(checkAndDelete)
      case Some(next) =>
        next.tap(checkAndDelete)
      case _ =>
    }
  }

  def deleteDuplicates5(head: ListNode): ListNode = {

    @tailrec
    def loop(slow:ListNode, fast:ListNode): ListNode =

      (slow, fast) match {

        case (_, null) => head
        case (slow, fast) if (slow.x == fast.x) => loop({slow.next = slow.next.next; slow}, fast.next)
        case _ => loop(slow.next, fast.next)
      }
    Option(head).fold(head){h => loop(h, h.next)}
  }

  def deleteDuplicates4(head: ListNode): ListNode = {
    head match {
      case h if (h== null || h.next== null)  => h
      case h if h.next.x == h.x => deleteDuplicates4(h.next)
      case h => new ListNode(h.x, deleteDuplicates4(h.next))
    }
  }

  def deleteDuplicates3(head: ListNode): ListNode = {
    if(head==null || head.next==null) return head
    val next = head.next
    if(head.x==next.x) deleteDuplicates3(next)
    else new ListNode(head.x,deleteDuplicates3(next))
  }

  //Not great solution
  def deleteDuplicates1(head: ListNode): ListNode = {
    var buf = head
    while (buf != null && buf.next != null ) {
      if (buf.x == buf.next.x) buf.next = buf.next.next
      else buf = buf.next
    }
    head
  }

  //Not good solution
  def deleteDuplicates2(head: ListNode): ListNode = {
    if(head==null ||head.next==null) head
    else{
      val temp = head
      def deleteDuplicatesAux(curr: ListNode, nxt: ListNode, flag:Boolean): ListNode={
        (curr,nxt, flag) match{
          case (c,null, f) => if(f) null else c
          case (c,n,false) if(c.x==n.x) => {
            c.next = deleteDuplicatesAux(c,n.next,true)
            c
          }
          case (c,n,true) if(c.x==n.x) => deleteDuplicatesAux(c,n.next,true)
          case (c,n,_) => {
            deleteDuplicatesAux(n,n.next,false)
            n
          }
        }
      }
      deleteDuplicatesAux(temp,temp.next,false)
      head
    }

  }

  var x = deleteDuplicates(new ListNode(2,new ListNode(2,new ListNode(2,null))))
//  var x = deleteDuplicates(new ListNode(1,new ListNode(2,new ListNode(3,null))))
//  var x = deleteDuplicates(new ListNode(1,new ListNode(1,new ListNode(2,new ListNode(3,null)))))
//  var x = deleteDuplicates(new ListNode(1,new ListNode(1,new ListNode(1,new ListNode(2,new ListNode(3,null))))))
//var x = deleteDuplicates(new ListNode(1,new ListNode(1,new ListNode(2,new ListNode(3,new ListNode(3,null))))))
  while(x!=null){
    println(x.x)
    x=x.next
  }
}


object DifferentImpl{
  implicit class EnhancedListNode(val node: ListNode) extends AnyVal {
    def hasNext(): Boolean = (node != null) && (node.next != null)
    def shouldSkip(): Boolean = (node.x == node.next.x)
    def skip(): Unit = node.next = node.next.next
  }

  def deleteDuplicates(head: ListNode): ListNode = {
    var curr = head
    while (curr.hasNext()) {
      if (curr.shouldSkip()) curr.skip()
      else curr = curr.next
    }

    head
  }
}
