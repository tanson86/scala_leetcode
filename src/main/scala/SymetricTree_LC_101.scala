import scala.annotation.tailrec
import scala.collection.mutable

object SymetricTree_LC_101 extends App{

  def isSymmetric(root: TreeNode): Boolean = {
    val que = new mutable.Queue[TreeNode]()
    que.enqueue(root.left)
    que.enqueue(root.right)
    while(!que.isEmpty){
      val left = que.dequeue()
      val right = que.dequeue()
      if(left==null && right==null){

      }
      else if(left!=null && right==null || left==null && right!=null || left.value != right.value) return false
      else{
        que.enqueue(left.left)
        que.enqueue(right.right)
        que.enqueue(left.right)
        que.enqueue(right.left)
      }

    }
    true
  }

  case class WorkItem(left: TreeNode, right: TreeNode)

  def isSymmetric2(root: TreeNode): Boolean = {
  @tailrec
  def recurse(work: List[WorkItem]): Boolean = {
    // Using a partial function literal doesn't work here very nicely because of the recursion.
    work match {
      case workItem :: workTail =>
        val left = workItem.left
        val right = workItem.right
        if (left == null && right == null) recurse(workTail)
        else if (left == null || right == null || left.value != right.value)
          false
        else
          recurse(
            WorkItem(left.left, right.right) :: WorkItem(
              left.right,
              right.left
            ) :: workTail
          )
      case Nil => true
    }
  }

  recurse(List(WorkItem(root.left, root.right)))
}
  
  def isSymmetric1(root: TreeNode): Boolean = {
    def isSymmetricAux(t1: TreeNode,t2: TreeNode): Boolean = {
      if((t1==null && t2!=null) || (t1!=null && t2==null) || (t1.value != t2.value)) false
      else if(t1==null && t2==null) true
      else
        isSymmetricAux(t1.left,t2.right) && isSymmetricAux(t1.right,t2.left)
    }
    isSymmetricAux(root.left,root.right)
  }

  val treeNode = new TreeNode(1,new TreeNode(2, new TreeNode(3),new TreeNode(4)),new TreeNode(2,new TreeNode(4),new TreeNode(3)))
//val treeNode = new TreeNode(1,new TreeNode(2, null,new TreeNode(3)),new TreeNode(2,null,new TreeNode(3)))
//val treeNode = new TreeNode(1,new TreeNode(2, new TreeNode(3),new TreeNode(4)),new TreeNode(2,null,new TreeNode(3)))
  println(isSymmetric(treeNode))
}
