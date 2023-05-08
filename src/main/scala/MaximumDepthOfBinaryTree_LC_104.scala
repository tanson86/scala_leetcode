import scala.annotation.tailrec

object MaximumDepthOfBinaryTree_LC_104 extends App{

  def maxDepth1(root: TreeNode): Int = {
    if(root==null) return 0
    Math.max(1+maxDepth1(root.left), 1+maxDepth1(root.right))
  }

  def maxDepth(root: TreeNode): Int = {

    import scala.math.max

    def maxDepth_(queue: List[(TreeNode, Int)], maxDepth: Int): Int = {
//      queue match {
//        case Nil => maxDepth
//        case (node, depth) :: tail =>
//
//          val left = Option(node.left).map(n => (n, depth + 1))
//          val right = Option(node.right).map(n => (n, depth + 1))
//
//          val list: List[(TreeNode, Int)] = tail :++ left :++ right
//
//          maxDepth_(list, max(depth, maxDepth))
//      }
        queue match {
          case Nil => maxDepth
          case (node,depth) :: tail =>
            val left = Option(node.left).map(_->depth.+(1))
            val right = Option(node.right).map(_->depth.+(1))
            val list = tail:++left:++right
            maxDepth_(list,max(depth, maxDepth))
        }
    }


    maxDepth_(Option(root).map(_->1).toList, 0)
  }

}
