object ConvertSortedArrayToBST_LC_108 extends App{

    def sortedArrayToBST(nums: Array[Int]): TreeNode = {
      def sortedArrayToBSTAux(l:Int, r:Int): TreeNode = {
        if(l>r) null

        val mid = l+((r-l)/2)
        new TreeNode(nums(mid), sortedArrayToBSTAux(l,mid-1), sortedArrayToBSTAux(mid+1,r))
      }
      sortedArrayToBSTAux(0,nums.length-1)
    }

  val tn = sortedArrayToBST(Array(-10))
  tn
}
