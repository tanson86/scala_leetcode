class FindDifferenceOfTwoArrays_LC_2215 extends App{
  def findDifference(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = {
    List(nums1.distinct.diff(nums2.distinct).toList,nums2.distinct.diff(nums1.distinct).toList)
  }

  def findDifference1(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = {
    val n1 = nums1.toSet
    val n2 = nums2.toSet

    List(
      (n1 -- n2).toList,
      (n2 -- n1).toList
    )
  }

  def findDifference2(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] =
    List((nums1.toSet -- nums2), (nums2.toSet -- nums1)).map(_.toList)

}
