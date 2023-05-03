import scala.annotation.tailrec

object SearchInsertPosition_LC_35 extends App{

  def searchInsert(nums: Array[Int], target: Int): Int = {
    def binarySearch(start:Int,end:Int):Int = {
      if(start>end) return start
      else{
        val mid = start+(end-start)/2
        nums(mid) match {
          case i if(i==target) => return mid
          case i if(i<target) => binarySearch(start+1,end)
          case _ => binarySearch(start,end-1)
        }
      }
    }
    binarySearch(0,nums.length-1)
  }

  def searchInsert1(nums: Array[Int], target: Int): Int = {
    nums.foldLeft(0)((idx,num)=>{
      if(num>=target)  idx
      else idx+1
    })
  }

  def searchInsert2(nums: Array[Int], target: Int): Int = {
    @tailrec
    def loop(index: Int): Int = {
      if (index >= nums.length) index
      else if (nums(index) >= target) index
      else loop(index + 1)
    }

    loop(0)
  }

  println(searchInsert(Array(1,2,4,5),4))
  println(searchInsert(Array(1,2,3,5),6))
}
