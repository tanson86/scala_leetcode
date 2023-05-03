object RemoveDuplicatesFromSortedArray_LC_26 extends App{

  def removeDuplicates(nums: Array[Int]): Int = {
    nums.foldLeft((1,nums(0))) ((tuple, current) => {
      val (uniqueCount:Int, previous:Int) = tuple
      if(previous==current) (uniqueCount,current)
      else{
        nums(uniqueCount) = current
        (uniqueCount+1,current)
      }
    })._1
  }

  def removeDuplicates1(nums: Array[Int]): Int = {
    val distinct = nums.distinct
    distinct.zipWithIndex.foreach((num,idx)=> nums(idx)=num)
    println(nums.mkString)
    distinct.size
  }

  def removeDuplicates2(nums: Array[Int]): Int = {
    val distinct = nums.distinct
    distinct.zipWithIndex.foreach{case(num,idx)=> nums(idx)=num}
    println(nums.mkString)
    distinct.size
  }

  def removeDuplicates3(nums: Array[Int]): Int = {
    if(nums.isEmpty) 0
    else{
      var i=0
      for(j<-(1.until(nums.length))){
        if(nums(i) != nums(j)){
          i+=1
          nums(i) = nums(j)
        }
      }
      i+1
    }
  }

  removeDuplicates(Array(1,1,2))
}
