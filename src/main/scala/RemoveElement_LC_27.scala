object RemoveElement_LC_27 extends App{
  def removeElement(nums: Array[Int], `val`: Int): Int = {
    val filter = nums.filter(_!=`val`)
    filter.zipWithIndex.foreach{case(num,idx) => nums(idx) = num}
    println(nums.mkString)
    filter.length;
  }

  def removeElement1(nums: Array[Int], `val`: Int): Int = {
    val filter = nums.filter(_!=`val`)
    filter.indices.foldLeft(nums)((num,idx)=>{
      num(idx)=filter(idx)
      num
    })
    filter.length
  }

  def removeElement2(nums: Array[Int], `val`: Int): Int = {
    nums.foldLeft(0){
      case (idx, value) if(value!=`val`) => {
        nums(idx) = value
        idx+1
      }
      case (idx,_) => {
        idx
      }
    }
  }

  def removeElement3(nums: Array[Int], `val`: Int): Int = {
    val x=`val`
    var i=0;
    for(j<-0 until(nums.length)){
      if(nums(j)!=x) {
        nums(i) = nums(j)
        i += 1
      }
    }
    println(nums.mkString)
    i
  }
  println(removeElement2(Array(1,3,2,5),3))
//  println(removeElement(Array(2,3,3,2),3))
//  println(removeElement(Array(0,1,2,2,3,0,4,2),2))
}
