object SignOfTheProductOfANumber_LC_1822 extends App{
  def arraySign(nums: Array[Int]): Int = {
    nums.map(_.sign).reduce(_*_)
    //nums.map(_.sign).product
  }

  def arraySign1(nums: Array[Int]): Int = {
    nums.collect {
      case i if i<0 => -1
      case i if i>0 => 1
      case _ => 0
    }.product
  }

  def arraySign2(nums: Array[Int]): Int = (nums foldLeft 1) {_ * _.sign}

  println(arraySign(Array(1,5,0,2,-3)))
//  println(arraySign(Array(-1,-2,-3,-4,3,2,1)))
//  println(arraySign(Array(41,65,14,80,20,10,55,58,24,56,28,86,96,10,3,84,4,41,13,32,42,43,83,78,82,70,15,-41)))

}
