object PlusOne_LC_66 extends App{

  def plusOne(digits: Array[Int]): Array[Int] = {
    val (arr,cond) = digits.foldRight((Array.emptyIntArray , true)) ((d,t)=>{
      t match {
        case (a,false) => ((d+1) +: a, false)
        case (a,true) if(d==9) => (0+:a, true)
        case (a,true) => ((d+1) +: a, false)
      }

    })
    if(cond)
      1+:arr
    else
      arr
  }

  def plusOne1(digits: Array[Int]): Array[Int] = {
    //for loop from the end of array to 0 and -1 every iteration because we want to look at the end of the array first and work backwards
    for (i <- digits.length - 1 to 0 by -1) {
      //if element is < 9 then +1 to it and return the array
      if (digits(i) < 9) {
        digits(i) += 1
        return digits
      }
      //if element is not < 9 then that element becomes 0 and we move onto the next number in the for loop
      digits(i) = 0
    }
    //if the for loop ends and we reach here then it means the left most element in the array was a 9 and it would've become a 0 in the for loop and the code below inserts a 1 to the left of the array to complete it
    1 +: digits
  }

  def plusOne2(digits: Array[Int]): Array[Int] = {
    var carry=1
    digits.zipWithIndex.foldRight(digits)((tup,digits)=>{
      val (num:Int,idx:Int) = tup
      val sum = digits(idx)+carry
      val div = sum/10
      if(div==1) {
        digits(idx)=0
        carry=1
      } else {
        digits(idx)=sum
        carry=0
      }
      digits
    })
    if(carry==1) 1+:digits else digits
  }

  def plusOne3(digits: Array[Int]): Array[Int] = {
    var carry=1
    digits.indices.foldRight(digits)((idx,digits)=>{
      val sum = digits(idx)+carry
      val div = sum/10
      if(div==1) {
        digits(idx)=0
        carry=1
      } else {
        digits(idx)=sum
        carry=0
      }
      digits
    })
    if(carry==1) 1+:digits else digits
  }



  println(plusOne1(Array(9,9)).mkString(","))
}
