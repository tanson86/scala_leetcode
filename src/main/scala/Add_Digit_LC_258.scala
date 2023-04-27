object Add_Digit_LC_258 extends App {

  private def add(s:String) = s.split("").map(x=>x.toInt).sum

  def addDigits(num: Int): Int = {
    val s:String = add(num.toString).toString
    if(s.length==1) s.toInt else addDigits(s.toInt)
  }

  //Approach using congruence formula
  def addDigits1(num: Int): Int = {
    if (num == 0) 0
    else if (num % 9 == 0) 9
    else num % 9
  }

  @scala.annotation.tailrec
  def addDigits3(num: Int): Int = {
    println("called")
    val s = num.toString
    val sums = (str: String) => str.foldLeft(0)(_ + _.toString.toInt)

    s.length match {
      case 1 => num
      case _ => addDigits3 (sums(s))
    }
  }
  println(addDigits3(256))
}
