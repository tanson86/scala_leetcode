object PalindromeNumber_LC_9 extends App {
  def isPalindrome(x: Int): Boolean = {
    if(x<0) return false
    if(x<10) return true
    val s:String = x.toString
    def isPalindromeAux(start:Int,end:Int):Boolean = {
      if(start>=end) true
      else if(s.charAt(start)==s.charAt(end)) isPalindromeAux(start+1,end-1)
      else false
    }
    isPalindromeAux(0,s.length-1)
  }

  def isPalindrome1(x: Int): Boolean = {
      x.toString == x.toString.reverse
  }

  def isPalindrome2(x: Int): Boolean = {
    Option.when(x>0) (reverse(x,0)).fold(false)(x==_)
  }

  def reverse(x:Int,res:Int):Int = {
    x match {
      case 0 => res
      case _ => reverse(x/10,res*10+x%10)
    }
  }

  def reverse1(input: Int, result: Int): Int =
    Option.when(input > 0)(reverse(input/10, result*10+input%10))
      .getOrElse(result)

  println(reverse(1995,0))
  println(isPalindrome1(123421))
}
