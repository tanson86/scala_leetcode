object Sqrt_LC_69 extends App {
  def mySqrt(x: Int): Int = {
    if(x<2) return x
    else{

      def mySqrtAux(l: Int,r:Int): Int = {
        val mid:Int = l+ (r-l)/2
        val sqr = mid.toLong*mid
          sqr match {
          case _ if(l>r) => r
          case m if m == x => mid
          case m if m > x => mySqrtAux(l,mid-1)
          case m => mySqrtAux(mid+1,r)
        }
      }
      mySqrtAux(2,x/2)
    }
  }

  println(mySqrt(100))
  println(mySqrt(101))
  println(mySqrt(99))
  println(mySqrt(2147395599))
}
