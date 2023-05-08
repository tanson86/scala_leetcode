object ClimibingStairs_LC_70 extends App{
  def climbStairs(n: Int): Int = {
    if(n<=3) n
    else{
      (4 to n).foldLeft((2,3))((t,_)=>{
        (t._2,t._1+t._2)
      })._2
    }
  }

  def climbStairs1(n: Int, dp0: Int = 1, dp1: Int = 1): Int =
    if (n > 1) climbStairs1(n - 1, dp1, dp0 + dp1)
    else dp1

  def climbStairs2(n: Int): Int = {
    if(n > 0) {
      val dp = Array.fill(n + 1)(0)
      dp(0) = 1
      dp(1) = 1
      for(i <- 2 to n) dp(i) = dp(i - 1) + dp(i - 2)
      dp(n)
    } else 0
  }

  println(climbStairs(5))
}
