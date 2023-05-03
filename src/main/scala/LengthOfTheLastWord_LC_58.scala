object LengthOfTheLastWord_LC_58 extends App{
  def lengthOfLastWord(s: String): Int = {
    s.trim.foldRight(0)((c,len)=>{
      c match {
        case x if(x!= ' ') => {
//          println(s"x is $x and len is $len")
          len+1
        }
        case _ => return len+1
      }
    })
  }

  def lengthOfLastWord1(s: String): Int = {
    return s.split(" ").last.length
  }

  def lengthOfLastWord2(s: String) = s.split(" +").last.length

  def lengthOfLastWord3(s: String): Int = {
      var len = 0;
      val sLen = s.trim.length-1
      for(i <- sLen to (0, -1)) {
        if (s.trim.charAt(i) == ' ')
          if(len==0)
            len = sLen - i
      }
    if(len==0)
      sLen+1
    else
      len
  }

  def lengthOfLastWord4(s: String): Int = {
    def lengthOfLastWordAux(st: String, idx:Int): Int = {
      if(idx==0) st.length
      else if(st.charAt(idx)==' ') (st.length-1)-idx
      else lengthOfLastWordAux(st,idx-1)
    }
    lengthOfLastWordAux(s.trim,s.trim.length-1)
  }

//  def lengthOfLastWord3(s: String): Int = {
//
//  }

//  println(lengthOfLastWord4("helo world"))
//    println(lengthOfLastWord4("hi tanson    hi     "))
//  println(lengthOfLastWord4("luffy is still joyboy"))
  println(lengthOfLastWord4("   fly me   to   the moon  "))
//  println(lengthOfLastWord4("a"))
//  println(lengthOfLastWord4("ab"))
  }

