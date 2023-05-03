object IndexOfFirstOccurenceOfString extends App{

  def strStr(haystack: String, needle: String): Int = {
    needle.isEmpty match {
      case true => 0
      case _ => haystack.sliding(needle.length,1).zipWithIndex.collectFirst{
        case (`needle`,idx) => idx // equivalent to case(s,idx) if(x==needle) => idx
      }.getOrElse(-1)
    }
  }
  //Not the best solution but the logic is very simple
  def strStr1(haystack: String, needle: String): Int = {
    var start = 0
    var end = needle.length
    if(needle.length>haystack.length) -1
    else{
      while(end<=haystack.length){
        if(haystack.substring(start,end)==needle) return start
        start+=1
        end+=1
      }
      -1
    }

  }

  println(strStr1("a","a"))
}
