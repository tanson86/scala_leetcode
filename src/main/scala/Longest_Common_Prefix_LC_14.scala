object Longest_Common_Prefix_LC_14 extends App{

//  println(
//    Array("abc","abe","abcd").map(_.toList.take(3))
//    .toList  //List(List(a, b, c), List(a,b,e), List(a, b, c))
//    .transpose //List(List(a, a, a), List(b, b, b), List(c, e, c))
//    .takeWhile(lst => lst.forall(_ == lst.head)) //List(List(a, a, a), List(b, b, b))
//    .map(_.head) //List(a,b)
//    .mkString //ab
//  )

  def longestCommonPrefix(strs: Array[String]): String = {
    val min = strs.sortBy(_.length).head.length
    val minSize = strs.map(_.size).min
    strs.map(_.toList.take(min))
      .toList  //List(List(a, b, c), List(a,b,e), List(a, b, c))
      .transpose //List(List(a, a, a), List(b, b, b), List(c, e, c))
      .takeWhile(lst => lst.forall(_ == lst.head)) //List(List(a, a, a), List(b, b, b))
      .map(_.head) //List(a,b)
      .mkString //ab
  }
}
