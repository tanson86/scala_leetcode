object ApiUsage extends App{

  val donuts = List("A","B","C")
  //Example of a for all
  println(donuts.forall(_ == donuts.head))
  val prices = List(2,4,8);
  //Example of a zip
  println(donuts.zip(prices))
  //List of lists
  println(List(donuts, prices))
  //Example of a transpose
  println(List(donuts, prices).transpose)
  //for each with string
  "(-)".foreach(x=>x match {
    case c if (c == '(' || c == ')') => println("contains")
    case c => println("not contains")
  })
  "(-)".foreach(x=>x match {
    case c @ ('(' | ')') => println("contains")
    case c => println("not contains")
  })

  val arr = Array(1,2,3,4)
  println(arr.drop(1).map(_=>arr(0)*2).mkString)
  println(arr.indices.mkString)
  println("abc".appended('d'))

  println("abcdefg".sliding(3,1).zipWithIndex.mkString)
  println("abcdefg".sliding(3,1).zipWithIndex.collectFirst{ case (s,i) if(s=="cde") => {
    i
  }}.getOrElse(-1))

  println(Array(1,2,3,3).diff(Array(1,1,2,2)).mkString(","))
}
