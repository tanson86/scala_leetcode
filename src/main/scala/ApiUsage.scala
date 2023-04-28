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
}
