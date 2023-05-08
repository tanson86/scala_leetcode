import scala.util.chaining.scalaUtilChainingOps

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

  println("1".padTo(3,0)) //Vector(1, 0, 0)
  println("1".padTo(3,'0')) //100

  println("1"(0).toInt-'0') //"1"(0) equivalent to "1".charAt(0)
  println("1".charAt(0).asDigit)
  val x = "x" indexOf ('0',0)
  println("Tap Example"+Array(1,2,3).tapEach(i=>i*2).mkString(","))
  val i:List[Int] = List(1,2,3)
  println(i.map(i=>i*2).mkString(","))

  println(!(true && true))

  val list: List[(String, Int)] =  List(("A",1))
  println(Nil :++ Option(("B",2)) :++ Option(("C",3)))

  println(Nil :+ Option(("B",2)) :+ Option(("C",3)))

  val left = Option(null).tap(println(_)).map(x=>1)
  println(Nil :++left)
}
