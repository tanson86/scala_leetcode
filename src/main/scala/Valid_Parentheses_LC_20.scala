object Valid_Parentheses_LC_20 extends App{
  import scala.collection.mutable
  val map = Map(')'->'(', '}'->'{',']'->'[')
  def isValid(s: String): Boolean = {
    val stack = new mutable.Stack[Char]()
    s.foreach{
      case c if map.contains(c) => {
        if(stack.isEmpty || map(c) != stack.pop())
        return false
      }
      case _ => stack.push(_)
    }
    stack.isEmpty
  }

  def isPair(c1:Char,c2:Char):Boolean = c1=='(' && c2==')' || c1=='{' && c2=='}' || c1=='[' && c2==']'

  def isValid_1(s: String): Boolean = {
    def isValidAux(index:Int, stack:List[Char]):Boolean = {
      if(index>=s.length)
        stack.isEmpty
      else {
        s.charAt(index) match {
          case c @ ('(' |'{'|'[') => isValidAux(index+1,s.charAt(index)+:stack)
          case c @ (')' |'}'|']') if(stack.isEmpty || !isPair(stack.head,c)) => false
          case c => isValidAux(index+1,stack.tail)
        }
      }

    }
    isValidAux(0,List.empty)
  }

  def isValid_2(s: String): Boolean = {
    s.foldLeft(List.empty[Char])((acc,c)=>{
      c match {
        case m @ ('('|'{'|'[') => m::acc
        case m @(')'|'}'|']') => if(acc.isEmpty || !isPair(acc.head,m)) return false
        else acc.tail
      }
    }).isEmpty
  }

}
