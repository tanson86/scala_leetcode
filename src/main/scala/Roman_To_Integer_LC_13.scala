object Roman_To_Integer_LC_13 extends App{
  def romanToInt(s: String): Int = {
    val list:List[Int] = s.map( x=>x match {
      case 'I'  =>  1
      case 'V'  =>   5
      case 'X'  =>  10
      case 'L'  =>  50
      case 'C'  =>  100
      case 'D'  =>  500
      case 'M'  =>  1000
    }
    ).toList

    def intNumber(l:List[Int]):Int = {
      l match {
        case h::Nil => h
        case h::t => {
          if(h>=t.head) h+intNumber(t)
          else -h+intNumber(t)
        }
        case _ => 0
      }
    }

    intNumber(list)
  }

  println(romanToInt("LVIII"))
  println(romanToInt("MCMXCIV"))
}
