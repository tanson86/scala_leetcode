object AddBinary_LC_67 extends App{

  def addBinary(a: String, b: String): String = {
    val maxLen = Math.max(a.length,b.length)
    val aNorm = a.reverse.padTo(maxLen,'0').reverse
    val bNorm = b.reverse.padTo(maxLen,'0').reverse
    def addBinaryRec(carry:Int,idx:Int):String = {
      if(idx<0){
            carry match {
              case 0 => return ""
              case _ =>  return "1"
            }
      }
      val x = aNorm(idx).asDigit
      val y = bNorm(idx).asDigit
      val sum = x+y+carry
      sum match {
        case 0 => addBinaryRec(0,idx-1)+"0"
        case 1 => addBinaryRec(0,idx-1)+"1"
        case 2 => addBinaryRec(1,idx-1)+"0"
        case 3 => addBinaryRec(1,idx-1)+"1"
      }
    }

    addBinaryRec(0,maxLen-1)
  }

  def addBinary1(a: String, b: String): String = {
//    println(Integer.parseInt(a,2) + Integer.parseInt(b,2).toBinaryString)
    println((BigInt.apply(a,2) + BigInt.apply(b,2)).toString(2))
    ""
  }

  println(addBinary("1","100"))
//  println(addBinary("1010","1011")) //10101
//  println(addBinary("10100000100100110110010000010101111011011001101110111111111101000000101111001110001111100001101","110101001011101110001111100110001010100001101011101010000011011011001011101111001100000011011110011"))
}
