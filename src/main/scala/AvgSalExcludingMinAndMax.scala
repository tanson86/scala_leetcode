object AvgSalExcludingMinAndMax extends App{

  def average(salary: Array[Int]): Double = (salary.sum - salary.min - salary.max).toDouble / (salary.length - 2)

  def average1(salary: Array[Int]): Double = (salary.sorted.drop(1).dropRight(1).sum/(salary.length-2)).toDouble

  println(average(Array(4000,3000,1000,2000)))
  println(average(Array(1000,2000,3000)))
  println(10/10000000d)
}
