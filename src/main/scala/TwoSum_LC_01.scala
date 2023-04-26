object TwoSum_LC_01 extends App {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    //Approach 1
    def twoSumRec(idx1:Int, idx2:Int ):Array[Int] = {
      if(idx1>=nums.length-1) Array.emptyIntArray
      else if(idx1!=idx2 && nums(idx1)+nums(idx2) == target) Array(idx1,idx2)
      else if(idx2<nums.length-1) twoSumRec(idx1,idx2+1)
      else twoSumRec(idx1+1,idx1+2)
    }
    twoSumRec(0,1);
    //Aproach 2
    def twoSumAux(index:Int, map:Map[Int,Int]):Array[Int] = {
      val diff:Int = target-nums(index)
      map.get(diff)
      if(map.contains(diff)) Array(index,map(diff))
      else twoSumAux(index+1,map+(nums(index)->index))
    }
    twoSumAux(0,Map.empty)
    //Approach 3
    val map:Map[Int,Int] = Map.empty;
    for((value,index)<-nums.zipWithIndex){
    map.get(target-value).foreach(x=> return Array(x,index))
    map+(value->index)
  }
    Array(0,0)
  }

  println(twoSum(Array(3,8,7,6), 9).mkString(","))
}
