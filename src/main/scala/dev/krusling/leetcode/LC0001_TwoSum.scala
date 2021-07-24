package dev.krusling.leetcode

// https://leetcode.com/problems/two-sum/
object LC0001_TwoSum extends App {

  // Two pass approach. Add all (value, index) to hashmap, then find complement in second pass.
  def twoSum1(nums: Array[Int], target: Int): Array[Int] = {
    val seen = Map.from(nums.zipWithIndex)
    nums.zipWithIndex.collectFirst(new PartialFunction[(Int, Int), Array[Int]] {
      override def isDefinedAt(x: (Int, Int)): Boolean = seen.contains(target-x._1) && seen(target-x._1) != x._2
      override def apply(x: (Int, Int)): Array[Int] = Array[Int](x._2, seen(target-x._1))
    }).get
  }

  // Same two pass approach, but a bit more elegant
  // Credit: ibtawfik
  def twoSum2(nums: Array[Int], target: Int): Array[Int] = {
    val withIndex = nums.zipWithIndex
    val valueIndexMap = withIndex.toMap
    withIndex.collectFirst{
      case (value, index) if valueIndexMap.get(target - value).exists(_ != index) =>
        Array(index, valueIndexMap(target - value))
    }.get
  }
}
