package dev.krusling.rockthejvm

import scala.annotation.tailrec

object SortingDemo {

  def sortList(list: List[Int]): List[Int] = {
    // insertion sort
    // Example: insert(2, [1,3,4]) = [1,2,3,4]
    /*
      insert(3, [1,2,4]) =
      1 :: insert(3, [2,4]) =
      1 :: 2 :: insert(3, [4]) =
      1 :: 2 :: 3 :: 4 =
      [1,2,3,4]
     */
    def insert(number: Int, sortedList: List[Int]): List[Int] =
      if (sortedList.isEmpty || number <= sortedList.head) number :: sortedList
      else sortedList.head :: insert(number, sortedList.tail)

    if (list.isEmpty || list.tail.isEmpty) list
    else insert(list.head, sortList(list.tail))

    /*
      sortList([4,3,2,1]) =
      insert(4, sortList([3,2,1])) = insert(4, [1,2,3]) = [1,2,3,4]

      sortList([3,2,1]) =
      insert(3, sortList([2,1])) = insert(3, [1,2]) = [1,2,3]

      sortList([2,1]) =
      insert(2, sortList([1]) = insert(2, [1]) = [1,2]

      sortList([1]) = [1]
     */
  }

  // TAIL RECURSION
  def sortBetter(list: List[Int]): List[Int] = {
    /*
      insertTailRec(4, [1,2,3,5], []) =
      insertTailRec(4, [2,3,5], [1]) =
      insertTailRec(4, [3,5], [2,1]) =
      insertTailRec(4, [5], [3,2,1]) =
      [1,2,3] ++ (4 :: [5]) =
      [1,2,3,4,5]

     */
    @tailrec
    def insertTailrec(number: Int, sortedList: List[Int], accumulator: List[Int]): List[Int] =
      if (sortedList.isEmpty || number <= sortedList.head) accumulator.reverse ++ (number :: sortedList)
      else insertTailrec(number, sortedList.tail, sortedList.head :: accumulator)

    @tailrec
    def sortTailRec(list: List[Int], accumulator: List[Int]): List[Int] = {
      if (list.isEmpty) accumulator
      else sortTailRec(list.tail, insertTailrec(list.head, accumulator, Nil))
    }

    sortTailRec(list, Nil)
  }


  def main(args: Array[String]): Unit = {
    println(sortBetter((1 to 1000000).reverse.toList))
  }


}
