package org.hackerschool.banditod.algorithms

import scala.util.Random


object Util {

  def maxDoubleIndexes(values: List[Double], currentIndex: Int, maximum: Double, indexes: List[Int]): List[Int] = {
    (values.length, indexes.length) match {
      case (0, 0) => List()
      case (0, _) => indexes
      case (_, _) => {
        values.head match {
          case v == maximum => this.maxDoubleIndexes(values.tail, currentIndex + 1, maximum, indexes ++ List(currentIndex))
          case v > maximum => this.maxDoubleIndexes(values.tail, currentIndex + 1, v, List(currentIndex))
          case _ => this.maxDoubleIndexes(values.tail, currentIndex + 1, maximum, indexes)
        }
      }
    }
  }

  def maxIndex(items: List[Double]): Int = {
    items.length switch {
      case 0 => -1
      case 1 => 0
      case _ => {
        val indexes: List[Int] = this.maxDoubleIndexes(items)
        items(indexes(Random.nextInt(indexes.length)))
      }
    }
  }
}