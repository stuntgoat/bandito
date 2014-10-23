package org.hackerschool.banditod.algorithms

import scala.util.Random
import scala.annotation.tailrec

object ListUtils {
  /** Used to get all indices for each of the maximum values in a List[Double] */
  @tailrec
  def maxDoubleIndices(values: List[Double], currentIndex: Int, maximum: Double, indices: List[Int]): List[Int] = {
    (values.length, indices.length) match {
      case (0, _) => indices
      case (_, _) => {
        values.head match {
          case v if v == maximum => this.maxDoubleIndices(values.tail, currentIndex + 1, maximum, currentIndex :: indices)
          case v if v > maximum => this.maxDoubleIndices(values.tail, currentIndex + 1, v, List(currentIndex))
          case _ => this.maxDoubleIndices(values.tail, currentIndex + 1, maximum, indices)
        }
      }
    }
  }

  /** Used to select a random index from the maximum value(s) in a List[Double]. */
  def randMaxIndex(items: List[Double]): Int = {
    items.length match {
      case 0 => -1
      case 1 => 0
      case _ => {
        val indices: List[Int] = this.maxDoubleIndices(items, 0, Double.NegativeInfinity, List())
        val randIdx: Int = Random.nextInt(indices.length)
        indices(randIdx)
      }
    }
  }

  def categoricalChoice(probabilities: List[Double], items: List[String]): String = {
    /**
      Select an item from a list, `items`, using a corresponding
      set of probabilities, probabilities`.

      Args:
      - probabilities: a list of probabilities, corresponding to `items`
      - items: items to be selected.

      Returns an item selected from `items`.

      */
    val rand = Random.nextDouble()
    var cum_prob: Double = 0.0

    for ((prob, idx) <- probabilities.zipWithIndex) {
      cum_prob += prob
      if (cum_prob > rand) {
        return items(idx)
      }
    }
    items.last
  }
}
