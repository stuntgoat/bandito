package org.hackerschool.banditod.algorithms


case class Softmax(
  val softmaxTempNumerator: Double = 1.0,

  /**
    Map arm names to tuple of (<ratio>, (<numerator>, <denominator>))
    for each arm's counts.
    */
  private var armRatioValues: Map[String, (Double, (Double, Double))] = Map()) extends Algorithm {

  // Holds the total number of pulls to date.
  private var totalDenominator: Double = 0

  def softmaxTemperature(_count: Double): Double = {
    /**
      Calculate the temperature given '_count' argument.
      */
    var count: Double = 0
    if (_count > 0) {
        count = _count
    }

    /** Add 1 and a small fractional value
      to prevent divide by zero errors and values >= 1 */
    this.softmaxTempNumerator / scala.math.log(count + (1 + .1e-7))
  }

  def probsFromRatios(ratios: List[Double], temperature: Double): List[Double] = {
    /**
      Given an iterable of conversions and a temperature, generate
      scaled and normalized probabilities for each.

      Args:
      - conversions: a list of conversion rates.
      - temp: an 'annealing temperature'

      */
    val denominator = ratios.map(x => scala.math.exp(x / temperature)).sum
    ratios.map(x => scala.math.exp(x / temperature) / denominator)
  }
  /**
    Implement the Algorithm abstract class.

    */

  def selectArm(): String = {
    if (this.armRatioValues.size == 1) {
      val arm: String = this.armRatioValues.keys.head
      // Increment arm pull.
      this.totalDenominator += 1
      return arm
    }

    var arms: List[String] = List()
    var ratios: List[Double] = List()
    for ((arm, values) <- this.armRatioValues.iterator) {
      arms ::= arm
      ratios ::= values._1
    }
    val temperature: Double = this.softmaxTemperature(this.totalDenominator)
    val probabilities: List[Double] = this.probsFromRatios(ratios, temperature)
    val arm: String = ListUtils.categoricalChoice(probabilities, arms)
    this.totalDenominator += 1
    arm
  }

  def selectFromSubset(arms: List[String]): String = {
    /** TODO*/
    ""
  }

  def initialize(_names: List[String]) = {
    /**
      - Is this method needed since we initialize during the class instantiation?

      */
    for (name <- _names) {
      this.addArm(name)
    }
  }

  def addArm(name: String): Unit = {
    if (!this.armRatioValues.contains(name)) {
      this.armRatioValues += name -> (0, (0, 0))
    }
  }

  def removeArm(name: String): Boolean = {
    if (!this.armRatioValues.contains(name)) {
      return false
    }
    this.armRatioValues -= name
    return true
  }
  def updateReward(arm: String, reward: Double = 1): Boolean = {
    /** TODO: */
    val armValues = this.armRatioValues.get(arm)
    if (!armValues.isEmpty) {
      for ((ratio, (numerator, denominator)) <- armValues) {
        val newNumerator = numerator + reward
        if (denominator == 0) {
          this.armRatioValues += arm -> (0, (newNumerator, denominator))
        } else {
          val newRatio = newNumerator / denominator
          this.armRatioValues += arm -> (newRatio, (newNumerator, denominator))
        }
      }
      return true
    }
    false
  }
}
