package org.hackerschool.banditod.algorithms

import scala.util.Random


case class Softmax(
  var names: List[String] = List[String](),
  var rewards: List[Double] = List[Double](),
  var pulls: List[Double] = List[Double](),
  var ratios: List[Double] = List[Double]()) extends Algorithm {

  private var minPullsOK: Boolean = false

  private var MIN_PULLS = 20
  private var softmaxTempNumerator: Double = 0

  // Holds the total number of pulls to date.
  private var totalDenominator: Double = 0

  def softmaxTemperature(_count: Double): Double = {
    var count: Double = 0
    if (_count > 0) {
        count = _count
    }

    /**
      Add 1 and a small fractional value
      to prevent divide by zero errors and values >= 1
      */
    this.softmaxTempNumerator / scala.math.log(count + (1 + .1e-7))
  }

  def equalProbs(ratios: List[Double]): List[Double] = {
    val rlen = ratios.length
    ratios.map(x => {1.0 / rlen.toDouble})
  }

  def probsFromRatios(ratios: List[Double], temperature: Double): List[Double] = {
    /**
      Given an iterable of conversions and a temperature, generate
      scaled and normalized probabilities for each.

      Args:
      - conversions: a list of conversion rates.
      - temp: an 'annealing temperature'

      */
    val rlen: Int = ratios.length
    rlen match {
      case 0 => List()
      case 1 => List(1.0)
      case _ => {
        val denominator = ratios.map(x => scala.math.exp(x / temperature)).sum
        ratios.map(x => scala.math.exp(x / temperature) / denominator)
      }
    }
  }

  /**
    Implement the Algorithm abstract class.

    */

  def selectArm(): String = {
    val lenArms = this.names.length

    if (lenArms == 1) {
      val arm: String = this.names.head
      this.updatePulls(arm, 1)
      return arm
    } else if (lenArms == 0) {
      return ""
    }

    /**
      If we have not acquired the minimum number of pulls for each arm,
      select a random arm.
      */
    if (this.minPullsOK == false) {
      val rIndex = Random.nextInt(lenArms)
      val arm: String = this.names(rIndex)
      this.updatePulls(arm, 1)
      return arm
    }

    val temperature: Double = this.softmaxTemperature(this.totalDenominator)
    val probabilities: List[Double] = this.probsFromRatios(this.ratios, temperature)
    val arm: String = ListUtils.categoricalChoice(probabilities, this.names)
    this.updatePulls(arm, 1)
    arm
  }

  def selectFromSubset(arms: List[String]): String = {
    val lenArms = arms.length
    if (lenArms == 1) {
      val arm: String = arms.head
      this.updatePulls(arm, 1)
      return arm
    } else if (lenArms == 0) {
      return ""
    }

    val indexes = arms.map(name => {this.names.indexOf(name)}).filter(idx => idx >= 0)
    var nSubset: List[String] = List()
    var rSubset: List[Double] = List()
    var pSubset: List[Double] = List()


    for (idx <- indexes) {
      nSubset ::= this.names(idx)
      rSubset ::= this.ratios(idx)
      pSubset ::= this.pulls(idx)
    }
    val temperature: Double = this.softmaxTemperature(pSubset.sum)
    val probabilities: List[Double] = this.probsFromRatios(rSubset, temperature)
    val arm: String = ListUtils.categoricalChoice(probabilities, nSubset)
    this.updatePulls(arm, 1)
    arm
  }

  def initialize(names: List[String]) = {
    for (name <- names) {
      this.addArm(name)
    }
  }

  def setTemp(temp: Double) = {this.softmaxTempNumerator = temp}

  def addArm(name: String): Unit = {
    if (!this.names.contains(name)) {
      /** Seed the arm with on pull and one */
      this.totalDenominator += 1
      this.rewards ::= 1
      this.pulls ::= 1
      this.ratios ::= 1
      this.names ::= name
    }
  }

  def removeArm(name: String): Boolean = {
    val armIdx = this.names.indexOf(name)
    if (armIdx < 0) {
      return false
    }
    this.names = this.names.take(armIdx) ++ this.names.drop(armIdx+1)
    this.rewards = this.rewards.take(armIdx) ++ this.rewards.drop(armIdx+1)
    this.pulls = this.pulls.take(armIdx) ++ this.pulls.drop(armIdx+1)
    this.ratios = this.ratios.take(armIdx) ++ this.ratios.drop(armIdx+1)
    return true
  }

  def updatePulls(arm: String, _pulls: Double = 1): Boolean = {
    if (_pulls <= 0) {
      return false
    }

    val armIdx = this.names.indexOf(arm)
    if (armIdx < 0) {
      return false
    }
    val currentPulls = this.pulls(armIdx)
    val currentRewards = this.rewards(armIdx)

    val newDenominator = currentPulls + _pulls
    val newRatio = currentRewards / newDenominator
    this.totalDenominator += _pulls

    this.ratios = this.ratios.updated(armIdx, newRatio)
    this.pulls = this.pulls.updated(armIdx, newDenominator)
    if (this.minPullsOK == false) {
      val minPulls = this.pulls.min
      if (minPulls >= this.MIN_PULLS) {
        this.minPullsOK = true
      }
    }
    return true
  }



  def updateReward(arm: String, reward: Double = 1): Boolean = {
    if (reward <= 0) {
      return false
    }

    val armIdx = this.names.indexOf(arm)
    if (armIdx < 0) {
      return false
    }
    val currentPulls = this.pulls(armIdx)
    val currentRewards = this.rewards(armIdx) + reward

    val newRatio = currentRewards / currentPulls

    this.ratios = this.ratios.updated(armIdx, newRatio)
    this.rewards = this.rewards.updated(armIdx, currentRewards)

    return true
  }
}
