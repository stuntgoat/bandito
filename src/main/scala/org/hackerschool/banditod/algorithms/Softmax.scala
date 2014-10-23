package org.hackerschool.banditod.algorithms

import scala.util.Random


case class Sofmax(
  val softmaxTempNumerator: Double = 1.0,

  /** Maps from arm name to ratio of numerator to denominator. */
  private var armRatioValues: Map[String, (Double, (Double, Double))] = Map()) extends Algorithm {

  def softmaxTemperature(_counts: Double): Double = {
    var counts: Double = 0
    if (_counts > 0) {
        counts = _counts
    }

    /** Add 1 and a small fractional value
      to prevent infinity and divide by zero errors and numbers. */
    this.softmaxTempNumerator / scala.math.log(counts + (1 + .1e-7))
  }

  /**
    Implement the Algorithm abstract class.

    */

  def selectArm(): String = {
    /** TODO*/
    ""
  }

  def selectFromSubset(arms: List[String]): String = {
    /** TODO*/
    ""
  }

  def initialize(_names: List[String]) = {
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
    return false
  }
}
