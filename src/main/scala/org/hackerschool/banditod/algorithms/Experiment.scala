package org.hackerschool.banditod

import org.hackerschool.banditod.algorithms.{Algorithm, EpsilonGreedy}


case class Experiment(name: String, algorithm: Algorithm)

object Experiment {
  def fromString(name: String): Experiment = {
    val algorithm = EpsilonGreedy()
    Experiment(name, algorithm)
  }
}
