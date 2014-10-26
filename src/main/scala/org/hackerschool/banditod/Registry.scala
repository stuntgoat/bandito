package org.hackerschool.banditod

import org.hackerschool.banditod.algorithms.{Algorithm, EpsilonGreedy, Softmax}


object Registry {
  private var _experiments: Map[String, Experiment] = Map()

  def algorithms[T >: Algorithm](name: String): Option[T] = {
    name match {
      case "EpsilonGreedy" => Some(new EpsilonGreedy)
      case "Softmax" => Some(new Softmax)
      case _ => None
    }
  }

  def register(experiment: Experiment): Boolean = {
    if (_experiments contains experiment.name) {
      false
    } else {
      _experiments = _experiments + (experiment.name -> experiment)
      true
    }
  }

  def delete(experiment: String): Boolean = {
    if (_experiments contains experiment) {
      _experiments = _experiments - experiment
      true
    } else {
      false
    }
  }

  def experiments(): Map[String, Experiment] = {
    _experiments
  }
}
