// $ ~/GitHub/scala-2.11.5/bin/scala envelopes.scala

import scala.util.Random

/** Monte Carlo simulation of the two envelopes problem. */
object Envelopes {
  
  val NUM_TRIALS = 10000
  var PRIOR_LOWER_MAX = 100
  val random = new Random
  
  /**
   * Runs a single trial where an envelope is chosen.  If the chosen envelope has a value < cutoff,
   * the function will switch envelopes, otherwise it will keep the envelope it has chosen. Returns
   * the value of the envelope it ultimately selects.
   */
  def singleTrial(cutoff: Int) : Int = {
    val lowerValue = random.nextInt(PRIOR_LOWER_MAX)
    val higherValue = 2 * lowerValue
    if (random.nextInt(2) == 0) {
      if (lowerValue >= cutoff) lowerValue else higherValue
    } else { 
      if (higherValue >= cutoff) higherValue else lowerValue
    }
  }

  /** Runs many trials at a given cutoff to approximate the expected value. */
  def multiTrial(cutoff: Int) : Float = {
    var totalResult = 0
    for (i <- 0 until NUM_TRIALS) {
      totalResult += singleTrial(cutoff)
    }
    return totalResult.toFloat / NUM_TRIALS
  }
  
  def main(args: Array[String]) {
    for (cutoff <- 0 until 2 * PRIOR_LOWER_MAX) {
      val expectedValue = multiTrial(cutoff)
      println(s"cutoff=$cutoff, expected_value=$expectedValue")
    }
  }
}