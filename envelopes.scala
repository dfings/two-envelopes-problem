// $ brew install scala
// $ scala envelopes.scala
// or
// $ scalac envelopes.scala
// $ scala Envelopes

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
  def singleTrial(cutoff: Int) : Float = {
    val lowerValue = random.nextFloat() * PRIOR_LOWER_MAX
    val higherValue = 2 * lowerValue
    if (random.nextBoolean()) {
      if (lowerValue >= cutoff) lowerValue else higherValue
    } else { 
      if (higherValue >= cutoff) higherValue else lowerValue
    }
  }

  /** Runs many trials at a given cutoff to approximate the expected value. */
  def multiTrial(cutoff: Int) : Float = {
    var totalResult = 0f
    for (i <- 0 until NUM_TRIALS) {
      totalResult += singleTrial(cutoff)
    }
    return totalResult / NUM_TRIALS
  }
  
  def main(args: Array[String]) {
    for (cutoff <- 0 until 2 * PRIOR_LOWER_MAX + 1) {
      val expectedValue = multiTrial(cutoff)
      println(s"cutoff=$cutoff, expected_value=$expectedValue")
    }
  }
}