// $ brew install scala
// $ scala envelopes.scala
// or
// $ scalac envelopes.scala
// $ scala main

/** Monte Carlo simulation of the two envelopes problem. */
val NUM_TRIALS = 10_000
var PRIOR_LOWER_MAX = 100
val random = scala.util.Random()

/**
 * Runs a single trial where an envelope is chosen.  If the chosen envelope has a value < cutoff,
 * the function will switch envelopes, otherwise it will keep the envelope it has chosen. Returns
 * the value of the envelope it ultimately selects.
 */
def singleTrial(cutoff: Int) =
  val lowerValue = random.nextDouble() * PRIOR_LOWER_MAX
  val higherValue = 2 * lowerValue
  if random.nextBoolean() 
    then (if lowerValue >= cutoff then lowerValue else higherValue)
    else (if higherValue >= cutoff then higherValue else lowerValue)

/** Runs many trials at a given cutoff to approximate the expected value. */
def multiTrial(cutoff: Int) =
  (1 to NUM_TRIALS).map { _ => singleTrial(cutoff) }.sum / NUM_TRIALS

@main def main() =
  for cutoff <- 0 to 2 * PRIOR_LOWER_MAX do
    println(s"cutoff=$cutoff, expected_value=${multiTrial(cutoff)}")