#!/usr/bin/env groovy

NUM_TRIALS = 10000
PRIOR_LOWER_MAX = 100
random = new Random()

/**
 * Runs a single trial where an envelope is chosen.  If the chosen envelope has
 * a value < cutoff, the function will switch envelopes, otherwise it will keep
 * the envelope it has chosen. Returns the value of the envelope it ultimately 
 * selects.
 */
def singleTrial(pick) {
  lowerValue = random.nextDouble() * PRIOR_LOWER_MAX
  higherValue = 2 * lowerValue
  return random.nextBoolean() ? pick(lowerValue, higherValue) : pick(higherValue, lowerValue)
}

/** Runs many trials at a given cutoff to approximate the expected value. */
def multiTrial(cutoff) {
  pick = {value, other -> value >= cutoff ? value : other}
  total = 0.0
  for (i in 0 .. NUM_TRIALS) {
    total += singleTrial(pick)
  }
  return total / NUM_TRIALS
}

for (cutoff in 0 .. (2 * PRIOR_LOWER_MAX)) {
  expectedValue = multiTrial(cutoff)
  println "cutoff=$cutoff, expected_value=$expectedValue"
}