#!/usr/bin/env xcrun swift

import Darwin  // For arc4random, etc.

let NUM_TRIALS = 1000000
let PRIOR_LOWER_MAX = 100

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
func single_trial(cutoff: Double) -> Double {
  let lower_value = Double(PRIOR_LOWER_MAX) * Double(arc4random()) /  Double(UInt32.max)
  let higher_value = 2 * lower_value
  let choice = arc4random_uniform(2)
  if choice == 0 {
    return lower_value >= cutoff ? lower_value : higher_value
  } else {
    return higher_value >= cutoff ? higher_value : lower_value
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
func multi_trial(cutoff: Int) -> Double {
  let cutoff64 = Double(cutoff)
  var total = 0.0
  for _ in 1...NUM_TRIALS {
    total += single_trial(cutoff64)
  }
  return total / Double(NUM_TRIALS)
}

for cutoff in 0...(2 * PRIOR_LOWER_MAX) {
  let expected_value = multi_trial(cutoff)
  println("cutoff=\(cutoff), expected_value=\(expected_value)")	
}
