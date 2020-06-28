#!/usr/bin/env swift
// or
// $ swiftc -o envelopes envelopes.swift; ./envelopes

let NUM_TRIALS = 10000
let PRIOR_LOWER_MAX = 100

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
func single_trial(_ cutoff: Double) -> Double {
  func pick(_ value: Double, _ other: Double) -> Double { return value >= cutoff ? value : other }
  let lower_value = Double.random(in: 0..<1) * Double(PRIOR_LOWER_MAX)
  let higher_value = 2 * lower_value
  return Bool.random() ? pick(lower_value, higher_value) : pick(higher_value, lower_value)
}

// Runs many trials at a given cutoff to approximate the expected value.
func multi_trial(_ cutoff: Double) -> Double {
  var total = 0.0
  for _ in 1...NUM_TRIALS {
    total += single_trial(cutoff)
  }
  return total / Double(NUM_TRIALS)
}

for cutoff in 0...(2 * PRIOR_LOWER_MAX) {
  let expected_value = multi_trial(Double(cutoff))
  print("cutoff=\(cutoff), expected_value=\(expected_value)")	
}
