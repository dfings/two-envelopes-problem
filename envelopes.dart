#!/usr/bin/env dart

// $ brew tap dart-lang/dart
// $ brew install dart

import 'dart:math';

const int NUM_TRIALS = 10000;
const int PRIOR_LOWER_MAX = 100;
final Random rng = new Random();

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
double singleTrial(int cutoff) {
  double lower_value = rng.nextDouble() * PRIOR_LOWER_MAX;
  double higher_value = 2 * lower_value;
  if (rng.nextBool()) {
    return lower_value >= cutoff ? lower_value : higher_value;
  } else {
    return higher_value >= cutoff ? higher_value : lower_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
double multiTrial(int cutoff) {
  double total = 0;
  for (int i = 0; i < NUM_TRIALS; i++) {
    total += singleTrial(cutoff);
  }
  return total / NUM_TRIALS;
}

void main() {
  for (int cutoff = 0; cutoff <= 2 * PRIOR_LOWER_MAX; cutoff++) {
    double expectedValue = multiTrial(cutoff);
    print('cutoff=$cutoff, expected_value=$expectedValue');
  }
}