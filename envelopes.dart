#!/usr/bin/env dart
// $ brew tap dart-lang/dart
// $ brew install dart

import 'dart:math';

const NUM_TRIALS = 10000;
const PRIOR_LOWER_MAX = 100;
final random = Random();

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately
// selects.
double singleTrial(int cutoff) {
  var pick = (value, other) => value >= cutoff ? value : other;
  var lower_value = random.nextDouble() * PRIOR_LOWER_MAX;
  var higher_value = 2 * lower_value;
  return random.nextBool()
      ? pick(lower_value, higher_value)
      : pick(higher_value, lower_value);
}

// Runs many trials at a given cutoff to approximate the expected value.
double multiTrial(int cutoff) {
  var total = 0.0;
  for (var i = 0; i < NUM_TRIALS; i++) total += singleTrial(cutoff);
  return total / NUM_TRIALS;
}

void main() {
  for (int cutoff = 0; cutoff <= 2 * PRIOR_LOWER_MAX; cutoff++) {
    var expectedValue = multiTrial(cutoff);
    print('cutoff=$cutoff, expected_value=$expectedValue');
  }
}
