#!/usr/bin/env node

var NUM_TRIALS = 10000;
var PRIOR_LOWER_MAX = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
function singleTrial(cutoff) {
  var lower_value = Math.random() * PRIOR_LOWER_MAX;
  var higher_value = 2 * lower_value;
  if (Math.random() < 0.5) {
    return lower_value >= cutoff ? lower_value : higher_value;
  } else {
    return higher_value >= cutoff ? higher_value : lower_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
function multiTrial(cutoff) {
  var total = 0;
  for (var i = 0; i < NUM_TRIALS; i++) {
    total += singleTrial(cutoff);
  }
  return total / NUM_TRIALS;
}

for (var cutoff = 0; cutoff <= 2 * PRIOR_LOWER_MAX; cutoff++) {
  console.log('cutoff=' + cutoff + ', expected_value=' + multiTrial(cutoff));
}
