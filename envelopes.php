#!/usr/bin/env php

<?php
$PRIOR_LOWER_MAX = 100;
$NUM_TRIALS = 10000;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
function single_trial($cutoff) {
  global $PRIOR_LOWER_MAX;
  $lower_value = $PRIOR_LOWER_MAX * (float) rand() / (float) getrandmax();  
  $higher_value = 2 * $lower_value;
  if (rand(0, 1) == 0) {
    return $lower_value >= $cutoff ? $lower_value : $higher_value;
  } else {
    return $higher_value >= $cutoff ? $higher_value : $lower_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
function multi_trial($cutoff) {
  global $NUM_TRIALS;
  $total = 0.0;
  for ($i = 0; $i < $NUM_TRIALS; $i++) {
    $total += single_trial($cutoff);
  }
  return $total / $NUM_TRIALS;
}

for ($cutoff = 0; $cutoff < 2 * $PRIOR_LOWER_MAX + 1; $cutoff++) {
  printf("cutoff=%d, expected_value=%f\n", $cutoff, multi_trial($cutoff));
}
?>