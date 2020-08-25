#!/usr/bin/env raku

# $ brew install rakudo-star

use v6;

constant $PRIOR_LOWER_MAX = 100;
constant $NUM_TRIALS = 10000;

# Runs a single trial where an envelope is chosen.  If the chosen envelope has
# a value < cutoff, the function will switch envelopes, otherwise it will keep
# the envelope it has chosen. Returns the value of the envelope it ultimately 
# selects.
sub single_trial($cutoff) {
  my $lower_value = $PRIOR_LOWER_MAX * rand;  
  my $higher_value = 2 * $lower_value;
  if (rand < 0.5) {
    return $lower_value >= $cutoff ?? $lower_value !! $higher_value;
  } else {
    return $higher_value >= $cutoff ?? $higher_value !! $lower_value;
  }
}

# Runs many trials at a given cutoff to approximate the expected value.
sub multi_trial($cutoff) {
  my $total = 0;
  for 0..$NUM_TRIALS {
    $total += single_trial($cutoff)
  }
  return $total / $NUM_TRIALS;
}

for 0..(2 * $PRIOR_LOWER_MAX + 1) -> $cutoff {
  sprintf("cutoff=%d, expected_value=%f", $cutoff, multi_trial($cutoff)).put;
}
