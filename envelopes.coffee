#!/usr/bin/env coffee

# $ brew install node
# $ npm install -g coffee-script

NUM_TRIALS = 10000;
PRIOR_LOWER_MAX = 100;

# Runs a single trial where an envelope is chosen.  If the chosen envelope has
# a value < cutoff, the function will switch envelopes, otherwise it will keep
# the envelope it has chosen. Returns the value of the envelope it ultimately 
# selects.
singleTrial = (cutoff) ->
  lower_value = Math.random() * PRIOR_LOWER_MAX
  higher_value = 2 * lower_value
  if Math.random() < 0.5
    return if lower_value >= cutoff then lower_value else higher_value
  else
    return if higher_value >= cutoff then higher_value else lower_value

# Runs many trials at a given cutoff to approximate the expected value.
multiTrial = (cutoff) ->
  total = 0
  for i  in [0..NUM_TRIALS]
    total += singleTrial(cutoff)
  return total / NUM_TRIALS;

for cutoff in [0..2 * PRIOR_LOWER_MAX]
  console.log('cutoff=' + cutoff + ', expected_value=' + multiTrial(cutoff))
