#!/usr/bin/env ruby

NUM_TRIALS = 10000
PRIOR_LOWER_MAX = 100

# Runs a single trial where an envelope is chosen.  If the chosen envelope has
# a value < cutoff, the function will switch envelopes, otherwise it will keep
# the envelope it has chosen. Returns the value of the envelope it ultimately 
# selects.
def single_trial(cutoff)
  lower_value = rand(PRIOR_LOWER_MAX)
  envelopes = [lower_value, 2 * lower_value].shuffle
  return (if envelopes[0] >= cutoff then envelopes[0] else envelopes[1] end)
end

# Runs many trials at a given cutoff to approximate the expected value.
def multi_trial(cutoff)
  total = 0
  for i in 0..NUM_TRIALS
    total += single_trial(cutoff)
  end
  return Float(total) / NUM_TRIALS
end

for cutoff in 0..(2 * PRIOR_LOWER_MAX)
  printf("cutoff=%s, expected_value=%f\n", cutoff, multi_trial(cutoff))
end
