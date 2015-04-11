#!/usr/bin/env ruby

NUM_TRIALS = 10000

# Runs many trials at a given cutoff to approximate the expected value.
def multi_trial(cutoff)
  total = 0
  cutoff_scaled = cutoff / 100.0
  for _ in 0..NUM_TRIALS
    lower_value = rand()
    higher_value = 2 * lower_value
    if rand() < 0.5 then
      total += (if lower_value >= cutoff then lower_value else higher_value end)
    else
      total += (if higher_value >= cutoff then higher_value else lower_value end)
    end
  end
  return 100 * total / NUM_TRIALS
end

for cutoff in 0..200
  printf("cutoff=%s, expected_value=%f\n", cutoff, multi_trial(cutoff))
end
