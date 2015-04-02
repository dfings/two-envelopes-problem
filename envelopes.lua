#!/usr/bin/env lua

NUM_TRIALS=10000
PRIOR_LOWER_MAX = 100

-- Runs a single trial where an envelope is chosen.  If the chosen envelope has
-- a value < cutoff, the function will switch envelopes, otherwise it will keep
-- the envelope it has chosen. Returns the value of the envelope it ultimately 
-- selects.
function single_trial(cutoff)  
  lower_value = math.random() * PRIOR_LOWER_MAX
  higher_value = 2 * lower_value
  if math.random(2) == 1 then
    return lower_value >= cutoff and lower_value or higher_value
  else
    return higher_value >= cutoff and higher_value or lower_value
  end
end

-- Runs many trials at a given cutoff to approximate the expected value.
function multi_trial(cutoff)
  total_result = 0
  for _ =1,NUM_TRIALS,1 do
    total_result = total_result + single_trial(cutoff)
  end
  return total_result / NUM_TRIALS
end

for cutoff=0,2*PRIOR_LOWER_MAX,1 do
  io.write(string.format("cutoff=%d, expected_value=%f\n", cutoff, multi_trial(cutoff)))
end
