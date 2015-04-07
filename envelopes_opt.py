#!/usr/bin/env python

import random

NUM_TRIALS = 20000
PRIOR_LOWER_MAX = 100
		
def multi_trial(cutoff):
  """Runs many trials at a given cutoff to approximate the expected value."""
  randfloat = random.random
  prior_lower_max = PRIOR_LOWER_MAX
  total_result = 0
  for _ in xrange(NUM_TRIALS):
    lower_value = randfloat() * prior_lower_max
    if randfloat() * 2 < 1:
      if lower_value >= cutoff:
        total_result = total_result + lower_value
      else:
        total_result = total_result + 2 * lower_value
    else:
      higher_value = 2 * lower_value
      if higher_value >= cutoff:
        total_result = total_result + higher_value
      else:
        total_result = total_result + lower_value        
  return total_result / NUM_TRIALS


if __name__ == '__main__':
  """Approximates the expected value for each integral cutoff value."""
  for cutoff in xrange(2 * PRIOR_LOWER_MAX):
    print "cutoff=%s, expected_value=%s" % (cutoff, multi_trial(cutoff))
