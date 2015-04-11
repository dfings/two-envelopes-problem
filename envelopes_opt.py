#!/usr/bin/env python

import random

NUM_TRIALS = 10000
		
def multi_trial(cutoff):
  """Runs many trials at a given cutoff to approximate the expected value."""
  randfloat = random.random  # Avoid . call in the inner loop.
  total_result = 0
  cutoff_scaled = cutoff / 100.0  # Avoid multiplication in the inner loop.
  for _ in xrange(NUM_TRIALS):
    lower_value = randfloat()
    if randfloat() < 0.5:
      if lower_value >= cutoff_scaled:
        # Avoid computation of higher_value in this path.
        total_result = total_result + lower_value  
      else:
        total_result = total_result + 2 * lower_value
    else:
      higher_value = 2 * lower_value
      if higher_value >= cutoff:
        total_result = total_result + higher_value
      else:
        total_result = total_result + lower_value        
  return 100 * total_result / NUM_TRIALS


if __name__ == '__main__':
  """Approximates the expected value for each integral cutoff value."""
  for cutoff in xrange(200):
    print "cutoff=%s, expected_value=%s" % (cutoff, multi_trial(cutoff))
