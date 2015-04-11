#!/usr/bin/env python

import random

NUM_TRIALS = 10000
		
def multi_trial(cutoff):
  """Runs many trials at a given cutoff to approximate the expected value."""
  randfloat = random.random
  total_result = 0
  for _ in xrange(NUM_TRIALS):
    lower_value = randfloat() * 100
    if randfloat() < 0.5:
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
  for cutoff in xrange(200):
    print "cutoff=%s, expected_value=%s" % (cutoff, multi_trial(cutoff))
