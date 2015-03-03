#!/usr/bin/python

import random

NUM_TRIALS = 10000
PRIOR_LOWER_MAX = 100

def single_trial(cutoff):
  """Runs a single trial where an envelope is chosen.

  If the chosen envelope has a value < cutoff, the function will switch 
  envelopes, otherwise it will keep the envelope it has chosen. Returns
  the value of the envelope it ultimately selects.
  """
  lower_envelope = random.random() * PRIOR_LOWER_MAX
  envelopes = [lower_envelope, 2 * lower_envelope]
  random.shuffle(envelopes)
  return envelopes[0] if envelopes[0] >= cutoff else envelopes[1]
	
		
def multi_trial(cutoff):
  """Runs many trials at a given cutoff to approximate the expected value."""
  total_result = 0
  for _ in xrange(NUM_TRIALS):
    total_result += single_trial(cutoff)
  return total_result / NUM_TRIALS


if __name__ == '__main__':
  """Approximates the expected value for each integral cutoff value."""
  for cutoff in xrange(2 * PRIOR_LOWER_MAX):
    print "cutoff=%s, expected_value=%s" % (cutoff, multi_trial(cutoff))
