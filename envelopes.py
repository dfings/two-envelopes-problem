#!/usr/bin/env python

import random

NUM_TRIALS = 10000
PRIOR_LOWER_MAX = 100


def single_trial(cutoff):
    """Runs a single trial where an envelope is chosen.

    If the chosen envelope has a value < cutoff, the function will switch
    envelopes, otherwise it will keep the envelope it has chosen. Returns
    the value of the envelope it ultimately selects.
    """
    lower_value = random.random() * PRIOR_LOWER_MAX
    higher_value = 2 * lower_value
    if random.random() < 0.5:
        return lower_value if lower_value >= cutoff else higher_value
    return higher_value if higher_value >= cutoff else lower_value


def multi_trial(cutoff):
    """Runs many trials at a given cutoff to approximate the expected value."""
    total = 0
    for _ in range(NUM_TRIALS):
        total += single_trial(cutoff)
    return total / NUM_TRIALS


# Approximates the expected value for each integral cutoff value.
for cutoff in range(2 * PRIOR_LOWER_MAX + 1):
    print("cutoff={}, expected_value={}".format(cutoff, multi_trial(cutoff)))
