#!/usr/bin/env python

import random

NUM_TRIALS = 10000


def multi_trial(cutoff):
    """Runs many trials at a given cutoff to approximate the expected value."""
    randfloat = random.random  # Avoid . call in the inner loop.
    total = 0
    cutoff_scaled = cutoff / 100.0  # Avoid multiplication in the inner loop.
    for _ in range(NUM_TRIALS):
        lower_value = randfloat()
        if randfloat() < 0.5:
            if lower_value >= cutoff_scaled:
                # Avoid computation of higher_value in this path.
                total = total + lower_value
            else:
                total = total + 2 * lower_value
        else:
            higher_value = 2 * lower_value
            if higher_value >= cutoff:
                total = total + higher_value
            else:
                total = total + lower_value
    return 100 * total / NUM_TRIALS


if __name__ == "__main__":
    """Approximates the expected value for each integral cutoff value."""
    for cutoff in range(201):
        print("cutoff={}, expected_value={}".format(cutoff, multi_trial(cutoff)))
