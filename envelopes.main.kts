#!/usr/bin/env kotlin

import kotlin.random.Random

val NUM_TRIALS = 10000
val PRIOR_LOWER_MAX = 100

/**
 * Runs a single trial where an envelope is chosen. If the chosen envelope has a value < cutoff, the
 * function will switch envelopes, otherwise it will keep the envelope it has chosen. Returns the
 * value of the envelope it ultimately selects.
 */
fun singleTrial(cutoff: Int): Double {
    val lowerValue = Random.nextDouble() * PRIOR_LOWER_MAX
    val higherValue = 2 * lowerValue
    return if (Random.nextBoolean()) {
        if (lowerValue >= cutoff) lowerValue else higherValue
    } else {
        if (higherValue >= cutoff) higherValue else lowerValue
    }
}

/** Runs many trials at a given cutoff to approximate the expected value. */
fun multiTrial(cutoff: Int) = (0..NUM_TRIALS).asSequence().map { singleTrial(cutoff) }.average()

for (cutoff in 0..(2 * PRIOR_LOWER_MAX)) {
    println("cutoff=$cutoff, expected_value=${multiTrial(cutoff)}")
}
