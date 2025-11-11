// $ brew install kotlin
// $ kotlinc envelopes.kt -d envelopes.kt.jar
// $ kotlin -classpath envelopes.kt.jar EnvelopesKt
// or (faster runtime)
// $ kotlinc envelopes.kt -include-runtime -d envelopes.kt.jar
// $ java -jar envelopes.kt.jar

import kotlin.random.Random

const val NUM_TRIALS = 10000
const val PRIOR_LOWER_MAX = 100

/**
 * Runs a single trial where an envelope is chosen. If the chosen envelope has a value < cutoff, the
 * function will switch envelopes, otherwise it will keep the envelope it has chosen. Returns the
 * value of the envelope it ultimately selects.
 */
fun singleTrial(cutoff: Int): Double {
    val lowerValue = Random.nextDouble() * PRIOR_LOWER_MAX
    val higherValue = 2 * lowerValue
    return when (Random.nextBoolean()) {
        true -> if (lowerValue >= cutoff) lowerValue else higherValue
        else -> if (higherValue >= cutoff) higherValue else lowerValue
    }
}

/** Runs many trials at a given cutoff to approximate the expected value. */
fun multiTrial(cutoff: Int) = (0..NUM_TRIALS).asSequence().map { singleTrial(cutoff) }.average()

fun main() {
    for (cutoff in 0..(2 * PRIOR_LOWER_MAX)) {
        println("cutoff=$cutoff, expected_value=${multiTrial(cutoff)}")
    }
}
