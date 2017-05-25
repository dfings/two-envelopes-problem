// $ brew install kotlin
// $ kotlinc envelopes.kt -d envelopes.kt.jar
// $ kotlin -classpath envelopes.kt.jar EnvelopesKt
// or (faster runtime)
// $ kotlinc envelopes.kt -include-runtime -d envelopes.kt.jar
// $ java -jar envelopes.kt.jar

import java.util.Random;

val NUM_TRIALS = 10000
val PRIOR_LOWER_MAX = 100
var random = Random()

/**
 * Runs a single trial where an envelope is chosen.  If the chosen envelope has
 * a value < cutoff, the function will switch envelopes, otherwise it will keep
 * the envelope it has chosen. Returns the value of the envelope it ultimately 
 * selects.
 */
fun singleTrial(cutoff: Int): Double {
  val pick = {value: Double, other: Double -> if (value >= cutoff) value else other}
  val lowerValue = random.nextDouble() * PRIOR_LOWER_MAX
  val higherValue = 2 * lowerValue
  return if (random.nextBoolean()) pick(lowerValue, higherValue) else pick(higherValue, lowerValue)
}

/** Runs many trials at a given cutoff to approximate the expected value. */
fun multiTrial(cutoff: Int): Double {
  var total = 0.0
  for (i in 0..NUM_TRIALS)
    total += singleTrial(cutoff)
  return total / NUM_TRIALS
}

fun main(args: Array<String>) {
  for (cutoff in 0..(2 * PRIOR_LOWER_MAX))
    println("cutoff=$cutoff, expected_value=%f".format(multiTrial(cutoff)))
}