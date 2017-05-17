// $ brew install kotlin
// $ kotlinc envelopes.kts -d envelopes.kt.jar
// $ kotlin -classpath envelopes.kt.jar EnvelopesKt
// or (faster runtime)
// $ kotlinc envelopes.kt -include-runtime -d envelopes.kt.jar
// $ java -jar envelopes.kt.jar

import java.util.Random;

val NUM_TRIALS = 10000
val PRIOR_LOWER_MAX = 100
var random = Random()

fun singleTrial(cutoff: Int): Double {
  val lowerValue = random.nextDouble() * PRIOR_LOWER_MAX
  val higherValue = 2 * lowerValue
  return when (random.nextBoolean()) {
    true -> if (lowerValue >= cutoff) lowerValue else higherValue  
    false -> if (higherValue >= cutoff) higherValue else lowerValue
  }
}

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