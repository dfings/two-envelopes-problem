// $ javac -cp . Envelopes.java
// $ java -cp . Envelopes

import java.util.Random;

/** Monte Carlo simulation of the two envelopes problem. */
public final class Envelopes {
 
  private static final int NUM_TRIALS = 10000;
  private static final int LOWER_PRIOR_MAX = 100;
  private static final Random random = new Random();
 
  @FunctionalInterface
  private interface Picker {
    double pick(double value, double other);
  }

  /**
   * Runs a single trial where an envelope is chosen.  If the chosen envelope has
   * a value &lt; cutoff, the function will switch envelopes, otherwise it will keep
   * the envelope it has chosen. Returns the value of the envelope it ultimately 
   * selects.
   */
  private static double singleTrial(Picker picker) {
    var lowerValue = random.nextDouble() * LOWER_PRIOR_MAX;
    var higherValue = 2 * lowerValue;
    return random.nextInt(2) == 0 
        ? picker.pick(lowerValue, higherValue) 
        : picker.pick(higherValue, lowerValue);
  }
  
  /** Runs many trials at a given cutoff to approximate the expected value. */
  private static double multiTrial(int cutoff) {
    Picker picker = (value, other) -> value >= cutoff ? value : other;
    var total = 0.0;
    for (int i = 0; i < NUM_TRIALS; ++i) {
      total += singleTrial(picker);
    }  
    return total / NUM_TRIALS;
  }
  
  public static void main(String[] args) {
    for (int cutoff = 0; cutoff <= 2 * LOWER_PRIOR_MAX; ++cutoff) {
      System.out.println(String.format("cutoff=%d, expected_value=%f",
          cutoff, multiTrial(cutoff)));
    }
  }
}