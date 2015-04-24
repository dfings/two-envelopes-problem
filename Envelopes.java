// $ javac -cp . Envelopes.java
// $ java -cp . Envelopes

import java.util.Random;

/** Monte Carlo simulation of the two envelopes problem. */
public final class Envelopes {
 
  private static final int NUM_TRIALS = 10000;
  private static final int LOWER_PRIOR_MAX = 100;
  private static final Random random = new Random();
 
  /**
   * Runs a single trial where an envelope is chosen.  If the chosen envelope has
   * a value &lt; cutoff, the function will switch envelopes, otherwise it will keep
   * the envelope it has chosen. Returns the value of the envelope it ultimately 
   * selects.
   */
  private static double singleTrial(int cutoff) {
    double lowerValue = random.nextDouble() * LOWER_PRIOR_MAX;
    double higherValue = 2 * lowerValue;
    if (random.nextInt(2) == 0) {
      return lowerValue >= cutoff ? lowerValue : higherValue;
    } else {
      return higherValue >= cutoff ? higherValue : lowerValue;
    }  
  }
  
  /** Runs many trials at a given cutoff to approximate the expected value. */
  private static double multiTrial(int cutoff) {
    double total = 0;
    for (int i = 0; i < NUM_TRIALS; ++i) {
      total += singleTrial(cutoff);
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