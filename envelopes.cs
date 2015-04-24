// $ brew install mono
// $ mcs -out:envelopes_cs.exe envelopes.cs
// $ mono envelopes_cs.exe

using System;

/** Monte Carlo simulation of the two envelopes problem. */
public sealed class Envelopes {
 
  private const int NumTrials = 10000;
  private const int LowerPriorMax = 100;
  private static readonly Random random = new Random();
 
  /**
   * Runs a single trial where an envelope is chosen.  If the chosen envelope has
   * a value &lt; cutoff, the function will switch envelopes, otherwise it will keep
   * the envelope it has chosen. Returns the value of the envelope it ultimately 
   * selects.
   */
  private static double SingleTrial(int cutoff) {
    double lowerValue = random.NextDouble() * LowerPriorMax;
    double higherValue = 2 * lowerValue;
    if (random.Next(2) == 0) {
      return lowerValue >= cutoff ? lowerValue : higherValue;
    } else {
      return higherValue >= cutoff ? higherValue : lowerValue;
    }  
  }
  
  /** Runs many trials at a given cutoff to approximate the expected value. */
  private static double MultiTrial(int cutoff) {
    double total = 0;
    for (int i = 0; i < NumTrials; ++i) {
      total += SingleTrial(cutoff);
    }  
    return total / NumTrials;
  }
  
  public static void Main() {
    for (int cutoff = 0; cutoff <= 2 * LowerPriorMax; ++cutoff) {
      Console.WriteLine("cutoff={0}, expected_value={1}", 
          cutoff, MultiTrial(cutoff));
    }
  }
}