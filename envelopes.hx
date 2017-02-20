// $ brew install haxe
// $ haxe -main Envelopes -js envelopes.hx.js
// $ node envelopes.hx.js

/** Monte Carlo simulation of the two envelopes problem. */
class Envelopes {
 
  static var NUM_TRIALS : Int = 10000;
  static var LOWER_PRIOR_MAX : Int = 100;
 
  /**
   * Runs a single trial where an envelope is chosen.  If the chosen envelope has
   * a value &lt; cutoff, the function will switch envelopes, otherwise it will keep
   * the envelope it has chosen. Returns the value of the envelope it ultimately 
   * selects.
   */
  static private function singleTrial(cutoff: Int): Float {
    var lowerValue = (Std.random(1<<30) / (1<<30)) * LOWER_PRIOR_MAX;
    var higherValue = 2 * lowerValue;
    if (Std.random(2) == 0) {
      return lowerValue >= cutoff ? lowerValue : higherValue;
    } else {
      return higherValue >= cutoff ? higherValue : lowerValue;
    }  
  }
  
  /** Runs many trials at a given cutoff to approximate the expected value. */
  static private function multiTrial(cutoff: Int): Float {
    var total = 0.0;
    for (i in 0...NUM_TRIALS) {
      total += singleTrial(cutoff);
    }  
    return total / NUM_TRIALS;
  }
  
  static public function main(): Void {
    var limit = 2 * LOWER_PRIOR_MAX;
    for (cutoff in 0...limit) {
      var expected_value = multiTrial(cutoff);
      trace('cutoff=${cutoff}, expected_value=${expected_value}');
    }
  }
}