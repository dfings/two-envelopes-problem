// $ brew install ceylon
// $ ceylon compile source/envelopes/envelopes.ceylon
// $ ceylon run --run envelopes::main envelopes

import ceylon.random { Random, DefaultRandom }

Integer numTrials = 10000;
Integer priorLowerMax = 100;
Random random = DefaultRandom();

/**
 * Runs a single trial where an envelope is chosen.  If the chosen envelope has
 * a value < cutoff, the function will switch envelopes, otherwise it will keep
 * the envelope it has chosen. Returns the value of the envelope it ultimately 
 * selects.
 */
Float singleTrial(Float cutoff) {
  value pick = (Float val, Float other) => val >= cutoff then val else other;
  value lowerValue = random.nextFloat() * priorLowerMax;
  value higherValue = 2 * lowerValue;
  return random.nextBoolean() then pick(lowerValue, higherValue) else pick(higherValue, lowerValue);
}

/** Runs many trials at a given cutoff to approximate the expected value. */
Float multiTrial(Integer cutoff) {
  variable value total = 0.0;
  value cutoffF = cutoff.float;
  for (i in 0:numTrials) {
    total += singleTrial(cutoffF);
  }
  return total / numTrials;
}

shared void main() {
  for (cutoff in 0:(2 * priorLowerMax + 1)) {
    print("cutoff=``cutoff``, expected_value=" + multiTrial(cutoff).string);    
  }
}
