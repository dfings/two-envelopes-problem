// $ brew install dmd
// $ dmd -run envelopes.d

import std.random;
import std.stdio;

const int kNumTrials = 10000;
const int kLowerPriorMax = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
double SingleTrial(int cutoff) {
  double lower_value = uniform(0.0f, kLowerPriorMax);
  double higher_value = 2 * lower_value;
  if (uniform(0, 2) == 0) {
    return lower_value >= cutoff ? lower_value : higher_value;
  } else {
    return higher_value >= cutoff ? higher_value : lower_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
double MultiTrial(int cutoff) {
  double total = 0;
  for (int i = 0; i < kNumTrials; ++i) {
    total += SingleTrial(cutoff);
  }  
  return total / kNumTrials;
}

void main() {
  for (int cutoff = 0; cutoff <= 2 * kLowerPriorMax; ++cutoff) {
    writefln("cutoff=%d, expected_value=%f", cutoff, MultiTrial(cutoff));
  }
}
