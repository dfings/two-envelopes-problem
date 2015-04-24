// $ clang envelopes.cc -O3 -o envelopes_cc
// $ ./envelopes_cc

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

const int kNumTrials = 10000;
const int kLowerPriorMax = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
double SingleTrial(int cutoff) {
  double lower_value = kLowerPriorMax * ((double) rand() / (RAND_MAX));
  double higher_value = 2 * lower_value;
  if (rand() % 2 == 0) {
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

int main(int argc, char* argv[]) {
  srand(time(NULL));
  for (int cutoff = 0; cutoff <= 2 * kLowerPriorMax; ++cutoff) {
    printf("cutoff=%d, expected_value=%f\n", cutoff, MultiTrial(cutoff));
  }
  return 0;
}
