#!/usr/bin/env RScript

# $ brew tap homebrew/science
# $ brew install R

NUM_TRIALS <- 10000
PRIOR_LOWER_MAX <- 100

# Runs a single trial where an envelope is chosen.  If the chosen envelope has
# a value < cutoff, the function will switch envelopes, otherwise it will keep
# the envelope it has chosen. Returns the value of the envelope it ultimately 
# selects.
singleTrial <- function(cutoff, rand1, rand2) {
  lower_value <- rand1 * PRIOR_LOWER_MAX
  higher_value <- 2 * lower_value
  if (rand2 < 0.5) {
    if (lower_value >= cutoff) lower_value else higher_value
  } else {
    if (higher_value >= cutoff) higher_value else lower_value
  }
}

# Runs many trials at a given cutoff to approximate the expected value.
multiTrial <- function(cutoff) {
  total <- 0
  # runif performance is pretty bad, this saves 7 seconds of runtime.
  rand_vals <- runif(2 * (NUM_TRIALS + 1))
  for (i in 1:NUM_TRIALS) {
    rand1 <- rand_vals[2 * i - 1]
    rand2 <- rand_vals[2 * i]
    # Inlining singleTrial would save another ~1.3 seconds.
    total <- total + singleTrial(cutoff, rand1, rand2)
  }
  total / NUM_TRIALS
}

for (cutoff in 0:(2 * PRIOR_LOWER_MAX)) {
  print(sprintf("cutoff=%s, expected_value=%f", cutoff, multiTrial(cutoff)))
}
