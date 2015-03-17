#!/bin/sh

# Shell scripting is super slow at this.
NUM_TRIALS=10000
PRIOR_LOWER_MAX=100

# Runs a single trial where an envelope is chosen.  If the chosen envelope has
# a value < cutoff, the function will switch envelopes, otherwise it will keep
# the envelope it has chosen. Returns the value of the envelope it ultimately 
# selects.
single_trial() {
  lower_envelope=$((RANDOM%PRIOR_LOWER_MAX))
  higher_envelope=$((lower_envelope*2))
  envelope=$((RANDOM%2))

  if [[ ( $envelope -eq 0 && $lower_envelope -gt $1 ) || 
        ( $envelope -eq 1 && $higher_envelope -lt $1 ) ]]; then
    echo $lower_envelope
  else
    echo $higher_envelope
  fi
}

# Runs many trials at a given cutoff to approximate the expected value.
NUM_TRIALS_MAX=$((NUM_TRIALS-1))
multi_trial() {
  total_result="0"
  for _ in $(seq 0 $NUM_TRIALS_MAX); do
    result=`single_trial $1`
    total_result=$((total_result+result))
  done
  echo "scale=5; $total_result / $NUM_TRIALS" | bc
}

CUTOFF_MAX=$((2*PRIOR_LOWER_MAX-1))
for cutoff in $(seq 0 $CUTOFF_MAX); do
  printf "cutoff=%s, expected_value=%s\n" "$@" $cutoff `multi_trial $cutoff`
done