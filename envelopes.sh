#!/bin/sh

# Shell scripting is super slow at this.
NUM_TRIALS=10000
PRIOR_LOWER_MAX=100

CUTOFF_MAX=$((2*PRIOR_LOWER_MAX-1))
NUM_TRIALS_MAX=$((NUM_TRIALS-1))
for cutoff in $(seq 0 $CUTOFF_MAX); do
  total_result="0"
  # Runs many trials at a given cutoff to approximate the expected value.
  for _ in $(seq 0 $NUM_TRIALS_MAX); do
    # Runs a single trial where an envelope is chosen.  If the chosen envelope has
    # a value < cutoff, the function will switch envelopes, otherwise it will keep
    # the envelope it has chosen. Returns the value of the envelope it ultimately 
    # selects.
    lower_envelope=$((RANDOM%PRIOR_LOWER_MAX))
    higher_envelope=$((lower_envelope*2))
    envelope=$((RANDOM%2))

    if [[ ( $envelope -eq 0 && $lower_envelope -gt $1 ) || 
          ( $envelope -eq 1 && $higher_envelope -lt $1 ) ]]; then
      total_result=$((total_result+lower_envelope))
    else
      total_result=$((total_result+higher_envelope))
    fi  
  done
  expected_value=`echo "scale=5; $total_result / $NUM_TRIALS" | bc`
  printf "cutoff=%s, expected_value=%s\n" "$@" $cutoff $expected_value
done