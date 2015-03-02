package main

import (
	"fmt"
	"math/rand"
	"time"
) 

const NumTrials = 10000
const LowerPriorMax = 100
var r = rand.New(rand.NewSource(time.Now().UnixNano()))

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
func singleTrial(cutoff float64) float64 {
	lowerValue := r.Float64() * LowerPriorMax
	higherValue := 2 * lowerValue
	envelope := r.Intn(2)  // [0, 1]
	if ((envelope == 0 && lowerValue >= cutoff) ||
	    (envelope == 1 && higherValue < cutoff)) {
		return lowerValue
	} else {
		return higherValue
	}
}

// Runs many trials at a given cutoff to approximate the expected value.
func multiTrial(cutoff int) float64 {
	cutoff64 := float64(cutoff)
	total_result := 0.0
	for i := 0; i < NumTrials; i++ {
		total_result += singleTrial(cutoff64)
	}
	return total_result / float64(NumTrials)
}

func main() {
	for cutoff := 0; cutoff < 2 * LowerPriorMax; cutoff++ {
		fmt.Printf("cutoff=%v, expected_value=%0.3f\n", cutoff, multiTrial(cutoff))
	}
}