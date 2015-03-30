// $ brew install rust
// $ rustc envelopes.rs
// $ ./envelopes.rs

// These are deprecated libraries :(
use std::rand;
use std::rand::distributions::{IndependentSample, Range};

static NUM_TRIALS: i32 = 10000;
static PRIOR_LOWER_MAX: i32 = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
fn single_trial(cutoff: f64, rng: &mut rand::ThreadRng) -> f64 {
  let value_range = Range::new(0f64, PRIOR_LOWER_MAX as f64);
  let lower_value = value_range.ind_sample(rng);
  let higher_value = 2f64 * lower_value;
  let choice_range = Range::new(0, 2);
  let choice = choice_range.ind_sample(rng);
  if (choice == 0 && lower_value >= cutoff) ||
     (choice == 1 && higher_value < cutoff) {
    return lower_value;
  } else {
    return higher_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
fn multi_trial(cutoff: i32, rng: &mut rand::ThreadRng) -> f64 {
  let cutoff64 = cutoff as f64;
  let mut total = 0f64;
  for _ in 0..NUM_TRIALS {
    total += single_trial(cutoff64, rng);
  }
  return total / NUM_TRIALS as f64;
}

fn main() {
  let mut rng = rand::thread_rng();
  for cutoff in 0..(2 * PRIOR_LOWER_MAX + 1) {
    println!("cutoff={0}, expected_value={1}", cutoff, multi_trial(cutoff, &mut rng));     
  }
}
