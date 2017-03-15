// $ brew install rust
// $ cargo build
// $ ./target/debug/envelopes.rs

extern crate rand;

use rand::{Rng, ThreadRng};

const NUM_TRIALS: i32 = 10000;
const PRIOR_LOWER_MAX: i32 = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately
// selects.
fn single_trial(cutoff: f64, rng: &mut ThreadRng) -> f64 {
  let lower_value = rng.next_f64() * PRIOR_LOWER_MAX as f64;
  let higher_value = 2f64 * lower_value;
  let chose_lower = rng.gen();
  if (chose_lower && lower_value >= cutoff) ||
     (!chose_lower && higher_value < cutoff) {
    return lower_value;
  } else {
    return higher_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
fn multi_trial(cutoff: i32) -> f64 {
  let cutoff64 = cutoff as f64;
  let mut rng = rand::thread_rng();
  let mut total = 0f64;
  for _ in 0..NUM_TRIALS {
    total += single_trial(cutoff64, &mut rng);
  }
  return total / NUM_TRIALS as f64;
}

fn main() {
  for cutoff in 0..(2 * PRIOR_LOWER_MAX + 1) {
    println!("cutoff={0}, expected_value={1}", cutoff, multi_trial(cutoff));
  }
}
