// $ brew install rust
// $ rustc envelopes.rs
// $ ./envelopes.rs

// These are deprecated libraries :(
use std::rand;
use std::rand::distributions::{IndependentSample, Range};

static NUM_TRIALS: i32 = 10000;
static PRIOR_LOWER_MAX: i32 = 100;

struct RandomContext {
  rng: rand::ThreadRng,
  value_range: Range<f64>,
  choice_range: Range<i32>
}

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
fn single_trial(cutoff: f64, ctx: &mut RandomContext) -> f64 {
  let lower_value = ctx.value_range.ind_sample(&mut ctx.rng);
  let higher_value = 2f64 * lower_value;
  let choice = ctx.choice_range.ind_sample(&mut ctx.rng);
  if (choice == 0 && lower_value >= cutoff) ||
     (choice == 1 && higher_value < cutoff) {
    return lower_value;
  } else {
    return higher_value;
  }
}

// Runs many trials at a given cutoff to approximate the expected value.
fn multi_trial(cutoff: i32, ctx: &mut RandomContext) -> f64 {
  let cutoff64 = cutoff as f64;
  let mut total = 0f64;
  for _ in 0..NUM_TRIALS {
    total += single_trial(cutoff64, ctx);
  }
  return total / NUM_TRIALS as f64;
}

fn main() {
  let mut ctx = RandomContext {
    rng: rand::thread_rng(),
    value_range: Range::new(0f64, PRIOR_LOWER_MAX as f64),
    choice_range: Range::new(0, 2)
  };
  for cutoff in 0..(2 * PRIOR_LOWER_MAX + 1) {
    println!("cutoff={0}, expected_value={1}", cutoff, multi_trial(cutoff, &mut ctx));     
  }
}
