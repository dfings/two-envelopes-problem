// $ brew install rust
// $ cargo build
// $ ./target/debug/envelopes.rs

extern crate rand;

use rand::Rng;

const NUM_TRIALS: i32 = 10000;
const PRIOR_LOWER_MAX: i32 = 100;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately
// selects.
fn single_trial<R: Rng>(cutoff: f64, rng: &mut R) -> f64 {
    let lower_value = rng.gen::<f64>() * PRIOR_LOWER_MAX as f64;
    let higher_value = 2f64 * lower_value;
    if rng.gen::<bool>() {
        if lower_value >= cutoff {
            lower_value
        } else {
            higher_value
        }
    } else {
        if higher_value >= cutoff {
            higher_value
        } else {
            lower_value
        }
    }
}

// Runs many trials at a given cutoff to approximate the expected value.
fn multi_trial(cutoff: i32) -> f64 {
    let mut rng = rand::thread_rng();
    let total: f64 = (0..NUM_TRIALS)
        .map(|_| single_trial(cutoff as f64, &mut rng))
        .sum();
    total / NUM_TRIALS as f64
}

fn main() {
    for cutoff in 0..(2 * PRIOR_LOWER_MAX + 1) {
        println!("cutoff={}, expected_value={}", cutoff, multi_trial(cutoff));
    }
}
