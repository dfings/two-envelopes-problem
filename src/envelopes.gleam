// brew install gleam
// gleam run

import gleam/float
import gleam/int
import gleam/io

const prior_lower_max = 100

const num_trials = 10_000

fn single_trial(cutoff: Int) -> Int {
  // Fix me
  cutoff
}

fn get_multi_trial_total(cutoff: Int, remaining: Int) -> Int {
  case remaining {
    0 -> 0
    _ -> single_trial(cutoff) + get_multi_trial_total(cutoff, remaining - 1)
  }
}

fn multi_trial(cutoff: Int) -> Float {
  let total = get_multi_trial_total(cutoff, num_trials)
  int.to_float(total) /. int.to_float(num_trials)
}

fn cutoff_trials(cutoff: Int, remaining: Int) -> Nil {
  case remaining {
    0 -> Nil
    _ -> {
      let expected_value = multi_trial(cutoff)
      io.println(
        "cutoff="
        <> int.to_string(cutoff)
        <> ", expected_value="
        <> float.to_string(expected_value),
      )
      cutoff_trials(cutoff + 1, remaining - 1)
    }
  }
}

pub fn main() -> Nil {
  cutoff_trials(0, 2 * prior_lower_max + 1)
}
