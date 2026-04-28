// $ brew install gleam
// $ gleam add gleam_stdlib
// $ gleam run

import gleam/float
import gleam/int
import gleam/io

const prior_lower_max = 100

const prior_lower_max_f = 100.0

const num_trials = 10_000

fn single_trial(cutoff: Int) -> Float {
  let lower_value = float.random() *. prior_lower_max_f
  let higher_value = lower_value *. 2.0
  let #(chosen_value, other_value) = case float.random() <. 0.5 {
    True -> #(lower_value, higher_value)
    False -> #(higher_value, lower_value)
  }
  case chosen_value >. int.to_float(cutoff) {
    True -> chosen_value
    False -> other_value
  }
}

fn get_multi_trial_total(cutoff: Int, remaining: Int, total: Float) -> Float {
  case remaining {
    0 -> total
    _ ->
      get_multi_trial_total(
        cutoff,
        remaining - 1,
        total +. single_trial(cutoff),
      )
  }
}

fn multi_trial(cutoff: Int) -> Float {
  let total = get_multi_trial_total(cutoff, num_trials, 0.0)
  total /. int.to_float(num_trials)
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
