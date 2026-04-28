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

fn multi_trial(cutoff: Int) -> Float {
  let total =
    int.range(from: 0, to: num_trials, with: 0.0, run: fn(acc, _) {
      acc +. single_trial(cutoff)
    })
  total /. int.to_float(num_trials)
}

pub fn main() -> Nil {
  int.range(from: 0, to: 2 * prior_lower_max + 1, with: Nil, run: fn(_, i) {
    io.println(
      "cutoff="
      <> int.to_string(i)
      <> ", expected_value="
      <> float.to_string(multi_trial(i)),
    )
  })
}
