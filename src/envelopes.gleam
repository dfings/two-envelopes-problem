// $ brew install gleam
// $ gleam add gleam_stdlib gleam_yielder
// $ gleam run

import gleam/float
import gleam/int
import gleam/io
import gleam/yielder.{fold, map, range}

const prior_lower_max = 100

const prior_lower_max_f = 100.0

const num_trials = 10_000

fn single_trial(cutoff: Int) -> Float {
  let lower_value = float.random() *. prior_lower_max_f
  let higher_value = lower_value *. 2.0
  let #(chosen_value, other_value) = case int.random(2) == 0 {
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
    range(from: 0, to: num_trials)
    |> map(fn(_) { single_trial(cutoff) })
    |> fold(0.0, float.add)
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
