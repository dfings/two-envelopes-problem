(*
 $ brew install fsharp
 $ fsharpc -o envelopes_fs.exe envelopes.fs
 $ mono envelopes_fs.exe
*)

let rng = System.Random()

let num_trials = 10000
let num_trials_f = float num_trials
let prior_lower_max = 100
let prior_lower_max_f = float prior_lower_max

(* Returns the result of a single trial. We switch if the value is below the
   cutoff. *)
let single_trial cutoff =
  let lower_value = rng.NextDouble() * prior_lower_max_f
  let higher_value = 2.0 * lower_value
  let choice = rng.Next(0, 2)
  if choice = 0 then
    if lower_value >= cutoff then lower_value else higher_value
  else
    if higher_value >= cutoff then higher_value else lower_value

(* Returns the total value of all of the trials for a given cutoff level. *)
let rec get_multi_trial_total cutoff i total =
  if i < num_trials then
    get_multi_trial_total cutoff (i + 1) (total + single_trial cutoff)
  else total

(* Computes the average value among many trials. *)
let multi_trial cutoff =
  let cutoff_f = float cutoff
  (get_multi_trial_total cutoff_f 0 0.) / num_trials_f

(* Prints the expected value for each possible cutoff. *)
for cutoff = 0 to 2 * prior_lower_max do
  Printf.printf "cutoff=%d, expected_value=%f\n" cutoff (multi_trial cutoff)