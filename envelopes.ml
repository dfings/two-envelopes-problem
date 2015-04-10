(*
 $ brew install ocaml
 $ ocamlc -o envelopes_ml envelopes.ml
 $ ./envelopes_ml
 or
 $ ocaml envelopes.ml
*)

Random.self_init();;

let num_trials = 10000
let num_trials_f = float_of_int num_trials
let prior_lower_max = 100
let prior_lower_max_f = float_of_int prior_lower_max

(* Returns the result of a single trial. We switch if the value is below the
   cutoff. *)
let single_trial cutoff =
  let lower_value = Random.float prior_lower_max_f in
  let higher_value = 2.0 *. lower_value and choice = Random.int 2 in
  let value, other = if choice == 0 
    then (lower_value, higher_value) 
    else (higher_value, lower_value) in
      if value >= cutoff then value else other;;

(* Returns the total value of all of the trials for a given cutoff level. *)
let rec get_multi_trial_total cutoff i total =
  if i < num_trials then
    get_multi_trial_total cutoff (i + 1) (total +. single_trial cutoff)
  else total;;

(* Computes the average value among many trials. *)
let multi_trial cutoff =
  let cutoff_f = float_of_int cutoff in
  (get_multi_trial_total cutoff_f 0 0.) /. num_trials_f;;

(* Prints the expected value for each possible cutoff. *)
for cutoff = 0 to 2 * prior_lower_max do
  Printf.printf "cutoff=%d, expected_value=%f\n" cutoff (multi_trial cutoff)
done;
