(*
 $ brew install ocaml
 $ ocamlc -o envelopes_ml envelopes.ml
 $ ./envelopes_ml
 or
 $ ocaml envelopes.ml
*)

Random.self_init();;

let num_trials = 10000
let prior_lower_max = 100

(* Returns the result of a single trial. We switch if the value is below the
   cutoff. *)
let single_trial cutoff =
  let lower_value = Random.float (float_of_int prior_lower_max) in
  let higher_value = 2.0 *. lower_value 
  and choice = Random.int 2 in
  if choice == 0 then
    if lower_value >= cutoff then lower_value else higher_value
  else
    if higher_value >= cutoff then higher_value else lower_value;;

(* Returns the total value of all of the trials for a given cutoff level. *)
let rec get_multi_trial_total cutoff i total =
  if i < num_trials then
    get_multi_trial_total cutoff (i + 1) (total +. single_trial cutoff)
  else total;;

(* Prints the expected value for each possible cutoff. *)
let rec cutoff_trials cutoff =
  let num_trials_f = float_of_int num_trials
  and cutoff_f = float_of_int cutoff in
  if cutoff <= 2 * prior_lower_max then begin
    Printf.printf "cutoff=%d, expected_value=%f\n"
      cutoff
      ((get_multi_trial_total cutoff_f 0 0.) /. num_trials_f);
    cutoff_trials (cutoff + 1);
  end
  else ();;

cutoff_trials 0
