(*
 $ brew install fsharp
 $ fsharpc -o envelopes_fs.exe envelopes.fs
 $ mono envelopes_fs.exe
*)

let rng = System.Random()

let numTrials = 10000
let numTrialsF = float numTrials
let priorLowerMax = 100
let priorLowerMaxF = float priorLowerMax

(* Returns the result of a single trial. We switch if the value is below the
   cutoff. *)
let singleTrial cutoff =
  let lowerValue = rng.NextDouble() * priorLowerMaxF
  let higherValue = 2.0 * lowerValue
  let choice = rng.Next(0, 2)
  if choice = 0 then
    if lowerValue >= cutoff then lowerValue else higherValue
  else
    if higherValue >= cutoff then higherValue else lowerValue

(* Returns the total value of all of the trials for a given cutoff level. *)
let rec getMultiTrialTotal cutoff i total =
  if i < numTrials then
    getMultiTrialTotal cutoff (i + 1) (total + singleTrial cutoff)
  else total

(* Computes the average value among many trials. *)
let multiTrial cutoff =
  let cutoffF = float cutoff
  (getMultiTrialTotal cutoffF 0 0.) / numTrialsF

(* Prints the expected value for each possible cutoff. *)
for cutoff = 0 to 2 * priorLowerMax do
  Printf.printf "cutoff=%d, expected_value=%f\n" cutoff (multiTrial cutoff)
