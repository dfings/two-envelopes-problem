*> $ brew install open-cobol
*> $ cobc -I/Users/$USER/homebrew/include -L/Users/$USER/homebrew/lib -free -x -o envelopes_cbl envelopes.cbl

identification division.
program-id. envelopes.

data division.
  working-storage section.
  *> Constants
  01 ws-num-trials pic 9(5) value 10000.
  01 ws-prior-lower-max pic 9(3) value 100.
  *> Variables
  01 ws-cutoff pic 9(3).
  01 ws-total comp-2.
  01 ws-trial-num pic 9(5).
  01 ws-expected-value comp-2.
  01 ws-lower-value comp-2.
  01 ws-higher-value comp-2.
  *> RNG support
  01 ws-rng comp-2.
  01 ws-tmp pic 9(4).

procedure division.
  main.
  *> Approximates the expected value for each integral cutoff value.
  perform multi-trial varying ws-cutoff 
      from 0 by 1 until ws-cutoff > 2 * ws-prior-lower-max.
  stop run.

  multi-trial.
  *> Runs many trials at a given cutoff to approximate the expected value.
  move zero to ws-total.
  perform single-trial varying ws-trial-num 
      from 1 by 1 until ws-trial-num > ws-num-trials.
  divide ws-total by ws-num-trials giving ws-expected-value.
  display 'cutoff='ws-cutoff', expected_value='ws-expected-value.

  single-trial.
  *> Runs a single trial where an envelope is chosen.  If the chosen envelope 
  *> has a value < cutoff, the function will switch envelopes, otherwise it 
  *> will keep the envelope it has chosen. Returns the value of the envelope 
  *> it ultimately selects.
  perform rng.
  multiply ws-rng by ws-prior-lower-max giving ws-lower-value.
  multiply ws-lower-value by 2 giving ws-higher-value.
  perform rng.
  if ws-rng < 0.5 then 
    if ws-lower-value >= ws-cutoff then
      add ws-lower-value to ws-total
    else
      add ws-higher-value to ws-total
    end-if
  else
    if ws-higher-value >= ws-cutoff then
      add ws-higher-value to ws-total
    else
      add ws-lower-value to ws-total
    end-if  
  end-if.

  rng.
  *> function random produces a non-uniform distribution.
  *> So ignore the first 5 digits it produces.
  compute ws-rng = function random.
  compute ws-tmp = ws-rng * 10000.
  compute ws-rng = ws-rng * 10000 - ws-tmp.
