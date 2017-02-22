// $ brew install fpc
// $ fpc envelopes.pas
// $ ./envelopes

program Envelopes;
uses sysutils;

const
num_trials = 10000;
lower_prior_max = 100;

var
cutoff: integer;
output: String;

// Runs a single trial where an envelope is chosen.  If the chosen envelope has
// a value < cutoff, the function will switch envelopes, otherwise it will keep
// the envelope it has chosen. Returns the value of the envelope it ultimately 
// selects.
function single_trial(cutoff: integer): double;
var
  lower_value: double;
  higher_value: double;
  envelope: integer;
begin
  lower_value := lower_prior_max * Random();
  higher_value := 2 * lower_value;
  envelope := Random(2);
  if ((envelope = 0) and (lower_value >= cutoff)) or
     ((envelope = 1) and (higher_value < cutoff)) then
    single_trial := lower_value 
  else
    single_trial := higher_value;
end;

// Runs many trials at a given cutoff to approximate the expected value.
function multi_trial(cutoff: integer): double;
var
  total: double;
  i: integer;
begin
  total := 0;
  for i := 0 to num_trials do
    total := total + single_trial(cutoff);
  multi_trial := total / num_trials;
end;

begin
  for cutoff := 0 to 2 * lower_prior_max do
  begin
    output := Format('cutoff=%d, expected_value=%2.10f',
                     [cutoff, multi_trial(cutoff)]);
    writeln(output:12)
  end;
end.