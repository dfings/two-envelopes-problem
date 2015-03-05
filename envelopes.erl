% $ erl
% 1> c(envelopes).
% 2> envelopes:main().

-module(envelopes).
-export([main/0]).

prior_lower_max() -> 100.
num_trials() -> 10000.

% Helper to generate the [picked, other] list for a given run.
get_trial_values(Envelope, LowerValue) ->
  Values = [LowerValue, 2 * LowerValue],
  if Envelope == 1 -> Values; true -> lists:reverse(Values) end.

% Returns the result of a single trial. We switch if the value is below the
% cutoff.
single_trial(Cutoff) ->
  Values = get_trial_values(random:uniform(2), random:uniform() * prior_lower_max()),
  Choice = lists:nth(1, Values),
  if Choice >= Cutoff -> Choice; true -> lists:nth(2, Values) end.
  
% Returns the total value of all of the trials for a given cutoff level.   
get_multi_trial_total(_, 0, Total) -> Total;
get_multi_trial_total(Cutoff, Remaining, Total) ->
  get_multi_trial_total(Cutoff, Remaining - 1, Total + single_trial(Cutoff)).
                
% Computes the average value among many trials.                   
multi_trial(Cutoff) ->
  get_multi_trial_total(Cutoff, num_trials() + 1, 0) / num_trials().
  
% Prints the expected value for each possible cutoff.
cutoff_trials(_, 0) -> ok;
cutoff_trials(Cutoff, Remaining) ->
  ExpectedValue = multi_trial(Cutoff),
  io:format("cutoff=~w, expected_value=~w~n", [Cutoff, ExpectedValue]),
  cutoff_trials(Cutoff + 1, Remaining - 1).
      
main() -> cutoff_trials(0, 2 * prior_lower_max() + 1).
