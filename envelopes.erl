-module(envelopes).
-export([main/0]).

% Returns the result of a single trial. We switch if the value is below the
% cutoff.
single_trial(Cutoff, PriorLowerMax) ->
  LowerValue = random:uniform() * PriorLowerMax,
  Envelopes = {LowerValue, 2 * LowerValue},
  Envelope = random:uniform(2),  % [1, 2]
  if 
    element(Envelope, Envelopes) >= Cutoff ->
      element(Envelope, Envelopes);
    true -> 
      element(3 - Envelope, Envelopes)  % Erlang is 1-indexed
  end.
  
% Returns the total value of all of the trials for a given cutoff level.   
get_multi_trial_total(_, _, 0, Total) -> Total;
get_multi_trial_total(Cutoff, PriorLowerMax, Remaining, Total) ->
  get_multi_trial_total(Cutoff, PriorLowerMax, Remaining - 1, 
                        Total + single_trial(Cutoff, PriorLowerMax)).
                
% Computes the average value among many trials.                   
multi_trial(Cutoff, PriorLowerMax, NumTrials) ->
  get_multi_trial_total(Cutoff, PriorLowerMax, NumTrials + 1, 0) / NumTrials.
  
% Prints the expected value for each possible cutoff.
cutoff_trials(_, 0, _, _) -> ok;
cutoff_trials(Cutoff, Remaining, PriorLowerMax, NumTrials) ->
  ExpectedValue = multi_trial(Cutoff, PriorLowerMax, NumTrials),
  io:format("cutoff=~w, expected_value=~w~n", [Cutoff, ExpectedValue]),
  cutoff_trials(Cutoff + 1, Remaining - 1, PriorLowerMax, NumTrials).
      
main() -> cutoff_trials(0, 201, 100, 10000).
