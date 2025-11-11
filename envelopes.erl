% $ brew install erlang
% $ erl -compile envelopes
% $ erl -noshell -s envelopes main -s init stop
% or
% $ erl
% 1> c(envelopes).
% 2> envelopes:main().

-module(envelopes).
-export([main/0]).

-define(PRIOR_LOWER_MAX, 100).
-define(NUM_TRIALS, 10000).

% Helper to generate the [picked, other] list for a given run.
get_trial_values(1, LowerValue) -> [LowerValue, 2 * LowerValue];
get_trial_values(2, LowerValue) -> [2 * LowerValue, LowerValue].

% Returns the result of a single trial. We switch if the value is below the
% cutoff.
single_trial(Cutoff) ->
  [Choice, Other] = get_trial_values(rand:uniform(2), rand:uniform() * ?PRIOR_LOWER_MAX),
  if Choice >= Cutoff -> Choice; true -> Other end.

% Returns the total value of all of the trials for a given cutoff level.
get_multi_trial_total(_, 0, Total) -> Total;
get_multi_trial_total(Cutoff, Remaining, Total) ->
  get_multi_trial_total(Cutoff, Remaining - 1, Total + single_trial(Cutoff)).

% Computes the average value among many trials.
multi_trial(Cutoff) ->
  get_multi_trial_total(Cutoff, ?NUM_TRIALS, 0) / ?NUM_TRIALS.

% Prints the expected value for each possible cutoff.
cutoff_trials(_, 0) -> ok;
cutoff_trials(Cutoff, Remaining) ->
  ExpectedValue = multi_trial(Cutoff),
  io:format("cutoff=~w, expected_value=~w~n", [Cutoff, ExpectedValue]),
  cutoff_trials(Cutoff + 1, Remaining - 1).

main() ->
  cutoff_trials(0, 2 * ?PRIOR_LOWER_MAX + 1).
