# $ brew install elixir
# $ elixir -r envelopes.ex -e 'Envelopes.main'
# or
# $ iex
# iex(1)> c("envelopes.ex")
# iex(2)> Envelopes.main  

defmodule Envelopes do

  @prior_lower_max 100
  @num_trials 10000

  # Returns the result of a single trial. We switch if the value is below the
  # cutoff.
  def single_trial(cutoff) do
    lower_value = :random.uniform() * @prior_lower_max
    higher_value = 2 * lower_value
    case :random.uniform(2) do
      1 -> if lower_value >= cutoff do lower_value else higher_value end
      2 -> if higher_value >= cutoff do higher_value else lower_value end
    end
  end

  # Returns the total value of all of the trials for a given cutoff level.
  def get_multi_trial_total(cutoff, i, total) do
    case i do
      @num_trials -> total
      _ -> get_multi_trial_total(cutoff, i + 1, total + single_trial(cutoff))
    end
  end

  # Prints the expected value for each possible cutoff.
  def cutoff_trials(cutoff) do
    if cutoff <= 2 * @prior_lower_max do
      expected_value = get_multi_trial_total(cutoff, 0, 0) / @num_trials
      IO.puts :io.format("cutoff=~w, expected_value=~w~n", [cutoff, expected_value])
      cutoff_trials(cutoff + 1)
    end
  end

  def main do
    cutoff_trials(0)
  end
end