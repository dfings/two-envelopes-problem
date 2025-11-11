# $ brew install elixir
# $ elixir -r envelopes.ex -e 'Envelopes.main'
# or
# $ iex
# iex(1)> c("envelopes.ex")
# iex(2)> Envelopes.main

defmodule Envelopes do
  @moduledoc """
  A simulation of the two envelopes problem.
  """

  @prior_lower_max 100
  @num_trials 10_000
  @cutoff_max 2 * @prior_lower_max

  @doc """
  Runs a single trial of the simulation.
  """
  @spec single_trial(number) :: number
  def single_trial(cutoff) do
    lower_value = :random.uniform() * @prior_lower_max
    higher_value = 2 * lower_value

    # You are presented with one of the envelopes at random.
    [presented_value, other_value] = Enum.shuffle([lower_value, higher_value])

    # You decide whether to switch based on the cutoff.
    if presented_value >= cutoff do
      presented_value # Keep the envelope
    else
      other_value # Switch to the other envelope
    end
  end

  @doc """
  Recursively calculates the total value from multiple trials for a given cutoff.
  This function is tail-recursive. It should be called with initial
  accumulators, e.g., `multi_trial(cutoff, 0, 0)`.
  """
  @spec multi_trial(number, non_neg_integer, number) :: number
  def multi_trial(_, @num_trials, total), do: total
  def multi_trial(cutoff, i, total) do
    multi_trial(cutoff, i + 1, total + single_trial(cutoff))
  end

  @doc """
  Runs the simulation for a range of cutoff values and prints the expected
  outcome for each cutoff.
  """
  def main do
    0..@cutoff_max
    |> Enum.each(fn cutoff ->
      total_value = multi_trial(cutoff, 0, 0)
      expected_value = total_value / @num_trials
      IO.puts("cutoff=#{cutoff}, expected_value=#{expected_value}")
    end)
  end
end
