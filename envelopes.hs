import System.Random

num_trials = 1000
prior_lower_max = 100
max_cutoff = 2 * prior_lower_max

-- Returns the result of a single trial. envelope=0 means we picked the lower
-- of the two envelopes, envelope=1 means we picked the higher. We switch if
-- the value is below the cutoff.
singleTrial envelope lower_value cutoff =
  if value >= cutoff then value else other
  where higher_value = 2 * lower_value
        (value, other) = if envelope == 0 
	      then (lower_value, higher_value) 
	      else (higher_value, lower_value)

-- Runs multiple trials to do Monte Carlo approximation of the expected value.
multiTrial envelopes lower_values cutoff = total_expected_value / total_runs
    where total_expected_value = fromIntegral (multiTrial' envelopes lower_values cutoff) 
          total_runs           = fromIntegral (length envelopes)
multiTrial' [] [] _ = 0
multiTrial' (e:es) (l:ls) cutoff = (singleTrial e l cutoff) + (multiTrial' es ls cutoff)

-- Generates the Monte Carlo approximation of the expected value for each 
-- possible cutoff value.
cutoffTrials envelopes lower_values cutoff
  | cutoff > max_cutoff = []
  | otherwise           = [(cutoff, expected_value)] ++ cutoffTrials e_next v_next (cutoff + 1)
  where (e_now, e_next) = splitAt num_trials envelopes
        (v_now, v_next) = splitAt num_trials lower_values
        expected_value  = multiTrial e_now v_now cutoff

-- Formats the output of cutoffTrials.
formatResult (cutoff, expected_value) = 
	"cutoff=" ++ show cutoff ++ ", expected_value=" ++ show expected_value 

-- Generates a new random integer list of a given length inside the given 
-- bounds.
randomIntList bounds len = do
  g <- newStdGen
  return (take len (randomRs bounds g :: [Int]))

-- Runs the Monte Carlo approximation for each possible cutoff using a randomly
-- generated input sequence of envelope choices and values.
main = do
  let len = num_trials * (max_cutoff + 1)
  envelopes <- randomIntList (0, 1) len
  values <- randomIntList (0, prior_lower_max) len
  let results = cutoffTrials envelopes values 0
  mapM_ putStrLn (map formatResult results)
