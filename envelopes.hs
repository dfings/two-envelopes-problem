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
multiTrial envelopes lower_values cutoff = multiTrial' envelopes lower_values cutoff 0 0
multiTrial' [] [] _ total_value total_trials = fromIntegral total_value / fromIntegral total_trials	
multiTrial' (e:es) (l:ls) cutoff total_value total_trials =
    multiTrial' es ls cutoff (total_value + new_value) (total_trials + 1)
    where new_value = singleTrial e l cutoff

-- Generates the Monte Carlo approximation of the expected value for each 
-- possible cutoff value.
cutoffTrials envelopes lower_values = cutoffTrials' envelopes lower_values 0
cutoffTrials' envelopes lower_values cutoff
  | cutoff == max_cutoff = [(cutoff, multiTrial envelopes lower_values max_cutoff)]
  | otherwise            = [(cutoff, multiTrial e_now v_now cutoff)] ++
                           cutoffTrials' e_next v_next (cutoff + 1)
  where (e_now, e_next) = splitAt num_trials envelopes
        (v_now, v_next) = splitAt num_trials lower_values

-- Formats the output of cutoffTrials.
formatResult (cutoff, expected_value) = 
	"cutoff=" ++ show cutoff ++ ", expected_value=" ++ show expected_value 

-- Generates a new random integer list of a given length inside the given 
-- bounds.
randomIntList bounds length = do
  g <- newStdGen
  return (take length (randomRs bounds g :: [Int]))

-- Runs the Monte Carlo approximation for each possible cutoff using a randomly
-- generated input sequence of envelope choices and values.
main = do
  let length = num_trials * (max_cutoff + 1)
  envelopes <- randomIntList (0, 1) length
  values <- randomIntList (0, prior_lower_max) length
  let results = cutoffTrials envelopes values
  mapM_ putStrLn (map formatResult results)
