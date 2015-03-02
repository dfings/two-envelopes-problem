import System.Random

num_trials = 1000
prior_lower_max = 100

singleTrial envelope lower_value cutoff
  | envelope == 0 && lower_value >= cutoff  = lower_value
  | envelope == 0                           = higher_value
  | envelope == 1 && higher_value >= cutoff = higher_value
  | envelope == 1                           = lower_value
  where higher_value = 2 * lower_value

multiTrial envelopes lower_values cutoff = multiTrial' envelopes lower_values cutoff 0 0
multiTrial' [] [] _ total_value total_trials = fromIntegral total_value / fromIntegral total_trials	
multiTrial' (e:es) (l:ls) cutoff total_value total_trials =
    multiTrial' es ls cutoff (total_value + new_value) (total_trials + 1)
    where new_value = singleTrial e l cutoff

cutoffTrials envelopes lower_values = cutoffTrials' envelopes lower_values 0
cutoffTrials' envelopes lower_values cutoff
  | cutoff == prior_lower_max = [(cutoff, multiTrial envelopes lower_values prior_lower_max)]
  | otherwise                 = [(cutoff, multiTrial e_now v_now cutoff)] ++ cutoffTrials' e_next v_next (cutoff + 1)
  where e_now = (take num_trials envelopes)
        e_next = (drop num_trials envelopes)
        v_now = (take num_trials lower_values)
        v_next = (drop num_trials lower_values)

formatResult (cutoff, expected_value) = "cutoff=" ++ show cutoff ++ ", expected_value=" ++ show expected_value 

randomList bounds length = do
  g <- newStdGen
  return (take length (randomRs bounds g :: [Int]))

main = do
  let length = num_trials * (prior_lower_max + 1)
  envelopes <- randomList (0, 1) length
  values <- randomList (0 ,100) length
  let results = cutoffTrials envelopes values
  mapM_ putStrLn (map formatResult results)
