-- $ brew install haskell-platform
-- $ runhaskell envelopes.hs
-- or
-- $ ghc -o envelopes_hs envelopes.hs; ./envelopes_hs

import System.Random

numTrials = 10000
priorLowerMax = 100
maxCutoff = 2 * priorLowerMax

-- Returns the result of a single trial. envelope=0 means we picked the lower
-- of the two envelopes, envelope=1 means we picked the higher. We switch if
-- the value is below the cutoff.
singleTrial envelope lowerValue cutoff =
  if value >= cutoff then value else other
  where higherValue = 2 * lowerValue
        (value, other) = if envelope == 0 
          then (lowerValue, higherValue) 
          else (higherValue, lowerValue)

-- Runs multiple trials to do Monte Carlo approximation of the expected value.
-- Surprisingly, a tail recursive version of this is much slower.
multiTrial envelopes lowerValues cutoff = totalExpectedValue / fromIntegral numTrials
    where totalExpectedValue = fromIntegral (multiTrial' envelopes lowerValues cutoff) 
          multiTrial' [] [] _ = 0
          multiTrial' (e:es) (l:ls) cutoff = (singleTrial e l cutoff) + (multiTrial' es ls cutoff)

-- Generates a new random integer list of a given length inside the given 
-- bounds.
randomIntList bounds len = do
  g <- newStdGen
  return (take len (randomRs bounds g :: [Int]))

-- Generates the Monte Carlo approximation of the expected value for each 
-- possible cutoff value.
cutoffTrials cutoff = do
  if cutoff /= maxCutoff then do
    envelopes <- randomIntList (0, 1) numTrials
    lowerValues <- randomIntList (0, priorLowerMax) numTrials
    let expectedValue = multiTrial envelopes lowerValues cutoff
    putStrLn $ "cutoff=" ++ show cutoff ++ ", expectedValue=" ++ show expectedValue
    cutoffTrials (cutoff + 1)
  else
    return ()

-- Runs the Monte Carlo approximation for each possible cutoff using a randomly
-- generated input sequence of envelope choices and values.
main = do
  cutoffTrials 0
