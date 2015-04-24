-- $ brew install haskell-platform
-- $ runhaskell envelopes.hs
-- or
-- $ ghc -o envelopes_hs envelopes.hs; ./envelopes_hs

import Control.Monad.State
import System.Random

numTrials = 10000
priorLowerMax = 100
priorLowerMaxF = fromIntegral priorLowerMax
maxCutoff = 2 * priorLowerMax

-- Returns the result of a single trial. envelope=0 means we picked the lower
-- of the two envelopes, envelope=1 means we picked the higher. We switch if
-- the value is below the cutoff.
getExpectedValue :: Int -> Float -> Float -> Float
getExpectedValue envelope lowerValue cutoff =
  if value >= cutoff then value else other
  where higherValue = 2 * lowerValue
        (value, other) = if envelope == 0 
          then (lowerValue, higherValue) 
          else (higherValue, lowerValue)

singleTrial :: Float -> State StdGen Float
singleTrial cutoff = do
  envelope <- state $ randomR (0, 1)
  lowerValue <- state $ randomR (0.0, priorLowerMaxF)
  return $ getExpectedValue envelope lowerValue cutoff

-- Runs multiple trials to do Monte Carlo approximation of the expected value.
-- Surprisingly, a tail recursive version of this is much slower.
multiTrial :: Float -> State StdGen Float
multiTrial cutoff = do
  loop 0 0
  where loop total count = do
        if count /= numTrials 
          then do
            singleTrialValue <- singleTrial cutoff
            loop (total + singleTrialValue) (count + 1)
          else return $ total / fromIntegral count

-- Generates the Monte Carlo approximation of the expected value for each 
-- possible cutoff value.
main :: IO ()
main = do
  loop 0
  where loop cutoff = do
        if cutoff <= maxCutoff then do
          gen <- newStdGen
          let expectedValue = evalState (multiTrial (fromIntegral cutoff)) gen
          putStrLn $ "cutoff=" ++ show cutoff ++ ", expectedValue=" ++ show expectedValue
          loop (cutoff + 1)
        else
          return ()
