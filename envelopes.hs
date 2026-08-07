-- System.Random is not a GHC boot library, so it needs installing separately.
-- This is the one file here with a dependency outside its language's stdlib.
-- $ brew install ghc cabal-install
-- $ cabal update && cabal install --lib random
-- $ runhaskell envelopes.hs
-- or
-- $ ghc -o envelopes_hs envelopes.hs; ./envelopes_hs
--
-- `cabal install --lib` writes a global package environment at
-- ~/.ghc/<arch>/environments/default, which changes what every ghc on the
-- machine can import. A self-contained alternative is to make this a cabal
-- script instead: a `#!/usr/bin/env cabal` shebang plus a
-- `{- cabal: build-depends: base, random -}` block, run with ./envelopes.hs.

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
main = do
  loop 0
  where loop cutoff = do
          if cutoff <= maxCutoff then do
            envelopes <- randomIntList (0, 1) numTrials
            lowerValues <- randomIntList (0, priorLowerMax) numTrials
            let expectedValue = multiTrial envelopes lowerValues cutoff
            putStrLn $ "cutoff=" ++ show cutoff ++ ", expectedValue=" ++ show expectedValue
            loop (cutoff + 1)
          else
            return ()
