
{-# LANGUAGE BangPatterns, NumericUnderscores #-}
module ZK.Groth16.Aux.Time where

--------------------------------------------------------------------------------

import Control.Monad
import Text.Printf
import System.IO
import System.Clock 

--------------------------------------------------------------------------------

measureTime :: IO a -> IO (TimeSpec,a)
measureTime action = do
  let clock = Realtime -- Monotonic      -- which one to choose??
  start <- getTime clock
  !y <- action
  stop <- seq y (getTime clock)
  let elapsed = diffTimeSpec stop start
  return (elapsed,y)

--------------------------------------------------------------------------------

printMeasureTime :: Bool -> String -> IO a -> IO a
printMeasureTime False text action = action
printMeasureTime True  text action = do
  putStr $ text ++ "..."
  hFlush stdout
  (elapsed,!y) <- measureTime action
  putStrLn (" " ++ formatTime elapsed)
  return y

--------------------------------------------------------------------------------

formatTime :: TimeSpec -> String
formatTime tspec = printf "%.3f secs" x where
  inano = toNanoSecs tspec
  x = fromInteger inano / 1_000_000_000.0 :: Double

--------------------------------------------------------------------------------

maybeWhen :: Bool -> IO a -> IO (Maybe a)
maybeWhen False _      = return Nothing
maybeWhen True  action = fmap Just action 

unlessWhen :: Bool -> IO a -> IO (Maybe a)
unlessWhen bool = maybeWhen (not bool)

--------------------------------------------------------------------------------
