-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Emit a sequence of sinusoids for testing
-- filter functions.

import System.Console.ParseArgs
import Text.Printf

data ArgIndex = ArgIntVals | ArgStartPeriod | ArgEndPeriod 
              | ArgSteps | ArgCycles 
              deriving (Ord, Eq, Show)

argd :: [Arg ArgIndex]
argd = [
  Arg { argIndex = ArgIntVals,
        argAbbr = Just 'i',
        argName = Just "intvals",
        argData = argDataOptional "range" ArgtypeInteger,
        argDesc = "Output integers in -range..range instead of floats" },
  Arg { argIndex = ArgStartPeriod,
        argAbbr = Just 's',
        argName = Just "start",
        argData = argDataDefaulted "samples" ArgtypeDouble 20.0,
        argDesc = "Start with a sinusoid of given floating-point period" },
  Arg { argIndex = ArgEndPeriod,
        argAbbr = Just 'e',
        argName = Just "end",
        argData = argDataDefaulted "samples" ArgtypeDouble 2.0,
        argDesc = "End with a sinusoid of the given floating-point period" },
  Arg { argIndex = ArgSteps,
        argAbbr = Just 'n',
        argName = Just "steps",
        argData = argDataDefaulted "count" ArgtypeInt 10,
        argDesc = "Number of sinusoid steps" },
  Arg { argIndex = ArgCycles,
        argAbbr = Just 'c',
        argName = Just "cycles",
        argData = argDataDefaulted "count" ArgtypeInt 10,
        argDesc = "Number of sinusoid cycles per step" } ]

data Desc = Desc {
  descStartPeriod, descEndPeriod :: Double,
  descSteps, descCycles :: Int }

genSamples :: Desc -> [Double]
genSamples desc =
  concatMap genSinusoid [0 .. steps - 1]
  where
    steps = descSteps desc
    genSinusoid step =
      concat $ replicate (descCycles desc) oneCycle
      where
        oneCycle = map sin [ 0.5 * p, 
                             1.5 * p ..
                             (period - 0.5) * p ]
                   where
                     p = 2.0 * pi / period
        period = let fs = 1.0 / s
                     fe = 1.0 / e in
                 let fx = fs * (fe / fs) ** (fStep / fSteps) in
                 1.0 / fx
                 where
                   fStep = fromIntegral step
                   fSteps = fromIntegral (steps - 1)
                   s = descStartPeriod desc
                   e = descEndPeriod desc


emitFloats :: [Double] -> IO ()
emitFloats samples = 
  mapM_ printDouble samples
  where
    printDouble s = 
      printf "%.15f\n" s

emitIntegers :: Integer -> [Double] -> IO ()
emitIntegers range samples = 
  mapM_ printInteger samples
  where
    printInteger s = 
      printf "%d\n" $ ((ceiling (s * fromIntegral range)) :: Integer)

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let desc = Desc {
        descStartPeriod = getRequiredArg argv ArgStartPeriod,
        descEndPeriod = getRequiredArg argv ArgEndPeriod,
        descSteps = getRequiredArg argv ArgSteps,
        descCycles = getRequiredArg argv ArgCycles }
  let samples = genSamples desc
  case getArg argv ArgIntVals of
    Nothing -> emitFloats samples
    Just range -> emitIntegers range samples
