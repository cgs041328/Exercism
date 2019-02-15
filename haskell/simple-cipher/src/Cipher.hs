module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random


caesarTransformWith :: (Int -> Int -> Int) -> String -> String -> String
caesarTransformWith op shifts text = zipWith shiftLetter text (cycle intShifts)
  where
    intShifts = map (subtract fromEnumA . fromEnum) shifts
    shiftLetter plainLetter shift = toEnum $ fromEnumA + ((fromEnum plainLetter - fromEnumA) `op` shift) `mod` (fromEnumZ - fromEnumA + 1)
    fromEnumA = fromEnum 'a'
    fromEnumZ = fromEnum 'z'


caesarEncode :: String -> String -> String
caesarEncode = caesarTransformWith (+)


caesarDecode :: String -> String -> String
caesarDecode = caesarTransformWith (-)


caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom plaintext = do
  g <- newStdGen
  let key = take (length plaintext) $ randomRs ('a', 'z') g
  return (key, caesarEncode key plaintext)
