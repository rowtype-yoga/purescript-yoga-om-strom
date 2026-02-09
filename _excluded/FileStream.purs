module Yoga.Om.Strom.FileStream where

import Prelude

import Data.Array as Array
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Yoga.Om (Om)
import Yoga.Om.Strom as Strom

foreign import _writeFile :: String -> String -> Effect Unit
foreign import _appendFile :: String -> String -> Effect Unit
foreign import _readFile :: String -> Effect String
foreign import _unlinkFile :: String -> Effect Unit

-- | Infinite stream of random ints in [lo, hi]
randomInts :: forall ctx err. Int -> Int -> Strom.Strom ctx err Int
randomInts lo hi =
  Strom.repeatOmStromInfinite (liftEffect $ randomInt lo hi)

-- | Stream n random integers to a file at the given path, one per line.
streamRandomToFile :: String -> Int -> Om {} () Unit
streamRandomToFile path n = do
  liftEffect $ _writeFile path ""
  randomInts 0 999999
    # Strom.takeStrom n
    # Strom.mapStrom (\i -> show i <> "\n")
    # Strom.traverseStrom_ (\line -> liftEffect $ _appendFile path line)

-- | Read a file and count non-empty lines
countLines :: String -> Effect Int
countLines path = do
  content <- _readFile path
  let lines = Array.filter (_ /= "") $ String.split (String.Pattern "\n") content
  pure $ Array.length lines
