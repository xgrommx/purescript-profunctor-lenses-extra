module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Lens.Argonaut (argonautSpec)

main :: Effect Unit
main = do
  argonautSpec
