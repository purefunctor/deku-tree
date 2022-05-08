module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import DekuTree (Sticks, makeDekuTree, sticksFromFoldable)
import Deku.Control (plant, text_)
import Deku.Toplevel (runInBody)
import Effect (Effect)

main :: Effect Unit
main = runInBody (makeDekuTree sticks)
  where
  sticks :: forall l p. Sticks l p
  sticks = sticksFromFoldable
    [ "Keyboard Events" /\ plant (text_ "todo!")
    , "Mouse Events" /\ plant (text_ "todo!")
    ]
