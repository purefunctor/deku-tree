module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Control (plant, text_)
import Deku.Toplevel (runInBody)
import DekuTree (Sticks, makeDekuTree, sticksFromFoldable)
import Effect (Effect)
import Example.Keyboard as Keyboard

main :: Effect Unit
main = runInBody (makeDekuTree sticks)
  where
  sticks :: forall l p. Sticks l p
  sticks = sticksFromFoldable
    [ "Keyboard Events" /\ Keyboard.view
    , "Mouse Events" /\ plant (text_ "todo!")
    ]
