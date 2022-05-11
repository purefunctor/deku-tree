module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Toplevel (runInBodyA)
import DekuTree (Sticks, makeDekuTree, sticksFromFoldable)
import Effect (Effect)
import Example.Keyboard as Keyboard

main :: Effect Unit
main = runInBodyA (makeDekuTree sticks)
  where
  sticks :: forall l p. Sticks l p
  sticks = sticksFromFoldable
    [ "Keyboard Events" /\ Keyboard.view
    , "Mouse Events" /\ text_ "todo!"
    ]
