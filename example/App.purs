module App where

import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import DekuTree (makeDekuTree, sticksFromFoldable)
import Example.Keyboard as Keyboard

app :: forall s m lock payload. Korok s m => Domable m lock payload
app = D.div_ (makeDekuTree sticks)
  where
  sticks = sticksFromFoldable
    [ "Keyboard Events" /\ Keyboard.view
    , "Mouse Events" /\ text_ "todo!"
    ]
