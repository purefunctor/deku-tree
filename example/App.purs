module App where

import Control.Monad.ST.Class (class MonadST)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Domable)
import Deku.DOM as D
import DekuTree (Sticks, makeDekuTree, sticksFromFoldable)
import Example.Keyboard as Keyboard
import FRP.Event (FromEvent)

app :: forall s m lock payload. MonadST s m => FromEvent m (Domable m lock payload)
app = D.div_ (makeDekuTree sticks)
  where
  sticks :: Sticks m lock payload
  sticks = sticksFromFoldable
    [ "Keyboard Events" /\ Keyboard.view
    , "Mouse Events" /\ text_ "todo!"
    ]
