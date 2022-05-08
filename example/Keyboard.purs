module Example.Keyboard where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute (class Attr, Attribute, (:=))
import Deku.Control (blank, plant, text_)
import Deku.Core (Domable)
import Deku.DOM (Style)
import Deku.DOM as D
import FRP.Event (Event, bang, makeEvent, subscribe)
import FRP.Event.Keyboard (down, up)

view :: forall lock payload. Domable lock payload
view = plant $
  D.div (bang $ D.Class := "keyboard-container")
    [ D.h2_
        [ text_ "Try hitting the following keys on your keyboard:"
        ]
    , D.hr_ blank
    , D.section (bang $ D.Class := "keyboard-container-row")
        [ D.div (keyDownEvent "KeyS" <|> keyUpEvent "KeyS") [ text_ "S" ]
        , D.div (keyDownEvent "KeyD" <|> keyUpEvent "KeyD") [ text_ "D" ]
        , D.div (keyDownEvent "KeyF" <|> keyUpEvent "KeyF") [ text_ "F" ]
        , D.div (keyDownEvent "Space" <|> keyUpEvent "Space") [ text_ "Space" ]
        , D.div (keyDownEvent "KeyJ" <|> keyUpEvent "KeyJ") [ text_ "J" ]
        , D.div (keyDownEvent "KeyK" <|> keyUpEvent "KeyK") [ text_ "K" ]
        , D.div (keyDownEvent "KeyL" <|> keyUpEvent "KeyL") [ text_ "L" ]
        ]
    , D.hr_ blank
    ]

keyDownEvent :: forall e. Attr e Style String => String -> Event (Attribute e)
keyDownEvent keyCode = makeEvent \k ->
  subscribe down \keyCode' ->
    if keyCode == keyCode' then
      k $ D.Style := "background-color: white; color: black;"
    else
      pure unit

keyUpEvent :: forall e. Attr e Style String => String -> Event (Attribute e)
keyUpEvent keyCode = makeEvent \k ->
  subscribe up \keyCode' ->
    if keyCode == keyCode' then
      k $ D.Style := (mempty :: String)
    else
      pure unit
