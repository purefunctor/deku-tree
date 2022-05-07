module Main where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute (class Attr, Attribute, (:=))
import Deku.Control (text_)
import Deku.DOM (Style)
import Deku.DOM as D
import Deku.Toplevel (runInBodyA)
import Effect (Effect)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Keyboard (down, up)

main :: Effect Unit
main = runInBodyA view
  where
  view :: forall lock. _ (_ lock _)
  view =
    [ D.header_ [ text_ "Deku Tree" ]
    , D.main_
        [ D.div (keyDownEvent "KeyD" <|> keyUpEvent "KeyD") [ text_ "D" ]
        , D.div (keyDownEvent "KeyF" <|> keyUpEvent "KeyF") [ text_ "F" ]
        , D.div (keyDownEvent "KeyJ" <|> keyUpEvent "KeyJ") [ text_ "J" ]
        , D.div (keyDownEvent "KeyK" <|> keyUpEvent "KeyK") [ text_ "K" ]
        ]
    , D.footer_ [ text_ "PureFunctor, 2022 <|> Made with <3 and FRP" ]
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
