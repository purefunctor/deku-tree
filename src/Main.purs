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
    , D.main_ $ [ "D", "F", "J", "K" ] <#> \key ->
        D.div (keyDownEvent key <|> keyUpEvent key) [ text_ key ]
    , D.footer_ [ text_ "PureFunctor, 2022 <|> Made with <3 and FRP" ]
    ]

keyDownEvent :: forall e. Attr e Style String => String -> Event (Attribute e)
keyDownEvent key = makeEvent \k ->
  subscribe down \keyCode ->
    if "Key" <> key == keyCode then
      k $ D.Style := "background-color: white; color: black;"
    else
      pure unit

keyUpEvent :: forall e. Attr e Style String => String -> Event (Attribute e)
keyUpEvent key = makeEvent \k ->
  subscribe up \keyCode ->
    if "Key" <> key == keyCode then
      k $ D.Style := (mempty :: String)
    else
      pure unit
