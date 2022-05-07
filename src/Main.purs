module Main where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute (class Attr, Attribute, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element)
import Deku.DOM (Style)
import Deku.DOM as D
import Deku.Toplevel (runInBodyA)
import Effect (Effect)
import FRP.Behavior (sample)
import FRP.Behavior.Mouse (position)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Keyboard (down, up)
import FRP.Event.Mouse (Mouse)
import FRP.Event.Mouse as Mouse
import Data.Maybe

type Env =
  { mouse :: Mouse
  }

main :: Effect Unit
main = do
  mouse <- Mouse.getMouse
  runInBodyA (view { mouse })

view :: forall lock payload. Env -> Array (Element lock payload)
view { mouse } =
  [ D.header_ [ text $ mouseMoveEvent mouse ]
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

mouseMoveEvent :: Mouse -> Event String
mouseMoveEvent mouse = Mouse.withPosition mouse animationFrame <#> _.pos >>> maybe "No Mouse!" show
