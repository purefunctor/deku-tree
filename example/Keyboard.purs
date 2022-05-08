module Example.Keyboard where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute (class Attr, Attribute, (:=))
import Deku.Control (blank, plant, text, text_)
import Deku.Core (Domable)
import Deku.DOM (Style)
import Deku.DOM as D
import Effect.Ref as Ref
import FRP.Event (Event, bang, makeEvent, subscribe)
import FRP.Event.Keyboard (down, up)

view :: forall lock payload. Domable lock payload
view = plant $
  [ D.h2_
      [ text_ "Try hitting the following keys on your keyboard:"
      ]
  , D.hr_ blank
  , D.section (bang $ D.Class := "keyboard-container-row")
      [ D.div (keyStyle "KeyS") [ text_ "S" ]
      , D.div (keyStyle "KeyD") [ text_ "D" ]
      , D.div (keyStyle "KeyF") [ text_ "F" ]
      , D.div (keyStyle "Space") [ text_ "Space" ]
      , D.div (keyStyle "KeyJ") [ text_ "J" ]
      , D.div (keyStyle "KeyK") [ text_ "K" ]
      , D.div (keyStyle "KeyL") [ text_ "L" ]
      ]
  , D.hr_ blank
  , D.section (bang $ D.Class := "keyboard-container-row")
      [ D.div_ [ text $ ctrState "KeyS" <#> show ]
      , D.div_ [ text $ ctrState "KeyD" <#> show ]
      , D.div_ [ text $ ctrState "KeyF" <#> show ]
      , D.div_ [ text $ ctrState "Space" <#> show ]
      , D.div_ [ text $ ctrState "KeyJ" <#> show ]
      , D.div_ [ text $ ctrState "KeyK" <#> show ]
      , D.div_ [ text $ ctrState "KeyL" <#> show ]
      ]
  , D.hr_ blank
  ]
  where
  keyStyle :: forall e. Attr e Style String => String -> Event (Attribute e)
  keyStyle keyCode = makeEvent \k -> do
    downC <- subscribe down \keyCode' ->
      when (keyCode == keyCode')
        $ k
        $ D.Style := "background-color: white; color: black;"
    upC <- subscribe up \keyCode' ->
      when (keyCode == keyCode')
        $ k
        $ D.Style := ""
    pure do
      downC
      upC

  ctrState :: String -> Event Int
  ctrState keyCode = bang 0 <|> increment
    where
    increment = makeEvent \k -> do
      current <- Ref.new 1
      subscribe down \keyCode' ->
        when (keyCode == keyCode') do
          current' <- Ref.read current
          k current'
          Ref.modify_ (_ + 1) current
