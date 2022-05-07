module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Deku.Attribute (cb, (:=))
import Deku.Control (plant, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (Event, bang, makeEvent)
import FRP.Event.VBus (V, vbus)
import Partial.Unsafe (unsafeCrashWith)
import Routing.Hash (matchesWith, setHash)
import Slug (generate, toString)
import Type.Prelude (Proxy(..))
import Web.Event.Event (preventDefault)

type Evt = V
  (
  )

type Env =
  {
  }

main :: Effect Unit
main = runInBody1 (view {})

view :: forall l p. Env -> Event (Domable l p)
view _ = vbus (Proxy :: Proxy Evt) viewFn
  where
  hashRoute :: Event String
  hashRoute = makeEvent \k -> matchesWith Just \old new -> do
    when (old /= Just new) (k new)

  viewFn :: _ -> _ -> Domable l p
  viewFn _ _ = plant $
    [ D.nav (bang $ D.Class := "sidebar")
        [ D.div (bang $ D.Class := "sidebar-title")
            [ D.a
                ( oneOfMap bang
                    [ D.Href := "/"
                    , D.OnClick := titleAnchorClick
                    ]
                )
                [ titleText
                ]
            ]
        , D.div (bang $ D.Class := "sidebar-items")
            [ D.ul_ $ [ "Keyboard and Mouse, and a very, very long title", "Mouse" ] <#> \item ->
                case toString <$> generate item of
                  Just item' ->
                    D.li_
                      [ D.a
                          ( oneOfMap bang
                              [ D.Href := item'
                              , D.OnClick := itemAnchorClick item'
                              ]
                          )
                          [ text_ item
                          ]
                      ]
                  Nothing ->
                    unsafeCrashWith "Invalid slug."
            ]
        ]
    , D.main (bang $ D.Class := "content")
        [ titleText
        ]
    ]
    where
    titleText = text $ titleText0 <|> titleTextN
      where
      titleText0 = bang $ "Deku Tree"
      titleTextN = hashRoute <#> \current ->
        if current == "/" || current == "" then
          "Deku Tree"
        else
          drop 1 current
    titleAnchorClick = cb \e -> do
      preventDefault e
      setHash "/"
    itemAnchorClick item = cb \e -> do
      preventDefault e
      setHash $ "/" <> item
