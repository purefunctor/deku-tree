module DekuTree where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (class Foldable, foldl, oneOfMap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (plant, switcher, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import FRP.Event (Event, bang, makeEvent)
import FRP.Event.VBus (V, vbus)
import Routing.Hash (matchesWith, setHash)
import Slug as Slug
import Type.Prelude (Proxy(..))
import Web.Event.Event (preventDefault)

type Sticks l p = Map String (String /\ Domable l p)

sticksFromFoldable
  :: forall f l p
   . Foldable f
  => f (String /\ Domable l p)
  -> Sticks l p
sticksFromFoldable = foldl go Map.empty
  where
  go a (k /\ v) = case Slug.generate k of
    Just k' ->
      Map.insert (Slug.toString k') (k /\ v) a
    Nothing ->
      a

type DekuTreeEvents = V ( )

makeDekuTree :: forall l p. Sticks l p -> Domable l p
makeDekuTree sticks = plant $ vbus (Proxy :: Proxy DekuTreeEvents) viewFn
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
                    , D.OnClick := cb \e -> do
                        preventDefault e
                        setHash "/"
                    ]
                )
                [ titleText
                ]
            ]
        , D.div (bang $ D.Class := "sidebar-items") sidebarItems
        ]
    , D.main (bang $ D.Class := "content") contentDomable
    ]
    where
    titleText = text $ titleText0 <|> titleTextN
      where
      titleText0 = bang $ "Deku Tree"
      titleTextN = hashRoute <#> \current ->
        if current == "/" || current == "" then
          "Deku Tree"
        else
          case Map.lookup (drop 1 current) sticks of
            Just (k /\ _) ->
              k
            Nothing ->
              "Not Found"
    contentDomable = flip switcher hashRoute \current ->
      if current == "/" || current == "" then
        D.div_ [ text_ "Deku Tree" ]
      else
        case Map.lookup (drop 1 current) sticks of
          Just (_ /\ v) ->
            D.div_ v
          Nothing ->
            D.div_ [ text_ "Not Found" ]
    sidebarItems = plant $ D.ul_ $ foldlWithIndex go [] sticks
      where
      go i a (k /\ _) = a <>
        [ D.li_
          [ D.a
              ( oneOfMap bang
                  [ D.Href := i
                  , D.OnClick := cb \e -> do
                      preventDefault e
                      setHash $ "/" <> i
                  ]
              )
            [ text_ k
            ]
          ]
        ]
