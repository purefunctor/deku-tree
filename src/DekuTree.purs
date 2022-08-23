module DekuTree where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (class Foldable, foldl, oneOfMap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (switcher_, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import FRP.Event (ZoraEvent, fromEvent, makeEvent)
import Routing.Hash (matchesWith, setHash)
import Slug as Slug
import Web.Event.Event (preventDefault)

type Sticks l p = List (String /\ String /\ Domable l p)

sticksFromFoldable
  :: forall f l p
   . Foldable f
  => f (String /\ Domable l p)
  -> Sticks l p
sticksFromFoldable = foldl go Nil
  where
  go a (k /\ v) = case Slug.generate k of
    Just k' ->
      Cons (Slug.toString k' /\ k /\ v) a
    Nothing ->
      a

makeDekuTree :: forall l p. Sticks l p -> Array (Domable l p)
makeDekuTree sticks = view
  where
  sticks' :: Map String (String /\ Domable l p)
  sticks' = Map.fromFoldable sticks

  hashRoute :: ZoraEvent String
  hashRoute = fromEvent $ makeEvent \k -> matchesWith Just \old new -> do
    when (old /= Just new) (k new)

  view :: Array (Domable l p)
  view =
    [ D.nav (pure $ D.Class := "sidebar")
        [ D.div (pure $ D.Class := "sidebar-title")
            [ D.a
                ( oneOfMap pure
                    [ D.Href := "/"
                    , D.OnClick := cb \e -> do
                        preventDefault e
                        setHash "/"
                    ]
                )
                [ titleText
                ]
            ]
        , D.div (pure $ D.Class := "sidebar-items")
            [ sidebarItems
            ]
        ]
    , D.main (pure $ D.Class := "content-head")
        [ contentDomable
        ]
    , D.div (pure $ D.Class := "right-padding")
        [
        ]
    ]
    where
    titleText = text $ titleText0 <|> titleTextN
      where
      titleText0 = pure $ "Deku Tree"
      titleTextN = hashRoute <#> \current ->
        if current == "/" || current == "" then
          "Deku Tree"
        else
          case Map.lookup (drop 1 current) sticks' of
            Just (k /\ _) ->
              k
            Nothing ->
              "Not Found"
    contentDomable = switcher_ D.div
      (\current ->
        if current == "/" || current == "" then
          D.div cls landingView
        else
          case Map.lookup (drop 1 current) sticks' of
            Just (_ /\ v) ->
              D.div cls [ v ]
            Nothing ->
              D.div cls [ text_ "Not Found" ]
      ) hashRoute
      where
      cls = pure $ D.Class := "content-body"
    sidebarItems = D.ul_ $ fx $ foldlWithIndex go Nil sticks'
      where
      fx :: forall a. List a -> Array a
      fx = List.toUnfoldable <<< List.reverse
      go i a (k /\ _) = flip Cons a $
        D.li_
          [ D.a
              ( oneOfMap pure
                  [ D.Href := i
                  , D.OnClick := cb \e -> do
                      preventDefault e
                      setHash $ "/" <> i
                  ]
              )
              [ text_ k
              ]
          ]

  landingView =
    [ D.h2_
        [ text_ "Welcome to Deku Tree."
        ]
    , D.hr_ []
    , D.article_
        [ text_ "A collection of FRP-based, interactive, mini web applications. "
        , text_ "To get started, click on any of the entries on the left-hand side "
        , text_ "of this web page. Have fun exploring!"
        ]
    ]
