module DekuTree where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST)
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
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import FRP.Event (AnEvent, FromEvent, bang, fromEvent, makeEvent)
import Routing.Hash (matchesWith, setHash)
import Slug as Slug
import Web.Event.Event (preventDefault)

type Sticks m l p = List (String /\ String /\ Domable m l p)

sticksFromFoldable
  :: forall s m f l p
   . Foldable f
  => MonadST s m
  => f (String /\ Domable m l p)
  -> Sticks m l p
sticksFromFoldable = foldl go Nil
  where
  go a (k /\ v) = case Slug.generate k of
    Just k' ->
      Cons (Slug.toString k' /\ k /\ v) a
    Nothing ->
      a

makeDekuTree :: forall s m l p. FromEvent m (MonadST s m => Sticks m l p -> Array (Domable m l p))
makeDekuTree sticks = view
  where
  sticks' :: Map String (String /\ Domable m l p)
  sticks' = Map.fromFoldable sticks

  hashRoute :: AnEvent m String
  hashRoute = fromEvent $ makeEvent \k -> matchesWith Just \old new -> do
    when (old /= Just new) (k new)

  view :: Array (Domable m l p)
  view =
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
        , D.div (bang $ D.Class := "sidebar-items")
            [ sidebarItems
            ]
        ]
    , D.main (bang $ D.Class := "content-head")
        [ contentDomable
        ]
    , D.div (bang $ D.Class := "right-padding")
        [
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
          case Map.lookup (drop 1 current) sticks' of
            Just (k /\ _) ->
              k
            Nothing ->
              "Not Found"
    contentDomable = flip switcher hashRoute \current ->
      if current == "/" || current == "" then
        D.div cls landingView
      else
        case Map.lookup (drop 1 current) sticks' of
          Just (_ /\ v) ->
            D.div cls [ v ]
          Nothing ->
            D.div cls [ text_ "Not Found" ]
      where
      cls = bang $ D.Class := "content-body"
    sidebarItems = D.ul_ $ fx $ foldlWithIndex go Nil sticks'
      where
      fx :: forall a. List a -> Array a
      fx = List.toUnfoldable <<< List.reverse
      go i a (k /\ _) = flip Cons a $
        D.li_
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
