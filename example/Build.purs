module Build where

import Prelude

import App (app)
import Data.Foldable (fold)
import Deku.Toplevel (Template(..), runSSR)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.FS.Aff (writeTextFile)
import Node.Encoding (Encoding(..))

head :: String
head = fold
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<title>Deku Tree</title>"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<meta charset=\"utf-8\">"
  , "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">"
  , "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>"
  , "<link href=\"https://fonts.googleapis.com/css2?family=Koulen&family=PT+Sans&display=swap\" rel=\"stylesheet\">"
  , "<link rel=\"stylesheet\" href=\"index.css\">"
  , "<link rel=\"icon\" href=\"favicon.ico\" type=\"image/x-icon\">"
  , "<script src=\"index.js\" type=\"module\"></script>"
  , "</head>"
  ]

tail :: String
tail = "</html>\n"

main :: Effect Unit
main = do
  text <- runSSR (Template { head, tail }) app
  launchAff_ $
    writeTextFile UTF8 "./public/index.html" text
