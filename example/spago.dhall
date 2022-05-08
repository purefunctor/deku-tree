let toplevel = ../spago.dhall

in  toplevel // { sources = toplevel.sources # [ "example/*.purs" ] }
