let toplevel = ../spago.dhall

in      toplevel
    //  { dependencies =
            toplevel.dependencies # [ "behaviors", "console", "effect", "refs", "st" ]
        , sources = toplevel.sources # [ "example/*.purs" ]
        }
