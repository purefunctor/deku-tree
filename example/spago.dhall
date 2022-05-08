let toplevel = ../spago.dhall

in      toplevel
    //  { dependencies = toplevel.dependencies # [ "behaviors", "effect" ]
        , sources = toplevel.sources # [ "example/*.purs" ]
        }
