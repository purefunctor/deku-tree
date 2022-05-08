let toplevel = ../spago.dhall

in      toplevel
    //  { dependencies = toplevel.dependencies # [ "effect" ]
        , sources = toplevel.sources # [ "example/*.purs" ]
        }
