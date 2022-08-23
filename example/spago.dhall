let toplevel = ../spago.dhall

in      toplevel
    //  { dependencies =
              toplevel.dependencies
            # [ "aff"
              , "effect"
              , "node-buffer"
              , "node-fs-aff"
              , "refs"
              , "st"
              ]
        , sources = toplevel.sources # [ "example/*.purs" ]
        }
