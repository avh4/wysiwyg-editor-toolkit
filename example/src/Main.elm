module Main exposing (main)

import Browser
import Html exposing (Html)


main : Program () {} Never
main =
    Browser.document
        { init = \() -> ( {}, Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view =
            \model ->
                { title = "avh4/wysiwyg-editor-toolkit demo"
                , body =
                    [ Html.h2 [] [ Html.text "avh4/wysiwyg-editor-toolkit demo" ]
                    ]
                }
        }
