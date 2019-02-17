module WysiwygEditorToolkitTests exposing (all)

import Expect
import Html exposing (Html)
import Json.Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, tag)
import TestContext exposing (TestContext)
import WysiwygEditorToolkit as Toolkit


all : Test
all =
    describe "WysiwygEditorToolkit" <|
        [ test "can edit the value of a text field" <|
            \() ->
                start (Toolkit.viewTextEditable identity)
                    |> fillInToolkitText
                        [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                        "updated"
                    |> TestContext.expectModel
                        (.editorData >> Expect.equal "updated")
        ]


type alias Model =
    { toolkitState : ()
    , editorData : String
    }


type Msg
    = Edit String


start : (String -> Html String) -> TestContext Msg Model ()
start view =
    TestContext.create
        { init =
            ( { toolkitState = ()
              , editorData = "init"
              }
            , ()
            )
        , update =
            \msg model ->
                case msg of
                    Edit text ->
                        ( { model | editorData = text }
                        , ()
                        )
        , view =
            \model ->
                view model.editorData
                    |> Html.map Edit
        }


fillInToolkitText : List Selector -> String -> TestContext msg model effect -> TestContext msg model effect
fillInToolkitText selectors text =
    TestContext.simulate
        (Query.find selectors)
        ( "content-changed"
        , Json.Encode.object
            [ ( "detail"
              , Json.Encode.object
                    [ ( "textContent", Json.Encode.string text ) ]
              )
            ]
        )
