module WysiwygEditorToolkitTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, tag)
import TestContext exposing (TestContext)
import WysiwygEditorToolkit as Toolkit


all : Test
all =
    describe "WysiwygEditorToolkit" <|
        let
            definition =
                Toolkit.emptyDefinition
        in
        [ test "renders the value of a text field" <|
            \() ->
                start definition (Toolkit.viewTextEditable_ identity) "init"
                    |> TestContext.expectView
                        (Query.find [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                            >> Query.has [ attribute "content" "init" ]
                        )
        , test "can edit the value of a text field" <|
            \() ->
                start definition (Toolkit.viewTextEditable_ identity) "init"
                    |> fillInToolkitText
                        [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                        "updated"
                    |> TestContext.expectModel
                        (.editorData >> Expect.equal "updated")
        ]


type alias Model data =
    { toolkitState : ()
    , editorData : data
    }


type Msg data
    = Edit data


start :
    Toolkit.Definition path data
    -> (Toolkit.Context path data -> Html data)
    -> data
    -> TestContext (Msg data) (Model data) ()
start definition testView init =
    TestContext.create
        { init =
            ( { toolkitState = ()
              , editorData = init
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
                let
                    context =
                        Toolkit.makeContext
                            definition
                            model.toolkitState
                            model.editorData
                in
                testView context
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


attribute key value =
    Test.Html.Selector.attribute (Html.Attributes.attribute key value)
