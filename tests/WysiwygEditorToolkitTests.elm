module WysiwygEditorToolkitTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, tag)
import TestContext exposing (TestContext)
import WysiwygEditorToolkit as Toolkit exposing (OfTwo(..))


all : Test
all =
    describe "WysiwygEditorToolkit"
        [ describe "an editor with one field" <|
            let
                definition =
                    Toolkit.string

                state =
                    Toolkit.initState Debug.toString []

                view d =
                    Toolkit.viewTextEditable d state ()
            in
            [ test "renders the value of a text field" <|
                \() ->
                    start definition view "init"
                        |> TestContext.expectView
                            (Query.find [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                                >> Query.has [ attribute "content" "init" ]
                            )
            , test "can edit the value of a text field" <|
                \() ->
                    start definition view "init"
                        |> fillInToolkitText
                            [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                            "updated"
                        |> TestContext.expectModel
                            (.editorData >> Expect.equal "updated")
            ]
        , describe "an editor with two fields" <|
            let
                definition =
                    Toolkit.object2
                        (\p ->
                            case p of
                                Title ->
                                    Just (OneOfTwo ())

                                Description ->
                                    Just (TwoOfTwo ())
                        )
                        ( .title, \x data -> { data | title = x }, Toolkit.string )
                        ( .description, \x data -> { data | description = x }, Toolkit.string )

                state =
                    Toolkit.initState Debug.toString []

                view d =
                    Toolkit.viewTextEditable d state Title
            in
            [ test "renders the value of a text field" <|
                \() ->
                    start definition view { title = "t1", description = "d1" }
                        |> TestContext.expectView
                            (Query.find [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                                >> Query.has [ attribute "content" "t1" ]
                            )
            , test "can edit the value of a text field" <|
                \() ->
                    start definition view { title = "t1", description = "d1" }
                        |> fillInToolkitText
                            [ tag "avh4-wysiwyg-editor-toolkit-text" ]
                            "updated"
                        |> TestContext.expectModel
                            (.editorData >> .title >> Expect.equal "updated")
            ]
        ]


type Path
    = Title
    | Description


type alias Model path data =
    { toolkitState : Toolkit.State path
    , editorData : data
    }


type Msg path
    = ToolkitMsg (Toolkit.Msg path)


start :
    Toolkit.Definition path data
    -> (Toolkit.Definition path data -> data -> Html (Toolkit.Msg path))
    -> data
    -> TestContext (Msg path) (Model path data) ()
start definition testView init =
    TestContext.create
        { init =
            ( { toolkitState = Toolkit.initState Debug.toString []
              , editorData = init
              }
            , ()
            )
        , update =
            \msg model ->
                case msg of
                    ToolkitMsg toolkitMsg ->
                        let
                            ( newData, newState ) =
                                Toolkit.update definition toolkitMsg model.toolkitState model.editorData
                        in
                        ( { model
                            | editorData = newData
                            , toolkitState = newState
                          }
                        , ()
                        )
        , view =
            \model ->
                testView definition model.editorData
                    |> Html.map ToolkitMsg
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
