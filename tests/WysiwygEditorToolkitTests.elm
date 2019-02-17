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

                view =
                    Toolkit.viewTextEditable identity ()
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
                        ( \f data -> { data | title = f data.title }, Toolkit.string )
                        ( \f data -> { data | description = f data.description }, Toolkit.string )

                view context =
                    Toolkit.viewTextEditable .title Title context
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


type alias Model data =
    { toolkitState : ()
    , editorData : data
    }


type Msg path
    = Edit path String


start :
    Toolkit.Definition path data
    -> (Toolkit.Context path data -> Html ( path, String ))
    -> data
    -> TestContext (Msg path) (Model data) ()
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
                    Edit path text ->
                        ( { model
                            | editorData = Toolkit.applyEdit definition path text model.editorData
                          }
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
                    |> Html.map (\( p, d ) -> Edit p d)
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
