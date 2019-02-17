module WysiwygEditorToolkit exposing
    ( Definition
    , string, int, list
    , OfTwo(..), OfThree(..), OfFive(..), object2, object3, object5
    , State, initState
    , applyEdit
    , Context, makeContext
    , viewTextEditable, viewTextStatic
    )

{-| WysiwygEditorToolkit gives you tools to create "what-you-see-is-what-you-get" (WYSIWYG) editors
for your UIs. Each view function in this module is part of a set of functions--a "static" version which simply renders the output, and an "editable" version which renders an editable field--all functions within the set are designed to render in the same visual style.


## Definitions

@docs Definition


### Trivial definitions

@docs string, int, list


### Complex definitions

@docs OfTwo, OfThree, OfFive, object2, object3, object5


## State and updating

@docs State, initState
@docs applyEdit


## Contexts

You will create a `Context` in your view function by combining your static `Definition`,
the opaque toolkit `State`, and your application's `data`.

@docs Context, makeContext


## Text

@docs viewTextEditable, viewTextStatic

-}

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Html.Events
import Html.Keyed
import Json.Decode


{-| A `Definition` represents a set of editing operations that can be applied to
a `data` value, where individual fields can be distinguished with `path` values.

For example, editing operations on a `List String` could be represented by
a `Definition Int (List String)` -- the path is an `Int` which indicates which item in
the list an edit corresponds to.

As further examples, the `path` type for a `Dict key value` would `key`.
The `path` type for a `{ enabled : Bool, name: String}` would be a union type
`type Path = Enabled | Name`.
The `path` for a single value would simply be `()`.

-}
type Definition path data
    = Definition
        { applyEdit : path -> String -> data -> data
        , getString : path -> data -> Maybe String
        }


{-| The definition of a data structure with a single editable String value
-}
string : Definition () String
string =
    Definition
        { applyEdit = \() text _ -> text
        , getString = \() text -> Just text
        }


{-| The definition of a data structure with a single editable Int value
-}
int : Definition () Int
int =
    Definition
        { applyEdit =
            \() text old ->
                String.toInt text
                    |> Maybe.withDefault old
        , getString =
            \() value -> Just (String.fromInt value)
        }


{-| The definition of a data structure with an editable list of editable data structures.
-}
list : Definition path data -> Definition ( Int, path ) (List data)
list item =
    Definition
        { applyEdit = \_ _ d -> d -- TODO
        , getString =
            \( i, p ) items ->
                List.drop i items
                    |> List.head
                    |> Maybe.andThen (getString item p)
        }


{-| A value representing one of two possiblities. (This is essentially equivalent to a `Result a b`, but here it is not implied that one of the possibilities is an "error" state.)
-}
type OfTwo a b
    = OneOfTwo a
    | TwoOfTwo b


{-| The definition of a data structure containing two editable data structures distinguishable by a `path` value.
-}
object2 :
    (path -> Maybe (OfTwo path1 path2))
    -> ( data -> data1, data1 -> data -> data, Definition path1 data1 )
    -> ( data -> data2, data2 -> data -> data, Definition path2 data2 )
    -> Definition path data
object2 deconstructPath ( get1, put1, def1 ) ( get2, put2, def2 ) =
    Definition
        { applyEdit =
            \path text data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfTwo p1) ->
                        put1 (applyEdit def1 p1 text (get1 data)) data

                    Just (TwoOfTwo p2) ->
                        put2 (applyEdit def2 p2 text (get2 data)) data
        , getString =
            \path data ->
                case deconstructPath path of
                    Nothing ->
                        Nothing

                    Just (OneOfTwo p1) ->
                        getString def1 p1 (get1 data)

                    Just (TwoOfTwo p2) ->
                        getString def2 p2 (get2 data)
        }


{-| A value representing one of three possibilities.
-}
type OfThree a b c
    = OneOfThree a
    | TwoOfThree b
    | ThreeOfThree c


{-| The definition of a data structure containing three editable data structures distinguishable by a `path` value.
-}
object3 :
    (path -> Maybe (OfThree path1 path2 path3))
    -> ( data -> data1, data1 -> data -> data, Definition path1 data1 )
    -> ( data -> data2, data2 -> data -> data, Definition path2 data2 )
    -> ( data -> data3, data3 -> data -> data, Definition path3 data3 )
    -> Definition path data
object3 deconstructPath ( get1, put1, def1 ) ( get2, put2, def2 ) ( get3, put3, def3 ) =
    Definition
        { applyEdit =
            \path text data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfThree p1) ->
                        put1 (applyEdit def1 p1 text (get1 data)) data

                    Just (TwoOfThree p2) ->
                        put2 (applyEdit def2 p2 text (get2 data)) data

                    Just (ThreeOfThree p3) ->
                        put3 (applyEdit def3 p3 text (get3 data)) data
        , getString =
            \path data ->
                case deconstructPath path of
                    Nothing ->
                        Nothing

                    Just (OneOfThree p1) ->
                        getString def1 p1 (get1 data)

                    Just (TwoOfThree p2) ->
                        getString def2 p2 (get2 data)

                    Just (ThreeOfThree p3) ->
                        getString def3 p3 (get3 data)
        }


{-| A value representing one of five possibilities.
-}
type OfFive a b c d e
    = OneOfFive a
    | TwoOfFive b
    | ThreeOfFive c
    | FourOfFive d
    | FiveOfFive e


{-| The definition of a data structure containing five editable data structures distinguishable by a `path` value.
-}
object5 :
    (path -> Maybe (OfFive path1 path2 path3 path4 path5))
    -> ( data -> data1, data1 -> data -> data, Definition path1 data1 )
    -> ( data -> data2, data2 -> data -> data, Definition path2 data2 )
    -> ( data -> data3, data3 -> data -> data, Definition path3 data3 )
    -> ( data -> data4, data4 -> data -> data, Definition path4 data4 )
    -> ( data -> data5, data5 -> data -> data, Definition path5 data5 )
    -> Definition path data
object5 deconstructPath ( get1, put1, def1 ) ( get2, put2, def2 ) ( get3, put3, def3 ) ( get4, put4, def4 ) ( get5, put5, def5 ) =
    Definition
        { applyEdit =
            \path text data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfFive p1) ->
                        put1 (applyEdit def1 p1 text (get1 data)) data

                    Just (TwoOfFive p2) ->
                        put2 (applyEdit def2 p2 text (get2 data)) data

                    Just (ThreeOfFive p3) ->
                        put3 (applyEdit def3 p3 text (get3 data)) data

                    Just (FourOfFive p4) ->
                        put4 (applyEdit def4 p4 text (get4 data)) data

                    Just (FiveOfFive p5) ->
                        put5 (applyEdit def5 p5 text (get5 data)) data
        , getString =
            \path data ->
                case deconstructPath path of
                    Nothing ->
                        Nothing

                    Just (OneOfFive p1) ->
                        getString def1 p1 (get1 data)

                    Just (TwoOfFive p2) ->
                        getString def2 p2 (get2 data)

                    Just (ThreeOfFive p3) ->
                        getString def3 p3 (get3 data)

                    Just (FourOfFive p4) ->
                        getString def4 p4 (get4 data)

                    Just (FiveOfFive p5) ->
                        getString def5 p5 (get5 data)
        }


{-| Private UI state for the editor toolkit.
You should create this with `initState` when your program starts, and store it in
your applications Model.

You will use this with `makeContext` in your view function,
and will update it with `applyEdit` in your update function.

-}
type alias State =
    ()


{-| Creates an initial toolkit State. See [`State`](#State).
-}
initState : State
initState =
    ()


{-| See [`makeContext`](#makeContext).
-}
type Context path data
    = Context
        { definition : Definition path data
        , data : data
        }


{-| You will create a `Context` in your view function by combining your static `Definition`,
the opaque toolkit `State`, and your application's `data`.
The `Context` is then passed to other functions in this module to render the UI widgets
that render and/or edit the `data`.
-}
makeContext : Definition path data -> State -> data -> Context path data
makeContext definition state data =
    Context
        { definition = definition
        , data = data
        }


{-| TODO: generalize this into an `update`
-}
applyEdit : Definition path data -> path -> String -> data -> data
applyEdit (Definition definition) path text data =
    definition.applyEdit path text data


getString : Definition path data -> path -> data -> Maybe String
getString (Definition definition) path data =
    definition.getString path data


{-| This is the static version of [`viewTextEditable`](#viewTextEditable).
-}
viewTextStatic : path -> Context path data -> Html msg
viewTextStatic path (Context context) =
    case getString context.definition path context.data of
        Nothing ->
            Html.text <|
                String.concat
                    [ "[The given path cannot render a text value: path = "
                    , Debug.toString path
                    , "  data = "
                    , Debug.toString context.data
                    , "]"
                    ]

        Just text ->
            -- NOTE: the keyed span is here to work around an elm/virtual-dom bug which causes a crash when it tries to remove contenteditable from a node that already has it set -- so we use a keyed node to ensure that we create a new DOM node instead of reusing an existing contenteditable DOM node
            Html.Keyed.node "span"
                []
                [ ( "static"
                  , Html.span [] [ Html.text text ]
                  )
                ]


{-| This is the editable version of [`viewTextStatic`](#viewTextStatic).
-}
viewTextEditable : path -> Context path data -> Html ( path, String )
viewTextEditable path (Context context) =
    case getString context.definition path context.data of
        Nothing ->
            Html.text <|
                String.concat
                    [ "[The given path cannot render a text value: path = "
                    , Debug.toString path
                    , "  data = "
                    , Debug.toString context.data
                    , "]"
                    ]

        Just text ->
            Html.Keyed.node "span"
                []
                [ ( "editable"
                  , Html.node "avh4-wysiwyg-editor-toolkit-text"
                        [ attribute "content" text
                        , Html.Events.on "content-changed"
                            (Json.Decode.at [ "detail", "textContent" ] Json.Decode.string)
                        ]
                        [ -- The content here is just a fallback for when the custom element isn't available.
                          -- When the custom element is available, the content here will be hidden by the custom element's shadow DOM.
                          -- TODO: add some kind of warning when this happens to point to how to load the custom element
                          Html.span
                            [ Html.Attributes.contenteditable True
                            , Html.Events.on "input"
                                (Json.Decode.at [ "target", "textContent" ] Json.Decode.string)
                            ]
                            [ Html.text text ]
                        ]
                  )
                ]
                |> Html.map (Tuple.pair path)
