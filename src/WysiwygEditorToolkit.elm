module WysiwygEditorToolkit exposing
    ( Definition
    , string, int, list
    , OfTwo(..), OfThree(..), OfFive(..), object2, object3, object5
    , State, initState
    , EditAction, update, mapAction
    , deleteAction
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
@docs EditAction, update, mapAction
@docs deleteAction


## Text

@docs viewTextEditable, viewTextStatic

-}

import Comments exposing (Comment)
import Dict exposing (Dict)
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
        { update : EditAction path -> data -> data
        , getString : path -> data -> Maybe String
        }


{-| The definition of a data structure with a single editable String value
-}
string : Definition () String
string =
    Definition
        { update =
            \(EditAction () op) value ->
                case op of
                    Edit text ->
                        text

                    Delete ->
                        value
        , getString = \() text -> Just text
        }


{-| The definition of a data structure with a single editable Int value
-}
int : Definition () Int
int =
    Definition
        { update =
            \(EditAction () op) old ->
                case op of
                    Edit text ->
                        String.toInt text
                            |> Maybe.withDefault old

                    Delete ->
                        old
        , getString =
            \() value -> Just (String.fromInt value)
        }


{-| The definition of a data structure with an editable list of editable data structures.
-}
list : Definition path data -> Definition (Maybe ( Int, Maybe path )) (List data)
list itemDef =
    Definition
        { update =
            \action items ->
                case action of
                    EditAction (Just ( index, Just p )) op ->
                        List.indexedMap
                            (\i item ->
                                if i == index then
                                    update itemDef (EditAction p op) item

                                else
                                    item
                            )
                            items

                    EditAction _ (Edit _) ->
                        items

                    EditAction (Just ( index, Nothing )) Delete ->
                        List.indexedMap
                            (\i item ->
                                if i == index then
                                    Nothing

                                else
                                    Just item
                            )
                            items
                            |> List.filterMap identity

                    EditAction _ Delete ->
                        items
        , getString =
            \mp items ->
                case mp of
                    Nothing ->
                        Nothing

                    Just ( _, Nothing ) ->
                        Nothing

                    Just ( i, Just p ) ->
                        List.drop i items
                            |> List.head
                            |> Maybe.andThen (getString itemDef p)
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
        { update =
            \(EditAction path op) data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfTwo p1) ->
                        put1 (update def1 (EditAction p1 op) (get1 data)) data

                    Just (TwoOfTwo p2) ->
                        put2 (update def2 (EditAction p2 op) (get2 data)) data
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
        { update =
            \(EditAction path op) data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfThree p1) ->
                        put1 (update def1 (EditAction p1 op) (get1 data)) data

                    Just (TwoOfThree p2) ->
                        put2 (update def2 (EditAction p2 op) (get2 data)) data

                    Just (ThreeOfThree p3) ->
                        put3 (update def3 (EditAction p3 op) (get3 data)) data
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
        { update =
            \(EditAction path op) data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfFive p1) ->
                        put1 (update def1 (EditAction p1 op) (get1 data)) data

                    Just (TwoOfFive p2) ->
                        put2 (update def2 (EditAction p2 op) (get2 data)) data

                    Just (ThreeOfFive p3) ->
                        put3 (update def3 (EditAction p3 op) (get3 data)) data

                    Just (FourOfFive p4) ->
                        put4 (update def4 (EditAction p4 op) (get4 data)) data

                    Just (FiveOfFive p5) ->
                        put5 (update def5 (EditAction p5 op) (get5 data)) data
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
-}
type State path
    = State
        { comments : Dict String (List Comment)
        }


{-| Creates an initial toolkit State. See [`State`](#State).
-}
initState : (path -> String) -> List ( path, List Comment ) -> State path
initState pathToString comments =
    State
        { comments =
            comments
                |> List.map (Tuple.mapFirst pathToString)
                |> Dict.fromList
        }


{-| Represents an edit to be performed on a data structure that can be navigated with the given `path` type.

Use [`update`](#update) along with your data structure's [`Definition`](#Definition) to apply the edit.

-}
type EditAction path
    = EditAction path Operation


type Operation
    = Edit String
    | Delete


{-| TODO: do we need to expose this?
-}
deleteAction : path -> EditAction path
deleteAction path =
    EditAction path Delete


{-| Transform an `EditAction` to operate on a larger data structure.
-}
mapAction : (path1 -> path) -> EditAction path1 -> EditAction path
mapAction f (EditAction path op) =
    EditAction (f path) op


{-| Apply an `EditAction` to a data structure.
-}
update : Definition path data -> EditAction path -> data -> data
update (Definition definition) action data =
    definition.update action data


getString : Definition path data -> path -> data -> Maybe String
getString (Definition definition) path data =
    definition.getString path data


{-| This is the static version of [`viewTextEditable`](#viewTextEditable).
-}
viewTextStatic : Definition path data -> path -> data -> Html msg
viewTextStatic definition path data =
    case getString definition path data of
        Nothing ->
            Html.text <|
                String.concat
                    [ "[The given path cannot render a text value: path = "
                    , Debug.toString path
                    , "  data = "
                    , Debug.toString data
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
viewTextEditable : Definition path data -> path -> data -> Html (EditAction path)
viewTextEditable definition path data =
    case getString definition path data of
        Nothing ->
            Html.text <|
                String.concat
                    [ "[The given path cannot render a text value: path = "
                    , Debug.toString path
                    , "  data = "
                    , Debug.toString data
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
                |> Html.map (Edit >> EditAction path)
