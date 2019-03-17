module WysiwygEditorToolkit exposing
    ( Definition
    , string, int, list
    , empty
    , OfTwo(..), OfThree(..), OfFive(..), object2, object3, object5
    , State, initState, focusState, Msg, Effect(..), update, mapMsg
    , focusCommentThread
    , EditAction, applyEditAction, mapAction
    , deleteAction
    , viewTextEditable, viewTextStatic
    , viewComments
    , Rendering, focusRendering
    , staticRendering, wysiwygRenderingWithComments
    )

{-| WysiwygEditorToolkit gives you tools to create "what-you-see-is-what-you-get" (WYSIWYG) editors
for your UIs. Each view function in this module is part of a set of functions--a "static" version which simply renders the output, and an "editable" version which renders an editable field--all functions within the set are designed to render in the same visual style.


## Definitions

@docs Definition


### Trivial definitions

@docs string, int, list


### Complex definitions

@docs empty
@docs OfTwo, OfThree, OfFive, object2, object3, object5


## State and updating

@docs State, initState, focusState, Msg, Effect, update, mapMsg
@docs focusCommentThread
@docs EditAction, applyEditAction, mapAction
@docs deleteAction


## Text

@docs viewTextEditable, viewTextStatic


## Comments

Comments are automatically displayed when using other editing widgets (like [`viewTextEditable`](#viewTextEditable)), but you can also add them manually for paths that do not correspond to other edigint widgets.

@docs viewComments


## Rendering

@docs Rendering, focusRendering
@docs staticRendering, wysiwygRenderingWithComments

-}

import Comments exposing (Comment)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (attribute, src, style)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Html.Keyed
import Json.Decode
import Time


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
        { applyEditAction : EditAction path -> data -> data
        , getString : path -> data -> Maybe String
        }


{-| The definition of an empty data structure
-}
empty : Definition path data
empty =
    Definition
        { applyEditAction = \_ value -> value
        , getString = \_ _ -> Nothing
        }


{-| The definition of a data structure with a single editable String value
-}
string : Definition () String
string =
    Definition
        { applyEditAction =
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
        { applyEditAction =
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
        { applyEditAction =
            \action items ->
                case action of
                    EditAction (Just ( index, Just p )) op ->
                        List.indexedMap
                            (\i item ->
                                if i == index then
                                    applyEditAction itemDef (EditAction p op) item

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
        { applyEditAction =
            \(EditAction path op) data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfTwo p1) ->
                        put1 (applyEditAction def1 (EditAction p1 op) (get1 data)) data

                    Just (TwoOfTwo p2) ->
                        put2 (applyEditAction def2 (EditAction p2 op) (get2 data)) data
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
        { applyEditAction =
            \(EditAction path op) data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfThree p1) ->
                        put1 (applyEditAction def1 (EditAction p1 op) (get1 data)) data

                    Just (TwoOfThree p2) ->
                        put2 (applyEditAction def2 (EditAction p2 op) (get2 data)) data

                    Just (ThreeOfThree p3) ->
                        put3 (applyEditAction def3 (EditAction p3 op) (get3 data)) data
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
        { applyEditAction =
            \(EditAction path op) data ->
                case deconstructPath path of
                    Nothing ->
                        data

                    Just (OneOfFive p1) ->
                        put1 (applyEditAction def1 (EditAction p1 op) (get1 data)) data

                    Just (TwoOfFive p2) ->
                        put2 (applyEditAction def2 (EditAction p2 op) (get2 data)) data

                    Just (ThreeOfFive p3) ->
                        put3 (applyEditAction def3 (EditAction p3 op) (get3 data)) data

                    Just (FourOfFive p4) ->
                        put4 (applyEditAction def4 (EditAction p4 op) (get4 data)) data

                    Just (FiveOfFive p5) ->
                        put5 (applyEditAction def5 (EditAction p5 op) (get5 data)) data
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
        { pathToString : path -> String
        , comments : Dict String (List Comment)
        , unsavedComments : Dict String String
        , focusedCommentThread : Maybe String
        , hoveredCommentThread : Maybe String
        }


{-| Creates an initial toolkit State. See [`State`](#State).
-}
initState : (path -> String) -> List ( path, Comment ) -> State path
initState pathToString comments =
    State
        { pathToString = pathToString
        , comments =
            comments
                |> List.map (Tuple.mapFirst pathToString)
                |> List.foldl (\( path, comment ) -> Dict.update path (\cs -> Just (Maybe.withDefault [] cs ++ [ comment ]))) Dict.empty
        , unsavedComments = Dict.empty
        , focusedCommentThread = Nothing
        , hoveredCommentThread = Nothing
        }


{-| Creates a State that focuses on a substructure of the given State.
-}
focusState : (path1 -> path) -> State path -> State path1
focusState mapPath (State state) =
    State
        { pathToString = mapPath >> state.pathToString
        , comments = state.comments
        , unsavedComments = state.unsavedComments
        , focusedCommentThread = state.focusedCommentThread
        , hoveredCommentThread = state.hoveredCommentThread
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


{-| Msgs that can affect the toolkit's `State`, which can be processes with [`update`](#update).
-}
type Msg path
    = EditActionMsg (EditAction path)
    | CommentsMsg path CommentsMsg
    | CreateCommentResponse path (Result () Comment)
    | FocusComment path
    | HoverCommentThread (Maybe path)


{-| Gives UI focus to the comment thread at the given path.
Notably, this will make the comment UI appear for a path for which no comments currently exist.
-}
focusCommentThread : path -> Msg path
focusCommentThread path =
    FocusComment path


{-| Transform a `Msg` to operate on a larger data structure.
-}
mapMsg : (path1 -> path) -> Msg path1 -> Msg path
mapMsg f msg =
    case msg of
        EditActionMsg action ->
            EditActionMsg (mapAction f action)

        CommentsMsg path m ->
            CommentsMsg (f path) m

        CreateCommentResponse path result ->
            CreateCommentResponse (f path) result

        FocusComment path ->
            FocusComment (f path)

        HoverCommentThread path ->
            HoverCommentThread (Maybe.map f path)


{-| Apply an `EditAction` to a data structure.
-}
applyEditAction : Definition path data -> EditAction path -> data -> data
applyEditAction (Definition definition) action data =
    definition.applyEditAction action data


{-| Effects that [`update`](#update) can ask you to perform on the toolkit's behalf.
-}
type Effect path
    = CreateComment path String (Result () Comment -> Msg path)


{-| Apply a [`Msg`](#Msg) to a data structure and a [`State`](#State).
-}
update : Definition path data -> Msg path -> State path -> data -> ( data, State path, Maybe (Effect path) )
update definition msg (State state) data =
    case msg of
        EditActionMsg editAction ->
            ( applyEditAction definition editAction data
            , State state
            , Nothing
            )

        CommentsMsg path (DraftCommentChanged text) ->
            ( data
            , State
                { state
                    | unsavedComments = Dict.insert (state.pathToString path) text state.unsavedComments
                }
            , Nothing
            )

        CommentsMsg path SubmitDraftComment ->
            let
                newComment =
                    Dict.get (state.pathToString path) state.unsavedComments
                        |> Maybe.withDefault ""
            in
            if String.trim newComment == "" then
                -- Don't save blank comments
                ( data, State state, Nothing )

            else
                ( data
                , State state
                  -- TODO: mark the comment as saving
                , Just (CreateComment path newComment (CreateCommentResponse path))
                )

        CreateCommentResponse path (Ok newComment) ->
            ( data
            , State
                { state
                    | unsavedComments = Dict.remove (state.pathToString path) state.unsavedComments
                    , comments =
                        Dict.update (state.pathToString path)
                            (\cs ->
                                Just
                                    (Maybe.withDefault [] cs
                                        ++ [ newComment ]
                                    )
                            )
                            state.comments
                }
            , Nothing
            )

        CreateCommentResponse path (Err ()) ->
            -- TODO: note the error and put back the unsaved comment
            ( data
            , State state
            , Nothing
            )

        FocusComment path ->
            ( data
            , State { state | focusedCommentThread = Just (state.pathToString path) }
            , Nothing
            )

        HoverCommentThread path ->
            ( data
            , State { state | hoveredCommentThread = Maybe.map state.pathToString path }
            , Nothing
            )


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
            Html.text text


{-| This is the editable version of [`viewTextStatic`](#viewTextStatic).
-}
viewTextEditable : Definition path data -> State path -> path -> data -> Html (Msg path)
viewTextEditable definition (State state) path data =
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
            let
                pathString =
                    state.pathToString path

                isFocused =
                    state.focusedCommentThread == Just pathString

                isHovered =
                    state.hoveredCommentThread == Just pathString

                hasComments =
                    Dict.get pathString state.comments
                        |> Maybe.withDefault []
                        |> (/=) []
            in
            Html.Keyed.node "div"
                [ style "position" "relative"
                , style "display" "inline-block"
                , if hasComments then
                    style "box-shadow" "yellow 0px -4px 2px -2px inset"

                  else
                    style "" ""
                , if isFocused || (hasComments && isHovered) then
                    style "background-color" "yellow"

                  else
                    style "" ""
                , onMouseEnter (HoverCommentThread (Just path))

                -- TODO: we need to track the hover sources independently so there isn't a race condition on removing the current hover
                , onMouseLeave (HoverCommentThread Nothing)
                ]
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
                        |> Html.map (Edit >> EditAction path >> EditActionMsg)
                  )
                , ( "comments"
                  , viewComments (State state) path
                  )
                ]


type CommentsMsg
    = DraftCommentChanged String
    | SubmitDraftComment


{-| Displays the comments thread (and comments editor) for the given path.

(Note that other views like [`viewTextEditable`](#viewTextEditable) include
this automatically, so you only need to use this for paths in your data that
don't have a more specific editor view.)

-}
viewComments : State path -> path -> Html (Msg path)
viewComments (State state) path =
    let
        pathString =
            state.pathToString path

        comments =
            Dict.get pathString state.comments
                |> Maybe.withDefault []

        unsavedComment =
            Dict.get pathString state.unsavedComments
                |> Maybe.withDefault ""

        isFocused =
            state.focusedCommentThread == Just pathString

        isHovered =
            state.hoveredCommentThread == Just pathString
    in
    if isFocused then
        Html.aside
            [ style "position" "absolute"
            , style "font-size" "10px"
            , style "background" "pink"
            , style "top" "0px"
            , style "left" "100%"
            , style "width" "250px"
            , style "border-radius" "5px"
            , style "border" "2px solid palevioletred"
            , style "opacity" "0.9"
            , style "z-index" "102"
            ]
        <|
            List.concat
                [ comments
                    |> List.map viewComment
                , [ Html.div
                        [ style "border-top" "2px solid palevioletred"
                        , style "padding" "10px"
                        ]
                        [ Html.textarea
                            [ style "border" "none"
                            , style "background" "transparent"
                            , style "width" "100%"
                            , onInput DraftCommentChanged
                            ]
                            [ Html.text unsavedComment
                            ]
                        , Html.button
                            [ onClick SubmitDraftComment
                            ]
                            [ Html.text "Send"
                            ]
                        ]
                        |> Html.map (CommentsMsg path)
                  ]
                ]

    else if List.isEmpty comments && String.trim unsavedComment == "" then
        Html.text ""

    else
        Html.aside
            [ style "position" "absolute"
            , style "font-size" "10px"
            , style "background" <|
                if isHovered then
                    "pink"

                else
                    "transparent"
            , style "top" "0px"
            , style "left" "100%"
            , style "width" "250px"
            , style "border-radius" "5px"
            , style "border" <|
                if isHovered then
                    "2px solid palevioletred"

                else
                    "2px solid transparent"
            , style "opacity" "0.7"
            , style "z-index" <|
                if isHovered then
                    "101"

                else
                    "100"
            , style "cursor" "pointer"
            , onClick (FocusComment path)
            , onMouseEnter (HoverCommentThread (Just path))
            , onMouseLeave (HoverCommentThread Nothing)
            ]
        <|
            List.concat
                [ case comments of
                    [] ->
                        []

                    [ single ] ->
                        [ viewComment single ]

                    [ first, second ] ->
                        [ viewComment first
                        , viewComment second
                        ]

                    first :: _ ->
                        [ viewComment first
                        , Html.text ("... " ++ String.fromInt (List.length comments - 2) ++ " comments ...")
                        , comments
                            |> List.reverse
                            |> List.head
                            |> Maybe.map viewComment
                            |> Maybe.withDefault (Html.text "")
                        ]
                , [ Html.div
                        [ style "border-top" "2px solid transparent"
                        , style "padding" "10px"
                        ]
                        [ Html.text unsavedComment
                        ]
                        |> Html.map (CommentsMsg path)
                  ]
                ]


viewComment : Comment -> Html msg
viewComment comment =
    Html.div
        [ style "display" "grid"
        , style "grid-template-columns" "25px 1fr 1fr"
        , style "grid-template-rows" "1fr 1fr"
        , style "grid-gap" "5px"
        , style "padding" "10px"
        ]
        [ Html.img
            [ src comment.author.avatar
            , style "width" "100%"
            , style "grid-area" "1/1/-1"
            ]
            []
        , Html.div
            [ style "grid-area" "1/2"
            ]
            [ Html.text comment.author.name ]
        , Html.div
            [ style "grid-area" "1/3"
            ]
            [ Html.text "1 day ago" -- TODO: use createdAt
            ]
        , Html.div
            [ style "grid-area" "2/2/-1/-1"
            ]
            [ Html.text comment.content ]
        ]


{-| A `Rendering` represents the configuration of how to render data.

If you write a view that takes a `Rendering` as input, then the view can be used both for
production, user-facing pages (by giving it a [`staticRendering`](#staticRendering)),
and for content editors (by giving it a [`wysiwygRenderingWithComments`](#wysiwygRenderingWithComments)).

-}
type alias Rendering path msg =
    { textBlock : path -> (List (Html msg) -> Html msg) -> Html msg
    , text : path -> Html msg
    , comments : path -> Html msg
    , withDelete : path -> (List (Html msg) -> Html msg) -> List (Html msg) -> Html msg
    }


{-| This is the simplest [`Rendering`](#Rendering), and it simply renders data as plain HTML with
no interactive editing or commenting controls.
You can use this in your production user-facing pages.
-}
staticRendering : Definition path data -> data -> Rendering path msg
staticRendering definition data =
    { textBlock = \path container -> container [ viewTextStatic definition path data ]
    , text = \path -> viewTextStatic definition path data
    , comments = \_ -> Html.text ""
    , withDelete = \_ container children -> container children
    }


{-| -}
wysiwygRenderingWithComments : (Msg path -> msg) -> Definition path data -> State path -> data -> Rendering path msg
wysiwygRenderingWithComments toMsg definition state data =
    { textBlock = \path container -> container [ Html.map toMsg (viewTextEditable definition state path data) ]
    , text = \path -> Html.map toMsg (viewTextEditable definition state path data)
    , comments = \path -> Html.map toMsg (viewComments state path)
    , withDelete =
        \path container children ->
            let
                deleteButton =
                    Html.button
                        [ style "position" "absolute"
                        , style "flex-grow" "1"
                        , style "top" "5px"
                        , style "right" "10px"
                        , style "background-color" "pink"
                        , style "opacity" "0.5"
                        , style "padding" "5px 15px"
                        , style "border-radius" "5px"
                        , onClick (toMsg <| EditActionMsg <| deleteAction path)
                        ]
                        [ Html.text "Delete" ]
            in
            container (children ++ [ deleteButton ])
    }


{-| Transform a `Rendering` that operates on a data structure into a `Rendering` that operates on a substructure of that data structure.
-}
focusRendering : (path1 -> path) -> Rendering path msg -> Rendering path1 msg
focusRendering f render =
    { textBlock = \p1 -> render.textBlock (f p1)
    , text = \p1 -> render.text (f p1)
    , comments = \p1 -> render.comments (f p1)
    , withDelete = \p1 -> render.withDelete (f p1)
    }
