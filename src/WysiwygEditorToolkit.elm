module WysiwygEditorToolkit exposing
    ( viewTextEditable, viewTextStatic
    , Context, Definition, emptyDefinition, makeContext, viewTextEditable_
    )

{-| WysiwygEditorToolkit gives you tools to create "what-you-see-is-what-you-get" (WYSIWYG) editors
for your UIs. Each view function in this module is part of a set of functions--a "static" version which simply renders the output, and an "editable" version which renders an editable field--all functions within the set are designed to render in the same visual style.


## Text

@docs viewTextEditable, viewTextStatic

-}

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Html.Events
import Html.Keyed
import Json.Decode


type Definition path data
    = Definition


emptyDefinition =
    Definition


type Context path data
    = Context
        { data : data
        }


type alias State =
    ()


makeContext : Definition path data -> State -> data -> Context path data
makeContext definition state data =
    Context
        { data = data
        }


{-| This is the static version of [`viewTextEditable`](#viewTextEditable).
-}
viewTextStatic : String -> Html msg
viewTextStatic text =
    -- NOTE: the keyed span is here to work around an elm/virtual-dom bug which causes a crash when it tries to remove contenteditable from a node that already has it set -- so we use a keyed node to ensure that we create a new DOM node instead of reusing an existing contenteditable DOM node
    Html.Keyed.node "span"
        []
        [ ( "static"
          , Html.span [] [ Html.text text ]
          )
        ]


viewTextEditable_ : (String -> msg) -> Context () String -> Html msg
viewTextEditable_ msg (Context context) =
    viewTextEditable msg context.data


{-| This is the editable version of [`viewTextStatic`](#viewTextStatic).
-}
viewTextEditable : (String -> msg) -> String -> Html msg
viewTextEditable msg text =
    Html.Keyed.node "span"
        []
        [ ( "editable"
          , Html.node "avh4-wysiwyg-editor-toolkit-text"
                [ attribute "content" text
                , Html.Events.on "content-changed"
                    (Json.Decode.at [ "detail", "textContent" ] Json.Decode.string)
                    |> Html.Attributes.map msg
                ]
                [ -- The content here is just a fallback for when the custom element isn't available.
                  -- When the custom element is available, the content here will be hidden by the custom element's shadow DOM.
                  -- TODO: add some kind of warning when this happens to point to how to load the custom element
                  Html.span
                    [ Html.Attributes.contenteditable True
                    , Html.Events.on "input"
                        (Json.Decode.at [ "target", "textContent" ] Json.Decode.string)
                        |> Html.Attributes.map msg
                    ]
                    [ Html.text text ]
                ]
          )
        ]
