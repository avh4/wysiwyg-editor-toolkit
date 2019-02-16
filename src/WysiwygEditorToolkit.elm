module WysiwygEditorToolkit exposing (viewTextEditable, viewTextStatic)

{-| WysiwygEditorToolkit gives you tools to create "what-you-see-is-what-you-get" (WYSIWYG) editors
for your UIs. Each view function in this module is part of a set of functions--a "static" version which simply renders the output, and an "editable" version which renders an editable field--all functions within the set are designed to render in the same visual style.


## Text

@docs viewTextEditable, viewTextStatic

-}

import Html exposing (Html)
import Html.Attributes exposing (contenteditable)
import Html.Events
import Html.Keyed
import Json.Decode


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


{-| This is the editable version of [`viewTextStatic`](#viewTextStatic).
-}
viewTextEditable : (String -> msg) -> String -> Html msg
viewTextEditable msg text =
    Html.Keyed.node "span"
        []
        [ ( "editable"
          , Html.span
                [ contenteditable True
                , onContentEdited msg
                ]
                [ Html.text text ]
          )
        ]


onContentEdited : (String -> msg) -> Html.Attribute msg
onContentEdited msg =
    Html.Events.on "input" (Json.Decode.at [ "target", "innerText" ] Json.Decode.string)
        |> Html.Attributes.map msg
