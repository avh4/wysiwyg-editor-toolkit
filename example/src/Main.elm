module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, h4, h5, img, li, p, small, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, rel, src, style, type_, value)
import Html.Events exposing (onClick)
import WysiwygEditorToolkit as Toolkit exposing (OfThree(..), OfTwo(..))


type alias Model =
    { editorData : PricingSummary
    , renderingMode : RenderingMode
    }


type RenderingMode
    = Static
    | Editable


initialModel : Model
initialModel =
    { editorData = pricingSummary
    , renderingMode = Editable
    }


type Msg
    = SetRenderingMode RenderingMode
    | ToolkitAction (Toolkit.EditAction PricingSummaryPath)
    | Add PricingSummaryPath
    | Delete PricingSummaryPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRenderingMode renderingMode ->
            ( { model | renderingMode = renderingMode }
            , Cmd.none
            )

        ToolkitAction action ->
            ( { model | editorData = Toolkit.update pricingSummaryDefinition action model.editorData }
            , Cmd.none
            )

        Add path ->
            ( { model | editorData = applyAdd path model.editorData }
            , Cmd.none
            )

        Delete path ->
            ( { model | editorData = applyDelete path model.editorData }
            , Cmd.none
            )


applyAdd : PricingSummaryPath -> PricingSummary -> PricingSummary
applyAdd path data =
    case path of
        Plans Nothing ->
            { data
                | plans =
                    data.plans
                        ++ [ { name = "New plan"
                             , callToAction = "Contact us"
                             , callToActionOutline = False
                             , features = [ "New features" ]
                             , pricePerMonth = { usd = 99 }
                             }
                           ]
            }

        _ ->
            data


applyDelete : PricingSummaryPath -> PricingSummary -> PricingSummary
applyDelete path data =
    case path of
        Plans (Just ( index, Nothing )) ->
            { data
                | plans =
                    data.plans
                        |> List.indexedMap
                            (\i plan ->
                                if i == index then
                                    Nothing

                                else
                                    Just plan
                            )
                        |> List.filterMap identity
            }

        _ ->
            data


view : Model -> Browser.Document Msg
view model =
    { title = "avh4/wysiwyg-editor-toolkit demo"
    , body =
        [ Html.h2 [] [ Html.text "avh4/wysiwyg-editor-toolkit demo" ]
        , Html.node "style" [] [ Html.text ".pricing-header { max-width: 700px }" ]
        , case model.renderingMode of
            Static ->
                Html.div []
                    [ Html.button [ onClick (SetRenderingMode Editable) ] [ Html.text "Editable" ]
                    ]

            Editable ->
                Html.div []
                    [ Html.button [ onClick (SetRenderingMode Static) ] [ Html.text "Static" ]
                    ]
        , Html.p [] [ Html.text "(example is taken from https://getbootstrap.com/docs/4.3/examples/pricing/)" ]
        , Html.hr [] []
        , pricingSummaryView model.renderingMode model.editorData
        , Html.hr [] []
        , Html.code [] [ Html.text (Debug.toString model.editorData) ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }


type alias PricingSummary =
    { title : String
    , intro : String
    , plans : List PricingPlan
    }


type PricingSummaryPath
    = Title
    | Intro
    | Plans (Maybe ( Int, Maybe PricingPlanPath ))


pricingSummaryDefinition : Toolkit.Definition PricingSummaryPath PricingSummary
pricingSummaryDefinition =
    Toolkit.object3
        (\path ->
            case path of
                Title ->
                    Just (OneOfThree ())

                Intro ->
                    Just (TwoOfThree ())

                Plans p ->
                    Just (ThreeOfThree p)
        )
        ( .title, \x data -> { data | title = x }, Toolkit.string )
        ( .intro, \x data -> { data | intro = x }, Toolkit.string )
        ( .plans, \x data -> { data | plans = x }, Toolkit.list pricingPlanDefinition )


type alias PricingPlan =
    { name : String
    , pricePerMonth :
        { usd : Int
        }
    , features : List String
    , callToAction : String
    , callToActionOutline : Bool
    }


type PricingPlanPath
    = Name
    | PriceUsd
    | Features (Maybe Int)


pricingPlanDefinition : Toolkit.Definition PricingPlanPath PricingPlan
pricingPlanDefinition =
    Toolkit.object3
        (\path ->
            case path of
                Name ->
                    Just (OneOfThree ())

                PriceUsd ->
                    Just (TwoOfThree ())

                Features Nothing ->
                    Just (ThreeOfThree Nothing)

                Features (Just i) ->
                    Just (ThreeOfThree (Just ( i, Just () )))
        )
        ( .name, \x plan -> { plan | name = x }, Toolkit.string )
        ( .pricePerMonth >> .usd, \x plan -> { plan | pricePerMonth = { usd = x } }, Toolkit.int )
        ( .features, \x plan -> { plan | features = x }, Toolkit.list Toolkit.string )


pricingSummary : PricingSummary
pricingSummary =
    { title = "Pricing"
    , intro = "Quickly build an effective pricing table for your potential customers with this Bootstrap example. It’s built with default Bootstrap components and utilities with little customization."
    , plans =
        [ { name = "Free"
          , pricePerMonth = { usd = 0 }
          , features =
                [ "10 users included"
                , "2 GB of storage"
                , "Email support"
                , "Help center access"
                ]
          , callToAction = "Sign up for free"
          , callToActionOutline = True
          }
        , { name = "Pro"
          , pricePerMonth = { usd = 15 }
          , features =
                [ "20 users included"
                , "10 GB of storage"
                , "Priority email support"
                , "Help center access"
                ]
          , callToAction = "Get started"
          , callToActionOutline = False
          }
        , { name = "Enterprise"
          , pricePerMonth = { usd = 29 }
          , features =
                [ "30 users included"
                , "15 GB of storage"
                , "Phone and email support"
                , "Help center access"
                ]
          , callToAction = "Contact us"
          , callToActionOutline = False
          }
        ]
    }


pricingSummaryView : RenderingMode -> PricingSummary -> Html Msg
pricingSummaryView renderingMode summary =
    let
        viewOrEditText path =
            case renderingMode of
                Static ->
                    Toolkit.viewTextStatic pricingSummaryDefinition path summary

                Editable ->
                    Toolkit.viewTextEditable pricingSummaryDefinition path summary
                        |> Html.map ToolkitAction

        addButton children =
            case renderingMode of
                Static ->
                    children

                Editable ->
                    children
                        ++ [ Html.button
                                [ style "position" "absolute"
                                , style "flex-grow" "1"
                                , style "bottom" "5px"
                                , style "right" "10px"
                                , style "background-color" "pink"
                                , style "opacity" "0.5"
                                , style "padding" "5px 15px"
                                , style "border-radius" "5px"
                                , onClick (Add (Plans Nothing))
                                ]
                                [ Html.text "Add" ]
                           ]
    in
    div []
        [ div [ class "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" ]
            [ h1 [ class "display-4" ] [ viewOrEditText Title ]
            , p [ class "lead" ]
                [ viewOrEditText Intro
                ]
            ]
        , div [ class "container" ]
            [ summary.plans
                |> List.indexedMap (\i plan -> viewPricingPlanCard renderingMode (\p -> Plans (Just ( i, p ))) plan)
                |> addButton
                |> div [ class "card-deck mb-3 text-center" ]
            ]
        ]


viewPricingPlanCard : RenderingMode -> (Maybe PricingPlanPath -> PricingSummaryPath) -> PricingPlan -> Html Msg
viewPricingPlanCard renderingMode parentPath pricingPlan =
    let
        viewOrEditText path =
            case renderingMode of
                Static ->
                    Toolkit.viewTextStatic pricingPlanDefinition path pricingPlan

                Editable ->
                    Toolkit.viewTextEditable pricingPlanDefinition path pricingPlan
                        |> Html.map (Toolkit.mapAction (Just >> parentPath))
                        |> Html.map ToolkitAction

        addButton children =
            case renderingMode of
                Static ->
                    children

                Editable ->
                    children
                        ++ [ Html.button
                                [ style "position" "absolute"
                                , style "flex-grow" "1"
                                , style "top" "5px"
                                , style "right" "10px"
                                , style "background-color" "pink"
                                , style "opacity" "0.5"
                                , style "padding" "5px 15px"
                                , style "border-radius" "5px"
                                , onClick (Delete (parentPath Nothing))
                                ]
                                [ Html.text "Delete" ]
                           ]

        buttonClass =
            if pricingPlan.callToActionOutline then
                "btn-outline-primary"

            else
                "btn-primary"
    in
    div [ class "card mb-4 shadow-sm" ]
        ([ div [ class "card-header" ]
            [ h4 [ class "my-0 font-weight-normal" ] [ viewOrEditText Name ]
            ]
         , div [ class "card-body" ]
            [ h1 [ class "card-title pricing-card-title" ]
                [ text "$"
                , viewOrEditText PriceUsd
                , text " "
                , small [ class "text-muted" ] [ text "/ mo" ]
                ]
            , pricingPlan.features
                |> List.indexedMap (\i feature -> li [] [ viewOrEditText (Features (Just i)) ])
                |> ul [ class "list-unstyled mt-3 mb-4" ]
            , button [ class "btn btn-lg btn-block", class buttonClass, type_ "button" ]
                [ text pricingPlan.callToAction ]
            ]
         ]
            |> addButton
        )
