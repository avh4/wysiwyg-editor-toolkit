module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, h4, h5, img, li, p, small, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, rel, src, type_, value)
import Html.Events exposing (onClick)
import WysiwygEditorToolkit as Toolkit


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
    | Edit PricingSummaryPath String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRenderingMode renderingMode ->
            ( { model | renderingMode = renderingMode }
            , Cmd.none
            )

        Edit path text ->
            ( { model | editorData = applyEdit path text model.editorData }
            , Cmd.none
            )


applyEdit : PricingSummaryPath -> String -> PricingSummary -> PricingSummary
applyEdit path text data =
    case path of
        Title ->
            { data | title = text }

        Intro ->
            { data | intro = text }

        Plan index rest ->
            { data
                | plans =
                    List.indexedMap
                        (\i plan ->
                            if index == i then
                                applyPlanEdit rest text plan

                            else
                                plan
                        )
                        data.plans
            }


applyPlanEdit : PricingPlanPath -> String -> PricingPlan -> PricingPlan
applyPlanEdit path text plan =
    case path of
        Name ->
            { plan | name = text }

        PriceUsd ->
            case String.toInt text of
                Just usd ->
                    { plan | pricePerMonth = { usd = usd } }

                Nothing ->
                    plan

        Features index ->
            { plan
                | features =
                    List.indexedMap
                        (\i feature ->
                            if index == i then
                                text

                            else
                                feature
                        )
                        plan.features
            }


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
    | Plan Int PricingPlanPath


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
    | Features Int


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
        viewOrEditText path value =
            case renderingMode of
                Static ->
                    Toolkit.viewTextStatic value

                Editable ->
                    Toolkit.viewTextEditable (Edit path) value
    in
    div []
        [ div [ class "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" ]
            [ h1 [ class "display-4" ] [ viewOrEditText Title summary.title ]
            , p [ class "lead" ]
                [ viewOrEditText Intro summary.intro
                ]
            ]
        , div [ class "container" ]
            [ summary.plans
                |> List.indexedMap (\i -> viewPricingPlanCard renderingMode (Plan i))
                |> div [ class "card-deck mb-3 text-center" ]
            ]
        ]


viewPricingPlanCard : RenderingMode -> (PricingPlanPath -> PricingSummaryPath) -> PricingPlan -> Html Msg
viewPricingPlanCard renderingMode parentPath pricingPlan =
    let
        viewOrEditText path value =
            case renderingMode of
                Static ->
                    Toolkit.viewTextStatic value

                Editable ->
                    Toolkit.viewTextEditable (Edit (parentPath path)) value

        buttonClass =
            if pricingPlan.callToActionOutline then
                "btn-outline-primary"

            else
                "btn-primary"
    in
    div [ class "card mb-4 shadow-sm" ]
        [ div [ class "card-header" ]
            [ h4 [ class "my-0 font-weight-normal" ] [ viewOrEditText Name pricingPlan.name ]
            ]
        , div [ class "card-body" ]
            [ h1 [ class "card-title pricing-card-title" ]
                [ text "$"
                , viewOrEditText PriceUsd (String.fromInt pricingPlan.pricePerMonth.usd)
                , text " "
                , small [ class "text-muted" ] [ text "/ mo" ]
                ]
            , pricingPlan.features
                |> List.indexedMap (\i feature -> li [] [ viewOrEditText (Features i) feature ])
                |> ul [ class "list-unstyled mt-3 mb-4" ]
            , button [ class "btn btn-lg btn-block", class buttonClass, type_ "button" ]
                [ text pricingPlan.callToAction ]
            ]
        ]
