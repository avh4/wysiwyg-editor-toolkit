module Main exposing (main)

import Browser
import Dict
import ExampleData
import Html exposing (Html, a, button, div, h1, h4, h5, img, li, p, small, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, rel, src, style, type_, value)
import Html.Events exposing (onClick)
import PricingSummary exposing (PricingPlan, PricingSummary)
import Time
import WysiwygEditorToolkit as Toolkit exposing (OfThree(..), OfTwo(..), Rendering)


type alias Model =
    { editorData : PricingSummary
    , toolkitState : Toolkit.State PricingSummaryPath
    , renderingMode : RenderingMode
    }


type RenderingMode
    = Static
    | Editable


initialModel : Model
initialModel =
    { editorData = ExampleData.pricingSummary
    , toolkitState =
        Toolkit.initState Debug.toString <|
            let
                quest =
                    { name = "Quest"
                    , avatar = "https://gravatar.com/avatar/f9879d71855b5ff21e4963273a886bfc"
                    }

                west =
                    { name = "West J"
                    , avatar = "https://www.gravatar.com/avatar/205e460b479e2e5b48aec07710c08d50"
                    }
            in
            [ ( Title
              , { content = "Maybe \"Plans for Everyone\"?"
                , author = quest
                , createdAt = Time.millisToPosix 0
                }
              )
            , ( Plans (Just ( 1, Just (Features Nothing) ))
              , { content = "Can we add one more item here?"
                , author = west
                , createdAt = Time.millisToPosix 250000000
                }
              )
            , ( Plans (Just ( 2, Just PriceUsd ))
              , { content = "Are the price changes happening next month?"
                , author = quest
                , createdAt = Time.millisToPosix 100000000
                }
              )
            , ( Plans (Just ( 2, Just PriceUsd ))
              , { content = "changes aren't finalized yet"
                , author = west
                , createdAt = Time.millisToPosix 200000000
                }
              )
            ]
    , renderingMode = Editable
    }


type Msg
    = SetRenderingMode RenderingMode
    | ToolkitAction (Toolkit.EditAction PricingSummaryPath)
    | ToolkitMsg (Toolkit.Msg PricingSummaryPath)
    | Add PricingSummaryPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRenderingMode renderingMode ->
            ( { model | renderingMode = renderingMode }
            , Cmd.none
            )

        ToolkitAction action ->
            ( { model | editorData = Toolkit.applyEditAction pricingSummaryDefinition action model.editorData }
            , Cmd.none
            )

        ToolkitMsg toolkitMsg ->
            let
                ( newData, newState, effect ) =
                    Toolkit.update pricingSummaryDefinition toolkitMsg model.toolkitState model.editorData

                _ =
                    Debug.log "Toolkit.Effect" effect
            in
            ( { model
                | editorData = newData
                , toolkitState = newState
              }
            , Cmd.none
            )

        Add path ->
            ( { model | editorData = applyAdd path model.editorData }
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
    let
        render =
            case model.renderingMode of
                Static ->
                    Toolkit.staticRendering pricingSummaryDefinition model.editorData

                Editable ->
                    Toolkit.wysiwygRenderingWithComments ToolkitMsg pricingSummaryDefinition model.toolkitState model.editorData
    in
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
        , pricingSummaryView render model.renderingMode model.editorData
        , Html.hr [] []
        , Html.code [] [ Html.text (Debug.toString model.editorData) ]
        , Html.hr [] []
        , Html.code [] [ Html.text (Debug.toString model.toolkitState) ]
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


pricingSummaryView : Rendering PricingSummaryPath Msg -> RenderingMode -> PricingSummary -> Html Msg
pricingSummaryView render renderingMode summary =
    let
        withAddButton container children =
            case renderingMode of
                Static ->
                    container children

                Editable ->
                    let
                        addButton =
                            Html.button
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
                    in
                    container (children ++ [ addButton ])
    in
    div []
        [ div [ class "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" ]
            [ h1 [ class "display-4" ] [ render.text Title ]
            , p [ class "lead" ]
                [ render.text Intro
                ]
            ]
        , div [ class "container" ]
            [ summary.plans
                |> List.indexedMap
                    (\i plan ->
                        viewPricingPlanCard
                            (Toolkit.focusRendering (\p -> Plans (Just ( i, p ))) render)
                            plan
                    )
                |> withAddButton (div [ class "card-deck mb-3 text-center" ])
            , render.comments (Plans Nothing)
            ]
        ]


viewPricingPlanCard : Rendering (Maybe PricingPlanPath) Msg -> PricingPlan -> Html Msg
viewPricingPlanCard render pricingPlan =
    let
        buttonClass =
            if pricingPlan.callToActionOutline then
                "btn-outline-primary"

            else
                "btn-primary"
    in
    render.withDelete Nothing
        (div [ class "card mb-4 shadow-sm" ])
        [ div [ class "card-header" ]
            [ h4 [ class "my-0 font-weight-normal" ] [ render.text (Just Name) ]
            ]
        , div [ class "card-body" ]
            [ h1 [ class "card-title pricing-card-title" ]
                [ text "$"
                , render.text (Just PriceUsd)
                , text " "
                , small [ class "text-muted" ] [ text "/ mo" ]
                ]
            , Html.div
                [ style "position" "relative"
                ]
                [ pricingPlan.features
                    |> List.indexedMap (\i feature -> li [] [ render.text (Just (Features (Just i))) ])
                    |> ul [ class "list-unstyled mt-3 mb-4" ]
                , render.comments (Just (Features Nothing))
                ]
            , button [ class "btn btn-lg btn-block", class buttonClass, type_ "button" ]
                [ text pricingPlan.callToAction ]
            ]
        ]
