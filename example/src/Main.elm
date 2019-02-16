module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, h4, h5, img, li, p, small, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, rel, src, type_)


main : Program () {} Never
main =
    Browser.document
        { init = \() -> ( {}, Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view =
            \model ->
                { title = "avh4/wysiwyg-editor-toolkit demo"
                , body =
                    [ Html.h2 [] [ Html.text "avh4/wysiwyg-editor-toolkit demo" ]
                    , Html.node "link" [ href "bootstrap-4.3.1.min.css", rel "stylesheet", type_ "text/css" ] []
                    , Html.node "link" [ href "../bootstrap-4.3.1.min.css", rel "stylesheet", type_ "text/css" ] []
                    , Html.node "style" [] [ Html.text ".pricing-header { max-width: 700px }" ]
                    , Html.p [] [ Html.text "(example is taken from https://getbootstrap.com/docs/4.3/examples/pricing/)" ]
                    , pricingView
                    ]
                }
        }


pricingView : Html msg
pricingView =
    div []
        [ div [ class "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" ]
            [ h1 [ class "display-4" ] [ text "Pricing" ]
            , p [ class "lead" ]
                [ text "Quickly build an effective pricing table for your potential customers with this Bootstrap example. It’s built with default Bootstrap components and utilities with little customization." ]
            ]
        , div [ class "container" ]
            [ div [ class "card-deck mb-3 text-center" ]
                [ viewPricingPlanCard
                    { name = "Free"
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
                , viewPricingPlanCard
                    { name = "Pro"
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
                , viewPricingPlanCard
                    { name = "Enterprise"
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
            ]
        ]


type alias PricingPlan =
    { name : String
    , pricePerMonth :
        { usd : Int
        }
    , features : List String
    , callToAction : String
    , callToActionOutline : Bool
    }


viewPricingPlanCard : PricingPlan -> Html msg
viewPricingPlanCard pricingPlan =
    let
        buttonClass =
            if pricingPlan.callToActionOutline then
                "btn-outline-primary"

            else
                "btn-primary"
    in
    div [ class "card mb-4 shadow-sm" ]
        [ div [ class "card-header" ]
            [ h4 [ class "my-0 font-weight-normal" ] [ text pricingPlan.name ]
            ]
        , div [ class "card-body" ]
            [ h1 [ class "card-title pricing-card-title" ]
                [ text "$"
                , text (String.fromInt pricingPlan.pricePerMonth.usd)
                , text " "
                , small [ class "text-muted" ] [ text "/ mo" ]
                ]
            , pricingPlan.features
                |> List.map (\feature -> li [] [ text feature ])
                |> ul [ class "list-unstyled mt-3 mb-4" ]
            , button [ class "btn btn-lg btn-block", class buttonClass, type_ "button" ]
                [ text pricingPlan.callToAction ]
            ]
        ]
