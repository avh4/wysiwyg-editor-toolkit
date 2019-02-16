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
                [ div [ class "card mb-4 shadow-sm" ]
                    [ div [ class "card-header" ]
                        [ h4 [ class "my-0 font-weight-normal" ] [ text "Free" ]
                        ]
                    , div [ class "card-body" ]
                        [ h1 [ class "card-title pricing-card-title" ]
                            [ text "$0 "
                            , small [ class "text-muted" ] [ text "/ mo" ]
                            ]
                        , ul [ class "list-unstyled mt-3 mb-4" ]
                            [ li [] [ text "10 users included" ]
                            , li [] [ text "2 GB of storage" ]
                            , li [] [ text "Email support" ]
                            , li [] [ text "Help center access" ]
                            ]
                        , button [ class "btn btn-lg btn-block btn-outline-primary", type_ "button" ]
                            [ text "Sign up for free" ]
                        ]
                    ]
                , div [ class "card mb-4 shadow-sm" ]
                    [ div [ class "card-header" ]
                        [ h4 [ class "my-0 font-weight-normal" ] [ text "Pro" ]
                        ]
                    , div [ class "card-body" ]
                        [ h1 [ class "card-title pricing-card-title" ]
                            [ text "$15 "
                            , small [ class "text-muted" ] [ text "/ mo" ]
                            ]
                        , ul [ class "list-unstyled mt-3 mb-4" ]
                            [ li [] [ text "20 users included" ]
                            , li [] [ text "10 GB of storage" ]
                            , li [] [ text "Priority email support" ]
                            , li [] [ text "Help center access" ]
                            ]
                        , button [ class "btn btn-lg btn-block btn-primary", type_ "button" ]
                            [ text "Get started" ]
                        ]
                    ]
                , div [ class "card mb-4 shadow-sm" ]
                    [ div [ class "card-header" ]
                        [ h4 [ class "my-0 font-weight-normal" ] [ text "Enterprise" ]
                        ]
                    , div [ class "card-body" ]
                        [ h1 [ class "card-title pricing-card-title" ]
                            [ text "$29 "
                            , small [ class "text-muted" ] [ text "/ mo" ]
                            ]
                        , ul [ class "list-unstyled mt-3 mb-4" ]
                            [ li [] [ text "30 users included" ]
                            , li [] [ text "15 GB of storage" ]
                            , li [] [ text "Phone and email support" ]
                            , li [] [ text "Help center access" ]
                            ]
                        , button [ class "btn btn-lg btn-block btn-primary", type_ "button" ]
                            [ text "Contact us" ]
                        ]
                    ]
                ]
            ]
        ]
