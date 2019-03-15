module ExampleData exposing (pricingSummary)

import PricingSummary exposing (PricingSummary)


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
