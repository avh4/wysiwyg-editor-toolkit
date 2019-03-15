module PricingSummary exposing (PricingPlan, PricingSummary)


type alias PricingSummary =
    { title : String
    , intro : String
    , plans : List PricingPlan
    }


type alias PricingPlan =
    { name : String
    , pricePerMonth :
        { usd : Int
        }
    , features : List String
    , callToAction : String
    , callToActionOutline : Bool
    }
