module Comments exposing (Author, Comment)

import Time


type alias Url =
    String


type alias Comment =
    { content : String
    , author : Author
    , createdAt : Time.Posix
    }


type alias Author =
    { name : String
    , avatar : Url
    }
