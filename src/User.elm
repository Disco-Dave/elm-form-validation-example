module User exposing (User)

import User.Age exposing (Age)
import User.DisplayName exposing (DisplayName)
import User.Email exposing (Email)


type alias User =
    { email : Email
    , displayName : DisplayName
    , age : Age
    }
