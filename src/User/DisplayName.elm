module User.DisplayName exposing
    ( DisplayName
    , DisplayNameError(..)
    , make
    , maxLength
    , toString
    )


type DisplayName
    = DisplayName String


toString : DisplayName -> String
toString displayName =
    case displayName of
        DisplayName value ->
            value


maxLength : Int
maxLength =
    25


type DisplayNameError
    = IsEmpty
    | IsLongerThan Int


make : String -> Result DisplayNameError DisplayName
make displayName =
    let
        trimmedDisplayName =
            String.trim displayName

        length =
            String.length trimmedDisplayName
    in
    if length == 0 then
        Err IsEmpty

    else if length > maxLength then
        Err <| IsLongerThan maxLength

    else
        Ok <| DisplayName trimmedDisplayName
