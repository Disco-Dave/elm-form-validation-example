module User.Email exposing
    ( Email
    , EmailError(..)
    , make
    , toString
    )

import Regex


type Email
    = Email String


toString : Email -> String
toString email =
    case email of
        Email value ->
            value


type EmailError
    = IsEmpty
    | IsInvalid


make : String -> Result EmailError Email
make rawEmail =
    let
        emailRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

        trimmedEmail =
            String.trim rawEmail

        isValid =
            Regex.contains emailRegex trimmedEmail
    in
    if String.isEmpty trimmedEmail then
        Err IsEmpty

    else if isValid then
        Ok (Email trimmedEmail)

    else
        Err IsInvalid
