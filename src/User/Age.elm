module User.Age exposing
    ( Age
    , AgeError(..)
    , make
    , maxAge
    , minAge
    , toInt
    )


type Age
    = Age Int


toInt : Age -> Int
toInt age =
    case age of
        Age value ->
            value


minAge : Int
minAge =
    18


maxAge : Int
maxAge =
    125


type AgeError
    = IsLessThanMinAge Int
    | IsGreaterThanMaxAge Int


make : Int -> Result AgeError Age
make age =
    if age < minAge then
        Err (IsLessThanMinAge minAge)

    else if age > maxAge then
        Err (IsGreaterThanMaxAge maxAge)

    else
        Ok (Age age)
