module Utils.State exposing
    ( State
    , StateMaybe
    , andThen
    , apply
    , applyMaybe
    , map
    , mapMaybe
    , pure
    , pureMaybe
    , run
    , traverse
    , traverseDict
    , traverseDictMaybe
    , traverseMaybe
    )

import Dict exposing (Dict)



-- ORIGINAL


type alias State state value =
    state -> ( state, value )


run : state -> State state value -> ( state, value )
run state value =
    value state


map : (fromValue -> toValue) -> State state fromValue -> State state toValue
map transform fromValue state =
    let
        ( s1, f ) =
            fromValue state
    in
    ( s1, transform f )


pure : value -> State state value
pure value state =
    ( state, value )


apply : State state fromValue -> State state (fromValue -> toValue) -> State state toValue
apply fromValue transform state =
    let
        ( s1, t ) =
            transform state

        ( s2, f ) =
            fromValue s1
    in
    ( s2, t f )


traverse : (fromValue -> State state toValue) -> List fromValue -> State state (List toValue)
traverse transform inputs =
    let
        runAll input currentState =
            map (::) (transform input)
                |> apply currentState
    in
    List.foldr runAll (pure []) inputs


traverseDict : (fromValue -> State state toValue) -> Dict comparable fromValue -> State state (Dict comparable toValue)
traverseDict transform inputs =
    let
        runAll key input currentState =
            map (Dict.insert key) (transform input)
                |> apply currentState
    in
    Dict.foldr runAll (pure Dict.empty) inputs


andThen : (fromValue -> State state toValue) -> State state fromValue -> State state toValue
andThen transform fromValue state =
    let
        ( s1, f ) =
            fromValue state

        ( s2, v ) =
            transform f s1
    in
    ( s2, v )



-- MAYBE VARIANT
-- aka (StateT s Maybe)


type alias StateMaybe state value =
    State state (Maybe value)


mapMaybe : (fromValue -> toValue) -> StateMaybe state fromValue -> StateMaybe state toValue
mapMaybe transform fromValue =
    map (Maybe.map transform) fromValue


pureMaybe : value -> StateMaybe state value
pureMaybe =
    pure << Just


applyMaybe : StateMaybe state fromValue -> StateMaybe state (fromValue -> toValue) -> StateMaybe state toValue
applyMaybe fromValue transform =
    let
        transformMaybe mf =
            case mf of
                Just f ->
                    Maybe.map f

                _ ->
                    always Nothing
    in
    apply fromValue (map transformMaybe transform)


traverseMaybe : (fromValue -> StateMaybe state toValue) -> List fromValue -> StateMaybe state (List toValue)
traverseMaybe transform inputs =
    let
        runAll input currentState =
            mapMaybe (::) (transform input)
                |> applyMaybe currentState
    in
    List.foldr runAll (pureMaybe []) inputs


traverseDictMaybe : (fromValue -> StateMaybe state toValue) -> Dict comparable fromValue -> StateMaybe state (Dict comparable toValue)
traverseDictMaybe transform inputs =
    let
        runAll key input currentState =
            mapMaybe (Dict.insert key) (transform input)
                |> applyMaybe currentState
    in
    Dict.foldr runAll (pureMaybe Dict.empty) inputs
