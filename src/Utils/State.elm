module Utils.State exposing
    ( State
    , StateMaybe
    , andThen
    , apply
    , applyMaybe
    , map
    , mapMaybe
    , run
    )

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


apply : State state fromValue -> State state (fromValue -> toValue) -> State state toValue
apply fromValue transform state =
    let
        ( s1, t ) =
            transform state

        ( s2, f ) =
            fromValue s1
    in
    ( s2, t f )


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
