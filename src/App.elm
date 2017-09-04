module App
    exposing
        ( Model(..)
        , Msg
        , InitialState
        , FetchSuccessState
        , FetchingState
        , FetchCap
        , FetchErrorState
        , ProgramDefinition
        , program
        , fetch
        , process
        )

{-| This module provides the means to initialize the application. The functions and types (model and msg) are designed such that at any point in time the application can only perform the operations it's designed to perform and nothing else. This is accomplished by:

- Representing the app's state as a sum type rather than a product type (record)
- Exposing the states for pattern matching, but requiring the constructors (tags) to take additional arguments which can only be provided via this module.
-}

import Html as Html exposing (Html)
import Http exposing (Error, get, send)
import Profile exposing (Profile, decodeProfile)


{-| This model represents the state of the application. Making the application state a sum type rather than a product type makes each state and it's requirements explicit.
-}
type Model
    = Initial InitialState FetchCap
    | Fetching FetchingState
    | FetchError FetchErrorState Error FetchCap
    | FetchSuccess FetchSuccessState Profile


{-| This represents the initial state of the application. Because the type is opaque (the "tags" are not exposed) instances cannot be created outside of this module. And the only function which produces this type is `program`, making it impossible to change into this state once the app is running!
-}
type InitialState
    = InitialState


type FetchSuccessState
    = FetchSuccessState


type FetchingState
    = FetchingState


{-| A pseudo-capability required to perform a Profile fetch.
-}
type FetchCap
    = FetchCap


type FetchErrorState
    = FetchErrorState


type Msg
    = RequestReceived (Result Error Profile)


{-| This record is a partial HTML program definition; it omits `init`. That's to make it impossible to accidently initialize the application twice.
-}
type alias ProgramDefinition msg =
    { view : Model -> Html msg
    , update : msg -> Model -> ( Model, Cmd msg )
    , subscriptions : Model -> Sub msg
    }


{-| Provides an initialized HTML program. Since the applications entry point, `main` returns only one of these, having `init` within makes it impossible to initialize the program twice.
-}
program : ProgramDefinition msg -> Program Never Model msg
program def =
    let
        init : ( Model, Cmd msg )
        init =
            ( Initial InitialState FetchCap, Cmd.none )
    in
        Html.program
            { init = init
            , update = def.update
            , view = def.view
            , subscriptions = def.subscriptions
            }


{-| Initiates a Profile fetch.
-}
fetch : (Msg -> msg) -> FetchCap -> ( Model, Cmd msg )
fetch f _ =
    let
        url =
            "https://www.codeschool.com/users/bijanbwb.json"

        request =
            get url decodeProfile

        cmd =
            send RequestReceived request
                |> Cmd.map f
    in
        ( Fetching FetchingState, cmd )


{-| Processes the result of a Profile fetch, transitioning to the appropriate state.
-}
process : (Msg -> msg) -> Msg -> ( Model, Cmd msg )
process _ (RequestReceived result) =
    let
        model =
            case result of
                Ok user ->
                    FetchSuccess FetchSuccessState user

                Err err ->
                    FetchError FetchErrorState err FetchCap
    in
        ( model, Cmd.none )
