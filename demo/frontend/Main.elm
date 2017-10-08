module Main exposing (main)

import Html exposing (Html)
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



---- MODEL


type alias Model =
    { username : String
    , email : String
    , password : String
    , passwordCopy : String
    }


init : ( Model, Cmd msg )
init =
    ( { username = ""
      , email = ""
      , password = ""
      , passwordCopy = ""
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = SetUsername String
    | SetEmail String
    | SetPassword String
    | SetPasswordCopy String
    | SignUp
    | WelcomeMessage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername string ->
            ( { model | username = string }
            , Cmd.none
            )

        SetEmail string ->
            ( { model | email = string }
            , Cmd.none
            )

        SetPassword string ->
            ( { model | password = string }
            , Cmd.none
            )

        SetPasswordCopy string ->
            ( { model | passwordCopy = string }
            , Cmd.none
            )

        SignUp ->
            ( model
            , signUp model
            )

        WelcomeMessage result ->
            case result of
                Err error ->
                    let
                        _ =
                            Debug.log "something went wrong"
                                error
                    in
                    ( model
                    , Cmd.none
                    )

                Ok message ->
                    let
                        _ =
                            Debug.log "message" message
                    in
                    ( model
                    , Cmd.none
                    )


signUp : Model -> Cmd Msg
signUp model =
    let
        body =
            [ "username" => Encode.string model.username
            , "email" => Encode.string model.email
            , "password" => Encode.string model.password
            , "password_copy" => Encode.string model.passwordCopy
            ]
                |> Encode.object
                |> Http.jsonBody
    in
    decodeSignUpResponse
        |> Http.post "/signup" body
        |> Http.send WelcomeMessage


decodeSignUpResponse : Decoder String
decodeSignUpResponse =
    Decode.fail "TODO: decode the actual response"



---- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ viewInput "username" SetUsername
        , viewInput "email" SetEmail
        , viewInput "password" SetPassword
        , viewInput "password again" SetPasswordCopy
        , Html.button
            [ Events.onClick SignUp ]
            [ Html.text "sign up" ]
        ]


viewInput : String -> (String -> msg) -> Html msg
viewInput label onInput =
    Html.div []
        [ Html.div []
            [ Html.text label ]
        , Html.input
            [ Events.onInput onInput ]
            []
        ]



---- HELPER


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
