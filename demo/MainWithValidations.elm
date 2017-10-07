module MainWithValidations exposing (main)

import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Validate exposing (..)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



---- MODEL


type alias Model =
    { username : Validatable String String
    , email : Validatable String String
    , password : Validatable String String
    , passwordCopy : Validatable String String
    }


init : ( Model, Cmd msg )
init =
    ( { username = empty
      , email = empty
      , password = empty
      , passwordCopy = empty
      }
    , Cmd.none
    )


validateModel : Model -> Model
validateModel model =
    let
        password =
            model.password
                |> isNotEmpty "you must provide a password"
                |> atLeast 6 "the password must contain at least 6 characters"
    in
    { model
        | username =
            model.username
                |> isNotEmpty "username must not be empty"
                |> consistsOfLetters "username must consist of letters only"
        , email =
            model.email
                |> isNotEmpty "email must not be empty"
                |> isEmail "this is not a valid email address"
        , password =
            password
        , passwordCopy =
            model.passwordCopy
                |> equals password "both passwords have to match up"
    }


type alias SignUpParams =
    { username : String
    , email : String
    , password : String
    }


signUpParams : Model -> Maybe SignUpParams
signUpParams model =
    case
        ( model.username |> validValue
        , model.email |> validValue
        , model.password |> validValue
        , model.passwordCopy |> validValue
        )
    of
        ( Just username, Just email, Just password, Just _ ) ->
            Just
                { username = username
                , email = email
                , password = password
                }

        _ ->
            Nothing



---- UPDATE


type Msg
    = SetUsername String
    | SetEmail String
    | SetPassword String
    | SetPasswordCopy String
    | ValidateForm
    | SignUp
    | WelcomeMessage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername string ->
            ( { model | username = unchecked string }
            , Cmd.none
            )

        SetEmail string ->
            ( { model | email = unchecked string }
            , Cmd.none
            )

        SetPassword string ->
            ( { model | password = unchecked string }
            , Cmd.none
            )

        SetPasswordCopy string ->
            ( { model | passwordCopy = unchecked string }
            , Cmd.none
            )

        ValidateForm ->
            ( model |> validateModel
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
    case model |> signUpParams of
        Just { username, email, password } ->
            let
                body =
                    [ "username" => Encode.string username
                    , "email" => Encode.string email
                    , "password" => Encode.string password
                    ]
                        |> Encode.object
                        |> Http.jsonBody
            in
            decodeSignUpResponse
                |> Http.post "/signup" body
                |> Http.send WelcomeMessage

        _ ->
            Cmd.none


decodeSignUpResponse : Decoder String
decodeSignUpResponse =
    Decode.fail "TODO: decode the actual response"



---- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ viewValidatedInput "username" SetUsername model.username
        , viewValidatedInput "email" SetEmail model.email
        , viewValidatedInput "password" SetPassword model.password
        , viewValidatedInput "password again" SetPasswordCopy model.passwordCopy
        , Html.button
            [ Events.onClick SignUp
            , Attributes.disabled (signUpParams model == Nothing)
            ]
            [ Html.text "sign up" ]
        ]


viewValidatedInput : String -> (String -> Msg) -> Validatable String String -> Html Msg
viewValidatedInput label onInput value =
    let
        viewErrors =
            case errors value of
                Nothing ->
                    []

                Just errors ->
                    errors
                        |> Set.toList
                        |> List.map viewError

        viewError error =
            Html.div []
                [ Html.text error ]
    in
    [ [ Html.div []
            [ Html.text label ]
      , Html.input
            [ Events.onInput onInput
            , Events.onBlur ValidateForm
            ]
            []
      ]
    , viewErrors
    ]
        |> List.concat
        |> Html.div []


viewInput : String -> (String -> Msg) -> Html Msg
viewInput label onInput =
    Html.div []
        [ Html.div []
            [ Html.text label ]
        , Html.input
            [ Events.onInput onInput
            , Events.onBlur ValidateForm
            ]
            []
        ]



---- HELPER


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
