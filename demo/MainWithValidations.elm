module MainWithValidations exposing (main)

import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Validate


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



---- MODEL


type alias Model =
    { username : Validatable String
    , nickname : Validatable (Maybe String)
    , email : Validatable String
    , password : Validatable String
    , passwordCopy : Validatable String
    }


type alias Validatable a =
    Validate.Validatable a String


init : ( Model, Cmd msg )
init =
    ( { username = Validate.empty
      , nickname = Validate.valid Nothing
      , email = Validate.empty
      , password = Validate.empty
      , passwordCopy = Validate.empty
      }
    , Cmd.none
    )


validateModel : Model -> ( Model, Cmd Msg )
validateModel model =
    let
        password =
            model.password
                |> Validate.isNotEmpty "you must provide a password"
                |> Validate.atLeast 6 "the password must contain at least 6 characters"

        username =
            model.username
                |> Validate.isNotEmpty "username must not be empty"
                |> Validate.consistsOfLetters "username must consist of letters only"
    in
    ( { model
        | username =
            username
        , nickname =
            model.nickname
                |> Validate.maybe
                    (Validate.consistsOfLetters "nickname must consist of letters only")
        , email =
            model.email
                |> Validate.isNotEmpty "email must not be empty"
                |> Validate.isEmail "this is not a valid email address"
        , password =
            password
        , passwordCopy =
            model.passwordCopy
                |> Validate.equals password "both passwords have to match up"
      }
    , case username |> Validate.validValue of
        Nothing ->
            Cmd.none

        Just username ->
            validateUsername username
    )


validateUsername : String -> Cmd Msg
validateUsername username =
    let
        body =
            [ "username" => Encode.string username ]
                |> Encode.object
                |> Http.jsonBody
    in
    decodeValidationErrors
        |> Http.post "/validate" body
        |> Http.send AddUsernameValidationErrors


decodeValidationErrors : Decoder (Set String)
decodeValidationErrors =
    Decode.list Decode.string
        |> Decode.map Set.fromList



---- PARAMS


type alias SignUpParams =
    { username : String
    , nickname : Maybe String
    , email : String
    , password : String
    }


signUpParams : Model -> Maybe SignUpParams
signUpParams model =
    case
        ( model.username |> Validate.validValue
        , model.nickname |> Validate.validValue
        , model.email |> Validate.validValue
        , model.password |> Validate.validValue
        , model.passwordCopy |> Validate.validValue
        )
    of
        ( Just username, Just nickname, Just email, Just password, Just _ ) ->
            Just
                { username = username
                , nickname = nickname
                , email = email
                , password = password
                }

        _ ->
            Nothing



---- UPDATE


type Msg
    = SetUsername String
    | SetNickname String
    | SetEmail String
    | SetPassword String
    | SetPasswordCopy String
    | ValidateForm
    | AddUsernameValidationErrors (Result Http.Error (Set String))
    | SignUp
    | WelcomeMessage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername string ->
            ( { model | username = Validate.unchecked string }
            , Cmd.none
            )

        SetNickname string ->
            ( { model
                | nickname =
                    if string /= "" then
                        Validate.unchecked (Just string)
                    else
                        Validate.valid Nothing
              }
            , Cmd.none
            )

        SetEmail string ->
            ( { model | email = Validate.unchecked string }
            , Cmd.none
            )

        SetPassword string ->
            ( { model | password = Validate.unchecked string }
            , Cmd.none
            )

        SetPasswordCopy string ->
            ( { model | passwordCopy = Validate.unchecked string }
            , Cmd.none
            )

        ValidateForm ->
            model |> validateModel

        AddUsernameValidationErrors result ->
            case result of
                Err error ->
                    ( model, Cmd.none )

                Ok validationErrors ->
                    ( { model
                        | username =
                            model.username
                                |> Validate.addErrors validationErrors
                      }
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
        Just { username, nickname, email, password } ->
            let
                body =
                    [ Just ("username" => Encode.string username)
                    , nickname
                        |> Maybe.map
                            (\nickname ->
                                "nickname" => Encode.string nickname
                            )
                    , Just ("email" => Encode.string email)
                    , Just ("password" => Encode.string password)
                    ]
                        |> List.filterMap identity
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
        [ viewInput "* username" SetUsername model.username
        , viewInput "nickname"
            SetNickname
            (model.nickname
                |> Validate.map (Maybe.withDefault "")
            )
        , viewInput "* email" SetEmail model.email
        , viewInput "* password" SetPassword model.password
        , viewInput "* password again" SetPasswordCopy model.passwordCopy
        , Html.button
            [ Events.onClick SignUp
            , Attributes.disabled (signUpParams model == Nothing)
            ]
            [ Html.text "sign up" ]
        ]


viewInput : String -> (String -> Msg) -> Validatable String -> Html Msg
viewInput label onInput value =
    let
        viewErrors =
            case Validate.errors value of
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



---- HELPER


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
