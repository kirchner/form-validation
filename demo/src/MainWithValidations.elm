module MainWithValidations exposing (main)

import Browser exposing (Document)
import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Validate


main : Program {} Model Msg
main =
    Browser.document
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
    , age : Validatable Int
    , password : Validatable String
    , passwordCopy : Validatable String
    }


type alias Validatable a =
    Validate.Validatable a String


init : {} -> ( Model, Cmd msg )
init _ =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { username = Validate.empty
    , nickname = Validate.valid Nothing
    , email = Validate.empty
    , age = Validate.empty
    , password = Validate.empty
    , passwordCopy = Validate.empty
    }


validateModel : Model -> ( Model, Cmd Msg )
validateModel model =
    let
        password =
            model.password
                |> Validate.isNotEmpty "You must provide a password."
                |> Validate.atLeast 6 "The password must contain at least 6 characters."

        username =
            model.username
                |> Validate.isNotEmpty "The username must not be empty."
                |> Validate.consistsOfLetters "The username must consist of letters only."
    in
    ( { model
        | username =
            username
        , nickname =
            model.nickname
                |> Validate.maybe
                    (Validate.consistsOfLetters "The nickname must consist of letters only.")
        , email =
            model.email
                |> Validate.isNotEmpty "You must provide an email."
                |> Validate.isEmail "This is not a valid email address."
        , password =
            password
        , passwordCopy =
            model.passwordCopy
                |> Validate.with password
                    (\validPassword ->
                        Validate.equals validPassword "Both passwords have to match up."
                    )
      }
    , case username |> Validate.validValue of
        Nothing ->
            Cmd.none

        Just validUsername ->
            validateUsernameViaBackend validUsername
    )


validateUsernameViaBackend : String -> Cmd Msg
validateUsernameViaBackend username =
    let
        body =
            [ ( "username", Encode.string username ) ]
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
    if Validate.validValue model.passwordCopy == Nothing then
        Nothing
    else
        Just SignUpParams
            |> collect model.username
            |> collect model.nickname
            |> collect model.email
            |> collect model.password


collect : Validatable a -> Maybe (a -> rest) -> Maybe rest
collect validatable maybeCollector =
    Maybe.map2 apply maybeCollector <|
        Validate.validValue validatable


apply : (a -> b) -> a -> b
apply f a =
    f a



---- UPDATE


type Msg
    = SetUsername String
    | SetNickname String
    | SetEmail String
    | SetAge String
    | SetPassword String
    | SetPasswordCopy String
    | ValidateForm
    | AddUsernameValidationErrors (Result Http.Error (Set String))
    | Reset
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

        SetAge string ->
            ( { model
                | age =
                    string
                        |> Validate.isInt "Age must be an integer."
                        |> Validate.satisfies (\age -> age >= 0)
                            "Your age must be positive"
              }
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

        Reset ->
            ( initialModel
            , Cmd.none
            )

        SignUp ->
            ( model
            , signUp model
            )

        WelcomeMessage result ->
            case result of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok message ->
                    ( model
                    , Cmd.none
                    )


signUp : Model -> Cmd Msg
signUp model =
    case model |> signUpParams of
        Just { username, nickname, email, password } ->
            let
                body =
                    [ Just ( "username", Encode.string username )
                    , nickname
                        |> Maybe.map
                            (\actualNickname ->
                                ( "nickname", Encode.string actualNickname )
                            )
                    , Just ( "email", Encode.string email )
                    , Just ( "password", Encode.string password )
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


view : Model -> Document Msg
view model =
    { title = "elm-form-validation demo"
    , body =
        [ Html.div
            [ Attributes.class "container" ]
            [ Html.form
                [ Attributes.class "form-horizontal"
                , Attributes.class "col-sm-offset-2"
                , Attributes.class "col-sm-8"
                ]
                [ Html.h1
                    [ Attributes.class "text-center" ]
                    [ Html.text "Join our service!" ]
                , viewInput "text" "* Username" SetUsername model.username
                , viewInput "text"
                    "Nickname"
                    SetNickname
                    (model.nickname
                        |> Validate.map (Maybe.withDefault "")
                    )
                , viewInput "email" "* Email" SetEmail model.email
                , model.age
                    |> Validate.map String.fromInt
                    |> viewInput "age" "* Age" SetAge
                , viewInput "password" "* Password" SetPassword model.password
                , viewInput "password" "* Password again" SetPasswordCopy model.passwordCopy
                , Html.div
                    [ Attributes.class "form-group" ]
                    [ Html.div
                        [ Attributes.class "col-sm-offset-4"
                        , Attributes.class "col-sm-8"
                        ]
                        [ Html.div
                            [ Attributes.class "row" ]
                            [ Html.div
                                [ Attributes.class "col-sm-6" ]
                                [ Html.button
                                    [ Attributes.class "btn"
                                    , Attributes.class "btn-warning"
                                    , Attributes.class "btn-block"
                                    , Events.preventDefaultOn "click"
                                        (Decode.succeed ( Reset, True ))
                                    ]
                                    [ Html.text "Clear form" ]
                                ]
                            , Html.div
                                [ Attributes.class "col-sm-6" ]
                                [ Html.button
                                    [ Attributes.class "btn"
                                    , Attributes.class "btn-primary"
                                    , Attributes.class "btn-block"
                                    , Events.preventDefaultOn "click"
                                        (Decode.succeed ( SignUp, True ))
                                    , Attributes.disabled (signUpParams model == Nothing)
                                    ]
                                    [ Html.text "Sign up" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewInput : String -> String -> (String -> Msg) -> Validatable String -> Html Msg
viewInput type_ label onInput value =
    let
        viewErrors =
            case Validate.errors value of
                Nothing ->
                    []

                Just errors ->
                    [ errors
                        |> Set.toList
                        |> List.map viewError
                        |> Html.div
                            [ Attributes.class "col-sm-offset-4"
                            , Attributes.class "col-sm-8"
                            ]
                    ]

        viewError error =
            Html.span
                [ Attributes.class "help-block" ]
                [ Html.text error ]

        id =
            label
                |> String.toLower
                |> String.toList
                |> List.filter Char.isLower
                |> String.fromList
    in
    [ [ Html.label
            [ Attributes.for id
            , Attributes.class "col-sm-4"
            , Attributes.class "control-label"
            ]
            [ Html.text label ]
      , Html.div
            [ Attributes.class "col-sm-8" ]
            [ Html.input
                [ Attributes.id id
                , Attributes.class "form-control"
                , Attributes.type_ type_
                , Events.onInput onInput
                , Events.onBlur ValidateForm
                ]
                []
            ]
      ]
    , viewErrors
    ]
        |> List.concat
        |> Html.div
            ([ [ Attributes.class "form-group" ]
             , if Validate.validValue value /= Nothing then
                [ Attributes.class "has-success" ]
               else if Validate.errors value /= Nothing then
                [ Attributes.class "has-error" ]
               else
                []
             ]
                |> List.concat
            )
