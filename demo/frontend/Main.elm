module Main exposing (main)

import Char
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


main : Program Never Model Msg
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
    , nickname : Maybe String
    , email : String
    , password : String
    , passwordCopy : String
    }


init : ( Model, Cmd msg )
init =
    ( { username = ""
      , nickname = Nothing
      , email = ""
      , password = ""
      , passwordCopy = ""
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = SetUsername String
    | SetNickname String
    | SetEmail String
    | SetPassword String
    | SetPasswordCopy String
    | Reset
    | SignUp
    | WelcomeMessage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername string ->
            ( { model | username = string }
            , Cmd.none
            )

        SetNickname string ->
            ( { model
                | nickname =
                    if string /= "" then
                        Just string
                    else
                        Nothing
              }
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

        Reset ->
            init

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
            [ Just ("username" => Encode.string model.username)
            , model.nickname
                |> Maybe.map (\nickname -> "nickname" => Encode.string nickname)
            , Just ("email" => Encode.string model.email)
            , Just ("password" => Encode.string model.password)
            , Just ("password_copy" => Encode.string model.passwordCopy)
            ]
                |> List.filterMap identity
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
    Html.div
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
            , viewInput "text" "Nickname" SetNickname (model.nickname |> Maybe.withDefault "")
            , viewInput "email" "* Email" SetEmail model.email
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
                                , Events.onWithOptions "click"
                                    { preventDefault = True
                                    , stopPropagation = False
                                    }
                                    (Decode.succeed Reset)
                                ]
                                [ Html.text "Clear form" ]
                            ]
                        , Html.div
                            [ Attributes.class "col-sm-6" ]
                            [ Html.button
                                [ Attributes.class "btn"
                                , Attributes.class "btn-primary"
                                , Attributes.class "btn-block"
                                , Events.onWithOptions "click"
                                    { preventDefault = True
                                    , stopPropagation = False
                                    }
                                    (Decode.succeed SignUp)
                                ]
                                [ Html.text "Sign up" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewInput : String -> String -> (String -> Msg) -> String -> Html Msg
viewInput type_ label onInput value =
    let
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
                ]
                []
            ]
      ]
    ]
        |> List.concat
        |> Html.div [ Attributes.class "form-group" ]



---- HELPER


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
