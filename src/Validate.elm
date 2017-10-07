module Validate
    exposing
        ( Validatable
        , atLeast
        , consistsOfLetters
        , empty
        , equals
        , errors
        , isEmail
        , isNotEmpty
        , satisfies
        , uncheck
        , unchecked
        , validValue
        )

{-| Suppose your application has a sign up form and you want to start
validating the user input before sending the actual request to the
server.

@docs Validatable

@docs empty

@docs unchecked

Now, before you can actually use the values provided by the user, you
have to run validations on your model:

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

In this example, the parameters we actually need to submit will be

    type alias SignUpParams =
        { username : String
        , email : String
        , password : String
        }

and we can extract a valid set of parameters with

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


# Creating validatable values

@docs empty, unchecked, uncheck


# Getting information

@docs validValue, errors


# Simple String Validations

@docs isNotEmpty, atLeast, consistsOfLetters, isEmail


# Creating Validations

@docs satisfies, equals

-}

import Char
import Regex
import Set exposing (Set)


{-| You have to wrap every value of your form inside this type. For
example, if your model looked something like

    type alias FormerModel =
        { username : String
        , email : String
        , password : String
        , passwordCopy : String
        }

you have to change it to

    type alias Model =
        { username : Validatable String String
        , email : Validatable String String
        , password : Validatable String String
        , passwordCopy : Validatable String String
        }

-}
type Validatable a comparable
    = Empty
    | Unchecked a
    | Valid a
    | Invalid a (Set comparable)


{-| Use this to initialize your values:

    init : Model
    init =
        { username = empty
        , email = empty
        , pasword = empty
        , passwordCopy = empty
        }

-}
empty : Validatable a comparable
empty =
    Empty


{-| Use this to update a value in your model in reaction to user input:

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            SetUsername string ->
                { model | username = unchecked string }

            ...

-}
unchecked : a -> Validatable a comparable
unchecked a =
    Unchecked a


{-| Set the value to unchecked if possible.
-}
uncheck : Validatable a comparable -> Validatable a comparable
uncheck value =
    case value of
        Empty ->
            Empty

        Unchecked _ ->
            value

        Valid a ->
            Unchecked a

        Invalid a _ ->
            Unchecked a


{-| Return the actual value if it has been successfully validated at
least once.
-}
validValue : Validatable a comparable -> Maybe a
validValue value =
    case value of
        Empty ->
            Nothing

        Unchecked _ ->
            Nothing

        Valid a ->
            Just a

        Invalid _ _ ->
            Nothing


{-| Return all validation errors if the value has been tried to be
validated at least once.
-}
errors : Validatable a comparable -> Maybe (Set comparable)
errors value =
    case value of
        Empty ->
            Nothing

        Unchecked _ ->
            Nothing

        Valid _ ->
            Nothing

        Invalid _ errors ->
            Just errors


{-| Check if the string value is non-empty. The first argument is the
error which is recorded if the value is empty.

    (unchecked "I am not empty!"
        |> isNotEmpty "the value must not be empty"
        |> validValue
    )
        == Just "I am not empty!"

    (unchecked ""
        |> isNotEmpty "the value must not be empty"
        |> errors
        |> Maybe.map toList
    )
        == Just [ "the value must not be empty" ]

-}
isNotEmpty : comparable -> Validatable String comparable -> Validatable String comparable
isNotEmpty error value =
    value |> satisfies (not << String.isEmpty) error


{-| Check if the string value is at least 6 characters long.
-}
atLeast : Int -> comparable -> Validatable String comparable -> Validatable String comparable
atLeast minimalLength error value =
    value |> satisfies (\string -> minimalLength <= String.length string) error


{-| Check if the string value only consists of letters.
-}
consistsOfLetters : comparable -> Validatable String comparable -> Validatable String comparable
consistsOfLetters error value =
    let
        onlyLetters string =
            string
                |> String.toLower
                |> String.toList
                |> List.all Char.isLower
    in
    value |> satisfies onlyLetters error


{-| Check if the string value is a proper email address.
-}
isEmail : comparable -> Validatable String comparable -> Validatable String comparable
isEmail error value =
    let
        validEmail string =
            string |> Regex.contains emailRegex

        emailRegex =
            Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                |> Regex.caseInsensitive
    in
    value |> satisfies validEmail error


{-| Check if the value satisfies the condition. If not add the provided
error to the list of errors.

**Note:** If the value was in an invalid state before and satisfies the
condition we drop the previous errors and return a valid value.

-}
satisfies : (a -> Bool) -> comparable -> Validatable a comparable -> Validatable a comparable
satisfies condition error value =
    case value of
        Empty ->
            Empty

        Unchecked a ->
            if condition a then
                Valid a
            else
                Invalid a (Set.singleton error)

        Valid a ->
            if condition a then
                value
            else
                Invalid a (Set.singleton error)

        Invalid a prevErrors ->
            if condition a then
                value
            else
                Invalid a (Set.insert error prevErrors)


{-| Given a reference value, check if the value equals it. The check is
only performed if the reference value was successfully validated. If the
reference value is empty or invalid we switch back the value to the unchecked
state, no matter what.
-}
equals :
    Validatable a comparable
    -> comparable
    -> Validatable a comparable
    -> Validatable a comparable
equals reference error value =
    case reference of
        Empty ->
            value |> uncheck

        Unchecked _ ->
            value

        Valid referenceA ->
            value |> satisfies ((==) referenceA) error

        Invalid _ _ ->
            value |> uncheck
