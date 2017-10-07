module Validate
    exposing
        ( Validated
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

{-| Create validatable forms.

@docs Validated


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

    type alias Model =
        { username : String
        , email : String
        , password : String
        , passwordCopy : String
        }

you have to change it to

    type alias Model =
        { username : Validated String String
        , email : Validated String String
        , password : Validated String String
        , passwordCopy : Validated String String
        }

-}
type Validated a comparable
    = Empty
    | Unchecked a
    | Valid a
    | Invalid a (Set comparable)


{-| Use this to initialize your values:

    init =
        { username = empty
        , email = empty
        , pasword = empty
        , passwordCopy = empty
        }

-}
empty : Validated a comparable
empty =
    Empty


{-| Use this to update a value in your model in reaction to user input:

    update msg model =
        case msg of
            SetUsername string ->
                { model | username = unchecked string }

            ...

-}
unchecked : a -> Validated a comparable
unchecked a =
    Unchecked a


{-| Set the value to unchecked if possible.
-}
uncheck : Validated a comparable -> Validated a comparable
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
validValue : Validated a comparable -> Maybe a
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
errors : Validated a comparable -> Maybe (Set comparable)
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
isNotEmpty : comparable -> Validated String comparable -> Validated String comparable
isNotEmpty error value =
    value |> satisfies (not << String.isEmpty) error


{-| Check if the string value is at least 6 characters long.
-}
atLeast : Int -> comparable -> Validated String comparable -> Validated String comparable
atLeast minimalLength error value =
    value |> satisfies (\string -> minimalLength <= String.length string) error


{-| Check if the string value only consists of letters.
-}
consistsOfLetters : comparable -> Validated String comparable -> Validated String comparable
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
isEmail : comparable -> Validated String comparable -> Validated String comparable
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
satisfies : (a -> Bool) -> comparable -> Validated a comparable -> Validated a comparable
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
    Validated a comparable
    -> comparable
    -> Validated a comparable
    -> Validated a comparable
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
