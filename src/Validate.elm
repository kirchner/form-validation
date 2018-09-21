module Validate
    exposing
        ( Validatable
        , addErrors
        , atLeast
        , consistsOfLetters
        , empty
        , equals
        , errors
        , isEmail
        , isFloat
        , isInt
        , isNotEmpty
        , map
        , mapErrors
        , maybe
        , rawValue
        , satisfies
        , try
        , uncheck
        , unchecked
        , valid
        , validValue
        , with
        )

{-|


# Creating validatable values

@docs Validatable

@docs empty, unchecked, uncheck, valid


# Getting information

@docs validValue, errors, rawValue


# Simple string validations

@docs isNotEmpty, atLeast, consistsOfLetters, isEmail


# Validations involving type casts

@docs isInt, isFloat


# Creating custom validations

@docs try, satisfies, equals, addErrors, map, mapErrors, maybe, with

-}

{-

   Copyright 2018 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

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
    | Invalid (Maybe a) (Set comparable)


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

        Invalid maybeA _ ->
            case maybeA of
                Just a ->
                    Unchecked a

                Nothing ->
                    --> TODO: do we want this behaviour?
                    Empty


{-| Create a valid value. Usefull for initializing with a default
value.
-}
valid : a -> Validatable a comparable
valid a =
    Valid a


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

        Invalid _ errorsSet ->
            Just errorsSet


{-| **I am not sure if it is a good idea to have this function at all,
so it may be removed in the future.**

Return the value, no matter if it is valid or not.

**Note:** If the value is in an invalid state, there may not be a raw
value anymore.

**Note:** Don't use this to extract the value for submitting the form.
Use `validValue` instead, to ensure at compile time that you only submit
valid values.

-}
rawValue : Validatable a comparable -> Maybe a
rawValue value =
    case value of
        Empty ->
            Nothing

        Unchecked a ->
            Just a

        Valid a ->
            Just a

        Invalid maybeA _ ->
            maybeA


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


{-| Check if the string value is at least the given count of characters long.
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
    value
        |> satisfies (Regex.contains emailRegex) error


emailRegex : Regex.Regex
emailRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            ("^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9]"
                ++ "(?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                ++ "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
            )


{-| Try to cast a `String` to an `Int`.
-}
isInt : comparable -> String -> Validatable Int comparable
isInt error input =
    input
        |> try (String.toInt >> Result.fromMaybe ()) (\_ -> error)


{-| Try to cast a `String` to a `Float`.
-}
isFloat : comparable -> String -> Validatable Float comparable
isFloat error input =
    input
        |> try (String.toFloat >> Result.fromMaybe ()) (\_ -> error)


{-| Run the provided computation which might result in an error. If it
succeeds, we get a valid value, if it fails we get an invalid state with
the given errors. So, you can do something like this:

    ("not a number"
        |> try String.toInt (\err -> "You must provide an integer: " ++ err)
        |> errors
        |> Maybe.map toList
    )
        == Just [ "You must provide an integer: could not convert ..." ]

-}
try : (a -> Result err b) -> (err -> comparable) -> a -> Validatable b comparable
try cast error value =
    case cast value of
        Err err ->
            Invalid Nothing (Set.singleton (error err))

        Ok casted ->
            Valid casted


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
                Invalid (Just a) (Set.singleton error)

        Valid a ->
            if condition a then
                value
            else
                Invalid (Just a) (Set.singleton error)

        Invalid maybeA prevErrors ->
            case maybeA of
                Just a ->
                    if condition a then
                        value
                    else
                        Invalid maybeA (Set.insert error prevErrors)

                Nothing ->
                    value


{-| Given a reference value, check if the value equals it.
-}
equals :
    a
    -> comparable
    -> Validatable a comparable
    -> Validatable a comparable
equals reference error value =
    value |> satisfies ((==) reference) error


{-| Add the given set of validation errors. This makes every value
which is not empty invalid. You can use this function if you need to
show validations to the user which can only be performed on the server,
for example checking if a username is still available.

**Note:** If the set is empty, we do not change the state of the value.

-}
addErrors : Set comparable -> Validatable a comparable -> Validatable a comparable
addErrors validationErrors value =
    if validationErrors |> Set.isEmpty then
        value
    else
        case value of
            Empty ->
                Empty

            Unchecked a ->
                Invalid (Just a) validationErrors

            Valid a ->
                Invalid (Just a) validationErrors

            Invalid maybeA previousErrors ->
                Invalid maybeA (Set.union validationErrors previousErrors)


{-| Apply the given function on the actual value.
-}
map : (a -> b) -> Validatable a comparable -> Validatable b comparable
map f value =
    case value of
        Empty ->
            Empty

        Unchecked a ->
            Unchecked (f a)

        Valid a ->
            Valid (f a)

        Invalid maybeA errorsSet ->
            Invalid (maybeA |> Maybe.map f) errorsSet


{-| Apply the given function on the error values.
-}
mapErrors :
    (comparableA -> comparableB)
    -> Validatable a comparableA
    -> Validatable a comparableB
mapErrors f value =
    case value of
        Empty ->
            Empty

        Unchecked a ->
            Unchecked a

        Valid a ->
            Valid a

        Invalid maybeA errorsSet ->
            Invalid maybeA (errorsSet |> Set.map f)


{-| Apply the validator on maybe values. If the value is `Nothing`, it
is left unchanged. This is usefull if you need to validate optional
arguments, for example

    nickname : Validatable (Maybe String) String
    nickname =
        valid Nothing

    (nickname
        |> maybe (consistsOfLetters "nickname must consist of letters only")
        |> validValue
    )
        == Just Nothing


    invalidNickname : Validatable (Maybe String) String
    invalidNickname =
        unchecked (Just "123")

    (invalidNickname
        |> maybe (consistsOfLetters "nickname must consist of letters only")
        |> errors
        |> Set.toList
    )
        == Just [ "nickname must consist of letters only" ]

-}
maybe :
    (Validatable a comparable -> Validatable a comparable)
    -> Validatable (Maybe a) comparable
    -> Validatable (Maybe a) comparable
maybe validator maybeValue =
    case maybeValue of
        Unchecked (Just a) ->
            Unchecked a
                |> validator
                |> map Just

        Valid (Just a) ->
            Valid a
                |> validator
                |> map Just

        Invalid (Just a) errorsSet ->
            Invalid a errorsSet
                |> validator
                |> map Just

        _ ->
            maybeValue


{-| Apply a validation only if another value was successfully validated.
This unchecks the value if the provided value was not valid. For
example, you can do something like this

    password : Validatable String String

    passwordCopy : Validatable String String
    passwordCopy =
        oldPasswordCopy
            |> Validate.with password
                (\validPassword ->
                    Validate.equals validPassword "Both passwords have to match up."
                )

-}
with :
    Validatable a comparable
    -> (a -> Validatable b comparable -> Validatable b comparable)
    -> Validatable b comparable
    -> Validatable b comparable
with reference validator value =
    case reference of
        Empty ->
            value |> uncheck

        Unchecked _ ->
            value |> uncheck

        Valid referenceA ->
            validator referenceA value

        Invalid _ _ ->
            value |> uncheck
