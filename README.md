# Form validation in elm

This is a collection of functions to add form validations to your elm
application.  There is also
a [demo](https://kirchner.github.io/form-validation).  I'm happy for any
feedback!


## Example

Suppose your application has a sign up form and you want to start
validating the user input before sending the actual request to the
server.

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

How you present the possible validation errors to the user is completely
up to you. You can use `error` to extract all errors of a value if there
are any.

In this example, the parameters we actually need to submit will be

    type alias SignUpParams =
        { username : String
        , email : String
        , password : String
        }

and we can extract a valid set of parameters with

    signUpParams : Model -> Maybe SignUpParams
    signUpParams model =
        if Validate.validValue model.passwordCopy == Nothing then
            Nothing
        else
            Just SignUpParams
                |> collect model.username
                |> collect model.email
                |> collect model.password

    collect : Validatable a -> Maybe (a -> rest) -> Maybe rest
    collect validatable maybeCollector =
        Maybe.map2 (\f a -> f a) maybeCollector <|
            Validate.validValue validatable
