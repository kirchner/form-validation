#!/bin/bash

elm-live frontend/MainWithValidations.elm \
    --dir=gh-pages \
    --output=gh-pages/elm.js \
    --before-build=./before-build.sh \
    --debug \
    --warn

