#!/bin/bash

mkdir -p ./gh-pages
mkdir -p ./gh-pages/css
cp static/index.html gh-pages/index.html
cp static/css/* gh-pages/css
