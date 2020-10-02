#!/bin/sh

mkdir -p static
cp src/cli.js static/
elm make --optimize src/Main.elm --output=static/main.js
