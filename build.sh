#!/bin/bash
set -o errexit -o nounset -o pipefail

rm -f ./target/*
mkdir -p ./target

pulp browserify --optimise --standalone 'shapecheck' > ./target/shapecheck.js
cp README.md LICENSE package.json ./target/