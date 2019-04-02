#! /usr/bin/env bash

# Setup a folder.
rm -rf submission downloads
mkdir -p submission/src downloads
cp -r submission_template/* submission/

# Get Code
cp -r ../src/Language submission/src
cp ../app/Main.hs submission/src
cp -r cw-automarker/*.spl submission/

# Get Megaparsec
git clone https://github.com/mrkkrp/megaparsec.git downloads/megaparsec
cp -r downloads/megaparsec/Text submission/src

# Get Parser-Combinators
git clone https://github.com/mrkkrp/parser-combinators.git downloads/parser-combinators
cp -r downloads/parser-combinators/Control submission/src