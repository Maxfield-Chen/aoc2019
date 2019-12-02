#!/bin/bash

stack new simple $1
cp stack.yaml $1/stack.yaml
cp template.cabal $1.cabal

# Fix incorrect executable name in template.cabal
sed -i "s/aoc2018/$1" $1/$1.cabal
