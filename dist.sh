#!/bin/bash
#
# dist
# ----
# Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
# Licence : BSD3
#
# Script to build the release

set -e

# Extract project parameters from _oasis
NAME=`oasis query Name 2> /dev/null`
VERSION=`oasis query Version 2> /dev/null`
PREFIX=$NAME-$VERSION
ARCHIVE=$(pwd)/$PREFIX.tar.gz

# Clean setup.data and other generated files.
make clean
make distclean

# Create a branch for the release
git checkout -b release-$VERSION

# Generate files
oasis setup

# Remove this script and dev-files
rm -f dist.sh opam .jenkins.sh

# Commit
git add --all --force
git commit
git tag $VERSION

git checkout master
