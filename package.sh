#!/bin/bash

# As of right now simply creates the Debian package. Plans for the future include
# creating packages for other distributions and maybe BSD as well.
#
# `fpm` to be installed.

# Stop the script if a command fails.
set -e

# Set the temporary package directory variable
TEMPORARY_PKG_DIR=package

# Extract the version from package.yaml
VERSION=$(awk '/^version:/ {print $2}' package.yaml)

# Build the Haskell project and copy the binaries
stack build --copy-bins --local-bin-path ./bin

# Create necessary directories in the temporary package directory
mkdir -p $TEMPORARY_PKG_DIR/usr/local/bin

# Copy the built binary to the temporary package directory
cp ./bin/burrow $TEMPORARY_PKG_DIR/usr/local/bin/burrow

# Run fpm to create the Debian package.
fpm -s dir -t deb -n burrow -v $VERSION \
  --description "Burrow gopherhole builder" \
  --maintainer "someodd <someodd@pm.me>" \
  --url "http://www.someodd.zip/showcase/burrow" \
  --license "GPL" \
  -C $TEMPORARY_PKG_DIR \
  usr/local/bin/burrow

# Clean up the temporary package directory
rm -rf $TEMPORARY_PKG_DIR
rm -rf ./bin
