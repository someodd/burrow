#!/bin/bash

# Prepare a commit/tag for release

# Stop the script if a command fails.
set -e

# Suggest a version.
#
# Get the current version from package.yaml and then suggest a new version which simply
# bumps the minor version.
CURRENT_VERSION=$(awk '/^version:/ {print $2}' package.yaml)
SUGGESTED_VERSION=$(echo $CURRENT_VERSION | awk -F. '{print $1"."$2+1".0.0"}')
# Ask the user for the version, defaulting to the suggestion.
read -p "Version [suggested: $SUGGESTED_VERSION]: " VERSION

# Update the version in package.yaml
sed -i "s/^version: .*/version: ${{ VERSION }}/" package.yaml

# Update the `CHANGELOG.md``
./update-changelog.sh ${{ VERSION }}

# Suggest the the tag command like `git tag -a v0.1.16.0 -m "release test + built-in spacecookie server"`
echo "git tag -a v${{ VERSION }} -m \"release test + built-in spacecookie server\""