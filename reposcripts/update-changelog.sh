#!/bin/bash

# A tool for release management.
#
# Automatically update the changelog for a new release.
#
# This includes setting [Unreleased] to the new version and managing the footnote links.

set -e

NEW_VERSION=$1

if [ -z "$NEW_VERSION" ]; then
    echo "Error: Version argument is required."
    exit 1
fi

# Remove 'v' prefix if present
NEW_VERSION=${NEW_VERSION#v}

# Get the current version
CURRENT_TAG=$(git describe --tags --abbrev=0 $(git rev-list --tags --max-count=1))
CURRENT_VERSION=${CURRENT_TAG#v}

# Get the date in YYYY-MM-DD format
RELEASE_DATE=$(date +%Y-%m-%d)

###################

# Update the Unreleased section to new version
sed -i "s/^## \[Unreleased\]/## [$NEW_VERSION] - $(date +%Y-%m-%d)/" CHANGELOG.md

# Add new Unreleased section
sed -i "/^## \[$NEW_VERSION\]/i## [Unreleased]\n" CHANGELOG.md

# Update the unreleased comparison link
sed -i "s|\[unreleased\]: .*|[unreleased]: https://github.com/someodd/burrow/compare/v$NEW_VERSION...HEAD|" CHANGELOG.md

# Add new version comparison link
sed -i "/\[unreleased\]: /a[$NEW_VERSION]: https://github.com/someodd/burrow/compare/v$CURRENT_VERSION...v$NEW_VERSION" CHANGELOG.md