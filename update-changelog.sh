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

# Get the date in YYYY-MM-DD format
RELEASE_DATE=$(date +%Y-%m-%d)

# Update the Changelog
sed -i '/## \[Unreleased\]/a\
\
## ['$NEW_VERSION'] - '$RELEASE_DATE'' CHANGELOG.md

sed -i 's/\[Unreleased\]: /['$NEW_VERSION']: /' CHANGELOG.md

sed -i '1i## [Unreleased]\
\
' CHANGELOG.md

# Get the previous tag
PREVIOUS_TAG=$(git describe --tags --abbrev=0 $(git rev-list --tags --skip=1 --max-count=1))

# Update the links at the bottom of the file
sed -i '6i\
[Unreleased]: https://github.com/username/repo/compare/v'$NEW_VERSION'...HEAD\
['$NEW_VERSION']: https://github.com/username/repo/compare/'$PREVIOUS_TAG'...v'$NEW_VERSION'' CHANGELOG.md

echo "Changelog updated for version $NEW_VERSION"