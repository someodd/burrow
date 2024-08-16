#!/bin/bash

# Script for automatically keeping the gopherhole git repository synchronized with its
# "mirror" on GitHub. This script also ensures that burrow is up-to-date.
set -e

# Remote repository URL
REMOTE_GOPHERHOLE_REPO_URL="https://github.com/$GITHUB_REPO_USER/$GITHUB_REPO_REPO"

# Local repository path
LOCAL_GOPHERHOLE_REPO_PATH="/srv/git/gopherhole.git"

# Check if /etc/skip_burrow_updates is 1
if [[ "${SKIP_BURROW_UPDATES}" == "1" ]]; then
    echo "Skipping Burrow update check"
else
    # Check the local Burrow version
    LOCAL_BURROW_VERSION="v$(burrow --version)"

    # Fetch the latest release information from GitHub
    LATEST_RELEASE=$(wget -qO- https://api.github.com/repos/someodd/burrow/releases/latest)

    # Extract the latest release version from the fetched data
    LATEST_BURROW_VERSION=$(echo $LATEST_RELEASE | grep -Po '"tag_name": "\K.*?(?=")')

    # Compare the local and latest Burrow versions
    if [ "$LOCAL_BURROW_VERSION" != "$LATEST_BURROW_VERSION" ]; then
        echo "The local Burrow version is NOT up-to-date with the latest release."
        echo "Local Burrow version: $LOCAL_BURROW_VERSION"
        echo "Latest Burrow version: $LATEST_BURROW_VERSION"
        /tmp/latest-deb.sh
    else
        echo "The local Burrow version is up-to-date with the latest release."
    fi
fi

# Update repo?
git config --global --add safe.directory "$LOCAL_GOPHERHOLE_REPO_PATH"

LATEST_REMOTE_COMMIT=$(wget -qO- "https://api.github.com/repos/$GITHUB_REPO_USER/$GITHUB_REPO_REPO/commits/$REPO_BRANCH" | 
    grep -m 1 '"sha":' | 
    sed 's/.*"sha": "\([^"]*\)".*/\1/')

# Navigate to the local repository
cd $LOCAL_GOPHERHOLE_REPO_PATH || { echo "Failed to navigate to $LOCAL_GOPHERHOLE_REPO_PATH"; exit 1; }

# Get the latest commit hashes for the local and remote branches
LOCAL_COMMIT=$(git rev-parse $REPO_BRANCH || echo "failed")

# Compare the local and remote commit hashes
if [ "$LOCAL_COMMIT" == "failed" ] || [ "$LOCAL_COMMIT" != "$LATEST_REMOTE_COMMIT" ]; then
    echo "The local repository is not up-to-date with the remote repository."
    echo "Local commit: $LOCAL_COMMIT"
    echo "Remote commit: $LATEST_REMOTE_COMMIT"
    git fetch "$REMOTE_GOPHERHOLE_REPO_URL" "$REPO_BRANCH:$REPO_BRANCH" --force
    git clone "$LOCAL_GOPHERHOLE_REPO_PATH" /tmp/gopherhole-clone
    cd /tmp/gopherhole-clone
    burrow build
    pkill burrow
    cp /tmp/gopherhole-clone/data/spacecookie.json /tmp/spacecookie.json
    rm -rf /tmp/gopherhole-clone
    nohup burrow serve --config /tmp/spacecookie.json > /dev/null 2>&1 &
    chown -R git:git /srv/git/gopherhole.git
    chown -R git:git /srv/gopher
else
    echo "The local repository is up-to-date with the remote repository."
fi