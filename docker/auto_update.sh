#!/bin/bash

# Script for automatically updating Burrow.
set -e

echo "auto update script"

# Local repository path (this path is hardcoded)
LOCAL_GOPHERHOLE_REPO_PATH="/srv/git/gopherhole.git"

# GitHub API endpoint for latest release information
GITHUB_API_URL="https://api.github.com/repos/someodd/burrow/releases/latest"

if [[ "${AUTO_BURROW_UPGRADE}" == "true" ]]; then
    # Check the local Burrow version
    LOCAL_BURROW_VERSION="v$(burrow --version)"

    # Fetch the latest release information from GitHub
    LATEST_RELEASE=$(wget -qO- "$GITHUB_API_URL")

    # Extract the latest release version from the fetched data
    LATEST_BURROW_VERSION=$(echo "$LATEST_RELEASE" | grep -Po '"tag_name": "\K.*?(?=")')

    # Compare the local and latest Burrow versions
    if [ "$LOCAL_BURROW_VERSION" != "$LATEST_BURROW_VERSION" ]; then
        echo "The local Burrow version is NOT up-to-date with the latest release."
        echo "Local Burrow version: $LOCAL_BURROW_VERSION"
        echo "Latest Burrow version: $LATEST_BURROW_VERSION"
        /usr/local/bin/install-latest-deb.sh
    fi
fi

# BELOW IS FOR SEEING IF THE LOCAL GOPHERHOLE IS UP TO DATE WITH THE REMOTE
# IF IT'S NOT THEN WE WILL SYNCH IT.
git config --global --add safe.directory "$LOCAL_GOPHERHOLE_REPO_PATH"

is_remote_newer() {
    echo "checking if remote is newer"
    # Return 0 if the remote repository is newer than the local repository, 1 otherwise.
    local local_repo="$1"
    local remote_repo="$2"
    local branch="${3:-main}"
    local local_date
    local remote_date

    # Get the latest commit timestamp of the local repository or default to 0 so that the
    # remote is always considered newer
    if [ -d "$local_repo/.git" ]; then
        # Get the latest commit timestamp of the local repository
        local_date=$(git -C "$local_repo" log -1 --format=%ct)
    else
        # Default to 0 if the local repository doesn't exist
        local_date=0
    fi

    # Get the latest commit timestamp of the remote repository
    echo "try date"
    remote_date=$(mkdir -p /tmp/temp-repo && \
                  cd /tmp/temp-repo && \
                  git init -q && \
                  git fetch --depth=1 "$remote_repo" "refs/heads/$branch" && \
                  git log -1 --format=%ct FETCH_HEAD && \
                  cd - > /dev/null && \
                  rm -rf /tmp/temp-repo)
    echo "got date"

    # Compare timestamps and return True if the remote is newer
    if [[ "$remote_date" -gt "$local_date" ]]; then
        return 0
    else
        return 1
    fi
}

echo "rest of script: checking if remote is newer"
cd "$LOCAL_GOPHERHOLE_REPO_PATH"
is_remote_newer "$LOCAL_GOPHERHOLE_REPO_PATH" "$GOPHERHOLE_REMOTE_URL" "$GOPHERHOLE_REMOTE_BRANCH"
is_newer=$?
echo "status of is_newer: $is_newer"

if [[ -n $GOPHERHOLE_REMOTE_URL ]] && [[ $is_newer -eq 0 ]]; then
    echo "Remote repository is newer. Start sync."
    rm -rf /tmp/gopherhole-clone
    git fetch "$GOPHERHOLE_REMOTE_URL" "$GOPHERHOLE_REMOTE_BRANCH:$GOPHERHOLE_REMOTE_BRANCH" --force
    "$LOCAL_GOPHERHOLE_REPO_PATH/hooks/post-receive"
fi

# Reset perms
chown -R git:git /srv/git
chown -R git:git /srv/gopher
chown -R git:git /srv