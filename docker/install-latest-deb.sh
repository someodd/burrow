#!/bin/bash

# Replace these with your GitHub username and repository name
GITHUB_USER="someodd"
REPO_NAME="burrow"

# Get the latest release information
LATEST_RELEASE=$(wget -qO- https://api.github.com/repos/$GITHUB_USER/$REPO_NAME/releases/latest)

# Extract the .deb asset URL
DEB_URL=$(echo $LATEST_RELEASE | grep -o '"browser_download_url": "[^"]*\.deb"' | grep -o 'https://[^"]*')

if [ -z "$DEB_URL" ]; then
    echo "No .deb file found in the latest release."
    exit 1
fi

# Extract the filename from the URL
FILENAME=$(basename $DEB_URL)

# Download the .deb file
wget -O "/tmp/burrow.deb" $DEB_URL

if [ $? -eq 0 ]; then
    echo "Successfully downloaded $FILENAME"
else
    echo "Failed to download $FILENAME"
    exit 1
fi

# Install the .deb file
apt-get install -y "/tmp/burrow.deb"