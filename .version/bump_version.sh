#!/bin/bash

# Read the current version from a VERSION file
current_version=$(cat .version/version)

# Increment the patch version (you can customize this logic)
new_version=$(echo $current_version | awk -F. '{print $1"."$2"."$3+1}')

# Update the VERSION file with the new version
echo $new_version > .version/version

# Commit the change
git commit -am "Bump version to $new_version"

# Create a new tag
git tag -a $new_version -m "Version $new_version"

# Push the commit and tag
#git push
#git push --tags
