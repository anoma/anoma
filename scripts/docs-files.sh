#!/bin/bash

# Destination folder where all files will be copied
destination_folder="./doc/files/"

mkdir -p $destination_folder
# Find all folders named "files" and copy their contents to the destination folder
find ./documentation -type d -name "files" -exec sh -c 'cp -r "$0"/* "$1"' {} "$destination_folder" \;

echo "All files copied and merged into $destination_folder"
