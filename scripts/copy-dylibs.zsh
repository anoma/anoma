#!/usr/bin/env zsh
# Copy the shared libraries depended upon by all the files in the given directory.

# Copy the dynamic libraries depended upon by the object file given in the first argument.
# The second argument is the path to the original object file; this is required to prevent
# an infinite recurse stemming from a self-referential shared library.
function cp_dylibs {
    object_file="$1"
    echo "Patching $object_file";
    # Find all the shared library dependencies of the given object file
    otool -L "$object_file" |
    # Select only those shared libraries that are added to the base system
    grep -o -e "/usr/local/opt/.*dylib" -e "/opt/.*dylib" |
    # For each shared library that has been added to the base system
    while read -r shared_library; do
        shared_lib_basename="$(basename $shared_library)"
        target_object_file="${object_file%/*}/$shared_lib_basename"
        # Only do something if we have not encountered this shared library yet
        if ! [ -e $target_object_file ]; then
            # Copy the shared library found into the release
            cp -v "$shared_library" "$target_object_file";
            # Patch the given object file to point the shared library copy
            install_name_tool -change "$shared_library" "@loader_path/$shared_lib_basename" "$object_file";
            # The above patching invalidates any existing signatures, so force ad-hoc signing of the object file
            codesign -s - -f "$object_file";
            # Recurse to handle the dependencies of the current shared library
            cp_dylibs "$target_object_file"
        fi
    done
}

# Move to the directory with the binaries
cd $1
# Copy the shared libraries required by each file in the release
find . -type f |
while read object_file; do
    # Recursively copy the shared libraries used by this object file and patch them
    cp_dylibs "$object_file";
done
