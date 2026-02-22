#!/usr/bin/env bash
# Copy the shared libraries depended upon by all the files in the given directory.

excludelist=$(cat scripts/excludelist | grep -o '^[^#]*')

# Copy the dynamic libraries depended upon by the object file given in the first argument.
# The second argument is the path to the original object file; this is required to prevent
# an infinite recurse stemming from a self-referential shared library.
function cp_sos {
    object_file="$1"
    echo "Patching $object_file";
    # Find all the shared library dependencies of the given object file
    ldd "$object_file" |
    # For each shared library depended upon by this object file
    while read -r shared_lib sep shared_lib_path mem_loc; do
        target_object_file="${object_file%/*}/$shared_lib"
        # Only process libraries that are not in the base system. Only do something if we
        # have not encountered this shared library yet
        if [[ "$sep" == "=>" ]] && [ ! -e $target_object_file ] &&
               { ! echo $excludelist | grep -w -q $shared_lib; }; then
            # Copy the shared library found into the release
            cp -v "$shared_lib_path" "$target_object_file";
            # Patch the given object file to point the shared library copy
            patchelf --add-rpath '$ORIGIN' "$object_file";
            # Recurse to handle the dependencies of the current shared library
            cp_sos $target_object_file;
        fi
    done
}

# Move to the directory with the binaries
cd $1
# Copy the shared libraries required by each file in the release
find . -type f -executable |
while read object_file; do
    # Recursively copy the shared libraries used by this object file and patch it
    cp_sos "$object_file";
done
