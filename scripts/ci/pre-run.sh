# script used only during CI execution.

# we don't want to skip pipeline execution if event is push
if [[ "push" == "$DRONE_BUILD_EVENT" ]]; then
  exit 0
fi

# TODO: need to fetch previous build exit code
# COMMIT_MESSAGE=$(git show -s --format=%B ${DRONE_COMMIT_SHA})
# CHECK="[ci]"

# case "$COMMIT_MESSAGE" in 
#   *"$CHECK"*)
#     echo "Skipping pipeline."
#     exit 78
#     ;;
# esac
