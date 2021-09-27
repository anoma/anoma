# script used only during CI execution.

git config user.email "gianmarco@heliax.dev"
git config user.name "Drone CI"

if [ -z "$(git status ':wasm/*.wasm' --porcelain)" ]; then 
    echo "No changes to commit."
    exit 0
fi

REMOTE=$(git remote get-url origin | cut -c 9-)
PUSH_URL="https://${GITHUB_TOKEN}@${REMOTE}"

git fetch origin $DRONE_SOURCE_BRANCH
git stash
git checkout $DRONE_SOURCE_BRANCH

if [ $? -ne 0 ]; then
    echo "Can't checkout $DRONE_SOURCE_BRANCH. Update wasm manually."
    exit 1
fi

git stash pop
git remote set-url origin $PUSH_URL

git add ':wasm/*.wasm'
git add ':wasm/*.lock'
git status
git commit -m "[ci]: update wasms"

git push

exit $?