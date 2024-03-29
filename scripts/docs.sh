mix docs

echo "var versionNodes = [" > ./doc/.doc-versions.js
app=`mix run -e 'IO.puts(Mix.Project.config()[:app])'`; \
for v in $(git tag | tac); do echo "{version: \"$v\", url: \"https://anoma.github.io/anoma/$v/\"}," >> ./doc/.doc-versions.js; done
echo "]" >> ./doc/.doc-versions.js

cat << EOF >> ./doc/.doc-versions.js

function comparePartials(versionA, versionB) {
    let a = versionA.version;
    let b = versionB.version;
    if (a === b) {
        return 0;
    }
    let splitA = a.split('.');
    let splitB = b.split('.');
    const length = Math.max(splitA.length, splitB.length);
    for (let i = 0; i < length; i++) {
        if (parseInt(splitA[i]) > parseInt(splitB[i]) ||
            ((splitA[i] === splitB[i]) && isNaN(splitB[i + 1]))) {
            return 1;
        }
        if (parseInt(splitA[i]) < parseInt(splitB[i]) ||
            ((splitA[i] === splitB[i]) && isNaN(splitA[i + 1]))) {
            return -1;
        }
    }
}

versionNodes = versionNodes.sort(comparePartials).reverse()
EOF
