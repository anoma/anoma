mix docs

echo "var versionNodes = [" > ./doc/.doc-versions.js
for v in $(git tag | sort -Vr); do
    echo "{version: \"$v\", url: \"https://anoma.github.io/anoma/$v/\"}," >> ./doc/.doc-versions.js
done
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
        let partA = parseInt(splitA[i] || 0);
        let partB = parseInt(splitB[i] || 0);
        if (partA > partB) return 1;
        if (partA < partB) return -1;
    }
    return 0;
}

versionNodes = versionNodes.sort(comparePartials).reverse()
EOF
