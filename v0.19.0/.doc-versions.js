var versionNodes = [
{version: "v0.9.0", url: "https://anoma.github.io/anoma/v0.9.0/"},
{version: "v0.8.0", url: "https://anoma.github.io/anoma/v0.8.0/"},
{version: "v0.7.0", url: "https://anoma.github.io/anoma/v0.7.0/"},
{version: "v0.6.0", url: "https://anoma.github.io/anoma/v0.6.0/"},
{version: "v0.5.0", url: "https://anoma.github.io/anoma/v0.5.0/"},
{version: "v0.4.0", url: "https://anoma.github.io/anoma/v0.4.0/"},
{version: "v0.3.0", url: "https://anoma.github.io/anoma/v0.3.0/"},
{version: "v0.28.0", url: "https://anoma.github.io/anoma/v0.28.0/"},
{version: "v0.27.0", url: "https://anoma.github.io/anoma/v0.27.0/"},
{version: "v0.25.0", url: "https://anoma.github.io/anoma/v0.25.0/"},
{version: "v0.24.1", url: "https://anoma.github.io/anoma/v0.24.1/"},
{version: "v0.24.0", url: "https://anoma.github.io/anoma/v0.24.0/"},
{version: "v0.23.0", url: "https://anoma.github.io/anoma/v0.23.0/"},
{version: "v0.22.0", url: "https://anoma.github.io/anoma/v0.22.0/"},
{version: "v0.21.0", url: "https://anoma.github.io/anoma/v0.21.0/"},
{version: "v0.20.0", url: "https://anoma.github.io/anoma/v0.20.0/"},
{version: "v0.2.0", url: "https://anoma.github.io/anoma/v0.2.0/"},
{version: "v0.19.1", url: "https://anoma.github.io/anoma/v0.19.1/"},
{version: "v0.19.0", url: "https://anoma.github.io/anoma/v0.19.0/"},
{version: "v0.18.0", url: "https://anoma.github.io/anoma/v0.18.0/"},
{version: "v0.17.0", url: "https://anoma.github.io/anoma/v0.17.0/"},
{version: "v0.16.0", url: "https://anoma.github.io/anoma/v0.16.0/"},
{version: "v0.15.0", url: "https://anoma.github.io/anoma/v0.15.0/"},
{version: "v0.14.0", url: "https://anoma.github.io/anoma/v0.14.0/"},
{version: "v0.13.0", url: "https://anoma.github.io/anoma/v0.13.0/"},
{version: "v0.12.0", url: "https://anoma.github.io/anoma/v0.12.0/"},
{version: "v0.11.0", url: "https://anoma.github.io/anoma/v0.11.0/"},
{version: "v0.10.0", url: "https://anoma.github.io/anoma/v0.10.0/"},
{version: "v0.1.0", url: "https://anoma.github.io/anoma/v0.1.0/"},
]

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
