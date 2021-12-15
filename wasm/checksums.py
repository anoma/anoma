import json
import glob
import hashlib
import os

checksums = {}
for wasm in sorted(glob.glob("wasm/*.wasm")):
    basename = os.path.basename(wasm)
    file_name = os.path.splitext(basename)[0] if wasm.count(".") == 1 else os.path.splitext(basename)[0].split('.')[0]
    checksums["{}.wasm".format(file_name)] = "{}.{}.wasm".format(file_name, hashlib.sha256(open(wasm, "rb").read()).hexdigest())
    os.rename(wasm, 'wasm/{}'.format(checksums["{}.wasm".format(file_name)]))

updated_wasms = list(checksums.values())

for wasm in sorted(glob.glob("wasm/*.wasm")):
    basename = os.path.basename(wasm)
    if not basename in updated_wasms:
        os.remove(wasm)

json.dump(checksums, open("wasm/checksums.json", "w"), indent=4, sort_keys=True)