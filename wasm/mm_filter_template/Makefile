cargo = $(env) cargo
rustup = $(env) rustup

build-release:
	# wasm is built into target/wasm32-unknown-unknown/release
	$(cargo) build --release --target wasm32-unknown-unknown

build:
	$(cargo) build --target wasm32-unknown-unknown

watch:
	$(cargo) watch

clean:
	$(cargo) clean && if [ -e $(wasm) ]; then rm $(wasm); fi

deps:
	$(rustup) target add wasm32-unknown-unknown

.PHONY : build-release build watch clean deps
