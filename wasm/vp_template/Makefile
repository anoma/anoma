cargo = $(env) cargo
rustup = $(env) rustup

# Linker flag "-s" for stripping (https://github.com/rust-lang/cargo/issues/3483#issuecomment-431209957)
build-release:
	# wasm is built into target/wasm32-unknown-unknown/release
	RUSTFLAGS='-C link-arg=-s' $(cargo) build --release --target wasm32-unknown-unknown

build:
	$(cargo) build --target wasm32-unknown-unknown

watch:
	$(cargo) watch

clean:
	$(cargo) clean && if [ -e $(wasm) ]; then rm $(wasm); fi

deps:
	$(rustup) target add wasm32-unknown-unknown

.PHONY : build-release build watch clean deps
