use std::fs::read_to_string;
use std::process::Command;
use std::{env, str};

/// Path to the .proto source files, relative to `shared` directory
const PROTO_SRC: &str = "./proto";

/// The version should match the one we use in the `Makefile`
const RUSTFMT_TOOLCHAIN_SRC: &str = "../rust-nightly-version";

fn main() {
    #[cfg(all(feature = "ABCI", feature = "ABCI-plus-plus"))]
    compile_error!(
        "`ABCI` and `ABCI-plus-plus` may not be used at the same time"
    );
    if let Ok(val) = env::var("COMPILE_PROTO") {
        if val.to_ascii_lowercase() == "false" {
            // Skip compiling proto files
            return;
        }
    }

    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed={}", PROTO_SRC);

    // Tell Cargo to build when the `ANOMA_DEV` env var changes
    println!("cargo:rerun-if-env-changed=ANOMA_DEV");
    // Enable "dev" feature if `ANOMA_DEV` is trueish
    if let Ok(dev) = env::var("ANOMA_DEV") {
        if dev.to_ascii_lowercase().trim() == "true" {
            println!("cargo:rustc-cfg=feature=\"dev\"");
        }
    }

    let mut use_rustfmt = false;

    // The version should match the one we use in the `Makefile`
    if let Ok(rustfmt_toolchain) = read_to_string(RUSTFMT_TOOLCHAIN_SRC) {
        // Try to find the path to rustfmt.
        if let Ok(output) = Command::new("rustup")
            .args(&[
                "which",
                "rustfmt",
                "--toolchain",
                rustfmt_toolchain.trim(),
            ])
            .output()
        {
            if let Ok(rustfmt) = str::from_utf8(&output.stdout) {
                // Set the command to be used by tonic_build below to format the
                // generated files
                let rustfmt = rustfmt.trim();
                if !rustfmt.is_empty() {
                    println!("using rustfmt from path \"{}\"", rustfmt);
                    env::set_var("RUSTFMT", rustfmt);
                    use_rustfmt = true
                }
            }
        }
    }

    tonic_build::configure()
        .out_dir("src/proto/generated")
        .format(use_rustfmt)
        // TODO try to add json encoding to simplify use for user
        // .type_attribute("types.Intent", "#[derive(serde::Serialize,
        // serde::Deserialize)]")
        .protoc_arg("--experimental_allow_proto3_optional")
        .compile(&[format!("{}/types.proto", PROTO_SRC)], &[PROTO_SRC])
        .unwrap();
}
