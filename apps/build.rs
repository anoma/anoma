use std::fs::{read_to_string, File};
use std::io::Write;
use std::process::Command;
use std::{env, str};

use git2::{DescribeFormatOptions, DescribeOptions, Repository};

/// Path to the .proto source files, relative to `apps` directory
const PROTO_SRC: &str = "./proto";

/// The version should match the one we use in the `Makefile`
const RUSTFMT_TOOLCHAIN_SRC: &str = "../rust-nightly-version";

fn main() {
    #[cfg(all(feature = "ABCI", feature = "ABCI-plus-plus"))]
    compile_error!(
        "`ABCI` and `ABCI-plus-plus` may not be used at the same time"
    );

    // Discover the repository version, if it exists
    println!("cargo:rerun-if-changed=../.git");
    let describe_opts = DescribeOptions::new();
    let mut describe_format = DescribeFormatOptions::new();
    describe_format.dirty_suffix("-dirty");
    let repo = Repository::discover(".").ok();
    let describe = match &repo {
        Some(repo) => repo.describe(&describe_opts).ok(),
        None => None,
    };
    let version_string = match describe {
        Some(describe) => describe.format(Some(&describe_format)).ok(),
        None => None,
    };
    let mut version_rs =
        File::create("./version.rs").expect("cannot write version");
    let pre = "pub fn anoma_version() -> &'static str { \"";
    let post = "\" }";
    match version_string {
        Some(version_string) => {
            version_rs
                .write_all(pre.as_bytes())
                .expect("cannot write version");
            version_rs
                .write_all(version_string.as_bytes())
                .expect("cannot write version");
            version_rs
                .write_all(post.as_bytes())
                .expect("cannot write version");
        }
        None => {
            version_rs
                .write_all(pre.as_bytes())
                .expect("cannot write version");
            version_rs
                .write_all(env!("CARGO_PKG_VERSION").as_bytes())
                .expect("cannot write version");
            version_rs
                .write_all(post.as_bytes())
                .expect("cannot write version");
        }
    };

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
                }
            }
        }
    }

    tonic_build::configure()
        .out_dir("src/lib/proto/generated")
        .format(true)
        .extern_path(".types", "::anoma::proto::generated::types")
        // This warning appears in tonic generated code
        .server_mod_attribute(".", "#[allow(clippy::unit_arg)]")
        // TODO try to add json encoding to simplify use for user
        // .type_attribute("types.Intent", "#[derive(serde::Serialize,
        // serde::Deserialize)]")
        .compile(&[format!("{}/services.proto", PROTO_SRC)], &[PROTO_SRC])
        .unwrap();
}
