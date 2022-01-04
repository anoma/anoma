//! Dynamic library helpers

/// The file extension for dynamic library for this target
#[allow(dead_code)]
#[cfg(windows)]
pub const FILE_EXT: &str = "dll";

/// The file extension for dynamic library for this target
#[allow(dead_code)]
#[cfg(target_os = "macos")]
pub const FILE_EXT: &str = "dylib";

/// The file extension for dynamic library for this target
#[allow(dead_code)]
#[cfg(all(unix, not(target_os = "macos")))]
pub const FILE_EXT: &str = "so";
