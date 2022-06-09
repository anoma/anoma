# Error handling

The current preference is to use `thiserror` for most code and `eyre` for reporting errors at the CLI level and the client.

To make the code robust, we should avoid using code that may panic for errors that are recoverable and handle all possible errors explicitly. Two exceptions to this rule are:
- prototyping, where it's fine to use `unwrap`, `expect`, etc.
- in code paths with conditional compilation **only** for development build, where it's preferable to use `expect` in place of `unwrap` to help with debugging

In case of panics, we should provide an error trace that is helpful for trouble-shooting and debugging.

A great post on error handling library/application distinction: <https://nick.groenen.me/posts/rust-error-handling/>.

The considered libraries:
- thiserror
- anyhow
- eyre

The current preference is to use eyre at the outermost modules to print any encountered errors nicely back to the user and thiserror elsewhere.

## Thiserror

- <https://crates.io/crates/thiserror>

Macros for user-derived error types. Commonly used for library code.

## Anyhow

- <https://crates.io/crates/anyhow>

Easy error handling helpers. Commonly used for application code.

## Eyre

- <https://crates.io/crates/eyre>

Fork of `anyhow` with custom error reporting.
