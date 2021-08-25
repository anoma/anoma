# Logging

Options to consider:
- env_logger
- slog
- tracing

The current preference is for tracing in combination with tracing-subscriber (to log collected events and traces), because we have some async and parallelized code. In future, we should also add tracing-appender for rolling file logging.

## Env_logger

<https://github.com/env-logger-rs/env_logger/>

A simple logger used by many Rust tools, configurable by env vars. Usually combined with [pretty-env-logger](https://github.com/seanmonstar/pretty-env-logger).

## Slog

<https://github.com/slog-rs/slog>

Composable, structured logger. Many extra libraries with extra functionality, e.g.:
- <https://github.com/slog-rs/envlogger> port of env_logger as a slog-rs drain 

## Tracing

<https://github.com/tokio-rs/tracing>

Tracing & logging better suited for concurrent processes and async code. Many extra libraries with extra functionality, e.g.:
- <https://github.com/tokio-rs/tracing/tree/master/tracing-appender> non-blocking log appender
- <https://github.com/tokio-rs/tracing/tree/master/tracing-log> allows to forward library log statements and to use this in combination with env_logger
