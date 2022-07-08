use std::future::Future;

use tokio::task::JoinHandle;
use tokio::sync::mpsc::{self, UnboundedSender, UnboundedReceiver};

/// Serves to identify an aborting async task, which is spawned
/// with an [`Aborter`].
pub type AbortingTask = &'static str;

/// A panic-proof handle for aborting a future. Will abort during stack
/// unwinding and its drop method sends abort message with `who` inside it.
pub struct Aborter {
    abort_send: UnboundedSender<AbortingTask>,
    abort_recv: UnboundedReceiver<AbortingTask>,
}

impl Aborter {
    pub fn spawn_abortable<A, F, R>(&self, who: AbortingTask, abortable: A) -> JoinHandle<R>
    where
        A: FnOnce(AbortGuard) -> F,
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        let abort = AbortGuard {
            who,
            sender: self.abort_send.clone(),
        };
        tokio::spawn(abortable(abort))
    }

    /// This future will resolve when:
    ///   1. User sends a shutdown signal
    ///   2. One of the child processes terminates, sending a message on `drop`
    pub async fn wait(self) -> AborterStatus {
        wait_for_abort(self.abort_recv).await
    }
}

/// A panic-proof handle for aborting a future. Will abort during stack
/// unwinding and its drop method sends abort message with `who` inside it.
pub struct AbortGuard {
    sender: mpsc::UnboundedSender<&'static str>,
    who: &'static str,
}

impl Drop for AbortGuard {
    fn drop(&mut self) {
        // Send abort message, ignore result
        let _ = self.sender.send(self.who);
    }
}

/// Function that blocks until either
///   1. User sends a shutdown signal
///   2. One of the child processes terminates, sending a message on `drop`
/// Returns a boolean to indicate which scenario occurred.
/// `true` means that the latter happened
///
/// It is used by the [`Aborter`].
#[cfg(unix)]
async fn wait_for_abort(
    mut abort_recv: UnboundedReceiver<AbortingTask>,
) -> AborterStatus {
    use tokio::signal::unix::{signal, SignalKind};
    let mut sigterm = signal(SignalKind::terminate()).unwrap();
    let mut sighup = signal(SignalKind::hangup()).unwrap();
    let mut sigpipe = signal(SignalKind::pipe()).unwrap();
    tokio::select! {
        signal = tokio::signal::ctrl_c() => {
            match signal {
                Ok(()) => tracing::info!("Received interrupt signal, exiting..."),
                Err(err) => tracing::error!("Failed to listen for CTRL+C signal: {}", err),
            }
        },
        signal = sigterm.recv() => {
            match signal {
                Some(()) => tracing::info!("Received termination signal, exiting..."),
                None => tracing::error!("Termination signal cannot be caught anymore, exiting..."),
            }
        },
        signal = sighup.recv() => {
            match signal {
                Some(()) => tracing::info!("Received hangup signal, exiting..."),
                None => tracing::error!("Hangup signal cannot be caught anymore, exiting..."),
            }
        },
        signal = sigpipe.recv() => {
            match signal {
                Some(()) => tracing::info!("Received pipe signal, exiting..."),
                None => tracing::error!("Pipe signal cannot be caught anymore, exiting..."),
            }
        },
        msg = abort_recv.recv() => {
            // When the msg is `None`, there are no more abort senders, so both
            // Tendermint and the shell must have already exited
            if let Some(who) = msg {
                 tracing::info!("{} has exited, shutting down...", who);
            }
            return AborterStatus::ChildProcessTerminated;
        }
    };
    AborterStatus::UserShutdownLedger
}

#[cfg(windows)]
async fn wait_for_abort(
    mut abort_recv: UnboundedReceiver<AbortingTask>,
) -> AborterStatus {
    let mut sigbreak = tokio::signal::windows::ctrl_break().unwrap();
    let _ = tokio::select! {
        signal = tokio::signal::ctrl_c() => {
            match signal {
                Ok(()) => tracing::info!("Received interrupt signal, exiting..."),
                Err(err) => tracing::error!("Failed to listen for CTRL+C signal: {}", err),
            }
        },
        signal = sigbreak.recv() => {
            match signal {
                Some(()) => tracing::info!("Received break signal, exiting..."),
                None => tracing::error!("Break signal cannot be caught anymore, exiting..."),
            }
        },
        msg = abort_recv.recv() => {
            // When the msg is `None`, there are no more abort senders, so both
            // Tendermint and the shell must have already exited
            if let Some(who) = msg {
                 tracing::info!("{} has exited, shutting down...", who);
            }
            return AborterStatus::ChildProcessTerminated;
        }
    };
    AborterStatus::UserShutdownLedger
}

#[cfg(not(any(unix, windows)))]
async fn wait_for_abort(
    mut abort_recv: UnboundedReceiver<AbortingTask>,
) -> AborterStatus {
    let _ = tokio::select! {
        signal = tokio::signal::ctrl_c() => {
            match signal {
                Ok(()) => tracing::info!("Received interrupt signal, exiting..."),
                Err(err) => tracing::error!("Failed to listen for CTRL+C signal: {}", err),
            }
        },
        msg = abort_recv.recv() => {
            // When the msg is `None`, there are no more abort senders, so both
            // Tendermint and the shell must have already exited
            if let Some(who) = msg {
                 tracing::info!("{} has exited, shutting down...", who);
            }
            return AborterStatus::ChildProcessTerminated;
        }
    };
    AborterStatus::UserShutdownLedger
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AborterStatus {
    /// The ledger process received a shutdown signal.
    UserShutdownLedger,
    /// One of the child processes terminates, signaling the [`Aborter`].
    ChildProcessTerminated,
}

impl AborterStatus {
    /// Checks if the reason for aborting was a child process terminating.
    pub fn child_terminated(self) -> bool {
        matches!(self, AborterStatus::ChildProcessTerminated)
    }
}
