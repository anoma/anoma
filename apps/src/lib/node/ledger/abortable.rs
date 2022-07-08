use std::future::Future;

use tokio::task::JoinHandle;
use tokio::sync::mpsc::{self, UnboundedSender, UnboundedReceiver};

/// Serves to identify an aborting async task, which is spawned
/// with an [`AbortableSpawner`].
pub type AbortingTask = &'static str;

/// An [`AbortableSpawner`] will spawn abortable tasks into the asynchronous runtime.
pub struct AbortableSpawner {
    abort_send: UnboundedSender<AbortingTask>,
    abort_recv: UnboundedReceiver<AbortingTask>,
    cleanup_jobs: Vec<Box<dyn FnOnce()>>,
}

/// Contains the state of an on-going [`AbortableSpawner`] task spawn.
pub struct WithCleanup<'a, A> {
    who: AbortingTask,
    abortable: A,
    spawner: &'a mut AbortableSpawner,
}

impl AbortableSpawner {
    /// Creates a new [`AbortableSpawner`].
    pub fn new() -> Self {
        let (abort_send, abort_recv) = mpsc::unbounded_channel();
        Self {
            abort_send,
            abort_recv,
            cleanup_jobs: Vec::new(),
        }
    }

    /// Spawns a new task into the asynchronous runtime, with an [`Aborter`] that shall
    /// be dropped when it is no longer running.
    ///
    /// For instance:
    ///
    /// ```rust
    /// let spawner = AbortableSpawner::new();
    /// spawner
    ///     .spawn_abortable("ExampleTask", |aborter| async {
    ///         drop(aborter);
    ///         println!("I have signaled a control task that I am no longer running!");
    ///     })
    ///     .with_no_cleanup();
    /// ```
    ///
    /// The return type of this method is [`WithCleanup`], such that a cleanup routine, after the
    /// abort is received, can be executed.
    pub fn spawn_abortable<'a, A>(&'a mut self, who: AbortingTask, abortable: A) -> WithCleanup<'a, A> {
        WithCleanup {
            who,
            abortable,
            spawner: self,
        }
    }

    /// This future will resolve when:
    ///
    ///   1. User sends a shutdown signal
    ///   2. One of the child processes terminates, sending a message on `drop`
    ///
    /// These two scenarios are represented by the [`AborterStatus`] enum.
    pub async fn wait_for_abort(self) -> AborterStatus {
        let status = wait_for_abort(self.abort_recv).await;

        for job in self.cleanup_jobs {
            todo!()
        }

        status
    }

    /// This method is responsible for actually spawning the async task into the runtime.
    fn spawn_abortable_task<A, F, R>(&self, who: AbortingTask, abortable: A) -> JoinHandle<R>
    where
        A: FnOnce(Aborter) -> F,
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        let abort = Aborter {
            who,
            sender: self.abort_send.clone(),
        };
        tokio::spawn(abortable(abort))
    }
}

impl<'a, A> WithCleanup<'a, A> {
    pub fn with_no_cleanup<F, R>(self) -> JoinHandle<R>
    where
        A: FnOnce(Aborter) -> F,
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        self.spawner.spawn_abortable_task(self.who, self.abortable)
    }

    pub fn with_conditional_cleanup<F, R, C>(self, cond: bool, cleanup: C) -> JoinHandle<R>
    where
        A: FnOnce(Aborter) -> F,
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
        C: FnOnce() + 'static,
    {
        if cond {
            self.spawner.cleanup_jobs.push(Box::new(cleanup));
        }
        self.with_no_cleanup()
    }

    pub fn with_cleanup<F, R, C>(self, cleanup: C) -> JoinHandle<R>
    where
        A: FnOnce(Aborter) -> F,
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
        C: FnOnce() + 'static,
    {
        self.with_conditional_cleanup(true, cleanup)
    }
}

/// A panic-proof handle for aborting a future. Will abort during stack
/// unwinding and its drop method sends abort message with `who` inside it.
pub struct Aborter {
    sender: mpsc::UnboundedSender<&'static str>,
    who: &'static str,
}

impl Drop for Aborter {
    fn drop(&mut self) {
        // Send abort message, ignore result
        let _ = self.sender.send(self.who);
    }
}

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

/// An [`AborterStatus`] represents one of two possible causes that resulted
/// in shutting down the ledger.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AborterStatus {
    /// The ledger process received a shutdown signal.
    UserShutdownLedger,
    /// One of the ledger's child processes terminated, signaling the [`AbortableSpawner`].
    ChildProcessTerminated,
}

impl AborterStatus {
    /// Checks if the reason for aborting was a child process terminating.
    pub fn child_terminated(self) -> bool {
        matches!(self, AborterStatus::ChildProcessTerminated)
    }
}
