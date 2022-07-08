/// A panic-proof handle for aborting a future. Will abort during stack
/// unwinding and its drop method sends abort message with `who` inside it.
pub struct Aborter {
    pub(super) sender: tokio::sync::mpsc::UnboundedSender<&'static str>,
    pub(super) who: &'static str,
}

impl Drop for Aborter {
    fn drop(&mut self) {
        // Send abort message, ignore result
        let _ = self.sender.send(self.who);
    }
}
