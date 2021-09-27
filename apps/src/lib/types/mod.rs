use std::collections::HashSet;

use anoma::proto::Intent;
use tokio::sync::oneshot::Sender;

#[derive(Debug)]
pub enum MatchmakerMessage {
    InjectTx(Vec<u8>),
    RemoveIntents(HashSet<Vec<u8>>),
    UpdateState(Vec<u8>),
    ApplyIntent(Intent, Sender<bool>),
}
