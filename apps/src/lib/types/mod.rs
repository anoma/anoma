use std::collections::HashSet;
use std::sync::mpsc::Sender;

use anoma::proto::Intent;

#[derive(Debug)]
pub enum MatchmakerMessage {
    InjectTx(Vec<u8>),
    RemoveIntents(HashSet<Vec<u8>>),
    UpdateState(Vec<u8>),
    ApplyIntent(Intent, Sender<bool>),
}
