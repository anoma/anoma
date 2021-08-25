#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Tx {
    #[prost(bytes = "vec", tag = "1")]
    pub code: ::prost::alloc::vec::Vec<u8>,
    /// TODO this optional is useless because it's default on proto3
    #[prost(bytes = "vec", optional, tag = "2")]
    pub data: ::core::option::Option<::prost::alloc::vec::Vec<u8>>,
    #[prost(message, optional, tag = "3")]
    pub timestamp: ::core::option::Option<::prost_types::Timestamp>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Intent {
    #[prost(bytes = "vec", tag = "1")]
    pub data: ::prost::alloc::vec::Vec<u8>,
    #[prost(message, optional, tag = "2")]
    pub timestamp: ::core::option::Option<::prost_types::Timestamp>,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct IntentGossipMessage {
    /// TODO remove oneof because it's not used so far
    #[prost(oneof = "intent_gossip_message::Msg", tags = "1")]
    pub msg: ::core::option::Option<intent_gossip_message::Msg>,
}
/// Nested message and enum types in `IntentGossipMessage`.
pub mod intent_gossip_message {
    /// TODO remove oneof because it's not used so far
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Msg {
        #[prost(message, tag = "1")]
        Intent(super::Intent),
    }
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Dkg {
    #[prost(string, tag = "1")]
    pub data: ::prost::alloc::string::String,
}
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct DkgGossipMessage {
    #[prost(oneof = "dkg_gossip_message::DkgMessage", tags = "1")]
    pub dkg_message: ::core::option::Option<dkg_gossip_message::DkgMessage>,
}
/// Nested message and enum types in `DkgGossipMessage`.
pub mod dkg_gossip_message {
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum DkgMessage {
        #[prost(message, tag = "1")]
        Dkg(super::Dkg),
    }
}
