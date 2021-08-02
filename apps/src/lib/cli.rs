//! The CLI commands that are re-used between the executables `anoma`,
//! `anoma-node` and `anoma-client`.
//!
//! The `anoma` executable groups together the most commonly used commands
//! inlined from the node and the client. The other commands for the node or the
//! client can be dispatched via `anoma node ...` or `anoma client ...`,
//! respectively.

use clap::{AppSettings, ArgMatches};

use super::config;
mod utils;
use utils::*;

const AUTHOR: &str = "Heliax AG <hello@heliax.dev>";
const APP_NAME: &str = "Anoma";
const CLI_VERSION: &str = "0.1.0";
const NODE_VERSION: &str = "0.1.0";
const CLIENT_VERSION: &str = "0.1.0";

// Main Anoma sub-commands
const NODE_CMD: &str = "node";
const CLIENT_CMD: &str = "client";

pub mod cmds {
    use clap::AppSettings;

    use super::utils::*;
    use super::{args, ArgMatches, CLIENT_CMD, NODE_CMD};

    /// Commands for `anoma` binary.
    #[derive(Debug)]
    pub enum Anoma {
        Node(AnomaNode),
        Client(AnomaClient),
        // Inlined commands from the node and the client.
        Ledger(Ledger),
        Gossip(Gossip),
        TxCustom(TxCustom),
        TxTransfer(TxTransfer),
        TxUpdateVp(TxUpdateVp),
        Intent(Intent),
    }
    impl Cmd for Anoma {
        fn add_sub(app: App) -> App {
            app.subcommand(AnomaNode::def())
                .subcommand(AnomaClient::def())
                .subcommand(Ledger::def())
                .subcommand(Gossip::def())
                .subcommand(TxCustom::def())
                .subcommand(TxTransfer::def())
                .subcommand(TxUpdateVp::def())
                .subcommand(Intent::def())
        }

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            let node = SubCmd::parse(matches).map_fst(Self::Node);
            let client = SubCmd::parse(matches).map_fst(Self::Client);
            let ledger = SubCmd::parse(matches).map_fst(Self::Ledger);
            let gossip = SubCmd::parse(matches).map_fst(Self::Gossip);
            let tx_custom = SubCmd::parse(matches).map_fst(Self::TxCustom);
            let tx_transfer = SubCmd::parse(matches).map_fst(Self::TxTransfer);
            let tx_update_vp = SubCmd::parse(matches).map_fst(Self::TxUpdateVp);
            let intent = SubCmd::parse(matches).map_fst(Self::Intent);
            node.or(client)
                .or(ledger)
                .or(gossip)
                .or(tx_custom)
                .or(tx_transfer)
                .or(tx_update_vp)
                .or(intent)
        }
    }

    /// Used as top-level commands (`Cmd` instance) in `anoman` binary.
    /// Used as sub-commands (`SubCmd` instance) in `anoma` binary.
    #[derive(Debug)]
    pub enum AnomaNode {
        Ledger(Ledger),
        // Boxed, because it's larger than other variants
        Gossip(Box<Gossip>),
        Config(Config),
    }
    impl Cmd for AnomaNode {
        fn add_sub(app: App) -> App {
            app.subcommand(Ledger::def())
                .subcommand(Gossip::def())
                .subcommand(Config::def())
        }

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            let ledger = SubCmd::parse(matches).map_fst(Self::Ledger);
            let gossip = SubCmd::parse(matches)
                .map_fst(|gossip| Self::Gossip(Box::new(gossip)));
            let config = SubCmd::parse(matches).map_fst(Self::Config);
            ledger.or(gossip).or(config)
        }
    }
    impl SubCmd for AnomaNode {
        const CMD: &'static str = NODE_CMD;

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .and_then(|matches| <Self as Cmd>::parse(matches))
        }

        fn def() -> App {
            <Self as Cmd>::add_sub(
                App::new(Self::CMD)
                    .about("Node sub-commands")
                    .setting(AppSettings::SubcommandRequiredElseHelp),
            )
        }
    }

    /// Used as top-level commands (`Cmd` instance) in `anomac` binary.
    /// Used as sub-commands (`SubCmd` instance) in `anoma` binary.
    #[derive(Debug)]
    pub enum AnomaClient {
        TxCustom(TxCustom),
        TxTransfer(TxTransfer),
        TxUpdateVp(TxUpdateVp),
        Intent(Intent),
        CraftIntent(CraftIntent),
        SubscribeTopic(SubscribeTopic),
        Bond(Bond),
    }
    impl Cmd for AnomaClient {
        fn add_sub(app: App) -> App {
            app.subcommand(TxCustom::def())
                .subcommand(TxTransfer::def())
                .subcommand(TxUpdateVp::def())
                .subcommand(Intent::def())
                .subcommand(CraftIntent::def())
                .subcommand(SubscribeTopic::def())
        }

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            let tx_custom = SubCmd::parse(matches).map_fst(Self::TxCustom);
            let tx_transfer = SubCmd::parse(matches).map_fst(Self::TxTransfer);
            let tx_update_vp = SubCmd::parse(matches).map_fst(Self::TxUpdateVp);
            let intent = SubCmd::parse(matches).map_fst(Self::Intent);
            let craft_intent =
                SubCmd::parse(matches).map_fst(Self::CraftIntent);
            let subscribe_topic =
                SubCmd::parse(matches).map_fst(Self::SubscribeTopic);
            tx_custom
                .or(tx_transfer)
                .or(tx_update_vp)
                .or(intent)
                .or(craft_intent)
                .or(subscribe_topic)
        }
    }
    impl SubCmd for AnomaClient {
        const CMD: &'static str = CLIENT_CMD;

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .and_then(|matches| <Self as Cmd>::parse(matches))
        }

        fn def() -> App {
            <Self as Cmd>::add_sub(
                App::new(Self::CMD)
                    .about("Client sub-commands")
                    .setting(AppSettings::SubcommandRequiredElseHelp),
            )
        }
    }

    #[derive(Debug)]
    pub enum Ledger {
        Run(LedgerRun),
        Reset(LedgerReset),
    }
    impl SubCmd for Ledger {
        const CMD: &'static str = "ledger";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let run = SubCmd::parse(matches).map_fst(Ledger::Run);
                let reset = SubCmd::parse(matches).map_fst(Ledger::Reset);
                run.or(reset)
                    // The `run` command is the default if no sub-command given
                    .or(Some((Ledger::Run(LedgerRun), matches)))
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Ledger node sub-commands. If no sub-command specified, \
                     defaults to run the node.",
                )
                .subcommand(LedgerRun::def())
                .subcommand(LedgerReset::def())
        }
    }

    #[derive(Debug)]
    pub struct LedgerRun;
    impl SubCmd for LedgerRun {
        const CMD: &'static str = "run";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (LedgerRun, matches))
        }

        fn def() -> App {
            App::new(Self::CMD).about("Run Anoma ledger node.")
        }
    }

    #[derive(Debug)]
    pub struct LedgerReset;
    impl SubCmd for LedgerReset {
        const CMD: &'static str = "reset";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (LedgerReset, matches))
        }

        fn def() -> App {
            App::new(Self::CMD).about(
                "Delete Anoma ledger node's and Tendermint node's storage \
                 data.",
            )
        }
    }

    #[derive(Debug)]
    pub enum Gossip {
        Run(GossipRun),
    }
    impl SubCmd for Gossip {
        const CMD: &'static str = "gossip";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let run = SubCmd::parse(matches).map_fst(Gossip::Run);
                run
                    // The `run` command is the default if no sub-command given
                    .or_else(|| {
                        Some((
                            Gossip::Run(GossipRun(args::GossipRun::parse(
                                matches,
                            ))),
                            matches,
                        ))
                    })
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Gossip node sub-commands. If no sub-command specified, \
                     defaults to run the node.",
                )
                .subcommand(GossipRun::def())
                .add_args::<args::GossipRun>()
        }
    }

    #[derive(Debug)]
    pub struct GossipRun(pub args::GossipRun);
    impl SubCmd for GossipRun {
        const CMD: &'static str = "run";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                (GossipRun(args::GossipRun::parse(matches)), matches)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Run a gossip node")
                .add_args::<args::GossipRun>()
        }
    }

    #[derive(Debug)]
    pub enum Config {
        Gen(ConfigGen),
    }
    impl SubCmd for Config {
        const CMD: &'static str = "config";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let gen = SubCmd::parse(matches).map_fst(Self::Gen);
                gen
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .about("Configuration sub-commands")
                .subcommand(ConfigGen::def())
        }
    }

    #[derive(Debug)]
    pub struct ConfigGen;
    impl SubCmd for ConfigGen {
        const CMD: &'static str = "gen";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (Self, matches))
        }

        fn def() -> App {
            App::new(Self::CMD).about("Generate the default configuration file")
        }
    }

    #[derive(Debug)]
    pub struct TxCustom(pub args::TxCustom);
    impl SubCmd for TxCustom {
        const CMD: &'static str = "tx";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)> {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                (TxCustom(args::TxCustom::parse(matches)), matches)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Send a transaction with custom WASM code")
                .add_args::<args::TxCustom>()
        }
    }

    #[derive(Debug)]
    pub struct TxTransfer(pub args::TxTransfer);
    impl SubCmd for TxTransfer {
        const CMD: &'static str = "transfer";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                (TxTransfer(args::TxTransfer::parse(matches)), matches)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Send a signed transfer transaction")
                .add_args::<args::TxTransfer>()
        }
    }

    #[derive(Debug)]
    pub struct TxUpdateVp(pub args::TxUpdateVp);
    impl SubCmd for TxUpdateVp {
        const CMD: &'static str = "update";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                (TxUpdateVp(args::TxUpdateVp::parse(matches)), matches)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Send a signed transaction to update account's validity \
                     predicate",
                )
                .add_args::<args::TxUpdateVp>()
        }
    }

    #[derive(Debug)]
    pub struct Bond(pub args::Bond);
    impl SubCmd for Bond {
        const CMD: &'static str = "bond";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (Bond(args::Bond::parse(matches)), matches))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("subscribe to a topic.")
                .add_args::<args::SubscribeTopic>()
        }
    }

    #[derive(Debug)]
    pub struct Intent(pub args::Intent);
    impl SubCmd for Intent {
        const CMD: &'static str = "intent";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (Intent(args::Intent::parse(matches)), matches))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Send an intent.")
                .add_args::<args::Intent>()
        }
    }

    #[derive(Debug)]
    pub struct CraftIntent(pub args::CraftIntent);
    impl SubCmd for CraftIntent {
        const CMD: &'static str = "craft-intent";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                (CraftIntent(args::CraftIntent::parse(matches)), matches)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Craft an intent.")
                .add_args::<args::CraftIntent>()
        }
    }

    #[derive(Debug)]
    pub struct SubscribeTopic(pub args::SubscribeTopic);
    impl SubCmd for SubscribeTopic {
        const CMD: &'static str = "subscribe-topic";

        fn parse(matches: &ArgMatches) -> Option<(Self, &ArgMatches)>
        where
            Self: Sized,
        {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                (
                    SubscribeTopic(args::SubscribeTopic::parse(matches)),
                    matches,
                )
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("subscribe to a topic.")
                .add_args::<args::SubscribeTopic>()
        }
    }
}

pub mod args {
    use std::net::SocketAddr;
    use std::path::PathBuf;
    use std::str::FromStr;

    use anoma::types::address::Address;
    use anoma::types::token;
    use libp2p::Multiaddr;

    use super::utils::*;
    use super::ArgMatches;

    const BASE_DIR: ArgDefault<PathBuf> =
        arg_default("base-dir", DefaultFn(|| ".anoma".into()));
    const DRY_RUN_TX: ArgFlag = flag("dry-run");
    const LEDGER_ADDRESS: Arg<tendermint::net::Address> = arg("ledger-address");
    const LEDGER_ADDRESS_DEFAULT: ArgDefault<tendermint::net::Address> =
        LEDGER_ADDRESS.default(DefaultFn(|| {
            let raw = "127.0.0.1:26657";
            tendermint::net::Address::from_str(raw).unwrap()
        }));
    const LEDGER_ADDRESS_OPT: ArgOpt<tendermint::net::Address> =
        LEDGER_ADDRESS.opt();
    const DATA_PATH: Arg<PathBuf> = arg("data-path");
    const DATA_PATH_OPT: ArgOpt<PathBuf> = arg_opt("data-path");
    const CODE_PATH: Arg<PathBuf> = arg("code-path");
    const PEERS: ArgMulti<String> = arg_multi("peers");
    const TOPIC: Arg<String> = arg("topic");
    const TOPICS: ArgMulti<String> = TOPIC.multi();
    const ADDRESS: Arg<Address> = arg("address");
    const MULTIADDR_OPT: ArgOpt<Multiaddr> = arg_opt("address");
    const RPC_SOCKET_ADDR: ArgOpt<SocketAddr> = arg_opt("rpc");
    const MATCHMAKER_PATH: ArgOpt<PathBuf> = arg_opt("matchmaker-path");
    const TX_CODE_PATH: ArgOpt<PathBuf> = arg_opt("tx-code-path");
    const FILTER_PATH: ArgOpt<PathBuf> = arg_opt("filter-path");
    const NODE: Arg<String> = arg("node");
    const TOKEN_SELL: Arg<Address> = arg("token-sell");
    const TOKEN_BUY: Arg<Address> = arg("token-buy");
    const AMOUNT_SELL: Arg<token::Amount> = arg("amount-sell");
    const AMOUNT_BUY: Arg<token::Amount> = arg("amount-buy");
    const FILE_PATH: ArgDefault<String> =
        arg_default("file-path", DefaultFn(|| "intent.data".into()));
    const SOURCE: Arg<Address> = arg("source");
    const TARGET: Arg<Address> = arg("target");
    const TOKEN: Arg<Address> = arg("token");
    const AMOUNT: Arg<token::Amount> = arg("amount");
    const VALIDATOR: Arg<Address> = arg("validator");

    /// Global command arguments
    #[derive(Debug)]
    pub struct Global {
        pub base_dir: PathBuf,
    }

    impl Args for Global {
        fn parse(matches: &ArgMatches) -> Self {
            let base_dir = BASE_DIR.parse(matches);
            Global { base_dir }
        }

        fn def(app: App) -> App {
            app.arg(BASE_DIR.def().about(
                "The base directory is where the client and nodes \
                 configuration and state is stored.",
            ))
        }
    }

    /// Custom transaction arguments
    #[derive(Debug)]
    pub struct TxCustom {
        /// Common tx arguments
        pub tx: Tx,
        /// Path to the tx WASM code file
        pub code_path: PathBuf,
        /// Path to the data file
        pub data_path: Option<PathBuf>,
    }

    impl Args for TxCustom {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let code_path = CODE_PATH.parse(matches);
            let data_path = DATA_PATH_OPT.parse(matches);
            Self {
                tx,
                code_path,
                data_path,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(
                    CODE_PATH
                        .def()
                        .about("The path to the transaction's WASM code."),
                )
                .arg(DATA_PATH_OPT.def().about(
                    "The data file at this path containing arbitrary bytes \
                     will be passed to the transaction code when it's \
                     executed.",
                ))
        }
    }

    /// Transfer transaction arguments
    #[derive(Debug)]
    pub struct TxTransfer {
        /// Common tx arguments
        pub tx: Tx,
        /// Transfer source address
        pub source: Address,
        /// Transfer target address
        pub target: Address,
        /// Transferred token address
        pub token: Address,
        /// Transferred token amount
        pub amount: token::Amount,
    }

    impl Args for TxTransfer {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let source = SOURCE.parse(matches);
            let target = TARGET.parse(matches);
            let token = TOKEN.parse(matches);
            let amount = AMOUNT.parse(matches);
            Self {
                tx,
                source,
                target,
                token,
                amount,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(SOURCE.def().about(
                    "The source account address. The source's key is used to \
                     produce the signature.",
                ))
                .arg(TARGET.def().about("The target account address."))
                .arg(TOKEN.def().about("The transfer token."))
                .arg(AMOUNT.def().about("The amount to transfer in decimal."))
        }
    }

    /// Transaction to update a VP arguments
    #[derive(Debug)]
    pub struct TxUpdateVp {
        /// Common tx arguments
        pub tx: Tx,
        /// Path to the VP WASM code file
        pub vp_code_path: PathBuf,
        /// Address of the account whose VP is to be updated
        pub addr: Address,
    }

    impl Args for TxUpdateVp {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let vp_code_path = CODE_PATH.parse(matches);
            let addr = ADDRESS.parse(matches);
            Self {
                tx,
                vp_code_path,
                addr,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(
                    CODE_PATH.def().about(
                        "The path to the new validity predicate WASM code.",
                    ),
                )
                .arg(ADDRESS.def().about(
                    "The account's address. It's key is used to produce the \
                     signature.",
                ))
        }
    }

    /// Bond arguments
    #[derive(Debug)]
    pub struct Bond {
        /// Common tx arguments
        pub tx: Tx,
        /// Source address (delegator for delegation or validator for
        /// self-bonds)
        pub source: Address,
        /// Validator address
        pub validator: Address,
        /// Amount of tokens to stake in a bond
        pub amount: token::Amount,
    }
    impl Args for Bond {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let source = SOURCE.parse(matches);
            let validator = VALIDATOR.parse(matches);
            let amount = AMOUNT.parse(matches);
            Self {
                tx,
                source,
                validator,
                amount,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(SOURCE.def().about(
                    "Source address (delegator for delegation or validator \
                     for self-bonds).",
                ))
                .arg(VALIDATOR.def().about("Validator address."))
                .arg(AMOUNT.def().about("Amount of tokens to stake in a bond."))
        }
    }

    /// Intent arguments
    #[derive(Debug)]
    pub struct Intent {
        /// Gossip node address
        pub node_addr: String,
        /// Path to the intent file
        pub data_path: PathBuf,
        /// Intent topic
        pub topic: String,
    }

    impl Args for Intent {
        fn parse(matches: &ArgMatches) -> Self {
            let node_addr = NODE.parse(matches);
            let data_path = DATA_PATH.parse(matches);
            let topic = TOPIC.parse(matches);
            Self {
                node_addr,
                data_path,
                topic,
            }
        }

        fn def(app: App) -> App {
            app.arg(NODE.def().about("The gossip node address."))
                .arg(DATA_PATH.def().about(
                    "The data of the intent, that contains all value \
                     necessary for the matchmaker.",
                ))
                .arg(
                    TOPIC.def().about(
                        "The subnetwork where the intent should be sent to",
                    ),
                )
        }
    }

    /// Craft intent for token exchange arguments
    #[derive(Debug)]
    pub struct CraftIntent {
        /// Source address
        pub addr: Address,
        /// Token to sell
        pub token_sell: Address,
        /// Amount of token to sell
        pub amount_sell: token::Amount,
        /// Token to buy
        pub token_buy: Address,
        /// Amount of token to buy
        pub amount_buy: token::Amount,
        /// Target file path
        pub file_path: String,
    }
    impl Args for CraftIntent {
        fn parse(matches: &ArgMatches) -> Self {
            let addr = ADDRESS.parse(matches);
            let token_sell = TOKEN_SELL.parse(matches);
            let amount_sell = AMOUNT_SELL.parse(matches);
            let token_buy = TOKEN_BUY.parse(matches);
            let amount_buy = AMOUNT_BUY.parse(matches);
            let file_path = FILE_PATH.parse(matches);
            Self {
                addr,
                token_sell,
                amount_sell,
                token_buy,
                amount_buy,
                file_path,
            }
        }

        fn def(app: App) -> App {
            app.arg(ADDRESS.def().about("The account address."))
                .arg(TOKEN_SELL.def().about("The selling token."))
                .arg(AMOUNT_SELL.def().about("The amount selling."))
                .arg(TOKEN_BUY.def().about("The buying token."))
                .arg(AMOUNT_BUY.def().about("The amount buying."))
                .arg(FILE_PATH.def().about("The output file"))
        }
    }

    /// Subscribe intent topic arguments
    #[derive(Debug)]
    pub struct SubscribeTopic {
        /// Gossip node address
        pub node_addr: String,
        /// Intent topic
        pub topic: String,
    }
    impl Args for SubscribeTopic {
        fn parse(matches: &ArgMatches) -> Self {
            let node_addr = NODE.parse(matches);
            let topic = TOPIC.parse(matches);
            Self { node_addr, topic }
        }

        fn def(app: App) -> App {
            app.arg(NODE.def().about("The gossip node address.")).arg(
                TOPIC
                    .def()
                    .about("The new topic of interest for that node."),
            )
        }
    }

    #[derive(Debug)]
    pub struct GossipRun {
        pub addr: Option<Multiaddr>,
        pub peers: Vec<String>,
        pub topics: Vec<String>,
        pub rpc: Option<SocketAddr>,
        pub matchmaker_path: Option<PathBuf>,
        pub tx_code_path: Option<PathBuf>,
        pub ledger_addr: Option<tendermint::net::Address>,
        pub filter_path: Option<PathBuf>,
    }
    impl Args for GossipRun {
        fn parse(matches: &ArgMatches) -> Self {
            let addr = MULTIADDR_OPT.parse(matches);
            let peers = PEERS.parse(matches);
            let topics = TOPICS.parse(matches);
            let rpc = RPC_SOCKET_ADDR.parse(matches);
            let matchmaker_path = MATCHMAKER_PATH.parse(matches);
            let tx_code_path = TX_CODE_PATH.parse(matches);
            let ledger_addr = LEDGER_ADDRESS_OPT.parse(matches);
            let filter_path = FILTER_PATH.parse(matches);
            Self {
                addr,
                peers,
                topics,
                rpc,
                matchmaker_path,
                tx_code_path,
                ledger_addr,
                filter_path,
            }
        }

        fn def(app: App) -> App {
            app.arg(
                MULTIADDR_OPT
                    .def()
                    .about("Gossip service address as host:port."),
            )
            .arg(PEERS.def().about("List of peers to connect to."))
            .arg(TOPICS.def().about("Enable a new gossip topic."))
            .arg(RPC_SOCKET_ADDR.def().about("Enable RPC service."))
            .arg(MATCHMAKER_PATH.def().about("The matchmaker."))
            .arg(
                TX_CODE_PATH
                    .def()
                    .about("The transaction code to use with the matchmaker"),
            )
            .arg(LEDGER_ADDRESS_OPT.def().about(
                "The address of the ledger as \"{scheme}://{host}:{port}\" \
                 that the matchmaker must send transactions to. If the scheme \
                 is not supplied, it is assumed to be TCP.",
            ))
            .arg(
                FILTER_PATH
                    .def()
                    .about("The private filter for the matchmaker"),
            )
        }
    }

    /// Common transaction arguments
    #[derive(Debug)]
    pub struct Tx {
        /// Simulate applying the transaction
        pub dry_run: bool,
        /// The address of the ledger node as host:port
        pub ledger_address: tendermint::net::Address,
    }

    impl Args for Tx {
        fn def(app: App) -> App {
            app.arg(
                DRY_RUN_TX
                    .def()
                    .about("Simulate the transaction application."),
            )
            .arg(LEDGER_ADDRESS_DEFAULT.def().about(
                "Address of a ledger node as \"{scheme}://{host}:{port}\". If \
                 the scheme is not supplied, it is assumed to be TCP.",
            ))
        }

        fn parse(matches: &ArgMatches) -> Self {
            let dry_run = DRY_RUN_TX.parse(matches);
            let ledger_address = LEDGER_ADDRESS_DEFAULT.parse(matches);
            Self {
                dry_run,
                ledger_address,
            }
        }
    }
}
pub fn anoma_cli() -> (cmds::Anoma, String) {
    let app = anoma_app();
    let matches = app.get_matches();
    let raw_sub_cmd =
        matches.subcommand().map(|(raw, _matches)| raw.to_string());
    let result = cmds::Anoma::parse(&matches);
    match (result, raw_sub_cmd) {
        (Some((cmd, _)), Some(raw_sub)) => return (cmd, raw_sub),
        _ => {
            anoma_app().print_help().unwrap();
        }
    }
    safe_exit(2);
}

pub fn anoma_node_cli() -> (cmds::AnomaNode, args::Global) {
    let app = anoma_node_app();
    let (cmd, matches) = cmds::AnomaNode::parse_or_print_help(app);
    (cmd, args::Global::parse(&matches))
}

pub fn anoma_client_cli() -> (cmds::AnomaClient, args::Global) {
    let app = anoma_client_app();
    let (cmd, matches) = cmds::AnomaClient::parse_or_print_help(app);
    (cmd, args::Global::parse(&matches))
}

fn anoma_app() -> App {
    let app = App::new(APP_NAME)
        .version(CLI_VERSION)
        .author(AUTHOR)
        .about("Anoma command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .add_args::<args::Global>();
    cmds::Anoma::add_sub(app)
}

fn anoma_node_app() -> App {
    let app = App::new(APP_NAME)
        .version(CLIENT_VERSION)
        .author(AUTHOR)
        .about("Anoma client command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .add_args::<args::Global>();
    cmds::AnomaNode::add_sub(app)
}

fn anoma_client_app() -> App {
    let app = App::new(APP_NAME)
        .version(NODE_VERSION)
        .author(AUTHOR)
        .about("Anoma node command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .add_args::<args::Global>();
    cmds::AnomaClient::add_sub(app)
}

pub fn update_gossip_config(
    args: args::GossipRun,
    config: &mut config::IntentGossiper,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(addr) = args.addr {
        config.address = addr
    }

    let matchmaker_arg = args.matchmaker_path;
    let tx_code_arg = args.tx_code_path;
    let ledger_address_arg = args.ledger_addr;
    let filter_arg = args.filter_path;
    if let Some(mut matchmaker_cfg) = config.matchmaker.as_mut() {
        if let Some(matchmaker) = matchmaker_arg {
            matchmaker_cfg.matchmaker = matchmaker
        }
        if let Some(tx_code) = tx_code_arg {
            matchmaker_cfg.tx_code = tx_code
        }
        if let Some(ledger_address) = ledger_address_arg {
            matchmaker_cfg.ledger_address = ledger_address
        }
        if let Some(filter) = filter_arg {
            matchmaker_cfg.filter = Some(filter)
        }
    } else if let (Some(matchmaker), Some(tx_code), Some(ledger_address)) = (
        matchmaker_arg.as_ref(),
        tx_code_arg.as_ref(),
        ledger_address_arg.as_ref(),
    ) {
        let matchmaker_cfg = Some(config::Matchmaker {
            matchmaker: matchmaker.clone(),
            tx_code: tx_code.clone(),
            ledger_address: ledger_address.clone(),
            filter: filter_arg,
        });
        config.matchmaker = matchmaker_cfg
    } else if matchmaker_arg.is_some()
        || tx_code_arg.is_some()
        || ledger_address_arg.is_some()
    // if at least one argument is not none then fail
    {
        panic!(
            "No complete matchmaker configuration found (matchmaker code \
             path, tx code path, and ledger address). Please update the \
             configuration with default value or use all cli argument to use \
             the matchmaker"
        );
    }
    if let Some(address) = args.rpc {
        config.rpc = Some(config::RpcServer { address });
    }
    Ok(())
}
