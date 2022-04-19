//! The CLI commands that are re-used between the executables `anoma`,
//! `anoma-node` and `anoma-client`.
//!
//! The `anoma` executable groups together the most commonly used commands
//! inlined from the node and the client. The other commands for the node or the
//! client can be dispatched via `anoma node ...` or `anoma client ...`,
//! respectively.

pub mod context;
mod utils;

use clap::{crate_authors, AppSettings, ArgMatches};
pub use utils::safe_exit;
use utils::*;

pub use self::context::Context;

include!("../../version.rs");

const APP_NAME: &str = "Anoma";

// Main Anoma sub-commands
const NODE_CMD: &str = "node";
const CLIENT_CMD: &str = "client";
const WALLET_CMD: &str = "wallet";

pub mod cmds {
    use clap::AppSettings;

    use super::utils::*;
    use super::{args, ArgMatches, CLIENT_CMD, NODE_CMD, WALLET_CMD};

    /// Commands for `anoma` binary.
    #[allow(clippy::large_enum_variant)]
    #[derive(Clone, Debug)]
    pub enum Anoma {
        // Sub-binary-commands
        Node(AnomaNode),
        Client(AnomaClient),
        Wallet(AnomaWallet),

        // Inlined commands from the node.
        Ledger(Ledger),
        Gossip(Gossip),
        Matchmaker(Matchmaker),

        // Inlined commands from the client.
        TxCustom(TxCustom),
        TxTransfer(TxTransfer),
        TxUpdateVp(TxUpdateVp),
        TxInitNft(TxInitNft),
        TxMintNft(TxMintNft),
        Intent(Intent),
    }

    impl Cmd for Anoma {
        fn add_sub(app: App) -> App {
            app.subcommand(AnomaNode::def())
                .subcommand(AnomaClient::def())
                .subcommand(AnomaWallet::def())
                .subcommand(Ledger::def())
                .subcommand(Gossip::def())
                .subcommand(Matchmaker::def())
                .subcommand(TxCustom::def())
                .subcommand(TxTransfer::def())
                .subcommand(TxUpdateVp::def())
                .subcommand(TxInitNft::def())
                .subcommand(TxMintNft::def())
                .subcommand(Intent::def())
        }

        fn parse(matches: &ArgMatches) -> Option<Self> {
            let node = SubCmd::parse(matches).map(Self::Node);
            let client = SubCmd::parse(matches).map(Self::Client);
            let wallet = SubCmd::parse(matches).map(Self::Wallet);
            let ledger = SubCmd::parse(matches).map(Self::Ledger);
            let gossip = SubCmd::parse(matches).map(Self::Gossip);
            let matchmaker = SubCmd::parse(matches).map(Self::Matchmaker);
            let tx_custom = SubCmd::parse(matches).map(Self::TxCustom);
            let tx_transfer = SubCmd::parse(matches).map(Self::TxTransfer);
            let tx_update_vp = SubCmd::parse(matches).map(Self::TxUpdateVp);
            let tx_nft_create = SubCmd::parse(matches).map(Self::TxInitNft);
            let tx_nft_mint = SubCmd::parse(matches).map(Self::TxMintNft);
            let intent = SubCmd::parse(matches).map(Self::Intent);
            node.or(client)
                .or(wallet)
                .or(ledger)
                .or(gossip)
                .or(matchmaker)
                .or(tx_custom)
                .or(tx_transfer)
                .or(tx_update_vp)
                .or(tx_nft_create)
                .or(tx_nft_mint)
                .or(intent)
        }
    }

    /// Used as top-level commands (`Cmd` instance) in `anoman` binary.
    /// Used as sub-commands (`SubCmd` instance) in `anoma` binary.
    #[derive(Clone, Debug)]
    #[allow(clippy::large_enum_variant)]
    pub enum AnomaNode {
        Ledger(Ledger),
        Gossip(Gossip),
        Matchmaker(Matchmaker),
        Config(Config),
    }

    impl Cmd for AnomaNode {
        fn add_sub(app: App) -> App {
            app.subcommand(Ledger::def())
                .subcommand(Gossip::def())
                .subcommand(Matchmaker::def())
                .subcommand(Config::def())
        }

        fn parse(matches: &ArgMatches) -> Option<Self> {
            let ledger = SubCmd::parse(matches).map(Self::Ledger);
            let gossip = SubCmd::parse(matches).map(Self::Gossip);
            let matchmaker = SubCmd::parse(matches).map(Self::Matchmaker);
            let config = SubCmd::parse(matches).map(Self::Config);
            ledger.or(gossip).or(matchmaker).or(config)
        }
    }
    impl SubCmd for AnomaNode {
        const CMD: &'static str = NODE_CMD;

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .and_then(<Self as Cmd>::parse)
        }

        fn def() -> App {
            <Self as Cmd>::add_sub(
                App::new(Self::CMD)
                    .about("Node sub-commands.")
                    .setting(AppSettings::SubcommandRequiredElseHelp),
            )
        }
    }

    /// Used as top-level commands (`Cmd` instance) in `anomac` binary.
    /// Used as sub-commands (`SubCmd` instance) in `anoma` binary.
    #[derive(Clone, Debug)]
    #[allow(clippy::large_enum_variant)]
    pub enum AnomaClient {
        /// The [`super::Context`] provides access to the wallet and the
        /// config. It will generate a new wallet and config, if they
        /// don't exist.
        WithContext(AnomaClientWithContext),
        /// Utils don't have [`super::Context`], only the global arguments.
        WithoutContext(Utils),
    }

    impl Cmd for AnomaClient {
        fn add_sub(app: App) -> App {
            app
                // Simple transactions
                .subcommand(TxCustom::def().display_order(1))
                .subcommand(TxTransfer::def().display_order(1))
                .subcommand(TxUpdateVp::def().display_order(1))
                .subcommand(TxInitAccount::def().display_order(1))
                .subcommand(TxInitValidator::def().display_order(1))
                // Nft transactions
                .subcommand(TxInitNft::def().display_order(1))
                .subcommand(TxMintNft::def().display_order(1))
                // PoS transactions
                .subcommand(Bond::def().display_order(2))
                .subcommand(Unbond::def().display_order(2))
                .subcommand(Withdraw::def().display_order(2))
                // Queries
                .subcommand(QueryEpoch::def().display_order(3))
                .subcommand(QueryBalance::def().display_order(3))
                .subcommand(QueryBonds::def().display_order(3))
                .subcommand(QueryVotingPower::def().display_order(3))
                .subcommand(QuerySlashes::def().display_order(3))
                .subcommand(QueryResult::def().display_order(3))
                // Intents
                .subcommand(Intent::def().display_order(4))
                .subcommand(SubscribeTopic::def().display_order(4))
                // Utils
                .subcommand(Utils::def().display_order(5))
        }

        fn parse(matches: &ArgMatches) -> Option<Self> {
            use AnomaClientWithContext::*;
            let tx_custom = Self::parse_with_ctx(matches, TxCustom);
            let tx_transfer = Self::parse_with_ctx(matches, TxTransfer);
            let tx_update_vp = Self::parse_with_ctx(matches, TxUpdateVp);
            let tx_init_account = Self::parse_with_ctx(matches, TxInitAccount);
            let tx_init_validator =
                Self::parse_with_ctx(matches, TxInitValidator);
            let tx_nft_create = Self::parse_with_ctx(matches, TxInitNft);
            let tx_nft_mint = Self::parse_with_ctx(matches, TxMintNft);
            let bond = Self::parse_with_ctx(matches, Bond);
            let unbond = Self::parse_with_ctx(matches, Unbond);
            let withdraw = Self::parse_with_ctx(matches, Withdraw);
            let query_epoch = Self::parse_with_ctx(matches, QueryEpoch);
            let query_balance = Self::parse_with_ctx(matches, QueryBalance);
            let query_bonds = Self::parse_with_ctx(matches, QueryBonds);
            let query_voting_power =
                Self::parse_with_ctx(matches, QueryVotingPower);
            let query_slashes = Self::parse_with_ctx(matches, QuerySlashes);
            let query_result = Self::parse_with_ctx(matches, QueryResult);
            let intent = Self::parse_with_ctx(matches, Intent);
            let subscribe_topic = Self::parse_with_ctx(matches, SubscribeTopic);
            let utils = SubCmd::parse(matches).map(Self::WithoutContext);
            tx_custom
                .or(tx_transfer)
                .or(tx_update_vp)
                .or(tx_init_account)
                .or(tx_init_validator)
                .or(tx_nft_create)
                .or(tx_nft_mint)
                .or(bond)
                .or(unbond)
                .or(withdraw)
                .or(query_epoch)
                .or(query_balance)
                .or(query_bonds)
                .or(query_voting_power)
                .or(query_slashes)
                .or(query_result)
                .or(intent)
                .or(subscribe_topic)
                .or(utils)
        }
    }

    impl AnomaClient {
        /// A helper method to parse sub cmds with context
        fn parse_with_ctx<T: SubCmd>(
            matches: &ArgMatches,
            sub_to_self: impl Fn(T) -> AnomaClientWithContext,
        ) -> Option<Self> {
            SubCmd::parse(matches)
                .map(|sub| Self::WithContext(sub_to_self(sub)))
        }
    }

    impl SubCmd for AnomaClient {
        const CMD: &'static str = CLIENT_CMD;

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .and_then(<Self as Cmd>::parse)
        }

        fn def() -> App {
            <Self as Cmd>::add_sub(
                App::new(Self::CMD)
                    .about("Client sub-commands.")
                    .setting(AppSettings::SubcommandRequiredElseHelp),
            )
        }
    }

    #[derive(Clone, Debug)]
    pub enum AnomaClientWithContext {
        // Ledger cmds
        TxCustom(TxCustom),
        TxTransfer(TxTransfer),
        QueryResult(QueryResult),
        TxUpdateVp(TxUpdateVp),
        TxInitAccount(TxInitAccount),
        TxInitValidator(TxInitValidator),
        TxInitNft(TxInitNft),
        TxMintNft(TxMintNft),
        Bond(Bond),
        Unbond(Unbond),
        Withdraw(Withdraw),
        QueryEpoch(QueryEpoch),
        QueryBalance(QueryBalance),
        QueryBonds(QueryBonds),
        QueryVotingPower(QueryVotingPower),
        QuerySlashes(QuerySlashes),
        // Gossip cmds
        Intent(Intent),
        SubscribeTopic(SubscribeTopic),
    }

    #[derive(Clone, Debug)]
    pub enum AnomaWallet {
        /// Key management commands
        Key(WalletKey),
        /// Address management commands
        Address(WalletAddress),
    }

    impl Cmd for AnomaWallet {
        fn add_sub(app: App) -> App {
            app.subcommand(WalletKey::def())
                .subcommand(WalletAddress::def())
        }

        fn parse(matches: &ArgMatches) -> Option<Self> {
            let key = SubCmd::parse(matches).map(Self::Key);
            let address = SubCmd::parse(matches).map(Self::Address);
            key.or(address)
        }
    }

    impl SubCmd for AnomaWallet {
        const CMD: &'static str = WALLET_CMD;

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .and_then(<Self as Cmd>::parse)
        }

        fn def() -> App {
            <Self as Cmd>::add_sub(
                App::new(Self::CMD)
                    .about("Wallet sub-commands.")
                    .setting(AppSettings::SubcommandRequiredElseHelp),
            )
        }
    }

    #[derive(Clone, Debug)]
    #[allow(clippy::large_enum_variant)]
    pub enum WalletKey {
        Gen(KeyGen),
        Find(KeyFind),
        List(KeyList),
        Export(Export),
    }

    impl SubCmd for WalletKey {
        const CMD: &'static str = "key";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let generate = SubCmd::parse(matches).map(Self::Gen);
                let lookup = SubCmd::parse(matches).map(Self::Find);
                let list = SubCmd::parse(matches).map(Self::List);
                let export = SubCmd::parse(matches).map(Self::Export);
                generate.or(lookup).or(list).or(export)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Keypair management, including methods to generate and \
                     look-up keys.",
                )
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .subcommand(KeyGen::def())
                .subcommand(KeyFind::def())
                .subcommand(KeyList::def())
                .subcommand(Export::def())
        }
    }

    /// Generate a new keypair and an implicit address derived from it
    #[derive(Clone, Debug)]
    pub struct KeyGen(pub args::KeyAndAddressGen);

    impl SubCmd for KeyGen {
        const CMD: &'static str = "gen";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Self(args::KeyAndAddressGen::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Generates a keypair with a given alias and derive the \
                     implicit address from its public key. The address will \
                     be stored with the same alias.",
                )
                .add_args::<args::KeyAndAddressGen>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct KeyFind(pub args::KeyFind);

    impl SubCmd for KeyFind {
        const CMD: &'static str = "find";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (Self(args::KeyFind::parse(matches))))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Searches for a keypair from a public key or an alias.")
                .add_args::<args::KeyFind>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct KeyList(pub args::KeyList);

    impl SubCmd for KeyList {
        const CMD: &'static str = "list";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (Self(args::KeyList::parse(matches))))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("List all known keys.")
                .add_args::<args::KeyList>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct Export(pub args::KeyExport);

    impl SubCmd for Export {
        const CMD: &'static str = "export";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| (Self(args::KeyExport::parse(matches))))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Exports a keypair to a file.")
                .add_args::<args::KeyExport>()
        }
    }

    #[derive(Clone, Debug)]
    pub enum WalletAddress {
        Gen(AddressGen),
        Find(AddressFind),
        List(AddressList),
        Add(AddressAdd),
    }

    impl SubCmd for WalletAddress {
        const CMD: &'static str = "address";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let gen = SubCmd::parse(matches).map(Self::Gen);
                let find = SubCmd::parse(matches).map(Self::Find);
                let list = SubCmd::parse(matches).map(Self::List);
                let add = SubCmd::parse(matches).map(Self::Add);
                gen.or(find).or(list).or(add)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Address management, including methods to generate and \
                     look-up addresses.",
                )
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .subcommand(AddressGen::def())
                .subcommand(AddressFind::def())
                .subcommand(AddressList::def())
                .subcommand(AddressAdd::def())
        }
    }

    /// Generate a new keypair and an implicit address derived from it
    #[derive(Clone, Debug)]
    pub struct AddressGen(pub args::KeyAndAddressGen);

    impl SubCmd for AddressGen {
        const CMD: &'static str = "gen";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                AddressGen(args::KeyAndAddressGen::parse(matches))
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Generates a keypair with a given alias and derive the \
                     implicit address from its public key. The address will \
                     be stored with the same alias.",
                )
                .add_args::<args::KeyAndAddressGen>()
        }
    }

    /// Find an address by its alias
    #[derive(Clone, Debug)]
    pub struct AddressFind(pub args::AddressFind);

    impl SubCmd for AddressFind {
        const CMD: &'static str = "find";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| AddressFind(args::AddressFind::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Find an address by its alias.")
                .add_args::<args::AddressFind>()
        }
    }

    /// List known addresses
    #[derive(Clone, Debug)]
    pub struct AddressList;

    impl SubCmd for AddressList {
        const CMD: &'static str = "list";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|_matches| AddressList)
        }

        fn def() -> App {
            App::new(Self::CMD).about("List all known addresses.")
        }
    }

    /// Generate a new keypair and an implicit address derived from it
    #[derive(Clone, Debug)]
    pub struct AddressAdd(pub args::AddressAdd);

    impl SubCmd for AddressAdd {
        const CMD: &'static str = "add";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| AddressAdd(args::AddressAdd::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Store an alias for an address in the wallet.")
                .add_args::<args::AddressAdd>()
        }
    }

    #[derive(Clone, Debug)]
    pub enum Ledger {
        Run(LedgerRun),
        Reset(LedgerReset),
    }

    impl SubCmd for Ledger {
        const CMD: &'static str = "ledger";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let run = SubCmd::parse(matches).map(Self::Run);
                let reset = SubCmd::parse(matches).map(Self::Reset);
                run.or(reset)
                    // The `run` command is the default if no sub-command given
                    .or(Some(Self::Run(LedgerRun)))
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

    #[derive(Clone, Debug)]
    pub struct LedgerRun;

    impl SubCmd for LedgerRun {
        const CMD: &'static str = "run";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|_matches| Self)
        }

        fn def() -> App {
            App::new(Self::CMD).about("Run Anoma ledger node.")
        }
    }

    #[derive(Clone, Debug)]
    pub struct LedgerReset;

    impl SubCmd for LedgerReset {
        const CMD: &'static str = "reset";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|_matches| Self)
        }

        fn def() -> App {
            App::new(Self::CMD).about(
                "Delete Anoma ledger node's and Tendermint node's storage \
                 data.",
            )
        }
    }

    #[derive(Clone, Debug)]
    pub enum Gossip {
        Run(GossipRun),
    }

    impl SubCmd for Gossip {
        const CMD: &'static str = "gossip";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let run = SubCmd::parse(matches).map(Gossip::Run);
                run
                    // The `run` command is the default if no sub-command given
                    .or_else(|| {
                        Some(Gossip::Run(GossipRun(args::GossipRun::parse(
                            matches,
                        ))))
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

    #[derive(Clone, Debug)]
    pub struct Matchmaker(pub args::Matchmaker);

    impl SubCmd for Matchmaker {
        const CMD: &'static str = "matchmaker";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Matchmaker(args::Matchmaker::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Run a matchmaker.")
                .add_args::<args::Matchmaker>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct GossipRun(pub args::GossipRun);

    impl SubCmd for GossipRun {
        const CMD: &'static str = "run";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| GossipRun(args::GossipRun::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Run a gossip node.")
                .add_args::<args::GossipRun>()
        }
    }

    #[derive(Clone, Debug)]
    pub enum Config {
        Gen(ConfigGen),
    }

    impl SubCmd for Config {
        const CMD: &'static str = "config";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .and_then(|matches| SubCmd::parse(matches).map(Self::Gen))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .about("Configuration sub-commands.")
                .subcommand(ConfigGen::def())
        }
    }

    #[derive(Clone, Debug)]
    pub struct ConfigGen;

    impl SubCmd for ConfigGen {
        const CMD: &'static str = "gen";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|_matches| Self)
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Generate the default configuration file.")
        }
    }

    #[derive(Clone, Debug)]
    pub struct QueryResult(pub args::QueryResult);

    impl SubCmd for QueryResult {
        const CMD: &'static str = "tx-result";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| QueryResult(args::QueryResult::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Query the result of a transaction.")
                .add_args::<args::QueryResult>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxCustom(pub args::TxCustom);

    impl SubCmd for TxCustom {
        const CMD: &'static str = "tx";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| TxCustom(args::TxCustom::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Send a transaction with custom WASM code.")
                .add_args::<args::TxCustom>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxTransfer(pub args::TxTransfer);

    impl SubCmd for TxTransfer {
        const CMD: &'static str = "transfer";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| TxTransfer(args::TxTransfer::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Send a signed transfer transaction.")
                .add_args::<args::TxTransfer>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxUpdateVp(pub args::TxUpdateVp);

    impl SubCmd for TxUpdateVp {
        const CMD: &'static str = "update";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| TxUpdateVp(args::TxUpdateVp::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Send a signed transaction to update account's validity \
                     predicate.",
                )
                .add_args::<args::TxUpdateVp>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxInitAccount(pub args::TxInitAccount);

    impl SubCmd for TxInitAccount {
        const CMD: &'static str = "init-account";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                TxInitAccount(args::TxInitAccount::parse(matches))
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Send a signed transaction to create a new established \
                     account.",
                )
                .add_args::<args::TxInitAccount>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxInitValidator(pub args::TxInitValidator);

    impl SubCmd for TxInitValidator {
        const CMD: &'static str = "init-validator";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                TxInitValidator(args::TxInitValidator::parse(matches))
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Send a signed transaction to create a new validator and \
                     its staking reward account.",
                )
                .add_args::<args::TxInitValidator>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct Bond(pub args::Bond);

    impl SubCmd for Bond {
        const CMD: &'static str = "bond";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Bond(args::Bond::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Bond tokens in PoS system.")
                .add_args::<args::Bond>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct Unbond(pub args::Unbond);

    impl SubCmd for Unbond {
        const CMD: &'static str = "unbond";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Unbond(args::Unbond::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Unbond tokens from a PoS bond.")
                .add_args::<args::Unbond>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct Withdraw(pub args::Withdraw);

    impl SubCmd for Withdraw {
        const CMD: &'static str = "withdraw";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Withdraw(args::Withdraw::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Withdraw tokens from previously unbonded PoS bond.")
                .add_args::<args::Withdraw>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct QueryEpoch(pub args::Query);

    impl SubCmd for QueryEpoch {
        const CMD: &'static str = "epoch";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| QueryEpoch(args::Query::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Query the epoch of the last committed block.")
                .add_args::<args::Query>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct QueryBalance(pub args::QueryBalance);

    impl SubCmd for QueryBalance {
        const CMD: &'static str = "balance";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| QueryBalance(args::QueryBalance::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Query balance(s) of tokens.")
                .add_args::<args::QueryBalance>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct QueryBonds(pub args::QueryBonds);

    impl SubCmd for QueryBonds {
        const CMD: &'static str = "bonds";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| QueryBonds(args::QueryBonds::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Query PoS bond(s).")
                .add_args::<args::QueryBonds>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct QueryVotingPower(pub args::QueryVotingPower);

    impl SubCmd for QueryVotingPower {
        const CMD: &'static str = "voting-power";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                QueryVotingPower(args::QueryVotingPower::parse(matches))
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Query PoS voting power.")
                .add_args::<args::QueryVotingPower>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct QuerySlashes(pub args::QuerySlashes);

    impl SubCmd for QuerySlashes {
        const CMD: &'static str = "slashes";

        fn parse(matches: &ArgMatches) -> Option<Self>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| QuerySlashes(args::QuerySlashes::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Query PoS applied slashes.")
                .add_args::<args::QuerySlashes>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxInitNft(pub args::NftCreate);

    impl SubCmd for TxInitNft {
        const CMD: &'static str = "init-nft";

        fn parse(matches: &ArgMatches) -> Option<Self>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| TxInitNft(args::NftCreate::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Create a new NFT.")
                .add_args::<args::NftCreate>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct TxMintNft(pub args::NftMint);

    impl SubCmd for TxMintNft {
        const CMD: &'static str = "mint-nft";

        fn parse(matches: &ArgMatches) -> Option<Self>
        where
            Self: Sized,
        {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| TxMintNft(args::NftMint::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Mint new NFT tokens.")
                .add_args::<args::NftMint>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct Intent(pub args::Intent);

    impl SubCmd for Intent {
        const CMD: &'static str = "intent";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Intent(args::Intent::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Send an intent.")
                .add_args::<args::Intent>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct SubscribeTopic(pub args::SubscribeTopic);

    impl SubCmd for SubscribeTopic {
        const CMD: &'static str = "subscribe-topic";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).map(|matches| {
                SubscribeTopic(args::SubscribeTopic::parse(matches))
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Subscribe intent gossip node with a matchmaker to a \
                     topic.",
                )
                .add_args::<args::SubscribeTopic>()
        }
    }

    #[derive(Clone, Debug)]
    pub enum Utils {
        JoinNetwork(JoinNetwork),
        InitNetwork(InitNetwork),
        InitGenesisValidator(InitGenesisValidator),
    }

    impl SubCmd for Utils {
        const CMD: &'static str = "utils";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches.subcommand_matches(Self::CMD).and_then(|matches| {
                let join_network =
                    SubCmd::parse(matches).map(Self::JoinNetwork);
                let init_network =
                    SubCmd::parse(matches).map(Self::InitNetwork);
                let init_genesis =
                    SubCmd::parse(matches).map(Self::InitGenesisValidator);
                join_network.or(init_network).or(init_genesis)
            })
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Utilities.")
                .subcommand(JoinNetwork::def())
                .subcommand(InitNetwork::def())
                .subcommand(InitGenesisValidator::def())
                .setting(AppSettings::SubcommandRequiredElseHelp)
        }
    }

    #[derive(Clone, Debug)]
    pub struct JoinNetwork(pub args::JoinNetwork);

    impl SubCmd for JoinNetwork {
        const CMD: &'static str = "join-network";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Self(args::JoinNetwork::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Configure Anoma to join an existing network.")
                .add_args::<args::JoinNetwork>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct InitNetwork(pub args::InitNetwork);

    impl SubCmd for InitNetwork {
        const CMD: &'static str = "init-network";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Self(args::InitNetwork::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about("Initialize a new test network.")
                .add_args::<args::InitNetwork>()
        }
    }

    #[derive(Clone, Debug)]
    pub struct InitGenesisValidator(pub args::InitGenesisValidator);

    impl SubCmd for InitGenesisValidator {
        const CMD: &'static str = "init-genesis-validator";

        fn parse(matches: &ArgMatches) -> Option<Self> {
            matches
                .subcommand_matches(Self::CMD)
                .map(|matches| Self(args::InitGenesisValidator::parse(matches)))
        }

        fn def() -> App {
            App::new(Self::CMD)
                .about(
                    "Initialize genesis validator's address, staking reward \
                     address, consensus key, validator account key and \
                     staking rewards key and use it in the ledger's node.",
                )
                .add_args::<args::InitGenesisValidator>()
        }
    }
}

pub mod args {

    use std::convert::TryFrom;
    use std::env;
    use std::fs::File;
    use std::net::SocketAddr;
    use std::path::PathBuf;
    use std::str::FromStr;

    use anoma::types::address::Address;
    use anoma::types::chain::{ChainId, ChainIdPrefix};
    use anoma::types::intent::{DecimalWrapper, Exchange};
    use anoma::types::key::*;
    use anoma::types::storage::Epoch;
    use anoma::types::token;
    use anoma::types::transaction::GasLimit;
    use libp2p::Multiaddr;
    use serde::Deserialize;
    #[cfg(not(feature = "ABCI"))]
    use tendermint::Timeout;
    #[cfg(not(feature = "ABCI"))]
    use tendermint_config::net::Address as TendermintAddress;
    #[cfg(feature = "ABCI")]
    use tendermint_config_abci::net::Address as TendermintAddress;
    #[cfg(feature = "ABCI")]
    use tendermint_stable::Timeout;

    use super::context::{WalletAddress, WalletKeypair, WalletPublicKey};
    use super::utils::*;
    use super::ArgMatches;
    use crate::config;
    use crate::config::TendermintMode;

    const ADDRESS: Arg<WalletAddress> = arg("address");
    const ALIAS_OPT: ArgOpt<String> = ALIAS.opt();
    const ALIAS: Arg<String> = arg("alias");
    const ALLOW_DUPLICATE_IP: ArgFlag = flag("allow-duplicate-ip");
    const AMOUNT: Arg<token::Amount> = arg("amount");
    const BASE_DIR: ArgDefault<PathBuf> = arg_default(
        "base-dir",
        DefaultFn(|| match env::var("ANOMA_BASE_DIR") {
            Ok(dir) => dir.into(),
            Err(_) => config::DEFAULT_BASE_DIR.into(),
        }),
    );
    const BROADCAST_ONLY: ArgFlag = flag("broadcast-only");
    const CHAIN_ID: Arg<ChainId> = arg("chain-id");
    const CHAIN_ID_OPT: ArgOpt<ChainId> = CHAIN_ID.opt();
    const CHAIN_ID_PREFIX: Arg<ChainIdPrefix> = arg("chain-prefix");
    const CODE_PATH: Arg<PathBuf> = arg("code-path");
    const CODE_PATH_OPT: ArgOpt<PathBuf> = CODE_PATH.opt();
    const CONSENSUS_TIMEOUT_COMMIT: ArgDefault<Timeout> = arg_default(
        "consensus-timeout-commit",
        DefaultFn(|| Timeout::from_str("1s").unwrap()),
    );
    const DATA_PATH_OPT: ArgOpt<PathBuf> = arg_opt("data-path");
    const DATA_PATH: Arg<PathBuf> = arg("data-path");
    const DECRYPT: ArgFlag = flag("decrypt");
    const DONT_ARCHIVE: ArgFlag = flag("dont-archive");
    const DRY_RUN_TX: ArgFlag = flag("dry-run");
    const EPOCH: ArgOpt<Epoch> = arg_opt("epoch");
    const FEE_AMOUNT: ArgDefault<token::Amount> =
        arg_default("fee-amount", DefaultFn(|| token::Amount::from(0)));
    const FEE_TOKEN: ArgDefaultFromCtx<WalletAddress> =
        arg_default_from_ctx("fee-token", DefaultFn(|| "XAN".into()));
    const FORCE: ArgFlag = flag("force");
    const GAS_LIMIT: ArgDefault<token::Amount> =
        arg_default("gas-limit", DefaultFn(|| token::Amount::from(0)));
    const GENESIS_PATH: Arg<PathBuf> = arg("genesis-path");
    const GENESIS_VALIDATOR: ArgOpt<String> = arg("genesis-validator").opt();
    const INTENT_GOSSIPER_ADDR: ArgDefault<SocketAddr> = arg_default(
        "intent-gossiper",
        DefaultFn(|| {
            let raw = "127.0.0.1:26661";
            SocketAddr::from_str(raw).unwrap()
        }),
    );
    const LEDGER_ADDRESS_ABOUT: &str =
        "Address of a ledger node as \"{scheme}://{host}:{port}\". If the \
         scheme is not supplied, it is assumed to be TCP.";
    const LEDGER_ADDRESS_DEFAULT: ArgDefault<TendermintAddress> =
        LEDGER_ADDRESS.default(DefaultFn(|| {
            let raw = "127.0.0.1:26657";
            TendermintAddress::from_str(raw).unwrap()
        }));
    const LEDGER_ADDRESS: Arg<TendermintAddress> = arg("ledger-address");
    const LOCALHOST: ArgFlag = flag("localhost");
    const MATCHMAKER_PATH: ArgOpt<PathBuf> = arg_opt("matchmaker-path");
    const MODE: ArgOpt<String> = arg_opt("mode");
    const MULTIADDR_OPT: ArgOpt<Multiaddr> = arg_opt("address");
    const NODE_OPT: ArgOpt<String> = arg_opt("node");
    const NODE: Arg<String> = arg("node");
    const NFT_ADDRESS: Arg<Address> = arg("nft-address");
    const OWNER: ArgOpt<WalletAddress> = arg_opt("owner");
    const PROTOCOL_KEY: ArgOpt<WalletPublicKey> = arg_opt("protocol-key");
    const PUBLIC_KEY: Arg<WalletPublicKey> = arg("public-key");
    const RAW_ADDRESS: Arg<Address> = arg("address");
    const RAW_PUBLIC_KEY_OPT: ArgOpt<common::PublicKey> = arg_opt("public-key");
    const REWARDS_CODE_PATH: ArgOpt<PathBuf> = arg_opt("rewards-code-path");
    const REWARDS_KEY: ArgOpt<WalletPublicKey> = arg_opt("rewards-key");
    const RPC_SOCKET_ADDR: ArgOpt<SocketAddr> = arg_opt("rpc");
    const SIGNER: ArgOpt<WalletAddress> = arg_opt("signer");
    const SIGNING_KEY_OPT: ArgOpt<WalletKeypair> = SIGNING_KEY.opt();
    const SIGNING_KEY: Arg<WalletKeypair> = arg("signing-key");
    const SOURCE: Arg<WalletAddress> = arg("source");
    const SOURCE_OPT: ArgOpt<WalletAddress> = SOURCE.opt();
    const TARGET: Arg<WalletAddress> = arg("target");
    const TO_STDOUT: ArgFlag = flag("stdout");
    const TOKEN_OPT: ArgOpt<WalletAddress> = TOKEN.opt();
    const TOKEN: Arg<WalletAddress> = arg("token");
    const TOPIC_OPT: ArgOpt<String> = arg_opt("topic");
    const TOPIC: Arg<String> = arg("topic");
    const TX_CODE_PATH: ArgOpt<PathBuf> = arg_opt("tx-code-path");
    const TX_HASH: Arg<String> = arg("tx-hash");
    const UNSAFE_DONT_ENCRYPT: ArgFlag = flag("unsafe-dont-encrypt");
    const UNSAFE_SHOW_SECRET: ArgFlag = flag("unsafe-show-secret");
    const VALIDATOR: Arg<WalletAddress> = arg("validator");
    const VALIDATOR_OPT: ArgOpt<WalletAddress> = VALIDATOR.opt();
    const VALIDATOR_ACCOUNT_KEY: ArgOpt<WalletPublicKey> =
        arg_opt("account-key");
    const VALIDATOR_CONSENSUS_KEY: ArgOpt<WalletKeypair> =
        arg_opt("consensus-key");
    const VALIDATOR_CODE_PATH: ArgOpt<PathBuf> = arg_opt("validator-code-path");
    const VALUE: ArgOpt<String> = arg_opt("value");
    const WASM_CHECKSUMS_PATH: Arg<PathBuf> = arg("wasm-checksums-path");
    const WASM_DIR: ArgOpt<PathBuf> = arg_opt("wasm-dir");

    /// Global command arguments
    #[derive(Clone, Debug)]
    pub struct Global {
        pub chain_id: Option<ChainId>,
        pub base_dir: PathBuf,
        pub wasm_dir: Option<PathBuf>,
        pub mode: Option<TendermintMode>,
    }

    impl Global {
        /// Parse global arguments
        pub fn parse(matches: &ArgMatches) -> Self {
            let chain_id = CHAIN_ID_OPT.parse(matches);
            let base_dir = BASE_DIR.parse(matches);
            let wasm_dir = WASM_DIR.parse(matches);
            let mode = MODE.parse(matches).map(TendermintMode::from);
            Global {
                chain_id,
                base_dir,
                wasm_dir,
                mode,
            }
        }

        /// Add global args definition. Should be added to every top-level
        /// command.
        pub fn def(app: App) -> App {
            app.arg(CHAIN_ID_OPT.def().about("The chain ID."))
                .arg(BASE_DIR.def().about(
                    "The base directory is where the nodes, client and wallet \
                     configuration and state is stored. This value can also \
                     be set via `ANOMA_BASE_DIR` environment variable, but \
                     the argument takes precedence, if specified. Defaults to \
                     `.anoma`.",
                ))
                .arg(WASM_DIR.def().about(
                    "Directory with built WASM validity predicates, \
                     transactions and matchmaker files. This must not be an \
                     absolute path as the directory is nested inside the \
                     chain directory. This value can also be set via \
                     `ANOMA_WASM_DIR` environment variable, but the argument \
                     takes precedence, if specified.",
                ))
                .arg(MODE.def().about(
                    "The mode in which to run Anoma. Options are \n\t * \
                     Validator (default)\n\t * Full\n\t * Seed",
                ))
        }
    }

    /// Transaction associated results arguments
    #[derive(Clone, Debug)]
    pub struct QueryResult {
        /// Common query args
        pub query: Query,
        /// Hash of transaction to lookup
        pub tx_hash: String,
    }

    impl Args for QueryResult {
        fn parse(matches: &ArgMatches) -> Self {
            let query = Query::parse(matches);
            let tx_hash = TX_HASH.parse(matches);
            Self { query, tx_hash }
        }

        fn def(app: App) -> App {
            app.add_args::<Query>().arg(
                TX_HASH
                    .def()
                    .about("The hash of the transaction being looked up."),
            )
        }
    }

    /// Custom transaction arguments
    #[derive(Clone, Debug)]
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
    #[derive(Clone, Debug)]
    pub struct TxTransfer {
        /// Common tx arguments
        pub tx: Tx,
        /// Transfer source address
        pub source: WalletAddress,
        /// Transfer target address
        pub target: WalletAddress,
        /// Transferred token address
        pub token: WalletAddress,
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

    /// Transaction to initialize a new account
    #[derive(Clone, Debug)]
    pub struct TxInitAccount {
        /// Common tx arguments
        pub tx: Tx,
        /// Address of the source account
        pub source: WalletAddress,
        /// Path to the VP WASM code file for the new account
        pub vp_code_path: Option<PathBuf>,
        /// Public key for the new account
        pub public_key: WalletPublicKey,
    }

    impl Args for TxInitAccount {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let source = SOURCE.parse(matches);
            let vp_code_path = CODE_PATH_OPT.parse(matches);
            let public_key = PUBLIC_KEY.parse(matches);
            Self {
                tx,
                source,
                vp_code_path,
                public_key,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(SOURCE.def().about(
                    "The source account's address that signs the transaction.",
                ))
                .arg(CODE_PATH_OPT.def().about(
                    "The path to the validity predicate WASM code to be used \
                     for the new account. Uses the default user VP if none \
                     specified.",
                ))
                .arg(PUBLIC_KEY.def().about(
                    "A public key to be used for the new account in \
                     hexadecimal encoding.",
                ))
        }
    }

    /// Transaction to initialize a new account
    #[derive(Clone, Debug)]
    pub struct TxInitValidator {
        pub tx: Tx,
        pub source: WalletAddress,
        pub account_key: Option<WalletPublicKey>,
        pub consensus_key: Option<WalletKeypair>,
        pub rewards_account_key: Option<WalletPublicKey>,
        pub protocol_key: Option<WalletPublicKey>,
        pub validator_vp_code_path: Option<PathBuf>,
        pub rewards_vp_code_path: Option<PathBuf>,
        pub unsafe_dont_encrypt: bool,
    }

    impl Args for TxInitValidator {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let source = SOURCE.parse(matches);
            let account_key = VALIDATOR_ACCOUNT_KEY.parse(matches);
            let consensus_key = VALIDATOR_CONSENSUS_KEY.parse(matches);
            let rewards_account_key = REWARDS_KEY.parse(matches);
            let protocol_key = PROTOCOL_KEY.parse(matches);
            let validator_vp_code_path = VALIDATOR_CODE_PATH.parse(matches);
            let rewards_vp_code_path = REWARDS_CODE_PATH.parse(matches);
            let unsafe_dont_encrypt = UNSAFE_DONT_ENCRYPT.parse(matches);
            Self {
                tx,
                source,
                account_key,
                consensus_key,
                rewards_account_key,
                protocol_key,
                validator_vp_code_path,
                rewards_vp_code_path,
                unsafe_dont_encrypt,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(SOURCE.def().about(
                    "The source account's address that signs the transaction.",
                ))
                .arg(VALIDATOR_ACCOUNT_KEY.def().about(
                    "A public key for the validator account. A new one will \
                     be generated if none given.",
                ))
                .arg(VALIDATOR_CONSENSUS_KEY.def().about(
                    "A consensus key for the validator account. A new one \
                     will be generated if none given.",
                ))
                .arg(REWARDS_KEY.def().about(
                    "A public key for the staking reward account. A new one \
                     will be generated if none given.",
                ))
                .arg(PROTOCOL_KEY.def().about(
                    "A public key for signing protocol transactions. A new \
                     one will be generated if none given.",
                ))
                .arg(VALIDATOR_CODE_PATH.def().about(
                    "The path to the validity predicate WASM code to be used \
                     for the validator account. Uses the default validator VP \
                     if none specified.",
                ))
                .arg(REWARDS_CODE_PATH.def().about(
                    "The path to the validity predicate WASM code to be used \
                     for the staking reward account. Uses the default staking \
                     reward VP if none specified.",
                ))
                .arg(UNSAFE_DONT_ENCRYPT.def().about(
                    "UNSAFE: Do not encrypt the generated keypairs. Do not \
                     use this for keys used in a live network.",
                ))
        }
    }

    /// Transaction to update a VP arguments
    #[derive(Clone, Debug)]
    pub struct TxUpdateVp {
        /// Common tx arguments
        pub tx: Tx,
        /// Path to the VP WASM code file
        pub vp_code_path: PathBuf,
        /// Address of the account whose VP is to be updated
        pub addr: WalletAddress,
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
    #[derive(Clone, Debug)]
    pub struct Bond {
        /// Common tx arguments
        pub tx: Tx,
        /// Validator address
        pub validator: WalletAddress,
        /// Amount of tokens to stake in a bond
        pub amount: token::Amount,
        /// Source address for delegations. For self-bonds, the validator is
        /// also the source.
        pub source: Option<WalletAddress>,
    }

    impl Args for Bond {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let validator = VALIDATOR.parse(matches);
            let amount = AMOUNT.parse(matches);
            let source = SOURCE_OPT.parse(matches);
            Self {
                tx,
                validator,
                amount,
                source,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(VALIDATOR.def().about("Validator address."))
                .arg(AMOUNT.def().about("Amount of tokens to stake in a bond."))
                .arg(SOURCE_OPT.def().about(
                    "Source address for delegations. For self-bonds, the \
                     validator is also the source.",
                ))
        }
    }

    /// Unbond arguments
    #[derive(Clone, Debug)]
    pub struct Unbond {
        /// Common tx arguments
        pub tx: Tx,
        /// Validator address
        pub validator: WalletAddress,
        /// Amount of tokens to unbond from a bond
        pub amount: token::Amount,
        /// Source address for unbonding from delegations. For unbonding from
        /// self-bonds, the validator is also the source
        pub source: Option<WalletAddress>,
    }

    impl Args for Unbond {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let validator = VALIDATOR.parse(matches);
            let amount = AMOUNT.parse(matches);
            let source = SOURCE_OPT.parse(matches);
            Self {
                tx,
                validator,
                amount,
                source,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(VALIDATOR.def().about("Validator address."))
                .arg(
                    AMOUNT
                        .def()
                        .about("Amount of tokens to unbond from a bond."),
                )
                .arg(SOURCE_OPT.def().about(
                    "Source address for unbonding from delegations. For \
                     unbonding from self-bonds, the validator is also the \
                     source.",
                ))
        }
    }

    /// Withdraw arguments
    #[derive(Clone, Debug)]
    pub struct Withdraw {
        /// Common tx arguments
        pub tx: Tx,
        /// Validator address
        pub validator: WalletAddress,
        /// Source address for withdrawing from delegations. For withdrawing
        /// from self-bonds, the validator is also the source
        pub source: Option<WalletAddress>,
    }

    impl Args for Withdraw {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let validator = VALIDATOR.parse(matches);
            let source = SOURCE_OPT.parse(matches);
            Self {
                tx,
                validator,
                source,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(VALIDATOR.def().about("Validator address."))
                .arg(SOURCE_OPT.def().about(
                    "Source address for withdrawing from delegations. For \
                     withdrawing from self-bonds, the validator is also the \
                     source.",
                ))
        }
    }

    // Transaction to create a new nft
    #[derive(Clone, Debug)]
    pub struct NftCreate {
        /// Common tx argumentsips
        pub tx: Tx,
        /// Path to the nft file description
        pub nft_data: PathBuf,
    }

    impl Args for NftCreate {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let data_path = DATA_PATH.parse(matches);

            Self {
                tx,
                nft_data: data_path,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(DATA_PATH.def().about("The path nft description file."))
        }
    }

    #[derive(Clone, Debug)]
    pub struct NftMint {
        /// Common tx arguments
        pub tx: Tx,
        /// The nft address
        pub nft_address: Address,
        /// The nft token description
        pub nft_data: PathBuf,
    }

    impl Args for NftMint {
        fn parse(matches: &ArgMatches) -> Self {
            let tx = Tx::parse(matches);
            let nft_address = NFT_ADDRESS.parse(matches);
            let data_path = DATA_PATH.parse(matches);

            Self {
                tx,
                nft_address,
                nft_data: data_path,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Tx>()
                .arg(NFT_ADDRESS.def().about("The nft address."))
                .arg(
                    DATA_PATH.def().about(
                        "The data path file that describes the nft tokens.",
                    ),
                )
        }
    }

    /// Query token balance(s)
    #[derive(Clone, Debug)]
    pub struct QueryBalance {
        /// Common query args
        pub query: Query,
        /// Address of an owner
        pub owner: Option<WalletAddress>,
        /// Address of a token
        pub token: Option<WalletAddress>,
    }

    impl Args for QueryBalance {
        fn parse(matches: &ArgMatches) -> Self {
            let query = Query::parse(matches);
            let owner = OWNER.parse(matches);
            let token = TOKEN_OPT.parse(matches);
            Self {
                query,
                owner,
                token,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Query>()
                .arg(
                    OWNER
                        .def()
                        .about("The account address whose balance to query."),
                )
                .arg(
                    TOKEN_OPT
                        .def()
                        .about("The token's address whose balance to query."),
                )
        }
    }

    /// Helper struct for generating intents
    #[derive(Debug, Clone, Deserialize)]
    pub struct ExchangeDefinition {
        /// The source address
        pub addr: String,
        /// The token to be sold
        pub token_sell: String,
        /// The minimum rate
        pub rate_min: String,
        /// The maximum amount of token to be sold
        pub max_sell: String,
        /// The token to be bought
        pub token_buy: String,
        /// The amount of token to be bought
        pub min_buy: String,
        /// The path to the wasm vp code
        pub vp_path: Option<String>,
    }

    impl TryFrom<ExchangeDefinition> for Exchange {
        type Error = &'static str;

        fn try_from(
            value: ExchangeDefinition,
        ) -> Result<Exchange, Self::Error> {
            let vp = if let Some(path) = value.vp_path {
                if let Ok(wasm) = std::fs::read(path.clone()) {
                    Some(wasm)
                } else {
                    eprintln!("File {} was not found.", path);
                    None
                }
            } else {
                None
            };

            let addr = Address::decode(value.addr)
                .expect("Addr should be a valid address");
            let token_buy = Address::decode(value.token_buy)
                .expect("Token_buy should be a valid address");
            let token_sell = Address::decode(value.token_sell)
                .expect("Token_sell should be a valid address");
            let min_buy = token::Amount::from_str(&value.min_buy)
                .expect("Min_buy must be convertible to number");
            let max_sell = token::Amount::from_str(&value.max_sell)
                .expect("Max_sell must be convertible to number");
            let rate_min = DecimalWrapper::from_str(&value.rate_min)
                .expect("Max_sell must be convertible to decimal.");

            Ok(Exchange {
                addr,
                token_sell,
                rate_min,
                max_sell,
                token_buy,
                min_buy,
                vp,
            })
        }
    }

    /// Query PoS bond(s)
    #[derive(Clone, Debug)]
    pub struct QueryBonds {
        /// Common query args
        pub query: Query,
        /// Address of an owner
        pub owner: Option<WalletAddress>,
        /// Address of a validator
        pub validator: Option<WalletAddress>,
    }

    impl Args for QueryBonds {
        fn parse(matches: &ArgMatches) -> Self {
            let query = Query::parse(matches);
            let owner = OWNER.parse(matches);
            let validator = VALIDATOR_OPT.parse(matches);
            Self {
                query,
                owner,
                validator,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Query>()
                .arg(
                    OWNER.def().about(
                        "The owner account address whose bonds to query.",
                    ),
                )
                .arg(
                    VALIDATOR_OPT
                        .def()
                        .about("The validator's address whose bonds to query."),
                )
        }
    }

    /// Query PoS voting power
    #[derive(Clone, Debug)]
    pub struct QueryVotingPower {
        /// Common query args
        pub query: Query,
        /// Address of a validator
        pub validator: Option<WalletAddress>,
        /// Epoch in which to find voting power
        pub epoch: Option<Epoch>,
    }

    impl Args for QueryVotingPower {
        fn parse(matches: &ArgMatches) -> Self {
            let query = Query::parse(matches);
            let validator = VALIDATOR_OPT.parse(matches);
            let epoch = EPOCH.parse(matches);
            Self {
                query,
                validator,
                epoch,
            }
        }

        fn def(app: App) -> App {
            app.add_args::<Query>()
                .arg(VALIDATOR_OPT.def().about(
                    "The validator's address whose voting power to query.",
                ))
                .arg(EPOCH.def().about(
                    "The epoch at which to query (last committed, if not \
                     specified).",
                ))
        }
    }

    /// Query PoS slashes
    #[derive(Clone, Debug)]
    pub struct QuerySlashes {
        /// Common query args
        pub query: Query,
        /// Address of a validator
        pub validator: Option<WalletAddress>,
    }

    impl Args for QuerySlashes {
        fn parse(matches: &ArgMatches) -> Self {
            let query = Query::parse(matches);
            let validator = VALIDATOR_OPT.parse(matches);
            Self { query, validator }
        }

        fn def(app: App) -> App {
            app.add_args::<Query>().arg(
                VALIDATOR_OPT
                    .def()
                    .about("The validator's address whose slashes to query."),
            )
        }
    }

    /// Intent arguments
    #[derive(Clone, Debug)]
    pub struct Intent {
        /// Gossip node address
        pub node_addr: Option<String>,
        /// Intent topic
        pub topic: Option<String>,
        /// Source address
        pub source: Option<WalletAddress>,
        /// Signing key
        pub signing_key: Option<WalletKeypair>,
        /// Exchanges description
        pub exchanges: Vec<Exchange>,
        /// The address of the ledger node as host:port
        pub ledger_address: TendermintAddress,
        /// Print output to stdout
        pub to_stdout: bool,
    }

    impl Args for Intent {
        fn parse(matches: &ArgMatches) -> Self {
            let node_addr = NODE_OPT.parse(matches);
            let data_path = DATA_PATH.parse(matches);
            let source = SOURCE_OPT.parse(matches);
            let signing_key = SIGNING_KEY_OPT.parse(matches);
            let to_stdout = TO_STDOUT.parse(matches);
            let topic = TOPIC_OPT.parse(matches);

            let file = File::open(&data_path).expect("File must exist.");
            let exchange_definitions: Vec<ExchangeDefinition> =
                serde_json::from_reader(file)
                    .expect("JSON was not well-formatted");

            let exchanges: Vec<Exchange> = exchange_definitions
                .iter()
                .map(|item| {
                    Exchange::try_from(item.clone()).expect(
                        "Conversion from ExchangeDefinition to Exchange \
                         should not fail.",
                    )
                })
                .collect();
            let ledger_address = LEDGER_ADDRESS_DEFAULT.parse(matches);

            Self {
                node_addr,
                topic,
                source,
                signing_key,
                exchanges,
                ledger_address,
                to_stdout,
            }
        }

        fn def(app: App) -> App {
            app.arg(
                NODE_OPT
                    .def()
                    .about("The gossip node address.")
                    .conflicts_with(TO_STDOUT.name),
            )
            .arg(DATA_PATH.def().about(
                "The data of the intent, that contains all value necessary \
                 for the matchmaker.",
            ))
            .arg(
                SOURCE_OPT
                    .def()
                    .about(
                        "Sign the intent with the key of a given address or \
                         address alias from your wallet.",
                    )
                    .conflicts_with(SIGNING_KEY_OPT.name),
            )
            .arg(
                SIGNING_KEY_OPT
                    .def()
                    .about(
                        "Sign the intent with the key for the given public \
                         key, public key hash or alias from your wallet.",
                    )
                    .conflicts_with(SOURCE_OPT.name),
            )
            .arg(LEDGER_ADDRESS_DEFAULT.def().about(LEDGER_ADDRESS_ABOUT))
            .arg(
                TOPIC_OPT
                    .def()
                    .about("The subnetwork where the intent should be sent to.")
                    .conflicts_with(TO_STDOUT.name),
            )
            .arg(
                TO_STDOUT
                    .def()
                    .about(
                        "Echo the serialized intent to stdout. Note that with \
                         this option, the intent won't be submitted to the \
                         intent gossiper RPC.",
                    )
                    .conflicts_with_all(&[NODE_OPT.name, TOPIC.name]),
            )
        }
    }

    /// Subscribe intent topic arguments
    #[derive(Clone, Debug)]
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

    #[derive(Clone, Debug)]
    pub struct GossipRun {
        pub addr: Option<Multiaddr>,
        pub rpc: Option<SocketAddr>,
    }

    impl Args for GossipRun {
        fn parse(matches: &ArgMatches) -> Self {
            let addr = MULTIADDR_OPT.parse(matches);
            let rpc = RPC_SOCKET_ADDR.parse(matches);
            Self { addr, rpc }
        }

        fn def(app: App) -> App {
            app.arg(
                MULTIADDR_OPT
                    .def()
                    .about("Gossip service address as host:port."),
            )
            .arg(RPC_SOCKET_ADDR.def().about("Enable RPC service."))
        }
    }

    #[derive(Clone, Debug)]
    pub struct Matchmaker {
        pub matchmaker_path: Option<PathBuf>,
        pub tx_code_path: Option<PathBuf>,
        pub intent_gossiper_addr: SocketAddr,
        pub ledger_addr: TendermintAddress,
        pub tx_signing_key: WalletKeypair,
        pub tx_source_address: WalletAddress,
    }

    impl Args for Matchmaker {
        fn parse(matches: &ArgMatches) -> Self {
            let intent_gossiper_addr = INTENT_GOSSIPER_ADDR.parse(matches);
            let matchmaker_path = MATCHMAKER_PATH.parse(matches);
            let tx_code_path = TX_CODE_PATH.parse(matches);
            let ledger_addr = LEDGER_ADDRESS_DEFAULT.parse(matches);
            let tx_signing_key = SIGNING_KEY.parse(matches);
            let tx_source_address = SOURCE.parse(matches);
            Self {
                intent_gossiper_addr,
                matchmaker_path,
                tx_code_path,
                ledger_addr,
                tx_signing_key,
                tx_source_address,
            }
        }

        fn def(app: App) -> App {
            app.arg(INTENT_GOSSIPER_ADDR.def().about(
                "Intent Gossiper endpoint for matchmaker connections as \
                 \"{host}:{port}\".",
            ))
            .arg(MATCHMAKER_PATH.def().about(
                "The file name of the matchmaker compiled to a dynamic \
                 library (the filename extension is optional).",
            ))
            .arg(
                TX_CODE_PATH
                    .def()
                    .about("The transaction code to use with the matchmaker."),
            )
            .arg(LEDGER_ADDRESS_DEFAULT.def().about(
                "The address of the ledger as \"{scheme}://{host}:{port}\" \
                 that the matchmaker must send transactions to. If the scheme \
                 is not supplied, it is assumed to be TCP.",
            ))
            .arg(SIGNING_KEY.def().about(
                "Sign the transactions created by the matchmaker with the key \
                 for the given public key, public key hash or alias from your \
                 wallet.",
            ))
            .arg(SOURCE.def().about(
                "Source address or alias of an address of the transactions \
                 created by the matchmaker. This must be matching the signing \
                 key.",
            ))
        }
    }

    /// Common transaction arguments
    #[derive(Clone, Debug)]
    pub struct Tx {
        /// Simulate applying the transaction
        pub dry_run: bool,
        /// Submit the transaction even if it doesn't pass client checks
        pub force: bool,
        /// Do not wait for the transaction to be added to the blockchain
        pub broadcast_only: bool,
        /// The address of the ledger node as host:port
        pub ledger_address: TendermintAddress,
        /// If any new account is initialized by the tx, use the given alias to
        /// save it in the wallet.
        pub initialized_account_alias: Option<String>,
        /// The amount being payed to include the transaction
        pub fee_amount: token::Amount,
        /// The token in which the fee is being paid
        pub fee_token: WalletAddress,
        /// The max amount of gas used to process tx
        pub gas_limit: GasLimit,
        /// Sign the tx with the key for the given alias from your wallet
        pub signing_key: Option<WalletKeypair>,
        /// Sign the tx with the keypair of the public key of the given address
        pub signer: Option<WalletAddress>,
    }

    impl Args for Tx {
        fn def(app: App) -> App {
            app.arg(
                DRY_RUN_TX
                    .def()
                    .about("Simulate the transaction application."),
            )
            .arg(FORCE.def().about(
                "Submit the transaction even if it doesn't pass client checks.",
            ))
            .arg(BROADCAST_ONLY.def().about(
                "Do not wait for the transaction to be applied. This will \
                 return once the transaction is added to the mempool.",
            ))
            .arg(LEDGER_ADDRESS_DEFAULT.def().about(LEDGER_ADDRESS_ABOUT))
            .arg(ALIAS_OPT.def().about(
                "If any new account is initialized by the tx, use the given \
                 alias to save it in the wallet. If multiple accounts are \
                 initialized, the alias will be the prefix of each new \
                 address joined with a number.",
            ))
            .arg(FEE_AMOUNT.def().about(
                "The amount being paid for the inclusion of this transaction",
            ))
            .arg(FEE_TOKEN.def().about("The token for paying the fee"))
            .arg(
                GAS_LIMIT.def().about(
                    "The maximum amount of gas needed to run transaction",
                ),
            )
            .arg(
                SIGNING_KEY_OPT
                    .def()
                    .about(
                        "Sign the transaction with the key for the given \
                         public key, public key hash or alias from your \
                         wallet.",
                    )
                    .conflicts_with(SIGNER.name),
            )
            .arg(
                SIGNER
                    .def()
                    .about(
                        "Sign the transaction with the keypair of the public \
                         key of the given address.",
                    )
                    .conflicts_with(SIGNING_KEY_OPT.name),
            )
        }

        fn parse(matches: &ArgMatches) -> Self {
            let dry_run = DRY_RUN_TX.parse(matches);
            let force = FORCE.parse(matches);
            let broadcast_only = BROADCAST_ONLY.parse(matches);
            let ledger_address = LEDGER_ADDRESS_DEFAULT.parse(matches);
            let initialized_account_alias = ALIAS_OPT.parse(matches);
            let fee_amount = FEE_AMOUNT.parse(matches);
            let fee_token = FEE_TOKEN.parse(matches);
            let gas_limit = GAS_LIMIT.parse(matches).into();

            let signing_key = SIGNING_KEY_OPT.parse(matches);
            let signer = SIGNER.parse(matches);
            Self {
                dry_run,
                force,
                broadcast_only,
                ledger_address,
                initialized_account_alias,
                fee_amount,
                fee_token,
                gas_limit,
                signing_key,
                signer,
            }
        }
    }

    /// Common query arguments
    #[derive(Clone, Debug)]
    pub struct Query {
        /// The address of the ledger node as host:port
        pub ledger_address: TendermintAddress,
    }

    impl Args for Query {
        fn def(app: App) -> App {
            app.arg(LEDGER_ADDRESS_DEFAULT.def().about(LEDGER_ADDRESS_ABOUT))
        }

        fn parse(matches: &ArgMatches) -> Self {
            let ledger_address = LEDGER_ADDRESS_DEFAULT.parse(matches);
            Self { ledger_address }
        }
    }

    /// Wallet generate key and implicit address arguments
    #[derive(Clone, Debug)]
    pub struct KeyAndAddressGen {
        /// Key alias
        pub alias: Option<String>,
        /// Don't encrypt the keypair
        pub unsafe_dont_encrypt: bool,
    }

    impl Args for KeyAndAddressGen {
        fn parse(matches: &ArgMatches) -> Self {
            let alias = ALIAS_OPT.parse(matches);
            let unsafe_dont_encrypt = UNSAFE_DONT_ENCRYPT.parse(matches);
            Self {
                alias,
                unsafe_dont_encrypt,
            }
        }

        fn def(app: App) -> App {
            app.arg(ALIAS_OPT.def().about(
                "The key and address alias. If none provided, the alias will \
                 be the public key hash.",
            ))
            .arg(UNSAFE_DONT_ENCRYPT.def().about(
                "UNSAFE: Do not encrypt the keypair. Do not use this for keys \
                 used in a live network.",
            ))
        }
    }

    /// Wallet key lookup arguments
    #[derive(Clone, Debug)]
    pub struct KeyFind {
        pub public_key: Option<common::PublicKey>,
        pub alias: Option<String>,
        pub value: Option<String>,
        pub unsafe_show_secret: bool,
    }

    impl Args for KeyFind {
        fn parse(matches: &ArgMatches) -> Self {
            let public_key = RAW_PUBLIC_KEY_OPT.parse(matches);
            let alias = ALIAS_OPT.parse(matches);
            let value = VALUE.parse(matches);
            let unsafe_show_secret = UNSAFE_SHOW_SECRET.parse(matches);

            Self {
                public_key,
                alias,
                value,
                unsafe_show_secret,
            }
        }

        fn def(app: App) -> App {
            app.arg(
                RAW_PUBLIC_KEY_OPT
                    .def()
                    .about("A public key associated with the keypair.")
                    .conflicts_with_all(&[ALIAS_OPT.name, VALUE.name]),
            )
            .arg(
                ALIAS_OPT
                    .def()
                    .about("An alias associated with the keypair.")
                    .conflicts_with(VALUE.name),
            )
            .arg(
                VALUE.def().about(
                    "A public key or alias associated with the keypair.",
                ),
            )
            .arg(
                UNSAFE_SHOW_SECRET
                    .def()
                    .about("UNSAFE: Print the secret key."),
            )
        }
    }

    /// Wallet list keys arguments
    #[derive(Clone, Debug)]
    pub struct KeyList {
        pub decrypt: bool,
        pub unsafe_show_secret: bool,
    }

    impl Args for KeyList {
        fn parse(matches: &ArgMatches) -> Self {
            let decrypt = DECRYPT.parse(matches);
            let unsafe_show_secret = UNSAFE_SHOW_SECRET.parse(matches);
            Self {
                decrypt,
                unsafe_show_secret,
            }
        }

        fn def(app: App) -> App {
            app.arg(DECRYPT.def().about("Decrypt keys that are encrypted."))
                .arg(
                    UNSAFE_SHOW_SECRET
                        .def()
                        .about("UNSAFE: Print the secret keys."),
                )
        }
    }

    /// Wallet key export arguments
    #[derive(Clone, Debug)]
    pub struct KeyExport {
        pub alias: String,
    }

    impl Args for KeyExport {
        fn parse(matches: &ArgMatches) -> Self {
            let alias = ALIAS.parse(matches);

            Self { alias }
        }

        fn def(app: App) -> App {
            app.arg(
                ALIAS
                    .def()
                    .about("The alias of the key you wish to export."),
            )
        }
    }

    /// Wallet address lookup arguments
    #[derive(Clone, Debug)]
    pub struct AddressFind {
        pub alias: String,
    }

    impl Args for AddressFind {
        fn parse(matches: &ArgMatches) -> Self {
            let alias = ALIAS.parse(matches);
            Self { alias }
        }

        fn def(app: App) -> App {
            app.arg(
                ALIAS_OPT
                    .def()
                    .about("An alias associated with the address."),
            )
        }
    }

    /// Wallet address add arguments
    #[derive(Clone, Debug)]
    pub struct AddressAdd {
        pub alias: String,
        pub address: Address,
    }

    impl Args for AddressAdd {
        fn parse(matches: &ArgMatches) -> Self {
            let alias = ALIAS.parse(matches);
            let address = RAW_ADDRESS.parse(matches);
            Self { alias, address }
        }

        fn def(app: App) -> App {
            app.arg(
                ALIAS
                    .def()
                    .about("An alias to be associated with the address."),
            )
            .arg(
                RAW_ADDRESS
                    .def()
                    .about("The bech32m encoded address string."),
            )
        }
    }

    #[derive(Clone, Debug)]
    pub struct JoinNetwork {
        pub chain_id: ChainId,
        pub genesis_validator: Option<String>,
    }

    impl Args for JoinNetwork {
        fn parse(matches: &ArgMatches) -> Self {
            let chain_id = CHAIN_ID.parse(matches);
            let genesis_validator = GENESIS_VALIDATOR.parse(matches);
            Self {
                chain_id,
                genesis_validator,
            }
        }

        fn def(app: App) -> App {
            app.arg(CHAIN_ID.def().about("The chain ID. The chain must be known in the https://github.com/heliaxdev/anoma-network-config repository."))
                .arg(GENESIS_VALIDATOR.def().about("The alias of the genesis validator that you want to set up as."))
        }
    }

    #[derive(Clone, Debug)]
    pub struct InitNetwork {
        pub genesis_path: PathBuf,
        pub wasm_checksums_path: PathBuf,
        pub chain_id_prefix: ChainIdPrefix,
        pub unsafe_dont_encrypt: bool,
        pub consensus_timeout_commit: Timeout,
        pub localhost: bool,
        pub allow_duplicate_ip: bool,
        pub dont_archive: bool,
    }

    impl Args for InitNetwork {
        fn parse(matches: &ArgMatches) -> Self {
            let genesis_path = GENESIS_PATH.parse(matches);
            let wasm_checksums_path = WASM_CHECKSUMS_PATH.parse(matches);
            let chain_id_prefix = CHAIN_ID_PREFIX.parse(matches);
            let unsafe_dont_encrypt = UNSAFE_DONT_ENCRYPT.parse(matches);
            let consensus_timeout_commit =
                CONSENSUS_TIMEOUT_COMMIT.parse(matches);
            let localhost = LOCALHOST.parse(matches);
            let allow_duplicate_ip = ALLOW_DUPLICATE_IP.parse(matches);
            let dont_archive = DONT_ARCHIVE.parse(matches);
            Self {
                genesis_path,
                wasm_checksums_path,
                chain_id_prefix,
                unsafe_dont_encrypt,
                consensus_timeout_commit,
                localhost,
                allow_duplicate_ip,
                dont_archive,
            }
        }

        fn def(app: App) -> App {
            app.arg(
                GENESIS_PATH.def().about(
                    "Path to the preliminary genesis configuration file.",
                ),
            )
            .arg(
                WASM_CHECKSUMS_PATH
                    .def()
                    .about("Path to the WASM checksums file."),
            )
            .arg(CHAIN_ID_PREFIX.def().about(
                "The chain ID prefix. Up to 19 alphanumeric, '.', '-' or '_' \
                 characters.",
            ))
            .arg(UNSAFE_DONT_ENCRYPT.def().about(
                "UNSAFE: Do not encrypt the generated keypairs. Do not use \
                 this for keys used in a live network.",
            ))
            .arg(CONSENSUS_TIMEOUT_COMMIT.def().about(
                "The Tendermint consensus timeout_commit configuration as \
                 e.g. `1s` or `1000ms`. Defaults to 10 seconds.",
            ))
            .arg(LOCALHOST.def().about(
                "Use localhost address for P2P and RPC connections for the \
                 validators ledger and intent gossip nodes",
            ))
            .arg(ALLOW_DUPLICATE_IP.def().about(
                "Toggle to disable guard against peers connecting from the \
                 same IP. This option shouldn't be used in mainnet.",
            ))
            .arg(
                DONT_ARCHIVE
                    .def()
                    .about("Do NOT create the release archive."),
            )
        }
    }

    #[derive(Clone, Debug)]
    pub struct InitGenesisValidator {
        pub alias: String,
        pub unsafe_dont_encrypt: bool,
    }

    impl Args for InitGenesisValidator {
        fn parse(matches: &ArgMatches) -> Self {
            let alias = ALIAS.parse(matches);
            let unsafe_dont_encrypt = UNSAFE_DONT_ENCRYPT.parse(matches);
            Self {
                alias,
                unsafe_dont_encrypt,
            }
        }

        fn def(app: App) -> App {
            app.arg(ALIAS.def().about("The validator address alias."))
                .arg(UNSAFE_DONT_ENCRYPT.def().about(
                    "UNSAFE: Do not encrypt the generated keypairs. Do not \
                     use this for keys used in a live network.",
                ))
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
        (Some(cmd), Some(raw_sub)) => return (cmd, raw_sub),
        _ => {
            anoma_app().print_help().unwrap();
        }
    }
    safe_exit(2);
}

pub fn anoma_node_cli() -> (cmds::AnomaNode, Context) {
    let app = anoma_node_app();
    cmds::AnomaNode::parse_or_print_help(app)
}

pub enum AnomaClient {
    WithoutContext(cmds::Utils, args::Global),
    WithContext(Box<(cmds::AnomaClientWithContext, Context)>),
}

pub fn anoma_client_cli() -> AnomaClient {
    let app = anoma_client_app();
    let mut app = cmds::AnomaClient::add_sub(app);
    let matches = app.clone().get_matches();
    match Cmd::parse(&matches) {
        Some(cmd) => {
            let global_args = args::Global::parse(&matches);
            match cmd {
                cmds::AnomaClient::WithContext(sub_cmd) => {
                    let context = Context::new(global_args);
                    AnomaClient::WithContext(Box::new((sub_cmd, context)))
                }
                cmds::AnomaClient::WithoutContext(sub_cmd) => {
                    AnomaClient::WithoutContext(sub_cmd, global_args)
                }
            }
        }
        None => {
            app.print_help().unwrap();
            safe_exit(2);
        }
    }
}

pub fn anoma_wallet_cli() -> (cmds::AnomaWallet, Context) {
    let app = anoma_wallet_app();
    cmds::AnomaWallet::parse_or_print_help(app)
}

fn anoma_app() -> App {
    let app = App::new(APP_NAME)
        .version(anoma_version())
        .author(crate_authors!("\n"))
        .about("Anoma command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp);
    cmds::Anoma::add_sub(args::Global::def(app))
}

fn anoma_node_app() -> App {
    let app = App::new(APP_NAME)
        .version(anoma_version())
        .author(crate_authors!("\n"))
        .about("Anoma node command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp);
    cmds::AnomaNode::add_sub(args::Global::def(app))
}

fn anoma_client_app() -> App {
    let app = App::new(APP_NAME)
        .version(anoma_version())
        .author(crate_authors!("\n"))
        .about("Anoma client command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp);
    cmds::AnomaClient::add_sub(args::Global::def(app))
}

fn anoma_wallet_app() -> App {
    let app = App::new(APP_NAME)
        .version(anoma_version())
        .author(crate_authors!("\n"))
        .about("Anoma wallet command line interface.")
        .setting(AppSettings::SubcommandRequiredElseHelp);
    cmds::AnomaWallet::add_sub(args::Global::def(app))
}
