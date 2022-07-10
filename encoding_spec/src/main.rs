//! Encoding spec markdown generator.
//!
//! When ran, this generator will:
//! - Get schema for all the types to be included in encoding docs
//! - Merge their definitions
//! - For each schema's declaration, look-up top-level definitions and format to
//!   md table
//! - For each non-top-level definition, format to md table
//!
//! Rebuild on changes with:
//! `cargo watch -x "run --bin anoma_encoding_spec" -i docs`

#![doc(html_favicon_url = "https://dev.anoma.net/master/favicon.png")]
#![doc(html_logo_url = "https://dev.anoma.net/master/rustdoc-logo.png")]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]

use std::collections::HashSet;
use std::io::Write;

use anoma::ledger::parameters::Parameters;
use anoma::proto::SignedTxData;
use anoma::types::address::Address;
use anoma::types::key::ed25519::{PublicKey, Signature};
use anoma::types::storage::{self, Epoch};
use anoma::types::transaction::pos;
use anoma::types::{token, transaction};
use borsh::{schema, BorshSchema};
use itertools::Itertools;
use lazy_static::lazy_static;
use madato::types::TableRow;

/// This generator will write output into this `docs` file.
const OUTPUT_PATH: &str = "docs/src/specs/encoding/generated-borsh-spec.md";

lazy_static! {
    /// Borsh types may be used by declarations. These are displayed differently in the [`md_fmt_type`].
    static ref BORSH_TYPES: HashSet<&'static str> =
        HashSet::from_iter([
            "string",
            "bool",
            "u8",
            "u16",
            "u32",
            "u64",
            "u128",
            "i8",
            "i16",
            "i32",
            "i64",
            "i128",
            "f32",
            "f64",
            // unit `()`
            "nil",
        ]);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = std::fs::File::create(OUTPUT_PATH).unwrap();

    write_generated_code_notice(&mut file)?;

    // Top-level definitions are displayed at the top
    let address_schema = Address::schema_container();
    let token_amount_schema = token::Amount::schema_container();
    let epoch_schema = Epoch::schema_container();
    let parameters_schema = Parameters::schema_container();
    // TODO update after <https://github.com/anoma/anoma/issues/225>
    let public_key_schema = PublicKey::schema_container();
    // TODO update after <https://github.com/anoma/anoma/issues/225>
    let signature_schema = Signature::schema_container();
    let signed_tx_data_schema = SignedTxData::schema_container();
    let init_account_schema = transaction::InitAccount::schema_container();
    let init_validator_schema = transaction::InitValidator::schema_container();
    let token_transfer_schema = token::Transfer::schema_container();
    let update_vp_schema = transaction::UpdateVp::schema_container();
    let pos_bond_schema = pos::Bond::schema_container();
    let pos_withdraw_schema = pos::Withdraw::schema_container();
    let wrapper_tx_schema = transaction::WrapperTx::schema_container();
    // TODO derive BorshSchema after <https://github.com/near/borsh-rs/issues/82>
    // let tx_result_schema = transaction::TxResult::schema_container();
    let tx_type_schema = transaction::TxType::schema_container();
    let prefix_value_schema = storage::PrefixValue::schema_container();

    // PoS
    // TODO add after <https://github.com/anoma/anoma/issues/439>
    // TODO imported from `use anoma::ledger::pos::Bonds;`
    // let pos_bonds_schema = Bonds::schema_container();

    // Merge type definitions
    let mut definitions = address_schema.definitions;
    // TODO check for conflicts (same name, different declaration)
    definitions.extend(token_amount_schema.definitions);
    definitions.extend(epoch_schema.definitions);
    definitions.extend(parameters_schema.definitions);
    definitions.extend(public_key_schema.definitions);
    definitions.extend(signature_schema.definitions);
    definitions.extend(signed_tx_data_schema.definitions);
    definitions.extend(init_account_schema.definitions);
    definitions.extend(init_validator_schema.definitions);
    definitions.extend(token_transfer_schema.definitions);
    definitions.extend(update_vp_schema.definitions);
    definitions.extend(pos_bond_schema.definitions);
    definitions.extend(pos_withdraw_schema.definitions);
    definitions.extend(wrapper_tx_schema.definitions);
    // definitions.extend(tx_result_schema.definitions);
    definitions.extend(tx_type_schema.definitions);
    definitions.extend(prefix_value_schema.definitions);
    // definitions.extend(pos_bonds_schema.definitions);
    let mut tables: Vec<Table> = Vec::with_capacity(definitions.len());

    // Add the top-level definitions first
    let address_definition =
        definitions.remove(&address_schema.declaration).unwrap();
    let address_table =
        definition_to_table(address_schema.declaration, address_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/address/enum.Address.html");
    tables.push(address_table);

    let token_amount_definition = definitions
        .remove(&token_amount_schema.declaration)
        .unwrap();
    let token_amount_table = definition_to_table(
        token_amount_schema.declaration,
        token_amount_definition,
    ).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/token/struct.Amount.html");
    tables.push(token_amount_table);

    let epoch_definition =
        definitions.remove(&epoch_schema.declaration).unwrap();
    let epoch_table =
        definition_to_table(epoch_schema.declaration, epoch_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/storage/struct.Epoch.html");
    tables.push(epoch_table);

    let parameters_definition =
        definitions.remove(&parameters_schema.declaration).unwrap();
    let parameters_table =
        definition_to_table(parameters_schema.declaration, parameters_definition).with_rust_doc_link("file:///Users/tz/dev/anoma/target/doc/anoma/ledger/parameters/struct.Parameters.html");
    tables.push(parameters_table);

    let public_key_definition =
        definitions.remove(&public_key_schema.declaration).unwrap();
    let public_key_table =
        definition_to_table(public_key_schema.declaration, public_key_definition).with_rust_doc_link(
            // TODO update after <https://github.com/anoma/anoma/issues/225>
            "https://dev.anoma.net/master/rustdoc/anoma/types/key/ed25519/struct.PublicKey.html");
    tables.push(public_key_table);

    let signature_definition =
        definitions.remove(&signature_schema.declaration).unwrap();
    let signature_table =
        definition_to_table(signature_schema.declaration, signature_definition).with_rust_doc_link(
            // TODO update after <https://github.com/anoma/anoma/issues/225>
            "https://dev.anoma.net/master/rustdoc/anoma/types/key/ed25519/struct.Signature.html");
    tables.push(signature_table);

    let signed_tx_data_definition = definitions
        .remove(&signed_tx_data_schema.declaration)
        .unwrap();
    let signed_tx_data_table =
        definition_to_table(signed_tx_data_schema.declaration, signed_tx_data_definition).with_rust_doc_link(
            // TODO update after <https://github.com/anoma/anoma/issues/225>
            "https://dev.anoma.net/master/rustdoc/anoma/types/key/ed25519/struct.SignedTxData.html");
    tables.push(signed_tx_data_table);

    let init_account_definition = definitions
        .remove(&init_account_schema.declaration)
        .unwrap();
    let init_account_table = definition_to_table(
        init_account_schema.declaration,
        init_account_definition,
    ).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/struct.InitAccount.html");
    tables.push(init_account_table);

    let init_validator_definition = definitions
        .remove(&init_validator_schema.declaration)
        .unwrap();
    let init_validator_table = definition_to_table(
        init_validator_schema.declaration,
        init_validator_definition,
    ).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/struct.InitValidator.html");
    tables.push(init_validator_table);

    let token_transfer_definition = definitions
        .remove(&token_transfer_schema.declaration)
        .unwrap();
    let token_transfer_table = definition_to_table(
        token_transfer_schema.declaration,
        token_transfer_definition,
    ).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/token/struct.Transfer.html");
    tables.push(token_transfer_table);

    let update_vp_definition =
        definitions.remove(&update_vp_schema.declaration).unwrap();
    let update_vp_table =
        definition_to_table(update_vp_schema.declaration, update_vp_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/struct.UpdateVp.html");
    tables.push(update_vp_table);

    let pos_bond_definition =
        definitions.remove(&pos_bond_schema.declaration).unwrap();
    let pos_bond_table =
        definition_to_table(pos_bond_schema.declaration, pos_bond_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/pos/struct.Bond.html");
    tables.push(pos_bond_table);

    let pos_withdraw_definition = definitions
        .remove(&pos_withdraw_schema.declaration)
        .unwrap();
    let pos_withdraw_table = definition_to_table(
        pos_withdraw_schema.declaration,
        pos_withdraw_definition,
    ).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/pos/struct.Withdraw.html");
    tables.push(pos_withdraw_table);

    let wrapper_tx_definition =
        definitions.remove(&wrapper_tx_schema.declaration).unwrap();
    let wrapper_tx_table = definition_to_table(
        wrapper_tx_schema.declaration,
        wrapper_tx_definition,
    ).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/wrapper/wrapper_tx/struct.WrapperTx.html");
    tables.push(wrapper_tx_table);

    // let tx_result_definition =
    //     definitions.remove(&tx_result_schema.declaration).unwrap();
    // let tx_result_table =
    //     definition_to_table(tx_result_schema.declaration,
    // tx_result_definition).with_rust_doc_link("TODO");
    // tables.push(tx_result_table);

    let tx_type_definition =
        definitions.remove(&tx_type_schema.declaration).unwrap();
    let tx_type_table =
        definition_to_table(tx_type_schema.declaration, tx_type_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/tx_types/enum.TxType.html");
    tables.push(tx_type_table);

    let prefix_value_definition = definitions
        .remove(&prefix_value_schema.declaration)
        .unwrap();
    let prefix_value_table =
        definition_to_table(prefix_value_schema.declaration, prefix_value_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/types/transaction/prefix_values/enum.TxType.html");
    tables.push(prefix_value_table);

    // Add PoS definitions
    // let pos_bonds_definition =
    //     definitions.remove(&pos_bonds_schema.declaration).unwrap();
    // let pos_bonds_table =
    // definition_to_table(pos_bonds_schema.declaration, pos_bonds_definition).with_rust_doc_link("https://dev.anoma.net/master/rustdoc/anoma/ledger/pos/type.Bonds.html");
    // tables.push(pos_bonds_table);

    // Then add the rest of definitions sorted by their names
    for (declaration, defition) in definitions
        .into_iter()
        .sorted_by_key(|(key, _val)| key.clone())
    {
        tables.push(definition_to_table(declaration, defition))
    }

    // Print the tables to markdown
    for table in tables {
        writeln!(file, "#### {}", escape_html(table.name))?;
        writeln!(file)?;
        writeln!(file, "{}", table.desc)?;
        writeln!(file)?;
        if let Some(rows) = table.rows {
            let md_table = madato::mk_table(&rows[..], &None);
            writeln!(file, "{}", md_table)?;
            writeln!(file)?;
        }
    }

    writeln!(file)?;
    write_generated_code_notice(&mut file)?;

    Ok(())
}

struct Table {
    name: String,
    desc: String,
    rows: Option<madato::types::Table<String, String>>,
}

fn definition_to_table(name: String, def: schema::Definition) -> Table {
    let (desc, rows) = match def {
        schema::Definition::Array { length, elements } => {
            let rows = None;
            let desc = format!(
                "Fixed-size array with {} elements of {}",
                length,
                md_fmt_type(elements)
            );
            (desc, rows)
        }
        schema::Definition::Sequence { elements } => {
            let rows = None;
            let desc =
                format!("Dynamic-size array of {}", md_fmt_type(elements));
            (desc, rows)
        }
        schema::Definition::Tuple { elements } => {
            let rows = None;
            let desc = format!(
                "Tuple of ({})",
                elements.into_iter().fold(String::new(), |acc, element| {
                    if acc.is_empty() {
                        md_fmt_type(element)
                    } else {
                        format!("{}, {}", acc, md_fmt_type(element))
                    }
                })
            );
            (desc, rows)
        }
        schema::Definition::Enum { variants } => {
            let mut rows = madato::types::Table::default();
            // build rows for: Variant, Name, Type
            for (variant, (name, type_name)) in variants.iter().enumerate() {
                rows.push(TableRow::from_iter([
                    ("Prefix byte".into(), variant.to_string()),
                    ("Name".into(), name.clone()),
                    ("Type".into(), md_fmt_type(type_name)),
                ]));
            }
            ("Enum".into(), Some(rows))
        }
        schema::Definition::Struct { fields } => {
            match fields {
                schema::Fields::NamedFields(fields) => {
                    let mut rows = madato::types::Table::default();
                    // build rows for: Position, Name, Type
                    for (variant, (name, type_name)) in
                        fields.iter().enumerate()
                    {
                        rows.push(TableRow::from_iter([
                            ("Position".into(), variant.to_string()),
                            ("Name".into(), name.clone()),
                            ("Type".into(), md_fmt_type(type_name)),
                        ]));
                    }
                    ("Struct with named fields".into(), Some(rows))
                }
                schema::Fields::UnnamedFields(fields) => {
                    let mut rows = madato::types::Table::default();
                    // build rows for: Field, Type
                    for (variant, type_name) in fields.iter().enumerate() {
                        rows.push(TableRow::from_iter([
                            ("Position".into(), variant.to_string()),
                            ("Type".into(), md_fmt_type(type_name)),
                        ]));
                    }
                    ("Struct with unnamed fields".into(), Some(rows))
                }
                schema::Fields::Empty => ("Empty struct (unit)".into(), None),
            }
        }
    };
    Table { name, desc, rows }
}

/// Format a type to markdown. For internal types, adds anchors.
fn md_fmt_type(type_name: impl AsRef<str>) -> String {
    if BORSH_TYPES.contains(type_name.as_ref()) {
        let type_name = escape_html(type_name);
        format!("{} (native type)", type_name)
    } else {
        let type_link = escape_fragment_anchor(&type_name);
        let type_name = escape_html(type_name);
        format!("[{}](#{})", type_name, type_link)
    }
}

fn write_generated_code_notice(
    file: &mut std::fs::File,
) -> Result<(), Box<dyn std::error::Error>> {
    writeln!(
        file,
        "<!--- THIS PAGE IS GENERATED FROM CODE: {}. Do not edit manually! -->",
        std::file!()
    )?;
    Ok(())
}

/// Escape a type for markdown (rendered as HTML)
fn escape_html(string: impl AsRef<str>) -> String {
    string.as_ref().replace('>', "&gt;").replace('<', "&lt;")
}

/// Escape a link to another type on the page
fn escape_fragment_anchor(string: impl AsRef<str>) -> String {
    // mdBook turns headings fragment links to lowercase
    string
        .as_ref()
        .replace('>', "")
        .replace('<', "")
        .replace(',', "")
        .replace(' ', "-")
        .to_ascii_lowercase()
}

impl Table {
    /// Add a link to rust-docs
    fn with_rust_doc_link(mut self, link: impl AsRef<str>) -> Self {
        self.desc = format!("{} ([rust-doc]({}))", self.desc, link.as_ref());
        self
    }
}
