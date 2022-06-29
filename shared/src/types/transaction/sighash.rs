use blake2b_simd::{Hash as Blake2bHash, Params as Blake2bParams};
use byteorder::{LittleEndian, WriteBytesExt};
use ff::PrimeField;
use group::GroupEncoding;
use masp_primitives::asset_type::AssetType;
use super::legacy::Script;
use masp_primitives::transaction::{
    components::{TxIn as Ti, TxOut as To},
    Transaction, TransactionData, OVERWINTER_VERSION_GROUP_ID, SAPLING_TX_VERSION,
    SAPLING_VERSION_GROUP_ID,
};
use super::components::{TxIn, TxOut};
use masp_primitives::consensus;

const MASP_SIGHASH_PERSONALIZATION_PREFIX: &[u8; 12] = b"Masp_SigHash";
const MASP_PREVOUTS_HASH_PERSONALIZATION: &[u8; 16] = b"Masp_PrevoutHash";
const MASP_SEQUENCE_HASH_PERSONALIZATION: &[u8; 16] = b"Masp_SequencHash";
pub(crate) const MASP_OUTPUTS_HASH_PERSONALIZATION: &[u8; 16] = b"Masp_OutputsHash";
const MASP_JOINSPLITS_HASH_PERSONALIZATION: &[u8; 16] = b"Masp_JSplitsHash";
const MASP_SHIELDED_SPENDS_HASH_PERSONALIZATION: &[u8; 16] = b"Masp_SSpendsHash";
const MASP_SHIELDED_CONVERTS_HASH_PERSONALIZATION: &[u8; 16] = b"MaspSConvertHash";
const MASP_SHIELDED_OUTPUTS_HASH_PERSONALIZATION: &[u8; 16] = b"Masp_SOutputHash";

pub const SIGHASH_ALL: u32 = 1;
const SIGHASH_NONE: u32 = 2;
const SIGHASH_SINGLE: u32 = 3;
const SIGHASH_MASK: u32 = 0x1f;
const SIGHASH_ANYONECANPAY: u32 = 0x80;

macro_rules! update_u32 {
    ($h:expr, $value:expr, $tmp:expr) => {
        (&mut $tmp[..4]).write_u32::<LittleEndian>($value).unwrap();
        $h.update(&$tmp[..4]);
    };
}

macro_rules! update_hash {
    ($h:expr, $cond:expr, $value:expr) => {
        if $cond {
            $h.update(&$value.as_ref());
        } else {
            $h.update(&[0; 32]);
        }
    };
}

#[derive(PartialEq)]
enum SigHashVersion {
    Sprout,
    Overwinter,
    Sapling,
}

impl SigHashVersion {
    fn from_tx(tx: &TransactionData<TxIn, TxOut>) -> Self {
        if tx.overwintered {
            match tx.version_group_id {
                OVERWINTER_VERSION_GROUP_ID => SigHashVersion::Overwinter,
                SAPLING_VERSION_GROUP_ID => SigHashVersion::Sapling,
                _ => unimplemented!(),
            }
        } else {
            SigHashVersion::Sprout
        }
    }
}

fn prevout_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(tx.vin.len() * 36);
    for t_in in &tx.vin {
        t_in.write_prevout(&mut data).unwrap();
    }
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_PREVOUTS_HASH_PERSONALIZATION)
        .hash(&data)
}

fn sequence_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(tx.vin.len() * 4);
    for t_in in &tx.vin {
        (&mut data)
            .write_u32::<LittleEndian>(t_in.sequence())
            .unwrap();
    }
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_SEQUENCE_HASH_PERSONALIZATION)
        .hash(&data)
}

fn outputs_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(tx.vout.len() * (4 + 1));
    for t_out in &tx.vout {
        t_out.write(&mut data).unwrap();
    }
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_OUTPUTS_HASH_PERSONALIZATION)
        .hash(&data)
}

fn joinsplits_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(
        tx.joinsplits.len()
            * if tx.version < SAPLING_TX_VERSION {
                1802 // JSDescription with PHGR13 proof
            } else {
                1698 // JSDescription with Groth16 proof
            },
    );
    for js in &tx.joinsplits {
        js.write(&mut data).unwrap();
    }
    data.extend_from_slice(&tx.joinsplit_pubkey.unwrap());
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_JOINSPLITS_HASH_PERSONALIZATION)
        .hash(&data)
}

fn shielded_spends_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(tx.shielded_spends.len() * 384);
    for s_spend in &tx.shielded_spends {
        data.extend_from_slice(&s_spend.cv.to_bytes());
        data.extend_from_slice(s_spend.anchor.to_repr().as_ref());
        data.extend_from_slice(&s_spend.nullifier);
        s_spend.rk.write(&mut data).unwrap();
        data.extend_from_slice(&s_spend.zkproof);
    }
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_SHIELDED_SPENDS_HASH_PERSONALIZATION)
        .hash(&data)
}

fn shielded_converts_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(tx.shielded_converts.len() * 256);
    for s_convert in &tx.shielded_converts {
        data.extend_from_slice(&s_convert.cv.to_bytes());
        data.extend_from_slice(s_convert.anchor.to_repr().as_ref());
        data.extend_from_slice(&s_convert.zkproof);
    }
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_SHIELDED_CONVERTS_HASH_PERSONALIZATION)
        .hash(&data)
}

fn shielded_outputs_hash(tx: &TransactionData<TxIn, TxOut>) -> Blake2bHash {
    let mut data = Vec::with_capacity(tx.shielded_outputs.len() * 948);
    for s_out in &tx.shielded_outputs {
        s_out.write(&mut data).unwrap();
    }
    Blake2bParams::new()
        .hash_length(32)
        .personal(MASP_SHIELDED_OUTPUTS_HASH_PERSONALIZATION)
        .hash(&data)
}

pub fn signature_hash_data(
    tx: &TransactionData<TxIn, TxOut>,
    consensus_branch_id: consensus::BranchId,
    hash_type: u32,
    transparent_input: Option<(usize, &Script, AssetType, u64)>,
) -> Vec<u8> {
    let sigversion = SigHashVersion::from_tx(tx);
    match sigversion {
        SigHashVersion::Overwinter | SigHashVersion::Sapling => {
            let mut personal = [0; 16];
            (&mut personal[..12]).copy_from_slice(MASP_SIGHASH_PERSONALIZATION_PREFIX);
            (&mut personal[12..])
                .write_u32::<LittleEndian>(consensus_branch_id.into())
                .unwrap();

            let mut h = Blake2bParams::new()
                .hash_length(32)
                .personal(&personal)
                .to_state();
            let mut tmp = [0; 8];

            update_u32!(h, tx.header(), tmp);
            update_u32!(h, tx.version_group_id, tmp);
            update_hash!(h, hash_type & SIGHASH_ANYONECANPAY == 0, prevout_hash(tx));
            update_hash!(
                h,
                hash_type & SIGHASH_ANYONECANPAY == 0
                    && (hash_type & SIGHASH_MASK) != SIGHASH_SINGLE
                    && (hash_type & SIGHASH_MASK) != SIGHASH_NONE,
                sequence_hash(tx)
            );
            if (hash_type & SIGHASH_MASK) != SIGHASH_SINGLE
                && (hash_type & SIGHASH_MASK) != SIGHASH_NONE
            {
                h.update(outputs_hash(tx).as_ref());
            } else if (hash_type & SIGHASH_MASK) == SIGHASH_SINGLE
                && transparent_input.is_some()
                && transparent_input.as_ref().unwrap().0 < tx.vout.len()
            {
                h.update(
                    &tx.vout[transparent_input.as_ref().unwrap().0].sighash().as_ref(),
                );
            } else {
                h.update(&[0; 32]);
            };
            update_hash!(h, !tx.joinsplits.is_empty(), joinsplits_hash(tx));
            if sigversion == SigHashVersion::Sapling {
                update_hash!(h, !tx.shielded_spends.is_empty(), shielded_spends_hash(tx));
                update_hash!(h, !tx.shielded_converts.is_empty(), shielded_converts_hash(tx));
                update_hash!(
                    h,
                    !tx.shielded_outputs.is_empty(),
                    shielded_outputs_hash(tx)
                );
            }
            update_u32!(h, tx.lock_time, tmp);
            update_u32!(h, tx.expiry_height, tmp);
            if sigversion == SigHashVersion::Sapling {
                let mut bytes = Vec::new();
                tx.value_balance.write(&mut bytes).expect("value balance to serialize");
                h.update(bytes.as_ref());
            }
            update_u32!(h, hash_type, tmp);

            if let Some((n, script_code, atype, value)) = transparent_input {
                let mut data = vec![];
                tx.vin[n].write_prevout(&mut data).unwrap();
                script_code.write(&mut data).unwrap();
                data.extend_from_slice(atype.get_identifier());
                data.extend_from_slice(value.to_le_bytes().as_ref());
                (&mut data)
                    .write_u32::<LittleEndian>(tx.vin[n].sequence())
                    .unwrap();
                h.update(&data);
            }

            h.finalize().as_ref().to_vec()
        }
        SigHashVersion::Sprout => unimplemented!(),
    }
}

pub fn signature_hash(
    tx: &Transaction<TxIn, TxOut>,
    consensus_branch_id: consensus::BranchId,
    hash_type: u32,
    transparent_input: Option<(usize, &Script, AssetType, u64)>,
) -> Vec<u8> {
    signature_hash_data(tx, consensus_branch_id, hash_type, transparent_input)
}
