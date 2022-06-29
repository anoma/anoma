use borsh::{BorshDeserialize, BorshSerialize};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use serde::{Deserialize, Serialize};
use std::io::{self, Read, Write};
use std::hash::Hash;
use masp_primitives::asset_type::AssetType;
use blake2b_simd::{Hash as Blake2bHash, Params as Blake2bParams};
use super::sighash::MASP_OUTPUTS_HASH_PERSONALIZATION;

use super::legacy::Script;
use masp_primitives::transaction::components::{TxIn as Ti, TxOut as To};

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Serialize, Deserialize, Hash)]
pub struct OutPoint {
    hash: [u8; 32],
    n: u32,
}

impl OutPoint {
    pub fn new(hash: [u8; 32], n: u32) -> Self {
        OutPoint { hash, n }
    }

    pub fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let mut hash = [0u8; 32];
        reader.read_exact(&mut hash)?;
        let n = reader.read_u32::<LittleEndian>()?;
        Ok(OutPoint { hash, n })
    }

    pub fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        writer.write_all(&self.hash)?;
        writer.write_u32::<LittleEndian>(self.n)
    }

    pub fn n(&self) -> u32 {
        self.n
    }

    pub fn hash(&self) -> &[u8; 32] {
        &self.hash
    }
}

impl BorshDeserialize for OutPoint {
    fn deserialize(buf: &mut &[u8]) -> borsh::maybestd::io::Result<Self> {
        Self::read(buf)
    }
}

impl BorshSerialize for OutPoint {
    fn serialize<W: Write>(&self, writer: &mut W) -> borsh::maybestd::io::Result<()> {
        self.write(writer)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Hash, PartialEq, Eq, PartialOrd)]
pub struct TxIn {
    pub prevout: OutPoint,
    pub script_sig: Script,
    pub sequence: u32,
}

impl TxIn {
    pub fn new(prevout: OutPoint) -> Self {
        TxIn {
            prevout,
            script_sig: Script::default(),
            sequence: std::u32::MAX,
        }
    }
}

impl Ti for TxIn {
    fn read(mut reader: &mut impl Read) -> io::Result<Self> {
        let prevout = OutPoint::read(&mut reader)?;
        let script_sig = Script::read(&mut reader)?;
        let sequence = reader.read_u32::<LittleEndian>()?;

        Ok(TxIn {
            prevout,
            script_sig,
            sequence,
        })
    }

    fn write(&self, mut writer: &mut impl Write) -> io::Result<()> {
        self.prevout.write(&mut writer)?;
        self.script_sig.write(&mut writer)?;
        writer.write_u32::<LittleEndian>(self.sequence)
    }

    fn write_prevout(&self, writer: &mut impl Write) -> io::Result<()> {
        self.prevout.write(writer)
    }

    fn sequence(&self) -> u32 {
        self.sequence
    }
}

impl BorshDeserialize for TxIn {
    fn deserialize(buf: &mut &[u8]) -> borsh::maybestd::io::Result<Self> {
        Self::read(buf)
    }
}

impl BorshSerialize for TxIn {
    fn serialize<W: Write>(&self, writer: &mut W) -> borsh::maybestd::io::Result<()> {
        self.write(writer)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub struct TxOut {
    pub asset_type: AssetType,
    pub value: u64,
    pub script_pubkey: Script,
}

impl To for TxOut {
    fn read(mut reader: &mut impl Read) -> io::Result<Self> {
        let asset_type = {
            let mut tmp = [0u8; 32];
            reader.read_exact(&mut tmp)?;
            AssetType::from_identifier(&tmp)
        }
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "value out of range"))?;
        let value = {
            let mut tmp = [0u8; 8];
            reader.read_exact(&mut tmp)?;
            u64::from_le_bytes(tmp)
        };
        let script_pubkey = Script::read(&mut reader)?;

        Ok(TxOut {
            asset_type,
            value,
            script_pubkey,
        })
    }

    fn write(&self, mut writer: &mut impl Write) -> io::Result<()> {
        writer.write_all(self.asset_type.get_identifier())?;
        writer.write_all(&self.value.to_le_bytes())?;
        self.script_pubkey.write(&mut writer)
    }

    fn sighash(&self) -> Blake2bHash {
        let mut data = vec![];
        self.write(&mut data).unwrap();
        Blake2bParams::new()
            .hash_length(32)
            .personal(MASP_OUTPUTS_HASH_PERSONALIZATION)
            .hash(&data)
    }
    
}

impl BorshDeserialize for TxOut {
    fn deserialize(buf: &mut &[u8]) -> borsh::maybestd::io::Result<Self> {
        Self::read(buf)
    }
}

impl BorshSerialize for TxOut {
    fn serialize<W: Write>(&self, writer: &mut W) -> borsh::maybestd::io::Result<()> {
        self.write(writer)
    }
}