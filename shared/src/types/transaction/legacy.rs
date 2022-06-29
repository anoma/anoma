//! Support for legacy transparent addresses and scripts.

use borsh::{BorshDeserialize, BorshSerialize};
use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{Deserialize, Serialize};
use std::io::{self, Read, Write};
use std::ops::Shl;
use std::hash::Hash;

/// T is the type for Script OpCodes
// pub trait Script<T>: Default + BorshSerialize + BorshDeserialize + Hash + Shl<&[u8]> + Shl<T> {
//     fn read(reader: &mut impl Read) -> io::Result<Self>;
//     fn address(&self) -> Option<TransparentAddress>;
// }

use masp_primitives::serialize::Vector;

/// Minimal subset of script opcodes.
enum OpCode {
    // push value
    PushData1 = 0x4c,
    PushData2 = 0x4d,
    PushData4 = 0x4e,

    // stack ops
    Dup = 0x76,

    // bit logic
    Equal = 0x87,
    EqualVerify = 0x88,

    // crypto
    Hash160 = 0xa9,
    CheckSig = 0xac,
}

// A serialized script, used inside transparent inputs and outputs of a transaction.
#[derive(Clone, Debug, Default, Serialize, Deserialize, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub struct Script(pub Vec<u8>);

impl Script {
    pub fn read<R: Read>(mut reader: R) -> io::Result<Self> {
        let script = Vector::read(&mut reader, |r| r.read_u8())?;
        Ok(Script(script))
    }

    pub fn write<W: Write>(&self, mut writer: W) -> io::Result<()> {
        Vector::write(&mut writer, &self.0, |w, e| w.write_u8(*e))
    }

    /// Returns the address that this Script contains, if any.
    pub fn address(&self) -> Option<TransparentAddress> {
        if self.0.len() == 25
            && self.0[0] == OpCode::Dup as u8
            && self.0[1] == OpCode::Hash160 as u8
            && self.0[2] == 0x14
            && self.0[23] == OpCode::EqualVerify as u8
            && self.0[24] == OpCode::CheckSig as u8
        {
            let mut hash = [0; 20];
            hash.copy_from_slice(&self.0[3..23]);
            Some(TransparentAddress::PublicKey(hash))
        } else if self.0.len() == 23
            && self.0[0] == OpCode::Hash160 as u8
            && self.0[1] == 0x14
            && self.0[22] == OpCode::Equal as u8
        {
            let mut hash = [0; 20];
            hash.copy_from_slice(&self.0[2..22]);
            Some(TransparentAddress::Script(hash))
        } else {
            None
        }
    }
}

impl BorshDeserialize for Script {
    fn deserialize(buf: &mut &[u8]) -> borsh::maybestd::io::Result<Self> {
        Self::read(buf)
    }
}

impl BorshSerialize for Script {
    fn serialize<W: Write>(&self, writer: &mut W) -> borsh::maybestd::io::Result<()> {
        self.write(writer)
    }
}

impl Shl<OpCode> for Script {
    type Output = Self;

    fn shl(mut self, rhs: OpCode) -> Self {
        self.0.push(rhs as u8);
        self
    }
}

impl Shl<&[u8]> for Script {
    type Output = Self;

    fn shl(mut self, data: &[u8]) -> Self {
        if data.len() < OpCode::PushData1 as usize {
            self.0.push(data.len() as u8);
        } else if data.len() <= 0xff {
            self.0.push(OpCode::PushData1 as u8);
            self.0.push(data.len() as u8);
        } else if data.len() <= 0xffff {
            self.0.push(OpCode::PushData2 as u8);
            self.0.extend(&(data.len() as u16).to_le_bytes());
        } else {
            self.0.push(OpCode::PushData4 as u8);
            self.0.extend(&(data.len() as u32).to_le_bytes());
        }
        self.0.extend(data);
        self
    }
}

/// A transparent address corresponding to either a public key or a `Script`.
#[derive(Debug, PartialEq)]
pub enum TransparentAddress {
    PublicKey([u8; 20]),
    Script([u8; 20]),
}

impl TransparentAddress {
    /// Generate the `scriptPubKey` corresponding to this address.
    pub fn script(&self) -> Script {
        match self {
            TransparentAddress::PublicKey(key_id) => {
                // P2PKH script
                Script::default()
                    << OpCode::Dup
                    << OpCode::Hash160
                    << &key_id[..]
                    << OpCode::EqualVerify
                    << OpCode::CheckSig
            }
            TransparentAddress::Script(script_id) => {
                // P2SH script
                Script::default() << OpCode::Hash160 << &script_id[..] << OpCode::Equal
            }
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::{OpCode, Script, TransparentAddress};

//     #[test]
//     fn script_opcode() {
//         {
//             let script = Script::default() << OpCode::PushData1;
//             assert_eq!(&script.0, &[OpCode::PushData1 as u8]);
//         }
//     }

//     #[test]
//     fn script_pushdata() {
//         {
//             let script = Script::default() << &[1, 2, 3, 4][..];
//             assert_eq!(&script.0, &[4, 1, 2, 3, 4]);
//         }

//         {
//             let short_data = vec![2; 100];
//             let script = Script::default() << &short_data[..];
//             assert_eq!(script.0[0], OpCode::PushData1 as u8);
//             assert_eq!(script.0[1] as usize, 100);
//             assert_eq!(&script.0[2..], &short_data[..]);
//         }

//         {
//             let medium_data = vec![7; 1024];
//             let script = Script::default() << &medium_data[..];
//             assert_eq!(script.0[0], OpCode::PushData2 as u8);
//             assert_eq!(&script.0[1..3], &[0x00, 0x04][..]);
//             assert_eq!(&script.0[3..], &medium_data[..]);
//         }

//         {
//             let long_data = vec![42; 1_000_000];
//             let script = Script::default() << &long_data[..];
//             assert_eq!(script.0[0], OpCode::PushData4 as u8);
//             assert_eq!(&script.0[1..5], &[0x40, 0x42, 0x0f, 0x00][..]);
//             assert_eq!(&script.0[5..], &long_data[..]);
//         }
//     }

//     #[test]
//     fn p2pkh() {
//         let addr = TransparentAddress::PublicKey([4; 20]);
//         assert_eq!(
//             &addr.script().0,
//             &[
//                 0x76, 0xa9, 0x14, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
//                 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x88, 0xac,
//             ]
//         );
//         assert_eq!(addr.script().address(), Some(addr));
//     }

//     #[test]
//     fn p2sh() {
//         let addr = TransparentAddress::Script([7; 20]);
//         assert_eq!(
//             &addr.script().0,
//             &[
//                 0xa9, 0x14, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07,
//                 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x87,
//             ]
//         );
//         assert_eq!(addr.script().address(), Some(addr));
//     }
// }
