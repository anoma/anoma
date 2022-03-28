//! This was taken from https://github.com/rust-ethereum/ethash
//! Apache-2 licensed Ethash implementation.

mod dag;
mod miller_rabin;

/// The reference algorithm used is from https://github.com/ethereum/wiki/wiki/Ethash
#[cfg(feature = "ethereum-headers")]
#[allow(missing_docs)]
pub mod ethash_algorithm {
    extern crate alloc;

    use core::ops::BitXor;

    use byteorder::{ByteOrder, LittleEndian};
    use ethereum_types::{H256, H512, H64, U256};
    use sha3::{Digest, Keccak256, Keccak512};

    pub use super::dag::LightDAG;
    use super::miller_rabin::is_prime;

    /// 2 to the power of 30
    pub const DATASET_BYTES_INIT: usize = 1073741824;
    /// 2 to the power of 23.
    pub const DATASET_BYTES_GROWTH: usize = 8388608;
    /// 2 to the power of 24.
    pub const CACHE_BYTES_INIT: usize = 16777216;
    /// 2 to the power of 17.
    pub const CACHE_BYTES_GROWTH: usize = 131072;
    /// 2 to power of 10
    pub const CACHE_MULTIPLIER: usize = 1024;
    /// Size of the mix hash
    pub const MIX_BYTES: usize = 128;
    /// Number of bytes in a standard word
    pub const WORD_BYTES: usize = 4;
    /// Length of the hash
    pub const HASH_BYTES: usize = 64;
    /// 2 to the power of 8
    pub const DATASET_PARENTS: usize = 256;
    /// 3 to the power of 1
    pub const CACHE_ROUNDS: usize = 3;
    /// 2 to the power of 6
    pub const ACCESSES: usize = 64;

    /// Get the cache size required given the block number.
    pub fn get_cache_size(epoch: usize) -> usize {
        let mut sz = CACHE_BYTES_INIT + CACHE_BYTES_GROWTH * epoch;
        sz -= HASH_BYTES;
        while !is_prime(sz / HASH_BYTES) {
            sz -= 2 * HASH_BYTES;
        }
        sz
    }

    /// Get the full dataset size given the block number.
    pub fn get_full_size(epoch: usize) -> usize {
        let mut sz = DATASET_BYTES_INIT + DATASET_BYTES_GROWTH * epoch;
        sz -= MIX_BYTES;
        while !is_prime(sz / MIX_BYTES) {
            sz -= 2 * MIX_BYTES
        }
        sz
    }

    fn fill_sha512(input: &[u8], a: &mut [u8], from_index: usize) {
        let mut hasher = Keccak512::default();
        hasher.update(input);
        let out = hasher.finalize();
        for i in 0..out.len() {
            a[from_index + i] = out[i];
        }
    }

    fn fill_sha256(input: &[u8], a: &mut [u8], from_index: usize) {
        let mut hasher = Keccak256::default();
        hasher.update(input);
        let out = hasher.finalize();
        for i in 0..out.len() {
            a[from_index + i] = out[i];
        }
    }

    /// Make an Ethash cache using the given seed.
    pub fn make_cache(cache: &mut [u8], seed: H256) {
        assert!(cache.len() % HASH_BYTES == 0);
        let n = cache.len() / HASH_BYTES;

        fill_sha512(&seed[..], cache, 0);

        for i in 1..n {
            let (last, next) = cache.split_at_mut(i * 64);
            fill_sha512(&last[(last.len() - 64)..], next, 0);
        }

        for _ in 0..CACHE_ROUNDS {
            for i in 0..n {
                let v =
                    (LittleEndian::read_u32(&cache[(i * 64)..]) as usize) % n;

                let mut r = [0u8; 64];
                for j in 0..64 {
                    let a = cache[((n + i - 1) % n) * 64 + j];
                    let b = cache[v * 64 + j];
                    r[j] = a.bitxor(b);
                }
                fill_sha512(&r, cache, i * 64);
            }
        }
    }

    /// Ethereum specific constant for the FNV hash algorithm
    pub const FNV_PRIME: u32 = 0x01000193;

    /// The Ethereum specific FNV hash algorithm
    fn fnv(v1: u32, v2: u32) -> u32 {
        let v1 = v1 as u64;
        let v2 = v2 as u64;

        (((v1 * 0x01000000) + (v1 * 0x193)) ^ v2) as u32
    }

    fn fnv64(a: [u8; 64], b: [u8; 64]) -> [u8; 64] {
        let mut r = [0u8; 64];
        for i in 0..(64 / 4) {
            let j = i * 4;

            LittleEndian::write_u32(
                &mut r[j..],
                fnv(
                    LittleEndian::read_u32(&a[j..]),
                    LittleEndian::read_u32(&b[j..]),
                ),
            );
        }
        r
    }

    fn fnv128(a: [u8; 128], b: [u8; 128]) -> [u8; 128] {
        let mut r = [0u8; 128];
        for i in 0..(128 / 4) {
            let j = i * 4;

            LittleEndian::write_u32(
                &mut r[j..],
                fnv(
                    LittleEndian::read_u32(&a[j..]),
                    LittleEndian::read_u32(&b[j..]),
                ),
            );
        }
        r
    }

    /// Calculate the dataset item.
    pub fn calc_dataset_item(cache: &[u8], i: usize) -> H512 {
        debug_assert!(cache.len() % 64 == 0);

        let n = cache.len() / 64;
        let r = HASH_BYTES / WORD_BYTES;
        let mut mix = [0u8; 64];
        for j in 0..64 {
            mix[j] = cache[(i % n) * 64 + j];
        }
        let mix_first32 = LittleEndian::read_u32(mix.as_ref()).bitxor(i as u32);
        LittleEndian::write_u32(mix.as_mut(), mix_first32);
        {
            let mut remix = [0u8; 64];
            remix[..64].copy_from_slice(&mix[..64]);
            fill_sha512(&remix, &mut mix, 0);
        }
        for j in 0..DATASET_PARENTS {
            let cache_index = fnv(
                (i.bitxor(j) & (u32::max_value() as usize)) as u32,
                LittleEndian::read_u32(&mix[(j % r * 4)..]),
            ) as usize;
            let mut item = [0u8; 64];
            let cache_index = cache_index % n;
            for i in 0..64 {
                item[i] = cache[cache_index * 64 + i];
            }
            mix = fnv64(mix, item);
        }
        let mut z = [0u8; 64];
        fill_sha512(&mix, &mut z, 0);
        H512::from(z)
    }

    /// "Main" function of Ethash, calculating the mix digest and result given
    /// the header and nonce.
    pub fn hashimoto<F: Fn(usize) -> H512>(
        header_hash: H256,
        nonce: H64,
        full_size: usize,
        lookup: F,
    ) -> (H256, H256) {
        hashimoto_with_hasher(
            header_hash,
            nonce,
            full_size,
            lookup,
            |data| {
                let mut hasher = Keccak256::default();
                hasher.update(&data);
                let mut res = [0u8; 32];
                res.copy_from_slice(hasher.finalize().as_slice());
                res
            },
            |data| {
                let mut hasher = Keccak512::default();
                hasher.update(&data);
                let mut res = [0u8; 64];
                res.copy_from_slice(hasher.finalize().as_slice());
                res
            },
        )
    }

    /// The hashimoto hash algorithm with a generic hash
    /// function allowed as input
    pub fn hashimoto_with_hasher<
        F: Fn(usize) -> H512,
        HF256: Fn(&[u8]) -> [u8; 32],
        HF512: Fn(&[u8]) -> [u8; 64],
    >(
        header_hash: H256,
        nonce: H64,
        full_size: usize,
        lookup: F,
        hasher256: HF256,
        hasher512: HF512,
    ) -> (H256, H256) {
        let n = full_size / HASH_BYTES;
        let w = MIX_BYTES / WORD_BYTES;
        const MIXHASHES: usize = MIX_BYTES / HASH_BYTES;
        let s = {
            let mut data = [0u8; 40];
            data[..32].copy_from_slice(&header_hash.0);
            data[32..].copy_from_slice(&nonce.0);
            data[32..].reverse();
            hasher512(&data)
        };
        let mut mix = [0u8; MIX_BYTES];
        for i in 0..MIXHASHES {
            for j in 0..64 {
                mix[i * HASH_BYTES + j] = s[j];
            }
        }

        for i in 0..ACCESSES {
            let p = (fnv(
                (i as u32).bitxor(LittleEndian::read_u32(s.as_ref())),
                LittleEndian::read_u32(&mix[(i % w * 4)..]),
            ) as usize)
                % (n / MIXHASHES)
                * MIXHASHES;
            let mut newdata = [0u8; MIX_BYTES];
            for j in 0..MIXHASHES {
                let v = lookup(p + j);
                for k in 0..64 {
                    newdata[j * 64 + k] = v[k];
                }
            }
            mix = fnv128(mix, newdata);
        }
        let mut cmix = [0u8; MIX_BYTES / 4];
        for i in 0..(MIX_BYTES / 4 / 4) {
            let j = i * 4;
            let a = fnv(
                LittleEndian::read_u32(&mix[(j * 4)..]),
                LittleEndian::read_u32(&mix[((j + 1) * 4)..]),
            );
            let b = fnv(a, LittleEndian::read_u32(&mix[((j + 2) * 4)..]));
            let c = fnv(b, LittleEndian::read_u32(&mix[((j + 3) * 4)..]));

            LittleEndian::write_u32(&mut cmix[j..], c);
        }
        let result = {
            let mut data = [0u8; 64 + MIX_BYTES / 4];
            data[..64].copy_from_slice(&s);
            data[64..].copy_from_slice(&cmix);
            hasher256(&data)
        };
        (H256::from(cmix), H256::from(result))
    }

    /// Ethash used by a light client. Only stores the 16MB cache rather than
    /// the full dataset.
    pub fn hashimoto_light(
        header_hash: H256,
        nonce: H64,
        full_size: usize,
        cache: &[u8],
    ) -> (H256, H256) {
        hashimoto(header_hash, nonce, full_size, |i| {
            calc_dataset_item(cache, i)
        })
    }

    /// Convert across boundary. `f(x) = 2 ^ 256 / x`.
    pub fn cross_boundary(val: U256) -> U256 {
        if val <= U256::one() {
            U256::max_value()
        } else {
            ((U256::one() << 255) / val) << 1
        }
    }

    /// Get the seedhash for a given block number.
    pub fn get_seedhash(epoch: usize) -> H256 {
        let mut s = [0u8; 32];
        for _ in 0..epoch {
            fill_sha256(&s.clone(), &mut s, 0);
        }
        H256::from_slice(s.as_ref())
    }
}

#[cfg(feature = "ethereum-headers")]
pub use ethash_algorithm::*;
