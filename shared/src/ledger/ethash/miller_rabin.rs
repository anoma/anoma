// Derived from https://github.com/huonw/primal/blob/master/primal-check/src/is_prime.rs

#[cfg(feature = "ethereum-headers")]
pub mod miller_rabin_algos {
    #[derive(Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Debug)]
    struct U128 {
        hi: u64,
        lo: u64,
    }

    fn modulo(mut a: U128, m: u64) -> u64 {
        if a.hi >= m {
            a.hi -= (a.hi / m) * m;
        }
        let mut x = a.hi;
        let mut y = a.lo;
        for _ in 0..64 {
            let t = (x as i64 >> 63) as u64;
            x = (x << 1) | (y >> 63);
            y <<= 1;
            if (x | t) >= m {
                x = x.wrapping_sub(m);
                y += 1;
            }
        }
        x
    }

    fn mul128(u: u64, v: u64) -> U128 {
        let u1 = u >> 32;
        let u0 = u & (!0 >> 32);
        let v1 = v >> 32;
        let v0 = v & (!0 >> 32);

        let t = u0 * v0;
        let w0 = t & (!0 >> 32);
        let k = t >> 32;

        let t = u1 * v0 + k;
        let w1 = t & (!0 >> 32);
        let w2 = t >> 32;

        let t = u0 * v1 + w1;
        let k = t >> 32;
        U128 {
            lo: (t << 32) + w0,
            hi: u1 * v1 + w2 + k,
        }
    }

    fn mod_mul_(a: u64, b: u64, m: u64) -> u64 {
        modulo(mul128(a, b), m)
    }

    fn mod_mul(a: u64, b: u64, m: u64) -> u64 {
        match a.checked_mul(b) {
            Some(r) => {
                if r >= m {
                    r % m
                } else {
                    r
                }
            }
            None => mod_mul_(a, b, m),
        }
    }

    fn mod_sqr(a: u64, m: u64) -> u64 {
        if a < (1 << 32) {
            let r = a * a;
            if r >= m { r % m } else { r }
        } else {
            mod_mul_(a, a, m)
        }
    }

    fn mod_exp(mut x: u64, mut d: u64, n: u64) -> u64 {
        let mut ret: u64 = 1;
        while d != 0 {
            if d % 2 == 1 {
                ret = mod_mul(ret, x, n)
            }
            d /= 2;
            x = mod_sqr(x, n);
        }
        ret
    }

    pub fn is_prime(n: usize) -> bool {
        const HINT: &[u64] = &[2];

        // we have a strict upper bound, so we can just use the witness
        // table of Pomerance, Selfridge & Wagstaff and Jeaschke to be as
        // efficient as possible, without having to fall back to
        // randomness.
        const WITNESSES: &[(u64, &[u64])] = &[
            (2_046, HINT),
            (1_373_652, &[2, 3]),
            (9_080_190, &[31, 73]),
            (25_326_000, &[2, 3, 5]),
            (4_759_123_140, &[2, 7, 61]),
            (1_112_004_669_632, &[2, 13, 23, 1662803]),
            (2_152_302_898_746, &[2, 3, 5, 7, 11]),
            (3_474_749_660_382, &[2, 3, 5, 7, 11, 13]),
            (341_550_071_728_320, &[2, 3, 5, 7, 11, 13, 17]),
            (0xFFFF_FFFF_FFFF_FFFF, &[2, 3, 5, 7, 11, 13, 17, 19, 23]),
        ];

        if n % 2 == 0 {
            return n == 2;
        }
        if n == 1 {
            return false;
        }

        let mut d = n - 1;
        let mut s = 0;
        while d % 2 == 0 {
            d /= 2;
            s += 1
        }

        let witnesses = WITNESSES
            .iter()
            .find(|&&(hi, _)| hi >= n as u64)
            .map(|&(_, wtnss)| wtnss)
            .unwrap();
        'next_witness: for &a in witnesses.iter() {
            let mut power = mod_exp(a, d as u64, n as u64);
            assert!(power < n as u64);
            if power == 1 || power == n as u64 - 1 {
                continue 'next_witness;
            }

            for _r in 0..s {
                power = mod_sqr(power, n as u64);
                assert!(power < n as u64);
                if power == 1 {
                    return false;
                }
                if power == n as u64 - 1 {
                    continue 'next_witness;
                }
            }
            return false;
        }

        true
    }
}

#[cfg(feature = "ethereum-headers")]
pub use miller_rabin_algos::*;
