//! Tooling for creating a pseudo-random cache
//! necessary for verifying Ethereum headers.

#[cfg(feature = "ethereum-headers")]
#[allow(missing_docs)]
pub mod pipelined_eth_cache {
    use std::thread::JoinHandle;

    use borsh::{BorshDeserialize, BorshSerialize};
    use ethereum_types::{H256, H64, U256};
    use tokio::sync::oneshot::{channel, Receiver};

    use super::super::{
        cross_boundary, get_cache_size, get_full_size, get_seedhash,
        hashimoto_light, make_cache,
    };
    use crate::types::ethereum_headers::EthereumHeader;

    const EPOCH_LENGTH: usize = 30_000;

    /// A task that is run in a separate thread.
    struct BackgroundTask<T: Send> {
        /// A handle to the thread running the task
        /// Once joined, this field will be None
        handle: Option<JoinHandle<()>>,
        /// Receiving end of a channel that notifies when
        /// the task is complete
        notification: Receiver<T>,
    }

    impl<T: 'static + Send> BackgroundTask<T> {
        /// Start a new background thread that runs the provided task.
        /// If the task finishes, the thread sends a notification.
        fn new<F: 'static + FnOnce() -> T + Send>(task: F) -> Self {
            let (notification_send, notification_recv) = channel();
            Self {
                handle: Some(std::thread::spawn(|| {
                    let result = task();
                    let _ = notification_send.send(result);
                })),
                notification: notification_recv,
            }
        }

        /// Check if the thread sent a notification indicating that it
        /// is finished. If so, join the thread and return the result.
        /// Else return None.
        fn check_finished(&mut self) -> Option<T> {
            self.notification.try_recv().ok().map(|val| {
                if let Some(handle) = self.handle.take() {
                    let _ = handle.join();
                }
                val
            })
        }
    }

    /// A pseudorandom cache used to verify Ethereum headers
    #[derive(Default, BorshSerialize, BorshDeserialize)]
    pub struct Cache {
        /// size of the cache
        pub full_size: usize,
        /// the actual cache
        pub cache: Vec<u8>,
    }

    /// A pipeline of Ethereum caches used to verify headers.
    /// This cache changes once every 30000 blocks (one epoch)
    /// and is fairly expensive to compute (1 - 2 minutes).
    ///
    /// We thus pipeline the caches and pre-compute upcoming
    /// caches in the background
    #[derive(BorshSerialize, BorshDeserialize)]
    pub struct EthVerifier {
        /// The current epoch
        epoch: usize,
        /// Cache for the previous epoch
        pub previous_cache: Option<Cache>,
        /// How long to keep the cache of the previous epoch
        pub keep_previous: u64,
        /// Cache for the current epoch
        pub cache: Cache,
        /// How many blocks ahead to compute the new cache
        pub update_ahead: u64,
        /// Cache for the next epoch
        pub next_cache: Option<Cache>,
        #[borsh_skip]
        /// Background task to update the cache asynchronously
        update: Option<BackgroundTask<Cache>>,
    }

    impl EthVerifier {
        /// Create a new pipeline of ethash caches starting at the epoch
        /// determined by the block number. Will not create any cache
        /// previous to said epoch.
        pub fn new(number: u64, keep_previous: u64, update_ahead: u64) -> Self {
            let epoch = (number / EPOCH_LENGTH as u64) as usize;
            let cache_size = get_cache_size(epoch);
            let seed = get_seedhash(epoch);

            let mut cache: Vec<u8> = vec![0; cache_size];
            make_cache(&mut cache, seed);

            Self {
                epoch,
                previous_cache: None,
                keep_previous,
                cache: Cache {
                    full_size: get_full_size(epoch),
                    cache,
                },
                update_ahead,
                next_cache: None,
                update: None,
            }
        }

        /// We pipeline the caches across epochs. EthVerifier always keeps
        /// track of the latest epoch witnessed.
        ///
        /// It also:
        ///  1. Pre-computes the cache for the next epoch `self.update_ahead`
        ///     blocks ahead. It does this asynchronously in the background
        ///  2. Keeps the cache for the previous epoch for `self.keep_previous`
        ///     blocks afterwards. Then it is dropped.
        pub fn update(&mut self, number: u64) {
            // this runs in the background asynchronously
            self.begin_precompute(number);
            // check if background task finished and store result
            self.check_precompute_finished();
            // check if the epoch changed and advance pipeline if so
            self.update_epoch(number);
            // check if we no longer need to keep the previous cache
            self.drop_previous_cache(number);
        }

        /// If the block number is `self.update_ahead` blocks from the next
        /// epoch, begin pre-computing the next cache in the background
        fn begin_precompute(&mut self, number: u64) {
            if self.update.is_none()
                && self.next_cache.is_none()
                && !self.is_valid_for(number + self.update_ahead)
            {
                let epoch = ((number + self.update_ahead) / EPOCH_LENGTH as u64)
                    as usize;
                self.update = Some(BackgroundTask::new(move || {
                    let cache_size = get_cache_size(epoch);
                    let mut cache: Vec<u8> = vec![0; cache_size];
                    let seed = get_seedhash(epoch);
                    make_cache(&mut cache, seed);
                    Cache {
                        full_size: get_full_size(epoch),
                        cache,
                    }
                }))
            }
        }

        /// Check if the background task precomputing the cache for the
        /// next epoch is finished. If so, store the result
        fn check_precompute_finished(&mut self) {
            if let Some(Some(new_cache)) =
                self.update.as_mut().map(|task| task.check_finished())
            {
                self.next_cache = Some(new_cache);
                self.update = None;
            }
        }

        /// If the next block is in the next epoch, update the pipeline
        fn update_epoch(&mut self, number: u64) {
            if !self.is_valid_for(number + 1) {
                // move current cache to previous cache
                if self.previous_cache.is_none() {
                    self.previous_cache = Some(Cache::default());
                }
                std::mem::swap(
                    self.previous_cache.as_mut().unwrap(),
                    &mut self.cache,
                );
                self.cache = self.next_cache.take().unwrap_or_default();
            }
            // update the epoch
            self.epoch = std::cmp::max(
                (number / EPOCH_LENGTH as u64) as usize,
                self.epoch,
            );
        }

        /// If the current block number is at least `self.keep_previous` blocks
        /// past the last epoch change, drop the previous cache. It is
        /// no longer needed.
        fn drop_previous_cache(&mut self, number: u64) {
            if number > self.keep_previous
                && self.is_valid_for(number - self.keep_previous)
            {
                self.previous_cache = None;
            }
        }

        /// Check if a block number is valid for the current epoch
        pub fn is_valid_for(&self, number: u64) -> bool {
            (number / EPOCH_LENGTH as u64) as usize == self.epoch
        }

        /// Essentially a cache miss. We get a request to verify a header for a
        /// cache that needs to be computed from scratch. This is very slow.
        fn compute_slow(
            &self,
            hash: H256,
            nonce: H64,
            number: u64,
        ) -> (H256, H256) {
            let epoch = (number / EPOCH_LENGTH as u64) as usize;
            let cache_size = get_cache_size(epoch);
            let seed = get_seedhash(epoch);

            let mut cache: Vec<u8> = vec![0; cache_size];
            make_cache(&mut cache, seed);
            hashimoto_light(hash, nonce, get_full_size(epoch), &cache)
        }

        /// Verifies an Ethereum block header
        pub fn verify_header(
            &self,
            EthereumHeader {
                hash,
                number,
                difficulty,
                nonce,
                mix_hash,
                ..
            }: EthereumHeader,
        ) -> bool {
            let epoch = (number / EPOCH_LENGTH as u64) as usize;
            let (mix, final_hash) = if epoch + 1 == self.epoch {
                match self.previous_cache.as_ref() {
                    Some(Cache { full_size, cache }) => hashimoto_light(
                        hash.into(),
                        nonce.into(),
                        *full_size,
                        cache,
                    ),
                    None => {
                        self.compute_slow(hash.into(), nonce.into(), number)
                    }
                }
            } else if epoch == self.epoch {
                hashimoto_light(
                    hash.into(),
                    nonce.into(),
                    self.cache.full_size,
                    &self.cache.cache,
                )
            } else {
                self.compute_slow(hash.into(), nonce.into(), number)
            };
            mix == H256::from(mix_hash)
                && U256::from(final_hash.0) <= cross_boundary(difficulty.into())
        }
    }

    #[cfg(test)]
    mod tests {
        use borsh::BorshDeserialize;
        use ethereum_types::{H256, H64, U256};
        use hex_literal::*;

        use super::EthVerifier;
        use crate::types::ethereum_headers::{Difficulty, EthereumHeader};
        use crate::types::hash::Hash;

        const DAG_PATH: &str = "/home/r2d2/Projects/anoma/shared/src/ledger/\
                                ethash/test_data/eth_pseudo_cache";

        /// Test that verifying an Ethereum header works, even if it is
        /// for an epoch whose cache is not in memory.
        #[test]
        fn hashimoto_no_precomputed_cache() {
            let light_dag = EthVerifier {
                epoch: 0,
                previous_cache: None,
                keep_previous: 0,
                cache: Default::default(),
                update_ahead: 0,
                next_cache: None,
                update: None,
            };
            let header = EthereumHeader {
                // bare_hash of block#8996777 on ethereum mainnet
                hash: H256::from(hex!(
                    "3c2e6623b1de8862a927eeeef2b6b25dea6e1d9dad88dca3c239be3959dc384a"
                )).into(),
                parent_hash: Hash([0; 32]),
                number: 8996777,
                difficulty: Difficulty::from(U256::from(2580289863567664_u128)),
                nonce: H64::from(hex!("a5d3d0ccc8bb8a29")).into(),
                mix_hash: H256::from(hex!(
                    "543bc0769f7d5df30e7633f4a01552c2cee7baace8a6da37fddaa19e49e81209"
                )).into(),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };
            // this should work even though the cache is not precomputed
            assert!(light_dag.verify_header(header));
        }

        /// Test that verifying an Ethereum header in the same
        /// epoch as the EthVerifier is fast.
        #[test]
        fn test_ethereum_in_current_epoch() {
            // EthVerifier::new(8996777, 0, 0);
            let mut eth_verifier: EthVerifier =
                BorshDeserialize::try_from_slice(
                    std::fs::read(DAG_PATH).expect("Test failed").as_slice(),
                )
                .expect("Test failed");
            let header = EthereumHeader {
                // bare_hash of block#8996777 on ethereum mainnet
                hash: H256::from(hex!(
                    "3c2e6623b1de8862a927eeeef2b6b25dea6e1d9dad88dca3c239be3959dc384a"
                )).into(),
                parent_hash: Hash([0; 32]),
                number: 8996777,
                difficulty: Difficulty::from(U256::from(2580289863567664_u128)),
                nonce: H64::from(hex!("a5d3d0ccc8bb8a29")).into(),
                mix_hash: H256::from(hex!(
                    "543bc0769f7d5df30e7633f4a01552c2cee7baace8a6da37fddaa19e49e81209"
                )).into(),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };
            // This should ensure the current cache is valid for this header.
            assert!(eth_verifier.is_valid_for(header.number));
            // Test that verification passes and is fast enough
            let start = std::time::Instant::now();
            assert!(eth_verifier.verify_header(header));
            let time_taken = std::time::Instant::now() - start;
            assert!(time_taken.as_secs() <= 5);

            // We now test that update is a no-op here.
            let old_epoch = eth_verifier.epoch;
            eth_verifier.update(8996777);
            assert_eq!(eth_verifier.epoch, old_epoch);
            assert!(eth_verifier.update.is_none());
            assert!(eth_verifier.previous_cache.is_none());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_none());
        }

        /// Test that update advances to the next epoch if it sees a header in
        /// the next epoch. The current epoch should move to the
        /// previous epoch and can still be used to verify headers.
        #[test]
        fn test_ethereum_in_previous_epoch() {
            // EthVerifier::new(8996777, 0, 0);
            let mut eth_verifier: EthVerifier =
                BorshDeserialize::try_from_slice(
                    std::fs::read(DAG_PATH).expect("Test failed").as_slice(),
                )
                .expect("Test failed");
            eth_verifier.keep_previous = 4000;

            let header = EthereumHeader {
                // bare_hash of block#8996777 on ethereum mainnet
                hash: H256::from(hex!(
                    "3c2e6623b1de8862a927eeeef2b6b25dea6e1d9dad88dca3c239be3959dc384a"
                )).into(),
                parent_hash: Hash([0; 32]),
                number: 8996777,
                difficulty: Difficulty::from(U256::from(2580289863567664_u128)),
                nonce: H64::from(hex!("a5d3d0ccc8bb8a29")).into(),
                mix_hash: H256::from(hex!(
                    "543bc0769f7d5df30e7633f4a01552c2cee7baace8a6da37fddaa19e49e81209"
                )).into(),
                state_root: Hash([0; 32]),
                transactions_root: Hash([0; 32]),
            };
            // check that the epoch changed and pipeline advanced correctly
            // pre-update
            assert!(eth_verifier.previous_cache.is_none());
            assert!(!eth_verifier.cache.cache.is_empty());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_none());
            let old_epoch = eth_verifier.epoch;

            eth_verifier.update(9000000);
            // post-update
            assert_eq!(old_epoch + 1, eth_verifier.epoch);
            assert!(eth_verifier.previous_cache.is_some());
            assert!(eth_verifier.cache.cache.is_empty());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_some());

            // Test that verification passes and is fast
            let start = std::time::Instant::now();
            assert!(eth_verifier.verify_header(header));
            let time_taken = std::time::Instant::now() - start;
            assert!(time_taken.as_secs() <= 5);

            // this does not change the epoch. So it should be a no-op
            eth_verifier.update(9000001);
            // post-update
            assert_eq!(old_epoch + 1, eth_verifier.epoch);
            assert!(eth_verifier.previous_cache.is_some());
            assert!(eth_verifier.cache.cache.is_empty());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_some());
        }

        /// Test that old caches are dropped once they are `keep_previous` or
        /// more blocks behind the start of the current epoch
        #[test]
        fn test_drop_old_cache() {
            // EthVerifier::new(8996777, 0, 0);
            let mut eth_verifier: EthVerifier =
                BorshDeserialize::try_from_slice(
                    std::fs::read(DAG_PATH).expect("Test failed").as_slice(),
                )
                .expect("Test failed");
            eth_verifier.keep_previous = 1;
            // check that the epoch changed and pipeline advanced correctly
            // pre-update
            assert!(eth_verifier.previous_cache.is_none());
            assert!(!eth_verifier.cache.cache.is_empty());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_none());
            let old_epoch = eth_verifier.epoch;

            eth_verifier.update(9000000);
            // post-update
            assert_eq!(old_epoch + 1, eth_verifier.epoch);
            assert!(eth_verifier.previous_cache.is_some());
            assert!(eth_verifier.cache.cache.is_empty());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_some());

            eth_verifier.update(9000001);
            // post-update
            assert_eq!(old_epoch + 1, eth_verifier.epoch);
            assert!(eth_verifier.previous_cache.is_none());
            assert!(eth_verifier.cache.cache.is_empty());
            assert!(eth_verifier.next_cache.is_none());
            assert!(eth_verifier.update.is_some());
        }

        /// Test that at `update_ahead` blocks prior to a new epoch, a
        /// background process begins computing the next cache. This
        /// should eventually be stored in the `next_cache` field
        #[test]
        fn test_background_process() {
            // EthVerifier::new(8996777, 0, 0);
            let mut eth_verifier: EthVerifier =
                BorshDeserialize::try_from_slice(
                    std::fs::read(DAG_PATH).expect("Test failed").as_slice(),
                )
                .expect("Test failed");
            // set the pipeline length to something realistic
            eth_verifier.update_ahead = 200;
            // we start updating `update_ahead` blocks ahead.
            let mut block_number = 9000000 - 200;
            eth_verifier.update(block_number);
            // this makes it easier to check that the current cache gets updated
            eth_verifier.cache.cache = vec![];
            // Background process should have started
            assert!(eth_verifier.update.is_some());
            let start = std::time::Instant::now();
            // It should not take longer than 200 seconds for this to finish
            while (std::time::Instant::now() - start).as_secs() < 300 {
                std::thread::sleep(std::time::Duration::from_secs(2));
                block_number += 1;
                // we should be able to keep updating while this happens
                eth_verifier.update(block_number);
                // the process has finished.
                if eth_verifier.update.is_none() {
                    break;
                }
            }
            assert!(block_number < (9000000 - 1));
            // we should now have the result stored in `next_cache`
            assert!(eth_verifier.next_cache.is_some());
            eth_verifier.update(9000000 - 1);
            // check that the new cache is stored
            assert!(!eth_verifier.cache.cache.is_empty());
            // check that there is no longer a background process.
            assert!(eth_verifier.update.is_none());
        }
    }
}

#[cfg(feature = "ethereum-headers")]
pub use pipelined_eth_cache::*;
