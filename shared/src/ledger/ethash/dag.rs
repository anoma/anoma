#[cfg(feature = "ethereum-headers")]
#[allow(missing_docs)]
pub mod dag_cache {
    use std::thread::JoinHandle;

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
    #[derive(Default)]
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
    pub struct LightDAG {
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
        /// Background task to update the cache asynchronously
        update: Option<BackgroundTask<Cache>>,
    }

    impl LightDAG {
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

        /// We pipeline the caches across epochs. LightDAG always keeps
        /// track of the latest epoch witnessed.
        ///
        /// It also:
        ///  1. Pre-computes the cache for the next epoch `self.update_ahead`
        ///     blocks ahead. It does this asynchronously in the background
        ///  2. Keeps the cache for the previous epoch for `self.keep_previous`
        ///     blocks afterwards. Then it is dropped.
        pub fn update(&mut self, number: u64) {
            // update the epoch
            self.epoch = std::cmp::max(
                (number / EPOCH_LENGTH as u64) as usize,
                self.epoch,
            );
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
                && !self.is_valid_for(number + self.update_ahead)
            {
                let epoch =
                    (number + self.update_ahead / EPOCH_LENGTH as u64) as usize;
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
                self.cache = self.next_cache.take().expect(
                    "Cache for the next epoch should have been pre-computed",
                );
            }
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
        use ethereum_types::{H256, H64, U256};
        use hex_literal::*;

        use super::{cross_boundary, LightDAG};

        #[test]
        fn hashimoto_should_work() {
            let light_dag = LightDAG {
                epoch: 0,
                previous_cache: None,
                keep_previous: 0,
                cache: Default::default(),
                update_ahead: 0,
                next_cache: None,
                update: None,
            };
            // bare_hash of block#8996777 on ethereum mainnet
            let partial_header_hash = H256::from(hex!(
            "3c2e6623b1de8862a927eeeef2b6b25dea6e1d9dad88dca3c239be3959dc384a"
        ));

            let (mixh, final_hash) = light_dag.compute_slow(
                partial_header_hash,
                H64::from(hex!("a5d3d0ccc8bb8a29")),
                8996777,
            );
            assert_eq!(
                mixh,
                H256::from(hex!(
                "543bc0769f7d5df30e7633f4a01552c2cee7baace8a6da37fddaa19e49e81209"
                ))
            );

            assert!(
                U256::from(final_hash.0)
                    <= cross_boundary(2580289863567664_u128.into())
            );
        }
    }
}

#[cfg(feature = "ethereum-headers")]
pub use dag_cache::*;
