use std::mem::ManuallyDrop;

use anoma::types::internal::HostEnvResult;
use anoma::vm::types::KeyVal;
use borsh::BorshDeserialize;

/// This function is a helper to handle the second step of reading var-len
/// values from the host.
///
/// In cases where we're reading a value from the host in the guest and
/// we don't know the byte size up-front, we have to read it in 2-steps. The
/// first step reads the value into a result buffer and returns the size (if
/// any) back to the guest, the second step reads the value from cache into a
/// pre-allocated buffer with the obtained size.
fn read_from_buffer(
    read_result: i64,
    result_buffer: unsafe extern "C" fn(u64),
) -> Option<Vec<u8>> {
    if HostEnvResult::is_fail(read_result) {
        None
    } else {
        let result: Vec<u8> = Vec::with_capacity(read_result as _);
        // The `result` will be dropped from the `target`, which is
        // reconstructed from the same memory
        let result = ManuallyDrop::new(result);
        let offset = result.as_slice().as_ptr() as u64;
        unsafe { result_buffer(offset) };
        let target = unsafe {
            Vec::from_raw_parts(offset as _, read_result as _, read_result as _)
        };
        Some(target)
    }
}

/// This function is a helper to handle the second step of reading var-len
/// values in a key-value pair from the host.
fn read_key_val_from_buffer<T: BorshDeserialize>(
    read_result: i64,
    result_buffer: unsafe extern "C" fn(u64),
) -> Option<(String, T)> {
    let key_val = read_from_buffer(read_result, result_buffer)
        .and_then(|t| KeyVal::try_from_slice(&t[..]).ok());
    key_val.and_then(|key_val| {
        // decode the value
        T::try_from_slice(&key_val.val)
            .map(|val| (key_val.key, val))
            .ok()
    })
}

/// Transaction environment imports
pub mod tx {
    use core::slice;
    use std::convert::TryFrom;
    use std::marker::PhantomData;

    use anoma::types::address;
    use anoma::types::address::Address;
    use anoma::types::chain::CHAIN_ID_LENGTH;
    use anoma::types::ibc::IbcEvent;
    use anoma::types::internal::HostEnvResult;
    use anoma::types::storage::{
        BlockHash, BlockHeight, Epoch, BLOCK_HASH_LENGTH,
    };
    use anoma::types::time::Rfc3339String;
    pub use borsh::{BorshDeserialize, BorshSerialize};

    #[derive(Debug)]
    pub struct KeyValIterator<T>(pub u64, pub PhantomData<T>);

    /// Try to read a Borsh encoded variable-length value at the given key from
    /// storage.
    pub fn read<T: BorshDeserialize>(key: impl AsRef<str>) -> Option<T> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_tx_read(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_tx_result_buffer)
            .and_then(|t| T::try_from_slice(&t[..]).ok())
    }

    /// Try to read a variable-length value as bytes at the given key from
    /// storage.
    pub fn read_bytes(key: impl AsRef<str>) -> Option<Vec<u8>> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_tx_read(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_tx_result_buffer)
    }

    /// Check if the given key is present in storage.
    pub fn has_key(key: impl AsRef<str>) -> bool {
        let key = key.as_ref();
        let found =
            unsafe { anoma_tx_has_key(key.as_ptr() as _, key.len() as _) };
        HostEnvResult::is_success(found)
    }

    /// Write a value to be encoded with Borsh at the given key to storage.
    pub fn write<T: BorshSerialize>(key: impl AsRef<str>, val: T) {
        let buf = val.try_to_vec().unwrap();
        write_bytes(key, buf);
    }

    /// Write a value as bytes at the given key to storage.
    pub fn write_bytes(key: impl AsRef<str>, val: impl AsRef<[u8]>) {
        let key = key.as_ref();
        unsafe {
            anoma_tx_write(
                key.as_ptr() as _,
                key.len() as _,
                val.as_ref().as_ptr() as _,
                val.as_ref().len() as _,
            )
        };
    }

    /// Write a temporary value to be encoded with Borsh at the given key to
    /// storage.
    pub fn write_temp<T: BorshSerialize>(key: impl AsRef<str>, val: T) {
        let buf = val.try_to_vec().unwrap();
        write_bytes_temp(key, buf);
    }

    /// Write a temporary value as bytes at the given key to storage.
    pub fn write_bytes_temp(key: impl AsRef<str>, val: impl AsRef<[u8]>) {
        let key = key.as_ref();
        unsafe {
            anoma_tx_write_temp(
                key.as_ptr() as _,
                key.len() as _,
                val.as_ref().as_ptr() as _,
                val.as_ref().len() as _,
            )
        };
    }

    /// Delete a value at the given key from storage.
    pub fn delete(key: impl AsRef<str>) {
        let key = key.as_ref();
        unsafe { anoma_tx_delete(key.as_ptr() as _, key.len() as _) };
    }

    /// Get an iterator with the given prefix.
    ///
    /// Important note: The prefix iterator will ignore keys that are not yet
    /// committed to storage from the block in which this transaction is being
    /// applied. It will only find keys that are already committed to
    /// storage (i.e. from predecessor blocks). However, it will provide the
    /// most up-to-date value for such keys.
    pub fn iter_prefix<T: BorshDeserialize>(
        prefix: impl AsRef<str>,
    ) -> KeyValIterator<T> {
        let prefix = prefix.as_ref();
        let iter_id = unsafe {
            anoma_tx_iter_prefix(prefix.as_ptr() as _, prefix.len() as _)
        };
        KeyValIterator(iter_id, PhantomData)
    }

    impl<T: BorshDeserialize> Iterator for KeyValIterator<T> {
        type Item = (String, T);

        fn next(&mut self) -> Option<(String, T)> {
            let read_result = unsafe { anoma_tx_iter_next(self.0) };
            super::read_key_val_from_buffer(read_result, anoma_tx_result_buffer)
        }
    }

    /// Insert a verifier address. This address must exist on chain, otherwise
    /// the transaction will be rejected.
    ///
    /// Validity predicates of each verifier addresses inserted in the
    /// transaction will validate the transaction and will receive all the
    /// changed storage keys and initialized accounts in their inputs.
    pub fn insert_verifier(addr: &Address) {
        let addr = addr.encode();
        unsafe { anoma_tx_insert_verifier(addr.as_ptr() as _, addr.len() as _) }
    }

    /// Update a validity predicate
    pub fn update_validity_predicate(addr: &Address, code: impl AsRef<[u8]>) {
        let addr = addr.encode();
        let code = code.as_ref();
        unsafe {
            anoma_tx_update_validity_predicate(
                addr.as_ptr() as _,
                addr.len() as _,
                code.as_ptr() as _,
                code.len() as _,
            )
        };
    }

    // Initialize a new account
    pub fn init_account(code: impl AsRef<[u8]>) -> Address {
        let code = code.as_ref();
        let result = Vec::with_capacity(address::ESTABLISHED_ADDRESS_BYTES_LEN);
        unsafe {
            anoma_tx_init_account(
                code.as_ptr() as _,
                code.len() as _,
                result.as_ptr() as _,
            )
        };
        let slice = unsafe {
            slice::from_raw_parts(
                result.as_ptr(),
                address::ESTABLISHED_ADDRESS_BYTES_LEN,
            )
        };
        Address::try_from_slice(slice)
            .expect("Decoding address created by the ledger shouldn't fail")
    }

    /// Emit an IBC event. There can be only one event per transaction. On
    /// multiple calls, only the last emitted event will be used.
    pub fn emit_ibc_event(event: &IbcEvent) {
        let event = BorshSerialize::try_to_vec(event).unwrap();
        unsafe {
            anoma_tx_emit_ibc_event(event.as_ptr() as _, event.len() as _)
        };
    }

    /// Get the chain ID
    pub fn get_chain_id() -> String {
        let result = Vec::with_capacity(CHAIN_ID_LENGTH);
        unsafe {
            anoma_tx_get_chain_id(result.as_ptr() as _);
        }
        let slice =
            unsafe { slice::from_raw_parts(result.as_ptr(), CHAIN_ID_LENGTH) };
        String::from_utf8(slice.to_vec()).expect("Cannot convert the ID string")
    }

    /// Get height of the current block
    pub fn get_block_height() -> BlockHeight {
        BlockHeight(unsafe { anoma_tx_get_block_height() })
    }

    /// Get time of the current block header as rfc 3339 string
    pub fn get_block_time() -> Rfc3339String {
        let read_result = unsafe { anoma_tx_get_block_time() };
        let time_value =
            super::read_from_buffer(read_result, anoma_tx_result_buffer)
                .expect("The block time should exist");
        Rfc3339String(
            String::try_from_slice(&time_value[..])
                .expect("The conversion shouldn't fail"),
        )
    }

    /// Get hash of the current block
    pub fn get_block_hash() -> BlockHash {
        let result = Vec::with_capacity(BLOCK_HASH_LENGTH);
        unsafe {
            anoma_tx_get_block_hash(result.as_ptr() as _);
        }
        let slice = unsafe {
            slice::from_raw_parts(result.as_ptr(), BLOCK_HASH_LENGTH)
        };
        BlockHash::try_from(slice).expect("Cannot convert the hash")
    }

    /// Get epoch of the current block
    pub fn get_block_epoch() -> Epoch {
        Epoch(unsafe { anoma_tx_get_block_epoch() })
    }

    /// Log a string. The message will be printed at the `tracing::Level::Info`.
    pub fn log_string<T: AsRef<str>>(msg: T) {
        let msg = msg.as_ref();
        unsafe {
            anoma_tx_log_string(msg.as_ptr() as _, msg.len() as _);
        }
    }

    /// These host functions are implemented in the Anoma's [`host_env`]
    /// module. The environment provides calls to them via this C interface.
    extern "C" {
        // Read variable-length data when we don't know the size up-front,
        // returns the size of the value (can be 0), or -1 if the key is
        // not present. If a value is found, it will be placed in the read
        // cache, because we cannot allocate a buffer for it before we know
        // its size.
        fn anoma_tx_read(key_ptr: u64, key_len: u64) -> i64;

        // Read a value from result buffer.
        fn anoma_tx_result_buffer(result_ptr: u64);

        // Returns 1 if the key is present, -1 otherwise.
        fn anoma_tx_has_key(key_ptr: u64, key_len: u64) -> i64;

        // Write key/value
        fn anoma_tx_write(
            key_ptr: u64,
            key_len: u64,
            val_ptr: u64,
            val_len: u64,
        );

        // Write a temporary key/value
        fn anoma_tx_write_temp(
            key_ptr: u64,
            key_len: u64,
            val_ptr: u64,
            val_len: u64,
        );

        // Delete the given key and its value
        fn anoma_tx_delete(key_ptr: u64, key_len: u64);

        // Get an ID of a data iterator with key prefix
        fn anoma_tx_iter_prefix(prefix_ptr: u64, prefix_len: u64) -> u64;

        // Returns the size of the value (can be 0), or -1 if there's no next
        // value. If a value is found, it will be placed in the read
        // cache, because we cannot allocate a buffer for it before we know
        // its size.
        fn anoma_tx_iter_next(iter_id: u64) -> i64;

        // Insert a verifier
        fn anoma_tx_insert_verifier(addr_ptr: u64, addr_len: u64);

        // Update a validity predicate
        fn anoma_tx_update_validity_predicate(
            addr_ptr: u64,
            addr_len: u64,
            code_ptr: u64,
            code_len: u64,
        );

        // Initialize a new account
        fn anoma_tx_init_account(code_ptr: u64, code_len: u64, result_ptr: u64);

        // Emit an IBC event
        fn anoma_tx_emit_ibc_event(event_ptr: u64, event_len: u64);

        // Get the chain ID
        fn anoma_tx_get_chain_id(result_ptr: u64);

        // Get the current block height
        fn anoma_tx_get_block_height() -> u64;

        // Get the time of the current block header
        fn anoma_tx_get_block_time() -> i64;

        // Get the current block hash
        fn anoma_tx_get_block_hash(result_ptr: u64);

        // Get the current block epoch
        fn anoma_tx_get_block_epoch() -> u64;

        // Requires a node running with "Info" log level
        fn anoma_tx_log_string(str_ptr: u64, str_len: u64);
    }
}

/// Validity predicate environment imports
pub mod vp {
    use core::slice;
    use std::convert::TryFrom;
    use std::marker::PhantomData;

    use anoma::types::chain::CHAIN_ID_LENGTH;
    use anoma::types::hash::{Hash, HASH_LENGTH};
    use anoma::types::internal::HostEnvResult;
    use anoma::types::key::*;
    use anoma::types::storage::{
        BlockHash, BlockHeight, Epoch, BLOCK_HASH_LENGTH,
    };
    pub use borsh::{BorshDeserialize, BorshSerialize};

    pub struct PreKeyValIterator<T>(pub u64, pub PhantomData<T>);

    pub struct PostKeyValIterator<T>(pub u64, pub PhantomData<T>);

    /// Try to read a Borsh encoded variable-length value at the given key from
    /// storage before transaction execution.
    pub fn read_pre<T: BorshDeserialize>(key: impl AsRef<str>) -> Option<T> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_vp_read_pre(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_vp_result_buffer)
            .and_then(|t| T::try_from_slice(&t[..]).ok())
    }

    /// Try to read a variable-length value as bytesat the given key from
    /// storage before transaction execution.
    pub fn read_bytes_pre(key: impl AsRef<str>) -> Option<Vec<u8>> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_vp_read_pre(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_vp_result_buffer)
    }

    /// Try to read a Borsh encoded variable-length value at the given key from
    /// storage after transaction execution.
    pub fn read_post<T: BorshDeserialize>(key: impl AsRef<str>) -> Option<T> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_vp_read_post(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_vp_result_buffer)
            .and_then(|t| T::try_from_slice(&t[..]).ok())
    }

    /// Try to read a variable-length value as bytes at the given key from
    /// storage after transaction execution.
    pub fn read_bytes_post(key: impl AsRef<str>) -> Option<Vec<u8>> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_vp_read_post(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_vp_result_buffer)
    }

    /// Try to read a Borsh encoded variable-length value at the given key from
    /// storage before transaction execution.
    pub fn read_temp<T: BorshDeserialize>(key: impl AsRef<str>) -> Option<T> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_vp_read_temp(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_vp_result_buffer)
            .and_then(|t| T::try_from_slice(&t[..]).ok())
    }

    /// Try to read a variable-length value as bytes at the given key from
    /// storage before transaction execution.
    pub fn read_bytes_temp(key: impl AsRef<str>) -> Option<Vec<u8>> {
        let key = key.as_ref();
        let read_result =
            unsafe { anoma_vp_read_temp(key.as_ptr() as _, key.len() as _) };
        super::read_from_buffer(read_result, anoma_vp_result_buffer)
    }

    /// Check if the given key was present in storage before transaction
    /// execution.
    pub fn has_key_pre(key: impl AsRef<str>) -> bool {
        let key = key.as_ref();
        let found =
            unsafe { anoma_vp_has_key_pre(key.as_ptr() as _, key.len() as _) };
        HostEnvResult::is_success(found)
    }

    /// Check if the given key is present in storage after transaction
    /// execution.
    pub fn has_key_post(key: impl AsRef<str>) -> bool {
        let key = key.as_ref();
        let found =
            unsafe { anoma_vp_has_key_post(key.as_ptr() as _, key.len() as _) };
        HostEnvResult::is_success(found)
    }

    /// Get an iterator with the given prefix before transaction execution
    pub fn iter_prefix_pre<T: BorshDeserialize>(
        prefix: impl AsRef<str>,
    ) -> PreKeyValIterator<T> {
        let prefix = prefix.as_ref();
        let iter_id = unsafe {
            anoma_vp_iter_prefix(prefix.as_ptr() as _, prefix.len() as _)
        };
        PreKeyValIterator(iter_id, PhantomData)
    }

    impl<T: BorshDeserialize> Iterator for PreKeyValIterator<T> {
        type Item = (String, T);

        fn next(&mut self) -> Option<(String, T)> {
            let read_result = unsafe { anoma_vp_iter_pre_next(self.0) };
            super::read_key_val_from_buffer(read_result, anoma_vp_result_buffer)
        }
    }

    /// Get an iterator with the given prefix after transaction execution
    pub fn iter_prefix_post<T: BorshDeserialize>(
        prefix: impl AsRef<str>,
    ) -> PostKeyValIterator<T> {
        let prefix = prefix.as_ref();
        let iter_id = unsafe {
            anoma_vp_iter_prefix(prefix.as_ptr() as _, prefix.len() as _)
        };
        PostKeyValIterator(iter_id, PhantomData)
    }

    impl<T: BorshDeserialize> Iterator for PostKeyValIterator<T> {
        type Item = (String, T);

        fn next(&mut self) -> Option<(String, T)> {
            let read_result = unsafe { anoma_vp_iter_post_next(self.0) };
            super::read_key_val_from_buffer(read_result, anoma_vp_result_buffer)
        }
    }

    /// Get the chain ID
    pub fn get_chain_id() -> String {
        let result = Vec::with_capacity(CHAIN_ID_LENGTH);
        unsafe {
            anoma_vp_get_chain_id(result.as_ptr() as _);
        }
        let slice =
            unsafe { slice::from_raw_parts(result.as_ptr(), CHAIN_ID_LENGTH) };
        String::from_utf8(slice.to_vec()).expect("Cannot convert the ID string")
    }

    /// Get height of the current block
    pub fn get_block_height() -> BlockHeight {
        BlockHeight(unsafe { anoma_vp_get_block_height() })
    }

    /// Get a block hash
    pub fn get_block_hash() -> BlockHash {
        let result = Vec::with_capacity(BLOCK_HASH_LENGTH);
        unsafe {
            anoma_vp_get_block_hash(result.as_ptr() as _);
        }
        let slice = unsafe {
            slice::from_raw_parts(result.as_ptr(), BLOCK_HASH_LENGTH)
        };
        BlockHash::try_from(slice).expect("Cannot convert the hash")
    }

    /// Get a tx hash
    pub fn get_tx_code_hash() -> Hash {
        let result = Vec::with_capacity(HASH_LENGTH);
        unsafe {
            anoma_vp_get_tx_code_hash(result.as_ptr() as _);
        }
        let slice =
            unsafe { slice::from_raw_parts(result.as_ptr(), HASH_LENGTH) };
        Hash::try_from(slice).expect("Cannot convert the hash")
    }

    /// Get epoch of the current block
    pub fn get_block_epoch() -> Epoch {
        Epoch(unsafe { anoma_vp_get_block_epoch() })
    }

    /// Verify a transaction signature. The signature is expected to have been
    /// produced on the encoded transaction [`anoma::proto::Tx`]
    /// using [`anoma::proto::Tx::sign`].
    pub fn verify_tx_signature(
        pk: &common::PublicKey,
        sig: &common::Signature,
    ) -> bool {
        let pk = BorshSerialize::try_to_vec(pk).unwrap();
        let sig = BorshSerialize::try_to_vec(sig).unwrap();
        let valid = unsafe {
            anoma_vp_verify_tx_signature(
                pk.as_ptr() as _,
                pk.len() as _,
                sig.as_ptr() as _,
                sig.len() as _,
            )
        };
        HostEnvResult::is_success(valid)
    }

    /// Verify a MASP transaction.
    pub fn verify_masp(
        tx: Vec<u8>,
    ) -> bool {
        let valid = unsafe {
            anoma_vp_verify_masp(tx.as_ptr() as _, tx.len() as _)
        };
        HostEnvResult::is_success(valid)
    }

    /// Log a string. The message will be printed at the `tracing::Level::Info`.
    pub fn log_string<T: AsRef<str>>(msg: T) {
        let msg = msg.as_ref();
        unsafe {
            anoma_vp_log_string(msg.as_ptr() as _, msg.len() as _);
        }
    }

    /// Evaluate a validity predicate with given data. The address, changed
    /// storage keys and verifiers will have the same values as the input to
    /// caller's validity predicate.
    ///
    /// If the execution fails for whatever reason, this will return `false`.
    /// Otherwise returns the result of evaluation.
    pub fn eval(vp_code: Vec<u8>, input_data: Vec<u8>) -> bool {
        let result = unsafe {
            anoma_vp_eval(
                vp_code.as_ptr() as _,
                vp_code.len() as _,
                input_data.as_ptr() as _,
                input_data.len() as _,
            )
        };
        HostEnvResult::is_success(result)
    }

    /// These host functions are implemented in the Anoma's [`host_env`]
    /// module. The environment provides calls to them via this C interface.
    extern "C" {
        // Read variable-length prior state when we don't know the size
        // up-front, returns the size of the value (can be 0), or -1 if
        // the key is not present. If a value is found, it will be placed in the
        // result buffer, because we cannot allocate a buffer for it before
        // we know its size.
        fn anoma_vp_read_pre(key_ptr: u64, key_len: u64) -> i64;

        // Read variable-length posterior state when we don't know the size
        // up-front, returns the size of the value (can be 0), or -1 if
        // the key is not present. If a value is found, it will be placed in the
        // result buffer, because we cannot allocate a buffer for it before
        // we know its size.
        fn anoma_vp_read_post(key_ptr: u64, key_len: u64) -> i64;

        // Read variable-length temporary state when we don't know the size
        // up-front, returns the size of the value (can be 0), or -1 if
        // the key is not present. If a value is found, it will be placed in the
        // result buffer, because we cannot allocate a buffer for it before
        // we know its size.
        fn anoma_vp_read_temp(key_ptr: u64, key_len: u64) -> i64;

        // Read a value from result buffer.
        fn anoma_vp_result_buffer(result_ptr: u64);

        // Returns 1 if the key is present in prior state, -1 otherwise.
        fn anoma_vp_has_key_pre(key_ptr: u64, key_len: u64) -> i64;

        // Returns 1 if the key is present in posterior state, -1 otherwise.
        fn anoma_vp_has_key_post(key_ptr: u64, key_len: u64) -> i64;

        // Get an ID of a data iterator with key prefix
        fn anoma_vp_iter_prefix(prefix_ptr: u64, prefix_len: u64) -> u64;

        // Read variable-length prior state when we don't know the size
        // up-front, returns the size of the value (can be 0), or -1 if
        // the key is not present. If a value is found, it will be placed in the
        // result buffer, because we cannot allocate a buffer for it before
        // we know its size.
        fn anoma_vp_iter_pre_next(iter_id: u64) -> i64;

        // Read variable-length posterior state when we don't know the size
        // up-front, returns the size of the value (can be 0), or -1 if the
        // key is not present. If a value is found, it will be placed in the
        // result buffer, because we cannot allocate a buffer for it before
        // we know its size.
        fn anoma_vp_iter_post_next(iter_id: u64) -> i64;

        // Get the chain ID
        fn anoma_vp_get_chain_id(result_ptr: u64);

        // Get the current block height
        fn anoma_vp_get_block_height() -> u64;

        // Get the current block hash
        fn anoma_vp_get_block_hash(result_ptr: u64);

        // Get the current tx hash
        fn anoma_vp_get_tx_code_hash(result_ptr: u64);

        // Get the current block epoch
        fn anoma_vp_get_block_epoch() -> u64;

        // Verify a transaction signature
        fn anoma_vp_verify_tx_signature(
            pk_ptr: u64,
            pk_len: u64,
            sig_ptr: u64,
            sig_len: u64,
        ) -> i64;

        fn anoma_vp_verify_masp(tx_ptr: u64, tx_len: u64) -> i64;

        // Requires a node running with "Info" log level
        fn anoma_vp_log_string(str_ptr: u64, str_len: u64);

        fn anoma_vp_eval(
            vp_code_ptr: u64,
            vp_code_len: u64,
            input_data_ptr: u64,
            input_data_len: u64,
        ) -> i64;
    }
}
