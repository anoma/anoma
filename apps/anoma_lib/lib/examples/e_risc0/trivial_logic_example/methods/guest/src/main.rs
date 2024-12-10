#![allow(warnings)]

use risc0_zkvm::{
    guest::env,
    sha::{Impl, Sha256, Digest},
};
use aarm_core::{
    resource::Resource,
    nullifier::Npk,
    encryption::{sha256_double, sha256_many}
};
use k256::Scalar;
use k256::{
    elliptic_curve::group::GroupEncoding,
    ProjectivePoint,
    AffinePoint
};
use k256::elliptic_curve::point::AffineCoordinates;
use k256::elliptic_curve::point::DecompactPoint;


#[derive(Debug)]
struct LogicInstance {
    cipher_text: Vec<[u8; 32]>,
    mac: Digest,
    sender_pk_x: [u8; 32],
    nonce: Digest,
}

#[derive(Debug)]
struct Cipher {
    cipher_text: Vec<[u8; 32]>,
    cur_state: Digest,
}

// Updated Merkle tree verification functions
fn check_merkle(current_root: Digest, node: Digest, is_left: bool) -> Digest {
    if is_left {
        sha256_double(node.as_bytes().to_vec(), current_root.as_bytes().to_vec())
    } else {
        sha256_double(current_root.as_bytes().to_vec(), node.as_bytes().to_vec())
    }
}

fn check_merkle_path(cur: Digest, path: &[((Digest), bool)]) -> Digest {
    path.iter().fold(cur, |acc, (node, is_left)| {
        check_merkle(acc, *node, *is_left)
    })
}

fn sum_u8_arrays(a: [u8; 32], b: [u8; 32]) -> [u8; 32] {
    let mut result = [0u8; 32];
    let mut carry = 0u8;
    
    for i in 0..32 {
        let sum = a[i] as u16 + b[i] as u16 + carry as u16;
        result[i] = (sum % 256) as u8;
        carry = (sum / 256) as u8;
    }
    
    result
}

fn update_cipher_state(cur_msg: [u8; 32], secret_key_x: Vec<u8>, cipher: &Cipher) -> Cipher {
    let new_state = sum_u8_arrays(cipher.cur_state.as_bytes().try_into().unwrap(), cur_msg);

    // Create new cipher text by adding new state
    let mut new_text = cipher.cipher_text.clone();
    new_text.push(new_state);

    // Hash new state with secret key
    let new_state_hash = sha256_double(new_state.to_vec(), secret_key_x);

    Cipher {
        cipher_text: new_text,
        cur_state: new_state_hash
    }
}

fn generate_cipher(initial_state: Digest, secret_key_x: Vec<u8>, plaintext: &[[u8; 32]]) -> Cipher {
    let mut cipher = Cipher {
        cipher_text: Vec::new(),
        cur_state: initial_state
    };

    // Process each plaintext message
    for msg in plaintext {
        cipher = update_cipher_state(*msg, secret_key_x.clone(), &cipher);
    }

    // Reverse final cipher text
    cipher.cipher_text.reverse();
    cipher
}

fn encrypt(
    messages: &[[u8; 32]], 
    pk_x: Scalar,
    sk: Scalar,
    nonce: Digest
) -> LogicInstance {
    let pk_affine = AffinePoint::decompact(&pk_x.to_bytes()).unwrap();
    let pk = ProjectivePoint::from(pk_affine);
    let secret_key = pk * sk;
    
    // Extract secret key components
    let secret_key_x = secret_key.to_affine().x().to_vec();

    let sender_inputs = vec![
        secret_key_x.clone(),
        pk_x.to_bytes().to_vec(),
        // pk_y.to_bytes().to_vec(),
        [10u8; 32].to_vec(),
    ];
    let sha_state = sha256_many(sender_inputs);


    // Generate cipher
    let final_cipher = generate_cipher(sha_state, secret_key_x.clone(), messages);

    // Get MAC
    let mac = final_cipher.cur_state;

    // Generate sender's public key
    // Note: In real implementation, this would use proper ECC operations
    let sender_pk = ProjectivePoint::GENERATOR * sk;
    let sender_pk_x = sender_pk.to_affine().x().to_vec().try_into().unwrap();

    // Create encryption result
    if final_cipher.cipher_text.len() == 10 {
        LogicInstance {
            cipher_text: final_cipher.cipher_text,
            mac,
            sender_pk_x,
            nonce,
        }
    } else {
        // Return zero-filled result for invalid cipher text length
        LogicInstance {
            cipher_text: vec![[0u8; 32]; 10],
            mac: Digest::ZERO,
            sender_pk_x: [0u8; 32],
            nonce: Digest::ZERO,
        }
    }
}



fn main() {

    // Read the input resource
    let (resource, 
        resource_nf_key, 
        merkle_path): (Resource, [u8; 32], Vec<([u8; 32], bool)>) = env::read();

    // Generate NPK
    let generated_npk = sha256_double(resource_nf_key.to_vec(), [0u8; 32].to_vec());
    
    // Determine if this is an output resource
    let is_output_resource = !merkle_path.is_empty() && merkle_path[0].1;
    
    // Calculate actual NPK
    let actual_npk = if is_output_resource {
        resource.npk
    } else {
        Npk::from_bytes(generated_npk.as_bytes().try_into().unwrap())
    };

    // Write the result to the journal
    // env::commit(&result);
}

