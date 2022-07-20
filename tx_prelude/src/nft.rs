use anoma::types::address::Address;
use anoma::types::nft;
use anoma::types::nft::NftToken;
use anoma::types::transaction::nft::{CreateNft, MintNft};

use super::*;

/// Initialize a new NFT token address.
pub fn init_nft(ctx: &mut Ctx, nft: CreateNft) -> EnvResult<Address> {
    let address = ctx.init_account(&nft.vp_code)?;

    // write tag
    let tag_key = nft::get_tag_key(&address);
    ctx.write(&tag_key, &nft.tag)?;

    // write creator
    let creator_key = nft::get_creator_key(&address);
    ctx.write(&creator_key, &nft.creator)?;

    // write keys
    let keys_key = nft::get_keys_key(&address);
    ctx.write(&keys_key, &nft.keys)?;

    // write optional keys
    let optional_keys_key = nft::get_optional_keys_key(&address);
    ctx.write(&optional_keys_key, nft.opt_keys)?;

    // mint tokens
    aux_mint_token(ctx, &address, &nft.creator, nft.tokens, &nft.creator)?;

    ctx.insert_verifier(&nft.creator)?;

    Ok(address)
}

pub fn mint_tokens(ctx: &mut Ctx, nft: MintNft) -> TxResult {
    aux_mint_token(ctx, &nft.address, &nft.creator, nft.tokens, &nft.creator)
}

fn aux_mint_token(
    ctx: &mut Ctx,
    nft_address: &Address,
    creator_address: &Address,
    tokens: Vec<NftToken>,
    verifier: &Address,
) -> TxResult {
    for token in tokens {
        // write token metadata
        let metadata_key =
            nft::get_token_metadata_key(nft_address, &token.id.to_string());
        ctx.write(&metadata_key, &token.metadata)?;

        // write current owner token as creator
        let current_owner_key = nft::get_token_current_owner_key(
            nft_address,
            &token.id.to_string(),
        );
        ctx.write(
            &current_owner_key,
            &token
                .current_owner
                .unwrap_or_else(|| creator_address.clone()),
        )?;

        // write value key
        let value_key =
            nft::get_token_value_key(nft_address, &token.id.to_string());
        ctx.write(&value_key, &token.values)?;

        // write optional value keys
        let optional_value_key = nft::get_token_optional_value_key(
            nft_address,
            &token.id.to_string(),
        );
        ctx.write(&optional_value_key, &token.opt_values)?;

        // write approval addresses
        let approval_key =
            nft::get_token_approval_key(nft_address, &token.id.to_string());
        ctx.write(&approval_key, &token.approvals)?;

        // write burnt propriety
        let burnt_key =
            nft::get_token_burnt_key(nft_address, &token.id.to_string());
        ctx.write(&burnt_key, token.burnt)?;
    }
    ctx.insert_verifier(verifier)?;
    Ok(())
}
