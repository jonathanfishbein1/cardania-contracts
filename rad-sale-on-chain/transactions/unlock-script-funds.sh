#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/buyer.skey

set buyerAddress "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set buyerTxOut "$buyerAddress+2000000 + 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
set buyerAddressCollateralUtxo "d93d20a6364efba27ec3c8f0cff5de738600db5f91f7ce3df54f7d0775c987db#0"

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set sellerTxOut "$sellerAddress+10000000"

set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
echo $scriptAddress
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in "f842817c25488f30434b07c7b54055b37d4dce1b8162dd27a98d9d766ef55aed#1" \
    --tx-in "7818593b8ddefdf8de766d0d6b4b2247facee29d01f645078dc8ee50dc46c19a#1" \
    --tx-in "7dfa44bba17617c4204d357b36f289d83a53d8c27c0f5c2e783b97f73d126451#3" \
    --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
    --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --tx-in-redeemer-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/buy-redeemer.json \
    --required-signer-hash d7b9b8604cdc89fe3421ec87e4f9e7013f109fa2efe7bed1c8b828bc \
    --tx-in-collateral $buyerAddressCollateralUtxo \
    --tx-out $buyerTxOut \
    --tx-out $sellerTxOut \
    --tx-out "$scriptAddress+2000000" \
    --tx-out-datum-hash-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --change-address $buyerAddress \
    --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
    --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body

cardano-cli transaction sign \
  --tx-body-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body \
  --testnet-magic 1097911063 \
  --signing-key-file $utxoskey \
  --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed


