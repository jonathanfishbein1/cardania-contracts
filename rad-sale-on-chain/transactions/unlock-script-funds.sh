#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/buyer.skey

set buyerAddress "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set buyerTxOut "$buyerAddress+2000000 + 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
set buyerAddressCollateralUtxo "c3e7f372ec821a654c2472313730a66890e2aa00835a14585a173a30dfc56cec#0"

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set sellerTxOut "$sellerAddress+10000000"

set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
echo $scriptAddress
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 1097911063 \
    --tx-in "520d76e815bc796c5c5d8cb6c1de23177573eb0043f16b58fddf54a4d0776f40#1" \
    --tx-in "669546d95a7e4f4934c6247ebfe0164bc7f8424dd95846002a85acad8287f13c#1" \
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


