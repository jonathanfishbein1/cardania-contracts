#!/usr/bin/env fish

set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
set sellerTxOut "$sellerAddress+1000000"
set sellerCollateralUtxo "9eb0d28f2909d863feedb4ad210c28c28809ff685b21952cc183f6c31d36df5b#0"

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "520d76e815bc796c5c5d8cb6c1de23177573eb0043f16b58fddf54a4d0776f40#0" \
  --tx-in "9e0938663296e4da86978fb2926885952af4a1bc5da120062e9ccc249fd4d4c8#3" \
  --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
  --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
  --tx-in-redeemer-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/close-redeemer.json \
  --required-signer-hash eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52 \
  --tx-in-collateral $sellerCollateralUtxo \
  --tx-out $sellerTxOut \
  --change-address $sellerAddress \
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
