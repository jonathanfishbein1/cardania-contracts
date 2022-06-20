#!/usr/bin/env fish

set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set sellerAddress "addr_test1qzd5rr90apw9z988q3274n8a4ztjgey5gjmwcjktfjzzw95twg0mg7ykwud2263mlllla45yyrdmfa04rxyepz8ghgxqd2yked"
set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
set sellerTxOut "$sellerAddress+10000000"
set sellerCollateralUtxo "5ef48a84c0af78ca9f665d6d812cf85f97949e7390c9ba1a824204a998d43739#2"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "5ef48a84c0af78ca9f665d6d812cf85f97949e7390c9ba1a824204a998d43739#3" \
  --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
  --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
  --tx-in-redeemer-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/redeemer.json \
  --required-signer-hash 9fcb76b9fcde6596ce9cd3456a4521f9497aa9e969ffcf0523613aeb \
  --tx-in-collateral $sellerCollateralUtxo \
  --tx-out $sellerTxOut \
  --change-address $sellerAddress \
  --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
  --out-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body"

# cardano-cli transaction sign \
#   --tx-body-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body" \
#   --testnet-magic 1097911063 \
#   --signing-key-file "$utxoskey" \
#   --out-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed"

# cardano-cli transaction submit \
#   --testnet-magic 1097911063 \
#   --tx-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed"
