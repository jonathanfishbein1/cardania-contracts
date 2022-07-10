#!/usr/bin/env fish

set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
set sellerTxOut "$sellerAddress+3000000+2 fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50.434c415353494342414259424c55453031"
set sellerCollateralUtxo "e1ac6670891d9a449c3fa119cf79642893b92cd7e398eb8073a94d05a3bc0b93#0"

set protocolParameters (eval cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json)

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "f162dc722df74ba3f85e1e1d9ed3d1be14b5b8c8d61588fe9ae60f149f137ac0#0" \
  --tx-in "f162dc722df74ba3f85e1e1d9ed3d1be14b5b8c8d61588fe9ae60f149f137ac0#2" \
  --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
  --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
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
