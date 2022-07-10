#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
set scriptTxOut "$scriptAddress+3000000+2 fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50.434c415353494342414259424c55453031"
set sellerAddressCollateralUtxo "c78e8ba6f72d0239a51e279df41f59f41de9b3e7266cd646211dfe421747b567#0"
set sellerAddressTokenUtxo "5fb67e2c706bcee1031837b83e25612703f5c6c600e24dd993bc6e8a46647d96#1"

echo $scriptAddress
set protocolParameters (eval cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json)

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in $sellerAddressTokenUtxo \
  --tx-in $sellerAddressCollateralUtxo \
  --tx-in '5fb67e2c706bcee1031837b83e25612703f5c6c600e24dd993bc6e8a46647d96#0' \
  --tx-out $scriptTxOut \
  --tx-out-datum-hash-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
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
