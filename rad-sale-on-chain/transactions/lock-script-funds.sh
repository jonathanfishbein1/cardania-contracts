#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
set scriptTxOut "$scriptAddress+3000000+2 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
set sellerAddressCollateralUtxo "639e0d6038aad1a71a4fab3a544c9580c8e7875048a3e54589dd3f6fcc897096#0"
set sellerAddressTokenUtxo "fe40f26d6f4e72c845b3c63d359e7a8fab73fe9700e9d2c0a510de625ecc8912#1"

echo $scriptAddress
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in $sellerAddressTokenUtxo \
  --tx-in $sellerAddressCollateralUtxo \
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
