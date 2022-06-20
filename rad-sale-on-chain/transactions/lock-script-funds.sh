#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
set scriptTxOut "$scriptAddress+3000000+1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
set sellerAddressUtxo "27a3db2bc0b56082b64bd8e0bf5d27b929203964440152cbef62baefc25cb9d7#0"

echo $scriptAddress
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "d8f14ad20abddcbcd9d67ef024b425fd072ace37e27367e7a66d8260c4ec0744#0" \
  --tx-in $sellerAddressUtxo \
  --tx-out $scriptTxOut \
  --tx-out-datum-hash-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
  --change-address $sellerAddress \
  --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
  --out-file "tx.body"

cardano-cli transaction sign \
  --tx-body-file "tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "tx.signed"
