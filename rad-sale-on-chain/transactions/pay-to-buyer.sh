#!/usr/bin/env fish

set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set addresOne "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set addresTwo "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set addressTwoTxOut "$addresOne+20000000"

set protocolParameters (eval cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json)

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "d40965386871234f5709b8037af0ac0afdaa75b4faa76a574c801c7af7d80ed6#0" \
  --tx-out $addressTwoTxOut \
  --change-address $addresTwo \
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
