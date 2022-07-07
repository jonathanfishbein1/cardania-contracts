#!/usr/bin/env fish

set utxoskey /home/jonathan/Documents/octoberFestMetaData/minting.skey

set addresOne "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set addresTwo "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set addressTwoTxOut "$addresOne+20000000"

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "5a944a4a93299b9d4d6b565b4fc2dcdaf6445cfc9a9944f238b8010f75b004fe#0" \
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
