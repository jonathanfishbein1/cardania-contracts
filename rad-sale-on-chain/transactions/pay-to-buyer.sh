#!/usr/bin/env fish

set utxoskey ../octoberFestMetaData/minting.skey

set addresOne "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set addresTwo "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set addressTwoTxOut "$addresOne+7000000"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "961012755cd73683ca51ec8ed796ab10134266b0817a75579d22b36c92ce355b#2" \
  --tx-out $addressTwoTxOut \
  --change-address $addresTwo \
  --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
  --out-file "tx.body"

cardano-cli transaction sign \
  --tx-body-file "tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file $utxoskey \
  --out-file "tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "tx.signed"
