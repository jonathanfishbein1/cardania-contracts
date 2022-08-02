#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/buyer.skey

set addresOne "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set addresTwo "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set addressTwoTxOut "$addresTwo+1500000+2 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"

set protocolParameters (eval cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json)

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "880956f0e674245faff610228307ab98061fe3d7acc91de4b7a7f1081dd2f815#1" \
  --tx-in "ddfd1cebf4bc847276505a42552891ad5260206642961c8f14c0749e321caee1#1" \
  --tx-out $addressTwoTxOut \
  --change-address $addresOne \
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
