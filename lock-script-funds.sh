utxoskey=../octoberFestMetaData/minting.skey

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file ../rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063`
scriptTxOut="$scriptAddress+3000000+1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
sellerAddressUtxo="137407bad42d4279864fe83d5efe9820cabc791946d79da0002057918af24d03#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "9a99e03d271ee7744a4687048f6cccd4259c48449962bdf3438e98ca64efd30f#0" \
  --tx-in "137407bad42d4279864fe83d5efe9820cabc791946d79da0002057918af24d03#1" \
  --tx-in "$sellerAddressUtxo" \
  --tx-out "$scriptTxOut" \
  --tx-out-datum-hash-file ../rad-sale-on-chain/transactions/datum.json \
  --change-address "$sellerAddress" \
  --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
  --out-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.body"

cardano-cli transaction sign \
  --tx-body-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.tx"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.tx"
