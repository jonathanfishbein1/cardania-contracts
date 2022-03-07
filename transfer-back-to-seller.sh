utxoskey=../octoberFestMetaData/buyer.skey

addresOne="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
addresTwo="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addressTwoTxOut="$addresTwo+1500000+1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
addressOneUtxo="089f862eccc04dcfb14fde62dee6f5d77b57dbb7de935c56e6fc4c4282b7e554#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "30823788bc8856ff67687089d77cbe84abc9ae422693db36ad1ea4541f9f5893#1" \
  --tx-in "$addressOneUtxo" \
  --tx-out "$addressTwoTxOut" \
  --change-address "$addresOne" \
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
