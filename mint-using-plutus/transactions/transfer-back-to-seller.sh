utxoskey=../octoberFestMetaData/buyer.skey

addresOne="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
addresTwo="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addressTwoTxOut="$addresTwo+1500000+1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
addressOneUtxo="0c200fd6ade8dcf81c2f9088583e2a8fcacd299b096f4bd8052309f87354964d#1"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "0c200fd6ade8dcf81c2f9088583e2a8fcacd299b096f4bd8052309f87354964d#0" \
  --tx-in "$addressOneUtxo" \
  --tx-out "$addressTwoTxOut" \
  --change-address "$addresOne" \
  --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
  --out-file "tx.body"

cardano-cli transaction sign \
  --tx-body-file "tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "tx.signed"
