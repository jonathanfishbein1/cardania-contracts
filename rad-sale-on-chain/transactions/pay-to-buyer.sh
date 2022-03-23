utxoskey=../octoberFestMetaData/minting.skey

addresOne="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
addresTwo="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addressTwoTxOut="$addresOne+7000000"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "a3cac96cfb26496bbec4ac07158c2ddb141577a275560dedfdce037b918c3e3d#0" \
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
