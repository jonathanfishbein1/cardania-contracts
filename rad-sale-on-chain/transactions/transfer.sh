utxoskey=../octoberFestMetaData/minting.skey

addresOne="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addresTwo="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
addressTwoTxOut="$addresTwo+10000000"
addressOneUtxo="33a0009e8b2dcfb3d4573ad62ff019e6f3bcdd0d81d47d4b2fad5f9073be2d38#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "50039f7dce00fe3b52f1e042f37c68843b72e9a781a4a31fe9d0a0f315a1b7b7#0" \
  --tx-in "fa6b076c8cab58621af44e985aff2e59c35509b278f83b4e8d5cb78dc2a2c5f2#0" \
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
