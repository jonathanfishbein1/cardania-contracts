utxoskey=../octoberFestMetaData/minting.skey

addresOne="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
addresTwo="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addressTwoTxOut="$addresOne+6000000"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "b0697288ceb9fa438aa0f27c0cd89a1d8154af8ada2c74e26f9830689711eb35#0" \
  --tx-in "b4385043771795b03ce6aff105fc5570bae25f83df2610cce7ae00231538ad07#0" \
  --tx-in "b50c33fc75cd039c1f172f8fb9b95b6fd6421aa259ea8c66d050a75105e1e4e7#0" \
  --tx-in "bef6f735d87c30951315c912135b913f956c71d54c1e398d33fa22027aa176a3#0" \
  --tx-in "cfc86741e803bd5796e11f7b33e798b5830b63d82aa7a883b3e6203de78c4997#0" \
  --tx-in "d6fc54f8aea2d094b7922b42f41f0a69b7adc680fecf14dafa09fa7711f5c31f#0" \
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
