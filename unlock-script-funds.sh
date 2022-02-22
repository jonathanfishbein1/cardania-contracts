utxoskey=../octoberFestMetaData/minting.skey

paymentAddress="addr1v8h0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65skjd5pp"
paymentAddressUtxo="00fe52686ae4992fbe6200af4340c8db6661fed304905fc160e123e3814e1830#0"
scriptAddressUtxo="00fe52686ae4992fbe6200af4340c8db6661fed304905fc160e123e3814e1830#2"
paymentTxOut="$paymentAddress+1800000"

cardano-cli transaction build \
    --alonzo-era \
    --mainnet \
    --tx-in ${scriptAddressUtxo} \
    --tx-in-script-file ../rad-sale-on-chain/transactions/result.plutus \
    --tx-in-datum-file ../rad-sale-on-chain/transactions/unit.json \
    --tx-in-redeemer-file ../rad-sale-on-chain/transactions/unit.json \
    --tx-in-collateral ${paymentAddressUtxo} \
    --tx-out "$paymentTxOut" \
    --change-address "$scriptAddress" \
    --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
    --out-file ../rad-sale-on-chain/transactions/redeem.body

cardano-cli transaction sign \
  --tx-body-file "../rad-sale-on-chain/transactions/redeem.body" \
  --mainnet \
  --signing-key-file "$utxoskey" \
  --out-file "../rad-sale-on-chain/transactions/redeem.tx"

cardano-cli transaction submit --tx-file "../rad-sale-on-chain/transactions/redeem.tx" --mainnet

