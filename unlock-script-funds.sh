cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file ../burn/transactions/protocol.json

utxoskey=../octoberFestMetaData/minting.skey

paymentAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
paymentAddressUtxo="db6c226409b47af3e501a6b087023100270e19d367d1fbdb60ce2d2cdae6f57f#0"
scriptAddressUtxo="db6c226409b47af3e501a6b087023100270e19d367d1fbdb60ce2d2cdae6f57f#1"
paymentTxOut="$paymentAddress+10000000"
scriptAddress=`cardano-cli address build --payment-script-file ../burn/transactions/result.plutus --testnet-magic 1097911063`

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in ${scriptAddressUtxo} \
    --tx-in-script-file ../burn/transactions/result.plutus \
    --tx-in-datum-value 0 \
    --tx-in-redeemer-value 0 \
    --tx-in-collateral ${paymentAddressUtxo} \
    --tx-out "$paymentTxOut" \
    --change-address "$scriptAddress" \
    --protocol-params-file ~/Documents/burn/transactions/protocol.json \
    --out-file ../burn/transactions/test-alonzo.tx

cardano-cli transaction sign \
  --tx-body-file "../burn/transactions/test-alonzo.tx" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "../burn/transactions/test-alonzo.signed"

cardano-cli transaction submit --tx-file "../burn/transactions/test-alonzo.signed" --testnet-magic 1097911063

