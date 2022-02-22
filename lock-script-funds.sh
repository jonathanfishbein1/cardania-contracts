utxoskey=../octoberFestMetaData/minting.skey

paymentAddress="addr1v8h0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65skjd5pp"
scriptAddress=`cardano-cli address build --payment-script-file ../rad-sale-on-chain/transactions/result.plutus --mainnet`
scriptTxOut="$scriptAddress+1800000+1 f0b63bb98a30166333b0c92b54ff8b9ec9d40ef48e991ce86d6cd4ef.524144546f6b656e73"
scriptTxOutTwo="$scriptAddress+1800000+99 f0b63bb98a30166333b0c92b54ff8b9ec9d40ef48e991ce86d6cd4ef.524144546f6b656e73"
paymentAddressUtxo="4578889132e1554fcce96a94481a682e8157a50d94867d6e264e579c3ea930f7#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --mainnet \
  --tx-in "$paymentAddressUtxo" \
  --tx-out "$scriptTxOut" \
  --tx-out "$scriptTxOutTwo" \
  --tx-out-datum-hash-file ../rad-sale-on-chain/transactions/unit.json \
  --change-address "$paymentAddress" \
  --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
  --out-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.body"

cardano-cli transaction sign \
  --tx-body-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.body" \
  --mainnet \
  --signing-key-file "$utxoskey" \
  --out-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.tx"

cardano-cli transaction submit \
  --mainnet \
  --tx-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.tx"
