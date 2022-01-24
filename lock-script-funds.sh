cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file ../swap-on-chain/transactions/protocol.json

utxoskey=../octoberFestMetaData/minting.skey

paymentAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file ../swap-on-chain/transactions/result.plutus --testnet-magic 1097911063`
scriptTxOut="$scriptAddress+30000000"
scriptdatumhash=`cardano-cli transaction hash-script-data --script-data-value 0`

paymentAddressUtxo="db6c226409b47af3e501a6b087023100270e19d367d1fbdb60ce2d2cdae6f57f#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "$paymentAddressUtxo" \
  --tx-out "$scriptTxOut" \
  --tx-out-datum-hash ${scriptdatumhash} \
  --change-address "$paymentAddress" \
  --protocol-params-file ~/Documents/swap-on-chain/transactions/protocol.json \
  --out-file "../swap-on-chain/transactions/plutusSubmit.body"

cardano-cli transaction sign \
  --tx-body-file "../swap-on-chain/transactions/plutusSubmit.body" \
  --mainnet \
  --signing-key-file "$utxoskey" \
  --out-file "../swap-on-chain/transactions/plutusSubmit.tx"

cardano-cli transaction submit --tx-file "../swap-on-chain/transactions/plutusSubmit.tx" --testnet-magic 1097911063
