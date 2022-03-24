utxovkey=/home/jonathan/Documents/octoberFestMetaData/minting.vkey
utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

transactionsPath=${transactionsPath}

utxoaddr=$(cardano-cli address build --testnet-magic 1097911063 --payment-verification-key-file $utxovkey)

cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file protocol.json

walletAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
wallettxOut="$walletAddress+2000000"
policyFile="${transactionsPath}result.plutus"
policyId=$(cardano-cli transaction policyid --script-file $policyFile)

for filepath in "${transactionPath}metadata/*"; do
  filenameWithExtension=$(basename $filepath)
  filename="${filenameWithExtension%.*}"
  mint="1 $policyId.$filename"
  txuot="${wallettxOut}+$mint" 

  cardano-cli query utxo --address "$utxoaddr" --testnet-magic 1097911063 --out-file "${transactionsPath}utxo.json"
  scriptownerUtxo1=$(jq -r 'keys[0]' "${transactionsPath}utxo.json")

  cardano-cli transaction build \
    --testnet-magic 1097911063 \
    --tx-in "$scriptownerUtxo1" \
    --tx-out "$txuot" \
    --change-address "$utxoaddr" \
    --mint="$mint" \
    --mint-script-file "${transactionsPath}result.plutus" \
    --metadata-json-file "$filepath" \
    --protocol-params-file protocol.json \
    --out-file "${transactionsPath}mint.body"

  cardano-cli transaction sign \
    --tx-body-file "${transactionsPath}mint.body" \
    --testnet-magic 1097911063 \
    --signing-key-file "$utxoskey" \
    --out-file "${transactionsPath}mint.tx"

  cardano-cli transaction submit --tx-file "${transactionsPath}mint.tx" --testnet-magic 1097911063
  sleep 480
done
