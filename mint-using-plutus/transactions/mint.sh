utxovkey=minting.vkey
utxoskey=minting.skey

utxoaddr=$(cardano-cli address build --testnet-magic 1097911063 --payment-verification-key-file $utxovkey)

cardano-cli query utxo --address "$utxoaddr" --testnet-magic 1097911063

cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file protocol.json

walletAddress="addr1qxldw6tu3l644fftqpepp9rz6c76kwcna5dnf9dh6lp6jjq32t906vdyzfah2x6rt4920uh42fz43extjglwftjym33qm66ygl"
wallettxOut="$walletAddress+2000000"


policyId="d0b4c7811012fc5e9860c2fe374265f4e465ff99586ed7352fa9a866"



for filepath in ./metadatas/*; do
  filenameWithExtension=$(basename $filepath)
  filename="${filenameWithExtension%.*}"
  mint="1 $policyId.$filename"
  txuot="${wallettxOut}+$mint" 

  cardano-cli query utxo --address "$utxoaddr" --testnet-magic 1097911063 --out-file "utxo.json"
  scriptownerUtxo1=$(jq -r 'keys[0]' "utxo.json")

  cardano-cli transaction build \
    --alonzo-era \
    --cardano-mode \
    --testnet-magic 1097911063 \
    --tx-in "$scriptownerUtxo1" \
    --tx-out "$txuot" \
    --change-address "$utxoaddr" \
    --mint="$mint" \
    --mint-script-file "policy.script" \
    --metadata-json-file "$filepath" \
    --protocol-params-file protocol.json \
    --out-file "mint.body"

  cardano-cli transaction sign \
    --tx-body-file "mint.body" \
    --testnet-magic 1097911063 \
    --signing-key-file "$utxoskey" \
    --out-file "mint.tx"

  cardano-cli transaction submit --tx-file "mint.tx" --testnet-magic 1097911063
  sleep 480
done
