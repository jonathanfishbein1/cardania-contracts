utxovkey=/home/jonathan/Documents/octoberFestMetaData/minting.vkey
utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

transactionsPath=/home/jonathan/Documents/cardania-contracts/mint-using-plutus/transactions/

cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file "${transactionsPath}protocol.json"

mintingWalletAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
testingWalletAddress="addr_test1qqfaj02yn24e38q8q4sea086yqem4tyt4n6seaxjaa4cnpzydjptew90z2m64jvqwxhzlke5kgkw82w3v52tfuwu3zssn2770q"
wallettxOut="$testingWalletAddress+2000000"
policyFile="${transactionsPath}result.plutus"
policyId=$(cardano-cli transaction policyid --script-file $policyFile)

for filepath in /home/jonathan/Documents/cardania-contracts/mint-using-plutus/metadata/*; do
  filenameWithExtension=$(basename "$filepath")
  filename="${filenameWithExtension%.*}"
  echo "$filename"
  tokenName=$(cabal exec token-name -- "$filename")
  mint="1 $policyId.$tokenName"
  txuot="${wallettxOut}+$mint" 
  echo "$filepath"
  cardano-cli query utxo --address "$mintingWalletAddress" --testnet-magic 1097911063 --out-file "${transactionsPath}utxo.json"
  scriptownerUtxo1=$(jq -r 'keys[0]' "${transactionsPath}utxo.json")

  cardano-cli transaction build \
    --alonzo-era \
    --cardano-mode \
    --testnet-magic 1097911063 \
    --tx-in-collateral "$scriptownerUtxo1" \
    --tx-in "$scriptownerUtxo1" \
    --tx-out "$txuot" \
    --change-address "$mintingWalletAddress" \
    --mint="$mint" \
    --mint-script-file "${transactionsPath}result.plutus" \
    --mint-redeemer-file "${transactionsPath}unit.json" \
    --required-signer-hash eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52 \
    --metadata-json-file "$filepath" \
    --protocol-params-file "${transactionsPath}protocol.json" \
    --out-file "${transactionsPath}mint.body"

  cardano-cli transaction sign \
    --tx-body-file "${transactionsPath}mint.body" \
    --testnet-magic 1097911063 \
    --signing-key-file "$utxoskey" \
    --out-file "${transactionsPath}mint.tx"

  cardano-cli transaction submit --tx-file "${transactionsPath}mint.tx" --testnet-magic 1097911063
  sleep 480
done
