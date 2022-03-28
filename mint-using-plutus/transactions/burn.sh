utxovkey=/home/jonathan/Documents/octoberFestMetaData/minting.vkey
utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

transactionsPath=/home/jonathan/Documents/cardania-contracts/mint-using-plutus/transactions/

cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file "${transactionsPath}protocol.json"

mintingWalletAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
wallettxOut="$mintingWalletAddress+1500000"
policyFile="${transactionsPath}result.plutus"
policyId=$(cardano-cli transaction policyid --script-file $policyFile)

mint="-2 $policyId.426f6e6573"
txuot="${wallettxOut}" 


scriptownerUtxo="64d175b8b695261edc83cf5350fb4c6e283108c9121e7928aa3eb0e71c7ad280#0"
collateralUtxo="fa6452e94b21815aea7e829a666b2f73506ee694925c5d7f92e3c41ad7772734#0"

cardano-cli transaction build \
  --testnet-magic 1097911063 \
  --tx-in "$scriptownerUtxo" \
  --tx-in "$collateralUtxo" \
  --tx-in-collateral "$collateralUtxo" \
  --tx-out "$txuot" \
  --change-address "$mintingWalletAddress" \
  --mint="$mint" \
  --mint-script-file "${transactionsPath}result.plutus" \
  --mint-redeemer-file "${transactionsPath}unit.json" \
  --required-signer-hash eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52 \
  --metadata-json-file "/home/jonathan/Documents/cardania-contracts/mint-using-plutus/metadata/AntigravityParticles.json" \
  --protocol-params-file "${transactionsPath}protocol.json" \
  --out-file "${transactionsPath}mint.body"

cardano-cli transaction sign \
  --tx-body-file "${transactionsPath}mint.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "${transactionsPath}mint.tx"

cardano-cli transaction submit --tx-file "${transactionsPath}mint.tx" --testnet-magic 1097911063
