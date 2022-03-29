utxovkey=/home/jonathan/Documents/octoberFestMetaData/minting.vkey
utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

transactionsPath=/home/jonathan/Documents/cardania-contracts/mint-using-plutus/transactions/

cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file "${transactionsPath}protocol.json"

mintingWalletAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
wallettxOut="$mintingWalletAddress+1500000"
policyFile="${transactionsPath}result.plutus"
policyId=$(cardano-cli transaction policyid --script-file $policyFile)

mint="-1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.416e746967726176697479205061727469636c6573 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.426f6e6573 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.43617264616e69756d + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.436f636f6e757473 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.436f70706572 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.437572736564204c6971756964 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.437573746f6469616c204e616e69746573 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4461726b776f6f64 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.456e65726779 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4661697468 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4672657368205761746572 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.46756e67616c2053706f726573 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.46756e6b79204c6971756964 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.467572 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.47656d73 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.477265656e73 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.496365 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.49726f6e + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4c656174686572 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4c69676874776f6f64 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4d616368696e6520436f6d706f6e656e7473 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4d61676d61 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4d656174 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.4d6574656f72697465 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.507269736d616c656166 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.507269736d617469632044757374 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.5175616e74756d204461726b20456e65726779 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52616469756d + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.526573656172636820506f696e7473 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d20426c7565 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d20477265656e + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d204f72616e6765 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d20507572706c65 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d20526564 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d2054696d656c6f7374 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.52756e6573202d2059656c6c6f77 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.53616c747761746572 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.53616e64 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.53746f6e65 + -1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.546f786963205761737465"
txuot="${wallettxOut}" 


scriptownerUtxo="fad5ce150690ce2d9112264b8d32881ba9c2b258717cc214f315a149b70dc3c7#0"
collateralUtxo="94df2ea0231b580dea56f56ce1474ab7c2d4b5faf36b45f2fa6aeec786d58268#0"

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
  --protocol-params-file "${transactionsPath}protocol.json" \
  --out-file "${transactionsPath}mint.body"

cardano-cli transaction sign \
  --tx-body-file "${transactionsPath}mint.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "${transactionsPath}mint.tx"

cardano-cli transaction submit --tx-file "${transactionsPath}mint.tx" --testnet-magic 1097911063
