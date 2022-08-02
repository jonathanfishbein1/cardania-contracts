utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

transactionsPath=/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/

addresOne="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addresTwo="addr_test1qqfaj02yn24e38q8q4sea086yqem4tyt4n6seaxjaa4cnpzydjptew90z2m64jvqwxhzlke5kgkw82w3v52tfuwu3zssn2770q"
addressTwoTxOut="$addresTwo+2000000 + 1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.426f6e6573 + 1 650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb.416e7469677261766974795061727469636c6573"
cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file protocol.json

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "0df2f5d684e76f4565bf25c5cf772795e9ba8c8eb1a1dd98610a04e6fe5e400b#1" \
  --tx-in "6211e04065ea4a1f980a62b13d0061dc0deabcdec8851b0f2168afcb1f07f2f0#1" \
  --tx-out "$addressTwoTxOut" \
  --change-address "$addresOne" \
  --protocol-params-file protocol.json \
  --out-file "${transactionsPath}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${transactionsPath}tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "${transactionsPath}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "${transactionsPath}tx.signed"
