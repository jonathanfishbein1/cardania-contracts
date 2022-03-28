utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

transactionsPath=/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/

addresOne="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
addresTwo="addr_test1qqfaj02yn24e38q8q4sea086yqem4tyt4n6seaxjaa4cnpzydjptew90z2m64jvqwxhzlke5kgkw82w3v52tfuwu3zssn2770q"
addressTwoTxOut="$addresTwo+30000000"
cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file protocol.json

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "0907bc65332ada538aebc2598881b7a394d0dfe546ee910614cad387bba4f858#1" \
  --tx-in "2eaee34a785c3755a3357a741dc1a2089efc3ba5c7e0e88222c6a910a7b633c5#0" \
  --tx-in "437a54f1474b54dfeebdb828f1e56739f230b6c9456b653b5d9eea97fbfccf7a#0" \
  --tx-in "52039648d6dd1cc8429627b11549c5e05cef9e69a05bdeeb5ef91c8cc7bbc6e3#0" \
  --tx-in "889837474d47681b7a11c485cddbe840636a510aef0b11ca9784b08dcc27292e#1" \
  --tx-in "936fa16d8bed65f801031a15e0b4da6a31fad965bfc0f50241e2d87ac12a71cb#1" \
  --tx-in "9b67802845d7442ac330b1bb80571d5b13fd982f0885ab5d4fe77b1128c90eec#0" \
  --tx-in "9d2c9779531d961680409c0c3502fc79a81025af1768c4ce1dcfed7fde0a6720#1" \
  --tx-in "d8aefcc51288a4f90e6e9cae469f410648ce581ab594961311337604e1b9085b#0" \
  --tx-in "dbe5e56fb471a38ed4e594184fda5e622e0ea933c7b6aabf89880eebf11bbc0d#1" \
  --tx-in "fccd012cafdf1ecfff9ee76948fcf34fb8b9f12b928b983584f944782a5fed63#1" \
  --tx-in "fccd012cafdf1ecfff9ee76948fcf34fb8b9f12b928b983584f944782a5fed63#0" \
  --tx-out "$addressTwoTxOut" \
  --change-address "$addresTwo" \
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
