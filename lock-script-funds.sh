utxoskey=../octoberFestMetaData/minting.skey

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file ../rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063`
scriptTxOut="$scriptAddress+3000000+1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
sellerAddressUtxo="30823788bc8856ff67687089d77cbe84abc9ae422693db36ad1ea4541f9f5893#2"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "ada0a6fc46806366066412beb8cebc1ab357983d5060e3107f9b08f40ecf622b#0" \
  --tx-in "fac6e4d56d91270348336584a0131abdc480c783d2edf626fa5df2b2daf04422#1" \
  --tx-in "$sellerAddressUtxo" \
  --tx-out "$scriptTxOut" \
  --tx-out-datum-hash-file ../rad-sale-on-chain/transactions/unit.json \
  --change-address "$sellerAddress" \
  --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
  --out-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.body"

cardano-cli transaction sign \
  --tx-body-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.tx"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "../rad-sale-on-chain/transactions/rad-sale-on-chain.tx"
