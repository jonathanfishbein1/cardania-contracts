utxoskey=../octoberFestMetaData/minting.skey

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file result.plutus --testnet-magic 1097911063`
scriptTxOut="$scriptAddress+3000000+3 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
sellerAddressUtxo="37607131022445c438e0512e1de44b8e0f27650036a835c3c2a3417ffe36ee54#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "433eb9e97aa2a47814c52182e7ed3bce6943bb4fc73bf2157840cfcbb0d02ecc#1" \
  --tx-in "59b5a23ea877c99cdb54700bf0e1e72f3bd399f552100d36a8a0125317b452a7#0" \
  --tx-in "$sellerAddressUtxo" \
  --tx-out "$scriptTxOut" \
  --tx-out-datum-hash-file unit.json \
  --change-address "$sellerAddress" \
  --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
  --out-file "tx.body"

cardano-cli transaction sign \
  --tx-body-file "tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "tx.signed"
