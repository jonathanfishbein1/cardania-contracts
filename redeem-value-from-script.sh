utxoskey=../octoberFestMetaData/minting.skey

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file ../rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063`
sellerTxOut="$sellerAddress+1500000+ 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
scriptUtxo="2b114eee505c9b9f700d7bd8c7b17c561e4c1819344c6542091fae0d4a74af93#1"
sellerUtxo="2b114eee505c9b9f700d7bd8c7b17c561e4c1819344c6542091fae0d4a74af93#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "$scriptUtxo" \
  --tx-in-script-file ../rad-sale-on-chain/transactions/result.plutus \
  --tx-in-datum-file ../rad-sale-on-chain/transactions/unit.json \
  --tx-in-redeemer-file ../rad-sale-on-chain/transactions/unit.json \
  --required-signer-hash eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52 \
  --tx-in-collateral ${sellerUtxo} \
  --tx-out "$sellerTxOut" \
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
