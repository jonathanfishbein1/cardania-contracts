utxoskey=../octoberFestMetaData/minting.skey

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file result.plutus --testnet-magic 1097911063`
sellerTxOut="$sellerAddress+1413762+1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
sellerUtxo="32f038ad2ab00adf7a03f7cc82ba71dbfa5c2a4f76d014a715dfe399b61837b7#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "32f038ad2ab00adf7a03f7cc82ba71dbfa5c2a4f76d014a715dfe399b61837b7#1" \
  --tx-in-script-file result.plutus \
  --tx-in-datum-file unit.json \
  --tx-in-redeemer-file unit.json \
  --required-signer-hash eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52 \
  --tx-in-collateral ${sellerUtxo} \
  --tx-out "$sellerTxOut" \
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
