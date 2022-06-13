utxoskey=/home/jonathan/Documents/octoberFestMetaData/minting.skey

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
scriptAddress=`cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/script.addr --testnet-magic 1097911063`
sellerTxOut="$sellerAddress+10000000"
sellerUtxo="02c0a48f069d2f46aaf0ab2e75747942eca727e0231f55dd32c7524b396dcf2e#2"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 1097911063 \
  --tx-in "8b21fff37af9024b435ed55813ffd312d9a483d1727a327075385fb1c9b999db#0" \
  --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
  --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
  --tx-in-redeemer-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/redeemer.json \
  --required-signer-hash eefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd52 \
  --tx-in-collateral ${sellerUtxo} \
  --tx-out "$sellerTxOut" \
  --change-address "$sellerAddress" \
  --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
  --out-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body"

cardano-cli transaction sign \
  --tx-body-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "/home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed"
