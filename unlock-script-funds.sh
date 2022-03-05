utxoskey=../octoberFestMetaData/buyer.skey

buyerAddress="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
buyerTxOut="$buyerAddress+2000000 + 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
buyerAddressUtxo="e21c0865ca6396facae6ce6aefd4def808d24e25dfa3114cfce0eb84a93ff82e#0"
buyerAddressUtxoCollateral="e21c0865ca6396facae6ce6aefd4def808d24e25dfa3114cfce0eb84a93ff82e#1"

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
sellerTxOut="$sellerAddress+10000000"

scriptAddress=`cardano-cli address build --payment-script-file ../rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063`

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in ${buyerAddressUtxo} \
    --tx-in "901c0255f54824e4d476691612d712f4eaf3e3dd9e1ee7d4a14b86ca51c89ff5#1" \
    --tx-in-script-file ../rad-sale-on-chain/transactions/result.plutus \
    --tx-in-datum-file ../rad-sale-on-chain/transactions/datum.json \
    --tx-in-redeemer-file ../rad-sale-on-chain/transactions/unit.json \
    --required-signer-hash d7b9b8604cdc89fe3421ec87e4f9e7013f109fa2efe7bed1c8b828bc \
    --tx-in-collateral ${buyerAddressUtxoCollateral} \
    --tx-out "$buyerTxOut" \
    --tx-out "$sellerTxOut" \
    --change-address ${buyerAddress} \
    --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
    --out-file ../rad-sale-on-chain/transactions/redeem.body

cardano-cli transaction sign \
  --tx-body-file "../rad-sale-on-chain/transactions/redeem.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "../rad-sale-on-chain/transactions/redeem.tx"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "../rad-sale-on-chain/transactions/redeem.tx" 


