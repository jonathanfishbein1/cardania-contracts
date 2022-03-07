utxoskey=../octoberFestMetaData/buyer.skey

buyerAddress="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
buyerTxOut="$buyerAddress+2000000 + 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
buyerAddressUtxoCollateral="fac6e4d56d91270348336584a0131abdc480c783d2edf626fa5df2b2daf04422#0"

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
sellerTxOut="$sellerAddress+10000000"

scriptAddress=`cardano-cli address build --payment-script-file ../rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063`

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in "30823788bc8856ff67687089d77cbe84abc9ae422693db36ad1ea4541f9f5893#0" \
    --tx-in "65d2208f2caf9066b32460a1cf7c7a1731f3838e4940a57b8f661aa4f6ebd60d#0" \
    --tx-in "65d2208f2caf9066b32460a1cf7c7a1731f3838e4940a57b8f661aa4f6ebd60d#1" \
    --tx-in "f1941f99b6babee42bda8161f7a18ab90d70bf30fcaa5c090996c8da0ae8117c#1" \
    --tx-in-script-file ../rad-sale-on-chain/transactions/result.plutus \
    --tx-in-datum-file ../rad-sale-on-chain/transactions/unit.json \
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


