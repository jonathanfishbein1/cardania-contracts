utxoskey=../octoberFestMetaData/buyer.skey

buyerAddress="addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
buyerTxOut="$buyerAddress+2000000 + 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
buyerAddressUtxoCollateral="e79a98d645ae54cbf1d93eb140e5077fcab80cda281b182baaece24b9a74afdb#0"

sellerAddress="addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
sellerTxOut="$sellerAddress+10000000"

scriptAddress=`cardano-cli address build --payment-script-file result.plutus --testnet-magic 1097911063`

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in "358d904f7b9773db6a2722ccdb0817ca7afffb543bd1c49ddc54af424fd6c686#0" \
    --tx-in "358d904f7b9773db6a2722ccdb0817ca7afffb543bd1c49ddc54af424fd6c686#1" \
    --tx-in "974885f3c6bc8b820b36b782bac9b49fea1a165dc346ffb5e3c31bab7dc55fd8#0" \
    --tx-in "d93d20a6364efba27ec3c8f0cff5de738600db5f91f7ce3df54f7d0775c987db#1" \
    --tx-in "0c200fd6ade8dcf81c2f9088583e2a8fcacd299b096f4bd8052309f87354964d#3" \
    --tx-in-script-file result.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --required-signer-hash d7b9b8604cdc89fe3421ec87e4f9e7013f109fa2efe7bed1c8b828bc \
    --tx-in-collateral ${buyerAddressUtxoCollateral} \
    --tx-out "$buyerTxOut" \
    --tx-out "$sellerTxOut" \
    --tx-out "$scriptAddress+2000000+ 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465" \
    --tx-out-datum-hash-file unit.json \
    --change-address ${buyerAddress} \
    --protocol-params-file ~/Documents/rad-sale-on-chain/transactions/protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file "tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "tx.signed" 


