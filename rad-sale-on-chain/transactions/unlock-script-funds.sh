#!/usr/bin/env fish
set utxoskey ../octoberFestMetaData/buyer.skey

set buyerAddress "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set buyerTxOut "$buyerAddress+2000000 + 1 641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e.43617264616e6961466f756e6465725768697465"
set buyerAddressUtxoCollateral "d93d20a6364efba27ec3c8f0cff5de738600db5f91f7ce3df54f7d0775c987db#0"

set sellerAddress "addr_test1qzd5rr90apw9z988q3274n8a4ztjgey5gjmwcjktfjzzw95twg0mg7ykwud2263mlllla45yyrdmfa04rxyepz8ghgxqd2yked"
set sellerTxOut "$sellerAddress+10000000"

set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
echo $scriptAddress
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in "35854d96b2500d33ec40b12ad16e9ac6caa8d125dd32db8b0da36afe49d06af8#1" \
    --tx-in "3e9d21c2e8536598f9936b51ebe70129f6069fac9b7f43d208586671b684dc19#0" \
    --tx-in "a0d35899eb0407379b47e8bc8893b619be0e61a8a4e785b0454dbe4733f530a1#1" \
    --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
    --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --tx-in-redeemer-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --required-signer-hash d7b9b8604cdc89fe3421ec87e4f9e7013f109fa2efe7bed1c8b828bc \
    --tx-in-collateral $buyerAddressUtxoCollateral \
    --tx-out $buyerTxOut \
    --tx-out $sellerTxOut \
    --tx-out "$scriptAddress+2000000" \
    --tx-out-datum-hash-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --change-address $buyerAddress \
    --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file "tx.body" \
  --testnet-magic 1097911063 \
  --signing-key-file "$utxoskey" \
  --out-file "tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file "tx.signed" 


