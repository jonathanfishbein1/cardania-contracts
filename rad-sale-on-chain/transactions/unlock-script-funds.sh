#!/usr/bin/env fish
set utxoskey /home/jonathan/Documents/octoberFestMetaData/buyer.skey

set buyerAddress "addr_test1vrtmnwrqfnwgnl35y8kg0e8euuqn7yyl5th700k3ezuz30q8hrt37"
set buyerTxOut "$buyerAddress+2000000+1 fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50.434c415353494342414259424c55453031"
set buyerAddressCollateralUtxo "1dd743d2ddf20bc4a49e00bcd04cbb612959b51b300b5c571eb775c6dd4b698b#0"
set sellerAddress "addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy"
set sellerTxOut "$sellerAddress+10000000"

set scriptAddress (eval cardano-cli address build --payment-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus --testnet-magic 1097911063)
echo $scriptAddress
set protocolParameters (eval cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json)

cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
    --testnet-magic 1097911063 \
    --tx-in "56bae5505f29a15802137aba1e3ea6c25c5967e37b851bbca704042422fc19c6#1" \
    --tx-in "5d381af6b17789411d68787dbdaa26c85ef777db5010fe9ac3a71a10a042f93a#1" \
    --tx-in-script-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/result.plutus \
    --tx-in-datum-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --tx-in-redeemer-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/buy-redeemer.json \
    --required-signer-hash d7b9b8604cdc89fe3421ec87e4f9e7013f109fa2efe7bed1c8b828bc \
    --tx-in-collateral $buyerAddressCollateralUtxo \
    --tx-out $buyerTxOut \
    --tx-out $sellerTxOut \
    --tx-out "$scriptAddress+3000000+1 fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50.434c415353494342414259424c55453031" \
    --tx-out-datum-hash-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/unit.json \
    --change-address $buyerAddress \
    --protocol-params-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/protocol.json \
    --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body

cardano-cli transaction sign \
  --tx-body-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.body \
  --testnet-magic 1097911063 \
  --signing-key-file $utxoskey \
  --out-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file /home/jonathan/Documents/cardania-contracts/rad-sale-on-chain/transactions/tx.signed


