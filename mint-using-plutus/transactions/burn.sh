cardano-cli query protocol-parameters --mainnet --out-file protocol.json

walletAddress="addr1qxldw6tu3l644fftqpepp9rz6c76kwcna5dnf9dh6lp6jjq32t906vdyzfah2x6rt4920uh42fz43extjglwftjym33qm66ygl"
wallettxOut="$walletAddress+1500000"


policyId="d0b4c7811012fc5e9860c2fe374265f4e465ff99586ed7352fa9a866"


mint="-1 $policyId.WarpFuelGalactic13"
txuot="${wallettxOut}+$mint" 


scriptownerUtxo1="0ec40b6a02746fe9a19679deceb14f84eb7f0955fcdd70eb74c2e2d25c85853f#0"

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --mainnet \
  --tx-in "$scriptownerUtxo1" \
  --tx-out "$txuot" \
  --change-address "$walletAddress" \
  --mint="$mint" \
  --mint-script-file "policy.script" \
  --metadata-json-file "./burningMetadatas/WarpFuelGalactic13.json" \
  --protocol-params-file protocol.json \
  --out-file "plutusburn.body"

cardano-cli transaction sign \
  --tx-body-file "plutusburn.body" \
  --mainnet \
  --signing-key-file "$utxoskey" \
  --out-file "plutusburn.tx"

cardano-cli transaction submit --tx-file "plutusmint.tx" --mainnet
