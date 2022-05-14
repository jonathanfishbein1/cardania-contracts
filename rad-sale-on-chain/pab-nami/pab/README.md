# Running RAD Sale PAB

We need to set up a symlink to the node.sock in this folder. We can use the next command:

`$ ln -s node.sock $CARDANO_NODE_SOCKET_PATH`

The cabal executable is in the main folder of the repository. Go to the cardania-contracts root folder and execute:

- First time:

`cabal run -- plutus-pab-nami-rad --config rad-sale-on-chain/pab-nami/pab/plutus-pab.yaml migrate`

- After the migration:

`cabal run -- plutus-pab-nami-rad --config rad-sale-on-chain/pab-nami/pab/plutus-pab.yaml webserver`


