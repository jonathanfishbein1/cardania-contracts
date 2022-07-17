import './style.css'
import * as Lucid from 'vasil'
import * as Wallet from '../wallet'

const
    registerConnectButton = document.getElementById('registerConnect'),
    delegateConnectButton = document.getElementById('delegateConnect'),
    deregisterConnectButton = document.getElementById('deregisterConnect'),
    poolId = "pool13dgxp4ph2ut5datuh5na4wy7hrnqgkj4fyvac3e8fzfqcc7qh0h",
    bk = "testnetwIyK8IphOti170JCngH0NedP0yK8wBZs",
    addWalletMessage = "Add a browser wallet",
    connectMessage = "connect wallet",
    registerMessage = "Register",
    deregisterMessage = "Deregister",
    delegateMessage = "Delegate",
    registeringMessage = "Registering...",
    delegatingMessage = "Delegating...",
    deregisteringMessage = "Deregistering..."
    , registeringSuccessMessage = "Successfully registered to SUMN!"
    , delegatingSuccessMessage = "Successfully delegated to SUMN!"
    , deregisterSuccessMessage = "Successfully deregistered from SUMN!"
    , blockfrostApi = 'https://cardano-testnet.blockfrost.io/api/v0'
    , blockfrostClient = new Lucid.Blockfrost(blockfrostApi, bk)
    , lucid = await Lucid.Lucid.new(blockfrostClient,
        'Testnet')
    , instantiateRegistertButton = () => {
        registerConnectButton!.innerText = registerMessage
        registerConnectButton?.removeEventListener('click', instantiateRegistertButton)
    },
    instantiateDelegateButton = () => {
        delegateConnectButton!.innerText = delegateMessage
        delegateConnectButton?.removeEventListener('click', instantiateDelegateButton)
    }
    , instantiateDeregisterButton = () => {
        deregisterConnectButton!.innerText = deregisterMessage
        deregisterConnectButton?.removeEventListener('click', instantiateDeregisterButton)

    },
    register = async () => {
        const rewardAddress = await lucid.wallet.rewardAddress()
        if (rewardAddress !== undefined) {
            const transaction =
                await lucid
                    .newTx()
                    .registerStake(rewardAddress)
                    .complete()
            transaction.txComplete
            console.log(transaction)
            console.log(transaction.txComplete)
            const signedTx = await transaction
                .sign()
                .complete()
            console.log(signedTx)
            const transactionHash = await signedTx
                .submit()
            console.log(transactionHash)
            transactionHash ?
                registerConnectButton!.innerText = registeringSuccessMessage
                :
                console.log('Transaction Hash', transaction)
        }
    }
    , instantiateRegister = () => {
        registerConnectButton?.addEventListener('click', async () => {
            registerConnectButton!.innerText = registeringMessage
            register()
        })
        registerConnectButton?.removeEventListener('click', instantiateRegister)
    }
    ,
    instantiateDelegate = () => {
        delegateConnectButton?.addEventListener('click', async () => {
            delegateConnectButton!.innerText = delegatingMessage
            delegate()
        })
        delegateConnectButton?.removeEventListener('click', instantiateDelegate)
    },
    delegate = async () => {
        const rewardAddress = await lucid.wallet.rewardAddress()
        if (rewardAddress !== undefined) {
            const transaction =
                await lucid
                    .newTx()
                    .delegateTo(rewardAddress, poolId)
                    .complete()
            transaction.txComplete
            console.log(transaction)
            console.log(transaction.txComplete)
            const signedTx = await transaction
                .sign()
                .complete()
            console.log(signedTx)
            const transactionHash = await signedTx
                .submit()
            console.log(transactionHash)
            transactionHash ?
                registerConnectButton!.innerText = registeringSuccessMessage
                :
                console.log('Transaction Hash', transaction)
        }
    }
// , instantiateCloseContract = () => {
//     deregisterConnectButton?.addEventListener('click', async () => {
//         deregisterConnectButton!.innerText = deregisteringMessage
//         closeContract()
//     })
//     deregisterConnectButton?.removeEventListener('click', instantiateCloseContract)
// },
// closeContract = async () => {
//     const closeRedeemer = new Lucid.Construct(1, [])
//         , serializedCloseRedeemer = Lucid.Data.to(closeRedeemer)
//         , utxos = (await lucid.utxosAt(scriptAddress))
//             .filter(utxo => utxo.datumHash === datumHash && utxo.assets[currencySymbol + assetNameHex] !== undefined)
//         , assetQuantity = utxos.reduce((accumulator: bigint, utxo) => accumulator + (utxo?.assets[currencySymbol + assetNameHex] as bigint), BigInt(0))
//         , transaction =
//             await lucid
//                 .newTx()
//                 .payToAddress(await lucid.wallet.address()
//                     , {
//                         lovelace: minLovelaceAmount
//                         , [currencySymbol + assetNameHex]: assetQuantity
//                     })
//                 .collectFrom(utxos, serializedCloseRedeemer)
//                 .attachSpendingValidator(radSaleScript)
//                 .addSigner(await lucid.wallet.address())
//                 .complete()
//         , signedTx = await transaction
//             .sign()
//             .complete()
//         , transactionHash = await signedTx
//             .submit()
//     console.log(transactionHash)
//     transactionHash ?
//         deregisterConnectButton!.innerText = deregisterSuccessMessage
//         :
//         console.log('Transaction Hash', transaction)
// }

registerConnectButton!.innerText = addWalletMessage
delegateConnectButton!.innerText = addWalletMessage
deregisterConnectButton!.innerText = addWalletMessage
if (Wallet.hasWallet('nami') == true) {
    const wallet = await Wallet.getWalletApi('nami') as any
    lucid.selectWallet(wallet)
    registerConnectButton!.innerText = connectMessage
    delegateConnectButton!.innerText = connectMessage
    deregisterConnectButton!.innerText = connectMessage
    registerConnectButton?.addEventListener('click', instantiateRegistertButton)
    registerConnectButton?.addEventListener('click', instantiateRegister)

    delegateConnectButton?.addEventListener('click', instantiateDelegateButton)
    delegateConnectButton?.addEventListener('click', instantiateDelegate)

    // deregisterConnectButton?.addEventListener('click', instantiateDeregisterButton)
    // deregisterConnectButton?.addEventListener('click', instantiateCloseContract)
}
