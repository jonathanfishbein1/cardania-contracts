import './style.css'
import * as Lucid from 'lucid-cardano'
import * as Wallet from '../wallet'

const
    registerConnectButton = document.getElementById('registerConnect'),
    delegateConnectButton = document.getElementById('delegateConnect'),
    deregisterConnectButton = document.getElementById('deregisterConnect'),
    registerAndDelegateButton = document.getElementById('registerAndDelegateConnect'),
    poolId = "pool13dgxp4ph2ut5datuh5na4wy7hrnqgkj4fyvac3e8fzfqcc7qh0h",
    bk = "testnetwIyK8IphOti170JCngH0NedP0yK8wBZs",
    addWalletMessage = "Add a browser wallet",
    connectMessage = "connect wallet",
    registerMessage = "Register",
    deregisterMessage = "Deregister",
    delegateMessage = "Delegate",
    registerAndDelegateMessage = "Delegate",
    registeringMessage = "Registering...",
    delegatingMessage = "Delegating...",
    deregisteringMessage = "Deregistering..."
    , registeringAndDelegatingMessage = "Delegating..."
    , registeringSuccessMessage = "Successfully registered to SUMN!"
    , delegatingSuccessMessage = "Successfully delegated to SUMN!"
    , deregisteringSuccessMessage = "Successfully deregistered from SUMN!"
    , registeringAndDelegatinguccessMessage = "Successfully delegated to SUMN!"
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

    }
    , instantiateRegisterAndDelegateButton = () => {
        registerAndDelegateButton!.innerText = registerAndDelegateMessage
        registerAndDelegateButton?.removeEventListener('click', instantiateRegisterAndDelegateButton)
    },
    register = async () => {
        const rewardAddress = await lucid.wallet.rewardAddress()
        if (rewardAddress !== undefined) {
            const transaction =
                await lucid
                    .newTx()
                    .registerStake(rewardAddress)
                    .complete()
                , signedTx = await transaction
                    .sign()
                    .complete()
                , transactionHash = await signedTx
                    .submit()
            transactionHash ?
                registerConnectButton!.innerText = registeringSuccessMessage
                :
                console.log('Error registering')
        }
        else
            console.log('Reward address is undefined')
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
                , signedTx = await transaction
                    .sign()
                    .complete()
                , transactionHash = await signedTx
                    .submit()
            transactionHash ?
                delegateConnectButton!.innerText = delegatingSuccessMessage
                :
                console.log('Error delegating')
        }
        else
            console.log('Reward address is undefined')
    }
    , instantiateDeregister = () => {
        deregisterConnectButton?.addEventListener('click', async () => {
            deregisterConnectButton!.innerText = deregisteringMessage
            deregister()
        })
        deregisterConnectButton?.removeEventListener('click', instantiateDeregister)
    },
    deregister = async () => {
        const rewardAddress = await lucid.wallet.rewardAddress()
        if (rewardAddress !== undefined) {
            const transaction =
                await lucid
                    .newTx()
                    .deregisterStake(rewardAddress)
                    .complete()
                , signedTx = await transaction
                    .sign()
                    .complete()
                , transactionHash = await signedTx
                    .submit()
            transactionHash ?
                deregisterConnectButton!.innerText = deregisteringSuccessMessage
                :
                console.log('Error deregistering')
        }
        else
            console.log('Reward address is undefined')
    }
    , instantiateRegisterAndDelegate = () => {
        registerAndDelegateButton?.addEventListener('click', async () => {
            registerAndDelegateButton!.innerText = registeringAndDelegatingMessage
            registerAndDelegate()
        })
        registerAndDelegateButton?.removeEventListener('click', instantiateRegisterAndDelegate)
    }
    , registerAndDelegate = async () => {
        const rewardAddress = await lucid.wallet.rewardAddress()
        if (rewardAddress !== undefined) {
            const transaction =
                await lucid
                    .newTx()
                    .registerStake(rewardAddress)
                    .delegateTo(rewardAddress, poolId)
                    .complete()
                , signedTx = await transaction
                    .sign()
                    .complete()
                , transactionHash = await signedTx
                    .submit()
            transactionHash ?
                registerAndDelegateButton!.innerText = registeringAndDelegatinguccessMessage
                :
                console.log('Error registering')
        }
        else
            console.log('Reward address is undefined')
    }

registerConnectButton!.innerText = addWalletMessage
delegateConnectButton!.innerText = addWalletMessage
deregisterConnectButton!.innerText = addWalletMessage
registerAndDelegateButton!.innerText = addWalletMessage
const supportedWallet = Wallet.hasWallet()
if (supportedWallet !== undefined) {
    const wallet = await Wallet.getWalletApi(supportedWallet) as any
    lucid.selectWallet(wallet)
    registerConnectButton!.innerText = connectMessage
    delegateConnectButton!.innerText = connectMessage
    deregisterConnectButton!.innerText = connectMessage
    registerAndDelegateButton!.innerText = connectMessage
    registerConnectButton?.addEventListener('click', instantiateRegistertButton)
    registerConnectButton?.addEventListener('click', instantiateRegister)

    delegateConnectButton?.addEventListener('click', instantiateDelegateButton)
    delegateConnectButton?.addEventListener('click', instantiateDelegate)

    deregisterConnectButton?.addEventListener('click', instantiateDeregisterButton)
    deregisterConnectButton?.addEventListener('click', instantiateDeregister)

    registerAndDelegateButton?.addEventListener('click', instantiateRegisterAndDelegateButton)
    registerAndDelegateButton?.addEventListener('click', instantiateRegisterAndDelegate)
}

