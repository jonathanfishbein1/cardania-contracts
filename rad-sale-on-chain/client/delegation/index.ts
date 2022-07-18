import './style.css'
import * as Lucid from 'lucid-cardano'
import * as Wallet from '../wallet'

const
    registerConnectButton = document.getElementById('registerConnect') as HTMLButtonElement,
    delegateConnectButton = document.getElementById('delegateConnect') as HTMLButtonElement,
    deregisterConnectButton = document.getElementById('deregisterConnect') as HTMLButtonElement,
    registerAndDelegateButton = document.getElementById('registerAndDelegateConnect') as HTMLButtonElement,
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
    register = async rewardAddress => {
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
    , instantiateRegister = (rewardAddress) => {
        registerConnectButton?.addEventListener('click', async () => {
            registerConnectButton!.innerText = registeringMessage
            register(rewardAddress)
        })
        registerConnectButton?.removeEventListener('click', instantiateRegister)
    }
    ,
    instantiateDelegate = rewardAddress => {
        delegateConnectButton?.addEventListener('click', async () => {
            delegateConnectButton!.innerText = delegatingMessage
            delegate(rewardAddress)
        })
        delegateConnectButton?.removeEventListener('click', instantiateDelegate)
    },
    delegate = async rewardAddress => {
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
    , instantiateDeregister = rewardAddress => {
        deregisterConnectButton?.addEventListener('click', async () => {
            deregisterConnectButton!.innerText = deregisteringMessage
            deregister(rewardAddress)
        })
        deregisterConnectButton?.removeEventListener('click', instantiateDeregister)
    },
    deregister = async rewardAddress => {
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
    , instantiateRegisterAndDelegate = rewardAddress => {
        registerAndDelegateButton?.addEventListener('click', async () => {
            registerAndDelegateButton!.innerText = registeringAndDelegatingMessage
            registerAndDelegate(rewardAddress)
        })
        registerAndDelegateButton?.removeEventListener('click', instantiateRegisterAndDelegate)
    }
    , registerAndDelegate = async rewardAddress => {
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

registerConnectButton!.innerText = addWalletMessage
delegateConnectButton!.innerText = addWalletMessage
deregisterConnectButton!.innerText = addWalletMessage
registerAndDelegateButton!.innerText = addWalletMessage
const supportedWallet = Wallet.hasWallet()
if (supportedWallet !== undefined) {
    const wallet = await Wallet.getWalletApi(supportedWallet) as any
    lucid.selectWallet(wallet)
    const rewardAddress = await lucid.wallet.rewardAddress()
    if (rewardAddress !== undefined) {
        const appliedInstantiageRegister = () => instantiateRegister(rewardAddress)
            , appliedInstantiageDelegate = () => instantiateDelegate(rewardAddress)
            , appliedInstantiageDeregister = () => instantiateDeregister(rewardAddress)
            , appliedInstantiageRegisterAndDelegate = () => instantiateRegisterAndDelegate(rewardAddress)
            , utils = new Lucid.Utils(lucid)
            , { address: { address } } = utils.getAddressDetails(rewardAddress)
            , account = await fetch(`${blockfrostApi}/accounts/${(address)}/`
                , { headers: { project_id: bk } })
                .then(res => res.json())
        console.log(account)
        if (account.active && account.pool_id === poolId) {
            if (deregisterConnectButton !== null) {
                deregisterConnectButton.style.visibility = 'visible'
                deregisterConnectButton.style.display = 'inline'
                deregisterConnectButton.disabled = false
                deregisterConnectButton?.addEventListener('click', instantiateDeregisterButton)
                deregisterConnectButton?.addEventListener('click', appliedInstantiageDeregister)
            }
            else
                console.log('Cannot find button on page')
        }
        else if (account.active) {
            if (delegateConnectButton !== null) {
                delegateConnectButton.style.visibility = 'visible'
                delegateConnectButton.style.display = 'inline'
                delegateConnectButton.disabled = false
                delegateConnectButton?.addEventListener('click', instantiateDelegateButton)
                delegateConnectButton?.addEventListener('click', appliedInstantiageDelegate)
            }
            else
                console.log('Cannot find button on page')

        }
        else {
            if (registerAndDelegateButton !== null) {
                registerAndDelegateButton.style.visibility = 'visible'
                registerAndDelegateButton.style.display = 'inline'
                registerAndDelegateButton.disabled = false
                registerAndDelegateButton?.addEventListener('click', instantiateRegisterAndDelegateButton)
                registerAndDelegateButton?.addEventListener('click', appliedInstantiageRegisterAndDelegate)
            }
            else
                console.log('Cannot find button on page')
        }
    }
    else
        console.log('Reward address is undefined')
}
else
    console.log('No supported wallet')

