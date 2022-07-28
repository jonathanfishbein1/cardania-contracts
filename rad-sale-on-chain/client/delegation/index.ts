import './style.css'
import * as Lucid from 'lucid-cardano'
import * as Wallet from '../wallet'
var { Elm } = require('./src/Main.elm')

const
    sumnPoolId = "pool13dgxp4ph2ut5datuh5na4wy7hrnqgkj4fyvac3e8fzfqcc7qh0h",
    bk = "testnetwIyK8IphOti170JCngH0NedP0yK8wBZs"
    , blockfrostApi = 'https://cardano-testnet.blockfrost.io/api/v0'
    , blockfrostClient = new Lucid.Blockfrost(blockfrostApi, bk)
    , lucid = await Lucid.Lucid.new(blockfrostClient,
        'Testnet'),
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
    },
    delegate = async rewardAddress => {
        const transaction =
            await lucid
                .newTx()
                .delegateTo(rewardAddress, sumnPoolId)
                .complete()
            , signedTx = await transaction
                .sign()
                .complete()
            , transactionHash = await signedTx
                .submit()
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
    }
    , registerAndDelegate = async rewardAddress => {
        const transaction =
            await lucid
                .newTx()
                .registerStake(rewardAddress)
                .delegateTo(rewardAddress, sumnPoolId)
                .complete()
            , signedTx = await transaction
                .sign()
                .complete()
            , transactionHash = await signedTx
                .submit()
    }





console.log('here')

console.log('here')

var app = Elm.Main.init({
    flags: [Wallet.hasWallet(), sumnPoolId],
    node: document.getElementById("elm-app-is-loaded-here")
})

app.ports.connectWallet.subscribe(async supportedWallet => {
    const wallet = await Wallet.getWalletApi(supportedWallet!) as any
    lucid.selectWallet(wallet)
    console.log(wallet)
    app.ports.walletConnection.send(supportedWallet)
})

app.ports.getAccountStatus.subscribe(async () => {
    const rewardAddress = await lucid.wallet.rewardAddress()
        , utils = new Lucid.Utils(lucid)
        , { address: { address } } = utils.getAddressDetails(rewardAddress!)
        , account = await fetch(`${blockfrostApi}/accounts/${(address)}/`
            , { headers: { project_id: bk } })
            .then(res => res.json())
    console.log(account)
    app.ports.receiveAccountStatus.send(JSON.stringify(account))
})


app.ports.registerAndDelegateToSumn.subscribe(async rewardAddress =>
    registerAndDelegate(rewardAddress))