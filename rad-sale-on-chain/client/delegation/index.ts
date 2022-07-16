declare var window: any
import './style.css'
import {
    Lucid, Blockfrost, Utils
} from 'lucid-cardano'
import * as Wallet from '../wallet'

const
    connectButton = document.getElementById('connect'),
    poolId = "pool1m3gg43uhtetn4hmw79u8836dyq8qe4cex8qnn6mks5egza7n6tp",
    bk = "mainnetU8uxziYphJfG9PTWWplZD2lEHMXbJHCX",
    connectMessage = "connect wallet",
    delegateMessage = "Delegate to SUMN",
    delagatingMessage = "Delegating...",
    undelagatingMessage = "Undelegating..."
    , successMessage = "Successfully delegated to SUMN!"
    , lucid = await Lucid.new(
        new Blockfrost('https://cardano-mainnet.blockfrost.io/api/v0', bk), 'Mainnet')
if (Wallet.hasWallet('eternl') == true) {
    const wallet = await Wallet.getWalletApi('eternl') as any
    lucid.selectWallet(wallet)
    const rewardAddress = await lucid.wallet.rewardAddress()
        , utils = new Utils(lucid)
    if (rewardAddress !== undefined) {
        const { address: { address } } = utils.getAddressDetails(rewardAddress)
            , account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${(address)}/`
                , { headers: { project_id: bk } })
                .then(res => res.json())
            , registrationStatus = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${(address)}/registrations`
                , { headers: { project_id: bk } })
                .then(res => res.json())
        console.log(address)
        console.log(registrationStatus)
        if (account.active && account.pool_id === poolId)
            connectButton!.innerText = successMessage
        else if (account.active) {
            connectButton!.innerText = connectMessage
            connectButton?.addEventListener('click', async () => {
                connectButton!.innerText = delegateMessage
                connectButton?.addEventListener('click', async () => {
                    connectButton!.innerText = delagatingMessage
                    if (rewardAddress !== undefined) {
                        const transaction =
                            await lucid
                                .newTx()
                                .delegateTo(address, poolId)
                                .addSigner(address)
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
                            connectButton!.innerText = successMessage
                            :
                            console.log('Transaction Hash', transaction)
                    }
                })
            })
        }
        else {
            console.log('register stake')
            connectButton!.innerText = connectMessage
            connectButton?.addEventListener('click', async () => {
                connectButton!.innerText = delegateMessage
                connectButton?.addEventListener('click', async () => {
                    connectButton!.innerText = delagatingMessage
                    if (rewardAddress !== undefined) {
                        const transaction =
                            await lucid
                                .newTx()
                                .registerStake(address)
                                .delegateTo(address, poolId)
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
                            connectButton!.innerText = successMessage
                            :
                            console.log('Transaction Hash', transaction)
                    }
                })
            })

        }
    }
}