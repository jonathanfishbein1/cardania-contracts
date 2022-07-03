declare var window: any
import './style.css'
import {
    Lucid, Blockfrost, Utils
} from 'lucid-cardano'
const supportedWallets = [
    'nami',
    'flint',
    'eternl'
]
    , getWalletApi = async namespace => {
        return await ('typhon' === namespace) ?
            window.cardano[namespace]
            :
            window.cardano[namespace].enable()
    }
    , isSupported = type => supportedWallets.includes(type)
    , hasWallet = type => isSupported(type) && window.cardano[type.toLowerCase()] !== undefined
const
    connectButton = document.getElementById('connect'),
    poolId = "pool1m3gg43uhtetn4hmw79u8836dyq8qe4cex8qnn6mks5egza7n6tp",
    bk = "testnetwIyK8IphOti170JCngH0NedP0yK8wBZs",
    connectMessage = "connect wallet",
    buyMessage = "Buy",
    buyingMessage = "Buying...",
    unbuyingMessage = "Unbuying..."
    , successMessage = "Successfully buyd to SUMN!"
    , lucid = await Lucid.new(
        new Blockfrost('https://cardano-testnet.blockfrost.io/api/v0', bk), 'Testnet')
if (hasWallet('eternl') == true) {
    const wallet = await getWalletApi('eternl') as any
    lucid.selectWallet(wallet)
    connectButton!.innerText = connectMessage
    connectButton?.addEventListener('click', async () => {
        connectButton!.innerText = buyMessage
        connectButton?.addEventListener('click', async () => {
            connectButton!.innerText = buyingMessage
            const lovelaceAmount = BigInt(Number(10000000))
            const transaction =
                await lucid
                    .newTx()
                    .payToAddress('addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy', { lovelace: lovelaceAmount })
                    .addSigner(await lucid.wallet.address())
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
        })
    })
}