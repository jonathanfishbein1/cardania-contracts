import './style.css'
import {
    hasWallet, getWallet
    , getRewardAddress
    //, buyTo
    , getCollateral
} from '@jonathanfishbein1/cardano-wallet-browser-extensions-interface'

const
    connectButton = document.getElementById('connect'),
    poolId = "dc508ac7975e573adf6ef17873c74d200e0cd71931c139eb76853281",
    bk = "mainnetU8uxziYphJfG9PTWWplZD2lEHMXbJHCX",
    connectMessage = "connect wallet",
    buyMessage = "buy",
    buyingMessage = "Buying..."
    , successMessage = "Successfully bought"



const protocolParameters = await fetch('https://cardano-mainnet.blockfrost.io/api/v0/epochs/latest/parameters'
    , { headers: { project_id: bk } })
    .then(res => res.json())
console.log('protocolParameters' + protocolParameters)
const sumnPool = await fetch('https://cardano-mainnet.blockfrost.io/api/v0/pools/pool1m3gg43uhtetn4hmw79u8836dyq8qe4cex8qnn6mks5egza7n6tp'
    , { headers: { project_id: bk } })
    .then(res => res.json())
console.log(sumnPool)
if (hasWallet('nami') == true) {
    const wallet = await getWallet('nami')
    wallet.name = 'Nami'
    console.log(wallet)
    console.log(await getCollateral(wallet))
    const rewardAddress = await getRewardAddress(wallet)
    const account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${rewardAddress}/`
        , { headers: { project_id: bk } })
        .then(res => res.json())
    //const transaction = await buyTo(wallet, poolId, protocolParameters, account)
    //console.log('Transaction Hash', transaction)
}

// if (hasWallet('eternl') == true) {
//     const wallet = await getWallet('eternl')
//         , rewardAddress = await getRewardAddress(wallet)
//         , account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${rewardAddress}/`
//             , { headers: { project_id: bk } })
//             .then(res => res.json())
//     console.log('account ', account)
//     if (account.active)
//         connectButton!.innerText = successMessage
//     else {
//         connectButton!.innerText = connectMessage
//         connectButton?.addEventListener('click', async () => {
//             connectButton!.innerText = buyMessage
//             connectButton?.addEventListener('click', async () => {
//                 connectButton!.innerText = buyingMessage
//                 const transaction = await buyTo(wallet, poolId, protocolParameters, account)
//                 transaction ?
//                     connectButton!.innerText = successMessage
//                     :
//                     console.log('Transaction Hash', transaction)
//             })
//         })

//     }
// }
// else
//     if (hasWallet('Flint') == true) {
//         const wallet = await getWallet('Flint')
//         console.log(wallet)
//         const rewardAddress = await getRewardAddress(wallet)
//         const account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${rewardAddress}/`
//             , { headers: { project_id: bk } })
//             .then(res => res.json())
//         const transaction = await buyTo(wallet, poolId, protocolParameters, account)
//         console.log('Transaction Hash', transaction)
//     }

//     else if (hasWallet('Typhon') == true) {
//         const wallet = await getWallet('Typhon')
//         console.log(wallet)
//         const rewardAddress = await getRewardAddress(wallet)
//         const account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${rewardAddress}/`
//             , { headers: { project_id: bk } })
//             .then(res => res.json())
//         const transaction = await buyTo(wallet, poolId, protocolParameters, account)
//         console.log('Transaction Hash', transaction)
//     }
// console.log(hasWallet('nami'))
// if (hasWallet('GeroWallet') == true) {
//     const wallet = await getWallet('GeroWallet')
//     console.log(wallet)
//     const rewardAddress = await getRewardAddress(wallet)
//     const account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${rewardAddress}/`
//         , { headers: { project_id: bk } })
//         .then(res => res.json())
//     const transaction = await buyTo(wallet, poolId, protocolParameters, account)
//     console.log('Transaction Hash', transaction)
// }
// else
//     if (hasWallet('yoroi') == true) {
//         const wallet = await getWallet('yoroi')
//         console.log(wallet)
//         const rewardAddress = await getRewardAddress(wallet)
//         const account = await fetch(`https://cardano-mainnet.blockfrost.io/api/v0/accounts/${rewardAddress}/`
//             , { headers: { project_id: bk } })
//             .then(res => res.json())
//         const transaction = await buyTo(wallet, poolId, protocolParameters, account)
//         console.log('Transaction Hash', transaction)
//     }