import * as wasm from '@emurgo/cardano-serialization-lib-browser'
import Blake2b from 'blake2b';
import { Buffer } from 'buffer';
import {balanceTxImpl} from '../utils/nami'

export class Cardano {

    constructor() {

    }
}

// Calculate Wallet ID concatenating the used addresses and the unused
// TODO: Not clear how it works
function calculateWalletId() {
    return Promise.all([window.cardano.getUsedAddresses(), window.cardano.getUnusedAddresses()])
        .then(([walletUsedAddresses, walletUnusedAddresses]) => {
            const addresses = walletUnusedAddresses.concat(walletUsedAddresses)
            return Blake2b(20)
                .update(Buffer.from(addresses.map(a => {
                    console.log(a)
                    const arrayBytes = Buffer.from(a, 'hex')
                    console.log(arrayBytes)
                    let address = wasm.Address.from_bytes(arrayBytes)
                    console.log(address.to_bech32())

                    return address.to_bech32()
                }).join('')))
                .digest('hex')
        })
}

// This method construct the body request for the 
// activate contract endpoint
function activateContractObject(walletId) {
    return {
        "caID": [],
        "caWalletId": {
            "getWalletId": walletId
        }
    }
}

// Make the request for activating the contract
export async function activateContractRequest() {

    const walletId = await calculateWalletId()

    const response = await fetch('/api/contract/activate',
        {
            method: 'POST'
            , headers: { 'Content-Type': 'application/json' }
            , body: JSON.stringify(activateContractObject(walletId))

        }
    )

    return await response.json()
}

function payToWalletObject(address, amount) {

    const wasmAddress = wasm.Address.from_bech32(address)

    const baseAddress = wasm.BaseAddress.from_address(wasmAddress)
    const paymentCredentialKeyHash = baseAddress.payment_cred().to_keyhash()
    const paymentPubKeyHash = Buffer.from(paymentCredentialKeyHash.to_bytes()).toString('hex')

    const stakeCredentialPubKeyHash = baseAddress.stake_cred().to_keyhash()
    const stakePubKeyHash = Buffer.from(stakeCredentialPubKeyHash.to_bytes()).toString('hex')

    return {
        "amount": {
            "getValue": [[{ "unCurrencySymbol": "" }, [[{ "unTokenName": "" }, amount]]]]
        }
        , "pkh": {
            "unPaymentPubKeyHash": {
                "getPubKeyHash": paymentPubKeyHash
            }
        }
        , "skh": {
            "unStakePubKeyHash": {
                "getPubKeyHash": stakePubKeyHash
            }
        }
    }
}


export async function payToWalletRequest(contractInstanceId, address, amount) {
    const paytoWalletObject = payToWalletObject(address, amount)

    const response = await fetch('/api/contract/instance/' + contractInstanceId + '/endpoint/PayToWallet', {
        method: "POST"
        , headers: { 'Content-Type': 'application/json' }
        , body: JSON.stringify(paytoWalletObject)
    })

    return await response.json()
}

export async function getContractInstanceStatus(contractInstanceId) {

    const response = await fetch('/api/contract/instance/' + contractInstanceId + '/status')

    return await response.json()
}

export async function paytoWalletAction(address, amount) {
    activateContractRequest().then(
        async result => {
            console.log(result)
            const contractInstanceId = await activateContractRequest()
            await payToWalletRequest(contractInstanceId.unContractInstanceId, address, amount)

            setTimeout(async () => {
                const contractInstanceStatus = await getContractInstanceStatus(contractInstanceId.unContractInstanceId)

                const txCBOR = contractInstanceStatus.cicYieldedExportTxs[0].transaction

                console.log(txCBOR)

                const btx = await balanceTxImpl(txCBOR)

                const sign = await window.cardano.signTx(btx, null)

                // Function to convert from Hex string to Array Uint8
                const fromHexString = hexString =>
                    new Uint8Array(hexString.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));

                const transaction = wasm.Transaction.from_bytes(fromHexString(btx))

                const signedTx = wasm.Transaction.new(transaction.body(), wasm.TransactionWitnessSet.from_bytes(fromHexString(sign)))

                // TODO Unecessary when 'balanceTx' is merged in Nami wallet API
                // padd with leading 0 if <16
                const i2hex = i => ('0' + i.toString(16)).slice(-2)

                // TODO Unecessary when 'balanceTx' is merged in Nami wallet API
                const toHexString = uint8 => Array.from(uint8).map(i2hex).join('');


                window.cardano.submitTx(toHexString(signedTx.to_bytes())).then(success => {
                    console.log("success: " + success)
                }).catch(error => {
                    console.log("error: " + error)
                })
            }, 1000)
        }
    )
}