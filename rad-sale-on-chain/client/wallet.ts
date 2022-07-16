declare var window: any

const supportedWallets = [
    'nami',
    'flint',
    'eternl'
]
export const getWalletApi = async namespace => {
    return await ('typhon' === namespace) ?
        window.cardano[namespace]
        :
        window.cardano[namespace].enable()
}
    , isSupported = type => supportedWallets.includes(type)
    , hasWallet = type => isSupported(type) && window.cardano[type.toLowerCase()] !== undefined