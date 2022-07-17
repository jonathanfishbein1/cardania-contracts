declare var window: any

export type SupportedWallets =
    'nami'
    | 'flint'
    | 'eternl'
    | 'typhon'

export const getWalletApi = async (wallet: SupportedWallets) => {
    return await ('typhon' === wallet) ?
        window.cardano[wallet]
        :
        window.cardano[wallet].enable()
}
    , hasWallet = (wallet: SupportedWallets) => window.cardano[wallet] !== undefined