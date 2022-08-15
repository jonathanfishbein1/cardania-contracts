declare var window: any

export type SupportedWallet =
    'nami'
    | 'flint'
    | 'eternl'

export const getWalletApi = async (wallet: SupportedWallet) => window.cardano[wallet].enable()
    , hasWallet = () => supportedWallets.find(supportedWallet => window.cardano[supportedWallet] !== undefined)
const supportedWallets: Array<SupportedWallet> =
    ['nami'
        , 'flint'
        , 'eternl']