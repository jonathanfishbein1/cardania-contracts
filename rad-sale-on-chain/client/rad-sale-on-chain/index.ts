import './style.css'
import * as Lucid from 'lucid-cardano'
import * as Wallet from '../wallet'

const
    buyConnectButton = document.getElementById('buyConnect'),
    startConnectButton = document.getElementById('startConnect'),
    closeConnectButton = document.getElementById('closeConnect'),
    bk = "testnetwIyK8IphOti170JCngH0NedP0yK8wBZs",
    addWalletMessage = "Add a browser wallet",
    connectMessage = "connect wallet",
    startMessage = "Start",
    closeMessage = "Close",
    buyMessage = "Buy",
    startingMessage = "Starting...",
    buyingMessage = "Buying...",
    closingMessage = "Closing..."
    , startSuccessMessage = "Successfully started contract!"
    , successMessage = "Successfully bought token!"
    , closeSuccessMessage = "Successfully closed contract!"
    , blockfrostApi = 'https://cardano-testnet.blockfrost.io/api/v0'
    , blockfrostClient = new Lucid.Blockfrost(blockfrostApi, bk)
    , lucid = await Lucid.Lucid.new(blockfrostClient,
        'Testnet')
    , scriptCBORHex = '59112159111e01000033232323232323232323232323232332232323232222322323253353330073333573466e1cd55ce9baa0064800080648c98d4cd5ce00d80c80c00b9999ab9a3370ea0089001109100091999ab9a3370ea00a9000109100111931a99ab9c01c01a0190180173333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa004900011991091980080180118101aba15002301a357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a8101aba15006335015335502302175a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502675a6ae854008c09cd5d09aba2500223263533573805e05a05805626aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a04ceb4d5d0a80118139aba135744a004464c6a66ae700bc0b40b00ac4d55cf280089baa001357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854010cd4055d71aba15003335015335502375c40026ae854008c074d5d09aba2500223263533573804e04a04804626ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053018357426aae79400c8cccd5cd19b875002480108c848888c008014c068d5d09aab9e500423333573466e1d400d20022321222230010053016357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7008808007c07807407006c4d55cea80089baa001357426ae8940088c98d4cd5ce00d80c80c00b880c09931a99ab9c49010350543500018017135573ca00226ea80044d55ce9baa0011232230023758002640026aa028446666aae7c004940248cd4020c010d5d080118019aba200201323232323333573466e1cd55cea801a40004666444246660020080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80700a1aba135744a004464c6a66ae7006806005c0584d55cf280089baa00135742a006666aa00eeb94018d5d0a80119a8053ae357426ae8940088c98d4cd5ce00b00a00980909aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02444646666aae7c008940208cd401ccd54050c018d55cea80118029aab9e500230043574400602426ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263533573802402001e01c01a26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263533573801e01a01801626aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931a99ab9c00d00b00a0091375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931a99ab9c01000e00d00c00b00a135573aa00226ea80048c8cccd5cd19b8750014800884880088cccd5cd19b8750024800084880048c98d4cd5ce00600500480400389aab9d3754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931a99ab9c01401201101000f00e00d00c00b00a135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98d4cd5ce00680580500480409aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6a66ae7002802001c0180144d55cea80089baa0011122232323333573466e1cd55cea80124000466aa012600c6ae854008c014d5d09aba2500223263533573801401000e00c26aae7940044dd5000a4c240022244246600200600492103505431001123230010012233003300200200133232323232323322323322323322323233223232323232323232323232332232323232323232323232322222323235005533500413353335553025120013322123330010030021122335002235503000123355502e00100250285029200133223300232533532533500110241533502410231024333200150113300a0013500922220013233502a335503102533502a3355031025001502b502b350092222004135502c032135502d49116496e636f727265637420547820746f2073656c6c657200500333002323355501f5335350012222222222003135502d4901094e6f207369676e6572002215335001135502e00222135503149110546f6f206d616e79207369676e65727300253353301b323235001223330260040020013355032302000b300f00b3300b002001480084d540b40cc4d540b9240115496e636f727265637420547820746f206275796572005003330023355501e5335350052235002222222222253353301a00a00b2135001223500122233355302512001223500222235008223500522325335335005233500425335333573466e3c0080041000fc5400c40fc80fc8cd401080fc94cd4ccd5cd19b8f00200104003f15003103f133504400a0091009153350032153350022133500223350022335002233500223303900200120422335002204223303900200122204222233500420422225335333573466e1c01800c11411054cd4ccd5cd19b8700500204504413303c00400110441044103d153350012103d103d503b00f1326353357389201024c660003d03c135502c491144e6f20636f6e74696e75696e67206f7574707574002215335001135502d0022213550304911a546f6f206d616e7920636f6e74696e75696e67206f757470757400253353355025350012220012335502632350012222222222533533355302412001501c235001225335333573466e3c00803c0c80c44d40d800c540d400884d40d0d400488004540c8c03c01c8c8ccccccd5d200111999ab9a3370e6aae7540092000233335573e6aae79400c8d40b40e0940b00dc940ac0d4940a8940a8940a8940a80d44dd5000909aa81681989aa816a491e4572726f7220646573657269616c697a696e672074784f7574446174756d00330023355501e533535005223500222222222223301a00a00b2135502c32350012220023500122001135502c49011343616e742066696e64206f776e20696e70757400232323350022355030001200253353301b3330225006301f00a300e00a32337020029001199811001180f805180700509aa81681989aa81724812257726f6e67204e617469766520746f6b656e206f7574707574207175616e7469747900533533320015011302000130205005135502c023135502d4911d57726f6e672061646120736372697074206f75747075742076616c75650000150265027233573800204040422a66a64646a0044444444444a66a666aa604224002a0324a66a666ae68cdc780600081681609a8188008a8180019081688159a8039111000a8008810099ab9c4911a57726f6e67207369676e657220746f20636c6f73652073616c650001f135002220021323235002222222222233355301b120015012503133553013120012350012200133355301b1200122350022225335001213500422335002200825335333573466e3c0040500cc0c84cd40e4cd541000100180204020401140c4024d4d400888d400888888888894cd4cc05c02802c84d400488d4004888d400c88cd40088c1012625335004213355042002001130404984c0dd262200230090012233355300c1200150035022350022222222222333553016120012235002222350032233500225335333573466e3c04c0040b40b04cd40cc01401c401c801d40b0024488cd54c038480048d400488cd540a8008cd54c044480048d400488cd540b4008ccd40048cc0b12000001223302d00200123302c00148000004cc024008004c8004d5409c8844894cd400454088884cd408cc010008cd54c018480040100048d400488880088d40048800888cccd40049406894068940688ccd54c0304800540108d4004894cd54cd4ccd5cd19b8f3500222002350042200201a0191333573466e1cd400888004d40108800406806440644d407800c5407400c4cd4034894cd40088400c40054058488ccd54c02448004d403540308d400488ccd54c03048004d4041403c8d400488ccd40048cc0492000001223301300200123301200148000004cc00c00800488cd54c01c480048d400488cd5408c008ccd40048cd54c02c480048d400488cd5409c008d5403400400488ccd5540200380080048cd54c02c480048d400488cd5409c008d54030004004ccd55400c024008004444888ccd54c010480054064cd54c01c480048d400488cd5408c008d54024004ccd54c0104800488d4008894cd4ccd54c03048004c8cd404888ccd400c88008008004d40048800448cc004894cd40084068400405c8d400488cc028008014018400c4cd407401000d4068004cd54c01c480048d400488c8cd5409000cc004014c8004d54098894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5407c88448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c004010c8004d540708844894cd40045405c884cd4060c010008cd54c01848004010004c8004d5406c88448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000488ccd5cd19b8f00200100c00b22333573466e1c00800402c028448cc004894cd40084004402802448cd400888ccd400c88008008004d40048800448848cc00400c0088d4004888800c44488cd40088d540440048c0080048cd4028cd54044014cd4028cd54044014ccc008004014015402d402c888c8c8c004014c8004d5405888cd400520002235002225335333573466e3c00802403002c4c01c0044c01800cc8004d5405488cd400520002235002225335333573466e3c00801c02c02840044c01800c48800848800522100112253350022130020011500312122300200311220013200135500b2211122253350011353500322200250072213355350052220013355350052220035008300400233355530071200100500400111220021221223300100400311122335001235500500123550043003001112122300200311212230010032233700004002464c6a66ae712401024c67000040031122123300100300249848004448c8c00400488cc00cc008008004cd4488cccc0092080dac4094891c641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e0048811443617264616e6961466f756e646572477265656e0048811ceefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd5200222212333300100500400300220011'
    , datum = Lucid.Data.empty()
    , currencySymbol = '641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e'
    , assetNameHex = '43617264616e6961466f756e646572477265656e'
    , radSaleScript: Lucid.Script = {
        type: 'PlutusV1'
        , script: scriptCBORHex
    }
    , scriptAddress = lucid.utils.validatorToAddress(radSaleScript)
    , minLovelaceAmount = BigInt(2000000)
    , priceOfTokenLovelace = BigInt(10000000)
    , datumHash = '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec'
    , instantiateStartButton = () => {
        startConnectButton!.innerText = startMessage
        startConnectButton?.removeEventListener('click', instantiateStartButton)
    },
    instantiateBuyButton = () => {
        buyConnectButton!.innerText = buyMessage
        buyConnectButton?.removeEventListener('click', instantiateBuyButton)
    }
    , instantiateCloseButton = () => {
        closeConnectButton!.innerText = closeMessage
        closeConnectButton?.removeEventListener('click', instantiateCloseButton)

    },
    startContract = async () => {
        const transaction =
            await lucid
                .newTx()
                .payToContract(scriptAddress
                    , datum
                    , {
                        lovelace: minLovelaceAmount,
                        [currencySymbol + assetNameHex]: BigInt(3)
                    })
                .complete()
            , signedTx = await transaction
                .sign()
                .complete()
            , transactionHash = await signedTx
                .submit()
        transactionHash ?
            startConnectButton!.innerText = startSuccessMessage
            :
            console.log('Error starting contract')
    }
    , instantiateStartContract = () => {
        startConnectButton?.addEventListener('click', async () => {
            startConnectButton!.innerText = startingMessage
            startContract()
        })
        startConnectButton?.removeEventListener('click', instantiateStartContract)
    },
    instantiateBuyContract = () => {
        buyConnectButton?.addEventListener('click', async () => {
            buyConnectButton!.innerText = buyingMessage
            buyContract()
        })
        buyConnectButton?.removeEventListener('click', instantiateBuyContract)
    },
    buyContract = async () => {
        const buyRedeemer = new Lucid.Construct(0, [])
            , serializedBuyRedeemer = Lucid.Data.to(buyRedeemer)
            , utxo = (await lucid.utxosAt(scriptAddress))
                .find(utxo => utxo.datumHash === datumHash && utxo.assets[currencySymbol + assetNameHex] !== undefined)
            , assetQuantity = utxo?.assets[currencySymbol + assetNameHex] as bigint
        if (utxo !== undefined) {
            const transaction =
                await lucid
                    .newTx()
                    .payToContract(scriptAddress
                        , datum
                        , {
                            lovelace: minLovelaceAmount,
                            [currencySymbol + assetNameHex]: (assetQuantity - BigInt(1))
                        })
                    .collectFrom([utxo], serializedBuyRedeemer)
                    .attachSpendingValidator(radSaleScript)
                    .addSigner(await lucid.wallet.address())
                    .payToAddress('addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy'
                        , { lovelace: priceOfTokenLovelace })
                    .payToAddress(await lucid.wallet.address(), {
                        lovelace: minLovelaceAmount
                        , [currencySymbol + assetNameHex]: BigInt(1)
                    })
                    .complete()
                , signedTx = await transaction
                    .sign()
                    .complete()
                , transactionHash = await signedTx
                    .submit()
            transactionHash ?
                buyConnectButton!.innerText = successMessage
                :
                console.log('Error buy token')
        }
        else
            console.log('utxo is undefined')
    }
    , instantiateCloseContract = () => {
        closeConnectButton?.addEventListener('click', async () => {
            closeConnectButton!.innerText = closingMessage
            closeContract()
        })
        closeConnectButton?.removeEventListener('click', instantiateCloseContract)
    },
    closeContract = async () => {
        const closeRedeemer = new Lucid.Construct(1, [])
            , serializedCloseRedeemer = Lucid.Data.to(closeRedeemer)
            , utxos = (await lucid.utxosAt(scriptAddress))
                .filter(utxo => utxo.datumHash === datumHash && utxo.assets[currencySymbol + assetNameHex] !== undefined)
            , assetQuantity = utxos.reduce((accumulator: bigint, utxo) => accumulator + (utxo?.assets[currencySymbol + assetNameHex] as bigint), BigInt(0))
            , transaction =
                await lucid
                    .newTx()
                    .payToAddress(await lucid.wallet.address()
                        , {
                            lovelace: minLovelaceAmount
                            , [currencySymbol + assetNameHex]: assetQuantity
                        })
                    .collectFrom(utxos, serializedCloseRedeemer)
                    .attachSpendingValidator(radSaleScript)
                    .addSigner(await lucid.wallet.address())
                    .complete()
            , signedTx = await transaction
                .sign()
                .complete()
            , transactionHash = await signedTx
                .submit()
        transactionHash ?
            closeConnectButton!.innerText = closeSuccessMessage
            :
            console.log('Error closing contract')
    }

startConnectButton!.innerText = addWalletMessage
buyConnectButton!.innerText = addWalletMessage
closeConnectButton!.innerText = addWalletMessage
const supportedWallet = Wallet.hasWallet()
if (supportedWallet !== undefined) {
    const wallet = await Wallet.getWalletApi(supportedWallet) as any
    lucid.selectWallet(wallet)
    startConnectButton!.innerText = connectMessage
    buyConnectButton!.innerText = connectMessage
    closeConnectButton!.innerText = connectMessage
    startConnectButton?.addEventListener('click', instantiateStartButton)
    startConnectButton?.addEventListener('click', instantiateStartContract)

    buyConnectButton?.addEventListener('click', instantiateBuyButton)
    buyConnectButton?.addEventListener('click', instantiateBuyContract)

    closeConnectButton?.addEventListener('click', instantiateCloseButton)
    closeConnectButton?.addEventListener('click', instantiateCloseContract)
}
else
    console.log('No supported wallet')

