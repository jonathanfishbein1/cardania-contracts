declare var window: any
import './style.css'
import * as Lucid from 'lucid-cardano'
import * as CMLB from 'lucid-cardano/custom_modules/cardano-multiplatform-lib-browser'
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
    , lucid = await Lucid.Lucid.new(
        new Lucid.Blockfrost('https://cardano-testnet.blockfrost.io/api/v0', bk), 'Testnet')
if (hasWallet('nami') == true) {
    const wallet = await getWalletApi('nami') as any
    lucid.selectWallet(wallet)
    connectButton!.innerText = connectMessage
    const rewardAddress = await lucid.wallet.rewardAddress()
        , utils = new Lucid.Utils(lucid)
    if (rewardAddress !== undefined) {
        const { address: { address } } = utils.getAddressDetails(rewardAddress)
        connectButton?.addEventListener('click', async () => {
            connectButton!.innerText = buyMessage
            connectButton?.addEventListener('click', async () => {
                connectButton!.innerText = buyingMessage
                const lovelaceAmount = BigInt(Number(10000000))
                const minLovelaceAmount = BigInt(Number(2000000))
                const redeemer = Lucid.Data.to(CMLB.PlutusData.new_constr_plutus_data(CMLB.ConstrPlutusData.new(BigNum.zero(), PlutusList.new())))
                console.log(redeemer.toString())
                const transaction =
                    await lucid
                        .newTx()
                        .attachSpendingValidator({
                            type: 'PlutusV1'
                            , script: '59116059115d01000033232323232323232323232323232332232323232222322323253353330073333573466e1cd55ce9baa0064800080648c98d4cd5ce00d80c80c00b9999ab9a3370ea0089001109100091999ab9a3370ea00a9000109100111931a99ab9c01c01a0190180173333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa004900011991091980080180118101aba15002301a357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a8101aba15006335015335502302175a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502675a6ae854008c09cd5d09aba2500223263533573805e05a05805626aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a04ceb4d5d0a80118139aba135744a004464c6a66ae700bc0b40b00ac4d55cf280089baa001357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854010cd4055d71aba15003335015335502375c40026ae854008c074d5d09aba2500223263533573804e04a04804626ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053018357426aae79400c8cccd5cd19b875002480108c848888c008014c068d5d09aab9e500423333573466e1d400d20022321222230010053016357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7008808007c07807407006c4d55cea80089baa001357426ae8940088c98d4cd5ce00d80c80c00b880c09931a99ab9c49010350543500018017135573ca00226ea80044d55ce9baa0011232230023758002640026aa028446666aae7c004940248cd4020c010d5d080118019aba200201323232323333573466e1cd55cea801a40004666444246660020080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80700a1aba135744a004464c6a66ae7006806005c0584d55cf280089baa00135742a006666aa00eeb94018d5d0a80119a8053ae357426ae8940088c98d4cd5ce00b00a00980909aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02444646666aae7c008940208cd401ccd54050c018d55cea80118029aab9e500230043574400602426ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263533573802402001e01c01a26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263533573801e01a01801626aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931a99ab9c00d00b00a0091375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931a99ab9c01000e00d00c00b00a135573aa00226ea80048c8cccd5cd19b8750014800884880088cccd5cd19b8750024800084880048c98d4cd5ce00600500480400389aab9d3754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931a99ab9c01401201101000f00e00d00c00b00a135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98d4cd5ce00680580500480409aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6a66ae7002802001c0180144d55cea80089baa0011122232323333573466e1cd55cea80124000466aa012600c6ae854008c014d5d09aba2500223263533573801401000e00c26aae7940044dd5000a4c240022244246600200600492103505431001123230010012233003300200200133232323232332233223232332232323232323232323233223232323232323232323232323232332232323232323232323232322222323235005533500413353353353353353253353332001501b3332001501032333553021120015009502535002222222222233355302b120012235002222350032233500225335333573466e3c04c0041181144cd40d801401c401c801d40bc024d401c8888004c8cd4090cd540e003ccd4090cd540e003c00540954094d401c88880100cc4d540d00cc4d540d5240116496e636f727265637420547820746f2073656c6c657200500123550350012355034222533500415335003153350021001103510351035235503500123353232335001235503800125335333553023120012253353301e3303133022300f002335503c302100c301100c48008c8c8d400888d400c88cccc0d001000c008004c8cc8848cc00400c008c0c40154cd400484d40dcc8488c00800cc0c8004540d540d0d400888800c4cd40a00080044005409cd400c88888888880244d540e12401124e6f206f757470757420746f206275796572002215335001135503903822135503c49119546f6f206d616e79206f75747075747320746f20627579657200533532333573466e240052000034035323333553024120013233502b22333502b0030010023502800133502a22230033002001200122337000029001000a40006018002266aa01caa06a66aa01c400264a66a002264c6a66ae712401035054380003b03a221002300c00113550364901094e6f207369676e6572005002235503600123550353002001235503500123355335301f004130314988854cd400454cd4cd540bcd40088880048cd540c0c8d400488888888894cd4ccd54c0c04800540d48d4004894cd4ccd5cd19b8f00200f0440431350400031503f002213503e35001220011503c301200723233550323333333574800446666ae68cdc39aab9d5002480008cccd55cf9aab9e5003235037041250360402503503e2503425034250342503403e23503503913754002426aa06e00226aa06e9201234572726f7220636f6e76657274696e672074784f7574446174756d20746f20756e69740022130354988d540d80048d540d4c0080048d540d40048cd54cd4ccc8005406cccd54c08848004d4055405c8ccc80054044c02c004ccc80054024c8c8cd4098cd540e8044cd4098cd540e8044005409d409cccc0800040400414010c8c8c8cd409ccd540ec00ccd409ccd540ec00800540a140a0cc0814018cd540e8c07c028c03c028c038024c074020c07c0100cc4d540d00cc4d540d52401134e6f206f757470757420746f207363726970740023550360012355035300200123357380020644a66a00220662c264646a0044444444444a66a666aa605a24002a0644a66a666ae68cdc780600081f81f09a81d8008a81d0019081f881e9a8039111000a80089a80111001099191a8011111111111199aa981489000a808a81699aa98090900091a80091000999aa981489000911a801111299a800909a8021119a80110041299a999ab9a3371e00202808a088266a06a66aa09200800c01020102008a05a0126a6a004446a0044444444444a66a66056014016426a002446a0024446a0064466a00446092931299a8021099aa82580100089824a4c260809311001180600089119aa98080900091a8009119aa81a00119aa98098900091a8009119aa81b801199a800919818a40000024466064004002466062002900000099805801000990009aa818910891299a8008a80f91099a810180200119aa98030900080200091a80091100111a800911111111100191a800911100108911801000a4410023500122002122333553014120013500c500a23500122333553017120013500f500d2350012233350012330254800000488cc0980080048cc09400520000013300300200122335530071200123500122335502b002333500123355300b1200123500122335502f00235500c0010012233355500802100200123355300b1200123500122335502f00235500b00100133355500301c002001111222333553011120015014335530071200123500122335502b002355008001333553011120012235002225335333553017120013500a500c235001223300a00200500610031335018004003501500133553007120012350012232335502c00330010053200135502e2253350011355009003221350022253353300c002008112223300200a00413006003002112122230030041121222300100412335014223335003220020020013500122001320013550242211225335001150122213350133004002335530061200100400111233001225335002101f100101c1123300100201c2253350021001101b1233500f22333500322002002001350012200112253350021001153350011019101a23500122220032235001223330050040020012223232300100532001355020223350014800088d4008894cd4ccd5cd19b8f00200901f01e13007001130060033200135501f223350014800088d4008894cd4ccd5cd19b8f00200701e01d1001130060032350012235002222222222253353300f00a00b21350012235001222333553014120012235002222350082235005225335333302600400300200113350200090081008501800f132635335738921024c66000270263200135501a2211222533500110022213300500233355300712001005004001223333500125010250102501023335530041200150092350012253355335333573466e3cd400888008d40108800806005c4ccd5cd19b87350022200135004220010180171017135014003150130033200135501822112225335001135008003221333500b005300400233355300712001005004001112200212212233001004003123500122001123500122002133500122533500221003100150071221233001003002222232335005233500425335333573466e3c0080040440405400c404080408cd4010804094cd4ccd5cd19b8f002001011010150031010153350032153350022133500223350022335002233500223300c00200120132335002201323300c00200122201322233500420132225335333573466e1c01800c05805454cd4ccd5cd19b8700500201601513300f00400110151015100e153350012100e100e2122300200322333573466e3c00800402802488ccd5cd19b8700200100900811225335002213002001150031212230020031122001223370000400246aa00a921144e6f20636f6e74696e75696e67206f757470757400122002122001112122300200311212230010032326353357389201024c67000040031122123300100300249848004448c8c00400488cc00cc008008004cd4488cccc0092080dac4094891c641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e0048811443617264616e6961466f756e64657257686974650048811ceefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd5200222212333300100500400300220011'
                        })
                        .collectFrom([{
                            txHash: 'f3d2f39db539ef45c3156242f41b00b6ba5a39e9918a3e50788c127cd2b113ee',
                            outputIndex: 1,
                            assets: {
                                '641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e43617264616e6961466f756e6465725768697465': BigInt(Number(1))
                            },
                            address: 'addr_test1wruumrldke6wh5rjfkyg9f7xztrh7nzvpzum36jqajnextshh0442',
                            datumHash: '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec'
                        }], redeemer)
                        .payToAddress('addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy'
                            , { lovelace: lovelaceAmount })
                        .payToContract('addr_test1wruumrldke6wh5rjfkyg9f7xztrh7nzvpzum36jqajnextshh0442'
                            , '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec'
                            , { lovelace: minLovelaceAmount })
                        .payToAddress(address, {
                            lovelace: minLovelaceAmount
                            , '641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e43617264616e6961466f756e6465725768697465': BigInt(Number(1))
                        })
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
}