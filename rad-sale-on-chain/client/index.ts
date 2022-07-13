declare var window: any
import './style.css'
import * as Lucid from 'lucid-cardano'

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
    startConnectButton = document.getElementById('startConnect'),
    bk = "testnetwIyK8IphOti170JCngH0NedP0yK8wBZs",
    connectMessage = "connect wallet",
    startMessage = "Start",
    buyMessage = "Buy",
    startingMessage = "Starting...",
    buyingMessage = "Buying...",
    unbuyingMessage = "Unbuying..."
    , successMessage = "Successfully buyd to SUMN!"
    , lucid = await Lucid.Lucid.new(
        new Lucid.Blockfrost('https://cardano-testnet.blockfrost.io/api/v0', bk), 'Testnet')
if (hasWallet('nami') == true) {
    const wallet = await getWalletApi('nami') as any
    lucid.selectWallet(wallet)
    startConnectButton!.innerText = connectMessage
    connectButton!.innerText = connectMessage
    const utils = new Lucid.Utils(lucid)
    const paymentAddressDetails = utils.getAddressDetails(await lucid.wallet.address())
    startConnectButton?.addEventListener('click', async () => {
        startConnectButton!.innerText = startMessage
        startConnectButton?.addEventListener('click', async () => {
            startConnectButton!.innerText = startingMessage
            const radSaleScript: Lucid.Script = {
                type: 'PlutusV1'
                , script: '5910d35910d001000033232323232323232323232323232332232323232222322323253353330073333573466e1cd55ce9baa0064800080648c98d4cd5ce00d80c80c00b9999ab9a3370ea0089001109100091999ab9a3370ea00a9000109100111931a99ab9c01c01a0190180173333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa004900011991091980080180118101aba15002301a357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a8101aba15006335015335502302175a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502675a6ae854008c09cd5d09aba2500223263533573805e05a05805626aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a04ceb4d5d0a80118139aba135744a004464c6a66ae700bc0b40b00ac4d55cf280089baa001357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854010cd4055d71aba15003335015335502375c40026ae854008c074d5d09aba2500223263533573804e04a04804626ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053018357426aae79400c8cccd5cd19b875002480108c848888c008014c068d5d09aab9e500423333573466e1d400d20022321222230010053016357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7008808007c07807407006c4d55cea80089baa001357426ae8940088c98d4cd5ce00d80c80c00b880c09931a99ab9c49010350543500018017135573ca00226ea80044d55ce9baa0011232230023758002640026aa028446666aae7c004940248cd4020c010d5d080118019aba200201323232323333573466e1cd55cea801a40004666444246660020080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80700a1aba135744a004464c6a66ae7006806005c0584d55cf280089baa00135742a006666aa00eeb94018d5d0a80119a8053ae357426ae8940088c98d4cd5ce00b00a00980909aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02444646666aae7c008940208cd401ccd54050c018d55cea80118029aab9e500230043574400602426ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263533573802402001e01c01a26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263533573801e01a01801626aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931a99ab9c00d00b00a0091375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931a99ab9c01000e00d00c00b00a135573aa00226ea80048c8cccd5cd19b8750014800884880088cccd5cd19b8750024800084880048c98d4cd5ce00600500480400389aab9d3754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931a99ab9c01401201101000f00e00d00c00b00a135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98d4cd5ce00680580500480409aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6a66ae7002802001c0180144d55cea80089baa0011122232323333573466e1cd55cea80124000466aa012600c6ae854008c014d5d09aba2500223263533573801401000e00c26aae7940044dd5000a4c2400222442466002006004921035054310011232300100122330033002002001332323232323233223232332233223232332232323232323232323233223232323232323232323232222232323500553350041335335335335335325335325335001102415335024102310243332001500f3300800135007222200132335025335502d027335025335502d02700150265026350072222004135501e023135501f490116496e636f727265637420547820746f2073656c6c6572005001235501f001235501e222533500415335003153350011002102510251025235501f0012335323355501e533535001222222222200313550204901094e6f207369676e6572002215335001135502100222135502449110546f6f206d616e79207369676e65727300253353301a32323500122333027004002001335502f301f00a300e00a3300a002001480084d540800944d54085240115496e636f727265637420547820746f2062757965720050022355020001235501f3002001235501f00123353355501d5335350042235002222222222253353301900a00b2135001223500122233355302412001223500222235008223500522325335335005233500425335333573466e3c0080041041005400c410081008cd4010810094cd4ccd5cd19b8f002001041040150031040133504000a0091009153350032153350022133500223350022335002233500223303800200120432335002204323303800200122204322233500420432225335333573466e1c01800c11811454cd4ccd5cd19b8700500204604513303b00400110451045103e153350012103e103e503700f132635335738921024c660003a039135501f491144e6f20636f6e74696e75696e67206f757470757400221533500113550200022213550234911a546f6f206d616e7920636f6e74696e75696e67206f757470757400253353355028350012220012335502932350012222222222533533355302312001501b235001225335333573466e3c00803c0cc0c84d40e400c540e000884d40dcd400488004540d4c0380188c8ccccccd5d200111999ab9a3370e6aae7540092000233335573e6aae79400c8d40c00d4940bc0d0940b80c8940b4940b4940b4940b40c84dd5000909aa81001289aa8102491e4572726f7220646573657269616c697a696e672074784f7574446174756d002355020001235501f3002001235501f00123353355501d533535004223500222222222223301900a00b2135501f32350012220023500122001135501f4911343616e742066696e64206f776e20696e70757400232323350022355023001200253353301a3330235005301e009300d00932337020029001199811801180f004980680489aa81001289aa810a4812257726f6e67204e617469766520746f6b656e206f7574707574207175616e7469747900533533320015010302100130215004135501f02413550204911d57726f6e672061646120736372697074206f75747075742076616c7565002355020001235501f300200123357380020444a66a00220462c264646a0044444444444a66a666aa604224002a0324a66a666ae68cdc780600081781709a81a8008a81a0019081788169a8039111000a80089a80111001099191a8011111111111199aa980d89000a809281719aa98098900091a80091000999aa980d89000911a801111299a800909a8021119a80110041299a999ab9a3371e00202806a068266a06c66aa07c00800c01020102008a05c0126a6a004446a0044444444444a66a6602e014016426a002446a0024446a0064466a0044607c931299a8021099aa8200010008981f24c2606a9311001180480091199aa980609000a801a80f9a8011111111111199aa980b09000911a8011111a8019119a8011299a999ab9a3371e02600205e05c266a06000a00e200e400ea05201224466aa601c2400246a0024466aa05000466aa60222400246a0024466aa056004666a00246605490000009119815801000919815000a400000266012004002640026aa04a442244a66a0022a03e44266a040600800466aa600c2400200800246a002444400446a00244004446666a0024a03c4a03c4a03c4666aa601824002a00846a00244a66aa66a666ae68cdc79a801110011a8021100100e00d8999ab9a3370e6a004440026a00844002038036203626a0440062a042006266a01a44a66a004420062002a034244666aa6012240026a01aa01846a00244666aa6018240026a020a01e46a00244666a00246602490000009119809801000919809000a4000002660060040024466aa600e2400246a0024466aa042004666a002466aa60162400246a0024466aa04a0046aa01a00200244666aaa01001c004002466aa60162400246a0024466aa04a0046aa018002002666aaa006012004002222444666aa600824002a02c66aa600e2400246a0024466aa0420046aa012002666aa600824002446a00444a66a666aa6018240026466a02444666a006440040040026a00244002246600244a66a0042038200203246a002446601400400a00c2006266a034008006a02e00266aa600e2400246a002446466aa044006600200a640026aa04844a66a00226aa0140064426a00444a66a6601800401022444660040140082600c006004640026aa03a4422444a66a00220044426600a004666aa600e2400200a0080022242444600600822424446002008640026aa034442244a66a0022a02844266a02a600800466aa600c24002008002640026aa0324422444a66a00226a00644002442666a00a440046008004666aa600e2400200a00800244666ae68cdc780100080700691199ab9a3370e00400201a0182246600244a66a004200220180162466a00444666a006440040040026a00244002244246600200600446a00244440062224466a00446aa00a0024600400222424460040062242446002006466a00a66aa01a00e66a00a66aa01a00e66600400200e00ea00ca00c4446464600200a640026aa0244466a0029000111a80111299a999ab9a3371e0040120180162600e0022600c006640026aa0224466a0029000111a80111299a999ab9a3371e00400e01601420022600c00624400424400222440042442446600200800691100112253350022130020011500312122300200311220012233700004002464c6a66ae71241024c67000040031122123300100300249848004448c8c00400488cc00cc008008004cd4488cccc0092080dac4094891cfda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db5000488111434c415353494342414259424c554530310048811ceefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd5200222212333300100500400300220011'
            }
            console.log(await lucid.wallet.address())
            const scriptAddress = lucid.utils.validatorToAddress(radSaleScript)
            console.log(scriptAddress)

            const transaction =
                await lucid
                    .newTx()
                    .payToContract(scriptAddress
                        , Lucid.Data.empty()
                        , {
                            lovelace: BigInt(Number(3000000)),
                            'fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50434c415353494342414259424c55453031': BigInt(Number(5))
                        })
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

    connectButton?.addEventListener('click', async () => {
        startConnectButton!.innerText = startMessage
        connectButton!.innerText = buyMessage
        connectButton?.addEventListener('click', async () => {
            connectButton!.innerText = buyingMessage
            startConnectButton!.innerText = startingMessage
            const lovelaceAmount = BigInt(Number(10000000))
            const minLovelaceAmount = BigInt(Number(2000000))
            const redeemer = new Lucid.Construct(0, [])
            const serializedRedeemer = Lucid.Data.to(redeemer)
            const radSaleScript: Lucid.Script = {
                type: 'PlutusV1'
                , script: '5910d35910d001000033232323232323232323232323232332232323232222322323253353330073333573466e1cd55ce9baa0064800080648c98d4cd5ce00d80c80c00b9999ab9a3370ea0089001109100091999ab9a3370ea00a9000109100111931a99ab9c01c01a0190180173333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa004900011991091980080180118101aba15002301a357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a8101aba15006335015335502302175a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502675a6ae854008c09cd5d09aba2500223263533573805e05a05805626aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a04ceb4d5d0a80118139aba135744a004464c6a66ae700bc0b40b00ac4d55cf280089baa001357426ae8940088c98d4cd5ce01581481401389aab9e5001137540026ae854010cd4055d71aba15003335015335502375c40026ae854008c074d5d09aba2500223263533573804e04a04804626ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053018357426aae79400c8cccd5cd19b875002480108c848888c008014c068d5d09aab9e500423333573466e1d400d20022321222230010053016357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7008808007c07807407006c4d55cea80089baa001357426ae8940088c98d4cd5ce00d80c80c00b880c09931a99ab9c49010350543500018017135573ca00226ea80044d55ce9baa0011232230023758002640026aa028446666aae7c004940248cd4020c010d5d080118019aba200201323232323333573466e1cd55cea801a40004666444246660020080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80700a1aba135744a004464c6a66ae7006806005c0584d55cf280089baa00135742a006666aa00eeb94018d5d0a80119a8053ae357426ae8940088c98d4cd5ce00b00a00980909aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02444646666aae7c008940208cd401ccd54050c018d55cea80118029aab9e500230043574400602426ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263533573802402001e01c01a26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263533573801e01a01801626aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931a99ab9c00d00b00a0091375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931a99ab9c01000e00d00c00b00a135573aa00226ea80048c8cccd5cd19b8750014800884880088cccd5cd19b8750024800084880048c98d4cd5ce00600500480400389aab9d3754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931a99ab9c01401201101000f00e00d00c00b00a135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98d4cd5ce00680580500480409aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6a66ae7002802001c0180144d55cea80089baa0011122232323333573466e1cd55cea80124000466aa012600c6ae854008c014d5d09aba2500223263533573801401000e00c26aae7940044dd5000a4c2400222442466002006004921035054310011232300100122330033002002001332323232323233223232332233223232332232323232323232323233223232323232323232323232222232323500553350041335335335335335325335325335001102415335024102310243332001500f3300800135007222200132335025335502d027335025335502d02700150265026350072222004135501e023135501f490116496e636f727265637420547820746f2073656c6c6572005001235501f001235501e222533500415335003153350011002102510251025235501f0012335323355501e533535001222222222200313550204901094e6f207369676e6572002215335001135502100222135502449110546f6f206d616e79207369676e65727300253353301a32323500122333027004002001335502f301f00a300e00a3300a002001480084d540800944d54085240115496e636f727265637420547820746f2062757965720050022355020001235501f3002001235501f00123353355501d5335350042235002222222222253353301900a00b2135001223500122233355302412001223500222235008223500522325335335005233500425335333573466e3c0080041041005400c410081008cd4010810094cd4ccd5cd19b8f002001041040150031040133504000a0091009153350032153350022133500223350022335002233500223303800200120432335002204323303800200122204322233500420432225335333573466e1c01800c11811454cd4ccd5cd19b8700500204604513303b00400110451045103e153350012103e103e503700f132635335738921024c660003a039135501f491144e6f20636f6e74696e75696e67206f757470757400221533500113550200022213550234911a546f6f206d616e7920636f6e74696e75696e67206f757470757400253353355028350012220012335502932350012222222222533533355302312001501b235001225335333573466e3c00803c0cc0c84d40e400c540e000884d40dcd400488004540d4c0380188c8ccccccd5d200111999ab9a3370e6aae7540092000233335573e6aae79400c8d40c00d4940bc0d0940b80c8940b4940b4940b4940b40c84dd5000909aa81001289aa8102491e4572726f7220646573657269616c697a696e672074784f7574446174756d002355020001235501f3002001235501f00123353355501d533535004223500222222222223301900a00b2135501f32350012220023500122001135501f4911343616e742066696e64206f776e20696e70757400232323350022355023001200253353301a3330235005301e009300d00932337020029001199811801180f004980680489aa81001289aa810a4812257726f6e67204e617469766520746f6b656e206f7574707574207175616e7469747900533533320015010302100130215004135501f02413550204911d57726f6e672061646120736372697074206f75747075742076616c7565002355020001235501f300200123357380020444a66a00220462c264646a0044444444444a66a666aa604224002a0324a66a666ae68cdc780600081781709a81a8008a81a0019081788169a8039111000a80089a80111001099191a8011111111111199aa980d89000a809281719aa98098900091a80091000999aa980d89000911a801111299a800909a8021119a80110041299a999ab9a3371e00202806a068266a06c66aa07c00800c01020102008a05c0126a6a004446a0044444444444a66a6602e014016426a002446a0024446a0064466a0044607c931299a8021099aa8200010008981f24c2606a9311001180480091199aa980609000a801a80f9a8011111111111199aa980b09000911a8011111a8019119a8011299a999ab9a3371e02600205e05c266a06000a00e200e400ea05201224466aa601c2400246a0024466aa05000466aa60222400246a0024466aa056004666a00246605490000009119815801000919815000a400000266012004002640026aa04a442244a66a0022a03e44266a040600800466aa600c2400200800246a002444400446a00244004446666a0024a03c4a03c4a03c4666aa601824002a00846a00244a66aa66a666ae68cdc79a801110011a8021100100e00d8999ab9a3370e6a004440026a00844002038036203626a0440062a042006266a01a44a66a004420062002a034244666aa6012240026a01aa01846a00244666aa6018240026a020a01e46a00244666a00246602490000009119809801000919809000a4000002660060040024466aa600e2400246a0024466aa042004666a002466aa60162400246a0024466aa04a0046aa01a00200244666aaa01001c004002466aa60162400246a0024466aa04a0046aa018002002666aaa006012004002222444666aa600824002a02c66aa600e2400246a0024466aa0420046aa012002666aa600824002446a00444a66a666aa6018240026466a02444666a006440040040026a00244002246600244a66a0042038200203246a002446601400400a00c2006266a034008006a02e00266aa600e2400246a002446466aa044006600200a640026aa04844a66a00226aa0140064426a00444a66a6601800401022444660040140082600c006004640026aa03a4422444a66a00220044426600a004666aa600e2400200a0080022242444600600822424446002008640026aa034442244a66a0022a02844266a02a600800466aa600c24002008002640026aa0324422444a66a00226a00644002442666a00a440046008004666aa600e2400200a00800244666ae68cdc780100080700691199ab9a3370e00400201a0182246600244a66a004200220180162466a00444666a006440040040026a00244002244246600200600446a00244440062224466a00446aa00a0024600400222424460040062242446002006466a00a66aa01a00e66a00a66aa01a00e66600400200e00ea00ca00c4446464600200a640026aa0244466a0029000111a80111299a999ab9a3371e0040120180162600e0022600c006640026aa0224466a0029000111a80111299a999ab9a3371e00400e01601420022600c00624400424400222440042442446600200800691100112253350022130020011500312122300200311220012233700004002464c6a66ae71241024c67000040031122123300100300249848004448c8c00400488cc00cc008008004cd4488cccc0092080dac4094891cfda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db5000488111434c415353494342414259424c554530310048811ceefb5b9dbac4a380296de0655f6ace6c97e9b981eef89a7bf53dcd5200222212333300100500400300220011'
            }
            console.log(await lucid.wallet.address())
            console.log(paymentAddressDetails)
            const scriptAddress = lucid.utils.validatorToAddress(radSaleScript)
            console.log(scriptAddress)
            const utxo = (await lucid.utxosAt(scriptAddress))[0]
            console.log(utxo)

            const transaction =
                await lucid
                    .newTx()
                    .collectFrom([utxo], serializedRedeemer)
                    .addSigner(await lucid.wallet.address())
                    .payToAddress('addr_test1vrh0kkuahtz28qpfdhsx2hm2eekf06des8h03xnm757u65sd6egwy'
                        , { lovelace: lovelaceAmount })
                    .payToContract(scriptAddress
                        , Lucid.Data.empty()
                        , {
                            lovelace: BigInt(Number(3000000)),
                            'fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50434c415353494342414259424c55453031': BigInt(Number(4))
                        })
                    .attachSpendingValidator(radSaleScript)
                    .payToAddress(await lucid.wallet.address(), {
                        lovelace: minLovelaceAmount
                        , 'fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50434c415353494342414259424c55453031': BigInt(Number(1))
                    })

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
