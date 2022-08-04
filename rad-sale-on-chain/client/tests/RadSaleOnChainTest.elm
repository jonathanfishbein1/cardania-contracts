module RadSaleOnChainTest exposing (suite)

import Expect
import Library
import RadSaleOnChain
import Test
import Test.Html.Query
import Test.Html.Selector


suite : Test.Test
suite =
    Test.describe "rad-sale-on-chain Tests"
        [ Test.test "test Connect with NotConnectedNotAbleTo" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.NotConnectedNotAbleTo RadSaleOnChain.Development

                    ( newModel, _ ) =
                        RadSaleOnChain.update (RadSaleOnChain.Connect Library.Nami) initialModel
                in
                Expect.equal
                    newModel
                    (RadSaleOnChain.NotConnectedNotAbleTo RadSaleOnChain.Development)
        , Test.test "test Connect with NotConnectedAbleTo" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.NotConnectedAbleTo RadSaleOnChain.Development Library.Nami

                    ( newModel, _ ) =
                        RadSaleOnChain.update (RadSaleOnChain.Connect Library.Nami) initialModel
                in
                Expect.equal
                    newModel
                    (RadSaleOnChain.Connecting RadSaleOnChain.Development)
        , Test.test "test ReceiveWalletConnected with Connecting" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.Connecting RadSaleOnChain.Development

                    ( newModel, _ ) =
                        RadSaleOnChain.update (RadSaleOnChain.ReceiveWalletConnected (Maybe.Just Library.Nami)) initialModel
                in
                Expect.equal
                    newModel
                    (RadSaleOnChain.Connected RadSaleOnChain.Development Library.Nami RadSaleOnChain.NotStarted RadSaleOnChain.NotBought RadSaleOnChain.NotClosed)
        , Test.test "test ReceiveConnectionEstablished with ConnectionEstablished" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.ConnectionEstablished RadSaleOnChain.Development Library.Nami

                    ( newModel, _ ) =
                        RadSaleOnChain.update RadSaleOnChain.ReceiveConnectionEstablished initialModel
                in
                Expect.equal
                    newModel
                    (RadSaleOnChain.Connected RadSaleOnChain.Development Library.Nami RadSaleOnChain.NotStarted RadSaleOnChain.NotBought RadSaleOnChain.NotClosed)
        , Test.test "test NotConnectedNotAbleTo view" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.NotConnectedNotAbleTo RadSaleOnChain.Development
                in
                Test.Html.Query.fromHtml (RadSaleOnChain.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "startButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "No available wallet"
                            ]
                        ]
        , Test.test "test NotConnectedAbleTo view" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.NotConnectedAbleTo RadSaleOnChain.Development Library.Nami
                in
                Test.Html.Query.fromHtml (RadSaleOnChain.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "startButton" ]
                    |> Test.Html.Query.has [ Test.Html.Selector.text "Connect" ]
        , Test.test "test ConnectionEstablished view" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.ConnectionEstablished RadSaleOnChain.Development Library.Nami
                in
                Test.Html.Query.fromHtml (RadSaleOnChain.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "startButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connection established"
                            ]
                        ]
        , Test.test "test Connecting view" <|
            \_ ->
                let
                    initialModel : RadSaleOnChain.Model
                    initialModel =
                        RadSaleOnChain.Connecting RadSaleOnChain.Development
                in
                Test.Html.Query.fromHtml (RadSaleOnChain.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "startButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connecting"
                            ]
                        ]
        ]
