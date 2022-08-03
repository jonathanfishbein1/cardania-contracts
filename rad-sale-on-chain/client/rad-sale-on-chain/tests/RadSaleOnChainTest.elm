module RadSaleOnChainTest exposing (suite)

import Expect
import Main
import Test
import Test.Html.Query
import Test.Html.Selector


suite : Test.Test
suite =
    Test.describe "rad-sale-on-chain Tests"
        [ Test.test "test Connect with NotConnectedNotAbleTo" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedNotAbleTo Main.Development

                    ( newModel, _ ) =
                        Main.update (Main.Connect Main.Nami) initialModel
                in
                Expect.equal
                    newModel
                    (Main.NotConnectedNotAbleTo Main.Development)
        , Test.test "test Connect with NotConnectedAbleTo" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedAbleTo Main.Development Main.Nami False

                    ( newModel, _ ) =
                        Main.update (Main.Connect Main.Nami) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connecting Main.Development)
        , Test.test "test ReceiveWalletConnected with Connecting" <|
            \_ ->
                let
                    initialModel =
                        Main.Connecting Main.Development

                    ( newModel, _ ) =
                        Main.update (Main.ReceiveWalletConnected (Maybe.Just Main.Nami)) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connected Main.Development Main.Nami False Main.NotStarted Main.NotBought Main.NotClosed)
        , Test.test "test ReceiveConnectionEstablished with ConnectionEstablished" <|
            \_ ->
                let
                    initialModel =
                        Main.ConnectionEstablished Main.Development Main.Nami

                    ( newModel, _ ) =
                        Main.update Main.ReceiveConnectionEstablished initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connected Main.Development Main.Nami False Main.NotStarted Main.NotBought Main.NotClosed)
        , Test.test "test NotConnectedNotAbleTo view" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedNotAbleTo Main.Development
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
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
                    initialModel =
                        Main.NotConnectedAbleTo Main.Development Main.Nami False
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "startButton" ]
                    |> Test.Html.Query.has [ Test.Html.Selector.text "Connect" ]
        , Test.test "test ConnectionEstablished view" <|
            \_ ->
                let
                    initialModel =
                        Main.ConnectionEstablished Main.Development Main.Nami
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
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
                    initialModel =
                        Main.Connecting Main.Development
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "startButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connecting"
                            ]
                        ]
        ]
