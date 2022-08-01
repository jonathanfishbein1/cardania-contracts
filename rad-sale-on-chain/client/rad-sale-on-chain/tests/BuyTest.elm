module BuyTest exposing (suite)

import Expect
import Main
import Test
import Test.Html.Query
import Test.Html.Selector


suite : Test.Test
suite =
    Test.describe "Delegation Tests"
        [ Test.test "test Connect with NotConnectedNotAbleTo" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedNotAbleTo

                    ( newModel, _ ) =
                        Main.update (Main.Connect Main.Nami) initialModel
                in
                Expect.equal
                    newModel
                    Main.NotConnectedNotAbleTo
        , Test.test "test Connect with NotConnectedAbleTo" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedAbleTo Main.Nami False

                    ( newModel, _ ) =
                        Main.update (Main.Connect Main.Nami) initialModel
                in
                Expect.equal
                    newModel
                    Main.Connecting
        , Test.test "test NotConnectedNotAbleTo view" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedNotAbleTo
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
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
                        Main.NotConnectedAbleTo Main.Nami False
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has [ Test.Html.Selector.text "Connect" ]
        , Test.test "test ConnectionEstablished view" <|
            \_ ->
                let
                    initialModel =
                        Main.ConnectionEstablished Main.Nami
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connection established"
                            ]
                        ]
        , Test.test "test Connecting view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connecting
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connecting"
                            ]
                        ]
        ]
