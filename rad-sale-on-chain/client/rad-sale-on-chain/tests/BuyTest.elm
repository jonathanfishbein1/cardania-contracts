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
                        Main.update (Main.Connect "" Main.Nami) initialModel
                in
                Expect.equal
                    newModel
                    Main.NotConnectedNotAbleTo
        , Test.test "test Connect with NotConnectedAbleTo" <|
            \_ ->
                let
                    initialModel =
                        Main.NotConnectedAbleTo "" Main.Nami False

                    ( newModel, _ ) =
                        Main.update (Main.Connect "" Main.Nami) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connecting "")
        , Test.test "test ReceiveWalletConnected with Connecting" <|
            \_ ->
                let
                    initialModel =
                        Main.Connecting ""

                    ( newModel, _ ) =
                        Main.update (Main.ReceiveWalletConnected (Maybe.Just Main.Nami)) initialModel
                in
                Expect.equal
                    newModel
                    (Main.GettingAcountStatus "" Main.Nami)
        , Test.test "test GetAccountStatus with ConnectionEstablished" <|
            \_ ->
                let
                    initialModel =
                        Main.ConnectionEstablished "" Main.Nami

                    ( newModel, _ ) =
                        Main.update Main.GetAccountStatus initialModel
                in
                Expect.equal
                    newModel
                    (Main.GettingAcountStatus "" Main.Nami)
        , Test.test "test ReceiveAccountStatus with GettingAcountStatus" <|
            \_ ->
                let
                    initialModel =
                        Main.GettingAcountStatus "" Main.Nami

                    account =
                        { stake_address = "", pool_id = "", active = True }

                    ( newModel, _ ) =
                        Main.update (Main.ReceiveAccountStatus (Result.Ok account)) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connected "" Main.Nami account Main.DelegatingToSumn False)
        , Test.test "test RegisterAndDelegateToSumn with Connected" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.NotDelegating False

                    ( newModel, _ ) =
                        Main.update (Main.RegisterAndDelegateToSumn account) initialModel
                in
                Expect.equal
                    newModel
                    (Main.RegisteringAndDelegating "" Main.Nami account)
        , Test.test "test ReceiveRegisterAndDelegateStatus with Connected NotDelegating" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.RegisteringAndDelegating "" Main.Nami account

                    ( newModel, _ ) =
                        Main.update (Main.ReceiveRegisterAndDelegateStatus True) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connected "" Main.Nami account Main.DelegatingToSumn False)
        , Test.test "test UndelegateFromSumn with Connected DelegatingToSumn" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.DelegatingToSumn False

                    ( newModel, _ ) =
                        Main.update Main.UndelegateFromSumn initialModel
                in
                Expect.equal
                    newModel
                    (Main.Undelegating "" Main.Nami account)
        , Test.test "test ReceiveUndelegateStatus with Undelegating" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Undelegating "" Main.Nami account

                    ( newModel, _ ) =
                        Main.update (Main.ReceiveUndelegateStatus True) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connected "" Main.Nami account Main.NotDelegating False)
        , Test.test "test Delegate with Connected DelegatingToOther" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.DelegatingToOther False

                    ( newModel, _ ) =
                        Main.update Main.DelegateToSumn initialModel
                in
                Expect.equal
                    newModel
                    (Main.Delegating "" Main.Nami account)
        , Test.test "test ReceiveDelegateStatus with Connected DelegatingToOther" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Delegating "" Main.Nami account

                    ( newModel, _ ) =
                        Main.update (Main.ReceiveDelegateToSumnStatus True) initialModel
                in
                Expect.equal
                    newModel
                    (Main.Connected "" Main.Nami account Main.DelegatingToSumn False)
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
                        Main.NotConnectedAbleTo "" Main.Nami False
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has [ Test.Html.Selector.text "Connect" ]
        , Test.test "test ConnectionEstablished view" <|
            \_ ->
                let
                    initialModel =
                        Main.ConnectionEstablished "" Main.Nami
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connection established"
                            ]
                        ]
        , Test.test "test GettingAcountStatus view" <|
            \_ ->
                let
                    initialModel =
                        Main.GettingAcountStatus "" Main.Nami
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Getting account status"
                            ]
                        ]
        , Test.test "test Connected NotDelegating view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.NotDelegating False
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.text "Register and Delegate"
                        ]
        , Test.test "test Connected DelegatingToOther view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.DelegatingToOther False
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.text "Delegate"
                        ]
        , Test.test "test Connected DelegatingToSumn view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.DelegatingToSumn False
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.text "Undelegate"
                        ]
        , Test.test "test Connecting view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connecting ""
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Connecting"
                            ]
                        ]
        , Test.test "test RegisteringAndDelegating view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.RegisteringAndDelegating "" Main.Nami account
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Registering and Delegating"
                            ]
                        ]
        , Test.test "test Delegating view" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Delegating "" Main.Nami account
                in
                Test.Html.Query.fromHtml (Main.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Delegating"
                            ]
                        ]
        ]
