module DelgetionTest exposing (suite)

import Delegation
import Expect
import Library
import Test
import Test.Html.Query
import Test.Html.Selector


suite : Test.Test
suite =
    Test.describe "Delegation Tests"
        [ Test.test "test Connect with NotConnectedNotAbleTo" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.NotConnectedNotAbleTo

                    ( newModel, _ ) =
                        Delegation.update (Delegation.Connect "" Library.Nami) initialModel
                in
                Expect.equal
                    newModel
                    Delegation.NotConnectedNotAbleTo
        , Test.test "test Connect with NotConnectedAbleTo" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.NotConnectedAbleTo "" Library.Nami

                    ( newModel, _ ) =
                        Delegation.update (Delegation.Connect "" Library.Nami) initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Connecting "")
        , Test.test "test ReceiveWalletConnected with Connecting" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connecting ""

                    ( newModel, _ ) =
                        Delegation.update (Delegation.ReceiveWalletConnected (Maybe.Just Library.Nami)) initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.GettingAcountStatus "" Library.Nami)
        , Test.test "test GetAccountStatus with ConnectionEstablished" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.ConnectionEstablished "" Library.Nami

                    ( newModel, _ ) =
                        Delegation.update Delegation.GetAccountStatus initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.GettingAcountStatus "" Library.Nami)
        , Test.test "test ReceiveAccountStatus with GettingAcountStatus" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.GettingAcountStatus "" Library.Nami

                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    ( newModel, _ ) =
                        Delegation.update (Delegation.ReceiveAccountStatus (Result.Ok account)) initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Connected "" Library.Nami account Delegation.DelegatingToSumn)
        , Test.test "test RegisterAndDelegateToSumn with Connected" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connected "" Library.Nami account Delegation.NotDelegating

                    ( newModel, _ ) =
                        Delegation.update Delegation.RegisterAndDelegateToSumn initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.RegisteringAndDelegating "" Library.Nami account)
        , Test.test "test ReceiveRegisterAndDelegateStatus with Connected NotDelegating" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.RegisteringAndDelegating "" Library.Nami account

                    ( newModel, _ ) =
                        Delegation.update (Delegation.ReceiveRegisterAndDelegateStatus True) initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Connected "" Library.Nami account Delegation.DelegatingToSumn)
        , Test.test "test UndelegateFromSumn with Connected DelegatingToSumn" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connected "" Library.Nami account Delegation.DelegatingToSumn

                    ( newModel, _ ) =
                        Delegation.update Delegation.UndelegateFromSumn initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Undelegating "" Library.Nami account)
        , Test.test "test ReceiveUndelegateStatus with Undelegating" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Undelegating "" Library.Nami account

                    ( newModel, _ ) =
                        Delegation.update (Delegation.ReceiveUndelegateStatus True) initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Connected "" Library.Nami account Delegation.NotDelegating)
        , Test.test "test Delegate with Connected DelegatingToOther" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connected "" Library.Nami account Delegation.DelegatingToOther

                    ( newModel, _ ) =
                        Delegation.update Delegation.DelegateToSumn initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Delegating "" Library.Nami account)
        , Test.test "test ReceiveDelegateStatus with Connected DelegatingToOther" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Delegating "" Library.Nami account

                    ( newModel, _ ) =
                        Delegation.update (Delegation.ReceiveDelegateToSumnStatus True) initialModel
                in
                Expect.equal
                    newModel
                    (Delegation.Connected "" Library.Nami account Delegation.DelegatingToSumn)
        , Test.test "test NotConnectedNotAbleTo view" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.NotConnectedNotAbleTo
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
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
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.NotConnectedAbleTo "" Library.Nami
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has [ Test.Html.Selector.text "Connect" ]
        , Test.test "test ConnectionEstablished view" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.ConnectionEstablished "" Library.Nami
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
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
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.GettingAcountStatus "" Library.Nami
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
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
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connected "" Library.Nami account Delegation.NotDelegating
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.text "Register and Delegate"
                        ]
        , Test.test "test Connected DelegatingToOther view" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connected "" Library.Nami account Delegation.DelegatingToOther
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.text "Delegate"
                        ]
        , Test.test "test Connected DelegatingToSumn view" <|
            \_ ->
                let
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connected "" Library.Nami account Delegation.DelegatingToSumn
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.text "Undelegate"
                        ]
        , Test.test "test Connecting view" <|
            \_ ->
                let
                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Connecting ""
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
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
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.RegisteringAndDelegating "" Library.Nami account
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
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
                    account : Delegation.Account
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel : Delegation.Model
                    initialModel =
                        Delegation.Delegating "" Library.Nami account
                in
                Test.Html.Query.fromHtml (Delegation.view initialModel)
                    |> Test.Html.Query.find [ Test.Html.Selector.id "delegationButton" ]
                    |> Test.Html.Query.has
                        [ Test.Html.Selector.all
                            [ Test.Html.Selector.disabled True
                            , Test.Html.Selector.text "Delegating"
                            ]
                        ]
        ]
