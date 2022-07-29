module DelgetionTest exposing (suite)

import Expect
import Main
import Test


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
                        Main.NotConnectedAbleTo "" Main.Nami

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
                    (Main.Connected "" Main.Nami account Main.DelegatingToSumn)
        , Test.test "test RegisterAndDelegateToSumn with Connected" <|
            \_ ->
                let
                    account =
                        { stake_address = "", pool_id = "", active = True }

                    initialModel =
                        Main.Connected "" Main.Nami account Main.NotDelegating

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
                    (Main.Connected "" Main.Nami account Main.DelegatingToSumn)
        ]
