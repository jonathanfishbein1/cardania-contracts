module DelgetionTest exposing (suite)

import Expect
import Main
import Test


suite : Test.Test
suite =
    Test.describe "Delegation Tests"
        [ Test.test "test Connect" <|
            \_ ->
                let
                    initialModel =
                        Main.init ( Main.encodeWallet Main.Nami, "" )
                in
                Expect.equal "" ""
        ]
