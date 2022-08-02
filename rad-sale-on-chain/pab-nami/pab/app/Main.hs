{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import qualified Plutus.PAB.Effects.Contract.Builtin
import qualified Plutus.PAB.Run
import qualified RadSaleContracts
import qualified Prelude

main :: Prelude.IO ()
main = do
  Plutus.PAB.Run.runWith (Plutus.PAB.Effects.Contract.Builtin.handleBuiltin @RadSaleContracts.RadSaleContracts)

-- Keep this here for now. Eventually, This function will call the `migrate`
-- command before running the webserver.
--
-- let opts = AppOpts{minLogLevel = Nothing, logConfigPath = Nothing, configPath = Nothing, runEkgServer = False, storageBackend = BeamSqliteBackend, cmd = PABWebserver, PAB.Command.passphrase = Nothing}
--     networkID = NetworkIdWrapper $ CAPI.Testnet $ CAPI.NetworkMagic 1097911063
--     config = PAB.Config.defaultConfig
--         { nodeServerConfig = def{mscNodeMode=AlonzoNode,mscNetworkId=networkID} -- def{mscSocketPath=nodeSocketFile socketPath,mscNodeMode=AlonzoNode,mscNetworkId=networkID}
--         , dbConfig = def{dbConfigFile = "plutus-pab.db"} -- def{dbConfigFile = T.pack (dir </> "plutus-pab.db")}
--         , chainIndexConfig = def -- def{PAB.CI.ciBaseUrl = PAB.CI.ChainIndexUrl $ BaseUrl Http "localhost" chainIndexPort ""}
--         , walletServerConfig = def -- def{Wallet.Config.baseUrl=WalletUrl walletUrl}
--         }

-- void . async $ runWithOpts @DemoContract handleBuiltin (Just config) opts{cmd=Migrate}
-- sleep 2
-- void . async $ runWithOpts @DemoContract handleBuiltin (Just config) opts{cmd=PABWebserver}

-- -- Pressing enter stops the server
-- void getLine

-- sleep :: Int -> IO ()
-- sleep n = threadDelay $ n * 1_000_000
