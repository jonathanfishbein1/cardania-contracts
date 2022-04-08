{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import qualified Data.Proxy
import qualified Options.Applicative
import qualified Plutus.PAB.Run.PSGenerator
import qualified RadSaleContracts
import qualified Prelude

parseOptions :: Prelude.IO Prelude.FilePath
parseOptions =
  Options.Applicative.customExecParser
    ( Options.Applicative.prefs Prelude.$
        Options.Applicative.disambiguate Prelude.<> Options.Applicative.showHelpOnEmpty
          Prelude.<> Options.Applicative.showHelpOnError
    )
    (Options.Applicative.info (Options.Applicative.helper Prelude.<*> psGenOutputDirParser) Options.Applicative.idm)

psGenOutputDirParser :: Options.Applicative.Parser Prelude.FilePath
psGenOutputDirParser =
  Options.Applicative.option
    Options.Applicative.str
    ( Options.Applicative.long "output-dir"
        Prelude.<> Options.Applicative.metavar "OUTPUT_DIR"
        Prelude.<> Options.Applicative.help "Output directory to write PureScript files to."
    )

main :: Prelude.IO ()
main = do
  psGenOutputDir <- parseOptions
  Plutus.PAB.Run.PSGenerator.generateAPIModule (Data.Proxy.Proxy :: Data.Proxy.Proxy RadSaleContracts.RadSaleContracts) psGenOutputDir
