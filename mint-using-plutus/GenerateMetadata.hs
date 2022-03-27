module Main (main) where

import qualified Data.Aeson
import qualified Data.Aeson.TH
import qualified Data.ByteString.Lazy.UTF8
import qualified Data.Char
import qualified Data.Digest.Pure.SHA
import qualified Data.List
import qualified Data.List.Split
import qualified Data.Text
import qualified GHC.Generics
import qualified Relude
import qualified System.Directory
import qualified System.IO
import qualified Text.Regex.TDFA

data AssetType
  = R ResourceAsset
  deriving (GHC.Generics.Generic)

customOptions :: Data.Aeson.TH.Options
customOptions =
  Data.Aeson.defaultOptions
    { Data.Aeson.sumEncoding = Data.Aeson.UntaggedValue
    }

instance Data.Aeson.ToJSON AssetType where
  toJSON = Data.Aeson.genericToJSON customOptions

newtype MetaData = MetaData
  { label :: Policy
  }
  deriving (GHC.Generics.Generic)

instance Data.Aeson.ToJSON MetaData where
  toJSON (MetaData lab) =
    Data.Aeson.object ["721" Data.Aeson..= lab]

newtype Policy = Policy
  { policy :: AssetType
  }
  deriving (GHC.Generics.Generic)

instance Data.Aeson.ToJSON Policy where
  toJSON (Policy pol) =
    Data.Aeson.object ["650296faadaccf9bb9852678ce0e881c5c09f1a6da9cb7531b3c23bb" Data.Aeson..= Data.Aeson.toJSON pol]

newtype ResourceAsset = ResourceAsset
  { name :: ResourceData
  }
  deriving (GHC.Generics.Generic)

instance Data.Aeson.ToJSON ResourceAsset where
  toJSON (ResourceAsset resourceData) =
    Data.Aeson.object [Data.Text.pack ((name :: ResourceData -> Relude.String) resourceData) Data.Aeson..= resourceData]

data ResourceData = ResourceData
  { image :: Relude.String,
    mediaType :: Relude.String,
    sha256 :: Relude.String,
    name :: Relude.String,
    description :: Relude.String
  }
  deriving (GHC.Generics.Generic)

instance Data.Aeson.ToJSON ResourceData

generateResourceMetaData :: Relude.String -> Relude.String -> Relude.Text -> ResourceData
generateResourceMetaData filePath contents cid =
  let sha = Data.Digest.Pure.SHA.sha256 (Data.ByteString.Lazy.UTF8.fromString contents)
      characterRegex = "[^.]*" :: Relude.String
   in ResourceData
        { image = "ipfs://" Relude.++ Relude.toString cid,
          mediaType = "image/png",
          sha256 = Relude.show sha,
          name = filePath Text.Regex.TDFA.=~ characterRegex :: Relude.String,
          description = ""
        }

main :: Relude.IO ()
main = do
  imageDir <- System.Directory.listDirectory "/home/jonathan/Documents/cardania-contracts/mint-using-plutus/images/"
  resourceCidsHandle <- "/home/jonathan/Documents/cardania-contracts/mint-using-plutus/CIDs.txt" `System.IO.openFile` System.IO.ReadMode
  resourceCidsContents <- System.IO.hGetContents resourceCidsHandle

  let resourceCids = Relude.lines (Relude.toText resourceCidsContents)
  Relude.zipWithM_
    ( \filePath cid ->
        do
          handle <- ("/home/jonathan/Documents/cardania-contracts/mint-using-plutus/images/" Relude.++ filePath) `System.IO.openBinaryFile` System.IO.ReadMode
          contents <- System.IO.hGetContents handle
          let resourceAssetData =
                generateResourceMetaData filePath contents cid
              resourceAsset = ResourceAsset {name = resourceAssetData}
          let resourceMetaData =
                MetaData
                  { label = Policy {policy = R resourceAsset}
                  }

          Data.Aeson.encodeFile
            ( "/home/jonathan/Documents/cardania-contracts/mint-using-plutus/metadata/"
                Relude.++ (name :: ResourceData -> Relude.String)
                  resourceAssetData
                Relude.++ ".json"
            )
            resourceMetaData
    )
    imageDir
    resourceCids