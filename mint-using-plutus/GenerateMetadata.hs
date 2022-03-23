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
  = R Resource
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
    Data.Aeson.object ["d0b4c7811012fc5e9860c2fe374265f4e465ff99586ed7352fa9a866" Data.Aeson..= Data.Aeson.toJSON pol]

newtype Resource = Resource
  { name :: ResourceData
  }
  deriving (GHC.Generics.Generic)

instance Data.Aeson.ToJSON Resource where
  toJSON (Resource resourceData) =
    Data.Aeson.object [( Data.Text.pack ((name :: ResourceData -> Relude.String) resourceData)) Data.Aeson..= resourceData]

data ResourceData = ResourceData
  { image :: Relude.String,
    mediaType :: Relude.String,
    sha256 :: Relude.String,
    name :: Relude.String,
    description :: Relude.String
  }
  deriving (GHC.Generics.Generic)

instance Data.Aeson.ToJSON ResourceData

generateResourceMetaData :: Relude.String -> Relude.Integer -> ResourceData
generateResourceMetaData contents index =
  let sha = Data.Digest.Pure.SHA.sha256 (Data.ByteString.Lazy.UTF8.fromString contents)
   in ResourceData
        { image = "ipfs://QmddEBi17KzzzbUG9PYSb5yoaWps1KgcYaTjCjqenqP1UD",
          mediaType = "image/png",
          sha256 = Relude.show sha,
          name = "Space Bretzel #"
        }


main :: Relude.IO ()
main = Relude.return ()