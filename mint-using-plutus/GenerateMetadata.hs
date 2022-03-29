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

descriptions :: [Relude.String]
descriptions =
  [ "A rare and incredibly useful arrangement of particles which defy the laws of traditional Newtonian physics. Most commonly utilized in modern spacecraft propulsion.",
    "The calcified remains of organic life. Useful in a variety of tradecraft. Often considered spooky.",
    "One of the most highly prized minerals in the galaxy. Cardanium is only found in relative proximity to unstable interdimensional portals. It is valued for its unparalleled beauty, rarity, and an impossibly dense crystalline atomic structure which makes it useful in a variety of civil, military, and scientific applications.",
    "Delicious and nutritious! Stay palm.",
    "A versatile and highly sought after metal. Particularly valued for its conductive properties.",
    "A miserable infusion of cursed nanites has rendered this liquid unsuitable for consumption. However, it does retain utility in military applications and in the darker corners of the scientific community…",
    "Liquid water infused with a swarm of Cardania’s most helpful nanites. Commonly used in advanced medical applications. The liquid emits a faint melodious tune.",
    "Densely grown wood from an evergreen subspecies. Useful in a variety of tradecraft.",
    "A standardized measure of stored energy. Can be generated via a number of processes and utilized in a number of applications",
    "A measure of the overall confidence or trust inspired by a leader, faction, or ideology.",
    "The foundation of most organic life within the galaxy, particularly within the Sol system. This resource is useful for most life forms to maintain their living status. ",
    "The Fungal species native to Cardania regularly emit reproductive spores in an ongoing effort to colonize the furthest reaches of the station. Spores have known psychedelic and psi enhancing properties, making them popular with many of Cardania’s humanoid denizens.",
    "A curiously reactive liquid that vibrationally attunes itself to the loudest nearby sound. This resource has become popular for a variety of military, scientific, and entertainment purposes.",
    "The soft, fluffy exterior of some galactic critter or another. Useful for basic textile and other applications.",
    "A variety of rare and beautiful gemstones. Commonly traded amongst organisms which utilize them for their aesthetic, industrial, and even spiritual value.",
    "Fresh leafy greens provide great nutritional value for a variety of organic species.",
    "The frozen, solid form of water. Useful for cooling things down!",
    "A highly valued and useful metal, prized for its durability as well as its magnetic properties.",
    "The tanned hide of an organic creature. Useful in a variety of tradecraft.",
    "Simple wood, harvested from a tree. Useful in a variety of tradecraft.",
    "A mishmash of electronics, machinery, metal, and wires. Typically harvested from the corpses of inorganic lifeforms, or mined from millennia old piles of scrap. A very useful resource for those who know how to process and utilize such components.",
    "Molten rock and mineral, superheated by volcanic activity or powerful lasers.",
    "The edible remains of an organic lifeform. Quite nutritious.",
    "The valuable mineral remnants of space rock.",
    "The fresh fruit and leaves of the wondrous Prismaleaf tree. Prismaleaf is valued for its incredible medical utility, and useful in a variety of restorative medicines and other tradecraft.",
    "Motes of multi-colored particles with trace elements indicating origin from an unstable interdimensional portal.",
    "Incredibly powerful and often dangerous energy harvested from near the Summoning Pool, an ancient technological wonder that powers the Cardania megastructure.",
    "A highly valuable and extremely radioactive alkaline earth mineral.",
    "An aggregate score approximating your ability to conduct investigation into scientific and magical phenomena, and apply findings to the material plane.",
    "These ancient carved runestones emit a faint blue light and are cold to the touch.",
    "These ancient carved runestones emit a faint green light and soft melodious hum.",
    "These ancient carved runestones emit a faint orange light and are warm to the touch.",
    "These ancient carved runestones emit a faint purple light. The surface of the runes shift as you gaze upon them.",
    "These ancient carved runestones emit a faint red light and soft, unintelligible whispers.",
    "These ancient carved runestones randomly phase in and out of the material plane.",
    "These ancient carved runestones absorb and transmit the energy of the Sun.",
    "Saltwater serves as the basis for much organic life. Useful and valuable for a variety of tradecraft. So salty.",
    "Tiny granules of rock and sediment. Useful in a variety of tradecraft and popularly used for sneak attacks.",
    "Solid rock hewn from the land. Useful in a variety of tradecraft.",
    "A noxious chemical soup birthed from machine processes. Useful in a variety of tradecraft, but best handled with extreme caution."
  ]

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
      sortedImageDir = Data.List.sort imageDir
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
    sortedImageDir
    resourceCids