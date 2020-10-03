{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © Herbert Valerio Riedel 2015-2019
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Data.YAML.Builder
    ( YamlBuilder (..)
    , ToYaml (..)
    , mapping
    , namedMapping
    , maybeNamedMapping
    , array
    , namedArray
    , maybeNamedArray
    , string
    , namedString
    , maybeNamedString
    , bool
    , namedBool
    , maybeNamedBool
    , null'
    , namedNull
    , maybeNamedNull
    , alias
    , toEvents
    ) where
      
import           Control.Arrow (second)
import           Data.Text              (Text,empty)
import           Data.YAML.Event        as YE
import qualified Data.YAML.Schema       as YS
-- import           Control.Applicative    as Ap
-- import           Control.Monad.Identity (runIdentity)
-- import           Data.Aeson             as J
-- import qualified Data.Aeson.Types       as J
-- import qualified Data.ByteString.Lazy   as BS.L
-- import qualified Data.ByteString        as BS
-- import qualified Data.Vector            as V
-- import qualified Data.YAML              as Y 

-- import qualified Data.YAML.Event        as YE
-- import qualified Data.YAML.Token        as YT
-- import           Data.Scientific
-- import qualified Data.Map               as Map
-- import qualified Data.HashMap.Strict    as HM

newtype YamlBuilder = YamlBuilder { unYamlBuilder :: [Event] -> [Event] }

class ToYaml a where
    toYaml :: a -> YamlBuilder
instance ToYaml YamlBuilder where
    toYaml = id
instance ToYaml a => ToYaml [(Text, a)] where
    toYaml = mapping . map (second toYaml)
instance ToYaml a => ToYaml [a] where
    toYaml = array . map toYaml
instance ToYaml Text where
    toYaml = string
instance ToYaml Int where
    toYaml i = YamlBuilder (YE.Scalar Nothing untagged Plain (YS.encodeInt (toInteger i)): )


maybeNamedMapping :: Maybe Anchor -> [(Text, YamlBuilder)] -> YamlBuilder
maybeNamedMapping anchor pairs = YamlBuilder $ \rest ->
    MappingStart anchor YE.untagged Block : foldr addPair (MappingEnd : rest) pairs
  where
    addPair (key, YamlBuilder value) after
        = YE.Scalar Nothing YE.untagged Plain key
        : value after

mapping :: [(Text, YamlBuilder)] -> YamlBuilder
mapping = maybeNamedMapping Nothing

namedMapping :: Text -> [(Text, YamlBuilder)] -> YamlBuilder
namedMapping name = maybeNamedMapping $ Just name

maybeNamedArray :: Maybe Anchor -> [YamlBuilder] -> YamlBuilder
maybeNamedArray anchor bs =
    YamlBuilder $ (SequenceStart anchor YE.untagged Block:) . flip (foldr go) bs . (SequenceEnd:)
  where
    go (YamlBuilder b) = b

-- tagStr = YE.mkTag "tag:yaml.org,2002:str"

array :: [YamlBuilder] -> YamlBuilder
array = maybeNamedArray Nothing

namedArray :: Text -> [YamlBuilder] -> YamlBuilder
namedArray name = maybeNamedArray $ Just name

maybeNamedString :: Maybe Anchor -> Text -> YamlBuilder
-- Empty strings need special handling to ensure they get quoted. This avoids:

maybeNamedString anchor s   =
    YamlBuilder (event :)
  where
    event = Scalar anchor YE.untagged DoubleQuoted s
    -- event
    --     -- Make sure that special strings are encoded as strings properly.
    --     | s `HashSet.member` specialStrings || isNumeric s = EventScalar (encodeUtf8 s) NoTag SingleQuoted $ unpack <$> anchor
    --     | otherwise = EventScalar (encodeUtf8 s) StrTag PlainNoTag $ unpack <$> anchor

string :: Text -> YamlBuilder
string = maybeNamedString Nothing

namedString :: Text -> Text -> YamlBuilder
namedString name = maybeNamedString $ Just name

maybeNamedBool :: Maybe Text -> Bool -> YamlBuilder
maybeNamedBool anchor b = YamlBuilder (YE.Scalar anchor untagged Plain (YS.encodeBool b) :)

bool :: Bool -> YamlBuilder
bool = maybeNamedBool Nothing

namedBool :: Text -> Bool -> YamlBuilder
namedBool name = maybeNamedBool $ Just name

maybeNamedNull :: Maybe Text -> YamlBuilder
maybeNamedNull anchor = YamlBuilder (YE.Scalar anchor untagged Plain empty :)

null' :: YamlBuilder
null' = maybeNamedNull Nothing

namedNull :: Text -> YamlBuilder
namedNull name = maybeNamedNull $ Just name

alias :: Text -> YamlBuilder
alias anchor = YamlBuilder (Alias anchor :)

toEvents :: YamlBuilder -> [Event]
toEvents (YamlBuilder front) =
    StreamStart : DocumentStart NoDirEndMarker: front [DocumentEnd True, StreamEnd]

-- toSource :: (Monad m, ToYaml a) => a -> ConduitM i Event m ()
-- toSource = mapM_ yield . toEvents . toYaml

-- toByteString :: ToYaml a => a -> ByteString
-- toByteString = toByteStringWith defaultFormatOptions

-- toByteStringWith :: ToYaml a => FormatOptions -> a -> ByteString
-- toByteStringWith opts yb = unsafePerformIO $ runConduitRes $ toSource yb .| encodeWith opts

-- writeYamlFile :: ToYaml a => FilePath -> a -> IO ()
-- writeYamlFile = writeYamlFileWith defaultFormatOptions

-- writeYamlFileWith :: ToYaml a => FormatOptions -> FilePath -> a -> IO ()
-- writeYamlFileWith opts fp yb = runConduitRes $ toSource yb .| encodeFileWith opts fp