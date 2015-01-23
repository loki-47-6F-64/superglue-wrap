{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Control.Monad
import GHC.Generics

data Config = Config {
  buildType           :: !T.Text,
  targets             :: ![T.Text],
  androidProjectRoot  :: !T.Text
} deriving (Show,Generic)

instance FromJSON Config


readConfig :: FilePath -> IO Config
readConfig path = (fromJust . decode) <$> B.readFile path


main = readConfig "config.json" >>= print
