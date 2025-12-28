{-# LANGUAGE DeriveAnyClass #-}

module Types (GotoResponse(..)) where
import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data GotoResponse = GotoResponse
  { success :: Bool,
    file :: Maybe FilePath,
    line :: Maybe Int,
    column :: Maybe Int,
    symbolName :: Maybe Text,
    packageName :: Maybe Text,
    message :: Text
  }
  deriving (Generic, ToJSON)
