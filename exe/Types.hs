{-# LANGUAGE DeriveAnyClass #-}

module Types (GotoResponse(..)) where
import GHC.Generics
import Data.Aeson

data GotoResponse = GotoResponse
  { success :: Bool,
    file :: Maybe FilePath,
    line :: Maybe Int,
    column :: Maybe Int,
    symbolName :: Maybe String,
    packageName :: Maybe String,
    message :: String
  }
  deriving (Generic, ToJSON)
