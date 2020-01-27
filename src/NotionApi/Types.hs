{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NotionApi.Types where

import           Data.Text
import           Control.Monad.Reader
import           GHC.Generics
import           Data.UUID
import           Data.Map                      as M
                                                ( Map )
import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON(..)
                                                , sumEncoding
                                                , defaultOptions
                                                , SumEncoding(..)
                                                , Value
                                                )
import           Control.Monad.Catch            ( MonadThrow )

newtype NotionId = NotionId Text
data NotionArgs = NotionArgs { nId :: NotionId, verbose :: Bool }
newtype NotionM a = NotionM { unP :: ReaderT NotionArgs IO a } deriving (Functor, Applicative, Monad, MonadReader NotionArgs, MonadIO, MonadThrow)

runNotionM :: Text -> NotionM a -> IO a
runNotionM notionId notionM =
  runReaderT (unP notionM) (NotionArgs (NotionId notionId) False)

runNotionMVerbose :: Text -> NotionM a -> IO a
runNotionMVerbose notionId notionM =
  runReaderT (unP notionM) (NotionArgs (NotionId notionId) True)

-- UserContent
newtype UserContent = UserContent { recordMap :: RecordMap } deriving (Show, Generic)
instance FromJSON UserContent
newtype RecordMap = RecordMap { notion_user :: Map UUID NotionUser } deriving (Show, Generic)
instance FromJSON RecordMap
data NotionUser = NotionUser { role :: Text, value :: UserData } deriving (Show, Generic)
instance FromJSON NotionUser
data UserData = UserData { id :: UUID, email :: Text, given_name :: Text, family_name :: Text } deriving (Show, Generic)
instance FromJSON UserData

-- Transactions
newtype Transaction = Transaction { operations :: [ Operation ] } deriving (Generic)
instance ToJSON Transaction
data Operation = Operation { id :: String, path :: [String], command :: String, table :: String, args :: [[Text]] } deriving (Generic)
instance ToJSON Operation


