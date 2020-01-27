{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NotionApi.Types where

import           Data.Text
import           Control.Monad.Reader
import           GHC.Generics
import           Data.UUID
import           Data.HashMap.Strict            ( HashMap )
import           Data.Aeson.Types               ( ToJSON(..)
                                                , FromJSON(..)
                                                , sumEncoding
                                                , defaultOptions
                                                , genericToJSON
                                                , unwrapUnaryRecords
                                                , SumEncoding(..)
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
newtype RecordMap = RecordMap { notion_user :: HashMap UUID NotionUser } deriving (Show, Generic)
instance FromJSON RecordMap
data NotionUser = NotionUser { role :: Text, value :: UserData } deriving (Show, Generic)
instance FromJSON NotionUser
data UserData = UserData { id :: UUID, email :: Text, given_name :: Text, family_name :: Text } deriving (Show, Generic)
instance FromJSON UserData

-- Transactions Low
newtype Transaction = Transaction { operations :: [ Operation ] } deriving (Generic)
instance ToJSON Transaction
data Operation = Operation { id :: Text, path :: [Text], command :: Text, table :: Text, args :: Arg } deriving (Generic)
instance ToJSON Operation
data TextOrNum = S Text | N Int deriving (Generic)
instance ToJSON TextOrNum where
  toJSON = genericToJSON
    (defaultOptions { sumEncoding = UntaggedValue, unwrapUnaryRecords = True })
data Arg =  ArrayArgs [[Text]]  | ObjArgs ( HashMap Text TextOrNum ) deriving (Generic)
instance ToJSON Arg where
  toJSON = genericToJSON
    (defaultOptions { sumEncoding = UntaggedValue, unwrapUnaryRecords = True })

-- Transactions High
data Element = SubHeader Text | TextContent Text | Divider
data Action = AppendElement { _type :: Element, parentPageId :: UUID }
            | InsertInElement { elementId :: UUID, content :: Text }

