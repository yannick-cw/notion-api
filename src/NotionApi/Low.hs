module NotionApi.Low
  ( NotionApiLow(..)
  )
where

import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON(..)
                                                , sumEncoding
                                                , defaultOptions
                                                , SumEncoding(..)
                                                , Value
                                                )
import           Data.ByteString.Lazy.Char8    as C
                                                ( unpack )
import           Data.ByteString.Lazy.Internal  ( ByteString )
import           Control.Monad.Reader
import           Data.Aeson                     ( genericParseJSON )
import           Data.Map                      as M
                                                ( Map )
import           Data.Text                     as T
                                                ( Text
                                                , pack
                                                )
import           Data.Text.IO                  as T
                                                ( putStrLn )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Network.Wreq
import           Data.UUID
import           GHC.Generics
import           Network.HTTP.Client            ( createCookieJar
                                                , Cookie(Cookie)
                                                )
import           Data.Aeson.Types               ( emptyObject
                                                , ToJSON
                                                , FromJSON(..)
                                                , sumEncoding
                                                , unwrapUnaryRecords
                                                , genericToJSON
                                                , fieldLabelModifier
                                                , defaultOptions
                                                , SumEncoding(..)
                                                )
import           Data.Aeson.Lens                ( _Object
                                                , key
                                                )
import           Data.Aeson                     ( toJSON
                                                , genericParseJSON
                                                )
import           Data.Time.Clock
import           Data.ByteString.Char8          ( pack )
import           Data.Text                      ( unpack )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                , maybeToList
                                                )
import           Data.HashMap.Strict            ( keys
                                                , HashMap
                                                , fromList
                                                )
import           Data.UUID
import           Data.List.Split                ( splitWhen )
import           Control.Lens
import           GHC.Generics
import           System.Random                  ( randomIO )
import           NotionApi.Types


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

class NotionApiLow m where
  loadUserContent :: m UserContent
  submitTransaction :: Transaction -> m ()
  --getRecordValues :: UUID -> m [RecordResponse]
  --searchBlocks :: SearchQuery -> m SearchResult

instance NotionApiLow NotionM where
  loadUserContent   = loadUserContent'
  submitTransaction = submitTransaction'

loadUserContent' :: NotionM UserContent
loadUserContent' = do
  opts <- cookieOpts
  let url = notionUrl ++ "loadUserContent"
  logRequest url emptyObject
  r <- liftIO $ postWith opts url emptyObject
  logResponse r
  userContentRes <- asJSON r
  return $ userContentRes ^. responseBody

submitTransaction' :: Transaction -> NotionM ()
submitTransaction' transaction = do
  opts <- cookieOpts
  let url  = notionUrl ++ "submitTransaction"
      body = toJSON transaction
  logRequest url body
  r <- liftIO $ postWith opts url body
  logResponse r
  return ()


--data SearchQuery = SearchQuery { query :: Text, table :: Text, id :: UUID, limit :: Int} deriving (Generic, Show)
--instance ToJSON SearchQuery

--data SearchResult = SearchResult { results :: [UUID], recordMap :: Maybe RecordMap } deriving (Generic, Show)
--instance FromJSON SearchResult
--newtype RecordMap = RecordMap { block :: Maybe (Map UUID Entry) } deriving (Generic, Show)
--instance FromJSON RecordMap
--newtype Entry = Entry { value :: Val } deriving (Generic, Show)
--instance FromJSON Entry
--data Val = Val { content :: Maybe [UUID], parent_id :: UUID, properties :: Maybe Properties} deriving (Generic, Show)
--instance FromJSON Val
--data Properties = Properties { source :: Maybe [[Source]], title :: Maybe [[Source]] } deriving (Generic, Show)
--instance FromJSON Properties
--data Source = TextContent Text | Other Value deriving (Generic, Show, Eq)
--instance FromJSON Source where
  --parseJSON = genericParseJSON (defaultOptions { sumEncoding = UntaggedValue })

--newtype RecordResponse = RecordResponse { results :: [Entry]} deriving (Generic, Show)
--instance FromJSON RecordResponse

cookieOpts :: NotionM Options
cookieOpts = do
  (NotionId notionId) <- asks nId
  now                 <- liftIO getCurrentTime
  let expires = addUTCTime (1440 * 3000) now
      c       = Cookie "token_v2"
                       (encodeUtf8 notionId)
                       expires
                       "notion.so"
                       "/"
                       now
                       now
                       False
                       False
                       True
                       True
      jar   = createCookieJar [c]
      opts  = defaults :: Options
      opts' = opts & cookies ?~ jar
  return opts'

logRequest :: ToJSON a => String -> a -> NotionM ()
logRequest url body = verboseOut ("Request Url: " <> T.pack url)
  *> verboseOut ("Request Body: " <> T.pack (C.unpack (encodePretty body)))

logResponse :: Response ByteString -> NotionM ()
logResponse res = verboseOut ("Response Status Code: " <> code)
  *> verboseOut ("Response Body: " <> body)
 where
  code = T.pack $ show $ res ^. responseStatus
  body = T.pack $ C.unpack $ res ^. responseBody

verboseOut :: Text -> NotionM ()
verboseOut t = do
  isVerbose <- asks verbose
  liftIO $ when isVerbose (T.putStrLn t)
