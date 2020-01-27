module NotionApi.High where

import           NotionApi.Types
import           NotionApi.Low
import           Data.Foldable                  ( toList )
import           System.Random                  ( randomIO )
import           Data.HashMap.Strict            ( fromList )
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Safe                           ( headMay )
import           Data.UUID                      ( toText )

getUserData :: NotionM UserData
getUserData = do
  userContent <- loadUserContent
  ( maybe (throwM $ userError "Could not find UserData") (return . value)
    . headMay
    . toList
    . notion_user
    . recordMap
    )
    userContent

runAction :: [Action] -> NotionM ()
runAction actions = do
  ops <- traverse actionToOperation actions
  submitTransaction (Transaction $ concat ops)
 where
  actionToOperation :: Action -> NotionM [Operation]
  actionToOperation (InsertInElement inId content') =
    return [addContent inId content']
  actionToOperation (AppendElement _type parentId) = do
    UserData userId _ _ _ <- getUserData
    opId                  <- liftIO randomIO
    case _type of
      SubHeader c -> return
        [ addSegment opId "sub_header" parentId userId
        , addAfter opId parentId
        , addContent opId c
        ]
      TextContent c -> return
        [ addSegment opId "text" parentId userId
        , addAfter opId parentId
        , addContent opId c
        ]
      Divider -> return
        [addSegment opId "divider" parentId userId, addAfter opId parentId]

  addAfter opId parentId = Operation
    { id      = toText parentId
    , path    = ["content"]
    , command = "listAfter"
    , table   = "block"
    , args    = ObjArgs (fromList [("id", S $ toText opId)])
    }
  addContent opId opContent = Operation { id      = toText opId
                                        , path    = ["properties", "title"]
                                        , command = "set"
                                        , table   = "block"
                                        , args    = ArrayArgs [[opContent]]
                                        }
  addSegment opId _type parentId userId = Operation
    { id      = toText opId
    , path    = []
    , command = "set"
    , table   = "block"
    , args    = ObjArgs
                  (fromList
                    [ ("id"          , S $ toText opId)
                    , ("version"     , N 1)
                    , ("alive"       , S "True")
                    , ("created_by"  , S $ toText userId)
                    , ("parent_id"   , S $ toText parentId)
                    , ("parent_table", S "block")
                    , ("type"        , S _type)
                    ]
                  )
    }

