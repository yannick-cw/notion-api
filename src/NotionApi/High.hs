module NotionApi.High where

import           NotionApi.Types
import           NotionApi.Low
import           Data.Foldable                  ( toList )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Exception
import           Safe                           ( headMay )

getUserData :: (NotionApiLow m, MonadError IOException m) => m UserData
getUserData = do
  userContent <- loadUserContent
  ( maybe (throwError $ userError "Could not find UserData") (return . value)
    . headMay
    . toList
    . notion_user
    . recordMap
    )
    userContent
