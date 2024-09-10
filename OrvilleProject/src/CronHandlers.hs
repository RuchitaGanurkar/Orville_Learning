{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PartialTypeSignatures #-}


module CronHandlers where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, asks)
import qualified Data.Text as T
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Orville.PostgreSQL as O

import Handler
import CronMarshaller

---------------------------------------------------------------------------------------------------

-- CRON TASK : DATE (10th September 2024)

{-

APIs

1. /cron
2. /create/identifier : POST
3. /update/identifier : PUT
4. /delete//identifier : DELETE



-}


type CronFormat = "cron" :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent
type CronCreate = "create" :> Capture "identifier" T.Text :> Post '[JSON] NoContent
type CronUpdate = "update" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Put '[JSON] NoContent
type CronDelete = "delete" :> Capture "identifier" T.Text :> Delete '[JSON] NoContent



type CAPI =
        CronFormat --postCron
   :<|> CronCreate -- handleCreate
   :<|> CronUpdate -- handleUpdate
   :<|> CronDelete -- handleDelete

type CAppServer = ServerT CAPI AppMonad

capi :: Proxy CAPI
capi = Proxy

-------------------------------------------------------------------------------------------------
-- This method can be used to insert data into cron table
{-
{
  "cronId": 1,
  "format": { "tag": "Create/Update/Delete", "contents": "Any Remark" },
  "status": { "tag": "Pending/Success/Error", "contents": "Any Remark" },
  "identifier": "test-identifier-check-1",
  "dataField": {
    "name": "R G",
    "age": 32,
    "mobile": 123456
  }
}

cronId | identifier : Must Be Unique

-}


postCron :: Cron -> AppMonad NoContent
postCron cron = do
  pool <- asks appDbPool
  liftIO $ O.runOrville pool $ do
    _ <- O.insertEntity cronTable cron
    return NoContent

-----------------------------------------------------------------------------------------------------------
-- This method will check for Format | Status 
-- Insert data into Graph Table if Format == Create && Status == Pending in Cron Table
-- Identifier must be uniquely used for each entry

handleCreate :: T.Text -> AppMonad NoContent
handleCreate ident = do
  pool <- asks appDbPool
  result <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals identifierField $ Identifier ident ))

  case result of
    Just cron -> case (format cron, status cron) of
      -- Create and Pending
      (Create _, Pending _) -> do
        -- Create Graph Table Entry
        _ <- liftIO $ O.runOrville pool $ O.insertEntity graphTable (Graph (cronId cron) (dataField cron))
        return NoContent
      --Update and Delete
      (Update _, _) -> throwError err400 { errBody = "Cannot create as the format is 'Update'" }
      (Delete _, _) -> throwError err400 { errBody = "Cannot create as the format is 'Delete'" }
       --Success and Error
      (_, Success _) -> throwError err400 { errBody = "Cannot create as the status is 'Success'" }
      (_, Error _) -> throwError err400 { errBody = "Cannot create as the status is 'Error'" }

    Nothing -> throwError err404 { errBody = "Cron record not found" }



-----------------------------------------------------------------------------------------------------------
-- This method will check for Format | Status 
-- Update data into Graph Table if Format == Update && Status == Pending in Cron Table
-- Identifier must be uniquely used for each entry
-- It accept Graph in request body
{-
{
  "g_id": 1,
  "g_details": {
    "name": "R G",
    "age": 25,
    "mobile": 9876490
  }
}
-}



handleUpdate :: T.Text -> Graph -> AppMonad NoContent
handleUpdate ident graph = do
  pool <- asks appDbPool

  result <- liftIO $ O.runOrville pool $
    O.findEntitiesBy cronTable (O.where_ (O.fieldEquals identifierField $ Identifier ident))

  case result of
    -- If Cron entry is found, proceed to check the format and status
    (cron:_) -> case (format cron, status cron) of
      -- Update and Pending
      (Update _, Pending _) -> do
        -- Update Graph Table Entry
        rowCount <- liftIO $ O.runOrville pool $ O.updateEntityAndReturnRowCount graphTable (cronId cron) graph
        if rowCount > 0
          then return NoContent
          else throwError err400 { errBody = "Failed to update the Graph table" }
      --Create and Delete 
      (Create _, _) -> throwError err400 { errBody = "Cannot update as the format is 'Create'" }
      (Delete _, _) -> throwError err400 { errBody = "Cannot update as the format is 'Delete'" }

      -- Success and Error
      (_, Success _) -> throwError err400 { errBody = "Cannot update as the status is 'Success'" }
      (_, Error _) -> throwError err400 { errBody = "Cannot delete as the status is 'Error'" }

    [] -> throwError err404 { errBody = "Cron record not found" }



-----------------------------------------------------------------------------------------------------------
-- This method will check for Format | Status 
-- Delete data into Graph Table if Format == Delete && Status == Pending in Cron Table
-- Identifier must be uniquely used for each entry

handleDelete :: T.Text -> AppMonad NoContent
handleDelete ident = do
  pool <- asks appDbPool

  result <- liftIO $ O.runOrville pool $
    O.findEntitiesBy cronTable (O.where_ (O.fieldEquals identifierField $ Identifier ident))

  case result of
     (cron:_) -> case (format cron, status cron) of
      -- Delete and Pending
      (Delete _, Pending _) -> do
        -- Delete the corresponding Graph entry
        maybeDeletedGraph <- liftIO $ O.runOrville pool $ O.deleteAndReturnEntity graphTable (cronId cron)
        case maybeDeletedGraph of
          Just _ -> return NoContent
          Nothing -> throwError err400 { errBody = "Failed to delete from the Graph table either format or status is wrong" }

      --Create and Update
      (Create _, _) -> throwError err400 { errBody = "Cannot delete as the format is 'Create'" }
      (Update _, _) -> throwError err400 { errBody = "Cannot delete as the format is 'Update'" }

      --Success and Error
      (_, Success _) -> throwError err400 { errBody = "Cannot delete as the status is 'Success'" }
      (_, Error _) -> throwError err400 { errBody = "Cannot delete as the status is 'Error'" }

     [] -> throwError err404 { errBody = "Cron record not found" }



-----------------------------------------------------------------------------------------------------------


cronServer :: CAppServer
cronServer =
         postCron
    :<|> handleCreate
    :<|> handleUpdate
    :<|> handleDelete


cronApp :: AppConfig -> Application
cronApp config = logStdoutDev $ serve capi (hoistServer capi (convertApp config) cronServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp cfg appMonad = Handler $ runReaderT appMonad cfg


-----------------------------------------------------------------------------------------------------------

