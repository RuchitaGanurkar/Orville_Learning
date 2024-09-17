{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}


module CronHandlers where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, asks)
import qualified Data.Text as T
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Orville.PostgreSQL as O

import Handler
import CronMarshaller
import Control.Monad (forM_)

---------------------------------------------------------------------------------------------------

-- CRON TASK : DATE (18th September 2024)

{-

APIs

1. /cron
2. /create/identifier : POST
3. /update/identifier : PUT
4. /delete//identifier : DELETE


PII : Personally Identifiable Information





-}


type CronFormat = "cron" :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent
type CronCreate = "create" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent
type CronUpdate = "update" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Put '[JSON] NoContent
-- type CronDelete = "delete" :> Capture "identifier" T.Text :> Delete '[JSON] NoContent



type CAPI =
        CronFormat --postCron
   :<|> CronCreate -- handleCreate
   :<|> CronUpdate -- handleUpdate
  --  :<|> CronDelete -- handleDelete

type CAppServer = ServerT CAPI AppMonad

capi :: Proxy CAPI
capi = Proxy

-------------------------------------------------------------------------------------------------
-- This method can be used to insert data into cron table

-- cronId | identifier : Must Be Unique



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

handleCreate :: T.Text -> Graph -> AppMonad NoContent
handleCreate ident graph = do
  pool <- asks appDbPool
  let loop = do
        -- Find the Cron entries by identifier
        result <- liftIO $ O.runOrville pool $
          O.findEntitiesBy cronTable (O.where_ (O.fieldEquals identifierField $ Identifier ident))

        forM_ result $ \cron -> do
          case (format cron, status cron) of

            (Create _, Pending _) -> do
              -- Insert into Graph table
              _ <- liftIO $ O.runOrville pool $ 
                O.insertEntity graphTable 
                (Graph (cronId cron)
                       (Identifier ident) 
                       (g_address graph) 
                       (g_bank_account graph) 
                       (dataField cron))

              -- Update Cron status to 'Success'
              let output = cron { status = Success "Success" }
              _ <- liftIO $ O.runOrville pool $ 
                O.updateEntity cronTable (cronId cron) output
              return NoContent

            (Update _, _) -> 
              throwError err400 { errBody = "Cannot create as the format is 'Update'" } -- check for Update format
            (Delete _, _) -> 
              throwError err400 { errBody = "Cannot create as the format is 'Delete'" } -- check for Delete format
            
            -- Invalid status are handled here | Irrespective of any format
            (_, Success _) -> 
              throwError err400 { errBody = "Cannot create as the status is 'Success'" }
            (_, Error _) -> 
              throwError err400 { errBody = "Cannot create as the status is 'Error'" }

        return NoContent
  loop  -- Infinite loop



-- Implement logic of deleting cron job entry from cronTable which is being added to graphTable 
-- or change status of same entry to success 

-----------------------------------------------------------------------------------------------------------
-- This method will check for Format | Status 
-- Update data into Graph Table if Format == Update && Status == Pending in Cron Table
-- Identifier must be uniquely used for each entry
-- It accept Graph in request body


handleUpdate :: T.Text -> Graph -> AppMonad NoContent
handleUpdate ident graph = do
  pool <- asks appDbPool
  let loop = do
        result <- liftIO $ O.runOrville pool $
          O.findEntitiesBy cronTable (O.where_ (O.fieldEquals identifierField $ Identifier ident))
        
        -- Process the cron entries
        forM_ result $ \cron -> do
          case (format cron, status cron) of
            -- Check if the format is 'Update' and the status is 'Pending'
            (Update _, Pending _) -> do
              -- Update the Graph table with values from the request body (graph)
              
              _ <- liftIO $ O.runOrville pool $ O.updateEntity graphTable (g_id graph) graph  -- assumed g_id is Primary Key for Graph table
              -- Update Cron status to 'Success'
              let output = cron { status = Success "Success" }
              _ <- liftIO $ O.runOrville pool $ O.updateEntity cronTable (cronId cron) output
      
              return ()
      
            -- Handle invalid formats
            (Create _, _) -> throwError err400 { errBody = "Cannot update as the format is 'Create'" }
            (Delete _, _) -> throwError err400 { errBody = "Cannot update as the format is 'Delete'" }
      
            -- Invalid status are handled here | Irrespective of any format
            (_, Success _) -> throwError err400 { errBody = "Cannot update as the status is 'Success'" }
            (_, Error _) -> throwError err400 { errBody = "Cannot update as the status is 'Error'" }

        return NoContent
  loop
-----------------------------------------------------------------------------------------------------------


cronServer :: CAppServer
cronServer =
         postCron
    :<|> handleCreate
    :<|> handleUpdate
    -- :<|> handleDelete


cronApp :: AppConfig -> Application
cronApp config = logStdoutDev $ serve capi (hoistServer capi (convertApp config) cronServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp cfg appMonad = Handler $ runReaderT appMonad cfg


-----------------------------------------------------------------------------------------------------------

