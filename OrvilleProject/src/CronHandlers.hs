{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

--CRON TASK : DATE (9th September 2024)

{-

APIs

1. /cron/identifier/format (create - update - delete)
2. /create/identifier : POST
3. /update/identifier : POST
4. /delete//identifier : DELETE



what each api do
  2. /create/identifier/{data in JSON format} 
      will takes the data in JSON format store it into cron table of DB 
      since data will have only {name and age} field , we need to mention status field as well with format as create or in api as create



  this will be POST request since storing data into DB

  2. since our format would be create , it'll check for Format data type
    call respective function for create (we are using handleCreate such function) but required
    type signature is handleCreate :: Identifier -> Data -> IO() | also check for identifier present or not
    if not throws or handler error handling case and if yes the 



  1. me forM_ ka use krke infinite loop for checking status Pending


cronId Should be unique


you can not do create for any identifier more than 1 (maybe primary or unique constraint ) - cron table me nahin chlega but graph me kr skte he



-}


type CronFormat = "cron" :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent

-----------------------------------------------------------------------------------------------------------
type CronCreate = "create" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent
type CronUpdate = "update" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Put '[JSON] NoContent
type CronDelete = "delete" :> Capture "identifier" T.Text :> Delete '[JSON] NoContent
-- for above 3 need to modify handleCreate/handleUpdate/handleDelete functions

-- -----------------------------------------------------------------------------------------------------------

-- type CronByStatus = "cron" :> "status" :> Capture "status" Status :> Get '[JSON] Cron
-- it retreive data from cron table based on status == Pending
-- type CronByFormat = "cron" :> "identifier" :> Capture "identifier" T.Text :> Get '[JSON] Cron
-- type CronByIdentifier = "cron" :> Capture "identifier" T.Text :> Get '[JSON] Cron
-- type PostGraphData = "cron" :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent



type CAPI = 
        CronFormat
   :<|> CronCreate -- handleCreate
  --  :<|> CronUpdate -- handleUpdate
  --  :<|> CronDelete -- handleDelete
  -- --  :<|> CronByStatus
  --  :<|> CronByFormat
  --  :<|> CronByIdentifier
  --  :<|> PostGraphData 


type CAppServer = ServerT CAPI AppMonad

capi :: Proxy CAPI 
capi = Proxy 

-----------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------



parseFormat :: T.Text -> Formats
parseFormat "Create" = Create (T.pack $ "Create")
parseFormat "Update" = Update (T.pack $ "Update")
parseFormat "Delete" = Delete (T.pack $ "Delete")
parseFormat _        = error "Invalid Format"


postCron :: Cron -> AppMonad NoContent
postCron cron = do
  pool <- asks appDbPool
  liftIO $ O.runOrville pool $ do
    -- Insert the Cron entity into the Cron table
    _ <- O.insertEntity cronTable cron
    return NoContent


handleCron :: T.Text -> T.Text -> Cron -> AppMonad NoContent
handleCron ident status cron = do
  let newCron = cron { identifier = Identifier ident, status = parseStatus status }
  postCron newCron


getCronByFormat :: T.Text -> AppMonad Cron
getCronByFormat formatText = do
  let format = parseFormat formatText
  pool <- asks appDbPool
  maybeCron <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals formatField format))
  case maybeCron of
    Nothing -> throwError err404
    Just cron -> return cron


-----------------------------------------------------------------------------------------------------------

getCronByStatus :: T.Text-> AppMonad Cron
getCronByStatus statusText = do 
  let status = parseStatus statusText
  pool <- asks appDbPool 
  maybeCron <- liftIO $ O.runOrville pool $ 
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals statusField status))
  case maybeCron of 
    Just cron -> return cron 
    Nothing -> throwError err404



parseStatus :: T.Text -> Status
parseStatus "Pending" = Pending (T.pack $ "Pending")
parseStatus "Success" = Success (T.pack $ "Success")
parseStatus _        = error "Invalid Status"


-----------------------------------------------------------------------------------------------------------

getCronByIdentifier :: T.Text -> AppMonad Cron
getCronByIdentifier ident = do
  pool <- asks appDbPool
  maybeCron <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals identifierField (Identifier ident)))
  case maybeCron of
    Nothing -> throwError err404
    Just cron -> return cron


-----------------------------------------------------------------------------------------------------------

postGraphData :: Graph -> AppMonad NoContent
postGraphData graph = do 
  pool <- asks appDbPool 
  liftIO $ O.runOrville pool $ do 
    _ <- O.insertEntity graphTable graph 
    return NoContent



-----------------------------------------------------------------------------------------------------------


-- Below functions will be modified later, 
-- since it must incoporate with Graph table corresponding to request type
    
handleCreate :: T.Text -> Graph -> AppMonad NoContent
handleCreate ident graph = do
  pool <- asks appDbPool
  maybeCron <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals identifierField (Identifier ident)))
  case maybeCron of
    Just cron -> case status cron of
      Pending _ -> do
        -- Insert Graph data only if status is Pending
        liftIO $ O.runOrville pool $ O.insertEntity graphTable graph
        return NoContent
      _ -> throwError err400  -- If the status is not Pending, return an error
    Nothing -> throwError err404  -- If the Cron record is not found






-----------------------------------------------------------------------------------------------------------


cronServer :: CAppServer
cronServer = 
         postCron 
    :<|> handleCreate 



cronApp :: AppConfig -> Application
cronApp config = logStdoutDev $ serve capi (hoistServer capi (convertApp config) cronServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp cfg appMonad = Handler $ runReaderT appMonad cfg


-----------------------------------------------------------------------------------------------------------

