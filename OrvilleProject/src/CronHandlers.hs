{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}



module CronHandlers where

import Data.Aeson.Types
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Orville.PostgreSQL as O

import Handler
import CronMarshaller

---------------------------------------------------------------------------------------------------

--CRON TASK : DATE (4th September 2024)

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



-- type PostCronFormat = 
--   "cron" :> Capture "identifier" T.Text 
--          :> Capture "format" T.Text 
--          :> ReqBody '[JSON] Cron 
--          :> Post '[JSON] NoContent
-- -- Done

-- type GetCronByCreate = 
--   "create" :> Capture "identifier" T.Text 
--          :> Get '[JSON] Cron


-- type CronByCreate =
--   "create" :> Capture "identifier" T.Text
--            :> ReqBody '[JSON] Cron 
--            :> Post '[JSON] NoContent


-- type CronByUpdate =
--   "update" :> Capture "identifier" T.Text
--            :> ReqBody '[JSON] Cron 
--            :> Post '[JSON] NoContent


-- type CronByDelete =
--   "delete" :> Capture "identifier" T.Text
--            :> ReqBody '[JSON] Cron 
--            :> Delete '[JSON] NoContent   


-- type GetCronByUpdate = 
--   "update" :> Capture "identifier" T.Text 
--          :> Get '[JSON] Cron


-- type GetCronByDelete = 
--   "delete" :> Capture "identifier" T.Text 
--          :> Get '[JSON] Cron

type CronFormat = "cron" :> Capture "identifier" T.Text :> Capture "format" T.Text :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent
type CronCreate = "create" :> Capture "identifier" T.Text :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent
type CronUpdate = "update" :> Capture "identifier" T.Text :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent
type CronDelete = "delete" :> Capture "identifier" T.Text :> ReqBody '[JSON] Cron :> Delete '[JSON] NoContent
type CronByFormat = "cron" :> "identifier" :> Capture "identifier" T.Text :> Get '[JSON] Cron
type PostGraphData = "cron" :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent



type CAPI = 
        CronFormat
   :<|> CronCreate 
   :<|> CronUpdate 
   :<|> CronDelete
   :<|> CronByFormat
   :<|> PostGraphData 


type CAppServer = ServerT CAPI AppMonad

capi :: Proxy CAPI 
capi = Proxy 

{-
{
  "cronId": 1,
  "format": {
    "tag": "Create", 
    "contents": "Create" 
  },
  "status": {
    "tag": "Pending", 
    "contents": "Pending" 
  },
	"identifier" : "567" ,
  "dataField": {
		"name" : "ABC" , 
		"age" : 27 ,
		"mobile" : 543218
	
	}
}
-}

postCron :: Cron -> AppMonad NoContent
postCron cron = do
  pool <- asks appDbPool
  liftIO $ O.runOrville pool $ do
    -- Insert into cron table
    _ <- O.insertEntity cronTable cron
    case status cron of
      Pending _ -> do
        let person = dataField cron
        -- -- Insert into graph table
        _ <- O.insertEntity graphTable (Graph { g_details = person })
        return ()
      _ -> return ()
    
    return NoContent



-- Function to extract PersonName from Cron
extractNameFromCron :: Cron -> PersonName
extractNameFromCron cron = name (dataField cron)

-- Function to extract PersonAge from Cron
extractAgeFromCron :: Cron -> PersonAge
extractAgeFromCron cron = age (dataField cron)

extractMobileFromCron :: Cron -> PersonMobile
extractMobileFromCron cron = mobile (dataField cron)



{-
	{
	"g_details" : {
		"name" : "ABC" ,
		"age" : 34 ,
	  "mobile" : 4567898
	}
-}

postGraphData :: Graph -> AppMonad NoContent
postGraphData graph = do 
  pool <- asks appDbPool 
  liftIO $ O.runOrville pool $ do 
    _ <- O.insertEntity graphTable graph 
    return NoContent


getCronByFormat :: T.Text -> AppMonad Cron
getCronByFormat formatText = do
  let format = parseFormat formatText
  pool <- asks appDbPool
  maybeCron <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals formatField format))
  case maybeCron of
    Nothing -> throwError err404
    Just cron -> return cron

getCronByIdentifier :: T.Text -> AppMonad Cron
getCronByIdentifier ident = do
  pool <- asks appDbPool
  maybeCron <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals identifierField (Identifier ident)))
  case maybeCron of
    Nothing -> throwError err404
    Just cron -> return cron


handleCronFormat :: T.Text -> T.Text -> Cron -> AppMonad NoContent
handleCronFormat ident format cron = do
  let newCron = cron { identifier = Identifier ident, format = parseFormat format }
  postCron newCron

handleCreate :: T.Text -> Cron -> AppMonad NoContent
handleCreate ident cron = do
  let newCron = cron { identifier = Identifier ident, format = Create ident }
  postCron newCron

handleUpdate :: T.Text -> Cron -> AppMonad NoContent
handleUpdate ident cron = do
  let newCron = cron { identifier = Identifier ident, format = Update ident }
  postCron newCron

handleDelete :: T.Text -> Cron -> AppMonad NoContent
handleDelete ident cron = do
  let newCron = cron { identifier = Identifier ident, format = Delete ident }
  postCron newCron

handleGetCronByIdentifier :: T.Text -> AppMonad Cron
handleGetCronByIdentifier = getCronByIdentifier



parseFormat :: T.Text -> Formats
parseFormat "Create" = Create (T.pack $ "Create")
parseFormat "Update" = Update (T.pack $ "Update")
parseFormat "Delete" = Delete (T.pack $ "Delete")
parseFormat _        = error "Invalid Format"





cronServer :: CAppServer
cronServer = handleCronFormat :<|> handleCreate :<|> handleUpdate :<|> handleDelete :<|> getCronByFormat :<|> postGraphData



cronApp :: AppConfig -> Application
cronApp config = logStdoutDev $ serve capi (hoistServer capi (convertApp config) cronServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp cfg appMonad = Handler $ runReaderT appMonad cfg

