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





Request Body :

{
  "cronId": 9 ,
  "format": {
    "tag": "Delete", -- no use
    "contents": "create" -- no use
  },
  "status": {
    "tag": "Error", -- store
    "contents": "Waiting" -- store
  },
	"identifier" : "567" ,
  "dataField": {
		"name" : "Ruchi" , (-- store)
		"age" : 24 (-- store)
	
	}
}

cronId Should be unique


you can not do create for any identifier more than 1 (maybe primary or unique constraint ) - cron table me nahin chlega but graph me kr skte he



-}







type PostCronFormat = 
  "cron" :> Capture "identifier" T.Text 
         :> Capture "format" T.Text 
         :> ReqBody '[JSON] Cron 
         :> Post '[JSON] NoContent
-- Done


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






-- type GetCronByCreate = 
--   "create" :> Capture "identifier" T.Text 
--          :> Get '[JSON] Cron


-- type GetCronByUpdate = 
--   "update" :> Capture "identifier" T.Text 
--          :> Get '[JSON] Cron


-- type GetCronByDelete = 
--   "delete" :> Capture "identifier" T.Text 
--          :> Get '[JSON] Cron




type CAPI = PostCronFormat 

-- :<|>  CronByCreate :<|> CronByUpdate :<|> CronByDelete


type CAppServer = ServerT CAPI AppMonad

capi :: Proxy CAPI 
capi = Proxy 




postCronFormat :: Cron -> AppMonad NoContent
postCronFormat cron = do
  pool <- asks appDbPool
  liftIO $ O.runOrville pool $ do
    _ <- O.insertEntity cronTable cron
    case status cron of
      Pending _ -> O.insertEntity graphTable (Graph { g_name = undefined, g_mobile = undefined, g_age = undefined })
      _         -> return ()
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


handleCronFormat :: T.Text -> T.Text -> Cron -> AppMonad NoContent
handleCronFormat ident format cron = do
  let newCron = cron { identifier = Identifier ident, format = parseFormat format }
  postCronFormat newCron





parseFormat :: T.Text -> Formats
parseFormat "Create" = Create (T.pack $ "Create")
parseFormat "Update" = Update (T.pack $ "Update")
parseFormat "Delete" = Delete (T.pack $ "Delete")
parseFormat _        = error "Invalid Format"




cronServer :: CAppServer
cronServer = handleCronFormat 
-- :<|> getCronByFormat


cronApp :: AppConfig -> Application
cronApp config = logStdoutDev $ serve capi (hoistServer capi (convertApp config) cronServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp cfg appMonad = Handler $ runReaderT appMonad cfg

