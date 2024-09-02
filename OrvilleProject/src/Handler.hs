{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}


module Handler where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Orville.PostgreSQL as O

import Marshaller
import Data.Aeson.Types


-- API Definitions
data ApiStudent = ApiStudent
  { student_id :: StudentId
  , student_name :: StudentName
  , student_age :: StudentAge
  } deriving (Generic, Show)
instance FromJSON ApiStudent where
    parseJSON = withObject "ApiStudent" $ \s -> ApiStudent <$> s .: "student_id" <*> s .: "student_name" <*> s .: "student_age"
    

instance ToJSON ApiStudent where 
    toJSON (ApiStudent student_id student_name student_age) = 
        object [ "student_id" .= student_id , "student_name" .= student_name , "student_age" .= student_age ]

type API =
       "student" :> ReqBody '[JSON] ApiStudent :> Post '[JSON] NoContent
  :<|> "student" :> Capture "id" Int32 :> Get '[JSON] ApiStudent
  :<|> "student" :> Capture "name" T.Text :> Get '[JSON] ApiStudent



api :: Proxy API
api = Proxy

-- App Configuration

type AppServer = ServerT API AppMonad



---------------------------------------------------------------------------------------------------

--CRON TASK : DATE (2nd September 2024)

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



-- CRON APIs

type CronAPI = 
  "cron" :> Capture "identifier" T.Text :> Capture "format" T.Text :> ReqBody '[JSON] Cron :> Post '[JSON] NoContent
  :<|>
  "create" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent
  :<|> 
  "update" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent
  :<|> 
  "delete" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Post '[JSON] NoContent
  :<|> 
   "create" :> Capture "identifier" T.Text :> ReqBody '[JSON] Graph :> Get '[JSON] Graph


type CronServer = ServerT CronAPI AppMonad

cronApi :: Proxy CronAPI 
cronApi = Proxy 



data AppConfig = AppConfig
  { 
    appDbPool :: O.ConnectionPool
  }

type AppMonad = ReaderT AppConfig (ExceptT ServerError IO)



postCronData :: Cron -> AppMonad NoContent
postCronData cronData = do 
  pool <- asks appDbPool 
  liftIO $ O.runOrville pool $ do 
    _ <- O.insertEntity cronTable cronData 
    case status cronData of 
      Pending _ -> O.insertEntity graphTable $  
                                              (Graph {
                                                graphId =  undefined ,
                                                graphData = undefined,
                                                graphIdentifier = undefined
                                              }) 
      _         -> return ()
    return NoContent                                    

-- check for type signature 
-- Cron -> Graph -> AppMonad NoContent


getCronIdentifier :: T.Text  -> AppMonad Cron 
getCronIdentifier check_identifier = do
  pool <- asks appDbPool 
  mCron <- liftIO $ O.runOrville pool $ do
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals identifierField (Identifier check_identifier)))
  case mCron of 
    Nothing -> throwError err404 
    Just cron -> return cron


-- parseFormat checks which format is given by api

parseFormat :: T.Text -> Formats
parseFormat "create" = Create (T.pack $ "create")
parseFormat "update" = Update (T.pack $ "update")
parseFormat "delete" = Delete (T.pack $ "delete")
parseFormat _        = error "Invalid format"



checkCronIdentifier :: T.Text -> AppMonad Cron
checkCronIdentifier =  getCronIdentifier


checkCronCreateFormat :: T.Text -> T.Text ->  AppMonad NoContent
checkCronCreateFormat check_identifier check_format = do 
  let new_cron = Cron {
    identifier = Identifier check_identifier ,
    format = Create check_format
  }
  postCronData new_cron


checkCronUpdateFormat :: T.Text -> T.Text ->  AppMonad NoContent
checkCronUpdateFormat check_identifier check_format = do 
  let new_cron = Cron {
    identifier = Identifier check_identifier ,
    format = Update check_format
  }
  postCronData new_cron
 

checkCronDeleteFormat :: T.Text -> T.Text ->  AppMonad NoContent
checkCronDeleteFormat check_identifier check_format = do 
  let new_cron = Cron {
    identifier = Identifier check_identifier ,
    format = Delete check_format
  }
  postCronData new_cron



getCronCode :: Int32 -> AppMonad Cron
getCronCode cid = do
  pool <- asks appDbPool
  maybeCron <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy cronTable (O.where_ (O.fieldEquals cronIdField (CronId cid)))
  case maybeCron of
    Nothing -> throwError err404
    Just cron -> return cron



checkCronCode :: Int32 -> AppMonad Cron
checkCronCode = getCronCode






cronServer:: CronServer
cronServer= checkCronIdentifier :<|> checkCronCreateFormat :<|> checkCronUpdateFormat :<|> checkCronDeleteFormat :<|> checkCronCode 




cron_app :: AppConfig -> Application
cron_app config = logStdoutDev $ serve cronApi (hoistServer cronApi (convertApp config) cronServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp cfg appMonad = Handler $ runReaderT appMonad cfg

















postStudent :: ApiStudent -> AppMonad NoContent
postStudent apiStudent = do
  pool <- asks appDbPool
  liftIO $ O.runOrville pool $ do
    let student = Student
          { studentId = student_id apiStudent
          , studentName = student_name apiStudent
          , studentAge = student_age apiStudent
          }
    _ <- O.insertEntity studentTable student
    return NoContent

getStudentById :: Int32 -> AppMonad ApiStudent
getStudentById sid = do
  pool <- asks appDbPool
  maybeStudent <- liftIO $ O.runOrville pool $
    O.findFirstEntityBy studentTable (O.where_ (O.fieldEquals studentIdField (StudentId sid)))
  case maybeStudent of
    Nothing -> throwError err404
    Just student -> return $ toApiStudent student

getStudentByName :: T.Text -> AppMonad ApiStudent
getStudentByName name = do
  pool <- asks appDbPool
  students <- liftIO $ O.runOrville pool $
    O.findEntitiesBy studentTable (O.where_ (O.fieldEquals studentNameField name))
  case students of
    [] -> throwError err404
    (student:_) -> return $ toApiStudent student

toApiStudent :: Student -> ApiStudent
toApiStudent student = ApiStudent
  { student_id = studentId student
  , student_name = studentName student
  , student_age= studentAge student
  }

appServer :: AppServer
appServer = postStudent :<|> getStudentById :<|> getStudentByName

student_app :: AppConfig -> Application
student_app config = logStdoutDev $ serve api (hoistServer api (convertApp config) appServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp config appMonad = Handler $ runReaderT appMonad config
