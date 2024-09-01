{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


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
data AppConfig = AppConfig
  { appDbPool :: O.ConnectionPool
  }

type AppMonad = ReaderT AppConfig (ExceptT ServerError IO)
type AppServer = ServerT API AppMonad

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

app :: AppConfig -> Application
app config = logStdoutDev $ serve api (hoistServer api (convertApp config) appServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp config appMonad = Handler $ runReaderT appMonad config
