
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Implementation where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as A
import System.Environment (lookupEnv)

-- Configuration for PostgreSQL
data PostgreSQLConfig = PostgreSQLConfig
  { pg_host :: String
  , pg_port :: String
  , pg_user :: String
  , pg_password :: String
  , pg_dbname :: String
  } deriving (Show, Eq)

pgConfigToString :: PostgreSQLConfig -> String
pgConfigToString c =
  "host=" <> pg_host c <>
  " port=" <> pg_port c <>
  " user=" <> pg_user c <>
  " password=" <> pg_password c <>
  " dbname=" <> pg_dbname c

getPostgresConfig :: IO (Maybe PostgreSQLConfig)
getPostgresConfig = do
  a <- lookupEnv "POSTGRES_HOST"
  b <- lookupEnv "POSTGRES_PORT"
  c <- lookupEnv "POSTGRES_USER"
  d <- lookupEnv "POSTGRES_PASS"
  e <- lookupEnv "POSTGRES_DB"
  return $ case (a, b, c, d, e) of
    (Just h, Just p, Just u, Just pa, Just da) -> Just $ PostgreSQLConfig h p u pa da
    _ -> Nothing

-- Use connection string to create ConnectionOptions
connectionOptions :: String -> O.ConnectionOptions
connectionOptions connString =
  O.ConnectionOptions
    { O.connectionString = connString
    , O.connectionNoticeReporting = O.DisableNoticeReporting
    , O.connectionPoolStripes = O.OneStripePerCapability
    , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
    , O.connectionPoolLingerTime = 10
    }

-- Models and Database Schema
newtype StudentId = StudentId Int32 deriving (Show, Eq, Generic, FromJSON, ToJSON)

type StudentName = T.Text
type StudentAge = Int32

data Student = Student
  { studentId :: StudentId
  , studentName :: StudentName
  , studentAge :: StudentAge
  } deriving (Show, Generic)

studentIdField :: O.FieldDefinition O.NotNull StudentId
studentIdField = O.coerceField (O.integerField "id")

studentNameField :: O.FieldDefinition O.NotNull StudentName
studentNameField = O.unboundedTextField "name"

studentAgeField :: O.FieldDefinition O.NotNull StudentAge
studentAgeField = O.coerceField (O.integerField "age")

studentMarshaller :: O.SqlMarshaller Student Student
studentMarshaller =
  Student
    <$> O.marshallField studentId studentIdField
    <*> O.marshallField studentName studentNameField
    <*> O.marshallField studentAge studentAgeField

studentTable :: O.TableDefinition (O.HasKey StudentId) Student Student
studentTable =
  O.mkTableDefinition
    "orville_student_demo"
    (O.primaryKey studentIdField)
    studentMarshaller

-- API Definitions
data ApiStudent = ApiStudent
  { apiStudentId :: StudentId
  , apiStudentName :: StudentName
  , apiStudentAge :: StudentAge
  } deriving (Generic, Show, FromJSON, ToJSON)

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
          { studentId = apiStudentId apiStudent
          , studentName = apiStudentName apiStudent
          , studentAge = apiStudentAge apiStudent
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
  { apiStudentId = studentId student
  , apiStudentName = studentName student
  , apiStudentAge = studentAge student
  }

appServer :: AppServer
appServer = postStudent :<|> getStudentById :<|> getStudentByName

app :: AppConfig -> Application
app config = logStdoutDev $ serve api (hoistServer api (convertApp config) appServer)
  where
    convertApp :: AppConfig -> AppMonad a -> Handler a
    convertApp config appMonad = Handler $ runReaderT appMonad config
