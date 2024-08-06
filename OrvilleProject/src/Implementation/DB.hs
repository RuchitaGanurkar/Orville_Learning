module Implementation.DB where

import Control.Monad.Reader
import Control.Monad.Except (throwError, ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Orville.PostgreSQL as O
import Implementation.Config (AppConfig(..))
import Implementation.Core
import qualified Orville.PostgreSQL.AutoMigration as M
import Servant
import Implementation.Models.Student ( studentTable ) 
import Orville.PostgreSQL.AutoMigration (autoMigrateSchema)


runDB :: O.QueryType -> AppMonad a
runDB query = do
  result <- liftIO $ O.executeDDL query  -- Correct function to execute DDL
  case result of
    Left err -> throwError err
    Right res -> return res



-- runDB :: MonadIO m => AppConfig -> (O.ConnectionPool -> IO a) -> m a
-- runDB appConfig query = 
--   liftIO $ query (myPool appConfig)


--runDBMigration :: ReaderT AppConfig (ExceptT ServerError IO)   ()
runDBMigration = do
  appConfig <- ask 
  runDB appConfig (autoMigrateSchema studentTable)
  return()


--QueryType | autoMigrateSchema | DDLQuery (nothing)