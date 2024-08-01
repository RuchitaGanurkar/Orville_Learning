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


-- runDB :: O.QueryType -> AppMonad a

-- runDB query = do
--   pool <- asks myPool
--   result <- liftIO $ O.DDLQuery pool query
--   case result of
--     Left err -> throwError err500
--     Right val -> return val


runDB :: MonadIO m => AppConfig -> (O.ConnectionPool -> IO a) -> m a
runDB appConfig query = 
  liftIO $ query (myPool appConfig)


--runDBMigration :: ReaderT AppConfig (ExceptT ServerError IO)   ()
runDBMigration = do
  appConfig <- ask 
  runDB appConfig (autoMigrateSchema studentTable)
  return()
