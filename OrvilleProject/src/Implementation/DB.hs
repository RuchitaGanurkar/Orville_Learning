module Implementation.DB where

import Control.Monad.Reader
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Orville.PostgreSQL as O
import Implementation.Config (AppConfig(..))
import Implementation.Core

runDB :: O.SqlExecution a -> AppMonad a
runDB query = do
  pool <- asks myPool
  result <- liftIO $ O.runOrville pool query
  case result of
    Left err -> throwError err500
    Right val -> return val

runDBMigration :: AppMonad ()
runDBMigration = runDB (O.executeMigration (O.migrateTables [studentTable]))
