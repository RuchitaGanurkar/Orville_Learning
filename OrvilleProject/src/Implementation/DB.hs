module Implementation.DB where

import Control.Monad.Reader
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Execution as Execution
import Implementation.Config (AppConfig(..))
import qualified Orville.PostgreSQL.Raw.Connection as RC
import Implementation.Models.Student (studentTable)
import Implementation.Core
import qualified Orville.PostgreSQL.Raw.Connection as O

runDB :: AppMonad a -> O.SqlExecutionError-> AppMonad a --check 
runDB appConfig query = do
  conn <- ask myPool
  result <- liftIO $ O.withConnection conn $ \conn -> runSqlPool query conn
  case result of
    Left err -> throwError err500
    Right val -> return val


runDBMigration :: AppMonad ()
runDBMigration = do 
  appConfig <- ask 
  runDB appConfig (runDBMigration migrateAll)



