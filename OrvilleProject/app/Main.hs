module Main where


import qualified Orville.PostgreSQL.AutoMigration as A
import qualified Orville.PostgreSQL as O
import Network.Wai.Handler.Warp (run)
import Handler
import Marshaller
import qualified Configuration as C
import System.Environment


main :: IO ()
main = do
  putStrLn "Application Running On Port 3000"
  mConnOpts <- lookupEnv "DB_CONN_STRING"
  case mConnOpts of
    Just connString-> do
      pool <- O.createConnectionPool (C.connectionOptions connString)
      O.runOrville pool $ A.autoMigrateSchema A.defaultOptions [A.SchemaTable cronTable]
      let appConfig = AppConfig { appDbPool = pool }
      run 3000 (app appConfig)
    Nothing -> error "Database Configuration Not Found"


