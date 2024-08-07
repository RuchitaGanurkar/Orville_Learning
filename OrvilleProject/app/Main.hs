module Main where


import qualified Orville.PostgreSQL.AutoMigration as A
import qualified Orville.PostgreSQL as O
import Network.Wai.Handler.Warp (run)
import Implementation


main :: IO ()
main = do
  putStrLn "Application Running On Port 3000"
  maybeConfig <- getPostgresConfig
  case maybeConfig of
    Just dbConfig -> do
      let connOpts = connectionOptions (pgConfigToString dbConfig)
      pool <- O.createConnectionPool connOpts
      O.runOrville pool $ A.autoMigrateSchema A.defaultOptions [A.SchemaTable studentTable]
      let appConfig = AppConfig { appDbPool = pool }
      run 3000 (app appConfig)
    Nothing -> error "Database Configuration Not Found"
