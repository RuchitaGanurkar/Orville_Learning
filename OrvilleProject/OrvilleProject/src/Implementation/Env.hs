{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Implementation.Env where

import System.Environment (lookupEnv)
import Implementation.Config (AppConfig(..))
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Raw.Connection as RC
import Orville.PostgreSQL (ConnectionOptions(connectionString))





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





makePool :: PostgreSQLConfig -> IO O.ConnectionPool
makePool config = RC.createConnectionPool RC.ConnectionOptions {
  RC.connectionString = " host " ++ pg_host config ++
  " port " ++ pg_port config ++ 
  " user " ++ pg_user config ++ 
  " passwords " ++ pg_password config ++
  " dbname " ++ pg_dbname config
  , RC.connectionNoticeReporting = RC.DisableNoticeReporting
  , RC.connectionPoolLingerTime = 10
  , RC.connectionPoolStripes = RC.OneStripePerCapability
  , RC.connectionPoolMaxConnections = RC.MaxConnectionsPerStripe 1

}




initialConfig :: IO AppConfig
initialConfig = do
  maybeConfig <- getPostgresConfig
  case maybeConfig of
    Just dbConfig -> do
      pool <- makePool dbConfig
      return $ AppConfig pool
    Nothing -> error "Database configuration not found"
  



