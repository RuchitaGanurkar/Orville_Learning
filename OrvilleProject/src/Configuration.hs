module Configuration where


import qualified Orville.PostgreSQL as O
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
