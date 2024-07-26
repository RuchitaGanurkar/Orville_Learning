-- -- -- {-# LANGUAGE DataKinds #-}
-- -- -- {-# LANGUAGE TypeOperators #-}
-- -- -- {-# LANGUAGE DeriveGeneric #-}
-- -- -- {-# LANGUAGE OverloadedStrings #-}

-- -- -- module Main where
-- -- -- import qualified Orville.PostgreSQL.Raw.Connection as Connection
-- -- -- import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
-- -- -- import qualified Orville.PostgreSQL as O
-- -- -- import RequiredApi
-- -- -- import Core 
-- -- -- import Env
-- -- -- import Servant
-- -- -- import RequiredTypes
-- -- -- import Network.Wai.Handler.Warp
-- -- -- import Env (getDBConfig)

-- -- -- main :: IO ()
-- -- -- main = do
-- -- --   checkDBConfig <- getDBConfig 
-- -- --   case checkDBConfig of 
-- -- --     Just dbConfig -> do 
-- -- --       pool <- Connection.createConnectionPool
-- -- --         Connection.ConnectionOptions
-- -- --         { 
-- -- --             Connection.connectionString = 
-- -- --             "host=" ++ host dbConfig ++
-- -- --             "post=" ++ port dbConfig ++ 

-- -- --              "user=" ++ user dbConfig ++ 
-- -- --              "password=" ++ password dbConfig ++ 

-- -- --              "dbname=" ++ database dbConfig ++

-- -- --              , Connection.connectionNoticeReporting = Connection.DisableNoticeReporting 
-- -- --              , Connection.connectionPoolStripes = Connection.OneStripePerCapability 
-- -- --              , Connection.connectionPoolLingerTime = 10 
-- -- --              , Connection.connectionPoolMaxConnections = Connection.MaxConnectionsPerStripe 1
-- -- --         }

-- -- --     Nothing -> "DB Configuration Details Invalid"
  
  
-- -- --   O.runOrville pool $ AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable studentTable]
  
  
  
-- -- --   putStrLn "running on port http://localhost:3000"
-- -- --   run 3000 (serve api (server pool))



-- -- {-# LANGUAGE DataKinds #-}
-- -- {-# LANGUAGE TypeOperators #-}
-- -- {-# LANGUAGE DeriveGeneric #-}
-- -- {-# LANGUAGE OverloadedStrings #-}

-- -- module Main where

-- -- import qualified Orville.PostgreSQL.Raw.Connection as Connection
-- -- import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
-- -- import qualified Orville.PostgreSQL as O
-- -- import RequiredTypes
-- -- import RequiredServer
-- -- import Core 
-- -- import Env
-- -- import Servant
-- -- import RequiredTypes
-- -- import Network.Wai.Handler.Warp

-- -- -- main :: IO ()
-- -- -- main = do
-- -- --   maybeDBConfig <- getDBConfig 
-- -- --   case maybeDBConfig of 
-- -- --     Nothing -> putStrLn "Error: Database configuration not found in environment variables."
-- -- --     Just dbConfig -> do 
-- -- --       pool <- Connection.createConnectionPool
-- -- --         Connection.ConnectionOptions
-- -- --         { 
-- -- --             Connection.connectionString = 
-- -- --                 " host=" ++ host dbConfig ++
-- -- --                 " port=" ++ port dbConfig ++ 
-- -- --                 " user=" ++ user dbConfig ++ 
-- -- --                 " password=" ++ password dbConfig ++ 
-- -- --                 " dbname=" ++ database dbConfig
-- -- --             , Connection.connectionNoticeReporting = Connection.DisableNoticeReporting 
-- -- --             , Connection.connectionPoolStripes = Connection.OneStripePerCapability 
-- -- --             , Connection.connectionPoolLingerTime = 10 
-- -- --             , Connection.connectionPoolMaxConnections = Connection.MaxConnectionsPerStripe 1
-- -- --         }
-- -- --       O.runOrville pool $ AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable studentTable]
-- -- --       putStrLn "running on port http://localhost:3000"
-- -- --       let config = AppConfig pool
-- -- --       run 3000 (app config)


-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Network.Wai.Handler.Warp (run)
-- import Servant
-- import Config (AppConfig(..), initialConfig)
-- import Core (AppMonad, runApp)
-- import Routes (api, appServer)
-- import DB (migrateDB)

-- app :: AppConfig -> Application
-- app cfg = serve api (hoistServer api (runApp cfg) appServer)

-- main :: IO ()
-- main = do
--   appConfig <- initialConfig
--   migrateDB appConfig
--   run 8080 (app appConfig)


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Implementation.Config (AppConfig(..), initialConfig)
import Implementation.Core (AppMonad, runApp)
import Implementation.Routes (api, appServer)
import Implementation.DB (migrateDB)

app :: AppConfig -> Application
app cfg = logStdoutDev $ serve api (hoistServer api (runApp cfg) appServer)

main :: IO ()
main = do
  appConfig <- initialConfig
  runDBMigrate appConfig
  run 3000 (app appConfig)
